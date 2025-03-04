mod traits;
mod ui;
mod validate;

use std::any::{type_name, TypeId};
use std::cmp::min;
use std::ops::RangeInclusive;
use std::process::ExitCode;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{mpsc, OnceLock};
use std::{fmt, time};

use indicatif::{MultiProgress, ProgressBar};
use rand::distributions::{Distribution, Standard};
use rayon::prelude::*;
use time::{Duration, Instant};
use traits::{Float, Generator, Int};
pub use ui::create_log_file;
use ui::Tee;

/// Test generators.
mod gen {
    pub mod exhaustive;
    pub mod exponents;
    pub mod fuzz;
    pub mod integers;
    pub mod long_fractions;
    pub mod many_digits;
    pub mod sparse;
    pub mod spot_checks;
    pub mod subnorm;
}

/// Fuzz iterations to run if not specified by CLI arg.
pub const DEFAULT_FUZZ_COUNT: u64 = 100_000_000;

/// If there are more tests than this threashold, the test will be defered until after all
/// others run (so as to avoid thread pool starvation).
const HUGE_TEST_CUTOFF: u64 = 5_000_000;

/// Seed for tests that use a deterministic RNG.
pub const SEED: [u8; 32] = *b"3.141592653589793238462643383279";

/// Global configuration
#[derive(Debug)]
pub struct Config {
    pub timeout: Duration,
    /// Failures per test
    pub max_failures: Option<u64>,
    pub fuzz_count: Option<u64>,
}

/// Collect, filter, and launch all tests.
pub fn run(cfg: Config, include: &[String], exclude: &[String]) -> ExitCode {
    gen::fuzz::FUZZ_COUNT.store(cfg.fuzz_count.unwrap_or(u64::MAX), Ordering::Relaxed);

    // With default parallelism, the CPU doesn't saturate. We don't need to be nice to
    // other processes, so do 1.5x to make sure we use all available resources.
    let threads = std::thread::available_parallelism()
        .map(Into::into)
        .unwrap_or(0)
        * 3
        / 2;
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()
        .unwrap();

    let mut tests = register_tests();
    tests.retain(|test| !exclude.iter().any(|exc| test.name.contains(exc)));
    if !include.is_empty() {
        tests.retain(|test| include.iter().any(|inc| test.name.contains(inc)));
    }

    let (logfile, logfile_name) = create_log_file();

    let mut out = Tee { f: &logfile };
    let elapsed = launch_tests(&mut tests, &cfg, &mut out);
    let ret = ui::finish(&tests, elapsed, &cfg, &mut out);

    println!("wrote results to {logfile_name}");

    ret
}

/// Enumerate tests to run but don't actaully run them.
pub fn register_tests() -> Vec<TestInfo> {
    let mut tests = Vec::new();

    // Register normal generators for all floats.
    register_float::<f32>(&mut tests);
    register_float::<f64>(&mut tests);

    // Register exhaustive tests for <= 32 bits. No more because it would take years.
    TestInfo::register::<f32, gen::exhaustive::Exhaustive<f32>>(&mut tests);

    tests.sort_unstable_by_key(|t| (t.float_name, t.gen_name));
    for i in 0..(tests.len() - 1) {
        if tests[i].gen_name == tests[i + 1].gen_name {
            panic!("dupliate test name {}", tests[i].gen_name);
        }
    }

    tests
}

/// Register all generators for a single float.
fn register_float<F: Float>(tests: &mut Vec<TestInfo>)
where
    RangeInclusive<F::Int>: Iterator<Item = F::Int>,
    <F::Int as TryFrom<u128>>::Error: std::fmt::Debug,
    Standard: Distribution<<F as traits::Float>::Int>,
{
    TestInfo::register::<F, gen::exponents::LargeExponents<F>>(tests);
    TestInfo::register::<F, gen::exponents::SmallExponents<F>>(tests);
    TestInfo::register::<F, gen::fuzz::Fuzz<F>>(tests);
    TestInfo::register::<F, gen::integers::LargeInt<F>>(tests);
    TestInfo::register::<F, gen::integers::SmallInt>(tests);
    TestInfo::register::<F, gen::long_fractions::RepeatingDecimal>(tests);
    TestInfo::register::<F, gen::many_digits::RandDigits<F>>(tests);
    TestInfo::register::<F, gen::sparse::FewOnesFloat<F>>(tests);
    TestInfo::register::<F, gen::sparse::FewOnesInt<F>>(tests);
    TestInfo::register::<F, gen::spot_checks::Special>(tests);
    TestInfo::register::<F, gen::subnorm::SubnormComplete<F>>(tests);
    TestInfo::register::<F, gen::subnorm::SubnormEdgeCases<F>>(tests);
}

/// Configuration for a single test.
#[derive(Debug)]
pub struct TestInfo {
    pub name: String,
    /// Tests are identified by the type ID of `(F, G)` (tuple of the float and generator type).
    /// This gives an easy way to associate messages with tests.
    id: TypeId,
    float_name: &'static str,
    gen_name: &'static str,
    /// Name for display in the progress bar.
    short_name: String,
    total_tests: u64,
    /// Function to launch this test.
    launch: for<'s> fn(&mpsc::Sender<Msg>, &TestInfo, &Config),
    /// Progress bar to be updated.
    pb: Option<ProgressBar>,
    /// Once completed, this will be set.
    completed: OnceLock<Completed>,
}

impl TestInfo {
    /// Create a `TestInfo` for a given float and generator, then add it to a list.
    fn register<F: Float, G: Generator<F>>(v: &mut Vec<Self>) {
        let f_name = type_name::<F>();
        let gen_name = G::NAME;
        let gen_short_name = G::SHORT_NAME;

        let info = TestInfo {
            id: TypeId::of::<(F, G)>(),
            float_name: f_name,
            gen_name,
            pb: None,
            name: format!("{f_name} {gen_name}"),
            short_name: format!("{f_name} {gen_short_name}"),
            launch: test_runner::<F, G>,
            total_tests: G::total_tests(),
            completed: OnceLock::new(),
        };
        v.push(info);
    }

    /// Create a progress bar for this test within a multiprogress bar.
    fn register_pb(&mut self, mp: &MultiProgress, drop_bars: &mut Vec<ProgressBar>) {
        self.pb = Some(ui::create_pb(
            mp,
            self.total_tests,
            &self.short_name,
            drop_bars,
        ));
    }

    /// When the test is finished, update progress bar messages and finalize.
    fn finalize_pb(&self, c: &Completed) {
        let pb = self.pb.as_ref().unwrap();
        ui::finalize_pb(pb, &self.short_name, c);
    }

    /// True if this should be run after all others.
    fn is_huge_test(&self) -> bool {
        self.total_tests >= HUGE_TEST_CUTOFF
    }
}

/// A message sent from test runner threads to the UI/log thread.
#[derive(Clone, Debug)]
struct Msg {
    id: TypeId,
    update: Update,
}

impl Msg {
    /// Wrap an `Update` into a message for the specified type. We use the `TypeId` of `(F, G)` to
    /// identify which test a message in the channel came from.
    fn new<F: Float, G: Generator<F>>(u: Update) -> Self {
        Self {
            id: TypeId::of::<(F, G)>(),
            update: u,
        }
    }

    /// Get the matching test from a list. Panics if not found.
    fn find_test<'a>(&self, tests: &'a [TestInfo]) -> &'a TestInfo {
        tests.iter().find(|t| t.id == self.id).unwrap()
    }

    /// Update UI as needed for a single message received from the test runners.
    fn handle(self, tests: &[TestInfo], out: &mut Tee, mp: &MultiProgress) {
        let test = self.find_test(tests);
        let pb = test.pb.as_ref().unwrap();

        match self.update {
            Update::Started => {
                out.write_mp(mp, format!("Testing '{}'", test.name));
            }
            Update::Progress { executed, failures } => {
                pb.set_message(format! {"{failures}"});
                pb.set_position(executed);
            }
            Update::Failure { fail, input } => {
                out.write_mp(
                    mp,
                    format!("Failure in '{}': {fail}. parsing '{input}'", test.name,),
                );
            }
            Update::Completed(c) => {
                test.finalize_pb(&c);

                let prefix = match c.result {
                    Ok(FinishedAll) => "Completed tests for",
                    Err(EarlyExit::Timeout) => "Timed out",
                    Err(EarlyExit::MaxFailures) => "Max failures reached for",
                };

                out.write_mp(
                    mp,
                    format!(
                        "{prefix} generator '{}' in {:?}. {} tests run, {} failures",
                        test.name, c.elapsed, c.executed, c.failures
                    ),
                );
                test.completed.set(c).unwrap();
            }
        };
    }
}

/// Status sent with a message.
#[derive(Clone, Debug)]
enum Update {
    /// Starting a new test runner.
    Started,
    /// Completed a out of b tests
    Progress { executed: u64, failures: u64 },
    /// Received a failed test
    Failure { fail: CheckFailure, input: Box<str> },
    /// Exited with an unexpected condition
    Completed(Completed),
}

/// Result of an input did not parsing successfully.
#[derive(Clone, Debug)]
enum CheckFailure {
    /// Above the zero cutoff but got rounded to zero.
    UnexpectedZero,
    /// Below the infinity cutoff but got rounded to infinity.
    UnexpectedInf,
    /// Above the negative infinity cutoff but got rounded to negative infinity.
    UnexpectedNegInf,
    /// Got a `NaN` when none was expected.
    UnexpectedNan,
    /// Expected `NaN`, got none.
    ExpectedNan,
    /// Expected infinity, got finite.
    ExpectedInf,
    /// Expected negative infinity, got finite.
    ExpectedNegInf,
    /// The value exceeded its error tolerance.
    InvalidReal {
        /// Error from the expected value, as a float.
        error_float: Option<f64>,
        /// Error as a rational string (since it can't always be represented as a float).
        error_str: Box<str>,
    },
}

impl fmt::Display for CheckFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CheckFailure::UnexpectedZero => {
                write!(f, "incorrectly rounded to 0 (expected nonzero)")
            }
            CheckFailure::UnexpectedInf => {
                write!(f, "incorrectly rounded to +inf (expected finite)")
            }
            CheckFailure::UnexpectedNegInf => {
                write!(f, "incorrectly rounded to -inf (expected finite)")
            }
            CheckFailure::UnexpectedNan => write!(f, "got a NaN where none was expected"),
            CheckFailure::ExpectedNan => write!(f, "expected a NaN but did not get it"),
            CheckFailure::ExpectedInf => write!(f, "expected +inf but did not get it"),
            CheckFailure::ExpectedNegInf => write!(f, "expected -inf but did not get it"),
            CheckFailure::InvalidReal {
                error_float,
                error_str,
            } => {
                write!(f, "real number did not parse correctly; error:{error_str}")?;
                if let Some(float) = error_float {
                    write!(f, " ({float})")?;
                }
                Ok(())
            }
        }
    }
}

/// Information about a completed test generator.
#[derive(Clone, Debug)]
struct Completed {
    /// Finished tests (both successful and failed).
    executed: u64,
    /// Failed tests.
    failures: u64,
    /// Extra exit information if unsuccessful.
    result: Result<FinishedAll, EarlyExit>,
    /// If there is something to warn about (e.g bad estimate), leave it here.
    warning: Option<Box<str>>,
    /// Total time to run the test.
    elapsed: Duration,
}

/// Marker for completing all tests (used in `Result` types).
#[derive(Clone, Debug)]
struct FinishedAll;

/// Reasons for exiting early.
#[derive(Clone, Debug)]
enum EarlyExit {
    Timeout,
    MaxFailures,
}

/// Run all tests in `tests`.
///
/// This launches a main thread that receives messages and handlees UI updates, and uses the
/// rest of the thread pool to execute the tests.
fn launch_tests(tests: &mut [TestInfo], cfg: &Config, out: &mut Tee) -> Duration {
    // Run shorter tests first
    tests.sort_unstable_by_key(|test| test.total_tests);

    for test in tests.iter() {
        out.write_sout(format!("Launching test '{}'", test.name));
    }

    // Configure progress bars
    let mut all_progress_bars = Vec::new();
    let mp = MultiProgress::new();
    mp.set_move_cursor(true);
    for test in tests.iter_mut() {
        test.register_pb(&mp, &mut all_progress_bars);
    }

    ui::set_panic_hook(all_progress_bars);

    let (tx, rx) = mpsc::channel::<Msg>();
    let start = Instant::now();

    rayon::scope(|scope| {
        // Thread that updates the UI
        scope.spawn(|_scope| {
            let rx = rx; // move rx

            loop {
                if tests.iter().all(|t| t.completed.get().is_some()) {
                    break;
                }

                let msg = rx.recv().unwrap();
                msg.handle(tests, out, &mp);
            }

            // All tests completed; finish things up
            drop(mp);
            assert_eq!(rx.try_recv().unwrap_err(), mpsc::TryRecvError::Empty);
        });

        // Don't let the thread pool be starved by huge tests. Run faster tests first in parallel,
        // then parallelize only within the rest of the tests.
        let (huge_tests, normal_tests): (Vec<_>, Vec<_>) =
            tests.iter().partition(|t| t.is_huge_test());

        // Run the actual tests
        normal_tests
            .par_iter()
            .for_each(|test| ((test.launch)(&tx, test, cfg)));

        huge_tests
            .par_iter()
            .for_each(|test| ((test.launch)(&tx, test, cfg)));
    });

    start.elapsed()
}

/// Test runer for a single generator.
///
/// This calls the generator's iterator multiple times (in parallel) and validates each output.
fn test_runner<F: Float, G: Generator<F>>(tx: &mpsc::Sender<Msg>, _info: &TestInfo, cfg: &Config) {
    tx.send(Msg::new::<F, G>(Update::Started)).unwrap();

    let total = G::total_tests();
    let gen = G::new();
    let executed = AtomicU64::new(0);
    let failures = AtomicU64::new(0);

    let checks_per_update = min(total, 1000);
    let started = Instant::now();

    // Function to execute for a single test iteration.
    let check_one = |buf: &mut String, ctx: G::WriteCtx| {
        let executed = executed.fetch_add(1, Ordering::Relaxed);
        buf.clear();
        G::write_string(buf, ctx);

        match validate::validate::<F>(buf, G::PATTERNS_CONTAIN_NAN) {
            Ok(()) => (),
            Err(e) => {
                tx.send(Msg::new::<F, G>(e)).unwrap();
                let f = failures.fetch_add(1, Ordering::Relaxed);
                // End early if the limit is exceeded.
                if cfg.max_failures.map_or(false, |mf| f >= mf) {
                    return Err(EarlyExit::MaxFailures);
                }
            }
        };

        // Send periodic updates
        if executed % checks_per_update == 0 {
            let failures = failures.load(Ordering::Relaxed);

            tx.send(Msg::new::<F, G>(Update::Progress { executed, failures }))
                .unwrap();

            if started.elapsed() > cfg.timeout {
                return Err(EarlyExit::Timeout);
            }
        }

        Ok(())
    };

    // Run the test iterations in parallel. Each thread gets a string buffer to write
    // its check values to.
    let res = gen
        .par_bridge()
        .try_for_each_init(|| String::with_capacity(100), check_one);

    let elapsed = started.elapsed();
    let executed = executed.into_inner();
    let failures = failures.into_inner();

    // Warn about bad estimates if relevant.
    let warning = if executed != total && res.is_ok() {
        let msg = format!(
            "executed tests != estimated ({executed} != {total}) for {}",
            G::NAME
        );

        Some(msg.into())
    } else {
        None
    };

    let result = res.map(|()| FinishedAll);
    tx.send(Msg::new::<F, G>(Update::Completed(Completed {
        executed,
        failures,
        result,
        warning,
        elapsed,
    })))
    .unwrap();
}
