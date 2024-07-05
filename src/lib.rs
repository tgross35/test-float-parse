mod traits;
mod validate;

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use rayon::prelude::*;
use std::any::{type_name, TypeId};
use std::cmp::min;
use std::fmt;
use std::io::prelude::*;
use std::process::ExitCode;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;
use std::{fs, io, sync::mpsc, time};
use time::{Duration, Instant};

use traits::{Float, Generator, Int};

mod gen {
    pub mod exhaustive;
    pub mod exponents;
    pub mod fuzz;
    pub mod integers;
    pub mod long_fractions;
    pub mod sparse;
    pub mod subnorm;
}

/// Fuzz iterations to run if not specified
pub const DEFAULT_FUZZ_COUNT: u64 = 20_000_000;

/// If there are more tests than this threashold, we launch them last and use parallel
/// iterators.
const HUGE_TEST_CUTOFF: u64 = 5_000_000;

/// Templates for progress bars
const PB_TEMPLATE:&str =
        "[{elapsed:3} {percent:3}%] {bar:20.cyan/blue} NAME ({pos}/{len}, {msg} f, {per_sec}, eta {eta})";
const PB_TEMPLATE_FINAL: &str =
    "[{elapsed:3} {percent:3}%] NAME ({pos}/{len}, {msg:.COLOR}, {per_sec}, {elapsed_precise})";

/// Seed for tests that use a deterministic RNG
pub const SEED: [u8; 32] = *b"3.141592653589793238462643383279";

/// Global configuration
#[derive(Debug)]
pub struct Config {
    pub timeout: Duration,
    /// Failures per test
    pub max_failures: Option<u64>,
    pub fuzz_count: Option<u64>,
}

/// Configuration for a test
#[derive(Debug)]
pub struct TestInfo {
    id: TypeId,
    f_name: &'static str,
    gen_name: &'static str,
    pub name: String,
    short_name: String,
    total_tests: u64,
    launch: for<'s> fn(mpsc::Sender<Msg>, &TestInfo, &Config),
    pb: Option<ProgressBar>,
    completed: OnceLock<Completed>,
}

impl TestInfo {
    /// Create a progress bar for this test in a multiprogress bar
    fn register_pb(&mut self, mp: &MultiProgress, drop_bars: &mut Vec<ProgressBar>) {
        let pb = mp.add(ProgressBar::new(self.total_tests));
        let short_name_padded = format!("{:16}", self.short_name);
        let pb_style =
            ProgressStyle::with_template(&PB_TEMPLATE.replace("NAME", &short_name_padded))
                .unwrap()
                .progress_chars("##-");

        pb.set_style(pb_style.clone());
        pb.set_message("0");
        drop_bars.push(pb.clone());
        self.pb = Some(pb)
    }

    /// When the test is finished, update progress bar messages and finalize.
    fn finalize_pb(&self, c: &Completed) {
        let pb = self.pb.as_ref().unwrap();
        let short_name_padded = format!("{:16}", self.short_name);
        let f = c.failures;

        // Use a tuple so we can use colors
        let (color, msg, finish): (&str, String, fn(&ProgressBar, String)) = match c.result {
            Ok(Finished) if f > 0 => (
                "red",
                format!("{f} f (finished with errors)",),
                ProgressBar::finish_with_message,
            ),
            Ok(Finished) => (
                "green",
                format!("{f} f (finished successfully)",),
                ProgressBar::finish_with_message,
            ),
            Err(EarlyExit::Timeout) => (
                "red",
                format!("{f} f (timed out)"),
                ProgressBar::abandon_with_message,
            ),
            Err(EarlyExit::MaxFailures) => (
                "red",
                format!("{f} f (failure limit)"),
                ProgressBar::abandon_with_message,
            ),
        };

        let pb_style = ProgressStyle::with_template(
            &PB_TEMPLATE_FINAL
                .replace("NAME", &short_name_padded)
                .replace("COLOR", color),
        )
        .unwrap();

        pb.set_style(pb_style);
        finish(pb, msg);
    }

    /// True if this should be run using parallel iterators.
    fn is_huge_test(&self) -> bool {
        self.total_tests >= HUGE_TEST_CUTOFF
    }
}

/// A message sent from test runner threads to the thread doing UI and logs
#[derive(Clone, Debug)]
struct Msg {
    id: TypeId,
    update: Update,
}

impl Msg {
    fn new<F: Float, G: Generator<F>>(u: Update) -> Self {
        Self {
            id: TypeId::of::<(F, G)>(),
            update: u,
        }
    }

    /// Get the matching test from a list
    fn find_test<'a>(&self, tests: &'a [TestInfo]) -> &'a TestInfo {
        tests.iter().find(|t| t.id == self.id).unwrap()
    }

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
                    Ok(Finished) => "Completed tests for",
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

/// Status of a message
#[derive(Clone, Debug)]
enum Update {
    Started,
    /// Completed a out of b tests
    Progress {
        executed: u64,
        failures: u64,
    },
    /// Received a failed test
    Failure {
        fail: CheckFailure,
        input: Box<str>,
    },
    /// Exited with an unexpected condition
    Completed(Completed),
}

/// An input did not parse successfully
#[derive(Clone, Debug)]
enum CheckFailure {
    /// Above the zero cutoff but got rounded to zero
    RoundedToZero,
    /// Below the infinity cutoff but got rounded to infinity
    RoundedToInf,
    /// Above the negative infinity cutoff but got rounded to negative infinity
    RoundedToNegInf,
    UnexpectedNan,
    ExpectedNan,
    InvalidReal {
        error_float: Option<f64>,
        error_str: Box<str>,
    },
}

impl fmt::Display for CheckFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CheckFailure::RoundedToZero => write!(f, "incorrectly rounded to 0 (expected nonzero)"),
            CheckFailure::RoundedToInf => {
                write!(f, "incorrectly rounded to +inf (expected finite)")
            }
            CheckFailure::RoundedToNegInf => {
                write!(f, "incorrectly rounded to -inf (expected finite)")
            }
            CheckFailure::UnexpectedNan => write!(f, "got a NaN where none was expected"),
            CheckFailure::ExpectedNan => write!(f, "expected a NaN but did not get it"),
            CheckFailure::InvalidReal {
                error_float: as_float,
                error_str: as_str,
            } => {
                write!(f, "real number did not parse correctly; error:{as_str}")?;
                if let Some(float) = as_float {
                    write!(f, " ({float})")?;
                }
                Ok(())
            }
        }
    }
}

/// Information about a completed test
#[derive(Clone, Debug)]
struct Completed {
    /// Finished tests (both successful and failed)
    executed: u64,
    failures: u64,
    /// Extra exit information if unsuccessful
    result: Result<Finished, EarlyExit>,
    /// If there is something to warn about (e.g bad estimate), leave it here
    warning: Option<Box<str>>,
    /// Total time to run the test
    elapsed: Duration,
}

/// Marker for completing all tests
#[derive(Clone, Debug)]
struct Finished;

/// Reasons for exiting early
#[derive(Clone, Debug)]
enum EarlyExit {
    Timeout,
    MaxFailures,
}

/// Collect, filter, and launch all tests
pub fn run(cfg: Config, include: &[String], exclude: &[String]) -> ExitCode {
    gen::fuzz::FUZZ_COUNT.store(cfg.fuzz_count.unwrap_or(u64::MAX), Ordering::Relaxed);
    let mut tests = register_tests();

    if !include.is_empty() {
        tests.retain(|test| include.iter().any(|inc| test.name.contains(inc)));
    }

    tests.retain(|test| !exclude.iter().any(|exc| test.name.contains(exc)));

    let (logfile, logfile_name) = log_file();
    let mut out = Tee { f: &logfile };
    let elapsed = launch_tests(&mut tests, &cfg, &mut out);
    let ret = finish(&tests, elapsed, &cfg, &mut out);

    drop(out);
    println!("wrote results to {logfile_name}");

    ret
}

/// Configure tests to run but don't actaully run them
pub fn register_tests() -> Vec<TestInfo> {
    let mut tests = Vec::new();

    // Register normal generators for all floats
    register_float::<f32>(&mut tests);
    register_float::<f64>(&mut tests);

    // Don't run exhaustive for bits > 32, it would take years.
    register_generator_for_float::<f32, gen::exhaustive::Exhaustive<f32>>(&mut tests);

    tests.sort_unstable_by_key(|t| (t.f_name, t.gen_name));
    tests
}

/// Register all generators for a single float
fn register_float<F: Float>(tests: &mut Vec<TestInfo>)
where
    <F::Int as TryFrom<u128>>::Error: std::fmt::Debug,
{
    register_generator_for_float::<F, gen::subnorm::SubnormEdge<F>>(tests);
    register_generator_for_float::<F, gen::subnorm::SubnormComplete<F>>(tests);
    register_generator_for_float::<F, gen::exponents::SmallExponents<F>>(tests);
    register_generator_for_float::<F, gen::exponents::LargeExponents<F>>(tests);
    register_generator_for_float::<F, gen::exponents::LargeNegExponents<F>>(tests);
    register_generator_for_float::<F, gen::sparse::FewOnes<F>>(tests);
    register_generator_for_float::<F, gen::integers::SmallInt>(tests);
    register_generator_for_float::<F, gen::integers::LargeInt<F>>(tests);
    register_generator_for_float::<F, gen::long_fractions::RepeatingDecimal>(tests);
    register_generator_for_float::<F, gen::fuzz::Fuzz<F>>(tests);
}

/// Register a single test generator for a specific float
fn register_generator_for_float<F: Float, G: Generator<F>>(v: &mut Vec<TestInfo>) {
    let f_name = type_name::<F>();
    let gen_name = G::NAME;
    let gen_short_name = G::SHORT_NAME;

    let info = TestInfo {
        id: TypeId::of::<(F, G)>(),
        f_name,
        gen_name,
        pb: None,
        name: format!("{f_name} {gen_name}"),
        short_name: format!("{f_name} {gen_short_name}"),
        launch: test_runner::<F, G>,
        total_tests: G::total_tests(),
        completed: OnceLock::new(),
    };
    v.push(info)
}

/// Run all tests in `tests`
fn launch_tests(tests: &mut [TestInfo], cfg: &Config, out: &mut Tee) -> Duration {
    // Run shorter tests first
    tests.sort_unstable_by_key(|test| test.total_tests);
    for test in tests.iter() {
        out.write_sout(format!("Launching test '{}'", test.name));
    }

    let mut panic_drop_bars = Vec::new();
    let mp = MultiProgress::new();
    mp.set_move_cursor(true);

    for test in tests.iter_mut() {
        test.register_pb(&mp, &mut panic_drop_bars);
    }

    // indicatif likes to eat panic messages. This workaround isn't ideal, but it improves things.
    // <https://github.com/console-rs/indicatif/issues/121>
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        for bar in &panic_drop_bars {
            bar.abandon();
            println!();
            io::stdout().flush().unwrap();
            io::stderr().flush().unwrap();
        }
        hook(info);
    }));

    let (tx, rx) = mpsc::channel::<Msg>();
    let start = Instant::now();

    rayon::scope(|scope| {
        let cfg = &cfg;

        // Worker thread that updates the UI
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

        normal_tests
            .par_iter()
            .for_each(|test| ((test.launch)(tx.clone(), test, cfg)));

        huge_tests
            .iter()
            .for_each(|test| ((test.launch)(tx.clone(), test, cfg)));
    });

    Instant::now() - start
}

/// Print final messages after all tests are complete.
fn finish(tests: &[TestInfo], total_elapsed: Duration, cfg: &Config, out: &mut Tee) -> ExitCode {
    out.write_sout(format!("\n\nResults:"));

    let mut failed_generators = 0;
    let mut stopped_generators = 0;

    for t in tests {
        let Completed {
            executed,
            failures,
            elapsed,
            warning,
            result,
        } = t.completed.get().unwrap();

        let stat = if result.is_err() {
            stopped_generators += 1;
            "STOPPED"
        } else if *failures > 0 {
            failed_generators += 1;
            "FAILURE"
        } else {
            "SUCCESS"
        };

        out.write_sout(format!(
            "    {stat} for generator '{name}'. {passed}/{executed} passed in {elapsed:?}",
            name = t.name,
            passed = executed - failures,
        ));

        if let Some(warning) = warning {
            out.write_sout(format!("      warning: {warning}"));
        }

        match result {
            Ok(Finished) => (),
            Err(EarlyExit::Timeout) => out.write_sout(format!(
                "      exited early; exceded {:?} timeout",
                cfg.timeout
            )),
            Err(EarlyExit::MaxFailures) => out.write_sout(format!(
                "      exited early; exceeded {:?} max failures",
                cfg.max_failures
            )),
        }
    }

    out.write_sout(format!(
        "{passed}/{} tests succeeded in {total_elapsed:?} ({passed} passed, {} failed, {} stopped)",
        tests.len(),
        failed_generators,
        stopped_generators,
        passed = tests.len() - failed_generators - stopped_generators,
    ));

    if failed_generators > 0 || stopped_generators > 0 {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

/// Test runer for a single generator
fn test_runner<'s, F: Float, G: Generator<F>>(
    tx: mpsc::Sender<Msg>,
    _info: &TestInfo,
    cfg: &Config,
) {
    tx.send(Msg::new::<F, G>(Update::Started)).unwrap();

    let est = G::total_tests();
    let gen = G::new();
    let executed = AtomicU64::new(0);
    let failures = AtomicU64::new(0);

    let checks_per_update = min(est, 1000);
    let started = Instant::now();

    let check_one = |buf: &mut String, ctx: G::WriteCtx| {
        let executed = executed.fetch_add(1, Ordering::Relaxed);
        buf.clear();
        G::write_string(buf, ctx);

        match validate::validate::<F>(&buf, G::PATTERNS_CONTAIN_NAN) {
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
            let elapsed = Instant::now() - started;
            let failures = failures.load(Ordering::Relaxed);

            tx.send(Msg::new::<F, G>(Update::Progress { executed, failures }))
                .unwrap();

            if elapsed > cfg.timeout {
                return Err(EarlyExit::Timeout);
            }
        }

        Ok(())
    };

    // We use a map so we have a way to exit early, `for_each(drop)` ensures it is consumed.
    let res = gen
        .par_bridge()
        .try_for_each_init(|| String::with_capacity(100), check_one);

    let elapsed = Instant::now() - started;
    let executed = executed.into_inner();
    let failures = failures.into_inner();

    // Warn about bad estimates
    let warning = if executed != est && res.is_ok() {
        Some(
            format!(
                "executed tests != estimated ({executed} != {est}) for {}",
                G::NAME
            )
            .into(),
        )
    } else {
        None
    };

    let result = res.map(|()| Finished);

    tx.send(Msg::new::<F, G>(Update::Completed(Completed {
        executed,
        failures,
        elapsed,
        warning,
        result,
    })))
    .unwrap();
}

/// Open a file with a reasonable name that we can dump data to
pub fn log_file() -> (fs::File, String) {
    let now = chrono::Utc::now();
    let name = format!("parse-float-{}.txt", now.format("%Y-%m-%dT%H_%M_%S_%3fZ"));
    (
        fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&name)
            .unwrap(),
        name,
    )
}

/// Tee output to the file and
struct Tee<'a> {
    f: &'a fs::File,
}

impl<'a> Tee<'a> {
    /// Write to both the file and above the multiprogress bar. Includes newline.
    fn write_mp(&mut self, mb: &MultiProgress, s: impl Into<String>) {
        let mut s = s.into();
        s.push('\n');
        self.f.write(s.as_bytes()).unwrap();
        s.pop();
        mb.println(s).unwrap();
    }

    /// Write to both the file and stdout. Includes newline.
    fn write_sout(&mut self, s: impl Into<String>) {
        let mut s = s.into();
        s.push('\n');
        self.f.write(s.as_bytes()).unwrap();
        io::stdout().write(s.as_bytes()).unwrap();
    }
}
