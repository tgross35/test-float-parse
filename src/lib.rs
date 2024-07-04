#![allow(unused)]

mod traits;
mod validate;

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::any::{type_name, TypeId};
use std::collections::HashMap;
use std::io::prelude::*;
use std::ops::ControlFlow;
use std::process::ExitCode;
use std::sync::atomic::{AtomicU64, Ordering};
use std::{fmt, fs, io, mem, ops, sync::mpsc, thread, time};
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

pub const SEED: [u8; 32] = *b"3.141592653589793238462643383279";

/// If there are more tests than this threashold, we launch them last and use parallel
/// iterators.
const HUGE_TEST_CUTOFF: u64 = 1_000_000;

/// Message passed from a test runner to the host
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
    fn find_test<'a>(&self, tests: &'a mut [TestInfo]) -> &'a mut TestInfo {
        tests.iter_mut().find(|t| t.id == self.id).unwrap()
    }

    fn handle(self, tests: &mut [TestInfo], out: &mut Tee, mp: &MultiProgress) {
        let test = self.find_test(tests);
        let mut pb = &mut test.pb.as_mut().unwrap();

        match self.update {
            Update::Started => {
                out.write_mp(mp, format!("Testing '{}'", test.name));
            }
            Update::Progress {
                executed,
                duration_each_us,
            } => {
                pb.set_position(executed);
            }
            Update::Failure { fail, input } => {
                out.write_mp(
                    mp,
                    format!(
                        "Failure in '{}': {}. parsing '{input}'",
                        test.name,
                        fail.msg()
                    ),
                );
            }
            Update::Completed(c) => {
                test.finalize_pb(&c);

                out.write_mp(
                    mp,
                    format!(
                        "Completed tests for generator '{}' in {:?}. {} tests run, {} failures",
                        test.name, c.elapsed, c.executed, c.failures
                    ),
                );
                test.completed = Some(c);
            }
        };
    }
}

#[derive(Clone, Debug)]
enum Update {
    Started,
    /// Completed a out of b tests
    Progress {
        executed: u64,
        duration_each_us: u64,
    },
    /// Received a failed test
    Failure {
        fail: Failure,
        input: Box<str>,
    },
    /// Exited with an unexpected condition
    Completed(Completed),
}

#[derive(Clone, Copy, Debug)]
enum Failure {
    /// Above the zero cutoff but got rounded to zero
    RoundedToZero,
    /// Below the infinity cutoff but got rounded to infinity
    RoundedToInf,
    /// Above the negative infinity cutoff but got rounded to negative infinity
    RoundedToNegInf,
    UnexpectedNan,
    ExpectedNan,
}

impl Failure {
    fn msg(self) -> &'static str {
        match self {
            Failure::RoundedToZero => "incorrectly rounded to 0 (expected nonzero)",
            Failure::RoundedToInf => "incorrectly rounded to +inf (expected finite)",
            Failure::RoundedToNegInf => "incorrectly rounded to -inf (expected finite)",
            Failure::UnexpectedNan => "got a NaN where none was expected",
            Failure::ExpectedNan => "expected a NaN but did not get it",
        }
    }
}

#[derive(Debug)]
pub struct TestInfo {
    id: TypeId,
    f_name: &'static str,
    gen_name: &'static str,
    gen_short_name: &'static str,
    pub name: String,
    short_name: String,
    run: bool,
    total_tests: u64,
    launch: for<'s> fn(&'s thread::Scope<'s, '_>, mpsc::Sender<Msg>, &TestInfo),
    pb: Option<ProgressBar>,
    completed: Option<Completed>,
}

impl TestInfo {
    fn finalize_pb(&mut self, c: &Completed) {
        let pb = self.pb.take().unwrap();
        pb.set_message(format!("{} ({} failures)", self.short_name, c.failures));

        pb.finish();
    }

    /// True if this should be run using parallel iterators.
    fn is_huge_test(&self) -> bool {
        self.total_tests >= HUGE_TEST_CUTOFF
    }
}

#[derive(Clone, Debug)]
struct Completed {
    executed: u64,
    failures: u64,
    /// Exists if exited with an error
    result: Result<(), Box<str>>,
    elapsed: Duration,
}

#[derive(Debug)]
pub struct Config {
    pub timeout: Duration,
}

/// Collect, filter, and launch all tests
pub fn run(cfg: &Config, include: &[String], exclude: &[String]) -> ExitCode {
    let mut tests = register_tests();

    if !include.is_empty() {
        tests.retain(|test| include.iter().any(|inc| test.name.contains(inc)));
    }

    tests.retain(|test| !exclude.iter().any(|exc| test.name.contains(exc)));

    let (logfile, logfile_name) = log_file();
    let mut out = Tee { f: &logfile };
    let elapsed = launch_tests(&mut tests, cfg, &mut out);
    let ret = finish(&tests, elapsed, &mut out);

    drop(out);
    println!("wrote results to {logfile_name}");

    ret
}

/// Configure tests to run
pub fn register_tests() -> Vec<TestInfo> {
    let mut tests = Vec::new();
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
    register_generator_for_float::<F, gen::exponents::SmallExponents>(tests);
    register_generator_for_float::<F, gen::exponents::LargeExponents>(tests);
    register_generator_for_float::<F, gen::sparse::FewOnes<F>>(tests);
    register_generator_for_float::<F, gen::integers::SmallInt>(tests);
    register_generator_for_float::<F, gen::integers::LargeInt>(tests);
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
        gen_short_name,
        name: format!("{f_name} {gen_name}"),
        short_name: format!("{f_name} {gen_short_name}"),
        run: true,
        launch: launch_one::<F, G>,
        total_tests: G::total_tests(),
        pb: None,
        completed: None,
    };
    v.push(info)
}

enum EndCondition {
    Timeout,
    Success(Duration),
}

/// Run all tests in `tests`
fn launch_tests(tests: &mut [TestInfo], config: &Config, out: &mut Tee) -> Duration {
    let (tx, rx) = mpsc::channel::<Msg>();
    let total_tests = tests.len();
    let mp = MultiProgress::new();
    mp.set_move_cursor(true);
    let pb_style = ProgressStyle::with_template(
        "[{elapsed}] {bar:40.cyan/blue} {pos}/{len} ({percent}%) {msg}",
    )
    .unwrap()
    .progress_chars("##-");

    // Run shorter tests first
    tests.sort_unstable_by_key(|test| test.total_tests);

    for test in tests.iter() {
        out.write_sout(format!("Launching test '{}'", test.name));
    }

    let start = Instant::now();

    let mut panic_drop_bars = Vec::new();

    for test in tests.iter_mut() {
        let mut pb = mp.add(ProgressBar::new(test.total_tests));
        pb.set_style(pb_style.clone());
        pb.set_message(test.short_name.clone());
        panic_drop_bars.push(pb.clone());
        test.pb = Some(pb);
    }

    // indicatif likes to eat panic messages. This workaround isn't ideal, but it improves things.
    // <https://github.com/console-rs/indicatif/issues/121>
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        for bar in &panic_drop_bars {
            bar.abandon();
            println!();
            io::stdout().flush();
            io::stderr().flush();
        }
        hook(info);
    }));

    thread::scope(move |scope| {
        for test in tests.iter() {
            (test.launch)(scope, tx.clone(), &test);
        }

        let end_condition = loop {
            let elapsed = Instant::now() - start;
            if elapsed > config.timeout {
                break EndCondition::Timeout;
            }

            if tests.iter().all(|t| t.completed.is_some()) {
                break EndCondition::Success(elapsed);
            }

            let msg = rx.recv().unwrap();
            msg.handle(tests, out, &mp);
        };

        drop(mp);
        assert_eq!(rx.try_recv().unwrap_err(), mpsc::TryRecvError::Empty);

        match end_condition {
            EndCondition::Timeout => {
                out.write_sout(format!("Timeout of {:?} reached", config.timeout))
            }
            EndCondition::Success(elapsed) => {
                out.write_sout(format!("Completed {total_tests} tests in {elapsed:?}"))
            }
        }
    });

    Instant::now() - start
}

fn finish(tests: &[TestInfo], total_elapsed: Duration, out: &mut Tee) -> ExitCode {
    out.write_sout(format!("\n\nResults:"));

    let mut failed_generators = 0;
    let mut errored_generators = 0;

    for t in tests {
        let Completed {
            executed,
            failures,
            elapsed,
            result,
        } = t.completed.as_ref().unwrap();

        let stat = if result.is_err() {
            errored_generators += 1;
            "ERROR"
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

        if let Err(reason) = result {
            out.write_sout(format!("      reason: {reason}"));
        }
    }

    out.write_sout(format!(
        "{passed}/{} tests succeeded in {total_elapsed:?} ({passed} passed, {} failed, {} errors)",
        tests.len(),
        failed_generators,
        errored_generators,
        passed = tests.len() - failed_generators - errored_generators,
    ));

    if failed_generators > 0 || errored_generators > 0 {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

/// Test runer for a single generator
fn launch_one<'s, F: Float, G: Generator<F>>(
    scope: &'s thread::Scope<'s, '_>,
    tx: mpsc::Sender<Msg>,
    info: &TestInfo,
) {
    scope.spawn(move || {
        tx.send(Msg::new::<F, G>(Update::Started));

        let mut est = G::total_tests();
        let mut gen = G::new();
        let mut executed = AtomicU64::new(0);
        let mut failures = 0;
        let mut result = Ok(());

        let checks_per_update = (est / 100000).clamp(1, 1000);
        let started = Instant::now();

        let check_one = |test_str: String| {
            let executed = executed.fetch_add(1, Ordering::Relaxed);

            match validate::validate::<F>(&test_str, G::PATTERNS_CONTAIN_NAN) {
                Ok(()) => (),
                Err(e) => tx.send(Msg::new::<F, G>(e)).unwrap(),
            };

            // Send periodic updates
            if executed % checks_per_update == 0 {
                let elapsed = Instant::now() - started;

                tx.send(Msg::new::<F, G>(Update::Progress {
                    executed,
                    duration_each_us: (elapsed.as_micros() / u128::from(executed).max(1))
                        .try_into()
                        .unwrap(),
                }))
                .unwrap();
            }
        };

        gen.par_bridge().for_each(check_one);

        let elapsed = Instant::now() - started;
        let executed = executed.into_inner();

        // Warn about bad estimates
        if executed != est {
            result = Err(format!(
                "executed tests != estimated ({executed} != {est}) for {}",
                G::NAME
            )
            .into());
        }

        tx.send(Msg::new::<F, G>(Update::Completed(Completed {
            executed,
            failures,
            elapsed,
            result,
        })));
    });
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
        mb.println(s);
    }

    /// Write to both the file and stdout. Includes newline.
    fn write_sout(&mut self, s: impl Into<String>) {
        let mut s = s.into();
        s.push('\n');
        self.f.write(s.as_bytes()).unwrap();
        io::stdout().write(s.as_bytes()).unwrap();
    }
}

/// Turn the integer into a float then print it in exponential format
fn update_buf_from_bits<F: Float>(s: &mut String, i: F::Int) -> &str {
    use std::fmt::Write;
    s.clear();
    write!(s, "{:e}", F::from_bits(i));
    s
}
