#![allow(unused)]

mod validate;

use std::any::{type_name, TypeId};
use std::collections::HashMap;
use std::io::prelude::*;
use std::ops::ControlFlow;
use std::process::ExitCode;
use std::str::FromStr;
use std::{fmt, fs, io, mem, ops, sync::mpsc, thread, time};
use time::{Duration, Instant};

mod gen {
    // pub mod long_fractions;
    // pub mod short_decimals;
    pub mod subnorm;
    // pub mod u64_pow2;
}

// Nothing up my sleeve: Just (PI - 3) in base 16.
#[allow(dead_code)]
pub const SEED: [u32; 3] = [0x243f_6a88, 0x85a3_08d3, 0x1319_8a2e];

pub fn validate(text: &str) {
    let mut out = io::stdout();
    let x: f64 = text.parse().unwrap();
    let f64_bytes: u64 = unsafe { mem::transmute(x) };
    let x: f32 = text.parse().unwrap();
    let f32_bytes: u32 = unsafe { mem::transmute(x) };
    writeln!(&mut out, "{:016x} {:08x} {}", f64_bytes, f32_bytes, text).unwrap();
}

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

    fn handle(self, tests: &mut [TestInfo], out: &mut Tee) {
        let test = self.find_test(tests);

        match self.update {
            Update::Started => writeln!(out, "Testing '{}'", test.name).unwrap(),
            Update::Progress {
                executed,
                total,
                duration_each_us,
            } => todo!(),
            Update::Failure() => todo!(),
            Update::Completed(c) => {
                writeln!(
                    out,
                    "Completed test '{}' in {:?}. {} tests run, {} failures",
                    test.name, c.elapsed, c.executed, c.failures
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
        total: u64,
        duration_each_us: u64,
    },
    Failure(),
    Completed(Completed),
}

#[derive(Debug)]
struct TestInfo {
    id: TypeId,
    f_name: &'static str,
    gen_name: &'static str,
    name: String,
    run: bool,
    launch: for<'s> fn(&'s thread::Scope<'s, '_>, mpsc::Sender<Msg>, &TestInfo),
    completed: Option<Completed>,
}

#[derive(Clone, Debug)]
struct Completed {
    executed: u64,
    failures: u64,
    elapsed: Duration,
}

#[derive(Debug)]
pub struct Config {
    pub timeout: Duration,
}

pub fn run(cfg: &Config) -> ExitCode {
    let mut tests = Vec::new();

    register_float::<f32>(&mut tests);
    register_float::<f64>(&mut tests);

    tests.sort_unstable_by_key(|t| (t.f_name, t.gen_name));
    // TODO: pop tests that don't match the filter
    dbg!(&tests);

    let (logfile, logfile_name) = log_file();
    let mut out = Tee(&logfile, io::stdout().lock());
    let elapsed = launch_tests(&mut tests, cfg, &mut out);
    let ret = finish(&tests, elapsed, &mut out);

    drop(out);
    println!("wrote results to {logfile_name}");

    ret
}

fn register_float<F: Float>(v: &mut Vec<TestInfo>) {
    register_generator_for_float::<F, gen::subnorm::SubnormEdge<F>>(v);
    register_generator_for_float::<F, gen::subnorm::SubnormComplete<F>>(v);
}

fn register_generator_for_float<F: Float, G: Generator<F>>(v: &mut Vec<TestInfo>) {
    let f_name = type_name::<F>();
    let gen_name = G::NAME;

    let info = TestInfo {
        id: TypeId::of::<(F, G)>(),
        f_name,
        gen_name,
        name: format!("{f_name} {gen_name}"),
        run: true,
        launch: launch_one::<F, G>,
        completed: None,
    };
    v.push(info)
}

/// Run all tests
///
/// Take `tests` by value so we can mutate it within the thread scope
fn launch_tests(tests: &mut [TestInfo], config: &Config, out: &mut Tee) -> Duration {
    let (tx, rx) = mpsc::channel::<Msg>();
    let mut status: HashMap<_, _> = tests.iter().map(|t| (t.id, false)).collect();
    let total_tests = tests.len();
    let start = Instant::now();

    thread::scope(move |scope| {
        for test in tests.iter() {
            if test.run {
                writeln!(out, "Launching test '{}'", test.name);
                (test.launch)(scope, tx.clone(), &test);
            }
        }

        loop {
            let elapsed = Instant::now() - start;
            if elapsed > config.timeout {
                writeln!(out, "Timeout of {:?} reached", config.timeout);
                break;
            }

            if tests.iter().all(|t| t.completed.is_some()) {
                writeln!(out, "Completed {total_tests} tests in {elapsed:?}");
                break;
            }

            let msg = rx.recv().unwrap();
            msg.handle(tests, out);
        }

        assert_eq!(rx.try_recv().unwrap_err(), mpsc::TryRecvError::Empty);
    });

    Instant::now() - start
}

fn finish(tests: &[TestInfo], total_elapsed: Duration, out: &mut Tee) -> ExitCode {
    writeln!(out, "\nResults:");

    let mut failed_tests = 0;

    for t in tests {
        let Completed {
            executed,
            failures,
            elapsed,
        } = t.completed.as_ref().unwrap();

        let stat = if *failures > 0 { "FAILURE" } else { "SUCCESS" };

        writeln!(
            out,
            "    {stat} for test '{name}'. {passed}/{executed} passed in {elapsed:?}",
            name = t.name,
            passed = executed - failures,
        );

        if *failures > 0 {
            failed_tests += 1;
        }
    }

    writeln!(
        out,
        "{}/{} tests succeeded in {total_elapsed:?}",
        tests.len() - failed_tests,
        tests.len()
    );

    if failed_tests > 0 {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn launch_one<'s, F: Float, G: Generator<F>>(
    s: &'s thread::Scope<'s, '_>,
    tx: mpsc::Sender<Msg>,
    info: &TestInfo,
) {
    s.spawn(move || {
        tx.send(Msg::new::<F, G>(Update::Started));

        let mut est = G::estimated_tests();
        let mut g = G::new();
        let mut executed = 0;
        let mut failures = 0;

        let update_increment = (est / 100).max(1);

        let started = Instant::now();

        while let Some(s) = g.next() {
            executed += 1;
            if executed > est {
                est = executed
            };

            // Send periodic updates
            if executed % update_increment == 0 {
                let elapsed = Instant::now() - started;

                tx.send(Msg::new::<F, G>(Update::Progress {
                    executed,
                    total: est,
                    duration_each_us: (elapsed.as_micros() / u128::from(executed))
                        .try_into()
                        .unwrap(),
                }));
            }
        }

        let elapsed = Instant::now() - started;
        tx.send(Msg::new::<F, G>(Update::Completed(Completed {
            executed,
            failures,
            elapsed,
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

struct Tee<'a>(&'a fs::File, io::StdoutLock<'a>);

impl<'a> Write for Tee<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf).and_then(|_| self.1.write(buf))
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush().and_then(|_| self.1.flush())
    }
}

trait Int:
    Copy
    + fmt::Debug
    + ops::Add<Output = Self>
    + ops::Sub<Output = Self>
    + ops::AddAssign
    + ops::Shl<u32, Output = Self>
    + PartialOrd
    + 'static
{
    const ZERO: Self;
    const ONE: Self;
}

macro_rules! impl_int {
    ($($ty:ty),+) => {
        $(
            impl Int for $ty {
                const ZERO: Self = 0;
                const ONE: Self = 0;
            }
        )+
    }
}

impl_int!(u32, u64);

trait Float: Copy + fmt::LowerExp + FromStr<Err: fmt::Debug> + Sized + 'static {
    /// Unsigned integer of same width
    type Int: Int;

    /// Total bits
    const BITS: u32;

    /// (Stored) bits in the mantissa)
    const MAN_BITS: u32;

    /// Bits in the exponent
    const EXP_BITS: u32 = Self::BITS - Self::MAN_BITS - 1;

    // const MAN_MASK: Self::Int = (Self::Int::ONE << Self::MAN_BITS) - Self::Int::ONE;
    const MAN_MASK: Self::Int;

    fn from_bits(i: Self::Int) -> Self;
    fn to_bits(self) -> Self::Int;
}

macro_rules! impl_float {
    ($($ty:ty, $ity:ty, $bits:literal);+) => {
        $(
            impl Float for $ty {
                type Int = $ity;
                const BITS: u32 = $bits;
                const MAN_BITS: u32 = Self::MANTISSA_DIGITS - 1;
                const MAN_MASK: Self::Int = (Self::Int::ONE << Self::MAN_BITS) - Self::Int::ONE;
                fn from_bits(i: Self::Int) -> Self { Self::from_bits(i) }
                fn to_bits(self) -> Self::Int { self.to_bits() }
            }
        )+
    }
}

impl_float!(f32, u32, 32; f64, u64, 64);

/// Implement this on
trait Generator<F: Float>: Sized + 'static {
    const NAME: &'static str;

    /// Approximate number of tests that will be run
    fn estimated_tests() -> u64;

    /// Create this generator
    fn new() -> Self;

    /// Return the next number in this generator to be tested
    fn next<'a>(&'a mut self) -> Option<&'a str>;
}

const fn const_min(a: u32, b: u32) -> u32 {
    if a <= b {
        a
    } else {
        b
    }
}
