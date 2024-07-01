#![allow(unused)]

mod validate;

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use num::bigint::ToBigInt;
use num::Integer;
use std::any::{type_name, TypeId};
use std::collections::HashMap;
use std::io::prelude::*;
use std::ops::ControlFlow;
use std::process::ExitCode;
use std::str::FromStr;
use std::{fmt, fs, io, mem, ops, sync::mpsc, thread, time};
use time::{Duration, Instant};
use validate::FloatConstants;

mod gen {
    pub mod few_ones;
    pub mod short_decimals;
    pub mod subnorm;
    pub mod u32_small;
}

// Nothing up my sleeve: Just (PI - 3) in base 16.
#[allow(dead_code)]
pub const SEED: [u32; 3] = [0x243f_6a88, 0x85a3_08d3, 0x1319_8a2e];

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
                        "Failure in '{}': {}. parsing {input}",
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
}

impl Failure {
    fn msg(self) -> &'static str {
        match self {
            Failure::RoundedToZero => "incorrectly rounded to 0 (expected nonzero)",
            Failure::RoundedToInf => "incorrectly rounded to +inf (expected finite)",
            Failure::RoundedToNegInf => "incorrectly rounded to -inf (expected finite)",
        }
    }
}

#[derive(Debug)]
struct TestInfo {
    id: TypeId,
    f_name: &'static str,
    gen_name: &'static str,
    gen_short_name: &'static str,
    name: String,
    short_name: String,
    run: bool,
    estimated_tests: u64,
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

pub fn run(cfg: &Config) -> ExitCode {
    let mut tests = Vec::new();

    register_float::<f32>(&mut tests);
    register_float::<f64>(&mut tests);

    tests.sort_unstable_by_key(|t| (t.f_name, t.gen_name));
    // TODO: pop tests that don't match the filter

    let (logfile, logfile_name) = log_file();
    let mut out = Tee { f: &logfile };
    let elapsed = launch_tests(&mut tests, cfg, &mut out);
    let ret = finish(&tests, elapsed, &mut out);

    drop(out);
    println!("wrote results to {logfile_name}");

    ret
}

fn register_float<F: Float>(v: &mut Vec<TestInfo>)
where
    <F::Int as TryFrom<u128>>::Error: std::fmt::Debug,
{
    register_generator_for_float::<F, gen::subnorm::SubnormEdge<F>>(v);
    register_generator_for_float::<F, gen::subnorm::SubnormComplete<F>>(v);
    register_generator_for_float::<F, gen::short_decimals::ShortDecimals>(v);
    register_generator_for_float::<F, gen::few_ones::FewOnes<F>>(v);
    register_generator_for_float::<F, gen::u32_small::SmallInt>(v);
}

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
        estimated_tests: G::estimated_tests(),
        pb: None,
        completed: None,
    };
    v.push(info)
}

enum EndCondition {
    Timeout,
    Success(Duration),
}

/// Run all tests
///
/// Take `tests` by value so we can mutate it within the thread scope
fn launch_tests(tests: &mut [TestInfo], config: &Config, out: &mut Tee) -> Duration {
    let (tx, rx) = mpsc::channel::<Msg>();
    let total_tests = tests.len();
    let mp = MultiProgress::new();
    let pb_style = ProgressStyle::with_template("[{elapsed}] {bar:40.cyan/blue} {pos}/{len} {msg}")
        .unwrap()
        .progress_chars("##-");

    for test in tests.iter() {
        out.write_sout(format!("Launching test '{}'", test.name));
    }

    let start = Instant::now();

    thread::scope(move |scope| {
        for test in tests.iter_mut() {
            if test.run {
                let mut pb = mp.add(ProgressBar::new(test.estimated_tests));
                pb.set_style(pb_style.clone());
                pb.set_message(test.short_name.clone());
                test.pb = Some(pb);

                (test.launch)(scope, tx.clone(), &test);
            }
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

    let mut failed_tests = 0;

    for t in tests {
        let Completed {
            executed,
            failures,
            elapsed,
            result,
        } = t.completed.as_ref().unwrap();

        let stat = if result.is_err() {
            "ERROR"
        } else if *failures > 0 {
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

        if *failures > 0 || result.is_err() {
            failed_tests += 1;
        }
    }

    out.write_sout(format!(
        "{}/{} tests succeeded in {total_elapsed:?}",
        tests.len() - failed_tests,
        tests.len()
    ));

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
        let mut result = Ok(());

        let update_increment = (est / 100).max(1);
        let started = Instant::now();

        while let Some(s) = g.next() {
            executed += 1;

            // Send periodic updates
            if executed % update_increment == 0 {
                let elapsed = Instant::now() - started;

                tx.send(Msg::new::<F, G>(Update::Progress {
                    executed,
                    duration_each_us: (elapsed.as_micros() / u128::from(executed))
                        .try_into()
                        .unwrap(),
                }));
            }
        }

        let elapsed = Instant::now() - started;

        // Warn about bad estimates
        if executed > est {
            result = Err(format!(
                "executed tests > estimated ({executed} {est}) for {}",
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

trait Int:
    Copy
    + fmt::Debug
    + fmt::Display
    + fmt::LowerHex
    + ops::Add<Output = Self>
    + ops::Sub<Output = Self>
    + ops::Shl<u32, Output = Self>
    + ops::Shr<u32, Output = Self>
    + ops::BitAnd<Output = Self>
    + ops::BitOr<Output = Self>
    + ops::Not<Output = Self>
    + ops::AddAssign
    + ops::BitAndAssign
    + ops::BitOrAssign
    + From<u8>
    + TryFrom<i8>
    + TryFrom<u64>
    + TryFrom<u128>
    + TryInto<u64, Error: fmt::Debug>
    + TryInto<u32, Error: fmt::Debug>
    + ToBigInt
    + PartialOrd
    + Integer
    + 'static
{
    type Signed: Int;

    const BITS: u32;
    const ZERO: Self;
    const ONE: Self;

    fn to_signed(self) -> Self::Signed;

    fn wrapping_neg(self) -> Self;

    fn hex(self) -> String {
        format!("{:x}", self)
    }
}

macro_rules! impl_int {
    ($($uty:ty, $sty:ty);+) => {
        $(
            impl Int for $uty {
                type Signed = $sty;
                const BITS: u32 = Self::BITS;
                const ZERO: Self = 0;
                const ONE: Self = 1;
                fn to_signed(self) -> Self::Signed {
                    self.try_into().unwrap()
                }
                fn wrapping_neg(self) -> Self {
                    self.wrapping_neg()
                }
            }

            impl Int for $sty {
                type Signed = Self;
                const BITS: u32 = Self::BITS;
                const ZERO: Self = 0;
                const ONE: Self = 1;
                fn to_signed(self) -> Self::Signed {
                    self
                }
                fn wrapping_neg(self) -> Self {
                    self.wrapping_neg()
                }
            }
        )+
    }
}

impl_int!(u32, i32; u64, i64);

trait Float:
    Copy + fmt::LowerExp + FromStr<Err: fmt::Display> + FloatConstants + Sized + 'static
{
    /// Unsigned integer of same width
    type Int: Int<Signed = Self::SInt>;
    type SInt: Int;

    /// Total bits
    const BITS: u32;

    /// (Stored) bits in the mantissa)
    const MAN_BITS: u32;

    /// Bits in the exponent
    const EXP_BITS: u32 = Self::BITS - Self::MAN_BITS - 1;
    const EXP_MAX: u32 = (1 << Self::EXP_BITS) - 1;
    const EXP_BIAS: u32 = Self::EXP_MAX >> 1;

    // const MAN_MASK: Self::Int = (Self::Int::ONE << Self::MAN_BITS) - Self::Int::ONE;
    const MAN_MASK: Self::Int;
    const SIGN_MASK: Self::Int;

    fn from_bits(i: Self::Int) -> Self;
    fn to_bits(self) -> Self::Int;

    fn is_sign_negative(self) -> bool {
        (self.to_bits() & Self::SIGN_MASK) > Self::Int::ZERO
    }

    /// Exponent without adjustment
    fn exponent(self) -> u32 {
        (self.to_bits() & (!Self::SIGN_MASK) >> Self::MAN_BITS)
            .try_into()
            .unwrap()
    }

    /// Adjusted for the bias
    fn exponent_adj(self) -> i32 {
        self.exponent() as i32 - Self::EXP_BIAS as i32
    }

    fn mantissa(self) -> Self::Int {
        self.to_bits() & Self::MAN_MASK
    }
}

macro_rules! impl_float {
    ($($ty:ty, $ity:ty, $bits:literal);+) => {
        $(
            impl Float for $ty {
                type Int = $ity;
                type SInt = <Self::Int as Int>::Signed;
                const BITS: u32 = $bits;
                const MAN_BITS: u32 = Self::MANTISSA_DIGITS - 1;
                const MAN_MASK: Self::Int = (Self::Int::ONE << Self::MAN_BITS) - Self::Int::ONE;
                const SIGN_MASK: Self::Int = Self::Int::ONE << (Self::BITS-1);
                fn from_bits(i: Self::Int) -> Self { Self::from_bits(i) }
                fn to_bits(self) -> Self::Int { self.to_bits() }
            }
        )+
    }
}

impl_float!(f32, u32, 32; f64, u64, 64);

/// Implement this on
trait Generator<F: Float>: 'static {
    const NAME: &'static str;
    const SHORT_NAME: &'static str;

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
