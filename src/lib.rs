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
    pub mod exhaustive;
    pub mod exponents;
    pub mod fuzz;
    pub mod integers;
    pub mod long_fractions;
    pub mod sparse;
    pub mod subnorm;
}

pub const SEED: [u8; 32] = *b"3.141592653589793238462643383279";

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
    let pb_style = ProgressStyle::with_template(
        "[{elapsed}] {bar:40.cyan/blue} {pos}/{len} ({percent}%) {msg}",
    )
    .unwrap()
    .progress_chars("##-");

    for test in tests.iter() {
        out.write_sout(format!("Launching test '{}'", test.name));
    }

    let start = Instant::now();

    let mut panic_drop_bars = Vec::new();

    for test in tests.iter_mut() {
        let mut pb = mp.add(ProgressBar::new(test.estimated_tests));
        pb.set_style(pb_style.clone());
        pb.set_message(test.short_name.clone());
        panic_drop_bars.push(pb.clone());
        test.pb = Some(pb);
    }

    // indicatif likes to eat panic messages <https://github.com/console-rs/indicatif/issues/121>
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

fn launch_one<'s, F: Float, G: Generator<F>>(
    scope: &'s thread::Scope<'s, '_>,
    tx: mpsc::Sender<Msg>,
    info: &TestInfo,
) {
    scope.spawn(move || {
        tx.send(Msg::new::<F, G>(Update::Started));

        let mut est = G::estimated_tests();
        let mut gen = G::new();
        let mut executed = 0;
        let mut failures = 0;
        let mut result = Ok(());

        let checks_per_update = (est / 100000).clamp(1, 1000);
        let started = Instant::now();

        for test_str in gen {
            executed += 1;

            match validate::validate::<F>(&test_str, G::PATTERNS_CONTAIN_NAN) {
                Ok(()) => (),
                Err(e) => tx.send(Msg::new::<F, G>(e)).unwrap(),
            };

            // Send periodic updates
            if executed % checks_per_update == 0 {
                let elapsed = Instant::now() - started;

                tx.send(Msg::new::<F, G>(Update::Progress {
                    executed,
                    duration_each_us: (elapsed.as_micros() / u128::from(executed))
                        .try_into()
                        .unwrap(),
                }))
                .unwrap();
            }
        }

        let elapsed = Instant::now() - started;

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
    + TryFrom<u32, Error: fmt::Debug>
    + TryFrom<u64, Error: fmt::Debug>
    + TryFrom<u128, Error: fmt::Debug>
    + TryInto<u64, Error: fmt::Debug>
    + TryInto<u32, Error: fmt::Debug>
    + ToBigInt
    + PartialOrd
    + Integer
    + 'static
{
    type Signed: Int;
    type Bytes: Default + AsMut<[u8]>;

    const BITS: u32;
    const ZERO: Self;
    const ONE: Self;
    const MAX: Self;

    fn to_signed(self) -> Self::Signed;
    fn wrapping_neg(self) -> Self;
    fn trailing_zeros(self) -> u32;
    fn from_le_bytes(bytes: Self::Bytes) -> Self;

    fn hex(self) -> String {
        format!("{:x}", self)
    }
}

macro_rules! impl_int {
    ($($uty:ty, $sty:ty);+) => {
        $(
            impl Int for $uty {
                type Signed = $sty;
                type Bytes = [u8; Self::BITS as usize / 8];
                const BITS: u32 = Self::BITS;
                const ZERO: Self = 0;
                const ONE: Self = 1;
                const MAX: Self = Self::MAX;
                fn to_signed(self) -> Self::Signed {
                    self.try_into().unwrap()
                }
                fn wrapping_neg(self) -> Self {
                    self.wrapping_neg()
                }
                fn trailing_zeros(self) -> u32 {
                    self.trailing_zeros()
                }
                fn from_le_bytes(bytes: Self::Bytes) -> Self {
                    Self::from_be_bytes(bytes)
                }
            }

            impl Int for $sty {
                type Signed = Self;
                type Bytes = [u8; Self::BITS as usize / 8];
                const BITS: u32 = Self::BITS;
                const ZERO: Self = 0;
                const ONE: Self = 1;
                const MAX: Self = Self::MAX;
                fn to_signed(self) -> Self::Signed {
                    self
                }
                fn wrapping_neg(self) -> Self {
                    self.wrapping_neg()
                }
                fn trailing_zeros(self) -> u32 {
                    self.trailing_zeros()
                }
                fn from_le_bytes(bytes: Self::Bytes) -> Self {
                    Self::from_be_bytes(bytes)
                }
            }
        )+
    }
}

impl_int!(u32, i32; u64, i64);

trait Float:
    Copy + fmt::Debug + fmt::LowerExp + FromStr<Err: fmt::Display> + FloatConstants + Sized + 'static
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

    /// A saturated exponent (all ones)
    const EXP_SAT: u32 = (1 << Self::EXP_BITS) - 1;

    /// The exponent bias, also its maximum value
    const EXP_BIAS: u32 = Self::EXP_SAT >> 1;

    const MAN_MASK: Self::Int;
    const SIGN_MASK: Self::Int;

    fn from_bits(i: Self::Int) -> Self;
    fn to_bits(self) -> Self::Int;

    fn is_sign_negative(self) -> bool {
        (self.to_bits() & Self::SIGN_MASK) > Self::Int::ZERO
    }

    /// Exponent without adjustment
    fn exponent(self) -> u32 {
        ((self.to_bits() >> Self::MAN_BITS) & Self::EXP_SAT.try_into().unwrap())
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
trait Generator<F: Float>: Iterator<Item = String> + 'static {
    const NAME: &'static str;
    const SHORT_NAME: &'static str;
    /// If false (default), validation will assert on NaN.
    const PATTERNS_CONTAIN_NAN: bool = false;

    /// Approximate number of tests that will be run
    fn estimated_tests() -> u64;

    /// Create this generator
    fn new() -> Self;

    // /// Return the next number in this generator to be tested
    // fn next<'a>(&'a mut self) -> Option<&'a str>;
}

const fn const_min(a: u32, b: u32) -> u32 {
    if a <= b {
        a
    } else {
        b
    }
}

/// Turn the integer into a float then print it in exponential format
fn update_buf_from_bits<F: Float>(s: &mut String, i: F::Int) -> &str {
    use std::fmt::Write;
    s.clear();
    write!(s, "{:e}", F::from_bits(i));
    s
}
