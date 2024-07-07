//! Progress bars and such.

use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;
use std::time::{Duration, SystemTime};

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};

use crate::{Completed, Config, EarlyExit, FinishedAll, TestInfo};

/// Templates for progress bars.
const PB_TEMPLATE:&str =
        "[{elapsed:3} {percent:3}%] {bar:20.cyan/blue} NAME ({pos}/{len}, {msg} f, {per_sec}, eta {eta})";
const PB_TEMPLATE_FINAL: &str =
    "[{elapsed:3} {percent:3}%] NAME ({pos}/{len}, {msg:.COLOR}, {per_sec}, {elapsed_precise})";

/// Create a new progress bar within a multiprogress bar.
pub fn create_pb(
    mp: &MultiProgress,
    total_tests: u64,
    short_name: &str,
    all_bars: &mut Vec<ProgressBar>,
) -> ProgressBar {
    let pb = mp.add(ProgressBar::new(total_tests));
    let short_name_padded = format!("{short_name:16}");
    let pb_style = ProgressStyle::with_template(&PB_TEMPLATE.replace("NAME", &short_name_padded))
        .unwrap()
        .progress_chars("##-");

    pb.set_style(pb_style.clone());
    pb.set_message("0");
    all_bars.push(pb.clone());
    pb
}

/// Removes the status bar and replace it with a message.
pub fn finalize_pb(pb: &ProgressBar, short_name: &str, c: &Completed) {
    let short_name_padded = format!("{short_name:16}");
    let f = c.failures;

    // Use a tuple so we can use colors
    let (color, msg, finish_pb): (&str, String, fn(&ProgressBar, String)) = match &c.result {
        Ok(FinishedAll) if f > 0 => (
            "red",
            format!("{f} f (finished with errors)",),
            ProgressBar::finish_with_message,
        ),
        Ok(FinishedAll) => (
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
    finish_pb(pb, msg);
}

/// Print final messages after all tests are complete.
pub fn finish(
    tests: &[TestInfo],
    total_elapsed: Duration,
    cfg: &Config,
    out: &mut Tee,
) -> ExitCode {
    out.write_sout("\n\nResults:");

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
            Ok(FinishedAll) => (),
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

/// indicatif likes to eat panic messages. This workaround isn't ideal, but it improves things.
/// <https://github.com/console-rs/indicatif/issues/121>.
pub fn set_panic_hook(drop_bars: Vec<ProgressBar>) {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        for bar in &drop_bars {
            bar.abandon();
            println!();
            io::stdout().flush().unwrap();
            io::stderr().flush().unwrap();
        }
        hook(info);
    }));
}

/// Open a file with a reasonable name that we can dump data to.
pub fn create_log_file() -> (fs::File, String) {
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let name = format!("parse-float-{now}.txt");
    (
        fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&name)
            .unwrap(),
        name,
    )
}

/// Tee output to the file and either stdout or a progress bar.
pub struct Tee<'a> {
    pub f: &'a fs::File,
}

impl<'a> Tee<'a> {
    /// Write to both the file and above the multiprogress bar. Includes newline.
    pub fn write_mp(&mut self, mb: &MultiProgress, s: impl Into<String>) {
        let mut s = s.into();
        s.push('\n');
        self.f.write_all(s.as_bytes()).unwrap();
        s.pop();
        mb.println(s).unwrap();
    }

    /// Write to both the file and stdout. Includes newline.
    pub fn write_sout(&mut self, s: impl Into<String>) {
        let mut s = s.into();
        s.push('\n');
        self.f.write_all(s.as_bytes()).unwrap();
        io::stdout().write_all(s.as_bytes()).unwrap();
    }
}
