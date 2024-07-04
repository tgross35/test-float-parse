use std::{process::ExitCode, time::Duration};

use test_float_parse as tfp;

static HELP: &str = "Usage:

  ./test-float-parse [--timeout x] [--exclude x] [include1 include2 ...]
  ./test-float-parse --list

Args:

  --timeout                Exit after this amount of time (in seconds).
  --exclude                Skip tests containing this string. May be specified
                           more than once.
  include (positional)     Include only tests with names containing these
                           strings. If this argument is not specified, all tests
                           are run.
  --list                   List available tests.
    
";

enum ArgMode {
    Any,
    Timeout,
    Exclude,
}

fn main() -> ExitCode {
    if cfg!(debug_assertions) {
        println!(
            "WARNING: running in debug mode. Release mode is recommended to reduce test duration."
        );
    }

    let args: Vec<_> = std::env::args().skip(1).collect();
    if args.iter().any(|arg| arg == "--help" || arg == "-h") {
        println!("{HELP}");
        return ExitCode::SUCCESS;
    }

    if args.iter().any(|arg| arg == "--list") {
        let tests = tfp::register_tests();
        println!("Available tests:");
        tests.iter().for_each(|t| println!("{}", t.name));

        return ExitCode::SUCCESS;
    }

    let mut cfg = tfp::Config {
        timeout: Duration::from_secs(60 * 60 * 3),
    };

    let mut mode = ArgMode::Any;
    let mut include = Vec::new();
    let mut exclude = Vec::new();

    // Simple command argument parser
    for arg in args {
        match mode {
            ArgMode::Any if arg == "--timeout" => mode = ArgMode::Timeout,
            ArgMode::Any if arg == "--exclude" => mode = ArgMode::Exclude,
            ArgMode::Any => {
                include.push(arg);
                mode = ArgMode::Any;
            }
            ArgMode::Timeout => {
                cfg.timeout = Duration::from_secs(arg.parse().unwrap());
                mode = ArgMode::Any;
            }
            ArgMode::Exclude => {
                exclude.push(arg);
                mode = ArgMode::Any;
            }
        }
    }

    tfp::run(&cfg, &include, &exclude)
}
