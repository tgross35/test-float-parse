use std::{process::ExitCode, time::Duration};

use test_float_parse as tfp;

static HELP: &str = r#"Usage:

  ./test-float-parse [--timeout x] [--exclude x] [INCLUDE ...]
  ./test-float-parse --list

Args:

  INCLUDE                  Include only tests with names containing these
                           strings. If this argument is not specified, all tests
                           are run.
  --timeout N              Exit after this amount of time (in seconds).
  --exclude FILTER         Skip tests containing this string. May be specified
                           more than once.
  --list                   List available tests.
  --max-failures N         Limit to N failures per test. Defaults to 20. Pass
                           "--max-failures none" to remove this limit.
    
"#;

enum ArgMode {
    Any,
    Timeout,
    Exclude,
    MaxFailures,
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
        max_failures: Some(20),
    };

    let mut mode = ArgMode::Any;
    let mut include = Vec::new();
    let mut exclude = Vec::new();

    // Simple command argument parser
    for arg in args {
        match mode {
            ArgMode::Any if arg == "--timeout" => mode = ArgMode::Timeout,
            ArgMode::Any if arg == "--exclude" => mode = ArgMode::Exclude,
            ArgMode::Any if arg == "--max-failures" => mode = ArgMode::MaxFailures,
            ArgMode::Any if arg.starts_with("--") => panic!("Unknown argument {arg}"),
            ArgMode::Any => {
                include.push(arg);
                mode = ArgMode::Any;
            }
            ArgMode::Timeout => {
                cfg.timeout = Duration::from_secs(arg.parse().unwrap());
                mode = ArgMode::Any;
            }
            ArgMode::MaxFailures => {
                if arg == "none" {
                    cfg.max_failures = None;
                    continue;
                }
                cfg.max_failures = Some(arg.parse().unwrap());
                mode = ArgMode::Any;
            }
            ArgMode::Exclude => {
                exclude.push(arg);
                mode = ArgMode::Any;
            }
        }
    }

    tfp::run(cfg, &include, &exclude)
}
