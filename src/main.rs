use std::{process::ExitCode, time::Duration};

use test_float_parse as tfp;

fn main() -> ExitCode {
    let cfg = tfp::Config {
        timeout: Duration::from_secs(60 * 60 * 3),
    };

    let exit = tfp::run(&cfg);

    exit
}
