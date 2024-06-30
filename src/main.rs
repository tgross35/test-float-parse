use std::{process::ExitCode, time::Duration};

use test_float_parse as tfp;

fn main() -> ExitCode {
    if cfg!(debug_assertions) {
        println!(
            "WARNING: running in debug mode. Release mode is recommended to reduce test duration."
        );
    }

    let cfg = tfp::Config {
        timeout: Duration::from_secs(60 * 60 * 3),
    };

    let exit = tfp::run(&cfg);

    exit
}
