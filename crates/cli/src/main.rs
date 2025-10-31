use std::process::ExitCode;

mod args;
mod run;

fn main() -> ExitCode {
    match run::real_main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::from(1)
        }
    }
}
