use std::process::ExitCode;

mod args;
mod format;
mod run;

// mimalloc is much faster than the system allocator for the many small
// allocations the interpreter makes.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() -> ExitCode {
    match run::real_main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::from(1)
        }
    }
}
