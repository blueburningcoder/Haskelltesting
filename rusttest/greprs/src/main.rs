extern crate greprslib;

use std::env;
use std::process;
use std::io::prelude::*;

use greprslib::Config;


/// this is the main function of this rust program, essentially only
/// reading the args and handling errors, all other functionality
/// is delegated to the 'library'.
fn main() {

    // reading the command line arguments and collecting them in a vector
//    let args: Vec<String> = env::args().collect();

    // this is the handle for the stderr
    let mut stderr = std::io::stderr();


    // if there were some problems creating the configuration
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        writeln!(&mut stderr, "Problem parsing arguments: {}", err)
            .expect("Could not write to stderr");
        process::exit(1);
    });

    // running the actual program, and catching possible errors.
    if let Err(e) = greprslib::run(config) {
        writeln!(&mut stderr, "Application error: {}", e)
            .expect("Could not write to stderr");

        process::exit(1);
    }
}

