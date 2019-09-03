use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;


/// this is the configuration type for this Program.
pub struct Config {
    pub query: String,
    pub filename: String,
    pub case_sensitive: bool,
}


/*
// a failable method creating a new Configuration based on the
// command line arguments
impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 3 {
            return Err("not enough arguments!");
        }

        let query = args[1].clone();
        let filename = args[2].clone();

        let case_sensitive = env::var("CASE_INSENSITIVE").is_err();

        Ok(Config { query, filename, case_sensitive })
    }
} // */

/// a failable method creating a new Configuration based on the
/// command line arguments
impl Config {
    pub fn new(mut args: std::env::Args) -> Result<Config, &'static str> {
        args.next();

        let query = match args.next() {
            Some(arg) => arg,
            None      => return Err("Didn't get a query."),
        };

        let filename = match args.next() {
            Some(arg) => arg,
            None      => return Err("Didn't get a file name"),
        };

        let case_sensitive = env::var("CASE_INSENSITIVE").is_err();

        Ok(Config { query, filename, case_sensitive })
    }
}


/// the starting point for the program. Taking a configuration
/// and trying to run it, could fail.
pub fn run(config: Config) -> Result<(), Box<Error>> {
    let mut f = File::open(config.filename)?;

    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let result = search(&config.query, &contents, config.case_sensitive);

    for line in result {
        println!("{}", line);
    }

    Ok(())
}


/// this is the actual case-sensitive search function, taking the
/// query and the contents of the file and returning all matching lines.
fn search<'a>(query: &str, contents: &'a str, case_sensitive: bool) -> Vec<&'a str> {
    if case_sensitive {
        contents.lines()
            .filter( |line| line.contains(query))
            .collect()
    } else {
        let query = query.to_lowercase();
        contents.lines()
            .filter( |line| line.to_lowercase().contains(&query) )
            .collect()
    }
}

/*
fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut results = Vec::new();

    for line in contents.lines() {
        if line.contains(query) {
            results.push(line);
        }
    }
    results
} // */


/*
// this is the case-insensitive variant of the search function, taking the
// query and the contents of the file and returning all matching lines.
fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let query = query.to_lowercase();
    let mut results = Vec::new();

    for line in contents.lines() {
        if line.to_lowercase().contains(&query) {
            results.push(line);
        }
    }
    results
} // */



/// some tests regarding the search functionality
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn case_sensitive() {
        let query = "duct";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
Duct tape.";

        assert_eq!(
            vec!["safe, fast, productive."],
            search(query, contents, true)
        );
    }

    #[test]
    fn case_insensitive() {
        let query = "rUsT";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
Trust me.";

        assert_eq!(
            vec!["Rust:", "Trust me."],
            search(query, contents, false)
        );
    }
}
