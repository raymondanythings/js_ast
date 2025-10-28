use std::env;
use std::fs;
use std::io::{self, Read};

use js_ast::{ParseError, parse_program, program_to_json};

fn main() {
    if let Err(error) = run() {
        eprintln!("Error: {}", error);
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut args: Vec<String> = env::args().skip(1).collect();
    let json_output = if let Some(pos) = args.iter().position(|arg| arg == "--json" || arg == "-j")
    {
        args.remove(pos);
        true
    } else {
        false
    };

    if args.len() > 1 {
        return Err("Expected at most one input path".into());
    }

    let source = if let Some(path) = args.pop() {
        fs::read_to_string(path)?
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };

    let program = parse_program(&source)
        .map_err(|err: ParseError| Box::new(err) as Box<dyn std::error::Error>)?;

    if json_output {
        let json = program_to_json(&program)?;
        println!("{}", json);
    } else {
        println!("{:#?}", program);
    }

    Ok(())
}
