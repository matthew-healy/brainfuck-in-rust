use brainfuck::{
    interpreter::Interpreter,
    parser::parse,
};

use std::{env, io};

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Useage: program path/to/brainfuck/src.b");
        std::process::exit(1);
    }

    let src = std::fs::read_to_string(&args[1])?;
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdout = io::stdout();
    let stdout = stdout.lock();

    let mut interpreter = Interpreter::new(stdin, stdout);
    interpreter.interpret(parse(&src).unwrap());

    Ok(())
}