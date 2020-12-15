use brainfuck::{
    EOF,
    lexer::Lexer,
    parser::Parser,
};

use std::{
    env,
    io::{self, Read, Write, Bytes}
};

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

    let lexer = Lexer::new(&src);
    let parser = Parser::new(lexer);

    let mut interpreter = Interpreter::new(stdin, stdout);
    interpreter.interpret(parser.parse().unwrap());

    Ok(())
}

use brainfuck::ast::*;

struct Interpreter<R: Read, W: Write> {
    reader: Bytes<R>,
    writer: W,
    runtime: Runtime,
}

impl <R: Read, W: Write> Interpreter<R, W> {
    fn new(reader: R, writer: W) -> Self {
        let reader = reader.bytes();
        Self { reader, writer, runtime: Runtime::new() }
    }

    fn interpret(&mut self, program: Program) {
        program.accept(self);
        self.writer.flush().expect("Handle this error better");
    }
}

impl <R: Read, W: Write> Visitor for Interpreter<R, W> {
    fn visit_increment_pointer(&mut self, times: usize) {
        self.runtime.increment_pointer(times);
    }

    fn visit_decrement_pointer(&mut self, times: usize) {
        self.runtime.decrement_pointer(times);
    }

    fn visit_increment_byte(&mut self, times: usize) {
        self.runtime.increment_byte(times);
    }

    fn visit_decrement_byte(&mut self, times: usize) {
        self.runtime.decrement_byte(times);
    }

    fn visit_write_byte(&mut self, times: usize) {
        for _ in 0..times {
            match self.writer.write(&[self.runtime.get_byte()]) {
                Ok(write_count) if write_count < 1 => {
                    panic!("Handle this error better");
                },
                Err(_error) => {
                    panic!("Also handle this error better")
                },
                Ok(_) => (),
            }
        }
    }

    fn visit_read_byte(&mut self, times: usize) {
        if let Err(_e) = self.writer.flush() {
            panic!("Handle this better");
        }
        for _ in 0..times {
            match self.reader.next() {
                Some(Ok(byte)) => self.runtime.set_byte(byte),
                None => self.runtime.set_byte(EOF),
                Some(Err(_error)) => panic!("Handle this error better"),
            }
        }
    }

    fn visit_loop(&mut self, block: &Block) {
        while self.runtime.get_byte() > 0 {
            block.accept(self);
        }
    }
}

struct Runtime {
    memory: [u8; 30_000],
    pointer: usize,
}

impl Runtime {
    fn new() -> Self {
        Runtime { memory: [0; 30_000], pointer: 0 }
    }

    fn increment_pointer(&mut self, times: usize) {
        self.pointer += times;
        if self.pointer >= self.memory.len() {
            panic!("Handle this error better")
        }
    }

    fn decrement_pointer(&mut self, times: usize) {
        if times > self.pointer {
            panic!("Handle this error also better")
        }
        self.pointer -= times;
    }

    fn increment_byte(&mut self, times: usize) {
        let pointee = self.memory[self.pointer];
        self.memory[self.pointer] = pointee.wrapping_add(times as u8);
    }

    fn decrement_byte(&mut self, times: usize) {
        let pointee = self.memory[self.pointer];
        self.memory[self.pointer] = pointee.wrapping_sub(times as u8);
    }

    fn get_byte(&mut self) -> u8 {
        self.memory[self.pointer]
    }

    fn set_byte(&mut self, to: u8) {
        self.memory[self.pointer] = to;
    }
}