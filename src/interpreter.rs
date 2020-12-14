use brainfuck::{
    ARRAY_LEN,
    EOF,
    error::Error,
    Instruction,
    InstructionKind,
    parse,
};

use std::{
    env,
    io::{self, Read, Write, Bytes}
};

fn run<R: Read, W: Write>(instructions: &[Instruction], mut input: Bytes<R>, output: &mut W) -> Result<(), Error> {
    let mut memory = [0_u8; ARRAY_LEN];
    let mut pointer = 0;
    let mut instruction_pointer = 0;

    while instruction_pointer < instructions.len() {
        let Instruction { kind, times, .. } = &instructions[instruction_pointer];
        use InstructionKind::*;
        match kind {
            IncrementPointer => {
                pointer += times;
                if pointer >= ARRAY_LEN {
                    return Err(Error::PointerAboveLimit)
                }
                instruction_pointer += 1;
            },
            DecrementPointer => {
                if *times > pointer {
                    return Err(Error::PointerBelowZero)
                }
                pointer -= times;
                instruction_pointer += 1;
            },
            IncrementByte => {
                memory[pointer] = memory[pointer].wrapping_add(*times as u8);
                instruction_pointer += 1;
            },
            DecrementByte => {
                memory[pointer] = memory[pointer].wrapping_sub(*times as u8);
                instruction_pointer += 1;
            },
            WriteByte => {
                for _ in 0..*times {
                    let result = output.write(&[memory[pointer]]);
                    match result {
                        Ok(bytes_written) if bytes_written < 1 => {
                            panic!("Failed to write byte {} to output", memory[pointer]);
                        },
                        Err(error) => {
                            panic!("Failed to write byte to out with error {}", error);
                        },
                        Ok(_) => (),
                    }
                }
                instruction_pointer += 1;
            },
            ReadByte => {
                if let Err(error) = output.flush() {
                    panic!("Error while flushing out: {}", error);
                }
                for _ in 0..*times {
                    let maybe_byte = input.next();
                    match maybe_byte {
                        Some(Ok(byte)) => {
                            memory[pointer] = byte;
                        },
                        None => {
                            memory[pointer] = EOF;
                        },
                        Some(Err(error)) => {
                            panic!("Error while trying to ready byte from input: {}", error);
                        },
                    }
                    instruction_pointer += 1;
                }
            },
            LoopStart { end_index } => {
                if memory[pointer] == 0 {
                    instruction_pointer = *end_index;
                } else {
                    instruction_pointer += 1;
                }
            },
            LoopEnd { start_index } => {
                if memory[pointer] != 0 {
                    instruction_pointer = *start_index;
                } else {
                    instruction_pointer += 1;
                }
            },
        }
    }
    Ok(())
}

fn parse_and_run<R: Read, W: Write>(src: &str, input: Bytes<R>, output: &mut W) -> Result<(), Error> {
    let instructions = parse(src)?;
    run(&instructions, input, output)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Useage: program path/to/brainfuck/src.b");
        std::process::exit(1);
    }

    let src = std::fs::read_to_string(&args[1])?;
    let stdin = io::stdin();
    let input = stdin.lock().bytes();
    let stdout = io::stdout();
    let mut output = stdout.lock();

    parse_and_run(&src, input, &mut output)?;
    output.flush()?;

    Ok(())
}