#![forbid(unsafe_code)]

mod error;

use crate::error::*;

mod token {
    pub const INCREMENT_POINTER: char = '>';
    pub const DECREMENT_POINTER: char = '<';
    pub const INCREMENT_BYTE: char = '+';
    pub const DECREMENT_BYTE: char = '-';
    pub const WRITE_BYTE: char = '.';
    pub const READ_BYTE: char = ',';
    pub const LOOP_START: char = '[';
    pub const LOOP_END: char = ']';

    pub const INSTRUCTIONS: &[char; 8] = &[
        INCREMENT_POINTER,
        DECREMENT_POINTER,
        INCREMENT_BYTE,
        DECREMENT_BYTE,
        WRITE_BYTE,
        READ_BYTE,
        LOOP_START,
        LOOP_END
    ];
}

#[derive(Debug, Eq, PartialEq)]
pub enum InstructionKind {
    IncrementPointer,
    DecrementPointer,
    IncrementByte,
    DecrementByte,
    WriteByte,
    ReadByte,
    LoopStart { end_index: usize },
    LoopEnd { start_index: usize }
}

impl InstructionKind {
    fn set_jump_index(&mut self, jump_index: usize) {
        use InstructionKind::*;
        match self {
            LoopStart { end_index } => *end_index = jump_index,
            LoopEnd { start_index } => *start_index = jump_index,
            _ => panic!("Attempted to set jump_index {} on {:?}", jump_index, self)
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Instruction {
    index: usize,
    kind: InstructionKind,
    times: usize,
}

impl Instruction {
    fn new(index: usize, c: char) -> Self {
        use token::*;
        use InstructionKind::*;
        let kind = match c {
            INCREMENT_POINTER => IncrementPointer,
            DECREMENT_POINTER => DecrementPointer,
            INCREMENT_BYTE => IncrementByte,
            DECREMENT_BYTE => DecrementByte,
            READ_BYTE => ReadByte,
            WRITE_BYTE => WriteByte,
            LOOP_START => LoopStart { end_index: 0 },
            LOOP_END => LoopEnd { start_index: 0 },
            c => panic!("Unrecognised command {}", c),
        };
        Instruction {
            index, 
            kind,
            times: 1,
        }
    }

    fn increment(&mut self) {
        self.times += 1;
    }
}

pub fn parse(src: &str) -> Result<Vec<Instruction>, Error> {
    let mut loop_depth = 0;
    let mut instructions: Vec<Instruction> = Vec::new();

    for c in src.chars().filter(|c| token::INSTRUCTIONS.contains(c)) {
        match c {
            token::LOOP_END if loop_depth == 0 => {
                return Err(Error::UnbalancedBrackets)
            }
            token::LOOP_END => loop_depth -= 1,
            token::LOOP_START => loop_depth += 1,
            _ => {}
        };

        let current = Instruction::new(instructions.len(), c);
        match instructions.last_mut() {
            Some(previous) if previous.kind == current.kind => {
                previous.increment();
            },
            _ => instructions.push(current),
        };
    }

    if loop_depth > 0 {
        return Err(Error::UnbalancedBrackets)
    }

    for i in 0..instructions.len() {
        let mut update_jump_index: Option<usize> = None;
        let Instruction { kind, times, .. } = &instructions[i];

        match kind {
            InstructionKind::LoopStart { .. } => {
                let mut loop_starts = *times;

                for j in (i + 1)..instructions.len() {
                    let Instruction { kind, times, ..} = &instructions[j];
                    match kind {
                        InstructionKind::LoopEnd { .. } => {
                            let loop_ends = *times;
                            loop_starts = loop_starts.saturating_sub(loop_ends);
                            if loop_starts == 0 {
                                update_jump_index = Some(j + 1);
                                break;
                            }
                        },
                        InstructionKind::LoopStart { .. } => {
                            let nested_loop_starts = *times;
                            loop_starts += nested_loop_starts;
                        },
                        _ => {},
                    }
                }
            },
            InstructionKind::LoopEnd { .. } => {
                let mut loop_ends = 1_usize;

                for j in (0..i).rev() {
                    let Instruction { kind, times, .. } = &instructions[j];
                    match kind {
                        InstructionKind::LoopStart { .. } => {
                            let loop_starts = *times;
                            loop_ends = loop_ends.saturating_sub(loop_starts);
                            if loop_ends == 0 {
                                if i == j + 1 {
                                    return Err(Error::InfiniteLoop);
                                }
                                update_jump_index = Some(j + 1);
                                break;
                            }
                        },
                        InstructionKind::LoopEnd { .. } => {
                            let nested_loop_ends = *times;
                            loop_ends += nested_loop_ends;
                        },
                        _ => {},
                    }
                }
            },
            _ => {},
        }

        if let Some(jump_index) = update_jump_index {
            instructions[i].kind.set_jump_index(jump_index);
        }
    }

    Ok(instructions)
}