#![forbid(unsafe_code)]

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

pub const ARRAY_LEN: usize = 30_000;

#[derive(Debug, Eq, PartialEq)]
pub enum InstructionKind {
    IncrememntPointer,
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
            INCREMENT_POINTER => IncrememntPointer,
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

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    PointerBelowZero,
    PointerAboveLimit,
    UnbalancedBrackets,
    InfiniteLoop,
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            PointerBelowZero => write!(f, "Error: mem pointer went below zero"),
            PointerAboveLimit => write!(f, "Error: mem pointer went above limit {}", ARRAY_LEN),
            UnbalancedBrackets => write!(f, "Error: unbalanced brackets in source"),
            InfiniteLoop => write!(f, "Error: potential infinite loop in source"),
        }
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


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Token {
    Greater,      // >
    Less,         // <
    Plus,         // +
    Minus,        // -
    FullStop,     // .
    Comma,        // ,
    LeftBracket,  // [
    RightBracket, // ]
}

struct Lexer<'a> {
    input: std::str::Chars<'a>,
}

impl <'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer { input: input.chars() }
    }

    fn lex(self) -> Vec<Token> {
        self.collect()
    }
}

impl <'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.input.next() {
            Some('>') => Some(Token::Greater),
            Some('<') => Some(Token::Less),
            Some('+') => Some(Token::Plus),
            Some('-') => Some(Token::Minus),
            Some('.') => Some(Token::FullStop),
            Some(',') => Some(Token::Comma),
            Some('[') => Some(Token::LeftBracket),
            Some(']') => Some(Token::RightBracket),
            None => None,
            _ => self.next(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_empty() {
        let lexer = Lexer::new("");
        assert_eq!(lexer.lex(), Vec::new());
    }

    #[test]
    fn lex_single_tokens() {
        for (input, expected) in [
            (">", Token::Greater),
            ("<", Token::Less),
            ("+", Token::Plus),
            ("-", Token::Minus),
            (".", Token::FullStop),
            (",", Token::Comma),
            ("[", Token::LeftBracket),
            ("]", Token::RightBracket),
        ].iter() {
            let lexer = Lexer::new(input);
            assert_eq!(lexer.lex(), vec![*expected]);
        }
    }

    #[test]
    fn lex_ignores_arbitrary_input() {
        let lexer = Lexer::new("hello world");
        assert_eq!(lexer.lex(), Vec::new());
    }

    #[test]
    fn lex_valid_program() {
        let lexer = Lexer::new("[->+<]");
        use Token::*;
        assert_eq!(lexer.lex(), vec![LeftBracket, Minus, Greater, Plus, Less, RightBracket]);
    }

    #[test]
    fn lex_ignores_arbitrary_input_within_valid_program() {
        let lexer = Lexer::new(
            "[ start loop\n\
             - decrement the start cell\n\
             > increment the pointer\n\
             + increment the next cell\n\
             < decrement the pointer\n\
             ] loop until the start cell is 0"
        );
        use Token::*;
        assert_eq!(lexer.lex(), vec![LeftBracket, Minus, Greater, Plus, Less, RightBracket]);
    }
}