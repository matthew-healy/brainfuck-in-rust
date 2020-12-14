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

#[derive(Debug)]
struct Program {
    nodes: Vec<SyntaxNode>,
}

impl Program {
    fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    fn add_node(&mut self, node: SyntaxNode) {
        self.nodes.push(node);
    }

    fn nodes(&self) -> &[SyntaxNode] {
        &self.nodes
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Block {
    nodes: Vec<SyntaxNode>
}

impl Block {
    fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    fn push(&mut self, node: SyntaxNode) {
        self.nodes.push(node);
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum SyntaxNode {
    IncrementPointer { times: usize },
    DecrementPointer { times: usize },
    IncrementByte { times: usize },
    DecrementByte { times: usize },
    WriteByte {times: usize },
    ReadByte { times: usize },
    Loop(Block),
}

struct Parser<Lex: Iterator<Item=Token>> {
    input: std::iter::Peekable<Lex>
}

impl <Lex: Iterator<Item=Token>> Parser<Lex> {
    fn new(input: Lex) -> Self {
        Self { input: input.peekable() }
    }

    fn parse(mut self) -> Result<Program, Error> {
        let mut program = Program::new();
        while let Some(token) = self.input.next() {
            use Token::*;
            let node = match token {
                LeftBracket => Ok(self.consume_loop()?),
                RightBracket => Err(Error::UnbalancedBrackets),
                t => Ok(self.consume_many(t)),
            };
            program.add_node(node?);
        }
        Ok(program)
    }

    fn consume_many(&mut self, token: Token) -> SyntaxNode {
        let mut times = 1;
        while let Some(next_token) = self.input.peek() {
            if token == *next_token {
                self.input.next();
                times += 1;
            } else { break }
        }
        Self::node_from_non_loop_token(token, times)
    }

    fn node_from_non_loop_token(token: Token, times: usize) -> SyntaxNode {
        use Token::*;
        use SyntaxNode::*;
        match token {
            Greater => IncrementPointer { times },
            Less => DecrementPointer{ times },
            Plus => IncrementByte { times },
            Minus => DecrementByte { times },
            FullStop => WriteByte { times },
            Comma => ReadByte { times },
            _ => panic!("node_from_non_loop_token called with loop token")
        }
    }

    fn consume_loop(&mut self) -> Result<SyntaxNode, Error> {
        let mut block = Block::new();
        while let Some(token) = self.input.next() {
            if token == Token::RightBracket {
                return if block.is_empty() {
                    Err(Error::InfiniteLoop)
                } else {
                    Ok(SyntaxNode::Loop(block))
                }
            }
            let node = match token {
                Token::LeftBracket => self.consume_loop()?,
                Token::RightBracket => unreachable!(),
                t => self.consume_many(t),
            };
            block.push(node);
        }
        Err(Error::UnbalancedBrackets)
    }
}

#[cfg(test)]
mod lexer_tests {
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

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn parse_empty() {
        let parser = Parser::new(vec![].into_iter());
        let program = parser.parse().unwrap();
        assert!(program.nodes().is_empty());
    }

    #[test]
    fn parse_single_tokens() {
        use Token::*;
        use SyntaxNode::*;
        for (input, expected) in &[
            (Greater, IncrementPointer { times: 1 }),
            (Less, DecrementPointer { times: 1 }),
            (Plus, IncrementByte { times: 1 }),
            (Minus, DecrementByte { times: 1 }),
            (FullStop, WriteByte { times: 1 }),
            (Comma, ReadByte { times: 1 }),
        ] {
            let parser = Parser::new(vec![*input].into_iter());
            let program = parser.parse().unwrap();
            assert_eq!(program.nodes(), &[expected.clone()])
        }
    }

    #[test]
    fn parse_same_token_multiple_times() {
        use Token::*;
        use SyntaxNode::*;
        for (input, expected) in &[
            (vec![Greater, Greater], IncrementPointer { times: 2 }),
            (vec![Less, Less, Less], DecrementPointer { times: 3 }),
            (vec![Plus, Plus, Plus, Plus] , IncrementByte { times: 4 }),
        ] {
            let parser = Parser::new(input.into_iter().map(|t| *t));
            let program = parser.parse().unwrap();
            assert_eq!(program.nodes(), &[expected.clone()])
        }
    }

    #[test]
    fn parse_sequence_of_tokens() {
        use Token::*;
        use SyntaxNode::*;
        let input = vec![Plus, Plus, Plus, Greater, Less, Minus, Minus];
        let expected = &[
            IncrementByte { times: 3},
            IncrementPointer { times: 1 },
            DecrementPointer { times: 1 },
            DecrementByte { times: 2 }
        ];
        let parser = Parser::new(input.into_iter());
        let program = parser.parse().unwrap();
        assert_eq!(program.nodes(), expected);
    }

    #[test]
    fn empty_loop_is_error() {
        use Token::*;
        use SyntaxNode::*;
        let input = vec![LeftBracket, RightBracket];
        let parser = Parser::new(input.into_iter());
        let error = parser.parse().unwrap_err();
        assert_eq!(error, Error::InfiniteLoop);
    }

    #[test]
    fn unmatched_loop_is_error() {
        for input in &[Token::LeftBracket, Token::RightBracket] {
            let parser = Parser::new(vec![*input].into_iter());
            let error = parser.parse().unwrap_err();
            assert_eq!(error, Error::UnbalancedBrackets);
        }
    }

    #[test]
    fn parse_simple_loop() {
        use Token::*;
        use SyntaxNode::*;
        let input = vec![LeftBracket, Minus, Greater, Plus, Less, RightBracket];
        let expected = Loop(Block { nodes: vec![
            DecrementByte { times: 1 },
            IncrementPointer { times: 1 },
            IncrementByte { times : 1 },
            DecrementPointer { times: 1 },
        ]});
        let parser = Parser::new(input.into_iter());
        let program = parser.parse().unwrap();
        assert_eq!(program.nodes(), &[expected])
    }

    #[test]
    fn parse_nested_loops() {
        use Token::*;
        use SyntaxNode:: *;
        let input = vec![LeftBracket, Minus, LeftBracket, Minus, RightBracket, RightBracket];
        let expected = Loop(Block { nodes: vec! [
            DecrementByte { times: 1 },
            Loop(Block { nodes: vec![DecrementByte { times: 1}]}),
        ]});
        let parser = Parser::new(input.into_iter());
        let program = parser.parse().unwrap();
        assert_eq!(program.nodes(), &[expected]);
    }
}