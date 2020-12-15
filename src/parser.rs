use crate::{
    ast::*,
    error::*,
    lexer::Token,
};

pub struct Parser<Lex: Iterator<Item=Token>> {
    input: std::iter::Peekable<Lex>
}

impl <Lex: Iterator<Item=Token>> Parser<Lex> {
    pub fn new(input: Lex) -> Self {
        Self { input: input.peekable() }
    }

    pub fn parse(mut self) -> Result<Program, Error> {
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
            block.add_node(node);
        }
        Err(Error::UnbalancedBrackets)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Block {
        fn from_vec(nodes: Vec<SyntaxNode>) -> Block {
            let mut block = Block::new();
            for node in nodes.into_iter() {
                block.add_node(node);
            }
            block
        }
    }

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
        let expected = Loop(Block::from_vec(vec![
            DecrementByte { times: 1 },
            IncrementPointer { times: 1 },
            IncrementByte { times : 1 },
            DecrementPointer { times: 1 },
        ]));
        let parser = Parser::new(input.into_iter());
        let program = parser.parse().unwrap();
        assert_eq!(program.nodes(), &[expected])
    }

    #[test]
    fn parse_nested_loops() {
        use Token::*;
        use SyntaxNode:: *;
        let input = vec![LeftBracket, Minus, LeftBracket, Minus, RightBracket, RightBracket];
        let expected = Loop(Block::from_vec(vec! [
            DecrementByte { times: 1 },
            Loop(Block::from_vec(vec![DecrementByte { times: 1}])),
        ]));
        let parser = Parser::new(input.into_iter());
        let program = parser.parse().unwrap();
        assert_eq!(program.nodes(), &[expected]);
    }
}