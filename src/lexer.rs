use std::str::Chars;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token {
    Greater,      // >
    Less,         // <
    Plus,         // +
    Minus,        // -
    FullStop,     // .
    Comma,        // ,
    LeftBracket,  // [
    RightBracket, // ]
}

pub struct Lexer<'a> {
    input: Chars<'a>,
}

impl <'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input: input.chars() }
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
        assert_eq!(lexer.collect::<Vec<Token>>(), Vec::new());
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
            assert_eq!(lexer.collect::<Vec<Token>>(), vec![*expected]);
        }
    }

    #[test]
    fn lex_ignores_arbitrary_input() {
        let lexer = Lexer::new("hello world");
        assert_eq!(lexer.collect::<Vec<Token>>(), Vec::new());
    }

    #[test]
    fn lex_valid_program() {
        let lexer = Lexer::new("[->+<]");
        use Token::*;
        assert_eq!(lexer.collect::<Vec<Token>>(), vec![LeftBracket, Minus, Greater, Plus, Less, RightBracket]);
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
        assert_eq!(lexer.collect::<Vec<Token>>(), vec![LeftBracket, Minus, Greater, Plus, Less, RightBracket]);
    }
}
