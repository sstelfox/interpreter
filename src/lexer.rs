use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

use crate::tokens::{Literal, SourceLocation, Token, TokenType};

pub trait Lexer: Iterator {
    fn had_error(&self) -> bool;
    fn next_token(&mut self) -> Token;
}

pub struct InputLexer<'a> {
    current_char: Option<char>,
    done: bool,
    input: Peekable<Chars<'a>>,
    offset: usize,
    had_error: bool,
}

impl<'a> InputLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars_input = input.chars();
        let init_char = chars_input.next();

        InputLexer {
            current_char: init_char,
            done: false,
            input: chars_input.peekable(),
            offset: 1,
            had_error: false,
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.current_char.as_ref()
    }

    fn peek_char2(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn read_char(&mut self) -> Option<char> {
        let cur_char = self.current_char;
        self.current_char = self.input.next();
        self.offset += 1;
        cur_char
    }

    fn read_numeric(&mut self, first_ch: char) -> f64 {
        let mut raw_txt: Vec<char> = vec![first_ch];
        let mut found_dot = false;

        loop {
            match self.peek_char() {
                Some(next_ch) => {
                    if next_ch.is_numeric() {
                        raw_txt.push(self.read_char().unwrap());
                        continue;
                    } else if next_ch == &'.' {
                        if found_dot {
                            break;
                        }

                        if let Some(next_ch2) = self.peek_char2() {
                            if next_ch2.is_numeric() {
                                found_dot = true;
                                raw_txt.push(self.read_char().unwrap());
                                continue;
                            }
                        }
                    }

                    break;
                }
                None => break,
            }
        }

        let numeric_str: String = raw_txt.iter().collect();
        numeric_str.parse::<f64>().unwrap()
    }

    fn read_text(&mut self, first_ch: char) -> String {
        let mut raw_txt: Vec<char> = vec![first_ch];

        loop {
            match self.peek_char() {
                Some(next_ch) => {
                    if next_ch.is_alphanumeric() {
                        raw_txt.push(self.read_char().unwrap());
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        raw_txt.iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }

            self.read_char();
        }
    }
}

impl Iterator for InputLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        Some(self.next_token())
    }
}

impl<'a> Lexer for InputLexer<'a> {
    fn had_error(&self) -> bool {
        self.had_error
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let mut literal: Option<Literal> = None;
        let mut source_loc = SourceLocation::new(self.offset);

        let token_type = match self.read_char() {
            Some('(') => TokenType::LeftParen,
            Some(')') => TokenType::RightParen,
            Some('{') => TokenType::LeftBrace,
            Some('}') => TokenType::RightBrace,
            Some(',') => TokenType::Comma,
            Some('.') => TokenType::Dot,
            Some('-') => TokenType::Minus,
            Some('+') => TokenType::Plus,
            Some(';') => TokenType::SemiColon,
            Some('*') => TokenType::Star,
            Some('!') => {
                if self.peek_char() == Some(&'=') {
                    self.read_char();
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            Some('=') => {
                if self.peek_char() == Some(&'=') {
                    self.read_char();
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            Some('<') => {
                if self.peek_char() == Some(&'=') {
                    self.read_char();
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            Some('>') => {
                if self.peek_char() == Some(&'=') {
                    self.read_char();
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            Some('/') => {
                if self.peek_char() == Some(&'/') {
                    // Handle comments
                    self.read_char();
                    self.skip_whitespace();

                    let mut raw_txt: Vec<char> = Vec::new();

                    loop {
                        match self.peek_char() {
                            Some(ch) => {
                                if ch == &'\n' {
                                    break;
                                }
                                raw_txt.push(self.read_char().unwrap());
                            }
                            None => break,
                        }
                    }

                    literal = Some(Literal::Text(raw_txt.iter().collect()));
                    TokenType::Comment
                } else {
                    TokenType::Slash
                }
            }
            Some('"') => {
                let mut raw_txt: Vec<char> = Vec::new();
                let mut token_type = TokenType::Text;

                loop {
                    match self.peek_char() {
                        Some('"') => {
                            self.read_char();
                            break;
                        }
                        // Handle escaped characters
                        Some('\\') => {
                            self.read_char();

                            // The "else" case will fall through to the next iteration of the
                            // loop so we don't have to handle it here...
                            if let Some(next_ch) = self.read_char() {
                                match next_ch {
                                    'n' => raw_txt.push('\n'),
                                    't' => raw_txt.push('\t'),
                                    '"' => raw_txt.push('"'),
                                    _ => {
                                        self.had_error = true;

                                        // We want the reported bad escape sequence to be
                                        // present if there is an unterminated string error
                                        raw_txt.push('\\');
                                        raw_txt.push(next_ch);

                                        // TODO: I need to embed a more appropriate error here
                                        // when there is an invalid escaped character instead
                                        // of just printing it out
                                        println!(
                                            "Invalid escape sequence found: \\{}",
                                            next_ch
                                        );

                                        token_type = TokenType::Invalid;
                                        literal = Some(Literal::Invalid(next_ch));

                                        // Note: intentionally dropping out of the loop so we
                                        // can try and consume the remainder of the string...
                                        // This does have the downside that if there is an
                                        // unmatched quote error as well, we can only return
                                        // one error... I'm leaning towards reporting the
                                        // unmatched quote one as it's more serious
                                    }
                                }
                            };
                        }
                        Some(_) => {
                            raw_txt.push(self.read_char().unwrap());
                        }
                        None => {
                            // Whelp we ran out of characters... hit an unmatched quote...
                            self.had_error = true;
                            token_type = TokenType::Invalid;

                            // Clear this in case there has been an invalid escape sequence,
                            // this one takes priority and I want the full unquoted string
                            literal = None;

                            // TODO: I need to embed a more appropriate error here when there
                            // is an unmatched quote instead of just printing it out
                            println!("Encountered unterminated string");

                            break;
                        }
                    }
                }

                if literal.is_none() {
                    literal = Some(Literal::Text(raw_txt.iter().collect()));
                }

                token_type
            }
            Some(ch) => {
                if ch.is_alphabetic() {
                    let ident = self.read_text(ch);

                    match ident.as_ref() {
                        "and" => TokenType::And,
                        "class" => TokenType::Class,
                        "else" => TokenType::Else,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "nil" => TokenType::Nil,
                        "or" => TokenType::Or,
                        "print" => TokenType::Print,
                        "return" => TokenType::Return,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        _ => {
                            literal = Some(Literal::Identifier(ident));
                            TokenType::Identifier
                        }
                    }
                } else if ch.is_numeric() {
                    literal = Some(Literal::Number(self.read_numeric(ch)));
                    TokenType::Number
                } else {
                    self.had_error = true;

                    // in order: check numeric, check alphanumerics (then check for idents),
                    // fallback on an illegal token?
                    literal = Some(Literal::Invalid(ch));
                    TokenType::Invalid
                }
            }
            None => {
                self.done = true;
                TokenType::EOF
            }
        };

        // This won't do anything unless we've extended beyond our starting
        // position
        source_loc.extend_to(self.offset - 1);

        return Token::new(token_type)
            .set_literal(literal)
            .set_location(Some(source_loc));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reserved_words() {
        let input_str =
            "and class else false for fun if nil or print return super this true var while";
        let mut lexer = InputLexer::new(&input_str);
        let mut token_list: Vec<Token> = Vec::new();

        loop {
            match lexer.next() {
                Some(tok) => {
                    token_list.push(tok);
                }
                None => {
                    break;
                }
            }
        }

        let expected = vec![
            Token::new(TokenType::And),
            Token::new(TokenType::Class),
            Token::new(TokenType::Else),
            Token::new(TokenType::False),
            Token::new(TokenType::For),
            Token::new(TokenType::Fun),
            Token::new(TokenType::If),
            Token::new(TokenType::Nil),
            Token::new(TokenType::Or),
            Token::new(TokenType::Print),
            Token::new(TokenType::Return),
            Token::new(TokenType::Super),
            Token::new(TokenType::This),
            Token::new(TokenType::True),
            Token::new(TokenType::Var),
            Token::new(TokenType::While),
            Token::new(TokenType::EOF),
        ];

        assert_eq!(token_list, expected);
        assert!(!lexer.had_error());
    }

    #[test]
    fn test_lexing_numerics() {
        let input_str = "1 23.8 890.111.floor()";
        let mut lexer = InputLexer::new(&input_str);
        let mut token_list: Vec<Token> = Vec::new();

        loop {
            match lexer.next() {
                Some(tok) => {
                    token_list.push(tok);
                }
                None => {
                    break;
                }
            }
        }

        let expected = vec![
            Token::new(TokenType::Number).set_literal(Some(Literal::Number(1.0))),
            Token::new(TokenType::Number).set_literal(Some(Literal::Number(23.8))),
            Token::new(TokenType::Number).set_literal(Some(Literal::Number(890.111))),
            Token::new(TokenType::Dot),
            Token::new(TokenType::Identifier)
                .set_literal(Some(Literal::Identifier("floor".to_string()))),
            Token::new(TokenType::LeftParen),
            Token::new(TokenType::RightParen),
            Token::new(TokenType::EOF),
        ];

        assert_eq!(token_list, expected);
        assert!(!lexer.had_error());
    }

    #[test]
    fn test_errored_input() {
        let input_str = "bad esca\\pe character";
        let mut lexer = InputLexer::new(&input_str);

        loop {
            if lexer.next().is_none() {
                break;
            }
        }
        assert!(lexer.had_error());

        let input_str = "unterminated \"quote";
        let mut lexer = InputLexer::new(&input_str);

        loop {
            if lexer.next().is_none() {
                break;
            }
        }
        assert!(lexer.had_error());
    }
}

pub struct TokenLexer {
    done: bool,
    had_error: bool,
    pos: usize,
    token_list: VecDeque<Token>,
}

impl TokenLexer {
    pub fn new(tokens: Vec<Token>) -> Self {
        TokenLexer {
            done: false,
            had_error: false,
            pos: 0,
            token_list: VecDeque::from(tokens),
        }
    }
}

impl Iterator for TokenLexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        Some(self.next_token())
    }
}

impl Lexer for TokenLexer {
    fn had_error(&self) -> bool {
        self.had_error
    }

    fn next_token(&mut self) -> Token {
        if self.done || self.token_list.is_empty() {
            self.done = true;
            return Token::new(TokenType::EOF);
        }

        let source_loc = SourceLocation::new(self.pos);
        self.pos += 1;

        // Grab the next token and label it with the location it was extracted from
        let mut token = self.token_list.pop_front().unwrap();
        token.location = Some(source_loc);

        if token.token_type == TokenType::Invalid {
            self.had_error = true;
        }

        token
    }
}
