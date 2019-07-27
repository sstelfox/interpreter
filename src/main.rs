use std::io::{self, BufRead, Write};
use std::str::Chars;
use std::iter::Peekable;
use std::fmt;
use std::error::Error;

#[derive(Debug, PartialEq)]
enum Token {
    Integer(i64),
    Illegal,
    Minus,
    Plus,
    EOF,
}

#[derive(Debug, PartialEq)]
enum ParserError {
    SyntaxError(Token),
}

impl Error for ParserError {
    fn description(&self) -> &str {
        use self::ParserError::*;

        match *self {
            SyntaxError(_) => "encountered unexpected token while parsing",
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParserError::*;

        match *self {
            SyntaxError(ref token) => write!(f, "Encountered an unexpected token {:?}", token),
        }
    }
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer { input: input.chars().peekable() }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some('+') => Token::Plus,
            Some('-') => {
                if let Some(ch) = self.peek_char() {
                    if ch.is_numeric() {
                        Token::Integer(self.read_numeric('-').expect("unable to read numeric valud"))
                    } else {
                        Token::Minus
                    }
                } else {
                    Token::Minus
                }
            },
            Some(ref ch) => {
                if ch.is_numeric() {
                    Token::Integer(self.read_numeric(*ch).expect("unable to read numeric valud"))
                } else {
                    Token::Illegal
                }
            },
            None => Token::EOF,
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn read_numeric(&mut self, first_ch: char) -> Result<i64, &str> {
        let mut numeric_chars = vec![first_ch];
        loop {
            match self.peek_char() {
                Some(ch) => {
                    if ch.is_numeric() {
                        numeric_chars.push(self.read_char().unwrap())
                    } else {
                        break;
                    }
                },
                None => {
                    break;
                },
            }
        }

        let numeric_str: String = numeric_chars.into_iter().collect();
        match numeric_str.parse::<i64>() {
            Ok(num) => Ok(num),
            Err(_) => Err("unable to parse numeric value"),
        }
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

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn term(&mut self) -> Result<Token, ParserError> {
        let current_token = self.lexer.next_token();
        match current_token {
            Token::Integer(_) => Ok(current_token),
            Token::EOF => Ok(current_token),
            _ => Err(ParserError::SyntaxError(current_token)),
        }
    }

    fn expr(&mut self) -> Result<Token, ParserError> {
        let mut left_token = self.term()?;
        if left_token == Token::EOF {
            return Ok(Token::EOF);
        }

        loop {
            // Note: I will loose this token, if there are other possibilities this state need to
            // be preserved (make current_token a persistent struct value thing?)
            let operation = self.lexer.next_token();
            match operation {
                Token::Minus => {
                    let right_token = self.term()?;
                    left_token = match (left_token, right_token) {
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left - right),
                        (_, right) => { return Err(ParserError::SyntaxError(right)); },
                    };
                },
                Token::Plus => {
                    let right_token = self.term()?;
                    left_token = match (left_token, right_token) {
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left + right),
                        (_, right) => { return Err(ParserError::SyntaxError(right)); },
                    };
                },
                Token::EOF => {
                    break;
                },
                _ => {
                    return Err(ParserError::SyntaxError(operation));
                },
            }
        }

        Ok(left_token)
    }

    fn new(lexer: Lexer<'a>) -> Self {
        Parser { lexer: lexer }
    }
}

fn main() {
    let stdin = io::stdin();

    loop {
        print!(">> ");
        io::stdout().flush().expect("error flushing stdout");

        let mut line = String::new();
        stdin.lock().read_line(&mut line).expect("unable to read from stdin");

        let lexer = Lexer::new(&mut line);
        let mut parser = Parser::new(lexer);

        match parser.expr() {
            Ok(Token::EOF) => break,
            Ok(token) => {
                println!("{:?}", token);
            },
            Err(err) => {
                println!("Error: {}", err);
            },
        }
    }
}
