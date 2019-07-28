use std::io::{self, BufRead, Write};
use std::str::Chars;
use std::iter::Peekable;
use std::fmt;
use std::error::Error;

#[derive(Debug, PartialEq)]
enum Token {
    Division,
    Integer(i64),
    Illegal,
    Minus,
    Multiplication,
    Plus,
    EOF,
}

#[derive(Debug, PartialEq)]
enum ParserError {
    Impossible,
    SyntaxError,
}

impl Error for ParserError {
    fn description(&self) -> &str {
        use self::ParserError::*;

        match *self {
            Impossible => "encountered a parser error path that should be impossible",
            SyntaxError => "encountered unexpected token while parsing",
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            _ => write!(f, "{}", self.description()),
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
            Some('/') => Token::Division,
            Some('*') => Token::Multiplication,
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some(ref ch) => {
                if ch.is_numeric() {
                    self.read_numeric(*ch).expect("unable to read numeric valud")
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

    fn read_numeric(&mut self, first_ch: char) -> Result<Token, ParserError> {
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
            Ok(num) => Ok(Token::Integer(num)),
            Err(_) => Err(ParserError::Impossible),
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
    current_token: Token,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn advance(&mut self) {
        //println!("Consumed: {:?}", self.current_token);
        self.current_token = self.lexer.next_token();
    }

    fn factor(&mut self) -> Result<Token, ParserError> {
        match self.current_token {
            Token::EOF => Ok(Token::EOF),
            Token::Integer(num) => {
                self.advance();
                Ok(Token::Integer(num))
            },
            Token::Minus => {
                self.advance();
                let right_factor = self.factor()?;

                match right_factor {
                    Token::Integer(val) => Ok(Token::Integer(val * -1)),
                    _ => Err(ParserError::SyntaxError),
                }
            },
            _ => Err(ParserError::SyntaxError),
        }
    }

    fn term(&mut self) -> Result<Token, ParserError> {
        let mut term_result = self.factor()?;

        loop {
            match self.current_token {
                Token::Division => {
                    self.advance();
                    let right_factor = self.factor()?;

                    term_result = match (&term_result, &right_factor) {
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left / right),
                        (_, _) => { return Err(ParserError::SyntaxError); },
                    };
                },
                Token::Multiplication => {
                    self.advance();
                    let right_factor= self.factor()?;

                    term_result = match (&term_result, &right_factor) {
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left * right),
                        (_, _) => { return Err(ParserError::SyntaxError); },
                    };
                },
                _ => break,
            }
        }

        Ok(term_result)
    }

    fn expr(&mut self) -> Result<Token, ParserError> {
        let mut expr_result = self.term()?;

        loop {
            match self.current_token {
                Token::Minus => {
                    self.advance();
                    let right_term = self.term()?;

                    expr_result = match (&expr_result, &right_term) {
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left - right),
                        (_, _) => { return Err(ParserError::SyntaxError); },
                    };
                },
                Token::Plus => {
                    self.advance();
                    let right_term = self.term()?;

                    expr_result = match (&expr_result, &right_term) {
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left + right),
                        (_, _) => { return Err(ParserError::SyntaxError); },
                    };
                },
                _ => break,
            }
        }

        Ok(expr_result)
    }

    fn new(mut lexer: Lexer<'a>) -> Self {
        let initial_token = lexer.next_token();

        Parser {
            current_token: initial_token,
            lexer: lexer,
        }
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

    // Clear the prompt on exit
    println!("");
}
