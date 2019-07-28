use std::io::{self, BufRead, Write};
use std::str::Chars;
use std::iter::Peekable;
use std::fmt;
use std::error::Error;

#[derive(Debug, PartialEq)]
enum Token {
    Division,
    Float(f64),
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
    SyntaxError(Token),
}

impl Error for ParserError {
    fn description(&self) -> &str {
        use self::ParserError::*;

        match *self {
            Impossible => "encountered a parser error path that should be impossible",
            SyntaxError(_) => "encountered unexpected token while parsing",
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParserError::*;

        match *self {
            SyntaxError(ref token) => write!(f, "Encountered an unexpected token {:?}", token),
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
        let mut is_float = false;

        loop {
            match self.peek_char() {
                Some(ch) => {
                    if ch.is_numeric() {
                        numeric_chars.push(self.read_char().unwrap())
                    } else if *ch == '.' {
                        is_float = true;
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

        if is_float {
            match numeric_str.parse::<f64>() {
                Ok(num) => Ok(Token::Float(num)),
                Err(_) => Err(ParserError::Impossible),
            }
        } else {
            match numeric_str.parse::<i64>() {
                Ok(num) => Ok(Token::Integer(num)),
                Err(_) => Err(ParserError::Impossible),
            }
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
        let left_token = self.lexer.next_token();
        match left_token {
            Token::Integer(_) => Ok(left_token),
            Token::Float(_) => Ok(left_token),
            Token::Minus => {
                let right_token = self.term()?;
                match right_token {
                    Token::Integer(val) => Ok(Token::Integer(val * -1)),
                    Token::Float(val) => Ok(Token::Float(val * -1.0)),
                    _ => Err(ParserError::SyntaxError(right_token)),
                }
            },
            Token::EOF => Ok(left_token),
            _ => Err(ParserError::SyntaxError(left_token)),
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
                Token::Division => {
                    let right_token = self.term()?;
                    left_token = match (left_token, right_token) {
                        (Token::Float(left), Token::Float(right)) => Token::Float(left / right),
                        (Token::Float(left), Token::Integer(right)) => Token::Float(left / right as f64),
                        (Token::Integer(left), Token::Float(right)) => Token::Float(left as f64 / right),
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left / right),
                        (_, right) => { return Err(ParserError::SyntaxError(right)); },
                    };
                },
                Token::Minus => {
                    let right_token = self.term()?;
                    left_token = match (left_token, right_token) {
                        (Token::Float(left), Token::Float(right)) => Token::Float(left - right),
                        (Token::Float(left), Token::Integer(right)) => Token::Float(left - right as f64),
                        (Token::Integer(left), Token::Float(right)) => Token::Float(left as f64 - right),
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left - right),
                        (_, right) => { return Err(ParserError::SyntaxError(right)); },
                    };
                },
                Token::Multiplication => {
                    let right_token = self.term()?;
                    left_token = match (left_token, right_token) {
                        (Token::Float(left), Token::Float(right)) => Token::Float(left * right),
                        (Token::Float(left), Token::Integer(right)) => Token::Float(left * right as f64),
                        (Token::Integer(left), Token::Float(right)) => Token::Float(left as f64 * right),
                        (Token::Integer(left), Token::Integer(right)) => Token::Integer(left * right),
                        (_, right) => { return Err(ParserError::SyntaxError(right)); },
                    };
                },
                Token::Plus => {
                    let right_token = self.term()?;
                    left_token = match (left_token, right_token) {
                        (Token::Float(left), Token::Float(right)) => Token::Float(left + right),
                        (Token::Float(left), Token::Integer(right)) => Token::Float(left + right as f64),
                        (Token::Integer(left), Token::Float(right)) => Token::Float(left as f64 + right),
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

    // Clear the prompt on exit
    println!("");
}
