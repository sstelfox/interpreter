use std::io::{self, BufRead, Write};
use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
enum Token {
    Integer(i64),
    Illegal,
    Minus,
    Plus,
    EOF,
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
        // Sanity check
        if !first_ch.is_numeric() && first_ch != '-' {
            println!("First character was: {}", first_ch);
            return Err("first character to read_numeric wasn't valid");
        }

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
    eof: bool,
}

impl<'a> Parser<'a> {
    fn evaluate(&mut self) -> Result<i64, &str> {
        let left = match self.lexer.next_token() {
            Token::Integer(num) => num,
            Token::EOF => {
                self.eof = true;
                return Err("reached EOF");
            }
            _ => {
                return Err("left token must be an Integer");
            },
        };

        let operation = match self.lexer.next_token() {
            Token::Minus => Token::Minus,
            Token::Plus => Token::Plus,
            Token::EOF => {
                self.eof = true;
                return Err("reached EOF");
            },
            _ => {
                return Err("left token must be an Integer");
            },
        };

        let right = match self.lexer.next_token() {
            Token::Integer(num) => num,
            Token::EOF => {
                self.eof = true;
                return Err("reached EOF");
            },
            _ => {
                return Err("left token must be an Integer");
            },
        };

        match operation {
            Token::Minus => Ok(left - right),
            Token::Plus => Ok(left + right),
            _ => Err("impossibru"),
        }
    }

    fn new(lexer: Lexer<'a>) -> Self {
        Parser { eof: false, lexer: lexer }
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

        match parser.evaluate() {
            Ok(result) => {
                println!("{}", result);
            },
            Err(err) => {
                println!("Error: {}", err);
            },
        }

        if parser.eof {
            break;
        }
    }
}
