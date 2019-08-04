mod errors {
    use std::error::Error;
    use std::fmt::{self, Display};

    use crate::tokens::Token;

    #[derive(Debug, PartialEq)]
    pub enum RIError {
        // This should be a LexicalError and I should ditch the Illegal token
        InvalidCharacter(Token),
    }

    impl Display for RIError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use self::RIError::*;

            match self {
                InvalidCharacter(tok) => write!(f, "encountered an invalid character: {:?}", tok),
            }
        }
    }

    impl Error for RIError {
        fn description(&self) -> &str {
            use self::RIError::*;

            match *self {
                InvalidCharacter(_) => {
                    "encountered an invalid character while tokenizing the input"
                }
            }
        }
    }
}

mod tokens {
    #[derive(Debug, PartialEq)]
    pub enum Literal {
        Identifier(String),
        Invalid(char),
        Number(f64),
        Text(String),
    }

    #[derive(Debug, PartialEq)]
    pub struct Token {
        pub token_type: TokenType,
        pub literal: Option<Literal>,
        pub location: Option<SourceLocation>,
    }

    impl Token {
        pub fn new(tok_t: TokenType) -> Self {
            Token {
                token_type: tok_t,
                literal: None,
                location: None,
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum TokenType {
        // Single character tokens
        Comma,
        Dot,
        LeftBrace,
        LeftParen,
        Minus,
        Plus,
        RightBrace,
        RightParen,
        SemiColon,
        Slash,
        Star,

        // One or two character tokens
        Bang,
        BangEqual,
        Equal,
        EqualEqual,
        Greater,
        GreaterEqual,
        Less,
        LessEqual,

        // Literals (Note: Text is named String in the reference grammar)
        Identifier,
        Number,
        Text,

        // Keywords
        And,
        Class,
        Else,
        False,
        For,
        Fun,
        If,
        Nil,
        Or,
        Print,
        Return,
        Super,
        This,
        True,
        Var,
        While,

        EOF,
        Invalid,
    }

    /// This data structure wraps lex'd tokens with information useful for diagnosing the source of
    /// errors in input programs.
    #[derive(Debug, PartialEq)]
    pub enum SourceLocation {
        Point(usize),
        Range(usize, usize),
    }

    impl SourceLocation {
        // SourceLocation's always start as a single character location, but can be extended to
        // cover a range.
        pub fn new(loc: usize) -> Self {
            SourceLocation::Point(loc)
        }

        pub fn extend_to(&mut self, end_pos: usize) {
            use self::SourceLocation::*;

            let start_pos = match *self {
                Point(start) => start,
                Range(start, _) => start,
            };

            // Should I do some kind of validation here? Check that end > start? Nah not for now...
            if (start_pos == end_pos) {
                *self = Point(start_pos);
            } else {
                *self = Range(start_pos, end_pos);
            }
        }
    }
}

mod lexer {
    use std::collections::VecDeque;
    use std::iter::Peekable;
    use std::str::Chars;

    use crate::tokens::{Literal, SourceLocation, Token, TokenType};

    pub trait Lexer {
        fn next_token(&mut self) -> Token;
    }

    pub struct TokenLexer {
        pos: usize,
        token_list: VecDeque<Token>,
    }

    impl TokenLexer {
        pub fn new(tokens: Vec<Token>) -> Self {
            TokenLexer {
                pos: 0,
                token_list: VecDeque::from(tokens),
            }
        }
    }

    impl Lexer for TokenLexer {
        fn next_token(&mut self) -> Token {
            if self.token_list.is_empty() {
                return Token::new(TokenType::EOF);
            }

            let source_loc = SourceLocation::new(self.pos);
            self.pos += 1;

            // Grab the next token and label it with the location it was extracted from
            let mut token = self.token_list.pop_front().unwrap();
            token.location = Some(source_loc);

            token
        }
    }

    pub struct InputLexer<'a> {
        input: Peekable<Chars<'a>>,
        offset: usize,
    }

    impl<'a> InputLexer<'a> {
        pub fn new(input: &'a str) -> Self {
            InputLexer {
                input: input.chars().peekable(),
                offset: 0,
            }
        }

        fn peek_char(&mut self) -> Option<&char> {
            self.input.peek()
        }

        fn read_char(&mut self) -> Option<char> {
            self.offset += 1;
            self.input.next()
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

    impl<'a> Lexer for InputLexer<'a> {
        fn next_token(&mut self) -> Token {
            self.skip_whitespace();

            let mut source_loc = SourceLocation::new(self.offset);
            let token_type = match self.read_char() {
                Some(',') => TokenType::Comma,
                Some('.') => TokenType::Dot,
                Some('[') => TokenType::LeftBrace,
                Some('(') => TokenType::LeftParen,
                Some('-') => TokenType::Minus,
                Some('+') => TokenType::Plus,
                Some(']') => TokenType::RightBrace,
                Some(')') => TokenType::RightParen,
                Some(';') => TokenType::SemiColon,
                // If I add comments I'll need to extend this to peek ahead a little bit
                Some('/') => TokenType::Slash,
                Some('*') => TokenType::Star,
                Some('!') => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    }
                },
                Some('=') => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    }
                },
                Some('>') => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    }
                },
                Some('<') => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    }
                },
                Some('"') => {
                    let mut raw_txt: Vec<char> = Vec::new();

                    loop {
                        match self.peek_char() {
                            Some('"') => {
                                self.read_char();
                                break;
                            },
                            // Handle escaped characters
                            Some('\\') => {
                                self.read_char();

                                // The "else" case will fall through to the next iteration of the
                                // loop so we don't have to handle it here...
                                if let Some(next_ch) = self.read_char() {
                                    raw_txt.push(next_ch);
                                };
                            },
                            Some(ch) => {
                                raw_txt.push(self.read_char().unwrap());
                            },
                            None => {
                                // Whelp we ran out of characters... hit an unmatched quote...
                                println!("SCREAMING INTERNALLY");
                                break;
                            },
                        }
                    }

                    let mut token = Token::new(TokenType::Text);
                    source_loc.extend_to(self.offset - 1);
                    token.location = Some(source_loc);
                    token.literal = Some(Literal::Text(raw_txt.iter().collect()));

                    return token;
                },
                Some(ch) => {
                    if ch.is_alphabetic() {
                        // Need to parse out an identifier
                        TokenType::Text
                    } else if ch.is_numeric() {
                        // Need to parse out a number
                        TokenType::Number
                    } else {
                        // in order: check numeric, check alphanumerics (then check for idents),
                        // fallback on an illegal token?
                        let mut inv_token = Token::new(TokenType::Invalid);
                        inv_token.location = Some(source_loc);
                        inv_token.literal = Some(Literal::Invalid(ch));

                        return inv_token;
                    }
                }
                None => TokenType::EOF,
            };

            let mut token = Token::new(token_type);
            token.location = Some(source_loc);
            return token;
        }
    }
}

mod parser {
}

mod interpreter {
    use std::fs::File;
    use std::io::{self, BufRead, Read, Write};

    use crate::lexer::{InputLexer, Lexer};
    use crate::tokens::TokenType;

    pub fn run_file(path: String) {
        // TODO: error handling and the like this is fast and dirty
        let mut file = File::open(path).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let mut lexer = InputLexer::new(&contents);
        let mut current_token = lexer.next_token();

        loop {
            println!("{:?}", current_token);
            current_token = lexer.next_token();

            if current_token.token_type == TokenType::EOF {
                break;
            }
        }
    }

    pub fn start_repl() {
        let stdin = io::stdin();

        loop {
            print!(">> ");
            io::stdout().flush().expect("error flushing stdout");

            let mut line = String::new();
            stdin
                .lock()
                .read_line(&mut line)
                .expect("unable to read from input");

            let mut lexer = InputLexer::new(&line);
            let mut current_token = lexer.next_token();

            if current_token.token_type == TokenType::EOF {
                println!("");
                break;
            }

            loop {
                println!("{:?}", current_token);
                current_token = lexer.next_token();

                if current_token.token_type == TokenType::EOF {
                    break;
                }
            }
        }
    }
}

const VERSION: &str = "0.1.1";

fn print_usage() {
    let prog_name = std::env::args().nth(0).unwrap();

    println!(
        "{} version {}, Copyright (C) 2019 Sam Stelfox",
        prog_name, VERSION
    );
    println!("This program is free software with ABSOLUTELY NO WARRANTY; You may");
    println!("redistribute it under the terms of the GNU Affero General Public");
    println!("License v3.0.\n");
    println!("Usage: {} [OPTIONS] [FILE]", prog_name);
    println!("  -h\tPrint this help text\n");
    println!("Run with no arguments to start a REPL or alternatively supply a");
    println!("file to be interpreted as the first argument.\n");
    println!("Report bugs at https://github.com/sstelfox/interpreter");
}

fn main() {
    let args = std::env::args();

    if args.len() == 1 {
        interpreter::start_repl();
    } else if args.len() == 2 {
        let arg = std::env::args().nth(1).unwrap();

        if "-h".to_string() == arg {
            print_usage();
            std::process::exit(0);
        } else {
            interpreter::run_file(arg);
        }
    } else {
        print_usage();
        std::process::exit(22);
    }
}
