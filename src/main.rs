mod errors {
    use std::error::Error;
    use std::fmt::{self, Display};

    use crate::tokens::TokenSpan;

    #[derive(Debug, PartialEq)]
    pub enum RIError {
        InvalidCharacter(TokenSpan),
    }

    impl Display for RIError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use self::RIError::*;

            match *self {
                InvalidCharacter(tok_sp) => {
                    write!(f, "encountered an invalid character {}", tok_sp.location())
                },
            }
        }
    }

    impl Error for RIError {
        fn description(&self) -> &str {
            use self::RIError::*;

            match *self {
                InvalidCharacter(_) => "encountered an invalid character while tokenizing the input",
            }
        }
    }
}

mod tokens {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Token {
        Integer(i64),

        LeftParen,
        RightParen,

        Division,
        Minus,
        Multiplication,
        Plus,

        EOF,
        Illegal,
    }

    /// This data structure wraps lex'd tokens with information useful for diagnosing the source of
    /// errors in input programs.
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum TokenSpan {
        /// Used by the TokenLexer for tests to indicate which of the preprocessed tokens were
        /// invalid. The token number is zero indexed to match its position in the input array.
        CountSpan(Token, usize),

        /// Used for single character tokens. The first numeric is the line the token was found on,
        /// the second is the column.
        SimpleSpan(Token, usize, usize),

        /// When a token is complex enough that it spans multiple characters this span is used to
        /// capture the full range of the input consumed. In order the numeric arguments are: line
        /// number, column start, column end.
        RangeSpan(Token, usize, usize, usize),
    }

    impl TokenSpan {
        pub fn location(&self) -> String {
            use self::TokenSpan::*;

            match *self {
                CountSpan(_, tok_id) => format!("at token number {}", tok_id),
                SimpleSpan(_, line, column) => format!("at line number {} column {}", line, column),
                RangeSpan(_, line, s_col, e_col) => format!("at line number {} between columns {} and {}", line, s_col, e_col),
            }
        }

        pub fn token(&self) -> Token {
            use self::TokenSpan::*;

            match *self {
                CountSpan(tok, _) => tok,
                SimpleSpan(tok, _, _) => tok,
                RangeSpan(tok, _, _, _) => tok,
            }
        }
    }
}

mod lexer {
    use std::iter::Peekable;
    use std::str::Chars;

    use crate::tokens::{Token, TokenSpan};

    pub trait Lexer {
        fn next_token(&mut self) -> TokenSpan;
    }

    pub struct TokenLexer {
        pos: usize,
        token_list: Vec<Token>,
    }

    impl TokenLexer {
        pub fn new(tokens: Vec<Token>) -> Self {
            TokenLexer { pos: 0, token_list: tokens }
        }
    }

    impl Lexer for TokenLexer {
        fn next_token(&mut self) -> TokenSpan {
            if self.pos >= self.token_list.len() {
                return TokenSpan::CountSpan(Token::EOF, self.pos);
            }

            self.pos += 1;
            TokenSpan::CountSpan(self.token_list[self.pos - 1], self.pos - 1)
        }
    }

    pub struct InputLexer<'a> {
        column: usize,
        line: usize,
        input: Peekable<Chars<'a>>,
    }

    impl<'a> InputLexer<'a> {
        pub fn new(input: &'a str) -> Self {
            InputLexer { column: 1, line: 1, input: input.chars().peekable() }
        }

        fn peek_char(&mut self) -> Option<&char> {
            self.input.peek()
        }

        fn read_char(&mut self) -> Option<char> {
            self.column += 1;
            self.input.next()
        }

        fn read_numeric(&mut self, first_ch: char) -> TokenSpan {
            if !first_ch.is_numeric() {
                // This should never happen and indicates a bug in the caller
                panic!("a non-numeric character '{}' was provided to read_numeric (presumably at line {}, column {})", first_ch, self.line, self.column - 1);
            }

            let start_column = self.column - 1;
            let mut numeric_chars = vec![first_ch];
            loop {
                match self.peek_char() {
                    Some(ch) => {
                        if !ch.is_numeric() { break; }
                        numeric_chars.push(self.read_char().unwrap());
                    },
                    None => break,
                }
            }

            let end_column = self.column - 1;
            let num = numeric_chars.into_iter().collect::<String>().parse::<i64>().unwrap();

            if start_column == end_column {
                TokenSpan::SimpleSpan(Token::Integer(num), self.line, start_column)
            } else {
                TokenSpan::RangeSpan(Token::Integer(num), self.line, start_column, end_column)
            }
        }

        fn skip_whitespace(&mut self) {
            while let Some(&c) = self.peek_char() {
                if !c.is_whitespace() { break; }

                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                }

                self.read_char();
            }
        }
    }

    impl<'a> Lexer for InputLexer<'a> {
        fn next_token(&mut self) -> TokenSpan {
            self.skip_whitespace();

            let current_line = self.line;
            let current_column = self.column;

            let token = match self.read_char() {
                Some('-') => Token::Minus,
                Some('+') => Token::Plus,
                Some('/') => Token::Division,
                Some('*') => Token::Multiplication,
                Some('(') => Token::LeftParen,
                Some(')') => Token::RightParen,
                Some(ref ch) => {
                    if ch.is_numeric() {
                        return self.read_numeric(*ch);
                    } else {
                        Token::Illegal
                    }
                },
                None => Token::EOF,
            };

            TokenSpan::SimpleSpan(token, current_line, current_column)
        }
    }

    #[test]
    fn test_token_lexer() {
        let mut lexer = TokenLexer::new(vec![
            Token::Integer(45), Token::Plus
        ]);

        assert_eq!(lexer.next_token(), TokenSpan::CountSpan(Token::Integer(45), 0));
        assert_eq!(lexer.next_token(), TokenSpan::CountSpan(Token::Plus, 1));
        assert_eq!(lexer.next_token(), TokenSpan::CountSpan(Token::EOF, 2));
    }

    #[test]
    fn test_input_lexer() {
        let test_input = "   10 + (4  * 2 - 300) / 8\n3 + 8";
        let mut lexer = InputLexer::new(test_input);

        assert_eq!(lexer.next_token(), TokenSpan::RangeSpan(Token::Integer(10), 1, 4, 5));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Plus, 1, 7));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::LeftParen, 1, 9));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Integer(4), 1, 10));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Multiplication, 1, 13));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Integer(2), 1, 15));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Minus, 1, 17));
        assert_eq!(lexer.next_token(), TokenSpan::RangeSpan(Token::Integer(300), 1, 19,21));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::RightParen, 1, 22));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Division, 1, 24));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Integer(8), 1, 26));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Integer(3), 2, 1));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Plus, 2, 3));
        assert_eq!(lexer.next_token(), TokenSpan::SimpleSpan(Token::Integer(8), 2, 5));
    }
}

mod parser {
    use crate::errors::RIError;
    use crate::lexer::Lexer;
    use crate::tokens::{Token, TokenSpan};

    pub struct Parser {
        current_token: TokenSpan,
        lexer: Box<dyn Lexer>,
    }

    impl Parser {
        fn advance(&mut self) {
            self.current_token = self.lexer.next_token();
        }

        pub fn construct_ast(&mut self) -> Result<TokenSpan, RIError> {
            match self.current_token.token() {
                Token::EOF => Ok(self.current_token),
                Token::Illegal => Err(RIError::InvalidCharacter(self.current_token)),
                _ => {
                    self.advance();
                    Ok(self.current_token)
                },
            }
        }

        pub fn new(mut lexer: Box<dyn Lexer>) -> Self {
            let initial_token = lexer.next_token();

            Parser {
                current_token: initial_token,
                lexer: lexer,
            }
        }
    }

    #[test]
    fn test_parser_construction() {
        let lexer = super::lexer::TokenLexer::new(vec![Token::EOF]);
        let mut parser = Parser::new(Box::new(lexer));

        assert_eq!(parser.current_token.token(), Token::EOF);
        assert_eq!(parser.construct_ast().unwrap().token(), Token::EOF);
    }

    #[test]
    fn test_illegal_error() {
        let lexer = super::lexer::TokenLexer::new(vec![Token::Illegal]);
        let mut parser = Parser::new(Box::new(lexer));

        assert_eq!(parser.construct_ast(), Err(RIError::InvalidCharacter(TokenSpan::CountSpan(Token::Illegal, 0))));
    }
}

mod repl {
    use std::io::{self, BufRead, Write};

    pub fn start_repl() {
        let stdin = io::stdin();

        loop {
            print!(">> ");
            io::stdout().flush().expect("error flushing stdout");

            let mut line = String::new();
            let bytes = stdin.lock().read_line(&mut line).expect("unable to read from input");

            if bytes == 0 {
                println!("");
                break;
            }

            print!("{}", line);
        }
    }
}

const VERSION: &str = "0.1.1";

fn print_usage() {
    let prog_name = std::env::args().nth(0).unwrap();

    println!("{} version {}, Copyright (C) 2019 Sam Stelfox", prog_name, VERSION);
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
        repl::start_repl();
    } else if args.len() == 2 {
        if Some("-h".to_string()) == std::env::args().nth(2) {
            print_usage();
        } else {
            // TODO: read in the specified file lex / parse / interpret it
            unimplemented!();
        }
    } else {
        print_usage();
    }
}
