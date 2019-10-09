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
        // Note: All numbers in this language are handled as floats
        Number(f64),
        Text(String),
    }

    #[derive(Debug)]
    pub struct Token {
        pub token_type: TokenType,
        pub literal: Option<Literal>,
        pub location: Option<SourceLocation>,
    }

    impl PartialEq for Token {
        // For the sake of equality the source location doesn't matter
        fn eq(&self, other: &Self) -> bool {
            self.token_type == other.token_type && self.literal == other.literal
        }
    }

    impl Token {
        pub fn new(tok_t: TokenType) -> Self {
            Token {
                token_type: tok_t,
                literal: None,
                location: None,
            }
        }

        pub fn set_literal(mut self, new_literal: Option<Literal>) -> Self {
            self.literal = new_literal;
            self
        }

        pub fn set_location(mut self, new_loc: Option<SourceLocation>) -> Self {
            self.location = new_loc;
            self
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

        // These aren't present in the reference language as tokens
        Comment,
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
        pub fn extend_to(&mut self, end_pos: usize) {
            use self::SourceLocation::*;

            let start_pos = match *self {
                Point(start) => start,
                Range(start, _) => start,
            };

            if start_pos != end_pos {
                // Should I do some kind of validation here? Check that end > start? Nah not for now...
                *self = Range(start_pos, end_pos);
            }
        }

        // SourceLocation's always start as a single character location, but can be extended to
        // cover a range.
        pub fn new(loc: usize) -> Self {
            SourceLocation::Point(loc)
        }
    }
}

mod lexer {
    use std::collections::VecDeque;
    use std::iter::Peekable;
    use std::str::Chars;

    use crate::tokens::{Literal, SourceLocation, Token, TokenType};

    pub trait Lexer: Iterator {
        fn had_error(&self) -> bool;
        fn next_token(&mut self) -> Token;
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

    impl Iterator for TokenLexer {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            if self.done {
                return None;
            }

            Some(self.next_token())
        }
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

    impl Iterator for InputLexer<'_> {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            if self.done {
                return None;
            }

            Some(self.next_token())
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
}

mod parser {
    use crate::tokens::{Literal, Token};
    use std::fmt::Debug;

    pub trait Expression: Debug {
        fn accept(&self, visitor: Box<&dyn ExpressionVisitor>);
    }

    macro_rules! define_expression {
        ($type_name:ident($($field_name:ident: $field_type:ty),*).$visit_name:ident) => {
            #[derive(Debug)]
            pub struct $type_name {
                $(
                    pub $field_name: $field_type,
                )*
            }

            impl $type_name {
                pub fn new($($field_name: $field_type),+) -> Self {
                    Self {
                        $(
                            $field_name: $field_name,
                        )*
                    }
                }
            }

            impl Expression for $type_name {
                fn accept(&self, visitor: Box<&dyn ExpressionVisitor>) {
                    visitor.$visit_name(self);
                }
            }
        };
    }

    pub trait ExpressionVisitor {
        fn visit_binary_expression(&self, _expr: &BinaryExpression) {}
        fn visit_grouping_expression(&self, _expr: &GroupingExpression) {}
        fn visit_literal_expression(&self, _expr: &LiteralExpression) {}
        fn visit_unary_expression(&self, _expr: &UnaryExpression) {}
    }

    define_expression!(BinaryExpression(left: Box<dyn Expression>, operator: Token, right: Box<dyn Expression>).visit_binary_expression);
    define_expression!(GroupingExpression(expr: Box<dyn Expression>).visit_grouping_expression);
    define_expression!(LiteralExpression(value: Literal).visit_literal_expression);
    define_expression!(UnaryExpression(operator: Token, right: Box<dyn Expression>).visit_unary_expression);

    pub struct AstPrinter;

    impl AstPrinter {
        pub fn new() -> Self {
            Self { }
        }

        pub fn print(&self, expr: Box<&dyn Expression>) {
            expr.accept(Box::new(self));
            println!("");
        }
    }

    impl ExpressionVisitor for AstPrinter {
        fn visit_binary_expression(&self, expr: &BinaryExpression) {
            print!("({:?} ", expr.operator);
            expr.left.accept(Box::new(self));
            print!(" ");
            expr.right.accept(Box::new(self));
            print!(")");
        }

        fn visit_grouping_expression(&self, expr: &GroupingExpression) {
            print!("(group ");
            expr.expr.accept(Box::new(self));
            print!(")");
        }

        fn visit_literal_expression(&self, expr: &LiteralExpression) {
            match &expr.value {
                Literal::Identifier(id) => { print!("id[{}]", id); }
                Literal::Invalid(ch) => { print!("!invalid[{}]!", ch); }
                Literal::Number(n) => { print!("num[{}]", n); }
                Literal::Text(txt) => { print!("txt[{}]", txt); }
            }
        }

        fn visit_unary_expression(&self, expr: &UnaryExpression) {
            print!("({:?} ", expr.operator);
            expr.right.accept(Box::new(self));
            print!(")");
        }
    }
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

        if lexer.had_error() {
            println!("Error: Encountered invalid tokens during lexing");
            std::process::exit(65);
        }
    }

    pub fn start_repl() {
        let stdin = io::stdin();

        loop {
            // TODO: Don't print this or flush if we're piping from STDIN
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

            if lexer.had_error() {
                println!("Error: Encountered invalid tokens during lexing");
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

use parser::{AstPrinter, BinaryExpression, GroupingExpression, LiteralExpression, UnaryExpression};
use tokens::{Literal, Token, TokenType};

fn main() {
    let args = std::env::args();

    let left_literal = LiteralExpression::new(Literal::Number(123.0));
    let left_unary = UnaryExpression::new(Token::new(TokenType::Minus), Box::new(left_literal));

    let right_literal = LiteralExpression::new(Literal::Number(45.67));
    let right_grouping = GroupingExpression::new(Box::new(right_literal));

    let root_expr = BinaryExpression::new(Box::new(left_unary), Token::new(TokenType::Star), Box::new(right_grouping));

    let ast_printer = AstPrinter::new();
    ast_printer.print(Box::new(&root_expr));

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
        // Command line usage error (/usr/include/sysexits.h)
        print_usage();
        std::process::exit(64);
    }
}
