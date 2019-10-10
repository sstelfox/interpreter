use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Identifier(String),
    Invalid(char),
    // Note: All numbers in this language are handled as floats
    Number(f64),
    Text(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Identifier(id) => write!(f, "{}", id),
            Literal::Invalid(ch) => write!(f, "!I({})!", ch),
            Literal::Number(num) => write!(f, "{}", num),
            Literal::Text(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug)]
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

    pub fn set_literal(mut self, new_literal: Option<Literal>) -> Self {
        self.literal = new_literal;
        self
    }

    pub fn set_location(mut self, new_loc: Option<SourceLocation>) -> Self {
        self.location = new_loc;
        self
    }
}

impl PartialEq for Token {
    // For the sake of equality the source location doesn't matter
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type && self.literal == other.literal
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.literal {
            Some(lit) => write!(f, "{}({})", self.token_type, lit),
            None => write!(f, "{}", self.token_type),
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

    // These aren't present in the reference language as tokens
    Comment,
    Invalid,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenType::*;

        let out = match self {
            And => "and",
            Bang => "!",
            BangEqual => "!=",
            Class => "class",
            Comma => ",",
            Comment => "Comment",
            Dot => ".",
            Else => "else",
            EOF => "(EOF)",
            Equal => "=",
            EqualEqual => "==",
            False => "false",
            For => "for",
            Fun => "fun",
            Greater => ">",
            GreaterEqual => ">=",
            Identifier => "ID",
            If => "if",
            Invalid => "Invalid",
            LeftBrace => "{",
            LeftParen => "(",
            Less => "<",
            LessEqual => "<=",
            Minus => "-",
            Nil => "nil",
            Number => "Num",
            Or => "or",
            Plus => "+",
            Print => "print",
            Return => "return",
            RightBrace => "}",
            RightParen => "",
            SemiColon => ";",
            Slash => "/",
            Star => "*",
            Super => "super",
            Text => "Text",
            This => "this",
            True => "true",
            Var => "var",
            While => "while",
        };

        write!(f, "{}", out)
    }
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
