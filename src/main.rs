use std::io::{self, BufRead, Write};
use std::str::Chars;
use std::iter::Peekable;
use std::fmt;
use std::error::Error;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token {
    Integer(i64),

    LeftParen,
    RightParen,

    Minus,
    Plus,

    Division,
    Multiplication,

    EOF,
    Illegal,
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
            Some('(') => Token::LeftParen,
            Some(')') => Token::RightParen,
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

#[derive(Debug)]
struct NodeTree {
    value: Node,

    left: Option<Box<NodeTree>>,
    right: Option<Box<NodeTree>>,
}

impl NodeTree {
    fn new(node: Node) -> Self {
        NodeTree {
            value: node,
            left: None,
            right: None,
        }
    }

    fn visit(&self) -> Result<Token, ParserError> {
        match self.value {
            Node::EOF => Ok(Token::EOF),
            Node::Numeric(token) => Ok(token),
            Node::BinaryOp(token) => {
                match (&self.left, &self.right) {
                    (Some(l), Some(r)) => {
                        let l_value = l.visit()?;
                        let r_value = r.visit()?;

                        match token {
                            Token::Division => {
                                match (l_value, r_value) {
                                    (Token::Integer(lint), Token::Integer(rint)) => Ok(Token::Integer(lint / rint)),
                                    (_, _) => Err(ParserError::SyntaxError),
                                }
                            },
                            Token::Minus => {
                                match (l_value, r_value) {
                                    (Token::Integer(lint), Token::Integer(rint)) => Ok(Token::Integer(lint - rint)),
                                    (_, _) => Err(ParserError::SyntaxError),
                                }
                            },
                            Token::Multiplication => {
                                match (l_value, r_value) {
                                    (Token::Integer(lint), Token::Integer(rint)) => Ok(Token::Integer(lint * rint)),
                                    (_, _) => Err(ParserError::SyntaxError),
                                }
                            },
                            Token::Plus => {
                                match (l_value, r_value) {
                                    (Token::Integer(lint), Token::Integer(rint)) => Ok(Token::Integer(lint + rint)),
                                    (_, _) => Err(ParserError::SyntaxError),
                                }
                            },
                            _ => Err(ParserError::SyntaxError),
                        }
                    },
                    (_, _) => {
                        Err(ParserError::SyntaxError)
                    },
                }
            },
        }
    }
}

impl From<Token> for NodeTree {
    fn from(token: Token) -> Self {
        match token {
            Token::Integer(_) => NodeTree::new(Node::Numeric(token)),
            Token::Plus | Token::Minus | Token::Multiplication | Token::Division => {
                NodeTree::new(Node::BinaryOp(token))
            },
            Token::EOF => NodeTree::new(Node::EOF),
            _ => {
                panic!("not a valid token for a tree node: {:?}", token);
            },
        }
    }
}

#[derive(Debug, PartialEq)]
enum Node {
    BinaryOp(Token),
    Numeric(Token),
    EOF,
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

    fn factor(&mut self) -> Result<NodeTree, ParserError> {
        match self.current_token {
            Token::EOF => Ok(NodeTree::from(Token::EOF)),
            Token::Integer(num) => {
                self.advance();
                Ok(NodeTree::from(Token::Integer(num)))
            },
            Token::Minus => {
                self.advance();

                // Build a short new tree that negates whatever the current
                // factor of trees is
                let mut tree = NodeTree::from(Token::Multiplication);
                let left = self.factor()?;

                tree.left = Some(Box::new(left));
                tree.right = Some(Box::new(NodeTree::from(Token::Integer(-1))));

                Ok(tree)
            },
            Token::LeftParen => {
                self.advance();
                let tree = self.expr()?;

                if self.current_token == Token::RightParen {
                    self.advance();
                    Ok(tree)
                } else {
                    Err(ParserError::SyntaxError)
                }
            },
            _ => Err(ParserError::SyntaxError),
        }
    }

    fn term(&mut self) -> Result<NodeTree, ParserError> {
        let mut current_tree = self.factor()?;

        loop {
            match self.current_token {
                Token::Division | Token::Multiplication => {
                    let mut new_tree = NodeTree::from(self.current_token);

                    self.advance();
                    let right = self.factor()?;

                    new_tree.left = Some(Box::new(current_tree));
                    new_tree.right = Some(Box::new(right));
                    current_tree = new_tree;
                },
                _ => break,
            }
        }

        Ok(current_tree)
    }

    fn expr(&mut self) -> Result<NodeTree, ParserError> {
        let mut current_tree = self.term()?;

        loop {
            match self.current_token {
                Token::Minus | Token::Plus => {
                    let mut new_tree = NodeTree::from(self.current_token);
                    self.advance();

                    let right = self.term()?;

                    new_tree.left = Some(Box::new(current_tree));
                    new_tree.right = Some(Box::new(right));
                    current_tree = new_tree;
                },
                _ => break,
            }
        }

        Ok(current_tree)
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

        let ast = match parser.expr() {
            Ok(tree) => tree,
            Err(err) => {
                println!("Error: {}", err);
                continue;
            },
        };

        if ast.value == Node::EOF {
            break;
        }

        println!("{:?}", ast);
        match ast.visit() {
            Ok(token) => { println!("{:?}", token) },
            Err(err) => { println!("Interpreter error: {}", err) },
        }
    }

    // Clear the prompt on exit
    println!("");
}
