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
