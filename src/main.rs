mod ast;
mod errors;
mod interpreter;
mod lexer;
mod tokens;

use ast::{AstPrinter, Expression};
use tokens::{Literal, Token, TokenType};

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

    let left_literal = Expression::Literal(Literal::Number(123.0));
    let left_unary = Expression::Unary(Token::new(TokenType::Minus), Box::new(left_literal));

    let right_literal = Expression::Literal(Literal::Number(45.67));
    let right_grouping = Expression::Grouping(Box::new(right_literal));

    let root_expr = Expression::Binary(Box::new(left_unary), Token::new(TokenType::Star), Box::new(right_grouping));

    let mut ast_printer = AstPrinter::new();
    ast_printer.print(&root_expr);

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
