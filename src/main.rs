use std::io::{self, BufRead, Write};

fn main() {
    let stdin = io::stdin();

    loop {
        print!(">> ");
        io::stdout().flush().expect("error flushing stdout");

        let mut line = String::new();
        let bytes = stdin.lock().read_line(&mut line).expect("unable to read from stdin");

        if bytes == 0 {
            break;
        }

        println!("{}", line);
    }
}
