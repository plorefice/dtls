use std::{env, fs::File, io::Read};

mod ast;

fn main() {
    let mut reader: Box<dyn Read> = if let Some(fname) = env::args().nth(1) {
        Box::new(File::open(fname).unwrap())
    } else {
        Box::new(std::io::stdin())
    };

    match ast::from_reader(&mut reader) {
        Ok(ast) => println!("{:?}", ast),
        Err(e) => panic!("{}", e),
    }
}