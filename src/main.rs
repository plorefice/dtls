mod ast;

use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let mut reader = if let Some(fname) = env::args().nth(1) {
        Box::new(File::open(fname).unwrap()) as Box<Read>
    } else {
        Box::new(std::io::stdin()) as Box<Read>
    };

    println!("{:#?}", ast::parse(&mut reader));
}
