use std::{
    env,
    fs::File,
    io::{self, BufReader, Read},
};

fn main() {
    let reader: Box<dyn Read> = match env::args().nth(1) {
        Some(fname) => Box::new(File::open(fname).unwrap()),
        None => Box::new(io::stdin()),
    };

    let source = {
        let mut buf = String::new();
        BufReader::new(reader).read_to_string(&mut buf).unwrap();
        buf
    };

    println!("{:?}", dts_parser::from_str(&source));
}
