use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

fn main() {
    let reader: Box<dyn Read> = if let Some(fname) = env::args().nth(1) {
        Box::new(File::open(fname).unwrap())
    } else {
        Box::new(std::io::stdin())
    };

    let source = {
        let mut buf = String::new();
        BufReader::new(reader).read_to_string(&mut buf).unwrap();
        buf
    };

    match dts_parser::from_str(source.as_str()) {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => panic!("Failed on input: {}", std::str::from_utf8(e.input).unwrap()),
    }
}
