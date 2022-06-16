use std::{
    env,
    fs::File,
    io::{self, BufReader, Read},
    process,
};

fn main() {
    let reader: Box<dyn Read> = if let Some(fname) = env::args().nth(1) {
        Box::new(File::open(fname).unwrap())
    } else {
        Box::new(io::stdin())
    };

    let source = {
        let mut buf = String::new();
        BufReader::new(reader).read_to_string(&mut buf).unwrap();
        buf
    };

    match dts_parser::from_str(source.as_str()) {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => {
            eprintln!("Errors: {:#?}", e);
            process::exit(-1);
        }
    }
}
