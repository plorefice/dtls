use std::{
    env,
    fs::File,
    io::{self, BufReader, Read},
    process,
};

use ariadne::{sources, Label, Report, ReportKind};
use chumsky::{error::SimpleReason, text::Character};

fn main() {
    let (reader, name): (Box<dyn Read>, _) = match env::args().nth(1) {
        Some(fname) => (Box::new(File::open(&fname).unwrap()), fname),
        None => (Box::new(io::stdin()), "stdin".into()),
    };

    let source = {
        let mut buf = String::new();
        BufReader::new(reader).read_to_string(&mut buf).unwrap();
        buf
    };

    let err = match dts_parser::from_str(&source) {
        Ok(ast) => {
            println!("{ast:#?}");
            return;
        }
        Err(err) => {
            // Take only the first error for now
            err[0].clone()
        }
    };

    let message = match err.reason() {
        SimpleReason::Unexpected => "unexpected token".to_string(),
        SimpleReason::Unclosed { delimiter, .. } => {
            format!("unclosed delimited '{}'", delimiter.to_char())
        }
        SimpleReason::Custom(s) => s.clone(),
    };

    let expected = err
        .expected()
        .filter_map(|c| c.map(|c| format!("'{c}'")))
        .collect::<Vec<_>>()
        .join(", ");

    let mut label_msg = format!("expected one of {expected}");
    if let Some(b) = err.found() {
        label_msg += &format!(", found '{b}'");
    };

    Report::build(ReportKind::Error, name.clone(), 42)
        .with_message(message)
        .with_label(Label::new((name.clone(), err.span())).with_message(label_msg))
        .finish()
        .eprint(sources([(name, source)]))
        .unwrap();

    process::exit(-1);
}
