mod ast;

fn main() {
    println!("{:#?}", ast::parse_file(std::env::args().nth(1).unwrap()));
}
