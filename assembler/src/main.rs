use parser::Parser;

use crate::lexer::lex;

mod lexer;
mod span;
mod parser;
mod ast;

fn main() {
    let file = "programs/fibonacci.l64";
    let src = std::fs::read_to_string(file).unwrap();
    let tokens = lex(&src);

    let ast = Parser::new(&tokens).parse();
    println!("{ast:#?}");
}
