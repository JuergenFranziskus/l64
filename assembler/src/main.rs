use crate::lexer::lex;
use code_gen::CodeGen;
use parser::Parser;

mod ast;
mod code_gen;
mod lexer;
mod parser;
mod span;

fn main() {
    let file = "programs/fibonacci.l64";
    let src = std::fs::read_to_string(file).unwrap();
    let tokens = lex(&src);

    let ast = Parser::new(&tokens).parse();
    let mut encoder = CodeGen::new();
    encoder.gen_code(&ast);
    let bytes = encoder.finish();

    println!("v3.0 hex words addressed");
    for (i, chunk) in bytes.chunks(16).enumerate() {
        print_hex(i * 16);
        print!(": ");
        for &byte in chunk {
            print_hex(byte as usize);
            print!(" ");
        }
        println!();
    }

}

fn print_hex(num: usize) {
    let num = &format!("{num:#04x}")[2..];
    print!("{num}");
}
