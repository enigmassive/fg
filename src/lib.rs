mod ast;
mod codegen;
mod parse;
mod typecheck;

use std::io;

use codegen::gen;
use parse::parse;
use typecheck::check;

pub fn compile<Write: io::Write>(src: &str, dest: &mut Write) {
    let ast = parse(src);
    check(ast.clone()).unwrap();
    gen(ast, dest).unwrap();
}
