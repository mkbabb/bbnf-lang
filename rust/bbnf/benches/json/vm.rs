#![feature(cold_path)]

//! BBNF JSON bytecode VM benchmark — cold per-parse.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf::pipeline::{PipelineOptions, compile_grammar};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

fn compiled_vm() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar =
        std::fs::read_to_string("../../grammar/json/json.bbnf").expect("failed to read json.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            let (_ir, program) = compiled_vm();
            b.bytes = input.len() as u64;
            b.iter(|| {
                let mut interp = Interpreter::new(&program, black_box(&input));
                let r = interp.run();
                assert!(r.success);
            });
        }
    };
}

bench!(data, "data.json");
bench!(twitter, "twitter.json");
bench!(citm, "citm_catalog.json");
bench!(canada, "canada.json");
bench!(data_xl, "data_xl.json");

benchmark_group!(benches, data, twitter, citm, canada, data_xl);
benchmark_main!(benches);
