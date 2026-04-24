
//! BBNF JSON bytecode VM benchmark — cold per-parse.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf::pipeline::{PipelineOptions, compile_grammar};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use divan::black_box;

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
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            let (_ir, program) = compiled_vm();
            {
                let mut interp = Interpreter::new(&program, &input);
                let r = interp.run();
                assert!(r.success, concat!($file, ": VM parse failed"));
                assert!(
                    r.offset as usize >= input.trim_end().len(),
                    concat!($file, ": VM incomplete parse ({}/{})"),
                    r.offset,
                    input.len(),
                );
            }
            b.with_inputs(|| input.clone()).bench_values(|input| {
                let mut interp = Interpreter::new(&program, black_box(&input));
                let r = interp.run();
                black_box(r.offset);
            });
        }
    };
}

bench!(data, "data.json");
bench!(twitter, "twitter.json");
bench!(citm, "citm_catalog.json");
bench!(canada, "canada.json");
bench!(data_xl, "data_xl.json");

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
