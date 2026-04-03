#![feature(cold_path)]

//! BBNF CSS L4 WASM backend benchmark — native WASM execution via wasmtime.
//!
//! Uses our DFA regex engine for host functions (same as VM/Rust paths).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::path::PathBuf;
use std::sync::Arc;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};
use parse_that::regex_engine::Dfa;
use wasmtime::*;

fn load(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

fn grammar_paths() -> Vec<PathBuf> {
    vec![PathBuf::from("../../grammar/css/l4/stylesheet.bbnf")]
}

struct WasmBundle {
    engine: Engine,
    wat_bytes: Vec<u8>,
    dfas: Arc<Vec<Dfa>>,
}

fn compiled_wasm() -> WasmBundle {
    let paths = grammar_paths();

    // Compile to IR to get type info.
    let mut ir = match compile_paths_request(
        &paths,
        &CompileRequest {
            options: PipelineOptions::default(),
            target: CompileTarget::Vm,
        },
    )
    .unwrap()
    {
        CompileOutput::Vm(ir) => ir,
        _ => unreachable!(),
    };
    bbnf_ir::passes::compute_sp_method_rules(&mut ir);
    bbnf_ir::passes::project_types(&mut ir);

    // Run driver + WASM emitter to get WAT + regex patterns.
    let analysis = bbnf::backend::analysis::BackendAnalysis::default();
    let call_strategies = vec![bbnf::backend::CallStrategy::DirectCall; ir.rules.len()];
    let mut dstate = bbnf::backend::driver::DriverState::new(call_strategies);
    // Pre-register ws pattern so the emitter knows its ID.
    let ws_regex_id = ir.ws_pattern.map(|ws_sid| dstate.register_regex(ir.get_string(ws_sid)));
    let mut emitter = bbnf::backend::wasm::WasmEmitter {
        module_name: "css_parser".into(),
        ws_regex_id,
    };
    let mut ctx = bbnf::backend::wasm::emitter::WasmEmitCtx::default();

    let wat_source =
        bbnf::backend::driver::compile_grammar(&ir, &analysis, &mut dstate, &mut emitter, &mut ctx);
    let wat_bytes = wat_source.into_bytes();

    let dfas: Vec<Dfa> = dstate
        .regex_patterns
        .iter()
        .map(|p| Dfa::compile(p).unwrap_or_else(|| panic!("failed to compile DFA: {p}")))
        .collect();

    WasmBundle {
        engine: Engine::default(),
        wat_bytes,
        dfas: Arc::new(dfas),
    }
}

struct HostState {
    dfas: Arc<Vec<Dfa>>,
}

fn instantiate_with_input(
    bundle: &WasmBundle,
    input: &str,
) -> Result<(Store<HostState>, Instance, u32)> {
    let module = Module::new(&bundle.engine, &*bundle.wat_bytes)?;
    let state = HostState {
        dfas: bundle.dfas.clone(),
    };
    let mut store = Store::new(&bundle.engine, state);
    let mut linker = Linker::new(&bundle.engine);

    linker.func_wrap(
        "host",
        "match_regex",
        |mut caller: Caller<'_, HostState>, pid: i32, off: i32, len: i32| -> i32 {
            let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
            let data = mem.data(&caller);
            let bytes = &data[..len as usize];
            let dfas = &caller.data().dfas;
            if (pid as usize) >= dfas.len() {
                return -1;
            }
            match dfas[pid as usize].find_at(bytes, off as usize) {
                Some(end) => end as i32,
                None => -1,
            }
        },
    )?;

    linker.func_wrap(
        "host",
        "number_convert",
        |mut caller: Caller<'_, HostState>, off: i32, len: i32| -> i32 {
            let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
            let data = mem.data(&caller);
            let bytes = &data[..len as usize];
            let mut pos = off as usize;
            if pos >= bytes.len() {
                return -1;
            }
            if bytes[pos] == b'-' || bytes[pos] == b'+' {
                pos += 1;
            }
            if pos >= bytes.len() || !bytes[pos].is_ascii_digit() {
                return -1;
            }
            while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            if pos < bytes.len() && bytes[pos] == b'.' {
                pos += 1;
                while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
            if pos < bytes.len() && (bytes[pos] == b'e' || bytes[pos] == b'E') {
                pos += 1;
                if pos < bytes.len() && (bytes[pos] == b'+' || bytes[pos] == b'-') {
                    pos += 1;
                }
                while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
            pos as i32
        },
    )?;

    let instance = linker.instantiate(&mut store, &module)?;
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("missing memory");
    let input_bytes = input.as_bytes();
    let input_len = input_bytes.len() as u32;

    let pages_needed = (input_bytes.len() / 65536) + 1;
    let current_pages = memory.size(&store) as usize;
    if pages_needed > current_pages {
        memory
            .grow(&mut store, (pages_needed - current_pages) as u64)
            .expect("failed to grow memory");
    }
    memory.data_mut(&mut store)[..input_bytes.len()].copy_from_slice(input_bytes);

    Ok((store, instance, input_len))
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            let bundle = compiled_wasm();
            b.bytes = input.len() as u64;

            let (mut store, instance, input_len) =
                instantiate_with_input(&bundle, &input).unwrap();
            let parse = instance
                .get_typed_func::<(i32, i32), i32>(&mut store, "parse")
                .unwrap();
            let memory = instance
                .get_memory(&mut store, "memory")
                .expect("missing memory");

            {
                let result = parse.call(&mut store, (0, input_len as i32)).unwrap();
                assert!(
                    result >= 0 && (result as usize) >= input.trim_end().len(),
                    concat!($file, ": WASM CSS parse failed or incomplete ({}/{})"),
                    result,
                    input.len(),
                );
            }
            b.iter(|| {
                let input_bytes = black_box(&input).as_bytes();
                memory.data_mut(&mut store)[..input_bytes.len()]
                    .copy_from_slice(input_bytes);
                let result = parse.call(&mut store, (0, input_len as i32)).unwrap();
                black_box(result);
            });
        }
    };
}

bench!(normalize, "normalize.css");
bench!(bootstrap, "bootstrap.css");

benchmark_group!(benches, normalize, bootstrap);
benchmark_main!(benches);
