#![feature(cold_path)]

//! BBNF JSON WASM backend benchmark — native WASM execution throughput.
//!
//! Compiles grammar → WAT, instantiates via wasmtime with real DFA regex
//! host functions from `parse_that::regex`. Measures parse throughput.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::sync::Arc;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};
use parse_that::regex::Dfa;
use wasmtime::*;

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

fn load_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json.bbnf").expect("failed to read json.bbnf")
}

/// Compiled WASM bundle: WAT bytes + DFA regex engines for host functions.
struct WasmBundle {
    engine: Engine,
    wat_bytes: Vec<u8>,
    dfas: Arc<Vec<Dfa>>,
}

/// Compile grammar to WAT, extract regex patterns from the emitter, build DFAs.
///
/// The emitter collects regex patterns in encounter order during compilation.
/// We run the compilation manually (not via pipeline) to access the emitter's
/// pattern list, ensuring the DFA indices match the `(i32.const pattern_id)`
/// values in the generated WAT.
fn compiled_wasm() -> WasmBundle {
    let grammar = load_grammar();

    // Compile to IR first (shared passes).
    let mut ir = match compile_grammar_request(
        &grammar,
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
    bbnf_ir::dag::ensure_dag(&mut ir);
    bbnf_ir::passes::project_types(&mut ir);

    // Run driver + WASM emitter manually to access regex_patterns.
    let analysis = bbnf::backend::driver::analysis::BackendAnalysis::default();
    let call_strategies =
        vec![bbnf::backend::CallStrategy::DirectCall; ir.rules.len()];
    let mut dstate = bbnf::backend::driver::DriverState::new(call_strategies);
    let ws_regex_id = ir.ws_pattern.map(|ws_sid| dstate.register_regex(ir.get_string(ws_sid)));
    let mut emitter = bbnf::backend::wasm::WasmEmitter {
        module_name: "json_parser".into(),
        ws_regex_id,
    };
    let mut ctx = bbnf::backend::wasm::emitter::WasmEmitCtx::default();

    let wat_source =
        bbnf::backend::driver::compile_grammar(&ir, &analysis, &mut dstate, &mut emitter, &mut ctx);
    let wat_bytes = wat_source.into_bytes();

    // Build DFAs from the driver's pattern list (guaranteed matching order).
    let dfas: Vec<Dfa> = dstate
        .regex_patterns
        .iter()
        .map(|pattern| {
            Dfa::compile(pattern)
                .unwrap_or_else(|| panic!("failed to compile DFA for '{}'", pattern))
        })
        .collect();

    WasmBundle {
        engine: Engine::default(),
        wat_bytes,
        dfas: Arc::new(dfas),
    }
}

/// Store data for wasmtime host functions.
struct HostState {
    dfas: Arc<Vec<Dfa>>,
}

/// Create a store + instance with real DFA host functions and input data.
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

    // Host function: match_regex(pattern_id, offset, input_len) → new_offset or -1.
    // Uses our parse_that DFA engine — same engine as the bytecode VM.
    linker.func_wrap(
        "host",
        "match_regex",
        |mut caller: Caller<'_, HostState>, pattern_id: i32, off: i32, len: i32| -> i32 {
            let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
            let data = memory.data(&caller);
            let input_bytes = &data[..len as usize];
            let dfas = &caller.data().dfas;
            let idx = pattern_id as usize;
            if idx >= dfas.len() {
                return -1;
            }
            match dfas[idx].find_at(input_bytes, off as usize) {
                Some(end) => end as i32,
                None => -1,
            }
        },
    )?;

    // Host function: number_convert(offset, input_len) → new_offset or -1.
    // Uses a regex DFA for number scanning (pattern 0 if it's the number regex).
    linker.func_wrap(
        "host",
        "number_convert",
        |mut caller: Caller<'_, HostState>, off: i32, len: i32| -> i32 {
            let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
            let data = memory.data(&caller);
            let input_bytes = &data[..len as usize];
            // Scan for a JSON number manually: [-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?
            // Use a simple byte-level scanner.
            let mut pos = off as usize;
            if pos >= input_bytes.len() {
                return -1;
            }
            // Optional sign.
            if input_bytes[pos] == b'-' || input_bytes[pos] == b'+' {
                pos += 1;
            }
            if pos >= input_bytes.len() || !input_bytes[pos].is_ascii_digit() {
                return -1;
            }
            while pos < input_bytes.len() && input_bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            // Optional fraction.
            if pos < input_bytes.len() && input_bytes[pos] == b'.' {
                pos += 1;
                while pos < input_bytes.len() && input_bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
            // Optional exponent.
            if pos < input_bytes.len() && (input_bytes[pos] == b'e' || input_bytes[pos] == b'E') {
                pos += 1;
                if pos < input_bytes.len()
                    && (input_bytes[pos] == b'+' || input_bytes[pos] == b'-')
                {
                    pos += 1;
                }
                if pos >= input_bytes.len() || !input_bytes[pos].is_ascii_digit() {
                    return -1;
                }
                while pos < input_bytes.len() && input_bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
            pos as i32
        },
    )?;

    let instance = linker.instantiate(&mut store, &module)?;

    // Copy input bytes to linear memory.
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("missing memory export");
    let input_bytes = input.as_bytes();
    let input_len = input_bytes.len() as u32;

    // Grow memory if needed.
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

            // Instantiate ONCE outside the loop (same as VM pre-compiles once).
            let (mut store, instance, input_len) =
                instantiate_with_input(&bundle, &input).unwrap();
            let parse = instance
                .get_typed_func::<(i32, i32), i32>(&mut store, "parse")
                .unwrap();
            let memory = instance
                .get_memory(&mut store, "memory")
                .expect("missing memory export");

            {
                // Warmup + correctness: assert the parse consumed full input.
                let result = parse.call(&mut store, (0, input_len as i32)).unwrap();
                assert!(
                    result >= 0 && (result as usize) >= input.trim_end().len(),
                    concat!($file, ": WASM parse failed or incomplete ({}/{})"),
                    result,
                    input.len(),
                );
            }
            b.iter(|| {
                // Re-copy input bytes (parser may have mutated linear memory state).
                let input_bytes = black_box(&input).as_bytes();
                memory.data_mut(&mut store)[..input_bytes.len()]
                    .copy_from_slice(input_bytes);
                let result = parse.call(&mut store, (0, input_len as i32)).unwrap();
                black_box(result);
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
