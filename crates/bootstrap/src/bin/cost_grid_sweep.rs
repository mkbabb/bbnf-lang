//! AW-IV.W5.3 — cost-model grid sweep harness.
//!
//! Diagnostic binary that compiles a grammar through the full
//! pipeline, lifts the DTA table, and reports the resulting state
//! count + extraction wall-clock. Weights are read from the standard
//! `BBNF_COST_*` env-var surface (see `bbnf_ir::CostConfig::from_env`).
//!
//! Intended to be invoked repeatedly under different `BBNF_COST_*`
//! weight configurations by `scripts/cost-grid-sweep.sh`; each
//! invocation prints one JSON object to stdout so the outer harness
//! can aggregate results without re-parsing human-readable output.
//!
//! Usage:
//!   cargo run --release -p bbnf-bootstrap --bin cost_grid_sweep -- <file.bbnf>
//!
//! Emits one JSON line to stdout of shape:
//!   {"grammar":"<path>","state_count":<n>,"extraction_ns":<n>,...}

use std::path::PathBuf;
use std::time::Instant;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::passes::lift_dta;

fn json_esc(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out
}

fn emit_error(path: &str, msg: &str) -> ! {
    println!(
        "{{\"grammar\":\"{}\",\"error\":\"{}\"}}",
        json_esc(path),
        json_esc(msg)
    );
    std::process::exit(1);
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("usage: cost_grid_sweep <grammar.bbnf>");
        std::process::exit(2);
    }
    let path_str = args[0].clone();
    let path = PathBuf::from(&path_str);

    let request = CompileRequest {
        target: CompileTarget::Vm,
        options: PipelineOptions::default(),
    };

    // Full pipeline wall-clock: lowering + normalizer loop + egraph
    // extraction + every downstream pass up through `CompileOutput::Vm`.
    // The egraph extraction is embedded inside; isolating just
    // extraction would require duplicating the pipeline stub. The
    // wall-clock delta between two runs at identical weights elsewhere
    // and only CostWeights differing localises the measurement to the
    // extraction stage (every other pass is deterministic w.r.t.
    // grammar source).
    let pipeline_start = Instant::now();
    let output = match compile_paths_request(&[path.clone()], &request) {
        Ok(o) => o,
        Err(e) => emit_error(&path_str, &format!("compile error: {}", e)),
    };
    let pipeline_ns = pipeline_start.elapsed().as_nanos();

    let ir = match output {
        CompileOutput::Vm(ir) => ir,
        _ => emit_error(&path_str, "expected CompileOutput::Vm"),
    };

    // DTA lift — post-extraction ground truth for state count.
    let lift_start = Instant::now();
    let table = lift_dta(&ir);
    let lift_ns = lift_start.elapsed().as_nanos();

    // Read back the cost weights that actually drove extraction so the
    // harness can correlate the measurement to the configured
    // parameters without trusting env-var plumbing end-to-end.
    let w = &ir.cost_config.egraph.weights;
    let structural = w.structural;
    let alt_per_branch = w.alt_per_branch;
    let dispatch_bonus = w.dispatch_bonus;
    let literal_cost = ir.cost_config.literal_cost;
    let regex_cost = ir.cost_config.regex_cost;
    let ref_cost = ir.cost_config.ref_cost;
    let seq_per_child = ir.cost_config.seq_per_child;

    let rules_len = ir.rules.len();
    let state_count = table.states.len();
    let max_nest = table.max_nesting_depth;

    // One JSON line per invocation. No pretty-printing — the outer
    // shell harness concatenates into an array.
    println!(
        "{{\"grammar\":\"{}\",\"rules\":{},\"state_count\":{},\"max_nesting_depth\":{},\"pipeline_ns\":{},\"lift_ns\":{},\"weights\":{{\"structural\":{},\"alt_per_branch\":{},\"dispatch_bonus\":{},\"literal_cost\":{},\"regex_cost\":{},\"ref_cost\":{},\"seq_per_child\":{}}}}}",
        json_esc(&path_str),
        rules_len,
        state_count,
        max_nest,
        pipeline_ns,
        lift_ns,
        structural,
        alt_per_branch,
        dispatch_bonus,
        literal_cost,
        regex_cost,
        ref_cost,
        seq_per_child,
    );
}
