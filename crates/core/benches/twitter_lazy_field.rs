//! AX.W1.A.5 — lazy-field-extraction bench over twitter.json via
//! `bbnf::json::Value`.
//!
//! Consumer-bench for W1.A's Value projection API. The bench parses
//! `data/json/twitter.json` through `JsonParser::parse`, projects the
//! root through `Value::from_tape`, then iterates `statuses[].text`
//! to exercise the `Array` + `Object::get` traversal surface.
//!
//! Matched against `sonic_rs::from_str::<sonic_rs::Value>` + the
//! same `statuses[].text` extraction so the two numbers are directly
//! comparable under identical `bencher` measurement overhead.
//!
//! W1.A ships SoA-only. W1.D's AoS sidecar (`packed_cache`) lands
//! later and will redirect single-record random-access reads through
//! the packed layout; this bench then exercises that path without
//! bench-side modifications — the Value walker stays identical, and
//! the hybrid switch is internal to `tape::columns`.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::path::PathBuf;

use bbnf::json::{JsonRuleIds, Value};
use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[path = "common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

// ── Inputs + rule-id resolution ───────────────────────────────────

fn load_twitter() -> String {
    let path = PathBuf::from("../../data/json/twitter.json");
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path.display(), e))
}

fn rule_ids() -> &'static JsonRuleIds {
    // One IR compile per process. Resolving rule ids per bench
    // iteration would dominate the measurement; the compile + table
    // build is lifted out so the hot loop sees the ids as a
    // `&'static` reference.
    static IDS: std::sync::OnceLock<JsonRuleIds> = std::sync::OnceLock::new();
    IDS.get_or_init(|| {
        let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|p| p.parent())
            .expect("workspace root")
            .to_path_buf();
        let bbnf_path = workspace.join("grammar/json/json.bbnf");
        let req = bbnf::pipeline::CompileRequest {
            target: bbnf::pipeline::CompileTarget::Vm,
            options: bbnf::pipeline::PipelineOptions::default(),
        };
        let output =
            bbnf::pipeline::compile_paths_request(&[bbnf_path], &req).expect("json compile");
        let ir = match output {
            bbnf::pipeline::CompileOutput::Vm(ir) => ir,
            _ => panic!("expected Vm output"),
        };
        JsonRuleIds::from_ir(&ir)
    })
}

// ── Extraction logic ──────────────────────────────────────────────

/// Extract every `text` field from every entry in the top-level
/// `statuses` array. Accumulates total text bytes + count as a
/// `u64` so the compiler can't elide the iteration.
fn extract_statuses_text_bbnf(value: &Value<'_>) -> u64 {
    let Value::Object(top) = value else {
        return 0;
    };
    let Some(Value::Array(statuses)) = top.get("statuses") else {
        return 0;
    };
    let mut total: u64 = 0;
    for status in statuses.iter() {
        let Value::Object(status_obj) = status else {
            continue;
        };
        if let Some(Value::String(text)) = status_obj.get("text") {
            total = total.wrapping_add(text.len() as u64);
        }
    }
    total
}

fn extract_statuses_text_sonic(value: &sonic_rs::Value) -> u64 {
    use sonic_rs::JsonContainerTrait;
    use sonic_rs::JsonValueTrait;
    let Some(statuses) = value.get("statuses").and_then(|v| v.as_array().cloned()) else {
        return 0;
    };
    let mut total: u64 = 0;
    for status in statuses.iter() {
        if let Some(text) = status.get("text").and_then(|v| v.as_str()) {
            total = total.wrapping_add(text.len() as u64);
        }
    }
    total
}

// ── Bench drivers ─────────────────────────────────────────────────

/// BBNF lane — full parse + Value projection + text extraction.
fn bbnf_parse_project_extract(b: &mut Bencher) {
    let input = load_twitter();
    b.bytes = input.len() as u64;
    // Warm-up validates the extraction works end-to-end on this
    // corpus before the timing loop starts.
    {
        let parsed = JsonParser::parse(&input).expect("twitter parse warm-up");
        let tape = parsed.tape();
        let root = parsed.root_offset();
        let value = Value::from_tape(tape, &input, root, rule_ids());
        let total = extract_statuses_text_bbnf(&value);
        black_box(total);
    }
    bench_with_timeout(b, limits::JSON_PARSE, || {
        let parsed = JsonParser::parse(black_box(&input)).unwrap();
        let tape = parsed.tape();
        let root = parsed.root_offset();
        let value = Value::from_tape(tape, black_box(&input), root, rule_ids());
        let total = extract_statuses_text_bbnf(&value);
        black_box(total);
        black_box(parsed);
    });
}

/// sonic-rs lane — `from_str::<Value>` + text extraction through
/// sonic's typed accessor surface.
fn sonic_parse_project_extract(b: &mut Bencher) {
    let input = load_twitter();
    b.bytes = input.len() as u64;
    {
        let value = sonic_rs::from_str::<sonic_rs::Value>(&input).expect("sonic warm-up");
        let total = extract_statuses_text_sonic(&value);
        black_box(total);
    }
    bench_with_timeout(b, limits::JSON_PARSE, || {
        let value = sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap();
        let total = extract_statuses_text_sonic(&value);
        black_box(total);
        black_box(value);
    });
}

benchmark_group!(benches, bbnf_parse_project_extract, sonic_parse_project_extract);
benchmark_main!(benches);
