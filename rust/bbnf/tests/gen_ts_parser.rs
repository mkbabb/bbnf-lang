//! Generator: compiles JSON grammar to TypeScript and writes the output
//! to `benches/ts/generated_json.mjs` for the Node.js benchmark.
//!
//! Run: `cargo test -p bbnf --test gen_ts_parser -- --nocapture`

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

fn json_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json-pure.bbnf")
        .expect("failed to read json-pure.bbnf")
}

#[test]
fn generate_ts_parser_for_bench() {
    let grammar = json_grammar();
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    };
    let source = match compile_grammar_request(&grammar, &request).unwrap() {
        CompileOutput::Ts(src) => src,
        _ => panic!("expected TS output"),
    };

    // Strip TypeScript type annotations to produce valid JS (ESM).
    let js = strip_ts_types(&source);

    let out_path = std::path::Path::new("benches/ts/generated_json.mjs");
    std::fs::write(out_path, &js).expect("failed to write generated_json.mjs");
    eprintln!("Wrote {} bytes to {}", js.len(), out_path.display());
}

/// Minimal TS → JS strip: remove type annotations, interfaces, and type aliases.
/// Sufficient for the BBNF-generated output which uses simple TS syntax.
fn strip_ts_types(ts: &str) -> String {
    let mut lines: Vec<String> = Vec::new();
    let mut skip_block = false;

    for line in ts.lines() {
        let trimmed = line.trim();

        // Skip interface blocks.
        if trimmed.starts_with("interface ") {
            skip_block = true;
            continue;
        }
        // Skip type alias lines.
        if trimmed.starts_with("type ") {
            // Multi-line type: skip until we see a lone `;`
            if !trimmed.ends_with(';') {
                skip_block = true;
            }
            continue;
        }
        if skip_block {
            if trimmed == "}" || trimmed.ends_with(';') {
                skip_block = false;
            }
            continue;
        }

        // Strip inline type annotations:
        // `(s: ParserState): FooValue | null` → `(s)`
        // `as const` → ``
        let mut cleaned = line.to_string();
        // Remove `: TypeName` from function params and return types.
        // Simple regex-free approach: handle the patterns we emit.
        cleaned = cleaned.replace(" as const", "");

        // Strip parameter types: `(s: ParserState)` → `(s)`
        if let Some(start) = cleaned.find("(s: ParserState)") {
            cleaned = cleaned.replace("(s: ParserState)", "(s)");
        }

        // Strip return type annotations: `): FooValue | null {` → `) {`
        if let Some(idx) = cleaned.find("): ") {
            if let Some(brace) = cleaned[idx..].find('{') {
                let before = &cleaned[..idx + 1];
                let after = &cleaned[idx + brace..];
                cleaned = format!("{before} {after}");
            }
        }

        // Strip `: { result: ... }` return type from export function
        if cleaned.contains("export function parse(input: string)") {
            cleaned = cleaned.replace("input: string", "input");
            if let Some(idx) = cleaned.find("): ") {
                if let Some(brace) = cleaned[idx..].find('{') {
                    let before = &cleaned[..idx + 1];
                    let after = &cleaned[idx + brace..];
                    cleaned = format!("{before} {after}");
                }
            }
        }

        lines.push(cleaned);
    }

    lines.join("\n")
}
