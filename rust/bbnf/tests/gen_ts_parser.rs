//! Generator: compiles JSON grammar to TypeScript and writes the output
//! to `benches/ts/generated_json.mjs` for the Node.js benchmark.
//!
//! Run: `cargo test -p bbnf --test gen_ts_parser -- --nocapture`

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

fn json_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json.bbnf")
        .expect("failed to read json.bbnf")
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

/// Strip TypeScript type annotations to produce valid ESM JavaScript.
///
/// Handles the patterns emitted by TsEmitter:
/// - `interface Foo { ... }` blocks → removed
/// - `type Foo = ...;` declarations → removed
/// - `(s: ParserState): RetType | null {` → `(s) {`
/// - `as const` → removed
/// - `start: number, end: number` → `start, end`
fn strip_ts_types(ts: &str) -> String {
    let mut lines: Vec<String> = Vec::new();
    let mut brace_depth: i32 = 0;
    let mut skipping = false;

    for line in ts.lines() {
        let trimmed = line.trim();

        // Start skipping interface/type blocks.
        if !skipping
            && (trimmed.starts_with("interface ") || trimmed.starts_with("type "))
        {
            // Single-line type: `type Foo = bar;`
            if trimmed.ends_with(';') && !trimmed.contains('{') {
                continue;
            }
            // Multi-line: skip until matching brace close.
            skipping = true;
            brace_depth = 0;
        }

        if skipping {
            for ch in trimmed.chars() {
                if ch == '{' {
                    brace_depth += 1;
                }
                if ch == '}' {
                    brace_depth -= 1;
                }
            }
            // Multi-line type aliases end with `;` at depth 0.
            if brace_depth <= 0 && (trimmed.ends_with(';') || trimmed == "}") {
                skipping = false;
            }
            continue;
        }

        let mut cleaned = line.to_string();

        // Remove `as const`.
        cleaned = cleaned.replace(" as const", "");

        // Strip parameter types: `(s: ParserState)` → `(s)`.
        cleaned = cleaned.replace("(s: ParserState)", "(s)");
        cleaned = cleaned.replace("(input: string)", "(input)");

        // Strip parameter types in function declarations:
        // `(start: number, end: number)` → `(start, end)`
        cleaned = cleaned.replace("start: number, end: number", "start, end");
        cleaned = cleaned.replace("input: string", "input");

        // Strip return type: `): RetType | null {` → `) {`
        // For object return types like `): { result: ...; offset: ... } {`,
        // find the LAST `{` on the line as the function body start.
        if let Some(idx) = cleaned.find("): ") {
            if let Some(last_brace) = cleaned.rfind('{') {
                if last_brace > idx {
                    let before = &cleaned[..idx + 1];
                    let after = &cleaned[last_brace..];
                    cleaned = format!("{before} {after}");
                }
            }
        }

        lines.push(cleaned);
    }

    lines.join("\n")
}
