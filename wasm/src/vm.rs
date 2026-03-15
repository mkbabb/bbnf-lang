//! Bytecode VM: grammar compilation, parsing, formatting, and handle management.
//!
//! Provides a WASM-friendly API where grammars are compiled once, stored by handle,
//! and reused for multiple parse/format calls.

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};

use bbnf::pipeline::{PipelineOptions, compile_grammar as compile_grammar_impl};
use bbnf_ir::GrammarIR;
use bbnf_ir::bytecode::BytecodeProgram;
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::{Interpreter, Value};
use serde::Serialize;
use wasm_bindgen::prelude::*;

// ── Grammar store ───────────────────────────────────────────────────────────

/// A compiled grammar handle: holds both the IR and pre-compiled bytecode.
struct CompiledGrammar {
    /// Used by `format_with_grammar` (pretty backend needs IR metadata).
    ir: GrammarIR,
    program: BytecodeProgram,
}

static NEXT_HANDLE: AtomicU32 = AtomicU32::new(1);

thread_local! {
    static GRAMMAR_STORE: RefCell<HashMap<u32, CompiledGrammar>> = RefCell::new(HashMap::new());
}

fn with_store<F, R>(f: F) -> R
where
    F: FnOnce(&mut HashMap<u32, CompiledGrammar>) -> R,
{
    GRAMMAR_STORE.with(|store| f(&mut store.borrow_mut()))
}

// ── Compile ─────────────────────────────────────────────────────────────────

/// Compile a BBNF grammar string into a bytecode program.
/// Returns a numeric handle for use with `parse_with_grammar` and `free_grammar`.
///
/// If `entry_rule` is provided and non-empty, it overrides the default entry rule
/// (which is the last rule in source order).
#[wasm_bindgen]
pub fn compile_grammar(grammar: &str, entry_rule: Option<String>) -> Result<u32, JsValue> {
    let options = PipelineOptions {
        entry_rule: entry_rule.filter(|s| !s.is_empty()),
        ..PipelineOptions::default()
    };
    let ir = compile_grammar_impl(grammar, &options)
        .map_err(|e| JsValue::from_str(&e))?;

    let program = compile_bytecode(&ir);
    let handle = NEXT_HANDLE.fetch_add(1, Ordering::Relaxed);

    with_store(|store| {
        store.insert(handle, CompiledGrammar { ir, program });
    });

    Ok(handle)
}

// ── Parse ───────────────────────────────────────────────────────────────────

/// A single parse diagnostic surfaced from the bytecode interpreter's FOLLOW sets.
#[derive(Serialize)]
struct WasmParseDiagnostic {
    rule_name: Option<String>,
    offset: u32,
    /// Pre-formatted message, e.g. "expected one of: 'a', 'b', '\\n'".
    expected: String,
}

#[derive(Serialize)]
struct WasmParseResult {
    success: bool,
    offset: u32,
    value: Option<WasmValue>,
    diagnostics: Vec<WasmParseDiagnostic>,
}

#[derive(Serialize)]
#[serde(tag = "type")]
enum WasmValue {
    Span { start: u32, end: u32 },
    Tagged { tag: String, start: u32, end: u32, children: Vec<WasmValue> },
    Array { items: Vec<WasmValue> },
    Nil,
}

fn value_to_wasm(value: &Value, strings: &[String]) -> WasmValue {
    match value {
        Value::Span(s, e) => WasmValue::Span { start: *s, end: *e },
        Value::Tagged { tag, span, children } => WasmValue::Tagged {
            tag: strings.get(*tag as usize).cloned().unwrap_or_default(),
            start: span.0,
            end: span.1,
            children: children.iter().map(|c| value_to_wasm(c, strings)).collect(),
        },
        Value::Array(items) => WasmValue::Array {
            items: items.iter().map(|v| value_to_wasm(v, strings)).collect(),
        },
        Value::Nil => WasmValue::Nil,
    }
}

/// Parse input using a previously compiled grammar.
/// Returns a JSON-serializable parse result.
#[wasm_bindgen]
pub fn parse_with_grammar(handle: u32, input: &str) -> Result<JsValue, JsValue> {
    with_store(|store| {
        let grammar = store
            .get(&handle)
            .ok_or_else(|| JsValue::from_str("Invalid grammar handle"))?;

        let mut interp = Interpreter::new(&grammar.program, input);
        let result = interp.run();

        let diagnostics: Vec<WasmParseDiagnostic> = result
            .diagnostics
            .iter()
            .map(|d| {
                let expected = if d.expected.is_empty() {
                    String::new()
                } else {
                    let chars: Vec<String> = d
                        .expected
                        .iter()
                        .map(|&b| {
                            if b == b'\n' {
                                "\\n".to_string()
                            } else if b == b'\t' {
                                "\\t".to_string()
                            } else if b == b'\r' {
                                "\\r".to_string()
                            } else if b.is_ascii_graphic() || b == b' ' {
                                format!("'{}'", b as char)
                            } else {
                                format!("0x{:02x}", b)
                            }
                        })
                        .collect();
                    format!("expected one of: {}", chars.join(", "))
                };
                WasmParseDiagnostic {
                    rule_name: d.rule_name.clone(),
                    offset: d.offset,
                    expected,
                }
            })
            .collect();

        let wasm_result = WasmParseResult {
            success: result.success,
            offset: result.offset,
            value: result.value.as_ref().map(|v| value_to_wasm(v, &grammar.program.strings)),
            diagnostics,
        };

        serde_wasm_bindgen::to_value(&wasm_result).map_err(|e| JsValue::from_str(&e.to_string()))
    })
}

/// Parse input, returning only success and offset — no tree serialization.
/// Use this when you only need to validate or measure raw parse throughput.
#[wasm_bindgen]
pub fn parse_check(handle: u32, input: &str) -> Result<JsValue, JsValue> {
    with_store(|store| {
        let grammar = store
            .get(&handle)
            .ok_or_else(|| JsValue::from_str("Invalid grammar handle"))?;

        let mut interp = Interpreter::new(&grammar.program, input);
        let result = interp.run();

        // Only serialize success + offset — skip the entire value tree.
        let obj = js_sys::Object::new();
        js_sys::Reflect::set(&obj, &"success".into(), &result.success.into())?;
        js_sys::Reflect::set(&obj, &"offset".into(), &result.offset.into())?;
        Ok(obj.into())
    })
}

// ── Format ──────────────────────────────────────────────────────────────────

/// Format input using a previously compiled grammar's @pretty hints.
/// Returns the formatted string, or null if parsing fails or no pretty hints are defined.
#[wasm_bindgen]
pub fn format_with_grammar(
    handle: u32,
    input: &str,
    max_width: u32,
    indent: u32,
    use_tabs: bool,
) -> Option<String> {
    with_store(|store| {
        let grammar = store.get(&handle)?;

        // Parse input using the bytecode VM.
        let mut interp = Interpreter::new(&grammar.program, input);
        let result = interp.run();

        let value = result.value.as_ref().filter(|_| result.success)?;

        // Format using the IR's pretty hints.
        let printer = pprint::Printer::new(max_width as usize, indent as usize, use_tabs);
        gorgeous::vm::format_value(&grammar.ir, value, input, printer)
    })
}

// ── Free ────────────────────────────────────────────────────────────────────

/// Free a compiled grammar, releasing its memory.
#[wasm_bindgen]
pub fn free_grammar(handle: u32) {
    with_store(|store| {
        store.remove(&handle);
    });
}
