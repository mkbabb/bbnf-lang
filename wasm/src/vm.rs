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
use bbnf_ir::compiler::{compile as compile_bytecode, compile_with_debug};
use bbnf_ir::interpreter::{DebugAction, DebugState, Interpreter, StepMode, Value};
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

/// Convert a parse tree Value directly to JsValue, skipping intermediate allocations.
fn value_to_js(value: &Value, strings: &[String]) -> JsValue {
    match value {
        Value::Span(s, e) => {
            let obj = js_sys::Object::new();
            let _ = js_sys::Reflect::set(&obj, &"type".into(), &"Span".into());
            let _ = js_sys::Reflect::set(&obj, &"start".into(), &(*s).into());
            let _ = js_sys::Reflect::set(&obj, &"end".into(), &(*e).into());
            obj.into()
        }
        Value::Tagged { tag, span, children } => {
            let obj = js_sys::Object::new();
            let _ = js_sys::Reflect::set(&obj, &"type".into(), &"Tagged".into());
            let tag_name = strings.get(*tag as usize).map(|s| s.as_str()).unwrap_or("");
            let _ = js_sys::Reflect::set(&obj, &"tag".into(), &tag_name.into());
            let _ = js_sys::Reflect::set(&obj, &"start".into(), &span.0.into());
            let _ = js_sys::Reflect::set(&obj, &"end".into(), &span.1.into());
            let arr = js_sys::Array::new_with_length(children.len() as u32);
            for (i, child) in children.iter().enumerate() {
                arr.set(i as u32, value_to_js(child, strings));
            }
            let _ = js_sys::Reflect::set(&obj, &"children".into(), &arr);
            obj.into()
        }
        Value::Array(items) => {
            let obj = js_sys::Object::new();
            let _ = js_sys::Reflect::set(&obj, &"type".into(), &"Array".into());
            let arr = js_sys::Array::new_with_length(items.len() as u32);
            for (i, item) in items.iter().enumerate() {
                arr.set(i as u32, value_to_js(item, strings));
            }
            let _ = js_sys::Reflect::set(&obj, &"items".into(), &arr);
            obj.into()
        }
        Value::Nil => {
            let obj = js_sys::Object::new();
            let _ = js_sys::Reflect::set(&obj, &"type".into(), &"Nil".into());
            obj.into()
        }
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

        // Build JsValue directly — no intermediate WasmValue allocation.
        let obj = js_sys::Object::new();
        js_sys::Reflect::set(&obj, &"success".into(), &result.success.into())?;
        js_sys::Reflect::set(&obj, &"offset".into(), &result.offset.into())?;

        if let Some(ref value) = result.value {
            js_sys::Reflect::set(&obj, &"value".into(), &value_to_js(value, &grammar.program.strings))?;
        } else {
            js_sys::Reflect::set(&obj, &"value".into(), &JsValue::NULL)?;
        }

        let diag_arr = js_sys::Array::new_with_length(diagnostics.len() as u32);
        for (i, d) in diagnostics.iter().enumerate() {
            let dobj = js_sys::Object::new();
            if let Some(ref name) = d.rule_name {
                js_sys::Reflect::set(&dobj, &"rule_name".into(), &name.into())?;
            }
            js_sys::Reflect::set(&dobj, &"offset".into(), &d.offset.into())?;
            js_sys::Reflect::set(&dobj, &"expected".into(), &d.expected.clone().into())?;
            diag_arr.set(i as u32, dobj.into());
        }
        js_sys::Reflect::set(&obj, &"diagnostics".into(), &diag_arr)?;

        Ok(obj.into())
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

// ── Debug ────────────────────────────────────────────────────────────────────

/// Compile a grammar with debug instrumentation (source map + DebugBreak opcodes).
/// Returns a handle usable with `debug_step` and `debug_get_state`.
#[wasm_bindgen]
pub fn compile_grammar_debug(grammar: &str, entry_rule: Option<String>) -> Result<u32, JsValue> {
    let options = PipelineOptions {
        entry_rule: entry_rule.filter(|s| !s.is_empty()),
        ..PipelineOptions::default()
    };
    let mut ir = compile_grammar_impl(grammar, &options)
        .map_err(|e| JsValue::from_str(&e))?;

    // Force all rules to be debug-instrumented for the playground debugger,
    // regardless of @debug directives in the grammar source.
    ir.debug_all = true;
    for rule in &mut ir.rules {
        rule.meta.directives.debug = true;
    }

    let program = compile_with_debug(&ir, true);
    let handle = NEXT_HANDLE.fetch_add(1, Ordering::Relaxed);

    with_store(|store| {
        store.insert(handle, CompiledGrammar { ir, program });
    });

    Ok(handle)
}

/// Debug-step: run the interpreter with the given step mode until it stops.
///
/// `mode` is one of: `"continue"`, `"stepRule"`, `"stepNode"`, `"stepInstruction"`.
/// `breakpoint_rules` is a JSON array of rule names to break on.
///
/// Returns a JSON object with the debug snapshot or completion status.
#[wasm_bindgen]
pub fn debug_step(
    handle: u32,
    input: &str,
    mode: &str,
    breakpoint_rules: &str,
) -> Result<JsValue, JsValue> {
    use std::collections::HashSet;

    // The step index tracks how many debug breaks to skip before stopping.
    // Each call to debug_step increments it, so the interpreter replays to the
    // correct position (deterministic re-execution model).
    thread_local! {
        static STEP_INDEX: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    }

    // "continue" from completed → reset to beginning.
    // Passing mode "reset" also resets.
    if mode == "reset" {
        STEP_INDEX.with(|c| c.set(0));
        return serde_wasm_bindgen::to_value(&WasmDebugSnapshot {
            stopped: false,
            rule_name: String::new(),
            rule_stack: Vec::new(),
            offset: 0,
            is_entry: false,
            is_error: false,
            completed: false,
        }).map_err(|e| JsValue::from_str(&e.to_string()));
    }

    let current_step = STEP_INDEX.with(|c| c.get());

    with_store(|store| {
        let grammar = store
            .get(&handle)
            .ok_or_else(|| JsValue::from_str("Invalid grammar handle"))?;

        let step_mode = match mode {
            "stepRule" => StepMode::StepRule,
            "stepNode" => StepMode::StepNode,
            "stepInstruction" => StepMode::StepInstruction,
            _ => StepMode::Continue,
        };

        // Parse breakpoint rule names → RuleIds.
        let bp_names: Vec<String> = serde_json::from_str(breakpoint_rules).unwrap_or_default();
        let mut breakpoints: HashSet<u32> = HashSet::new();
        for name in &bp_names {
            if let Some(rule) = grammar.ir.find_rule(name) {
                breakpoints.insert(rule.id);
            }
        }

        // Re-execute from scratch, skipping `current_step` breaks before stopping.
        let mut breaks_hit = 0usize;
        let target_break = current_step + 1; // Stop at the NEXT break after current position.

        let mut interp = Interpreter::new(&grammar.program, input);
        interp.debug_state = Some(DebugState {
            breakpoints: breakpoints.clone(),
            step_mode: step_mode.clone(),
            trace: Vec::new(),
            on_break: Box::new(move |_snap| {
                breaks_hit += 1;
                if breaks_hit >= target_break {
                    DebugAction::Stop
                } else {
                    // Skip this break — continue with the same step mode.
                    match step_mode {
                        StepMode::StepRule => DebugAction::StepRule,
                        StepMode::StepNode => DebugAction::StepNode,
                        StepMode::StepInstruction => DebugAction::StepInstruction,
                        StepMode::Continue => DebugAction::Continue,
                    }
                }
            }),
        });

        let result = interp.run();

        // Extract snapshot from the trace.
        let mut snapshot_data: Option<WasmDebugSnapshot> = None;
        if let Some(ref dbg) = interp.debug_state {
            if let Some(last) = dbg.trace.last() {
                let rule_name = grammar
                    .program
                    .rule_names
                    .get(last.rule_id as usize)
                    .and_then(|&sid| grammar.program.strings.get(sid as usize))
                    .cloned()
                    .unwrap_or_default();

                let rule_stack: Vec<String> = interp
                    .rule_stack_snapshot()
                    .iter()
                    .filter_map(|&rid| {
                        grammar
                            .program
                            .rule_names
                            .get(rid as usize)
                            .and_then(|&sid| grammar.program.strings.get(sid as usize))
                            .cloned()
                    })
                    .collect();

                // Did we actually stop, or did we exhaust all breaks?
                let trace_len = dbg.trace.len();
                let actually_stopped = trace_len >= target_break;

                if actually_stopped {
                    STEP_INDEX.with(|c| c.set(current_step + 1));
                    snapshot_data = Some(WasmDebugSnapshot {
                        stopped: true,
                        rule_name,
                        rule_stack,
                        offset: last.offset,
                        is_entry: last.is_entry,
                        is_error: false,
                        completed: false,
                    });
                }
            }
        }

        let snapshot = snapshot_data.unwrap_or_else(|| {
            // Parse completed without hitting the target break.
            STEP_INDEX.with(|c| c.set(0)); // Reset for next session.
            WasmDebugSnapshot {
                stopped: false,
                rule_name: String::new(),
                rule_stack: Vec::new(),
                offset: result.offset,
                is_entry: false,
                is_error: !result.success,
                completed: true,
            }
        });

        serde_wasm_bindgen::to_value(&snapshot).map_err(|e| JsValue::from_str(&e.to_string()))
    })
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct WasmDebugSnapshot {
    stopped: bool,
    rule_name: String,
    rule_stack: Vec<String>,
    offset: u32,
    is_entry: bool,
    is_error: bool,
    completed: bool,
}
