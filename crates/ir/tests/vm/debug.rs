//! Tests for bytecode debug infrastructure: DebugBreak opcodes, source map,
//! and interpreter debug hooks.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use bbnf_ir::bytecode::Op;
use bbnf_ir::compiler::{compile, compile_with_debug};
use bbnf_ir::interpreter::{DebugAction, DebugState, Interpreter, StepMode};
use bbnf_ir::{GrammarIR, GrammarSpan, IrNode, IrRule, RuleDirectives, RuleMeta};

fn make_debug_ir() -> GrammarIR {
    // Grammar: entry = value ; value = "x" ;
    GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta {
                    directives: RuleDirectives {
                        debug: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                source_span: Some(GrammarSpan { start: 0, end: 10 }),
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta {
                    directives: RuleDirectives {
                        debug: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                source_span: Some(GrammarSpan { start: 12, end: 25 }),
            },
        ],
        entry: 0,
        strings: vec!["entry".into(), "value".into(), "x".into()],
        fns: Vec::new(),
        types: Vec::new(),
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
    }
}

// ── DebugBreak opcodes ───────────────────────────────────────────────────────

#[test]
fn compiler_emits_debug_break_for_debug_rules() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    let debug_breaks: Vec<_> = program
        .code
        .iter()
        .filter(|op| matches!(op, Op::DebugBreak { .. }))
        .collect();

    // 2 rules × 2 (entry + exit) = 4 DebugBreak opcodes.
    assert_eq!(
        debug_breaks.len(),
        4,
        "should emit 4 DebugBreak opcodes (2 rules × entry+exit)"
    );
}

#[test]
fn compiler_no_debug_break_without_flag() {
    let ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Literal(1),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["entry".into(), "x".into()],
        fns: Vec::new(),
        types: Vec::new(),
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
    };
    let program = compile(&ir);

    let debug_breaks: Vec<_> = program
        .code
        .iter()
        .filter(|op| matches!(op, Op::DebugBreak { .. }))
        .collect();
    assert_eq!(
        debug_breaks.len(),
        0,
        "should have no DebugBreak without debug flag"
    );
}

#[test]
fn compiler_debug_all_instruments_all_rules() {
    let mut ir = make_debug_ir();
    ir.debug_all = true;
    for rule in &mut ir.rules {
        rule.meta.directives.debug = false; // Clear per-rule, rely on debug_all.
    }
    let program = compile_with_debug(&ir, true);

    let debug_breaks: Vec<_> = program
        .code
        .iter()
        .filter(|op| matches!(op, Op::DebugBreak { .. }))
        .collect();
    assert_eq!(
        debug_breaks.len(),
        4,
        "debug_all should instrument all rules"
    );
}

// ── Source map ───────────────────────────────────────────────────────────────

#[test]
fn source_map_populated_with_debug() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);
    assert_eq!(
        program.source_map.len(),
        2,
        "source map should have one entry per rule"
    );
    assert_eq!(program.source_map[0].rule_id, 0);
    assert_eq!(program.source_map[0].span.start, 0);
    assert_eq!(program.source_map[0].span.end, 10);
    assert_eq!(program.source_map[1].rule_id, 1);
}

#[test]
fn source_map_empty_without_debug() {
    let ir = make_debug_ir();
    let program = compile(&ir);
    assert!(
        program.source_map.is_empty(),
        "source map should be empty without debug"
    );
}

// ── Interpreter debug hooks ──────────────────────────────────────────────────

#[test]
fn interpreter_debug_break_fires_on_breakpoint() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    let break_count = Arc::new(AtomicUsize::new(0));
    let count_clone = break_count.clone();

    let mut interp = Interpreter::new(&program, "x");
    interp.debug_state = Some(DebugState {
        breakpoints: {
            let mut set = HashSet::new();
            set.insert(1); // Breakpoint on rule 1 ("value").
            set
        },
        step_mode: StepMode::Continue,
        trace: Vec::new(),
        on_break: Box::new(move |snapshot| {
            count_clone.fetch_add(1, Ordering::Relaxed);
            assert_eq!(snapshot.rule_id, 1, "should break on rule 1");
            DebugAction::Continue
        }),
    });

    let result = interp.run();
    assert!(result.success, "parse should succeed");
    // Rule 1 has entry + exit DebugBreak → on_break fires twice.
    assert_eq!(break_count.load(Ordering::Relaxed), 2);
}

#[test]
fn interpreter_step_rule_stops_at_every_debug_break() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    let break_count = Arc::new(AtomicUsize::new(0));
    let count_clone = break_count.clone();

    let mut interp = Interpreter::new(&program, "x");
    interp.debug_state = Some(DebugState {
        breakpoints: HashSet::new(),
        step_mode: StepMode::StepRule,
        trace: Vec::new(),
        on_break: Box::new(move |_| {
            count_clone.fetch_add(1, Ordering::Relaxed);
            DebugAction::StepRule // Keep stepping.
        }),
    });

    let result = interp.run();
    assert!(result.success);
    // 2 rules × (entry + exit) = 4 breaks.
    assert_eq!(break_count.load(Ordering::Relaxed), 4);
}

#[test]
fn interpreter_debug_stop_halts_execution() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    let mut interp = Interpreter::new(&program, "x");
    interp.debug_state = Some(DebugState {
        breakpoints: HashSet::new(),
        step_mode: StepMode::StepRule,
        trace: Vec::new(),
        on_break: Box::new(|_| DebugAction::Stop),
    });

    let result = interp.run();
    // Stop should halt execution before completing the parse.
    assert!(!result.success, "parse should fail when stopped early");
}

#[test]
fn interpreter_trace_records_entries() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    let mut interp = Interpreter::new(&program, "x");
    interp.debug_state = Some(DebugState {
        breakpoints: HashSet::new(),
        step_mode: StepMode::Continue, // Don't stop, just record.
        trace: Vec::new(),
        on_break: Box::new(|_| DebugAction::Continue),
    });

    let result = interp.run();
    assert!(result.success);

    let trace = &interp.debug_state.as_ref().unwrap().trace;
    assert_eq!(trace.len(), 4, "should record 4 trace entries");
    // First entry: rule 0 (entry), is_entry=true.
    assert_eq!(trace[0].rule_id, 0);
    assert!(trace[0].is_entry);
    // Second: rule 1 entry.
    assert_eq!(trace[1].rule_id, 1);
    assert!(trace[1].is_entry);
    // Third: rule 1 exit.
    assert_eq!(trace[2].rule_id, 1);
    assert!(!trace[2].is_entry);
    // Fourth: rule 0 exit.
    assert_eq!(trace[3].rule_id, 0);
    assert!(!trace[3].is_entry);
}

#[test]
fn interpreter_no_debug_state_no_overhead() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    // No debug_state — DebugBreak is a no-op (just pc += 1).
    let mut interp = Interpreter::new(&program, "x");
    let result = interp.run();
    assert!(
        result.success,
        "should parse successfully with no debug state"
    );
}

#[test]
fn interpreter_debug_snapshot_has_rule_stack() {
    let ir = make_debug_ir();
    let program = compile_with_debug(&ir, true);

    let captured_stack_len = Arc::new(AtomicUsize::new(0));
    let len_clone = captured_stack_len.clone();

    let mut interp = Interpreter::new(&program, "x");
    interp.debug_state = Some(DebugState {
        breakpoints: {
            let mut set = HashSet::new();
            set.insert(1); // Break on inner rule.
            set
        },
        step_mode: StepMode::Continue,
        trace: Vec::new(),
        on_break: Box::new(move |snapshot| {
            if snapshot.is_entry && snapshot.rule_id == 1 {
                len_clone.store(snapshot.rule_stack.len(), Ordering::Relaxed);
            }
            DebugAction::Continue
        }),
    });

    let result = interp.run();
    assert!(result.success);
    // When rule 1 is entered via Call, it's pushed onto rule_stack.
    // Rule 0 (entry) is entered directly (no Call), so only rule 1 is on the stack.
    let stack_len = captured_stack_len.load(Ordering::Relaxed);
    assert!(
        stack_len >= 1,
        "rule_stack should have >=1 entries at inner rule entry, got {}",
        stack_len
    );
}
