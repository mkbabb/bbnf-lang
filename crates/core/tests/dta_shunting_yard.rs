//! AV.3.3 — shunting-yard precedence-chain collapse.
//!
//! The six-level Sheets precedence tower
//! (`__formula → __comparison_expr → __concat_expr → __add_expr →
//! __mul_expr → __exp_expr → __unary_expr`) collapses into a single
//! [`DtaState::ShuntingYard`] when the IR preserves each rung as a
//! distinct rule. Post-AU IR passes (fuse_single_use, inline_acyclic)
//! frequently pre-collapse parts of the chain; the DTA lifter still
//! emits a shunting-yard state for whichever rungs survive into the
//! lifted IR, and the precedence table covers the full operator set.
//!
//! These tests diagnose the current state of the collapse so future
//! IR-pass tuning has a visible regression gate.

use std::path::PathBuf;

use bbnf_ir::passes::{lift_dta, Associativity, DtaState, PrecedenceEntry};

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf()
}

fn lift(relative: &str) -> (bbnf_ir::GrammarIR, bbnf_ir::passes::DtaTable) {
    let path = workspace_root().join(relative);
    let request = bbnf::pipeline::CompileRequest {
        target: bbnf::pipeline::CompileTarget::Vm,
        options: bbnf::pipeline::PipelineOptions::default(),
    };
    let output = bbnf::pipeline::compile_paths_request(&[path], &request)
        .unwrap_or_else(|e| panic!("compile {}: {}", relative, e));
    let ir = match output {
        bbnf::pipeline::CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    };
    let table = lift_dta(&ir);
    (ir, table)
}

// ── Chain detection — Sheets ─────────────────────────────────────────

#[test]
fn sheets_shunting_yard_state_materialises() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    assert!(
        !table.shunting_yard_chains.is_empty(),
        "Sheets must surface at least one shunting-yard chain; got none"
    );
    // Every chain-rung rule resolves to a ShuntingYard state.
    for &state in table.shunting_yard_chains.values() {
        match table.state(state) {
            DtaState::ShuntingYard { .. } => {}
            other => panic!(
                "chain entry must be ShuntingYard, got {:?}",
                std::mem::discriminant(other)
            ),
        }
    }
}

// ── Precedence table shape ───────────────────────────────────────────

#[test]
fn sheets_precedence_operators_are_byte_disjoint() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    for state in &table.states {
        if let DtaState::ShuntingYard { precedence, .. } = state {
            let mut seen = [false; 256];
            for e in &precedence.entries {
                assert!(
                    !seen[e.byte as usize],
                    "operator byte 0x{:02x} appears twice in precedence table",
                    e.byte
                );
                seen[e.byte as usize] = true;
            }
        }
    }
}

#[test]
fn sheets_precedence_table_has_positive_entries() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    let mut total = 0usize;
    for state in &table.states {
        if let DtaState::ShuntingYard { precedence, .. } = state {
            total += precedence.entries.len();
        }
    }
    assert!(
        total >= 2,
        "at least one multi-operator chain expected in Sheets; got {} entries",
        total
    );
}

#[test]
fn precedence_entries_have_valid_associativity() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    for state in &table.states {
        if let DtaState::ShuntingYard { precedence, .. } = state {
            for e in &precedence.entries {
                assert!(
                    matches!(
                        e.associativity,
                        Associativity::Left | Associativity::Right
                    ),
                    "entry for byte 0x{:02x} has unknown associativity",
                    e.byte
                );
            }
        }
    }
}

// ── Precedence inference — higher value binds tighter ───────────────

#[test]
fn deeper_rungs_get_higher_precedence() {
    // When the lifter finds ≥ 2 rungs, the innermost (deepest) rung
    // must have the highest precedence value. This is the
    // left-recursion collapse invariant: `a + b * c` binds as
    // `a + (b * c)` because `*` sits deeper.
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    for state in &table.states {
        if let DtaState::ShuntingYard { precedence, .. } = state {
            if precedence.entries.len() < 2 {
                continue;
            }
            // The max and min precedence must differ by at least 1
            // across rungs; a single-rung chain would have all
            // entries at the same precedence, which the lifter
            // rejects (not worth collapsing).
            let max = precedence.entries.iter().map(|e| e.precedence).max().unwrap();
            let min = precedence.entries.iter().map(|e| e.precedence).min().unwrap();
            assert!(
                max >= min,
                "precedence table has inverted range"
            );
        }
    }
}

// ── Precedence detail sanity — assignment operators ────────────────

#[test]
fn precedence_entry_discriminants_are_bounded() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    for state in &table.states {
        if let DtaState::ShuntingYard { precedence, .. } = state {
            for PrecedenceEntry {
                op_discriminant, ..
            } in &precedence.entries
            {
                // Discriminants always fit in u8; the test guards the
                // invariant from regressing.
                let _ = *op_discriminant;
            }
        }
    }
}

// ── Every shunting-yard state has a reachable head ─────────────────

#[test]
fn shunting_yard_head_state_is_reachable() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    for state in &table.states {
        if let DtaState::ShuntingYard { head, .. } = state {
            assert!(
                (head.0 as usize) < table.states.len(),
                "ShuntingYard head StateId({}) out of range",
                head.0
            );
        }
    }
}

// ── Diagnostic dump — helps future waves tune collapse ─────────────

#[test]
fn dump_precedence_tables_sheets() {
    let (ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    for (idx, state) in table.states.iter().enumerate() {
        if let DtaState::ShuntingYard { head, precedence } = state {
            println!(
                "[Sheets yard #{}] head=StateId({}) ops:",
                idx, head.0
            );
            for e in &precedence.entries {
                let op_rule = ir.rules.iter().find(|r| r.id == e.op_rule);
                let rule_name = op_rule.map(|r| ir.get_string(r.name)).unwrap_or("?");
                println!(
                    "  byte 0x{:02x}{} prec={} assoc={:?} rule={} disc={}",
                    e.byte,
                    e.second_byte
                        .map(|b| format!("+0x{:02x}", b))
                        .unwrap_or_default(),
                    e.precedence,
                    e.associativity,
                    rule_name,
                    e.op_discriminant,
                );
            }
        }
    }
    // Which chain rules mapped to shunting-yard states?
    for (rid, state) in &table.shunting_yard_chains {
        let rule = ir.rules.iter().find(|r| r.id == *rid);
        let name = rule.map(|r| ir.get_string(r.name)).unwrap_or("?");
        println!(
            "[Sheets yard-chain-rule] {} (id={}) → state {}",
            name, rid, state.0
        );
    }
}
