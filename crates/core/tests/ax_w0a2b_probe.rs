//! AX.W0a.2.b — entry-reachable Ref classification probe.
//!
//! Walks every grammar's entry-reachable Ref graph and emits the
//! per-target classification state. Captures the zero-unclassified-
//! Refs outcome the detector widening wave produced; serves as the
//! verification artefact for hard gate 1 ("every diagnostic-listed
//! Ref is now classified").
//!
//! The output is deterministic; dumping to stdout via `--nocapture`
//! is the recipe the orchestrator uses to regenerate
//! `docs/benchmarks/post-AX-W0a2b-refs-after.md`.

use bbnf::backend::rust::emitter::shapes::dispatcher::collect_value_refs;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::{GrammarIR, IrNode};
use std::path::PathBuf;

fn compile(rel: &str, structural: bool) -> GrammarIR {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let p = PathBuf::from(manifest).join(rel);
    let options = PipelineOptions {
        structural,
        ..PipelineOptions::default()
    };
    let request = CompileRequest {
        options,
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&p), &request)
        .unwrap_or_else(|e| panic!("grammar {rel} compile: {e:?}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm for {rel}, got {other:?}"),
    }
}

/// BFS from entry through classified rule bodies; collect every
/// (parent, child, child_tag) triple where parent is classified but
/// child is unclassified.
fn entry_unclassified_refs(ir: &GrammarIR) -> Vec<(String, String, ShapeTag)> {
    let mut out: Vec<(String, String, ShapeTag)> = Vec::new();
    let mut visited: std::collections::HashSet<bbnf_ir::RuleId> = Default::default();
    let mut stack: Vec<bbnf_ir::RuleId> = vec![ir.entry];
    visited.insert(ir.entry);

    while let Some(rid) = stack.pop() {
        let Some(rule) = ir.rules.iter().find(|r| r.id == rid) else {
            continue;
        };
        let parent_name = ir.get_string(rule.name).to_string();
        let parent_tag = ir.shape_assignments.get(rid);
        let is_alt_root = rid == ir.entry && matches!(&rule.body, IrNode::Alt(_, _));
        if !parent_tag.is_classified() && !is_alt_root && rid != ir.entry {
            continue;
        }

        let refs = collect_value_refs(&rule.body);
        for target_rid in refs {
            let target = &ir.rules[target_rid as usize];
            let target_name = ir.get_string(target.name).to_string();
            let target_tag = ir.shape_assignments.get(target_rid);
            if !target_tag.is_classified() {
                out.push((parent_name.clone(), target_name.clone(), target_tag));
            } else if visited.insert(target_rid) {
                stack.push(target_rid);
            }
        }
    }
    out.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
    out.dedup_by(|a, b| a.0 == b.0 && a.1 == b.1);
    out
}

fn dump(label: &str, rel: &str, structural: bool) {
    let ir = compile(rel, structural);
    let entry_name = ir
        .rules
        .iter()
        .find(|r| r.id == ir.entry)
        .map(|r| ir.get_string(r.name).to_string())
        .unwrap_or_default();
    let refs = entry_unclassified_refs(&ir);
    println!("## {label}");
    println!();
    println!("Entry rule: `{entry_name}`");
    println!();
    println!("Entry-reachable unclassified Refs: **{}**", refs.len());
    println!();
    if refs.is_empty() {
        println!("_(none)_");
    } else {
        println!("| Parent | Target |");
        println!("|---|---|");
        for (parent, target, _tag) in &refs {
            println!("| `{parent}` | `{target}` |");
        }
    }
    println!();
}

#[test]
fn probe_all_grammars() {
    println!("# AX.W0a.2.b — entry-reachable unclassified Refs");
    println!();
    dump("JSON", "../../grammar/json/json.bbnf", false);
    dump("CSS L4", "../../grammar/css/l4/stylesheet.bbnf", false);
    dump(
        "Sheets",
        "../../grammar/google-sheets/google-sheets.bbnf",
        false,
    );
    dump("BBNF", "../../grammar/bbnf/bbnf.bbnf", false);
    dump("EBNF", "../../grammar/ebnf/ebnf.bbnf", false);
    dump("BNF", "../../grammar/bnf/bnf.bbnf", false);
    dump("BbnfBootstrap", "../../grammar/bbnf/bbnf.bbnf", true);
}

#[test]
fn all_grammars_have_zero_entry_reachable_unclassified_refs() {
    for (label, rel, structural) in [
        ("JSON", "../../grammar/json/json.bbnf", false),
        ("CSS L4", "../../grammar/css/l4/stylesheet.bbnf", false),
        (
            "Sheets",
            "../../grammar/google-sheets/google-sheets.bbnf",
            false,
        ),
        ("BBNF", "../../grammar/bbnf/bbnf.bbnf", false),
        ("EBNF", "../../grammar/ebnf/ebnf.bbnf", false),
        ("BNF", "../../grammar/bnf/bnf.bbnf", false),
        ("BbnfBootstrap", "../../grammar/bbnf/bbnf.bbnf", true),
    ] {
        let ir = compile(rel, structural);
        let refs = entry_unclassified_refs(&ir);
        assert!(
            refs.is_empty(),
            "{label}: {} entry-reachable unclassified Refs: {:?}",
            refs.len(),
            refs,
        );
    }
}
