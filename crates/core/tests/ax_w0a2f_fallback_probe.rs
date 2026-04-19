//! AX.W0a.2.f — fallback-position probe.
//!
//! For each grammar, enumerate the entry-reachable classified rules
//! whose bodies carry a dispatcher-fallback position (Alt / Regex /
//! Negate / Minus / TokenDispatch at an inline Seq-position). These
//! are the rules the emitter would currently emit with a
//! `#dispatcher_ident` call — a cross-shape recursive edge that the
//! inline-position emitter must replace before
//! `body_has_dispatcher_fallback_position` can be deleted.
//!
//! Supplement to `ax_w0a2b_probe`, which confirmed every entry-
//! reachable Ref is classified. The remaining admission blocker for
//! the 6 non-JSON grammars is the dispatcher-fallback predicate; this
//! probe isolates where the predicate fires so the remediation scope
//! is measurable.

use bbnf::backend::rust::emitter::shapes::dispatcher::collect_value_refs;
use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
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

/// Re-implements `body_has_dispatcher_fallback_position` (not `pub`) so
/// the probe can walk the same predicate the emitter gates on.
fn body_has_fallback(node: &IrNode) -> bool {
    match node {
        IrNode::Regex(_) | IrNode::Alt(_, _) | IrNode::Negate(_)
        | IrNode::Minus(_, _) | IrNode::TokenDispatch { .. } => true,
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            body_has_fallback(inner)
        }
        IrNode::Seq(children) => children.iter().any(body_has_fallback),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            body_has_fallback(lhs) || body_has_fallback(rhs)
        }
        IrNode::Repeat { inner, .. } => body_has_fallback(inner),
        IrNode::Ref(_) | IrNode::Literal(_) | IrNode::Epsilon => false,
    }
}

/// Return a brief discriminant tag for the outermost fallback-position
/// node kind inside the rule body.
fn fallback_kind(node: &IrNode) -> Option<&'static str> {
    match node {
        IrNode::Alt(_, _) => Some("Alt"),
        IrNode::Regex(_) => Some("Regex"),
        IrNode::Negate(_) => Some("Negate"),
        IrNode::Minus(_, _) => Some("Minus"),
        IrNode::TokenDispatch { .. } => Some("TokenDispatch"),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            fallback_kind(inner)
        }
        IrNode::Seq(children) => children.iter().find_map(|c| fallback_kind(c)),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            fallback_kind(lhs).or_else(|| fallback_kind(rhs))
        }
        IrNode::Repeat { inner, .. } => fallback_kind(inner),
        _ => None,
    }
}

fn dump(label: &str, rel: &str, structural: bool) {
    let ir = compile(rel, structural);
    let entry_name = ir
        .rules
        .iter()
        .find(|r| r.id == ir.entry)
        .map(|r| ir.get_string(r.name).to_string())
        .unwrap_or_default();

    // BFS from entry through classified bodies; record any classified
    // rule whose body carries a fallback position.
    let mut visited: std::collections::HashSet<bbnf_ir::RuleId> = Default::default();
    let mut stack: Vec<bbnf_ir::RuleId> = vec![ir.entry];
    visited.insert(ir.entry);
    let mut blockers: Vec<(String, &'static str, ShapeTag)> = Vec::new();

    while let Some(rid) = stack.pop() {
        let Some(rule) = ir.rules.iter().find(|r| r.id == rid) else {
            continue;
        };
        let parent_name = ir.get_string(rule.name).to_string();
        let parent_tag = ir.shape_assignments.get(rid);
        let is_entry = rid == ir.entry;
        if !parent_tag.is_classified() && !is_entry {
            continue;
        }
        if body_has_fallback(&rule.body) {
            let kind = fallback_kind(&rule.body).unwrap_or("?");
            blockers.push((parent_name.clone(), kind, parent_tag));
        }
        let refs = collect_value_refs(&rule.body);
        for target_rid in refs {
            if visited.insert(target_rid) {
                stack.push(target_rid);
            }
        }
    }
    blockers.sort_by(|a, b| a.0.cmp(&b.0));
    blockers.dedup_by(|a, b| a.0 == b.0);

    println!("## {label}");
    println!();
    println!("Entry rule: `{entry_name}`");
    println!();
    println!(
        "Entry-reachable classified rules with fallback-position body: **{}**",
        blockers.len()
    );
    println!();
    if blockers.is_empty() {
        println!("_(none)_");
    } else {
        // Partition by shape — shapes that handle their body natively
        // vs shapes that would emit `#dispatcher_ident` fallback.
        // Native-handling shapes: Wrap, Keyword, AltDispatch, HRegex,
        // Number, String, Scalar (Literal/Ref only).
        // Fallback-risk shapes: Flat, ArgList, Array, Pratt, Unordered,
        // Object — body positions may hit `#dispatcher_ident` when the
        // per-emitter extract_ref returns None.
        let native = |t: ShapeTag| matches!(
            t,
            ShapeTag::Wrap | ShapeTag::Keyword | ShapeTag::AltDispatch
                | ShapeTag::HRegex | ShapeTag::Number | ShapeTag::String
                | ShapeTag::Scalar
        );
        println!("| Rule | Outer fallback kind | ShapeTag | Native-handling |");
        println!("|---|---|---|---|");
        for (rule, kind, tag) in &blockers {
            let n = if native(*tag) { "yes" } else { "**RISK**" };
            println!("| `{rule}` | {kind} | {tag:?} | {n} |");
        }
    }
    println!();
}

#[test]
fn probe_all_grammars() {
    println!("# AX.W0a.2.f — entry-reachable dispatcher-fallback positions");
    println!();
    dump("JSON", "../../grammar/json/json.bbnf", false);
    dump("CSS L4", "../../grammar/css/l4/stylesheet.bbnf", false);
    dump("Sheets", "../../grammar/google-sheets/google-sheets.bbnf", false);
    dump("BBNF", "../../grammar/bbnf/bbnf.bbnf", false);
    dump("EBNF", "../../grammar/ebnf/ebnf.bbnf", false);
    dump("BNF", "../../grammar/bnf/bnf.bbnf", false);
    dump("BbnfBootstrap", "../../grammar/bbnf/bbnf.bbnf", true);
}
