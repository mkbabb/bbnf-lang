//! AX.W0a.2.b — diagnostic BFS probe (one-off).
//!
//! Walks the entry-reachable Ref graph for each grammar collecting the
//! full set of entry-reachable unclassified Refs. Output dumped to
//! stdout; orchestrator captures to
//! `docs/benchmarks/post-AX-W0a2b-refs.md`.

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

/// BFS from entry through classified rule bodies; collect every
/// (parent_rule_name, child_rule_name) pair where parent is classified
/// but child is unclassified.
fn entry_unclassified_refs(ir: &GrammarIR) -> Vec<(String, String, ShapeTag)> {
    let mut out: Vec<(String, String, ShapeTag)> = Vec::new();
    let mut visited: std::collections::HashSet<bbnf_ir::RuleId> = Default::default();
    let mut stack: Vec<bbnf_ir::RuleId> = vec![ir.entry];
    visited.insert(ir.entry);

    while let Some(rid) = stack.pop() {
        let Some(rule) = ir.rules.iter().find(|r| r.id == rid) else {
            continue;
        };
        // Transparent rules: the emitter skips, but the reachability
        // walk must traverse their Refs.
        let parent_name = ir.get_string(rule.name).to_string();
        // Skip BFS-walking from an unclassified entry — the gate
        // already rejects. But if the entry is Alt-of-Refs (JSON) the
        // entry is transparent and its Refs are the true entry points.
        let parent_tag = ir.shape_assignments.get(rid);
        let is_alt_root = rid == ir.entry
            && matches!(&rule.body, IrNode::Alt(_, _));
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
        println!("| Parent | Target | Target body shape |");
        println!("|---|---|---|");
        let mut seen_targets: std::collections::BTreeSet<&str> = Default::default();
        for (parent, target, _tag) in &refs {
            // Inspect target body to label its structural shape.
            let target_rule = ir.rules.iter().find(|r| ir.get_string(r.name) == *target).unwrap();
            let shape = describe_body(&target_rule.body);
            println!("| `{parent}` | `{target}` | {shape} |");
            seen_targets.insert(target.as_str());
        }
        // For each unique target, dump per-branch classification detail.
        println!();
        println!("### Per-target branch classifications");
        println!();
        for target in &seen_targets {
            let target_rule = ir.rules.iter().find(|r| ir.get_string(r.name) == *target).unwrap();
            let body_stripped = match &target_rule.body {
                IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => inner.as_ref(),
                other => other,
            };
            if let IrNode::Alt(branches, _) = body_stripped {
                println!("**`{target}`** (Alt, {} branches):", branches.len());
                for (i, b) in branches.iter().enumerate() {
                    let detail = describe_branch(&b.node, &ir);
                    println!("  - [{i}] {detail}");
                }
                println!();
            }
        }
    }
    println!();
}

fn describe_branch(node: &IrNode, ir: &GrammarIR) -> String {
    match node {
        IrNode::Ref(rid) => {
            let target = &ir.rules[*rid as usize];
            let name = ir.get_string(target.name);
            let tag = ir.shape_assignments.get(*rid);
            format!("Ref({name}) → {tag:?}")
        }
        IrNode::Map { inner, .. } => format!("Map({})", describe_branch(inner, ir)),
        IrNode::OptionalWhitespace(inner) => format!("OW({})", describe_branch(inner, ir)),
        IrNode::Literal(_) => "Literal".to_string(),
        IrNode::Regex(_) => "Regex".to_string(),
        IrNode::Seq(children) => format!("Seq({})", children.len()),
        IrNode::Alt(bs, _) => format!("Alt({})", bs.len()),
        IrNode::Next(..) => "Next(..)".to_string(),
        IrNode::Skip(..) => "Skip(..)".to_string(),
        IrNode::Repeat { .. } => "Repeat(..)".to_string(),
        _ => "Other".to_string(),
    }
}

/// A one-line structural description of `node` for the report.
fn describe_body(node: &IrNode) -> String {
    match node {
        IrNode::Alt(bs, _) => {
            let branch_kinds: Vec<&str> = bs
                .iter()
                .map(|b| match &b.node {
                    IrNode::Ref(_) => "Ref",
                    IrNode::Regex(_) => "Regex",
                    IrNode::Literal(_) => "Literal",
                    IrNode::Seq(_) => "Seq",
                    IrNode::Alt(_, _) => "Alt",
                    _ => "Other",
                })
                .collect();
            format!("Alt({})", branch_kinds.join(" \\| "))
        }
        IrNode::Seq(children) => format!("Seq(len={})", children.len()),
        IrNode::Next(..) => "Next(..)".to_string(),
        IrNode::Skip(..) => "Skip(..)".to_string(),
        IrNode::Repeat { lo, hi, .. } => format!("Repeat({lo},{hi})"),
        IrNode::Ref(_) => "Ref".to_string(),
        IrNode::Regex(_) => "Regex".to_string(),
        IrNode::Literal(_) => "Literal".to_string(),
        IrNode::Map { inner, .. } => format!("Map({})", describe_body(inner)),
        IrNode::OptionalWhitespace(inner) => format!("OW({})", describe_body(inner)),
        IrNode::Epsilon => "Epsilon".to_string(),
        IrNode::Minus(..) => "Minus(..)".to_string(),
        IrNode::Negate(_) => "Negate(..)".to_string(),
        IrNode::TokenDispatch { .. } => "TokenDispatch".to_string(),
    }
}

#[test]
fn probe_all_grammars() {
    println!("# AX.W0a.2.b — entry-reachable unclassified Refs");
    println!();
    dump("JSON", "../../grammar/json/json.bbnf", false);
    dump("CSS L4", "../../grammar/css/l4/stylesheet.bbnf", false);
    dump("Sheets", "../../grammar/google-sheets/google-sheets.bbnf", false);
    dump("BBNF", "../../grammar/bbnf/bbnf.bbnf", false);
    dump("EBNF", "../../grammar/ebnf/ebnf.bbnf", false);
    dump("BNF", "../../grammar/bnf/bnf.bbnf", false);
    dump("BbnfBootstrap", "../../grammar/bbnf/bbnf.bbnf", true);
}

#[test]
fn dump_css_alignDecl() {
    let ir = compile("../../grammar/css/l4/stylesheet.bbnf", false);
    // Dump a few unclassified rules' bodies in detail.
    for name in ["alignDecl", "flexNumDecl", "typeSelector"] {
        if let Some(rule) = ir.rules.iter().find(|r| ir.get_string(r.name) == name) {
            let tag = ir.shape_assignments.get(rule.id);
            println!("### `{name}` tag={tag:?}");
            println!("body = {:#?}", &rule.body);
            println!();
        }
    }
}

#[test]
fn dump_has_shape_dispatcher_entrypoint_outcome() {
    use bbnf::backend::rust::emitter::shapes::has_shape_dispatcher_entrypoint;
    for (label, path, structural) in [
        ("JSON", "../../grammar/json/json.bbnf", false),
        ("CSS L4", "../../grammar/css/l4/stylesheet.bbnf", false),
        ("Sheets", "../../grammar/google-sheets/google-sheets.bbnf", false),
        ("BBNF", "../../grammar/bbnf/bbnf.bbnf", false),
        ("EBNF", "../../grammar/ebnf/ebnf.bbnf", false),
        ("BNF", "../../grammar/bnf/bnf.bbnf", false),
        ("BbnfBootstrap", "../../grammar/bbnf/bbnf.bbnf", true),
    ] {
        let ir = compile(path, structural);
        let out = has_shape_dispatcher_entrypoint(&ir);
        println!("  {label:16} has_shape_dispatcher_entrypoint = {out}");
    }
}

#[test]
fn dump_remaining_unclassified() {
    for (label, path, structural) in [
        ("Sheets", "../../grammar/google-sheets/google-sheets.bbnf", false),
        ("BNF", "../../grammar/bnf/bnf.bbnf", false),
    ] {
        let ir = compile(path, structural);
        println!("### {label}");
        for name in ["let_args", "alternation", "expression", "term", "let_binding", "let_call"] {
            if let Some(rule) = ir.rules.iter().find(|r| ir.get_string(r.name) == name) {
                let tag = ir.shape_assignments.get(rule.id);
                println!("  `{name}` tag={tag:?}");
                let body_stripped = match &rule.body {
                    IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                        inner.as_ref()
                    }
                    other => other,
                };
                let brief = describe_body(body_stripped);
                println!("    body = {brief}");
                // If Seq, dump child shapes.
                if let IrNode::Seq(children) = body_stripped {
                    for (i, c) in children.iter().enumerate() {
                        let cs = match c {
                            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                                inner.as_ref()
                            }
                            other => other,
                        };
                        println!("    pos {i}: {}", describe_branch(cs, &ir));
                    }
                }
            }
        }
        println!();
    }
}

#[test]
fn dump_bbnf_value_atom() {
    let ir = compile("../../grammar/bbnf/bbnf.bbnf", false);
    for name in ["value_atom", "value_unary", "string_lit", "lhs", "alternation", "let_args"] {
        if let Some(rule) = ir.rules.iter().find(|r| ir.get_string(r.name) == name) {
            let tag = ir.shape_assignments.get(rule.id);
            println!("### `{name}` tag={tag:?}");
            println!("body first-byte set = {:?}", rule.meta.first_set);
            // Dump a short body summary without the full tree.
            let body_stripped = match &rule.body {
                IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                    inner.as_ref()
                }
                other => other,
            };
            match body_stripped {
                IrNode::Alt(bs, _) => {
                    for (i, b) in bs.iter().enumerate() {
                        let stripped = match &b.node {
                            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                                inner.as_ref()
                            }
                            other => other,
                        };
                        println!("  branch {i}: {}", describe_branch(stripped, &ir));
                    }
                }
                IrNode::Seq(c) => {
                    for (i, child) in c.iter().enumerate() {
                        let stripped = match child {
                            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                                inner.as_ref()
                            }
                            other => other,
                        };
                        println!("  pos {i}: {}", describe_branch(stripped, &ir));
                    }
                }
                other => println!("  body = {}", describe_branch(other, &ir)),
            }
            println!();
        }
    }
}
