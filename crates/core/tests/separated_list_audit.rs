//! LW2 ruling S1-B: `( x << sep ? ) *` reads literally (R-b: spec text
//! first, `docs/bbnf/operators.md` "Separated lists"). No emitter
//! rewrites it into a strict separated list, so a grammar that means a
//! strict list must write `x (sep x)*`.
//!
//! This audit lowers every tracked `.bbnf` outside `restart/` and
//! `docs/` (the `grammar/MANIFEST` set) and lists each rule whose IR
//! carries the literal form `Repeat { Skip(x, Repeat { sep, 0, 1 }),
//! hi > 1 }`. Each such use must be ruled: its intent is the literal
//! reading, recorded with the reason in [`RULED_LITERAL`]. A use not in
//! that table is a grammar that meant a strict list and was never
//! rewritten; a ruled row that no longer occurs is stale.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::{GrammarIR, IrNode};

/// `(grammar path, rule, reason)`: uses of `( x << sep ? ) *|+` whose
/// intent is the literal reading. None among the lowered grammars.
///
/// Read by hand because the Rust front end does not lower them:
/// `grammar/misc/json-commented.bbnf` and its TypeScript fixture copy
/// are comment-syntax fixtures (`/*a*/` in every position); their
/// `( value << ","? ) *` lists are the literal repetition the
/// TypeScript package has always read, not a JSON claim (the fixture
/// has no whitespace rule and single-quoted strings), so they keep it.
/// `grammar/misc/emoji.bbnf` (and its copy) and the six pinned
/// `typescript/test/fixtures/value-js/` grammars (X-8, a released
/// tarball) do not use the form.
const RULED_LITERAL: &[(&str, &str, &str)] = &[];

/// `(grammar path, rule, ask)`: TypeScript-package fixture copies of
/// grammars that mean a strict list, held for ruling S1-C. They feed
/// the frozen TypeScript conformance reference
/// (`typescript/test/conformance/grammars.json`) and two value-shape
/// tests, so their rewrite re-freezes reference values; the held set
/// is named exactly and any other use is RED.
const HELD: &[(&str, &str, &str)] = &[
    (
        "typescript/test/fixtures/grammar/css/css-keyframes.bbnf",
        "KEYFRAME_BLOCK",
        "S1-C",
    ),
    (
        "typescript/test/fixtures/grammar/css/css-stylesheet.bbnf",
        "KEYFRAME_BLOCK",
        "S1-C",
    ),
    (
        "typescript/test/fixtures/grammar/lang/google-sheets.bbnf",
        "func_args",
        "S1-C",
    ),
    (
        "typescript/test/fixtures/grammar/lang/google-sheets.bbnf",
        "lambda_params",
        "S1-C",
    ),
    (
        "typescript/test/fixtures/grammar/lang/json.bbnf",
        "array",
        "S1-C",
    ),
    (
        "typescript/test/fixtures/grammar/lang/json.bbnf",
        "object",
        "S1-C",
    ),
];

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn tracked_grammars(root: &Path) -> Vec<String> {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(root)
        .args(["ls-files", "-z", "--", "*.bbnf"])
        .output()
        .expect("git ls-files");
    assert!(out.status.success(), "git ls-files failed");
    String::from_utf8(out.stdout)
        .expect("utf-8 paths")
        .split('\0')
        .filter(|p| !p.is_empty())
        .filter(|p| !p.starts_with("restart/") && !p.starts_with("docs/"))
        .map(str::to_owned)
        .collect()
}

fn is_literal_optional_separator_repeat(node: &IrNode) -> bool {
    matches!(node, IrNode::Repeat { inner, hi, .. } if *hi > 1
        && matches!(inner.as_ref(), IrNode::Skip(_, sep)
            if matches!(sep.as_ref(), IrNode::Repeat { lo: 0, hi: 1, .. })))
}

fn walk(node: &IrNode, hit: &mut bool) {
    if is_literal_optional_separator_repeat(node) {
        *hit = true;
    }
    match node {
        IrNode::Seq(children) => children.iter().for_each(|c| walk(c, hit)),
        IrNode::Alt(branches, _) => branches.iter().for_each(|b| walk(&b.node, hit)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => walk(inner, hit),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            walk(a, hit);
            walk(b, hit);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            walk(token, hit);
            arms.iter().for_each(|a| walk(&a.continuation, hit));
            walk(fallback, hit);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

fn literal_form_rules(ir: &GrammarIR) -> Vec<String> {
    ir.rules
        .iter()
        .filter(|r| {
            let mut hit = false;
            walk(&r.body, &mut hit);
            hit
        })
        .map(|r| ir.get_string(r.name).to_owned())
        .collect()
}

#[test]
fn literal_optional_separator_forms_are_ruled() {
    let root = workspace_root();
    let mut found = BTreeSet::new();
    let mut unlowered = Vec::new();
    for rel in tracked_grammars(&root) {
        let request = CompileRequest {
            options: PipelineOptions::default(),
            target: CompileTarget::Vm,
        };
        match compile_paths_request(&[root.join(&rel)], &request) {
            Ok(CompileOutput::Vm(ir)) => {
                for rule in literal_form_rules(&ir) {
                    found.insert((rel.clone(), rule));
                }
            }
            Ok(other) => panic!("{rel}: expected VM output, got {other:?}"),
            Err(e) => unlowered.push(format!("{rel}: {e:?}")),
        }
    }
    let ruled: BTreeSet<(String, String)> = RULED_LITERAL
        .iter()
        .chain(HELD)
        .map(|(g, r, _)| ((*g).to_owned(), (*r).to_owned()))
        .collect();
    let unruled: Vec<_> = found.difference(&ruled).collect();
    let stale: Vec<_> = ruled.difference(&found).collect();
    // Read by hand (see `RULED_LITERAL`): the comment-syntax and emoji
    // fixtures, `func-body.bbnf` (a module whose `dimension` comes from
    // its importer) and the pinned value.js grammars.
    eprintln!("not lowered (read by hand in the audit): {unlowered:#?}");
    assert!(
        unruled.is_empty() && stale.is_empty(),
        "literal `( x << sep ? ) *` uses not ruled (rewrite as `x (sep x)*` if strict, or rule them literal): {unruled:#?}\nstale ruled rows: {stale:#?}"
    );
}
