//! AY.W2.1 — Empirical Named-collapse probe.
//!
//! Per `docs/tranches/AY/waves/W2.md` §AY.W2.1 + the A6 design doc
//! (`docs/tranches/AX/audit/next-tranche/A6-named-preservation-design.md`),
//! `TypeDesc::Named(_)` for CSS L4's `colorFunction` / `colorFn` / `colorMix`
//! and JSON's `string` rule collapses somewhere before reaching the Rust
//! emit layer. A6 conjectured H1 (e-graph cost-guided extraction unwraps
//! the outer Map) vs H2 (alias/transparent metadata stamping).
//!
//! This probe runs the full pipeline twice for each grammar:
//!  - `structural = true` (pre-optimisation snapshot — preserve_identity
//!    keeps every rule, normalizer and e-graph skipped).
//!  - default (production path).
//!
//! Cross-comparing the two snapshots for each named-annotated rule
//! discriminates the actual root cause. The probe's findings (recorded
//! in `docs/tranches/AY/audit/AYW2-named-collapse-probe.md`) are that
//! neither H1 nor H2 fires:
//!
//!  - JSON's `string` survives end-to-end as `Named("String")` — no fix
//!    needed for that rule.
//!  - CSS L4's `colorFn` (and `colorMix`) projects as `Tuple([Span, U8,
//!    BoxedEnum, ...])` even in the structural pre-opt snapshot — the
//!    outer Map was never at the body root because the BBNF grammar's
//!    precedence binds `-> input : <Name>` to the rightmost `factor`,
//!    so the annotation only wraps the closing `)` literal.
//!  - `colorFunction` and `colorMix` are eliminated by `prune_unreachable`
//!    because the entry-reachable `value` rule (in `properties.bbnf`)
//!    does not reference `color`. The `color → colorMix → color` cycle
//!    is genuinely unreachable from `stylesheet`.
//!
//! Both root causes are GRAMMAR-level, not pipeline-pass-level. The
//! audit doc records the recommended fix surface; this probe stays in
//! the codebase as a pinned reproducer.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, RuleId, TypeDesc};

// ───────────────────────── Shared probe helpers ─────────────────────────

/// What we know about a Named-annotated rule after the pipeline runs.
#[derive(Debug)]
struct NamedRuleProbe {
    rule_name: String,
    expected_named: String,
    /// True iff the rule still exists in `ir.rules` post-pipeline.
    survives_in_rules: bool,
    /// True iff the rule's `RuleMeta::is_alias` is `Some(_)`.
    flagged_as_alias: bool,
    /// True iff the rule's `RuleMeta::is_transparent` is true.
    flagged_as_transparent: bool,
    /// True iff the rule body still contains an `IrNode::Map { fn_id }`
    /// where the resolved `FnDescriptor` is `Expr { return_type:
    /// Some(Named(<expected_named>)), .. }`. Walks the body recursively
    /// (Map/Seq/Alt/etc.) — true if ANY descendant carries the
    /// Named annotation.
    body_carries_named_map: bool,
    /// True iff the rule's outermost body node is `IrNode::Map { .. }`
    /// (regardless of return_type). This is how `MapConstraint` grounds
    /// the rule's CSP variable to the map_type — when the outer Map
    /// is missing, the rule's TypeDesc inherits whatever the inner
    /// Seq/Alt projects, which is NOT Named.
    body_root_is_map: bool,
    /// `ir.types[rule.id]` if present.
    projected_type: Option<TypeDesc>,
    /// String form of `projected_type` for human reading (`Named("Color")`,
    /// `Tuple([Span, Span, Span])`, etc.).
    projected_type_display: String,
}

fn probe_named_rule(ir: &GrammarIR, rule_name: &str, expected_named: &str) -> NamedRuleProbe {
    let rule = ir.find_rule(rule_name);
    let survives = rule.is_some();

    let flagged_as_alias = rule.map(|r| r.meta.is_alias.is_some()).unwrap_or(false);
    let flagged_as_transparent = rule.map(|r| r.meta.is_transparent).unwrap_or(false);

    let body_carries_named_map = rule
        .map(|r| body_carries_named_map(&r.body, ir, expected_named))
        .unwrap_or(false);

    let body_root_is_map = rule
        .map(|r| matches!(&r.body, IrNode::Map { .. }))
        .unwrap_or(false);

    let projected_type = rule.and_then(|r| {
        ir.types
            .iter()
            .find_map(|(id, td)| (*id == r.id).then(|| td.clone()))
    });

    let projected_type_display = match &projected_type {
        Some(TypeDesc::Named(sid)) => format!("Named(\"{}\")", ir.get_string(*sid)),
        Some(td) => format!("{td:?}"),
        None => "<no entry>".to_string(),
    };

    NamedRuleProbe {
        rule_name: rule_name.to_string(),
        expected_named: expected_named.to_string(),
        survives_in_rules: survives,
        flagged_as_alias,
        flagged_as_transparent,
        body_carries_named_map,
        body_root_is_map,
        projected_type,
        projected_type_display,
    }
}

/// Recursively walk `node`, returning true if any nested `IrNode::Map`
/// resolves to `FnDescriptor::Expr { return_type: Some(Named(<expected>)), .. }`.
fn body_carries_named_map(node: &IrNode, ir: &GrammarIR, expected_named: &str) -> bool {
    match node {
        IrNode::Map { inner, fn_id } => {
            if let FnDescriptor::Expr { return_type, .. } = &ir.fns[*fn_id as usize] {
                if let Some(TypeDesc::Named(sid)) = return_type {
                    if ir.get_string(*sid) == expected_named {
                        return true;
                    }
                }
            }
            body_carries_named_map(inner, ir, expected_named)
        }
        IrNode::Seq(children) => children
            .iter()
            .any(|c| body_carries_named_map(c, ir, expected_named)),
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| body_carries_named_map(&b.node, ir, expected_named)),
        IrNode::Repeat { inner, .. } => body_carries_named_map(inner, ir, expected_named),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_carries_named_map(a, ir, expected_named)
                || body_carries_named_map(b, ir, expected_named)
        }
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => {
            body_carries_named_map(inner, ir, expected_named)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            body_carries_named_map(token, ir, expected_named)
                || arms.iter().any(|a| {
                    body_carries_named_map(&a.continuation, ir, expected_named)
                        || a.map_fn.is_some_and(|mf| {
                            if let FnDescriptor::Expr { return_type, .. } = &ir.fns[mf as usize] {
                                if let Some(TypeDesc::Named(sid)) = return_type {
                                    return ir.get_string(*sid) == expected_named;
                                }
                            }
                            false
                        })
                })
                || body_carries_named_map(fallback, ir, expected_named)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => false,
    }
}

/// Locate every rule whose lowered body contains a Map → `Expr { Named(_) }`
/// descendant — the universe of rules that COULD survive as Named at emit.
/// Used to confirm Section 1 of A6: the annotation reaches IR-construction.
fn collect_lowered_named_universe(ir: &GrammarIR) -> Vec<(RuleId, String, String)> {
    let mut out: Vec<(RuleId, String, String)> = Vec::new();
    for rule in &ir.rules {
        let mut found: Option<String> = None;
        walk_collect_named(&rule.body, ir, &mut found);
        if let Some(named) = found {
            out.push((rule.id, ir.get_string(rule.name).to_string(), named));
        }
    }
    out
}

fn walk_collect_named(node: &IrNode, ir: &GrammarIR, found: &mut Option<String>) {
    if found.is_some() {
        return;
    }
    match node {
        IrNode::Map { inner, fn_id } => {
            if let FnDescriptor::Expr { return_type, .. } = &ir.fns[*fn_id as usize] {
                if let Some(TypeDesc::Named(sid)) = return_type {
                    *found = Some(ir.get_string(*sid).to_string());
                    return;
                }
            }
            walk_collect_named(inner, ir, found);
        }
        IrNode::Seq(children) => {
            for c in children {
                walk_collect_named(c, ir, found);
                if found.is_some() {
                    return;
                }
            }
        }
        IrNode::Alt(branches, _) => {
            for b in &*branches {
                walk_collect_named(&b.node, ir, found);
                if found.is_some() {
                    return;
                }
            }
        }
        IrNode::Repeat { inner, .. } => walk_collect_named(inner, ir, found),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            walk_collect_named(a, ir, found);
            if found.is_none() {
                walk_collect_named(b, ir, found);
            }
        }
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => {
            walk_collect_named(inner, ir, found)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            walk_collect_named(token, ir, found);
            if found.is_some() {
                return;
            }
            for arm in arms {
                walk_collect_named(&arm.continuation, ir, found);
                if found.is_some() {
                    return;
                }
                if let Some(mf) = arm.map_fn {
                    if let FnDescriptor::Expr { return_type, .. } = &ir.fns[mf as usize] {
                        if let Some(TypeDesc::Named(sid)) = return_type {
                            *found = Some(ir.get_string(*sid).to_string());
                            return;
                        }
                    }
                }
            }
            walk_collect_named(fallback, ir, found);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

fn run_pipeline_to_vm_ir(grammar_paths: &[PathBuf]) -> GrammarIR {
    let request = CompileRequest {
        target: CompileTarget::Vm,
        options: PipelineOptions::default(),
    };
    let out = compile_paths_request(grammar_paths, &request)
        .expect("pipeline must succeed for the probe");
    match out {
        CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    }
}

/// Run the pipeline with `structural = true`. In this mode every rule
/// has `preserve_identity = true` set upstream, which:
/// (a) skips the structural normalizer loop entirely (no inline /
///     fuse / prune destructive rewrites);
/// (b) skips the e-graph saturation block (gated on `!options.structural`);
/// so the resulting `ir.rules` is the post-lowering view BEFORE any
/// optimisation. Comparing the structural snapshot's rule survival
/// against the optimised snapshot's rule survival isolates the
/// pipeline pass that drops the rule.
fn run_pipeline_structural(grammar_paths: &[PathBuf]) -> GrammarIR {
    let request = CompileRequest {
        target: CompileTarget::Vm,
        options: PipelineOptions {
            structural: true,
            ..Default::default()
        },
    };
    let out = compile_paths_request(grammar_paths, &request)
        .expect("structural-mode pipeline must succeed for the probe");
    match out {
        CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    }
}

/// Find every rule that references `target_name` via `Ref(target_id)`
/// in its body. Returns `(referencing_rule_name, occurrence_count)`.
fn find_references_to(ir: &GrammarIR, target_name: &str) -> Vec<(String, usize)> {
    let target_id = match ir.find_rule(target_name) {
        Some(r) => r.id,
        None => return Vec::new(),
    };
    let mut out: Vec<(String, usize)> = Vec::new();
    for rule in &ir.rules {
        let count = count_refs_to(&rule.body, target_id);
        if count > 0 {
            out.push((ir.get_string(rule.name).to_string(), count));
        }
    }
    out
}

fn count_refs_to(node: &IrNode, target: RuleId) -> usize {
    match node {
        IrNode::Ref(id) => {
            if *id == target {
                1
            } else {
                0
            }
        }
        IrNode::Seq(children) => children.iter().map(|c| count_refs_to(c, target)).sum(),
        IrNode::Alt(branches, _) => branches
            .iter()
            .map(|b| count_refs_to(&b.node, target))
            .sum(),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => count_refs_to(inner, target),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs_to(a, target) + count_refs_to(b, target)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_refs_to(token, target)
                + arms
                    .iter()
                    .map(|a| count_refs_to(&a.continuation, target))
                    .sum::<usize>()
                + count_refs_to(fallback, target)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => 0,
    }
}

/// Print a one-line description of a node — the kind + immediate-child
/// summary. For `Map` nodes, shows the resolved `FnDescriptor` discriminator.
fn node_kind_summary(node: &IrNode, ir: &GrammarIR) -> String {
    match node {
        IrNode::Map { fn_id, .. } => {
            let kind = match &ir.fns[*fn_id as usize] {
                FnDescriptor::EnumWrap { variant } => {
                    format!("Map[EnumWrap({})]", ir.get_string(*variant))
                }
                FnDescriptor::BoxWrap => "Map[BoxWrap]".to_string(),
                FnDescriptor::NumberConvert { .. } => "Map[NumberConvert]".to_string(),
                FnDescriptor::HexConvert { .. } => "Map[HexConvert]".to_string(),
                FnDescriptor::SpanCapture => "Map[SpanCapture]".to_string(),
                FnDescriptor::Expr { return_type, .. } => match return_type {
                    Some(TypeDesc::Named(sid)) => {
                        format!("Map[Expr → Named(\"{}\")]", ir.get_string(*sid))
                    }
                    Some(td) => format!("Map[Expr → {td:?}]"),
                    None => "Map[Expr → <none>]".to_string(),
                },
            };
            kind
        }
        IrNode::Seq(_) => "Seq".to_string(),
        IrNode::Alt(branches, _) => format!("Alt[{}]", branches.len()),
        IrNode::Repeat { lo, hi, .. } => format!("Repeat[{lo}..={hi}]"),
        IrNode::Ref(rid) => {
            let name = ir
                .rules
                .iter()
                .find(|r| r.id == *rid)
                .map(|r| ir.get_string(r.name))
                .unwrap_or("<unknown>");
            format!("Ref({rid}={name})")
        }
        IrNode::Skip(_, _) => "Skip".to_string(),
        IrNode::Next(_, _) => "Next".to_string(),
        IrNode::Minus(_, _) => "Minus".to_string(),
        IrNode::Negate(_) => "Negate".to_string(),
        IrNode::OptionalWhitespace(_) => "OptionalWhitespace".to_string(),
        IrNode::TokenDispatch { .. } => "TokenDispatch".to_string(),
        IrNode::Literal(_) => "Literal".to_string(),
        IrNode::Regex(_) => "Regex".to_string(),
        IrNode::Epsilon => "Epsilon".to_string(),
    }
}

/// Print the body shape (limited depth) for one rule, to surface
/// where the outer Map went.
fn print_body_shape(ir: &GrammarIR, rule_name: &str, max_depth: usize) {
    let rule = match ir.find_rule(rule_name) {
        Some(r) => r,
        None => {
            eprintln!("  body shape: <{rule_name} eliminated from ir.rules>");
            return;
        }
    };
    eprintln!("  body shape of {rule_name} (depth ≤ {max_depth}):");
    fn walk(node: &IrNode, ir: &GrammarIR, indent: usize, depth_remaining: usize) {
        let prefix = "    ".repeat(indent);
        eprintln!("{prefix}- {}", node_kind_summary(node, ir));
        if depth_remaining == 0 {
            return;
        }
        match node {
            IrNode::Map { inner, .. }
            | IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner) => {
                walk(inner, ir, indent + 1, depth_remaining - 1);
            }
            IrNode::Seq(children) => {
                for c in children {
                    walk(c, ir, indent + 1, depth_remaining - 1);
                }
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    walk(&b.node, ir, indent + 1, depth_remaining - 1);
                }
            }
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                walk(a, ir, indent + 1, depth_remaining - 1);
                walk(b, ir, indent + 1, depth_remaining - 1);
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                walk(token, ir, indent + 1, depth_remaining - 1);
                for a in arms {
                    walk(&a.continuation, ir, indent + 1, depth_remaining - 1);
                }
                walk(fallback, ir, indent + 1, depth_remaining - 1);
            }
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        }
    }
    walk(&rule.body, ir, 1, max_depth);
}

/// Print a probe report — one row per (rule, expected_named) target.
fn print_probe_report(grammar_label: &str, probes: &[NamedRuleProbe], ir: &GrammarIR) {
    eprintln!("\n========== AY.W2.1 PROBE: {grammar_label} ==========");
    eprintln!("ir.rules.len()    = {}", ir.rules.len());
    eprintln!("ir.types.len()    = {}", ir.types.len());
    eprintln!(
        "ir.types Named(*) = {}",
        ir.types
            .iter()
            .filter(|(_, td)| matches!(td, TypeDesc::Named(_)))
            .count()
    );

    // Universe of lowered Named-bearing rules — confirms A6 §1:
    // annotation reaches IR.
    let universe = collect_lowered_named_universe(ir);
    eprintln!(
        "Named-bearing rules surviving normalizer + e-graph (post-pipeline): {}",
        universe.len()
    );
    for (rid, name, named) in &universe {
        eprintln!("  surviving: rule_id={rid} name={name:<24} named={named}");
    }

    eprintln!("\n--- per-rule probe ---");
    eprintln!(
        "{:<16}  {:<10}  {:<8}  {:<8}  {:<10}  {:<10}  {:<8}  {}",
        "rule", "expected", "survive", "alias", "transparent", "body_root", "any_map", "ir.types"
    );
    for p in probes {
        eprintln!(
            "{:<16}  {:<10}  {:<8}  {:<8}  {:<11}  {:<10}  {:<8}  {}",
            p.rule_name,
            p.expected_named,
            p.survives_in_rules,
            p.flagged_as_alias,
            p.flagged_as_transparent,
            if p.body_root_is_map { "Map" } else { "non-Map" },
            p.body_carries_named_map,
            p.projected_type_display,
        );
    }

    // Backreferences: who points at the rule? When a rule eliminated
    // from `ir.rules`, walking back through the referrers tells us
    // whether the consumer was rewritten (e-graph extraction picked a
    // form without the Ref) or whether the rule had no consumer
    // (orphan).
    eprintln!("\n--- back-references ---");
    for p in probes {
        let referrers = find_references_to(ir, &p.rule_name);
        if referrers.is_empty() {
            eprintln!(
                "  {:<16}  no live referrers in ir.rules (consumer rule may have lost the Ref)",
                p.rule_name
            );
        } else {
            let summary: Vec<String> = referrers
                .iter()
                .map(|(name, count)| format!("{name}×{count}"))
                .collect();
            eprintln!("  {:<16}  referrers: {}", p.rule_name, summary.join(", "));
        }
    }

    // For each surviving probe rule, dump its body shape so the H1
    // case (Map descended → not at root) is unambiguous.
    eprintln!("\n--- body shapes (depth ≤ 4) ---");
    for p in probes {
        if p.survives_in_rules {
            print_body_shape(ir, &p.rule_name, 4);
        }
    }

    // Hypothesis discrimination summary. The verdicts below classify
    // each rule by post-pipeline observation; cross-reference the
    // pre-opt structural snapshot to discriminate grammar-level vs
    // pipeline-level causes.
    eprintln!("\n--- hypothesis discrimination ---");
    for p in probes {
        let verdict = if !p.survives_in_rules {
            "ELIMINATED (rule pruned — check pre-opt snapshot for whether \
             the rule was reachable from entry)"
        } else if p.flagged_as_alias {
            "H2-shape (is_alias stamped — canonicalize_aliases drops shell)"
        } else if p.flagged_as_transparent {
            "H2-shape (is_transparent stamped — Named never reaches CSP)"
        } else if !p.body_carries_named_map {
            "H1-shape (body lost the Named-bearing Map; e-graph extraction)"
        } else if !p.body_root_is_map {
            "BODY-ROOT-NOT-MAP (Map descended but body root is non-Map; \
             MapConstraint not grounding the rule_var. Cross-check pre-opt \
             snapshot — if pre-opt body root is also non-Map, this is a \
             grammar-precedence issue, NOT an IR-pipeline collapse.)"
        } else if !matches!(p.projected_type, Some(TypeDesc::Named(_))) {
            "OTHER (rule + Map intact yet ir.types is not Named — \
             CSP-propagation issue)"
        } else {
            "PASS — Named survives end-to-end (no fix needed for this rule)"
        };
        eprintln!(
            "  {:<16}  expected={:<10}  → {}",
            p.rule_name, p.expected_named, verdict
        );
    }
}

// ───────────────────────── CSS L4 probe ─────────────────────────

#[test]
fn css_l4_named_pipeline_probe() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let stylesheet = manifest.join("../../grammar/css/l4/stylesheet.bbnf");

    // Two-phase probe: capture the pre-optimiser snapshot via
    // `structural = true`, then capture the post-optimiser snapshot
    // via the production path. Diffing the two surfaces the exact
    // pass(es) that drop the Named-bearing rules.
    let structural_ir = run_pipeline_structural(&[stylesheet.clone()]);
    eprintln!("\n========== AY.W2.1 PRE-OPT (structural) snapshot — CSS L4 ==========");
    eprintln!(
        "ir.rules.len() (structural)    = {}",
        structural_ir.rules.len()
    );
    eprintln!(
        "ir.types Named(*) (structural) = {}",
        structural_ir
            .types
            .iter()
            .filter(|(_, td)| matches!(td, TypeDesc::Named(_)))
            .count()
    );
    for name in ["color", "colorFunction", "colorFn", "colorMix"] {
        if let Some(rule) = structural_ir.find_rule(name) {
            let projected = structural_ir
                .types
                .iter()
                .find_map(|(id, td)| (*id == rule.id).then(|| td.clone()));
            eprintln!(
                "  pre-opt: {name:<16}  is_alias={:<12}  is_transparent={:<5}  is_cyclic={:<5}  preserve_identity={:<5}  body_root={:<10}  ir.types={}",
                format!("{:?}", rule.meta.is_alias),
                rule.meta.is_transparent,
                rule.meta.is_cyclic,
                rule.meta.preserve_identity,
                node_kind_summary(&rule.body, &structural_ir),
                projected
                    .as_ref()
                    .map(|t| match t {
                        TypeDesc::Named(sid) =>
                            format!("Named(\"{}\")", structural_ir.get_string(*sid)),
                        other => format!("{other:?}"),
                    })
                    .unwrap_or_else(|| "<none>".to_string()),
            );
        } else {
            eprintln!("  pre-opt: {name:<16}  <not present even in structural mode>");
        }
    }
    eprintln!("\n--- pre-opt colorFn body shape (depth ≤ 4) ---");
    print_body_shape(&structural_ir, "colorFn", 4);
    eprintln!("\n--- pre-opt colorFunction body shape (depth ≤ 4) ---");
    print_body_shape(&structural_ir, "colorFunction", 4);
    eprintln!("\n--- pre-opt colorMix body shape (depth ≤ 4) ---");
    print_body_shape(&structural_ir, "colorMix", 4);
    eprintln!("\n--- pre-opt color body shape (depth ≤ 4) ---");
    print_body_shape(&structural_ir, "color", 4);
    eprintln!("\n--- pre-opt value body shape (depth ≤ 3) ---");
    print_body_shape(&structural_ir, "value", 3);

    let ir = run_pipeline_to_vm_ir(&[stylesheet]);

    // Three CSS L4 colour rules each declare `-> input : Color`.
    let probes: Vec<NamedRuleProbe> = ["colorFunction", "colorFn", "colorMix"]
        .iter()
        .map(|name| probe_named_rule(&ir, name, "Color"))
        .collect();

    print_probe_report("CSS L4", &probes, &ir);

    // The `color` rule is the parent — it should reference each
    // colour function rule as `Ref(_)`. Inspect it to see whether
    // the e-graph or post-loop rewrite eliminated those Refs.
    eprintln!("\n--- parent `color` rule shape (depth ≤ 6) ---");
    print_body_shape(&ir, "color", 6);

    // The `color` rule's cousin `colorFn` survives but its body root
    // is no longer Map. Walk it to confirm where the Map went.
    eprintln!("\n--- `colorFn` body shape (depth ≤ 6) ---");
    print_body_shape(&ir, "colorFn", 6);

    // Trace `value` (the only rule that references colorFn after
    // optimisation). If it inlined the colour Alt, that's where the
    // shell collapse happened.
    eprintln!("\n--- `value` rule shape post-opt (depth ≤ 6) ---");
    print_body_shape(&ir, "value", 6);

    // Pre-opt vs post-opt rule survival diff.
    eprintln!("\n--- rule survival diff (pre-opt → post-opt) ---");
    let pre_names: std::collections::HashSet<String> = structural_ir
        .rules
        .iter()
        .map(|r| structural_ir.get_string(r.name).to_string())
        .collect();
    let post_names: std::collections::HashSet<String> = ir
        .rules
        .iter()
        .map(|r| ir.get_string(r.name).to_string())
        .collect();
    let dropped: Vec<&String> = pre_names.difference(&post_names).collect();
    let added: Vec<&String> = post_names.difference(&pre_names).collect();
    eprintln!(
        "  pre-opt rules: {}  →  post-opt rules: {}",
        pre_names.len(),
        post_names.len()
    );
    eprintln!("  dropped (pre-only): {} rules", dropped.len());
    for name in ["color", "colorFunction", "colorMix", "namedColor", "hex"] {
        let pre_present = pre_names.contains(name);
        let post_present = post_names.contains(name);
        eprintln!("    {name:<16}  pre-opt: {pre_present:<5}  post-opt: {post_present}");
    }
    let added_synth: Vec<&&String> = added.iter().filter(|n| n.starts_with("__")).collect();
    eprintln!(
        "  added (post-only): {} rules ({} are synth `__*`)",
        added.len(),
        added_synth.len()
    );

    // The probe is informational and always succeeds; its role is to
    // print the per-rule discrimination so the orchestrator (and the
    // W2.2 fix author) can read the report without parsing pipeline-
    // internal traces. The post-fix wire-contract test
    // `named_type_preservation.rs` (W2.7) asserts the actual gate.
    let _any_named = probes
        .iter()
        .any(|p| matches!(&p.projected_type, Some(TypeDesc::Named(_))));
}

// ───────────────────────── JSON probe ─────────────────────────

#[test]
fn json_named_pipeline_probe() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let json_grammar = manifest.join("../../grammar/json/json.bbnf");

    let ir = run_pipeline_to_vm_ir(&[json_grammar]);

    // JSON's `string` rule declares `-> decode_json_string_to_arena(input) : String`.
    let probes: Vec<NamedRuleProbe> = [("string", "String")]
        .iter()
        .map(|(n, e)| probe_named_rule(&ir, n, e))
        .collect();

    print_probe_report("JSON", &probes, &ir);
}
