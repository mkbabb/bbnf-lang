//! Pass: Type inference for IR rules.
//!
//! Walks each rule's body and infers a `TypeDesc` describing the Rust/TS type
//! that the rule produces. Populates `GrammarIR::types` with `(RuleId, TypeDesc)`
//! pairs consumed by codegen backends.
//!
//! Also collects sub-variants for heterogeneous alternations and stores them
//! in `RuleMeta::sub_variants`.

mod infer;
mod subvariants;
mod utils;

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId, SubVariant, TypeDesc};

use subvariants::{collect_sub_variants_raw, validate_sub_variant_uniqueness_raw};

// Re-export for codegen.
pub use infer::{infer_node, infer_node_in_vec};
pub use utils::{InferCtx, InferMap, InferRecorder, try_flatten_pair};

/// Infer types for all rules and populate `ir.types`.
///
/// Rules are processed in topological order (the order they appear in `ir.rules`),
/// so a rule's dependencies are always inferred before the rule itself. Cyclic
/// rules are assigned `BoxedEnum` for any back-references.
///
/// Implements 5 fixes over the baseline:
/// - B.1: sp_method_rules Span override in Seq (all-Span guard)
/// - B.2: @pretty consumable flag for tuple preservation
/// - B.3: Custom mapping return type from closure annotation
/// - B.4: Cyclic→acyclic type override (BoxedEnum for non-inlined acyclic refs in cyclic context)
/// - B.5: Sub-variant collection for heterogeneous alternations
pub fn infer_types(ir: &mut GrammarIR) {
    let mut cache: HashMap<RuleId, TypeDesc> = HashMap::new();

    // Collect metadata before inference (avoids borrow issues).
    let rule_meta: HashMap<RuleId, (bool, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.id, (r.meta.is_cyclic, r.meta.span_eligible)))
        .collect();

    // Collect which rules have @pretty (for B.2).
    let pretty_preserve_rules: HashMap<RuleId, bool> = ir
        .rules
        .iter()
        .map(|r| {
            let has_pretty = r.meta.pretty.is_some();
            (r.id, has_pretty)
        })
        .collect();

    // Identify acyclic rules (for B.4 override).
    let acyclic_rules: std::collections::HashSet<RuleId> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_cyclic)
        .map(|r| r.id)
        .collect();

    // Process rules in order (topological after pipeline passes).
    // Record sub-expression types for codegen via InferRecorder.
    let recorder = InferRecorder::new();

    // Collect rule IDs + metadata to avoid borrowing ir during inference.
    let rule_ids: Vec<(RuleId, bool, bool)> = ir
        .rules
        .iter()
        .map(|r| {
            let is_cyclic = rule_meta.get(&r.id).copied().unwrap_or_default().0;
            let pretty_preserve = pretty_preserve_rules.get(&r.id).copied().unwrap_or(false);
            (r.id, is_cyclic, pretty_preserve)
        })
        .collect();

    for &(id, is_cyclic, pretty_preserve) in &rule_ids {
        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            pretty_preserve,
            recorder: Some(&recorder),
        };

        // Infer on the ORIGINAL body (not a clone) so pointers match codegen.
        let ty = infer_node(&ir.rules[id as usize].body, &ctx);
        cache.insert(id, ty);
    }

    // Comprehensive recording pass: walk every node in every rule body and record
    // both infer_node and infer_node_in_vec results. The cache is fully populated,
    // so this is just lookups + recording. Ensures the InferMap covers all nodes
    // that codegen might query (including B.1-overridden Ref children and
    // nodes skipped by the main inference path).
    for &(id, is_cyclic, pretty_preserve) in &rule_ids {
        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            pretty_preserve,
            recorder: Some(&recorder),
        };
        record_all_nodes(&ir.rules[id as usize].body, &ctx);
    }

    // B.5: Collect sub-variants for heterogeneous alternations.
    // Two-pass: first collect raw (String name, TypeDesc, branch_index), then intern names.
    let rule_names: HashMap<RuleId, String> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    let mut raw_sub_variants: HashMap<RuleId, Vec<subvariants::RawSubVariant>> = HashMap::new();

    for &(id, is_cyclic, _) in &rule_ids {
        let rule = &ir.rules[id as usize];
        if rule.meta.is_transparent {
            continue;
        }
        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            pretty_preserve: false,
            recorder: None, // No recording needed for sub-variants
        };
        let rule_name = rule_names
            .get(&id)
            .expect("rule ID must exist in rule_names map");
        let svs = collect_sub_variants_raw(rule_name, &rule.body, &ctx);
        if !svs.is_empty() {
            raw_sub_variants.insert(id, svs);
        }
    }

    // Validate cross-rule sub-variant uniqueness (using raw names).
    validate_sub_variant_uniqueness_raw(&raw_sub_variants, &rule_names);

    // Clear all existing sub-variants before writing new ones.
    // This is critical when infer_types runs multiple times (e.g., pipeline call
    // with b1_span_collapse=false, then generate_all with b1_span_collapse=true).
    // Without clearing, stale sub-variants from the first run persist and cause
    // type mismatches in codegen.
    for rule in &mut ir.rules {
        rule.meta.sub_variants.clear();
    }

    // Intern sub-variant names and write to rules.
    for rule in &mut ir.rules {
        if let Some(raw_svs) = raw_sub_variants.remove(&rule.id) {
            rule.meta.sub_variants = raw_svs
                .into_iter()
                .map(|rsv| {
                    // Intern the variant name into the string table.
                    let name_id =
                        if let Some(pos) = ir.strings.iter().position(|s| s == &rsv.variant_name) {
                            pos as u32
                        } else {
                            let id = ir.strings.len() as u32;
                            ir.strings.push(rsv.variant_name);
                            id
                        };
                    SubVariant {
                        variant_name: name_id,
                        ty: rsv.ty,
                        branch_index: rsv.branch_index,
                    }
                })
                .collect();
        }
    }

    ir.types = cache.into_iter().collect();
    ir.types.sort_by_key(|(id, _)| *id);

    // Store the precomputed sub-expression type map for codegen.
    ir.infer_map = Some(recorder.into_map());
}

/// Walk every node in the tree and record both `infer_node` and `infer_node_in_vec`
/// results. The cache must be fully populated before calling this.
fn record_all_nodes(node: &IrNode, ctx: &utils::InferCtx<'_>) {
    // Record types only if not already recorded by the main inference pass.
    // This preserves B.1 override types while filling in nodes that the main
    // pass didn't visit (e.g., discarded sides of Skip/Next).
    let node_ty = infer::infer_node(node, &ctx.with_no_recorder());
    let vec_ty = infer::infer_node_in_vec(node, &ctx.with_no_recorder());
    if let Some(rec) = ctx.recorder {
        rec.record_node_if_absent(node, &node_ty);
        rec.record_vec_elem_if_absent(node, &vec_ty);
    }

    // Recurse into children.
    match node {
        IrNode::Seq(children) => {
            let consumed = ctx.consumed();
            for c in children {
                record_all_nodes(c, &consumed);
            }
        }
        IrNode::Alt(branches, _) => {
            let consumed = ctx.consumed();
            for b in branches {
                record_all_nodes(&b.node, &consumed);
            }
        }
        IrNode::Repeat { inner, .. } => {
            let consumed = ctx.consumed();
            record_all_nodes(inner, &consumed);
        }
        IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
            let consumed = ctx.consumed();
            record_all_nodes(l, &consumed);
            record_all_nodes(r, &consumed);
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } | IrNode::Negate(inner) => {
            record_all_nodes(inner, ctx);
        }
        IrNode::TokenDispatch { arms, .. } => {
            let consumed = ctx.consumed();
            for arm in arms {
                record_all_nodes(&arm.continuation, &consumed);
            }
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
