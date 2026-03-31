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

// Re-export for codegen to use the SAME inference functions (no divergence).
pub use infer::{infer_node, infer_node_in_vec};
pub use utils::{InferCtx, try_flatten_pair};

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
        .map(|r| {
            (
                r.id,
                (
                    r.meta.is_cyclic,
                    r.meta.span_eligible,
                ),
            )
        })
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
    let rules_snapshot: Vec<(RuleId, IrNode, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.body.clone(), r.meta.is_transparent))
        .collect();

    for (id, body, _is_transparent) in &rules_snapshot {
        let (is_cyclic, _span_eligible) =
            rule_meta.get(id).copied().unwrap_or_default();

        // B.4: For cyclic rules, override acyclic Ref types to BoxedEnum.
        // This matches AST codegen's behavior where acyclic deps in cyclic context
        // get boxed_enum_type to enable (Vec<A>, A) → Vec<A> flattening.
        let cyclic_context = is_cyclic;

        // B.2: Set pretty_preserve for the top-level call only.
        let pretty_preserve = pretty_preserve_rules.get(id).copied().unwrap_or(false);

        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context,
            pretty_preserve,
        };

        let ty = infer_node(body, &ctx);
        cache.insert(*id, ty);
    }

    // B.5: Collect sub-variants for heterogeneous alternations.
    // Two-pass: first collect raw (String name, TypeDesc, branch_index), then intern names.
    let rule_names: HashMap<RuleId, String> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    let mut raw_sub_variants: HashMap<RuleId, Vec<subvariants::RawSubVariant>> = HashMap::new();

    for (id, body, is_transparent) in &rules_snapshot {
        if *is_transparent {
            continue;
        }
        let (is_cyclic, _) = rule_meta.get(id).copied().unwrap_or_default();
        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            pretty_preserve: false,
        };
        let rule_name = rule_names.get(id)
            .expect("rule ID must exist in rule_names map");
        let svs = collect_sub_variants_raw(rule_name, body, &ctx);
        if !svs.is_empty() {
            raw_sub_variants.insert(*id, svs);
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
                    let name_id = if let Some(pos) =
                        ir.strings.iter().position(|s| s == &rsv.variant_name)
                    {
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
}
