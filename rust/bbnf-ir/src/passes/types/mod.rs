//! Pass: Type projection for IR rules.
//!
//! Uses the `csp-solver` crate to infer types for all IR nodes via AC-3
//! propagation. The solver assigns a `TypeDesc` to every node in both normal
//! and vec-context, then exports the results into the `TypeMap` consumed by
//! codegen.
//!
//! Also collects sub-variants for heterogeneous alternations and stores them
//! in `RuleMeta::sub_variants`.

pub mod constraint;
pub mod generate;
mod subvariants;
mod utils;

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId, SubVariant, TypeDesc};

use constraint::SeqChildKind;
use generate::generate_constraints;
use subvariants::{collect_sub_variants_raw, validate_sub_variant_uniqueness_raw};

// Re-export for codegen.
pub use utils::{TypeMap, try_flatten_pair};

/// Project types for all rules and populate `ir.types`.
///
/// Uses the `csp-solver` crate with AC-3 propagation to infer types for every
/// IR node. The solver handles:
/// 1. Span-method override: Ref nodes to rules with `_sp()` methods project as
///    Span in Seq context, with a safety guard that reverts the override when any
///    child is Span through complex expressions (Repeat, Skip, etc.).
/// 2. Span preservation: When `preserve_spans` is set, the top-level Seq preserves
///    individual Span identity (skips consecutive-Span compression). Currently
///    sourced from `@pretty` directives; flag is consumed after the first Seq.
/// 3. Annotated return type: Map nodes with FnDescriptor::Expr use the return
///    type from the MapExpr, falling back to Span.
/// 4. Vec-context types: Ref nodes in Vec context produce Enum (not BoxedEnum),
///    since Vec provides heap indirection.
/// 5. Sub-variant collection: Heterogeneous alternation branches get generated
///    variant names for codegen coercion.
pub fn project_types(ir: &mut GrammarIR) {
    // Phase 1: Generate constraint system from the IR structure.
    let mut system = generate_constraints(ir);

    // Phase 2: Solve via monotonic fixed-point propagation.
    let _ = system.csp.propagate_monotonic();

    // Phase 3: Extract solved types into TypeMap.
    let mut type_map = TypeMap::default();

    // Build node_types and vec_elem_types from solved variables.
    for (&node_id, &var_id) in &system.node_vars {
        if let Some(ty) = &system.csp.variables[var_id as usize].domain.solved {
            type_map.insert_node_type(node_id, ty.clone());
        }
    }
    for (&node_id, &var_id) in &system.vec_context_vars {
        if let Some(ty) = &system.csp.variables[var_id as usize].domain.solved {
            type_map.insert_vec_elem_type(node_id, ty.clone());
        }
    }

    // Build seq metadata from the tracked Seq constraints.
    for seq_meta in &system.seq_constraints {
        let generate::SeqConstraintMeta {
            var,
            children,
            preserve_spans,
            sp_override_originals,
            collapse_simple_spans,
            child_node_kinds,
        } = seq_meta;

        // Resolve child types from solver.
        let child_types: Vec<TypeDesc> = children
            .iter()
            .map(|&c| {
                system.csp.variables[c as usize]
                    .domain
                    .solved
                    .clone()
                    .unwrap_or(TypeDesc::Span)
            })
            .collect();

        // Replicate the span-method safety guard logic to determine effective types.
        let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
        let all_simple_span = all_span
            && *collapse_simple_spans
            && !preserve_spans
            && child_node_kinds
                .iter()
                .zip(child_types.iter())
                .all(|(kind, ty)| match kind {
                    SeqChildKind::Optional => false,
                    SeqChildKind::SpOverrideRef => true,
                    SeqChildKind::Other => *ty == TypeDesc::Span,
                });

        let effective_types = if all_span && !all_simple_span {
            children
                .iter()
                .zip(sp_override_originals.iter())
                .map(|(child_var, orig)| {
                    if let Some(orig_var) = orig {
                        system.csp.variables[*orig_var as usize]
                            .domain
                            .solved
                            .clone()
                            .unwrap_or(TypeDesc::Span)
                    } else {
                        system.csp.variables[*child_var as usize]
                            .domain
                            .solved
                            .clone()
                            .unwrap_or(TypeDesc::Span)
                    }
                })
                .collect::<Vec<_>>()
        } else {
            child_types
        };

        // Re-record per-node types for the children in this Seq.
        for (child_var, eff_ty) in children.iter().zip(effective_types.iter()) {
            let node_id = find_node_id_for_var(&system, *child_var);
            if let Some(nid) = node_id {
                type_map.insert_node_type(nid, eff_ty.clone());
            }
        }

        let preserve = *preserve_spans
            && effective_types.iter().all(|t| *t == TypeDesc::Span);

        if let Some(seq_ptr) = find_seq_ptr_for_constraint(&system, children) {
            type_map.insert_seq_child_types(seq_ptr, effective_types);
            type_map.insert_seq_preserve_spans(seq_ptr, preserve);

            if let Some(result_ty) = &system.csp.variables[*var as usize].domain.solved {
                type_map.insert_seq_result_type(seq_ptr, result_ty.clone());
            }
        }
    }

    // Phase 4: Build ir.types from rule variables.
    let mut types_map: HashMap<RuleId, TypeDesc> = HashMap::new();
    for (&rule_id, &var_id) in &system.rule_vars {
        if let Some(ty) = &system.csp.variables[var_id as usize].domain.solved {
            types_map.insert(rule_id, ty.clone());
        }
    }

    // Phase 5: Sub-variant collection using the solved TypeMap.
    let rule_names: HashMap<RuleId, String> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    let mut raw_sub_variants: HashMap<RuleId, Vec<subvariants::RawSubVariant>> = HashMap::new();

    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let rule_name = rule_names
            .get(&rule.id)
            .expect("rule ID must exist in rule_names map");
        let svs = collect_sub_variants_raw(rule_name, &rule.body, &type_map);
        if !svs.is_empty() {
            raw_sub_variants.insert(rule.id, svs);
        }
    }

    // Validate cross-rule sub-variant uniqueness.
    validate_sub_variant_uniqueness_raw(&raw_sub_variants, &rule_names);

    // Clear all existing sub-variants before writing new ones.
    for rule in &mut ir.rules {
        rule.meta.sub_variants.clear();
    }

    // Intern sub-variant names and write to rules.
    for rule in &mut ir.rules {
        if let Some(raw_svs) = raw_sub_variants.remove(&rule.id) {
            rule.meta.sub_variants = raw_svs
                .into_iter()
                .map(|rsv| {
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

    // Phase 6: Correction pass -- align Repeat vec_elem_types with ir.types.
    for rule in &ir.rules {
        let rule_td = types_map.get(&rule.id);
        if let Some(td) = rule_td {
            correct_repeat_elem_types(&rule.body, td, &mut type_map);
        }
    }

    // Phase 7: Collect distinct scratch types for codegen Vec generation.
    let mut scratch: Vec<TypeDesc> = Vec::new();

    fn collect_repeat_scratch(node: &IrNode, map: &utils::TypeMap, out: &mut Vec<TypeDesc>) {
        match node {
            IrNode::Repeat { inner, hi, .. } if *hi > 1 => {
                if let Some(ty) = map.vec_elem_type(inner) {
                    if *ty != TypeDesc::Span && !out.contains(ty) {
                        out.push(ty.clone());
                    }
                }
                collect_repeat_scratch(inner, map, out);
            }
            IrNode::Seq(children) => {
                for c in children {
                    collect_repeat_scratch(c, map, out);
                }
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    collect_repeat_scratch(&b.node, map, out);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                collect_repeat_scratch(l, map, out);
                collect_repeat_scratch(r, map, out);
            }
            IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. }
            | IrNode::Negate(inner) => {
                collect_repeat_scratch(inner, map, out);
            }
            IrNode::Repeat { inner, .. } => {
                collect_repeat_scratch(inner, map, out);
            }
            IrNode::TokenDispatch { arms, fallback, .. } => {
                for arm in arms {
                    collect_repeat_scratch(&arm.continuation, map, out);
                }
                collect_repeat_scratch(fallback, map, out);
            }
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        }
    }
    for rule in &ir.rules {
        collect_repeat_scratch(&rule.body, &type_map, &mut scratch);
    }

    // Also collect Vec inners from rule types.
    fn collect_scratch_from_td(td: &TypeDesc, out: &mut Vec<TypeDesc>) {
        match td {
            TypeDesc::Vec(inner) => {
                if *inner.as_ref() != TypeDesc::Span && !out.contains(inner.as_ref()) {
                    out.push(inner.as_ref().clone());
                }
                collect_scratch_from_td(inner, out);
            }
            TypeDesc::Option(inner) => collect_scratch_from_td(inner, out),
            TypeDesc::Tuple(elems) => {
                for e in elems {
                    collect_scratch_from_td(e, out);
                }
            }
            _ => {}
        }
    }
    for (_, td) in &types_map {
        collect_scratch_from_td(td, &mut scratch);
    }
    type_map.set_scratch_types(scratch);

    ir.type_map = Some(type_map);
    ir.types = types_map.into_iter().collect();
    ir.types.sort_by_key(|(id, _)| *id);
}

/// Find the node_id associated with a variable ID, by scanning the node_vars map.
fn find_node_id_for_var(
    system: &generate::ConstraintSystem,
    var_id: csp_solver::constraint::VarId,
) -> Option<usize> {
    // Check if any node maps directly to this var.
    for (&node_id, &v) in &system.node_vars {
        if v == var_id {
            return Some(node_id);
        }
    }
    // Also check vec_context_vars.
    for (&node_id, &v) in &system.vec_context_vars {
        if v == var_id {
            return Some(node_id);
        }
    }
    None
}

/// Find the Seq's children slice pointer from the seq_metadata,
/// matching by the child variable IDs stored during constraint generation.
fn find_seq_ptr_for_constraint(
    system: &generate::ConstraintSystem,
    child_vars: &[csp_solver::constraint::VarId],
) -> Option<usize> {
    for (&ptr, meta) in &system.seq_metadata {
        if meta.child_vars == child_vars {
            return Some(ptr);
        }
    }
    None
}

/// Correct Repeat vec_elem_types to match the Vec inner from ir.types.
fn correct_repeat_elem_types(node: &IrNode, rule_type: &TypeDesc, map: &mut utils::TypeMap) {
    fn extract_all_vec_inners<'a>(td: &'a TypeDesc, out: &mut Vec<&'a TypeDesc>) {
        match td {
            TypeDesc::Vec(inner) => out.push(inner.as_ref()),
            TypeDesc::Tuple(elems) => {
                for e in elems {
                    extract_all_vec_inners(e, out);
                }
            }
            TypeDesc::Option(inner) => extract_all_vec_inners(inner, out),
            _ => {}
        }
    }

    let mut vec_inners = Vec::new();
    extract_all_vec_inners(rule_type, &mut vec_inners);
    let vec_inner = match vec_inners.as_slice() {
        [inner] => *inner,
        _ => return,
    };

    fn walk_and_correct(node: &IrNode, vec_inner: &TypeDesc, map: &mut utils::TypeMap) {
        match node {
            IrNode::Repeat { inner, lo: _, hi } if *hi > 1 => {
                map.set_vec_elem_type(inner.as_ref(), vec_inner.clone());
            }
            IrNode::Seq(children) => {
                for c in children {
                    walk_and_correct(c, vec_inner, map);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                walk_and_correct(l, vec_inner, map);
                walk_and_correct(r, vec_inner, map);
            }
            IrNode::OptionalWhitespace(inner) => {
                walk_and_correct(inner, vec_inner, map);
            }
            _ => {}
        }
    }

    walk_and_correct(node, vec_inner, map);
}
