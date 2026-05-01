//! Pass: Type projection for IR rules.
//!
//! Uses the `csp-solver` crate to infer types for all IR nodes via AC-3
//! propagation. The solver assigns a `TypeDesc` to every node in both normal
//! and vec-context, then exports the results into the `TypeMap` consumed by
//! codegen. The public `TypeMap` is keyed by stable `NodeId` via `ir.dag`;
//! the CSP's internal pointer-keyed bookkeeping is translated at extraction.
//!
//! Also collects sub-variants for heterogeneous alternations and stores them
//! in `RuleMeta::sub_variants`.

pub mod constraint;
pub mod generate;
pub mod registry;
mod subvariants;
mod type_map;

use std::collections::HashMap;

use rustc_hash::FxHashMap;

use crate::dag::{GrammarDag, NodeId};
use crate::{GrammarIR, IrNode, RuleId, SubVariant, TypeDesc, TypeDescInterner};

use constraint::SeqChildKind;
use generate::generate_constraints;
use subvariants::{collect_sub_variants_raw, validate_sub_variant_uniqueness_raw};

// Re-export for codegen.
pub use type_map::{TypeMap, try_flatten_pair};

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
    // The durable DAG substrate is built exactly once per compile
    // in `pipeline::compile` before any facts/strategy phase runs.
    // Unit tests that exercise this pass in isolation must call
    // `bbnf_ir::dag::ensure_dag(&mut ir)` beforehand. Asserting
    // here keeps the invariant honest instead of papering over
    // it with a defensive fallback.
    assert!(
        ir.dag.is_some(),
        "project_types requires ir.dag — call bbnf_ir::dag::ensure_dag in tests",
    );

    // Phase 1: Generate constraint system from the IR structure.
    let mut system = generate_constraints(ir);

    // Phase 2: Solve via monotonic fixed-point propagation.
    let _ = system.csp.propagate();

    // Phase 2b: Cycle-break any rule variables still unsolved after
    // propagation. `RefConstraint` suspends propagation while the
    // target rule is unsolved so scalar rule types can flow to every
    // reference. Rule-to-rule cycles (e.g. `a = b; b = a`) never reach
    // a solved state under that policy — propagate would stall
    // waiting for either endpoint. Ground any surviving unsolved rule
    // variables to `BoxedEnum` and re-run propagation so every
    // reference inside the cycle inherits the fallback.
    {
        let unsolved_rule_vars: Vec<csp_solver::constraint::VarId> = system
            .rule_vars
            .values()
            .copied()
            .filter(|&var| system.csp.variables[var as usize].domain.solved.is_none())
            .collect();
        if !unsolved_rule_vars.is_empty() {
            for var in unsolved_rule_vars {
                system.csp.variables[var as usize].domain.solved = Some(TypeDesc::BoxedEnum);
            }
            let _ = system.csp.propagate();
        }
    }

    // Tranche X Phase 0: build a reverse var→node map ONCE before
    // the Seq-children loop below. The previous implementation was a
    // linear `iter().chain().find_map()` scan over `system.node_vars`
    // + `system.vec_context_vars` called inside that loop, which is
    // O(rules × children × vars) and dominated `compile_css_l4`
    // post-Tranche W (23.13% self-time). The reverse map collapses
    // the lookup to O(1) with one allocation per compile.
    //
    // The chain order in the prior `find_node_id_for_var` preferred
    // `node_vars` over `vec_context_vars` on the (impossible) tie.
    // We preserve that bias by inserting `vec_context_vars` first
    // and then overwriting with `node_vars`.
    let mut var_to_node: FxHashMap<csp_solver::constraint::VarId, NodeId> =
        FxHashMap::with_capacity_and_hasher(
            system.node_vars.len() + system.vec_context_vars.len(),
            Default::default(),
        );
    for (&nid, &var_id) in &system.vec_context_vars {
        var_to_node.insert(var_id, nid);
    }
    for (&nid, &var_id) in &system.node_vars {
        var_to_node.insert(var_id, nid);
    }

    // Phase 3: Extract solved types into TypeMap keyed by `NodeId`.
    let mut type_map = TypeMap::default();

    // Build node_types and vec_elem_types from solved variables.
    // Maps are keyed on stable `NodeId` directly — the old pointer
    // side map is gone.
    for (&nid, &var_id) in &system.node_vars {
        if let Some(ty) = &system.csp.variables[var_id as usize].domain.solved {
            type_map.insert_node_type(nid, ty.clone());
        }
    }
    for (&nid, &var_id) in &system.vec_context_vars {
        if let Some(ty) = &system.csp.variables[var_id as usize].domain.solved {
            type_map.insert_vec_elem_type(nid, ty.clone());
        }
    }

    // Build seq metadata from the tracked Seq constraints.
    for seq_meta in &system.seq_constraints {
        let generate::SeqConstraintMeta {
            seq_node_id,
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
        // O(1) lookup via the var_to_node reverse map built above.
        for (child_var, eff_ty) in children.iter().zip(effective_types.iter()) {
            if let Some(&nid) = var_to_node.get(child_var) {
                type_map.insert_node_type(nid, eff_ty.clone());
            }
        }

        let preserve = *preserve_spans && effective_types.iter().all(|t| *t == TypeDesc::Span);

        // Seq-keyed entries go under the Seq node's `NodeId`.
        let seq_nid = *seq_node_id;
        type_map.insert_seq_child_types(seq_nid, effective_types);
        type_map.insert_seq_preserve_spans(seq_nid, preserve);

        if let Some(result_ty) = &system.csp.variables[*var as usize].domain.solved {
            type_map.insert_seq_result_type(seq_nid, result_ty.clone());
        }
    }

    // Phase 3.5: Compute structural (pre-collapse) types for
    // emission. Walk the IR and detect where collapsed types differ
    // from structural. `ir.dag` is asserted non-None at entry.
    {
        let dag = ir.dag.as_ref().expect("ir.dag asserted non-None at entry");
        for rule in &ir.rules {
            compute_structural_types_for_node(&rule.body, &mut type_map, dag);
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
    //
    // Tranche X phase 4: collect borrowed `&str` rule names instead
    // of one `String` per rule. Pre-X this allocated N strings per
    // compile via `to_string()`. The names live in `ir.strings`
    // which is not mutated during sub-variant collection, so the
    // borrows are valid for the rest of the function body.
    let rule_names: FxHashMap<RuleId, &str> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name)))
        .collect();

    let mut raw_sub_variants: HashMap<RuleId, Vec<subvariants::RawSubVariant>> = HashMap::new();

    {
        let dag = ir.dag.as_ref().expect("ir.dag asserted non-None at entry");
        for rule in &ir.rules {
            if rule.meta.is_transparent {
                continue;
            }
            let rule_name = rule_names
                .get(&rule.id)
                .expect("rule ID must exist in rule_names map");
            let svs = collect_sub_variants_raw(rule_name, &rule.body, &type_map, dag);
            if !svs.is_empty() {
                raw_sub_variants.insert(rule.id, svs);
            }
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
    {
        let dag = ir.dag.as_ref().expect("ir.dag asserted non-None at entry");
        for rule in &ir.rules {
            if let Some(td) = types_map.get(&rule.id) {
                correct_repeat_elem_types(&rule.body, td, &mut type_map, dag);
            }
        }
    }

    // Phase 7: Collect distinct scratch types for codegen Vec generation.
    let mut scratch: Vec<TypeDesc> = Vec::new();

    fn collect_repeat_scratch(
        node: &IrNode,
        map: &type_map::TypeMap,
        dag: &GrammarDag,
        out: &mut Vec<TypeDesc>,
    ) {
        match node {
            IrNode::Repeat { inner, hi, .. } if *hi > 1 => {
                if let Some(nid) = dag.node_for(inner.as_ref()) {
                    if let Some(ty) = map.vec_elem_type(nid) {
                        if *ty != TypeDesc::Span && !out.contains(ty) {
                            out.push(ty.clone());
                        }
                    }
                }
                collect_repeat_scratch(inner, map, dag, out);
            }
            IrNode::Seq(children) => {
                for c in children {
                    collect_repeat_scratch(c, map, dag, out);
                }
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    collect_repeat_scratch(&b.node, map, dag, out);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                collect_repeat_scratch(l, map, dag, out);
                collect_repeat_scratch(r, map, dag, out);
            }
            IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. }
            | IrNode::Negate(inner) => {
                collect_repeat_scratch(inner, map, dag, out);
            }
            IrNode::Repeat { inner, .. } => {
                collect_repeat_scratch(inner, map, dag, out);
            }
            IrNode::TokenDispatch { arms, fallback, .. } => {
                for arm in arms {
                    collect_repeat_scratch(&arm.continuation, map, dag, out);
                }
                collect_repeat_scratch(fallback, map, dag, out);
            }
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        }
    }
    {
        let dag = ir.dag.as_ref().expect("ir.dag asserted non-None at entry");
        for rule in &ir.rules {
            collect_repeat_scratch(&rule.body, &type_map, dag, &mut scratch);
        }
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

    // Tranche AA.1 — populate the type_desc hash-cons interner.
    // Walk every TypeDesc that survived projection into `type_map`,
    // `types_map`, and rule-level sub-variants, interning each into
    // `ir.type_desc_interner`. The interner is then the single stable
    // source of `TypeDescId` identity for downstream consumers
    // (AA.5 dispatch signatures, AA.7 TaggedUnion narrowing, AA.15
    // tape view codegen). Idempotent: structurally equal TypeDescs
    // collapse to the same id.
    let mut interner = TypeDescInterner::new();
    populate_interner(&type_map, &types_map, &ir.rules, &mut interner);
    ir.type_desc_interner = interner;

    // Tranche AZ-I.W1 — populate the StructRegistry.
    //
    // Once the upstream solver has produced `types_map` and `type_map`,
    // every `Named` rule projects a `StructLayout` whose discriminator
    // and fields derive from the rule's `IrNode` body shape. The
    // registry-population phase reads the projected types but does not
    // mutate them; it only writes into `ir.struct_registry`. Idempotent:
    // structurally identical IR + types produce the same registry.
    //
    // The closure collects rule_types into a fast-hash map keyed by
    // RuleId so the per-rule layout build is O(1) per lookup.
    let rule_types_for_registry: FxHashMap<RuleId, TypeDesc> =
        types_map.iter().map(|(id, ty)| (*id, ty.clone())).collect();
    registry::populate_struct_registry(ir, &rule_types_for_registry, &type_map);

    ir.type_map = Some(type_map);
    ir.types = types_map.into_iter().collect();
    ir.types.sort_by_key(|(id, _)| *id);
}

/// Intern every `TypeDesc` that appears anywhere in the projected type
/// map, rule type map, or rule sub-variants. Called at the end of
/// [`project_types`] to populate `GrammarIR::type_desc_interner`.
///
/// The interner's idempotency makes this a simple walk — duplicate
/// shapes collapse to the same id, so the resulting table is exactly
/// as many entries as the grammar has distinct structural types.
fn populate_interner(
    type_map: &type_map::TypeMap,
    types_map: &HashMap<RuleId, TypeDesc>,
    rules: &[crate::IrRule],
    interner: &mut TypeDescInterner,
) {
    // Rule-level types (one entry per rule root).
    for ty in types_map.values() {
        intern_recursive(ty, interner);
    }
    // Per-node types assigned by the projection CSP.
    for (_, ty) in type_map.iter_node_types() {
        intern_recursive(ty, interner);
    }
    for (_, ty) in type_map.iter_vec_elem_types() {
        intern_recursive(ty, interner);
    }
    for (_, ty) in type_map.iter_structural_types() {
        intern_recursive(ty, interner);
    }
    for (_, ty) in type_map.iter_seq_result_types() {
        intern_recursive(ty, interner);
    }
    for (_, types) in type_map.iter_seq_child_types() {
        for ty in types {
            intern_recursive(ty, interner);
        }
    }
    // Sub-variant types.
    for rule in rules {
        for sv in &rule.meta.sub_variants {
            intern_recursive(&sv.ty, interner);
        }
    }
    // Repeat scratch types (already distinct, but intern them for completeness).
    for ty in type_map.scratch_types() {
        intern_recursive(ty, interner);
    }
}

/// Recursively intern `ty` and every nested TypeDesc inside it. This
/// ensures the interner contains not only the outermost shape but
/// every sub-shape, so a downstream consumer that destructures a
/// `TypeDesc::Vec(inner)` can intern the inner with a single hash
/// lookup rather than a full walk.
fn intern_recursive(ty: &TypeDesc, interner: &mut TypeDescInterner) {
    // Intern the outermost first so the id order reflects the walk.
    interner.intern(ty.clone());
    match ty {
        TypeDesc::Option(inner) | TypeDesc::Vec(inner) => {
            intern_recursive(inner, interner);
        }
        TypeDesc::Tuple(elems) => {
            for e in elems {
                intern_recursive(e, interner);
            }
        }
        TypeDesc::Span
        | TypeDesc::F64
        | TypeDesc::Bool
        | TypeDesc::I8
        | TypeDesc::U8
        | TypeDesc::I16
        | TypeDesc::U16
        | TypeDesc::I32
        | TypeDesc::U32
        | TypeDesc::I64
        | TypeDesc::U64
        | TypeDesc::BoxedEnum
        | TypeDesc::Enum
        | TypeDesc::Named(_) => {}
    }
}

/// Correct Repeat vec_elem_types to match the Vec inner from
/// `ir.types`.
fn correct_repeat_elem_types(
    node: &IrNode,
    rule_type: &TypeDesc,
    map: &mut type_map::TypeMap,
    dag: &GrammarDag,
) {
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

    fn walk_and_correct(
        node: &IrNode,
        vec_inner: &TypeDesc,
        map: &mut type_map::TypeMap,
        dag: &GrammarDag,
    ) {
        match node {
            IrNode::Repeat { inner, lo: _, hi } if *hi > 1 => {
                if let Some(nid) = dag.node_for(inner.as_ref()) {
                    map.set_vec_elem_type(nid, vec_inner.clone());
                }
            }
            IrNode::Seq(children) => {
                for c in children {
                    walk_and_correct(c, vec_inner, map, dag);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                walk_and_correct(l, vec_inner, map, dag);
                walk_and_correct(r, vec_inner, map, dag);
            }
            IrNode::OptionalWhitespace(inner) => {
                walk_and_correct(inner, vec_inner, map, dag);
            }
            _ => {}
        }
    }

    walk_and_correct(node, vec_inner, map, dag);
}

/// Compute structural (pre-collapse) types for nodes where collapsed types
/// differ from the actual runtime topology. This enables the emit codegen to
/// generate correct destructuring code.
///
/// Walks the IR tree and detects collapse points:
/// 1. Optional(Span) → Span: Repeat(inner, 0, 1) with collapsed Span → structural Option(Span)
/// 2. Seq Span compression: result_type has fewer elements than children
/// 3. try_flatten_pair: (T, Vec(T)) collapsed to Vec(T)
fn compute_structural_types_for_node(
    node: &IrNode,
    type_map: &mut type_map::TypeMap,
    dag: &GrammarDag,
) {
    match node {
        IrNode::Repeat { inner, lo, hi } => {
            // Collapse 1: Optional(Span) → Span.
            if *lo == 0 && *hi == 1 {
                if let (Some(node_nid), Some(inner_nid)) =
                    (dag.node_for(node), dag.node_for(inner.as_ref()))
                {
                    if let Some(collapsed) = type_map.node_type(node_nid).cloned() {
                        if collapsed == TypeDesc::Span {
                            if let Some(inner_ty) = type_map.node_type(inner_nid).cloned() {
                                type_map.insert_structural_type(
                                    node_nid,
                                    TypeDesc::Option(Box::new(inner_ty)),
                                );
                            }
                        }
                    }
                }
            }
            compute_structural_types_for_node(inner, type_map, dag);
        }

        IrNode::Seq(children) => {
            // Collapse 2-3: Seq compression / try_flatten_pair. The
            // seq_result_type is the COLLAPSED type; the structural
            // type is the un-compressed, un-flattened Tuple of all
            // child types.
            if let Some(seq_nid) = dag.node_for(node) {
                if let Some(result_type) = type_map.seq_result_type(seq_nid).cloned() {
                    if let Some(child_types) = type_map.seq_child_types(seq_nid) {
                        let child_types = child_types.to_vec();
                        let structural_result = match child_types.len() {
                            0 => TypeDesc::Tuple(vec![]),
                            1 => child_types.into_iter().next().unwrap(),
                            _ => TypeDesc::Tuple(child_types),
                        };
                        if structural_result != result_type {
                            type_map.insert_structural_type(seq_nid, structural_result);
                        }
                    }
                }
            }
            for child in children {
                compute_structural_types_for_node(child, type_map, dag);
            }
        }

        IrNode::Alt(branches, _) => {
            for b in branches {
                compute_structural_types_for_node(&b.node, type_map, dag);
            }
        }
        IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
            compute_structural_types_for_node(l, type_map, dag);
            compute_structural_types_for_node(r, type_map, dag);
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) | IrNode::Negate(inner) => {
            compute_structural_types_for_node(inner, type_map, dag);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            compute_structural_types_for_node(token, type_map, dag);
            for arm in arms {
                compute_structural_types_for_node(&arm.continuation, type_map, dag);
            }
            compute_structural_types_for_node(fallback, type_map, dag);
        }
        _ => {} // Leaf nodes: Literal, Regex, Epsilon, Ref.
    }
}
