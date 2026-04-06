//! EmitPlan: pre-computed emission strategy from shared decisions.
//!
//! Consumes `decisions::decide_*()` to build a tree that describes exactly how
//! to destructure and emit each rule's value. The codegen walk (`codegen.rs`)
//! then converts this to TokenStream with zero type queries.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use crate::backend::decisions::{self, *};
use crate::backend::FlattenStrategy;
use crate::generate::ir_types::IrCodegenCtx;

// ═════════════════════════════════════════════════════════════════════════════
// EmitPlan types
// ═════════════════════════════════════════════════════════════════════════════

pub enum EmitPlan {
    /// Structural content: emit from IR, no value consumed.
    Structural(Vec<Frag>),
    /// Emit value as-is (Span text, f64, etc.)
    Leaf(Leaf),
    /// Sequence decomposition.
    Seq(SeqPlan),
    /// Alternation dispatch.
    Alt(AltPlan),
    /// Vec/Option iteration.
    Repeat(RepeatPlan),
    /// Rule reference.
    Ref(RefPlan),
    /// Map reversal.
    Map(MapPlan),
    /// Flattened Vec: `(T, Vec<T>) → Vec<T)`.
    FlatVec(FlatVecPlan),
}

/// Structural fragment.
pub enum Frag {
    Char(u8),
    Text(String),
}

/// Leaf emission (value already the right type).
pub enum Leaf {
    SpanText,
    F64,
    Hex,
    Display,
}

/// Seq decomposition.
pub struct SeqPlan {
    pub children: Vec<SeqChild>,
}

pub enum SeqChild {
    /// Structural: emit from IR.
    Structural(Vec<Frag>),
    /// Span at tuple position: emit val.{index}.as_str().
    TupleSpan { index: usize },
    /// Value at tuple position: destructure and recurse.
    TupleValue { index: usize, plan: Box<EmitPlan> },
    /// Sole value child (non-Tuple result): value IS this child directly.
    Direct { plan: Box<EmitPlan> },
}

/// Alt dispatch.
pub enum AltPlan {
    /// Constant-reverse: match on value, emit literal.
    ConstantReverse(Vec<ConstantReverseArm>),
    /// Enum variant dispatch.
    Dispatch {
        branches: Vec<AltBranch>,
    },
}

pub struct AltBranch {
    pub variant_name: String,
    pub plan: Box<EmitPlan>,
}

/// Repeat iteration.
pub enum RepeatPlan {
    Optional { inner: Box<EmitPlan> },
    SepBy { element: Box<EmitPlan>, separator: Vec<Frag> },
    Plain { element: Box<EmitPlan> },
}

/// Rule reference.
pub struct RefPlan {
    pub rule_name: String,
    pub strategy: RefStrategy,
}

pub enum RefStrategy {
    /// Match on enum variant, unwrap, call rule's emit fn.
    /// Used in Seq (Tuple index) and Repeat (iterator) contexts.
    Call,
    /// Call rule's emit fn directly (value is already the inner type).
    /// Used inside Alt match arms where the variant is already unwrapped.
    DirectCall,
    /// Transparent rule: inline body plan.
    Inline { body: Box<EmitPlan> },
}

/// Map reversal.
pub struct MapPlan {
    pub strategy: MapReverse,
    pub inner: Box<EmitPlan>,
}

/// Flat Vec from `(T, Vec<T))` collapse.
pub struct FlatVecPlan {
    pub item: Box<EmitPlan>,
    pub separator: Vec<Frag>,
}

// ═════════════════════════════════════════════════════════════════════════════
// Plan computation
// ═════════════════════════════════════════════════════════════════════════════

/// Compute the emission plan for a rule.
pub fn compute_rule_plan(rule: &bbnf_ir::IrRule, ir: &GrammarIR, ctx: &IrCodegenCtx) -> EmitPlan {
    compute_node_plan(&rule.body, ir, ctx)
}

/// Compute the emission plan for an IR node.
fn compute_node_plan(node: &IrNode, ir: &GrammarIR, ctx: &IrCodegenCtx) -> EmitPlan {
    match node {
        // ── Structural ───────────────────────────────────────────────
        IrNode::Literal(sid) => EmitPlan::Structural(literal_frags(*sid, ir)),
        IrNode::Epsilon => EmitPlan::Structural(vec![]),
        IrNode::Negate(_) => EmitPlan::Structural(vec![]),
        IrNode::OptionalWhitespace(inner) => compute_node_plan(inner, ir, ctx),

        // ── Value leaves ─────────────────────────────────────────────
        IrNode::Regex(_) => EmitPlan::Leaf(Leaf::SpanText),

        // ── Composites ───────────────────────────────────────────────
        IrNode::Seq(children) => compute_seq_plan(children, ir, ctx),
        IrNode::Alt(branches, _) => compute_alt_plan(branches, ir, ctx),
        IrNode::Repeat { inner, lo, hi } => compute_repeat_plan(inner, *lo, *hi, ir, ctx),

        // ── Ref ──────────────────────────────────────────────────────
        IrNode::Ref(rule_id) => compute_ref_plan(*rule_id, ir, ctx),

        // ── Binary ───────────────────────────────────────────────────
        IrNode::Skip(kept, structural) => {
            let k = compute_node_plan(kept, ir, ctx);
            let s = collect_structural(structural, ir);
            // Emit kept value, then structural suffix.
            EmitPlan::Seq(SeqPlan {
                children: vec![
                    SeqChild::Direct { plan: Box::new(k) },
                    SeqChild::Structural(s),
                ],
            })
        }
        IrNode::Next(structural, kept) => {
            let s = collect_structural(structural, ir);
            let k = compute_node_plan(kept, ir, ctx);
            EmitPlan::Seq(SeqPlan {
                children: vec![
                    SeqChild::Structural(s),
                    SeqChild::Direct { plan: Box::new(k) },
                ],
            })
        }
        IrNode::Minus(lhs, _) => compute_node_plan(lhs, ir, ctx),

        // ── Map ──────────────────────────────────────────────────────
        IrNode::Map { inner, fn_id } => {
            let strategy = decisions::decide_map(*fn_id, ir);
            let inner_plan = compute_node_plan(inner, ir, ctx);
            EmitPlan::Map(MapPlan {
                strategy,
                inner: Box::new(inner_plan),
            })
        }

        // ── TokenDispatch ────────────────────────────────────────────
        IrNode::TokenDispatch { .. } => EmitPlan::Leaf(Leaf::SpanText),
    }
}

// ── Seq ──────────────────────────────────────────────────────────────────────

fn compute_seq_plan(children: &[IrNode], ir: &GrammarIR, ctx: &IrCodegenCtx) -> EmitPlan {
    let decision = decisions::decide_seq(children, ir);

    // All-Span: pure structural.
    if decision.all_span {
        let frags: Vec<Frag> = children.iter()
            .flat_map(|c| collect_structural(c, ir))
            .collect();
        return EmitPlan::Structural(frags);
    }

    // Flatten: (T, Vec<T)) → flat Vec iteration.
    if let Some(flatten) = &decision.flatten {
        return compute_flat_vec_plan(children, &decision, flatten, ir, ctx);
    }

    // Tuple or single-value mixed.
    let is_tuple = matches!(&decision.result_type, TypeDesc::Tuple(_));
    let non_span_count = decision.child_types.iter()
        .filter(|t| **t != TypeDesc::Span)
        .count();

    let mut plan_children = Vec::with_capacity(children.len());

    if is_tuple {
        let tuple_len = match &decision.result_type {
            TypeDesc::Tuple(elems) => elems.len(),
            _ => unreachable!(),
        };

        if tuple_len == decision.child_types.len() {
            // 1:1 mapping. Use raw node_type to detect Options hidden by overrides.
            for (i, (child, ty)) in children.iter().zip(decision.child_types.iter()).enumerate() {
                let raw_ty = ir.type_map.as_ref()
                    .and_then(|tm| tm.node_type(child).cloned())
                    .unwrap_or(TypeDesc::Span);

                // Check BOTH the overridden type AND the raw type.
                // If the override says Span but the raw type is Option, use TupleValue.
                if *ty == TypeDesc::Span && !matches!(raw_ty, TypeDesc::Option(_)) {
                    plan_children.push(SeqChild::TupleSpan { index: i });
                } else {
                    let child_plan = compute_node_plan(child, ir, ctx);
                    plan_children.push(SeqChild::TupleValue {
                        index: i,
                        plan: Box::new(child_plan),
                    });
                }
            }
        } else {
            // Span compression: result Tuple has fewer elements than children.
            // Result Tuple elements are authoritative. Use the same grouping
            // strategy as the parse driver: consecutive Span children compress
            // into one Span group, non-Span children map 1:1.
            let result_elems = match &decision.result_type {
                TypeDesc::Tuple(elems) => elems.clone(),
                _ => unreachable!("is_tuple but result_type is not Tuple"),
            };

            // Build groups from child_types (matches project_seq_type's compression).
            let mut groups: Vec<(TypeDesc, Vec<usize>)> = Vec::new();
            let mut span_run: Vec<usize> = Vec::new();

            for (i, ty) in decision.child_types.iter().enumerate() {
                if *ty == TypeDesc::Span {
                    span_run.push(i);
                } else {
                    if !span_run.is_empty() {
                        groups.push((TypeDesc::Span, std::mem::take(&mut span_run)));
                    }
                    groups.push((ty.clone(), vec![i]));
                }
            }
            if !span_run.is_empty() {
                groups.push((TypeDesc::Span, span_run));
            }

            // Map groups to result Tuple elements.
            // If counts differ, compression alignment is off — fall back to
            // treating each result element as a TupleValue.
            if groups.len() == result_elems.len() {
                // Aligned: map groups to Tuple elements. The RESULT ELEMENT type
                // is authoritative (not the group type, which may have overrides).
                for (group_idx, (_group_type, child_indices)) in groups.iter().enumerate() {
                    let result_elem = &result_elems[group_idx];
                    // Check raw type of primary child to detect Options.
                    let ci0 = child_indices[0];
                    let raw_ty = ir.type_map.as_ref()
                        .and_then(|tm| tm.node_type(&children[ci0]).cloned())
                        .unwrap_or(TypeDesc::Span);

                    if *result_elem == TypeDesc::Span && !matches!(raw_ty, TypeDesc::Option(_)) {
                        // Span in result and raw: TupleSpan + structural.
                        for (j, &ci) in child_indices.iter().enumerate() {
                            if j == 0 {
                                plan_children.push(SeqChild::TupleSpan { index: group_idx });
                            } else {
                                plan_children.push(SeqChild::Structural(
                                    collect_structural(&children[ci], ir),
                                ));
                            }
                        }
                    } else {
                        // Non-Span OR Optional: emit as TupleValue.
                        let child_plan = compute_node_plan(&children[ci0], ir, ctx);
                        plan_children.push(SeqChild::TupleValue {
                            index: group_idx,
                            plan: Box::new(child_plan),
                        });
                    }
                }
            } else {
                // Misaligned: iterate result elements directly.
                // ALL non-Span result elements become TupleValue.
                // Span result elements become TupleSpan.
                let mut non_span_cursor = 0usize;
                let non_span_children: Vec<&IrNode> = children.iter()
                    .zip(decision.child_types.iter())
                    .filter(|(_, ty)| **ty != TypeDesc::Span)
                    .map(|(c, _)| c)
                    .collect();

                for (tuple_idx, elem_type) in result_elems.iter().enumerate() {
                    if *elem_type == TypeDesc::Span {
                        plan_children.push(SeqChild::TupleSpan { index: tuple_idx });
                    } else {
                        if non_span_cursor < non_span_children.len() {
                            let child = non_span_children[non_span_cursor];
                            let child_plan = compute_node_plan(child, ir, ctx);
                            plan_children.push(SeqChild::TupleValue {
                                index: tuple_idx,
                                plan: Box::new(child_plan),
                            });
                            non_span_cursor += 1;
                        }
                    }
                }
            }
        }
    } else if non_span_count == 1 {
        // Single value child: value IS the child directly.
        for (child, ty) in children.iter().zip(decision.child_types.iter()) {
            if *ty == TypeDesc::Span {
                plan_children.push(SeqChild::Structural(collect_structural(child, ir)));
            } else {
                let child_plan = compute_node_plan(child, ir, ctx);
                plan_children.push(SeqChild::Direct {
                    plan: Box::new(child_plan),
                });
            }
        }
    } else {
        // Multiple value children, non-Tuple result.
        let mut val_idx = 0;
        for (child, ty) in children.iter().zip(decision.child_types.iter()) {
            if *ty == TypeDesc::Span {
                plan_children.push(SeqChild::Structural(collect_structural(child, ir)));
            } else {
                let child_plan = compute_node_plan(child, ir, ctx);
                plan_children.push(SeqChild::TupleValue {
                    index: val_idx,
                    plan: Box::new(child_plan),
                });
                val_idx += 1;
            }
        }
    }

    EmitPlan::Seq(SeqPlan { children: plan_children })
}

fn compute_flat_vec_plan(
    children: &[IrNode],
    decision: &SeqDecision,
    flatten: &FlattenStrategy,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> EmitPlan {
    let repeat_idx = match flatten {
        FlattenStrategy::HeadThenVec => 1,
        FlattenStrategy::VecThenTail => 0,
    };
    let scalar_idx = 1 - repeat_idx;

    // The item plan: how to emit each element in the flat Vec.
    let item_plan = compute_vec_item_plan(&children[scalar_idx], ir, ctx);
    let separator = extract_repeat_separator(&children[repeat_idx], ir);

    EmitPlan::FlatVec(FlatVecPlan {
        item: Box::new(item_plan),
        separator,
    })
}

// ── Alt ──────────────────────────────────────────────────────────────────────

fn compute_alt_plan(branches: &[bbnf_ir::AltBranch], ir: &GrammarIR, ctx: &IrCodegenCtx) -> EmitPlan {
    // All-Span Alt: every branch produces Span (e.g., Alt of Literals).
    // No variant match needed — emit the value as Span text.
    let all_span = branches.iter().all(|b| {
        ir.type_map.as_ref()
            .and_then(|tm| tm.node_type(&b.node).cloned())
            .unwrap_or(TypeDesc::Span) == TypeDesc::Span
    });
    if all_span {
        return EmitPlan::Leaf(Leaf::SpanText);
    }

    let decision = decisions::decide_alt(branches, ir, &ctx.global_sub_variants);

    match decision.kind {
        AltDecisionKind::ConstantReverse(arms) => {
            EmitPlan::Alt(AltPlan::ConstantReverse(arms))
        }
        AltDecisionKind::Dispatch(branch_decisions) => {
            let plan_branches: Vec<AltBranch> = branch_decisions.iter().map(|bd| {
                let branch = &branches[bd.branch_index];
                match &bd.variant {
                    AltVariantKind::Transparent { rule_id } => {
                        // Transparent: no variant. Inline the rule body's alt plan.
                        // The transparent rule's body is typically an Alt or a single node.
                        let ref_rule = &ir.rules[*rule_id as usize];
                        // Recursively compute: the transparent body's Alt branches
                        // get flattened into the parent Alt.
                        // For now: create a pseudo-branch that inlines the body.
                        // This is handled in codegen by NOT matching a variant.
                        // We still need SOME variant name... Actually transparent
                        // rules DON'T have variants. We need to handle this differently.
                        // The transparent body's branches should be LIFTED into this Alt.
                        lift_transparent_branches(&ref_rule.body, ir, ctx)
                    }
                    AltVariantKind::RuleVariant { name, .. } => {
                        // Alt match already unwraps the variant → DirectCall.
                        vec![AltBranch {
                            variant_name: name.clone(),
                            plan: Box::new(EmitPlan::Ref(RefPlan {
                                rule_name: name.clone(),
                                strategy: RefStrategy::DirectCall,
                            })),
                        }]
                    }
                    AltVariantKind::SubVariant { name } => {
                        let plan = compute_node_plan(&branch.node, ir, ctx);
                        vec![AltBranch {
                            variant_name: name.clone(),
                            plan: Box::new(plan),
                        }]
                    }
                    AltVariantKind::Direct => {
                        // Enum/BoxedEnum — the branch produces the enum directly.
                        // Lift its inner structure (same as transparent).
                        lift_transparent_branches(&branch.node, ir, ctx)
                    }
                }
            }).flatten().collect();

            EmitPlan::Alt(AltPlan::Dispatch { branches: plan_branches })
        }
    }
}

/// Lift a transparent rule's body branches into the parent Alt.
fn lift_transparent_branches(body: &IrNode, ir: &GrammarIR, ctx: &IrCodegenCtx) -> Vec<AltBranch> {
    match body {
        IrNode::Alt(branches, _) => {
            // Recursively compute Alt plan for the transparent body, then extract branches.
            let alt_plan = compute_alt_plan(branches, ir, ctx);
            match alt_plan {
                EmitPlan::Alt(AltPlan::Dispatch { branches }) => branches,
                EmitPlan::Alt(AltPlan::ConstantReverse(_)) => {
                    // Constant reverse can't be lifted — wrap in a single pseudo-branch.
                    vec![AltBranch {
                        variant_name: "__constant".to_string(),
                        plan: Box::new(alt_plan),
                    }]
                }
                other => vec![AltBranch {
                    variant_name: "__transparent".to_string(),
                    plan: Box::new(other),
                }],
            }
        }
        // Non-Alt transparent body: single branch.
        IrNode::Ref(rule_id) => {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                lift_transparent_branches(&ref_rule.body, ir, ctx)
            } else {
                let name = ir.get_string(ref_rule.name).to_string();
                vec![AltBranch {
                    variant_name: name.clone(),
                    plan: Box::new(EmitPlan::Ref(RefPlan {
                        rule_name: name,
                        strategy: RefStrategy::DirectCall,
                    })),
                }]
            }
        }
        _ => {
            // Leaf transparent body (Literal, Map, etc.). Look up the sub-variant
            // for its type in global_sub_variants.
            let ty = ir.type_map.as_ref()
                .and_then(|tm| tm.node_type(body).cloned())
                .unwrap_or(TypeDesc::Span);
            let variant_name = ctx.global_sub_variants.get(&ty)
                .or_else(|| {
                    let normalized = match &ty {
                        TypeDesc::BoxedEnum => &TypeDesc::Enum,
                        other => other,
                    };
                    ctx.global_sub_variants.get(normalized)
                })
                .cloned();
            if let Some(name) = variant_name {
                let plan = compute_node_plan(body, ir, ctx);
                vec![AltBranch { variant_name: name, plan: Box::new(plan) }]
            } else {
                // No variant found — this node's type matches no sub-variant.
                // Skip (the value is emitted via another branch).
                vec![]
            }
        }
    }
}

// ── Repeat ───────────────────────────────────────────────────────────────────

fn compute_repeat_plan(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> EmitPlan {
    let decision = decisions::decide_repeat(inner, lo, hi, ir);

    match decision.kind {
        RepeatKind::Optional => {
            let inner_plan = compute_node_plan(inner, ir, ctx);
            EmitPlan::Repeat(RepeatPlan::Optional { inner: Box::new(inner_plan) })
        }
        RepeatKind::SepBy => {
            let (elem, sep) = decisions::detect_sep_by(inner).unwrap();
            let elem_plan = compute_vec_item_plan(elem, ir, ctx);
            let sep_frags = collect_structural(sep, ir);
            EmitPlan::Repeat(RepeatPlan::SepBy {
                element: Box::new(elem_plan),
                separator: sep_frags,
            })
        }
        RepeatKind::Plain => {
            let item_plan = compute_vec_item_plan(inner, ir, ctx);
            EmitPlan::Repeat(RepeatPlan::Plain { element: Box::new(item_plan) })
        }
    }
}

// ── Ref ──────────────────────────────────────────────────────────────────────

fn compute_ref_plan(rule_id: u32, ir: &GrammarIR, ctx: &IrCodegenCtx) -> EmitPlan {
    let ref_rule = &ir.rules[rule_id as usize];
    if ref_rule.meta.is_transparent {
        EmitPlan::Ref(RefPlan {
            rule_name: ir.get_string(ref_rule.name).to_string(),
            strategy: RefStrategy::Inline {
                body: Box::new(compute_node_plan(&ref_rule.body, ir, ctx)),
            },
        })
    } else {
        let rule_type = ctx.rule_types.get(&ref_rule.id)
            .cloned()
            .unwrap_or_else(|| ctx.enum_type.clone());
        EmitPlan::Ref(RefPlan {
            rule_name: ir.get_string(ref_rule.name).to_string(),
            strategy: RefStrategy::Call,
        })
    }
}

// ── Vec item emission plan ───────────────────────────────────────────────────

/// Compute plan for a single Vec item, handling enum unwrapping for Refs.
fn compute_vec_item_plan(node: &IrNode, ir: &GrammarIR, ctx: &IrCodegenCtx) -> EmitPlan {
    // Direct Ref to non-transparent rule: Vec stores enum variants → unwrap.
    if let IrNode::Ref(rule_id) = node {
        let ref_rule = &ir.rules[*rule_id as usize];
        if !ref_rule.meta.is_transparent {
            let name = ir.get_string(ref_rule.name).to_string();
            let rule_type = ctx.rule_types.get(&ref_rule.id)
                .cloned()
                .unwrap_or_else(|| ctx.enum_type.clone());
            return EmitPlan::Ref(RefPlan {
                rule_name: name.clone(),
                strategy: RefStrategy::Call,
            });
        }
    }
    // Next(structural, Ref): emit structural, then unwrap Ref.
    if let IrNode::Next(structural, kept) = node {
        if let IrNode::Ref(rule_id) = kept.as_ref() {
            let ref_rule = &ir.rules[*rule_id as usize];
            if !ref_rule.meta.is_transparent {
                let s = collect_structural(structural, ir);
                let name = ir.get_string(ref_rule.name).to_string();
                let rule_type = ctx.rule_types.get(&ref_rule.id)
                    .cloned()
                    .unwrap_or_else(|| ctx.enum_type.clone());
                let ref_plan = EmitPlan::Ref(RefPlan {
                    rule_name: name.clone(),
                    strategy: RefStrategy::Call,
                });
                return EmitPlan::Seq(SeqPlan {
                    children: vec![
                        SeqChild::Structural(s),
                        SeqChild::Direct { plan: Box::new(ref_plan) },
                    ],
                });
            }
        }
    }
    // Skip(Ref, structural): unwrap Ref, then emit structural.
    if let IrNode::Skip(kept, structural) = node {
        if let IrNode::Ref(rule_id) = kept.as_ref() {
            let ref_rule = &ir.rules[*rule_id as usize];
            if !ref_rule.meta.is_transparent {
                let name = ir.get_string(ref_rule.name).to_string();
                let s = collect_structural(structural, ir);
                let ref_plan = EmitPlan::Ref(RefPlan {
                    rule_name: name.clone(),
                    strategy: RefStrategy::Call,
                });
                return EmitPlan::Seq(SeqPlan {
                    children: vec![
                        SeqChild::Direct { plan: Box::new(ref_plan) },
                        SeqChild::Structural(s),
                    ],
                });
            }
        }
    }
    compute_node_plan(node, ir, ctx)
}

// ═════════════════════════════════════════════════════════════════════════════
// Structural helpers
// ═════════════════════════════════════════════════════════════════════════════

fn literal_frags(sid: u32, ir: &GrammarIR) -> Vec<Frag> {
    let s = ir.get_string(sid);
    if s.len() == 1 {
        vec![Frag::Char(s.as_bytes()[0])]
    } else {
        vec![Frag::Text(s.to_string())]
    }
}

/// Collect structural content from an IR node tree.
pub fn collect_structural(node: &IrNode, ir: &GrammarIR) -> Vec<Frag> {
    match node {
        IrNode::Literal(sid) => literal_frags(*sid, ir),
        IrNode::Ref(rule_id) => collect_structural(&ir.rules[*rule_id as usize].body, ir),
        IrNode::Epsilon | IrNode::Negate(_) => vec![],
        IrNode::OptionalWhitespace(inner) => collect_structural(inner, ir),
        IrNode::Seq(children) => children.iter().flat_map(|c| collect_structural(c, ir)).collect(),
        IrNode::Skip(l, _) | IrNode::Next(_, l) => collect_structural(l, ir),
        IrNode::Repeat { inner, .. } => collect_structural(inner, ir),
        _ => vec![],
    }
}

fn extract_repeat_separator(node: &IrNode, ir: &GrammarIR) -> Vec<Frag> {
    if let IrNode::Repeat { inner, .. } = node {
        if let IrNode::Next(sep, _) = inner.as_ref() {
            return collect_structural(sep, ir);
        }
        if let IrNode::Skip(_, opt) = inner.as_ref() {
            if let IrNode::Repeat { inner: sep, lo: 0, hi: 1 } = opt.as_ref() {
                return collect_structural(sep, ir);
            }
        }
    }
    vec![]
}
