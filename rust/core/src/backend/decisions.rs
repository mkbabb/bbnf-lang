//! Shared decision layer: pure type-resolution functions.
//!
//! These functions query the TypeMap to resolve structural decisions about IR nodes.
//! Both the parse driver (`driver.rs`) and the emit plan builder (`generate/emit/`)
//! consume these decisions — one source of truth for all type classification.
//!
//! ## Design
//!
//! Each `decide_*` function takes an IR node (or slice) + the GrammarIR (for TypeMap
//! access) + parent context (ValuePlacement) and returns a decision struct describing
//! the resolved types, child classification, and structural strategy.

use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, MapExpr, RuleId, TypeDesc};

use super::types::{FlattenStrategy, ValuePlacement};

// ═════════════════════════════════════════════════════════════════════════════
// Decision types
// ═════════════════════════════════════════════════════════════════════════════

/// Resolved Seq decomposition.
#[derive(Debug)]
pub struct SeqDecision {
    /// Post-compression, post-flatten result type.
    pub result_type: TypeDesc,
    /// All children are Span → pure structural.
    pub all_span: bool,
    /// Per-child resolved types (post Tuple-element override).
    pub child_types: Vec<TypeDesc>,
    /// Flatten strategy for `(T, Vec<T>) → Vec<T)`.
    pub flatten: Option<FlattenStrategy>,
}

/// Per-child alloc decision (computed from the child's TypeDesc + parent alloc).
pub fn child_alloc(ty: &TypeDesc, parent_alloc: ValuePlacement) -> ValuePlacement {
    match ty {
        TypeDesc::BoxedEnum => ValuePlacement::Alloc,
        TypeDesc::Enum if parent_alloc == ValuePlacement::Alloc => ValuePlacement::Alloc,
        TypeDesc::Vec(inner) if **inner != TypeDesc::Span => ValuePlacement::Alloc,
        _ => ValuePlacement::Inline,
    }
}

/// Resolved Alt decomposition.
#[derive(Debug)]
pub struct AltDecision {
    pub kind: AltDecisionKind,
}

#[derive(Debug)]
pub enum AltDecisionKind {
    /// All branches are `Map(Literal, Expr(constant))` → value-based match.
    ConstantReverse(Vec<ConstantReverseArm>),
    /// Enum variant dispatch.
    Dispatch(Vec<AltBranchDecision>),
}

/// A constant-reverse arm: literal text → constant value.
#[derive(Debug)]
pub struct ConstantReverseArm {
    pub literal: String,
    pub expr: ConstantKind,
}

#[derive(Debug)]
pub enum ConstantKind {
    Bool(bool),
    Int(i64),
    Float(f64),
}

/// A resolved Alt branch.
#[derive(Debug)]
pub struct AltBranchDecision {
    pub branch_index: usize,
    pub ty: TypeDesc,
    pub variant: AltVariantKind,
}

/// How an Alt branch maps to an enum variant.
#[derive(Debug)]
pub enum AltVariantKind {
    /// Ref to non-transparent rule → variant name = rule name.
    RuleVariant { rule_id: RuleId, name: String },
    /// Ref to transparent rule → no enum variant; inline body.
    Transparent { rule_id: RuleId },
    /// Sub-variant from global_sub_variants (heterogeneous non-Ref branch).
    SubVariant { name: String },
    /// Enum/BoxedEnum branch with no sub-variant wrapping.
    Direct,
}

/// Resolved Repeat decomposition.
#[derive(Debug)]
pub struct RepeatDecision {
    pub kind: RepeatKind,
    pub elem_type: TypeDesc,
}

#[derive(Debug)]
pub enum RepeatKind {
    Optional,
    SepBy,
    Plain,
}

/// Resolved Map reversal.
#[derive(Debug, Clone)]
pub enum MapReverse {
    /// NumberConvert → emit f64.
    F64,
    /// HexConvert → emit hex format.
    Hex,
    /// SpanCapture → emit span text.
    SpanText,
    /// EnumWrap / BoxWrap → transparent (emit inner).
    Passthrough,
    /// Constant literal → emit the matched literal from IR.
    Constant,
    /// General Expr → Display fallback.
    Display,
}

// ═════════════════════════════════════════════════════════════════════════════
// Decision functions
// ═════════════════════════════════════════════════════════════════════════════

/// Resolve Seq type decomposition from TypeMap.
///
/// Mirrors the decision logic in `driver.rs:compile_seq` lines 375-475.
pub fn decide_seq(children: &[IrNode], ir: &GrammarIR) -> SeqDecision {
    let type_map = ir.type_map.as_ref();

    // Step 1: child types from seq_child_types (pointer-based lookup).
    let child_types: Vec<TypeDesc> = type_map
        .and_then(|tm| {
            tm.seq_child_types_by_ptr(children.as_ptr() as usize)
                .map(|s| s.to_vec())
        })
        .unwrap_or_else(|| {
            children
                .iter()
                .map(|c| {
                    type_map
                        .and_then(|tm| tm.node_type(c).cloned())
                        .unwrap_or(TypeDesc::Span)
                })
                .collect()
        });

    // Step 2: result type.
    let result_type = type_map
        .and_then(|tm| tm.seq_result_type(children.as_ptr() as usize).cloned())
        .unwrap_or(TypeDesc::Span);

    // Step 3: all-Span check.
    let all_span = match &result_type {
        TypeDesc::Span => true,
        _ if child_types.iter().all(|t| *t == TypeDesc::Span) && !type_map.is_some() => true,
        _ => result_type == TypeDesc::Span,
    };

    // Step 4: override child types from result Tuple elements.
    let child_types = if let TypeDesc::Tuple(ref result_elems) = result_type {
        if result_elems.len() == child_types.len() {
            result_elems.clone()
        } else {
            child_types
        }
    } else {
        child_types
    };

    // Step 5: flatten detection.
    let flatten = detect_flatten(&result_type, &child_types);

    SeqDecision {
        result_type,
        all_span,
        child_types,
        flatten,
    }
}

/// Resolve Alt variant assignment.
///
/// Mirrors decision logic in `driver.rs:compile_alt`.
pub fn decide_alt(
    branches: &[AltBranch],
    ir: &GrammarIR,
    global_sub_variants: &std::collections::HashMap<TypeDesc, String>,
) -> AltDecision {
    // Check constant-reverse.
    if let Some(arms) = try_constant_reverse(branches, ir) {
        return AltDecision {
            kind: AltDecisionKind::ConstantReverse(arms),
        };
    }

    let type_map = ir.type_map.as_ref();

    let decisions: Vec<AltBranchDecision> = branches
        .iter()
        .enumerate()
        .map(|(i, branch)| {
            let ty = type_map
                .and_then(|tm| tm.node_type(&branch.node).cloned())
                .unwrap_or(TypeDesc::Span);

            let variant = match &branch.node {
                IrNode::Ref(rule_id) => {
                    let ref_rule = &ir.rules[*rule_id as usize];
                    if ref_rule.meta.is_transparent {
                        AltVariantKind::Transparent { rule_id: *rule_id }
                    } else {
                        let name = ir.get_string(ref_rule.name).to_string();
                        AltVariantKind::RuleVariant {
                            rule_id: *rule_id,
                            name,
                        }
                    }
                }
                _ => {
                    // Non-Ref branch: look up sub-variant by type.
                    if let Some(name) = global_sub_variants.get(&ty) {
                        AltVariantKind::SubVariant {
                            name: name.clone(),
                        }
                    } else {
                        // Normalize BoxedEnum → Enum for lookup.
                        let normalized = match &ty {
                            TypeDesc::BoxedEnum => TypeDesc::Enum,
                            other => other.clone(),
                        };
                        if let Some(name) = global_sub_variants.get(&normalized) {
                            AltVariantKind::SubVariant {
                                name: name.clone(),
                            }
                        } else {
                            AltVariantKind::Direct
                        }
                    }
                }
            };

            AltBranchDecision {
                branch_index: i,
                ty,
                variant,
            }
        })
        .collect();

    AltDecision {
        kind: AltDecisionKind::Dispatch(decisions),
    }
}

/// Resolve Repeat element type and structure.
pub fn decide_repeat(inner: &IrNode, lo: u32, hi: u32, ir: &GrammarIR) -> RepeatDecision {
    let type_map = ir.type_map.as_ref();

    if lo == 0 && hi == 1 {
        let elem_type = type_map
            .and_then(|tm| tm.node_type(inner).cloned())
            .unwrap_or(TypeDesc::Span);
        return RepeatDecision {
            kind: RepeatKind::Optional,
            elem_type,
        };
    }

    let is_sep_by = detect_sep_by(inner).is_some();

    let elem_type = type_map
        .and_then(|tm| tm.vec_elem_type(inner).cloned())
        .or_else(|| {
            type_map.and_then(|tm| {
                let ty = tm.node_type(inner).cloned()?;
                Some(if ty == TypeDesc::BoxedEnum {
                    TypeDesc::Enum
                } else {
                    ty
                })
            })
        })
        .unwrap_or_else(|| match inner {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => TypeDesc::Span,
            IrNode::Ref(_) => TypeDesc::Enum,
            _ => TypeDesc::BoxedEnum,
        });

    RepeatDecision {
        kind: if is_sep_by {
            RepeatKind::SepBy
        } else {
            RepeatKind::Plain
        },
        elem_type,
    }
}

/// Resolve Map FnDescriptor reversal.
pub fn decide_map(fn_id: u32, ir: &GrammarIR) -> MapReverse {
    match &ir.fns[fn_id as usize] {
        FnDescriptor::NumberConvert => MapReverse::F64,
        FnDescriptor::HexConvert { .. } => MapReverse::Hex,
        FnDescriptor::SpanCapture => MapReverse::SpanText,
        FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => MapReverse::Passthrough,
        FnDescriptor::Expr { expr, .. } => match expr {
            MapExpr::IntLit(_) | MapExpr::FloatLit(_) | MapExpr::StringLit(_)
            | MapExpr::BoolLit(_) | MapExpr::Input => MapReverse::Constant,
            _ => MapReverse::Display,
        },
    }
}

// ═════════════════════════════════════════════════════════════════════════════
// Helpers (moved from driver.rs)
// ═════════════════════════════════════════════════════════════════════════════

/// Detect sep_by pattern: `Skip(element, Repeat(separator, 0, 1))`.
pub fn detect_sep_by(inner: &IrNode) -> Option<(&IrNode, &IrNode)> {
    if let IrNode::Skip(element, opt_sep) = inner {
        if let IrNode::Repeat {
            inner: separator,
            lo: 0,
            hi: 1,
        } = opt_sep.as_ref()
        {
            return Some((element.as_ref(), separator.as_ref()));
        }
    }
    None
}

/// Detect `(T, Vec<T>)` or `(Vec<T>, T)` flattening.
pub fn detect_flatten(
    result_type: &TypeDesc,
    child_types: &[TypeDesc],
) -> Option<FlattenStrategy> {
    let TypeDesc::Vec(_) = result_type else {
        return None;
    };
    if child_types.len() != 2 {
        return None;
    }
    match (&child_types[0], &child_types[1]) {
        (_, TypeDesc::Vec(_)) => Some(FlattenStrategy::HeadThenVec),
        (TypeDesc::Vec(_), _) => Some(FlattenStrategy::VecThenTail),
        _ => None,
    }
}

/// Check if all Alt branches are Map(Literal, Expr(constant)).
fn try_constant_reverse(branches: &[AltBranch], ir: &GrammarIR) -> Option<Vec<ConstantReverseArm>> {
    let mut arms = Vec::new();
    for branch in branches {
        let IrNode::Map { inner, fn_id } = &branch.node else {
            return None;
        };
        let IrNode::Literal(sid) = inner.as_ref() else {
            return None;
        };
        let FnDescriptor::Expr { expr, .. } = &ir.fns[*fn_id as usize] else {
            return None;
        };
        let lit = ir.get_string(*sid).to_string();
        let kind = match expr {
            MapExpr::BoolLit(b) => ConstantKind::Bool(*b),
            MapExpr::IntLit(n) => ConstantKind::Int(*n),
            MapExpr::FloatLit(f) => ConstantKind::Float(*f),
            _ => return None,
        };
        arms.push(ConstantReverseArm { literal: lit, expr: kind });
    }
    Some(arms)
}
