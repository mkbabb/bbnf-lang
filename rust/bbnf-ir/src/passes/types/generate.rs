//! CSP constraint generation from IR structure.
//!
//! Walks each rule's body and emits type constraints per node.
//! The constraints form a directed graph that the AC-3 solver propagates.
//!
//! Two type variables are created per node:
//! - **Primary**: The node's type in normal context (project_node)
//! - **Vec-context**: The node's type when used as a Vec element (project_node_in_vec)
//!
//! Seq metadata (child types, preserve_spans) is recorded during generation
//! for later export to the TypeMap.

use std::collections::HashMap;

use crate::{FnDescriptor, GrammarIR, IrNode, RuleId, TypeDesc};

use super::constraint::*;

/// Result of constraint generation: variables + constraints + rule→var mapping.
pub struct ConstraintSystem {
    pub vars: Vec<TypeVar>,
    pub constraints: Vec<TypeConstraint>,
    /// Maps RuleId → TypeVarId for rule-level type variables.
    pub rule_vars: HashMap<RuleId, TypeVarId>,
    /// Maps node_id (pointer) → TypeVarId for normal-context type variables.
    pub node_vars: HashMap<usize, TypeVarId>,
    /// Maps node_id (pointer) → TypeVarId for vec-context type variables.
    pub vec_context_vars: HashMap<usize, TypeVarId>,
    /// Per-Seq metadata: children pointer → (child var IDs, preserve_spans).
    pub seq_metadata: HashMap<usize, SeqMetadata>,
}

/// Metadata recorded during constraint generation for Seq nodes.
pub struct SeqMetadata {
    /// TypeVarIds for each child in the Seq (normal context).
    pub child_vars: Vec<TypeVarId>,
    /// Whether preserve_spans was set for this Seq.
    pub preserve_spans: bool,
}

/// Generate the constraint system from a GrammarIR.
pub fn generate_constraints(ir: &GrammarIR) -> ConstraintSystem {
    let mut cg = ConstraintGenerator {
        vars: Vec::new(),
        constraints: Vec::new(),
        rule_vars: HashMap::new(),
        node_vars: HashMap::new(),
        vec_context_vars: HashMap::new(),
        seq_metadata: HashMap::new(),
        ir,
    };

    // Phase 1: Allocate a type variable for each rule.
    for rule in &ir.rules {
        let var = cg.new_var(rule.id as usize);
        cg.rule_vars.insert(rule.id, var);
    }

    // Phase 2: Generate constraints from each rule's body.
    for rule in &ir.rules {
        let rule_var = cg.rule_vars[&rule.id];
        let preserve_spans = rule.meta.directives.pretty.is_some();
        let body_var = cg.generate_node(&rule.body, preserve_spans);

        // Rule's type = its body's type.
        cg.constraints.push(TypeConstraint::Equal {
            target: rule_var,
            source: body_var,
        });
    }

    ConstraintSystem {
        vars: cg.vars,
        constraints: cg.constraints,
        rule_vars: cg.rule_vars,
        node_vars: cg.node_vars,
        vec_context_vars: cg.vec_context_vars,
        seq_metadata: cg.seq_metadata,
    }
}

struct ConstraintGenerator<'a> {
    vars: Vec<TypeVar>,
    constraints: Vec<TypeConstraint>,
    rule_vars: HashMap<RuleId, TypeVarId>,
    node_vars: HashMap<usize, TypeVarId>,
    vec_context_vars: HashMap<usize, TypeVarId>,
    seq_metadata: HashMap<usize, SeqMetadata>,
    ir: &'a GrammarIR,
}

impl<'a> ConstraintGenerator<'a> {
    fn new_var(&mut self, node_id: usize) -> TypeVarId {
        let id = self.vars.len() as TypeVarId;
        self.vars.push(TypeVar {
            solved: None,
            node_id,
        });
        id
    }

    /// Generate constraints for an IR node, returning its primary type variable.
    ///
    /// Also generates the vec-context variable and records it in `vec_context_vars`.
    fn generate_node(&mut self, node: &IrNode, preserve_spans: bool) -> TypeVarId {
        let node_id = node as *const IrNode as usize;
        let var = self.new_var(node_id);
        self.node_vars.insert(node_id, var);

        // Create vec-context variable for this node.
        let vec_var = self.new_var(node_id);
        self.vec_context_vars.insert(node_id, vec_var);

        match node {
            // Leaf constraints — ground types
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {
                self.constraints.push(TypeConstraint::Ground {
                    var,
                    ty: TypeDesc::Span,
                });
                // Vec context: same as normal for leaves.
                self.constraints.push(TypeConstraint::Ground {
                    var: vec_var,
                    ty: TypeDesc::Span,
                });
            }

            // Reference constraint
            IrNode::Ref(_rule_id) => {
                // Normal context: Ref always produces BoxedEnum.
                self.constraints.push(TypeConstraint::Ground {
                    var,
                    ty: TypeDesc::BoxedEnum,
                });
                // Vec context: Ref produces Enum (Vec provides heap indirection).
                // ALL refs return Enum in Vec context -- transparent refs included.
                // The old code had `project_node_in_vec` always return Enum for Ref.
                self.constraints.push(TypeConstraint::Ground {
                    var: vec_var,
                    ty: TypeDesc::Enum,
                });
            }

            // Sequence constraint — Tuple of children
            IrNode::Seq(children) => {
                // Span-method override: Ref children to rules with has_sp_method (and
                // not transparent) get overridden to Span in Seq context.
                let mut child_vars = Vec::with_capacity(children.len());
                let mut sp_override_originals = Vec::with_capacity(children.len());
                let mut child_node_kinds = Vec::with_capacity(children.len());

                for c in children {
                    let is_sp_override = if let IrNode::Ref(id) = c {
                        let rule = &self.ir.rules[*id as usize];
                        rule.meta.has_sp_method && !rule.meta.is_transparent
                    } else {
                        false
                    };

                    let child_kind = if is_sp_override {
                        SeqChildKind::SpOverrideRef
                    } else if let IrNode::Repeat { lo: 0, hi: 1, .. } = c {
                        SeqChildKind::Optional
                    } else {
                        SeqChildKind::Other
                    };

                    if is_sp_override {
                        // Create an override variable set to Span for the child.
                        let override_var = self.new_var(c as *const IrNode as usize);
                        self.constraints.push(TypeConstraint::Ground {
                            var: override_var,
                            ty: TypeDesc::Span,
                        });
                        // Also generate the real child variable for safety guard revert.
                        let real_var = self.generate_node(c, false);
                        sp_override_originals.push(Some(real_var));
                        child_vars.push(override_var);
                    } else {
                        let child_var = self.generate_node(c, false);
                        sp_override_originals.push(None);
                        child_vars.push(child_var);
                    }

                    child_node_kinds.push(child_kind);
                }

                // Record seq metadata for TypeMap export.
                let seq_ptr = children.as_ptr() as usize;
                self.seq_metadata.insert(
                    seq_ptr,
                    SeqMetadata {
                        child_vars: child_vars.clone(),
                        preserve_spans,
                    },
                );

                self.constraints.push(TypeConstraint::Seq {
                    var,
                    children: child_vars,
                    preserve_spans,
                    sp_override_originals,
                    collapse_simple_spans: self.ir.collapse_simple_spans,
                    child_node_kinds,
                });

                // Vec context for a Seq: delegate to normal projection.
                // Seq in Vec context always uses project_node (not in_vec).
                self.constraints.push(TypeConstraint::Equal {
                    target: vec_var,
                    source: var,
                });
            }

            // Alternation constraint — join of branches
            IrNode::Alt(branches, _dispatch) => {
                let branch_vars: Vec<TypeVarId> = branches
                    .iter()
                    .map(|b| self.generate_node(&b.node, false))
                    .collect();
                self.constraints.push(TypeConstraint::Alt {
                    var,
                    branches: branch_vars.clone(),
                });

                // Vec context for Alt: try in_vec projection for branches.
                // Collect vec-context variables for each branch.
                let vec_branch_vars: Vec<TypeVarId> = branches
                    .iter()
                    .map(|b| {
                        let b_id = &b.node as *const IrNode as usize;
                        self.vec_context_vars[&b_id]
                    })
                    .collect();

                self.constraints.push(TypeConstraint::AltInVec {
                    var: vec_var,
                    branches: vec_branch_vars,
                    standard_var: var,
                });
            }

            // Repetition constraint
            IrNode::Repeat { inner, lo, hi } => {
                let inner_var = self.generate_node(inner, false);
                let inner_id = inner.as_ref() as *const IrNode as usize;

                if *lo == 0 && *hi == 1 {
                    // Optional — check if inner is a transparent ref.
                    let transparent_ref = if let IrNode::Ref(rule_id) = inner.as_ref() {
                        self.ir.rules[*rule_id as usize].meta.is_transparent
                    } else {
                        false
                    };

                    self.constraints.push(TypeConstraint::Optional {
                        var,
                        inner: inner_var,
                        transparent_ref,
                    });

                    // Vec context for Optional: delegate to normal.
                    self.constraints.push(TypeConstraint::Equal {
                        target: vec_var,
                        source: var,
                    });
                } else {
                    // Many / Many1: use the vec-context variable for inner elements.
                    let inner_vec_var = self.vec_context_vars[&inner_id];
                    self.constraints.push(TypeConstraint::Repeat {
                        var,
                        inner: inner_vec_var,
                    });

                    // Vec context for Repeat: delegate to normal.
                    self.constraints.push(TypeConstraint::Equal {
                        target: vec_var,
                        source: var,
                    });
                }
            }

            // Operator constraints — keep one side
            IrNode::Skip(a, b) => {
                let a_var = self.generate_node(a, false);
                let _b_var = self.generate_node(b, false);
                self.constraints.push(TypeConstraint::Project {
                    var,
                    kept: a_var,
                });

                // Vec context: Skip propagates in_vec to the kept side.
                let a_id = a.as_ref() as *const IrNode as usize;
                let a_vec_var = self.vec_context_vars[&a_id];
                self.constraints.push(TypeConstraint::Project {
                    var: vec_var,
                    kept: a_vec_var,
                });
            }
            IrNode::Next(a, b) => {
                let _a_var = self.generate_node(a, false);
                let b_var = self.generate_node(b, false);
                self.constraints.push(TypeConstraint::Project {
                    var,
                    kept: b_var,
                });

                // Vec context: Next propagates in_vec to the kept side.
                let b_id = b.as_ref() as *const IrNode as usize;
                let b_vec_var = self.vec_context_vars[&b_id];
                self.constraints.push(TypeConstraint::Project {
                    var: vec_var,
                    kept: b_vec_var,
                });
            }
            IrNode::Minus(a, b) => {
                let a_var = self.generate_node(a, false);
                let _b_var = self.generate_node(b, false);
                self.constraints.push(TypeConstraint::Project {
                    var,
                    kept: a_var,
                });

                // Vec context: Minus propagates in_vec to the kept side.
                let a_id = a.as_ref() as *const IrNode as usize;
                let a_vec_var = self.vec_context_vars[&a_id];
                self.constraints.push(TypeConstraint::Project {
                    var: vec_var,
                    kept: a_vec_var,
                });
            }

            // Negate — always produces empty tuple.
            IrNode::Negate(inner) => {
                // Generate inner for side effects (its sub-tree needs vars).
                let _inner_var = self.generate_node(inner, false);
                self.constraints.push(TypeConstraint::Ground {
                    var,
                    ty: TypeDesc::Tuple(vec![]),
                });
                // Vec context: same.
                self.constraints.push(TypeConstraint::Ground {
                    var: vec_var,
                    ty: TypeDesc::Tuple(vec![]),
                });
            }

            // OptionalWhitespace — transparent
            IrNode::OptionalWhitespace(inner) => {
                let inner_var = self.generate_node(inner, preserve_spans);
                self.constraints.push(TypeConstraint::Equal {
                    target: var,
                    source: inner_var,
                });

                // Vec context: propagates through.
                let inner_id = inner.as_ref() as *const IrNode as usize;
                let inner_vec_var = self.vec_context_vars[&inner_id];
                self.constraints.push(TypeConstraint::Equal {
                    target: vec_var,
                    source: inner_vec_var,
                });
            }

            // Map constraint — type comes from FnDescriptor
            IrNode::Map { inner, fn_id } => {
                let _inner_var = self.generate_node(inner, false);
                let fn_desc = &self.ir.fns[*fn_id as usize];
                let map_type = match fn_desc {
                    FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                    FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                    FnDescriptor::NumberConvert => TypeDesc::F64,
                    FnDescriptor::HexConvert { .. } => TypeDesc::U32,
                    FnDescriptor::SpanCapture => TypeDesc::Span,
                    FnDescriptor::Expr { return_type, .. } => {
                        return_type.clone().unwrap_or(TypeDesc::Span)
                    }
                };
                self.constraints.push(TypeConstraint::Map {
                    var,
                    return_type: map_type.clone(),
                });
                // Vec context for Map: same type (Map determines its own type).
                self.constraints.push(TypeConstraint::Map {
                    var: vec_var,
                    return_type: map_type,
                });
            }

            // TokenDispatch — join of arm types + fallback
            IrNode::TokenDispatch {
                token: _,
                arms,
                fallback,
            } => {
                let mut branch_vars = Vec::new();
                for arm in arms {
                    let cont_var = self.generate_node(&arm.continuation, false);
                    // If arm has a map function, apply it
                    if let Some(map_fn_id) = arm.map_fn {
                        let mapped_var = self.new_var(0);
                        let fn_desc = &self.ir.fns[map_fn_id as usize];
                        let map_type = match fn_desc {
                            FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                            FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                            FnDescriptor::NumberConvert => TypeDesc::F64,
                            FnDescriptor::HexConvert { .. } => TypeDesc::U32,
                            FnDescriptor::SpanCapture => TypeDesc::Span,
                            FnDescriptor::Expr { return_type, .. } => {
                                return_type.clone().unwrap_or(TypeDesc::Span)
                            }
                        };
                        self.constraints.push(TypeConstraint::Map {
                            var: mapped_var,
                            return_type: map_type,
                        });
                        branch_vars.push(mapped_var);
                    } else {
                        branch_vars.push(cont_var);
                    }
                }
                let fallback_var = self.generate_node(fallback, false);
                branch_vars.push(fallback_var);

                self.constraints.push(TypeConstraint::Alt {
                    var,
                    branches: branch_vars,
                });

                // Vec context for TokenDispatch: delegate to normal.
                self.constraints.push(TypeConstraint::Equal {
                    target: vec_var,
                    source: var,
                });
            }
        }

        var
    }
}
