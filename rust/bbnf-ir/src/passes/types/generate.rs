//! CSP constraint generation from IR structure.
//!
//! Walks each rule's body and emits type constraints per node.
//! The constraints form a directed graph that the AC-3 solver propagates.

use std::collections::HashMap;

use crate::{FnDescriptor, GrammarIR, IrNode, RuleId, TypeDesc};

use super::constraint::*;

/// Result of constraint generation: variables + constraints + rule→var mapping.
pub struct ConstraintSystem {
    pub vars: Vec<TypeVar>,
    pub constraints: Vec<TypeConstraint>,
    /// Maps RuleId → TypeVarId for rule-level type variables.
    pub rule_vars: HashMap<RuleId, TypeVarId>,
}

/// Generate the constraint system from a GrammarIR.
pub fn generate_constraints(ir: &GrammarIR) -> ConstraintSystem {
    let mut cg = ConstraintGenerator {
        vars: Vec::new(),
        constraints: Vec::new(),
        rule_vars: HashMap::new(),
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
        // AC-3: equality arc between rule variable and body variable.
        cg.constraints.push(TypeConstraint::Equal {
            target: rule_var,
            source: body_var,
        });
    }

    ConstraintSystem {
        vars: cg.vars,
        constraints: cg.constraints,
        rule_vars: cg.rule_vars,
    }
}

struct ConstraintGenerator<'a> {
    vars: Vec<TypeVar>,
    constraints: Vec<TypeConstraint>,
    rule_vars: HashMap<RuleId, TypeVarId>,
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

    /// Generate constraints for an IR node, returning its type variable.
    fn generate_node(&mut self, node: &IrNode, preserve_spans: bool) -> TypeVarId {
        let node_id = node as *const IrNode as usize;
        let var = self.new_var(node_id);

        match node {
            // Leaf constraints — ground types (AC-3: singleton domain)
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {
                self.constraints.push(TypeConstraint::Ground {
                    var,
                    ty: TypeDesc::Span,
                });
            }

            // Reference constraint — equality with rule's type variable
            IrNode::Ref(rule_id) => {
                if let Some(&rule_var) = self.rule_vars.get(rule_id) {
                    // In direct context, Ref produces BoxedEnum (heap-indirected).
                    // The actual type flows through the rule variable.
                    // For now, we use the existing project_node behavior:
                    // Ref → BoxedEnum in normal context, Enum in Vec context.
                    self.constraints.push(TypeConstraint::Ground {
                        var,
                        ty: TypeDesc::BoxedEnum,
                    });
                } else {
                    self.constraints.push(TypeConstraint::Ground {
                        var,
                        ty: TypeDesc::BoxedEnum,
                    });
                }
            }

            // Sequence constraint — Tuple of children
            IrNode::Seq(children) => {
                let child_vars: Vec<TypeVarId> = children
                    .iter()
                    .map(|c| self.generate_node(c, false))
                    .collect();
                self.constraints.push(TypeConstraint::Seq {
                    var,
                    children: child_vars,
                    preserve_spans,
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
                    branches: branch_vars,
                });
            }

            // Repetition constraint
            IrNode::Repeat { inner, lo, hi } => {
                let inner_var = self.generate_node(inner, false);
                if *lo == 0 && *hi == 1 {
                    // Optional
                    self.constraints.push(TypeConstraint::Optional {
                        var,
                        inner: inner_var,
                    });
                } else {
                    // Many / Many1
                    self.constraints.push(TypeConstraint::Repeat {
                        var,
                        inner: inner_var,
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
            }
            IrNode::Next(a, b) => {
                let _a_var = self.generate_node(a, false);
                let b_var = self.generate_node(b, false);
                self.constraints.push(TypeConstraint::Project {
                    var,
                    kept: b_var,
                });
            }
            IrNode::Minus(a, b) => {
                let a_var = self.generate_node(a, false);
                let _b_var = self.generate_node(b, false);
                self.constraints.push(TypeConstraint::Project {
                    var,
                    kept: a_var,
                });
            }

            // Negate — same type as inner
            IrNode::Negate(inner) => {
                let inner_var = self.generate_node(inner, false);
                self.constraints.push(TypeConstraint::Equal {
                    target: var,
                    source: inner_var,
                });
            }

            // OptionalWhitespace — transparent
            IrNode::OptionalWhitespace(inner) => {
                let inner_var = self.generate_node(inner, false);
                self.constraints.push(TypeConstraint::Equal {
                    target: var,
                    source: inner_var,
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
                            FnDescriptor::Expr { return_type, .. } => {
                                return_type.clone().unwrap_or(TypeDesc::Span)
                            }
                            _ => TypeDesc::Span,
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
            }
        }

        var
    }
}
