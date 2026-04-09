//! CSP constraint generation from IR structure.
//!
//! Walks each rule's body and emits type constraints per node, building
//! a `csp_solver::Csp<TypeDomain>` directly. Each constraint is a struct
//! implementing the CSP crate's `Constraint<TypeDomain>` trait.
//!
//! Two type variables are created per node:
//! - **Primary**: The node's type in normal context (project_node)
//! - **Vec-context**: The node's type when used as a Vec element (project_node_in_vec)
//!
//! Seq metadata (child types, preserve_spans) is recorded during generation
//! for later export to the TypeMap.

use std::collections::HashMap;

use csp_solver::Csp;

use crate::dag::NodeId;
use crate::{FnDescriptor, GrammarIR, IrNode, RuleId, TypeDesc};

use super::constraint::*;

/// Result of constraint generation: CSP + rule-to-var mapping + metadata.
///
/// Internal pointer-keyed maps (`node_vars`, `vec_context_vars`,
/// `seq_metadata`) are an implementation detail of constraint
/// generation. A parallel `node_id_for_ptr` side map translates
/// those pointer keys to stable `NodeId`s at extraction time so the
/// public `TypeMap` can use DAG-resolved keys.
pub struct ConstraintSystem {
    pub csp: Csp<TypeDomain>,
    /// Maps `RuleId -> TypeVarId` for rule-level type variables.
    pub rule_vars: HashMap<RuleId, TypeVarId>,
    /// Maps pointer -> `TypeVarId` for normal-context type variables.
    pub node_vars: HashMap<usize, TypeVarId>,
    /// Maps pointer -> `TypeVarId` for vec-context type variables.
    pub vec_context_vars: HashMap<usize, TypeVarId>,
    /// Per-Seq metadata: pointer -> `(child var IDs, preserve_spans)`.
    pub seq_metadata: HashMap<usize, SeqMetadata>,
    /// Seq constraints tracked for TypeMap export.
    pub seq_constraints: Vec<SeqConstraintMeta>,
    /// Side map: pointer -> stable `NodeId` from `ir.dag`. Populated
    /// during `generate_node` when the DAG knows the node; used at
    /// extraction to key the public `TypeMap` by `NodeId` rather
    /// than by pointer.
    pub node_id_for_ptr: HashMap<usize, NodeId>,
}

/// Metadata recorded during constraint generation for Seq nodes.
pub struct SeqMetadata {
    /// TypeVarIds for each child in the Seq (normal context).
    pub child_vars: Vec<TypeVarId>,
    /// Whether preserve_spans was set for this Seq.
    pub preserve_spans: bool,
}

/// Metadata for Seq constraints, used during TypeMap export.
pub struct SeqConstraintMeta {
    /// The Seq node's stable `NodeId`, populated from `ir.dag` during
    /// constraint generation. `None` when the DAG doesn't know about
    /// the node (tests without a DAG).
    pub seq_node_id: Option<NodeId>,
    pub var: TypeVarId,
    pub children: Vec<TypeVarId>,
    pub preserve_spans: bool,
    pub sp_override_originals: Vec<Option<TypeVarId>>,
    pub collapse_simple_spans: bool,
    pub child_node_kinds: Vec<SeqChildKind>,
}

/// Generate the constraint system from a GrammarIR.
pub fn generate_constraints(ir: &GrammarIR) -> ConstraintSystem {
    let mut cg = ConstraintGenerator {
        csp: Csp::new(),
        rule_vars: HashMap::new(),
        node_vars: HashMap::new(),
        vec_context_vars: HashMap::new(),
        seq_metadata: HashMap::new(),
        seq_constraints: Vec::new(),
        node_id_for_ptr: HashMap::new(),
        ir,
    };

    // Phase 1: Allocate a type variable for each rule.
    for rule in &ir.rules {
        let var = cg.new_var();
        cg.rule_vars.insert(rule.id, var);
    }

    // Phase 2: Generate constraints from each rule's body.
    for rule in &ir.rules {
        let rule_var = cg.rule_vars[&rule.id];
        let preserve_spans = rule.meta.directives.pretty.is_some();
        let body_var = cg.generate_node(&rule.body, preserve_spans);

        // Rule's type = its body's type.
        cg.csp
            .add_constraint(EqualConstraint::new(rule_var, body_var));
    }

    ConstraintSystem {
        csp: cg.csp,
        rule_vars: cg.rule_vars,
        node_vars: cg.node_vars,
        vec_context_vars: cg.vec_context_vars,
        seq_metadata: cg.seq_metadata,
        seq_constraints: cg.seq_constraints,
        node_id_for_ptr: cg.node_id_for_ptr,
    }
}

struct ConstraintGenerator<'a> {
    csp: Csp<TypeDomain>,
    rule_vars: HashMap<RuleId, TypeVarId>,
    node_vars: HashMap<usize, TypeVarId>,
    vec_context_vars: HashMap<usize, TypeVarId>,
    seq_metadata: HashMap<usize, SeqMetadata>,
    seq_constraints: Vec<SeqConstraintMeta>,
    node_id_for_ptr: HashMap<usize, NodeId>,
    ir: &'a GrammarIR,
}

impl<'a> ConstraintGenerator<'a> {
    fn new_var(&mut self) -> TypeVarId {
        self.csp.add_variable(TypeDomain::unsolved())
    }

    /// Generate constraints for an IR node, returning its primary type variable.
    ///
    /// Also generates the vec-context variable and records it in `vec_context_vars`.
    fn generate_node(&mut self, node: &IrNode, preserve_spans: bool) -> TypeVarId {
        let node_id = node as *const IrNode as usize;
        let var = self.new_var();
        self.node_vars.insert(node_id, var);

        // Record the `NodeId` for this tree position so the extractor
        // can translate the internal pointer key to a stable id when
        // building the public `TypeMap`. If the DAG isn't populated,
        // the side map stays empty for this pointer and the extractor
        // skips it.
        if let Some(dag) = self.ir.dag.as_ref() {
            if let Some(nid) = dag.node_for(node) {
                self.node_id_for_ptr.insert(node_id, nid);
            }
        }

        // Create vec-context variable for this node.
        let vec_var = self.new_var();
        self.vec_context_vars.insert(node_id, vec_var);

        match node {
            // Leaf constraints -- ground types
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {
                self.csp
                    .add_constraint(GroundConstraint::new(var, TypeDesc::Span));
                self.csp
                    .add_constraint(GroundConstraint::new(vec_var, TypeDesc::Span));
            }

            // Reference constraint
            IrNode::Ref(_rule_id) => {
                // Normal context: Ref always produces BoxedEnum.
                self.csp
                    .add_constraint(GroundConstraint::new(var, TypeDesc::BoxedEnum));
                // Vec context: Ref produces Enum (Vec provides heap indirection).
                self.csp
                    .add_constraint(GroundConstraint::new(vec_var, TypeDesc::Enum));
            }

            // Sequence constraint -- Tuple of children
            IrNode::Seq(children) => {
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
                        let override_var = self.new_var();
                        self.csp
                            .add_constraint(GroundConstraint::new(override_var, TypeDesc::Span));
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

                // Track metadata for TypeMap export. The Seq's
                // `NodeId` (if known to the DAG) keys the public
                // `TypeMap` Seq entries at extraction.
                let seq_node_id = self.node_id_for_ptr.get(&node_id).copied();
                self.seq_constraints.push(SeqConstraintMeta {
                    seq_node_id,
                    var,
                    children: child_vars.clone(),
                    preserve_spans,
                    sp_override_originals: sp_override_originals.clone(),
                    collapse_simple_spans: self.ir.collapse_simple_spans,
                    child_node_kinds: child_node_kinds.clone(),
                });

                self.csp.add_constraint(SeqConstraint::new(
                    var,
                    child_vars,
                    preserve_spans,
                    sp_override_originals,
                    self.ir.collapse_simple_spans,
                    child_node_kinds,
                ));

                // Vec context for a Seq: delegate to normal projection.
                self.csp
                    .add_constraint(EqualConstraint::new(vec_var, var));
            }

            // Alternation constraint -- join of branches
            IrNode::Alt(branches, _dispatch) => {
                let branch_vars: Vec<TypeVarId> = branches
                    .iter()
                    .map(|b| self.generate_node(&b.node, false))
                    .collect();
                self.csp
                    .add_constraint(AltConstraint::new(var, branch_vars.clone()));

                // Vec context for Alt: try in_vec projection for branches.
                let vec_branch_vars: Vec<TypeVarId> = branches
                    .iter()
                    .map(|b| {
                        let b_id = &b.node as *const IrNode as usize;
                        self.vec_context_vars[&b_id]
                    })
                    .collect();

                self.csp
                    .add_constraint(AltInVecConstraint::new(vec_var, vec_branch_vars, var));
            }

            // Repetition constraint
            IrNode::Repeat { inner, lo, hi } => {
                let inner_var = self.generate_node(inner, false);
                let inner_id = inner.as_ref() as *const IrNode as usize;

                if *lo == 0 && *hi == 1 {
                    // Optional
                    let transparent_ref = if let IrNode::Ref(rule_id) = inner.as_ref() {
                        self.ir.rules[*rule_id as usize].meta.is_transparent
                    } else {
                        false
                    };

                    self.csp.add_constraint(OptionalConstraint::new(
                        var,
                        inner_var,
                        transparent_ref,
                    ));

                    self.csp
                        .add_constraint(EqualConstraint::new(vec_var, var));
                } else {
                    // Many / Many1: use the vec-context variable for inner elements.
                    let inner_vec_var = self.vec_context_vars[&inner_id];
                    self.csp
                        .add_constraint(RepeatConstraint::new(var, inner_vec_var));

                    self.csp
                        .add_constraint(EqualConstraint::new(vec_var, var));
                }
            }

            // Operator constraints -- keep one side
            IrNode::Skip(a, b) => {
                let a_var = self.generate_node(a, false);
                let _b_var = self.generate_node(b, false);
                self.csp
                    .add_constraint(ProjectConstraint::new(var, a_var));

                let a_id = a.as_ref() as *const IrNode as usize;
                let a_vec_var = self.vec_context_vars[&a_id];
                self.csp
                    .add_constraint(ProjectConstraint::new(vec_var, a_vec_var));
            }
            IrNode::Next(a, b) => {
                let _a_var = self.generate_node(a, false);
                let b_var = self.generate_node(b, false);
                self.csp
                    .add_constraint(ProjectConstraint::new(var, b_var));

                let b_id = b.as_ref() as *const IrNode as usize;
                let b_vec_var = self.vec_context_vars[&b_id];
                self.csp
                    .add_constraint(ProjectConstraint::new(vec_var, b_vec_var));
            }
            IrNode::Minus(a, b) => {
                let a_var = self.generate_node(a, false);
                let _b_var = self.generate_node(b, false);
                self.csp
                    .add_constraint(ProjectConstraint::new(var, a_var));

                let a_id = a.as_ref() as *const IrNode as usize;
                let a_vec_var = self.vec_context_vars[&a_id];
                self.csp
                    .add_constraint(ProjectConstraint::new(vec_var, a_vec_var));
            }

            // Negate -- always produces empty tuple.
            IrNode::Negate(inner) => {
                let _inner_var = self.generate_node(inner, false);
                self.csp
                    .add_constraint(GroundConstraint::new(var, TypeDesc::Tuple(vec![])));
                self.csp
                    .add_constraint(GroundConstraint::new(vec_var, TypeDesc::Tuple(vec![])));
            }

            // OptionalWhitespace -- transparent
            IrNode::OptionalWhitespace(inner) => {
                let inner_var = self.generate_node(inner, preserve_spans);
                self.csp
                    .add_constraint(EqualConstraint::new(var, inner_var));

                let inner_id = inner.as_ref() as *const IrNode as usize;
                let inner_vec_var = self.vec_context_vars[&inner_id];
                self.csp
                    .add_constraint(EqualConstraint::new(vec_var, inner_vec_var));
            }

            // Map constraint -- type comes from FnDescriptor
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
                self.csp
                    .add_constraint(MapConstraint::new(var, map_type.clone()));
                self.csp
                    .add_constraint(MapConstraint::new(vec_var, map_type));
            }

            // TokenDispatch -- join of arm types + fallback
            IrNode::TokenDispatch {
                token: _,
                arms,
                fallback,
            } => {
                let mut branch_vars = Vec::new();
                for arm in arms {
                    let cont_var = self.generate_node(&arm.continuation, false);
                    if let Some(map_fn_id) = arm.map_fn {
                        let mapped_var = self.new_var();
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
                        self.csp
                            .add_constraint(MapConstraint::new(mapped_var, map_type));
                        branch_vars.push(mapped_var);
                    } else {
                        branch_vars.push(cont_var);
                    }
                }
                let fallback_var = self.generate_node(fallback, false);
                branch_vars.push(fallback_var);

                self.csp
                    .add_constraint(AltConstraint::new(var, branch_vars));

                self.csp
                    .add_constraint(EqualConstraint::new(vec_var, var));
            }
        }

        var
    }
}
