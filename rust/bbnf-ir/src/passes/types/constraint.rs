//! CSP type constraint definitions.
//!
//! Models type inference as a Constraint Satisfaction Problem:
//! - **Variables**: One `TypeVar` per IR node, holding a candidate `TypeDesc`
//! - **Constraints**: Type equations/inequalities from grammar structure
//! - **Arcs**: Directed edges between variables for AC-3 propagation
//!
//! The domain of each variable is a single `TypeDesc` value (deterministic —
//! no search/backtracking needed). AC-3 propagation resolves all constraints
//! in a single pass for acyclic grammars; cyclic grammars use SCC fixed-point.

use crate::TypeDesc;

/// A type variable in the CSP. Each IR node gets one.
///
/// CSP terminology:
/// - **Domain**: the set of possible types (in our case, always a singleton after solving)
/// - **Solved**: the assigned type after constraint propagation
#[derive(Clone, Debug)]
pub struct TypeVar {
    /// The resolved type. Starts as `None` (unsolved), filled by the solver.
    pub solved: Option<TypeDesc>,
    /// Node identity for debugging / error reporting.
    pub node_id: usize,
}

/// A type constraint between variables.
///
/// CSP terminology: each constraint is an arc in the constraint graph.
/// AC-3 processes arcs until all are consistent.
#[derive(Clone, Debug)]
pub enum TypeConstraint {
    /// τ = TypeDesc (ground constraint — leaf node with known type)
    /// AC-3: Singleton domain, immediately resolved.
    Ground {
        var: TypeVarId,
        ty: TypeDesc,
    },

    /// τ_target = τ_source (equality — reference or transparent wrapper)
    /// AC-3: Arc between two variables; when one resolves, the other does too.
    Equal {
        target: TypeVarId,
        source: TypeVarId,
    },

    /// τ = Tuple(τ_c₁, ..., τ_cₙ) (sequence — children compose into tuple)
    /// After solving children, computes the Seq type (with Span compression).
    Seq {
        var: TypeVarId,
        children: Vec<TypeVarId>,
        /// Whether to preserve consecutive Span identity (@pretty).
        preserve_spans: bool,
        /// Parallel children vars for span-method override tracking.
        /// Each entry is `Some(original_var)` for a Ref child that has has_sp_method
        /// (the child's primary var is already set to Span), or `None` for non-overridden children.
        sp_override_originals: Vec<Option<TypeVarId>>,
        /// Whether collapse_simple_spans is enabled on the grammar.
        collapse_simple_spans: bool,
        /// IrNode pointer keys for the children (for span-method override revert checks).
        child_node_kinds: Vec<SeqChildKind>,
    },

    /// τ = join(τ_b₁, ..., τ_bₙ) (alternation — least upper bound)
    /// CSP subtype constraint: if all branches have the same type, use it;
    /// otherwise BoxedEnum.
    Alt {
        var: TypeVarId,
        branches: Vec<TypeVarId>,
    },

    /// τ = join_in_vec(τ_b₁, ..., τ_bₙ) (alternation in Vec context)
    /// Like Alt but uses in-vec projection for branch types.
    AltInVec {
        var: TypeVarId,
        branches: Vec<TypeVarId>,
        /// Fallback var for heterogeneous case (standard Alt projection).
        standard_var: TypeVarId,
    },

    /// τ = Option(τ_inner) (optional — repeat 0..1)
    Optional {
        var: TypeVarId,
        inner: TypeVarId,
        /// Whether the inner is a transparent Ref (produces Option<Enum>).
        transparent_ref: bool,
    },

    /// τ = Vec(τ_inner) (repetition — repeat 1..∞ or 0..∞)
    /// inner is the VEC-CONTEXT variable for the inner node.
    Repeat {
        var: TypeVarId,
        inner: TypeVarId,
    },

    /// τ = τ_kept (skip/next/minus — keep one side, discard other)
    Project {
        var: TypeVarId,
        kept: TypeVarId,
    },

    /// τ = return_type (map — override from FnDescriptor)
    /// Forward checking: the @host return type constrains the variable directly.
    Map {
        var: TypeVarId,
        return_type: TypeDesc,
    },
}

/// Metadata about a Seq child for span-method override revert decisions.
#[derive(Clone, Debug)]
pub enum SeqChildKind {
    /// A Ref child with span-method override active.
    SpOverrideRef,
    /// An Optional (Repeat 0..1) child.
    Optional,
    /// Any other node.
    Other,
}

/// Index into the TypeVar array.
pub type TypeVarId = u32;

/// A directed arc for AC-3 propagation.
///
/// When `source` variable changes, re-evaluate the constraint and
/// potentially update `target`.
#[derive(Clone, Debug)]
pub struct PropagationArc {
    /// The constraint to re-evaluate.
    pub constraint_idx: usize,
    /// Variables that, when changed, trigger re-evaluation.
    pub watch: Vec<TypeVarId>,
}
