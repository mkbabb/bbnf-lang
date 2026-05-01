//! `TypeObligation` — named diagnostics for under-determined type
//! resolutions surfaced by the projection CSP.
//!
//! The historical projection pipeline collapsed every under-determined
//! `Ref` or `Alt` to `TypeDesc::BoxedEnum` silently. Per AZ-III
//! Invariant 7 ("no silent fallback"), every such collapse must surface
//! a NAMED obligation so downstream consumers (struct registry, audit,
//! codegen, debug renderers) can see exactly why a given Ref position
//! or Alt join was lifted, and which structural shape lives underneath
//! the tagged-union wrapper.
//!
//! The two obligation classes today are disjoint and complementary:
//!
//! - **Compound `Ref` collapse** (`UnresolvedCompoundRef`) at
//!   `crates/ir/src/passes/types/constraint/reference.rs:74`. The
//!   target rule resolved to a compound type (`Tuple`, `Vec`, `Option`,
//!   `Named`, `BoxedEnum`, `Enum`); the Ref was wrapped in `BoxedEnum`
//!   because the position carries an enum variant in tagged-union
//!   codegen. The obligation records the underlying compound shape
//!   verbatim. `cyclic == true` indicates the rule was grounded by the
//!   post-propagation cycle-break path
//!   (`crates/ir/src/passes/types/mod.rs`). The projected `TypeDesc`
//!   remains `BoxedEnum` — the obligation augments the projection with
//!   the structural type that the Ref *would* have inherited if it had
//!   not been wrapped.
//!
//! - **Heterogeneous `Alt` join** (`HeterogeneousAltJoin`) at
//!   `crates/ir/src/passes/types/constraint/revise.rs:123`. The Alt's
//!   branch types disagree on payload shape and could not be reduced
//!   by either the simple homogeneity check or the AU.2.5 factored-
//!   prefix homogeneity check. The constraint solver lifts the
//!   deduplicated branch list into a new `TypeDesc::HeterogeneousAltJoin`
//!   variant — codegen still lowers it to the same `Box<Enum>` layout
//!   `BoxedEnum` always produced, but `ir.types` now records the join
//!   evidence structurally. The post-projection walk pairs each
//!   surfaced lift with the owning `RuleId` and Alt `NodeId` and emits
//!   one obligation per join site.
//!
//! Both classes share the same `Vec<TypeObligation>` surface on
//! `GrammarIR::type_obligations` so a single drain step at the end of
//! `project_types` publishes the diagnostic stream and downstream
//! consumers iterate over the unified list.

use std::cell::RefCell;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::dag::NodeId;
use crate::types::{RuleId, TypeDesc};

/// A named obligation surfaced by the type projection CSP whenever a
/// silent fallback would otherwise have collapsed under-determined
/// type resolution into `TypeDesc::BoxedEnum`.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeObligation {
    /// A compound `Ref` whose target rule resolved to a compound type
    /// (`Tuple`, `Vec`, `Option`, `Named`, `Enum`, `BoxedEnum`). The
    /// `Ref` position carries an enum variant in tagged-union codegen,
    /// so the projected `TypeDesc` is `BoxedEnum`; the `structural_type`
    /// records the actual shape under the wrapper. When `cyclic == true`
    /// the target rule was grounded by the post-propagation cycle break,
    /// not by a normal compound resolution.
    UnresolvedCompoundRef {
        /// The `IrNode::Ref` node id whose `var` was assigned `BoxedEnum`.
        /// `None` for cyclic rule grounds where the Ref node id is not
        /// in scope at the cycle-break site.
        ref_node: Option<NodeId>,
        /// The target rule whose compound type forced the `BoxedEnum`
        /// fallback.
        target_rule: RuleId,
        /// The compound `TypeDesc` the Ref *would* have inherited if
        /// the position were not wrapped in an enum variant.
        structural_type: TypeDesc,
        /// Whether this obligation was raised by the cycle-break path
        /// rather than the normal compound-Ref path.
        cyclic: bool,
    },
    /// A heterogeneous alternation join that the CSP could not collapse
    /// to a homogeneous payload type at
    /// `crates/ir/src/passes/types/constraint/revise.rs`. The
    /// constraint solver lifts the distinct branch list into
    /// `TypeDesc::HeterogeneousAltJoin(branches)` — backend codegen
    /// lowers that identically to the historical `BoxedEnum` layout
    /// (a `Box<Enum>` view), and this obligation surfaces the join
    /// site for diagnostics and audit consumption.
    ///
    /// `branches` is the deduplicated set of distinct branch types
    /// observed at join time, in first-appearance order. `rule` is the
    /// rule containing the Alt (when known) and `node` is the Alt
    /// node's stable `NodeId`.
    HeterogeneousAltJoin {
        rule: Option<RuleId>,
        node: Option<NodeId>,
        branches: Vec<TypeDesc>,
    },
}

impl TypeObligation {
    /// Concise human-readable name of the obligation kind. Stable
    /// across releases — used by diagnostic stream consumers and the
    /// `W3a-types-obligations.txt` archive.
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::UnresolvedCompoundRef { .. } => "UnresolvedCompoundRef",
            Self::HeterogeneousAltJoin { .. } => "HeterogeneousAltJoin",
        }
    }

    /// Render the obligation as a single human-readable diagnostic
    /// line. Used by the `W3a-types-obligations.txt` archival flow and
    /// any downstream report writer.
    pub fn diagnostic(&self) -> String {
        match self {
            Self::UnresolvedCompoundRef {
                ref_node,
                target_rule,
                structural_type,
                cyclic,
            } => {
                let kind = if *cyclic {
                    "cyclic compound Ref"
                } else {
                    "unresolved compound Ref"
                };
                let node_str = match ref_node {
                    Some(nid) => format!("node {nid}"),
                    None => "<cycle-break>".to_string(),
                };
                format!(
                    "{kind}: {node_str} → rule {target_rule} :: structural {structural_type:?} \
                     (projected BoxedEnum for tagged-union wrapper)"
                )
            }
            Self::HeterogeneousAltJoin {
                rule,
                node,
                branches,
            } => {
                let rule_str = match rule {
                    Some(rid) => format!("rule {rid}"),
                    None => "<unknown rule>".to_string(),
                };
                let node_str = match node {
                    Some(nid) => format!("node {nid}"),
                    None => "<unknown node>".to_string(),
                };
                format!(
                    "heterogeneous Alt join: {node_str} in {rule_str} :: branches {branches:?} \
                     (lifted to TypeDesc::HeterogeneousAltJoin; lowers to Box<Enum>)"
                )
            }
        }
    }
}

/// Deduplicate equal types, preserving first-appearance order. Used by
/// the join site to canonicalise a branch-type list before recording it
/// as the embedded payload of `TypeDesc::HeterogeneousAltJoin` and the
/// obligation stream.
///
/// The CSP solver sees small branch counts (typical CSS-style
/// `value = ident | length | percentage | color` peaks at ~6 branches;
/// the largest production grammar Alt is ~10 distinct shapes), so the
/// O(n²) dedup is cheaper than a HashSet allocation.
pub fn dedup_branch_types(branches: &[&TypeDesc]) -> Vec<TypeDesc> {
    let mut out: Vec<TypeDesc> = Vec::with_capacity(branches.len());
    for &ty in branches {
        if !out.iter().any(|existing| existing == ty) {
            out.push(ty.clone());
        }
    }
    out
}

/// Shared collector handed to constraint impls that need to record an
/// obligation during a `revise` call. `Rc<RefCell<…>>` so the
/// constraint structs can hold a clone-on-construction handle without
/// taking a mutable borrow on the CSP variable arena.
#[derive(Clone, Debug, Default)]
pub struct ObligationSink {
    inner: Rc<RefCell<Vec<TypeObligation>>>,
}

impl ObligationSink {
    /// Construct an empty sink.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record one obligation. Idempotent only at the call site — the
    /// solver may visit the same Ref multiple times during fixed-point
    /// iteration; the projection-pass driver dedups before publishing.
    pub fn record(&self, obligation: TypeObligation) {
        self.inner.borrow_mut().push(obligation);
    }

    /// Drain every recorded obligation, returning ownership to the
    /// caller. Used by `project_types` once solver propagation has
    /// converged.
    pub fn drain(&self) -> Vec<TypeObligation> {
        std::mem::take(&mut *self.inner.borrow_mut())
    }
}
