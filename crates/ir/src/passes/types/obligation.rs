//! `TypeObligation` — named diagnostics for under-determined type
//! resolutions surfaced by the projection CSP.
//!
//! The historical projection pipeline collapsed every under-determined
//! Ref or Alt to `TypeDesc::BoxedEnum` silently. Per AZ-III Invariant 7
//! ("no silent fallback"), an unresolved compound `Ref` or a cyclic
//! rule resolution must surface a NAMED obligation so downstream
//! consumers (struct registry, audit, codegen) can see exactly why a
//! given Ref position was chosen and which structural type lives
//! underneath the tagged-union wrapper.
//!
//! The obligation does not change the projected `TypeDesc` — `BoxedEnum`
//! remains the grammar-general layout for compound Ref positions in
//! tagged-union codegen. The obligation augments the projection with
//! the structural type that the Ref *would* have inherited if it had
//! not been wrapped in an enum variant, plus the cycle context when the
//! resolution is cyclic.
//!
//! Scope (AZ-III.W3a.2):
//!
//! - `UnresolvedCompoundRef { ref_node, target_rule, structural_type,
//!   cyclic }`: the Ref's target rule resolved to a compound type
//!   (`Tuple`, `Vec`, `Option`, `Named`, `BoxedEnum`, `Enum`); the Ref
//!   was wrapped in `BoxedEnum` because the position holds an enum
//!   variant. The `structural_type` records the underlying compound
//!   shape verbatim. `cyclic == true` indicates the rule was grounded
//!   by the post-propagation cycle-break path
//!   (`crates/ir/src/passes/types/mod.rs`).
//!
//! W3a.3 will land `HeterogeneousAltJoin` next to this variant for the
//! `revise.rs:123` site.

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
}

impl TypeObligation {
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
        }
    }
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
