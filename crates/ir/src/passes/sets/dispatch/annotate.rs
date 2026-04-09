//! Recursive tree walk that consumes the eligibility table and installs
//! `AltDispatch` payloads on eligible Alt nodes.
//!
//! `containing_follow` carries the contextual FOLLOW set: for top-level
//! nodes this is `FOLLOW(rule)`, but for nodes inside a `Seq` it is
//! `FIRST(suffix)` (the first set of the remaining sequence elements).
//! Nullable branches use this contextual FOLLOW to participate in dispatch
//! tables; non-nullable branches don't need it.

use crate::dag::GrammarDag;
use crate::{CharSet128, IrNode};

use super::build::{try_build_dispatch, try_build_fallback_dispatch};
use super::eligibility::DispatchEligibility;
use super::first_set::{node_first_set, suffix_follow};

/// Recursively walk an `IrNode` tree and annotate eligible Alt nodes.
///
/// `eligibility` is the CSP pre-computed dispatch eligibility table: when an
/// Alt is marked non-dispatchable (without FOLLOW context), the walk can skip
/// the full `try_build_dispatch` call. FOLLOW-aware dispatch still proceeds
/// for nullable branches, since that context is only available during the walk.
pub(super) fn annotate_node(
    node: &mut IrNode,
    containing_follow: Option<&CharSet128>,
    dag: &GrammarDag,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
    eligibility: &DispatchEligibility,
) {
    // Capture the Alt-arm node id before the mutable destructuring
    // borrow, so the eligibility lookup below doesn't need another
    // `&node` read while we hold `&mut branches, dispatch`.
    let node_nid = match node {
        IrNode::Alt(_, _) => Some(
            dag.node_for(node)
                .expect("every Alt visited by annotate_node must be in the DAG"),
        ),
        _ => None,
    };

    match node {
        IrNode::Alt(branches, dispatch) => {
            let node_nid = node_nid.unwrap();
            // Recurse into children first.
            for branch in branches.iter_mut() {
                annotate_node(
                    &mut branch.node,
                    containing_follow,
                    dag,
                    rule_metas,
                    strings,
                    eligibility,
                );
            }

            // Skip if already annotated.
            if dispatch.is_some() {
                return;
            }

            // Recompute FIRST sets for branches that lack them (after inlining/fusing,
            // newly created AltBranch nodes may have first_set=None).
            for branch in branches.iter_mut() {
                if branch.first_set.is_none() {
                    branch.first_set = node_first_set(&branch.node, rule_metas, strings);
                }
            }

            // Consult the CSP pre-computed eligibility.
            // When the pre-computation determined non-dispatchable AND there are
            // no nullable branches (where FOLLOW context could rescue dispatch),
            // we skip the full try_build_dispatch and go straight to fallback.
            let pre_eligible = eligibility.get(&node_nid).copied();
            let has_nullable = branches.iter().any(|b| b.first_set.is_none());

            if pre_eligible == Some(false) && !has_nullable {
                // CSP says non-dispatchable, no nullable branch could be rescued
                // by FOLLOW — try fallback dispatch only.
                if let Some(table) = try_build_fallback_dispatch(branches) {
                    *dispatch = Some(table);
                }
                return;
            }

            // Try to build a full dispatch table (all branches disjoint).
            // Falls back to fallback-aware dispatch when the last branch
            // has a superset FIRST set (e.g. genericDecl catch-all).
            if let Some(table) = try_build_dispatch(branches, containing_follow) {
                *dispatch = Some(table);
            } else if let Some(table) = try_build_fallback_dispatch(branches) {
                *dispatch = Some(table);
            }
        }
        IrNode::Seq(children) => {
            // For each child in the Seq, compute the contextual follow from
            // FIRST(suffix) rather than blindly passing the rule-level FOLLOW.
            for i in 0..children.len() {
                let child_follow = if i + 1 < children.len() {
                    suffix_follow(children, i + 1, containing_follow, rule_metas, strings)
                } else {
                    containing_follow.cloned()
                };
                annotate_node(
                    &mut children[i],
                    child_follow.as_ref().or(containing_follow),
                    dag,
                    rule_metas,
                    strings,
                    eligibility,
                );
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            annotate_node(inner, containing_follow, dag, rule_metas, strings, eligibility);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            annotate_node(a, containing_follow, dag, rule_metas, strings, eligibility);
            annotate_node(b, containing_follow, dag, rule_metas, strings, eligibility);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            annotate_node(token, containing_follow, dag, rule_metas, strings, eligibility);
            for arm in arms {
                annotate_node(
                    &mut arm.continuation,
                    containing_follow,
                    dag,
                    rule_metas,
                    strings,
                    eligibility,
                );
            }
            annotate_node(
                fallback,
                containing_follow,
                dag,
                rule_metas,
                strings,
                eligibility,
            );
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
