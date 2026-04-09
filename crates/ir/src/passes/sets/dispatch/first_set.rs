//! Pure FIRST/FOLLOW helpers used by the eligibility pre-compute and the
//! tree-walk annotator. None of these mutate the IR — they only read it.

use crate::regex_first;
use crate::{CharSet128, IrNode};

/// Compute the FIRST set of an `IrNode`.
///
/// Returns `Some(charset)` for non-nullable nodes, `None` for nullable ones.
/// `rule_metas` provides pre-computed `(first_set, nullable)` for rule references.
pub(super) fn node_first_set(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            if let Some(b) = s.bytes().next() {
                let mut cs = CharSet128::new();
                cs.add(b);
                Some(cs)
            } else {
                None // empty literal = nullable
            }
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            regex_first::regex_first_chars(pattern).filter(|cs| !cs.is_empty())
        }
        IrNode::Ref(rule_id) => {
            let (ref first_set, nullable) = rule_metas[*rule_id as usize];
            if nullable {
                None
            } else {
                Some(first_set.clone())
            }
        }
        IrNode::Seq(children) => {
            let mut combined = CharSet128::new();
            for child in children {
                if let Some(fs) = node_first_set(child, rule_metas, strings) {
                    combined.union(&fs);
                    return Some(combined); // non-nullable child stops propagation
                }
                // Child is nullable — continue to include next child's FIRST
                if let Some(fs) = node_first_set_nullable_part(child, rule_metas) {
                    combined.union(&fs);
                }
            }
            None // all children nullable
        }
        IrNode::Alt(branches, _) => {
            let mut combined = CharSet128::new();
            for b in branches {
                if let Some(ref fs) = b.first_set {
                    combined.union(fs);
                } else {
                    return None; // nullable branch
                }
            }
            Some(combined)
        }
        IrNode::Repeat { lo: 0, .. } | IrNode::Epsilon => None,
        IrNode::Repeat { inner, .. } => node_first_set(inner, rule_metas, strings),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            node_first_set(inner, rule_metas, strings)
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => node_first_set(a, rule_metas, strings),
        IrNode::Minus(a, _) => node_first_set(a, rule_metas, strings),
        IrNode::Negate(_) => None, // zero-width, nullable
        IrNode::TokenDispatch { token, .. } => node_first_set(token, rule_metas, strings),
    }
}

/// Get the FIRST set contribution from a nullable node (the non-None part).
pub(super) fn node_first_set_nullable_part(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
) -> Option<CharSet128> {
    match node {
        IrNode::Ref(rule_id) => {
            let (ref first_set, _) = rule_metas[*rule_id as usize];
            if first_set.is_empty() {
                None
            } else {
                Some(first_set.clone())
            }
        }
        IrNode::Repeat { inner, .. } => node_first_set_nullable_part(inner, rule_metas),
        _ => None,
    }
}

/// Compute the contextual FOLLOW for a node at position `from` within a Seq.
///
/// Returns `FIRST(children[from..])` unioned with `rule_follow` if the suffix
/// is entirely nullable.
pub(super) fn suffix_follow(
    children: &[IrNode],
    from: usize,
    rule_follow: Option<&CharSet128>,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Option<CharSet128> {
    let mut combined = CharSet128::new();
    for child in &children[from..] {
        if let Some(fs) = node_first_set(child, rule_metas, strings) {
            combined.union(&fs);
            // Non-nullable child — suffix can't produce anything beyond this.
            return Some(combined);
        }
        // Nullable child — include its FIRST and continue.
        if let Some(fs) = node_first_set_nullable_part(child, rule_metas) {
            combined.union(&fs);
        }
    }
    // All suffix children are nullable — include rule FOLLOW.
    if let Some(follow) = rule_follow {
        combined.union(follow);
    }
    if combined.is_empty() {
        None
    } else {
        Some(combined)
    }
}
