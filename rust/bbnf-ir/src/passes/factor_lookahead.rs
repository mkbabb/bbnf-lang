//! Pass: Factor common regex prefixes with lookahead dispatch.
//!
//! Detects Alt branches where:
//! 1. Multiple branches start with the same (or overlapping) Regex/Literal prefix
//! 2. The continuations after the prefix have disjoint FIRST sets
//!
//! Restructures `Alt([Seq(R, A, ...), Seq(R, B, ...)])` → `Seq(R, Alt([Seq(A, ...), Seq(B, ...)]))`.
//! The inner Alt gets disjoint FIRST sets (from the continuations), enabling O(1) dispatch.
//!
//! This extends `factor_common_prefixes` to handle the case where branches share
//! a regex prefix but differ in what follows — common in grammars where the same
//! token class (e.g., identifiers) appears in different contexts (e.g., CSS
//! declarations vs nested selectors).

use crate::{AltBranch, AltDispatch, CharSet128, GrammarIR, IrNode, regex_first};

/// Factor common regex prefixes with lookahead dispatch across the entire IR.
pub fn factor_regex_with_lookahead(ir: &mut GrammarIR) {
    let strings = ir.strings.clone();
    let rule_metas: Vec<(CharSet128, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.meta.first_set.clone(), r.meta.nullable))
        .collect();

    for rule in &mut ir.rules {
        rule.body = factor_node(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &strings,
            &rule_metas,
        );
    }
}

fn factor_node(
    node: IrNode,
    strings: &[String],
    rule_metas: &[(CharSet128, bool)],
) -> IrNode {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse first.
            let branches: Vec<AltBranch> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = factor_node(b.node, strings, rule_metas);
                    b
                })
                .collect();

            // Skip if already has dispatch or < 2 branches.
            if dispatch.is_some() || branches.len() < 2 {
                return IrNode::Alt(branches, dispatch);
            }

            // Try to factor common regex prefix with lookahead dispatch.
            if let Some(factored) = try_factor_alt(&branches, strings, rule_metas) {
                return factor_node(factored, strings, rule_metas);
            }

            IrNode::Alt(branches, dispatch)
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(|c| factor_node(c, strings, rule_metas)).collect())
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(factor_node(*inner, strings, rule_metas)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(factor_node(*a, strings, rule_metas)),
            Box::new(factor_node(*b, strings, rule_metas)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(factor_node(*a, strings, rule_metas)),
            Box::new(factor_node(*b, strings, rule_metas)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(factor_node(*a, strings, rule_metas)),
            Box::new(factor_node(*b, strings, rule_metas)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(factor_node(*inner, strings, rule_metas))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(factor_node(*inner, strings, rule_metas)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(factor_node(*inner, strings, rule_metas)),
            fn_id,
        },
        other => other,
    }
}

/// Unwrap Map and OptionalWhitespace wrappers to find the "semantic" expression.
fn unwrap_to_seq(node: &IrNode) -> Option<&[IrNode]> {
    match node {
        IrNode::Seq(children) => Some(children),
        IrNode::Map { inner, .. } => unwrap_to_seq(inner),
        IrNode::OptionalWhitespace(inner) => unwrap_to_seq(inner),
        _ => None,
    }
}

/// Extract the leading node of a Seq (first element, unwrapping OW/Map).
fn leading_expr(node: &IrNode) -> Option<&IrNode> {
    let children = unwrap_to_seq(node)?;
    if children.is_empty() {
        return None;
    }
    let first = &children[0];
    // Unwrap OW on the leading element itself.
    match first {
        IrNode::OptionalWhitespace(inner) => Some(inner),
        IrNode::Map { inner, .. } => Some(inner),
        other => Some(other),
    }
}

/// Compute FIRST set of an IrNode.
fn first_set_of(
    node: &IrNode,
    strings: &[String],
    rule_metas: &[(CharSet128, bool)],
) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            s.bytes().next().map(|b| {
                let mut cs = CharSet128::new();
                cs.add(b);
                cs
            })
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            regex_first::regex_first_chars(pattern).filter(|cs| !cs.is_empty())
        }
        IrNode::Ref(rule_id) => {
            let (ref fs, nullable) = rule_metas[*rule_id as usize];
            if nullable || fs.is_empty() {
                None
            } else {
                Some(fs.clone())
            }
        }
        IrNode::Seq(children) => {
            if children.is_empty() {
                return None;
            }
            first_set_of(&children[0], strings, rule_metas)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            first_set_of(inner, strings, rule_metas)
        }
        IrNode::Epsilon => None,
        _ => None,
    }
}

/// Get the "continuation" FIRST set — the FIRST of the second element in a branch's Seq.
fn continuation_first(
    node: &IrNode,
    strings: &[String],
    rule_metas: &[(CharSet128, bool)],
) -> Option<CharSet128> {
    let children = unwrap_to_seq(node)?;
    if children.len() < 2 {
        return None;
    }
    // The continuation starts at children[1]. But children[0] might be OW-wrapped,
    // in which case children[1] is the REAL second element.
    // Also handle: Seq([OW(Regex), Literal(":"), ...]) where OW wrapping the regex
    // means the actual second element is children[1].
    first_set_of(&children[1], strings, rule_metas)
}

/// Try to factor an Alt by extracting a common regex prefix.
///
/// Checks all pairs of branches for:
/// 1. Both are Seq-like (after unwrapping Map/OW)
/// 2. Both start with a Regex (possibly the same, possibly overlapping)
/// 3. The continuations (second element onward) have disjoint FIRST sets
///
/// If found, restructures to `Seq(common_regex, Alt(continuations, dispatch))`.
fn try_factor_alt(
    branches: &[AltBranch],
    strings: &[String],
    rule_metas: &[(CharSet128, bool)],
) -> Option<IrNode> {
    if branches.len() != 2 {
        return None;
    }

    // Both branches must be Seq-like.
    let seq_a = unwrap_to_seq(&branches[0].node)?;
    let seq_b = unwrap_to_seq(&branches[1].node)?;

    if seq_a.len() < 2 || seq_b.len() < 2 {
        return None;
    }

    // Leading expressions must both be Regex.
    let lead_a = leading_expr(&branches[0].node)?;
    let lead_b = leading_expr(&branches[1].node)?;

    // Both must be Regex (not Ref or other).
    let (regex_a_sid, regex_b_sid) = match (lead_a, lead_b) {
        (IrNode::Regex(a), IrNode::Regex(b)) => (*a, *b),
        _ => return None,
    };

    // FIRST sets must overlap (otherwise factor_common_prefixes or dispatch already handles it).
    let first_a = regex_first::regex_first_chars(&strings[regex_a_sid as usize])?;
    let first_b = regex_first::regex_first_chars(&strings[regex_b_sid as usize])?;

    if first_a.is_disjoint(&first_b) {
        return None; // No overlap — dispatch table can handle this directly.
    }

    // Continuation FIRST sets must be disjoint for dispatch.
    let cont_a = continuation_first(&branches[0].node, strings, rule_metas)?;
    let cont_b = continuation_first(&branches[1].node, strings, rule_metas)?;

    if !cont_a.is_disjoint(&cont_b) {
        return None; // Continuations also overlap — can't dispatch.
    }

    // Choose the more restrictive regex as the common prefix.
    // If A ⊆ B (A's FIRST is subset of B's), use A as the prefix.
    // Otherwise, if the regexes are the same, use either.
    // Otherwise, we can't safely factor (the prefix regex might match
    // different lengths for different inputs).
    let common_regex_sid = if regex_a_sid == regex_b_sid {
        regex_a_sid
    } else if is_subset(&first_a, &first_b) {
        // A is more restrictive — use it as prefix.
        // But this means branch B's inputs that don't match A won't get the prefix scan.
        // We'd need to keep branch B for those inputs — too complex for now.
        return None;
    } else {
        return None; // Different regexes with overlapping but non-subset FIRST — can't factor.
    };

    // Build continuations from each branch (everything after the first element).
    let cont_a_nodes = strip_leading_seq(&branches[0].node)?;
    let cont_b_nodes = strip_leading_seq(&branches[1].node)?;

    // Build dispatch table for the inner Alt.
    let mut table = vec![255u8; 128];
    for code in cont_a.iter() {
        table[code as usize] = 0;
    }
    for code in cont_b.iter() {
        table[code as usize] = 1;
    }

    let inner_alt = IrNode::Alt(
        vec![
            AltBranch {
                node: cont_a_nodes,
                first_set: Some(cont_a),
            },
            AltBranch {
                node: cont_b_nodes,
                first_set: Some(cont_b),
            },
        ],
        Some(AltDispatch { table }),
    );

    Some(IrNode::Seq(vec![IrNode::Regex(common_regex_sid), inner_alt]))
}

/// Check if charset A is a subset of charset B.
fn is_subset(a: &CharSet128, b: &CharSet128) -> bool {
    // A ⊆ B iff A ∩ B = A iff (A & ~B) = 0
    (a.bits[0] & !b.bits[0]) == 0 && (a.bits[1] & !b.bits[1]) == 0
}

/// Strip the leading element from a branch's Seq, preserving Map/OW wrappers.
///
/// Returns the continuation as a single IrNode (Seq or single node).
fn strip_leading_seq(node: &IrNode) -> Option<IrNode> {
    match node {
        IrNode::Seq(children) if children.len() > 1 => {
            let rest: Vec<IrNode> = children[1..].to_vec();
            if rest.len() == 1 {
                Some(rest.into_iter().next().unwrap())
            } else {
                Some(IrNode::Seq(rest))
            }
        }
        IrNode::Map { inner, fn_id } => {
            let stripped = strip_leading_seq(inner)?;
            Some(IrNode::Map {
                inner: Box::new(stripped),
                fn_id: *fn_id,
            })
        }
        IrNode::OptionalWhitespace(inner) => {
            let stripped = strip_leading_seq(inner)?;
            Some(IrNode::OptionalWhitespace(Box::new(stripped)))
        }
        _ => None,
    }
}
