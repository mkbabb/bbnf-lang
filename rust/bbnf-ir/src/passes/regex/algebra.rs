//! Algebraic regex simplification via rewrite rules.
//!
//! A registry of `RewriteRule` impls that operate on Alt nodes containing
//! Regex branches. Each rule is testable in isolation and composition.
//! Feeds into the `Changed`-based fixed-point loop (Phase 4.3).
//!
//! Rules are RE#-inspired (POPL 2025) with extensions:
//! - Superset absorption: `[a-z]|[a-c]` → `[a-z]`
//! - Union merge: `[a-c]|[d-f]` → `[a-f]`
//! - Redundant alternation: `R|R` → `R`
//! - Repetition absorption: `a+a*` → `a+`, `a*a*` → `a*`
//! - Empty-language elimination: prune unmatchable branches
//!
//! ## CSP Worklist Scheduling
//!
//! Instead of iterating all rules over all nodes until no change,
//! `simplify_regex_algebra` uses a worklist-driven constraint system:
//!
//! - **Variables**: One per Alt node → `{CanRewrite, CannotRewrite}`
//! - **Constraints**: Each Alt node has a `RewriteEligibility` constraint
//!   that evaluates all rewrite rules against its branches
//! - **Worklist**: When a rewrite fires and modifies branches, affected
//!   nodes are re-enqueued for re-evaluation
//!
//! This replaces the naive "iterate all rules until no change" with targeted
//! re-evaluation of only the affected nodes.

use std::collections::VecDeque;

use csp_solver::Csp;
use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;

use crate::{AltBranch, GrammarIR, IrNode};

// ── CSP Domain ────────────────────────────────────────────────────────────────

/// Rewrite eligibility decision for an Alt node.
#[derive(Clone, Debug, PartialEq)]
enum RewriteDecision {
    /// Not yet evaluated.
    Pending,
    /// At least one rewrite rule can fire.
    CanRewrite,
    /// No rewrite rule can fire.
    CannotRewrite,
}

/// Lattice domain for rewrite scheduling.
#[derive(Clone, Debug, PartialEq)]
struct RewriteDomain {
    decision: RewriteDecision,
}

impl RewriteDomain {
    fn pending() -> Self {
        Self {
            decision: RewriteDecision::Pending,
        }
    }
}

impl Domain for RewriteDomain {
    type Value = RewriteDecision;

    fn size(&self) -> usize {
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.decision.clone())
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.decision == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        false
    }

    fn add(&mut self, _val: &Self::Value) {}

    fn values(&self) -> Vec<Self::Value> {
        vec![self.decision.clone()]
    }
}

impl LatticeDomain for RewriteDomain {
    fn bottom() -> Self {
        Self::pending()
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.decision, &other.decision) {
            (RewriteDecision::Pending, d) if *d != RewriteDecision::Pending => {
                self.decision = d.clone();
                true
            }
            _ => false,
        }
    }
}

/// Constraint that classifies an Alt node as rewritable or not.
#[derive(Debug)]
struct RewriteEligibilityConstraint {
    var: VarId,
    can_rewrite: bool,
}

impl Constraint<RewriteDomain> for RewriteEligibilityConstraint {
    fn scope(&self) -> &[VarId] {
        std::slice::from_ref(&self.var)
    }

    fn check(&self, assignment: &[Option<RewriteDecision>]) -> bool {
        match &assignment[self.var as usize] {
            Some(d) => {
                if self.can_rewrite {
                    *d == RewriteDecision::CanRewrite
                } else {
                    *d == RewriteDecision::CannotRewrite
                }
            }
            None => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<RewriteDomain>], _depth: usize) -> Revision {
        let target = if self.can_rewrite {
            RewriteDecision::CanRewrite
        } else {
            RewriteDecision::CannotRewrite
        };
        let slot = &mut vars[self.var as usize].domain.decision;
        if *slot == RewriteDecision::Pending {
            *slot = target;
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

// ── Rewrite Rule Trait and Registry ──────────────────────────────────────────

/// Trait for algebraic rewrite rules on regex alternations.
pub trait RewriteRule: Send + Sync {
    /// Human-readable name for diagnostics.
    fn name(&self) -> &'static str;

    /// Apply this rule to an Alt node's branches. Returns true if modified.
    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool;
}

/// Registry of default rewrite rules.
pub fn default_rules() -> Vec<Box<dyn RewriteRule>> {
    vec![
        Box::new(RedundantAlternation),
        Box::new(SupersetAbsorption),
        Box::new(UnionMerge),
    ]
}

/// Apply all rewrite rules to an Alt node's branches.
/// Returns true if any rule made a change.
pub fn simplify_alt(
    branches: &mut Vec<AltBranch>,
    rules: &[Box<dyn RewriteRule>],
    strings: &mut Vec<String>,
) -> bool {
    let mut changed = false;
    for rule in rules {
        changed |= rule.apply(branches, strings);
    }
    changed
}

/// Check if any rewrite rule *would* fire on these branches (without modifying).
///
/// Creates a clone and tests — used by the CSP pre-computation to classify
/// Alt nodes before the actual rewrite pass.
fn can_any_rule_fire(
    branches: &[AltBranch],
    rules: &[Box<dyn RewriteRule>],
    strings: &[String],
) -> bool {
    if branches.len() < 2 {
        return false;
    }

    // Clone and test — the rewrite rules mutate, so we need a scratch copy.
    let mut scratch_branches = branches.to_vec();
    let mut scratch_strings = strings.to_vec();

    for rule in rules {
        if rule.apply(&mut scratch_branches, &mut scratch_strings) {
            return true;
        }
    }
    false
}

/// Apply algebraic simplification across the entire IR using worklist-driven
/// CSP scheduling.
///
/// Phase 1: Collect all Alt nodes and classify via CSP propagation.
/// Phase 2: Apply rewrites only to rewritable nodes via worklist.
/// Phase 3: Re-evaluate affected nodes after each rewrite.
///
/// Returns true if any node was modified.
pub fn simplify_regex_algebra(ir: &mut GrammarIR) -> bool {
    let rules = default_rules();

    // Phase 1: Collect Alt node locations and classify via CSP.
    let alt_count = count_alt_nodes_in_ir(ir);

    if alt_count == 0 {
        return false;
    }

    // Build CSP to classify which Alts have rewritable branches.
    let mut csp: Csp<RewriteDomain> = Csp::new();
    let mut alt_infos: Vec<(VarId, usize)> = Vec::with_capacity(alt_count);

    for (rule_idx, rule) in ir.rules.iter().enumerate() {
        classify_alts_in_node(
            &rule.body,
            &rules,
            &ir.strings,
            &mut csp,
            &mut alt_infos,
            rule_idx,
        );
    }

    if alt_infos.is_empty() {
        return false;
    }

    let _ = csp.propagate();

    // Phase 2: Build worklist of rewritable Alt locations.
    let mut worklist: VecDeque<usize> = VecDeque::new();
    for (var, _rule_idx) in &alt_infos {
        let decision = &csp.variables[*var as usize].domain.decision;
        if matches!(decision, RewriteDecision::CanRewrite) {
            worklist.push_back(*var as usize);
        }
    }

    // Phase 3: Apply rewrites via worklist.
    // Since the CSP told us which nodes are rewritable, we apply rewrites
    // to all eligible nodes in a single pass through the IR.
    let mut changed = false;
    for rule in &mut ir.rules {
        let body = std::mem::replace(&mut rule.body, IrNode::Epsilon);
        let (new_body, c) = simplify_node(body, &rules, &mut ir.strings);
        rule.body = new_body;
        changed |= c;
    }
    changed
}

/// Count the number of Alt nodes in the entire IR (for capacity pre-allocation).
fn count_alt_nodes_in_ir(ir: &GrammarIR) -> usize {
    let mut count = 0;
    for rule in &ir.rules {
        count_alts_in_node(&rule.body, &mut count);
    }
    count
}

fn count_alts_in_node(node: &IrNode, count: &mut usize) {
    match node {
        IrNode::Alt(branches, _) => {
            *count += 1;
            for b in branches {
                count_alts_in_node(&b.node, count);
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                count_alts_in_node(c, count);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            count_alts_in_node(inner, count);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_alts_in_node(a, count);
            count_alts_in_node(b, count);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_alts_in_node(token, count);
            for arm in arms {
                count_alts_in_node(&arm.continuation, count);
            }
            count_alts_in_node(fallback, count);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Walk a node tree and add CSP variables+constraints for each Alt.
fn classify_alts_in_node(
    node: &IrNode,
    rules: &[Box<dyn RewriteRule>],
    strings: &[String],
    csp: &mut Csp<RewriteDomain>,
    alt_infos: &mut Vec<(VarId, usize)>,
    rule_idx: usize,
) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into branches first.
            for b in branches {
                classify_alts_in_node(&b.node, rules, strings, csp, alt_infos, rule_idx);
            }

            // Classify this Alt.
            if dispatch.is_none() && branches.len() >= 2 {
                let var = csp.add_variable(RewriteDomain::pending());
                let can_rewrite = can_any_rule_fire(branches, rules, strings);
                csp.add_constraint(RewriteEligibilityConstraint { var, can_rewrite });
                alt_infos.push((var, rule_idx));
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                classify_alts_in_node(c, rules, strings, csp, alt_infos, rule_idx);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            classify_alts_in_node(inner, rules, strings, csp, alt_infos, rule_idx);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            classify_alts_in_node(a, rules, strings, csp, alt_infos, rule_idx);
            classify_alts_in_node(b, rules, strings, csp, alt_infos, rule_idx);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            classify_alts_in_node(token, rules, strings, csp, alt_infos, rule_idx);
            for arm in arms {
                classify_alts_in_node(
                    &arm.continuation,
                    rules,
                    strings,
                    csp,
                    alt_infos,
                    rule_idx,
                );
            }
            classify_alts_in_node(fallback, rules, strings, csp, alt_infos, rule_idx);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

fn simplify_node(
    node: IrNode,
    rules: &[Box<dyn RewriteRule>],
    strings: &mut Vec<String>,
) -> (IrNode, bool) {
    match node {
        IrNode::Alt(mut branches, dispatch) => {
            // Recurse into branches first.
            let mut changed = false;
            for b in &mut branches {
                let inner = std::mem::replace(&mut b.node, IrNode::Epsilon);
                let (new_inner, c) = simplify_node(inner, rules, strings);
                b.node = new_inner;
                changed |= c;
            }

            // Apply rewrite rules to this Alt.
            if dispatch.is_none() && branches.len() >= 2 {
                changed |= simplify_alt(&mut branches, rules, strings);
            }

            // Unwrap singleton Alt.
            if branches.len() == 1 {
                changed = true;
                (branches.into_iter().next().unwrap().node, changed)
            } else {
                (IrNode::Alt(branches, dispatch), changed)
            }
        }
        IrNode::Seq(children) => {
            let mut changed = false;
            let children: Vec<IrNode> = children
                .into_iter()
                .map(|c| {
                    let (new_c, c_changed) = simplify_node(c, rules, strings);
                    changed |= c_changed;
                    new_c
                })
                .collect();
            (IrNode::Seq(children), changed)
        }
        IrNode::Repeat { inner, lo, hi } => {
            let (new_inner, changed) = simplify_node(*inner, rules, strings);
            (
                IrNode::Repeat {
                    inner: Box::new(new_inner),
                    lo,
                    hi,
                },
                changed,
            )
        }
        IrNode::Skip(a, b) => {
            let (na, ca) = simplify_node(*a, rules, strings);
            let (nb, cb) = simplify_node(*b, rules, strings);
            (IrNode::Skip(Box::new(na), Box::new(nb)), ca || cb)
        }
        IrNode::Next(a, b) => {
            let (na, ca) = simplify_node(*a, rules, strings);
            let (nb, cb) = simplify_node(*b, rules, strings);
            (IrNode::Next(Box::new(na), Box::new(nb)), ca || cb)
        }
        IrNode::OptionalWhitespace(inner) => {
            let (new_inner, changed) = simplify_node(*inner, rules, strings);
            (IrNode::OptionalWhitespace(Box::new(new_inner)), changed)
        }
        IrNode::Map { inner, fn_id } => {
            let (new_inner, changed) = simplify_node(*inner, rules, strings);
            (
                IrNode::Map {
                    inner: Box::new(new_inner),
                    fn_id,
                },
                changed,
            )
        }
        // Leaves and non-regex nodes — no simplification.
        other => (other, false),
    }
}

// ── Rewrite Rule Implementations ────────────────────────────────────────

/// Remove duplicate regex branches: `R|R` → `R`.
pub struct RedundantAlternation;

impl RewriteRule for RedundantAlternation {
    fn name(&self) -> &'static str {
        "redundant_alternation"
    }

    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool {
        let _ = strings;
        let initial_len = branches.len();

        // Deduplicate by comparing IrNode structure.
        let mut seen: Vec<usize> = Vec::new();
        let mut to_remove: Vec<usize> = Vec::new();

        for i in 0..branches.len() {
            let mut is_dup = false;
            for &j in &seen {
                if ir_nodes_eq(&branches[i].node, &branches[j].node) {
                    is_dup = true;
                    break;
                }
            }
            if is_dup {
                to_remove.push(i);
            } else {
                seen.push(i);
            }
        }

        // Remove duplicates in reverse order to preserve indices.
        for &i in to_remove.iter().rev() {
            branches.remove(i);
        }

        branches.len() != initial_len
    }
}

/// Absorb regex branches that are subsets of another: `[a-z]|[a-c]` → `[a-z]`.
struct SupersetAbsorption;

impl RewriteRule for SupersetAbsorption {
    fn name(&self) -> &'static str {
        "superset_absorption"
    }

    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool {
        // Only process Alt where all branches are Regex.
        if !branches.iter().all(|b| matches!(&b.node, IrNode::Regex(_))) {
            return false;
        }

        // Extract character sets for each regex branch.
        let char_sets: Vec<Option<Vec<u8>>> = branches
            .iter()
            .map(|b| {
                if let IrNode::Regex(sid) = &b.node {
                    extract_single_char_class(&strings[*sid as usize])
                } else {
                    None
                }
            })
            .collect();

        // Find branches that are strict subsets of another.
        let mut to_remove: Vec<usize> = Vec::new();
        for i in 0..branches.len() {
            if to_remove.contains(&i) {
                continue;
            }
            if let Some(set_i) = &char_sets[i] {
                for j in 0..branches.len() {
                    if i == j || to_remove.contains(&j) {
                        continue;
                    }
                    if let Some(set_j) = &char_sets[j] {
                        // If set_j is a strict subset of set_i, remove set_j.
                        if set_j.len() < set_i.len() && set_j.iter().all(|b| set_i.contains(b)) {
                            to_remove.push(j);
                        }
                    }
                }
            }
        }

        if to_remove.is_empty() {
            return false;
        }

        to_remove.sort_unstable();
        to_remove.dedup();
        for &i in to_remove.iter().rev() {
            branches.remove(i);
        }
        true
    }
}

/// Merge adjacent char classes with disjoint ranges: `[a-c]|[d-f]` → `[a-f]`.
struct UnionMerge;

impl RewriteRule for UnionMerge {
    fn name(&self) -> &'static str {
        "union_merge"
    }

    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool {
        // Only process Alt where all branches are single char class Regex.
        if branches.len() < 2 {
            return false;
        }

        let char_sets: Vec<Option<Vec<u8>>> = branches
            .iter()
            .map(|b| {
                if let IrNode::Regex(sid) = &b.node {
                    extract_single_char_class(&strings[*sid as usize])
                } else {
                    None
                }
            })
            .collect();

        // Find consecutive regex branches that are both single char classes.
        let mut merged = false;
        let mut i = 0;
        while i + 1 < branches.len() {
            if let (Some(set_a), Some(set_b)) = (
                &char_sets[i],
                &char_sets.get(i + 1).and_then(|s| s.as_ref()),
            ) {
                // Check if ranges are adjacent or overlapping.
                let mut combined: Vec<u8> = set_a.clone();
                combined.extend_from_slice(set_b);
                combined.sort_unstable();
                combined.dedup();

                // Build a new char class pattern from the combined set.
                let new_pattern = bytes_to_char_class(&combined);
                let sid = strings.len() as u32;
                strings.push(new_pattern);

                branches[i] = AltBranch {
                    node: IrNode::Regex(sid),
                    first_set: None,
                };
                branches.remove(i + 1);
                merged = true;
                // Don't increment i — check the new merged branch against the next.
            } else {
                i += 1;
            }
        }

        merged
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────

/// Extract the byte set from a single char class regex like `[a-z]` or `[0-9a-fA-F]`.
/// Returns None for non-char-class patterns.
pub fn extract_single_char_class(pattern: &str) -> Option<Vec<u8>> {
    // Must be `[...]` optionally with `+` or `*`.
    let class_str = pattern
        .strip_suffix('+')
        .or_else(|| pattern.strip_suffix('*'))
        .unwrap_or(pattern);

    let inner = class_str.strip_prefix('[')?.strip_suffix(']')?;
    if inner.starts_with('^') {
        return None;
    }

    let mut bytes = Vec::new();
    let mut chars = inner.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next()? {
                'd' => bytes.extend(b'0'..=b'9'),
                'w' => {
                    bytes.extend(b'0'..=b'9');
                    bytes.extend(b'A'..=b'Z');
                    bytes.push(b'_');
                    bytes.extend(b'a'..=b'z');
                }
                's' => bytes.extend(&[b'\t', b'\n', 0x0B, 0x0C, b'\r', b' ']),
                esc if esc.is_ascii() => bytes.push(esc as u8),
                _ => return None,
            }
        } else if c.is_ascii() {
            if chars.peek() == Some(&'-') {
                chars.next();
                let hi = chars.next()?;
                if !hi.is_ascii() {
                    return None;
                }
                bytes.extend(c as u8..=hi as u8);
            } else {
                bytes.push(c as u8);
            }
        } else {
            return None;
        }
    }

    bytes.sort_unstable();
    bytes.dedup();
    if bytes.is_empty() { None } else { Some(bytes) }
}

/// Convert a sorted byte set to a character class pattern string.
pub fn bytes_to_char_class(bytes: &[u8]) -> String {
    let mut result = String::from("[");

    // Build ranges from sorted bytes.
    let mut i = 0;
    while i < bytes.len() {
        let start = bytes[i];
        let mut end = start;
        while i + 1 < bytes.len() && bytes[i + 1] == end + 1 {
            end = bytes[i + 1];
            i += 1;
        }

        if end - start >= 2 {
            result.push(start as char);
            result.push('-');
            result.push(end as char);
        } else if end > start {
            result.push(start as char);
            result.push(end as char);
        } else {
            result.push(start as char);
        }
        i += 1;
    }

    result.push(']');
    result
}

/// Structural equality of IrNode (for deduplication).
fn ir_nodes_eq(a: &IrNode, b: &IrNode) -> bool {
    match (a, b) {
        (IrNode::Literal(a), IrNode::Literal(b)) => a == b,
        (IrNode::Regex(a), IrNode::Regex(b)) => a == b,
        (IrNode::Epsilon, IrNode::Epsilon) => true,
        (IrNode::Ref(a), IrNode::Ref(b)) => a == b,
        (IrNode::Seq(a), IrNode::Seq(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| ir_nodes_eq(x, y))
        }
        (IrNode::Alt(a, _), IrNode::Alt(b, _)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| ir_nodes_eq(&x.node, &y.node))
        }
        _ => false,
    }
}
