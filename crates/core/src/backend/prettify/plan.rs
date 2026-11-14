//! Backend-agnostic prettify planning: build per-rule plans from IR metadata.

use bbnf_ir::{GrammarIR, IrNode, IrRule};

use super::types::{PrettyPolicy, PrettyRulePlan, SeparatorPolicy, WrapperPolicy};

/// Build prettify plans for all rules in the grammar.
pub fn build_rule_plans(ir: &GrammarIR) -> Vec<PrettyRulePlan> {
    let mut ref_counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        count_refs_vec(&rule.body, &mut ref_counts);
    }

    ir.rules
        .iter()
        .enumerate()
        .map(|(idx, rule)| {
            let policy = PrettyPolicy::from_rule(rule, ir);
            let inline = should_inline_rule(rule, &policy, ref_counts[idx]);
            PrettyRulePlan { inline, policy }
        })
        .collect()
}

impl PrettyPolicy {
    /// Derive formatting policy from `@pretty` directive hints on a rule.
    pub fn from_rule(rule: &IrRule, ir: &GrammarIR) -> Self {
        let hints = rule.meta.directives.pretty.as_ref();
        let wrapper = match hints {
            Some(h) if h.off => WrapperPolicy::Off,
            Some(h) if h.block && h.indent => WrapperPolicy::BlockIndent,
            Some(h) if h.block => WrapperPolicy::Block,
            Some(h) if h.group && h.indent => WrapperPolicy::GroupIndent,
            Some(h) if h.group => WrapperPolicy::Group,
            _ => WrapperPolicy::None,
        };

        let separator = match hints {
            Some(h) if h.split.is_some() => SeparatorPolicy::None,
            Some(h) if h.blankline => SeparatorPolicy::Blankline,
            Some(h) if h.block || h.hardbreak || h.fast => SeparatorPolicy::Hardline,
            Some(h) if h.nobreak => SeparatorPolicy::Space,
            Some(h) if h.compact || h.off => SeparatorPolicy::None,
            Some(h) if h.softbreak => SeparatorPolicy::Softline,
            Some(h) => h
                .sep
                .as_ref()
                .map(|sep| SeparatorPolicy::Custom(sep.clone()))
                .unwrap_or(SeparatorPolicy::None),
            None => SeparatorPolicy::None,
        };

        let is_ws_rule =
            if let (IrNode::Regex(body_sid), Some(ws_sid)) = (&rule.body, ir.ws_pattern) {
                *body_sid == ws_sid
            } else {
                false
            };

        let split = hints.and_then(|h| h.split.clone());
        let reads_to_line_end = !is_ws_rule && ends_reading_to_line_end(&rule.body, ir);

        Self {
            wrapper,
            separator,
            is_ws_rule,
            split,
            reads_to_line_end,
        }
    }
}

/// Whether every way `node` can end is a regex that ends in `.*`: an
/// unbounded repetition of the class `.` reads (every byte but a line feed),
/// so the match runs to its line end.
fn ends_reading_to_line_end(node: &IrNode, ir: &GrammarIR) -> bool {
    match node {
        IrNode::Regex(sid) => regex_reads_to_line_end(ir.get_string(*sid)),
        IrNode::Seq(items) => items
            .last()
            .is_some_and(|last| ends_reading_to_line_end(last, ir)),
        IrNode::Alt(branches, _) => {
            !branches.is_empty()
                && branches
                    .iter()
                    .all(|branch| ends_reading_to_line_end(&branch.node, ir))
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            ends_reading_to_line_end(inner, ir)
        }
        _ => false,
    }
}

fn regex_reads_to_line_end(pattern: &str) -> bool {
    use parse_that_regex::Hir;
    fn last(hir: &Hir) -> &Hir {
        match hir {
            Hir::Group(inner) => last(inner),
            Hir::Concat(items) => items.last().map_or(hir, last),
            _ => hir,
        }
    }
    let (Ok(hir), Ok(dot)) = (parse_that_regex::parse(pattern), parse_that_regex::parse(".")) else {
        return false;
    };
    matches!(last(&hir), Hir::Repetition(rep) if rep.max.is_none() && *last(&rep.sub) == dot)
}

fn should_inline_rule(rule: &IrRule, policy: &PrettyPolicy, ref_count: u32) -> bool {
    // A rule that reads to its line end keeps its own function: the line
    // break it owes is placed there (grammar.rs).
    if policy.split.is_some() || policy.is_ws_rule || policy.reads_to_line_end {
        return false;
    }
    if rule.meta.is_cyclic || rule.meta.directives.recover.is_some() {
        return false;
    }
    let cost = estimate_prettify_cost(&rule.body);
    if ref_count > 1 && cost > 3 {
        return false;
    }
    cost <= 6 && max_alt_branches(&rule.body) <= 2
}

/// Estimate the codegen cost of a prettify expression (for inline decisions).
pub fn estimate_prettify_cost(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => 1,
        IrNode::Ref(_) => 6,
        IrNode::Seq(children) => 1 + children.iter().map(estimate_prettify_cost).sum::<usize>(),
        IrNode::Alt(branches, _) => {
            4 + branches
                .iter()
                .map(|b| estimate_prettify_cost(&b.node))
                .sum::<usize>()
        }
        IrNode::Repeat { inner, .. } => 7 + estimate_prettify_cost(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            estimate_prettify_cost(a) + estimate_prettify_cost(b)
        }
        IrNode::Negate(inner) => 2 + estimate_prettify_cost(inner),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            1 + estimate_prettify_cost(inner)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            5 + estimate_prettify_cost(token)
                + arms
                    .iter()
                    .map(|arm| estimate_prettify_cost(&arm.continuation))
                    .sum::<usize>()
                + estimate_prettify_cost(fallback)
        }
    }
}

fn max_alt_branches(node: &IrNode) -> usize {
    match node {
        IrNode::Alt(branches, _) => {
            let inner_max = branches
                .iter()
                .map(|b| max_alt_branches(&b.node))
                .max()
                .unwrap_or(0);
            branches.len().max(inner_max)
        }
        IrNode::Seq(children) => children.iter().map(max_alt_branches).max().unwrap_or(0),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => max_alt_branches(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            max_alt_branches(a).max(max_alt_branches(b))
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            let arm_max = arms
                .iter()
                .map(|arm| max_alt_branches(&arm.continuation))
                .max()
                .unwrap_or(0);
            max_alt_branches(token)
                .max(arm_max)
                .max(max_alt_branches(fallback))
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 0,
    }
}

fn count_refs_vec(node: &IrNode, counts: &mut [u32]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(c) = counts.get_mut(*id as usize) {
                *c += 1;
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                count_refs_vec(child, counts);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                count_refs_vec(&branch.node, counts);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => count_refs_vec(inner, counts),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs_vec(a, counts);
            count_refs_vec(b, counts);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_refs_vec(token, counts);
            for arm in arms {
                count_refs_vec(&arm.continuation, counts);
            }
            count_refs_vec(fallback, counts);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}
