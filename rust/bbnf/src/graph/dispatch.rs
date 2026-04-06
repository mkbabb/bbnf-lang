//! FIRST set conflict detection for alternation rules.

use std::collections::HashMap;

use super::charset::CharSet;
use super::first_sets::{FirstSets, compute_expr_first, unwrap_rule};
use crate::types::{AST, Expression};

// NOTE: DispatchTable and build_dispatch_table were removed — AST-level dispatch
// is dead code. The IR pass `generate_dispatch_tables` handles dispatch.

/// A FIRST set conflict between two branches of an alternation.
#[derive(Debug, Clone)]
pub struct FirstSetConflict {
    pub branch_a: usize,
    pub branch_b: usize,
    pub overlap: CharSet,
}

/// Find FIRST set conflicts in alternation rules.
pub fn find_first_set_conflicts<'a>(
    ast: &'a AST<'a>,
    first_sets: &FirstSets<'a>,
) -> HashMap<String, Vec<FirstSetConflict>> {
    let name_to_key: HashMap<&str, &'a Expression<'a>> = ast
        .keys()
        .filter_map(|lhs| match lhs {
            Expression::Nonterminal(tok) => Some((tok.value.as_ref(), lhs)),
            _ => None,
        })
        .collect();

    let mut conflicts = HashMap::new();

    for (lhs, rhs) in ast {
        let name = match lhs {
            Expression::Nonterminal(tok) => tok.value.to_string(),
            _ => continue,
        };

        let inner = unwrap_rule(rhs);
        let branches = match inner {
            Expression::Alternation(tok) => &tok.value[..],
            _ => continue,
        };

        if branches.len() < 2 {
            continue;
        }

        if let Some(rule_first) = first_sets.first.get(lhs) {
            if rule_first.len() <= 1 {
                continue;
            }
        }

        let branch_firsts_vec: Vec<(CharSet, bool)>;
        let branch_firsts: &[(CharSet, bool)] =
            if let Some(cached) = first_sets.branch_firsts.get(lhs) {
                cached
            } else {
                branch_firsts_vec = branches
                    .iter()
                    .map(|branch| {
                        let mut cs = CharSet::new();
                        let is_nullable = compute_expr_first(
                            branch,
                            &first_sets.first,
                            &first_sets.nullable,
                            &name_to_key,
                            &mut cs,
                        );
                        (cs, is_nullable)
                    })
                    .collect();
                &branch_firsts_vec
            };

        let mut rule_conflicts = Vec::new();
        let mut union_so_far = CharSet::new();

        for i in 0..branch_firsts.len() {
            let (ref branch_i_first, _) = branch_firsts[i];

            if i > 0 && branch_i_first.is_disjoint(&union_so_far) {
                union_so_far.union(branch_i_first);
                continue;
            }

            for (j, (branch_j_first, _)) in branch_firsts.iter().enumerate().skip(i + 1) {
                let overlap = branch_i_first.intersection(branch_j_first);
                if !overlap.is_empty() {
                    rule_conflicts.push(FirstSetConflict {
                        branch_a: i,
                        branch_b: j,
                        overlap,
                    });
                }
            }

            union_so_far.union(branch_i_first);
        }

        if !rule_conflicts.is_empty() {
            conflicts.insert(name, rule_conflicts);
        }
    }

    conflicts
}
