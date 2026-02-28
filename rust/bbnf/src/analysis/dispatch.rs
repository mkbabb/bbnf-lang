//! Dispatch table construction and FIRST set conflict detection.

use std::collections::HashMap;

use crate::types::{Expression, AST};
use super::charset::CharSet;
use super::first_sets::{FirstSets, compute_expr_first, unwrap_rule};

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
        let branch_firsts: &[(CharSet, bool)] = if let Some(cached) = first_sets.branch_firsts.get(lhs) {
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

/// A lookup table that maps each ASCII character code to the index of the
/// alternative branch that should be tried, or -1 if no branch matches.
#[derive(Clone, Debug)]
pub struct DispatchTable {
    pub table: [i8; 128],
}

impl DispatchTable {
    pub fn lookup(&self, code: u8) -> Option<usize> {
        if code >= 128 {
            return None;
        }
        let idx = self.table[code as usize];
        if idx < 0 { None } else { Some(idx as usize) }
    }
}

/// Attempt to build a dispatch table for a set of alternative expressions.
pub fn build_dispatch_table<'a>(
    alternatives: &[&'a Expression<'a>],
    first_sets: &FirstSets<'a>,
    ast: &'a AST<'a>,
) -> Option<DispatchTable> {
    if alternatives.is_empty() {
        return None;
    }

    let name_to_key: HashMap<&str, &'a Expression<'a>> = ast
        .keys()
        .filter_map(|lhs| match lhs {
            Expression::Nonterminal(tok) => Some((tok.value.as_ref(), lhs)),
            _ => None,
        })
        .collect();

    let mut alt_first_sets: Vec<CharSet> = Vec::with_capacity(alternatives.len());

    for &alt in alternatives {
        let mut cs = CharSet::new();
        let is_nullable = compute_expr_first(alt, &first_sets.first, &first_sets.nullable, &name_to_key, &mut cs);

        if is_nullable {
            return None;
        }

        if cs.is_empty() {
            return None;
        }

        alt_first_sets.push(cs);
    }

    for i in 0..alt_first_sets.len() {
        for j in (i + 1)..alt_first_sets.len() {
            if !alt_first_sets[i].is_disjoint(&alt_first_sets[j]) {
                return None;
            }
        }
    }

    let mut table = [-1i8; 128];
    for (idx, cs) in alt_first_sets.iter().enumerate() {
        if idx > i8::MAX as usize {
            return None;
        }
        for code in cs.iter() {
            table[code as usize] = idx as i8;
        }
    }

    Some(DispatchTable { table })
}
