//! FIRST set computation for grammar analysis.

use std::collections::HashMap;
use std::collections::HashSet;

use crate::types::{Expression, AST};
use super::charset::CharSet;
use super::deps::Dependencies;
use super::scc::SccResult;
use super::regex_first::regex_first_chars;

/// The result of FIRST set analysis.
#[derive(Debug)]
pub struct FirstSets<'a> {
    pub first: HashMap<&'a Expression<'a>, CharSet>,
    pub nullable: HashSet<&'a Expression<'a>>,
    pub branch_firsts: HashMap<&'a Expression<'a>, Vec<(CharSet, bool)>>,
}

/// Compute FIRST sets and nullability for all nonterminals in the grammar.
pub fn compute_first_sets<'a>(
    ast: &'a AST<'a>,
    deps: &Dependencies<'a>,
    scc_result: &SccResult<'a>,
) -> FirstSets<'a> {
    let mut first: HashMap<&'a Expression<'a>, CharSet> = HashMap::new();
    let mut nullable: HashSet<&'a Expression<'a>> = HashSet::new();

    let name_to_key: HashMap<&str, &'a Expression<'a>> = ast
        .keys()
        .filter_map(|lhs| match lhs {
            Expression::Nonterminal(tok) => Some((tok.value.as_ref(), lhs)),
            _ => None,
        })
        .collect();

    let expr_to_rhs: HashMap<&'a Expression<'a>, &'a Expression<'a>> = ast
        .iter()
        .collect();

    for (lhs, _) in ast {
        first.entry(lhs).or_default();
    }

    for scc in &scc_result.sccs {
        if scc.len() == 1 && !scc_result.cyclic_rules.contains(scc[0]) {
            let lhs = scc[0];
            if let Some(&rhs) = expr_to_rhs.get(lhs) {
                let rhs_expr = unwrap_rule(rhs);
                let mut expr_first = CharSet::new();
                let expr_nullable = compute_expr_first(
                    rhs_expr, &first, &nullable, &name_to_key, &mut expr_first,
                );
                let entry = first.entry(lhs).or_default();
                entry.union(&expr_first);
                if expr_nullable {
                    nullable.insert(lhs);
                }
            }
        } else {
            loop {
                let mut changed = false;
                let mut memo_cache: HashMap<*const Expression<'a>, (CharSet, bool)> = HashMap::new();
                for &lhs in scc {
                    if let Some(&rhs) = expr_to_rhs.get(lhs) {
                        let rhs_expr = unwrap_rule(rhs);
                        let mut expr_first = CharSet::new();
                        let expr_nullable = compute_expr_first_memoized(
                            rhs_expr, &first, &nullable, &name_to_key, &mut expr_first, &mut memo_cache,
                        );
                        let entry = first.entry(lhs).or_default();
                        let old_bits = entry.bits;
                        entry.union(&expr_first);
                        if entry.bits != old_bits {
                            changed = true;
                        }
                        if expr_nullable && !nullable.contains(lhs) {
                            nullable.insert(lhs);
                            changed = true;
                        }
                    }
                }
                if !changed {
                    break;
                }
            }
        }
    }

    let _ = deps;

    let mut branch_firsts: HashMap<&'a Expression<'a>, Vec<(CharSet, bool)>> = HashMap::new();
    for (lhs, rhs) in ast.iter() {
        let inner = unwrap_rule(rhs);
        if let Expression::Alternation(tok) = inner {
            let branches = &tok.value[..];
            let per_branch: Vec<(CharSet, bool)> = branches
                .iter()
                .map(|branch| {
                    let mut cs = CharSet::new();
                    let is_nullable = compute_expr_first(
                        branch, &first, &nullable, &name_to_key, &mut cs,
                    );
                    (cs, is_nullable)
                })
                .collect();
            branch_firsts.insert(lhs, per_branch);
        }
    }

    FirstSets { first, nullable, branch_firsts }
}

fn compute_expr_first_memoized<'a>(
    expr: &'a Expression<'a>,
    first_sets: &HashMap<&'a Expression<'a>, CharSet>,
    nullable_set: &HashSet<&'a Expression<'a>>,
    name_to_key: &HashMap<&str, &'a Expression<'a>>,
    out: &mut CharSet,
    cache: &mut HashMap<*const Expression<'a>, (CharSet, bool)>,
) -> bool {
    let ptr = expr as *const Expression<'a>;
    if let Some((cached_cs, cached_nullable)) = cache.get(&ptr) {
        out.union(cached_cs);
        return *cached_nullable;
    }
    let mut local_out = CharSet::new();
    let is_nullable = compute_expr_first(expr, first_sets, nullable_set, name_to_key, &mut local_out);
    out.union(&local_out);
    cache.insert(ptr, (local_out, is_nullable));
    is_nullable
}

pub(crate) fn unwrap_rule<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::Rule(inner, _) => inner,
        other => other,
    }
}

pub(crate) fn compute_expr_first<'a>(
    expr: &'a Expression<'a>,
    first_sets: &HashMap<&'a Expression<'a>, CharSet>,
    nullable_set: &HashSet<&'a Expression<'a>>,
    name_to_key: &HashMap<&str, &'a Expression<'a>>,
    out: &mut CharSet,
) -> bool {
    match expr {
        Expression::Literal(token) => {
            let s: &str = &token.value;
            // Unescape to get the actual first byte (e.g. `\"` → `"`, `\\` → `\`)
            let first_byte = if s.starts_with('\\') {
                match s.as_bytes().get(1) {
                    Some(b'n') => Some(b'\n'),
                    Some(b't') => Some(b'\t'),
                    Some(b'r') => Some(b'\r'),
                    Some(b'\\') => Some(b'\\'),
                    Some(b'\'') => Some(b'\''),
                    Some(b'"') => Some(b'"'),
                    Some(b'0') => Some(b'\0'),
                    Some(&other) => Some(other),
                    None => Some(b'\\'),
                }
            } else {
                s.bytes().next()
            };
            if let Some(b) = first_byte {
                if b < 128 {
                    out.add(b);
                }
            }
            s.is_empty()
        }

        Expression::Regex(token) => {
            let pattern: &str = &token.value;
            if let Some(cs) = regex_first_chars(pattern) {
                out.union(&cs);
            }
            false
        }

        Expression::Nonterminal(token) => {
            let name: &str = &token.value;
            if let Some(&key) = name_to_key.get(name) {
                if let Some(fs) = first_sets.get(key) {
                    out.union(fs);
                }
                return nullable_set.contains(key);
            }
            false
        }

        Expression::Concatenation(token) => {
            let exprs = &token.value;
            for child in exprs {
                let child_nullable = compute_expr_first(child, first_sets, nullable_set, name_to_key, out);
                if !child_nullable {
                    return false;
                }
            }
            true
        }

        Expression::Alternation(token) => {
            let exprs = &token.value;
            let mut any_nullable = false;
            for child in exprs {
                let child_nullable = compute_expr_first(child, first_sets, nullable_set, name_to_key, out);
                if child_nullable {
                    any_nullable = true;
                }
            }
            any_nullable
        }

        Expression::Optional(inner) | Expression::Many(inner) | Expression::OptionalWhitespace(inner) => {
            compute_expr_first(&inner.value, first_sets, nullable_set, name_to_key, out);
            true
        }

        Expression::Many1(inner) => {
            compute_expr_first(&inner.value, first_sets, nullable_set, name_to_key, out)
        }

        Expression::Group(inner) => {
            compute_expr_first(&inner.value, first_sets, nullable_set, name_to_key, out)
        }

        Expression::Epsilon(_) => {
            true
        }

        Expression::Rule(inner, _) => {
            compute_expr_first(inner, first_sets, nullable_set, name_to_key, out)
        }

        Expression::Skip(left, _right) => {
            compute_expr_first(&left.value, first_sets, nullable_set, name_to_key, out)
        }

        Expression::Next(_left, right) => {
            let left_nullable = compute_expr_first(&_left.value, first_sets, nullable_set, name_to_key, out);
            if left_nullable {
                compute_expr_first(&right.value, first_sets, nullable_set, name_to_key, out);
            }
            false
        }

        Expression::Minus(left, _right) => {
            compute_expr_first(&left.value, first_sets, nullable_set, name_to_key, out)
        }

        Expression::MappedExpression((inner, _)) | Expression::DebugExpression((inner, _)) => {
            compute_expr_first(&inner.value, first_sets, nullable_set, name_to_key, out)
        }

        Expression::MappingFn(_) | Expression::ProductionRule(_, _) => {
            false
        }
    }
}
