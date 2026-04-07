use std::collections::{HashMap, HashSet};

use bbnf::graph::get_nonterminal_name;
use bbnf::types::Expression;

use super::types::{ReferenceInfo, RuleInfo, SemanticTokenInfo, token_types};

/// Format a byte as a display-friendly character for inlay hints.
pub fn format_char(b: u8) -> String {
    match b {
        b'\t' => "\\t".into(),
        b'\n' => "\\n".into(),
        b'\r' => "\\r".into(),
        b' ' => "SP".into(),
        0x0b => "\\v".into(),
        0x0c => "\\f".into(),
        c if c.is_ascii_graphic() => format!("'{}'", c as char),
        c => format!("0x{:02x}", c),
    }
}


/// Check if a rule RHS is effectively empty (epsilon only).
pub fn is_empty_rhs(expr: &Expression<'_>) -> bool {
    match expr {
        Expression::Epsilon(_) => true,
        Expression::Rule(rhs, _) => is_empty_rhs(rhs),
        _ => false,
    }
}

/// Public wrapper for `compute_expression_end` (used by selection_range).
pub fn compute_expression_end_pub(expr: &Expression<'_>) -> Option<usize> {
    compute_expression_end(expr)
}

/// Recursively collect nonterminal references from an expression.
pub fn collect_references(expr: &Expression<'_>, refs: &mut Vec<ReferenceInfo>) {
    match expr {
        Expression::Nonterminal(tok) => {
            refs.push(ReferenceInfo {
                name: tok.value.to_string(),
                span: (tok.span.start, tok.span.end),
            });
        }
        Expression::Alternation(inner) | Expression::Concatenation(inner) => {
            for child in inner.inner() {
                collect_references(child, refs);
            }
        }
        Expression::Skip(l, r) | Expression::Next(l, r) | Expression::Minus(l, r) => {
            collect_references(l.inner(), refs);
            collect_references(r.inner(), refs);
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner)
        | Expression::SpanCapture(inner) => {
            collect_references(inner.inner(), refs);
        }
        Expression::Rule(rhs, _) => {
            collect_references(rhs, refs);
        }
        Expression::MappedExpression(expr_tok, _) => {
            collect_references(expr_tok.inner(), refs);
        }
        Expression::DebugExpression((expr_tok, _)) => {
            collect_references(expr_tok.inner(), refs);
        }
        Expression::Closure(_params, body) => {
            collect_references(body.inner(), refs);
        }
        Expression::GrammarCall(name_tok, args) => {
            refs.push(ReferenceInfo {
                name: name_tok.value.to_string(),
                span: (name_tok.span.start, name_tok.span.end),
            });
            for arg in args {
                collect_references(arg, refs);
            }
        }
        _ => {}
    }
}

/// Collect semantic tokens from an expression tree.
pub fn collect_semantic_tokens(expr: &Expression<'_>, tokens: &mut Vec<SemanticTokenInfo>) {
    match expr {
        Expression::Nonterminal(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::RULE_REFERENCE,
            });
        }
        Expression::Literal(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::STRING,
            });
        }
        Expression::Regex(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::REGEXP,
            });
        }
        Expression::Epsilon(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::KEYWORD,
            });
        }
        Expression::Alternation(inner) | Expression::Concatenation(inner) => {
            for child in inner.inner() {
                collect_semantic_tokens(child, tokens);
            }
        }
        Expression::Skip(l, r) | Expression::Next(l, r) | Expression::Minus(l, r) => {
            collect_semantic_tokens(l.inner(), tokens);
            collect_semantic_tokens(r.inner(), tokens);
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner)
        | Expression::SpanCapture(inner) => {
            collect_semantic_tokens(inner.inner(), tokens);
        }
        Expression::Rule(rhs, _) => {
            collect_semantic_tokens(rhs, tokens);
        }
        Expression::MappedExpression(expr_tok, _) => {
            collect_semantic_tokens(expr_tok.inner(), tokens);
        }
        Expression::DebugExpression((expr_tok, _)) => {
            collect_semantic_tokens(expr_tok.inner(), tokens);
        }
        Expression::Closure(_params, body) => {
            collect_semantic_tokens(body.inner(), tokens);
        }
        Expression::GrammarCall(name_tok, args) => {
            tokens.push(SemanticTokenInfo {
                span: (name_tok.span.start, name_tok.span.end),
                token_type: token_types::RULE_REFERENCE,
            });
            for arg in args {
                collect_semantic_tokens(arg, tokens);
            }
        }
        _ => {}
    }
}

/// Compute the end byte offset of an expression.
pub fn compute_expression_end(expr: &Expression<'_>) -> Option<usize> {
    match expr {
        Expression::Literal(tok) | Expression::Nonterminal(tok) | Expression::Regex(tok) => {
            Some(tok.span.end)
        }
        Expression::Epsilon(tok) => Some(tok.span.end),
        Expression::Alternation(inner) | Expression::Concatenation(inner) => inner
            .inner()
            .last()
            .and_then(|e| compute_expression_end(e))
            .or(Some(inner.span.end)),
        Expression::Skip(_, r) | Expression::Next(_, r) | Expression::Minus(_, r) => {
            compute_expression_end(r.inner()).or(Some(r.span.end))
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner)
        | Expression::SpanCapture(inner) => Some(inner.span.end),
        Expression::Rule(rhs, arrow) => {
            if let Some(a) = arrow {
                Some(a.span.end)
            } else {
                compute_expression_end(rhs)
            }
        }
        Expression::ProductionRule(_, rhs) => compute_expression_end(rhs),
        Expression::MappedExpression(_, arrow) => Some(arrow.span.end),
        Expression::DebugExpression((expr_tok, _)) => compute_expression_end(expr_tok.inner()),
        Expression::Closure(_params, body) => Some(body.span.end),
        Expression::GrammarCall(name_tok, args) => {
            if let Some(last) = args.last() {
                compute_expression_end(last)
            } else {
                Some(name_tok.span.end)
            }
        }
    }
}

/// Quick one-line formatting of an expression for hover text.
pub fn format_expression_short(expr: &Expression<'_>) -> String {
    match expr {
        Expression::Literal(tok) => format!("\"{}\"", tok.value),
        Expression::Nonterminal(tok) => tok.value.to_string(),
        Expression::Regex(tok) => format!("/{}/", tok.value),
        Expression::Epsilon(_) => "\u{03b5}".into(),
        Expression::Group(inner) => {
            format!("({})", format_expression_short(inner.inner()))
        }
        Expression::Optional(inner) => {
            format!("[{}]", format_expression_short(inner.inner()))
        }
        Expression::OptionalWhitespace(inner) => {
            format!("{}?w", format_expression_short(inner.inner()))
        }
        Expression::SpanCapture(inner) => {
            format!("@{{{}}}", format_expression_short(inner.inner()))
        }
        Expression::Many(inner) => {
            format!("{{{}}}", format_expression_short(inner.inner()))
        }
        Expression::Many1(inner) => {
            format!("{}+", format_expression_short(inner.inner()))
        }
        Expression::Skip(l, r) => {
            format!(
                "{} << {}",
                format_expression_short(l.inner()),
                format_expression_short(r.inner())
            )
        }
        Expression::Next(l, r) => {
            format!(
                "{} >> {}",
                format_expression_short(l.inner()),
                format_expression_short(r.inner())
            )
        }
        Expression::Minus(l, r) => {
            format!(
                "{} - {}",
                format_expression_short(l.inner()),
                format_expression_short(r.inner())
            )
        }
        Expression::Concatenation(inner) => inner
            .inner()
            .iter()
            .map(|e| format_expression_short(e))
            .collect::<Vec<_>>()
            .join(", "),
        Expression::Alternation(inner) => inner
            .inner()
            .iter()
            .map(|e| format_expression_short(e))
            .collect::<Vec<_>>()
            .join(" | "),
        Expression::Rule(rhs, _) => format_expression_short(rhs),
        Expression::ProductionRule(lhs, rhs) => {
            format!(
                "{} = {}",
                format_expression_short(lhs),
                format_expression_short(rhs)
            )
        }
        Expression::MappedExpression(expr_tok, arrow) => {
            format!("{} -> {}", format_expression_short(expr_tok.inner()), format_value_expr_short(&arrow.expr))
        }
        Expression::DebugExpression((expr_tok, _)) => format_expression_short(expr_tok.inner()),
        Expression::Closure(params, body) => {
            let params_str: Vec<&str> = params.iter().map(|p| p.value.as_ref()).collect();
            format!("|{}| {}", params_str.join(", "), format_expression_short(body.inner()))
        }
        Expression::GrammarCall(name_tok, args) => {
            let args_str: Vec<String> = args.iter().map(|a| format_expression_short(a)).collect();
            format!("{}({})", name_tok.value, args_str.join(", "))
        }
    }
}

/// Quick formatting of a value expression for hover text.
pub fn format_value_expr_short(expr: &bbnf::ValueExpr<'_>) -> String {
    use bbnf::ValueExpr;
    match expr {
        ValueExpr::IntLit(tok) | ValueExpr::FloatLit(tok) => tok.value.to_string(),
        ValueExpr::BoolLit(tok) => if tok.value { "true" } else { "false" }.into(),
        ValueExpr::StringLit(tok) => format!("\"{}\"", tok.value),
        ValueExpr::Input(_) => "input".into(),
        ValueExpr::InputProp(_, prop) => format!("input.{}", prop.value),
        ValueExpr::Ident(tok) => tok.value.to_string(),
        ValueExpr::FnCall(name, args) => {
            let args_str: Vec<String> = args.iter().map(|a| format_value_expr_short(a)).collect();
            format!("{}({})", name.value, args_str.join(", "))
        }
        ValueExpr::BinOp(op, lhs, rhs) => {
            format!("{} {} {}", format_value_expr_short(lhs), op, format_value_expr_short(rhs))
        }
        ValueExpr::UnaryOp(op, inner) => {
            format!("{}{}", op, format_value_expr_short(inner))
        }
        ValueExpr::Paren(inner) => {
            format!("({})", format_value_expr_short(inner))
        }
        ValueExpr::Closure(params, body) => {
            let params_str: Vec<&str> = params.iter().map(|p| p.value.as_ref()).collect();
            format!("|{}| {}", params_str.join(", "), format_value_expr_short(body))
        }
    }
}

/// Build a representative cycle path string for a rule within its SCC.
///
/// Walks the dependency graph from `start` following only edges within the SCC members,
/// until it returns to `start`, producing a string like "expr -> term -> factor -> expr".
///
/// A2: Uses a reverse index (`name_to_deps`) for O(1) lookup of a rule's dependencies
/// instead of scanning all entries in `deps` for each step.
pub fn build_cycle_path(
    start: &str,
    scc_members: &[&str],
    deps: &HashMap<Expression<'_>, HashSet<Expression<'_>>>,
) -> String {
    let member_set: HashSet<&str> = scc_members.iter().copied().collect();

    // A2: Build reverse index from rule name to its dependency set.
    let name_to_deps: HashMap<&str, &HashSet<Expression>> = deps
        .iter()
        .filter_map(|(k, v)| get_nonterminal_name(k).map(|n| (n, v)))
        .collect();

    let mut path = vec![start];
    let mut visited = HashSet::new();
    visited.insert(start);
    let mut current = start;

    loop {
        // A2: O(1) lookup for `current`'s dependencies.
        let mut found_next = false;
        if let Some(dep_set) = name_to_deps.get(current) {
            // Walk deps looking for a member of the SCC.
            for dep in *dep_set {
                if let Some(dep_name) = get_nonterminal_name(dep) {
                    if dep_name == start && path.len() > 1 {
                        // Completed the cycle.
                        path.push(start);
                        return path.join(" \u{2192} ");
                    }
                    if member_set.contains(dep_name) && !visited.contains(dep_name) {
                        visited.insert(dep_name);
                        path.push(dep_name);
                        current = dep_name;
                        found_next = true;
                        break;
                    }
                }
            }
        }
        if !found_next {
            // Couldn't extend the path -- close the cycle back to start.
            path.push(start);
            return path.join(" \u{2192} ");
        }
    }
}

/// Compute the set of rule names reachable from root rules via BFS.
///
/// Root rules are: the first and last rules in the grammar, plus any rule referenced by an import.
pub fn compute_reachable_rules(
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
) -> HashSet<String> {
    let mut reachable = HashSet::new();

    if rules.is_empty() {
        return reachable;
    }

    // The first and last rules are both plausible entry points.
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(rules[0].name.clone());
    reachable.insert(rules[0].name.clone());
    let last = rules.len() - 1;
    if last != 0 {
        queue.push_back(rules[last].name.clone());
        reachable.insert(rules[last].name.clone());
    }

    // BFS from root rules.
    while let Some(current) = queue.pop_front() {
        // Find all rules referenced by `current`.
        if let Some(&idx) = rule_index.get(&current) {
            let rule = &rules[idx];
            for refinfo in &rule.references {
                if !reachable.contains(&refinfo.name) {
                    reachable.insert(refinfo.name.clone());
                    queue.push_back(refinfo.name.clone());
                }
            }
        }
    }

    reachable
}
