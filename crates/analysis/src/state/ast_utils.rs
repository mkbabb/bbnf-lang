use std::collections::{HashMap, HashSet};

use bbnf::grammar::generated::BbnfBootstrapEnum;

use super::types::{token_types, ReferenceInfo, RuleInfo, SemanticTokenInfo};

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
pub fn is_empty_rhs(node: &BbnfBootstrapEnum<'_>) -> bool {
    match node {
        BbnfBootstrapEnum::term_0(_) => true,
        // Unwrap structural wrappers
        BbnfBootstrapEnum::term(inner) => is_empty_rhs(inner),
        BbnfBootstrapEnum::factor((_, inner, None, _)) => is_empty_rhs(inner),
        BbnfBootstrapEnum::mapped_factor((inner, None)) => is_empty_rhs(inner),
        BbnfBootstrapEnum::binary_factor((first, rest)) if rest.is_empty() => is_empty_rhs(first),
        BbnfBootstrapEnum::concatenation(parts) if parts.len() == 1 => is_empty_rhs(parts[0].0),
        BbnfBootstrapEnum::alternation(branches) if branches.len() == 1 => {
            is_empty_rhs(branches[0].0)
        }
        _ => false,
    }
}

/// Public wrapper for `compute_expression_end` (used by selection_range).
pub fn compute_expression_end_pub(node: &BbnfBootstrapEnum<'_>) -> Option<usize> {
    compute_expression_end(node)
}

/// Recursively collect nonterminal references from a bootstrap AST node.
pub fn collect_references(node: &BbnfBootstrapEnum<'_>, refs: &mut Vec<ReferenceInfo>) {
    match node {
        // Leaf: identifier reference
        BbnfBootstrapEnum::identifier(s) => {
            refs.push(ReferenceInfo {
                name: s.as_str().to_string(),
                span: (s.start, s.end),
            });
        }

        // term_1: identifier + optional call args
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = bbnf::grammar::host::extract_span_text(ident);
            let ident_span = match ident {
                BbnfBootstrapEnum::identifier(s) => (s.start, s.end),
                _ => (0, 0),
            };
            refs.push(ReferenceInfo {
                name: name.to_string(),
                span: ident_span,
            });
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                collect_references(first_arg, refs);
                for (_comma, arg) in *rest_args {
                    collect_references(arg, refs);
                }
            }
        }

        // Structural: alternation, concatenation
        BbnfBootstrapEnum::alternation(branches) => {
            for (branch, _pipe) in *branches {
                collect_references(branch, refs);
            }
        }
        BbnfBootstrapEnum::concatenation(parts) => {
            for (part, _comma) in *parts {
                collect_references(part, refs);
            }
        }

        // Binary factor: first + [(op, operand)]
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            collect_references(first, refs);
            for (_, operand) in *rest {
                collect_references(operand, refs);
            }
        }

        // Mapped factor: inner + optional mapping
        BbnfBootstrapEnum::mapped_factor((inner, _mapping)) => {
            collect_references(inner, refs);
        }

        // Factor: (comment?, term, modifier?, comment?)
        BbnfBootstrapEnum::factor((_, term, _, _)) => {
            collect_references(term, refs);
        }

        // Term variants
        BbnfBootstrapEnum::term(inner) => {
            collect_references(inner, refs);
        }
        BbnfBootstrapEnum::term_2((_open, inner, _close)) => {
            collect_references(inner, refs);
        }

        // Closure: |params| body
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => {
            collect_references(body, refs);
        }

        // Terminals: literal, regex, modifier, epsilon, comments — no refs
        BbnfBootstrapEnum::literal(_)
        | BbnfBootstrapEnum::regex(_)
        | BbnfBootstrapEnum::modifier(_)
        | BbnfBootstrapEnum::term_0(_)
        | BbnfBootstrapEnum::comment(_)
        | BbnfBootstrapEnum::big_comment(_)
        | BbnfBootstrapEnum::binary_operators(_) => {}

        // Value expression variants — no grammar nonterminal refs
        BbnfBootstrapEnum::value_or(_)
        | BbnfBootstrapEnum::value_and(_)
        | BbnfBootstrapEnum::value_cmp(_)
        | BbnfBootstrapEnum::value_add(_)
        | BbnfBootstrapEnum::value_mul(_)
        | BbnfBootstrapEnum::value_unary(_)
        | BbnfBootstrapEnum::value_unary_0(_)
        | BbnfBootstrapEnum::value_atom(_)
        | BbnfBootstrapEnum::value_atom_0(_)
        | BbnfBootstrapEnum::value_input(_)
        | BbnfBootstrapEnum::value_fn_call(_)
        | BbnfBootstrapEnum::value_closure(_)
        | BbnfBootstrapEnum::int_lit(_)
        | BbnfBootstrapEnum::float_lit(_)
        | BbnfBootstrapEnum::bool_lit(_)
        | BbnfBootstrapEnum::string_lit(_)
        | BbnfBootstrapEnum::value_ident(_)
        | BbnfBootstrapEnum::type_annotation(_)
        | BbnfBootstrapEnum::type_name(_)
        | BbnfBootstrapEnum::cmp_op(_)
        | BbnfBootstrapEnum::mul_op(_)
        | BbnfBootstrapEnum::add_op(_) => {}

        // Directive variants — no grammar nonterminal refs
        BbnfBootstrapEnum::import_directive(_)
        | BbnfBootstrapEnum::import_directive_0(_)
        | BbnfBootstrapEnum::import_path(_)
        | BbnfBootstrapEnum::import_items(_)
        | BbnfBootstrapEnum::recover_directive(_)
        | BbnfBootstrapEnum::pretty_directive(_)
        | BbnfBootstrapEnum::pretty_directive_0(_)
        | BbnfBootstrapEnum::ws_directive(_)
        | BbnfBootstrapEnum::token_directive(_)
        | BbnfBootstrapEnum::debug_directive(_)
        | BbnfBootstrapEnum::debug_directive_0(_)
        | BbnfBootstrapEnum::host_directive(_) => {}

        // Grammar/rule level — should not appear inside rule RHS
        BbnfBootstrapEnum::grammar(_)
        | BbnfBootstrapEnum::rule(_)
        | BbnfBootstrapEnum::directive(_) => {}

        // Phantom + catch-all
        _ => {}
    }
}

/// Collect semantic tokens from a bootstrap AST node.
pub fn collect_semantic_tokens(node: &BbnfBootstrapEnum<'_>, tokens: &mut Vec<SemanticTokenInfo>) {
    match node {
        // Identifier — nonterminal reference
        BbnfBootstrapEnum::identifier(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::RULE_REFERENCE,
            });
        }

        // term_1: identifier + optional call args
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let ident_span = match ident {
                BbnfBootstrapEnum::identifier(s) => (s.start, s.end),
                _ => (0, 0),
            };
            tokens.push(SemanticTokenInfo {
                span: ident_span,
                token_type: token_types::RULE_REFERENCE,
            });
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                collect_semantic_tokens(first_arg, tokens);
                for (_comma, arg) in *rest_args {
                    collect_semantic_tokens(arg, tokens);
                }
            }
        }

        // Literals
        BbnfBootstrapEnum::literal(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::STRING,
            });
        }

        // Regex
        BbnfBootstrapEnum::regex(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::REGEXP,
            });
        }

        // Epsilon
        BbnfBootstrapEnum::term_0(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::KEYWORD,
            });
        }

        // Structural: alternation, concatenation
        BbnfBootstrapEnum::alternation(branches) => {
            for (branch, _pipe) in *branches {
                collect_semantic_tokens(branch, tokens);
            }
        }
        BbnfBootstrapEnum::concatenation(parts) => {
            for (part, _comma) in *parts {
                collect_semantic_tokens(part, tokens);
            }
        }

        // Binary factor
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            collect_semantic_tokens(first, tokens);
            for (_, operand) in *rest {
                collect_semantic_tokens(operand, tokens);
            }
        }

        // Mapped factor
        BbnfBootstrapEnum::mapped_factor((inner, _mapping)) => {
            collect_semantic_tokens(inner, tokens);
        }

        // Factor
        BbnfBootstrapEnum::factor((_, term, _, _)) => {
            collect_semantic_tokens(term, tokens);
        }

        // Term variants
        BbnfBootstrapEnum::term(inner) => {
            collect_semantic_tokens(inner, tokens);
        }
        BbnfBootstrapEnum::term_2((_open, inner, _close)) => {
            collect_semantic_tokens(inner, tokens);
        }

        // Closure
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => {
            collect_semantic_tokens(body, tokens);
        }

        // Everything else — no tokens to emit
        _ => {}
    }
}

/// Compute the end byte offset of a bootstrap AST node.
pub fn compute_expression_end(node: &BbnfBootstrapEnum<'_>) -> Option<usize> {
    match node {
        // Span leaves
        BbnfBootstrapEnum::identifier(s)
        | BbnfBootstrapEnum::literal(s)
        | BbnfBootstrapEnum::regex(s)
        | BbnfBootstrapEnum::term_0(s)
        | BbnfBootstrapEnum::modifier(s)
        | BbnfBootstrapEnum::binary_operators(s)
        | BbnfBootstrapEnum::comment(s)
        | BbnfBootstrapEnum::big_comment(s) => Some(s.end),

        // Alternation: end of last branch's pipe separator
        BbnfBootstrapEnum::alternation(branches) => {
            branches.last().map(|(branch, pipe)| {
                // Use the pipe separator end if it's non-empty, otherwise the branch end
                if pipe.end > pipe.start {
                    pipe.end
                } else {
                    compute_expression_end(branch).unwrap_or(pipe.end)
                }
            })
        }

        // Concatenation: end of last part's comma separator
        BbnfBootstrapEnum::concatenation(parts) => parts.last().map(|(part, comma)| {
            if comma.end > comma.start {
                comma.end
            } else {
                compute_expression_end(part).unwrap_or(comma.end)
            }
        }),

        // Binary factor: end of last operand, or first if no rest
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            if let Some((_, last_operand)) = rest.last() {
                compute_expression_end(last_operand)
            } else {
                compute_expression_end(first)
            }
        }

        // Mapped factor: end of mapping if present, else inner
        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            if let Some((_arrow, (value_expr, type_ann))) = mapping {
                if let Some(ta) = type_ann {
                    compute_expression_end(ta)
                } else {
                    compute_expression_end(value_expr)
                }
            } else {
                compute_expression_end(inner)
            }
        }

        // Factor: rightmost non-None component
        BbnfBootstrapEnum::factor((_, term, modifier, trailing_comment)) => {
            if let Some(tc) = trailing_comment {
                compute_expression_end(tc)
            } else if let Some(m) = modifier {
                compute_expression_end(m)
            } else {
                compute_expression_end(term)
            }
        }

        // Term: delegate to inner
        BbnfBootstrapEnum::term(inner) => compute_expression_end(inner),

        // term_1: identifier + optional call
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            if let Some((_open, _first, _rest, close)) = call_args {
                Some(close.end)
            } else {
                compute_expression_end(ident)
            }
        }

        // term_2: grouped (open, inner, close)
        BbnfBootstrapEnum::term_2((_open, _inner, close)) => Some(close.end),

        // Closure: end of body
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => compute_expression_end(body),

        // Value expression leaves
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => Some(s.end),

        // Value expression compounds — recurse to find end
        BbnfBootstrapEnum::value_or((first, rest))
        | BbnfBootstrapEnum::value_and((first, rest)) => {
            if let Some((_, last)) = rest.last() {
                compute_expression_end(last)
            } else {
                compute_expression_end(first)
            }
        }
        BbnfBootstrapEnum::value_cmp((first, rest))
        | BbnfBootstrapEnum::value_add((first, rest))
        | BbnfBootstrapEnum::value_mul((first, rest)) => {
            if let Some((_, last)) = rest.last() {
                compute_expression_end(last)
            } else {
                compute_expression_end(first)
            }
        }
        BbnfBootstrapEnum::value_unary(inner) | BbnfBootstrapEnum::value_atom(inner) => {
            compute_expression_end(inner)
        }
        BbnfBootstrapEnum::value_unary_0((_op, inner)) => compute_expression_end(inner),
        BbnfBootstrapEnum::value_atom_0((_open, _inner, close)) => Some(close.end),
        BbnfBootstrapEnum::value_fn_call((_name, _open, _args, close)) => Some(close.end),
        BbnfBootstrapEnum::value_input((ident, props)) => {
            if let Some((_, last)) = props.last() {
                compute_expression_end(last)
            } else {
                Some(ident.end)
            }
        }
        BbnfBootstrapEnum::value_closure((_pipe, _first_param, _params, _pipe2, body)) => {
            compute_expression_end(body)
        }
        BbnfBootstrapEnum::type_annotation((_colon, ty)) => compute_expression_end(ty),
        BbnfBootstrapEnum::type_name(s) => Some(s.end),

        // Directives, grammar-level — shouldn't appear in RHS but handle gracefully
        _ => None,
    }
}

/// Quick one-line formatting of a bootstrap AST node for hover text.
pub fn format_expression_short(node: &BbnfBootstrapEnum<'_>) -> String {
    match node {
        BbnfBootstrapEnum::identifier(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::literal(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::regex(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::term_0(_) => "\u{03b5}".into(),

        BbnfBootstrapEnum::alternation(branches) => branches
            .iter()
            .map(|(branch, _)| format_expression_short(branch))
            .collect::<Vec<_>>()
            .join(" | "),

        BbnfBootstrapEnum::concatenation(parts) => parts
            .iter()
            .map(|(part, _)| format_expression_short(part))
            .collect::<Vec<_>>()
            .join(", "),

        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            if rest.is_empty() {
                format_expression_short(first)
            } else {
                let mut s = format_expression_short(first);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        format_expression_short(op),
                        format_expression_short(operand)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            let inner_str = format_expression_short(inner);
            if let Some((_arrow, (value_expr, type_ann))) = mapping {
                let val_str = format_value_expr_short(value_expr);
                if let Some(ta) = type_ann {
                    format!(
                        "{} -> {} : {}",
                        inner_str,
                        val_str,
                        format_value_expr_short(ta)
                    )
                } else {
                    format!("{} -> {}", inner_str, val_str)
                }
            } else {
                inner_str
            }
        }

        BbnfBootstrapEnum::factor((_comment, term, modifier, _trailing)) => {
            let term_str = format_expression_short(term);
            if let Some(m) = modifier {
                format!("{}{}", term_str, format_expression_short(m))
            } else {
                term_str
            }
        }

        BbnfBootstrapEnum::term(inner) => format_expression_short(inner),

        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = bbnf::grammar::host::extract_span_text(ident);
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                let mut args = vec![format_expression_short(first_arg)];
                for (_comma, arg) in *rest_args {
                    args.push(format_expression_short(arg));
                }
                format!("{}({})", name, args.join(", "))
            } else {
                name.to_string()
            }
        }

        BbnfBootstrapEnum::term_2((open, inner, _close)) => {
            let inner_str = format_expression_short(inner);
            let bracket = open.as_str();
            match bracket {
                "(" => format!("({})", inner_str),
                "[" => format!("[{}]", inner_str),
                "{" => format!("{{{}}}", inner_str),
                _ => format!("({})", inner_str),
            }
        }

        BbnfBootstrapEnum::modifier(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::binary_operators(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            let first_name = bbnf::grammar::host::extract_span_text(first_param);
            let mut param_names: Vec<&str> = vec![first_name];
            for (_comma, p) in *rest_params {
                if let BbnfBootstrapEnum::identifier(s) = p {
                    param_names.push(s.as_str());
                }
            }
            format!(
                "|{}| {}",
                param_names.join(", "),
                format_expression_short(body)
            )
        }

        // Value expression formatting
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::comment(_) | BbnfBootstrapEnum::big_comment(_) => String::new(),

        _ => "...".into(),
    }
}

/// Quick formatting of a value expression for hover text.
pub fn format_value_expr_short(node: &BbnfBootstrapEnum<'_>) -> String {
    match node {
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::value_input((ident, props)) => {
            if props.is_empty() {
                ident.as_str().to_string()
            } else {
                let mut s = ident.as_str().to_string();
                for (_dot, prop) in *props {
                    s.push('.');
                    s.push_str(&format_value_expr_short(prop));
                }
                s
            }
        }

        BbnfBootstrapEnum::value_fn_call((name, _open, args, _close)) => {
            let name_str = bbnf::grammar::host::extract_span_text(name);
            if let Some((first, rest)) = args {
                let mut arg_strs = vec![format_value_expr_short(first)];
                for (_comma, arg) in *rest {
                    arg_strs.push(format_value_expr_short(arg));
                }
                format!("{}({})", name_str, arg_strs.join(", "))
            } else {
                format!("{}()", name_str)
            }
        }

        BbnfBootstrapEnum::value_or((first, rest))
        | BbnfBootstrapEnum::value_and((first, rest)) => {
            if rest.is_empty() {
                format_value_expr_short(first)
            } else {
                let mut s = format_value_expr_short(first);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        op.as_str(),
                        format_value_expr_short(operand)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::value_cmp((first, rest))
        | BbnfBootstrapEnum::value_add((first, rest))
        | BbnfBootstrapEnum::value_mul((first, rest)) => {
            if rest.is_empty() {
                format_value_expr_short(first)
            } else {
                let mut s = format_value_expr_short(first);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        format_value_expr_short(op),
                        format_value_expr_short(operand)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::value_unary(inner) | BbnfBootstrapEnum::value_atom(inner) => {
            format_value_expr_short(inner)
        }

        BbnfBootstrapEnum::value_unary_0((op, inner)) => {
            format!("{}{}", op.as_str(), format_value_expr_short(inner))
        }

        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => {
            format!("({})", format_value_expr_short(inner))
        }

        BbnfBootstrapEnum::value_closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            let first_name = bbnf::grammar::host::extract_span_text(first_param);
            let mut param_names: Vec<&str> = vec![first_name];
            for (_comma, p) in *rest_params {
                if let BbnfBootstrapEnum::value_ident(s) = p {
                    param_names.push(s.as_str());
                } else if let BbnfBootstrapEnum::identifier(s) = p {
                    param_names.push(s.as_str());
                }
            }
            format!(
                "|{}| {}",
                param_names.join(", "),
                format_value_expr_short(body)
            )
        }

        BbnfBootstrapEnum::type_annotation((_colon, ty)) => {
            format!(": {}", format_value_expr_short(ty))
        }
        BbnfBootstrapEnum::type_name(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::cmp_op(s)
        | BbnfBootstrapEnum::mul_op(s)
        | BbnfBootstrapEnum::add_op(s) => s.as_str().to_string(),

        _ => "...".into(),
    }
}

/// Build a representative cycle path string for a rule within its SCC.
///
/// Walks the dependency graph from `start` following only edges within the SCC members,
/// until it returns to `start`, producing a string like "expr -> term -> factor -> expr".
pub fn build_cycle_path(
    start: &str,
    scc_members: &[&str],
    deps: &HashMap<&str, HashSet<&str>>,
) -> String {
    let member_set: HashSet<&str> = scc_members.iter().copied().collect();

    let mut path = vec![start];
    let mut visited = HashSet::new();
    visited.insert(start);
    let mut current = start;

    loop {
        let mut found_next = false;
        if let Some(dep_set) = deps.get(current) {
            for dep_name in dep_set {
                if *dep_name == start && path.len() > 1 {
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
