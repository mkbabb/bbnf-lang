//! Grammar extraction: `BbnfBootstrapEnum` → `ParsedGrammar`.
//!
//! Walks the top-level `grammar` variant and assembles the typed
//! `ParsedGrammar` from the schema-emitted `as_*_directive()` accessors
//! and rule destructuring.
//!
//! All structural CST traversal lives in the schema-emitted helpers
//! (`span_text`, `identifier_text`, `identifier_span`, `as_*_directive`).
//! This module only handles the *typed*-grammar mapping (CST → public
//! `crate::types::*` structs).

use std::borrow::Cow;

use parse_that::Span;

use super::generated::BbnfBootstrapEnum;
use crate::types::*;

/// Extract a `ParsedGrammar` from the generated parser output.
pub fn extract_grammar<'a>(ast: &'a BbnfBootstrapEnum<'a>) -> ParsedGrammar<'a> {
    let BbnfBootstrapEnum::grammar(items) = ast else {
        panic!("expected grammar variant, got {:?}", std::mem::discriminant(ast));
    };

    let mut grammar = ParsedGrammar::empty();

    for item in items.iter() {
        // grammar_item / directive are transparent wrappers — peel them.
        let inner = peel_wrappers(item);
        absorb_item(inner, &mut grammar);
    }

    grammar
}

fn peel_wrappers<'a>(node: &'a BbnfBootstrapEnum<'a>) -> &'a BbnfBootstrapEnum<'a> {
    match node {
        BbnfBootstrapEnum::grammar_item(inner) | BbnfBootstrapEnum::directive(inner) => {
            peel_wrappers(inner)
        }
        other => other,
    }
}

/// Map a single grammar item (rule or directive) into the typed `ParsedGrammar`.
fn absorb_item<'a>(item: &'a BbnfBootstrapEnum<'a>, grammar: &mut ParsedGrammar<'a>) {
    // Rules: tuple shape `(lhs, "=", rhs, terminator)`.
    if let BbnfBootstrapEnum::rule((lhs, _eq, rhs, _term)) = item {
        let name = BbnfBootstrapEnum::span_text(lhs);
        let name_span = BbnfBootstrapEnum::identifier_span(lhs);
        grammar.rules.insert(name, RuleEntry { name_span, rhs });
        return;
    }

    // Directives: schema-emitted typed accessors.
    if let Some(d) = item.as_recover_directive() {
        grammar.recovers.push(RecoverDirective {
            rule_name: Cow::Borrowed(d.rule_name),
            sync_expr: d.sync_expr,
            span: d.span,
        });
        return;
    }

    if let Some(d) = item.as_pretty_directive() {
        let hints: Vec<Cow<'a, str>> = d.hints.iter().map(pretty_hint_text).collect();
        grammar.pretties.push(PrettyDirective {
            rule_name: Cow::Owned(d.target.to_string()),
            hints,
            span: d.span,
        });
        return;
    }

    if let Some(d) = item.as_token_directive() {
        grammar.token_rules.push(Cow::Owned(d.name.to_string()));
        return;
    }

    if let Some(d) = item.as_debug_directive() {
        grammar.debug_rules.push(Cow::Owned(d.target.to_string()));
        return;
    }

    if let Some(d) = item.as_ws_directive() {
        // The inner is a `regex` Span variant; strip the surrounding `/.../`.
        let raw = BbnfBootstrapEnum::span_text(d.value);
        let stripped = raw
            .strip_prefix('/')
            .and_then(|s| s.strip_suffix('/'))
            .unwrap_or(raw);
        grammar.ws_pattern = Some(Cow::Borrowed(stripped));
        return;
    }

    if let Some(d) = item.as_host_directive() {
        let return_type = d
            .type_annotation
            .map(|t| Cow::Owned(BbnfBootstrapEnum::span_text(t).to_string()));
        grammar.host_fns.push(HostFnDecl {
            name: Cow::Owned(d.name.to_string()),
            return_type,
        });
        return;
    }

    if let Some(d) = item.as_import_directive() {
        absorb_import(d.inner, d.span, &mut grammar.imports);
    }
}

/// Decode the inner of an `import_directive` — either a glob `import_path`
/// or a selective `import_directive_0` (`{ items } from "path"`).
fn absorb_import<'a>(
    inner: &'a BbnfBootstrapEnum<'a>,
    directive_span: Span<'a>,
    imports: &mut Vec<ImportDirective<'a>>,
) {
    match inner {
        // Selective: @import { items } from "path"
        BbnfBootstrapEnum::import_directive_0((items_enum, _from_kw, path_enum)) => {
            let mut names = Vec::new();
            if let BbnfBootstrapEnum::import_items((_open, (first, rest), _close)) = items_enum {
                let push_name = |names: &mut Vec<ImportedName<'a>>, n: &'a BbnfBootstrapEnum<'a>| {
                    let s = BbnfBootstrapEnum::identifier_span(n);
                    if !s.as_str().is_empty() {
                        names.push(ImportedName {
                            name: Cow::Borrowed(s.as_str()),
                            span: s,
                        });
                    }
                };
                push_name(&mut names, first);
                for (_comma, name) in *rest {
                    push_name(&mut names, name);
                }
            }
            let path_span = match path_enum {
                BbnfBootstrapEnum::import_path(s) => *s,
                _ => directive_span,
            };
            imports.push(ImportDirective {
                path: Cow::Borrowed(strip_quotes(path_span.as_str())),
                span: directive_span,
                items: Some(names),
            });
        }
        // Glob: @import "path"
        BbnfBootstrapEnum::import_path(path_span) => {
            imports.push(ImportDirective {
                path: Cow::Borrowed(strip_quotes(path_span.as_str())),
                span: directive_span,
                items: None,
            });
        }
        _ => {}
    }
}

fn strip_quotes(raw: &str) -> &str {
    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        &raw[1..raw.len() - 1]
    } else {
        raw
    }
}

/// Format a single `pretty_hint` node as `name` or `name(arg)`.
///
/// `pretty_hint = identifier , ("(" , string_lit , ")")?` — the parsed
/// shape is `(identifier_node, paren_arg_span)`. The arg span includes
/// any parens; if non-empty we just append the raw text.
fn pretty_hint_text<'a>(node: &BbnfBootstrapEnum<'a>) -> Cow<'a, str> {
    match node {
        BbnfBootstrapEnum::pretty_hint((ident, arg_span)) => {
            let name = BbnfBootstrapEnum::span_text(ident);
            let arg = arg_span.as_str();
            if arg.is_empty() {
                Cow::Owned(name.to_string())
            } else {
                Cow::Owned(format!("{}{}", name, arg))
            }
        }
        other => Cow::Owned(BbnfBootstrapEnum::span_text(other).to_string()),
    }
}

