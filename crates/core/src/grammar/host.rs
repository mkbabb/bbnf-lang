//! Grammar extraction: `BbnfBootstrapNodeView` → `ParsedGrammar`.
//!
//! Walks the top-level `grammar` root view and assembles the typed
//! `ParsedGrammar` from the schema-emitted `as_*_directive()` accessors
//! and rule destructuring.
//!
//! All structural CST traversal lives in the view-layer cursor
//! accessors (`span_text`, `identifier_text`, `identifier_span`,
//! `as_*_directive`, `rule_kind`, `children`, `child`). This module
//! only handles the *typed*-grammar mapping (CST view → public
//! `crate::types::*` structs).
//!
//! Tranche AC.2: replaced the `BbnfBootstrapEnum` enum pattern-match
//! walker with a cursor-backed view walker. The view API comes from
//! `crate::grammar::generated::BbnfBootstrapNodeView` (emitted by the
//! tape-first generator) and the `Parsed<BbnfBootstrap>` owning type
//! from `crate::runtime::Parsed`.

use std::borrow::Cow;

use parse_that::Span;

use super::generated::{BbnfBootstrap, BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::runtime::Parsed;
use crate::types::*;

/// Extract a `ParsedGrammar` from a finished tape-first parse.
///
/// The caller holds a `&'a Parsed<BbnfBootstrap>` whose internal
/// tape + owned input outlive the returned `ParsedGrammar<'a>`.
/// The root view is lent by `parsed.view()` (GAT-bound by the
/// `Root` impl on `BbnfBootstrap`).
pub fn extract_grammar<'a>(parsed: &'a Parsed<BbnfBootstrap>) -> ParsedGrammar<'a> {
    let root = parsed.view();
    assert_eq!(
        root.rule_kind(),
        BbnfBootstrapRuleKind::grammar,
        "extract_grammar: expected `grammar` root rule, got {:?}",
        root.rule_kind(),
    );

    let mut grammar = ParsedGrammar::empty();

    // `grammar = grammar_item*` — iterate direct children as views.
    for item in root.children() {
        // `grammar_item` / `directive` are transparent wrappers — peel
        // them before dispatching.
        let inner = peel_wrappers(item);
        absorb_item(inner, &mut grammar);
    }

    grammar
}

/// Peel transparent `grammar_item` / `directive` wrappers.
fn peel_wrappers<'a>(node: BbnfBootstrapNodeView<'a>) -> BbnfBootstrapNodeView<'a> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::grammar_item | BbnfBootstrapRuleKind::directive => {
            if let Some(child) = node.child(0) {
                peel_wrappers(child)
            } else {
                node
            }
        }
        _ => node,
    }
}

/// Map a single grammar item (rule or directive) into the typed `ParsedGrammar`.
fn absorb_item<'a>(
    item: BbnfBootstrapNodeView<'a>,
    grammar: &mut ParsedGrammar<'a>,
) {
    // Rules: tuple shape `(lhs, "=", rhs, terminator)`.
    if item.rule_kind() == BbnfBootstrapRuleKind::rule {
        let lhs = item.child(0).expect("rule: missing lhs child");
        let rhs = item.child(2).expect("rule: missing rhs child");
        let name = lhs.span_text();
        let name_span = lhs.identifier_span();
        grammar.rules.insert(
            name,
            RuleEntry {
                name_span,
                rhs,
            },
        );
        return;
    }

    // Directives: schema-emitted typed accessors.
    if let Some(d) = item.as_recover_directive() {
        grammar.recovers.push(RecoverDirective {
            rule_name: d.rule_name,
            sync_expr: d.sync_expr,
            span: d.span,
        });
        return;
    }

    if let Some(d) = item.as_pretty_directive() {
        let hints: Vec<Cow<'a, str>> = d
            .hints
            .iter()
            .map(|h| pretty_hint_text(*h))
            .collect();
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
        // `d.value` is a `regex` leaf view; strip the surrounding `/.../`.
        let raw = d.value.span_text();
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
            .map(|t| Cow::Owned(t.span_text().to_string()));
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
    inner: BbnfBootstrapNodeView<'a>,
    directive_span: Span<'a>,
    imports: &mut Vec<ImportDirective<'a>>,
) {
    match inner.rule_kind() {
        // Selective: @import { items } from "path"
        BbnfBootstrapRuleKind::import_directive_0 => {
            let items_view = inner.child(0).expect("import_directive_0: missing items");
            let path_view = inner.child(2).expect("import_directive_0: missing path");
            let mut names = Vec::new();
            if items_view.rule_kind() == BbnfBootstrapRuleKind::import_items {
                // `import_items = "{" (first (, rest)*) "}"` — the
                // first identifier is child(1), the `(comma, ident)`
                // pairs live under child(2) as a repeat, and the
                // closing brace is child(3). The schema accessor on
                // the view walks this shape directly.
                let first = items_view
                    .child(1)
                    .expect("import_items: missing first identifier");
                push_import_name(&mut names, first);
                if let Some(rest) = items_view.child(2) {
                    for pair in rest.children() {
                        // pair = (",", ident)
                        if let Some(name) = pair.child(1) {
                            push_import_name(&mut names, name);
                        }
                    }
                }
            }
            let path_span = if path_view.rule_kind() == BbnfBootstrapRuleKind::import_path {
                let (lo, hi) = path_view.span();
                Span::new(lo as usize, hi as usize, path_view.input())
            } else {
                directive_span
            };
            imports.push(ImportDirective {
                path: Cow::Borrowed(strip_quotes(path_span.as_str())),
                span: directive_span,
                items: Some(names),
            });
        }
        // Glob: @import "path"
        BbnfBootstrapRuleKind::import_path => {
            let (lo, hi) = inner.span();
            let raw = &inner.input()[lo as usize..hi as usize];
            imports.push(ImportDirective {
                path: Cow::Borrowed(strip_quotes(raw)),
                span: directive_span,
                items: None,
            });
        }
        _ => {}
    }
}

fn push_import_name<'a>(names: &mut Vec<ImportedName<'a>>, node: BbnfBootstrapNodeView<'a>) {
    let s = node.identifier_span();
    if !s.as_str().is_empty() {
        names.push(ImportedName {
            name: Cow::Borrowed(s.as_str()),
            span: s,
        });
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
/// shape is `(identifier_node, paren_arg_opt)`. The arg slot's span
/// includes the parens if present; if the optional arg group is
/// absent we fall back to the bare identifier text.
fn pretty_hint_text<'a>(node: BbnfBootstrapNodeView<'a>) -> Cow<'a, str> {
    if node.rule_kind() == BbnfBootstrapRuleKind::pretty_hint {
        let ident = node.child(0).expect("pretty_hint: missing identifier");
        let name = ident.span_text();
        // The optional `(string_lit)` group is child(1); its span
        // covers the parens + literal when present.
        if let Some(arg_group) = node.child(1) {
            let (lo, hi) = arg_group.span();
            if hi > lo {
                let arg = &arg_group.input()[lo as usize..hi as usize];
                return Cow::Owned(format!("{}{}", name, arg));
            }
        }
        return Cow::Owned(name.to_string());
    }
    Cow::Owned(node.span_text().to_string())
}
