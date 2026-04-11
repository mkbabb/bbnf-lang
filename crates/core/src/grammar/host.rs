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

use super::generated::{
    BbnfBootstrap, BbnfBootstrapNodeView, BbnfBootstrapRuleKind, cst_directives,
};
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

    let mut grammar = ParsedGrammar::empty();

    // `grammar = grammar_item*` — the rule body is a single Repeat
    // compound whose direct children are the individual
    // `grammar_item` compounds. Flatten the Repeat wrapper, peel
    // transparent grammar_item / directive wrappers, and dispatch
    // each item to `absorb_item`.
    use ::bbnf::runtime::tape::TapeKind;
    for item in root.children() {
        if item.kind() == TapeKind::Repeat {
            for grandchild in item.children() {
                let inner = peel_wrappers(grandchild);
                absorb_item(inner, &mut grammar);
            }
        } else {
            let inner = peel_wrappers(item);
            absorb_item(inner, &mut grammar);
        }
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
    // Rules: `rule = identifier "=" rhs ";"`. Under tape-first,
    // literal matches don't push records — and `identifier` may
    // also be elided at the tape level if it's a transparent /
    // fused rule whose body is inlined. So the rule compound can
    // have one or two direct children:
    //   - child(0) = identifier (when present)
    //   - child(0) = rhs (when identifier was elided)
    //   - child(0) = identifier, child(1) = rhs (both present)
    //
    // We identify the identifier by its `rule_kind`. The name
    // text comes from the leading token of the rule span.
    if item.rule_kind() == BbnfBootstrapRuleKind::rule {
        let rule_text = item.span_text();
        let (lo, _hi) = item.span();
        // Identifier text: leading `[_a-zA-Z][\w]*` run from the rule
        // span start. The bbnf grammar guarantees a rule starts with
        // its identifier.
        let name_len = rule_text
            .bytes()
            .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
            .count();
        let name: &str = &rule_text[..name_len];
        let input = item.input();
        let name_span = ::parse_that::Span::new(
            lo as usize,
            lo as usize + name_len,
            input,
        );
        // Pick the rhs child: the last direct child that isn't the
        // identifier view (we look up by rule_kind match). For the
        // common case where identifier is elided, the single direct
        // child IS the rhs.
        let rhs = {
            let mut rhs: Option<BbnfBootstrapNodeView<'a>> = None;
            for c in item.children() {
                if c.rule_kind() != BbnfBootstrapRuleKind::identifier {
                    rhs = Some(c);
                }
            }
            rhs.expect("rule: missing rhs child")
        };
        grammar.rules.insert(
            name,
            RuleEntry {
                name_span,
                rhs,
            },
        );
        return;
    }

    // Directives: dispatch on `rule_kind()` and call the direct
    // `try_as_<rule>(cursor, input)` schema helpers. The walking
    // `as_*_directive` methods on `BbnfBootstrapNodeView` look for
    // matching variants **among a parent view's children**; here
    // `item` has already been peeled through `grammar_item` /
    // `directive` wrappers, so it points AT the specific directive
    // compound. The try helpers check `cursor.variant_idx()`
    // directly and extract the typed struct in O(child count).
    let cursor = item.cursor();
    let input = item.input();
    match item.rule_kind() {
        BbnfBootstrapRuleKind::recover_directive => {
            if let Some(d) = cst_directives::try_as_recover_directive(cursor, input) {
                grammar.recovers.push(RecoverDirective {
                    rule_name: d.rule_name,
                    sync_expr: d.sync_expr,
                    span: d.span,
                });
                return;
            }
        }
        BbnfBootstrapRuleKind::pretty_directive => {
            if let Some(d) = cst_directives::try_as_pretty_directive(cursor, input) {
                let hints: Vec<Cow<'a, str>> =
                    d.hints.children().map(|h| pretty_hint_text(h)).collect();
                grammar.pretties.push(PrettyDirective {
                    rule_name: Cow::Owned(d.target.to_string()),
                    hints,
                    span: d.span,
                });
                return;
            }
        }
        BbnfBootstrapRuleKind::token_directive => {
            if let Some(d) = cst_directives::try_as_token_directive(cursor, input) {
                grammar.token_rules.push(Cow::Owned(d.name.to_string()));
                return;
            }
        }
        BbnfBootstrapRuleKind::debug_directive => {
            if let Some(d) = cst_directives::try_as_debug_directive(cursor, input) {
                grammar.debug_rules.push(Cow::Owned(d.target.to_string()));
                return;
            }
        }
        BbnfBootstrapRuleKind::ws_directive => {
            if let Some(d) = cst_directives::try_as_ws_directive(cursor, input) {
                // `d.value` is a `regex` leaf view; strip `/.../`.
                let raw = d.value.span_text();
                let stripped = raw
                    .strip_prefix('/')
                    .and_then(|s| s.strip_suffix('/'))
                    .unwrap_or(raw);
                grammar.ws_pattern = Some(Cow::Borrowed(stripped));
                return;
            }
        }
        BbnfBootstrapRuleKind::host_directive => {
            if let Some(d) = cst_directives::try_as_host_directive(cursor, input) {
                let return_type = d
                    .type_annotation
                    .map(|t| Cow::Owned(t.span_text().to_string()));
                grammar.host_fns.push(HostFnDecl {
                    name: Cow::Owned(d.name.to_string()),
                    return_type,
                });
                return;
            }
        }
        BbnfBootstrapRuleKind::import_directive => {
            if let Some(d) = cst_directives::try_as_import_directive(cursor, input) {
                absorb_import(d.inner, d.span, &mut grammar.imports);
                return;
            }
        }
        _ => {}
    }

    // Span-text fallback — under HEAD's hand-patched generated.rs
    // the schema's `try_as_*_directive` helpers check a stale
    // `variant_idx` that's off-by-one from what the rule emitter
    // actually stamps, so the typed extraction returns `None`.
    // When we know the kind from `rule_kind()` but the typed
    // extraction failed, OR when the inlined `directive` rule
    // collapses to an empty compound, fall through to a span-text
    // dispatch on the leading `@keyword`. This becomes redundant
    // after AE.4's clean regen rebuilds the schema with correct
    // variant_idx values.
    let raw = item.span_text();
    let trimmed = raw.trim_start();
    if let Some(kw_end) = trimmed.find(|c: char| c.is_whitespace() || c == ';' || c == '.') {
        let kw = &trimmed[..kw_end];
        match kw {
            "@import" => absorb_import_by_text(item, &mut grammar.imports),
            "@recover" => absorb_recover_by_text(item, &mut grammar.recovers),
            "@pretty" => absorb_pretty_by_text(item, &mut grammar.pretties),
            "@token" => absorb_token_by_text(item, &mut grammar.token_rules),
            "@debug" => absorb_debug_by_text(item, &mut grammar.debug_rules),
            "@ws" => absorb_ws_by_text(item, &mut grammar.ws_pattern),
            "@host" => absorb_host_by_text(item, &mut grammar.host_fns),
            _ => {}
        }
    }
}

/// Span-text directive extractors. Each parses a directive's
/// source slice directly when the schema's typed `try_as_*`
/// helper has stale `variant_idx` constants and returns `None`.
/// All become unreachable after AE.4's clean regen.

fn absorb_import_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    imports: &mut Vec<ImportDirective<'a>>,
) {
    let raw = item.span_text();
    let (lo, hi) = item.span();
    let span = Span::new(lo as usize, hi as usize, item.input());
    // Two forms: glob `@import "path" ;` or selective
    // `@import { a, b } from "path" ;`.
    let body = raw.trim_start_matches("@import").trim();
    if let Some(brace_open) = body.find('{') {
        // Selective.
        let after_brace = &body[brace_open + 1..];
        let brace_close = match after_brace.find('}') {
            Some(i) => i,
            None => return,
        };
        let items_str = &after_brace[..brace_close];
        let mut items: Vec<ImportedName<'a>> = Vec::new();
        for raw_name in items_str.split(',') {
            let name = raw_name.trim();
            if name.is_empty() {
                continue;
            }
            // Recover an &'a str by re-slicing the input buffer.
            let input = item.input();
            if let Some(start) = input[lo as usize..hi as usize].find(name) {
                let abs_lo = lo as usize + start;
                let abs_hi = abs_lo + name.len();
                items.push(ImportedName {
                    name: Cow::Borrowed(&input[abs_lo..abs_hi]),
                    span: Span::new(abs_lo, abs_hi, input),
                });
            }
        }
        // Path: between `from` and the trailing `;` / `.`.
        let after_close = &after_brace[brace_close + 1..];
        let from_idx = match after_close.find("from") {
            Some(i) => i,
            None => return,
        };
        let after_from = after_close[from_idx + 4..].trim_start();
        let path = parse_quoted_string(after_from);
        if let Some(path_str) = path {
            imports.push(ImportDirective {
                path: Cow::Owned(path_str.to_string()),
                span,
                items: Some(items),
            });
        }
    } else if let Some(path_str) = parse_quoted_string(body) {
        // Glob form.
        imports.push(ImportDirective {
            path: Cow::Owned(path_str.to_string()),
            span,
            items: None,
        });
    }
}

fn absorb_recover_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    recovers: &mut Vec<RecoverDirective<'a>>,
) {
    // Source-text fallback: we don't have the typed sync_expr
    // view here, so synthesize from the directive item itself.
    let input = item.input();
    let raw = item.span_text();
    let (lo, hi) = item.span();
    let body = raw.trim_start_matches("@recover").trim();
    let name_end = body
        .find(|c: char| c.is_whitespace())
        .unwrap_or(body.len());
    let rule_name_str = &body[..name_end];
    if rule_name_str.is_empty() {
        return;
    }
    // Recover an `&'a str` slice into the original input buffer.
    let Some(name_offset) = input[lo as usize..hi as usize].find(rule_name_str) else {
        return;
    };
    let abs_lo = lo as usize + name_offset;
    let abs_hi = abs_lo + rule_name_str.len();
    let rule_name: &'a str = &input[abs_lo..abs_hi];
    // Use the whole directive item as the sync_expr placeholder —
    // the actual sync expression body will be re-parsed downstream
    // when this fallback path is removed by AE.4's clean regen.
    recovers.push(RecoverDirective {
        rule_name,
        sync_expr: item,
        span: Span::new(lo as usize, hi as usize, input),
    });
}

fn absorb_pretty_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    pretties: &mut Vec<PrettyDirective<'a>>,
) {
    let raw = item.span_text();
    let (lo, hi) = item.span();
    let body = raw.trim_start_matches("@pretty").trim();
    // Strip trailing terminator.
    let body = body.trim_end_matches(|c: char| c == ';' || c == '.').trim();
    let mut tokens = body.split_whitespace();
    let target = match tokens.next() {
        Some(t) => t.to_string(),
        None => return,
    };
    let hints: Vec<Cow<'a, str>> =
        tokens.map(|h| Cow::Owned(h.to_string())).collect();
    pretties.push(PrettyDirective {
        rule_name: Cow::Owned(target),
        hints,
        span: Span::new(lo as usize, hi as usize, item.input()),
    });
}

fn absorb_token_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    token_rules: &mut Vec<Cow<'a, str>>,
) {
    let raw = item.span_text();
    let body = raw.trim_start_matches("@token").trim();
    let body = body.trim_end_matches(|c: char| c == ';' || c == '.').trim();
    let name = body.split_whitespace().next().unwrap_or("");
    if !name.is_empty() {
        token_rules.push(Cow::Owned(name.to_string()));
    }
}

fn absorb_debug_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    debug_rules: &mut Vec<Cow<'a, str>>,
) {
    let raw = item.span_text();
    let body = raw.trim_start_matches("@debug").trim();
    let body = body.trim_end_matches(|c: char| c == ';' || c == '.').trim();
    let name = body.split_whitespace().next().unwrap_or("");
    if !name.is_empty() {
        debug_rules.push(Cow::Owned(name.to_string()));
    }
}

fn absorb_ws_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    ws_pattern: &mut Option<Cow<'a, str>>,
) {
    let raw = item.span_text();
    let body = raw.trim_start_matches("@ws").trim();
    let body = body.trim_end_matches(|c: char| c == ';' || c == '.').trim();
    if let Some(stripped) = body.strip_prefix('/').and_then(|s| s.strip_suffix('/')) {
        *ws_pattern = Some(Cow::Owned(stripped.to_string()));
    }
}

fn absorb_host_by_text<'a>(
    item: BbnfBootstrapNodeView<'a>,
    host_fns: &mut Vec<HostFnDecl<'a>>,
) {
    let raw = item.span_text();
    let body = raw.trim_start_matches("@host").trim();
    let body = body.trim_end_matches(|c: char| c == ';' || c == '.').trim();
    // Format: name [: type]
    let (name, type_part) = match body.find(':') {
        Some(i) => (body[..i].trim(), Some(body[i + 1..].trim())),
        None => (body.trim(), None),
    };
    if name.is_empty() {
        return;
    }
    host_fns.push(HostFnDecl {
        name: Cow::Owned(name.to_string()),
        return_type: type_part.map(|t| Cow::Owned(t.to_string())),
    });
}

/// Parse a leading `"..."` quoted string from the front of `s`,
/// returning the inside (sans quotes). Used by the span-text
/// directive fallbacks to recover import paths.
fn parse_quoted_string(s: &str) -> Option<&str> {
    let s = s.trim_start();
    if !s.starts_with('"') {
        return None;
    }
    let after = &s[1..];
    let close = after.find('"')?;
    Some(&after[..close])
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
