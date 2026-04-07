//! Grammar extraction: `BbnfBootstrapEnum` → `ParsedGrammar`.
//!
//! Walks the top-level `grammar` variant and extracts rules + directives.
//! No intermediate AST types — rules store direct references into the
//! bootstrap parse tree.

use std::borrow::Cow;

use parse_that::Span;

use super::generated::BbnfBootstrapEnum;
use crate::types::*;

/// Extract a `ParsedGrammar` from the generated parser output.
pub fn extract_grammar<'a>(ast: &'a BbnfBootstrapEnum<'a>) -> ParsedGrammar<'a> {
    let BbnfBootstrapEnum::grammar(items) = ast else {
        panic!("expected grammar variant, got {:?}", std::mem::discriminant(ast));
    };

    let mut imports = Vec::new();
    let mut recovers = Vec::new();
    let mut pretties = Vec::new();
    let mut ws_pattern = None;
    let mut debug_rules = Vec::new();
    let mut token_rules = Vec::new();
    let mut host_fns = Vec::new();
    let mut rules: AST<'a> = indexmap::IndexMap::new();

    for (_comment_before, item, _comment_after) in items.iter() {
        let inner = match item {
            BbnfBootstrapEnum::directive(d) => d,
            other => other,
        };
        extract_item(
            inner,
            &mut imports,
            &mut recovers,
            &mut pretties,
            &mut ws_pattern,
            &mut debug_rules,
            &mut token_rules,
            &mut host_fns,
            &mut rules,
        );
    }

    ParsedGrammar {
        imports,
        recovers,
        pretties,
        rules,
        ws_pattern,
        debug_rules,
        token_rules,
        host_fns,
    }
}

fn extract_item<'a>(
    item: &'a BbnfBootstrapEnum<'a>,
    imports: &mut Vec<ImportDirective<'a>>,
    recovers: &mut Vec<RecoverDirective<'a>>,
    pretties: &mut Vec<PrettyDirective<'a>>,
    ws_pattern: &mut Option<Cow<'a, str>>,
    debug_rules: &mut Vec<Cow<'a, str>>,
    token_rules: &mut Vec<Cow<'a, str>>,
    host_fns: &mut Vec<HostFnDecl<'a>>,
    rules: &mut AST<'a>,
) {
    match item {
        BbnfBootstrapEnum::rule((lhs_span, rhs, _terminator)) => {
            // The lhs_span may include trailing `= ` from the concatenation collapsing.
            // Extract just the identifier: leading sequence of [_a-zA-Z][_a-zA-Z0-9-]*.
            let full = lhs_span.as_str();
            let name = full
                .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
                .map_or(full, |i| &full[..i]);
            rules.insert(name, RuleEntry {
                name_span: *lhs_span,
                rhs,
            });
        }

        BbnfBootstrapEnum::import_directive((_kw, inner, _term)) => {
            extract_import(inner, imports);
        }

        BbnfBootstrapEnum::recover_directive((_kw, name, rhs, term)) => {
            let name_span = identifier_span(name);
            recovers.push(RecoverDirective {
                rule_name: Cow::Borrowed(name_span.as_str()),
                sync_expr: rhs,
                span: *term,
            });
        }

        BbnfBootstrapEnum::pretty_directive((_kw, target, hints, term)) => {
            let target_str = match target {
                BbnfBootstrapEnum::pretty_directive_0(s) => Cow::Borrowed(s.as_str()),
                other => Cow::Owned(identifier_str(other).to_string()),
            };
            let hint_strs: Vec<Cow<'a, str>> = hints
                .iter()
                .map(|h| Cow::Borrowed(identifier_str(h)))
                .collect();
            pretties.push(PrettyDirective {
                rule_name: target_str,
                hints: hint_strs,
                span: *term,
            });
        }

        BbnfBootstrapEnum::ws_directive((_kw, regex_enum, _term)) => {
            if let BbnfBootstrapEnum::regex(s) = regex_enum {
                let inner = &s.as_str()[1..s.as_str().len() - 1];
                *ws_pattern = Some(Cow::Borrowed(inner));
            }
        }

        BbnfBootstrapEnum::token_directive((_kw, name, _term)) => {
            token_rules.push(Cow::Owned(identifier_str(name).to_string()));
        }

        BbnfBootstrapEnum::debug_directive((_kw, target, _term)) => {
            match target {
                BbnfBootstrapEnum::debug_directive_0(s) => {
                    debug_rules.push(Cow::Borrowed(s.as_str()));
                }
                other => {
                    debug_rules.push(Cow::Owned(identifier_str(other).to_string()));
                }
            }
        }

        BbnfBootstrapEnum::host_directive((_kw, name, _term)) => {
            host_fns.push(HostFnDecl {
                name: Cow::Owned(identifier_str(name).to_string()),
                return_type: None,
            });
        }

        // Directive wrapper — unwrap and recurse.
        BbnfBootstrapEnum::directive(inner) => {
            extract_item(inner, imports, recovers, pretties, ws_pattern, debug_rules, token_rules, host_fns, rules);
        }

        _ => {}
    }
}

fn extract_import<'a>(inner: &'a BbnfBootstrapEnum<'a>, imports: &mut Vec<ImportDirective<'a>>) {
    match inner {
        // Selective: @import { items } from "path"
        BbnfBootstrapEnum::import_directive_0((items_or_path, path_span)) => {
            if let BbnfBootstrapEnum::import_items((_open, items, _close)) = items_or_path {
                let names: Vec<ImportedName<'a>> = std::iter::once(items.first())
                    .flatten()
                    .chain(items.iter().skip(1))
                    .map(|(_comma, name)| {
                        let s = identifier_span(name);
                        ImportedName {
                            name: Cow::Borrowed(s.as_str()),
                            span: s,
                        }
                    })
                    .collect();
                let path = extract_import_path(path_span);
                imports.push(ImportDirective {
                    path: Cow::Borrowed(path),
                    span: *path_span,
                    items: Some(names),
                });
            }
        }
        // Glob: @import "path"
        BbnfBootstrapEnum::import_path(path_span) => {
            let path = extract_import_path(path_span);
            imports.push(ImportDirective {
                path: Cow::Borrowed(path),
                span: *path_span,
                items: None,
            });
        }
        _ => {}
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────

fn identifier_str<'a>(node: &'a BbnfBootstrapEnum<'a>) -> &'a str {
    match node {
        BbnfBootstrapEnum::identifier(s) => s.as_str(),
        _ => "",
    }
}

fn identifier_span<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Span<'a> {
    match node {
        BbnfBootstrapEnum::identifier(s) => *s,
        _ => Span::default(),
    }
}

fn extract_import_path<'a>(span: &Span<'a>) -> &'a str {
    let raw = span.as_str();
    if raw.starts_with('"') && raw.ends_with('"') {
        &raw[1..raw.len() - 1]
    } else {
        raw
    }
}
