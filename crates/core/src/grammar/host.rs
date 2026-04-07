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
        BbnfBootstrapEnum::rule((lhs, _eq, rhs, _terminator)) => {
            let name = extract_span_text(lhs);
            let name_span = match lhs {
                BbnfBootstrapEnum::identifier(s) => *s,
                _ => Span::default(),
            };
            rules.insert(name, RuleEntry {
                name_span,
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
            let target_str = Cow::Owned(extract_span_text(target).to_string());
            let hint_strs: Vec<Cow<'a, str>> = hints
                .iter()
                .map(|h| Cow::Owned(extract_span_text(h).to_string()))
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
            token_rules.push(Cow::Owned(extract_span_text(name).to_string()));
        }

        BbnfBootstrapEnum::debug_directive((_kw, target, _term)) => {
            let text = extract_span_text(target);
            debug_rules.push(Cow::Owned(text.to_string()));
        }

        BbnfBootstrapEnum::host_directive((_kw, name, _term)) => {
            host_fns.push(HostFnDecl {
                name: Cow::Owned(extract_span_text(name).to_string()),
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
        BbnfBootstrapEnum::import_directive_0((items_enum, _from_kw, path_enum)) => {
            if let BbnfBootstrapEnum::import_items((_open, first, rest, _close)) = items_enum {
                let mut names = Vec::new();
                // First identifier (no comma prefix).
                let s = identifier_span(first);
                if !s.as_str().is_empty() {
                    names.push(ImportedName {
                        name: Cow::Borrowed(s.as_str()),
                        span: s,
                    });
                }
                for (_comma, name) in *rest {
                    let s = identifier_span(name);
                    if !s.as_str().is_empty() {
                        names.push(ImportedName {
                            name: Cow::Borrowed(s.as_str()),
                            span: s,
                        });
                    }
                }
                let path_span = match path_enum {
                    BbnfBootstrapEnum::import_path(s) => *s,
                    _ => Span::default(),
                };
                let path = extract_import_path(&path_span);
                imports.push(ImportDirective {
                    path: Cow::Borrowed(path),
                    span: path_span,
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

/// Extract the text content from any bootstrap enum variant by recursively
/// unwrapping structural wrappers to find a leaf span. This is robust against
/// codegen optimization differences that may wrap simple values in structural
/// variants (e.g., `*` arriving as a `binary_factor` wrapper).
pub fn extract_span_text<'a>(node: &'a BbnfBootstrapEnum<'a>) -> &'a str {
    match node {
        // Leaf span variants.
        BbnfBootstrapEnum::identifier(s)
        | BbnfBootstrapEnum::literal(s)
        | BbnfBootstrapEnum::regex(s)
        | BbnfBootstrapEnum::modifier(s)
        | BbnfBootstrapEnum::comment(s)
        | BbnfBootstrapEnum::big_comment(s)
        | BbnfBootstrapEnum::binary_operators(s)
        | BbnfBootstrapEnum::debug_directive_0(s)
        | BbnfBootstrapEnum::pretty_directive_0(s)
        | BbnfBootstrapEnum::term_0(s) => s.as_str(),

        // Structural wrappers — unwrap and recurse.
        BbnfBootstrapEnum::term(inner)
        | BbnfBootstrapEnum::value_atom(inner)
        | BbnfBootstrapEnum::value_unary(inner)
        | BbnfBootstrapEnum::directive(inner) => extract_span_text(inner),

        BbnfBootstrapEnum::factor((_, t, _, _)) => extract_span_text(t),
        BbnfBootstrapEnum::mapped_factor((inner, _)) => extract_span_text(inner),
        BbnfBootstrapEnum::binary_factor((first, _)) => extract_span_text(first),
        BbnfBootstrapEnum::term_1((ident, _)) => extract_span_text(ident),
        BbnfBootstrapEnum::term_2((_, inner, _))
        | BbnfBootstrapEnum::value_atom_0((_, inner, _)) => extract_span_text(inner),

        // Alternation/concatenation with single element.
        BbnfBootstrapEnum::alternation(branches) if !branches.is_empty() => {
            extract_span_text(branches[0].0)
        }
        BbnfBootstrapEnum::concatenation(parts) if !parts.is_empty() => {
            extract_span_text(parts[0].0)
        }

        _ => "",
    }
}
