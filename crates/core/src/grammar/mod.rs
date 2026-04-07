//! BBNF grammar parser.
//!
//! Single path: generated parser (bootstrap) + host extraction.
//!
//! Sub-modules:
//! - `generated` — auto-generated parser from bbnf.bbnf (bootstrap, checked-in)
//! - `host` — extraction: BbnfBootstrapEnum → ParsedGrammar

#[allow(unused, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::all)]
pub mod generated;
pub mod host;

use crate::types::ParsedGrammar;

/// Parse a BBNF grammar by splitting into individual items and parsing each.
///
/// The bootstrap parser's `grammar` rule has a limitation: its repetition
/// `(comment?, (directive|rule)?w, comment?)*` doesn't handle consecutive
/// comments or multiple top-level items reliably. We work around this by
/// splitting the source at statement terminators (`;` or `.`) and parsing
/// each segment as a standalone single-item grammar.
pub fn parse(source: &str) -> Option<ParsedGrammar<'_>> {
    let mut grammar = ParsedGrammar::empty();

    // Split source into individual statements at `;` or `.` boundaries.
    // Each segment is a complete directive or rule.
    for segment in split_into_statements(source) {
        let segment = segment.trim();
        if segment.is_empty() || segment.chars().all(|c| c.is_whitespace()) {
            continue;
        }
        // Skip comment-only segments.
        if segment.lines().all(|l| {
            let t = l.trim();
            t.is_empty() || t.starts_with("//") || t.starts_with("/*")
        }) {
            continue;
        }

        // Ensure segment ends with a terminator for the bootstrap parser.
        let with_term = if segment.ends_with(';') || segment.ends_with('.') {
            segment.to_string()
        } else {
            format!("{} ;", segment)
        };
        let leaked: &str = Box::leak(with_term.into_boxed_str());

        let ctx = Box::leak(Box::new(
            generated::__BbnfBootstrapEnumCtx::with_capacity(leaked.len() / 4),
        ));
        let (result, _state) = generated::BbnfBootstrap::grammar()
            .parse_return_state_with_context(leaked, ctx);

        let mut extracted = false;
        if let Some(ast) = result {
            let ast = Box::leak(Box::new(ast));
            let partial = host::extract_grammar(ast);
            // Check if the bootstrap parser actually extracted anything.
            let has_content = !partial.rules.is_empty()
                || !partial.imports.is_empty()
                || !partial.recovers.is_empty()
                || !partial.pretties.is_empty()
                || partial.ws_pattern.is_some()
                || !partial.debug_rules.is_empty()
                || !partial.token_rules.is_empty()
                || !partial.host_fns.is_empty();
            if has_content {
                extracted = true;
                grammar.imports.extend(partial.imports);
                grammar.recovers.extend(partial.recovers);
                grammar.pretties.extend(partial.pretties);
                for (&name, entry) in &partial.rules {
                    grammar.rules.insert(name, entry.clone());
                }
                if partial.ws_pattern.is_some() {
                    grammar.ws_pattern = partial.ws_pattern;
                }
                grammar.debug_rules.extend(partial.debug_rules);
                grammar.token_rules.extend(partial.token_rules);
                grammar.host_fns.extend(partial.host_fns);
            }
        }
        if !extracted {
            // Bootstrap parser produced nothing for this segment. Try text-based
            // extraction for @import directives (workaround for bootstrap parser
            // selective import bug — will be removed once generated.rs is regenerated).
            // Strip comments from segment before text extraction.
            let stripped: &str = Box::leak(
                segment
                    .lines()
                    .filter(|l| {
                        let t = l.trim();
                        !t.is_empty() && !t.starts_with("//") && !t.starts_with("/*")
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
                    .into_boxed_str(),
            );
            if let Some(imp) = extract_import_text(stripped) {
                grammar.imports.push(imp);
            }
        }
    }

    if grammar.rules.is_empty() && grammar.imports.is_empty() {
        // Try parsing the whole source as a single grammar (for empty/trivial grammars).
        let ctx = Box::leak(Box::new(
            generated::__BbnfBootstrapEnumCtx::with_capacity(source.len() / 8),
        ));
        let cleaned: &str = Box::leak(source.to_string().into_boxed_str());
        let (result, _) = generated::BbnfBootstrap::grammar()
            .parse_return_state_with_context(cleaned, ctx);
        if let Some(ast) = result {
            let ast = Box::leak(Box::new(ast));
            return Some(host::extract_grammar(ast));
        }
    }

    Some(grammar)
}

/// Parse a BBNF grammar file, returning both the result and the parser state.
///
/// The state's `offset` is synthesized from the segment-based parser: if
/// all segments parsed successfully, `offset = source.len()`.
pub fn parse_with_state(source: &str) -> (Option<ParsedGrammar<'_>>, parse_that::ParserState<'_>) {
    let parsed = parse(source);

    // Synthesize a ParserState. If parsing extracted any content, report full consumption.
    let offset = match &parsed {
        Some(pg) if !pg.rules.is_empty() || !pg.imports.is_empty() => source.len(),
        _ => 0,
    };

    let state = parse_that::ParserState {
        src: source,
        src_bytes: source.as_bytes(),
        offset,
        end: source.len(),
        furthest_offset: offset,
        context_ptr: std::ptr::null(),
        memo: Default::default(),
    };

    (parsed, state)
}

/// Split source into statement segments at `;` or `.` boundaries.
/// Respects string literals (double-quoted) and regex (/-delimited).
fn split_into_statements(source: &str) -> Vec<&str> {
    let mut segments = Vec::new();
    let bytes = source.as_bytes();
    let mut start = 0;
    let mut i = 0;
    let mut in_string = false;
    let mut in_regex = false;

    while i < bytes.len() {
        let b = bytes[i];
        match b {
            b'"' if !in_regex => {
                in_string = !in_string;
                i += 1;
            }
            b'/' if !in_string && !in_regex => {
                // Check if this starts a comment or regex.
                if i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    // Line comment — skip to end of line.
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    // Block comment — skip to */
                    i += 2;
                    while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                        i += 1;
                    }
                    i += 2; // skip */
                } else {
                    in_regex = true;
                    i += 1;
                }
            }
            b'/' if in_regex => {
                in_regex = false;
                i += 1;
            }
            b'\\' if in_string || in_regex => {
                i += 2; // skip escaped char
            }
            b';' | b'.' if !in_string && !in_regex => {
                segments.push(&source[start..=i]);
                start = i + 1;
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }

    // Trailing segment (no terminator).
    if start < bytes.len() {
        let remaining = &source[start..];
        if !remaining.trim().is_empty() {
            segments.push(remaining);
        }
    }

    segments
}

/// Text-based extraction of `@import` directives as a fallback when the
/// bootstrap parser can't handle the syntax (known limitation for selective imports).
fn extract_import_text(segment: &str) -> Option<crate::types::ImportDirective<'_>> {
    use std::borrow::Cow;
    use crate::types::{ImportDirective, ImportedName};

    let trimmed = segment.trim().trim_end_matches(';').trim_end_matches('.').trim();
    if !trimmed.starts_with("@import") {
        return None;
    }
    let after_kw = trimmed["@import".len()..].trim_start();

    if after_kw.starts_with('{') {
        // Selective: @import { a, b } from "path"
        let close_brace = after_kw.find('}')?;
        let names_str = &after_kw[1..close_brace];
        let after_brace = after_kw[close_brace + 1..].trim_start();
        let after_from = after_brace.strip_prefix("from")?.trim_start();
        let path = extract_quoted_string(after_from)?;

        let items: Vec<ImportedName<'_>> = names_str
            .split(',')
            .filter_map(|name| {
                let name = name.trim();
                if name.is_empty() {
                    None
                } else {
                    Some(ImportedName {
                        name: Cow::Borrowed(name),
                        span: parse_that::Span::default(),
                    })
                }
            })
            .collect();

        Some(ImportDirective {
            path: Cow::Borrowed(path),
            span: parse_that::Span::default(),
            items: Some(items),
        })
    } else if after_kw.starts_with('"') {
        // Glob: @import "path"
        let path = extract_quoted_string(after_kw)?;
        Some(ImportDirective {
            path: Cow::Borrowed(path),
            span: parse_that::Span::default(),
            items: None,
        })
    } else {
        None
    }
}

fn extract_quoted_string(s: &str) -> Option<&str> {
    let s = s.trim();
    if s.starts_with('"') {
        let end = s[1..].find('"')?;
        Some(&s[1..1 + end])
    } else {
        None
    }
}
