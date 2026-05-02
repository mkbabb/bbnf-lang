//! `path!` proc-macro implementation.
//!
//! Five stages:
//!
//! 1. **Parse** the macro input — `path!(GrammarMarker, seg, seg, ...)`.
//!    The first comma-separated argument is a Rust path resolving to a
//!    grammar marker (e.g. `Json`); the trailing identifier is the
//!    fixture key. The remaining arguments are path segments: string
//!    literals (which the W2.3 path-lexer tokenises and may contain
//!    compound segments like `"foo.bar[0]"`), integer literals, or `*`
//!    for wildcard.
//! 2. **Lex** every string literal through `bbnf_regex::lex_path`. The
//!    lexer's `proc_macro2::Span` flows through to any diagnostic so
//!    the error squiggle anchors at the offending segment.
//! 3. **Lower** the lexer's `PathToken` stream to a sequence of
//!    [`OwnedSegment`]s — the macro's internal segment representation
//!    (`Field` / `Index` / `Wildcard` / `VariantName`). The shape
//!    mirrors `bbnf::path::OwnedPathSegment` but lives here so the
//!    proc-macro can manipulate the segments without the offline
//!    checker round-trip.
//! 4. **Validate** the segment sequence against the per-grammar
//!    [`crate::registry::GrammarFixture`] using a hand-rolled walker
//!    that mirrors `bbnf::path::type_check::check_path_against_registry`.
//!    On failure, emit a `proc_macro2::Span`-anchored
//!    `syn::Error::new` carrying the offending segment, the resolved
//!    struct type, and the valid alternatives.
//! 5. **Emit** the `bbnf::path::TypedPath::<Marker, ()>::from_owned(...)`
//!    expression literal at the macro call-site.
//!
//! Architecture caveat — re-stated:
//!
//! A proc-macro can only see its input and what its `Cargo.toml`
//! lets it `use`. The per-grammar `StructRegistry` lives behind
//! `bbnf::path` (in the `bbnf` crate), which depends on this crate; the
//! macro therefore consumes the registry through a synthetic
//! per-grammar fixture in [`crate::registry`] until `cargo xtask regen`
//! exposes the production const. The expansion's emitted code names
//! `bbnf::path::TypedPath` / `bbnf::path::OwnedPathSegment` symbols by
//! their absolute path; the consumer crate must have `bbnf` in scope.

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Error, Lit, LitInt, LitStr, Path, Result, Token};

use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{LayoutKind, StructLayout, StructRegistry};

use crate::registry::{GrammarFixture, fixture_for_marker, supported_markers};

/// One owned path segment as the macro carries it internally between
/// lex / validate / emit. Mirrors `bbnf::path::OwnedPathSegment`; kept
/// local so the macro can produce the expression literal that
/// constructs the final `OwnedPathSegment` at the call-site without
/// importing the upstream type.
#[derive(Debug, Clone)]
enum OwnedSegment {
    Field { text: String, span: Span },
    Index { value: usize, span: Span },
    Wildcard { span: Span },
    VariantName { text: String, span: Span },
}

impl OwnedSegment {
    fn span(&self) -> Span {
        match self {
            OwnedSegment::Field { span, .. } => *span,
            OwnedSegment::Index { span, .. } => *span,
            OwnedSegment::Wildcard { span } => *span,
            OwnedSegment::VariantName { span, .. } => *span,
        }
    }

    fn render(&self) -> String {
        match self {
            OwnedSegment::Field { text, .. } => text.clone(),
            OwnedSegment::Index { value, .. } => value.to_string(),
            OwnedSegment::Wildcard { .. } => "*".to_string(),
            OwnedSegment::VariantName { text, .. } => format!("@{text}"),
        }
    }
}

/// Top-level macro-input AST. Parsed by `syn::parse2` from the raw
/// `TokenStream` passed to the proc-macro entry point.
struct PathInput {
    grammar: Path,
    /// Trailing arguments — path segments. Carried as `MacroArg` so we
    /// can preserve the source span for error anchoring.
    args: Vec<MacroArg>,
}

enum MacroArg {
    StringLit(LitStr),
    IntLit(LitInt),
    Wildcard(Span),
}

impl Parse for PathInput {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let grammar: Path = input.parse()?;
        let mut args: Vec<MacroArg> = Vec::new();
        while !input.is_empty() {
            let _: Token![,] = input.parse()?;
            if input.is_empty() {
                break;
            }
            let lookahead = input.lookahead1();
            if lookahead.peek(LitStr) {
                let lit: LitStr = input.parse()?;
                args.push(MacroArg::StringLit(lit));
            } else if lookahead.peek(LitInt) {
                let lit: LitInt = input.parse()?;
                args.push(MacroArg::IntLit(lit));
            } else if lookahead.peek(Token![*]) {
                let star: Token![*] = input.parse()?;
                args.push(MacroArg::Wildcard(star.span));
            } else if lookahead.peek(Lit) {
                // Other literal kinds (float, bool, byte-string) are
                // not part of the path alphabet.
                let lit: Lit = input.parse()?;
                let span = match &lit {
                    Lit::Float(l) => l.span(),
                    Lit::Bool(l) => l.span,
                    Lit::Byte(l) => l.span(),
                    Lit::ByteStr(l) => l.span(),
                    Lit::Char(l) => l.span(),
                    Lit::CStr(l) => l.span(),
                    _ => Span::call_site(),
                };
                return Err(Error::new(
                    span,
                    "path! segment must be a string literal, integer literal, or `*`",
                ));
            } else {
                return Err(lookahead.error());
            }
        }
        Ok(PathInput { grammar, args })
    }
}

/// Top-level expansion entry. Returns the expanded `TokenStream` or a
/// `syn::Error` for the proc-macro entry to convert into a compile
/// error.
pub(crate) fn expand(input: TokenStream) -> Result<TokenStream> {
    let parsed: PathInput = syn::parse2(input)?;

    // Resolve the grammar marker to its fixture. The marker's *trailing*
    // segment names the fixture key (`Json`, `CssL4`, ...); leading
    // path segments (e.g. `bbnf::path::Json`) are passed through to the
    // emitted expression unchanged.
    let marker_ident = parsed
        .grammar
        .segments
        .last()
        .map(|seg| seg.ident.clone())
        .ok_or_else(|| {
            Error::new(
                Span::call_site(),
                "path! grammar marker must be a non-empty path",
            )
        })?;
    let marker_name = marker_ident.to_string();

    let fixture = fixture_for_marker(&marker_name).ok_or_else(|| {
        Error::new(
            marker_ident.span(),
            format!(
                "unknown grammar marker `{}`; supported markers: {}",
                marker_name,
                supported_markers().join(", "),
            ),
        )
    })?;

    // Lower every macro argument into one or more `OwnedSegment`s.
    let segments = lower_args_to_segments(&parsed.args)?;

    if segments.is_empty() {
        return Err(Error::new(
            Span::call_site(),
            "path! requires at least one segment after the grammar marker",
        ));
    }

    // Validate the segment sequence against the per-grammar fixture.
    validate_against_fixture(&segments, &fixture)?;

    // Emit the typed-path expression literal.
    let grammar_path = &parsed.grammar;
    let segment_exprs: Vec<TokenStream> = segments.iter().map(emit_segment).collect();

    Ok(quote! {
        ::bbnf::path::TypedPath::<#grammar_path, ()>::from_owned(
            ::std::vec![ #( #segment_exprs ),* ]
        )
    })
}

// ─── Lowering: macro args → `OwnedSegment`s ──────────────────────────

fn lower_args_to_segments(args: &[MacroArg]) -> Result<Vec<OwnedSegment>> {
    let mut out: Vec<OwnedSegment> = Vec::new();
    for arg in args {
        match arg {
            MacroArg::StringLit(lit) => {
                lower_string_lit(lit, &mut out)?;
            }
            MacroArg::IntLit(lit) => {
                let value: usize = lit.base10_parse().map_err(|e| {
                    Error::new(lit.span(), format!("path! index must fit in `usize`: {e}"))
                })?;
                out.push(OwnedSegment::Index {
                    value,
                    span: lit.span(),
                });
            }
            MacroArg::Wildcard(span) => {
                out.push(OwnedSegment::Wildcard { span: *span });
            }
        }
    }
    Ok(out)
}

/// Tokenise a string-literal argument through `bbnf_regex::lex_path`
/// and lower the resulting `PathToken` stream into `OwnedSegment`s.
///
/// String literals are not constrained to a single segment — `"foo"`
/// produces one `Field`, but `"foo.bar[0]"` produces three segments
/// (`Field("foo")`, `Field("bar")`, `Index(0)`). The W2.3 path lexer
/// emits `Dot`, `LBracket`, `RBracket`, `Star`, `Dollar` structural
/// tokens we re-fold here.
fn lower_string_lit(lit: &LitStr, out: &mut Vec<OwnedSegment>) -> Result<()> {
    let span = lit.span();
    let raw = lit.value();
    let tokens = bbnf_regex::lex_path(&raw, span).map_err(|err| {
        Error::new(
            err.span,
            format!(
                "path! lex error at byte {} of `{}`: {}",
                err.byte_offset, raw, err.message
            ),
        )
    })?;

    // State-machine consumption. The path alphabet is:
    //   ident                  → Field / VariantName
    //   `$`                    → root marker (skipped)
    //   `[` index `]`          → Index segment
    //   `.` ident              → Field segment
    //   `*`                    → Wildcard segment
    //
    // `@ident` (variant select) is not part of the path-string lexer
    // alphabet; variant selection arrives only via the dedicated
    // syntax (a `LitStr` whose first character is `@`) — see W2.5
    // `variant_select` for the runtime side.
    use bbnf_regex::PathTokenKind as K;

    // If the literal starts with `@`, treat the entire string as a
    // variant-name segment (no further structural tokens accepted).
    if let Some(rest) = raw.strip_prefix('@') {
        let trimmed = rest.trim();
        if trimmed.is_empty() {
            return Err(Error::new(
                span,
                "path! variant-name segment must name a variant after `@`",
            ));
        }
        out.push(OwnedSegment::VariantName {
            text: trimmed.to_string(),
            span,
        });
        return Ok(());
    }

    let mut idx = 0usize;
    while idx < tokens.len() {
        let tok = &tokens[idx];
        match tok.kind {
            K::Eof => break,
            K::Dollar | K::Dot => {
                // Root marker `$` is informational; `.` is a separator.
                idx += 1;
            }
            K::Ident => {
                out.push(OwnedSegment::Field {
                    text: tok.text.clone(),
                    span: tok.span,
                });
                idx += 1;
            }
            K::Star => {
                out.push(OwnedSegment::Wildcard { span: tok.span });
                idx += 1;
            }
            K::LBracket => {
                // Expect an `Index` then `]`.
                let next = tokens.get(idx + 1).ok_or_else(|| {
                    Error::new(tok.span, "path! `[` not followed by an index or `]`")
                })?;
                match next.kind {
                    K::Index => {
                        let value: usize = next.text.parse().map_err(|e| {
                            Error::new(
                                next.span,
                                format!(
                                    "path! index `{}` does not parse as `usize`: {e}",
                                    next.text
                                ),
                            )
                        })?;
                        out.push(OwnedSegment::Index {
                            value,
                            span: next.span,
                        });
                        let close = tokens.get(idx + 2).ok_or_else(|| {
                            Error::new(next.span, "path! `[index` not closed by `]`")
                        })?;
                        if close.kind != K::RBracket {
                            return Err(Error::new(
                                close.span,
                                format!("path! expected `]` after index, found `{}`", close.text),
                            ));
                        }
                        idx += 3;
                    }
                    K::Star => {
                        out.push(OwnedSegment::Wildcard { span: next.span });
                        let close = tokens
                            .get(idx + 2)
                            .ok_or_else(|| Error::new(next.span, "path! `[*` not closed by `]`"))?;
                        if close.kind != K::RBracket {
                            return Err(Error::new(
                                close.span,
                                format!("path! expected `]` after `*`, found `{}`", close.text),
                            ));
                        }
                        idx += 3;
                    }
                    _ => {
                        return Err(Error::new(
                            next.span,
                            format!(
                                "path! `[` must be followed by an index or `*`, found `{}`",
                                next.text
                            ),
                        ));
                    }
                }
            }
            K::RBracket => {
                return Err(Error::new(tok.span, "path! unmatched `]`"));
            }
            K::Index => {
                // Bare integer at top level is also a valid index — the
                // lexer surfaces it without a surrounding `[`/`]` pair
                // when the user wrote `"items.0.text"`.
                let value: usize = tok.text.parse().map_err(|e| {
                    Error::new(
                        tok.span,
                        format!("path! index `{}` does not parse as `usize`: {e}", tok.text),
                    )
                })?;
                out.push(OwnedSegment::Index {
                    value,
                    span: tok.span,
                });
                idx += 1;
            }
        }
    }

    Ok(())
}

// ─── Validation against the per-grammar fixture ──────────────────────

/// Cursor over the layout graph during compile-time path validation.
/// Mirrors `bbnf::path::type_check::ResolveCursor`.
#[derive(Debug, Clone, Copy)]
enum ResolveCursor<'a> {
    Layout(&'a StructLayout),
    Type(&'a TypeDesc, &'a str),
}

fn validate_against_fixture(segments: &[OwnedSegment], fixture: &GrammarFixture) -> Result<()> {
    let entry_layout = fixture
        .registry
        .layout_by_name(fixture.entry_rule)
        .ok_or_else(|| {
            Error::new(
                segments
                    .first()
                    .map(OwnedSegment::span)
                    .unwrap_or_else(Span::call_site),
                format!(
                    "internal: grammar fixture entry rule `{}` not registered",
                    fixture.entry_rule
                ),
            )
        })?;

    let mut cursor = ResolveCursor::Layout(entry_layout);

    for (idx, seg) in segments.iter().enumerate() {
        cursor = step(cursor, seg, idx, &fixture.registry)?;
    }
    Ok(())
}

fn step<'r>(
    cursor: ResolveCursor<'r>,
    segment: &OwnedSegment,
    segment_index: usize,
    registry: &'r StructRegistry,
) -> Result<ResolveCursor<'r>> {
    match cursor {
        ResolveCursor::Layout(layout) => step_into_layout(layout, segment, segment_index),
        ResolveCursor::Type(ty, parent) => {
            step_into_type(ty, parent, segment, segment_index, registry)
        }
    }
}

fn step_into_layout<'r>(
    layout: &'r StructLayout,
    segment: &OwnedSegment,
    segment_index: usize,
) -> Result<ResolveCursor<'r>> {
    match segment {
        OwnedSegment::Field { text, span } => match layout.kind {
            LayoutKind::TaggedEnum => Err(Error::new(
                *span,
                format!(
                    "path[{}] `{}`: expected variant on tagged-enum `{}`; valid variants: {}",
                    segment_index,
                    text,
                    layout.rule_name,
                    branch_alternatives_csv(layout),
                ),
            )),
            _ => match layout.field(text) {
                Some(field) => descend_type(&field.type_desc, layout.rule_name.as_str()),
                None => Err(Error::new(
                    *span,
                    format!(
                        "path[{}] `{}` not found on `{}`; valid fields: {}",
                        segment_index,
                        text,
                        layout.rule_name,
                        field_alternatives_csv(layout),
                    ),
                )),
            },
        },
        OwnedSegment::Index { span, .. } => {
            if let Some(element) = layout.fields.iter().find(|f| f.is_repeat_element()) {
                descend_type(&element.type_desc, layout.rule_name.as_str())
            } else if let TypeDesc::Vec(inner) = &layout.rule_type {
                descend_type(inner, layout.rule_name.as_str())
            } else {
                Err(Error::new(
                    *span,
                    format!(
                        "path[{}] index into non-list `{}`",
                        segment_index, layout.rule_name
                    ),
                ))
            }
        }
        OwnedSegment::Wildcard { span } => {
            if let Some(element) = layout.fields.iter().find(|f| f.is_repeat_element()) {
                descend_type(&element.type_desc, layout.rule_name.as_str())
            } else if let TypeDesc::Vec(inner) = &layout.rule_type {
                descend_type(inner, layout.rule_name.as_str())
            } else {
                Err(Error::new(
                    *span,
                    format!(
                        "path[{}] wildcard over non-list `{}`",
                        segment_index, layout.rule_name
                    ),
                ))
            }
        }
        OwnedSegment::VariantName { text, span } => {
            if !matches!(layout.kind, LayoutKind::TaggedEnum) {
                return Err(Error::new(
                    *span,
                    format!(
                        "path[{}] `@{}` requires a tagged-enum; `{}` is not one",
                        segment_index, text, layout.rule_name
                    ),
                ));
            }
            let mut matches = layout.branches().filter(|(_, f)| f.name == *text);
            let first = matches.next();
            let extra = matches.next();
            match (first, extra) {
                (Some((_, field)), None) => {
                    descend_type(&field.type_desc, layout.rule_name.as_str())
                }
                (None, _) => Err(Error::new(
                    *span,
                    format!(
                        "path[{}] `@{}` not a variant of `{}`; valid: {}",
                        segment_index,
                        text,
                        layout.rule_name,
                        branch_alternatives_csv(layout),
                    ),
                )),
                (Some(_), Some(_)) => Err(Error::new(
                    *span,
                    format!(
                        "path[{}] `@{}` is ambiguous on `{}` — multiple branches share the name",
                        segment_index, text, layout.rule_name
                    ),
                )),
            }
        }
    }
}

fn step_into_type<'r>(
    ty: &'r TypeDesc,
    parent: &'r str,
    segment: &OwnedSegment,
    segment_index: usize,
    registry: &'r StructRegistry,
) -> Result<ResolveCursor<'r>> {
    match (ty, segment) {
        (TypeDesc::Vec(inner), OwnedSegment::Index { .. })
        | (TypeDesc::Vec(inner), OwnedSegment::Wildcard { .. }) => descend_type(inner, parent),
        (TypeDesc::Option(inner), seg) => {
            step_into_type(inner, parent, seg, segment_index, registry)
        }
        (TypeDesc::Named(name_id), _) => {
            // Without the strings table the proc-macro relies on the
            // fixture's `rule_id ↔ StringId` pairing — `registry.rs`
            // sets each `Named(StringId)` to the corresponding
            // layout's `rule_id`, so `registry.layout(name_id)` is the
            // direct lookup. Production grammars carry a strings
            // table on `GrammarIR`; the IR `path_check` pass (W2.2)
            // does the real `Named` → layout walk through that table.
            match registry.layout(*name_id) {
                Some(layout) => step_into_layout(layout, segment, segment_index),
                None => Err(Error::new(
                    segment.span(),
                    format!(
                        "path[{}] descended through unregistered Named rule under `{}`",
                        segment_index, parent
                    ),
                )),
            }
        }
        (scalar, _) if scalar.is_scalar_payload() => Err(Error::new(
            segment.span(),
            format!(
                "path[{}] `{}` cannot descend into scalar payload of `{}`",
                segment_index,
                segment.render(),
                parent
            ),
        )),
        (_, OwnedSegment::Index { .. }) | (_, OwnedSegment::Wildcard { .. }) => Err(Error::new(
            segment.span(),
            format!(
                "path[{}] index/wildcard on non-list under `{}`",
                segment_index, parent
            ),
        )),
        _ => Err(Error::new(
            segment.span(),
            format!(
                "path[{}] `{}` does not resolve under `{}`",
                segment_index,
                segment.render(),
                parent
            ),
        )),
    }
}

fn descend_type<'r>(ty: &'r TypeDesc, parent: &'r str) -> Result<ResolveCursor<'r>> {
    Ok(ResolveCursor::Type(ty, parent))
}

fn field_alternatives_csv(layout: &StructLayout) -> String {
    layout
        .fields()
        .map(|f| f.name.as_str())
        .collect::<Vec<_>>()
        .join(", ")
}

fn branch_alternatives_csv(layout: &StructLayout) -> String {
    layout
        .branches()
        .map(|(_, f)| f.name.as_str())
        .collect::<Vec<_>>()
        .join(", ")
}

// ─── Emission: `OwnedSegment` → `OwnedPathSegment` constructor expr ──

fn emit_segment(seg: &OwnedSegment) -> TokenStream {
    match seg {
        OwnedSegment::Field { text, .. } => {
            quote! { ::bbnf::path::OwnedPathSegment::Field(::std::string::String::from(#text)) }
        }
        OwnedSegment::Index { value, .. } => {
            let v = *value;
            quote! { ::bbnf::path::OwnedPathSegment::Index(#v) }
        }
        OwnedSegment::Wildcard { .. } => {
            quote! { ::bbnf::path::OwnedPathSegment::Wildcard }
        }
        OwnedSegment::VariantName { text, .. } => {
            quote! { ::bbnf::path::OwnedPathSegment::VariantName(::std::string::String::from(#text)) }
        }
    }
}
