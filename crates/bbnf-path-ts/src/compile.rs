//! Path-string lex / lower / validate pipeline mirroring the
//! `bbnf-path` proc-macro, surfacing [`PathErrorPayload`] (TS-bridge
//! diagnostic) instead of `syn::Error`.
//!
//! ## Why mirrored?
//!
//! `bbnf-path` is `proc-macro = true`; proc-macro crates cannot be
//! consumed as a library. The cdylib therefore re-implements the
//! lex/lower/validate logic in plain Rust against the same
//! [`bbnf_ir::registry::StructRegistry`] surface and emits the same
//! diagnostic shape — the isomorphism test at
//! `tests/isomorphic_path_error.rs` pins the two surfaces together.
//!
//! ## Span-less lexer reuse
//!
//! `bbnf_regex::lex_path` takes a `proc_macro2::Span` for diagnostic
//! anchoring. At cdylib runtime there is no source file to anchor at,
//! so the cdylib hands the lexer `Span::call_site()` and reads only the
//! lex-error `byte_offset` + `message` fields when surfacing
//! diagnostics — the segment span is meaningless in a runtime
//! TS-binding context.
//!
//! ## Execution
//!
//! [`execute`] applies a validated [`TypedPathPayload`] to a
//! `serde_json::Value` document view and returns the leaf value (or
//! `Value::Null` if the path traverses a missing field). The W5.2
//! Node-execute proof drives this entry through `cargo nextest`
//! against `data/json/twitter.json`.

use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{LayoutKind, StructLayout, StructRegistry};
use proc_macro2::Span;
use serde_json::Value;

use crate::fixture::{GrammarFixture, fixture_for_marker, supported_markers};
use crate::schema::{
    OwnedSegmentPayload, PathErrorPayload, PathErrorReasonPayload, TypedPathPayload,
};

/// Lex / lower / validate `path_str` against the named grammar's
/// fixture; produce a serializable [`TypedPathPayload`] on success or a
/// [`PathErrorPayload`] on failure.
pub(crate) fn compile(path_str: &str, grammar: &str) -> Result<TypedPathPayload, PathErrorPayload> {
    let fixture = fixture_for_marker(grammar).ok_or_else(|| {
        PathErrorPayload::new(
            0,
            grammar,
            "<grammar marker>",
            supported_markers()
                .iter()
                .map(|s| (*s).to_string())
                .collect(),
            PathErrorReasonPayload::UnregisteredRule,
        )
    })?;

    let segments = lex_and_lower(path_str)?;
    validate_against_fixture(&segments, &fixture)?;

    Ok(TypedPathPayload {
        grammar: grammar.to_string(),
        segments,
    })
}

/// Apply a validated [`TypedPathPayload`] to a JS-side document view.
/// Returns the leaf value or `Value::Null` for missing fields.
pub(crate) fn execute(path: &TypedPathPayload, document: &Value) -> Value {
    let mut cursor: &Value = document;
    for seg in &path.segments {
        match seg {
            OwnedSegmentPayload::Field(name) => match cursor {
                Value::Object(map) => match map.get(name) {
                    Some(child) => cursor = child,
                    None => return Value::Null,
                },
                _ => return Value::Null,
            },
            OwnedSegmentPayload::Index(idx) => match cursor {
                Value::Array(arr) => match arr.get(*idx) {
                    Some(child) => cursor = child,
                    None => return Value::Null,
                },
                _ => return Value::Null,
            },
            OwnedSegmentPayload::Wildcard | OwnedSegmentPayload::VariantName(_) => {
                // W5.1 ships scalar / object / array execution; wildcard
                // and variant-name execution paths land in W5.2 once the
                // Node-execute proof exercises the lazy lane against
                // twitter.json. The cdylib returns `Null` for these
                // segments rather than panic so the TS frontend can
                // detect the unsupported case at runtime.
                return Value::Null;
            }
        }
    }
    cursor.clone()
}

// ─── Lex + lower ─────────────────────────────────────────────────────

fn lex_and_lower(path_str: &str) -> Result<Vec<OwnedSegmentPayload>, PathErrorPayload> {
    // The proc-macro accepts a comma-separated list of literals; the TS
    // template-tag instead passes a single dotted/bracketed string
    // (`"$.statuses[0].text"`). The path-lexer accepts the unified
    // alphabet, so we feed the entire input through `lex_path` once.
    let mut out: Vec<OwnedSegmentPayload> = Vec::new();

    if let Some(rest) = path_str.strip_prefix('@') {
        let trimmed = rest.trim();
        if trimmed.is_empty() {
            return Err(PathErrorPayload::new(
                0,
                "@",
                "<path>",
                Vec::new(),
                PathErrorReasonPayload::UnknownVariant,
            ));
        }
        out.push(OwnedSegmentPayload::VariantName(trimmed.to_string()));
        return Ok(out);
    }

    let tokens = bbnf_regex::lex_path(path_str, Span::call_site()).map_err(|err| {
        PathErrorPayload::new(
            0,
            err.message.clone(),
            "<path lexer>",
            Vec::new(),
            PathErrorReasonPayload::UnknownField,
        )
    })?;

    use bbnf_regex::PathTokenKind as K;
    let mut idx = 0usize;
    while idx < tokens.len() {
        let tok = &tokens[idx];
        match tok.kind {
            K::Eof => break,
            K::Dollar | K::Dot => {
                idx += 1;
            }
            K::Ident => {
                out.push(OwnedSegmentPayload::Field(tok.text.clone()));
                idx += 1;
            }
            K::Star => {
                out.push(OwnedSegmentPayload::Wildcard);
                idx += 1;
            }
            K::LBracket => {
                let next = tokens.get(idx + 1).ok_or_else(|| {
                    PathErrorPayload::new(
                        out.len(),
                        "[",
                        "<path>",
                        Vec::new(),
                        PathErrorReasonPayload::UnknownField,
                    )
                })?;
                match next.kind {
                    K::Index => {
                        let value: usize = next.text.parse().map_err(|_| {
                            PathErrorPayload::new(
                                out.len(),
                                next.text.clone(),
                                "<path>",
                                Vec::new(),
                                PathErrorReasonPayload::IndexIntoNonList,
                            )
                        })?;
                        out.push(OwnedSegmentPayload::Index(value));
                        let close = tokens.get(idx + 2).ok_or_else(|| {
                            PathErrorPayload::new(
                                out.len(),
                                next.text.clone(),
                                "<path>",
                                Vec::new(),
                                PathErrorReasonPayload::UnknownField,
                            )
                        })?;
                        if close.kind != K::RBracket {
                            return Err(PathErrorPayload::new(
                                out.len(),
                                close.text.clone(),
                                "<path>",
                                Vec::new(),
                                PathErrorReasonPayload::UnknownField,
                            ));
                        }
                        idx += 3;
                    }
                    K::Star => {
                        out.push(OwnedSegmentPayload::Wildcard);
                        let close = tokens.get(idx + 2).ok_or_else(|| {
                            PathErrorPayload::new(
                                out.len(),
                                "*",
                                "<path>",
                                Vec::new(),
                                PathErrorReasonPayload::UnknownField,
                            )
                        })?;
                        if close.kind != K::RBracket {
                            return Err(PathErrorPayload::new(
                                out.len(),
                                close.text.clone(),
                                "<path>",
                                Vec::new(),
                                PathErrorReasonPayload::UnknownField,
                            ));
                        }
                        idx += 3;
                    }
                    _ => {
                        return Err(PathErrorPayload::new(
                            out.len(),
                            next.text.clone(),
                            "<path>",
                            Vec::new(),
                            PathErrorReasonPayload::UnknownField,
                        ));
                    }
                }
            }
            K::RBracket => {
                return Err(PathErrorPayload::new(
                    out.len(),
                    "]",
                    "<path>",
                    Vec::new(),
                    PathErrorReasonPayload::UnknownField,
                ));
            }
            K::Index => {
                let value: usize = tok.text.parse().map_err(|_| {
                    PathErrorPayload::new(
                        out.len(),
                        tok.text.clone(),
                        "<path>",
                        Vec::new(),
                        PathErrorReasonPayload::IndexIntoNonList,
                    )
                })?;
                out.push(OwnedSegmentPayload::Index(value));
                idx += 1;
            }
        }
    }

    Ok(out)
}

// ─── Validation against the per-grammar fixture ──────────────────────

#[derive(Debug, Clone, Copy)]
enum ResolveCursor<'a> {
    Layout(&'a StructLayout),
    Type(&'a TypeDesc, &'a str),
}

fn validate_against_fixture(
    segments: &[OwnedSegmentPayload],
    fixture: &GrammarFixture,
) -> Result<(), PathErrorPayload> {
    if segments.is_empty() {
        return Err(PathErrorPayload::new(
            0,
            "",
            fixture.entry_rule.to_string(),
            Vec::new(),
            PathErrorReasonPayload::EmptyPath,
        ));
    }

    let entry_layout = fixture
        .registry
        .layout_by_name(fixture.entry_rule)
        .ok_or_else(|| {
            PathErrorPayload::new(
                0,
                segment_render(&segments[0]),
                fixture.entry_rule.to_string(),
                Vec::new(),
                PathErrorReasonPayload::UnregisteredRule,
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
    segment: &OwnedSegmentPayload,
    segment_index: usize,
    registry: &'r StructRegistry,
) -> Result<ResolveCursor<'r>, PathErrorPayload> {
    match cursor {
        ResolveCursor::Layout(layout) => step_into_layout(layout, segment, segment_index),
        ResolveCursor::Type(ty, parent) => {
            step_into_type(ty, parent, segment, segment_index, registry)
        }
    }
}

fn step_into_layout<'r>(
    layout: &'r StructLayout,
    segment: &OwnedSegmentPayload,
    segment_index: usize,
) -> Result<ResolveCursor<'r>, PathErrorPayload> {
    match segment {
        OwnedSegmentPayload::Field(text) => match layout.kind {
            LayoutKind::TaggedEnum => Err(PathErrorPayload::new(
                segment_index,
                text.clone(),
                layout.rule_name.clone(),
                branch_alternatives(layout),
                PathErrorReasonPayload::UnknownVariant,
            )),
            _ => match layout.field(text) {
                Some(field) => descend_type(&field.type_desc, layout.rule_name.as_str()),
                None => Err(PathErrorPayload::new(
                    segment_index,
                    text.clone(),
                    layout.rule_name.clone(),
                    field_alternatives(layout),
                    PathErrorReasonPayload::UnknownField,
                )),
            },
        },
        OwnedSegmentPayload::Index(_) => {
            if let Some(element) = layout.fields.iter().find(|f| f.is_repeat_element()) {
                descend_type(&element.type_desc, layout.rule_name.as_str())
            } else if let TypeDesc::Vec(inner) = &layout.rule_type {
                descend_type(inner, layout.rule_name.as_str())
            } else {
                Err(PathErrorPayload::new(
                    segment_index,
                    segment_render(segment),
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReasonPayload::IndexIntoNonList,
                ))
            }
        }
        OwnedSegmentPayload::Wildcard => {
            if let Some(element) = layout.fields.iter().find(|f| f.is_repeat_element()) {
                descend_type(&element.type_desc, layout.rule_name.as_str())
            } else if let TypeDesc::Vec(inner) = &layout.rule_type {
                descend_type(inner, layout.rule_name.as_str())
            } else {
                Err(PathErrorPayload::new(
                    segment_index,
                    "*",
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReasonPayload::IndexIntoNonList,
                ))
            }
        }
        OwnedSegmentPayload::VariantName(text) => {
            if !matches!(layout.kind, LayoutKind::TaggedEnum) {
                return Err(PathErrorPayload::new(
                    segment_index,
                    format!("@{text}"),
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReasonPayload::UnknownVariant,
                ));
            }
            let mut matches = layout.branches().filter(|(_, f)| f.name == *text);
            let first = matches.next();
            let extra = matches.next();
            match (first, extra) {
                (Some((_, field)), None) => {
                    descend_type(&field.type_desc, layout.rule_name.as_str())
                }
                (None, _) => Err(PathErrorPayload::new(
                    segment_index,
                    format!("@{text}"),
                    layout.rule_name.clone(),
                    branch_alternatives(layout),
                    PathErrorReasonPayload::UnknownVariant,
                )),
                (Some(_), Some(_)) => Err(PathErrorPayload::new(
                    segment_index,
                    format!("@{text}"),
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReasonPayload::AmbiguousVariant,
                )),
            }
        }
    }
}

fn step_into_type<'r>(
    ty: &'r TypeDesc,
    parent: &'r str,
    segment: &OwnedSegmentPayload,
    segment_index: usize,
    registry: &'r StructRegistry,
) -> Result<ResolveCursor<'r>, PathErrorPayload> {
    match (ty, segment) {
        (TypeDesc::Vec(inner), OwnedSegmentPayload::Index(_))
        | (TypeDesc::Vec(inner), OwnedSegmentPayload::Wildcard) => descend_type(inner, parent),
        (TypeDesc::Option(inner), seg) => {
            step_into_type(inner, parent, seg, segment_index, registry)
        }
        (TypeDesc::Named(name_id), _) => match registry.layout(*name_id) {
            Some(layout) => step_into_layout(layout, segment, segment_index),
            None => Err(PathErrorPayload::new(
                segment_index,
                segment_render(segment),
                parent.to_string(),
                Vec::new(),
                PathErrorReasonPayload::UnregisteredRule,
            )),
        },
        (scalar, _) if scalar.is_scalar_payload() => Err(PathErrorPayload::new(
            segment_index,
            segment_render(segment),
            parent.to_string(),
            Vec::new(),
            PathErrorReasonPayload::FieldOnScalar,
        )),
        (_, OwnedSegmentPayload::Index(_)) | (_, OwnedSegmentPayload::Wildcard) => {
            Err(PathErrorPayload::new(
                segment_index,
                segment_render(segment),
                parent.to_string(),
                Vec::new(),
                PathErrorReasonPayload::IndexIntoNonList,
            ))
        }
        _ => Err(PathErrorPayload::new(
            segment_index,
            segment_render(segment),
            parent.to_string(),
            Vec::new(),
            PathErrorReasonPayload::UnknownField,
        )),
    }
}

fn descend_type<'r>(
    ty: &'r TypeDesc,
    parent: &'r str,
) -> Result<ResolveCursor<'r>, PathErrorPayload> {
    Ok(ResolveCursor::Type(ty, parent))
}

fn segment_render(seg: &OwnedSegmentPayload) -> String {
    match seg {
        OwnedSegmentPayload::Field(s) => s.clone(),
        OwnedSegmentPayload::Index(i) => i.to_string(),
        OwnedSegmentPayload::Wildcard => "*".to_string(),
        OwnedSegmentPayload::VariantName(s) => format!("@{s}"),
    }
}

fn field_alternatives(layout: &StructLayout) -> Vec<String> {
    layout.fields().map(|f| f.name.clone()).collect()
}

fn branch_alternatives(layout: &StructLayout) -> Vec<String> {
    layout.branches().map(|(_, f)| f.name.clone()).collect()
}
