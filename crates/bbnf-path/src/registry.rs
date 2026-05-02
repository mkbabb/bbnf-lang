//! Compile-time `StructRegistry` fixtures keyed by grammar marker.
//!
//! The proc-macro must validate paths against the per-grammar
//! [`StructRegistry`] at expansion time. The production registry is
//! built by `cargo xtask regen` and persisted alongside generated
//! parser code; until that pipeline exposes a per-grammar
//! `RegistryDescriptor` const the macro can `use`, the W2.4 landing
//! ships a synthetic per-grammar fixture sufficient to exercise the
//! hard-gate scenarios. The fixture mirrors the canonical shape of
//! each grammar (JSON `Document`/`statuses[*]`, CSS L4 root, etc.) so
//! `path!(Json, "statuses", 0, "text")` resolves end-to-end and
//! `path!(Json, "statuses", 0, "nope")` fails compilation with the
//! correct alternatives list.
//!
//! When `cargo xtask regen` exposes the production const, this module
//! reduces to a thin marker→const dispatch — the macro's public surface
//! stays unchanged.

use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{StringId, TypeDesc};

/// One grammar's registry fixture: the entry-rule name plus the
/// populated [`StructRegistry`].
pub(crate) struct GrammarFixture {
    /// Entry-rule name the path resolution starts at (the document
    /// root). Typically the start rule of the grammar's top-level
    /// production.
    pub(crate) entry_rule: &'static str,
    /// Populated registry keyed by `RuleId`.
    pub(crate) registry: StructRegistry,
}

/// Resolve a grammar marker's trailing identifier (`Json`, `CssL4`,
/// `Sheets`, `Bbnf`) to its compile-time fixture. Returns `None` if
/// the marker is unknown — the macro surfaces a `syn::Error` naming the
/// supported markers.
pub(crate) fn fixture_for_marker(name: &str) -> Option<GrammarFixture> {
    match name {
        "Json" => Some(json_fixture()),
        "CssL4" => Some(css_l4_fixture()),
        "Sheets" => Some(sheets_fixture()),
        "Bbnf" => Some(bbnf_fixture()),
        _ => None,
    }
}

/// Supported marker names, sorted, for diagnostic alternatives.
pub(crate) fn supported_markers() -> &'static [&'static str] {
    &["Bbnf", "CssL4", "Json", "Sheets"]
}

// ─── JSON fixture ────────────────────────────────────────────────────

const JSON_DOC_RULE: &str = "Document";
const JSON_DOC_ID: StringId = 0;
const JSON_STATUS_RULE: &str = "Status";
const JSON_STATUS_ID: StringId = 1;

/// Synthetic JSON-grammar registry exercising the hard-gate path
/// `path!(Json, "statuses", 0, "text")`:
///
/// ```text
/// Document = id: F64, statuses: Vec<Status>
/// Status   = text: Span, retweets: F64, sensitive: Bool
/// ```
fn json_fixture() -> GrammarFixture {
    let mut r = StructRegistry::new();

    r.insert(StructLayout {
        rule_id: JSON_DOC_ID,
        rule_name: JSON_DOC_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![
            TypeDesc::F64,
            TypeDesc::Vec(Box::new(TypeDesc::Named(JSON_STATUS_ID))),
        ]),
        fields: vec![
            StructField {
                name: "id".to_string(),
                type_desc: TypeDesc::F64,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "statuses".to_string(),
                type_desc: TypeDesc::Vec(Box::new(TypeDesc::Named(JSON_STATUS_ID))),
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });

    r.insert(StructLayout {
        rule_id: JSON_STATUS_ID,
        rule_name: JSON_STATUS_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::F64, TypeDesc::Bool]),
        fields: vec![
            StructField {
                name: "text".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "retweets".to_string(),
                type_desc: TypeDesc::F64,
                source: FieldSource::SeqPosition { position: 1 },
            },
            StructField {
                name: "sensitive".to_string(),
                type_desc: TypeDesc::Bool,
                source: FieldSource::SeqPosition { position: 2 },
            },
        ],
    });

    GrammarFixture {
        entry_rule: JSON_DOC_RULE,
        registry: r,
    }
}

// ─── CSS L4 fixture ──────────────────────────────────────────────────
//
// Per-grammar fixtures pair `rule_id` (registry key) with the `StringId`
// inside `TypeDesc::Named(...)` so the macro's `Named` resolver can map
// each named-rule reference back to its layout. The mapping is 1:1 in
// the fixture (the `rule_id` and the `StringId` of the same rule are
// identical numbers); production grammars carry a strings table on
// `GrammarIR` but the fixture's ID-pair shortcut is sufficient here.

const CSS_DOC_RULE: &str = "Stylesheet";
const CSS_DOC_ID: StringId = 100;
const CSS_RULE_RULE: &str = "Rule";
const CSS_RULE_ID: StringId = 101;
const CSS_DECL_RULE: &str = "Declaration";
const CSS_DECL_ID: StringId = 102;

/// Synthetic CSS L4 fixture: stylesheet → rules → declarations.
fn css_l4_fixture() -> GrammarFixture {
    let mut r = StructRegistry::new();

    r.insert(StructLayout {
        rule_id: CSS_DOC_ID,
        rule_name: CSS_DOC_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Vec(Box::new(TypeDesc::Named(CSS_RULE_ID))),
        fields: vec![StructField {
            name: "rules".to_string(),
            type_desc: TypeDesc::Vec(Box::new(TypeDesc::Named(CSS_RULE_ID))),
            source: FieldSource::SeqPosition { position: 0 },
        }],
    });

    r.insert(StructLayout {
        rule_id: CSS_RULE_ID,
        rule_name: CSS_RULE_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![
            TypeDesc::Span,
            TypeDesc::Vec(Box::new(TypeDesc::Named(CSS_DECL_ID))),
        ]),
        fields: vec![
            StructField {
                name: "selector".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "declarations".to_string(),
                type_desc: TypeDesc::Vec(Box::new(TypeDesc::Named(CSS_DECL_ID))),
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });

    r.insert(StructLayout {
        rule_id: CSS_DECL_ID,
        rule_name: CSS_DECL_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::Span]),
        fields: vec![
            StructField {
                name: "property".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "value".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });

    GrammarFixture {
        entry_rule: CSS_DOC_RULE,
        registry: r,
    }
}

// ─── Sheets fixture ──────────────────────────────────────────────────

const SHEETS_DOC_RULE: &str = "Formula";

/// Minimal Sheets fixture — formula root with a single typed expression
/// payload. The Sheets grammar's full struct shape lands in W4 codegen;
/// the W2.4 fixture exercises the macro path through a non-JSON marker.
fn sheets_fixture() -> GrammarFixture {
    let mut r = StructRegistry::new();

    r.insert(StructLayout {
        rule_id: 20,
        rule_name: SHEETS_DOC_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Span,
        fields: vec![StructField {
            name: "expression".to_string(),
            type_desc: TypeDesc::Span,
            source: FieldSource::SeqPosition { position: 0 },
        }],
    });

    GrammarFixture {
        entry_rule: SHEETS_DOC_RULE,
        registry: r,
    }
}

// ─── BBNF self-host fixture ──────────────────────────────────────────

const BBNF_DOC_RULE: &str = "Grammar";
const BBNF_DOC_ID: StringId = 200;
const BBNF_RULE_RULE: &str = "Rule";
const BBNF_RULE_ID: StringId = 201;

/// BBNF self-host fixture: grammar → rules.
fn bbnf_fixture() -> GrammarFixture {
    let mut r = StructRegistry::new();

    r.insert(StructLayout {
        rule_id: BBNF_DOC_ID,
        rule_name: BBNF_DOC_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Vec(Box::new(TypeDesc::Named(BBNF_RULE_ID))),
        fields: vec![StructField {
            name: "rules".to_string(),
            type_desc: TypeDesc::Vec(Box::new(TypeDesc::Named(BBNF_RULE_ID))),
            source: FieldSource::SeqPosition { position: 0 },
        }],
    });

    r.insert(StructLayout {
        rule_id: BBNF_RULE_ID,
        rule_name: BBNF_RULE_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::Span]),
        fields: vec![
            StructField {
                name: "name".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "body".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });

    GrammarFixture {
        entry_rule: BBNF_DOC_RULE,
        registry: r,
    }
}
