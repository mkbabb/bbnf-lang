//! Per-grammar [`StructRegistry`] fixtures keyed by grammar marker.
//!
//! Mirrors `crates/bbnf-path/src/registry.rs` exactly — the cdylib
//! validates the same paths the proc-macro validates, against the
//! same fixture shapes. When T4 (`pub const REGISTRY: StructRegistry`
//! in generated code) lands, both call sites swap their fixture
//! lookup for the production const without changing the rest of the
//! pipeline.
//!
//! The fixture shape is deliberately small (Document/Status for JSON,
//! Stylesheet/Rule/Declaration for CSS L4, etc.) — large enough to
//! exercise the hard-gate scenarios, small enough that drift between
//! the proc-macro and the cdylib is mechanical to detect at review.
//!
//! W5.1 keeps the fixture shape identical to W2.4's fixture; the
//! cdylib's only role is to walk the same shape from the TS frontend.

use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{StringId, TypeDesc};

/// One grammar's registry fixture.
pub(crate) struct GrammarFixture {
    /// Entry-rule name path resolution starts at.
    pub(crate) entry_rule: &'static str,
    /// Populated registry keyed by `RuleId`.
    pub(crate) registry: StructRegistry,
}

/// Resolve a grammar marker's name to its compile-time fixture.
pub(crate) fn fixture_for_marker(name: &str) -> Option<GrammarFixture> {
    match name {
        "Json" => Some(json_fixture()),
        "CssL4" => Some(css_l4_fixture()),
        "Sheets" => Some(sheets_fixture()),
        "Bbnf" => Some(bbnf_fixture()),
        _ => None,
    }
}

/// Supported marker names sorted, for diagnostic alternatives.
pub(crate) fn supported_markers() -> &'static [&'static str] {
    &["Bbnf", "CssL4", "Json", "Sheets"]
}

// ─── JSON fixture ────────────────────────────────────────────────────

const JSON_DOC_RULE: &str = "Document";
const JSON_DOC_ID: StringId = 0;
const JSON_STATUS_RULE: &str = "Status";
const JSON_STATUS_ID: StringId = 1;

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

const CSS_DOC_RULE: &str = "Stylesheet";
const CSS_DOC_ID: StringId = 100;
const CSS_RULE_RULE: &str = "Rule";
const CSS_RULE_ID: StringId = 101;
const CSS_DECL_RULE: &str = "Declaration";
const CSS_DECL_ID: StringId = 102;

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
