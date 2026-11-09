//! PROTO-2 (LOCK-6 6A) — canonical `bbnf::path` reference dumper.
//!
//! Emits the `(segment_index, segment_str, struct_name, alternatives,
//! reason)` tuple the *canonical* Rust frontend
//! (`bbnf::path::check_path_against_registry`) surfaces for the five
//! drive paths, as a JSON array on stdout. The Node drive
//! (`node_drive.mjs`) loads the wasm cdylib and compares its
//! `PathErrorPayload` field-for-field against these rows.
//!
//! The registry fixture is byte-identical to the one in
//! `tests/isomorphic_path_error.rs` and `src/fixture.rs` (JSON grammar:
//! Document{ id:F64, statuses:Vec<Status> }, Status{ text, retweets,
//! sensitive }). Both frontends validate the same shape; the Node drive
//! proves where they agree and where they diverge.

use bbnf::path::{
    check_path_against_registry, Json, PathError, PathErrorReason, PathSegment, TypedPath,
};
use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{StringId, TypeDesc};

const JSON_DOC_RULE: &str = "Document";
const JSON_DOC_ID: StringId = 0;
const JSON_STATUS_ID: StringId = 1;

fn json_registry() -> StructRegistry {
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
        rule_name: "Status".to_string(),
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
    r
}

fn reason_name(r: &PathErrorReason) -> &'static str {
    match r {
        PathErrorReason::UnknownField => "UnknownField",
        PathErrorReason::UnknownVariant => "UnknownVariant",
        PathErrorReason::IndexIntoNonList => "IndexIntoNonList",
        PathErrorReason::FieldOnScalar => "FieldOnScalar",
        PathErrorReason::EmptyPath => "EmptyPath",
        PathErrorReason::UnregisteredRule => "UnregisteredRule",
        PathErrorReason::AmbiguousVariant => "AmbiguousVariant",
        PathErrorReason::WildcardOverflow => "WildcardOverflow",
    }
}

fn json_str(s: &str) -> String {
    let mut o = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            _ => o.push(c),
        }
    }
    o.push('"');
    o
}

fn row(name: &str, res: Result<TypedPath<Json, ()>, PathError>) -> String {
    match res {
        Ok(tp) => {
            let segs: Vec<String> = tp
                .owned_segments()
                .iter()
                .map(|s| json_str(&s.to_string()))
                .collect();
            format!(
                "{{\"path\":{},\"ok\":true,\"segments\":[{}]}}",
                json_str(name),
                segs.join(",")
            )
        }
        Err(e) => {
            let alts: Vec<String> = e.alternatives.iter().map(|a| json_str(a)).collect();
            format!(
                "{{\"path\":{},\"ok\":false,\"segment_index\":{},\"segment_str\":{},\"struct_name\":{},\"alternatives\":[{}],\"reason\":{}}}",
                json_str(name),
                e.segment_index,
                json_str(&e.segment_str),
                json_str(&e.struct_name),
                alts.join(","),
                json_str(reason_name(&e.reason)),
            )
        }
    }
}

fn main() {
    let reg = json_registry();
    let go = |segs: &[PathSegment<'_>]| {
        check_path_against_registry::<Json, ()>(segs, &reg, JSON_DOC_RULE)
    };

    let rows = vec![
        row(
            "$.statuses[0].text",
            go(&[
                PathSegment::Field("statuses"),
                PathSegment::Index(0),
                PathSegment::Field("text"),
            ]),
        ),
        row("$.nope", go(&[PathSegment::Field("nope")])),
        row(
            "$.id[0]",
            go(&[PathSegment::Field("id"), PathSegment::Index(0)]),
        ),
        row("", go(&[])),
        row(
            "$.statuses[0].nope",
            go(&[
                PathSegment::Field("statuses"),
                PathSegment::Index(0),
                PathSegment::Field("nope"),
            ]),
        ),
    ];

    println!("[{}]", rows.join(","));
}
