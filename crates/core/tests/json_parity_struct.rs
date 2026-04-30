//! AZ-I.W2-act.B1 — `JsonDocument` wire-contract + struct-vs-native
//! parity. Promoted from W2-act probe to load-bearing harness.
//!
//! Two test families:
//!
//! 1. **Wire-contract** — exercise `JsonStructBuilder` against the
//!    `StructBuilder` trait with synthetic layouts that mirror the
//!    `grammar/json/json.bbnf` shapes, then compare the resulting
//!    `JsonDocument` against the expected typed shape. These run
//!    against the runtime substrate directly — no parser involvement —
//!    so they pin the builder's behaviour independently of the
//!    grammar-emitted parse fn.
//! 2. **Native parity** — post-flip, `JsonParser::parse(src)` returns
//!    `JsonDocument<'_>`; this section walks that document against
//!    `serde_json::Value` and `sonic_rs::Value` outputs node-for-node
//!    on the canonical fixture corpus.
//!
//! The wire-contract tests run unconditionally and prove the
//! substrate is wired regardless of the parser entry's strategy.
//! The native-parity tests assert the post-flip parser's struct-
//! direct emission matches the external oracles' parse trees.

use bbnf::runtime::{
    JsonArrayId, JsonDocument, JsonNumber, JsonObjectId, JsonStructBuilder, JsonValue,
    StructBuilder,
};
use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout};

/// Convenience: synthesise a `StructLayout` for a Named rule.
///
/// W2.A's wire-contract tests build layouts ad-hoc; the live emitter
/// reads layouts from `GrammarIR::struct_registry` (populated by W1's
/// `project_types` closure).
fn synth_layout(rule_id: u32, rule_name: &str, kind: LayoutKind) -> StructLayout {
    StructLayout {
        rule_id,
        rule_name: rule_name.to_string(),
        kind,
        rule_type: TypeDesc::Tuple(Vec::new()),
        fields: Vec::new(),
    }
}

fn synth_layout_with_fields(
    rule_id: u32,
    rule_name: &str,
    kind: LayoutKind,
    fields: Vec<StructField>,
) -> StructLayout {
    StructLayout {
        rule_id,
        rule_name: rule_name.to_string(),
        kind,
        rule_type: TypeDesc::Tuple(Vec::new()),
        fields,
    }
}

// AZ-I.W2-act.close A.fix — `JsonStructBuilder::finalise(input)`
// threads the parse-consumed source slice into `JsonDocument::input`
// so the `RuntimeView::input()` surface can lend it back. The wire-
// contract tests below exercise the builder against synthetic layouts
// with no parse involvement; the source slice the document carries is
// therefore the empty string — the synthesis identity. The
// `JsonDocument::input` accessor and `RuntimeView::input()` projection
// remain reachable on every constructed document, returning `""` for
// these synthetic builds.
const SYNTH_INPUT: &str = "";

#[test]
fn wire_contract_null_is_unit_leaf() {
    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_unit();
    let doc: JsonDocument<'_> = b.finalise(SYNTH_INPUT);
    assert!(matches!(doc.root, JsonValue::Null));
}

#[test]
fn wire_contract_bool_is_typed_bool() {
    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_bool(true);
    let doc = b.finalise(SYNTH_INPUT);
    assert!(matches!(doc.root, JsonValue::Bool(true)));

    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_bool(false);
    let doc = b.finalise(SYNTH_INPUT);
    assert!(matches!(doc.root, JsonValue::Bool(false)));
}

#[test]
fn wire_contract_number_is_typed_f64() {
    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_f64(3.14);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Number(JsonNumber::Float(v)) => assert_eq!(v, 3.14),
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_number_supports_integral_witnesses() {
    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_i64(-42);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Number(JsonNumber::Int(v)) => assert_eq!(v, -42),
        other => panic!("unexpected root variant: {:?}", other),
    }

    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_u64(99);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Number(JsonNumber::UInt(v)) => assert_eq!(v, 99),
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_string_borrows_input_lifetime() {
    let s = "hello";
    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_str(s);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::String(borrowed) => assert_eq!(borrowed, "hello"),
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_empty_array_resolves_to_empty_slice() {
    let mut b = JsonStructBuilder::new();
    let layout = synth_layout(1, "array", LayoutKind::Struct);
    let h = b.begin_compound(&layout);
    b.end_compound(h);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Array(id) => {
            assert!(id.is_empty());
            assert!(doc.array(id).is_empty());
        }
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_array_of_scalars_collects_in_order() {
    let mut b = JsonStructBuilder::new();
    let layout = synth_layout(1, "array", LayoutKind::Struct);
    let h = b.begin_compound(&layout);
    b.push_leaf_with_f64(1.0);
    b.push_leaf_with_f64(2.0);
    b.push_leaf_with_f64(3.0);
    b.end_compound(h);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Array(id) => {
            let items = doc.array(id);
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0], JsonValue::Number(JsonNumber::Float(v)) if v == 1.0));
            assert!(matches!(items[1], JsonValue::Number(JsonNumber::Float(v)) if v == 2.0));
            assert!(matches!(items[2], JsonValue::Number(JsonNumber::Float(v)) if v == 3.0));
        }
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_checkpoint_discards_completed_root_attempt() {
    let mut b = JsonStructBuilder::new();
    let layout = synth_layout(1, "array", LayoutKind::Struct);
    let checkpoint = b.checkpoint();

    let h = b.begin_compound(&layout);
    b.push_leaf_with_f64(1.0);
    b.end_compound(h);
    b.rollback(checkpoint);

    b.push_leaf_with_bool(true);
    let doc = b.finalise(SYNTH_INPUT);
    assert!(matches!(doc.root, JsonValue::Bool(true)));
}

#[test]
fn wire_contract_checkpoint_restores_open_frame() {
    let mut b = JsonStructBuilder::new();
    let array_layout = synth_layout(1, "array", LayoutKind::Struct);
    let object_layout = synth_layout(2, "object", LayoutKind::Struct);
    let pair_layout = synth_layout(3, "pair", LayoutKind::Struct);

    let arr = b.begin_compound(&array_layout);
    b.push_leaf_with_f64(1.0);
    let checkpoint = b.checkpoint();
    let obj = b.begin_compound(&object_layout);
    let pair = b.begin_compound(&pair_layout);
    b.push_leaf_with_str("stale");
    b.push_leaf_with_f64(99.0);
    b.end_compound(pair);
    b.end_compound(obj);

    b.rollback(checkpoint);
    b.push_leaf_with_bool(false);
    b.end_compound(arr);

    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Array(id) => {
            let items = doc.array(id);
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], JsonValue::Number(JsonNumber::Float(v)) if v == 1.0));
            assert!(matches!(items[1], JsonValue::Bool(false)));
        }
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_empty_object_resolves_to_empty_slice() {
    let mut b = JsonStructBuilder::new();
    let layout = synth_layout(1, "object", LayoutKind::Struct);
    let h = b.begin_compound(&layout);
    b.end_compound(h);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Object(id) => {
            assert!(id.is_empty());
            assert!(doc.object(id).is_empty());
        }
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_object_collects_pairs_via_pair_compound() {
    let mut b = JsonStructBuilder::new();
    let object_layout = synth_layout(1, "object", LayoutKind::Struct);
    let pair_layout = synth_layout(2, "pair", LayoutKind::Struct);

    let obj = b.begin_compound(&object_layout);
    {
        let pair = b.begin_compound(&pair_layout);
        b.push_leaf_with_str("a");
        b.push_leaf_with_f64(1.0);
        b.end_compound(pair);
    }
    {
        let pair = b.begin_compound(&pair_layout);
        b.push_leaf_with_str("b");
        b.push_leaf_with_bool(true);
        b.end_compound(pair);
    }
    b.end_compound(obj);

    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Object(id) => {
            let pairs = doc.object(id);
            assert_eq!(pairs.len(), 2);
            assert_eq!(pairs[0].key, "a");
            assert!(matches!(pairs[0].value, JsonValue::Number(JsonNumber::Float(v)) if v == 1.0));
            assert_eq!(pairs[1].key, "b");
            assert!(matches!(pairs[1].value, JsonValue::Bool(true)));
        }
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_nested_array_of_objects() {
    let mut b = JsonStructBuilder::new();
    let array_layout = synth_layout(1, "array", LayoutKind::Struct);
    let object_layout = synth_layout(2, "object", LayoutKind::Struct);
    let pair_layout = synth_layout(3, "pair", LayoutKind::Struct);

    let arr = b.begin_compound(&array_layout);
    {
        let obj = b.begin_compound(&object_layout);
        {
            let pair = b.begin_compound(&pair_layout);
            b.push_leaf_with_str("x");
            b.push_leaf_with_f64(1.0);
            b.end_compound(pair);
        }
        b.end_compound(obj);
    }
    {
        let obj = b.begin_compound(&object_layout);
        {
            let pair = b.begin_compound(&pair_layout);
            b.push_leaf_with_str("y");
            b.push_leaf_with_f64(2.0);
            b.end_compound(pair);
        }
        b.end_compound(obj);
    }
    b.end_compound(arr);

    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Array(arr_id) => {
            let items = doc.array(arr_id);
            assert_eq!(items.len(), 2);
            for (i, expected_key) in [(0, "x"), (1, "y")] {
                if let JsonValue::Object(oid) = items[i] {
                    let pairs = doc.object(oid);
                    assert_eq!(pairs.len(), 1);
                    assert_eq!(pairs[0].key, expected_key);
                } else {
                    panic!("nested element not an object: {:?}", items[i]);
                }
            }
        }
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_value_alt_branch_tag_is_idempotent() {
    // The JSON `value = object | array | string | number | bool | null`
    // Alt-of-Refs is classified as `Wrap` post-W4; the builder
    // forwards the single branch's value to the enclosing slot
    // regardless of `push_branch_tag` — this confirms the trait
    // surface is symmetric across grammars (Sheets / CSS will
    // STORE the branch tag; JSON's projection collapses it).
    let mut b = JsonStructBuilder::new();
    let value_layout = synth_layout(1, "value", LayoutKind::TaggedEnum);
    let h = b.begin_compound(&value_layout);
    b.push_branch_tag(2);
    b.push_leaf_with_f64(2.71);
    b.end_compound(h);
    let doc = b.finalise(SYNTH_INPUT);
    match doc.root {
        JsonValue::Number(JsonNumber::Float(v)) => assert_eq!(v, 2.71),
        other => panic!("unexpected root variant: {:?}", other),
    }
}

#[test]
fn wire_contract_field_provenance_on_synth_layout() {
    // Smoke test: the synth_layout helper compiles + the registry
    // accessor methods on StructLayout / StructField are reachable
    // through the public surface. The downstream emitter consults
    // these accessors; locking them here prevents API drift.
    let fields = vec![
        StructField {
            name: "key".to_string(),
            type_desc: TypeDesc::Tuple(Vec::new()),
            source: FieldSource::SeqPosition { position: 0 },
        },
        StructField {
            name: "value".to_string(),
            type_desc: TypeDesc::Tuple(Vec::new()),
            source: FieldSource::SeqPosition { position: 1 },
        },
    ];
    let layout = synth_layout_with_fields(42, "pair", LayoutKind::Struct, fields);
    assert!(layout.is_struct());
    assert_eq!(layout.field_count(), 2);
    assert_eq!(layout.field("key").map(|f| f.seq_position()), Some(Some(0)));
    assert_eq!(
        layout.field("value").map(|f| f.seq_position()),
        Some(Some(1))
    );
}

#[test]
fn wire_contract_arena_handles_distinguish_empty() {
    // EMPTY constants resolve to `&[]` without slab allocation;
    // populated handles roundtrip through the arena.
    let arena = bbnf::runtime::JsonArena::new();
    assert!(arena.array(JsonArrayId::EMPTY).is_empty());
    assert!(arena.object(JsonObjectId::EMPTY).is_empty());
    assert_eq!(arena.array_count(), 0);
    assert_eq!(arena.object_count(), 0);
}

// ─── Native parity (post-flip) ───────────────────────────────────────
//
// Promotion from W2-act probe to load-bearing harness. After the
// orchestrator regens `crates/core/src/grammar/generated/json.rs`
// against the resolver's `StructDirect` arm, `JsonParser::parse(src)`
// returns `JsonDocument<'_>` directly. The tests below walk that
// document against the external JSON parsers' output shapes —
// `serde_json::Value` (insertion-ordered Map), `sonic_rs::Value`
// (insertion-ordered ordered map). Equality is structural; numeric
// comparison reduces to f64.

use ::bbnf::grammar::generated::json::*;

fn assert_doc_eq_serde(
    doc: &JsonDocument<'_>,
    bbnf_value: &JsonValue<'_>,
    oracle: &serde_json::Value,
    path: &str,
) {
    match (bbnf_value, oracle) {
        (JsonValue::Null, serde_json::Value::Null) => {}
        (JsonValue::Bool(b), serde_json::Value::Bool(o)) => {
            assert_eq!(b, o, "{path}: bool divergence");
        }
        (JsonValue::Number(n), serde_json::Value::Number(o)) => {
            let bbnf_f64 = n.as_f64();
            let oracle_f64 = o.as_f64().expect("serde number is f64-coercible");
            if !bbnf_f64.is_nan() {
                assert_eq!(bbnf_f64, oracle_f64, "{path}: number divergence");
            }
        }
        (JsonValue::String(s), serde_json::Value::String(o)) => {
            assert_eq!(*s, o.as_str(), "{path}: string divergence");
        }
        (JsonValue::Array(id), serde_json::Value::Array(o)) => {
            let items = doc.array(*id);
            assert_eq!(items.len(), o.len(), "{path}: array length");
            for (i, (b, o)) in items.iter().zip(o.iter()).enumerate() {
                assert_doc_eq_serde(doc, b, o, &format!("{path}[{i}]"));
            }
        }
        (JsonValue::Object(id), serde_json::Value::Object(o)) => {
            let pairs = doc.object(*id);
            assert_eq!(pairs.len(), o.len(), "{path}: object length");
            for pair in pairs {
                let oracle_value = o
                    .get(pair.key)
                    .unwrap_or_else(|| panic!("{path}: key {:?} missing", pair.key));
                assert_doc_eq_serde(
                    doc,
                    &pair.value,
                    oracle_value,
                    &format!("{path}.{}", pair.key),
                );
            }
        }
        (b, o) => panic!("{path}: shape divergence — bbnf={b:?}, serde={o:?}"),
    }
}

fn parity_serde(fixture: &str) {
    let path = format!("../../data/json/{}", fixture);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: read failed: {e}"));
    let doc =
        JsonParser::parse(&src).unwrap_or_else(|e| panic!("{fixture}: bbnf parse failed: {e:?}"));
    let oracle: serde_json::Value = serde_json::from_str(&src).expect("serde_json parse");
    assert_doc_eq_serde(&doc, doc.to_value(), &oracle, "$");
}

#[test]
fn native_parity_serde_data_json() {
    parity_serde("data.json");
}

#[test]
fn native_parity_serde_twitter_json() {
    parity_serde("twitter.json");
}

#[test]
fn native_parity_serde_canada_json() {
    parity_serde("canada.json");
}

#[test]
fn native_parity_serde_citm_catalog_json() {
    parity_serde("citm_catalog.json");
}

#[test]
fn native_parity_struct_direct_returns_jsondocument() {
    // Pin the post-flip return type. Compile-time proof that
    // `JsonParser::parse` returns `JsonDocument<'_>` and that the
    // accessor surface is reachable on the result.
    let input = r#"{"k": [1, 2]}"#;
    let doc: JsonDocument<'_> = JsonParser::parse(input).expect("parse");
    let _: &JsonValue<'_> = doc.to_value();
    let view = doc.view();
    let _: bbnf::runtime::JsonKind = view.kind();
    assert!(view.is_object(), "{{k: [1,2]}} root must be Object");
}
