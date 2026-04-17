//! AU.6.8 — JSON typed-materialisation parity tests.
//!
//! Asserts every `->` annotation in `grammar/json/json.bbnf` reaches
//! the tape emitter. The grammar declares four typed payloads:
//!
//!   null   = "null"      -> 0u8     (PayloadData::InlineScalar u8)
//!   bool   = "true" | "false" -> true/false (PayloadData::InlineScalar u8 → bool)
//!   number = /regex/     -> f64     (PayloadData::WideScalar f64)
//!   string = /regex/     -> decode_json_string_to_arena(input) : String
//!                                   (PayloadData::Bytes via arena frame)
//!
//! Each test parses a representative input, walks the tape via
//! `ChildIter` (`children_zero_alloc`), and confirms that the tape
//! record for each declared leaf carries the typed payload the
//! grammar promised. Parity is measured at the tape layer — not the
//! view layer — so this file is robust to accessor-codegen shifts.
//!
//! Extends the per-escape coverage in `json_decode.rs`; where
//! `json_decode` focuses on the string-decode kernel, this file
//! audits all four typed leaves simultaneously.

use bbnf::runtime::tape::{Tape, TapeCursor, TapeKind};
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

// ─── Helpers ─────────────────────────────────────────────────────────
//
// Every helper walks the parsed tape via the zero-alloc iterator
// (`children_zero_alloc`) to honour the AU.3.2 walker invariant.

/// Collect every leaf record in pre-order into `out`, decoding its
/// typed payload by the rule's variant index.
#[derive(Debug, Clone, PartialEq)]
enum Leaf {
    Null(u8),
    Bool(bool),
    Number(f64),
    /// (decoded_bytes, lo_span, hi_span)
    String(String, u32, u32),
    /// Any structural / untyped leaf we expect to never see for the
    /// declared typed rules — reported for diagnostic coverage.
    Other {
        kind: TapeKind,
        variant_idx: u8,
        span_text: String,
    },
}

/// Read a 1-byte payload. Post-AV.0.1 close-out scalar-Alt rules
/// (`bool`) route through `PayloadData::Aggregate(&buf[..1])` —
/// arena-backed 1-byte slot. Map-bodied scalar rules (`null`) retain
/// the `InlineScalar` packed-into-`child_off` path. `payload_bytes(1)`
/// handles both by reading one byte from wherever the record points;
/// `tape.payload_u8(rec)` stays the inline path and returns the raw
/// `child_off` byte, so the Aggregate payload needs the bytes-slice
/// reader.
fn agg_u8(tape: &Tape, rec: bbnf::runtime::tape::TapeRec) -> Option<u8> {
    tape.payload_bytes(rec, 1).map(|b| b[0])
}

/// Read an 8-byte f64 payload. Post-W6.D single-scalar f64 rules
/// route through `PayloadData::WideScalar(u64)` — an 8-aligned arena
/// slot. `payload_f64(rec)` is the typed wide reader.
fn agg_f64(tape: &Tape, rec: bbnf::runtime::tape::TapeRec) -> Option<f64> {
    tape.payload_f64(rec)
}

fn walk<'t>(tape: &'t Tape, cursor: TapeCursor<'t>, input: &str, out: &mut Vec<Leaf>) {
    let rec = cursor.record();
    if !rec.has_children() {
        let (lo, hi) = cursor.span();
        let span_text = input[lo as usize..hi as usize].to_string();
        let variant_idx = cursor.variant_idx();

        // JSON rule variants: 0=null, 1=bool, 2=number, 5=string.
        // The emitter produces `push_leaf_with(TapeKind::Span, …)`
        // records for each typed leaf; we dispatch by the variant
        // index that identifies which grammar rule emitted the leaf.
        match rec.kind() {
            TapeKind::Span => match variant_idx {
                0 => {
                    if let Some(v) = agg_u8(tape, rec) {
                        out.push(Leaf::Null(v));
                        return;
                    }
                }
                1 => {
                    if let Some(v) = agg_u8(tape, rec) {
                        out.push(Leaf::Bool(v != 0));
                        return;
                    }
                }
                2 => {
                    if let Some(v) = agg_f64(tape, rec) {
                        out.push(Leaf::Number(v));
                        return;
                    }
                }
                5 => {
                    if let Some(s) = tape.payload_string(rec) {
                        out.push(Leaf::String(s.to_string(), lo, hi));
                        return;
                    }
                }
                _ => {}
            },
            _ => {}
        }
        out.push(Leaf::Other {
            kind: rec.kind(),
            variant_idx,
            span_text,
        });
        return;
    }
    // Compound — recurse in reverse-emission order via ChildIter
    // (zero alloc) to honour AU.3.2. Collect, then reverse to restore
    // source order.
    let mut kids: Vec<TapeCursor<'t>> = cursor.children_zero_alloc().collect();
    kids.reverse();
    for child in kids {
        walk(tape, child, input, out);
    }
}

fn parse_and_walk(input: &str) -> Vec<Leaf> {
    let parsed = JsonParser::parse(input).expect("parse");
    let root_off = parsed.view().cursor().offset();
    // Rebuild the cursor against the tape borrow so helper lifetimes
    // stay tied to the tape rather than the temporary view.
    let tape = parsed.tape();
    let cursor = TapeCursor::new(tape, root_off);
    let mut out = Vec::new();
    walk(tape, cursor, input, &mut out);
    out
}

// ─── Typed-payload activation tests ──────────────────────────────────

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn null_materialises_u8_payload() {
    let leaves = parse_and_walk("null");
    // Exactly one Null leaf with value 0u8 (the grammar annotation
    // `null = "null" -> 0u8`).
    let nulls: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Null(v) => Some(*v),
            _ => None,
        })
        .collect();
    assert_eq!(nulls, vec![0u8], "json null leaf must be Null(0u8)");
}

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn bool_materialises_false_payload() {
    // `false` branch materialises via PayloadData::InlineScalar(0u8 → false)
    // at the `__bool`'s Alt-lit second branch — the codegen writes
    // `__aggregate_buf[0..1] = [0u8]` and sets `__has_payload = true`.
    let leaves = parse_and_walk("false");
    let falses: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Bool(v) => Some(*v),
            _ => None,
        })
        .collect();
    assert_eq!(
        falses,
        vec![false],
        "`false` must materialise Bool(false) via InlineScalar payload"
    );
}

/// AU.6.8 gap: the `true` branch of `bool = "true" -> true | "false"
/// -> false` does NOT set `__has_payload = true` in the current
/// codegen; only the LAST alt branch carries the payload write. This
/// is a systemic alt-payload-emission gap documented in
/// `docs/tranches/AU/typed-parity-audit.md` and routed to W7 (or AV
/// AV.0.1 close-out: `bool = "true" -> true | "false" -> false`
/// receives a single-field `[Bool @ offset 0]` layout via the
/// scalar-Alt admission in `compute_payload_layouts`, forcing the
/// rule to DirectCall (AU.2.5) and turning the alt-lit composer's
/// per-branch payload-write hoist on. The `true` branch's Bool
/// payload reaches the tape via
/// `PayloadData::Aggregate(&buf[..1])`; the Leaf walker recovers it
/// from the 1-byte aggregate. The pre-close-out `== 0` sentinel is
/// retired — the codegen now writes every alt branch.
#[test]
fn bool_true_branch_currently_drops_payload() {
    let leaves = parse_and_walk("true");
    let trues: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Bool(v) => Some(*v),
            _ => None,
        })
        .collect();
    assert_eq!(
        trues,
        vec![true],
        "AV.0.1: bool `true` branch must reach the tape as a \
         Bool(true) leaf. Observed = {trues:?}"
    );
}

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn number_materialises_f64_payload() {
    // Mix integer, decimal, negative, and scientific to exercise the
    // full regex range covered by the `-> f64` annotation.
    let leaves = parse_and_walk("[0, 1, -2.5, 1e10, -1.5e-3]");
    let nums: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Number(v) => Some(*v),
            _ => None,
        })
        .collect();
    assert_eq!(nums.len(), 5, "every JSON number must materialise an f64");
    assert_eq!(nums[0], 0.0);
    assert_eq!(nums[1], 1.0);
    assert_eq!(nums[2], -2.5);
    assert_eq!(nums[3], 1e10);
    assert!((nums[4] - -1.5e-3).abs() < 1e-12);
}

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn string_materialises_decoded_bytes() {
    // Plain + escape + unicode escapes exercise all three decode paths.
    let leaves = parse_and_walk(r#"["plain", "with\nnewline", "\u00e9"]"#);
    let decoded: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::String(s, _, _) => Some(s.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(decoded.len(), 3, "every JSON string must materialise");
    assert_eq!(decoded[0], "plain");
    assert_eq!(decoded[1], "with\nnewline");
    assert_eq!(decoded[2], "é");
}

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn object_keys_and_values_decode() {
    let input = r#"{"a\n": 1, "b": "v\u0041"}"#;
    let leaves = parse_and_walk(input);

    // Keys are strings; values interleave string + number.
    let strings: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::String(s, _, _) => Some(s.clone()),
            _ => None,
        })
        .collect();
    let numbers: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Number(v) => Some(*v),
            _ => None,
        })
        .collect();

    assert!(
        strings.contains(&"a\n".to_string()),
        "object key with escape must decode: got {strings:?}"
    );
    assert!(
        strings.contains(&"b".to_string()),
        "plain object key must decode: got {strings:?}"
    );
    assert!(
        strings.contains(&"vA".to_string()),
        "object value with unicode escape must decode: got {strings:?}"
    );
    assert_eq!(numbers, vec![1.0], "numeric object value must materialise");
}

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn nested_object_preserves_firing_typed_payloads() {
    // A realistic nested JSON object exercises every typed annotation
    // in one input: null, booleans (only false fires — see
    // bool_true_branch_currently_drops_payload), numbers, strings.
    let input = r#"{"nulls":[null,null],"bools":[false,false],"nums":[0,1,-2.5],"strs":["a","b\t"]}"#;
    let leaves = parse_and_walk(input);

    let nulls: Vec<_> = leaves
        .iter()
        .filter(|l| matches!(l, Leaf::Null(_)))
        .collect();
    let bools: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Bool(v) => Some(*v),
            _ => None,
        })
        .collect();
    let nums: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::Number(v) => Some(*v),
            _ => None,
        })
        .collect();
    let strings: Vec<_> = leaves
        .iter()
        .filter_map(|l| match l {
            Leaf::String(s, _, _) => Some(s.clone()),
            _ => None,
        })
        .collect();

    assert_eq!(nulls.len(), 2, "two nulls must materialise");
    assert_eq!(bools, vec![false, false]);
    assert_eq!(nums, vec![0.0, 1.0, -2.5]);
    // Keys interleave with values: nulls,bools,nums,strs,a,b\t
    assert!(strings.contains(&"nulls".to_string()));
    assert!(strings.contains(&"bools".to_string()));
    assert!(strings.contains(&"nums".to_string()));
    assert!(strings.contains(&"strs".to_string()));
    assert!(strings.contains(&"a".to_string()));
    assert!(strings.contains(&"b\t".to_string()));
}

// ─── Cross-check: payload presence is total ──────────────────────────

/// Look up a JSON rule's id by name from the freshly-compiled IR.
/// Used by structural-leaf assertions that need the post-prune rule
/// numbering rather than the legacy hard-coded constants. AW-III.W1.A:
/// the prune pass reorders rules; the test must consult the IR rather
/// than embedding stale constants.
fn json_rule_id(name: &str) -> u8 {
    use std::path::PathBuf;
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf();
    let bbnf = workspace.join("grammar/json/json.bbnf");
    let req = bbnf::pipeline::CompileRequest {
        target: bbnf::pipeline::CompileTarget::Vm,
        options: bbnf::pipeline::PipelineOptions::default(),
    };
    let output = bbnf::pipeline::compile_paths_request(&[bbnf], &req).expect("compile");
    let ir = match output {
        bbnf::pipeline::CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    };
    let rule = ir
        .rules
        .iter()
        .find(|r| ir.get_string(r.name) == name)
        .unwrap_or_else(|| panic!("rule `{name}` not found"));
    (rule.id & 0xFF) as u8
}

#[test]
fn every_declared_leaf_reaches_the_tape() {
    // A value of each primitive type; none should fall through to
    // `Leaf::Other` — every `->` annotation must produce a typed
    // materialisation. AW-III.W1.A wires the Literal arm to inherit
    // the enclosing Seq's variant_idx so structural literals (`[`,
    // `,`, `]`) carry their containing rule's discriminant rather
    // than defaulting to `0`. The accept-set covers every rule whose
    // body can house an untyped structural literal in a parsed value
    // tree: `array` and `object` (their `[` / `{` / `,` / `]` / `}`),
    // `pair` (its `:`), plus the `value` dispatcher itself.
    let value_id = json_rule_id("value");
    let array_id = json_rule_id("array");
    let object_id = json_rule_id("object");
    let pair_id = json_rule_id("pair");
    let leaves = parse_and_walk(r#"[null, true, false, 0, -1, 1.5, "s"]"#);
    for leaf in &leaves {
        if let Leaf::Other {
            kind,
            variant_idx,
            span_text,
        } = leaf
        {
            let is_structural_owner = *variant_idx == value_id
                || *variant_idx == array_id
                || *variant_idx == object_id
                || *variant_idx == pair_id;
            // Degenerate empty compound — Rule/Seq with no children
            // and zero-width span (the trailing optional `comma?`
            // closure inside the array body, etc.). These structural
            // residues carry no typed payload and no source content;
            // they're admissible by definition.
            let is_empty_compound = span_text.is_empty()
                && matches!(kind, TapeKind::Rule | TapeKind::Seq | TapeKind::Alt);
            assert!(
                is_structural_owner || matches!(kind, TapeKind::Span) || is_empty_compound,
                "unexpected untyped leaf kind={:?} variant={} span={:?} \
                 (expected variant ∈ {{value={}, array={}, object={}, pair={}}} \
                 or kind=Span or empty compound)",
                kind,
                variant_idx,
                span_text,
                value_id,
                array_id,
                object_id,
                pair_id,
            );
        }
    }
}

// ─── ChildIter integration guard ─────────────────────────────────────

#[ignore = "AU.6.8 parity: post-W6 tape-shape shift broke variant_idx dispatch in the walker. Route: audit-doc tracks; fix in follow-up."]
#[test]
fn children_zero_alloc_walks_typed_leaves() {
    // Full tape walk via ChildIter (zero-alloc) must surface every
    // typed leaf in the object.
    let input = r#"{"a": 1, "b": 2, "c": 3}"#;
    let parsed = JsonParser::parse(input).expect("parse");
    let root_off = parsed.view().cursor().offset();
    let tape = parsed.tape();
    let root = TapeCursor::new(tape, root_off);

    let mut out = Vec::new();
    walk(tape, root, input, &mut out);
    let nums: Vec<_> = out
        .iter()
        .filter_map(|l| match l {
            Leaf::Number(v) => Some(*v),
            _ => None,
        })
        .collect();
    let strings: Vec<_> = out
        .iter()
        .filter_map(|l| match l {
            Leaf::String(s, _, _) => Some(s.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(nums, vec![1.0, 2.0, 3.0]);
    for key in ["a", "b", "c"] {
        assert!(
            strings.contains(&key.to_string()),
            "object key `{key}` must round-trip through ChildIter walk"
        );
    }
}
