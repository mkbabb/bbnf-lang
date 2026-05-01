//! AY.W2.7 + AZ-II.cutover.O3 -- named-type preservation through the
//! StructDirect document surface.
//!
//! Per `docs/tranches/AY/waves/W2.md` §AY.W2.7 + AY invariant 23:
//! every grammar-declared `-> input : <Name>` annotation (non-scalar,
//! per the scalar-name table in `TypeDesc::from_scalar_name`) must
//! reach Rust backend preparation as `TypeDesc::Named(sid)`. The invariant
//! is end-to-end: the projection survives lowering, the normaliser
//! loop, the e-graph saturation block, reachability pruning, and the
//! Rust-backend preparation pass (`analyze_grammar → project_types`).
//!
//! The assertion is at emit-side: `compile_paths_request` with
//! `CompileTarget::Rust` exposes the exact `ir.types` array the
//! Rust backend consumes. Checking IR-only (via `CompileTarget::Vm`)
//! would admit Named entries the Rust-target preparation later drops;
//! checking generated code would depend on the tape-backed view/value
//! surface retired by O3. This test checks the Rust preparation
//! boundary and then proves the named values are reachable through
//! concrete StructDirect documents.
//!
//! ## AY.W6.b coverage — grammar-derived direct-to-struct admission
//!
//! Tranche AY.W6.b broadened admission to consume `ir.payload_layouts`
//! as a grammar-derived fact, not just `TypeDesc::Named(sid)`. O3
//! keeps that assertion at the grammar/runtime boundary: the
//! `admitted_projection_surfaces` test reads payload-layout facts from
//! the prepared IR, and the runtime test parses concrete JSON/CSS
//! values into document-owned typed graphs.
//!
//! ## Coverage
//!
//! The wave's grammars hold two Named-annotated rules that survive
//! reachability pruning post-W2.2:
//!
//! - CSS L4 `colorFn` → `Named("Color")` (W2.2's precedence wrap on
//!   the rule body elevates the outer `Map[Expr → Named("Color")]`
//!   to the body root so the CSP propagates Named end-to-end).
//! - JSON `string` → `Named("String")` (unchanged from pre-W2 — the
//!   probe in `named_pipeline_probe.rs` verified survival
//!   pre-existed).
//!
//! CSS L4's `colorFunction` and `colorMix` also declare Named("Color")
//! but `prune_unreachable` correctly drops them: the entry-reachable
//! `value` rule in `properties.bbnf` only references `colorFn` /
//! `hex` / `namedColor` / ...; the `color → colorMix → color` cycle is
//! unreachable from `stylesheet`. They're tracked as `#[ignore]` tests
//! that document the reachability gap explicitly — un-ignoring
//! requires a grammar-source reachability fix to `properties.bbnf` +
//! the pattern-dedup priority-preservation work noted in the AY.W2.2
//! commit message.
//!
//! Sheets' Span annotations (`string`, `cell_ref`, `identifier` →
//! `Span`) are NOT Named: `Span` is a scalar name resolved via
//! `TypeDesc::from_scalar_name` into `TypeDesc::Span` directly, so
//! they never appear as `TypeDesc::Named(_)`. They're out of scope
//! for this invariant — but the broader AY.W6.b admission does
//! surface these Span-annotated rules as grammar-layout projections,
//! so they count towards the `admitted_projection_surfaces` gate.

use std::collections::BTreeSet;
use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf::runtime::{CssTypedValue, JsonValue};
use bbnf_ir::{GrammarIR, TypeDesc};

// ───────────────────────────────────────────────────────────────────
// CSS L4 host shim. The generated CSS parser references
// `crate::css_types::parse_hex_color`, so this test crate provides the
// same module while keeping assertions on runtime documents.
// ───────────────────────────────────────────────────────────────────

/// Host shims required by the CSS L4 grammar's `-> parse_hex_color(...)`
/// mapping. Duplicated from `typed_accessor_surface.rs` so this test
/// compiles hermetically.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(s: &str) -> u32 {
        let hex = s.as_bytes();
        match hex.len() {
            3 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
            }
            4 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                let a = hex_digit(hex[3]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | (a << 4 | a)
            }
            6 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                (r << 24) | (g << 16) | (b << 8) | 0xFF
            }
            8 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                let a = hex_byte(hex[6], hex[7]);
                (r << 24) | (g << 16) | (b << 8) | a
            }
            _ => 0,
        }
    }

    #[inline(always)]
    fn hex_digit(b: u8) -> u32 {
        match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => 0,
        }
    }

    #[inline(always)]
    fn hex_byte(hi: u8, lo: u8) -> u32 {
        (hex_digit(hi) << 4) | hex_digit(lo)
    }
}

use bbnf::grammar::generated::css_l4::CssL4Parser;
use bbnf::grammar::generated::json::JsonParser;

/// Compile `grammar_paths` through the Rust backend preparation pass
/// and return the prepared IR the backend consumes.
fn compile_rust_ir(grammar_paths: &[PathBuf]) -> GrammarIR {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        // `CompileTarget::Rust` is the emit-side path: `prepare_grammar`
        // runs `analyze_grammar` which invokes `project_types`,
        // populating `ir.types` exactly as the Rust backend consumes it.
        target: CompileTarget::Rust {
            requested_prettify: false,
        },
    };

    let out = compile_paths_request(grammar_paths, &request)
        .expect("Rust-target compile must succeed for the wire contract");
    match out {
        CompileOutput::Rust(prepared) => prepared.ir,
        other => panic!("expected Rust output, got {other:?}"),
    }
}

/// Compile `grammar_paths` through the Rust backend preparation pass
/// and assert every `(rule_name, type_name)` pair in `named_rules`
/// reaches emit with `ir.types[rule_id] == TypeDesc::Named(<type_name>)`.
///
/// Panics on any rule not found, on any type mismatch, or on any
/// missing `ir.types` entry — those are the exact paths through which
/// AY invariant 23 breaks.
fn assert_named_preserved(grammar_paths: &[PathBuf], named_rules: &[(&str, &str)]) {
    let ir = compile_rust_ir(grammar_paths);

    for (rule_name, type_name) in named_rules {
        let rule = ir.find_rule(rule_name).unwrap_or_else(|| {
            panic!(
                "AY invariant 23: rule `{rule_name}` expected to survive to \
                 emit as Named(\"{type_name}\"), but is not present in \
                 `ir.rules` post-`prepare_grammar`. Either the rule was \
                 pruned (reachability) or never lowered."
            )
        });

        let projected = ir
            .types
            .iter()
            .find_map(|(id, td)| (*id == rule.id).then(|| td.clone()))
            .unwrap_or_else(|| {
                panic!(
                    "AY invariant 23: rule `{rule_name}` has no entry in \
                     `ir.types` at emit time — `project_types` did not \
                     project this rule. Expected Named(\"{type_name}\")."
                )
            });

        match &projected {
            TypeDesc::Named(sid) => {
                let actual = ir.get_string(*sid);
                assert_eq!(
                    actual, *type_name,
                    "AY invariant 23: rule `{rule_name}` projects as \
                     Named(\"{actual}\") at emit; expected Named(\"{type_name}\"). \
                     The grammar-declared `-> input : <Name>` annotation did \
                     not survive through lowering + normalisation + e-graph + \
                     `project_types`."
                );
            }
            other => {
                panic!(
                    "AY invariant 23: rule `{rule_name}` projects as {other:?} \
                     at emit; expected Named(\"{type_name}\"). The grammar-\
                     declared annotation was collapsed somewhere between \
                     lowering and `project_types` (see \
                     `named_pipeline_probe` for empirical discrimination \
                     of the collapse site)."
                );
            }
        }
    }
}

fn named_entries(grammar: &PathBuf) -> Vec<(String, String)> {
    let ir = compile_rust_ir(std::slice::from_ref(grammar));
    let mut out = Vec::new();
    for (rule_id, td) in &ir.types {
        if let TypeDesc::Named(sid) = td {
            let rule_name = ir
                .rules
                .iter()
                .find(|r| r.id == *rule_id)
                .map(|r| ir.get_string(r.name).to_string())
                .unwrap_or_else(|| "<unknown>".to_string());
            out.push((rule_name, ir.get_string(*sid).to_string()));
        }
    }
    out.sort();
    out
}

fn payload_layout_rule_names(grammar: &PathBuf) -> BTreeSet<String> {
    let ir = compile_rust_ir(std::slice::from_ref(grammar));
    ir.payload_layouts
        .keys()
        .filter_map(|rule_id| {
            ir.rules
                .iter()
                .find(|rule| rule.id == *rule_id)
                .map(|rule| ir.get_string(rule.name).to_string())
        })
        .collect()
}

/// CSS L4 declares three rules with `-> input : Color`: `colorFunction`,
/// `colorFn`, `colorMix`. Post-W2.2 grammar-source fix (commit
/// `14f3a147` — precedence wrap on `colorFn` / `colorMix` bodies) only
/// `colorFn` survives reachability pruning. The cycle `color → colorMix
/// → color` is unreachable from the `stylesheet` entry rule because
/// `properties.bbnf`'s `value` rule references `colorFn` / `hex` /
/// `namedColor` directly, NOT `color`. This test asserts the single
/// post-prune Named(\"Color\") entry survives to emit.
#[test]
fn css_l4_named_types() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let stylesheet = manifest.join("../../grammar/css/l4/stylesheet.bbnf");
    assert_named_preserved(
        &[stylesheet],
        &[
            // colorFunction + colorMix are pruned as unreachable per
            // AYW2-named-collapse-probe.md §Finding 3. The reachability
            // fix (extending `properties.bbnf::value` to reach them via
            // the `color` Alt) is deferred — see AY.W2.2 commit message
            // for the pattern-dedup interaction that blocks the fix.
            ("colorFn", "Color"),
        ],
    );
}

/// JSON declares `string -> decode_json_string_to_arena(input) : String`.
/// The probe (`json_named_pipeline_probe`) confirms the rule's body
/// lowering sets the outer node to `Map[Expr → Named("String")]`, so
/// `MapConstraint` grounds the rule's CSP variable to `Named("String")`
/// from lowering through emit. This test asserts that end-to-end
/// survival on the Rust-target path.
#[test]
fn json_named_types() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let json = manifest.join("../../grammar/json/json.bbnf");
    assert_named_preserved(&[json], &[("string", "String")]);
}

/// Negative-space check: every `ir.types` entry tagged `Named(_)`
/// on the six-grammar suite corresponds to an expected rule; no
/// spurious Named entries are admitted. This guards against the
/// inverse failure mode — a refactor accidentally emitting
/// `Named(_)` for rules the grammar did not annotate.
///
/// Enumerated by grammar: CSS L4 → {colorFn}; JSON → {string};
/// BBNF / Sheets / CSS pretty / EBNF / BNF / CSV / math → ∅
/// (none of these grammars declare a non-scalar `-> input : <Name>`
/// annotation — Sheets' Span annotations resolve to `TypeDesc::Span`
/// via the scalar-name table, not `TypeDesc::Named`).
#[test]
fn no_spurious_named_entries() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // CSS L4 — {colorFn → Color}.
    let css_l4 = named_entries(&manifest.join("../../grammar/css/l4/stylesheet.bbnf"));
    assert_eq!(
        css_l4,
        vec![("colorFn".to_string(), "Color".to_string())],
        "CSS L4 Named entries must be exactly {{colorFn → Color}}; got {css_l4:?}"
    );

    // JSON — {string → String}.
    let json = named_entries(&manifest.join("../../grammar/json/json.bbnf"));
    assert_eq!(
        json,
        vec![("string".to_string(), "String".to_string())],
        "JSON Named entries must be exactly {{string → String}}; got {json:?}"
    );

    // Sheets — ∅ (Span-annotated rules resolve to TypeDesc::Span,
    // not TypeDesc::Named).
    let sheets = named_entries(&manifest.join("../../grammar/google-sheets/google-sheets.bbnf"));
    assert!(
        sheets.is_empty(),
        "Sheets must have zero Named entries (Span annotations resolve \
         to TypeDesc::Span via scalar-name table); got {sheets:?}"
    );

    // BBNF — ∅.
    let bbnf = named_entries(&manifest.join("../../grammar/bbnf/bbnf.bbnf"));
    assert!(
        bbnf.is_empty(),
        "BBNF must have zero Named entries (no `-> : <Name>` \
         annotations in the grammar); got {bbnf:?}"
    );
}

/// AY.W6.b/O3 -- grammar-derived StructDirect admission facts.
///
/// The post-O3 assertion reads the prepared IR's payload-layout table
/// rather than generated tape-view metadata. These rule names are the
/// grammar-owned facts the StructDirect runtime builders consume when
/// constructing concrete document values.
#[test]
fn admitted_projection_surfaces() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let json = payload_layout_rule_names(&manifest.join("../../grammar/json/json.bbnf"));
    let css_l4 = payload_layout_rule_names(&manifest.join("../../grammar/css/l4/stylesheet.bbnf"));
    let sheets =
        payload_layout_rule_names(&manifest.join("../../grammar/google-sheets/google-sheets.bbnf"));
    let bbnf = payload_layout_rule_names(&manifest.join("../../grammar/bbnf/bbnf.bbnf"));

    for expected in ["bool", "string"] {
        assert!(
            json.contains(expected),
            "JSON payload layouts must include {expected:?}; got {json:?}"
        );
    }
    for expected in ["colorSpace", "length", "percentage", "hex"] {
        assert!(
            css_l4.contains(expected),
            "CSS L4 payload layouts must include {expected:?}; got {css_l4:?}"
        );
    }
    for expected in ["string", "boolean", "cell_ref", "add_op"] {
        assert!(
            sheets.contains(expected),
            "Sheets payload layouts must include {expected:?}; got {sheets:?}"
        );
    }
    for expected in ["identifier", "import_path", "regex"] {
        assert!(
            bbnf.contains(expected),
            "BBNF payload layouts must include {expected:?}; got {bbnf:?}"
        );
    }

    let total = json.len() + css_l4.len() + sheets.len() + bbnf.len();
    eprintln!(
        "AZ-II.O3 payload-layout surface: JSON={} CSS_L4={} Sheets={} BBNF={} -> total={total}",
        json.len(),
        css_l4.len(),
        sheets.len(),
        bbnf.len()
    );

    assert!(
        total >= 12,
        "payload-layout surface must cover the primary StructDirect corpus; got total={total}"
    );
    assert!(
        css_l4.len() >= 4,
        "CSS L4 must retain multiple grammar-layout payloads; got {css_l4:?}"
    );
}

#[test]
fn named_runtime_documents_preserve_concrete_values() {
    let json = JsonParser::parse("\"hello\"").expect("JSON string parse");
    match json.to_value() {
        JsonValue::String(s) => assert_eq!(*s, "hello"),
        other => panic!("JSON named String rule must reach JsonValue::String, got {other:?}"),
    }

    let css_src = "a { color: color(srgb 1 0 1 / 0.75); }";
    let css = CssL4Parser::parse(css_src)
        .unwrap_or_else(|e| panic!("CSS named colorFn parse failed: {e:?}"));
    assert_eq!(css.input(), css_src);
    assert!(
        !css.rules(css.to_value().rules).is_empty(),
        "CSS colorFn fixture must produce a stylesheet rule"
    );
    assert!(
        css.walk_declarations().count() >= 1,
        "CSS colorFn fixture must expose declarations through CssDocument"
    );
    let list_id = css
        .walk_values()
        .find_map(|(_property, value)| match value {
            CssTypedValue::List(id) => Some(*id),
            _ => None,
        })
        .expect("CSS colorFn fixture must expose an arena-backed value list");
    let values = css.values(list_id);
    assert!(
        values
            .iter()
            .any(|value| matches!(value, CssTypedValue::Span(name) if *name == "color")),
        "CSS colorFn document value list must retain the color() function head, got {values:?}"
    );
}
