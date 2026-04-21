//! AY.W2.7 + AY.W6.b — Named-type preservation + grammar-derived
//! direct-to-struct admission wire-contract test.
//!
//! Per `docs/tranches/AY/waves/W2.md` §AY.W2.7 + AY invariant 23:
//! every grammar-declared `-> input : <Name>` annotation (non-scalar,
//! per the scalar-name table in `TypeDesc::from_scalar_name`) must
//! reach the Rust tape emitter as `TypeDesc::Named(sid)`. The invariant
//! is end-to-end: the projection survives lowering, the normaliser
//! loop, the e-graph saturation block, reachability pruning, and the
//! Rust-backend preparation pass (`analyze_grammar → project_types`).
//!
//! The assertion is at emit-side — `compile_paths_request` with
//! `CompileTarget::Rust` exposes the exact `ir.types` array the
//! `emit_direct_to_struct_projection` emitter walks when building the
//! `PROJECTION_DIRECT_TO_STRUCT` const + `__grammar_projection_<rule>`
//! marker functions. Checking IR-only (via `CompileTarget::Vm`) would
//! admit Named entries the Rust-target preparation later drops;
//! checking emit-only would not surface which rule loses Named.
//! This test checks both boundaries. Post-AY-II.W0'.b every admission
//! emits a runnable `materialize_projection_<rule>_<Grammar>` helper
//! AND a fused-pipeline consumer call site inside
//! `project_value_<Grammar>`; the structural + runtime wire-contract
//! lives in `tests/projection_totality.rs`.
//!
//! ## AY.W6.b coverage — grammar-derived direct-to-struct admission
//!
//! Tranche AY.W6.b broadened the emitter's direct-to-struct admission
//! to consume `ir.payload_layouts` as a grammar-derived fact, not
//! just `TypeDesc::Named(sid)`. The admitted surface is now a per-
//! grammar `pub struct <Grammar><RuleCamel>Projection` emitted
//! alongside the legacy `PROJECTION_DIRECT_TO_STRUCT` const entries.
//!
//! The `admitted_projection_surfaces` test below asserts at least
//! four admitted direct-to-struct surfaces across the corpus via
//! `#[derive(Parser)]` + the emitted `PROJECTION_DIRECT_TO_STRUCT`
//! const — which is the same `cargo expand` surface the AY.W6.2
//! hard gate inspects. Surface-by-surface the derives below prove
//! that grammar-layout admissions emit concrete structs without
//! name dispatch.
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

use std::path::PathBuf;

use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_derive::Parser;
use bbnf_ir::{GrammarIR, TypeDesc};

// ───────────────────────────────────────────────────────────────────
// AY.W6.b — `#[derive(Parser)]` sites so `cargo expand -p bbnf --test
// named_type_preservation` surfaces the admitted direct-to-struct
// artefacts. Each derive instantiates the full emitter pipeline; the
// resulting `PROJECTION_DIRECT_TO_STRUCT` const + per-grammar
// `<Grammar><RuleCamel>Projection` structs are the structural
// evidence the wave's hard gate inspects.
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

/// JSON grammar — admits the resolver-backed `("string", "String")`
/// entry plus one or more grammar-layout projections (e.g. `bool`).
#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonG;

/// CSS L4 — the richest grammar. Admits `colorFn` as resolver-backed
/// `Color` plus a large suite of grammar-layout projections
/// (`length`, `angle`, `time`, unit rules, prop groups, …).
#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4G;

/// Sheets — admits grammar-layout projections only (no Named
/// annotations survive per the W2 coverage note).
#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", skip_recover)]
struct SheetsG;

/// BBNF — self-hosted; admits grammar-layout projections only.
#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf")]
struct BbnfG;

/// Compile `grammar_paths` through the Rust backend preparation pass
/// (which runs `analyze_grammar → project_types` per
/// `crates/core/src/backend/driver/analysis.rs:136`) and assert every
/// `(rule_name, type_name)` pair in `named_rules` reaches emit with
/// `ir.types[rule_id] == TypeDesc::Named(<type_name>)`.
///
/// Panics on any rule not found, on any type mismatch, or on any
/// missing `ir.types` entry — those are the exact paths through which
/// AY invariant 23 breaks.
fn assert_named_preserved(grammar_paths: &[PathBuf], named_rules: &[(&str, &str)]) {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        // `CompileTarget::Rust` is the emit-side path: `prepare_grammar`
        // runs `analyze_grammar` which invokes `project_types`,
        // populating `ir.types` exactly as the Rust emitter consumes
        // it at `emit_direct_to_struct_projection`.
        target: CompileTarget::Rust { requested_prettify: false },
    };

    let out = compile_paths_request(grammar_paths, &request)
        .expect("Rust-target compile must succeed for the wire contract");
    let prepared = match out {
        CompileOutput::Rust(prepared) => prepared,
        other => panic!("expected Rust output, got {other:?}"),
    };
    let ir: &GrammarIR = &prepared.ir;

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
    assert_named_preserved(&[stylesheet], &[
        // colorFunction + colorMix are pruned as unreachable per
        // AYW2-named-collapse-probe.md §Finding 3. The reachability
        // fix (extending `properties.bbnf::value` to reach them via
        // the `color` Alt) is deferred — see AY.W2.2 commit message
        // for the pattern-dedup interaction that blocks the fix.
        ("colorFn", "Color"),
    ]);
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
    assert_named_preserved(&[json], &[
        ("string", "String"),
    ]);
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
    fn count_named(grammar: &PathBuf) -> Vec<(String, String)> {
        let request = CompileRequest {
            options: PipelineOptions::default(),
            target: CompileTarget::Rust { requested_prettify: false },
        };
        let out = compile_paths_request(std::slice::from_ref(grammar), &request)
            .expect("compile must succeed");
        let prepared = match out {
            CompileOutput::Rust(p) => p,
            other => panic!("expected Rust output, got {other:?}"),
        };
        let ir = &prepared.ir;
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

    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // CSS L4 — {colorFn → Color}.
    let css_l4 = count_named(&manifest.join("../../grammar/css/l4/stylesheet.bbnf"));
    assert_eq!(
        css_l4,
        vec![("colorFn".to_string(), "Color".to_string())],
        "CSS L4 Named entries must be exactly {{colorFn → Color}}; got {css_l4:?}"
    );

    // JSON — {string → String}.
    let json = count_named(&manifest.join("../../grammar/json/json.bbnf"));
    assert_eq!(
        json,
        vec![("string".to_string(), "String".to_string())],
        "JSON Named entries must be exactly {{string → String}}; got {json:?}"
    );

    // Sheets — ∅ (Span-annotated rules resolve to TypeDesc::Span,
    // not TypeDesc::Named).
    let sheets =
        count_named(&manifest.join("../../grammar/google-sheets/google-sheets.bbnf"));
    assert!(
        sheets.is_empty(),
        "Sheets must have zero Named entries (Span annotations resolve \
         to TypeDesc::Span via scalar-name table); got {sheets:?}"
    );

    // BBNF — ∅.
    let bbnf = count_named(&manifest.join("../../grammar/bbnf/bbnf.bbnf"));
    assert!(
        bbnf.is_empty(),
        "BBNF must have zero Named entries (no `-> : <Name>` \
         annotations in the grammar); got {bbnf:?}"
    );
}

/// AY.W6.b — grammar-derived direct-to-struct admission surface.
///
/// The wave broadened `emit_direct_to_struct_projection` from the
/// narrow `TypeDesc::Named(sid)` admission arm to the full
/// `ir.payload_layouts` surface, so every non-transparent rule whose
/// child sequence projects onto a fixed-layout scalar tuple admits
/// direct-to-struct storage. This test asserts the admitted count
/// across the four `#[derive(Parser)]` grammars above crosses the
/// AY.W6.b floor of four admissions — the wave's deliverable vs
/// AY.W2's miss.
///
/// The assertion reads the derive-emitted `PROJECTION_DIRECT_TO_STRUCT`
/// const directly via the proc-macro-produced module namespace. Each
/// grammar's const is a `&[(&str, &str); N]` of admitted projections.
/// Cross-grammar admissions count additively; `N` per grammar is the
/// per-grammar admission count (CSS L4 alone clears the floor with
/// its unit + prop-group rules, but the test asserts the aggregate
/// so partial regressions in any grammar surface at this gate).
#[test]
fn admitted_projection_surfaces() {
    // Per-grammar admission counts read via the grammar's associated
    // constant. Each `<Grammar>::PROJECTION_DIRECT_TO_STRUCT` alias
    // resolves to the per-grammar emitted slice; the alias is a
    // namespace-safe way to disambiguate across four grammars coexisting
    // in one test binary (the module-scope `pub use __<grammar>::*;`
    // glob would otherwise collide on the unqualified name).
    let json_total = JsonG::PROJECTION_DIRECT_TO_STRUCT.len();
    let css_l4_total = CssL4G::PROJECTION_DIRECT_TO_STRUCT.len();
    let sheets_total = SheetsG::PROJECTION_DIRECT_TO_STRUCT.len();
    let bbnf_total = BbnfG::PROJECTION_DIRECT_TO_STRUCT.len();

    let total = json_total + css_l4_total + sheets_total + bbnf_total;
    eprintln!(
        "AY-II.W0.d admission surface: JSON={json_total} CSS_L4={css_l4_total} \
         Sheets={sheets_total} BBNF={bbnf_total} → total={total}"
    );

    // AY.W6.b floor: at least four admitted direct-to-struct surfaces
    // across the corpus. The pre-W6.b baseline was two (CSS L4 `colorFn`
    // + JSON `string`), both resolver-backed `Named(_)` admissions.
    // Grammar-layout admissions add every rule whose scalar-tuple
    // projection succeeded; a regression that silently drops the
    // layout-driven arm fails this gate even if the resolver-backed
    // arm still fires.
    assert!(
        total >= 4,
        "AY.W6.b: admitted direct-to-struct surfaces must be >= 4 \
         (wave-declared floor); got total={total} \
         (JSON={json_total}, CSS_L4={css_l4_total}, \
         Sheets={sheets_total}, BBNF={bbnf_total})"
    );

    // AY.W6.b structural assertion: CSS L4 must admit at least four
    // direct-to-struct surfaces on its own. Its unit + prop-group +
    // keyword rules are the richest grammar-layout source in the corpus,
    // so a regression that drops the layout-driven arm surfaces here
    // even before the aggregate floor fires.
    assert!(
        css_l4_total >= 4,
        "AY.W6.b: CSS L4 must admit at least four direct-to-struct \
         surfaces (unit + prop-group rules from the layout pass); \
         got {css_l4_total}"
    );

    // AY-II.W0'.b totality invariant — per grammar:
    // `PROJECTION_DIRECT_TO_STRUCT.len() ==
    //  PROJECTION_MATERIALIZERS.len() ==
    //  PROJECTION_CONSUMERS.len()`.
    // The canonical gate (structural + runtime-call-count) lives in
    // `tests/projection_totality.rs`; mirroring the structural
    // assertion here keeps this test's name-type preservation story
    // colocated with the admission-count story. Post-W0'.b every
    // admission's consumer is a materializer-driven projection
    // variant (not `Unknown` fallback); the consumer-side evidence
    // is `PROJECTION_CONSUMERS[i]` naming the per-grammar enum
    // variant that wraps the projection struct.
    for (label, admissions, materializers, consumers) in [
        (
            "JsonG",
            JsonG::PROJECTION_DIRECT_TO_STRUCT,
            JsonG::PROJECTION_MATERIALIZERS,
            JsonG::PROJECTION_CONSUMERS,
        ),
        (
            "CssL4G",
            CssL4G::PROJECTION_DIRECT_TO_STRUCT,
            CssL4G::PROJECTION_MATERIALIZERS,
            CssL4G::PROJECTION_CONSUMERS,
        ),
        (
            "SheetsG",
            SheetsG::PROJECTION_DIRECT_TO_STRUCT,
            SheetsG::PROJECTION_MATERIALIZERS,
            SheetsG::PROJECTION_CONSUMERS,
        ),
        (
            "BbnfG",
            BbnfG::PROJECTION_DIRECT_TO_STRUCT,
            BbnfG::PROJECTION_MATERIALIZERS,
            BbnfG::PROJECTION_CONSUMERS,
        ),
    ] {
        assert_eq!(
            admissions.len(),
            materializers.len(),
            "{label}: AY-II invariant 7 — admission count ({}) must equal \
             materializer count ({})",
            admissions.len(),
            materializers.len(),
        );
        assert_eq!(
            admissions.len(),
            consumers.len(),
            "{label}: AY-II invariant 7 — admission count ({}) must equal \
             consumer count ({})",
            admissions.len(),
            consumers.len(),
        );
    }
}
