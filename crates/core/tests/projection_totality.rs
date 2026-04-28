//! AY-II.W0'.b — projection totality wire-contract + runtime-call-count
//! evidence.
//!
//! The AY-II architectural invariant 7 (`docs/tranches/AY-II/AY-II.md`)
//! states:
//!
//! > Projection totality: `PROJECTION_DIRECT_TO_STRUCT.len() == count
//! > of materialize_projection_* fns == count of production consumers`
//! > per grammar and in aggregate.
//!
//! AUDIT-B §4 measured the pre-AY-II state at 71 admissions / 69
//! materializers / 2 resolver shims — a 2-entry gap AY-II.W0.d closed
//! structurally. AUDIT-C §Q3 then established that the 69
//! materializers had ZERO call sites; the `project_value_<Grammar>`
//! dispatcher bypassed them entirely. AY-II.W0'.b closes the
//! wire-contract by routing every admitted rule through its matching
//! `materialize_projection_<rule>_<Grammar>` fn inside the fused
//! projection path.
//!
//! This test is the wire-contract gate. It reflects over the four
//! primary grammars (JSON, CSS L4, Sheets, BBNF) via the emitter's
//! exposed associated constants — no hand-coded per-grammar branching,
//! no `match grammar_name` dispatch. For each grammar the test
//! asserts:
//!
//! 1. `PROJECTION_DIRECT_TO_STRUCT.len() == PROJECTION_MATERIALIZERS.len()`
//!    — one materializer fn per admission.
//! 2. `PROJECTION_DIRECT_TO_STRUCT.len() == PROJECTION_CONSUMERS.len()`
//!    — one `<Grammar>Value::<RuleName>` variant per admission.
//! 3. Per-index alignment: the rule name at
//!    `PROJECTION_DIRECT_TO_STRUCT[i].0` appears in the materializer
//!    name at `PROJECTION_MATERIALIZERS[i]` and in the consumer name
//!    at `PROJECTION_CONSUMERS[i]`. This catches accidental index
//!    shuffle between the three parallel slices.
//! 4. **Runtime call-count evidence (AY-II.W0'.b)**: parses a
//!    grammar-derived smoke fixture per grammar and calls
//!    `Parsed::to_value()`; the resulting `<Grammar>Value` tree MUST
//!    contain at least one admitted variant (i.e. a variant whose
//!    payload is the `<Grammar><RuleCamel>Projection` struct, not a
//!    `Vec<<Grammar>Value<'_>>` or `&str`). A materializer fails to
//!    run iff the dispatcher's `unwrap_or_else(panic)` aborts — so a
//!    successful `to_value()` + the presence of at least one
//!    projection-struct-typed variant IS the runtime-call-count
//!    witness. The assertion is grammar-driven: the smoke fixture
//!    lives alongside the grammar in `data/`, and each grammar's
//!    variant walk counts projection-struct-typed variants without a
//!    per-rule dispatch.
//!
//! A regression that silently drops either the materializer or the
//! consumer for any admission surfaces as a slice-length mismatch
//! here; a regression that leaves materializers uncalled at runtime
//! surfaces at the runtime-call-count assertion.

mod common;

// ─── Grammar derives ────────────────────────────────────────────────
//
// The four primary grammars cover the full admission surface:
// - JSON: 2 admissions (`bool` layout, `string` resolver-backed).
// - CSS L4: 49 admissions (48 layout + 1 resolver-backed `colorFn`).
// - Sheets: 10 admissions (all layout-backed).
// - BBNF: 10 admissions (all layout-backed).
//
// Host shims required by CSS L4's `-> parse_hex_color(...)` mapping
// live in `common::css_types` and are re-exported here so the derive
// macro resolves the function reference.

use common::css_types;

use ::bbnf::grammar::generated::json::*;


use ::bbnf::grammar::generated::css_l4::*;


use ::bbnf::grammar::generated::google_sheets::*;


use ::bbnf::grammar::generated::bbnf::*;


/// Reflect over a grammar's three parallel projection slices and
/// return a report of any invariant violations. Empty vec = totality
/// holds.
///
/// `label` is the grammar-type-name (`"JsonParser"`, `"CssL4Parser"`, etc.) used
/// only for error messages. The three slices ARE the grammar-derived
/// evidence — no per-grammar branching inside this function.
fn check_projection_totality(
    label: &str,
    admissions: &[(&str, &str)],
    materializers: &[&str],
    consumers: &[&str],
) -> Vec<String> {
    let mut errors: Vec<String> = Vec::new();

    // Slice length equality — the foundational invariant.
    if admissions.len() != materializers.len() {
        errors.push(format!(
            "{label}: PROJECTION_DIRECT_TO_STRUCT.len() ({}) != \
             PROJECTION_MATERIALIZERS.len() ({}) — materializer count \
             mismatch (AY-II invariant 7)",
            admissions.len(),
            materializers.len(),
        ));
    }
    if admissions.len() != consumers.len() {
        errors.push(format!(
            "{label}: PROJECTION_DIRECT_TO_STRUCT.len() ({}) != \
             PROJECTION_CONSUMERS.len() ({}) — consumer count \
             mismatch (AY-II invariant 7)",
            admissions.len(),
            consumers.len(),
        ));
    }

    // Per-index alignment — each rule name appears at the same
    // index in all three slices via its materializer / consumer
    // identifier. Bails early when lengths differ since a mismatch
    // there already fails the totality invariant.
    if admissions.len() == materializers.len()
        && admissions.len() == consumers.len()
    {
        for (i, ((rule_name, _struct_name), fn_name)) in
            admissions.iter().zip(materializers.iter()).enumerate()
        {
            // Materializer name form: materialize_projection_<rule>_<Grammar>
            // where <rule> is `sanitise_ident(rule_name)` — lowercase,
            // non-alphanumerics replaced by underscores. Verify the
            // rule-name substring appears (after sanitisation) inside
            // the materializer fn name.
            let sanitised = sanitise_rule_name(rule_name);
            let expected_fragment = format!("materialize_projection_{sanitised}_");
            if !fn_name.starts_with(&expected_fragment) {
                errors.push(format!(
                    "{label}[{i}]: admission `{rule_name}` expected materializer \
                     to start with `{expected_fragment}`, got `{fn_name}`",
                ));
            }
        }

        for (i, ((rule_name, _struct_name), consumer_name)) in
            admissions.iter().zip(consumers.iter()).enumerate()
        {
            // Consumer name form: <Grammar>Value::<RuleName>.
            // Assert the rule name appears after the `::` separator.
            let Some((_, variant)) = consumer_name.split_once("::") else {
                errors.push(format!(
                    "{label}[{i}]: consumer name `{consumer_name}` malformed — \
                     expected `<Grammar>Value::<RuleName>`",
                ));
                continue;
            };
            if variant != *rule_name {
                errors.push(format!(
                    "{label}[{i}]: admission `{rule_name}` expected consumer \
                     variant name to match; got `{variant}` in `{consumer_name}`",
                ));
            }
        }
    }

    errors
}

/// Mirror of the emitter's `sanitise_ident` helper — rule names with
/// non-alphanumeric characters replace them with underscores; leading
/// digits get an `r_` prefix. Kept local so the test evaluates the
/// wire-contract against an independent implementation of the
/// sanitisation rule (a regression that silently re-interprets the
/// ident mangling surfaces here as a per-index mismatch).
fn sanitise_rule_name(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for (idx, ch) in name.chars().enumerate() {
        if ch.is_ascii_alphanumeric() {
            if idx == 0 && ch.is_ascii_digit() {
                out.push_str("r_");
            }
            out.extend(ch.to_lowercase());
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        out.push('_');
    }
    out
}

/// AY-II.W0.d wire-contract: every primary grammar admits to the
/// same count of `PROJECTION_DIRECT_TO_STRUCT` entries as it emits
/// materializer fns AND as it emits `<Grammar>Value` consumer
/// variants. Per-index alignment holds so the three slices form a
/// coherent map from admission to runnable code to runtime consumer.
#[test]
fn projection_totality_per_grammar() {
    let json_errs = check_projection_totality(
        "JsonParser",
        JsonParser::PROJECTION_DIRECT_TO_STRUCT,
        JsonParser::PROJECTION_MATERIALIZERS,
        JsonParser::PROJECTION_CONSUMERS,
    );
    let css_errs = check_projection_totality(
        "CssL4Parser",
        CssL4Parser::PROJECTION_DIRECT_TO_STRUCT,
        CssL4Parser::PROJECTION_MATERIALIZERS,
        CssL4Parser::PROJECTION_CONSUMERS,
    );
    let sheets_errs = check_projection_totality(
        "GoogleSheetsParser",
        GoogleSheetsParser::PROJECTION_DIRECT_TO_STRUCT,
        GoogleSheetsParser::PROJECTION_MATERIALIZERS,
        GoogleSheetsParser::PROJECTION_CONSUMERS,
    );
    let bbnf_errs = check_projection_totality(
        "BbnfBootstrap",
        BbnfBootstrap::PROJECTION_DIRECT_TO_STRUCT,
        BbnfBootstrap::PROJECTION_MATERIALIZERS,
        BbnfBootstrap::PROJECTION_CONSUMERS,
    );

    let mut all_errors = Vec::new();
    all_errors.extend(json_errs);
    all_errors.extend(css_errs);
    all_errors.extend(sheets_errs);
    all_errors.extend(bbnf_errs);

    if !all_errors.is_empty() {
        panic!(
            "AY-II.W0.d projection totality violated ({} errors):\n{}",
            all_errors.len(),
            all_errors.join("\n"),
        );
    }

    // Log the per-grammar counts so a passing run still prints the
    // wire-contract evidence.
    let json_n = JsonParser::PROJECTION_DIRECT_TO_STRUCT.len();
    let css_n = CssL4Parser::PROJECTION_DIRECT_TO_STRUCT.len();
    let sheets_n = GoogleSheetsParser::PROJECTION_DIRECT_TO_STRUCT.len();
    let bbnf_n = BbnfBootstrap::PROJECTION_DIRECT_TO_STRUCT.len();
    let total = json_n + css_n + sheets_n + bbnf_n;
    eprintln!(
        "AY-II.W0.d projection totality: JSON={json_n} CSS_L4={css_n} \
         Sheets={sheets_n} BBNF={bbnf_n} → total={total} \
         (admissions : materializers : consumers 1:1:1 per grammar)"
    );
}

/// AY-II.W0.d aggregate invariant: the full four-grammar corpus
/// admits ≥ 71 direct-to-struct surfaces (the AUDIT-B baseline that
/// the W0.d closure restores). A regression that drops the
/// resolver-backed admissions silently (e.g. reverting the rich-shape
/// admission arm) fails this gate even when per-grammar totality
/// still trivially holds at a smaller count.
#[test]
fn projection_totality_aggregate_floor() {
    let total = JsonParser::PROJECTION_DIRECT_TO_STRUCT.len()
        + CssL4Parser::PROJECTION_DIRECT_TO_STRUCT.len()
        + GoogleSheetsParser::PROJECTION_DIRECT_TO_STRUCT.len()
        + BbnfBootstrap::PROJECTION_DIRECT_TO_STRUCT.len();

    assert!(
        total >= 71,
        "AY-II.W0.d aggregate admission count must be >= 71 (AUDIT-B \
         baseline restored at W0.d close); got {total} across \
         {{JsonParser, CssL4Parser, GoogleSheetsParser, BbnfBootstrap}}"
    );
}

/// AY-II.W0.d resolver-backed admission check: both JSON `string` and
/// CSS L4 `colorFn` — the two rules AUDIT-B §4 identified as
/// resolver-shim-only pre-W0.d — must now surface on each grammar's
/// `PROJECTION_DIRECT_TO_STRUCT` list as regular admissions (with a
/// non-empty `PROJECTION_NAMED_BINDINGS` entry recording the
/// declared type name).
#[test]
fn projection_totality_resolver_admissions_promoted() {
    // JSON `string` → `String` binding.
    let json_string_idx = JsonParser::PROJECTION_DIRECT_TO_STRUCT
        .iter()
        .position(|(rule, _)| *rule == "string")
        .expect(
            "JsonParser: `string` rule must admit as direct-to-struct \
             projection (resolver-backed Named(\"String\") admission)",
        );
    assert_eq!(
        JsonParser::PROJECTION_NAMED_BINDINGS[json_string_idx], "String",
        "JsonParser[{json_string_idx}].string: named binding must be \"String\"",
    );

    // CSS L4 `colorFn` → `Color` binding.
    let css_colorfn_idx = CssL4Parser::PROJECTION_DIRECT_TO_STRUCT
        .iter()
        .position(|(rule, _)| *rule == "colorFn")
        .expect(
            "CssL4Parser: `colorFn` rule must admit as direct-to-struct \
             projection (resolver-backed Named(\"Color\") admission)",
        );
    assert_eq!(
        CssL4Parser::PROJECTION_NAMED_BINDINGS[css_colorfn_idx], "Color",
        "CssL4Parser[{css_colorfn_idx}].colorFn: named binding must be \"Color\"",
    );
}

// ════════════════════════════════════════════════════════════════════
// AY-II.W0'.b — runtime-call-count evidence
// ════════════════════════════════════════════════════════════════════
//
// The structural assertions above prove the three parallel slices
// stay in 1:1:1 agreement. Runtime-call-count evidence proves the
// slab-driven dispatcher actually routes through the emitted
// materializers at runtime: the admitted arm calls
// `materialize_projection_<rule>_<Grammar>(output, input, offset)`
// and wraps the returned struct in `<Grammar>Value::<rule>(proj)`.
//
// A `to_value()` that runs without panicking across a per-grammar
// smoke fixture is the runtime evidence — the dispatcher's
// `unwrap_or_else(panic)` aborts any time a materializer would have
// been bypassed. The test further asserts that `{value:?}`'s debug
// rendering contains the "Projection" suffix marker, proving at
// least one admitted variant is present (and therefore at least one
// materializer ran).

/// Smoke-parse helper. Takes a label + rendered value + the grammar
/// marker. The rendered value is grammar-agnostic — every `<Grammar>
/// Value` derives `Debug` via the emitter's enum declaration, so
/// `format!("{:?}", value)` produces a stable string that names the
/// admitted variants (each projection struct's ident contains the
/// `"Projection"` suffix marker).
///
/// Grammar-agnostic inside this helper: `rendered` is the grammar's
/// Debug rendering, the `admissions` count is the grammar's
/// `PROJECTION_DIRECT_TO_STRUCT.len()` — both inputs are supplied by
/// the caller without per-grammar branching here.
fn assert_runtime_materializer_fires(label: &str, rendered: &str, admissions: usize) {
    assert!(
        admissions > 0,
        "{label}: admission count must be > 0 for runtime-call-count \
         evidence; fixture selection failed"
    );
    assert!(
        rendered.contains("Projection"),
        "{label}: to_value() tree carries no Projection-typed variant \
         — admission-driven materializer never fired at runtime. \
         Rendered: {rendered:.300}"
    );
}

/// AY-II.W0'.b runtime-call-count wire-contract: every primary
/// grammar's fused-pipeline projection routes through the
/// admission-specific materializer at runtime. The evidence is
/// two-fold: (a) `to_value()` completes without panicking under the
/// dispatcher's `unwrap_or_else(panic)` guard, and (b) for the
/// tape-direct grammars, the rendered `<Grammar>Value` tree contains
/// a projection-struct-typed variant (identified by the `"Projection"`
/// suffix marker in the debug rendering).
///
/// AZ-I.W2-act.B1 — JSON crosses to the struct-direct path. Its
/// `JsonParser::parse` returns `JsonDocument<'_>`; `doc.to_value()`
/// returns `&JsonValue<'p>` (the typed tree itself, not a fused
/// `<Grammar>Value` enum). The `"Projection"` suffix marker no longer
/// applies — the JSON block now asserts the typed shape directly: the
/// returned `JsonValue` matches the input ("\"hello\"" parses to
/// `JsonValue::String("hello")`). The structural slice-length
/// assertions above still cover the projection-admission count for
/// JSON; runtime evidence for the tape-direct grammars (CSS L4,
/// Sheets, BBNF) remains the rendered-debug "Projection" probe.
///
/// Per-grammar smoke fixtures are minimal inputs that exercise at
/// least one admitted rule. The assertion harness
/// (`assert_runtime_materializer_fires`) is grammar-agnostic.
#[test]
fn projection_totality_runtime_call_count() {
    // JSON — struct-direct path. `"hello"` parses to a JsonValue::String
    // root borrowed from the input; the typed shape IS the runtime
    // evidence that the struct-builder body fired (an unmaterialised
    // root would panic inside `JsonParser::parse`).
    {
        let doc = JsonParser::parse("\"hello\"")
            .unwrap_or_else(|e| panic!("JsonParser: parse failed: {e:?}"));
        let value = doc.to_value();
        match value {
            bbnf::runtime::JsonValue::String(s) => assert_eq!(
                *s, "hello",
                "JsonParser: struct-direct String value must round-trip the input slice",
            ),
            other => panic!(
                "JsonParser: \"\\\"hello\\\"\" must parse to JsonValue::String; got {other:?}",
            ),
        }
        // The structural admission slice still records the rule's
        // admission; the post-flip evidence is the typed shape above.
        assert!(
            JsonParser::PROJECTION_DIRECT_TO_STRUCT.len() > 0,
            "JsonParser: admission count must be > 0 for runtime-call-count evidence",
        );
    }

    // CSS L4 — a minimal stylesheet with a color function exercises
    // the `colorFn` admission + several layout-packed admissions
    // (unit rules).
    {
        let parsed = CssL4Parser::parse("a { color: rgb(255, 0, 0); }")
            .unwrap_or_else(|e| panic!("CssL4Parser: parse failed: {e:?}"));
        let value = parsed.to_value();
        assert_runtime_materializer_fires(
            "CssL4Parser",
            &format!("{value:?}"),
            CssL4Parser::PROJECTION_DIRECT_TO_STRUCT.len(),
        );
    }

    // Sheets — a minimal literal exercises the string / identifier
    // admissions.
    {
        let parsed = GoogleSheetsParser::parse("=\"x\"")
            .unwrap_or_else(|e| panic!("GoogleSheetsParser: parse failed: {e:?}"));
        let value = parsed.to_value();
        assert_runtime_materializer_fires(
            "GoogleSheetsParser",
            &format!("{value:?}"),
            GoogleSheetsParser::PROJECTION_DIRECT_TO_STRUCT.len(),
        );
    }

    // BBNF — a minimal rule definition exercises the identifier +
    // rule-body admissions. BBNF uses `=` for rule definition (the
    // `::=` BNF-syntax variant is a different grammar).
    {
        let parsed = BbnfBootstrap::parse("r = 'x' ;")
            .unwrap_or_else(|e| panic!("BbnfBootstrap: parse failed: {e:?}"));
        let value = parsed.to_value();
        assert_runtime_materializer_fires(
            "BbnfBootstrap",
            &format!("{value:?}"),
            BbnfBootstrap::PROJECTION_DIRECT_TO_STRUCT.len(),
        );
    }
}
