//! AY-II.W0.d — projection totality wire-contract.
//!
//! The AY-II architectural invariant 7 (`docs/tranches/AY-II/AY-II.md`)
//! states:
//!
//! > Projection totality: `PROJECTION_DIRECT_TO_STRUCT.len() == count
//! > of materialize_projection_* fns == count of production consumers`
//! > per grammar and in aggregate.
//!
//! AUDIT-B §4 measured the pre-AY-II state at 71 admissions / 69
//! materializers / 2 resolver shims — a 2-entry gap that AUDIT-C §1
//! attributed to `value_materialize.rs::emit_projection_fns` mirroring
//! the layout arm only while `grammar.rs::collect_projection_admissions`
//! also admitted the resolver-backed arm. AY-II.W0.d unifies the two
//! walks behind a single `collect_projection_admissions` helper and
//! emits a runnable materializer + a production consumer for every
//! admission.
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
//!
//! A regression that silently drops either the materializer or the
//! consumer for any admission surfaces as a slice-length mismatch here.

mod common;

use bbnf_derive::Parser;

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

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonG;

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4G;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", skip_recover)]
struct SheetsG;

#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf")]
struct BbnfG;

/// Reflect over a grammar's three parallel projection slices and
/// return a report of any invariant violations. Empty vec = totality
/// holds.
///
/// `label` is the grammar-type-name (`"JsonG"`, `"CssL4G"`, etc.) used
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
        "JsonG",
        JsonG::PROJECTION_DIRECT_TO_STRUCT,
        JsonG::PROJECTION_MATERIALIZERS,
        JsonG::PROJECTION_CONSUMERS,
    );
    let css_errs = check_projection_totality(
        "CssL4G",
        CssL4G::PROJECTION_DIRECT_TO_STRUCT,
        CssL4G::PROJECTION_MATERIALIZERS,
        CssL4G::PROJECTION_CONSUMERS,
    );
    let sheets_errs = check_projection_totality(
        "SheetsG",
        SheetsG::PROJECTION_DIRECT_TO_STRUCT,
        SheetsG::PROJECTION_MATERIALIZERS,
        SheetsG::PROJECTION_CONSUMERS,
    );
    let bbnf_errs = check_projection_totality(
        "BbnfG",
        BbnfG::PROJECTION_DIRECT_TO_STRUCT,
        BbnfG::PROJECTION_MATERIALIZERS,
        BbnfG::PROJECTION_CONSUMERS,
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
    let json_n = JsonG::PROJECTION_DIRECT_TO_STRUCT.len();
    let css_n = CssL4G::PROJECTION_DIRECT_TO_STRUCT.len();
    let sheets_n = SheetsG::PROJECTION_DIRECT_TO_STRUCT.len();
    let bbnf_n = BbnfG::PROJECTION_DIRECT_TO_STRUCT.len();
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
    let total = JsonG::PROJECTION_DIRECT_TO_STRUCT.len()
        + CssL4G::PROJECTION_DIRECT_TO_STRUCT.len()
        + SheetsG::PROJECTION_DIRECT_TO_STRUCT.len()
        + BbnfG::PROJECTION_DIRECT_TO_STRUCT.len();

    assert!(
        total >= 71,
        "AY-II.W0.d aggregate admission count must be >= 71 (AUDIT-B \
         baseline restored at W0.d close); got {total} across \
         {{JsonG, CssL4G, SheetsG, BbnfG}}"
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
    let json_string_idx = JsonG::PROJECTION_DIRECT_TO_STRUCT
        .iter()
        .position(|(rule, _)| *rule == "string")
        .expect(
            "JsonG: `string` rule must admit as direct-to-struct \
             projection (resolver-backed Named(\"String\") admission)",
        );
    assert_eq!(
        JsonG::PROJECTION_NAMED_BINDINGS[json_string_idx], "String",
        "JsonG[{json_string_idx}].string: named binding must be \"String\"",
    );

    // CSS L4 `colorFn` → `Color` binding.
    let css_colorfn_idx = CssL4G::PROJECTION_DIRECT_TO_STRUCT
        .iter()
        .position(|(rule, _)| *rule == "colorFn")
        .expect(
            "CssL4G: `colorFn` rule must admit as direct-to-struct \
             projection (resolver-backed Named(\"Color\") admission)",
        );
    assert_eq!(
        CssL4G::PROJECTION_NAMED_BINDINGS[css_colorfn_idx], "Color",
        "CssL4G[{css_colorfn_idx}].colorFn: named binding must be \"Color\"",
    );
}
