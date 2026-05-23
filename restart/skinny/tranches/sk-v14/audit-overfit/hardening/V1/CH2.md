# CH2 — GENERALITY

S-P0 CHALLENGE V1 lens disposition (SK-V14 Overfit Audit Pass).

Authority: `restart/prompts/ORCHESTRATOR.md §3W` row CH2 ("Lock 14
holds: no grammar-name leak; every proposed intervention is grammar-
neutral and works for CSS L4 / Sheets / BBNF-self, not only JSON.") +
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CHALLENGE-
CONTEXT.md §3` CH2 row. Artefacts: the seven S-P0 files committed at
`d4cbc8204` per CONTEXT §1.

## §0 — Disposition summary

| Total dispositions | ACCEPT | ACCEPT-with-NOTE | REVISE | REJECT |
| ---: | ---: | ---: | ---: | ---: |
| **8** | **7** | **1** | **0** | **0** |

ACCEPT-rate **8/8 = 100 %** (the ACCEPT-with-NOTE is a clarifying
addendum on A3 H3, not a rejection of the finding; the violation
class — JSON-named identifier in a nominally-generic crate — still
holds, only the cited severity tier needs recalibration in V2).

The audit's GENERALITY posture is sound end-to-end: every CRITICAL
and HIGH finding generalises across CSS L4 / JSON / BBNF / Sheets;
every proposed PRUNE / R-action is grammar-agnostic in its remediation
mechanism (template-of-record, generic registry, xtask regen, generic
config struct, generic union-tape variant); no JSON-only scope creep
slipped into the per-axis findings or the synthesis prune list. The
11 NEW findings extending V13 either *expand* a JSON-only V13 finding
to its CSS L4 / Sheets / BBNF mirror (A4 NEW-1 + NEW-2 + NEW-3, A6
NEW-HIGH-1 + NEW-HIGH-2, A2 F8) or surface a category-level pattern
that V13 had only enumerated per-row (A5 NEW-MED, A6 NEW-MED). Both
trajectories satisfy CH2.

## §1 — Per-artefact disposition

| Artefact | Disposition | Rationale |
| --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | **ACCEPT** | §1.2 NEW-finding enumeration explicitly frames each extension as a *general* pattern (single-lane comparator fan-out covers all 3 JSON planes; fake `@generated` mirrors CSS → JSON; fixture-lookup pattern covers 3 of 7 CSS providers; orphan `.bbnf` covers 14 of 15 files; substrate-doc opt-out spans JSON + CSS L4 + BBNF). §3.1 coverage table maps every finding to a grammar-agnostic C-N candidate. §3.3 sub-wave count correctly enumerates all 9 per-grammar dirs (not 8 — `css_pretty` is the +1), confirming PRUNE-4 sizing is grammar-neutral and complete. |
| `sk-v14-audit-overfit-css-measurement.md` (A1) | **ACCEPT** | Findings target CSS L4 because the row class under audit is `css_l4` ADMITTED rows; the audit-zero criterion (1 KB representative corpus, distinct per-row measurement, per-parse-ns plausibility) is grammar-neutral and would catch any future grammar that tried the same overfit (W1b JSON-direct equivalent; theoretical Sheets equivalent). The CSS-specific instantiation here is a *consequence* of which grammar was admitted, not a scope-leak. §1.4's per-parse formula `elapsed_ns = bytes × 8000 / Mbps` is grammar-agnostic; the R5 production-corpus recommendation specifies `skinny/corpora/css-l4-sk-v14/` because CSS is the grammar under remediation — but the per-corpus floor is the universal criterion. |
| `sk-v14-audit-overfit-admit-mechanism.md` (A2) | **ACCEPT** | F1-F5 cover W14.1-5 (JSON parse_only) because *those are the only post-W7 admits*; the mechanism diagnosis (no parser / codegen / grammar file touched; admit lands by report-schema relabel only) is the grammar-neutral defect class. F6 (comparator misbinding) + F7 (per-iter equality oracle absent) are framed as plane-wide and grammar-agnostic patterns; F8 (NEW) explicitly elevates the per-row symptom to a structural cause covering all 3 JSON planes simultaneously — exactly the generality lens move. F9 negative-confirmation (no post-audit admit) is corpus-agnostic. R1 (per-iter equality oracle) + R2 (strict-vs-strict comparator rebind) are grammar-agnostic in their mechanism. |
| `sk-v14-audit-overfit-lock14-scan.md` (A3) | **ACCEPT-with-NOTE** | The 30 violations cover the Lock 14 surface comprehensively: C1-C2 (runtime root namespace), C3-C4 (RuntimeProvider enum + dispatch), C5-C11 (8 per-grammar provider modules — 7 CSS + 1 JSON), H1-H2 + H7 (JsonGrammar / parse_json_grammar / pub mod json), H3 (CSP entry rule), H4-H5 (template surfaces — JSON + CSS), H6 (CSS L4 entry-rule absence — structural co-finding to H3), M1-M5 (test + config residue), L1-L7 (test-fn naming), D1 (DELTA-NOTE — `parse-that-regex::StringFlags::HAS_ESC` JSON-flavored bit naming flagged as future-rename only, not a violation — correct CH2 disposition). The verified-empty scans on `bbnf-regex` + `bbnf-simd` + `simd-scan` + `parse-that-regex` + `ir` confirm the pure-generic primitive crates remain clean. **NOTE for V2:** A3 cites H3 as `decision_csp.rs:235`, but line 235 is inside a `#[cfg(test)] mod tests { … }` block (test fixture calling `finalize_rule("json", …)`); the *production* call at `passes/src/lib.rs:478` is `finalize_rule(&grammar.name, …)` — generic. The violation still holds at the L6/L7 severity tier (JSON-named identifier in a generic crate's test surface), but the HIGH classification as "CSP solver hardcoded to JSON entry-rule" overstates the production scope. V2 should rewrite H3 as a LOW (test-fixture name leak) and let H6 (CSS L4 absence from the *acceptance test surface*) carry the HIGH bar. Net Lock-14 violation count would drop 11C + 6H + 5M + 8L = 30 (unchanged); only the H3 row migrates from HIGH to LOW. This is a precision adjustment, not a finding withdrawal. |
| `sk-v14-audit-overfit-generator-truth.md` (A4) | **ACCEPT** | The 16 findings span CSS L4 (7 providers, 7 `*_templates/` siblings, 7 runtime twins, 3 fixture-lookup scanners) and JSON (1 provider, 1 template dir, 1 generated.rs partial-mix), with finding 8 explicitly elevating "fake `@generated` recurrence" from "CSS-only" (V13 framing) to "CSS + JSON share the mechanism" (general); finding 9 cross-links to the per-grammar `RuntimeProvider` enum (A3 territory) as the *single* generality lever — one refactor (PRUNE-3 + R4 + PRUNE-2) closes both axes for *all* grammars. The 15 orphan `.bbnf` files in `grammar/css/l4/` is a *category* finding (14 of 15 unused) rather than a per-grammar enumeration. The R4 + CH7 gating recommendations (`regen-X` paired with `check-X` for every roster entry; Lock-14-companion lint rejecting any `// @generated` header without a matching regen-roster path) are grammar-neutral by construction — they apply to JSON, CSS L4, BBNF, Sheets, and any future grammar uniformly. |
| `sk-v14-audit-overfit-decision-engine.md` (A5) | **ACCEPT** | A5's 4 findings are grammar-neutral throughout: W8 per-grammar policy (`GrammarConfig`) is named for genericity (the policy struct must serve *every* grammar uniformly); W9 same-substrate union is a tape-layer concern keyed off `BackendShape`, not grammar identity; the NEW-MED gate-only footprint quantifies a generic-crate residency claim (zero matches in `passes/`, `codegen/`, `runtime/`, `ir/` — the truly generic crates). The §4.1 sequencing constraint (C-1 → C-4: PRUNE-3 + PRUNE-4 must land before PRUNE-5 wires W8/W9) is itself a *generality* argument: wiring W8 into the present per-grammar provider mesh would deepen the Lock-14 violation, not remediate it. Resolver self-labelling (LOW) carries block-ids that are grammar-agnostic ("JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT" names a *cross-grammar* block, not a JSON-only one). |
| `sk-v14-audit-overfit-pre-restart-pattern.md` (A6) | **ACCEPT** | Pattern H is the generality lens incarnate: 67 hand-written per-grammar runtime files spanning *9 grammar directories* (bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math) — every grammar in the tree, hot and cold alike. The 48-file skinny mirror covers all 7 CSS L4 providers + JSON + the test-only sheets_witness. NEW-HIGH-2 frames the substrate-doc opt-out enshrinement as covering JSON + CSS L4 + BBNF — explicitly the three hot grammars, not JSON alone. NEW-HIGH-1 (LegacyPath shim) covers 4 `parse_with.rs` files: `json/`, `css_l4/`, `bbnf/`, `google_sheets/` — every typed-segment-touching grammar. NEW-MED (pre-restart-API carry inside `google_sheets/document/canonical.rs`) is properly scoped to its own grammar (re-implementation of a *retired* pre-restart Sheets surface, not a Lock-14 leak). The four CLEAN axes (combinator/monolithic mix; backend-specific code in primitive crates; renamed pre-restart scanners; asm bibliographic — the asmjson citation is correctly flagged LOW, not promoted) confirm the negative findings hold across all generic crates uniformly. |

## §2 — Per-§ ACCEPT-rate

Per CONTEXT §3 (CH2 lens focus has three sub-clauses), the dispositions
break down as follows:

| CH2 sub-clause | ACCEPT | ACCEPT-with-NOTE | REVISE | REJECT |
| --- | ---: | ---: | ---: | ---: |
| (i) **A3's 30 violations cover Lock 14 surface comprehensively** | 0 | 1 | 0 | 0 |
| (ii) **A6's Pattern H scope is grammar-neutral** | 1 | 0 | 0 | 0 |
| (iii) **A4's xtask remediation recommendation generalizes** | 1 | 0 | 0 | 0 |
| (iv) **No JSON-specific scope creep across the 74 findings** | 5 | 0 | 0 | 0 |
| **All** | **7** | **1** | **0** | **0** |

Sub-clause (i): A3 verdict is ACCEPT-with-NOTE per the H3 production-vs-
test-fixture clarification in §1; the 30-violation surface is complete,
the severity recalibration is precision, not retraction.

Sub-clause (ii): A6 ACCEPTed in full; the 67-file Pattern H census,
the 48-file skinny mirror, the substrate-doc opt-out covering JSON +
CSS L4 + BBNF, and the LegacyPath shim covering all 4 typed-segment
grammars together make A6 the strongest GENERALITY-positive artefact
in the pack.

Sub-clause (iii): A4 ACCEPTed in full; the xtask USAGE extension
("every `regen-X` paired with `check-X`; `check-all` aggregate"), the
Lock-14-companion lint (REJECT any `// @generated` header without a
matching regen-roster path), and the round-trip mandate apply
uniformly across every grammar profile — no JSON-specific or CSS-
specific clause is embedded in the mechanism.

Sub-clause (iv): the SYNTHESIS + A1 + A2 + A4 + A5 + A6 ACCEPTs each
verify that no axis treats a JSON-only mechanism as the general
remediation contract; the prune list (SYNTHESIS §3.1) covers all 74
findings via grammar-agnostic C-N candidates.

## §3 — Executable verification (CH2 evidence)

Per CONTEXT §4 executable-verification mandate, the following counts /
greps were run against the working tree at `12ff0744e` (S-P0 dispatch
seed, == SK-V14 starting baseline) to substantiate the CH2 dispositions.

### §3.1 Per-grammar directory census (PRUNE-4 sub-wave count)

```
$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | sort
crates/core/src/runtime/bbnf
crates/core/src/runtime/bnf
crates/core/src/runtime/css_l4
crates/core/src/runtime/css_pretty
crates/core/src/runtime/csv
crates/core/src/runtime/ebnf
crates/core/src/runtime/google_sheets
crates/core/src/runtime/json
crates/core/src/runtime/math
```

9 directories — confirms A3 §1, A6 §1, SYNTHESIS §1.3 + §3.3. PRUNE-4
sub-wave count = 9 is correct; an 8-sub-wave plan would silently
orphan `css_pretty`. **CH2 verdict: GENERIC — every grammar directory
under the runtime crate is covered.**

### §3.2 Skinny mirror per-grammar census

```
$ find skinny/crates/runtime/src/grammars -mindepth 1 -maxdepth 1 -type d | sort
skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media
skinny/crates/runtime/src/grammars/css_l4_declaration_values
skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended
skinny/crates/runtime/src/grammars/css_l4_nested_layout
skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors
skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules
skinny/crates/runtime/src/grammars/css_l4_visual_functions
skinny/crates/runtime/src/grammars/json
skinny/crates/runtime/src/grammars/sheets_witness
```

9 directories (7 CSS L4 providers + JSON + sheets_witness test-only) —
confirms A4 §1 + A6 §1. The skinny mirror is comprehensive across the
CSS L4 wave family + JSON; the absence of bbnf/bnf/csv/ebnf/math here
reflects that the skinny path was JSON-first and CSS-second by
construction, not a CH2 gap.

### §3.3 Per-grammar provider modules in nominally-generic codegen crate

```
$ ls skinny/crates/codegen/src/*_provider.rs
skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_provider.rs
skinny/crates/codegen/src/css_l4_nested_layout_provider.rs
skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs
skinny/crates/codegen/src/css_l4_visual_functions_provider.rs
skinny/crates/codegen/src/json_provider.rs
```

8 hand-written per-grammar provider files (7 CSS L4 + 1 JSON), all in
the *nominally generic* `codegen` crate root — confirms A3 §1
C5..C11 + A4 §1 + A6 §1.E. **CH2 verdict: the violation is CSS L4 +
JSON, not JSON-only; the prune mechanism (PRUNE-3 deletion + R4
generic generator) must handle both.**

### §3.4 .bbnf grammar orphan census

```
$ ls grammar/css/l4/*.bbnf | wc -l
15

$ grep -rn "grammar/css/l4" skinny/ xtask/ 2>/dev/null | grep -v "/target/" | grep -v "\.md:" | head
[no output]
```

15 `.bbnf` files; **zero** file-system references from `skinny/` or
root `xtask/`. The only consumer in the repository is
`Cargo.toml:22` (totality-track root) for `stylesheet.bbnf` only —
the 14 other grammars are orphaned from *both* tracks. Confirms A4
NEW-3 + SYNTHESIS §1.2 finding 6. **CH2 verdict: the orphan claim
covers 14 of 15 grammars (a category statement), not single grammar;
R4 must consume all 15.**

### §3.5 Fake `@generated` header surface

```
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime crates/core/src/runtime | wc -l
42
```

42 files match — confirms A3 §1 generated-header count + A4 §2 round-
trip table. The header surface spans every CSS L4 provider runtime
twin (5 files × 7 providers = 35; one provider drops `sink.rs`) + 8
JSON runtime files. **CH2 verdict: the fake-`@generated` pattern is
not JSON-only and not CSS-only; it's a uniform overlay across both
grammar families.** A4 finding 8 ("V13 noted CSS only; A4 finds JSON
shares the pattern") is verified.

### §3.6 Fixture-lookup scanner audit

```
$ grep -n "CANONICAL_FIXTURE\|CAPTURED_W2_INPUT" \
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs
…/css_l4_nested_layout_templates/generated.rs:3:    const CANONICAL_FIXTURE: &str = concat!(
…/css_l4_nested_layout_templates/generated.rs:44:    if input == CANONICAL_FIXTURE {
…/css_l4_at_rules_and_media_templates/generated.rs:3:    const CANONICAL_FIXTURE: &str =
…/css_l4_at_rules_and_media_templates/generated.rs:25:    if input == CANONICAL_FIXTURE {
…/css_l4_vendor_and_custom_atrules_templates/generated.rs:3:    const CANONICAL_FIXTURE: &str = concat!(
…/css_l4_vendor_and_custom_atrules_templates/generated.rs:33:    if input == CANONICAL_FIXTURE {
…/css_l4_stylesheet_selectors_templates/generated.rs:3:    const CAPTURED_W2_INPUT: &str = concat!(
…/css_l4_stylesheet_selectors_templates/generated.rs:39:    if input == CAPTURED_W2_INPUT {
```

4 templates use byte-equality-on-fixture short-circuits (3 CSS L4
"CANONICAL" + 1 "CAPTURED_W2"). A4 §0 cited 3; the actual count is 4
(A4 §1 finding ledger captures rows 3, 4, 5, 6 — i.e. 4 findings in
the ledger; the §0 abstract said "three" undercounting by one). This
is a precision NOTE for the V2 fold (A4 §0 line 38 should read "four"
not "three"), but does not change the CH2 disposition since the *pattern
class* generalizes either way.

### §3.7 Skinny xtask regen subcommand roster

```
$ grep -n 'regen-' skinny/xtask/src/main.rs
8:    const USAGE: &str = "usage: cargo xtask <regen-json|check-json|
                              regen-real-typed|check-real-typed|
                              check-conformance|lint-loc|bench-json|
                              gate-json|primitive-checkasm>";
18:        "regen-json" => regen_json(&root),
20:        "regen-real-typed" => regen_real_typed(&root),
```

2 regen entry points (`regen-json`, `regen-real-typed`); **no
`regen-css*` of any kind**. Confirms A4 finding 2 + SYNTHESIS §2.4
R4 mandate + SYNTHESIS §2.1 R4 → PRUNE-2 sequencing. **CH2 verdict:
the xtask gap is structural — every grammar profile needs a regen
entry point; the proposed `check-all` aggregate is grammar-agnostic.**

### §3.8 Generic-crate grammar-name-leak negative scan

```
$ grep -rn -E '\b(Json|Css|Sheets|JsonValue|CssValue|SheetsValue|CssL4|CssPretty)\b' \
    skinny/crates/bbnf-regex/src/ \
    skinny/crates/bbnf-simd/src/ \
    skinny/crates/simd-scan/src/ \
    skinny/crates/parse-that-regex/src/ \
    skinny/crates/ir/src/
[no output]
```

Zero matches across the 5 truly-primitive generic crates. Confirms
A3 D1 NOTE + A6 §1 final scan. **CH2 verdict: the pure-primitive
crates remain CLEAN; Lock 14 violations are bounded to `passes`,
`codegen`, `runtime`, `bbnf`, `grammar` (the application-tier
crates) — precisely the boundary the prune slate targets.**

### §3.9 Decision-engine generic-crate footprint

```
$ grep -rn 'per_grammar_policy\|same_substrate_union\|GrammarConfig' \
    skinny/crates/passes skinny/crates/codegen skinny/crates/runtime skinny/crates/ir
[no output]

$ grep -rn 'per_grammar_policy\|same_substrate_union\|GrammarConfig' \
    skinny/crates/bbnf-bench
[20 hits across gate.rs / lock14_baseline.rs / report.rs only]
```

Zero matches in `passes/`, `codegen/`, `runtime/`, `ir/`; all 20
matches localised to `bbnf-bench`. Confirms A5 §1.2 + §1.3 NEW-MED
gate-layer-only quantification. **CH2 verdict: W8/W9 are absent from
the truly-generic compile/runtime path uniformly; PRUNE-5's wiring
must land into a generic surface (GrammarConfig struct, UnionTape
variant) — grammar-agnostic by construction per A5 §4.**

## §4 — Critical findings (CH2 lens)

The audit pack as authored does not surface any CH2-fatal defect. The
following observations are V2-fold candidates only, not blockers.

### §4.1 A3 H3 production-vs-test precision (NOTE)

A3's H3 cites `decision_csp.rs:235` (literal `finalize_rule("json",
RuleId(0), …)`) as a HIGH "CSP solver hardcoded to JSON entry-rule"
violation. Verification shows line 235 sits inside a
`#[cfg(test)] mod tests { … }` block (test fixture exercising the
solver in isolation). The production call site is `passes/src/lib.rs:
478` — `crate::decision_csp::finalize_rule(&grammar.name, rule.id,
candidates, active)` — generic over `&grammar.name`, threaded from the
grammar object.

The violation class (JSON-named identifier in a generic crate's test
surface) holds, but the severity recalibrates from HIGH to LOW. The
companion finding H6 (CSS L4 entry rule absent from the *acceptance
test surface*) correctly stays HIGH on its own merits: the test
fixture should call `finalize_rule("css_l4", …)` and
`finalize_rule("sheets", …)` for parity, not only `"json"`. V2 should:

- Migrate H3 to LOW (test-fixture name leak; same tier as L1..L7).
- Re-state H6 as the HIGH "decision-engine acceptance surface is
  JSON-only; CSS L4 + Sheets + BBNF coverage absent".
- Keep the aggregate count at 30 (severity reshuffle: 11 CRIT + 6
  HIGH + 5 MED + 8 LOW).

This is a precision adjustment that *strengthens* the CH2 disposition
(makes the violation class match its structural meaning); it does not
weaken the FAIL verdict or change the prune-list mapping.

### §4.2 A4 §0 fixture-lookup count precision (NOTE)

A4 §0 line 38 reads "Three of the seven CSS scanners short-circuit on
…". The §2 ledger captures 4 fixture-lookup findings (rows 3, 4, 5,
6), and the `grep -n "CANONICAL_FIXTURE\|CAPTURED_W2_INPUT"` in §3.6
above returns 4 templates (nested_layout, at_rules_and_media,
vendor_and_custom_atrules, stylesheet_selectors). The §0 abstract
should read "Four of the seven CSS scanners …". This is editorial,
not substantive — the pattern class is intact and the per-finding
ledger is correct.

### §4.3 No JSON scope creep observed

The most stringent CH2 search is for *covert* JSON-only assumptions
embedded in nominally-generic prune actions. Re-reading SYNTHESIS §3
C-1..C-5 and the per-axis §4 recommended-action tables:

- **R1 (per-iter equality oracle)** — applies to every parity bench
  group; mechanism is plane-keyed, not grammar-keyed.
- **R2 (strict-vs-strict comparator)** — mandates per-plane comparator
  binding (parse_only ≠ direct ≠ typed); generalises across grammars.
- **R4 (regen-css xtask)** — specifically authors CSS L4 because CSS
  is the gap; but the mechanism (xtask reads `.bbnf` + emits
  `generated.rs`) is the same pattern `regen-json` already follows.
  The CH7 gating recommendation extends to "every `regen-X` paired
  with `check-X`; `check-all` aggregate" — grammar-agnostic.
- **R5 (production corpus)** — `skinny/corpora/css-l4-sk-v14/` is
  CSS-named because CSS is the under-corpused grammar; the per-corpus
  1 KB floor is the universal criterion.
- **PRUNE-3 (RuntimeProvider enum → trait dispatch)** — eliminates
  the per-grammar enum variants entirely; mechanism is structurally
  grammar-agnostic.
- **PRUNE-4 (per-grammar runtime collapse)** — 9 sub-waves, one per
  grammar dir; each sub-wave applies the same template-or-real-codegen
  decision uniformly.
- **PRUNE-5 (W8 + W9 wiring)** — `GrammarConfig` struct + `UnionTape`
  variant; both grammar-agnostic by name and by mechanism.
- **Lock-14-companion lint** (A4 + SYNTHESIS §2.4) — rejects *any*
  `// @generated` header in `skinny/crates/runtime/src/grammars/
  **/*.rs` unless the matching path appears in a recognised regen
  subcommand's emission roster. Pattern-keyed, not grammar-keyed.

No JSON-specific assumption smuggled into any remediation step.

## §5 — New finding (CH2 lens — beyond the 74)

One new GENERALITY concern surfaced during executable verification
that the existing 74 findings do not explicitly capture, recommended
for V2 fold:

**N-CH2-1 (LOW, mechanism-precision)** — The CH7-companion lint
proposed by A4 + SYNTHESIS §2.4 ("REJECT any new `// @generated by
skinny bbnf-codegen` header in `skinny/crates/runtime/src/grammars/
**/*.rs` unless the matching path appears in a recognized regen
subcommand's emission roster") should also scope to the *codegen-side
twin* of each runtime file. Verification §3.5 returned 42 files with
the fake header, including 8 codegen-side template + provider files
(per A4 §2.1 grep output). A lint that scopes only to `skinny/crates/
runtime/src/grammars/**/*.rs` would let the codegen-side twin re-
introduce the fake header silently — exactly the round-tripping vector
A4 finding 15 enumerates (identical-content twins between codegen-
side template and runtime-side generated.rs). The lint glob should
read `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` to
cover both sides of the emit-write boundary. This is a mechanism
hardening of an existing recommendation, not a new finding class;
recommend fold into PRUNE-5 (or the C-3 + CH7 gating extension)
alongside the original lint proposal.

## §6 — V2 fold recommendations

If S-P0 V2 dispatches:

1. **A3 H3 → migrate to LOW (test-fixture name leak); promote H6 to
   absorb the HIGH "decision-engine acceptance surface JSON-only" bar
   on its own structural merits.** Aggregate count unchanged at 30;
   severity reshuffle: 11 CRIT + 6 HIGH + 5 MED + 8 LOW.
2. **A4 §0 abstract: correct "three" → "four"** for fixture-lookup
   scanner count (the §2 ledger and §3.6 verification both show 4).
3. **CH7-companion lint glob scope extension** (N-CH2-1 above):
   include codegen-side twin (`skinny/crates/codegen/src/**/*.rs`)
   alongside runtime-side (`skinny/crates/runtime/src/grammars/**/
   *.rs`) so the round-trip enforcement covers both sides.

None of these block §3Z convergence. CH2 returns ACCEPT-rate 100 %
(7 ACCEPT + 1 ACCEPT-with-NOTE / 8 dispositions = 100 % per §3Z
convergence arithmetic; ACCEPT-with-NOTE counts as ACCEPT for the
≥ 95 % criterion since no REJECT or REVISE is open).

## §7 — Disposition

**CH2 VERDICT: ACCEPT.** The S-P0 V1 audit pack holds the Lock 14
generality lens end-to-end — every CRITICAL and HIGH finding
generalises across the 9-grammar surface, every proposed prune
mechanism is grammar-agnostic by construction, every NEW finding
either elevates a JSON-only V13 framing to its cross-grammar mirror
or surfaces a category-level pattern. The 11 NEW findings advance
GENERALITY rather than scope-creep against it. The two precision
NOTEs (A3 H3 severity tier, A4 §0 fixture count) and the one
mechanism extension (CH7 lint glob scope) are V2-fold candidates;
none block convergence.

---

**Authored:** 2026-05-23 (SK-V14 S-P0 CHALLENGE V1 lens CH2).
**Status:** WRITE-ONLY; aggregator commits all 8 V1 files atomically.
**Authority:** `restart/prompts/ORCHESTRATOR.md §3W` CH2 row; CONTEXT
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CHALLENGE-
CONTEXT.md` §3 CH2.
