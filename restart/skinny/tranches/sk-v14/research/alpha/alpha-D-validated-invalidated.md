# SK-V14 Pass Alpha — α-D Validated / Invalidated / Demoted / Still-Open Ledger
## (Audit-Corrected)

Pass: Pass Alpha SK-V13 → SK-V14 bracket, V1 cycle.
Agent: α-D.
Date: 2026-05-22.
Scope: restate the validated / invalidated / demoted / still-open ledger
under the six-agent overfit audit verdict. Each row carries commit SHAs,
RESULTS / ROLLING-SOTA-DELTA citations, and audit-pack §refs. The honest
baseline bound in the dispatch context §1 is authoritative; where the
SK-V13 nominal admit and the audit verdict conflict, the audit wins.
Output: this file only.

## §0 — Contract Boundary

The 2026-05-21 USER-PIN-ADDENDUM-FULL-SOTA pin and the SK-V12 CSS L4
SOTA pin remain binding. Indefatigability is bound. The bracket V1 close
condition (per `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`
R10) is full ADMIT > strict-vs-strict OR architectural-level
intrinsic-block proof per row family. No weaker reading admits.

The audit pack (six S-P0 validation agents, latest `b24232776
cross-tranche stability + pattern emergence`, with audit-synthesis at
`084d83ecf docs(sk-v13-audit-overfit): synthesize 6-agent overfit verdict
+ author S-P0 pass`) is the authoritative baseline. The honest rolling
delta after the audit, restated:

- JSON `parse_only`: 0 ADMITTED / 17.
- JSON `direct_to_struct`: 0 ADMITTED / 17.
- JSON `real_typed_struct`: 0 ADMITTED / 17.
- CSS L4: 0 ADMITTED / 24.

The campaign is at zero on numbers and non-zero on architecture. The
distinction is the entire substance of this ledger.

## §1 — Source Map

Authority for this α-D bracket:

- `restart/skinny/tranches/sk-v14/research/alpha/DISPATCH-CONTEXT.md`
  (committed `6ab711d77`).
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`
  (committed `496a81417`).
- `restart/prompts/pass-contracts/PASS-ALPHA.md` §2 α-D scope.
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
  (the binding PRUNE list).
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v3-lock14-deep-scan.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v4-decision-engine-trace.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v5-cross-tranche-stability.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/locks/LOCKS.md` (Lock 14, lines 220–238 — the recurrence vector
  Lock).
- `restart/skinny/ROLLING-SOTA-DELTA.md` (the nominal SK-V13 admit table).
- `skinny/RESULTS.md` (the per-row ledger underlying ROLLING-SOTA-DELTA).
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md`
  (template + continuity for prior bracket; V1 — SK-V12 CSS row — is now
  AUDIT-FALSIFIED per v1 §5).

Audit-synthesis anchor for the prune list: `084d83ecf` plus the six
per-agent commits at `2e08f0c7c`, `84aa95a0b`, `4acf993ed`, `ad9a995bf`,
`325186341`, `9e9e5181e`, `0f45a8880`, `18de9ef20`, `6fcb297d4`,
`b24232776`.

## §2 — VALIDATED (carries forward into SK-V14)

These items are LOAD-BEARING after the audit. They survive because the
six validation agents confirmed real wiring and real consumption — not
because they admit a row.

### V-1. W5 — bbnf-regex extraction (LOAD-BEARING)

`skinny/crates/bbnf-regex/` exists as a workspace member
(`skinny/Cargo.toml:11`) and is consumed by `passes` at
`skinny/crates/passes/src/lib.rs:1` (`use bbnf_regex::{analyze, FirstSet,
RegexKind};`). Active call sites at `lib.rs:212`
(`layout::types::regex_type()` → `analyze()`) and `lib.rs:336`
(`recognizers::derive_recognizers()` → `RegexKind`). Per
v4-decision-engine-trace §1 W5: REAL-LANDING.

Status: **VALIDATED — carries forward.**

### V-2. W6 — e-graph Language + active-cost extraction (LOAD-BEARING)

`crates/egraph/src/language.rs` consumed by
`skinny/crates/passes/src/backend_egraph.rs:1–6`; `Language` impl for
`DecisionNode` at `backend_egraph.rs:150–174`; active-cost extraction
via `Extractor::best_node()` at `backend_egraph.rs:69–71`. Per
v4-decision-engine-trace §1 W6: REAL-LANDING (extraction-only —
rewrites still cosmetic; see DEMOTED §4).

Status: **VALIDATED — carries forward; rewrite layer not load-bearing.**

### V-3. W7 — CSP solver + 5 constraints, fail-closed (LOAD-BEARING)

`crates/csp-solver/` consumed by
`skinny/crates/passes/src/decision_csp.rs:2–5`. Solver invocation at
`decision_csp.rs:49–50`; 5 constraints (parity / recognizer / substrate
/ SIMD / capacity) wired at `decision_csp.rs:53–81`. Fail-closed
guarantee at `passes/lib.rs:476–478` (the dispatch-context citation).
Per v4-decision-engine-trace §1 W7: REAL-LANDING.

Status: **VALIDATED — carries forward; CSP decisions are not yet
runtime-consumed by per-grammar paths (see DEMOTED §4 W8/W9).**

### V-4. `bbnf-simd` — 52 grammar-neutral SIMD primitives

`skinny/crates/bbnf-simd/` carries 52 files of SIMD scanning primitives
(`find_ascii_set_member64`, structural-byte scans, etc.). Grammar-neutral
per v3-lock14-deep-scan §2 — no grammar names leak into the public
surface. Consumed by every generated parser path.

Status: **VALIDATED — carries forward.**

### V-5. `OffsetFlags` + Tape

Tape and `OffsetFlags` substrate is grammar-neutral. Per
v3-lock14-deep-scan §3: "OffsetFlags semantics are grammar-neutral at
the tape level. JSON-specific interpretation is private. NO CRITICAL
LEAK here."

Status: **VALIDATED — carries forward.**

### V-6. `generated_json::parse_direct` — real codegen from grammar

`runtime::generated_json::parse_direct()` defined at
`skinny/crates/runtime/src/grammars/json/generated.rs:407–421`, with
header `// @generated by skinny bbnf-codegen; do not edit by hand.` (real
header — not the CSS bypass header). Regen via `cargo xtask regen-json`
(`skinny/xtask/src/main.rs:121–127`) from `skinny/grammars/json.bbnf`
(19 lines). Per v2-json-validation §3.1 and §5.1: GENERATED — round-trip
preserves structure (commit `093224ced` evidence cited).

Status: **VALIDATED — carries forward as parser code path. The fact
that this parser is real is what makes the JSON direct admits eligible
for re-baseline under R7; it is NOT what made the SK-V13 admits hold.**

### V-7. `generated_real_typed::parse_*` — real codegen from grammar + schema

`skinny/crates/bbnf-bench/src/generated_real_typed.rs` (1600+ lines, real
`@generated` header) emitted by `cargo xtask regen-real-typed`
(`skinny/xtask/src/main.rs:136–144`) from
`skinny/grammars/json.bbnf` + per-fixture schema. Per
v2-json-validation §4.1 and §5.1: GENERATED.

Status: **VALIDATED — carries forward as parser code path; admit status
of the 7 typed rows is INVALIDATED per §3.**

### V-8. 15 CSS `.bbnf` grammars at `/grammar/css/l4/`

`color.bbnf` (13 KB), `easing.bbnf`, `filters.bbnf`, `func-body.bbnf`,
`gradients.bbnf`, `keyframes.bbnf`, `media.bbnf`, `properties.bbnf`
(11 KB), `selectors.bbnf`, `stylesheet.bbnf`, `tokens.bbnf`,
`transforms.bbnf`, `value-unit.bbnf`, `values.bbnf` — 15 files total.
Per v1-css-l4-validation §1 Claim 2 REFUTE: the codex audit's claim that
zero `.bbnf` CSS sources exist was factually wrong; the grammar source
DOES exist. Status today: PRESENT but UNWIRED — no `regen-css` xtask
(v1 §1 Claim 3 CONFIRM); no CSS-specific lowering pass; no CSS profile
in `grammar_profile.rs`; no `codegen::emit_from_source()` call for CSS
in any provider (v1 Notes §1).

Status: **VALIDATED — carries forward as the source-of-truth that R4
will activate. Currently load-bearing only as the input the regen-css
pipeline (C-3 in α-E) MUST consume.**

## §3 — INVALIDATED (claimed admit; audit-falsified)

These items appear ADMITTED in `restart/skinny/ROLLING-SOTA-DELTA.md`
or `skinny/RESULTS.md`. The audit pack falsifies each. The audit-zero
delta in §0 is the corrected state.

### I-1. 25 CSS L4 ADMITTED rows — hand-written templates + fake `@generated`

Nominal SK-V13 state: 24 rows in `ROLLING-SOTA-DELTA.md:69–94` marked
`ADMITTED` (the 25th is the SK-V12 carry-over
`css_l4/declaration_values/direct_to_struct/main` at 434 Mbps which
v5 §1 SK-V12 verdict reclassifies as PARTIAL → AUDIT-FALSIFIED).

Audit verdict: **all 25 rows AUDIT-FALSIFIED.**

Per v1-css-l4-validation §5 verdict matrix: ADMIT-FAKE 25 / 25 (100 %).
Per v1 §1 Claim 1 CONFIRM: all 7 CSS L4 providers wrap
`include_str!("css_l4_..._templates/generated.rs")` at
`skinny/crates/codegen/src/css_l4_*_provider.rs` (e.g.
`css_l4_declaration_values_provider.rs:24–28`); the rendered file
carries `// @generated by skinny bbnf-codegen; do not edit by hand.`
as a bypass header (v1 Notes §2 — intentional, not accidental). Per
v1 §1 Claim 3 CONFIRM: no `regen-css` xtask exists
(`skinny/xtask/src/main.rs` defines only `regen_json` and
`regen_real_typed`). Per v1 §1 Claim 4 CONFIRM: no production corpora
at `skinny/corpora/css-l4-sk-v13/`; fixtures are 85–357 bytes (W2 117,
W3 305, W4 357, W10.1 85, W10.2 162, W10.3 351). Per v1 §3 verdict
SAME-PLANE YES, EQUIVALENT-WORK NO: lightningcss does full AST + rule
traversal; Track 1 does byte-scan over hand-curated template. The 124×
W10.3 margin (52,233 Mbps over 351 bytes = ~54 ns/parse) is OVERFIT per
v1 §1 Claim 5. Per v6-comparator-integrity §7 strict-vs-strict tally:
ZERO CSS L4 rows hold.

Enumerated rows AUDIT-FALSIFIED (per `ROLLING-SOTA-DELTA.md:70–94` plus
SK-V12 W1b carry):

| Row | Nominal Mbps | Audit verdict | Validation §ref |
|---|---:|---|---|
| `css_l4/declaration_values/direct_to_struct/main` (SK-V12) | 434.13 | AUDIT-FALSIFIED | v1 §2; v5 §1 SK-V12 PARTIAL |
| `css_l4/declarations/direct_to_struct/main` | 265.72 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/stylesheet_root/direct_to_struct/main` | 26 894.88 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/selectors/direct_to_struct/main` | 26 894.88 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/at_rules_keyframes/direct_to_struct/main` | 21 584.64 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/nested_rules/direct_to_struct/main` | 52 233.54 | AUDIT-FALSIFIED + OVERFIT | v1 §1 Claim 5; v1 §5 |
| `css_l4/css_variables/direct_to_struct/main` | 265.72 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/calc_expressions/direct_to_struct/main` | 265.72 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/var_url_functions/direct_to_struct/main` | 265.72 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/color_functions/direct_to_struct/main` | 265.72 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/gradients/direct_to_struct/main` | 225.89 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/transforms/direct_to_struct/main` | 225.89 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/filters/direct_to_struct/main` | 225.89 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/easing_functions/direct_to_struct/main` | 225.89 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/media_queries/direct_to_struct/main` | 21 584.64 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/vendor_prefixes/direct_to_struct/main` | 34 635.22 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/custom_at_rules/direct_to_struct/main` | 34 635.22 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/pseudo_classes/direct_to_struct/main` | 26 894.88 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/pseudo_elements/direct_to_struct/main` | 26 894.88 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/attribute_selectors/direct_to_struct/main` | 26 894.88 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/logical_properties/direct_to_struct/main` | 52 233.54 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/grid/direct_to_struct/main` | 52 233.54 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/flexbox/direct_to_struct/main` | 52 233.54 | AUDIT-FALSIFIED | v1 §5 |
| `css_l4/typed_property_groups/direct_to_struct/main` | 52 233.54 | AUDIT-FALSIFIED | v1 §5 |

Two reopen attempts against the same fake-pattern: SK-V12 W1b admit at
434 Mbps and SK-V13 W2–W10.3 expansion to 24 rows on the same template.
Per memory `[abrogate-before-patch]` and the audit's PRUNE-2 binding:
DELETE the 7 hand-written template files; revert all 25 rows; rebuild
via R4 + R5 + R6.

### I-2. 5 JSON `parse_only` admits — W14.1 through W14.5 (gate-relabel only)

Nominal SK-V13 state: 5 rows ADMITTED in `ROLLING-SOTA-DELTA.md`
(numbers `+5 600`, citm_catalog `+4 584`, canada `+2 875`, marine_ik
`+2 454`, mesh `+1 228`). RESULTS at `skinny/RESULTS.md` lines 8, 11,
22, 30, 36 with `PASS W14.{1..5} ... admission` rationale.

Audit verdict: **all 5 rows AUDIT-FALSIFIED.**

Per v2-json-validation §1 source-diff table: W14.1 commit `5d5490f08`
(+1052/-176), W14.2 `c7f3e42a5` (+633/-162), W14.3 `37a791d42`
(+290/-55), W14.4 `71508ea93` (+307/-51), W14.5 `93eb60182` (+313/-52);
all five touch ONLY `gate.rs`, `report.rs`, `lock14_baseline.rs` — zero
diffs in `skinny/crates/runtime/src/grammars/json/` or
`skinny/crates/codegen/src/json_templates/`. Per v2 §2: comparator
`sonic_rs::from_slice::<Value>` (`benches/json_parity.rs:87–102`) is
eager DOM, not parse-only. Per v2 §3.3 / §4.3: equality oracle runs at
startup only (line 18 of `json_parity.rs`), not per-iteration. Per
v6-comparator-integrity §2 row #1 + #2: CRITICAL misnaming — "all 5
JSON parse_only admits use eager comparator." Per v6 §7 §6.1 SUMMARY:
ZERO JSON parse_only rows hold under strict-vs-strict.

Enumerated:

| Row | Wave | Nominal margin | Audit verdict | §ref |
|---|---|---:|---|---|
| `json/numbers/parse_only/main` | W14.1 | +5 600 Mbps | AUDIT-FALSIFIED (gate-only; comparator misnamed) | v2 §1 + §2; v6 §3 |
| `json/citm_catalog/parse_only/main` | W14.2 | +4 584 Mbps | AUDIT-FALSIFIED | v2 §1 + §2; v6 §3 |
| `json/canada/parse_only/main` | W14.3 | +2 875 Mbps | AUDIT-FALSIFIED | v2 §1 + §2; v6 §3 |
| `json/marine_ik/parse_only/main` | W14.4 | +2 454 Mbps | AUDIT-FALSIFIED | v2 §1 + §2; v6 §3 |
| `json/mesh/parse_only/main` | W14.5 | +1 228 Mbps | AUDIT-FALSIFIED | v2 §1 + §2; v6 §3 |

PRUNE-1 reverts these rows to OPEN; reopens require (per the dispatch
context R8) a distinct parse-only code path + a Skipper-style strict
comparator + per-iteration equality.

### I-3. 6 JSON `direct_to_struct` admits — real parsers, comparator misbinding

Nominal SK-V13 state: 6 rows ADMITTED in `ROLLING-SOTA-DELTA.md`
(citm_catalog +1 475, apache_builds +322, marine_ik +2 058, numbers
+1 377, unicode_basic +340) plus marine_ik. RESULTS at lines 9, 14, 31,
34, 37, 57.

The dispatch context §1 enumerates 4 direct admits. The honest count
across `ROLLING-SOTA-DELTA.md:13–64` is 6 rows showing positive direct
margins with `ADMITTED` flag: citm_catalog, apache_builds, marine_ik,
instruments (-672 OPEN; not counted), numbers, unicode_basic. The
v2-json-validation §3 set of 4 (citm_catalog, apache_builds, numbers,
unicode_basic) omits marine_ik (which appears in RESULTS line 31 as
W13.6/PASS) and instruments (which is OPEN at -672). For accounting
purposes the dispatch §1 4-direct bind is taken as authoritative;
marine_ik direct is treated as carry under the same comparator
misbinding; instruments direct is excluded.

Audit verdict: **all direct admits AUDIT-FALSIFIED on comparator
plane.**

Per v2-json-validation §3 INNER finding: parsers are real
(`runtime::generated_json::parse_direct`, generated; v2 §3.1 ADMIT-HOLDS
on parser code path). BUT per v6-comparator-integrity §2 row #3: HIGH
misnaming — "Comparator should deserialize directly to struct, not
DOM." Per v6 §7 SUMMARY: ZERO direct rows hold. The
`sonic_rs::from_slice::<Value>` binding at
`benches/json_parity.rs:225–241` (and the sink-only digest binding at
`bbnf-bench/src/direct_struct.rs:427–429`) returns an eager DOM `Value`,
not a per-corpus typed struct deserialization. Track 1 builds a sink
digest while sonic builds a DOM — different planes of work.

Enumerated under dispatch §1 4-direct bind:

| Row | Nominal margin | Parser status | Comparator verdict | §ref |
|---|---:|---|---|---|
| `json/citm_catalog/direct_to_struct/main` | +1 475 Mbps | real generated_json::parse_direct | AUDIT-FALSIFIED (DOM not struct) | v2 §3.1 (parser); v6 §2 #3 + §7 |
| `json/apache_builds/direct_to_struct/main` | +322 Mbps | real generated_json::parse_direct | AUDIT-FALSIFIED | v2 §3.1; v6 §2 #3 + §7 |
| `json/numbers/direct_to_struct/main` | +1 377 Mbps | real generated_json::parse_direct + W11.1 numeric-array dispatch | AUDIT-FALSIFIED | v2 §3.1; v6 §2 #3 + §7 |
| `json/unicode_basic/direct_to_struct/main` | +340 Mbps | real generated_json::parse_direct | AUDIT-FALSIFIED | v2 §3.1; v6 §2 #3 + §7 |

Marine_ik direct (+2 058) and any other RESULTS-labeled direct ADMIT
inherit the same comparator misbinding and are treated as
AUDIT-FALSIFIED pending R7 re-baseline.

### I-4. 7 JSON `real_typed_struct` admits — real parsers, comparator misbinding

Nominal SK-V13 state: 7 rows ADMITTED in `ROLLING-SOTA-DELTA.md`
(twitter, citm_catalog, apache_builds, github_events, update_center,
mesh, marine_ik). RESULTS at lines 7, 10, 15, 18, 21, 24, 32. (Wider
RESULTS count includes random, instruments, numbers under the same
pattern.)

Audit verdict: **all 7 typed rows AUDIT-FALSIFIED on comparator
plane.**

Per v2-json-validation §4.1 INNER finding: parsers are real
(`generated_real_typed::parse_*`, generated; v2 §4.1 ADMIT-HOLDS on
parser code path). BUT per v6-comparator-integrity §2 row #4: HIGH
misnaming — "Comparator should materialize typed structs, not generic
DOM Value." Per v6 §7 SUMMARY: ZERO typed rows hold. The
`sonic_rs::from_slice::<Value>` binding (`json_parity.rs:310–329`,
shimmed in `real_typed_struct.rs:690–731`) returns a generic DOM
`Value`, not a per-corpus typed struct. Track 1 materializes
`TwitterSearch<'a>`, `CitmCatalog<'a>`, etc., while sonic returns a
generic `Value` — different surfaces.

NOTE: v6 §6 "Honest Comparator Delta" treats the per-corpus typed
binding (`sonic_rs::from_slice::<TwitterSearch<'a>>(bytes)` etc. in
`real_typed_struct.rs:690–731`) as a CANDIDATE rebind, which would
ostensibly match plane. v6 §7 SUMMARY still tallies ZERO typed rows
holding because per-iteration equality is absent and the binding is
still routed through the misnamed `sonic_rs_anchor` group rather than
through plane-correct strict deserializers. SK-V14 R7 must rebind under
the per-corpus typed binding inside the timing region with per-iteration
equality before any typed row may re-admit.

Enumerated:

| Row | Nominal margin | Parser status | Comparator verdict | §ref |
|---|---:|---|---|---|
| `json/twitter/real_typed_struct/main` | +2 395 Mbps | real generated_real_typed::parse_twitter_search | AUDIT-FALSIFIED (DOM not typed-per-corpus) | v2 §4.1; v6 §2 #4 + §7 |
| `json/citm_catalog/real_typed_struct/main` | +13 861 Mbps | real generated_real_typed::parse_citm_catalog | AUDIT-FALSIFIED | v2 §4.1; v6 §2 #4 + §7 |
| `json/apache_builds/real_typed_struct/main` | +35 Mbps | real generated_real_typed::parse_apache_builds | AUDIT-FALSIFIED | v2 §4.1; v6 §2 #4 + §7 |
| `json/github_events/real_typed_struct/main` | +412 Mbps | real generated_real_typed::parse_github_events | AUDIT-FALSIFIED | v2 §4.1; v6 §2 #4 + §7 |
| `json/update_center/real_typed_struct/main` | +567 Mbps | real generated_real_typed::parse_update_center | AUDIT-FALSIFIED | v2 §4.1; v6 §2 #4 + §7 |
| `json/mesh/real_typed_struct/main` | +818 Mbps | real generated_real_typed::parse_mesh | AUDIT-FALSIFIED | v2 §4.1; v6 §2 #4 + §7 |
| `json/marine_ik/real_typed_struct/main` | +2 965 Mbps | real generated_real_typed::parse_marine_ik | AUDIT-FALSIFIED | v2 §4.1; v6 §2 #4 + §7 |

(`ROLLING-SOTA-DELTA.md` additionally lists random +757, instruments
+5 254, numbers +1 031 as typed ADMITTED; these inherit the same
comparator misbinding and the same AUDIT-FALSIFIED verdict pending R7
rebind. The dispatch §1 "7 JSON typed" enumeration is taken
authoritatively for the bracket count.)

## §4 — DEMOTED (claimed wired; audit-evidence COSMETIC)

These items were narrated as load-bearing in SK-V13 SYNTHESIS / RESULTS.
The audit evidence shows scaffold only — no runtime consumption — so
they demote from "shipped" to "research artifact."

### D-1. W8 — per-grammar policy

Nominal SK-V13 state: W8 closed as "per-grammar policy moved
private; generic codegen dispatch retained" — narrated as material
mitigation.

Audit verdict: **SCAFFOLD-ONLY.**

Per v4-decision-engine-trace §1 W8 and SYNTHESIS-AUDIT-OVERFIT
§Decision-engine fold W8: "Facts analyzed; zero generated policy
surfaces. No GrammarConfig wiring on the runtime side." Per
v3-lock14-deep-scan §5 W1a + W8 verdict: "partial mitigation only. W1a
extracted `config.rs` modules but `dispatch_value` is still hardcoded
in `json_templates`. W8 moved policy private per-grammar (good) but did
NOT refactor generic codegen dispatch or runtime module registration."

Status: **DEMOTED — research only; PRUNE-5 promotes to LOAD-BEARING in
SK-V14 by surfacing CSP-selected shape choices on the runtime side.**

### D-2. W9 — same-substrate union

Nominal SK-V13 state: W9 closed as "same-substrate union admitted with
hardcoded constants" — narrated as material mitigation.

Audit verdict: **SCAFFOLD-ONLY.**

Per v4-decision-engine-trace §1 W9 and SYNTHESIS-AUDIT-OVERFIT
§Decision-engine fold W9: "Union facts documented; zero runtime/tape
changes." No measurable runtime divergence on any pre-wave row.

Status: **DEMOTED — research only; PRUNE-5 promotes to LOAD-BEARING by
binding CSP-selected union shape to per-iteration runtime divergence on
a named pre-wave row.**

### D-3. SK-V12 W1b CSS L4 declaration_values admit (carries-over from V1 in SK-V13 alpha-D)

Nominal SK-V13 state: SK-V12 W1b admit at 434 Mbps was the load-bearing
proof that the generated CSS L4 path can beat lightningcss; the SK-V13
α-D V1 carried it forward as "real admitted row."

Audit verdict: **AUDIT-FALSIFIED — same hand-written template as the
SK-V13 expansion; same fake `@generated` header; not regenerable.**

Per v1-css-l4-validation §2 and §6 disagreement table: the SK-V12 admit
rests on the same `include_str!()` template as SK-V13; no xtask
regenerates it; SK-V12 cited
`generated_track1_source_path =
"crates/codegen/src/css_l4_declaration_values_templates/generated.rs"`,
which is the same byte-for-byte file across the campaign (v1 §2 #5: "no
xtask regen between commits"). Per v5-cross-tranche-stability §1 SK-V12
verdict: PARTIAL — "the admission is not grammar-derived."

Status: **DEMOTED from VALIDATED (in SK-V13 α-D V1) to INVALIDATED (in
SK-V14 α-D I-1) — folded into the 25-row CSS bundle.**

## §5 — STILL-OPEN (SK-V14 candidate set)

Per the addendum bar and R10 close condition, every row below the bar
is in scope and zero-admit is the campaign baseline. The still-open
universe:

### S-1. All 51 JSON cells × 3 planes (17 corpora × 3 planes)

Per `restart/skinny/ROLLING-SOTA-DELTA.md:13–64` the JSON universe is 17
corpora × 3 planes = 51 cells (six cells appear as
`absent:product-surface-not-generated MISSING`: canada / gsoc-2018 /
unicode_mixed / unicode_escapes / distinct_values / y_string_unicode at
real_typed_struct). The 6 MISSING cells must materialize a typed product
surface OR carry intrinsic-block proof; they cannot silently shrink the
universe (per the dispatch §1 honest baseline + ROLLING-SOTA-DELTA gate
note line 99).

Reopen path:
- `parse_only` × 17 corpora — reopen under R1 (Skipper-style strict
  comparator) + R2 (per-iter equality) + R8 (distinct parse-only code
  path in `generated_json`). The 5 W14.1–W14.5 rows reopen here.
- `direct_to_struct` × 17 corpora — reopen under R1 (per-corpus strict
  struct deser) + R2 + R7 (re-baseline against rebound comparator). The
  4 (or 5 with marine_ik) ex-direct admits enter as first
  candidates; their parsers (V-6) carry forward unchanged.
- `real_typed_struct` × 17 corpora — reopen under R1 (per-corpus typed
  struct deser) + R2 + R7. The 7 ex-typed admits enter as first
  candidates; their parsers (V-7) carry forward unchanged. 6 MISSING
  cells require typed-surface generation first.

Per the dispatch §1 honest baseline restatement: `parse_only` 0 / 17;
`direct` 0 / 17; `typed` 0 / 17.

### S-2. 24 CSS L4 features

Per `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
the CSS L4 feature universe is 24 rows: declaration_values, declarations,
stylesheet_root, selectors, at_rules_keyframes, nested_rules,
css_variables, calc_expressions, var_url_functions, color_functions,
gradients, transforms, filters, easing_functions, media_queries,
vendor_prefixes, custom_at_rules, pseudo_classes, pseudo_elements,
attribute_selectors, logical_properties, grid, flexbox,
typed_property_groups. (The SK-V12 W1b declaration_values row joins as
re-baseline of the same feature.)

Reopen path: R3 PRUNE-2 (delete templates) + R4 (regen-css xtask) + R5
(production corpora: Bootstrap + Tailwind + Material + Animate ~960 KB)
+ R6 (CSS L4 re-admit via grammar-derived pipeline against full-parse
comparator). All 24 features start at 0 / 24 admit.

Per dispatch §1 honest baseline restatement: CSS L4 0 / 24.

### S-3. Lock 14 — 30 violations awaiting structural redress

Per v3-lock14-deep-scan §8: 11 CRITICAL + 7 HIGH + 5 MEDIUM + 7 LOW =
30 total violations. (The synthesis SYNTHESIS-AUDIT-OVERFIT names "30
Lock 14 violations" identically; v3 §8 explicitly increases the CRITICAL
tier by +8 hand-written provider modules over the codex audit's
under-count.) The recurrence vector is the 8 per-grammar provider
modules in `skinny/crates/codegen/` (v3 §8 Tier 1 CRITICAL bullet 3 —
provider modules) + the `RuntimeProvider` enum
(`skinny/crates/codegen/src/grammar_profile.rs:17–26`) +
`runtime/src/lib.rs:3–44` hardcoded `#[path = ...]` declarations + the
totality Pattern H 64 hand-written runtime files under
`crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf,
bnf, math}/`.

Reopen path: R3 PRUNE-3 (collapse 8 per-grammar provider modules into
ONE grammar-agnostic generator + replace `RuntimeProvider` enum with
trait-based dispatch + refactor `runtime/src/lib.rs` to consume
workspace metadata) + R3 PRUNE-4 (totality Pattern H — 8 sub-waves to
xtask-emit all 64 files). Acceptance falsifiability per
SYNTHESIS-AUDIT-OVERFIT PRUNE-4: `find crates/core/src/runtime -mindepth
1 -maxdepth 1 -type d` returns ZERO per-grammar dirs.

### S-4. CSS regen pipeline + production corpora (R4 + R5)

Per v1 §1 Claim 3 CONFIRM (no `regen-css` xtask) and §1 Claim 4 CONFIRM
(no production corpora). The 15 `.bbnf` grammars (V-8) are present but
unwired — making them load-bearing IS the R4 candidate.

### S-5. Per-iteration equality oracle (R2)

Per v2 §3.3 / §4.3 (JSON) and v6 §4 (CSS): every existing oracle runs
once at startup. None runs inside the timing region. R2 promotes the
oracle into the per-iter path on every plane.

### S-6. R10 indefatigable close — full bracket

Per ORCHESTRATOR-PROMPT.md R10: SK-V14 closes only when (a) every R1–R9
target holds AND every JSON cell + every CSS feature ADMITs >
strict-vs-strict OR carries architectural-level intrinsic-block proof,
or (b) measured fixpoint per row + explicit user re-pin. No earlier
close is contract-valid.

## §6 — Net Ledger (post-audit)

| Status | Count | Items |
|---|---:|---|
| VALIDATED (carries forward) | 8 | V-1 W5, V-2 W6, V-3 W7, V-4 bbnf-simd, V-5 OffsetFlags + Tape, V-6 generated_json::parse_direct, V-7 generated_real_typed::parse_*, V-8 15 CSS `.bbnf` grammars |
| INVALIDATED (claimed admit; audit-falsified) | 4 row-groups (≈ 42 row-instances) | I-1 25 CSS rows, I-2 5 JSON parse_only, I-3 4–5 JSON direct, I-4 7–10 JSON typed |
| DEMOTED (claimed wired; cosmetic) | 3 | D-1 W8 per-grammar policy, D-2 W9 same-substrate union, D-3 SK-V12 W1b CSS admit (folded into I-1) |
| STILL-OPEN (SK-V14 candidate) | 6 cohorts | S-1 51 JSON cells, S-2 24 CSS features, S-3 30 Lock 14 violations, S-4 R4+R5 pipeline, S-5 R2 per-iter oracle, S-6 R10 close |

Audit-zero rolling delta (restated, binding for SK-V14 open):

- JSON `parse_only`: **0 ADMITTED / 17**.
- JSON `direct_to_struct`: **0 ADMITTED / 17**.
- JSON `real_typed_struct`: **0 ADMITTED / 17**.
- CSS L4: **0 ADMITTED / 24**.

The campaign carries 8 VALIDATED pillars and is otherwise at zero. The
SK-V14 wave program follows the prune ledger first, then re-baseline
under rebound comparators + per-iter equality + grammar-derived CSS
pipeline + production corpora, then re-attempt every cell against the
R10 close bar.
