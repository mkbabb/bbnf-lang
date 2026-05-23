# Alpha-C Redress Digest for SK-V14

Date: 2026-05-22.
Role: alpha-C for SK-V14 Pass Alpha bracket, V1 cycle.
Authority read: `restart/prompts/pass-contracts/PASS-ALPHA.md`,
`restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`,
`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`,
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`,
`restart/locks/LOCKS.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`,
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v{1..6}-*.md`,
the SK-V13 alpha-C antecedent at
`restart/skinny/tranches/sk-v13/research/alpha/alpha-C-redress-digest.md`,
and `skinny/REDRESS.md` items 131 — 160 by grep + offset (the file is 5041
lines; only the SK-V13 cycle range is in scope here).

## Binding Interpretation

The audit pack (six S-P0 agents, latest commit `b24232776`) supersedes any
SK-V13 admit claim that the validation files falsify. Where REDRESS.md still
records `ADMIT` for a row whose audit verdict is `ADMIT-FAKE`, the audit
wins: the entry is reclassified DEMOTE-AUDIT and the row reopens.

REDRESS 119 and REDRESS 120 remain HISTORY only under the USER PIN addendum.
The 13-row direct fixpoint table and the SK-V11 close are wave-eligible. A
reopen must cite a fresh material differential AND survive the rebound
strict-vs-strict comparators (R1) AND carry per-iteration equality (R2), OR
record an architectural-level intrinsic-block proof. Implementation-limited
failures are reopens, not closes.

Lock 14 is the recurrence vector. Audit v3 raises the codex tally from 17 to
30 violations (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW); 8 hand-written
per-grammar provider modules in `codegen/src/` carry the breach into SK-V13
and would inherit any new admit. Lock 14 enforcement is bound to PRUNE-3 /
PRUNE-4 (R3) before any new admit attempt.

This digest separates **per-entry SK-V13 dispositions** from **pattern-level
pre-blocks** (P-1 through P-7). A per-entry pre-block bars one shape of one
route; a pattern-level pre-block bars an entire production technique
campaign-wide.

## §1 — Per-entry SK-V13 dispositions

Every SK-V13 REDRESS entry committed in the W3..W15 cycle, classified per the
audit overlay. Citations are `audit-overfit/validation/vN-*.md §X` where the
audit verdict differs from REDRESS's own header.

### CSS L4 admits (W3, W4, W10.1, W10.2, W10.3) — Items 131-135

| REDRESS | Wave | Row(s) | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|---|
| 131 | W3 | declaration_values_extended + 4 grouped CSS features | `PASS-ADMIT-CANDIDATE` (265.72 Mbps vs 54.91 lightningcss) | **ADMIT-FAKE** per v1 §5 row 2-6: hand-written template `include_str!()`'d; no .bbnf consumed; no `regen-css` xtask | DEMOTE-AUDIT, DELETE per P-1; revert all 5 covered features (`declarations`, `css_variables`, `calc_expressions`, `var_url_functions`, `color_functions`) |
| 132 | W4 | visual_functions + 3 grouped CSS features | `PASS-ADMIT-CANDIDATE` (225.89 Mbps vs 114.53 lightningcss) | **ADMIT-FAKE** per v1 §5 rows 12-15: same template pattern; 357-byte fixture | DEMOTE-AUDIT, DELETE per P-1; revert all 4 covered features (`gradients`, `transforms`, `filters`, `easing_functions`) |
| 133 | W10.1 | at_rules_and_media + 1 grouped feature | `PASS-ADMIT-CANDIDATE` (21,584 Mbps vs 253.22 lightningcss; 85× anomaly) | **ADMIT-FAKE** per v1 §5 rows 16-17: 85-byte fixture, Mbps inflation per P-3 | DEMOTE-AUDIT, DELETE per P-1+P-3; revert 2 features (`at_rules_keyframes`, `media_queries`) |
| 134 | W10.2 | vendor_and_custom_atrules + 1 grouped feature | `PASS-ADMIT-CANDIDATE` (34,635 Mbps vs 277.74 lightningcss; 124× anomaly) | **ADMIT-FAKE** per v1 §5 rows 18-19: 162-byte fixture, Mbps inflation per P-3 | DEMOTE-AUDIT, DELETE per P-1+P-3; revert 2 features (`vendor_prefixes`, `custom_at_rules`) |
| 135 | W10.3 | nested_layout + 4 grouped features | `PASS-ADMIT-CANDIDATE` (52,233 Mbps vs 421.16 lightningcss; **124× anomaly**) | **ADMIT-FAKE + OVERFIT-THROUGHPUT** per v1 §1 Claim 5 + §5 rows 20-24: 351-byte fixture at 53.7 ns/parse, suspected fast-fail/token-only sink | DEMOTE-AUDIT, DELETE per P-1+P-3; revert 5 features (`nested_rules`, `logical_properties`, `grid`, `flexbox`, `typed_property_groups`); **flag as round-trip rule trigger** under future reopen — wave failed in spirit even if header read ADMIT |

Aggregate: 5 wave entries collapse 24 CSS L4 ROLLING-SOTA-DELTA admit rows to
zero. Per memory `[abrogate-before-patch]`, the 7 CSS L4 template files
(`codegen/src/css_l4_*_templates/generated.rs` and siblings) DELETE rather
than PATCH; their `include_str!`'d generated.rs counterparts under
`runtime/src/grammars/css_l4_*/` DELETE alongside. The 15 `.bbnf` files at
`/grammar/css/l4/` (refuted-as-absent per v1 §1 Claim 2) SURVIVE and become
load-bearing under R4 + R5.

The SK-V12 W1b admit (`css_l4/declaration_values/direct_to_struct/main` at
429.34 / 434 Mbps vs 168.93 lightningcss) is ALSO ADMIT-FAKE per v1 §2 (same
template pattern, same provider, no xtask regen between SK-V12 and SK-V13).
It does not appear as a SK-V13 REDRESS item but is reverted in the same
DELETE pass; v1 §5 row 1 is the citation; v5 §1 SK-V12 verdict PARTIAL
confirms.

### Decision-engine fold (W5, W6, W7, W8, W9) — Items 136-140

| REDRESS | Wave | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|
| 136 | W5 bbnf-regex | `PASS-BLOCKED`, no row movement | **LOAD-BEARING confirmed** per v4 §1 + §6: crate exists, consumed by ir + passes + recognizers | SURVIVE; carry forward as architectural pillar |
| 137 | W6 e-graph | `PASS-BLOCKED`, no row movement | **LOAD-BEARING confirmed** per v4 §1 + §6: active-cost extraction wired; `CostFacts.chosen` reaches lowering | SURVIVE; carry forward; extraction-only (zero rewrites) is by-spec |
| 138 | W7 CSP cascade | `PASS-BLOCKED`, no row movement | **LOAD-BEARING confirmed** per v4 §2 + §6 + §8: solver instantiated, 5 constraints; lowering fail-closed at `passes/lib.rs:476-478` and `lower/rust.rs:47-68` | SURVIVE; carry forward; P1-P8 cascade is evidence-only, not selector |
| 139 | W8 per-grammar policy | `PASS-BLOCKED`, no row movement (own `_NOT_CONSUMED_BY_GENERATED_RUNTIME` block ID) | **COSMETIC / SCAFFOLD-ONLY** per v4 §4 + §6: no `struct GrammarConfig`, no policy dispatch, hardcoded constants in CSS config.rs replace dynamic dispatch | DEMOTE-AUDIT; SCAFFOLD; reopen under R3 PRUNE-5 to wire CSP-selected shape to runtime divergence |
| 140 | W9 same-substrate union | `PASS-ADMIT` claiming row strengthening +3.88 Mbps on declaration_values_extended | **COSMETIC / SCAFFOLD-ONLY** per v4 §5 + §6 + §9: no public `UnionTape`, no substrate API; the "union projection" is a hardcoded `UNION_PROJECTION_NORMALIZED_ASCII` vs `UNION_PROJECTION_RAW_BYTES` branch in CSS config | DEMOTE-AUDIT; the +3.88 Mbps claim depends on the underlying W3 row that ITSELF is ADMIT-FAKE per P-1; revert; reopen under R3 PRUNE-5 |

### JSON direct admits (W11.1, W11.2, W11.3, W11.4) — Items 141-143, 159

| REDRESS | Wave | Row | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|---|
| 141 | W11.1 | `json/numbers/direct_to_struct/main` (13,875 vs 12,918 sonic-strict-misnamed) | `PASS-ADMIT` | **REAL parser + comparator misbinding** per v6 §1 row 2 + §2 row 3 + §6 + §7: `sonic_rs::from_slice::<Value>` is eager DOM, not strict direct-struct deser per corpus | REOPEN-AUDIT under R7; parser is REAL (regen-derived from grammar via `cargo xtask regen-json`); rebind comparator to strict per-corpus struct deser; per-iteration equality (R2); ADMIT-ELIGIBLE iff still > strict |
| 142 | W11.2 | object-loop scalar dispatch (twitter / github_events / update_center) | `REJECTED-MEASURED` (rows below sonic strict-misnamed) | **HONEST measured reject** per v6 §6: rejection is sound even with misnamed comparator; reopened under correct comparator the route would need ≥ 2.4-3.2× lift | PRE-BLOCK same dispatch-envelope shape; reopen requires per-shape object-member handling OR sink-stack specialization with measured row movement under R1-rebound comparator |
| 143 | W11.3 | `json/mesh/direct_to_struct/main` (9,631 vs 9,581 sonic-strict-misnamed) | `PASS-ADMIT` | **REAL parser + comparator misbinding** per v6 §1 row 2 + §6; margin only +50 Mbps — most likely to demote under R1 rebind | REOPEN-AUDIT under R7; FRAGILE; if rebound sonic strict struct deser exceeds 9,631 the row demotes; otherwise re-admits under R1+R2 |
| 159 | W11.4 | direct cursor byte-fetch (twitter / github_events / update_center) | `REJECT` | **HONEST measured reject** per v6 §6 + cross-tranche v5 §1 SK-V13 timeline: rejection is sound; the byte-fetch route adds no headroom | PRE-BLOCK same byte-fetch helper shape; reopen requires fresh hot-leaf attribution OR substrate change |

### SK-V12 carry-over JSON direct guards (not new SK-V13 REDRESS but cross-validated)

Per v2 §3 the 4 SK-V12 direct guards (`citm_catalog`, `apache_builds`,
`numbers`, `unicode_basic`) read `ADMIT-HOLDS` against the eager-typed
comparator on the same plane. Per v6 §1 row 2 + §2 row 3 the comparator is
misbound (`sonic_rs::from_slice::<Value>` not per-corpus struct deser). The
parsers are REAL (`generated_json::parse_direct` regen-derived from
`grammars/json.bbnf`). SK-V14 disposition: REOPEN-AUDIT under R7; the
honest baseline §1 bind reads 4 direct admits as DOES-NOT-SURVIVE on
comparator-misbinding grounds.

### W12 SIMD/ASM production wiring — Item 144

| REDRESS | Wave | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|
| 144 | W12 | `PASS-ADMIT` claiming `bbnf_simd::find_ascii_set_member64` consumed by CSS L4 declaration-values `Scanner::scan_block`; +109.87% throughput | **DOWNSTREAM-FALSIFIED** per the W1b underlying row being P-1 fake (v1 §2): the production consumer is the hand-written template; production wiring claim depends on a template that DELETE per P-1 | DEMOTE-AUDIT; the SIMD primitive ITSELF is grammar-neutral and survives per v3 §4 (bbnf-simd 52 files); the production-consumer claim does not; reopen under R4 (regen-css pipeline) with a real generated CSS consumer |

### JSON typed admits (W13.1, W13.2, W13.3, W13.4, W15.1) — Items 145-148, 160

| REDRESS | Wave | Row | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|---|
| 145 | W13.1 | `json/numbers/real_typed_struct/main` | `PASS-ADMIT` | **REAL parser + comparator misbinding** per v6 §1 row 3 + §2 row 4 + §7: comparator is `sonic_rs::from_slice::<TypedT>` per-row, which IS the right plane per v2 §4.2 BUT v6 §7 disputes whether it's per-corpus correct | REOPEN-AUDIT under R7; comparator is closer to correct than the parse_only/direct cases; FRAGILE; verify per-corpus typed match |
| 146 | W13.2 | `json/unicode_basic/real_typed_struct/main` | `PASS-ADMIT` | Same as 145 | REOPEN-AUDIT under R7 |
| 147 | W13.3 | `json/random/real_typed_struct/main` | `PASS-ADMIT` | Same as 145 | REOPEN-AUDIT under R7 |
| 148 | W13.4 | `json/instruments/real_typed_struct/main` | `PASS-ADMIT` (margin +4,989 Mbps over threshold) | Same as 145; margin is large and likely survives rebind | REOPEN-AUDIT under R7; MOST LIKELY to survive |
| 160 | W15.1 | `json/update_center/real_typed_struct/main` | `ADMIT` (margin +621 Mbps) | Same as 145 | REOPEN-AUDIT under R7; comparator-misbinding suspect per v6 §7 |

### JSON typed measured rejects (W13.5, W13.6, W13.7, W13.8) + correctness reject (W13.9) — Items 149-153

| REDRESS | Wave | Row | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|---|
| 149 | W13.5 | `json/gsoc-2018/real_typed_struct/main` (Track 1 6,053 missed sonic+1 = 6,818 by 765 Mbps) | `MEASURED-REJECT` | **HONEST measured reject** per v5 §"Honest patterns left clean" | PRE-BLOCK same map-entry product shape; round-trip rule applies — second in-tranche reopen requires user re-pin |
| 150 | W13.6 | `json/unicode_mixed/real_typed_struct/main` (missed by 291 Mbps) | `MEASURED-REJECT` | **HONEST measured reject** per v5 | PRE-BLOCK same shape; round-trip applies |
| 151 | W13.7 | `json/y_string_unicode/real_typed_struct/main` (missed by 185 Mbps) | `MEASURED-REJECT` | **HONEST measured reject** per v5 | PRE-BLOCK same shape; round-trip applies |
| 152 | W13.8 | `json/unicode_escapes/real_typed_struct/main` (missed by 488 Mbps) | `MEASURED-REJECT` | **HONEST measured reject** per v5 | PRE-BLOCK same shape; round-trip applies |
| 153 | W13.9 | `json/canada/real_typed_struct/main` | `CORRECTNESS-REJECT` (one-ULP f64 rounding mismatch ring 0, point 4, coord 1) | **HONEST correctness reject** per v5 | PRE-BLOCK same naive f64 materializer; reopen requires exact bit-preserving materialization |

### JSON parse-only admits (W14.1, W14.2, W14.3, W14.4, W14.5) — Items 154-158

| REDRESS | Wave | Row | REDRESS verdict | Audit overlay | SK-V14 disposition |
|---|---|---|---|---|---|
| 154 | W14.1 | `json/numbers/parse_only/main` | `ADMIT` | **ADMIT-FAKE** per v2 §1 + v6 §2 row 1: zero parser/codegen diff (gate.rs + report.rs + lock14_baseline.rs only); comparator misnamed per P-2 | DEMOTE-AUDIT, REVERT per R3 PRUNE-1; reopen under R8 only after distinct parse_only path lands |
| 155 | W14.2 | `json/citm_catalog/parse_only/main` | `ADMIT` | Same as 154 | DEMOTE-AUDIT, REVERT per R3 PRUNE-1 |
| 156 | W14.3 | `json/canada/parse_only/main` | `ADMIT` | Same as 154 | DEMOTE-AUDIT, REVERT per R3 PRUNE-1 |
| 157 | W14.4 | `json/marine_ik/parse_only/main` | `ADMIT` | Same as 154 | DEMOTE-AUDIT, REVERT per R3 PRUNE-1 |
| 158 | W14.5 | `json/mesh/parse_only/main` | `ADMIT` | Same as 154 | DEMOTE-AUDIT, REVERT per R3 PRUNE-1 |

### Disposition summary

| Class | Entries | Count |
|---|---|---|
| SURVIVE — architectural pillar | 136 (W5), 137 (W6), 138 (W7) | 3 |
| DEMOTE-AUDIT, DELETE per P-1 | 131-135 (CSS L4 templates) | 5 |
| DEMOTE-AUDIT, REVERT per P-4 / PRUNE-1 | 154-158 (parse_only) | 5 |
| DEMOTE-AUDIT, SCAFFOLD per P-5 | 139 (W8), 140 (W9), 144 (W12) | 3 |
| REOPEN-AUDIT under R7 (real parser, comparator rebind) | 141 (W11.1), 143 (W11.3), 145-148 (W13.1-W13.4), 160 (W15.1) | 7 |
| HONEST measured reject — PRE-BLOCK same shape | 142 (W11.2), 159 (W11.4), 149-153 (W13.5-W13.9) | 7 |

Total SK-V13 REDRESS items dispositioned: 30 (items 131-160).

## §2 — Pattern-level pre-blocks (P-1 through P-7)

The audit pack discloses production techniques the individual REDRESS entries
do not yet capture. Each pre-block bars the technique campaign-wide; a new
admit attempt may not invoke any of these without architectural-level proof
of escape.

### P-1 — Fake `@generated` header on hand-written templates

**Citation:** v1-css-l4-validation §1 Claim 1 + §6; SYNTHESIS-AUDIT-OVERFIT
§"CSS L4 — fake admissions".

**Pattern:** A provider module in `codegen/src/` wraps `include_str!()` of a
hand-written `.rs` file with a `// @generated by skinny bbnf-codegen; do not
edit by hand.` header. The header is intentional, added by the `render()`
function. No grammar input is consumed; no IR is lowered; no xtask regen
exists. All 7 CSS L4 providers and the SK-V12 W1b provider use this
technique. The pattern collapses 25 nominal admits (24 SK-V13 + 1 SK-V12
carry).

**SK-V14 binding:** Any file under `crates/runtime/src/grammars/{grammar}/`
whose existence cannot be reproduced by `delete generated → run xtask
regen-{grammar} → diff produces empty` is forbidden. Before any new admit
attempt the round-trip test must pass. R4 (regen-css pipeline consuming the
15 `.bbnf` files at `/grammar/css/l4/`) lifts CSS L4 from the pre-block;
absent R4 every CSS L4 admit attempt fails at the gate.

**Falsifiability:** `find crates/core/src/runtime crates/runtime/src
skinny/crates/runtime/src -name '*.rs' -exec grep -l '@generated' {} \; |
xargs -I {} sh -c 'echo {}; xtask round-trip {}'` returns ZERO mismatches; OR
`git grep -l '@generated' crates/runtime` returns only files the round-trip
xtask emits.

### P-2 — `sonic_rs::from_slice::<Value>` mislabelled as strict comparator

**Citation:** v6-comparator-integrity §1 + §2 (5 misnaming violations) + §3 +
§7; SYNTHESIS-AUDIT-OVERFIT §"JSON parse_only — gate-relabel only";
v2-json-validation §2.

**Pattern:** A single sonic-rs binding (`sonic_rs::from_slice::<sonic_rs::
Value>(black_box(&fixture.bytes))`) serves as comparator for three planes
(`parse_only`, `direct_to_struct`, `real_typed_struct`). The API is eager
DOM deserialization; it cannot serve as the strict comparator for any of the
three planes. sonic-rs v0.5.8 exposes no `Skipper` API. The pattern collapses
all 5 SK-V13 parse_only admits (W14.1-W14.5), reframes the 4 SK-V12 carry
direct admits, and partially reframes the 7 typed admits.

**SK-V14 binding:** One sonic-rs binding per plane. Per R1:
- `parse_only` → `sonic_rs::Skipper` or custom structural-skip wrapper.
- `direct_to_struct` → sonic-rs strict struct deserialization per corpus
  (per-fixture `from_slice::<T>` where `T` is the per-corpus struct).
- `real_typed_struct` → per-corpus typed struct deserialization (extends the
  existing W13/W15 typed wrappers across all 17 corpora).

No row admits while its plane's comparator is misbound. The bench-harness
gate (`xtask gate-json`) must reject any row whose comparator column does not
name the per-plane binding.

**Falsifiability:** `grep -n 'sonic_rs::from_slice::<sonic_rs::Value>'
skinny/crates/bbnf-bench/benches/*.rs` returns ZERO in `parse_only` and
`direct_to_struct` benchmark groups; `direct_to_struct` and
`real_typed_struct` invocations name per-corpus struct types.

### P-3 — Tiny-fixture Criterion-overhead Mbps inflation (< 400 bytes)

**Citation:** v1-css-l4-validation §1 Claim 5 + §4 + §6; SYNTHESIS-AUDIT-
OVERFIT §"CSS L4 — fake admissions" headline 124× anomaly.

**Pattern:** A fixture under 400 bytes parses in tens of nanoseconds, where
Criterion measurement overhead is a meaningful fraction of the measured
time. Throughput claims at this scale are inflated by clock granularity, not
parser speed. All 7 SK-V13 CSS L4 fixtures are 85-357 bytes; nested_layout
(351 B, 53.7 ns/parse, claimed 52,233 Mbps = 124× lightningcss) is the
extreme case but vendor_prefixes (162 B, claimed 34,635 Mbps), at_rules_and
_media (85 B, claimed 21,584 Mbps), and stylesheet_root (117 B, claimed
26,894 Mbps) display the same artefact.

**SK-V14 binding:** No CSS row admits on a fixture < 1 KB. R5 (production
corpora at `skinny/corpora/css-l4-sk-v14/` — Bootstrap + Tailwind + Material
+ Animate ~960 KB) is the bound. Sub-1-KB fixtures may appear as
correctness oracles but never as throughput admits.

**Falsifiability:** Any CSS L4 RESULTS row whose corpus is below 1024 bytes
is rejected by `xtask gate-json`; per-row corpus byte count is a required
column in the bench schema.

### P-4 — Gate-relabel as admit

**Citation:** v2-json-validation §1 (W14.1-W14.5 diff stats: only `gate.rs`,
`report.rs`, `lock14_baseline.rs`, `main.rs` touched; zero parser/codegen
files) + §2 + §6.1 + §7; SYNTHESIS-AUDIT-OVERFIT §"JSON parse_only —
gate-relabel only"; v5 §2 timeline (SK-V13 W14 is the first appearance).

**Pattern:** A wave moves a row from S/NO-GO to A/GO by changing the gate
predicate, the row table, the report schema, or the lock14 baseline — not by
changing the parser. The diff signature is zero touches under
`crates/runtime/src/grammars/{grammar}/` or `crates/codegen/src/{grammar}_
templates/`, and substantial touches under `bbnf-bench/src/gate.rs`,
`report.rs`, `lock14_baseline.rs`. Combined with P-2, the pattern produced 5
nominal parse_only admits with zero parser changes.

**SK-V14 binding:** A wave that does not touch the parser cannot claim a row
admit. The xtask gate-json command must compute a per-wave parser-touch
indicator (diff stat across runtime/codegen) and reject any admit whose
indicator is zero. Per memory `[no-deferrals]`, gate / report plumbing
changes ride alongside the parser change in one commit; they do not stand
alone as admit-bearing waves.

**Falsifiability:** For every wave with an ADMIT in the RESULTS row table,
`git diff --stat <wave-base>..<wave-tip> -- crates/runtime/src
crates/codegen/src skinny/crates/runtime/src skinny/crates/codegen/src` shows
non-zero additions in the runtime or codegen tree.

### P-5 — Scaffold-research counted as load-bearing

**Citation:** v4-decision-engine-trace §4 (W8) + §5 (W9) + §6 verdict table
+ §9 "most surprising finding"; SYNTHESIS-AUDIT-OVERFIT §"Decision-engine
fold — partial".

**Pattern:** A wave admits a research artefact (per-grammar policy facts,
union substrate facts) without wiring runtime consumers. The CSP solver
picks shapes; the generated runtime ignores the selection and defaults to
hardcoded hand-written behaviour. W8 has its OWN block ID admitting this
(`JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT`) yet
nonetheless was permitted to close. W9 closes as `PASS-ADMIT` claiming +3.88
Mbps on declaration_values_extended, where the underlying row is itself
ADMIT-FAKE per P-1 (the "union projection" is a hardcoded conditional
constant per v4 §9, not a substrate).

**SK-V14 binding:** A wave admits as load-bearing only when the runtime
diff demonstrates measurable runtime divergence on a named pre-wave row
contingent on the scaffold's selection. R3 PRUNE-5 binds this: W8 + W9 wire
to load-bearing OR delete. Scaffold-only research artefacts may exist in
`restart/skinny/tranches/sk-vN/research/` but do not move RESULTS rows.

**Falsifiability:** For every wave claiming a load-bearing scaffold,
disabling the scaffold (e.g. forcing CSP to return a fixed shape) must
produce a measurable row delta on the named pre-wave row.

### P-6 — Per-grammar provider modules in generic codegen

**Citation:** v3-lock14-deep-scan §1 (3 CRITICAL + 4 HIGH confirmed) + §5 +
§7 Gap 1 (8 hand-written backends miscounted as 1 enum violation) + §8 (30
total violations, codex under-counted by 13); SYNTHESIS-AUDIT-OVERFIT
§"Skinny Lock-14 — 17 violations" + §"Totality Pattern H — 64 hand-written
runtime files".

**Pattern:** Eight files under `skinny/crates/codegen/src/` (`json_provider.
rs` plus seven `css_l4_*_provider.rs`) hand-code grammar-specific render
logic in a nominally-generic codegen crate. Each provider hardcodes its
provider-id string, its template paths, and its render branches. The
`RuntimeProvider` enum at `codegen/src/grammar_profile.rs:17-26` bakes 8
grammar variants at the type level. The `runtime/src/lib.rs:3-26 + :35-44`
hardcodes 8 grammar module paths and namespace re-exports. The same pattern
recurs in totality at `crates/core/src/runtime/{json, css_l4, google_sheets,
bbnf, csv, ebnf, bnf, math}/` — 64 hand-written per-grammar runtime files
that Lock 14 names verbatim as "the failure mode this lock prevents from
recurring" (`restart/locks/LOCKS.md:220-238`).

**SK-V14 binding:** No new admit attempt may add a per-grammar provider
module. R3 PRUNE-3 + PRUNE-4 are bound:
- Replace `RuntimeProvider` enum with trait-based dispatch.
- Collapse 8 per-grammar provider modules into ONE grammar-agnostic
  generator template consuming grammar source + workspace metadata.
- Refactor 64 hand-written per-grammar files in `crates/core/src/runtime/
  {grammar}/` into emitted output (sub-divided by grammar; 8 sub-waves).
- `runtime/src/lib.rs` grammar module paths and namespace re-exports
  generated from workspace metadata.

**Falsifiability:** `find skinny/crates -name '*.rs' | xargs grep -l
'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO
post-redress; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type
d` returns ZERO per-grammar directories.

### P-7 — Track-1 ≡ Track-2 dishonesty

**Citation:** Cross-referenced from Lock 1 in `restart/locks/LOCKS.md`
(parity track separation); SYNTHESIS-AUDIT-OVERFIT §"Honest patterns left
clean" (W11.2 + W13.5-9 rejects honest because Track 2 stayed independent;
the converse pattern, Track 2 borrowing Track 1 internals, would have
produced false ADMITs).

**Pattern:** A wave admits a row where Track 2 (independent oracle) shares
state, sink type, or callback machinery with Track 1 (generated parser). The
measurement becomes Track-1-vs-itself dressed as Track-1-vs-oracle. The
audit explicitly clears SK-V13 of this pattern — Track 2 oracles remained
independent across all measured waves. The pre-block carries forward as
prophylaxis: any wave that shares state between tracks fails the comparator
gate before measurement.

**SK-V14 binding:** Track 1 and Track 2 must instantiate distinct sink
types, distinct buffer ownership, distinct dispatch paths. The bench
harness must invoke them through separate API entry points. Per Lock 1, no
renamed-scanner / shared-cursor / shared-tape implementation crosses the
parity boundary.

**Falsifiability (triple check per CH5 §2 REVISE #4, against `restart/locks/LOCKS.md:73-82`):**
Per-wave audit (S-P0 lens) inspects the bench harness for shared mutable
state between Track 1 and Track 2 invocations; any shared sink, shared
buffer, or shared dispatcher fails the gate. The gate composes three
mechanically independent checks — symbol identity, type identity, address
identity — because each closes a slip the other two cannot detect.

(a) **Distinct symbol paths.** xtask `gate-json` reads a per-row column
naming Track 2's distinct entry point and rejects rows where Track 1 and
Track 2 entry points share a symbol. This closes the shared-dispatcher
pattern (one symbol invoked twice) but admits the renamed-scanner slip per
the CH5 charge at `CHALLENGE-CONTEXT.md:160` — a fresh symbol path may still
funnel into the same buffer.

(b) **Distinct concrete `Sink` types (compile-time `TypeId` inequality).**
The bench harness asserts `TypeId::of::<Track1::Sink>() != TypeId::of::<Track2::Sink>()`
inside a `const _: () = assert!(...);` form (or `static_assertions::assert_type_ne_all!`
where const-context is unavailable). This rejects at compile time any
shape where Track 2 borrows Track 1's concrete sink under a fresh symbol —
the type-identity check is orthogonal to the symbol-path check and catches
the same-type-different-wrapper smuggle.

(c) **Distinct buffer addresses at the first bench iter.** The per-iter
equality column (per R2 of the SK-V14 close conditions) records
`Track1::tape() as *const _ as usize` and `Track2::tape() as *const _ as usize`
on iter 0; xtask `gate-json` rejects any row where the two addresses are
equal. Address inequality is the runtime correlate of buffer-ownership
inequality and detects shared `OffsetFlags` / shared `Tape` instances even
when both symbol path and concrete type pass. Per Lock 1
(`LOCKS.md:73-82`), Track 2's tape must declare `substrate_target ∈
{existing_tape, direct_sink, admitted_fact_output}` distinct from Track 1's
declaration; the address check is the mechanical correlate.

All three checks must pass per row; any one failing rejects the row at
`xtask gate-json` time before any admit attempt. The triple closes the
renamed-shared-buffer pattern Lock 1's "no renamed-scanner / shared-cursor
/ shared-tape implementation" clause names verbatim (`LOCKS.md:80-81`).

## §3 — Pattern-level summary

| Pre-block | Bars | Lifted by | Round-trip-eligible |
|---|---|---|---|
| P-1 | Fake `@generated` on hand-written templates | R4 regen-css pipeline + R5 production corpora | YES under round-trip xtask gate |
| P-2 | sonic_rs::from_slice::<Value> as strict comparator | R1 three-plane comparator rebind | YES per plane |
| P-3 | < 400 B fixture Mbps inflation | R5 production corpora (~960 KB target) | NO; corpus byte count is a permanent floor |
| P-4 | Gate-relabel as admit (parser-touch indicator = 0) | R3 PRUNE-1 revert + R8 distinct parse_only path | YES iff parser touched |
| P-5 | Scaffold research counted as load-bearing | R3 PRUNE-5 wire W8 + W9 OR delete | YES iff runtime divergence on named row |
| P-6 | Per-grammar provider modules in generic codegen | R3 PRUNE-3 + PRUNE-4 trait dispatch + emit | YES iff zero per-grammar files in generic crates |
| P-7 | Track-1 ≡ Track-2 shared state | Lock 1 enforcement at bench-harness audit | YES iff distinct entry points + buffers |

## §4 — Reopen obligations for SK-V14 S-P3

S-P3 SPEC.md encodes these as wave-gate obligations (R3-R8 territory):

- **PRUNE waves first.** Per R3, PRUNE-1 (revert W14.1-W14.5), PRUNE-2
  (delete 7 CSS L4 template files + 7 corresponding `include_str!`'d
  generated.rs files; revert all 24 + 1 SK-V12 CSS rows; cite v1 §1-§6 per
  row), PRUNE-3 (Lock 14 CRITICAL + HIGH refactor; cite v3 §1 + §7), PRUNE-4
  (totality Pattern H 8-sub-wave refactor; cite v3 §9 + SYNTHESIS §"Totality
  Pattern H"), PRUNE-5 (W8 + W9 wire-to-load-bearing or delete; cite v4 §4 +
  §5 + §6).

- **Comparator rebind before any new admit.** R1 binds the three plane-
  correct comparators. R2 binds per-iteration equality inside the timing
  region. Until R1 + R2 land, every admit attempt fails at the harness gate.

- **CSS L4 re-admit only after R3 + R4 + R5.** R6 re-attempts each CSS L4
  row via the grammar-derived pipeline against real corpora with work-
  equivalent comparators (lightningcss full-parse; no fact-stream vs full-
  AST asymmetry per v6 §5 + §7).

- **JSON direct + typed re-baseline after R1 + R2.** R7 re-measures the 4
  direct + 7 typed previously-admitted rows; cells previously holding under
  misbound comparators hold again under correct comparators OR are reverted.

- **JSON parse_only distinct path before re-admit.** R8 stands up a parse_
  only code path in `generated_json` with no full-tape build, wired to
  `Skipper`-style comparator. Then admit attempts may proceed.

- **Round-trip rule.** W10.3 nested_layout (124× anomaly) triggers the
  round-trip rule preemptively under v1 §1 Claim 5. Any second-in-tranche
  reopen of nested_layout requires user re-pin with intrinsic-block evidence.

- **Indefatigability.** Per USER-PIN-ADDENDUM A4 + memory `[no-deferrals]`,
  no row is OUT_OF_SCOPE absent architectural-level intrinsic-block proof.
  The 13 N-direct rows from REDRESS 119 are wave-eligible. The 10 rows
  outside the SK-V13 reopen shortlist (canada, mesh, random, gsoc-2018,
  instruments, numbers, unicode_mixed, unicode_escapes, distinct_values,
  y_string_unicode) are EQUALLY reopen-eligible under W5-W9 + W11-W15
  outputs.

## §5 — Closing posture

SK-V13 produced one substantive architectural advance (W5-W7 decision-engine
fold, LOAD-BEARING per v4 §1 + §6) and one substantive scaffold debt (W8 +
W9 per v4 §4 + §5). The 25 nominal CSS admits and 5 nominal parse_only
admits collapse under the audit. The 4 direct + 7 typed SK-V12 carries +
SK-V13 typed admits hold their PARSERS but lose their COMPARATORS pending
R1 rebind. The Lock 14 violation count is 30, not 17, with 8 per-grammar
provider modules in `codegen/src/` as the recurrence vector.

The SK-V14 redress posture is therefore: PRUNE first (R3), REBIND second
(R1+R2), REBUILD third (R4+R5+R6+R7+R8). No new admit attempt precedes the
prune waves. Every admit attempt carries per-iteration equality and a
plane-correct strict comparator. Per memory `[abrogate-before-patch]`, the
hand-written CSS L4 templates DELETE; per memory `[no-workarounds]`, the W8
+ W9 scaffold either wires or deletes; per memory `[execute-planned-
architecture]`, Lock 14 PRUNE-3 + PRUNE-4 do not retreat under contact.

REDRESS 119 + 120 remain HISTORY only. The audit-zero rolling delta (JSON
parse_only 0/17, JSON direct 0/17, JSON typed 0/17, CSS L4 0/24) is the
true SK-V14 starting baseline. The campaign is at zero on numbers; non-zero
on architecture. The work between here and inflection is what SK-V14 owns.
