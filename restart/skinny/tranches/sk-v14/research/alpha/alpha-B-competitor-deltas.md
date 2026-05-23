# α-B Competitor Deltas (Rebound-Comparator Overlay) — SK-V14

Scope per `restart/skinny/tranches/sk-v14/research/alpha/DISPATCH-CONTEXT.md`
§α-B: per-comparator per-corpus deltas vs bbnf Track 1, with the SK-V14
rebound-comparator overlay that classifies every historic delta as
HONEST, SUSPECT, or COMPARATOR-PENDING-R1 / CORPUS-PENDING-R5 /
PIPELINE-PENDING-R4 against the audit-bound baseline.

## §0 — Bound baseline (do not re-litigate)

The honest baseline at `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:71-77`
fixes the campaign rolling delta at JSON parse_only 0/17, JSON direct
0/17, JSON typed 0/17, CSS L4 0/24. Every "ADMITTED" row in
`restart/skinny/ROLLING-SOTA-DELTA.md:13-93` is either AUDIT-FALSIFIED
or AUDIT-SUSPECT under the rebound-comparator rule. This document
restates per row × plane × comparator what is measurable today (the
misbound number), what is NOT measurable until R1/R4/R5 land, and
which historic deltas survive once the comparators are rebound.

## §1 — JSON comparator inventory at HEAD

### §1.1 — Per-plane comparator binding (single source of truth)

| Plane | Comparator-label in `benches/json_parity.rs` | Actual work | Honest-plane-equivalent comparator needed | Citation |
|---|---|---|---|---|
| `parse_only` | `sonic_rs_anchor` → `sonic_rs::from_slice::<sonic_rs::Value>(bytes)` | eager DOM deserialize + allocation + Value tree construct | `sonic_rs::Skipper` (structural skip), absent from v0.5.8 public API | `benches/json_parity.rs:87-102` per `v6-comparator-integrity.md:46-51` |
| `direct_to_struct` | `sonic_rs_direct_to_struct` → `bbnf_bench::direct_struct::sonic_digest(bytes)` → `sonic_rs::from_slice(bytes)` | eager DOM Value, then digest projection — same surface as parse_only, just wrapped | per-corpus strict struct deserialization via `sonic_rs::from_slice::<CorpusStruct<'a>>` | `bbnf-bench/src/direct_struct.rs:427-429` per `v6-comparator-integrity.md:14-18` |
| `real_typed_struct` | `sonic_rs_real_typed_struct` → match on `RealTypedFixture` → `sonic_rs::from_slice::<TwitterSearch<'a>>` (etc.) | per-corpus typed deser ALREADY (one of the three planes the comparator is plane-correct on) | already correct; the comparator misnaming claim does NOT extend here at the binding level — the SUSPECT vector is per-corpus typed schema parity | `bbnf-bench/src/real_typed_struct.rs:690-731` per `v2-json-validation.md:202-211` |

The audit headline "all three JSON planes use the same eager-DOM
comparator" is exact for parse_only + direct; for real_typed the
binding is per-corpus typed and the misbinding charge is narrower
(the SK-V13 framing of "one sonic_rs::from_slice<Value> for everything"
was the codex undercount; v2 §3.2 / §4.2 surface that direct +
typed have DIFFERENT bindings, both still eager-deser but typed already
matches plane). The SK-V14 R1 rebind is therefore three distinct moves,
not one.

### §1.2 — Comparator availability ledger (campaign-wide JSON)

Counts hold from prior alpha-B SK-V13 capture; R1 rebind does not add
or subtract comparator coverage, only re-binds sonic-rs. The non-sonic
columns are unchanged.

| Comparator | parse_only coverage | direct coverage | typed coverage | Strictness plane | Honest-baseline use |
|---|---:|---:|---:|---|---|
| sonic-rs strict (R1-rebound) | 17 / 17 (pending R1) | 17 / 17 (pending R1) | 7 / 17 (per-corpus typed; 10 corpora lack typed structs) | strict | binding gate |
| sonic-rs lossy | 17 / 17 | 0 | 0 | permissive | flaw-probe only |
| serde_json | 17 / 17 | 17 / 17 | 7 / 17 | strict | reference baseline |
| simdjson DOM | 13 / 17 | 0 | 0 | strict-typed (DOM) | secondary gate |
| simdjson On Demand | 0 / 17 | 0 | 0 | strict | coverage debt |
| yyjson default | 6 / 17 | 0 | 0 | strict | secondary gate |
| yyjson minify | 0 / 17 | 0 | 0 | strict | coverage debt |
| asmjson SWAR | 0 / 17 | 0 | 0 | permissive | coverage debt (flaw probe only) |
| asmjson AVX-512 | 0 / 17 | 0 | 0 | strict (Zen 4 only) | OUT (aarch64) |
| RapidJSON default | 6 / 17 | 0 | 0 | permissive | coverage debt (flaw probe only) |

The non-sonic columns of `parse_only` are unchanged by R1: simdjson DOM
13/17, yyjson 6/17, RapidJSON 6/17 are HONEST today (those comparators
do their own work; the misbinding was on the sonic-rs binding). They
remain honest under R1 but cease to be the binding admission gate
(per addendum A3 + ORCHESTRATOR §R1: sonic-rs strict is the binding
plane, not simdjson DOM).

## §2 — JSON parse_only — per-row rebound overlay (17 rows)

The five SK-V13 W14.1–W14.5 admits are gate-relabel admits per
`v2-json-validation.md:13-25` (zero parser or codegen diffs; commits
touch `gate.rs`/`report.rs`/`lock14_baseline.rs` only). The Mbps in
column "T1 vs sonic-eager" are the historic numbers — what was measurable
under the misbound comparator. Column "T1 vs sonic-Skipper" is the
honest-plane delta, which is COMPARATOR-PENDING-R1 because
`sonic_rs::Skipper` is unavailable in v0.5.8.

| Corpus | T1 Mbps | sonic-eager Mbps (historic misbound) | Δ vs sonic-eager | sonic-Skipper Mbps | Δ vs sonic-Skipper | simdjson DOM Δ | yyjson Δ | SK-V13 verdict | SK-V14 verdict |
|---|---:|---:|---:|---:|---:|---:|---:|---|---|
| twitter | 15561 | 21013 | -25.9% | COMPARATOR-PENDING-R1 | PENDING | -36.5% | -49.7% | OPEN | OPEN |
| citm_catalog | 30150 | 25565 | +17.9% | COMPARATOR-PENDING-R1 | PENDING | +43.9% vs simdjson; -15.8% vs simdjson DOM | -15.8% / +43.9% | ADMITTED (W14.2) | AUDIT-FALSIFIED → OPEN |
| canada | 16977 | 14101 | +20.4% | COMPARATOR-PENDING-R1 | PENDING | +47.7% | +30.6% | ADMITTED (W14.3) | AUDIT-FALSIFIED → OPEN |
| apache_builds | 12767 | 17351 | -26.4% | COMPARATOR-PENDING-R1 | PENDING | -64.6% | -21.6% | OPEN | OPEN |
| github_events | 14966 | 23009 | -35.0% | COMPARATOR-PENDING-R1 | PENDING | -62.2% | -30.2% | OPEN | OPEN |
| update_center | 11791 | 19661 | -40.0% | COMPARATOR-PENDING-R1 | PENDING | -61.5% | -36.4% | OPEN | OPEN |
| mesh | 12987 | 11758 | +10.4% | COMPARATOR-PENDING-R1 | PENDING | +37.9% | n/a | ADMITTED (W14.5) | AUDIT-FALSIFIED → OPEN |
| random | 9946 | 15665 | -36.5% | COMPARATOR-PENDING-R1 | PENDING | -51.8% | n/a | OPEN | OPEN |
| gsoc-2018 | 23587 | 50363 | -53.2% | COMPARATOR-PENDING-R1 | PENDING | n/a | n/a | OPEN | OPEN |
| marine_ik | 12357 | 9902 | +24.8% | COMPARATOR-PENDING-R1 | PENDING | n/a | n/a | ADMITTED (W14.4) | AUDIT-FALSIFIED → OPEN |
| instruments | 17468 | 19630 | -11.0% | COMPARATOR-PENDING-R1 | PENDING | n/a | n/a | OPEN | OPEN |
| numbers | 19267 | 13666 | +41.0% | COMPARATOR-PENDING-R1 | PENDING | n/a | n/a | ADMITTED (W14.1) | AUDIT-FALSIFIED → OPEN |
| unicode_mixed | 9294 | 18858 | -50.7% | COMPARATOR-PENDING-R1 | PENDING | -29.3% | n/a | OPEN | OPEN |
| unicode_escapes | 13550 | 19274 | -29.7% | COMPARATOR-PENDING-R1 | PENDING | -29.7% vs simdjson | n/a | OPEN | OPEN |
| unicode_basic | 12041 | 16126 | -25.3% | COMPARATOR-PENDING-R1 | PENDING | -25.3% | n/a | OPEN | OPEN |
| distinct_values | 9920 | 18161 | -45.4% | COMPARATOR-PENDING-R1 | PENDING | -54.7% | n/a | OPEN | OPEN |
| y_string_unicode | 6590 | 13861 | -52.5% | COMPARATOR-PENDING-R1 | PENDING | -52.7% | n/a | OPEN | OPEN |

Citations: `restart/skinny/ROLLING-SOTA-DELTA.md:14-64` (T1 + sonic-eager
columns); `skinny/RESULTS.md:5-44` (simdjson DOM + yyjson + RapidJSON
deltas); `v6-comparator-integrity.md:32-40` (binding mislabel proof);
`v2-json-validation.md:13-21` (gate-only-diff proof for W14.1-.5).

The five W14.1–.5 historic "admits" all had **positive** Δ vs the
misbound sonic-eager comparator. Under R1 (sonic-Skipper), the
binding admission floor changes: Track 1 builds a full tape; Skipper
builds nothing. **Even if Skipper-Mbps is roughly 1.4–1.6× the eager-Mbps
(structural-skip-only avoids alloc + populate)**, the five admits
would need to clear that bar to survive. None of the five surviving
admit-margins (+41.0 % / +20.4 % / +17.9 % / +24.8 % / +10.4 %) is
likely to clear a 1.4–1.6× rebound. Projection: 0–2 of the 5 historic
admits survive R1, with **the addendum's strict-vs-strict rule
forcing OPEN for any that do not clear**.

## §3 — JSON direct_to_struct — per-row rebound overlay (17 rows)

The SK-V12 carry-over admits (citm_catalog, apache_builds, marine_ik,
numbers, unicode_basic per `ROLLING-SOTA-DELTA.md`; the prior alpha-B
ledger surfaces `marine_ik` + 4 others) are REAL parsers per
`v2-json-validation.md:60-79`; the codepath is
`runtime::generated_json::parse_direct` which IS grammar-derived from
`grammars/json.bbnf` via `cargo xtask regen-json`. The misbinding is
narrower: the sonic_rs comparator at this plane is
`sonic_rs::from_slice(bytes)` returning an eager DOM Value, then the
digest projection runs (`bbnf-bench/src/direct_struct.rs:427-429`),
which is the SAME eager-DOM surface as parse_only. R1 demands a
per-corpus strict struct comparator.

| Corpus | T1 Mbps | sonic-eager-DOM Mbps (historic misbound) | Δ vs sonic-eager | sonic-strict-struct Mbps | Δ vs sonic-strict-struct | SK-V13 verdict | SK-V14 verdict |
|---|---:|---:|---:|---:|---:|---|---|
| twitter | 11908 | 15110 | -21.2% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| citm_catalog | 21414 | 19938 | +7.4% | COMPARATOR-PENDING-R1 | PENDING | ADMITTED | AUDIT-SUSPECT → COMPARATOR-PENDING-R1 |
| canada | 10962 | 12205 | -10.2% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| apache_builds | 11428 | 11105 | +2.9% | COMPARATOR-PENDING-R1 | PENDING | ADMITTED | AUDIT-SUSPECT → COMPARATOR-PENDING-R1 |
| github_events | 12483 | 16197 | -22.9% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| update_center | 8546 | 11183 | -23.6% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| mesh | 9661 | 9757 | -1.0% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| random | 7801 | 8944 | -12.8% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| gsoc-2018 | 15385 | 23880 | -35.6% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| marine_ik | 10513 | 8454 | +24.4% | COMPARATOR-PENDING-R1 | PENDING | ADMITTED | AUDIT-SUSPECT → COMPARATOR-PENDING-R1 |
| instruments | 12060 | 12731 | -5.3% | COMPARATOR-PENDING-R1 | PENDING | ADMITTED | AUDIT-SUSPECT → COMPARATOR-PENDING-R1 |
| numbers | 14125 | 12747 | +10.8% | COMPARATOR-PENDING-R1 | PENDING | ADMITTED | AUDIT-SUSPECT → COMPARATOR-PENDING-R1 |
| unicode_mixed | 5062 | 10654 | -52.5% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| unicode_escapes | 5523 | 14299 | -61.4% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| unicode_basic | 9317 | 8977 | +3.8% | COMPARATOR-PENDING-R1 | PENDING | ADMITTED | AUDIT-SUSPECT → COMPARATOR-PENDING-R1 |
| distinct_values | 6540 | 11949 | -45.3% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |
| y_string_unicode | 5061 | 8999 | -43.8% | COMPARATOR-PENDING-R1 | PENDING | OPEN | OPEN |

Citations: `restart/skinny/ROLLING-SOTA-DELTA.md:15-63`;
`skinny/RESULTS.md:6-44` (Track 1 + sonic Mbps + verdict);
`v2-json-validation.md:139-145` (admit-holds verdict at the WRONG plane);
`v6-comparator-integrity.md:225` (campaign-wide direct-plane comparator
misnaming).

The historic 6 direct admits (citm_catalog, apache_builds, marine_ik,
instruments, numbers, unicode_basic) have margins ranging +2.9 % to
+24.4 %. A per-corpus struct comparator does LESS work than DOM (no
generic Value allocation; populates a typed struct directly), so
sonic-strict-struct will run **faster** than sonic-eager-DOM on most
corpora. The historic margin will compress, possibly invert. Projection:
1–3 of the 6 survive R1; the audit-zero baseline (0 / 17) stands
until R1 + R2 land and re-baselining occurs.

## §4 — JSON real_typed_struct — per-row rebound overlay (7 available rows + 10 MISSING)

Real_typed is the plane where the SK-V13 sonic-rs binding is ALREADY
per-corpus typed (`real_typed_struct.rs:690-731` matches on
`RealTypedFixture` and dispatches `sonic_rs::from_slice::<TwitterSearch<'a>>`
etc.). The misbinding charge is narrower here: the comparator IS on
the typed plane; the SUSPECT vector is whether the per-corpus typed
schemas match Track 1's output schema. Per
`v6-comparator-integrity.md:213-217`: "These still need a typed-struct
comparator, not eager Value DOM. Current sonic_rs binding does NOT
support per-corpus-specific typed structs" — this overstates; per
`v2-json-validation.md:202-211` the per-corpus typed bindings DO
exist for the 7 covered corpora.

| Corpus | T1 Mbps | sonic-typed Mbps (per-corpus, plane-correct) | Δ vs sonic-typed | SK-V13 verdict | SK-V14 verdict |
|---|---:|---:|---:|---|---|
| twitter | 17898 | 15502 | +15.5% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| citm_catalog | 36719 | 22857 | +60.6% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| apache_builds | 8127 | 8091 | +0.4% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| github_events | 13040 | 12627 | +3.3% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| update_center | 13191 | 12623 | +4.5% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| mesh | 9686 | 8867 | +9.2% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| marine_ik | 12164 | 9198 | +32.2% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| instruments | 21464 | 16209 | +32.4% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| numbers | 13281 | 12249 | +8.4% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| random | 8151 | 7393 | +10.3% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| canada | MISSING | MISSING | n/a | MISSING (no typed product surface) | MISSING |
| gsoc-2018 | MISSING | MISSING | n/a | MISSING | MISSING |
| unicode_mixed | MISSING | MISSING | n/a | MISSING | MISSING |
| unicode_escapes | MISSING | MISSING | n/a | MISSING | MISSING |
| unicode_basic | 6753 | 6045 | +11.7% | ADMITTED | AUDIT-SUSPECT → ORACLE-PENDING-R2 |
| distinct_values | MISSING | MISSING | n/a | MISSING | MISSING |
| y_string_unicode | MISSING | MISSING | n/a | MISSING | MISSING |

Citations: `ROLLING-SOTA-DELTA.md:16-58` (T1 + sonic-typed); 
`skinny/RESULTS.md:7-43` (verdict provenance + W6/W13.1/.3/.4/W15.1 admits);
`v2-json-validation.md:249-257` (admit-holds at the binding level under
the plane-correct comparator); `v6-comparator-integrity.md:78-90` (NO
per-iteration equality oracle inside the timing region).

Real_typed comparator-plane is the closest the SK-V13 binding came to
honesty: per-corpus typed deser on both sides. The SUSPECT vector is
NOT comparator misnaming (it is plane-correct) but the missing
per-iteration equality oracle (`assert_real_typed_parity` runs ONCE at
startup, not on the timed path). R2 (per-iteration equality oracle
inside the timing region) is the gate: the 10 listed historic admits
should survive R2 if startup-equality already passes and per-iteration
adds only oracle-call overhead identical to baseline. Projection: 7–10
of the 10 historic typed admits survive R2 with intact margin; the
audit-zero baseline (0 / 17) stands until R2 lands and per-iter equality
columns populate the schema, with 7 MISSING corpora (canada, gsoc-2018,
unicode_mixed/escapes, distinct_values, y_string_unicode, mesh per
some bindings) remaining structurally absent until typed schemas land.

## §5 — CSS L4 — per-row rebound overlay (24 rows)

Per `v1-css-l4-validation.md:217-251` every one of the 24 SK-V13 CSS L4
admits (plus the SK-V12 W1b declaration_values admit) is ADMIT-FAKE:
hand-written templates `include_str!()`-ed under a fake `@generated`
header; the 15 `.bbnf` files at `/grammar/css/l4/` are unwired (no
`cargo xtask regen-css` exists). The lightningcss comparator at
`src/nonjson_css_l4.rs:638` (`StyleSheet::parse(input, ParserOptions::default())`)
does full AST parsing + rule traversal + fact projection — substantially
more work than Track 1's hand-curated `Scanner::new()` + byte-scan
loop (`runtime/src/grammars/css_l4_declaration_values/generated.rs:1-37`).
Tiny fixtures (85–357 bytes) amplify Criterion startup overhead into
inflated Mbps.

| Row | T1 Mbps (historic) | lightningcss Mbps | Δ Mbps | Fixture bytes | SK-V13 verdict | SK-V14 verdict |
|---|---:|---:|---:|---:|---|---|
| css_l4/declaration_values/direct_to_struct/main | 434.13 | 169.23 | +264.90 | 187 (SK-V12 W1b) | ADMITTED (SK-V12 close) | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/declarations/direct_to_struct/main | 265.72 | 55.91 | +209.81 | 305 (W3) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/stylesheet_root/direct_to_struct/main | 26894.88 | 596.05 | +26298.83 | 117 (W2) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/selectors/direct_to_struct/main | 26894.88 | 596.05 | +26298.83 | 117 (W2 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/at_rules_keyframes/direct_to_struct/main | 21584.64 | 254.22 | +21330.42 | 85 (W10.1) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/nested_rules/direct_to_struct/main | 52233.54 | 422.16 | +51811.38 | 351 (W10.3) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/css_variables/direct_to_struct/main | 265.72 | 55.91 | +209.81 | 305 (W3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/calc_expressions/direct_to_struct/main | 265.72 | 55.91 | +209.81 | 305 (W3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/var_url_functions/direct_to_struct/main | 265.72 | 55.91 | +209.81 | 305 (W3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/color_functions/direct_to_struct/main | 265.72 | 55.91 | +209.81 | 305 (W3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/gradients/direct_to_struct/main | 225.89 | 115.53 | +110.37 | 357 (W4) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/transforms/direct_to_struct/main | 225.89 | 115.53 | +110.37 | 357 (W4 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/filters/direct_to_struct/main | 225.89 | 115.53 | +110.37 | 357 (W4 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/easing_functions/direct_to_struct/main | 225.89 | 115.53 | +110.37 | 357 (W4 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/media_queries/direct_to_struct/main | 21584.64 | 254.22 | +21330.42 | 85 (W10.1 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/vendor_prefixes/direct_to_struct/main | 34635.22 | 278.74 | +34356.48 | 162 (W10.2) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/custom_at_rules/direct_to_struct/main | 34635.22 | 278.74 | +34356.48 | 162 (W10.2 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/pseudo_classes/direct_to_struct/main | 26894.88 | 596.05 | +26298.83 | 117 (W2 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/pseudo_elements/direct_to_struct/main | 26894.88 | 596.05 | +26298.83 | 117 (W2 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/attribute_selectors/direct_to_struct/main | 26894.88 | 596.05 | +26298.83 | 117 (W2 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/logical_properties/direct_to_struct/main | 52233.54 | 422.16 | +51811.38 | 351 (W10.3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/grid/direct_to_struct/main | 52233.54 | 422.16 | +51811.38 | 351 (W10.3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/flexbox/direct_to_struct/main | 52233.54 | 422.16 | +51811.38 | 351 (W10.3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |
| css_l4/typed_property_groups/direct_to_struct/main | 52233.54 | 422.16 | +51811.38 | 351 (W10.3 grouped) | ADMITTED | AUDIT-FALSIFIED → CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 |

Citations: `restart/skinny/ROLLING-SOTA-DELTA.md:70-93` (T1 + lightningcss
Mbps); `v1-css-l4-validation.md:217-247` (24-row admit-fake verdict
matrix); `v1-css-l4-validation.md:154-198` (lightningcss work-differential
proof); `v1-css-l4-validation.md:202-215` (fixture-size table); 
`v6-comparator-integrity.md:104-109` (no per-iteration equality oracle).

The 24 historic CSS deltas are uncomputable as competitor deltas in
the SK-V14 sense because (a) Track 1 has no grammar-derived parser
(R4 builds the regen-css pipeline; only then is there a comparable
artefact), (b) the fixtures are 85–357-byte research embeds not
production corpora (R5 lands Bootstrap + Tailwind + Material + Animate),
and (c) the lightningcss comparator does qualitatively different work
(full AST + traversal + fact projection vs Track 1's hand-curated
byte-scan + sidecar facts). The historic "+264.90 Mbps" through
"+51811.38 Mbps" margins are tiny-fixture Criterion-overhead artefacts
on different-plane comparators; they have no SK-V14 successor numbers
until R4 + R5 land. A second-comparator (cssparser oracle) was
historically wired but suffers the identical work-differential.

## §6 — Comparator-availability gap as SK-V14 telemetry debt

The audit-bound state imposes new comparator coverage targets that
SK-V14 R-targets must reach. Recapitulating from §1.2 with R-pointer:

| Plane | Comparator gap to close | R-pointer | Coverage at HEAD | Coverage required for SK-V14 admit gate |
|---|---|---|---:|---:|
| parse_only | sonic-rs Skipper (structural-skip-only) | R1 + R8 | 0 / 17 | 17 / 17 |
| direct_to_struct | sonic-rs strict per-corpus struct deser | R1 + R7 | 0 / 17 | 17 / 17 |
| real_typed_struct | per-iteration equality oracle inside timing region | R2 | 0 / 7 | 7 / 7 (extant) + 10 NEW typed surfaces |
| real_typed_struct | per-corpus typed struct schemas for the 10 MISSING corpora | R7 | 7 / 17 | 17 / 17 |
| CSS L4 | regen-css xtask consuming `/grammar/css/l4/*.bbnf` | R4 | 0 / 24 | 24 / 24 |
| CSS L4 | production corpora `skinny/corpora/css-l4-sk-v14/` ≥ 800 KB | R5 | 0 KB | ~960 KB target |
| CSS L4 | work-equivalent lightningcss comparator (full-parse on both sides) | R6 | 0 / 24 | 24 / 24 |
| CSS L4 | per-iteration equality oracle (currently startup-only) | R2 + R6 | 0 / 24 | 24 / 24 |
| JSON / CSS | simdjson On Demand secondary comparator | (post-R1, SK-V14 wave) | 0 / 17 + 0 / 24 | discretionary; secondary gate |
| JSON / CSS | yyjson minify secondary comparator | (post-R1, SK-V14 wave) | 0 / 17 + 0 / 24 | discretionary; flaw probe |
| JSON | asmjson AVX-512 / SWAR | OUT (aarch64 only) | 0 | 0 (per `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:183`) |

## §7 — Roll-up: HONEST vs SUSPECT vs PENDING per row × plane × comparator

| Plane | HONEST (today, audit-bound) | SUSPECT (historic + audit-flagged) | COMPARATOR-PENDING-R1 | ORACLE-PENDING-R2 | CORPUS/PIPELINE-PENDING-R4+R5+R6 |
|---|---:|---:|---:|---:|---:|
| JSON parse_only (17 rows) | 0 | 5 (the historic W14 admits) | 17 | 0 | 0 |
| JSON direct (17 rows) | 0 | 6 (historic carry-over admits) | 17 | 0 | 0 |
| JSON typed (17 rows; 10 covered + 7 MISSING) | 0 | 10 (historic admits) | 0 | 10 | 7 (MISSING surfaces) |
| CSS L4 (24 rows) | 0 | 24 | 0 | 0 | 24 |
| **Campaign total (75 rows)** | **0** | **45** | **34** | **10** | **31** |

The 0/75 HONEST baseline matches `ORCHESTRATOR-PROMPT.md:71-77`. The
45 SUSPECT rows are the SK-V13 ADMITTED set re-marked under audit;
the comparator-pending dimensions sum to more than 45 because rows
carry multiple PENDING conditions (a JSON typed row is both ORACLE-PENDING-R2
and either ADMITTED-extant or MISSING; a CSS row is both PIPELINE-PENDING-R4
and CORPUS-PENDING-R5 and COMPARATOR-WORK-PENDING-R6).

## §8 — Escalations

- **None new beyond the dispatch context**. The 5 sonic comparator
  misnamings (per `v6-comparator-integrity.md:30-40`) are the binding
  set; the SK-V14 R1 rebind exactly addresses them. No new misnaming
  pattern surfaces from the per-row delta walk in §2–§5.
- **Sonic v0.5.8 Skipper absence is the one architectural-block risk**
  for R1 on the parse_only plane (per `v6-comparator-integrity.md:60`).
  If `sonic-rs` does not expose a Skipper-equivalent API, the
  parse_only comparator gate becomes "custom strict-skip wrapper
  authored in-tree"; this expands R1 scope by ~80 LOC (an in-tree
  wrapper) but does not block the rebind architecturally.
- **The 10 typed-MISSING corpora** (canada, gsoc-2018, unicode_mixed,
  unicode_escapes, distinct_values, y_string_unicode, plus three others
  visible as `absent:product-surface-not-generated` in
  `ROLLING-SOTA-DELTA.md`) are scope expansion for R7 — the typed
  schemas do not exist in `bbnf-bench/src/real_typed_struct.rs`.
  Without R7 those rows cannot enter the typed admit pool regardless
  of comparator rebind.
