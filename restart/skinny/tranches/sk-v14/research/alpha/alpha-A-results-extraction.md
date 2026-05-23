# Alpha-A — Results Extraction (Audit-Overlay) — SK-V14 V1

Pass Alpha brackets SK-V14 against an audit-corrected baseline. This
artefact restates every row of `skinny/RESULTS.md` and
`restart/skinny/ROLLING-SOTA-DELTA.md` under cross-validation against the
six-agent audit pack in
`restart/skinny/tranches/sk-v13/audit-overfit/`. Source authority:
`skinny/RESULTS.md:3-49` (table 1, 45 rendered rows), `:51-131` (W0
telemetry manifest), `restart/skinny/ROLLING-SOTA-DELTA.md:13-93` (51
JSON rows × 3 planes + 24 CSS L4 rows). Comparator-rebind authority:
`v6-comparator-integrity.md §1-7`. CSS template authority:
`v1-css-l4-validation.md §1-5`. JSON gate-relabel authority:
`v2-json-validation.md §1-4`. Honest baseline restated from
`DISPATCH-CONTEXT.md §1`.

Conventions used throughout:

- (a) **nominal SK-V13 state** = the ledger as it stood before the
  audit. Track 1 Mbps + tranche_admitted from `RESULTS.md` and
  `ROLLING-SOTA-DELTA.md`.
- (b) **audit-corrected state** = the verdict after cross-validation,
  with cite to the validation pack §reference that falsifies or
  sustains. Mark each row AUDIT-FALSIFIED, AUDIT-SUSTAINED-COMPARATOR-PENDING,
  or AUDIT-PENDING.
- Mbps figures retained verbatim from RESULTS.md table 1; SK-V13
  predecessor figures retained from the prior SK-V13 alpha-A artefact
  (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:97-148`)
  where they differ from the current ledger.
- Hot-leaf attribution column points at the `criterion-slope-profile`
  artefact path per row (`RESULTS.md` "Hot leaf" column).
- `Δ vs SK-V12` is computed off the prior alpha-A row inventory; where
  the prior bracket recorded a different T1 (re-baselined under
  SK-V12-close), the delta uses that figure.

The honest-baseline §1 bind from DISPATCH-CONTEXT collapses the campaign
to zero per-row admits: **JSON parse_only 0/17, JSON direct 0/17, JSON
typed 0/17, CSS L4 0/24**. Per-row restatement follows.

---

## §1 JSON — `parse_only` plane (17 rows)

| Corpus | Outcome (nominal) | T1 Mbps | T2 Mbps | sonic-strict Mbps | Δ vs sonic-strict (nominal) | Δ vs SK-V12 alpha-A T1 | Strictness | Output plane | tranche_admitted (nominal) | Audit overlay (b) | Audit §ref |
|---|---|---:|---:|---:|---:|---:|---|---|---|---|---|
| twitter | S/NO-GO | 15561 | 12190 | 21013 | -25.9% | +2071 (vs 13490) | deferred | borrowed-view over offset tape | OPEN | OPEN-UNCHANGED; never claimed admit | v2 §1 (no parser change) |
| citm_catalog | A/GO (W14.2) | 30150 | 20574 | 25565 | +17.9% | +6010 (vs 24140) | strict (nominal) | DOM | **ADMITTED** | **AUDIT-FALSIFIED** — revert to S/NO-GO/OPEN | v2 §1-4 (gate-relabel; comparator misnamed `sonic_rs::from_slice::<Value>`); v6 §3 (not strict parse_only) |
| canada | A/GO (W14.3) | 16977 | 17119 | 14101 | +20.4% | +9299 (vs 7678) | strict (nominal) | DOM | **ADMITTED** | **AUDIT-FALSIFIED** — revert to S/NO-GO/OPEN | v2 §1-4; v6 §3 |
| apache_builds | S/NO-GO | 12767 | 12326 | 17351 | -26.4% | +7333 (vs 5434) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| github_events | S/NO-GO | 14966 | 13096 | 23009 | -35.0% | +7940 (vs 7026) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| update_center | S/NO-GO | 11791 | 9435 | 19661 | -40.0% | +6447 (vs 5344) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| mesh | A/GO (W14.5) | 12987 | 11522 | 11758 | +10.4% | +3092 (vs 9895) | strict (nominal) | DOM | **ADMITTED** | **AUDIT-FALSIFIED** — revert to S/NO-GO/OPEN | v2 §1-4; v6 §3 |
| random | S/NO-GO | 9946 | 8010 | 15665 | -36.5% | +5790 (vs 4156) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| gsoc-2018 | S/NO-GO | 23587 | 22245 | 50363 | -53.2% | +14458 (vs 9129) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| marine_ik | A/GO (W14.4) | 12357 | 12302 | 9902 | +24.8% | +2333 (vs 10024) | strict (nominal) | DOM | **ADMITTED** | **AUDIT-FALSIFIED** — revert to S/NO-GO/OPEN | v2 §1-4; v6 §3 |
| instruments | S/NO-GO | 17468 | 12312 | 19630 | -11.0% | +6870 (vs 10598) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| numbers | A/GO (W14.1) | 19267 | 19126 | 13666 | +41.0% | +4803 (vs 14464) | strict (nominal) | DOM | **ADMITTED** | **AUDIT-FALSIFIED** — revert to S/NO-GO/OPEN | v2 §1-4; v6 §3 |
| unicode_mixed | S/NO-GO | 9294 | 8129 | 18858 | -50.7% | +4726 (vs 4568) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| unicode_escapes | S/NO-GO | 13550 | 12644 | 19273 | -29.7% | +8809 (vs 4741) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| unicode_basic | S/NO-GO | 12041 | 11137 | 16125 | -25.3% | +2117 (vs 9924) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| distinct_values | S/NO-GO | 9920 | 6488 | 18160 | -45.4% | +722 (vs 9198) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |
| y_string_unicode | S/NO-GO | 6590 | 6165 | 13860 | -52.5% | +277 (vs 6313) | deferred | borrowed-view | OPEN | OPEN-UNCHANGED | v2 §1 |

**Hot-leaf attribution (all parse_only rows):**
`criterion-slope-profile:json_<corpus>/track1_generated/new/estimates.json`,
`hot-leaf=criterion-slope-profile`, `row=json/<corpus>/parse_only/main`
(`RESULTS.md:5,8,11,13,16,19,22,25,28,30,33,36,39,41,43,46,48`).

**Plane audit-zero delta (post-overlay):** 0/17 ADMITTED. The 5 W14
admits collapse under v2 §1 (commits `5d5490f08`, `c7f3e42a5`,
`37a791d42`, `71508ea93`, `93eb60182` touch only `gate.rs`,
`report.rs`, `lock14_baseline.rs`; zero parser-source diffs). All 17
rows are OPEN under SK-V14.

---

## §2 JSON — `direct_to_struct` plane (17 rows)

| Corpus | Outcome (nominal) | T1 Mbps | T2 Mbps | sonic-strict Mbps | Δ vs sonic-strict (nominal) | Δ vs SK-V12 alpha-A T1 | Strictness | Output plane | tranche_admitted (nominal) | Audit overlay (b) | Audit §ref |
|---|---|---:|---:|---:|---:|---:|---|---|---|---|---|
| twitter | N-direct/NO-GO | 11908 | 11023 | 15110 | -21.2% | -160 (vs 12068) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3.4 (parser is generated; gate already NO-GO) |
| citm_catalog | A/GO | 21414 | 20630 | 19938 | +7.4% | -209 (vs 21623) | deferred | digest | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser, comparator misbinding | v2 §3 (`parse_direct` is real); v6 §1 row 3, §3 ("eager_typed DOM, not direct struct"), §6 |
| canada | N-direct/NO-GO | 10962 | 10545 | 12205 | -10.2% | +600 (vs 10362) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| apache_builds | A/GO | 11428 | 10390 | 11105 | +2.9% | +31 (vs 11397) | strict (nominal) | digest | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser, comparator misbinding | v2 §3; v6 §1 row 3, §6 |
| github_events | N-direct/NO-GO | 12483 | 11308 | 16197 | -22.9% | +121 (vs 12362) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| update_center | N-direct/NO-GO | 8546 | 7682 | 11183 | -23.6% | +74 (vs 8472) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| mesh | N-direct/NO-GO | 9661 | 8830 | 9757 | -1.0% | +870 (vs 8791) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| random | N-direct/NO-GO | 7801 | 7069 | 8944 | -12.8% | +54 (vs 7747) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| gsoc-2018 | N-direct/NO-GO | 15385 | 15012 | 23880 | -35.6% | +157 (vs 15228) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| marine_ik | A/GO | 10513 | 9607 | 8454 | +24.4% | +1070 (vs 9443) | deferred | digest | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser, comparator misbinding | v2 §3; v6 §1 row 3, §6 |
| instruments | A/GO | 12060 | 11193 | 12731 | -5.3% | -16 (vs 12076) | strict (nominal) | digest | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser, comparator misbinding | v2 §3; v6 §1 row 3, §6 |
| numbers | A/GO (W2) | 14125 | 12700 | 12747 | +10.8% | +1885 (vs 12240) | strict (nominal) | digest | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser w/ W11.1 numeric-array dispatch, comparator misbinding | v2 §3 (W11.1 cited); v6 §1 row 3, §6 |
| unicode_mixed | N-direct/NO-GO | 5062 | 4929 | 10653 | -52.5% | +445 (vs 4617) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| unicode_escapes | N-direct/NO-GO | 5523 | 5431 | 14298 | -61.4% | +409 (vs 5114) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| unicode_basic | A/GO | 9317 | 8512 | 8976 | +3.8% | +1183 (vs 8134) | deferred | digest | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser, comparator misbinding | v2 §3; v6 §1 row 3, §6 |
| distinct_values | N-direct/NO-GO | 6540 | 5801 | 11948 | -45.3% | +535 (vs 6005) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |
| y_string_unicode | N-direct/NO-GO | 5061 | 3806 | 8998 | -43.8% | +86 (vs 4975) | deferred | digest | OPEN | OPEN-UNCHANGED | v2 §3 |

**Hot-leaf attribution (all direct rows):**
`criterion-slope-profile:json_<corpus>/track1_direct_to_struct/new/estimates.json`,
`hot-leaf=criterion-slope-profile`,
`row=json/<corpus>/direct_to_struct/main`
(`RESULTS.md:6,9,12,14,17,20,23,26,29,31,34,37,40,42,44,47,49`).

**Plane audit-zero delta (post-overlay):** 0/17 ADMITTED. The 6 direct
admits (citm_catalog, apache_builds, marine_ik, instruments, numbers,
unicode_basic) carry REAL `generated_json::parse_direct` parsers per v2
§3.1 + §6.2; they FALSIFY because the sonic-rs comparator binding is
`sonic_rs::from_slice::<Value>()` (eager-typed DOM, not strict
direct-struct deser per-corpus) — v6 §1 row 3, §3, §6. These are
SUSPECT until R1 rebind to a true direct-struct comparator lands.
Reclassification under SK-V14 R7: AUDIT-FALSIFIED nominal admit;
SUSPECT-PENDING-R1 status; re-opens for re-admit under rebound
comparator. (`v2-json-validation.md` §3.4 records ADMIT-HOLDS *against
the misbound comparator*; §6 of v6 supersedes per honest-baseline
bind.)

DISPATCH §1 enumerates "4 JSON direct admits" — the 6 actually marked
ADMITTED in `ROLLING-SOTA-DELTA.md:13-93` are citm_catalog, apache_builds,
marine_ik, instruments, numbers, unicode_basic. All six fall under the
same comparator misbinding pattern (v6 §1 row 3). PRUNE-1's revert scope
must read the 6-row authoritative count, not the dispatch §1 "4" — the
two are reconciled per the table below so a downstream agent enumerating
the PRUNE-1 surface lands on a single number.

**Direct-admit reconciliation (DISPATCH §1 "4" vs ROLLING-SOTA-DELTA "6"):**

| Source | Count | Rows | Audit overlay |
|---|---:|---|---|
| DISPATCH-CONTEXT.md §1 line 47 | 4 | citm_catalog, apache_builds, numbers, unicode_basic (SK-V12 + W2/W10 carries the dispatch enumeration) | All AUDIT-FALSIFIED per v6 §1 row 3 |
| ROLLING-SOTA-DELTA.md:13-93 (authoritative) | 6 | +2: **marine_ik**, **instruments** | Both AUDIT-FALSIFIED per v6 §1 row 3 (same comparator-misbinding pattern: `sonic_rs::from_slice::<Value>()` eager-typed DOM, not strict direct-struct deser per-corpus) |

PRUNE-1 revert scope binds the 6-row count; the dispatch §1 "4" is a
trace of the v2 §3-specifically-cited rows, not the full direct-admit
ledger.

---

## §3 JSON — `real_typed_struct` plane (17 rows)

| Corpus | Outcome (nominal) | T1 Mbps | T2 Mbps | sonic-strict Mbps | Δ vs sonic-strict (nominal) | Δ vs SK-V12 alpha-A T1 | Strictness | Output plane | tranche_admitted (nominal) | Audit overlay (b) | Audit §ref |
|---|---|---:|---:|---:|---:|---:|---|---|---|---|---|
| twitter | A/GO | 17898 | 16355 | 15502 | +15.5% | -989 (vs 18887) | deferred | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** — REAL parser, comparator misbinding | v2 §4 (`parse_twitter_search` is generated); v6 §1 row 4 (eager-typed DOM, not per-corpus typed) |
| citm_catalog | A/GO | 36719 | 19693 | 22857 | +60.6% | +289 (vs 36430) | deferred | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| canada | absent | n/a | n/a | n/a | n/a | n/a | n/a | typed direct | **MISSING** | MISSING-UNCHANGED | `ROLLING-SOTA-DELTA.md:22` |
| apache_builds | A/GO | 8127 | 6756 | 8091 | +0.5% | -486 (vs 8613) | deferred | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| github_events | A/GO (W6) | 13040 | 12552 | 12627 | +3.3% | -58 (vs 13098) | strict (nominal) | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| update_center [ext†] | A/GO (W15.1) | 13191 | 10417 | 12623 | +4.5% | +856 (vs 12335) | strict (nominal) | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| mesh | A/GO | 9686 | 7885 | 8867 | +9.2% | -135 (vs 9821) | deferred | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| random [ext†] | A/GO (W13.3) | 8151 | 5384 | 7393 | +10.3% | n/a (new typed row) | strict (nominal) | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| gsoc-2018 | absent | n/a | n/a | n/a | n/a | n/a | n/a | typed direct | **MISSING** | MISSING-UNCHANGED | `ROLLING-SOTA-DELTA.md:40` |
| marine_ik | A/GO | 12164 | 10004 | 9198 | +32.2% | -50 (vs 12214) | deferred | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| instruments [ext†] | A/GO (W13.4) | 21464 | 12262 | 16209 | +32.4% | n/a (new typed row) | strict (nominal) | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| numbers [ext†] | A/GO (W13.1) | 13281 | 9765 | 12249 | +8.4% | n/a (new typed row) | strict (nominal) | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| unicode_mixed | absent | n/a | n/a | n/a | n/a | n/a | n/a | typed direct | **MISSING** | MISSING-UNCHANGED | `ROLLING-SOTA-DELTA.md:52` |
| unicode_escapes | absent | n/a | n/a | n/a | n/a | n/a | n/a | typed direct | **MISSING** | MISSING-UNCHANGED | `ROLLING-SOTA-DELTA.md:55` |
| unicode_basic [ext†] | A/GO (W13.2) | 6753 | 4333 | 6044 | +11.7% | n/a (new typed row) | strict (nominal) | typed direct | **ADMITTED** | **AUDIT-FALSIFIED** | v2 §4; v6 §1 row 4 |
| distinct_values | absent | n/a | n/a | n/a | n/a | n/a | n/a | typed direct | **MISSING** | MISSING-UNCHANGED | `ROLLING-SOTA-DELTA.md:61` |
| y_string_unicode | absent | n/a | n/a | n/a | n/a | n/a | n/a | typed direct | **MISSING** | MISSING-UNCHANGED | `ROLLING-SOTA-DELTA.md:64` |

**Hot-leaf attribution (all typed rows):**
`criterion-slope-profile:json_<corpus>/track1_real_typed_struct/new/estimates.json`,
`hot-leaf=criterion-slope-profile`,
`row=json/<corpus>/real_typed_struct/main`
(`RESULTS.md:7,10,15,18,21,24,27,31,35,38,45`).

**Plane audit-zero delta (post-overlay):** 0/17 ADMITTED. Eleven typed
admits enumerated above (twitter, citm_catalog, apache_builds,
github_events, update_center, mesh, random, marine_ik, instruments,
numbers, unicode_basic) carry REAL `generated_real_typed::parse_*`
parsers per v2 §4.1 + §6.3; they FALSIFY because the sonic-rs
comparator binding is `sonic_rs::from_slice::<Value>()` rather than a
per-corpus `sonic_rs::from_slice::<TypedStruct>()` (v6 §1 row 4).
DISPATCH §1 enumerates "7 JSON typed admits" — the 11 enumerated rows
exceed that count; the discrepancy is that ROLLING-SOTA-DELTA records
11 ADMITTED typed rows (rows :16, :19, :25, :28, :31, :34, :37, :43,
:46, :49, :58) vs the 7 that v2 §4 specifically traced
(twitter, citm_catalog, apache_builds, github_events, update_center,
mesh, marine_ik — the SK-V12 carries). The W13.1/W13.2/W13.3/W13.4 +
W15.1 newer typed admits (numbers, unicode_basic, random, instruments,
update_center) are not separately traced in v2 but fall under the same
v6 §1 row 4 comparator pattern. All 11 reclassify AUDIT-FALSIFIED;
SUSPECT-PENDING-R1.

**[ext†] extension-row legend:** The `[ext†]` marker on five rows above
(`update_center`, `random`, `instruments`, `numbers`, `unicode_basic`)
flags the SK-V13 wave-program typed extensions that lift the typed
admit count from the dispatch §1 "7" to the ROLLING-SOTA-DELTA "11".
Wave-id mapping per row:

| Row | Wave id | Admit landing | Per-row v2/v6 binding |
|---|---|---|---|
| `numbers` | **W13.1** | New typed row (no SK-V12 baseline) | v2 §4 silent; v6 §1 row 4 (same comparator pattern) |
| `unicode_basic` | **W13.2** | New typed row | v2 §4 silent; v6 §1 row 4 |
| `random` | **W13.3** | New typed row | v2 §4 silent; v6 §1 row 4 |
| `instruments` | **W13.4** | New typed row | v2 §4 silent; v6 §1 row 4 |
| `update_center` | **W15.1 (adjusted)** | SK-V12 carry adjusted under W15.1 | v2 §4 traces SK-V12 baseline; W15.1 adjustment falls under v6 §1 row 4 |

These five extension rows (4 W13.x new + 1 W15.1 adjustment) all
reclassify under the same v6 §1 row 4 pattern as the 7 v2-traced
SK-V12 carries; PRUNE-1's revert scope must enumerate the full 11-row
count, while α-C §1's REOPEN-AUDIT scope for W13/W15 routes through R7
per the same row-4 binding. The wave-id column above is the single
source of truth for which rows extend the dispatch §1 enumeration.

Six rows are MISSING per `ROLLING-SOTA-DELTA.md:22,40,52,55,61,64`
(canada, gsoc-2018, unicode_mixed, unicode_escapes, distinct_values,
y_string_unicode). Marked MISSING-UNCHANGED — typed product surface
never generated.

---

## §4 CSS L4 — `direct_to_struct` plane (24 rows)

The CSS L4 plane carries the SK-V12 W1b headline (`declaration_values`
at 2.54× lightningcss) plus 23 SK-V13 wave admits (W2 / W3 / W4 /
W10.1 / W10.2 / W10.3). Per `v1-css-l4-validation.md §1-5`: ALL 24
ADMITTED rows reduce to `include_str!()` of hand-written templates with
fake `@generated` headers (the SK-V12 admit and 23 SK-V13 admits use
byte-identical generator file paths). Per v1 §1 Claim 3, no
`regen-css` xtask exists; per v1 §4, all fixtures are 85–357 bytes
embedded in bench source — no production corpora. Per
`SYNTHESIS-AUDIT-OVERFIT.md §CSS L4 — fake admissions`: the W10.3
`nested_layout` row at 52,234 Mbps / 422 Mbps lightningcss = **124×
margin on 351 bytes (~54 ns/parse)** is OVERFIT-THROUGHPUT; W2 rows at
26,895 Mbps on 117 bytes, W10.1 at 21,585 Mbps on 85 bytes, W10.2 at
34,635 Mbps on 162 bytes are inflated by tiny-corpus + Criterion
overhead artefacts. Per v1 §3, the lightningcss comparator does full
AST parsing + rule traversal while Track 1 reads a hand-written
template — same-plane *naming* but NOT equivalent-work.

| Row | Wave | T1 Mbps | lightningcss Mbps | margin (nominal) | Δ vs lightning (nominal) | fixture bytes | Output plane | tranche_admitted (nominal) | Audit overlay (b) |
|---|---|---:|---:|---:|---:|---:|---|---|---|
| css_l4/declaration_values | SK-V12 W1b | 434.13 | 169.23 | +264.90 | +156.7% | 187 | css_l4_decl_value_fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — same hand-written template; carries the SK-V12 admit; v1 §2 |
| css_l4/declarations | SK-V13 W3 | 265.72 | 55.91 | +209.81 | +375.3% | 305 | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — hand-written template; v1 §5 |
| css_l4/stylesheet_root | SK-V13 W2 | 26894.88 | 596.05 | +26298.83 | +4412% | 117 | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — tiny corpus + template; v1 §5 + SYNTHESIS §CSS L4 |
| css_l4/selectors | SK-V13 W2 | 26894.88 | 596.05 | +26298.83 | +4412% | 117 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/at_rules_keyframes | SK-V13 W10.1 | 21584.64 | 254.22 | +21330.42 | +8390% | 85 | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/nested_rules | SK-V13 W10.3 | 52233.54 | 422.16 | +51811.38 | +12273% | 351 | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED — OVERFIT-THROUGHPUT (124×, ~54 ns/parse)** — v1 §1 Claim 5; SYNTHESIS §CSS L4 |
| css_l4/css_variables | SK-V13 W3 | 265.72 | 55.91 | +209.81 | +375.3% | 305 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/calc_expressions | SK-V13 W3 | 265.72 | 55.91 | +209.81 | +375.3% | 305 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/var_url_functions | SK-V13 W3 | 265.72 | 55.91 | +209.81 | +375.3% | 305 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/color_functions | SK-V13 W3 | 265.72 | 55.91 | +209.81 | +375.3% | 305 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/gradients | SK-V13 W4 | 225.89 | 115.53 | +110.37 | +95.5% | 357 | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/transforms | SK-V13 W4 | 225.89 | 115.53 | +110.37 | +95.5% | 357 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/filters | SK-V13 W4 | 225.89 | 115.53 | +110.37 | +95.5% | 357 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/easing_functions | SK-V13 W4 | 225.89 | 115.53 | +110.37 | +95.5% | 357 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/media_queries | SK-V13 W10.1 | 21584.64 | 254.22 | +21330.42 | +8390% | 85 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/vendor_prefixes | SK-V13 W10.2 | 34635.22 | 278.74 | +34356.48 | +12325% | 162 | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/custom_at_rules | SK-V13 W10.2 | 34635.22 | 278.74 | +34356.48 | +12325% | 162 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/pseudo_classes | SK-V13 W2 | 26894.88 | 596.05 | +26298.83 | +4412% | 117 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/pseudo_elements | SK-V13 W2 | 26894.88 | 596.05 | +26298.83 | +4412% | 117 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/attribute_selectors | SK-V13 W2 | 26894.88 | 596.05 | +26298.83 | +4412% | 117 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/logical_properties | SK-V13 W10.3 | 52233.54 | 422.16 | +51811.38 | +12273% | 351 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/grid | SK-V13 W10.3 | 52233.54 | 422.16 | +51811.38 | +12273% | 351 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/flexbox | SK-V13 W10.3 | 52233.54 | 422.16 | +51811.38 | +12273% | 351 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |
| css_l4/typed_property_groups | SK-V13 W10.3 | 52233.54 | 422.16 | +51811.38 | +12273% | 351 (grouped) | fact_stream | **ADMITTED** | **AUDIT-FALSIFIED** — v1 §5 |

**Hot-leaf attribution (CSS rows):** Per
`RESULTS.md:46,94` and the SK-V12 close artefact at
`restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`,
the canonical CSS leaf is
`criterion:target/criterion/nonjson_css_l4/track1_generated_css_l4_decl_values`
for W1b; SK-V13 CSS rows trace to `nonjson_css_l4/track1_*` Criterion
sub-targets per wave (W2/W3/W4/W10.1/W10.2/W10.3). All rows fail v1 §3
work-equivalence: Track 1 reads `include_str!()` template, lightningcss
constructs full AST.

**Plane audit-zero delta (post-overlay):** 0/24 ADMITTED. Per v1 §5
verdict matrix: ADMIT-HOLDS 0 / 24, ADMIT-FAKE 25 / 25 when the SK-V12
W1b row is included. The campaign headline `+2.54×` collapses on the
same hand-written template that powers all 23 SK-V13 admits; the
124× / 4412× / 8390% / 12325% inflation figures are tiny-fixture +
Criterion-overhead artefacts per SYNTHESIS §CSS L4 + v1 §4.

---

## §5 Cycles-per-byte (c/B) & telemetry

`RESULTS.md` table 1 does not carry a c/B column; the SK-V9 W0
telemetry manifest (`RESULTS.md:51-131`) carries `ns_per_byte` per
row + `track1_ns`, `bytes`, sample-cost facts, profile + host triple,
and per-row comparator evidence binding. Two representative rows:

- `json/twitter/parse_only/main` — `ns_per_byte=0.514090`,
  `track1_ns=324655.33`, `bytes=631515` (`RESULTS.md:55`).
- `json/citm_catalog/parse_only/main` — `ns_per_byte=0.265339`,
  `track1_ns=458294.95`, `bytes=1727204` (`RESULTS.md:58`).

c/B = `ns_per_byte` × `cpu_freq_GHz`. Host triple is
`aarch64-apple-darwin` / Apple M5 Max (`RESULTS.md:55`). Effective
c/B remains schema-debt at the table-1 surface per the prior SK-V13
alpha-A artefact `:92-95`. The audit does not contest the c/B
substrate; it contests the comparator bindings and template
provenance that the c/B sits beneath.

**c/B column LOC budget (carry-with-C-2):** Closing the c/B schema
debt is bounded by ≈ **80-120 LOC** total, decomposed as: (i) ~30-40
LOC for `bbnf-bench/src/report.rs` table-1 column emission (header +
per-row `ns_per_byte × cpu_freq_GHz` formatter + units suffix); (ii)
~20-30 LOC for `cpu_freq_GHz` plumbing through the existing
host-triple capture path (the `aarch64-apple-darwin` / Apple M5 Max
detection at the report-init site already exists per
`RESULTS.md:55`; the freq lookup is a new constant table or sysctl
read); (iii) ~20-30 LOC for the `xtask gate-json` schema extension to
validate the column's presence on every row (parallels the
`comparator_plane` + `per_iter_equality` columns C-2 introduces); (iv)
~10-20 LOC for downstream test fixtures + telemetry-manifest schema
update at the `RESULTS.md:51-131` SK-V9 W0 manifest surface.

This budget routes through **C-2's harness scope** in the same commit
as the comparator rebind: C-2's owner path enumerates
`bbnf-bench/src/report.rs` per α-E §4 (lines 234-240 of the candidate
shortlist), and the c/B column add lands on the same file with no new
owner-path expansion. The 80-120 LOC fits inside C-2's existing 600
LOC lower-bound envelope (600-1.0k per α-E §2) without requiring an
envelope-ceiling raise; the c/B add is folded into C-2's same-wave
consumer rule per `[execute-planned-architecture]` so the column lands
WITH its first consuming bench row in the same commit, not as a
support-only landing.

The lazy-tape materialisation lines at `RESULTS.md:137-180` record
per-corpus offset / sparse-flag / allocated-tape / payload byte
counters and tape-event histograms (object-opens, array-opens, closes,
quotes, numbers, literals, separators). These survive the audit
unchanged — substrate accounting is grammar-neutral per DISPATCH §1
SURVIVES (Tape + OffsetFlags).

---

## §6 Audit verdict summary per plane

| Plane | Nominal ADMITTED | Audit-corrected ADMITTED | Audit-corrected OPEN | Audit-corrected MISSING | Total |
|---|---:|---:|---:|---:|---:|
| JSON parse_only | 5 (W14.1/.2/.3/.4/.5) | **0** | 17 | 0 | 17 |
| JSON direct | 6 (incl. SK-V12 carries + W2 numbers) | **0** | 17 | 0 | 17 |
| JSON typed | 11 (SK-V12 carries + W13.1/.2/.3/.4 + W15.1) | **0** | 11 | 6 (typed surface ungenerated) | 17 |
| CSS L4 | 24 (SK-V12 W1b + 23 SK-V13 wave admits) | **0** | 24 | 0 | 24 |
| **Campaign total** | **46** | **0** | **69** | **6** | **75** |

The SK-V14 entry surface is therefore **0 / 75 ADMITTED**. All 46
nominal admits reclassify per the validation pack:

- 5 parse_only — AUDIT-FALSIFIED (gate-relabel; v2 §1-4).
- 6 direct + 11 typed — AUDIT-FALSIFIED; SUSPECT-PENDING-R1 (real
  parsers, comparator misbinding; v6 §1 rows 3-4, §3, §6).
- 24 CSS L4 — AUDIT-FALSIFIED (hand-written templates + tiny
  corpora; v1 §1-5).

No row carries AUDIT-PENDING; every nominal-admit row has been
cross-validated by the six-agent audit pack. The 35 nominal OPEN /
MISSING rows are AUDIT-NEUTRAL (status unchanged by overlay).

---

## §7 Forward pointers (handoff to peer α-agents)

- **α-B (competitor deltas):** the audit-corrected sonic-rs deltas in
  the per-row tables above use the misbound `sonic_rs::from_slice::<Value>()`
  baseline; the STRICT-VS-STRICT deltas for parse_only / direct / typed
  planes are COMPARATOR-PENDING-R1 (sonic-rs Skipper for parse_only;
  per-corpus typed deser for typed; direct-struct deser for direct).
  The CSS lightningcss deltas are CORPUS-PENDING-R5 + PIPELINE-PENDING-R4
  per v1 §3 + §4.
- **α-C (REDRESS digest):** the 5 W14 admits (REDRESS entries cited at
  commits `5d5490f08, c7f3e42a5, 37a791d42, 71508ea93, 93eb60182` per
  v2 §1) need PRUNE-1 reverts; the 24 CSS admits (REDRESS-123–127 for
  W1b; SK-V13 W2 / W3 / W4 / W10.1-3 entries) need PRUNE-2 reverts; the
  SK-V12 declaration_values admit (REDRESS-127) reverts on the same
  v1 §2 finding.
- **α-D (validated/invalidated/demoted/still-open):** the
  audit-corrected ADMITTED column is 0/75; the SURVIVES bind (W5 /
  W6 / W7 / bbnf-simd / OffsetFlags+Tape / `generated_json::parse_direct`
  / `generated_real_typed::parse_*` / 15 CSS `.bbnf` grammars at
  `/grammar/css/l4/`) is the only carry-forward; the 35 nominal-OPEN
  rows become SK-V14 candidates; the SK-V12 W1b admit moves from
  ADMITTED → INVALIDATED.
- **α-E (candidate shortlist):** every row in this table is downstream
  of C-5 (PRUNE-1 + PRUNE-2 clean-revert), C-1 (Lock-14 refactor
  cluster), C-2 (comparator rebind + per-iter equality oracle), C-3
  (regen-css pipeline + production corpora), C-4 (W8 + W9 scaffold →
  load-bearing) per DISPATCH §α-E.
- **α-F (SYNTHESIS / HANDOFF):** the §0 goalset row enumeration is the
  same 75-row surface above (51 JSON × 3 planes + 24 CSS L4 features);
  the §0 close-condition (R10) reads every cell as currently 0/75
  ADMITTED with an architectural-level intrinsic-block proof carrying
  the residual.

---

## Citations

- `restart/skinny/tranches/sk-v14/research/alpha/DISPATCH-CONTEXT.md:18-60`
  — §1 honest baseline bind (SURVIVES + DOES NOT SURVIVE + audit-zero
  deltas).
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:21-67`
  — cross-axis audit verdict + per-axis findings.
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md:7-251`
  — CSS L4 25-row ADMIT-FAKE verdict matrix.
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md:9-26,29-51,55-145,148-258,304-341,373-385`
  — W14 gate-relabel + SK-V12 carry-over disposition (ADMIT-HOLDS
  against misbound comparator only).
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v3-lock14-deep-scan.md:344-374`
  — 30 Lock-14 violations (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW).
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v4-decision-engine-trace.md`
  — W5/W6/W7 REAL-LANDING; W8/W9 SCAFFOLD-ONLY.
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v5-cross-tranche-stability.md:67-101,125-136`
  — campaign-wide honest baseline + prune scope.
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md:9-37,41-73,76-109,113-158,162-194,198-216,220-228`
  — campaign-wide comparator misbinding (5 violations) + strict-vs-strict
  honest delta.
- `skinny/RESULTS.md:3-49` — current table-1 row authority.
- `skinny/RESULTS.md:51-131` — SK-V9 W0 telemetry manifest (ns_per_byte
  + sample-cost facts + comparator evidence).
- `skinny/RESULTS.md:133-185` — gate notes, payload-arena counters,
  lazy-tape materialisation, SK-V12 campaign close note.
- `restart/skinny/ROLLING-SOTA-DELTA.md:10-64` — JSON 51-row × 3-plane
  ledger.
- `restart/skinny/ROLLING-SOTA-DELTA.md:66-93` — CSS L4 24-row ledger.
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:97-148`
  — predecessor row inventory for Δ vs SK-V12 baselines.
