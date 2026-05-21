# Alpha A Results Extraction - SK-V13

Role: alpha-A for SK-V13 Pass Alpha. Scope: extract SK-V12 close results and the
current SK-V13 row state under the binding addendum.

## Binding Addendum Applied

- G1: CSS L4 close means full lightningcss parity across 24 non-OUT_OF_SCOPE
  feature rows: 1 admitted in SK-V12, 23 still to land.
- G5: JSON close means every row across 17 corpora x 3 planes is greater than
  sonic-rs strict. The 51-row target is `parse_only`, `direct_to_struct`, and
  `real_typed_struct` for every corpus. `parse_only` is admission-eligible.
- No fixpoint close is allowed except architectural-level intrinsic-block
  proofs. REDRESS-119-style JSON direct residuals are current state, not a
  SK-V13 close excuse.
- Every bracket must publish a rolling per-row delta table.

## SK-V12 Close Extraction

SK-V12 closed by `PASS-ADMIT`, not FIXPOINT, through one CSS L4 row:

| Row | Track 1 | Comparator | Threshold | Margin | Ratio | Equality |
|---|---:|---:|---:|---:|---:|---|
| `css_l4/declaration_values/direct_to_struct/main` | 429.34420791225705 Mbps | lightningcss 168.92962215656692 Mbps | 169.92962215656692 Mbps | 259.41458575569015 Mbps | 2.5415566697611705x | `pass:track1=cssparser=lightningcss` |

Output plane: `css_l4_declaration_value_fact_stream`.
Independent oracle: cssparser at 217.42665242186035 Mbps.
Fact stream SHA-256:
`caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`.

CSS admission row numbers in `skinny/RESULTS.md`:

| Field | Value |
|---|---|
| Outcome / verdict | `A / GO` |
| Track 1 | 429.34420791225705 Mbps |
| cssparser oracle / Track 2 | 217.42665242186035 Mbps |
| lightningcss strict | 168.92962215656692 Mbps |
| generated LOC guard | 287 LOC, `pass:generated_loc<=360` |
| input bytes | 187 |
| REDRESS | 125 candidate, promoted by 127 |

Tranche disposition:
W1a REDRESS-121 restored GrammarConfig / Lock 14 legality and refreshed JSON
guards; W2 REDRESS-122 closed the `escape_mask_64` correctness prerequisite;
W1b-1 REDRESS-123 admitted the CSS generated Track 1 + cssparser oracle
scaffold; W1b-2a REDRESS-124 admitted the same-plane lightningcss comparator;
W1b-2b REDRESS-125 produced `PASS-ADMIT-CANDIDATE`; W4 REDRESS-126 routed the
delimiter ASM-gen microbench and demoted the five orphan primitives; W5
REDRESS-127 promoted `PASS-ADMIT`.

## Current SK-V13 Row State

Current `skinny/RESULTS.md` first table has 42 rows: 41 JSON rows plus the 1
CSS close row. Under G5, JSON must be normalized to a 51-row target, so 10
`real_typed_struct` rows are currently absent from the rendered authority and
must become explicit rows or explicit blockers.

Current result counts:

| Surface | A / GO | N-direct / NO-GO | S / NO-GO | L / NO-GO | Missing |
|---|---:|---:|---:|---:|---:|
| JSON rendered rows | 12 | 12 | 17 | 0 | 10 typed rows |
| CSS rendered rows | 1 | 0 | 0 | 0 | 23 G1 feature rows |
| Combined rendered rows | 13 | 12 | 17 | 0 | n/a |

JSON 51-row target framing:

| Plane | Current rendered state | SK-V13 addendum target |
|---|---|---|
| `parse_only` | 17 `S / NO-GO` rows | 17 admission-eligible rows, all > sonic-rs strict |
| `direct_to_struct` | 5 `A / GO`, 12 `N-direct / NO-GO` | 17 rows, all > sonic-rs strict |
| `real_typed_struct` | 7 `A / GO`, 10 absent | 17 rows, all > sonic-rs strict |

Absent typed rows: `canada`, `random`, `gsoc-2018`, `instruments`, `numbers`,
`unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`,
`y_string_unicode`.

Current JSON direct `N-direct / NO-GO` rows:
`twitter`, `canada`, `github_events`, `update_center`, `mesh`, `random`,
`gsoc-2018`, `instruments`, `unicode_mixed`, `unicode_escapes`,
`distinct_values`, `y_string_unicode`.

Current JSON parse `S / NO-GO` rows:
all 17 JSON corpora. Under the addendum these are no longer diagnostic-only;
they are row targets.

### Current Row Inventory For B0

`margin` is `Track 1 - (strict comparator + 1)` where a strict comparator is
available. CSS uses lightningcss; JSON uses sonic-rs strict. The current
`skinny/RESULTS.md` top table does not carry cycles/byte, so `c/B` is recorded
as schema debt here and must be filled by S-P1 TSV capture.

| Row | State | Strictness | Plane | T1 | T2 | Strict comparator | Margin | c/B | Hot leaf / evidence |
|---|---|---|---|---:|---:|---:|---:|---|---|
| `json/twitter/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 13490 | 10867 | 18716 | -5227 | schema-debt | `RESULTS.md:5`; `criterion-slope-profile:json_twitter/track1_generated` |
| `json/twitter/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 12068 | 11221 | 15150 | -3083 | schema-debt | `RESULTS.md:6`; `criterion-slope-profile:json_twitter/track1_direct_to_struct` |
| `json/twitter/real_typed_struct/main` | A/GO | deferred | typed direct | 18887 | 16583 | 15761 | 3125 | schema-debt | `RESULTS.md:7`; `criterion-slope-profile:json_twitter/track1_real_typed_struct` |
| `json/citm_catalog/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 24140 | 18580 | 20645 | 3494 | schema-debt | `RESULTS.md:8`; `criterion-slope-profile:json_citm_catalog/track1_generated` |
| `json/citm_catalog/direct_to_struct/main` | A/GO | deferred | digest | 21623 | 20611 | 20026 | 1596 | schema-debt | `RESULTS.md:9`; `criterion-slope-profile:json_citm_catalog/track1_direct_to_struct` |
| `json/citm_catalog/real_typed_struct/main` | A/GO | deferred | typed direct | 36430 | 19610 | 22186 | 14243 | schema-debt | `RESULTS.md:10`; `criterion-slope-profile:json_citm_catalog/track1_real_typed_struct` |
| `json/canada/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 7678 | 7384 | 4302 | 3375 | schema-debt | `RESULTS.md:11`; `criterion-slope-profile:json_canada/track1_generated` |
| `json/canada/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 10362 | 10227 | 11745 | -1384 | schema-debt | `RESULTS.md:12`; `criterion-slope-profile:json_canada/track1_direct_to_struct` |
| `json/apache_builds/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 5434 | 5594 | 8919 | -3486 | schema-debt | `RESULTS.md:13`; `criterion-slope-profile:json_apache_builds/track1_generated` |
| `json/apache_builds/direct_to_struct/main` | A/GO | strict | digest | 11397 | 10269 | 11134 | 262 | schema-debt | `RESULTS.md:14`; `criterion-slope-profile:json_apache_builds/track1_direct_to_struct` |
| `json/apache_builds/real_typed_struct/main` | A/GO | deferred | typed direct | 8613 | 7002 | 8322 | 290 | schema-debt | `RESULTS.md:15`; `criterion-slope-profile:json_apache_builds/track1_real_typed_struct` |
| `json/github_events/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 7026 | 6803 | 12263 | -5238 | schema-debt | `RESULTS.md:16`; `criterion-slope-profile:json_github_events/track1_generated` |
| `json/github_events/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 12362 | 11343 | 16336 | -3975 | schema-debt | `RESULTS.md:17`; `criterion-slope-profile:json_github_events/track1_direct_to_struct` |
| `json/github_events/real_typed_struct/main` | A/GO | strict | typed direct | 13098 | 12768 | 12837 | 260 | schema-debt | `RESULTS.md:18`; `criterion-slope-profile:json_github_events/track1_real_typed_struct` |
| `json/update_center/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 5344 | 4242 | 13836 | -8493 | schema-debt | `RESULTS.md:19`; `criterion-slope-profile:json_update_center/track1_generated` |
| `json/update_center/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 8472 | 7690 | 11239 | -2768 | schema-debt | `RESULTS.md:20`; `criterion-slope-profile:json_update_center/track1_direct_to_struct` |
| `json/update_center/real_typed_struct/main` | A/GO | deferred | typed direct | 12335 | 10663 | 12887 | -553 | schema-debt | `RESULTS.md:21`; `criterion-slope-profile:json_update_center/track1_real_typed_struct` |
| `json/mesh/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 9895 | 8065 | 8980 | 914 | schema-debt | `RESULTS.md:22`; `criterion-slope-profile:json_mesh/track1_generated` |
| `json/mesh/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 8791 | 9088 | 9841 | -1051 | schema-debt | `RESULTS.md:23`; `criterion-slope-profile:json_mesh/track1_direct_to_struct` |
| `json/mesh/real_typed_struct/main` | A/GO | deferred | typed direct | 9821 | 8262 | 9132 | 688 | schema-debt | `RESULTS.md:24`; `criterion-slope-profile:json_mesh/track1_real_typed_struct` |
| `json/random/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 4156 | 3689 | 7116 | -2961 | schema-debt | `RESULTS.md:25`; `criterion-slope-profile:json_random/track1_generated` |
| `json/random/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 7747 | 7053 | 8907 | -1161 | schema-debt | `RESULTS.md:26`; `criterion-slope-profile:json_random/track1_direct_to_struct` |
| `json/gsoc-2018/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 9129 | 10590 | 16925 | -7797 | schema-debt | `RESULTS.md:27`; `criterion-slope-profile:json_gsoc-2018/track1_generated` |
| `json/gsoc-2018/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 15228 | 14595 | 23439 | -8212 | schema-debt | `RESULTS.md:28`; `criterion-slope-profile:json_gsoc-2018/track1_direct_to_struct` |
| `json/marine_ik/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 10024 | 9428 | 7333 | 2690 | schema-debt | `RESULTS.md:29`; `criterion-slope-profile:json_marine_ik/track1_generated` |
| `json/marine_ik/direct_to_struct/main` | A/GO | deferred | digest | 9443 | 9582 | 8503 | 939 | schema-debt | `RESULTS.md:30`; `criterion-slope-profile:json_marine_ik/track1_direct_to_struct` |
| `json/marine_ik/real_typed_struct/main` | A/GO | deferred | typed direct | 12214 | 10164 | 9230 | 2983 | schema-debt | `RESULTS.md:31`; `criterion-slope-profile:json_marine_ik/track1_real_typed_struct` |
| `json/instruments/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 10598 | 7602 | 15207 | -4610 | schema-debt | `RESULTS.md:32`; `criterion-slope-profile:json_instruments/track1_generated` |
| `json/instruments/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 12076 | 11069 | 12433 | -358 | schema-debt | `RESULTS.md:33`; `criterion-slope-profile:json_instruments/track1_direct_to_struct` |
| `json/numbers/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 14464 | 14446 | 10231 | 4232 | schema-debt | `RESULTS.md:34`; `criterion-slope-profile:json_numbers/track1_generated` |
| `json/numbers/direct_to_struct/main` | A/GO | strict | digest | 12240 | 11788 | 12676 | -437 | schema-debt | `RESULTS.md:35`; `criterion-slope-profile:json_numbers/track1_direct_to_struct` |
| `json/unicode_mixed/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 4568 | 3215 | 6942 | -2375 | schema-debt | `RESULTS.md:36`; `criterion-slope-profile:json_unicode_mixed/track1_generated` |
| `json/unicode_mixed/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 4617 | 4528 | 10433 | -5817 | schema-debt | `RESULTS.md:37`; `criterion-slope-profile:json_unicode_mixed/track1_direct_to_struct` |
| `json/unicode_escapes/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 4741 | 9398 | 14603 | -9863 | schema-debt | `RESULTS.md:38`; `criterion-slope-profile:json_unicode_escapes/track1_generated` |
| `json/unicode_escapes/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 5114 | 5072 | 14134 | -9021 | schema-debt | `RESULTS.md:39`; `criterion-slope-profile:json_unicode_escapes/track1_direct_to_struct` |
| `json/unicode_basic/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 9924 | 9025 | 12757 | -2834 | schema-debt | `RESULTS.md:40`; `criterion-slope-profile:json_unicode_basic/track1_generated` |
| `json/unicode_basic/direct_to_struct/main` | A/GO | deferred | digest | 8134 | 8148 | 8842 | -709 | schema-debt | `RESULTS.md:41`; `criterion-slope-profile:json_unicode_basic/track1_direct_to_struct` |
| `json/distinct_values/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 9198 | 6102 | 17080 | -7883 | schema-debt | `RESULTS.md:42`; `criterion-slope-profile:json_distinct_values/track1_generated` |
| `json/distinct_values/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 6005 | 5324 | 11503 | -5499 | schema-debt | `RESULTS.md:43`; `criterion-slope-profile:json_distinct_values/track1_direct_to_struct` |
| `json/y_string_unicode/parse_only/main` | S/NO-GO | deferred | borrowed view over offset tape vs DOM | 6313 | 6023 | 13842 | -7530 | schema-debt | `RESULTS.md:44`; `criterion-slope-profile:json_y_string_unicode/track1_generated` |
| `json/y_string_unicode/direct_to_struct/main` | N-direct/NO-GO | deferred | digest | 4975 | 3544 | 8228 | -3254 | schema-debt | `RESULTS.md:45`; `criterion-slope-profile:json_y_string_unicode/track1_direct_to_struct` |
| `css_l4/declaration_values/direct_to_struct/main` | A/GO | strict | `css_l4_declaration_value_fact_stream` | 429 | 217 | 168.930 | 259.414 | schema-debt | `RESULTS.md:46`, `:94`; `criterion:target/criterion/nonjson_css_l4/track1_generated_css_l4_decl_values` |
| `json/canada/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/random/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/gsoc-2018/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/instruments/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/numbers/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/unicode_mixed/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/unicode_escapes/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/unicode_basic/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/distinct_values/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |
| `json/y_string_unicode/real_typed_struct/main` | absent | n/a | typed direct | n/a | n/a | n/a | n/a | absent-row debt | absent from top table |

## CSS G1 Gap State

SK-V13 scoping records 30 CSS feature rows: 1 PARITY, 7 PARTIAL, 16 MISSING,
and 6 OUT_OF_SCOPE. Under the addendum, the 24 non-OUT_OF_SCOPE rows are the G1
surface: the SK-V12 declaration-values token row is the single admitted row;
the remaining 23 partial/missing rows must admit or carry architectural-level
intrinsic-block proof. Measured implementation rejection is REDRESS evidence,
not close authority.

Required non-OUT_OF_SCOPE families include declarations, stylesheet root,
selectors, at-rules/media, nested rules, CSS variables, calc/var/url, color
functions, gradients, transforms, filters, easing, vendor/custom at-rules,
pseudo-classes/elements, attribute selectors, comments where admitted by the
output plane, and strictness/accounting rows. Explicit out-of-scope rows in
the scoping doc are container queries, layer queries, scope queries, whitespace
preservation, source mapping, and error recovery unless later re-scoped.

## Rolling Delta Table Skeleton

Every SK-V13 bracket should publish this table, filled per row and never only
as aggregate text:

| Bracket | Domain | Row | Plane | Baseline T1 Mbps | Current T1 Mbps | sonic/lightning strict Mbps | Delta vs baseline | Delta vs strict comparator | Outcome / verdict | Evidence artifact | Notes |
|---|---|---|---|---:|---:|---:|---:|---:|---|---|---|
| SK-V13-B0 | JSON | `json/<corpus>/parse_only/main` | borrowed view / parse | TBD | TBD | TBD sonic-rs strict | TBD | TBD | TBD | TBD | parse_only admission-eligible |
| SK-V13-B0 | JSON | `json/<corpus>/direct_to_struct/main` | digest | TBD | TBD | TBD sonic-rs strict | TBD | TBD | TBD | TBD | include Track 2 parity |
| SK-V13-B0 | JSON | `json/<corpus>/real_typed_struct/main` | typed direct | TBD | TBD | TBD sonic-rs strict | TBD | TBD | TBD | TBD | add absent typed rows |
| SK-V13-B0 | CSS | `css_l4/<feature>/direct_to_struct/main` | fact stream / typed facts | TBD | TBD | TBD lightningcss strict | TBD | TBD | TBD | TBD | one row per G1 feature |

## Evidence Citations

- `restart/prompts/pass-contracts/PASS-ALPHA.md:20-27`, `:77-108`: Alpha A
  must extract every results row and bind per-row deltas, strictness, output
  plane, comparator Mbps, and hot-leaf evidence.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md` and
  `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-31`,
  `:33-46`, `:172-183`: CSS is authoritative; SK-V12 W1 close target is
  generated CSS L4 > lightningcss on the same output plane; addendum reference
  extends the pin campaign-wide, restores `parse_only` admission eligibility,
  and raises JSON to every path > sonic-rs strict.
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:5-21`, `:31-35`: records
  `PASS-ADMIT` by CSS row, the exact Track 1/cssparser/lightningcss Mbps,
  threshold, margin, ratio, fact SHA, tranche dispositions, JSON guard hold,
  zero-orphan state, and the fact that this was not a FIXPOINT close.
- `skinny/RESULTS.md:3-46`, `:94`, `:146-148`: current row authority; first
  table yields 41 JSON rows, 1 CSS row, 13 combined `A / GO`, 12
  `N-direct / NO-GO`, and 17 `S / NO-GO`.
- `skinny/REDRESS.md:3638-3640`, `:3721-3724`, `:3824-3828`, `:3864-3869`:
  REDRESS-123 through REDRESS-127 record the CSS scaffold, lightningcss
  comparator, SOTA report gate, W4 delimiter microbench route, and W5 close
  promotion; REDRESS-127 states JSON guards held by AWK verification after the
  CSS row edit.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:13-18`,
  `:96-132`: records the single admitted CSS declaration-values row, the
  1/7/16/6 parity count, and the wave shortlist for remaining CSS L4 parity.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`:
  records PMU staleness, 13 JSON direct residual rows, 17-corpus capture scope,
  and the need for fresh SK-V13 PMU over CSS plus JSON residuals.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`:
  records residual JSON policy leaks in config/value/sink surfaces that matter
  for CSS full parity rows.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`:
  records W4 orphan disposition, the `a64_ascii_set_run_skip` route, and the
  same-wave consumer requirement for any production SIMD/ASM admission.
