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

## CSS G1 Gap State

SK-V13 scoping records 30 CSS feature rows: 1 PARITY, 7 PARTIAL, 16 MISSING,
and 6 OUT_OF_SCOPE. Under the addendum, the 24 non-OUT_OF_SCOPE rows are the G1
surface: the SK-V12 declaration-values token row is the single admitted row;
the remaining 23 partial/missing rows must land or be measured-rejected.

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

- `restart/prompts/pass-contracts/PASS-ALPHA.md`: Alpha A must extract every
  results row and bind per-row deltas, strictness, output plane, comparator
  Mbps, and hot-leaf evidence.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`: CSS is
  authoritative; SK-V12 W1 close target is generated CSS L4 > lightningcss on
  the same output plane; addendum reference extends the pin campaign-wide,
  restores `parse_only` admission eligibility, and raises JSON to every path >
  sonic-rs strict.
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`: records `PASS-ADMIT` by CSS
  row, the exact Track 1/cssparser/lightningcss Mbps, threshold, margin, ratio,
  fact SHA, tranche dispositions, JSON guard hold, zero-orphan state, and the
  fact that this was not a FIXPOINT close.
- `skinny/RESULTS.md`: current row authority; first table yields 41 JSON rows,
  1 CSS row, 13 combined `A / GO`, 12 `N-direct / NO-GO`, and 17 `S / NO-GO`.
- `skinny/REDRESS.md`: REDRESS-123 through REDRESS-127 record the CSS scaffold,
  lightningcss comparator, SOTA report gate, W4 delimiter microbench route, and
  W5 close promotion; REDRESS-127 states JSON guards held by AWK verification
  after the CSS row edit.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`:
  records the single admitted CSS declaration-values row, the 1/7/16/6 parity
  count, and the wave shortlist for remaining CSS L4 parity.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`:
  records PMU staleness, 13 JSON direct residual rows, 17-corpus capture scope,
  and the need for fresh SK-V13 PMU over CSS plus JSON residuals.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`:
  records residual JSON policy leaks in config/value/sink surfaces that matter
  for CSS full parity rows.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`:
  records W4 orphan disposition, the `a64_ascii_set_run_skip` route, and the
  same-wave consumer requirement for any production SIMD/ASM admission.
