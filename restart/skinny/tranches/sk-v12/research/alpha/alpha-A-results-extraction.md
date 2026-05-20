# SK-V12 Alpha-A Results Extraction - Pin-Aware Re-Bracket

Date: 2026-05-20.

Pass: Alpha SK-V11 -> SK-V12 re-bracket, lane Alpha-A.

Scope: extract the carried SK-V11 close results under the 2026-05-20 USER PIN
and identify the opening measurement surface for SK-V12. This lane is
read-only synthesis: no source, gate, or benchmark semantics are changed here.

## Sources Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md`.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 0.5, for guard floors.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`.

## Pin Delta

The prior Alpha-A extraction was correct for the pre-pin SK-V12 packet, but it
is no longer sufficient. USER PIN D1/D2 rebinds the first non-JSON target to
CSS L4, not Sheets, and raises the close bar from
`ceil(baseline_mbps * 1.01)` to `lightningcss_mbps + 1` on the same corpus,
same output plane, with strict equality.

Load-bearing pin effects for this lane:

- The W1 V2 Sheets plan at commit `e24a7e01` is **OBSOLETE**. Sheets is now a
  fallback only after a CSS L4 redress attempt, not after preflight-only
  scouting.
- The SK-V11 W1a non-JSON report lane is still only a report/gate lane. It did
  not create a generated CSS L4 runtime, update `skinny/RESULTS.md`, or admit a
  non-JSON row.
- REDRESS 112/113 are superseded as a category-level dispatch block because
  CSS L4 is now the explicit mandate, but their factual measurements remain
  true: generated CSS L4 Track 1 is absent today.
- REDRESS 114-119 remain unchanged for JSON direct residuals. JSON direct rows
  are guard/reopen ledger rows, not the W1 first target.
- USER PIN D3/D4 unblocks union-substrate and ASM-gen categories for new,
  materially different attempts. REDRESS 88/89/90 and 96/97/98 stay historical
  implementation evidence that new plans must cite.

## CSS L4 Target Absence

There is no current generated CSS L4 row.

Evidence:

- `skinny/RESULTS.md` contains only `json/...` row identities; no
  `css_l4/...` row exists.
- The runtime grammar directory contains generated JSON and
  `sheets_witness` only. There is no
  `skinny/crates/runtime/src/grammars/css_l4/` or
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`.
- REDRESS 112 records that W1b selected
  `css_l4/declaration_values/direct/main` but admitted no baseline report
  because skinny codegen still routed emission through the JSON-only provider.
- REDRESS 113 records that W2 could not measure a CSS intervention because
  `W1b_css_baseline_mbps` was undefined.

Current CSS L4 admission floor: **unmeasured**. Under USER PIN D2 it becomes
`lightningcss_mbps + 1` only after W1 measures the same corpus and output plane
against lightningcss with strict equality and gate-consumed provenance.

## Carried JSON Close Surface

REDRESS 120 closed SK-V11 as a measured fixpoint, not as overall direct `GO`.
W9 made no behavior source, generated runtime, benchmark body, gate semantic,
or `skinny/RESULTS.md` change. The SK-V12 seed JSON state is therefore the
unchanged SK-V11 close surface:

| Family | Carried state | SK-V12 role |
|---|---:|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | Diagnostic only; no SOTA admission |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | Guard rows plus REDRESS 119 reopen ledger |
| `real_typed_struct` | 7 `A / GO` | Product-plane guard rows |
| generated non-JSON parser | none admitted | CSS L4 first target under USER PIN |
| Overall | `N-direct / NoGo` | Seed outcome |

Delta vs SK-V11 close: zero row movement. `skinny/RESULTS.md` remains the JSON
measurement authority, while the SK-V12 profile audit supplies the current
SK-V12-open c/B and hot-leaf attribution from `/tmp/skv12-p1`.
Local citations: the row surface is rendered in `skinny/RESULTS.md:5-45`, the
overall outcome and Track 2 independence notes are at `skinny/RESULTS.md:143-146`,
and the SK-V11 close/fixpoint statement is REDRESS 120 at
`skinny/REDRESS.md:3531-3553`.

## Parse-Only Rows

Common role: diagnostic only. These rows may inform profiling, but no
parse-only row can close SK-V12 or satisfy USER PIN D6.

| Row | Outcome | T1/T2 Mbps | c/B | Strictness | Output plane | Profile leaf | Carry-forward |
|---|---|---:|---:|---|---|---|---|
| `twitter/parse_only` | `S / NO-GO` | 10474 / 7757 | 2.214 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `citm_catalog/parse_only` | `S / NO-GO` | 26791 / 18271 | 1.123 | deferred | borrowed view over offset tape vs DOM | container_dispatch | unchanged diagnostic |
| `canada/parse_only` | `L / NO-GO` | 15544 / 16215 | 1.933 | deferred | borrowed view over offset tape vs DOM | container_dispatch | unchanged diagnostic |
| `apache_builds/parse_only` | `S / NO-GO` | 12733 / 12196 | 2.737 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `github_events/parse_only` | `S / NO-GO` | 14805 / 12791 | 2.281 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `update_center/parse_only` | `S / NO-GO` | 11493 / 9033 | 2.893 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `mesh/parse_only` | `S / NO-GO` | 13325 / 12128 | 2.653 | deferred | borrowed view over offset tape vs DOM | container_dispatch | unchanged diagnostic |
| `random/parse_only` | `S / NO-GO` | 7747 / 7554 | 3.519 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `gsoc-2018/parse_only` | `S / NO-GO` | 4887 / 4544 | 1.481 | deferred | borrowed view over offset tape vs DOM | simd_movemask | unchanged diagnostic |
| `marine_ik/parse_only` | `S / NO-GO` | 10675 / 11700 | 2.556 | deferred | borrowed view over offset tape vs DOM | container_dispatch | unchanged diagnostic |
| `instruments/parse_only` | `S / NO-GO` | 16574 / 11587 | 2.028 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `numbers/parse_only` | `S / NO-GO` | 17941 / 18328 | 1.742 | deferred | borrowed view over offset tape vs DOM | number_digit_span | unchanged diagnostic |
| `unicode_mixed/parse_only` | `S / NO-GO` | 1883 / 7326 | 4.297 | deferred | borrowed view over offset tape vs DOM | string_escape_decode | unchanged diagnostic |
| `unicode_escapes/parse_only` | `S / NO-GO` | 3733 / 2421 | 2.819 | deferred | borrowed view over offset tape vs DOM | unicode_escape_hex_decode | unchanged diagnostic |
| `unicode_basic/parse_only` | `S / NO-GO` | 3217 / 2985 | 2.865 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `distinct_values/parse_only` | `S / NO-GO` | 2335 / 1675 | 3.585 | deferred | borrowed view over offset tape vs DOM | bounded_plain_string_scan | unchanged diagnostic |
| `y_string_unicode/parse_only` | `S / NO-GO` | 1965 / 2695 | 5.622 | deferred | borrowed view over offset tape vs DOM | unicode_escape_hex_decode | unchanged diagnostic |

## Direct-To-Struct Rows

Common output plane: generated Track 1 SinkOnly digest vs independent hand
Track 2 SinkOnly digest. REDRESS 119 is the direct residual authority. USER
PIN D6 makes these JSON rows guard/reopen rows after CSS L4, not the first
admission target.

| Row | Outcome | T1/T2 Mbps | sonic direct | c/B | Floor / guard | Carry-forward |
|---|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | `N-direct / NO-GO` | 11613 / 10816 | 15113 | 2.950 | reopen floor 13740 | REDRESS 119 fixpoint; W5/W7/W8 exhausted |
| `citm_catalog/direct_to_struct` | `A / GO` | 18563 / 17787 | 15530 | 1.612 | guard 18191 / 17431 | admitted guard row |
| `canada/direct_to_struct` | `N-direct / NO-GO` | 10316 / 9819 | 11700 | 3.254 | reopen floor 10637 | REDRESS 119 fixpoint; W3 sibling route rejected |
| `apache_builds/direct_to_struct` | `A / GO` | 11254 / 10189 | 10995 | 3.058 | guard 11028 / 9996 | admitted guard row |
| `github_events/direct_to_struct` | `N-direct / NO-GO` | 11918 / 10596 | 14743 | 2.830 | reopen floor 13403 | REDRESS 119 fixpoint |
| `update_center/direct_to_struct` | `N-direct / NO-GO` | 8187 / 7474 | 11064 | 4.120 | reopen floor 10059 | REDRESS 119 fixpoint |
| `mesh/direct_to_struct` | `N-direct / NO-GO` | 8561 / 8652 | 9542 | 3.956 | reopen floor 8675 | REDRESS 114 measured reject |
| `random/direct_to_struct` | `N-direct / NO-GO` | 7693 / 6949 | 8665 | 4.403 | reopen floor 7878 | REDRESS 115 measured reject |
| `gsoc-2018/direct_to_struct` | `N-direct / NO-GO` | 2665 / 2578 | 4110 | 2.336 | reopen floor 3737 | REDRESS 119 fixpoint |
| `marine_ik/direct_to_struct` | `A / GO` | 8938 / 9437 | 8473 | 3.650 | guard 8759 / 9248 | admitted guard row |
| `instruments/direct_to_struct` | `N-direct / NO-GO` | 11569 / 10736 | 9865 | 2.863 | reopen floor 8969 | W0-clamped; docs-only admission pre-blocked |
| `numbers/direct_to_struct` | `N-direct / NO-GO` | 4479 / 2366 | 2667 | 2.703 | reopen floor 2425 | W0-clamped; Track 2 misses floor |
| `unicode_mixed/direct_to_struct` | `N-direct / NO-GO` | 3753 / 2427 | 2846 | 7.454 | reopen floor 2588 | W0-clamped; Track 2 misses floor |
| `unicode_escapes/direct_to_struct` | `N-direct / NO-GO` | 1345 / 1341 | 3785 | 6.722 | reopen floor 3441 | Unicode route blocked by W5/W6 and REDRESS 107/108 |
| `unicode_basic/direct_to_struct` | `A / GO` | 2299 / 2227 | 2353 | 3.768 | guard 2253 / 2182 | admitted guard row |
| `distinct_values/direct_to_struct` | `N-direct / NO-GO` | 1750 / 1625 | 2923 | 5.469 | reopen floor 2658 | REDRESS 119 fixpoint |
| `y_string_unicode/direct_to_struct` | `N-direct / NO-GO` | 1983 / 1029 | 4344 | 9.993 | reopen floor 3950 | REDRESS 119 fixpoint |

## Real-Typed-Struct Rows

Common output plane: generated Track 1 typed product vs independent Track 2
structural or serde oracle. These are product-plane guard wins. They do not
convert the overall surface out of `N-direct / NoGo`, and they do not satisfy
the CSS L4 pin.

| Row | Outcome | T1/T2 Mbps | sonic typed | c/B | Guard floor | Carry-forward |
|---|---|---:|---:|---:|---:|---|
| `twitter/real_typed_struct` | `A / GO` | 17740 / 15912 | 15010 | 1.881 | 17385 / 15593 | admitted typed guard |
| `citm_catalog/real_typed_struct` | `A / GO` | 30539 / 17675 | 20726 | 0.964 | 29928 / 17321 | admitted typed guard |
| `apache_builds/real_typed_struct` | `A / GO` | 8478 / 6892 | 8106 | 4.088 | 8308 / 6754 | admitted typed guard |
| `github_events/real_typed_struct` | `A / GO` | 11871 / 12275 | 12224 | 2.706 | 11633 / 12029 | admitted typed guard |
| `update_center/real_typed_struct` | `A / GO` | 11851 / 10358 | 12467 | 2.798 | 11613 / 10150 | admitted typed guard |
| `mesh/real_typed_struct` | `A / GO` | 9403 / 7897 | 8923 | 3.694 | 9214 / 7739 | admitted typed guard |
| `marine_ik/real_typed_struct` | `A / GO` | 11788 / 10096 | 9010 | 2.932 | 11552 / 9894 | admitted typed guard |

## Guard Floor Ledger

These floors are binding whenever a wave refreshes JSON results or touches a
JSON-producing path. A wave that does not touch JSON-producing paths may instead
prove `skinny/RESULTS.md` unchanged.
Local citations: the opening family posture is inherited from
`restart/skinny/tranches/sk-v12/SPEC.md:169-180`; direct residual proof rows
are REDRESS 119 at `skinny/REDRESS.md:3495-3527`; admitted guard row Mbps live
in `skinny/RESULTS.md:6-45`.

Direct guard floors:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard floors:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Direct residual reopen floors:

| Row | Track 1 | Track 2 | sonic direct | floor |
|---|---:|---:|---:|---:|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 |

## Audit Carry-Forward For Alpha Lanes

- Sheets execution scout: useful only as fallback technical inventory. Its
  concrete Sheets-first execution route is obsolete under USER PIN D1.
- SIMD audit: five aarch64 orphan primitives remain in scope for eventual
  zero-orphan close, but any SIMD admission must first fix or verify the
  `escape_mask_64` NEON correctness bug and pass Lock 16 scalar/checkasm/
  same-wave-consumer gates.
- Profile truth audit: SK-V12-open PMU authority is `/tmp/skv12-p1`; CSS L4 has
  not yet been profiled because no generated CSS L4 baseline exists.
- Value API audit: seven Lock 14 leaks in JSON generated templates block legal
  CSS L4 emission until the `GrammarConfig`/per-grammar config surface removes
  JSON policy from generic emission paths.
- Decision engine audit: CSP/e-graph are absent from skinny and the cost model
  is a passive ledger; candidate lanes must not claim optimizer generality from
  current hardcoded shape selection.
- Totality fold scout: Lock 14/16 amendments are owed; the `escape_mask_64`
  falsifier is `0xCAFEF00DBAADF00D`, and SIMD SOTA claims are blocked until
  parity is restored.

## Alpha-A Carry Forward

1. CSS L4 is the authoritative SK-V12 target. There is no current CSS L4 row,
   no lightningcss comparator number, and no generated CSS L4 runtime module.
2. The first measurable floor for CSS L4 is `lightningcss_mbps + 1`, not a
   percentage over an internal baseline.
3. The existing JSON surface is unchanged from SK-V11 close. It supplies guard
   floors and residual fixpoint evidence, not the W1 admission target.
4. The prior Sheets W1 V2 plan is obsolete and must be annotated as historical
   if referenced by later Alpha or S-P3 lanes.
5. A pin-aware next lane must treat union-substrate and ASM-gen categories as
   unblocked for new material differentials while preserving the measured
   evidence of prior rejected implementations.
