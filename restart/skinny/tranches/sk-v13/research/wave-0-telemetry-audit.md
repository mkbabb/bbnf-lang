# SK-V13 W0 Telemetry Audit

**Date:** 2026-05-21
**Wave:** W0 - PMU baseline lock and rolling SOTA delta
**Scope:** read-only research for the W0 triumvirate
**Source commit:** `dad6094fc`

## Authority Read

- `restart/skinny/tranches/sk-v13/SPEC.md` Section 3 makes W0 an
  infrastructure-only wave. It may edit `skinny/crates/bbnf-bench/`,
  `skinny/xtask/src/`, `skinny/RESULTS.md`,
  `restart/skinny/ROLLING-SOTA-DELTA.md`, and W0 research artefacts.
- W0 must capture the SK-V13-open universe across all extant JSON rows,
  the CSS L4 target inventory, comparators, run ids, host/build/feature
  facts, and hot leaves.
- W0 must consume the emitted fields through `gate-json`, CSS companion
  gates, or a rolling-delta gate in the same wave.
- W0 may not change parser, scanner, SIMD, codegen, generated runtime, or
  product behavior.

## Current State

- `skinny/RESULTS.md` already contains the JSON 17-corpus universe across
  `parse_only`, `direct_to_struct`, and `real_typed_struct`, plus the
  SK-V12 admitted CSS row
  `css_l4/declaration_values/direct_to_struct/main`.
- The JSON manifest rows still carry `SK-V9-open` run ids because the live
  bench report machinery is the historical W0 emitter. That is acceptable
  only if the SK-V13 rolling table records it explicitly as carried source
  evidence rather than silently treating it as fresh SK-V13 behavior.
- The CSS companion gate exists for the SK-V12 row through
  `--skv12-css-l4-sota-report`; it validates same-run Criterion lanes and
  retained fact-stream equality for the admitted declaration-values row.
- `skinny/xtask/src/main.rs::validate_w0_results_snapshot` is stale. It
  still expects a SK-V10/SK-V9 manifest row count and a uniform
  `sk-v9-open:criterion-fnv64-*` marker, so it cannot be the SK-V13 W0
  rolling-delta consumer.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` already rejects stale rendered
  `RESULTS.md` when JSON gate output diverges, but it does not yet validate
  the SK-V13 target universe or `restart/skinny/ROLLING-SOTA-DELTA.md`.

## CSS Feature Universe

The SK-V13 scoping matrix defines 30 CSS feature rows:

- 1 already admitted: declaration values.
- 23 pinned parity targets: declarations, selectors, stylesheet root,
  at-rules/keyframes, nested rules, CSS variables, calc expressions,
  var/url functions, color functions, gradients, transforms, filters,
  easing functions, media queries, custom at-rules, pseudo-classes,
  pseudo-elements, attribute selectors, vendor prefixes, display/visibility,
  position/sizing, text/fonts, backgrounds/borders.
- 6 out-of-scope/diagnostic rows: container queries, layer queries, scope
  queries, comments, whitespace preservation, source mapping, error
  recovery, and strict/permissive mode policy are noted in the matrix; W0
  must render the non-OUT_OF_SCOPE inventory used by the addendum.

The rolling table must not collapse missing CSS features into prose; each
feature needs a row with current status and the lightningcss comparator state.

## JSON Universe

The addendum pins all 51 JSON rows:

- 17 `parse_only` rows, now admission-eligible.
- 17 `direct_to_struct` rows, including all 13 prior N-direct residuals.
- 17 `real_typed_struct` rows, including 10 missing product surfaces.

W0 should render all 51 rows even when a row is currently absent from
`RESULTS.md`, with an explicit absent reason. Silent omission would make the
row universe non-measurable.

## W0 Research Conclusion

W0 should land a generated or hand-curated rolling-delta artefact plus a gate
consumer that enforces:

1. exactly 51 JSON target rows,
2. the pinned CSS feature inventory with one admitted SK-V12 row and open
   rows for the rest,
3. explicit absent reasons for missing typed/product rows,
4. no stale or permissive comparator used as an admission anchor,
5. no behavior source changes.

The safe implementation route is to add a small xtask/gate validator for
`restart/skinny/ROLLING-SOTA-DELTA.md`, update the stale W0 snapshot logic to
SK-V13 semantics, and commit the rolling table generated from the current
`RESULTS.md` evidence.
