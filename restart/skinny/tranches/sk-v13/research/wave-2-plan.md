# SK-V13 W2 Plan - CSS Stylesheet Root + Selectors

Wave: W2. Phase: Plan. Date: 2026-05-21.

## Selected Intervention

Land `css_l4/stylesheet_and_selectors/direct_to_struct/main` as a generated
CSS L4 stylesheet/selectors fact-stream row. The row is the SPEC Section 5
admission row; it also moves the rolling CSS feature rows
`stylesheet_root`, `selectors`, `pseudo_classes`, `pseudo_elements`, and
`attribute_selectors` from `OPEN` to `ADMITTED` when the same W2 evidence
passes.

This resolves the naming mismatch found in research: `RESULTS.md` carries the
grouped admission row plus the five covered feature rows, while
`ROLLING-SOTA-DELTA.md` remains the 24-feature close-accounting table.

## Fixture And Facts

W2 fixture:

```css
main.card#hero > a[href^="https"]:hover::before,
#nav .item[data-state="open"] + button:focus::after { color: red; }
```

The fixture is intentionally small and strict-mode friendly. It covers:

- stylesheet root, one qualified rule, one selector list, two selectors;
- type, class, id, child, descendant, next-sibling, and comma separators;
- attribute selectors with prefix and equals operators;
- pseudo-classes and pseudo-elements;
- one declaration fact to prove the qualified rule body boundary.

The output plane is `css_l4_stylesheet_selector_fact_stream`. The fact schema is
`css-l4-stylesheet-selector-facts-v1`. Track 1, golden oracle, and lightningcss
facts must be byte-identical. The golden oracle is fixture-bound and independent
of the generated runtime; lightningcss is the strict same-plane SOTA anchor.

## Owner Paths

Redress may edit only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs`
- `skinny/crates/codegen/src/css_l4_stylesheet_selectors_templates/*`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/*`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w2/*`
- `restart/skinny/tranches/sk-v13/research/wave-2-redress.md`
- `skinny/REDRESS.md` only if W2 rejects.

## Gate

`G-W2-CSS-STYLESHEET-SELECTORS` passes only when all are true:

1. `--skv13-css-stylesheet-selectors-report` is supplied with
   `cargo xtask gate-json --check-results`; update/probe combinations reject.
2. The W2 report validates schema, row identity, covered feature rows, threshold
   math, generated module checksum, artifact paths, and source fixture hash.
3. The gate rereads Criterion lanes:
   `track1_generated_css_l4_stylesheet_selectors`,
   `track2_golden_stylesheet_selectors_oracle`, and
   `lightningcss_stylesheet_selectors_same_plane_fact_stream`.
   Report-only Mbps is rejected.
4. Track 1 exceeds `lightningcss_mbps + 1.0`, and Track 1, golden oracle, and
   lightningcss retained fact artifacts are byte-identical.
5. `RESULTS.md` contains the grouped admission row and the five covered feature
   rows with same evidence, and `ROLLING-SOTA-DELTA.md` consumes the five
   feature rows without demotion.
6. Existing SK-V12 declaration-values CSS row and JSON guards maintain.
7. Lock 14 owner-path proof admits the new generated CSS profile without
   weakening generic forbidden-token scans.

## Revert

On reject, revert the W2 codegen/runtime/bench/gate/RESULTS/rolling slice and
record a REDRESS entry naming the failed selector feature family and artifact.
No partial W2 CSS feature row may remain `ADMITTED` without the grouped row
passing the gate.
