# SK-V13 W4 Plan - CSS Visual Functions Pack

Wave: W4. Phase: Plan. Date: 2026-05-22.

## Selected Intervention

Land `css_l4/visual_functions/direct_to_struct/main` as a generated CSS L4
visual-functions fact-stream row. The row is a grouped parity row, analogous to
W2 stylesheet/selectors and W3 declaration-values-extended.

The W4 row moves these rolling CSS feature rows when its grouped report passes:

- `gradients`
- `transforms`
- `filters`
- `easing_functions`

At-rules, media/keyframes, nested rules, vendor/custom taxonomy, logical
properties, grid, flexbox, and typed property groups are routed to W10 subwaves
unless a sidecar produces a strict-equality row small enough for the W4 cap.

## Fixture And Facts

W4 fixture:

```css
.hero {
  background-image: linear-gradient(45deg, #123456 0%, color-mix(in srgb, red 30%, blue) 100%);
  transform: translate3d(10px, 20%, 0) rotate(12deg) scale(1.2);
  filter: blur(2px) brightness(1.2) contrast(80%);
  transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
  animation-timing-function: steps(4, jump-end);
}
```

The fixture is strict-mode friendly and covers:

- gradient function and color-stop facts;
- transform functions with length, percentage, angle, and number arguments;
- filter functions with length, number, and percentage arguments;
- cubic-bezier and steps easing functions;
- existing hash, number, percentage, dimension, ident, function, delimiter,
  comma, and parenthesis token families.

The output plane is `css_l4_visual_function_fact_stream`. The fact schema is
`css-l4-visual-function-facts-v1`. Track 1, independent oracle, and
lightningcss facts must be byte-identical. The independent oracle may be a
hand-checked golden fact stream as long as cssparser or lightningcss strict
parse checks prove the fixture acceptance envelope.

## Owner Paths

Redress may edit only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_visual_functions_provider.rs`
- `skinny/crates/codegen/src/css_l4_visual_functions_templates/*`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_visual_functions/*`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w4/*`
- `restart/skinny/tranches/sk-v13/research/wave-4-redress.md`
- `skinny/REDRESS.md` only if W4 rejects.

## Gate

`G-W4-CSS-VISUAL-FUNCTIONS` passes only when all are true:

1. `--skv13-css-visual-functions-report` is supplied with
   `cargo xtask gate-json --check-results`; update/probe combinations reject.
2. The W4 report validates schema, row identity, covered feature rows, fixture
   checksum, generated module checksum, threshold math, artifact paths, and
   source isolation.
3. The gate rereads Criterion lanes in a separate W4 group:
   `track1_generated_css_l4_visual_functions`,
   `track2_golden_visual_functions_oracle`, and
   `lightningcss_visual_functions_same_plane_fact_stream`.
   Report-only Mbps is rejected.
4. Track 1 exceeds `lightningcss_mbps + 1.0`, and Track 1, oracle, and
   lightningcss retained fact artifacts are byte-identical.
5. `RESULTS.md` contains the grouped W4 admission row and the four covered
   feature rows with the same evidence, while `ROLLING-SOTA-DELTA.md` consumes
   the four feature rows without demotion.
6. Existing SK-V12 declaration-values, W2 stylesheet/selectors, W3
   declaration-values-extended, and JSON guard rows maintain through the same
   `gate-json --check-results` invocation.
7. Lock 14 owner-path proof admits only the W4 CSS-specific profile paths; no
   generic JSON string, number, selector, block, or source-map policy enters
   generic crates.

## Revert

On reject, revert the W4 codegen/runtime/bench/gate/RESULTS/rolling slice and
record a REDRESS entry naming the failed visual-functions family and artifact.
No W4 feature row may remain `ADMITTED` without the grouped W4 row passing the
gate.
