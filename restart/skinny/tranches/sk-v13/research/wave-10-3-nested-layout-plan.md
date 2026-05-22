# SK-V13 W10.3 Plan - CSS Nested Layout Pack

Wave: W10.3. Phase: Plan. Date: 2026-05-22.

## Selected Intervention

Land `css_l4/nested_layout/direct_to_struct/main` as a generated CSS L4
fact-stream row.

The W10.3 row moves exactly these rolling CSS feature rows when its grouped
report passes:

- `nested_rules`
- `logical_properties`
- `grid`
- `flexbox`
- `typed_property_groups`

No JSON row, parse-only row, SIMD primitive, union variant, or decision-engine
surface is admitted by this wave.

## Fixture And Facts

W10.3 fixture:

```css
.grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1rem;&>.item{margin-inline-start:1rem;inline-size:calc(100% - 2rem)}}
.nav{display:flex;flex-direction:row;align-items:center;justify-content:space-between;padding-block:1rem;border-inline-start:2px solid #123456}
.type{color:#123456;font-size:clamp(1rem,2vw,2rem);line-height:1.4}
```

The final newline is part of the fixture. Redress records the exact byte,
FNV64, and SHA-256 identity after the fixture is written.

The output plane is `css_l4_nested_layout_fact_stream`. The fact schema is
`css-l4-nested-layout-facts-v1`. Track 1, independent oracle, and lightningcss
facts must be byte-identical. The fact stream must prove:

- stylesheet root and rule count;
- one nested rule with depth and parent selector facts;
- grid declaration family facts;
- flex declaration family facts;
- logical property facts;
- typed property group facts for color, length, display/layout, font, and
  border families;
- exact declaration order and source spans.

The lightningcss sidecar must parse the fixture and assert the expected
style-rule, nested-rule, and declaration families before returning the
source-sidecar fact stream. Parse success alone is not sufficient.

## Owner Paths

Redress may edit only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_nested_layout_provider.rs`
- `skinny/crates/codegen/src/css_l4_nested_layout_templates/*`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_nested_layout/*`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w10.3/*`
- `skinny/REDRESS.md`

Forbidden paths include generic emitter/substrate code, JSON runtime/template
paths, SIMD crates, x86 routes, new directives, BIR variants, `BackendShape`
expansion, public substrate APIs, and non-CSS policy surfaces.

## Gate

`G-W10-3-CSS-NESTED-LAYOUT` passes only when all are true:

1. `--skv13-css-nested-layout-report` is supplied with
   `cargo xtask gate-json --check-results`; update/probe combinations reject.
2. The W10.3 report validates schema `sk-v13-css-nested-layout-sota-v1`,
   wave `SK-V13-W10.3`, row identity, covered feature rows, fixture checksum,
   generated module checksum, threshold math, retained artifact paths, and
   source isolation.
3. The gate rereads Criterion lanes in group `nonjson_css_l4_w10_3`:
   `track1_generated_css_l4_nested_layout`,
   `track2_golden_nested_layout_oracle`, and
   `lightningcss_nested_layout_same_plane_fact_stream`.
4. Track 1 exceeds `lightningcss_mbps + 1.0`, and Track 1, oracle, and
   lightningcss retained fact artifacts are byte-identical.
5. `RESULTS.md` contains the grouped W10.3 admission row and the five covered
   feature rows with the same evidence, while `ROLLING-SOTA-DELTA.md` consumes
   the five feature rows without demotion.
6. Existing SK-V12 declaration-values, W2 stylesheet/selectors, W3
   declaration-values-extended, W4 visual-functions, W10.1 at-rules/media,
   W10.2 vendor/custom, and JSON guard rows maintain through the companion gate
   invocation.
7. Lock 14 owner-path proof admits only the W10.3 CSS-specific profile paths;
   no generic JSON string, number, selector, block, source-map, or grammar
   policy enters generic crates.

## Revert

On reject, revert the W10.3 codegen/runtime/bench/gate/RESULTS/rolling slice
and record REDRESS with the failed nested/layout feature family and retained
artifact evidence. No covered feature row may remain `ADMITTED` without the
grouped W10.3 row passing the gate.
