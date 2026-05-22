# SK-V13 W10.2 Plan - CSS Vendor And Custom At-Rules

Wave: W10.2. Phase: Plan. Date: 2026-05-22.

## Selected Intervention

Land `css_l4/vendor_and_custom_atrules/direct_to_struct/main` as a generated
CSS L4 vendor/custom fact-stream row.

The W10.2 row moves exactly these rolling CSS feature rows when its grouped
report passes:

- `vendor_prefixes`
- `custom_at_rules`

`nested_rules`, `logical_properties`, `grid`, `flexbox`, and
`typed_property_groups` are routed to later W10 subwaves. They are not covered
by this fixture.

## Fixture And Facts

W10.2 fixture:

```css
@custom-media --narrow (max-width:30em);
@-webkit-keyframes fade{from{opacity:0}to{opacity:1}}
a{-webkit-user-select:none;-moz-user-select:none;user-select:none}
```

The final newline is part of the fixture. Fixture identity:

```text
bytes=162
fnv64=b7905e059e2fe40e
sha256=367122942a2c937654b35a1065edc33ae85694a4bcd02b50d6ed50ea1631995f
```

The output plane is `css_l4_vendor_custom_fact_stream`. The fact schema is
`css-l4-vendor-custom-facts-v1`. Track 1, independent oracle, and lightningcss
facts must be byte-identical. The fact stream must prove:

- stylesheet root and rule count;
- `@custom-media` rule with name `--narrow`;
- custom media condition feature `max-width` with value `30em`;
- vendor-prefixed `@-webkit-keyframes` with name `fade`;
- keyframe selectors `from` and `to`;
- style rule declaration names `-webkit-user-select`, `-moz-user-select`, and
  `user-select`;
- vendor prefix classification for `webkit` and `moz`.

The lightningcss sidecar must assert `CssRule::CustomMedia`, vendor-prefixed
`CssRule::Keyframes`, and style declarations retaining all three user-select
spellings. `CssRule::Unknown` is not admitted in W10.2; arbitrary unknown
at-rules require a wider variant matrix and remain outside this subwave.

## Owner Paths

Redress may edit only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs`
- `skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/*`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/*`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w10.2/*`
- `skinny/REDRESS.md`

Forbidden paths include generic emitter/substrate code, JSON runtime/template
paths, SIMD crates, x86 routes, new directives, BIR variants, `BackendShape`
expansion, public substrate APIs, and arbitrary unknown-at-rule admission
without a dedicated variant matrix.

## Gate

`G-W10-2-CSS-VENDOR-CUSTOM` passes only when all are true:

1. `--skv13-css-vendor-custom-report` is supplied with
   `cargo xtask gate-json --check-results`; update/probe combinations reject.
2. The W10.2 report validates schema `sk-v13-css-vendor-custom-sota-v1`,
   wave `SK-V13-W10.2`, run id `sk-v13-w10-2:fixture-fnv64-b7905e059e2fe40e`,
   row identity, covered feature rows, fixture checksum, generated module
   checksum, threshold math, retained artifact paths, and source isolation.
3. The gate rereads Criterion lanes in group `nonjson_css_l4_w10_2`:
   `track1_generated_css_l4_vendor_and_custom_atrules`,
   `track2_golden_vendor_custom_oracle`, and
   `lightningcss_vendor_custom_same_plane_fact_stream`. Report-only Mbps is
   rejected.
4. Track 1 exceeds `lightningcss_mbps + 1.0`, and Track 1, oracle, and
   lightningcss retained fact artifacts are byte-identical.
5. `RESULTS.md` contains the grouped W10.2 admission row and the two covered
   feature rows with the same evidence, while `ROLLING-SOTA-DELTA.md` consumes
   the two feature rows without demotion.
6. Existing SK-V12 declaration-values, W2 stylesheet/selectors, W3
   declaration-values-extended, W4 visual-functions, W10.1 at-rules/media, and
   JSON guard rows maintain through the companion gate invocation.
7. Lock 14 owner-path proof admits only the W10.2 CSS-specific profile paths;
   no generic JSON string, number, selector, block, source-map, or grammar
   policy enters generic crates.

## Revert

On reject, revert the W10.2 codegen/runtime/bench/gate/RESULTS/rolling slice
and record REDRESS with the failed vendor/custom feature family and retained
artifact evidence. No covered feature row may remain `ADMITTED` without the
grouped W10.2 row passing the gate.
