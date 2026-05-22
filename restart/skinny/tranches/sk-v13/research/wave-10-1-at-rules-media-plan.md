# SK-V13 W10.1 Plan - CSS At-Rules And Media

Wave: W10.1. Phase: Plan. Date: 2026-05-22.

## Selected Intervention

Land `css_l4/at_rules_and_media/direct_to_struct/main` as a generated CSS L4
at-rules/media fact-stream row.

The W10.1 row moves exactly these rolling CSS feature rows when its grouped
report passes:

- `at_rules_keyframes`
- `media_queries`

`custom_at_rules`, `vendor_prefixes`, `nested_rules`, `logical_properties`,
`grid`, `flexbox`, and `typed_property_groups` are routed to later W10
subwaves. They are not covered by this fixture.

## Fixture And Facts

W10.1 fixture:

```css
@media screen and (min-width:1px){a{color:red}}
@keyframes k{from,50%,to{opacity:1}}
```

The final newline is part of the fixture. Fixture identity:

```text
bytes=85
fnv64=83cb4eb20e5253c7
sha256=234dde82e1ead1e66be251a5d219892b666f16e853fcd5c03e67aca22fb07958
```

The output plane is `css_l4_at_rules_media_fact_stream`. The fact schema is
`css-l4-at-rules-media-facts-v1`. Track 1, independent oracle, and
lightningcss facts must be byte-identical. The fact stream must prove the
media rule, media type, media feature name/value, nested qualified-rule body,
keyframes rule, keyframes name, keyframe selectors, and keyframe declaration
body boundary. Nested declaration facts do not admit declaration or selector
rows.

The lightningcss sidecar must assert typed `CssRule::Media` and
`CssRule::Keyframes` surfaces, reject `CssRule::Unknown`, reject hidden
`MediaCondition::Unknown`, and detect dropped keyframe blocks. Pretty-printed
CSS equality is not admissible.

## Owner Paths

Redress may edit only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs`
- `skinny/crates/codegen/src/css_l4_at_rules_and_media_templates/*`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/*`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w10.1/*`
- `skinny/REDRESS.md`

Forbidden paths include generic emitter/substrate code, JSON runtime/template
paths, SIMD crates, x86 routes, new directives, BIR variants, `BackendShape`
expansion, and public substrate APIs.

## Gate

`G-W10-1-CSS-AT-RULES-MEDIA` passes only when all are true:

1. `--skv13-css-at-rules-media-report` is supplied with
   `cargo xtask gate-json --check-results`; update/probe combinations reject.
2. The W10.1 report validates schema `sk-v13-css-at-rules-media-sota-v1`,
   wave `SK-V13-W10.1`, run id `sk-v13-w10-1:fixture-fnv64-83cb4eb20e5253c7`,
   row identity, covered feature rows, fixture checksum, generated module
   checksum, threshold math, retained artifact paths, and source isolation.
3. The gate rereads Criterion lanes in group `nonjson_css_l4_w10_1`:
   `track1_generated_css_l4_at_rules_and_media`,
   `track2_golden_at_rules_and_media_oracle`, and
   `lightningcss_at_rules_and_media_same_plane_fact_stream`.
   Report-only Mbps is rejected.
4. Track 1 exceeds `lightningcss_mbps + 1.0`, and Track 1, oracle, and
   lightningcss retained fact artifacts are byte-identical.
5. `RESULTS.md` contains the grouped W10.1 admission row and the two covered
   feature rows with the same evidence, while `ROLLING-SOTA-DELTA.md` consumes
   the two feature rows without demotion.
6. Existing SK-V12 declaration-values, W2 stylesheet/selectors, W3
   declaration-values-extended, W4 visual-functions, and JSON guard rows
   maintain through the companion gate invocation.
7. Lock 14 owner-path proof admits only the W10.1 CSS-specific profile paths;
   no generic JSON string, number, selector, block, source-map, or grammar
   policy enters generic crates.

## Revert

On reject, revert the W10.1 codegen/runtime/bench/gate/RESULTS/rolling slice
and record REDRESS with the failed at-rules/media feature family and retained
artifact evidence. No covered feature row may remain `ADMITTED` without the
grouped W10.1 row passing the gate.
