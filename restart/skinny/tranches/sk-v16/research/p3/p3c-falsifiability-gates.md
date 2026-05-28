# SK-V16 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V16.
Date: 2026-05-28.
Scope: bind measurable gates for SK-V16 W0..W11.
Output: this file.
Pass Alpha goalset: JSON 51-row guard, CSS grammar-derived typed equality and cssparser SOTA, dirty generated retirement/block, Pattern H collapse, Decision/BackendShape guards, FNV quarantine, and profile-first aarch64 SIMD only.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## Section 1 - Synthesis

Every SK-V16 wave closes on a measured row gate or a gate-consumed report.
Unmeasurable prose gates reject. CSS speed does not count until typed equality
passes. The old W8R CSS tuple (`track1=2319.041`, `cssparser=2362.037`,
`lightningcss=929.281`) is wrong-plane diagnostic evidence only.

Global formulas after W0 captures `SK-V16-open`:

- JSON Track 1 maintain: `max(0.98 * open_track1_mbps, sonic_strict_mbps + 1.000)`.
- JSON Track 2 maintain: `0.98 * open_track2_mbps` plus independent Track 2 proof.
- CSS typed equality: `css_typed_summary_equal=true` and Track 1/cssparser pass counts match.
- CSS typed SOTA: `track1_typed_mbps >= cssparser_typed_mbps + 1.000`.

## Section 2 - Deliverable

| Wave | Named rows | Threshold | Exit gate | Revert protocol |
|---|---|---|---|---|
| W0 | all 51 JSON rows; all 24 CSS rows | JSON formulas; CSS remains OPEN/non-admission | `gate-json --check-results` and SK-V16 report consumers reject missing fields; no behavior diff | revert report/gate schema and record baseline miss |
| W1 | dirty CSS generated files and `generated_real_typed.rs`; all JSON guard rows | JSON formulas; generated checks either clean or intrinsically blocked | dirty report includes exact `git status --short`, broad command, owner, disposition | revert generated/report slice; save rejected patch path in REDRESS |
| W2 | Lock 14 roots; JSON guard | JSON formulas | scan report includes roots, exclusions, reasons, owners, self-scan, affected rows, disposition | revert scan/gate change and record intrinsic block |
| W3 | 24 CSS rows; JSON guard | JSON formulas; CSS no-admission | gate rejects `CSS_GENERATED_RS`, fact-stream, `CssFullParseSummary`, `parse_full`, brace/FNV/broadcast proof as admission | revert CSS report/gate edits |
| W4 | 24 CSS rows | no speed floor; provider source must be grammar-derived | CSS provider report points to `grammar/css/l4/*.bbnf` and generated metadata | revert provider/generator/generated CSS outputs |
| W5 | 24 CSS rows | equality only; speed ignored | typed summaries, pass/error counts, and value/document surfaces match cssparser same workload | revert typed API/comparator slice; CSS remains OPEN |
| W6 | 24 CSS rows plus JSON guard | CSS Track 1 >= cssparser + 1.000; JSON formulas | admitted CSS rows are row-local typed equal same-workload rows; `admitted_rows=24` or REDRESS miss table | revert speed intervention and results/redress slice |
| W7 | Pattern H file set; JSON guard | count exactly 67 | roundtrip report proves generator-owned source; header-only rejects | revert report/generator slice |
| W8 | Pattern H generated outputs; JSON guard | count 67 and byte-equivalent roundtrip | deletion/replacement only with generator restoration no later than the same wave | revert generated runtime/template slice |
| W9 | Decision/BackendShape reports; JSON guard | JSON formulas | e-graph rewrite, non-tautological CSP, all-five lowerer proof, no sixth shape | revert decision/lowerer slice |
| W10 | selected primitive/tape rows | target table below plus full JSON formulas | scalar oracle, strict checkasm/parity where native, same-wave consumer, no pre-block reopen | revert primitive/consumer/generated output/results together |
| W11 | all close axes | all prior gates still green | close audit accepts or records intrinsic block for every axis | revert close docs if overclaim detected |

## Section 3 - Falsifiability Binding

Planning floors for common W10 targets, rebound by W0:

| Row | Open Track1 | Sonic strict | Track1 floor | Open Track2 | Track2 floor |
|---|---:|---:|---:|---:|---:|
| `twitter/parse_only` | 8349.290 | 4913.095 | 8182.304 | 4558.264 | 4467.099 |
| `github_events/parse_only` | 8148.582 | 5014.433 | 7985.610 | 5092.727 | 4990.872 |
| `update_center/parse_only` | 5671.345 | 4707.613 | 5557.918 | 2837.898 | 2781.140 |
| `random/parse_only` | 3093.724 | 2937.264 | 3031.850 | 2414.011 | 2365.731 |
| `gsoc-2018/parse_only` | 13213.304 | 11355.449 | 12949.038 | 6976.158 | 6836.635 |
| `y_string_unicode/parse_only` | 3169.901 | 2417.909 | 3106.503 | 2326.950 | 2280.411 |
| `canada/parse_only` | 16709.901 | 12970.929 | 16375.703 | 8631.499 | 8458.869 |
| `numbers/parse_only` | 14472.308 | 7452.774 | 14182.862 | 7360.584 | 7213.372 |
| `unicode_mixed/parse_only` | 7379.340 | 7011.268 | 7231.753 | 5656.917 | 5543.779 |
| `unicode_escapes/parse_only` | 7897.449 | 2984.079 | 7739.500 | 7849.500 | 7692.510 |
| `unicode_basic/parse_only` | 9445.728 | 7059.901 | 9256.813 | 5420.279 | 5311.873 |
| `canada/direct_to_struct` | 4749.599 | 2733.746 | 4654.607 | 3479.489 | 3409.899 |
| `canada/real_typed_struct` | 4761.909 | 2736.418 | 4666.671 | 3397.878 | 3329.920 |
| `unicode_escapes/direct_to_struct` | 2357.459 | 1852.453 | 2310.310 | 3858.621 | 3781.449 |
| `unicode_escapes/real_typed_struct` | 2244.473 | 2036.703 | 2199.584 | 3939.118 | 3860.336 |
| `gsoc-2018/direct_to_struct` | 7228.198 | 6669.742 | 7083.634 | 6036.352 | 5915.625 |
| `gsoc-2018/real_typed_struct` | 7176.742 | 6627.652 | 7033.207 | 6233.927 | 6109.248 |

CSS row set for W5/W6: `declaration_values`, `declarations`,
`stylesheet_root`, `selectors`, `at_rules_keyframes`, `nested_rules`,
`css_variables`, `calc_expressions`, `var_url_functions`, `color_functions`,
`gradients`, `transforms`, `filters`, `easing_functions`, `media_queries`,
`vendor_prefixes`, `custom_at_rules`, `pseudo_classes`, `pseudo_elements`,
`attribute_selectors`, `logical_properties`, `grid`, `flexbox`, and
`typed_property_groups`.

## Section 4 - Pre-Blocked Routes

The gates reject CSS broadcast admission, brace-counter proof, fact-stream
proof, string-literal generated proof, FNV production migration, dirty
generated files as proof, x86 implementation scope, retained sidecars/cursors,
aux density/projection tables, parser-owned structural streams, public
`UnionTape`, sixth `BackendShape`, PMULL/CSSC promotion from ISA/checkasm
alone, decoded-string, structural-stream, string64, fixed-shape Unicode retry,
and numeric shortcut routes without fresh row-local proof.

## Section 5 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v16/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v16/HANDOFF.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v16/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
