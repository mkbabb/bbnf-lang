# SK-V14 W11S Parse-Only Stage-0 Attribution

Date: 2026-05-27.

Status: ATTRIBUTION ONLY. No source patch lands, no `skinny/RESULTS.md` row
moves, and `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Scope

W11S ran a same-HEAD release-native Stage-0 profile for the six remaining JSON
`parse_only` residual rows: `twitter`, `github_events`, `update_center`,
`random`, `gsoc-2018`, and `distinct_values`.

The goal was to prove the next source attempt's ownership before editing. This
is materially different from W11Q's indexed string skip and from the abandoned
cap-only parse_only tiny-string replay: W11S does not test a source patch. It
only attributes the hot leaves of the current generated validator.

## Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`:

```sh
CARGO_TARGET_DIR=/tmp/skv14-stage0-parse-only-target \
  RUSTC_WRAPPER= \
  RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct \
  --features runtime/parse-attribution

for corpus in twitter github_events update_center random gsoc-2018 distinct_values; do
  samply record --no-open --save-only --unstable-presymbolicate -r 4000 \
    -o "$ROOT/profiles/parse_only__${corpus}__track1.json.gz" \
    -- /tmp/skv14-stage0-parse-only-target/release/profile_direct \
    "$iters" "$corpus" parse_only_track1 0
done

PYTHONPATH=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v13/research/p1/support \
  python3 - "$ROOT" <extract_hotleaf_top20.py top_rows harness>
```

Retained temp root:
`/tmp/skv14-stage0-parse-only-20260527T190115Z`.

## Identity

```text
samply 0.13.1
6aadc08cbc0e50ea13b24ada5983eef0d1fef05b
a0dce9abdb396aea0fd4e0be64143c89ae00518883600ead13c71f930cfadbe4  /tmp/skv14-stage0-parse-only-target/release/profile_direct
```

The source HEAD is the W11R close commit. This evidence commit itself is
documentation-only.

## Probe Results

| corpus | Mbps | ns/byte | cycles/byte | instructions | checksum |
|---|---:|---:|---:|---:|---:|
| `twitter` | 3908.146 | 2.047006 | 3.680742 | 20710042125 | 0 |
| `github_events` | 4645.777 | 1.721994 | 3.357478 | 19552921642 | 0 |
| `update_center` | 3093.490 | 2.586076 | 4.638935 | 27429010693 | 0 |
| `random` | 2877.965 | 2.779742 | 5.569139 | 38087176317 | 0 |
| `gsoc-2018` | 12245.516 | 0.653300 | 1.767418 | 12612031302 | 0 |
| `distinct_values` | 3739.926 | 2.139080 | 6.190405 | 30047833178 | 0 |

## Top Leaves

| corpus | rank | pct | samples | total | function |
|---|---:|---:|---:|---:|---|
| `twitter` | 1 | 44.99 | 6608 | 14687 | `runtime::generated_json::generated::parse_only_string` |
| `twitter` | 2 | 17.68 | 2597 | 14687 | `runtime::generated_json::generated::parse_only_consume_container_next` |
| `twitter` | 3 | 8.86 | 1301 | 14687 | `runtime::generated_json::generated::parse_only_key_colon` |
| `twitter` | 4 | 7.70 | 1131 | 14687 | `runtime::generated_json::generated::parse_only_value_iterative` |
| `twitter` | 5 | 5.37 | 789 | 14687 | `runtime::generated_json::generated::match_number_at_digit` |
| `twitter` | 6 | 4.91 | 721 | 14687 | `runtime::generated_json::generated::parse_only_begin_value` |
| `twitter` | 7 | 3.08 | 452 | 14687 | `runtime::generated_json::generated::parse_only_take_structural` |
| `twitter` | 8 | 2.10 | 309 | 14687 | `runtime::generated_json::generated::parse_only_skip_ws` |
| `twitter` | 9 | 1.65 | 242 | 14687 | `_platform_memcmp` |
| `twitter` | 10 | 1.39 | 204 | 14687 | `runtime::generated_json::generated::parse_only_literal` |
| `github_events` | 1 | 55.87 | 7279 | 13028 | `runtime::generated_json::generated::parse_only_string` |
| `github_events` | 2 | 10.88 | 1418 | 13028 | `runtime::generated_json::generated::parse_only_key_colon` |
| `github_events` | 3 | 10.20 | 1329 | 13028 | `runtime::generated_json::generated::parse_only_consume_container_next` |
| `github_events` | 4 | 6.32 | 824 | 13028 | `runtime::generated_json::generated::parse_only_value_iterative` |
| `github_events` | 5 | 6.06 | 790 | 13028 | `runtime::generated_json::generated::parse_only_begin_value` |
| `github_events` | 6 | 4.71 | 613 | 13028 | `runtime::generated_json::generated::match_number_at_digit` |
| `github_events` | 7 | 2.52 | 328 | 13028 | `runtime::generated_json::generated::parse_only_take_structural` |
| `github_events` | 8 | 1.30 | 169 | 13028 | `runtime::generated_json::generated::parse_only_skip_ws` |
| `github_events` | 9 | 0.35 | 45 | 13028 | `runtime::generated_json::generated::parse_only_consume` |
| `github_events` | 10 | 0.34 | 44 | 13028 | `runtime::generated_json::generated::parse_only_consume_array_next` |
| `update_center` | 1 | 57.95 | 12722 | 21953 | `runtime::generated_json::generated::parse_only_string` |
| `update_center` | 2 | 9.30 | 2042 | 21953 | `runtime::generated_json::generated::parse_only_consume_container_next` |
| `update_center` | 3 | 7.88 | 1730 | 21953 | `runtime::generated_json::generated::parse_only_value_iterative` |
| `update_center` | 4 | 6.73 | 1477 | 21953 | `runtime::generated_json::generated::parse_only_key_colon` |
| `update_center` | 5 | 6.66 | 1463 | 21953 | `runtime::generated_json::generated::parse_only_take_structural` |
| `update_center` | 6 | 6.48 | 1423 | 21953 | `runtime::generated_json::generated::parse_only_begin_value` |
| `update_center` | 7 | 2.93 | 644 | 21953 | `runtime::generated_json::generated::parse_only_consume_array_next` |
| `update_center` | 8 | 1.15 | 253 | 21953 | `runtime::generated_json::generated::parse_only_skip_ws` |
| `update_center` | 9 | 0.41 | 91 | 21953 | `runtime::generated_json::generated::parse_only_consume` |
| `update_center` | 10 | 0.16 | 36 | 21953 | `runtime::generated_json::generated::parse_only_literal` |
| `random` | 1 | 31.62 | 7116 | 22507 | `runtime::generated_json::generated::parse_only_string` |
| `random` | 2 | 11.92 | 2683 | 22507 | `runtime::generated_json::generated::parse_only_consume_container_next` |
| `random` | 3 | 10.66 | 2399 | 22507 | `runtime::generated_json::generated::parse_only_consume_array_next` |
| `random` | 4 | 10.07 | 2266 | 22507 | `runtime::generated_json::generated::parse_only_key_colon` |
| `random` | 5 | 9.04 | 2034 | 22507 | `runtime::generated_json::generated::match_number_at_digit` |
| `random` | 6 | 8.13 | 1830 | 22507 | `runtime::generated_json::generated::parse_only_begin_value` |
| `random` | 7 | 7.66 | 1723 | 22507 | `runtime::generated_json::generated::parse_only_value_iterative` |
| `random` | 8 | 5.68 | 1279 | 22507 | `runtime::generated_json::generated::parse_only_take_structural` |
| `random` | 9 | 3.00 | 676 | 22507 | `runtime::generated_json::generated::parse_only_skip_ws` |
| `random` | 10 | 0.92 | 208 | 22507 | `runtime::generated_json::generated::parse_only_number` |
| `gsoc-2018` | 1 | 80.25 | 4194 | 5226 | `runtime::generated_json::generated::parse_only_string` |
| `gsoc-2018` | 2 | 6.30 | 329 | 5226 | `runtime::generated_json::generated::parse_only_consume_container_next` |
| `gsoc-2018` | 3 | 3.98 | 208 | 5226 | `runtime::generated_json::generated::parse_only_value_iterative` |
| `gsoc-2018` | 4 | 3.54 | 185 | 5226 | `runtime::generated_json::generated::parse_only_key_colon` |
| `gsoc-2018` | 5 | 2.10 | 110 | 5226 | `runtime::generated_json::generated::parse_only_begin_value` |
| `gsoc-2018` | 6 | 1.66 | 87 | 5226 | `runtime::generated_json::generated::parse_only_take_structural` |
| `gsoc-2018` | 7 | 1.59 | 83 | 5226 | `runtime::generated_json::generated::parse_only_skip_ws` |
| `gsoc-2018` | 8 | 0.23 | 12 | 5226 | `read` |
| `gsoc-2018` | 9 | 0.21 | 11 | 5226 | `runtime::generated_json::generated::parse_only_consume` |
| `gsoc-2018` | 10 | 0.04 | 2 | 5226 | `write` |
| `distinct_values` | 1 | 68.15 | 10736 | 15753 | `runtime::generated_json::generated::parse_only_string` |
| `distinct_values` | 2 | 11.01 | 1735 | 15753 | `runtime::generated_json::generated::parse_only_key_colon` |
| `distinct_values` | 3 | 8.39 | 1322 | 15753 | `runtime::generated_json::generated::parse_only_consume_container_next` |
| `distinct_values` | 4 | 7.09 | 1117 | 15753 | `runtime::generated_json::generated::parse_only_value_iterative` |
| `distinct_values` | 5 | 3.08 | 485 | 15753 | `runtime::generated_json::generated::parse_only_begin_value` |
| `distinct_values` | 6 | 0.71 | 112 | 15753 | `runtime::generated_json::generated::match_number_at_digit` |
| `distinct_values` | 7 | 0.61 | 96 | 15753 | `runtime::generated_json::generated::parse_only_take_structural` |
| `distinct_values` | 8 | 0.36 | 56 | 15753 | `runtime::generated_json::generated::parse_only_consume_array_next` |
| `distinct_values` | 9 | 0.19 | 30 | 15753 | `runtime::generated_json::generated::parse_only_skip_ws` |
| `distinct_values` | 10 | 0.15 | 24 | 15753 | `runtime::generated_json::generated::parse_only_consume` |

## Interpretation

The `runtime/parse-attribution` feature took effect: the rank-1 leaf is not
`dispatch_value`, `profile_direct::run_once`, or timer overhead. The dominant
current hot leaf for every remaining parse_only residual row is
`parse_only_string`:

| corpus | rank-1 share |
|---|---:|
| `twitter` | 44.99% |
| `github_events` | 55.87% |
| `update_center` | 57.95% |
| `random` | 31.62% |
| `gsoc-2018` | 80.25% |
| `distinct_values` | 68.15% |

Next parse_only implementation work must therefore target the generated
string validator or its shared `parse-that-regex` termination primitive. Pure
delimiter plumbing, whitespace skipping, object/array dispatch reshaping, and
cap-only tiny-string widening are not justified by this attribution without a
new material string-scanner differential.
