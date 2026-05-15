# SK-V6 reinforcement cohort B3: retained parse three-way profile

Date: 2026-05-15. Workspace: `/Users/mkbabb/Programming/bbnf-lang`. Source mode: read-only; only `/tmp` build/profile/report artifacts were created. Existing dirty files were present before and after: `skinny/crates/bbnf-bench/src/metadata.rs`, `skinny/xtask/src/bin/capacity_probe.rs`.

Scope: retained parse Track 1 (`runtime::generated_json::parse`) on `twitter`, `gsoc-2018`, and `unicode_escapes`. BBNF Samply captures use `profile-lazy` built with `runtime/parse-attribution`; those profiles are for attribution, while the three-way throughput comparison uses the current baseline authority in `skinny/RESULTS.md`. c/B assumes a 3.5 GHz Apple performance core (`c/B = 28000 / Mbps`).

## Artifacts

- BBNF attribution binary: `/tmp/skv6-B3-profile-target/release/profile-lazy`
- BBNF build command: `CARGO_TARGET_DIR=/tmp/skv6-B3-profile-target cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution`
- BBNF profiles: `/tmp/skv6-B3-profiles/twitter.profile.json.gz`, `/tmp/skv6-B3-profiles/gsoc-2018.profile.json.gz`, `/tmp/skv6-B3-profiles/unicode_escapes.profile.json.gz` plus matching `.syms.json` files
- Current baseline table: `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md`
- Criterion excerpts present: `/Users/mkbabb/Programming/bbnf-lang/skinny/target/criterion/json_<corpus>/{track1_generated,sonic_rs_anchor,simd_json_borrowed,simd_json_owned}/new/estimates.json`
- Existing sonic-rs Samply profiles used for hot-leaf context where present: `/Users/mkbabb/Programming/bbnf-lang/skinny/profile/sonic-rs-expanded/twitter.value.inlined.profile.json.gz`, `/Users/mkbabb/Programming/bbnf-lang/skinny/profile/sonic-rs-expanded/unicode_escapes.value.inlined.profile.json.gz`
- No local Rust `simd-json` Samply profile files were found for these rows; comparison uses criterion/RESULTS throughput. Existing C++ `simdjson-expanded` profiles are not substituted for Rust `simd-json`.

## Current Retained Parse Three-Way

| corpus | bbnf Track 1 | bbnf c/B | sonic-rs | sonic c/B | simd-json borrowed | simd-b c/B | simd-json owned | simd-o c/B | read |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|
| twitter | 15597 Mbps | 1.80 | 21184 Mbps | 1.32 | 14744 Mbps | 1.90 | 12182 Mbps | 2.30 | bbnf is 0.74x sonic-rs, 1.06x simd-json borrowed |
| gsoc-2018 | 23161 Mbps | 1.21 | 43207 Mbps | 0.65 | 21038 Mbps | 1.33 | 18671 Mbps | 1.50 | bbnf is 0.54x sonic-rs, 1.10x simd-json borrowed |
| unicode_escapes | 12905 Mbps | 2.17 | 16048 Mbps | 1.74 | 4128 Mbps | 6.78 | 4292 Mbps | 6.52 | bbnf is 0.80x sonic-rs, 3.13x simd-json borrowed |

All three retained rows are still `G / NO-GO` in `skinny/RESULTS.md`, anchored against sonic-rs. BBNF already clears Rust `simd-json` borrowed on these rows, but does not clear sonic-rs.

## Fresh BBNF Parse-Attribution Runs

| corpus | iters | samples | profile Mbps | profile c/B | hot leaves >=1% | dominant class mix |
|---|---:|---:|---:|---:|---:|---|
| twitter | 4750 | 8566 | 11182 | 2.50 | 12 | string 61.2%, parse-driver 24.0%, tape 4.7%, number 2.5% |
| gsoc-2018 | 901 | 4961 | 19615 | 1.43 | 6 | string 84.2%, parse-driver 10.0%, tape 2.5% |
| unicode_escapes | 2854 | 8671 | 11068 | 2.53 | 3 | string 91.4%, parse-driver 4.0%, tape 1.6%, number 1.1% |

The parse-attribution build makes individual generated helpers visible, so its Mbps should not replace the canonical gate numbers above.

## BBNF Hot Leaves

### twitter

| self% | leaf |
|---:|---|
| 42.26% | `match_tiny_plain_string` |
| 18.13% | `match_string_at_quote` |
| 9.82% | `consume_container_next` |
| 5.48% | `parse_key_colon` |
| 4.51% | `ParserState::emit_plain_offset` |
| 4.05% | `consume_quote_at_cursor` |
| 3.15% | `dispatch_value` |
| 2.16% | `match_number_at_digit` |
| 2.02% | `_platform_memcmp` |
| 1.19% | `consume_structural` |
| 1.10% | `skip_ws` |
| 1.03% | `parse_literal` |

### gsoc-2018

| self% | leaf |
|---:|---|
| 59.54% | `match_string_at_quote` |
| 23.87% | `match_tiny_plain_string` |
| 3.47% | `consume_container_next` |
| 3.39% | `parse_key_colon` |
| 2.62% | `consume_quote_at_cursor` |
| 2.36% | `ParserState::emit_plain_offset` |

### unicode_escapes

| self% | leaf |
|---:|---|
| 90.44% | `match_string_at_quote` |
| 1.21% | `ParserState::emit_plain_offset` |
| 1.01% | `consume_quote_at_cursor` |

## Competitor Profile Context

- sonic-rs local Samply exists for `twitter` and `unicode_escapes` only. `twitter.value.inlined`: one fused object parser leaf dominates (`Parser::parse_object`, 80.57% self), with `simdutf8` 7.03%, memmove 5.85%, memcmp 2.58%, and array parser 2.30%. `unicode_escapes.value.inlined`: `Parser::parse_object` 52.74% and `handle_unicode_codepoint_mut` 40.23% dominate.
- No sonic-rs Samply profile for `gsoc-2018` was found locally; only current criterion/RESULTS throughput is available for that row.
- Rust `simd-json` local data for these rows is criterion throughput only. It is behind bbnf on all three current retained rows, especially `unicode_escapes` (4.1-4.3 Gbps vs bbnf 12.9 Gbps), but there are no local Rust `simd-json` hot leaves to attribute.

## Next Intervention Implications

1. Scanner work is not the next retained lever for this cohort. In the parse-attribution profiles, `simd_scan::scan_json_parse_index` is not a hot leaf; the live cost is generated string/key handling and quote/string matching.
2. `twitter` is split between tiny-string probing and fallback string matching. The actionable shape is the key/string path around `match_tiny_plain_string`, `consume_quote_at_cursor`, `parse_key_colon`, and `consume_container_next`, not another side table or broad structural prepass.
3. `gsoc-2018` is overwhelmingly string-body matching (`match_string_at_quote` + tiny-string = 83.4%). Before code work, a sonic-rs `gsoc-2018` profile would be the most useful missing comparator because sonic-rs is the row anchor and no local hot-leaf capture exists.
4. `unicode_escapes` is a single-lane escape/string problem: `match_string_at_quote` alone is 90.4% self. Tape emission, container control, and numeric parsing are too small to close this row. Any retained intervention has to live inside the trusted string/escape matcher and explain surrogate/escape-heavy behavior explicitly.
5. Three-way outcome: bbnf is already ahead of Rust `simd-json` on these rows, so the reinforcement target is sonic-class string fusion, especially reducing helper fan-out on object/string workloads while preserving the current single-substrate retained tape contract.
