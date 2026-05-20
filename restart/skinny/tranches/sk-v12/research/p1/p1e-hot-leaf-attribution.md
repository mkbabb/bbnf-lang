# SK-V12 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V12.
Date: 2026-05-20.
Scope: synthesize SK-V12-open hot-family attribution from fresh
`/tmp/skv12-p1` measurement facts plus behavior-equivalent SK-V11 accepted
source maps.
Output: this file.
Baseline: SK-V12-open (`50bd1648`).
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: release profile with debug symbols; `RUSTFLAGS="-C target-cpu=native"`;
target directory `/tmp/skv12-profile-target-50bd1648`.
Profile tool: `samply 0.13.1`, retained `xctrace` Time Profiler / CPU Counters
trace bundles, and `proc_pid_rusage` PMU rows under `/tmp/skv12-p1`.
Corpus coverage: 17/17 parse rows, 17/17 direct rows, 7/7 typed rows.

Shared capture provenance:

- Capture root: `/tmp/skv12-p1`.
- Completion markers: PMU parse at `2026-05-20T06:34:59Z`, PMU product at
  `2026-05-20T06:38:35Z`, samply at `2026-05-20T06:41:16Z`, and xctrace at
  `2026-05-20T06:54:56Z`.
- Host/toolchain observed during this synthesis:
  `macOS 26.4.1; aarch64-apple-darwin; Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Artifact count: 82 samply `.json.gz` files, 82 companion `.json.syms.json`
  files, and 164 retained xctrace trace bundles across parse/direct/typed
  lanes and Time Profiler / CPU Counters templates.
- Result authority remains `skinny/RESULTS.md` plus REDRESS 119/120. PMU
  throughput and cycles-per-byte are diagnostic planning facts, not row
  admission facts.

## Section 1 - Method

Read-only inputs:

```sh
sed -n '1,300p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,340p' restart/skinny/tranches/sk-v12/SYNTHESIS.md
sed -n '1,180p' restart/skinny/tranches/sk-v12/HANDOFF.md
sed -n '1,90p' restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md
sed -n '1,180p' skinny/RESULTS.md
sed -n '3280,3565p' skinny/REDRESS.md
sed -n '1,420p' restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md
find restart/skinny/tranches/sk-v11/research/p1/hardening -type f | sort
sed -n '1,220p' /tmp/skv12-p1/pmu/capture_status.tsv
sed -n '1,120p' /tmp/skv12-p1/pmu/parse_pmu_rows.tsv
sed -n '1,160p' /tmp/skv12-p1/pmu/product_pmu_rows.tsv
sed -n '1,80p' /tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv
find /tmp/skv12-p1 -maxdepth 4 -type f | sort
find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
rg -n "ensure_runtime_profile|emit_from_source|emit_typed_from_source" skinny/crates/codegen/src
rg -n "skip_ascii_whitespace|match_string_at_quote_trusted_utf8|unescape_string|read_hex_unit_scalar|hex_nibble" skinny/crates/parse-that-regex/src/lib.rs
rg -n "scan_digit_run|match_number_span_from_first|materialize_u64|materialize_f64" skinny/crates/parse-that-regex/src/number/mod.rs
rg -n "dispatch_value|match_tiny_plain_string_with_cap|parse_array_element_at_direct|parse_object_value_at_direct" skinny/crates/runtime/src/grammars/json/generated.rs
```

This artifact intentionally does not wait for sibling SK-V12 P1-A through P1-D
documents, because those may be in progress. It uses raw `/tmp/skv12-p1`
capture/status data, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and the accepted
SK-V11 P1-E/hardening source maps.

### Evidence Lanes

| Lane | Fresh SK-V12 authority | What it can prove here |
|---|---|---|
| PMU TSVs | `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`, `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` | fresh cycles, instructions, c/B, CPI, and probe Mbps shape |
| Capture status | `/tmp/skv12-p1/pmu/capture_status.tsv` | artifact coverage and return-code shape |
| Samply profiles | `/tmp/skv12-p1/samply/{parse,direct,typed}/*.json.gz` plus `.syms.json` | retained sample/profile artifacts and source symbol maps |
| Xctrace traces | `/tmp/skv12-p1/{parse-xctrace,direct-xctrace}/{time-profiler,cpu-counters}/*.trace` | retained trace bundles, not exported inline percentage tables |
| Source attribution | SK-V11 P1-E and hardening, verified against current source paths | grammar-neutral primitive family attribution for behavior-equivalent JSON code |

Critical caveat: the retained SK-V12 samply profiles report
`symbolicated=false`, and there are no `/tmp/skv12-p1/.../exports/summary.json`
files equivalent to the SK-V11 xctrace summaries. The companion `.syms.json`
files resolve symbol maps, but this P1-E has no exact per-inlined-frame
self-time percentages. Therefore the row tables below do not fabricate
percentages. Hardening must either accept this as a source-map attribution fold
or require a fresh symbolicated summary export before demanding exact inline
percentages.

## Section 2 - Fresh Measurement Facts

Final capture status:

| Family | Rows | Status |
|---|---:|---|
| `pmu-parse` | 34 | PASS `rc=0` |
| `pmu-direct` | 34 | PASS `rc=0` |
| `pmu-typed` | 14 | PASS `rc=0` |
| `samply-parse` | 34 | PASS `rc=0` |
| `samply-direct` | 34 | PASS `rc=0` |
| `samply-typed` | 14 | PASS `rc=0` |
| `xctrace-time-profiler-parse` | 34 | PASS `rc=54` |
| `xctrace-time-profiler-direct` | 34 | PASS `rc=54` |
| `xctrace-time-profiler-typed` | 14 | PASS `rc=54` |
| `xctrace-cpu-counters-parse` | 34 | PASS `rc=54` |
| `xctrace-cpu-counters-direct` | 34 | PASS, 32 rows `rc=54`, 2 rows `rc=0` |
| `xctrace-cpu-counters-typed` | 14 | PASS `rc=54` |

The `rc=54` xctrace rows are retained time-limit trace captures, not failed
status rows. Example logs show "Reached specified time limit" for parse
captures and "Target app exited" for direct captures, followed by saved `.trace`
bundles.

Initial product PMU capture had a cwd/fixture-location failure:
`pmu-direct` 34/34 and `pmu-typed` 14/14 failed at `rc=134`, with
`profile_direct` unable to locate fixtures from the initial working directory.
The rerun rows in `capture_status.tsv` are the final authority and all product
PMU rows pass at `rc=0`.

PMU aggregate shape:

| Plane | Rows | Aggregate c/B | Aggregate CPI |
|---|---:|---:|---:|
| parse | 34 | 2.938593 | 0.204887 |
| direct | 34 | 4.331411 | 0.183717 |
| typed guards | 14 | 3.123173 | 0.185056 |

The PMU TSV schema exposes cycles, instructions, c/B, CPI, user ns, system ns,
and checksums. It does not expose branch-miss, L1, or LLC columns. This P1-E
does not infer missing counter classes.

### Fresh c/B Tables

Parse c/B:

| Corpus | Track 1 / Track 2 c/B |
|---|---:|
| `twitter` | 2.214 / 2.845 |
| `citm_catalog` | 1.123 / 1.653 |
| `canada` | 1.933 / 2.076 |
| `apache_builds` | 2.737 / 2.841 |
| `github_events` | 2.281 / 2.657 |
| `update_center` | 2.893 / 3.735 |
| `mesh` | 2.653 / 2.803 |
| `random` | 3.519 / 4.407 |
| `gsoc-2018` | 1.481 / 1.572 |
| `marine_ik` | 2.556 / 2.798 |
| `instruments` | 2.028 / 2.933 |
| `numbers` | 1.742 / 1.812 |
| `unicode_mixed` | 4.297 / 3.893 |
| `unicode_escapes` | 2.819 / 2.726 |
| `unicode_basic` | 2.865 / 3.229 |
| `distinct_values` | 3.585 / 5.684 |
| `y_string_unicode` | 5.622 / 5.901 |

Direct c/B:

| Corpus | Track 1 / Track 2 c/B |
|---|---:|
| `twitter` | 2.950 / 3.200 |
| `citm_catalog` | 1.612 / 1.717 |
| `canada` | 3.254 / 3.366 |
| `apache_builds` | 3.058 / 3.374 |
| `github_events` | 2.830 / 3.092 |
| `update_center` | 4.120 / 4.597 |
| `mesh` | 3.956 / 3.832 |
| `random` | 4.403 / 4.890 |
| `gsoc-2018` | 2.336 / 2.427 |
| `marine_ik` | 3.650 / 3.593 |
| `instruments` | 2.863 / 3.099 |
| `numbers` | 2.703 / 2.761 |
| `unicode_mixed` | 7.454 / 7.663 |
| `unicode_escapes` | 6.722 / 6.846 |
| `unicode_basic` | 3.768 / 4.161 |
| `distinct_values` | 5.469 / 6.209 |
| `y_string_unicode` | 9.993 / 11.302 |

Typed c/B:

| Corpus | Track 1 / Track 2 c/B |
|---|---:|
| `twitter` | 1.881 / 2.124 |
| `citm_catalog` | 0.964 / 1.815 |
| `apache_builds` | 4.088 / 6.081 |
| `github_events` | 2.706 / 3.000 |
| `update_center` | 2.798 / 3.515 |
| `mesh` | 3.694 / 4.732 |
| `marine_ik` | 2.932 / 3.396 |

## Section 3 - Grammar-Neutral Primitive Map

These names are the behavior-equivalent SK-V11 accepted source-level
attribution vocabulary, verified against current source paths. They are not
fresh SK-V12 inline percentage claims.

| Canonical primitive | Evidence members and current source loci |
|---|---|
| `bounded_plain_string_scan` | generated tiny matcher `match_tiny_plain_string_with_cap::<16>` / `::<8>` at `skinny/crates/runtime/src/grammars/json/generated.rs:171`; Track 2 tiny matcher at `skinny/crates/bbnf-bench/src/track2/json.rs:314`; hand direct tiny string at `skinny/crates/bbnf-bench/src/direct_struct.rs:565`; typed tiny/plain helpers at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811` and `:1825`; full-string scan support at `skinny/crates/parse-that-regex/src/lib.rs:162` and `:547` |
| `string_escape_decode` | `validate_string_escape`, `validate_unicode_escape_run`, and `unescape_string` at `skinny/crates/parse-that-regex/src/lib.rs:284`, `:347`, and `:718` |
| `unicode_escape_hex_decode` | `read_hex_unit_scalar` and `hex_nibble` at `skinny/crates/parse-that-regex/src/lib.rs:945` and `:959` |
| `number_digit_span` | `match_number_span_from_first`, `scan_digit_run`, `parse_eight_digits`, `materialize_u64`, and `materialize_f64` at `skinny/crates/parse-that-regex/src/number/mod.rs:38`, `:106`, `:214`, `:247`, and `:261` |
| `ascii_whitespace_skip` | `skip_ascii_whitespace` and `skip_ascii_spaces` at `skinny/crates/parse-that-regex/src/lib.rs:113` and `:128` |
| `container_dispatch` | generated `dispatch_value`, `parse_value_at`, `consume_structural`, `consume_container_next`, `consume_array_next`, `parse_object_value_at_direct`, and `parse_array_element_at_direct` at `skinny/crates/runtime/src/grammars/json/generated.rs:47`, `:37`, `:292`, `:310`, `:348`, `:468`, and `:508`; Track 2 `parse_value_at`, `parse_key_colon`, and `consume_container_next` at `skinny/crates/bbnf-bench/src/track2/json.rs:53`, `:97`, and `:271` |
| `simd_movemask` | `bbnf_simd::aarch64::movemask::movemask_u8x16` at `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`; trailing-zero helpers remain core support evidence, not separate behavior authority |
| `output_digest_hash` | `JsonDirectDigest::fold_string_scalar`, `hash_bytes`, and `mix`/`wrapping_add` support at `skinny/crates/bbnf-bench/src/direct_struct.rs:123`, `:717`, and `:739`; typed hash support `fold_opt_str`, `hash_str`, and `mix` at `skinny/crates/bbnf-bench/src/real_typed_struct.rs:687`, `:734`, and `:742` |

## Section 4 - Row Attribution

### Parse Diagnostic Rows

All parse rows remain diagnostic only: 16 `S / NO-GO` and `canada` as
`L / NO-GO`. No parse-only row can admit SK-V12 or reopen W3/parse-only
routes.

| Corpus | Fresh parse c/B T1/T2 | Behavior-equivalent primitive family | SK-V12 treatment |
|---|---:|---|---|
| `twitter` | 2.214 / 2.845 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `simd_movemask` | diagnostic string row |
| `citm_catalog` | 1.123 / 1.653 | `ascii_whitespace_skip`, `bounded_plain_string_scan`, `container_dispatch` | diagnostic whitespace/string row |
| `canada` | 1.933 / 2.076 | `number_digit_span`, `container_dispatch` | diagnostic `L / NO-GO` anomaly, not SOTA close |
| `apache_builds` | 2.737 / 2.841 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `simd_movemask` | diagnostic short-string row |
| `github_events` | 2.281 / 2.657 | `bounded_plain_string_scan`, `simd_movemask`, `ascii_whitespace_skip` | diagnostic tiny-string row |
| `update_center` | 2.893 / 3.735 | `bounded_plain_string_scan`, `simd_movemask`, `container_dispatch` | diagnostic tiny-string row |
| `mesh` | 2.653 / 2.803 | `number_digit_span`, `container_dispatch`, `ascii_whitespace_skip` | diagnostic numeric/dispatch row |
| `random` | 3.519 / 4.407 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `container_dispatch` | diagnostic cold-sensitive string row |
| `gsoc-2018` | 1.481 / 1.572 | `simd_movemask`, `bounded_plain_string_scan` | diagnostic SIMD string-scan row |
| `marine_ik` | 2.556 / 2.798 | `container_dispatch`, `number_digit_span`, `ascii_whitespace_skip` | diagnostic numeric/dispatch row |
| `instruments` | 2.028 / 2.933 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `container_dispatch` | diagnostic string/whitespace row |
| `numbers` | 1.742 / 1.812 | `number_digit_span`, `container_dispatch` | diagnostic pure-number row |
| `unicode_mixed` | 4.297 / 3.893 | `string_escape_decode`, `container_dispatch`, `bounded_plain_string_scan` | diagnostic unicode row |
| `unicode_escapes` | 2.819 / 2.726 | `unicode_escape_hex_decode`, `string_escape_decode`, `bounded_plain_string_scan` | diagnostic unicode hex row |
| `unicode_basic` | 2.865 / 3.229 | `bounded_plain_string_scan`, `simd_movemask`, `container_dispatch` | diagnostic UTF-8 string row |
| `distinct_values` | 3.585 / 5.684 | `bounded_plain_string_scan`, `ascii_whitespace_skip` | diagnostic tiny-string dominant row |
| `y_string_unicode` | 5.622 / 5.901 | `unicode_escape_hex_decode`, `string_escape_decode`, `bounded_plain_string_scan` | diagnostic sparse unicode row |

### Direct Guard And Residual Rows

Direct rows retain the SK-V11 close state. Four rows are admitted guards;
thirteen are residual/pre-blocked `N-direct / NO-GO` rows. The fresh SK-V12 PMU
shape does not itself satisfy the REDRESS 114-119 reopen rule.

| Corpus | SK-V12 state | Fresh direct c/B T1/T2 | Behavior-equivalent primitive family | SK-V12 treatment |
|---|---|---:|---|---|
| `twitter` | residual | 2.950 / 3.200 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `simd_movemask` | pre-blocked by W5/W7/W8 fixpoint |
| `citm_catalog` | guard | 1.612 / 1.717 | `ascii_whitespace_skip`, `bounded_plain_string_scan`, `container_dispatch` | preserve direct guard |
| `canada` | residual | 3.254 / 3.366 | `number_digit_span`, `container_dispatch` | pre-blocked by W3/W8 fixpoint |
| `apache_builds` | guard | 3.058 / 3.374 | `output_digest_hash`, `bounded_plain_string_scan`, `ascii_whitespace_skip` | preserve strict measured-row guard |
| `github_events` | residual | 2.830 / 3.092 | `bounded_plain_string_scan`, `simd_movemask`, `ascii_whitespace_skip` | pre-blocked by W5/W7/W8 fixpoint |
| `update_center` | residual | 4.120 / 4.597 | `bounded_plain_string_scan`, `output_digest_hash`, `simd_movemask` | pre-blocked by W5/W7/W8 fixpoint |
| `mesh` | residual | 3.956 / 3.832 | `number_digit_span`, `container_dispatch`, `ascii_whitespace_skip` | pre-blocked by REDRESS 114 |
| `random` | residual | 4.403 / 4.890 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `output_digest_hash` | pre-blocked by REDRESS 115 plus W5/W7 |
| `gsoc-2018` | residual | 2.336 / 2.427 | `simd_movemask`, `bounded_plain_string_scan` | pre-blocked by W5/W7/W8 fixpoint |
| `marine_ik` | guard | 3.650 / 3.593 | `number_digit_span`, `container_dispatch`, `ascii_whitespace_skip` | preserve direct guard |
| `instruments` | W0-clamped residual | 2.863 / 3.099 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `container_dispatch` | docs-only admission pre-blocked |
| `numbers` | W0-clamped residual | 2.703 / 2.761 | `number_digit_span`, `container_dispatch` | W3 route rejected; no reopen |
| `unicode_mixed` | W0-clamped residual | 7.454 / 7.663 | `string_escape_decode`, `bounded_plain_string_scan`, `container_dispatch` | W6 route blocked; no reopen |
| `unicode_escapes` | residual | 6.722 / 6.846 | `string_escape_decode`, `unicode_escape_hex_decode`, `bounded_plain_string_scan` | pre-blocked by W5/W6 and proof-only limits |
| `unicode_basic` | guard | 3.768 / 4.161 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `simd_movemask` | preserve direct guard |
| `distinct_values` | residual | 5.469 / 6.209 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, `output_digest_hash` | pre-blocked by W5/W7/W8 fixpoint |
| `y_string_unicode` | residual | 9.993 / 11.302 | `unicode_escape_hex_decode`, `string_escape_decode` | pre-blocked by W5/W6/W8 fixpoint |

The highest fresh direct c/B pressure is still unicode/string shaped:
`y_string_unicode` at 9.993 / 11.302 c/B, `unicode_mixed` at
7.454 / 7.663 c/B, and `unicode_escapes` at 6.722 / 6.846 c/B. That is a
planning fact only. It is not fresh material evidence to dispatch a JSON
unicode/string residual route before the non-JSON priority resolves.

### Typed Guard Rows

All typed rows remain guard rows. Track 1 is generated typed product evidence;
Track 2 is independent oracle/comparator evidence and must not be folded into
generated product attribution.

| Corpus | Fresh typed c/B T1/T2 | Behavior-equivalent primitive family | SK-V12 treatment |
|---|---:|---|---|
| `twitter` | 1.881 / 2.124 | `bounded_plain_string_scan`, `ascii_whitespace_skip`, typed state/hash support | preserve typed guard |
| `citm_catalog` | 0.964 / 1.815 | `ascii_whitespace_skip`, `bounded_plain_string_scan`, typed skip-value support | preserve typed guard |
| `apache_builds` | 4.088 / 6.081 | `bounded_plain_string_scan`, typed state compare, `output_digest_hash` support | preserve typed guard |
| `github_events` | 2.706 / 3.000 | `bounded_plain_string_scan`, typed state/hash support | preserve typed guard |
| `update_center` | 2.798 / 3.515 | `bounded_plain_string_scan`, UTF-8/string support, typed state/hash support | preserve typed guard |
| `mesh` | 3.694 / 4.732 | `number_digit_span`, `ascii_whitespace_skip`, typed sequence support | preserve typed guard |
| `marine_ik` | 2.932 / 3.396 | `number_digit_span`, `ascii_whitespace_skip`, typed sequence support | preserve typed guard |

## Section 5 - SK-V12-Specific Planning Target

The primary SK-V12 planning target is not a JSON hot leaf. It is the generated
non-JSON baseline blocker carried by REDRESS 112/113 and made binding by
REDRESS 120, `SYNTHESIS.md`, `HANDOFF.md`, and G-Alpha.

Fresh inventory in the current tree still shows the blocker:

- `skinny/crates/codegen/src/json_provider.rs:4` accepts runtime emission only
  when `backend.grammar_name == "json"` and errors for any other grammar.
- `skinny/crates/codegen/src/lib.rs:108` and `:146` call
  `json_provider::ensure_runtime_profile` for direct and typed emission.
- `skinny/crates/runtime/src/grammars/` contains generated `json` and
  `sheets_witness`, but no generated `css_l4`, `css_l4_declaration_values`,
  `sheets`, or `bbnf_self` runtime module.
- `skinny/crates/bbnf-bench/src/report.rs` contains the accepted W1a non-JSON
  report lane for `css_l4`, `sheets`, and `bbnf_self`, but that lane is report
  consumption, not a generated Track 1 baseline.

Therefore S-P2/S-P3 should plan around standing up exactly one generated
non-JSON direct or typed baseline first, preferred order CSS L4 declaration
values, Sheets, then BBNF-self. A JSON-only micro-wave before that priority
succeeds or records a measured `BLOCKED` route would contradict SK-V12's
opening contract.

## Section 6 - REDRESS Pre-Blocks

| Temptation from the hot-family map | Prior route status | Required SK-V12 treatment |
|---|---|---|
| Structural rediscovery, retained classes, sidecar vectors, parser-owned cursors, streaming cursor, class column, or `UnionTape` | REDRESS 50, 51, 53, 96, 97, 98, 102, 114, 119, and 120 close or retire the W3/parse-only/substrate families | Do not reopen W3 or parse-only from `container_dispatch` / structural facts |
| Parse-only throughput, structural scan, masking probes, sidecar freshness, or PMU/cycles | REDRESS 102 and SK-V12 Section 4 classify these as nonproducer evidence | Keep as diagnostics only |
| Bounded string spans, decoded-byte source folds, single-quartet shortcut, StringBlock16, retained boundary collapse, or eager materialization | REDRESS 54, 55, 60-69, 72, 82, 83, 116, 117, and 119 block or constrain these families | `bounded_plain_string_scan`, `string_escape_decode`, and `unicode_escape_hex_decode` may guide research only after a legal row/oracle consumer exists |
| Parser-control carry, object next-key carry, object-pair value-byte compaction | REDRESS 63, 65, and 84 constrain or reject these routes | Treat key/container symbols as attribution, not dispatch authority |
| Generic numeric fallback or number slot reuse | REDRESS 80 and 114 reject the generic/numeric slot close route | `number_digit_span` stays a primitive family, not a reopened W3/W8 route |
| PMULL/CTZ/bitmap body fill from movemask/trailing-zero pressure | REDRESS 88, 89, and 90 reject default rewires/body-fill routes | `simd_movemask` may be a micro-proof candidate only with a same-wave consumer |
| Direct digest as typed proof or output host-sink retry | REDRESS 118 blocks the host-sink route; SK-V12 Section 4 bars direct digest as typed proof | `output_digest_hash` is guard/residual attribution only |
| JSON direct residual row movement | REDRESS 114-119 give measured fixpoint proof; REDRESS 120 routes SK-V12 elsewhere | Reopen only with fresh material evidence beyond REDRESS 114-119 and only after non-JSON priority resolves |
| Generated non-JSON report lane as generated baseline | REDRESS 111 admitted only the report/gate lane; REDRESS 112 rejected generated CSS L4 baseline; REDRESS 113 blocked the intervention | Treat the generated non-JSON runtime/codegen gap as the first planning target |

## Section 7 - Delta vs SK-V11

No `skinny/RESULTS.md` row moved between SK-V11 close and SK-V12-open. The
SK-V12 seed result surface remains:

| Family | State | P1-E consequence |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic hot-family evidence only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | guard preservation plus pre-blocked residual ledger |
| `real_typed_struct` | 7 `A / GO` | typed guard surface |
| generated non-JSON parser | no admitted generated baseline | primary SK-V12 blocker and planning target |
| overall | `N-direct / NoGo` | unchanged seed outcome |

Fresh SK-V12 PMU rows are materially useful for cost shape, but they do not
create new source-level material evidence against REDRESS 114-119. The
behavior-equivalent primitive families remain the SK-V11 accepted families:
`bounded_plain_string_scan`, `string_escape_decode`,
`unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`,
`container_dispatch`, `simd_movemask`, and `output_digest_hash`.

## Section 8 - Anomalies And Hardening Caveats

1. Exact SK-V12 per-inlined-frame self-time percentages are absent from the
   retained artifacts inspected here. The profiles and traces exist, and
   companion `.syms.json` files resolve symbol maps, but this artifact does not
   fabricate inline percentages from unsymbolicated sample profiles.
2. Hardening should decide whether to accept source-level attribution plus
   fresh PMU/capture facts for V1, or require a regenerated xctrace/samply
   summary export with exact inline percentages.
3. Xctrace rows are retained trace bundles with mostly `rc=54` time-limit
   return codes. P1-E treats them as artifacts, not as clean-exit percentage
   exports.
4. The initial product PMU run failed due fixture lookup from the wrong
   directory; final rerun PMU rows are all `rc=0`. This is a provenance caveat,
   not a row failure.
5. PMU rows expose cycles/instructions/c/B/CPI only. Branch/L1/LLC claims are
   absent and must not be inferred.
6. Typed Track 2 leaves remain comparator/oracle evidence. They cannot prove a
   generated typed Track 1 primitive or direct row.
7. The largest fresh direct c/B rows remain unicode/string shaped, but SK-V12
   explicitly routes away from JSON direct retries until the generated
   non-JSON baseline priority succeeds or honestly blocks.
8. Current non-JSON codegen/runtime inventory still matches REDRESS 112:
   `json_provider` is JSON-only and no generated CSS L4 runtime exists.
9. Exact SK-V12 capture build flags are not embedded in `/tmp/skv12-p1`;
   hardening should either accept the inherited SK-V11 release/profile
   discipline or require a regenerated capture manifest.

## Section 9 - Sources

- S-P1 profile prompt: `restart/prompts/skinny/PASS-1-PROFILE.md`.
- SK-V12 synthesis: `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- SK-V12 handoff: `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- G-Alpha: `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`.
- Result authority: `skinny/RESULTS.md`.
- REDRESS ledger through REDRESS 120: `skinny/REDRESS.md`.
- Accepted SK-V11 P1-E: `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`.
- Accepted SK-V11 hardening: `restart/skinny/tranches/sk-v11/research/p1/hardening/`.
- PMU status: `/tmp/skv12-p1/pmu/capture_status.tsv`.
- Initial product failure status:
  `/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv`.
- Parse PMU rows: `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`.
- Product PMU rows: `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`.
- Samply artifacts: `/tmp/skv12-p1/samply/{parse,direct,typed}/*.json.gz`
  and companion `.json.syms.json` files.
- Xctrace artifacts:
  `/tmp/skv12-p1/{parse-xctrace,direct-xctrace}/{time-profiler,cpu-counters}/*.trace`.
- Codegen blocker: `skinny/crates/codegen/src/json_provider.rs` and
  `skinny/crates/codegen/src/lib.rs`.
- Runtime grammar inventory: `skinny/crates/runtime/src/grammars/`.
- Runtime generated JSON source:
  `skinny/crates/runtime/src/grammars/json/generated.rs`.
- Track 2 JSON source: `skinny/crates/bbnf-bench/src/track2/json.rs`.
- Direct digest source: `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- Generated typed source: `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.
- Typed digest source: `skinny/crates/bbnf-bench/src/real_typed_struct.rs`.
- Parse primitive source: `skinny/crates/parse-that-regex/src/lib.rs` and
  `skinny/crates/parse-that-regex/src/number/mod.rs`.
- SIMD movemask source: `skinny/crates/bbnf-simd/src/aarch64/movemask.rs`.
