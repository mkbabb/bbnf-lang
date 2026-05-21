# SK-V13 S-P1 V3 Evidence Ledger

Pass: S-P1 Profile. Cycle: V3 fold support.
Date: 2026-05-21.
Scope: canonical row/primitive status ledger for S-P1 V3.
Output: this file.

## Status Vocabulary

| Status | Meaning |
|---|---|
| `json-parse-envelope` | A generated JSON parse wrapper is the hot leaf; not a grammar-neutral primitive. |
| `json-direct-envelope` | A generated JSON direct wrapper is the hot leaf; not a grammar-neutral primitive. |
| `resolved-json-unicode-candidate` | A named unicode/string primitive with file:line, JSON-confirmed only. |
| `json-scan-primitive-candidate` | A scanner primitive measured on JSON mode-III probes; non-JSON confirmation still absent. |
| `json-typed-only` | A generated JSON typed-product leaf; cannot generalize to CSS/Sheets. |
| `css-profiled-nonparser-overhead` | CSS row has equality/throughput/profile artefacts, but top leaves are timer/fact-sink overhead. |
| `timer/noise` | Top leaf is timing/system overhead, not parser work. |
| `missing-product-surface` | The row is absent from the generated product surface and is not profiled. |
| `function-only-sidecar` | Sidecar resolves a function name but no source file:line; not precise primitive attribution. |
| `unavailable_from_current_export` | Requested counter field is not emitted by the current xctrace export. |

All rows in this ledger are `profile_signal_not_gate_admission`.

## Direct-To-Struct Ledger

Profile pattern: `/tmp/skv13-p1-v2/samply/profiles/direct__{corpus}__track{1,2}.json.gz`.
Sidecar pattern: same basename with `.json.syms.json`.
Log pattern: `/tmp/skv13-p1-v2/samply/logs/direct__{corpus}__track{1,2}.log`.
Top-20 ledger: `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`.

| Row | Plane | Track 1 rank-1 | Self-time | File:line | Primitive class | Status | Non-JSON confirmed |
|---|---|---|---:|---|---|---|---|
| twitter | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 74.0% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| citm_catalog | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 58.4% | `skinny/crates/runtime/src/grammars/json/generated.rs:506` | dispatch envelope | `json-direct-envelope` | no |
| canada | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 85.3% | `skinny/crates/runtime/src/grammars/json/generated.rs:506` | dispatch envelope | `json-direct-envelope` | no |
| apache_builds | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 38.1% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| github_events | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 67.7% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| update_center | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 68.3% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| mesh | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 76.7% | `skinny/crates/runtime/src/grammars/json/generated.rs:506` | dispatch envelope | `json-direct-envelope` | no |
| random | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 37.7% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| gsoc-2018 | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 60.2% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| marine_ik | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 72.3% | `skinny/crates/runtime/src/grammars/json/generated.rs:506` | dispatch envelope | `json-direct-envelope` | no |
| instruments | direct_to_struct | `Option<&u8>::copied` | 58.3% | `core/src/option.rs:2141` | generic inline/noise | `function-only-sidecar` | no |
| numbers | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 76.1% | `skinny/crates/runtime/src/grammars/json/generated.rs:506` | dispatch envelope | `json-direct-envelope` | no |
| unicode_mixed | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 55.9% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| unicode_escapes | direct_to_struct | `parse_that_regex::unescape_string` | 46.7% | `skinny/crates/parse-that-regex/src/lib.rs:718` | unicode/string decode | `resolved-json-unicode-candidate` | no |
| unicode_basic | direct_to_struct | `parse_object_value_at_direct::<JsonDigestSink>` | 44.1% | `skinny/crates/runtime/src/grammars/json/generated.rs:466` | dispatch envelope | `json-direct-envelope` | no |
| distinct_values | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 49.5% | `skinny/crates/runtime/src/grammars/json/generated.rs:542` | dispatch envelope | `json-direct-envelope` | no |
| y_string_unicode | direct_to_struct | `parse_array_element_at_direct::<JsonDigestSink>` | 19.5% | `skinny/crates/runtime/src/grammars/json/generated.rs:506` | dispatch envelope | `json-direct-envelope`; Track 2 rank-1 is `timer/noise` | no |

## Parse-Only Ledger

V3 does not promote V1 parse save-only samples beyond their stated precision.
Profile pattern: `/tmp/skv13-p1/samply/profiles/parse__{corpus}__track{1,2}.json.gz`.

| Rows | Plane | Dominant Track 1 attribution | Status |
|---|---|---|---|
| 15/17 parse rows | parse_only | `runtime::generated_json::generated::dispatch_value` in `skinny/crates/runtime/src/grammars/json/generated.rs` | `json-parse-envelope` |
| distinct_values | parse_only | `match_tiny_plain_string_with_cap::<16>`; sidecar lacks file:line | `function-only-sidecar` |
| y_string_unicode | parse_only | `parse_that_regex::read_hex_unit_scalar` at `skinny/crates/parse-that-regex/src/lib.rs:946` | `resolved-json-unicode-candidate` |

## Typed Product Ledger

Typed profile pattern: `/tmp/skv13-p1/samply/profiles/typed__{corpus}__real_typed_track{1,2}.json.gz`.

| Row | Plane | Track 1 attribution | Status |
|---|---|---|---|
| twitter | real_typed_struct | `DirectParser::skip_value` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1739`) | `json-typed-only` |
| citm_catalog | real_typed_struct | `DirectParser::skip_value` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1739`) | `json-typed-only` |
| apache_builds | real_typed_struct | `parse_option_scalar_string` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1199`) | `json-typed-only` |
| github_events | real_typed_struct | `DirectParser::skip_value` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1740`) | `json-typed-only` |
| update_center | real_typed_struct | `parse_type_plugin` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:473`) | `json-typed-only` |
| mesh | real_typed_struct | `parse_type_mesh` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:828`) | `json-typed-only` |
| marine_ik | real_typed_struct | `parse_type_marine_geometry_data` (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1015`) | `json-typed-only` |
| canada, random, gsoc-2018, instruments, numbers, unicode_mixed, unicode_escapes, unicode_basic, distinct_values, y_string_unicode | real_typed_struct | no generated typed product surface in S-P1 | `missing-product-surface` |

## Mode-III Ledger

Mode-III profile pattern: `/tmp/skv13-p1-v2/mode3/profiles/mode3__{corpus}__{mode}.json.gz`.
All 85 captured mode-III rank-1 rows are citable by function and profile path,
but the V2 sidecar extraction lacks source file:line for the rank-1 row. They
therefore remain `function-only-sidecar` for CH1/CH6 file:line closure.

| Mode | Rows | Dominant rank-1 family | Status | Material boundary |
|---|---:|---|---|---|
| `host_call_eager_decode` | 17/17 | generated JSON parse/eager decode functions, function-only in top-20 TSV | `function-only-sidecar` | not a route; profile signal only |
| `alternate_scalar_plan` | 17/17 | `serde_json::Value` scalar-plan functions, function-only in top-20 TSV | `function-only-sidecar` | not a BBNF primitive |
| `cold_first_parse` | 17/17 | generated JSON parse functions, function-only in top-20 TSV | `function-only-sidecar` | cold-first signal only |
| `structural_scan_scalar` | 17/17 | `runtime::generated_json::scan::scan_tail` (`skinny/crates/runtime/src/grammars/json/scan.rs:107` source anchor) | `json-scan-primitive-candidate`; sidecar row is function-only | JSON structural-set only |
| `structural_scan_simd` | 17/17 | `scan_structurals` (`scan.rs:22`) or `bulk_emit_positions_64_neon` (`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2`) source anchors | `json-scan-primitive-candidate`; sidecar row is function-only | scanner micro-result only; REDRESS 96/97/98 not reopened |

Unsupported mode-III routes:

| Mode | Status |
|---|---|
| `alternate_pext_mask_plan` | unsupported: `aarch64_no_pext` |
| `alternate_dispatch_table_plan` | unsupported: `disabled_duplicate_probe` |

## CSS Ledger

| Row | Plane | Artefacts | Top profile leaves | Status |
|---|---|---|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | CSS L4 direct | `/tmp/skv13-p1-v2/css/logs/css_l4_declaration_values_all_modes.log`; `/tmp/skv13-p1-v2/css/profiles/css_l4_declaration_values_all_modes.json.gz`; sidecar with same basename | 17.6% `mach_absolute_time`; 13.7% `LocalFactSink::finish`; 7.5% `FactSink::finish` | `css-profiled-nonparser-overhead`; equality/throughput/profile exists, parser hot leaf unresolved |

## Counter Ledger

| Counter family | Status |
|---|---|
| cycles / instructions / c/B / CPI | available for V1 parse/direct/typed PMU and V2 direct/mode-III logs |
| branch misses | `unavailable_from_current_export` |
| L1 misses | `unavailable_from_current_export` |
| LLC misses | `unavailable_from_current_export` |

## REDRESS Guards

- Direct row profile signals do not reopen REDRESS 119/120. Every direct-row
  reopen under the user pin must cite the prior fixpoint and name a material
  differential plus same-harness strict comparator evidence.
- Dispatch, masking, tiny-string, and unescape signals do not reopen pre-pin
  rejected route families such as parse-time aux side tables, parser-local
  structural cursors, event sidecars, dispatch-table/function-pointer
  alternates, decoded-string stats sinks, generic source visitors, or
  source-method digest folds.
- PEXT, function-only ASM leaves, and C/C++ sidecar gaps do not create orphan
  SIMD primitives or reopen REDRESS-126. Future SIMD routes need scalar
  reference, checkasm/parity, feature-mask disclosure, same-wave consumer, and
  zero-orphan disposition.
