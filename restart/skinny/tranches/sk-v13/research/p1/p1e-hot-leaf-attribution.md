# SK-V13 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V13 / S-P1 V2 fold.
Date: 2026-05-21.
Scope: per-corpus hot-leaf synthesis across parse, direct, typed, mode-III, and CSS declaration-values profile artefacts.
Output: this file.
Baseline: SK-V13-open (`7ee299096be7d7fdaa0e69344a6cd18bbd55524f`; source-equivalent to V1 for `skinny/crates/`).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS="-C target-cpu=native"` for V2 direct/mode-III/CSS captures.
Profile tool: samply 0.13.1 saved Firefox-profile JSON plus `.json.syms.json` sidecars; V2 top-20 extractor output.
Corpus coverage: parse 17/17 from V1, direct 17/17 from V2, typed 7/17 from V1 generated typed rows, mode III 17/17 x 5 from V2, CSS declaration-values 1/1 from V2.

## §1 - Method (commands run; verbatim, reproducible)

Read profile cohort and raw artefacts:

```bash
sed -n '1,260p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,220p' restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md
sed -n '1,220p' restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md
sed -n '1,220p' restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md
sed -n '1,220p' restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md
```

Resolve saved samply profiles through sidecars:

```bash
python3 /tmp/skv13-p1-v2/summary/extract-hotleaf-top20-equivalent.py
# output materialized as:
# /tmp/skv13-p1-v2/summary/hotleaf_top20.tsv
# /tmp/skv13-p1-v2/summary/direct_summary.tsv
# /tmp/skv13-p1-v2/summary/mode3_summary.tsv
```

The extractor maps each sample's leaf frame RVA to the owning library in the
profile, resolves the RVA through the matching `.json.syms.json` symbol table,
and emits rank, percent self-time, sample count, function, file, line, library,
and profile path. The script body was executed in the orchestrator shell; the
TSVs are the citable artefacts.

## §2 - Findings (per-corpus table; file:line on every hot-leaf claim)

Classification vocabulary here is profile-only: `dispatch-envelope`,
`hand-oracle`, `unicode`, `scan`, `simd-scan`, `typed-generated`,
`css-fact-sink`, and `timer/noise`.

| Corpus | parse_only Track 1 rank-1 | direct Track 1 rank-1 | mode-III structural SIMD rank-1 | typed Track 1 rank-1 |
|---|---|---|---|---|
| twitter | `dispatch_value` (`generated.rs:46`, V1) | 74.0% `parse_object_value_at_direct` (`generated.rs:466`) | 67.5% `scan_structurals` (`scan.rs`) | `DirectParser::skip_value` (`generated_real_typed.rs:1739`, V1) |
| citm_catalog | `dispatch_value` (`generated.rs:45`, V1) | 58.4% `parse_array_element_at_direct` (`generated.rs:506`) | 69.0% `scan_structurals` (`scan.rs`) | `DirectParser::skip_value` (`generated_real_typed.rs:1739`, V1) |
| canada | `dispatch_value` (`generated.rs:45`, V1) | 85.3% `parse_array_element_at_direct` (`generated.rs:506`) | 52.6% `scan_structurals` (`scan.rs`) | missing typed row |
| apache_builds | `dispatch_value` (`generated.rs:46`, V1) | 38.1% `parse_object_value_at_direct` (`generated.rs:466`) | 62.1% `scan_structurals` (`scan.rs`) | `parse_option_scalar_string` (`generated_real_typed.rs:1199`, V1) |
| github_events | `dispatch_value` (`generated.rs:49`, V1) | 67.7% `parse_object_value_at_direct` (`generated.rs:466`) | 65.6% `scan_structurals` (`scan.rs`) | `DirectParser::skip_value` (`generated_real_typed.rs:1740`, V1) |
| update_center | `dispatch_value` (`generated.rs:45`, V1) | 68.3% `parse_object_value_at_direct` (`generated.rs:466`) | 57.5% `scan_structurals` (`scan.rs`) | `parse_type_plugin` (`generated_real_typed.rs:473`, V1) |
| mesh | `dispatch_value` (`generated.rs:45`, V1) | 76.7% `parse_array_element_at_direct` (`generated.rs:506`) | 62.6% `scan_structurals` (`scan.rs`) | `parse_type_mesh` (`generated_real_typed.rs:828`, V1) |
| random | `dispatch_value` (`generated.rs:45`, V1) | 37.7% `parse_object_value_at_direct` (`generated.rs:466`) | 48.7% `scan_structurals` (`scan.rs`) | missing typed row |
| gsoc-2018 | `dispatch_value` (`generated.rs:45`, V1) | 60.2% `parse_object_value_at_direct` (`generated.rs:466`) | 77.2% `scan_structurals` (`scan.rs`) | missing typed row |
| marine_ik | `dispatch_value` (`generated.rs:45`, V1) | 72.3% `parse_array_element_at_direct` (`generated.rs:506`) | 55.3% `scan_structurals` (`scan.rs`) | `parse_type_marine_geometry_data` (`generated_real_typed.rs:1015`, V1) |
| instruments | `dispatch_value` (`generated.rs:46`, V1) | 58.3% `Option<&u8>::copied` (`core/src/option.rs:2141`) | 69.9% `scan_structurals` (`scan.rs`) | missing typed row |
| numbers | `dispatch_value` (`generated.rs:45`, V1) | 76.1% `parse_array_element_at_direct` (`generated.rs:506`) | 51.4% `scan_structurals` (`scan.rs`) | missing typed row |
| unicode_mixed | `dispatch_value` (`generated.rs:45`, V1) | 55.9% `parse_object_value_at_direct` (`generated.rs:466`) | 72.0% `scan_structurals` (`scan.rs`) | missing typed row |
| unicode_escapes | `dispatch_value` (`generated.rs:45`, V1) | 46.7% `parse_that_regex::unescape_string` (`parse-that-regex/src/lib.rs:718`) | 87.5% `scan_structurals` (`scan.rs`) | missing typed row |
| unicode_basic | `dispatch_value` (`generated.rs:45`, V1) | 44.1% `parse_object_value_at_direct` (`generated.rs:466`) | 52.0% `scan_structurals` (`scan.rs`) | missing typed row |
| distinct_values | `match_tiny_plain_string_with_cap::<16>` (V1 sidecar line absent) | 49.5% `parse_array_element_at_direct` (`generated.rs:542`) | 48.2% `bulk_emit_positions_64_neon` (sidecar line absent) | missing typed row |
| y_string_unicode | `read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:946`, V1) | 19.5% `parse_array_element_at_direct` (`generated.rs:506`) | 52.9% `scan_structurals` (`scan.rs`) | missing typed row |

CSS declaration-values V2 profile:

| Row | Rank-1/2/3 self-time leaves | Classification |
|---|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | 17.6% `mach_absolute_time`; 13.7% `LocalFactSink::finish`; 7.5% `FactSink::finish`; full top-20 at `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv` | throughput/equality measured, but profile is dominated by timer/fact-sink leaves rather than a parser primitive |

## §3 - Delta vs SK-V12 (per row; Mbps + c/B + classification)

No checked SK-V12 hot-leaf TSV exists. V2 delta is therefore relative to the
S-P1 V1 challenge defects:

| Defect from V1 | V2 disposition |
|---|---|
| Direct hot leaves invalid because profiles were panic paths | Resolved: 34 non-panic direct profiles with rank-1 symbols and full top-20 TSV |
| Mode-III profiles absent | Resolved: 85 profiles, 85 counter rows, 2 unsupported probes explicitly routed |
| CSS hot leaf absent | Partially resolved: one CSS samply profile exists, but top leaves are timer/fact-sink dominated |
| Branch/L1/LLC fields missing | Preserved honestly: xctrace export tested, fields unavailable from current export |
| Parse sidecar mismatch | Preserved honestly: parse profile remains V1 save-only with offline sidecars |

## §4 - Anomalies + masking signals (flagged for S-P2)

- Direct Track 1 is mostly a generated dispatch-envelope profile; S-P2 must
  not infer primitive wins from envelope dominance.
- The clearest named direct primitive is `parse_that_regex::unescape_string`
  on `unicode_escapes` at `parse-that-regex/src/lib.rs:718`.
- Structural SIMD scan is consistently faster than scalar scan in P1-C, but
  REDRESS 96/97/98 make it unsafe to infer that consuming a union substrate is
  profitable. This remains a fresh-measurement antecedent, not a route.
- CSS declaration-values sampling needs longer or narrower profiling before a
  parser hot leaf is named; current rank-1 is timer overhead.
- Sidecar symbol resolution produces file:line for most Rust leaves; some ASM
  and system leaves have function-only attribution. Those cells are explicit,
  not silently resolved.

## §5 - Sources (every artefact path + run id)

- `/tmp/skv13-p1/samply/profiles/parse__*.json.gz`
- `/tmp/skv13-p1/samply/profiles/parse__*.json.syms.json`
- `/tmp/skv13-p1/samply/profiles/typed__*.json.gz`
- `/tmp/skv13-p1/samply/profiles/typed__*.json.syms.json`
- `/tmp/skv13-p1-v2/samply/profiles/direct__*.json.gz`
- `/tmp/skv13-p1-v2/samply/profiles/direct__*.json.syms.json`
- `/tmp/skv13-p1-v2/mode3/profiles/mode3__*.json.gz`
- `/tmp/skv13-p1-v2/mode3/profiles/mode3__*.json.syms.json`
- `/tmp/skv13-p1-v2/css/profiles/css_l4_declaration_values_all_modes.json.gz`
- `/tmp/skv13-p1-v2/css/profiles/css_l4_declaration_values_all_modes.json.syms.json`
- `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`
- `/tmp/skv13-p1-v2/summary/direct_summary.tsv`
- `/tmp/skv13-p1-v2/summary/mode3_summary.tsv`
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
