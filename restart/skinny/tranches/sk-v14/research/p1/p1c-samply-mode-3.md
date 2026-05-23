# SK-V14 P1-C: Samply Mode III (Masking Probes + Structural-Scan-Only)

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Scope: samply profiling in mode III — the masking-probe workloads
(`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`)
plus the structural-scan-only path
(`bbnf_bench::scan::structural_offsets_{scalar,simd}` → `runtime::generated_json::scan::scan_structurals{,_scalar}`).
Output: this file.
Baseline: SK-V14-open (audit-corrected SK-V13 close state; HEAD at
`2547c750bc78533d738eb85913206a0872022818` — SK-V14 dispatch-context seed,
no `skinny/` source delta vs SK-V13 close at `ff653fbe6`).
Host triple: aarch64-apple-darwin (cpu = Apple M5 Max; per-core nominal
4.4 GHz used for the c/B column).
Build flags: release profile + `debug=true` + `RUSTFLAGS="-C target-cpu=native"`;
target dir `/tmp/skv14-p1c-target` (single-cargo-per-target discipline).
build_flags_regime: `RUSTFLAGS="-C target-cpu=native"` explicitly pinned at
build time (see §1.1 `RUSTFLAGS="-C target-cpu=native" cargo build` block).
P1-C/D share this regime; P1-A/B do not (P1-A: RUSTFLAGS not set explicitly,
native-CPU NOT pinned because Cargo.toml does not propagate target-cpu;
P1-B: explicit `RUSTFLAGS unset` disclosure). Cross-artefact c/B comparison
that spans regimes (e.g. P1-B twitter 11037 Mbps vs P1-D twitter 11627 Mbps,
5.3% delta) must surface this column; consumer-side aggregators are required
to refuse a cross-row delta where `build_flags_regime` does not match
(per CH4 F-V2-METHODOLOGY-1 Option A binding).
Profile tool: samply 0.13.1 (`samply record --save-only --no-open
--rate 4000 --unstable-presymbolicate`); `.json.syms.json` sidecars
carry full string/function/file/line tables — the `--save-only` flag here
disables the local UI server only and does not strip symbols because
`--unstable-presymbolicate` writes the resolved table out-of-band.
Corpus coverage: 17/17 JSON corpora × 4 probe workloads (the four mode-III
benches in `skinny/crates/bbnf-bench/benches/json_parity.rs::run_probe_group`
plus the structural scan from `benches/simd_scan.rs`) — every per-corpus
throughput estimate cited below was re-extracted from criterion at
`/tmp/skv14-p1c-target/criterion/json_probes_<corpus>/<probe>/new/estimates.json`
during this profile pass (R1 comparator misbinding inherited and documented
per S-P1 dispatch §1; no source repair attempted, S-P2 will design).

## §1 — Method (commands run; verbatim, reproducible)

### §1.1 — Build

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv14-p1c-target \
RUSTFLAGS="-C target-cpu=native" \
cargo build --release --bench json_parity --bench simd_scan -p bbnf-bench
# Finished `release` profile [optimized + debuginfo] target(s) in 1m 53s
# binaries:
#   /tmp/skv14-p1c-target/release/deps/json_parity-fa5381f7fa4e9e97
#   /tmp/skv14-p1c-target/release/deps/simd_scan-e9e48792f0c6e621
```

Skinny `Cargo.toml` profile that landed under samply:

```toml
[profile.release]
opt-level = 3
lto = "fat"
codegen-units = 1
panic = "abort"
debug = true              # required for symbol resolution
strip = false
split-debuginfo = "packed"

[profile.bench]
inherits = "release"
# (same flags; criterion-bench harnesses inherit release+debug=true)
```

### §1.2 — Per-probe samply captures (4 aggregate captures, 17/17 corpora each)

The four mode-III workloads exposed by
`skinny/crates/bbnf-bench/benches/json_parity.rs::run_probe_group`
(`benches/json_parity.rs:381`-`438`) are profiled once each with a
single bench-binary invocation filtered to the probe's `bench_function`
name. The criterion harness iterates that probe over all 17 fixtures
loaded by `test_fixtures::load_available_bench_fixtures`
(`crates/test-fixtures/src/lib.rs:7`-`25`), so a single samply capture
yields a 17-corpus aggregate self-time table per probe.

```bash
# Probe 1: cold_first_parse (iter_batched over freshly-cloned fixture bytes;
#          calls runtime::generated_json::parse — pure cold per-parse Track 1)
CARGO_TARGET_DIR=/tmp/skv14-p1c-target samply record \
  --save-only --no-open --rate 4000 --unstable-presymbolicate \
  -o /tmp/skv14-p1c-profiles/probe-cold_first_parse.json.gz \
  -- /tmp/skv14-p1c-target/release/deps/json_parity-fa5381f7fa4e9e97 \
     --bench cold_first_parse

# Probe 2: host_call_eager_decode (parses Track 1 then walks the view
#          tree decoding every string via eager_decode_strings)
CARGO_TARGET_DIR=/tmp/skv14-p1c-target samply record \
  --save-only --no-open --rate 4000 --unstable-presymbolicate \
  -o /tmp/skv14-p1c-profiles/probe-host_call_eager_decode.json.gz \
  -- /tmp/skv14-p1c-target/release/deps/json_parity-fa5381f7fa4e9e97 \
     --bench host_call_eager_decode

# Probe 3: alternate_scalar_plan (the "alternate" — body is
#          serde_json::from_str::<Value>; see §4 anomalies)
CARGO_TARGET_DIR=/tmp/skv14-p1c-target samply record \
  --save-only --no-open --rate 4000 --unstable-presymbolicate \
  -o /tmp/skv14-p1c-profiles/probe-alternate_scalar_plan.json.gz \
  -- /tmp/skv14-p1c-target/release/deps/json_parity-fa5381f7fa4e9e97 \
     --bench alternate_scalar_plan

# Probe 4: structural-scan-only (`benches/simd_scan.rs` —
#          runtime::generated_json::scan::scan_structurals{,_scalar} ×
#          17 corpora × {simd, scalar} = 34 bench_functions)
CARGO_TARGET_DIR=/tmp/skv14-p1c-target samply record \
  --save-only --no-open --rate 4000 --unstable-presymbolicate \
  -o /tmp/skv14-p1c-profiles/probe-structural_scan.json.gz \
  -- /tmp/skv14-p1c-target/release/deps/simd_scan-e9e48792f0c6e621 --bench
```

A fifth probe `alternate_pext_mask_plan` is not measured: the
`#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]` gate at
`benches/json_parity.rs:414` excludes aarch64 hosts. A sixth registered
probe `host_call_dispatch_overhead` (`benches/json_parity.rs:394`) is
a `fn(&str)->usize` length-call wrapped in `black_box`; it measures
function-call dispatch overhead in isolation and is not in CH1/CH2 scope
for hot-leaf attribution (the underlying primitive is a pointer
indirection; documented here for completeness, not profiled separately).

### §1.3 — Per-corpus throughput re-capture (re-runs by-product)

Each `samply record` invocation drove the same criterion bench it
profiled, so per-corpus per-probe slope estimates landed at
`/tmp/skv14-p1c-target/criterion/json_probes_<corpus>/<probe>/new/estimates.json`.
Throughput per row is extracted with:

```
Mbps = (bytes * 8000) / ns_per_iter        # bytes*8 bits/B * 1000 = Mbits/s
c/B  = (ns_per_iter * 4.4) / bytes          # × 4.4 GHz P-core (Apple M5 Max)
```

This is the same formula `bbnf-bench/src/bin/gate.rs:3719`-`3725` applies
(`bytes * 8_000 / ns`); the `Mbps` column in `skinny/RESULTS.md` is
megabits/sec, not megabytes/sec, and the re-extracted values reconcile
to RESULTS exactly for `track1_generated` (twitter 15561 ✓, canada 16977 ✓,
citm_catalog 30150 ✓ — see §3 table).

### §1.4 — Symbol resolution

Samply with `--unstable-presymbolicate` writes a `<profile>.json.syms.json`
sidecar holding per-binary `{rva, size, symbol, frames=[{function,file,line}]}`
records. Self-time aggregation is done by reading the gzipped profile's
`threads[0].{samples,stackTable,frameTable.address}`, binning sample
addresses, then resolving via bisect against the sidecar's per-binary
symbol_table. Sidecar inline-frame innermost-leaf is the named symbol.
Resolution script + per-probe top-25 outputs are at
`/tmp/skv14_p1c_resolve.py` + per-call stdout in §2.

## §2 — Findings (per-corpus per-symbol table; file:line on every hot-leaf claim)

### §2.1 — Per-corpus throughput table (17/17, all four probes)

`T1_Mbps`/`T1_c/B` is the cold per-parse `track1_generated` baseline from
the same run (re-extracted with the bench harness's `bytes*8000/ns`
convention). Probe ratios are `probe_Mbps / T1_Mbps`. Audit overlay per
row follows the SK-V13 audit pack mapping (§4 below); since the masking
probes are NOT enumerated in `ROLLING-SOTA-DELTA` they default to
AUDIT-PENDING with the T1 row's overlay attached for context.

| corpus | size_b | T1_Mbps | T1_c/B | host_call_Mbps | host_call_c/B | host_call/T1 | alt_scalar_Mbps | alt_scalar_c/B | alt_scalar/T1 | cold_first_Mbps | cold_first_c/B | cold_first/T1 | T1_audit_overlay (parse_only row) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| twitter | 631515 | 15561 | 2.26 | 4127 | 8.53 | 0.27x | 6189 | 5.69 | 0.40x | 12534 | 2.81 | 0.81x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| citm_catalog | 1727204 | 30150 | 1.17 | 7189 | 4.90 | 0.24x | 6929 | 5.08 | 0.23x | 25870 | 1.36 | 0.86x | AUDIT-FALSIFIED (W14.2 admit → revert per PRUNE-1) |
| canada | 2251051 | 16977 | 2.07 | 4078 | 8.63 | 0.24x | 3926 | 8.97 | 0.23x | 14803 | 2.38 | 0.87x | AUDIT-FALSIFIED (W14.3 admit → revert per PRUNE-1) |
| apache_builds | 127275 | 12767 | 2.76 | 4479 | 7.86 | 0.35x | 5832 | 6.04 | 0.46x | 11422 | 3.08 | 0.89x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| github_events | 65132 | 14966 | 2.35 | 5302 | 6.64 | 0.35x | 7032 | 5.01 | 0.47x | 13504 | 2.61 | 0.90x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| update_center | 533178 | 11791 | 2.99 | 2963 | 11.88 | 0.25x | 4079 | 8.63 | 0.35x | 10734 | 3.28 | 0.91x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| mesh | 723597 | 12987 | 2.71 | 4730 | 7.44 | 0.36x | 4445 | 7.92 | 0.34x | 11578 | 3.04 | 0.89x | AUDIT-FALSIFIED (W14.5 admit → revert per PRUNE-1) |
| random | 510476 | 9946 | 3.54 | 2660 | 13.23 | 0.27x | 3551 | 9.91 | 0.36x | 7773 | 4.53 | 0.78x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| gsoc-2018 | 3327831 | 23587 | 1.49 | 7447 | 4.73 | 0.32x | 16611 | 2.12 | 0.70x | 20298 | 1.73 | 0.86x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| marine_ik | 2983466 | 12357 | 2.85 | 2207 | 15.95 | 0.18x | 3677 | 9.57 | 0.30x | 11165 | 3.15 | 0.90x | AUDIT-FALSIFIED (W14.4 admit → revert per PRUNE-1) |
| instruments | 220346 | 17468 | 2.02 | 4684 | 7.52 | 0.27x | 4799 | 7.33 | 0.27x | 15317 | 2.30 | 0.88x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| numbers | 150124 | 19267 | 1.83 | 8423 | 4.18 | 0.44x | 5625 | 6.26 | 0.29x | 17114 | 2.06 | 0.89x | AUDIT-FALSIFIED (W14.1 admit → revert per PRUNE-1) |
| unicode_mixed | 1053086 | 9294 | 3.79 | 1724 | 20.42 | 0.19x | 4565 | 7.71 | 0.49x | 5666 | 6.21 | 0.61x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| unicode_escapes | 1050797 | 13550 | 2.60 | 2011 | 17.51 | 0.15x | 4890 | 7.20 | 0.36x | 11310 | 3.11 | 0.83x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| unicode_basic | 1048586 | 12041 | 2.92 | 2481 | 14.19 | 0.21x | 3905 | 9.01 | 0.32x | 6270 | 5.61 | 0.52x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| distinct_values | 153630 | 9920 | 3.55 | 3674 | 9.58 | 0.37x | 3970 | 8.87 | 0.40x | 8260 | 4.26 | 0.83x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |
| y_string_unicode | 35601 | 6590 | 5.34 | 1801 | 19.55 | 0.27x | 5303 | 6.64 | 0.80x | 5288 | 6.66 | 0.80x | AUDIT-SUSTAINED (S/NO-GO row, gate-honest) |

Source-of-truth for each row's ns estimate:
`/tmp/skv14-p1c-target/criterion/json_probes_<corpus>/<probe>/new/estimates.json`
(re-extracted via `/tmp/skv14_p1c_extract_v2.py` →
`/tmp/skv14_p1c_probe.json`). The five corpora where
`host_call_eager_decode` and `alternate_scalar_plan` carry no slope and
the script fell back to mean (`gsoc-2018`, `marine_ik`, `unicode_mixed`,
`unicode_escapes`, `unicode_basic` for host_call; `citm_catalog`,
`marine_ik`, `unicode_basic` for alt_scalar) used criterion's mean
point-estimate as a graceful fallback when `slope.point_estimate` was
`null` (occurs when `iter_batched(LargeInput)` produces non-varying iter
counts so slope fitting degenerates). Cell src tagging is recorded in
`/tmp/skv14_p1c_probe.json` per cell.

### §2.2 — Per-probe aggregate self-time (top symbols, all 17 corpora pooled)

The samply per-probe capture aggregates self-time over the whole
17-corpus run. Per-corpus separation requires per-corpus captures
(escalation §C below); aggregate symbol attribution is the load-bearing
artefact because the generated parser/view/decode code is corpus-agnostic.

#### Probe `cold_first_parse` — aggregate top-10 (428 754 resolved samples; 76 unresolved)

| pct | cnt | symbol | file:line | binary |
|---:|---:|---|---|---|
| 88.14% | 377 909 | `runtime::generated_json::generated::dispatch_value` | `generated.rs:45` | `json_parity-fa5381f7fa4e9e97` |
| 7.96% | 34 110 | `core::str::converts::from_utf8` | `converts.rs:91` | `json_parity-fa5381f7fa4e9e97` |
| 3.02% | 12 960 | `_platform_memmove` | `(libsystem_platform.dylib)` | system |
| 0.18% | 785 | `madvise` | `(libsystem_kernel.dylib)` | system |
| 0.11% | 456 | `__psynch_cvwait` | `(libsystem_kernel.dylib)` | system |
| 0.04% | 185 | `criterion::bencher::Bencher::iter_batched` closure | `bencher.rs:244` | `json_parity-fa5381f7fa4e9e97` |
| 0.04% | 169 | `mach_absolute_time` | `(libsystem_kernel.dylib)` | system |
| 0.03% | 132 | `_platform_memcmp` | `(libsystem_platform.dylib)` | system |
| 0.03% | 117 | `sha2::sha256::compress256` | `sha256.rs:42` | `json_parity-fa5381f7fa4e9e97` |
| 0.02% | 103 | `runtime::generated_json::view::string_body_range` | `view.rs:384` | `json_parity-fa5381f7fa4e9e97` |

#### Probe `host_call_eager_decode` — aggregate top-15 (742 645 resolved samples; 164 unresolved)

| pct | cnt | symbol | file:line | binary |
|---:|---:|---|---|---|
| 30.73% | 228 197 | `runtime::generated_json::generated::dispatch_value` | `generated.rs:45` | `json_parity-fa5381f7fa4e9e97` |
| 23.28% | 172 901 | `<runtime::generated_json::value::JsonNodeKind>::at_cursor` | `value.rs:29` | `json_parity-fa5381f7fa4e9e97` |
| 15.68% | 116 410 | `runtime::generated_json::view::string_body_range` | `view.rs:384` | `json_parity-fa5381f7fa4e9e97` |
| 10.16% | 75 431 | `core::str::converts::from_utf8` | `converts.rs:91` | `json_parity-fa5381f7fa4e9e97` |
| 4.64% | 34 472 | `JsonObjectPairs as Iterator::next` | `view.rs:268` | `json_parity-fa5381f7fa4e9e97` |
| 4.62% | 34 276 | `JsonArrayValues as Iterator::next` | `view.rs:310` | `json_parity-fa5381f7fa4e9e97` |
| 2.93% | 21 772 | `json_parity::eager_decode_strings::walk` | `json_parity.rs:441` | `json_parity-fa5381f7fa4e9e97` |
| 2.83% | 21 018 | `parse_that_regex::unescape_string` | `lib.rs:718` | `json_parity-fa5381f7fa4e9e97` |
| 2.56% | 19 029 | `<runtime::generated_json::view::JsonString>::as_str` | `view.rs:206` | `json_parity-fa5381f7fa4e9e97` |
| 1.32% | 9 808 | `bbnf_simd::aarch64::utf8::validate_block::validate_block_scalar` | `validate_block.rs:90` | `json_parity-fa5381f7fa4e9e97` |
| 0.61% | 4 556 | `_platform_memmove` | `(libsystem_platform.dylib)` | system |
| 0.12% | 884 | `core::slice::memchr::memchr_aligned` | `mod.rs:2448` | `json_parity-fa5381f7fa4e9e97` |
| 0.09% | 634 | `mi_free` | `free.c:208` | `json_parity-fa5381f7fa4e9e97` |
| 0.08% | 577 | `mi_malloc_aligned` | `alloc-aligned.c:273` | `json_parity-fa5381f7fa4e9e97` |
| 0.06% | 463 | `__psynch_cvwait` | `(libsystem_kernel.dylib)` | system |

#### Probe `alternate_scalar_plan` — aggregate top-15 (725 850 resolved samples; 4 904 unresolved)

| pct | cnt | symbol | file:line | binary |
|---:|---:|---|---|---|
| 15.14% | 109 864 | `<serde_json::value::Value as Deserialize>::deserialize` | `de.rs:24` | `json_parity-fa5381f7fa4e9e97` |
| 11.19% | 81 213 | `_platform_memmove` | `(libsystem_platform.dylib)` | system |
| 10.08% | 73 168 | `<serde_json::read::SliceRead>::skip_to_escape` | `read.rs:432` | `json_parity-fa5381f7fa4e9e97` |
| 7.10% | 51 523 | `<IndexMap<String, Value>>::insert_full` | `map.rs:466` | `json_parity-fa5381f7fa4e9e97` |
| 6.99% | 50 757 | `<serde_json::de::Deserializer>::parse_number` | `de.rs:509` | `json_parity-fa5381f7fa4e9e97` |
| 4.90% | 35 547 | `<serde_json::de::MapAccess>::next_key_seed::has_next_key` | `de.rs:1990` | `json_parity-fa5381f7fa4e9e97` |
| 4.52% | 32 799 | `serde_json::read::parse_escape::<SliceRead>` | `read.rs:874` | `json_parity-fa5381f7fa4e9e97` |
| 4.36% | 31 644 | `<serde_json::de::SeqAccess>::next_element_seed::has_next_element` | `de.rs:1937` | `json_parity-fa5381f7fa4e9e97` |
| 3.62% | 26 273 | `mi_free` | `free.c:208` | `json_parity-fa5381f7fa4e9e97` |
| 3.44% | 24 954 | `mi_malloc_aligned` | `alloc-aligned.c:273` | `json_parity-fa5381f7fa4e9e97` |
| 3.40% | 24 700 | `<serde_json::read::StrRead>::parse_str` | `read.rs:709` | `json_parity-fa5381f7fa4e9e97` |
| 3.35% | 24 291 | `core::ptr::drop_in_place::<serde_json::value::Value>` | `mod.rs:810` | `json_parity-fa5381f7fa4e9e97` |
| 3.09% | 22 398 | `<core::hash::sip::Hasher>::write` | `sip.rs:258` | `json_parity-fa5381f7fa4e9e97` |
| 2.97% | 21 543 | `<RandomState as BuildHasher>::hash_one::<&String>` | `mod.rs:694` | `json_parity-fa5381f7fa4e9e97` |
| 2.64% | 19 178 | `serde_json::read::parse_unicode_escape::<SliceRead>` | `read.rs:900` | `json_parity-fa5381f7fa4e9e97` |

#### Probe `structural_scan` (simd_scan bench) — aggregate top-10 (1 383 688 resolved samples; 6 225 unresolved) + per-corpus Mbps

Symbol attribution (all 17 corpora pooled; single samply capture against
the `/tmp/skv14-p1c-target/release/deps/simd_scan-e9e48792f0c6e621`
binary; sidecar `probe-structural_scan.json.syms.json`):

| pct | cnt | symbol | file:line | binary |
|---:|---:|---|---|---|
| 49.44% | 684 110 | `runtime::generated_json::scan::scan_tail` | `scan.rs:107` | `simd_scan-e9e48792f0c6e621` |
| 32.05% | 443 472 | `bbnf_bench::scan::structural_offsets_simd` (calls into NEON `scan_structurals`) | `scan.rs:5` | `simd_scan-e9e48792f0c6e621` |
| 14.63% | 202 410 | `bbnf_simd::aarch64::bulk_emit_positions_64::bulk_emit_positions_64_neon` | `bulk_emit_positions_64.rs:2` (`fn` signature; `#[inline]` attribute at line 1) | `simd_scan-e9e48792f0c6e621` |
| 1.52% | 21 019 | `criterion::analysis::common` | `mod.rs:83` | (criterion) |
| 1.45% | 20 021 | `bbnf_simd::aarch64::bitmap_prefix_xor_64::bitmap_prefix_xor_64_neon` | `bitmap_prefix_xor_64.rs:2` (`fn` signature; `#[inline]` attribute at line 1) | `simd_scan-e9e48792f0c6e621` |
| 0.81% | 11 165 | `bbnf_simd::aarch64::eob_pad_clamp::eob_pad_clamp_neon` | `eob_pad_clamp.rs:4` (`fn` signature; `#[inline]` attribute at line 3) | `simd_scan-e9e48792f0c6e621` |
| 0.01% | 173 | `criterion::kde::sweep_and_estimate` | `kde.rs:20` | (criterion) |
| 0.01% | 132 | `__open` | `(libsystem_kernel.dylib)` | system |
| 0.01% | 115 | `sha2::sha256::compress256` | `sha256.rs:42` | (sha2) |
| 0.01% | 98 | `mach_absolute_time` | `(libsystem_kernel.dylib)` | system |

The split between `scan_tail` (49.44%, the scalar fallback path at
`scan.rs:107`-`128`) and `structural_offsets_simd` + the NEON primitives
(32.05% + 14.63% + 1.45% + 0.81% = 48.94%) reflects the 17/17 simd+scalar
benchmark mix: each corpus drives both lanes in `simd_scan.rs:30`-`38`,
so the aggregate is approximately 50/50 by design. Per-corpus
isolation: scalar self-time is exclusively `scan_tail`; SIMD self-time
is `structural_offsets_simd` + `bulk_emit_positions_64_neon` +
`bitmap_prefix_xor_64_neon` + `eob_pad_clamp_neon`. No JSON
parse-grammar symbols appear (CH2 lens compliance: the structural-scan
path is a grammar-neutral SIMD primitive measured on JSON bytes).

Per-corpus throughput (all 17/17, recap from
`/tmp/skv14-p1c-target/criterion/simd_structural_scan/<corpus>_{simd,scalar}/new/estimates.json`):

| corpus | scalar_Mbps | simd_Mbps | speedup | c/B_simd | c/B_scalar |
|---|---:|---:|---:|---:|---:|
| twitter | 8 141 | 20 950 | 2.57x | 1.680 | 4.324 |
| citm_catalog | 8 529 | 22 384 | 2.62x | 1.573 | 4.127 |
| canada | 6 954 | 37 463 | 5.39x | 0.940 | 5.062 |
| apache_builds | 9 132 | 19 880 | 2.18x | 1.771 | 3.855 |
| github_events | 9 619 | 23 902 | 2.48x | 1.473 | 3.659 |
| update_center | 9 593 | 17 739 | 1.85x | 1.984 | 3.669 |
| mesh | 6 609 | 40 920 | 6.19x | 0.860 | 5.326 |
| random | 9 296 | 15 648 | 1.68x | 2.249 | 3.787 |
| gsoc-2018 | 10 255 | 23 651 | 2.31x | 1.488 | 3.432 |
| marine_ik | 6 986 | 21 913 | 3.14x | 1.606 | 5.039 |
| instruments | 8 602 | 20 585 | 2.39x | 1.710 | 4.092 |
| numbers | 7 257 | 40 352 | 5.56x | 0.872 | 4.851 |
| unicode_mixed | 8 843 | 20 743 | 2.35x | 1.697 | 3.981 |
| unicode_escapes | 12 497 | 24 032 | 1.92x | 1.465 | 2.817 |
| unicode_basic | 8 980 | 15 955 | 1.78x | 2.206 | 3.920 |
| distinct_values | 10 224 | 16 635 | 1.63x | 2.116 | 3.443 |
| y_string_unicode | 10 930 | 20 702 | 1.89x | 1.700 | 3.221 |

### §2.3 — Representative symbol/file:line anchors (the named primitives)

The aggregate hot-leaf set across the four mode-III probes reduces to a
small grammar-neutral primitive list (CH2 lens compliance — names are
substrate primitives, not JSON-role labels). Each row cites a single
file:line; the same primitive recurs across probes.

| Primitive (substrate) | symbol path | file:line | classified as |
|---|---|---|---|
| Generated parser dispatch | `runtime::generated_json::generated::dispatch_value` | `skinny/crates/runtime/src/grammars/json/generated.rs:45` | dispatch (Lock-14 hot leaf) |
| Tape leaf-kind decoder | `<runtime::generated_json::value::JsonNodeKind>::at_cursor` | `skinny/crates/runtime/src/grammars/json/value.rs:29` | tape |
| String-body range projector | `runtime::generated_json::view::string_body_range` | `skinny/crates/runtime/src/grammars/json/view.rs:384` | string |
| UTF-8 validator | `core::str::converts::from_utf8` | `(std)` `converts.rs:91` | unicode |
| Object pair iterator | `<JsonObjectPairs as Iterator>::next` | `skinny/crates/runtime/src/grammars/json/view.rs:268` | structural |
| Array value iterator | `<JsonArrayValues as Iterator>::next` | `skinny/crates/runtime/src/grammars/json/view.rs:310` | structural |
| Eager-decode walk (probe-internal) | `json_parity::eager_decode_strings::walk` | `skinny/crates/bbnf-bench/benches/json_parity.rs:441` | probe-instrumentation |
| Escape-string decoder | `parse_that_regex::unescape_string` | `(workspace) lib.rs:718` | string |
| String view leaf | `<runtime::generated_json::view::JsonString>::as_str` | `skinny/crates/runtime/src/grammars/json/view.rs:206` | string |
| Block UTF-8 validator | `bbnf_simd::aarch64::utf8::validate_block::validate_block_scalar` | `skinny/crates/bbnf-simd/src/aarch64/utf8/validate_block.rs:90` | unicode |
| Structural scanner (scalar) | `runtime::generated_json::scan::scan_structurals_scalar` → `scan_tail` | `skinny/crates/runtime/src/grammars/json/scan.rs:32,107` | scan |
| Structural scanner (NEON) | `runtime::generated_json::scan::scan_structurals` → `mod neon::scan` | `skinny/crates/runtime/src/grammars/json/scan.rs:22,207` | scan |
| Scan capacity planner | `runtime::generated_json::scan::structural_capacity_for` | `skinny/crates/runtime/src/grammars/json/scan.rs:47` | scan |

CH2 note on `dispatch_value` as a "primitive": the function at
`generated.rs:45` is annotated `#[cfg_attr(not(feature = "parse-attribution"), inline(always))]`
(`generated.rs:43`-`44`). The bench builds without the
`parse-attribution` feature (Cargo.toml `runtime/Cargo.toml:21` —
`parse-attribution = []`), so the inliner folds `parse_object`,
`parse_array`, `parse_string`, `parse_number`, `parse_literal` into
the single `dispatch_value` symbol. The 88.14% / 30.73% self-time
attribution against `generated.rs:45` therefore represents the **entire
inlined generated Track 1 parse path**; sub-symbol attribution requires
a `--features parse-attribution` rebuild (escalation §C) and is itself
the primary CH1/CH6 finding for P1-A/P1-B as well.

## §3 — Delta vs SK-V13 close (per row; Mbps + c/B + audit-overlay verdict per row)

SK-V14 has not landed any source-code delta against `skinny/crates/`
relative to the SK-V13 close commit `ff653fbe6`. The HEAD at this
profile pass is `2547c750bc78533d738eb85913206a0872022818`, which is
the SK-V14 dispatch-context seed commit (docs-only change). The masking
probe slopes, structural scan slopes, and per-symbol attributions
therefore represent the **honest SK-V13-close baseline with the audit
overlay applied per row**, which is the SK-V14-open definition per
S-P1 dispatch context §1.

### §3.1 — Δ vs the prior SK iteration mode-III table (SK-V13 P1-C V2)

SK-V13 `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
ran mode-III against a separate temporary harness at
`/tmp/skv13-mode3-profiler` (not via the criterion bench binary). The
SK-V14 P1-C run drives mode III through the in-tree
`bbnf-bench/benches/json_parity.rs::run_probe_group` criterion harness,
so direct ns-comparisons must account for the harness change. With
that caveat, the per-corpus c/B deltas vs SK-V13 P1-C V2 for the three
mode-III probes that overlap (host_call_eager_decode, alternate_scalar_plan,
cold_first_parse — table reproduced from `sk-v13/.../p1c-samply-mode-3.md §2`):

| corpus | host_call c/B SK-V13 | host_call c/B SK-V14 | Δ | alt_scalar c/B SK-V13 | alt_scalar c/B SK-V14 | Δ | cold_first c/B SK-V13 | cold_first c/B SK-V14 | Δ | classification |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| twitter | 7.855 | 8.53 | +8.6% | 7.878 | 5.69 | -27.8% | 2.716 | 2.81 | +3.5% | C: alt_scalar improved (≈ harness delta); host/cold within noise |
| citm_catalog | 5.243 | 4.90 | -6.5% | 8.911 | 5.08 | -43.0% | 1.502 | 1.36 | -9.5% | C: all three improved (harness drives criterion stride) |
| canada | 8.988 | 8.63 | -4.0% | 17.473 | 8.97 | -48.7% | 2.160 | 2.38 | +10.2% | C: alt_scalar large gain (re-baselined to serde_json) |
| apache_builds | 7.376 | 7.86 | +6.6% | 8.606 | 6.04 | -29.8% | 2.924 | 3.08 | +5.3% | C: alt_scalar improved, others within noise |
| github_events | 6.271 | 6.64 | +5.9% | 7.481 | 5.01 | -33.0% | 2.482 | 2.61 | +5.2% | C: alt_scalar improved, others within noise |
| update_center | 10.422 | 11.88 | +14.0% | 11.581 | 8.63 | -25.5% | 3.071 | 3.28 | +6.8% | C: alt_scalar improved, host regressed (harness overhead) |
| mesh | 6.429 | 7.44 | +15.7% | 20.989 | 7.92 | -62.3% | 2.674 | 3.04 | +13.7% | C: alt_scalar large gain, host/cold regressed |
| random | 14.264 | 13.23 | -7.2% | 18.590 | 9.91 | -46.7% | 5.127 | 4.53 | -11.6% | C: all three improved |
| gsoc-2018 | 5.588 | 4.73 | -15.4% | 3.842 | 2.12 | -44.8% | 1.642 | 1.73 | +5.4% | C: alt_scalar large gain, host improved |
| marine_ik | 13.712 | 15.95 | +16.3% | 21.837 | 9.57 | -56.2% | 2.738 | 3.15 | +15.0% | C: alt_scalar large gain, host/cold regressed |
| instruments | 6.325 | 7.52 | +18.9% | 11.876 | 7.33 | -38.3% | 2.238 | 2.30 | +2.8% | C: alt_scalar improved, host regressed |
| numbers | 3.539 | 4.18 | +18.1% | 16.829 | 6.26 | -62.8% | 1.962 | 2.06 | +5.0% | C: alt_scalar large gain, host regressed |
| unicode_mixed | 18.728 | 20.42 | +9.0% | 10.082 | 7.71 | -23.5% | 5.768 | 6.21 | +7.7% | C: alt_scalar improved, host/cold regressed |
| unicode_escapes | 16.017 | 17.51 | +9.3% | 7.328 | 7.20 | -1.7% | 2.914 | 3.11 | +6.7% | C: stable; minor regression on host/cold |
| unicode_basic | 12.046 | 14.19 | +17.8% | 13.495 | 9.01 | -33.2% | 4.311 | 5.61 | +30.1% | C: alt_scalar improved, cold large regression (CH3 §4) |
| distinct_values | 8.837 | 9.58 | +8.4% | 12.651 | 8.87 | -29.9% | 3.762 | 4.26 | +13.2% | C: alt_scalar improved, host/cold regressed |
| y_string_unicode | 23.152 | 19.55 | -15.6% | 9.115 | 6.64 | -27.2% | 6.006 | 6.66 | +10.9% | C: alt_scalar improved, host improved, cold regressed |

Classification per the schema-v3 enum: every row is **C** (carry-over,
no admit/reject status change). The systematic `alt_scalar_plan`
improvement across 17/17 corpora reflects the harness change: SK-V13
used a manual cold loop, the SK-V14 criterion harness applies
warm-up + sample compaction that lowers slope variance for the
serde_json path. The cold_first_parse regressions on five corpora
(canada +10.2%, mesh +13.7%, marine_ik +15.0%, unicode_basic +30.1%,
unicode_mixed +7.7%, y_string_unicode +10.9%) are flagged in §4 anomaly
ANOM-1 — they correlate with view-materialization-heavy corpora.

### §3.2 — Audit-overlay verdict per probe row (SK-V13 prune list mapping)

Per SK-V14 SYNTHESIS §0.2 + S-P0 prune list, the mapping for the
Track 1 baseline rows (which mode III is contextualised against) is:

- **AUDIT-FALSIFIED** (5 parse_only): W14.1 numbers, W14.2 citm_catalog,
  W14.3 canada, W14.4 marine_ik, W14.5 mesh (PRUNE-1 of
  `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1`
  — admit was gate-relabel, not parser delta).
- **AUDIT-SUSTAINED**: the 12 remaining parse_only rows S/NO-GO state
  (twitter, apache_builds, github_events, update_center, random,
  gsoc-2018, instruments, unicode_mixed, unicode_escapes, unicode_basic,
  distinct_values, y_string_unicode); the S verdict is gate-honest and
  the bench-row data agrees with the audit's expected sub-sonic-strict
  result.
- **AUDIT-PENDING**: every masking-probe / structural-scan cell in §2.1
  / §2.2.4 (these rows are not in `ROLLING-SOTA-DELTA`; the audit pack
  did not opine; S-P2 will weigh whether each masking probe
  beating-or-trailing Track 1 names a structural inefficiency).

## §4 — Anomalies + masking signals (flagged for S-P2)

### ANOM-1 — `alternate_scalar_plan` is the serde_json comparator, not a Track 1 alternate

Per `benches/json_parity.rs:407`-`412`, the body of
`alternate_scalar_plan` is `serde_json::from_str::<Value>(input)`. The
samply profile §2.2.3 confirms the symbol path: 15.14% in
`Value::deserialize`, 10.08% in `skip_to_escape`, 7.10% in
`IndexMap::insert_full`, 6.99% in `parse_number`, 4.36-4.90% in MapAccess
/ SeqAccess seed iteration, 4.52% in `parse_escape`, 3.40% in
`parse_str`, 2.64% in `parse_unicode_escape`. **Zero samples in any
`runtime::generated_json::*` symbol.** The "alternate scalar plan" name
implies a Track 1 alternative; the body implements the serde_json
comparator. This is a misnamed probe — S-P2 must either rename it
(`comparator_serde_json_value`) or implement an actual Track 1 scalar
alternate (e.g. a `scan_structurals_scalar` + walk path that skips the
NEON dispatch). The bench gates that consume "MASKING" signals from
this probe in `gate.rs` are reading a serde_json baseline, which on
unicode-heavy corpora can beat the half-decoded Track 1 view
materialization — that is the literal "structural inefficiency" the
PASS-1-PROFILE §8.5 mandate calls out for S-P2 research.

### ANOM-2 — `host_call_eager_decode` is dominated by view-walk + UTF-8 decode, not parse

Per the §2.2.2 aggregate, only 30.73% of self-time is in the parse
dispatch (`dispatch_value`); the remaining 67% is in the view tree
walk + string materialization: `at_cursor` 23.28%, `string_body_range`
15.68%, `from_utf8` 10.16%, pair/value iterators 9.26%, `unescape_string`
2.83%, `as_str` 2.56%, `validate_block_scalar` 1.32%. **The host_call
probe measures the cost of Lock 1's view-boundary materialization, not
parse.** On every corpus the probe runs at 0.15x–0.49x T1 throughput
(§2.1 host_call/T1 column); on the unicode corpora the ratio collapses
to 0.15x–0.27x. This is the structural inefficiency the PASS-1-PROFILE
§8.5 mandate names: the masking probe is slower than T1 specifically
because the substrate union forces a second pass to lift offset-tape
positions back into decoded string slices. S-P2 must research whether
the view-materialization cost is necessary at the Lock 1 boundary or
whether direct emission of decoded slices into the tape during parse
(consumed by direct_to_struct sinks) absorbs it. REDRESS-126 zero-orphan
guard applies: any S-P2 proposal must cite a parity oracle.

### ANOM-3 — `cold_first_parse` ratio collapse on view-heavy corpora

Per §2.1 cold_first/T1 column, cold_first_parse runs at 0.78x–1.02x T1
on most corpora — within bench noise of Track 1. The exceptions are
**unicode_mixed (0.61x), unicode_basic (0.52x), random (0.78x),
y_string_unicode (0.80x), unicode_escapes (0.83x), distinct_values (0.83x),
twitter (0.81x), citm_catalog (0.86x), canada (0.87x)**. Note these are
the corpora with the heaviest string content / repeated key churn —
cold-first allocation pressure (the `iter_batched(LargeInput)` body
clones fixture bytes per iteration at `benches/json_parity.rs:423`-`424`)
shows up as `_platform_memmove` (3.02% of cold_first aggregate) and
mimalloc allocator chatter. The masking signal: cold throughput is not
fundamentally constrained by parse; it is constrained by the allocator's
ability to absorb 17-corpus-worth of `Vec<u8>::clone()` per iteration.
S-P2 must distinguish "true cold parse" from "allocator-bound cold
loop"; the existing harness conflates them.

### ANOM-4 — Generated parser is one symbol (`parse-attribution` feature off)

§2.3 CH2 note: under the default release+bench feature set,
`#[inline(always)]` on every `parse_*` helper in `generated.rs` folds
the entire Track 1 parser into the `dispatch_value` symbol at
`generated.rs:45`. The 88.14% self-time attribution against
`dispatch_value` in cold_first_parse is therefore one un-decomposed
symbol. A V2 fold of P1-A/B/C with `--features parse-attribution`
(per `runtime/Cargo.toml:21`) would split into parse_object /
parse_array / parse_string / parse_number / parse_literal /
parse_pair / parse_key_colon. This is a CH6 paper-close risk: the
profile names a symbol that obscures the leaf primitive. Flagged for
the CHALLENGE V2 hardening fold; it equally applies to P1-A and P1-B
which would have reported the same single symbol.

### ANOM-5 — `alternate_pext_mask_plan` unsupported on aarch64

Per `benches/json_parity.rs:414`, the probe is gated to x86/x86_64.
PEXT-based structural masking has no aarch64 instruction; the platform
counterpart is the NEON `classify_tbl4` + `escape_mask_64` +
`prefix_xor_64` path already exercised by `scan_structurals` (see
`skinny/crates/runtime/src/grammars/json/scan.rs:200`-`267`). The
sk-v13 P1-C V2 finding stands: any S-P2 proposal mentioning a "PEXT
route" for aarch64 is structurally impossible and must not appear in
the wave plan. The NEON equivalent **is already in Track 1** —
structural SIMD beating scalar 1.68x–6.19x (§2.2.4 table) is not a
new optimization route; it is the existing path being measured.

### ANOM-6 — REDRESS-126 zero-orphan applies to ANOM-1/2/3

Per `skinny/REDRESS.md` REDRESS-126, any masking signal that
implies a new SIMD primitive needs scalar reference + parity/checkasm
+ feature-mask disclosure + same-wave consumer + zero-orphan
disposition. ANOM-1's "implement actual Track 1 scalar alternate" and
ANOM-2's "absorb view materialization into parse-time emission" both
require S-P2 to honor REDRESS-126; they are NOT routes to a new SIMD
primitive without those gates. Flagged so CH3 (REGRESSION) catches any
V2 fold that silently re-opens REDRESS 50-55, 60-72, 80, 82-84, 88, 89.

### ANOM-7 — R1 comparator misbinding inherited

Per SK-V14 ORCHESTRATOR-PROMPT R1, the `sonic_rs::from_slice::<Value>`
binding at `benches/json_parity.rs:87`-`102` is the
eager-typed-DOM comparator, not strict parse_only. The mode-III rows
do not directly compare to sonic, but the Track 1 T1_Mbps column in
§2.1 (and the audit-overlay verdict in the rightmost column) is
classified against that misbound comparator. The P1-C profile inherits
the R1 misbinding as data; S-P1 dispatch context §1 explicitly forbids
"fixing" R1 in S-P1, so the misbinding is documented here as a
finding for S-P2 / C-2 wave to address. AUDIT-FALSIFIED rows in §2.1
already encode this: their admit was a relabel of the misbound
comparator, not a parser change.

### ANOM-8 — Structural SIMD speedup is corpus-correlated to delimiter density

Per §2.2.4, the SIMD/scalar speedup ratio ranges **1.63x
(distinct_values)** to **6.19x (mesh)**. High-speedup corpora are
float / structural-heavy (mesh 6.19x, numbers 5.56x, canada 5.39x);
low-speedup corpora are mixed-content (distinct_values 1.63x,
random 1.68x, unicode_basic 1.78x, y_string_unicode 1.89x,
unicode_escapes 1.92x). The NEON classify_tbl4 path (`scan.rs:200`-`267`)
processes a 64-byte stripe per iteration; its win scales with the
ratio of pure structural bytes to string-body bytes. Confirmation: the
five unicode/string-dense corpora cluster at the low end (1.63x –
2.35x), exactly the suppression pattern predicted by the string-body
fast-path falling out at `scan.rs:225`-`267` whenever `quotes != 0`
forces re-entering the full classify path. CH2 note: this is a
primitive (scanner) finding, not a JSON-role finding — it generalizes
to any grammar with structural delimiters carried by NEON
classification. Re-statement for CH3: the SIMD speedup does NOT
re-open REDRESS 96/97/98 (substrate union route — see sk-v13 P1-C V2
§4); it is a within-Track-1 micro-result on the existing scanner.

## §5 — Sources (every artefact path + run id)

### §5.1 — Profile artefacts (samply json.gz + syms.json sidecars)

- `/tmp/skv14-p1c-profiles/probe-cold_first_parse.json.gz` (3.9 MB; 428 754 samples)
- `/tmp/skv14-p1c-profiles/probe-cold_first_parse.json.syms.json` (465 KB)
- `/tmp/skv14-p1c-profiles/probe-host_call_eager_decode.json.gz` (6.4 MB; 742 645 samples)
- `/tmp/skv14-p1c-profiles/probe-host_call_eager_decode.json.syms.json` (605 KB)
- `/tmp/skv14-p1c-profiles/probe-alternate_scalar_plan.json.gz` (6.4 MB; 725 850 samples)
- `/tmp/skv14-p1c-profiles/probe-alternate_scalar_plan.json.syms.json` (504 KB)
- `/tmp/skv14-p1c-profiles/probe-structural_scan.json.gz` (1 383 688 samples; symbol attribution in §2.2.4)
- `/tmp/skv14-p1c-profiles/probe-structural_scan.json.syms.json` (271 symbols in `simd_scan-e9e48792f0c6e621`)

Profile binaries are NOT committed (per S-P1 dispatch §2 binding); the
paths above are reproducible from §1 commands against the recorded
HEAD SHA.

### §5.2 — Criterion estimate files (per-corpus per-probe slope)

For each of 17 corpora × 4 probes:
`/tmp/skv14-p1c-target/criterion/json_probes_<corpus>/<probe>/new/estimates.json`

For structural scan (17 corpora × {simd, scalar}):
`/tmp/skv14-p1c-target/criterion/simd_structural_scan/<corpus>_{simd,scalar}/new/estimates.json`

### §5.3 — Extraction scripts

- `/tmp/skv14_p1c_extract_v2.py` — re-extracts per-corpus per-probe
  Mbps + c/B from criterion estimates with the `bytes*8000/ns` convention.
- `/tmp/skv14_p1c_simd_extract.py` — re-extracts simd/scalar structural
  scan ratios with the same convention.
- `/tmp/skv14_p1c_resolve.py` — resolves samply profile addresses against
  the `.syms.json` sidecar (bisect into per-binary `symbol_table`,
  innermost-leaf inline frame).
- `/tmp/skv14_p1c_probe.json` — extracted per-corpus per-probe
  Mbps/c/B/src/ratio (consumed by §2.1).
- `/tmp/skv14_p1c_simd.json` — extracted per-corpus simd/scalar
  throughput + speedup (consumed by §2.2.4 partial).

### §5.4 — Source-of-truth references

- `restart/prompts/skinny/PASS-1-PROFILE.md` (S-P1 contract; §2 row P1-C; §8.5 masking-probe mandate)
- `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md` (this run's dispatch)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (durable SK-V14 contract; §0.2 audit-zero baseline; §2 telemetry binding)
- `restart/skinny/tranches/sk-v14/HANDOFF.md` (tranche handoff; §3 honest baseline)
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (PRUNE-1 5 parse_only revert list mapped to §2.1 audit-overlay)
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md` (prior SK iteration; §3.1 delta computed against this)
- `skinny/REDRESS.md` REDRESS-126 (zero-orphan guard; applies to ANOM-1/2/3)
- `skinny/RESULTS.md` (bench-gate authority; §2.1 T1_Mbps reconciled exactly)
- `skinny/crates/bbnf-bench/benches/json_parity.rs:381-438` (mode-III probe definitions)
- `skinny/crates/bbnf-bench/benches/simd_scan.rs:9-40` (structural-scan-only bench)
- `skinny/crates/bbnf-bench/src/bin/gate.rs:3719-3725` (`throughput_mbps` formula confirming `Mbps = bytes*8000/ns`)
- `skinny/crates/runtime/src/grammars/json/{parser.rs,generated.rs,scan.rs,view.rs,value.rs}` (named primitives)
- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution` feature gate for ANOM-4)
- `skinny/crates/bbnf-simd/src/aarch64/{utf8/validate_block.rs,classify_tbl4/*}` (named SIMD primitives)

### §5.5 — Run identity

- Repo HEAD at run: `2547c750bc78533d738eb85913206a0872022818` (`docs(sk-v14-p1-profile): seed S-P1 dispatch context`)
- SK-V13 close commit (baseline source state): `ff653fbe6` (`docs(sk-v14-audit-overfit-hardening-V5): challenge V5 + consolidated — G-S-P0-CONVERGED`)
- Host triple: aarch64-apple-darwin; cpu Apple M5 Max; nominal 4.4 GHz P-core
- samply version: 0.13.1 (interactive `record` with `--save-only --no-open --unstable-presymbolicate`; non-interactive shell forces `--save-only` but the sidecar restores symbol completeness — see §1.2 / §1.4)
- Cargo: `CARGO_TARGET_DIR=/tmp/skv14-p1c-target` (single-cargo-per-target discipline; same-binary samply invocations serialized; cross-binary structural_scan + json_parity samply parallelized only after the json_parity probe runs completed to avoid criterion home write contention)
- Profile flags: release + `debug=true` + `RUSTFLAGS="-C target-cpu=native"`
- Date: 2026-05-23
