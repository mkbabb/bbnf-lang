# SK-V9 P1-C: Samply Mode III Masking And Structural-Scan Profile

Pass: S-P1 Profile. Cycle: V2 post-W0 rerun.
Date: 2026-05-18.
Scope: masking probes (`host_call_eager_decode`, `alternate_scalar_plan`,
`cold_first_parse`) plus structural-scan-only SIMD path for all 17 corpora.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, bench profile, `debug=true`,
`strip=false`, `split-debuginfo=packed`.
Profile tool: `samply 0.13.1`, interactive `record --no-open`,
`--unstable-presymbolicate`, 4000 Hz, main-thread-only.
Corpus coverage: 17/17 for all three probes and 17/17 structural scan.

## §1 - Method

Commands:

```bash
export OUT=/tmp/skv9-p1-rerun
export CARGO_TARGET_DIR="$OUT/target"
export RUSTFLAGS="-C target-cpu=native"
cargo build --profile=bench -p bbnf-bench --bench json_parity --bench simd_scan
samply record --no-open --rate 4000 --main-thread-only \
  --unstable-presymbolicate \
  -o "$OUT/profiles/p1c/<corpus>.<probe>.profile.json.gz" \
  "$OUT/target/release/deps/json_parity-2b989ba81fb30ea5" \
  --bench --profile-time 1 "json/probes/<corpus>/<probe>"
samply record --no-open --rate 4000 --main-thread-only \
  --unstable-presymbolicate \
  -o "$OUT/profiles/p1c/<corpus>.structural_scan.simd.profile.json.gz" \
  "$OUT/target/release/deps/simd_scan-b7676c9550d0bd5e" \
  --bench --profile-time 1 "simd/structural_scan/<corpus>/simd"
```

Harness caveat: P1-C uses Criterion bench executables because the repo has no
focused probe profile binary. The profiles are valid and symbol-resolving, but
they include Criterion harness frames and fixture/parity setup frames.

## §2 - Findings

| Corpus | Mode III top self-time symbols |
|---|---|
| `twitter` | eager: 19.4% `dispatch_value`; serde: 11.6% `IndexMap::insert_full`; cold: 68.1% `dispatch_value`; scan: 68.0% `structural_offsets_simd` |
| `citm_catalog` | eager: 30.8% `JsonNodeKind::at_cursor`; serde: 16.2% serde map key; cold: 64.1% `dispatch_value`; scan: 69.6% `structural_offsets_simd` |
| `canada` | eager: 36.6% `JsonNodeKind::at_cursor`; serde: 21.6% serde `Value` deserialize; cold: 72.5% `dispatch_value`; scan: 50.5% `structural_offsets_simd` |
| `apache_builds` | eager: 31.9% `dispatch_value`; serde: 16.4% `_platform_memmove`; cold: 78.2% `dispatch_value`; scan: 61.3% `structural_offsets_simd` |
| `github_events` | eager: 31.1% `dispatch_value`; serde: 11.5% serde `skip_to_escape`; cold: 75.0% `dispatch_value`; scan: 74.8% `structural_offsets_simd` |
| `update_center` | eager: 23.6% `dispatch_value`; serde: 15.0% `_platform_memmove`; cold: 79.5% `dispatch_value`; scan: 53.9% `structural_offsets_simd` |
| `mesh` | eager: 31.8% `dispatch_value`; serde: 21.2% serde `Value` deserialize; cold: 70.1% `dispatch_value`; scan: 57.4% `structural_offsets_simd` |
| `random` | eager: 25.0% `dispatch_value`; serde: 12.3% `_platform_memmove`; cold: 64.2% `dispatch_value`; scan: 46.3% `structural_offsets_simd` |
| `gsoc-2018` | eager: 28.1% `string_body_range`; serde: 19.3% serde `skip_to_escape`; cold: 73.8% `dispatch_value`; scan: 80.2% `structural_offsets_simd` |
| `marine_ik` | eager: 35.2% `JsonNodeKind::at_cursor`; serde: 15.8% serde `Value` deserialize; cold: 56.9% `dispatch_value`; scan: 53.9% `structural_offsets_simd` |
| `instruments` | eager: 21.9% `JsonNodeKind::at_cursor`; serde: 9.9% serde `Value` deserialize; cold: 67.3% `dispatch_value`; scan: 63.2% `structural_offsets_simd` |
| `numbers` | eager: 36.7% `dispatch_value`; serde: 29.7% serde `parse_number`; cold: 73.1% `dispatch_value`; scan: 59.7% `structural_offsets_simd` |
| `unicode_mixed` | eager: 26.5% `string_body_range`; serde: 14.9% serde `skip_to_escape`; cold: 52.2% `dispatch_value`; scan: 72.0% `structural_offsets_simd` |
| `unicode_escapes` | eager: 41.4% `string_body_range`; serde: 30.9% serde `parse_escape`; cold: 78.8% `dispatch_value`; scan: 86.6% `structural_offsets_simd` |
| `unicode_basic` | eager: 25.4% `from_utf8`; serde: 12.9% `_platform_memmove`; cold: 49.5% `dispatch_value`; scan: 52.4% `structural_offsets_simd` |
| `distinct_values` | eager: 32.8% `dispatch_value`; serde: 11.0% `_platform_memmove`; cold: 74.5% `dispatch_value`; scan: 53.1% `structural_offsets_simd` |
| `y_string_unicode` | eager: 25.0% `dispatch_value`; serde: 16.5% serde `parse_unicode_escape`; cold: 77.0% `dispatch_value`; scan: 67.6% `structural_offsets_simd` |

Full top-20 self-time data with source paths and line numbers is in
`/tmp/skv9-p1-rerun/profile-summary.json`.

## §3 - Delta vs SK-V8

Mode III probes are diagnostic non-producers in W0. No mode III profile changes
row class, Track 1 admission, Track 2 proof, direct proof, or typed proof.

## §4 - Anomalies + Masking Signals

- Eager decode shifts hot time out of the fused parse leaf and into view access:
  `JsonNodeKind::at_cursor`, `string_body_range`, iterators, and UTF-8 checks.
- `alternate_scalar_plan` profiles are serde_json baselines, not proposed
  implementation routes.
- Structural scan profiles consistently attribute to transient scan functions;
  W0 fences them as `structural_scan+masking_probes+pmu+cycles:nonproducer`.
- There is no focused P1-C binary, so any S-P2 use of these profiles must filter
  Criterion harness frames.
- P1-D PMU absence blocks S-P1 convergence.

## §5 - Sources

- `/tmp/skv9-p1-rerun/profiles/p1c/*.profile.json.gz`
- `/tmp/skv9-p1-rerun/profiles/p1c/*.profile.json.syms.json`
- `/tmp/skv9-p1-rerun/logs/p1c.*.log`
- `/tmp/skv9-p1-rerun/profile-summary.json`
