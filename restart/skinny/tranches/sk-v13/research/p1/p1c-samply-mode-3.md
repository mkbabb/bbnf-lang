# SK-V13 P1-C: Samply Mode III Masking + Structural Probe Profile

Pass: S-P1 Profile. Cycle: V13 / S-P1 V2 fold.
Date: 2026-05-21.
Scope: samply profiling mode III: `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, and structural-scan-only probes.
Output: this file.
Baseline: SK-V13-open (`7ee299096be7d7fdaa0e69344a6cd18bbd55524f`; no `skinny/crates/` source delta from the V1 profile build).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS="-C target-cpu=native"`, temp profiler target `/tmp/skv13-mode3-profiler-target`.
Profile tool: samply 0.13.1, `samply record --save-only --unstable-presymbolicate -r 1000`; proc counters emitted by the temporary mode-III profiler.
Corpus coverage: 17/17 JSON corpora x 5 captured probes = 85/85 profiles, 0 bad rc; `alternate_pext_mask_plan` and `alternate_dispatch_table_plan` explicitly unsupported.

V3 fold note: the mode-III harness provenance is checked in at
`support/mode3-harness-provenance.md`, and the mode-III rows are classified in
`support/evidence-ledger-v3.md`. Rank-1 mode-III rows remain
`function-only-sidecar` unless the source anchor is separately stated.

## §1 - Method (commands run; verbatim, reproducible)

Identity:

```bash
cat /tmp/skv13-p1-v2/artifacts/identity.txt
# root=/tmp/skv13-p1-v2
# repo=/Users/mkbabb/Programming/bbnf-lang
# head=7ee299096be7d7fdaa0e69344a6cd18bbd55524f
# date=2026-05-21T06:56:28Z
```

The mode-III profiler was a temporary harness outside the repository at
`/tmp/skv13-mode3-profiler`. It linked the checked-out `runtime` and
`bbnf-bench` crates, then executed cold single-process probes without editing
tracked source.

```bash
CARGO_TARGET_DIR=/tmp/skv13-mode3-profiler-target \
RUSTFLAGS='-C target-cpu=native' \
cargo build --release --manifest-path /tmp/skv13-mode3-profiler/Cargo.toml

samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1-v2/mode3/profiles/mode3__${corpus}__${mode}.json.gz \
  /tmp/skv13-mode3-profiler-target/release/skv13-mode3-profiler \
  "${corpus}" "${mode}" "${iters}"
```

Coverage checks:

```bash
awk -F '\t' 'NR>1{n++; bad+=($4!=0)} END{print n,bad+0}' \
  /tmp/skv13-p1-v2/mode3/capture_status.tsv
# 85 0

awk -F '\t' 'NR>1{n++} END{print n+0}' /tmp/skv13-p1-v2/mode3/mode3_rows.tsv
# 85

cat /tmp/skv13-p1-v2/mode3/unsupported.tsv
# unsupported all alternate_pext_mask_plan aarch64_no_pext
# unsupported all alternate_dispatch_table_plan disabled_duplicate_probe
```

## §2 - Findings (per-corpus table; file:line on every hot-leaf claim)

The full top-20 symbol extraction for every mode-III profile is
`/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`. Structural rows resolve to
`skinny/crates/runtime/src/grammars/json/scan.rs` when sidecars carry line
frames; some NEON symbols resolve to function names without file:line because
the sidecar lacks inline frames for those ASM-facing leaves.

| Corpus | host_call Mbps / cB | alternate_scalar Mbps / cB | cold_first Mbps / cB | structural scalar top | structural SIMD top | SIMD/scalar Mbps |
|---|---:|---:|---:|---|---|---:|
| twitter | 4260.142 / 7.855 | 4244.225 / 7.878 | 12323.050 / 2.716 | 96.9% `scan_tail` | 67.5% `scan_structurals` | 2.33x |
| citm_catalog | 5507.054 / 5.243 | 3194.645 / 8.911 | 19038.622 / 1.502 | 95.8% `scan_tail` | 69.0% `scan_structurals` | 2.32x |
| canada | 3016.084 / 8.988 | 1828.552 / 17.473 | 14953.562 / 2.160 | 96.8% `scan_tail` | 52.6% `scan_structurals` | 5.01x |
| apache_builds | 3773.688 / 7.376 | 3033.349 / 8.606 | 8490.531 / 2.924 | 92.6% `scan_tail` | 62.1% `scan_structurals` | 2.10x |
| github_events | 4808.017 / 6.271 | 3902.262 / 7.481 | 9624.589 / 2.482 | 95.4% `scan_tail` | 65.6% `scan_structurals` | 2.02x |
| update_center | 3122.228 / 10.422 | 2813.984 / 11.581 | 10248.726 / 3.071 | 95.6% `scan_tail` | 57.5% `scan_structurals` | 1.89x |
| mesh | 5051.347 / 6.429 | 1581.993 / 20.989 | 12228.383 / 2.674 | 97.1% `scan_tail` | 62.6% `scan_structurals` | 5.04x |
| random | 2016.474 / 14.264 | 1513.999 / 18.590 | 5484.563 / 5.127 | 96.4% `scan_tail` | 48.7% `scan_structurals` | 1.49x |
| gsoc-2018 | 4851.651 / 5.588 | 7153.570 / 3.842 | 19729.022 / 1.642 | 94.8% `scan_tail` | 77.2% `scan_structurals` | 2.16x |
| marine_ik | 2449.712 / 13.712 | 1536.291 / 21.837 | 12128.453 / 2.738 | 96.1% `scan_tail` | 55.3% `scan_structurals` | 3.06x |
| instruments | 4593.891 / 6.325 | 2508.692 / 11.876 | 10807.279 / 2.238 | 95.8% `scan_tail` | 69.9% `scan_structurals` | 2.08x |
| numbers | 7191.784 / 3.539 | 1987.181 / 16.829 | 12480.800 / 1.962 | 97.8% `scan_tail` | 51.4% `scan_structurals` | 4.96x |
| unicode_mixed | 1810.853 / 18.728 | 3336.033 / 10.082 | 5776.651 / 5.768 | 96.2% `scan_tail` | 72.0% `scan_structurals` | 2.18x |
| unicode_escapes | 2109.132 / 16.017 | 4515.239 / 7.328 | 10969.810 / 2.914 | 96.0% `scan_tail` | 87.5% `scan_structurals` | 1.84x |
| unicode_basic | 2804.541 / 12.046 | 2503.356 / 13.495 | 7607.130 / 4.311 | 94.9% `scan_tail` | 52.0% `scan_structurals` | 1.67x |
| distinct_values | 3348.059 / 8.837 | 2251.698 / 12.651 | 6932.905 / 3.762 | 95.3% `scan_tail` | 48.2% `bulk_emit_positions_64_neon` | 1.52x |
| y_string_unicode | 1181.961 / 23.152 | 3272.506 / 9.115 | 4484.753 / 6.006 | 84.4% `scan_tail` | 52.9% `scan_structurals` | 1.92x |

Representative symbol/file anchors:

- `runtime::generated_json::scan::scan_tail`:
  `skinny/crates/runtime/src/grammars/json/scan.rs:32`-`35`.
- `runtime::generated_json::scan::scan_structurals`:
  `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`30`.
- `bbnf_simd::aarch64::bulk_emit_positions_64::bulk_emit_positions_64_neon`:
  sidecar symbol name only in V2; file:line absent.

## §3 - Delta vs SK-V12 (per row; Mbps + c/B + classification)

SK-V12 did not publish a mode-III corpus matrix, so there is no like-for-like
prior-tranche delta. V2 closes the V1 P1-C coverage defect:

| Probe family | V1 state | V2 state | Classification |
|---|---|---|---|
| `host_call_eager_decode` | absent | 17/17 profiles + proc counters | measured masking probe |
| `alternate_scalar_plan` | absent | 17/17 profiles + proc counters | measured masking probe |
| `cold_first_parse` | absent | 17/17 profiles + proc counters | measured cold-first probe |
| structural scalar | absent except stale Canada note | 17/17 profiles + proc counters | measured structural baseline |
| structural SIMD | absent except stale Canada note | 17/17 profiles + proc counters | measured structural SIMD comparator |
| `alternate_pext_mask_plan` | absent | unsupported: aarch64 has no PEXT | unsupported, not a miss |
| `alternate_dispatch_table_plan` | disabled historically | unsupported: duplicate probe disabled | unsupported, not a valid row |

## §4 - Anomalies + masking signals (flagged for S-P2)

- Structural SIMD beats scalar scan on all 17 corpora, but that is a scanner
  micro-result. It does not by itself reopen the REDRESS 96/97/98 union route;
  any union attempt must cite a material differential and prove row movement.
- The biggest structural SIMD/scalar ratios are `mesh` 5.04x, `canada` 5.01x,
  and `numbers` 4.96x. The smallest are `random` 1.49x and
  `distinct_values` 1.52x.
- `alternate_scalar_plan` beats `host_call_eager_decode` on `gsoc-2018`,
  `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`; this is a
  masking signal for string/unicode rows, not an implementation plan.
- `alternate_pext_mask_plan` is explicitly out on aarch64. It must not appear
  in S-P2 as a missing optimization route.
- Some NEON symbols lack file:line frames in sidecars. The function names are
  citable; file:line precision remains a CH6 risk for those ASM leaves.
- REDRESS-126 zero-orphan guard: PEXT, line-poor NEON leaves,
  `bulk_emit_positions_64_neon`, and absent C/C++ sidecars are telemetry or
  attribution gaps only. They do not create an orphan SIMD primitive or reopen
  production PMULL/CSSC/prefix-XOR/bulk-emission routes. Any future SIMD
  primitive needs scalar reference, parity/checkasm, feature-mask disclosure,
  same-wave consumer, and zero-orphan disposition.

## §5 - Sources (every artefact path + run id)

- `/tmp/skv13-p1-v2/artifacts/identity.txt`
- `/tmp/skv13-p1-v2/mode3/capture_status.tsv`
- `/tmp/skv13-p1-v2/mode3/mode3_rows.tsv`
- `/tmp/skv13-p1-v2/mode3/unsupported.tsv`
- `/tmp/skv13-p1-v2/mode3/logs/mode3__{corpus}__{mode}.log`
- `/tmp/skv13-p1-v2/mode3/profiles/mode3__{corpus}__{mode}.json.gz`
- `/tmp/skv13-p1-v2/mode3/profiles/mode3__{corpus}__{mode}.json.syms.json`
- `/tmp/skv13-p1-v2/summary/mode3_summary.tsv`
- `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`
- `restart/skinny/tranches/sk-v13/research/p1/support/mode3-harness-provenance.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
