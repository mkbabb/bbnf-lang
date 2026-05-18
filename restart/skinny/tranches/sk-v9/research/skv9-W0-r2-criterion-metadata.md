# SK-V9 W0 R2: Criterion Metadata And Capture Coherence

Date: 2026-05-18.
Wave: SK-V9 W0 telemetry-lock research.
Role: R2 Criterion metadata and capture coherence.
Owned output: `restart/skinny/tranches/sk-v9/research/skv9-W0-r2-criterion-metadata.md`.
Disposition: research schema only. No code, benchmark output, or commit.

## 1. Scope

W0 is a gate-only telemetry recovery wave. It may update run identity, report
labels, manifest validation, replay metadata, and diagnostic fences. It must not
move parser, scanner, SIMD, codegen, throughput cells, Apache/CITM measured-row
admission, direct-product proof, or strict admission from deferred/view-boundary
rows. This follows the P1 hardening fold that requires W0 before any behavior
wave and records the current `gate-json --advisory --check-results` failure as
`twitter SIMD metadata invalid: SIMD metadata is from a different capture`.

R2's target is a falsifiable schema for a coherent `SK-V9-open` Criterion
capture. The schema must make stale cache reuse, mixed row metadata, missing
native build flags, and lane sample-policy drift observable to `gate-json`.

## 2. Evidence Read

| Source | R2 finding |
|---|---|
| `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:42-48` | W0 may update telemetry identity, labels, manifest validation, replay metadata, and diagnostic fences only. |
| `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:81-83` | Current advisory gate fails because SIMD metadata is from a different capture. |
| `skinny/crates/bbnf-bench/src/metadata.rs:20-65` | `RowMetadata` schema v3 records host, flags, target CPU, profile, input identity, commit, sample policy, track, semantics, parity hashes, and cache mode. It does not record a row-local run id. |
| `skinny/crates/bbnf-bench/src/metadata.rs:110-123` | `HostFacts::probe()` reads `RUSTFLAGS`, derives `target_cpu`, captures `git rev-parse HEAD`, and stores them into every row. |
| `skinny/crates/bbnf-bench/src/metadata.rs:517-533` | `target_cpu` parsing accepts both `-C target-cpu=...` and `-Ctarget-cpu=...`, but the SIMD gate later requires the canonical full `rustflags` string. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs:31-42` | JSON benchmark policy is per fixture: `canada` uses sample size 50 and 8 seconds; all others use sample size 100 and 5 seconds. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs:490-508` | JSON benchmark metadata is written to `target/criterion/json_<corpus>/<bench>/metadata.toml`. |
| `skinny/crates/bbnf-bench/benches/simd_scan.rs:13-14` and `skinny/crates/bbnf-bench/benches/simd_scan.rs:80-86` | SIMD structural scan policy is sample size 100, 5 seconds, 3 second warmup, 95% confidence, 0.05 significance, 0.02 noise threshold. |
| `skinny/crates/bbnf-bench/benches/simd_scan.rs:49-61` | SIMD scan writes `RowMetadata::simd_scan(...)` with scalar parity hash before Criterion executes the timed group. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:375-410` | Current report run facts hard-code `sk-v8-open:criterion-fnv64-<fingerprint>` and render `target_cpu` from the gate process environment. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:673-743` | The run fingerprint hashes selected `estimates.json` and `metadata.toml` files, including W0 main rows, SIMD metadata, and Canada SIMD estimates. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:1075-1112` | Main JSON metadata validation checks required fields, fixture hash/bytes, same-capture fields, and required benchmark semantics. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:1119-1184` | Same-capture currently means identical host, flags, target CPU, profile, commit, warmup, sample size, measurement time, confidence, outlier rejection, and statistical method within the row cohort being validated. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs:1389-1447` | SIMD metadata validation checks required fields, fixture identity, SIMD semantics, same host/build/profile/commit as the main row, exact native build policy, 100/5 sample policy, and scalar parity hash equality. |
| `skinny/crates/bbnf-bench/src/gate.rs:136-182` | Strict admission rejects stale, historical, absent, sidecar-same-run, non-native, mismatched-plane, deferred, or view-boundary evidence. |
| `skinny/RESULTS.md:44-85` and `skinny/RESULTS.md:138-141` | Current rendered manifest is still `SK-V8-open`, has a uniform `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` run id, and states that native Rust comparators are same-run while C++ sidecars are historical or absent. |

Local cache observation, non-authority: the current `skinny/target/criterion`
cache has `json_twitter/track1_generated/metadata.toml` at commit
`00c184136ed8371c1f076b6b750e2bec313e7803`, while
`simd_structural_scan/twitter_simd/metadata.toml` is at
`7e490271ff1bb151178e001a217989e8c71e87ec`. That concretely explains the
hardening failure class: the SIMD row is stale relative to the main row.

## 3. Capture Model

`SK-V9-open` must be a single report-level capture with lane-specific Criterion
policies. It is not enough for each row to have valid v3 metadata; the report
must prove that every row admitted into the manifest belongs to one replayable
capture identity.

### 3.1 Report Header

| Field | Required value / rule |
|---|---|
| `wave_id` | `SK-V9-open`. Any `SK-V8-open` label is a stale baseline, not W0 closure. |
| `run_id` | `sk-v9-open:criterion-fnv64-<16 lowercase hex>`, derived from the same selected Criterion inputs that `gate-json` consumes. |
| `consumer` | `gate-json`, same wave. The report is invalid if the manifest is written but not consumed by the gate in W0. |
| `criterion_root` | One declared `target/criterion` root, preferably an empty or W0-dedicated `CARGO_TARGET_DIR` before capture. |
| `bbnf_commit` | One commit for every JSON metadata row, every SIMD metadata row, and the gate process. Mixed commits falsify the capture even if each fixture validates locally. |
| `profile` | `bench`. |
| `rustflags` | Exactly `-C target-cpu=native` for rows and gate-rendered run facts. |
| `target_cpu` | `native`. |
| `host` | One host tuple: CPU model, CPU arch, OS kernel, rustc host triple. |
| `fixture_suite` | The 17-corpus fixture set loaded by `test_fixtures::load_available_bench_fixtures()`, with each row bound to fixture `sha256` and byte length. |

### 3.2 Row Cohorts

Each corpus has one JSON cohort and one SIMD diagnostic row.

| Cohort | Membership | Same-capture keys |
|---|---|---|
| JSON main cohort | `track1_generated`, `track2_handcoded`, native Rust competitors, direct rows, and any baseline-expected real typed rows for the corpus. | `cpu_model`, `cpu_arch`, `os_kernel`, `rustflags`, `target_cpu`, `profile`, `bbnf_commit`, `warmup_samples`, `warmup_time_s`, `sample_size`, `measurement_time_s`, `confidence_interval`, `outlier_rejection`, `statistical_method`. |
| SIMD diagnostic row | `simd_structural_scan/<corpus>_simd/metadata.toml`. | Must match the JSON cohort on host/build/profile/commit keys; must use its own SIMD lane sample policy; must carry the correct fixture hash/bytes and scalar parity hash. |
| Report-wide capture | All corpus cohorts. | Must share host/build/profile/commit keys report-wide. Per-corpus sample policy may differ only where the lane policy allows it. |

The current code validates same-capture within each JSON corpus and validates
SIMD against the first main row for host/build/profile/commit. R2's schema adds
one report-wide rule: no corpus may carry a different `bbnf_commit`, `rustflags`,
`target_cpu`, `profile`, host model, arch, or OS kernel under the same
`SK-V9-open` run id.

## 4. Sample Policy

Sample policy is lane-specific. A coherent capture must reject accidental
uniformity rules that erase the `canada` exception, and must also reject stale
SIMD rows whose policy or commit is inherited from another run.

| Lane | Corpus | Required `sample_size` | Required `measurement_time_s` | Required warmup / stats |
|---|---|---:|---:|---|
| JSON main | `canada` | 50 | 8.0 | warmup samples 3, warmup 3.0s, confidence 0.95, outlier `iqr`, method `bootstrap`. |
| JSON main | every non-`canada` corpus | 100 | 5.0 | warmup samples 3, warmup 3.0s, confidence 0.95, outlier `iqr`, method `bootstrap`. |
| SIMD scan | every corpus | 100 | 5.0 | warmup 3.0s, confidence 0.95, significance 0.05, noise threshold 0.02; row metadata must expose the warmup, confidence, outlier, and method fields. |

`canada` therefore has a legitimate JSON-vs-SIMD sample-policy difference:
JSON rows use 50/8, SIMD uses 100/5. That difference must not be reported as a
capture mismatch. A SIMD mismatch is instead a host/build/profile/commit,
semantic, policy, or parity-hash failure.

## 5. Native Build Policy

W0 must use the canonical command environment for both benchmarks and the gate:

```bash
RUSTFLAGS="-C target-cpu=native"
PROFILE=bench
```

The SIMD gate currently requires the exact row string
`rustflags = "-C target-cpu=native"` plus `target_cpu = "native"`. A logically
equivalent spelling such as `-Ctarget-cpu=native`, a `.cargo/config.toml`
default, or an empty `RUSTFLAGS` environment may parse or compile correctly, but
it is not a coherent W0 capture under the current validation policy. The
research schema therefore treats exact native row metadata as the acceptance
surface.

Because `RunFacts` also derives `target_cpu` from the gate process environment,
the gate invocation must use the same `RUSTFLAGS` as the bench invocation. This
prevents a report whose row metadata says native while the manifest feature mask
renders `target_cpu=default`.

## 6. SIMD Metadata Mismatch

The expected SIMD row for each corpus is diagnostic evidence only:

| Field family | Required state |
|---|---|
| Fixture identity | `input_sha256` and `input_bytes` match the fixture. |
| Semantics | `track=simd_scan`, `workload=cycles_per_byte`, `materialisation=structural_offsets`, `strictness=strict`, `output_plane=offset bitmap`, `parse_mode=simd_scan`. |
| Capture | Same CPU model, CPU arch, OS kernel, `rustflags`, `target_cpu`, `profile`, and `bbnf_commit` as the corpus JSON main row. |
| Policy | `profile=bench`, `rustflags=-C target-cpu=native`, `target_cpu=native`, `warmup_samples=3`, `warmup_time_s=3.0`, `sample_size=100`, `measurement_time_s=5.0`, `confidence_interval=0.95`, `outlier_rejection=iqr`, `statistical_method=bootstrap`. |
| Parity | The fixture-specific scalar parity hash in metadata equals the freshly recomputed scalar scan hash and the SIMD scan hash. |

The hardening failure is falsifiable by this table. If the main row commit is
HEAD and the SIMD row commit is older, the error must be
`SIMD metadata is from a different capture`; the correct recovery is a fresh
SIMD Criterion capture in the same declared capture root, not a paper override.

## 7. Falsifiability Gate

`SK-V9-open` W0 is accepted only if every item below is mechanically checked by
the same-wave gate. Any failure keeps W0 open and blocks behavior waves.

| Gate | Reject when |
|---|---|
| Run identity | `wave_id` is not `SK-V9-open`, `run_id` does not start with `sk-v9-open:criterion-fnv64-`, the hash is not 16 lowercase hex, or manifest rows have non-uniform run ids. |
| Report-wide capture | Any admitted row has a different host/build/profile/commit key from the declared capture, except for lane-specific sample policy. |
| Main JSON metadata | Required fields are absent, fixture hash/bytes mismatch, a required benchmark semantic row is missing, or a corpus cohort mixes sample policy or statistics internally. |
| SIMD metadata | SIMD row is stale, missing, semantically unsupported, not native, not 100/5, not same host/build/profile/commit as the main row, or parity hash differs from the recomputed scalar scan. |
| Native flags | Any row or gate-rendered manifest field reports empty/default/non-native target CPU, or the exact row `rustflags` is not `-C target-cpu=native`. |
| Sample policy | JSON `canada` is not 50/8, JSON non-`canada` rows are not 100/5, or any SIMD row is not 100/5. |
| Strict admission | Any deferred/view-boundary row, stale/historical/absent sidecar, lossy/permissive comparator, output-plane mismatch, or sidecar-same-run comparator is admitted as strict evidence. |
| Behavior fence | W0 changes parser, scanner, SIMD behavior, codegen behavior, throughput cells, Apache/CITM measured-row admission, direct-product proof, or production substrate facts. |

A coherent local replay shape is:

```bash
export RUSTFLAGS="-C target-cpu=native"
export PROFILE=bench
export CARGO_TARGET_DIR="$PWD/skinny/target/skv9-w0"
cargo bench -p bbnf-bench --bench json_parity
cargo bench -p bbnf-bench --bench simd_scan
cargo xtask gate-json --advisory --check-results
```

The dedicated `CARGO_TARGET_DIR` is a schema recommendation, not a behavior
change. Its purpose is to make stale mixed captures impossible without deleting
unrelated benchmark caches.

## 8. Implementation Implications For W0

R2 recommends that the eventual W0 implementation be constrained to gate/report
metadata changes:

1. Change the report wave/run label from `SK-V8-open` to `SK-V9-open`.
2. Preserve the existing Criterion fingerprint input selection, but bind it to
   the `sk-v9-open:criterion-fnv64-<hash>` prefix.
3. Add report-wide capture coherence across corpus cohorts for host/build/profile
   and commit keys.
4. Make expected sample policy explicit: JSON `canada` 50/8, JSON others 100/5,
   SIMD all 100/5.
5. Keep SIMD structural scan and masking/cycles telemetry diagnostic
   non-producers.
6. Keep strict admission blocked for deferred/view-boundary rows and for
   historical, absent, lossy, permissive, plane-mismatched, or unstructured
   sidecar evidence.

The schema is intentionally falsifiable: a single stale metadata file, default
target CPU, mixed commit, wrong sample policy, stale `SK-V8-open` run id, or
attempted behavior movement is enough to keep W0 from closing.
