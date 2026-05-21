# SK-V13 S-P1 V4 Mode-III Harness Provenance

Pass: S-P1 Profile. Cycle: V4 fold support.
Date: 2026-05-21.
Scope: durable provenance for the temporary mode-III profiler used by S-P1 V2.
Output: this file.

## Identity

The V2 mode-III profiler lived outside the repository at
`/tmp/skv13-mode3-profiler`. It linked the checked-out `runtime` and
`bbnf-bench` crates and wrote artefacts under `/tmp/skv13-p1-v2/mode3/`.
The V4 fold also preserves a checked-in, repo-relative source snapshot at
`restart/skinny/tranches/sk-v13/research/p1/support/harnesses/mode3/`.

Original temporary hashes:

| Path | SHA-256 |
|---|---|
| `/tmp/skv13-mode3-profiler/Cargo.toml` | `96701e77a4a374858641fc42d8aaddb9726f44eea1a59e3db462c4ad8cde4346` |
| `/tmp/skv13-mode3-profiler/Cargo.lock` | `d90b02f25001ba6613af1acb7e7fd3c0ef5a6164eaa522606e1fc922b0ed23a0` |
| `/tmp/skv13-mode3-profiler/src/main.rs` | `5cfe77de3e9fd5781d3d2377cfc443c208780646df559fbd280e97480138cc71` |
| `/tmp/skv13-mode3-profiler-target/release/skv13-mode3-profiler` | `2abddeb839620667abb0163acd06ec88e9d83c84384c08328dadbdaa17a5d028` |

Checked-in V4 source hashes:

| Path | SHA-256 |
|---|---|
| `support/harnesses/mode3/Cargo.toml` | `f76cc09b403a31a2c0b7ffa0b9a151f6f0f603631090d7d40211d4ed84037bd5` |
| `support/harnesses/mode3/Cargo.lock` | `d90b02f25001ba6613af1acb7e7fd3c0ef5a6164eaa522606e1fc922b0ed23a0` |
| `support/harnesses/mode3/src/main.rs` | `5cfe77de3e9fd5781d3d2377cfc443c208780646df559fbd280e97480138cc71` |

Original build command:

```bash
CARGO_TARGET_DIR=/tmp/skv13-mode3-profiler-target \
RUSTFLAGS='-C target-cpu=native' \
cargo build --release --manifest-path /tmp/skv13-mode3-profiler/Cargo.toml
```

Checked-in V4 rebuild command:

```bash
CARGO_TARGET_DIR=/tmp/skv13-mode3-profiler-target-v4 \
RUSTFLAGS='-C target-cpu=native' \
cargo build --release \
  --manifest-path restart/skinny/tranches/sk-v13/research/p1/support/harnesses/mode3/Cargo.toml
```

Verified V4 rebuild binary hash:

| Path | SHA-256 |
|---|---|
| `/tmp/skv13-mode3-profiler-target-v4/release/skv13-mode3-profiler` | `2abddeb839620667abb0163acd06ec88e9d83c84384c08328dadbdaa17a5d028` |

Capture command shape:

```bash
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1-v2/mode3/profiles/mode3__${corpus}__${mode}.json.gz \
  /tmp/skv13-mode3-profiler-target/release/skv13-mode3-profiler \
  "${corpus}" "${mode}" "${iters}"
```

## Cargo.toml

The checked-in `Cargo.toml` uses repo-relative paths and an empty
`[workspace]` table so it remains an isolated harness rather than a root
workspace member:

```toml
[package]
name = "skv13-mode3-profiler"
version = "0.1.0"
edition = "2021"

[workspace]

[dependencies]
libc = "0.2"
serde_json = "1"
runtime = { path = "../../../../../../../../../skinny/crates/runtime", features = ["bench-counters"] }
bbnf-bench = { path = "../../../../../../../../../skinny/crates/bbnf-bench" }
```

## Source Summary

The harness exposes seven probe names:

| Probe | Disposition |
|---|---|
| `host_call_dispatch_overhead` | implemented, not part of the captured five-row V2 matrix |
| `host_call_eager_decode` | captured 17/17 |
| `alternate_scalar_plan` | captured 17/17 via `serde_json::Value` |
| `cold_first_parse` | captured 17/17 with cloned input |
| `structural_scan_scalar` | captured 17/17 via `bbnf_bench::scan::structural_offsets_scalar` |
| `structural_scan_simd` | captured 17/17 via `bbnf_bench::scan::structural_offsets_simd` |
| `alternate_pext_mask_plan` | unsupported: `aarch64_no_pext` |
| `alternate_dispatch_table_plan` | unsupported: `disabled_duplicate_probe` |

`src/main.rs` was 168 lines at hash
`5cfe77de3e9fd5781d3d2377cfc443c208780646df559fbd280e97480138cc71`. Its
fixtures are located with the same corpus mapping used by the P1 direct
profile: `twitter`, `citm_catalog`, and `canada` under
`skinny/crates/test-fixtures/corpus/json/`; `update_center` under
`skinny/test_data/update-center.json`; all other rows under
`skinny/test_data/{corpus}.json`.

The harness emits one `PROBE_RESULT` line with `mbps`, `cycles`,
`instructions`, `cycles_per_byte`, and `cpi` derived from `proc_pid_rusage`
before/after counters.
