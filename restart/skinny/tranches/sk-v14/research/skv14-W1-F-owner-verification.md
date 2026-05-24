# SK-V14 W1F: Owner Paths And Verification

Date: 2026-05-24.
Scope: W1 owner-path, no-behavior-change, and verification-risk inventory.
Output: this file.

## §1 — Findings

- W1 owner paths in `restart/skinny/tranches/sk-v14/SPEC.md:381-392` are `skinny/crates/bbnf-bench/benches/json_parity.rs`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, `skinny/crates/bbnf-bench/src/`, `skinny/xtask/src/main.rs`, `skinny/RESULTS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, and `skinny/REDRESS.md`.
- `SPEC.md:384` also names `skinny/xtask/src/gate.rs`, but that file is not present at HEAD; the active xtask validator is `skinny/xtask/src/main.rs`.
- The W1 SPEC path `skinny/ROLLING-SOTA-DELTA.md` is stale; the file at HEAD is `restart/skinny/ROLLING-SOTA-DELTA.md`.
- W1 forbidden paths are parser/runtime behavior, codegen behavior, SIMD/asm/product behavior outside the benchmark harness, fixture/corpus content, and generated parser output.
- Current unrelated dirty files are SK-V12/SK-V13 JSON research artefacts plus an untracked handoff prompt; W1 must not stage them.
- The W0 Lock 14 generated-header lint is in `skinny/crates/bbnf-bench/src/lock14_baseline.rs` and should continue to run in `cargo test --profile ax-iter -p xtask -p bbnf-bench`.

## §2 — Recommendations

- Treat `restart/skinny/ROLLING-SOTA-DELTA.md` and `skinny/xtask/src/main.rs` as the corrected W1 owner surfaces.
- Before redress, record a plan that W1 changes benchmark/gate/report/ledger files only and does not touch `crates/core/src/runtime`, generated grammar output, or codegen.
- Verification commands:
  - `cargo test --profile ax-iter -p xtask -p bbnf-bench`
  - `cargo xtask gate-json --check-results --skv14-existing-results-capture`
  - `cargo xtask gate-json --with-cost-facts --check-results`
  - `rg "sonic_rs_anchor|from_slice::<sonic_rs::Value>" skinny/crates/bbnf-bench`
  - `grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md`
  - `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  - `git diff --name-only -- crates/core/src/runtime skinny/crates/codegen skinny/crates/runtime`

## §3 — Risks

- W1 requires ledger and benchmark-harness edits, so a source diff will exist. The no-behavior proof should mean no parser/runtime/codegen/generated output changes, not no benchmark-harness changes.
- A raw `git add -u` would stage unrelated dirty research JSON artefacts. Stage explicit W1 paths only.
- `cargo xtask gate-json --check-results` without the W0 existing-capture flag still traverses Criterion metadata and can fail on stale local metadata; W1 checks should use the W0 capture path unless W1 intentionally generates a fresh cold capture.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:381-467`
- `skinny/xtask/src/main.rs:360-573`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `restart/skinny/ROLLING-SOTA-DELTA.md:14-93`
- `git status --short` at W1 research dispatch
