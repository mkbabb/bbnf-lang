# SK-V14 W0F: Verification Map

Date: 2026-05-24.

Scope: W0 focused verification commands, cargo-target discipline, and owner-path risk.

Output: this file.

## §1 — Findings (concrete file:line cited)

1. W0's authorized edit surface is narrow. SPEC §3 names only `skinny/crates/bbnf-bench/`, `skinny/xtask/src/`, `skinny/RESULTS.md`, `skinny/ROLLING-SOTA-DELTA.md`, `restart/skinny/tranches/sk-v14/research/`, and `skinny/REDRESS.md` only if W0 rejects (`restart/skinny/tranches/sk-v14/SPEC.md:317-324`). The same section says W0 lands no parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output change (`restart/skinny/tranches/sk-v14/SPEC.md:352-365`).

2. W0 is a schema and gate lock, not an admit wave. It must capture `SK-V14-open`, add `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`, and Lock 1 triad telemetry, then make `xtask gate-json` reject missing fields, stale sidecar strict claims, unsupported outcomes, strict-admission failures, comparator/output-plane mismatches, and fake generated headers (`restart/skinny/tranches/sk-v14/SPEC.md:341-350`).

3. The full W0 row floor is 75 rows: 51 JSON cells plus 24 CSS L4 features. Section 0.4 requires every current row to carry the SK-V14 additions and `SK-V14-open` delta (`restart/skinny/tranches/sk-v14/SPEC.md:118-160`). P3-D makes the gate falsifiable: `cargo xtask gate-json --check-results` must assert all 31 schema slots, reject asymmetric comparator work, reject failed per-iter equality, reject AUDIT-FALSIFIED admits without fresh evidence, reject Track 1/Track 2 hidden coupling, and enforce the W0 baseline delta (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:108-117`).

4. The current skinny xtask command surface is centralized in `skinny/xtask/src/main.rs`. The usage list includes `bench-json` and `gate-json` (`skinny/xtask/src/main.rs:8`); `bench-json` shells to `cargo bench -p bbnf-bench` and on full runs calls `gate_json(... --update-results)` (`skinny/xtask/src/main.rs:209-234`); `gate-json` shells to `cargo run -p bbnf-bench --bin gate -- <args>` (`skinny/xtask/src/main.rs:242-257`).

5. Existing `gate-json` passthrough validation recognizes SK-V12/SK-V13 report flags, but not SK-V14 flags yet (`skinny/xtask/src/main.rs:265-302`). P3-D says W0 must extend that allowlist with at least `--skv14-comparator-rebind-report`, `--skv14-per-iter-equality-report`, `--skv14-audit-overlay-report`, and `--skv14-track2-entry-point-report`, then ingest them in `bbnf-bench/src/bin/gate.rs` (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:98-106`).

6. Existing `gate-json --check-results` already validates the rolling-delta snapshot shape: 51 JSON rows and 24 CSS rows, with required `schema_version`, `run_id`, `g_omega_status`, and consumer gate strings (`skinny/xtask/src/main.rs:355-368`, `skinny/xtask/src/main.rs:436-467`). SK-V14 should extend this snapshot lock, not bypass it.

7. The executable bench/gate data source is Criterion output under `CRITERION_HOME` or `CARGO_TARGET_DIR/criterion`. `apply_bench_output_env` propagates `CARGO_TARGET_DIR` and derives `CRITERION_HOME` when it is set (`skinny/xtask/src/main.rs:304-313`); the gate reads `criterion_root()` and fixture groups before building the report (`skinny/crates/bbnf-bench/src/bin/gate.rs:383-390`).

8. Current W0-style metadata validation is strict about capture policy. `validate_w0_capture_metadata` rejects rows missing required metadata, wrong fixture hash/bytes, non-bench profile, missing `RUSTFLAGS="-C target-cpu=native"`, non-native target CPU, mixed captures, or missing required bench lanes (`skinny/crates/bbnf-bench/src/bin/gate.rs:3819-3914`). SIMD metadata is likewise required to match the same capture and strict scan policy (`skinny/crates/bbnf-bench/src/bin/gate.rs:4239-4298`).

9. The bench crate has exactly three Criterion bench targets today: `json_parity`, `simd_scan`, and `nonjson_css_l4` (`skinny/crates/bbnf-bench/Cargo.toml:31-41`). `json_parity` measures many JSON lanes with 5s default measurement time, 8s for Canada, and sample sizes from 30 to 200 (`skinny/crates/bbnf-bench/benches/json_parity.rs:31-41`, `skinny/crates/bbnf-bench/benches/json_parity.rs:517-528`); `simd_scan` uses 5s/100 samples (`skinny/crates/bbnf-bench/benches/simd_scan.rs:84-95`); `nonjson_css_l4` uses 5s/30 samples (`skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs:307-318`). The full bench refresh is therefore long/heavy.

10. Cargo profile selection must be explicit. The skinny workspace defines `bench` as release-derived, fat-LTO, `codegen-units=1`, `panic=abort`, `debug=true`, and packed split debuginfo (`skinny/Cargo.toml:87-95`), while `ax-iter` is the bounded iteration profile (`skinny/Cargo.toml:97-100`). The root cargo config also documents that iteration aliases carry `--profile ax-iter`, bench aliases are single cargo invocations, and parallel cargo commands against one target silently serialize (`.cargo/config.toml:12-25`, `.cargo/config.toml:139-151`, `.cargo/config.toml:176-207`).

11. Memory imposes three command-discipline constraints. Long cargo/test/build/bench/expand/samply commands must tee full output to a file on the first invocation and inspect that file thereafter (`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_test_output_to_file.md:7-30`). At most one cargo invocation may be in flight per `CARGO_TARGET_DIR` (`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_single_cargo_per_target.md:7-13`). Iteration-loop cargo check/test commands must carry `--profile ax-iter`; `cargo bench` is heavy close-proof surface, not an iteration-loop command (`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_iter_profile_always.md:7-15`).

12. Bench reruns are constrained. SPEC gives W0 "report/gate tests, malformed sidecar-evidence rejection, full-table schema validation incl. 4 SK-V14 columns" and allows only one gate refresh plus one confirm rerun if variance invalidates telemetry (`restart/skinny/tranches/sk-v14/SPEC.md:275-280`). Memory also says benchmarks must be a single invocation, not separate sequential bench commands (`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_bench_single_run.md:7-11`), and only cold per-parse numbers are valid (`/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_no_warm_benches.md:7-11`).

## §2 — Recommendations (named falsifiability gates)

Run W0 commands from the skinny workspace, with one target directory reserved for the whole W0 verification pass:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/bbnf-skv14-w0-target
export CRITERION_HOME=/tmp/bbnf-skv14-w0-target/criterion
export RUSTFLAGS="-C target-cpu=native"
mkdir -p /tmp/bbnf-skv14-w0-logs
```

Do not run another cargo command against `/tmp/bbnf-skv14-w0-target` while any command below is still active. If another agent needs cargo concurrently, it must use a distinct `CARGO_TARGET_DIR` and accept the rebuild cost.

1. **G-W0-UNIT-SCHEMA** — medium/heavy, output-to-file.

   Proves the xtask passthrough, gate/report/metadata structs, Lock 14 baseline tests, and newly-added SK-V14 negative fixtures compile and pass under the iteration profile.

   ```bash
   cargo test --profile ax-iter -p xtask -p bbnf-bench 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-unit-schema.log
   ```

   Required redress tests under this gate:
   - missing `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, and `track2_entry_point` reject with row id + field name;
   - malformed sidecar evidence rejects;
   - `sidecar-same-run` without a structured manifest rejects;
   - added fake `// @generated by skinny bbnf-codegen` header outside a recognized emission roster rejects.

2. **G-W0-GENERATED-STALE-CHECK** — medium, output-to-file.

   Proves W0 did not require generated JSON/runtime rewrites.

   ```bash
   cargo xtask check-json 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-check-json.log
   cargo xtask check-real-typed 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-check-real-typed.log
   ```

3. **G-W0-FULL-BENCH-REFRESH** — long/heavy, output-to-file, single invocation only.

   Captures `SK-V14-open` and refreshes the gate-consumed Criterion tree in one run. Do not split by bench target or by corpus filter.

   ```bash
   cargo xtask bench-json 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-bench-json-refresh.log
   ```

4. **G-W0-FULL-SCHEMA-GATE** — medium/heavy, output-to-file.

   Read-only confirmation after the refresh. It must parse all 75 main rows, consume the four SK-V14 telemetry additions, enforce the rolling snapshot, and fail if `RESULTS.md` is stale.

   ```bash
   cargo xtask gate-json --check-results 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-gate-json-check-results.log
   ```

5. **G-W0-COSTFACTS-SMOKE** — medium, output-to-file.

   Keeps the existing CostFacts path from regressing while W0 extends the schema carrier. This is not a performance rerun.

   ```bash
   cargo xtask gate-json --with-cost-facts --check-results 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-gate-json-costfacts.log
   ```

6. **G-W0-OWNER-PATH-SLICE** — lightweight, no cargo.

   Run from the repo root after redress, before staging. The first command should list only W0 owner paths; the second should be empty unless the W0 rejection path intentionally touched `skinny/REDRESS.md`.

   ```bash
   cd /Users/mkbabb/Programming/bbnf-lang
   git diff --name-only -- skinny/crates/bbnf-bench skinny/xtask/src skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md restart/skinny/tranches/sk-v14/research
   git diff --name-only -- skinny/REDRESS.md
   ```

7. **G-W0-BEHAVIOR-FREEZE** — lightweight, no cargo.

   Expected output is empty. Any hit means W0 leaked outside the Section 3 behavior-free owner contract.

   ```bash
   cd /Users/mkbabb/Programming/bbnf-lang
   git diff --name-only -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/ir skinny/crates/passes crates/core/src/runtime
   git diff -U0 -- skinny/crates/runtime/src/grammars skinny/crates/codegen/src | rg '^\+.*@generated by skinny bbnf-codegen'
   ```

8. **G-W0-RERUN-CEILING** — process gate.

   If `G-W0-FULL-BENCH-REFRESH` produces variance that invalidates telemetry, one confirm refresh is allowed. Do not run corpus-by-corpus benches or separate `cargo bench --bench ...` commands. Inspect `/tmp/bbnf-skv14-w0-logs/*.log` with `rg`, `tail`, or `sed`; do not rerun cargo to get a different output slice.

## §3 — Risks (REDRESS entries to pre-block)

1. **Startup-only equality route: REDRESS 28, 33.** W0 must pre-block any equality proof outside the timing region. P3-D names these entries and binds the fix to `per_iter_equality` inside the measured iteration (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:127-130`).

2. **Comparator-plane mismatch: REDRESS 50-55 plus audit-falsified direct/typed/parse rows.** W0 must reject `sonic_rs::from_slice::<Value>` or any eager DOM comparator reused as strict evidence for direct, typed, or parse_only planes. SPEC marks P-2 as the misbinding pattern (`restart/skinny/tranches/sk-v14/SPEC.md:1077`); the audit ledger binds JSON direct REDRESS 131-135 + 141, typed REDRESS 143 + 145-153 + 160, and parse_only REDRESS 154-158 to plane-correct re-admit framing (`restart/skinny/tranches/sk-v14/SPEC.md:1132-1149`).

3. **Stale sidecar as strict anchor: REDRESS 60-72.** W0 must reject stale sidecar strict claims and any `sidecar-same-run` claim without a structured manifest. SPEC requires missing sidecars to render `absent:<reason>` and says W0 admits no sidecar same-run manifest (`restart/skinny/tranches/sk-v14/SPEC.md:347-360`); P3-D names REDRESS 60-72 as the stale-sidecar route to kill (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:131`).

4. **Producer-only telemetry: REDRESS 80, 82-84, 88, 89.** W0 must not add columns that are rendered but not consumed. P3-D states every emitted field must be consumed by `cargo xtask gate-json` in the same wave and names these REDRESS entries as the pre-block (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:12`, `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:132`).

5. **Orphan kernel / no same-wave consumer: REDRESS 96-98.** W0 should reject any row whose evidence is a primitive/checkasm/scaffold without a same-wave consumer class. P3-D binds `same_wave_consumer_class` as required and names REDRESS 96-98 (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:58`, `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:133`).

6. **Sidecar substrate / aux projection: REDRESS 119/120.** W0 should pre-block sidecar event vectors, aux density tables, parser-owned projection state, or hidden second substrates. P3-D binds `structural_projection_status` and `substrate_cardinality` as required gate fields and names REDRESS 119/120 (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:55-57`, `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:134`).

7. **Track 1 equals Track 2: REDRESS 126 and P-7.** W0 must require `track2_entry_point` and reject hidden coupling. SPEC says P-7 is enforced by the `track2_entry_point` column (`restart/skinny/tranches/sk-v14/SPEC.md:1082`); P3-D gives the common-ancestor rejection test and names REDRESS 126 (`restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:83-87`, `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:135`).

8. **Gate relabel as admit: P-4 / REDRESS 154-158.** W0 must pre-block any row-close claim where source diffs touch only gate/report/Lock14 code. SPEC names P-4 as the gate-relabel pattern (`restart/skinny/tranches/sk-v14/SPEC.md:1079`); P3-E says REDRESS 154-158 are AS-WRITTEN audit-falsified and can only reopen later through a distinct parse_only code path plus Skipper-class comparator (`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md:812-823`).

9. **Fake generated header / hand-written generated output: P-1.** W0's Lock-14 companion lint must reject new `@generated` headers unless the path appears in a recognized regen emission roster. SPEC names P-1 as a recurrence vector (`restart/skinny/tranches/sk-v14/SPEC.md:1076`), and P3-E binds the lint glob and recognized-emission condition (`restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md:852-857`).

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:73-82`, `:118-166`, `:275-280`, `:315-377`, `:1071-1098`, `:1116-1167`.
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md:1-135`.
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md:800-857`.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:198-206`.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-100`.
- `skinny/xtask/src/main.rs:8`, `:209-313`, `:355-585`, `:817-875`, `:984-1120`.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:383-745`, `:3819-4305`.
- `skinny/crates/bbnf-bench/src/report.rs:1-160`.
- `skinny/crates/bbnf-bench/src/metadata.rs:1-220`.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:520-565`, `:990-1075`, `:1290-1375`, `:1385-1518`.
- `skinny/crates/bbnf-bench/Cargo.toml:31-41`.
- `skinny/Cargo.toml:72-100`.
- `.cargo/config.toml:12-25`, `:139-151`, `:176-207`.
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md:8-12`, `:80-81`.
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_test_output_to_file.md:7-30`.
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_single_cargo_per_target.md:7-13`.
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_iter_profile_always.md:7-15`.
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_bench_single_run.md:7-11`.
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_no_warm_benches.md:7-11`.
