# CH2 GENERALITY / Lock 14 - SK-V8 W0 Hardening V6

Verdict: ACCEPT

Confidence: 86%

## Reviewed Surfaces

- Orchestrator challenge contract: `restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:85`, especially CH2's Lock 14 / grammar-neutral lens.
- SK-V8 generality and W0 gates: `restart/skinny/tranches/sk-v8/SPEC.md:261`-`restart/skinny/tranches/sk-v8/SPEC.md:286` and `restart/skinny/tranches/sk-v8/SPEC.md:288`-`restart/skinny/tranches/sk-v8/SPEC.md:347`.
- W0 dispatch and handoff constraints: `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:88`, `restart/skinny/tranches/sk-v8/HANDOFF.md:139`-`restart/skinny/tranches/sk-v8/HANDOFF.md:166`.
- W0 implementation surfaces changed at HEAD `6c0bc15d44142abf0b965d9daee7070b1f32dd99`: `skinny/crates/bbnf-bench/src/report.rs` and `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- Result and redress context: `skinny/RESULTS.md`, `skinny/REDRESS.md`.

## Evidence

- `git show --stat --oneline --decorate HEAD` shows current HEAD is `6c0bc15d (HEAD -> master) fix(sk-v8-wave0): fold hardening V5 row identity blockers`, changing only `skinny/crates/bbnf-bench/src/bin/gate.rs` and `skinny/crates/bbnf-bench/src/report.rs`.
- `git status --short` was clean before and after review commands.
- `rg -c '^\\| json/' skinny/RESULTS.md` reports `38`, matching the W0 main-row count required by `restart/skinny/tranches/sk-v8/SPEC.md:324`.
- `rg -n --pcre2 '^\\| [^|]+ \\| parse_only \\| (?!S |L )' skinny/RESULTS.md` returned no rows; current `parse_only` rows remain substrate-guard non-admission `S` or preserved hard failure `L`, matching `restart/skinny/tranches/sk-v8/SPEC.md:326`-`restart/skinny/tranches/sk-v8/SPEC.md:327`.
- `cargo test -p bbnf-bench` passed: 52 library tests, 8 `gate.rs` binary tests, and doc tests. This includes Lock 14 allowlist tests plus W0 row identity, sidecar, comparator, strict-admission, profile-placeholder, and exact-baseline tests.
- `cargo run -p xtask --release -- check-json` passed, preserving generated JSON output.
- `cargo run -p xtask --release -- check-conformance` passed: 21 valid fixtures accepted; 7 invalid fixtures rejected.
- `cargo run -p xtask --release -- gate-json --advisory` did not complete in this local workspace because untracked Criterion metadata is stale: `twitter SIMD metadata invalid: SIMD metadata has unsupported capture policy`. The reproducer is that command from `skinny/`; the local metadata has `rustflags = ""` and `target_cpu = "default"` at `skinny/target/criterion/simd_structural_scan/twitter_simd/metadata.toml:5`-`skinny/target/criterion/simd_structural_scan/twitter_simd/metadata.toml:7`, while the gate requires `rustflags == "-C target-cpu=native"` and `target_cpu == "native"` at `skinny/crates/bbnf-bench/src/bin/gate.rs:1427`-`skinny/crates/bbnf-bench/src/bin/gate.rs:1438`. This is local capture freshness, not a committed source or Lock 14 leak.

## CH2 Findings

- No generic-crate W0 source edit landed. HEAD changes only the benchmark/report gate slice, while the Lock 14 freeze covers generic roots such as `crates/runtime/src`, `crates/ir/src`, `crates/passes/src`, `crates/codegen/src`, `crates/grammar/src`, `crates/bbnf/src`, `crates/bbnf-simd/src`, and `crates/parse-that-regex/src` at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:397`.
- The JSON row identity is confined to the benchmark/report fixture surface. The new baseline table lives in `bbnf-bench` and binds row ids, outcomes, verdicts, and Mbps at `skinny/crates/bbnf-bench/src/report.rs:646`-`skinny/crates/bbnf-bench/src/report.rs:915`; `Report::validate_sk_v8_w0` consumes those values and rejects row count, unknown row id, outcome, verdict, or +/-1% throughput drift at `skinny/crates/bbnf-bench/src/report.rs:493`-`skinny/crates/bbnf-bench/src/report.rs:531`.
- Grammar-name checks in W0 are report/gate row-schema checks, not generic policy. `validate_sk_v8_w0` requires the W0 telemetry grammar/domain to be `json`/`json_bench` at `skinny/crates/bbnf-bench/src/report.rs:322`-`skinny/crates/bbnf-bench/src/report.rs:327`, and `parse_row_id` accepts only `json/<corpus>/<workload>/main` at `skinny/crates/bbnf-bench/src/report.rs:1317`-`skinny/crates/bbnf-bench/src/report.rs:1327`; both are inside `bbnf-bench` report validation.
- The gate remains telemetry-only. `gate-json` calls the Lock 14 validator before report construction at `skinny/crates/bbnf-bench/src/bin/gate.rs:41`-`skinny/crates/bbnf-bench/src/bin/gate.rs:44`, demotes admission-capable parse outcomes to `S` for W0 at `skinny/crates/bbnf-bench/src/bin/gate.rs:126` and `skinny/crates/bbnf-bench/src/bin/gate.rs:364`-`skinny/crates/bbnf-bench/src/bin/gate.rs:372`, validates schema plus W0 telemetry at `skinny/crates/bbnf-bench/src/bin/gate.rs:319`-`skinny/crates/bbnf-bench/src/bin/gate.rs:327`, and writes `RESULTS.md` only under explicit `--update-results`/`--write-results` at `skinny/crates/bbnf-bench/src/bin/gate.rs:329`-`skinny/crates/bbnf-bench/src/bin/gate.rs:339`.
- Sidecar evidence stays non-admitting. Populated sidecars must be historical and absent sidecars must carry `absent:<reason>` at `skinny/crates/bbnf-bench/src/report.rs:1123`-`skinny/crates/bbnf-bench/src/report.rs:1152`; `sidecar-same-run` is rejected without a structured manifest at `skinny/crates/bbnf-bench/src/report.rs:1227`-`skinny/crates/bbnf-bench/src/report.rs:1232`.
- No new directive, BIR variant, or substrate surface is introduced by this HEAD. The diff does not touch grammar/IR/pass/codegen/runtime/SIMD source, and the Lock 14 validator freezes those roots against dirty state and parent diff at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:405`. It also pins the `BackendShape` surface to five variants and forbids `UnionTape` at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:491`.
- The committed W0 manifest is still report telemetry, not production behavior. `skinny/RESULTS.md:44` starts the `SK-V8 W0 Telemetry Manifest`, `skinny/RESULTS.md:48` shows the first row using `gate_only` and historical/absent sidecar evidence, and `skinny/RESULTS.md:141` states the manifest is consumed by `gate-json` with native Rust comparators same-run and C++ sidecars historical or absent.

## Material Blockers

None for CH2 / Lock 14. I found no W0 JSON policy in generic crates, no grammar-name leak outside the benchmark/report fixture surface changed by W0, no new directive/BIR/substrate surface, and no production behavior edit.

## Residual Risks

- I could not complete a live `gate-json --advisory` run from the current local `target/criterion` capture because the local metadata was not captured with the native W0 policy. This does not change the source verdict, but a CH1/CH6 close should refresh or restore matching Criterion artifacts before using the live gate as proof.
- W0 intentionally hard-codes JSON row ids and comparator names inside `bbnf-bench`. That remains acceptable only while those strings stay confined to benchmark/report fixtures and are not migrated into generic crates.
- The non-JSON proof is unchanged-output based for this HEAD because no generic code changed. If a later fold edits `grammar`, `ir`, `passes`, `codegen`, `runtime`, `bbnf-simd`, `bbnf`, or `parse-that-regex`, it needs fresh CSS L4 / Sheets / BBNF-self proof under `restart/skinny/tranches/sk-v8/SPEC.md:279`-`restart/skinny/tranches/sk-v8/SPEC.md:282`.
