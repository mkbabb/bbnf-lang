# SK-V8 W0 Hardening V11 CH1 - Correctness

Date: 2026-05-18.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

Verdict: ACCEPT.

Confidence: 95%.

V11 closes the CH1-correctness parts of the V10 CH4 rejection. The V11 source
footprint claim resolves against the V9-fold baseline, empty host/feature
metadata is now rejected, W0 manifest semantics are executable, strict admission
still rejects non-GO and hard-failure outcomes, and the focused evidence commands
resolve from the `skinny/` Cargo workspace. If all V11 lenses ACCEPT, this cycle
can count only as the first qualifying ACCEPT after the V10 reset; convergence
still needs a second consecutive qualifying cycle under the orchestrator rule
(`restart/prompts/ORCHESTRATOR.md:120`,
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:64`).

## Scope Reviewed

- CH1 authority and convergence governance:
  `restart/prompts/ORCHESTRATOR.md:83`,
  `restart/prompts/ORCHESTRATOR.md:112`,
  `restart/prompts/ORCHESTRATOR.md:116`,
  `restart/prompts/ORCHESTRATOR.md:120`.
- SK-V8 W0 strictness, outcomes, required telemetry, W0 task/exit gates, and
  same-wave consumer:
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:97`,
  `restart/skinny/tranches/sk-v8/SPEC.md:100`,
  `restart/skinny/tranches/sk-v8/SPEC.md:110`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:312`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`.
- W0 dispatch/handoff constraints:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:87`.
- V10 rejection and required V11 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:35`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:45`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:53`.
- Current code around W0 validation:
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/gate.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, and current
  `skinny/RESULTS.md`.

## Evidence

1. The cost-footprint claim resolves. `git diff --numstat 00c3485a..61d5cc3b
   -- skinny/crates/bbnf-bench/src/report.rs` returns `118 13`, matching the
   commit-body claim that the live post-V9 report validator footprint is under
   the `<=120` post-V6 fold insertion cap. The target commit itself is a
   compaction from V10: `git show --numstat 61d5cc3b -- .../report.rs` returns
   `58 109`.

2. W0 manifest semantics are executable, not prose-only. Row validation calls
   `validate_w0_manifest_semantics()` after exact run-id, sample, profile, and
   hot-leaf checks (`skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:342`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`). The manifest helper enforces
   exact pre-W1 CostFacts sentinels, `redress_entry=none`, Track 2 independence,
   exact benchmark build flags, structured non-empty host/feature metadata, and
   workload-specific substrate tuples
   (`skinny/crates/bbnf-bench/src/report.rs:1007`,
   `skinny/crates/bbnf-bench/src/report.rs:1027`,
   `skinny/crates/bbnf-bench/src/report.rs:1039`,
   `skinny/crates/bbnf-bench/src/report.rs:1053`,
   `skinny/crates/bbnf-bench/src/report.rs:1063`).

3. The empty metadata blocker is closed. `has_nonempty()` rejects empty
   semicolon fields by stripping the key prefix and requiring a non-empty tail
   (`skinny/crates/bbnf-bench/src/report.rs:1021`). Host metadata must have a
   non-empty host triple plus non-empty `arch` and `cpu`
   (`skinny/crates/bbnf-bench/src/report.rs:1039`,
   `skinny/crates/bbnf-bench/src/report.rs:1043`); feature metadata must have
   non-empty `arch`, `os`, `simd`, and exact `target_cpu=native`
   (`skinny/crates/bbnf-bench/src/report.rs:1053`). The focused full-baseline
   negative mutates both empty host and empty feature payloads and requires W0
   validation failure (`skinny/crates/bbnf-bench/src/report.rs:2053`,
   `skinny/crates/bbnf-bench/src/report.rs:2065`,
   `skinny/crates/bbnf-bench/src/report.rs:2068`).

4. Strict admission still cannot use non-GO or hard-failure outcomes. The strict
   helper parses the outcome and rejects anything whose verdict is not
   `Verdict::Go` before row strictness, measured validation path, plane, or
   comparator freshness can matter (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:139`). `G`, `I`, `K`, `L`, `M`,
   `N-direct`, and `S` are `NoGo`, while `J` is `Invalid`
   (`skinny/crates/bbnf-bench/src/gate.rs:72`,
   `skinny/crates/bbnf-bench/src/gate.rs:82`). The strict test covers `D`, `E`,
   `F-positive`, `F-noise`, `G`, `I`, `J`, `K`, `L`, `M`, `N-direct`, and `S` as
   rejects (`skinny/crates/bbnf-bench/src/gate.rs:459`,
   `skinny/crates/bbnf-bench/src/gate.rs:478`). W0 report validation also keeps
   the current 38 rows deferred/view-boundary
   (`skinny/crates/bbnf-bench/src/report.rs:1096`,
   `skinny/crates/bbnf-bench/src/report.rs:1103`,
   `skinny/crates/bbnf-bench/src/report.rs:1109`).

5. Strict-vs-strict comparator discipline still holds. Strict admission requires
   row strictness, comparator strictness, measured-row UTF-8, complete escape
   validation, matching row/comparator planes, measured validation path, and
   same-run native freshness with `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:157`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). Report validation separately
   binds native comparator ids to workload-specific bench names and planes
   (`skinny/crates/bbnf-bench/src/report.rs:1313`,
   `skinny/crates/bbnf-bench/src/report.rs:1324`,
   `skinny/crates/bbnf-bench/src/report.rs:1337`,
   `skinny/crates/bbnf-bench/src/report.rs:1367`) and rejects
   `sidecar-same-run` without a structured manifest
   (`skinny/crates/bbnf-bench/src/report.rs:1263`,
   `skinny/crates/bbnf-bench/src/report.rs:1287`). Focused tests cover plane
   mismatch, stale sidecar, and sidecar-same-run rejection
   (`skinny/crates/bbnf-bench/src/gate.rs:501`,
   `skinny/crates/bbnf-bench/src/gate.rs:512`).

6. Run id, profile artifact, hot leaf, sample cost, and required metadata are
   measurable on the generated gate path. The gate constructs `sample_cost` only
   from finite positive Track 1 nanoseconds and non-zero bytes, otherwise `n/a`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:463`), and report validation rejects
   `sample_count=0`, `sample_cost` containing `n/a`, or a missing
   `ns_per_byte=` token (`skinny/crates/bbnf-bench/src/report.rs:342`,
   `skinny/crates/bbnf-bench/src/report.rs:345`). Profile artifacts must match
   the expected row/workload Criterion path, and hot leaf must bind exactly to the
   same profile artifact and row id
   (`skinny/crates/bbnf-bench/src/report.rs:968`,
   `skinny/crates/bbnf-bench/src/report.rs:981`). `gate-json` validates the
   rendered report before writing or comparing `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`).

7. Current rendered evidence matches the closed boundary. `skinny/RESULTS.md`
   keeps the canada hard failure as `L / NO-GO`, `Strictness=deferred`,
   `parse_utf8=view-boundary` (`skinny/RESULTS.md:10`). The manifest renders run
   id, validation path, profile artifact, sample cost, sample count, build flags,
   host triple, feature mask, CostFacts, substrate tuple, Track 2 status, and
   comparator evidence (`skinny/RESULTS.md:46`, `skinny/RESULTS.md:48`,
   `skinny/RESULTS.md:53`). The report note states native Rust comparators are
   same-run and C++ sidecars are historical or absent, never W0 strict anchors
   (`skinny/RESULTS.md:141`).

## Commands Run

- `git rev-parse HEAD` -> `61d5cc3b4312883e026060174e876a0c18b34703`.
- `git diff --numstat 00c3485a..61d5cc3b --
  skinny/crates/bbnf-bench/src/report.rs` -> `118 13`.
- `git diff --check 00c3485a..61d5cc3b --
  skinny/crates/bbnf-bench/src/report.rs` -> clean.
- `git diff --name-only 0bd16f6d..61d5cc3b -- <frozen behavior paths>` ->
  empty output for runtime, codegen, SIMD, generated/product, grammar, fixture,
  scan, parity, and materialization surfaces.
- From `skinny/`: `cargo test -p bbnf-bench w0_ -- --nocapture` -> passed 12
  report W0 tests and 8 gate-bin W0 tests.
- From `skinny/`: `cargo test -p bbnf-bench strict -- --nocapture` -> passed 5
  focused strict tests.
- From `skinny/`: `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` ->
  passed 1 focused sidecar-same-run test.
- From `skinny/`: `cargo test -p bbnf-bench` -> passed 52 library tests, 8
  gate-bin tests, and doc-tests.
- From `skinny/`: `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C
  target-cpu=native' cargo xtask gate-json --advisory --check-results` -> exit
  0; output retained `Overall outcome N-direct / NoGo.`
- From `skinny/`: `cargo xtask check-json` -> exit 0.
- From `skinny/`: `cargo xtask check-real-typed` -> exit 0.
- From `skinny/`: `cargo xtask check-conformance` -> exit 0 with `21 valid
  fixtures accepted; 7 invalid fixtures rejected`.

## Blockers

None for CH1.

## Required Fold If Rejecting

None. This CH1 disposition is ACCEPT.

## Residual Risk

- `sample_cost` validation is shape-based in `report.rs`: it rejects `n/a` and
  requires an `ns_per_byte=` token, but it does not parse the token as a finite
  numeric value (`skinny/crates/bbnf-bench/src/report.rs:345`). This is not a V11
  blocker because the generated gate path computes the field from finite timing
  data (`skinny/crates/bbnf-bench/src/bin/gate.rs:463`), but a later W1/W6
  manifest parser should make this a numeric parse if the manifest becomes an
  external input.
- Row-level `validate_w0_outcome()` still allows internal W0 spellings beyond
  the rendered SPEC set before full-report baseline validation
  (`skinny/crates/bbnf-bench/src/report.rs:955`,
  `skinny/crates/bbnf-bench/src/report.rs:959`). Full W0 report validation still
  binds exact row ids, outcomes, verdicts, and throughput to
  `SK_V8_OPEN_BASELINE` (`skinny/crates/bbnf-bench/src/report.rs:494`,
  `skinny/crates/bbnf-bench/src/report.rs:512`), so this is not a current
  admission route.
- Cargo evidence commands resolve from the `skinny/` workspace, which owns
  `bbnf-bench`. The repository root does not expose that package id directly.
