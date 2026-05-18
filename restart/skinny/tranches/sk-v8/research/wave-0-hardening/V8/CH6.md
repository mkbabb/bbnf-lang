# SK-V8 W0 Hardening V8 CH6

Date: 2026-05-18.

Target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

Current `HEAD` at review time was `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd`,
which adds only V7 hardening documents. I reviewed W0 implementation state at
the target commit and treated the V7 files as challenge history, not
implementation changes.

## Verdict

ACCEPT.

Confidence: 95%.

V8 can count as the second consecutive qualifying ACCEPT for W0 only if the
V8 consolidated result is also >=95% ACCEPT with zero open critical defects and
no unresolved REVISE. CH6 alone does not dispatch W1-W6. V7 is recorded as the
first qualifying ACCEPT cycle after V6 reset the counter
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`,
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:17`),
and the orchestrator requires two consecutive >=95% ACCEPT cycles
(`restart/prompts/ORCHESTRATOR.md:118`,
`restart/prompts/ORCHESTRATOR.md:120`). W1-W6 remain blocked until V8
consolidation and closure commits land, and each later wave still needs its own
entry gates, plan/challenge requirements, and dispatch authority
(`restart/skinny/tranches/sk-v8/HANDOFF.md:238`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:103`).

## Reviewed Surfaces

- W0 telemetry contract and strict-admission boundary:
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:77`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`.
- W0 exit gate, behavior freeze, sidecar refusal, same-wave consumer, and
  W1-W6 block:
  `restart/skinny/tranches/sk-v8/SPEC.md:326`,
  `restart/skinny/tranches/sk-v8/SPEC.md:348`,
  `restart/skinny/tranches/sk-v8/SPEC.md:355`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`,
  `restart/skinny/tranches/sk-v8/SPEC.md:372`.
- W0 dispatch and downstream locking:
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:87`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`.
- W0 report/gate implementation:
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:37`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:50`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`.
- Current W0 evidence artifact:
  `skinny/RESULTS.md:44`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:141`.
- V7 challenge history:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:10`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:47`.

## Commands And Evidence

- `git diff --name-status f452e837..HEAD --`: only V7 hardening archive files
  were added. `git diff --exit-code f452e837..HEAD -- skinny/... restart/skinny/tranches/sk-v8/{SPEC.md,HANDOFF.md,DISPATCH-PROMPT.md}`
  returned no implementation or live-packet diff.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-v8-review-target cargo test -p bbnf-bench w0_ -- --nocapture`:
  PASS; 12 report W0 tests and 8 gate-bin W0 tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-v8-review-target cargo test -p bbnf-bench strict_admission -- --nocapture`:
  PASS; strict-admission rejection tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-v8-review-target cargo test -p bbnf-bench sidecar -- --nocapture`:
  PASS; sidecar and sidecar-same-run rejection coverage passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-v8-review-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`:
  expected fail-closed negative result, `twitter metadata invalid: missing
  Criterion metadata rows`, because the temp target had no Criterion evidence.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`:
  PASS against the captured W0 Criterion root and committed `RESULTS.md`.
- Copied-root run-id drift probe: copied `/tmp/skv8-w0-target/criterion` to
  `/tmp/skv8-v8-runid.dzu7of/criterion`, appended one newline to
  `json_twitter/track1_generated/new/estimates.json`, then ran
  `CARGO_TARGET_DIR=/tmp/skv8-v8-runid.dzu7of RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`.
  Expected nonzero result occurred: `Schema/W0 validation failure:
  json/twitter/parse_only/main run_id moved from SK-V8-open baseline
  sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
  sk-v8-open:criterion-fnv64-a5417170e7ed57aa`.
- `git show f452e837:skinny/RESULTS.md | awk ...`: `main_rows=38`,
  `manifest_rows=38`.
- Frozen behavior-surface diff from `0bd16f6d..f452e837` over grammar,
  runtime/tape, SIMD, codegen, generated typed/product helpers, Track 2,
  parity, scan, materialization, and SIMD hook paths returned no paths.
- W0 accounting check:
  `git diff --shortstat 0bd16f6d..6c0bc15d -- <seven W0 files>` returned
  `7 files changed, 3532 insertions(+), 253 deletions(-)`;
  `git diff --shortstat 6c0bc15d..f452e837 -- <W0 files plus packet docs>`
  returned `4 files changed, 67 insertions(+), 7 deletions(-)`.
- `git diff --check f452e837..HEAD --`: PASS.

## Findings

1. No blocker: closure is live evidence, not paper close. `gate-json` reads the
   Criterion root selected by `CARGO_TARGET_DIR` or workspace `target`, probes
   run facts from that root, builds the report, validates schema and W0, then
   compares the rendered output to committed `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:37`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:50`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:329`). Missing Criterion metadata
   failed closed in the temp-target negative run.

2. No blocker: every material telemetry field is consumed by gate/report
   validation. The row validator checks required telemetry text fields, then
   semantically gates grammar/domain, row identity, outcome, wave/delta, exact
   run id, sample count/cost, profile artifact, hot leaf, CostFacts alternatives,
   same-wave consumer class, comparator evidence, and admission boundary
   (`skinny/crates/bbnf-bench/src/report.rs:277`,
   `skinny/crates/bbnf-bench/src/report.rs:322`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:330`,
   `skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:342`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:361`,
   `skinny/crates/bbnf-bench/src/report.rs:375`). The report-level validator
   also enforces the exact 38-row baseline, no duplicates, no unknown/missing
   rows, exact outcome/verdict, and +/-1.0% Track 1/Track 2 drift
   (`skinny/crates/bbnf-bench/src/report.rs:499`,
   `skinny/crates/bbnf-bench/src/report.rs:517`,
   `skinny/crates/bbnf-bench/src/report.rs:529`,
   `skinny/crates/bbnf-bench/src/report.rs:532`).

3. No blocker: run-id drift fails closed. The target binds W0 validation to
   `SK_V8_OPEN_RUN_ID`
   (`skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:660`), and the exact-baseline test
   rejects both single-row and uniform fake run IDs
   (`skinny/crates/bbnf-bench/src/report.rs:1976`,
   `skinny/crates/bbnf-bench/src/report.rs:1980`). The copied-root mutation
   confirmed the live `gate-json` path recomputes a different fingerprint and
   rejects before admission.

4. No blocker: sidecar, stale, permissive, lossy, and view-boundary evidence
   cannot strict-admit. Strict admission requires strict row evidence, measured
   UTF-8, complete escape validation, matching output plane, measured-row
   validation, same-run native comparator freshness, and `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:157`,
   `skinny/crates/bbnf-bench/src/gate.rs:160`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). W0 sidecar comparators reject
   `sidecar-same-run` without a structured manifest and require exact
   source/freshness shapes
   (`skinny/crates/bbnf-bench/src/report.rs:1211`,
   `skinny/crates/bbnf-bench/src/report.rs:1235`,
   `skinny/crates/bbnf-bench/src/report.rs:1241`). Native comparators are
   workload/plane-specific and must use `same-run-native` plus
   `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/report.rs:1261`,
   `skinny/crates/bbnf-bench/src/report.rs:1272`,
   `skinny/crates/bbnf-bench/src/report.rs:1297`,
   `skinny/crates/bbnf-bench/src/report.rs:1303`).

5. No blocker: W1-W6 remain blocked by the live packet. The dispatch prompt says
   not to dispatch W1-W6 from the prompt alone and requires W0 admission, fresh
   plans, exact owner paths, row gates, same-wave consumers, challenge acceptance
   where applicable, and dispatch authority
   (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:103`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:108`). Handoff repeats
   that W1-W6 remain blocked until W0 closes and each later wave satisfies its
   own gates (`restart/skinny/tranches/sk-v8/HANDOFF.md:238`).

## Material Blockers

None found.

## Residual Risks

- This CH6 ACCEPT is not a unilateral W0 close. W0 can close only after V8
  consolidation records >=95% ACCEPT with zero critical defects/no unresolved
  REVISE, and closure must be committed before W1-W6 dispatch.
- Some telemetry fields remain consumed as required present strings rather than
  deep enums, especially host/build text, `redress_entry`, and
  `track2_independence_status`
  (`skinny/crates/bbnf-bench/src/report.rs:287`,
  `skinny/crates/bbnf-bench/src/report.rs:295`,
  `skinny/crates/bbnf-bench/src/report.rs:313`). I do not classify this as a
  CH6 blocker because the closure-critical predicates are semantic and
  fail-closed: row identity, run id, profile/sample evidence, strict admission,
  sidecar freshness/source, 38-row baseline, and throughput drift.
- The run-id constant is a frozen W0-opening identity. Future waves that refresh
  Criterion evidence need an accepted replacement baseline rule; reusing the
  W0 constant for refreshed evidence should fail, as the copied-root probe
  showed.
