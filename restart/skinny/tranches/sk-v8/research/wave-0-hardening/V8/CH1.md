# SK-V8 W0 Hardening V8 CH1 - Correctness

Date: 2026-05-18.

Target reviewed: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).
Current HEAD `ff6d09c6` was treated as documentation-only for implementation
review; `git diff --name-status f452e837..HEAD` lists only V7 hardening docs.

Verdict: REJECT.

Confidence: 94%.

V8 cannot count as the second consecutive ACCEPT. V7 is documented as the first
qualifying ACCEPT after V6 reset the counter
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`),
and the orchestrator requires two consecutive >=95% ACCEPT cycles with zero
critical defects (`restart/prompts/ORCHESTRATOR.md:120`). This CH1 REJECT breaks
that chain.

## Reviewed surfaces

- CH1 lens requirements: correctness claims must cite file:line/evidence and
  strictness-plane deltas must match (`restart/prompts/ORCHESTRATOR.md:83`).
- W0 convergence/governance: challenge cycles and two-cycle convergence
  (`restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:120`).
- SK-V8 W0 packet: comparator classes, outcome enum, required telemetry, W0
  target, W0 tasks, exit gate, same-wave consumer, and pre-blocked routes
  (`restart/skinny/tranches/sk-v8/SPEC.md:65`,
  `restart/skinny/tranches/sk-v8/SPEC.md:97`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:159`,
  `restart/skinny/tranches/sk-v8/SPEC.md:312`,
  `restart/skinny/tranches/sk-v8/SPEC.md:348`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`).
- Handoff and dispatch constraints: W0 current rows are deferred, sidecars are
  historical/absent planning signals, and W0 is telemetry-only
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:46`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:87`).
- `skinny/RESULTS.md` main table and W0 manifest rows
  (`skinny/RESULTS.md:3`, `skinny/RESULTS.md:46`, `skinny/RESULTS.md:141`).
- W0 code in `skinny/crates/bbnf-bench/src/{gate.rs,report.rs,bin/gate.rs}`,
  plus Lock 14 validation and behavior-surface diff checks.

## Commands and evidence

- `git show --stat --oneline --find-renames f452e837` - target changes only
  `DISPATCH-PROMPT.md`, `HANDOFF.md`, `SPEC.md`, and
  `skinny/crates/bbnf-bench/src/report.rs`.
- `git diff --name-status f452e837..HEAD` - only V7 hardening docs were added.
- `cargo test -p bbnf-bench w0_ -- --nocapture` - PASS: 20 focused W0 tests
  passed, including exact baseline, row identity, throughput drift, run-id drift,
  sidecar freshness, native comparator source/semantic mismatch, and Criterion
  fingerprint filtering.
- `cargo test -p bbnf-bench strict -- --nocapture` - PASS: 5 strict-admission
  tests passed.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` - PASS: sidecar
  same-run without structured manifest rejected.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  - PASS/exit 0 against the existing Criterion capture; output retained
  `Overall outcome N-direct / NoGo.` and the W0 telemetry note that native Rust
  comparators are same-run while C++ sidecars are historical or absent planning
  signals.
- `git diff --name-only 0bd16f6d..f452e837 -- <frozen behavior paths>` - no
  output for runtime/parser/tape/SIMD/codegen/generated/Track2/parity/scan/
  materialization/fixtures/grammar paths.

## Material blocker

### B1 - Hard-failure rows can be converted into accepted strict claims

The packet says hard-failure outcomes and `S` cannot support strict SOTA
admission (`restart/skinny/tranches/sk-v8/SPEC.md:100`) and that stale,
historical, lossy, permissive, or view-boundary evidence cannot be strict
admission (`restart/skinny/tranches/sk-v8/SPEC.md:204`). W0 current rows are
also explicitly deferred (`restart/skinny/tranches/sk-v8/HANDOFF.md:42`).

The implemented strict-admission helper rejects unsupported outcomes, `K`, and
`S`, then accepts a strict/measured/native same-run shape
(`skinny/crates/bbnf-bench/src/gate.rs:135`,
`skinny/crates/bbnf-bench/src/gate.rs:139`,
`skinny/crates/bbnf-bench/src/gate.rs:151`). It does not reject other hard
failures such as `L` or `M`, even though `L` and `M` are NoGo hard-failure
outcomes (`skinny/crates/bbnf-bench/src/gate.rs:16`,
`skinny/crates/bbnf-bench/src/gate.rs:83`).

`Report::validate_sk_v8_w0()` fixes row count, row id, outcome, verdict, and
Track 1/Track 2 throughput against `SK_V8_OPEN_BASELINE`
(`skinny/crates/bbnf-bench/src/report.rs:499`,
`skinny/crates/bbnf-bench/src/report.rs:517`,
`skinny/crates/bbnf-bench/src/report.rs:529`). It also binds `run_id` to the
exact SK-V8-open fingerprint (`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:660`). But it does not require W0
strictness, `parse_utf8`, measured-validation path, or output plane to remain
the SK-V8-open deferred/view-boundary shape. If a row makes a strict claim,
`validate_w0_admission_boundary()` delegates to the helper and accepts the first
passing native comparator (`skinny/crates/bbnf-bench/src/report.rs:1012`,
`skinny/crates/bbnf-bench/src/report.rs:1043`,
`skinny/crates/bbnf-bench/src/report.rs:1060`).

Focused repro:

1. Built a temporary crate under `/tmp/skv8-report-probe` depending on
   `skinny/crates/bbnf-bench`.
2. Constructed a full `Report` from `SK_V8_OPEN_BASELINE`, preserving exact
   row ids, outcomes, verdicts, Track 1/Track 2 baselines, and
   `SK_V8_OPEN_RUN_ID`.
3. Mutated only `json/canada/parse_only/main` from deferred/view-boundary to
   `strict`, `parse_utf8=measured-row`, `measured_validation_path=measured-row`,
   and `output_plane=DOM` while keeping the exact baseline `L / NO-GO`
   outcome/verdict and throughput.
4. Called `report.validate_sk_v8_w0()`.

Observed output:

```text
baseline: true
canada_L_strict_DOM: Ok(())
```

A narrower helper probe also showed:

```text
A: true
K: false
L: true
M: true
S: false
N-direct: true
```

This breaks the strict-vs-strict comparator validation challenge point. The W0
gate can accept a hard-failure row as a measured strict claim without moving its
exact row id, exact outcome/verdict, exact throughput, or exact run id.

Required redress: either W0 must enforce the current deferred/view-boundary row
shape exactly for all 38 SK-V8-open rows, or `gate::validate_strict_admission`
must reject all hard-failure/non-admission outcomes before accepting native
same-run strict evidence. The safer fix is both: freeze W0 rows as deferred for
this baseline, and make the helper reject `G`, `I`, `J`, `K`, `L`, `M`, and `S`
for strict admission.

## Surfaces that held under this challenge

- Exact row identity and row cardinality held for the current generated report:
  duplicate, unknown, missing, and rendered-row mismatch paths reject
  (`skinny/crates/bbnf-bench/src/report.rs:507`,
  `skinny/crates/bbnf-bench/src/report.rs:514`,
  `skinny/crates/bbnf-bench/src/report.rs:532`,
  `skinny/crates/bbnf-bench/src/report.rs:1072`).
- Exact outcome/verdict and Track 1/Track 2 throughput baselines reject drift
  above +/-1.0% (`skinny/crates/bbnf-bench/src/report.rs:517`,
  `skinny/crates/bbnf-bench/src/report.rs:523`,
  `skinny/crates/bbnf-bench/src/report.rs:942`).
- Exact run-id binding held for the V6 blocker: arbitrary single-row and uniform
  `sk-v8-open:test` mutations reject
  (`skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:1976`).
- Row-manifest Criterion fingerprinting is scoped to W0 rows and excludes
  unvalidated future/probe artifacts while including baseline row estimates and
  metadata (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:707`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1770`).
- Sidecar freshness/source validation rejects `sidecar-same-run` without a
  structured manifest and rejects populated sidecars marked absent
  (`skinny/crates/bbnf-bench/src/report.rs:1131`,
  `skinny/crates/bbnf-bench/src/report.rs:1133`,
  `skinny/crates/bbnf-bench/src/report.rs:1211`,
  `skinny/crates/bbnf-bench/src/report.rs:1235`).
- Required telemetry is consumed at least at presence/baseline levels by
  `Report::validate_sk_v8_w0()` and by `gate-json` before RESULTS comparison
  (`skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:330`).

## Residual risks

Not accepting, so no accepting residual-risk posture applies. Secondary note for
the redress cycle: several telemetry fields are consumed primarily as non-empty
strings rather than closed enums (`build_flags`, `host_triple`, `feature_mask`,
`substrate_surface`, `structural_projection_status`, `substrate_cardinality`,
`track2_independence_status`). That did not drive this REJECT because B1 is
already sufficient and directly falsifies strict-admission correctness.
