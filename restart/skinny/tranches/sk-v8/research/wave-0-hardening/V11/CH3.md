# SK-V8 W0 Hardening V11 CH3 - Regression

Date: 2026-05-18.

Target: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Verdict

ACCEPT.

Confidence: 96%.

The V11 fold does not reopen a REDRESS route, does not change a behavior owner,
and preserves the admitted V10 W0 telemetry-consumption checks. The two V10 CH4
blockers are folded: the live post-V6 report footprint is under the `<=120`
cap, and empty host/feature metadata now fails closed.

## Evidence

CH3's contract is regression-focused: no proposal may reopen a REDRESS route or
silently regress an admitted row (`restart/prompts/ORCHESTRATOR.md:85`), and W0
is telemetry-only with all 38 current rows required to satisfy telemetry and
stay within +/-1.0% of `SK-V8-open` (`restart/skinny/tranches/sk-v8/SPEC.md:346`,
`restart/skinny/tranches/sk-v8/SPEC.md:357`). The inherited route ledger blocks
new directives, BIR/backend/substrate surfaces, sidecar/parallel substrates,
stale comparator strict admission, `parse_only` or telemetry rows as W3
consumers, Track 1/Track 2 coupling, benchmark-private parsers, and automatic
implementation dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:756`,
`restart/skinny/tranches/sk-v8/SPEC.md:762`). The latest REDRESS entries admit
CostFacts only under evidence/reporting boundaries and keep W10 PMULL/CTZ bitmap
bodies rejected (`skinny/REDRESS.md:2468`, `skinny/REDRESS.md:2504`,
`skinny/REDRESS.md:2594`).

The target patch is a report-validation-only fold. `git show --stat
61d5cc3b` reports only `skinny/crates/bbnf-bench/src/report.rs`, with `58
insertions / 109 deletions`. `git diff --name-only
0bd16f6d..61d5cc3b -- skinny/crates/runtime skinny/crates/bbnf-simd
skinny/crates/codegen skinny/crates/ir skinny/crates/grammar
skinny/crates/bbnf skinny/crates/bbnf-bench/src/direct_struct.rs
skinny/crates/bbnf-bench/src/generated_real_typed.rs
skinny/crates/bbnf-bench/src/materialization.rs
skinny/crates/bbnf-bench/src/parity.rs
skinny/crates/bbnf-bench/src/probes.rs
skinny/crates/bbnf-bench/src/real_typed_struct.rs
skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/track2
skinny/grammars skinny/crates/test-fixtures` returned empty, so V11 does not
touch runtime, SIMD, codegen, generated/product, Track 2, fixture, or grammar
behavior surfaces.

The V10 cost blocker is folded. V10 CH4 rejected `3a9fa326` because the live
post-V6 fold was `169 insertions / 13 deletions` against the `<=120` cap and no
reauthorization existed (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:45`).
V11 compacted the same live slice: `git diff --shortstat
00c3485a..61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs` reports `118
insertions / 13 deletions`, under the live cap.

The V10 metadata blocker is folded. `validate_w0_manifest_semantics()` now
requires exact W0 CostFacts sentinels, `redress_entry=none`, and
`track2_independence_status=independent_verified`
(`skinny/crates/bbnf-bench/src/report.rs:1007`). It then validates exact build
flags, a non-empty host triple with non-empty `arch` and `cpu`, and a feature
mask with non-empty `arch`, `os`, and `simd` plus exact `target_cpu=native`
(`skinny/crates/bbnf-bench/src/report.rs:1020`,
`skinny/crates/bbnf-bench/src/report.rs:1039`,
`skinny/crates/bbnf-bench/src/report.rs:1053`). The V11 test mutates both empty
metadata cases and requires W0 validation to fail
(`skinny/crates/bbnf-bench/src/report.rs:2065`,
`skinny/crates/bbnf-bench/src/report.rs:2068`).

Opening row identities and admitted outcomes remain pinned. The report validator
requires exactly the `SK_V8_OPEN_BASELINE` row count, rejects duplicates and
unknown row ids, rejects outcome/verdict movement, and checks Track 1 and Track
2 against the +/-1.0% baseline bound
(`skinny/crates/bbnf-bench/src/report.rs:494`,
`skinny/crates/bbnf-bench/src/report.rs:512`,
`skinny/crates/bbnf-bench/src/report.rs:524`,
`skinny/crates/bbnf-bench/src/report.rs:937`). The baseline table encodes the
current 38 W0 row ids/outcomes/verdicts/throughputs
(`skinny/crates/bbnf-bench/src/report.rs:669`). `awk` over `skinny/RESULTS.md`
reported `main_rows=38`, `manifest_rows=38`, `parse_only / S=16`,
`parse_only / L=1`, `direct_to_struct / A=3`, `direct_to_struct / N-direct=14`,
and `real_typed_struct / A=4`, matching SPEC/HANDOFF opening state
(`restart/skinny/tranches/sk-v8/SPEC.md:153`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:34`).

Run-id drift and strict-admission drift fail closed. Each row must carry the
constant W0 run id, and any row or uniform run-id mutation is rejected by the
baseline test (`skinny/crates/bbnf-bench/src/report.rs:655`,
`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:2031`). W0 rows also remain
`deferred`/`view-boundary`, parse rows cannot become strict admission, and
sidecar `sidecar-same-run` claims reject without a structured manifest
(`skinny/crates/bbnf-bench/src/report.rs:1096`,
`skinny/crates/bbnf-bench/src/report.rs:1275`,
`skinny/crates/bbnf-bench/src/report.rs:1287`).

CostFacts/redress/Track 2/substrate tuple consumption survived the compaction.
The exact parse/direct/typed substrate tuples are centralized in
`w0_substrate_tuple()` and compared against each row's manifest tuple
(`skinny/crates/bbnf-bench/src/report.rs:1069`,
`skinny/crates/bbnf-bench/src/report.rs:1083`). The W0 baseline test mutates
CostFacts, redress, Track 2, metadata, and substrate fields while preserving the
rest of the report, and every mutation must fail
(`skinny/crates/bbnf-bench/src/report.rs:2053`,
`skinny/crates/bbnf-bench/src/report.rs:2058`). `gate-json` consumes schema-v3
and W0 validation before rendering/comparing `skinny/RESULTS.md`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:319`), and the rendered manifest
includes run id, build metadata, CostFacts, redress, substrate tuple, consumer,
Track 2, and comparator evidence (`skinny/crates/bbnf-bench/src/report.rs:575`).

Verification passed:

- `cargo test -p bbnf-bench w0_ -- --nocapture` passed 20 focused W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture` passed 5 strict tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed 1 focused
  sidecar same-run test.
- `cargo test -p bbnf-bench` passed 60 total package/bin tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results` passed.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and `cargo xtask
  check-conformance` passed; conformance reported 21 valid fixtures accepted and
  7 invalid fixtures rejected.
- `git diff --check` passed.

## Blockers

None.

## Required Fold

None.

## Residual Risk

This is still a W0 telemetry/report gate, not behavior-wave evidence. W1-W6
remain blocked until W0 receives the required consecutive ACCEPT cycles, and any
future behavior wave must reprove REDRESS reopening predicates with fresh W0
evidence, same-wave consumer, no-regression gate, and challenge acceptance.
