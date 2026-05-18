# SK-V8 W0 Hardening V12 CH3 - Regression

Date: 2026-05-18.

Target: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Verdict

ACCEPT.

Confidence: 97%.

This is the unchanged second CH3 qualifying cycle after V11. I found no
regression since the V11 archive: no REDRESS route is reopened, no admitted row
is silently regressed, W0 row identities/outcomes/verdicts/throughput remain
pinned, CostFacts/redress/Track 2/substrate tuple consumption remains executable,
and run-id/build-metadata drift still fails closed.

## Evidence

CH3's governing lens is regression: no proposal may reopen a `skinny/REDRESS.md`
route, and no admitted row may regress silently
(`restart/prompts/ORCHESTRATOR.md:85`). Convergence still requires two
consecutive challenge cycles at >=95% ACCEPT with no open critical defect or
orphan REVISE (`restart/prompts/ORCHESTRATOR.md:118`,
`restart/prompts/ORCHESTRATOR.md:120`). V11 accepted 6/6 and explicitly left W0
open for this V12 unchanged re-challenge
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:12`,
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:70`,
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:82`).

The target remains unchanged. `git show --stat --oneline
61d5cc3b4312883e026060174e876a0c18b34703` reports only
`skinny/crates/bbnf-bench/src/report.rs`, with `58 insertions / 109 deletions`.
The current HEAD is `b34dbeb81da7b29bb8135de4d54238d12765ed24`; `git diff
--stat 61d5cc3b4312883e026060174e876a0c18b34703..HEAD` reports only the seven V11
hardening archive files, and `git diff --name-only
61d5cc3b4312883e026060174e876a0c18b34703..HEAD -- skinny/RESULTS.md
skinny/REDRESS.md skinny/crates skinny/xtask
restart/skinny/tranches/sk-v8/SPEC.md
restart/skinny/tranches/sk-v8/HANDOFF.md
restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md` returned empty. That rules out
post-V11 drift in W0 source, RESULTS, REDRESS, or the dispatch packet.

No REDRESS route reopened. The inherited pre-block ledger still blocks new
directives, BIR/backend/substrate surfaces, sidecar/parallel substrates,
sidecar/permissive/lossy/stale strict admission, `parse_only`/telemetry rows as
W3 consumers, Track 1/Track 2 coupling, benchmark-private parsers, and automatic
implementation dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:756`,
`restart/skinny/tranches/sk-v8/SPEC.md:762`). The latest REDRESS authorities keep
CostFacts admitted only as evidence/reporting boundaries
(`skinny/REDRESS.md:2468`, `skinny/REDRESS.md:2502`) and keep PMULL prefix-XOR
and CSSC CTZ/bulk production bodies rejected
(`skinny/REDRESS.md:2594`, `skinny/REDRESS.md:2595`).

W0 rows remain pinned. SPEC/HANDOFF define the opening state as 16
`parse_only` `S / NO-GO`, one `parse_only` `L / NO-GO`, three
`direct_to_struct` `A / GO`, fourteen `direct_to_struct` `N-direct / NO-GO`, and
four `real_typed_struct` `A / GO`
(`restart/skinny/tranches/sk-v8/SPEC.md:153`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:34`). My `awk` audit over
`skinny/RESULTS.md` returned `main_rows=38`, `manifest_rows=38`,
`parse_only/S=16`, `parse_only/L=1`, `direct_to_struct/A=3`,
`direct_to_struct/N-direct=14`, and `real_typed_struct/A=4`. The live manifest
continues to render `SK-V8-open`, the constant run id, build metadata,
`none:pre-W1` CostFacts, `redress=none`, workload substrate tuple, `gate_only`,
and `independent_verified` Track 2 status
(`skinny/RESULTS.md:44`, `skinny/RESULTS.md:48`,
`skinny/RESULTS.md:49`, `skinny/RESULTS.md:50`).

The validator enforces those pins rather than trusting the rendered table. It
requires exactly `SK_V8_OPEN_BASELINE.len()` rows, rejects duplicates and unknown
row ids, rejects outcome/verdict movement, and checks Track 1/Track 2 against the
1% baseline bound (`skinny/crates/bbnf-bench/src/report.rs:494`,
`skinny/crates/bbnf-bench/src/report.rs:502`,
`skinny/crates/bbnf-bench/src/report.rs:512`,
`skinny/crates/bbnf-bench/src/report.rs:524`). The baseline stores the constant
run id and the 38 row identities/outcomes/verdicts/throughputs
(`skinny/crates/bbnf-bench/src/report.rs:655`,
`skinny/crates/bbnf-bench/src/report.rs:669`), and row validation rejects run-id
movement before accepting a W0 row
(`skinny/crates/bbnf-bench/src/report.rs:336`).

CostFacts/redress/Track 2/substrate tuple consumption remains intact. The W0
manifest semantics require exact pre-W1 CostFacts sentinels, `redress_entry=none`,
and `track2_independence_status=independent_verified`
(`skinny/crates/bbnf-bench/src/report.rs:1007`,
`skinny/crates/bbnf-bench/src/report.rs:1009`). The same helper requires
non-empty host/feature metadata with exact `target_cpu=native`
(`skinny/crates/bbnf-bench/src/report.rs:1020`,
`skinny/crates/bbnf-bench/src/report.rs:1039`,
`skinny/crates/bbnf-bench/src/report.rs:1053`) and exact workload substrate
tuples for parse, direct, and typed rows
(`skinny/crates/bbnf-bench/src/report.rs:1069`,
`skinny/crates/bbnf-bench/src/report.rs:1083`). The W0 mutation test still
forces CostFacts, redress, Track 2, metadata, and substrate drift to fail
(`skinny/crates/bbnf-bench/src/report.rs:2053`,
`skinny/crates/bbnf-bench/src/report.rs:2058`,
`skinny/crates/bbnf-bench/src/report.rs:2066`,
`skinny/crates/bbnf-bench/src/report.rs:2068`).

Gate consumption remains same-wave. `gate-json` validates schema-v3 and
`validate_sk_v8_w0()` before rendering or comparing `skinny/RESULTS.md`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:319`). The sidecar same-run route
still fails closed without a structured manifest
(`skinny/crates/bbnf-bench/src/report.rs:1287`), matching the W0 dispatch
requirement that `gate-json` reject malformed sidecar evidence and same-run
sidecar claims without a manifest
(`restart/skinny/tranches/sk-v8/SPEC.md:355`).

Verification passed from the skinny workspace:

- `cargo test -p bbnf-bench w0_ -- --nocapture`: 20 W0-filtered tests passed.
- `cargo test -p bbnf-bench strict -- --nocapture`: 5 strict tests passed.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`: 1 focused sidecar
  test passed.
- `cargo test -p bbnf-bench`: 52 library tests and 8 gate-bin tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results`: exited 0 and rendered the same
  `N-direct / NoGo` report.
- `git diff --name-only 0bd16f6d..61d5cc3b4312883e026060174e876a0c18b34703 --
  skinny/crates/runtime skinny/crates/bbnf-simd skinny/crates/codegen
  skinny/crates/ir skinny/crates/grammar skinny/crates/bbnf
  skinny/crates/bbnf-bench/src/direct_struct.rs
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/materialization.rs
  skinny/crates/bbnf-bench/src/parity.rs
  skinny/crates/bbnf-bench/src/probes.rs
  skinny/crates/bbnf-bench/src/real_typed_struct.rs
  skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/track2
  skinny/grammars skinny/crates/test-fixtures`: returned empty.
- `git diff --check`: passed.

## Blockers

None.

## Required Fold If Rejecting

Not applicable; CH3 accepts V12.

## Residual Risk

This CH3 acceptance is not W0 closure by itself. It only establishes the
regression lens for the unchanged second qualifying cycle. W1-W6 remain blocked
until the full V12 challenge cycle consolidates with zero critical defects and no
unresolved REVISE, and future behavior waves must still reprove REDRESS reopening
predicates with fresh W0 evidence, a same-wave consumer, a no-regression gate,
REDRESS citation, and challenge acceptance.
