# CH5 W0 V7 Hardening Challenge

## Verdict

ACCEPT.

Confidence: 94%.

Reviewed target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

## Reviewed Surfaces

- CH5 lens and convergence rules: `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:81`, `restart/prompts/ORCHESTRATOR.md:87`,
  `restart/prompts/ORCHESTRATOR.md:104`, and
  `restart/prompts/ORCHESTRATOR.md:118`.
- SK-V8 W0 authority, strict comparator boundary, telemetry fields,
  non-negotiables, V7 cost fold, W1 consumer gate, W3 substrate constraints, and
  route ledger: `restart/skinny/tranches/sk-v8/SPEC.md:31`,
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:191`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:398`,
  `restart/skinny/tranches/sk-v8/SPEC.md:499`,
  `restart/skinny/tranches/sk-v8/SPEC.md:550`,
  `restart/skinny/tranches/sk-v8/SPEC.md:758`, and
  `restart/skinny/tranches/sk-v8/SPEC.md:803`.
- Dispatch and handoff W0-only scope, conditional W1-W6 gates, and
  substrate-ceiling caveats:
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:63`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:95`, and
  `restart/skinny/tranches/sk-v8/HANDOFF.md:150`.
- Current results and redress surfaces:
  `skinny/RESULTS.md:3`, `skinny/RESULTS.md:44`,
  `skinny/RESULTS.md:46`, `skinny/RESULTS.md:48`,
  `skinny/REDRESS.md:110`, `skinny/REDRESS.md:126`,
  `skinny/REDRESS.md:729`, `skinny/REDRESS.md:742`,
  `skinny/REDRESS.md:1736`, and `skinny/REDRESS.md:2466`.
- W0 implementation surfaces:
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/report.rs:1038`,
  `skinny/crates/bbnf-bench/src/report.rs:1211`,
  `skinny/crates/bbnf-bench/src/report.rs:1976`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:307`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:472`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:501`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:603`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1769`,
  `skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/gate.rs:163`, and
  `skinny/xtask/src/main.rs:240`.

## Findings

No material CH5 blocker found.

The V7 fold closes the V6 run-identity hidden-coupling blocker. W0 row
validation now compares every telemetry row against the exact
`SK_V8_OPEN_RUN_ID`, not just a non-empty or same-prefix string
(`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:660`). The focused regression test
rejects both a single-row `sk-v8-open:test` mutation and a uniform fake run id
across all rows (`skinny/crates/bbnf-bench/src/report.rs:1976`,
`skinny/crates/bbnf-bench/src/report.rs:1980`). The producer-side fingerprint is
computed over sorted W0 Criterion inputs (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`)
and admits only paths that resolve to a current W0 opening row
(`skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:745`); its test excludes volatile
probe estimates, unvalidated future groups, and valid-fixture/unvalidated-row
files while still changing on a real W0 row mutation
(`skinny/crates/bbnf-bench/src/bin/gate.rs:1769`).

No parallel substrate, renamed scanner, parser-owned fact slot, or sidecar
producer landed in the W0 implementation slice. The V7 cost fold explicitly
admits only the measured W0 telemetry gate/report/Lock 14 scope and requires the
frozen behavior-surface diff to stay empty (`restart/skinny/tranches/sk-v8/SPEC.md:322`,
`restart/skinny/tranches/sk-v8/SPEC.md:335`). My frozen-surface diff check from
`0bd16f6d..HEAD` returned no paths for grammar input, runtime JSON/tape, SIMD,
codegen, generated/product helpers, Track 2, parity, scan, or materialization.
The emitted W0 substrate fields remain report facts consumed by the gate:
`borrowed_view_over_offset_tape` with cardinality `one` for parse rows and
`sink_only_digest` / `typed_direct_projection` with `zero_or_inert` for
direct/typed rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:603`). They do not
create a public substrate API or a second retained parser surface.

Sidecar evidence remains isolated as planning evidence. W0 emits historical
sidecar values or explicit absences (`skinny/crates/bbnf-bench/src/bin/gate.rs:557`)
and rejects `sidecar-same-run` without a structured manifest
(`skinny/crates/bbnf-bench/src/report.rs:1235`). Strict admission rejects stale,
historical, absent, or non-native comparator freshness
(`skinny/crates/bbnf-bench/src/gate.rs:163`). The current manifest shows those
sidecars as `historical:sk-v7-sidecar-profile` or `absent:<reason>` and uses
`gate_only` as the same-wave consumer (`skinny/RESULTS.md:48`).

Track 1 / Track 2 honesty held for W0. The gate report still states that Track
1 is `runtime::generated_json::parse` and Track 2 is the independent hand-coded
parser over `runtime::tape`, with a checklist that Track 2 never calls Track 1
(`skinny/crates/bbnf-bench/src/bin/gate.rs:307`). The current manifest reports
`track2_independence_status=independent_verified` on all 38 rows, and
`cargo test -p bbnf-bench` passed
`track2::json::tests::emits_track1_compatible_offsets_without_calling_track1_parser`.
REDRESS still records one tape/direct substrate and rejects parser-owned side
tables, EventCursor wrappers, and parser-owned decoded scratch as reopened
routes (`skinny/REDRESS.md:126`, `skinny/REDRESS.md:729`,
`skinny/REDRESS.md:742`, `skinny/REDRESS.md:1736`).

The V7 cost-governance fold does not authorize later behavior waves by analogy.
The broader W0 scope is tied to exact files, exact accounting, a frozen-surface
condition, and commit-sliced rollback
(`restart/skinny/tranches/sk-v8/SPEC.md:322`,
`restart/skinny/tranches/sk-v8/SPEC.md:341`). W1-W6 remain conditional and
require W0 closure, fresh owner paths/gates, required challenge acceptance, and
orchestrator/user dispatch before redress
(`restart/skinny/tranches/sk-v8/SPEC.md:807`). W1 is still responsible for
making `gate-json --with-cost-facts` reject missing evidence after W1 and for
consuming CostFacts/comparator fields in the same wave
(`restart/skinny/tranches/sk-v8/SPEC.md:398`,
`restart/skinny/tranches/sk-v8/SPEC.md:411`,
`restart/skinny/tranches/sk-v8/SPEC.md:418`). Therefore the existing W0
`none:pre-W1` placeholders and the current non-fatal SK-V7 CostFacts diagnostics
are not W2/W3/W4 dispatch authority.

## Commands And Evidence

- `git rev-parse HEAD && git log -1 --oneline`: HEAD is
  `f452e8373ed717731dd5e720c1d947c086cc22c9` with subject
  `fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`.
- `git status --short`: clean before writing this artifact.
- `git diff --stat 0bd16f6d..HEAD -- <seven W0 files>`: 7 files, 3550
  insertions, 253 deletions. The slight insertion count over the SPEC's
  3532-to-V6 accounting is the post-V6 V7 run-id/test/doc fold, matching the
  documented post-V6 scope.
- `git diff --name-only 0bd16f6d..HEAD -- skinny/grammars/json.bbnf
  skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd
  skinny/crates/grammar skinny/crates/ir skinny/crates/passes
  skinny/crates/bbnf skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/real_typed_struct.rs
  skinny/crates/bbnf-bench/src/direct_struct.rs
  skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs
  skinny/crates/bbnf-bench/src/materialization.rs`: returned no paths.
- `awk` manifest parse of `skinny/RESULTS.md`: 38 main rows, 38 manifest rows,
  one run id (`sk-v8-open:criterion-fnv64-9a37562ed3d0383a`) on all rows,
  `gate_only=38`, `independent_verified=38`, cardinality `one=17` and
  `zero_or_inert=21`.
- `cargo test -p bbnf-bench w0_ -- --nocapture`: PASS; 12 W0 report tests and
  8 W0 gate-bin tests passed.
- `cargo test -p bbnf-bench`: PASS; 52 library tests and 8 gate-bin tests
  passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results`: PASS; the gate replay exited 0
  and matched committed `skinny/RESULTS.md`.
- `cargo xtask gate-json --with-cost-facts --advisory`: exited 0 and printed
  schema `sk-v7-costfacts-v1` with `BBNF-COSTFACTS-MISSING-EVIDENCE`
  diagnostics. This confirms the current command is still the pre-W1 diagnostic
  surface described by REDRESS 87, not an admitted W1 consumer.
- `git diff --check`: PASS.

## Material Blockers

None.

No repro produced a CH5 rejection. The exact run-id binding, row identity,
sidecar same-run rejection, W0-only consumer classification, Track 2
independence evidence, and frozen behavior-surface checks all held.

## Residual Risks

- `gate-json --with-cost-facts --advisory` is still non-fatal for missing
  CostFacts evidence. That is acceptable for W0 because W1 is explicitly tasked
  with making it reject missing evidence, but W1 must not cite the current
  diagnostic-only command as its close gate.
- `validate_strict_admission()` currently has focused W0 coverage for `K`, `S`,
  deferred/view-boundary, sidecar, and plane mismatch. Before any new strict
  behavior row is admitted, W1 should re-check the full Section 0.3 hard-failure
  outcome set so no future `L`, `N-direct`, or other NO-GO outcome can become a
  strict-admission carrier by accident.
- The run fingerprint is FNV64 over selected Criterion files. That is sufficient
  as W0 stale-evidence identity, not a security hash.
- Track 2 independence is accepted for the current W0 telemetry and frozen
  behavior roots. Later W2/W3/W4 behavior waves still need fresh same-wave
  Track 1 / Track 2 independence proof and cannot inherit this W0 checklist as
  production-consumer evidence.
