# CH5 W0 V8 Hardening Challenge

## Verdict

ACCEPT.

Confidence: 95%.

Reviewed target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).
Current HEAD `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd` only adds V7
hardening documents relative to the target; those documents were not counted as
implementation changes.

V8 second-consecutive status: this CH5 result can count as the unchanged
second consecutive ACCEPT contribution for W0. Overall V8 can count as the
second consecutive accept only if the consolidated V8 result is also ACCEPT
with no critical defect and no unresolved REVISE.

## Reviewed Surfaces

- W0 owner paths, tasks, cost fold, exit gate, same-wave consumer, and
  pre-blocked routes:
  `restart/skinny/tranches/sk-v8/SPEC.md:290`,
  `restart/skinny/tranches/sk-v8/SPEC.md:310`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`,
  `restart/skinny/tranches/sk-v8/SPEC.md:762`.
- Handoff sidecar caveat, W3 telemetry-consumer caveat, and telemetry-only W0
  rule: `restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:95`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`.
- Dispatch cost fold and downstream blocks:
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:65`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:181`.
- W0 report and gate code:
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/report.rs:1211`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:474`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:557`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:603`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/gate.rs:135`.
- Lock 14 and Track 2 surfaces:
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
  `skinny/crates/bbnf-bench/src/track2/json.rs:1`,
  `skinny/crates/bbnf-bench/src/track2/json.rs:357`.
- Committed W0 results manifest and notes:
  `skinny/RESULTS.md:44`, `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:140`, `skinny/RESULTS.md:141`.

## Hidden-Coupling Challenge

No material CH5 hidden-coupling blocker found.

W0 remains telemetry/gate/report only. The target implementation slice from
`0bd16f6d..f452e837` changes only `skinny/RESULTS.md`,
`skinny/crates/bbnf-bench/src/bin/gate.rs`,
`skinny/crates/bbnf-bench/src/gate.rs`,
`skinny/crates/bbnf-bench/src/lib.rs`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
`skinny/crates/bbnf-bench/src/report.rs`, and
`skinny/xtask/src/main.rs`. The frozen-surface diff over runtime JSON/tape,
SIMD, codegen, generated/product helpers, Track 2, parity, scan, and
materialization was empty. That matches the W0 cost fold's condition that the
larger gate/report scope is admissible only while behavior surfaces stay frozen
(`restart/skinny/tranches/sk-v8/SPEC.md:335`).

No parallel substrate, renamed scanner, or public side substrate landed. Lock
14 freezes the grammar, runtime, IR, passes, codegen, SIMD, parse-that-regex,
Track 2, parity, scan, and materialization roots
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`) and separately rejects
BackendShape drift or `UnionTape`/`union_tape` in the IR surface
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`). The W0 substrate fields
are report facts: parse rows render `borrowed_view_over_offset_tape` with
cardinality `one`; direct and typed rows render `zero_or_inert`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:603`).

No sidecar producer or same-run sidecar coupling is admitted. W0 emits native
Rust comparator evidence as `same-run-native`, while C++ sidecar values are
either `historical:sk-v7-sidecar-profile` or explicit `absent:<reason>` fields
(`skinny/crates/bbnf-bench/src/bin/gate.rs:557`). The report validator rejects
sidecar source/freshness mismatch and rejects `sidecar-same-run` without a
structured manifest (`skinny/crates/bbnf-bench/src/report.rs:1211`,
`skinny/crates/bbnf-bench/src/report.rs:1235`). Strict admission also rejects
stale, historical, absent, and non-native freshness
(`skinny/crates/bbnf-bench/src/gate.rs:163`).

Track 1 / Track 2 honesty held for W0. Track 1 remains the generated parser
bench path and Track 2 remains the hand-coded parser over `runtime::tape`, with
the report note explicitly saying Track 2 never calls
`runtime::generated_json::parse` (`skinny/RESULTS.md:140`). The Track 2 parser
does have its own private cursor, but it is an unchanged benchmark oracle, not a
new production substrate or W0 behavior surface
(`skinny/crates/bbnf-bench/src/track2/json.rs:14`). The compatibility test
compares Track 2 offsets against Track 1 without using Track 1 as the parser
under test (`skinny/crates/bbnf-bench/src/track2/json.rs:357`).

The run-id and FNV64 identity hazards are closed enough for W0 stale-evidence
identity. The gate computes a run id from selected W0 Criterion inputs
(`skinny/crates/bbnf-bench/src/bin/gate.rs:390`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:673`), and report validation hard
compares every row to the exact accepted constant
`sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
(`skinny/crates/bbnf-bench/src/report.rs:336`,
`skinny/crates/bbnf-bench/src/report.rs:660`). The input selector excludes
probe/future/unvalidated rows and admits only paths that map to current W0 row
ids (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`). FNV64 remains a residual
non-cryptographic collision risk, but it is not being used as an adversarial
security boundary.

No cost-governance loophole was found that would let W3/W4 treat W0 telemetry
as a production consumer. W0 validates `same_wave_consumer_class == "gate_only"`
(`skinny/crates/bbnf-bench/src/report.rs:361`), and the SK-V8 route ledger
blocks telemetry rows, `parse_only`, `tape_vs_tape`, sidecar evidence,
parser-owned cursor/facts, sidecar substrate, and parallel substrate as W3
production authority (`restart/skinny/tranches/sk-v8/SPEC.md:762`,
`restart/skinny/tranches/sk-v8/SPEC.md:769`). W1 still owns CostFacts gate
binding before later behavior waves can cite CostFacts evidence
(`restart/skinny/tranches/sk-v8/SPEC.md:398`,
`restart/skinny/tranches/sk-v8/SPEC.md:411`).

## Commands And Evidence

- `git rev-parse HEAD && git rev-parse f452e837`: HEAD is
  `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd`; target is
  `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `git diff --name-status f452e837..HEAD`: only adds V7 hardening docs under
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/`.
- `git show --stat --oneline f452e837`: f452 changes only SK-V8 docs plus
  `skinny/crates/bbnf-bench/src/report.rs`; report change is exact run-id
  binding and focused tests.
- `git diff --name-status 0bd16f6d..f452e837 -- skinny`: only the seven W0
  owner files named above changed.
- `git diff --quiet 0bd16f6d..f452e837 -- <runtime/codegen/IR/passes/SIMD/
  Track2/parity/scan/direct/typed/generated surfaces>`: exited 0 and printed
  `frozen-surface-diff-empty`.
- `cargo test -p bbnf-bench w0_`: PASS. 12 W0 report tests and 8 W0 gate-bin
  tests passed, including run-id drift rejection, sidecar mismatch rejection,
  same-run sidecar rejection, W0 metadata validation, and FNV scope filtering.
- `cargo test -p bbnf-bench lock14_baseline::tests`: PASS. 7 Lock14 tests
  passed, including frozen-root coverage and backend-shape drift checks.
- `awk` over `skinny/RESULTS.md` W0 manifest: `rows=38`,
  `run=sk-v8-open:criterion-fnv64-9a37562ed3d0383a`, `run_count=38`,
  `gate_only=38`, `independent=38`, `one=17`, `zero_or_inert=21`.
- `cargo xtask gate-json --check-results --advisory >/tmp/sk-v8-w0-gate.out`:
  failed closed on local Criterion artifacts with
  `twitter SIMD metadata invalid: SIMD metadata has unsupported capture policy`.
  This is not a CH5 implementation blocker: the validator rejects unsupported
  local capture policy at `skinny/crates/bbnf-bench/src/bin/gate.rs:1427`, and
  the focused tests plus committed manifest verify the target W0 gate logic.

## Material Blockers

None.

No repro showed a hidden parallel substrate, sidecar producer, scanner rename,
Track1/Track2 coupling, parser-owned production cursor/fact slot, permissive
run-id acceptance, FNV64 scope escape, or W3/W4 production-consumer loophole.

## Residual Risks

- Full live `gate-json` could not be re-run from the current local
  `target/criterion` artifacts because the gate correctly rejected their SIMD
  capture policy. A fresh benchmark capture is still required for any future
  W0 result refresh.
- FNV64 is adequate for W0 stale-artifact identity, not a security hash.
- Existing Track 2 has a private parser cursor as an unchanged benchmark oracle;
  later W2/W3/W4 work must re-prove independence and cannot inherit W0
  telemetry as production-consumer evidence.
- `none:pre-W1` CostFacts placeholders are acceptable only for W0. W1 must make
  missing CostFacts evidence fatal before later waves cite CostFacts as
  dispatch or close evidence.
