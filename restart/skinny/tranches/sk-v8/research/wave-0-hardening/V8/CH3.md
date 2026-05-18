# SK-V8 W0 Hardening V8 CH3 - Regression

Date: 2026-05-18.

Target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

Current HEAD `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd` only adds V7
hardening docs relative to the target, so this challenge treats `f452e837` as
the implementation state and uses the V7 docs only as prior accept evidence.

## Verdict

ACCEPT.

Confidence: 93%.

V8 can count as the CH3 component of the second consecutive ACCEPT cycle. It
does not close W0 by itself: W0 still needs V8 consolidation at >=95% ACCEPT,
zero open critical defects, and no unresolved REVISE under the orchestrator
two-cycle rule (`restart/prompts/ORCHESTRATOR.md:118`,
`restart/prompts/ORCHESTRATOR.md:120`,
`restart/prompts/ORCHESTRATOR.md:123`). If the other V8 lenses and
consolidation also accept unchanged target `f452e837`, this cycle may be the
second consecutive W0 accept after V7.

## Reviewed Surfaces

- CH3 and convergence governance: `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:85`,
  `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- V7 prior accept evidence: V7 consolidated ACCEPT and first-cycle status at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:10`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:26`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:50`.
- V6 blockers being re-challenged: run-id and cost-governance rejection plus
  required fold at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:22`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`.
- Live W0 contract and no-paper-close boundaries:
  `restart/skinny/tranches/sk-v8/SPEC.md:63`,
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`,
  `restart/skinny/tranches/sk-v8/SPEC.md:756`,
  `restart/skinny/tranches/sk-v8/SPEC.md:803`.
- Dispatch and handoff blocks:
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:63`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:171`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:127`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:178`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`.
- Current row and REDRESS/pre-block evidence:
  `skinny/RESULTS.md:3`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:85`,
  `skinny/RESULTS.md:138`,
  `skinny/RESULTS.md:141`,
  `skinny/REDRESS.md:2130`,
  `skinny/REDRESS.md:2152`,
  `skinny/REDRESS.md:2179`,
  `skinny/REDRESS.md:2589`,
  `skinny/REDRESS.md:2594`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:42`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:57`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:103`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:133`.
- W0 gate/report implementation:
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/report.rs:942`,
  `skinny/crates/bbnf-bench/src/report.rs:1012`,
  `skinny/crates/bbnf-bench/src/report.rs:1083`,
  `skinny/crates/bbnf-bench/src/report.rs:1211`,
  `skinny/crates/bbnf-bench/src/report.rs:1261`,
  `skinny/crates/bbnf-bench/src/report.rs:1905`,
  `skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:474`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1075`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1385`.

## Disposition

No admitted-row regression found. The checked-in W0 table still has 38 main
rows and 38 manifest rows. The main-table mix is 16 `S`, 1 `L`, 14 `N-direct`,
and 7 `A`; manifest rows are all `SK-V8-open`, all `baseline`, all
`gate_only`, and all carry `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`.
That matches the SPEC opening posture for 16 substrate-guard parse rows, one
hard parse failure, 14 direct guard rows, 3 direct GO rows, and 4 real-typed GO
rows (`restart/skinny/tranches/sk-v8/SPEC.md:153`,
`restart/skinny/tranches/sk-v8/SPEC.md:159`, `skinny/RESULTS.md:5`,
`skinny/RESULTS.md:42`, `skinny/RESULTS.md:48`, `skinny/RESULTS.md:85`).
The executable report validator also enforces exact row count, unique known row
ids, unchanged outcome/verdict, and +/-1.0% Track 1/Track 2 drift against
`SK_V8_OPEN_BASELINE` (`skinny/crates/bbnf-bench/src/report.rs:499`,
`skinny/crates/bbnf-bench/src/report.rs:514`,
`skinny/crates/bbnf-bench/src/report.rs:517`,
`skinny/crates/bbnf-bench/src/report.rs:529`,
`skinny/crates/bbnf-bench/src/report.rs:532`).

The V6 run-id blocker is closed for CH3. `SK_V8_OPEN_RUN_ID` is a fixed
fingerprint constant (`skinny/crates/bbnf-bench/src/report.rs:660`), every W0
row must equal it (`skinny/crates/bbnf-bench/src/report.rs:336`), and the V8
test run passed the negative cases for both a single stale row in an otherwise
valid report and a uniform stale report
(`skinny/crates/bbnf-bench/src/report.rs:1976`,
`skinny/crates/bbnf-bench/src/report.rs:1980`). That covers stale and mixed
run-id false accepts. The computed run id is derived from the Criterion root
fingerprint (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:390`) and then emitted into row
telemetry (`skinny/crates/bbnf-bench/src/bin/gate.rs:489`).

No REDRESS route is reopened. SPEC Section 10 still blocks sidecar/permissive/
lossy/stale strict admission, `tape_vs_tape` or telemetry-only production
consumers, orphan primitives, Track 1/Track 2 coupling, and the named REDRESS
families (`restart/skinny/tranches/sk-v8/SPEC.md:758`,
`restart/skinny/tranches/sk-v8/SPEC.md:764`,
`restart/skinny/tranches/sk-v8/SPEC.md:775`). The P3-E ledger keeps the same
blocked route families and evidence requirements
(`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:42`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:89`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:103`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:133`).
Executable comparator checks reject stale/historical/absent freshness as strict
evidence, sidecar-same-run without a structured manifest, native comparator
source/plane drift, and unsupported comparators
(`skinny/crates/bbnf-bench/src/gate.rs:135`,
`skinny/crates/bbnf-bench/src/gate.rs:157`,
`skinny/crates/bbnf-bench/src/gate.rs:163`,
`skinny/crates/bbnf-bench/src/gate.rs:172`,
`skinny/crates/bbnf-bench/src/report.rs:1127`,
`skinny/crates/bbnf-bench/src/report.rs:1211`,
`skinny/crates/bbnf-bench/src/report.rs:1235`,
`skinny/crates/bbnf-bench/src/report.rs:1261`).

No behavior-surface drift found. The target diff after V6 touches only W0
packet docs and `skinny/crates/bbnf-bench/src/report.rs`; the broader W0 scope
from baseline to target is confined to the seven W0 report/gate/results/xtask
files recorded by the V7 cost fold. The frozen behavior-surface diff over
grammar input, runtime JSON/tape, SIMD, codegen, generated/product helpers,
Track 2, parity, scan, materialization, and SIMD scan hook is empty. This
matches the SPEC condition that the larger telemetry/report/gate scope is
admissible only while frozen behavior remains unchanged
(`restart/skinny/tranches/sk-v8/SPEC.md:335`,
`restart/skinny/tranches/sk-v8/SPEC.md:357`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:154`).

No schema-only close path found. `gate-json` adds the SK-V8 W0 telemetry note,
then exits invalid if `validate_schema_v3()` or `validate_sk_v8_w0()` fails
(`skinny/crates/bbnf-bench/src/bin/gate.rs:315`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:319`). The same W0 slice sets the
manifest consumer to `gate_only` (`skinny/crates/bbnf-bench/src/bin/gate.rs:495`)
and SPEC explicitly pre-blocks row-close claims from schema completion
(`restart/skinny/tranches/sk-v8/SPEC.md:363`). W1-W6 remain blocked until W0
closure and later wave gates (`restart/skinny/tranches/sk-v8/SPEC.md:807`,
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:238`).

## Commands And Evidence

- `git status --short`: clean before evidence and after test runs.
- `git rev-parse HEAD`: `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd`.
- `git rev-parse f452e837`: `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `git diff --name-status f452e837..ff6d09c6`: only V7 hardening docs were
  added under `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/`.
- `git show --stat --oneline --name-status f452e837`: target fold touches
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md`,
  `restart/skinny/tranches/sk-v8/SPEC.md`, and
  `skinny/crates/bbnf-bench/src/report.rs`.
- `awk` over `skinny/RESULTS.md`: 38 main rows; outcomes `16 S`, `1 L`,
  `14 N-direct`, `7 A`; verdicts `31 NO-GO`, `7 GO`; 38 manifest rows; all
  run ids `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`; all wave ids
  `SK-V8-open`; all deltas `baseline`; all consumers `gate_only`.
- Frozen behavior diff:
  `git diff --name-only 0bd16f6d..f452e837 -- skinny/grammars/json.bbnf skinny/crates/runtime/src/grammars/json skinny/crates/runtime/src/tape skinny/crates/bbnf-simd skinny/crates/codegen skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/materialization.rs skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/parse-that-regex/src/integration/simd_scan_hook`:
  empty output.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo test -p bbnf-bench w0_ -- --nocapture`
  from `skinny/`: passed 20 W0 tests, including report row identity,
  malformed sidecar, strict view-boundary, native comparator, sidecar source,
  profile placeholder, exact baseline, single stale run-id, and uniform stale
  run-id cases. An initial root-level invocation failed before compile because
  the repo root is not the Cargo workspace for `bbnf-bench`; it was rerun from
  `skinny/`.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo test -p bbnf-bench strict -- --nocapture`:
  passed 5 strict-admission tests.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo test -p bbnf-bench sidecar_same_run -- --nocapture`:
  passed the sidecar-same-run rejection test.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo xtask check-json`: passed.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo xtask check-real-typed`:
  passed.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo xtask check-conformance`:
  passed, `21 valid fixtures accepted; 7 invalid fixtures rejected`.
- `CARGO_TARGET_DIR=/tmp/skv8-v8-ch3-target cargo xtask gate-json --advisory --check-results`:
  failed closed before row admission with `twitter metadata invalid: missing
  Criterion metadata rows`. This was run against an external clean target dir,
  so it proves missing local Criterion evidence does not silently admit W0
  rows; it is not a fresh W0 measurement replay.

## Material Blockers

None found for CH3.

I did not find an admitted-row regression, a REDRESS route reopen, behavior
surface drift, stale/mixed run-id acceptance, schema-only close path, or frozen
behavior-surface drift.

## Residual Risks

- I did not run a fresh full Criterion `bench-json --advisory` capture under
  `RUSTFLAGS='-C target-cpu=native'`. This acceptance relies on the checked-in
  W0 manifest, exact validators, focused tests, and frozen behavior diff rather
  than a new full performance replay.
- The clean-target `gate-json --check-results` probe failed closed because no
  Criterion metadata rows existed in `/tmp/skv8-v8-ch3-target`. It did not
  exercise the local `skinny/target/criterion` state.
- V7 consolidated has a malformed expanded target SHA at line 5, but the V7 CH3
  artifact and `git rev-parse f452e837` both bind to
  `f452e8373ed717731dd5e720c1d947c086cc22c9`
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/CH3.md:4`).
  I treat that as prior-artifact citation hygiene, not a CH3 implementation
  blocker.
