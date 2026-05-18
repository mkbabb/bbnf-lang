# CH5 W0 V6 Hardening Challenge

## Verdict

ACCEPT.

Confidence: 96%.

Reviewed target: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`
(`fix(sk-v8-wave0): fold hardening V5 row identity blockers`).

## Reviewed Surfaces

- CH5 lens and convergence rules: `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:81`, `restart/prompts/ORCHESTRATOR.md:87`,
  `restart/prompts/ORCHESTRATOR.md:104`, and
  `restart/prompts/ORCHESTRATOR.md:118`.
- SK-V8 W0 gates, strict comparator boundary, telemetry requirements, and
  pre-blocked hidden-coupling routes: `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:191`,
  `restart/skinny/tranches/sk-v8/SPEC.md:310`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`, and
  `restart/skinny/tranches/sk-v8/SPEC.md:731`.
- W0 dispatch and no-behavior boundary:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:78`, and
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`.
- Current W0 row and manifest surfaces: `skinny/RESULTS.md:3`,
  `skinny/RESULTS.md:44`, `skinny/RESULTS.md:87`, and
  `skinny/RESULTS.md:138`.
- V5 rejection fold target: `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:20`
  and `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:29`.
- W0 implementation: `skinny/crates/bbnf-bench/src/report.rs:493`,
  `skinny/crates/bbnf-bench/src/report.rs:646`,
  `skinny/crates/bbnf-bench/src/report.rs:928`,
  `skinny/crates/bbnf-bench/src/report.rs:1064`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:414`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:603`,
  and `skinny/crates/bbnf-bench/src/bin/gate.rs:673`.

## Findings

No material CH5 blocker found.

The V6 fold closes the V5 row-identity hole. `Report::validate_sk_v8_w0()`
requires exactly the `SK_V8_OPEN_BASELINE` row count, rejects duplicate or
unknown `row_id`, and now rejects per-row `outcome_id`, `verdict`, Track 1, or
Track 2 movement from the opening tuple
(`skinny/crates/bbnf-bench/src/report.rs:493`,
`skinny/crates/bbnf-bench/src/report.rs:501`,
`skinny/crates/bbnf-bench/src/report.rs:511`,
`skinny/crates/bbnf-bench/src/report.rs:517`,
`skinny/crates/bbnf-bench/src/report.rs:523`). The baseline row struct now
stores `row_id`, `outcome_id`, `verdict`, Track 1, and Track 2 as the identity
tuple (`skinny/crates/bbnf-bench/src/report.rs:646`), and the regression test
accepts the exact opening baseline while rejecting `twitter/parse_only` `S -> K`
and `twitter/direct_to_struct` `N-direct / NO-GO -> A / GO`
(`skinny/crates/bbnf-bench/src/report.rs:1897`,
`skinny/crates/bbnf-bench/src/report.rs:1944`,
`skinny/crates/bbnf-bench/src/report.rs:1949`,
`skinny/crates/bbnf-bench/src/report.rs:1958`).

The V6 fold also closes the run-id hidden-coupling hole found in V5. `run_id`
is built from `criterion_fingerprint()` (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:390`), and the fingerprint now admits
only files whose `(corpus, workload)` resolves to a W0 opening row via
`sk_v8_open_baseline()` (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:733`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:745`). The focused test proves
volatile probe files, `json_unvalidated_future`, and
`json_canada/sonic_rs_real_typed_struct` do not perturb the fingerprint, while a
real W0 row estimate does (`skinny/crates/bbnf-bench/src/bin/gate.rs:1769`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1781`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1794`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1800`).

No parallel substrate, sidecar producer, renamed scanner, parser-owned
cursor/fact slot, or Track 1 / Track 2 dishonesty landed in the V6 fold. The
commit diff touches only `skinny/crates/bbnf-bench/src/bin/gate.rs` and
`skinny/crates/bbnf-bench/src/report.rs`. The emitted W0 substrate facts remain
`borrowed_view_over_offset_tape` with cardinality `one` for parse rows, and
`sink_only_digest` / `typed_direct_projection` with `zero_or_inert` for
direct/typed rows (`skinny/crates/bbnf-bench/src/bin/gate.rs:603`). The report
still renders `same_wave_consumer_class=gate_only` and
`track2_independence_status=independent_verified`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:472`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:495`), matching the W0-only telemetry
scope in SPEC and DISPATCH.

Sidecar evidence remains isolated as historical or absent planning evidence.
Recognized sidecars reject `sidecar-same-run` without a structured manifest and
must carry exact historical/absence source strings
(`skinny/crates/bbnf-bench/src/report.rs:1203`,
`skinny/crates/bbnf-bench/src/report.rs:1221`,
`skinny/crates/bbnf-bench/src/report.rs:1227`,
`skinny/crates/bbnf-bench/src/report.rs:1233`). Native strict comparators remain
workload-plane-specific same-run anchors with `sidecar_freshness=n/a`
(`skinny/crates/bbnf-bench/src/report.rs:1253`,
`skinny/crates/bbnf-bench/src/report.rs:1277`,
`skinny/crates/bbnf-bench/src/report.rs:1289`,
`skinny/crates/bbnf-bench/src/report.rs:1295`).

## Commands And Evidence

- `git rev-parse HEAD && git status --short`: HEAD is
  `6c0bc15d44142abf0b965d9daee7070b1f32dd99`; status was clean before writing
  this artifact.
- `git diff --name-only 6c0bc15d^ 6c0bc15d`: only
  `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`.
- `awk` main-row parse of `skinny/RESULTS.md`: 38 rows; 16
  `parse_only:S:NO-GO`, 1 `parse_only:L:NO-GO`, 3 `direct_to_struct:A:GO`, 14
  `direct_to_struct:N-direct:NO-GO`, and 4 `real_typed_struct:A:GO`.
- `awk` manifest parse of `skinny/RESULTS.md`: 38 manifest rows; cardinality
  `one=17`, `zero_or_inert=21`; consumer `gate_only=38`; Track 2
  `independent_verified=38`.
- `cargo test -p bbnf-bench w0_ -- --nocapture`: PASS; 12 W0 report tests and
  8 W0 gate-bin tests passed.
- `cargo test -p bbnf-bench`: PASS; 52 library tests and 8 gate-bin tests
  passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`:
  PASS; rendered W0 report matched committed `skinny/RESULTS.md`.
- Dynamic fingerprint mutation probe on a copied Criterion root:
  `baseline=PASS`, `excluded=PASS`, `validated_mutation=REJECTED`. Added
  volatile `json_probes_twitter/.../estimates.json` and unvalidated
  `json_canada/sonic_rs_real_typed_struct/new/estimates.json` did not perturb
  the gate; appending whitespace to validated
  `json_twitter/track1_generated/new/estimates.json` made `--check-results`
  reject stale `RESULTS.md`.
- `git diff --check`: PASS.
- Frozen behavior root diff:
  `git diff --name-only 6c0bc15d^ 6c0bc15d -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/grammar skinny/crates/ir skinny/crates/passes skinny/crates/bbnf skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs`
  returned no paths.

## Material Blockers

None.

No repro rejects W0 under CH5. The exact row manifest and row-identity gates
held under unit tests, gate replay, RESULTS/manifest parsing, and dynamic
Criterion-root mutation.

## Residual Risks

- `Report::validate_sk_v8_w0()` checks `run_id` as required non-empty telemetry
  (`skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:297`) while exact fingerprint
  selection is enforced by the W0 producer and gate replay path
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`). That is sufficient for this
  W0 slice, but a later reusable report parser should not treat arbitrary
  externally constructed `SkV8Telemetry.run_id` strings as independently proven.
- The fingerprint uses FNV64 over sorted relative paths and file bytes
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:673`). This is an identity checksum
  for W0 telemetry stability, not a security hash.
- Track 2 independence remains test/checklist-backed for W0 telemetry and current
  behavior roots, including the existing Track 2 test
  `track2::json::tests::emits_track1_compatible_offsets_without_calling_track1_parser`
  observed in `cargo test -p bbnf-bench`. Any later W2/W3/W4 behavior admission
  still needs fresh same-wave Track 1 / Track 2 structural-independence proof.
