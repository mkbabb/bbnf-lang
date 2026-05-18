# SK-V8 W0 Verification Plan Research

Role: W0 research agent F. Scope is verification planning only. W0 is
telemetry-only, with implementation owner paths limited to `skinny/crates/bbnf-bench/`,
`skinny/xtask/src/`, `skinny/RESULTS.md`, W0 research artifacts, and
`skinny/REDRESS.md` only on rejection.

## Required Proof Surface

W0 must prove four things without admitting behavior work:

1. Every current main row has gate-consumed SK-V8 telemetry: Section 0.4 fields,
   hot leaf/profile artifact, sample cost, sample count, host/build/feature data,
   run id, wave id, sidecar freshness, and `SK-V8-open` delta.
2. `gate-json` rejects bad evidence in the same W0 slice: missing required fields,
   placeholder hot leaves, missing run/profile/delta, unsupported outcomes,
   malformed sidecar manifest, stale sidecar strict claims, and strict-admission
   rows failing Section 0.2 comparator discipline.
3. No parser, scanner, SIMD, asm, codegen, generated parser, or product-plane
   behavior source changed.
4. `skinny/RESULTS.md` is the deterministic W0 gate output: 38 current main
   rows, 17 `parse_only` rows remain substrate-guard non-admission (`K`, or `S`
   if W0 amends the schema), four current `real_typed_struct` GO rows remain GO,
   populated sidecar cells have manifest coverage, missing sidecars carry
   `sidecar_freshness=absent:<reason>`, and all throughput cells are within
   +/-1.0% of `SK-V8-open`.

## Smallest Focused Command Set

Run from `skinny/` unless a command explicitly names repository-root paths.

1. Focused bench/gate/report tests:

```sh
cargo test -p bbnf-bench
```

Expected signal: all existing parity/direct/typed/report/gate tests pass, plus
new W0 negative tests prove missing telemetry, malformed sidecar manifests,
stale strict sidecar claims, unsupported outcomes, and parse-only strict GO
claims reject before throughput admission.

2. Generated-output drift checks:

```sh
cargo xtask check-json
cargo xtask check-real-typed
```

Expected signal: both commands exit 0 with generated outputs matching checked-in
sources. These are enough with the diff guard below; do not spend W0 budget on
full workspace tests or `primitive-checkasm` unless a W0 edit actually touches
those paths, which should reject the slice.

3. Full W0 measurement and gate update:

```sh
cargo xtask bench-json --advisory
```

Expected signal: one full Criterion capture writes metadata sidecars and then
runs the W0-updated `gate-json` path. Advisory mode may leave current
`N-direct`/`K` row outcomes intact, but must not allow hard W0 failures:
missing telemetry, schema invalidity, parity/hash failures, malformed sidecar
coverage, stale strict evidence, unsupported outcomes, or > +/-1.0%
`SK-V8-open` movement.

4. RESULTS idempotence after the measurement:

```sh
before=$(shasum -a 256 RESULTS.md)
cargo xtask gate-json --advisory >/tmp/sk-v8-w0-gate-json.out
after=$(shasum -a 256 RESULTS.md)
test "$before" = "$after"
```

Expected signal: second `gate-json` exits 0 and the RESULTS checksum is
unchanged, proving the working `RESULTS.md` content is exactly the gate
rendering over the captured sidecars. The captured output should name the
38-row W0 validation; malformed-sidecar rejection is proved by the focused test
command above.

5. No behavior-source drift from the repository root:

```sh
git diff --exit-code -- \
  skinny/crates/bbnf \
  skinny/crates/bbnf-simd \
  skinny/crates/codegen \
  skinny/crates/grammar \
  skinny/crates/ir \
  skinny/crates/parse-that-regex \
  skinny/crates/passes \
  skinny/crates/runtime \
  skinny/crates/simd-scan \
  skinny/crates/test-fixtures \
  skinny/grammars \
  skinny/test_data
```

Expected signal: no diff. Any diff in these paths rejects W0 even if the gate is
green. The only allowed non-research W0 diffs are `skinny/crates/bbnf-bench/`,
`skinny/xtask/src/`, `skinny/RESULTS.md`, and `skinny/REDRESS.md` on rejection.

## Tests To Add Or Update

- Update `metadata::tests::row_metadata_has_required_fields` or add
  `metadata::tests::w0_metadata_contains_required_telemetry` to cover every W0
  required field emitted by bench sidecars, including run id, wave id, host/build
  facts, feature mask, sample cost/count, sidecar freshness, and profile artifact.
- Add `report::tests::w0_results_manifest_round_trips_required_fields` to prove
  rendered rows or the gate-consumed manifest preserve Section 0.4 telemetry and
  `SK-V8-open` deltas for parse/direct/real-typed workloads.
- Add `report::tests::w0_schema_rejects_missing_delta_or_profile` by extending
  the current missing-comparator validation pattern.
- Add `gate::tests::w0_gate_rejects_missing_required_telemetry`.
- Add `gate::tests::w0_gate_rejects_unsupported_outcome`.
- Add `gate::tests::w0_gate_rejects_malformed_sidecar_manifest`.
- Add `gate::tests::w0_gate_rejects_stale_sidecar_strict_claim`.
- Add `gate::tests::w0_parse_only_cannot_admit_strict_sota_go`.
- Add an idempotence-style unit or integration helper for the W0 report reader
  if `gate-json` gains a machine-readable manifest; it should assert exactly 38
  current main rows and matching RESULTS/manifest row ids.

## Rerun And Timeout Budget

Hard budget: W0 implementation plus measurement plus rollback is 90 minutes.

- `cargo test -p bbnf-bench`: 5 minute timeout. No rerun on assertion failure;
  fix or revert the W0 slice.
- `cargo xtask check-json` and `cargo xtask check-real-typed`: 2 minutes each.
  No rerun on failure; any generated drift is a W0 blocker.
- `cargo xtask bench-json --advisory`: 65 minute timeout. Permit at most one
  full rerun only for obvious environmental noise, and only if the first run
  leaves enough wall-clock budget for gate/idempotence/diff checks and rollback.
  Do not rerun missing-telemetry, malformed-sidecar, unsupported-outcome, parity,
  or source-drift failures.
- RESULTS idempotence `gate-json`: 8 minute timeout. No rerun if checksum changes;
  that is a RESULTS consistency failure.
- Diff guard: 1 minute. Any forbidden diff rejects W0.

Reserve at least 10 minutes for rollback or `skinny/REDRESS.md` if W0 rejects.
If the 90 minute cap cannot contain the full command set, W0 should reject with
REDRESS rather than narrow the evidence.
