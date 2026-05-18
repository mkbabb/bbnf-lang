# SK-V9 W0 Close: Telemetry-Lock Recovery

Date: 2026-05-18.
Wave: W0.
Disposition: PASS.

## Result

`G-W0-TELEMETRY-LOCK` is closed. `skinny/RESULTS.md` now carries the active
`SK-V9-open` W0 telemetry manifest and `gate-json` consumes it through the
same-wave check path.

Run id:

```text
sk-v9-open:criterion-fnv64-cd1673844eeea12f
```

Manifest shape:

- 38 main rows.
- 17 `parse_only` rows, all `S / NO-GO`.
- 17 `direct_to_struct` rows: 3 `A / GO`, 14 `N-direct / NO-GO`.
- 4 `real_typed_struct` rows, all `A / GO`.
- No Apache/CITM/Canada measured `real_typed_struct` additions.
- All main rows remain `Strictness=deferred` and `parse_utf8=view-boundary`.
- Diagnostic surfaces remain fenced by
  `structural_scan+masking_probes+pmu+cycles:nonproducer`.

## W0 Redress Decisions

W0 repaired three gate conflicts discovered during same-wave consumption:

1. Criterion root coherence: `xtask` now normalizes `CARGO_TARGET_DIR` and
   propagates `CRITERION_HOME`, so the producer and consumer read one capture.
2. Fresh opening numbers: SK-V9-open is a fresh telemetry baseline. The W0
   validator locks row identities, metadata, verdict boundaries, and admission
   fences; it does not reject solely because fresh Mbps differs from old SK-V8
   constants.
3. No W0 admissions: fresh diagnostic no-go labels may relabel within
   diagnostic `NO-GO`, but direct digest rows that newly pass are clamped to
   their baseline `N-direct / NO-GO` state unless a later behavior wave admits
   them.

## Evidence

Passed:

```bash
cargo test -p bbnf-bench --lib --bins
cargo check -p xtask
RUSTFLAGS="-C target-cpu=native" CARGO_TARGET_DIR=target/skv9-w0 cargo xtask gate-json --advisory --update-results
RUSTFLAGS="-C target-cpu=native" CARGO_TARGET_DIR=target/skv9-w0 cargo xtask gate-json --advisory --check-results
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
cargo xtask gate-json --with-cost-facts --advisory --check-results
git diff --check
git diff --exit-code -- skinny/crates/runtime skinny/crates/ir skinny/crates/passes skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/parse-that-regex skinny/grammars skinny/test_data skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs
```

Expected advisory debt:

```text
cargo xtask lint-loc
crates/bbnf-bench: 9630/3300 LOC
xtask: 1263/650 LOC
```

The LOC failure is pre-existing budget debt and is not a W0 behavior movement.

## Dispatch Consequence

W0 is closed, but SK-V9 behavior waves are still blocked. The next executable
step is a fresh post-W0 S-P1 rerun against this `SK-V9-open` manifest, followed
by challenge convergence. No W1+ behavior dispatch is authorized until
`G-S-P1-RERUN-CONVERGED` and then `G-BEHAVIOR-RELEASE` pass.
