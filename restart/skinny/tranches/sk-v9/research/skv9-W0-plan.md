# SK-V9 W0 Plan: Telemetry-Lock Recovery

Date: 2026-05-18.
Wave: W0.
Status: dispatch plan for redress.

## Objective

Produce and consume a coherent `SK-V9-open` telemetry manifest without behavior
movement. W0 repairs the post-S-P1 hardening blocker: the active report/gate
surface still names `SK-V8-open`, and advisory gate validation observed stale
SIMD metadata from a different capture. W0 does not admit rows, move throughput
as behavior evidence, or release behavior candidates.

## Owner Paths

Allowed source/report paths for this plan:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`, only if strict-admission refusal
  tests need adjustment.
- `skinny/crates/bbnf-bench/src/metadata.rs`, only if metadata coherence checks
  need adjustment.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, only if a telemetry-only
  classification update is required.
- `skinny/crates/bbnf-bench/benches/json_parity.rs`, only for metadata
  assertions.
- `skinny/crates/bbnf-bench/benches/simd_scan.rs`, only for SIMD diagnostic
  metadata assertions.
- `skinny/xtask/src/main.rs`, only for existing `RESULTS.md` marker strings in
  `validate_w0_results_snapshot`.
- `skinny/RESULTS.md`, only through `cargo xtask gate-json --advisory
  --update-results`.
- `skinny/REDRESS.md`, only if W0 rejects or routes a failed source attempt.
- `restart/skinny/tranches/sk-v9/SPEC.md`,
  `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`,
  `restart/skinny/tranches/sk-v9/HANDOFF.md`, and W0 close docs.

Freeze paths: parser, scanner, SIMD behavior, runtime, IR, passes, codegen,
fixtures, grammars, generated parser output, generated typed output,
`direct_struct.rs`, `real_typed_struct.rs`, Track 2 product logic, and non-JSON
grammar behavior.

## Required Redress Tasks

1. Change active report/gate labels from `SK-V8-open` to `SK-V9-open`.
2. Change the report run-id prefix to `sk-v9-open:criterion-fnv64-`.
3. Keep the existing Criterion fingerprint selection unless validation proves it
   stale or incomplete.
4. Regenerate or refresh the Criterion cache with
   `RUSTFLAGS="-C target-cpu=native"` so main JSON and SIMD metadata are from
   one capture.
5. Update `skinny/RESULTS.md` through the gate update path.
6. Keep row count at 38 and row identities unchanged.
7. Preserve typed/direct fences:
   - no Apache/CITM measured `real_typed_struct` rows in W0;
   - Canada typed remains rejected;
   - direct digest rows remain guard/control evidence.
8. Preserve diagnostic non-producer fences for structural scan, PMU,
   cycles-per-byte, masking probes, and Criterion slopes.

## Same-Wave Consumer

`gate-json` is the same-wave consumer. W0 fails if the report renders a
producer-only manifest or if `gate-json --advisory --check-results` cannot
consume the SK-V9-open manifest after the update.

## Verification Commands

Run from `skinny/` unless stated otherwise:

```bash
cargo fmt
cargo test -p bbnf-bench --lib --bins
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --update-results
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results
cargo xtask lint-loc
```

Repository-root diff checks:

```bash
git diff --check
git diff --exit-code -- skinny/crates/runtime skinny/crates/ir skinny/crates/passes skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/parse-that-regex skinny/grammars skinny/test_data skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs
```

`cargo xtask lint-loc` is currently expected to report pre-existing budget debt
in `crates/bbnf-bench` and `xtask`; W0 records that debt rather than hiding it.

## Falsifiability Gate

`G-W0-TELEMETRY-LOCK` PASS requires:

- `skinny/RESULTS.md` has `SK-V9-open` active telemetry and one uniform
  `sk-v9-open:criterion-fnv64-<16 hex>` run id.
- `gate-json --advisory --check-results` passes after update.
- Manifest row count remains 38.
- No frozen behavior path changes.
- No row additions, outcome upgrades, verdict upgrades, strict-admission
  upgrades, or typed/direct product shortcuts.
- Diagnostic rows remain non-producers.
- Verification evidence and any advisory debt are recorded in the close commit.

## Revert Protocol

If W0 touches a freeze path, weakens validation to pass stale/mixed evidence,
changes behavior, admits a blocked row, or cannot make `gate-json` consume the
SK-V9-open manifest, revert the W0 source/report/result slice and record a
REDRESS entry. Do not close W0 by prose or by leaving the manifest advisory-only.
