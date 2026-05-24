# SK-V14 W0 Plan: Baseline Profile And Telemetry Lock

Date: 2026-05-24.
Wave: W0.
Phase: plan.
Research inputs:
- `skv14-W0-A-telemetry-schema.md`
- `skv14-W0-B-audit-overlay.md`
- `skv14-W0-C-sidecar-freshness.md`
- `skv14-W0-D-lock14-baseline.md`
- `skv14-W0-E-open-capture.md`
- `skv14-W0-F-verification-map.md`

## 1. Decision

W0 will extend the existing RESULTS report through a gate-consumed
SK-V14 manifest, not by widening the visible 26-column table.
The visible table remains the historical throughput surface; the new manifest
becomes the authoritative W0 telemetry carrier consumed by
`cargo xtask gate-json --check-results`.

The SK-V14 manifest must cover exactly 75 rows:
- 51 JSON cells from the rolling row inventory.
- 24 CSS L4 target rows from the rolling row inventory.

Every manifest row carries:
- `row_id`
- `track1_entry_point`
- `track2_entry_point`
- `comparator_plane`
- `per_iter_equality`
- `audit_overlay_verdict`
- `audit_overlay_reference`
- `substrate_target`
- `retention_lifetime`
- `policy_owner`
- `sidecar_freshness`
- `sk_v14_open_delta`

The W0 implementation is schema/gate/report only. It must not demote old
admitted rows, remove CSS templates, change parser/runtime/codegen behavior,
or rerun warm Criterion benches as the capture method. PRUNE-1 and PRUNE-2
remain W1 and W4 work.

## 2. Redress Scope

Authorized owner paths:
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`

Disallowed W0 paths unless the wave rejects and records REDRESS:
- parser/runtime behavior under `skinny/crates/runtime`
- codegen behavior or generated output under `skinny/crates/codegen`
- SIMD/asm/product behavior
- fixture or corpus content
- `skinny/REDRESS.md`

## 3. Implementation Steps

1. Rename the W0-facing report identity from SK-V9/SK-V13 residue to
   `SK-V14-open` while preserving the committed throughput cells as the
   capture seed.
2. Extend the report telemetry struct and renderer with the SK-V14 manifest
   fields listed in Section 1.
3. Populate JSON manifest rows from the generated report data and add explicit
   manifest rows for the six missing typed JSON cells and 24 CSS L4 target rows.
4. Populate `audit_overlay_verdict` as:
   - `AUDIT-FALSIFIED` for 5 parse_only, 6 direct, 11 typed, and 24 CSS rows.
   - `AUDIT-PENDING` for the remaining 29 JSON rows.
   - no `AUDIT-SUSTAINED` in W0.
5. Populate `comparator_plane` as W0 binding metadata:
   - parse_only: `sonic_rs::Skipper`
   - direct: `<corpus>::strict_struct_deser`
   - typed: `<corpus>::typed_strict_struct_deser`
   - CSS L4: `lightningcss full-parse`
6. Populate `per_iter_equality` as a non-admit W0 placeholder for rows that
   cannot honestly claim R2 yet; strict `PASS` enforcement for admits belongs
   to W1/W8/W9/W10 after the measured equality harness exists.
7. Populate Lock 1 v+1 triple values using the locked vocabulary in
   `LOCKS.md`.
8. Keep historical and absent sidecars non-anchor; reject `sidecar-same-run`
   until a structured sidecar manifest parser exists.
9. Add the Lock 14 companion lint to reject new generated-header introductions
   outside the W0 baseline/recognized emission roster.
10. Extend `validate_w0_results_snapshot` so `--check-results` consumes the
    SK-V14 manifest and rejects missing fields, unsupported enum values,
    duplicate/missing row ids, stale sidecar strict claims, and rowset drift.

## 4. Falsifiability Gates

- `G-W0-ROWSET-75`: the manifest has exactly 51 JSON cells and 24 CSS L4 rows.
- `G-W0-SCHEMA-PRESENCE`: every manifest row has all SK-V14 fields populated.
- `G-W0-AUDIT-OVERLAY`: the falsified/pending counts are 46/29/0 sustained.
- `G-W0-SIDECAR-FRESHNESS`: `sidecar-same-run` rejects without a structured
  same-run manifest; absent sidecars use `absent:<reason>`.
- `G-W0-TRACK2-ENTRY`: Track 2 entry points are present and do not collapse to
  Track 1 private runtime tape internals.
- `G-W0-LOCK1-TRIPLE`: Lock 1 triple values are in the locked vocabularies.
- `G-W0-GENERATED-HEADER`: a new generated-header token under the locked roots
  rejects unless rostered.
- `G-W0-NO-BEHAVIOR-DIFF`: final diff is confined to W0 report/gate/docs paths.

## 5. Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` with one cargo target:

```bash
export CARGO_TARGET_DIR=/tmp/bbnf-skv14-w0-target
export CRITERION_HOME=/tmp/bbnf-skv14-w0-target/criterion
export RUSTFLAGS="-C target-cpu=native"
mkdir -p /tmp/bbnf-skv14-w0-logs
cargo test --profile ax-iter -p xtask -p bbnf-bench 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-unit-schema.log
cargo xtask gate-json --check-results 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-gate-json-check-results.log
cargo xtask gate-json --with-cost-facts --check-results 2>&1 | tee /tmp/bbnf-skv14-w0-logs/g-w0-gate-json-costfacts.log
```

Run from `/Users/mkbabb/Programming/bbnf-lang`:

```bash
grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --name-only -- skinny/crates/bbnf-bench skinny/xtask/src skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md restart/skinny/tranches/sk-v14/research
git diff --name-only -- skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-simd skinny/crates/ir skinny/crates/passes crates/core/src/runtime
git diff -U0 -- skinny/crates/runtime/src/grammars skinny/crates/codegen/src | rg '^\+.*@generated by skinny bbnf-codegen'
```

Expected invariant outputs:
- Lock count: 16.
- Pattern H runtime file count: 67.
- Behavior-freeze diff: empty.

## 6. Revert Protocol

If any W0 gate fails intrinsically, revert the W0 implementation commits
together, restore the opening RESULTS schema/report state, and record a W0
REDRESS rejection naming the missing field, rowset mismatch, sidecar failure,
generated-header failure, or behavior leak.
