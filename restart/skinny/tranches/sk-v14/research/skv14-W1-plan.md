# SK-V14 Wave W1 Plan: Comparator Rebind, Equality Gate, And PRUNE-1

Date: 2026-05-24.
Wave: W1.
Phase: plan.
Inputs:
- `skv14-W1-A-comparator-rebind.md`
- `skv14-W1-B-per-iter-equality.md`
- `skv14-W1-C-prune1-json-revert-ledger.md`
- `skv14-W1-D-sonic-api-feasibility.md`
- `skv14-W1-E-gate-report-risk.md`
- `skv14-W1-F-owner-verification.md`

## 1. Intervention

W1 lands one integrated intervention: retire the DOM-bound parse_only
`sonic_rs_anchor`, introduce truthful plane-specific comparator identities,
add gate-consumed per-iteration equality requirements for future strict admits,
and PRUNE-1 all 22 audit-falsified JSON admits from the visible and rolling
surfaces.

The plan does not claim a new admit. The redress must keep W1 as a prune and
gate-hardening wave: if no fresh cold-compatible per-parse capture exists, W1
must leave equality as non-admit for reverted/open rows and make the gate reject
any later strict admission that lacks structured PASS equality.

## 2. Owner Paths

Authorized paths:
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/lib.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/sonic_skipper.rs` if a local wrapper is needed
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `skinny/REDRESS.md`

Forbidden paths:
- parser/runtime behavior outside the benchmark harness
- codegen behavior or generated parser output
- SIMD/asm/product behavior outside existing benchmark comparator wrappers
- fixture and corpus content
- unrelated dirty SK-V12/SK-V13 research JSON artefacts

## 3. Redress Steps

1. Add a local Skipper-class sonic wrapper that uses public sonic-rs 0.5.8
   serde deserialization into `serde::de::IgnoredAny` plus end-of-input
   validation. Do not cite a nonexistent public `sonic_rs::Skipper` type.
2. Rename the parse_only comparator lane from `sonic_rs_anchor` to a
   Skipper-class lane such as `sonic_rs_skipper`; remove direct source uses of
   `sonic_rs::from_slice::<sonic_rs::Value>` from the W1 comparator path.
3. Fix JSON manifest Track 2 entry-point helpers so direct and typed rows name
   `direct_struct::track2_digest` and `real_typed_struct::track2_typed`, not
   sonic comparator functions.
4. Add a structured per-iteration equality parser and gate rule. For any JSON
   row that is `A` / `GO` or rolling `ADMITTED` after W1, require
   `PASS:scope=criterion-timing;...;checks=<n>;mismatches=0`. Reject
   `legacy:*`, `not_admitted:*`, `startup:*`, `posthoc:*`, blank, zero checks,
   and non-zero mismatches for such rows.
5. Extend `xtask` SK-V14 manifest parsing to retain all 32 cells and reject
   hidden stale evidence: parse_only DOM comparator evidence, `sonic_rs_anchor`
   evidence, Track 2 equal to comparator functions, and `sidecar-same-run`
   without structured manifest.
6. PRUNE-1 the row-keyed 22 JSON population:
   - 5 parse_only: numbers, citm_catalog, canada, marine_ik, mesh.
   - 6 direct: citm_catalog, apache_builds, marine_ik, instruments, numbers,
     unicode_basic.
   - 11 typed: twitter, citm_catalog, apache_builds, github_events,
     update_center, mesh, random, marine_ik, instruments, numbers,
     unicode_basic.
7. Update `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` so
   JSON parse_only/direct/typed each show 0/17 admitted post-W1.
8. Append 22 row-keyed `skinny/REDRESS.md` entries. Cite validation-pack
   `v2 §1-4` for parse_only and `v6 §1 + §3` for direct/typed. Avoid the stale
   SPEC by-number mapping for direct rows.

## 4. Falsifiability Gate

- `rg "sonic_rs_anchor|from_slice::<sonic_rs::Value>" skinny/crates/bbnf-bench`
  returns no W1 comparator source hits.
- `cargo xtask gate-json --check-results --skv14-existing-results-capture`
  rejects any post-W1 JSON admit with placeholder equality, stale DOM
  parse_only comparator evidence, hidden Track2=comparator coupling, or
  `sidecar-same-run`.
- `cargo xtask gate-json --with-cost-facts --check-results` passes.
- `cargo test --profile ax-iter -p xtask -p bbnf-bench` passes.
- `restart/skinny/ROLLING-SOTA-DELTA.md` has zero JSON `ADMITTED` rows after
  W1.
- `skinny/RESULTS.md` has no JSON visible-table `A | GO` rows after W1; CSS
  rows remain W4 work.
- Invariants remain: 16 locks; Pattern H runtime file count 67; no diff under
  `crates/core/src/runtime`, `skinny/crates/codegen`, or generated output.

## 5. Hard Cap

Redress cap: 90 minutes. Commit at 0.9N with passing gate evidence or revert
and record W1 rejection.

## 6. Revert Protocol

If the comparator wrapper or manifest gate cannot be made truthful, revert all
W1 source and ledger edits together, preserve the research and plan artefacts,
and append a W1 rejection entry naming the missing comparator binding or
per-iteration equality gate. Do not leave a partial relabel.

## 7. Same-Wave Consumer

`xtask gate-json` is the same-wave consumer for the comparator/equality
manifest fields and PRUNE-1 ledger state. The benchmark harness is the producer
for future cold-compatible strict evidence; W1 closes only the gate and audit
revert, not new admission rows.

## 8. Pre-Blocked Routes

- DOM `sonic_rs::Value` as parse_only comparator.
- Treating PRUNE-1 as a new admit.
- `per_iter_equality=PASS` without measured timing-region evidence.
- Direct/typed Track 2 entries pointing to sonic comparator functions.
- Staging unrelated dirty tranche JSON artefacts.
