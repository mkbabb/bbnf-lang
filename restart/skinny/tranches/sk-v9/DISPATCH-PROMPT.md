# SK-V9 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration SK-V9.
It binds to the SK-V9 packet at `restart/skinny/tranches/sk-v9/`.

G-Alpha is closed. S-P1 V1 did not converge and is only an opening gap ledger.
The initial dispatch is W0 only. Do not dispatch behavior waves until
`G-W0-TELEMETRY-LOCK`, `G-S-P1-RERUN-CONVERGED`, and `G-BEHAVIOR-RELEASE` pass.

## Required Reading

Read in order:

1. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
2. `restart/prompts/skinny/PASS-1-PROFILE.md`.
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
5. `restart/skinny/tranches/sk-v9/SYNTHESIS.md`.
6. `restart/skinny/tranches/sk-v9/SPEC.md`.
7. `restart/skinny/tranches/sk-v9/HANDOFF.md`.
8. `restart/skinny/tranches/sk-v9/research/p1/`.
9. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`.
10. `restart/skinny/tranches/sk-v9/research/skv9-W0-r1-gate-report-baseline.md`.
11. `restart/skinny/tranches/sk-v9/research/skv9-W0-r2-criterion-metadata.md`.
12. `restart/skinny/tranches/sk-v9/research/skv9-W0-r3-diagnostic-fences.md`.
13. `restart/skinny/tranches/sk-v9/research/skv9-W0-r4-typed-direct-fences.md`.
14. `restart/skinny/tranches/sk-v9/research/skv9-W0-r5-lock14-redress.md`.
15. `restart/skinny/tranches/sk-v9/research/skv9-W0-r6-spec-dispatch-shape.md`.
16. `skinny/RESULTS.md`.
17. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Dispatch status | Hard cap |
|---|---|---|---|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | Dispatchable now | <=90 min |
| Interlock | Section 4 | Fresh S-P1 Rerun | Conditional after W0 | <=90 min |
| W1 | Section 5 | Revised S-P2/S-P3 Candidate Release | Blocked placeholder | <=90 min |
| W2 | Section 6 | Typed Row Admission Candidate | Blocked placeholder | <=90 min |
| W3 | Section 7 | Tape Plus Structural-Projection Candidate | Blocked placeholder | <=90 min |
| W4 | Section 8 | Direct Contract Candidate | Blocked placeholder | <=90 min |
| W5 | Section 9 | Close And Alpha Feedback | Blocked placeholder | <=90 min |

If requested wave is not W0 and `G-BEHAVIOR-RELEASE` is not PASS, refuse
dispatch and record why. A placeholder section is not dispatch authority.

## W0 Protocol

W0 is telemetry-only. It may update run identity, report labels, manifest
validation, replay metadata, diagnostic fences, and the existing RESULTS marker
checker. It must not move parser, scanner, SIMD behavior, runtime, IR, passes,
codegen, generated output, direct product behavior, real typed source/product
behavior, or strict admission.

Allowed owner paths are the paths named in `SPEC.md` Section 3. Any other source
path returns REVISE before editing.

Phase 1 - Research:

- Use the archived W0 research cohort under
  `restart/skinny/tranches/sk-v9/research/skv9-W0-*.md`.
- Add new research only if a W0 blocker is discovered.
- Research writes one artifact and does not edit source.

Phase 2 - Plan:

- Write or update one W0 plan artifact under
  `restart/skinny/tranches/sk-v9/research/`.
- The plan must name owner paths, freeze paths, exact `SK-V9-open` run-id
  method, same-wave consumer, verification commands, revert slice, and
  pre-blocked REDRESS routes.

Phase 3 - Redress:

- Implement only W0 telemetry/report/gate changes.
- Produce a fresh Criterion capture with `RUSTFLAGS="-C target-cpu=native"`.
- Update `skinny/RESULTS.md` only through the gate update path.
- Run the verification commands named in the W0 plan.
- PASS disposition admits W0 telemetry-lock.
- FAIL disposition records REDRESS and leaves behavior waves blocked.

## W0 Falsifiability Gate

`G-W0-TELEMETRY-LOCK` PASS requires:

1. `SK-V9-open` appears in the manifest and stale `SK-V8-open` run labels are
   absent from the active W0 section.
2. One uniform `sk-v9-open:criterion-fnv64-<16 hex>` run id is used across all
   manifest rows.
3. The manifest has exactly the current 38 main rows.
4. No Apache/CITM/Canada measured typed rows are added.
5. Direct digest rows stay digest guards, not product proof.
6. Structural-scan-only, masking probes, cycles-per-byte, PMU, and Criterion
   slope artifacts remain diagnostic non-producers.
7. `gate-json` consumes the manifest and rejects stale/mixed metadata.
8. Behavior freeze paths are unchanged.
9. Tests and gates named by the W0 plan complete, with any pre-existing
   advisory debt recorded.

## Conditional Release

After W0, run a fresh S-P1 profile cycle over the SK-V9-open baseline. Challenge
it. If hardening does not converge, W1+ remains blocked.

After S-P1 convergence, rerun or revise S-P2/S-P3 against fresh evidence. Only
then may a behavior wave receive a specific dispatch prompt. Behavior dispatch
must name row gates, owner paths, no-regression floors, same-wave consumer,
challenge status, REDRESS pre-blocks, revert slice, and verification commands.

## Always Blocked Unless A Future Accepted Plan Reopens

- New directive, BIR variant, substrate surface, `BackendShape`, `UnionTape`,
  public substrate API, sidecar substrate, parser-owned cursor/facts, or
  parallel substrate.
- Strict admission from `parse_only`, sidecar, permissive, lossy, stale,
  historical, absent, deferred, or view-boundary evidence.
- Apache/CITM measured typed row admission from source/product parity alone.
- Canada typed admission from length, digest, schema, field-count, coordinate,
  or partial-fixture evidence.
- Direct digest as typed product proof.
- Structural projection without a single-substrate proof and same-wave retained
  hot-path consumer.
- PMU, masking probes, Criterion slopes, or cycles-per-byte as behavior
  producers.

## Status Discipline

Before a status reply, reconcile agent status, running cargo/rustc processes,
artifact mtimes, and dirty worktree state. Keep research, plan, challenge,
redress, and close artifacts separated in commits.
