# T-P3 V4 Confirmation CHALLENGE Context

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Prior clean cycle: `cc815640c` (`docs(sk-v15-t-p3): record V3 hardening clean cycle`).
Cycle: `V4`.
Output directory: `restart/audit/totality/p3/hardening/V4/`.

## Authority

1. `restart/prompts/totality/PASS-3-SYNTHESIS.md`.
2. `restart/prompts/ORCHESTRATOR.md` §3W/§3Z.
3. `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md`.
4. T-P3 V3 target artifacts:
   - `restart/audit/totality/p3/3A-architecture-synthesis.md`
   - `restart/audit/totality/p3/3B-master-plan-reconciliation.md`
   - `restart/audit/totality/p3/3C-locks-crystallisation.md`
   - `restart/audit/totality/p3/3C-locks-v+1-diff.md`
   - `restart/audit/totality/p3/3D-skinny-fold.md`
   - `restart/audit/totality/p3/3E-grammar-generalisation.md`
   - `restart/audit/totality/p3/3F-migration-handoff.md`
5. Prior hardening:
   - `restart/audit/totality/p3/hardening/HARDENING-T-P3-V3-CONSOLIDATED.md`
   - `restart/audit/totality/p3/hardening/V3/CH1.md` through `CH7.md`

## Confirmation Purpose

V3 was clean-cycle 1. V4 is a confirmation challenge against the same target
packet. It must independently re-run the seven lenses and required local checks;
do not rubber-stamp V3. If V4 returns seven `ACCEPT` lenses with zero orphan
`REVISE`, T-P3 reaches normal §3Z lock: two consecutive clean cycles, zero orphan
`REVISE`, and V≤5.

## Locked Ground Truth

- T-P3 proposes only. Any live edit to `ARCHITECTURE.md`, `MASTER-PLAN.md`,
  `LOCKS.md`, `HANDOFF.md`, or `MIGRATION.md` before Pass Omega CRUD is a
  boundary fault.
- T-P1 is `CLEAN-FINAL / G1-AUTO-PINNED`, not normal two-clean-cycle §3Z.
- T-P2 is normal §3Z locked at `cafb95682`.
- G3 auto-passes only after T-P3 §3Z lock under the active user pin; G-Omega is
  the only mandatory user gate.
- The 16 numbered locks and exact five `BackendShape` variants
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` must remain
  intact. `FactStream` is output-plane/category language, not a sixth shape.
- SK-V15 remains PRUNE-before-REBUILD. T-P3 cannot authorize implementation
  waves directly.

## Lens Assignments

Each CH agent writes exactly one file in this directory and does not stage or
commit. Use `ACCEPT`, `REVISE`, or `REJECT`. Every defect must cite the target
artifact path and line, the conflicting evidence path and line, a repair
directive, owner, and severity. Hard cap: 90 minutes.

- `CH1.md` CORRECTNESS: independently verify evidence citations, path
  resolution, path:line range, 3C live 1E/2X coverage, extractable LOCKS diff,
  and absence of target-packet edits outside the seven T-P3 proposal artifacts.
- `CH2.md` GENERALITY: verify Lock 14 holds, no JSON narrowing, concrete
  non-JSON receivers, and no forbidden directive/BIR/substrate/public API/
  sidecar/sixth-shape addition.
- `CH3.md` REGRESSION: verify no REDRESS route reopened, stale receiver blocks
  stayed historical/pre-block, delete-before-provider stayed blocked, and
  V3/V4/V5/V6/V7/V8 corrective gates are preserved.
- `CH4.md` COST: independently verify the V3 CH4 coverage matrices close
  `CH4-V2-001` and `CH4-V2-002` without W12, broad CSSOM parity,
  challenge-time implementation overflow, or doc-only implementation gates.
- `CH5.md` HIDDEN COUPLING: verify no parallel substrate, sidecar producer,
  renamed-scanner Lock 1 violation, Track 1 == Track 2 dishonesty, fact-stream
  shape leak, runtime regex/DFA substrate, or x86 close evidence.
- `CH6.md` ANTI-PAPER-CLOSE: verify no prose closure, engineered deferral,
  uncited validation, G3/G-Omega confusion, CRUD-4 cleanup loophole, or unrouted
  open question remains.
- `CH7.md` OVERFIT-PRUNE: scan for wave-graph cycles, broadcast admission, gate
  exclusions, CSS fake parity, wrong-host close evidence, FNV bench leakage,
  delete-before-provider sequencing, and self-exempting grep gates.

## Required Local Checks

Run and cite results where relevant:

```sh
git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3
git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v4.diff
git apply --check /tmp/tp3-locks-v4.diff
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
rg -n 'ORCHESTRATOR-PROMPT|2F-parse-that-gaps\.md:518|owner \(`bbnf-regex`|`bbnf-regex`, `bbnf-simd`|follow-up docs-only cleanup|why still open|why it remains open|receiving gate|re-entry trigger|receiver / blocker$|runtime regex engines remain inadmissible without' \
  restart/audit/totality/p3/3A-architecture-synthesis.md \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3C-locks-crystallisation.md \
  restart/audit/totality/p3/3C-locks-v+1-diff.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3E-grammar-generalisation.md \
  restart/audit/totality/p3/3F-migration-handoff.md
```

Expected invariant outputs at target context: 16 numbered locks, 67 Pattern H
runtime files, and no stale-pattern matches. Any mismatch is `REJECT` unless the
agent proves unrelated dirty state is responsible and names the exact file.

## Consolidation Rule

The orchestrator will aggregate CH1-CH7 into
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V4-CONSOLIDATED.md`.
V4 locks T-P3 only if all seven lenses return `ACCEPT`, there are no orphan
`REVISE` items, and no target-packet edit is required.
