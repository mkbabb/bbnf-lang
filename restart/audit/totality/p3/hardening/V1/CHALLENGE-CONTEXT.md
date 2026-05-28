# T-P3 V1 CHALLENGE Context

Target packet: `0a0508acd` (`docs(sk-v15-t-p3): add V1 synthesis packet`).
Cycle: `V1`.
Output directory: `restart/audit/totality/p3/hardening/V1/`.

## Authority

1. `restart/prompts/totality/PASS-3-SYNTHESIS.md`.
2. `restart/prompts/ORCHESTRATOR.md` §3W/§3Z.
3. `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md`.
4. T-P3 V1 target artifacts:
   - `restart/audit/totality/p3/3A-architecture-synthesis.md`
   - `restart/audit/totality/p3/3B-master-plan-reconciliation.md`
   - `restart/audit/totality/p3/3C-locks-crystallisation.md`
   - `restart/audit/totality/p3/3C-locks-v+1-diff.md`
   - `restart/audit/totality/p3/3D-skinny-fold.md`
   - `restart/audit/totality/p3/3E-grammar-generalisation.md`
   - `restart/audit/totality/p3/3F-migration-handoff.md`
5. Evidence base:
   - `restart/audit/totality/p1/`
   - `restart/audit/totality/p2/`
   - `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
   - `restart/ARCHITECTURE.md`
   - `restart/MASTER-PLAN.md`
   - `restart/locks/LOCKS.md`
   - `restart/HANDOFF.md`
   - `restart/MIGRATION.md`
   - `restart/skinny/tranches/sk-v15/{SYNTHESIS,SPEC,HANDOFF,DISPATCH-PROMPT}.md`
   - `skinny/REDRESS.md`
   - `skinny/RESULTS.md`

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
directive, owner, and severity. Hard cap: 90 minutes. V1 all-ACCEPT is allowed
only if defended with executable checks; PASS-3 warns that V1 all-ACCEPT is a
paper-close smell.

- `CH1.md` CORRECTNESS: verify every proposed delta cites real T-P1/T-P2/PASS-
  IMPL/SK-V15/V1 evidence; every cited path resolves; 3C covers every live 1E
  and 2X amendment candidate; extracted `3C-locks-v+1-diff.md` applies cleanly
  to current `LOCKS.md`.
- `CH2.md` GENERALITY: verify Lock 14 holds, no JSON narrowing, non-JSON
  receivers are concrete for CSS L4 plus Sheets or BBNF-self, and no proposed
  delta adds a directive, BIR variant, substrate, public substrate API,
  retained sidecar, or sixth `BackendShape`.
- `CH3.md` REGRESSION: verify no REDRESS route is reopened, stale SK-V14/SK-V13
  receiver blocks are not revived as current dispatch, delete-before-provider
  sequences remain blocked, and dependency rows precede deletion/retirement.
- `CH4.md` COST: verify every delta states LOC budget, propagation count/risk,
  wave alignment, and same-wave consumer or gate. Flag hidden W12 overflow,
  unbounded CSSOM rewrites, or wide edits disguised as doc-only gates.
- `CH5.md` HIDDEN COUPLING: verify no parallel substrate, sidecar producer,
  renamed-scanner Lock 1 violation, Track 1 == Track 2 dishonesty, fact-stream
  as `BackendShape`, runtime regex substrate, or x86 diagnostic evidence as
  aarch64 close evidence.
- `CH6.md` ANTI-PAPER-CLOSE: verify no artifact claims closure from prose, no
  engineered deferral, no uncited validation claim, no G3/G-Omega confusion,
  and every open question has a receiver, blocker, and gate.
- `CH7.md` OVERFIT-PRUNE: apply the SK-V15 forward-lens addenda. Specifically
  scan for wave-graph cycles, broadcast admission, gate exclusions, CSS fake
  parity, wrong-host close evidence, FNV bench leakage, and
  delete-before-provider sequencing.

## Required Local Checks

Run and cite results where relevant:

```sh
git show --stat --oneline 0a0508acd -- restart/audit/totality/p3
git diff --check 0a0508acd^ 0a0508acd -- restart/audit/totality/p3
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
git apply --check /tmp/tp3-locks-v1.diff
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

Expected invariant outputs at target context: 16 numbered locks and 67 Pattern H
runtime files. Any mismatch is `REJECT` unless the agent proves unrelated dirty
state is responsible and names the exact file.

## Consolidation Rule

The orchestrator will aggregate CH1-CH7 into
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md`.
For §3Z, V1 can be clean-cycle 1 only if all seven lenses return `ACCEPT`,
there are no orphan `REVISE` items, and no target-packet edit is required. Any
`REVISE` or `REJECT` forces a V2 synthesis fold before further challenge.
