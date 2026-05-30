# Omega-C Locks Amendments — Pass Omega V5 SK-V17 Tape-Fold

Pass: Pass Omega V5 (SK-V17 T-P3 tape-fold CRUD application).
Gate: G-Omega CLOSED by user this turn.
Master HEAD at apply: `2a76916ac`.
Commit: `7157be073`.
Scope: apply the LOCKED T-P3 §3Z LOCKS v+1 deltas
(`restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md`) to
`restart/locks/LOCKS.md`.

This Omega-C note is a DISTINCT Pass Omega V5 leg from the SK-V14 W5R
no-op recorded in `ΩC-locks-amendments.md`. The SK-V14 W5R leg required no
LOCKS amendment; this SK-V17 tape-fold leg applies a real LOCKS v+1 addendum.

## Verdict

`LOCKS.md` amendment APPLIED. The `## SK-V17 T-P3 Crystallisation Addendum`
crystallises the five LOCKED T-P2 fold designs (`LAC-2F-FOLD-01..05`) and their
six T-P1 antecedents (`LAC-1E-SKV17-01..06`) into five addendum clauses on
Locks 1, 2, 10, 14, and 16. These are AMENDMENTS to existing locks, NOT new
locks. The hunk inserted between the SK-V15 addendum's Lock 16 clause and the
`## v+1 Governance Boundary`.

## Per-Clause Disposition

| Source delta | Lock | Clause | Disposition |
|---|---|---|---|
| D-SKV17-L01 | Lock 1 | tape-substrate-union | Retires live eager `OpenFrame` builders into flat-tape commit-by-construction; converges AoS `TapeRec` onto PROVEN-AND-BENCHED SoA `Tape<'input>` as the SINGLE post-fold encoding; declares `substrate_target` on all 8 `OnceCell<StructuralIndex>` carriers. Per-leaf runtime `StructRegistry::layout` indirection REJECT. |
| D-SKV17-L02 | Lock 2 | StructLayout-reconcile | Prices the `StructLayout`→`Layout` reconcile by two disjoint paths (960-site rename vs `LayoutFacts.backend_shape` side-table); neither chosen in-lock; route selection is an SK-V18 wave decision. |
| D-SKV17-L10 | Lock 10 | tape-category | Tape folds in as the SUBSTRATE the five `BackendShape` shapes project from (`substrate_target = existing_tape`), per LAC-1E-14 FactStream precedent — NOT a 6th `BackendShape` variant. Five-shape search domain held verbatim. |
| D-SKV17-L14 | Lock 14 | ValueRef/classifier-generalisation | Lazy grammar-parametric `ValueRef<'doc,'input,K,G:EventGrammar>` is the ONE materialization plane, re-emitted by a single grammar-agnostic generator. SCOPE-HONEST: value-fold exercised JSON+CSS ONLY. preserve-rich-ast holds. |
| D-SKV17-L16 | Lock 16 | NEON-classifier-manifest | Shared NEON classifier registered as a Lock-16 primitive-manifest ROW with `retention_lifetime = transient-single-call`, `substrate_target = existing_tape`, checkasm under `BBNF_SIMD_STRICT=1`; aarch64 primary, no SVE, no x86 close path. |

## Verification (post-apply)

`grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md` returns `16` (PRESERVED).

The five-shape `BackendShape` canon remains verbatim, NO 6th:

```text
{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
```

`grep -cF "{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}"` = 6
(five prior + the addendum); `grep -nE "EagerTape, OffsetTape, EventTape,
SinkOnly, CollapsedStage,[A-Za-z]"` is EMPTY (no six-variant tuple).

The tape is recorded as a substrate-manifest CATEGORY (`substrate_target =
existing_tape`), not a new substrate and not a sixth shape (LAC-1E-14
precedent).

## Residual Absorption (2 non-blocking REVISE)

- **CH4-V3-01** (D07 scaffold->body cost-cell band): a MASTER-PLAN cost-table
  surface concern, not LOCKS; rides forward to the cost-table CRUD leg.
- **CH6-V3-7** (3E defer-word re-order + 3C anti-silent-satisfy clause): the
  3C anti-silent-satisfy clause is absorbed by the gate object's distribution
  invariant — the Lock 10 clause's mandatory inline cross-reference to the
  Lock 1 substrate manifest — which the applied addendum carries verbatim.

## No Re-Opened REDRESS

The addendum introduces no AZ-IV eager path, no `StructRegistry` runtime
indirection, and no retained fact-stream sidecar. Each is explicitly REJECT in
the applied Lock 1/14 clauses.

## Final Omega-C Determination

The SK-V17 LOCKS v+1 addendum is in force under the `## v+1 Governance
Boundary`. The 16-lock count, the five `BackendShape` variants, the
tape-as-substrate-category placement, aarch64-only posture, and preserve-rich-ast
are all preserved. CRUD-3 LOCKS complete at `7157be073`.
