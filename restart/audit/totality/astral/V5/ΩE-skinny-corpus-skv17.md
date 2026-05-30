# Ω-E Skinny Corpus — Pass Omega V5 SK-V17 Tape-Fold (T-P3 §3Z apply)

Pass: Pass Omega V5 (SK-V17 T-P3 tape-fold CRUD application).
Source: T-P3 §3Z convergence, commit chain to `2a76916ac`; LOCKS v+1
addendum applied at `7157be073` (CRUD-3 LOCKS leg).
Date: 2026-05-30. Gate: G-Omega CLOSED by user this turn.

This is a DISTINCT Pass Omega V5 leg from the SK-V14 W5R cycle's CRUD-5
(`ΩE-skinny-corpus.md` precedent; this note mirrors its structure). CRUD-5
SK-V17 propagates the 3D skinny-fold + 3E grammar-generalisation deltas into
the six skinny corpus surfaces, preserving the monotonic skinny→totality fold
direction and the pre-existing dirty SK-V12/13 research JSON + skinny
`css_l4_*` generated.rs untouched.

## Per-surface delta log

| Surface | Lines BEFORE | Lines AFTER | Δ | SK-V17 receiver added |
|---|---:|---:|---:|---|
| `restart/skinny/INDEX.md` | 198 | 217 | +19 | SK-V17 fold state (3D-D07/D08) |
| `restart/skinny/SUBSTRATE.md` | 778 | 820 | +42 | tape-as-unified-substrate + ValueRef<G> (3D-D01/D02, 3E-D01/D02/D09) |
| `restart/skinny/COMPILER.md` | 1024 | 1074 | +50 | shared NEON classifier + single generator (3D-D02/D03/D07, 3E-D04/D06/D07/D08) |
| `restart/skinny/BENCH.md` | 2255 | 2283 | +28 | canonical N≥50 harness + lightningcss fair bar (3D CSS >SOTA non-fit, 3E P5a/P5b) |
| `restart/skinny/HARDENING.md` | 234 | 252 | +18 | fold-scope honesty firewall (3D BANNER, 3E-D05/D07) |
| `restart/skinny/WORKSPACE.md` | 738 | 756 | +18 | SK-V17 fold state (3D-D08, 3E-D08) |
| **Totals** | **5227** | **5402** | **+175** | 6 surfaces |

## Delta → surface mapping (per the CRUD-5 brief)

| brief target | source 3X delta | surface carrier |
|---|---|---|
| SUBSTRATE ← tape-as-unified-substrate + `ValueRef<G>` | `3D-SK17-D01` (SoA `Tape` authoritative), `3D-SK17-D02` (lazy `ValueRef<G>`), `3E17-D01` (type-param vehicle), `3E17-D02` (grammar-column-free fence), `3E17-D09` (compile-time `FieldSource`) | SUBSTRATE.md SK-V17 substrate receiver, 4 numbered clauses |
| COMPILER ← shared NEON classifier + single generator | `3D-SK17-D03` (classifier authoritative), `3D-SK17-D02` (one generator), `3E17-D04` (alphabet-as-data manifest), `3E17-D06` (CSS eq-set-fan consumer), `3E17-D07` (by-construction scoping), `3E17-D08` (onboarding predicates + leak census) | COMPILER.md SK-V17 compiler receiver, 4 numbered clauses + 5-shape canon reaffirmation |
| BENCH ← canonical N≥50 harness + lightningcss fair bar | `3D-SK17` CSS `>SOTA` non-fit row + SCOPE-HONESTY BANNER, `3E17` P5a/P5b split | BENCH.md SK-V17 bench receiver, 3 numbered clauses |
| INDEX ← SK-V17 fold state | `3D-SK17-D07` (Sheets/BBNF gap to 3E), `3D-SK17-D08` (monotonic direction) | INDEX.md SK-V17 fold-state block |
| WORKSPACE ← SK-V17 fold state | `3D-SK17-D08` (monotonic direction), `3E17-D08` (leak-census owner) | WORKSPACE.md SK-V17 workspace receiver |
| HARDENING (relevant) ← scope-honesty firewall | `3D-SK17` SCOPE-HONESTY BANNER, `3E17-D05` (no 6th shape), `3E17-D07` (by-construction scoping) | HARDENING.md SK-V17 hardening receiver |

## Residual absorption (2 non-blocking REVISE)

- **CH4-V3-01** (D07 scaffold→body cost-cell band, ~4×270 = 800–1100 LOC):
  recorded in BENCH.md SK-V17 receiver clause 3 as a MASTER-PLAN cost-table
  concern (one cost-row at the SK-V18 wave allocation), not a BENCH threshold
  amendment. Rides forward to the cost-table CRUD leg.
- **CH6-V3-7** (3E defer-word re-order + 3C anti-silent-satisfy clause):
  absorbed in COMPILER.md SK-V17 receiver clause 3 — the `EBNF/BNF/CSV/math`
  matrix cell carries the full receiver/blocker/gate defer triple (receiver =
  SK-V18 onboarding wave; blocker = no `structural_index`/scan witness for math;
  gate = Lock-14 future-grammar onboarding test), never an unqualified defer.
  The 3C anti-silent-satisfy clause is carried by the Lock-10 mandatory inline
  Lock-1 manifest cross-reference (already in the applied LOCKS addendum), noted
  in HARDENING.md SK-V17 receiver.

## Invariant verification (post-apply)

| invariant | check | result |
|---|---|---|
| 16-lock count preserved | `grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` = 16 | PASS |
| 5-shape BackendShape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` verbatim, NO 6th | `grep -nE "EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage,[A-Za-z]" restart/skinny/*.md` empty; canon cited verbatim in INDEX + COMPILER | PASS |
| tape = substrate-manifest CATEGORY (LAC-1E-14 precedent) | COMPILER.md receiver: "the tape folds in as the substrate-manifest CATEGORY the five shapes project from … NOT a sixth `BackendShape`" | PASS |
| aarch64-only (no x86/AVX/SVE) | COMPILER.md + WORKSPACE.md receivers: "aarch64-only; x86/AVX-512/SVE permanently pre-blocked as close paths" | PASS |
| preserve-rich-ast | SUBSTRATE.md receiver clause 2: "the lazy view IS the rich-AST materialization plane, never a typed-AST flattening" | PASS |
| no re-opened REDRESS (AZ-IV eager, StructRegistry indirection, fact-stream) | SUBSTRATE.md clause 4 (eager tree = fold-DELETION target), clause 3 (per-leaf `StructRegistry::layout` REJECT), BENCH/COMPILER (fact-stream-String CSS admission retired) | PASS |
| pre-existing dirty files untouched | `git diff --name-only` over the six surfaces returns only the six; SK-V12/13 research JSON, skinny `css_l4_*` generated.rs, `docs/precepts` not staged | PASS |
| monotonic skinny→totality direction | INDEX.md + WORKSPACE.md receivers: SK-V18 adopts skinny `Tape`/`ValueRef` INTO crates/core; §9 names FORBIDDEN-IN-SKINNY; totality never dictates back | PASS |

## Source provenance

- Master HEAD at apply: `2a76916ac` (T-P3 §3Z LOCKED proposed deltas).
- LOCKS v+1 addendum applied (CRUD-3 LOCKS leg): `7157be073`.
- 3D source: `restart/audit/totality/sk-v17/p3/3d-skinny-fold.md` (8 deltas).
- 3E source: `restart/audit/totality/sk-v17/p3/3e-grammar-generalisation.md`
  (9 deltas).
- Consolidator: `restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md`.
- Precedent mirror: `restart/audit/totality/astral/V2/ΩE-skinny-corpus.md`.

## Carry-forward

CRUD-5 SK-V17 narrows the skinny corpus to the SK-V17 T-P3 tape-fold state. The
remaining Pass Omega V5 SK-V17 CRUD legs (CRUD-1 ARCHITECTURE ← 3A, CRUD-2
MASTER-PLAN ← 3B incl. the CH4-V3-01 cost-row, CRUD-4 HANDOFF/MIGRATION ← 3F,
CRUD-6 AUDIT close) apply their corresponding deltas. No source / generated
runtime / gate / `RESULTS.md` / `REDRESS.md` / live skinny SPEC edits land in
this CRUD.
