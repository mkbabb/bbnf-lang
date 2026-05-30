---
lens: CH3 REGRESSION
pass: T-P3-synthesis
cycle: V1
reviewer: CH3 REGRESSION (V1)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
subject: SK-V17 T-P3 synthesis artefacts 3a-3f + 3c-locks-v+1-diff.md
scope: no delta re-opens a REDRESS pre-block (AZ-IV eager, StructRegistry indirection, fact-stream, x86); 3D does not promote a rejected route; 3C does not weaken a REDRESS-strengthened lock
counts:
  accept: 16
  revise: 1
  reject: 0
---

# CH3 REGRESSION — SK-V17 T-P3 V1

## Verdict

**1 REVISE, 0 REJECT, 16 ACCEPT.** The synthesis is regression-sound: every
proposed delta treats the four named REDRESS pre-blocks (AZ-IV eager value tree,
StructRegistry/Arena/Builder per-leaf indirection, CSS fact-stream String, x86/AVX/SVE)
as a DELETION-target / FENCE / REJECT, never as a carried or revived route. 3D's
monotonic fold promotes no rejected route. 3C weakens no REDRESS-strengthened lock.
The sole defect is mechanical-but-load-bearing: the **G-Omega gate object
`3c-locks-v+1-diff.md` does not `git apply` cleanly** because its hunk header
line-counts are fabricated — a CH1-class defect surfaced from the CH3 seat because a
non-applying gate object cannot be verified against REDRESS at all. Content is
correct; the fix is a one-line header correction.

## Method

Read the REDRESS pre-block authority (`restart/skinny/tranches/sk-v17/research/p3/
p3e-preblocked-ledger.md`, `restart/skinny/tranches/sk-v17/research/alpha/
alphaC-redress-digest.md` §1-§8 — the canonical mapping of the four named pre-blocks),
the in-force `restart/locks/LOCKS.md` Lock 1/10/14/16 bodies + the strengthened
v+1 clauses (substrate-union ELEVATION `:137`-`149`; FactStream 5th category
`:100`-`116`; Lock 14 no-hand-written-runtime `:349`; Lock 16 manifest `:607`), and
all seven synthesis artefacts. Ran grep scans for unguarded x86/union-substrate/L9/udot
promotion and tested the diff via `git apply --check`.

## Disposition by Delta / Section

### 3C LOCKS crystallisation + 3c-locks-v+1-diff.md (the load-bearing G3 gate object)

| item | disposition | finding |
|---|---|---|
| **3c-locks-v+1-diff.md hunk header** | **REVISE** | The hunk header is `@@ -606,6 +606,52 @@`. Actual body: **7 old-side lines, 22 new-side lines** (7 context + 15 added, 0 removed). `git apply --check` reports `corrupt patch at line 27`. Re-running with the corrected header `@@ -606,7 +606,22 @@` **applies clean** (verified). PASS-3 §3 CH1 + §8.6: the `3c-locks-v+1-diff.md` "applies cleanly to the current `LOCKS.md`" is the G-Omega gate contract; a non-applying gate object is unverifiable against REDRESS. Content re-opens nothing — fix is header-only. **Fix:** `restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md:49` — change `@@ -606,6 +606,52 @@` to `@@ -606,7 +606,22 @@`. |
| Lock 1 tape-substrate-union clause (`3c-locks-v+1-diff.md:58`) | ACCEPT | Eager `OpenFrame` builders RETIRED; per-leaf `StructRegistry::layout(rule)` runtime walk is REJECT (re-opens 28-65×/983×/10583×); cross-call retained classifier state barred via the cited Lock 1 ELEVATION (`LOCKS.md:137`-`149`); OnceCell carriers must resolve `existing_tape`/`local_temp_only` else REDRESS-53. Reinforces all of pre-blocks §1/§2/§6; weakens nothing. |
| Lock 10 tape-category clause (`3c-locks-v+1-diff.md:62`) | ACCEPT | 5-shape canon verbatim; tape is a substrate-manifest CATEGORY not a 6th shape; 6th remains G-Omega-gated; x86 CollapsedStage mechanically refused on aarch64; no D6 second substrate. No lock narrowing. |
| Lock 14 ValueRef/classifier clause (`3c-locks-v+1-diff.md:64`) | ACCEPT | Directly reinforces Lock 14 `LOCKS.md:349` ("no hand-written per-grammar runtime file enters any generic crate"); scope-honest JSON+CSS-only value-fold; preserve-rich-ast held. No JSON/CSS-narrowing (clears CH2 firewall this lens cross-checks). |
| Lock 16 NEON-classifier-manifest clause (`3c-locks-v+1-diff.md:66`) | ACCEPT | `retention_lifetime = transient-single-call` explicitly cites the Lock 1 v+1 ELEVATION (`LOCKS.md:137`-`149`) — reinforces, never weakens, the strengthened no-cross-call-carry lock; x86 barred as close path; eq-set fan the one real NEON body, table/prefix honest scalar passthroughs. |
| Disposition matrix (9 ACCEPT / 3 ORQ-ACCEPT / 2 MODIFY / 0 REJECT / 0 DEFER) | ACCEPT | The 2 MODIFYs (Lock-2 `StructLayout` reconcile) record both priced routes and bar Lock-2 closure by `LayoutFacts` alone — no route promotion, no lock weakening. The 3 ORQs are crystallised pre-gates with named receiver+blocker+gate, not engineered-defers. Refutation rows 1-5 (`3c-locks-crystallisation.md:163`-`167`) preserve every REDRESS-strengthened pre-block as a REJECT-class clause. |

### 3D skinny-fold (monotonic; does NOT promote a rejected route)

| item | disposition | finding |
|---|---|---|
| R1 eager-tree → locks-strengthening (`3d:76`, `3D-SK17-D04`) | ACCEPT | AZ-IV 118× eager `CssTypedValue`+`pending_*` Vecs is a "fold-DELETION target, never carried forward into either tree." Matches redress digest §1 (ADMIT-UNDER-FRAMING via lazy projection). Not promoted; replaced by D02 lazy `ValueRef<G>`. |
| R2 registry → locks-strengthening (`3d:77`, `3D-SK17-D05`) | ACCEPT | 28-65×/983×/10583× StructRegistry/Arena/Builder per-leaf indirection becomes the no-per-leaf-lookup FENCE (resolved once at codegen). Matches digest §2a PERMANENT PRE-BLOCK. The live coupling `arena.rs:47` is SEVERED by the eager retirement, not relocated. |
| R3 fact-stream → locks-strengthening (`3d:78`, `3D-SK17-D06`) | ACCEPT | CSS fact-stream String retires to diagnostic-only; the V1 FactStream category survives ONLY as typed-schema/provenance output plane. Matches digest §3 ("string-only fact streams rejected; admit only as DIAGNOSTIC-ONLY") and the in-force Lock 1 v+1 (`LOCKS.md:100`-`116`). No Lock-1 narrowing — the two FactStream senses (typed output-plane vs barred String-admission) are correctly disjoint. |
| W1/W2/W3 wins → V1-authoritative (`3d:73`-`75`, D01/D02/D03) | ACCEPT | The proven SoA `Tape`/`ValueRef<G>`/`select_classifier` become authoritative; a dual AoS/SoA end-state is "transient-only, not a Lock-1 closure." SoA-adopt (2F-FOLD-U1) is affirmed as the proven encoding — parity-keep would re-open the dual-substrate risk; correctly handled. No union-substrate thesis (96/97/98), no L9/udot/i8mm promotion. |
| G1 generality-gap → 3E (`3d:79`, `3D-SK17-D07`) | ACCEPT | Sheets/BBNF-self routed to 3E as by-construction; no fleet-wide over-claim; Lock 14 not narrowed to JSON. |
| D08 monotonic-direction clause (`3d:92`) | ACCEPT | `StructLayout`/`TapeStructBuilder`/`TapeCursor` are FORBIDDEN-IN-SKINNY; SK-V18 adopts skinny INTO crates/core, never relocates core into skinny. Monotonic invariant held (PASS-3 §8.4). |

### 3A ARCHITECTURE / 3B MASTER-PLAN / 3E grammar-generalisation / 3F migration

| item | disposition | finding |
|---|---|---|
| 3A D01-D08 (`3a:57`-`64`) | ACCEPT | D01 retires eager builders + SoA convergence (no dual end-state); D05 holds the StructRegistry fence; D03/D08 bar x86 close path + REDRESS-53 re-entry. No pre-block re-opened. |
| 3B Refuted-Route Confirmation (`3b:98`-`109`) — the CH3 firewall section | ACCEPT | Explicitly tabulates all four named pre-blocks (AZ-IV, StructRegistry, fact-stream, x86) + dual AoS/SoA, each as a fold FENCE not a revived wave. §13.5 SK-V15 / SK-V14 historical blocks are NOT revived. No refuted wave proposed for revival; the SK-V18 receiver block (MP.SK18.W0-W6) is forward fold work, not a refuted-wave revival. |
| 3E D01-D09 + onboarding predicates (`3e`) | ACCEPT | x86/AVX/SVE consistently "barred"/"mechanically refused on aarch64"; CSS bound to the eq-set fan NEON body (not the scalar table delegate); no CSS-narrowing of Lock 14/16; the `crates/simd-scan` retain-x86 scope is bounded "WITHOUT admitting x86 as a close path" (matches digest §6 diagnostic-only allowance). |
| 3F D01-D09 (`3f`) | ACCEPT | Migration receiver carries eager retirement / registry fence (3F17-MH-05 binds the AZ-IV pre-block) / substrate-category as DELETION/FENCE/REJECT; monotonic skinny→SK-V18-core direction preserved; no inversion. |

## Pre-block re-open scan (negative gate — all clean)

- **x86/AVX/SVE**: every occurrence guarded ("barred", "mechanically refuses aarch64",
  "WITHOUT admitting x86 as a close path", "diagnostic-only"). Retention of existing
  multi-arch `crates/simd-scan` kernels is explicitly NOT a close-path admission —
  matches redress digest §6.
- **union-substrate / UnionTape / D6 / 96-98**: appear only inside REJECT/fence framing;
  no active promotion. SoA convergence rides the REDRESS-140 differential (cardinality
  one, index == tape offsets), not the retired union-substrate thesis.
- **L9 Alt-mode / udot / i8mm**: not promoted to active in any artefact; "commit-by-construction"
  references are the proven SoA flat-tape commit, not the CONDITIONAL L9 speculative-rollback route.
- **eager / per-leaf StructRegistry / fact-stream**: framed as "fold-DELETION target,
  never carried forward" / "REJECT" / "retires to diagnostic-only" — never carried, retained, or admitted.

## Open Questions (tagged to lens)

| lens | question | rationale |
|---|---|---|
| CH1 | After the V1→V2 fold corrects the `3c-locks-v+1-diff.md` header, the aggregator must re-run `git apply --check` against `restart/locks/LOCKS.md` at master HEAD to confirm the corrected gate object applies. | The G-Omega gate object's clean-apply is the load-bearing G3 contract (PASS-3 §8.6); a stale/uncorrected header would silently fail at Pass Omega CRUD. |

## Required Fix (V2 fold)

1. `restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md:49` — replace hunk header
   `@@ -606,6 +606,52 @@` with `@@ -606,7 +606,22 @@`. Verified: with this header the
   patch applies clean against `restart/locks/LOCKS.md` at master HEAD `2a76916ac`. No
   content change required; this is the sole REVISE.
