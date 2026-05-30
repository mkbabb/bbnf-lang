---
lens: CH3 REGRESSION
pass: T-P3-synthesis
cycle: V2
reviewer: CH3 REGRESSION (V2)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
subject: SK-V17 T-P3 synthesis artefacts 3a-3f + 3c-locks-v+1-diff.md
scope: no delta re-opens a REDRESS pre-block (AZ-IV eager, StructRegistry indirection, fact-stream, x86); 3D does not promote a rejected route; 3C does not weaken a REDRESS-strengthened lock
counts:
  accept: 17
  revise: 0
  reject: 0
---

# CH3 REGRESSION — SK-V17 T-P3 V2

## Verdict

**17 ACCEPT, 0 REVISE, 0 REJECT (100% ACCEPT).** The V2 fold is
regression-clean. The sole V1 REVISE — the mechanical hunk-header miscount in the
G-Omega gate object `3c-locks-v+1-diff.md` — is RESOLVED: the header now reads
`@@ -606,7 +606,22 @@` and `git apply --check` returns **EXIT 0** against
`restart/locks/LOCKS.md` at master HEAD `2a76916ac` (verified this cycle, no
content change to the addendum). Every proposed delta still treats the four named
REDRESS pre-blocks (AZ-IV eager value tree, StructRegistry/Arena/Builder per-leaf
indirection, CSS fact-stream String, x86/AVX/SVE) as a DELETION-target / FENCE /
REJECT — never as a carried or revived route. 3D's monotonic fold promotes no
rejected route. 3C weakens no REDRESS-strengthened lock. The CH3-tagged open
question from V1 (re-run `git apply --check` after the header fix) is answered.

## Method

Re-derived the four named pre-blocks from canonical authority — the SK-V17 SPEC §9
"Global blocks (all waves)" + "REJECTed candidates barred from the active shortlist"
(`restart/skinny/tranches/sk-v17/SPEC.md:789`-`855`): (1) AZ-IV eager value tree
(118×), (2) StructRegistry/Arena<G>/Builder<G> hot-path indirection
(28-65×/983×/10583×), (3) CSS fact-stream String as a live admission plane, (4)
x86/AVX-512/SVE (aarch64 only) — plus the inherited D6 second-substrate /
UnionTape / sixth-shape / cross-call-classifier-state family. Read the in-force
`restart/locks/LOCKS.md` Lock 1/10/14/16 bodies + the strengthened v+1 clauses
(substrate-union ELEVATION `:137`-`149`; FactStream 5th category `:100`-`116`; Lock
14 no-hand-written-runtime `:349`; Lock 16 manifest `:607`). Extracted the
`3c-locks-v+1-diff.md` proposed diff and ran `git apply --check` against `LOCKS.md`
at master HEAD. Grep-scanned all seven synthesis artefacts for unguarded
x86/union-substrate/eager/per-leaf-registry/fact-stream-admission/sixth-shape/L9-udot
promotion.

## Gate-object apply check (the V1 REVISE — RESOLVED)

| item | disposition | finding |
|---|---|---|
| **3c-locks-v+1-diff.md hunk header** | **ACCEPT (was REVISE in V1)** | The header now reads `@@ -606,7 +606,22 @@` (`3c-locks-v+1-diff.md:49`). The hunk body carries 7 old-side context lines (the Lock 16 clause `LOCKS.md:606`-`607` + blank `:608` + the post-context blank `:609` + `## v+1 Governance Boundary` `:610` etc.) and 22 new-side lines (7 context + 15 added, 0 removed). Extracted via `awk` fence-strip and ran `git apply --check /tmp/ch3_locks.patch` → **EXIT 0**. The V1 CH3 open question ("re-run `git apply --check` after the header fix") is hereby answered: the corrected gate object applies clean. No content change; the addendum body is byte-identical to V1. |

## Disposition by Delta / Section

### 3C LOCKS crystallisation + 3c-locks-v+1-diff.md (the load-bearing G3 gate object)

| item | disposition | finding |
|---|---|---|
| Lock 1 tape-substrate-union clause (`3c-locks-v+1-diff.md:58`) | ACCEPT | Eager `OpenFrame` builders RETIRED (not adopted); per-leaf `StructRegistry::layout(rule)` runtime walk is REJECT and cites the measured 28-65×/983×/10583× regression (`SPEC.md:793`-`795`); `begin_compound` reads `layout.rule_id & 0x1F` only (grep-zero `StructRegistry`); OnceCell carriers must resolve `existing_tape`/`local_temp_only` else REDRESS-53 (`SPEC.md:577`/`:825`/`:839`). Reinforces pre-blocks §1 (eager) + §2 (registry) + the second-substrate family; weakens nothing. A dual AoS/SoA end-state is a Lock-1 closure REJECT, admissible only as transient fold-state — matches §9 "Second substrate" block. |
| Lock 10 tape-category clause (`3c-locks-v+1-diff.md:62`) | ACCEPT | 5-shape canon verbatim `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; tape is a substrate-manifest CATEGORY not a 6th shape; 6th remains G-Omega-gated (`LOCKS.md:109`) + SK-V17 §9-barred (`SPEC.md:808`); the `admits_collapsed_stage` x86-binding mechanically refuses on aarch64; no D6 second substrate (`SPEC.md:854`). No lock narrowing; no x86 close path. |
| Lock 14 ValueRef/classifier clause (`3c-locks-v+1-diff.md:64`) | ACCEPT | Lazy `ValueRef<G>` replaces the per-grammar EAGER value enums (`css_l4/value.rs:414`) — the AZ-IV pre-block answered by lazy projection, not carried forward. Directly reinforces Lock 14 (`LOCKS.md:349`); scope-honest JSON+CSS-only value-fold; preserve-rich-ast held (`SPEC.md:252`). No JSON/CSS-narrowing — Lock 14 stays grammar-neutral (clears the CH2 firewall this lens cross-checks). |
| Lock 16 NEON-classifier-manifest clause (`3c-locks-v+1-diff.md:66`) | ACCEPT | `retention_lifetime = transient-single-call` explicitly cites the Lock 1 v+1 ELEVATION (`LOCKS.md:137`-`149`) — reinforces, never weakens, the strengthened no-cross-call-carry lock (the "cross-call classifier-state retention" §9 block). x86 barred as close path; aarch64 primary, no SVE (`SPEC.md:806`); eq-set fan is the one real NEON body, table/prefix honestly-declared scalar passthroughs. |
| Disposition matrix (9 ACCEPT / 3 ORQ-ACCEPT / 2 MODIFY / 0 REJECT / 0 DEFER) | ACCEPT | The 2 MODIFYs (Lock-2 `StructLayout` reconcile) record both priced routes and bar Lock-2 closure by `LayoutFacts` alone — no route promotion, no lock weakening. The 3 ORQs carry named receiver+blocker+gate. No silent drop; the refutation clauses preserve every REDRESS-strengthened pre-block as a REJECT-class fence. |
| Invariant block (16 locks / 5 shapes / no new substrate) | ACCEPT | The addendum adds no numbered lock (the 16 locks are ordered list items `1.`-`16.` in `LOCKS.md`; the addendum inserts only addendum clauses, none renumbered/retired/added); restates the five shapes verbatim; records the tape as `substrate_target = existing_tape` not a new substrate or sixth shape; NEON classifier `transient-single-call` no cross-call state. Governance boundary in force above `## v+1 Governance Boundary` (`LOCKS.md:610`). |

### 3D skinny-fold (monotonic; does NOT promote a rejected route)

| item | disposition | finding |
|---|---|---|
| R1 eager-tree → locks-strengthening (`3d:97`, `3D-SK17-D04`) | ACCEPT | AZ-IV 118× eager `CssTypedValue`+six `pending_*` Vecs is a "fold-DELETION target, never carried forward into either tree"; the pre-block is anchored to the construct (per-leaf typed/f64/Box heap alloc), not a symbol list. Matches §9 "AZ-IV eager value tree". Not promoted; REPLACED by D02 lazy `ValueRef<G>`. |
| R2 registry → locks-strengthening (`3d:98`, `3D-SK17-D05`) | ACCEPT | 28-65×/983×/10583× StructRegistry/Arena/Builder per-leaf indirection becomes the no-per-leaf-lookup FENCE (resolved once at codegen). Matches §9 "StructRegistry / Arena<G> / Builder<G> hot-path indirection ... No registry lookup in the per-leaf hot path". The live coupling `arena.rs:47` is SEVERED by the eager retirement, not relocated. |
| R3 fact-stream → locks-strengthening (`3d:99`, `3D-SK17-D06`) | ACCEPT | CSS fact-stream String retires to diagnostic-only; the V1 FactStream category survives ONLY as a typed-schema/provenance output plane (`LOCKS.md:100`-`116`, `substrate_target = admitted_fact_output`). Matches §9 "CSS fact-stream String ... diagnostic-only" + the W5C array RETIRE clause. No Lock-1 narrowing — the two FactStream senses (typed output-plane vs barred String-admission) are correctly disjoint. |
| W1/W2/W3 wins → V1-authoritative (`3d:94`-`96`, D01/D02/D03) | ACCEPT | Proven SoA `Tape`/`ValueRef<G>`/`select_classifier` become authoritative; a dual AoS/SoA end-state is "transient-only, not a Lock-1 closure." SoA-adopt (2F-FOLD-U1) affirmed as the proven encoding (JSON 51/51 strict A/GO > sonic). No union-substrate thesis, no L9-Alt-mode / udot / i8mm promotion. D03 is alphabet-as-data scan-leaf FFI under the 4 LLVM shapes — NOT CollapsedStage (aarch64 CollapsedStage is UNKNOWN-2D-05). |
| G1 generality-gap → 3E (`3d:100`, `3D-SK17-D07`) | ACCEPT | Sheets/BBNF-self routed to 3E as by-construction-not-by-exercise; the SCOPE-HONESTY BANNER (`3d:45`-`51`, answering CH7.md:144) marks every Sheets/BBNF claim predicted/SK-V18-pending; no fleet-wide over-claim; Lock 14 not narrowed to JSON. |
| D08 monotonic-direction clause (`3d:113`) | ACCEPT | `StructLayout`/`TapeStructBuilder`/`TapeCursor` are FORBIDDEN-IN-SKINNY (§9 "Second substrate"); SK-V18 adopts skinny INTO crates/core, never relocates core into skinny. Monotonic invariant held (PASS-3 §8.4). |
| V2 fold-disposition coherence | ACCEPT | The V2 frontmatter folds CH3 V1 rows R1/R2/R3/W1W2W3/G1/D08 as ACCEPT and the CH7 open-question as a non-blocking exec-summary banner; the cross-artefact integrity note (`3d:82`) correctly records the 3C header fix and confirms each 3D→3C cross-ref points at a gate object that applies clean. No delta added, removed, or promoted in the fold. |

### 3A ARCHITECTURE / 3B MASTER-PLAN / 3E grammar-generalisation / 3F migration

| item | disposition | finding |
|---|---|---|
| 3A D01 / D05 + CH3 OQ (`3a:62`,`:66`,`:107`) | ACCEPT | D01 retires the live eager `OpenFrame` builders (`CssStructBuilder` 817 LOC, `JsonStructBuilder` 231 LOC) and converges AoS→SoA single-encoding (dual end-state transient-only); D05 holds the StructRegistry fence as REJECT and names the SOLE live coupling `arena.rs:47` (`compound_kind_for_layout`, grep-verified unique caller) severed by D01. The V1 CH4/CH5 REVISEs (40-file blast radius, arena.rs caller path) are folded with verified grep counts. The CH3 OQ tags REDRESS-53 parallel-index correctly. No pre-block re-opened. |
| 3B Refuted-Route Confirmation (`3b:98`-`112`) — the CH3 firewall section | ACCEPT | The four named pre-blocks (AZ-IV, StructRegistry, fact-stream, x86) inherit "superseded-but-binding" status; §13.3/§13.4 SK-V14/NW historical blocks are NOT revived; the SK-V18 receiver block is forward fold work, not a refuted-wave revival. MP-3B-SKV17-D03 keeps the 5-shape canon unchanged and records the tape as a substrate CATEGORY, not a 6th shape. |
| 3E D01-D09 + onboarding predicates (`3e:58`,`:140`) | ACCEPT | x86/AVX/SVE consistently barred / "mechanically refused on aarch64"; CSS bound to the eq-set fan NEON body (not the scalar table delegate); the `EagerTape` Pratt (math) cell is the legitimate 5-shape canon member (predicted/deferred per Lock-14 onboarding test), NOT an eager-value-tree re-open; no CSS-narrowing of Lock 14/16. The future-grammar onboarding test survives. |
| 3F D01-D09 (`3f:63`-`70`,`:94`-`105`) | ACCEPT | Migration receiver carries eager retirement (MH-03), the AZ-IV registry fence (MH-05 + §9 SPEC `:791`-`794`), fact-stream String demoted to diagnostic-only (`3f:105`), no-second-substrate fence (MH-06), and x86/AVX-512/SVE in the "must NOT" column (`3f:94`). Monotonic skinny→SK-V18-core direction preserved; no inversion. |

## Pre-block re-open scan (negative gate — all clean)

- **x86/AVX/SVE**: every occurrence guarded — `3f:94` lists "x86/AVX-512/SVE close
  path" in the must-NOT column; the `admits_collapsed_stage` x86-binding is the
  mechanical-refusal-on-aarch64 argument (`3c:62`, `3a`, `3e:58`); the
  `crates/simd-scan` multi-arch retention is bounded "WITHOUT admitting x86 as a
  close path"; the `udot`/i8mm orphan kernel is REFUTED (no CSS antecedent, no live
  consumer; `3a:111`) and confined to the deferred appendix. No active promotion of
  a host-blocked route.
- **union-substrate / UnionTape / D6 / 96-98 / sixth shape**: appear only inside
  REJECT/fence framing. SoA convergence rides the cardinality-one index==tape-offsets
  identity, not the retired union-substrate thesis. The tape is a substrate-manifest
  CATEGORY; a 6th `BackendShape` stays G-Omega-gated.
- **eager value tree / per-leaf StructRegistry / fact-stream String**: framed as
  "fold-DELETION target, never carried forward" / "REJECT" / "retires to
  diagnostic-only" across 3A/3C/3D/3F — never carried, retained, or admitted.
- **cross-call classifier-state retention**: barred via `retention_lifetime =
  transient-single-call` citing the Lock 1 ELEVATION (`LOCKS.md:137`-`149`).

## Open Questions (tagged to lens)

None blocking. The V1 CH1-tagged question (re-run `git apply --check` on the
corrected gate object) is answered in this cycle: **EXIT 0**, the gate object
applies clean against `LOCKS.md` at master HEAD `2a76916ac`.

## Required Fix (V3 fold)

None. The sole V1 REVISE is resolved; no new regression defect surfaced. CH3
returns 100% ACCEPT for cycle V2.
