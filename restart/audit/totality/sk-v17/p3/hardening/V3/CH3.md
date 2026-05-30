---
lens: CH3 REGRESSION
pass: T-P3-synthesis
cycle: V3
reviewer: CH3 REGRESSION (V3)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
subject: SK-V17 T-P3 synthesis artefacts 3a-3f + 3c-locks-v+1-diff.md
scope: no delta re-opens a REDRESS pre-block (AZ-IV eager, StructRegistry indirection, fact-stream, x86); 3D does not promote a rejected route; 3C does not weaken a REDRESS-strengthened lock
counts:
  accept: 17
  revise: 0
  reject: 0
---

# CH3 REGRESSION — SK-V17 T-P3 V3

## Verdict

**17 ACCEPT, 0 REVISE, 0 REJECT (100% ACCEPT).** Second consecutive
all-ACCEPT cycle for CH3. The V3 fold is regression-clean. Every proposed
delta continues to treat the four named REDRESS pre-blocks — AZ-IV eager value
tree (118×), StructRegistry/Arena/Builder per-leaf hot-path indirection
(28-65×/983×/10583×), CSS fact-stream String as a live admission plane,
x86/AVX-512/SVE — as a DELETION-target / FENCE / REJECT, never as a carried or
revived route. 3D's monotonic fold promotes no rejected route. 3C weakens no
REDRESS-strengthened lock; the five §3Z refutation rows survive intact as
REJECT-class clause text. The lone V3 mutation — the CH5-V2-R01 distribution-
invariant bullet now folded into the gate object's Invariant Check
(`3c-locks-v+1-diff.md:103`) — *strengthens* the Lock-10/Lock-1 substrate-
manifest anchor rather than weakening it, and is appended in the prose section
outside the diff hunk, so the G-Omega gate object still `git apply --check`
clean (EXIT 0) against `restart/locks/LOCKS.md` at master HEAD `2a76916ac`
(re-verified this cycle). No new regression defect surfaced.

## Method

Re-derived the four named pre-blocks from canonical authority — the SK-V17 SPEC
§9 "Global blocks (all waves)" + "REJECTed candidates barred from the active
shortlist" (`restart/skinny/tranches/sk-v17/SPEC.md:789`-`855`): (1) AZ-IV eager
value tree (118×, `:790`-`792`), (2) StructRegistry/Arena<G>/Builder<G> hot-path
indirection (28-65×/983×/10583×, `:793`-`796`), (3) CSS fact-stream String as a
live admission plane (`:797`-`799`), (4) x86/AVX-512/SVE — aarch64 only
(`:826`-`827`) — plus the inherited D6 second-substrate / UnionTape /
sixth-shape / cross-call-classifier-state family (`:828`-`834`). Read the
in-force `restart/locks/LOCKS.md` Lock 1/10/14/16 bodies + the strengthened v+1
clauses (substrate-union ELEVATION `:137`-`149`; FactStream 5th category
`:100`-`116`; Lock 14 no-hand-written-runtime `:349`; Lock 16 manifest `:607`).
Extracted the `3c-locks-v+1-diff.md` proposed diff fence via `awk` and ran
`git apply --check` against `LOCKS.md` at master HEAD → **EXIT 0**. Grep-scanned
all seven synthesis artefacts for unguarded x86/union-substrate/eager/per-leaf-
registry/fact-stream-admission/sixth-shape/L9-udot promotion; every hit lands
inside REJECT/fence/barred framing. Diffed the V3 frontmatter + bodies against
the V2-ACCEPTED state to isolate the cycle's mutations.

## Gate-object apply check (the V1 REVISE — STILL RESOLVED)

| item | disposition | finding |
|---|---|---|
| **3c-locks-v+1-diff.md hunk header** | **ACCEPT** | The header reads `@@ -606,7 +606,22 @@` (`3c-locks-v+1-diff.md:63`). Extracted the diff fence and ran `git apply --check` → **EXIT 0** against `restart/locks/LOCKS.md` at master HEAD `2a76916ac`. The hunk body is byte-identical to V2; the V3 mutation (distribution-invariant bullet) lives in the **prose Invariant Check** (`:103`), OUTSIDE the diff fence, so it does not perturb the hunk arithmetic. The V1 CH3 open question stays answered: the gate object applies clean. |

## Disposition by Delta / Section

### 3C LOCKS crystallisation + 3c-locks-v+1-diff.md (the load-bearing G3 gate object)

| item | disposition | finding |
|---|---|---|
| Lock 1 tape-substrate-union clause (`3c-locks-v+1-diff.md:72`) | ACCEPT | Eager `OpenFrame` builders RETIRED (not adopted, `builder.rs:16`/`:9`); per-leaf `StructRegistry::layout(rule)` runtime walk is REJECT and cites the measured 28-65×/983×/10583× regression (`SPEC.md:793`-`795`); `begin_compound` reads `layout.rule_id & 0x1F` only (`tape/mod.rs:185`-`186`, grep-zero `StructRegistry`); the live coupling `arena.rs:47` is SEVERED by the eager retirement; OnceCell carriers must resolve `existing_tape`/`local_temp_only` else REDRESS-53 (`SPEC.md:577`/`:825`/`:839`). Reinforces pre-blocks §1 (eager) + §2 (registry) + the second-substrate family; weakens nothing. A dual AoS/SoA end-state is a Lock-1 closure REJECT, admissible only as transient fold-state — matches §9 "Second substrate". |
| Lock 2 StructLayout-reconcile clause (`3c-locks-v+1-diff.md:74`) | ACCEPT | Records BOTH priced routes (rename 960 sites vs `LayoutFacts.backend_shape` side-table, the latter grep-zero in `crates/`) and bars Lock-2 closure by `LayoutFacts` alone while public `Layout`/`LayoutSink` remain absent (`LOCKS.md:162`-`166`). No route promotion, no lock weakening, no pre-block touched — route selection deferred to an SK-V18 wave with named receiver, not a lock edit. |
| Lock 10 tape-category clause (`3c-locks-v+1-diff.md:76`) | ACCEPT | 5-shape canon verbatim `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` (`LOCKS.md:107`-`108`); tape is a substrate-manifest CATEGORY (`substrate_target = existing_tape`) not a 6th shape; 6th remains G-Omega-gated (`LOCKS.md:109`) + SK-V17 §9-barred (`SPEC.md:808`); the `admits_collapsed_stage` x86-binding mechanically refuses on aarch64 (`ARCHITECTURE.md:1151`,`:1282`); aarch64 CollapsedStage is the spec-named UNKNOWN-2D-05, not a fresh gap; no D6 second substrate (`SPEC.md:854`). No lock narrowing; no x86 close path. |
| Lock 14 ValueRef/classifier clause (`3c-locks-v+1-diff.md:78`) | ACCEPT | Lazy `ValueRef<'doc,'input,K,G:EventGrammar>` replaces the per-grammar EAGER value enums (`css_l4/value.rs:414`) — the AZ-IV pre-block answered by lazy projection, not carried forward. Directly reinforces Lock 14 (`LOCKS.md:349`, no hand-written per-grammar runtime file enters a generic crate); scope-honest JSON+CSS-only value-fold (Sheets/BBNF by-construction-not-by-exercise); preserve-rich-ast held (`SPEC.md:252`). No JSON/CSS-narrowing — Lock 14 stays grammar-neutral (clears the CH2 firewall this lens cross-checks). |
| Lock 16 NEON-classifier-manifest clause (`3c-locks-v+1-diff.md:80`) | ACCEPT | `retention_lifetime = transient-single-call` explicitly cites the Lock 1 v+1 ELEVATION (`LOCKS.md:137`-`149`) — reinforces, never weakens, the strengthened no-cross-call-carry lock (the "cross-call classifier-state retention" §9 block). x86 barred as close path; aarch64 primary, no SVE (`SPEC.md:806`); eq-set fan is the one real NEON body, `byte_class_from_table_64`/`bitmap_prefix_xor_64` honestly-declared scalar passthroughs; `crates/simd-scan` multi-arch reconcile binds WITHOUT admitting x86 as a close path. |
| Disposition matrix (9 ACCEPT / 3 ORQ-ACCEPT / 2 MODIFY / 0 REJECT / 0 DEFER) | ACCEPT | The 2 MODIFYs (Lock-2 `StructLayout` reconcile) record both priced routes and bar Lock-2 closure by `LayoutFacts` alone — no route promotion, no lock weakening. The 3 ORQs carry named receiver+blocker+gate. No silent drop. The five refutation rows (`3c-locks-crystallisation.md:184`-`200`: 6th-shape REFUTED, per-leaf `StructRegistry::layout` REFUTED, AoS/SoA dual REJECT, fleet-wide value-plane scoped, x86/SVE barred) preserve every REDRESS-strengthened pre-block as a REJECT-class fence. The 0-REJECT tally is LOCKED-input provenance (`:169`-`180`), not absent hostile content. |
| **Distribution-invariant bullet (V3 fold of CH5-V2-R01)** (`3c-locks-v+1-diff.md:103`) | ACCEPT | The CH5-V2-R01 REVISE landed: the distribution invariant — if Pass Omega distributes the addendum into Lock bodies, the Lock-10 tape-category clause MUST retain its inline cross-ref to the Lock-1 substrate manifest (`substrate_target = existing_tape`) — is now copied into the gate object's Invariant Check. This is a REGRESSION-STRENGTHENING addition: it fences a forward-distribution apply-time path that, if severed, "re-opens the silent-6th-shape reading." It strengthens, never weakens, the Lock-10/Lock-1 anchor and re-opens no pre-block. Verified appended OUTSIDE the diff fence — gate object still applies clean. |
| Invariant block (16 locks / 5 shapes / no new substrate) (`3c-locks-v+1-diff.md:100`-`105`) | ACCEPT | Adds no numbered lock (16 headings unchanged at `LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`), restates the five shapes verbatim, records the tape as `substrate_target = existing_tape` not a new substrate/6th shape, NEON classifier transient-single-call no cross-call state. Governance boundary in force above `## v+1 Governance Boundary` (`LOCKS.md:610`). |

### 3D skinny-fold (monotonic; does NOT promote a rejected route)

| item | disposition | finding |
|---|---|---|
| R1 eager-tree → locks-strengthening (`3d:100`, SK17L-003 / L-SK17-01) | ACCEPT | AZ-IV eager value tree REFUTED 118× (canada 1.83ms→215.7ms); the `crates/core` `CssTypedValue` eager enum + six `pending_*` Vecs (`css_l4/builder.rs:71-79`) is "a fold-DELETION target, never carried forward." Matches §9 "AZ-IV eager value tree". Not promoted; REPLACED by the W2 lazy `ValueRef<G>`. |
| R2 registry → locks-strengthening (`3d:101`, SK17L-004 / L-SK17-02) | ACCEPT | StructRegistry/Arena/Builder per-leaf indirection REFUTED 28-65×/983×/10583×; Lock 1 carries the no-per-leaf-registry-lookup fence, `StructRegistry::layout(rule_id)` (`struct.rs:313`,`:331`) resolved ONCE at codegen, never per-leaf. Matches §9 "No registry lookup in the per-leaf hot path." The `arena.rs:47` coupling SEVERED by the eager retirement, not relocated. |
| R3 fact-stream → locks-strengthening (`3d:102`, SK17L-005 / L-SK17-03) | ACCEPT | CSS fact-stream String REFUTED as admission plane (~34% self-time `emit_*`, benched String not typed CSSOM); the Lock 1 v+1 FactStream category survives ONLY for typed-schema/provenance output planes; the String CSS-admission plane "retires to diagnostic-only." Matches §9 "diagnostic-only" + `LOCKS.md:100`-`116`. The two FactStream senses (typed output-plane vs barred String-admission) correctly disjoint — no Lock-1 narrowing. |
| W1/W2/W3 wins → V1-authoritative (`3d:94`-`96`, D01/D02/D03) | ACCEPT | Proven SoA `Tape<'input>`/`ValueRef<G>`/`select_classifier` become authoritative; AoS `TapeRec` converges onto SoA, a dual end-state is "transient-only, not a Lock-1 closure." JSON rides SoA >SOTA (51/51 strict A/GO, `RESULTS.md:5-55`). No union-substrate thesis, no L9-Alt-mode/udot/i8mm promotion; D03 is alphabet-as-data scan-leaf FFI under the 4 LLVM shapes — NOT the CollapsedStage (aarch64 CollapsedStage is UNKNOWN-2D-05). |
| G1 generality-gap → 3E (`3d:107`, SK17L-009) | ACCEPT | Sheets/BBNF-self routed to 3E as by-construction-not-by-exercise; the SCOPE-HONESTY BANNER (`3d:43`-`51`) marks every Sheets/BBNF AND CSS->SOTA claim predicted/SK-V18-pending; no fleet-wide over-claim; Lock 14 not narrowed to JSON. |
| D08 monotonic-direction clause | ACCEPT | `StructLayout`/`TapeStructBuilder`/`TapeCursor` are FORBIDDEN-IN-SKINNY (§9 "Second substrate"); SK-V18 adopts skinny INTO crates/core, never relocates core into skinny (`1d:128`). Monotonic invariant held (PASS-3 §8.4). |
| V3 fold-disposition coherence (`3d:90`-`93`) | ACCEPT | The V3 cross-artefact integrity note records the 3C gate-object clean apply and confirms every 3D→3C cross-ref (D01/D04/D05/D06→Lock 1; D03→Lock 16; D07→Lock 14) points at a gate object that applies clean. The frontmatter folds the V2 3D-touching dispositions as ACCEPT and correctly records `revised: []` (no 3D-touching REVISE landed in V2). No delta added, removed, or promoted in the fold. |

### 3A ARCHITECTURE / 3B MASTER-PLAN / 3E grammar-generalisation / 3F migration

| item | disposition | finding |
|---|---|---|
| 3A D01 / D05 + CH3 OQ | ACCEPT | D01 retires the live eager `OpenFrame` builders (`CssStructBuilder` 817 LOC, `JsonStructBuilder`) and converges AoS→SoA single-encoding (dual end-state transient-only); D05 holds the StructRegistry fence as REJECT and names the SOLE live coupling `arena.rs:47` severed by D01. No pre-block re-opened; REDRESS-53 parallel-index tagged correctly. |
| 3B Refuted-Route Confirmation (`3b:117`) — the CH3 firewall section | ACCEPT | The four named pre-blocks (AZ-IV eager, StructRegistry indirection, fact-stream-String, x86) explicitly "inherit the same superseded-but-binding status"; §13.3 SK-V14 / §13.4 MP-NW historical blocks are NOT revived; MP-3B-SKV17-D03 keeps the 5-shape canon unchanged and records the tape as a substrate CATEGORY not a 6th shape (`3b:164`). The SK-V18 receiver block is forward fold work, not a refuted-wave revival. |
| 3E D01-D09 + onboarding predicates (`3e:63`) | ACCEPT | x86/AVX/SVE consistently barred / "mechanically refused on aarch64" (the `admits_collapsed_stage` x86-binding); CSS bound to the eq-set fan NEON body not the scalar table delegate; the `EagerTape` Pratt (math) cell is a legitimate 5-shape canon member (predicted/deferred per the Lock-14 onboarding test), NOT an eager-value-tree re-open; no CSS-narrowing of Lock 14/16. The future-grammar onboarding test survives. |
| 3F D01-D09 (`3f:99`,`:215`) | ACCEPT | Migration receiver carries eager retirement, the AZ-IV registry fence, fact-stream String demoted to diagnostic-only, no-second-substrate fence, and "x86/AVX-512/SVE close path" + "cross-call classifier-state carry" in the must-NOT column (`3f:99`). The CH5 OQ (`3f:215`) correctly tags the BackendShape-category 5-shape coherence cross-check. Monotonic skinny→SK-V18-core direction preserved; no inversion. |

## Pre-block re-open scan (negative gate — all clean)

- **x86/AVX/SVE**: every occurrence guarded — `3f:99` lists "x86/AVX-512/SVE
  close path" in the must-NOT column; `3d:69` names "x86/AVX/SVE" among the
  locks-strengthening rejections; the `admits_collapsed_stage` x86-binding is the
  mechanical-refusal-on-aarch64 argument (`3c-diff:76`, `3a`, `3e:63`); the
  `crates/simd-scan` multi-arch retention is bounded "WITHOUT admitting x86 as a
  close path" (`3c-diff:80`); the `udot`/i8mm orphan is REFUTED/deferred. No
  active promotion of a host-blocked route.
- **union-substrate / UnionTape / D6 / 96-98 / sixth shape**: appear only inside
  REJECT/fence/G-Omega-gated/not-a-6th-shape framing (`3c-diff:38`,
  `3c-crystallisation:73`,`:144`,`:187`; `3b:164`). SoA convergence rides the
  cardinality-one index==tape-offsets identity, not the retired union-substrate
  thesis. The tape is a substrate-manifest CATEGORY; a 6th `BackendShape` stays
  G-Omega-gated.
- **eager value tree / per-leaf StructRegistry / fact-stream String**: framed as
  "fold-DELETION target, never carried forward" / "REJECT" / "retires to
  diagnostic-only" across 3A/3C/3D/3F — never carried, retained, or admitted.
- **cross-call classifier-state retention**: barred via `retention_lifetime =
  transient-single-call` citing the Lock 1 ELEVATION (`LOCKS.md:137`-`149`).
- **V3 mutation audit**: the sole V3 body mutation — the distribution-invariant
  bullet (`3c-locks-v+1-diff.md:103`) — STRENGTHENS the Lock-10/Lock-1 anchor and
  is appended outside the diff fence; it re-opens no pre-block and does not
  perturb the gate object's clean apply.

## Open Questions (tagged to lens)

None blocking. The V1 CH1/CH3 question (re-run `git apply --check` on the
corrected gate object) stays answered: **EXIT 0** this cycle. The V3
distribution-invariant addition was verified to leave the diff fence intact.

## Required Fix (V4 fold)

None. No regression defect surfaced. CH3 returns 100% ACCEPT for cycle V3 — the
second consecutive all-ACCEPT CH3 cycle (V2: 17/0/0; V3: 17/0/0), satisfying the
≥95%-ACCEPT-for-two-consecutive-cycles convergence criterion from this lens'
seat (PASS-3 §4; ORCHESTRATOR §3Z).
