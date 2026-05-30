---
lens: CH5 HIDDEN-COUPLING
pass: T-P2-research (SK-V17 totality)
cycle: V1
generated_at: 2026-05-29T00:00:00Z
reviewer_method: "PASS-2-RESEARCH §3 CH5 + ORCHESTRATOR §3W. Read all six 2X dossiers (2a..2f) + the converged T-P1 inputs (1a..1f + HARDENING-T-P1-SKV17-CONVERGED.md) + the V1 spec surfaces (restart/ARCHITECTURE.md §7.3 substrate/plane-table, restart/locks/LOCKS.md Lock 1/10/14/16, restart/skinny/tranches/sk-v17/SPEC.md §9 second-substrate block). Every load-bearing substrate / sidecar / classifier-state / substrate_target claim re-anchored LIVE at master HEAD 91b6893b0 via git rev-parse + grep -n/-c + sed -n. No recalled LOC; every coupling-sensitive fact verified at source."
master_head: 91b6893b0b61d1c3213d02afe4ec62f22c16ae38
t_p1_locked_sha: 445925167154de73540e3ea3283d0170371de790
scope: "substrate-union (Lock 1) preserved — tape+projection ONE substrate; tape NOT a silent 6th BackendShape (Lock 10 5-shape canon); proposes substrate-manifest category OR G-Omega-gated 6th, never silent; mask stream = transient producer not retained sidecar (CH5 §3W); Layer 0/Layer 1 clean two-layer dependency not a coupling"
first_hygiene_action: "CH1-V5-001 (enumerated-filename residual) — verified RESOLVED on disk, not carried: grep -c 'collapsed_stage}.rs' 1b = 3; grep ',collapsed}' {1a,1b,1e} = 0 (exit 1). All six dossiers report this correctly. No fold required."
disposition_counts:
  accept: 18
  revise: 4
  reject: 0
  total: 22
accept_rate_pct: 81.8
---

# CH5 HIDDEN-COUPLING — SK-V17 T-P2 V1

## Verdict

The T-P2 V1 dossier set is **strong on the central CH5 axis** and free of the
catastrophic coupling pathologies this lens exists to catch. There is **no
silent 6th `BackendShape`**; **no parallel substrate**; **no retained sidecar**;
**no cross-call classifier-state proposal**. Every dossier discharges the
divergence-D question with the **LAC-1E-14 FactStream precedent** verbatim — the
tape is a *substrate-manifest category* under the fixed 5-shape canon, never a
shape peer — and the proposal is **explicit**, never silent (the dispatch's
"propose, do NOT silently add a 6th" is honoured in the negative across 2a/2c/2d/
2e/2f). The mask stream stays a transient producer (`local_temp_only`, ARCH
plane (4) `:1804`); the `OnceCell<StructuralIndex>` is correctly flagged as the
live retention-site that MUST be classified before wiring; the NEON classifier
declares `retention_lifetime = transient-single-call`. 2e even carries an
explicit "Fold Coherence Note (CH5 hidden-coupling pre-empt)" (`2e:356-368`).

The lens nonetheless finds **four genuine hidden-coupling residuals** — each a
*declaration gap* where a retained structure or a live coupling-site is named in
prose but its substrate-manifest classification (`substrate_target` /
`retention_lifetime` / `policy_owner`, mandatory per `LOCKS.md:118-127`) is left
implicit. An implicit retention is exactly the manifest hole the Lock 1 v+1
substrate manifest was authored to close. These are REVISE (not REJECT): the
fold direction is correct; the classification must be made explicit so T-P3
cannot inherit an unclassified retained substrate.

§3 expects ≥30% REVISE for V1; this lens returns **18.2% REVISE** on the CH5
axis alone, consistent with a dossier set that pre-empted the coupling lens
deliberately. The four REVISEs are real and load-bearing for the substrate
manifest's completeness.

---

## Live-anchored coupling verifications (the facts the dispositions rest on)

| Coupling-sensitive fact | Live check at `91b6893b0` | Result |
|---|---|---|
| Tape-as-substrate not 6th shape (LAC-1E-14) | `LOCKS.md:100-116`; ARCH plane-table `:1796-1804` ("NOT a 6th `BackendShape` … Per LAC-1E-14 this is the 5th SUBSTRATE-manifest category") | CONFIRMED — precedent exists, dossiers cite it exactly |
| 5-shape canon held verbatim | `LOCKS.md:107-108` `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; SPEC `:807` "sixth `BackendShape`" in the second-substrate REJECT block | CONFIRMED — domain fixed, 6th G-Omega-gated |
| substrate_target manifest values | `LOCKS.md:121-122` `{local_temp_only, existing_tape, direct_sink, admitted_fact_output}`; lifetimes `{local_loop, generated_function, output_row}`; owners `{generated_grammar, caller_data, none}` | CONFIRMED — four-value manifest is the declaration surface |
| Mask stream = transient producer | ARCH `:1804` plane (4) "`StructuralIndex` mask streams … Transient producer; never a retained sidecar … structural projection IS the tape" | CONFIRMED |
| `OnceCell<StructuralIndex>` is LIVE retained per-parser state | `crates/core/src/grammar/generated/json.rs:701` `pub(crate) structural_index: OnceCell<…>`; `ensure_structural_index` `:719` | CONFIRMED — this is the live retention-site F7/U-2E-02 correctly fences |
| `begin_compound` (tape path) does NOT call StructRegistry | `crates/core/src/runtime/tape/mod.rs:185-186` reads `layout.rule_id & 0x1F` only; `grep StructRegistry::layout tape/mod.rs` = 0 | CONFIRMED — tape path is fence-clean |
| StructRegistry IS live in the EAGER arena path | `crates/core/src/runtime/bbnf/arena.rs:47` `StructRegistry::compound_kind_for_layout(layout)` (runtime call); `arena_template.rs:64` doc-comment only | CONFIRMED — the live coupling-site is in the FOLD-B deletion target, not the tape path |
| SoA `Tape` retains `payloads: PayloadArena` | `skinny/crates/runtime/src/tape/mod.rs:99` `payloads: PayloadArena` (a retained struct member) | CONFIRMED — a retained substrate member whose substrate_target no dossier declares (CH5-V1-001) |
| No cross-call classifier state | `LOCKS.md:137-149` Lock 1 v+1 ELEVATION; dossiers declare `retention_lifetime=transient-single-call` for the classifier | CONFIRMED |

---

## §3W lens-axis dispositions (per dossier section)

### 2a — SOTA-landscape (substrate folds)

| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| FOLD-2A-A flat-tape one-encoding | ACCEPT | Substrate-union-preserving; dual AoS/SoA named a transient fold-state only, end-state = Lock-1 violation (`2a:101-104`). No parallel substrate. |
| FOLD-2A-B eager OpenFrame retirement | ACCEPT | Deletes the `Vec<OpenFrame>::clone` 86.07% pathology Lock 1 names; retirement REDUCES coupling. The 22+-file revert slice is the deletion of a parallel substrate, not the addition of one. |
| FOLD-2A-C lazy `ValueRef<G>` plane | ACCEPT | "the `ValueRef` borrows `&'doc Tape<'input>`, the `&'i Tape<'i>` + cursor shape Lock 1 names" (`2a:174-175`) — view rides the one tape, no second value tree. |
| FOLD-2A-D tape = substrate-manifest, not 6th shape | ACCEPT | The load-bearing CH5 row. Explicit "PROPOSE, do not silently add" (`2a:185-186`); `substrate_target=existing_tape`; LAC-1E-14 precedent cited at the right lines. Coupling-clean. |
| FOLD-2A-E NEON classifier Lock-16 entry | **REVISE** | The `Vec<u32>` index is correctly named "the index IS the tape's `offsets`, not a parallel retained vector — REDRESS-53 fence" (`2a:215-216`). But the fold states `substrate_target=existing_tape` for the *classifier output* without reconciling the LIVE `OnceCell<StructuralIndex>` retention (`json.rs:701`): when the index is held in a `OnceCell` separate from a wired tape it is `local_temp_only` at best and a REDRESS-53 sidecar at worst. 2a names `existing_tape` as if settled; the live state is `OnceCell`-retained and UNclassified. Fix: cross-reference FOLD-2A-E's `substrate_target` claim to the F7/U-2E-02 all-8-carrier classification as a PRE-condition (the index is not yet `existing_tape` — it is a retained OnceCell pending classification). |
| FOLD-2A-F StructRegistry/FieldSource fence | **REVISE** (shared with 2c/2d/2e/2f) | The fence is correctly framed as compile-time-only, but 2a (like all five fence-carriers) describes the per-leaf lookup ABSTRACTLY and never names the LIVE coupling-site `bbnf/arena.rs:47` `StructRegistry::compound_kind_for_layout(layout)` — a runtime `StructRegistry` call that exists TODAY in the eager arena path. A fence that does not name the live wire it severs is a fence in prose only. Fix: name `crates/core/src/runtime/bbnf/arena.rs:47` as the live runtime-`StructRegistry` coupling-site retired by FOLD-B, distinguishing it from the fence-clean `begin_compound` tape path (`tape/mod.rs:185-186`, grep-zero `StructRegistry`). |

### 2b — Primitive-vocabulary (Layer 0/Layer 1)

| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| A1 two-layer vocabulary (Layer 0 vendored / Layer 1 Rust) | ACCEPT | The dependency is one-directional and clean: "Rust intrinsic modules … include Layer 0, never vice-versa" (`2b:70`). No Layer-0↔Layer-1 coupling; the divergence (bbnf.asm macros vs Rust `PrimitiveKernels`) is a realization-medium reconcile, not a coupling. CH5-clean. |
| A4 / refuted-row: `Vec<u32>` index IS the tape, no sidecar | ACCEPT | "The primitive produces; the tape consumes once (L4). No sidecar. CH5-clean." (`2b:79`) — the exact transient-producer discipline CH5 requires. |
| FOLD-L2 `push_plain_offset` tape-append | ACCEPT | `substrate_target=existing_tape`; "append into the one tape, no `Vec<OpenFrame>` clone" (`2b:182-183`). Coupling-clean. |
| FOLD-L4 tokenize-once shared-scan reuse | ACCEPT | Explicitly bound: "the index IS tape … never a retained parallel vector (REDRESS-53)" scoped to ALL 8 carriers (`2b:208-211`). Strong CH5 discipline. |
| FOLD-L5 `comment_body_mask_64` / FOLD-L6 `bracket_depth_mask_64` | ACCEPT | Both carry `retention: within-block-only` / `within-call-only (Lock 1 v+1)` (`2b:229,:245`); the `depth_carry` is "threaded WITHIN a single `scan_components_to_index` call, init-0-per-parse, NEVER retained across calls" (`2b:236-238`). This is the exact no-cross-call-carry discipline the Lock 1 v+1 ELEVATION mandates. CH5-clean. |
| FOLD-L7 `CapacityPlan::OneShotSimd` | ACCEPT | "sizes the EXISTING `offsets` vector … no second vector" (`2b:249-250`). No new substrate. |
| FOLD-L8 sparse-flag side-table | **REVISE** | The claim "the side-table adds no substrate (CH5-clean)" + "Lock 1 (no new substrate — the sparse pair already exists)" (`2b:269-270`) is correct that `flag_cursors`/`flag_values` already exist (verified `tape/mod.rs:97-98`). BUT the row never declares the side-table's `substrate_target` / `retention_lifetime`. The sparse pair is a RETAINED member of the `Tape` (lives for the tape's lifetime, not a single call) — so its honest classification is `substrate_target=existing_tape` / `retention_lifetime=output_row` (it IS part of the tape), NOT "no substrate." "Adds no substrate" understates: it is part of the ONE substrate, and saying "no substrate" risks T-P3 reading the flag pair as un-manifested. Fix: classify the sparse-flag pair explicitly as `existing_tape` (a member of the one tape), not "no substrate." |
| FSM/frame-stack macros refuted; udot-CSS refuted | ACCEPT | Both correctly filed `source-present-unwired`/`architectural-block-with-REDRESS`; the `CollapsedStage` spine kept x86-pinned + aarch64-refused. No orphan-coupling admitted. |

### 2c — Grammar-neutrality

| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| SK17-2C-A flat tape grammar-neutral substrate | ACCEPT | Fence: "the surviving encoding must remain grammar-column-free; sparse flags only" (`2c:107-108`) — bars the AV.04 dense class column, the historical parallel-substrate shape. CH5-clean. |
| SK17-2C-B OpenFrame retirement | ACCEPT | Retirement reduces coupling; no-delete-before-replacement fence (`2c:130`). |
| SK17-2C-C lazy `ValueRef<G>` | ACCEPT | "the projection IS over the one tape, no second value tree" (`2c:153`). |
| SK17-2C-D tape = substrate-manifest, not 6th shape | ACCEPT | Explicit; FactStream precedent at ARCH `:1803` cited; "any proposal that reads as a 6th `BackendShape` is REJECT (no silent shape)" (`2c:175-176`). The strongest CH5-axis framing of the five. |
| SK17-2C-E NEON classifier | ACCEPT | "no retained cross-call classifier state (Lock 1 v+1 ELEVATION) — the alphabet is per-call constructed" (`2c:201-202`). Coupling-clean. |
| SK17-2C-F FieldSource compile-time fence | **REVISE** (shared) | Same fence-naming gap as 2a-F: 2c states "any per-leaf `StructRegistry::layout`/`Arena<G>`/`Builder<G>` in the runtime hot path is REJECT" (`2c:223-224`) — but never names the LIVE `arena.rs:47` runtime-`StructRegistry` call that is the actual present-tense coupling. The fence names the abstract class, not the live wire. Fix per the shared-REVISE remedy below. |
| SK17-2C-ONBOARD future-grammar gate | ACCEPT | The onboarding test IS a coupling falsifier (a generic-crate grammar branch = coupling = FAIL). Strengthens CH5. |
| CH5 self-anchor (`2c:333-335`) | ACCEPT | Self-pre-empt is accurate as far as it goes; the PayloadArena/sparse-flag declaration gaps (CH5-V1-001/002) are the residual it did not reach. |

### 2d — Cost-model + 5-shape

| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| FOLD-2D-01 tape = substrate-manifest not 6th shape | ACCEPT | The load-bearing D verdict, exact LAC-1E-14 reuse; 5-shape domain held verbatim (`2d:84-86`). |
| FOLD-2D-02 cost selects per-rule INTO the one tape | ACCEPT | "a shape selecting a NEW substrate is a CSP-INFEASIBLE plan" (`2d:274`); the e-graph rejects non-admitted `substrate_target` (LAC-2D-06). This is the cost-model FSM/CollapsedStage keeping the mask stream a transient producer — the CH5 §3W requirement on 2D explicitly. Coupling-clean. |
| FOLD-2D-03 lazy `ValueRef<G>` plane | ACCEPT | Not a shape; read strategy over a retaining shape. No coupling. |
| FOLD-2D-04 AoS↔SoA single-encoding | ACCEPT | "A dual AoS/SoA end-state is a Lock-1 violation … not a tree-local option" (`2d:161-162`). |
| FOLD-2D-05 NEON classifier scan-cost fact | ACCEPT | "never *retains* state across calls"; transient `Vec<u32>` = `existing_tape`/`local_temp_only` (`2d:182-185`). The Lock 1 v+1 ELEVATION cited (`:196-198`). Coupling-clean. |
| FOLD-2D-06 FieldSource compile-time fence | **REVISE** (shared) | 2D's `begin_compound` anchor is exact (`tape/mod.rs:185-186`), and the row is correct that "a naive per-leaf `StructRegistry::layout(rule)` (`struct.rs:331`) re-opens" the regression. But — like the other four — it cites the per-leaf lookup as a HYPOTHETICAL ("a naive … would") and does not record that a LIVE runtime `StructRegistry::compound_kind_for_layout` call EXISTS TODAY at `arena.rs:47` in the eager path the fold deletes. The fence is sound but under-anchored: it must name the present-tense coupling-site so the fold's deletion of arena.rs is recognized as the act that severs it. Fix per shared remedy. |
| FOLD-2D-07 aarch64 CollapsedStage UNKNOWN-2D-05 | ACCEPT | NEON under four LLVM shapes' scan-leaf FFI; no aarch64 CollapsedStage; x86 mechanically refused. No hidden x86 coupling. |

### 2e — Host-arch esoterica

| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| FOLD-2E-A..F (six folds) | ACCEPT | Each substrate-coupling claim re-anchored; the explicit "Fold Coherence Note (CH5 hidden-coupling pre-empt)" (`2e:356-368`) correctly sequences the six folds as co-dependent-not-orthogonal and states "No fold implies a parallel substrate, a sidecar producer, or a Lock-1 violation." The mask stream named transient (plane 4); the OnceCell classified before wiring. The single residual is the shared fence-naming gap (folded into the shared REVISE, not double-counted here) and the PayloadArena declaration gap (CH5-V1-001, raised at its primary site 2b/2f). 2e is the cleanest CH5 dossier. |

### 2f — Fold-gaps

| § / candidate | disposition | rationale (CH5 axis) |
|---|---|---|
| F1 OpenFrame retirement / F3 AoS↔SoA closure / F4 tape-substrate-not-6th-shape | ACCEPT | F4 is the primary D-refutation, dispatch-instruction-exact ("propose, do NOT silently add a 6th" discharged in the negative, `2f:240-241`). 5-shape domain held. Coupling-clean. |
| F2 lazy `ValueRef<G>` plane | ACCEPT | One generator, view over the one tape; no second cursor/builder type (SPEC `:532-537`). |
| F5 NEON classifier manifest row | ACCEPT | Declares `retention_lifetime=transient-single-call`, `substrate_target=existing_tape`, same-wave consumer = the tape (`2f:272-273`). Coupling-clean. |
| F6 StructRegistry/FieldSource fence | **REVISE** (shared) | 2f is the most rigorous of the five fence-carriers (it names `struct.rs:84,202,313,331` and the `begin_compound` property) — but it too frames the per-leaf walk as the hypothetical REJECT and omits the LIVE `arena.rs:47` runtime-`StructRegistry` coupling-site. Since 2f is the fold-gaps dossier, it is the right home to record the present-tense coupling the fold severs. Fix per shared remedy. |
| F7 OnceCell substrate_target classification (all 8 carriers) | ACCEPT | The single best CH5 row in the set: it names the LIVE `OnceCell<StructuralIndex>` retention (verified `json.rs:701`), mandates classification to `existing_tape`/`local_temp_only` BEFORE wiring, and names REDRESS-53 re-entry as the failure (`2f:336-361`). This is precisely the retained-sidecar fence CH5 demands. |
| F8 BackendShape selector wiring | ACCEPT | The selector outputs a projection mode, never a substrate; `substrate_target` binding on every `BackendExpr` (`2f:394`). No coupling. |
| F9 StructLayout rename | ACCEPT | Identifier reconcile; grammar-blind; no coupling. |
| CH5 pre-emption (`2f:505-508`) | ACCEPT | Accurate; the PayloadArena (CH5-V1-001) and the sparse-flag-as-`existing_tape` (CH5-V1-002) declaration gaps are the residuals beyond its reach. |

---

## The four hidden-coupling residuals (REVISE, with concrete fixes)

### CH5-V1-001 — `PayloadArena` substrate_target / retention_lifetime UNDECLARED
**Where.** `2b:190` (FOLD-L3), `2f:436` (F2 defended-assertion), `2e:134` (FOLD-2E-B),
`2a:148`-adjacent (FOLD-2A-C — names the lazy plane reconstructed "node kind recovered
from the source byte … `PayloadArena` the bounded escape hatch"). **All four name
`PayloadArena` as the escape hatch for irreducible scalars; none declares its
substrate-manifest classification.**
**Coupling.** `PayloadArena` is a RETAINED member of the SoA `Tape`
(`skinny/crates/runtime/src/tape/mod.rs:99` `payloads: PayloadArena`) — it lives the
tape's full lifetime, not a single call. Per `LOCKS.md:118-127` every retained
structure MUST declare `substrate_target` / `retention_lifetime` / `policy_owner`. An
"escape hatch" named in prose with no manifest classification is exactly the implicit
retained-substrate hole the Lock 1 v+1 manifest closes. Left implicit, T-P3 could
inherit `PayloadArena` as an un-manifested second retained store beside `offsets` —
the parallel-substrate shape Lock 1 forbids.
**Fix.** Each dossier that names `PayloadArena` declares it `substrate_target =
existing_tape` (it is a MEMBER of the one tape, not a sidecar) / `retention_lifetime =
output_row` / `policy_owner = generated_grammar`, AND states the bound: payloads are
written ONLY for irreducible scalars that cannot be re-derived from the source byte at
the offset (the `PayloadArena.write_count==0 on re-readable leaves` invariant 2b
already cites at `:184`). This makes the escape hatch a declared part of the ONE
substrate, not an undeclared retention. Primary owner: 2b-L3 + 2f-F2; the others
cross-reference.

### CH5-V1-002 — sparse-flag side-table "adds no substrate" understates its tape-membership
**Where.** `2b:269-270` (FOLD-L8): "the side-table adds no substrate (CH5-clean)" /
"Lock 1 (no new substrate — the sparse pair already exists)".
**Coupling.** Correct that `flag_cursors`/`flag_values` pre-exist (`tape/mod.rs:97-98`).
But "adds no substrate" is the wrong frame: the sparse pair IS part of the one
substrate (the `Tape`), retained for the tape's lifetime. Saying "no substrate" invites
T-P3 to read the flag pair as un-manifested — the same implicit-retention risk as
CH5-V1-001. The honest CH5 statement is "part of the ONE substrate," not "no
substrate."
**Fix.** Reword FOLD-L8's Lock-surface to: `substrate_target = existing_tape`
(the sparse pair is a member of the one tape) / `retention_lifetime = output_row`;
the side-table is part of the single substrate, paid only where non-zero — it adds
no SECOND substrate. Keep the branch-tag guard (Lock 14) unchanged.

### CH5-V1-003 — the StructRegistry fence names an abstract per-leaf lookup, not the LIVE coupling-site (SHARED across 2a-F / 2c-F / 2d-06 / 2e-F / 2f-F6)
**Where.** `2a:259-261`, `2c:223-224`, `2d:206-207`, `2e:256-257`, `2f:489` — all five
frame the fence as "a NAIVE per-leaf `StructRegistry::layout(rule)` WOULD re-open" the
28-65×/983×/10583× regression. All five cite `struct.rs:331` `layout(rule_id)` as the
hypothetical hazard.
**Coupling.** The fence is sound but UNDER-ANCHORED: a LIVE runtime `StructRegistry`
call exists TODAY at `crates/core/src/runtime/bbnf/arena.rs:47`
(`StructRegistry::compound_kind_for_layout(layout)`) in the eager arena path. The
fence-carriers describe the coupling as a future hypothetical and never record that
the present-tense coupling already exists in the path FOLD-B deletes. A fence that does
not name the live wire it severs cannot prove the wire is severed. (The tape path is
verified fence-clean: `tape/mod.rs:185-186` `begin_compound` reads `layout.rule_id &
0x1F` only; `grep StructRegistry tape/mod.rs` = 0.)
**Fix (one shared edit, applied by the fence-owning dossier 2f-F6, cross-referenced by
the others).** Name `crates/core/src/runtime/bbnf/arena.rs:47`
`StructRegistry::compound_kind_for_layout(layout)` as the LIVE runtime-`StructRegistry`
coupling-site in the eager path; state that FOLD-B's deletion of the eager arena
builders is the act that severs it; and contrast it with the fence-clean
`begin_compound` tape path (grep-zero `StructRegistry`). This converts the fence from
"a future lookup would be bad" to "this present lookup is the coupling the fold
removes" — a falsifiable fence, not a prose one.

### CH5-V1-004 — FOLD-2A-E asserts `substrate_target=existing_tape` for the classifier output without reconciling the LIVE OnceCell retention
**Where.** `2a:215-216` (FOLD-2A-E): "the index IS the tape's `offsets`, not a parallel
retained vector — REDRESS-53 fence" + Lock-surface "`substrate_target=existing_tape`".
**Coupling.** The classifier's `Vec<u32>` is `existing_tape` ONLY once the tape is
wired and the index has become its `offsets`. TODAY the index is held in a retained
`OnceCell<StructuralIndex>` (`json.rs:701`) SEPARATE from any wired tape — i.e. its
present-tense classification is `local_temp_only` (pending) or, if mis-wired beside a
tape, a REDRESS-53 sidecar. 2a-E states `existing_tape` as if settled, eliding the
F7/U-2E-02 pre-condition that 2f and 2e carry correctly. The dossier set is internally
INCONSISTENT here: 2f-F7 / 2e-U-2E-02 say "classify BEFORE wiring, could be either
value"; 2a-E says "existing_tape" flatly.
**Fix.** FOLD-2A-E cross-references F7 (`2f:336-361`) / U-2E-02 (`2e:336`): the
classifier output's `substrate_target` is RESOLVED to `existing_tape` only after the
all-8-carrier OnceCell classification and tape wiring; until then it is the retained
`OnceCell` pending classification. State the pre-condition rather than the settled
value.

---

## What the lens did NOT find (the strong-coupling axes that PASS)

- **No silent 6th `BackendShape`.** Every D-fold proposes the substrate-manifest
  CATEGORY explicitly (LAC-1E-14), and every dossier states a 6th shape is G-Omega
  gated. Verified the canon is held verbatim (`LOCKS.md:107-108`) and SPEC `:807` bars
  the sixth shape in the second-substrate REJECT block. This is the lens's primary
  charge and the dossiers clear it cleanly.
- **No parallel substrate / sidecar.** The eager `OpenFrame` `Vec<OpenFrame>::clone`
  (the 86.07% samply pathology) is the DELETION target, not a carry-forward; the mask
  stream is a transient producer (`local_temp_only`); the `Vec<u32>` index is the tape's
  `offsets` (index IS the tape), fenced against REDRESS-53.
- **No cross-call classifier state.** The Lock 1 v+1 ELEVATION (`LOCKS.md:137-149`) is
  honoured: L5/L6 masks carry `within-block-only`/`within-call-only`; the classifier
  declares `transient-single-call`. The `depth_carry` is init-0-per-parse, never
  retained.
- **Layer 0 / Layer 1 clean two-layer dependency.** 2b keeps the dependency
  one-directional (Rust modules include Layer 0, never vice-versa); the bbnf.asm-vs-Rust
  divergence is a realization-medium reconcile, not a coupling. The `CollapsedStage`
  spine is kept x86-pinned + aarch64-refused — no hidden x86 close-path coupling.
- **No re-opened REDRESS.** AZ-IV eager, StructRegistry indirection, fact-stream,
  broadcast, FNV, x86 — none re-grounded as viable. F-folds honour every pre-block.

---

## §3W disposition summary

- **ACCEPT: 18** (the six D-folds + tape-substrate category across all dossiers; the
  transient-producer/no-cross-call-state discipline; the OpenFrame retirement;
  the F7 OnceCell-classification fence; the Layer 0/Layer 1 cleanliness; 2e's
  coherence note).
- **REVISE: 4** (CH5-V1-001 PayloadArena undeclared; CH5-V1-002 sparse-flag
  "no substrate" mis-frame; CH5-V1-003 fence-site under-anchoring [shared, counted
  once]; CH5-V1-004 2a-E settled-vs-pending `substrate_target` inconsistency).
- **REJECT: 0** (no fold implies a parallel substrate, a silent 6th shape, a retained
  sidecar, or a Lock-1 violation; the fold direction is correct throughout).
- **Accept-rate (CH5 axis): 18/22 = 81.8%.**

All four REVISEs are *declaration-completeness* defects on the substrate manifest —
not architectural reversals. The fold's coupling posture is correct; the manifest
classification of two retained tape members (`PayloadArena`, the sparse-flag pair) and
one live coupling-site (`arena.rs:47`) must be made EXPLICIT so T-P3 cannot inherit an
unclassified retained substrate or an unfalsifiable fence. Folding these four converts
the dossier set from "coupling-clean by assertion" to "coupling-clean by manifest."

First hygiene action CH1-V5-001: verified RESOLVED on disk (not carried); no edit
required.
