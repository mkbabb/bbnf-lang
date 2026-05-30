---
lens: CH5-HIDDEN-COUPLING
pass: T-P3-synthesis
cycle: V1
subject: SK-V17 T-P3 synthesis artefacts (3A-3F + 3c-locks-v+1-diff.md)
reviewer: CH5 HIDDEN-COUPLING (V1)
master_head: 2a76916ac
generated_at: 2026-05-29T00:00:00Z
scan_focus: substrate-union (Lock 1) holds; tape is substrate-manifest CATEGORY not silent 6th BackendShape; 5-shape canon coherent across 3A/3B/3E; no parallel substrate/sidecar; StructRegistry/FieldSource fence intact; no Track-1≡Track-2 dishonesty
verdict: CONVERGENT-WITH-REVISE
counts:
  accept: 21
  revise: 4
  reject: 0
---

# CH5 HIDDEN-COUPLING — SK-V17 T-P3 V1

## Scope of this lens (PASS-3 §3 CH5 + §8.1/§8.2)

CH5 scans for: a proposed delta implying a **parallel substrate**, a **sidecar
producer**, a **renamed-scanner Lock-1 violation**, or a **Track 1 ≡ Track 2
dishonesty**; the **substrate union holds** across every 3A surface delta; 3C's
accepted amendments **introduce no coupling**; the tape is a substrate-manifest
**CATEGORY** not a silent **6th BackendShape** (the 5-shape canon coherent
across 3A/3B/3E); the **StructRegistry/FieldSource fence** is intact.

## Verification ledger (anchors confirmed at master_head 2a76916ac)

| anchor | claim | verified | result |
|---|---|---|---|
| `restart/locks/LOCKS.md:75` | Lock 1: parallel substrates dead; "if structural offsets are retained, the structural projection IS the tape" | yes | HOLDS — the no-parallel-substrate spine the fold rides |
| `restart/locks/LOCKS.md:100`-`127` | LAC-1E-14 FactStream = "5th admitted-product category at the Lock 1 substrate manifest … NOT a 6th `BackendShape` variant"; substrate_target vocab `{local_temp_only, existing_tape, direct_sink, admitted_fact_output}` | yes | HOLDS — the load-bearing categorical precedent the tape-fold reuses verbatim; `existing_tape` is an admitted value (`:121`) |
| `restart/locks/LOCKS.md:107`-`108` | 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` | yes | HOLDS verbatim |
| `restart/locks/LOCKS.md:137`-`149` | retention_lifetime vocab `{transient-single-call, retained-within-chunk, retained-across-call-boundary}`; third value REJECT; "no cross-call retained classifier state" | yes | HOLDS — NEON clause's `transient-single-call` is the admissible value |
| `skinny/crates/runtime/src/tape/mod.rs:94`-`100` | SoA `Tape<'input>` with `payloads: PayloadArena` as a MEMBER | yes | HOLDS — PayloadArena is a tape member (line 99), not a sidecar |
| `skinny/crates/runtime/src/tape/mod.rs:175` | `ValueRef<'doc,'input,K,G:EventGrammar>` | yes | HOLDS — grammar enters as TYPE param, zero runtime arm |
| `crates/core/src/runtime/tape/mod.rs:185`-`186` | tape `begin_compound` reads `layout.rule_id & 0x1F` only | yes | HOLDS — grep-zero StructRegistry; fence-clean |
| `crates/core/src/runtime/bbnf/arena.rs:47` | `match StructRegistry::compound_kind_for_layout(layout)` | yes | HOLDS — live runtime StructRegistry coupling, in eager `bbnf/builder.rs:102` path |
| `restart/skinny/tranches/sk-v17/SPEC.md:793`-`795` | 28-65×/983×/10583× StructRegistry per-leaf regression | yes | HOLDS |
| `restart/skinny/tranches/sk-v17/SPEC.md:808` | §9 bar: "sixth `BackendShape`, retained sidecars, sidecar event" = second-substrate bar | yes | HOLDS — directly backs no-6th-shape disposition |

The five LACs and the v+1 diff carry **no new directive, no new BIR variant, no
new substrate, no public substrate API, no retained sidecar, no sixth shape**.
The substrate-union spine (Lock 1) is preserved on every surface I scanned.

## Disposition by artefact / delta

### 3A-architecture-synthesis.md

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `ARCH-3A-S17-D01` tape-as-unified-substrate (eager-OpenFrame retire + AoS→SoA single encoding) | **ACCEPT** | Exactly-one-encoding obligation cited (`LOCKS.md:75`); dual AoS/SoA admissible only as transient fold-state; eliminates the `Vec<OpenFrame>::clone` parallel-substrate pathology Lock 1 names. No parallel substrate implied. |
| `ARCH-3A-S17-D02` lazy `ValueRef<G>` value-plane | **ACCEPT** | One materialization plane replacing per-grammar eager enums; grammar via TYPE param (`tape/mod.rs:175`), zero runtime branch; preserve-rich-ast held. No coupling. |
| `ARCH-3A-S17-D03` shared NEON classifier manifest | **ACCEPT** | `retention_lifetime=transient-single-call` (admissible per `LOCKS.md:145`-`149`); `substrate_target=existing_tape`; same-wave consumer = tape. No cross-call retained classifier state — the strictest Lock-1 v+1 ELEVATION clause is honoured. |
| `ARCH-3A-S17-D04` BackendShape-category disposition | **ACCEPT** | Tape recorded at substrate manifest per LAC-1E-14 precedent, NOT a 6th shape; two independent grounds (categorical precedent + `admits_collapsed_stage` x86-binding). 5-shape canon verbatim. Coherent with 3B-D03 and 3E-D05. |
| `ARCH-3A-S17-D05` StructRegistry/FieldSource fence ("the live coupling-site `arena.rs:47` is severed by the eager-OpenFrame retirement (D01)") | **REVISE** | CH5-V1-R01 below. The severance is real but the enumeration is imprecise: `arena.rs:47` is reached through `bbnf/builder.rs:102` (the bbnf eager builder), NOT the css_l4 OpenFrame path the surrounding D01 prose centres on. css_l4 and json builders have ZERO StructRegistry coupling; the other 6 grammars use a LOCAL `match layout.rule_id`. The fence holds; the cite is under-specified. |
| `ARCH-3A-S17-D06` Lock-2 `StructLayout` reconcile note | **ACCEPT** | Pricing note; no substrate coupling. Path-(b) `LayoutFacts.backend_shape` side-table is a side-table field (Lock 10 `:269`), not a new substrate. |
| `ARCH-3A-S17-D07` BackendShape selector wiring ("WIRES, does not build"; `backend_shape` side-table field) | **ACCEPT** | Side-table field, grammar-author-blind; wires existing `crates/egraph`+`crates/csp-solver`. No new substrate, no surface annotation coupling. |
| `ARCH-3A-S17-D08` three-ORQ disposition | **ACCEPT** | U2 names REDRESS-53 re-entry as the parallel-index hazard with the OnceCell `substrate_target` pre-gate as the guard — the exact CH5 coupling fence. No engineered-defer. |
| CH5 Open Question row (PayloadArena `substrate_target=existing_tape`) | **ACCEPT** | The artefact pre-raises the CH5 concern and resolves it: PayloadArena is a tape MEMBER (`tape/mod.rs:99`, verified live), manifest-declared `existing_tape`/`output_row`/`generated_grammar`. Implicit-retention sidecar hole closed. |

### 3B-master-plan-reconciliation.md

| delta / claim | CH5 disposition | basis |
|---|---|---|
| B-tranche row: "the eager `OpenFrame` is the fold-DELETION target, not a sibling … No parallel substrate" | **ACCEPT** | The substrate-union framing is correct: deletion, not coexistence. |
| H.W1 row: "F7's `OnceCell<StructuralIndex>` substrate_target pre-gate enforces the structural projection IS the tape … No new BIR variant, no new directive" | **ACCEPT** | Aligns with `LOCKS.md:75`; the OnceCell pre-gate is the anti-parallel-index fence. |
| H.W4 / MP-3B-SKV17-D03: tape = Lock-1 substrate-manifest CATEGORY, explicitly NOT a 6th BackendShape, across §13/§13.5/§13.1 | **ACCEPT** | 5-shape canon coherence delta; coherent with 3A-D04 and 3E-D05. §13.2 MP.NW6 already states the same canon (`MASTER-PLAN.md:640`). |
| Refuted-Route Confirmation table (D6 second substrate / dual AoS-SoA / fact-stream-String / x86) | **ACCEPT** | Every refuted route is carried as a FENCE, not revived; no parallel-substrate revival. |
| MP.SK18.W5 row: F5 NEON manifest + F8 selector "5-shape canon PRESERVED — F4 disposes the tape as substrate-manifest category, not a 6th shape" | **ACCEPT** | Coherent canon statement on the wave surface. |
| MP-3B-SKV17-D08 "F3 exactly-one-encoding closure … A dual AoS/SoA end-state or a retained index parallel to a wired tape re-opens REDRESS-53" | **ACCEPT** | Names the parallel-substrate hazard and its REDRESS receiver precisely. |
| CH5 Open Question row (which 3C v+1 text classifies the tape so no MASTER cross-ref reads as a 6th shape) | **ACCEPT** | The cross-surface coherence dependency is correctly routed to 3C; substrate-union holds. |

### 3C-locks-crystallisation.md + 3c-locks-v+1-diff.md

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `D-SKV17-L01` tape-substrate-union clause (eager-OpenFrame retire + SoA single encoding + all-8 OnceCell substrate_target + StructRegistry fence) | **ACCEPT** | The clause is the substrate-union spine restated: exactly ONE encoding survives; dual end-state REJECT; per-leaf `StructRegistry::layout(rule)` runtime walk REJECT; OnceCell carriers must resolve `existing_tape` or `local_temp_only`. No coupling introduced; every hazard fenced. The `arena.rs:47` severance cite carries the same R01 imprecision as 3A-D05 but the fence logic is sound. |
| `D-SKV17-L10` tape-category-not-sixth-shape clause | **ACCEPT** | Crystallised in the negative on LAC-1E-14 precedent (`LOCKS.md:100`-`116`) + `admits_collapsed_stage` x86-binding; 5-shape domain verbatim; 6th remains G-Omega gated. The binding coherence clause; no silent 6th shape. |
| `D-SKV17-L14` ValueRef/classifier-generalisation: "The shared classifier's grammar-generality is config-breadth (alphabet-as-data) across 8 of 9 generated grammars — a SEPARATE axis from the value-fold, never the same as fleet-wide value-plane proof" | **ACCEPT** | This is the explicit Track-1≡Track-2 anti-dishonesty fence: the clause keeps the classifier config-breadth axis DISJOINT from the JSON+CSS-only value-plane proof, refusing to launder one into the other. Exactly what CH5 requires. |
| `D-SKV17-L16` NEON-classifier-manifest: `retention_lifetime=transient-single-call`; eq-set fan the one real NEON body; table/prefix honest scalar-delegate-non-ASM | **ACCEPT** | No cross-call retained classifier state; the renamed-scanner Lock-1 violation is structurally impossible (transient single-call only). No sidecar producer. |
| `D-SKV17-L02` StructLayout-reconcile (MODIFY) | **ACCEPT** | Pricing-only; path-(b) `LayoutFacts.backend_shape` is a side-table, not a new substrate. No coupling. |
| Disposition tally (9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER) and v+1 diff `@@ -606,6 +606,52 @@` | **REVISE** | CH5-V1-R02 + CH5-V1-R03 below. The diff Invariant-Check is sound (16 locks, 5 shapes, no new substrate); the residual concerns are (a) the diff's "0 MODIFY" surface-count vs 3c-locks-crystallisation's "2 MODIFY" — a frontmatter coherence gap, and (b) the addendum is appended as a section AFTER the SK-V15 addendum, deferring Lock-body distribution to Pass Omega — a placement coupling that should name the distribution invariant. |
| Refutation rows (6th BackendShape REFUTED; per-leaf StructRegistry REFUTED; AoS/SoA dual end-state REFUTED; fleet-wide value-plane REFUTED; x86/SVE REFUTED) | **ACCEPT** | Every load-bearing refutation preserved as a REJECT-class clause; no revival; the substrate-union and 5-shape spine fully fenced. |

### 3E-grammar-generalisation.md

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `3E17-D02` tape-substrate grammar-column-free fence ("flags are sparse position-keyed side-vectors … a kind-partitioned dense class column would be the AV.04/REDRESS-96 barred shape") | **ACCEPT** | This is the strongest CH5 fence in the packet: it pre-blocks the dense per-grammar class column that would be a parallel-substrate-by-stealth. Verified against `tape/mod.rs:94`-`100` (offsets/flag_cursors/flag_values/payloads — sparse, position-keyed). No grammar column. |
| `3E17-D05` per-grammar BackendShape matrix = substrate category (5-shape verbatim; CollapsedStage aarch64-refused; "any proposal reading as a 6th `BackendShape` is REJECT") | **ACCEPT** | The per-grammar matrix is the §8.2 coherence surface; it restates shape assignments unchanged and the substrate relationship explicit. Coherent with 3A-D04, 3B-D03, 3C-L10. No 6th shape. |
| `3E17-D07` by-construction-not-by-exercise scoping ("with only one of Sheets/BBNF-self witnessed, the claim is SCOPED … may not use fleet-wide wording") | **ACCEPT** | The honest-scope fence: prevents the value-plane proof being mis-stated fleet-wide — the Track-1≡Track-2 dishonesty CH5 guards. JSON+CSS by-exercise; Sheets/BBNF-self by-construction. |
| `3E17-D09` FieldSource compile-time projection fence ("a per-leaf runtime `StructRegistry::layout` walk is BOTH the refuted 28-65×/983×/10583× indirection AND a grammar-shaped runtime dispatch") | **ACCEPT** | Couples the perf fence to the grammar-neutrality fence correctly: the runtime walk would be BOTH a regression AND a hidden grammar coupling. Compile-time emission keeps both. Verified `begin_compound` is grep-zero StructRegistry (`tape/mod.rs:185`-`186`). |
| P1 leak-census predicate: "7 hits, all `crates/ir/src/registry/strategy.rs` … string-ident registry + doc-comment examples … NOT runtime `match grammar {}` arms" with monotonic-decrease-to-zero | **REVISE** | CH5-V1-R04 below. The honest catalogue of the 7 grammar-name leak sites is correct and laudable (no paper-clean claim), but the predicate routes them under "monotonic-decrease-to-zero" without naming WHICH SK-V18 wave owns the decrease — a latent string-ident coupling left without an explicit receiving wave. |
| Per-Grammar matrix rows + Primitive Vocabulary Transfer (alphabet-as-data; `JsonSink` is NOT a generic contract `ARCHITECTURE.md:1269`) | **ACCEPT** | No JSON-narrowing; the sink contract is per-grammar generated, not a JSON-coupled generic. Substrate-union preserved across all grammar columns. |

### 3D / 3F (CH5 surface scan)

| artefact | CH5 disposition | basis |
|---|---|---|
| 3D-skinny-fold (monotonic skinny→totality; SKINNY-proven wins V1-authoritative) | **ACCEPT** (surface scan) | The monotonic direction never relocates core constructs into skinny nor dictates back; no parallel-substrate fold. Not the CH5 primary surface; no coupling found on scan. |
| 3F-migration-handoff (rename/abrogate deltas + next-cycle directive) | **ACCEPT** (surface scan) | Rename/abrogate decisions carry no substrate coupling; the next-cycle directive routes SK-V18 entry through the substrate-union + OnceCell pre-gates. No CH5 violation on scan. |

## REVISE findings (4)

### CH5-V1-R01 — `arena.rs:47` severance cite is under-specified (3A-D05, 3C-L01)

3A-D05 and 3C-L01 both name "**the** live coupling-site `crates/core/src/runtime/bbnf/arena.rs:47`" (definite, singular) as the StructRegistry coupling severed by the eager-OpenFrame retirement, and frame it within D01's css_l4-OpenFrame-centred prose. The verified reality at master_head:
- `crates/core/src/runtime/bbnf/arena.rs:47` `match StructRegistry::compound_kind_for_layout(layout)` IS the sole live runtime `StructRegistry::compound_kind_for_layout` coupling, reached through `crates/core/src/runtime/bbnf/builder.rs:102` (the **bbnf** eager OpenFrame `begin_compound`, line 99-105).
- `crates/core/src/runtime/css_l4/` and `crates/core/src/runtime/json/` builders have **ZERO** StructRegistry coupling (grep-empty).
- The other six grammars (`bnf`, `ebnf`, `csv`, `math`, `css_pretty`, `google_sheets`) call `from_layout` with a **local** `match layout.rule_id` (e.g. `bnf/kind.rs:19`-`27`) — already fence-clean.

**Concrete fix:** in 3A-D05 and 3C-L01, change the cite to name the caller and scope it to bbnf precisely: "the sole live runtime `StructRegistry::compound_kind_for_layout` coupling is `bbnf/arena.rs:47`, reached through the eager `bbnf/builder.rs:102` `begin_compound`; css_l4/json builders carry no such coupling and the remaining six grammars resolve `from_layout` via a local `match layout.rule_id`. The eager-builder retirement (D01/F1) severs the one bbnf site; the tape `begin_compound` is already grep-zero StructRegistry (`tape/mod.rs:185`-`186`)." The fence verdict (REJECT any per-leaf runtime walk) is unchanged; only the enumeration tightens. This is load-bearing because a future SK-V18 owner reading "arena.rs:47 severed by eager retirement" could miss that bbnf — not css_l4 — is the coupling carrier and over-scope or under-scope the severance.

### CH5-V1-R02 — MODIFY count incoherence between 3c-locks-v+1-diff and 3c-locks-crystallisation

`3c-locks-v+1-diff.md:39` states the per-row dispositions as "(9 ACCEPT, 3 ORQ-ACCEPT, **2 MODIFY**, 0 REJECT, 0 DEFER)" in the executive summary, while the same file's frontmatter has no disposition tally; `3c-locks-crystallisation.md:55` states "**9 ACCEPT, 5 MODIFY, 0 REJECT, 0 DEFER**" in its exec summary but its own Disposition tally (`:141`-`147`) lists 9 ACCEPT + 3 ORQ-ACCEPT + **2 MODIFY**. The "5 MODIFY" in the crystallisation exec summary is an internal contradiction (the tally says 2). CH5 flags this because a mis-stated disposition count is the surface where a silent coupling (a candidate dispositioned one way in prose, another in the gate object) can hide.

**Concrete fix:** reconcile to the verified tally — 9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER (14 candidates). Correct `3c-locks-crystallisation.md:55` "9 ACCEPT, 5 MODIFY" → "9 ACCEPT, 3 ORQ-ACCEPT, 2 MODIFY". The 2 MODIFYs (LAC-2F-FOLD-05, LAC-1E-SKV17-04, the Lock-2 reconcile pair) are correct; only the headline count is wrong.

### CH5-V1-R03 — addendum-as-section vs Lock-body distribution leaves a placement coupling unnamed

The v+1 diff appends one `## SK-V17 T-P3 Crystallisation Addendum` section after the SK-V15 addendum (`LOCKS.md:608`-`609`), bundling clauses for Locks 1/2/10/14/16 in one block, and defers the per-lock-body distribution to Pass Omega (3c-locks-crystallisation Open Question CH1 `:197`). This is governance-correct but leaves a CH5-relevant coupling unstated: if Pass Omega distributes the Lock-10 tape-category clause WITHOUT the cross-reference to the Lock-1 substrate manifest (`substrate_target=existing_tape`), the category placement loses its anchor and could later be misread as free-standing (the silent-6th-shape regression vector). The addendum's Lock-10 clause carries the cross-ref inline today, but a distribution that splits Lock-1 and Lock-10 must preserve it.

**Concrete fix:** add an Invariant-Check line to `3c-locks-v+1-diff.md` (Invariant Check section, after the "Numbered locks unchanged" bullet): "**Distribution invariant**: if Pass Omega distributes clauses into Lock bodies, the Lock-10 tape-category clause MUST retain its inline cross-reference to the Lock-1 substrate manifest (`substrate_target=existing_tape`, `LOCKS.md:118`-`127`); a Lock-10 clause severed from the Lock-1 manifest anchor re-opens the silent-6th-shape reading." This names the coupling the distribution must preserve.

### CH5-V1-R04 — P1 grammar-name-leak predicate lacks a named SK-V18 receiving wave (3E17-D08)

3E's P1 onboarding predicate honestly catalogues 7 grammar-name leak sites at `crates/ir/src/registry/strategy.rs` (`:132,:137,:149,:197-198,:292,:315`) and labels them "monotonic-decrease-to-zero (HEAD → 0)" — correctly refusing a paper-clean pass. But the decrease is not bound to a named SK-V18 wave the way the substrate/value/NEON folds are (3B MP.SK18.W0..W6). A latent grammar-name coupling (string-ident registry) left under "monotonic-decrease" without a receiving wave is a soft engineered-defer in CH5's coupling-firewall reading: the leak is the catalogued ARCH-3A-D09 surface, but no fold wave OWNS reducing it.

**Concrete fix:** in 3E17-D08 (and the P1 predicate row), name the receiver: route the 7 `strategy.rs` string-ident leak sites to the SK-V18 wave that owns the `ValueRef<G>` generator (MP.SK18.W3) or the Lock-14 leak-census gate, with the monotonic-decrease target tied to that wave's close gate — not an unowned "HEAD → 0". If no SK-V18 wave reduces them in-cycle, state the scoping rule (the leak is doc-comment + string-ident, NOT a runtime `match grammar {}` arm, and is admitted as a catalogued non-zero baseline under Lock-14 with a named future receiver). Either binds the coupling to an owner.

## Cross-surface 5-shape coherence verdict (§8.2)

The 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
is stated verbatim and the tape-as-substrate-category (not 6th shape) is
**coherent across all three required surfaces**:
- 3A-D04 (`ARCHITECTURE.md` §7.3/§9): category per LAC-1E-14 + `admits_collapsed_stage` x86-binding.
- 3B-D03 (`MASTER-PLAN.md` §13 H.W4/§13.1/§13.2): category, "5-shape canon STAYS UNCHANGED", §13.2 MP.NW6 self-consistent.
- 3E-D05 (`ARCHITECTURE.md` §7.3 matrix): per-grammar shapes unchanged, substrate relationship explicit, 6th-shape REJECT.
- 3C-L10 (`LOCKS.md` Lock 10): the binding coherence clause, two independent grounds.

No surface touches the canon without the others; no §8.2 coherence REJECT. The
tape is never a silent 6th shape on any surface.

## Substrate-union + sidecar verdict (Lock 1)

- **No parallel substrate**: every fold design DELETES the eager `OpenFrame`/AoS competitor; exactly-one-encoding obligation enforced; OnceCell carriers gated to `existing_tape`/`local_temp_only`.
- **No sidecar producer**: `PayloadArena` is a tape MEMBER (`tape/mod.rs:99`), manifest-declared `existing_tape`/`output_row`/`generated_grammar`; the NEON classifier is `transient-single-call` (no cross-call retained state, the strictest Lock-1 v+1 ELEVATION clause).
- **No renamed-scanner Lock-1 violation**: the classifier carries no retained mask/class/prefix stream across calls.
- **No Track-1≡Track-2 dishonesty**: 3C-L14 + 3E-D07 explicitly keep the classifier config-breadth axis DISJOINT from the JSON+CSS-only value-plane proof; no fleet-wide over-claim.
- **StructRegistry/FieldSource fence intact**: tape `begin_compound` grep-zero StructRegistry; the one live runtime coupling (`bbnf/arena.rs:47`) is in the eager path and severed by retirement; any per-leaf runtime walk is REJECT (28-65×/983×/10583×). (Enumeration tightened per R01.)

## CH5 verdict

**CONVERGENT-WITH-REVISE.** The substrate-union holds; the tape is consistently
a substrate-manifest CATEGORY, never a silent 6th shape; the 5-shape canon is
coherent across 3A/3B/3E; no parallel substrate, no sidecar producer, no
renamed-scanner violation, no Track-1≡Track-2 dishonesty; the StructRegistry
fence is intact. The four REVISEs are precision/coherence tightenings (cite
enumeration, MODIFY count, distribution invariant, leak-census receiver) — none
is a coupling defect; each has a named author, source path:line, and concrete
fix. Counts: 21 ACCEPT, 4 REVISE, 0 REJECT.
