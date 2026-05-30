---
lens: CH5-HIDDEN-COUPLING
pass: T-P3-synthesis
cycle: V2
subject: SK-V17 T-P3 synthesis artefacts (3A-3F + 3c-locks-v+1-diff.md)
reviewer: CH5 HIDDEN-COUPLING (V2)
master_head: 2a76916ac
generated_at: 2026-05-29T00:00:00Z
scan_focus: substrate-union (Lock 1) holds; tape is substrate-manifest CATEGORY not silent 6th BackendShape; 5-shape canon coherent across 3A/3B/3E; no parallel substrate/sidecar; StructRegistry/FieldSource fence intact; no Track-1≡Track-2 dishonesty; V1 REVISE fold verification
verdict: CONVERGENT-WITH-REVISE
counts:
  accept: 26
  revise: 2
  reject: 0
prior_cycle_fold_audit:
  CH5-V1-R01: FOLDED (3A-D05 + 3C-L01 — arena.rs:47 caller-path + unique-caller property)
  CH5-V1-R02: FOLDED (3c-locks-crystallisation:55 + 3c-locks-v+1-diff:39 — tally reconciled to 9A/3ORQ/2M)
  CH5-V1-R03: PARTIAL (landed in 3c-locks-crystallisation:197; ABSENT from 3c-locks-v+1-diff Invariant Check — the G3 gate object) → CH5-V2-R01
  CH5-V1-R04: NOT-FOLDED (absent from 3E V2 revised-block; P1 predicate still unowned "HEAD → 0") → CH5-V2-R02
---

# CH5 HIDDEN-COUPLING — SK-V17 T-P3 V2

## Scope of this lens (PASS-3 §3 CH5 + §8.1/§8.2)

CH5 scans for: a proposed delta implying a **parallel substrate**, a **sidecar
producer**, a **renamed-scanner Lock-1 violation**, or a **Track 1 ≡ Track 2
dishonesty**; the **substrate union holds** across every 3A surface delta; 3C's
accepted amendments **introduce no coupling**; the tape is a substrate-manifest
**CATEGORY** not a silent **6th BackendShape** (5-shape canon coherent across
3A/3B/3E); the **StructRegistry/FieldSource fence** is intact. V2 additionally
audits whether the four V1 CH5 REVISEs folded.

## V1 REVISE fold audit (the load-bearing V2 obligation)

A V2 hardening cycle that does not verify the prior fold is paper-hardening
(PASS-3 §4). The four V1 CH5 REVISEs resolve as follows.

| V1 finding | claimed fix target | verified state at master_head | verdict |
|---|---|---|---|
| **CH5-V1-R01** arena.rs:47 cite under-specified | name bbnf caller path + scope precisely | **FOLDED.** `3a-architecture-synthesis.md:66` (D05 row) now reads "the SOLE live-runtime `StructRegistry`-method coupling is `crates/core/src/runtime/bbnf/arena.rs:47` … reached only via the eager bbnf builder `crates/core/src/runtime/bbnf/builder.rs:102` …; css_l4 + json builders carry ZERO `StructRegistry` coupling … the other 6 grammars resolve compound kind from a local `match layout.rule_id` (`bnf/kind.rs:20`)". The unique-caller property is stated (`grep -rn compound_kind_for_layout crates/` = defn `struct.rs:388` + one caller). `3c-locks-crystallisation.md:94` (L01) carries the same precise enumeration. Independently re-verified: `grep -rn compound_kind_for_layout crates/` = exactly `struct.rs:388` (defn) + `bbnf/arena.rs:47` (sole caller). | **ACCEPT** |
| **CH5-V1-R02** MODIFY-count incoherence | reconcile to 9A/3ORQ/2M | **FOLDED.** `3c-locks-v+1-diff.md:39` = "(9 ACCEPT, 3 ORQ-ACCEPT, 2 MODIFY, 0 REJECT, 0 DEFER)"; `3c-locks-crystallisation.md:55` = "9 ACCEPT, 3 ACCEPT (ORQ-crystallised), 2 MODIFY, 0 REJECT, 0 DEFER"; tally `:146` = "MODIFY 2 — LAC-2F-FOLD-05, LAC-1E-SKV17-04". The "5 MODIFY" contradiction is gone; both surfaces now agree. | **ACCEPT** |
| **CH5-V1-R03** distribution invariant missing | add to **`3c-locks-v+1-diff.md`** Invariant Check section | **PARTIAL.** The invariant text landed in `3c-locks-crystallisation.md:197`-`200` ("**Distribution invariant**: under ANY distribution, the Lock-10 tape-category clause MUST retain an inline cross-reference to the Lock-1 substrate manifest … Severing that cross-ref re-opens a silent-6th-shape reading"). BUT `3c-locks-v+1-diff.md` — the **G3 gate object Pass Omega reads and applies** — carries it nowhere (`grep -c "distribution invariant" 3c-locks-v+1-diff.md` = 0; its Invariant Check `:84`-`91` lacks the line). The V1 fix explicitly named the v+1-diff Invariant Check as the target. | **REVISE → CH5-V2-R01** |
| **CH5-V1-R04** leak-census receiver unnamed | route 7 `strategy.rs` sites to a named SK-V18 wave | **NOT FOLDED.** 3E's V2 `prior_cycle_dispositions_folded.revised` block (`:32`-`38`) folds CH2-V1-S9/S10/S11, CH4-V1-03, CH6-V1-08 — but **does not list CH5-V1-R04**. The P1 predicate (`:169`) still reads "monotonic-decrease-to-zero (HEAD → 0)" with no named SK-V18 receiving wave; 3E17-D08 (`:104`) still says "monotonic-decrease (HEAD baseline non-zero, catalogued)" without an owning wave; 3B carries no wave owning the `strategy.rs` decrease (`grep -i "strategy.rs\|grammar-name leak" 3b-master-plan-reconciliation.md` = empty). | **REVISE → CH5-V2-R02** |

Two of four V1 REVISEs folded clean; R03 partially folded (wrong artefact);
R04 not folded. Per ORQ §3W "zero orphan REVISE", both unresolved items carry
forward as V2 REVISEs with a tightened, single-artefact fix.

## Verification ledger (anchors re-confirmed at master_head 2a76916ac)

| anchor | claim | result |
|---|---|---|
| `restart/locks/LOCKS.md:75` | Lock 1: "parallel substrates are dead"; "A SIMD mask stream is a transient producer, not a retained sidecar; if structural offsets are retained, the structural projection IS the tape" | HOLDS — the no-parallel-substrate + no-sidecar spine the fold rides |
| `restart/locks/LOCKS.md:107`-`109` | 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; 6th G-Omega gated | HOLDS verbatim |
| `restart/locks/LOCKS.md:118`-`127` | LAC-1E-14 substrate_target vocab includes `existing_tape`; "Any retained class/mask stream … `UnionTape`, or second tape is rejected unless G-Omega" | HOLDS — `existing_tape` admitted; second-substrate barred |
| `skinny/crates/runtime/src/tape/mod.rs:94`-`100` | SoA `Tape<'input>` with `payloads: PayloadArena` as MEMBER (line 99) | HOLDS — PayloadArena is a tape member, not a sidecar |
| `skinny/crates/runtime/src/tape/mod.rs:175` | `ValueRef<'doc,'input,K,G:EventGrammar>` | HOLDS — grammar is a TYPE param, zero runtime arm |
| `crates/core/src/runtime/bbnf/arena.rs:47` + `crates/ir/src/registry/struct.rs:388` | sole live `compound_kind_for_layout` caller + defn | HOLDS — unique-caller property real; R01 cite accurate |

The five LACs and the v+1 diff carry **no new directive, no new BIR variant, no
new substrate, no public substrate API, no retained sidecar, no sixth shape**.
The substrate-union spine (Lock 1) is preserved on every surface scanned. No V2
delta was newly added (`3a:14` + `3b:19` `newly_added: []`) — the cycle folds
V1 REVISEs only, so no fresh coupling vector entered the packet.

## Disposition by artefact / delta

### 3A-architecture-synthesis.md (V2)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `ARCH-3A-S17-D01` tape-as-unified-substrate (eager-OpenFrame retire + AoS→SoA single encoding) | **ACCEPT** | Exactly-one-encoding obligation cited (`LOCKS.md:75`); eliminates the `Vec<OpenFrame>::clone` parallel-substrate pathology Lock 1 names. No parallel substrate. (V2 blast-radius re-priced to 40 files per CH4 fold; not a CH5 axis.) |
| `ARCH-3A-S17-D02` lazy `ValueRef<G>` value-plane | **ACCEPT** | One materialization plane; grammar via TYPE param (`tape/mod.rs:175`), zero runtime branch; preserve-rich-ast held. No coupling. |
| `ARCH-3A-S17-D03` shared NEON classifier manifest | **ACCEPT** | `retention_lifetime=transient-single-call` (admissible per `LOCKS.md:137`-`149`); `substrate_target=existing_tape`; same-wave consumer = tape. No cross-call retained classifier state. |
| `ARCH-3A-S17-D04` BackendShape-category disposition | **ACCEPT** | `3a:65` records the tape at the substrate manifest per LAC-1E-14, NOT a 6th shape; two independent grounds (categorical precedent + `admits_collapsed_stage` x86-binding mechanically refusing on aarch64). 5-shape domain verbatim. Coherent with 3B-D03 + 3E-D05. |
| `ARCH-3A-S17-D05` StructRegistry/FieldSource fence | **ACCEPT** (R01 folded) | `3a:66` now names the bbnf caller path precisely and the unique-caller property. The fence holds; the cite is exact. css_l4/json builders grep-zero StructRegistry; other 6 grammars use local `match layout.rule_id`. R01 discharged. |
| `ARCH-3A-S17-D06/D07/D08` (StructLayout reconcile / selector wiring / 3-ORQ) | **ACCEPT** | Side-table `LayoutFacts.backend_shape` is a Lock-10 side-table field, not a substrate; selector WIRES existing `crates/egraph`+`crates/csp-solver`; U2 names REDRESS-53 + OnceCell `substrate_target` pre-gate. No new substrate. |

### 3B-master-plan-reconciliation.md (V2)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| B-tranche row "eager `OpenFrame` is the fold-DELETION target … No parallel substrate" | **ACCEPT** | Deletion, not coexistence — substrate-union framing correct. |
| H.W1 OnceCell `substrate_target` pre-gate / MP-3B-SKV17-D03 tape-category | **ACCEPT** | `3b:159` — tape = Lock-1 substrate-manifest CATEGORY, "5-shape canon STAYS UNCHANGED across §13/§13.5/§13.1". §13.2 MP.NW6 self-consistent (`MASTER-PLAN.md:640`). Coherent with 3A-D04 + 3E-D05. |
| Refuted-Route table (D6 second substrate / dual AoS-SoA / fact-stream-String / x86) | **ACCEPT** | Every refuted route carried as a FENCE, not revived. |
| MP.SK18.W5 NEON manifest + selector "5-shape canon PRESERVED" | **ACCEPT** | Coherent canon statement on the wave surface. |
| CH5 Open Question row (3C v+1 text classifies the tape so no MASTER cross-ref reads as 6th shape) | **ACCEPT** | Cross-surface coherence routed to 3C; substrate-union holds. |

### 3C-locks-crystallisation.md + 3c-locks-v+1-diff.md (V1 artefacts, V2 packet)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `D-SKV17-L01` tape-substrate-union clause | **ACCEPT** | `3c-locks-v+1-diff.md:58` — exactly ONE encoding survives; dual end-state REJECT; per-leaf `StructRegistry::layout(rule)` REJECT; OnceCell carriers resolve `existing_tape`/`local_temp_only`; the `arena.rs:47` cite now precise (R01). No coupling. |
| `D-SKV17-L10` tape-category-not-sixth-shape | **ACCEPT** | `3c-locks-v+1-diff.md:62` — crystallised in the negative on LAC-1E-14 + `admits_collapsed_stage` x86-binding; 5-shape domain verbatim; 6th G-Omega gated; carries inline cross-ref to Lock-1 manifest (`substrate_target = existing_tape`, `LOCKS.md:119`-`127`). The binding coherence clause. |
| `D-SKV17-L14` ValueRef/classifier-generalisation (config-breadth axis DISJOINT from value-plane proof) | **ACCEPT** | The explicit Track-1≡Track-2 anti-dishonesty fence: classifier config-breadth never laundered into the JSON+CSS-only value-plane proof. |
| `D-SKV17-L16` NEON-classifier-manifest `transient-single-call` | **ACCEPT** | `3c-locks-v+1-diff.md:66` — no cross-call retained classifier state; renamed-scanner Lock-1 violation structurally impossible; honest scalar-delegate-non-ASM passthroughs declared. No sidecar producer. |
| Disposition tally + MODIFY count | **ACCEPT** (R02 folded) | Tally reconciled to 9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY across both files. The frontmatter-coherence gap is closed. |
| v+1-diff Invariant Check section (`:84`-`91`) | **REVISE → CH5-V2-R01** | The R03 distribution invariant is present in the crystallisation prose (`:197`) but ABSENT from the gate object's Invariant Check. The gate object travels to Pass Omega; the distribution constraint must ride it. |
| Refutation rows (6th shape / per-leaf StructRegistry / AoS-SoA dual / fleet-wide value-plane / x86-SVE REFUTED) | **ACCEPT** | Every load-bearing refutation preserved as REJECT-class; no revival; substrate-union + 5-shape spine fully fenced. |

### 3E-grammar-generalisation.md (V2)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `3E17-D02` tape-substrate grammar-column-free fence | **ACCEPT** | Pre-blocks the dense per-grammar class column (AV.04/REDRESS-96 barred shape). Verified `tape/mod.rs:94`-`100` is sparse position-keyed (offsets/flag_cursors/flag_values/payloads). No grammar column. |
| `3E17-D05` per-grammar BackendShape matrix = substrate category | **ACCEPT** | 5-shape verbatim; CollapsedStage aarch64-refused; "any proposal reading as a 6th BackendShape is REJECT". Coherent with 3A-D04, 3B-D03, 3C-L10. (V2 fold tagged predicted-cells, P5 split, P6 firewall — none re-opens coupling.) |
| `3E17-D06` CSS eq-set-fan non-JSON consumer binding | **ACCEPT** | Lock 16 "≥1 non-JSON consumer" binds to the eq-set fan NEON body, not the scalar-delegate table passthrough. No JSON-narrowing. |
| `3E17-D07` by-construction-not-by-exercise scoping | **ACCEPT** | The honest-scope fence: value-plane proof scoped JSON+CSS by-exercise, Sheets/BBNF-self by-construction — never fleet-wide. The Track-1≡Track-2 dishonesty CH5 guards. P6 value-axis firewall (`:175`) reinforces it. |
| `3E17-D09` FieldSource compile-time projection fence | **ACCEPT** | Couples perf fence to grammar-neutrality fence: the runtime `StructRegistry::layout` walk is BOTH a regression AND a grammar-shaped runtime dispatch. Verified `begin_compound` grep-zero StructRegistry (`tape/mod.rs:185`-`186`). |
| P1 leak-census predicate (`:169`) + 3E17-D08 (`:104`) | **REVISE → CH5-V2-R02** | Honest 7-site catalogue (laudable, no paper-clean claim) but still routed under unowned "monotonic-decrease-to-zero (HEAD → 0)" — no named SK-V18 receiving wave; CH5-V1-R04 was not folded. |
| Per-Grammar matrix + Primitive Vocabulary Transfer (`JsonSink` not generic `ARCHITECTURE.md:1269`) | **ACCEPT** | No JSON-narrowing; sink contract per-grammar generated. Substrate-union preserved across all grammar columns. |

### 3D / 3F (CH5 surface scan, V2)

| artefact | CH5 disposition | basis |
|---|---|---|
| 3D-skinny-fold | **ACCEPT** | `3d:132` "If the SoA convergence does not fit one wave, parity-prove transiently then converge — never ship a parallel substrate"; `3d:142` REDRESS-53 fence on retained index. Monotonic; no parallel-substrate fold. |
| 3F-migration-handoff (3F17-MH-03/06) | **ACCEPT** (strong) | `3f:68` 3F17-MH-06 explicitly encodes the CH5 hidden-coupling firewall: "the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`; an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` ALONGSIDE the proven `Tape`/`ValueRef` is a Lock-1 type-ambivalence violation (REJECT)"; `substrate_target = existing_tape`/`transient-single-call`; "no sidecar mask producer, no parallel source pass, no sixth BackendShape". `3f:65` 3F17-MH-03 single-encoding closure gate. `3f:204` CH5 open-question row routes the 5-shape coherence concern to 3A/3E + Pass Omega CRUD. No CH5 violation. |

## REVISE findings (2)

### CH5-V2-R01 — R03 distribution invariant is in the crystallisation prose but ABSENT from the G3 gate object (3c-locks-v+1-diff.md)

CH5-V1-R03's concrete fix named **`3c-locks-v+1-diff.md`** (the Invariant Check
section) as the target, because the v+1-diff IS the G3 gate object that Pass
Omega's CRUD wave reads and applies (PASS-3 §5: "3C-locks-v+1-diff.md ← line-level
LOCKS diff (G3 gate object)"; §8.1). The fix landed instead only in the
crystallisation companion doc: `3c-locks-crystallisation.md:197`-`200` carries
the full "**Distribution invariant**: under ANY distribution, the Lock-10
tape-category clause MUST retain an inline cross-reference to the Lock-1 substrate
manifest (`substrate_target = existing_tape`, `LOCKS.md:118`-`127`). Severing that
cross-ref re-opens a silent-6th-shape reading." But `3c-locks-v+1-diff.md`'s
Invariant Check (`:84`-`91`) has **no** such line (`grep -c "distribution
invariant" 3c-locks-v+1-diff.md` = 0).

This is a residual coupling, not a paper one: if Pass Omega CRUD distributes the
addendum clauses into Lock bodies reading ONLY the gate object (the artefact §5
designates for it), it never sees the distribution constraint, and a Lock-10
tape-category clause severed from the Lock-1 manifest anchor re-opens the
silent-6th-shape reading — the exact coupling CH5 guards. The crystallisation
doc is the disposition-matrix surface; the v+1-diff is the apply surface; the
constraint must ride the apply surface.

**Concrete fix:** add to `3c-locks-v+1-diff.md` Invariant Check (after the "No
new directive / BIR variant / substrate …" bullet at `:88`): "**Distribution
invariant**: if Pass Omega distributes the addendum clauses into Lock bodies, the
Lock-10 tape-category clause MUST retain an inline cross-reference to the Lock-1
substrate manifest (`substrate_target = existing_tape`, `restart/locks/LOCKS.md:118`-`127`);
a Lock-10 clause severed from the Lock-1 manifest anchor re-opens the
silent-6th-shape reading." Identical text to the crystallisation doc; the fix is a
copy into the gate object. The present-state guard (the inline cross-ref the
Lock-10 clause already carries at `3c-locks-v+1-diff.md:62`) is intact, so this is
a forward-distribution fence, not a present defect — but the gate object must
carry its own apply-time invariant.

### CH5-V2-R02 — CH5-V1-R04 was not folded: the grammar-name leak-census still lacks a named SK-V18 receiving wave (3E17-D08, P1 predicate)

CH5-V1-R04 asked 3E to route the 7 `crates/ir/src/registry/strategy.rs`
grammar-name leak sites (`:132,:137,:149,:197-198,:292,:315`) to a named SK-V18
wave (e.g. the `ValueRef<G>` generator wave MP.SK18.W3 or the Lock-14 leak-census
gate), with the monotonic-decrease target tied to that wave's close gate — not an
unowned "HEAD → 0". 3E's V2 `prior_cycle_dispositions_folded.revised` block
(`3e:32`-`38`) folds CH2-V1-S9/S10/S11, CH4-V1-03, CH6-V1-08 — but **omits
CH5-V1-R04 entirely**. The P1 predicate (`3e:169`) still reads
"monotonic-decrease-to-zero (HEAD → 0); NOT a clean pass"; 3E17-D08 (`3e:104`)
still reads "monotonic-decrease (HEAD baseline non-zero, catalogued)"; and no 3B
wave owns the decrease (`grep -i "strategy.rs\|grammar-name leak"
3b-master-plan-reconciliation.md` = empty). This is an **orphan unresolved
REVISE** — a §3W "zero orphan REVISE" gate violation, and a soft engineered-defer
in CH5's coupling-firewall reading: the string-ident grammar-name coupling is
catalogued (good) but no fold wave OWNS reducing it.

**Concrete fix (single-artefact, choose one):**
(a) **Name the receiver** — in `3e:104` (3E17-D08) and `3e:169` (P1 predicate),
bind the 7 `strategy.rs` sites to the SK-V18 wave that owns the `ValueRef<G>`
generator (3B's value-plane wave — `3e:237` already names "SK-V18 W2" as the
ARCH-3A-S17-D02 inheritor) OR the Lock-14 leak-census gate, with the
monotonic-decrease target tied to that wave's close gate; OR
(b) **State the scoping rule** — if no SK-V18 wave reduces the 7 sites in-cycle,
declare them an admitted catalogued non-zero baseline under Lock 14 (they are
doc-comment + string-ident registry entries, NOT runtime `match grammar {}` arms,
per `2c:279`-`281`), with a named future receiver and a re-entry trigger.

Either binds the coupling to an owner. The honest catalogue is preserved; only the
unowned "HEAD → 0" target needs an owner or an explicit admitted-baseline rule.
Add CH5-V1-R04 to 3E's V3 `revised:` fold block so the disposition is not silently
dropped (a silent drop is itself a CH1+CH6 REJECT, §8.1).

## Cross-surface 5-shape coherence verdict (§8.2)

The 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
is stated verbatim and the tape-as-substrate-category (not 6th shape) is
**coherent across all required surfaces** in V2:
- 3A-D04 (`3a:65`, `ARCHITECTURE.md` §7.3/§9): category per LAC-1E-14 + `admits_collapsed_stage` x86-binding.
- 3B-D03 (`3b:159`, `MASTER-PLAN.md` §13/§13.1/§13.2): category, "5-shape canon STAYS UNCHANGED", §13.2 MP.NW6 self-consistent.
- 3E-D05 (`3e`, matrix): per-grammar shapes unchanged, substrate relationship explicit, 6th-shape REJECT.
- 3C-L10 (`3c-locks-v+1-diff.md:62`, Lock 10): the binding coherence clause, two independent grounds, inline Lock-1 cross-ref.

No surface touches the canon without the others; no §8.2 coherence REJECT. The
tape is never a silent 6th shape on any surface. 16-lock count preserved
(`3c-locks-v+1-diff.md:86`).

## Substrate-union + sidecar verdict (Lock 1)

- **No parallel substrate**: every fold design DELETES the eager `OpenFrame`/AoS competitor; exactly-one-encoding obligation enforced (`3f:65` mechanical gate); OnceCell carriers gated to `existing_tape`/`local_temp_only`.
- **No sidecar producer**: `PayloadArena` is a tape MEMBER (`tape/mod.rs:99`); the NEON classifier is `transient-single-call` (the strictest Lock-1 v+1 ELEVATION clause).
- **No renamed-scanner Lock-1 violation**: the classifier carries no retained mask/class/prefix stream across calls.
- **No Track-1≡Track-2 dishonesty**: 3C-L14 + 3E-D07 + P6 value-axis firewall keep the classifier config-breadth axis DISJOINT from the JSON+CSS-only value-plane proof; no fleet-wide over-claim.
- **StructRegistry/FieldSource fence intact**: tape `begin_compound` grep-zero StructRegistry; the one live runtime coupling (`bbnf/arena.rs:47`) is in the eager path, severed by retirement, cite now precise (R01); any per-leaf runtime walk is REJECT (28-65×/983×/10583×).

## CH5 verdict

**CONVERGENT-WITH-REVISE.** The substrate-union holds; the tape is consistently a
substrate-manifest CATEGORY, never a silent 6th shape; the 5-shape canon is
coherent across 3A/3B/3E; no parallel substrate, no sidecar producer, no
renamed-scanner violation, no Track-1≡Track-2 dishonesty; the StructRegistry fence
is intact. Two of the four V1 REVISEs folded clean (R01 cite precision, R02 MODIFY
count); the other two carry forward — R03's distribution invariant landed in the
crystallisation prose but not in the G3 gate object it was directed at, and R04's
leak-census receiver was not folded at all (orphan REVISE, §3W violation). Neither
carried REVISE is a coupling defect introduced in V2; both are fold-completion
gaps — but the §3Z/§3W convergence criterion ("zero orphan unresolved REVISE")
bars an all-ACCEPT close until both fold. Each has a named author (3C / 3E), a
source artefact:line, and a single-artefact concrete fix. Counts: 26 ACCEPT,
2 REVISE, 0 REJECT.
