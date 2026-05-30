---
lens: CH5-HIDDEN-COUPLING
pass: T-P3-synthesis
cycle: V3
subject: SK-V17 T-P3 synthesis artefacts (3A-3F + 3c-locks-v+1-diff.md)
reviewer: CH5 HIDDEN-COUPLING (V3)
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
generated_at: 2026-05-29T00:00:00Z
scan_focus: substrate-union (Lock 1) holds; tape is substrate-manifest CATEGORY not silent 6th BackendShape; 5-shape canon coherent across 3A/3B/3E; no parallel substrate/sidecar; StructRegistry/FieldSource fence intact; no Track-1≡Track-2 dishonesty; V2 REVISE fold verification
verdict: CONVERGENT-ALL-ACCEPT
counts:
  accept: 28
  revise: 0
  reject: 0
prior_cycle_fold_audit:
  CH5-V2-R01: FOLDED (distribution invariant now in 3c-locks-v+1-diff.md Invariant Check :103, verbatim from crystallisation doc)
  CH5-V2-R02: FOLDED (3E routes 7 strategy.rs sites to MP.SK18.W3 + admitted Lock-14 baseline + re-entry trigger; CH5-V1-R04 now in 3E V3 revised: block 3e:42)
---

# CH5 HIDDEN-COUPLING — SK-V17 T-P3 V3

## Scope of this lens (PASS-3 §3 CH5 + §8.1/§8.2)

CH5 scans for a proposed delta implying a **parallel substrate**, a **sidecar
producer**, a **renamed-scanner Lock-1 violation**, or a **Track 1 ≡ Track 2
dishonesty**; that the **substrate union holds** across every 3A surface delta;
that 3C's accepted amendments **introduce no coupling**; that the tape is a
substrate-manifest **CATEGORY** not a silent **6th BackendShape** (5-shape canon
coherent across 3A/3B/3E); and that the **StructRegistry/FieldSource fence** is
intact. V3 additionally audits whether the two V2 CH5 REVISEs folded — a cycle
that does not verify the prior fold is paper-hardening (PASS-3 §4).

## V2 REVISE fold audit (the load-bearing V3 obligation)

The V2 wave returned **CONVERGENT-WITH-REVISE** against 3A-3F (26 ACCEPT, 2
REVISE, 0 REJECT — `V2/CH5.md:10`-`14`). Both REVISEs were fold-completion gaps,
not coupling defects newly introduced. Both fold clean at V3.

| V2 finding | claimed fix target | verified state at master_head 2a76916ac | verdict |
|---|---|---|---|
| **CH5-V2-R01** R03 distribution invariant absent from the G3 gate object (`3c-locks-v+1-diff.md`) | copy the invariant into `3c-locks-v+1-diff.md` Invariant Check after the "No new directive …" bullet, verbatim from the crystallisation doc | **FOLDED.** `3c-locks-v+1-diff.md:103` now carries the full bullet: "**Distribution invariant**: if Pass Omega distributes the addendum clauses into Lock bodies, the Lock-10 tape-category clause MUST retain an inline cross-reference to the Lock-1 substrate manifest (`substrate_target = existing_tape`, `LOCKS.md:118`-`127`); a Lock-10 clause severed from the Lock-1 manifest anchor re-opens the silent-6th-shape reading." It sits in the Invariant Check (`:98`-`105`) directly after the "No new directive / BIR variant / substrate …" bullet (`:102`), exactly where the V2 fix named it. The fold ledger (`3c-locks-v+1-diff.md:55`) records it. Independently re-verified: `grep -in "distribution invariant" 3c-locks-v+1-diff.md` = `:103` present (was 0 at V2). The gate object now carries its own apply-time invariant; Pass Omega CRUD reading ONLY the gate object sees the constraint. | **ACCEPT** |
| **CH5-V2-R02** CH5-V1-R04 leak-census receiver unnamed; 7 `strategy.rs` sites routed under unowned "HEAD → 0" | name a SK-V18 receiving wave + monotonic-decrease tied to its close gate OR an admitted catalogued non-zero Lock-14 baseline with re-entry trigger; add CH5-V1-R04 to 3E's V3 `revised:` fold block | **FOLDED.** Three independent confirmations: (1) 3E's V3 `revised:` block now lists `CH5-V1-R04-3E17-D08-leak-census-receiver-unnamed` (`3e:42`) — no silent drop. (2) The P1 predicate row (`3e:186`) and the dedicated "P1 leak-census receiver (CH5-V2-R02 fold)" paragraph (`3e:206`-`221`) bind the decrease of the 7 `crates/ir/src/registry/strategy.rs` sites to **MP.SK18.W3** (the Lazy `ValueRef<G>` projection generator wave, `3b:147`) with the monotonic-decrease target tied to W3's close gate; fail-closed, the 7 sites are an admitted catalogued non-zero Lock-14 baseline (string-ident registry + doc-comment, NOT a runtime `match grammar {}` arm, `2c:279`-`281`) with re-entry trigger = next SK-V18 onboarding-wave leak-census gate. (3) 3E17-D08 itself (`3e:121`) carries the receiver-binding in the delta body; the CH4 matrix row (`3e:278`) and Open-Question CH5 row (`3e:291`) reinforce it. MP.SK18.W3 is a real 3B wave (`3b:147`), so the receiver is not phantom. The unowned "HEAD → 0" is eliminated; the coupling is bound to an owner. | **ACCEPT** |

Both V2 CH5 REVISEs fold clean. **Zero orphan REVISE carries forward** (§3W).

## Verification ledger (anchors re-confirmed at master_head 2a76916ac)

The load-bearing coupling claims are re-executed at HEAD, not trusted from prose.

| anchor | claim | result |
|---|---|---|
| `grep -rn compound_kind_for_layout crates/` | the UNIQUE live `StructRegistry`-method coupling is `crates/core/src/runtime/bbnf/arena.rs:47`; defn at `crates/ir/src/registry/struct.rs:388` | **HOLDS** — exactly two hits: `arena.rs:47` (sole caller) + `struct.rs:388` (defn). The unique-caller property is real; the D05/L01 cite is exact. |
| `grep -rln StructRegistry crates/core/src/runtime/{css_l4,json}/` | css_l4 + json builders carry ZERO `StructRegistry` coupling | **HOLDS** — empty. The fence-clean claim is verified; only the bbnf eager arena path couples. |
| `crates/core/src/runtime/tape/mod.rs:186` | `begin_compound` reads `layout.rule_id & 0x1F` only (grep-zero StructRegistry) | **HOLDS** — `let meta = (layout.rule_id & 0x1F) as u8;`; no registry call. The projection hot path is fence-clean by construction. |
| `grep -rl 'JsonStructBuilder\|CssStructBuilder' crates/` | eager-OpenFrame retirement blast radius = 40 files | **HOLDS** — 40. The D01/3B-W1 figure is single-sourced and accurate (V1 CH4-01 fold stable). |
| `skinny/crates/runtime/src/tape/mod.rs:94`-`100` | SoA `Tape<'input>` with `payloads: PayloadArena` as a struct MEMBER (line 99) | **HOLDS** — `payloads: PayloadArena` is a field of `Tape`, not a sidecar. The V2 CH5 open-question (PayloadArena implicit-retention hole) is answered: it is a declared member, `substrate_target=existing_tape` / `retention_lifetime=output_row` per 2F. |
| `skinny/crates/runtime/src/tape/mod.rs:175` | `ValueRef<'doc,'input,K,G:EventGrammar>` — grammar is a TYPE param | **HOLDS** — `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>`; monomorphised, zero runtime `match grammar {}` arm. |
| `restart/locks/LOCKS.md:108` | 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` | **HOLDS** verbatim; a 6th remains G-Omega gated (`:109`). |
| `git apply --check` on `3c-locks-v+1-diff.md` diff block vs `LOCKS.md` | the gate object applies cleanly | **HOLDS** — EXIT 0. Hunk `@@ -606,7 +606,22 @@` arithmetic-correct; the addendum inserts between the SK-V15 Lock-16 clause and `## v+1 Governance Boundary`. |

The five LACs and the v+1 diff carry **no new directive, no new BIR variant, no
new substrate, no public substrate API, no retained sidecar, no sixth shape**.
The substrate-union spine (Lock 1, `LOCKS.md:75`) is preserved on every surface
scanned. No V3 delta was newly added (`3a:14`, `3b:19`, `3c:14`, `3d:25`,
`3e:26`, `3f` carried `newly_added: []`) — the cycle folds V2 REVISEs only, so no
fresh coupling vector entered the packet.

## Disposition by artefact / delta

### 3A-architecture-synthesis.md (V3)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `ARCH-3A-S17-D01` tape-as-unified-substrate (eager-OpenFrame retire + AoS→SoA single encoding) | **ACCEPT** | Exactly-one-encoding obligation cited (`LOCKS.md:75`); the eager `OpenFrame`/AoS competitor is the DELETION target, not a sibling; eliminates the `Vec<OpenFrame>::clone` parallel-substrate pathology Lock 1 names. Dual AoS/SoA admissible ONLY as a transient fold-state (CH4 matrix `3a:112`). No parallel substrate. |
| `ARCH-3A-S17-D02` lazy `ValueRef<G>` value-plane | **ACCEPT** | One materialization plane; grammar via TYPE param (`tape/mod.rs:175`, re-verified), zero runtime branch; preserve-rich-ast held (lazy view, not flattened). No coupling. |
| `ARCH-3A-S17-D03` shared NEON classifier manifest | **ACCEPT** | `retention_lifetime=transient-single-call` (admissible per `LOCKS.md:137`-`149`); `substrate_target=existing_tape`; same-wave consumer = the tape. No cross-call retained classifier state; renamed-scanner Lock-1 violation structurally impossible. |
| `ARCH-3A-S17-D04` BackendShape-category disposition | **ACCEPT** | `3a:85` records the tape at the Lock-1 substrate manifest per LAC-1E-14, NOT a 6th shape; two independent grounds (categorical FactStream precedent + `admits_collapsed_stage` x86-binding mechanically refusing on aarch64). 5-shape domain verbatim. Coherent with 3B-D03 + 3E-D05 + 3C-L10. |
| `ARCH-3A-S17-D05` StructRegistry/FieldSource fence | **ACCEPT** | `3a:86` names the bbnf caller path precisely (`arena.rs:47`←`builder.rs:102`) and the unique-caller property; both re-verified at HEAD. css_l4/json grep-zero StructRegistry; the other 6 grammars use a local `match layout.rule_id` (`bnf/kind.rs:20`). The fence is concrete, not abstract; per-leaf runtime walk is REJECT (28-65×/983×/10583×). |
| `ARCH-3A-S17-D06/D07/D08` (StructLayout reconcile / selector wiring / 3-ORQ) | **ACCEPT** | `LayoutFacts.backend_shape` is a Lock-10 side-table field, grammar-author-blind, not a substrate; D07 WIRES the existing `crates/egraph`+`crates/csp-solver` decision engine, does not build a fresh one; D08-U2 names REDRESS-53 + the all-8 OnceCell `substrate_target` pre-gate. No new substrate. |

### 3B-master-plan-reconciliation.md (V3)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| B/F/G tranche rows — "eager `OpenFrame` is the fold-DELETION target … No parallel substrate" (`3b:94`-`96`) | **ACCEPT** | Deletion, not coexistence — substrate-union framing correct; the proven SoA tape IS B's "single parse substrate". |
| MP.SK18.W0 OnceCell `substrate_target` pre-gate + MP-3B-SKV17-D03 tape-category (`3b:144`,`:164`) | **ACCEPT** | All 8 carriers classified BEFORE wiring; the tape = Lock-1 substrate-manifest CATEGORY, "5-shape canon STAYS UNCHANGED across §13/§13.5/§13.1". Coherent with 3A-D04 + 3E-D05. |
| MP.SK18.W3 leak-census receiver binding (CH5-V2-R02) (`3b:147`) | **ACCEPT (R02 folded)** | MP.SK18.W3 is the real wave 3E binds the 7 `strategy.rs` sites' decrease to; its codegen subsumes the BackendShape strategy registry string-idents. The receiver is concrete, not phantom. |
| Refuted-Route table (D6 second substrate / dual AoS-SoA / fact-stream-String / x86) (`3b:124`-`130`) | **ACCEPT** | Every refuted route carried as a FENCE, not revived; F3 closes to EXACTLY ONE encoding; AoS→SoA coexistence transient-only. |
| MP.SK18.W5 NEON manifest + selector "5-shape canon PRESERVED" (`3b:149`) | **ACCEPT** | Coherent canon statement on the wave surface; the eq-set fan is the one real NEON Layer-1 body; no source-inventory admission. |

### 3C-locks-crystallisation.md + 3c-locks-v+1-diff.md (V3 gate object)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `D-SKV17-L01` tape-substrate-union clause (`3c-locks-v+1-diff.md:72`) | **ACCEPT** | Exactly ONE encoding survives; dual end-state REJECT; per-leaf `StructRegistry::layout(rule)` REJECT; OnceCell carriers resolve `existing_tape`/`local_temp_only`; the `arena.rs:47` cite precise + re-verified. No coupling. |
| `D-SKV17-L10` tape-category-not-sixth-shape (`3c-locks-v+1-diff.md:76`) | **ACCEPT** | Crystallised in the negative on LAC-1E-14 + `admits_collapsed_stage` x86-binding; 5-shape domain verbatim; 6th G-Omega gated; carries inline cross-ref to the Lock-1 manifest (`substrate_target = existing_tape`, `LOCKS.md:119`-`127`). The binding coherence clause. |
| `D-SKV17-L14` ValueRef/classifier-generalisation (config-breadth DISJOINT from value-plane proof) (`3c-locks-v+1-diff.md:78`) | **ACCEPT** | The explicit Track-1≡Track-2 anti-dishonesty fence: classifier config-breadth (8-of-9 grammars) is a SEPARATE axis, never laundered into the JSON+CSS-only value-plane proof. Scope-honest. |
| `D-SKV17-L16` NEON-classifier-manifest `transient-single-call` (`3c-locks-v+1-diff.md:80`) | **ACCEPT** | No cross-call retained classifier state; renamed-scanner Lock-1 violation structurally impossible; `byte_class_from_table_64`/`bitmap_prefix_xor_64` honestly declared `scalar-delegate-non-ASM`, not SIMD row-movers. No sidecar producer. |
| **Invariant Check distribution-invariant bullet** (`3c-locks-v+1-diff.md:103`) | **ACCEPT (R01 folded)** | The distribution invariant now rides the gate object itself, after the "No new directive …" bullet, verbatim from the crystallisation doc. Pass Omega CRUD reading only the gate object now sees the forward-distribution fence against a Lock-10 clause severed from the Lock-1 manifest anchor. The residual coupling CH5-V2-R01 named is closed. |
| Invariant Check (16-lock count / 5-shape canon / no new substrate) (`3c-locks-v+1-diff.md:100`-`105`) | **ACCEPT** | 16 numbered locks unchanged; 5-shape verbatim; tape recorded as `substrate_target = existing_tape`, not a new substrate; NEON `transient-single-call`; OnceCell carriers `existing_tape`/`local_temp_only`. Re-verified: `git apply --check` EXIT 0. |

### 3D-skinny-fold.md (V3)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `3D-SK17-D01` SoA `Tape` V1-authoritative; dual end-state transient-only (`3d:120`) | **ACCEPT** | Affirms SoA-adopt; "parity-keep would re-open the dual-substrate Lock-1 risk"; the Cost-And-Non-Fit row (`3d:146`) — "If the SoA convergence does not fit one wave, parity-prove transiently then converge — never ship a parallel substrate" — is the explicit CH5 firewall. Monotonic; no parallel substrate. |
| `3D-SK17-D05` registry-fence locks-strengthening (`3d:124`) | **ACCEPT** | `arena.rs:47` coupling inside the eager arena path, severed by the eager-OpenFrame retirement (not relocated); `begin_compound` grep-zero StructRegistry re-verified. The CH5 Open-Question row (`3d:159`) explicitly asks how SK-V18 proves the coupling is SEVERED not relocated — a fence, not a defer. |
| `3D-SK17-D07` Sheets/BBNF generality gap to 3E (`3d:126`) | **ACCEPT** | By-construction-not-by-exercise scoping preserved; no fleet-wide over-claim; routes the non-JSON story to 3E. Track-1≡Track-2 honesty held: scan generality is breadth-of-CONFIG, not breadth-of-PROOF. |
| `3D-SK17-D08` monotonic-direction governance clause (`3d:127`) | **ACCEPT** | SK-V18 adopts skinny `Tape`/`ValueRef` INTO crates/core; never relocates `TapeStructBuilder`/`StructLayout`/`TapeCursor` into skinny. Direction-inversion is a CH3/CH5 REJECT. No coupling inversion. |

### 3E-grammar-generalisation.md (V3)

| delta / claim | CH5 disposition | basis |
|---|---|---|
| `3E17-D02` tape-substrate grammar-column-free fence (`3e:115`) | **ACCEPT** | Pre-blocks the dense per-grammar class column (AV.04/REDRESS-96 barred shape); flags are sparse position-keyed side-vectors (`offsets`/`flag_cursors`/`flag_values`/`PayloadArena`, re-verified `tape/mod.rs:94`-`100`). No grammar column. |
| `3E17-D05` per-grammar BackendShape matrix = substrate category (`3e:118`) | **ACCEPT** | 5-shape verbatim; CollapsedStage aarch64-refused; "any proposal reading as a 6th `BackendShape` is REJECT". Coherent with 3A-D04, 3B-D03, 3C-L10. The matrix cells tag non-witnessed rows `predicted (cost-model-pending)` — honest, no proof inflation. |
| `3E17-D08` onboarding tape predicates + P1 leak-census receiver (CH5-V2-R02 fold) (`3e:121`,`:206`) | **ACCEPT (R02 folded)** | The 7 `strategy.rs` sites' decrease is OWNED by MP.SK18.W3 with the target tied to W3's close gate; fail-closed admitted Lock-14 baseline + re-entry trigger; the sites are doc-comment + string-ident registry entries, NOT `match grammar {}` arms (coupling-honest, not coupling-active). The unowned "HEAD → 0" is gone; the coupling is bound to an owner. P6 value-axis firewall distinct from the P1 classifier/leak axis. |
| `3E17-D07` by-construction-not-by-exercise scoping (`3e:120`) | **ACCEPT** | The honest-scope fence: value-plane proof JSON+CSS by-exercise, Sheets/BBNF-self by-construction — never fleet-wide. The Track-1≡Track-2 dishonesty CH5 guards is structurally barred. |
| `3E17-D09` FieldSource compile-time projection fence (`3e:122`) | **ACCEPT** | Couples the perf fence to the grammar-neutrality fence: the runtime `StructRegistry::layout` walk is BOTH a regression AND a grammar-shaped runtime dispatch; `begin_compound` grep-zero StructRegistry re-verified. |
| Per-Grammar matrix + Primitive Vocabulary Transfer (`JsonSink` not generic, `ARCHITECTURE.md:1269`) (`3e:172`) | **ACCEPT** | No JSON-narrowing; sink contract per-grammar generated. Substrate-union preserved across all grammar columns. |

### 3F-migration-handoff.md (V3)

| artefact / delta | CH5 disposition | basis |
|---|---|---|
| 3F17-MH-03 single-encoding closure gate (`3f:69`) | **ACCEPT** | Encodes Lock-1 "parallel substrates are dead" as a mechanical migration gate: after the fold EXACTLY ONE encoding survives (`grep` proves AoS `TapeRec` retired XOR SoA `Tape` retired, never both live); dual state transient-only. |
| 3F17-MH-06 no-second-substrate fence (`3f:72`) | **ACCEPT (strong)** | Explicitly encodes the CH5 hidden-coupling firewall: "the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`; an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` ALONGSIDE the proven `Tape`/`ValueRef` is a Lock-1 type-ambivalence violation (REJECT)"; NEON classifier `substrate_target = existing_tape` / `transient-single-call`; "no sidecar mask producer, no parallel source pass, no sixth BackendShape". The textbook CH5 fence. |

## Cross-surface 5-shape coherence verdict (§8.2)

The 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
is stated verbatim and the tape-as-substrate-category (not 6th shape) is
**coherent across all required surfaces** in V3:

- 3A-D04 (`3a:85`, `ARCHITECTURE.md` §7.3/§9): category per LAC-1E-14 + `admits_collapsed_stage` x86-binding.
- 3B-D03 (`3b:164`, `MASTER-PLAN.md` §13/§13.1/§13.2): category, "5-shape canon STAYS UNCHANGED", §13.2 MP.NW6 self-consistent (`MASTER-PLAN.md:640`).
- 3E-D05 (`3e:118`, matrix): per-grammar shapes unchanged, substrate relationship explicit, 6th-shape REJECT.
- 3C-L10 (`3c-locks-v+1-diff.md:76`, Lock 10): the binding coherence clause, two independent grounds, inline Lock-1 cross-ref; the distribution invariant (`:103`) now fences the forward-distribution silent-6th-shape reading.

No surface touches the canon without the others; no §8.2 coherence REJECT. The
only "6th shape" strings across 3A/3B/3E are REJECT/category-not-6th framing or
CH5 open-question phrasing ("so no MASTER cross-reference reads as a 6th shape");
no surface introduces a sixth shape. 16-lock count preserved
(`3c-locks-v+1-diff.md:100`).

## Substrate-union + sidecar verdict (Lock 1)

- **No parallel substrate**: every fold design DELETES the eager `OpenFrame`/AoS competitor; exactly-one-encoding obligation enforced (3F17-MH-03 mechanical gate, `3f:69`); OnceCell carriers gated to `existing_tape`/`local_temp_only` before wiring (all 8, `3b:144`).
- **No sidecar producer**: `PayloadArena` is a tape struct MEMBER (`tape/mod.rs:99`, re-verified); the NEON classifier is `transient-single-call` (the strictest Lock-1 v+1 ELEVATION clause); 3E17-D02 keeps the surviving encoding grammar-column-free.
- **No renamed-scanner Lock-1 violation**: the classifier carries no retained mask/class/prefix stream across calls; 3C-L16 declares it `transient-single-call`.
- **No Track-1≡Track-2 dishonesty**: 3C-L14 + 3E-D07 + the P6 value-axis firewall keep the classifier config-breadth axis DISJOINT from the JSON+CSS-only value-plane proof; no fleet-wide over-claim; Sheets/BBNF-self cells tagged `predicted`.
- **StructRegistry/FieldSource fence intact**: tape `begin_compound` grep-zero StructRegistry (`tape/mod.rs:186`, re-verified); the one live runtime coupling (`bbnf/arena.rs:47`, sole `compound_kind_for_layout` caller, re-verified) is in the eager path, severed by retirement; any per-leaf runtime walk is REJECT (28-65×/983×/10583×).
- **Gate-object apply-time fences ride the gate object**: both the distribution invariant (`3c-locks-v+1-diff.md:103`) and the no-new-substrate/no-sidecar/no-6th-shape invariants (`:102`) travel with the artefact Pass Omega CRUD reads.

## CH5 verdict

**CONVERGENT-ALL-ACCEPT.** The substrate-union holds; the tape is consistently a
substrate-manifest CATEGORY, never a silent 6th shape; the 5-shape canon is
coherent across 3A/3B/3E (and 3C/3F); no parallel substrate, no sidecar producer,
no renamed-scanner violation, no Track-1≡Track-2 dishonesty; the
StructRegistry/FieldSource fence is intact and re-verified at HEAD. Both V2 CH5
REVISEs fold clean: R01's distribution invariant now rides the G3 gate object's
Invariant Check (`3c-locks-v+1-diff.md:103`), and R02's leak-census receiver is
bound to MP.SK18.W3 with an admitted Lock-14 baseline + re-entry trigger (3E),
with CH5-V1-R04 recorded in 3E's V3 `revised:` block — no silent drop. Zero
orphan unresolved REVISE carries forward (§3W). No fresh coupling vector entered
the V3 packet (`newly_added: []` on every surface). All load-bearing greps
re-executed at master_head 2a76916ac confirm the prose. Counts: **28 ACCEPT,
0 REVISE, 0 REJECT.**
