---
pass: T-P2-research (SK-V17 totality greater-spec fold)
artefact: HARDENING-CONSOLIDATED
cycle: V3
aggregator: T-P2-CHALLENGE-aggregator
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_sha: 91b6893b0
t_p1_excavation_sha: 445925167154de73540e3ea3283d0170371de790
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 (CH1–CH6) + §4 + §8; ORCHESTRATOR §3W/§3Z
dossiers_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
lenses_run: [CH1, CH2, CH3, CH4, CH5, CH6, CH7]
cycle_accept_rate_pct: 100.0
cycle_history_accept_pct: [V1=89.1, V2=98.6, V3=100.0]
consecutive_ge95_cycles: 2          # V2 (98.6%) + V3 (100.0%)
orphan_revise: 0
v_ceiling: 5
v_current: 3
verdict: CONVERGED
ready_for: T-P3
g2_gate: open (auto-close eligible — §3Z criteria met)
---

# HARDENING-T-P2-SKV17-V3-CONSOLIDATED — Greater-Spec Fold, Converged

This is the V3 CHALLENGE consolidation for T-P2, the **totality-track greater-spec
fold** pass. T-P2 does not ground the JSON engine (that was the skinny S-P2); it
designs how the SKINNY-PROVEN unified-tape / lazy-view / `StructLayout`-projection
+ aarch64-NEON model **folds into the V1 totality spec** — the SK-V18
implementation the spec must direct. The six T-P1 divergence sections (A tape
AoS↔SoA, B eager OpenFrame, C value-API per-grammar-EAGER, D BackendShape 5-shape
canon must absorb tape-as-substrate, E NEON JSON-only vs shared classifier,
F StructRegistry/FieldSource fence — T-P1 CONVERGED, commit `91b6893b0`) are the
fold surface. Every fold proposal in this consolidation is **grammar-neutral
(Lock 14)**, **substrate-union-preserving (Lock 1)**, **5-shape-canon-coherent
(Lock 10)**, **scalar-ref + checkasm (Lock 16, aarch64-only NEON)**, file:line/SHA
grounded, with the §2 candidate enumeration load-bearing.

---

## §3Z — Convergence verdict

**CONVERGED. Ready for T-P3.**

| cycle | ACCEPT | REVISE | REJECT | total | accept-% | character |
|---|---|---|---|---|---|---|
| V1 | 254 | 29 | 2 | 285 | **89.1%** | folding cycle (≥30% REVISE expected; 2 load-bearing REJECT) |
| V2 | 285 | 4 | 0 | 289 | **98.6%** | V1-disposition fold + re-verify cycle |
| V3 | 291 | 0 | 0 | 291 | **100.0%** | convergence cycle (all-ACCEPT on ground-truth re-execution) |

Per-lens V3 (all seven; CH7 is the monotonic-extension OVERFIT-PRUNE lens):

| lens | V1 | V2 | V3 | V3 verdict |
|---|---|---|---|---|
| CH1 CORRECTNESS | 39A/5R/0X | 43A/1R/0X | **44A/0R/0X** | every fold traces to a named T-P1 divergence + resolving file:line/SHA; zero confabulation |
| CH2 GENERALITY | 44A/6R/0X | 51A/1R/0X | **52A/0R/0X** | Lock 14 holds; `G:EventGrammar` carriage + alphabet-as-data transfer grammar-neutrally |
| CH3 REGRESSION | 45A/2R/0X | 47A/0R/0X | **48A/0R/0X** | zero re-opened REDRESS; AZ-IV/StructRegistry/fact-stream/x86 fences hold |
| CH4 COST | 33A/6R/**1X** | 39A/1R/0X | **40A/0R/0X** | every primitive carries scalar-ref + checkasm + same-wave consumer + LOC |
| CH5 HIDDEN-COUPLING | 18A/4R/0X | 23A/1R/0X | **24A/0R/0X** | substrate-union ONE substrate; mask stream transient producer; no cross-call state |
| CH6 ANTI-PAPER-CLOSE | 37A/3R/**1X** | 41A/0R/0X | **41A/0R/0X** | zero engineered-defer; convergence-cycle all-ACCEPT (≠ V1 paper-close all-ACCEPT) |
| CH7 OVERFIT-PRUNE | 38A/3R/0X | 41A/0R/0X | **42A/0R/0X** | genuinely-general tape model; lightningcss the fair bar; no fixture/FNV/broadcast re-entry |

**§3Z criteria, each satisfied:** (1) ≥95% ACCEPT for **two consecutive cycles**
— V2 98.6%, V3 100.0%; (2) **zero orphan REVISE** — the sole V2 residual
(CH1-2F-01-RESIDUAL) is folded and re-verified at V3, no REVISE drew across three
cycles; (3) **V ≤ 5** — converged at V3; (4) zero open critical defects. Both V1
REJECTs (CH4-2a-001 blanket-Lock-16; CH6-V1-R01 aarch64-esoterica grounding-defer
loop) were discharged at V2 and re-verified held at V3. The first-hygiene action
**CH1-V5-001** (the enumerated-filename residual) is VERIFIED-FOLDED-ON-DISK at
`91b6893b0` (`collapsed_stage.rs` exists; `collapsed_tape.rs` absent;
`grep ',collapsed}' {1a,1b,1e}` = 0/0/0; `collapsed_stage}.rs` enumerated form ×3
in `1b`). The brace-glob occurrences surviving in T-P1 hardening artefacts are
quoted-as-the-defect historical records, correctly untouched.

---

## §3Z — The LOCKED fold design (the directed SK-V18 fold)

Five fold designs are LOCKED — carried clean across V2→V3 with zero open
disposition. They are the spec amendments T-P3 3C distils and Pass Omega applies
post-G-Omega. Each is recorded as a LOCKS-AMENDMENTS-CANDIDATE (LAC-2F-FOLD-01..05),
realised in dossier 2F's §2 fold enumeration (F1–F9) and corroborated across
2A/2C/2D/2E.

### 1 — Tape-as-unified-substrate (LAC-2F-FOLD-01 + LAC-2F-FOLD-02; F1/F3/F4/F7)

**Proposal.** The flat tape is the **one substrate the 5 `BackendShape` shapes
project from**, adopted into `crates/core` as a single post-fold encoding. The
spec directs SK-V18 to (i) retire the live **eager `OpenFrame` builders** (F1) and
(ii) converge the AoS `TapeRec` (`crates/core/.../tape/record.rs:103`,
`#[repr(C,align(4))]`, 16-byte/align-4 const-asserted) onto the PROVEN-AND-BENCHED
SoA `Tape<'input>` (`skinny/.../tape/mod.rs:94-100`: `source`, `offsets:Vec<u32>`,
`flag_cursors:Vec<u32>`, `flag_values:Vec<u8>`, `payloads:PayloadArena`, `id`)
as the **single surviving encoding** (F3) — a dual AoS/SoA end-state is admissible
ONLY as a transient fold-state, never a permissible Lock-1 closure. The all-8
`OnceCell<StructuralIndex>` carriers declare `substrate_target` (F7) as the
pre-gate so the index IS the tape, never a sidecar.

- **Grammar-neutral (Lock 14):** the tape members are grammar-blind; `begin_compound`
  reads `layout.rule_id & 0x1F` only (`tape/mod.rs:185-186`), no grammar name enters
  the encoding. PayloadArena carries `substrate_target = existing_tape` /
  `retention_lifetime = output_row` / `policy_owner = generated_grammar`.
- **Lock-surface:** Lock 1 (substrate-union — ONE substrate), Lock 10 (5 shapes ARE
  tape projections, `ARCHITECTURE.md:1088`). `LOCKS.md:75` — exactly one encoding
  survives. 200–700 LOC; high (eager retirement) + medium (encoding); SK-V18 fold.
- **T-P1 antecedent:** Divergence **A** (`1a` SUB17-001 AoS↔SoA) + Divergence **B**
  (eager OpenFrame). Live target: `crates/core/.../css_l4/builder.rs:16`
  (`enum OpenFrame<'p>` + `pending_value` + six `pending_*` Vecs).

### 2 — Lazy `ValueRef<G>` value-API (LAC-2F-FOLD-03 home; F2)

**Proposal.** The spec names **one grammar-parametric `ValueRef<'doc,'input,K,G:EventGrammar>`**
(`skinny/.../tape/mod.rs:175`) as the **unified materialization plane** — replacing
the per-grammar EAGER value enums (the divergence). A single `@generated`
projection generator re-emits all per-grammar value surfaces (`json/value.rs:143`
`value_from_ref`; the absent core `css_l4/value.rs:414` equivalent) by projecting
lazily from `StructLayout`, resolving the layout **once at codegen**, never per-leaf.

- **Grammar-neutral (Lock 14):** the `G:EventGrammar` type parameter is the
  generality vehicle — one projection, per-grammar resolved at the type boundary;
  `@generated` per-grammar allowance keeps it grammar-neutral by construction.
- **preserve-rich-ast:** the lazy view is the materialization plane, never a
  flattening of the typed AST (SPEC `:252`); it lifts the per-grammar EAGER enums
  to the one grammar-parametric projection (`1d` SK17L-002).
- **Lock-surface:** Lock 14 (single generator). `ARCHITECTURE.md:1088` value/output
  plane union.
- **T-P1 antecedent:** Divergence **C** (value-API per-grammar-EAGER; `1c` runtime
  evidence, `1d` SK17L-002). **Scope-honest:** the `ValueRef<G>` value-plane fold is
  exercised **JSON+CSS only** (Sheets/BBNF-self by-construction under SK-V18) — not
  asserted fleet-wide (CH2-V1-R6 fold; `sheets_witness` 24-LOC-stub evidence).

### 3 — Shared NEON classifier primitive-manifest entry (LAC-2F-FOLD-03; F5)

**Proposal.** Register the shared `select_classifier(alphabet)` /
`scan_structural(input, &StructuralAlphabet)` classifier as a **Lock-16
primitive-manifest ROW**: abstract primitive = *alphabet-parametrised byte
classification*; the spec NEON narrative folds from JSON-first to the proven
alphabet-as-data form. Leaf datum `alphabet: &'static [u8;64]`
(`skinny/.../dispatch.rs:42`), wired across 8-of-9 generated grammars (math.rs
excepted). Manifest row carries `scalar-reference =
scalar/byte_class_from_eq_set_64.rs`, checkasm parity, `substrate_target =
existing_tape`, `retention_lifetime = transient-single-call`, same-wave consumer =
the tape.

- **Grammar-neutral (Lock 14):** generality is **config-breadth** — `StructuralAlphabet`
  (`crates/simd-scan/src/alphabet.rs:19-37`: `singletons`/`digraph_mask`/
  `digraph_pairs`/`quote_classes`) is config DATA, not grammar branches; the CSS
  non-JSON consumer binds via the `;{` eq-set fan / slot-59 collision
  (SK-V17 `SPEC.md:314-317`).
- **scalar-ref + checkasm (aarch64-only):** the eq-set fan is the **one real NEON
  Layer-1 body** (87 LOC, 8 distinct NEON intrinsics); `byte_class_from_table_64`
  and `bitmap_prefix_xor_64` are verified **scalar passthroughs** (line-3 delegates),
  honestly declared. Multi-arch `crates/simd-scan` scope-reconciled WITHOUT admitting
  x86 as a close path (no-SVE, `SPEC:806`).
- **Lock-surface:** Lock 14 + Lock 16 (manifest; `LOCKS.md:453-489`,
  `:137-149` no-cross-call-carry). 0-LOC narrative + manifest row; 100–400 LOC
  scope reconcile.
- **T-P1 antecedent:** Divergence **E** (NEON JSON-only vs shared classifier; `1e`
  E-divergence). **(This is the surface that carried the lone V2 REVISE
  CH1-2F-01-RESIDUAL — see residual section; now folded.)**

### 4 — BackendShape-canon disposition: substrate-manifest CATEGORY, not a 6th shape (LAC-2F-FOLD-02; F4 — the primary D-fold)

**Proposal (the dispatch's "propose, do NOT silently add a 6th" discharged in the
negative).** The tape folds into the V1 spec as the **substrate the 5
`BackendShape` shapes project from**, recorded at the Lock 1 substrate manifest
(`substrate_target = existing_tape`, `LOCKS.md:119-127`) — a **substrate-manifest
category**, NOT a 6th `BackendShape` variant. This is the **LAC-1E-14 FactStream
precedent applied verbatim** (`LOCKS.md:100-116`): FactStream is "the 5th admitted-
product *category* at the Lock 1 substrate manifest … a substrate-manifest
classification only; it is NOT a 6th `BackendShape` variant." The 5-shape Lock-10
search domain holds verbatim: `{EagerTape, OffsetTape, EventTape, SinkOnly,
CollapsedStage}` (`LOCKS.md:107-108`).

- **The verdict stands on TWO independent grounds** (not one citation echoed five
  ways): (i) the categorical precedent **LAC-1E-14**; and (ii) the **mechanical
  arch-refusal** of the only arch-gated shape — `admits_collapsed_stage` binds
  `CollapsedStage` to `target.arch == x86 + target.avx512bw`
  (`ARCHITECTURE.md:1151`/`:1282`), so on the aarch64 M5 Max target that predicate
  mechanically refuses, leaving no mechanism by which a 6th aarch64 shape could be
  required.
- **Grammar-neutral (Lock 14):** the substrate-manifest category is grammar-blind —
  every e-graph candidate / backend rewrite / SIMD consumer declares
  `substrate_target` regardless of grammar.
- **Lock-surface:** Lock 1 (substrate manifest) + Lock 10 (5-shape domain). A 6th
  variant remains **G-Omega gated** (`LOCKS.md:109`); SPEC §9-barred (`:808`).
  0 LOC (canon + precedent); medium prose.
- **T-P1 antecedent:** Divergence **D** (`1a` SUB17-005, `1e` D-1E-SKV17-04 /
  LAC-1E-SKV17-05). The aarch64 CollapsedStage is the spec-named **UNKNOWN-2D-05**,
  not a fresh gap; NEON sits under the four LLVM shapes' scan-leaf FFI; no x86 close
  path, no D6 second substrate (SPEC `:854`).

### 5 — StructRegistry / FieldSource fence (LAC-2F-FOLD-04; F6 — the regression firewall)

**Proposal.** Add the **no-per-leaf-registry-lookup fence** to the substrate
manifest: the `FieldSource` projection walk inside the live `StructRegistry`
(`crates/ir/src/registry/struct.rs:84` `FieldSource`, `:313` `StructRegistry`,
`:331` `layout(rule_id)`) is **compile-time emission resolved once at codegen**.
ANY per-leaf runtime `StructRegistry::layout(rule)` indirection in the tape/
projection hot path re-opens the measured **28-65× / 983× / 10583×** regression
(SPEC `:793-795`) and is REJECT. The lazy `ValueRef<G>` generator (fold #2)
resolves the layout once at codegen, never per-leaf.

- **Grammar-neutral (Lock 14):** the fence is a property of the projection emission,
  grammar-blind. `begin_compound` reads `layout.rule_id & 0x1F` only
  (`tape/mod.rs:185-186`, grep-zero `StructRegistry`) — fence-clean.
- **Live coupling site named (CH5 fence target):**
  `crates/core/.../bbnf/arena.rs:47` = `match
  StructRegistry::compound_kind_for_layout(layout) {` inside the **eager arena
  path** — the present-tense coupling FOLD-B (eager-OpenFrame retirement) severs.
- **Lock-surface:** Lock 1. 0 LOC (fence); high (regression class); SK-V18 fold gate.
- **T-P1 antecedent:** Divergence **F** (StructRegistry/FieldSource fence; `1e`
  D-1E-SKV17-03 / LAC-04). Keeps the **AZ-IV eager indirection pre-blocked**
  (SPEC `:791-794` 118× regression).

> A sixth LAC, **LAC-2F-FOLD-05** (refinement, Lock 2), re-prices the
> `StructLayout`→`Layout` canonical-name reconcile by two disjoint paths — (a)
> 960-site rename, generator-side; (b) `LayoutFacts.backend_shape` side-table,
> sized as the 0→N introduce-site delta (`grep StructLayout crates/`=960 vs
> `grep 'backend_shape|LayoutFacts' crates/`=0). It is a Lock-2 sub-surface
> reconcile, not one of the five core fold designs; carried clean V2→V3.

---

## §3Z — REJECTed proposals (refutations, the most load-bearing rows)

T-P2 refuses these by construction; each is a refutation row, not a deferral.

1. **A 6th `BackendShape` variant — REFUTED.** The tape is the substrate the 5
   shapes project from, recorded at the substrate manifest (fold #4); a 6th variant
   is G-Omega-gated (`LOCKS.md:109`) and SPEC §9-blocked (`:808`). The dispatch's
   "do NOT silently add a 6th" is discharged in the negative on TWO grounds
   (LAC-1E-14 + `admits_collapsed_stage` x86-binding).
2. **Per-leaf runtime `StructRegistry::layout(rule)` projection walk — REFUTED**
   (CH4 + CH5 + CH3 corroborated). Re-opens the 28-65×/983×/10583× regression
   (SPEC `:793-795`); the walk MUST be compile-time resolved once at codegen
   (fold #5). This is the AZ-IV indirection pre-block held.
3. **AoS/SoA dual end-state — REFUTED as a Lock-1 closure.** Exactly ONE encoding
   survives (`LOCKS.md:75`); AoS/SoA coexistence is admissible ONLY as a transient
   fold-state (fold #1).
4. **The fabricated "recognizer beats lightningcss 2-3×" (V1 CH7-001) — DELETED**
   and held deleted V2→V3. The CSS lightningcss bar is UNMEASURED-PENDING
   (`SPEC:207`); the >SOTA carrier re-grounds to the measured JSON recognizer
   `> sonic-rs` fact (`RESULTS.md:5-12`: twitter parse_only 8349.290 > sonic
   4913.095; direct 17585.679 > strict 14857.624; typed 10705.052 > 8952.253).
5. **Fleet-wide value-plane proof — REFUTED** (CH2). The `ValueRef<G>` value-plane
   fold is JSON+CSS-exercised only; Sheets/BBNF-self are by-construction under
   SK-V18, not by-exercise (`sheets_witness` 24-LOC stub, SK17L-009).
6. **The `udot`/i8mm digit-MAC orphan kernel — REFUTED** (CH4); no CSS antecedent,
   no live consumer. FOLD-L9 (Alt-mode) sits in the deferred appendix, not the wired
   set. No fixture/FNV/broadcast re-entry across the LOCKED skinny pre-blocks
   (L-SK17-04/05, SK17L-010).
7. **Any x86/AVX-512/SVE close route — REFUTED** as a close path. Every such mention
   sits in refuted-route / `admits_collapsed_stage` co-requirement / architecture-
   pressure context. aarch64 is primary; no SVE (`SPEC:806`); asmjson/Sneller framed
   host-blocked SPEC §9, diagnostic-only.

---

## §3Z — Residual REVISE

**ZERO orphan REVISE at V3.** The convergence trajectory carried exactly one
residual into V3, now folded:

- **CH1-2F-01-RESIDUAL** (the lone V2 REVISE — a citation-precision defect, claim
  true): the `alphabet.rs:118` line-anchor used for the `StructuralAlphabet`
  manifest grounding was re-pointed to the struct-definition site
  `alphabet.rs:19-37`, with `:118` correctly retained for the orthogonal
  `KernelShape::select(alphabet)`. The fold landed in **three dossiers** (2F
  LAC-2F-FOLD-03 `2f:582`; 2D FOLD-2D-05 / LAC-2D-S17-03 / source list; 2A's
  existing `:19-37` confirmed, no edit). Live-verified at `91b6893b0`: `:19` =
  `pub struct StructuralAlphabet {`; `:118` = `pub fn select(alphabet:
  &StructuralAlphabet)`. **No new mis-anchor introduced** — the only remaining
  `:118`-for-struct occurrences are inside `V2/CH1.md` as quoted-defect history.
  The CH1-V5-001 recorded-but-unapplied pathology did **not** recur.
  **DISCHARGED.**

No REVISE drew across three consecutive cycles; no author paper-folded; the
orchestrator escalation trigger (same REVISE × 3, V4 ceiling, V5 BLOCKED) did not
fire.

---

## §3Z — Next move: ready-for-T-P3

The fold architecture is **sound and stable** against live source at master
`91b6893b0`, each design grammar-neutral (Lock 14) / substrate-union-preserving
(Lock 1) / 5-shape-canon-coherent (Lock 10) / scalar-ref + checkasm (Lock 16,
aarch64-only):

- **tape-as-unified-substrate** — eager-OpenFrame retirement (`css_l4/builder.rs:16`
  live target) + AoS→SoA single-encoding closure + all-8 `OnceCell` substrate_target
  pre-gate;
- **lazy `ValueRef<G>` value-API** — the one grammar-parametric materialization
  plane, `@generated`-allowed, preserve-rich-ast;
- **shared NEON classifier** — Lock-16 manifest row, eq-set fan the one proven NEON
  body, table/prefix scalar-delegate-non-ASM, checkasm-parity;
- **BackendShape-canon disposition** — substrate-manifest CATEGORY (LAC-1E-14
  precedent + independent `admits_collapsed_stage` x86-refusal), no silent 6th;
- **StructRegistry/FieldSource fence** — live coupling at `arena.rs:47`,
  compile-time-resolved-once, AZ-IV pre-blocked.

**T-P3 inherits** the converged fold dossiers (2A–2F) + five LOCKED
LOCKS-AMENDMENTS-CANDIDATEs (LAC-2F-FOLD-01..05, +the Lock-2 sub-surface
LAC-2F-FOLD-05 reconcile) and three Open Research Questions for synthesis
disposition: **2F-FOLD-U1** (SoA `Tape` as the declared SK-V18 convergence-target
encoding vs AoS-keep-and-prove-parity — 2F recommends SoA, the proven-and-benched
encoding; parity-vs-adopt is a T-P3 call); **2F-FOLD-U2** (each of the 8
`OnceCell<StructuralIndex>` carriers classified `existing_tape` vs `local_temp_only`
BEFORE wiring, else REDRESS-53 re-entry); **2F-FOLD-U3** (whether aarch64
CollapsedStage ever admits, or NEON permanently sits under the 4 LLVM shapes'
scan-leaf FFI — no x86 close path, no D6 second substrate).

Per PASS-2-RESEARCH §6, T-P2 convergence reaches **G2** (optional pin). With the
§3Z criteria met — two consecutive ≥95% cycles (V2 98.6%, V3 100.0%), zero orphan
REVISE, V ≤ 5, zero open critical defects — G2 is **auto-close eligible**. On G2
close the orchestrator updates `restart/HANDOFF.md` to **ready-for-T-P3** and
dispatches T-P3 per `totality/PASS-3-SYNTHESIS.md`. T-P3 does not dispatch before
G2.

**VERDICT: T-P2 CONVERGED at V3 (100.0% ACCEPT, second consecutive ≥95% cycle,
zero orphan REVISE, V = 3 ≤ 5). The greater-spec fold is locked and ready for
T-P3 synthesis.**
