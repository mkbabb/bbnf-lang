---
agent: 2C
pass: T-P2-research
cycle: V3
generated_at: 2026-05-29T23:25:40Z
v2_regenerated_at: 2026-05-29T20:10:00Z
v3_regenerated_at: 2026-05-29T21:40:00Z
subject: tape-fold (greater-spec fold of the SKINNY-proven unified-tape / lazy ValueRef<G> projection / StructLayout / aarch64-NEON model into the V1 totality spec)
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p1_divergences_grounded: [A-tape-AoS-SoA, B-eager-OpenFrame, C-value-API-eager, D-BackendShape-5shape-canon, E-NEON-shared-classifier, F-StructRegistry-FieldSource-fence]
master_head: 91b6893b0
t_p1_source_sha: 445925167
primary_sources_cited: 13
techniques_grounded: 9
techniques_refuted: 4
hygiene_action_CH1_V5_001:
  status: ALREADY-FOLDED-AT-HEAD
  note: "CH1-V5-001 (the enumerated-filename residual) is the 1b live_truth_method + BSHAPE17-004 brace-glob `{eager,offset,event,collapsed}_tape.rs` (non-existent collapsed_tape.rs; real file collapsed_stage.rs). The prompt directs folding it on first touch of 1a/1e; on inspection the defect was located in 1b, NOT 1a/1e. Live grep at HEAD 91b6893b0: 1a + 1e carry ZERO brace-glob; 1b:12 + 1b:97 already carry the enumerated executing form `{eager_tape,offset_tape,event_tape,collapsed_stage}.rs=17 each, sink_only.rs=270` (folded at the convergence commit 91b6893b0). The only remaining brace-glob occurrences are inside the hardening artefacts (V5/V6-confirm CH1.md/CH6.md, HARDENING-*-CONSOLIDATED.md) where they are QUOTED AS THE DEFECT being recorded — correct historical records, not to be altered. Hygiene action satisfied; no edit needed at this SHA."
v2_challenge_2c_verdict:
  status: ALL-ACCEPT-ZERO-OPEN-REVISE
  note: "The V2 six-lens wave returned ZERO REVISE and ZERO REJECT bound to 2C. CH1 (all 7 folds + 8 grounding rows ACCEPT, CH1.md:157-160), CH2 (2C folds ACCEPT; CH2-V1-R4 folded; the one residual REVISE CH2-V2-R1 is 2B's FOLD-L6, not 2C), CH3 (0 REJECT/0 REVISE, CH3.md:225), CH4 (all 6 candidates + ONBOARD + 2 LACs ACCEPT, CH4.md:176), CH5 (all rows ACCEPT, CH5.md:129-135), CH6 (7/7 ACCEPT, CH6.md:59), CH7 (CONVERGED 41/41, CH7.md:181). The V2 residual REVISEs (CH4-V2-001, CH2-V2-R1, CH5-V2-001, CH1-2F-01-RESIDUAL) all bind to 2B/2F dossiers, NOT 2C — no orphan REVISE carried into V3."
v3_dispositions_folded:
  accepted: []
  rejected: []
  revised: []   # zero open REVISE bound to 2C from the V2 wave
  cleanliness_nits_discharged:
    - CH6-V2-COUNT-NIT   # CH6.md:161-162 flagged the ONBOARD Predicate-2 prose ("8 grammar dirs") mismatching its inline 9-item enumeration. Re-run live at HEAD 91b6893b0: find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d = 9 grammar dirs (bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math) + tape/. Leading numeral corrected to 9 at the ONBOARD Predicate-2 body, the frontmatter recap, and the CH6 self-anchor. Ground-truth-exact; CH1-lane line-ref class; not a gating REVISE.
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - CH1-2C-01    # ARCH:1803 ordinal disambiguation — plane-table (3) vs substrate-category 5th; FactStream row carries both, neither a shape. Folded at Candidate-D body / grounding-table / Defended-#3 / refuted-#1 / LAC-SK17-2C-01.
    - CH2-V1-R4    # Candidate-E CSS non-JSON consumer bound to the eq-set fan (byte_class_from_eq_set_64_neon, the one real NEON body) via the slot-59 collision (SPEC:316-317); table_64 aarch64 entry recorded as scalar-delegate-non-ASM. Folded at grounding-table / Candidate-E / Defended-#4 / LAC-SK17-2C-02.
    - CH6-V1-V01   # ONBOARD reclassified as a verify_action with live HEAD baseline executed at 91b6893b0 reported inline (Predicate 1 = 7 string-ident/doc sites in strategy.rs = catalogued ARCH-3A-D09 leak surface, monotonic-decrease; Predicate 2 = 9 @generated grammar dirs + tape/ substrate, clean).
    - CH6-V1-V03   # D-fold carries an independent corroborating anchor (ARCH:1206 CollapsedStage x86-only/aarch64-refused) beyond the shared LAC-1E-14 citation.
  first_cycle_additions:
    - SK17-2C-A-AOS-SOA-GRAMMAR-NEUTRAL-OFFSET-TAPE
    - SK17-2C-B-OPENFRAME-RETIREMENT-GRAMMAR-NEUTRAL
    - SK17-2C-C-VALUEREF-G-LAZY-PROJECTION-PLANE
    - SK17-2C-D-BACKENDSHAPE-SUBSTRATE-MANIFEST-NOT-6TH-SHAPE
    - SK17-2C-E-SELECT-CLASSIFIER-ALPHABET-LOCK16-PRIMITIVE
    - SK17-2C-F-FIELDSOURCE-COMPILE-TIME-FENCE-GRAMMAR-NEUTRAL
    - SK17-2C-ONBOARD-FUTURE-GRAMMAR-ONBOARDING-VERIFY-ACTION
    - SK17-2C-BY-CONSTRUCTION-NOT-BY-EXERCISE-SPLIT
locks_amendment_candidates: 2
---

## Executive Summary

2C grounds the **grammar-neutrality (Lock 14) of the tape-fold** — the fold of the
SKINNY-proven flat-tape + lazy `ValueRef<G>` projection + `StructLayout`-walk +
shared aarch64-NEON `select_classifier(alphabet)` model into the V1 totality spec.
The load-bearing question: does the fold of each of the six T-P1 divergences (A
tape AoS↔SoA, B eager `OpenFrame`, C eager value-API, D 5-shape canon absorption,
E NEON, F `StructRegistry`/`FieldSource` fence) keep the substrate grammar-neutral
per Lock 14 — "the substrate carries ZERO grammar-specific code" (`LOCKS.md:349`)?

The grammar-neutrality vehicle is proven and load-bearing: the SKINNY tape carries
the grammar in a **type parameter** — `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>`
(`skinny/crates/runtime/src/tape/mod.rs:175`) — and the NEON leaf's only grammar
datum is `alphabet: &[u8;64]` passed to `select_classifier` (Lock-14 vehicle,
`restart/skinny/tranches/sk-v17/SPEC.md:314-315`). The fold is grammar-neutral
**by construction**: the generic substrate (`Tape`, `TapeBuilder`, `ValueRef`,
`scan_structural`) carries no grammar policy; per-grammar surfaces are `@generated`
(`crates/core/src/runtime/{json,css_l4}/value.rs:1`), which Lock 14 ALLOWS.

Four refutations constrain T-P3 hardest: (1) the divergence-D **6th BackendShape**
is refuted — the tape is a substrate-manifest *category* under the 5-shape canon
(LAC-1E-14 precedent, `LOCKS.md:107-108`), not a new shape; (2) grammar-neutrality
is proven **by-construction on JSON+CSS only**, NOT by-exercise fleet-wide — a fold
claimed "grammar-neutral, fleet-proven" without a Sheets/BBNF-self witness fails
the Lock 14 v+1 per-wave gate (`LOCKS.md:374-387`); (3) the divergence-F
`FieldSource` walk is grammar-neutral ONLY if compile-time-resolved — a per-leaf
runtime `StructRegistry` walk re-opens the 28-65×/983×/10583× regression and is
the fence, not the fold; (4) the divergence-B eager `OpenFrame` is itself the
Lock-14-named overfit ("CSS L4 14-variant `OpenFrame`", `LOCKS.md:349`) — its
retirement is a Lock 14 obligation, not merely a performance one. No 6th shape,
no new substrate, no parallel directive proposed.

## Technique Grounding Table

| spec-claim or T-P1 divergence id | published source / live-evidence cited | grounded / refuted / partial | bbnf-specific note (grammar-neutral transfer reason) |
|---|---|---|---|
| **Type-parameterised value plane** (`ValueRef<…,G:EventGrammar>` carries the grammar in a type param, not a runtime branch) | `skinny/crates/runtime/src/tape/mod.rs:175,191,202`; `event_grammar::{AnyGrammar,EventGrammar}` `:11`; Lock 14 (`LOCKS.md:349`) | grounded | The grammar enters the value plane as a TYPE parameter `G` and the kind as `K`, monomorphised at codegen — zero `match grammar {…}` arm at runtime. This is the Lock-14-honoured generalisation vehicle: the substrate is generic over `G`, the per-grammar `value_from_ref` is `@generated` per grammar. |
| **Alphabet-as-data classifier** (`select_classifier(alphabet:&[u8;64])` — the one grammar datum) | `restart/skinny/tranches/sk-v17/SPEC.md:314-317`; `skinny/crates/bbnf-simd/src/dispatch.rs:42,89-113`; eq-set NEON body `aarch64/byte_class_from_eq_set_64.rs:33`; core `scan_structural(input,&StructuralAlphabet)` `crates/simd-scan/src/lib.rs:80` | grounded | simdjson/sonic-rs classify a FIXED JSON alphabet (`{ } [ ] : , "`); bbnf's classifier takes the alphabet as DATA. The grammar's structural bytes are a `[u8;64]` (skinny) / `StructuralAlphabet` (core) mined per grammar — Lock 14's "Quote, escape, control, delimiter … must come from generated grammar config" (`LOCKS.md:418-423`). The classifier is **wired (scan-leaf) across 8 core grammars** (1B BSHAPE17-009) — config-breadth; the value-plane fold is by-exercise on JSON+CSS only. CSS exercises a REAL NEON body, not a scalar delegate: its `;{` pair collides on the `& 0x3f` slot-59 table slot so it routes to the eq-set fan (`SPEC.md:316-317`), the one proven aarch64 NEON Layer-1 body. |
| **`@generated`-per-grammar value/builder/view modules** (Lock-14 ALLOWED, not hand-written) | `crates/core/src/runtime/{json,css_l4}/value.rs:1` (`// @generated by xtask regen-{json,css}`); Lock 14 v+1 generated-output allowance (`LOCKS.md:352-358`) | grounded | The per-grammar locus is NOT a Lock 14 violation — the files are template-emitted from grammar source + workspace metadata. The divergence (1A/1C/1D/1E/1F unanimous) is the EAGER materialization SHAPE, not the per-grammar provenance. The fold retargets the GENERATOR to emit lazy `ValueRef<G>`, not the hand-authoring of per-grammar code. |
| **Abstract-primitive lift (cross-grammar primitive translation)** | `LOCKS.md:472-489` (dav1d/ffmpeg primitives translate to byte-stream parsing for ANY grammar; per-grammar selection cost-model-derived); `restart/skinny/tranches/sk-v17/SPEC.md:594` (L5 `comment_body_mask_64`, L6 `bracket_depth_mask_64` digraph-parameterised) | grounded | The dav1d pixel kernels do not transfer (`LOCKS.md:485-489`); the PRIMITIVES underneath (cross-lane permute, MAC, saturating arith) do. The bbnf-specific reason: the per-grammar selection is `cost-model-derived from Grammar IR (alphabet size, number-token presence, …)` — DATA, not a grammar branch. |
| **BackendShape as 5-shape canon (no 6th shape)** | `LOCKS.md:107-108` (`{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`); 1E Verified-Invariant (`FactStream` is the 5th substrate-manifest *category*, NOT a 6th shape, `LOCKS.md:100-109`); `restart/skinny/tranches/sk-v17/SPEC.md:808` (SK-V17 §9 bars a 6th shape) | grounded | The proven tape is the OffsetTape shape's substrate, NOT a new shape. Grammar-neutrality holds because the shape is selected per-rule by the cost model (data), and the tape substrate is shared across all shapes. The 6th-shape route is refuted below. |
| **Divergence-D: tape as a SIXTH BackendShape** | `LOCKS.md:107-108` (5-shape canon, monotonically fixed); SK-V17 §9 (`SPEC.md:808` "no 6th shape"); 1B U-BSHAPE17-001 | **refuted** | The flat tape is the *substrate* the OffsetTape/EventTape shapes project, NOT a shape peer. Per the LAC-1E-14 precedent, the live `FactStream` row sits at the output-plane table at `ARCHITECTURE.md:1803` — plane-table ordinal **(3)** "Fact-stream output" — and that SAME row carries "Per LAC-1E-14 this is the 5th SUBSTRATE-manifest category, not a 6th BackendShape variant" (`ARCHITECTURE.md:1803`; `LOCKS.md:100-108`). The two ordinals are distinct: plane-table row (3) ≠ substrate-manifest category 5th. A substrate/output category is NOT a shape peer either way; the tape is likewise a substrate-manifest category under the canon, not a shape. Adding it as a 6th shape would be a silent new directive (G-Omega gated). REFUTED: propose substrate-manifest category, never a 6th shape. |
| **Divergence-F: per-leaf `FieldSource`/`StructRegistry` walk as the projection mechanism** | `restart/skinny/tranches/sk-v17/SPEC.md:793-795` (28-65×/983×/10583× regression); `crates/ir/src/registry/struct.rs:84,313,331`; `crates/core/src/runtime/tape/mod.rs:185-186` (`begin_compound` reads `layout.rule_id` only) | **refuted (as runtime walk)** | A per-leaf runtime `StructRegistry::layout(rule)` walk IS the AZ-IV/StructRegistry indirection refuted at 28-65×/983×/10583×. The `FieldSource` walk is grammar-neutral ONLY as compile-time projection-emission resolved once. REFUTED as a hot-path mechanism; grounded as a codegen-time mechanism. |
| **Divergence-B: eager `OpenFrame` carried forward** | `LOCKS.md:349` (names "CSS L4 14-variant `OpenFrame`" as the overfit Lock 14 prevents); `crates/core/src/runtime/css_l4/builder.rs:16` (817 LOC, 7 pending fields); AZ-IV pre-block (`SPEC.md:791`) | **refuted (retire, never carry)** | The eager `OpenFrame` god-module is BOTH the AZ-IV 118× eager-tree pre-block AND a Lock-14-named overfit. Its retirement is a Lock 14 obligation, not just a perf one. REFUTED: replace with lazy `ValueRef<G>`, do not carry forward. |
| **Fleet-wide grammar-neutral CLAIM on JSON+CSS exercise alone** | `restart/skinny/tranches/sk-v17/SPEC.md:112-113` ("Projection generality exercised by-construction on JSON + CSS only; Sheets/BBNF-self is the SK-V18 proof"); Lock 14 v+1 gate (`LOCKS.md:382-387` "With only one of Sheets or BBNF-self, the claim is scoped to the witnessed grammars and may not use fleet-wide grammar-neutral wording") | **refuted (as a fleet-wide claim)** | `sheets_witness` is a 24-LOC `EventGrammar` stub with NO `.bbnf`/`BackendRule` (1D SK17L-009) — it CANNOT serve as a projection exercise. The fold's grammar-neutrality is breadth-of-CONFIG (alphabet/layout-as-data), proven by-exercise on JSON+CSS only. REFUTED: T-P3 must scope the wording, not claim fleet-wide proof. |

## §2 Candidate / Fold Enumeration (load-bearing)

Each candidate is one fold of one T-P1 divergence, carrying: **shape** (what the
fold is), **T-P1-divergence antecedent** (the source divergence id + file:line),
**grammar-neutral verdict** (Lock 14 disposition), and **lock surface** (which V1
locks the fold touches + the fence). Substrate-union-preserving (Lock 1),
5-shape-canon-coherent (Lock 10), scalar-ref+checkasm where NEON (Lock 16),
grammar-neutral (Lock 14). aarch64 only. preserve-rich-ast throughout.

### Candidate SK17-2C-A — Flat offset tape as the grammar-neutral substrate

- **Shape.** Fold the core 16-byte AoS `TapeRec` (`crates/core/src/runtime/tape/record.rs:103`)
  and the skinny SoA `Tape<'input>` (`offsets:Vec<u32>` + sparse `flag_cursors`/`flag_values`
  + `PayloadArena`, `skinny/crates/runtime/src/tape/mod.rs:94-100`) to EXACTLY ONE
  encoding post-SK-V18. The substrate carries NO grammar column — flags are sparse
  position-keyed side-vectors, not a per-grammar dense class column.
- **T-P1-divergence antecedent.** Divergence A (1A SUB17-002, 1C RT17-001, 1F COH17-001):
  cross-tree shape drift AoS↔SoA; Lock-1 exactly-one-encoding closure (`LOCKS.md:75`
  "parallel substrates are dead"). The proven SoA is the Lock-1-ADMISSIBLE offset tape
  (`LOCKS.md:75,:86`), NOT the buried AV.04 dense class-column SoA (`LOCKS.md:784`).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (Lock 14 honoured). The tape substrate
  is grammar-as-data: `begin_compound(&StructLayout)` dispatches on the `StructLayout`,
  "never on per-grammar route strings" (`crates/core/src/runtime/tape/mod.rs:54-56`).
  Both encodings are grammar-neutral; the choice between them is a perf/parity question,
  not a grammar-neutrality one. The fold does not introduce a grammar branch.
- **Lock surface.** Lock 1 (one encoding post-fold; dual is a transient fold-state only).
  Lock 14 (no grammar column in the substrate — FENCE: a kind-partitioned dense class
  column would be the AV.04 overfit, barred). Lock 10 (the encoding is the OffsetTape
  shape's substrate, not a new shape). **Fence:** the surviving encoding must remain
  grammar-column-free; sparse flags only.

### Candidate SK17-2C-B — Eager `OpenFrame` retirement (Lock-14 overfit deletion)

- **Shape.** DELETE the eager `JsonStructBuilder`/`CssStructBuilder` `OpenFrame`
  builders (`json/builder.rs:9` 231 LOC; `css_l4/builder.rs:16` 817 LOC god-module,
  6 `pending_*` Vecs + 1 `pending_value:Option` `:71-79`); wire the tape builder
  (`push_plain_offset`, branchless u32 write).
- **T-P1-divergence antecedent.** Divergence B (1A SUB17-003 Builder row, 1D SK17L-003,
  1E D-1E-SKV17-02): the eager `OpenFrame` is the live substrate; AZ-IV 118× eager-value-tree
  pre-block (`SPEC.md:791`); the CSS builder is a 817-LOC god-module.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL OBLIGATION (Lock 14 mandate, not
  merely allowed). Lock 14 NAMES "CSS L4 14-variant `OpenFrame`" as "the current
  overfitting mess … the failure mode this lock prevents from recurring"
  (`LOCKS.md:349`). The 7-field `pending_*` per-grammar builder is grammar-specific
  state. Retiring it for the generic tape builder REMOVES grammar-specific substrate
  code — a Lock 14 win, beyond the perf win. The replacement (`push_plain_offset`)
  carries zero grammar policy (`SK17L-001`, "carries no grammar policy", 1D:107).
- **Lock surface.** Lock 14 (the `OpenFrame` is the named overfit; deletion is the
  lock's purpose). Lock 1 (no `Vec<OpenFrame>::clone`; the tape is the one substrate).
  AZ-IV pre-block (replace, never carry). **Fence:** the same-wave replacement (tape
  consumer) must be proven BEFORE deletion (no-delete-before-replacement, Lock 14
  v+1 `LOCKS.md` + 2C-prior-totality precedent).

### Candidate SK17-2C-C — Lazy `ValueRef<G>` projection as the unified materialization plane

- **Shape.** Retarget the per-grammar value-API generator (`xtask regen-{json,css}`)
  to emit the lazy `ValueRef<'doc,'input,K,G:EventGrammar>` projection
  (`skinny/crates/runtime/src/tape/mod.rs:175`) read by `value_from_ref` per grammar
  (`skinny/crates/runtime/src/grammars/json/value.rs:143`), replacing the eager
  per-grammar typed enums (`CssTypedValue<'p>` `css_l4/value.rs:414`). ONE
  `BackendRule`-walking generator emits document/value/view/visitor (`SPEC.md:54-61`).
- **T-P1-divergence antecedent.** Divergence C (1A SUB17-004, 1C RT17-002, 1D SK17L-002,
  1E D-1E-SKV17-02, 1F COH17-002): per-grammar EAGER value enums, NO `ValueRef`/
  `value_from_ref` in core (grep-zero); the value API is `@generated` (Lock-14 ALLOWED),
  EAGER not hand-written.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (Lock 14 honoured, preserve-rich-ast).
  The grammar enters via the type parameter `G:EventGrammar` (monomorphised at codegen),
  not a runtime branch; the per-grammar `value_from_ref` is `@generated` from grammar
  source + metadata (Lock 14 v+1 allowance, `LOCKS.md:352-358`). The lazy `ValueRef`
  view preserves the rich AST (CSSOM via lazy projection, not flattened, not eager —
  `css_rich_ast_preserved`, `SPEC.md:172,558`). The divergence is the EAGER shape, NOT
  the per-grammar locus.
- **Lock surface.** Lock 14 (type-param grammar carriage + `@generated` allowance —
  the generalisation vehicle). preserve-rich-ast (the lazy view is the rich-AST plane,
  not a span flatten). Lock 1 (the projection IS over the one tape, no second value tree).
  **Fence:** a CSS-only generator that never re-emits JSON FAILS the round-trip
  (`SPEC.md:62,557`); the generator must re-emit JSON byte-equal (CH2 firewall).

### Candidate SK17-2C-D — BackendShape: tape as substrate-manifest category, NOT a 6th shape

- **Shape.** The flat tape is the SUBSTRATE that the OffsetTape/EventTape `BackendShape`
  shapes project — a substrate-manifest category under the existing 5-shape canon
  (`{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`, `LOCKS.md:107-108`),
  NOT a new shape. The aarch64-NEON union sits under the four LLVM shapes' scan-leaf
  FFI (no aarch64 CollapsedStage; that is the spec-named UNKNOWN-2D-05, `ARCH:1206`).
- **T-P1-divergence antecedent.** Divergence D (1A SUB17-005, 1B BSHAPE17-005, 1C RT17-007,
  1D SK17L-006, 1E D-1E-SKV17-04, 1F COH17-004): §7.3 `CollapsedStage` is x86/AVX-512-pinned;
  SK-V17 aarch64-NEON has no CollapsedStage; the 5-shape canon must absorb the tape-as-substrate.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (Lock 14 honoured) AND 5-shape-canon-coherent
  (Lock 10). The shape is selected per-rule by the cost model (data-derived from Grammar
  IR), and the tape substrate is shared grammar-neutrally across all shapes. Per the
  LAC-1E-14 precedent, a substrate/output category is NOT a shape peer: the live `FactStream`
  row sits at output-plane-table ordinal **(3)** "Fact-stream output" (`ARCH:1803`) AND that
  same row declares itself "the 5th SUBSTRATE-manifest category, not a 6th BackendShape
  variant" — plane-table ordinal (3) and substrate-manifest category 5th are distinct
  ordinals on one row, neither a shape. **Independent corroborating anchor (beyond LAC-1E-14):**
  the `CollapsedStage` row at `ARCH:1206` mechanically proves no 6th-shape route is needed on
  the SK-V17 aarch64 target — the only AVX-512-FSM shape is `target.arch == x86 + avx512bw`
  gated, "aarch64 mechanically refused" (`ARCH:1206`, LAC-2D-06), so the aarch64-NEON union
  absorbs into the four LLVM shapes' scan-leaf FFI and creates no shape pressure. PROPOSE the
  substrate-manifest category; do NOT silently add a 6th shape (G-Omega gated, §3W).
- **Lock surface.** Lock 10 (5-shape canon fixed; no 6th shape). Lock 14 (per-rule shape
  selection is data, not a grammar branch). Lock 1 (the substrate-manifest category is
  the tape; one substrate). **Fence:** any proposal that reads as a 6th `BackendShape`
  is REJECT (no silent shape); aarch64-NEON absorbs into the four LLVM shapes.
  **LAC candidate** (LAC-SK17-2C-01 below).

### Candidate SK17-2C-E — Shared NEON `select_classifier(alphabet)` as a Lock-16 primitive-manifest entry

- **Shape.** The shared aarch64-NEON byte-class classifier `select_classifier(alphabet:&[u8;64])`
  (`skinny/crates/bbnf-simd/src/dispatch.rs:42`) / core `scan_structural(input,&StructuralAlphabet)`
  (`crates/simd-scan/src/lib.rs:80`) is the grammar-neutral structural-scan leaf — already
  grammar-general across 8 core grammars (1B BSHAPE17-009, impl-exceeds-spec). The fold
  records it as a Lock-16 primitive-manifest entry: scalar-ref + checkasm parity, abstract
  primitive name, same-wave consumer.
- **T-P1-divergence antecedent.** Divergence E (1A SUB17-007/008, 1B BSHAPE17-009,
  1E D-1E-SKV17-06, 1F COH17-005/008): NEON spec-narrative is JSON-framed; the impl is
  already alphabet-parametrised + grammar-general; core scan is multi-arch (impl-exceeds-spec).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (Lock 14 vehicle, Lock 16 admissible).
  The alphabet is the ONLY grammar datum (`SPEC.md:314-315`); the classifier is generic
  over the alphabet. Lock 16 v+1 requires a manifest row (`stable primitive id, abstract
  primitive name, primary ISA/library citation, hardware gate, scalar reference, strict
  checkasm/parity command, … grammar policy source, … same-wave production consumer`,
  `LOCKS.md:495-505`). Lock 16: "A primitive claimed grammar-neutral must exercise at
  least one non-JSON consumer or record a measured deletion/rejection" (`LOCKS.md:423-425`)
  — **CSS L4 is the non-JSON consumer, and it binds to a REAL NEON body, not a scalar
  delegate.** The CSS `;{` structural pair collides on the `& 0x3f` low-6-bit table slot
  (`;`=0x3B → slot 59; `{`=0x7B → slot 59), so `lo6_table_admissible` returns false
  (`dispatch.rs:101-113`) and CSS routes to the **eq-set fan** `byte_class_from_eq_set_64`
  (`SPEC.md:316-317` verbatim "the CSS `;{` pair uses the eq-set fan, NOT the lo6 table (the
  `& 0x3f` slot-59 collision)"). That eq-set fan IS the one real aarch64 NEON Layer-1 body —
  `byte_class_from_eq_set_64_neon` (`aarch64/byte_class_from_eq_set_64.rs:33`, vld1q/vceqq/vorrq
  stripe reduction), with scalar reference (`scalar/byte_class_from_eq_set_64.rs:26`) and
  checkasm parity. By contrast `byte_class_from_table_64_neon` is a 2-LOC scalar passthrough
  (`aarch64/byte_class_from_table_64.rs:2-3` → `crate::scalar::byte_class_from_table_64_scalar`),
  so the Lock-16 "≥1 non-JSON consumer" requirement binds to the eq-set fan's real NEON body,
  not the table delegate.
- **Lock surface.** Lock 16 (primitive-manifest row; scalar-ref + checkasm; aarch64 gate;
  abstract primitive name `byte_class_from_eq_set_64` — the proven NEON body — with the
  `byte_class_from_range_64` sibling `LOCKS.md:332-339`; the `byte_class_from_table_64`
  aarch64 entry is a scalar-delegate-non-ASM close-state, NOT a NEON-body manifest row).
  Lock 14 (alphabet-as-data; no JSON constant in the generic classifier). **Fence:** no
  retained cross-call classifier state (Lock 1 v+1 ELEVATION, `LOCKS.md:137-149`) — the
  alphabet is per-call constructed; x86/AVX/SVE barred (aarch64 only).

### Candidate SK17-2C-F — `StructRegistry`/`FieldSource` compile-time fence (grammar-neutral projection)

- **Shape.** The `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}`
  walk (`crates/ir/src/registry/struct.rs:84`) that produces the `ValueRef<G>` projection
  is COMPILE-TIME projection-emission, resolved ONCE at codegen, baked into the generated
  parser body — NEVER a per-leaf runtime `StructRegistry::layout(rule)` lookup. The fence
  keeps the AZ-IV/StructRegistry indirection pre-blocked.
- **T-P1-divergence antecedent.** Divergence F (1A SUB17-009 + Firewall row, 1B BSHAPE17-006/007,
  1C RT17-006, 1D SK17L-004, 1E D-1E-SKV17-03): the `StructRegistry`/`Arena<G>`/`Builder<G>`
  hot-path indirection refuted at 28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG
  tailwind (`SPEC.md:793-795`); `begin_compound` already takes a pre-resolved `&StructLayout`.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL ONLY AS A COMPILE-TIME WALK. The `FieldSource`
  walk IS the `BackendRule`-walk recipe (grammar-neutral, layout-as-data); but a per-leaf
  RUNTIME walk through the live `StructRegistry` BTreeMap is the refuted indirection — and
  it would ALSO be a grammar-shaped runtime dispatch (re-introducing per-grammar policy in
  the hot path). Compile-time emission keeps both the perf AND the grammar-neutrality.
- **Lock surface.** Lock 14 (the layout walk is grammar-as-data, resolved at codegen).
  Lock 1 (no second substrate; the registry is compile-time, the tape is runtime). Plus
  the Lock-2 `StructLayout`→`Layout` 960-site rename surface (`grep`=960, `LOCKS.md:160`,
  not a 2C-owned candidate — 1E ownership). **Fence (CRITICAL):** any per-leaf
  `StructRegistry::layout`/`Arena<G>`/`Builder<G>` in the runtime hot path is REJECT
  (re-opens the worst measured regression). 0 LOC to catalogue; the fence is load-bearing.

### Candidate SK17-2C-ONBOARD — Future-grammar onboarding test (the Lock-14 generality gate, with live HEAD baseline)

- **Shape.** The fold's grammar-neutrality is verified by a future-grammar onboarding test:
  adding a grammar is a config + grammar-source change with NO code change in any generic
  crate (`LOCKS.md:349` "Adding a new grammar is a config + grammar-source change with NO
  code change in any generic or other-grammar crate"). The falsifier is the per-wave
  generic-crate grammar-name + grammar-shape leak census, with a monotonic-decrease-to-zero
  rule — NOT a present-tense clean gate (the leak surface is non-zero at HEAD and catalogued).
- **Live HEAD baseline (executed at 91b6893b0, reported inline per CH6-V1-V01).** This
  candidate is a **verify_action with a measured HEAD baseline**, not an asserted clean pass:
  - **Predicate 1 — grammar-name leak in generic crates.** `rg 'JsonParser|CssL4Parser'
    crates/ir/src crates/simd-scan/src` = **7 hits, all in `crates/ir/src/registry/strategy.rs`**
    (`:132,:137,:149,:197-198,:292,:315`). These are NOT runtime `match grammar {}` dispatch
    branches — they are the resolver's **string-ident registry** (`idents: &["JsonParser",
    "JsonGrammar"]`, `&["CssL4Parser"]`) plus doc-comment examples: grammar-name DATA a generic
    resolver maps, exactly the catalogued "Generic-Crate Grammar-Name Leak Surface (ARCH-3A-D09)"
    at `ARCH:1289-1296` (leak class (c) parser-name leak sites). The Lock 14 v+1 disposition is a
    published baseline + monotonic-decrease rule (HEAD → 0), not a present clean state.
  - **Predicate 2 — per-grammar dirs in the GENERIC runtime root.** `find
    crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` = **9 grammar dirs**
    (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) — **all carry
    `// @generated by xtask regen-<grammar>`** (verified head-of-`mod.rs` at HEAD `91b6893b0`:
    `regen-bbnf`/`regen-css-pretty`/`regen-google-sheets` confirmed verbatim, Lock-14 ALLOWED) —
    plus `tape/` (the shared substrate, `//! Shared flat-tape runtime substrate.`, grammar-neutral).
    ZERO hand-written per-grammar dirs in the generic root; the per-grammar locus is generated output.
    (The 9-dir count is ground-truth-exact at HEAD; the V2 prose's "8" recap nit, CH6-V2 §161-162,
    is discharged — the inline enumeration was always 9; the leading numeral now matches.)
- **T-P1-divergence antecedent.** The Lock 14 v+1 per-wave gate (`LOCKS.md:374-387`); the
  by-construction-not-by-exercise split (1D SK17L-009, 1F COH17-008): generality is
  breadth-of-config, proven by-exercise on JSON+CSS only.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL GATE (a verify_action, not a fold). The
  tape/`ValueRef<G>`/classifier fold is grammar-neutral by construction; the onboarding
  census is the falsifier. Predicate 2 is clean (all per-grammar runtime dirs `@generated`).
  Predicate 1 is NON-ZERO at HEAD (7 string-ident/doc sites in one resolver file) — the
  catalogued ARCH-3A-D09 leak surface under a monotonic-decrease-to-zero Lock-14 v+1 rule, not
  a clean pass. A fold that requires a generic-crate *runtime dispatch* grammar branch to
  onboard a new grammar FAILS Lock 14 regardless of its perf merits.
- **Lock surface.** Lock 14 (the per-wave grammar-name + grammar-shape leak census; live HEAD
  baseline above). **Fence:** with only one of Sheets or BBNF-self witnessed, the claim is
  SCOPED to the witnessed grammars and may not use fleet-wide grammar-neutral wording
  (`LOCKS.md:382-387`).

## Architectural Assertions Defended

1. **The tape substrate is grammar-neutral by type-parameter, not by runtime branch.**
   `ValueRef<…,G:EventGrammar>` (`tape/mod.rs:175`) carries the grammar in `G`,
   monomorphised at codegen. This is stronger than simdjson's fixed-JSON On-Demand:
   the grammar generalises through the type system, zero runtime `match grammar`.
   Grounded against Lock 14 (`LOCKS.md:349`) + the live skinny signature.

2. **The eager `OpenFrame` retirement is a Lock 14 obligation.** Lock 14 NAMES the
   "CSS L4 14-variant `OpenFrame`" as the overfit it prevents (`LOCKS.md:349`). The
   fold's deletion of `css_l4/builder.rs` (817 LOC, 7 pending fields) is the lock's
   purpose discharged, not a side effect — the grammar-specific builder state is the
   overfit. Grounded against the lock body + the live builder (`css_l4/builder.rs:71-79`).

3. **The tape is a substrate-manifest category, not a 6th BackendShape.** The 5-shape
   canon is monotonically fixed (`LOCKS.md:107-108`); the `FactStream` precedent shows a
   substrate/output category is NOT a shape peer — the live row sits at output-plane-table
   ordinal **(3)** "Fact-stream output" (`ARCH:1803`) while declaring itself the 5th
   substrate-manifest category on that same row (distinct ordinals, neither a shape). The
   OffsetTape/EventTape shapes PROJECT the tape; the tape is their shared grammar-neutral
   substrate. Independently corroborated by `ARCH:1206`: the lone AVX-512-FSM shape
   `CollapsedStage` is x86-only with "aarch64 mechanically refused", so the aarch64 target
   needs no 6th-shape route.

4. **The shared classifier scan-leaf is already wired across 8 grammars (config-breadth) —
   the fold is the spec narrative, not the code.** `scan_structural(input,&StructuralAlphabet)`
   is wired (scan-leaf) across 8 core grammars (1B BSHAPE17-009); the alphabet is grammar-mined
   data. This is config-breadth, value-plane-exercised on JSON+CSS only (not a fleet-wide
   value-fold proof). CSS exercises the eq-set-fan NEON body (the slot-59 route, `SPEC.md:316-317`),
   so the Lock-16 "≥1 non-JSON consumer" binds to a real NEON body, not the scalar-delegate
   table entry. The fold updates the V1 spec's JSON-scanner framing to the alphabet-parametrised
   shared form — a 0-LOC narrative fold, with a Lock-16 manifest row (the eq-set fan) for admission.

## Architectural Assertions Refuted (the load-bearing rows)

1. **REFUTED — tape as a 6th BackendShape.** The flat tape is the substrate the existing
   shapes project, not a shape peer. A 6th shape would be a silent new directive (G-Omega
   gated, §3W). Per LAC-1E-14, propose a substrate-manifest category under the 5-shape
   canon — the live `FactStream` precedent sits at output-plane-table ordinal (3) yet is the
   5th substrate-manifest category, neither a shape (`ARCH:1803`). Corroborated by the
   x86-only/aarch64-refused `CollapsedStage` row (`ARCH:1206`). (`LOCKS.md:107-108`; SK-V17 §9
   `SPEC.md:808`.)

2. **REFUTED — per-leaf `FieldSource`/`StructRegistry` runtime walk as the projection
   mechanism.** This IS the refuted indirection (28-65×/983×/10583×, `SPEC.md:793-795`)
   AND a grammar-shaped runtime dispatch. The `FieldSource` walk is grammar-neutral ONLY
   as compile-time projection-emission resolved once. (`struct.rs:84,313,331`;
   `begin_compound` `tape/mod.rs:185-186`.)

3. **REFUTED — fleet-wide grammar-neutral CLAIM on JSON+CSS exercise alone.** `sheets_witness`
   is a 24-LOC `EventGrammar` stub with no `.bbnf`/`BackendRule` (1D SK17L-009); it cannot
   exercise the projection. Grammar-neutrality is breadth-of-config, proven by-exercise on
   JSON+CSS only; the fleet-wide wording is SK-V18's proof. Lock 14 v+1 scopes the claim
   (`LOCKS.md:382-387`).

4. **REFUTED — eager value tree carried forward.** The eager `CssTypedValue` + `pending_*`
   builders ARE the AZ-IV 118× eager-tree shape (`SPEC.md:791`) AND a Lock-14 overfit;
   the fold REPLACES them with lazy `ValueRef<G>`, never carries them. No-delete-before-
   same-wave-replacement. (`css_l4/builder.rs:71-79`, `value.rs:414`.)

## Open Research Questions

| UNKNOWN | blocking question | verify_action |
|---|---|---|
| OQ-SK17-2C-01 | Does the surviving one-encoding tape (Candidate A) remain grammar-column-free under the cost-model's per-rule shape selection, or does a high-cardinality grammar pressure a kind-partitioned column (the barred AV.04 shape)? | T-P3/SK-V18: confirm the surviving encoding's flags stay sparse position-keyed side-vectors (`tape/mod.rs:96-98`), never a dense per-grammar class column (`LOCKS.md:75,:784`); the cost model selects the shape, not the substrate layout. |
| OQ-SK17-2C-02 | Can the `ValueRef<G>` generator (Candidate C) re-emit ALL 8 per-grammar value surfaces from the single `BackendRule`-walk, or do CSS-specific value semantics (colour functions, calc) require a grammar branch in the generator? | T-P3: confirm CSS per-grammar deviations (`LOCKS.md:349` "CSS L4 colour-function emit") are encoded in grammar metadata + source, NOT a generator branch; re-emit JSON byte-equal as the CH2 firewall (`SPEC.md:62,557`). |
| OQ-SK17-2C-03 | Is the substrate-manifest category for the tape (Candidate D) expressible without a new manifest field, or does it require a `substrate_target` value the V1 manifest lacks? | T-P3: map the tape against the four admitted `substrate_target` values (`LOCKS.md:118-127`); confirm OffsetTape/EventTape already cover it; no new manifest field = no silent directive. |

## LOCKS-AMENDMENTS-CANDIDATE

Candidates only; disposition is T-P3 3C (G3-gated); merge is Pass Omega
(post-G-Omega). Two candidates this cycle. Scanned axes: Lock 14 (grammar-neutral
substrate / type-param value plane / `@generated` allowance / future-grammar
onboarding), Lock 10 (5-shape canon / tape-as-substrate-category), Lock 16
(`select_classifier` manifest row). No candidate raised against Lock 1 (1A/1C/1E own
the one-encoding closure) or Lock 2 (1E owns the `StructLayout` rename).

| candidate | type | target locks | proposed candidate text | supporting path:line evidence | loc/risk/wave_hint |
|---|---|---|---|---|---|
| LAC-SK17-2C-01 | refinement | L10, L1 | Catalogue the flat tape as a **substrate-manifest category** under the fixed 5-shape `BackendShape` canon, NOT a 6th shape: the OffsetTape/EventTape shapes PROJECT the tape; the tape is their shared grammar-neutral substrate. Per the LAC-1E-14 / `FactStream` precedent — the live row sits at output-plane-table ordinal (3) "Fact-stream output" yet is the 5th substrate-manifest category, distinct ordinals, neither a shape — a substrate/output category is not a shape peer. Independently corroborated by `ARCH:1206`: the lone AVX-512-FSM `CollapsedStage` is x86-only ("aarch64 mechanically refused"), so the aarch64 target needs no 6th-shape route. Any proposal that reads as a 6th `BackendShape` is REJECT (silent new directive, G-Omega gated). | `LOCKS.md:100-108` (FactStream = 5th substrate-manifest category, 5-shape canon held); `ARCH:1803` (FactStream live row: plane-ordinal (3) + 5th substrate category, not a shape); `ARCH:1206` (CollapsedStage x86-only, aarch64 mechanically refused); `restart/skinny/tranches/sk-v17/SPEC.md:808` (SK-V17 §9 bars a 6th shape); 1B U-BSHAPE17-001 | 0 LOC (canon holds; categorisation only) / low / T-P3 disposition |
| LAC-SK17-2C-02 | addition | L16, L14 | Record the shared aarch64-NEON eq-set fan `byte_class_from_eq_set_64` (the one proven NEON Layer-1 body) as a Lock-16 primitive-manifest row with abstract-primitive name (`byte_class_from_eq_set_64` + `byte_class_from_range_64` sibling), aarch64 hardware gate, scalar reference (`scalar/byte_class_from_eq_set_64.rs:26`), strict checkasm parity command, the alphabet as grammar-policy-source DATA, and **CSS L4 as the named non-JSON same-wave consumer that binds to this NEON body** — CSS's `;{` pair collides on the `& 0x3f` slot-59 table slot, so it routes to the eq-set fan, NOT the lo6 table (`SPEC.md:316-317`; `dispatch.rs:101-113`). The aarch64 `byte_class_from_table_64` entry is recorded as a scalar-delegate-non-ASM close-state (2-LOC passthrough, `aarch64/byte_class_from_table_64.rs:2-3`), not a NEON-body manifest row. No retained cross-call classifier state; x86/AVX/SVE barred. | `LOCKS.md:332-339` (range-class sibling), `:423-425` (non-JSON consumer requirement), `:495-505` (manifest-row fields); `restart/skinny/tranches/sk-v17/SPEC.md:316-317` (eq-set fan, slot-59 collision); `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33` (real NEON body); `skinny/crates/bbnf-simd/src/dispatch.rs:42,101-113`; `crates/simd-scan/src/lib.rs:80` | 0-LOC fold (eq-set fan already general) + manifest row / low / T-P2→T-P3 |

## CH-lens self-anchors (pre-empting the §3 wave)

- **CH1 (correctness):** every citation resolves at file:line; the SHA is the T-P1
  source `445925167` for live-impl anchors, master HEAD `91b6893b0` for the fold target.
  No confabulated citation; the four refutations match the literature's actual position
  (Lock 14 names `OpenFrame`; SK-V17 §9 bars a 6th shape; AZ-IV 118× measured). **CH1-2C-01
  folded:** the `ARCH:1803` ordinal is disambiguated — the live `FactStream` row sits at
  output-plane-table ordinal (3) AND declares itself the 5th substrate-manifest category on
  that same row; the prose no longer conflates plane-ordinal (3) with substrate-category 5th.
- **CH2 (generality):** every fold is grounded grammar-neutrally; the by-construction-not-
  by-exercise split is explicit (JSON+CSS proven, Sheets/BBNF-self SK-V18); the fleet-wide
  claim is REFUTED, not asserted. **CH2-V1-R4 folded:** the Candidate-E / LAC-SK17-2C-02
  CSS non-JSON consumer is bound to the eq-set fan (`byte_class_from_eq_set_64_neon`, the
  one real NEON body) via the `;{` slot-59 collision (`SPEC:316-317`), NOT the lo6 table
  (a scalar delegate); the classifier scan-leaf is scoped as config-breadth wired across 8
  grammars, value-plane-exercised JSON+CSS only.
- **CH3 (regression):** no fold re-opens a REDRESS route — AZ-IV eager (Candidate B replaces,
  never carries), StructRegistry indirection (Candidate F fences), fact-stream (not a 2C
  surface), x86 (barred). D6 second-substrate inversion guarded (monotonic skinny→core).
- **CH4 (cost):** each candidate carries LOC/risk + the Lock-16 admission cost (scalar-ref +
  checkasm for E); same-wave consumers named (Candidate B's replacement is the tape consumer;
  E's is CSS L4); no orphan-kernel research.
- **CH5 (hidden coupling):** no fold implies a parallel substrate, a sidecar, or a Lock-1
  violation; the classifier (E) keeps the alphabet per-call (no retained state); the tape (D)
  is a substrate category, not a retained sidecar.
- **CH6 (anti-paper-close):** no fold claims "validated/proven" on citation density; every
  grounded fold states the bbnf-specific grammar-neutral transfer reason (type-param carriage,
  alphabet-as-data, compile-time `FieldSource` walk); no deferral to "a later research pass"
  beyond the explicitly-scoped SK-V18 by-exercise proof. **CH6-V1-V01 folded:** the ONBOARD
  candidate is reclassified as a verify_action with the two predicates EXECUTED at HEAD
  `91b6893b0` and the live results reported inline (Predicate 1 = 7 string-ident/doc sites
  in `strategy.rs` = catalogued ARCH-3A-D09 leak surface under monotonic-decrease, not a
  clean pass; Predicate 2 = 9 `@generated` grammar dirs + `tape/` substrate, clean) — no
  asserted falsifier without an attached result. **CH6-V2 §161-162 count nit folded (V3):** the
  Predicate-2 leading numeral now reads 9 (ground-truth-exact at HEAD), matching its inline
  enumeration. **CH6-V1-V03 folded:** the D-fold carries
  an independent corroborating anchor (`ARCH:1206` CollapsedStage x86-only, aarch64
  mechanically refused) beyond the shared LAC-1E-14 citation.
