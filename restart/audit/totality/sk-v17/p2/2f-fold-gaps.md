---
agent: 2F
pass: T-P2-research
cycle: V3
generated_at: 2026-05-29T00:00:00Z
subject: greater-spec fold of the SKINNY-proven tape/NEON/projection model into the V1 totality spec
t_p1_inventories_consumed: [1a, 1b, 1c, 1d, 1e, 1f]
master_head: 91b6893b0b61d1c3213d02afe4ec62f22c16ae38
t_p1_locked_commit: 91b6893b0
t_p1_excavation_sha: 445925167154de73540e3ea3283d0170371de790
primary_sources_cited: 14            # V1-spec lock surfaces + ARCH §7.3 + SK-V17 SPEC + live crates/core + skinny source
techniques_grounded: 9               # one per fold candidate F1..F9
techniques_refuted: 2                # F4d 6th-BackendShape, F6b per-leaf-registry-walk projection
prior_cycle_dispositions_folded:
  accepted: [F1, F2, F3, F4-tape-category, F6-fence, F8]
  rejected: []
  revised: [CH1-2F-01-RESIDUAL]
  first_cycle_additions: []
v3_fold_log:
  CH1-2F-01-RESIDUAL: "LAC-2F-FOLD-03 (2f:580) re-anchored alphabet.rs:118 -> alphabet.rs:19-37 for the StructuralAlphabet manifest grounding (struct + rich-alphabet definition: singletons/digraph_mask/digraph_pairs/quote_classes), the anchor 2A/LAC-2A-SKV17-02 uses; :118 is the orthogonal KernelShape::select(alphabet) site and stays correct in the F5 body (:290). Verified live at HEAD 91b6893b0: :19-37 = StructuralAlphabet struct, :118 = KernelShape::select. Anchor-precision only; claim true and grounded elsewhere; zero orphan."
v2_fold_log:
  CH1-2F-01: "F5 alphabet-as-data re-anchored LOCKS.md:315-317/:312-314 -> SK-V17 SPEC.md:314-317/:312 (verbatim, verified); wrong-document defect, claim true"
  CH2-V1-R6: "Defended-#3 + F5 aligned: 8-of-9 = scan-leaf config-breadth; ValueRef<G> value-plane fold (F2) JSON+CSS-exercised only; not fleet-wide proof (LOCKS.md:382-387,:423-425)"
  CH4-2f-001: "F7 same-wave consumer named as co-waved F1/F3 tape-wiring (the classification gates the wiring it is co-waved with); not orphan pre-gate"
  CH5-V1-003: "F6 (shared-owner) names live coupling-site crates/core/.../bbnf/arena.rs:47 StructRegistry::compound_kind_for_layout(layout); FOLD-B severs it; contrast fence-clean begin_compound (tape/mod.rs:185-186, grep-zero StructRegistry)"
  CH5-V1-001: "Defended-#2 declares PayloadArena substrate_target=existing_tape (member of one tape, mod.rs:99) / retention_lifetime=output_row / policy_owner=generated_grammar; closes implicit-retention hole"
  CH6-V1-V02: "F9 path-(b) UNKNOWN -> bounded sizing route: 0->N introduce-site delta (StructLayout 960 rename surface vs backend_shape/LayoutFacts 0 side-table surface)"
  CH6-V1-V03: "F4 D-fold carries independent corroborating anchor beyond LAC-1E-14: admits_collapsed_stage x86-bound (ARCH:1151/:1282) mechanically refuses on aarch64 -> no 6th-shape mechanism; verdict stands on TWO grounds"
hygiene_ch1_v5_001: folded-confirmed-resolved   # 1b:12 + 1b:97 enumerated form present on disk (collapsed_stage}.rs ×3, zero ,collapsed} residual)
locks_amendment_candidates: 5        # LAC-2F-FOLD-01..05
---

# T-P2 2F — Greater-Spec Fold-Gaps Dossier (SKINNY tape/NEON/projection → V1 totality spec)

## Hygiene — CH1-V5-001 folded (first action, per dispatch)

The dispatch mandates folding **CH1-V5-001** (the enumerated-filename residual)
on first touch of 1a/1e. On-disk verification at master `91b6893b0`:
`1b-codegen-evidence.md:12` (`live_truth_method`) and `:97` (BSHAPE17-004 row)
**already carry the enumerated executing form** `wc -l
skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs
= 17 each, sink_only.rs = 270` — the exact concrete fix the T-P1 CONVERGED
verdict prescribed (`HARDENING-T-P1-SKV17-CONVERGED.md:254`). `grep -c
'collapsed_stage}.rs' 1b = 3`; `grep ',collapsed}' {1a,1b,1e} = 0` (exit 1, no
match). The brace-glob never expands to the non-existent `collapsed_tape.rs`;
the command executes clean. **CH1-V5-001 is RESOLVED on disk, not carried.** No
1a/1e edit is required (the defect lived only in 1b, and 1b is already correct);
this dossier records the resolution rather than re-applying it. Residual REVISE
count carried into T-P2: **zero**.

## Executive Summary (≤200 words)

The T-P1 excavation isolates **six** load-bearing fold divergences (A tape
AoS↔SoA, B eager OpenFrame, C per-grammar-eager value-API, D BackendShape canon
vs tape-substrate, E NEON JSON-only-narrative vs shared classifier, F
StructRegistry/FieldSource fence). 2F designs the **greater-spec fold**: how the
V1 totality spec (`ARCHITECTURE.md`, `LOCKS.md`, `MASTER-PLAN.md`) must direct
the SK-V18 adoption of the SKINNY-proven flat-tape / lazy-`ValueRef<G>` /
`StructLayout`-projection + shared NEON `select_classifier(alphabet)` model. The
fold is **nine candidates** (F1–F9), each grounded grammar-neutral (Lock 14),
substrate-union-preserving (Lock 1), 5-shape-canon-coherent (Lock 10), and
NEON-scalar-ref+checkasm (Lock 16). The **load-bearing refutation**: the tape is
NOT a 6th `BackendShape` — the **LAC-1E-14 FactStream precedent** (`LOCKS.md:100`)
already established the substrate-manifest *category* as the canonical place for
"tape-as-substrate," and ARCH §7.3 already frames the 5 shapes as *projections of
the tape substrate* (`ARCHITECTURE.md:1088`). The tape folds in as the
**substrate the 5 shapes project from**, not a sixth projection. The second
refutation: the `FieldSource`/`StructLayout` walk must stay compile-time
projection-emission; a per-leaf registry walk re-opens the 28-65×/983×/10583×
regression (SPEC `:793-795`). Five LACs carry the fold to T-P3.

## Technique Grounding Table

| Fold claim / T-P1 divergence | V1-spec / SKINNY source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Flat tape is the V1 substrate; eager OpenFrame retires (A+B) | `LOCKS.md:75` (one substrate); `skinny/.../tape/mod.rs:94`; `crates/core/.../css_l4/builder.rs:16` (817 LOC) | grounded | the SK-V18 implementation the spec must direct; the eager builder IS the AZ-IV K-block shape (SPEC `:791`) |
| Lazy `ValueRef<G>` is the unified materialization plane (C) | `skinny/.../tape/mod.rs:175`; `grammars/json/value.rs:143`; ARCH `:1088` union | grounded | preserve-rich-ast (SPEC `:252`); one grammar-parametric projection re-emits all 8 per-grammar surfaces |
| Tape is a substrate-manifest CATEGORY, NOT a 6th BackendShape (D) | `LOCKS.md:100-116` (LAC-1E-14 FactStream precedent); `ARCHITECTURE.md:1088` (5 shapes ARE tape projections) | grounded | the precedent the dispatch names; "propose, do NOT silently add a 6th" satisfied by the category form |
| A 6th `BackendShape` variant for the tape | `LOCKS.md:107-109` (5-shape domain held); SPEC `:808` (sixth-shape pre-block) | **refuted** | G-Omega-gated per Lock 10 v+1; the fold needs no 6th shape — the tape is the substrate under the 4 LLVM shapes |
| Shared NEON `select_classifier(alphabet)` as Lock-16 primitive-manifest entry (E) | `LOCKS.md:453-489` (manifest); `skinny/.../dispatch.rs:42`; `crates/simd-scan/src/lib.rs:80` | grounded | already grammar-general (alphabet-as-data); 0-LOC narrative fold + Lock-16 manifest row |
| `FieldSource`/`StructLayout` walk is COMPILE-TIME projection-emission (F) | SPEC `:793-795`; `crates/core/.../tape/mod.rs:185-186`; `struct.rs:84,313,331` | grounded | `begin_compound` reads `layout.rule_id & 0x1F` only; no per-leaf registry lookup |
| Per-leaf `StructRegistry::layout(rule)` walk in the projection hot path | SPEC `:793-795` (28-65×/983×/10583×) | **refuted** | re-opens the worst measured regression; the AZ-IV-IV indirection pre-block (`1e` D-1E-SKV17-03) |
| AoS `TapeRec` ↔ SoA `Tape` converge to exactly ONE encoding post-fold (A) | `LOCKS.md:75` ("parallel substrates are dead"); `record.rs:103`; `tape/mod.rs:94` | grounded | both Lock-1-admitted *transient* fold-states; dual end-state = Lock-1 violation |
| aarch64 CollapsedStage is the spec-named UNKNOWN-2D-05, not a fresh gap (D) | `ARCHITECTURE.md:1282` (`admits_collapsed_stage` predicate per LAC-2D-06) + `:1151` (x86-binding); `LOCKS.md:520-533`; SPEC `:854` (D6 second-substrate REJECT) | grounded | NEON sits under the 4 LLVM shapes' scan-leaf FFI; `admits_collapsed_stage` is x86-bound (ARCH:1151) so mechanically refuses on aarch64 — independent corroboration of no-6th-shape beyond LAC-1E-14 (CH6-V1-V03) |

## The fold model — what the spec already says, what the fold adds

The V1 spec is **closer to the SKINNY model than the eager `crates/core` impl
is**. Two surfaces are already correct in the spec and only need the *impl* to
fold to them; two are the genuine spec-direction gaps the fold must write.

1. **ARCH §7.3 already frames the 5 shapes as projections of the tape**
   (`ARCHITECTURE.md:1088`): *"the five `BackendShape` variants below are the
   five ways the substrate may project for a given rule."* The tape IS the
   substrate; `EagerTape`/`OffsetTape`/`EventTape` retain a queryable document,
   `SinkOnly` does not. This is the SKINNY model's exact shape — the spec does
   not need a new substrate concept, it needs the *impl* to stop carrying the
   eager OpenFrame as a parallel substrate beside the dormant tape.
2. **LAC-1E-14 already established the substrate-manifest category**
   (`LOCKS.md:100-116`): `FactStream` is the 5th *category* at the Lock 1
   substrate manifest, "a substrate-manifest classification only; it is NOT a
   6th `BackendShape` variant." This is the **precedent the fold reuses verbatim**
   for "tape-as-substrate": the tape is named at the substrate manifest
   (`substrate_target ∈ {local_temp_only, existing_tape, direct_sink,
   admitted_fact_output}`, `LOCKS.md:119-127`), not as a 6th shape.
3. **The genuine spec-direction gap (B+C)**: the V1 spec does not yet *direct*
   the eager-OpenFrame-deletion / lazy-`ValueRef<G>` adoption as the SK-V18
   monotonic move. `MASTER-PLAN.md` §H/W1 and ARCH §7.3 describe the *target*
   shapes but not the *retirement of the live eager builders* as the fold step.
4. **The genuine spec-direction gap (E)**: the NEON narrative in ARCH/MASTER
   still frames the classifier JSON-first; the impl proves it grammar-general
   across 8 grammars. The fold rewrites the narrative to the
   alphabet-parametrised shared form and adds the Lock-16 manifest row.

## §2 Fold candidate / fold enumeration (LOAD-BEARING)

Each candidate carries: **shape** (the concrete spec-fold) · **T-P1-divergence
antecedent** (the locked excavation row it discharges) · **grammar-neutral
verdict** (Lock 14) · **lock surface** (the spec/lock text the fold writes or the
fence it preserves). All anchored at file:line / SHA. crates/core is the fold
TARGET; skinny is the proven engine; the monotonic direction is skinny→core.

---

### F1 — Eager OpenFrame retirement → flat-tape commit-by-construction (Divergence B)

**Shape.** The V1 spec must DIRECT the SK-V18 deletion of the live eager
`OpenFrame` builders — `CssStructBuilder` (`crates/core/src/runtime/css_l4/builder.rs:16`,
**817 LOC**, 6 `pending_*` Vecs `:74-79` + 1 `pending_value: Option<…>` `:71`)
and `JsonStructBuilder` (`crates/core/src/runtime/json/builder.rs:9`, 231 LOC) —
replacing them with the SKINNY-proven flat-tape commit-by-construction
(`push_plain_offset` = one branchless u32 write into `offsets`, SK-V17 SPEC
`:446`). The spec text the fold writes: a `MASTER-PLAN.md` §H fold-directive row
that names `OpenFrame` builder retirement as the SK-V18 first move, with the
`grep -rln JsonStructBuilder|CssStructBuilder` 22+-file revert slice (`1a:126`).

**T-P1-divergence antecedent.** Divergence **B** (`CONVERGED §B`,
`1a` SUB17-003, `1e` D-1E-SKV17-02 + CH3-ledger AZ-IV row): the 817-LOC CSS +
231-LOC JSON eager builders are *the live substrate*; the tape is UNWIRED
(`TapeStructBuilder` grep-zero outside `runtime/tape/`). This is the AZ-IV
eager-value-tree pre-block (118× regression, SPEC `:791`) sitting live in core.

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. The eager
builders are *per-grammar runtime surfaces* (Lock-14 ALLOWED today,
`1e` L14 row), but they are the **fold-deletion target**, not a leak. The flat
tape that replaces them is grammar-as-data (`begin_compound(&StructLayout)`
dispatches on the layout, never on per-grammar route strings,
`tape/mod.rs:54-56`). The fold REMOVES per-grammar materialization machinery;
it does not add grammar branches to a generic crate.

**Lock surface.** Lock 1 (`LOCKS.md:75`, no `Vec<OpenFrame>::clone` parallel
substrate — the 86.07% samply pathology); the AZ-IV K-block (SPEC `:791`); SPEC
§9 W1 row (`:824`, "AZ-IV eager") inviolate. **Risk: high; ~300-700 LOC
generator-side + per-grammar regen across 8 grammars; 22+ files.**

---

### F2 — Lazy `ValueRef<G>` as the unified materialization plane (Divergence C)

**Shape.** The spec must name the **one grammar-parametric `ValueRef<'doc,'input,K,G>`
projection** (`skinny/crates/runtime/src/tape/mod.rs:175`) as the V1
materialization plane that re-emits all 8 per-grammar value surfaces, replacing
the per-grammar eager typed enums (`CssTypedValue<'p>`,
`crates/core/src/runtime/css_l4/value.rs:414`; no `ValueRef`/`value_from_ref` in
core, grep = 0). The accessor generator walks the `BackendRule`/`FieldSource`
shape and emits `document/value/view/visitor` per grammar isomorphic to JSON's
`value_from_ref` (`skinny/.../grammars/json/value.rs:143`), over the EXISTING
tape — NO new cursor/builder type (SK-V17 W2 task, SPEC `:532-537`).

**T-P1-divergence antecedent.** Divergence **C** (`CONVERGED §C`, `1a`
SUB17-004, `1e` D-1E-SKV17-02). The divergence is the **EAGER materialization
shape, not the `@generated` provenance** — the value modules are `@generated by
xtask regen-{json,css}` (`value.rs:1`), which Lock 14 ALLOWS; hand-authoring
would be the Lock-14 VIOLATION (the V5 fold corrected this mischaracterisation,
`1a` V5 fold). The fold acts on the GENERATOR to emit a lazy projection.

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. ONE
grammar-agnostic accessor generator consumes (grammar source + workspace
metadata) and produces typed Rust per Lock 14 (`LOCKS.md:349` —
"per-grammar runtime modules … emitted from a single grammar-agnostic generator
template"). The generic `runtime/{tape,view.rs}` carry NO grammar names (`1a`
SUB17-004); per-grammar `JsonChildrenIter`/`CssChildrenIter` are generated
surfaces (generated-output allowance, `LOCKS.md:351-358`). **Load-bearing W2
gate**: the JSON `value_from_ref` rider must re-emit BYTE-EQUAL through the new
generator (SPEC `:550-557`) — a CSS-only generator that never re-emits JSON
FAILS (the generic-named-CSS-generator CH2 failure mode).

**Lock surface.** Lock 14 (`LOCKS.md:349`, single grammar-agnostic generator);
preserve-rich-ast (SPEC `:252`, `css_rich_ast_preserved`); the AZ-IV K-block
(no per-leaf `Box::new`, SPEC `:792`). **Risk: high; generator-LOC (one accessor
generator) distinguished from regen-LOC (per-grammar value.rs/view.rs/document.rs
× 8); ~300-700 LOC.**

---

### F3 — AoS `TapeRec` ↔ SoA `Tape` exactly-one-encoding closure (Divergence A)

**Shape.** The spec must declare the **post-fold convergence-target tape
encoding** as an EXPLICIT divergence, not an open question (resolving
`1a` U-SUB17-001 / `1e` 1E-SKV17-U1). Core's 16-byte AoS `TapeRec`
(`crates/core/src/runtime/tape/record.rs:103`, const-asserted `size_of==16` +
`align_of==4` `:120-121`) and skinny's SoA `Tape<'input>` (6 members,
`offsets: Vec<u32>` + sparse `flag_cursors`/`flag_values` + `PayloadArena`,
`skinny/.../tape/mod.rs:94-100`) are BOTH Lock-1-admitted offset tapes — but
post-fold **exactly ONE encoding survives across both trees** (`LOCKS.md:75`
"parallel substrates are dead"). The core mod-doc admits the AoS shape is
provisional: "kept AoS first … the same `TapeCursor` API rides a later SoA
split" (`crates/core/src/runtime/tape/mod.rs:6-9`). **2F recommends the fold
direction name the SoA `Tape` as the proven convergence anchor** (it is the
benched-and-proven encoding; core's AoS is the unwired adoption stub), with the
AoS→SoA transition as the SK-V18 fold-state, NOT a dual end-state.

**T-P1-divergence antecedent.** Divergence **A** (`CONVERGED §A`,
`1a` SUB17-002, `1e` D-1E-SKV17-01). The Lock-1 closure obligation is a
catalogued invariant T-P2 must discharge (`CONVERGED §A` Lock-1-closure row).

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. Both encodings
are grammar-as-data byte-keyed tapes (no grammar branches); the encoding choice
(AoS vs SoA) is a layout decision orthogonal to grammar-neutrality. SoA's sparse
`flag_cursors`/`flag_values` are position-keyed side-vectors paid only where
non-zero (`tape/mod.rs:97-98`), each flag bit a `BackendRule` branch-tag
projection (SPEC `:543`) — grammar-neutral by construction.

**Lock surface.** Lock 1 (`LOCKS.md:75`, exactly-one-encoding; dual end-state =
violation); the §9 second-substrate pre-block (`SPEC:807-811`, skinny
`StructLayout`/`TapeStructBuilder`/`TapeCursor` FORBIDDEN-in-skinny — the fold
adopts the PROVEN skinny `Tape`/`ValueRef` INTO core, never relocating core
constructs into skinny, `1e` D6 row). **Risk: medium; 200-600 LOC SK-V18 fold.**
→ **LAC-2F-FOLD-01.**

---

### F4 — Tape as substrate-manifest CATEGORY, not a 6th BackendShape (Divergence D, primary)

**Shape.** The spec must place the tape at the **Lock 1 substrate manifest** as
the substrate the 5 shapes project from — NOT as a 6th `BackendShape` variant.
This is the **LAC-1E-14 FactStream precedent applied verbatim** (`LOCKS.md:100-116`):
`FactStream` is "the 5th admitted-product *category* at the Lock 1 substrate
manifest … a substrate-manifest classification only; it is NOT a 6th
`BackendShape` variant. The 5-shape `BackendShape` search domain at Lock 10
holds: `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`." The fold
adds NO new variant: ARCH §7.3 already states the 5 shapes ARE the tape's
projections (`ARCHITECTURE.md:1088`, "the five ways the substrate may project").
The tape's adoption is recorded at the substrate-target manifest
(`substrate_target = existing_tape`, `LOCKS.md:119-127`), the categorical home
the precedent already carved.

**T-P1-divergence antecedent.** Divergence **D** (`CONVERGED §D`, `1a`
SUB17-005, `1e` D-1E-SKV17-04 / LAC-1E-SKV17-05). The dispatch's exact
instruction: *"propose, do NOT silently add a 6th."* This candidate proposes the
**category form** (substrate-manifest), refuting the 6th-shape form.

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. The
substrate-manifest category is grammar-blind — every e-graph candidate / backend
rewrite / SIMD consumer declares `substrate_target` regardless of grammar
(`LOCKS.md:118-127`). No grammar name enters the categorisation.

**Independent corroborating anchor (CH6-V1-V03 fold).** Beyond the LAC-1E-14
FactStream precedent (which 2A/2C/2D/2E/2F all cite for the same verdict — a
single load-bearing citation must not stand 1×5), this D-fold carries an
*independent* mechanical corroboration that no 6th-shape route is needed on
aarch64: the `admits_collapsed_stage` predicate binds the ONLY shape with an
arch-specific close path (`CollapsedStage`) to **`target.arch == x86` +
`target.avx512bw`** (`ARCHITECTURE.md:1151` LAC-2D-06 binding, restated `:1282`).
On aarch64 that predicate **mechanically refuses** — so the 5-shape canon's
single arch-gated variant is unreachable on the M5 Max target, leaving no
mechanism by which a 6th aarch64 shape could be required. The tape-as-substrate
verdict thus stands on TWO independent grounds: (i) the categorical precedent
(LAC-1E-14), and (ii) the mechanical arch-refusal of the only arch-gated shape
(`admits_collapsed_stage` x86-bound, ARCH:1151/:1282) — not a single citation
echoed five ways.

**Lock surface (REFUTATION row — load-bearing).** A 6th `BackendShape` variant
is **REFUTED**: G-Omega-gated per Lock 10 v+1 (`LOCKS.md:109`) and barred by SPEC
§9 ("sixth `BackendShape`," `:808`). The 5-shape domain holds (`LOCKS.md:107-108`,
`:272-273`). The aarch64 CollapsedStage candidate is the spec-named **UNKNOWN-2D-05**
(`ARCHITECTURE.md:1282`), NOT a fresh gap — NEON sits under the four LLVM shapes'
scan-leaf FFI, no x86 close path, no D6 second substrate (SPEC `:854`). **Risk:
0 LOC (canon holds + precedent reused); medium to write the category-fold prose.**
→ **LAC-2F-FOLD-02.**

---

### F5 — Shared NEON `select_classifier(alphabet)` as a Lock-16 primitive-manifest entry (Divergence E)

**Shape.** The spec narrative (ARCH/MASTER NEON framing) must fold from JSON-first
to the **alphabet-parametrised shared classifier** — already proven
grammar-general. The leaf datum is `alphabet: &'static [u8;64]` via
`select_classifier(alphabet) -> SelectedClassifier`
(`skinny/crates/bbnf-simd/src/dispatch.rs:42`); core's richer
`scan_structural(input, &StructuralAlphabet) -> StructuralIndex`
(`crates/simd-scan/src/lib.rs:80`, `KernelShape::select(alphabet)` `alphabet.rs:118`)
is WIRED across 8 of 9 generated grammars (`math.rs` excepted; its `ScanState`
`:287-289` holds only `nospace_bits`, no `OnceCell<StructuralIndex>` field). The
fold registers a **Lock-16 primitive-manifest row** for the classifier:
scalar-reference (`scalar/byte_class_from_eq_set_64.rs`) + checkasm parity
(`tests/checkasm_*`), `substrate_target = existing_tape`, `retention_lifetime =
transient-single-call`, same-wave consumer = the tape (SPEC `:104`). This is a
**0-LOC narrative fold** (the classifier is built) + the manifest row.

**T-P1-divergence antecedent.** Divergence **E** (`CONVERGED §E`, `1a`
SUB17-007/008, `1e` D-1E-SKV17-06 / LAC-1E-SKV17-06). The classifier ALREADY
exceeds the spec (impl-exceeds-spec): it is alphabet-as-data, NOT JSON-only.

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. The alphabet is
the ONLY grammar datum (SK-V17 `SPEC.md:314-317` verbatim — "The L1 classifier's
only grammar datum is the `alphabet: &[u8;64]` passed to `select_classifier`
(Lock-14 vehicle); the CSS `;{` pair uses the eq-set fan, NOT the lo6 table (the
`& 0x3f` slot-59 collision)"). [CH1-2F-01 fold: the alphabet-as-data datum lives
at SK-V17 `SPEC.md:314-317`/`:312`, NOT `LOCKS.md` — V1 mis-pointed the document;
claim true and now re-anchored to the verbatim source.] The richer
`StructuralAlphabet` (`digraph_mask`, `quote_classes`) is config-breadth,
generated byte-set data + opaque class ordinals — grammar-as-data, not grammar
policy. **Scoping (CH2-V1-R6):** the 8-of-9 wiring below is *scan-leaf wiring*
(config-breadth — the alphabet is the only datum); it is NOT the same as the
`ValueRef<G>` *value-plane fold* (F2), which is exercised JSON+CSS-only. The
classifier's grammar-generality is config-breadth-by-exercise, not a
fleet-wide value-fold proof (`LOCKS.md:382-387`, `:423-425`).

**Lock surface.** Lock 16 (`LOCKS.md:453-489`, primitive manifest +
scalar-ref+checkasm mandate); Lock 1 v+1 ELEVATION (`LOCKS.md:137-149`, no
cross-call retained classifier state — `scan_structural` retains the OUTPUT
index per-parse, NOT classifier carry, `1e` CH5 row); aarch64-only (SPEC `:258`,
`:806`); the multi-arch `crates/simd-scan` scope-reconcile (narrow-to-aarch64 vs
retain x86/avx2/wasm kernels WITHOUT admitting x86 as a close path). **Risk:
low/0-LOC narrative fold + manifest row; 100-400 LOC scope reconcile.**
→ **LAC-2F-FOLD-03.**

---

### F6 — StructRegistry/FieldSource compile-time projection fence (Divergence F, the regression firewall)

**Shape.** The spec must add a **no-per-leaf-registry-lookup fence** to the
substrate manifest: the `FieldSource` projection walk inside the live
`StructRegistry` (`crates/ir/src/registry/struct.rs:84` `FieldSource` enum, `:313`
`StructRegistry`, `:331` `layout(rule_id)`, `:337` `layout_by_name`) is
**compile-time projection-emission, resolved once at codegen** — NOT a runtime
per-leaf indirection. `begin_compound(&StructLayout)` takes a PRE-RESOLVED layout
by reference and reads only `layout.rule_id & 0x1F`
(`crates/core/src/runtime/tape/mod.rs:185-186`) — the fold must preserve this
no-runtime-lookup property. The `ValueRef<G>` projection generator (F2) resolves
the layout ONCE at codegen, never per-leaf.

**T-P1-divergence antecedent.** Divergence **F** (`CONVERGED §F`, `1a`
SUB17-009, `1e` D-1E-SKV17-03 + CH3 do-not-redrive ledger). This is the AZ-IV-IV
StructRegistry-indirection pre-block the dispatch names as "pre-blocked" — the
fence keeping it pre-blocked.

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. `FieldSource`
(`{TypedLeaf, BranchTag, SeqPosition, RepeatElement, RuleReference}`) IS the
`BackendRule`-walk recipe (`1a` Cross-Tree Layout row); resolving it at
compile-time emits the field routing once per grammar from the generator, with
NO grammar branches in any generic crate. The registry is keyed by `RuleId`
(`BTreeMap<RuleId, StructLayout>` `:314`) — grammar-blind by data.

**Live coupling-site (CH5-V1-003 SHARED fold, owned by F6; cross-ref 2A/2C/2D/2E).**
The fence must name the LIVE wire it severs, not only the HYPOTHETICAL per-leaf
`StructRegistry::layout(rule)` (`struct.rs:331`). The present-tense coupling-site
that exists TODAY is **`crates/core/src/runtime/bbnf/arena.rs:47`**:
`match StructRegistry::compound_kind_for_layout(layout) { … }` inside the **eager
arena path** (verified live at master `91b6893b0`). FOLD-B (F1 eager-OpenFrame
retirement) DELETES the eager builders and thereby SEVERS this present-tense
coupling. Contrast with the **fence-clean tape path**: `begin_compound(layout:
&StructLayout)` at `crates/core/src/runtime/tape/mod.rs:185-186` reads only the
pre-resolved layout (`layout.rule_id & 0x1F`, mod-doc `:54-56`) — `grep
StructRegistry skinny/.../tape/mod.rs` = 0 (no registry coupling in the tape
path). The fold's regression firewall is therefore concrete: delete the
arena.rs:47 eager coupling-site, retain the registry-free `begin_compound` tape
projection. [V1 named only the abstract hypothetical; the live wire at arena.rs:47
is now named as the present-tense coupling FOLD-B severs.]

**Lock surface (REFUTATION row — load-bearing).** A per-leaf
`StructRegistry::layout(rule)` runtime walk is **REFUTED**: it re-opens the worst
measured regression (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG
tailwind, SPEC `:793-795`). The fence is **0 LOC but HIGH/regression-class if
violated** (`1a` SUB17-009). Plus the Lock-2 `StructLayout`→canonical-name
960-site rename surface (`grep -rn StructLayout crates/` = 960, `LOCKS.md:160`;
the L02 drift, mis-priced ~8× by V2) is a separable T-P3 reconcile. **Risk:
0-LOC fence; CRITICAL if violated; 960-site rename medium (path-(a) full rename;
path-(b) lock re-scope — but `LayoutFacts` grep-zero in crates/, so path-(b)
is non-zero in core, `1e` LAC-04).** → **LAC-2F-FOLD-04.**

---

### F7 — OnceCell<StructuralIndex> substrate_target classification across all 8 carriers (Divergence A/E sub-fence)

**Shape.** Before any tape wiring, EACH of the 8 generated grammars'
`OnceCell<StructuralIndex>` carriers (json/ebnf/bnf/csv/css_l4/css_pretty/
google_sheets/bbnf; init at `json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`,
`bbnf.rs:4843`) must declare `substrate_target` ∈ `{existing_tape (index IS the
tape), local_temp_only}` per the Lock 1 v+1 manifest (`LOCKS.md:119-127`). The
retained index feeds the eager `OpenFrame` builders today; under SK-V18 it must
become the tape's `offsets` (index IS the tape, `ARCHITECTURE.md:1088`) or
`local_temp_only` — never a retained index parallel to a wired tape.

**T-P1-divergence antecedent.** Divergence **A/E** sub-fence (`1a`
U-SUB17-002 / `1e` 1E-SKV17-U2 / LAC-1E-SKV17-03; COH-014 prior-totality
contradiction caught — the all-8 census, not the 4-grammar undercount).

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. The
classification is per-carrier substrate-target declaration, grammar-blind; the
`StructuralIndex` is generated byte-offset data (the scan output), not grammar
policy. The classification MUST scope to ALL 8 carriers (not a 4-grammar
sample) — the COH-014 false-negative the V3 fold corrected.

**Same-wave consumer (CH4-2f-001 fold).** The classification is NOT an orphan
pre-gate — its same-wave consumer is the **co-waved F1/F3 tape-wiring** itself:
F7's `substrate_target` classification GATES the very wiring it is co-waved with
(F1 eager-OpenFrame retirement + F3 SoA `Tape` convergence). The classification
of each `OnceCell<StructuralIndex>` carrier must resolve to `existing_tape` (index
IS the tape) BEFORE F1/F3 wire the tape, else a retained index runs parallel to
the now-wired tape (REDRESS-53 re-entry). The consumer is concrete and same-wave:
the F1/F3 SK-V18 tape-wiring move under LAC-2F-FOLD-01. [V1 named the consumer as
bare "the tape," reading as an orphan pre-gate; it is the co-waved wiring.]

**Lock surface.** Lock 1 v+1 substrate manifest (`LOCKS.md:118-127`); REDRESS-53
(a retained parallel index collapses into REDRESS-53, SPEC `:577`/`:825`/`:839`);
the §9 W2 condition 1 (L1/L4 index == tape-offsets identity, `:837-839`). **Risk:
0 LOC (classification); HIGH (REDRESS-53 re-entry if mis-declared); SK-V18
pre-gate.** → folds under **LAC-2F-FOLD-01** (one-substrate closure).

---

### F8 — BackendShape selector wiring (skinny `derive_backend_shape` → core; Divergence D)

**Shape.** The `BackendShape` enum + `derive_backend_shape` selector live ONLY
in skinny (`skinny/crates/ir/src/lib.rs:340`, `skinny/crates/passes/src/lib.rs:392,401`;
`grep -rn 'enum BackendShape' crates/` = 0, `grep -rn derive_backend_shape
crates/` = 0). Core carries only the single-variant `EmitStrategy::StructDirect`
(`crates/ir/src/registry/strategy.rs:104,224`; `PRODUCTION_MANIFEST_TABLE` with 9
`ManifestStrategyEntry` rows). The fold WIRES the 5-shape selector into core atop
the `StructDirect` lineage, consuming the already-present `crates/egraph` +
`crates/csp-solver` decision engine (`backend_egraph` 311 LOC + `decision_csp`
273 LOC) — the fold WIRES them, does not build them (`1b` BSHAPE17 rows). The
selector is the published SOTA pipeline class (equality-saturation candidate gen
→ bounded saturation → CSP feasibility → cost extraction, `ARCHITECTURE.md:1118-1176`),
not a fixed P1-P8 cascade.

**T-P1-divergence antecedent.** Divergence **D** (`CONVERGED §D` Shape-enum /
Decision-engine rows; `1b` BSHAPE17-002/004). The 4 skinny lowerers are 17-LOC
scaffolds (`{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` = 17 each,
sink_only.rs = 270 — the CH1-V5-001-folded enumerated form).

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. The selector
derives `backend_shape` per-rule from grammar-derived facts (first-set
disjointness, output mode, transitive `@error(recover)`, `@host fn`
decoded-at-parse, `@layout` scope, target features, `ARCHITECTURE.md:1138-1141`)
— `backend_shape` is a side-table field, NOT a surface annotation (Lock 10,
`LOCKS.md:269`). No grammar author annotates the shape; no grammar name enters
the selector.

**Lock surface.** Lock 10 (`LOCKS.md:269-274`, 5-shape domain + decision-engine
cost-evidence clause); `substrate_target` binding on every `BackendExpr` node
(LAC-2D-06, `LOCKS.md:520-533`); fail-closed on e-graph cap / CSP timeout / stale
cost evidence (`LOCKS.md:290-293`). **Risk: medium; 60-200 LOC selector +
600-1400 LOC joint decision-engine wiring envelope.**

---

### F9 — Lock-2 `StructLayout` canonical-name reconcile (Divergence F sub-surface)

**Shape.** `StructLayout` is Lock-2-RETIRED (canonical name `Layout`/`LayoutFacts`,
`LOCKS.md:160`) yet LIVE across **960 sites** in crates/core + crates/ir
(`pub struct StructLayout` `crates/ir/src/registry/struct.rs:202`; `grep -rn
StructLayout crates/` = 960). The fold must reconcile via two disjoint priced
paths (the V2 1F conflated them at 40-120 LOC, mis-priced ~8×): **(a) full
rename** — migrate `StructLayout`→`Layout` across 960 sites, generator-side,
regenerating 8 parsers + ~16 tests; **(b) lock re-scope to side-table** — but
`LayoutFacts.backend_shape` is **skinny/prior-totality-only** (`grep -rn
LayoutFacts crates/` = 0; `grep -rn backend_shape crates/core crates/ir` = 0;
present only `skinny/crates/passes/src/lib.rs` — `LayoutFacts` struct `:90`,
`backend_shape: HashMap<RuleId, BackendShape>` field `:96` + second decl `:385`),
so path-(b)'s crates/core realisation is NON-ZERO/UNKNOWN, not ~0 LOC.

**path-(b) sizing route (CH6-V1-V02 fold — UNKNOWN → bounded measurable step).**
V1's path-(b) refused the under-pricing but carried no next measurement. The
concrete sizing step: the side-table model imports the skinny `LayoutFacts`/
`backend_shape` carrier (`skinny/crates/passes/src/lib.rs:90,:96`) into crates/core,
which today has ZERO such consumer sites (`grep -rn backend_shape crates/core
crates/ir` = 0 at master `91b6893b0`). path-(b)'s realisation LOC is therefore
bounded by the count of `BackendShape`/`LayoutFacts` consumer sites that the
skinny side-table model REQUIRES introducing into crates/core to make the
`backend_shape` side-table queryable at the same points the rename's 960
`StructLayout` sites are read. **verify_action:** T-P3 establishes the baseline
`grep -rcn 'StructLayout' crates/` (=960, the rename surface) against `grep -rcn
'backend_shape\|LayoutFacts' crates/` (=0, the side-table surface), then sizes
path-(b) as the *delta* — the introduce-cost of the side-table carrier + every
read-site the rename would have touched that the side-table must now route
through `backend_shape.get(rule)` — converting NON-ZERO/UNKNOWN into a counted
introduce-site number. The 0→N introduce delta is the path-(b) price.

**T-P1-divergence antecedent.** Divergence **F** sub-surface (`1a` SUB17-006,
`1e` D-1E-SKV17-05 / LAC-1E-SKV17-04 — the V5-re-priced path-(b)).

**Grammar-neutral verdict (Lock 14).** GROUNDED grammar-neutral. The rename is a
generator-side identifier change; `StructLayout`/`Layout` is grammar-blind (keyed
by `RuleId`). The regen touches generated output only (allowance, `LOCKS.md:351`).

**Lock surface.** Lock 2 (`LOCKS.md:160`, name-retirement); the v+1 note bars
claiming Lock 2 closure by `LayoutFacts` ALONE while public `Layout`/`LayoutSink`
remain absent (`LOCKS.md:162-166`) — path-(b) is a re-scope, not a closure.
**Risk: path-(a) 960-site/medium; path-(b) text-only re-scope low, core
materialisation UNKNOWN; T-P3 3C + SK-V18 regen.** → **LAC-2F-FOLD-05.**

## Architectural Assertions Defended

1. **The tape is the V1 substrate; the 5 BackendShapes are its projections.**
   ARCH §7.3 already states this verbatim (`ARCHITECTURE.md:1088`); the fold
   directs the *impl* to fold to the spec, not the spec to the impl. The eager
   OpenFrame is the parallel-substrate pathology Lock 1 forbids
   (`LOCKS.md:75`, the `Vec<OpenFrame>::clone` 86.07% samply pathology).
2. **Lazy `ValueRef<G>` preserves rich-ast.** The SKINNY-proven projection
   reconstructs the typed CSSOM by lazy view, node kind recovered from the source
   byte at the offset (no stored tag), `PayloadArena` the bounded escape hatch for
   irreducible scalars only (SPEC `:538-541`). preserve-rich-ast holds
   (`LOCKS.md` no-flatten; SPEC `:252`). **[CH5-V1-001 fold — substrate manifest
   declaration]:** `PayloadArena` is NOT a sidecar — it is a RETAINED member of
   the one SoA Tape (`skinny/crates/runtime/src/tape/mod.rs:99`
   `payloads: PayloadArena`, verified live), so per `LOCKS.md:118-127` it carries
   `substrate_target = existing_tape` (member of the one tape, not a parallel
   substrate) / `retention_lifetime = output_row` / `policy_owner =
   generated_grammar`, bounded by the `PayloadArena.write_count == 0` invariant on
   re-readable leaves (irreducible scalars only). Implicit retention would be the
   parallel-substrate hole Lock 1 forbids; the manifest declaration closes it.
3. **The shared NEON classifier is grammar-general by data, proven.** 8 of 9
   grammars wire the **scan-leaf** (`1a` SUB17-007/008; `math.rs` excepted — its
   `ScanState` line-281 is a doc-comment, not an `OnceCell<StructuralIndex>`
   field, verified) — this is **config-breadth** (the alphabet is the only
   grammar datum, SK-V17 `SPEC.md:314-317`). The `ValueRef<G>` **value-plane**
   fold (F2) is a SEPARATE axis, **exercised JSON+CSS only**; scan-leaf wiring
   across 8 grammars is NOT a fleet-wide value-fold proof (`LOCKS.md:382-387`,
   `:423-425`). [CH2-V1-R6 fold: this row previously dropped the
   value-fold-vs-scan-wiring scoping that F5's own lock-surface row carries; the
   8-of-9 summary line is now bound to the scan-leaf config-breadth axis only.]
   The fold is a narrative fold + manifest row, not a build.
4. **The substrate-manifest category is the canonical home for tape-as-substrate.**
   The LAC-1E-14 FactStream precedent (`LOCKS.md:100-116`) already carved this
   category for exactly this purpose; the tape reuses it (`substrate_target =
   existing_tape`).

## Architectural Assertions Refuted (most load-bearing)

1. **The tape is NOT a 6th `BackendShape` variant** (REFUTES any fold that adds
   a 6th shape). G-Omega-gated (`LOCKS.md:109`), SPEC §9-blocked (`:808`); the
   5-shape domain holds (`LOCKS.md:107-108`). The tape is the substrate the
   shapes project from, recorded at the substrate manifest — the precedent
   (LAC-1E-14) is dispositive. **This is the dispatch's "propose, do NOT silently
   add a 6th" discharged in the negative.**
2. **The `FieldSource`/`StructLayout` walk is NOT a per-leaf runtime registry
   lookup** (REFUTES any projection design that walks `StructRegistry::layout(rule)`
   per leaf). It re-opens the 28-65×/983×/10583× regression (SPEC `:793-795`);
   the walk MUST be compile-time projection-emission resolved once at codegen
   (`begin_compound` reads `layout.rule_id & 0x1F` only, `tape/mod.rs:185-186`).
3. **The AoS/SoA dual end-state is NOT a permissible Lock-1 closure** (REFUTES
   keeping both encodings post-fold). Exactly ONE encoding survives
   (`LOCKS.md:75`); AoS/SoA coexistence is admissible ONLY as a transient
   fold-state.

## Open Research Questions

| UNKNOWN | blocking question | verify_action |
|---|---|---|
| 2F-FOLD-U1 | Is the SoA `Tape` the declared SK-V18 convergence-target encoding, or does core keep AoS `TapeRec` and prove parity? (resolves `1a` U-SUB17-001) | T-P3 reads `crates/core/.../tape/mod.rs:6-9` ("AoS first … later SoA split") against SPEC `:110-114` + `LOCKS.md:75`; 2F recommends SoA (the proven-and-benched encoding) but the parity-vs-adopt choice is a T-P3 synthesis call. |
| 2F-FOLD-U2 | Is each of the 8 `OnceCell<StructuralIndex>` carriers `existing_tape` (index IS the tape) or `local_temp_only`? (resolves `1e` 1E-SKV17-U2) | T-P3 reads all 8 generated parsers' `scan_structural` sites and classifies against the four `substrate_target` values BEFORE wiring the tape, else REDRESS-53 re-entry (SPEC `:577`/`:825`/`:839`). |
| 2F-FOLD-U3 | Does the aarch64 CollapsedStage (UNKNOWN-2D-05) ever admit, or does NEON permanently sit under the 4 LLVM shapes' scan-leaf FFI? | T-P2 2E source-backed aarch64 strategy (`ARCHITECTURE.md:1282` `admits_collapsed_stage` per LAC-2D-06, x86-bound at `:1151`, requires 2E source before any aarch64 admission); 2F asserts NO admission without that 2E source — no x86 close path, no D6 second substrate (SPEC `:854`). |

## LOCKS-AMENDMENTS-CANDIDATE

Candidates only; disposition is T-P3 3C, G3-gated; merge is Pass Omega, post
G-Omega. The 16-lock count is FIXED; no lock re-numbered. Scanned axes: Lock 1
substrate-union (AoS/SoA closure, OnceCell classification, OpenFrame retirement),
Lock 2 (StructLayout rename surface), Lock 10 (tape-as-category vs 6th shape;
selector wiring), Lock 14 (ValueRef<G> single generator; scan grammar-generality),
Lock 16 (NEON classifier manifest row; two-crate multi-arch scope). These 5 LACs
extend the 6 T-P1 LACs (`1e` LAC-1E-SKV17-01..06) into the **fold-direction**
form — they name the spec text the fold WRITES, not just the divergence catalogued.

| candidate | type | target locks | proposed candidate text | supporting path:line evidence | loc/risk/wave_hint |
|---|---|---|---|---|---|
| LAC-2F-FOLD-01 | refinement | L01, L10 | The spec must DIRECT the SK-V18 fold: retire the live eager `OpenFrame` builders (F1) AND converge AoS `TapeRec` → the proven SoA `Tape` as the single post-fold encoding (F3), with the all-8 `OnceCell<StructuralIndex>` `substrate_target` declaration (F7) as the pre-gate. Exactly ONE encoding survives; a dual AoS/SoA end-state is NOT a permissible Lock-1 end-state. | `LOCKS.md:75`; `crates/core/.../tape/record.rs:103`; `skinny/.../tape/mod.rs:94`; `crates/core/.../css_l4/builder.rs:16`; `crates/core/.../grammar/generated/json.rs:732`; SPEC `:110-114`. | 200-700 LOC / high (eager retirement) + medium (encoding) / SK-V18 fold |
| LAC-2F-FOLD-02 | refinement | L01, L10 | The tape folds into the V1 spec as the **substrate the 5 `BackendShape` shapes project from**, recorded at the Lock 1 substrate manifest (`substrate_target = existing_tape`) per the LAC-1E-14 FactStream precedent — NOT as a 6th `BackendShape` variant. The 5-shape domain holds; a 6th variant remains G-Omega gated. ARCH §7.3 already frames the 5 shapes as tape projections; the fold makes the substrate-category placement explicit. | `LOCKS.md:100-116` (LAC-1E-14 precedent), `:107-109` (5-shape domain); `ARCHITECTURE.md:1088`; SPEC `:808`. | 0 LOC (canon + precedent) / medium (prose) / T-P3 §3C |
| LAC-2F-FOLD-03 | addition | L14, L16 | Register the shared NEON `select_classifier(alphabet)` / `scan_structural(input, &StructuralAlphabet)` classifier as a Lock-16 primitive-manifest ROW: abstract primitive = alphabet-parametrised byte classification; scalar reference `scalar/byte_class_from_eq_set_64.rs`; checkasm parity; `substrate_target = existing_tape`; `retention_lifetime = transient-single-call`; same-wave consumer = the tape. Fold the JSON-first narrative to the grammar-general form (0-LOC). Scope-reconcile the multi-arch `crates/simd-scan` WITHOUT admitting x86 as a close path. | `LOCKS.md:453-489` (manifest), `:137-149` (Lock-1 v+1 no-cross-call-carry); SK-V17 `SPEC.md:314-317`/`:312` (alphabet-as-data Lock-14 vehicle, CH1-2F-01 re-anchor); `skinny/.../dispatch.rs:42`; `crates/simd-scan/src/lib.rs:80`, `alphabet.rs:19-37` (the `StructuralAlphabet` struct + rich-alphabet manifest grounding — `singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes`, the anchor 2A/LAC-2A-SKV17-02 uses; `:118` is the orthogonal `KernelShape::select(alphabet)` site cited in the F5 body, not the struct definition); SPEC `:104`, `:258`. | 0 LOC narrative + manifest row / low; 100-400 LOC scope reconcile / medium / T-P2→T-P3 |
| LAC-2F-FOLD-04 | addition | L01 | Add the no-per-leaf-registry-lookup fence to the substrate manifest: the `FieldSource` projection walk inside the live `StructRegistry` is compile-time emission resolved once at codegen; ANY per-leaf runtime `StructRegistry::layout(rule)` indirection in the tape/projection hot path re-opens the 28-65×/983×/10583× regression and is REJECT. The lazy `ValueRef<G>` generator (F2) resolves the layout once at codegen, never per-leaf. | SPEC `:793-795`; `crates/ir/src/registry/struct.rs:84,202,313,331`; `crates/core/.../tape/mod.rs:185-186`. | 0 LOC (fence) / high (regression class) / SK-V18 fold gate |
| LAC-2F-FOLD-05 | refinement | L02 | Re-price + direct the Lock-2 `StructLayout`→`Layout` reconcile by the TWO disjoint paths: (a) full rename across 960 sites, generator-side, regenerating 8 parsers + ~16 tests; (b) lock re-scope to a `LayoutFacts.backend_shape` side-table — but `LayoutFacts`/`backend_shape` are grep-zero in crates/ (skinny/prior-totality-only), so path-(b)'s crates/core realisation is NON-ZERO/UNKNOWN, not ~0 LOC. **path-(b) sizing route (CH6-V1-V02):** size as the 0→N introduce-site delta — baseline `grep -rcn StructLayout crates/`=960 (rename surface) vs `grep -rcn 'backend_shape\|LayoutFacts' crates/`=0 (side-table surface); the price is the side-table carrier introduce-cost + every read-site routed through `backend_shape.get(rule)`. The v+1 note bars Lock-2 closure by `LayoutFacts` alone while public `Layout`/`LayoutSink` remain absent. | `LOCKS.md:160,162-166`; `crates/ir/src/registry/struct.rs:202`; `grep StructLayout crates/`=960, `grep 'backend_shape\|LayoutFacts' crates/`=0; `skinny/crates/passes/src/lib.rs:90,:96,:385`. | path-(a) 960-site/medium; path-(b) text-only low / core UNKNOWN-now-sizable / T-P3 3C + SK-V18 regen |

## CHALLENGE pre-emption (the fold's own firewall posture)

- **CH2 GENERALITY**: every F-candidate carries a grammar-neutral verdict
  grounded against Lock 14; the `ValueRef<G>` generator (F2) re-emits JSON
  byte-equal (the CH2 generic-named-CSS-generator failure mode is the W2 gate,
  SPEC `:550-557`); the classifier (F5) is alphabet-as-data across 8 grammars.
- **CH3 REGRESSION**: no F-candidate re-opens a REDRESS-falsified route — F1
  honours the AZ-IV pre-block (deletes the eager builder, does not extend it);
  F6 keeps the StructRegistry indirection pre-blocked; F4 keeps the D6 second
  substrate + x86 CollapsedStage barred.
- **CH4 COST**: F5's classifier carries scalar-ref + checkasm (Lock 16); every
  F-candidate names a same-wave consumer (the tape for F5/F7; the generated
  projection for F2; the SK-V18 fold for F1/F3).
- **CH5 HIDDEN COUPLING**: F4 keeps the tape a substrate manifest CATEGORY (not
  a sidecar / parallel substrate); F7 declares `substrate_target` before wiring
  (no retained parallel index, REDRESS-53 pre-blocked); the mask stream stays a
  transient producer (Lock 1 v+1, no cross-call classifier carry). **V2:** F6
  names the LIVE coupling-site (`arena.rs:47`
  `StructRegistry::compound_kind_for_layout`) that FOLD-B severs, contrasted with
  the fence-clean `begin_compound` (grep-zero `StructRegistry`); Defended-#2
  declares the retained `PayloadArena` member as `substrate_target=existing_tape`
  (no implicit-retention hole). Both retained tape members are manifest-declared,
  not coupling-clean-by-assertion.
- **CH6 ANTI-PAPER-CLOSE**: no F-candidate claims "validated" on citation
  density; each names the bbnf-specific transfer reason and the concrete
  spec-text-to-write or fence-to-preserve. The hygiene fold (CH1-V5-001) is
  resolved on disk, not deferred to a later pass.

---

**2F verdict.** The greater-spec fold is **nine candidates (F1–F9) + five LACs
(LAC-2F-FOLD-01..05)**, each grammar-neutral / substrate-union-preserving /
5-shape-canon-coherent / NEON-scalar-ref+checkasm, file:line/SHA-grounded at
master `91b6893b0`. The load-bearing design move: the tape folds into the V1 spec
as the **substrate the 5 BackendShapes project from** (LAC-1E-14 precedent), NOT
a 6th shape; the eager OpenFrame retires into the flat tape + lazy `ValueRef<G>`
plane (the SK-V18 monotonic move); the `FieldSource`/`StructLayout` walk stays
compile-time projection-emission (the regression firewall). CH1-V5-001 folded.

**V3 fold (1 REVISE discharged, 0 REJECT, 0 orphan):** CH1-2F-01-RESIDUAL —
LAC-2F-FOLD-03 (`2f:580`) re-anchored `alphabet.rs:118` → `alphabet.rs:19-37`
for the `StructuralAlphabet` manifest grounding (the struct + rich-alphabet
definition — `singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes` — the
anchor 2A/LAC-2A-SKV17-02 uses); `:118` is the orthogonal
`KernelShape::select(alphabet)` site and remains correct in the F5 body
(`:290`). Verified live at master `91b6893b0`: `:19-37` = the `StructuralAlphabet`
struct, `:118` = `KernelShape::select`. Anchor-precision only; the claim was true
and grounded elsewhere; zero orphan. The V2-discharged set holds below.

**V2 fold (7 REVISE discharged, 0 REJECT, 0 orphan):** CH1-2F-01 (F5
alphabet-as-data re-anchored to SK-V17 SPEC.md:314-317, not LOCKS); CH2-V1-R6
(Defended-#3 + F5 scoped: 8-of-9 = scan-leaf config-breadth, ValueRef<G>
value-plane fold JSON+CSS-exercised only); CH4-2f-001 (F7 consumer = co-waved
F1/F3 tape-wiring, not orphan); CH5-V1-003 (F6 names live `arena.rs:47`
coupling-site FOLD-B severs); CH5-V1-001 (Defended-#2 declares `PayloadArena`
`substrate_target=existing_tape`); CH6-V1-V02 (F9 path-(b) bounded 0→N sizing
route); CH6-V1-V03 (F4 carries independent `admits_collapsed_stage`-x86-bound
corroboration beyond LAC-1E-14). Every REVISE was anchor-precision or
scoping-completeness; every underlying fold claim held. Residual REVISE: zero.
