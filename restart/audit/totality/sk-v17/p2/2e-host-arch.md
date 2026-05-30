---
agent: 2E
pass: T-P2-research
cycle: V3
generated_at: 2026-05-29T00:00:00Z
t_p1_inventories_consumed: [1a, 1b, 1c, 1d, 1e, 1f]
subject: greater-spec FOLD of the SKINNY-proven unified-tape / lazy-ValueRef<G> / StructLayout-projection + aarch64-NEON model into the V1 totality spec
master_head: 91b6893b0
t_p1_input_locked_at: 91b6893b0 (T-P1 CONVERGED input lock per dispatch)
primary_sources_cited: 9         # T-P1 file:line anchors + skinny/core source + LOCKS/ARCH/MP/SPEC surfaces actually cited in body (frontmatter count = exercised-in-body, not inventory; CH6-V1-R01)
techniques_grounded: 9
techniques_refuted: 4             # spec-assumed shape the fold/divergence-evidence does not support
prior_cycle_dispositions_folded:
  accepted:
    - FOLD-2E-A-flat-tape-adoption           # CH1/CH2/CH3/CH4/CH5/CH6 V2 ACCEPT
    - FOLD-2E-B-eager-openframe-retirement    # CH2/CH3/CH4 V2 ACCEPT; AZ-IV deletion target
    - FOLD-2E-C-lazy-valueref-materialization-plane  # CH2/CH3/CH4/CH7 V2 ACCEPT
    - FOLD-2E-D-tape-as-substrate-manifest-category-not-6th-shape   # CH2/CH3/CH5/CH6 V2 ACCEPT; corroborating-anchor held
    - FOLD-2E-E-shared-neon-classifier-lock16  # CH2/CH3/CH4 V2 ACCEPT; close-state taxonomy held (was CH4-2e-001/CH2-V1-R5)
    - FOLD-2E-F-structlayout-fieldsource-fence  # CH3/CH4/CH5 V2 ACCEPT; arena.rs:47 live wire held
    - CH1-2E-01      # V2 ACCEPT: signature names @ MASTER-PLAN.md:622; ARCH:1284 scan-leaf-FFI context only
    - CH4-2e-001     # V2 ACCEPT: table_64/prefix_xor filed scalar-delegate-non-ASM, not NEON-body rows
    - CH5-V1-001     # V2 ACCEPT: PayloadArena substrate_target=existing_tape / output_row / generated_grammar
    - CH5-V1-003     # V2 ACCEPT: live coupling-site arena.rs:47 StructRegistry::compound_kind_for_layout named
    - CH6-V1-R01     # V2 ACCEPT: U-2E-04 defer-loop deleted, concrete sources named, frontmatter count reconciled
    - CH7-001        # V2 DISCHARGED-ACCEPT: fabricated "2-3×" deleted; affirmative anti-claim + correct sonic-rs re-cite
    - CH7-002        # V2 DISCHARGED-ACCEPT: parity reworded to SK-V18 strict-equality GATE obligation everywhere
  rejected: []
  revised:
    - CH5-V2-001-COHORT-ALIGN   # V2 REVISE on 2B L5/L6 retention-token drift; the same canonical-token discipline applied to MY classifier surfaces: FOLD-2E-E + LAC-2E-SKV17-02 now carry retention_lifetime=transient-single-call (the Lock 1 v+1 primitive enum LOCKS.md:146-148), aligning with sibling 2a LAC-2A-SKV17-02 (:417) + 2f-F5 (:295). Cross-dossier consistency fold; declaration-precision class.
  first_cycle_additions: []
locks_amendment_candidates: 4
hygiene_action_folded: CH1-V5-001   # VERIFIED RESOLVED-ON-DISK at 91b6893b0: 1b carries enumerated collapsed_stage}.rs; grep ',collapsed}' {1a,1b,1e} = 0; collapsed_stage.rs exists / collapsed_tape.rs does not. Residual REVISE discharged. ARCH:1206 carries collapsed_stage.rs:15-17 (enumerated form).
---

# SK-V17 T-P2 2E — The Greater-Spec Tape/NEON/Projection Fold

## Executive Summary

2E designs the **fold** of the SKINNY-PROVEN model — flat tape + lazy
`ValueRef<G:EventGrammar>` projection from `StructLayout` + shared NEON
`select_classifier(alphabet)` + commit-by-construction — into the V1 totality
spec, grounded on the six T-P1 divergences (A tape AoS↔SoA, B eager OpenFrame,
C value-API per-grammar-EAGER, D BackendShape canon, E NEON shared classifier,
F StructRegistry/FieldSource fence). It is not the JSON-engine grounding of the
skinny S-P2; it is the totality-architecture fold the spec must direct for the
SK-V18 implementation. The fold is **monotonic skinny-proven → crates/core**
(SPEC `:110-114`); crates/core is the TARGET, skinny/crates the proven engine.

The proven **JSON recognizer** beats SOTA — Track 1 cold > sonic-rs strict on
the same plane across the corpus (`RESULTS.md:5-55`: twitter 8349 > 4913 sonic
Skipper, citm_catalog direct 33366 > 21250 sonic strict, canada parse_only
16709 > 12970 sonic Skipper) and the SK-V17 SoA-vs-sonic proof (1d:60-66,
SK17L-001). The residual gap SK-V17 excavated is a **materialization code-shape
divergence** — the eager `OpenFrame` value tree (SPEC `:791` AZ-IV global
block) — NOT a benchmarked CSS deficit. The fair CSS >SOTA bar is
**lightningcss full-CSSOM** (SPEC `:122`) and it is **UNMEASURED-PENDING**: all
per-corpus lightningcss endpoints await the W0 N≥50 harness (SPEC `:207`); no
"2-3×" CSS figure exists in SPEC/RESULTS/T-P1 and none is asserted here
(CH7-001 fold). The fold therefore retires the eager `OpenFrame` god-modules
(the AZ-IV-pre-blocked 118× shape, SPEC `:791`) and installs the lazy
`ValueRef<G>` projection (`tape/mod.rs:175`) as the **unified materialization
plane**, with full typed-AST parity to lightningcss as the SK-V18 strict-equality
GATE (`css_typed_summary_equal=true` before speed, SPEC `:129`), not a property
held at this pass (CH7-002 fold).

The flat tape is folded NOT as a 6th BackendShape but as a **substrate-manifest
category** under the 5-shape canon, exactly per the LAC-1E-14 FactStream
precedent (`LOCKS.md:100-116`); the four output-planes already carry the tape
substrate, and the `admits_collapsed_stage` x86 co-requirement (ARCH `:1206`)
mechanically proves no 6th-shape route is needed on aarch64. The shared NEON
classifier is folded as a **Lock-16 primitive-manifest entry** whose signature
names live at `MASTER-PLAN.md:622` (the H.W2.5 macro vocabulary), with ARCH
`:1284` cited only for the four-LLVM-shapes scan-leaf-FFI context (CH1-2E-01
fold), and whose classifier primitive carries the canonical Lock 1 v+1
`retention_lifetime=transient-single-call` (no cross-call retained classifier
state, `LOCKS.md:137-148`; CH5-V2-001 cohort-alignment fold). The fold admits as
a **proven grammar-neutral NEON Layer-1 body only the eq-set fan**
`byte_class_from_eq_set_64_neon`
(`aarch64/byte_class_from_eq_set_64.rs:33`, the one real NEON body, 87 LOC);
`byte_class_from_table_64` / `bitmap_prefix_xor_64` are live 3-line scalar
passthroughs to `crate::scalar::*` (`aarch64/*.rs:3`) and close
`scalar-delegate-non-ASM` per the 2B taxonomy + Lock-16 close-state
(`LOCKS.md:506-513`); FSM/frame macros are `source-present-unwired` (CH2-V1-R5
+ CH4-2e-001 fold). The `StructLayout`/`FieldSource` fence stays compile-time
projection-emission; the live coupling-site it severs is
`StructRegistry::compound_kind_for_layout(layout)` at
`crates/core/src/runtime/bbnf/arena.rs:47` in the eager arena path (CH5-V1-003
fold).

**Two manifest vocabularies, never conflated (CH5-V2-001 firewall).** The SoA
`Tape`'s retained `payloads: PayloadArena` member is a SUBSTRATE row and carries
the **substrate-manifest** lifetime enum `retention_lifetime=output_row`
(`LOCKS.md:122-123`, allowed lifetimes `{local_loop, generated_function,
output_row}`); the shared NEON classifier is a Lock 1 v+1 **primitive** and
carries the **primitive** lifetime enum `retention_lifetime=transient-single-call`
(`LOCKS.md:146-148`, `{transient-single-call, retained-within-chunk,
retained-across-call-boundary}`). These are two distinct vocabularies for two
distinct row kinds (a retained tape member vs. a per-call SIMD primitive); 2E
declares each in its own enum and never the cross.

**Refuted** (most load-bearing): a 6th BackendShape for the tape (Lock-10 bars
it); a dual AoS/SoA end-state (Lock-1 exactly-one-encoding, `LOCKS.md:75`); a
per-leaf runtime registry walk; and any x86/AVX-512 close path on the M5 Max
aarch64 target (SPEC `:806`).

**First hygiene action (per dispatch): CH1-V5-001 — VERIFIED RESOLVED-ON-DISK.**
The enumerated-filename residual is folded at master `91b6893b0`: 1b carries the
enumerated `collapsed_stage}.rs` form (`grep -c 'collapsed_stage}.rs' 1b = 3`),
`grep ',collapsed}'` in {1a,1b,1e} = 0 (exit 1, no match), `collapsed_stage.rs`
exists and `collapsed_tape.rs` does not, and ARCH `:1206` carries the enumerated
`collapsed_stage.rs:15-17` marker. The defect lived only in 1b and is already
correct; no 1a/1e fold required. The residual REVISE carried from T-P1 is
discharged.

## Technique Grounding Table

| T-P1 divergence | Source cited | grounded / refuted / partial | bbnf-specific fold note |
|---|---|---|---|
| **A** AoS `TapeRec` ↔ SoA `Tape` | core AoS `TapeRec` (16-byte); skinny SoA `Tape` six-member (`tape/mod.rs:94-100`: `source`/`offsets`/`flag_cursors`/`flag_values`/`payloads`/`id`); `LOCKS.md:75` "parallel substrates are dead"; core mod-doc "AoS first … later SoA split" | **grounded (one-encoding closure)** | Exactly ONE encoding survives post-fold; AoS/SoA coexistence is transient fold-state only. The proven SoA is the >SOTA carrier (1d:60-66) — fold adopts SoA OR proves AoS parity, NOT both. |
| **B** eager `OpenFrame` → lazy projection | CSS `CssStructBuilder` (817 LOC eager `OpenFrame` god-module); JSON `JsonStructBuilder` (231 LOC); SPEC `:791` AZ-IV 118× global block | **grounded (deletion target)** | Eager `CssTypedValue` + six `pending_*` Vec + `pending_value:Option` IS the AZ-IV eager-tree shape. Replace with lazy `ValueRef<G>`; never carry forward. |
| **C** per-grammar eager enums → `ValueRef<G>` | skinny `ValueRef<'doc,'input,K,G>` (`tape/mod.rs:175`); JSON `value_from_ref` witness; `value.rs:1` `@generated by xtask regen-*` | **grounded (Lock-14 honoured)** | Divergence is the EAGER materialization shape, not the `@generated` provenance. The fold acts on the regen generator to emit one grammar-parametric lazy projection. preserve-rich-ast: full typed-AST parity is the SK-V18 GATE, never flattened. |
| **D** BackendShape canon absorbs tape | `LOCKS.md:107-108` 5-shape canon; `:100-116` LAC-1E-14 (substrate-manifest category ≠ 6th shape); ARCH `:1799-1804` 4-plane table; ARCH `:1206` (`admits_collapsed_stage` x86 co-requirement — no aarch64 6th-shape route) | **grounded (substrate-manifest, NOT 6th shape)** | Tape is the realisation of plane (1) `existing_tape` / plane (4) `local_temp_only`, governed by `substrate_target`, NOT a new `BackendShape` variant. LAC-1E-14 is the precedent; ARCH `:1206` is the independent corroborating anchor (CH6-V03). |
| **E** NEON shared classifier | `select_classifier(&[u8;64])` `dispatch.rs:42`; classifier macro-name set `MASTER-PLAN.md:622` (H.W2.5); the one real NEON body `byte_class_from_eq_set_64_neon` `aarch64/byte_class_from_eq_set_64.rs:33`; ARCH `:1284` (four-LLVM-shapes scan-leaf-FFI context only); `LOCKS.md:453-489` Lock 16; `:506-513` close-state; `:137-148` Lock 1 v+1 primitive `retention_lifetime` enum | **grounded (Lock-16 manifest, scoped)** | The classifier is grammar-general by alphabet-as-data. Fold the JSON-scanner framing to the alphabet-parametrised shared form, declaring the primitive `retention_lifetime=transient-single-call`. Admit ONLY the eq-set fan as a proven NEON Layer-1 body; table/prefix are scalar delegates (CH2-V1-R5, CH4-2e-001). |
| **F** `StructLayout`/`FieldSource` fence | live coupling-site `StructRegistry::compound_kind_for_layout(layout)` `crates/core/.../bbnf/arena.rs:47`; fence-clean tape path `begin_compound(&StructLayout)` (grep-zero `StructRegistry`); SPEC `:793-795` | **grounded (0-LOC fence; CRITICAL if violated)** | FOLD-B's deletion of the eager arena builders severs the `arena.rs:47` wire; the tape `begin_compound` already takes a pre-resolved `&StructLayout` with no `StructRegistry` lookup. A naive per-leaf registry walk re-opens the worst measured regression. |
| **D'** aarch64 CollapsedStage | ARCH `:1206,:1278-1282` UNKNOWN-2D-05 + `admits_collapsed_stage` predicate; SPEC `:854` D6/asmjson x86 host-blocked | **refuted-as-fresh-gap / grounded-as-spec-named-unknown** | NOT a fresh gap: aarch64 CollapsedStage is the spec-named UNKNOWN-2D-05, mechanically refused at `admits_collapsed_stage`. NEON sits under the four LLVM shapes' scan-leaf FFI; no aarch64 CollapsedStage admission this pass. |
| sixth BackendShape for the tape | `LOCKS.md:107-109,:272-273` (G-Omega gated); SPEC `:808` | **refuted** | A 6th `BackendShape` is barred by Lock 10 + SPEC §9 second-substrate block. The tape is a substrate-manifest substrate, not a shape. |
| dual AoS/SoA end-state | `LOCKS.md:75`; 1a:93,105,148; 1e LAC-1E-SKV17-01 | **refuted** | A post-fold dual encoding is a Lock-1 violation, not a tree-local choice. |
| x86 / AVX-512 close path | SPEC `:806`; ARCH `:1276-1282`; 1d L-SK17-06 | **refuted** | Apple M5 Max has no SVE; aarch64 NEON only; x86 esoterica are >SOTA-x86 hardware-gated, never the close route. |

## The Six Candidate Folds (load-bearing enumeration)

Each fold: **shape** (what the fold installs) · **T-P1-divergence antecedent**
(the catalogued divergence it discharges) · **grammar-neutral verdict** (Lock 14)
· **lock surface** (the locks it touches and the closure obligation).

### FOLD-2E-A — Flat-tape adoption (one encoding under Lock 1)

- **Shape.** crates/core adopts the SKINNY-proven flat tape as the single live
  substrate. The convergence target is the SoA `Tape<'input>` six-member shape
  (`source: &'input [u8]`, `offsets: Vec<u32>`, sparse `flag_cursors: Vec<u32>` /
  `flag_values: Vec<u8>`, `payloads: PayloadArena`, `id`,
  `tape/mod.rs:94-100`) — OR the AoS `TapeRec` proven byte-parity-equal to the
  SoA projection. The core mod-doc already names the path: "kept AoS first …
  the same `TapeCursor` API rides a later SoA split". **2E names the convergence
  target the proven SoA** (it is the >SOTA carrier, 1d:60-66; JSON Track 1 cold
  > sonic same-plane, `RESULTS.md:5-55`); the AoS-first state is the transition,
  the SoA split is the end-state. Exactly ONE encoding survives (`LOCKS.md:75`
  "parallel substrates are dead").
- **T-P1-divergence antecedent.** Divergence **A** (SUB17-002 / SK17L-001 /
  D-1E-SKV17-01). Core AoS `TapeRec` and skinny SoA `Tape` are both Lock-1-admitted
  offset tapes; the fold resolves the cross-tree shape bifurcation to one.
- **Grammar-neutral verdict.** GROUNDED. The tape carries NO grammar policy:
  `push_plain_offset` is one branchless u32 write (1d:60); the `TapeStructBuilder`
  dispatches on the `StructLayout`, never on per-grammar route strings. Both AoS
  and SoA are grammar-as-data. Lock 14 holds.
- **Lock surface.** Lock 1 (substrate-union; exactly-one-encoding closure
  obligation). The dual end-state is a Lock-1 violation; the closure is a
  **catalogued divergence**, not an open question (LAC-1E-SKV17-01). Risk
  medium; 200-600 LOC SK-V18; eager-builder retirement propagates 22+ files
  (1a:126). Sequenced WITH FOLD-2E-B (the tape has no live consumer until the
  eager builder is retired, 1d:146).
  - **Substrate-member manifest (CH5-V1-001 fold; SUBSTRATE vocabulary).** The
    SoA `Tape`'s `payloads: PayloadArena` (`tape/mod.rs:99`) is a RETAINED member
    of the one tape, NOT a sidecar. It is a **substrate row** and is declared in
    the **substrate-manifest** vocabulary (`LOCKS.md:118-127`): `substrate_target
    = existing_tape` (member of the one tape) / `retention_lifetime = output_row`
    (the substrate-manifest lifetime enum `{local_loop, generated_function,
    output_row}`, `:122-123` — NOT the primitive enum `:146-148`) / `policy_owner
    = generated_grammar`, bounded by the `PayloadArena.write_count == 0` invariant
    on re-readable leaves. Implicit retention would be the parallel-substrate hole
    Lock 1 forbids; the manifest declaration closes it. (The classifier primitive's
    `retention_lifetime` is the OTHER enum — see FOLD-2E-E; never conflated.)

### FOLD-2E-B — Eager OpenFrame retirement (AZ-IV pre-block deletion)

- **Shape.** Delete the eager `OpenFrame` builders — CSS `CssStructBuilder`
  (817 LOC, `stack: Vec<OpenFrame>` + six `pending_* Vec` + `pending_value:
  Option<CssTypedValue>`) and JSON `JsonStructBuilder` (231 LOC). The live parse
  path threads the wired structural index into the flat tape (FOLD-2E-A) by
  commit-by-construction; the value tree is materialized lazily on demand
  (FOLD-2E-C), not eagerly at parse time.
- **T-P1-divergence antecedent.** Divergence **B** (the load-bearing divergence;
  SK17L-003 / L-SK17-01). The eager `CssTypedValue` + `pending_*` Vecs ARE the
  AZ-IV eager-value-tree shape the skinny path REFUTED at 118× (canada
  1.83ms→215.7ms, SPEC `:791`). It is the AZ-IV-pre-blocked **fold-DELETION
  target**, never carried forward.
- **Grammar-neutral verdict.** GROUNDED. The eager per-grammar builders are
  per-grammar runtime surfaces (Lock 14 ALLOWED as generated output), but they
  are the fold-deletion target, not a Lock-14 leak (1e L14 row). Their
  replacement — the flat tape + lazy projection — is grammar-neutral.
- **Lock surface.** Lock 1 (no `Vec<OpenFrame>::clone` parallel substrate,
  `LOCKS.md:75`) + Lock 10 (the AZ-IV pre-block is the 118× refutation;
  materialization stays lazy-by-default). Risk high; the 817-LOC CSS god-module
  is the deletion target (1d SK17L-003); regen-gated re-emit across 8 grammars.
  **This is the recognizer-vs-materialization code-shape divergence SK-V17
  excavated** (the eager `OpenFrame`, SPEC `:791`) — the fold closes it by
  deletion + lazy projection, not by optimizing the eager tree, and not by
  closing a benchmarked CSS deficit (the lightningcss bar is UNMEASURED-PENDING,
  SPEC `:207`).

### FOLD-2E-C — Lazy `ValueRef<G>` as the unified materialization plane

- **Shape.** One grammar-parametric lazy projection generator emits the value
  API for all 8 grammars: `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>`
  (`tape/mod.rs:175`) read by a per-grammar `value_from_ref<'doc,'input>` rider
  (JSON witness `grammars/json/value.rs`, zero per-node heap alloc). The fold
  retargets the EXISTING `@generated by xtask regen-{json,css}` path
  (`value.rs:1`) to emit the lazy `ValueRef<G>` projection instead of the eager
  typed enums (`CssTypedValue<'p>`). The projection is the **single
  materialization surface** — one `ValueRef<G>` plane, one Visitor pattern,
  borrowing `&'i Tape<'i>` + cursor (Lock 1 borrow shape, `LOCKS.md:75`).
- **T-P1-divergence antecedent.** Divergence **C** (SUB17-004 / SK17L-002 /
  D-1E-SKV17-02). The divergence is the EAGER materialization shape, NOT the
  `@generated` provenance (Lock-14 honoured — hand-writing would be the
  VIOLATION). The fold establishes the one `ValueRef<G>` projection generator
  that re-emits all 8 per-grammar value surfaces.
- **Grammar-neutral verdict.** GROUNDED, with a CH2 firewall. The projection
  generator must re-emit JSON byte-equally (the W2 R-CH2-1 isomorphism anchor,
  1d:108); a CSS-only generator that never re-emits JSON FAILS CH2. The
  `BackendRule`/`FieldSource` walk is the recipe, alphabet/layout are DATA.
  **preserve-rich-ast obligation (CH7-002 fold):** the projection must reach
  full typed-AST parity with lightningcss — this is the SK-V18 strict-equality
  GATE (`css_typed_summary_equal=true` gate-before-speed + `css_rich_ast_preserved
  =true`, SPEC `:129`; `assert_lightningcss_strict_equality` SPEC `:98`), an
  obligation/target the fold must meet, NOT a property held at this pass.
- **Lock surface.** Lock 1 (one materialisation surface) + Lock 14 (one
  grammar-agnostic generator template) + Lock 2 (the projection walks `Layout`,
  the canonical record). Risk high; 300-700 LOC generator-side + per-grammar
  regen across 8 grammars; generator-LOC distinguished from regen-LOC. The
  **durable fold** — this is the materialization plane the whole spec converges
  on.

### FOLD-2E-D — Tape as substrate-manifest substrate, NOT a 6th BackendShape

- **Shape.** The flat tape is folded into the spec as the **realisation of the
  retained-tape output-plane** (plane (1) `existing_tape`, ARCH `:1801`) under
  the 5-shape `BackendShape` canon — NOT as a new shape variant. The
  `EagerTape`/`OffsetTape`/`EventTape` shapes are the three ways the substrate
  *projects* for a rule (ARCH `:1088`, `:1799-1804`); the flat tape is the
  concrete substrate those shapes write to, carrying `substrate_target =
  existing_tape` (parse-retained) or `local_temp_only` (transient scan index,
  plane (4)). The grammar's structural-index `OnceCell` classifies under one of
  these two `substrate_target` values before any tape wiring (LAC-1E-SKV17-03,
  U-2E-02).
- **T-P1-divergence antecedent.** Divergence **D** (SUB17-005 / SK17L-006 /
  D-1E-SKV17-04). The dispatch's load-bearing question: is the tape a 6th
  BackendShape OR a substrate-manifest category under the 5-shape canon? 2E
  **proposes the latter and does NOT silently add a 6th** — grounded on the
  **LAC-1E-14 precedent** (`LOCKS.md:100-116`): FactStream is the 5th
  *substrate-manifest category*, explicitly NOT a 6th `BackendShape`; the tape
  is governed identically as a substrate under `substrate_target`, not a shape.
  **Independent corroborating anchor (CH6-V03 fold):** the
  `admits_collapsed_stage` x86 co-requirement at ARCH `:1206` mechanically
  proves no 6th-shape route is needed on aarch64 — the only shape that would
  consume hand-written NASM (`CollapsedStage`/`FSM_DISPATCH_THREADED`) is x86-pinned
  and mechanically refused on aarch64, so the tape cannot become a sixth shape;
  it is a substrate. This anchor is independent of the LAC-1E-14/FactStream
  citation the other D-folds share.
- **Grammar-neutral verdict.** GROUNDED. The substrate-manifest classification
  is grammar-neutral by construction — every e-graph candidate, backend rewrite,
  and SIMD consumer declares `substrate_target`/`retention_lifetime`/`policy_owner`
  (`LOCKS.md:118-127`), no grammar name. Lock 14 holds.
- **Lock surface.** Lock 10 (5-shape canon `{EagerTape, OffsetTape, EventTape,
  SinkOnly, CollapsedStage}` holds verbatim, `LOCKS.md:107-108`; a 6th shape is
  G-Omega gated `:272-273`) + Lock 1 (substrate manifest, the LAC-1E-14
  precedent extends to the tape). The aarch64 CollapsedStage is the spec-named
  UNKNOWN-2D-05 (ARCH `:1206`), NOT a fresh gap; NEON absorbs under the four
  LLVM shapes' scan-leaf FFI. Risk medium (60-200 LOC selector + decision-engine
  wiring; the `derive_backend_shape` selector lives only in skinny
  `passes/lib.rs:392`, wired into core atop the single `EmitStrategy::StructDirect`).
  0-LOC for the canon itself; the wiring is the cost.

### FOLD-2E-E — Shared NEON classifier as a Lock-16 primitive-manifest entry

- **Shape.** The shared NEON classifier vocabulary is folded as Lock-16
  primitive-manifest entries. The signature-name set is the **H.W2.5 macro
  vocabulary** at `MASTER-PLAN.md:622` (`BYTE_CLASS_FROM_TABLE_64`,
  `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`,
  `EOB_PAD_CLAMP`, the AArch64 structural+terminator classifier,
  `BULK_EMIT_POSITIONS_64`); the live skinny dispatch table mirrors them
  (`PrimitiveKernels` `dispatch.rs:51-52`). ARCH `:1284` is cited only for the
  **four-LLVM-shapes scan-leaf-FFI context** (which shapes consume Layer-1
  primitives at scan-shaped inner loops), NOT as the signature table (CH1-2E-01
  fold). The spec's JSON-scanner narrative folds to the
  **alphabet-parametrised shared form**: `select_classifier(alphabet: &'static
  [u8;64])` (`dispatch.rs:42`) / `scan_structural(input, &StructuralAlphabet)`,
  where the alphabet is the only grammar datum (SPEC `:314-317`, the Lock-14
  vehicle).
- **Primitive substrate declaration (CH5-V2-001 cohort-alignment fold; PRIMITIVE
  vocabulary).** The classifier is a Lock 1 v+1 **primitive**, not a substrate
  member; it therefore carries the **primitive** `retention_lifetime` enum, NOT
  the substrate-member enum FOLD-2E-A uses. Its manifest row declares
  `substrate_target = existing_tape` (it feeds the tape's `offsets` — resolving
  to `existing_tape` AFTER the U-2E-02 OnceCell classify-before-wiring step,
  pending today) / `retention_lifetime = transient-single-call`
  (`LOCKS.md:146-148`, `{transient-single-call, retained-within-chunk,
  retained-across-call-boundary}`; the per-call composed form, no cross-call
  retained classifier state — the Lock 1 v+1 substrate-union ELEVATION,
  `LOCKS.md:137-148`) / `policy_owner = generated_grammar` (alphabet-as-data),
  same-wave consumer = the tape. This is the **canonical token** — it aligns with
  the sibling cohort rows `2a LAC-2A-SKV17-02` (`:417`
  `retention_lifetime=transient-single-call`) and `2f-F5` (`:295` same), and is
  the same canonical discipline CH5-V2-001 imposed on 2B's L5/L6 prose drift
  (`within-block-only`/`within-call-only` → `transient-single-call`). A
  classifier primitive that retained cross-call mask/prev-state would be the
  `retained-across-call-boundary` REJECT class (`LOCKS.md:147-148`); the fold
  declares `transient-single-call` and stays substrate-union-compatible by
  construction.
- **Close-state taxonomy (CH2-V1-R5 + CH4-2e-001 fold).** Admit only **exercised
  consumers**, not vocabulary presence; adopt the 2B Lock-16 close-state taxonomy
  (`LOCKS.md:506-513`):
  - **`byte_class_from_eq_set_64_neon`** (`aarch64/byte_class_from_eq_set_64.rs:33`,
    87 LOC: four `vld1q_u8` stripes + `vceqq_u8`/`vorrq_u8` reduce) is the **one
    real NEON Layer-1 body** today (verified live at `91b6893b0`). It is the
    proven grammar-neutral NEON manifest row — scalar-ref + strict checkasm +
    consumer (the JSON+CSS classify path; CSS `;{` uses the eq-set fan, NOT the
    lo6 `classify_tbl4` table, per the slot-59 `& 0x3f` collision SPEC `:316-317`,
    binding the CSS non-JSON consumer to a real NEON body per CH2-V1-R4).
  - **`byte_class_from_table_64` / `bitmap_prefix_xor_64`** are live **3-line
    scalar passthroughs** to `crate::scalar::*` (`aarch64/byte_class_from_table_64.rs:3`,
    `aarch64/bitmap_prefix_xor_64.rs:3`, verified live at `91b6893b0`: each `*_neon`
    fn delegates at line 3 to `crate::scalar::*`). They close
    **`scalar-delegate-non-ASM`** (Lock-16 close-state `LOCKS.md:507`); the
    manifest MUST NOT price them as NEON-body rows with NEON consumers. Any
    future NEON body for either is a separate wave with its own scalar-ref +
    checkasm + consumer + row movement.
  - **FSM/frame macros** (`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`/`POP`)
    are source-only with no scalar/checkasm/consumer; they close
    **`source-present-unwired`**/`architectural-block-with-REDRESS` — the
    `CollapsedStage` spine, x86/AVX-512-pinned, mechanically refused on aarch64
    (UNKNOWN-2D-05). Not an SK-V17/SK-V18 aarch64 admission.

  A `1:1 ARCH-signature mapping` is **naming, not exercise-proof**: the fold
  admits the eq-set fan on exercised-consumer grounds and files the rest by their
  live close-state.
- **T-P1-divergence antecedent.** Divergence **E** (SUB17-007/008 / SK17L-008 /
  D-1E-SKV17-06). The classifier scan-leaf is **wired (scan-leaf) across 8
  grammars** (`scan_structural` in json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf;
  1a:147) — but this is **config-breadth, value-plane-exercised on JSON+CSS only**
  (CH2-V1-R3 alignment); scan-leaf wiring across 8 grammars is NOT a fleet-wide
  value-fold proof. It is impl-exceeds-spec — a **0-LOC narrative fold** of the
  JSON-scanner framing, plus a 100-400 LOC scope reconcile of the multi-arch
  `crates/simd-scan` against the aarch64-only proven `skinny/crates/bbnf-simd`.
- **Grammar-neutral verdict.** GROUNDED, **scoped**. The alphabet-as-data
  classifier is the Lock-14 vehicle (SPEC `:314-317`); the rich
  `StructuralAlphabet` (`singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes`)
  is config-breadth, not grammar branching. Lock 14 holds. Generality is
  **breadth-of-config, value-plane-exercised JSON+CSS only** (1d SK17L-008);
  Sheets/BBNF-self by-construction, proof SK-V18; **may not claim fleet-wide**
  (`LOCKS.md:382-387,:423-425`).
- **Lock surface.** Lock 16 (primitive manifest; scalar-ref + strict checkasm
  with `BBNF_SIMD_STRICT=1`, `LOCKS.md:491-493`; same-wave consumer; close-state
  `:506-513`; the primitive `retention_lifetime=transient-single-call`,
  `:146-148`) + Lock 1 v+1 (no cross-call retained classifier state, the
  substrate-union ELEVATION `:137-148`) + Lock 14 (grammar-neutral alphabet).
  aarch64 NEON only (NEON baseline; x86 avx2/avx512/wasm cfg-gated non-aarch64;
  no x86 admission, SPEC `:806`). Risk low/0-LOC narrative fold + 100-400 LOC
  scope reconcile (LAC-1E-SKV17-06). The multi-arch `crates/simd-scan` scope
  decision (narrow-to-aarch64 vs retain x86 kernels) binds to the fold WITHOUT
  admitting x86 as a close path.

### FOLD-2E-F — `StructLayout`/`FieldSource` compile-time fence (regression firewall)

- **Shape.** Keep the AZ-IV indirection pre-blocked. The `FieldSource` projection
  walk (inside the live `StructRegistry`) is COMPILE-TIME projection-emission,
  resolved once at codegen — the `BackendRule`/`FieldSource` walk emits the field
  routing into the generated projection (FOLD-2E-C), NOT a per-leaf runtime walk.
  **Live coupling-site (CH5-V1-003 fold):** the present-tense wire FOLD-B's
  deletion severs is `StructRegistry::compound_kind_for_layout(layout)` at
  `crates/core/src/runtime/bbnf/arena.rs:47` (in `BbnfCompoundKind::from_layout`,
  the eager arena path; verified live at `91b6893b0`). Deleting the eager builders
  (FOLD-2E-B) severs this runtime registry coupling. Contrast the fence-clean
  tape path: `begin_compound(&StructLayout)` already takes a pre-resolved layout
  by reference and reads only `layout.rule_id & 0x1F` — grep-zero `StructRegistry`
  inside it; no per-leaf `StructRegistry::layout(rule)` lookup. The fold
  preserves this no-runtime-lookup property. Separately, the Lock-2
  `StructLayout`→`Layout` rename surface is reconciled generator-side.
- **T-P1-divergence antecedent.** Divergence **F** (SUB17-009 / SK17L-004 /
  D-1E-SKV17-03; the regression firewall). A naive per-leaf
  `StructRegistry::layout(rule)` re-opens the worst measured regression
  (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind, SPEC
  `:793-795`).
- **Grammar-neutral verdict.** GROUNDED. The `FieldSource` enum
  `{TypedLeaf, BranchTag, SeqPosition, RepeatElement, RuleReference}` IS the
  BackendRule-walk recipe, resolved at compile time, grammar-neutral. Lock 14
  holds.
- **Lock surface.** Lock 1 (no parser-owned per-leaf indirection) + Lock 2
  (`StructLayout` Lock-2-retired but live at the rename surface; path-(a)
  full-rename / medium, path-(b) `LayoutFacts` side-table is skinny/prior-totality-only
  — `grep -rn LayoutFacts crates/` = 0, 1e LAC-04). **0-LOC fence;
  CRITICAL/regression if violated.** This is the pre-block the fold inherits
  inviolate — the AZ-IV/StructRegistry pre-blocks are not re-openable (no
  re-opened REDRESS per dispatch); the live `arena.rs:47` wire is severed by
  deletion, not re-routed.

## Architectural Assertions Defended

1. **The tape is the realisation of the retained-tape output-plane, governed by
   `substrate_target` — NOT a 6th BackendShape.** Grounded on the LAC-1E-14
   FactStream precedent (`LOCKS.md:100-116`, ARCH plane-table `:1799-1804`) AND
   the independent `admits_collapsed_stage` x86 co-requirement (ARCH `:1206`)
   which mechanically proves no aarch64 6th-shape route exists. The 5-shape
   canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` holds
   verbatim. The fold extends the precedent to the flat tape without a silent
   6th shape (G-Omega gated). [FOLD-2E-D]

2. **The lazy `ValueRef<G>` projection is the single materialization plane that
   closes the materialization code-shape divergence SK-V17 excavated.** The
   proven JSON recognizer beats sonic-rs on the same plane (`RESULTS.md:5-55`,
   1d:60-66); the gap is the eager `OpenFrame` materialization shape (SPEC
   `:791`), NOT a benchmarked CSS deficit — the lightningcss bar is
   UNMEASURED-PENDING (SPEC `:207`). The fold deletes the eager `OpenFrame`
   (FOLD-2E-B) and installs the lazy projection (FOLD-2E-C) — one surface, one
   Visitor; full typed-AST parity with lightningcss is the SK-V18 GATE
   (SPEC `:129`), the obligation the fold must meet. [FOLD-2E-B+C]

3. **The eq-set fan `byte_class_from_eq_set_64_neon` is the one proven
   grammar-neutral NEON Layer-1 body; the classifier vocabulary admits by
   exercised consumer, not vocabulary presence; the primitive carries the
   canonical `retention_lifetime=transient-single-call`.** The H.W2.5 signature
   names (`MASTER-PLAN.md:622`) are the manifest vocabulary; only the eq-set fan
   (`aarch64/byte_class_from_eq_set_64.rs:33`) is a real NEON body with scalar-ref
   + checkasm + JSON+CSS consumer. `byte_class_from_table_64`/`bitmap_prefix_xor_64`
   close `scalar-delegate-non-ASM` (3-line passthroughs, `aarch64/*.rs:3`);
   FSM/frame macros close `source-present-unwired`. The primitive declares the
   Lock 1 v+1 `retention_lifetime=transient-single-call` (`LOCKS.md:146-148`),
   matching 2a `:417` and 2f `:295`. A `1:1 ARCH-signature mapping` is naming,
   not exercise-proof. [FOLD-2E-E]

4. **The `FieldSource`/`StructLayout` walk is compile-time projection-emission;
   the live coupling-site `arena.rs:47` is severed by deletion.** The eager-arena
   `StructRegistry::compound_kind_for_layout(layout)` (`arena.rs:47`) is the
   present-tense wire; FOLD-B's deletion severs it. The fence-clean tape path
   `begin_compound(&StructLayout)` is grep-zero `StructRegistry`. The fold
   inherits the correct no-per-leaf-lookup shape. [FOLD-2E-F]

5. **Exactly ONE tape encoding survives the fold; the SoA is the convergence
   target.** The dual AoS/SoA state is transient; the SoA is the >SOTA carrier
   (`RESULTS.md:5-55`). Its retained member `payloads: PayloadArena` is declared
   in the SUBSTRATE-manifest vocabulary `substrate_target = existing_tape` /
   `retention_lifetime = output_row` (`:122-123`) / `policy_owner =
   generated_grammar` (`LOCKS.md:118-127`), not an implicit sidecar — distinct
   from the classifier primitive's `transient-single-call` (FOLD-2E-E), never
   conflated. [FOLD-2E-A]

## Architectural Assertions Refuted

1. **A 6th BackendShape for the tape is refuted.** Lock 10 bars it
   (`LOCKS.md:107-109,:272-273`); SPEC §9 second-substrate block names "sixth
   `BackendShape`" forbidden (`:808`). The substrate-manifest category (LAC-1E-14)
   is the correct vehicle — the dispatch's explicit constraint ("propose, do NOT
   silently add a 6th") is honoured by the substrate-manifest fold.

2. **A dual AoS/SoA end-state is refuted.** `LOCKS.md:75` "parallel substrates
   are dead" — a post-fold dual encoding is a Lock-1 violation, not a tree-local
   choice. The core mod-doc's "AoS first … later SoA split" is a transition, not
   an end-state declaration.

3. **A per-leaf runtime `StructRegistry::layout(rule)` walk is refuted.** It
   re-opens the 28-65×/983×/10583× regression (SPEC `:793-795`); the walk MUST
   stay compile-time projection-emission. The live `arena.rs:47` coupling is
   severed by deletion, never re-routed into the hot path. (AZ-IV indirection
   pre-block, not re-openable.)

4. **Any x86 / AVX-512 / SVE close path is refuted on the M5 Max aarch64
   target.** SPEC `:806` (Apple cores have no SVE); ARCH `:1276-1282` (x86
   esoterica are architecture-pressure ONLY, cannot close aarch64 admission). The
   aarch64 CollapsedStage is the spec-named UNKNOWN-2D-05, mechanically refused
   at `admits_collapsed_stage` — NOT a fresh gap, NOT admitted this pass.

## Open Research Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-2E-01 | Is the SoA the declared SK-V18 convergence target, or does core keep AoS and prove byte-parity? 2E names SoA (>SOTA carrier); T-P3 must record the closure as a catalogued divergence, not an open question. | T-P3/Omega assert the Lock-1 one-substrate closure (`LOCKS.md:75`) against the core mod-doc + `tape/mod.rs:94`; the dual encoding is NOT a permissible end-state. No LOCKS edit by T-P2. |
| U-2E-02 | Does each grammar's structural-index `OnceCell` (all 8 carriers) classify `existing_tape` (index IS the tape) or `local_temp_only` under the Lock-1 v+1 manifest? | T-P2/SK-V18 read all 8 `scan_structural` sites and classify against the four `substrate_target` values BEFORE wiring the tape — classify-before-wiring, either value — else REDRESS-53 re-entry (SPEC `:577`/`:825`/`:839`). Cross-ref 2f-F7 / 2a-E (the output resolves to `existing_tape` ONLY after the all-8-carrier classification + tape wiring). |
| U-2E-03 | Does `crates/simd-scan` narrow to the proven aarch64 set or retain x86/avx512/wasm kernels post-fold (scope, not defect)? | T-P2 compare `simd-scan/lib.rs` vs SK-V17 aarch64-only mandate (SPEC `:258`); architecture-scope decision; no x86 admitted as a close path. |
| U-2E-04 | Is the aarch64 CollapsedStage (UNKNOWN-2D-05) reachable via a NEON FSM-dispatch primitive, or does NEON stay strictly under the four LLVM shapes' scan-leaf FFI? | **CONCRETE sources NOW (CH6-V1-R01 fold; no defer-loop):** the Arm A64 ISA manual FSM-dispatch lineage + Lemire 2026 `svmatch_u8`-on-NEON port + Validark 2024 (the same sources 2B grounds at 2b:73/166-167). **Bounded refutation recorded:** NEON has no AVX-512-mask branchless-FSM analogue — the `FSM_DISPATCH_THREADED`/`CollapsedStage` spine is x86/AVX-512-pinned (ARCH `:1278-1282`), mechanically refused on aarch64 at `admits_collapsed_stage` (LAC-2D-06). No admission this pass; NEON stays under the four LLVM shapes' scan-leaf FFI. This resolves 2d:264 UNKNOWN-2D-05 ("2E must supply the aarch64 source"): the source is supplied here and the refutation is bounded — no future-cycle defer. |

## LOCKS-AMENDMENTS-CANDIDATE

Candidates only; disposition is T-P3 3C, G-Omega-gated; merge is Pass Omega.
Four candidates. Scanned axes: Lock 1 (substrate-union one-encoding closure +
tape-as-substrate-manifest + PayloadArena member manifest + classifier primitive
`transient-single-call`), Lock 10 (5-shape canon absorption, no 6th shape),
Lock 14 (classifier grammar-generality, scoped JSON+CSS), Lock 16 (NEON
primitive manifest with close-state taxonomy). These extend/mirror the six T-P1
1E LACs (LAC-1E-SKV17-01..06) into the fold-design register; they do not
re-number the 16 locks.

| candidate | type | target locks | proposed candidate text | supporting path:line evidence | loc/risk/wave_hint |
|---|---|---|---|---|---|
| LAC-2E-SKV17-01 | refinement | L01, L10 | Extend the LAC-1E-14 substrate-manifest precedent to the flat tape: the unified flat tape is the realisation of plane (1) `existing_tape` / plane (4) `local_temp_only` under the 4-plane output table, NOT a 6th `BackendShape`. The 5-shape canon holds verbatim; the tape is governed by `substrate_target`, not a shape variant. Corroborated by the `admits_collapsed_stage` x86 co-requirement (no aarch64 6th-shape route). | `LOCKS.md:100-116` (LAC-1E-14); ARCH `:1799-1804` (4-plane table); `:1088` (union); `:1206` (UNKNOWN-2D-05 + `admits_collapsed_stage`). | 0 LOC (precedent extension) / medium (T-P3 spec-amendment) / SK-V18 fold design |
| LAC-2E-SKV17-02 | addition | L16, L01 | Admit the shared NEON classifier as Lock-16 primitive-manifest rows with EXPLICIT close-state taxonomy AND the canonical Lock 1 v+1 primitive `retention_lifetime`: `byte_class_from_eq_set_64_neon` admits as the one proven NEON Layer-1 body (scalar-ref + strict checkasm + JSON+CSS consumer; CSS uses the eq-set fan not the lo6 table; `substrate_target=existing_tape` / `retention_lifetime=transient-single-call` / `policy_owner=generated_grammar`, no cross-call retained classifier state); `byte_class_from_table_64`/`bitmap_prefix_xor_64` close `scalar-delegate-non-ASM` (live 3-line passthroughs); FSM/frame macros close `source-present-unwired`. Vocabulary names from `MASTER-PLAN.md:622`; admission is exercised-consumer, not name-presence. aarch64 NEON only. | `MASTER-PLAN.md:622` (H.W2.5 macro vocabulary); `dispatch.rs:42,51-52`; `aarch64/byte_class_from_eq_set_64.rs:33`; `aarch64/byte_class_from_table_64.rs:3` + `bitmap_prefix_xor_64.rs:3` (passthroughs); `LOCKS.md:453-489,:491-493,:506-513,:137-148` (primitive `retention_lifetime` enum); ARCH `:1284` (scan-leaf-FFI context only). | 0 LOC (eq-set fan proven) / low / SK-V18 fold |
| LAC-2E-SKV17-03 | refinement | L01, L14 | The lazy `ValueRef<G>` projection is the SINGLE materialization plane post-fold; the eager `OpenFrame` per-grammar builders are the fold-deletion target (not carried). One grammar-agnostic generator emits the projection for all 8 grammars; CH2 requires JSON byte-equal re-emission. preserve-rich-ast: full typed-AST parity with lightningcss is the SK-V18 strict-equality GATE (`css_typed_summary_equal=true`), an obligation not a held property. | `tape/mod.rs:175`; `json/value.rs` (`value_from_ref` witness); `CssStructBuilder` (817 LOC deletion target); `value.rs:1` (@generated); SPEC `:129` (gate). | 300-700 LOC generator + 8-grammar regen / high / SK-V18 fold |
| LAC-2E-SKV17-04 | refinement | L01 | Record the AoS/SoA one-encoding closure as a CATALOGUED divergence, not an open question: SoA is the named SK-V18 convergence target (>SOTA carrier); a dual end-state is a Lock-1 violation. The SoA's retained member `payloads: PayloadArena` is declared in the SUBSTRATE-manifest vocabulary `substrate_target=existing_tape` (member of the one tape, not a sidecar) / `retention_lifetime=output_row` (the substrate-manifest enum, distinct from the primitive enum) / `policy_owner=generated_grammar`. (Reinforces LAC-1E-SKV17-01 + closes CH5-V1-001.) | `LOCKS.md:75,:118-127` (substrate-manifest vocabulary; `output_row` at `:122-123`); `tape/mod.rs:94-100` (`payloads: PayloadArena` member); `RESULTS.md:5-55`. | 200-600 LOC / medium / SK-V18 fold |

## Fold Coherence Note (CH5 hidden-coupling pre-empt)

The six folds are sequenced, not orthogonal: FOLD-2E-A (tape) and FOLD-2E-B
(eager retirement) are co-dependent (the tape has no live consumer until the
eager builder retires); FOLD-2E-C (lazy projection) consumes the tape; FOLD-2E-D
(substrate-manifest) classifies the tape under Lock 1; FOLD-2E-E (NEON) feeds the
tape's `offsets`; FOLD-2E-F (fence) keeps the projection compile-time and severs
the live `arena.rs:47` `StructRegistry::compound_kind_for_layout` coupling by
deletion. No fold implies a parallel substrate, a sidecar producer, or a Lock-1
violation: the `StructuralIndex` mask stream is a transient producer (plane (4)
`local_temp_only`, ARCH `:1804`), never a retained sidecar; the structural-index
`OnceCell` becomes the tape's `offsets` (index IS the tape) or `local_temp_only`,
classified before wiring (U-2E-02); the SoA's `payloads: PayloadArena` is a
declared retained member of the ONE tape (`substrate_target=existing_tape`,
substrate-manifest `retention_lifetime=output_row`, CH5-V1-001); the NEON
classifier primitive declares `retention_lifetime=transient-single-call` (the
primitive enum, no cross-call retained state, CH5-V2-001-cohort-align). The two
`retention_lifetime` vocabularies (substrate-member `:122-123` vs. primitive
`:146-148`) are kept in their distinct row kinds, never conflated. The monotonic
direction is skinny-proven → crates/core (SPEC `:110-114`); no crates/core
construct is relocated into skinny.
