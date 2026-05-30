---
agent: 2b
pass: T-P2-research
cycle: V3
generated_at: 2026-05-29T12:00:00Z
t_p1_inventories_consumed: [1a, 1b, 1c, 1d, 1e, 1f]
primary_sources_cited: 19
techniques_grounded: 11
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted:
    - CH2-V2-2B-FOLD-L1-L2-L3-L4-L5-L7-L8-L9-grammar-neutral-ACCEPT
    - CH5-V2-2B-FOLD-L1-L2-L3-L4-L7-L8-L9-coupling-clean-ACCEPT
    - CH1-V2-2B-citations-resolve-ACCEPT
    - CH3-V2-2B-no-redress-reopen-ACCEPT
    - CH4-V2-2B-admission-cost-realistic-ACCEPT
    - CH6-V2-2B-no-paper-close-ACCEPT
  rejected: []
  revised:
    - CH2-V2-R1-FOLD-L6-mask-only-construction-parameterisation-DOMAIN-not-exercise-set-by-construction-grammar-neutral-by-exercise-unwired
    - CH5-V2-001-FOLD-L5-L6-retention-prose-replaced-with-canonical-retention_lifetime-transient-single-call-enum
  v2_revised_carried_forward:
    - CH1-2B-01-expand-HARDENING-S-P2-V3-full-path-2b118-151-309
    - CH2-V1-R1-FOLD-L9-JSON-witnessed-only-deletion-rejection-clause-recorded-not-shortlisted
    - CH2-V1-R2-FOLD-L8-scope-branch-tag-projection-JSON-CSS-only-not-fleet-wide
    - REVISE-2B-01-FOLD-L2-checkasm-diagnostic-fact-stream-oracle-comparator-only
    - CH4-2b-001-FOLD-L5-reanchor-escape_mask_64-carry-idiom-to-lib.rs
    - CH4-2b-002-FOLD-L6-add-abrogate-measurement-gate-W3-antecedent
    - CH4-2b-003-FOLD-L9-demote-Alt-mode-to-deferred-pending-reprofile-appendix
    - CH5-V1-001-FOLD-L3-declare-PayloadArena-substrate_target-retention-policy_owner
    - CH5-V1-002-FOLD-L8-reword-sparse-flag-substrate_target-existing_tape-not-no-substrate
  first_cycle_additions:
    - PV17-001-PrimitiveKernels-struct-IS-the-Layer1-vocabulary
    - PV17-002-Layer1-realized-in-Rust-not-bbnf.asm-macros
    - PV17-003-x86inc-Layer0-vendored-skinny-only-totality-path-absent
    - PV17-004-checkasm-harness-is-dav1d-discipline-proven
    - PV17-005-eq-set-is-the-one-real-NEON-body-table-prefixxor-scalar-passthrough
    - PV17-006-L5-L6-net-new-masks-grammar-neutral-by-parameterisation
    - PV17-007-Lock16-manifest-row-schema-is-the-fold-lock-surface
    - PV17-008-abstract-primitive-lift-discipline-transfers-process-not-pixels
locks_amendment_candidates: 4
sk_cycle: SK-V17
t_p1_entry_state: CONVERGED-V6-confirm-97.4pct-CH1-V5-001-resolved
host_close_route: Apple-M5-Max-aarch64
master_head: 91b6893b0
---

# T-P2 2B — Primitive-Vocabulary Fold (SK-V17 Totality)

## Executive Summary

The SKINNY-proven primitive vocabulary is already a clean two-layer system, but
its REALIZED shape diverges from the V1 spec's authored shape, and the fold must
reconcile that divergence — not re-derive the spec's `bbnf.asm`-macro story. Layer 0
is the dav1d/x264 `x86inc.asm` macro corpus vendored verbatim at
`skinny/crates/bbnf-simd/ext/x86/x86inc.asm` (diagnostic-only on the aarch64 close
route). Layer 1 is the bbnf-authored grammar-neutral primitive vocabulary — but it
is realized as the Rust `PrimitiveKernels` dispatch struct
(`skinny/crates/bbnf-simd/src/dispatch.rs:50-56`) over per-arch intrinsic modules,
NOT as the `bbnf.asm` Layer-1 macros that `LOCKS.md:474` + `MASTER-PLAN §H.W2.5:622`
authored. The proven dav1d admission discipline IS present and load-bearing: a
`checkasm_*` differential per primitive (`tests/checkasm_*.rs`), each a scalar
reference + `guarded_call` stack-canary + aarch64 callee-saved sentinel guard
(`tests/checkasm_common.rs:50-112`) — the FFmpeg/dav1d process verbatim. The
load-bearing live truth the fold must carry: only ONE Layer-1 body is a real NEON
kernel today — `byte_class_from_eq_set_64_neon` (the CSS-admissible eq-set fan,
`aarch64/byte_class_from_eq_set_64.rs:33-72`) plus the JSON `classify_tbl4` path;
`byte_class_from_table_64_neon` and `bitmap_prefix_xor_64_neon` are 4-LOC scalar
passthroughs (`aarch64/byte_class_from_table_64.rs:3`,
`aarch64/bitmap_prefix_xor_64.rs:3`). The fold's Lock-16 surface is the v+1 primitive
manifest schema (`LOCKS.md:480-489`): the fold-target totality tree
(`crates/simd-scan`) has NO Layer-0 vendoring (`crates/bbnf-simd/ext/x86/` ABSENT)
and a different primitive vocabulary (no `PrimitiveKernels` struct; multi-arch
kernels). The fold therefore is: (1) adopt the proven `PrimitiveKernels` Rust
dispatch shape into `crates/simd-scan`; (2) carry the L1–L9 pool
(S-P2 LOCKED `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:92-251`)
as the totality Lock-16 manifest rows; (3)
reconcile the `bbnf.asm`-macro authored shape — the spec authored a macro layer the
proven path realized in Rust. No new primitive admitted without scalar reference +
strict `BBNF_SIMD_STRICT=1` checkasm + same-wave consumer (Lock 16). aarch64 only;
x86 Layer 0 is diagnostic.

## Technique Grounding Table

(spec-claim / T-P1-divergence-id | published source cited | grounded/refuted/partial | bbnf-specific note)

| spec-claim or T-P1 divergence | primary source cited | verdict | bbnf-specific note |
|---|---|---|---|
| Two-layer vocabulary: Layer 0 vendored ASM, Layer 1 bbnf-authored, one-directional dependency (axis §8.2; `LOCKS.md:474`). | dav1d `x86inc.asm` / FFmpeg x264 macro corpus, vendored at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`; BSD-2 (`MASTER-PLAN.md:622` Layer-0-read-only clause). | grounded | Layer 0 is real on the skinny tree; **diagnostic-only** on the aarch64 close route (x86 macros do not close M5 Max rows). The dependency IS one-way: Rust intrinsic modules + the `ext/x86/bbnf.asm` macro contract include Layer 0, never vice-versa. |
| Layer 1 is the `bbnf.asm` grammar-neutral primitive-macro vocabulary (`MASTER-PLAN §H.W2.5:622`: `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `EOB_PAD_CLAMP`, `BULK_EMIT_POSITIONS_64`). | live `PrimitiveKernels` struct `skinny/crates/bbnf-simd/src/dispatch.rs:50-56`; `prim` module `src/lib.rs:251-295`. | **partial / divergence** | The Layer-1 vocabulary EXISTS but is **realized in Rust** (`PrimitiveKernels` fn-ptr dispatch + per-arch `src/{scalar,aarch64}/*.rs`), NOT as `bbnf.asm` macros consumed by a `CollapsedStage` `.asm`. The six authored macro names map 1:1 to the six `PrimitiveKernels` fields — the vocabulary is identical, the realization medium differs. PV17-002. |
| Admission requires scalar reference + checkasm differential per primitive (Lock 16 `:478,:491-493`; SK-V17 SPEC `:245-246`, `:620`). | dav1d/FFmpeg `checkasm` discipline; live `tests/checkasm_common.rs:33-112` (scalar-vs-candidate + `guarded_call` stack canary + `callee_saved_register_then` x19-x28 sentinels); per-primitive `tests/checkasm_byte_class_from_table_64.rs:13-49` (density sweep + alignment windows). | grounded | The dav1d **process** (not pixel kernels) transferred verbatim: scalar oracle, guarded differential, alignment sweep, register-clobber guard. `BBNF_SIMD_STRICT=1` is the admission gate (`LOCKS.md:491`). This is the Lock-16 admission machinery the fold inherits unchanged. PV17-004. |
| The L1 byte-class classifier is the grammar-neutral structural-scan primitive; alphabet is the only grammar datum (Lock 14; SK-V17 SPEC `:314-317`). | `select_classifier(alphabet: &'static [u8;64])` `dispatch.rs:42`; eq-set fan `aarch64/byte_class_from_eq_set_64.rs:33-72` (four `vld1q_u8` stripes + `vceqq_u8`/`vorrq_u8` reduce); Lemire 2019 "Arbitrary byte-to-byte maps using ARM NEON" (`vqtbl4q_u8`); Lemire 2026 "fastest way to match characters on ARM" (NEON `svmatch_u8` port). | grounded | The eq-set fan IS the Lock-14 vehicle: the only grammar datum is `alphabet`/`set: &[u8]`. CSS uses the eq-set fan NOT the lo6 `classify_tbl4` (the `;{`→slot-59 `& 0x3f` collision, `dispatch.rs:106`). JSON+CSS witnessed; Sheets/BBNF by-construction (Lock 14, proof SK-V18). |
| `byte_class_from_table_64` / `bitmap_prefix_xor_64` aarch64 paths are NEON wins. | live `aarch64/byte_class_from_table_64.rs:3` + `aarch64/bitmap_prefix_xor_64.rs:3` — both 4-LOC scalar passthroughs to `crate::scalar::*`. | **refuted** | Confirmed at master `91b6893b0`: these two are scalar delegates, NOT NEON bodies (matches L1 note `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:108`). The manifest MUST file them `scalar-delegate-non-ASM` (Lock 16 close-state `:507`), never as aarch64 SIMD row movement. PV17-005. |
| L5 `comment_body_mask_64` + L6 `bracket_depth_mask_64` are net-new grammar-neutral masks (S-P2 LOCKED `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:169-199`; SK-V17 SPEC §6 `:594-607,:631-634`). | scalar/checkasm files ABSENT today (`src/scalar/comment*`, `src/scalar/bracket*` confirmed absent); §2 verbatim sketches `p2e:120-129,:155-165`. | grounded (REQUIRED-NEW) | L5 grammar-neutral by digraph parameterisation `(open:[u8;2],close:[u8;2])` — C/Rust/JS/SQL block comments; uses `escape_mask_64` `overflowing_add` carry idiom NOT PMULL (REDRESS-88). L6 sees only masks (open/close abstracted by L1), scalar running-balance default NOT CTZ (REDRESS-89). Both REQUIRE scalar ref + checkasm BEFORE wiring. PV17-006. |
| Lock-16 v+1 primitive manifest: every `core::arch`/`target_feature`/`asm!` use-site maps to a manifest row (`LOCKS.md:480-489`). | manifest schema `LOCKS.md:482-487`; close-states `:506-513`; prior-totality 2B manifest template `restart/audit/totality/p2/2B-primitive-vocabulary.md:144-155`. | grounded | The manifest row IS the fold's Lock-16 surface: {primitive id, abstract-primitive name, ISA/library citation, hardware gate, scalar ref, strict checkasm command, corpus/equality parity, grammar-policy source, substrate target, retention lifetime, policy owner, same-wave consumer, expected row, LOC/risk, rollback, abrogate threshold, disposition}. PV17-007. |
| Abstract primitive lifts transfer the dav1d/ffmpeg *process*, not the pixel kernels (Lock 16 `:476`; T-P2 axis §8.1). | Lock 16 `:476` (motion-comp/IDCT/loop-filter/film-grain do NOT translate; `vextq_u8` cross-lane permute, `udot` MAC, saturating arith, cache hints, mask-FSM DO); msac `cnt/buf/end` refill (`/tmp/dav1d-research/dav1d/src/x86/msac.asm:80-220`). | grounded | The abstract-primitive lift is grammar-neutral by construction: cross-chunk byte-context propagation (`vextq_u8`), byte-window MAC (`udot`, gated — no CSS antecedent §9), branchless overflow-clamped accumulation. Each names an abstract primitive + a per-grammar cost-model selection, never a JSON pin. PV17-008. |
| FSM/frame-stack macros (`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`/`POP`) are admissible Layer-1 vocabulary. | `ext/x86/bbnf.asm:317-473` (prior 2B); no scalar/checkasm/consumer (`restart/audit/totality/p2/2B-primitive-vocabulary.md:75,:153`). | refuted | Source-only contracts; no scalar ref, no checkasm, no consumer. They are the `CollapsedStage` spine — x86/AVX-512-pinned, mechanically refused on aarch64 (UNKNOWN-2D-05, `ARCHITECTURE.md:1206`). Fold files them `source-present-unwired`/`architectural-block-with-REDRESS`; they are NOT an SK-V17/SK-V18 aarch64 admission. |
| The `Vec<u32>` structural index the classifier produces IS a retained sidecar. | `dispatch.rs:42` produces a `Vec<u32>` index; §9 condition (`SPEC:621`, `:648`): the index IS the tape's `offsets`. | refuted (correctly pre-blocked) | The classifier's `Vec<u32>` output is the tape's `offsets` (index == tape identity, Lock 1), NOT a parallel retained vector (REDRESS-53, `SPEC:577,:656`). The primitive produces; the tape consumes once (L4). No sidecar. CH5-clean. |
| The totality fold-target carries the proven `PrimitiveKernels` vocabulary + Layer-0 vendoring. | `crates/simd-scan/src/lib.rs:49-68` (multi-arch kernels, NO `PrimitiveKernels` struct, NO `prim` module); `crates/bbnf-simd/ext/x86/` ABSENT (only `skinny/crates/bbnf-simd/ext/x86/` exists). | **refuted / divergence** | The totality `crates/simd-scan` is a DIFFERENT, BROADER vocabulary (neon+avx2+avx512+wasm+scalar, alphabet-keyed) with no `PrimitiveKernels` fn-ptr dispatch and no Layer-0 vendor dir. The fold must adopt the proven aarch64-narrow `PrimitiveKernels` shape, not the multi-arch `simd-scan` shape (1A SUB17-008, 1E LAC-06). PV17-001/003. |

## Architectural Assertions Defended

### A1 — The two-layer vocabulary is real, but Layer 1 is Rust, not `bbnf.asm`

Layer 0 (`skinny/crates/bbnf-simd/ext/x86/x86inc.asm`) is the dav1d/x264 macro
corpus vendored verbatim, BSD-2, read-only. It is **diagnostic-only** on the
aarch64 close route: x86 ABI/DSP macros do not close M5 Max rows (Lock 16 `:518`;
SK-V17 SPEC `:258` x86 bar). The proven Layer-1 vocabulary is the **Rust**
`PrimitiveKernels` struct (`dispatch.rs:50-56`): five `fn`-pointer fields selected
once per process (`select_primitive_kernels()` `:63-87`), each routing to a
`src/aarch64/*.rs` NEON body or a `src/scalar/*.rs` reference. This is the
load-bearing divergence the fold must reconcile: `LOCKS.md:474` and
`MASTER-PLAN §H.W2.5:622` authored Layer 1 as `bbnf.asm` macros (the macro names
`BYTE_CLASS_FROM_TABLE_64` etc. are spelled in the spec), but the proven path
realized the same vocabulary as Rust intrinsic modules. The vocabulary content is
identical (six primitives, 1:1 with the macro names); only the medium differs. The
fold proposes the Rust `PrimitiveKernels` realization as the totality Layer-1 shape,
with the `bbnf.asm` macros retained as the `CollapsedStage` `.asm` spine ONLY
(x86/AVX-512, host-gated, UNKNOWN-2D-05) — never an aarch64 close path.

### A2 — The dav1d checkasm discipline transferred verbatim and is the admission gate

`tests/checkasm_common.rs` carries the FFmpeg/dav1d process exactly: `guarded_call`
(`:34-39`) wraps every candidate in a 1KB stack canary with xor-fold verification
(`:50-72`); `callee_saved_register_then` (`:85-112`) writes sentinels into x19–x28
via inline `asm!` and asserts the candidate preserved them. Each primitive carries a
per-shape sweep — `checkasm_byte_class_from_table_64.rs` runs a density sweep
(`:13-31`, densities 0..256) and a 64-window alignment sweep (`:33-49`), comparing
`bbnf_simd::prim::byte_class_from_table_64` against
`scalar::byte_class_from_table_64_scalar`. This is the Lock-16 admission machinery
(`:478,:491`); `BBNF_SIMD_STRICT=1` gates close. The fold inherits this harness
unchanged — it is the proven realization of axis §8.1 (scalar oracle + checkasm
differential + same-wave consumer).

### A3 — The L1–L9 candidate pool is the totality Lock-16 manifest

The S-P2 LOCKED pool
(`restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:92-251`)
is the candidate-primitive set the fold carries into the totality Lock-16 manifest. Eight survived CH1–CH7 ACCEPT (L1,
L2, L3, L4, L5, L6, L7, L8); L9 is conditional on a post-CF-1 re-profile. Each is a
manifest row below. The pool is grammar-neutral by construction: every primitive
carries a scalar-ref (or N/A-non-SIMD), a checkasm (or correctness analogue), a
same-wave consumer, and a grammar-neutral verdict.

### A4 — Only the eq-set fan is a real NEON body today; the rest are scalar delegates

Live at `91b6893b0`: `byte_class_from_eq_set_64_neon`
(`aarch64/byte_class_from_eq_set_64.rs:33-72`) is the one real NEON Layer-1 body —
four `vld1q_u8` stripes, per-member `vceqq_u8`/`vorrq_u8` OR-reduce, `movemask_u8x16`
pack. The JSON lo6 path (`classify_tbl4`) is the second. `byte_class_from_table_64`
and `bitmap_prefix_xor_64` aarch64 modules are 4-LOC scalar passthroughs
(`:3` each). The manifest MUST file these `scalar-delegate-non-ASM` (Lock 16 `:507`),
never claim them as NEON row movement — the prior-totality 2B already refuted the
overclaim (`p2/2B:66,:181`) and the live state confirms the refutation holds at
SK-V17 HEAD.

## Architectural Assertions Refuted (load-bearing)

| assertion | refutation | constraint on T-P3 |
|---|---|---|
| The totality fold adopts the multi-arch `crates/simd-scan` vocabulary as Layer 1. | `crates/simd-scan` carries neon+avx2+avx512+wasm+scalar (`lib.rs:55-65`) and NO `PrimitiveKernels`/`prim` module — it is BROADER than the proven aarch64-narrow set and a different dispatch shape. The proven Layer-1 vocabulary is `skinny/crates/bbnf-simd`'s `PrimitiveKernels`. | The fold adopts the PROVEN aarch64 `PrimitiveKernels` shape; the multi-arch `simd-scan` kernels are a scope-narrowing decision (1A SUB17-008 / 1E LAC-06), NOT an admission of x86/avx512 close paths. Lock 16 `:518`. |
| Layer 1 is `bbnf.asm` macros, per the V1 spec. | The proven Layer 1 is Rust `PrimitiveKernels`; the `bbnf.asm` macros are unconsumed contracts (no aarch64 body, no checkasm, no consumer — `p2/2B:75,:153`). | T-P3 must reconcile `LOCKS.md:474` / `MASTER-PLAN §H.W2.5:622` against the realized Rust vocabulary. The macro layer survives ONLY as the x86/AVX-512 `CollapsedStage` spine (host-gated, UNKNOWN-2D-05), never aarch64 Layer 1. |
| The Layer-0 `x86inc.asm` is vendored in the totality tree. | `crates/bbnf-simd/ext/x86/` is ABSENT; the vendor dir exists ONLY at `skinny/crates/bbnf-simd/ext/x86/`. Lock 16 `:474` + MASTER-PLAN cite the totality path `crates/bbnf-simd/ext/x86/x86inc.asm` which does not exist. | The fold either vendors Layer 0 into the totality tree (if x86 CollapsedStage is ever pursued — G-Omega-gated, UNKNOWN-2D-05) or re-anchors Lock 16 `:474` to the skinny path. The aarch64 close route needs no Layer 0. |
| `byte_class_from_table_64`/`bitmap_prefix_xor_64` NEON bodies are SIMD wins. | 4-LOC scalar passthroughs (`aarch64/*.rs:3`). | These rows close `scalar-delegate-non-ASM` (Lock 16 `:507`); any future NEON body is a separate wave with its own scalar-ref + checkasm + consumer + row movement. No fold-time NEON claim. |
| FSM/frame-stack macros admit as Layer-1 vocabulary. | Source-only `ext/x86/bbnf.asm:317-473`; no oracle/checkasm/consumer; the `CollapsedStage` spine is x86-pinned + aarch64-refused (`ARCHITECTURE.md:1206`). | These are NOT an SK-V17/SK-V18 aarch64 admission. Fold close-state: `source-present-unwired` or `architectural-block-with-REDRESS`. |
| `udot`/i8mm digit-block MAC admits for CSS via the Lock-16 allowlist. | No benched CSS antecedent (SK-V17 SPEC §6 pre-block `:655`: "the orphan udot/i8mm digit kernel — no benched CSS antecedent, §9"). | The `udot` abstract-primitive (Lock 16 `:458`) is grammar-neutral and admissible for number-heavy grammars (JSON number, future Sheets), but admission requires a same-wave consumer with a profiled antecedent — CSS has none. No orphan kernel. |

## The Fold — Candidate / Primitive Enumeration (load-bearing)

Each: **shape** · **T-P1-divergence antecedent** · **grammar-neutral verdict** · **lock surface**.
The pool is the S-P2 LOCKED L1–L9
(`restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:92-251`);
each row also names its
`PrimitiveKernels` realization + Lock-16 close-state. aarch64 only.

### FOLD-L1 — Block-wide byte-class structural classifier (eq-set fan)
- **Shape.** `select_classifier(alphabet: &'static [u8;64])` (`dispatch.rs:42`) → `Vec<u32>`
  structural index; CSS-admissible backend is `byte_class_from_eq_set_64_neon`
  (`aarch64/byte_class_from_eq_set_64.rs:33-72`, four `vld1q_u8` + `vceqq_u8`/`vorrq_u8`
  reduce, `set.len()<=8`); JSON path is `classify_tbl4` lo6 table. Alphabet is the only
  grammar datum.
- **T-P1 antecedent.** Divergence E (1A SUB17-007/008, 1E D-1E-SKV17-06): core
  `StructuralAlphabet` is richer config (digraphs/quote-classes) than proven `[u8;64]` —
  generality is config-breadth, JSON/CSS-exercised. The fold carries the proven aarch64
  `select_classifier` shape into `crates/simd-scan`, narrowing the multi-arch breadth.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (JSON+CSS-witnessed); `alphabet` is the
  Lock-14 vehicle (`SPEC:314-317`). Sheets/BBNF by-construction, proof SK-V18.
- **Lock surface.** Lock 16 manifest row {abstract: block-byte-class classify; ISA: Lemire
  2019 `vqtbl4q_u8` + Lemire 2026 NEON `svmatch_u8` port; scalar-ref:
  `src/scalar/byte_class_from_eq_set_64.rs`; checkasm: `tests/checkasm_byte_class_from_eq_set_64.rs`
  strict; gate: aarch64 NEON; consumer: L2 tape build same-wave; substrate-target:
  `existing_tape` (index IS offsets); disposition: `wired`}. Lock 14 (alphabet-as-data).

### FOLD-L2 — Tape-append materialization op (`push_plain_offset`)
- **Shape.** `TapeBuilder` Open/Close/Leaf appends — one branchless u32 write into
  `offsets: Vec<u32>` (`assembler.rs:71`), no StructRegistry indirection; retires the
  eager `OpenFrame`/fact-stream plane.
- **T-P1 antecedent.** Divergence A+B (1A SUB17-002/003, 1D SK17L-001/003): retire the
  eager `OpenFrame` builders (`css_l4/builder.rs:16`), wire the flat tape append. The
  per-leaf hot path must NOT re-enter the `StructRegistry::layout` lookup (Divergence F
  fence, 1A SUB17-009, 1E D-1E-SKV17-03).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — `TapeBuilder` carries no grammar-keyed
  field; which positions push is `BackendRule`-derived data.
- **Lock surface.** Lock 1 (substrate-union: append into the one tape, no `Vec<OpenFrame>`
  clone). Lock 16 {scalar-ref: N/A-substrate-op; checkasm: tape ↔ **diagnostic** fact-stream
  corpus-parity (the fact-stream as oracle/comparator ONLY — `substrate_target =
  admitted_fact_output` per LAC-1E-14, `LOCKS.md:102-105`; the diagnostic
  `emit_full_parse`/`CssFullParseSummary` global block `SPEC:799-800`; NOT a live admission
  plane, NOT re-admitted into the one tape) + cssparser 8-field equality +
  `PayloadArena.write_count==0` on re-readable leaves; consumer: L3 same-wave; substrate-target:
  `existing_tape`; disposition: `wired`}.

### FOLD-L3 — Grammar-parametric lazy `ValueRef<G>` projection
- **Shape.** Reconstruct typed CSSOM on demand via a `ValueRef` cursor over the SAME `Tape`,
  isomorphic to JSON `value_from_ref` (`json/value.rs:143`); kind recovered from the source
  byte at the offset (no stored tag); `PayloadArena` the bounded escape hatch for
  irreducible scalars only. `ValueRef<'doc,'input,K,G: EventGrammar>` (`tape/mod.rs:175`).
- **PayloadArena substrate declaration (CH5-V1-001).** `PayloadArena` is NOT a sidecar — it
  is a RETAINED member of the one SoA `Tape` (`tape/mod.rs:99` `payloads: PayloadArena`,
  verified live; constructed `:109`/`:117`), so per `LOCKS.md:118-127` it MUST carry a full
  classification: `substrate_target = existing_tape` (a member of the one tape, not a parallel
  vector), `retention_lifetime = output_row` (lives the tape's lifetime, read on demand by the
  `ValueRef` cursor), `policy_owner = generated_grammar` (the `@generated by xtask regen-*`
  emitter decides which leaves spill). Bounded by the
  `PayloadArena.write_count == 0`-on-re-readable-leaves invariant (FOLD-L2 checkasm): a leaf
  whose bytes are recoverable from the source span at its offset NEVER writes the arena. Implicit
  retention here would be the parallel-substrate hole Lock 1 forbids; the explicit declaration
  closes it (coupling-clean-by-manifest, not by-assertion).
- **T-P1 antecedent.** Divergence C (1A SUB17-004, 1D SK17L-002, 1E D-1E-SKV17-02): core
  value API is per-grammar EAGER generated enums (`css_l4/value.rs:414`), not lazy
  `ValueRef<G>`. The fold acts on the GENERATOR (the `@generated by xtask regen-*` path,
  Lock-14 ALLOWED — 1A V5 fold) to emit the lazy projection. preserve-rich-ast: the
  `ValueRef` is lazy, not flattened, not eager.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL by construction (type-witnessed): `ValueRef`
  is generic over `G`; JSON and CSS instantiate the SAME cursor type. Sheets/BBNF SK-V18.
- **Lock surface.** Lock 1 (one substrate, the view rides the tape). Lock 14 (`ValueRef<G>`
  generic, no grammar pin). The `FieldSource` walk is COMPILE-TIME projection-emission
  resolved once (Divergence-F fence), NEVER a per-leaf `StructRegistry` lookup. Lock 16
  {scalar-ref: N/A-cursor-read (`value_from_ref` is the JSON reference); checkasm: cssparser
  8-field equality round-trip; disposition: `wired`}.

### FOLD-L4 — Tokenize-once shared-scan reuse
- **Shape.** Consume L1's structural index ONCE via a per-grammar template; the index IS the
  tape (Lock 1), no parser-local second cursor. Eliminates the 2–3× byte re-walk.
- **T-P1 antecedent.** Divergence E sidecar fence (1A firewall row, 1E REDRESS-53 ledger):
  the `OnceCell<StructuralIndex>` in all 8 generated parsers must become the tape's `offsets`
  (index IS tape) OR `local_temp_only` (`LOCKS.md:119-126`), never a retained parallel
  vector (REDRESS-53, `SPEC:577,:656`). Scope to ALL 8 carriers (1E LAC-03).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — generic reuse pattern, per-grammar template;
  which bytes index is grammar data. Bounded to the index==tape-offsets identity (§6).
- **Lock surface.** Lock 1 (single substrate, REDRESS-53 pre-block). Lock 16 {scalar-ref:
  N/A-consumption-pattern; checkasm: cssparser equality (output-invariant under reuse);
  substrate-target: `existing_tape`; disposition: `wired`}.

### FOLD-L5 — `comment_body_mask_64` (NET-NEW suppressor mask)
- **Shape.** Transient `u64` body mask suppressing comment-region bytes, digraph-parameterised
  `(open:[u8;2], close:[u8;2])`; region fill reuses the `escape_mask_64` `overflowing_add`
  carry idiom (live scalar at `skinny/crates/bbnf-simd/src/lib.rs:175`, the carry at `:188,:190`
  — NO `src/scalar/escape_mask_64.rs` sibling exists; the carry idiom is `lib.rs`-resident) NOT
  PMULL; AND-NOTed into the L1 index. 1-bit carry within one block sequence.
- **T-P1 antecedent.** Divergence E NEON-vocabulary (1E L16 manifest); net-new — no
  `src/scalar/comment*` today. Comment-skip arm of the ~69% scan leaf (S-P1).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL by digraph parameterisation (C/Rust/JS/SQL
  block comments); no CSS pin.
- **Lock surface.** Lock 16 {abstract: digraph-region suppressor; ISA: the `escape_mask_64`
  scalar carry idiom (`skinny/crates/bbnf-simd/src/lib.rs:175`, `overflowing_add` at `:188,:190`)
  / `vqaddq_u8` NEON analogue, NOT PMULL (REDRESS-88); scalar-ref: REQUIRED-NEW
  `src/scalar/comment_body_mask_64.rs` (absent at `91b6893b0`; verified — `comment_body_mask_64`
  scalar + checkasm genuinely do not exist today, the REQUIRED-NEW classification is honest);
  checkasm: REQUIRED-NEW `tests/checkasm_comment_body_mask_64.rs`; consumer: L1 composition
  same-wave; `retention_lifetime = transient-single-call` (CH5-V2-001 fold — the canonical Lock 1
  v+1 enum value, `LOCKS.md:146-148`; the carry is a 1-bit overflow within one block sequence,
  the descriptive gloss "within block sequence" is single-call-bounded per its own body);
  disposition: `wired` if W3 lands, else `deleted`}.
  Scalar ref + checkasm BEFORE wiring (Lock 16 `:491`).

### FOLD-L6 — `bracket_depth_mask_64` (NET-NEW depth-balance mask)
- **Shape.** Transient interior mask over open/close MASKS (already abstracted from bracket
  bytes by L1 — sees masks, never literal bracket bytes); SHIPPED body is a scalar running
  balance over the two precomputed masks with i32 `depth_carry` threaded WITHIN a single
  `scan_components_to_index` call, init-0-per-parse, NEVER retained across calls. CTZ refinement
  is consumer-only + parity-gated, NOT the default.
- **T-P1 antecedent.** Divergence E + Lock-1 v+1 ELEVATION (1E `:145`): no cross-call retained
  classifier/carry state (`LOCKS.md:137-149`). `consume_balanced_at` recursion (S-P1).
- **Grammar-neutral verdict (CH2-V2-R1 fold).** GRAMMAR-NEUTRAL by **mask-only construction** —
  the primitive sees only the L1-abstracted open/close masks, never literal bracket bytes, so it
  carries zero grammar datum (the strongest Lock-14 form, stronger than alphabet-as-data). The
  nested-balance pattern parameterises over any bracket-pair grammar; the JSON arrays/objects,
  CSS component blocks, BBNF `()`/`[]`, Sheets parens enumeration is the **parameterisation
  DOMAIN, NOT an exercise set** — L6 is REQUIRED-NEW with no wired consumer at HEAD
  (`src/scalar/bracket_depth_mask_64.rs` absent at `91b6893b0`, zero wired consumer): it is
  by-construction grammar-neutral, by-exercise unwired pending the W3 abrogate gate
  (`disposition: wired` if W3 lands AND the abrogate gate clears, else `deleted`). This matches
  sibling L5's parameterisation-domain framing ("no CSS pin", `2b` FOLD-L5), not a fleet-wide
  exercise claim.
- **Lock surface.** Lock 16 {abstract: nested-balance depth; ISA: scalar running-balance default
  (REDRESS-89 bars CTZ default body); scalar-ref: REQUIRED-NEW `src/scalar/bracket_depth_mask_64.rs`;
  checkasm: REQUIRED-NEW `tests/checkasm_bracket_depth_mask_64.rs`;
  `retention_lifetime = transient-single-call` (CH5-V2-001 fold — the canonical Lock 1 v+1 enum
  value, `LOCKS.md:146-148`; the `depth_carry` is init-0-per-parse, threaded WITHIN a single
  `scan_components_to_index` call and NEVER retained across calls, so the descriptive gloss
  "within `scan_components_to_index` call" is single-call-bounded per its own body — NOT
  `retained-within-chunk`); **abrogate threshold** (PV17-007 schema mandate, CH4-2b-002): DELETE this
  REQUIRED-NEW primitive if, on a profiled W3 antecedent, the scalar running-balance body does
  NOT move the `consume_balanced_at` arm of the ~69% scan leaf (S-P1) by a measurable margin —
  i.e. the REQUIRED-NEW LOC/checkasm cost is bound to a measured self-time reduction, no
  orphan-kernel admission; disposition: `wired` if W3 lands AND the abrogate gate clears, else
  `deleted`}.

### FOLD-L7 — One-shot SIMD capacity reservation
- **Shape.** `CapacityPlan::OneShotSimd` sizes the EXISTING `offsets` vector from the L1 scan
  count (`scan_structurals(src).positions().len()+8`) in one cold `Vec::reserve`; no second
  vector, no per-corpus capacity literal.
- **T-P1 antecedent.** Divergence A alloc floor (1D SK17L-001 / L-SK17-05 FNV/fixture-overfit
  fence): scratch sizes from `input.len()` + BackendRule shape, never per-corpus.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — `CapacityPlan` grammar-free; the count reuses
  the shared classifier with the CSS alphabet as the only datum.
- **Lock surface.** Lock 16 {scalar-ref: the consumed `scan_structurals_scalar` count; checkasm:
  the L1 classifier's existing differential; consumer: L2 (the tape it sizes), gated behind
  L2/L3 + NEON scan; disposition: `wired`}. Lock 8 (no per-corpus literal).

### FOLD-L8 — Sparse-flag side-table
- **Shape.** Opaque flag bits in the EXISTING `flag_cursors`/`flag_values` sparse pair
  (`tape/mod.rs:97-98`, `flags_at` binary-search) — NOT a new vector, NOT a widened
  per-position record, NOT a dense parallel column; paid only where non-zero. L3's
  kind-disambiguation mechanism.
- **T-P1 antecedent.** Divergence C value-API support; Lock-14 guard (1D SK17L-002): each flag
  bit MUST be a `BackendRule` branch-tag projection, NOT a hand-curated per-rule catalogue
  (else it relocates `W5C_REQUEST_FACT_PROFILES` into flag form → CH2 REVISE).
- **Grammar-neutral verdict.** GENERALISABLE-WITH-GUARD — flag = `BackendRule` branch-tag
  projection only. **Scoping (CH2-V1-R2):** the branch-tag projection is EXERCISED on JSON+CSS
  ONLY — the kind-disambiguation rides `ValueRef<G>` (JSON+CSS by-exercise; `sheets_witness` is
  a 24-LOC stub). Sheets/BBNF are by-construction, proof SK-V18; this row may NOT claim
  fleet-wide generality (`LOCKS.md:382-387` — with only one of Sheets/BBNF the claim is scoped
  to the witnessed grammars). Generality here is config-breadth, value-plane-exercised JSON+CSS.
- **Lock surface.** Lock 1 — the sparse pair `flag_cursors`/`flag_values` (`tape/mod.rs:97-98`,
  verified live) PRE-EXISTS and is part of the one substrate: `substrate_target = existing_tape`
  (a member of the one tape, not a SECOND substrate), `retention_lifetime = output_row` (retained
  for the tape's lifetime), paid ONLY where non-zero via `flags_at` binary-search (CH5-V1-002 —
  this is not "no substrate"; it adds no SECOND substrate). Lock 14 (the branch-tag guard, scoped
  JSON+CSS). Lock 16 {scalar-ref: N/A-substrate-op; checkasm: corpus-parity analogue (round-trips
  with L2); consumer: L3 same-wave; substrate-target: `existing_tape`; disposition: `wired`}.

The wired FOLD-Ln enumeration ends at L8. FOLD-L9 below is DEFERRED — it has no live
consumer on the LOCKED profile and is recorded-not-shortlisted; it is carried in the
deferred appendix (not the wired enumeration) so T-P3 does not read it as shortlisted.

## Deferred-Pending-Reprofile Appendix (NOT wired; CH4-2b-003 demotion)

### FOLD-L9 — Commit-by-construction Alt-mode (DEFERRED — no live consumer)
- **Status.** DEFERRED-PENDING-REPROFILE — demoted from the wired FOLD-Ln enumeration
  (CH4-2b-003): there is NO live consumer on the LOCKED profile (orphan-kernel risk). The
  conditional gating is correct; the demotion only prevents T-P3 from reading L9 as shortlisted.
- **Shape.** Codegen property: emit NO speculative checkpoint for pure-lexical keyword-dispatch
  Alts depositing nothing structural; the spine commits as it scans, driven by the L1 index;
  rides D3's O(1) `offsets.len()` checkpoint / `truncate` rollback (no `split_off`, no
  `Vec<Vec>`).
- **T-P1 antecedent.** Recognition-control loop (S-P1); CONDITIONAL on a post-CF-1 re-profile
  (N≥50) surfacing the recognition-control loop or a speculative-rollback leaf as top-N
  self-time. No live consumer on the LOCKED profile.
- **Grammar-neutral verdict (CH2-V1-R1).** JSON-WITNESSED-ONLY codegen property derived from
  `BackendRule` Alt shape; not CSS-keyed. Per Lock 14 v+1 (`LOCKS.md:423-425`), a JSON witness
  alone does not establish grammar-neutrality — generality requires ≥1 non-JSON consumer OR a
  recorded measured deletion/rejection. This row is therefore admitted under the
  **deletion/rejection clause** (it carries a measured-rollback abrogate gate, below), NOT as a
  grammar-neutral fold; its disposition is **recorded-not-shortlisted** until the post-CF-1
  re-profile surfaces a non-JSON consumer.
- **Lock surface.** Lock 16 {scalar-ref: N/A-codegen-control-flow; checkasm: recognizer-output
  equality with/without the Alt-mode pass (byte-identical tape); consumer: GATED on the
  re-profile (none today); disposition: `recorded-not-shortlisted` — `wired` ONLY if the
  re-profile fires AND a non-JSON consumer surfaces; else `deleted` under the Lock 14 v+1
  deletion/rejection clause}.

## Open Research Questions

| id | UNKNOWN | verify_action |
|---|---|---|
| OQ-2b-01 | Does the fold adopt the proven Rust `PrimitiveKernels` dispatch shape into `crates/simd-scan`, or retain the totality multi-arch `scan_structural` dispatch and graft the aarch64 narrow set onto it? | T-P3 compares `dispatch.rs:50-87` (`PrimitiveKernels` fn-ptr select) against `crates/simd-scan/src/lib.rs:80-114` (cfg-gated per-arch dispatch); name the convergence-target dispatch shape. The narrow aarch64 set is the proven path; the multi-arch breadth is impl-exceeds-spec. |
| OQ-2b-02 | Is the `bbnf.asm` Layer-1 macro layer (`LOCKS.md:474`, `MASTER-PLAN §H.W2.5:622`) retained as the x86 `CollapsedStage` spine ONLY, or fully retired in favour of the Rust realization? | T-P3 disposition: the macro layer survives only if a host-gated x86 `CollapsedStage` wave is ever pursued (UNKNOWN-2D-05, G-Omega-gated). The aarch64 close route uses Rust `PrimitiveKernels`, never the macros. |
| OQ-2b-03 | Does the totality tree vendor Layer 0 (`crates/bbnf-simd/ext/x86/x86inc.asm`, ABSENT today), or does Lock 16 `:474` re-anchor to the skinny path `skinny/crates/bbnf-simd/ext/x86/`? | T-P3 re-anchor the Lock-16 `:474` + MASTER-PLAN citation; the aarch64 close needs no Layer 0, so vendoring is gated on the x86 CollapsedStage decision (OQ-2b-02). |
| OQ-2b-04 | When do `byte_class_from_table_64`/`bitmap_prefix_xor_64` graduate from scalar-delegate to real NEON bodies, if ever? | A future wave with scalar-ref (present) + a real NEON body + strict checkasm + same-wave consumer + row movement; the lo6 table path is JSON-only (CSS uses eq-set), so the table NEON body has no CSS antecedent — likely stays scalar-delegate through SK-V18. |

## LOCKS-AMENDMENTS-CANDIDATE

Candidates only; disposition is T-P3 3C, G3-gated; merge is Pass Omega.
Scanned axes: Lock 16 (manifest schema, Layer-0/Layer-1 separation, realization medium,
scalar-delegate close-state, abstract-primitive lift), Lock 14 (alphabet-as-data, flag
branch-tag guard), Lock 1 (index==tape, no sidecar). Four candidates.

| id | target lock / surface | proposed candidate text | supporting evidence | loc/risk/wave_hint |
|---|---|---|---|---|
| LAC-2b-SKV17-01 | Lock 16 / MASTER-PLAN §H.W2.5 | Reconcile the Layer-1 realization medium: the proven Layer-1 vocabulary is the Rust `PrimitiveKernels` struct (`dispatch.rs:50-56`), NOT the `bbnf.asm` macros (`LOCKS.md:474`, `MASTER-PLAN:622`). The six authored macro names map 1:1 to the `PrimitiveKernels` fields; record the Rust realization as the aarch64 Layer-1 shape and scope the `bbnf.asm` macros to the x86 `CollapsedStage` spine only (host-gated, UNKNOWN-2D-05). | `skinny/crates/bbnf-simd/src/dispatch.rs:50-56`; `LOCKS.md:474`; `MASTER-PLAN.md:622`; macro contracts `ext/x86/bbnf.asm`. | doc/lock-text + manifest; medium; T-P3 3C + SK-V18 fold. |
| LAC-2b-SKV17-02 | Lock 16 `:474` | Re-anchor the Layer-0 vendor citation: `x86inc.asm` is vendored at `skinny/crates/bbnf-simd/ext/x86/`, NOT the cited totality path `crates/bbnf-simd/ext/x86/` (ABSENT at master `91b6893b0`). Totality vendoring is gated on the x86 `CollapsedStage` decision; the aarch64 close route needs no Layer 0. | `find x86inc.asm` → `skinny/crates/bbnf-simd/ext/x86/x86inc.asm` only; `crates/bbnf-simd/ext/x86/` ABSENT; `LOCKS.md:474`. | doc-only re-anchor; low; T-P3 3C. |
| LAC-2b-SKV17-03 | Lock 16 `:506-513` | Affirm `scalar-delegate-non-ASM` as the close-state for `byte_class_from_table_64`/`bitmap_prefix_xor_64` (4-LOC passthroughs at `aarch64/*.rs:3`); the manifest must NOT file them as NEON row movement. The one real Layer-1 NEON body is `byte_class_from_eq_set_64_neon` + the JSON `classify_tbl4` path. | `aarch64/byte_class_from_table_64.rs:3`, `aarch64/bitmap_prefix_xor_64.rs:3`; `aarch64/byte_class_from_eq_set_64.rs:33-72`; `LOCKS.md:507`. | manifest rows; low; SK-V18 manifest. |
| LAC-2b-SKV17-04 | Lock 16 manifest schema | Carry the S-P2 LOCKED L1–L9 pool (`restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:92-251`) as the totality Lock-16 primitive-manifest rows verbatim; each row {primitive id, abstract name, ISA/library citation, hardware gate, scalar ref, strict checkasm command, substrate-target ∈ {local_temp_only, existing_tape, direct_sink, admitted_fact_output}, same-wave consumer, disposition}. L5/L6 are REQUIRED-NEW (scalar-ref + checkasm absent today). | `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:92-251`; `LOCKS.md:480-489,:528-530`; SK-V17 SPEC §6 `:594-607`. | manifest population; medium; SK-V18 fold. |
