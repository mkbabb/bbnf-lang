---
agent: 1F
pass: T-P1-excavation
cycle: V5
generated_at: 2026-05-30T17:00:00Z
spec_surfaces_audited:
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/skinny/tranches/sk-v17/SPEC.md
  - restart/skinny/tranches/sk-v17/research/p2/p2d-substrate-tape.md
  - restart/skinny/tranches/sk-v17/research/p2/p2f-grammar-neutral.md
  - restart/skinny/tranches/sk-v17/research/p3/p3f-spec-draft.md
  - restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md
files_audited_count: 28
live_truth_method: "wc -l + rg/grep path:line over restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md, restart/skinny/tranches/sk-v17/{SPEC.md,research/}; Read over crates/core/src/runtime/tape/{mod,record}.rs, crates/core/src/runtime/{json,css_l4}/builder.rs, crates/core/src/runtime/css_l4/view.rs, crates/ir/src/registry/struct.rs, crates/simd-scan/src/lib.rs; grep over crates/core/src/grammar/generated/*.rs + crates/core/src/backend/rust/emitter/shapes/substrate.rs + skinny/crates/runtime/src/tape/mod.rs + skinny/crates/bbnf-simd/src/dispatch.rs; LOCKS.md:75 re-read verbatim for the offset-tape-admissible vs columnar-SoA-buried split; no cargo/build mutation"
prior_cycle_dispositions_folded:
  accepted:
    - CH5-S4-5-shape-canon-aarch64-NEON-absorption-under-4-LLVM-shapes
    - CH5-S5-exactly-one-encoding-post-fold-transient-only
    - CH5-S6-monotonic-direction-forbidden-name-collision
    - CH7-1f-coherence-COH17-005-008-overfit-clean
    - V4-CH7-1f-coherence-COH17-001-008-3A-SoA-fold-clean-ACCEPT
    - V4-CH1-V4-001-math.rs-not-a-COH17-002-value-api-carrier-no-leak-in-1f-coherence
  rejected: []
  revised:
    - CH7-S3-A-COH17-001-offset-tape-admissible-vs-columnar-SoA-buried
    - CH7-S3-A-U-COH17-002-offset-tape-admissible-vs-columnar-SoA-buried
    - CH1-V3-001-class-COH17-002-view.rs-path-qualified-css_l4-view.rs:44
  first_cycle_additions:
    - COH17-001-tape-shape-AoS-vs-SoA
    - COH17-002-value-api-per-grammar-vs-ValueRefG
    - COH17-003-tape-unwired-vs-scan-wired-split
    - COH17-004-backendshape-x86-collapsedstage-vs-aarch64-neon
    - COH17-005-simd-scan-multiarch-vs-aarch64-only
    - COH17-006-StructLayout-name-retired-but-live-in-core
    - COH17-007-substrate-manifest-5th-factstream-vs-skv17-no-factstream-admission
    - COH17-008-select-classifier-narrow-vs-StructuralAlphabet-wide
divergence_count:
  spec_claims_implemented: 1
  spec_claims_unimplemented: 4
  impl_exceeds_spec: 2
  unknown: 1
locks_amendment_candidates: 0
---

## Executive Summary

SK-V17 T-P1 1F excavates the tape/substrate/value-API/BackendShape/NEON surfaces
of the TOTALITY V1 spec against the crates/core fold target, grounded in the
SKINNY-proven model (SK-V17 SPEC §0–§2, master `445925167`). The central coherence
fact is a NAMING + SHAPE bifurcation between the two trees that the totality fold
must reconcile: the SKINNY-proven substrate is `Tape<'input>` SoA (`offsets:
Vec<u32>` + sparse `flag_cursors`/`flag_values` + `PayloadArena`) with the
grammar-parametric `ValueRef<'doc,'input,K,G>` (`skinny/crates/runtime/src/tape/mod.rs:94`,
`:175`); the crates/core fold target is `TapeStructBuilder` with a 16-byte AoS
`TapeRec` and `TapeCursor` and NO `ValueRef<G>`
(`crates/core/src/runtime/tape/mod.rs:58`, `record.rs:103`, `cursor.rs`). The
crates/core tape is empirically UNWIRED — the live JSON/CSS parse paths use the
eager `JsonStructBuilder`/`CssStructBuilder` with `OpenFrame` enums + `pending_*`
Vecs (`crates/core/src/runtime/json/builder.rs:9`, `css_l4/builder.rs:16`,
`json/parse_with.rs:34`), exactly the SK-V17 §0.1.11 "UNWIRED tape, SK-V18 fold"
posture. By contrast crates/core's SIMD structural scan IS wired and
already grammar-general + multi-arch (`scan_structural(input, &StructuralAlphabet)`
over neon/avx2/avx512/wasm/scalar, `crates/simd-scan/src/lib.rs:80`,
consumed by ALL 8 generated parsers — json.rs:732, css_l4.rs:15982, ebnf.rs:1381,
bnf.rs:848, csv.rs:566, css_pretty.rs:1905, google_sheets.rs:3559, bbnf.rs:4843).
The §7.3 BackendShape
canon's `CollapsedStage` is x86/AVX-512-pinned (`ARCHITECTURE.md:1109`, `:1186`,
`:1206`) where SK-V17 is aarch64-NEON-only and bars x86/AVX/SVE (SPEC §1, §9).
No new substrate, BIR variant, or shape is proposed; divergences are catalogued
for T-P2/T-P3.

## Spec-Claim ↔ Implementation Table

| ID | Spec / corpus claim (path:line) | Live / counter-surface evidence (path:line) | Verdict | Divergence class | Note |
|---|---|---|---|---|---|
| COH17-001 | ARCH Lock-1 union says "if structural offsets are retained, the structural projection IS the tape" and the five BackendShape variants project the substrate (`restart/ARCHITECTURE.md:1088`); SK-V17 names the proven substrate as the SoA `Tape` with `offsets: Vec<u32>` + sparse flags + `PayloadArena` (`restart/skinny/tranches/sk-v17/SPEC.md:51-53`, `:229-235`; `restart/skinny/tranches/sk-v17/research/p2/p2d-substrate-tape.md:34-49`). | crates/core fold target is `TapeStructBuilder` over a 16-byte AoS `TapeRec` (`crates/core/src/runtime/tape/mod.rs:58`, `record.rs:103,120`), NOT the proven SoA `Tape<'input>` (`skinny/crates/runtime/src/tape/mod.rs:94`). The crates/core mod doc itself states "kept AoS first … the same TapeCursor API rides a later SoA split" (`crates/core/src/runtime/tape/mod.rs:6-9`). | spec-surface-vs-core shape drift | unimplemented | Two different tape encodings (AoS core vs SoA skinny-proven); the fold must converge to EXACTLY ONE post-SK-V18 per Lock 1's "parallel substrates are dead" (`LOCKS.md:75`). **The proven SoA `Tape` is the Lock-1-ADMISSIBLE offset tape, NOT the buried columnar SoA.** `LOCKS.md:75` carries BOTH clauses: (a) admissibility — "The projection may be an offset tape, event tape, or collapsed-stage event sink"; the proven `Tape`'s `offsets: Vec<u32>` IS that offset tape (`ARCHITECTURE.md:1088` "if structural offsets are retained, the structural projection IS the tape"); its `flag_cursors`/`flag_values` are position-keyed SPARSE side-vectors (`skinny/crates/runtime/src/tape/mod.rs:96-98`), not a dense class column; and (b) the dead shape — "columnar SoA designed in AV.04 archaeology but never activated … Columnar SoA stays buried" names the AV.04 DENSE class-column SoA, a DIFFERENT construct from the proven offset-tape. So "adopt proven SoA" is the admissible offset-tape, not a resurrection of the buried class-column. A dual encoding is a transient fold-state, NOT a permissible end-state (see divergence below + U-COH17-002). NOT a parallel substrate within either tree — a cross-tree shape mismatch. |
| COH17-002 | SK-V17 W2 generalizes the value-API to one `BackendRule`-walking projection generator emitting `document/value/view/visitor`, isomorphic to JSON's `value_from_ref` over the EXISTING `Tape`/`ValueRef`, grammar-parametric `ValueRef<…,G>` (`restart/skinny/tranches/sk-v17/SPEC.md:58-62`, `:533-541`; `restart/skinny/tranches/sk-v17/research/p2/p2f-grammar-neutral.md:30-40`; skinny `value_from_ref` at `skinny/crates/runtime/src/grammars/json/value.rs:143`). | crates/core has per-grammar GENERATED (`@generated by xtask regen-*`) value/view modules (`crates/core/src/runtime/css_l4/value.rs:414` `pub enum CssTypedValue<'p>`, `crates/core/src/runtime/css_l4/view.rs:44` `pub struct CssChildrenIter<'a,'p>` — both real symbols at their cited lines; `crates/core/src/runtime/json/value.rs` has NO `value_from_ref`/`ValueRef` — grep returns empty). The crates/core value API is NOT a single `ValueRef<G>` lazy projection; it is per-grammar eager typed enums fed by `OpenFrame` builders. | per-grammar value-API vs lazy-ValueRef<G> generalization | unimplemented | The §0.1.11 fold target: crates/core's value layer is eager-per-grammar (GENERATED, not hand-written — the fold acts on the GENERATOR, per CH1-V3-002/003), not the lazy `ValueRef<G>` projection the SKINNY path proves. |
| COH17-003 | SK-V17 §0.1.11 states the unified tape/layout/projection is structured so the TOTALITY tree (`crates/core/src/runtime/tape/`) can adopt it in SK-V18; the tape there is UNWIRED (`restart/skinny/tranches/sk-v17/SPEC.md:110-114`). | Confirmed: `TapeStructBuilder` is referenced NOWHERE outside `crates/core/src/runtime/tape/` (grep over crates/core empty); live JSON parse uses `JsonStructBuilder::new()` (`crates/core/src/runtime/json/parse_with.rs:34,44`); CSS uses `CssStructBuilder` with `OpenFrame`+`pending_*` (`crates/core/src/runtime/css_l4/builder.rs:16,66-79`). Yet the SIMD scan IS wired into generated parsers (`crates/core/src/grammar/generated/json.rs:732` `scan_structural`). | tape-unwired + scan-wired split | spec-claims-implemented (UNWIRED confirmed as stated) | The fold target's tape is dead code while its structural scan is live — an asymmetry T-P2 must reconcile: the index exists but no tape consumes it; the eager builders consume neither. |
| COH17-004 | §7.3 BackendShape canon: `CollapsedStage` "fuses mask-state and emission for AVX-512-class hardware" (`restart/ARCHITECTURE.md:1088`), lowers to `skinny/crates/bbnf-simd/src/x86_64/{grammar}_collapsed.asm` (`:1186`), CSP binds `admits_collapsed_stage` to `target.arch == x86 + avx512bw` (`:1151`, `:1171`, `:1206`). | SK-V17 is aarch64-only and EXPLICITLY bars x86/AVX-512/SVE and any 6th shape / asm collapsed-stage on the CSS path (`restart/skinny/tranches/sk-v17/SPEC.md:258` "aarch64 only", `:806`, §9 W3 pre-block `:826`, §9 REJECTed `:854`). The NEON path produces only a `Vec<u32>` structural index via `select_classifier` (`:99-106`, `:628-636`). | spec-x86-CollapsedStage vs skinny-aarch64-NEON | unimplemented (no aarch64 CollapsedStage; x86 path not the SK-V17 target) | The 5-shape canon holds at Lock 10 (`LOCKS.md:107-108`); but the canon's CollapsedStage admission is x86-pinned, mechanically refused on aarch64 (`ARCHITECTURE.md:1206`). The aarch64 CollapsedStage candidate is ALREADY the spec-named **UNKNOWN-2D-05** (`ARCHITECTURE.md:1206` "aarch64 candidate is UNKNOWN-2D-05") — a spec-flagged open unknown, NOT an undiscovered gap. SK-V17's NEON union sits entirely under the four LLVM shapes' scan-leaf FFI, NOT CollapsedStage. T-P2 must map how aarch64-NEON absorbs into the canon without a 6th shape, resolving UNKNOWN-2D-05. |
| COH17-005 | MASTER §13 SIMD vocabulary is x86-heavy (`ext/x86/bbnf.asm`, `FSM_DISPATCH_THREADED`, AVX-512 `CollapsedStage`, `restart/MASTER-PLAN.md:622,626,667`); ARCH lists bbnf-simd as AVX2/AVX512/NEON/WASM/scalar (`ARCHITECTURE.md:89`). | crates/core's wired scan crate `simd-scan` carries neon + avx2 + avx512 + wasm + scalar kernels generically (`crates/simd-scan/src/{neon,avx2,avx512,wasm,scalar}.rs`, dispatch `lib.rs:86-113`). SK-V17 uses ONLY the narrower aarch64 `skinny/crates/bbnf-simd` `select_classifier(&[u8;64])` (`dispatch.rs:42`); SK-V17 bars x86/AVX/SVE (SPEC `:258,:806`). | impl/multiarch-exceeds-spec-scope vs aarch64-only-skinny | impl-exceeds-spec | crates/core's scan layer is BROADER (multi-arch) than the SK-V17 proven path (aarch64-only). The fold must decide whether the totality scan retains x86/wasm kernels or narrows to the proven aarch64 set; this is a scope question, not a defect. Two separate simd crates exist: `crates/simd-scan` (totality, multi-arch) and `skinny/crates/bbnf-simd` (skinny, aarch64). |
| COH17-006 | Lock 2 RETIRED the term `StructLayout`; canonical name is `Layout`/`LayoutFacts` (`restart/locks/LOCKS.md:160`); alphaC re-states "Lock 2 (LOCKS.md:160) RETIRED the term StructLayout" (`restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:29`). | crates/core (the fold target) carries `StructLayout` LIVE: `pub struct StructLayout` (`crates/ir/src/registry/struct.rs:202`), consumed by `begin_compound(&StructLayout)` (`crates/core/src/runtime/tape/mod.rs:185`). The retired name is the live core API. | spec-retired-name vs core-live-name | unimplemented (name not migrated in core) | The totality tree has NOT migrated `StructLayout` → `Layout` per Lock 2; the SKINNY tree uses `BackendRule` + `LayoutFacts.backend_shape` (alphaC `:29`). `grep -rn StructLayout crates/` = **960 references** spanning `ir/registry/struct.rs:202` + codegen emitter + ~16 test files — NOT 40-120 LOC. A Lock-2 naming reconciliation surface for T-P3 (no candidate raised here per 1E ownership). |
| COH17-007 | Lock 1 v+1 admits `FactStream` as the 5th substrate-manifest category with `substrate_target=admitted_fact_output` (`restart/locks/LOCKS.md:100-116`); ARCH preserves FactStream as output-plane category #3 (`ARCHITECTURE.md:1803`). | SK-V17 RETIRES the CSS fact-stream String as an admission plane entirely — `emit_fact_stream`/`CssFullParseSummary` are diagnostic-only and the W1 PRUNE deletes the live plane (`restart/skinny/tranches/sk-v17/SPEC.md:243-244`, `:390-394`, `:796-797`). | spec-admits-factstream vs skinny-retires-it-as-admission | unknown | NOT a contradiction: Lock 1's FactStream category is "output-plane with strict comparator/oracle provenance", while SK-V17 bars fact-stream-as-CSS-admission-plane specifically. The fold must state whether the totality FactStream category survives once SK-V17 proves the tape plane; verify_action below. |
| COH17-008 | SK-V17 NEON leaf datum is `alphabet: &[u8;64]` via `select_classifier` (`restart/skinny/tranches/sk-v17/SPEC.md:99-106`, §2.1 `:316`); the eq-set fan is the CSS-admissible backend, lo6 table refused on CSS alphabet (`:594-596`, §9 `:826,:853`). | crates/core's `StructuralAlphabet` is wider config: `singletons: &[u8]`, `digraph_pairs`, `digraph_mask: [u64;4]`, `quote_classes` (`crates/simd-scan/src/alphabet.rs:19-37`), with `KernelShape::select` (NibbleLut/WideLut/MultiCmp, `avx2.rs:39`). | impl-richer-alphabet-config vs skinny-[u8;64] | impl-exceeds-spec | The totality alphabet config is richer (digraphs, quote classes, kernel-shape selection) than the proven aarch64 `[u8;64]` classifier. NOTE: the `quote_classes` doc itself (`crates/simd-scan/src/alphabet.rs:35-37`) shows quote-classes are JSON/CSS-motivated (`b'"'` for JSON, `b'\''`/`b'"'` for CSS); the generality is **JSON/CSS-exercised-only** (breadth-of-config, NOT breadth-of-proof), matching SK-V17 SPEC §0.1.11 item 11 (`:112-114` "Projection generality exercised by-construction on JSON + CSS only"). Both are grammar-as-data (Lock 14 honoured); the fold must reconcile the two alphabet abstractions into one. |

## Cross-Tree Substrate Map (SK-V17 fold subject)

| Concern | SKINNY-proven (skinny/crates) | TOTALITY fold target (crates/core) | Fold reconciliation |
|---|---|---|---|
| Tape encoding | SoA `Tape<'input>`: `offsets: Vec<u32>` + sparse `flag_cursors`/`flag_values` + `PayloadArena` (`skinny/crates/runtime/src/tape/mod.rs:94`; p2d `:34-49`) | AoS 16-byte `TapeRec` in `TapeStructBuilder` (`crates/core/src/runtime/tape/mod.rs:58`, `record.rs:103`) | ONE substrate (Lock 1); SK-V18 adopts the proven SoA or proves AoS parity |
| Builder | `TapeBuilder`/`push_plain_offset` (SPEC `:53,:444`) | `TapeStructBuilder` impl `StructBuilder` (UNWIRED) + live eager `JsonStructBuilder`/`CssStructBuilder` `OpenFrame` (`crates/core/src/runtime/json/builder.rs:9`, `css_l4/builder.rs:16`) | retire eager `OpenFrame` builders; wire tape builder |
| Value API | grammar-parametric `ValueRef<'doc,'input,K,G>` + `value_from_ref` per grammar (`skinny/crates/runtime/src/tape/mod.rs:175`; `skinny/crates/runtime/src/grammars/json/value.rs:143`) | per-grammar eager typed enums; NO `ValueRef`/`value_from_ref` in core (grep empty over `crates/core/src/runtime/json/value.rs`) | generate one `BackendRule`/`FieldSource`-walking lazy projection. The FieldSource walk is **compile-time projection-emission, resolved ONCE** at codegen — NOT a per-leaf runtime `StructRegistry` walk; a naive per-leaf walk re-opens the 28-65×/983×/10583× regression (SPEC `:794-795`) |
| Layout shape | `BackendRule` + `LayoutFacts.backend_shape` (alphaC `:29`); skinny ir/cost.rs | `StructLayout` + `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}` (`crates/ir/src/registry/struct.rs:84,202`); `FieldSource` lives INSIDE the live `StructRegistry` (`struct.rs:84,313`) | Lock-2 name reconcile; FieldSource IS the BackendRule-walk recipe, walked **once at compile time** (not per-leaf), citing SPEC `:794-795` (StructRegistry hot-path-indirection pre-block) |
| Structural scan | aarch64 `select_classifier(&[u8;64])` → `Vec<u32>` (`skinny/crates/bbnf-simd/src/dispatch.rs:42`); CSS scan is the W3 wire, **gated behind tape activation** (SPEC `:106`) — wiring-state, not design property | multi-arch `scan_structural(&StructuralAlphabet)` → `StructuralIndex`, WIRED into ALL 8 generated grammars (json.rs:732, css_l4.rs:15982, ebnf, bnf, csv, css_pretty.rs:1905, google_sheets.rs:3559, bbnf.rs:4843) (`crates/simd-scan/src/lib.rs:80`) | scan already grammar-general in core (all 8); the missing primitive is the **tape consumer**, not the scan |
| BackendShape | 5-shape canon, NEON under 4 LLVM shapes, NO aarch64 CollapsedStage (SPEC §9) | 5-shape enum, CollapsedStage x86/AVX-512-pinned (`ARCHITECTURE.md:1109,1206`) | canon holds; aarch64 absorbs into 4 LLVM shapes |

## Divergences Catalogued

| ID | Divergence | Evidence | LOC / risk |
|---|---|---|---|
| COH17-001 | Tape encoding differs: core AoS `TapeRec` vs skinny-proven SoA `Tape`. **Lock-1 post-fold closure obligation: EXACTLY ONE encoding** — a dual encoding is not a permissible end-state (`LOCKS.md:75` "parallel substrates are dead"). The proven SoA `Tape` is the Lock-1-admissible OFFSET tape (`LOCKS.md:75` "the projection may be an offset tape"), NOT the buried AV.04 dense class-column columnar-SoA (`LOCKS.md:75` "columnar SoA … never activated … stays buried"). | `crates/core/src/runtime/tape/record.rs:103,120`; `skinny/crates/runtime/src/tape/mod.rs:94-100` (offsets:96 + sparse flag_cursors:97/flag_values:98); p2d `:34-49`; `LOCKS.md:75` | 200-600 LOC SK-V18 fold; medium (both exist, neither is parallel within its tree). Re-emit is generator-side / regen-gated; eager-builder retirement touches 22+ files (all 8 generated parsers carrying `*StructBuilder` + emitter shape hierarchy + runtime/mod.rs + both parse_with.rs, via `grep -rln JsonStructBuilder|CssStructBuilder`). |
| COH17-002 | Value API is per-grammar eager GENERATED enums in core (`@generated by xtask regen-*`), not lazy `ValueRef<G>`. | `crates/core/src/runtime/css_l4/value.rs:414` (`CssTypedValue`), `crates/core/src/runtime/css_l4/view.rs:44` (`CssChildrenIter`); no `value_from_ref` in `crates/core/src/runtime/json/value.rs`; skinny `value_from_ref` at `skinny/crates/runtime/src/grammars/json/value.rs:143` | 300-700 LOC projection generator fold; high. Distinguish GENERATOR-LOC (emitter projection generator — the actual fold edit surface) from regenerated emitted-LOC (per-grammar value.rs/view.rs re-emit set). |
| COH17-003 | crates/core tape UNWIRED while structural scan WIRED — asymmetric. | `crates/core/src/runtime/json/parse_with.rs:34`; `crates/core/src/grammar/generated/json.rs:732`; TapeStructBuilder grep-zero outside tape/ | 0 LOC to catalogue; high to wire (the SK-V18 subject) |
| COH17-004 | §7.3 CollapsedStage is x86/AVX-512; SK-V17 aarch64 has no CollapsedStage. | `restart/ARCHITECTURE.md:1109,1151,1186,1206`; SPEC `:258,:806,:854` | 0 LOC (canon holds); medium (fold must absorb aarch64-NEON without a 6th shape per Lock 10 `:107-108`) |
| COH17-005 | Two SIMD crates: `crates/simd-scan` (multi-arch, wired) vs `skinny/crates/bbnf-simd` (aarch64, proven). | `crates/simd-scan/src/{neon,avx2,avx512,wasm}.rs`; `skinny/crates/bbnf-simd/src/dispatch.rs:42`; SPEC `:99,:599` | 100-400 LOC scope reconcile; medium |
| COH17-006 | `StructLayout` is Lock-2-retired but live in crates/core/crates/ir. | `restart/locks/LOCKS.md:160`; `crates/ir/src/registry/struct.rs:202`; alphaC `:29`; `grep -rn StructLayout crates/` = 960 | generator-side rename regenerating all 8 parsers + ~16 tests; **960-site surface**; medium (was mis-priced ~8× as 40-120 LOC/low) |
| COH17-007 | Lock 1 FactStream 5th category vs SK-V17 retiring fact-stream as CSS admission plane. | `restart/locks/LOCKS.md:100-116`; SPEC `:243,:796` | 0 LOC; low (reconcilable framing) |
| COH17-008 | core `StructuralAlphabet` (digraph/quote/kernel-shape) richer than proven `[u8;64]`. | `crates/simd-scan/src/alphabet.rs:19-37`; `skinny/crates/bbnf-simd/src/dispatch.rs:42`; p2f `:30-37` | 50-200 LOC alphabet reconcile; low (both grammar-as-data) |

## Gaps / Missing Primitives

| Gap | Evidence | Receiver |
|---|---|---|
| No live tape consumer in crates/core: `TapeStructBuilder` exists but no parse path calls it. | `crates/core/src/runtime/json/parse_with.rs:34` uses `JsonStructBuilder`; `TapeStructBuilder` grep-zero outside `tape/` | SK-V18 fold (T-P2/T-P3 map; SK-V17 proves in skinny first) |
| No `ValueRef<G>` lazy-projection generator in crates/core; value APIs are per-grammar eager typed enums GENERATED per grammar (`@generated by xtask regen-*`, NOT hand-written — the fold target is the generator). | `crates/core/src/runtime/css_l4/value.rs`, `json/value.rs` (no `value_from_ref`; both `@generated` at `:1`) | SK-V17 W2 proves in skinny (SPEC §5); SK-V18 folds into core (the generator must emit a `ValueRef<G>` projection in place of the eager enum) |
| No aarch64 CollapsedStage admission path; the §7.3 canon's 5th shape is x86-only. | `restart/ARCHITECTURE.md:1206`; SPEC §9 `:854` | T-P2 research: how aarch64-NEON union maps into the canon without a 6th shape |
| Core CSS structural scan IS wired — the missing primitive is the TAPE CONSUMER, not the scan. `css_l4.rs:15976-15982` builds the full `StructuralAlphabet{singletons,digraph_mask,digraph_pairs,quote_classes}` and calls `scan_structural(input,&alphabet)`; `OnceCell<StructuralIndex>` at `:15951`. All 8 generated grammars are scan-wired (not json/ebnf/bnf/csv only). The residual TRUE Lock-1 gap is that no tape consumes the grammar-general structural index. | `crates/core/src/grammar/generated/css_l4.rs:15936,15951,15976-15982`; `grep -c scan_structural css_l4.rs` = 1 (same as json/ebnf/bnf/csv); `TapeStructBuilder` grep-zero outside `tape/` | SK-V17 W3 wires the skinny-side CSS scan (gated behind tape activation, SPEC `:106`); SK-V18 wires the core tape consumer |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-COH17-001 (COH17-007) | Does the Lock 1 `FactStream` 5th substrate-manifest category survive once SK-V17 proves the tape plane retires fact-stream-as-CSS-admission, or does it become diagnostic-only across both trees? | T-P3/Omega compare `restart/locks/LOCKS.md:100-116` against SK-V17 SPEC `:243-244,:796-797`; state whether FactStream category is retained for non-CSS output planes or demoted; no LOCKS edit by T-P1. |
| U-COH17-002 (CATALOGUED DIVERGENCE — exactly-one-encoding closure) | Lock 1 (`LOCKS.md:75` "parallel substrates are dead") obliges EXACTLY ONE substrate encoding post-SK-V18. The two tape encodings (core AoS / skinny SoA) MUST converge to one; a dual encoding is NOT a permissible end-state. **The "adopt proven SoA" candidate is NOT self-contradictory against `LOCKS.md:75`'s "columnar SoA is dead":** the buried-and-dead shape is the AV.04 DENSE class-column columnar-SoA ("designed in AV.04 archaeology but never activated"), whereas the proven `Tape` is the Lock-1-ADMISSIBLE offset tape ("the projection may be an offset tape") whose `offsets: Vec<u32>` IS the structural projection (`ARCHITECTURE.md:1088`) and whose `flag_cursors`/`flag_values` are SPARSE position-keyed side-vectors, not a dense class column (`skinny/crates/runtime/src/tape/mod.rs:96-98`). The two are DIFFERENT constructs; adopting the offset-tape does not resurrect the class-column. Open: which encoding survives — adopt the proven (admissible) offset-tape SoA, or keep AoS and prove parity? | Re-read `crates/core/src/runtime/tape/mod.rs:5-9` ("AoS first … later SoA split") against SPEC `:110-114` and `LOCKS.md:75` (BOTH the "offset tape" admissibility clause AND the "columnar SoA … buried" dead-class-column clause); T-P2 names the single convergence-target encoding, distinguishing offset-tape-SoA (admissible) from class-column-SoA (dead); T-P3 asserts the exactly-one-encoding post-SK-V18 obligation explicitly (not left as a permissible dual end-state). |
| U-COH17-003 | Does the totality scan layer (`crates/simd-scan`, multi-arch) narrow to the proven aarch64 set, or retain x86/avx512/wasm kernels post-fold? | T-P2 compare `crates/simd-scan/src/lib.rs:80-113` against SK-V17 aarch64-only mandate (SPEC `:258`); decide scope (architecture pressure, not a defect). |
