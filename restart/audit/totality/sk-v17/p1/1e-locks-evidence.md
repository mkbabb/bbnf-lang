---
agent: 1E
pass: T-P1-excavation
cycle: V5
generated_at: 2026-05-29T23:59:00Z
spec_surfaces_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/locks/LOCKS.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/skinny/tranches/sk-v17/SPEC.md
  - restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
  - restart/audit/totality/p1/1F-coherence-scan.md (prior-totality COH-014)
  - crates/core/src/runtime/tape/{mod,record,arena,cursor}.rs
  - crates/core/src/runtime/json/builder.rs
  - crates/core/src/runtime/css_l4/{builder,value}.rs
  - crates/core/src/grammar/generated/{json,ebnf,bnf,csv,css_l4,css_pretty,google_sheets,bbnf}.rs
  - crates/ir/src/registry/struct.rs
  - crates/simd-scan/src/{lib,alphabet}.rs
  - skinny/crates/runtime/src/tape/mod.rs
  - skinny/crates/runtime/src/grammars/json/value.rs
files_audited_count: 31
live_truth_method: "grep -n / grep -c / sed -n / wc -l / ls over crates/core, crates/ir, crates/simd-scan, skinny/crates/runtime, restart/{ARCHITECTURE,MASTER-PLAN}.md, restart/locks/LOCKS.md, restart/skinny/tranches/sk-v17/SPEC.md at master HEAD 445925167; no cargo/build/test/edit of source"
prior_cycle_dispositions_folded:
  accepted:
    - V4-CH5-S3-renamed-scanner-no-cross-call-state (re-verified live: scan_structural OUTPUT index per-parse, no classifier carry)
    - V4-LAC-1E-SKV17-05-unbundled-free-UNKNOWN-2D-05-0-LOC (clean per-cost separation mirroring SUB17-005; ACCEPT)
    - V4-CH5-S1..S7-substrate-firewall-1e-parity (1e:101/111/122/138 BackendShape + classifier-state rows ACCEPT)
    - V3-§6-1e-locks-Verified-Invariants-L01-L02-L10-L14-L16-COH-014 (16-lock + 5-shape canon re-verified)
  revised_this_cycle:
    - V4-LAC-1E-SKV17-04-path-b (CH4 REVISE: path-(b) priced "~0 LOC (LayoutFacts already live)" but grep -rn LayoutFacts crates/=0; LayoutFacts absent from crates/core fold target, present only skinny/crates/passes/src/lib.rs:85,91; LOCKS.md:162-166 pointer resolves to skinny/prior-totality state; FIX: qualify side-table as skinny/prior-totality-only + state non-zero crates/core materialisation cost + cite grep-zero anchor; path split itself correct, path-(a) 960-site/medium live-verified)
    - V4-CH5-S8 (CH5 REVISE: 1e:179 REDRESS-53 re-entry anchored at SPEC :578 [adjacent sidecar-vector clause] not the naming line; REDRESS-53 named at :577/:657/:825/:839 [grep]; FIX: re-anchor 1e:179 to :577/:825/:839; same failure class V3 closed in 1A; one-line, no verdict change)
  rejected:
    - CH1-V2-003-css_l4-scan-not-wired (FALSE-NEGATIVE; all 8 grammars scan-wired)
    - S1.9-no-css_l4-scan-wired (same false-negative)
    - REJECT-§1-css-structural-scan-not-wired (same false-negative)
  revised:
    - CH1-V2-002-value_from_ref-path (added grammars/ segment)
    - CH1-V2-004-OnceCell-census-omits-css_l4
    - CH1-V2-005-nine-pending-Vecs (corrected to 6 Vec + 1 Option)
    - CH5-S4-nine-pending-count (same)
    - CH1-V2-006-divergence_count-overstated (1F frontmatter)
    - S1.4-CollapsedStage-UNKNOWN-2D-05 (spec-named unknown, not fresh gap)
    - S1.8-COH17-008-alphabet-generality (JSON/CSS-exercised, not breadth-of-proof)
    - CH3-R1-StructRegistry-pre-block-absent (added ledger row + fence)
    - CH3-R2-FieldSource-walk-compile-time (annotated)
    - CH3-R3-1d-1e-pre-block-identifier-absent (this artefact + 1d author the ledger)
    - CH5-S8-Lock-1-one-substrate-closure-obligation (catalogued divergence)
    - COH-014-prior-totality-contradiction (JSON+Sheets carriers caught)
  first_cycle_additions:
    - LAC-1E-SKV17-01-aarch64-CollapsedStage-UNKNOWN-2D-05-carrier
    - LAC-1E-SKV17-02-StructRegistry-no-per-leaf-lookup-fence
    - LAC-1E-SKV17-03-OnceCell-StructuralIndex-all-8-substrate_target
    - LAC-1E-SKV17-04-StructLayout-Lock2-rename-surface (V4: split full-rename vs narrow-to-side-table; V5: path-(b) re-priced — LayoutFacts grep-zero in crates/, side-table skinny/prior-totality-only)
    - LAC-1E-SKV17-05-aarch64-CollapsedStage-UNKNOWN-2D-05 (V4: free 0-LOC, unbundled from simd-scan; V5 ACCEPT)
    - LAC-1E-SKV17-06-simd-scan-multiarch-scope-reconcile (V4: split out per FOLD, mirror 1A SUB17-008)
divergence_count:
  spec_claims_implemented: 3
  spec_claims_unimplemented: 5
  impl_exceeds_spec: 2
  unknown: 2
locks_amendment_candidates: 6
---

# Totality T-P1 1E Locks Evidence — SK-V17 (tape / substrate / value-API / BackendShape / NEON)

## Executive Summary

This V5 inventory audits the locks that govern the SK-V17 unified-tape fold —
Lock 1 (substrate-union), Lock 2 (Layout naming), Lock 10 (5-shape
`BackendShape` canon), Lock 14 (grammar-neutral), Lock 16 (primitive
manifest) — against the `crates/core` fold target at master HEAD `445925167`,
and folds the two V4 CHALLENGE REVISEs that touched 1E: CH4 LAC-04 path-(b)
(`LayoutFacts` priced as live in core where `grep -rn LayoutFacts crates/`=0;
re-priced as skinny/prior-totality-only side-table + non-zero crates/core
materialisation cost) and CH5-S8 (the REDRESS-53 re-entry anchor moved off
the adjacent sidecar-vector clause at SPEC `:578` onto the verbatim naming
lines `:577`/`:825`/`:839`, where `grep REDRESS-53` resolves). The 16-lock count and the 5-shape
canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` both hold
(`restart/locks/LOCKS.md:107-108`). The central V3 correction: the V2 1F triad
catalogued a FALSE-NEGATIVE — that `crates/core` CSS structural scan is
unwired — when in truth **all 8 generated grammars carry
`scan_structural` + `OnceCell<StructuralIndex>`**, css_l4 included
(`crates/core/src/grammar/generated/css_l4.rs:15982`,`:15936`). The scan is
grammar-general and wired today; the real Lock-1 gap is the absent tape
consumer, not the scan. Substrate truth in core: ONE retained tape construct
(`TapeStructBuilder` over 16-byte AoS `TapeRec`), UNWIRED; the live substrate
is the eager `OpenFrame`/`pending_*` builders (`json/builder.rs:9`,
`css_l4/builder.rs:16`, 817 LOC god-module). The SK-V17-proven model is the SoA
`Tape<'input>` + grammar-parametric `ValueRef<G>` +
`value_from_ref`. The most load-bearing pre-block this fold inherits and the V2
triad WHOLLY OMITTED: the **StructRegistry / Arena<G> / Builder<G> hot-path
indirection** (28-65× / 983× / 10583× regression,
`restart/skinny/tranches/sk-v17/SPEC.md:793-795`) — `StructLayout` lives in
`crates/ir/src/registry/struct.rs:202` and feeds `begin_compound`
(`crates/core/src/runtime/tape/mod.rs:185`); the `FieldSource` projection walk
MUST be compile-time emission resolved once, never a per-leaf registry lookup.
This artefact proposes amendment candidates only; it disposes none.

## Verified Invariants

| invariant | status | evidence |
|---|---|---|
| 16-lock count | verified | `restart/locks/LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453` are the 16 numbered lock headings. |
| 5-shape `BackendShape` canon | verified | `restart/locks/LOCKS.md:107-108` pins `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; `FactStream` is the 5th substrate-manifest *category*, NOT a 6th shape (`:100-109`); SK-V17 §9 bars a sixth shape (`restart/skinny/tranches/sk-v17/SPEC.md:808`). |
| Structural scan wired across ALL 8 grammars | verified (V3 correction) | `grep -c scan_structural` returns 1 for each of json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf generated files; `OnceCell<StructuralIndex>` present in all 8. Canonical: `crates/core/src/grammar/generated/json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`, `bbnf.rs:4843`; signature `crates/simd-scan/src/lib.rs:80`. |
| ONE tape construct in core (no second substrate) | verified | `TapeStructBuilder` (`crates/core/src/runtime/tape/mod.rs:58`), `TapeRec` 16-byte AoS (`record.rs:103`, const-asserted `:120`), `PayloadArena` (`arena.rs`), `TapeCursor` (`cursor.rs`); UNWIRED (grep-zero outside `tape/`). |

## Spec-Claim ↔ Implementation Table

| lock | verdict | supporting evidence | audit note |
|---|---|---|---|
| L01 substrate-union / no parallel substrate / index-IS-tape | partial / core-tape-UNWIRED, scan-WIRED | ARCH Lock-1 union "if structural offsets are retained, the structural projection IS the tape" (`restart/ARCHITECTURE.md:1088`); LOCKS.md `:75`, v+1 manifest `:118-127`, ELEVATION `:137-149`. Live: ONE `TapeStructBuilder` UNWIRED (`crates/core/src/runtime/tape/mod.rs:58`); live substrate is eager `OpenFrame` (`json/builder.rs:9`, `css_l4/builder.rs:16`); structural scan wired in all 8 grammars feeding eager builders, not the tape. | The Lock-1 union is partially honoured: ONE substrate exists per tree but they DIFFER (core AoS `TapeRec` vs skinny SoA `Tape`); the post-fold closure obligation is exactly-one-encoding, not a dual end-state. |
| L02 Layout canonical name (StructLayout retired) | drifted (name not migrated in core) | Lock 2 retires `StructLayout` → `Layout`/`LayoutFacts` (`restart/locks/LOCKS.md:160`); alphaC re-states it (`restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:29`). Live: `pub struct StructLayout` at `crates/ir/src/registry/struct.rs:202`, consumed by `begin_compound(&StructLayout)` at `crates/core/src/runtime/tape/mod.rs:185` and imported `:26`. | The retired term IS the live core API. Lock-2 reconcile is a 960-site rename (see L02 cost below), not a leaf change. |
| L10 5-shape canon / CollapsedStage admission | partial / canon holds, CollapsedStage aarch64-refused | Lock 10 5-shape domain (`restart/locks/LOCKS.md:107-108`, `:269-274`); ARCH CollapsedStage "fuses mask-state and emission for AVX-512-class hardware" (`restart/ARCHITECTURE.md:1088`), `admits_collapsed_stage` co-requires `target.arch == x86 + target.avx512bw + Entry(_)` (`:1151,:1171,:1206`; LOCKS.md `:520-533`). SK-V17 is aarch64-only and bars x86/AVX/SVE (`restart/skinny/tranches/sk-v17/SPEC.md:258,:806,:808`). | The aarch64 CollapsedStage is **UNKNOWN-2D-05** — a SPEC-NAMED open unknown (`restart/ARCHITECTURE.md:1206,:1279-1280`), not a fresh gap. SK-V17 NEON sits under the four LLVM shapes' scan-leaf FFI. The canon absorbs aarch64-NEON without a 6th shape. |
| L14 grammar-neutral / scan grammar-general | honoured for scan; eager builders are per-grammar runtime (allowed) | Lock 14 (`restart/locks/LOCKS.md:349`); generated-output allowance `:351-358`. Live: `scan_structural(input, &StructuralAlphabet)` is grammar-as-DATA (alphabet config: `singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes`, `crates/simd-scan/src/alphabet.rs:19-37`); `TapeStructBuilder` "dispatches on the StructLayout … never on per-grammar route strings" (`crates/core/src/runtime/tape/mod.rs:54-56`). Per-grammar `CssStructBuilder`/`CssTypedValue` (`css_l4/value.rs:414`) are per-grammar runtime surfaces (Lock 14 ALLOWED). | Scan and tape are grammar-neutral by data; the eager per-grammar builders are the fold-deletion target, not a leak. |
| L16 primitive manifest / NEON allowlist | partial / aarch64 NEON proven, x86 diagnostic | Lock 16 NEON allowlist (`restart/locks/LOCKS.md:453-478`), manifest mandate `:480-489`, strict `:491-493`. SK-V17 NEON leaf is `select_classifier(&[u8;64])` → `Vec<u32>` (aarch64-only); x86 AVX/AVX-512 is diagnostic (`restart/ARCHITECTURE.md:33`; `restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:307-316`). Core scan crate `simd-scan` carries neon/avx2/avx512/wasm/scalar kernels (`crates/simd-scan/src/lib.rs`). | The proven NEON path is the admission target; core's multi-arch scan is BROADER than the proven aarch64 set — a fold-scope question (architecture pressure), not a defect. |

## Divergences Catalogued

| id | locks | divergence | evidence | files-touched surface |
|---|---|---|---|---|
| D-1E-SKV17-01 | L01 | Tape encoding differs cross-tree: core 16-byte AoS `TapeRec` vs SK-V17-proven SoA `Tape<'input>` (`offsets: Vec<u32>` + sparse `flag_cursors` + `PayloadArena`). Post-fold MUST converge to exactly ONE encoding (LOCKS.md `:75` "parallel substrates are dead"). | `crates/core/src/runtime/tape/record.rs:103,120`; `skinny/crates/runtime/src/tape/mod.rs:94,:96,:99`; core mod-doc admits "kept AoS first … the same TapeCursor API rides a later SoA split" (`crates/core/src/runtime/tape/mod.rs:6-9`). | re-emit is generator-side / regen-gated; eager-builder retirement touches 22+ files (all 8 generated parsers + emitter shape hierarchy + `runtime/mod.rs` + both `parse_with.rs`), verified via `grep -rln JsonStructBuilder|CssStructBuilder`. |
| D-1E-SKV17-02 | L01, L14 | Value API is per-grammar eager typed enums in core, not the lazy grammar-parametric `ValueRef<G>` + `value_from_ref` projection the SK-V17 path proves. | core `crates/core/src/runtime/css_l4/value.rs:414` `CssTypedValue`; `value_from_ref`/`ValueRef` grep-ZERO over `crates/core/src/runtime/json/value.rs`. SK-V17-proven: `skinny/crates/runtime/src/grammars/json/value.rs:143` `value_from_ref(inner: ValueRef<'doc,'input>)`; `skinny/crates/runtime/src/tape/mod.rs:175` `pub struct ValueRef<'doc,'input,K,G>`. | hand-written generator-LOC (emitter projection generator) vs regenerated emitted-LOC (per-grammar value.rs/view.rs re-emit set); distinguish at fold. |
| D-1E-SKV17-03 | L01 | StructRegistry / Arena<G> / Builder<G> hot-path indirection is a load-bearing GLOBAL pre-block the V2 1F triad WHOLLY OMITTED. `StructLayout` lives in the live registry and feeds `begin_compound`; a naive per-leaf `FieldSource` walk re-opens the 28-65× / 983× / 10583× regression. | `restart/skinny/tranches/sk-v17/SPEC.md:793-795` (global block), `:483`, `:824` (W1 row); `crates/ir/src/registry/struct.rs:202` `StructLayout`, `:313` `StructRegistry`, `:84` `FieldSource`; consumed at `crates/core/src/runtime/tape/mod.rs:185` `begin_compound`. | the live `begin_compound` reads only `layout.rule_id` (`crates/core/src/runtime/tape/mod.rs:186`), not a per-leaf registry lookup — the fence is "no per-leaf StructRegistry lookup in the hot path"; the `FieldSource` projection walk MUST be compile-time emission resolved once. |
| D-1E-SKV17-04 | L10 | §7.3 CollapsedStage is x86/AVX-512-pinned and mechanically refused on aarch64; SK-V17 aarch64-NEON has no CollapsedStage. | `restart/ARCHITECTURE.md:1088,:1151,:1171,:1186,:1206`; `restart/locks/LOCKS.md:520-533`; `restart/skinny/tranches/sk-v17/SPEC.md:258,:806,:808`. The aarch64 candidate is **UNKNOWN-2D-05** (`restart/ARCHITECTURE.md:1206,:1279-1280`). | 0 LOC (canon holds; spec-named unknown); medium to research the aarch64 absorption. |
| D-1E-SKV17-05 | L02 | `StructLayout` Lock-2-retired but live across ~960 references in `crates/ir`/codegen emitter/~16 test files; mis-priced ~8× by the V2 1F (which estimated 40-120 LOC/low). | `restart/locks/LOCKS.md:160`; `crates/ir/src/registry/struct.rs:202`; `grep -rn StructLayout crates/` ≈ 960 references spanning ir/registry + codegen emitter + test files. | generator-side rename regenerating all 8 parsers + ~16 tests; risk medium, 960-site surface. |
| D-1E-SKV17-06 | L16 | Two SIMD crates exist: `crates/simd-scan` (multi-arch neon/avx2/avx512/wasm/scalar, wired in core) vs `skinny/crates/bbnf-simd` (aarch64 `select_classifier`, proven). Core scan is BROADER than the proven aarch64 set. | `crates/simd-scan/src/lib.rs:80`; `skinny/crates/runtime/src/tape/mod.rs` (skinny path); `restart/skinny/tranches/sk-v17/SPEC.md:258` aarch64-only. | impl-exceeds-spec; scope reconcile (architecture pressure), not a defect. |

## CH3 Do-Not-Redrive Ledger (pre-block fences the fold inherits)

The SK-V17 §9 pre-blocks are keyed to the **skinny benched surface**, but each
pre-fences the `crates/core` fold target. The fold MUST NOT re-open these:

| pre-block | binding evidence | fold fence (do-not-redrive) |
|---|---|---|
| **StructRegistry / Arena<G> / Builder<G> hot-path indirection** (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind) | `restart/skinny/tranches/sk-v17/SPEC.md:793-795`, `:483`, `:824` (W1) | **NO per-leaf registry lookup in the per-leaf hot path.** The `FieldSource` walk (`crates/ir/src/registry/struct.rs:84` inside the live `StructRegistry` `:313`) is COMPILE-TIME projection-emission, resolved once at codegen, NOT a runtime per-leaf `StructRegistry` indirection. `begin_compound` (`crates/core/src/runtime/tape/mod.rs:185`) takes `&StructLayout` and reads `layout.rule_id` only — the fold must preserve this no-runtime-lookup property (SPEC `:794-795`). |
| Parallel retained index (REDRESS-53) | `restart/skinny/tranches/sk-v17/SPEC.md:577` [parallel-vector naming], `:825` (W2 row), `:839` (shortlist condition 1) — REDRESS-53 named verbatim at `:577`/`:657`/`:825`/`:839` (grep); `:824` (W2) | The `OnceCell<StructuralIndex>` in all 8 generated parsers MUST become the tape's `offsets` (index IS the tape, ARCH `:1088`) or `local_temp_only` — never a retained index parallel to a wired tape. |
| AZ-IV eager value tree (118× regression) | `restart/skinny/tranches/sk-v17/SPEC.md:791-792` | The eager `CssTypedValue` + `pending_*` builders (`crates/core/src/runtime/css_l4/builder.rs:71-79`) ARE the eager-value-tree shape; replace with lazy `ValueRef<G>`, do not carry forward. |
| Second substrate (D6) | `restart/skinny/tranches/sk-v17/SPEC.md:807-811`, `:854`, `:483` | The §9 block names skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor` as FORBIDDEN-in-skinny; SK-V18 adopts the PROVEN skinny `Tape`/`ValueRef` INTO core (monotonic skinny→totality, SPEC `:110-114`), never relocating core constructs into skinny. |
| x86 / AVX-512 / SVE / lo6 on CSS / aarch64 CollapsedStage | `restart/skinny/tranches/sk-v17/SPEC.md:806,:826,:854`; `restart/ARCHITECTURE.md:1206` (UNKNOWN-2D-05) | No aarch64 CollapsedStage admission, no x86 close path; the aarch64 candidate is the SPEC-named UNKNOWN-2D-05, requiring a 2E source-backed strategy before any admission. |
| Cross-call retained classifier state (Lock 1 v+1 ELEVATION) | `restart/locks/LOCKS.md:137-149`; `restart/skinny/tranches/sk-v17/SPEC.md:810` | No quote-mask/escape-mask/prev-state/prefix-XOR carry across a call boundary; `retention_lifetime = retained-across-call-boundary` is the REJECT class (LOCKS.md `:147`). Verified ACCEPT: core `scan_structural` retains the OUTPUT index per-parse, NOT classifier carry state (`crates/simd-scan/src/lib.rs:80`). |

## Prior-Totality Contradiction Caught (COH-014)

The prior totality 1F COH-014 (`restart/audit/totality/p1/1F-coherence-scan.md:87`)
enumerated the root `OnceCell<StructuralIndex>` carriers as **JSON + Google
Sheets** (`crates/core/src/grammar/generated/json.rs:695-703`,
`google_sheets.rs:3542-3605`). The SK-V17 V2 1f-coherence Gaps row
(`restart/audit/totality/sk-v17/p1/1f-coherence-scan.md:107`) claimed
"json/ebnf/bnf/csv only … css_l4 grep absent there." These contradict: COH-014
already proved Sheets carries the sidecar, and the live grep at HEAD `445925167`
proves ALL 8 generated grammars carry it. The V2 undercount is a false-negative;
V3 corrects to the all-8 census. A do-not-redrive scan must carry this so V3+
never re-derives the undercount.

## LOCKS-AMENDMENTS-CANDIDATE

Candidates only; disposition is T-P3 3C, G3-gated; merge is Pass Omega.
Six candidates this cycle. Scanned axes: Lock 1 substrate-union (AoS/SoA
closure, OnceCell classification, StructRegistry fence), Lock 2 (StructLayout
rename surface — full-rename vs narrow-to-side-table), Lock 10 (aarch64
CollapsedStage / UNKNOWN-2D-05), Lock 14 (scan grammar-generality), Lock 16
(NEON manifest / two-crate multi-arch scope). V5 folds the two V4 REVISEs:
CH4 re-prices LAC-04 path-(b) (the side-table is skinny/prior-totality-only —
`grep -rn LayoutFacts crates/`=0 — so its crates/core cost is non-zero/UNKNOWN,
not ~0 LOC), and CH5-S8 re-anchors the REDRESS-53 re-entry citation from the
adjacent sidecar-vector clause to the naming lines `:577`/`:825`/`:839`. LAC-05
(free UNKNOWN-2D-05, 0 LOC, Lock 10) is ACCEPTed — cleanly separated from the
simd-scan multi-arch narrowing (100-400 LOC, Lock 16, LAC-06), mirroring 1A's
SUB17-005/008 separation.

| candidate | type | target locks | proposed candidate text | supporting path:line evidence | loc/risk/wave_hint |
|---|---|---|---|---|---|
| LAC-1E-SKV17-01 | refinement | L01, L10 | Catalogue the post-fold one-substrate closure obligation as an EXPLICIT divergence, not an open question: once SK-V17 proves the tape plane, exactly ONE tape encoding survives across both trees; a dual AoS/SoA end-state is NOT a permissible Lock-1 end-state. | `restart/locks/LOCKS.md:75` ("parallel substrates are dead"); `crates/core/src/runtime/tape/record.rs:103`; `skinny/crates/runtime/src/tape/mod.rs:94`; `restart/skinny/tranches/sk-v17/SPEC.md:110-114`. | 200-600 LOC / medium / SK-V18 fold |
| LAC-1E-SKV17-02 | addition | L01, L10 | Add a no-per-leaf-registry-lookup fence to the substrate manifest: the `FieldSource` projection walk inside the live `StructRegistry` is compile-time emission resolved once; any per-leaf runtime `StructRegistry` indirection in the tape hot path re-opens the 28-65×/983×/10583× regression and is REJECT. | `restart/skinny/tranches/sk-v17/SPEC.md:793-795`; `crates/ir/src/registry/struct.rs:84,:202,:313`; `crates/core/src/runtime/tape/mod.rs:185-186`. | 0 LOC (fence) / high (regression class) / SK-V18 fold gate |
| LAC-1E-SKV17-03 | refinement | L01, L14 | The Lock 1 v+1 `substrate_target` classification of `OnceCell<StructuralIndex>` must scope to ALL 8 generated grammars (json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf), not a 4-grammar sample; each carrier declares `existing_tape` (index IS the tape) or `local_temp_only` before any tape wiring. | `crates/core/src/grammar/generated/json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`, `bbnf.rs:4843`; `restart/locks/LOCKS.md:118-127`. | 0 LOC (classification) / high / SK-V18 fold pre-gate |
| LAC-1E-SKV17-04 | refinement | L02 | Re-price the Lock-2 `StructLayout`→`Layout` reconcile per the TWO disjoint candidate paths (the V2 1F's 40-120 LOC/low estimate conflated them): **(a) full rename** — migrate `StructLayout`→`Layout` across all 960 sites, generator-side, regenerating all 8 parsers + ~16 tests; **(b) lock re-scope to side-table** — re-scope Lock 2 closure toward a `LayoutFacts.backend_shape` side-table per the existing v+1 note, leaving `StructLayout` as the registry record under a deferred public-name freeze. | `restart/locks/LOCKS.md:160,:162-166`; `crates/ir/src/registry/struct.rs:202`; `grep -rn StructLayout crates/` = 960 sites; `grep -rn LayoutFacts crates/` = **0** (both verified HEAD `445925167`); `LayoutFacts` is present only in `skinny/crates/passes/src/lib.rs:85,:91` (prior-totality/skinny). | path-(a) full rename: 960-site / medium / T-P3 3C + SK-V18 regen (live-verified). path-(b) lock re-scope: **NOT ~0 LOC against the fold target.** `LayoutFacts.backend_shape` is the SKINNY / prior-totality side-table (`skinny/crates/passes/src/lib.rs:85,:91`); it is **absent from `crates/core` at HEAD `445925167` — `grep -rn LayoutFacts crates/`=0** — so the LOCKS.md:162-166 "live side-table evidence today" pointer resolves to the prior-totality audit (`restart/audit/totality/p1/1E-locks-evidence.md:64`), NOT crates/core. The "~0 LOC" therefore prices only the *lock re-scope text* (doc-only); ANY crates/core realisation of the side-table is a non-zero generator-side surface (UNKNOWN until the side-table lands in core) / low for the text-only re-scope, UNKNOWN for the core materialisation / T-P3 3C. NOTE: the v+1 note bars claiming Lock 2 closure by `LayoutFacts` ALONE while public `Layout`/`LayoutSink` remain absent (`restart/locks/LOCKS.md:162-166`), so path-(b) is a re-scope of the lock, not a closure. |
| LAC-1E-SKV17-05 | refinement | L10 | Record the aarch64 CollapsedStage question as the SPEC-named UNKNOWN-2D-05, NOT a fresh gap: the 5-shape canon (`restart/locks/LOCKS.md:107-108`) holds and absorbs aarch64-NEON without a 6th shape; no LOCKS edit, no x86 close path. | `restart/ARCHITECTURE.md:1206,:1279-1280`; `restart/locks/LOCKS.md:107-108,:520-533`; `restart/skinny/tranches/sk-v17/SPEC.md:258`. | 0 LOC (canon holds; spec-named unknown) / medium (T-P2 source-backed aarch64 strategy per ARCH:1206) / T-P2 research |
| LAC-1E-SKV17-06 | refinement | L16 | Bind the multi-arch `crates/simd-scan` scope decision (narrow-to-aarch64 vs retain x86/avx2/wasm/scalar kernels) to the fold WITHOUT admitting x86 as a close path; the proven path uses only the aarch64 `select_classifier` leaf and core's scan is BROADER than the proven set (impl-exceeds-spec). | `crates/simd-scan/src/lib.rs:80` (signature), `:53-65` (kernel set); `skinny/crates/bbnf-simd/src/dispatch.rs:42`; `restart/skinny/tranches/sk-v17/SPEC.md:258`. | 100-400 LOC (scope reconcile, mirrors 1A SUB17-008) / medium / T-P2 research |

## Open Questions

| UNKNOWN | why unknown | verify_action |
|---|---|---|
| 1E-SKV17-U1 | Does the totality tree converge to ONE tape encoding in SK-V18 (adopt proven SoA) or keep AoS and prove parity? The core mod-doc admits "AoS first … later SoA split" (`crates/core/src/runtime/tape/mod.rs:6-9`) — a transition state, not a declared end-state. | T-P2 names the convergence target tape shape against `restart/skinny/tranches/sk-v17/SPEC.md:110-114`; no LOCKS edit by T-P1. |
| 1E-SKV17-U2 | Is each grammar's `OnceCell<StructuralIndex>` a `local_temp_only` scratch or an `existing_tape` projection under the Lock 1 v+1 manifest (`restart/locks/LOCKS.md:118-127`)? | T-P2 read all 8 generated parsers' `scan_structural` sites and classify against the four `substrate_target` values before wiring the tape, else REDRESS-53 re-entry (`restart/skinny/tranches/sk-v17/SPEC.md:577` [parallel-vector naming]; W2 row `:825`; shortlist condition 1 `:839`). |
