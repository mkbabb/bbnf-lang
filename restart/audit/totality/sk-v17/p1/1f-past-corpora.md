---
agent: 1F
pass: T-P1-excavation
cycle: V5
generated_at: 2026-05-30T17:00:00Z
spec_surfaces_audited:
  - restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md
  - restart/skinny/tranches/sk-v17/SPEC.md
  - restart/skinny/tranches/sk-v17/research/p2/p2d-substrate-tape.md
  - restart/skinny/tranches/sk-v17/research/p2/p2f-grammar-neutral.md
  - restart/audit/totality/p1/1F-coherence-scan.md
files_audited_count: 12
live_truth_method: "grep over restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md + SPEC.md §9; cross-ref prior totality 1F at restart/audit/totality/p1/1F-{coherence-scan,anti-pattern,past-corpora}.md; V5 re-verified REDRESS-53 named-lines live (`grep -n REDRESS-53 SPEC.md` = :577/:657/:825/:839; :578 is the continuation clause); no cargo/build mutation"
prior_cycle_dispositions_folded:
  accepted:
    - S7-ledger-rows-resolve-verbatim
    - V4-CH7-1f-past-corpora-do-not-redrive-ledger-clean-ACCEPT
  rejected: []
  revised:
    - S7-prior-totality-COH-014-undercount-flag-added
    - CH5-S5-second-substrate-all-8-enumeration
    - S3.2-PC17-004-UNKNOWN-2D-05-cross-ref
    - V5-CH5-S8-class-PC17-001-REDRESS-53-anchor-:578-corrected-to-:577-named-line
  first_cycle_additions:
    - PC17-001-redress-53-parallel-index
    - PC17-002-redress-96-97-98-substrate-ceiling
    - PC17-003-AZ-IV-eager-value-tree
    - PC17-004-x86-collapsedstage-not-target
    - PC17-005-StructLayout-OpenFrame-CssArena-totality-only
    - PC17-006-second-substrate-D6
divergence_count:
  spec_claims_implemented: 0
  spec_claims_unimplemented: 0
  impl_exceeds_spec: 0
  unknown: 0
locks_amendment_candidates: 0
---

## Executive Summary

Past-corpora do-not-redrive ledger for the SK-V17 totality T-P1 1F scope. The
SKINNY corpus has already measured the parallel-substrate, eager-value-tree,
retained-class-column, and x86 routes "into the ground"
(`restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:13`). The
totality fold must NOT re-derive these. The single most load-bearing past-corpora
fact for this 1F scope: the totality-tree constructs `StructLayout`, `OpenFrame`,
`CssArena`, `begin_compound`, `TapeStructBuilder` return ZERO on the skinny benched
surface — they are TOTALITY artefacts, and every SK-V17 pre-block is keyed to the
skinny benched surface, NOT to crates/core (alphaC `:20-25`). So the crates/core
tape excavated in 1F-coherence / 1f-anti-pattern is the fold TARGET, and the
SK-V17 pre-blocks pre-fence its fold: a second substrate (D6), a parallel retained
index (REDRESS-53), an eager value tree (AZ-IV), and any x86/AVX/SVE path are all
pre-blocked routes the fold inherits.

## Past-Corpora Do-Not-Redrive Ledger

| Finding family | Prior-corpora evidence | SK-V17 fold implication |
|---|---|---|
| Parallel retained index (REDRESS-53) | SK-V17 SPEC §9 W2/W3 pre-blocks "the L1/L4 index retained as a parallel vector (REDRESS-53)" and binds "L1/L4 index == tape-offsets identity … A retained parallel index collapses into REDRESS-53" (`restart/skinny/tranches/sk-v17/SPEC.md:577`, `:825`, `:839`; REDRESS-53 is NAMED at `:577`/`:657`/`:825`/`:839` — `:578` is the continuation clause "retained cursor / aux density / sidecar event vector", not the REDRESS-53 name). | The crates/core `OnceCell<StructuralIndex>` (1f-anti-pattern AP17-002) must become the tape's `offsets` (index IS the tape) or `local_temp_only` — never a retained index parallel to a wired tape. The fold inherits REDRESS-53. |
| Retained class-column / streaming cursor / class-lane (REDRESS 96/97/98) | Lock 1 binding history: "Full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, and UnionTape-style retained structures are not shortlist-safe" (`restart/locks/LOCKS.md:129-135`); generalized to ALL transient classifier-state primitives (`:142-144`). | The fold's tape must not introduce a class column or streaming cursor; the proven `Tape` carries NO class column (p2d `:34-49` — six members, one position-keyed vector). Do not re-derive a class-column tape for crates/core. |
| AZ-IV eager value tree (118× regression) | SK-V17 global block: "AZ-IV eager value tree: eager per-leaf payload, f64-alloc-per-number, per-color Box<CssColor>. Materialization stays lazy-by-default" (`restart/skinny/tranches/sk-v17/SPEC.md:791-793`). | The crates/core CSS builder's eager `CssTypedValue` enum + `pending_*` Vecs (`crates/core/src/runtime/css_l4/builder.rs:74-79`, `value.rs:414`) IS the eager-value-tree shape; the fold must replace it with lazy `ValueRef<G>` projection (COH17-002), NOT carry the eager tree forward. |
| x86 / AVX-512 / SVE not the target | alphaC §6: existing x86_64 AVX2/AVX-512 classify modules exist in bbnf-simd "but are not the admission target. The architecture is aarch64 Apple M5 Max only; SVE-disallowed" (`restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:307-316`); SPEC §9 W3 pre-block + REJECTed set (`:806,:826,:854`). | The §7.3 CollapsedStage x86/AVX-512 path (COH17-004) and crates/simd-scan's avx2/avx512/wasm kernels (COH17-005) are NOT the SK-V17 admission target; the fold must not re-derive an aarch64 CollapsedStage or admit x86 as the close path. The aarch64 CollapsedStage question is the SPEC-NAMED **UNKNOWN-2D-05** (`restart/ARCHITECTURE.md:1206` "aarch64 candidate is UNKNOWN-2D-05; requires 2E source-backed aarch64 strategy before any aarch64 admission") — a recorded open unknown, NOT a fresh decision. T-P2 decides totality scope of the multi-arch kernels (architecture pressure, not admission). |
| Second substrate (D6) | SK-V17 §9 REJECTed candidates: "D6 second substrate" (`restart/skinny/tranches/sk-v17/SPEC.md:854`); §9 second-substrate global block enumerates `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public UnionTape, new substrate APIs, sixth BackendShape (`:807-811`). | NOTE: the §9 second-substrate block names skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor` as FORBIDDEN-in-skinny constructs — these are precisely the crates/core fold-target names (1f COH17-001/006). The fold reconciliation must NOT relocate the crates/core `TapeStructBuilder`/`TapeCursor` INTO skinny as a second substrate; SK-V18 adopts the PROVEN skinny `Tape`/`ValueRef` shape into crates/core, not vice-versa (SPEC `:110-114` direction is monotonic skinny→totality). |
| StructLayout/OpenFrame/CssArena are totality-only | alphaC: grep returns ZERO for `StructLayout`, `OpenFrame`, `CssArena`, `begin_compound`, `TapeStructBuilder` on the skinny benched surface; "every pre-block below is keyed to the skinny benched surface" (`restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:20-25`). Lock 2 retired `StructLayout` → `Layout` (`:29`). | The 1F-coherence COH17-006 StructLayout-name divergence is confirmed past-corpora: these are crates/core-tree names. The fold must NOT introduce them into skinny; the totality-tree carries the retired name and the AoS `TapeRec` shape pending SK-V18. |

## Direction-Monotonicity Note (CH3 regression guard)

Per SK-V17 SPEC §0.1.11 and §8 downstream (`:110-114`, `:779-780`) and the
totality pass §8.5 ("skinny → totality direction is monotonic"), the fold flows
skinny-proven → crates/core, never the reverse. The crates/core constructs
excavated here (`TapeStructBuilder`, AoS `TapeRec`, `StructLayout`, eager
`OpenFrame` builders, `OnceCell<StructuralIndex>`) are the FOLD TARGET state, not
proposals; SK-V17 proves the SoA `Tape`/`ValueRef<G>`/`select_classifier` model in
skinny, and SK-V18 absorbs it into crates/core. T-P1 catalogues both sides; it does
not dictate back to a live skinny iteration.

## Prior-Totality Continuity (SK-V14 1F)

The prior totality 1F (`restart/audit/totality/p1/1F-coherence-scan.md`, cycle V4)
named COH-014 root `OnceCell<StructuralIndex>` coupling (`:87`) and COH-008
BackendShape depth (`:81`); both re-anchor here as AP17-002 and COH17-004
respectively, independently re-cited at current crates/core line positions. The
SK-V14 1F is historical ledger; this SK-V17 1F is the live inventory for the
tape/value-API/NEON fold scope. No SK-V14 finding is re-derived without a current
crates/core re-anchor.

**DO-NOT-CARRY-UNDERCOUNT flag (CH3 regression guard, V3 fold):** prior COH-014
itself enumerated JSON + Google Sheets as `OnceCell<StructuralIndex>` carriers —
which ALREADY contradicts the sibling V2 1f-coherence Gaps row claim of
"json/ebnf/bnf/csv only". A do-not-redrive scan catches this: the V3 live truth is
that ALL 8 generated grammars carry the scan + `OnceCell<StructuralIndex>`
(`crates/core/src/grammar/generated/{json,css_l4,ebnf,bnf,csv,css_pretty,google_sheets,bbnf}.rs`
— scan calls at json:732, css_l4:15982, ebnf:1381, bnf:848, csv:566,
css_pretty:1905, google_sheets:3559, bbnf:4843). The V2 sibling Gaps row mis-labelled
a PAID surface (CSS scan IS wired) as unpaid fold cost; V3 deletes/replaces it
(the true residual fact is the missing TAPE consumer, not the scan). T-P2 must
carry the all-8 census, not the V2 undercount.

## Second-Substrate Carrier Enumeration (CH5-S/§5 fold — all 8)

The Lock-1 firewall scope for the retained `OnceCell<StructuralIndex>` sidecar must
enumerate ALL 8 carriers, not a 4-grammar sample: `json.rs:701` (field),
`css_l4.rs:15951`, `ebnf.rs:1335`, `bnf.rs:802`, `csv.rs:520`, `css_pretty.rs:1859`,
`google_sheets.rs:3513`, `bbnf.rs:4797`. U-AP17-001's `substrate_target`
classification verify_action scopes to all 8, NOT json.rs alone. Each must become
the tape's `offsets` (index IS the tape, Lock 1) or `local_temp_only`; a retained
index parallel to a wired tape is REDRESS-53 (SPEC `:825,:837-840`).
