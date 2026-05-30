---
lens: CH4-COST
pass: T-P2-research
cycle: V3
reviewer: CH4 COST (V3)
subject: SK-V17 T-P2 RESEARCH fold dossiers (2a..2f)
master_head: 91b6893b0
t_p1_input_locked: 91b6893b0 (T-P1 CONVERGED)
generated_at: 2026-05-29T00:00:00Z
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 CH4 + restart/prompts/ORCHESTRATOR.md §3W/§3Z
scope: each fold carries propagation surface + LOC class + scalar-ref+checkasm (NEON) + same-wave consumer
dossiers_reviewed: [2a, 2b, 2c, 2d, 2e, 2f]
prior_cycle: V2 (CH4 = 39 ACCEPT / 1 REVISE / 0 REJECT = 97.5%)
fold_proposals_dispositioned: 40
accept: 40
revise: 0
reject: 0
verdict_pct_accept: 100.0
prior_dispositions_folded: [CH4-V2-001]
---

# CH4 COST — SK-V17 T-P2 RESEARCH Hardening (V3)

## Lens charter (PASS-2-RESEARCH §3 CH4)

> Every grounded primitive carries an admission cost: scalar reference +
> checkasm parity per Lock 16; a same-wave consumer is named; LOC/risk for
> adoption is realistic; no orphan-kernel research.

CH4 scans exactly the cost surface, per fold-proposal: (1) is the propagation/LOC
class realistic and grounded; (2) where the fold touches a NEON primitive, is
there a scalar reference + checkasm parity; (3) is a same-wave consumer named;
(4) is the proposal free of orphan-kernel research. CH4 does NOT re-litigate
generality (CH2), regression (CH3), or hidden coupling (CH5).

The cost surface entering V3 was already at 97.5% — the S-P2 LOCKED L1–L9 manifest
schema {scalar-ref · checkasm · same-wave consumer · LOC · abrogate} carries the
discipline by construction, and all six V1 CH4 non-ACCEPTs discharged in V2. V3
carries exactly ONE inherited REVISE (CH4-V2-001) to verify-folded, plus a
full live re-verification of every load-bearing cost fact at master `91b6893b0`.

## V2→V3 fold verification — the single CH4 V2 REVISE discharged

V2 CH4 returned 1 REVISE (CH4-V2-001) with a file:line + concrete fix. It is
folded in the V3 dossier, and its load-bearing source fact re-greped live:

| V2 finding | V2 verdict | V3 fold location | live re-verification | discharged |
|---|---|---|---|---|
| **CH4-V2-001** 2f F2 carried the lazy-`ValueRef<G>` cost as a single "300-700 LOC" figure that did NOT distinguish generator-side LOC from the per-grammar regen × 8 output LOC, unlike the isomorphic ACCEPTED 2a FOLD-2A-C / 2e FOLD-2E-C | REVISE | `2f:190-192` (F2 Lock-surface row) now reads: *"Risk: high; generator-LOC (one accessor generator) distinguished from regen-LOC (per-grammar value.rs/view.rs/document.rs × 8); ~300-700 LOC."* — the exact 2e FOLD-2E-C split (`2e:229-230`) | `2e:229-230` = "300-700 LOC generator-side + per-grammar regen across 8 grammars; generator-LOC distinguished from regen-LOC"; `2a:199` = "300-700 LOC GENERATOR-SIDE (the template body in the accessor generator)"; `2f:190-192` now mirrors both — the generator-vs-regen split is present in all three isomorphic folds | ✅ |

**Fold-log placement note (below disposition threshold).** The 2f F2 prose
discharges CH4-V2-001, but the dossier's `v3_fold_log` frontmatter (`2f:19-20`)
enumerates only `CH1-2F-01-RESIDUAL` and does NOT list CH4-V2-001 by id. This is a
fold-log bookkeeping omission, not a fold defect — the *cost fix itself is present
and correct in the F2 body*. I note it as Observation O-2; it is not a REVISE
(the substantive fold is done; a missing fold-log line item does not re-open the
cost gap, which is closed in the prose that T-P3 reads). The V2-discharged set
(CH4-2f-001 F7 consumer) remains correctly logged at `2f:24`.

## Live cost-fact re-verification at master 91b6893b0

Every load-bearing V3 cost claim re-greped live before disposition:

| cost fact | dossier claim | live verification | verdict |
|---|---|---|---|
| eq-set fan is THE one real NEON Layer-1 body | 2a/2b/2c/2e/2f | `aarch64/byte_class_from_eq_set_64.rs` = **87 LOC**; distinct genuine NEON intrinsic set = **8** `{vld1q_u8, vceqq_u8, vandq_u8, vorrq_u8, vdupq_n_u8, vget_low_u8, vget_high_u8, vaddv_u8}` (a 9th grep hit `vemask_u8` is a FALSE match — the `movemask_u8x16` helper fn at `:79`, not an intrinsic) | CONFIRMED (8 distinct, 87 LOC) |
| `byte_class_from_table_64` / `bitmap_prefix_xor_64` aarch64 are scalar passthroughs, NOT NEON bodies | 2a `:264-265`, 2b `:93,:164`, 2c `:227-228`, 2e `:320-324` | both `*_neon` fns delegate on **line 3** to `crate::scalar::*` (`byte_class_from_table_64.rs:3` → `byte_class_from_table_64_scalar`; `bitmap_prefix_xor_64.rs:3` → `bitmap_prefix_xor_64_scalar`) | CONFIRMED (line-3 delegate) |
| L5/L6 (`comment_body_mask_64`/`bracket_depth_mask_64`) scalar+checkasm genuinely ABSENT (REQUIRED-NEW honest) | 2b FOLD-L5/L6 | `scalar/comment*` / `scalar/bracket*` = none; `checkasm_comment*` / `checkasm_bracket*` = none | CONFIRMED (absent) |
| `escape_mask_64` carry idiom is `lib.rs`-resident, no `scalar/` sibling (CH4-2b-001 V1 fold) | 2b FOLD-L5 `:259` | `fn escape_mask_64` only in `skinny/crates/bbnf-simd/src/lib.rs`; `scalar/escape_mask_64.rs` absent | CONFIRMED |
| CSS builder = 817 LOC, JSON builder = 231 LOC (the eager OpenFrame retirement surface, F1/2a-B/2e-B) | 2a/2e/2f F1 | `css_l4/builder.rs` = **817**; `json/builder.rs` = **231** (1048 total) | CONFIRMED (exact) |
| activation engine = `backend_egraph` 311 + `decision_csp` 273 = 584 LOC present-but-inert (F8/2d-02 600-1400 LOC activation) | 2d `:117,:278`, 2f F8 `:434-436` | `skinny/crates/passes/src/backend_egraph.rs` = **311**; `skinny/crates/passes/src/decision_csp.rs` = **273** (584 total) | CONFIRMED (exact) |
| `perf_cost:0` persists at HEAD (2d activation-not-derivation refutation current) | 2d Refuted `:278` | `decision_csp.rs:242,:254`; `lib.rs:584,1952,1964,1991,2012` — 7 sites | CONFIRMED (persists) |
| `enum BackendShape` / `derive_backend_shape` absent in core, present skinny only (fold WIRES, not builds) | 2f F8 `:426-428` | `grep enum BackendShape crates/` = **0**; `grep derive_backend_shape crates/` = **0**; both present `skinny/crates/{ir/src/lib.rs:340, passes/src/lib.rs:392}` | CONFIRMED |
| `EmitStrategy::StructDirect` is the single core variant the selector wires atop | 2f F8, 2d-S17-03 | `strategy.rs:107` StructDirect variant; `:265` `return EmitStrategy::StructDirect` | CONFIRMED |
| StructLayout 960-site rename surface; LayoutFacts/backend_shape grep-zero in crates/ (F9 path-(b) sizing) | 2f F9 `:464,:472`, 2a `:11` | `grep StructLayout crates/` = **960**; `grep LayoutFacts crates/` = **0**; `grep backend_shape crates/core crates/ir` = **0** | CONFIRMED (exact) |
| `arena.rs:47` live coupling-site FOLD-B severs (CH5-V1-003 fold, F6) | 2f F6 `:359`, 2a/2c/2d/2e | `crates/core/src/runtime/bbnf/arena.rs:47` = `match StructRegistry::compound_kind_for_layout(layout) {` | CONFIRMED (live) |
| `begin_compound` registry-free fence reads `layout.rule_id & 0x1F` only (F6 regression firewall) | 2f F6 `:339`, 2a/2e | core `tape/mod.rs:185-186` = `fn begin_compound(&mut self, layout: &StructLayout)` then `(layout.rule_id & 0x1F) as u8`; no `StructRegistry` in the body | CONFIRMED |
| AoS `TapeRec` 16-byte vs SoA `Tape` 6-member encodings (Divergence A, F3) | 2f F3 `:202-204` | `record.rs:103` `struct TapeRec` + `:120` `size_of==16` + `:121` `align_of==4`; skinny `tape/mod.rs:94-100` = `{source, offsets, flag_cursors, flag_values, payloads, id}` (6 members) | CONFIRMED |

**Provenance of the V3 cost surface is clean.** No confabulated scalar-ref, no
phantom checkasm, no fabricated LOC envelope. Every numeric cost fact (87, 8, 817,
231, 311, 273, 584, 960, 0) is exact at HEAD. The eq-set-fan intrinsic-count
sharpening this cycle (the V1/V2 "12 NEON intrinsics" → V3 "8 distinct NEON
intrinsics across 27 calls", `2a:263,:271-273`) is a correctness improvement, not a
cost inflation — the genuine distinct intrinsic set IS 8 (verified live; the prior
"12" over-counted), and the 87-LOC body figure is unchanged. The cost claim turns
on the 87-LOC body + scalar-ref + checkasm, none of which the recount disturbed.

---

## Per-dossier fold-proposal dispositions (V3)

### 2b — Primitive-Vocabulary Fold (the Lock-16 cost spine)

| proposal | scalar-ref | checkasm | same-wave consumer | LOC/risk | V3 disposition |
|---|---|---|---|---|---|
| FOLD-L1 eq-set classifier | `scalar/byte_class_from_eq_set_64.rs` ✓ | strict checkasm ✓ | L2 tape build (same-wave) | wired, 0-LOC primitive | **ACCEPT** |
| FOLD-L2 tape-append op | N/A-substrate-op | tape↔fact-stream + cssparser 8-field + `write_count==0` | L3 same-wave | substrate-op | **ACCEPT** |
| FOLD-L3 lazy `ValueRef<G>` | N/A-cursor (`value_from_ref` JSON ref) | cssparser 8-field round-trip | the projection consumer (W2) | 300-700 LOC generator | **ACCEPT** |
| FOLD-L4 tokenize-once reuse | N/A-consumption | cssparser equality (output-invariant) | the tape (index IS offsets) | consumption pattern | **ACCEPT** |
| FOLD-L5 `comment_body_mask_64` | REQUIRED-NEW; idiom anchored `lib.rs:175` ✓ | REQUIRED-NEW (absent — verified) | L1 composition same-wave | ~30-60 LOC, wired-if-W3-lands | **ACCEPT** |
| FOLD-L6 `bracket_depth_mask_64` | REQUIRED-NEW (absent) | REQUIRED-NEW (absent) | L1/index consumer (W3) | wired-if-W3-AND-abrogate-clears (`:303-307`) | **ACCEPT** |
| FOLD-L7 one-shot SIMD reserve | consumes `scan_structurals_scalar` count ✓ | L1 differential | L2 (the tape it sizes) | gated behind L2/L3 | **ACCEPT** |
| FOLD-L8 sparse-flag side-table | N/A-substrate-op | corpus-parity analogue | L3 same-wave | substrate-op | **ACCEPT** |
| FOLD-L9 Alt-mode (DEFERRED appendix) | N/A-codegen | recognizer-output equality | GATED on re-profile (none) | recorded-not-shortlisted (`:344-372`) | **ACCEPT** |
| A4 / LAC-2b-SKV17-03 scalar-delegate close-state | ✓ live | n/a | n/a | manifest rows | **ACCEPT** |
| Refuted: `udot`/i8mm digit MAC (no CSS antecedent, `:166`) | n/a | n/a | correctly refused (orphan) | — | **ACCEPT** |
| Refuted: FSM/frame macros | absent | absent | absent | — | **ACCEPT** |
| LAC-2b-SKV17-01/02/04 | per-row | per-row | per-row | doc/manifest | **ACCEPT** (×3) |

All three V1 REVISEs remain discharged at V3: L5's idiom anchored to `lib.rs`
(no `scalar/` sibling, re-verified), L6's abrogate threshold present (`:303-307`,
"DELETE … if the scalar running-balance body does not move the … scan leaf on a
profiled W3 antecedent by a measurable margin"), L9 in the deferred appendix
(`:344`, "The wired FOLD-Ln enumeration ends at L8"). All 14 rows ACCEPT.

### 2a — SOTA Landscape Fold

| proposal | V3 disposition | note |
|---|---|---|
| FOLD-2A-A flat tape | **ACCEPT** | 200-600 LOC SK-V18 |
| FOLD-2A-B eager retirement | **ACCEPT** | 817 (css) + 231 (json) = 1048-LOC demolition + 22 files |
| FOLD-2A-C lazy ValueRef | **ACCEPT** | `:199` 300-700 LOC GENERATOR-SIDE distinguished from per-grammar regen |
| FOLD-2A-D substrate-manifest | **ACCEPT** | 0-LOC canon; ARCH:1206 anchor |
| FOLD-2A-E NEON Lock-16 entry | **ACCEPT** | per-primitive table `:263-265`; eq-set (87 LOC / 8 distinct intrinsics) = only NEON body; two delegates filed `scalar-delegate-non-ASM` |
| FOLD-2A-F FieldSource fence | **ACCEPT** | 0-LOC fence; arena.rs:47 live coupling-site named |
| 3 LACs | **ACCEPT** (×3) | LAC-2A-SKV17-02 carries the per-primitive split + 8-distinct-intrinsic precision |

FOLD-2A-E's per-primitive table (the V1 REJECT → V2 ACCEPT conversion) holds at V3
and is sharpened: the eq-set fan row now states "8 distinct NEON intrinsics across
27 calls" (verified: 8 distinct), the two delegates filed `scalar-delegate-non-ASM`
with no NEON consumer to claim. ACCEPT.

### 2e — Greater-Spec Tape/NEON/Projection Fold

| proposal | V3 disposition | note |
|---|---|---|
| FOLD-2E-A flat-tape adoption | **ACCEPT** | substrate; sequenced WITH 2E-B |
| FOLD-2E-B eager retirement | **ACCEPT** | 817+231 LOC delete + regen |
| FOLD-2E-C lazy ValueRef | **ACCEPT** | `:229-230` 300-700 LOC generator-side + per-grammar regen; generator-LOC distinguished from regen-LOC (the CH4-V2-001 anchor sibling) |
| FOLD-2E-D substrate-manifest not 6th shape | **ACCEPT** | 0-LOC canon + selector |
| FOLD-2E-E shared NEON classifier | **ACCEPT** | close-state taxonomy `:320-324`; eq-set only NEON body; delegates 3-line passthroughs (`aarch64/*.rs:3`) |
| FOLD-2E-F StructLayout/FieldSource fence | **ACCEPT** | 0-LOC; arena.rs:47 live wire named |
| LAC-2E-SKV17-01..04 | **ACCEPT** (×4) | LAC-02 carries `retention_lifetime=transient-single-call` (CH5-V2-001 cohort-align fold) |

FOLD-2E-E's close-state taxonomy holds and now carries the canonical Lock 1 v+1
`retention_lifetime=transient-single-call` token (CH5-V2-001 cohort-align fold,
`2e:30,:495`), aligning the classifier surface with sibling 2a/2f primitive enums.
This is a declaration-precision improvement, not a cost change. ACCEPT.

### 2f — Greater-Spec Fold-Gaps (F1–F9)

| proposal | V3 disposition | note |
|---|---|---|
| F1 eager retirement | **ACCEPT** | high, 300-700 LOC + 22 files; 817+231 demolition |
| F2 lazy ValueRef | **ACCEPT** (was CH4-V2-001 REVISE) | `:190-192` now distinguishes generator-LOC from per-grammar regen × 8 — the 2e-C/2a-C split adopted |
| F3 AoS↔SoA | **ACCEPT** | 200-600 LOC; AoS 16-byte / SoA 6-member verified |
| F4 tape=substrate-category | **ACCEPT** | 0-LOC; independent `admits_collapsed_stage` x86-bound corroboration `:262-271` |
| F5 shared NEON Lock-16 row | **ACCEPT** | eq-set scalar+checkasm ✓; `transient-single-call` consumer = the tape |
| F6 FieldSource fence | **ACCEPT** | 0-LOC; arena.rs:47 live wire named; begin_compound registry-free verified |
| F7 OnceCell substrate_target classify | **ACCEPT** | consumer = co-waved F1/F3 tape-wiring `:406-414` |
| F8 BackendShape selector wiring | **ACCEPT** | 584-LOC engine present (311+273), fold WIRES; 60-200 + 600-1400 joint |
| F9 StructLayout reconcile | **ACCEPT** | 960-site (exact); path-(b) 0→N introduce-delta sizing route `:475-490` |
| LAC-2F-FOLD-01..05 | **ACCEPT** (×5) | per-target |

F2 discharges CH4-V2-001 (the cost fix is in the prose at `:190-192`). All other
F-rows carry a named same-wave consumer + realistic LOC class. F8's 600-1400 LOC
"joint, non-additive" envelope with F9's 960-site rename is correctly framed as a
shared surface (not summed) per 2d `:288`. All 14 rows ACCEPT.

### 2d — Cost-Model + 5-shape BackendShape Fold

All 7 FOLD-2D rows + 3 LACs ACCEPT. The activation-not-derivation refutation
(`2d:278`) is current: `backend_egraph.rs:65-67` runs 0 rewrite rules,
`decision_csp.rs:53-83` self-selects then accepts all, `perf_cost:0` persists at 7
sites (re-greped live). The 600-1400 LOC joint, non-additive envelope (BSHAPE17-002
⊕ 003) is a realistic activation cost for the 584-LOC present-but-inert engine.
This remains the single most honest cost claim in the dossier set. ACCEPT.

### 2c — Grammar-Neutrality Fold

All 6 candidates (SK17-2C-A..F) + ONBOARD verify-action + 2 LACs ACCEPT. 2c's
candidates inherit their cost surface from 2a/2e/2f; Candidate-E correctly binds the
CSS non-JSON consumer to the eq-set fan (the one real NEON body) via the slot-59
collision (`SPEC.md:316-317`), with `byte_class_from_table_64` aarch64 entry
recorded `scalar-delegate-non-ASM` (CH2-V1-R4 fold, `2c:33,:227-234`). No orphan
primitive, no unpriced NEON body. ACCEPT.

---

## Disposition census (V3)

| dossier | ACCEPT | REVISE | REJECT | total |
|---|---|---|---|---|
| 2a | 10 | 0 | 0 | 10 |
| 2b | 14 | 0 | 0 | 14 |
| 2c | 8 | 0 | 0 | 8 |
| 2d | 10 | 0 | 0 | 10 |
| 2e | 10 | 0 | 0 | 10 |
| 2f | 14 | 0 | 0 | 14 |
| **dedup total** | **40** | **0** | **0** | **40** |

**ACCEPT 40 / REVISE 0 / REJECT 0 = 100% ACCEPT, 0% non-ACCEPT.**

The cost surface is fully converged from the CH4 lens. The single inherited V2
REVISE (CH4-V2-001) is discharged with a live-verified, sibling-isomorphic fold;
every load-bearing cost fact re-verified exact at master `91b6893b0`; zero new
REVISE; zero orphan disposition.

## Orphan-REVISE check (ORCHESTRATOR §3Z)

Zero REVISE this cycle → zero orphan REVISE by construction. The two Observations
below are explicitly BELOW the disposition threshold (no fold change, no cost gap),
recorded for T-P3 deftness only — not orphan dispositions.

## Observations (below disposition threshold — noted, not dispositioned)

**O-1 (scalar-delegate LOC-count drift persists).** The two scalar-delegate bodies
are described as "line-3" / "3-line" (2a `:264`, 2e `:320-324`, 2f), "4-LOC"
(2b `:164`), and "2-LOC" (2c `:227-228`). Live: each `*_neon` fn delegates on
**line 3** to `crate::scalar::*`. The close-state (`scalar-delegate-non-ASM`, no
NEON consumer) is correct in all five dossiers and no cost claim turns on the
literal count. This is the V2 O-1 carried forward unchanged. A deft one-token
harmonization to "line-3 delegate" at T-P3 synthesis would close the cosmetic
drift; it is not a REVISE (the cost framing is identical and correct everywhere).

**O-2 (2f fold-log omits CH4-V2-001 by id).** The 2f `v3_fold_log` frontmatter
(`2f:19-20`) enumerates `CH1-2F-01-RESIDUAL` only; it does not list CH4-V2-001 as a
folded id, though the CH4-V2-001 cost fix IS present and correct in the F2 body
(`2f:190-192`). Bookkeeping omission, not a fold defect — the cost gap T-P3 reads
is closed in the prose. Below threshold; a one-line fold-log addition at T-P3 would
make the discharge auditable from the frontmatter, but the substantive fold is done.

**O-3 (eq-set-fan intrinsic count sharpened, correctly).** The V1/V2 dossiers said
"12 NEON intrinsics"; V3 sharpens to "8 distinct NEON intrinsics across 27 calls"
(`2a:263`). Live verification confirms the genuine distinct set is **8** (the
earlier "12" over-counted; one grep-adjacent token, `vemask_u8`, is the
`movemask_u8x16` helper, not an intrinsic). This is a correctness gain, not a cost
inflation — the 87-LOC body figure the cost turns on is unchanged. Noted as a
positive (the precision moved toward the live truth, not away).

## CH4 cross-cutting findings (V3)

1. **The cross-dossier scalar-delegate-vs-NEON-body split is uniformly held.**
   2a `:263-265`, 2b A4/`:93,:164`, 2c `:227-234`, 2e `:320-324`, and 2f F5 all
   state the eq-set fan (87 LOC) as the one NEON body and the two delegates as
   `scalar-delegate-non-ASM` (line-3 passthroughs). The Lock-16 cost is realistic
   across the full cohort — the load-bearing V1 cross-cutting finding is closed and
   stays closed through V3.

2. **The cost-selector activation framing holds at HEAD.** `perf_cost:0` persists
   (7 sites, re-greped); the 584-LOC engine (311+273, exact) is present-but-inert
   in skinny, absent in core; F8/2d price the fold as the 600-1400 LOC activation,
   not the enum. No regression from V2.

3. **REQUIRED-NEW cost stays fully bound.** L5's idiom is anchored to the live
   `lib.rs` home (no `scalar/` sibling — re-verified); L6 carries the explicit
   abrogate threshold (`2b:303-307`) tying the REQUIRED-NEW LOC + checkasm to a
   measured W3 self-time reduction with a deletion arm. Both scalar+checkasm are
   genuinely absent (verified), both gated `wired-if-W3-lands`. No open-ended cost.

4. **No live orphan kernel admitted; no REDRESS route re-opened.** The `udot`/i8mm
   digit MAC (no CSS antecedent) is correctly refused (`2b:166`); FOLD-L9 (Alt-mode,
   no live consumer) is in the deferred appendix, not the wired set; F7's pre-gate
   consumer is the co-waved F1/F3 wiring. The aarch64-only discipline holds (x86
   `crates/simd-scan` kernels are architecture-pressure, never a close path); the
   AZ-IV eager / StructRegistry-indirection / D6 second-substrate pre-blocks are
   all preserved as fences, not re-driven.

## Verdict

CH4 COST returns **100% ACCEPT** (40/40), **0 REVISE / 0 REJECT**, zero orphan
REVISE (ORCHESTRATOR §3Z). The single inherited V2 REVISE (CH4-V2-001) is
discharged: 2f F2 (`:190-192`) now distinguishes generator-side LOC from
per-grammar regen × 8 output LOC, isomorphic to the already-ACCEPTED 2a-C/2e-C.
Every load-bearing cost fact re-verified EXACT at master `91b6893b0` (87, 8, 817,
231, 311, 273, 584, 960, 0). The S-P2 LOCKED L1–L9 manifest schema enforces
{scalar-ref · checkasm · same-wave consumer · LOC · abrogate} per primitive; the
scalar-delegate-vs-NEON-body split is uniform across 2a/2b/2c/2e/2f; the
activation-not-derivation framing is current; no REDRESS route re-opened; the
aarch64-only discipline holds throughout; preserve-rich-ast and the
no-6th-BackendShape constraint are honoured.

From the CH4 lens this is the **second consecutive cycle at the §4 ≥95% bar**
(V2 = 97.5%, V3 = 100%), with zero open critical defects and zero orphan REVISE —
the cost surface satisfies the convergence criterion. The three Observations are
explicitly below the disposition threshold (cosmetic LOC-token drift, a fold-log
bookkeeping omission, and a correctness-positive intrinsic-count sharpening) and
require no fold change.
