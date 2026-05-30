---
lens: CH4-COST
pass: T-P2-research
cycle: V2
reviewer: CH4 COST (V2)
subject: SK-V17 T-P2 RESEARCH fold dossiers (2a..2f)
master_head: 91b6893b0
t_p1_input_locked: 91b6893b0 (T-P1 CONVERGED)
generated_at: 2026-05-29T00:00:00Z
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 CH4 + restart/prompts/ORCHESTRATOR.md §3W/§3Z
scope: each fold carries propagation surface + LOC class + scalar-ref+checkasm (NEON) + same-wave consumer
dossiers_reviewed: [2a, 2b, 2c, 2d, 2e, 2f]
prior_cycle: V1 (CH4 = 33 ACCEPT / 6 REVISE / 1 REJECT = 82.5%)
fold_proposals_dispositioned: 40
accept: 39
revise: 1
reject: 0
verdict_pct_accept: 97.5
prior_dispositions_folded: [CH4-2b-001, CH4-2b-002, CH4-2b-003, CH4-2e-001, CH4-2f-001, CH4-2a-001]
---

# CH4 COST — SK-V17 T-P2 RESEARCH Hardening (V2)

## Lens charter (PASS-2-RESEARCH §3 CH4)

> Every grounded primitive carries an admission cost: scalar reference +
> checkasm parity per Lock 16; a same-wave consumer is named; LOC/risk for
> adoption is realistic; no orphan-kernel research.

CH4 scans exactly the cost surface, per fold-proposal: (1) is the propagation/LOC
class realistic and grounded; (2) where the fold touches a NEON primitive, is
there a scalar reference + checkasm parity; (3) is a same-wave consumer named;
(4) is the proposal free of orphan-kernel research. CH4 does NOT re-litigate
generality (CH2), regression (CH3), or hidden coupling (CH5).

## V1→V2 fold verification — every V1 CH4 disposition discharged

V1 CH4 returned 6 REVISE + 1 REJECT. Each was a concrete file:line + fix. Every
one is folded in the V2 dossiers, and each fold's load-bearing source fact was
re-greped live at master `91b6893b0` before this V2 disposition:

| V1 finding | V1 verdict | V2 fold location | live re-verification | discharged |
|---|---|---|---|---|
| **CH4-2b-001** FOLD-L5 mis-cited `escape_mask_64` scalar idiom path | REVISE | `2b:249-251,:256-258` re-anchors the carry idiom to `skinny/crates/bbnf-simd/src/lib.rs:175` (`overflowing_add` `:188,:190`); states no `scalar/escape_mask_64.rs` sibling exists | `grep escape_mask_64 lib.rs` → `:175`; `overflowing_add` → `:188,:190`; `ls scalar/escape_mask_64.rs` → absent — **idiom is `lib.rs`-resident, reanchor exact** | ✅ |
| **CH4-2b-002** FOLD-L6 REQUIRED-NEW lacked abrogate threshold | REVISE | `2b:278-283` adds the explicit abrogate gate: DELETE the REQUIRED-NEW primitive if the scalar running-balance body does not move the `consume_balanced_at` arm of the ~69% scan leaf on a profiled W3 antecedent by a measurable margin | the PV17-007 schema mandate (`2b:86`) now carries `abrogate threshold`; the gate binds LOC/checkasm cost to a measured self-time reduction | ✅ |
| **CH4-2b-003** FOLD-L9 Alt-mode enumerated among wired folds | REVISE | `2b:319-347` — wired enumeration explicitly ends at L8; FOLD-L9 demoted to a "Deferred-Pending-Reprofile Appendix" marked DEFERRED / `recorded-not-shortlisted` | the appendix header `:323` + `:319-321` close-statement prevent T-P3 reading L9 as shortlisted | ✅ |
| **CH4-2e-001** FOLD-2E-E priced table/prefix-xor as NEON-body rows | REVISE | `2e:271-296` adopts the 2B close-state taxonomy: eq-set fan = the one NEON body; `byte_class_from_table_64`/`bitmap_prefix_xor_64` filed `scalar-delegate-non-ASM`, NOT priced as NEON-body rows | both `*_neon` fns are line-3 delegates to `crate::scalar::*` (verified) — taxonomy split now correct | ✅ |
| **CH4-2f-001** F7 same-wave consumer was bare "the tape" | REVISE | `2f:404-412` names F7's consumer as the co-waved F1/F3 tape-wiring (the classification GATES the wiring it is co-waved with), not an orphan pre-gate | F7 frontmatter `2f:22` records the fold verbatim; consumer is now concrete + same-wave | ✅ |
| **CH4-2a-001** FOLD-2A-E blanket Lock-16 vocabulary admission, no per-primitive grounding | **REJECT** | `2a:247-258` replaces the blanket admission with a per-primitive table: eq-set fan (87 LOC / 12 intrinsics) = the only NEON-body row; table_64/prefix_xor = `scalar-delegate-non-ASM`; remaining primitives admitted per-row in 2b | `byte_class_from_eq_set_64.rs` = 87 LOC / 12 NEON intrinsics; table/prefix delegates line-3 — per-primitive split is now present + correct | ✅ |

**The REJECT is converted by enumeration, not deletion** (`2a` frontmatter `:21`:
"0 fold deleted — CH4-2a-001 is a REJECT of the AS-WRITTEN over-admission, folded
by enumerating per-primitive"). This is the correct disposition response: the fold
is sound, the over-admission text was the defect, and the per-primitive table
discharges it. The eq-set fan's status as the single NEON body is now stated
identically in 2a, 2b, and 2e — the cross-dossier convergence V1 demanded
(V1 cross-cutting finding #1) is achieved.

## Live cost-fact re-verification at master 91b6893b0

Every load-bearing V2 cost claim re-greped live:

| cost fact | dossier claim | live verification | verdict |
|---|---|---|---|
| `escape_mask_64` scalar carry idiom is `lib.rs`-resident, no `scalar/` sibling | 2b FOLD-L5 (CH4-2b-001 fold) | `lib.rs:175` fn + `overflowing_add :188,:190`; `scalar/escape_mask_64.rs` absent | CONFIRMED |
| `compound_kind_for_layout` live coupling-site (CH5-V1-003) | 2a/2e/2f F6 | `arena.rs:47` `match StructRegistry::compound_kind_for_layout(layout)` present | CONFIRMED |
| eq-set fan is the one real NEON body | 2a/2b/2e | `byte_class_from_eq_set_64.rs` = 87 LOC, 12 NEON intrinsics (`vld1q_u8`/`vceqq_u8`/`vorrq_u8`) | CONFIRMED |
| table_64 / prefix_xor_64 aarch64 are scalar passthroughs | 2a/2b/2e | both `*_neon` fns delegate at line 3 to `crate::scalar::*` | CONFIRMED |
| L5/L6 scalar+checkasm genuinely ABSENT (REQUIRED-NEW honest) | 2b FOLD-L5/L6 | `scalar/{comment,bracket}*` = none; `checkasm_{comment,bracket}*` = none | CONFIRMED |
| `perf_cost:0` persists at HEAD (2d refutation current) | 2d Refuted, FOLD-2D-02 | `lib.rs:584,1952,1964,...` multiple sites | CONFIRMED |
| F8 decision engine = 584 LOC present (skinny), 0 in core | 2f F8 | `backend_egraph.rs`=311 + `decision_csp.rs`=273=584; `enum BackendShape` 0 in `crates/`, present skinny only; `derive_backend_shape` skinny only | CONFIRMED |
| `EmitStrategy::StructDirect` is the single core variant | 2f F8 | `strategy.rs:107` StructDirect; the fold wires the selector atop it | CONFIRMED |
| StructLayout 960-site rename surface | 2a F/2f F9 | `grep -rc StructLayout crates/` = 960 occurrences across 79 files | CONFIRMED (exact) |

**Provenance of the V2 cost surface is clean.** No confabulated scalar-ref, no
phantom checkasm, no fabricated LOC envelope. The one minor LOC-count drift V1
flagged (the scalar-delegate body is described variously as "2-line"/"3-line"/
"4-LOC" across 2a/2b/2e) persists, but it is below the disposition threshold: the
*close-state* (`scalar-delegate-non-ASM`, no NEON consumer) is correct in all
three, and the literal body is a single line-3 delegate statement either way — no
cost-claim turns on the exact LOC count. Noted, not dispositioned (see Observation
O-1 below).

---

## Per-dossier fold-proposal dispositions (V2)

### 2b — Primitive-Vocabulary Fold (the Lock-16 cost spine)

| proposal | scalar-ref | checkasm | same-wave consumer | LOC/risk | V2 disposition |
|---|---|---|---|---|---|
| FOLD-L1 eq-set classifier | `scalar/byte_class_from_eq_set_64.rs` ✓ | strict checkasm ✓ | L2 tape build (same-wave) | wired, 0-LOC primitive | **ACCEPT** |
| FOLD-L2 tape-append op | N/A-substrate-op | tape↔diagnostic-fact-stream + cssparser 8-field + `write_count==0` | L3 same-wave | substrate-op | **ACCEPT** |
| FOLD-L3 lazy `ValueRef<G>` | N/A-cursor (`value_from_ref` JSON ref) | cssparser 8-field round-trip | the projection consumer (W2) | 300-700 LOC generator | **ACCEPT** |
| FOLD-L4 tokenize-once reuse | N/A-consumption | cssparser equality (output-invariant) | the tape (index IS offsets) | consumption pattern | **ACCEPT** |
| FOLD-L5 `comment_body_mask_64` | REQUIRED-NEW, idiom reanchored to `lib.rs:175` ✓ | REQUIRED-NEW (absent — verified) | L1 composition same-wave | wired-if-W3-lands, ~30-60 LOC | **ACCEPT** (was CH4-2b-001) |
| FOLD-L6 `bracket_depth_mask_64` | REQUIRED-NEW (absent) | REQUIRED-NEW (absent) | L1/index consumer (W3) | wired-if-W3-AND-abrogate-clears | **ACCEPT** (was CH4-2b-002; abrogate gate added `:278-283`) |
| FOLD-L7 one-shot SIMD reserve | consumes `scan_structurals_scalar` count ✓ | L1 differential | L2 (the tape it sizes) | gated behind L2/L3 | **ACCEPT** |
| FOLD-L8 sparse-flag side-table | N/A-substrate-op | corpus-parity analogue | L3 same-wave | substrate-op | **ACCEPT** |
| FOLD-L9 Alt-mode (DEFERRED appendix) | N/A-codegen | recognizer-output equality | GATED on re-profile (none) | recorded-not-shortlisted | **ACCEPT** (was CH4-2b-003; demoted to appendix `:323-347`) |
| A4 / LAC-2b-SKV17-03 scalar-delegate close-state | ✓ live | n/a | n/a | manifest rows | **ACCEPT** |
| Refuted: `udot`/i8mm digit MAC (no CSS antecedent) | n/a | n/a | correctly refused (orphan) | — | **ACCEPT** |
| Refuted: FSM/frame macros | absent | absent | absent | — | **ACCEPT** |
| LAC-2b-SKV17-01/02/04 | per-row | per-row | per-row | doc/manifest | **ACCEPT** (×3) |

2b's three V1 REVISEs are fully discharged. The abrogate threshold added to FOLD-L6
(`2b:278-283`) is the single most important cost fix in the cycle: it binds the
REQUIRED-NEW LOC + checkasm cost to a measured self-time reduction on a profiled W3
antecedent, closing the only open-ended cost in the dossier set. All 14 rows ACCEPT.

### 2a — SOTA Landscape Fold

| proposal | V2 disposition | note |
|---|---|---|
| FOLD-2A-A flat tape | **ACCEPT** | 200-600 LOC SK-V18; eager retirement touches 22+ files |
| FOLD-2A-B eager retirement | **ACCEPT** | 1048-LOC demolition (231+817) + 22 files grounded |
| FOLD-2A-C lazy ValueRef | **ACCEPT** | 300-700 LOC generator-side, distinguished from regen-LOC |
| FOLD-2A-D substrate-manifest | **ACCEPT** | 0-LOC canon; independent ARCH:1206 anchor added |
| **FOLD-2A-E NEON Lock-16 entry** | **ACCEPT** (was CH4-2a-001 REJECT) | per-primitive table `:247-258`; eq-set = only NEON body; delegates filed scalar-delegate-non-ASM |
| FOLD-2A-F FieldSource fence | **ACCEPT** | 0-LOC fence; live coupling-site arena.rs:47 named |
| 3 LACs | **ACCEPT** (×3) | LAC-2A-SKV17-02 carries the per-primitive split |

The V1 REJECT is fully converted. FOLD-2A-E `:247-258` now carries the same
per-primitive {close-state · NEON-body? · admission} table 2b carries, with the
eq-set fan as the only NEON-body row and the two delegates filed
`scalar-delegate-non-ASM`. The "no primitive admitted without a scalar reference"
§9 posture is now honoured per-member. ACCEPT.

### 2e — Greater-Spec Tape/NEON/Projection Fold

| proposal | V2 disposition | note |
|---|---|---|
| FOLD-2E-A flat-tape adoption | **ACCEPT** | substrate; sequenced WITH 2E-B |
| FOLD-2E-B eager retirement | **ACCEPT** | 817+231 LOC delete + regen |
| FOLD-2E-C lazy ValueRef | **ACCEPT** | 300-700 LOC generator |
| FOLD-2E-D substrate-manifest not 6th shape | **ACCEPT** | 0-LOC canon + 60-200 selector |
| **FOLD-2E-E shared NEON classifier** | **ACCEPT** (was CH4-2e-001 REVISE) | close-state taxonomy `:271-296`; eq-set only NEON body; delegates scalar-delegate-non-ASM |
| FOLD-2E-F StructLayout/FieldSource fence | **ACCEPT** | 0-LOC; arena.rs:47 live wire named |
| LAC-2E-SKV17-01..04 | **ACCEPT** (×4) | per-target |

FOLD-2E-E `:271-296` now files `byte_class_from_table_64`/`bitmap_prefix_xor_64` as
`scalar-delegate-non-ASM` and admits only the eq-set fan as a NEON-body row with a
real consumer. The inflated NEON-body pricing V1 flagged is removed. ACCEPT.

### 2f — Greater-Spec Fold-Gaps (F1–F9)

| proposal | V2 disposition | note |
|---|---|---|
| F1 eager retirement | **ACCEPT** | high, 300-700 LOC + 22 files |
| F2 lazy ValueRef | **ACCEPT** | 300-700 LOC |
| F3 AoS↔SoA | **ACCEPT** | 200-600 LOC |
| F4 tape=substrate-category | **ACCEPT** | 0-LOC |
| F5 shared NEON Lock-16 row | **ACCEPT** | eq-set scalar+checkasm ✓ |
| F6 FieldSource fence | **ACCEPT** | 0-LOC; arena.rs:47 live wire named |
| **F7 OnceCell substrate_target classify** | **ACCEPT** (was CH4-2f-001 REVISE) | consumer = co-waved F1/F3 tape-wiring `:404-412` |
| F8 BackendShape selector wiring | **ACCEPT** | 584-LOC engine present (311+273), fold WIRES; 600-1400 joint |
| F9 StructLayout reconcile | **ACCEPT** | 960-site (exact), regen-gated |
| LAC-2F-FOLD-01..05 | **ACCEPT** (×5) | per-target |

F7 `:404-412` names the co-waved F1/F3 tape-wiring as the same-wave consumer (the
classification gates the wiring it is co-waved with). No orphan pre-gate. ACCEPT.

### 2d — Cost-Model + 5-shape BackendShape Fold

All 7 FOLD-2D rows + 3 LACs ACCEPT (carried from V1; no V1 CH4 non-ACCEPT existed
for 2d). FOLD-2D-02 prices the cost-selector fold as the **activation** of the
present-but-inert 584-LOC engine (`backend_egraph.rs`=311 + `decision_csp.rs`=273,
re-verified live), explicitly refuting (`2d:270`) that the 0-rule egraph /
`perf_cost:0` / self-accepting CSP proves the model selects the shape —
`perf_cost:0` persists at HEAD across multiple sites. The 600-1400 LOC joint,
non-additive envelope (BSHAPE17-002 ⊕ 003) is a realistic activation cost. This
remains the single most honest cost claim in the dossier set.

### 2c — Grammar-Neutrality Fold

All 6 candidates (SK17-2C-A..F) + the ONBOARD verify-action + 2 LACs ACCEPT. 2c's
candidates are grammar-neutrality groundings, not new primitive admissions; their
cost surface is inherited from 2a/2e/2f. SK17-2C-E correctly anchors the
classifier scalar-ref/checkasm to Lock 16 by reference and (CH2-V1-R4 fold,
`2c:11`) binds the CSS non-JSON consumer to the eq-set fan — the one real NEON
body — via the slot-59 collision, with `table_64` recorded scalar-delegate-non-ASM.
No orphan primitive, no unpriced NEON body. ACCEPT.

---

## Disposition census (V2)

| dossier | ACCEPT | REVISE | REJECT | total |
|---|---|---|---|---|
| 2a | 10 | 0 | 0 | 10 |
| 2b | 14 | 0 | 0 | 14 |
| 2c | 8 | 0 | 0 | 8 |
| 2d | 10 | 0 | 0 | 10 |
| 2e | 10 | 0 | 0 | 10 |
| 2f | 13 | 1 | 0 | 14 |
| **dedup total** | **39** | **1** | **0** | **40** |

**ACCEPT 39 / REVISE 1 / REJECT 0 = 97.5% ACCEPT, 2.5% non-ACCEPT.**

The cost surface converges: all six V1 non-ACCEPTs are discharged with
live-verified fixes; the cross-dossier scalar-delegate-vs-NEON-body split V1
demanded (the load-bearing V1 cross-cutting finding) is now stated identically in
2a, 2b, 2e, and 2c. One new REVISE this cycle (CH4-V2-001 below) is a residual
cost-completeness gap on F2's generator-LOC split, not a fold defect.

## The 1 non-ACCEPT item (orphan-REVISE prevention)

**CH4-V2-001 (2f F2, REVISE).** `restart/.../2f-fold-gaps.md` F2 (lazy `ValueRef<G>`
plane). F2 carries the cost as "high, 300-700 LOC" but, unlike 2a FOLD-2A-C
(`2a:188` "300-700 LOC generator-side + per-grammar regen × 8") and 2e FOLD-2E-C
(`2e:210-211` "300-700 LOC generator-side + per-grammar regen across 8 grammars;
generator-LOC distinguished from regen-LOC"), F2 does NOT distinguish the
generator-side LOC from the per-grammar regen × 8 output LOC. The
[generated-size-budget] memory and the [no-value-discard]/[typed-materialization-
invariant] discipline make this split load-bearing: the generator change is a
bounded one-time edit, the regen output is an O(grammars) artefact with its own
per-tranche line-count budget. As written, F2's single "300-700 LOC" figure reads
as the total fold cost and understates the regen-output propagation that 2a/2e
explicitly carry. **Concrete fix:** F2 must adopt 2e FOLD-2E-C's split — state the
300-700 LOC as the generator-side cost and separately name the per-grammar regen ×
8 output as a distinct (budgeted) propagation surface, so the lazy-projection fold
cost is not read as a single bounded edit. This is a cost-completeness REVISE
(the F2 fold itself is sound and isomorphic to the ACCEPTED 2a-C/2e-C); the fix is
a one-clause LOC-split alignment with the sibling dossiers, no fold change.

## Observations (below disposition threshold — noted, not dispositioned)

**O-1 (scalar-delegate LOC-count drift).** The two scalar-delegate bodies are
described as "2-line" (2e `:281`), "3-line" (2e/2a body prose), and "4-LOC" (2b
`:60,:84,:142`). Live: each `*_neon` fn is a passthrough whose delegate call is on
line 3. The close-state (`scalar-delegate-non-ASM`, no NEON consumer) is correct in
all three dossiers and no cost claim turns on the literal count. Below threshold;
a deft one-token harmonization to "line-3 delegate" at T-P3 synthesis would close
the cosmetic drift, but it is not a REVISE.

**O-2 (compound F8/F9 envelope is honestly joint).** F8 (600-1400 LOC) and F9
(960-site rename) both touch the StructLayout/selector surface; the dossiers price
the StructDirect→per-rule-selector fold as a *joint, non-additive* envelope
(`2d:280` "600-1400 LOC joint envelope (BSHAPE17-002 ⊕ 003, non-additive)"). This
is the correct cost framing — the two folds share the rename surface and must not
be summed. No double-counting; noted as a positive.

## CH4 cross-cutting findings (V2)

1. **The cross-dossier scalar-delegate convergence is achieved.** The single most
   load-bearing V1 CH4 finding — that 2a/2e priced table_64/prefix_xor as NEON-body
   rows while 2b correctly filed them scalar-delegate — is resolved. 2a `:247-258`,
   2e `:271-296`, 2b A4/LAC-03, and 2c (CH2-V1-R4 fold) now all state the eq-set fan
   as the one NEON body and the two delegates as `scalar-delegate-non-ASM`. The
   Lock-16 cost is now realistic across the cohort.

2. **The cost-selector activation framing holds at HEAD.** `perf_cost:0` persists
   (re-greped live); the 584-LOC engine is present-but-inert in skinny, absent in
   core. F8/2d price the fold as activation (600-1400 LOC joint), not the enum. No
   regression in this claim from V1.

3. **REQUIRED-NEW cost is now fully bound.** L5's idiom citation is reanchored to
   the live `lib.rs:175` home; L6 now carries an explicit abrogate threshold tying
   the REQUIRED-NEW LOC + checkasm to a measured W3 self-time reduction. Both
   REQUIRED-NEW primitives' scalar+checkasm are genuinely absent (verified), and
   both are gated `wired-if-W3-lands` with a deletion arm. No open-ended cost.

4. **No live orphan kernel admitted.** The `udot`/i8mm digit MAC (no CSS
   antecedent) is correctly refused by every dossier; FOLD-L9 (Alt-mode, no live
   consumer) is demoted to a deferred appendix and not counted among the wired
   folds. F7's pre-gate consumer is now the co-waved F1/F3 wiring (not orphan).

## Verdict

CH4 COST returns **97.5% ACCEPT** (39/40), **1 REVISE / 0 REJECT**, the single
non-ACCEPT (CH4-V2-001) carrying a file:line + concrete fix (zero orphan REVISE,
ORCHESTRATOR §3Z). All six V1 non-ACCEPTs are discharged with live-verified folds;
the cross-dossier scalar-delegate-vs-NEON-body split V1 demanded is achieved across
2a/2b/2c/2e. The one residual REVISE is a generator-vs-regen LOC-split alignment on
2f F2 against the already-ACCEPTED isomorphic 2a-C/2e-C — a cost-completeness gap,
not a fold defect. The cost surface is well-grounded: the S-P2 LOCKED L1–L9 manifest
schema enforces {scalar-ref · checkasm · same-wave consumer · LOC · abrogate}; no
REDRESS route re-opened; the aarch64-only discipline holds throughout; preserve-
rich-ast and the no-6th-BackendShape constraint are honoured. At the §4 ≥95% bar —
the cohort converges this cycle from the CH4 lens; one REVISE folds into V3.
