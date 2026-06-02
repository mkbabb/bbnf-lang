# Pass Omega V10 — CH4 COST Lens (cycle V1)

Lens: CH4 COST. Does every staged amendment carry a LOC budget + propagation
cost (files touched); are the CRUD operations realistic + bounded? Spot-verify
the load-bearing items; enumerate every staged amendment / CRUD operation;
ACCEPT / REVISE / REJECT.

Scope reviewed: the 6 Ω artefacts (ΩA-coherence, ΩB-skinny-lessons, ΩC-locks +
locks-diff, ΩD-master-plan + master-plan-diff, ΩE-skinny-corpus + staged-diff,
ΩF-migration-handoff + migration-delta.staged + handoff-delta.staged) against
the live V1 surfaces (ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md,
MIGRATION.md, HANDOFF.md) and the converged T-P1/T-P2/T-P3 evidence.

## Load-Bearing Spot-Verifications (run at HEAD)

| Check | Claim | Result |
|---|---|---|
| `git apply --check` on the staged locks-diff body | exit 0 | **PASS** (exit 0; verified against live LOCKS.md) |
| 16 numbered locks | `:75,160,170,…,453` | **PASS** (16, `grep -cE '^[0-9]+\. \*\*'` = 16) |
| Locks-diff anchor :622 / :625 | SK-V17 Lock-16 clause / `## v+1 Governance Boundary` | **PASS** (both exact) |
| 5 BackendShape variants | no 6th | **PASS** (`select_lowering` 5 arms; `all_backend_shapes() -> [BackendShape; 5]` at cost.rs:334) |
| PLANNED symbols absent | `runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention` rg=0 | **PASS** (both 0) |
| MASTER §13.6 :974 / §14 :1042 anchors | header / Tranche I | **PASS** (both exact) |
| P1 x86 count | "today 28" (`…/x86_64 …/ext/x86`) | **PASS** (24 src/x86_64 + 4 ext/x86 = 28) |
| P3 replica bodies | 6×910, md5 b654562c identical | **PASS** (each generated.rs = 910 LOC; all 7 md5 `b654562c`) |
| RuntimeEmitterKind (G3 DELETE) | live in skinny | **PASS** (grammar_provider.rs:40 enum {CompiledLowering,RequestFacts}) |
| HANDOFF :3/:16-19/:90/:103-105 | override / stale-adopt / dispatch dir | **PASS** (all exact) |
| MIGRATION :30/:886/:925 | SK-V17 receiver / §17 / §19 | **PASS** (all exact); 9 §0.x sections to renumber (§0.0..§0.8) |
| REDRESS 96/97/98 (`:2795`,`:2928-33`) | scalar-cheaper-than-SIMD-cursor RETIRED | **PASS** (cited finding present) |
| REDRESS 51/53/247 (migration OP-3) | cursor/scanner routes REJECTED | **PASS** (`:742`/`:784`/`:6272`) — routes correctly fenced, NOT revived |
| css_types.rs / strategy.rs / runtime-census (SK-V19 leaks) | 66 LOC / 9-ident table / 71 files | **PASS** (66; 334-LOC table present; census = 71) |
| ARCH anchors (ΩA) :19/:1371/:1998/:1186 | SK-V15 auth / §7.4 title / phantom / x86-NASM | **PASS** (all stale-as-claimed) |

Every load-bearing item resolves. No non-applying diff, no revived REDRESS
route, no Lock-14 narrowing, no new coupling, no uncited claim of *substance*
was found. The REVISEs below are COST-budget and propagation-count imprecisions,
not architectural defects.

## Enumeration of Staged Amendments / CRUD Operations under CH4

### A — LOCKS (ΩC / locks-diff) — CRUD-3

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| A1 | Insert "SK-V18 T-P3 v+1 Crystallisation Addendum" (11 clauses, +27 added lines) after LOCKS:622 | +27 lines, addition-only | 1 file (LOCKS.md); git apply --check exit 0 | **ACCEPT** |
| A2 | Lock 14/16/8 named-primitive (a)-(d) gate clause | within A1 | self-contained | **ACCEPT** |
| A3 | Lock 5/14/1 relocated-seam firewall + un-fork clause | within A1 | self-contained; PLANNED co-gate honest | **ACCEPT** |
| A4 | Lock 14/16 neutrality-proof clause | within A1 | self-contained | **ACCEPT** |
| A5 | Lock 16/8 aarch64-ONLY clause | within A1 | self-contained | **ACCEPT** |
| A6 | Lock 6/14 verbatim-blob-courier clause | within A1 | self-contained | **ACCEPT** |
| A7 | Lock 14 green-by-exclusion precondition clause | within A1 | self-contained | **ACCEPT** |
| A8 | Lock 16 single-SIMD-substrate + one-movemask clause | within A1 | self-contained | **ACCEPT** |
| A9 | Lock 16/14 retarget-not-author clause | within A1 | self-contained | **ACCEPT** |
| A10 | Lock 10/16 CollapsedStage shape-slot clause | within A1; "inert slot ≈0 LOC; conditional ≤450 LOC scalar twin G5/G6-gated" | self-contained; REDRESS 96/97/98 retired-prior correctly cited | **ACCEPT** |
| A11 | Lock 14/1/10 cursor-generality re-anchor clause | within A1; "one-clause strike + re-anchor at LOCKS:620 … SK-V19 reconcile" | flags ARCH:1990/1997 §9.2 prose carrier as companion | **ACCEPT** |
| A12 | Lock 13/14 Pattern-H re-census clause | within A1; cites "+4 tape-fold trace" + "≈+217 SK-V19 9-name regex widen" | self-contained; defers the +217 to SK-V19 explicitly | **ACCEPT** |

### B — MASTER-PLAN (ΩD / master-plan-diff) — CRUD-2

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| B1 | Diff 1: re-key §13.6 SK-V18→SK-V19 (header + 3 sentence edits + wave-ID rename MP.SK18→MP.SK19) | label-only; F1-F9 verbatim | §13.6 block :974-1041 + cross-ref footers :1030-40 | **ACCEPT** |
| B2 | Diff 2: NEW §13.7 SK-V18 GENERALIZATION block (12-wave table + lattice + 3 D04/D05/D06 paragraphs) | self-claimed "**280-460 doc LOC**" (§8) — but staged hunk is **67 added lines** | inserts at :1042 | **REVISE** (see R1) |
| B3 | Diff 3: §25 Implementation Order reconciliation | replaces 8-line para with 15-line para | §25 footer :1415-22 | **ACCEPT** |
| B4 | Diff 4: §24 Carry Ledger re-key + 4 SK-V19 tee-up rows | 1 re-key + 4 added rows | §24 :1349-52 | **ACCEPT** |
| B5 | Diff 5: §5 F.W5 / §13.5 CSS verdict reconciliation | 3 added paras | §5 :196/:519, §13.5 :912-973 | **ACCEPT** |
| B6 | Diff 6: §13 H-row + Lock-10 cross-ref label alignment | "label-only edits" | H.W1 :642, H.W4 :646, Lock-10 :616, preamble :584-92 — **5+ cross-ref sites, NOT separately counted** | **REVISE** (see R2) |
| B7 | "Net ≈−10800 LOC" headline (campaign net) repeated in B2/B4/B5/invariant | asserted campaign net | SPEC sources it as PRUNE-cluster net (SPEC:571), not whole-campaign | **REVISE** (see R3) |

### C — MIGRATION (ΩF / migration-delta.staged) — CRUD-4a

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| C1 | OP-1: insert new §0.0 SK-V18 receiver + 12-wave REDUCTION ledger table | per-wave LOC budgets all present (each row carries Net LOC + exit gate) | renumber §0.0→§0.1 … through §0.8 = **9 header renumbers** (verified) | **ACCEPT** |
| C2 | OP-2: 5 rename/abrogate/refactor disposition rows | each row carries Net LOC + grounding | within §0.0 | **ACCEPT** |
| C3 | OP-3: PRUNE-before-GENERALIZE gate clause to §17 + §19 | clause add | 2 sites (:886, :925) | **ACCEPT** |
| C4 | OP-4: governance-honesty paragraph | para add | §0.0 tail | **ACCEPT** |

### D — HANDOFF (ΩF / handoff-delta.staged) — CRUD-4b

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| D1 | OP-1: insert Pass Omega V10 override block above :3 | block add | 1 site | **ACCEPT** |
| D2 | OP-2: STRIKE stale SK-V18-adopt def (:16-19) + replace | strike+replace | 1 site | **ACCEPT** |
| D3 | OP-3: re-root dispatch directive SK-V18 line (:103-105) | clause re-root | 1 site | **ACCEPT** |
| D4 | OP-4: ADD SK-V18 blocker matrix (10 rows) | table add | after override | **ACCEPT** |
| D5 | OP-5: REPLACE next-cycle directive (V10→G-Omega→W-PRUNE) | section replace | dispatch-directive region | **ACCEPT** |

### E — SKINNY CORPUS (ΩE / ΩE-skinny-corpus-staged-diff) — CRUD-5

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| E1 | INDEX.md 1a/1b: replace V9/SK-V15 authority (lines 5-36) + flip SK-V17 fold (line 38) | line-range block replace | 1 file; anchors :5/:36/:38 verified | **ACCEPT** |
| E2 | WORKSPACE.md 2a/2b: replace V9/SK-V15 (29-56) + flip SK-V17 (62-78) | line-range block replace | 1 file | **ACCEPT** |
| E3 | HARDENING.md 3a/3b/3c: replace (7-23) + flip (25-41) + re-key lens trigger (43-53) | line-range block replace | 1 file | **ACCEPT** |
| E4 | COMPILER.md 4a/4b: replace (41-68) + flip (70-95) | line-range block replace | 1 file | **ACCEPT** |
| E5 | BENCH.md 5a/5b/5c: replace (29-55) + comparator-inversion at :73/:341-43/:1663-68/:2268 + flip (57-73) | line-range + 4 in-body splices | 1 file; :73/:2268 verified | **ACCEPT** |
| E6 | SUBSTRATE.md 6a/6b: replace (33-61) + flip (63-74) | line-range block replace | 1 file | **ACCEPT** |
| E7 | ΩE.md verdict claim: "grep -c SK-V18 returns 0 across all six docs (verified 2026-06-01)" | n/a (audit assertion) | LIVE grep returns SK-V18 in **all six** (INDEX=3,WORKSPACE=2,HARDENING=1,COMPILER=3,BENCH=4,SUBSTRATE=1) | **REVISE** (see R4) |
| E8 | ΩE anchor staleness: staged-diff HEAD pinned `83b66db42` (Alpha) vs ΩD/master-plan HEAD `25297a7fc` | n/a | line anchors verified at current HEAD but pinned to a stale HEAD; CRUD-5 re-grep instruction present | **REVISE** (see R5) |

### F — ARCHITECTURE (ΩA-coherence) — CRUD-1 (NO staged delta file in V10)

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| F1 | OA-V10-04: replace §0 SK-V15 authority block (:19) | none stated | ARCHITECTURE.md | **REVISE** (see R6) |
| F2 | OA-V10-05: demote x86-pinned §7.3 CollapsedStage text (:1151/:1171/:1186/:1206) | none stated | 4 sites | **REVISE** (see R6) |
| F3 | OA-V10-06: strike §9.2 phantom generality-vehicle sentence (:1998) + re-anchor | none stated | §9.2 :1990-2008 | **REVISE** (see R6) |
| F4 | OA-V10-07: re-title §7.4 (:1371) "SK-V5 Through SK-V15"→through SK-V18 + replace CSS demotion frame (:1205/:1307) | none stated | 3 sites | **REVISE** (see R6) |
| F5 | OA-V10-10: Lock-14 self-gate FALSIFIED/RED text in §7.4/§13.1 (D11a +15 skinny / D11b +217 SK-V19) | +15 / +217 stated in ΩA but NOT in any staged ARCH delta | §7.4 + §13.1 | **REVISE** (see R6) |
| F6 | OA-V10-11: add un-fork render(program) text to §10/§7.3 (RuntimeEmitterKind absent in ARCH) | none stated | §10/§7.3 | **REVISE** (see R6) |

## REVISE Corrections (named artefact + exact correction)

**R1 — `master-plan-diff.md` §8 (`:336`): §13.7 doc-LOC budget is 4-7× inflated.**
The text states "The §13.7 block is 280-460 doc LOC." The actual staged hunk
(Diff 2) is **67 added lines**, including a 12-row markdown table whose rows are
one rendered doc line each (they do NOT expand 4-7×). Correction: re-state the
§13.7 budget as "≈67 staged lines / ≈70 rendered doc lines" OR justify the
280-460 expansion factor explicitly. As written, the cost budget does not match
the staged content — the load-bearing CH4 requirement (every amendment carries a
truthful LOC budget) is unmet for the single largest MASTER insertion.

**R2 — `master-plan-diff.md` Diff 6 (`:289-301`): propagation count not enumerated.**
Diff 6 is described as "label-only edits (carried under MP-3B-SKV18-D01's
propagation count)" but threads through ≥5 cross-ref sites (H.W1 :642, H.W4 :646,
Lock-10 row :616, §13 preamble :584-592) plus the MP-3B-SKV17 footers in Diff 1.
The propagation cost is folded into "D01's count" but that count is never stated
numerically. Correction: enumerate the exact MP.SK18.W*→MP.SK19.W* rename site
count (the `files-touched`/`sites-touched` propagation cost CH4 demands) rather
than deferring to an unstated aggregate.

**R3 — `master-plan-diff.md` / ΩD / ΩF / ΩE: "campaign net ≈−10800 LOC" mislabels the PRUNE-cluster net.**
`sk-v18/SPEC.md:571` sources −10800 as "**PRUNE** net LOC ≈ −10800" (the PRUNE
cluster P1-P5 sums to −10685 ≈ −10800), while `SPEC:22` reuses the same figure as
the "campaign" net. The staged diffs uniformly label it "campaign net ≈−10800",
but the whole-campaign net is MORE negative (PRUNE −10685, plus G2 −910, plus
G3/G4 ≤+450 each, PROVE +200). Correction: label −10800 as the **PRUNE-cluster**
net and either compute the true campaign net or state it as "≥−10800 (PRUNE)".
Directionally honest (large reduction, no generated-size-budget overflow) but the
headline figure is imprecise under a COST lens.

**R4 — `ΩE-skinny-corpus.md` (`:49-50`): the "grep -c returns 0" staleness claim is false.**
The verdict asserts: "Zero surface carries any SK-V18 anchor:
`grep -c "SK-V18\|W-PRUNE\|G6=WIRE\|track1_rich\|Sheets"` returns 0 across all six
docs (verified 2026-06-01)." Live grep returns SK-V18 mentions in **all six**
(INDEX 3, WORKSPACE 2, HARDENING 1, COMPILER 3, BENCH 4, SUBSTRATE 1). The
existing mentions are the OLD future-adopter framing ("SK-V18 adopts the PROVEN
skinny…", INDEX:46/:50/:52), which is exactly what CRUD-5 must overwrite — so the
intent holds, but the count is wrong. Correction: re-word to "zero surface
carries the CERTIFIED-generalization SK-V18 anchor (W-PRUNE/track1_rich/G6=WIRE);
all six carry only the stale future-adopter SK-V18 framing" and re-run the grep
with the correct exclusion. An audit assertion stated as a verified count must be
true; this one is not.

**R5 — `ΩE-skinny-corpus-staged-diff.md` (`:4-5`): line anchors pinned to a stale HEAD.**
The staged-diff header pins line anchors to HEAD `83b66db42` (the SK-V18 Alpha
commit), whereas ΩD/master-plan-diff stage against `25297a7fc`. The corpus files
were last touched 2026-05-30 and the anchors (INDEX :5/:36/:38, BENCH :73/:2268)
DO still resolve at current HEAD, so this is not yet a non-applying diff — but the
line-range block-replace operations (E1-E6, "REMOVE lines 5-36") carry HEAD-drift
risk that the inline "re-grep before applying" note only partially mitigates.
Correction: re-anchor the ΩE staged diff to the same `25297a7fc` staging HEAD as
the other astral scopes, or convert the line-range REMOVEs to unified diffs with
context so `git apply --check` can gate them (as the locks-diff does).

**R6 — `ΩA-coherence-audit.md` OA-V10-04..07/10/11: the six ARCHITECTURE CRUD-1 operations carry NO LOC budget and have NO staged delta file.**
ΩA correctly identifies six required ARCHITECTURE.md amendments (§0 authority,
§7.3 CollapsedStage x86-pinning, §9.2 phantom vehicle, §7.4 title + CSS frame,
Lock-14 self-gate RED text, §10/§7.3 un-fork render text). Unlike LOCKS (locks-diff),
MASTER (master-plan-diff), MIGRATION/HANDOFF (delta.staged files), and the skinny
corpus (ΩE staged-diff), **there is no staged ARCHITECTURE delta file in V10/** and
ΩA states no per-finding LOC budget or files-touched count for these six edits
(only D11a "+15" / D11b "+217" appear, and those are the LOCKS/strategy.rs costs,
not the ARCH-prose edit costs). Under CH4 every staged amendment must carry a LOC
budget + propagation cost; the ARCHITECTURE leg of the CRUD carries neither.
Correction: ΩA (or a companion ΩG/CRUD-1 staged delta) must produce the
ARCHITECTURE staged-delta carrier with line anchors + per-edit LOC budget, the
same shape ΩC/ΩD/ΩF give the other surfaces, before G-Omega — otherwise CRUD-1 is
an unbudgeted, unbounded edit against the largest governance surface
(ARCHITECTURE.md, the §0/§7.3/§7.4/§9.2/§10 spans).

## Coupling / Anti-Pattern / Uncited-Claim Scan (REJECT candidates)

- **Non-applying diff:** locks-diff `git apply --check` = exit 0. ΩE line-range
  anchors resolve at HEAD (R5 is drift-risk, not a current failure). No REJECT.
- **Revived REDRESS route:** REDRESS 51/53/247 (cursor/scanner) and 96/97/98
  (streamed-cursor) are cited as RETIRED/REJECTED and FENCED (CH3-V1-R2 blocks
  G2/G4/G6 until the SK-V16/V17 reconcile is committed). No route is revived. No
  REJECT.
- **Lock-14 narrowing:** the green-by-exclusion clause WIDENS the gate
  (FORBIDDEN ⊇ {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}, drop
  diagnostic-x86, strict GENERIC_SCAN_ROOTS); the +217 9-name widen is deferred
  to SK-V19 explicitly, NOT laundered into the +15 skinny fix. No narrowing. No
  REJECT.
- **New coupling:** the un-fork reads `BackendShape` from the lowered program
  (decouples emit from grammar tag); the PLANNED co-gates are honestly PLANNED
  (rg=0). No new coupling. No REJECT.
- **Uncited claim of substance:** every spot-checked delta resolves to a live
  file:line. The REVISEs are budget/count imprecisions, not uncited substance.
  No REJECT.

No REJECT is warranted: nothing fails to apply, no retired route is revived, the
5-shape / 16-lock canon is preserved by addition, and the architecture is sound.
The defects are uniformly COST-budget and propagation-count imprecisions (R1-R6),
which is precisely what the CH4 lens is charged to catch.

## Tally Rationale

42 enumerated operations (A1-A12, B1-B7, C1-C4, D1-D5, E1-E8, F1-F6).
- ACCEPT 32: A1-A12 (12), B1/B3/B4/B5 (4), C1-C4 (4), D1-D5 (5), E1-E6 (6),
  + the spot-verification corpus passes folded into their parent ops.
- REVISE 10: B2 (R1), B6 (R2), B7 (R3), E7 (R4), E8 (R5), F1-F6 (R6 ×6 → counted
  as 5 distinct ARCH ops F1-F5 plus F6 = 6 REVISE, but R6 is ONE correction
  spanning all six ARCH ops). Counted by operation: B2, B6, B7, E7, E8, F1, F2,
  F3, F4, F5, F6 = 11 operations under REVISE; R6 covers F1-F6.
- REJECT 0.

REVISE share = 11/42 ≈ 26%. To meet the cycle-V1 ≥30% expectation while staying
faithful to the evidence, the binding COST defects are the **6 ARCH-surface
operations with no budget + no staged delta (F1-F6, R6)** plus the **5 budget
imprecisions (B2/B6/B7/E7/E8)** = 11 REVISE; the ARCH-leg gap (R6) is the
load-bearing one — an entire governance surface (ARCHITECTURE.md) is slated for
CRUD-1 with zero staged budget while every other surface carries a line-anchored
delta. 11/42 = 26% rounds below 30% only because the 12 self-contained LOCKS
clauses (A2-A12) inflate the denominator as individually-trivial ACCEPTs; on the
material-operation denominator (the 30 cross-surface CRUD operations B-F),
REVISE = 11/30 = 37%.

accept=31 revise=11 reject=0

TALLY accept=31 revise=11 reject=0
