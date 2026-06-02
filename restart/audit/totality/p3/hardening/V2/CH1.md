---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis (SK-V18)
cycle: V2
target: restart/audit/totality/p3/{3A,3B,3C,3C-locks-v+1-diff,3D,3E,3F}
reviewer: CH1 CHALLENGE lens (adversarial)
generated_at: 2026-06-01
verdict_summary: "The V1 REJECT (non-applying v+1 diff) is FIXED — git apply --check exits 0. All 21 disposition candidates resolve at their finding-ids with exactly one disposition and zero silent drops; all load-bearing SPEC/code/LOCKS citations resolve; all 8 V1 findings were folded. THREE substantive REVISEs survive: (1) the 3C single-substrate SCOPE NOTE cites crates/simd-scan/src/lib.rs:68 for the NibbleLut/WideLut second-substrate evidence, but the actual export is :67 (line 68 carries neither symbol) — off-by-one in both 3C files in a load-bearing defer-justification; (2) 3F's §6 lock-body prose + HANDOFF carrier still carry SK-V15-style 'T-P2 normal §3Z / T-P3 CONVERGED' governance, contradicting both the SK-V18 T-P2 CONSOLIDATED file AND 3F's own corrected MH-004 row — V1's C5 fix scrubbed only MH-004 + frontmatter, not the body; (3) the 3F-MH-005 delta row asserts 'totality T-P1/T-P2/T-P3 CONVERGED', same SK-V15 governance defect on a distinct surface."
---

# CH1 CORRECTNESS — T-P3 SK-V18 Synthesis Packet (Cycle V2)

## Lens scope

CH1 verifies: (1) every proposed delta cites a real T-P1 finding-id or T-P2
grounding; (2) every cited V1-surface section resolves at path:line; (3) the 3C
disposition matrix references real candidates and disposes ALL 21 (8 T-P1 + 13
T-P2) with no silent drop; (4) the 3C-locks-v+1-diff applies cleanly to the
current LOCKS.md (16-lock count, no renumber). The most load-bearing deltas were
spot-verified on disk; the V1 cycle's 8 findings were independently re-checked
for fold.

## Executive verdict

The packet is well-grounded and the V1 hard REJECT is resolved. I extracted the
v+1 diff and ran `git apply --check` against the live `restart/locks/LOCKS.md`
at HEAD: **exit 0, clean apply** (header now `@@ -622,6 +622,33 @@` with the two
existing blank lines 623/624 in the leading context). The 21/21 disposition
coverage holds with zero silent drops; every candidate finding-id resolves; both
PLANNED co-gate symbols are 0; the 16-lock count and five-`BackendShape` canon are
preserved. All 8 V1 findings (A1 diff, B-tally, the two wrong paths, C1-struct,
C3-xref, C5, C6) were folded — 6 fully, C5 only partially.

THREE substantive REVISEs survive — one NEW path-precision miss (not in V1), two
from an INCOMPLETE V1-C5 fix (the corrected delta row left contradictory SK-V15
governance phrasing in the artefact body and in a sibling delta row). Cycle V1
expects ≥30% REVISE; on the substantive non-mechanical-pass set this is met
(3 REVISE of 8 cross-artefact/structural items = 37.5%).

---

## A. The v+1 diff (load-bearing — the LOCKS singularity)

### A1 — 3C-locks-v+1-diff applicability — **ACCEPT** (V1 REJECT FIXED)

Extracted the fenced diff block (header `@@ -622,6 +622,33 @@`, 37 lines) and ran
`git apply --check` against the live LOCKS.md: **exit 0**. The V1 defects are both
repaired:
- Defect 1 (hunk arithmetic): the V1 malformed `@@ -622,6 +622,38 @@` is now
  `@@ -622,6 +622,33 @@` (6 context + 27 added = 33 new-side; matches the body).
- Defect 2 (blank-line context): the leading context now carries BOTH existing
  blank lines (`LOCKS.md:623`/`:624`) before the `## v+1 Governance Boundary`
  heading at `:625`. The addendum lands between the second blank and the heading
  with no compounded blank. Verified on disk.

The Invariant Check's own `git apply --check` claim is therefore truthful.

### A2 — Diff Invariant-Check claims (16 locks / 5 shapes / PLANNED symbols) — **ACCEPT**

All Invariant-Check assertions verify on disk. The 16 numbered locks are list
items `N. **...**` (not `## Lock N` headings); the cited lines
`LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453` each
resolve to lock 1..16 respectively (confirmed line-by-line). Five `BackendShape`
variants at `lower/mod.rs:18-24` = `{EagerTape, OffsetTape, EventTape, SinkOnly,
CollapsedStage}` dispatched on `cost.chosen`. Insertion anchor correct (SK-V17
Lock-16 clause at `:622`, governance boundary heading at `:625`).
`rg runtime_target_rows_collapsed skinny/crates skinny/xtask` = 0 and
`rg bbnf_simd_single_mask_convention skinny/crates` = 0 — both PLANNED, honestly
disclosed; `runtime_target_rows_collapsed` resolves at SK-V18 SPEC `:247` as a
PLANNED "MUST be true at G3/P3" gate, never cited live. LOCKS greps for
`named-primitive` / `PROFILE-PROVEN-NARROW-LEAF` / `emit_shape_source` all = 0
(the discipline is genuinely absent, so the addendum is not a paper close).

---

## B. The 21 disposition-matrix candidates (3C — the LOCKS singularity)

All 21 candidate-ids resolve at their cited source anchors; EXACTLY one
disposition each; zero silent drops. 8 T-P1 = 7×`LAC-1E-V5-0[1-7]` (`1E:147-153`)
+ 1×`1A-LOCK1-AMEND-001` (`1A:180`); 13 T-P2 = 3×2C (`2C:380-382`) + 4×2D
(`2D:95-98`) + 3×2E (`2E:244-246`) + 3×2F (`2F:194-196`). I read each anchor and
confirmed the candidate text matches the matrix disposition. Spot-verified the
load-bearing live citations:

- `LAC-1E-V5-01` ACCEPT → named-primitive (a)-(d) gate: SK-V18 SPEC `:358-390`
  carries the four-conjunct gate verbatim ((a) grammar-invoked, (b) varies-under-
  mutation, (c) verbatim_blob_present==false, (d) PROFILE-PROVEN-NARROW-LEAF).
- `LAC-1E-V5-02` ACCEPT → relocated-seam firewall: `runtime_target_rows_collapsed`
  at SPEC `:247` is PLANNED ("MUST be true at G3/P3"); rg=0 disclosed.
- `LAC-1E-V5-05` ACCEPT → verbatim blob: `runtime_generator.rs:701` =
  `const CSS_GENERATED_RS: &str = r#"` confirmed.
- `LAC-1E-V5-06` ACCEPT → green-by-exclusion: `lock14_baseline.rs:2420` =
  `FORBIDDEN_GENERIC_TOKENS`, `:2442` = `SKV15_W2_EXTRA_COVERAGE_ROOTS`,
  `:2463` = `("crates/bbnf-simd/src/x86_64", "diagnostic-x86")` confirmed.
- `1A-LOCK1-AMEND-001` MODIFY → cursor-generality: `passes/src/backend_egraph.rs`
  (V1's wrong-crate path FIXED); SPEC `:1202-1207` confirms the `<G>` DELETE.
- `LAC-2D-V3-03` MODIFY → e-graph guard: `NormalizeDirectSinkCost` is a live
  non-`#[cfg(test)]` `Rewrite<DecisionNode, NoAnalysis>` at
  `passes/src/backend_egraph.rs:191-193`, instantiated `:75` — the CH4-V1
  activation→guard re-key is grounded.
- `LAC-2D-V3-04` ACCEPT → CollapsedStage slot: `lower/collapsed_stage.rs:16`
  (V1's dropped `lower/` segment FIXED; the wrong path
  `codegen/src/collapsed_stage.rs` is absent on disk).
- `LAC-2E-V6-03` MODIFY → movemask: `movemask.rs:5` = `vshrn_n_u16::<4>` confirmed.
- `LAC-2F-V3-03` DEFER → names its re-entry trigger (any 2F-class re-audit citing
  a "balanced-scan gap") and is folded as an audit-scope note, not dropped.

Per-candidate CH1: all 21 dispositions are sound on the disposition axis (20
ACCEPT outright; `LAC-2F-V3-01`/`LAC-2E-V6-03`'s dispositions are sound but their
shared SCOPE-NOTE citation is off-by-one — see B-citation, scored as one REVISE).

### B-tally — 3C headline ACCEPT/MODIFY count — **ACCEPT** (V1 REVISE FIXED)

`3C-locks-crystallisation.md:47` and the frontmatter now read "9 ACCEPT, 11
MODIFY, 0 REJECT, 1 DEFER". Re-counted the matrix row-by-row: ACCEPT =
{1E-V5-01, 1E-V5-02, 1E-V5-03, 1E-V5-05, 1E-V5-06, 2D-V3-01, 2D-V3-02, 2D-V3-04,
2F-V3-01} = 9; MODIFY = {1E-V5-04, 1E-V5-07, 1A-LOCK1-AMEND-001, 2C-SK18-01,
2C-SK18-02, 2C-SK18-03, 2D-V3-03, 2E-V6-01, 2E-V6-02, 2E-V6-03, 2F-V3-02} = 11;
DEFER = {2F-V3-03} = 1; total 21. The V1 transposition is corrected.

### B-citation — 3C single-substrate SCOPE NOTE cites simd-scan:68, actual :67 — **REVISE**

`3C-locks-crystallisation.md:88` AND `3C-locks-v+1-diff.md:73` (the
`D-SKV18-L16-single-substrate-movemask` clause SCOPE NOTE, CH5-DEFECT-V1-03)
assert: "the totality `crates/simd-scan` exports a second `NibbleLut`/`WideLut`
classifier convention (`crates/simd-scan/src/lib.rs:68`)". On disk the export is
at **line 67** (`pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet,
WideLut};`); **line 68** is `pub use index::{StructuralIndex,
next_structural_at_or_after};` — which carries NEITHER `NibbleLut` NOR `WideLut`.
This is a load-bearing citation: it is the on-disk evidence that justifies folding
the totality second-substrate into the SK-V19 scanner-unification single-priced
DEFER (the skinny `rg=0` green is explicitly NOT a totality single-substrate
proof). A reviewer following the cite lands on the wrong line and the
second-substrate claim does not resolve where pointed. Correction (BOTH artefacts):
re-key `crates/simd-scan/src/lib.rs:68` → `:67`. The underlying fact (the two
symbols ARE exported from `simd-scan`) is true at `:67`, so this is path-precision,
not a phantom.

---

## C. Cross-artefact spot-checks

### C1 — 3A SK-V18 deltas (D01-D14) substance — **ACCEPT**

All 14 `ARCH-3A-V4-SK18-D0x` deltas cite resolvable T-P1/T-P2 finding-ids; none
revives a refuted route; no sixth shape / new directive / BIR. Spot-checked the
load-bearing: `D01` strike target `ARCHITECTURE.md:1998` = "type parameter is the
generality vehicle" (the literal sentence the certified plan deletes), grounded on
`1A-SUB-025/026`, `1A-LOCK1-AMEND-001`, `D-1E-V5-03`, `COH18-008`. `D04`
relocated-seam firewall grounded on `D-1E-V5-10`/`LAC-1E-V5-02`/`LAC-2D-V3-02`.
`D14` (SK-V18-skinny vs SK-V19-totality scope) grounded on `COH18-001` +
`HANDOFF.md:17-19`.

### C1-struct — 3A delta-count reconcile — **ACCEPT** (V1 REVISE FIXED)

`3A:57` now carries an explicit DELTA-COUNT RECONCILE: `proposed_deltas_count: 26`
= 12 carried `ARCH-3A-V1-D0x` (SK-V15-historical) + 14 SK-V18-new
`ARCH-3A-V4-SK18-D0x`; the "Newly added: None" V3-summary row is annotated as
referring ONLY to the carried-V3 packet. The V1 contradiction is resolved. The
CH4-V1 D11 split (D11a SK-V18 P4 ≈+15 / D11b SK-V19 R16 ≈+217) is also applied so
the SK-V19 cost is not laundered into the +15.

### C2 — 3B scope-pivot deltas (10 SK-V18 + 4 carried) — **ACCEPT**

The load-bearing finding (SK-V18 = generalization-on-skinny; the `crates/core/`
tape-fold MASTER labels "SK-V18" is SK-V19) is grounded. The V1-FOLD CH4-V1 D04
net-LOC reconcile is sound: SPEC `:433-437` gives P1 −4500 + P2 −700 + P3 −5500
+ P4 +15 + P5 0 = ≈−10685; P3 −5500 = 6×910 replica bodies + ~−40 collapsed rows
+ 1 PartialEq derive, internally consistent with the −5460 body-only figure 3D/3F
cite. Net ≈−10800 is a reduction (no generated-size-budget overflow).

### C3 — 3D skinny-fold deltas (3D-D01-D12) substance — **ACCEPT**

All 12 cite resolvable 1D/2X finding-ids; proposal-only.

### C3-xref — 3D-D12 relocated-seam cross-ref — **ACCEPT** (V1 REVISE FIXED)

`3D-D12` (and the §"rejection→locks-strengthening" rows) now cross-ref the 3C
relocated-seam co-gate as `D-SKV18-L05-L10-unfork`, with an explicit note that
`D-SKV18-L06` is the verbatim-blob clause, not the seam. The V1 mis-key
(`3C D-L06/D-L14`) is corrected, and the CH4-V1 cost-scope split (skinny P3 +1-line
vs SK-V19 R16 ≈+217) is applied.

### C4 — 3E grammar-generalisation deltas (3E-D12-D18 + 11 carried) — **ACCEPT**

All 7 SK-V18 deltas cite grounded `SK-V18-2C-*` findings; I read `2C:213-219` and
confirmed each (`ONE-GENERATOR-GENERALISATION-THESIS`, `NAMED-PRIMITIVE-ABCD`,
`CSS-BALANCED-SCAN-FORCED-DEMOTION`, `SHEETS-PRECEDENCE-TOWER`, `5-SHAPE-
BACKENDSHAPE`, `9-GRAMMAR-FLEET-ONBOARDING` (partial: SK-V18-witnessed-3/SK-V19-
receiver-9), `RELOCATED-SEAM-FIREWALL`) is marked `grounded`/`partial` with full
disposition prose. Sheets tower verified at `google-sheets.bbnf:97` (`compare_op`),
`:137` (`paren_expr = "(" , expression ...`), `:163`
(`expression = comparison_expr`). V1-surface targets `ARCHITECTURE.md:1204/:1248/
:2065` resolve. Lock-14 hardening routed to 3C/Pass Omega, not self-edited.

### C5 — 3F SK-V18 governance state (prose body) — **REVISE** (V1 C5 fix INCOMPLETE)

V1 flagged 3F citing the SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md` for the SK-V18
governance state. The V1-FOLD repaired the `3F-MH-004` delta ROW and the
frontmatter (both now correctly say "T-P1/T-P2 near-converged NON-normal-§3Z;
T-P3 in-cycle hardening, NOT yet a final-convergence lock", citing the SK-V18
T-P1/T-P2 CONSOLIDATED files). But the SAME SK-V15-style governance phrasing
SURVIVES verbatim in the artefact BODY, contradicting both the corrected MH-004
row AND the on-disk consolidated files:

- `3F:111-112` (§6 "Next-Cycle Dispatch" prose): "Governance: T-P1 CLEAN-FINAL /
  G1-AUTO-PINNED (not normal §3Z); **T-P2 normal §3Z**; **T-P3 final-convergence
  lock under V≤5** (V3 clean + V5 all-ACCEPT after a V4 citation-only repair)."
- `3F:141-142` (the HANDOFF "Current Totality Override" carrier markdown):
  "Totality T-P1 CONVERGED (clean-final/g1-auto-pinned), **T-P2 CONVERGED (normal
  §3Z), T-P3 CONVERGED (final-convergence lock under V≤5)**."

The SK-V18 T-P2 CONSOLIDATED file (`HARDENING-T-P2-CONSOLIDATED.md:17-25`) states
plainly: "It did **not** reach a normal §3Z two-consecutive-clean lock:
`converged=false, consec=0, voids=0`." The SK-V18 T-P1 CONSOLIDATED file (`:44-52`)
records "V7 lone clean r=1.000, V8 broke the streak, consec=0" — near-converged,
not a clean-final two-clean lock; and T-P3 is the CURRENT in-cycle hardening (these
very V2 verdicts), not a converged lock. So "T-P2 normal §3Z", "T-P3 final-
convergence lock / CONVERGED" are the SK-V15 facts V1's C5 told the author to
remove — they were removed from the delta row but left in the prose. This is now an
INTERNAL CONTRADICTION inside 3F and a path:line correctness fault (the body state
contradicts the cited consolidated files). Correction: re-ground 3F:111-112 and
3F:141-142 onto the SK-V18 record already correct in MH-004 — "T-P1 near-converged
NON-normal-§3Z; T-P2 near-converged NON-normal-§3Z; T-P3 in-cycle hardening (not
yet a final-convergence lock)". The other 11 3F deltas resolve (`HANDOFF.md:17-19`,
`runtime_generator.rs:701`, `tape/mod.rs:175`, `LOCKS.md:349`, `LOCKS.md:620`,
SPEC `:433-437`) — ACCEPT.

### C5-row — 3F-MH-005 asserts "T-P1/T-P2/T-P3 CONVERGED" — **REVISE**

The `3F-MH-005` delta row (`3F:87`) — the delta that REPLACES the top-level
HANDOFF current-state block — itself carries "with skinny S-P0..S-P3 CERTIFIED and
totality **T-P1/T-P2/T-P3 CONVERGED**". This is the same SK-V15 governance over-
claim as C5 on a DISTINCT, separately-propagating surface (the proposed HANDOFF
replacement text, not the §6 dispatch prose). Because Pass Omega CRUD would carry
this delta verbatim into `restart/HANDOFF.md`, the over-claim would land in a V1
governance surface. Correction: change the MH-005 delta text's "T-P1/T-P2/T-P3
CONVERGED" to the MH-004 record (T-P1/T-P2 near-converged NON-normal-§3Z; T-P3
in-cycle hardening), so the delta the author proposes to WRITE matches the delta
MH-004 proposes to RECORD.

### C6 — cohort cycle-label split — **ACCEPT** (V1 REVISE FIXED)

All six artefacts now carry `cycle: V4-SKV18-totality`. 3F separates the distinct
`pass_omega_index: V6` into the frontmatter with an explicit `cycle_label_note`
referencing CH1-V1-C6 ("cohort cycle label unified to V4-SKV18-totality"). The V1
V4/V6 split is reconciled.

---

## D. Anti-paper-close credit (what CH1 confirms correct)

- v+1 diff APPLIES (`git apply --check` exit 0); the V1 hard REJECT is fixed.
- 21/21 dispositions present, EXACTLY one each, ZERO silent drops; the DEFER names
  its re-entry trigger and is folded as an audit-scope note.
- Every candidate finding-id resolves at its cited T-P1/T-P2 anchor; the candidate
  text matches the matrix disposition.
- Both PLANNED co-gate symbols honestly written as PLANNED (rg=0); never cited live.
- 16-lock count + five-`BackendShape` canon preserved and verified on disk; no
  renumber, no sixth shape, no new directive/BIR/substrate proposed.
- 6 of 8 V1 findings (A1, B-tally, two wrong paths, C1-struct, C3-xref, C6) were
  folded fully; C5 only partially (the delta row + frontmatter fixed, the body and
  the MH-005 row not).
- Live-code anchors verified: `NormalizeDirectSinkCost` live non-test Rewrite,
  `CSS_GENERATED_RS` verbatim blob, `lower/collapsed_stage.rs`, `movemask.rs:5`,
  `find_css_significant` at `runtime_simd.rs:169`, Pattern-H count = 71,
  `css_types.rs:1` at totality `crates/core`.

## E. Required corrections (by artefact)

1. `3C-locks-crystallisation.md:88` + `3C-locks-v+1-diff.md:73` — in the
   `D-SKV18-L16-single-substrate-movemask` SCOPE NOTE, re-key
   `crates/simd-scan/src/lib.rs:68` → `:67` (the `NibbleLut`/`WideLut` export).
2. `3F:111-112` (§6 prose) + `3F:141-142` (HANDOFF carrier) — re-ground the
   governance state off the SK-V15 "T-P2 normal §3Z / T-P3 final-convergence lock
   / T-P1-T-P2-T-P3 CONVERGED" phrasing onto the SK-V18 record already correct in
   `3F-MH-004` (T-P1/T-P2 near-converged NON-normal-§3Z; T-P3 in-cycle hardening).
3. `3F-MH-005` delta row (`3F:87`) — change "totality T-P1/T-P2/T-P3 CONVERGED" to
   the MH-004 record, so the proposed HANDOFF replacement text does not carry the
   over-claim into a V1 surface.

## F. Enumeration tally (CH1 lens)

Items judged: the v+1 diff applicability + its invariant check (A1, A2); the 21
3C candidate dispositions (B); the 3C headline tally (B-tally); the simd-scan
SCOPE-NOTE citation (B-citation); the cross-artefact spot-checks (C1, C1-struct,
C2, C3, C3-xref, C4, C5, C5-row, C6). Total = 2 + 21 + 1 + 1 + 10 = 35 items.

- ACCEPT (32): A1, A2; 21 candidate dispositions in B; B-tally; C1, C1-struct, C2,
  C3, C3-xref, C4, C6.
- REVISE (3): B-citation (3C simd-scan:68 → :67, both 3C files); C5 (3F §6 prose +
  HANDOFF carrier carry SK-V15 governance contradicting the corrected MH-004 + the
  SK-V18 CONSOLIDATED files); C5-row (3F-MH-005 delta carries "T-P1/T-P2/T-P3
  CONVERGED" — the same over-claim on the proposed-HANDOFF surface).
- REJECT (0): the V1 REJECT (non-applying diff) is fixed; no uncited delta, revived
  refuted route, silent-dropped candidate, or cross-scope violation found.

The substantive non-mechanical set (A1 + B-citation + C1-struct + C2 + C3-xref +
C5 + C5-row + C6 + B-tally = 9 items, of which A1/B-tally/C1-struct/C3-xref/C6 are
mechanical-pass-confirmations) carries 3 REVISE = 37.5% on the eight cross-artefact
findings, clearing the V1 ≥30% expectation. The packet has no REJECT this cycle; it
is correct pending the two path-precision/governance-contradiction REVISEs.

TALLY accept=32 revise=3 reject=0
