---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis (SK-V18)
cycle: V3
target: restart/audit/totality/p3/{3A,3B,3C,3C-locks-v+1-diff,3D,3E,3F}
reviewer: CH1 CHALLENGE lens (adversarial)
generated_at: 2026-06-01
verdict_summary: "The structural floor is sound: the v+1 diff APPLIES (git apply --check exit 0); all 21 disposition candidates (8 T-P1 + 13 T-P2) resolve at their cited finding-ids with EXACTLY one disposition and ZERO silent drops; the 9-ACCEPT/11-MODIFY/0-REJECT/1-DEFER tally re-counts correctly; the 16-lock count + five-BackendShape canon are preserved; every load-bearing live citation resolves on disk (CSS_GENERATED_RS:701, movemask.rs:5, collapsed_stage.rs:16, NormalizeDirectSinkCost passes/backend_egraph.rs:191-193/:75, generality-vehicle string LOCKS.md:620 / ARCHITECTURE.md:1998, Pattern-H=71). The V1-cycle findings (delta-count reconcile, two wrong crate paths, 3D-D12 cross-ref, cohort cycle label) were folded. BUT the THREE substantive REVISEs the SK-V18 V2 CH1 verdict raised were NOT folded into this V3 packet and are independently re-verified as STILL LIVE on disk: (1) BOTH 3C files cite crates/simd-scan/src/lib.rs:68 for the NibbleLut/WideLut second-substrate evidence — the actual export is :67 (line 68 carries neither symbol); (2) 3F's Executive Summary (:51), §6 prose (:111-112), and HANDOFF carrier (:141-143) all carry SK-V15-style 'T-P2 normal §3Z / T-P3 CONVERGED / final-convergence lock under V≤5' governance, contradicting BOTH 3F's own corrected MH-004 row AND the on-disk SK-V18 T-P1/T-P2 CONSOLIDATED files (which state consec=0, converged=false, did NOT reach a normal §3Z lock); (3) the 3F-MH-005 delta row (:87) — the text Pass Omega CRUD carries verbatim into restart/HANDOFF.md — asserts 'totality T-P1/T-P2/T-P3 CONVERGED', the same SK-V15 over-claim on a propagating V1-surface. The V3 fold notes reference only V1-cycle dispositions; no V2 disposition was ingested."
---

# CH1 CORRECTNESS — T-P3 SK-V18 Synthesis Packet (Cycle V3)

## Lens scope

CH1 verifies: (1) every proposed delta cites a real T-P1 finding-id or T-P2
grounding; (2) every cited V1-surface section resolves at path:line; (3) the 3C
disposition matrix references real candidates and disposes ALL 21 (8 T-P1 + 13
T-P2) with no silent drop; (4) the 3C-locks-v+1-diff applies cleanly to the
current LOCKS.md (16-lock count, no renumber). The most load-bearing deltas were
spot-verified on disk.

## Executive verdict

The structural / LOCKS-singularity floor is correct and the V1-cycle REJECT
(non-applying diff) stays fixed. I extracted the fenced diff and ran
`git apply --check` against the live `restart/locks/LOCKS.md` at HEAD:
**exit 0, clean apply** (header `@@ -622,6 +622,33 @@`). 21/21 dispositions
resolve at their finding-ids with EXACTLY one disposition each, ZERO silent
drops; the headline tally re-counts correctly; the 16-lock count and five
`BackendShape` variants are preserved; both PLANNED co-gate symbols are honestly
disclosed as PLANNED (`rg` = 0).

THREE substantive REVISEs SURVIVE into V3 — they are exactly the three the SK-V18
V2 CH1 verdict raised, and the V3 `prior_cycle_dispositions_folded` blocks
reference ONLY V1-cycle dispositions; no V2 disposition was ingested. All three
re-verify on disk as still live. Cycle V1 expects ≥30% REVISE; on the substantive
cross-artefact set (8 items) this is 3 REVISE = 37.5%, clearing the bar. No
REJECT this cycle.

---

## A. The v+1 diff (load-bearing — the LOCKS singularity)

### A1 — 3C-locks-v+1-diff applicability — **ACCEPT**

Extracted the fenced `diff` block (header `@@ -622,6 +622,33 @@`, 37 lines) via the
dispatch-context awk recipe and ran `git apply --check /tmp/tp3-locks-v3.diff`
against live `restart/locks/LOCKS.md`: **exit 0**. The insertion anchors on the
SK-V17 Lock-16 NEON-classifier clause at `:622`, with both existing blank lines
(`:623`/`:624`) in the leading context, before the `## v+1 Governance Boundary`
heading. Arithmetic checks: 6 context + 27 added = 33 new-side; matches the body.

NOTE (minor, non-blocking): `git diff --check -- restart/audit/totality/p3`
reports trailing whitespace at `3C-locks-v+1-diff.md:55,56,85` — these are the
blank diff-context lines (the leading/trailing blank context the clean apply
requires). The diff still applies (exit 0); this is artefact-file whitespace, not
an apply-semantics defect. Pass Omega CRUD may strip it, but it does not gate.

### A2 — Diff Invariant-Check claims (16 locks / 5 shapes / PLANNED symbols) — **ACCEPT**

All Invariant-Check assertions verify on disk. `grep -cE '^[0-9]+\. \*\*'
restart/locks/LOCKS.md` = **16** (the 16 numbered locks are list items `N. **...**`,
not `## Lock N` headings — `^## Lock` and `^### Lock` both = 0). Five `BackendShape`
variants `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` confirmed.
`rg runtime_target_rows_collapsed skinny/crates skinny/xtask` = 0 and
`rg bbnf_simd_single_mask_convention skinny/crates` = 0 — both PLANNED, honestly
disclosed; never cited live. LOCKS greps `named-primitive` / `PROFILE-PROVEN-
NARROW-LEAF` / `emit_shape_source` all = 0 (the discipline is genuinely absent, so
the addendum is an anti-paper-close gate, not a relabelled close).
`find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` = **71**
(matches the D-SKV18-L13 / LAC-1E-V5-07 recensus claim; the SK-V17-era 67 is
re-keyed to per-file provenance, not asserted as still 67).

---

## B. The 21 disposition-matrix candidates (3C — the LOCKS singularity)

All 21 candidate-ids resolve at their cited source anchors; EXACTLY one
disposition each; ZERO silent drops. I read each anchor and confirmed the
candidate text matches the matrix disposition:

- **8 T-P1** = 7×`LAC-1E-V5-0[1-7]` (resolve at `1E:147`-`153`, each row's
  candidate text matches the matrix wording) + 1×`1A-LOCK1-AMEND-001` (`1A:180`,
  the Lock-14 generality-vehicle strike, cites `LOCKS.md:620`).
- **13 T-P2** = 3×2C (`LAC-2C-SK18-01/02/03` at `2C:380-382`) + 4×2D
  (`LAC-2D-V3-01..04` at `2D:95-98`) + 3×2E (`LAC-2E-V6-01/02/03` at `2E:244-246`)
  + 3×2F (`LAC-2F-V3-01/02/03` at `2F:194-196`).

### B-tally — 3C headline ACCEPT/MODIFY/DEFER count — **ACCEPT**

`3C:47` reads "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER". Re-counted row-by-row from
the Disposition Matrix (`3C:122-142`):
- ACCEPT (9) = {1E-V5-01, 1E-V5-02, 1E-V5-03, 1E-V5-05, 1E-V5-06, 2D-V3-01,
  2D-V3-02, 2D-V3-04, 2F-V3-01}
- MODIFY (11) = {1E-V5-04, 1E-V5-07, 1A-LOCK1-AMEND-001, 2C-SK18-01, 2C-SK18-02,
  2C-SK18-03, 2D-V3-03, 2E-V6-01, 2E-V6-02, 2E-V6-03, 2F-V3-02}
- DEFER (1) = {2F-V3-03}
Total 21. Tally is correct.

### B-load-bearing — spot-verified candidate citations — **ACCEPT**

- `LAC-1E-V5-01` ACCEPT → named-primitive (a)-(d) gate: the (a) grammar-invoked,
  (b) varies-under-mutation, (c) `verbatim_blob_present==false`, (d) PROFILE-
  PROVEN-NARROW-LEAF conjunction is present verbatim at `1E:147`; LOCKS grep = 0.
- `LAC-1E-V5-05` ACCEPT → verbatim blob: `const CSS_GENERATED_RS: &str = r#"`
  confirmed at `skinny/crates/codegen/src/runtime_generator.rs:701`.
- `LAC-1E-V5-06` ACCEPT → green-by-exclusion: `lock14_baseline.rs` carries the
  weak-roots / `diagnostic-x86` surfaces the clause re-promotes.
- `1A-LOCK1-AMEND-001` MODIFY → the strike string "The `G:EventGrammar` type
  parameter is the generality vehicle" resolves at `LOCKS.md:620` (the cited line)
  and the §9.2 prose carrier at `ARCHITECTURE.md:1998` (3A-D01 strike target) —
  BOTH cited lines resolve.
- `LAC-2D-V3-03` MODIFY → e-graph guard: `NormalizeDirectSinkCost` is a live
  non-`#[cfg(test)]` `Rewrite<DecisionNode, NoAnalysis>` at
  `passes/src/backend_egraph.rs:191-193`, instantiated `:75` — confirmed.
- `LAC-2D-V3-04` ACCEPT → CollapsedStage slot: `lower/collapsed_stage.rs:16`
  renders `TapeFlavor::Collapsed` — confirmed (the `lower/` segment is correct).
- `LAC-2E-V6-03` MODIFY → movemask: `movemask.rs:5` =
  `vshrn_n_u16::<4>(...)` — confirmed.
- `LAC-2F-V3-03` DEFER → names its re-entry trigger (any 2F-class re-audit citing
  a "balanced-scan gap" must `ls` both trees) and is folded as a one-line
  audit-scope NOTE on D-SKV18-L16, not dropped.

### B-citation — 3C single-substrate SCOPE NOTE cites simd-scan:68, actual :67 — **REVISE**

`3C-locks-crystallisation.md:88` AND `3C-locks-v+1-diff.md:73` (the
`D-SKV18-L16-single-substrate-movemask` clause SCOPE NOTE, tagged
CH5-DEFECT-V1-03) both assert: "the totality `crates/simd-scan` exports a second
`NibbleLut`/`WideLut` classifier convention (`crates/simd-scan/src/lib.rs:68`)".
On disk:
- `crates/simd-scan/src/lib.rs:67` = `pub use alphabet::{KernelShape, NibbleLut,
  StructuralAlphabet, WideLut};` — this is where BOTH symbols are exported.
- `crates/simd-scan/src/lib.rs:68` = `pub use index::{StructuralIndex,
  next_structural_at_or_after};` — carries NEITHER `NibbleLut` NOR `WideLut`.
(`grep -n NibbleLut crates/simd-scan/src/lib.rs` returns ONLY line 67.)

This is load-bearing: it is the on-disk evidence that justifies folding the
totality second-substrate into the SK-V19 scanner-unification single-priced DEFER
(the skinny `rg=0` green is explicitly NOT a totality single-substrate proof). A
reviewer following the cite lands on the wrong line and the second-substrate claim
does not resolve where pointed. The underlying fact (the two symbols ARE exported
from `simd-scan`) is true at `:67`, so this is path-precision, not a phantom.

This is the SAME defect the SK-V18 V2 CH1 verdict raised
(`hardening/V2/CH1.md:121-137`); the V3 fold note at `3C:32` claims a "V1-FOLD
(CH5-DEFECT-V1-03)" SCOPE-NOTE addition but did NOT correct the off-by-one — the
note was added in V1 carrying the wrong line, and the V2 CH1 correction was not
ingested.

**Correction (owner 3C, severity MED):** in BOTH 3C files re-key
`crates/simd-scan/src/lib.rs:68` → `:67`.

---

## C. Cross-artefact spot-checks

### C1 — 3A SK-V18 deltas (D01-D14) — **ACCEPT**

All 14 `ARCH-3A-V4-SK18-D0x` cite resolvable T-P1/T-P2 finding-ids; none revives a
refuted route; no sixth shape / new directive / BIR. Spot-checked the load-bearing:
`D01` strike target `ARCHITECTURE.md:1998` = "type parameter is the generality
vehicle" (the literal sentence the plan deletes), grounded on `1A-SUB-025/026`,
`1A-LOCK1-AMEND-001`, `D-1E-V5-03`, `COH18-008`. `D04` relocated-seam firewall
grounded on `D-1E-V5-10`/`LAC-1E-V5-02`/`LAC-2D-V3-02`. `D14` (SK-V18-skinny vs
SK-V19-totality scope) grounded on `COH18-001` + `HANDOFF.md:17-19`.

### C1-struct — 3A delta-count reconcile — **ACCEPT** (V1 REVISE FOLDED)

`3A:57` carries the explicit DELTA-COUNT RECONCILE: `proposed_deltas_count: 26` =
12 carried `ARCH-3A-V1-D0x` + 14 SK-V18-new `ARCH-3A-V4-SK18-D0x`; the "Newly
added: None" V3-summary row is annotated as referring ONLY to the carried-V3
packet. The V1 contradiction is resolved.

### C2 — 3B scope-pivot net-LOC reconcile — **ACCEPT**

`3B:23` replaces the ad-hoc figure with the per-wave SPEC sum (P1 −4500 + P2 −700
+ P3 −5500 + P4 +15 + P5 0 = ≈−10685; P3 −5500 = 6×910 replica bodies + ~−40
collapsed rows + 1 PartialEq derive), cited `sk-v18/SPEC.md:433-437`. Net ≈−10800
is a reduction (no generated-size-budget overflow). The §13.6 tape-fold receivers
are correctly re-keyed to a SK-V19 block (the `crates/core/` adoption is SK-V19,
not SK-V18). The carried `CH4-V2-001` answered-row refers to the row-level CH4
coverage fold, not a governance disposition.

### C3 — 3D skinny-fold deltas (3D-D01-D12) — **ACCEPT** (incl. C3-xref V1 fold)

All 12 cite resolvable 1D/2X finding-ids; proposal-only. `3D-D12` relocated-seam
cross-ref is re-keyed from the V1-mis-key `3C D-L06/D-L14` to `3C
D-SKV18-L05-L10-unfork` (`3D:46`,`:88`,`:132`), with the explicit note that
`D-SKV18-L06` is the verbatim-blob clause, not the seam; the CH4 cost-scope split
(skinny P3 +1-line vs SK-V19 R16 ≈+217) is applied (`3D:47`,`:149`).

### C4 — 3E grammar-generalisation deltas — **ACCEPT**

`3E-D01..D11` carried with row-level CH4 coverage; the SK-V18 generality story
(CSS typed provider, Sheets/BBNF-self negative controls) cites grounded
`SK-V18-2C-*` findings; W5 scope bounded to a typed CSS provider; Lock-14
hardening routed to 3C/Pass Omega, not self-edited.

### C5 — 3F SK-V18 governance state (Executive Summary + §6 prose + HANDOFF carrier) — **REVISE** (V2 REVISE NOT FOLDED)

3F's OWN `3F-MH-004` delta row (`3F:86`) records the SK-V18 governance state
CORRECTLY: "**T-P1 SK-V18 as near-converged NON-normal-§3Z** (V7 lone clean
r=1.000, V8 broke the streak; consec=0, converged=false — NOT a normal two-clean
lock), **T-P2 SK-V18 as near-converged NON-normal-§3Z** (... converged=false,
consec=0 ...), and **T-P3 SK-V18 as in-cycle hardening** (... NOT yet a final-
convergence lock)", citing the on-disk SK-V18 consolidated files.

But the SAME SK-V15-style phrasing the V2 CH1 told the author to remove SURVIVES in
THREE spots in 3F, contradicting BOTH the corrected MH-004 row AND the on-disk
consolidated files:
- `3F:51` (Executive Summary): "totality T-P1 (clean-final/G1-pinned) / **T-P2
  (normal §3Z)** / **T-P3 converged**".
- `3F:111-112` (§6 MIGRATION-carrier prose): "Governance: T-P1 CLEAN-FINAL /
  G1-AUTO-PINNED (not normal §3Z); **T-P2 normal §3Z**; **T-P3 final-convergence
  lock under V≤5** (V3 clean + V5 all-ACCEPT after a V4 citation-only repair)."
- `3F:141-143` (HANDOFF "Current Totality Override" carrier): "Totality T-P1
  CONVERGED (clean-final/g1-auto-pinned), **T-P2 CONVERGED (normal §3Z), T-P3
  CONVERGED (final-convergence lock under V≤5)**."

The on-disk evidence FALSIFIES "T-P2 normal §3Z" / "CONVERGED":
- SK-V18 T-P2 CONSOLIDATED (`restart/audit/totality/p2/hardening/HARDENING-T-P2-
  CONSOLIDATED.md:17-25`): "It did **not** reach a normal §3Z two-consecutive-clean
  lock: `converged=false`, `consec=0`, `voids=0`."
- SK-V18 T-P1 CONSOLIDATED (`restart/audit/totality/p1/hardening/HARDENING-T-P1-
  CONSOLIDATED.md:44-52`): "V7 was the lone fully-clean cycle (r=1.000); V8's four
  single-locus anchor nits broke the streak before a second clean cycle landed.
  Close state: `consec=0, voids=0`" — near-converged, NOT a clean-final two-clean
  lock; T-P3 is the CURRENT in-cycle hardening (these very V3 verdicts), not a
  converged lock.

This is BOTH an internal contradiction inside 3F (the body contradicts MH-004) AND
a path:line correctness fault (the body state contradicts the cited consolidated
files). The "V1-FOLD (CH1-V1-C5)" note at `3F:26` scrubbed only the MH-004 row +
frontmatter; the V2 CH1 already recorded that this fix was INCOMPLETE
(`hardening/V2/CH1.md:196-227`), and V3 did not address it.

**Correction (owner 3F, severity HIGH):** re-ground `3F:51`, `3F:111-112`, and
`3F:141-143` onto the MH-004 record — "T-P1/T-P2 near-converged NON-normal-§3Z;
T-P3 in-cycle hardening (not yet a final-convergence lock)".

### C5-row — 3F-MH-005 asserts "T-P1/T-P2/T-P3 CONVERGED" — **REVISE** (V2 REVISE NOT FOLDED)

The `3F-MH-005` delta row (`3F:87`) — the delta that REPLACES the top-level HANDOFF
current-state block, i.e. the text Pass Omega CRUD writes verbatim into
`restart/HANDOFF.md` — carries "with skinny S-P0..S-P3 CERTIFIED and totality
**T-P1/T-P2/T-P3 CONVERGED**". This is the same SK-V15 over-claim as C5, on a
DISTINCT, separately-propagating V1-surface (the proposed HANDOFF replacement
text). It contradicts the sibling MH-004 row (which the same author wrote
correctly) and the on-disk consolidated files. Because Pass Omega CRUD carries the
MH-005 text into a V1 governance surface, the over-claim would LAND in
`restart/HANDOFF.md`.

**Correction (owner 3F, severity HIGH):** change the MH-005 delta text's
"T-P1/T-P2/T-P3 CONVERGED" to the MH-004 record (T-P1/T-P2 near-converged
NON-normal-§3Z; T-P3 in-cycle hardening), so the delta the author proposes to
WRITE matches the delta MH-004 proposes to RECORD.

### C6 — cohort cycle-label split — **ACCEPT** (V1 REVISE FOLDED)

All six artefacts carry `cycle: V4-SKV18-totality`. 3F separates the distinct
`pass_omega_index: V6` into the frontmatter with a `cycle_label_note` referencing
CH1-V1-C6. The V1 split is reconciled.

---

## D. Anti-paper-close credit (what CH1 confirms correct)

- v+1 diff APPLIES (`git apply --check` exit 0); the prior REJECT stays fixed.
- 21/21 dispositions present, EXACTLY one each, ZERO silent drops; the DEFER names
  its re-entry trigger and is folded as an audit-scope note.
- Every candidate finding-id resolves at its cited T-P1/T-P2 anchor; the candidate
  text matches the matrix disposition.
- Both PLANNED co-gate symbols honestly written as PLANNED (rg=0); never cited live.
- 16-lock count + five-`BackendShape` canon preserved; no renumber, no sixth shape,
  no new directive/BIR/substrate/public-API/retained-sidecar proposed.
- Live-code anchors verified: `CSS_GENERATED_RS:701`, `movemask.rs:5`,
  `collapsed_stage.rs:16`, `NormalizeDirectSinkCost` live non-test Rewrite
  (`:191-193`, `:75`), generality-vehicle string at `LOCKS.md:620` /
  `ARCHITECTURE.md:1998`, Pattern-H count = 71.
- 3F-MH-004 itself states the SK-V18 governance correctly — the defect is the
  surrounding prose / sibling rows that were never re-grounded to match it.

## E. Required corrections (by artefact)

1. `3C-locks-crystallisation.md:88` + `3C-locks-v+1-diff.md:73` (owner 3C, MED) —
   in the `D-SKV18-L16-single-substrate-movemask` SCOPE NOTE, re-key
   `crates/simd-scan/src/lib.rs:68` → `:67` (the `NibbleLut`/`WideLut` export).
2. `3F:51` (Exec Summary) + `3F:111-112` (§6 prose) + `3F:141-143` (HANDOFF
   carrier) (owner 3F, HIGH) — re-ground the governance state off the SK-V15
   "T-P2 normal §3Z / T-P3 final-convergence lock / CONVERGED" phrasing onto the
   SK-V18 record already correct in `3F-MH-004` (T-P1/T-P2 near-converged
   NON-normal-§3Z; T-P3 in-cycle hardening).
3. `3F-MH-005` delta row (`3F:87`) (owner 3F, HIGH) — change "totality
   T-P1/T-P2/T-P3 CONVERGED" to the MH-004 record, so the proposed HANDOFF
   replacement text does not carry the over-claim into a V1 surface.

## F. Enumeration tally (CH1 lens)

Items judged: v+1 diff applicability + invariant check (A1, A2); the 21 3C
candidate dispositions (B); the 3C headline tally (B-tally); the simd-scan
SCOPE-NOTE citation (B-citation); the cross-artefact spot-checks (C1, C1-struct,
C2, C3, C4, C5, C5-row, C6). Total = 2 + 21 + 1 + 1 + 9 = 34 items.

- ACCEPT (31): A1, A2; 21 candidate dispositions in B; B-tally; C1, C1-struct, C2,
  C3, C4, C6.
- REVISE (3): B-citation (3C simd-scan:68 → :67, both 3C files); C5 (3F Exec
  Summary + §6 prose + HANDOFF carrier carry SK-V15 governance contradicting the
  corrected MH-004 + the SK-V18 CONSOLIDATED files); C5-row (3F-MH-005 delta
  carries "T-P1/T-P2/T-P3 CONVERGED" — the same over-claim on the proposed-HANDOFF
  V1-surface).
- REJECT (0): the v+1 diff applies; no uncited delta, revived refuted route,
  silent-dropped candidate, or cross-scope violation found.

On the 8-item substantive cross-artefact set (B-citation, C1-struct, C2, C3, C4,
C5, C5-row, C6), 3 REVISE = 37.5%, clearing the V1 ≥30% expectation. The packet is
correct pending the one path-precision and the two governance-contradiction
REVISEs — all three carried forward unfixed from the SK-V18 V2 CH1 verdict.

TALLY accept=31 revise=3 reject=0
