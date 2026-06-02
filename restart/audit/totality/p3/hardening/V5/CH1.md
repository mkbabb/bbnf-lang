---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis (SK-V18)
cycle: V5
target: restart/audit/totality/p3/{3A,3B,3C,3C-locks-v+1-diff,3D,3E,3F}
reviewer: CH1 CHALLENGE lens (adversarial)
generated_at: 2026-06-01
verdict_summary: "The structural / LOCKS-singularity floor is correct and re-verified independently on disk: the v+1 diff APPLIES clean (git apply --check exit 0, header @@ -622,6 +622,33 @@, 37-line block, 6 context + 27 added); all 21 disposition candidates (8 T-P1 + 13 T-P2) resolve at their cited finding-ids (1E:147-153, 1A:180, 2C:380-382, 2D:95-98, 2E:244-246, 2F:194-196) with EXACTLY one disposition and ZERO silent drops; the disposition-column tally re-counts to 9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER / total 21, byte-matching the 3C:49 headline; the 16-lock count + five-BackendShape canon (EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage live at lower/mod.rs:18-24) are preserved with no renumber / sixth shape / new directive / BIR / substrate / public-API / retained-sidecar; both PLANNED co-gate symbols (runtime_target_rows_collapsed, bbnf_simd_single_mask_convention) are rg=0 and disclosed PLANNED, never cited live; the three LOCKS-discipline greps (named-primitive / PROFILE-PROVEN-NARROW-LEAF / emit_shape_source) are all 0 (the addendum is an anti-paper-close gate, not a relabelled close); no boundary fault (git status of all five V1 surfaces EMPTY). The three V3-cycle CH1 REVISEs remain FOLDED and re-verified (simd-scan:67 NibbleLut re-export with grep returning ONLY 67; 3F:53-56 / MH-004 / MH-005 governance prose all read near-converged NON-normal-§3Z matching the on-disk T-P1/T-P2 CONSOLIDATED). Every load-bearing live anchor resolves: CSS_GENERATED_RS:701, movemask.rs:5, collapsed_stage.rs:16, NormalizeDirectSinkCost:191-193/:75, generality-vehicle at LOCKS.md:620 + ARCHITECTURE.md:1998, Pattern-H=71, 3B per-wave net-LOC -4500/-700/-5500/+15/0=-10685 matching SPEC:433-437, CH2-V3-R01 token set {GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar} byte-identical across all six artefacts. ONE REVISE survives - the V4-flagged 3E 2C-grounding line-drift is NOT folded and is CARRIED ORPHAN into V5: 3E-D01/D02/D04/D07/D08/D09 cite 2C:62/64/70/72/73/75 but the named finding-ids resolve at 2C:69/71/77/79/80/82 (systematic +7); additionally the 3E Executive Summary (3E:83) cites 2C:72/75 for the Sheets/BBNF-self claim, but 2C:72=SK-V15-2C-CSS-CUSTOM-PROPERTIES-AND-CALC and 2C:75=SK-V15-2C-CSS-BROADCAST-ADMISSION (real-but-WRONG CSS candidates), with the intended SHEETS-FUTURE / FUTURE-ONBOARDING ids at 2C:79/82. Named ids resolve (no silent drop, REVISE not REJECT), but a reviewer following the cite lands on the wrong line/candidate; 3E carries NO drift disclosure (grep staler|snapshot|line-drift = 0; only the unrelated CH3-V3-R1 Pattern-H RE-KEY at 3E-D11). The 2C dossier (mtime 19:45) precedes 3E (mtime 20:53): the author keyed to a staler 2C and the re-edited SK-V18 2C shifted these ids down ~7 lines. Same path-precision class as the now-folded V3 simd-scan REVISE; identical to the V4 CH1 finding and demonstrably unaddressed since (3E mtime unchanged from the V4 review)."
---

# CH1 CORRECTNESS — T-P3 SK-V18 Synthesis Packet (Cycle V5)

## Lens scope

CH1 verifies: (1) every proposed delta cites a real T-P1 finding-id or T-P2
grounding; (2) every cited V1-surface section resolves at path:line; (3) the 3C
disposition matrix references real candidates and disposes ALL 21 (8 T-P1 + 13
T-P2) with no silent drop; (4) the 3C-locks-v+1-diff applies cleanly to the
current LOCKS.md (16-lock count, no renumber). Every load-bearing delta was
spot-verified on disk against the current (2026-06-01) artefacts, not the V4
write-up. I did NOT rubber-stamp V4: each claim was re-extracted independently.

## Executive verdict

The structural / LOCKS-singularity floor is correct. The three V3 REVISEs the
SK-V18 V3 CH1 verdict raised remain FOLDED and I re-confirmed each on disk. ONE
REVISE survives this cycle — the V4-flagged 3E 2C-grounding line-drift, which is
NOT folded and is carried ORPHAN into V5 (the same finding, the same six delta
cites, plus the Exec-Summary cite the V4 verdict did not isolate). No REJECT: the
v+1 diff applies, all 21 candidates resolve with one disposition each, zero silent
drops, no revived refuted route, no cross-scope violation. Cycle V1 expects ≥30%
REVISE; on the substantive cross-artefact set this is 1 REVISE — below the V1 bar
because the three V3 REVISEs are folded and only the carried-3E drift remains live.
The packet is materially converged; this verdict records the single residual and
its carried-orphan status.

---

## A. The v+1 diff (load-bearing — the LOCKS singularity)

### A1 — 3C-locks-v+1-diff applicability — **ACCEPT**

Extracted the fenced `diff` block via the dispatch awk recipe (37 lines, single
header `@@ -622,6 +622,33 @@`) and ran `git apply --check /tmp/tp3-locks-v5.diff`
against live `restart/locks/LOCKS.md`: **exit 0, clean apply**. The insertion
anchors on the SK-V17 Lock-16 NEON-classifier-manifest clause at `:622` with both
blank context lines, before the `## v+1 Governance Boundary` heading. Arithmetic:
6 context + 27 added = 33 new-side; matches the header.

### A2 — Diff Invariant-Check claims (16 locks / 5 shapes / PLANNED symbols) — **ACCEPT**

`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = **16**. The 16 numbered lock
headings resolve at the diff-claimed lines `:75,160,170,179,181,183,200,202,260,
269,319,328,336,349,436,453` (each read on disk; each carries `N. **<lock>**`).
Five `BackendShape` variants `{EagerTape, OffsetTape, EventTape, SinkOnly,
CollapsedStage}` confirmed live in the `select_lowering` match at
`skinny/crates/codegen/src/lower/mod.rs:18`-`24`. `rg -c
runtime_target_rows_collapsed skinny/crates skinny/xtask` = 0 and `rg -c
bbnf_simd_single_mask_convention skinny/crates` = 0 — both PLANNED, honestly
disclosed inline, never cited live. LOCKS greps `named-primitive` /
`PROFILE-PROVEN-NARROW-LEAF` / `emit_shape_source` all = 0 (the discipline is
genuinely absent, so the addendum is an anti-paper-close gate, not a relabelled
close). `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` =
**71** (matches the D-SKV18-L13 / LAC-1E-V5-07 recensus claim).

---

## B. The 21 disposition-matrix candidates (3C — the LOCKS singularity)

All 21 candidate-ids resolve at their cited source anchors; EXACTLY one
disposition each; ZERO silent drops. I read each anchor and confirmed the
candidate text matches the matrix disposition:

- **8 T-P1** = 7×`LAC-1E-V5-0[1-7]` (resolve at `1E:147`-`153`, each row's
  candidate text matches the matrix wording) + 1×`1A-LOCK1-AMEND-001` (`1A:180`,
  the Lock-14 generality-vehicle strike, cites `LOCKS.md:620`).
- **13 T-P2** = 3×2C (`LAC-2C-SK18-01/02/03` at `2C:380`-`382`) + 4×2D
  (`LAC-2D-V3-01..04` at `2D:95`-`98`) + 3×2E (`LAC-2E-V6-01/02/03` at `2E:244`-
  `246`) + 3×2F (`LAC-2F-V3-01/02/03` at `2F:194`-`196`). All read on disk;
  candidate text matches.

### B-tally — 3C headline ACCEPT/MODIFY/DEFER count — **ACCEPT**

`3C:49` reads "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER". I re-extracted the
disposition COLUMN (field 6) of every matrix row `3C:124`-`144` with an awk
field-split (not a substring grep — substring grep over-counts because rationale
prose contains "REJECT"/"DEFER" words). Column tally: **ACCEPT=9, MODIFY=11,
REJECT=0, DEFER=1, total=21**. Byte-matches the headline.
- ACCEPT (9) = {1E-V5-01, 1E-V5-02, 1E-V5-03, 1E-V5-05, 1E-V5-06, 2D-V3-01,
  2D-V3-02, 2D-V3-04, 2F-V3-01}
- MODIFY (11) = {1E-V5-04, 1E-V5-07, 1A-LOCK1-AMEND-001, 2C-SK18-01, 2C-SK18-02,
  2C-SK18-03, 2D-V3-03, 2E-V6-01, 2E-V6-02, 2E-V6-03, 2F-V3-02}
- DEFER (1) = {2F-V3-03}

### B-load-bearing — spot-verified candidate citations — **ACCEPT**

- `LAC-1E-V5-01` ACCEPT → named-primitive (a)-(d) gate: the candidate text "Bind
  the §6 named-primitive (a)-(d) gate" is present at `1E:147`; LOCKS grep
  `named-primitive` / `PROFILE-PROVEN-NARROW-LEAF` = 0.
- `LAC-1E-V5-05` ACCEPT → verbatim blob: `const CSS_GENERATED_RS: &str = r#"`
  confirmed at `skinny/crates/codegen/src/runtime_generator.rs:701`.
- `1A-LOCK1-AMEND-001` MODIFY → the strike string "The `G:EventGrammar` type
  parameter is the generality vehicle" resolves at `LOCKS.md:620`; the §9.2 prose
  carrier resolves at `ARCHITECTURE.md:1998` (the literal sentence; 3A-D01 strike
  target). The `1A:180` candidate row names `LOCKS.md:620` as companion.
- `LAC-2D-V3-03` MODIFY → e-graph guard: `NormalizeDirectSinkCost` is a live
  non-`#[cfg(test)]` `Rewrite<DecisionNode, NoAnalysis>` at
  `skinny/crates/passes/src/backend_egraph.rs:191`-`193`, instantiated `:75` —
  confirmed on disk.
- `LAC-2D-V3-04` ACCEPT → CollapsedStage slot: `lower/collapsed_stage.rs:16`
  renders `TapeFlavor::Collapsed` — confirmed.
- `LAC-2E-V6-03` MODIFY → movemask: `aarch64/movemask.rs:5` =
  `vshrn_n_u16::<4>(...)` — confirmed.
- `LAC-2F-V3-03` DEFER → names its re-entry trigger (any 2F-class re-audit citing
  a "balanced-scan gap" must `ls` both trees) and is folded as a one-line
  audit-scope NOTE on D-SKV18-L16, not dropped.

### B-citation — 3C single-substrate SCOPE NOTE simd-scan cite — **ACCEPT** (V3 REVISE FOLDED)

`3C-locks-crystallisation.md:90` AND `3C-locks-v+1-diff.md:76` both read
`crates/simd-scan/src/lib.rs:67`. On disk `:67` =
`pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};` (the
NibbleLut/WideLut export — `grep -n NibbleLut crates/simd-scan/src/lib.rs` returns
ONLY 67) and `:68` =
`pub use index::{StructuralIndex, next_structural_at_or_after};` (the probe API
the cite explicitly distinguishes). The V3 off-by-one is corrected and stays
corrected.

---

## C. Cross-artefact spot-checks

### C1 — 3A SK-V18 deltas (D01-D14) + delta-count reconcile — **ACCEPT** (V1 REVISE FOLDED)

`proposed_deltas_count: 26` = 12 carried `ARCH-3A-V1-D0x` + 14 SK-V18-new
`ARCH-3A-V4-SK18-D0x`; the explicit DELTA-COUNT RECONCILE at `3A:60` annotates the
"Newly added: None" V3-summary row as referring ONLY to the carried-V3 packet.
`ARCH-3A-V4-SK18-D01` strike target `ARCHITECTURE.md:1998` = "The `G:EventGrammar`
type parameter is the generality vehicle" (the literal sentence, read on disk),
grounded on `1A-SUB-025`/`026`, `1A-LOCK1-AMEND-001`, `D-1E-V5-03`, `COH18-008`.
No sixth shape / new directive / BIR proposed.

### C2 — 3B scope-pivot net-LOC reconcile — **ACCEPT**

`3B:23` replaces the ad-hoc −10700 with the per-wave SPEC sum P1 −4500 + P2 −700 +
P3 −5500 + P4 +15 + P5 0 = ≈−10685, cited `sk-v18/SPEC.md:433-437`. I read
`SPEC.md:433`-`437`: the P1/P2/P3/P4/P5 deletion figures match exactly (P3 −5500 =
6×910 replica bodies + ~−40 collapsed rows + 1 PartialEq derive). Net is a
REDUCTION, no generated-size-budget overflow.

### C3 — 3D skinny-fold deltas (3D-D01-D12) — **ACCEPT**

All cite resolvable 1D/2X finding-ids; proposal-only. `3D-D12` relocated-seam
cross-ref re-keyed to `3C D-SKV18-L05-L10-unfork` (`3D:46`,`:90`) with the explicit
note `D-SKV18-L06` is the verbatim-blob clause, not the seam (V1-FOLD
CH1-V1-C3-xref); the CH4 cost-scope split (skinny P3 +1-line vs SK-V19 R16 ≈+217)
is applied (`3D:47`). `3D-D04` already carries the certified SK-V18 token form.

### C4 — 3E grammar-generalisation deltas: 2C grounding line-drift — **REVISE** (CARRIED ORPHAN from V4)

The 11 carried `3E-D01..D11` deltas ground on SK-V15-2C finding-ids in the current
SK-V18 2C dossier. SIX of the cited `2C:NN` line numbers are stale by a systematic
+7 lines — the cited path:line lands on stray prose/header lines, and the named
finding-id resolves +7 lines below:

| 3E delta | cites | finding-id | actual line | drift |
|---|---|---|---|---|
| `3E-D01` (`3E:103`) | `2C:62` | `SK-V15-2C-METADATA-GRAMMAR-OWNERSHIP` | `2C:69` | +7 |
| `3E-D02` (`3E:104`) | `2C:64` | `SK-V15-2C-CSS-VALUE-API-SURFACE` | `2C:71` | +7 |
| `3E-D04` (`3E:106`) | `2C:70` | `SK-V15-2C-GENERIC-GRAMMAR-SWITCH` | `2C:77` | +7 |
| `3E-D07` (`3E:109`) | `2C:72` | `SK-V15-2C-SHEETS-FUTURE-GRAMMAR` | `2C:79` | +7 |
| `3E-D08` (`3E:110`) | `2C:73` | `SK-V15-2C-BBNF-SELF-FUTURE-GRAMMAR` | `2C:80` | +7 |
| `3E-D09` (`3E:111`) | `2C:75` | `SK-V15-2C-FUTURE-GRAMMAR-ONBOARDING-TEST` | `2C:82` | +7 |

On disk `2C:62` = "a live SK-V18 cost cell. The SK-V15→SK-V18 wave map is in …"
(stray preamble prose); `2C:64` = "Grounding Table. Do NOT copy a `W#` owner …"
(header prose) — neither carries the named finding-id. The named ids DO resolve
(no silent drop, no phantom) at `2C:69/71/77/79/80/82`.

ADDITIONALLY — and SHARPER than the V4 finding isolated — the 3E **Executive
Summary** (`3E:83`) cites `2C:72` and `2C:75` to support the prose claim "Sheets
and BBNF-self are mandatory falsifiers … because their grammar forms exercise
references, functions, operators, directives, imports, and recursive grammar
ownership" (`3E:79`-`82`). But on disk `2C:72` = `SK-V15-2C-CSS-CUSTOM-
PROPERTIES-AND-CALC` and `2C:75` = `SK-V15-2C-CSS-BROADCAST-ADMISSION` — two REAL
BUT WRONG CSS-scoped candidates, not the Sheets/BBNF-self ids the prose intends
(`SK-V15-2C-SHEETS-FUTURE-GRAMMAR` at `2C:79`, `SK-V15-2C-FUTURE-GRAMMAR-
ONBOARDING-TEST` at `2C:82`). This is the same +7 systematic drift: the cite lands
on a wrong-candidate line rather than no-candidate prose, so it is at least as
misleading as the delta-table cites.

The 2C dossier was last written 19:45; 3E was last written 20:53 (mtime unchanged
since the V4 review), so the author keyed to a STALER 2C snapshot and the re-edited
SK-V18 2C shifted these finding-ids down ~7 lines. 3E carries NO acknowledgement of
the drift (`grep -niE 'staler|snapshot|line.drift|pre-re-edit'` = 0; the only
RE-KEY in 3E is the unrelated CH3-V3-R1 Pattern-H 67→71 note at 3E-D11). This is
the SAME path-precision class as the now-folded V3 simd-scan REVISE and is
IDENTICAL to the V4 CH1 finding — demonstrably UNADDRESSED since (3E mtime did not
advance between the V4 review and this V5 review). It is therefore a CARRIED ORPHAN
REVISE: a defect flagged in the immediately prior cycle, with a concrete repair
directive, that the packet did not fold.

**Correction (owner 3E, severity MED):** re-key the six `2C:NN` grounding cites in
`3E-D01/D02/D04/D07/D08/D09` to the current SK-V18 2C positions
`2C:69/71/77/79/80/82` (each +7), AND re-key the Exec-Summary `3E:83` cite from
`2C:72/75` to `2C:79/82`, OR carry a one-line "carried SK-V15 cites keyed to the
pre-re-edit 2C snapshot; finding-ids resolve by name +7 below" disclosure. No
disposition changes; finding-ids resolve by name.

### C5 — 3F SK-V18 governance state — **ACCEPT** (V3 REVISE FOLDED)

`3F-MH-004` (`3F:91`) records the SK-V18 governance correctly. The three prose
carriers the V3 CH1 flagged remain re-grounded onto MH-004:
- `3F:53`-`56` (Exec Summary): "totality T-P1 near-converged NON-normal-§3Z (V7
  lone clean r=1.000, V8 broke the streak; consec=0, converged=false) / T-P2
  near-converged NON-normal-§3Z (converged=false, consec=0) / T-P3 in-cycle
  hardening (… NOT yet a final-convergence lock)".
- `3F-MH-005` (`3F:92`) HANDOFF replacement carries the same near-converged
  NON-normal-§3Z wording, not "CONVERGED".
Both match the on-disk SK-V18 T-P1 CONSOLIDATED (`…/p1/hardening/HARDENING-T-P1-
CONSOLIDATED.md:50`-`53`: "V7 was the lone … `consec=0, voids=0, converged=false`")
and T-P2 CONSOLIDATED (`…/p2/hardening/HARDENING-T-P2-CONSOLIDATED.md:19`-`20`,
`:204`-`205`: "did not reach a normal §3Z two-consecutive-clean lock:
`converged=false`, `consec=0`, `voids=0` … V≤5 hard-ceiling near-converged"). The
V3 over-claim stays removed.

### C6 — boundary fault (no live V1-surface edits) — **ACCEPT**

`git status --short` of `LOCKS.md`, `ARCHITECTURE.md`, `MASTER-PLAN.md`,
`MIGRATION.md`, `HANDOFF.md` is EMPTY for all five — T-P3 proposes only; no live
edit to any V1 surface before Pass Omega CRUD. (The dirty `restart/audit/totality/
p1/1E-*.md`, `1F-*.md` in global git status are P1 EVIDENCE files, not V1
surfaces.)

### C7 — CH2-V3-R01 byte-identical token set — **ACCEPT**

The certified SK-V18 SPEC token-set body
`{GENERATED_RS,CSS_GENERATED_RS,EventGrammar,*EventGrammar}` is present and
identical across ALL SIX cited loci: `3A:217`, `3B:149`, `3C-crystallisation:89`,
`3C-v+1-diff:74`, `3D:91`, `3E:337` (the SET BODY matches at every locus; 3A/3B/3D
use `⊇` / spaced punctuation as a prefix but the brace body is byte-identical). The
"byte-identical across 3A-D11/3B-P4/3C/3D/3E/the v+1 diff" self-claim holds.
SPEC:711-712 carries the canonical form.

---

## D. Anti-paper-close credit (what CH1 confirms correct)

- v+1 diff APPLIES (`git apply --check` exit 0); LOCKS singularity intact.
- 21/21 dispositions present, EXACTLY one each, ZERO silent drops; the DEFER names
  its re-entry trigger and is folded as an audit-scope note.
- Every candidate finding-id resolves at its cited T-P1/T-P2 anchor; candidate text
  matches the matrix disposition.
- Disposition-column tally re-counted via field-split: 9/11/0/1, matching the
  headline.
- All three V3 REVISEs FOLDED and independently re-verified (simd-scan :67; 3F
  governance prose; 3F-MH-005).
- Both PLANNED co-gate symbols honestly written as PLANNED (rg=0); never cited live.
- 16-lock count + five-`BackendShape` canon preserved; no renumber, sixth shape,
  new directive/BIR/substrate/public-API/retained-sidecar.
- No boundary fault: all five V1 surfaces clean in git status.
- CH2-V3-R01 token set byte-identical across all six artefacts.
- Live anchors verified: `CSS_GENERATED_RS:701`, `movemask.rs:5`,
  `collapsed_stage.rs:16`, `NormalizeDirectSinkCost:191-193/:75`, five
  BackendShape at `lower/mod.rs:18-24`, generality-vehicle at `LOCKS.md:620` /
  `ARCHITECTURE.md:1998`, Pattern-H = 71, 3B net-LOC −10685 matching SPEC:433-437.

## E. Required corrections (by artefact)

1. `3E-D01/D02/D04/D07/D08/D09` (`3E:103,104,106,109,110,111`) AND the Exec Summary
   (`3E:83`) (owner 3E, MED) — re-key the six stale `2C:62/64/70/72/73/75`
   grounding cites to the current SK-V18 2C positions `2C:69/71/77/79/80/82` (each
   +7), and re-key the Exec-Summary `2C:72/75` cite (real-but-WRONG CSS candidates)
   to `2C:79/82`, OR disclose the carried-SK-V15-snapshot keying. Finding-ids
   resolve by name; path-precision only. This is a CARRIED ORPHAN from V4 (the same
   finding, unaddressed; 3E mtime unchanged since the V4 review).

## F. Enumeration tally (CH1 lens)

Items judged: v+1 diff applicability + invariant check (A1, A2); the 21 3C
candidate dispositions (B); the 3C headline tally (B-tally); the simd-scan
SCOPE-NOTE citation (B-citation); the cross-artefact spot-checks (C1, C2, C3, C4,
C5, C6, C7). Total = 2 + 21 + 1 + 1 + 7 = 32 items.

- ACCEPT (31): A1, A2; 21 candidate dispositions in B; B-tally; B-citation
  (simd-scan :67, V3 REVISE folded); C1 (3A + delta-count reconcile, V1 REVISE
  folded), C2, C3, C5 (3F governance, V3 REVISE folded), C6, C7.
- REVISE (1): C4 (3E-D01/D02/D04/D07/D08/D09 + the Exec-Summary `3E:83` ground on
  stale `2C:62/64/70/72/73/75` cites that drift +7 against the current SK-V18 2C
  dossier; the named finding-ids resolve at `2C:69/71/77/79/80/82`; the Exec-Summary
  cite lands on the wrong real CSS candidates — CARRIED ORPHAN from V4).
- REJECT (0): the v+1 diff applies; no uncited delta, revived refuted route,
  silent-dropped candidate, or cross-scope violation found.

On the substantive cross-artefact set, 1 REVISE is BELOW the V1 ≥30% expectation —
because the three V3 REVISEs are now FOLDED and only the carried-3E 2C-grounding
drift remains live. The packet is materially converged; the single residual is a
MED path-precision repair of the same class as the now-fixed V3 simd-scan defect,
but it is a CARRIED ORPHAN: flagged in V4, unaddressed, and therefore an orphan
REVISE that blocks a clean cycle.

TALLY accept=31 revise=1 reject=0
