---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis (SK-V18)
cycle: V1
target: restart/audit/totality/p3/{3A,3B,3C,3C-locks-v+1-diff,3D,3E,3F}
reviewer: CH1 CHALLENGE lens (adversarial)
generated_at: 2026-06-01
verdict_summary: "1 hard REJECT (the 3C v+1 diff does not apply to the current LOCKS.md) + multiple REVISE. The 21/21 disposition coverage and per-finding-id grounding hold, but the load-bearing v+1 diff fails git apply on TWO compounding defects, the 3C headline ACCEPT/MODIFY tally is transposed, two 3C/diff paths are wrong, and 3F cites a SK-V15 hardening packet for the SK-V18 governance state."
---

# CH1 CORRECTNESS — T-P3 SK-V18 Synthesis Packet (Cycle V1)

## Lens scope

CH1 verifies: (1) every proposed delta cites a real T-P1 finding-id or T-P2
grounding; (2) every cited V1-surface section resolves at path:line; (3) the 3C
disposition matrix references real candidates and disposes ALL 21 (8 T-P1 + 13
T-P2) with no silent drop; (4) the 3C-locks-v+1-diff applies cleanly to the
current LOCKS.md (16-lock count, no renumber). Load-bearing deltas spot-verified
on disk.

## Executive verdict

The packet is, in the main, well-grounded. I resolved the 8 T-P1 candidate
anchors (`1E:147`-`153`, `1A:180`), all 13 T-P2 anchors (`2C:380`-`382`,
`2D:95`-`98`, `2E:244`-`246`, `2F:194`-`196`), the 16-lock heading set
(`LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`), the
five-`BackendShape` canon (`lower/mod.rs:18`-`24`), and a broad sample of live
citations (`movemask.rs:5`, `runtime_generator.rs:701`, `runtime_simd.rs:169`,
`passes/src/lib.rs:658`, Pattern-H count = 71, both PLANNED co-gate symbols = 0,
`ARCHITECTURE.md:1998/:2003-2005/:1204/:2065`, `HANDOFF.md:17-19`,
`MASTER-PLAN.md:974`, Sheets grammar `:97/:137/:163`). The 21/21 disposition
coverage holds with **zero silent drops**.

BUT the single most load-bearing CH1 requirement FAILS: **the 3C-locks-v+1-diff
does NOT apply to the current LOCKS.md** (two compounding defects). That is a
hard REJECT. Path-precision and tally REVISEs accompany it. Cycle V1 ≥30% REVISE
is met on the substantive-finding set.

---

## A. The v+1 diff (load-bearing — the LOCKS singularity)

### A1 — 3C-locks-v+1-diff applicability — **REJECT**

`git apply --check` on the extracted diff → `error: corrupt patch at line 38`.
It does NOT apply. Two independent, compounding defects:

**Defect 1 (hunk-header arithmetic).** Header reads `@@ -622,6 +622,38 @@`. The
actual hunk body is 5 context + 28 added + 0 removed lines, i.e. old-side = 5,
new-side = 33. The header claims old-side = 6 and new-side = 38. The parser
exhausts the old-side body before the declared count of 6 → "corrupt patch at
line 38". Correct header: `@@ -622,5 +622,33 @@`.

**Defect 2 (context mismatch — survives the header fix).** With the header
corrected, `git apply --check` still reports `patch failed: restart/locks/LOCKS.md:622`
/ `patch does not apply`, and `git apply --recount` (auto-fixes arithmetic) ALSO
fails. The diff's leading context reconstructs as `[622 Lock-16 clause][1 blank]
## v+1 Governance Boundary[blank][The v+1 text…]` — exactly ONE blank between the
SK-V17 Lock-16 clause and the governance heading. The CURRENT LOCKS.md has TWO
blanks there: lines 623 and 624 are both blank, the heading is at 625. The diff
was authored against a single-blank state; the context window cannot match.

Correction (artefact + exact fix): in `3C-locks-v+1-diff.md` set the hunk header
to `@@ -622,5 +622,33 @@` AND re-anchor the trailing context to BOTH blank lines
(623, 624) preceding `## v+1 Governance Boundary` (625); re-confirm with
`git apply --check`. The dispatch requirement ("the 3C-locks-v+1-diff applies
cleanly to the current LOCKS.md") is unmet until both are fixed.

### A2 — Diff Invariant-Check claims (16 locks / 5 shapes / PLANNED symbols) — **ACCEPT**

All Invariant-Check assertions verify on disk: 16 lock headings at the cited
lines; five `BackendShape` variants at `lower/mod.rs:18-24`; correct insertion
anchor (SK-V17 Lock-16 clause at 622, governance boundary at 625);
`rg runtime_target_rows_collapsed` = 0 and `rg bbnf_simd_single_mask_convention`
= 0 (both PLANNED, accurately disclosed). The substance is correct; only the
patch mechanics (A1) are broken.

---

## B. The 21 disposition-matrix candidates (3C — the LOCKS singularity)

All 21 candidate-ids resolve at their cited source anchors; EXACTLY one
disposition each; zero silent drops. The 8 T-P1 = 7×`LAC-1E-V5-0[1-7]`
(`1E:147-153`) + 1×`1A-LOCK1-AMEND-001` (`1A:180`); the 13 T-P2 = 3×2C + 4×2D +
3×2E + 3×2F. Per-candidate CH1 verdict:

| candidate | 3C disposition | CH1 | note |
|---|---|---|---|
| LAC-1E-V5-01 | ACCEPT | ACCEPT | `1E:147`; SPEC `:358-390`; LOCKS grep = 0. |
| LAC-1E-V5-02 | ACCEPT | ACCEPT | `1E:148`; PLANNED symbol disclosed (rg=0). |
| LAC-1E-V5-03 | ACCEPT | ACCEPT | `1E:149`; forced-demotion grounded. |
| LAC-1E-V5-04 | MODIFY | ACCEPT | `1E:150`; sound fold with 2E V6-01. |
| LAC-1E-V5-05 | ACCEPT | ACCEPT | `1E:151`; `runtime_generator.rs:701` = `const CSS_GENERATED_RS` verified. |
| LAC-1E-V5-06 | ACCEPT | ACCEPT | `1E:152`; lock14_baseline lines cited. |
| LAC-1E-V5-07 | MODIFY | ACCEPT | `1E:153`; live count = 71, baseline 67 verified. |
| 1A-LOCK1-AMEND-001 | MODIFY | **REVISE** | `1A:180` resolves and substance right, but the clause cites `backend_egraph.rs:40-87` under `skinny/crates/codegen/src/` — file is at `skinny/crates/passes/src/backend_egraph.rs` (wrong crate). |
| LAC-2C-SK18-01 | MODIFY | ACCEPT | `2C:380`. |
| LAC-2C-SK18-02 | MODIFY | ACCEPT | `2C:381`. |
| LAC-2C-SK18-03 | MODIFY | ACCEPT | `2C:382`; `strategy.rs:137`/`css_types.rs:1` verified. |
| LAC-2D-V3-01 | ACCEPT | ACCEPT | `2D:95`; iburg DOI + `lower/mod.rs:18` verified. |
| LAC-2D-V3-02 | ACCEPT | ACCEPT | `2D:96`. |
| LAC-2D-V3-03 | MODIFY | ACCEPT | `2D:97`; activation→guard re-key sound. |
| LAC-2D-V3-04 | ACCEPT | **REVISE** | `2D:98` resolves and substance right, but cites `collapsed_stage.rs:16` as `skinny/crates/codegen/src/collapsed_stage.rs` — real path is `skinny/crates/codegen/src/lower/collapsed_stage.rs` (`lower/` dropped). |
| LAC-2E-V6-01 | MODIFY | ACCEPT | `2E:244`. |
| LAC-2E-V6-02 | MODIFY | ACCEPT | `2E:245`; `runtime_simd.rs:169`=`find_css_significant` verified. |
| LAC-2E-V6-03 | MODIFY | ACCEPT | `2E:246`; `movemask.rs:5`=`vshrn_n_u16::<4>` verified. |
| LAC-2F-V3-01 | ACCEPT | ACCEPT | `2F:194`. |
| LAC-2F-V3-02 | MODIFY | ACCEPT | `2F:195`. |
| LAC-2F-V3-03 | DEFER | ACCEPT | `2F:196`; DEFER names re-entry trigger + folded as audit-scope note. |

### B-tally — 3C headline ACCEPT/MODIFY count transposed — **REVISE**

`3C-locks-crystallisation.md:40` and the frontmatter narrative state "**11
ACCEPT, 9 MODIFY**, 0 REJECT, 1 DEFER". The actual matrix is **9 ACCEPT, 11
MODIFY**, 0 REJECT, 1 DEFER (verified row-by-row; total 21 correct). The ACCEPT
and MODIFY counts are transposed. Correction: change line 40 + the V4-summary
narrative to "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER". The candidate census (21)
and zero-silent-drop claim are themselves correct.

---

## C. Cross-artefact spot-checks

### C1 — 3A SK-V18 deltas (D01-D14) substance — **ACCEPT**

Spot-checked the load-bearing: `D01` cites `1A-SUB-025/026`, `1A-LOCK1-AMEND-001`,
`D-1E-V5-03` (=phantom `<G>`, `1E:107`), `COH18-008`; strike target
`ARCHITECTURE.md:1998` = the literal "generality vehicle" sentence. `D04` cites
`D-1E-V5-10` (relocated-seam, `1E:114`). All 14 deltas are grounded; none revives
a refuted route. ACCEPT on substance.

### C1-struct — 3A delta-count self-contradiction — **REVISE**

3A frontmatter declares `proposed_deltas_count: 26`, yet the V3-packet "V3 Delta
Summary" row (`3A:56`) says **Newly added: None**, and the V3 Proposed Delta
Table lists only the 12 carried `ARCH-3A-V1-D0x`; the 14 SK-V18 deltas appear
only in the EXTENSION (`3A:169`+). A CH1 reader who stops at the V3 summary sees
"None added" contradicting the 26-count. Correction: reconcile the frontmatter
count with an explicit "12 carried + 14 SK-V18-new" note at the V3-summary row.

### C2 — 3B scope-pivot deltas (MP-3B-SKV18-D01-D10 + 4 carried) — **ACCEPT**

The load-bearing finding (SK-V18 = generalization-on-skinny; the `crates/core/`
tape-fold MASTER labels "SK-V18" is SK-V19) is grounded: `MASTER-PLAN.md:974` =
"§13.6 SK-V18 Tape-Fold Adoption Receiver Block"; `HANDOFF.md:17-19` = "SK-V18:
it adopts the SKINNY-proven … into the totality `crates/core/` tree". `COH18-001`
cited correctly. No refuted route revived; no sixth shape / new directive / BIR.

### C3 — 3D skinny-fold deltas (3D-D01-D12) substance — **ACCEPT**

All 12 cite resolvable 1D/2X finding-ids; proposal-only (no V1 edit).

### C3-xref — 3D-D12 3C cross-reference imprecise — **REVISE**

3D-D12 (R16 relocated-seam co-gate) cross-refs "3C D-L06/D-L14". In 3C the
relocated-seam co-gate is `D-SKV18-L05-L10-unfork`; `D-SKV18-L06` is the
verbatim-blob clause, `D-SKV18-L14-*` the named-primitive/green-by-exclusion
clauses. Re-key the relocated-seam cross-ref to `D-SKV18-L05-L10-unfork`. The
underlying 1D ids (G-12, D-3) resolve, so this is precision not a phantom.

### C4 — 3E grammar-generalisation deltas (3E-D12-D18 + 11 carried) — **ACCEPT**

All 7 SK-V18 deltas cite grounded 2C ids (`SK-V18-2C-*` at `2C:213-219`,
`LAC-2C-SK18-02` at `2C:381`), 1E LAC ids, and published literature; Sheets tower
verified at `google-sheets.bbnf:97/137/163`; V1-surface targets
(`ARCHITECTURE.md:1204/:1248/:2065`) resolve; Lock-14 hardening routed to
3C/Pass Omega, not self-edited.

### C5 — 3F SK-V18 T-P3 governance citation is cross-cycle — **REVISE**

3F-MH-004 and the HANDOFF/MIGRATION carriers assert the CURRENT SK-V18 T-P3
governance state ("final-convergence lock under V≤5, V3 clean + V5 all-ACCEPT
after a V4 citation-only repair") citing `HARDENING-T-P3-V5-CONSOLIDATED.md:9-26`.
That file is the **SK-V15** T-P3 V5 packet (target `77b6e9fd7` =
"docs(sk-v15-t-p3): repair V4 citation finding"); it asserts a **42-candidate /
23-ACCEPT-19-MODIFY** 3C matrix and that the diff "applies cleanly" — both SK-V15
facts contradicting the current SK-V18 (21-candidate / 9-11 split / non-applying
diff). Re-ground the SK-V18 T-P3 governance paragraph on the SK-V18 T-P3
convergence record (mirroring 3C's honest use of the SK-V18
`HARDENING-T-P1-CONSOLIDATED.md` / `HARDENING-T-P2-CONSOLIDATED.md`), not the
SK-V15 V5 file. The other 11 3F deltas resolve (`HANDOFF.md:17-19`,
`runtime_generator.rs:701`, `tape/mod.rs:175`, `LOCKS.md:349`, `LOCKS.md:620`) —
ACCEPT.

### C6 — cohort cycle-label split — **REVISE**

3A/3B/3C/3D/3E carry `cycle: V4-SKV18`; 3F carries `cycle: V6-SKV18` (and a
"Pass Omega V6"). A single T-P3 synthesis cohort should converge on one cycle
label; the V4/V6 split will mislead the Pass Omega CRUD reader about which
hardening cycle produced the packet. Reconcile to one label (or annotate why the
3F MIGRATION/HANDOFF leg carries a distinct Pass-Omega index).

---

## D. Anti-paper-close credit (what CH1 confirms correct)

- 21/21 dispositions present, EXACTLY one each, **zero silent drops**; the DEFER
  (LAC-2F-V3-03) names its re-entry trigger and is folded as an audit-scope note.
- Every candidate finding-id resolves at its cited T-P1/T-P2 anchor.
- Both PLANNED co-gate symbols honestly written as PLANNED (rg=0); never cited live.
- 16-lock count + five-`BackendShape` canon preserved and verified on disk; no
  renumber, no sixth shape, no new directive/BIR/substrate proposed.
- 3A/3B/3E/3F V1-surface anchors resolve at path:line.

## E. Required corrections (by artefact)

1. `3C-locks-v+1-diff.md` — REJECT until fixed: header → `@@ -622,5 +622,33 @@`
   AND re-anchor trailing context for the two blanks (623, 624) before line 625;
   re-confirm `git apply --check`.
2. `3C-locks-crystallisation.md:40` + frontmatter — tally → "9 ACCEPT, 11 MODIFY,
   0 REJECT, 1 DEFER".
3. `3C` row LAC-2D-V3-04 + diff CollapsedStage clause — path →
   `skinny/crates/codegen/src/lower/collapsed_stage.rs:16`.
4. `3C` row 1A-LOCK1-AMEND-001 + diff cursor-generality clause — path →
   `skinny/crates/passes/src/backend_egraph.rs`.
5. `3A:56` — reconcile "Newly added: None" with `proposed_deltas_count: 26`.
6. `3D-D12` — re-key relocated-seam cross-ref to `D-SKV18-L05-L10-unfork`.
7. `3F-MH-004` + carriers — re-ground the SK-V18 T-P3 governance citation off the
   SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md`.
8. cohort — reconcile the V4/V6 cycle-label split.

## F. Enumeration tally (CH1 lens)

Items judged: the v+1 diff applicability + its invariant check (A1, A2); the 21
3C candidate dispositions (B); the 3C headline tally (B-tally); the cross-artefact
spot-checks (C1, C1-struct, C2, C3, C3-xref, C4, C5, C6). Total = 2 + 21 + 1 + 8
= 32 items.

- ACCEPT (24): A2; 19 candidate rows in B; C1, C2, C3, C4.
- REVISE (7): B/1A-LOCK1-AMEND-001 (wrong crate path); B/LAC-2D-V3-04 (dropped
  `lower/` segment); B-tally (transposed counts); C1-struct (3A count); C3-xref
  (3D cross-ref); C5 (3F SK-V15 governance mis-citation); C6 (V4/V6 split).
- REJECT (1): A1 (3C-locks-v+1-diff does not apply to current LOCKS.md — malformed
  header AND blank-line context mismatch).

REVISE fraction on the substantive-finding set (the 8 non-mechanical-pass items
= 7 REVISE + 1 REJECT, plus the diff REJECT dominating) clears the V1 ≥30%
expectation. The packet cannot pass CH1 until A1 (the non-applying diff) is
fixed.

TALLY accept=24 revise=7 reject=1
