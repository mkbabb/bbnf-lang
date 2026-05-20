Verdict: ACCEPT

# SK-V12 S-P1 Hardening V5 CH3: Regression / REDRESS

Date: 2026-05-20.
Scope: audit current repo commit `fe7ae2ab`, the S-P1 packet,
`skinny/RESULTS.md`, and `skinny/REDRESS.md` for regression and REDRESS
discipline in the confirmation cycle after V4 all-ACCEPT.

## Findings

1. Commit `fe7ae2ab40d3ba205445f07bc4cd870d68cdb1cb` is documentation-only for
   the V4 hardening archive. Its changed paths are exactly the added V4 CH1
   through CH6 files plus `V4/CONSOLIDATED.md`. A targeted diff from
   `fe7ae2ab^` to `fe7ae2ab` over `skinny/RESULTS.md`, `skinny/REDRESS.md`,
   `skinny/crates`, top-level `crates`, and the S-P1 packet files
   `p1a` through `p1f`, `skv12-p1-capture-manifest.md`, and
   `skv12-p1-replay.tsv` is empty. The audited commit therefore contains no
   behavior source, gate/report source, RESULTS, REDRESS, replay-ledger, or S-P1
   packet mutation.

2. V4 is an archive of the first clean challenge cycle, not a gate-state update.
   `V4/CONSOLIDATED.md` records all six lenses as `ACCEPT` with no required
   folds (`restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CONSOLIDATED.md:10`-`:17`),
   zero open REVISE and zero critical findings (`:19`-`:22`), and explicitly
   says no row or gate moved (`:22`). Its consequence is only procedural: one
   further all-ACCEPT confirmation cycle is required before S-P2 (`:24`-`:28`).

3. No row movement is present. P1-F records a clean pre-write status and no
   `skinny/RESULTS.md` or `skinny/REDRESS.md` diff from SK-V11 close
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:66`-`:68`).
   It extracts the live surface as 16 `parse_only S / NO-GO`, 1
   `parse_only L / NO-GO`, 4 `direct_to_struct A / GO`, 13
   `direct_to_struct N-direct / NO-GO`, 7 `real_typed_struct A / GO`, no
   generated non-JSON baseline row, and overall `N-direct / NoGo`
   (`:77`-`:83`). Recounting the current `skinny/RESULTS.md` main table yields
   the same counts, and the live summary still says `Overall outcome N-direct /
   NoGo` (`skinny/RESULTS.md:143`).

4. W3 and parse-plane routes remain closed. REDRESS 102 keeps W3 proof-only,
   with no behavior source, generated output, benchmark body, or row movement,
   and records no live W3 dispatch route through union/event substrate, retained
   class columns, `UnionTape`, structural or streaming cursor, class-lane-only
   routes, parser-owned structural projection, or W4-through-W3 cascade-lock
   (`skinny/REDRESS.md:3042`-`:3048`). The validator evidence there rejects
   parse-only SOTA movement (`:3051`-`:3056`). Current S-P1 evidence agrees:
   P1-F says `S` and `L` parse rows remain diagnostics, not SOTA admission
   (`p1f-results-delta.md:85`-`:87`), and P1-E's pre-block matrix keeps W3,
   parse-only throughput, structural scan, masking probes, PMU/cycles, numeric
   slot reuse, direct digest as typed proof, and JSON direct residual movement
   diagnostic or pre-blocked
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:326`-`:336`).

5. No gate-status mutation is present. REDRESS 119 closes W8 as a measured
   direct fixpoint, not direct `GO`, with no behavior source intervention, no
   gate schema or validator semantic change, and no `skinny/RESULTS.md` row
   movement (`skinny/REDRESS.md:3497`-`:3505`). REDRESS 120 closes SK-V11 as a
   measured fixpoint, not overall direct `GO` or grammar-generalization
   admission, with no behavior source, generated runtime, benchmark body, gate
   semantic, or `skinny/RESULTS.md` change (`:3531`-`:3538`). It preserves the
   final result surface and overall `N-direct / NoGo` (`:3539`-`:3541`) and
   routes SK-V12 to solve the generated non-JSON baseline first while treating
   the 13 residual direct rows as exhausted unless future material evidence
   exceeds REDRESS 114-119 (`:3542`-`:3553`).

6. The profile docs create no implementation authority. The capture manifest
   states that result authority remains `skinny/RESULTS.md`, records profile
   evidence only, and moves no rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:15`-`:16`).
   It makes `skv12-p1-replay.tsv` the authoritative replay surface while command
   blocks are readable recipes only (`:40`-`:43`), and labels samply rows
   retained artifact-only while self-time percentages come from exported xctrace
   Time Profiler XML (`:58`-`:62`). P1-E states that top-leaf percentages are
   self-time attribution and row admission still belongs to
   Criterion/`skinny/RESULTS.md` (`p1e-hot-leaf-attribution.md:360`-`:365`);
   it also says fresh PMU and self-time rows are cost shape, not new source-level
   material evidence against REDRESS 114-119 (`:340`-`:352`). `SYNTHESIS.md`
   reinforces the boundary: it is not behavior implementation authority
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-`:9`) and pre-blocks PMU,
   cycles, structural-scan, masking-probe, Criterion-slope, sidecar freshness,
   and parser inventory as behavior producers (`:228`-`:244`).

## Required Fold

None. V5 CH3 accepts the post-V4 confirmation surface: no row moved, W3 and the
parse plane remain closed, gate status remains `N-direct / NoGo`, and the S-P1
profile documents do not create implementation authority.
