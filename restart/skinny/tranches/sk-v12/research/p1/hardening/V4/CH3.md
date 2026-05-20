Verdict: ACCEPT

# SK-V12 S-P1 Hardening V4 CH3: Regression / REDRESS

Date: 2026-05-20.
Scope: audit commit `6d19429f`, the S-P1 packet, `skinny/RESULTS.md`, and
`skinny/REDRESS.md` for regression and REDRESS discipline after the V3
replay/source/symbol hardening fold.

## Findings

1. Commit `6d19429f` is confined to profile documentation. Its changed paths
   are exactly `restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md`
   and `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`.
   A targeted diff against `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/crates`,
   top-level `crates`, and `skv12-p1-replay.tsv` is empty. That confirms no
   row table, REDRESS ledger, source, gate, or replay-ledger mutation in the
   audited commit.

2. The V3 fold only resolves retained xctrace symbol labels. The fold ledger
   scopes the change as a CH1 fold "without behavior-source changes", says it
   re-parsed existing xctrace Time Profiler XML under `/tmp/skv12-p1`, and says
   it did not record fresh benchmark or profile runs
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:5`,
   `:11-12`). Its validation reports 82 summary rows and 410 detail rows with
   zero source `:0`, zero symbol `:0`, zero any-field `:0`, and zero unresolved
   markers (`:24-49`). Its unchanged-boundaries section keeps the replay TSV as
   the exact command surface, preserves PMU aggregates, preserves the Mode III
   absence boundary, and states `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
   behavior source remain unchanged (`:56-63`).

3. The S-P1 manifest still fences result authority and replay authority. It
   states that `skinny/RESULTS.md` remains result authority and that the manifest
   records profile evidence only and moves no rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:15-16`).
   It defines `skv12-p1-replay.tsv` as the authoritative command surface for
   independent replay, with command blocks only as readable recipes (`:40-43`),
   and labels samply rows retained artifact-only while self-time percentages come
   from exported xctrace Time Profiler XML (`:58-62`). The folded invariant is
   limited to concrete source anchors and resolved symbol labels in derived TSVs:
   82/82 summary rows and 410/410 detail rows contain no `:0` in the named
   source/symbol fields and no `UNRESOLVED_LINE_ZERO` markers (`:165-169`).

4. No row movement or gate-status mutation is present in the S-P1 packet. P1-F
   records that `git status --short` was clean before that artifact and that the
   SK-V11 close comparison produced no `skinny/RESULTS.md` or `skinny/REDRESS.md`
   diff (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:66-68`).
   It extracts the live surface as 16 `parse_only S / NO-GO`, 1
   `parse_only L / NO-GO`, 4 `direct_to_struct A / GO`, 13
   `direct_to_struct N-direct / NO-GO`, 7 `real_typed_struct A / GO`, and
   overall `N-direct / NoGo`, all with no SK-V11 delta (`:77-83`). Current
   `skinny/RESULTS.md` matches that surface: the main table still ends with
   "Overall outcome N-direct / NoGo" (`skinny/RESULTS.md:143`).

5. W3 and parse-plane reopen remain blocked. REDRESS 102 admits W3 only as
   proof-only, with no behavior source, generated output, benchmark body, or row
   movement; it also records no live W3 dispatch route and validator rejection
   of parse-only SOTA movement (`skinny/REDRESS.md:3042-3057`). P1-E repeats
   that parse rows remain diagnostic only and cannot admit SK-V12 or reopen
   W3/parse-only routes
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:221-223`).
   Its REDRESS pre-block matrix keeps structural rediscovery, retained classes,
   sidecars, cursors, class columns, `UnionTape`, parse-only throughput,
   structural scan, masking probes, PMU/cycles, numeric slot reuse, and JSON
   direct residual movement diagnostic or pre-blocked (`:326-336`).

6. REDRESS close authority is preserved. REDRESS 119 closes W8 as a measured
   direct fixpoint, not direct `GO`, with no behavior source intervention, no
   W8a split, no gate schema or validator semantic change, and no
   `skinny/RESULTS.md` row movement (`skinny/REDRESS.md:3497-3505`). REDRESS 120
   closes SK-V11 as a measured fixpoint, not overall direct `GO` or grammar
   generalization, with no behavior source, generated runtime, benchmark body,
   gate semantic, or `skinny/RESULTS.md` change (`:3531-3538`). It preserves the
   final surface as 16 `parse_only S / NO-GO`, 1 `L / NO-GO`, 4 direct
   `A / GO`, 13 direct `N-direct / NO-GO`, 7 typed `A / GO`, and overall
   `N-direct / NoGo` (`:3539-3541`), and routes SK-V12 to solve generated
   non-JSON baseline first while treating residual direct rows as exhausted
   unless future material evidence exceeds REDRESS 114-119 (`:3542-3553`).

7. The replay/source/symbol folds create no implementation authority. The
   packet treats fresh PMU and self-time rows as useful cost shape only, not
   new source-level material evidence against REDRESS 114-119
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:340-352`).
   Its planning section keeps the primary SK-V12 target at the generated
   non-JSON baseline blocker and says a JSON-only micro-wave before that priority
   succeeds or records a measured block would contradict the opening contract
   (`:299-322`). The source and symbol labels therefore remain attribution
   evidence, not source-edit, dispatch, gate, or row-admission authority.

## Required Fold

None. The V4 CH3 REDRESS/regression audit accepts commit `6d19429f`: no row
movement occurred, W3 and parse-plane routes remain closed, gate status remains
`N-direct / NoGo`, and the replay/source/symbol folds did not create
implementation authority.
