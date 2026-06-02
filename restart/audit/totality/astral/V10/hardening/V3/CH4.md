# CH4 COST — SK-V18 Pass Omega V10 CHALLENGE (cycle V3)

LENS: does every staged amendment carry a LOC budget + propagation cost (files /
sites touched); are the CRUD operations realistic + bounded? Adversarial review of
the 6 Ω artefacts + 5 staged deltas under `restart/audit/totality/astral/V10/`
against the live V1 surfaces and the converged T-P1/T-P2/T-P3 evidence.

Disposition convention: ACCEPT = budgeted + bounded + verified; REVISE = budgeted
but a count/scope/arithmetic error the named artefact must correct; REJECT = a
non-applying diff / revived REDRESS route / Lock-14 narrowing / coupling /
uncited claim. Cycle V1 expects ≥30% REVISE.

## Load-Bearing Spot-Verifications (run at HEAD)

| Check | Result |
|---|---|
| `git apply --check` on staged `locks-diff.md` (awk-extracted diff block) | EXIT 0 — APPLIES CLEAN |
| `git apply --check` on `architecture-delta.staged.md` two gated hunks | EXIT 0 — APPLIES CLEAN |
| 16 numbered locks at `:75..:453` | confirmed; addendum adds no Lock 17 |
| 5 `BackendShape` variants (`lower/mod.rs:20-24`, `cost.rs:334`) | confirmed; no 6th |
| PLANNED symbols `runtime_target_rows_collapsed` / `bbnf_simd_single_mask_convention` rg | both = 0 (PLANNED, not live) — honest |
| REDRESS 51/53/246/247 cited as G6/G4/G2 bounds | all 4 exist + REJECTED; admissible-vs-rejected distinction sound (NOT a revived route) |
| §H anchors §13.6:974 / §24:1346 / §25:1415 / MP.NW6:662 / Lock-10:616 | all resolve verbatim |
| ARCH anchors §7.4:1371 / §9.2:1998 / §0:19; MIGRATION §0.0:30/§17:886/§19:925; HANDOFF :3/:16-19/:90/:103-105 | all resolve verbatim |
| 7 `css_l4_*/generated.rs` md5 identity (claim `b654562c`, P3) | all 7 = `b654562ccff46ed62dd48e9ace325830` — EXACT |
| `css_provider_source` live bench field (report.rs) vs PLANNED firewall predicate | live field present `:1168`; diff discloses "distinct from the live … of the same name" — coupling AVOIDED |

## Enumerated Staged Amendments / CRUD Operations Under CH4

### locks-diff.md (11-clause addendum, single hunk @@ -622,6 +622,33 @@)

1. **Clause set (a)-(d) gate / firewall / neutrality / aarch64-ONLY / verbatim-blob
   / green-by-exclusion / single-substrate / retarget / CollapsedStage-slot /
   cursor-re-anchor / Pattern-H** — ADD-only, 27 new-side lines, `git apply --check`
   exit 0, 16 locks + 5 shapes preserved, both co-gates written PLANNED. The single
   CRUD op (one addendum insert after `:622`, before `:625`) is bounded and the
   propagation cost is exactly one file. **ACCEPT.**

2. **aarch64-ONLY clause "P1 DELETION target" cost** — clause names the x86 surface
   a P1 deletion but cites no LOC in-clause; the LOC lands in the master-plan/
   migration P1 rows (verified below). In-clause this is governance, not a budgeted
   amendment, so the missing in-clause number is by-design. **ACCEPT.**

### master-plan-diff.md (6 staged diffs)

3. **Diff 1 — §13.6 re-key SK-V18→SK-V19** (header + preamble + receiver-row wave-IDs
   + MP-3B-SKV17 footers). Propagation enumerated: F1-F9 preserved verbatim; rename
   `MP.SK18.W0..W6`→`MP.SK19.W0..W6`. Bounded, label-only. **ACCEPT.**

4. **Diff 2 — NEW §13.7 GENERALIZATION 12-wave block.** Per-wave LOC budget + same-wave
   consumer + RED exit-gate falsifier per row — exactly the CH4 discipline. P1 −4500,
   P2 −700, P3 −5500, P4 +15, P5 0, G1-G6 ≤450 hand each, PROVE +200, H1 0. The
   campaign-LOC token `≈ −10800` is reconciled IN-LINE to the per-wave SPEC sum
   `≈ −10685` (the 115-LOC band disclosed honestly). **ACCEPT** on budget; the hunk
   line-count claim is a REVISE — see item 13.

5. **Diff 2 P1 row "del ≈−4500 / today 28 files"** — `find x86_64 + ext/x86 -type f`
   = **28** (24 src + 4 ext); actual LOC = **4346** (`wc -l` over .rs/.asm/.c). The
   "≈−4500" overstates the true 4346 by ~154. Within the "≈" band but loose. **ACCEPT**
   (file count exact; LOC within rounding).

6. **Diff 2 P2 row "del ≈−700 / today 64 = 48 nonjson + 16 gate.rs"** — `rg -c
   measure_mbps|lightningcss_facts` = **64** (48 `nonjson_css_l4.rs` + 16 `bin/gate.rs`).
   EXACT match including the gate.rs sub-split. **ACCEPT.**

7. **Diff 2 P3 row "collapse 7 css_l4 replicas + RuntimeTarget row-collapse / ≈−5500"**
   — disk shows **7** css_l4 dirs each 910 LOC, all md5-identical; the collapse keeps
   1 and deletes **6** (6×910 = 5460 ≈ −5500). The row text "collapse 7 css_l4
   replicas" is loosely worded (7 present, 6 deleted) where MIGRATION/SPEC correctly
   say "6 of 7 deleted". The LOC budget −5500 is honest; the "7 replicas" phrasing in
   the §13.7 receiver row should read "6-of-7" for parity with the MIGRATION row
   (`migration-delta.staged.md:56`) and SPEC:435. **REVISE** (master-plan-diff §2
   §13.7 P3 row: change "collapse 7 css_l4 replicas" → "collapse 6 of 7 byte-identical
   css_l4 replicas" to match the other two surfaces' deletion count).

8. **Diff 2 P5 row "≈0 (rename-only) / today 7"** — `parse_w11_1_number` resolves to
   **7 in `json/generated.rs` AND 7 in `json_sink_direct.rs` (template source)** = 14
   across the two functional files (+1 lib.rs reference). The exit-gate falsifier
   `grep -c parse_w11_1_number == 0` spans BOTH files, so the "today 7" undercounts
   the falsifier surface by half. The budget "rename-only at template source; 1:1
   regen" is correct (the rename at the template propagates to the generated file), so
   the COST is bounded — but the "today 7" census is a citation undercount. **REVISE**
   (master-plan-diff Diff 2 P5 row + migration-delta `:58`: state "today 14 = 7
   generated + 7 template-source `json_sink_direct.rs`" so the `== 0` falsifier scope
   is honest).

9. **Diff 3 — §25 Implementation Order** (SK-V15→V16→V17→V18→V19 monotonic). Label +
   sequence edit, no LOC code cost, bounded. **ACCEPT.**

10. **Diff 4 — §24 Carry Ledger re-key + 4 SK-V19 tee-up rows.** Each tee-up row
    carries its DEFER cost: (a) R16 PartialEq 9-row + regex widen ≈+217; (b)
    css_types.rs 66 LOC relocate-or-delete; (c) scanner asymmetry +8/9 OnceCell.
    Budgeted, DEFER-to-SK-V19, NOT bolted into SK-V18. **ACCEPT** — and this is the
    correct anti-Lock-14-narrowing move (the 9-name widen is explicitly NOT an SK-V18
    bolt-on, preserving the strict gate).

11. **Diff 5 — §5 F.W5 / §13.5 CSS verdict.** F.W5 annotated UN-FORK-unrealised;
    CSS verdict UPGRADE carries the directional caveat (loadavg 4.35, H1 re-lock
    pending) and explicitly forbids the un-caveated "MEASUREMENT-VALID" word. Prose,
    bounded, no over-claim. **ACCEPT.**

12. **Diff 6 — §13 H-row label alignment.** Propagation enumerated in-diff: "Total
    Diff-6 sites = 6" (H.W1:642, H.W4:646, Lock-10:616, §13 preamble:584-592 ×3).
    This is the model CH4 propagation disclosure. **ACCEPT.**

### architecture-delta.staged.md (2 gated hunks + 4 anchored splices)

13. **CRUD-1 per-finding LOC + propagation table** (the R9 fix). Six findings, each
    line-anchored + budgeted + site-counted; two gated hunks `git apply --check` exit
    0; four splices carry byte-exact re-grep-HALT anchors. This is the strongest CH4
    artefact. BUT the stated total "≈ +56 / −37 prose LOC … net ≈ +19": additions
    sum to +56 (✓), deletions to **−39** (18+10+3+8), net **+17** — the −37/+19 figures
    are a 2-LOC arithmetic slip. **REVISE** (`architecture-delta.staged.md:87` +
    `ΩA-coherence-audit.md:283`: "−37 … net ≈ +19" → "−39 … net ≈ +17"; or itemize the
    2-LOC residual).

14. **OA-V10-10 §7.4/§13.1 Lock-14 RED prose** — records skinny D11a +15 inline, tees
    D11b ≈+217 to SK-V19, explicitly "do NOT bolt the 9-name widen". The +15/+217 code
    budgets are flagged "owned by CRUD-3/SK-V19, NOT this ARCHITECTURE prose edit, and
    are not double-counted." Correct cost-ownership separation; no Lock-14 narrowing.
    **ACCEPT.**

### migration-delta.staged.md (4 OPs) + handoff-delta.staged.md (5 OPs)

15. **MIGRATION OP-1 12-wave receiver table** — per-wave Net-LOC column mirrors the
    SPEC manifest. P1 row says "`bbnf-simd/src/x86_64/` (24 files)"; master-plan-diff
    P1 says "today 28". The two surfaces scope the SAME P1 deletion differently:
    MIGRATION counts `src/x86_64` only (24), master-plan-diff counts `src/x86_64 +
    ext/x86` (28). Both are individually verifiable but read as a discrepancy to a
    cross-surface reader, and the verify grep `find …/x86_64 …/ext/x86 -type f == 0`
    spans BOTH trees (28), so the MIGRATION "(24 files)" parenthetical undercounts the
    actual deletion reach by 4 files. **REVISE** (migration-delta `:54`: "(24 files)"
    → "(24 src/x86_64 + 4 ext/x86 = 28 files)" to match the verify grep's reach and
    the master-plan-diff count).

16. **MIGRATION OP-1/OP-2 "9 `checkasm_parity.rs` x86_64 call sites DECOUPLED"** —
    `rg -c x86_64 skinny/crates/bbnf-simd/tests/checkasm_parity.rs` = **11** (all x86
    tokens = 11). The SAME-commit decouple touches 11 sites, not 9; the propagation
    cost of the P1 build-soundness coupling is undercounted by 2. **REVISE**
    (migration-delta `:54`,`:85` + master-plan-diff §13.7 P1 row + ΩD: "9 checkasm
    x86_64 call sites" → "11" so the P1 SAME-commit decouple reach is honest;
    re-grep before the CRUD merge as counts may drift).

17. **MIGRATION OP-2 css_types.rs RELOCATE-or-DELETE → SK-V19 (66 LOC)** — routed to
    SK-V19, cites Lock 14:349 verbatim, admits ONLY a `crates/css/` declaration crate
    (Lock 14(c)) else delete. Budgeted, deferred, NOT a silent drop, NOT a Lock-14
    relaxation. **ACCEPT.**

18. **MIGRATION OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 REDRESS block** — abuts
    REDRESS 51/53/246/247 (verified REJECTED, item 246 bounds G4 structural-stream
    route). NOT a revived route — it is a fence AGAINST revival. **ACCEPT.**

19. **HANDOFF OP-1..OP-5** — current-override insert + stale-SK-V18-adopt strike +
    dispatch-directive re-root + 10-row blocker matrix + next-cycle directive. Each
    blocker row maps to a receiver wave + measurable gate; the SK-V19 tee-up carriers
    (a)-(e) are each cited, none dropped. No LOC code cost (HANDOFF is prose authority).
    Bounded. **ACCEPT.**

### ΩD / ΩC / ΩA (consuming docs)

20. **ΩD §8 "the Diff 2 hunk is 67 added lines … no 4-7× expansion"** — the §13.7 Diff 2
    hunk (`+### §13.7 …` through its closing fence) is **76 added `+` lines**, not 67.
    The claim's PURPOSE (refute a 4-7× doc expansion) still holds — 76 doc lines for a
    12-row wave table + 5 gate bullets + 4 per-D paragraphs is ~1 doc line per source
    line, no expansion. But the literal "67" is a count error. **REVISE**
    (`master-plan-diff.md:356` + ΩD §8 cross-ref: "67 added lines" → "76 added lines";
    the no-expansion conclusion is unaffected).

21. **ΩC disposition 9A/11M/0R/1D + "6 of 11 clauses changed; md5 differs"** — the
    consolidation-of-3C claim; `git apply --check` exit 0 (re-verified), 11-clause
    disposition preserved, DEFER folded as a one-line audit-scope note (not dropped).
    Bounded, no clause added/dropped/reversed. **ACCEPT.**

22. **ΩA harmonization (the −10800/−10685 band)** — ΩA `:221` flags the figure must be
    cited as "≈ −10800 campaign (per-wave SPEC sum ≈ −10685)" so the two never read as
    a contradiction; every downstream artefact (ΩD, MIGRATION, HANDOFF, master-plan-diff)
    DOES carry the paired form. This is the correct CH4 cost-harmonization. **ACCEPT.**

## Findings Summary (anti-patterns checked, none found)

- **Non-applying diff:** NONE — both `git apply --check` gates exit 0 at HEAD.
- **Revived REDRESS route:** NONE — 51/53/246/247 are FENCED, not revived; the
  admissible-vs-rejected distinction (G4 Cursor = VIEW over existing tape; rejected =
  second-substrate driver) is sound.
- **Lock-14 narrowing:** NONE — the 9-name regex widen + R16 PartialEq full-row collapse
  are explicitly DEFERRED to SK-V19, NOT bolted into SK-V18; the strict gate is preserved.
- **Coupling:** NONE — `css_provider_source` PLANNED predicate is honestly disclosed as
  distinct from the live `report.rs:1168` bench field of the same name.
- **Uncited claim:** NONE under cost lens — every LOC budget traces to SPEC:431-447/:571
  or a disk census I reproduced.

The REVISE items (7, 8, 13, 15, 16, 20) are all COUNT / SCOPE-PRECISION errors in
otherwise-budgeted-and-bounded amendments: a "7 vs 6-of-7" replica phrasing, a "7 vs
14" leak-census undercount, a "−37 vs −39" arithmetic slip, a "24 vs 28" file-scope
mismatch, a "9 vs 11" checkasm-site undercount, and a "67 vs 76" hunk-line count. None
revive a refuted route or narrow a lock; each is a one-token correction to the named
artefact so the propagation cost reads honestly across surfaces. The cost discipline
itself (per-wave LOC + same-wave consumer + RED falsifier + propagation-site tables) is
present and exemplary — the staged amendments are realistic and bounded; the campaign is
a genuine REDUCTION with no `[generated-size-budget]` overflow.

TALLY accept=16 revise=6 reject=0
