# Pass Omega V10 CHALLENGE — CH1 CORRECTNESS — Cycle V1

Lens: CH1 CORRECTNESS. Does every cited file:line/SHA resolve; does every REDRESS
reference match content; does the staged `locks-diff` apply cleanly
(`git apply --check` exit 0) to live `LOCKS.md`; does the `master-plan-diff` cite
real §H waves + real SHAs.

Scope: the 6 Ω artefacts (ΩA-ΩF) + the staged diffs under
`restart/audit/totality/astral/V10/` (`locks-diff.md`, `master-plan-diff.md`,
`ΩE-skinny-corpus-staged-diff.md`, `handoff-delta.staged.md`,
`migration-delta.staged.md`) against the live V1 surfaces and the converged
T-P1/T-P2/T-P3 evidence.

Verdict: **REVISE REQUIRED.** The primary CH1 gates PASS — the staged
`locks-diff` applies cleanly (`git apply --check` exit 0), every spot-checked
SHA resolves, every REDRESS reference matches content, the `master-plan-diff`
cites real §H waves at the live file:line. But three citation-correctness defects
recur across the cross-document corpus (an off-by-one LOCKS line, a stale
"Pass-Omega-V6" label embedded in a staged-for-MERGE diff body, and an
inconsistent REDRESS item-set). Cycle-V1 ≥30% REVISE is met (5 of 14 = 36%).

## Primary CH1 Gate Results (spot-verified at HEAD `25297a7fc`)

| Gate | Command | Result |
|---|---|---|
| Staged locks-diff applies cleanly | `awk … \| git apply --check -` | **EXIT 0 (CLEAN)** — re-run twice, stable |
| Hunk math internally consistent | `@@ -622,6 +622,33 @@`; 6 context + 27 added = 33 | **CONSISTENT** (28 `+` lines − 1 `+++` header = 27) |
| 16 numbered locks preserved | `grep -nE '^[0-9]+\. \*\*' LOCKS.md \| head -16` | **16** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`; addendum adds no Lock 17 |
| Five BackendShape variants, no sixth | `lower/mod.rs:20-24` + `cost.rs:334 [BackendShape; 5]` | **5** `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; `lib.rs:341-345` enum confirms |
| Two PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed`; `bbnf_simd_single_mask_convention` | **0 / 0** — both PLANNED, not live |
| Insertion anchor resolves | Lock-16 NEON clause `:622`; `## v+1 Governance Boundary` `:625` | **EXACT** — addendum lands `:622`→`:625`, leaves SK-V15/SK-V17 addenda untouched |

## §H Wave + SHA Resolution (master-plan-diff)

ALL spot-checked SHAs resolve to real commits: `25297a7fc` (= live HEAD, staging
HEAD claim correct), `66232b7c3` (SK-V15 W11 close), `1c5bd7a25` (SK-V16 W6-tape),
`f6a38445b` (SK-V17 W4/W5 close), `6fb812752`/`3f6eb603d` (T-P1/T-P2),
`9b52e162d`/`784ceb418`/`820798161`/`4e4aa0648` (S-P1/S-P2/S-P3),
`83b66db42`/`0fbee121f` (alpha/S-P0), `c64148ef2`/`232479e4d`/`ea8138056`/`6bb4b2a6c`
(SK-V16/V17 evidence). ALL §H wave line-anchors resolve EXACTLY:
- §13.6 header `:974` = live "SK-V18 Tape-Fold Adoption Receiver Block" (re-key old-side byte-exact)
- §13.6 preamble `:976-990` old-side = live text byte-exact
- §14 Tranche I `:1042` (insert anchor for §13.7) — exact
- §25 footer `:1415-1422` old-side ("MP.SK18.W0) dispatch to ADOPT the proven substrate" at `:1419`) — exact
- F.W5 "Nine seed grammars build through new template" `:519` — exact

## REDRESS References (all match content)

- locks-diff CollapsedStage clause `skinny/REDRESS.md:2795-2944` (finding `:2928-2933`):
  `:2795` = "SK-V9 Wave 3 Union Event-Model Class-Column Redress"; the M5-Max
  scalar-cheaper-than-SIMD-cursor finding is at `:2928-2933`. **MATCH.**
- "REDRESS 96/97/98 RETIRED": Item 98 retires `G-W3-UNION-SUBSTRATE` at `:2910`. **MATCH.**
- ΩB pre-block: item 246 `:6184` (W11T Parse-Only Structural Stream Reject),
  247 `:6230` (W11V String64 Reject), 51 `:742` (SK-V5 event-cursor REJECTED),
  53 `:784` (SK-V5 structural-mask cursor REJECTED). **ALL MATCH the described shapes.**

## Live-Surface Evidence Spot-Checks (ΩA / ΩB / ΩF)

All resolve at the live file:line: ARCH §0 `:19` = "SK-V15 current authority";
ARCH §9.2 `:1998` = "type parameter is the generality vehicle" (the `G:EventGrammar`
antecedent wraps from `:1997`); ARCH §7.4 `:1371` = "SK-V5 Through SK-V15"; ARCH
`RuntimeEmitterKind` count = 0 (present in skinny `runtime_generator.rs:1,17,25`);
`crates/core/src/css_types.rs` present (2373 B); `strategy.rs:137-155` 9-ident
table; `runtime_generator.rs:16` = `match request.profile_contract.emitter`,
`:91` = `normalize(CSS_GENERATED_RS)` call, `:701` = `const CSS_GENERATED_RS: &str`;
7 css_l4 `generated.rs` replicas present; HANDOFF `:16-19`/`:3-5`/`:47`/`:103-105`
+ MIGRATION `:30`/`:190`/`:886`/`:925` + SPEC `:19-21`/`:46-49`/`:435` — ALL exact.

## Enumerated Staged Amendments / CRUD Operations Under CH1

| # | Artefact | Staged amendment / CRUD op | Verdict |
|---|---|---|---|
| 1 | locks-diff (ΩC) | The 11-clause SK-V18 T-P3 v+1 Crystallisation Addendum, inserted `:622`→`:625`; `git apply --check` EXIT 0; hunk math 6+27=33 consistent; 16 locks / 5 shapes preserved; 2 PLANNED co-gates absent | **ACCEPT** |
| 2 | ΩC-locks-amendments | 9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER disposition matrix; byte-identical to converged 3C-locks-v+1-diff; all verification commands resolve | **ACCEPT** |
| 3 | master-plan-diff Diff 1 | Re-key §13.6 SK-V18 Tape-Fold → SK-V19 Totality-Fold; header `:974` + preamble `:976-990` old-side byte-exact against live | **ACCEPT** |
| 4 | master-plan-diff Diff 2 | NEW §13.7 SK-V18 GENERALIZATION 12-wave block, insert before §14 `:1042`; wave manifest matches SPEC `:431-447`; net ≈−10800 LOC reduction. **DEFECT: line `:192` of the `+`-prefixed (merge-bound) block reads "Pass-Omega-V6 / pre-W-PRUNE blocker"** — the stale-V6 label ΩA-OA-V10-03 and ΩF `:52-57` resolve to V10; this V6 will merge VERBATIM into live MASTER-PLAN §13.7 and collide with the historical V6 W5BR receiver (`MIGRATION.md:190`). ΩF harmonized its own deltas V6→V10; Ω-D did not. **Also** `:192` cites the 3-item REDRESS set "51/53/247" where the complete set is the 4-item "51,53,246,247" (ΩF `:275`, ΩB pre-block; 246 = the G4 structural-stream driver). | **REVISE** — Ω-D author: in Diff 2's §13.7 `+` block replace "Pass-Omega-V6" with "Pass-Omega-V10", and "REDRESS items 51/53/247" with "REDRESS items 51/53/246/247" (or scope the 3-item set explicitly to G2/G6 and name 246 for G4). |
| 5 | master-plan-diff Diff 3 | §25 implementation-order re-key; old-side `:1415-1422` resolves; monotonic skinny→totality restored | **ACCEPT** |
| 6 | master-plan-diff Diff 4 | §24 carry-ledger re-key + 4 SK-V19 tee-up rows; old-side `:1349-1352` row re-key; no shape/lock change | **ACCEPT** |
| 7 | master-plan-diff Diff 5/6 | §5 F.W5 / §13.5 CSS verdict reconcile + §13 H-row label alignment; F.W5 `:519` + §13.6/§13.5 anchors resolve | **ACCEPT** |
| 8 | ΩA-coherence | 12 findings + 12 cohesion fixes; every cited evidence anchor resolves at HEAD; 5-shape canon PASS; correctly flags the stale-V6 (OA-V10-03) as a citation-hygiene REVISE | **ACCEPT** (correctly self-identifies the V6 defect; its own citations clean) |
| 9 | ΩB-skinny-lessons | SK-V1..V18 trajectory digest; `runtime_generator.rs:16,91`, 7-replica, REDRESS 246/247/51/53 all resolve. **DEFECT: `:89` and `:204` call the SK-V16/V17 reconcile "a Pass-Omega-V6 / pre-W-PRUNE blocker"** — the same stale-V6 label ΩA-OA-V10-03 / ΩF reject; ΩB is a digest (not merge-bound), but it feeds the ARCH implementation-status CRUD. | **REVISE** — ΩB author: replace both "Pass-Omega-V6" occurrences with "Pass-Omega-V10" to match the corpus harmonization. |
| 10 | ΩE-skinny-corpus-diff | 6-surface prose-replacement diff (INDEX/WORKSPACE/HARDENING/COMPILER/BENCH/SUBSTRATE); line anchors at `83b66db42` with an explicit CRUD-5 re-grep instruction. Spot-checked anchors resolve: INDEX `:5`/`:38`, COMPILER `:54`/`:116`, BENCH `:73`. **Caveat (not a defect): anchors are at `83b66db42`, not HEAD `25297a7fc`** — but the diff carries first-line anchor strings + an explicit re-grep mandate (`:388`), so drift is self-mitigating; it is a prose CRUD, not a `git apply` target. | **ACCEPT** (anchors carry self-correcting re-grep; verified-resolvable) |
| 11 | ΩF-migration-handoff | Pass-label reconcile (V6→V10) + 5 migration decisions + COH18-001 cross-check table. **DEFECT: cites `restart/locks/LOCKS.md:621` for "The `G:EventGrammar` type parameter is the generality vehicle" (`:108`, cross-check table `:173`); the text is on line `620`** (verified — single long line; locks-diff cursor-generality clause correctly cites `:620`). | **REVISE** — ΩF author: correct `LOCKS.md:621`→`:620` in the strike-target citation (this is the line the CRUD-3/SK-V19 strike will edit; an off-by-one mis-targets it). |
| 12 | handoff-delta.staged | OP-1..OP-5 HANDOFF deltas; anchors `:3`, `:16-19`, `:90-110`, `:103-105` resolve; 502-line claim correct; V6→V10 harmonized in the body. **DEFECT: `:169` + `:192` repeat the `LOCKS.md:621` off-by-one** (should be `:620`). | **REVISE** — handoff-delta author: correct both `LOCKS:621`→`LOCKS:620` (OP-5 step 5 + the SK-V19 tee-up carrier (e)). |
| 13 | migration-delta.staged | OP-1..OP-4 MIGRATION deltas; anchors §17 `:886`, §19 `:925`, §0.0 `:30`, SPEC `:435`/`:19-21`/`:46-49` resolve; 12-wave + 5-decision tables; V6→V10 harmonized. **DEFECT: Sources header `:10` + OP-2 row `:86` repeat the `LOCKS.md:621` off-by-one** (the phantom-axis strike-target; should be `:620`). | **REVISE** — migration-delta author: correct both `LOCKS.md:621`→`:620` (Sources + the phantom `<G>` disposition row's companion-reconcile citation). |
| 14 | ΩF/ΩB/migration/master-plan REDRESS item-set | The G2/G4/G6 abutment prose cites the 3-item set "51/53/247" (ΩF `:162`, migration-delta `:114`, master-plan-diff `:192`) while ΩF CH3 `:275` + ΩB pre-block cite the complete 4-item set "51,53,246,247" (246 = G4's structural-stream driver). Self-inconsistent on which items the waves abut. | **REVISE** — harmonize the abutment prose to the 4-item set, or scope the 3-item list to G2/G6 and name 246 for G4 explicitly. (Folded into #4 for the master-plan-diff occurrence; #11/#13 carry the ΩF/migration occurrences.) |

## REJECT Scan (none triggered)

No CH1 REJECT condition fires: the staged diff APPLIES (`git apply --check` exit
0), no REDRESS route is revived (the §13.7 gates fence AZ-IV eager / StructRegistry
per-leaf / fact-stream / x86 / second-substrate; CH3 blocks G2/G4/G6 until the
SK-V16/V17 reconcile commits), Lock-14 is NOT narrowed (amendment by ADDITION; the
green-by-exclusion clause STRENGTHENS the gate; `FORBIDDEN_GENERIC_TOKENS` written
byte-identical across 3A-D11/3C/3B-P4/3D-D04/v+1), no coupling is introduced (the
un-fork reads `BackendShape` from the lowered program, the relocated-seam firewall
+ `runtime_target_rows_collapsed` co-gate are NECESSARY-NOT-SUFFICIENT-aware), and
no claim is uncited (every clause carries a resolving Evidence: anchor).

## Conclusion

CH1's load-bearing gates PASS: the locks-diff applies cleanly, SHAs resolve, §H
waves resolve, REDRESS references match. The REVISE set is entirely
citation-correctness hygiene — one off-by-one LOCKS line (`:621`→`:620`) that
recurs in ΩF + both staged deltas and mis-targets the exact line a future strike
edits; one stale "Pass-Omega-V6" label embedded in the MERGE-BOUND §13.7
master-plan diff body (plus a digest occurrence in ΩB) that ΩA/ΩF already resolve
to V10 elsewhere; and a 3-item-vs-4-item REDRESS abutment-set inconsistency. None
blocks G-Omega on a structural ground, but all three must be corrected BEFORE the
CRUD merge so no off-by-one strike, no false-current V6 collision, and no
incomplete pre-block reaches a live V1 surface.

TALLY accept=8 revise=5 reject=0
