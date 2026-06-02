# Pass Omega V10 CHALLENGE — CH1 CORRECTNESS — Cycle V2

Lens: CH1 CORRECTNESS. Does every cited file:line/SHA resolve; does every REDRESS
reference match content; does the staged `locks-diff` apply cleanly
(`git apply --check` exit 0) to live `LOCKS.md`; does the `master-plan-diff` cite
real §H waves + real SHAs.

Scope: the 6 Ω artefacts (ΩA-ΩF) + the staged diffs under
`restart/audit/totality/astral/V10/` (`locks-diff.md`, `master-plan-diff.md`,
`ΩE-skinny-corpus-staged-diff.md`, `handoff-delta.staged.md`,
`migration-delta.staged.md`) against the live V1 surfaces and the converged
T-P1/T-P2/T-P3 evidence. HEAD at audit = `25297a7fc` (git status snapshot was
stale; live HEAD verified by `git rev-parse`).

Verdict: **REVISE REQUIRED.** Every primary CH1 gate PASS — independently
re-verified, not parroted from cycle-V1. The cross-document corpus has CONVERGED
on the entire cycle-V1 REVISE set (the "Pass-Omega-V6" labels, the `LOCKS:621`
off-by-one, the 3-item-vs-4-item REDRESS abutment set are ALL redressed at this
snapshot — zero residue). But the V2 fresh adversarial pass surfaces TWO
citation-correctness defects cycle-V1 GRADED ACCEPT and missed: a FALSE
"byte-identical" provenance claim in ΩC (the staged locks-diff is a hardened
SUPERSET of 3C, not byte-identical), and an off-by-3 line-range mis-citation in
the merge-bound master-plan-diff Diff 4 (the §24 carry-row is at `:1346`, cited
as `:1349-1352`, which points at four unrelated rows). Cycle-V1 ≥30% REVISE is
met (5 of 14 = 36%).

## Primary CH1 Gate Results (independently re-run at HEAD `25297a7fc`)

| Gate | Command | Result |
|---|---|---|
| Staged locks-diff applies cleanly | `awk '/^```diff$/{flag=1;next}/^```$/{flag=0}flag' locks-diff.md \| git apply --check -` | **EXIT 0 (CLEAN)** — re-run, stable |
| 16 numbered locks preserved | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` | **16** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`; addendum adds no Lock 17 |
| Five BackendShape variants, no sixth | `lower/mod.rs:20-24` + `cost.rs:334 [BackendShape; 5]` | **5** `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` |
| Two PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed`; `bbnf_simd_single_mask_convention` | **0 / 0** — both PLANNED, not live |
| Insertion anchor resolves | Lock-16 NEON clause `:622`; `## v+1 Governance Boundary` `:625` | **EXACT** — addendum lands `:622`→`:625`, leaves SK-V15/SK-V17 addenda untouched (`:581-607`/`:610-622`) |
| Hunk math internally consistent | `@@ -622,6 +622,33 @@`; 6 context + 27 added = 33 | **CONSISTENT** |

## §H Wave + SHA Resolution (master-plan-diff) — all spot-checks resolve EXACTLY

§H wave anchors, re-resolved against live `restart/MASTER-PLAN.md`:
- §13.6 header `:974` = live "SK-V18 Tape-Fold Adoption Receiver Block …" (old-side byte-exact). **EXACT**
- §14 Tranche I `:1042` = "## 14. Tranche I - Recovery, Incremental, LSP" (§13.7 insert anchor). **EXACT**
- §25 footer `:1419` = "MP.SK18.W0) dispatch to ADOPT the proven substrate into crates/core" (within cited `:1415-1422`). **EXACT**
- F.W5 `:519` = "| F.W5 | Current nine grammar regeneration. | Nine seed grammars build through new template. |". **EXACT**
- MP.NW6 `:662` (Lock-14 negative-control standard cited in locks-diff neutrality clause). **EXACT**
- H.W4.LOCK14 `:605` (PARTIAL single-negative-control row cited in locks-diff). **EXACT**

SHAs: ALL 20 spot-checked resolve to real commits with matching descriptions —
`25297a7fc` (=live HEAD, T-P3), `66232b7c3` (SK-V15 W11), `1c5bd7a25` (SK-V16
W6-tape), `f6a38445b` (SK-V17 W4/W5 close), `6fb812752`/`3f6eb603d` (T-P1/T-P2),
`9b52e162d`/`784ceb418`/`820798161`/`4e4aa0648` (S-P1/S-P2/S-P3),
`83b66db42`/`0fbee121f` (alpha/S-P0), `c64148ef2`/`232479e4d`/`ea8138056`/`6bb4b2a6c`
(SK-V16/V17 evidence), plus ΩA's `33b51d8f4`/`2a76916ac`/`7157be073`/`91b6893b0`/
`139ab1e4a`/`85a043224`.

## REDRESS References — all resolve, all match content

- locks-diff CollapsedStage clause `skinny/REDRESS.md:2795` = "SK-V9 Wave 3 Union
  Event-Model Class-Column Redress"; finding `:2928-2933` = the M5-Max
  scalar-cheaper-than-SIMD-cursor finding. **MATCH.**
- REDRESS pre-block items: `:742` = item 51 "SK-V5 event-cursor … REJECTED";
  `:784` = item 53 "SK-V5 structural-mask … REJECTED"; `:6184` = item 246 "SK-V14
  W11T Parse-Only Structural Stream Reject"; `:6446` = "SK-V15 W11 Close" (ledger
  end). **ALL MATCH.**
- 3B `:177` = "MP.SK19.SCANNER-UNIFY `simd-scan` probe-API reconcile … renamed/
  parallel-scanner risk is ACTIVE" (cited by the NEW V10 single-substrate clause).
  **MATCH** (this anchor is in V10 but NOT in 3C — see DEFECT-V2-01).

## Cycle-V1 REVISE Set — independently re-checked, ALL REDRESSED

The cycle-V1 CH1 verdict logged 5 REVISE items. At THIS snapshot every one is
corrected — I re-ran each, not assumed:
- **"Pass-Omega-V6" labels** (V1 #4/#8/#9, master-plan-diff `:192`, ΩB `:89`/`:204`):
  `grep -rinE 'omega.?v6|v6.?blocker' V10/*.md *.staged.md` returns ONLY
  "Pass-Omega-V10" + the honest historical-V6 references (ΩF `:49`/`:54`
  collision-rationale; migration-delta `:152` "no occurrence labels the CURRENT
  pass"). The merge-bound master-plan-diff `:200` now reads "Pass-Omega-V10 /
  pre-W-PRUNE blocker". **REDRESSED.**
- **`LOCKS:621` off-by-one** (V1 #11/#12/#13, ΩF + both staged deltas): zero
  `:621` references remain; ALL now cite `LOCKS.md:620` — the correct line ("The
  `G:EventGrammar` type parameter is the generality vehicle" verified on `:620`,
  `:621` is blank). **REDRESSED.**
- **3-item-vs-4-item REDRESS set** (V1 #14): every occurrence now uniformly cites
  "51/53/246/247" (master-plan-diff `:201`/`:335`, ΩF `:163`/`:278`, ΩD `:83`,
  migration-delta `:116`). **REDRESSED.**

## Live-Surface Evidence Spot-Checks (independently resolved)

- LOCKS: `:620` generality-vehicle phrase confirmed; `:107-108` 5-shape canon
  (ΩF `:177`); `:71` "Gestalt — sixteen locks"; `:622`/`:625` insertion anchors.
- ARCH: `:19` "SK-V15 current authority"; `:1371` "SK-V5 Through SK-V15
  Implementation Status"; `:1186` CollapsedStage x86 row; `:1998`
  "type parameter is the generality vehicle".
- HANDOFF (502 lines): `:3` "Current Totality Override - 2026-05-30"; `:16-19`
  stale SK-V18-adopt paragraph (strike-target byte-matches OP-2); `:47`
  "Historical Pass Omega V2..V9 packets"; `:90` "Pass Omega V5 SK-V17 …
  directive"; `:103-105` "dispatch **SK-V18 W0** (the `crates/core` tape-fold)".
- MIGRATION (1061 lines): `:30` "## 0.0 Current SK-V17 Tape-Fold Migration
  Receiver"; `:190` "## 0.6 Historical Pass Omega V6 W5BR Migration Receiver"
  (confirms the V6-collision rationale is REAL); `:886` "## 17. Tranche-Level
  Migration Sequence"; `:925` "## 19. Migration Gates".
- sk-v18/SPEC.md: `:19-21` "SK-V18 is the GENERALIZATION cycle"; `:46-49`
  "W-PRUNE (P1–P5) is the ONLY dispatch-now-eligible cluster"; `:435` P3 "≈ −5500
  (6×910 replica bodies deleted …)"; `:571` "≈ −10800."; `:431-447` 12-wave
  manifest; `:711-712` FORBIDDEN_GENERIC_TOKENS.
- skinny corpus: BENCH.md (2283 lines) `:32-34`/`:73`/`:341-343`/`:2268` carry
  the `cssparser`/`lightningcss` diagnostic-not-floor comparator text ΩE flags as
  the stale-vs-current inversion; INDEX.md `:5` "Pass Omega V9 / SK-V15 authority
  update"; COMPILER.md `:54` 5-shape canon.
- COH IDs: COH18-001 (HANDOFF-scope-drift), COH18-014 (A.W2/F.W5 nine-seed),
  COH18-015 (scanner-crate asymmetry) all resolve in `1F-coherence-scan.md`;
  3D-D08 ("substrate-sidecar-lock") in `3D-skinny-fold.md:20`; 3F-MH-003 in
  `3F-migration-handoff.md:14`/`:24`. 1E LAC-1E-V5-01..07 at `:147-153`.

## Enumerated Staged Amendments / CRUD Operations Under CH1

| # | Artefact | Staged amendment / CRUD op | Verdict |
|---|---|---|---|
| 1 | locks-diff (ΩC) | The 11-clause SK-V18 T-P3 v+1 Crystallisation Addendum, inserted `:622`→`:625`; `git apply --check` EXIT 0; 16 locks / 5 shapes preserved; 2 PLANNED co-gates absent; every Evidence anchor (SPEC §6 `:358-390`, 1E `:147-153`, REDRESS `:2795`/`:2928-2933`, 3B `:177`, MP.NW6 `:662`/H.W4.LOCK14 `:605`) resolves; `LOCKS:620` cited correctly. | **ACCEPT** |
| 2 | ΩC-locks-amendments | Disposition matrix (9 ACCEPT/11 MODIFY/0 REJECT/1 DEFER) resolves against `3C-locks-crystallisation.md`; all verification commands resolve. **DEFECT (V1 missed): `:19` + `:123-124` assert the staged diff body is "byte-identical" to `3C-locks-v+1-diff.md`. It is NOT — `diff` of the two extracted diff bodies returns 6 differing clauses (relocated-seam firewall, neutrality-proof, single-substrate, CollapsedStage, cursor-generality, Pattern-H), 12 changed lines. The V10 versions are HARDENED supersets (they add the present-tense "UNBUILT at HEAD" honesty, the `8-of-9 not re-verified` caveat, the `MP-3B-SKV18-D07`/3B `:177` ownership pointer, the UNKNOWN-2D-05 discharge). The content is GOOD and well-cited; the PROVENANCE CLAIM is false.** This is a CH1 reference-mismatch: a cited relation ("byte-identical consolidation") contradicted by the file content. | **REVISE** — ΩC author: at `:19` and `:123-124` replace "byte-identical consolidation of the converged `3C-locks-v+1-diff.md`" with "hardened consolidation of the converged `3C-locks-v+1-diff.md` (six clauses expanded with present-tense-honesty + ownership-pointer refinements; no clause dropped, added, or REVERSED)". (locks-diff.md `:5` already correctly says "consolidation", not "byte-identical" — only ΩC over-claims.) |
| 3 | master-plan-diff Diff 1 | Re-key §13.6 SK-V18 Tape-Fold → SK-V19 Totality-Fold; header `:974` + §25 `:1419` + preamble old-side resolve byte-exact; F1-F9 preserved verbatim; MP.SK18.W*→MP.SK19.W* rename. | **ACCEPT** |
| 4 | master-plan-diff Diff 2 | NEW §13.7 SK-V18 GENERALIZATION 12-wave block, insert before §14 `:1042`; wave manifest matches SPEC `:431-447`; net ≈−10800 LOC reduction; `:200` "Pass-Omega-V10" + `:201` 4-item REDRESS set BOTH now correct (cycle-V1 defects redressed). All MP-3B-SKV18-D0x deltas resolve. | **ACCEPT** |
| 5 | master-plan-diff Diff 3 | §25 implementation-order re-key; old-side `:1415-1422` resolves (`:1419` ADOPT line exact); monotonic skinny→totality restored. | **ACCEPT** |
| 6 | master-plan-diff Diff 4 | §24 carry-ledger re-key + 4 SK-V19 tee-up rows. **DEFECT (V1 #6 graded ACCEPT with the same wrong range): `:244` cites the old-side "SK-V18 tape-fold adoption (MP.SK18.W0..W6)" carry-row at `(:1349-1352)`. The live row is a SINGLE line at `:1346`; lines `:1349-1352` point at four UNRELATED rows ("Declaration-crate escape valve / Layout lowering / Cursor skip / PASS-3 consumers"). The diff's quoted old-side text (`:250`) IS byte-exact to live `:1346`, so the hunk would still locate by content, but the cited line-range mis-targets the strike by ~3 lines in a MERGE-BOUND diff.** | **REVISE** — Ω-D author: in Diff 4 (`:244`) correct the old-side citation `(:1349-1352)` → `(:1346)` (the row is a single line; the §24 section header is `:1336`, the carry row is `:1346`). |
| 7 | master-plan-diff Diff 5/6 | §5 F.W5 / §13.5 CSS verdict reconcile + §13 H-row label alignment; F.W5 `:519` + §13.6/§13.5/H.W4 `:605` anchors resolve; label-only edits. | **ACCEPT** |
| 8 | ΩA-coherence | 12 findings + 12 cohesion fixes; every cited ARCH/LOCKS/SHA anchor independently resolved (`:19`,`:1371`,`:1186`,`:1998`; SHA list `:57` all resolve; 5-shape canon byte-identical across ARCH/MASTER/COMPILER/LOCKS); correctly self-identifies OA-V10-03 (the now-redressed V6 drift) + the net-LOC harmonization CF-11; its own citations are clean. | **ACCEPT** |
| 9 | ΩB-skinny-lessons | SK-V1..V18 trajectory digest; `runtime_generator.rs`, 7-replica, REDRESS 246/247/51/53 resolve; `:89`/`:204` now read "Pass-Omega-V10" (cycle-V1 V6 defect redressed). | **ACCEPT** |
| 10 | ΩE-skinny-corpus + staged diff | 6-surface prose-resync (INDEX/WORKSPACE/HARDENING/COMPILER/BENCH/SUBSTRATE); BENCH.md (2283 ln) inversion anchors `:32-34`/`:73`/`:341-343`/`:2268` resolve; INDEX `:5`/COMPILER `:54` resolve; CRUD-5 prose target carrying first-line anchors + re-grep mandate (NOT a `git apply` target), so the `83b66db42`-vs-HEAD anchoring is self-mitigating. | **ACCEPT** |
| 11 | ΩF-migration-handoff | Pass-label reconcile (V6→V10, well-grounded: MIGRATION `:190` historical-V6 collision is REAL) + 5 migration decisions + COH18-001 cross-check table; `LOCKS:620` cited correctly throughout (`:109`,`:176`,`:218`,`:247`); `:177` 16-lock/5-shape canon at `LOCKS:107-108` resolves; 4-item REDRESS set; HANDOFF/MIGRATION/SPEC anchors all resolve. | **ACCEPT** |
| 12 | handoff-delta.staged | OP-1..OP-5 HANDOFF deltas; live anchors `:3`/`:16-19`/`:47`/`:90`/`:103-105` resolve byte-exact; 502-line claim correct; `LOCKS:620` cited correctly (`:170`,`:193`); V6→V10 harmonized; 10-row blocker matrix each with measurable gate. | **ACCEPT** |
| 13 | migration-delta.staged | OP-1..OP-4 MIGRATION deltas; live anchors §0.0 `:30`, §0.6-V6 `:190`, §17 `:886`, §19 `:925` resolve byte-exact; SPEC `:435`/`:46-49`/`:571` resolve; `LOCKS:620` cited correctly (`:88`,`:9`); 4-item REDRESS set (`:116`); V6→V10 harmonized. **Cosmetic-only (not REVISE): `:5` header "502/1061-line surfaces" conflates HANDOFF's 502 with MIGRATION's 1061; both numbers individually correct, MIGRATION is 1061.** | **ACCEPT** |
| 14 | ΩD-master-plan-reconciliation | 10 SKV18 + 4 carried delta dispositions; `MP-3B-SKV18-D01..D10` map to MASTER surfaces; COH18-001/014, 3D-D08, 3F-MH-003 resolve; `:83` 4-item REDRESS set + "item 246 bounds G4" correct; CH-lens residuals consistent with the staged diffs. (ΩD `:40` cites COH18-014 and master-plan-diff `:47` cites COH18-001 for the same pivot — both are valid facets, NOT an inconsistency.) | **ACCEPT** |

## REJECT Scan (none triggered)

No CH1 REJECT condition fires:
- The staged locks-diff APPLIES (`git apply --check` exit 0, re-run stable).
- No REDRESS route is revived: the §13.7 gates fence AZ-IV eager / StructRegistry
  per-leaf / fact-stream / x86 / second-substrate; CH3-V1-R2 blocks G2/G4/G6 until
  the SK-V16/V17 reconcile commits; the CollapsedStage clause clears (does not
  re-open) the M5 scalar-cheaper REDRESS 96/97/98-RETIRED prior.
- Lock-14 is NOT narrowed — amendment by ADDITION; the green-by-exclusion clause
  STRENGTHENS the gate; `FORBIDDEN_GENERIC_TOKENS` is written byte-identically
  across 3A-D11/3C/3B-P4/3D-D04/v+1 (the ONE token-set the corpus claims byte
  identity for, and that claim is TRUE — distinct from the ΩC false byte-identity
  claim about the whole diff body).
- No coupling introduced — the un-fork reads `BackendShape` from the lowered
  program; the relocated-seam firewall + `runtime_target_rows_collapsed` co-gate
  are NECESSARY-NOT-SUFFICIENT-aware.
- No uncited claim — every load-bearing clause carries a resolving Evidence anchor.

## Conclusion

CH1's load-bearing gates PASS, independently re-run: the locks-diff applies
cleanly, all SHAs resolve, all §H waves resolve, all REDRESS references match.
The corpus has CONVERGED on the entire cycle-V1 REVISE set (V6 labels, the
`LOCKS:621` off-by-one, the 3/4-item REDRESS abutment) — verified by direct
re-grep, zero residue. The V2 REVISE set is the fresh adversarial yield cycle-V1
graded ACCEPT and missed: (a) ΩC's FALSE "byte-identical to 3C" provenance claim
— the staged locks-diff is a hardened SUPERSET (6 clauses expanded, none
reversed), so the diff is SOUND but the label lies; and (b) the merge-bound
master-plan-diff Diff 4 §24 carry-row line-range, cited `:1349-1352` but living
at `:1346` (the cited range points at four unrelated rows). Neither blocks
G-Omega on a structural ground — the locks-diff still applies, the §24 hunk still
locates by its byte-exact old-side text — but both must be corrected BEFORE the
CRUD merge so no false provenance label and no mis-targeted strike-range reaches
a live V1 surface.

TALLY accept=12 revise=2 reject=0
