---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis
cycle: V1
subject: SK-V17 T-P3 synthesis artefacts
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
artefacts_reviewed:
  - restart/audit/totality/sk-v17/p3/3a-architecture-synthesis.md
  - restart/audit/totality/sk-v17/p3/3c-locks-crystallisation.md
  - restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md
verdict: REVISE
counts:
  accept: 18
  revise: 1
  reject: 0
---

# CH1 CORRECTNESS — SK-V17 T-P3 V1

## Mandate

CH1 scans, per PASS-3-SYNTHESIS §3 / ORCHESTRATOR §3W:
1. every proposed delta cites a real T-P2 LAC / T-P1 divergence;
2. every cited V1-surface section resolves at `file:line`;
3. the 3C disposition matrix references real amendment candidates;
4. `3c-locks-v+1-diff.md` applies cleanly to the current `LOCKS.md`
   (`git apply --check`).

This review is READ-ONLY against the V1 surfaces. T-P3 PROPOSES; Pass Omega
CRUD applies post-G-Omega. Master HEAD `2a76916ac` (confirmed
`git rev-parse HEAD` = `2a76916ac1959ef027df4d28e09be2b0b0bbec7f`).

## Executive verdict — REVISE (1 load-bearing defect, mechanical)

The citation base is **sound across the board**: every one of the 5 LOCKS
deltas and 8 ARCHITECTURE deltas cites a real T-P2 LAC / T-P1 divergence;
every cited V1-surface section resolves at `file:line`; the 14-candidate
disposition matrix references only real amendment candidates with zero
silent drops. **One load-bearing CH1 defect**: the
`3c-locks-v+1-diff.md` unified diff **does NOT apply cleanly** — `git apply
--check` returns `corrupt patch at line 27 / EXIT 128`. The root cause is a
hunk-header line-count error (the anchor and content are correct). This is
a REVISE, not a REJECT: the fix is one line.

## Finding CH1-SKV17-01 (REVISE) — 3c-locks-v+1-diff.md fails `git apply --check`

**Severity: load-bearing.** The `3c-locks-v+1-diff.md` is the G3 gate
object (PASS-3 §2 / §8.1; this dispatch names it "load-bearing"). It MUST
`git apply --check` clean. It does not.

**Reproduction** (extract the fenced ```diff block, apply against HEAD
`2a76916ac`):

```
$ git apply --check /tmp/skv17.patch
error: corrupt patch at line 27
EXIT: 128
```

**Root cause.** The hunk header at `restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md:49`
reads:

```
@@ -606,6 +606,52 @@
```

It declares **6 old-side lines and 52 new-side lines**. The actual hunk
body contains **7 old-side context lines and 22 new-side lines** (7 context
+ 15 insertions). Both counts are wrong; the new-side count (52 vs 22) is
the fatal one — `git apply` reads to the declared 52 new lines, runs off
the hunk end at body line 27, and aborts as "corrupt patch".

**The anchor and content are CORRECT.** The 7 old-side context lines —
blank(606), `- Lock 16 primitive-manifest clause...`(607), blank(608),
blank(609), `## v+1 Governance Boundary`(610), blank(611),
`The v+1 text above is active only because...`(612) — match
`restart/locks/LOCKS.md:606`-`612` exactly. The insertion point (after the
SK-V15 addendum's Lock-16 clause, before `## v+1 Governance Boundary`) is
the one the artefact's prose describes
(`3c-locks-v+1-diff.md:29`-`33`, `:76`-`82`). The defect is isolated to the
header arithmetic.

**Concrete fix** (`restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md:49`):

```
-@@ -606,6 +606,52 @@
+@@ -606,7 +606,22 @@
```

**Verification of the fix.** With the corrected header the patch applies
cleanly:

```
$ git apply --check /tmp/skv17_fixed.patch
EXIT: 0
```

**Note on precedent.** The prior SK-V14 3C diff
(`restart/audit/totality/p3/3C-locks-v+1-diff.md:37`, header
`@@ -581,3 +581,32 @@`) `git apply --check`-applies clean (EXIT 0) against
its target state — so the diff-block convention itself is sound; the SK-V17
header is simply mis-counted. No structural/methodology defect; a single
arithmetic correction restores the gate object.

**Disposition: REVISE.** Re-emit `3c-locks-v+1-diff.md` with the header
`@@ -606,7 +606,22 @@` and re-run `git apply --check` (must EXIT 0) before
the cycle re-converges. The G3 gate cannot present a non-applying diff.

## ACCEPT rows

### 3C — five LOCKS deltas (all cite real LACs; all sections resolve)

| delta | source LAC/divergence | citation resolves | disposition |
|---|---|---|---|
| D-SKV17-L01-tape-substrate-union | LAC-2F-FOLD-01 (`2f-fold-gaps.md:580` ✓), LAC-2F-FOLD-04 (`:583` ✓), LAC-1E-SKV17-01/02/03 (`1E-locks-evidence.md:178`-`180` ✓), 2F-FOLD-U1/U2 (`:563`-`564` ✓) | `LOCKS.md:75` (exactly-one-encoding ✓), code anchors `css_l4/builder.rs:16`=`enum OpenFrame` ✓, `tape/record.rs:103`=`struct TapeRec` ✓, `skinny tape/mod.rs:94`=`struct Tape<'input>` ✓, `arena.rs:47`=`StructRegistry::compound_kind_for_layout` ✓, `tape/mod.rs:185-186`=`begin_compound`+`rule_id & 0x1F` ✓, SPEC `:793-795`=28-65×/983×/10583× regression ✓ | **ACCEPT** |
| D-SKV17-L02-structlayout-reconcile | LAC-2F-FOLD-05 (`:584` ✓), LAC-1E-SKV17-04 (`:181` ✓) | `LOCKS.md:160` (Layout canonical ✓), `:162-166` (v+1 live-state ✓); grep `StructLayout crates/`=960 / `LayoutFacts crates/`=0 (path-(b) sizing) | **ACCEPT** |
| D-SKV17-L10-tape-category-not-sixth-shape | LAC-2F-FOLD-02 (`:581` ✓), LAC-1E-SKV17-05 (`:182` ✓), 2F-FOLD-U3 (`:565` ✓) | `LOCKS.md:100-116` (LAC-1E-14 FactStream precedent ✓), `:107-108` (5-shape canon ✓), `:109` (6th G-Omega gated ✓); `ARCH:1088` (5 shapes ARE projections ✓), `:1151`/`:1282` (`admits_collapsed_stage` x86-bound ✓), `:1206`/`:1279-1280` (CollapsedStage / UNKNOWN-2D-05 ✓); SPEC `:808` (§9 6th-shape bar ✓) | **ACCEPT** |
| D-SKV17-L14-valueref-classifier-generalisation | LAC-2F-FOLD-03 generality-half (`:582` ✓), LAC-1E-SKV17-03 (`:180` ✓) | `LOCKS.md:349` (Lock 14 grammar-generalisation ✓), `skinny tape/mod.rs:175` (ValueRef ✓), `css_l4/value.rs:414` (eager value enum ✓), SPEC `:252` (preserve-rich-ast ✓); scope-honesty note `2f-fold-gaps.md:530-534` (JSON+CSS-only ✓) | **ACCEPT** |
| D-SKV17-L16-neon-classifier-manifest | LAC-2F-FOLD-03 manifest-half (`:582` ✓), LAC-1E-SKV17-06 (`:183` ✓) | `LOCKS.md:137-149` (Lock-1 v+1 no-cross-call-carry ✓), `:453`+ (Lock 16 manifest ✓); SPEC `:314-317` (alphabet-as-data ✓), `:806` (aarch64-primary no-SVE ✓) | **ACCEPT** |

All five deltas cite real LACs and divergences; all cited LOCKS/ARCH/SPEC
sections resolve at `file:line`. ACCEPT on the citation axis (CH1's scope);
the apply-check defect (CH1-SKV17-01) is a header-arithmetic REVISE
independent of these five rows' correctness.

### 3C — 14-candidate disposition matrix (all reference real candidates)

Every disposition-matrix row references a real amendment candidate that
resolves at `file:line`:
- 5 LOCKED fold LACs `LAC-2F-FOLD-01..05` at `2f-fold-gaps.md:580`-`584` (verbatim rows present) ✓
- 6 T-P1 antecedents `LAC-1E-SKV17-01..06` at `1E-locks-evidence.md:178`-`183` (verbatim rows present) ✓
- 3 ORQs `2F-FOLD-U1..U3` at `2f-fold-gaps.md:563`-`565` (verbatim rows present) ✓

No candidate is silently dropped (the 14 fold into 5 clauses with every
candidate accounted: 9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0
DEFER; tally at `3c-locks-crystallisation.md:141`-`147` is internally
consistent with the matrix at `:122`-`137`). **ACCEPT.**

Minor note (non-blocking, ≤CH4/CH6, not a CH1 citation defect): the
`3c-locks-v+1-diff.md` frontmatter `delta_summary` describes the tally as
"9 ACCEPT, 3 ORQ-ACCEPT, 2 MODIFY" (`:39`), agreeing with the matrix; an
earlier executive line in the crystallisation reads "9 ACCEPT, 5 MODIFY"
(`3c-locks-crystallisation.md:55`) which is loose shorthand — the canonical
tally table (`:141`-`147`) is correct (2 MODIFY). Flag for the author to
reconcile the prose count; does not affect any citation or the diff.

### 3A — eight ARCHITECTURE deltas (all cite real findings; all sections resolve)

| delta | source finding cited | ARCH section resolves | disposition |
|---|---|---|---|
| ARCH-3A-S17-D01 | LAC-2F-FOLD-01 (`2f:580` ✓), Div A+B, `1e:126` ✓ | §7.3 `:1088` ✓, §9.1 `:1840`/`:1861` ✓ | **ACCEPT** |
| ARCH-3A-S17-D02 | LAC-2F-FOLD-03-home/F2 (`2f:158-192`), Div C, `1e:127` ✓ | §9.2 `:1863` ✓, §9.1 `:1861` ✓ | **ACCEPT** |
| ARCH-3A-S17-D03 | LAC-2F-FOLD-03/F5 (`2f:582` ✓), Div E, `1e:131` ✓ | §13/§7.3 `:1083` (PrimitiveFacts ✓) | **ACCEPT** |
| ARCH-3A-S17-D04 | LAC-2F-FOLD-02/F4 (`2f:581` ✓), Div D, `1e:129` ✓ | §7.3 `:1090`-`1116` (BackendShape enum ✓), §9 taxonomy ✓ | **ACCEPT** |
| ARCH-3A-S17-D05 | LAC-2F-FOLD-04/F6 (`2f:583` ✓), Div F, `1e:128`/`:140` ✓ | §7.3 `:1128`-`1133` (manifest binding ✓) | **ACCEPT** |
| ARCH-3A-S17-D06 | LAC-2F-FOLD-05/F9 (`2f:584` ✓), `1e:130`/`:181` ✓ | §7.4 `:1286` ✓, §7.3 `:1075` (LayoutFacts ✓) | **ACCEPT** |
| ARCH-3A-S17-D07 | F8 (`2f:424-457`), Div D, BSHAPE17-002/004 | §7.3 `:1118`-`1176` (cost-model pipeline ✓), ledger `:1188`-`1206` ✓ | **ACCEPT** |
| ARCH-3A-S17-D08 | 2F-FOLD-U1/U2/U3 (`2f:563-565` ✓), `1e:189-190` | §9.2 `:1877`-`1888` ✓, §7.3 ledger `:1206` ✓ | **ACCEPT** |

All eight deltas cite real T-P2 LACs / T-P1 divergences and resolve their
ARCH sections at `file:line`. **ACCEPT** on the CH1 citation axis.

## Cross-cut confirmations (CH1 scope)

- **Numbered locks preserved**: `LOCKS.md` carries 16 numbered lock headings
  (`:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`); the
  diff adds none, retires none, renumbers none. ✓
- **5-shape canon verbatim**: `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}` (`LOCKS.md:107`-`108`); the addendum restates the five in
  the heading and Lock-10 clause; no 6th variant. ✓
- **Insertion point**: after the SK-V15 addendum's Lock-16 clause
  (`LOCKS.md:607`), before `## v+1 Governance Boundary` (`:610`). The diff
  context lines anchor here exactly. ✓
- **Master HEAD**: `2a76916ac1959ef027df4d28e09be2b0b0bbec7f` confirmed. ✓

## Disposition summary

| object | disposition |
|---|---|
| 3c-locks-v+1-diff.md `git apply --check` | **REVISE** (CH1-SKV17-01: header `-606,6 +606,52` → `-606,7 +606,22`) |
| D-SKV17-L01..L16 (5 LOCKS deltas) | **ACCEPT** (all cite real LACs; all sections resolve) |
| 14-candidate disposition matrix | **ACCEPT** (all reference real candidates; zero silent drops) |
| ARCH-3A-S17-D01..D08 (8 ARCH deltas) | **ACCEPT** (all cite real findings; all sections resolve) |

**Counts: 18 ACCEPT, 1 REVISE, 0 REJECT.**

The single REVISE is the load-bearing one — the G3 gate object must
`git apply --check` clean and currently does not. The fix is one line and
verified. Once the header is corrected and the patch re-checked (EXIT 0),
CH1 has zero open defects.

## Open question tagged to lens

| lens | question |
|---|---|
| CH1 | After the header fix, Pass Omega CRUD must re-run `git apply --check` against the then-current `LOCKS.md` (line numbers shift if any earlier addendum lands first); the gate object's anchor at `:606`-`612` is stable only if no prior CRUD op precedes it. Recommend the diff carry a re-anchor note for Pass Omega. |
