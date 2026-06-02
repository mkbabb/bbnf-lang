# CH6 NEXT-TRANCHE-IMPACT — Pass Omega V10 CHALLENGE (cycle V1)

Lens: does Ω-F's next-cycle directive specify concrete measurable entry
conditions; are the G-Omega sign-off items (the locks-diff, the master-plan-diff,
the CRUD plan, the SK-V18 close summary) concretely measurable?

Boundary respected: all artefacts are STAGED ONLY under
`restart/audit/totality/astral/V10/`; no live governance surface is edited. I
spot-verified load-bearing items rather than re-deriving T-P3.

## Spot-verifications (the load-bearing items)

1. **`git apply --check` on the staged locks-diff → EXIT 0.** Confirmed at HEAD:
   `awk '/^```diff$/{flag=1;next} /^```$/{flag=0} flag{print}' locks-diff.md | git apply --check -` exits 0.
   The upstream `3C-locks-v+1-diff.md` ALSO applies (exit 0). The hunk header
   `@@ -622,6 +622,33 @@` is the corrected form (the prior-cycle CH6-V1-01 REJECT
   of the malformed `@@ -622,6 +622,38 @@` / one-blank body is already folded;
   `3C-locks-v+1-diff.md:22`). 16 numbered locks present at the cited lines
   `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`
   (`grep -nE '^[0-9]+\. \*\*'`); insertion lands after the SK-V17 Lock-16 clause
   (`LOCKS.md:622`) and before `## v+1 Governance Boundary` (`:625`) — verified.
2. **A cited §H wave resolves.** The §13.7 table's H-row alignments resolve in
   live MASTER-PLAN: H.W4 SinkOnly (`:144`,`:604`), H.W4.LOCK14 (`:605`),
   H.W5 x86 successor (`:146`,`:149`), H.W6 CSS >SOTA (`:147`-`:148`). The
   master-plan-diff anchors all resolve: §13.6 header at `:974`, §14 (the §13.7
   insertion point) at `:1042`, §25 footer at `:1415` with the `-`-side text
   byte-matching the live surface.
3. **A REDRESS reference resolves.** REDRESS items 51/53/247 (cited by the
   CH3-V1-R2 G2/G4/G6 pre-block) are all genuine rejected routes:
   item 53 "is REJECTED" (`skinny/REDRESS.md:786`), item 247 closes as `REJECT`
   (`:6232`), item 51 is the open route 53 closed (`:787`). The "measured-and-
   reverted shape" framing is grounded.
4. **Cited SHAs + V9 close resolve.** `33b51d8f4` = "pass-omega-v5-crud6-audit"
   (V5 closed for SK-V17); `c5a4f7644` = "pass-omega-v5-crud4-handoff-migration"
   (the CRUD-4 analog ΩF cites); `restart/audit/totality/astral/V9/G-OMEGA-SIGNOFF.md`
   = CLOSED 2026-05-28. The directory lineage V1..V9-closed → V10-live is sound;
   the V10 index is authoritative, and "Pass Omega V6" is a genuine historical
   content-label (the MIGRATION.md:190 "Historical Pass Omega V6 W5BR Migration
   Receiver", confirmed verbatim) — so a current-pass "V6" label is a real
   false-current collision, not a hypothetical.
5. **Live drift COH18-001 confirmed.** `HANDOFF.md:16-19` defines SK-V18 as the
   totality-`crates/core/`-adopt cycle ("it adopts the SKINNY-proven … model into
   the totality `crates/core/` tree"), and the dispatch directive (d) at `:103-105`
   says "dispatch **SK-V18 W0** (the `crates/core` tape-fold)". The staged HANDOFF
   strike/re-root targets the correct lines.

## Measurability verdict on the G-Omega sign-off items

- **locks-diff** — MECHANICALLY measurable: `git apply --check` exit 0, four
  verification greps, 16-lock/5-shape invariant check. Strongest item.
- **Ω-F next-cycle directive** — concrete measurable entry conditions PRESENT:
  W-PRUNE predicates `x86_tree_deleted==true` (P1), `runtime_target_rows_collapsed==true`
  (P3), `lock14_gate_scans_codegen==true` (P4), `grep -c parse_w11_1_number==0`
  (P5); the 10-row blocker matrix each carries a measurable gate; the GENERALIZE
  lattice (`sk-v18/SPEC.md:535-547`) is predecessor-gated with named falsifiers.
  Grounded in the certified SPEC (`:46-49`, `:19-21`, `:429-449` all resolve).
- **master-plan-diff** — measurable by ANCHOR RESOLUTION only, NOT mechanically:
  9 ```diff fences but ZERO `@@`/`diff --git` headers — these are CONTENT-SHAPE
  diffs, not git-appliable hunks (by design; §8 routes the cost to the manual CRUD
  pass). Acceptable, but it is the weakest of the diff items and carries the
  defect below.
- **CRUD plan** — measurable: CRUD-1..CRUD-6 each name surface + operation +
  owner; CRUD-6 is an explicit citation-scrub gate. CF-01..CF-12 cohesion fixes
  each name surface + owner.
- **SK-V18 close summary** — the G-OMEGA-PACKAGE 14-line spec is defined in the
  workflow; the disposition (9A/11M/0R/1D = 21) is consistent across ΩC/ΩF/
  master-plan-diff/staged deltas.

## Enumeration of staged amendments / CRUD operations (CH6 lens)

| # | Staged amendment / CRUD op | Artefact | Disposition |
|---|---|---|---|
| 1 | locks-diff: SK-V18 T-P3 v+1 Crystallisation Addendum (11 clauses, `git apply --check` exit 0) | `locks-diff.md` / CRUD-3 | ACCEPT |
| 2 | Ω-F next-cycle directive: 8-step sequence + W-PRUNE entry predicates | `ΩF-migration-handoff.md` | ACCEPT |
| 3 | HANDOFF OP-1..OP-5 (override block, strike `:16-19`, re-root `:103-105`, blocker matrix, directive) | `handoff-delta.staged.md` / CRUD-4b | ACCEPT |
| 4 | MIGRATION OP-1 §0.0 SK-V18 Pass Omega V10 receiver + 12-wave ledger | `migration-delta.staged.md` / CRUD-4a | ACCEPT |
| 5 | MIGRATION OP-2 five disposition rows (x86/courier/replicas/phantom/css_types) | `migration-delta.staged.md` | ACCEPT |
| 6 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 pre-block (V10-labelled, correct) | `migration-delta.staged.md` | ACCEPT |
| 7 | master-plan-diff Diff 2: NEW §13.7 SK-V18 GENERALIZATION block — carries **"Pass-Omega-V6 / pre-W-PRUNE blocker"** (`:192`) | `master-plan-diff.md` / CRUD-2 | **REVISE** |
| 8 | master-plan-diff Diff 1/3/4/5/6 (re-key §13.6→SK-V19, §25, §24, §5/§13.5, §13 H-row): anchors resolve; content-shape (not git-appliable) | `master-plan-diff.md` / CRUD-2 | ACCEPT |
| 9 | Net-LOC harmonization (CF-11): Ω-F cites only "≈ −10800", omits the "(per-wave SPEC sum ≈ −10685)" dual figure CF-11 mandates for Ω-F | `ΩF` + both staged deltas | **REVISE** |
| 10 | ΩD narrative carries "Pass-Omega-V6 / pre-W-PRUNE blocker" (`ΩD:82`) — reasoning artefact, not merged, but cycle-staged under the same refusal condition | `ΩD-master-plan-reconciliation.md` | **REVISE** |
| 11 | ΩB carries "Pass-Omega-V6 / pre-W-PRUNE blocker" ×2 (`ΩB:89`,`:204`) — same non-propagation | `ΩB-skinny-lessons.md` | **REVISE** |
| 12 | ΩF P3 figure grounding: "−5460 = 6×910 replica bodies + ~−40 collapsed rows + 1 PartialEq" (`:103`,`:149`) — 6×910 already = 5460, so the sum is internally muddled vs SPEC:435 "≈ −5500" | `ΩF-migration-handoff.md` | **REVISE** |
| 13 | CRUD plan CRUD-1..CRUD-6 + CF-01..CF-12 (incl. CRUD-6 V6→V10 scrub) | `ΩA-coherence-audit.md` | ACCEPT |

## The load-bearing REVISE (item 7) — the material finding

Ω-F's own resolution mandates: "**Everywhere 3F says 'Pass Omega V6', the staged
diffs read 'Pass Omega V10'**" (`ΩF:54`), and Ω-F's Refusal Condition fails CRUD-4
closed if "Any staged text labels the current pass 'Pass Omega V6' (a false-current
collision with the historical V6 W5BR receiver, `restart/MIGRATION.md:190`)"
(`ΩF:255`-`:256`). ΩA independently flags this as OA-V10-03 / CF-02 and adds a
CRUD-6 scrub line for it (`ΩA:121`,`:226`,`:247`).

**Yet the staged `master-plan-diff.md` §13.7 block — the text CRUD-2 applies to
the V1 surface MASTER-PLAN.md — still reads "Pass-Omega-V6 / pre-W-PRUNE blocker"
(`master-plan-diff.md:192`).** The Ω-D author propagated the upstream 3F "V6"
label into the staged §13.7 CH3-V1-R2 paragraph WITHOUT applying the V6→V10
reconcile that Ω-F mandates and that the sibling `migration-delta.staged.md:113`
correctly applies ("a Pass-Omega-V10 / pre-W-PRUNE blocker"). The "V6" here labels
the *receiver of the SK-V16/V17 reconcile blocker* = the current pre-W-PRUNE pass
= Pass Omega V10; calling it "V6" is exactly the false-current collision the
consolidated refusal condition forbids. CRUD-2 would write the forbidden label
into MASTER-PLAN §13.7 and rely on CRUD-6 catching it — a staged amendment must
not depend on a downstream scrub to undo a label its own cycle's refusal condition
bars.

CORRECTION: in `master-plan-diff.md:192`, change `Pass-Omega-V6 / pre-W-PRUNE
blocker` → `Pass-Omega-V10 / pre-W-PRUNE blocker` (matching
`migration-delta.staged.md:113` and the ΩF resolution). Items 10/11 are the same
correction in the non-merged reasoning artefacts ΩD/ΩB; item 9 adds the CF-11
dual-figure parenthetical to the staged deltas + ΩF; item 12 repairs the P3
arithmetic to read "≈ −5500 (6×910 = −5460 replica bodies + ~−40 collapsed rows +
1 PartialEq)".

## Not found (checked, clean)

- No non-applying diff in the locks-diff (exit 0, twice).
- No revived REDRESS route: 51/53/247 are fenced as rejected, the §13.7
  Invariant Check fences AZ-IV-eager / StructRegistry-per-leaf / fact-stream /
  x86 / second-substrate.
- No Lock-14 narrowing: the addendum preserves the 16-lock count + 5-shape canon
  by addition; the green-by-exclusion clause WIDENS (`FORBIDDEN ⊇ {GENERATED_RS,
  CSS_GENERATED_RS, EventGrammar, *EventGrammar}`); the css_types.rs / 9-ident
  collapse are explicitly tee'd to SK-V19, not laundered into the SK-V18 +15.
- No coupling: GENERALIZE/PROVE waves are predecessor-gated with named entry
  predicates; P4-before-G2/G3 is a hard ordering; the skinny-vs-totality firewall
  scope is kept distinct (CH5-DEFECT-V1-02).
- No uncited claim in the load-bearing chain: every spot-check resolved.

## Tally rationale

13 enumerated items: 8 ACCEPT, 5 REVISE, 0 REJECT. REVISE share = 5/13 ≈ 38.5%,
above the ≥30% cycle-V1 expectation. The single load-bearing defect (item 7) is a
staged-text label that violates the cycle's own consolidated refusal condition and
would otherwise be merged into a V1 surface; the other four REVISEs are the same
non-propagation in sibling artefacts (10/11), a CF-11 dual-figure omission (9), and
a P3 arithmetic wobble (12). No REJECT: the locks-diff applies, no route is
revived, no lock is narrowed.

TALLY accept=8 revise=5 reject=0
