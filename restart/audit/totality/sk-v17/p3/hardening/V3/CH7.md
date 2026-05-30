---
lens: CH7 OVERFIT-PRUNE
pass: T-P3-synthesis
cycle: V3
reviewer: CH7 (V3)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
subject: restart/audit/totality/sk-v17/p3/{3a,3b,3c,3d,3e,3f}.md + 3c-locks-v+1-diff.md
focus: 16-lock count preserved (no silent renumber; ADD/RETIRE G-Omega-gated); no contrivance; the fold is genuinely general; no fabricated speed claim; lightningcss the fair bar
dispositions: { ACCEPT: 13, REVISE: 0, REJECT: 0 }
verdict: ACCEPT (100.0% ACCEPT)
prior_cycle_dispositions_folded:
  accepted:
    - CH7-S17-V2-R1-css-sota-overclaim-exec-summary-and-handoff-carrier   # FOLDED: 3f:31 "contract for"; 3f:88 "JSON >SOTA-proven, CSS >SOTA the SK-V18 proof obligation, bar not yet met"
    - CH7-S17-V2-OQ-css-sota-stamp                                        # FOLDED: 3f:143-147 cold-start carrier stamp "CSS >SOTA = SK-V18 obligation, NOT met"
---

# CH7 OVERFIT-PRUNE — T-P3 SK-V17 Synthesis (cycle V3)

## Lens scope

CH7 is the overfit-prune lens. It does not re-audit citation-resolution (CH1)
or coupling (CH5); it asks five questions and only those: (1) is the 16-lock
count genuinely preserved, with no silent renumber and ADD/RETIRE G-Omega-gated;
(2) is any delta a contrivance dressed up as principle; (3) is the
tape/`ValueRef<G>`/NEON fold *genuinely* general or JSON+CSS overfit wearing a
generality costume; (4) is any speed claim fabricated or over-stated; (5) is
lightningcss (and the JSON SOTA cohort) held as the fair, un-gamed bar. I
re-executed the load-bearing claims against ground truth at HEAD `2a76916ac`,
not against the prose's self-citations.

## V2 fold confirmation (the single V2 REVISE + its OQ both resolved)

| V2 item | V2 disposition | V3 status |
|---|---|---|
| **CH7-S17-V2-R1** — 3F exec summary (`3f:31`) + HANDOFF carrier (`3f:84`) framed SK-V17 as "proving CSS-on-tape … >SOTA", attaching ">SOTA" to CSS and calling it proven, while ground truth says the CSS >SOTA bar is NOT met (UNMEASURED-PENDING) | REVISE | **FOLDED.** `3f:31` now reads "the converged skinny **contract for** CSS-on-tape / lazy-`ValueRef` / shared-NEON — CSS the SK-V17 first-mover, the **CSS >SOTA bar UNMEASURED-PENDING and held as the SK-V18 proof obligation** … the JSON model is >SOTA-proven". `3f:88` (the cold-start carrier) now reads "SK-V17 (the SKINNY tape-fold **contract** for … **JSON >SOTA-proven, CSS >SOTA the SK-V18 proof obligation, bar not yet met**)". Verified verbatim. The exact CH7 over-stated-speed failure mode is removed; the framing matches 3D's banner, 3E P5b, and 3F's own next-cycle CH3 text. |
| **CH7-S17-V2-OQ** — the cold-start HANDOFF carrier should stamp "CSS >SOTA = SK-V18 obligation, NOT met" adjacent to the SK-V18 dispatch line, so no cold-start agent reads CSS >SOTA as achieved | open question | **FOLDED.** `3f:143`-`147` now carries the stamp verbatim: "CSS >SOTA = SK-V18 obligation, NOT met: SK-V17 proved the JSON model >SOTA (`skinny/RESULTS.md`) and converged the CSS-on-tape CONTRACT; ALL per-corpus lightningcss endpoints are UNMEASURED-PENDING (`SPEC.md:207`) … SK-V18 W0 carries the CSS >SOTA bar as a PROOF OBLIGATION, not an achieved win." At the right altitude, adjacent to the next-directive block. Honest. |

## Verification ledger (ground-truth re-execution at HEAD `2a76916ac`)

| claim under test | source | re-executed result | verdict |
|---|---|---|---|
| 16 numbered locks, no renumber | `restart/locks/LOCKS.md` | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = 16; highest-numbered = 16; the v+1 diff inserts an addendum and adds/retires/renumbers ZERO numbered lock | TRUE |
| 5 BackendShape variants verbatim | `restart/locks/LOCKS.md:107`-`108` | `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` present; addendum + Lock-10 clause restate the five, add no 6th; the 6th remains G-Omega gated (`:109`) | TRUE |
| v+1 diff applies clean | `3c-locks-v+1-diff.md` ```diff``` block, `@@ -606,7 +606,22 @@` | extracted the diff block, ran `git apply --check` → EXIT 0, "APPLIES CLEAN"; context at `LOCKS.md:606`-`610` matches (SK-V15 Lock-16 clause → blank → `## v+1 Governance Boundary`) | TRUE |
| `StructLayout` = 960 sites in `crates/` | `3c:94`, `3a:82`, `3b`, `3f:66` | `grep -rc StructLayout crates/` summed = 960; `skinny/crates/` = 0 (distinct surface) | TRUE |
| `backend_shape`/`LayoutFacts` = 0 in `crates/` | `3c-locks-v+1-diff.md:74` | `grep -rE 'backend_shape\|LayoutFacts' crates/ \| wc -l` = 0 (side-table surface is 0→N introduce, as the clause states) | TRUE |
| JSON >SOTA carrier real measured row | `skinny/RESULTS.md:5,61` | twitter parse_only: Track 1 **8349.290** Mbps > sonic-rs strict **4913.095** Mbps (**+69.9%**), per-iter equality PASS, aarch64/Apple M5 Max, 400 iters — real measured row, not invented | TRUE |
| CSS >SOTA NOT met / UNMEASURED-PENDING | `restart/skinny/tranches/sk-v17/HANDOFF.md:44`-`45`; `SPEC.md:207` | HANDOFF: "the **>SOTA bar is NOT met and nothing on the CSS path moved**"; SPEC: "ALL per-corpus lightningcss endpoints are **UNMEASURED-PENDING**" | TRUE — and this is the ground that the V2 R1 fold now respects across 3D/3E/3F |
| 3F R1 fold landed (no residual over-claim) | `3f:31`,`:88`,`:143`-`147` | grep across all `p3/*.md` for `proving .{0,30}>sota` / `proof of css.{0,20}>sota` → 0 hits; the only "proving"/">SOTA"/CSS co-occurrences are now the corrected "contract for" + "proof obligation, NOT met" phrasings | TRUE |

Every load-bearing lock/grammar/speed number CH7 could falsify resolved to
ground truth. No fabrication surfaced in 3A/3B/3C/3D/3E/3F. The single V2
over-stated CSS framing is folded out; no new over-claim entered V3.

## Section dispositions

### §3A ARCHITECTURE synthesis — ACCEPT
Eight deltas; each fold is conservative against the V1 surface (ARCH §7.3
already frames the five shapes as tape projections; the fold makes the
retirement-of-the-live-eager-builders step explicit, invents no substrate).
`ARCH-3A-S17-D01`'s "exactly ONE encoding survives; a dual AoS/SoA end-state
admissible ONLY as a transient fold-state" is the correct anti-overfit closure,
and the SoA `Tape` it converges onto is the proven-and-benched form. No CSS
>SOTA over-claim hides in 3A (verified by grep): the `>sota` co-occurrence at
`3a:82` is the proven SoA fold-directive prose, not a CSS speed assertion. The
`udot`/i8mm orphan kernel remains correctly REFUTED in the deferred appendix.
No contrivance; no fabricated speed.

### §3B MASTER-PLAN reconciliation — ACCEPT
No refuted wave revived; the Refuted-Route Confirmation fences (AZ-IV 118× /
per-leaf indirection / fact-stream-String / x86) are carried as pre-blocks, not
re-derived. The `3b:115` row treats F2's lazy `ValueRef<G>` as the proven shape
of the pending SK-V15 W5/W6 provider — a fold of an already-pending wave, not a
new speed claim. The 5-shape canon is preserved across §13. No fabricated
throughput.

### §3C LOCKS crystallisation — ACCEPT
Five lock-addendum clauses (Locks 1/2/10/14/16), zero silent drops, 9 ACCEPT /
3 ORQ-ACCEPT / 2 MODIFY / 0 REJECT / 0 DEFER. The diff applies clean (verified
EXIT 0). The 16-lock count and 5-shape canon are preserved verbatim; the
Invariant Check (`3c-locks-v+1-diff.md:100`-`105`) re-states them, and the V2
CH5-R01 distribution-invariant bullet now rides the gate object (`:103`). No
lock renumbered; ADD/RETIRE correctly G-Omega-gated; the addendum is the gate
object, not an in-place numbered-lock edit. The Lock-14 clause is explicitly
SCOPE-HONEST ("exercised JSON+CSS ONLY; Sheets/BBNF-self by-construction …
may not be claimed fleet-wide") — the anti-overfit posture the lens wants in the
lock text itself.

### §3D skinny-fold — ACCEPT
Monotonic direction held: SKINNY wins → V1-authoritative; rejections →
locks-strengthening; totality never dictates back. The SCOPE-HONESTY BANNER
(`3d:45`-`51`) is in place and V3 EXTENDS it with the CSS-`>SOTA`-UNMEASURED-PENDING
clause (`3d:49`,`:72`), so the G3-skim firewall now covers the CSS speed axis,
not only the by-construction-vs-by-exercise axis. The dedicated SK-V18 CSS
`>SOTA` non-fit row (`3d:149`,`:161`) states plainly that no 3D delta asserts CSS
>SOTA met and that an artefact asserting it FAILS CH7. The ONE load-bearing WIN
is the SoA `Tape`+`ValueRef<G>`, grounded in the verified 8349 > 4913 row. This
is the anti-overfit posture.

### §3E grammar-generalisation — ACCEPT (the strongest anti-overfit section)
This is where overfit would hide, and it does not. The per-grammar matrix tags
every non-JSON/non-CSS shape cell *predicted (cost-model-pending)* at the CELL
level (`3e:151`-`157`) with an explicit **by-construction (SK-V18 proof)**
provenance column — a reader scanning the dominant-shape column reads
`predicted` on every Sheets/BBNF-self/EBNF/BNF/CSV/math row. P5 is split into
P5a (CSS classifier-scan, wired/measured) vs P5b (CSS tape-consumer,
SK-V18-pending, NOT measured); P6 is the orthogonal value-plane firewall. The
`3E17-D07-by-construction-not-by-exercise-scoping` clause bars fleet-wide wording
when only one of Sheets/BBNF-self is witnessed (`sheets_witness` is a 24-LOC stub
with no `BackendRule`). The classifier generality is config-breadth
(alphabet-as-data across 8-of-9 grammars), never conflated with fleet-wide
value-plane proof. The math row is correctly DEFERRED with a named receiver +
blocker + gate. General, not overfit. Lock 14 grammar-neutrality preserved; no
JSON-narrowing, no CSS-narrowing.

### §3F MIGRATION/HANDOFF — ACCEPT (the V2 REVISE folded here)
The single V2 CH7 REVISE is FOLDED: the exec summary (`3f:31`) and the HANDOFF
cold-start carrier (`3f:88`) now frame SK-V17 as the converged CSS-on-tape
*contract* with the CSS >SOTA bar held as the SK-V18 proof obligation and
explicitly NOT met, while the JSON model is correctly stated as >SOTA-proven.
The V2 open question is answered: the cold-start carrier carries the "CSS >SOTA
= SK-V18 obligation, NOT met" stamp (`3f:143`-`147`) adjacent to the SK-V18
dispatch line, mirroring 3D's banner. Both edits are proposal-only text, no
delta dropped, no lock touched. lightningcss is now held as the fair (unmet,
UNMEASURED-PENDING) CSS bar across all six artefacts; the JSON SOTA cohort
(sonic-rs/serde) is held fair via same-run same-plane RESULTS rows. The
fabricated-speed-claim firewall this lens owns is intact.

## Overfit-prune findings: NONE

- **16-lock count**: preserved, verified verbatim (`grep -cE` = 16, highest = 16);
  ADD/RETIRE G-Omega-gated; the addendum is the gate object, not an in-place
  numbered-lock edit; the diff applies clean. No silent renumber. ACCEPT.
- **5-shape canon**: `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}` verbatim; the tape is disposed as a substrate-manifest
  CATEGORY, not a silent 6th shape, on two independent grounds (categorical
  precedent + `admits_collapsed_stage` aarch64-refusal). ACCEPT.
- **Contrivance scan**: every delta answers a T-P1 divergence or T-P2 LAC with
  verified path:line; the `udot`/i8mm orphan kernel is correctly REFUTED and kept
  in the deferred appendix; the StructRegistry fence is principled, not bolted-on.
  No contrivance.
- **Genuine generality**: §3E proves it with cell-level by-construction tagging,
  a live-baseline monotonic onboarding test, the P5a/P5b severance, and the P6
  value-axis firewall. The fold is general (alphabet-as-data, grammar-parametric
  `ValueRef<G>`), not JSON+CSS overfit. No fleet-wide over-claim survives.
- **No fabricated speed claim**: the JSON number (8349 > 4913 Mbps, +69.9%) is
  cited to `skinny/RESULTS.md` and re-verified as a real measured aarch64 row.
  The single V2 over-stated claim ("proving CSS … >SOTA") is folded out; grep
  confirms zero residual "proving … >SOTA" anywhere in p3.
- **lightningcss fair bar**: held fair for JSON via >SOTA same-plane RESULTS
  rows; for CSS the bar is the strict same-run full-CSSOM comparator and is
  correctly UNMEASURED-PENDING everywhere, now including 3F (the V2 R1 lines that
  read it as met are folded). The bar is held fair across all six artefacts.

## Open questions

None. The V2 CH7 open question (cold-start carrier stamp) is ANSWERED and landed
at `3f:143`-`147`. No new overfit aperture surfaced in V3.

## Verdict

**ACCEPT (100.0% ACCEPT).** 13 ACCEPT, 0 REVISE, 0 REJECT. The 16-lock count and
5-shape canon are preserved and verified verbatim; the diff applies clean; the
fold is genuinely general (not overfit) with cell-level by-construction scoping,
a live-baseline onboarding test, and the P5a/P5b + P6 axis severances; the JSON
SOTA bar is held fair and the load-bearing 8349 > 4913 row re-verified. Both V2
CH7 items — the over-stated CSS >SOTA framing in 3F's exec summary + HANDOFF
carrier (REVISE) and the cold-start-stamp open question — are folded and verified
verbatim. No lock renumbered; no candidate dropped; no fabricated or over-stated
speed claim survives; lightningcss is held as the fair, unmet, UNMEASURED-PENDING
CSS bar across all six artefacts. This is CH7's second consecutive clean cycle
(V2 92.3% with one REVISE → V3 100.0% with that REVISE folded), satisfying the
≥95%-for-two-consecutive-cycles convergence direction on the overfit-prune axis.
