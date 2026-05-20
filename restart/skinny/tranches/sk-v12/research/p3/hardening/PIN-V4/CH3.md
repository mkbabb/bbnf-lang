# SK-V12 S-P3 PIN-V4 CH3 Regression / REDRESS Challenge

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: PIN-V4.
Lens: CH3 regression, JSON guard floors, REDRESS material differentials,
REDRESS 111-120 blockers, FIXPOINT evidence, revert protocol, stale
measurement prevention, and W1b-1/W1b-2 fallback correction.
Date: 2026-05-20.
Packet under review: commit `471bf53e`.
Output: this file.

## Disposition

PASS.

Confidence: 97%.

The PIN-V4 packet preserves the PIN-V3 accepted CH3 controls. JSON guard
floors remain binding unless a wave records a measured REDRESS demotion.
Union and ASM-gen are reopened only at category level; historical REDRESS
implementations remain citation and material-differential requirements.
REDRESS 111-120 remain blockers or routed evidence, not CSS or JSON-direct
admission paths. FIXPOINT requires measurement, not plan prose. Revert
protocols, stale-measurement rejection, and the corrected W1b-1/W1b-2 fallback
split are present across SPEC, dispatch, and the six P3 packet files.

## Findings

1. PASS - JSON guard floors and measured demotion remain regression-safe.

   The user pin keeps JSON direct and typed guard floors active and requires
   measured gate disposition for any demotion (`USER-PIN-W1-CSS-L4-SOTA.md:84`-`95`).
   SPEC carries the concrete direct and typed guard floor tables and requires
   any JSON-producing wave to rerun guards or prove no JSON path and
   `skinny/RESULTS.md` movement (`SPEC.md:187`-`211`). P3-C repeats the guard
   floors and inherited shape (`p3c-falsifiability-gates.md:94`-`127`), while
   P3-D makes guard state, no-touch proof, full guard rows, floor source, and
   REDRESS demotion required telemetry (`p3d-telemetry-schema.md:200`-`218`).
   The live results table still renders the admitted guard rows and diagnostic
   parse-only rows, including the four direct guard rows and seven typed guard
   rows (`skinny/RESULTS.md:5`-`41`).

2. PASS - REDRESS material differentials are preserved for union and ASM-gen.

   The pin reopens union and ASM-gen only at category level while preserving
   REDRESS 96/97/98 and 88/89/90 as historical measured implementations that
   new attempts must cite and materially differentiate (`USER-PIN-W1-CSS-L4-SOTA.md:50`-`69`).
   S-P2 convergence carries the same boundary into S-P3 (`HARDENING-S-P2-CONVERGED.md:54`-`63`).
   P3-B requires W3 to cite REDRESS 96/97/98 and name a material differential
   (`p3b-wave-sequencing.md:42`-`48`), and W4 to cite REDRESS 88/89/90 when
   adjacent while disposing the orphan set (`p3b-wave-sequencing.md:50`-`62`).
   P3-C and P3-E preserve the same replay blockers and admissible fresh-attempt
   framing (`p3c-falsifiability-gates.md:313`-`323`,
   `p3e-preblocked-ledger.md:55`-`57`, `p3e-preblocked-ledger.md:78`-`80`).
   The underlying REDRESS history supports this treatment for 96/97/98
   (`skinny/REDRESS.md:2795`-`2950`) and 88/89/90 (`skinny/REDRESS.md:2508`-`2598`).

3. PASS - REDRESS 111-120 remain blockers or routed evidence.

   The pin leaves REDRESS 111 and 114-120 unchanged, and supersedes 112/113 only
   by making CSS L4 the explicit mandate (`USER-PIN-W1-CSS-L4-SOTA.md:118`-`121`).
   P3-E keeps REDRESS 111 as report-lane-only and JSON direct residuals as
   guard/routed-only unless a post-CSS wave supplies fresh material evidence
   (`p3e-preblocked-ledger.md:63`-`64`, `p3e-preblocked-ledger.md:95`,
   `p3e-preblocked-ledger.md:126`-`155`). SPEC blocks replays of REDRESS 111-120
   without material differential and CHALLENGE (`SPEC.md:656`-`657`), and P3-C
   says REDRESS 119/120 remain routed guard evidence unless later fresh material
   evidence measures both tracks (`p3c-falsifiability-gates.md:469`-`481`).
   The REDRESS ledger confirms 112/113 blocked the old non-JSON baseline axis,
   114/115 are measured direct rejects, 116/117/118 are entry blocks, and
   119/120 close SK-V11 as fixpoint without direct or non-JSON admission
   (`skinny/REDRESS.md:3311`-`3553`).

4. PASS - FIXPOINT evidence cannot paper-close.

   SPEC requires measured CSS redress, measured ADMIT uncloseability, one new
   measured union attempt with REDRESS 96/97/98 differential, one new measured
   ASM-gen attempt with scalar/checkasm/microbench/same-wave evidence and
   REDRESS 88/89/90 differential when adjacent, zero production orphans, and
   REDRESS for every miss (`SPEC.md:66`-`83`). P3-B repeats the W5 requirement
   for measured CSS, W3, W4, zero-orphan, and JSON guard evidence before
   FIXPOINT (`p3b-wave-sequencing.md:64`-`69`,
   `p3b-wave-sequencing.md:196`-`199`). P3-C denies plan-time W3 credit and
   requires source implementation or accepted microbench rejection plus REDRESS
   evidence (`p3c-falsifiability-gates.md:339`-`347`); W4 likewise counts only
   with scalar/checkasm/microbench/equality evidence and no production orphans
   (`p3c-falsifiability-gates.md:397`-`402`). W5 close fails on skipped
   union/ASM-gen in FIXPOINT or any future-phase promise (`p3c-falsifiability-gates.md:424`-`450`).

5. PASS - Revert protocols and stale-measurement prevention are explicit.

   SPEC has per-wave revert protocols for W0, W1a, W2, W1b-1, W1b-2, W3, W4,
   and W5 (`SPEC.md:312`, `SPEC.md:348`-`349`, `SPEC.md:384`-`385`,
   `SPEC.md:444`-`445`, `SPEC.md:493`-`494`, `SPEC.md:540`-`541`,
   `SPEC.md:601`-`602`, `SPEC.md:639`). P3-C ties W1b-2, W3, and W4 rollback
   to stale run, guard regression, row miss, equality miss, or replay failures
   (`p3c-falsifiability-gates.md:266`-`269`,
   `p3c-falsifiability-gates.md:348`-`350`,
   `p3c-falsifiability-gates.md:404`-`407`). Stale measurement prevention is
   schema-bound: stale run ids, mixed-run comparators, oracle coupling,
   producer-only telemetry, missing provenance, and permissive/lossy comparators
   reject (`p3c-falsifiability-gates.md:90`-`92`,
   `p3d-telemetry-schema.md:76`-`79`, `p3d-telemetry-schema.md:257`-`282`).
   DISPATCH carries the same gate-consumed telemetry and failure conditions
   (`DISPATCH-PROMPT.md:169`-`183`).

6. PASS - The W1b-1/W1b-2 fallback correction remains folded.

   PIN-V3 consolidated this as a load-bearing accepted fact: W1b-1 scaffold
   failure records REDRESS and returns to plan, but fallback stays blocked until
   W1b-2 records measured CSS lightningcss comparator/admission redress
   (`PIN-V3/CONSOLIDATED.md:31`-`34`). PIN-V4 SPEC preserves that split:
   W1b-1 scaffold failure does not satisfy the post-CSS-redress fallback
   condition, and Sheets/BBNF remain blocked until W1b-2 measured redress unless
   the user re-pins or S-P3 revises topology (`SPEC.md:433`-`442`). W1b-2 is the
   first comparator/admission wave whose measured failure can support later
   fallback planning (`SPEC.md:482`-`491`). P3-B mirrors this by saying Sheets
   and BBNF-self are not W1b-1/W1b-2 alternatives and may enter only after W1b-2
   records measured CSS BLOCKED/REJECTED evidence (`p3b-wave-sequencing.md:104`-`110`).
   P3-C forbids hidden same-redress fallback (`p3c-falsifiability-gates.md:258`-`264`),
   and P3-E records the same W1b-1/W1b-2 ledger split (`p3e-preblocked-ledger.md:75`-`76`).

## Required Fixes

None.

## CH3 Result

PASS for CH3 regression / REDRESS.
