# SK-V12 S-P3 PIN-V3 CH3 Regression / REDRESS Challenge

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: PIN-V3.
Lens: CH3 regression, JSON guard floors, REDRESS material differential,
FIXPOINT evidence, revert protocol, stale measurement prevention, and
W1b-1/W1b-2 fallback correction.
Date: 2026-05-20.
Packet under review: commit `4c53119f`.
Output: this file.

## Disposition

PASS.

Confidence: 97%.

The PIN-V3 packet preserves the PIN-V2 CH3 regression controls and folds the
one W1b fallback ambiguity found by the prior challenge. JSON direct and typed
guard floors remain binding with measured REDRESS demotion as the only allowed
miss path. REDRESS 96/97/98 and 88/89/90 remain material-differential history,
not implementation authority. REDRESS 111-120 remain fenced from CSS or direct
row admission unless a later wave supplies fresh material evidence, CHALLENGE,
measurement, and gate consumption. FIXPOINT requires measured CSS, union,
ASM-gen, zero-orphan, and REDRESS evidence. Stale runs, producer-only telemetry,
missing comparator artifacts, unresolved Lock 14/16, orphan production SIMD, and
guard misses without measured REDRESS fail closed.

## Findings

1. PASS - JSON guard floors and measured-demotion rules remain regression-safe.

   SPEC requires JSON direct and typed guard floors to hold, or a miss is recorded
   as a measured REDRESS demotion, while keeping `parse_only` diagnostic-only
   (`SPEC.md:61`-`64`). It carries the concrete direct and typed guard floor
   tables (`SPEC.md:187`-`206`) and requires any JSON-producing behavior wave to
   rerun guards or prove no JSON path and `skinny/RESULTS.md` movement
   (`SPEC.md:208`-`211`). P3-C repeats the guard floor tables and inherited
   result shape (`p3c-falsifiability-gates.md:94`-`127`). P3-D makes guard state,
   full guard rows, floor source, and REDRESS-demotion evidence required telemetry
   (`p3d-telemetry-schema.md:200`-`218`). The live result surface still renders
   parse-only as NO-GO and the JSON rows as guard surfaces (`skinny/RESULTS.md:5`,
   `skinny/RESULTS.md:143`-`145`).

2. PASS - REDRESS material differentials are preserved for union and ASM-gen.

   The user pin reopens union and ASM-gen only at category level while preserving
   REDRESS 96/97/98 and 88/89/90 as historical measured implementations requiring
   citation, material differential, and CHALLENGE (`USER-PIN-W1-CSS-L4-SOTA.md:50`-`69`).
   SPEC binds that into FIXPOINT and the reopened route ledger (`SPEC.md:72`-`79`,
   `SPEC.md:661`-`669`). W3 requires W1b-2 CSS evidence, a fresh hot leaf, and
   CHALLENGE acceptance of the REDRESS 96/97/98 differential (`SPEC.md:513`-`520`);
   W4 requires REDRESS adjacency/cost acceptance, scalar/checkasm/microbench, a
   same-wave consumer, and orphan disposition (`SPEC.md:563`-`588`). P3-E records
   the same per-wave replay blocks and admissible framing (`p3e-preblocked-ledger.md:75`-`80`,
   `p3e-preblocked-ledger.md:139`-`145`). The underlying REDRESS history supports
   this treatment for 96/97/98 (`skinny/REDRESS.md:2797`-`2950`) and 88/89/90
   (`skinny/REDRESS.md:2510`-`2618`).

3. PASS - REDRESS 111-120 remain blockers/routed evidence, not admission.

   The pin leaves REDRESS 111 and 114-120 unchanged, and supersedes 112/113 only
   by making CSS L4 the explicit mandate (`USER-PIN-W1-CSS-L4-SOTA.md:118`-`121`).
   SPEC blocks replays of REDRESS 111-120 without material differential and
   CHALLENGE (`SPEC.md:656`). P3-E keeps REDRESS 111 as report-lane-only and JSON
   direct residuals as guard/routed only unless fresh post-CSS evidence exists
   (`p3e-preblocked-ledger.md:63`-`64`, `p3e-preblocked-ledger.md:95`,
   `p3e-preblocked-ledger.md:126`-`155`). The REDRESS entries confirm 111 is a
   non-admitting report lane, 112/113 block the old generated non-JSON baseline
   axis, 114/115 are measured direct rejects, 116/117/118 are entry blocks, and
   119/120 close SK-V11 as fixpoint without direct or non-JSON admission
   (`skinny/REDRESS.md:3284`-`3553`).

4. PASS - FIXPOINT cannot paper-close.

   SPEC requires measured CSS redress, measured ADMIT uncloseability, one new
   measured union attempt with REDRESS 96/97/98 differential, one new measured
   ASM-gen attempt with scalar/checkasm/microbench/same-wave evidence and
   REDRESS 88/89/90 differential when adjacent, zero production orphans, and
   REDRESS for every miss (`SPEC.md:66`-`83`). P3-B repeats the close rule and
   requires measured CSS, W3, W4, zero-orphan, and JSON guard evidence before W5
   records FIXPOINT (`p3b-wave-sequencing.md:64`-`69`,
   `p3b-wave-sequencing.md:196`-`199`). P3-C denies plan-time W3 credit and
   requires source or accepted microbench rejection plus REDRESS evidence
   (`p3c-falsifiability-gates.md:339`-`347`); W4 similarly counts only with
   measured scalar/checkasm/microbench/equality evidence and no production
   orphans (`p3c-falsifiability-gates.md:397`-`402`). W5 close fails on missing
   lightningcss, missing oracle, unresolved Lock 14/16, skipped union/ASM-gen in
   FIXPOINT, or future-phase promises (`p3c-falsifiability-gates.md:411`-`450`).

5. PASS - Revert protocols and stale-measurement prevention are present.

   SPEC has per-wave revert protocols for W0, W1a, W2, W1b-1, W1b-2, W3, W4, and
   W5 (`SPEC.md:312`, `SPEC.md:348`-`349`, `SPEC.md:384`-`385`,
   `SPEC.md:444`-`445`, `SPEC.md:493`-`494`, `SPEC.md:540`-`541`,
   `SPEC.md:601`-`602`, `SPEC.md:639`). P3-C ties W1b-2, W3, and W4 rollback to
   stale run, guard regression, row miss, equality miss, or replay failures
   (`p3c-falsifiability-gates.md:266`-`269`,
   `p3c-falsifiability-gates.md:348`-`350`,
   `p3c-falsifiability-gates.md:404`-`407`). Stale measurement prevention is
   schema-bound: stale run ids, mixed-run comparators, oracle coupling,
   producer-only fields, missing provenance, and permissive/lossy comparators
   reject (`p3c-falsifiability-gates.md:90`-`92`,
   `p3d-telemetry-schema.md:76`-`79`, `p3d-telemetry-schema.md:257`-`282`).
   DISPATCH carries the same gate-consumed telemetry and failure conditions
   (`DISPATCH-PROMPT.md:169`-`183`).

6. PASS - The W1b-1/W1b-2 fallback correction is folded.

   Commit `4c53119f` fixes the prior ambiguity in SPEC: W1b-1 scaffold
   BLOCKED/FAIL records REDRESS and returns to plan, but does not satisfy the
   post-CSS-redress fallback condition; Sheets/BBNF remain blocked until W1b-2
   records measured CSS lightningcss comparator/admission redress unless the user
   re-pins or S-P3 revises the topology (`SPEC.md:433`-`442`). W1b-2 remains the
   first wave whose measured comparator failure can support later fallback
   planning (`SPEC.md:482`-`491`). P3-B mirrors this by saying Sheets/BBNF are
   not W1b-1/W1b-2 alternatives and may enter only after W1b-2 records measured
   CSS BLOCKED/REJECTED evidence (`p3b-wave-sequencing.md:104`-`110`). P3-C
   forbids hidden same-redress fallback and ties JSON fallback consumers to W1b-2
   measured CSS redress (`p3c-falsifiability-gates.md:258`-`264`,
   `p3c-falsifiability-gates.md:330`, `p3c-falsifiability-gates.md:375`). P3-E
   likewise states that W1b-1 cannot record CSS ADMIT and W1b-2 is the measured
   comparator attempt after which fallback can be routed (`p3e-preblocked-ledger.md:75`-`76`).

## Required Fixes

None.

## CH3 Result

PASS for CH3 regression / REDRESS.
