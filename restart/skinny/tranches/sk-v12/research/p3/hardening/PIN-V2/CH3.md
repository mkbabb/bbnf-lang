# SK-V12 S-P3 PIN-V2 CH3 Regression / REDRESS Challenge

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: PIN-V2.
Lens: CH3 regression, JSON guard floors, REDRESS material differential, FIXPOINT evidence, revert protocol, and stale measurement prevention.
Date: 2026-05-20.
Packet under review: commit `7316d87b`.
Output: this file.

## Disposition

PASS.

Confidence: 96%.

The PIN-V2 packet folds the PIN-V1 CH3 regressions. JSON direct and typed guard
floors are explicit and demotion is measured-only. REDRESS 96/97/98 and 88/89/90
are treated as historical material-differential evidence rather than category
blocks. REDRESS 111-120 remain blocked/routed unless a new material source delta,
CHALLENGE, and measurement exist. ADMIT is reserved for CSS L4 Track 1 strictly
greater than `lightningcss_mbps + 1`; W3/W4 non-close successes are evidence, not
paper ADMIT. FIXPOINT requires measured CSS, union, ASM-gen, zero-orphan, and
REDRESS evidence. Stale run ids, stale witnesses, producer-only telemetry,
missing comparator artifacts, and guard misses without measured REDRESS fail
closed.

## Findings

1. PASS - JSON guard floors and demotion rules are binding and regression-safe.

   SPEC makes JSON direct/typed guard floors an ADMIT requirement unless a miss is
   recorded as a measured REDRESS demotion, and keeps `parse_only` diagnostic-only
   (`SPEC.md:61`-`62`). The concrete guard floor tables are present for direct
   rows and typed rows (`SPEC.md:187`-`206`), and any behavior wave touching JSON
   production-capable paths must rerun guards or prove no JSON path and
   `skinny/RESULTS.md` movement (`SPEC.md:208`-`211`). P3-C repeats the same
   demotion-only rule and lists the direct/typed floors (`p3c-falsifiability-gates.md:94`-`127`).
   P3-D requires all guard rows, a guard run id, a floor source, and row-to-REDRESS
   demotion evidence for misses (`p3d-telemetry-schema.md:210`-`214`). The seed
   result surface still renders `parse_only` as NO-GO and direct/typed rows as
   guard surfaces (`skinny/RESULTS.md:5`-`31`, `skinny/RESULTS.md:143`-`145`).

2. PASS - REDRESS 96/97/98 and 88/89/90 are reopened only at category level, with
   material differentials required for new attempts.

   The user pin states that union REDRESS 96/97/98 remain measured-rejected
   implementations and that new attempts must cite them, name the material
   differential, and pass CHALLENGE (`USER-PIN-W1-CSS-L4-SOTA.md:50`-`56`).
   It likewise rescinds ASM-gen blocks only at category level while preserving
   the historical rejected implementations (`USER-PIN-W1-CSS-L4-SOTA.md:60`-`69`).
   SPEC binds those requirements into FIXPOINT and wave entry (`SPEC.md:72`-`79`,
   `SPEC.md:513`-`518`, `SPEC.md:563`-`575`) and into the reopened-route ledger
   (`SPEC.md:657`-`667`). P3-E gives the exact replay blocks and admissible
   framing for W3 and W4 (`p3e-preblocked-ledger.md:55`-`56`,
   `p3e-preblocked-ledger.md:78`-`80`, `p3e-preblocked-ledger.md:139`-`145`).
   The underlying REDRESS history supports that handling: 96 and 97 are measured
   union failures (`skinny/REDRESS.md:2797`-`2848`,
   `skinny/REDRESS.md:2852`-`2906`), 98 retires the old union thesis
   (`skinny/REDRESS.md:2910`-`2950`), 88 rejects PMULL as the default
   prefix-XOR body (`skinny/REDRESS.md:2510`-`2540`), 89 rejects the CTZ/bulk
   consumer (`skinny/REDRESS.md:2544`-`2585`), and 90 admits canary hardening
   only while preserving the bitmap-body rejects (`skinny/REDRESS.md:2589`-`2618`).

3. PASS - REDRESS 111-120 blockers are preserved and cannot be replayed as CSS
   or direct-row admission.

   The pin leaves REDRESS 111 and 114-120 unchanged and only supersedes the
   generated non-JSON blocker by making CSS L4 the explicit mandate
   (`USER-PIN-W1-CSS-L4-SOTA.md:118`-`121`). SPEC still blocks replays of
   REDRESS 111-120 without material differential and CHALLENGE (`SPEC.md:654`),
   while P3-E records REDRESS 111 as report-lane-only, REDRESS 114-120 as
   guard/routed-only, and JSON direct residual movement as non-close evidence
   unless fresh material evidence arrives (`p3e-preblocked-ledger.md:63`-`64`,
   `p3e-preblocked-ledger.md:95`, `p3e-preblocked-ledger.md:126`-`155`).
   The REDRESS entries confirm the blockers: 111 is a non-admitting report lane
   (`skinny/REDRESS.md:3284`-`3308`), 112/113 block generated non-JSON/CSS
   baseline creation (`skinny/REDRESS.md:3313`-`3355`), 114/115 are measured
   direct-row rejects (`skinny/REDRESS.md:3359`-`3409`), 116/117/118 are entry
   blocks (`skinny/REDRESS.md:3413`-`3489`), and 119/120 close SK-V11 as
   fixpoint without direct row or non-JSON admission (`skinny/REDRESS.md:3497`-`3553`).

4. PASS - FIXPOINT evidence requirements are concrete enough to prevent a paper
   close.

   SPEC requires measured CSS redress, ADMIT measured uncloseable, a new measured
   union attempt with REDRESS 96/97/98 differential, a new ASM-gen attempt with
   scalar/checkasm/microbench/same-wave evidence and REDRESS 88/89/90
   differential when adjacent, zero production orphans, and REDRESS for every
   miss (`SPEC.md:66`-`83`). P3-B repeats the close rule and requires measured
   CSS, W3, W4, zero-orphan, and JSON guard evidence before W5 can record
   FIXPOINT (`p3b-wave-sequencing.md:64`-`68`, `p3b-wave-sequencing.md:196`-`199`).
   P3-C explicitly prevents plan-only fixpoint credit for W3 and requires source
   or accepted microbench rejection plus fresh evidence (`p3c-falsifiability-gates.md:339`-`347`);
   W4 likewise counts only with measured scalar/checkasm/microbench/equality
   evidence and no production orphans at close (`p3c-falsifiability-gates.md:397`-`402`).
   W5 close binds the full ADMIT/FIXPOINT evidence list and fails future-phase
   promises (`p3c-falsifiability-gates.md:411`-`450`).

5. PASS - Revert protocols and stale measurement prevention are present in the
   wave gates.

   SPEC has per-wave revert protocols for W0, W1a, W2, W1b-1, W1b-2, W3, W4, and
   W5 (`SPEC.md:312`, `SPEC.md:348`-`349`, `SPEC.md:384`-`385`,
   `SPEC.md:442`-`443`, `SPEC.md:491`-`492`, `SPEC.md:538`-`539`,
   `SPEC.md:599`-`600`, `SPEC.md:637`). P3-C includes the same revert slices and
   explicitly ties W1b-2, W3, and W4 rollback to stale run, guard regression, row
   miss, equality miss, or replay failures (`p3c-falsifiability-gates.md:266`-`269`,
   `p3c-falsifiability-gates.md:348`-`350`,
   `p3c-falsifiability-gates.md:404`-`407`). Stale-measurement prevention is also
   schema-bound: telemetry rejects stale run ids, mixed-run comparators, oracle
   coupling, producer-only fields, and permissive/lossy comparator admission
   (`p3c-falsifiability-gates.md:90`-`92`; `p3d-telemetry-schema.md:76`-`79`,
   `p3d-telemetry-schema.md:257`-`282`). DISPATCH carries the same gate-consumed
   field list and failure conditions (`DISPATCH-PROMPT.md:169`-`181`).

## Required Fixes

None.

## CH3 Result

PASS for CH3 regression / REDRESS.
