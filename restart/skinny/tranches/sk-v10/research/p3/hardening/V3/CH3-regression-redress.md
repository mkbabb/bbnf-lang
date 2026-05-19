# SK-V10 S-P3 V3 CH3: Regression And REDRESS

Verdict: ACCEPT.
Acceptance: 96%.
Date: 2026-05-19.

Scope: V3 confirmation of the V2-accepted CH3 regression/REDRESS posture. This
checks for regression from V2 acceptance on REDRESS pre-blocks, material
differential requirements, W10b/direct/typed maintain floors, W3 retirement, and
W9 narrowing.

## Reviewed

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH3-regression-redress.md`

## Findings

1. No regression from V2 acceptance. ACCEPT.

   V2 accepted CH3 at 95% with no required fixes
   (`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:15-18`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH3-regression-redress.md:3-10`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH3-regression-redress.md:130-132`).
   The live V2 challenge-folded SPEC still states that REDRESS 96-98 retire W3
   and that no SK-V10 wave may reopen it by rename
   (`restart/skinny/tranches/sk-v10/SPEC.md:10-13`). The dispatch prompt still
   carries W3 reopen failure as load-bearing dispatch fact
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:176-178`).

2. REDRESS pre-blocks remain binding, not advisory. ACCEPT.

   The P3-E ledger is still negative authority: later SPEC or dispatch text may
   tighten it but may not loosen it without fresh CHALLENGE disposition
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:19-23`).
   The live SPEC incorporates that ledger as binding negative authority and lists
   the hard blocks for W3/union, W4-through-W3, parse-only SOTA, sidecar
   producers, generic JSON leaks, direct-vs-typed relabeling, Canada typed
   shortcut, PMULL/CTZ defaults, eager scratch/materialization replay, and
   capacity pre-scan product evidence
   (`restart/skinny/tranches/sk-v10/SPEC.md:748-767`). The dispatch prompt
   repeats the same pre-block list and requires REDRESS-adjacent routes to state
   the material differential before implementation
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:179-199`).

3. The material differential requirement is preserved for the requested REDRESS
   families. ACCEPT.

   P3-E defines a material differential as the new fact that makes a later route
   different from a REDRESS-rejected route, and explicitly rejects rename,
   narrower prose, or helper-name changes as sufficient
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:36-45`).
   Its checklist remains complete: specific REDRESS rows, rejected mechanism, new
   mechanism, consumer plane and call site, oracle, same-host bench or Criterion
   rows, failure threshold and revert protocol, non-interchangeability across
   direct/typed/retained/Track 2 evidence, generic-proof need, and failure
   REDRESS entry
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:267-284`).
   The SPEC binds this to REDRESS 28/33, 50-55, 60-72, 80, 82-84, 88, 89, and
   96-98 (`restart/skinny/tranches/sk-v10/SPEC.md:766-767`).

4. W10b, direct, and typed maintain floors remain load-bearing. ACCEPT.

   Direct row movement still requires same-run strict direct comparator evidence,
   generated Track 1, independent Track 2/oracle, matching output plane,
   validation path, gate consumption, and both tracks meeting
   `ceil(sonic_direct / 1.10)` (`restart/skinny/tranches/sk-v10/SPEC.md:35-39`).
   The direct floor table, direct maintain floors, typed maintain floors, and
   W10b maintain block are all still in SPEC Section 0.2
   (`restart/skinny/tranches/sk-v10/SPEC.md:67-122`). The dispatch prompt repeats
   that direct, typed, and W10b floors are load-bearing and says W10b binds any
   aarch64 SIMD/string/unescape/number/whitespace/byte-class/movemask/parse-loop
   production wiring
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:147-157`).

5. W3 retirement remains explicit and operational. ACCEPT.

   The manifest makes W3 a firewall with no row movement
   (`restart/skinny/tranches/sk-v10/SPEC.md:167-174`) and the manifest rules state
   it is not the retired W3 implementation route
   (`restart/skinny/tranches/sk-v10/SPEC.md:177-191`). The W3 section is
   governance/proof-only, audits for W3 aliases, parse-only SOTA claims, and
   W4-through-W3 dependencies, and exits only if no live route through W3
   union/event substrate, class column, streaming cursor, `UnionTape`, or W4
   cascade-lock exists
   (`restart/skinny/tranches/sk-v10/SPEC.md:360-394`).

6. W9 narrowing is preserved and does not weaken regression gates. ACCEPT.

   The manifest limits W9 to proven `C4`-`C7`, while `C8` digit/number and `C9`
   whitespace/class work cannot feed W9 without a future SPEC/CHALLENGE amendment
   (`restart/skinny/tranches/sk-v10/SPEC.md:188-191`). W9 production still
   requires an accepted W7/W8 proof for the exact primitive and caller, production
   caller consumption in the same commit, scalar fallback and differential parity,
   Section 2.1 for generic/codegen/runtime edits, direct or typed row gates,
   Track 2/oracle independence, W10b maintain floors, and parse-only preservation
   as `S / NO-GO` (`restart/skinny/tranches/sk-v10/SPEC.md:627-676`). The
   dispatch prompt independently limits W9 to one proven primitive, one existing
   production caller, one consumer plane, and one row-moving target set
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:163-166`).

## Required Fixes

None.

## Verdict

ACCEPT. V3 confirms no regression from V2 CH3 acceptance. REDRESS pre-blocks
remain binding; material differentials remain mandatory for REDRESS 28/33,
50-55, 60-72, 80, 82-84, 88, 89, and 96-98; W10b/direct/typed maintain floors
remain load-bearing; W3 remains retired as an implementation route; and W9
narrowing removes unproved C8/C9 production paths without loosening regression
gates.
