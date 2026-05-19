# SK-V10 S-P3 V2 CH3: Regression And REDRESS

Verdict: ACCEPT.
Acceptance: 95%.
Date: 2026-05-19.

Scope: audit whether the V2 folded contract preserves the REDRESS-safe posture
for REDRESS 28/33, 50-55, 60-72, 80, 82-84, 88, 89, and 96-98; keeps material
differentials required; keeps W10b, direct, and typed maintain floors binding;
and ensures the narrowed W9 route does not weaken regression gates.

## Reviewed

- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p3/hardening/V1/CH3-regression-redress.md`
- `skinny/REDRESS.md`

## Findings

1. The V2 fold preserves the global CH3 contract. ACCEPT.

   The SPEC keeps W3 retired at the top of the contract and forbids reopening it
   by rename (`restart/skinny/tranches/sk-v10/SPEC.md:10-13`). It also binds
   direct row movement to same-run strict direct comparator evidence, generated
   Track 1, independent Track 2/oracle, matching output plane, validation path,
   gate consumption, and `ceil(sonic_direct / 1.10)` floors
   (`restart/skinny/tranches/sk-v10/SPEC.md:35-39`). Typed row movement remains
   restricted to generated/serde_json/sonic-rs/independent checksum parity over
   the full fixture with same-run typed comparator rows, and digest evidence
   still cannot admit typed product rows
   (`restart/skinny/tranches/sk-v10/SPEC.md:40-42`). Failed waves must record
   measured REDRESS evidence instead of closing on promises
   (`restart/skinny/tranches/sk-v10/SPEC.md:155-156`).

2. Material differentials remain mandatory for the requested REDRESS families.
   ACCEPT.

   The binding ledger defines a material differential as the new fact that makes
   a future route different from a rejected route, and explicitly says a rename,
   narrower prose, or different helper name is insufficient
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:36-45`).
   The SPEC incorporates that requirement for any route adjacent to REDRESS
   28/33, 50-55, 60-72, 80, 82-84, 88, 89, or 96-98
   (`restart/skinny/tranches/sk-v10/SPEC.md:766-767`), and the dispatch prompt
   repeats it as a pre-implementation obligation
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:195-197`).

3. W3/REDRESS 96-98 remain closed, and W3 is now only a firewall. ACCEPT.

   REDRESS 96 and 97 each falsified the W3 implementation shape against every W3
   must-improve and W10b maintain row (`skinny/REDRESS.md:2823-2837`,
   `skinny/REDRESS.md:2881-2895`), and REDRESS 98 retires the union-substrate
   thesis rather than merely blocking one patch (`skinny/REDRESS.md:2910-2949`).
   The V2 SPEC makes W3 a no-source firewall whose exit gate audits for union
   substrate, class column, streaming cursor, `UnionTape`, and W4 cascade-lock
   aliases, with no row movement (`restart/skinny/tranches/sk-v10/SPEC.md:360-394`).

4. Direct-side REDRESS 50-55 and 66-69 remain blocked without weakening direct
   floors. ACCEPT.

   W1 is contract-only and blocks direct-vs-typed relabeling, REDRESS 93,
   REDRESS 73, REDRESS 50-55, and REDRESS 66-69 absent a new direct-contract
   material differential (`restart/skinny/tranches/sk-v10/SPEC.md:274-317`).
   W2 is zero behavior source and moves only direct rows that pass both generated
   Track 1 and independent Track 2/oracle direct floors, while preserving direct
   guard floors and typed guard floors when refreshed
   (`restart/skinny/tranches/sk-v10/SPEC.md:319-358`). W10 retains the same direct
   posture: at most three selected direct rows, same-run strict direct comparator
   plane, Track 2 independence, direct guard floors, typed guard floors when
   refreshed, and no REDRESS 73, REDRESS 93, W3, sidecar, scratch, or parse-only
   reopen (`restart/skinny/tranches/sk-v10/SPEC.md:678-716`).

5. Typed product floors remain binding and direct evidence cannot cross planes.
   ACCEPT.

   The V2 fold preserves the six existing typed maintain floors in Section 0.2
   (`restart/skinny/tranches/sk-v10/SPEC.md:99-108`). W4 requires generated
   Track 1, independent Track 2/oracle, serde_json typed, sonic-rs typed,
   full-fixture checksum parity, `ceil(same-run sonic_typed / 1.10)`, Track 2
   independence, existing typed maintain floors, and keeps `instruments` direct
   digest evidence direct-plane only (`restart/skinny/tranches/sk-v10/SPEC.md:411-439`).
   W6 repeats the same typed comparator/oracle/parity requirements for root typed
   row admission and keeps Canada typed blocked
   (`restart/skinny/tranches/sk-v10/SPEC.md:501-526`).

6. W10b maintain floors remain load-bearing for SIMD/string/unescape production.
   ACCEPT.

   The W10b maintain block is explicitly bound to any aarch64 SIMD, string,
   unescape, number, whitespace, byte-class, movemask, or parse-loop production
   wiring (`restart/skinny/tranches/sk-v10/SPEC.md:110-122`). The dispatch prompt
   repeats that these floors bind the same production classes
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:145-151`). W9, the only
   production kernel wave, requires all W10b maintain floors to hold
   (`restart/skinny/tranches/sk-v10/SPEC.md:659-668`), so REDRESS 88/89-style
   default PMULL/CTZ rewires cannot slip through on primitive correctness alone.

7. W7/W8/W9 keep REDRESS 28/33, 60-72, 80, 82-84, 88, and 89 fenced. ACCEPT.

   The pre-block ledger names the requested families: retained tiny-string NEON
   wiring is not the parse fix; retained/direct string and materialization routes
   are mostly rejected with only narrow non-global admissions; REDRESS 80 numeric
   work needs a fresh hot leaf and generated direct/typed consumer; REDRESS
   82-84 remain blocked on current baselines; PMULL and CTZ defaults remain
   blocked (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:254-265`).
   W7 is proof-only and explicitly blocks REDRESS 28/33, REDRESS 60-62, REDRESS
   72 global cap-16 policy, and parse-only SOTA admission
   (`restart/skinny/tranches/sk-v10/SPEC.md:528-574`). W8 is proof-only and
   blocks REDRESS 64, REDRESS 66-69, REDRESS 82, REDRESS 83, and PMULL/CTZ
   defaults (`restart/skinny/tranches/sk-v10/SPEC.md:576-625`). W9 cannot be an
   orphan primitive or W3 consumer and must keep parse-only rows `S / NO-GO`
   (`restart/skinny/tranches/sk-v10/SPEC.md:627-676`).

8. W9 narrowing strengthens rather than weakens regression gates. ACCEPT.

   V1 CH3 accepted the W9 safety posture, while CH1 asked that W9 narrow from
   loosely proven `C4`-`C9` language to actually proved candidates. V2 does that:
   the manifest limits W9 to proven `C4`-`C7`, and states that `C8` digit/number
   and `C9` whitespace/class work cannot feed W9 without a future SPEC/CHALLENGE
   amendment (`restart/skinny/tranches/sk-v10/SPEC.md:171-174`,
   `restart/skinny/tranches/sk-v10/SPEC.md:188-191`). The narrowing does not
   remove any row gate: W9 still requires scalar fallback and differential parity,
   same-commit production caller, Section 2.1 for generic/codegen/runtime edits,
   direct or typed row floors, Track 2 independence for row movement, W10b
   maintain floors, and parse-only `S / NO-GO` preservation
   (`restart/skinny/tranches/sk-v10/SPEC.md:659-668`).

## Required Fixes

None.

## Hygiene Notes

- The SPEC's W7/W8 pre-block text relies on the binding Section 15 and P3-E
  ledger for REDRESS 80 and 84 rather than repeating those item numbers in each
  wave. This is acceptable because the ledger is explicitly binding and the SPEC
  says it tightens but does not loosen it
  (`restart/skinny/tranches/sk-v10/SPEC.md:748-767`).
- Future W9 plans should paste the P3-E material-differential checklist for the
  touched REDRESS family before implementation, especially if a numeric or
  whitespace candidate is proposed through a future amendment.

## Verdict

ACCEPT. The V2 folded contract preserves the REDRESS-safe posture. It does not
reopen REDRESS 28/33, 50-55, 60-72, 80, 82-84, 88, 89, or 96-98 routes; material
differentials remain required; W10b, direct, and typed maintain floors remain
binding; and W9 narrowing removes unproved candidates without weakening the
regression gates.
