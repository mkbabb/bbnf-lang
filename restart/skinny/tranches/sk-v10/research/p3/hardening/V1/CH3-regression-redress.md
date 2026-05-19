# SK-V10 S-P3 V1 CH3: Regression And REDRESS

Verdict: ACCEPT.
Acceptance: 92%.
Date: 2026-05-19.

Scope: audit the S-P3 V1 SK-V10 wave plan for regression risk against
`skinny/REDRESS.md`, with emphasis on REDRESS 28/33, 50-55, 60-72, 80,
82-84, 88, 89, and 96-98. This review checks whether every REDRESS-adjacent
wave carries a material differential, revert protocol, maintain floors where
behavior can affect measured rows, and a refusal for reopened rejected routes.

## Reviewed

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v10/SPEC.md`
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH3-regression-redress.md`
- `skinny/REDRESS.md`

## Findings

1. The final SPEC carries the required global CH3 clamps. ACCEPT.

   PASS-3 requires P3-C to define per-wave corpus thresholds, maintain budgets,
   exit gates, and revert protocols, and requires P3-E to enumerate REDRESS
   routes that waves must not reopen (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:58-63`,
   `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:123-126`). The final SPEC
   binds no behavior source change before owner paths, gates, maintain floors,
   same-wave consumer, hard cap, and revert protocol are named
   (`restart/skinny/tranches/sk-v10/SPEC.md:135-140`), and it requires failed
   waves to record measured REDRESS evidence rather than close on a promise
   (`restart/skinny/tranches/sk-v10/SPEC.md:155-156`). The dispatch prompt
   repeats that any REDRESS-adjacent route must state a material differential and
   that a rename, narrower prose, or helper reshuffle is insufficient
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:174-176`).

2. W1, W2, and W10 do not reopen direct-side rejected routes. ACCEPT.

   The direct waves are adjacent to REDRESS 50-55 sidecar/cursor/materializer
   failures and REDRESS 66-69 direct string/materialization failures. The SPEC
   keeps W1 contract-only, requires gate-json to reject missing output plane,
   comparator plane, strictness, run id, validation path, Track 2 independence,
   or provenance, and forbids row movement (`restart/skinny/tranches/sk-v10/SPEC.md:260-267`).
   Its pre-blocks explicitly keep direct-vs-typed relabeling, REDRESS 93,
   REDRESS 73, REDRESS 50-55, and REDRESS 66-69 blocked
   (`restart/skinny/tranches/sk-v10/SPEC.md:271-274`). W2 is zero behavior
   source and requires selected direct rows, direct guard rows, and typed guard
   rows to satisfy Section 0.2 floors before any report movement
   (`restart/skinny/tranches/sk-v10/SPEC.md:276-314`). W10 is behavior-capable,
   but it is limited to one accepted direct mechanism, at most three direct
   target rows, strict direct Track 1/Track 2 floors, guard floors, and an
   explicit no-reopen gate for REDRESS 73, REDRESS 93, W3, sidecar, scratch, and
   parse-only routes (`restart/skinny/tranches/sk-v10/SPEC.md:617-640`).

3. W3 is a firewall, not a reopened W3 implementation route. ACCEPT.

   REDRESS 96 measured the class-column plus move-consumed structural-index W3
   route and failed every W3 must-improve and W10b maintain row
   (`skinny/REDRESS.md:2795-2848`). REDRESS 97 removed the full vector and used a
   streaming cursor, but again failed every W3 and W10b row
   (`skinny/REDRESS.md:2850-2906`). REDRESS 98 retires the union-substrate thesis
   rather than leaving it open for a renamed implementation
   (`skinny/REDRESS.md:2908-2950`). The SK-V10 SPEC adopts that retirement in
   the opening contract (`restart/skinny/tranches/sk-v10/SPEC.md:10-13`) and
   makes W3 a no-source firewall whose exit gate is an audit for W3 aliases,
   parse-only SOTA claims, and W4-through-W3 dependencies
   (`restart/skinny/tranches/sk-v10/SPEC.md:316-350`). No reopened rejected W3
   route is present.

4. W4, W5, and W6 preserve typed-plane boundaries and maintain floors. ACCEPT.

   The typed waves are not direct-digest relabels. W4 admits only
   `instruments/real_typed_struct` with generated Track 1, independent Track
   2/oracle, serde_json typed, sonic-rs typed, full-fixture checksum parity,
   same-run floor, and existing typed maintain floors
   (`restart/skinny/tranches/sk-v10/SPEC.md:367-394`). W5 is proof-only and
   cannot move `RESULTS.md` (`restart/skinny/tranches/sk-v10/SPEC.md:420-434`).
   W6 moves at most one root-unblocked typed row after W5, requires full typed
   comparator/oracle/parity evidence, and keeps existing typed maintain floors
   (`restart/skinny/tranches/sk-v10/SPEC.md:453-477`). This is materially
   different from the rejected hand-authored typed sink route in REDRESS 70,
   while preserving the accepted host/API schema-source direction from REDRESS
   71 (`skinny/REDRESS.md:1888-1940`, `skinny/REDRESS.md:1942-1993`).

5. W7, W8, and W9 correctly fence string, escape, segment, and SIMD routes.
   ACCEPT.

   REDRESS 28/33 reject active retained 16-byte tiny-string NEON wiring as the
   parse fix despite parity-green primitives (`skinny/REDRESS.md:324-337`,
   `skinny/REDRESS.md:394-418`). REDRESS 64, 66-69, 82, 83, 88, and 89 reject
   retained Unicode validation, direct source-hook/scratch/byte-output/semantic
   fact materialization, per-quartet escape replay, StringBlock16 wrapper,
   PMULL default prefix-XOR, and CTZ bulk default routes
   (`skinny/REDRESS.md:1582-1635`, `skinny/REDRESS.md:1686-1886`,
   `skinny/REDRESS.md:2285-2356`, `skinny/REDRESS.md:2508-2585`). W7 and W8 are
   proof-only, require exactly one primitive family, scalar oracle,
   differential/checkasm parity, same-host caller microbench, cap/plane/caller
   naming, and no production wiring or `RESULTS.md` movement
   (`restart/skinny/tranches/sk-v10/SPEC.md:479-558`). W9 is the only production
   wiring wave and requires a W7/W8-proven primitive, same-commit production
   caller, scalar/differential parity, direct or typed row gates, W10b maintain
   floors, and parse-only rows remaining `S / NO-GO`
   (`restart/skinny/tranches/sk-v10/SPEC.md:560-601`). This is a material
   differential from the rejected routes: proof-first, one family, one current
   caller, no default PMULL/CTZ, no orphan primitive, and full maintain floors.

6. The binding pre-block list is complete for the requested REDRESS families.
   ACCEPT.

   P3-E defines the material-differential vocabulary and states that a rename,
   narrower prose, or different helper name is not enough
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:36-45`).
   Its global pre-blocks cover W3/union, W4 cascade-lock, parse-only SOTA,
   sidecar/parallel substrate producers, generic JSON policy leaks,
   direct-vs-typed relabeling, Canada shortcut, PMULL/CTZ defaults, eager
   scratch/materialization replay, and capacity pre-scan as product evidence
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:47-60`).
   The REDRESS family register names every requested family: 28/33, 50-55,
   60-72, 80, 82-84, 88, 89, and 96-98
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:229-240`),
   and the final SPEC incorporates that same hard-block list
   (`restart/skinny/tranches/sk-v10/SPEC.md:672-691`).

7. Intermediate P3-E wave labels are stale relative to the final SPEC, but the
   final contract corrects them. ACCEPT WITH HYGIENE NOTE.

   P3-E's per-wave sections use an earlier grouping where W2 is
   `instruments`, W3 is root typing, W4 is kernel work, and W5 is telemetry
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:119-227`).
   P3-F explicitly notes that P3-C and P3-E group gates under W4/W5-style
   headings while the final SPEC should preserve the P3-B W0-W10 topology
   (`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:168-173`).
   The final SPEC and dispatch prompt do preserve the corrected topology
   (`restart/skinny/tranches/sk-v10/SPEC.md:160-175`,
   `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:42-57`). This is citation
   hygiene, not a CH3 regression, because Section 15 and the wave sections bind
   the same pre-blocks under the final wave numbers.

## Required Fixes

None required for CH3 acceptance.

## Non-Blocking Follow-Ups

- If P3-E is edited in a later docs pass, align its per-wave headings to the
  final SPEC's W0-W10 names to reduce dispatch-reader ambiguity.
- Require each future wave plan to paste the P3-E material differential
  checklist verbatim when it touches REDRESS 28/33, 50-55, 60-72, 80, 82-84,
  88, 89, or 96-98. The final SPEC already requires this before implementation
  (`restart/skinny/tranches/sk-v10/SPEC.md:690-691`).

## Verdict

ACCEPT. The S-P3 V1 SK-V10 packet is regression-safe under the CH3 lens. The
binding SPEC and dispatch prompt require material differential, revert protocol,
maintain floors where behavior can touch measured rows, and explicit pre-blocks
for the requested REDRESS families. No reopened rejected route is present.
