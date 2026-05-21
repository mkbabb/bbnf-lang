# SK-V13 S-P3 V1 CH6 Anti-Paper-Close Challenge

Lens: CH6 anti-paper-close.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## Verdict

REVISE.

The global anti-paper-close posture is largely correct: the SPEC blocks G-Omega
bypass, forbids future-consumer/support-only language, rejects implementation
blocks as close evidence, and requires strict comparators plus rolling no-demote
checks. The V1 packet still leaves two close leaks: W5-W8 can close as
support/consumer plumbing without a measured row-movement or architectural-block
gate, and several row-family subwave sections omit an explicit same-wave
consumer line even though the dispatch packet requires one.

## Findings

1. Decision/policy waves still admit on support plumbing instead of row movement.
   The CH6 contract asks whether each wave closes on measurement rather than
   "wired" or "integrated" language and whether the same-wave consumer is named
   (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140`). The triumvirate
   contract likewise requires a falsifiability gate, revert protocol, and
   same-wave consumer for every shipped primitive or generated path
   (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177`,
   `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:202`). The SPEC's
   global non-negotiables state the right rule: no producer without a same-wave
   measured consumer, no "wired/integrated" close, and no support-only behavior
   wave (`restart/skinny/tranches/sk-v13/SPEC.md:259`,
   `restart/skinny/tranches/sk-v13/SPEC.md:265`,
   `restart/skinny/tranches/sk-v13/SPEC.md:267`). But W5 can exit on regex
   API tests and IR/pass consumption with JSON/CSS guards maintaining, without a
   strict row threshold or row-move gate
   (`restart/skinny/tranches/sk-v13/SPEC.md:534`). W6 similarly exits on bounded
   e-graph/cost telemetry and guard maintenance
   (`restart/skinny/tranches/sk-v13/SPEC.md:566`). W7 exits on CSP/cascade
   fail-closed behavior and guard maintenance
   (`restart/skinny/tranches/sk-v13/SPEC.md:600`). W8 permits "JSON output
   unchanged or improved" and "at least one touched JSON/CSS row consumes" the
   policy surface, which is a consumer gate but not a measured row-movement gate
   (`restart/skinny/tranches/sk-v13/SPEC.md:637`). This is paper-close risk for
   G2/W5-W7 and generated policy W8.

2. Row-family subwaves need explicit same-wave consumer fields in the SPEC, not
   only an implicit row name. The dispatch prompt requires every wave packet to
   name the same-wave consumer path (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:61`,
   `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:68`). Earlier CSS waves do
   this explicitly, for example W2, W3, and W4 name generated CSS row consumers
   (`restart/skinny/tranches/sk-v13/SPEC.md:455`,
   `restart/skinny/tranches/sk-v13/SPEC.md:481`,
   `restart/skinny/tranches/sk-v13/SPEC.md:509`). W10.N, W11.N, W13, and W14.N
   have row thresholds and revert protocols, but no explicit "Same-wave consumer"
   line in their SPEC sections (`restart/skinny/tranches/sk-v13/SPEC.md:692`,
   `restart/skinny/tranches/sk-v13/SPEC.md:716`,
   `restart/skinny/tranches/sk-v13/SPEC.md:783`,
   `restart/skinny/tranches/sk-v13/SPEC.md:812`). That omission lets an
   implementation prompt rely on "generated row exists" instead of a named
   production caller/hot path.

3. Close, comparator, and demotion rules are acceptable and should be preserved
   during the fold. The SPEC rejects ordinary fixpoint close, historical
   REDRESS-119/120 close authority, and implementation-limited misses
   (`restart/skinny/tranches/sk-v13/SPEC.md:73`,
   `restart/skinny/tranches/sk-v13/SPEC.md:118`). It requires strict same-plane
   comparator evidence and rejects stale/permissive/report-only admission paths
   (`restart/skinny/tranches/sk-v13/SPEC.md:81`,
   `restart/skinny/tranches/sk-v13/SPEC.md:87`). It also blocks silent demotion
   at global close and rolling-delta levels
   (`restart/skinny/tranches/sk-v13/SPEC.md:69`,
   `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:363`,
   `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:293`).

4. G-Omega bypass is blocked. The dispatch prompt forbids W0 or later
   implementation waves until G-Omega is closed and S-P3 has converged or been
   explicitly pinned (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:32`).
   SPEC Section 21 repeats that only planning/research and read-only inspection
   are allowed until both gates close
   (`restart/skinny/tranches/sk-v13/SPEC.md:892`,
   `restart/skinny/tranches/sk-v13/SPEC.md:903`). CH6 does not require a fold on
   this point.

## Required Fold Items

1. Add a per-wave row-movement or architectural-block exit clause to W5, W6, W7,
   and W8. For W5-W7, the resolver/cost/cascade artifacts must be consumed by a
   named generated selection path and either move at least one JSON or CSS row by
   the P3-C `row_move_toward_sota` rule or record a measured architectural block.
   For W8, "unchanged or improved" is insufficient; the policy/sink/flag surface
   must move a named row, admit a row, or block architecturally.

2. Add explicit `Same-wave consumer:` lines to W10.N, W11.N, W13, and W14.N in
   the SPEC. Each line should name the production caller or generated row path
   that exercises the feature, direct sink, typed product, or parse path in the
   same redress commit.

3. Mirror those two changes in `DISPATCH-PROMPT.md` so implementation agents
   cannot soften W5-W8 into support-only landings or dispatch W10.N/W11.N/W13/W14.N
   without a named consumer.

4. Preserve the existing no-G-Omega-bypass, strict comparator, no-fixpoint-close,
   implementation-block-does-not-close, rollback, and no-silent-demotion language.

## Evidence

- Read `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`,
  `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`,
  `restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md` through
  `p3f-spec-draft.md`, `restart/skinny/tranches/sk-v13/SPEC.md`, and
  `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md`.
- Local check requested by dispatch:
  `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH6.md`
  (PASS).
