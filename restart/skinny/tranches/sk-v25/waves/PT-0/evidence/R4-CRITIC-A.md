# PT-0 common harness R4 — hostile critic A

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Canonical agent: `/root/g3_r2_critic_a/pt0_r4_critic_a`.

Frozen author record:
`fcb9b81ccc478d6eb7ef95021cb8b9c182c3594c80c6d5b744996e08a224e611`.

Verdict: **REJECT.** R4 earns narrow harness-engineering credit, not a current
or sealed evidence gate.

## Findings

1. **P0 — the bundle is stale by construction.** `docs/tranches/T` is a captured
   operand. The author record was appended there after capture, so current verify
   exits `PT-0 profile receipt drifted`. The current bundle has no current-baseline
   authority.
2. **P1 — the run census is not semantically cross-bound.** Verification checks
   only `run.census.sha256`, not exact equality with `summary(receipt)`. A run with
   empty repository/tool rows and falsified config/profile summaries was accepted
   after updating its seal member hash.
3. **P2 — crash/concurrency claims exceed their tests.** Tests cover callback
   order, in-process competing directory creation, static symlink ancestry, and
   fault injection only after a complete receipt or run. Process-kill, write,
   sync, link, unlink, directory-sync, and ancestry-swap behavior is unproven.
4. **P2 — executable identity under concurrent replacement is unproven.** The
   resolved payload is read, path-executed, then read again. Persistent drift is
   detected, but a replace/execute/restore race is not.
5. **Declared parse-only scope is correct; categorical no-Value-read is
   unproven.** Exact profiles exclude Value, but child tools inherit ambient
   environment without a filesystem read-denial witness. No actual Value read was
   found.

## Accepted facts

- All governed current and historical hashes match; the suite passes 25/25.
- Bundle members, modes, byte counts, and hashes match.
- Exclusive direct-child directory creation, synced no-replace member creation,
  seal-last ordering, exact member enumeration, regular-file checks, separate
  GREEN capture/RED gate fields, and npm omission are real improvements.
- The author correctly claims zero PT-0, product, performance, or movement credit.

Smallest repair: remove or exactly verify duplicated run summaries; freeze every
captured author/design byte before terminal capture; place later review evidence
outside the captured operand; produce a new unique bundle and two current verifies;
add subprocess fault witnesses before claiming crash durability.

No file was edited by the critic.
