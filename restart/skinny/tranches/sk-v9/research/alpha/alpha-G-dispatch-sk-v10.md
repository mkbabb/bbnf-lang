# Alpha-G Dispatch: SK-V9 -> SK-V10 Resequence

Date: 2026-05-19.

Status: DISPATCHABLE. User/orchestrator accepted the REDRESS 98 conclusion:
retire W3, resequence, and dispatch Pass Alpha. This artefact is the handoff
stub for `dispatch alpha SK-V9 -> SK-V10`; the six Alpha agents must write the
new SK-V10 packet under `restart/skinny/tranches/sk-v10/`.

## Trigger

SK-V9 no longer waits on W3. REDRESS 96 implemented the full class-column
union substrate and REDRESS 97 implemented the allocation-free streaming cursor;
both were correctness-green and both missed every W3 must-improve floor plus
every W10b maintain floor. W3 CHALLENGE V4 rejected the remaining
class-lane-only route as non-admissible. REDRESS 98 retires
`G-W3-UNION-SUBSTRATE` as falsified.

## Required Inputs

- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 94-98
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/SPEC.md`
- `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-research.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-plan.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-research-v2.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v2.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v3.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-research-v3.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v3.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v4.md`
- `/tmp/skv9-waveW3-rejected.patch`
- `/tmp/skv9-waveW3-v2-rejected.patch`

## Alpha Scope

Alpha-A results extraction:
Record the current SK-V9 result table exactly. Preserve the W1 typed product
wins: `apache_builds/real_typed_struct` 8174 Mbps versus sonic typed-strict
8110 Mbps, and `citm_catalog/real_typed_struct` 35102 Mbps versus sonic
typed-strict 22058 Mbps. Record parse_only as `S / NO-GO` substrate-guard
evidence, not as a current SOTA scoreboard.

Alpha-B competitor deltas:
Compute strict-vs-strict typed-plane deltas first. The live SOTA-bearing fact
is typed product, not parse_only. Sidecar comparator rows remain planning or
freshness evidence unless the SK-V10 contract creates a measured same-run
strict gate.

Alpha-C REDRESS digest:
Promote REDRESS 98 to a hard pre-block. Do not reopen W3 by renaming the
substrate, splitting the class column, or moving the same thesis under a new
gate. Carry REDRESS 96 and REDRESS 97 as the measurement evidence, not as
implementation bugs to repair.

Alpha-D validated/invalidated ledger:
Validate the typed measured-row path and the retained grammar proof. Invalidate
the substrate-ceiling thesis for this host. Demote W4 cascade-lock assumptions
to historical context.

Alpha-E candidate shortlist:
Prioritize typed-plane generalization candidates such as `github_events`,
`gsoc-2018`, and `instruments`, each with fresh strict-vs-strict product gates.
Carry existing-substrate unicode/string candidates only if they name current
offset-tape, string-scanner, or unicode-unescape call sites with scalar
reference, checkasm where applicable, same-wave consumer, and W10b maintain
gates.

Alpha-F contract draft:
Draft `restart/skinny/tranches/sk-v10/SYNTHESIS.md` and
`restart/skinny/tranches/sk-v10/HANDOFF.md` with no W3 gate, no parse_only
SOTA close condition, and no W4 cascade-lock. Downstream S-P3 may author a new
SPEC only after the SK-V10 Alpha goalset converges.

## Non-Negotiables

- Do not force W3 a fourth time under `G-W3-UNION-SUBSTRATE` or a renamed
  equivalent.
- Do not restore the W3 cascade-lock as a W4 entry gate.
- Do not score parse_only rows as SOTA admissions while they remain
  `S / NO-GO`.
- Do not admit Canada typed rows by analogy to Apache/CITM.
- Do not dispatch W4 source work until a fresh existing-substrate gate is
  written and challenged.

## Dispatch

Dispatch Pass Alpha per `restart/prompts/pass-contracts/PASS-ALPHA.md`:

```text
dispatch alpha SK-V9 -> SK-V10
```

Outputs must land under `restart/skinny/tranches/sk-v10/`. `G-ALPHA-SK-V10`
does not close until the Alpha cohort and challenge converge and the user signs
off.
