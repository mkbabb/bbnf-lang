# Q9 — classifier collision under multi-payload activation

**Status**: resolved, research artefact scoped into AZ.W0
**Owner tranche**: AZ.W0 (research); AZ body (implementation if tractable) or AZ-open re-plan trigger (if not)
**Decision date**: 2026-04-23
**Affects**: AZ, classifier subsystem, payload dispatch

## Context

The classifier is the dispatch layer that reads a tape record's
discriminator and routes to the matching payload shape. Under a
single-payload regime per rule it is essentially a match on a small
integer. Under AZ's multi-payload activation — where a single rule
may emit into more than one payload shape depending on its
projection — the classifier grows a second axis: which shape among a
set, selected by context.

A naive extension of the current classifier to two axes risks
collisions: two rules whose discriminator tuples differ only in a
field the classifier does not key on would dispatch to the wrong
payload. The symptoms are plausible ambiguity (correct output on
common inputs, wrong output on edge cases) and the debugging cost
scales with how late the collision is caught.

The question was whether to address the classifier rework proactively
as part of AZ planning or to defer to a reactive sub-wave that opens
only if collisions are observed in practice.

## Decision

**Front-load research in AZ.W0.** AZ authors a research artefact,
`docs/tranches/AZ/CLASSIFIER-UNIFICATION.md`, that characterizes the
full classifier state space under multi-payload activation and
identifies the collision surface. If the research reveals classifier
unification is a deeper refactor than AZ can carry, that is a re-plan
trigger at AZ opening — not a mid-AZ reactive sub-wave.

## Reasoning

A reactive sub-wave was explicitly rejected as a recipe for
divergence. The failure mode of reactive sub-waves is well-attested:
the sub-wave opens under pressure of a specific symptom, is scoped
to that symptom, and either misses the broader class of collisions or
inflates its scope mid-flight. Either outcome damages tranche
integrity.

Front-loading research catches the scope question before AZ planning
commits to a body scope that cannot carry the refactor. The artefact
is a small cost — a focused research doc, not a full implementation —
and pays off in planning accuracy.

The re-plan trigger is the explicit escape valve. If the research
reveals the refactor is genuinely too large for AZ, the honest response
is to say so at AZ opening, re-plan AZ, and schedule the classifier
work appropriately. That honest response is only available if the
research happens first.

Deferring the research entirely was rejected on the same grounds as
deferring the dispatch itself — the collision class grows with every
multi-payload rule shipped, and discovery cost grows with it.

## Resolution mechanism

1. AZ.W0 adds `docs/tranches/AZ/CLASSIFIER-UNIFICATION.md` to the
   wave's deliverables list.
2. Research agent for AZ.W0 enumerates current classifier state
   space, projects forward under multi-payload activation, and
   identifies every tuple shape where two rules could collide.
3. Artefact concludes with a scope recommendation: (a) tractable
   within AZ body, (b) requires AZ re-plan, (c) requires a
   classifier-dedicated tranche.
4. If (a), implementation folds into AZ's normal wave structure. If
   (b) or (c), AZ opening triggers re-plan.

## Follow-up gate

AZ opening checklist includes "classifier unification artefact
authored and scoped." Without it, AZ does not open. Post-AZ, if
multi-payload collisions surface in testing, the artefact is revisited
rather than re-discovered.

## References

- `docs/tranches/AZ/CLASSIFIER-UNIFICATION.md` (to be authored in AZ.W0)
- Q0 Shape C re-sequence: `00-tape-abrogation-shape-c.md`
- Feedback: `feedback_no_orthogonal_codepaths.md`,
  `feedback_execute_planned_architecture.md`
