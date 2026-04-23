# Q0 — tape abrogation / direct-to-struct sequencing

**Status**: resolved
**Owner tranche**: AZ (new)
**Decision date**: 2026-04-23
**Affects**: AY-II, AZ, BA, BB; retires BC letter

## Context

Tranches AY-I and AY-II were scoped on the assumption that the tape substrate
(the flat, append-only payload ring introduced during AO/AP) would remain the
load-bearing data structure all the way through typed-materialization parity.
A subsequent review surfaced a stronger target: rather than emit tape records
and then re-project into typed structs, the emitter can descend directly into
the typed struct tree, collapsing two allocation phases into one and closing
the remaining gap to lightningcss on dense typed grammars.

The question the review posed was sequencing. Option A kept AY-II as the
landing site for direct-to-struct and accepted that it would balloon. Option
B split direct-to-struct out into a fresh tranche before opening BA. Option
C — the one adopted — holds AY-II to its original charter (perf gap recovery
on the existing tape substrate) and creates a new tranche AZ that absorbs
direct-to-struct plus the full dissolution of the tape, with later-letter
tranches shifting down.

The matter is not purely cosmetic. If AY-II were stretched to hold
direct-to-struct, two unrelated risks would braid: the perf recovery work
needs comparable benchmarks against today's tape, while direct-to-struct
deletes today's tape, and the comparison baseline would vanish mid-tranche.

## Decision

**Shape C.** AY-II closes as currently scoped, measured against tape-substrate
baselines. Tranche AZ (new letter) absorbs direct-to-struct plus full tape
dissolution. BA (formerly BB) holds pointer queries over the struct tree. BB
(formerly BC) holds e-graph rule inference. The BC letter is retired.

## Reasoning

Holding AY-II to the tape substrate preserves a clean A/B comparison: AY-II
demonstrates how much of the lightningcss gap is recoverable without
abrogating the substrate, and AZ then demonstrates the incremental delta
from abrogation. Mixing the two collapses the ability to attribute wins.

Creating AZ rather than appending to AY-II honors the new-tranche-new-doc
discipline: the scope pivot is real (substrate change, not a deeper AY-II
subquestion), and a new letter keeps the planning surface legible.

Retiring BC and re-lettering is cheap today — no code references the later
letters — and avoids an irregular gap in the tranche sequence.

The cost: one additional tranche letter in flight, and the later tranches
lose one letter of headroom. Neither is load-bearing. Re-lettering is a
one-time tax paid now rather than carried as "BA (née BB)" shorthand.

## Resolution mechanism

1. Open `docs/tranches/AZ/AZ.md` at AY-II close. Charter: direct-to-struct
   emission, StructRegistry closure, sidecar-pointer micro-bench, derive
   cache migration (see Q7), classifier unification research (see Q9).
2. Rename BA-draft planning notes to BA (pointer queries over struct tree),
   BB-draft to BB (e-graph rule inference). Delete BC-draft.
3. AY-II bench narrative explicitly notes "tape-substrate baseline; AZ
   replaces substrate."
4. Memory entry `project_ay_tranche.md` updated to reflect Shape C.

## Follow-up gate

AZ opening is gated on AY-II closing with tape-substrate bench numbers
committed. If AY-II fails to close the gap enough to justify the tape, the
AZ plan must still land direct-to-struct — the perf motivation for
abrogation does not depend on AY-II outcome.

## References

- `docs/tranches/AY-II.md` (current scope)
- `docs/tranches/AZ/AZ.md` (to be authored at AY-II close)
- Feedback memory: `feedback_new_tranche_new_doc.md`, `feedback_no_workarounds_arch.md`
