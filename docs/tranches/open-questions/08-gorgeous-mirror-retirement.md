# Q8 — gorgeous-mirror retirement

**Status**: resolved and executed
**Owner tranche**: master housekeeping
**Decision date**: 2026-04-23
**Affects**: sibling worktree `~/Programming/gorgeous`

## Context

`gorgeous` existed as a sibling worktree mirroring a subset of the
pretty-printing and formatting logic that now lives inside bbnf-lang's
fused prettify pipeline. The mirror predated fused prettify; at the
time it hosted exploratory formatter variants that were easier to
iterate outside the main crate graph. After fused prettify landed,
gorgeous's scope collapsed to a near-duplicate of in-tree logic, kept
alive mostly for a handful of reference outputs.

The question was whether gorgeous still earned its keep. Keeping it
meant paying the dual-maintenance tax: every formatter change that
landed in bbnf-lang either had to be mirrored to gorgeous or
tolerate drift. Deleting it meant losing whatever reference value the
mirror carried.

The review concluded the reference value was negligible — the
reference outputs are captured in bbnf-lang's own snapshot tests — and
the tax was real. The feedback memo `feedback_doc_alongside_code` and
the instructional directive to avoid duplicate subsystems both
pointed to removal.

## Decision

**Delete.** The sibling `/Users/mkbabb/Programming/gorgeous` was
moved to `~/.Trash/gorgeous-retired-2026-04-23` on the master commit
that closed the decision. Q8 is recorded as resolved-and-executed;
no follow-up work remains.

## Reasoning

Dual-maintenance was the load-bearing cost. Every formatter change
faced a choice — mirror or drift — and neither option was free.
Mirroring doubled the work. Drift turned the sibling into a source of
false signal.

The reference outputs argument was the strongest case for keeping
gorgeous, and it fell on the fact that bbnf-lang's snapshot suite
already captures them under version control. Losing gorgeous loses
nothing the repository does not retain.

Trash-move rather than permanent delete preserves a recovery window
without keeping the directory active. The dated name makes it
self-documenting if recovery is ever needed.

No alternative was seriously considered — partial retirement (keeping
gorgeous read-only) would have left the dual-maintenance tax
nominally in place while confusing contributors about which path was
authoritative.

## Resolution mechanism

Already executed:

1. Sibling worktree moved to `~/.Trash/gorgeous-retired-2026-04-23`.
2. Reference outputs verified present in bbnf-lang snapshot tests.
3. Master commit closed the retirement.

No further steps. This document exists for historical attribution,
not as a pending work item.

## Follow-up gate

None required. Occasional quarterly check that no contributor
reintroduces a gorgeous-shaped mirror crate. If a genuinely new
formatter subsystem emerges, it lives in-tree under `crates/` per
the standard layout.

## References

- Trash location: `~/.Trash/gorgeous-retired-2026-04-23`
- Reference outputs now sole-sourced from bbnf-lang snapshot tests
- Feedback: `feedback_doc_alongside_code.md`,
  `feedback_no_workarounds.md`
