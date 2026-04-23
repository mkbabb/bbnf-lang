# Open Questions — Resolution Index

This directory holds per-question resolution docs for the open questions
surfaced by the GESTALT §10 audit. Each question is a decision point that
crosses tranche boundaries or touches architecture the tranche docs cannot
reasonably absorb on their own — rule storage locations, cache invalidation
keys, tranche re-sequencing, and similar cross-cutting concerns.

The doc pattern, one file per question, captures:

- **Context** — what the question asked, why it emerged, what breaks if
  it stays unanswered.
- **Decision** — the resolved answer, stated as a standalone assertion.
- **Reasoning** — why this decision, including alternatives considered
  and the cost of each tradeoff.
- **Resolution mechanism** — the concrete code, doc, or config change
  that carries the decision, and where it lands.
- **Follow-up gate** — the standing check (CI job, review cadence,
  metric) that keeps the resolution honest after it lands.
- **References** — file paths, commit SHAs, and related feedback memos.

A question exits this directory only by being resolved and, where the
decision implies implementation, scoped into a tranche wave. Unresolved
questions stay here with `Status: in-progress` or `Status: deferred` so
the open surface is legible from one directory listing.

## Status matrix

| Q# | Topic | Owner tranche | Decision summary | Doc |
|----|-------|---------------|------------------|-----|
| 0 | Tape abrogation / direct-to-struct sequencing | AZ (new) | Shape C: AY-II stays tape-substrate, AZ absorbs direct-to-struct; BA/BB shift down, BC retired | [00-tape-abrogation-shape-c.md](00-tape-abrogation-shape-c.md) |
| 1 | Backward-pointer form | AZ (sidecar); BA.W0 (geometry) | Sidecar column — parallel index built on demand, no bloat when unused | [01-backward-pointer-form.md](01-backward-pointer-form.md) |
| 2 | StructRegistry partial-close | AZ.W1 | Hard fail and block — per-grammar IR audit, any red grammar blocks BA opening | [02-structregistry-partial-close.md](02-structregistry-partial-close.md) |
| 3 | VM oracle throughput vs e-graph | BB | E-graph first, VM-oracle residue only; accepted VM rules fold back into e-graph | [03-vm-oracle-vs-egraph.md](03-vm-oracle-vs-egraph.md) |
| 4 | Rule storage, ranker, human review | BB + every grammar | Grammar-specific rules colocated, fleet-wide in `crates/ir-rewrites/`; mandatory ranker; 3-tier review | [04-rule-storage-ranker-review.md](04-rule-storage-ranker-review.md) |
| 5 | Cross-worktree toolchain pin drift | B1.W2.c | CI guardrail — shared workflow fails build on any divergence across bbnf-lang/parse-that/pprint pins | [05-cross-worktree-pin-drift.md](05-cross-worktree-pin-drift.md) |
| 6 | test-threads × -Zthreads collision | B1 profile config | Drop test threads on `ax-iter` (=2), keep on `close` (=8); 30s iter-test wall-clock gate | [06-test-threads-zthreads-collision.md](06-test-threads-zthreads-collision.md) |
| 7 | Derive-cache invalidation key | AZ.W0 | Composite `(grammar-sha256, derive-crate-version, rustc-sha)` with robustness/perf/observability/migration discipline | [07-derive-cache-invalidation-key.md](07-derive-cache-invalidation-key.md) |
| 8 | gorgeous-mirror retirement | master housekeeping | Deleted — sibling moved to `~/.Trash/gorgeous-retired-2026-04-23`; reference outputs now sole-sourced from in-tree snapshots | [08-gorgeous-mirror-retirement.md](08-gorgeous-mirror-retirement.md) |
| 9 | Classifier collision under multi-payload activation | AZ.W0 research | Front-load research artefact in AZ.W0; re-plan trigger at AZ opening if refactor exceeds tranche | [09-classifier-collision-frontload.md](09-classifier-collision-frontload.md) |

## Resolution status tally

- **Resolved**: Q0, Q1 (sidecar decision; geometry deferred), Q2, Q3,
  Q4, Q5, Q6, Q7, Q9 (research scoped)
- **Resolved and executed**: Q8
- **In progress**: none
- **Deferred**: none

All ten items have decisions recorded. Implementation surfaces across
AZ (Q0, Q1, Q2, Q7, Q9), BB (Q3, Q4), and B1 (Q5, Q6).

## Cross-references

- GESTALT §10 audit — source of the original question enumeration
- Feedback memory: `feedback_new_tranche_new_doc.md`,
  `feedback_no_workarounds.md`, `feedback_no_orthogonal_codepaths.md`,
  `feedback_pluggable_components.md`, `feedback_iter_profile_always.md`

## Authoring notes

New open questions enter this directory with the next unused number,
`Status: in-progress`, and as much of the template as the question
supports at the time of filing. A question without a decision still
earns a doc so the open surface is visible. Resolution promotes the
status to `resolved`; it does not move the doc.
