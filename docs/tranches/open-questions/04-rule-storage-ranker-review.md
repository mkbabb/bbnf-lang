# Q4 — rule curation: storage, ranker, review

**Status**: resolved
**Owner tranche**: BB
**Decision date**: 2026-04-23
**Affects**: BB, every grammar directory, new `crates/ir/src/rewrites/`

## Context

Rule inference produces candidates at a rate that outstrips human
review. The question clustered four sub-decisions that only make sense
together: where rules live on disk, how they are ranked for review, how
review itself is tiered, and how the first-run bootstrap is bounded.

Storage location matters because rules come in two flavors — grammar-
specific (a CSS shorthand expansion that has no meaning in JSON) and
fleet-wide (a generic algebraic identity that applies anywhere an
integer appears). Colocating both flavors in one registry collapses the
distinction; separating them forces every new grammar to touch core.

Ranking matters because without one the review queue is FIFO, which
guarantees the most important rules land after the trivial ones
someone already spotted by hand.

Review tiering matters because a uniform review pipeline wastes
expert time on rules that are obviously sound and obviously small. A
flat pipeline does not scale past the first grammar.

Bootstrap matters because the first run, on a cold e-graph, emits a
large candidate set; without a bound, review blocks the tranche.

## Decision

- **Storage**: grammar-specific rules live with the grammar
  (`grammar/<name>/rewrites/*.ron`). Fleet-wide rules live in a new
  `crates/ir/src/rewrites/` module within the existing `bbnf-ir`
  crate. Rules are never added to `crates/core`. A standalone
  `ir-rewrites` crate was rejected — rules operate on IR shapes and
  are not general-purpose; they do not need an independent crate
  boundary.
- **Ranker**: mandatory, automatic. Score is the product of match
  frequency, cost delta, generality, similarity to ground truth,
  novelty, and tree size.
- **Review**: tiered. Class-1 trivial rewrites auto-accept with
  spot-check sampling. Class-2 structural rewrites follow a fast-track
  review template. Class-3 novel rewrites go through full review. Goal
  is >90% of rules landing in Classes 1–2.
- **Bootstrap**: first-run queue is bounded by the Tranche H
  ground-truth set size (~dozens of rules). One-time effort.

## Reasoning

Separating storage by scope is the extensibility contract: new
grammars ship rules without editing any shared crate. Fleet-wide
rules have exactly one home. Colocating everything in `crates/core`
was rejected because it would make core a god crate and force
grammar authors to fork core to experiment. A standalone
`crates/ir-rewrites/` crate was also rejected: rule schema, ranker,
and tiering are IR-shape-specific and belong to the IR substrate;
isolating them as a separate crate adds a boundary without a
corresponding ownership split. `feedback_general-infra-crates`
applies to constructs that are genuinely general-purpose (e-graph,
cost models) — rewrite rules over `IrNode` are not.

Mandatory ranking is justified by the residue analysis in Q3: even a
10% VM residue across a multi-grammar run is thousands of rules. An
unranked queue is untriageable.

Tiering is the review-pipeline analog of cost-based dispatch. Class-1
rules have a shape small enough and a cost delta large enough that a
human adds negligible signal; automated acceptance with sampled audit
is strictly better than a review rubber-stamp. Class-3 rules are the
opposite — shape novelty is where human judgment matters most — and
deserve the full review surface. Class-2 is the middle tier where a
template captures the structural check without demanding a bespoke
writeup.

Bootstrap bounding is a deliberate throttle. Running inference against
the full candidate space on a cold e-graph is a useful test, but
landing all of it in one sitting is not. The Tranche H ground truth
provides a concrete upper bound that is large enough to demonstrate
coverage and small enough to review.

Tradeoffs: the ranker and tiering apparatus is new code that must be
maintained. The alternative — a manual queue — costs more per rule
every round and scales worse.

## Resolution mechanism

1. BB creates `crates/ir/src/rewrites/` with the fleet-wide registry API.
2. BB adds `rewrites/` subdirectory per existing grammar.
3. Ranker implemented in `crates/ir/src/rewrites/rank.rs`. Scoring
   factors are pluggable (see `feedback_pluggable_components.md`).
4. Review-tier classifier implemented in the same crate. Tier
   boundaries are configurable constants, reviewed quarterly.
5. Bootstrap run loads Tranche H ground truth, emits the bounded
   first-round candidate set, and halts for review before expanding.

## Follow-up gate

Quarterly review of tier-distribution statistics. If Class-3 share
climbs above 15% sustained, the ranker is mis-scoring novelty and
needs recalibration. If Class-1 auto-accept sampled-audit turns up
any false positives, auto-acceptance pauses until the cause is
found.

## References

- `crates/ir/src/rewrites/` (new, authored in BB)
- `grammar/<name>/rewrites/` (new, authored per grammar)
- Feedback: `feedback_pluggable_components.md`,
  `feedback_general_infra_crates.md`, `feedback_no_god_modules.md`
