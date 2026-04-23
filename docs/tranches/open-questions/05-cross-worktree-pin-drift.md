# Q5 — cross-worktree toolchain pin drift

**Status**: resolved
**Owner tranche**: B1.W2.c
**Decision date**: 2026-04-23
**Affects**: bbnf-lang, parse-that, pprint CI

## Context

bbnf-lang, parse-that, and pprint are three cooperating Rust crates
developed in sibling worktrees on most contributors' machines. Each
has its own `rust-toolchain.toml`. When they drift — one pinned to a
newer nightly than the others — builds in the umbrella workspace
either fail or, worse, succeed with subtly different compiler
behavior. The failure mode is diagnosed slowly because each crate's
local build looks fine in isolation.

Nothing structural prevents drift. A contributor updating one crate's
pin for a stabilization reason has no signal that the sibling crates
need the same bump. The cost is borne by whoever next runs a
cross-crate integration test.

The question was whether to accept drift as a contributor-discipline
matter (document the expectation, review pins in PRs) or install a
mechanical guardrail.

## Decision

**CI guardrail.** A shared workflow reads `rust-toolchain.toml` from
bbnf-lang, parse-that, and pprint and fails the build if they disagree
on toolchain channel, date, or component set. Lands in B1.W2.c.

## Reasoning

Contributor discipline has failed this test historically. The
mechanical guardrail has a small one-time authoring cost and a zero
per-PR cost thereafter. A pin bump in one crate now triggers a red
build until the siblings are bumped, which is the right-sized signal:
small enough to be a fast round-trip, loud enough to be impossible to
miss.

The cost is that a genuinely intentional divergence — say, one crate
needing a nightly feature not yet on the shared channel — requires
an explicit bypass, which the workflow does not offer. The tradeoff is
accepted; such divergence has not occurred in practice, and if it did,
the right resolution is to bump the shared channel, not carry the
divergence.

Reading pins from all three crates requires the CI runner to have
access to the sibling repositories. The workflow uses sparse
checkouts to fetch just the `rust-toolchain.toml` files, not full
source trees.

## Resolution mechanism

1. B1.W2.c authors `.github/workflows/toolchain-pin-check.yml` in
   bbnf-lang.
2. Workflow runs on every push and every PR to master.
3. Workflow fetches `rust-toolchain.toml` from bbnf-lang, parse-that,
   pprint via sparse checkout.
4. Fails if any field (channel, date, components, targets) disagrees
   across the three files.
5. Error message names the divergent field and the three observed
   values.

## Follow-up gate

Workflow itself is the gate; drift cannot merge. Quarterly review
confirms no skip labels or override paths have been added to the
workflow.

## References

- `.github/workflows/toolchain-pin-check.yml` (to be authored in B1.W2.c)
- Sibling worktrees: `parse-that/rust-toolchain.toml`,
  `pprint/rust-toolchain.toml`
