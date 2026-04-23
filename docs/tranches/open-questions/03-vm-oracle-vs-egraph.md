# Q3 — VM oracle throughput vs e-graph

**Status**: resolved
**Owner tranche**: BB (rule inference)
**Decision date**: 2026-04-23
**Affects**: BB

## Context

BB's rule-inference stage needs to decide which candidate rewrite rules
are sound and worth keeping. Two mechanisms were on the table. The
e-graph approach constructs equivalence classes under a current rewrite
set and proves a candidate sound if the two sides end up in the same
class. The VM-oracle approach runs candidate inputs through the
interpreter on both sides of the rule and checks for observational
equivalence.

The e-graph is faster, terminates on well-formed inputs, and scales
with rewrite-set size rather than input-space size. Its blind spot is
that it cannot prove rules the current rewrite set does not already
imply — in particular, genuinely novel equalities that would expand
what the e-graph can see.

The VM oracle is slower and operates on sampled inputs, so its
"proof" is statistical rather than structural. Its strength is that it
has no blind spot from the current rewrite set: if two programs compute
the same thing on the sampled inputs, the oracle sees it regardless of
whether the e-graph can rewrite one into the other.

The question was whether to pick one or run both, and if both, which
one gates which.

## Decision

**Both, in that order.** The e-graph runs first — every candidate
rewrite is checked for equivalence under the current rewrite set. The
VM oracle runs only on the residue: candidates the e-graph was silent
on. Accepted VM-proved rules extend the e-graph for subsequent rounds,
so the residue shrinks monotonically.

## Reasoning

The e-graph handles the bulk of candidates cheaply. Empirically the
residue is expected to be under 10% of enumeration once the e-graph is
warmed with the ground-truth rule set from Tranche H. A sub-10% VM
workload is comfortable on the iteration budget.

Using the VM as the primary filter was rejected on cost. The e-graph
scales; the VM scales poorly past trivial input spaces.

Using the e-graph alone was rejected on blind spots. Any rule the
current set cannot already derive is invisible. Rule inference that
cannot discover genuinely novel rules is not doing the job.

The accepted-VM-rule-extends-the-e-graph loop is the architectural
point: the two mechanisms are not parallel, they compose. The VM's
discoveries feed the e-graph, and next round's e-graph is strictly
stronger. This matches the Changed-flag convergence pattern already
adopted elsewhere.

## Resolution mechanism

1. BB scopes the e-graph first. Rewrite set seeded from Tranche H
   ground-truth rules.
2. BB scopes the VM oracle with sampled input generation. Sampling
   strategy is pluggable per grammar (see Q4 on pluggable components).
3. The filter pipeline is: candidate → e-graph check → if silent, VM
   check → if proved, emit rule and fold into e-graph for next round.
4. Residue fraction is a tracked metric. If it exceeds 20% sustained,
   the e-graph is under-seeded and the ground-truth set needs revisit.

## Follow-up gate

BB's definition-of-done includes a convergence plot: residue fraction
per round over a fixed enumeration budget. The plot must trend
monotonically downward. A flat or rising residue is a bug in the
e-graph-extension loop and blocks close.

## References

- `crates/ir/src/rewrites/` (see Q4)
- Feedback: `feedback_changed_flag_convergence.md`, `feedback_pluggable_components.md`
