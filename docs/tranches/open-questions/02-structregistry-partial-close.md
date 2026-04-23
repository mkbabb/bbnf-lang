# Q2 — StructRegistry partial-close

**Status**: resolved
**Owner tranche**: AZ.W1
**Decision date**: 2026-04-23
**Affects**: AZ, BA gate

## Context

The StructRegistry is the authoritative per-grammar mapping from rule
identifier to concrete struct layout. Under the tape substrate a rule
with an unresolved layout degraded gracefully — the tape stored the raw
payload, and downstream passes either cast it or skipped it. Under
direct-to-struct emission there is no tape to fall back to. Every rule
reachable from a reachable entry point must have a fully closed layout
by the time codegen runs, or the emitter has no target to write into.

The question was whether AZ could open BA (pointer queries) while the
registry was partially closed — i.e., with a handful of production
grammars still carrying rules the layout inference could not resolve —
or whether closure was a hard gate. Partial-close would mean BA would
ship with a fallback for unresolved rules. Hard-close would mean AZ
does not land until every production grammar passes registry closure.

## Decision

**Hard fail and block.** The IR audit pass runs per grammar and produces
a pass/fail verdict. Any production-grammar fail blocks BA opening.
AZ.W1 is the wave that does the closure work; its definition-of-done
includes a green audit across every production grammar in the repo.
Hard-fail-and-block semantics are non-negotiable.

## Reasoning

A fallback for unresolved rules is exactly the kind of orthogonal
codepath the `no-workarounds` and `no-orthogonal-codepaths` feedback
memos exist to prevent. Once shipped, the fallback becomes the path of
least resistance: new grammars are written against it, the closure
pressure relaxes, and within a tranche or two the registry is in worse
shape than it started. The "we'll tighten later" trajectory has a
unanimous prior of never tightening.

Hard-close has a real cost. Grammars with genuinely ambiguous layouts
must either be rewritten (ambiguity hoisted into a named variant) or
have their rules split until every path has a concrete layout. This is
work that would otherwise be deferred. AZ absorbs that work.

The alternative — shipping BA on a partially-closed registry — costs
more. BA's pointer queries depend on every node having a known layout
to know which fields are pointer-carrying. Without closure BA's query
engine needs its own fallback, which propagates the ambiguity forward.

## Resolution mechanism

1. AZ.W1 runs the IR audit pass against every grammar in
   `grammar/*/`. The pass emits a report per grammar.
2. Any rule that cannot be closed is listed with the inference step
   that failed. The grammar author (or AZ agent) resolves by splitting
   or annotating.
3. CI adds a job that runs the audit and fails the build on any red
   grammar. No grammar-specific allowlists.
4. BA.md opening checklist includes "IR audit green across all
   production grammars on current master."

## Follow-up gate

The audit CI job is the standing gate. Any PR that introduces a grammar
rule with unresolvable layout fails the build. No override flag; the
PR author fixes the rule. Quarterly review confirms no allowlists or
skip flags have crept into the audit runner.

## References

- `crates/ir-audit/` (to be authored in AZ.W1)
- CI config addition lands in AZ.W1
- Feedback: `feedback_no_workarounds.md`, `feedback_no_orthogonal_codepaths.md`
