# Tranche AH — System Audit

AH was a five-part audit of the post-AG substrate, designed to
surface gaps between live infrastructure and active consumption
before the emission wiring tranche.

## AH.0 — Materialization audit

Inventory of `MaterializationClass` assignments across production
grammars.  Questions: how many rules classify as `TransparentElide`?
Do any rules that *should* project directly land in `MustTape`
because the classifier is too conservative?  What is the distribution
across JSON, CSS L4, and Google Sheets grammars?

## AH.1 — CSP audit

Verify that the 5 variable families (Alt, Wrap, Engine, Mat, Tier)
interact correctly under the per-component solver.  Confirm
`TierFollowsMaterialization`, `EnginePropagation`, and
`ParentCompatibility` constraints fire on real grammars and produce
sane tier assignments.  Check for over-constraint (all rules forced
to Tape) or under-constraint (Direct assigned to rules that cannot
support it).

## AH.2 — E-graph audit

Confirm that `CostWeights` shared between `GrammarCostModel` and
`RegexExtractionCost` produce consistent extraction results.  Check
whether the cost model biases extraction toward Direct-enabling body
shapes or is tier-agnostic.  Verify rewrite rule parity between the
grammar tier and HIR tier.

## AH.3 — Recognizer/kernel/pattern audit

Audit `recognize_patterns`, `delim_scan`, `key_dispatch`, and
`factor_regex_with_lookahead` for interactions with the emission tier
system.  Do any pattern recognizers produce output that conflicts
with Direct emission?  Are there hot-path patterns that would benefit
from Direct emission but are currently Tape-locked?

## AH.4 — Cost model audit

End-to-end cost model coherence: do the CostWeights knobs
(`dispatch_branch`, `dispatch_table`, `inline_body_size_penalty`,
`cross_module_coercion`, etc.) produce globally optimal emission
under the current constraint set?  Profile representative grammars
and compare CSP-solved tiers against manually-reasoned optimal tiers.

## Disposition

AH's audit questions were absorbed into Tranche AI's design phase.
Rather than executing the audits as standalone diagnostics, the
findings informed AI's implementation plan directly:

- AH.0 findings drove AI.2's eligibility widening scope.
- AH.1 findings confirmed the CSP was producing valid but
  conservative assignments, motivating AI.4's cross-component
  reconciliation.
- AH.2 findings exposed the cost model's tier-agnosticism,
  directly motivating AI.3's emission-aware bonus.
- AH.3 and AH.4 findings were folded into AI.6's cleanup pass.

No commits under AH; all deliverables land under AI.
