# SK-V13 S-P3 V1 CH2 Generality / Lock 14

| Field | Value |
|---|---|
| Pass | S-P3 Synthesis-Plan |
| Cycle | V1 CHALLENGE |
| Date | 2026-05-21 |
| Lens | CH2 Generality / Lock 14 |
| Output | `restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH2.md` |

## Verdict

REVISE.

S-P3 V1 is not JSON-only and is not a REJECT. P3-A gives every shortlisted
candidate a grammar-neutral verdict, the live SPEC has a Section 2.1 Lock 14
gate, and the SPEC/DISPATCH surfaces forbid grammar-name branches, generated
JSON policy in generic crates, public substrate APIs, and generic JSON/CSS
policy helpers.

The fold blocker is narrower and inherited from Omega CH2: witness cardinality.
The current SPEC and DISPATCH require CSS L4 plus only one of Sheets or
BBNF-self for generic-crate edits. Omega CH2 resolved that fleet-wide
grammar-neutral claims need a strict CSS positive lane plus both Sheets and
BBNF-self fail-closed or generated-role witnesses. With only one of the two
non-CSS witnesses, the claim must be explicitly scoped to the witnessed
grammars and may not call itself fleet-wide grammar-neutral closure.

## Evidence Table

| Check | Disposition | Evidence | CH2 finding |
|---|---|---|---|
| Governing CH2 scope | ACCEPT | The S-P3 prompt defines CH2 as checking that every shortlisted candidate carries the S-P2 grammar-neutral verdict, that SPEC Section 2.1 exists, that every generic-crate edit has a non-JSON proof over CSS L4 / Sheets / BBNF-self, and that JSON policy in a generic crate fails CH2 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116`-`120`). ORCHESTRATOR CH2 requires Lock 14, no grammar-name leak, and interventions that work for CSS L4, Sheets, and BBNF-self, not only JSON (`restart/prompts/ORCHESTRATOR.md:81`-`85`). | The lens requires explicit cross-grammar evidence, not JSON/CSS-only proof. |
| Candidate grammar-neutral verdicts | ACCEPT | P3-A's shortlist table has a `Grammar-neutral verdict` column and every candidate P3A-0 through P3A-7 carries a verdict (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:63`-`72`). The verdicts are appropriately conditional where the candidate touches policy, primitives, JSON-only rows, union, or SIMD. | Every shortlisted candidate carries a grammar-neutral disposition. Conditional dispositions are acceptable because the exit gates bind them. |
| Generic-crate policy ownership | ACCEPT | SPEC Section 2.1 bans public JSON/CSS/Sheets/BBNF-specific APIs in generic crates, grammar-name/corpus/role/CSS-feature branches, JSON quote/escape/control policy, and CSS feature semantics in generic primitives; policy belongs to per-grammar providers/templates while generic codegen consumes grammar-derived facts (`restart/skinny/tranches/sk-v13/SPEC.md:324`-`340`). W8 further requires dispatch/string/escape/number/sink/view/flag policy to move into generated per-grammar surfaces and forbids public `GrammarConfig`, generic `JsonSink` acceleration, and JSON quote/backslash/control constants in generic code (`restart/skinny/tranches/sk-v13/SPEC.md:616`-`647`). | Generated-policy ownership and no JSON policy in generic crates are present. |
| Non-negotiable no-leak posture | ACCEPT | SPEC Section 1 forbids grammar-name branches or JSON policy in generic crates (`restart/skinny/tranches/sk-v13/SPEC.md:247`-`268`). SPEC Section 20 pre-blocks generic JSON/CSS policy in generic crates, including renamed helpers, and preserves REDRESS 121-127 Lock 14 evidence (`restart/skinny/tranches/sk-v13/SPEC.md:874`-`888`). | The live SPEC has the right Lock 14 prohibitions. |
| Non-JSON proof cardinality | REVISE | SPEC Section 2.1 says any generic edit must prove CSS L4 and "at least one of Sheets or BBNF-self" (`restart/skinny/tranches/sk-v13/SPEC.md:338`-`340`). DISPATCH repeats "CSS L4 plus Sheets or BBNF-self" in the required packet and Lock 14 policy (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:70`-`71`, `:132`-`:133`). P3-C repeats the same one-witness rule for decision-engine generic edits (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:240`-`241`). | This is too weak for fleet-wide grammar-neutral claims under Omega CH2. It must distinguish scoped one-witness proof from fleet-wide two-witness proof. |
| Omega witness-cardinality pressure | REVISE | Omega CH2 returns REVISE because fleet-wide grammar-neutral claims require a strict CSS positive lane plus both Sheets and BBNF-self fail-closed or generated-role witnesses; with only one negative control, claims must stay scoped to the witnessed grammars (`restart/audit/totality/astral/V1/hardening/CH2.md:17`, `:31`, `:36`-`:42`). | S-P3 V1 needs to fold the resolved Omega rule, not re-open the cardinality question. |
| CSS plus Sheets/BBNF-self cascade proof | ACCEPT | SYNTHESIS requires the hardcoded P1-P8 cascade to fail closed for JSON, CSS, Sheets, and BBNF-self after the resolver lands (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:232`-`235`). SPEC W7 tasks require deleting or fail-closing the old cascade for JSON, CSS, Sheets, and BBNF-self (`restart/skinny/tranches/sk-v13/SPEC.md:593`-`598`). HANDOFF refusal conditions reject downstream plans that let the old cascade silently serve JSON/CSS/Sheets/BBNF-self rows (`restart/skinny/tranches/sk-v13/HANDOFF.md:166`-`168`). | The cascade proof already includes both non-CSS witnesses; the missing cardinality rule is localized to generic-crate non-JSON proof wording. |
| Telemetry as grammar data, not behavior branch | ACCEPT | P3-D includes `grammar_id` values for JSON, CSS L4, Sheets, BBNF-self, or user grammars and states that this is a telemetry key only; generic crate behavior may not branch on it (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:120`-`125`). SPEC common telemetry fields include `grammar_id`, `lock14_status`, and `lock16_status` as gate data (`restart/skinny/tranches/sk-v13/SPEC.md:128`-`161`). | The telemetry schema can carry grammar-neutral evidence without authorizing runtime grammar branches. |

## Required Fold Actions

1. In `restart/skinny/tranches/sk-v13/SPEC.md` Section 2.1, replace the current
   non-JSON proof bullet with:

   ```text
   - Non-JSON proof: any generic edit must prove a strict CSS L4 positive lane
     and both Sheets and BBNF-self fail-closed, compile/lower/cost,
     unchanged-output, or generated-role fact-row witnesses before making
     fleet-wide grammar-neutral claims. With CSS L4 plus only one of Sheets or
     BBNF-self, the wave may proceed only with a scoped witness label naming the
     covered grammars; it may not use fleet-wide, universal, or grammar-neutral
     closure wording.
   ```

2. In `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md`, replace both
   occurrences of "CSS L4 plus Sheets or BBNF-self" with the same cardinality
   rule:

   ```text
   CSS L4 plus both Sheets and BBNF-self for fleet-wide grammar-neutral claims;
   CSS L4 plus only one of Sheets or BBNF-self is a scoped witness and cannot
   close a fleet-wide Lock 14 claim.
   ```

3. In `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md`,
   replace the decision-engine Lock 14 proof sentence with:

   ```text
   - Lock 14 proof is mandatory for generic crate edits. Fleet-wide claims require
     CSS L4 plus both Sheets and BBNF-self fail-closed, compile/lower/cost,
     unchanged-output, or generated-role fact-row witnesses. CSS L4 plus only one
     of Sheets or BBNF-self is scoped non-JSON evidence and cannot close a
     fleet-wide grammar-neutral claim.
   ```

4. In `restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md`
   Section 2.3, append this sentence to the REDRESS 121 Lock 14 gate-feed bullet:

   ```text
   Fleet-wide Lock 14 closure also requires a strict CSS L4 positive lane plus
   both Sheets and BBNF-self fail-closed or generated-role witnesses; with only
   one of those non-CSS witnesses, the result is scoped to the witnessed grammars.
   ```

5. In the next P3-A/P3-B fold, preserve the existing candidate verdicts but add a
   global note with this text:

   ```text
   Conditional grammar-neutral candidates touching generic crates can claim
   fleet-wide grammar neutrality only after the SPEC Section 2.1 two-witness rule
   passes. Until then, their verdict is scoped to JSON/CSS plus the actually
   witnessed non-CSS grammar.
   ```

No source, generated runtime, `skinny/RESULTS.md`, `skinny/REDRESS.md`, or
ledger edit is required by this CH2 fold.

## S-P3 Convergence Impact

Blocks S-P3 convergence: YES.

Per ORCHESTRATOR Section 3Z, hardening without folding is paper-hardening and
the pass cannot advance with an orphan unresolved REVISE (`restart/prompts/ORCHESTRATOR.md:112`-`121`). This CH2 issue is not an implementation blocker by itself, but S-P3 V1 cannot converge until the witness-cardinality fold lands in the SPEC/DISPATCH/P3 gate text and a later CH2 cycle accepts it.
