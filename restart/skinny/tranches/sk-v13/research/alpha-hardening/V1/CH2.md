# SK-V13 Alpha Hardening V1 CH2 - Generality / Lock 14

Disposition: **REVISE, non-blocking for Alpha-F as a bracket, blocking for any
S-P3 `SPEC.md` or Wave 0 dispatch until the two Lock 14 edits below are folded.**

Scope: CH2 reviews the SK-V13 Alpha contract for grammar neutrality, non-JSON
evidence handling, JSON-only generic branch exclusion, and G-Omega / totality
fold criticality before W0. The governing lens requires Lock 14 to hold with no
grammar-name leak and with interventions that work for CSS L4, Sheets, and
BBNF-self rather than JSON only (`restart/prompts/ORCHESTRATOR.md:74-88`).
Pass Alpha specifically asks CH2 whether the goalset respects Lock 14 and works
for non-JSON grammars (`restart/prompts/pass-contracts/PASS-ALPHA.md:33-40`).

## Findings

1. **ACCEPT - The close contract preserves grammar neutrality at the Alpha-F
   level.** The SK-V13 synthesis makes the decision-engine fold preserve JSON
   behavior while forbidding grammar-specific branches in generic crates
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:59-65`), names grammar-name
   branches in generic crates as pre-blocked (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:218-231`),
   and requires totality V1.1 to fold GrammarConfig / Lock 14 evidence before
   Wave 0 (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:111-121`). That matches
   Lock 14's zero-overfit rule: generic crates carry no grammar-named modules,
   no `match grammar { Json => ... }` arms, no grammar-specific public types,
   and no per-grammar feature flags (`restart/locks/LOCKS.md:78`).

2. **ACCEPT - CSS and non-JSON are treated as measured evidence, not as prose
   garnish.** The contract records the SK-V12 CSS admission as one precise
   same-plane row, not full parity (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`),
   and the scoping matrix shows the admitted CSS row is declaration-value tokens
   only, with stylesheet, selectors, at-rules, variables, calc, color functions,
   gradients, transforms, filters, easing, and related surfaces partial or
   missing (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:11-18`,
   `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:94-132`).
   Alpha-A carries this as 1 admitted CSS row plus 23 remaining non-OUT_OF_SCOPE
   feature rows (`restart/skinny/tranches/sk-v13/research/alpha/alpha-A-results-extraction.md:88-101`),
   and Alpha-B says the same single row is 1/24 covered with 23/24 remaining
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-B-competitor-deltas.md:27-34`).

3. **ACCEPT - The value/config scoping is honest about residual JSON coupling.**
   The Alpha-D ledger carries GrammarConfig only as partial Lock 14 progress and
   names unresolved JSON coupling in value dispatch, string/escape policy,
   number policy, key-colon structure, `OffsetFlags`, and `JsonSink`
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:73-95`,
   `restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:177-183`).
   The value/API scoping independently says the current surface is not grammar
   neutral and that CSS requires generated dispatch, CSS string/escape policy,
   comment-aware whitespace, CSS number policy, keyword policy, and a generated
   CSS sink (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:165-176`,
   `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:180-215`).

4. **REVISE - Alpha-E leaves an override loophole for new public substrate
   surfaces.** E2's falsifiability gate correctly rejects grammar-name branches
   and JSON role branches in generic paths (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:167-172`),
   but then says a public trait, new directive, new BIR variant, new
   `BackendShape`, or public `UnionTape`-style substrate change is rejected
   "unless S-P3 explicitly records a user-approved SPEC override"
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:173-175`).
   That escape hatch conflicts with the orchestrator non-negotiables forbidding
   new BBNF directives, new BIR variants, new substrates, and JSON code in
   generic crates (`restart/prompts/ORCHESTRATOR.md:197-205`), and with Lock 10's
   rule that no new directive carries materialization policy
   (`restart/locks/LOCKS.md:70`). Fold requirement: replace the override with
   "SPEC may only narrow owner paths and gates; it cannot authorize new
   directives, BIR variants, `BackendShape`s, public substrate APIs, or
   grammar-specific generic behavior."

5. **REVISE - The decision-engine scoping still permits a JSON-only fallback
   shape unless S-P3 hardens it.** The decision-engine scoping identifies the
   current recognizer mining as hardcoded to the JSON alphabet
   (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:13-20`)
   and proposes an e-graph driver behind `#[cfg(feature = "sk-v13-egraph")]`
   with old builds preserved (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:66-73`,
   `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:179-187`).
   A default-off resolver plus retained cascade can become a JSON-only generic
   branch if S-P3 does not make the old path non-admissible for new rows. Fold
   requirement: S-P3 must state that no generated JSON, CSS, Sheets, or BBNF-self
   admission may use the hardcoded P1-P8 cascade after the resolver wave, and
   any fallback must be grammar-neutral, gate-visible, and row-rejecting rather
   than silently selecting JSON policy.

6. **ACCEPT - G-Omega and totality folds are critical pre-W0 gates.** The
   handoff says `SPEC.md` and `DISPATCH-PROMPT.md` are intentionally absent and
   must be authored only after required G-Omega pre-W0 work
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:5-8`), and it blocks any Wave 0
   source, generated runtime, gate/report, RESULTS, or REDRESS edits before
   G-Omega closes (`restart/skinny/tranches/sk-v13/HANDOFF.md:54-74`,
   `restart/skinny/tranches/sk-v13/HANDOFF.md:85-92`). The pass-framework
   scoping names Lock 14 per-wave gate, Lock 16 checkasm / escape-mask canon,
   and non-JSON telemetry schema as critical blockers (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md:226-258`,
   `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md:262-273`).

## Required Fold

1. In S-P3, delete the Alpha-E override path for new directives, BIR variants,
   `BackendShape`s, public substrate APIs, and grammar-specific generic behavior.
   User approval can repin scope, but it cannot be represented as a local
   `SPEC.md` bypass of Lock 10 / Lock 14 / CH5 substrate discipline.

2. In S-P3, make the decision-engine fold fail closed for generic fallback:
   after the resolver wave, the hardcoded P1-P8 cascade cannot admit JSON-only
   or generic rows, and any retained compatibility branch must emit a visible
   row rejection or non-admission signal.

3. Preserve the existing G-Omega pre-W0 block. Lock 14 per-wave gate language,
   Lock 16 checkasm / escape-mask canonicalization, and non-JSON telemetry schema
   must land through totality before any implementation wave.

With those folds, CH2 can accept the Alpha contract as grammar-neutral enough
for S-P3 planning. Without them, the contract can still bracket SK-V13, but it
must not authorize W0 or any generated/runtime/generic behavior edit.
