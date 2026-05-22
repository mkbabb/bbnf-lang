# SK-V13 S-P3 V2 CH2 Generality / Lock 14

| Field | Value |
|---|---|
| Pass | S-P3 Synthesis-Plan |
| Cycle | V2 CHALLENGE |
| Lens | CH2 Generality / Lock 14 |
| Commit under review | `9f8bbfce5` |
| Output | `restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH2.md` |

## Verdict

ACCEPT.

The V1 CH2 fold landed. Fleet-wide generic-crate grammar-neutral claims now
require strict CSS L4 plus both Sheets and BBNF-self witnesses; CSS plus only
one non-CSS witness is explicitly scoped; JSON/CSS policy is fenced out of
generic crates; and telemetry grammar ids remain gate data, not generic-crate
behavior.

## Governing Evidence

- ORCHESTRATOR defines CH2 as Lock 14: no grammar-name leak and interventions
  that work for CSS L4, Sheets, and BBNF-self, not only JSON
  (`restart/prompts/ORCHESTRATOR.md:81`-`:85`), and keeps "No JSON code in
  generic crates" as a CH2 Lock 14 enforcement rule
  (`restart/prompts/ORCHESTRATOR.md:199`-`:204`).
- PASS-3 preserves grammar-agnostic Rust fixture code and Lock 14 no-overfit
  pressure (`restart/audit/pass-3-runtime/PASS-3.md:418`), limits new-grammar
  onboarding to the grammar source plus metadata block before parity fixtures
  (`restart/audit/pass-3-runtime/PASS-3.md:420`-`:425`), and carries BBNF-self,
  CSS L4, and Google Sheets as generated grammar rows
  (`restart/audit/pass-3-runtime/PASS-3.md:431`-`:439`).
- The V1 consolidation required replacing the one-witness rule with the Omega
  cardinality rule: CSS L4 plus both Sheets and BBNF-self for fleet-wide claims,
  with CSS plus one non-CSS witness scoped to witnessed grammars
  (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:57`-`:60`).

## Fold Items

| V1 CH2 fold item | Disposition | Evidence |
|---|---|---|
| SPEC Section 2.1 two-witness rule | ACCEPT | SPEC now requires a strict CSS L4 positive lane plus both Sheets and BBNF-self before fleet-wide grammar-neutral claims, and scopes CSS plus one of those non-CSS witnesses to named covered grammars (`restart/skinny/tranches/sk-v13/SPEC.md:370`-`:390`). |
| DISPATCH required packet and Lock 14 text | ACCEPT | The required packet asks for CSS L4 plus both Sheets and BBNF-self for fleet-wide claims and labels CSS plus one non-CSS witness scoped (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:65`-`:78`). Lock 14 repeats that CSS L4 plus one of Sheets/BBNF-self cannot close fleet-wide Lock 14 (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:133`-`:146`). |
| P3-C decision-engine Lock 14 proof | ACCEPT | P3-C now states that generic-crate fleet-wide grammar-neutral claims require CSS L4 plus both Sheets and BBNF-self witnesses, while CSS L4 plus only one is scoped non-JSON evidence (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:240`-`:244`). |
| P3-E REDRESS 121 gate feed | ACCEPT | P3-E now adds the fleet-wide Lock 14 closure rule to REDRESS 121: strict CSS L4 plus both Sheets and BBNF-self, with one non-CSS witness scoped (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:105`-`:111`). |
| P3-A/P3-B fold note | ACCEPT | P3-A added a V2 fold note that P3A-1 through P3A-7 fleet-wide grammar-neutral claims are conditional on SPEC Section 2.1's two-witness rule (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:10`-`:15`) and calls out the per-grammar policy/sink candidate as needing that proof (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:88`-`:92`). P3-B's sequencing keeps decision waves from encoding CSS/JSON branches in generic crates (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:123`-`:126`) and requires JSON/CSS/Sheets/BBNF-self fail-closed cascade proof (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:171`-`:178`). |

## CH2 Checks

| Check | Disposition | Evidence |
|---|---|---|
| Fleet-wide generic-crate grammar-neutral claims require CSS L4 plus both non-CSS witnesses | ACCEPT | SPEC Section 2.1, DISPATCH, P3-C, and P3-E all carry the two-witness rule (`restart/skinny/tranches/sk-v13/SPEC.md:384`-`:390`; `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:142`-`:146`; `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:240`-`:244`; `restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:105`-`:111`). |
| CSS plus only one non-CSS witness is scoped | ACCEPT | SPEC says CSS L4 plus only one of Sheets or BBNF-self must name covered grammars and may not use fleet-wide, universal, or grammar-neutral closure wording (`restart/skinny/tranches/sk-v13/SPEC.md:387`-`:390`). DISPATCH and P3-C mirror that as scoped evidence only (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:76`-`:78`; `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:243`-`:244`). |
| No JSON/CSS policy leaks into generic crates | ACCEPT | SPEC prohibits public JSON/CSS/Sheets/BBNF-specific APIs in generic crates, grammar-name/role/CSS-feature behavior branches, JSON quote/escape/control policy, and CSS feature semantics in generic primitives (`restart/skinny/tranches/sk-v13/SPEC.md:374`-`:383`). W8 also blocks public `GrammarConfig`, generic `JsonSink` acceleration, and JSON quote/backslash/control constants in generic code (`restart/skinny/tranches/sk-v13/SPEC.md:693`-`:710`). Section 20 blocks generic JSON/CSS policy in generic crates (`restart/skinny/tranches/sk-v13/SPEC.md:958`-`:963`). |
| Telemetry grammar ids remain data, not behavior | ACCEPT | P3-D defines `grammar_id` values as `json`, `css_l4`, `sheets`, `bbnf_self`, or `user:<slug>` and states they are telemetry keys only; behavior may not branch on them in generic crates (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:120`-`:125`). SPEC carries `grammar_id`, `lock14_status`, and `lock16_status` as required telemetry/gate fields (`restart/skinny/tranches/sk-v13/SPEC.md:127`-`:138`; `restart/skinny/tranches/sk-v13/SPEC.md:178`-`:184`). |
| No stale one-witness wording in live P3 authority files | ACCEPT | The stale-phrase scan over SPEC, DISPATCH, and live P3 authority files returned no matches. |

## Evidence Summary

The fold is grammar-neutral in the CH2 sense. Generic crates may consume
generated facts and opaque tables, but they may not branch on grammar ids or
embed JSON/CSS policy. Fleet-wide claims are now gated by CSS L4 plus both
Sheets and BBNF-self; one non-CSS witness can proceed only as scoped evidence.

No CH2 blocker remains for S-P3 V2.
