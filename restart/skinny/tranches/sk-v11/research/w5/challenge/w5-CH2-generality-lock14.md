# SK-V11 W5 CHALLENGE CH2 - Generality / Lock 14

Date: 2026-05-20.
Scope: W5 Phase 2.5 CHALLENGE, CH2 generality / Lock 14 lens.
Output: this file.
Disposition: ACCEPT.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 9 and Section 2.2.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R5-grammar-neutral.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-string-span-implementation.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-gate-risk-matrix.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R1-parse-that-string-span.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R2-generated-consumers.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R3-simd-string-block.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R4-row-gates-measurement.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R6-preblocked-risk.md`.
- `skinny/REDRESS.md` REDRESS 113, plus downstream REDRESS 114 and 115 carry state.
- Source inventory for the live JSON emission boundary:
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`,
  `skinny/crates/codegen/src/sink_direct.rs`,
  `skinny/crates/runtime/src/grammars/json/generated.rs`,
  and `skinny/crates/parse-that-regex/src/lib.rs`.

## CH2 Question

Does the W5 plan keep bounded string span work inside a JSON direct-plane closure
slice, avoid leaking JSON policy into `parse-that-regex` or other generic
surfaces, require same-wave non-JSON proof for any true generic behavior change,
keep CSS/Sheets as a future measured route rather than current proof, and carry
REDRESS 113 honestly?

## Verdict

ACCEPT.

The plan is CH2-admissible because the selected redress packet is scalar-only,
JSON direct-plane-only, and row-limited to `random/direct_to_struct/main`. It
uses the existing generated JSON direct `parse_string_direct` surface and keeps
`parse-that-regex`, `bbnf-simd`, real-typed generation, and non-JSON generated
parser claims out of scope unless a new same-wave generated non-JSON proof exists.

This is not a generality close. W5 may state that the C2 span shape is
grammar-neutral by parameterization, but it must not claim non-JSON generated
parser intervention, non-JSON row movement, or SK-V11 Lock 14 closure. REDRESS
113 remains blocked and must be carried into W5 redress.

## Checks

| Check | Assessment | Evidence |
|---|---|---|
| Selected surface is JSON direct-plane only | ACCEPT | The plan selects generated JSON direct bounded string span consumed by generated direct `parse_string_direct`, with one row: `random/direct_to_struct/main` (`w5-plan-string-span-implementation.md:17`-`46`). It explicitly says the packet does not close REDRESS 113, W3 numeric, W4 container-tail, or grammar-neutral generated-parser proof (`:43`-`:46`). |
| `parse-that-regex` JSON policy does not expand | ACCEPT | W5 R1 sketches a possible generic span API, but the final plan makes `parse-that-regex` read-only and says any public parse-that span API or generic/codegen/runtime-outside-JSON behavior change returns to REVISE/BLOCKED because same-wave non-JSON proof is unavailable (`w5-plan-string-span-implementation.md:61`-`73`, `:187`-`:189`). Redress must not add JSON delimiter, backslash, control-cutoff, `\u`, surrogate, or trusted-UTF-8 policy to generic parse-that code. |
| JSON constants remain generated JSON-local | ACCEPT WITH BOUNDARY | The implementation sketch uses delimiter `"`, escape `\`, control cutoff `0x20`, and cap 8 only because the selected caller is generated JSON direct SinkOnly (`w5-plan-string-span-implementation.md:97`-`116`). Current emitted JSON uses `match_tiny_plain_string_direct::<8>` and falls back to `parse_that_regex::match_string_at_quote_trusted_utf8`; that is already JSON runtime output (`generated.rs:166`-`180`, `:610`-`:635`). The W5 patch must keep those constants in generated JSON direct code, not in reusable grammar-neutral parse-that or lowering logic. |
| Codegen edit does not become generic proof | ACCEPT WITH BOUNDARY | Section 2.2 requires same-wave CSS L4 / Sheets / BBNF-self generated parser proof when generic behavior changes (`SPEC.md:230`-`245`). W5 edits may touch `sink_direct.rs` only as a renderer for the existing JSON direct output and must regenerate JSON output from the named input (`w5-plan-string-span-implementation.md:128`-`147`). Current codegen still gates runtime emission through `json_provider::ensure_runtime_profile`, which accepts only `json` (`codegen/src/lib.rs:108`-`127`, `json_provider.rs:4`-`12`). Any change that relaxes `json_provider`, changes generic lowering semantics, or affects a future non-JSON grammar without measured proof is CH2 REVISE/BLOCKED. |
| Same-wave non-JSON proof rule is preserved | ACCEPT | SPEC requires every generic/codegen/runtime-outside-JSON edit to have same-wave CSS L4, Sheets, or BBNF-self proof (`SPEC.md:170`-`178`, `:239`-`:245`). W5 R5 repeats that non-JSON proof means generated non-JSON Track 1, independent same-plane Track 2/oracle, strict equality, throughput against baseline, gate consumption, and no JSON policy leak (`w5-R5-grammar-neutral.md:19`-`28`). The plan rejects parse-that, SIMD, typed-generated, or runtime-outside-JSON behavior changes without that proof (`w5-plan-string-span-implementation.md:293`-`295`). |
| CSS/Sheets future route is honest | ACCEPT | R5 says Sheets is the cleaner future W5 C2 proof surface because doubled-quote strings isolate the span policy, while CSS remains the broader SK-V11 non-JSON axis because it exercises more primitive families (`w5-R5-grammar-neutral.md:94`-`134`). R5 also says neither currently satisfies the non-JSON proof gate because the generated Track 1 baseline is missing (`:133`-`:134`). W5 may use CSS/Sheets facts as parameterization evidence only, not row admission or W1b/W2 substitute (`:148`-`:161`). |
| REDRESS 113 is carried honestly | ACCEPT | REDRESS 113 records W2 as `BLOCKED` because W1b admitted no generated non-JSON baseline, W2 may not create the first measurable non-JSON row, and W3-W8 may continue only as direct-plane closure/fixpoint waves with W2's non-JSON axis blocked (`skinny/REDRESS.md:3340`-`3355`). W5 R4 and the plan both say W5 remains JSON direct-plane closure and must carry REDRESS 113 forward (`w5-R4-row-gates-measurement.md:21`-`34`; `w5-plan-string-span-implementation.md:271`-`273`, `:300`-`:301`). |
| Track 2 and oracle independence remain required | ACCEPT | The plan requires generated Track 1, hand Track 2, serde_json, and sonic-rs agreement; Track 2 must not call generated Track 1 helpers or generated span symbols (`w5-plan-string-span-implementation.md:117`-`123`, `:153`-`:162`). This keeps W5 from using coupled JSON implementation detail as generality proof. |

## Required Redress Boundaries

- Keep W5 as a JSON direct-plane bounded string span attempt for
  `random/direct_to_struct/main`.
- Do not edit `skinny/crates/parse-that-regex/src/lib.rs` for behavior. If a
  parse-that source edit is needed, return to REVISE/BLOCKED unless the same wave
  also lands and gate-consumes a generated CSS L4, Sheets, or BBNF-self
  string/literal proof under new authority.
- Do not relax `json_provider::ensure_runtime_profile`, change generic lowering
  semantics, add a grammar-neutral runtime template, or make generated non-JSON
  claims from `sink_direct.rs` changes.
- Keep JSON delimiter, escape, control-cutoff, UTF-8, surrogate, and cap policy
  in generated JSON caller code. Do not move that policy into generic parse-that,
  `bbnf-simd`, BIR, directives, public substrate APIs, or runtime outside
  generated JSON modules.
- Treat CSS and Sheets only as future route evidence. The preferred future narrow
  C2 route is generated Sheets baseline then W5-shaped intervention; the broader
  CSS route needs its own generated CSS L4 baseline first.
- Carry REDRESS 113 as blocked. W5 redress may admit or reject only its selected
  JSON direct row; it may not close the non-JSON axis, REDRESS 114, or REDRESS
  115.

## Failure Conditions

Return REVISE before source work if implementation requires a public parse-that
span API, non-JSON/generated-parser claim, second row, typed generated consumer,
SIMD body, relaxed `json_provider`, generic lowering change, or runtime-outside-
generated-JSON behavior change without same-wave non-JSON proof.

Return REJECT/REDRESS if the landed slice leaks JSON policy into generic code,
uses REDRESS 113 as proof, treats CSS/Sheets grammar citations as an oracle,
couples Track 2 to generated Track 1, admits a non-selected Unicode residual, or
records W5 redress as closing W2/W3/W4.

DISPOSITION: ACCEPT
