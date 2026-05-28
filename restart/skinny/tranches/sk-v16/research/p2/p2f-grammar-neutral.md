# SK-V16 P2-F: Grammar-Neutral Primitive Verdicts

Pass: S-P2 Research. Cycle: V16.
Date: 2026-05-28.
Scope: Lock 14 verdict for candidate primitives from P2-B/C/D/E.
Output: this file.
P1 hot-leaf antecedents: scanner/string, scanner/whitespace, scanner/number, tape/view, generated product.
Lock surface: Lock 14.

## Section 1 - Findings

Lock 14 allows generic codegen to consume generated provider manifests,
generated grammar facts, and generated sink/fact/value surfaces. It does not
allow generic crates to branch on JSON/CSS runtime families, hard-code JSON
punctuation, or encode grammar-shaped profiles.

P2-F therefore treats a primitive as admissible only when its API is one of:

- byte set -> mask;
- mask/carry -> mask/carry;
- mask -> positions;
- byte block -> decoded primitive atom with caller-owned semantics;
- tape cursor plus generated grammar table -> next tape operation.

Anything else is rejected or reframed before S-P3.

## Section 2 - Candidate Primitives

| Candidate | Grammar-neutral verdict | Accepted shape | Rejected shape |
|---|---|---|---|
| `byte_class_from_table_64` | ACCEPT-CANDIDATE | generated table, input block, output mask | hard-coded JSON structural bytes in `bbnf-simd` |
| `byte_class_from_eq_set_64` | ACCEPT-CANDIDATE | generated delimiter set, cursor/end, output first/member mask | CSS provider-specific branch in generic code |
| `string_special_block_16` | ACCEPT-CANDIDATE-WITH-CHECKASM | terminator/escape/control-limit parameters | JSON quote/backslash-only API |
| `escape_mask_64` | ACCEPT-CANDIDATE | backslash mask plus carry | retained cross-call classifier state |
| `hex_quad_decode_x4` | REFRAME | hex quartet decode and validity mask only | JSON Unicode string materializer or fixed-shape row retry |
| `digit_block_accumulate` | ACCEPT-CANDIDATE-WITH-CONSUMER | digit validity plus integer accumulation, caller supplies numeric policy | Canada-only mantissa/coordinate patch |
| `skip_class_run_64` | ACCEPT-CANDIDATE | generated class table plus stop set | CSS identifier scanner embedded in generic crate |
| `tape_cursor_step` | ACCEPT-SCALAR-CANDIDATE | generated grammar kind table over sealed tape cursor | retained cursor/list, aux density table, second tape |
| `string_body_range_fast` | ACCEPT-SCALAR-CANDIDATE | tape-local range computation under generated view | parse-time eager decode or decoded-string sidecar |
| `skip_trivia` | ACCEPT-CANDIDATE | generated trivia policy over whitespace/comment classes | hard-coded CSS comments or JSON whitespace in generic code |
| `take_structural_after_layout` | ACCEPT-CANDIDATE | generated layout set plus expected delimiter set | parser-owned structural cursor or retained index |
| `generated_first_literal_dispatch` | ACCEPT-CANDIDATE-WITH-GENERATED-OWNER | generated FIRST/literal table consumed by runtime templates | generic crate grammar branch or profile array |
| `materialize_f64_exact_scalar_fallback` | QUARANTINE | scalar exact fallback under generated number policy | SIMD/numeric admission without REDRESS 80 closure |
| prefetch/store hints | REJECT-FOR-S-P2 | not a primitive until P1 names tape write/read cost | architecture decoration without row-local consumer |

## Section 3 - Grammar-Neutrality

CSS L4 can consume byte-class, string-special, delimiter-search, digit, and
hex primitives for identifiers, strings, URLs, escapes, dimensions, and
declarations. That does not authorize CSS semantics inside `bbnf-simd`.

Sheets can consume digit blocks for row/column coordinates, byte classes for
cell/reference lexing, and string-special scans for quoted cells or formulas.

BBNF-self can consume delimiter sets, identifier class runs, string-special
blocks, and tape cursor operations for grammar-source parsing.

JSON can consume all of the above, but JSON must not be the only witness unless
S-P3 labels the candidate as a JSON-specific generated template surface rather
than a generic primitive.

## Section 4 - Risks

- Generic codegen leak axes from S-P0 remain open until pruned. These
  candidates do not authorize adding new generic grammar branches.
- P2-F rejects any primitive whose proof is "this row got faster" without
  grammar-neutral API shape and same-wave consumer.
- CSS typed equality and cssparser same-workload retime are SK-V16 close
  criteria. A byte primitive may help, but cannot substitute for typed CSS API
  and equality proof.
- Dirty generated state cannot serve as proof of grammar-neutrality.
- Harness checksum, local FNV, and legacy CSS fact-stream proof paths are
  rejected as primitive sources. They have no grammar-neutral parser semantics.

## Section 5 - Sources

- `restart/locks/LOCKS.md:603`
- `restart/skinny/tranches/sk-v16/SYNTHESIS.md:62`-`93`
- `restart/skinny/tranches/sk-v16/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2e-parse-that-gaps.md`
