# G1 Gate Amendment — split byte-equivalence (ratified at G-Omega-wave gate, 2026-06-02)

## Finding (G1 research, restart/skinny/tranches/sk-v18/waves/g1/research.md)
`json/generated.rs` is a 3-source concatenation: template oracle (391 LOC) +
`JSON_PARSE_ONLY_GENERATED_RS` parse-only courier (353 LOC, an ITERATIVE explicit-stack
frame machine) + `json_sink_direct::render` (452 LOC, recursive descent, the 91.5% product).
The `SinkOnlyExpr` tree encodes a RECURSIVE lowering; demanding the recursive walk reproduce
the iterative parse-only courier BYTE-EXACT would force the exact L1 "grammar-driven banner over
hand-written blob" REJECT the campaign exists to prevent. Honest §6/feasibility finding.

## Amendment (SPEC §4 G1.2.1, ratified)
Split the byte-equivalence gate:
- **HARD byte-equivalence** retained for the `sink_direct` path (the 91.5% PRODUCT — must not
  move a byte; `parse_object_value_at_direct` + `parse_array_element_at_direct` byte-exact, same
  inline cfg, same sink.* call sites).
- **STRUCTURAL + BEHAVIORAL equivalence** for the recognizer-only parse-only path: 51/51 JSON
  strict held, `verbatim_blob_present == false`, the `.bbnf`-mutation falsifier fires, regen
  --check clean — the parse-only emit is derived as a recognize-only emit POLICY of the SAME
  walk (not a separate iterative blob, not a courier swap).

## Clarifications (folded)
- (a)-gate: `json.bbnf` has ZERO `->` arrows; the grammar-INVOKED seam is `RegexProgram`
  span_kind -> callee selection (string/number leaf scanners), not a literal `->` arrow. The
  named primitives are the EXISTING external callees (string: match_tiny_plain_string_direct +
  unescape_string; number: match_number_span_from_first + materialize_u64).
- P5/metalang: already ZERO for JSON at HEAD (the shipped file uses parse_number_*); the SPEC
  §3.5/§4 line-cites (801/841/881) are stale. P5 pre-satisfied for JSON.

## Disposition
Recorded as a G1 wave-level amendment; to be folded into SPEC §4 + the LOCKS §6 escape at the
next Pass Omega. Does NOT relax the un-fork or the grammar-derivation; relaxes ONLY the
over-strict byte-equivalence on the architecturally-distinct iterative recognizer path.
