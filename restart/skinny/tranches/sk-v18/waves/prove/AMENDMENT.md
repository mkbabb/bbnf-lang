# PROVE Amendment — root-cause frontend extension + general recursive-descent emitter (ratified 2026-06-03)

## Finding (waves/prove/research.md, verified by code-read + lowering probe)
G3's "un-fork" is a unified DISPATCH over two grammar-SHAPE-specialized emitters: SinkOnly
(`json_sink_direct`, requires a `value` rule + the 5 JSON arm kinds) and Scan (`css_scan`, requires
`ruleBlock`/`atRuleBody`/`cssString` + 3 balanced pairs, emits a flat byte-scan). The Sheets precedence
tower fits NEITHER. Deeper still, the skinny `parse_grammar` is a MINIMAL IR parser that REJECTS the
Sheets operators `->`/`?w`/`,`/`<<` — the same gap that forced CSS into a tolerant text-harvest. So the
negative control fired at the frontend, not just the emitter.

## Amendment (ratified — root-cause extension over the two narrower alternatives)
The user chose the ROOT-CAUSE path (over "tolerant tower-harvest" and "record outcome N"):
1. **Extend the frontend** so a precedence-tower grammar PARSES and LOWERS through the EXISTING pipeline:
   - `parse_grammar` parse-and-records `->` (→ `ExprKind::Project{body,target,ty}`), `<<`/`>>`
     (→ `ExprKind::Discard{left,right,keep_left}`), `?w` (→ a standalone `Annotation{ws}` sequence
     element), `,` (an explicit sequence separator), and accepts `@pretty`/`@ws` directives (recorded by
     the facts harvest, skipped in the AST). The new `ExprKind` variants lower TRANSPARENTLY (Project →
     body; Discard → `Seq[left,right]`) so the JSON/CSS grammars — which use none of these constructs —
     keep byte-identical output.
   - The nullable-repeat validator is relaxed: a nullable `Repeat` body (Sheets `(arg << comma?)+` with
     `arg = expression?`) is ALLOWED; loop-progress safety moves to the EMITTER (progress-guarded loops).
2. **A general recursive-descent emitter** over the EXISTING `SinkOnlyProgram` (Sheets lowers to one:
   39 rules, `entry=formula`, the 7-level tower intact), dispatched by SHAPE — a program with a JSON
   `value`-dispatch shape routes to `json_sink_direct` (byte-identical); else to the general emitter.
   No `match grammar {…}` arm; the dispatch reads the lowered program's shape.
3. **Routing**: `first_unsupported()` no longer treats the now-supported `->`/`?w`/`,`/`<<` as
   Scan-triggers; `@token` keeps CSS on its byte-identical Scan path; Sheets (no `@token`) routes to the
   SinkOnly artifact and thence the general emitter.

## Gate (unchanged from SPEC §9)
generator-only (no `SHEETS_GENERATED_RS` blob, no `GoogleSheets =>` arm); `generated_md5_distinct`;
`sheets_grammar_shape == pratt-operator` (≥7 chained level fns + cyclic `paren_expr`); the Sheets value
type instantiates the G4 `Cursor`/`DocumentView` seam; JSON 51/51 + CSS rich-parity + JSON/CSS
generated.rs byte-identical preserved throughout.

## Binding fallback (still armed)
If the general emitter cannot render the tower without a Sheets-specific contrivance (a verbatim blob, a
grammar-name arm, a hand-authored precedence primitive), the honest close is outcome N — surfaced, never
paper-closed.

## Disposition
Wave-level redesign, ratified; folds into SPEC §9 at the next Pass Omega. Status: the FRONTEND extension
is landed + verified (Sheets parses+lowers to a 39-rule SinkOnlyProgram; JSON/CSS byte-identical). The
general emitter + routing + grammar-derived G4 view + runtime wiring is the remaining slice.
