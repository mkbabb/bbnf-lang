# PROVE Wave Research — the negative control fired (root-cause: the minimal IR parser)

Date: 2026-06-03. Wave: SK-V18 PROVE (§9, the Sheets negative control). Predecessors closed: G1–G4, G6
(G6 committed `369f4ffe7`, >SOTA 1.5–3.3× over lightningcss confirmed under the bench profile). Outcome
of research: the negative control surfaced a REAL structural finding, verified by direct code-read AND an
empirical lowering probe. The chosen disposition (ratified by the user) is the ROOT-CAUSE extension, not
a workaround and not a paper-close N.

## 1 — The finding: G3's un-fork is a unified DISPATCH, not a unified emitter
`runtime_generator.rs render()` dispatches `LoweredArtifact{SinkOnly|Scan}` to TWO grammar-shape-
specialized emitters — neither is a general grammar→parser emitter:
- **SinkOnly → `json_sink_direct`** requires a rule literally named `value` (`mod.rs:98`) and the five
  JSON arm kinds (`Container`/`Sequence`/`String`/`Number`/`Literal`-bool/null). Its `render()` calls a
  fixed JSON-structural sequence (`render_value_dispatch` / `render_container_rules` / `render_string_rule`
  / `render_number_rules`); the header hard-emits `use super::sink::JsonSink`; the prefixes are
  `parse_object_value_at_direct` / `parse_array_element_at_direct`. Grammar-derived ONLY over JSON-shaped
  grammars.
- **Scan → `css_scan::derive_alphabet`** (`css_scan.rs:487-572`) hard-requires rules named `ruleBlock`,
  `atRuleBody`, `cssString`, a `*Decl` in `properties.bbnf`, AND exactly 3 balanced pairs `{}()[]`. It
  emits a flat balanced-byte scan (the `flat-stream` shape), never a precedence tower. CSS-shaped only.

The Sheets precedence tower (`A = B (op B)*` × 7 levels + cyclic `paren_expr`) is structurally NEITHER —
it is the FIRST grammar that is not a relabeled JSON/CSS courier, exactly as §9 anticipated.

## 2 — The deeper blocker (the lowering probe verdict)
A throwaway probe (`codegen::prove_probe_sink_only`, fed the real `google-sheets.bbnf`) localized the
failure ONE layer below the emitter:
```text
google_sheets :: STAGE=parse_grammar ERR=BBNF-PARSE: unexpected token `-` at byte 329
json          :: STAGE=OK backend_rules=15 names=[…value…] sink_rules=15 has_value_rule=true
```
`skinny crates/grammar/src/lib.rs parse_grammar` is a MINIMAL IR parser — it admits only the
`"lit" /re/ (group) ref */+/?` shape and REJECTS the Sheets operators: `->` (31×, byte 329 is the
`number -> f64` leaf), `?w` (13×), `,` (42×). This is the SAME "lowering gap" the `css_scan.rs` header
documents as intentional — CSS only ever reaches a parser via its tolerant TEXT-harvest precisely because
`parse_grammar` cannot parse it either. JSON parses fully (15 rules → a complete `SinkOnlyProgram` with a
`value` rule) because JSON uses only the minimal shape.

**Consequence:** a general descent emitter is MOOT until the Sheets grammar parses into an AST. The
lowering (`lower::sink_only::lower_expr`) IS already generic over the full `SinkOnlyExpr` vocabulary
(`Seq`/`Alt`/`RepeatLoop`/`CallRule`/…) — the JSON-shape requirement lives in the EMITTER, not the
lowering. So the bottleneck is the FRONTEND parser, then the emitter; the middle (passes + lowering) is
believed general (to be confirmed by the understand pass).

## 3 — The two rejected alternatives
- **Tolerant tower-harvest** (a css_scan-style text-harvest of the Sheets grammar, no parser change):
  rejected — a SECOND text-harvest is a Sheets-shaped contrivance, a weaker generalization claim, and
  drifts toward the N line the §9 fallback reserves for genuine unworkability.
- **Outcome N now** (paper the negative control as "generalization not real"): declined by the user in
  favor of the real fix — the campaign ethos is architectural transposition for generality, not retreat.
  The binding fallback REMAINS armed: if the extension hits a genuine Sheets-specific contrivance
  (a precedence primitive that is hand-authored Sheets logic, a verbatim blob, a `GoogleSheets =>` arm),
  the honest close is N, surfaced — never paper-closed.

## 4 — The ratified disposition (root-cause)
EXTEND `skinny parse_grammar` to parse-and-record `->` (typed + value projections), `?w` (whitespace
modifier), and `,` (sequence separator) — the operators the facts-harvest (`parse_runtime_source_facts`)
ALREADY recognizes — so the Sheets structural AST builds. Then: lower the projections through the
existing passes/lowering, build a GENERAL recursive-descent emitter over arbitrary `SinkOnlyExpr`
dispatched by SHAPE (not grammar name), and emit a grammar-derived narrow G4 `Cursor`/`DocumentView`
view. JSON + CSS specialized emitters retained BYTE-IDENTICAL (the §9 preservation gate; `regen --check`
+ JSON 51/51 + CSS rich-parity are the guards). A principled benefit: closing the lowering gap at the
root lets CSS retire its text-harvest in a later wave.

The exact change-points (parser tokenizer, the passes projection flow, the emitter shape-dispatch, the
roster selection, the lock-14 owner-paths, the JSON/CSS preservation surface) are mapped in the
companion UNDERSTAND inventory before any frontend edit.
