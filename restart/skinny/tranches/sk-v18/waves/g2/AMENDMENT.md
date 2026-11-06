# G2 Gate Amendment — CSS lowering path + split derivation-equivalence (ratified 2026-06-02)

## Findings (G2 research, waves/g2/research.md)
1. **Lowering-gap (decisive):** CSS LITERALLY cannot reach `lower::lower_to_rust`. `passes::compile`'s
   `first_unsupported` (grammar/src/lib.rs:214-234) REJECTS CSS's `@token` (stylesheet.bbnf:13), 8×
   `@pretty` (:53-60), and 113 `->` typed projections — that is WHY CSS is on the RequestFacts courier
   path. CSS also has no `SinkOnlyExpr` shape for a balanced scan.
2. **Byte-exact is impossible without the L1 reject:** `CssFullParser` is a FLAT balanced-delimiter
   scan; `stylesheet.bbnf` describes a RICH RECURSIVE CSSOM. A grammar-walk that reproduces the flat
   scan byte-exact would be the combinator-shaped recursive descent (= lightningcss's architecture =
   categorical >SOTA regress). NOTE: G2.2's exit gate already carries NO byte-equivalence conjunct.

## Amendment (ratified)
1. **Lowering path:** author a NEW `crates/codegen/src/lower/css_scan.rs` (+ `css_scan_direct.rs` per
   SPEC §5) that derives the CSS provider (the `find_component_delim`/`consume_balanced_at` scan, the
   `parse_block`/`parse_declaration`/`parse_at_rule` drivers, the lazy rich projection) from
   `stylesheet.bbnf`'s RAW grammar AST (`request.sources`), NOT the BackendIr `SinkOnlyProgram` path.
   DELETE `CSS_GENERATED_RS`. The structural alphabet is grammar-DERIVED: `{ } ;` from rule-blocks,
   `: :: ( ) [ ]` from selectors, `/*` from `@ws`, quotes from `cssString`.
2. **`css_balanced_component_scan` (§6 named primitive):** the 94.1% scan, grammar-INVOKED, taking
   grammar-DERIVED byte-set ARGS, blob-free, with the per-primitive arg-mutation falsifier (mutate the
   invoking rule -> emitted ARG byte sets change). The NEUTRALITY-PROOF FORCES the CSS-scoped name
   (`css_balanced_component_scan`): the byte-SKIP recognizer SHELL emits nothing and JSON/Sheets
   balancing is parse-WITH-emit (structurally incompatible) -> a neutral caller is UNREACHABLE; only
   the INNER alphabet-scan kernel (the `bbnf-simd` eq-set scan) stays neutral and is the G6 retarget seam.
3. **Split derivation-equivalence gate (mirrors G1):** HARD blob-free + arg-mutation for the primitive;
   STRUCTURAL+BEHAVIORAL equivalence for the drivers/projection = the 9-field cssparser oracle parity
   (EXACT) + the same-run `track1_rich/lightningcss > 1.0×` >SOTA gate (no regression vs pre-G2 captured
   at G2 entry in ONE quiet run) + the `.bbnf`-mutation falsifier + `regen --check` clean. NOT byte-exact.

## Disposition
Wave-level amendment, ratified; folds into SPEC §5 + the LOCKS §6 escape at the next Pass Omega. Does
NOT relax the un-fork or the grammar-derivation — it builds the CSS generator GENUINELY from the grammar
AST. The CSS-specific `css_scan.rs` is a SHAPE renderer the G3 un-fork later dispatches on BackendShape
(not a grammar-name fork); G3 reconciles the dispatch.
