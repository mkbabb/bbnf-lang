# SK-V18 G2 — PROGRESS (CSS generator)

## Layout note (paths)
- cwd = `/Users/mkbabb/Programming/bbnf-lang/skinny`; codegen crate at
  `crates/codegen/`. Grammar at `/Users/mkbabb/Programming/bbnf-lang/grammar/css/l4/`.
  restart docs at `/Users/mkbabb/Programming/bbnf-lang/restart/...`.

## Done
- `crates/codegen/src/lower/css_scan.rs` — the grammar-AST alphabet derivation
  (`derive_program` → `CssScanProgram` + `ScanAlphabet`). Harvests `{ } ;` from
  `ruleBlock`/`atRuleBody`, `:` from a `*Decl` rule, `()`/`[]` from selectors,
  quotes from `cssString`, comment `/` from `@ws`. STOP-set accessors
  (at_rule/qualified/declaration/colon_before) + the fixed-9 SKIP set.
- `crates/codegen/src/css_scan_direct/{mod,drivers,projection}.rs` — the emitter
  (sibling of json_sink_direct). `render(program)` emits: alphabet consts (STOP
  arrays + SKIP), the flat `CssFullParser` driver loop calling the
  `css_balanced_component_scan` runtime primitive, the lazy rich projection
  (CssNode/CssRule/CssDeclaration/CssTypedValue/CssDocument/summaries +
  scan_value_end/number helpers). scan_value_end balanced bytes grammar-derived.
- `crates/runtime/src/runtime_scan.rs` — the `css_balanced_component_scan` +
  `css_consume_balanced` primitive ALREADY EXISTED (prior agent); blob-free,
  rides neutral `find_css_significant`. Wired in runtime lib.rs.
- `runtime_generator.rs::emit_request_facts` — now calls
  `lower::css_scan::derive_program` + `css_scan_direct::render`; the
  `CSS_GENERATED_RS` const is DELETED (file 1254→346 lines).
- Module wiring: `lower/mod.rs` (+css_scan), `lib.rs` (+css_scan_direct pub mod).

## STATUS (live)
- DONE: codegen builds green; regen-css writes 837-line grammar-derived provider.
- DONE: verbatim_blob_present == FALSE (grep r#" == 0 in generated.rs; CSS_GENERATED_RS const deleted).
- DONE: generated.rs calls css_balanced_component_scan w/ grammar-derived SKIP_ALPHABET
  [b'\'',b'"',b'/',b'(',b')',b'[',b']',b'{',b'}'] + STOP consts AT_RULE_STOP=b";{}",
  QUALIFIED_STOP=b"{};", DECLARATION_STOP=b";}", COLON_BEFORE_STOP=b":{};".
- DONE: runtime crate builds green.
- DONE: 9-field cssparser oracle parity EXACT — rich_cssom_matches_cssparser_on_fixture +
  rich_cssom_matches_cssparser_on_real_corpora (4/4 corpora) PASS.
- DONE: css_scan unit tests PASS incl. arg_mutation_falsifier_fires (drop atRuleBody `;` -> derivation rejects).
- IN FLIGHT: css_canon_bench >SOTA ratios (per corpus track1_rich/lightningcss).
- DONE: re-regen dropped unused import; workspace build green; check-css-l4 exit 0;
  css_l4_generated_runtimes_reproducible_from_request PASS.
- DONE: >SOTA all 4 corpora >1.0x (bootstrap 1.554, tailwind 2.020, mcw 1.487, animate 1.017).
- FIXED: 3 codegen tests that fed the placeholder W5C_CSS_SOURCE (which the old
  courier ignored) now use the real l4 grammar (w5c_full_css_request) — the body
  is genuinely derived so it needs the real structural rules. Deleted dead
  W5C_CSS_SOURCE + w5c_css_request helpers. Also w5a/w5b success-path calls.
- DONE: full codegen lib suite 34/34 green.
- DONE: END-TO-END .bbnf-mutation falsifier FIRES (mutate ruleBlock `{`->`<` ->
  regen FAILS; revert+regen -> byte-exact md5 f80ae72283eb0d15a9d23d6a4b0bbc6d restored).
- DONE: G2-LEDGER.md written.

## G2 COMPLETE — all exit-gate conjuncts green. Not committed (per mandate).

## Remaining (next steps)
1. Get `cargo build --profile ax-iter -p codegen` green (iterating compile errors).
2. `cargo run --profile ax-iter -p xtask -- regen-css` → regenerates
   `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`.
3. Inspect regenerated generated.rs; build workspace.
4. Parity: `cargo test --profile ax-iter` css_l4_parity / css_l4_canonical_parity
   / assert_rich_strict_equality (9-field cssparser oracle, 4/4 corpora).
5. >SOTA: `cargo run --profile ax-iter -p bbnf-bench --bin css_canon_bench` →
   track1_rich/lightningcss > 1.0x per corpus.
6. Falsifier: mutate stylesheet.bbnf ruleBlock/";", regen, confirm STOP consts
   change; revert+regen byte-exact.
7. Add css_scan unit test (derived bytes == expected; arg-mutation fires).
8. Write G2-LEDGER.md telemetry.

## §6 FINDING (lowering-gap, deeper than AMENDMENT anticipated)
The skinny `grammar::parse_grammar` IR parser admits ONLY the minimal
`"lit" /re/ (group) ref */+/?` shape — it REJECTS the CSS operators `,` `->`
`>>` `<<` `?w`. So CSS has NO full AST in the skinny tree (the frontend
`parse_runtime_source_facts` is a fact SCANNER, not an AST builder). The
AMENDMENT assumed "parse request.sources into the rule AST" — impossible via
parse_grammar. RESOLUTION: css_scan.rs performs a faithful CSS-tolerant
STRUCTURAL harvest — splits each source into `name = body ;` rules, reads each
rule body's TERMINAL set (quoted literals + regex patterns + refs), treats CSS
operators as inert. The alphabet is read off the grammar TERMINALS, so a .bbnf
mutation of an invoking rule's literal changes the harvested byte (falsifier
holds). This is a genuine grammar derivation, not regex-over-text.

## Key facts
- find_css_significant requires `delimiters.len() <= 4` — colon_before_stop is
  exactly 4 bytes (`: { } ;`). OK.
- Primitive returns `ScanError`; emitted drivers map via `scan_error_into`.
- SKIP set MUST equal `b"'\"/()[]{}"` (the runtime lib test FIXED const).
- Do NOT commit.
