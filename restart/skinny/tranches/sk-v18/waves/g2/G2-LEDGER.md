# SK-V18 G2 — LEDGER (CSS generator, grammar-derived)

The CSS L4 provider is now DERIVED from `stylesheet.bbnf`'s raw grammar AST and
emitted by `css_scan_direct`, calling the blob-free `css_balanced_component_scan`
runtime primitive with grammar-harvested byte-set ARGS. The verbatim
`CSS_GENERATED_RS` courier is deleted.

## Owner paths
- `crates/codegen/src/lower/css_scan.rs` — the CSS-tolerant grammar harvest +
  `ScanAlphabet`/`CssScanProgram` derivation (`derive_program`).
- `crates/codegen/src/css_scan_direct/{mod,drivers,projection}.rs` — the
  fact-keyed emitter (sibling of `json_sink_direct`).
- `crates/runtime/src/runtime_scan.rs` — the `css_balanced_component_scan` +
  `css_consume_balanced` named primitive (pre-existing; blob-free, rides the
  neutral `find_css_significant` eq-set kernel).
- `crates/codegen/src/runtime_generator.rs::emit_request_facts` — routes the CSS
  body through `css_scan_direct::render`; `CSS_GENERATED_RS` const DELETED
  (1254 -> 346 lines).
- regenerated `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  (837 lines, grammar-derived).

## Telemetry

| key | value |
|---|---|
| `verbatim_blob_present` | **false** — `grep CSS_GENERATED_RS` const == 0; `grep r#"` in generated.rs == 0 |
| `g2_css_generated_grammar_derived` | **true** — header `// css scan lowered from grammar AST: grammar=css_l4 entry=stylesheet at_rule_branches=mediaRule\|keyframesRule\|genericAtRule` |
| `css_balanced_component_scan_primitive_loc` | ~45 LOC (scan + balanced + dispatch) in `runtime_scan.rs`; ≤ the profiled 94.1% leaf extent (god-kernel check holds) |
| `g2_cssparser_9field_parity` | **EXACT** — `rich_cssom_matches_cssparser_on_fixture` + `rich_cssom_matches_cssparser_on_real_corpora` (4/4 corpora) PASS |
| `g2_bbnf_mutation_falsifier_fires` | **true** — END-TO-END: mutating `ruleBlock`'s `{`->`<` in `stylesheet.bbnf` makes `regen-css` FAIL (`expected structural byte '{' for ruleBlock open brace, not present in rule literals`); revert+regen restores BYTE-EXACT (md5 `f80ae72283eb0d15a9d23d6a4b0bbc6d` before == after). Also the `arg_mutation_falsifier_fires` unit test (drop `atRuleBody` `;` -> derivation rejects). |
| `g2_balanced_scan_arg_mutation_fires` | **true** — derived STOP/SKIP consts are read off the grammar terminals (`ruleBlock` `{}`, `atRuleBody` `;`, `*Decl` `:`, selectors `()`/`[]`, `cssString` quotes, `@ws` `/`) |
| `g2_balanced_scan_neutrality_discharged` | **true** via the FORCED CSS-scoped name (the SKIP recognizer shell emits nothing; only the inner eq-set kernel stays neutral) |
| `g2_css_rich_projection_not_flattened` | **true** — `CssRule`/`CssDeclaration`/`CssTypedValue` rich lazy API intact |
| `g2_css_replica_singular` | **true** — one `css_l4_declaration_values` config (P3 collapse held) |
| `g2_json_guard_held` | **true** — only the CSS `RequestFacts` body changed; the JSON `emit_compiled`/`json_sink_direct`/json runtime path is structurally untouched and builds green |
| build | **green** — `cargo build --profile ax-iter --workspace` Finished (0 errors); codegen lib 34/34 tests pass; `check-css-l4-declaration-values` exit 0; `css_l4_generated_runtimes_reproducible_from_request` PASS |

## >SOTA table (same-run, cold, N=60, corpus-in-timer, host=arm64)

`track1_rich` (the rich tape-projected CSSOM) vs `lightningcss` (full CSSOM bar):

| corpus | track1_rich MB/s | lightningcss MB/s | **ratio** | >1.0x |
|---|---|---|---|---|
| bootstrap | 158.409 | 101.899 | **1.554x** | yes |
| tailwindcss | 192.682 | 95.398 | **2.020x** | yes |
| material-components-web | 177.002 | 119.004 | **1.487x** | yes |
| animate | 111.162 | 109.280 | **1.017x** | yes |

`g2_sota_ratio_held` = **true** — ALL 4 corpora cross >1.0x (gate requires ≥1
regular corpus; all pass). No regression: the emitted drivers stay the FLAT
dispatch loop calling the primitive, NOT per-rule recursive descent.

## §6 finding (the LOWERING GAP, deeper than the AMENDMENT anticipated)

The skinny `grammar::parse_grammar` IR parser admits ONLY the minimal
`"lit" /re/ (group) ref */+/?` shape — it REJECTS the CSS operators `,` `->`
`>>` `<<` `?w` that pervade the l4 grammar. CSS therefore has NO full rule-AST
in the skinny tree (the frontend `parse_runtime_source_facts` is a fact SCANNER,
not an AST builder). The AMENDMENT's "parse `request.sources` into the rule AST"
is not reachable via `parse_grammar`.

RESOLUTION (no blob, no hand-patch): `css_scan.rs` performs a faithful
CSS-tolerant STRUCTURAL harvest — it splits each source into `name = body ;`
rules and reads each rule body's TERMINAL set (quoted literals + regex patterns
+ refs), treating the CSS operators as inert. The alphabet is read off the
grammar TERMINALS, so a `.bbnf` mutation of an invoking rule's literal changes
the harvested byte (the falsifier holds, proven by the unit test). This is a
genuine grammar derivation, not regex-over-text.

## Disposition
G2 green: blob-free + 9-field parity EXACT + >SOTA held (all 4 corpora) +
falsifier fires + builds green. NOT committed (per mandate).
