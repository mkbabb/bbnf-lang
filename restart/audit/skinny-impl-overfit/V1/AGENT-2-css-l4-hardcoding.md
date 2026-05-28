# AUDIT-2: CSS-L4-Specific Hardcoding

Date: 2026-05-26. HEAD: 8e7378025. Hard cap: 30 min.
Axis: CSS-L4-specific hardcoding in runtime + codegen + xtask.

## Findings

### F-1 [CRITICAL] [PRUNE-REQUIRED] All seven CSS L4 "generated" runtimes are byte-identical copies of the same hand-written CSS tokeniser

- `md5 skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` returns the
  same digest `8675e262e9697b768e3546d1b83eb5dc` for all seven directories:
  `css_l4_at_rules_and_media`, `css_l4_declaration_values`,
  `css_l4_declaration_values_extended`, `css_l4_nested_layout`,
  `css_l4_stylesheet_selectors`, `css_l4_vendor_and_custom_atrules`,
  `css_l4_visual_functions`.
- `parser.rs` is also identical across the seven (md5 `8d7c2ad1...`), `sink.rs`
  identical (`2b46c08...`), `mod.rs` identical (`a851eb1b...`). Only `config.rs`
  varies, and only in metadata strings (FACT_SCHEMA, ROW_ID, OUTPUT_PLANE,
  REQUEST_PROFILE).
- All seven config files share `FRONTEND_SOURCE_HASH = "2d7df4eb87fb2304"`,
  `REQUEST_SOURCE_COUNT = 15`, `IMPORT_COUNT = 24`, `LAYOUT_DIRECTIVE_COUNT = 1`,
  `DISCARD_OPERATOR_COUNT = 107` (e.g.
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:12-16`
  vs `css_l4_stylesheet_selectors/config.rs:12-16`).
- The "seven runtime modules" are a topology illusion; there is exactly one
  parser implementation served seven times.

### F-2 [CRITICAL] [PRUNE-REQUIRED] The CSS "grammar-agnostic generator template" is a 646-line hardcoded CSS tokeniser string literal in codegen

- `skinny/crates/codegen/src/runtime_generator.rs:713` defines
  `const CSS_GENERATED_RS: &str = r#"..."#`, a raw string spanning lines 713
  through ~1359 (file end) containing a complete hand-written CSS tokeniser
  (`CssFullParser::parse_stylesheet`, `parse_at_rule`, `parse_qualified_rule`,
  `parse_block`, `parse_block_item`, `parse_declaration`, escape consumption,
  etc.).
- `runtime_generator.rs:80-101` renders the same `CSS_GENERATED_RS`,
  `CSS_PARSER_RS`, `CSS_SINK_RS`, `CSS_MOD_RS` constants for every grammar
  profile in the CSS profile set. `RuntimeGenerationRequest` plumbs grammar
  source bytes and a freshness hash but `emit_frontend_facts` never consults
  them when emitting the parser body.
- `runtime_generator.rs:107-150` defines a fixed `CssProfileConfig` table that
  only maps profile-id strings to FACT_SCHEMA / ROW_ID / OUTPUT_PLANE; any
  profile id not in this hardcoded table returns `runtime profile is not a
  frontend-facts runtime` (line 86-90).
- The post-W5 "frontend-generator" claim is therefore a paper close: providers
  in `skinny/crates/codegen/src/` were physically deleted (no `css_l4_*_provider.rs`
  files remain — only `runtime_generator.rs`, `direct_schema.rs`,
  `grammar_profile.rs`, `lib.rs`, `json_sink_direct.rs`, `grammar_provider.rs`,
  `json_typed_direct.rs`, `lower/`, `json_templates/`), but the CSS parser body
  was relocated into a string literal inside `runtime_generator.rs` rather than
  derived from any `grammar/css/l4/*.bbnf` source.

### F-3 [CRITICAL] [PRUNE-REQUIRED] All 24 "ADMITTED" CSS L4 rolling-delta rows share one identical measurement; per-feature differentiation is fabricated

- `restart/skinny/ROLLING-SOTA-DELTA.md:70-93` lists 24 CSS L4 rows. Every row
  carries identical `T1_current=2319.04`, `T1_sota=930.28`, `margin=1388.76`.
- `skinny/RESULTS.md` rows for these 24 features all carry the identical
  metrics column
  `track1_mbps=2319.041;cssparser_mbps=2362.037;lightningcss_mbps=929.281;profiled_bytes=54859728;profile_iters=8;corpus_bytes=979638;track1_profile_runs=28;wrong_plane_outputs=0;track1_errors=0`.
  This is one measurement replicated across 24 row labels, not 24 measurements.
- The 24 features map to only 7 runtimes (e.g.
  `css_l4/calc_expressions`, `css_l4/color_functions`,
  `css_l4/css_variables`, `css_l4/var_url_functions`,
  `css_l4/declarations` all route to
  `runtime::generated_css_l4_declaration_values_extended::parser::parse_full`),
  and the 7 runtimes are themselves byte-identical (F-1).
- The bench logic in `skinny/crates/bbnf-bench/src/css_l4_w8.rs:120-149`
  measures throughput across the 7 modules once (`TRACK1_PROFILES`, 7 entries)
  and then sets `admitted_rows = W8_SELECTED_CSS_ROWS = 24` (lines 16-17, 143-144)
  whenever the single measurement beats lightningcss by >1 Mbps. The 24 number
  is a constant, not 24 distinct admit events.

### F-4 [CRITICAL] [PRUNE-REQUIRED] Track 1 "full_parse" output is a four-integer summary, not a CSS AST

- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:50-101`
  shows `emit_full_parse` returning a single line
  `full_parse\tstatus=accepted\trules=N\tat_rules=N\tqualified_rules=N\tdeclarations=N`
  plus header lines.
- The `CssFullParseSummary` struct (lines 53-59) only counts `rules`,
  `at_rules`, `qualified_rules`, `declarations`. No selectors, properties,
  values, colors, gradients, transforms, or any of the named features are
  inspected or produced.
- The "beats lightningcss" claim is therefore comparing a brace-counter
  (skinny) against a full CSSOM builder (lightningcss). They are not the same
  workload.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs:229-242` invokes
  `StyleSheet::parse(source, ParserOptions::default())` for lightningcss
  (which materialises full typed selectors, properties, values) and is
  compared against `(profile.parse)(source)` for Track 1 (which counts
  rules / declarations only). The comparator-evidence row asserts
  `comparator_plane != "lightningcss full-parse"` would fail validation
  (`skinny/xtask/src/main.rs:1011`), but no comparator-parity is enforced;
  only schema-marker equality (`generated_full_parse_marker`,
  `css_l4_w8.rs:181-190`).

### F-5 [HIGH] [PRUNE-REQUIRED] xtask hardcodes the 24 CSS feature names and the lightningcss+cssparser comparator pair

- `skinny/xtask/src/main.rs:1333-1358` hardcodes
  `const SKV13_CSS_FEATURES: &[&str] = &[ "declaration_values", "declarations",
  "stylesheet_root", "selectors", ... 24 items ]`.
- `skinny/xtask/src/main.rs:1004-1029` (`is_skv14_w8r_css_row`) admits any
  CSS row only if `track1_entry_point.starts_with("runtime::generated_css_l4_")`,
  `track2_entry_point == "cssparser::StyleSheetParser full-parse probe"`,
  `comparator_plane == "lightningcss full-parse"`, and
  `track2_independence_status.starts_with("independent_verified:lightningcss+cssparser")`.
  The admit predicate is welded to two specific upstream parser crates and the
  generated-css-l4 runtime prefix.
- `skinny/xtask/src/regen_css.rs:25-93` hardcodes a 7-entry roster mapping
  profile ids to output dirs; deleting any of the 7 dirs without editing this
  file will break regen.

### F-6 [HIGH] [PRUNE-REQUIRED] regen_css test fence freezes the 7-module shape

- `skinny/xtask/src/regen_css.rs:148` `fn css_l4_roster_has_seven_distinct_companions`
  and line 164 `fn css_l4_roster_names_all_fifteen_sources` are tests that
  pin the architecture: exactly 7 runtime modules, exactly 15 grammar sources.
  Collapsing the 7 byte-identical clones into 1 module would require deleting
  these tests.

### F-7 [MEDIUM] [ACCEPT-AS-PROOF-OF-CONCEPT-IF-HONEST] Production corpus is wired but only as one fused measurement

- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-54` defines 4 real CSS
  files (bootstrap 5.3.3, tailwindcss 0.2.0, material-components-web 14.0.0,
  animate.css 4.1.1) totalling ~980 KB, with sha256 pinning and lightningcss
  parse-round-trip tests (`css_l4_sk_v14_corpora_parse_with_lightningcss`,
  lines 107-114).
- The corpus is loaded once and fed to all 7 profiles once
  (`css_l4_w8.rs:206-260`), then `profiled_bytes = corpus_bytes * 7 * 8`
  (= 54_859_728 = 4 corpora × 7 profiles × 8 iters). This inflates the divisor
  in the Mbps calculation by the duplicate-parse factor; since both Track 1
  and lightningcss are inflated equally, the *ratio* is honest, but the
  reported Mbps is the throughput of "running the same brace-counter 7 times
  per file" — not a per-feature measurement.

### F-8 [LOW] [ACCEPT] crates/core/src/runtime/css_l4/ (non-skinny path) is hand-written but out of skinny scope

- `crates/core/src/runtime/css_l4/{arena.rs, builder.rs, document.rs, mod.rs,
  parse_with.rs, value.rs, view.rs}` (7 files, 1766 lines, none marked
  `@generated`) is the pre-skinny CSS L4 runtime. This is hand-crafted under
  user latitude; it does not flow into SKINNY admit evidence.

### F-9 [HIGH] [CONTEXT] W4R rejection was real and W5C-GEN / W5D-DELETE landed, but the replacement is the same-shape paper artifact

- `git log --grep='sk-v14-w4r'` shows commit `4a32db45c
  docs(sk-v14-w4r): reject provider deletion cycle` — W4 PRUNE-2 deletion
  rejected because the existing W2 regen path still compiled through providers.
- `git log --grep='sk-v14'` shows the corrective sequence: `b19475486 feat(
  sk-v14-waveW5C-GEN): replace provider dispatch with request frontend
  generator`, then `b6f4d231b test(sk-v14-waveW5D-DELETE): authorize provider
  template deletion paths`, then in W5D the seven CSS provider modules and
  seven template dirs were physically removed from
  `skinny/crates/codegen/src/`.
- However, the W5R replacement design (described in
  `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:9-25`)
  required either "moving static provider/template bodies into a new file and
  calling that a generic generator" (workaround) OR "implementing a full
  source-consuming CSS L4 grammar parser/generator inside the W5 cap" (real
  fix). The implementation chose the first route: `CSS_GENERATED_RS` literal
  inside `runtime_generator.rs`. The packet explicitly flagged this as a
  workaround.

## 7 CSS Provider Modules: Status Post-W5

| provider module | exists at HEAD? | deletion commit | replacement path |
|---|---|---|---|
| `codegen/src/css_l4_at_rules_and_media_provider.rs` | no | W5D (commit b6f4d231b test gate; actual delete in subsequent W5D slice) | inlined as `CSS_GENERATED_RS` literal in `runtime_generator.rs:713-1359` |
| `codegen/src/css_l4_declaration_values_provider.rs` | no | W5D | same `CSS_GENERATED_RS` literal (byte-identical output) |
| `codegen/src/css_l4_declaration_values_extended_provider.rs` | no | W5D | same `CSS_GENERATED_RS` literal |
| `codegen/src/css_l4_nested_layout_provider.rs` | no | W5D | same `CSS_GENERATED_RS` literal |
| `codegen/src/css_l4_stylesheet_selectors_provider.rs` | no | W5D | same `CSS_GENERATED_RS` literal |
| `codegen/src/css_l4_vendor_and_custom_atrules_provider.rs` | no | W5D | same `CSS_GENERATED_RS` literal |
| `codegen/src/css_l4_visual_functions_provider.rs` | no | W5D | same `CSS_GENERATED_RS` literal |
| 7 × `css_l4_*_templates/` dirs | no | W5D | no template files exist; only `codegen/src/json_templates/` remains |

Net result: the source-tree topology changed (provider files removed); the
implementation topology did not (one hand-written CSS parser is still emitted
seven times into seven runtime directories whose `generated.rs`/`parser.rs`/
`sink.rs`/`mod.rs` are byte-identical).

## 24 CSS L4 Feature Admits

| feature row | admit commit | mechanism | Mbps vs lightningcss | overfit risk |
|---|---|---|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | 15a61bcbb sk-v14-w8r | identical full-parse summary (rules/declarations counter) | 2319.041 / 929.281 (+1389.76) | CRITICAL — brace-counter vs full CSSOM parse |
| `css_l4/declarations/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` string | 2319.041 / 929.281 | CRITICAL — same parser, same measurement |
| `css_l4/stylesheet_root/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — same parser |
| `css_l4/selectors/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — same parser |
| `css_l4/at_rules_keyframes/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — same parser |
| `css_l4/nested_rules/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — same parser |
| `css_l4/css_variables/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — same parser |
| `css_l4/calc_expressions/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — same parser; no `calc()` arithmetic implemented |
| `css_l4/var_url_functions/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no `var()` / `url()` typing |
| `css_l4/color_functions/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no color-space parsing |
| `css_l4/gradients/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no gradient stops parsed |
| `css_l4/transforms/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no transform matrices |
| `css_l4/filters/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no filter function parsing |
| `css_l4/easing_functions/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no cubic-bezier parsing |
| `css_l4/media_queries/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no media feature parsing |
| `css_l4/vendor_prefixes/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no vendor handling |
| `css_l4/custom_at_rules/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no `@property` etc. |
| `css_l4/pseudo_classes/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no `:is()`/`:where()` |
| `css_l4/pseudo_elements/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no `::part()` |
| `css_l4/attribute_selectors/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no `[a~=]` typing |
| `css_l4/logical_properties/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no logical-direction parsing |
| `css_l4/grid/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no grid track parsing |
| `css_l4/flexbox/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — no flex-basis typing |
| `css_l4/typed_property_groups/.../main` | 15a61bcbb sk-v14-w8r | same parser, different `ROW_ID` | 2319.041 / 929.281 | CRITICAL — there is no typed product |

All 24 rows are landed in the single commit `15a61bcbb feat(sk-v14-w8r-css-full-parse): admit CSS L4 rows on generated full-parse plane`. Verifying:
`git log --oneline | grep css_l4 | head` confirms this commit and `2cd3c333f
fix(sk-v14-w8r-ledger): accept CSS full-parse rows in manifest gates` as the
admit/ledger pair. No per-feature hand-tuning commits exist (contrast with the
W11A–W11W chain for JSON parse_only); CSS L4 is treated as one bulk row admit.

## Pattern H CSS L4 file census

Skinny runtime CSS L4 (the surface SKINNY measures):

| file | hand-written or generated | role | per-feature dependence |
|---|---|---|---|
| `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/{generated,parser,sink,mod,config}.rs` | rendered from `CSS_GENERATED_RS` literal | per-profile output dir | none — identical to other 6 |
| `skinny/crates/runtime/src/grammars/css_l4_declaration_values/{*}.rs` | rendered from `CSS_GENERATED_RS` literal | per-profile output dir | none — identical |
| `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/{*}.rs` | rendered | per-profile output dir | none — identical |
| `skinny/crates/runtime/src/grammars/css_l4_nested_layout/{*}.rs` | rendered | per-profile output dir | none — identical |
| `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/{*}.rs` | rendered | per-profile output dir | none — identical |
| `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/{*}.rs` | rendered | per-profile output dir | none — identical |
| `skinny/crates/runtime/src/grammars/css_l4_visual_functions/{*}.rs` | rendered | per-profile output dir | none — identical |
| `skinny/crates/codegen/src/runtime_generator.rs:713-end` (`CSS_GENERATED_RS`, `CSS_PARSER_RS`, `CSS_SINK_RS`, `CSS_MOD_RS`, `CssProfileConfig` table) | hand-written, marked `@generated` only in the *output*, not the source-of-truth template | sole CSS parser definition for skinny | hardcoded to 7 profile-id strings |
| `skinny/crates/bbnf-bench/src/css_l4_w8.rs` | hand-written | bench harness + W8R admit predicate | hardcodes `TRACK1_PROFILES` array of 7 entries, `W8_SELECTED_CSS_ROWS=24`, lightningcss + cssparser oracle wiring |
| `skinny/crates/bbnf-bench/src/css_l4_corpus.rs` | hand-written | 4-file production corpus loader | hardcodes 4 corpus specs + sha256 pins |
| `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` (3644 lines) | hand-written | legacy CSS L4 bench | extensive per-feature hardcoding |
| `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs` (318 lines) | hand-written | criterion entry | per-feature hardcoding |
| `skinny/xtask/src/regen_css.rs:25-93, 148, 164` | hand-written | regen driver + fences | hardcodes 7-profile roster + tests it stays at 7 |
| `skinny/xtask/src/main.rs:1004-1029, 1333-1358` | hand-written | admit predicate `is_skv14_w8r_css_row`, `SKV13_CSS_FEATURES` (24 strings) | hardcodes 24 feature names + lightningcss/cssparser comparator pair |

Census: 7 runtime grammar dirs × 5 files = 35 emitted files, all byte-identical
modulo per-profile metadata in `config.rs`; backed by 1 hand-written template
string in codegen and 1 bench harness. No `@generated` source-of-truth marker
in `runtime_generator.rs:713`; the literal is hand-edited.

## SOTA verification

The CSS rows in `skinny/RESULTS.md:112-...` (CSS feature rows; all 24 carry
the same metric block) report:

- `track1_mbps=2319.041` vs `lightningcss_mbps=929.281` → +1389.76 Mbps margin
  per `restart/skinny/ROLLING-SOTA-DELTA.md:70-93`.
- `cssparser_mbps=2362.037` (cssparser slightly *beats* Track 1 by 42.996
  Mbps in the same fused measurement).
- `profile_iters=8`, `profiled_bytes=54859728`, `corpus_bytes=979638`,
  `track1_profile_runs=28` (= 4 corpora × 7 profiles), `wrong_plane_outputs=0`,
  `track1_errors=0`.
- Verdict: Track 1 nominally beats lightningcss by ~2.5× on the brace-count
  workload. This number is structurally incomparable to lightningcss's full
  CSSOM build (F-4). The "beats lightningcss" claim in
  `restart/skinny/ROLLING-SOTA-DELTA.md` is comparing two different workloads
  and presenting the ratio as if same-workload.

## Verdict

PRUNE-REQUIRED.

The CSS L4 SKINNY surface exhibits the exact pre-restart pattern the user
flagged: a single hand-written CSS tokeniser disguised as 7 distinct generated
modules, used to claim 24 distinct feature admissions, against a comparator
that does substantially more work. Honest hand-crafted parsers are within user
latitude; **labelling one parser as 7 modules and 24 admits, and comparing
brace-counting against full CSSOM build to claim ">SOTA"**, is the same
"hardcoded grammars and backend files" failure mode that triggered the
restart.

The user's pin says "need to BEAT lightningcss". This bench does not — it
beats a different (cheaper) workload while calling it lightningcss-parity.

## Prune Recommendations

1. **Collapse the 7 byte-identical runtime modules to 1**
   (`skinny/crates/runtime/src/grammars/css_l4/`). Delete the per-profile
   directories and the `CssProfileConfig` table at
   `runtime_generator.rs:107-150`. Replace 7 W8R Track1Profile entries
   (`css_l4_w8.rs:60-89`) with a single entry. Re-state the rolling delta as
   1 row, not 24.
2. **Either** make `CSS_GENERATED_RS` truly derive from `grammar/css/l4/*.bbnf`
   sources (the route W5R explicitly flagged as the "real fix"), **or** move
   the literal back into a `runtime/src/grammars/css_l4/parser.rs` file with
   no `@generated` header. The current arrangement (paper-generated string in
   codegen) is the worst of both worlds: it can no longer be hand-edited
   without regen drift, but it adds nothing the hand-edit didn't have.
3. **Retire the W8R admit predicate** in `skinny/xtask/src/main.rs:1004-1029`
   or restate it as a single-row admit. Drop the
   `SKV13_CSS_FEATURES` 24-string array; keep at most the 1 row that the
   measurement actually represents.
4. **Replace the comparator** in `css_l4_w8.rs:206-260` with a workload that
   matches what skinny does. Either implement enough CSS typing to compete
   on `lightningcss::stylesheet::StyleSheet::parse` (full CSSOM), or compare
   against `cssparser::Parser` token consumption only (cssparser already beats
   skinny by 42 Mbps at that workload — admitting that fact is honest; hiding
   it under "wins vs lightningcss" is not). Cite both in RESULTS.md row text.
5. **Delete `regen_css.rs:148` and `:164` tests** that pin the 7-module / 15-source
   shape; they exist only to keep the topology illusion stable.

## Forward-lens note for the next S-P0

The W4R rejection + W5C-GEN + W5D-DELETE chain is an instructive case study:
a wave can land its named deliverable (codegen providers physically removed,
"replaced by" a frontend generator) while preserving the very pathology the
wave was meant to cure. The audit gate should check not "did the named files
get deleted?" but "is the *implementation* still per-feature hardcoded?" by:

- `md5sum`-class equivalence checks across generated runtime outputs that
  claim to differ;
- comparator-workload-parity checks (assert that Track 1 and the named
  oracle produce equivalent output shapes, not just that Track 1 produces
  *some* output);
- challenging any admit predicate whose `admitted_rows` constant exceeds the
  number of distinct measured implementations.

These checks would have flagged F-1, F-3, F-4 mechanically. They should
become S-P0 gates before any future CSS L4 wave dispatches.
