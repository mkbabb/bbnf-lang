# SK-V12 W1b-1 A1 - CSS Grammar Generation

Scope: read-only research for the CSS L4 declaration-values generated Track 1
scaffold. No source edits.

## Conclusion

The minimal legal W1b-1 scaffold is a scalar generated CSS declaration parser
that emits `css_l4_declaration_value_fact_stream` facts for
`css_l4/declaration_values/direct_to_struct/main`, plus an independent hand
oracle over the same fixture and fact stream. Do not use `stylesheet.bbnf` or
the generic `values.bbnf` entry as the row root. The semantic entrypoint should
be `properties.bbnf::declaration`, wrapped only if codegen needs an explicit
profile/root name such as `css_l4_declaration_values = declaration`.

W1b-1 does not claim lightningcss admission. It creates generated Track 1,
independent Track 2/oracle, strict equality, finite Mbps, generated-size
telemetry, and gate-consumed provenance. W1b-2 owns lightningcss throughput and
`> lightningcss_mbps + 1`.

## Minimal Scaffold

- Runtime module: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`.
- Generated Track 1 source: CSS-specific provider/profile, not JSON templates
  copied under a neutral name.
- Parser root: `declaration` from `grammar/css/l4/properties.bbnf`.
- Output plane: `css_l4_declaration_value_fact_stream`, with reviewable fact
  bytes, not digest-only authority.
- Fact stream minimum: normalized property name/id, declaration class, value
  token/span facts, `!important` flag, and input offset/checksum context.
- Oracle: hand-written CSS declaration fact extractor in `bbnf-bench`,
  independent from generated runtime and generated source.
- Consumer: `nonjson_css_l4` equality test and Criterion bench in the same
  wave.

## Grammar Findings

`properties.bbnf` is the right surface because it carries property dispatch,
custom-property handling, typed declaration groups, fallback declaration, and
`importantSuffix`. `values.bbnf` is broader generic value grammar; using it as
the root loses property identity and the declaration-level output plane.

Transitive reality: `properties.bbnf` imports `value-unit.bbnf`, `color.bbnf`,
`func-body.bbnf`, and `keywords.bbnf`. The SPEC owner list names only
`tokens`, `values`, `value-unit`, and `properties`; redress must either avoid
editing the transitive imports or revise the owner table before touching them.

## Generator Gaps / Blockers

1. `grammar::parse_grammar` currently skips `@import`; it does not resolve
   imported CSS files.
2. The skinny grammar parser does not implement all CSS BBNF syntax used here:
   comma sequence separators, `>>` / `<<`, `?w`, `@{...}`, `->`
   constants/host projections/type annotations.
3. `codegen::grammar_profile` registers only the `json` profile.
4. `json_sink_direct` is JSON-specific: `JsonSink`, object/array/string/
   number/bool/null dispatch, JSON literals, and JSON span assumptions.
5. `runtime/src/lib.rs` exports generated JSON only plus proof-only
   `sheets_witness`; no CSS module export exists.
6. The current SK-V12 non-JSON report gate validates `output_plane =
   direct_sink`, while W1b-1 requires `css_l4_declaration_value_fact_stream`.

## Owner Table Gaps

Likely plan-time owner additions before redress:

- `skinny/crates/codegen/src/grammar_profile.rs`
- a new CSS provider/template file under `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/xtask/src/main.rs` if adding a CSS-specific gate/check command

## Commands

Evidence command:

```sh
cargo test -p codegen grammar_profile_fields_are_consumed -- --nocapture
```

Expected redress commands:

```sh
cargo test -p codegen css_l4_declaration_values_profile_fields_are_consumed -- --nocapture
cargo test -p runtime css_l4_declaration_values -- --nocapture
cargo test -p bbnf-bench nonjson_css_l4_strict_equality -- --nocapture

CARGO_TARGET_DIR=/tmp/skv12-w1b1-target \
CRITERION_HOME=/tmp/skv12-w1b1-css-l4-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- nonjson/css_l4/declaration_values/direct_to_struct

RUSTFLAGS="-C target-cpu=native" \
cargo run -p xtask -- gate-json \
  --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json
```
