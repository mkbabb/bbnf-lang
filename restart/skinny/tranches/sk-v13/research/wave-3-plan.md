# SK-V13 W3 Plan - CSS Declaration-Value Expansion

Wave: W3. Phase: Plan. Date: 2026-05-21.

## Selected Intervention

Land `css_l4/declaration_values_extended/direct_to_struct/main` as a generated
CSS L4 extended declaration-value fact-stream row. The row is separate from the
SK-V12 declaration-values admit, which remains a maintain guard.

The W3 row moves these rolling CSS feature rows when its grouped report passes:

- `declarations`
- `css_variables`
- `calc_expressions`
- `var_url_functions`
- `color_functions`

Quoted strings and escaped identifiers are required facts inside the grouped W3
row but are not standalone rows in the current 24-feature rolling matrix.

## Fixture And Facts

W3 fixture:

```css
:root { --brand-\31: rgb(255 128 0 / 50%); --gap: calc(100% - 2rem); }
.card { width: calc(var(--gap, 10px) + clamp(1rem, 2vw, 3rem)); color: color-mix(in srgb, var(--brand-\31) 80%, white); background-image: url("/assets/bg\\ space.svg"); mask-image: url(/assets/mask.svg); content: "escaped\\Aline"; }
```

The fixture is strict-mode friendly and covers:

- custom property declaration and escaped custom-property identifier;
- nested `calc()`, `var()`, and `clamp()` recursion with a W3 recursion cap;
- color functions (`rgb`, `color-mix`);
- quoted and unquoted URL forms;
- quoted string content and escaped string content;
- existing numeric, percentage, dimension, identifier, delimiter, comma, and
  parenthesis token families.

The output plane is `css_l4_declaration_value_extended_fact_stream`. The fact
schema is `css-l4-declaration-value-extended-facts-v1`. Track 1, cssparser
oracle, and lightningcss facts must be byte-identical. The cssparser oracle is
the independent token/fact reference; lightningcss is the strict same-plane
SOTA anchor.

Escaped-token facts use normalized lexeme bytes rather than claiming source
spans derived from normalized values. Declaration-level source ranges may
remain because they are found by the generated scanner before token
normalization.

## Owner Paths

Redress may edit only:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/*`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/*`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w3/*`
- `restart/skinny/tranches/sk-v13/research/wave-3-redress.md`
- `skinny/REDRESS.md` only if W3 rejects.

## Gate

`G-W3-CSS-DECLARATION-VALUES-EXTENDED` passes only when all are true:

1. `--skv13-css-declaration-values-extended-report` is supplied with
   `cargo xtask gate-json --check-results`; update/probe combinations reject.
2. The W3 report validates schema, row identity, covered feature rows, fixture
   checksum, generated module checksum, threshold math, artifact paths, and
   source isolation.
3. The gate rereads Criterion lanes in a separate W3 group:
   `track1_generated_css_l4_decl_values_extended`,
   `track2_cssparser_decl_values_extended_oracle`, and
   `lightningcss_decl_values_extended_same_plane_fact_stream`.
   Report-only Mbps is rejected.
4. Track 1 exceeds `lightningcss_mbps + 1.0`, and Track 1, cssparser oracle,
   and lightningcss retained fact artifacts are byte-identical.
5. `RESULTS.md` contains the grouped W3 admission row and the five covered
   feature rows with the same evidence, while `ROLLING-SOTA-DELTA.md` consumes
   the five feature rows without demotion.
6. Existing SK-V12 declaration-values, W2 stylesheet/selectors, and JSON guard
   rows maintain through the same `gate-json --check-results` invocation.
7. Lock 14 owner-path proof admits only the W3 CSS-specific profile paths; no
   generic JSON string, number, or source-map policy enters generic crates.

## Revert

On reject, revert the W3 codegen/runtime/bench/gate/RESULTS/rolling slice and
record a REDRESS entry naming the failed extended declaration-value family and
artifact. No W3 feature row may remain `ADMITTED` without the grouped W3 row
passing the gate.
