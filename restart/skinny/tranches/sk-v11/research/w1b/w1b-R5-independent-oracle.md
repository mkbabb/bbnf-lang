# SK-V11 W1b R5: Independent Oracle / Track 2

Date: 2026-05-20.
Scope: research-only pass for W1b Phase 1, R5 independent oracle / Track 2. Read SPEC Section 5 and W1b/P3 materials; inspect available CSS L4 runtime, builder, tests, existing oracle patterns, and candidate strict-equality routes for one generated non-JSON direct or typed row without the oracle calling generated Track 1.
Output: this file only.

## Finding

V2 supersession note: Phase 2 and CHALLENGE select
`css_l4/declaration_values/direct/main` on
`css_l4_declaration_value_fact_bytes`. The original R5 typed-route
recommendation below is retained as oracle-strength research, but it is
superseded for W1b dispatch because current skinny W1b cannot produce an
admissible generated typed CSS Track 1 or add the required dependency/manifest
owners. W1b's selected oracle is `css_l4_decl_value_fact_oracle` on direct
fact bytes.

Selected oracle path after Phase 2: `css_l4/declaration_values/direct/main`
with a same-run independent `css_l4_decl_value_fact_oracle` fact-byte stream.

The row should compare byte-identical canonical fact streams, not parser ASTs and
not pretty-printed full stylesheets. Track 1 is the generated CSS L4 typed parser
row. The oracle parses the same selected CSS corpus with `lightningcss`, walks its
own AST, emits the same small fact schema, serializes that schema with a stable
non-lossy encoder, and compares bytes with Track 1's separately produced fact
stream. The oracle implementation must live outside generated Track 1 and must
not call `CssL4Parser::parse`, `CssL4Parser::stylesheet_prettify`,
`runtime::css_l4::parse_with`, generated SinkOnly helpers, generated typed
helpers, or old hand-only non-JSON runtime code.

Use direct fact bytes rather than a digest plane for W1b. A direct digest row is
possible, but it adds hash/collision/sink coupling before W7's output-sink lane
and can hide a mismatch behind a digest contract. A direct fact-byte stream is
smaller, stricter, easier for the gate to explain, and lines up with the
selected non-JSON target: CSS L4 declaration values.

## Contract Read

SPEC Section 5 makes W1b a baseline/oracle lane, not an intervention lane. It
must create exactly one generated non-JSON direct or typed parser baseline row,
name an independent Track 2/oracle, prove strict output equality, prove gate
consumption, and prove `json_provider` policy does not leak into the selected
generated parser (`restart/skinny/tranches/sk-v11/SPEC.md:327-368`). The
cross-cutting independence gate requires the Track 2/oracle source path and
forbids calls into generated Track 1, generated SinkOnly helpers, generated typed
helpers, or hidden shared parser code (`restart/skinny/tranches/sk-v11/SPEC.md:247-252`).

P3 repeats the same constraints:

- P3-B sequences W1a -> W1b -> W2; W1b creates exactly one generated non-JSON
  baseline plus independent oracle, and W2 consumes it (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:64-86`).
- P3-C's `G-W1b-NONJSON-BASELINE` requires generated Track 1 Mbps, independent
  Track 2/oracle Mbps, strict output equality, provenance fields, and gate
  consumption; no intervention or row admission is allowed (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:75-80`).
- P3-D allows non-JSON grammar ids `css_l4`, `sheets`, and `bbnf_self`, with
  `css_l4_bench` domains, SPEC-named non-JSON workloads, a same-plane
  independent oracle, and a consumed `track2_independence_status`
  (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:154-172`).
- P3-E blocks W1b from behavior intervention, coupled oracle evidence, JSON
  provider claims, old hand-runtime proof, and row admission from baseline
  creation (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:160-164`).

W1a already prepared the report shape for a future CSS row. The fixture-level row
identity is `css_l4/declaration_values/direct/main` or
`css_l4/declaration_values/typed/main`, with `sheets` and `bbnf_self` fallbacks
(`restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md:52-87`).
The live W1a validator currently recognizes `css_l4/declaration_values`,
`sheets/formula`, and `bbnf_self/grammar`, including direct and typed fixture
identities, but only as schema-only W1a evidence
(`skinny/crates/bbnf-bench/src/report.rs:1762-1837`).

## Available CSS L4 Surface

The full repo already has a CSS L4 struct-direct runtime under
`crates/core/src/runtime/css_l4/`. Its module contract says the generated parser
writes directly into `CssDocument` via `CssStructBuilder`, with no tape symbols in
the CSS L4 parse path (`crates/core/src/runtime/css_l4/mod.rs:1-10`). The document
exposes `walk_declarations()` and `walk_values()`, which are exactly the current
typed-value traversal surface (`crates/core/src/runtime/css_l4/document.rs:137-185`).

The builder is specialised enough for a declaration-values typed row:

- `finalise()` returns a `CssDocument` over an arena-backed `StyleSheet`
  (`crates/core/src/runtime/css_l4/builder.rs:270-284`).
- declaration frames cover `declaration`, `customPropertyDecl`, `genericDecl`,
  and the typed `*Decl` family (`crates/core/src/runtime/css_l4/builder.rs:439-450`).
- declaration finalisation projects zero, one, or many parsed values into
  `CssTypedValue::Span`, a single typed value, or an arena-backed value list
  (`crates/core/src/runtime/css_l4/builder.rs:629-653`).
- scalar leaves route into typed values: `f64` numbers, `i64` integers, packed
  `u32` colors through `push_leaf_with_u64`, and string/span values through
  `push_leaf_with_str` (`crates/core/src/runtime/css_l4/builder.rs:853-945`).
- `Declaration` carries `property`, `value`, and `important`, and `CssTypedValue`
  is the declared typed value alternation (`crates/core/src/runtime/css_l4/value.rs:716-828`).

The existing CSS tests prove useful surfaces but do not by themselves satisfy
W1b independence:

- `lightningcss_parity.rs` already uses `lightningcss` as an external parser and
  projects color values from both sides, but the bbnf side calls generated
  `CssL4Parser::parse`, so this is a parity harness, not the independent oracle
  implementation (`crates/core/tests/lightningcss_parity.rs:54-90`,
  `crates/core/tests/lightningcss_parity.rs:161-249`).
- `css_l4_canonical_parity.rs` compares bbnf prettify output against
  `lightningcss` canonical output through a shared `token_normalize`, but it only
  has byte equality for `normalize.css`; larger fixtures are scale/interop only
  (`crates/core/tests/css_l4_canonical_parity.rs:1-19`,
  `crates/core/tests/css_l4_canonical_parity.rs:115-196`).
- `css_l4_parity.rs`, `css_l4_dimensions.rs`, and
  `css_l4_named_color_parity.rs` validate typed graph materialization, but they
  either inspect bbnf's own typed graph or derive expected values from the grammar
  source. Those are regression tests, not an independent Track 2 oracle
  (`crates/core/tests/css_l4_parity.rs:70-88`,
  `crates/core/tests/css_l4_named_color_parity.rs:18-99`).
- `crates/core/benches/css/l4.rs` benchmarks `CssL4Parser::parse` over
  normalize/bootstrap/tailwind, and `crates/core/benches/css/competitors.rs`
  already benchmarks `lightningcss::StyleSheet::parse` separately
  (`crates/core/benches/css/l4.rs:144-178`,
  `crates/core/benches/css/competitors.rs:148-202`).

Skinny does not yet have a non-JSON generated runtime lane. Its codegen profile
guard accepts only `backend.grammar_name == "json"` and both normal and typed
emission call that guard (`skinny/crates/codegen/src/json_provider.rs:4-13`,
`skinny/crates/codegen/src/lib.rs:102-168`). The skinny runtime exports generated
JSON plus proof-gated witnesses, not a generated CSS L4 runtime
(`skinny/crates/runtime/src/lib.rs:1-16`). W1b therefore cannot claim non-JSON
baseline authority from the current skinny generator until it creates or names a
real generated non-JSON row.

## Candidate Oracle Routes

1. CSS L4 direct declaration-value fact stream.

   This is the selected W1b route. Track 1 serializes a small fact stream from
   the generated CSS L4 direct output, for example:

   ```text
   DeclFact {
     ordinal,
     property,
     important,
     value_kind,
     scalar_payload_bits_or_canonical_string,
   }
   ```

   The oracle parses the same corpus through an independent W1b source module,
   projects only the agreed subset needed for the row, and emits the same
   `DeclFact` stream. The W1b corpus should be curated to the intersection where
   both sides have a precise, stable interpretation: property names, important
   flags, scalar values represented as exact fact bytes, and raw value spans when
   the selected fact schema permits them.
   Avoid shorthand expansion, property canonical reordering, calc simplification,
   vendor-specific recovery, and declarations whose equality needs CSS cascade
   knowledge.

   Strict equality is byte equality of the encoded fact streams. The gate
   records `output_plane = css_l4_declaration_value_fact_bytes`,
   `comparator_plane = css_l4_declaration_value_fact_bytes`, `comparator_id =
   css_l4_decl_value_fact_oracle`, `comparator_freshness = same-run-oracle`,
   `sidecar_freshness = n/a`, and `track2_independence_status =
   independent_verified`.

2. CSS L4 direct digest fact stream via `lightningcss`.

   A direct row can hash the same `DeclFact` stream on both sides and compare the
   final digest. This is workable only if the full fact bytes are also available
   on mismatch and the digest algorithm is treated as output-sink evidence, not
   parser semantics. It is less desirable for W1b because C8/W7 already reserve
   digest/hash work as an oracle or host sink, and digest-only output can obscure
   a value-level divergence.

3. Full canonical CSS output: bbnf prettify versus `lightningcss::to_css`.

   Existing code already explores this. It is not the recommended W1b oracle
   because it calls generated `stylesheet_prettify`, not the generated direct or
   typed parser row, and because `token_normalize` contains many semantic
   cancellation rules. It is useful as a supplemental interop test but too broad
   and too adapter-heavy for the first strict W1b baseline.

4. Admission/counting oracle using `cssparser` or `lightningcss` rule counts.

   This is parse-plane or counting-plane evidence, not the same output plane.
   Existing `lightningcss_parity.rs` explicitly rejects declaration counting as
   the wrong structure for parity. It cannot prove strict output equality for a
   generated direct or typed row.

5. Grammar-derived host map oracle for named colors / hex colors.

   This can validate individual payloads, but it is coupled to bbnf grammar
   source and host shim semantics. It is a regression helper, not an independent
   Track 2 oracle.

6. Sheets or BBNF-self fallback.

   Sheets formulas and BBNF-self remain legitimate fallbacks per SPEC, but the
   currently inspected surfaces show a better first path for CSS L4: external
   `lightningcss` parser availability, existing CSS corpus, existing typed
   declaration traversal, and existing CSS parity tests. Sheets would need a
   formula evaluator or parser oracle with equally explicit same-plane facts.
   BBNF-self risks self-host coupling unless the oracle is a separate parser with
   a bounded output schema.

## Recommended Implementation Shape For W1b

Create a W1b-specific oracle/report path, not a broad relaxation of JSON
`gate-json`:

- Row id: `css_l4/declaration_values/direct/main`.
- Output plane: `css_l4_declaration_value_fact_bytes`.
- Track 1 source path: the selected generated CSS L4 direct parser row and its
  fact-byte serializer. In skinny this requires real generated non-JSON
  emission; do not reuse old hand runtime as generated authority.
- Oracle source path: a new non-generated declaration-value fact oracle module in
  the W1b owner surface, for example
  `skinny/crates/bbnf-bench/benches/nonjson_oracles/css_l4_decl_value.rs`. The
  Criterion bench may call this module, but parser and fact projection logic
  must not be hidden in the benchmark body. It must not import generated Track 1
  modules.
- Equality artifact: store or render both stable fact-stream hashes plus a
  mismatch dump path, but make the pass predicate byte equality of the fact
  stream, not hash equality alone.
- Gate consumer: extend the W1a companion non-JSON report validator for W1b
  baseline semantics, changing `measured_validation_path` from `schema-only` to
  `measured-row` and `same_wave_consumer_class` to a generated CSS L4 typed
  parser consumer. Keep JSON W0 validation unchanged.
- Throughput: benchmark Track 1 and oracle under the same run id, host, flags,
  feature mask, and sample count. W1b records baseline Mbps only; no intervention
  admits until W2.

The curated W1b corpus should be intentionally boring and strict. Examples:

```css
a { color: #ff00ff; width: 50%; opacity: .5; margin-left: -10px; }
b { background-color: rgb(255 128 0 / 0.5) !important; }
@media (min-width: 640px) { c { height: 100px; color: red; } }
```

If `lightningcss` canonicalizes a value in a way that cannot be projected without
CSS semantic evaluation, remove that value from W1b. W1b needs one strict row,
not broad CSS coverage.

## Coupling Risks

1. Oracle calls generated Track 1. Any oracle helper that imports
   `CssL4Parser`, generated CSS modules, generated SinkOnly helpers, generated
   typed helpers, or `runtime::css_l4::parse_with` fails SPEC Section 2.3.
   Parity tests may call both sides, but the Track 2/oracle implementation itself
   cannot call Track 1.

2. Shared serializer hides producer bugs. A common fact encoder is acceptable only
   if it receives already-materialized facts from each side. Do not share value
   projection logic that maps bbnf `CssTypedValue` and `lightningcss` AST nodes
   through the same parser-shaped helper. Keep two projection modules and one
   trivial stable byte encoder.

3. `token_normalize` becomes a comparator bridge. The existing CSS canonical
   normalizer has many semantic cancellation rules. Reusing it for W1b would make
   equality depend on an adapter that can erase real differences. The typed
   declaration fact stream should avoid source-level normalization except for
   explicitly typed scalar canonicalization such as f64 bits, lowercased property
   names when both parsers define ASCII-insensitive property identity, and exact
   RGBA integer tuples.

4. Grammar-derived expected values are not independent. Loading
   `grammar/css/l4/color.bbnf` to compute expected named colors proves the
   generated parser follows its grammar, not that Track 2 is independent. The
   W1b oracle should source named colors from `lightningcss`'s parsed value model
   or from an explicitly external CSS color table if needed.

5. `lightningcss` may canonicalize more CSS than bbnf. Shorthand expansion,
   declaration reordering, color folding, calc simplification, media query range
   rewriting, and vendor recovery can create false mismatches or false
   equalities. The mitigation is a narrow W1b corpus and fact schema, not a
   growing normalizer.

6. Old core CSS runtime proof is not skinny generated authority. The repo has a
   mature CSS L4 runtime in `crates/core`, but W1b owner paths are the skinny
   generator/runtime/bench surfaces. A W1b row must be generated in the selected
   lane or explicitly approved as the generated Track 1 source; hand-only or
   legacy non-skinny runtime evidence is pre-blocked.

7. JSON provider bypass can become paper generality. Skinny currently gates
   runtime generation through `json_provider::ensure_runtime_profile`. W1b must
   either replace/bypass that path for the selected non-JSON generator with a
   real CSS target or prove it is untouched. A renamed JSON helper or a JSON
   template with CSS labels is not a Lock 14 proof.

8. Digest-only output weakens typed proof. If the row is direct/digest, require
   full fact-stream bytes as the oracle comparison substrate and make the digest a
   reporting artifact. Do not let output hash equality stand in for typed output
   equality.

9. Producer-only telemetry can return. W1a's core lesson still applies: every
   non-JSON field, oracle source, strictness flag, sample field, and independence
   claim must be consumed by the same W1b gate. A companion report is safer than
   inserting non-JSON rows into `skinny/RESULTS.md` until the JSON snapshot path
   is explicitly extended.

## Verdict

Use `css_l4/declaration_values/direct/main` and an independent
`css_l4_decl_value_fact_oracle` that emits a stable declaration-value fact stream
without calling generated Track 1. This is the selected W1b route after Phase 2:
it uses the preferred CSS L4 target, stays inside the current owner surface, can
prove strict byte equality if generated Track 1 exists, keeps oracle logic
separate from generated Track 1, and creates only a baseline W2 can consume
without smuggling in an intervention.

Do not use full canonical CSS prettify, declaration counts, grammar-derived maps,
old hand runtimes, or digest-only equality as the primary W1b oracle.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R1-gate-validator.md`
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `crates/core/src/runtime/css_l4/mod.rs`
- `crates/core/src/runtime/css_l4/document.rs`
- `crates/core/src/runtime/css_l4/builder.rs`
- `crates/core/src/runtime/css_l4/value.rs`
- `crates/core/src/runtime/css_l4/arena.rs`
- `crates/core/benches/css/l4.rs`
- `crates/core/benches/css/competitors.rs`
- `crates/core/tests/lightningcss_parity.rs`
- `crates/core/tests/css_l4_canonical_parity.rs`
- `crates/core/tests/css_l4_parity.rs`
- `crates/core/tests/css_l4_dimensions.rs`
- `crates/core/tests/css_l4_named_color_parity.rs`
- `crates/core/tests/common/css_normalize.rs`
- `grammar/css/l4/properties.bbnf`
- `grammar/css/l4/value-unit.bbnf`
- `grammar/css/l4/color.bbnf`

Self-verdict: research-only. No source files edited; no baseline row created.
