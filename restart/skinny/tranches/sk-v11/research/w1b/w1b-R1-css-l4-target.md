# SK-V11 W1b R1: CSS L4 Target Viability

Pass: W1b Phase 1 research.
Date: 2026-05-20.
Scope: decide whether CSS L4 `declaration_values` direct or typed is the best
W1b generated non-JSON baseline target; inventory existing generated
parser/runtime pieces; name exact owner files future W1b redress would need.
Output: this file only.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 5 and the W1b/W2
  dependency clauses.
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md` W1b rows, gates, and
  preblocks.
- `skinny/REDRESS.md` REDRESS 111.
- W1a fixtures and gate implementation:
  `restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json`,
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.
- CSS grammar/runtime/codegen surfaces:
  `grammar/css/l4/*.bbnf`,
  `crates/core/src/grammar/generated/css_l4.rs`,
  `crates/core/src/runtime/css_l4/`,
  `crates/core/benches/css/`,
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`,
  `skinny/crates/codegen/src/lower/`,
  `skinny/crates/codegen/src/direct_schema.rs`,
  `skinny/crates/codegen/src/sink_direct.rs`,
  `skinny/crates/codegen/src/typed_direct.rs`,
  `skinny/crates/runtime/src/lib.rs`.

## Finding

V2 supersession note: Phase 2 and CHALLENGE keep the R1-selected target
`css_l4/declaration_values/direct/main`, but narrow the output plane to
`css_l4_declaration_value_fact_bytes`. R1 wording that permits digest-only
authority or `skinny/crates/bbnf-bench/src/track2/` ownership is superseded by
the revised W1b plan.

Recommended W1b baseline target: `css_l4/declaration_values/direct/main`.

Typed declaration-value facts are the better semantic oracle plane, but they are
not the best W1b baseline target under the current skinny implementation and
owner limits. A typed W1b row would need a real generated CSS typed Track 1 in
skinny. That does not exist today. The existing full-repo CSS L4 typed parser is
generated, but it is outside the W1b owner paths and depends on a specialized
hand runtime under `crates/core/src/runtime/css_l4/`; W1b preblocks old
non-JSON struct-direct modules as generated-baseline proof. Reusing that stack
would launder prior non-skinny runtime work into W1b authority.

Direct is the narrower viable baseline because W1b only needs a generated
Track 1, an independent same-plane oracle, strict equality, throughput, and gate
consumption. A direct digest or byte fact stream can be scoped to declaration
values without building the CSS typed graph. It still requires new generated
non-JSON Track 1 work, but it avoids the larger typed runtime/schema problem and
keeps W1b as a baseline lane rather than an intervention lane.

This is a viability call, not a W2 admission preference. W2 may still choose a
typed CSS intervention if W1b creates the baseline/oracle lane and CHALLENGE
approves the larger generated typed surface.

## Why CSS L4 Stays The Right Grammar

CSS L4 declaration values remain the best non-JSON grammar target. SPEC and
DISPATCH prefer CSS L4, then Sheets, then BBNF-self. The CSS grammar has real
value pressure: identifiers, strings, numbers, dimensions, colors, functions,
fallbacks, comments, and property dispatch. It exercises the generality axis
that JSON cannot exercise, and W2 is explicitly a CSS L4 generated
direct/typed intervention wave.

The selected row should stay smaller than full stylesheet parsing. Use
`declaration_values` as the corpus/workload name from W1a. The grammar source
can come from the existing value/declaration files:

- `grammar/css/l4/values.bbnf` for generic value forms.
- `grammar/css/l4/properties.bbnf` for declaration-level property/value
  dispatch.
- `grammar/css/l4/stylesheet.bbnf` only if the selected generated parser needs
  full stylesheet snippets as its syntactic wrapper.

If redress needs a dedicated entry rule, add one selected wrapper grammar input
under `grammar/css/l4/`, for example a declaration-values entry that imports the
existing files. Do not broaden to a second grammar or full CSS benchmark suite.

## Existing Pieces

### Full Repo CSS Stack

The root workspace already has a generated CSS L4 parser:
`crates/core/src/grammar/generated/css_l4.rs`. It exposes
`CssL4Parser::parse`, generated dispatchers, and prettify entry points. The root
manifest maps `css_l4` to `grammar/css/l4/stylesheet.bbnf` and binds
`CssL4Parser` to `crate::runtime::css_l4::CssStructBuilder` and
`CssDocument`.

The corresponding runtime under `crates/core/src/runtime/css_l4/` is a rich
typed struct-direct runtime. It has `CssStructBuilder`, `CssDocument`,
`CssTypedValue`, arena-backed lists, declaration/value walkers, and
`parse_with`. Existing tests prove useful CSS typed behavior, and the root
crate has `lightningcss`/`cssparser` comparators in tests and benches.

These pieces are inventory and oracle inspiration, not W1b Track 1 authority:
they are outside SPEC Section 5 owner paths, and the runtime is specialized
hand code. W1b should not use them as the generated skinny baseline.

### Skinny Generated Stack

Skinny codegen currently emits JSON only:

- `skinny/crates/codegen/src/lib.rs` calls
  `json_provider::ensure_runtime_profile` for both direct and typed emission.
- `skinny/crates/codegen/src/json_provider.rs` accepts only
  `backend.grammar_name == "json"` and emits JSON templates/names.
- `skinny/crates/runtime/src/lib.rs` exports generated JSON plus proof-gated
  witnesses; it does not export generated CSS.

The lowerer inventory is partially reusable but not sufficient by itself:

- `lower::sink_only` records BIR rule names, literals, span kinds, direct
  shapes, and dispatch counts. Its data model is closer to grammar-neutral
  metadata than the renderers are.
- `sink_direct.rs` renders a JSON parser using `JsonSink`, JSON objects,
  arrays, strings, numbers, bool/null, and JSON parse errors. It is not a CSS
  direct renderer.
- `typed_direct.rs` renders a JSON object/field parser from `DirectSchemaSet`.
  It hardcodes JSON string, number, bool/null, object, array, and skip-value
  syntax. It is not a CSS typed renderer.
- `direct_schema.rs` is a typed host schema surface, but its current consumer is
  the JSON typed-direct renderer.

Skinny pass materialization is also JSON-shaped. It derives object/array/pair,
string/number/bool/null/value roles from grammar structure and classifies all
non-string/non-whitespace regexes as `SpanKind::Number`. CSS declaration values
will not naturally produce the required `DirectBuild`/SinkOnly program for a CSS
direct parser without selected new lowering or a target-specific generated
baseline renderer.

### W1a Gate

REDRESS 111 added only a schema/report lane. The accepted W1a fixture already
models `css_l4/declaration_values/direct/main`, but it is explicitly
schema-only:

- schema `sk-v11-w1a-nonjson-v1`;
- `wave_id = SK-V11-W1a`;
- `outcome_id = S`, `verdict = NO-GO`;
- `measured_validation_path = schema-only`;
- `same_wave_consumer_class = non_json_gate_schema_only`;
- `internal_oracle` W1a sentinel.

W1b cannot reuse that fixture as baseline authority. It needs a sibling W1b
baseline schema/validator and a real generated Track 1 benchmark.

## Direct Versus Typed

| Choice | Viability | Main blocker | W1b recommendation |
|---|---|---|---|
| `css_l4/declaration_values/direct/main` | Best current W1b fit. | Still needs a generated non-JSON skinny Track 1 and a same-plane oracle/report. | Select for W1b. Keep the output as stable direct fact bytes; digest-only equality is not W1b authority. |
| `css_l4/declaration_values/typed/main` | Best semantic oracle quality, poor W1b fit. | No skinny generated CSS typed parser/runtime exists; existing CSS typed stack is outside owner/preblocked proof; `typed_direct.rs` is JSON syntax. | Defer to W2 or require CHALLENGE to approve a much larger generated typed slice. |

Direct should not be interpreted as "parse-only". The generated Track 1 must
emit a strict direct output plane: stable declaration-value fact bytes. If only acceptance,
rule counts, or source offsets are compared, the W1b gate should reject.

Typed should not be interpreted as "use `CssL4Parser::parse` from the root
crate". That would be a generated parser plus old hand runtime from another
stack, not a W1b-generated skinny baseline.

## Exact Owner Files Future W1b Redress Would Need

For the recommended direct row, the minimal redress slice should name these
files up front:

- `grammar/css/l4/values.bbnf`
- `grammar/css/l4/properties.bbnf`
- `grammar/css/l4/stylesheet.bbnf`
- optional selected wrapper input under `grammar/css/l4/`, only if CHALLENGE
  chooses a dedicated declaration-values entry
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/mod.rs`
- one new or changed file under `skinny/crates/codegen/src/lower/` for the
  selected non-JSON direct metadata/lowering path, if the baseline uses BIR
  lowering rather than a generated target harness
- `skinny/crates/runtime/src/lib.rs`
- generated output under
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/bbnf-bench/benches/nonjson_baseline.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v11/research/w1b/reports/nonjson-baseline-css-l4-direct.json`

If the oracle cannot live inside a reviewable module under the existing
`skinny/crates/bbnf-bench/benches/` owner path without becoming opaque,
CHALLENGE should reject the positive route. The revised W1b plan names:

- `skinny/crates/bbnf-bench/benches/nonjson_oracles/css_l4_decl_value.rs`

If the plan chooses typed despite this R1 viability finding, it must also name
the typed-specific owner files and explain the budget expansion:

- `skinny/crates/codegen/src/direct_schema.rs`
- a new generated typed CSS runtime/output location under
  `skinny/crates/runtime/src/grammars/`
- a typed fact serializer/consumer in the bench surface
- any dependency manifest edit needed for an external oracle. In particular,
  `skinny/crates/bbnf-bench/Cargo.toml` currently does not depend on
  `lightningcss` or `cssparser`, and SPEC Section 5 does not list that manifest
  as an owner path. That must be resolved by CHALLENGE before implementation,
  not hidden inside redress.

Do not edit these for the recommended W1b direct baseline unless CHALLENGE
revises the plan:

- `crates/core/src/grammar/generated/css_l4.rs`
- `crates/core/src/runtime/css_l4/`
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/RESULTS.md`

## Redress Gate Shape

The direct baseline should pass only when all of these are true:

1. One row exists: `css_l4/declaration_values/direct/main`.
2. Generated Track 1 is produced from named CSS grammar input and checked in or
   regenerated as generated output, not hand-patched.
3. Track 1 does not call `json_provider`, generated JSON runtime, generated
   JSON SinkOnly helpers, generated typed helpers, or root-crate CSS runtime.
4. Independent Track 2/oracle is same-output-plane and does not call generated
   Track 1.
5. Strict equality passes on the selected declaration-values corpus.
6. Criterion throughput is rendered with run id, host, flags, sample count,
   output plane, source artifacts, and oracle status.
7. A W1b sibling non-JSON gate consumes the report. W1a's schema-only validator
   remains unchanged.
8. No behavior intervention admits, no JSON row moves, and
   `skinny/RESULTS.md` remains byte-stable.

## Rejection Boundary

Return REVISE before redress if the plan requires any of these:

- using the root `CssL4Parser`/`CssStructBuilder` stack as W1b generated Track 1;
- treating W1a fixture sentinels as W1b baseline authority;
- claiming typed baseline from `typed_direct.rs` without replacing its JSON
  parser semantics for the selected CSS target;
- adding a `lightningcss`/`cssparser` dependency without naming
  `skinny/crates/bbnf-bench/Cargo.toml` as an owner path;
- comparing parser acceptance, counts, offsets, or pretty-printed CSS instead of
  a strict direct/typed output plane;
- landing more than one non-JSON row or a W2-style intervention.

Self-verdict: CSS L4 declaration values are viable as W1b's selected grammar
only through a new skinny generated direct baseline. The existing CSS typed
runtime is useful context but not admissible W1b Track 1 proof. Typed should be
deferred unless CHALLENGE explicitly accepts the larger generated typed owner
surface.
