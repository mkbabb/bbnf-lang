# SK-V7 W7 R2 - Passes Phase B Neutrality

Date: 2026-05-16.

Status: read-only diagnosis. No source files were edited.

## Inputs Read

- `restart/skinny/tranches/sk-v7/SPEC.md:306-323`: W7 covers Lock 14
  Phase A+B; Phase B specifically requires a grammar-neutral
  `passes/src/lib.rs` recognizer refactor, `cargo test --workspace`, and a
  `passes::compile()` path that consumes the `Grammar` parameter without
  literal-name matching.
- `restart/skinny/tranches/sk-v7/HANDOFF.md:66-93`: pre-blocked routes include
  SK-V5 UTF-8 fusion, SK-V6 retained/direct materialization routes, object
  next-key carry, EventCursor/sidecar prepasses, function-pointer dispatch,
  capacity prescan, generic SWAR whitespace, separator elision, pair-token
  fusion, and PSI/DTA Rust-codegen automata.
- `skinny/crates/passes/src/lib.rs:26-45`: current `compile()` owns the full
  pass pipeline and directly calls the JSON-specific shape and recognizer
  producers before extracting `BackendIr`.
- `skinny/crates/passes/src/lib.rs:211-251`: current shape and recognizer facts
  are hard-coded by `shapes_for_json()` and `nominate_json()`.
- `skinny/crates/passes/src/lib.rs:652-683`: `extract::single_plan()` still
  selects a rule literally named `json`, then stores recognizers and shape
  facts in `BackendIr`.
- `skinny/crates/passes/src/lib.rs:719-833`: the current literal-name
  materialization hook is named `materialization_descriptor`, not
  `materialization_for_rule`; `rg` found no `materialization_for_rule` symbol.
- `skinny/crates/passes/src/lib.rs:572-579`: hot-path derivation also carries a
  JSON literal by preferring `json` before `parse_value` and `rules.first()`.
- `skinny/crates/ir/src/lib.rs:388-417`: `Recognizer::SimdScan` and
  `StructuralAlphabet` are already generic IR carriers, but
  `StructuralAlphabet::json()` is a JSON-specific constructor.
- `skinny/crates/grammar/src/lib.rs:80-98`: skinny grammar directives are
  deliberately restricted to `@import` and `@token`, so this route must not add
  directive-based metadata.
- `skinny/grammars/json.bbnf:11-18`: the JSON grammar has enough structural
  information to derive entry, value, pair, array, and object roles without
  spelling those roles in the pass implementation.

## Inventory

### `compile()`

`compile()` normalizes, infers types, runs layout, then calls
`shapes::shapes_for_json()` and `recognizers::nominate_json(&normalized)`
before `extract::single_plan()` (`passes/src/lib.rs:26-32`). Backend shape
selection already consumes the normalized grammar, backend IR, layout facts,
and target features without JSON-specific rule names at the call site
(`passes/src/lib.rs:33-39`).

Ownership problem: `compile()` is the choke point where generic passes become
JSON-bound. Phase B should leave `compile(grammar)` as the public entry point
but replace the hard-coded fact producers with grammar-derived helpers:

```text
let entry = derive_entry_rule(&normalized)?;
let materialization = derive_materialization(&normalized, &layout_facts, entry)?;
let shape_facts = shapes::derive_shape_facts(&normalized, &materialization, entry);
let recognizers = recognizers::derive_recognizers(&normalized);
let backend_ir = extract::single_plan(&normalized, &layout_facts, entry, shape_facts, recognizers, &materialization)?;
```

The exact names can differ, but ownership should be explicit: grammar analysis
derives facts, and extraction consumes facts. Extraction should not rediscover
JSON by string matching.

### `shapes_for_json()`

`shapes_for_json()` constructs nine curated JSON shapes:
`JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`,
`JsonNumber`, `JsonBool`, and `JsonNull` (`passes/src/lib.rs:211-239`). The
shape names and field types are JSON products, not generic pass facts.

Refactor target: derive shape names from grammar name plus rule role. For the
checked-in JSON grammar, `grammar.name == "json"` can yield the neutral prefix
`Json`, while rule names produce `JsonObject`, `JsonArray`, `JsonPair`,
`JsonString`, and `JsonNumber` without a hard-coded JSON table. The value enum
can be derived from the `value = ws (object | array | string | number | bool |
null) ws` pattern (`skinny/grammars/json.bbnf:11`), with literal-only boolean
and null branches represented as primitive/unit variants rather than requiring
JSON-specific pass code.

Risk: generating the exact current `JsonValue` enum shape is the hardest part
of the Phase B cleanup because the current shape table encodes product naming,
field naming, and primitive branch lowering in one hand-written place.

### `nominate_json()`

`nominate_json()` ignores the grammar parameter and always emits a pre-entry
exact SIMD scan over `StructuralAlphabet::json()` (`passes/src/lib.rs:245-251`).
The IR type is neutral (`Recognizer::SimdScan` carries `SimdMode`,
`StructuralAlphabet`, and `SimdSite` at `ir/src/lib.rs:388-394`), but the
alphabet constructor hard-codes JSON bytes `{ } [ ] , : "` at
`ir/src/lib.rs:411-416`.

Refactor target: introduce `recognizers::derive_recognizers(grammar)`. It can
derive a structural alphabet by traversing grammar expressions:

- include single-byte punctuation literals, which gives `{`, `}`, `[`, `]`,
  `,`, and `:` for the JSON grammar;
- include the quote byte when a regex pattern begins with a quote, matching the
  current string recognizer need (`passes/src/lib.rs:498-500` already has a
  related first-byte rule);
- omit keyword literals such as `true`, `false`, and `null`;
- return no SIMD recognizer when the derived alphabet is empty.

This keeps the same `Recognizer::SimdScan` substrate and same-wave consumer
while deleting the JSON constructor call from `passes`.

### Literal-name materialization

`materialize_rule()` wraps selected rules with span marks, tape emits,
direct-build fields, and return (`passes/src/lib.rs:719-741`). Selection is
driven by `materialization_descriptor(name)`, whose `match name` arms are
literal JSON rule names: `object`, `array`, `pair`, `string`, `number`, `bool`,
and `null` (`passes/src/lib.rs:750-831`). The fields also spell JSON shape
names and child rule names, for example `JsonObject` with repeated `pair`
members (`passes/src/lib.rs:752-762`), `JsonPair` with `key` and `value`
children (`passes/src/lib.rs:774-792`), and `JsonString`/`JsonNumber` span
fields (`passes/src/lib.rs:794-815`).

Refactor target: derive a `HashMap<RuleId, MaterializationDescriptor>` once,
then make `materialize_rule(rule_id, body, descriptors)` lookup by `RuleId`.
The descriptor derivation should use grammar structure rather than rule names:

- braced delimiter rule with a repeated pair-like child becomes
  `TapeKind::Container`;
- bracketed delimiter rule with repeated value-like child becomes
  `TapeKind::Sequence`;
- a two-child separator rule around `:` becomes `TapeKind::KeyValuePair`;
- a quoted-string regex becomes `TapeKind::StringValue` with a span field;
- the JSON number regex shape becomes `TapeKind::NumberValue`;
- literal-only `true|false` and `null` rules become `BoolValue` and
  `NullValue`.

If a grammar does not match those structural patterns, no descriptor should be
emitted for that rule. That fallback preserves generic behavior without adding
new directives or a new BIR variant.

### Entry and hot-path ownership

`extract::single_plan()` currently requires `grammar.rule_by_name("json")` and
returns `PassError::MissingEntry("json")` if absent (`passes/src/lib.rs:652-660`).
It then lowers every rule, materializes by literal rule name, wraps only the
literal JSON entry rule in `BackendExpr::Entry`, and stores the unchanged
recognizers and shape facts in `BackendIr` (`passes/src/lib.rs:662-682`).

`hot_path::derive_hot_path()` has a second JSON-biased entry heuristic:
`rule_by_name("json")`, then `rule_by_name("parse_value")`, then the first
rule (`passes/src/lib.rs:572-579`).

Refactor target: derive a neutral entry rule before extraction. The least
disruptive rule is:

1. prefer a rule whose name equals `grammar.name`;
2. otherwise prefer the last rule, which matches the checked-in JSON grammar's
   conventional entry location (`skinny/grammars/json.bbnf:18`);
3. otherwise error with the grammar name, not the string `json`.

Pass the resulting `RuleId` into both extraction and hot-path derivation. This
keeps entry ownership in `compile()` and removes the remaining literal JSON
entry lookup from generic passes.

## Recommended Phase B Route

1. Add small internal fact types in `passes/src/lib.rs` only:
   `EntryRule`, `MaterializationPlan`, and possibly
   `DerivedRuleRole`. Keep them private unless W8 needs a public shape.
2. Replace `shapes_for_json()` with `shapes::derive_shape_facts(grammar,
   materialization, entry)`. JSON-specific names should fall out of neutral
   naming rules: PascalCase grammar prefix plus PascalCase rule role.
3. Replace `nominate_json()` with `recognizers::derive_recognizers(grammar)`.
   Construct `StructuralAlphabet { bytes }` directly; do not call
   `StructuralAlphabet::json()` from `passes`.
4. Replace `materialization_descriptor(name)` with descriptor derivation keyed
   by `RuleId`. `materialize_rule()` should receive `RuleId` and use the
   descriptor map, not a `&str` literal-name match.
5. Change `extract::single_plan()` to accept the derived entry `RuleId` and
   materialization map. It should not call `rule_by_name("json")`.
6. Change `hot_path::derive_hot_path()` to accept an optional derived entry
   `RuleId` instead of probing `json`/`parse_value`.
7. Update existing tests rather than deleting coverage. The JSON fixture can
   remain a fixture, but assertions should prove that the neutral derivation
   still produces the current JSON backend facts.

This route keeps the existing BIR vocabulary, keeps recognizers as consumers of
existing grammar facts, and avoids moving JSON-specific logic into another
generic crate.

## Risks

- Shape parity risk: the current curated JSON table encodes naming and field
  layout that downstream code may expect exactly. The Phase B implementation
  must assert current JSON shape facts before and after.
- Over-inference risk: non-JSON grammars with braces, colons, or quoted regexes
  could be misclassified. Mitigate by requiring complete structural patterns;
  partial matches should produce no descriptor.
- Entry-rule risk: switching from literal `json` to a neutral entry heuristic
  can change behavior for grammars whose intended entry is not named after the
  grammar and is not last. Existing skinny grammar has no entry directive, and
  directives must not be expanded in this wave (`grammar/src/lib.rs:80-98`), so
  the heuristic must be documented and tested.
- Recognizer risk: a derived punctuation alphabet may overcollect bytes for
  grammars where punctuation is lexical rather than structural. Because the
  recognizer is a pre-entry scan hint, not semantic lowering, the safe fallback
  is to emit no recognizer unless a minimum structural pattern is present.
- Lock 14 risk: moving `Json*` spellings from production code into tests may be
  acceptable as fixture coverage, but W7's audit gate may count test code. If
  the grep gate is whole-crate, tests need neutral names or expected values
  generated from the JSON fixture rather than inline `Json*` strings.

## Test Commands

Minimum Phase B verification:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p passes
cargo test --workspace
```

Lock 14/local audit checks:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
rg -n 'shapes_for_json|nominate_json|materialization_descriptor|rule_by_name\("json"\)|MissingEntry\("json"|StructuralAlphabet::json\(' skinny/crates/passes/src/lib.rs
rg -n 'Json[A-Z]|"object"|"array"|"pair"|"string"|"number"|"bool"|"null"' skinny/crates/passes/src/lib.rs
```

If W7 Phase A and B are integrated in the same branch, finish with the wave
exit gate:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test --workspace
```

## Bottom Line

Phase B should not add annotations, directives, BIR variants, sidecar
substrates, or JSON configuration objects. The neutral route is to move from
literal-name producers to grammar-derived facts, then pass those facts into
extraction and backend-shape selection. The current JSON output can remain the
golden behavior, but it should be produced by grammar structure rather than by
hard-coded JSON names in `passes`.
