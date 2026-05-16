# SK-V7 W8 R3 - typed_direct/schema_direct/direct_schema Lock 14 cleanup research

Scope: read-only research for the W8 codegen slice covering
`skinny/crates/codegen/src/json_typed_direct.rs`,
`skinny/crates/codegen/src/lower/schema_direct.rs`, and
`skinny/crates/codegen/src/direct_schema.rs`. No source code was edited.

## Required context

- SPEC §10 assigns Wave 8 to Lock 14 Phase C + D. For codegen it names
  `codegen/src/lib.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, and
  `lower/schema_direct.rs`, with the tasks to replace `emit_json_*` with
  `emit_grammar_*`, collapse hardcoded JSON shape rosters into grammar-derived
  facts, and preserve byte-identical generated output via `xtask gen --check`
  (`restart/skinny/tranches/sk-v7/SPEC.md:325-339`).
- A5 §2.5 marked `json_typed_direct.rs` HIGH because it used JSON primitive
  calls and `field.json_key`; after W7, this file already imports
  grammar-neutral `match_string_at_quote_trusted_utf8`,
  `skip_ascii_whitespace`, `unescape_string`, and
  `number::match_number_span_from_first`, so the old `match_json_*`,
  `skip_json_whitespace`, and `unescape_json_string` names are no longer
  current in this slice (`skinny/crates/codegen/src/json_typed_direct.rs:25-29`).
- A5 §2.6 remains current: `schema_direct.rs` still validates the JSON shape
  roster and JSON literal recognizers in the generic lower path
  (`restart/skinny/tranches/sk-v7/research/skv7-A5-lock-audit.md:249-252`).
- B3 Class C calls this the riskiest codegen phase. For this subtask, the
  applicable rows are the `json_typed_direct.rs` `field.json_key` rename,
  `schema_direct.rs` required-shape parameterization, and `direct_schema.rs`
  `json_key -> key_literal` rename. B3 also recommends splitting Class C into
  `3b (json_typed_direct rebrand + schema_direct)` when the broader commit is
  too large (`restart/skinny/tranches/sk-v7/research/skv7-B3-lock14-sequence.md:97-116`,
  `:209-230`, `:319-333`).

## Current JSON/name leaks in this slice

### `json_typed_direct.rs`

| Line(s) | Leak | Current fact after W7 | W8 implication |
|---:|---|---|---|
| file path | `json_typed_direct.rs` module name | File remains JSON-branded even though primitive imports are now grammar-neutral. | Rename module to `typed_direct.rs` with the broader Class C module rename. |
| `267`, `296` | `field.json_key` | Emitted match arms for accepted and ignored fields still read the schema field name `json_key`. | Rename schema field to `key_literal` or `field_key` and update both render call sites. |
| `82`, `94`, `319`, `329`, `346` | emitted object/array punctuation and key/value parse shape | Structs, maps, map-entry vecs, and vec helpers still emit JSON object/array syntax directly. | Keep behavior for JSON, but drive it from a grammar/schema parse profile instead of treating the renderer as grammar-neutral. |
| `288-294` | `DirectSkipKind::{Array,Object,String,Number,Bool,Null}` mapped to JSON skip helpers/literals | Skip policy is coupled to JSON value categories; `Null` emits `consume_literal(b"null")`. | Either keep as a JSON value-profile enum under generated JSON metadata or rename to grammar-neutral token/value classes supplied by DirectFieldFacts. |
| `431-432` | `ws()` uses `skip_ascii_whitespace` | The name is neutral, but the profile is JSON whitespace bytes only. | Parameterize whitespace/trivia recognizer if W8 is claiming generic typed direct output. |
| `470-488`, `617-627`, `632-652` | string parse/skip assumes `"` delimiter, backslash escape, C0 control cutoff | W7 removed JSON public API names, but semantics remain RFC-8259 JSON string semantics. | Acceptable for generated JSON output only; generic renderer needs `StringProfile`/escape policy facts. |
| `497-515`, `570-572` | literal bytes `true`, `false`, `null` and dispatch bytes `t/f/n` | JSON literal recognizers are embedded in generated parser runtime. | Drive from literal recognizer facts; for byte-identical JSON output, the facts must reproduce these exact bytes and error paths. |
| `560-574`, `577-613` | `skip_value`, `skip_object`, `skip_array` JSON recursive skipper | Unknown-field skip is implemented as a JSON value skipper. | Rename/parameterize as generated value-skip profile; do not present it as grammar-neutral shared code. |

Important negative finding: the old A5 examples `match_json_*`,
`skip_json_whitespace`, and `unescape_json_string` are no longer present in
this file. Current greps only find generic parse-that names at
`json_typed_direct.rs:25-29`, `:481`, `:488`, `:554`, and `:625`.

### `lower/schema_direct.rs`

| Line(s) | Leak | W8 implication |
|---:|---|---|
| `16-24` | Required shape allowlist hardcodes `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`. | Replace with shape requirements supplied by the direct schema/facts contract, not a literal roster. |
| `31-35` | Required literal recognizers hardcode `true`, `false`, `null`; error says `JSON literal recognizers`. | Replace with typed-direct literal requirements from grammar facts and template the diagnostic by grammar/profile name. |
| `37` | `direct_shape_roster` blindly clones all `sink_only.direct_shapes`. | Fine as output metadata, but if W8 adds required-shape facts, verify the roster is deterministic and still byte-identical for JSON. |

### `direct_schema.rs`

| Line(s) | Leak | W8 implication |
|---:|---|---|
| `35-46` | Public schema fields are named `json_key` on `DirectFieldSchema` and `DirectIgnoredFieldSchema`. | Rename to `key_literal` as B3 prescribes. This is the smallest safe subtask in this slice. |
| `50-58` | `DirectSkipKind::{Array,Object,String,Number,Bool,Null}` encode JSON value categories. | Decide whether these are JSON-profile value kinds or generic value classes. If kept in generic schema, prefer names tied to grammar facts or recognizer ids. |
| `167`, `175-198` | Validator variable/errors say `json_keys`, `empty JSON key`, and `duplicate JSON key`. | Rename to `key_literals` and neutral diagnostics. |
| `64-67`, `69-74` | `capacity_hint` is already present for `Vec` and `MapEntriesVec`. | W7/W3 capacity work has landed here; do not re-open it in W8 except to preserve generated output. |

## Feasible W8 rename/parameterization

Recommended sub-slice for this R3 area:

1. Rename the schema field surface:
   `DirectFieldSchema::json_key -> key_literal`,
   `DirectIgnoredFieldSchema::json_key -> key_literal`, local
   `json_keys -> key_literals`, and JSON-key diagnostics to neutral key-literal
   diagnostics. Update `json_typed_direct.rs:267` and `:296`.
2. Rename the module/file as part of the broader Class C sequence:
   `json_typed_direct.rs -> typed_direct.rs`. Do this with the matching
   `codegen/src/lib.rs` module rename, not in isolation, or the crate will not
   compile.
3. Replace `schema_direct.rs:16-24` with a required-shapes input derived from
   the DirectFieldFacts/schema contract. Minimal shape:
   `lower_program(sink_only, schema, requirements)` where requirements carries
   `required_shapes` and `required_literals`. If signatures must stay small,
   add the requirements to `DirectSchemaSet` only if that matches existing
   schema ownership; avoid manufacturing JSON defaults inside `lower_program`.
4. Replace `schema_direct.rs:31-35` with `required_literals` iteration. For the
   JSON grammar, those facts must be exactly `true`, `false`, and `null`.
5. Treat `json_typed_direct.rs` parser-runtime JSON bytes as generated
   JSON-profile output, not as shared generic runtime. Full generality needs
   profiles for object delimiters, array delimiters, key/value separator,
   element separator, string delimiter/escape policy, number scanner,
   whitespace/trivia, bool/null literals, and unknown-value skip.

Do not reintroduce adapters named `Json*` in generic codegen to ease the
rename. W7 already moved primitive parse-that usage to neutral names; adding
compatibility aliases would regress the Lock 14 textual surface.

## Generated output invariant

The gating invariant is byte-identical generated JSON output. For this slice,
the implementation may rename Rust codegen internals and parameterize facts,
but for the JSON grammar it must emit the same generated module text:

- Same `// schema_hash` and `// direct_shapes` order.
- Same public root functions and typed structs from the schema.
- Same parser behavior for whitespace, object/array delimiters, `:`/`,` syntax,
  strings, numbers, booleans, null, unknown-field skip, duplicate-field checks,
  and trailing-character errors.
- Same Vec/Map capacity behavior, especially `capacity_hint.unwrap_or(0)` at
  `json_typed_direct.rs:314` and `:341`.

Any W8 parameterization should be proven by comparing generated artifacts before
and after the change, not only by passing unit tests.

## Risks

- Signature drift: `lower_program()` currently takes only `SinkOnlyProgram` and
  `DirectSchemaSet` (`schema_direct.rs:11-14`). Adding fact inputs affects every
  caller and can spill outside this slice if not staged with the broader Class C
  owner.
- Partial rename risk: changing `direct_schema.rs` fields without updating all
  constructors will compile-fail elsewhere. Use compiler errors to find all
  schema construction sites, but stage only intended codegen/schema files in the
  eventual implementation.
- Semantic overclaim: after W7 the names are more neutral, but
  `json_typed_direct.rs` still emits a JSON parser. Renaming the file without
  adding facts closes a name leak, not the hardcoded grammar-shape leak.
- Diagnostic churn can break snapshot/text tests. Neutral diagnostics are
  desirable, but generated output must remain byte-identical unless diagnostics
  are outside generated artifacts.
- `DirectSkipKind` is a borderline item. It may be acceptable as JSON-profile
  schema data, but as a generic schema enum it still looks like a JSON value
  taxonomy. Decide before broadening the grep gate.

## Exact tests and greps

Baseline audit greps for this R3 slice:

```bash
rg -n 'json_key|json_keys|JSON key|Json(Object|Array|Pair|String|Number|Bool|Null)|typed DirectBuild requires JSON|b"true"|b"false"|b"null"|skip_value|skip_object|skip_array|DirectSkipKind::(Array|Object|String|Number|Bool|Null)' \
  skinny/crates/codegen/src/json_typed_direct.rs \
  skinny/crates/codegen/src/lower/schema_direct.rs \
  skinny/crates/codegen/src/direct_schema.rs
```

Post-cleanup expected greps for this R3 slice:

```bash
rg -n 'json_key|json_keys|JSON key|typed DirectBuild requires JSON' \
  skinny/crates/codegen/src/typed_direct.rs \
  skinny/crates/codegen/src/lower/schema_direct.rs \
  skinny/crates/codegen/src/direct_schema.rs
```

Expected result: no hits.

```bash
rg -n 'Json(Object|Array|Pair|String|Number|Bool|Null)' \
  skinny/crates/codegen/src/lower/schema_direct.rs
```

Expected result: no hits in the lower path. JSON shape names may still appear
in generated JSON fixtures/templates until the broader Class C/E relocation is
complete, but not in `lower/schema_direct.rs`.

Broader W8/Class C checks:

```bash
cargo run -p xtask --release -- gen --check
cargo test -p bbnf-codegen
cargo test --workspace
cargo bench --workspace --no-run
cargo clippy --workspace --all-targets
```

Lock 14 spot checks after the full codegen phase:

```bash
rg -n 'json_typed_direct|json_sink_direct|emit_json_|parse_json_grammar' skinny/crates/codegen/src
rg -n 'json_key|JSON key|typed DirectBuild requires JSON' skinny/crates/codegen/src
rg -n 'Json(Object|Array|Pair|String|Number|Bool|Null)' skinny/crates/codegen/src/lower
cargo xtask lint-no-hardcoded-grammars
```

Performance/parity checks if the parser-runtime parameterization changes more
than names:

```bash
cargo run -p bbnf-bench --release -- gate --baseline pre-lock14.json
cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|citm_catalog|instruments)/(track1_generated|track2_handcoded|sonic_rs_anchor|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct)$'
```
