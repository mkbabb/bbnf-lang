# SK-V12 W1a A1 - Codegen Template Lock-14 Leaks

Date: 2026-05-20.

Authority read: `restart/skinny/tranches/sk-v12/SPEC.md` Section 4,
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`, and
`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`.

Scope: codegen JSON template/provider generic-policy leaks in
`skinny/crates/codegen/src/json_templates/generated.rs` plus adjacent
provider/template emitters. This is research only: no source edits, no
staging, no commits.

## W1a Legal Target

W1a must make CSS L4 emission legal before W1b emits a CSS parser. The owner
work is not to hand-write CSS, widen JSON, or claim a CSS row. It is to move
grammar policy out of generic codegen and into `GrammarConfig` or equivalent
generated metadata for: structural alphabet, FIRST/follow dispatch, layout /
trivia, escape policy, number policy, flag semantics, and sink/view/kind
bindings.

## Seven-Leak Codegen Map

| Leak | Codegen evidence | Minimal W1a owner edit |
|---|---|---|
| 1. Structural alphabet hardcoded to JSON | `skinny/crates/codegen/src/json_templates/generated.rs:10` defines `STRUCTURAL_ALPHABET_JSON = b"{}[],:\""`, `:15` asserts the same literal, and `skinny/crates/codegen/src/json_provider.rs:56-57` copies JSON `scan.rs` wholesale. The copied scanner itself has JSON bytes at `skinny/crates/runtime/src/grammars/json/scan.rs:6-7`. | Extract structural bytes from generated metadata. Existing IR already carries recognizer alphabets at `skinny/crates/ir/src/lib.rs:454-475`; codegen should emit per-grammar `config::STRUCTURAL_BYTES` / `STRUCTURAL_ALPHABET` and scanner capacity hooks. JSON keeps the same bytes, but the generic template/provider stops naming `_JSON` or baking `b"{}[],:\""`. |
| 2. Value dispatch hardcoded to JSON primaries | `generated.rs:47-57` matches `{`, `[`, `"`, `-`, digits, `t`, `f`, `n` and maps to `JsonNodeKind`; `skinny/crates/codegen/src/json_templates/value.rs:12-26` defines JSON-only node kinds; `value.rs:29-46` classifies tape cursors from JSON bytes. Direct sink repeats the same dispatch in `skinny/crates/codegen/src/sink_direct.rs:120-160`, `:165-241`. | Generate a per-grammar node-kind enum and dispatch table from FIRST sets. Existing first-byte logic exists privately in `skinny/crates/passes/src/lib.rs:719-779`; W1a can move/copy a codegen-local extractor over `BackendIr` / `BackendExpr` without adding a BIR variant. Template code should iterate metadata arms rather than matching JSON literals. |
| 3. String quote/escape policy is JSON backslash policy | `generated.rs:95-100` and `:142-153` call JSON quote/string matchers and set `OffsetFlags::HAS_ESC`; `generated.rs:171-180` treats `"` as terminator and `\` / controls as blockers; `generated.rs:189-200` calls `match_string_at_quote_trusted_utf8`. Views decode with JSON rules at `skinny/crates/codegen/src/json_templates/view.rs:205-214` and `:383-386`. Direct sink repeats this at `sink_direct.rs:315-350`. | Add generated `StringPolicy` / `EscapePolicy` metadata: opener, closer, escape style, control policy, fast-path cap, matcher, and decoder. JSON metadata points to the current parse-that JSON matcher. CSS and Sheets later supply their own policies. Generic template code should call policy hooks, not `match_string_at_quote_trusted_utf8` or `unescape_string` directly. |
| 4. Number policy is JSON-only | `generated.rs:52` admits only `-` and digits as number starts; `generated.rs:205-217` delegates to `match_number_span_from_first`. `value.rs:41` repeats JSON number starts, and `view.rs:230-233` materializes through `serde_json::Number`. Direct sink repeats number starts at `sink_direct.rs:143`, `:183`, `:223` and JSON number matching at `sink_direct.rs:356-400`. | Add generated `NumberPolicy` metadata: start-byte set, sign rules, leading-dot, leading-zero, fraction/exponent shape, suffix/unit handling, scalar terminators, and materializer. JSON uses the existing matcher; CSS dimensions and Sheets numbers must not be accepted by loosening the JSON matcher globally. |
| 5. Object/key/value member model assumes quoted JSON keys and `:` | `generated.rs:83-115` hardcodes `parse_pair` as quoted string then colon; `generated.rs:62-78` hardcodes object braces and comma/end handling. `view.rs:105-112` exposes `JsonObject::get`, `view.rs:258-297` assumes key cursor then value cursor and next key is a `JsonNodeKind::String`, and `view.rs:128-133` canonicalizes with JSON string serialization plus `:`. Direct sink hardcodes the same object member rule at `sink_direct.rs:247-280`. | Generate container/member rules from grammar metadata / lowered rules. If W1a keeps JSON object helpers, keep them in JSON-owned generated files, not generic templates. CSS declaration/property and Sheets forms need their own generated rule/view shapes rather than reuse of `parse_key_colon` or `JsonObjectPairs`. |
| 6. Offset flag semantics encode JSON escape meaning | Codegen imports `OffsetFlags` in `generated.rs:4` and sets `OffsetFlags::HAS_ESC` at `generated.rs:99-100` and `:152-153`. Parser exposes generic flag patching at `skinny/crates/codegen/src/json_templates/parser.rs:5` and `:41-42`; view interprets `HAS_ESC` as JSON unescape at `view.rs:205-214`. Runtime bit names are fixed at `skinny/crates/runtime/src/tape/mod.rs:20-23`. | Keep tape flags as opaque bits and move interpretation into generated metadata: `config::flags::STRING_NEEDS_DECODE` plus `config::decode_string`. Do not globally redefine `HAS_ESC` for non-JSON; W1a should either stop using named JSON bits in generic templates or confine those names to JSON config. |
| 7. Sink/view/kind bindings are JSON types | `skinny/crates/codegen/src/json_provider.rs:27-35` exports `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, and `JsonVisitor`; `json_provider.rs:60-62` copies the JSON sink trait. The appended direct renderer imports `JsonSink` at `sink_direct.rs:82`, declares `parse_direct<'i, S: JsonSink>` at `sink_direct.rs:100`, and emits JSON callback names throughout `sink_direct.rs:124-241`, `:251-307`, and `:410-447`. The source sink trait is JSON-only at `skinny/crates/runtime/src/grammars/json/sink.rs:3-119`. | Generate per-grammar sink/view/kind names and callback surfaces. The direct renderer should take a metadata naming/policy bundle, or the current renderer should be renamed/contained as JSON-only while a grammar-neutral renderer is introduced. CSS must get `Css...` views/sink callbacks; it must not implement `JsonSink`. |

## Minimal Owner Edit Shape

1. Add a codegen metadata extraction step before provider emission. It should
   build a small `GeneratedGrammarMetadata` from `BackendIr` plus the compiled
   grammar facts: module names, generated type names, structural alphabet,
   dispatch FIRST sets, layout/trivia policy, span policies, flag meanings,
   node-kind roster, and sink callback names. Reuse existing IR surfaces:
   `BackendIr.grammar_name` / `recognizers` / `rules`
   (`skinny/crates/ir/src/lib.rs:393-399`), `Recognizer::SimdScan`
   (`:454-460`), `SpanKind` (`:479-483`), and direct decode metadata
   (`:521-573`).

2. Emit a per-grammar `config.rs` or private `GrammarConfig` implementation in
   each generated grammar module. The config owns the literals and policies;
   parser/view/value/sink templates import `super::config`, not JSON constants.
   If a runtime trait is needed, keep it crate-private or sealed so W1a does
   not create a public substrate API.

3. Split JSON-owned templates from generic templates. JSON-specific names
   (`JsonNodeKind`, `JsonSink`, `JsonValue`, `JsonRoot`, `JsonVisitor`,
   `serde_json`, JSON `true`/`false`/`null`) are legal only in JSON generated
   output or JSON-owned metadata/templates. They are not legal in generic
   provider/rendering code that CSS L4 would reuse.

4. Replace the `json_provider::ensure_runtime_profile` choke point
   (`skinny/crates/codegen/src/json_provider.rs:4-12`, called from
   `skinny/crates/codegen/src/lib.rs:108` and `:146`) with a metadata-backed
   provider selection. W1a does not need full CSS emission, but the path must
   fail closed for missing metadata rather than fail because the grammar name
   is not `json`.

5. Preserve JSON by regenerating JSON output and proving parity/guards. JSON
   policy movement should be behavior-neutral: the JSON generated runtime can
   still contain JSON policy, but the generic codegen path must no longer
   require JSON policy to exist.

## No-Go Routes

- Do not add `if grammar_name == "css"` / `"sheets"` branches beside the JSON
  branch. That repeats the Lock-14 leak in another grammar.
- Do not extend JSON match arms to accept CSS or Sheets syntax.
- Do not reuse `JsonSink`, `JsonNodeKind`, `JsonValue`, or JSON views for CSS.
- Do not loosen `match_number_span_from_first` or JSON string matching to cover
  non-JSON grammars globally.
- Do not reinterpret `OffsetFlags::HAS_ESC` as a universal escape flag or add
  more globally named grammar bits.
- Do not add a directive, BIR variant, `BackendShape`, public substrate API, or
  hand-only CSS parser in W1a.
- Do not claim a CSS parser row, SOTA movement, or `parse_only` admission from
  W1a. W1a is a legality gate only.

## Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang` unless noted.

```sh
git status --short
```

```sh
cd skinny && cargo test -p codegen
```

```sh
cd skinny && cargo test -p runtime
```

```sh
cd skinny && cargo xtask check-json
```

```sh
cd skinny && cargo xtask check-conformance
```

```sh
cd skinny && cargo xtask lint-loc
```

```sh
rg -n 'grammar_name == "json"|runtime emission currently supports grammar profile|STRUCTURAL_ALPHABET_JSON|JsonSink|JsonNodeKind|JsonValue|JsonRoot|JsonVisitor|OffsetFlags::HAS_ESC|match_string_at_quote_trusted_utf8|match_number_span_from_first|serde_json' skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/sink_direct.rs skinny/crates/codegen/src/typed_direct.rs skinny/crates/codegen/src/lower skinny/crates/codegen/src/json_provider.rs
```

Expected after W1a: no hits in generic emitters/providers, or hits only in
files explicitly renamed/scoped as JSON-owned and excluded from CSS provider
reuse.

```sh
rg -n 'b"\{\}\[\],:\\""|b"true"|b"false"|b"null"|ExpectedColon|ExpectedCommaOr' skinny/crates/codegen/src
```

Expected after W1a: hits are confined to JSON-owned generated metadata,
fixtures, or tests; generic CSS-reusable templates must be clean.

```sh
cd skinny && cargo xtask gate-json --check-results --advisory
```

If W1a adds a non-JSON Lock-14 report artifact for the gate, also run:

```sh
cd skinny && cargo xtask gate-json --check-results --advisory --w1a-non-json-report ../restart/skinny/tranches/sk-v12/research/w1a/w1a-lock14-report.json
```
