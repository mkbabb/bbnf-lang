# SK-V7 W7 Phase 1 Research: parse-that-regex API

## Scope Read

- W7 owner scope is `parse-that-regex/src/lib.rs` for the
  `JsonStringMatch` rename and 9 HIGH leaks, plus `passes/src/lib.rs`
  for Phase B (`restart/skinny/tranches/sk-v7/SPEC.md:306-314`).
- W7 exits only when parse-that-regex has no JSON-prefixed public types,
  passes consumes grammar parameters without literal-name matching, and
  Lock 14 HIGH count drops by at least 20 (`restart/skinny/tranches/sk-v7/SPEC.md:317-320`).
- The SK-V7 synthesis says Lock 14 currently has about 46 HIGH leaks,
  with parse-that-regex owning 9 of them and B3 sequencing Phase A before
  Phase B (`restart/skinny/tranches/sk-v7/SYNTHESIS.md:149-167`).
- Pre-blocked routes remain binding: UTF-8 fusion routes, retained/direct
  side-state routes, object next-key carry, global tiny-string cap,
  byte-output unescape, DirectBuild semantic string facts, NEON
  tiny-string wiring, pair-token fusion, function-pointer dispatch,
  capacity prescan, generic SWAR whitespace skipper, separator elision,
  raw f64 shortcut, PSI/DTA automaton, and EventCursor prepass
  (`restart/skinny/tranches/sk-v7/HANDOFF.md:66-93`).

## Public API Inventory

### Current neutral substrate already present

| API | Evidence | Notes |
|---|---:|---|
| `StringMode` | `skinny/crates/parse-that-regex/src/lib.rs:43-48` | Public, but variants still include `StrictJson` and `StrictJsonTrustedUtf8`; the type is the right policy carrier but the variant names still encode JSON. |
| `StringFlags` | `skinny/crates/parse-that-regex/src/lib.rs:57-93` | Already neutral; exposes `HAS_ESC`, `HAS_CONTROL`, `HAS_NON_ASCII`, `NEEDS_DECODE`, and `UTF8_VALIDATED`. |
| `StringMatch` | `skinny/crates/parse-that-regex/src/lib.rs:95-117` | Already neutral and has `content_start()`, `content_end()`, and `needs_decode()` accessors. This is the target collapse for `JsonStringMatch`. |
| `match_string` / `match_string_at_quote` | `skinny/crates/parse-that-regex/src/lib.rs:343-413` | Already neutral entry points, but still hard-code JSON quote/backslash/control scanning through `skip_json_string_plain`. |
| `number::NumberSpan` | `skinny/crates/parse-that-regex/src/number/mod.rs:4-14` | Already neutral and richer than `JsonNumberMatch`; direct paths already consume it. |
| `number::match_number_span` / `match_number_span_from_first` | `skinny/crates/parse-that-regex/src/number/mod.rs:31-103` | Already neutral number scanner with rich facts. |

### Public JSON-prefixed string/number surface

| Current API | Evidence | Consumer impact |
|---|---:|---|
| `JsonStringMatch` | `skinny/crates/parse-that-regex/src/lib.rs:33-40` | Redundant with `StringMatch`; consumers use field access to `content_start`, `content_end`, and `needs_unescape`. |
| `JsonNumberMatch` | `skinny/crates/parse-that-regex/src/lib.rs:119-124` | Redundant with `number::NumberSpan` for `start`, `end`, `is_integer`; direct code already uses `NumberSpan`. |
| `match_json_number` | `skinny/crates/parse-that-regex/src/lib.rs:163-171` | Public JSON-named wrapper around the legacy number scanner. |
| `match_json_number_from_first` | `skinny/crates/parse-that-regex/src/lib.rs:173-235` | Public JSON-named scanner returning `JsonNumberMatch`. |
| `validate_json_number` | `skinny/crates/parse-that-regex/src/lib.rs:259-265` | Public JSON-named validator; self-tests use it at `skinny/crates/parse-that-regex/src/lib.rs:1118-1126`. |
| `match_json_string` | `skinny/crates/parse-that-regex/src/lib.rs:267-277` | Public JSON-named wrapper around quoted string matching; retained views use it to rematch string body ranges. |
| `match_json_string_at_quote` | `skinny/crates/parse-that-regex/src/lib.rs:279-291` | Public JSON-named wrapper that currently maps neutral `StringMatch` back into `JsonStringMatch`. |
| `match_json_string_at_quote_trusted_utf8` | `skinny/crates/parse-that-regex/src/lib.rs:293-341` | Hot public JSON-named trusted path for generated `&str` parsers; must preserve the existing trusted UTF-8 fast scanner shape. |
| `validate_json_string` | `skinny/crates/parse-that-regex/src/lib.rs:846-852` | Public JSON-named validator; self-tests currently exercise the underlying JSON matcher. |
| `classify_json_string_content` | `skinny/crates/parse-that-regex/src/lib.rs:766-779` | Public JSON-named string-content classifier used internally by `unescape_json_string` and tests. |
| `decode_json_unicode_escape` | `skinny/crates/parse-that-regex/src/lib.rs:434-476` | Public JSON-named Unicode escape decoder used by `unescape_json_string` and tests. |
| `unescape_json_string` | `skinny/crates/parse-that-regex/src/lib.rs:854-946` | Public JSON-named string materializer with many runtime/codegen consumers. |

`skip_json_whitespace` is public and JSON-prefixed at
`skinny/crates/parse-that-regex/src/lib.rs:126-139`, but it is not a
string/number primitive. It is nevertheless entangled with the same Lock 14
audit because generated retained, direct, and Track 2 code import it.

## Current Skinny Consumers

| Consumer | API names | Evidence |
|---|---|---:|
| parse-that-regex self-tests | `validate_json_number`, `match_json_string`, `match_string`, `classify_json_string_content`, `unescape_json_string`, `decode_json_unicode_escape` | `skinny/crates/parse-that-regex/src/lib.rs:1118-1352` |
| runtime generated retained parser | `JsonStringMatch`, `JsonNumberMatch`, `match_json_string_at_quote_trusted_utf8`, `match_json_number_from_first`, `skip_json_whitespace` | imports at `skinny/crates/runtime/src/grammars/json/generated.rs:5-8`; string wrapper at `:186-197`; number wrapper at `:202-214`; whitespace at `:239-240`, `:245-252`, `:291-329`, `:347-369` |
| runtime generated direct parser | `match_json_string_at_quote_trusted_utf8`, `skip_json_whitespace`, `unescape_json_string` through `JsonSink` | direct whitespace at `skinny/crates/runtime/src/grammars/json/generated.rs:408-432`, object/array loops at `:548-604`, trusted string at `:609-635`; sink unescape at `skinny/crates/runtime/src/grammars/json/sink.rs:16-35`, `:44-92` |
| codegen generated retained template | `JsonStringMatch`, `JsonNumberMatch`, `match_json_string_at_quote_trusted_utf8`, `match_json_number_from_first`, `skip_json_whitespace` | imports and wrappers at `skinny/crates/codegen/src/json_templates/generated.rs:5-8`, `:186-214`; whitespace at `:239-240`, `:245-252`, `:291-329`, `:347-369` |
| codegen direct sink template | `match_json_string_at_quote_trusted_utf8`, `skip_json_whitespace` | rendered whitespace at `skinny/crates/codegen/src/json_sink_direct.rs:145-185`, `:300-354`; trusted string at `:364-390` |
| Track 2 hand parser | `match_json_string_at_quote_trusted_utf8`, `match_json_number_from_first`, `skip_json_whitespace` | imports at `skinny/crates/bbnf-bench/src/track2/json.rs:1-4`; key string at `:96-134`; value string at `:157-182`; number at `:185-191`; whitespace at `:207-220` |
| retained runtime view | `match_json_string`, `unescape_json_string` | imports at `skinny/crates/runtime/src/grammars/json/view.rs:4`; escaped string materialization at `:206-217`; body rematch at `:382-386` |
| retained view template | `match_json_string`, `unescape_json_string` | imports at `skinny/crates/codegen/src/json_templates/view.rs:4`; escaped string materialization at `:206-217`; body rematch at `:382-386` |
| typed DirectBuild template | `match_json_string_at_quote_trusted_utf8`, `skip_json_whitespace`, `unescape_json_string`; already uses neutral number module | imports at `skinny/crates/codegen/src/json_typed_direct.rs:25-29`; trusted strings at `:470-492`, `:618-628` |
| generated real typed bench module | `match_json_string_at_quote_trusted_utf8`, `skip_json_whitespace`, `unescape_json_string`; already uses neutral number module | imports at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:10-14`; trusted strings at `:940-959`, `:1086-1095` |
| direct struct bench | `match_json_string_at_quote_trusted_utf8`, `skip_json_whitespace`, `unescape_json_string`; already imports neutral `NumberSpan` | imports at `skinny/crates/bbnf-bench/src/direct_struct.rs:7-13`; trusted string at `:541-562` |
| runtime JSON sink | `unescape_json_string`, `RegexError` | import at `skinny/crates/runtime/src/grammars/json/sink.rs:1`; source hooks at `:16-35`, `:44-92` |

No non-test skinny consumer currently calls `validate_json_number`,
`validate_json_string`, `classify_json_string_content`, or
`decode_json_unicode_escape` directly. Their current consumers are
parse-that-regex internals and tests (`skinny/crates/parse-that-regex/src/lib.rs:846-946`,
`:1118-1352`).

## Safe Rename / Collapse Route

1. Collapse number first because the direct code has already proven the neutral
   surface: replace `JsonNumberMatch` with `number::NumberSpan`, replace
   `match_json_number_from_first` with `number::match_number_span_from_first`,
   and replace `match_json_number` / `validate_json_number` with neutral
   `match_number_span` / `validate_number` wrappers. The retained generated
   parser and Track 2 only read `start` and `end` today
   (`skinny/crates/runtime/src/grammars/json/generated.rs:202-207`,
   `skinny/crates/bbnf-bench/src/track2/json.rs:185-191`).

2. Collapse `JsonStringMatch` into `StringMatch` rather than introducing a new
   compatibility struct. Existing generated consumers convert as follows:
   `span.content_start` -> `span.content_start()`,
   `span.content_end` -> `span.content_end()`, and
   `span.needs_unescape` -> `span.needs_decode()`. The target methods already
   exist (`skinny/crates/parse-that-regex/src/lib.rs:102-116`).

3. Introduce a small `SpecialByteSet` parameter beside `StringMode`, not a new
   substrate. The minimum shape is a copyable value carrying terminator,
   escape, and control-limit bytes for quoted strings. Existing JSON string
   semantics become a call-site-supplied value equivalent to quote,
   backslash, and `0x20`, instead of hard-coded names in the public API.
   This routes through the existing scan sites now hard-coding `b'"'`, `b'\\'`,
   and `0x20` (`skinny/crates/parse-that-regex/src/lib.rs:593-675`,
   `:678-705`, `:708-719`, `:766-829`, `:948-976`).

4. Rename `StringMode::StrictJson` to a grammar-neutral validating mode and
   `StringMode::StrictJsonTrustedUtf8` to a grammar-neutral trusted mode.
   Keep `GrammarString` and `ByteString` unless Phase A decides to reduce the
   enum further. The policy branch is centralized in `StringMode::validates_utf8`
   (`skinny/crates/parse-that-regex/src/lib.rs:42-55`).

5. Preserve the trusted UTF-8 fast path as a neutral API, not by routing hot
   generated parsers through the generic UTF-8 validating scanner. The current
   trusted function skips UTF-8 validation and uses `skip_json_string_plain_trusted`
   (`skinny/crates/parse-that-regex/src/lib.rs:298-341`, `:678-705`).
   A safe public replacement is `match_string_at_quote_trusted_utf8(input,
   offset, specials) -> Result<StringMatch, RegexError>`.

6. Rename the string materialization functions only after the matcher collapse
   compiles: `classify_json_string_content` -> `classify_string_content` with
   `SpecialByteSet`; `decode_json_unicode_escape` -> a neutral Unicode escape
   decoder; `unescape_json_string` -> a neutral escaped-content materializer
   with an explicit escape policy if the implementation keeps JSON-compatible
   escapes. The current materializer owns escape semantics and error offsets
   (`skinny/crates/parse-that-regex/src/lib.rs:854-946`), so it should not be
   silently generalized by name alone.

7. Update generated and template consumers in the same change. Runtime
   generated files are checked in, and templates are their source authority:
   runtime retained uses the legacy APIs at
   `skinny/crates/runtime/src/grammars/json/generated.rs:5-8`, while the
   template uses the same imports at
   `skinny/crates/codegen/src/json_templates/generated.rs:5-8`.

8. Do not reopen pre-blocked performance routes during Phase A. This route is
   mechanical API collapse and Lock 14 cleanup, not string SIMD wiring,
   side-state, pair-token fusion, EventCursor, global tiny-string cap, or
   direct materialization redesign (`restart/skinny/tranches/sk-v7/HANDOFF.md:71-93`).

## Risks

- **Trusted-path regression:** replacing
  `match_json_string_at_quote_trusted_utf8` with the generic validating matcher
  may reintroduce UTF-8 work or different flag paths on hot generated parsers.
  Keep a neutral trusted helper with the current trusted scanner shape
  (`skinny/crates/parse-that-regex/src/lib.rs:298-341`, `:678-705`).

- **Field-to-method fallout:** current consumers read `JsonStringMatch` fields
  directly, especially `content_start`, `content_end`, and `needs_unescape`
  (`skinny/crates/codegen/src/json_typed_direct.rs:481-488`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:549-558`). The migration must
  update all consumers atomically.

- **Number-span cost:** `number::NumberSpan` carries rich facts beyond
  `JsonNumberMatch` (`skinny/crates/parse-that-regex/src/number/mod.rs:4-14`).
  Retained parse currently only needs `start` and `end`
  (`skinny/crates/runtime/src/grammars/json/generated.rs:202-207`).
  Measure retained parse rows after the collapse; if it regresses, the plan
  needs a neutral lean span strategy rather than restoring JSON names.

- **Materializer semantics:** `unescape_json_string` is not just a name; it
  defines JSON-compatible escapes, surrogate validation, non-character
  acceptance, and exact error offsets (`skinny/crates/parse-that-regex/src/lib.rs:854-946`,
  `:1292-1352`). A neutral rename should carry an explicit policy or clearly
  preserve the JSON-compatible policy for JSON-generated callers.

- **Whitespace spillover:** `skip_json_whitespace` is outside string/number
  collapse but is public, heavily consumed, and JSON-prefixed
  (`skinny/crates/parse-that-regex/src/lib.rs:126-139`;
  `skinny/crates/runtime/src/grammars/json/generated.rs:239-240`;
  `skinny/crates/bbnf-bench/src/track2/json.rs:207-220`). If the Lock 14 audit
  counts function names rather than only public types, this must be renamed or
  parameterized in W7/W8, not ignored.

## Test / Gate Commands

Recommended after the Phase A implementation:

```sh
cargo fmt --check
cargo test -p parse-that-regex
cargo test -p runtime
cargo test -p codegen
cargo test -p bbnf-bench --lib
cargo run -p xtask --release -- check-json
cargo test --workspace
```

Lock 14 grep checks for the Phase A slice:

```sh
rg -n 'pub (struct|enum|type).*Json|Json(String|Number)Match|match_json_(string|number)|validate_json_(string|number)|classify_json_string|decode_json_unicode|unescape_json_string' skinny/crates/parse-that-regex/src
rg -n 'JsonStringMatch|JsonNumberMatch|match_json_string|match_json_number' skinny/crates
```

No-regression bench smoke for the renamed hot paths:

```sh
cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|citm_catalog|instruments|numbers|unicode_escapes)/(track1_generated|track2_handcoded|sonic_rs_anchor|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct)$'
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

The bench smoke is not a substitute for the W7 redress gate, but it is the
minimum evidence needed to prove the API collapse did not silently reopen a
pre-blocked performance route.
