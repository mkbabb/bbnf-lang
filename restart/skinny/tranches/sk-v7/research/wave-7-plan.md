# SK-V7 Wave 7 Plan: Lock 14 Phase A+B API and Pass Neutralization

Inputs:

- `wave-7-r1-parse-that-api.md`: inventories public JSON-prefixed
  parse-that-regex APIs and consumers.
- `wave-7-r2-passes-neutral.md`: maps `passes::compile()` JSON binding and
  the grammar-derived replacement route.
- `wave-7-r3-lock14-leaks.md`: counts the W7 Lock 14 HIGH leak surface and
  rejects public compatibility aliases.
- `wave-7-r4-verification-gate.md`: defines generated-output, correctness,
  and `RESULTS.md` invariants.
- `restart/skinny/tranches/sk-v7/SPEC.md` Section 9.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` Section 3.

## Intervention

Lock 14 Phase A+B API and Pass Neutralization.

This is a refactor wave, not a throughput intervention. The redress commit
will remove JSON-named public primitive types/functions from
`parse-that-regex`, migrate all same-wave consumers to neutral names, and
replace `passes::compile()` JSON-specific fact producers with
grammar-derived entry, recognizer, shape, and materialization facts.

## Owner Paths

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/codegen/src/json_typed_direct.rs`
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/REDRESS.md`

`skinny/RESULTS.md` is an explicit non-owner for W7 unless the wave rejects
and documents an accidental refresh. The intended successful redress leaves it
unchanged.

## Planned Source Shape

### Phase A: parse-that-regex neutral API

1. Delete the public `JsonStringMatch` and `JsonNumberMatch` structs.
2. Route retained and direct consumers to `StringMatch` and
   `number::NumberSpan`.
3. Replace public JSON-named entry points with neutral names that preserve
   current semantics:
   - `skip_json_whitespace` -> `skip_ascii_whitespace`;
   - `match_json_string*` -> `match_string*` forms using neutral
     `StringMode` variants;
   - `match_json_string_at_quote_trusted_utf8` ->
     `match_string_at_quote_trusted_utf8`;
   - `match_json_number*` -> `number::match_number_span*`;
   - `validate_json_number` / `validate_json_string` ->
     `validate_number` / `validate_string`;
   - `classify_json_string_content` -> `classify_string_content`;
   - `decode_json_unicode_escape` -> `decode_unicode_escape`;
   - `unescape_json_string` -> `unescape_string`.
4. Rename `StringMode::StrictJson` and `StrictJsonTrustedUtf8` to neutral
   UTF-8 policy names while preserving behavior. Do not route trusted
   generated parsers through a validating scanner.
5. Do not keep public JSON compatibility aliases in `parse-that-regex`.
   Same-wave consumers migrate instead.

### Phase B: passes grammar-neutral derivation

1. Replace `shapes::shapes_for_json()` with grammar-derived shape facts.
   For the existing JSON grammar, generated shape names must remain the same
   because the JSON runtime and tests are the same-wave consumer. The source
   of those names must be structural derivation from grammar/rule facts rather
   than a public JSON fact producer.
2. Replace `recognizers::nominate_json()` with
   `recognizers::derive_recognizers(grammar)`, deriving the exact structural
   alphabet from grammar punctuation and string-leading regex facts.
3. Derive a neutral entry rule in `compile()` and pass it through layout,
   hot-path derivation, and extraction. `extract::single_plan()` must not call
   `rule_by_name("json")` or emit `MissingEntry("json")`.
4. Replace literal-name materialization lookup with a descriptor map keyed by
   `RuleId`, derived from complete JSON-like structural patterns. Partial
   matches produce no descriptor rather than grammar-specific fallback code.
5. Keep the current BIR and IR vocabulary. Do not add directives, BIR
   variants, or a new substrate.

## Falsifiability Gate

W7 admits only if all of these pass:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p parse-that-regex
cargo test -p passes
cargo test -p codegen
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
cargo run -p xtask --release -- check-conformance
cargo test --workspace
```

Audit checks:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
rg -n 'pub (struct|enum|type|fn).*(Json|json)|StrictJson|StrictJsonTrustedUtf8|JsonStringMatch|JsonNumberMatch' skinny/crates/parse-that-regex/src
rg -n 'shapes_for_json|nominate_json|rule_by_name\("json"\)|MissingEntry\("json"|StructuralAlphabet::json\(\)' skinny/crates/passes/src/lib.rs
git diff --exit-code -- skinny/RESULTS.md
```

The first two grep commands must return zero production API/compile-binding
leaks. JSON fixture names in generated-runtime tests may remain only where they
are not public parse-that API and not generic pass selection logic. The Lock
14 HIGH reduction target is at least 20 relative to the SK-V7 audit baseline.

Generated-output invariant:

- `check-json` and `check-real-typed` must pass.
- Any generated file diff must be mechanical API spelling only. No parser
  control, offset emission, sink-only direct code, structural alphabet, tape
  flags, view logic, error offsets, or file ordering may drift.

## Hard Cap

240 minutes for W7 redress. If the combined Phase A+B redress cannot fit,
close this cycle as a measured rejection or split into W7b with a fresh
triumvirate, per the SK-V7 dispatch contract.

## Revert Protocol

On failure:

1. Save the rejected source/status patch to
   `/tmp/skv7-wave-7-lock14-neutralization-rejected.patch`.
2. Restore source files and `skinny/RESULTS.md`.
3. Add a `skinny/REDRESS.md` entry with the failed grep/test/generation gate,
   exact output, and the next candidate shape.
4. Commit:
   `docs(sk-v7-wave7-redress): reject lock14 phase-a-b neutralization`.

On success:

1. Keep the source changes and any checked-in generated files required by
   neutral API spelling.
2. Leave `skinny/RESULTS.md` unchanged.
3. Add a `skinny/REDRESS.md` admit entry with the audit and correctness
   evidence.
4. Commit:
   `feat(sk-v7-wave7): admit lock14 phase-a-b neutralization`.

## Same-Wave Consumer

The same-wave consumers are:

- generated retained JSON parser and view/sink runtime;
- generated direct JSON code and generated real-typed bench module;
- Track 2 hand JSON parser and direct-struct bench helpers;
- `passes::compile()` compiling the existing JSON grammar into the same
  backend facts consumed by `codegen`.

No new primitive, SIMD kernel, parser sidecar, or orphan substrate is added.

## Pre-Blocked Routes

Per `restart/skinny/tranches/sk-v7/HANDOFF.md` Section 3 and
`skinny/REDRESS.md`, this plan does not reopen:

- REDRESS 28+33 Class A tiny-string wiring;
- REDRESS 50-55 UTF-8 fusion routes;
- REDRESS 60-72 retained/direct materialization routes;
- REDRESS 83 generated-retained StringBlock16 tiny probe;
- REDRESS 84 object-pair value-byte control compaction;
- object next-key carry;
- parser-owned decoded scratch;
- byte-output unescape;
- DirectBuild semantic string facts;
- separator elision;
- pair-token fusion;
- function-pointer dispatch;
- capacity prescan;
- generic SWAR whitespace;
- raw f64 shortcut;
- PSI/DTA Rust-codegen automata;
- EventCursor or sidecar structural prepasses.
