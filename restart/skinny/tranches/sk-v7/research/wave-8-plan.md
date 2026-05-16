# SK-V7 Wave 8 Plan: Lock 14 Phase C+D Codegen Shell Neutralization

Inputs:

- `wave-8-r1-codegen-api.md`: inventories codegen public API, module, and
  template-entry leaks.
- `wave-8-r2-sink-direct.md`: maps `json_sink_direct` validation and
  generated-output invariants.
- `wave-8-r3-typed-schema.md`: maps typed-direct, schema-direct, and
  direct-schema JSON key/shape leaks.
- `wave-8-r4-ir-verifier.md`: identifies the remaining IR residue and the
  available verifier commands.
- `restart/skinny/tranches/sk-v7/SPEC.md` Section 10.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` Section 3.

## Intervention

Lock 14 Phase C+D Codegen Shell Neutralization.

This is a refactor wave, not a throughput intervention. W8 will make the
generic codegen shell and schema surface grammar-neutral while preserving the
checked-in JSON generated output byte-for-byte. The hand-authored emitted JSON
parser bodies remain per-grammar generated output for this wave because the
current `SinkOnlyProgram` does not yet carry route/primitive facts capable of
authoring arbitrary grammar recursive-descent code.

## Owner Paths

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/codegen/src/json_typed_direct.rs`
- `skinny/crates/codegen/src/lower/schema_direct.rs`
- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/ir/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf/src/lib.rs`
- `skinny/REDRESS.md`

`skinny/RESULTS.md` is a non-owner. W8 must not refresh benchmark authority
or claim throughput movement.

## Planned Source Shape

1. Rename the private codegen renderer modules:
   - `json_sink_direct.rs` -> `sink_direct.rs`;
   - `json_typed_direct.rs` -> `typed_direct.rs`;
   - update `codegen/src/lib.rs` call sites.
2. Rename the codegen public and internal API:
   - `emit_json_from_source(source)` -> `emit_from_source(grammar_name, source)`;
   - `emit_json(backend)` -> `emit(backend)`;
   - `emit_json_typed_from_source(source, schema)` ->
     `emit_typed_from_source(grammar_name, source, schema)`;
   - `emit_json_with_layout` / `emit_json_typed_with_layout` ->
     `emit_with_layout` / `emit_typed_with_layout`.
   Non-JSON grammar names may fail explicitly until later waves add
   generalized runtime templates. They must not silently compile as JSON.
3. Migrate same-wave callers in `bbnf` and `xtask` to the neutral APIs.
   Do not leave production `emit_json_*` wrappers after caller migration.
4. Replace `direct_schema` key naming:
   - `json_key` -> `key_literal`;
   - `json_keys` locals and diagnostics -> `key_literals` / key-literal
     wording;
   - update real-typed schema constructors and typed-direct renderer arms.
5. Replace `lower/schema_direct.rs` hardcoded JSON shape and literal validation
   with requirements derived from `SinkOnlyProgram` facts. For this wave the
   JSON generated output must remain unchanged, but the generic lower path must
   not carry `JsonObject`/`JsonArray`/`JsonPair`/`JsonString`/`JsonNumber`/
   `JsonBool`/`JsonNull` allowlists or a `JSON literal recognizers` diagnostic.
6. Neutralize `sink_direct` validation text and roster checks. Use observed
   `SinkOnlyProgram` facts and DirectBuild field rosters rather than generic
   codegen constants named `JSON_RULES` / `JSON_SHAPES`. Keep emitted JSON
   parser bodies byte-identical.
7. Remove IR residue:
   - delete `StructuralAlphabet::json()`;
   - replace JSON-whitespace string equality in `regex_is_nullable` with a
     generic regex-nullability helper for the current regex subset. Add focused
     IR tests for nullable whitespace-like regexes and non-nullable string and
     number regexes.

## Falsifiability Gate

W8 admits only if all of these pass:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p codegen
cargo test -p ir
cargo test -p bbnf
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-real-typed
cargo run -p xtask --release -- check-conformance
cargo test --workspace
```

Generated-output invariant:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
git diff --exit-code -- skinny/crates/runtime/src/grammars/json/generated.rs
git diff --exit-code -- skinny/crates/bbnf-bench/src/generated_real_typed.rs
git diff --exit-code -- skinny/RESULTS.md
```

The SPEC names `cargo run -p xtask --release -- gen --check`, but the current
skinny xtask does not expose that command. For W8, `check-json` and
`check-real-typed` are the runnable byte-identical generation gates. Root
workspace `cargo xtask regen --check` may also be run as an ancillary guard
when staged files touch root grammar-relevant paths.

Audit checks:

```bash
cd /Users/mkbabb/Programming/bbnf-lang
rg -n 'json_sink_direct|json_typed_direct|emit_json_|emit_json\(|parse_json_grammar|json_key|json_keys|JSON key|typed DirectBuild requires JSON|StructuralAlphabet::json\(\)|pub fn json\(\)' skinny/crates/codegen/src skinny/crates/ir/src/lib.rs skinny/xtask/src skinny/crates/bbnf/src/lib.rs
rg -n 'Json(Object|Array|Pair|String|Number|Bool|Null)' skinny/crates/codegen/src/lower/schema_direct.rs
rg -n 'const REQUIRED_RULES|const REQUIRED_SHAPES|JSON SinkOnly renderer' skinny/crates/codegen/src/sink_direct.rs
rg -n 'pattern == r"\[ \\t\\n\\r\]\*"' skinny/crates/ir/src/lib.rs
```

All four audit commands must return zero matches. JSON names may remain in
per-grammar generated templates/runtime output and JSON-specific facade APIs,
but not in the W8 generic codegen lower path, codegen public API, schema key
surface, or IR convenience helpers named above.

## Hard Cap

360 minutes for W8 redress. If the source delta grows past the SPEC sub-split
trigger or generated output drifts, close the sub-cycle as a measured rejection
or split into W8b with a fresh triumvirate.

## Revert Protocol

On failure:

1. Save the rejected source/status patch to
   `/tmp/skv7-wave-8-lock14-codegen-shell-rejected.patch`.
2. Restore source files and generated outputs.
3. Add a `skinny/REDRESS.md` entry with the failed grep/test/generation gate
   and the next candidate shape.
4. Commit:
   `docs(sk-v7-wave8-redress): reject lock14 phase-c-d codegen shell neutralization`.

On success:

1. Keep the codegen/IR neutralization changes.
2. Leave generated runtime, generated real-typed output, and `skinny/RESULTS.md`
   unchanged.
3. Add a `skinny/REDRESS.md` admit entry with correctness, generation, and
   audit evidence.
4. Commit:
   `feat(sk-v7-wave8): admit lock14 phase-c-d codegen shell neutralization`.

## Same-Wave Consumer

The same-wave consumers are:

- `xtask` JSON regeneration/check commands using neutral `codegen` APIs;
- `bbnf` facade compilation using neutral `codegen` APIs;
- codegen unit tests over the current JSON grammar;
- generated JSON runtime and real-typed modules, verified byte-identical by
  `check-json` and `check-real-typed`;
- IR validation tests covering regex nullability.

No new primitive, SIMD kernel, parser sidecar, directive, BIR variant, or
substrate is added.

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
