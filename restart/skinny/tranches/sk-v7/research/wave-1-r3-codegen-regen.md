# SK-V7 W1 R3 Codegen Regen Diagnosis

## Scope

Read-only diagnosis for W1 codegen/regeneration risk. Sources read:

- `restart/skinny/tranches/sk-v7/SPEC.md` §3.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` §3.
- `skinny/crates/codegen/src/json_templates/generated.rs`.
- `skinny/xtask/src/main.rs`.
- `skinny/crates/codegen/src/lib.rs`.
- Adjacent owner context in `skinny/crates/ir/src/lib.rs`,
  `skinny/crates/passes/src/lib.rs`,
  `skinny/crates/codegen/src/json_sink_direct.rs`, and
  `skinny/crates/codegen/src/lower/schema_direct.rs`.

No code was edited.

## Findings

1. W1 should not require a semantic edit to
   `skinny/crates/codegen/src/json_templates/generated.rs`. The template does
   not mention `TapeKind` or `DirectBuildDecode`; it imports JSON runtime view
   names and parse-that-regex JSON match types, which are W7/W8 Lock 14
   residue, not the W1 Class D rename target.

2. The generated runtime file actually checked by the skinny regen gate is
   `skinny/crates/runtime/src/grammars/json/generated.rs`. `xtask regen-json`
   calls `codegen::emit_json_from_source`, then writes the emitted file set into
   `crates/runtime/src/grammars/json`. The codegen template at
   `crates/codegen/src/json_templates/generated.rs` is an `include_str!`
   input to that emission path, not an output rewritten by `regen-json`.

3. `cargo run -p xtask --release -- check-json` is the live byte-equality
   command in this workspace. The older `cargo run -p xtask --release -- gen
   --check` wording appears in SK-V7 research, but `xtask` currently exposes
   `regen-json`, `check-json`, `regen-real-typed`, `check-real-typed`,
   `check-conformance`, `lint-loc`, `bench-json`, `gate-json`, and
   `primitive-checkasm`; there is no live `gen --check` subcommand.

4. "Byte-identical generated.rs excluding rename diff" is stricter here than it
   sounds. Because W1 renames IR enum variants and decode policy tags, not
   generated public JSON view types, the expected diff in both
   `crates/codegen/src/json_templates/generated.rs` and
   `crates/runtime/src/grammars/json/generated.rs` is empty. A diff containing
   `JsonObject`, `JsonString`, `JsonNumber`, direct shape roster text, or parser
   helper body changes is not a harmless W1 rename diff.

5. Current W1-relevant old symbols are concentrated as expected:
   `TapeKind::{Object,Array,Pair,String,Number,Bool,Null}` occurs only in
   `passes::materialization_for_rule` consumer arms, and
   `DirectBuildDecode::{JsonString,JsonNumber}` occurs in the IR enum
   definition. JSON public runtime names (`JsonString`, `JsonNumber`,
   `JsonObject`, etc.) remain in generated/runtime/codegen template files and
   are not a W1 deletion target.

6. The W1 Lock 14 HIGH reduction should be measured against the Class D target:
   the grammar-named `TapeKind` variant set plus
   `DirectBuildDecode::{JsonString,JsonNumber}`. Broader codegen JSON names are
   known remaining Phase C/W8 debt, and counting them against W1 would conflate
   phases.

7. `cargo test --workspace` is load-bearing for W1 because the rename crosses
   serde-derived IR types and BIR consumers. `codegen` unit tests also validate
   deterministic emission and that generated `generated.rs` still contains the
   sink-only lowered direct parser. If W1 deletes `materialization_for_rule`,
   passes tests around JSON-to-BIR shape and direct build emission are the first
   likely failures.

## Exact Commands

Read commands used:

```sh
sed -n '125,147p' restart/skinny/tranches/sk-v7/SPEC.md
sed -n '66,94p' restart/skinny/tranches/sk-v7/HANDOFF.md
sed -n '1,220p' skinny/crates/codegen/src/json_templates/generated.rs
tail -n 80 skinny/crates/codegen/src/json_templates/generated.rs
sed -n '110,155p' skinny/xtask/src/main.rs
sed -n '1,75p' skinny/crates/codegen/src/lib.rs
sed -n '190,210p' skinny/crates/codegen/src/lib.rs
sed -n '433,520p' skinny/crates/ir/src/lib.rs
sed -n '700,760p' skinny/crates/passes/src/lib.rs
rg -n 'TapeKind|DirectBuildDecode|JsonString|JsonNumber|materialization_for_rule' skinny/crates/codegen/src skinny/crates/ir/src skinny/crates/passes/src skinny/crates/runtime/src/grammars/json -g '!*target*'
rg -n 'check-json|regen-json|byte-identical generated|Lock 14 HIGH|TapeKind rename|Class D' restart/skinny/tranches/sk-v7/research restart/skinny/tranches/sk-v7/SYNTHESIS.md restart/skinny/tranches/sk-v7/SPEC.md restart/skinny/tranches/sk-v7/HANDOFF.md
```

Recommended W1 redress verification commands, run from `skinny/`:

```sh
cargo run -p xtask --release -- check-json
cargo test --workspace
git diff --exit-code -- crates/runtime/src/grammars/json/generated.rs crates/codegen/src/json_templates/generated.rs
rg -n 'TapeKind::(Object|Array|Pair|String|Number|Bool|Null)|DirectBuildDecode::(JsonString|JsonNumber)' crates/ir/src crates/passes/src
```

If implementation intentionally rewrites emitted runtime files, use this
sequence instead and inspect the diff before admission:

```sh
cargo run -p xtask --release -- regen-json
git diff -- crates/runtime/src/grammars/json/generated.rs
cargo run -p xtask --release -- check-json
```

Optional but prudent if DirectBuild lowering changes during the same W1 patch:

```sh
cargo run -p xtask --release -- check-real-typed
```

## Risks

1. Deleting `passes::materialization_for_rule` is more than a rename if no
   replacement direct-shape source is already wired. That can silently remove
   `TapeEmit` or `DirectBuild` nodes, and then `check-json` may fail through the
   generated direct parser append path.

2. Treating public `JsonString`/`JsonNumber` runtime type names as W1 targets
   would overreach into W7/W8 codegen/parse-that-regex cleanup and would likely
   perturb generated output. W1 should leave those names alone.

3. The SPEC mentions `generated.rs` under `crates/codegen/src/json_templates`,
   but the live regen equality gate checks `crates/runtime/src/grammars/json`.
   A W1 plan should name both surfaces explicitly: template unchanged unless
   deliberately edited; runtime generated file byte-identical after `check-json`.

4. There is no current `lint-no-hardcoded-grammars` or `gen --check` xtask
   command in the skinny workspace. W1 Lock 14 proof needs explicit `rg`
   evidence plus `check-json`, not a non-existent xtask invocation.
