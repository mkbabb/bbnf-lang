# AZ-II.cutover.O3a-J1 Redress Probe

Agent: AZ-II O3a-J1 redress/probe
Worktree: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress`
Branch: `codex/azii-o3a-j1-redress`
Status: halt pending plan amendment; ready for source redress after owning wave spec authorizes edits.

## Scope boundary

No source files were edited. This probe read the JSON tests, JSON runtime, checked-in generated JSON parser, and keyword emitter sources, then ran focused reproductions with:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress/target-azii-o3a-j1-redress
```

## Focused failure evidence

### Scalar bool materialization

Command:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress/target-azii-o3a-j1-redress cargo nextest run -p bbnf --cargo-profile ax-iter --test json_parity bool_true_materialises_to_bool_true bool_false_materialises_to_bool_false every_declared_leaf_reaches_the_document nested_object_preserves_typed_payloads --no-fail-fast -- --nocapture
```

Output summary:

- Exit: 100.
- Cold build completed in `4m 20s`; nextest then ran 4 selected tests.
- Summary: `4 tests run: 0 passed, 4 failed, 9 skipped`.
- `bool_false_materialises_to_bool_false`: `assertion failed: matches!(doc.root, JsonValue::Bool(false))`.
- `bool_true_materialises_to_bool_true`: `assertion failed: matches!(doc.root, JsonValue::Bool(true))`.
- `every_declared_leaf_reaches_the_document`: `assertion failed: matches!(items[1], JsonValue::Bool(true))`.
- `nested_object_preserves_typed_payloads`: `assertion left == right failed: two nulls must materialise; left: 4 right: 2`.

Likely root: `true` and `false` both materialize as `JsonValue::Null`, inflating null counts and erasing bool leaves.

Expanded/generated evidence:

- `target-azii-o3a-j1-redress/expand/json.rs` was produced with:

```bash
mkdir -p target-azii-o3a-j1-redress/expand && CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress/target-azii-o3a-j1-redress cargo expand --profile ax-iter -p bbnf grammar::generated::json > target-azii-o3a-j1-redress/expand/json.rs
```

- Exit: 0; finished in `40.89s`.
- `rg -n "parse_keyword_JsonParser_bool|push_leaf_with_unit\\(\\)|push_leaf_with_bool" target-azii-o3a-j1-redress/expand/json.rs` showed:
  - `1030: pub fn parse_keyword_JsonParser_bool<'p>(...)`
  - `1050: builder.push_leaf_with_unit();`
  - `1068: builder.push_leaf_with_unit();`
  - no `push_leaf_with_bool` inside the bool body.

Checked-in generated source matches the expanded output: `crates/core/src/grammar/generated/json.rs:1085` and `:1103` call `builder.push_leaf_with_unit()` for the `false` and `true` branches. Runtime maps unit to null in `crates/core/src/runtime/json/builder.rs:367`.

Emitter source already contains the intended struct-direct bool emission in `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:52-57`, selecting `builder.push_leaf_with_bool(((payload) as u32) != 0u32)`.

### Value parity and simd-json split

Command:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress/target-azii-o3a-j1-redress cargo nextest run -p bbnf --cargo-profile ax-iter --test json_value_parity json_parses_bools json_parses_nested_object simdjson_parity_flat_object simdjson_parity_mixed_array simdjson_parity_scalars --no-fail-fast -- --nocapture
```

Output summary:

- Exit: 100.
- Warm build completed in `0.51s`; nextest ran 5 selected tests.
- Summary: `5 tests run: 0 passed, 5 failed, 9 skipped`.
- Bool failures match scalar collapse: `$: shape divergence - bbnf=Null, serde=Bool(true)` and `$.outer.inner[2]: shape divergence - bbnf=Null, serde=Bool(true)`.
- Simd-json object/array failures show a separate oracle-adapter issue: `$.a: bbnf=Number but simd-json=U64` and `$[0]: bbnf=Number but simd-json=U64`.
- `simdjson_parity_scalars` first fails on bool collapse: `$: bbnf=Null but simd-json=Bool`.

Likely root split:

- Bool failures: same generated keyword bool bug.
- Simd-json U64 failures: test adapter in `crates/core/tests/json_value_parity.rs:189-195` calls `oracle.as_f64()` and treats simd-json `U64` as non-numeric, while serde parity accepts JSON number values through f64 coercion.

### Structural/accessor/wrap lane

Command:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress/target-azii-o3a-j1-redress cargo nextest run -p bbnf --cargo-profile ax-iter --test structural structural_scalar_bool_true structural_scalar_bool_false structural_object_two_pairs --test serialize_roundtrip json_true json_false --test typed_accessor_surface json_accessor_surface json_compile_time_accessors --test wrap_compound_elision json_scalar_at_top_level_emits_one_record json_object_of_scalars_record_ceiling --no-fail-fast -- --nocapture
```

Output summary:

- Exit: 100.
- Warm build completed in `0.57s`; nextest ran 9 selected tests across 4 binaries.
- Summary: `9 tests run: 0 passed, 9 failed, 45 skipped`.
- Serialize bools: `json_false` emitted `"null"` instead of `"false"`; `json_true` emitted `"null"` instead of `"true"`.
- Structural bools: `left: Null right: Bool(false)` and `left: Null right: Bool(true)`.
- Wrap scalar: `scalar "true": got Null, want Bool(true)`.
- Object scalar wrap: `expected scalar pair value (W2.6 wrap elision), got Null`.
- `structural_object_two_pairs` got root object and pair payload assertions before failing only on count: `expected at least 5 nodes ... got 3`.
- `json_accessor_surface` printed `Alt ... 0`, then failed `JSON must emit at least one Alt view (bool, value)`.
- `json_compile_time_accessors` failed `JsonParser::parse("true") must yield a Bool root`.

Likely root split:

- Serialize/structural/wrap scalar failures are downstream of bool materializing as null.
- `structural_object_two_pairs` appears to be an invariant mismatch: `crates/core/src/runtime/json/view.rs` documents object `children()` as pair values only, not keys, and `count_reachable` likewise excludes keys. The earlier pair-key/value assertions in the test pass before the count assertion fails.
- `json_accessor_surface` is not fixed by bool materialization alone; it is a view/projection emission issue where JSON has zero emitted Alt views.

### Throughput

Command:

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-j1-redress/target-azii-o3a-j1-redress cargo bench --profile profiling-prep -p bbnf --features competitor,stress,vm --bench json_monolithic -- data_xl
```

Output summary:

- Exit: 101.
- `profiling-prep` bench build completed in `1m 51s`.
- The filter did not prevent earlier lanes from running; observed:
  - `canada`: mean `219.7 ms`
  - `citm`: mean `4.975 ms`
  - `data_s`: mean `38.96 us`
- `data_xl` panicked at `crates/core/benches/json/../common/timeout.rs:112:17` with `bench iteration exceeded wall-clock limit - performance regression? (iteration took 2.480789125s, limit 1s)`.
- This reproduces the baseline timeout (`2.478697958s`) within noise.

Likely root: separate O6 throughput issue. The bool correctness patch will not materially change `data_xl`; a profile/expand pass against the post-correctness parser should own the performance root cause.

## Likely files to patch

- `crates/core/src/grammar/generated/json.rs`: generated output currently wrong for bool branches; do not hand-edit, regenerate after source/wave approval.
- `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs`: inspect first, but source already sketches the intended bool branch. If regen still emits unit, the bug is in IR typing/payload visibility feeding this emitter.
- `crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs`: verify `alt_branch_payload_value` sees `MapExpr::BoolLit` for JSON bool after current pipeline factoring.
- `crates/core/tests/json_value_parity.rs`: adjust simd-json number adapter to accept `U64`/`I64` numeric variants instead of treating `as_f64() == None` as non-numeric.
- `crates/core/tests/structural.rs` or `crates/core/src/runtime/json/view.rs`: decide whether keys are structural children. Current runtime docs say no; current test count expects yes.
- `crates/core/tests/typed_accessor_surface.rs` plus generated view/projection emitter files under `crates/core/src/backend/rust/view/**`: restore/route JSON Alt view emission for `bool`/`value` if still required after O3/O4 return-model decisions.
- `crates/core/benches/json/monolithic.rs` and generated parser hot path: O6 throughput proof after correctness redress.

## Proposed diff sketch

Do not deploy until the O3a-J1 plan/wave amendment authorizes source redress.

1. Regenerate JSON from the current emitter and inspect the generated bool body:

```text
cargo xtask regen --grammar json
```

Expected generated delta, produced by regen rather than hand edit:

```diff
- builder.push_leaf_with_unit();
+ builder.push_leaf_with_bool(false);
...
- builder.push_leaf_with_unit();
+ builder.push_leaf_with_bool(true);
```

2. If regen does not produce that delta, patch the source path so `emit_parse_keyword_struct_direct` receives `TypeDesc::Bool` and `alt_branch_payload_value` returns `0u32` / `1u32` for JSON bool branches after all factoring/inlining passes. Add a focused wire-contract asserting expanded/generated `parse_keyword_JsonParser_bool` contains `push_leaf_with_bool` and not two unit pushes.

3. Patch simd-json numeric parity separately:

```text
let oracle_f64 = oracle.as_f64()
    .or_else(|| oracle.as_u64().map(|v| v as f64))
    .or_else(|| oracle.as_i64().map(|v| v as f64))
    .unwrap_or_else(...);
```

4. Decide the object key structural contract before editing tests/runtime. If keys are not structural children, lower the `structural_object_two_pairs` count expectation to 3 and keep explicit key assertions. If keys must be structural children, update `JsonView::children()` and document the API shift.

5. Treat `json_accessor_surface` as a projection/view-emission lane, not as scalar bool fallout. Route to O3/P1 or O4 depending on whether generated views survive the return-model purge.

6. Re-run:

```bash
CARGO_TARGET_DIR=<unique> cargo nextest run -p bbnf --cargo-profile ax-iter --test json_parity --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique> cargo nextest run -p bbnf --cargo-profile ax-iter --test json_value_parity --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique> cargo nextest run -p bbnf --cargo-profile ax-iter --test structural --no-fail-fast -- --nocapture
CARGO_TARGET_DIR=<unique> cargo nextest run -p bbnf --cargo-profile ax-iter --test serialize_roundtrip json_true json_false -- --nocapture
CARGO_TARGET_DIR=<unique> cargo nextest run -p bbnf --cargo-profile ax-iter --test typed_accessor_surface json_accessor_surface json_compile_time_accessors -- --nocapture
CARGO_TARGET_DIR=<unique> cargo nextest run -p bbnf --cargo-profile ax-iter --test wrap_compound_elision --no-fail-fast -- --nocapture
```

7. After correctness redress lands, O6 should rerun `json_monolithic::data_xl` under the canonical close bench surface and profile the remaining 2.48s timeout.

## Disposition

Halt source redress now. Ready to proceed once the plan amendment assigns:

- scalar bool/generated freshness to a source-redress wave;
- simd-json numeric oracle handling to a test/parity subtask;
- object key structural count to either runtime API or test-invariant owner;
- Alt accessor emission to O3/P1 or O4;
- `data_xl` timeout to O6 post-correctness profiling.
