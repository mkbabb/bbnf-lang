# AZ-II O3a-J1 Plan - JSON Materialization, Parity, and Throughput

Status: plan lane output. No source redress is authorized by this
document. The orchestrator applies the amendment text below only after
all O3a plan lanes return and the shared O3/O4/O6 wave files can be
updated in one coordinated pass.

## Evidence Read

- `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt` records the
  O3a baseline: 1645 tests run, 1561 passed, 84 failed, 25 skipped.
- `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt` records
  `json_monolithic::data_xl` exceeding the bench cap at
  `2.478697958s` against a `1s` limit.
- Focused confirmation command:
  `cargo nextest run -p bbnf --test json_parity --test json_value_parity --test structural --test wrap_compound_elision --test serialize_roundtrip --test typed_accessor_surface --cargo-profile ax-iter bool_false_materialises_to_bool_false bool_true_materialises_to_bool_true every_declared_leaf_reaches_the_document nested_object_preserves_typed_payloads json_parses_bools json_parses_nested_object simdjson_parity_flat_object simdjson_parity_mixed_array simdjson_parity_scalars structural_object_two_pairs structural_scalar_bool_false structural_scalar_bool_true json_accessor_surface json_compile_time_accessors json_object_of_scalars_record_ceiling json_scalar_at_top_level_emits_one_record json_false json_true > /tmp/az-ii-o3a-j1-focused-nextest.txt 2>&1`
- Focused output confirmed these root signatures: bool roots materialise
  as null, `json_false`/`json_true` emit `"null"`, JSON accessor audit
  reports `Alt` coverage `0`, `structural_object_two_pairs` counts 3
  reachable nodes where the test expects 5, and simd-json object/array
  parity rejects integer oracle values such as `U64`.
- Current generated JSON evidence: `crates/core/src/grammar/generated/json.rs`
  emits `builder.push_leaf_with_unit()` in both `false` and `true`
  branches of `parse_keyword_JsonParser_bool`; the nearby comment still
  claims bool branches should call the bool leaf path.

## Root Cause Split

1. **O4.J1-K1 - JSON keyword bool payload emission.** The generated
   bool keyword parser consumes `true` and `false` but deposits the
   unit/null leaf. This is an emitter payload-return defect in the
   keyword/branch family and explains scalar bool failures, nested
   bool/null count drift, scalar serialize drift, and wrap scalar drift.
2. **O3.J1-P1 - JSON document projection/accessor accounting.** The
   JSON accessor audit has no Alt coverage and no KV-pair coverage.
   `structural_object_two_pairs` reaches the object and pair values,
   but the structural count contract still does not match the
   document-owned projection shape. O3 owns this because O3 already owns
   document projection and typed accessor proof while deleting generated
   view residue.
3. **O6.J1-H1 - JSON parity harness and corpus close.** The simd-json
   flat-object and mixed-array failures are not parse failures in the
   focused output; the bbnf side is numeric and the oracle reports an
   unsigned integer witness. O6 owns oracle-normalisation, corpus parity,
   and post-O5 source-owned residual assignment.
4. **O6.J1-B1 - JSON `data_xl` throughput proof.** The O3a bench did
   not produce a valid `data_xl` throughput number. O6 owns the fresh
   post-O5 sequential JSON bench lane, profile/symbol evidence, and the
   delta from the O3a timeout.

## Owner Table

| Failure or timeout | Primary owner | Required redress/proof |
|---|---|---|
| `bbnf::json_parity bool_false_materialises_to_bool_false` | O4.J1-K1 | Generated bool branch deposits `JsonValue::Bool(false)`. |
| `bbnf::json_parity bool_true_materialises_to_bool_true` | O4.J1-K1 | Generated bool branch deposits `JsonValue::Bool(true)`. |
| `bbnf::json_value_parity json_parses_bools` | O4.J1-K1 | Serde parity for `true` and `false` sees bool, not null. |
| `bbnf::json_value_parity simdjson_parity_scalars` | O4.J1-K1 | First scalar bool parity miss closes with the bool payload fix; O6 re-runs for numeric residuals. |
| `bbnf::structural structural_scalar_bool_false` | O4.J1-K1 | Root tree is `Bool(false)`. |
| `bbnf::structural structural_scalar_bool_true` | O4.J1-K1 | Root tree is `Bool(true)`. |
| `bbnf::serialize_roundtrip json_false` | O4.J1-K1 | Parse/emit returns `"false"`, not `"null"`. |
| `bbnf::serialize_roundtrip json_true` | O4.J1-K1 | Parse/emit returns `"true"`, not `"null"`. |
| `bbnf::json_parity every_declared_leaf_reaches_the_document` | O4.J1-K1 | Bool array entries survive as bool leaves. |
| `bbnf::json_parity nested_object_preserves_typed_payloads` | O4.J1-K1 | Null and bool counts stop aliasing. |
| `bbnf::json_value_parity json_parses_nested_object` | O4.J1-K1 | Nested `true` survives at `$.outer.inner[2]`. |
| `bbnf::wrap_compound_elision json_scalar_at_top_level_emits_one_record` | O4.J1-K1 | Top-level `true` and `false` land as scalar bool roots. |
| `bbnf::wrap_compound_elision json_object_of_scalars_record_ceiling` | O4.J1-K1 | Object scalar values include bool, not null. |
| `bbnf::typed_accessor_surface json_compile_time_accessors` | O4.J1-K1 | Runtime assertion on `JsonParser::parse("true")` passes after bool payload repair. |
| `bbnf::structural structural_object_two_pairs` | O3.J1-P1 | Document-owned projection/count contract is corrected and proves both pairs and scalar values are reachable. |
| `bbnf::typed_accessor_surface json_accessor_surface` | O3.J1-P1 | JSON accessor audit records nonzero Alt coverage and the required payload-bearing classes. |
| `bbnf::json_value_parity simdjson_parity_flat_object` | O6.J1-H1 | Numeric oracle witness comparison accepts equivalent integer/f64 values without hiding parse drift. |
| `bbnf::json_value_parity simdjson_parity_mixed_array` | O6.J1-H1 | Same numeric oracle fix, then full mixed-array structural comparison passes. |
| `bbnf::json_canonical_parity canonical_parity_twitter` | O6.J1-H1 | Re-run after O4/O3 fixes; any remaining canonical diff gets a source owner before O6 close. |
| `bbnf::json_parity parity_twitter_json` | O6.J1-H1 | Re-run sonic structural parity after O4/O3 fixes. |
| `bbnf::json_parity_struct native_parity_serde_twitter_json` | O6.J1-H1 | Re-run serde corpus parity after O4/O3 fixes. |
| `bbnf::json_parity_struct native_parity_serde_canada_json` | O6.J1-H1 | Re-run serde corpus parity after O4/O3 fixes. |
| `bbnf::sonic_rs_parity sonic_rs_parity_twitter` | O6.J1-H1 | Re-run sonic-rs corpus parity after O4/O3 fixes. |
| `bbnf::sonic_rs_parity sonic_rs_parity_data_xl` | O6.J1-H1 | Re-run sonic-rs large-corpus parity after O4/O3 fixes. |
| `json_monolithic::data_xl` timeout at `2.478697958s` | O6.J1-B1 | Fresh post-O5 JSON bench lane records a real value and cites delta from the timeout artifact. |

## Verification Commands

O4.J1-K1 post-redress:

```bash
cargo nextest run -p bbnf \
  --test json_parity \
  --test json_value_parity \
  --test structural \
  --test serialize_roundtrip \
  --test wrap_compound_elision \
  --test typed_accessor_surface \
  --cargo-profile ax-iter \
  bool_false_materialises_to_bool_false \
  bool_true_materialises_to_bool_true \
  every_declared_leaf_reaches_the_document \
  nested_object_preserves_typed_payloads \
  json_parses_bools \
  json_parses_nested_object \
  simdjson_parity_scalars \
  structural_scalar_bool_false \
  structural_scalar_bool_true \
  json_false \
  json_true \
  json_scalar_at_top_level_emits_one_record \
  json_object_of_scalars_record_ceiling \
  json_compile_time_accessors \
  --no-fail-fast \
  > /tmp/az-ii-o3a-j1-o4-k1.txt 2>&1
```

O3.J1-P1 post-redress:

```bash
cargo nextest run -p bbnf \
  --test typed_accessor_surface \
  --test structural \
  --cargo-profile ax-iter \
  json_accessor_surface \
  structural_object_two_pairs \
  --no-fail-fast \
  > /tmp/az-ii-o3a-j1-o3-p1.txt 2>&1
```

O6.J1-H1 semantic parity:

```bash
cargo nextest run -p bbnf \
  --test json_value_parity \
  --test json_canonical_parity \
  --test json_parity \
  --test json_parity_struct \
  --test sonic_rs_parity \
  --cargo-profile ax-iter \
  simdjson_parity_flat_object \
  simdjson_parity_mixed_array \
  canonical_parity_twitter \
  parity_twitter_json \
  native_parity_serde_twitter_json \
  native_parity_serde_canada_json \
  sonic_rs_parity_twitter \
  sonic_rs_parity_data_xl \
  --no-fail-fast \
  > /tmp/az-ii-o3a-j1-o6-h1.txt 2>&1
```

O6.J1-B1 throughput:

```bash
make ay-bench-close WAVE=O6 \
  > /tmp/az-ii-o6-bench-close.txt 2>&1
rg -n 'json_monolithic|data_xl|bench iteration exceeded|error:' \
  /tmp/az-ii-o6-bench-close.txt \
  docs/benchmarks/post-AZ-II-O6-json.txt \
  docs/benchmarks/post-AZ-II.json
```

O6 must also run the existing O6 symbol/profile gates before claiming
the throughput number as publishable close evidence.

## Wave-Amendment Text

### Amendment for `docs/tranches/AZ-II/waves/cutover/O3.md`

Append a new phase after O3.11:

```markdown
### AZ-II.cutover.O3.12 O3a J1 JSON Projection Integration

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-J1-plan.md`
O3.J1-P1. Repair JSON document-owned projection/accessor accounting so
the JSON accessor audit records nonzero Alt coverage and the structural
object-two-pairs contract proves both pairs and scalar values reachable
without generated view residue.

Files touched: `crates/core/tests/typed_accessor_surface.rs`,
`crates/core/tests/structural.rs`, and only the document-owned runtime
or emitter projection files already allowed by O3.

Sub-gate: `cargo nextest run -p bbnf --test typed_accessor_surface --test structural --cargo-profile ax-iter json_accessor_surface structural_object_two_pairs --no-fail-fast` passes and O3's generated-view scan remains clean.
```

Add this hard-gate bullet:

```markdown
7. O3a J1 O3.J1-P1 is closed: JSON accessor coverage and structural
   object projection pass on document-owned APIs.
```

### Amendment for `docs/tranches/AZ-II/waves/cutover/O4.md`

Append a new phase after O4.11:

```markdown
### AZ-II.cutover.O4.12 O3a J1 JSON Keyword Payload Integration

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-J1-plan.md`
O4.J1-K1. Repair the keyword/branch struct-return emitter contract so
JSON `true` and `false` branches deposit bool leaves through the
concrete document builder. This fix must be made in the existing
keyword/branch shape path while the legacy parsed-return wrapper and
fallback strategy are being deleted; no adapter return path may be
introduced.

Files touched: `crates/core/src/backend/rust/emitter/shapes/keyword/**`,
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/**`,
`crates/core/src/backend/rust/emitter/shapes/flat/**`,
`crates/core/src/backend/rust/emitter/shapes/dispatcher/**`,
`crates/core/src/grammar/generated/*.rs`, and the focused JSON tests
listed in the J1 plan if assertions need source-owned tightening.

Sub-gate: `/tmp/az-ii-o3a-j1-o4-k1.txt` records the focused bool,
nested object, wrap, serialize, and compile-time accessor tests passing
after the orchestrator-owned regen.
```

Add this hard-gate bullet:

```markdown
7. O3a J1 O4.J1-K1 is closed: generated JSON bool branches deposit
   bool leaves and no JSON scalar failure is hidden by an adapter return
   path.
```

### Amendment for `docs/tranches/AZ-II/waves/cutover/O6.md`

Append a new phase after O6.12:

```markdown
### AZ-II.cutover.O6.13 O3a J1 JSON Parity and data_xl Close

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-J1-plan.md`
O6.J1-H1 and O6.J1-B1. Re-run JSON value parity, canonical parity,
serde corpus parity, sonic-rs corpus parity, and the JSON bench lane
after O3/O4/O5. Numeric oracle comparisons must accept equivalent
integer/f64 witnesses without masking parse-tree drift. The JSON
`data_xl` bench must produce a real post-O5 measurement and cite the
delta from `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`.

Files touched: `crates/core/tests/json_value_parity.rs`,
`crates/core/tests/json_canonical_parity.rs`,
`crates/core/tests/json_parity.rs`,
`crates/core/tests/json_parity_struct.rs`,
`crates/core/tests/sonic_rs_parity.rs`,
`crates/core/benches/json/monolithic.rs`,
`crates/core/benches/json/value.rs`,
`docs/benchmarks/post-AZ-II-O6-json.txt`, and
`docs/benchmarks/post-AZ-II.json`.

Sub-gate: `/tmp/az-ii-o3a-j1-o6-h1.txt` passes; `post-AZ-II-O6-json.txt`
and `post-AZ-II.json` contain `data_xl` with no timeout marker,
placeholder, or failed-parse marker.
```

Add this hard-gate bullet:

```markdown
9. O3a J1 O6.J1-H1 and O6.J1-B1 are closed: all routed JSON corpus
   parity failures pass or block O7 with a source-owned residual, and
   `json_monolithic::data_xl` has a measured post-O5 value with the
   O3a timeout delta cited.
```

## Dispatch Order

1. O4.J1-K1 redress may run before O3 source redress because it fixes
   generated bool materialization used by O3/O6 verification.
2. O3.J1-P1 runs after or alongside O3 generated-view purge, but it
   cannot close until O4.J1-K1 no longer makes JSON bool roots null.
3. O6.J1-H1 and O6.J1-B1 run only after O3, O4, and O5 close. O6 may
   repair test harness/oracle code and bench command surfaces, but any
   remaining source materialization defect must be routed back to the
   owning implementation wave before parity or throughput is claimed.

## Non-Goals

- No source edits in this plan lane.
- No edits to shared O3/O4/O6 wave specs from this worktree.
- No adapter API that preserves a removed return model.
- No rescue path through retired cursor-shaped runtime state.
