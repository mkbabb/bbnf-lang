# AZ-II.cutover.O3a-J1 - JSON Materialization, Parity, and Throughput
**Opens after**: AZ-II.cutover.O3a baseline capture and six-agent audit synthesis
**Agents**: up to 10 parallel
**Hard gate**: every JSON failure and the `json_monolithic::data_xl` timeout have a proved root cause, a source-redress owner, and a post-redress verification command before O6 claims JSON parity or throughput.
**Status**: complete_with_misses

2026-04-29 Round 1 triad complete: research, plan, and redress/probe
artifacts exist under `docs/tranches/AZ-II/audit/O3a-J1-*.md`.
Source redress is routed to O3/O4/O6 and remains blocked until those
owning wave amendments are integrated.

## Scope

1. Split JSON failures into scalar payload, object/projection,
   accessor/wrap, corpus parity, and throughput lanes.
2. Prove whether the failures share branch-tag, leaf-payload,
   document projection, serializer, or return-model causes.
3. Create or amend the owning implementation wave before source
   redress lands.
4. Route return-model causes to O4, generated projection causes to O3,
   and parity/bench proof to O6.
5. Reject compatibility adapters that reintroduce tape-shaped or
   `Parsed<R>`-shaped APIs.

## Failure Assignment

| Lane | Failed tests |
|---|---|
| Scalar payloads | `bbnf::json_parity bool_false_materialises_to_bool_false`; `bbnf::json_parity bool_true_materialises_to_bool_true`; `bbnf::json_value_parity json_parses_bools`; `bbnf::json_value_parity simdjson_parity_scalars`; `bbnf::structural structural_scalar_bool_false`; `bbnf::structural structural_scalar_bool_true`; `bbnf::serialize_roundtrip json_false`; `bbnf::serialize_roundtrip json_true` |
| Object/projection | `bbnf::json_parity every_declared_leaf_reaches_the_document`; `bbnf::json_parity nested_object_preserves_typed_payloads`; `bbnf::json_value_parity json_parses_nested_object`; `bbnf::json_value_parity simdjson_parity_flat_object`; `bbnf::json_value_parity simdjson_parity_mixed_array`; `bbnf::structural structural_object_two_pairs` |
| Accessors/wrap | `bbnf::typed_accessor_surface json_accessor_surface`; `bbnf::typed_accessor_surface json_compile_time_accessors`; `bbnf::wrap_compound_elision json_object_of_scalars_record_ceiling`; `bbnf::wrap_compound_elision json_scalar_at_top_level_emits_one_record` |
| Corpus parity | `bbnf::json_canonical_parity canonical_parity_twitter`; `bbnf::json_parity parity_twitter_json`; `bbnf::json_parity_struct native_parity_serde_twitter_json`; `bbnf::json_parity_struct native_parity_serde_canada_json`; `bbnf::sonic_rs_parity sonic_rs_parity_twitter`; `bbnf::sonic_rs_parity sonic_rs_parity_data_xl` |
| Throughput | `json_monolithic::data_xl` timed out at `2.478697958s` during `make ay-bench-close WAVE=az-ii-doc-baseline` |

## File Bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/O3a-J1-research.md` | create |
| `docs/tranches/AZ-II/audit/O3a-J1-plan.md` | create |
| `docs/tranches/AZ-II/waves/cutover/O3.md` | modify if projection-owned |
| `docs/tranches/AZ-II/waves/cutover/O4.md` | modify if return-model-owned |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | modify for parity/throughput gates |
| `crates/core/src/runtime/json/**` | future redress |
| `crates/core/src/backend/rust/emitter/shapes/**` | future redress |
| `crates/core/tests/{json_*,sonic_rs_parity,structural,typed_accessor_surface,wrap_compound_elision,serialize_roundtrip}.rs` | future redress |
| `crates/core/benches/json/monolithic.rs` | future O6 redress/proof only |

**Do NOT touch**: `crates/tape/**`, `runtime/parsed.rs`, benchmark
result JSON, or non-JSON grammar runtimes in the research/plan lanes.
Source redress begins only after the plan lane commits the owning wave
amendment.

## Triumvirate Dispatch

| Lane | Agents | Deliverable |
|---|---:|---|
| Research | 3 | Scalar/payload root cause; projection/accessor root cause; corpus/bench root cause |
| Plan + wave creation | 1 | `O3a-J1-plan.md` plus O3/O4/O6 amendments naming exact owners |
| Redress | up to 4 | Source commits only within the amended owner wave; may halt with proof if substrate change exceeds this child spec |
| Orchestrator | 1 | Integrate reports, run focused nextest, and update progress |

## Hard Gate

1. `docs/tranches/AZ-II/audit/O3a-J1-research.md` names the root cause
   for each lane above.
2. `docs/tranches/AZ-II/audit/O3a-J1-plan.md` assigns every JSON test
   and the `data_xl` timeout to O3, O4, O6, or a named child wave.
3. Post-redress `cargo nextest run -p bbnf --test json_parity --cargo-profile ax-iter -- --nocapture` passes.
4. Post-redress `cargo nextest run -p bbnf --test json_value_parity --cargo-profile ax-iter -- --nocapture` passes.
5. Post-redress `cargo nextest run -p bbnf --test sonic_rs_parity --cargo-profile ax-iter -- --nocapture` passes or blocks O6.
6. O6 records a fresh `json_monolithic::data_xl` measurement and cites
   the delta from the O3a timeout artifact.

## Dependencies

- **Depends on**: AZ-II.cutover.O3a
- **Blocks**: O3 close if projection-owned; O4 close if return-model-owned; O6 JSON parity/performance close
