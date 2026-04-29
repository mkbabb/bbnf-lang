# AZ-II.cutover.O3a-S1 - Sheets Branch Payloads and Serialization
**Opens after**: AZ-II.cutover.O3a baseline capture and six-agent audit synthesis
**Agents**: up to 10 parallel
**Hard gate**: every Sheets branch, literal, operator, range, unary, corpus, and serializer failure has a proved root cause and source owner before O6 claims Sheets health.
**Status**: complete_with_misses

2026-04-29 Round 1 triad complete: research, plan, and redress/probe
artifacts exist under `docs/tranches/AZ-II/audit/O3a-S1-*.md`.
S1 is not return-model-owned; source redress is routed to S1-E1,
S1-R1, and S1-SER1 before O6 may claim Sheets health.

## Scope

1. Split Sheets failures into branch payloads, grammar admission,
   document projection, and serializer/self-parity lanes.
2. Prove whether failures belong to return-model deletion in O4, Sheets
   runtime payload handling, or serializer emission.
3. Create or amend the owning implementation wave before source
   redress lands.
4. Ensure O6 Sheets smokes only run after payload/serializer owners are
   green or explicitly blocking.

## Failure Assignment

| Lane | Failed tests |
|---|---|
| Branch/literal payloads | `bbnf::sheets_parity boolean_first_branch_fires_true_payload`; `bbnf::sheets_parity error_literal_error_branch_fires_payload`; `bbnf::sheets_parity error_literal_divzero_branch_fires_payload`; `bbnf::sheets_parity error_literal_first_branch_fires`; `bbnf::sheets_parity error_literal_factored_branch_fires_payload`; `bbnf::sheets_parity error_literal_name_branch_fires_payload`; `bbnf::sheets_parity error_literal_num_branch_fires_payload`; `bbnf::sheets_parity error_literal_ref_branch_fires_payload`; `bbnf::sheets_parity error_literal_spill_branch_fires_payload`; `bbnf::sheets_parity error_literal_value_branch_fires_payload` |
| Operator/range/unary admission | `bbnf::sheets_parity operator_branches_parse`; `bbnf::sheets_parity range_ref_parses_with_and_without_sheet_prefix`; `bbnf::sheets_parity unary_prefix_first_branch_fires_0u8` |
| Corpus self-parity | `bbnf::sheets_self_parity corpus_simple`; `bbnf::sheets_self_parity corpus_nested`; `bbnf::sheets_self_parity corpus_stress` |
| Serializer payloads | `bbnf::sheets_self_parity serialize_roundtrip_array_literal_multi_row`; `bbnf::sheets_self_parity serialize_roundtrip_array_literal_single_row`; `bbnf::sheets_self_parity serialize_roundtrip_error_generic`; `bbnf::sheets_self_parity serialize_roundtrip_error_na`; `bbnf::sheets_self_parity serialize_roundtrip_error_divzero`; `bbnf::sheets_self_parity serialize_roundtrip_error_name`; `bbnf::sheets_self_parity serialize_roundtrip_error_null`; `bbnf::sheets_self_parity serialize_roundtrip_error_num`; `bbnf::sheets_self_parity serialize_roundtrip_error_ref`; `bbnf::sheets_self_parity serialize_roundtrip_error_spill`; `bbnf::sheets_self_parity serialize_roundtrip_error_value`; `bbnf::sheets_self_parity serialize_roundtrip_range_ref_column`; `bbnf::sheets_self_parity serialize_roundtrip_range_ref_quoted_sheet`; `bbnf::sheets_self_parity serialize_roundtrip_range_ref_sheet_prefixed`; `bbnf::sheets_self_parity serialize_roundtrip_string_empty`; `bbnf::sheets_self_parity serialize_roundtrip_unary_plus`; `bbnf::sheets_self_parity serialize_roundtrip_unary_minus` |

## File Bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/O3a-S1-research.md` | create |
| `docs/tranches/AZ-II/audit/O3a-S1-plan.md` | create |
| `docs/tranches/AZ-II/waves/cutover/O4.md` | modify if return-model-owned |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | modify |
| `crates/core/src/runtime/google_sheets/**` | future redress |
| `crates/core/src/backend/rust/emitter/shapes/**` | future redress |
| `crates/core/tests/{sheets_parity,sheets_self_parity,sheets_expr_parity}.rs` | future redress |

**Do NOT touch**: JSON/CSS runtime, tape crate deletion, or benchmark
JSON in S1 research/plan lanes. Source redress begins only after owner
wave amendments land.

## Triumvirate Dispatch

| Lane | Agents | Deliverable |
|---|---:|---|
| Research | 3 | Branch/literal root cause; admission root cause; serializer root cause |
| Plan + wave creation | 1 | `O3a-S1-plan.md` plus O4/O6/source-owner amendments |
| Redress | up to 4 | Source commits within amended owner wave |
| Orchestrator | 1 | Integrate reports and run focused nextest |

## Hard Gate

1. `docs/tranches/AZ-II/audit/O3a-S1-research.md` identifies the root
   cause for all 33 failed Sheets tests.
2. `docs/tranches/AZ-II/audit/O3a-S1-plan.md` assigns each failure to
   O4, O6, or a named source redress child wave.
3. Post-redress `cargo nextest run -p bbnf --test sheets_parity --cargo-profile ax-iter -- --nocapture` passes.
4. Post-redress `cargo nextest run -p bbnf --test sheets_self_parity --cargo-profile ax-iter -- --nocapture` passes.
5. O6 cites S1 before claiming Sheets semantic health.

## Dependencies

- **Depends on**: AZ-II.cutover.O3a
- **Blocks**: O4 if return-model-owned; O6 Sheets close
