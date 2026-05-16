# SK-V6 B5 Primitive Gap Inventory

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-15
Scope: read-only inventory across `parse-that-regex`, `bbnf-simd`, generated JSON runtime/codegen, current results, and A6/SK-V6 audit reports. No repository files were edited.

## Authorities Read

- Current row authority: `skinny/RESULTS.md:3-21` retained parse rows and `skinny/RESULTS.md:25-45` direct rows.
- A6 ledger: `restart/skinny/audit/SK-V5-COHORT/skv5-A6-research-ledger.md:19-21`, `:157-180`, `:185-210`.
- SK-V6 synthesis: `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md:13-25`, `:40-109`, `:163-280`.
- Generated retained JSON: `skinny/crates/runtime/src/grammars/json/generated.rs:12-17`, `:90-156`, `:159-214`.
- Generated direct JSON: `skinny/crates/runtime/src/grammars/json/generated.rs:394-460`; `skinny/crates/codegen/src/json_sink_direct.rs:364-497`; `skinny/crates/codegen/src/json_typed_direct.rs:455-535`.
- JSON scan/capacity: `skinny/crates/runtime/src/grammars/json/scan.rs:1-54`, `:107-198`, `:200-276`.
- Parse primitives: `skinny/crates/parse-that-regex/src/lib.rs:127-257`, `:298-341`, `:516-705`, `:766-946`, `:979-1102`; `skinny/crates/parse-that-regex/src/number/mod.rs:31-272`.
- SIMD primitives: `skinny/crates/bbnf-simd/src/lib.rs:106-124`, `:170-223`, `:231-272`; `skinny/crates/bbnf-simd/src/dispatch.rs:49-87`; `skinny/crates/bbnf-simd/src/aarch64/mod.rs:1-32`.

## Current Row Map

Retained parse current G/NO-GO rows are `twitter` (`RESULTS.md:5`), `citm_catalog` (`:6`), `apache_builds` (`:8`), `github_events` (`:9`), `update_center` (`:10`), `random` (`:12`), `gsoc-2018` (`:13`), `instruments` (`:15`), `unicode_mixed` (`:17`), `unicode_escapes` (`:18`), `unicode_basic` (`:19`), `distinct_values` (`:20`), and `y_string_unicode` (`:21`). Current A/GO retained rows are `canada`, `mesh`, `marine_ik`, and `numbers` (`RESULTS.md:7`, `:11`, `:14`, `:16`).

Direct `direct_to_struct` current PASS rows are `citm_catalog`, `apache_builds`, `github_events`, and `instruments` (`RESULTS.md:29`, `:31-32`, `:39`). Current direct red rows are `twitter`, `canada`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, and `y_string_unicode` (`RESULTS.md:27`, `:30`, `:33`, `:35-38`, `:40-45`). The `real_typed_struct` rows for `twitter` and `update_center` pass but are a different output plane (`RESULTS.md:28`, `:34`).

Note: A6 is historical and records an older direct-pass count (`skv5-A6:19-21`). The current SK-V6 synthesis and `RESULTS.md` are stricter: current direct digest Track 1 is generated runtime, and only four `direct_to_struct` rows pass (`GRAND-SYNTHESIS-SK-V6.md:13-22`).

## Gap Inventory

| Area | Current primitive state | Gap | Exact affected rows | Grammar-neutral primitive name(s) |
|---|---|---|---|---|
| Classification / structural SIMD | `bbnf-simd` has grammar-neutral byte-class primitives: `byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`, and `byte_class_from_eq_set_64` exposed through `prim` (`bbnf-simd/src/lib.rs:231-272`) and selected through `PrimitiveKernels` (`dispatch.rs:49-87`). JSON scan consumes `eob_pad_clamp`, `escape_mask_64`, `prefix_xor_64`, `compact_mask`, and AArch64 table classifiers (`scan.rs:107-198`, `:200-276`). | No missing classifier primitive is the current close. `generated::attach_structural_index` still only asserts the alphabet and discards state (`generated.rs:12-17`), so structural scan is not a retained parser control substrate. SK-V6 says Canada structural scan is already green at 69075 Mbps vs 40000 Mbps floor (`GRAND-SYNTHESIS-SK-V6.md:24-25`). Remaining structural/container cost is generated cadence, not byte classification (`GRAND-SYNTHESIS-SK-V6.md:60-74`). | Structural/container cluster: `citm_catalog` retained G (`RESULTS.md:6`), secondary in `instruments` (`:15`) and `marine_ik` retained GO guard (`:14`). All retained rows consume generated parser control, but classifier repair should not be charged to parse-G rows as the primary gap. | Existing: `byte_class_mask_64_from_table`, `byte_class_mask_64_from_eq_set`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp_64`. Do not add: `retained_structural_sidecar_cursor` or `capacity_prescan_mask`; both are blocked route classes. |
| Retained string boundary | Generated retained string paths run `match_tiny_plain_string` first, then call `match_string_at_quote` / `match_json_string_at_quote_trusted_utf8` (`generated.rs:90-103`, `:142-156`, `:159-198`). Trusted parse-that matcher scans for quote, backslash, and control bytes without raw UTF-8 validation (`parse-that-regex/src/lib.rs:298-341`, `:679-705`). | The hot gap is matcher control/tail behavior, not raw UTF-8 validation and not deleting the tiny probe. R1b records that `skip_json_string_plain_trusted` scans AArch64 16-byte blocks, then 8-byte SWAR, then returns without scalar tail completion, causing outer byte stepping (`skv6-R1b:25-29`, `:90-112`). Candidate 1 deletion of the tiny probe and Candidate 2 always-wide scanner are rejected (`skv6-R1b:31-80`; `skv6-R2c:18-24`, `:95-106`). | Retained string-wrapper cluster: `twitter`, `random`, `unicode_mixed`, `unicode_basic`, `apache_builds`, `github_events`, `update_center`, `gsoc-2018`, `distinct_values`, `y_string_unicode`, plus string share of `instruments` (`GRAND-SYNTHESIS-SK-V6.md:40-58`; row lines `RESULTS.md:5`, `:8-10`, `:12-13`, `:15`, `:17`, `:19-21`). R1b next candidate targets `y_string_unicode`, `gsoc-2018`, `unicode_mixed`, with `twitter`, `instruments`, `canada` guards (`skv6-R1b:116-129`). | Existing: `tiny_plain_string_probe`, `quoted_span_match_trusted`, `string_special_mask_16`. Candidate scalar primitive/helper: `trusted_string_special_tail_scan`. Rejected/blocked now: `always_wide_string_special_mask_64`, `delete_tiny_plain_probe`, `raw_utf8_fused_string_scan`. |
| Direct string / output plane | Generated direct parser emits `ParsedString { raw, needs_unescape }` and routes to `JsonSink::*_source` (`generated.rs:401-445`; `json_sink_direct.rs:169-292`, `:364-399`). Direct sink number/string template preserves source spans and then materializes through sink/default decode. | Direct misses are split between generated string parser/receiver folding and escaped-string materialization. SK-V6 R3 shows direct Track 1 now reaches generated `parse_direct` and old bench-private diagnoses are invalid (`skv6-R3:49-77`). R2g says current `direct_to_struct` is a semantic full-digest stressor, not representative DirectBuild closure (`skv6-R2g:6-24`, `:80-137`). | Direct rows with receiver/fold or parser-string costs: `apache_builds`, `github_events`, `update_center`, `random`, `gsoc-2018`, `unicode_basic`, `distinct_values`, plus `twitter`/`instruments` generated parse-scanner dominated (`skv6-R3:35-47`, `:53-61`; row lines `RESULTS.md:27`, `:31-33`, `:36-37`, `:39`, `:43-44`). Direct string/Unicode cluster rows: `unicode_mixed`, `unicode_escapes`, `y_string_unicode`, plus string share of other red rows (`GRAND-SYNTHESIS-SK-V6.md:76-94`). | Existing: `source_span_string_event`, `needs_unescape_flag`. Candidate output-shape primitive/fact, not SIMD-first: `field_string_materialize_event`, `borrowed_or_owned_string_materializer`, `selected_field_string_fact`. Keep digest stressor separate from representative DirectBuild closure. |
| Unicode escape decode / unescape materializer | Parse-that has scalar `decode_json_unicode_escape` (`lib.rs:434-476`), escape-run validator (`:479-514`), AArch64-only x4 batch decoder call (`:516-591`), and public `unescape_json_string` (`:854-946`). `bbnf-simd` already provides `unescape_uxxxx`/x4 and surrogate join; R3e cites those as present and parity-covered (`skv6-R3e:19-34`). | Missing piece is not new SIMD vector semantics. R3e says admit the next close as a parse-that scalar/reference materializer rewrite reusing existing SIMD calls (`skv6-R3e:5-16`, `:55-68`). R1e identifies current output cost: x4 path decodes units then pushes chars one by one, and simple escapes dispatch through per-byte match arms (`skv6-R1e:29-59`). | Direct `unicode_escapes` (`RESULTS.md:42`) and `unicode_mixed` (`:41`) are primary; `y_string_unicode` (`:45`) is noisy but relevant; guards are `unicode_basic` (`:43`) and number/plain-string rows such as `numbers` (`:40`) or `distinct_values` (`:44`). R1e expected row impact is `unicode_escapes`, `unicode_mixed`, `unicode_basic`, `y_string_unicode`, plus guard row (`skv6-R1e:123-165`). | Existing: `hex4_to_u16`, `hex4x4_to_u16x4`, `surrogate_pair_join`, `string_special_mask_16`. Candidate: `escaped_run_decode_utf8`, with subpaths `unicode_escape_run_decode_utf8` and `simple_escape_run_decode_utf8`. Do not add a new `bbnf-simd` primitive unless new vector semantics appear. |
| Raw UTF-8 validation | Parser `parse_bytes` validates bytes into `&str` before generated retained parse (`parser.rs:54-67`). Trusted string matcher skips raw UTF-8 validation by design (`parse-that-regex/src/lib.rs:293-341`). Untrusted/string modes still validate code points (`:594-675`, `:979-1047`), and AArch64 16-byte validation exists (`unicode/utf8_block.rs:21-35`; `bbnf-simd/src/aarch64/utf8/validate_block.rs`). | Not a current primitive gap for retained parse. SK-V6 explicitly invalidates the SK-V5 prescription to fold raw UTF-8 validation into the NEON body scan (`GRAND-SYNTHESIS-SK-V6.md:46-58`, `:272-280`). | Guards: all retained rows, especially Unicode retained rows `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `y_string_unicode` (`RESULTS.md:17-21`). But current action should not target raw UTF-8 fusion. | Existing: `utf8_validate_block_16`, `utf8_codepoint_validate_scalar`. Blocked now: `raw_utf8_fused_string_scan`. |
| Float / decimal number materialization | Parse-that has number-span scanning with digit accumulation (`number/mod.rs:31-102`), SWAR 8/4/2-digit accumulation (`:106-223`), integer materializers (`:225-258`), and `materialize_f64` with Eisel-Lemire fast path and `str::parse` fallback (`:260-272`; `eisel_lemire/mod.rs:131-177`). Generated direct emits numbers through `match_number_span_from_first`, integer materializers, then `materialize_f64` (`json_sink_direct.rs:405-497`; `json_typed_direct.rs:509-535`). | The old missing Eisel-Lemire algorithm class is closed; remaining number gap is generated number-array materialization/emission shape, especially for `canada`. SK-V6 R3 attributes `canada` direct to `parse_number_array_direct` 49.1%, `materialize_f64` 12.3%, and `emit_number_array_direct` 11.2% (`skv6-R3:36`, `:55-56`); synthesis says no Wave 2 retained parse work should route through number parsing (`GRAND-SYNTHESIS-SK-V6.md:96-109`). | Direct number-heavy red rows in current matrix: `canada` (`RESULTS.md:30`, note `:163`), `mesh` (`:35`, note `:178`), `marine_ik` (`:38`, note `:190`), and `numbers` (`:40`, note `:197`). `numbers` retained parse is A/GO (`RESULTS.md:16`), so this is direct/materialization, not retained parse. | Existing: `decimal_span_scan`, `decimal_span_to_i64`, `decimal_span_to_u64`, `decimal_span_to_f64`. Candidate codegen/materialization names: `numeric_array_materialize_emit`, `number_sink_emit_typed`, `decimal_span_classify_int_float`. No new float SIMD primitive is indicated by current evidence. |

## Current Tests Covering These Surfaces

`parse-that-regex`:

- JSON shape and string tests: `numbers_match_json_shape`, `strings_report_escape_state`, `string_primitive_reports_flags`, byte-string mode, dense Unicode escape runs, invalid UTF-8, bad escapes/surrogates, error offsets, content prefilter, unescape, Unicode boundaries, invalid offsets, lone surrogates, and noncharacter acceptance (`parse-that-regex/src/lib.rs:1118-1352`).
- Number tests: span facts, integer widths, common integer materialization, and representative Eisel-Lemire bit matches (`number/mod.rs:379-448`).
- Integer boundary tests: `i64::MIN`, `u64::MAX`, and overflow (`number/integer.rs:56-76`).

`bbnf-simd`:

- Current admitted primitive gates listed in `CHECKASM-REPORT.md:230-244`: `BYTE_CLASS_FROM_TABLE_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `EOB_PAD_CLAMP`, plus dedicated `BYTE_CLASS_FROM_EQ_SET_64`, `checkasm_parity`, and `checkasm_utf8_block`.
- Reproduction/strict gate command: `cargo run -p xtask --release -- primitive-checkasm` runs release `checkasm_parity` and `checkasm_utf8_block` with `BBNF_SIMD_STRICT=1` (`CHECKASM-REPORT.md:205-217`). Strict `checkasm_parity` currently records the known NEON escape carry divergence in older report text (`CHECKASM-REPORT.md:102-126`); current dedicated primitive gates are stricter.
- Byte-class eq-set dedicated tests: alignment sweep, set-size sweep, adversarial seeds, Twitter corpus parity, empty set, constant fills, duplicate set entry, tail padding (`tests/checkasm_byte_class_from_eq_set_64.rs:192-508`).
- Classifier/checkasm tests: alignment sweep, random full alphabet, corpus parity, scalar anchors, AArch64 intrinsic parity, x86 intrinsic placeholder, robust bench mean (`tests/checkasm_parity.rs:239-684`).
- UTF-8/unescape tests: ASCII, multibyte complete, continuation across boundary, overlong/surrogates rejection, x4 unescape parity (`tests/checkasm_utf8_block.rs:11-59`).
- AArch64 primitive smokes: movemask, TBL classifier, selected classifier, quad load, byte-context shifts, string block masks, string-special scalar parity, digit MAC, dot product, streaming store (`tests/aarch64_primitives.rs:22-187`).

## Grammar-Neutral Primitive Vocabulary

Use these names in future reports/dispatch instead of JSON-specific or benchmark-row names:

- `byte_class_mask_64_from_table`
- `byte_class_mask_64_from_eq_set`
- `bitmap_prefix_xor_64`
- `bitmap_next_set_bit`
- `bulk_emit_positions_64`
- `eob_pad_clamp_64`
- `tiny_plain_string_probe`
- `quoted_span_match_trusted`
- `string_special_mask_16`
- `trusted_string_special_tail_scan`
- `hex4_to_u16`
- `hex4x4_to_u16x4`
- `surrogate_pair_join`
- `escaped_run_decode_utf8`
- `simple_escape_run_decode_utf8`
- `unicode_escape_run_decode_utf8`
- `decimal_span_scan`
- `decimal_span_classify_int_float`
- `decimal_span_to_i64`
- `decimal_span_to_u64`
- `decimal_span_to_f64`
- `source_span_string_event`
- `field_string_materialize_event`
- `number_sink_emit_typed`

## Bottom Line

The primitive layer is not empty: `bbnf-simd` already owns byte classification, prefix/carry, position emit, tail clamp, string-special, UTF-8 block, and `\uXXXX` decode surfaces with scalar/checkasm coverage. The current row gaps are:

1. Retained string matcher control/tail behavior, not raw UTF-8 fusion and not deleting `match_tiny_plain_string`.
2. Direct Unicode/string materializer shape through existing `unescape_json_string`, not new SIMD semantics first.
3. Direct numeric array materialization/emission shape, not a missing Eisel-Lemire primitive.
4. Generated structural/container cadence, not a missing structural classifier or scanner-floor primitive.

Any new SIMD primitive should be rejected unless it introduces vector semantics not already represented by the current `bbnf-simd` surface and lands with scalar executable spec, checkasm coverage, and a same-wave generated/runtime consumer.
