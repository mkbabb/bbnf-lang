# SK-V15 W2-B - Lock 16 Primitive Status Audit

Scope: read-only audit of `skinny/crates/bbnf-simd/**`,
`skinny/xtask/src/main.rs`, and SIMD/ASM report validation.

## Findings

The live aarch64 primitive surface is broader than the current report model.
`bbnf-simd` exports `aarch64`, `scalar`, and `x86_64` modules from
`skinny/crates/bbnf-simd/src/lib.rs:1`, while public wrappers live under
`prim` (`skinny/crates/bbnf-simd/src/lib.rs:251`). On aarch64,
`dispatch.rs` selects NEON implementations for:

- `byte_class_from_table_64`
- `bitmap_prefix_xor_64`
- `bitmap_next_set_bit`
- `bulk_emit_positions_64`
- `eob_pad_clamp`

`byte_class_from_eq_set_64` also dispatches to the aarch64 NEON body
(`skinny/crates/bbnf-simd/src/lib.rs:282`).

Source-present aarch64 native code includes `classify_tbl4`, `string_block`,
`utf8/validate_block`, `unescape_uxxxx`, `match_tiny_plain_string`,
`cache_hints`, and `digit_mac`. `cache_hints.rs` and `digit_mac.rs` contain
inline `asm!`; `digit_mac.rs` also carries `#[target_feature(enable =
"dotprod")]`.

`xtask primitive-checkasm` is strict when used: it runs with
`BBNF_SIMD_STRICT=1` and removes `BBNF_SIMD_INJECT_BUG`
(`skinny/xtask/src/main.rs:1845`). The fixed list omits
`checkasm_escape_mask_64`, even though that test exists and validates
`bbnf_simd::escape_mask_64` against a scalar reference
(`skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs:3`).

The SIMD/ASM report validator is route-level, not manifest-level.
`SkV13SimdAsmProductionReport` carries one `selected_primitive`, a global
`checkasm_status`, and hard-coded W12 CSS delimiter evidence
(`skinny/crates/bbnf-bench/src/report.rs:1057`,
`skinny/crates/bbnf-bench/src/report.rs:2391`). It does not enumerate every
source-present primitive, require Apple M5 Max identity, or mark x86 as
diagnostic-only. `gate.rs` verifies only report artifact hashes
(`skinny/crates/bbnf-bench/src/bin/gate.rs:2536`).

## Required Primitive Statuses

W2 primitive status must classify every `core::arch`, `target_feature`, and
`asm!` source-present primitive as exactly one of:

- `wired`
- `scalar-delegated`
- `deleted`
- `strict-checkasm-admitted`
- `architectural-block-with-REDRESS`
- `diagnostic-x86`

Apple M5 Max/aarch64 is the only admission host. x86/AVX rows can remain
source-present only as diagnostic or non-admission rows.

## W2 Redress Shape

Add `checkasm_escape_mask_64` to the strict `primitive-checkasm` list. Add a
gate-consumed primitive manifest, or an equivalent in-code validator, that
requires strict checkasm command evidence (`BBNF_SIMD_STRICT=1`), scalar
reference status, consumer path, rollback/REDRESS route, and final disposition
for every source-present primitive.
