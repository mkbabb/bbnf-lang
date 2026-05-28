# SK-V16 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V16.
Date: 2026-05-28.
Scope: primitive gaps between S-P1 hot leaves, parse-that-regex, and bbnf-simd.
Output: this file.
P1 hot-leaf antecedents: scanner/string, scanner/whitespace, scanner/number, generated product.
Lock surface: both.

## Section 1 - Findings

The parse-that layer has useful scalar leaves but no complete grammar-neutral
primitive vocabulary for the P1 hot surface.

Current scalar leaves:

- `skip_string_plain_trusted` and `first_control_byte` live at
  `skinny/crates/parse-that-regex/src/lib.rs:686`-`712`.
- Escape validation and runs live at `skinny/crates/parse-that-regex/src/lib.rs:411`-`430`.
- Hex unit decode lives at `skinny/crates/parse-that-regex/src/lib.rs:1094`-`1100`.
- Number digit scanning lives at
  `skinny/crates/parse-that-regex/src/number/mod.rs:106`-`165`.

Current primitive layer:

- `bbnf_simd::prim` exposes byte-class table, byte-class eq-set, prefix XOR,
  next-set-bit, EOB clamp, and bulk emit through
  `skinny/crates/bbnf-simd/src/lib.rs:255`-`291`.
- `scan_dispatch` emits a public `StructuralIndex` at
  `skinny/crates/bbnf-simd/src/lib.rs:106`-`127`; for SK-V16, retained use must
  be rechecked against Lock 1 if it is not consumed as tape construction.

## Section 2 - Candidate Primitives

| Gap | Candidate shape | Layer | Scalar-ref status | Checkasm/parity |
|---|---|---|---|---|
| string special scan is tied to JSON names in parse-that call sites | `scan_until_special(bytes, terminator, escape, control_limit)` returning masks/first index | Layer 0 byte primitive | scalar exists in `aarch64/string_block.rs` but API not generalized | add dedicated checkasm before wiring |
| whitespace/delimiter search lacks a generated set surface | `find_ascii_set_member64(bytes, cursor, end, generated_set)` | Layer 0 byte primitive | existing scalar path through `bbnf-simd/src/lib.rs:209`-`226` | existing eq-set checkasm covers core |
| hex-unit scalar loop is one quartet at a time | `hex_quad_decode_x4` returning codepoints or reject mask | Layer 0 byte primitive | scalar and NEON bodies exist | needs dedicated unicode checkasm and semantic consumer |
| number scan has 4-digit DotProd body only | `digit_block_accumulate_8/16` plus digit-validity mask | Layer 0 byte primitive | 4-digit scalar/DotProd exists; widened scalar needed | new checkasm across lengths and non-digit poison cases |
| parser control still owns repeated cursor/kind lookups | `generated_value_step` over tape cursor and grammar table | Layer 1 grammar-neutral parser primitive | scalar design needed | golden parity, not checkasm |
| CSS/Sheets identifiers need class-run, not JSON string semantics | `skip_class_run_64(class_table, stop_set)` | Layer 0 byte primitive | scalar can be table-class plus first-set | checkasm byte-class plus generated consumer |
| generated parser composes layout skip plus expected delimiter checks repeatedly | `take_structural_after_layout(input, cursor, layout_set, expected_set)` | Layer 1 parse-that helper over Layer 0 byte-set masks | scalar design needed | golden/parser parity across EOF, invalid bytes, object/array delimiters |
| floating fallback remains product-plane expensive | `materialize_f64_exact_scalar_fallback(raw, span)` | Layer 1 number materializer | scalar design against Rust/serde bit parity | property tests first; no SIMD/checkasm until digit or mantissa kernel exists |

## Section 3 - Grammar-Neutrality

Layer 0 primitives must speak bytes, masks, offsets, and carries. Layer 1
primitives may speak generated grammar tables and tape cursor transitions. No
primitive may speak JSON object/key/string names in generic crates.

`hex_quad_decode_x4` is grammar-neutral as a hex decoder; JSON Unicode
materialization is not. `digit_block_accumulate` is grammar-neutral for JSON
numbers, CSS dimensions, Sheets coordinates, and BBNF repetition counts if
range and sign rules are generated above it.

## Section 4 - Risks

- REDRESS 28+33/60-72 reject tiny-string and retained-string replay.
- REDRESS 80 requires fresh row-local number evidence. S-P1 supplies a number
  leaf, but S-P3 must still prove row movement and same-wave consumer.
- REDRESS 82-84/242-247 block old Unicode materialization retries.
- `scan_dispatch` returning `StructuralIndex` is acceptable only when consumed
  as transient proof or tape construction. A retained public structural index is
  a Lock 1 fault.
- Do not admit a parse-that primitive because a comparator uses it. Admission
  requires scalar, parity, and same-wave consumer in this repo.
- `materialize_f64_exact_scalar_fallback` is a scalar parse-that gap, not a
  SIMD gap. It cannot be used to launder a numeric route around REDRESS 80.

## Section 5 - Sources

- `restart/skinny/tranches/sk-v16/research/p1/p1e-hot-leaf-attribution.md`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/xtask/src/main.rs:14`-`25`
- `restart/locks/LOCKS.md:603`-`:607`
