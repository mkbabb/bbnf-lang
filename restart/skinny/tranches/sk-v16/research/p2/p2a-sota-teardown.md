# SK-V16 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V16.
Date: 2026-05-28.
Scope: asmjson, sonic-rs, simdjson, and yyjson comparator architecture keyed to the S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: scanner/string, scanner/whitespace, scanner/number, tape/view, generated product.
Lock surface: both.

## Section 1 - Findings

S-P1 names real parser leaves, not speculative kernels:

- string and escape scan: `first_control_byte`, `skip_string_plain_trusted`,
  `match_string_at_quote`, `validate_string_escape`, `read_hex_unit_scalar`;
- whitespace scan: `skip_ascii_whitespace`;
- number scan: `scan_digit_run`, `is_two_ascii_digits`, serde decimal parse;
- tape and view access: `JsonNodeKind::at_cursor`, `Tape::offset_at`,
  `string_body_range`, `next_sibling_cursor`;
- generated product leaves: `parse_type_unicode_escapes_document`,
  `parse_type_gsoc_proposal`, `parse_type_unicode_mixed_document`, string enum
  folds, and `DirectParser::skip_value`.

Local source anchors:

- `skinny/crates/parse-that-regex/src/lib.rs:686`-`700` is the current plain
  string scanner and first-control-byte loop.
- `skinny/crates/parse-that-regex/src/lib.rs:411`-`424` and `:1094`-`:1100`
  are escape and hex-unit scalar validation.
- `skinny/crates/parse-that-regex/src/number/mod.rs:106`-`165` is the current
  digit-run and two-digit scalar leaf.
- `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`35` and `:207`-`:275`
  are the JSON structural scanner; it already routes through the aarch64 NEON
  path at HEAD.
- `skinny/crates/runtime/src/tape/mod.rs:94`-`150` is the retained tape shape:
  offsets plus flag vectors, no public second tape.
- `skinny/crates/runtime/src/grammars/json/view.rs:355`-`430` is where view
  cursor walks and string-body ranges become S-P1 Mode III costs.

Comparator teardown:

| Comparator | Architecture claim | What bbnf lacks or must not copy |
|---|---|---|
| simdjson | Two-stage design: stage 1 identifies structure/string marks and validates UTF-8, stage 2 builds a tape and parses strings/numbers. Source: https://github.com/simdjson/simdjson/blob/master/HACKING.md#design-notes. | bbnf may borrow the stage-1 lesson only if the structural projection is the tape or a transient same-call producer. A retained `StructuralIndex`, sidecar stream, or second source pass violates Lock 1. |
| sonic-rs | SIMD is used for long strings, float fractions, field lookup, and whitespace; UTF-8 validation is default except unchecked APIs. Source: https://github.com/cloudwego/sonic-rs#benchmark and `#about-utf-8`. | bbnf already beats sonic strict on 51 JSON rows, but S-P1 still finds string/number/view leaves. Borrow primitive classes, not sonic's product plane. |
| yyjson | Strict RFC 8259 parser, accurate int/uint/double, DOM-oriented C API. Source: https://github.com/ibireme/yyjson#features. | yyjson is a product comparator, not a primitive oracle. Its relevance is strict number/string correctness and DOM/tape allocation discipline. |
| asmjson | 64-byte classification and SWAR/AVX-512 design are documented in the public crate docs. Source: https://docs.rs/asmjson/latest/asmjson/. | x86 implementation is out. The transferable idea is 64-byte byte-class/SWAR shape; any AVX-512 path remains rejected for Apple M5 Max scope. |

Local comparator source checks:

- `sonic-rs-0.5.7/src/util/arch/aarch64.rs:49` exposes a 64-byte
  `get_nonspace_bits` NEON path; `sonic-rs-0.5.7/src/parser.rs:1358` consumes
  it in whitespace skipping.
- `sonic-rs-0.5.7/src/util/string.rs:39` and
  `sonic-rs-0.5.7/src/parser.rs:904`-`942` show the `StringBlock` path used
  during string parsing.
- `sonic-rs-0.5.7/src/parser.rs:1402`-`1444` is the strict number skip path.
- `simd-json-0.14.3/src/impls/neon/stage1.rs:101`-`157` finds whitespace
  and structural masks on NEON; `simd-json-0.14.3/src/stage2.rs:99`-`164`
  consumes structural indexes in stage 2.
- `asmjson-0.2.6/README.md:188` documents DOM tape output with O(1)
  structural skips. That is useful pressure, but not a Lock 1-safe substrate
  import.

## Section 2 - Candidate Primitives

| Candidate | Shape | Scalar-ref status | Arch | P1 antecedent |
|---|---|---|---|---|
| `byte_class_from_table_64` | classify 64 bytes against a grammar-provided byte table, return bitmask | existing scalar at `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:2`; checkasm listed in `skinny/xtask/src/main.rs:17` | aarch64 TBL-backed candidate already selected through `skinny/crates/bbnf-simd/src/dispatch.rs:63`-`74` | structural scan tail, whitespace/terminator loops |
| `string_special_block_16` | for a 16-byte stripe, emit terminator, escape, control, and non-ASCII masks | existing scalar at `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`-`53`; needs dedicated checkasm before production wiring | aarch64 NEON compare plus movemask, `:57`-`:72` | string scan and escape validation |
| `escape_mask_64` | derive escaped-character mask and carry from backslash mask | existing scalar in `skinny/crates/bbnf-simd/src/lib.rs:175`-`205`; checkasm listed in `skinny/xtask/src/main.rs:22` | aarch64 wrapper currently selected through primitive table | quote/escape handling in JSON scan |
| `hex_quad_decode_x4` | decode four `\uXXXX` quartets or reject with scalar-identical error | scalar and NEON bodies exist in `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`-`166`; requires fresh same-wave consumer due prior Unicode rejects | aarch64 TBL | `read_hex_unit_scalar`, `hex_nibble`, unicode rows |
| `digit_block_accumulate` | classify and accumulate 4/8/16 ASCII digits into integer lanes | scalar fallback in `parse-that-regex` and a 4-digit DotProd body at `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5`-`49`; checkasm required before widening | aarch64 UDOT only if target feature and row-local consumer | number scan, Canada/numbers rows |
| `tape_cursor_step` | grammar-neutral cursor-kind/offset access over the sealed tape, no sidecar | scalar-only design candidate; no SIMD claim | non-SIMD substrate primitive | `JsonNodeKind::at_cursor`, `Tape::offset_at`, `next_sibling_cursor` |

## Section 3 - Grammar-Neutrality

`byte_class_from_table_64`, `string_special_block_16`, `escape_mask_64`, and
`digit_block_accumulate` are grammar-neutral when their inputs are generated
byte sets or generated numeric policies. They can serve JSON, CSS L4, Sheets,
and BBNF-self without encoding grammar names in `bbnf-simd`.

`hex_quad_decode_x4` is grammar-neutral only as a hex-quad decoder. A JSON
Unicode semantic materializer is not grammar-neutral and remains blocked unless
S-P3 gives it a generated semantic consumer for another grammar.

`tape_cursor_step` is grammar-neutral if it is a tape API refinement consumed
by generated views. It becomes a Lock 1 violation if it creates retained
cursor/list state, side tables, or parser-owned projections.

## Section 4 - Risks

- REDRESS 28+33 and 60-72 block tiny-string/StringBlock replay under old
  framing.
- REDRESS 50-55 and Lock 1 block retained side tables, event cursors,
  parser-local structural-mask cursors, decoded-string stats sinks, and
  renamed sidecars.
- REDRESS 80 blocks numeric route promotion without fresh BBNF-side hot-leaf
  evidence. S-P1 provides a fresh number leaf, but S-P3 must still bind an
  executable consumer and row-local measurement.
- REDRESS 82-84, 242-247 block one-quartet Unicode and decoded-string retry
  under old semantics.
- REDRESS 88/89 block PMULL/CSSC production promotion from ISA appeal alone.
  They can be candidates only with scalar reference, checkasm parity, and
  same-wave consumer proof.

## Section 5 - Sources

- `restart/skinny/tranches/sk-v16/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1c-samply-mode-3.md`
- `restart/locks/LOCKS.md:75`-`152`, `:603`-`:607`
- `skinny/REDRESS.md`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.7/src/util/arch/aarch64.rs`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.7/src/util/string.rs`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.7/src/parser.rs`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.14.3/src/impls/neon/stage1.rs`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/simd-json-0.14.3/src/stage2.rs`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/asmjson-0.2.6/README.md`
- simdjson HACKING design notes: https://github.com/simdjson/simdjson/blob/master/HACKING.md#design-notes
- sonic-rs README benchmark and UTF-8 notes: https://github.com/cloudwego/sonic-rs#benchmark
- yyjson README features: https://github.com/ibireme/yyjson#features
- asmjson docs: https://docs.rs/asmjson/latest/asmjson/
