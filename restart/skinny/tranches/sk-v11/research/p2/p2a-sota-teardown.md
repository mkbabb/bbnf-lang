# SK-V11 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: strict comparator architecture teardown keyed to SK-V11 S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan, string_escape_decode, unicode_escape_hex_decode, number_digit_span, ascii_whitespace_skip, container_dispatch, simd_movemask, output_digest_hash.
Lock surface: Lock 1 + Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

S-P1 names the accepted hot-leaf vocabulary and rows: string/tiny residuals
(`twitter`, `github_events`, `update_center`, `random`, `distinct_values`),
number/sequence rows (`canada`, `mesh`), unicode rows
(`unicode_escapes`, `y_string_unicode`), and the SIMD string-support row
(`gsoc-2018`). The canonical leaves are listed in
`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:104`.
Current S-P2 scope therefore targets direct and typed product planes, not
parse-only: `skinny/RESULTS.md:143` records overall `N-direct / NoGo`,
`skinny/RESULTS.md:144` names Track 1/Track 2, and `skinny/RESULTS.md:146`
states that C++ sidecars are historical or absent and never strict anchors in
W0. REDRESS 102 makes parse-only movement proof-only and non-behavioral
(`skinny/REDRESS.md:3040`).

| Comparator | Structural classification | String and number fast paths | Output plane | Strict comparator discipline | What it does that bbnf does not |
|---|---|---|---|---|---|
| asmjson | Classifies up to 64 bytes with AVX-512BW assembly or SWAR masks and uses those masks to skip whitespace/string bodies [A1][A6]. Portable Rust path has `ByteState { whitespace, quotes, backslashes, delimiters }` and a chunk FSM [A6]. | Strings are found by quote/backslash masks and backslash parity in the FSM [A2]. Numbers are stored as raw atom slices after JSON-number validation, including a C-callable validator for the assembly path [A3]. | SAX sink (`parse_with`) and flat DOM tape (`parse_to_dom`); the AVX-512 DOM entry writes `DomEntry` records directly into a preallocated array [A4][A5]. | Not a strict anchor for S-P2 gates: its README says bytes below `0x20` are whitespace and string contents are not scanned for unescaped controls [A7]. It is a flaw/probe source for architecture ideas only. | Direct-threaded x86 assembly, direct tape writes from assembly, and sink dispatch from assembly. bbnf has a generated scalar byte-dispatch parser (`skinny/crates/runtime/src/grammars/json/generated.rs:47`), an offset tape builder (`skinny/crates/runtime/src/tape/assembler.rs:42`), and sink-only direct parsing (`skinny/crates/runtime/src/grammars/json/generated.rs:409`), but no hand-written assembly producer and no direct assembly tape/SAX plane. |
| sonic-rs | Does not use simdjson-style two-stage parsing; it applies SIMD to long strings, number fractions, field/element lookup, and whitespace [S1]. It has 64-byte quote/backslash/in-string masks for skipping containers [S2]. | Uses zero-copy string visits when possible, a 24-byte scalar key fast path that rejects escapes/control bytes, raw-number visiting when configured, and AArch64 digit packing for 1-16 digits [S3][S5]. | Primary SOTA plane is direct Serde/typed product: README says it parses directly into Rust structs without a temporary tape [S1]. It also has `LazyValue`, `Number`, and `RawNumber` surfaces [S1][S4]. | `from_slice` validates UTF-8 and trailing content; `from_slice_unchecked` is unsafe and requires caller-guaranteed UTF-8, while `utf8_lossy` is an explicit permissive mode [S6]. S-P2 should compare against strict `from_slice` or direct strict rows only. | Direct object/array loops use `u16` pair probes for comma/value, colon/value, and `,"` separators [S3]. bbnf has admitted array next-byte carry in retained parsing (`skinny/crates/runtime/src/grammars/json/generated.rs:348`) but direct object/array product loops still re-enter byte dispatch (`skinny/crates/runtime/src/grammars/json/generated.rs:468`, `:508`) and do not have sonic's short-key Serde visitor path. |
| simdjson | Stage 1 scans 64/128-byte blocks into `structural_indexes` [J1]. Its scanner computes string masks, structural/operator masks, scalar starts, whitespace masks, escape parity, UTF-8 checks, and unescaped-control detection [J3][J4][J5]. AArch64 classification uses TBL/TBX-style tables and movemask packing [J6]. | Strings expose raw JSON strings and low-level unescape into caller buffers, while On Demand parses numbers from scalar spans at access time [J7][J8]. | DOM tape is an array of 64-bit words; strings live on a separate string tape, and array/object open entries point to matching close entries for skips [J2]. On Demand walks the structural index without materializing a DOM value tree [J7][J8]. | Strict DOM/On Demand is a valid JSON comparator, but C++ sidecars are not direct-product anchors in W0 (`skinny/RESULTS.md:146`). On Demand raw-key lookup intentionally matches raw keys without unescaping [J8], so direct typed-field comparators must align lookup semantics. | A retained structural-index producer plus On Demand cursor is the main delta. bbnf tried full class-column and streaming cursor shapes and rejected them (`skinny/REDRESS.md:2795`, `:2850`, `:2910`). Any simdjson lesson must be a transient producer consumed as the single tape/direct substrate under Lock 1 (`restart/locks/LOCKS.md:52`), not a sidecar. |
| yyjson | Portable ANSI C, no explicit SIMD, strict RFC 8259 default, and performance built around inlined FSM/goto parsing plus branch-predictable unrolled loops [Y1][Y4]. | Strings use an unrolled 16-byte ASCII skip/copy loop, validate UTF-8 unless invalid-unicode is explicitly allowed, and decode `\uXXXX` plus surrogate pairs in the string reader [Y5]. Numbers use unrolled integral/fraction digit loops and strict leading-zero/fraction/exponent checks, with optional raw-number/bignum flags [Y6]. | Immutable DOM arena: `yyjson_val` is 16 bytes (`tag` + payload union), arrays/objects store lengths and relative offsets, and `yyjson_read_max_memory_usage` documents the 16-byte-per-value sizing [Y3][Y7]. | Use default flags (`0`) only. `yyjson_read` accepts flags where `0` means no options, and permissive JSON5/invalid-unicode/extended-number/extended-whitespace flags are explicitly non-standard [Y2][Y3]. | yyjson's comparator lesson is i-cache and value-pool discipline, not a second bbnf DOM. bbnf's retained plane is an offset tape with source, offsets, sparse flags, and payload arena (`skinny/crates/runtime/src/tape/mod.rs:94`), and direct-only `SinkOnly` retains no queryable document identity per Lock 1. Replacing it with yyjson's DOM arena would violate the current substrate contract. |

Strict-vs-strict takeaway: sonic-rs strict direct/typed rows are the direct gate;
yyjson default and simdjson strict DOM are useful architecture comparators but not
direct-product anchors in the current W0 table; asmjson is a non-strict
architecture probe. The target surface remains `direct_to_struct` digest and
`real_typed_struct`, because S-P1/P1-F classify parse-only as diagnostic and W0
direct rows as the unresolved product surface
(`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:123`,
`skinny/RESULTS.md:95`).

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

1. `byte_class_mask64`
   - Shape: `fn(&[u8; 64], ByteClassSpec) -> ClassMasks64`, where the spec is
     grammar metadata for whitespace, quote, escape, structural/operator, and
     scalar-start classes. It returns transient bitmasks only; no retained
     index, class column, or sidecar.
   - Scalar-ref status: required first; scalar loop/SWAR reference must be the
     oracle. SIMD checkasm parity required for any AArch64 body.
   - Arch: scalar/SWAR, then AArch64 NEON TBL/TBX plus movemask. x86 AVX-512 is
     comparator evidence only.
   - P1 antecedent: `ascii_whitespace_skip`, `bounded_plain_string_scan`,
     `container_dispatch`, `simd_movemask`.
   - Comparator antecedent: asmjson `ByteState`, sonic-rs string/nonspace masks,
     simdjson `json_character_block::classify` [A6][S2][S5][J6].

2. `string_special_block16_or64`
   - Shape: `fn(ptr, terminator, escape, control_limit) -> { terminator_mask,
     escape_mask, control_mask }`, with a first-special-byte helper and no UTF-8
     policy beyond caller-selected mode.
   - Scalar-ref status: existing scalar and 16-byte AArch64 shape exist in bbnf
     (`skinny/crates/parse-that-regex/src/lib.rs:547`,
     `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`); any widening needs a
     scalar executable mirror and same-caller proof.
   - Arch: scalar/SWAR, AArch64 NEON. 64-byte widening is research-only until it
     clears the REDRESS 61/62 failure class.
   - P1 antecedent: `bounded_plain_string_scan`, `string_escape_decode`,
     `simd_movemask`.
   - Comparator antecedent: sonic-rs long-string SIMD, simdjson string scanner,
     yyjson unrolled ASCII skip [S1][S2][J4][Y5].

3. `prefix_xor_quote_state64`
   - Shape: `fn(quote_mask, escape_mask, prev_in_string, prev_escaped) ->
     { in_string_mask, real_quote_mask, next_in_string, next_escaped }`.
   - Scalar-ref status: required; reference must use scalar/SWAR prefix xor and
     the exact JSON escape-parity law.
   - Arch: scalar/SWAR production body first; AArch64 NEON bit operations only
     behind row gates. PMULL is pre-blocked as the default hot body.
   - P1 antecedent: `bounded_plain_string_scan`, `container_dispatch`,
     `simd_movemask`.
   - Comparator antecedent: sonic-rs `get_string_bits` and simdjson
     `json_string_scanner`/`json_escape_scanner` [S2][S5][J4][J5].

4. `whitespace_run_skip64`
   - Shape: `fn(bytes, offset, WhitespaceSet) -> offset`, with a block classifier
     that skips only the grammar-declared whitespace set.
   - Scalar-ref status: bbnf has scalar/SWAR reference for JSON whitespace
     (`skinny/crates/parse-that-regex/src/lib.rs:113`); generic form needs
     metadata-driven table tests.
   - Arch: scalar/SWAR, AArch64 NEON byte-class mask.
   - P1 antecedent: `ascii_whitespace_skip`, `container_dispatch`.
   - Comparator antecedent: sonic-rs `get_nonspace_bits`, simdjson whitespace
     classification, yyjson `char_is_space`/`skip_trivia` split [S5][J6][Y4].

5. `separator_pair_probe16`
   - Shape: `fn(ptr, SeparatorSpec) -> PairAction`, where the action carries
     `Next(byte)`, `Done`, or `SlowPath`. It covers comma+next, colon+value, and
     comma+quote probes in direct/typed loops only.
   - Scalar-ref status: trivial scalar `u16` reference required with endian tests.
   - Arch: scalar unaligned 16-bit load; no SIMD required.
   - P1 antecedent: `container_dispatch`, `ascii_whitespace_skip`.
   - Comparator antecedent: sonic-rs object/array pair reads [S3].
   - Discipline: do not reopen retained object next-key carry; REDRESS 65
     rejected that shape (`skinny/REDRESS.md:1637`). This candidate is only for
     direct/typed product loops with measured same-wave consumer rows.

6. `digit_block_accumulate_16`
   - Shape: `fn(bytes, max_digits) -> { value_u64, digits_consumed, non_digit }`
     plus a caller-owned JSON-number grammar validator. It accelerates digit
     accumulation, not number policy.
   - Scalar-ref status: current bbnf has 8/4/2-digit scalar/SWAR span parsing
     (`skinny/crates/parse-that-regex/src/number/mod.rs:106`); 16-digit body
     needs scalar mirror, overflow fixtures, and direct-row consumer.
   - Arch: scalar/SWAR, AArch64 NEON unzip/multiply-add or UDOT-shaped variant
     if P2-C admits the instruction route.
   - P1 antecedent: `number_digit_span`.
   - Comparator antecedent: sonic-number AArch64 digit pack/accumulate and yyjson
     unrolled 19-digit loops [S7][Y6].
   - Discipline: no mantissa/table-only route; REDRESS 80 found zero canada
     fallback pool (`skinny/REDRESS.md:2215`).

7. `hex_escape_quad_decode`
   - Shape: `fn(&[u8; 4]) -> Result<u16, HexError>` and optional
     `fn(&[u8; 16]) -> QuadHexResult` for four contiguous `uXXXX` units, with
     surrogate policy outside the raw hex primitive.
   - Scalar-ref status: current scalar hex unit exists through
     `read_hex_unit_scalar`/`hex_nibble` per S-P1
     (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:108`);
     SIMD needs scalar oracle and caller-level direct row gate.
   - Arch: scalar, AArch64 TBL/TBX nibble lookup; existing four-unit proof is
     proof-only until a real production delta appears.
   - P1 antecedent: `unicode_escape_hex_decode`, `string_escape_decode`.
   - Comparator antecedent: yyjson `read_uni_esc` and simdjson unescape buffer API
     [Y5][J7].

8. `live_mask_to_tape_or_sink`
   - Shape: consume a live structural/string/scalar mask in the same loop that
     writes `TapeBuilder` offsets or direct sink events; no `Vec<Structural>`,
     no retained class column unless it is the tape projection itself.
   - Scalar-ref status: current scalar reference is the generated parser plus
     `TapeBuilder::push_plain_offset` (`skinny/crates/runtime/src/grammars/json/generated.rs:292`,
     `skinny/crates/runtime/src/tape/assembler.rs:71`).
   - Arch: scalar first, AArch64 NEON mask producer only with same-loop consumer.
   - P1 antecedent: `container_dispatch`, `simd_movemask`,
     `ascii_whitespace_skip`.
   - Comparator antecedent: simdjson stage1-to-stage2 structural index and
     asmjson direct DOM writes [J1][J2][A4].
   - Discipline: this is a substrate candidate, not a sidecar candidate. It must
     satisfy Lock 1 and not revive REDRESS 96/97/98.

9. `output_digest_hash_block`
   - Shape: `fn(hash_state, bytes_or_decoded_segment) -> hash_state`, with
     product-plane hooks for borrowed raw string bytes and decoded escape segments.
   - Scalar-ref status: scalar hash reference must be bit-exact with current
     digest. No semantic string fact side tables.
   - Arch: portable hash first; AArch64 AES/PMULL-adjacent hash body only if
     P2-C and REDRESS constraints allow it.
   - P1 antecedent: `output_digest_hash`, `string_escape_decode`.
   - Comparator antecedent: sonic-rs direct Serde visitor and asmjson SAX sink
     show output can be produced without DOM allocation [S1][A5].
   - Discipline: REDRESS 54/55/66-69 rejected exact decoded stats, fused
     materializer, source-hook field-layout, and semantic fact hashing routes;
     any new digest primitive must have a different measured consumer and cross
     direct floors, not just remove allocation.

Non-candidates from comparator teardown: retained structural sidecars,
parse-only stage1 wins, asmjson permissive whitespace/control behavior, yyjson
DOM replacement, and x86-only AVX-512 assembly kernels. They are useful
architectural pressure, but they do not satisfy Lock 1, Lock 14, or the
strict direct-product comparator contract.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Grammar-neutral verdict |
|---|---|
| `byte_class_mask64` | Generalisable. Byte sets come from grammar metadata: JSON structurals, CSS delimiter/comment/string sets, Sheets separators, and BBNF-self token delimiters can all compile to class tables under Lock 14. |
| `string_special_block16_or64` | Generalisable as a quoted-token scanner with grammar-provided terminator, escape byte, and control policy. CSS strings and identifiers differ in policy, but the primitive is byte-set based. |
| `prefix_xor_quote_state64` | Generalisable only for grammars with quote/escape parity regions. For CSS comments or BBNF block strings, the same API can carry a different region-state policy; JSON must not be baked into the generic crate. |
| `whitespace_run_skip64` | Generalisable. Whitespace set is metadata; JSON has four bytes, CSS may include comments via caller policy, and Sheets/BBNF can provide their own skip sets. |
| `separator_pair_probe16` | Generalisable as a grammar-template optimization over delimiter+next-byte pairs. It is not a generic JSON function; codegen emits it from first/follow metadata for direct/typed loops. |
| `digit_block_accumulate_16` | Generalisable to decimal digit spans. JSON number grammar, CSS numeric tokens, Sheets numbers, and BBNF integer literals can share digit accumulation while callers own grammar policy. |
| `hex_escape_quad_decode` | Generalisable as a fixed-width hex decoder. JSON `uXXXX`, CSS hex escapes, Sheets escapes, and BBNF unicode literals differ in terminators and surrogate policy, so only raw nibble decode belongs in a generic primitive. |
| `live_mask_to_tape_or_sink` | Generalisable if the mask producer consumes grammar-declared structural alphabets and the output is the existing tape/sink substrate. JSON-only class enums in generic crates would violate Lock 14. |
| `output_digest_hash_block` | Generalisable as an output-plane hash fold over bytes/segments. The sink contract and checksum semantics are per benchmark/product, but the primitive must not know JSON fields or corpora. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- Lock 1 forbids parallel substrates: a SIMD mask stream may be transient, and
  retained structural offsets must be the tape projection itself
  (`restart/locks/LOCKS.md:52`). Lock 14 forbids grammar-specific code in
  generic crates (`restart/locks/LOCKS.md:78`).
- Parse-only is diagnostic. REDRESS 102 admits the parse firewall and reports
  17 parse rows with no parse row outside `S / NO-GO`
  (`skinny/REDRESS.md:3040`). S-P2-A candidates must target direct/typed
  product planes.
- Retained side-table/cursor routes are blocked. REDRESS 50 rejects parse-time
  aux side tables (`skinny/REDRESS.md:715`); REDRESS 51 rejects byte-class
  whitespace cursors and precomputed `StructuralIndex`/`Vec<JsonEvent>` routes
  (`skinny/REDRESS.md:742`); REDRESS 53 rejects parser-local structural cursors
  as second scanners (`skinny/REDRESS.md:784`).
- Full union/class-column structural substrate routes are retired for SK-V9:
  REDRESS 96, 97, and 98 rejected full class-column, streaming cursor, and the
  union-substrate thesis (`skinny/REDRESS.md:2795`, `:2850`, `:2910`).
- String widening is pre-blocked unless the candidate is materially different.
  REDRESS 61 and 62 rejected always-wide and delayed-wide 64-byte trusted string
  scanners (`skinny/REDRESS.md:1380`, `:1439`), and REDRESS 106 rejected the full
  string primitive micro-proof (`skinny/REDRESS.md:3150`).
- Unicode escape batching is not enough by itself. REDRESS 64 rejected the
  retained four-unit validator in production (`skinny/REDRESS.md:1582`);
  REDRESS 107 accepted a hex escape proof only, and REDRESS 108 rejected
  production movement because no real source delta crossed direct floors
  (`skinny/REDRESS.md:3172`, `:3198`).
- Direct string/digest rewrites are fragile. REDRESS 54/55 rejected exact
  decoded stats and fused quote-source materialization
  (`skinny/REDRESS.md:815`, `:846`); REDRESS 66-69 remain the string allocation,
  receiver, byte-writing, and semantic-fact hashing pre-block family.
- Numeric fallback widening is blocked. REDRESS 80 found zero canada f64
  fallback pool and requires any next numeric route to name a fresh numeric
  scan/dispatch hot leaf (`skinny/REDRESS.md:2215`).
- PMULL/CTZ bit tricks are not automatically admissible. REDRESS 88 rejected
  PMULL prefix-XOR as the default hot body, and REDRESS 89 rejected the CTZ bulk
  consumer after JSON row regressions (`skinny/REDRESS.md:2508`,
  `skinny/REDRESS.md:2542`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

External primary sources:

- [A1] asmjson README, 64-byte AVX-512BW/SWAR classification and skip claim:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/README.md#L7-L12
- [A2] asmjson Rust FSM and string quote/backslash mask path:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L657-L760
- [A3] asmjson number validator and C entry point:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L275-L334
- [A4] asmjson AVX-512 DOM entry point writes `DomEntry` values directly:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L190-L223
  and
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L395-L516
- [A5] asmjson SAX trait and public `parse_with` / `parse_with_zmm` sink plane:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/sax.rs#L1-L47
  and
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L613-L650
- [A6] asmjson `ByteState` and SWAR classifier:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/src/lib.rs#L1182-L1278
- [A7] asmjson strictness caveat:
  https://github.com/atomicincrement/asmjson/blob/3d6965d5a013677198366758cb50fb8637d54d58/README.md#L209-L222
- [S1] sonic-rs README, SIMD usage and direct Rust-struct parse with no tape:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/README.md#L53-L90
- [S2] sonic-rs 64-byte string bits and container skip:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L153-L200
- [S3] sonic-rs key fast path, raw-number visitor, direct object/array pair probes:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L336-L527
- [S4] sonic-rs LazyValue raw slice path:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/parser.rs#L654-L684
- [S5] sonic-rs AArch64 whitespace bitmask and prefix-xor helpers:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/util/arch/aarch64.rs#L20-L77
- [S6] sonic-rs strict/lossy/unchecked UTF-8 discipline:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/serde/de.rs#L83-L85
  and
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/src/serde/de.rs#L1306-L1334
- [S7] sonic-number AArch64 digit pack/accumulate:
  https://github.com/cloudwego/sonic-rs/blob/03545a9530346fe279b674dd496e037d94204bc5/sonic-number/src/arch/aarch64.rs#L1-L137
- [J1] simdjson stage1/stage2 dispatch:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/haswell.cpp#L132-L148
- [J2] simdjson tape layout, string tape, and skip pointers:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/doc/tape.md#L4-L6
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/doc/tape.md#L70-L73
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/doc/tape.md#L120-L144
- [J3] simdjson structural/scalar scanner:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_scanner.h#L15-L160
- [J4] simdjson string scanner:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_string_scanner.h#L14-L92
- [J5] simdjson escape scanner and structural indexer:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_escape_scanner.h#L14-L144
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/generic/stage1/json_structural_indexer.h#L193-L285
- [J6] simdjson AArch64 character classification:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/src/arm64.cpp#L40-L80
- [J7] simdjson On Demand unescape API and scalar accessors:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/include/simdjson/generic/ondemand/parser.h#L348-L392
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/include/simdjson/generic/ondemand/value_iterator-inl.h#L551-L623
- [J8] simdjson raw string/key semantics:
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/include/simdjson/generic/ondemand/value.h#L250-L267
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/include/simdjson/generic/ondemand/value.h#L335-L354
  https://github.com/simdjson/simdjson/blob/7732480b25c63e9a12c87dcfd0d68e6c7dd354b9/include/simdjson/generic/ondemand/value.h#L437-L438
- [Y1] yyjson README features and default sample read:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/README.md#L10-L18
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/README.md#L83-L109
- [Y2] yyjson read flags, including non-standard/permissive modes:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L760-L830
- [Y3] yyjson read APIs and 16-byte value sizing:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L920-L1003
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L1104-L1115
- [Y4] yyjson whitespace and FSM/goto parser:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L875-L902
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L3355-L3405
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L5210-L5325
- [Y5] yyjson string reader, ASCII skip/copy, UTF-8 validation, escapes:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L4666-L5065
- [Y6] yyjson number reader:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L3880-L4075
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.c#L4510-L4625
- [Y7] yyjson value layout:
  https://github.com/ibireme/yyjson/blob/95f4c61bc1e24176f2aa4f430902705a995f1c97/src/yyjson.h#L4878-L4894

Local sources and prior tranche anchors:

- PASS-2 schema and P2-A scope:
  `restart/prompts/skinny/PASS-2-RESEARCH.md:46` and
  `restart/prompts/skinny/PASS-2-RESEARCH.md:62`.
- SK-V11 hot-leaf attribution:
  `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:104`.
- Direct residual and guard rows:
  `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:147`.
- Current W0/RESULTS direct, tape, and sidecar authority:
  `skinny/RESULTS.md:3`, `skinny/RESULTS.md:95`, and
  `skinny/RESULTS.md:143`.
- Lock 1 and Lock 14:
  `restart/locks/LOCKS.md:52` and `restart/locks/LOCKS.md:78`.
- bbnf generated JSON parser/direct/tape sources:
  `skinny/crates/runtime/src/grammars/json/generated.rs:47`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:171`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:292`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:348`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:409`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:468`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:508`,
  `skinny/crates/runtime/src/tape/mod.rs:94`, and
  `skinny/crates/runtime/src/tape/assembler.rs:42`.
- bbnf current primitive sources:
  `skinny/crates/parse-that-regex/src/lib.rs:113`,
  `skinny/crates/parse-that-regex/src/lib.rs:162`,
  `skinny/crates/parse-that-regex/src/lib.rs:284`,
  `skinny/crates/parse-that-regex/src/lib.rs:302`,
  `skinny/crates/parse-that-regex/src/lib.rs:547`,
  `skinny/crates/parse-that-regex/src/lib.rs:718`,
  `skinny/crates/parse-that-regex/src/number/mod.rs:38`,
  `skinny/crates/parse-that-regex/src/number/mod.rs:106`, and
  `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`.
- REDRESS pre-block surface:
  `skinny/REDRESS.md:715`, `skinny/REDRESS.md:742`,
  `skinny/REDRESS.md:784`, `skinny/REDRESS.md:815`,
  `skinny/REDRESS.md:846`, `skinny/REDRESS.md:1380`,
  `skinny/REDRESS.md:1439`, `skinny/REDRESS.md:1490`,
  `skinny/REDRESS.md:1582`, `skinny/REDRESS.md:1637`,
  `skinny/REDRESS.md:2215`, `skinny/REDRESS.md:2508`,
  `skinny/REDRESS.md:2542`, `skinny/REDRESS.md:2795`,
  `skinny/REDRESS.md:2850`, `skinny/REDRESS.md:2910`,
  `skinny/REDRESS.md:3040`, `skinny/REDRESS.md:3150`,
  `skinny/REDRESS.md:3172`, and `skinny/REDRESS.md:3198`.
