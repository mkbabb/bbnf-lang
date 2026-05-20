# SK-V11 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-19.
Scope: strict comparator architecture teardown keyed to SK-V11 S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan, string_escape_decode,
unicode_escape_hex_decode, number_digit_span, ascii_whitespace_skip,
container_dispatch, simd_movemask, output_digest_hash.
Lock surface: Lock 1 + Lock 14.

V2 fold rule: comparator evidence is not admission. A retained P2-A candidate
must be anchored to one of the eight S-P1 hot leaves, must carry an executable
scalar reference, must name a same-wave direct/typed product consumer and
same-output proof shape, and must state its reject boundary. Parse-only rows are
diagnostic only. REDRESS 96/97/98/102 keep W3, retained structural class lanes,
structural-position vectors, streaming cursors, parser-owned projections, and
parse-only movement blocked.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

Strict direct/typed comparison remains the unresolved product surface:
`skinny/RESULTS.md:143` records `N-direct / NoGo`, `skinny/RESULTS.md:144`
names Track 1/Track 2, `skinny/RESULTS.md:146` says C++ sidecars are historical
or absent and never strict W0 anchors, and REDRESS 102 makes parse-only movement
proof-only and non-behavioral (`skinny/REDRESS.md:3040`).

| Comparator-derived idea | V2 class | Why it carries or does not carry |
|---|---|---|
| asmjson 64-byte `ByteState` masks and SWAR/AVX-512 classification [A1][A6] | Support-only | Useful mask vocabulary for `byte_class_mask64_transient`, but asmjson is not a strict comparator because its README allows bytes below `0x20` as whitespace and does not scan string contents for unescaped controls [A7]. x86 assembly is pressure only. |
| asmjson direct DOM/SAX writes from assembly [A4][A5] | Comparator pressure | It proves direct output planes can be produced without a separate DOM walk, but bbnf cannot import an assembly producer or a second retained substrate under Lock 1. |
| sonic-rs strict direct Serde/typed parse with no temporary tape [S1] | Comparator pressure | Strict `from_slice` is the closest SOTA direct-product comparator; lossy/unchecked modes are excluded [S6]. |
| sonic-rs key fast path, separator pair reads, raw-number visitor, AArch64 digit pack [S3][S7] | Candidate | These map to retained local pair-probe and digit-accumulation candidates only when the bbnf consumer is same-wave generated direct/typed output. Raw-key semantics must not change decoded-key equality. |
| sonic/simdjson string masks and quote/escape state [S2][S5][J4][J5] | Support-only | Quote-state math can validate transient masks. It is not a retained cursor, class lane, or independent row mover. PMULL quote-state stays pre-blocked as a default hot body. |
| simdjson stage1 structural index, DOM tape, string tape, and On Demand cursor [J1][J2][J7][J8] | Comparator pressure | Retained structural indexes, class columns, structural-position vectors, and streaming cursors are non-candidates. A transient mask may feed only the existing `TapeBuilder` or direct/typed sink in the same loop, and retained parse evidence remains diagnostic. |
| simdjson AArch64 byte classification tables and movemask packing [J6] | Support-only | Useful for a scalar-first byte-set classifier and checkasm parity. It does not admit an Arm body without the same-wave consumer proof. |
| yyjson portable parser shape: FSM/goto parser, whitespace helpers, unrolled ASCII string and number loops [Y1][Y4][Y5][Y6] | Support-only | V2 makes only the narrower sourced claim: the reviewed yyjson paths are portable C/FSM/unrolled-loop comparators. It makes no broader architecture claim. |
| yyjson 16-byte DOM values and arena layout [Y3][Y7] | Comparator pressure | Useful i-cache/value-pool pressure, but replacing bbnf's offset tape or direct `SinkOnly` plane would violate Lock 1. |

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Each parser packet below is S-P3-admissible only with the stated same-wave
proof. If the proof cannot be named in the implementation packet, the row
demotes to support-only or comparator pressure. C8 is carried as an output-plane
oracle/sink surface, not as parser vocabulary.

### C1 `byte_class_mask64_transient`

- **Class:** candidate.
- **P1 antecedents:** `ascii_whitespace_skip`, `bounded_plain_string_scan`,
  `container_dispatch`, `simd_movemask`.
- **Shape:** `fn(block, ByteClassSpec) -> ClassMasks64`, returning transient
  whitespace, quote, escape, structural/operator, and scalar-start masks. The
  result is consumed immediately; no retained class column, structural-position
  vector, or sidecar is allowed.
- **Scalar-reference sketch:** scalar loop over bytes `i = 0..n`; set bit `i`
  when `block[i]` belongs to the grammar-provided byte set. Quote/escape state
  is checked against the support-only scalar quote-parity oracle before any
  structural/scalar-start mask is trusted outside strings.
- **Same-wave consumer/proof:** same patch must wire the masks into a generated
  direct `SinkOnly` or generated typed product loop on an S-P1 residual row.
  Proof compares generated Track 1 output to independent Track 2 or an output
  oracle on identical strict inputs; SIMD bodies also require strict checkasm
  parity against the scalar loop. Existing retained `TapeBuilder` use may be a
  guard or micro-proof only, not an admission surface.
- **Output plane:** direct/typed product plane. Retained parse is diagnostic.
- **Feature/fallback:** scalar/SWAR is the default. AArch64 NEON TBL/TBX plus
  movemask may be gated only after scalar and checkasm pass. x86 AVX-512 remains
  comparator pressure.
- **Reject boundary:** reject on any retained class lane, structural-position
  buffer, streaming cursor, parser-owned projection, hidden sidecar, parse-only
  win, strict-parity failure, or missing direct/typed row movement.

### C2 `bounded_plain_string_end`

- **Class:** candidate.
- **P1 antecedents:** `bounded_plain_string_scan`, `string_escape_decode`,
  `simd_movemask`.
- **Shape:** `fn(bytes, start, limit, StringScanSpec) -> StringScanStop`,
  returning the first terminator, escape, control byte, or limit stop. UTF-8 and
  escape semantics remain caller policy; the primitive only finds the next byte
  that requires caller action.
- **Scalar-reference sketch:** byte-by-byte loop from `start` to `limit`;
  terminate on grammar-provided quote/terminator, escape byte, or control byte
  below the caller's control limit. The reference records the exact stop offset
  and stop kind.
- **Same-wave consumer/proof:** same patch must replace a generated direct or
  generated typed string/key scan in the hot string residual rows. Proof covers
  plain, escaped, control-byte, boundary, and UTF-8-valid/invalid strict cases
  and compares generated Track 1 product output against independent Track 2 or
  output oracle. A SIMD body requires strict checkasm for aligned, unaligned,
  short, boundary, and random cases.
- **Output plane:** direct/typed string product and digest sinks. Retained parse
  callers may run as guards only.
- **Feature/fallback:** scalar first; 16-byte SIMD may be proposed only with the
  caller proof. A 64-byte scan is research-only until it is materially different
  from the REDRESS 61/62/83/106 family.
- **Reject boundary:** reject 64-byte retained trusted scans, delayed-wide
  retained scans, generated-retained `StringBlock16` wrappers, NEON tiny-parser
  wiring, primitive-only microbench wins, or any claim that primitive parity
  implies production movement.

### C3 `whitespace_run_skip64`

- **Class:** candidate.
- **P1 antecedents:** `ascii_whitespace_skip`, `container_dispatch`.
- **Shape:** `fn(bytes, offset, WhitespaceSet) -> offset`, skipping only bytes
  in the grammar-declared whitespace set.
- **Scalar-reference sketch:** loop while `offset < len` and
  `WhitespaceSet::contains(bytes[offset])`; return the first non-whitespace
  offset. For JSON the set is the four RFC whitespace bytes; comment-aware CSS
  trivia is caller policy, not this primitive.
- **Same-wave consumer/proof:** same patch must wire the skip into generated
  direct or generated typed value/separator entry points. Proof compares Track 1
  generated output to an independent Track 2 or oracle and includes strict
  whitespace, no-whitespace, long-run, short-run, and terminator cases. SIMD
  variants require strict checkasm parity.
- **Output plane:** direct/typed product plane. Retained parse rows can only
  diagnose parity and regressions.
- **Feature/fallback:** scalar/SWAR default; AArch64 byte-class mask optional
  behind feature detection and scalar fallback.
- **Reject boundary:** reject comment-aware layout in the generic primitive,
  hidden JSON policy in generic crates, parse-only movement, or no product-row
  improvement on the declared same-wave consumer.

### C4 `separator_pair_probe16_direct`

- **Class:** candidate.
- **P1 antecedent:** `container_dispatch`.
- **Shape:** `fn(ptr, SeparatorSpec) -> PairAction`, using only the current
  pointer to classify local comma/value, colon/value, or comma/quote pairs in
  generated direct/typed loops.
- **Scalar-reference sketch:** endian-independent scalar read of at most two
  bytes, compare against generated per-grammar pair table, and return
  `Next(byte)`, `Done`, or `SlowPath`. The probe owns no persistent parser
  state.
- **Same-wave consumer/proof:** same patch must wire a local direct/typed
  object or array separator site and compare Track 1 generated output to
  independent Track 2 or oracle. The proof must include whitespace-present
  slow paths, EOF/boundary cases, object member separators, array separators,
  and decoded-key equality.
- **Output plane:** direct/typed product plane only.
- **Feature/fallback:** scalar unaligned 16-bit load or byte pair compare;
  no SIMD requirement.
- **Reject boundary:** reject object next-key carry, value-byte compaction,
  retained next-byte state beyond the local probe, JSON pair policy in generic
  crates, parse-only evidence, or any reopening of REDRESS 63/65/84 object
  carry variants.

### C5 `digit_block_accumulate_16`

- **Class:** candidate.
- **P1 antecedent:** `number_digit_span`.
- **Shape:** `fn(bytes, max_digits) -> DigitAccum { value_u64,
  digits_consumed, first_non_digit }`. It accelerates decimal digit
  accumulation only; JSON number grammar remains caller-owned.
- **Scalar-reference sketch:** loop up to `min(max_digits, 16)` while
  `b'0' <= byte <= b'9'`, updating `value = value * 10 + digit`; stop on first
  non-digit. Caller validates leading zero, fraction, exponent, sign, range,
  and conversion policy.
- **Same-wave consumer/proof:** same patch must wire a generated direct or typed
  numeric consumer on the `canada`/`mesh`-style number rows and compare final
  product output against independent Track 2 or oracle. Proof includes
  0/1/15/16/17+ digit spans, overflow boundaries, fraction/exponent handoff,
  and strict invalid-number cases. AArch64 bodies require strict checkasm
  parity against the scalar accumulator.
- **Output plane:** direct/typed numeric product. Retained parse is diagnostic.
- **Feature/fallback:** scalar/SWAR default; AArch64 NEON unzip/multiply-add or
  UDOT-shaped body only behind feature detection and scalar fallback.
- **Reject boundary:** reject mantissa/table-only routes, f64 fallback rewrites,
  primitive-owned JSON policy, row movement limited to parse-only, or any
  repeat of the REDRESS 80 zero-fallback-pool failure.

### C8 `output_digest_hash_oracle`

- **Class:** non-parser output-plane surface.
- **P1 antecedent:** `output_digest_hash`.
- **Shape:** `fn(hash_state, bytes_or_decoded_segment) -> hash_state`, folding
  raw borrowed string bytes and decoded escape segments into the existing public
  digest semantics.
- **Scalar-reference sketch:** bit-exact mirror of the current scalar
  `fold_string_scalar`, `hash_bytes`, and `mix` behavior
  (`skinny/crates/bbnf-bench/src/direct_struct.rs:123`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:717`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:737`). Tests compare hash
  state after every raw and decoded segment boundary.
- **Same-wave consumer/proof:** same patch must wire a generated direct/typed
  product output sink that already computes the digest. Proof compares generated
  Track 1 product output to independent Track 2 or an output oracle that
  recomputes digest from decoded values without calling Track 1 or reading a
  hidden sidecar.
- **Output plane:** benchmark/product output sink only. This is not parser
  substrate, not generic parser semantics, and not a retained fact table.
- **Feature/fallback:** portable scalar default. AArch64 hash acceleration is
  eligible only if profiling shows digest work is the limiting direct/typed hot
  leaf and strict parity passes.
- **Reject boundary:** reject if profiling does not isolate digest as limiting,
  if no direct/typed product row moves, if cache hints/prefetch alone are the
  change, if semantic string facts are retained for hashing, or if digest logic
  enters generic parser crates.

### §2.1 — Support-only primitives and proof surfaces

Support-only items can be used to prove candidate packets, but they do not move
to S-P3 as standalone candidates without a same-wave product consumer.

| Support item | Allowed use | Boundary |
|---|---|---|
| `quote_escape_state64` | Scalar prefix/parity oracle for C1/C2 masks using JSON escape-parity law or a generated per-grammar quote-state policy. | No retained in-string vector, no streaming cursor, no PMULL default hot body, no standalone row claim. |
| `hex_escape_quad_decode` | Single-quartet scalar oracle and optional x4 proof/checkasm surface for fixed-width hex nibble decode. | `uXXXX` production remains blocked. Reusing the already-consuming `unescape_string` caller is REDRESS 107/108 paper-close unless a new escaped-segment source delta and direct/typed consumer are named. |
| `movemask_pack64` | Bit-pack oracle for C1/C2/C3 SIMD bodies. | No standalone candidate and no class-column storage. |
| `live_mask_same_loop_consume` | Accounting rule for consuming transient masks into generated direct/typed output or the existing `TapeBuilder` in the same loop. | Not a substrate candidate. No retained structural index, structural-position vector, parser-owned projection, sidecar, or parse-only admission. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Generic API must remain | Generated per grammar | Forbidden coupling |
|---|---|---|---|
| C1 `byte_class_mask64_transient` | Byte-set mask API over caller-provided sets. | Whitespace, quote, escape, structural/operator, and scalar-start sets. | JSON-named classes in generic crates, retained class columns, or generated sidecars. |
| C2 `bounded_plain_string_end` | Stop-byte scan over terminator/escape/control metadata. | String delimiters, escape policy, control limit, UTF-8 mode, and decode caller. | JSON string semantics in `bbnf-simd` or `parse-that-regex`; retained wide string facts. |
| C3 `whitespace_run_skip64` | Byte-set skip only. | Per-grammar whitespace bytes and caller-level trivia policy. | CSS comments or JSON whitespace policy baked into the primitive. |
| C4 `separator_pair_probe16_direct` | Local pair compare over generated pair table. | FIRST/follow pair tables at direct/typed separator sites. | Persistent next-byte carry, object/key/value-byte compaction, or JSON pair constants in generic code. |
| C5 `digit_block_accumulate_16` | Decimal digit accumulation only. | Number grammar, range policy, fraction/exponent/sign policy, and final conversion. | Primitive-owned JSON number validation or f64 fallback policy. |
| C8 `output_digest_hash_oracle` | Host/output sink fold over bytes or decoded segments. | Benchmark/product digest semantics and output proof. | Parser substrate semantics, generic parser hash facts, or hidden side tables. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **Parse-only remains diagnostic.** Candidate admission must be direct/typed
  product-plane evidence or a non-JSON generated direct/typed parser
  intervention. Retained parse may only prove correctness or guard regressions.
- **W3 remains blocked.** No retained class column, structural-position vector,
  streaming cursor, parser-owned projection, structural sidecar, or retained
  structural index is allowed, even if described as a tape projection. The only
  retained projection is the existing offset tape itself under Lock 1.
- **String widening is pre-blocked.** REDRESS 61/62/83/106 reject retained
  wide-string scans, delayed-wide trusted scans, generated-retained
  `StringBlock16`, and primitive-parity-only production claims.
- **x4 Unicode escape production is blocked.** Hex decode can be proof/support
  only until a new source delta and same-wave direct/typed consumer escape
  REDRESS 107/108.
- **Container carry is narrow.** Local scalar pair probes are allowed only at
  direct/typed current-pointer sites. Object next-key carry, value-byte
  compaction, and retained carry state are rejected.
- **Numeric fallback stays closed.** Digit accumulation cannot become a
  mantissa-table, f64 fallback, or parser-policy rewrite.
- **Digest/hash stays output-plane only.** It is a benchmark/product sink
  candidate only if the scalar digest source and direct/typed consumer are both
  named; cache placement or prefetch inventory does not admit it.

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
- bbnf scalar digest sources:
  `skinny/crates/bbnf-bench/src/direct_struct.rs:123`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:717`, and
  `skinny/crates/bbnf-bench/src/direct_struct.rs:737`.
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
