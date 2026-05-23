# SK-V14 P2-E: parse-that Primitive Gaps

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-23.
Scope: parse-that's primitive vocabulary — which SIMD / string / float / regex primitives the S-P1 hot leaves demand that parse-that does not yet expose, per gap the missing primitive's shape + scalar-reference sketch + Layer-0/Layer-1 placement in the bbnf-simd two-layer vocabulary.
Output: this file.
P1 hot-leaf antecedents: `match_tiny_plain_string_with_cap` (distinct_values parse-only 96.3%), `parse_that_regex::skip_string_plain_trusted` (envelope-masked per F-V2-P1ABC-RERECORD), `read_hex_unit_scalar` (y_string_unicode parse-only 100%), `validate_string_escape` (escape-validation prelude), `hex_nibble` (nibble decode helper), `skip_ascii_whitespace` (JSON value-position prelude), `number::scan_digit_run` (float-heavy mesh/canada/numbers, mode-III SIMD ratios 4.96x–5.04x), `bbnf_simd::movemask_u8x16` (backbone of every aarch64 NEON probe). Per dispatch context §1 + P1-E §4.1, 13/17 parse-only + 14/17 direct rank-1 envelopes fold these primitives behind `dispatch_value` / `parse_object_value_at_direct` / `parse_array_element_at_direct`; F-V2-P1ABC-RERECORD with `runtime/parse-attribution` cracks the envelope. This artefact designs against the inner-leaf list under the envelope, not the envelope itself, per P1-E §4.1 CH2 reading.
Lock surface: Lock 1 (substrate union — every plain-string scan that emits a tape position must not propose a parallel substrate); Lock 14 (grammar-neutrality — string / number / unicode primitives are not JSON property of parse-that; CSS L4, Sheets, BBNF-self all demand the same quoted/numeric/escape vocabulary). Lock 16 binds the scalar-reference-first admission discipline (every gap entry below carries a scalar reference sketch).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### §1.1 — parse-that-regex primitive surface at HEAD (Layer-1 inventory)

The crate exposes one module per primitive family — string at `skinny/crates/parse-that-regex/src/lib.rs`, number at `skinny/crates/parse-that-regex/src/number/mod.rs`, unicode at `skinny/crates/parse-that-regex/src/unicode/mod.rs`, integration at `skinny/crates/parse-that-regex/src/integration/mod.rs`. The crate's `Cargo.toml` records name `parse-that-regex` (per `skinny/crates/parse-that-regex/Cargo.toml:1`); workspace dependency entry at `skinny/Cargo.toml:31`. The string surface enumerates:

| family | function | file:line | shape | bbnf-simd consumer |
|---|---|---|---|---|
| string-scan SWAR | `skip_ascii_whitespace` | `lib.rs:113` | scalar driver; calls `skip_ascii_spaces` | none — 8-byte SWAR inline |
| string-scan SWAR | `skip_ascii_spaces` | `lib.rs:128` | 8-byte block with `zero_byte_mask` | none (inline SWAR) |
| string-scan SWAR | `skip_string_plain` | `lib.rs:462` | NEON 16-byte via `bbnf_simd::aarch64::string_block::scan_string_special_block` THEN 8-byte SWAR fallback via `string_special_mask` | `bbnf_simd::aarch64::string_block` (16-byte path); inline SWAR (8-byte path) |
| string-scan SWAR | `skip_string_plain_trusted` | `lib.rs:547` | same dual path; UTF-8 trusted | same |
| string-scan helper | `string_special_mask` | `lib.rs:577` | 8-byte SWAR test for `"`, `\`, control | inline; no Layer-1 hook |
| string-scan helper | `string_escape_control_mask` | `lib.rs:832` | 8-byte SWAR for `\` + control (no quote) | inline; no Layer-1 hook |
| string-scan helper | `find_next_escape_or_control` | `lib.rs:813` | 8-byte SWAR loop over `string_escape_control_mask` | inline; no Layer-1 hook |
| string match | `match_string` / `match_string_at_quote` | `lib.rs:212`, `lib.rs:228` | classifier driver | `bbnf_simd::aarch64::string_block` (transitive) |
| string match (trusted) | `match_string_at_quote_trusted_utf8` | `lib.rs:162` | trusted-UTF-8 fast lane | same |
| escape validate | `validate_string_escape` | `lib.rs:284` | 1-byte switch over `\X` | none |
| escape validate | `validate_unicode_escape_run` | `lib.rs:347` | hex-run validator | none — scalar chain |
| escape decode | `decode_unicode_escape` | `lib.rs:302` | full `\uXXXX` decode + surrogate join | calls `read_hex_unit_scalar` |
| escape decode | `unescape_string` | `lib.rs:718` | full canonicalization; aarch64 batch via `unescape_four_unicode_escapes` | `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon` (aarch64 only) |
| string content classify | `classify_string_content` | `lib.rs:634` | NEON dispatch + scalar reference | `bbnf_simd::aarch64::movemask_u8x16` (private re-impl at `lib.rs:698`) |
| hex helpers | `read_hex_unit_scalar` | `lib.rs:945` | 4-nibble decode | none |
| hex helpers | `hex_nibble` | `lib.rs:959` | 1-byte ASCII→nibble switch | none |
| UTF-8 validate | `validate_utf8_codepoint` | `lib.rs:843` | full per-codepoint validator with Hoehrmann-style range checks | none — full scalar |
| UTF-8 validate | `validate_utf8_prefix` | `lib.rs:602` | call-`validate_utf8_codepoint` driver | none |
| UTF-8 helpers | `is_utf8_continuation`, `is_high_surrogate`, `is_low_surrogate` | `lib.rs:914`, `lib.rs:969`, `lib.rs:974` | range tests | none |
| number scan | `match_number_span` / `match_number_span_from_first` | `number/mod.rs:32`, `number/mod.rs:38` | structural number-shape driver | none — calls `scan_digit_run` |
| number scan | `scan_digit_run` | `number/mod.rs:106` | 8 / 4 / 2 / 1 byte SWAR ladder | inline; no Layer-1 hook |
| number scan helper | `is_eight_ascii_digits`, `is_four_ascii_digits`, `is_two_ascii_digits` | `number/mod.rs:187`, `number/mod.rs:176`, `number/mod.rs:165` | SWAR range test | inline; no Layer-1 hook |
| number SWAR | `parse_eight_digits`, `parse_four_digits`, `parse_two_digits` | `number/mod.rs:214`, `number/mod.rs:207`, `number/mod.rs:198` | SWAR digit-pack via `digits * 10` shift fold | inline; no Layer-1 hook |
| number materialize | `materialize_f64` | `number/mod.rs:261` | Eisel-Lemire on span.mantissa + decimal_exp; `text.parse::<f64>` fallback | inline |
| number materialize | `materialize_i64` / `materialize_u64` | `number/mod.rs:226`, `number/mod.rs:247` | `i64::try_from(mantissa)` fast path; `integer::parse_i64` overflow path | none |
| number Eisel-Lemire | `eisel_lemire::compute_f64` | `number/eisel_lemire/algorithm.rs` (via mod) | branchless mantissa × 10^exp table | none |
| integration | `simd_scan_hook` | `integration/simd_scan_hook.rs` | hook surface to `bbnf-simd` consumer | bbnf-simd |
| utility | `zero_byte_mask` | `lib.rs:630` | classic SWAR `block.wrapping_sub(ones) & !block & high_bits` | inline — appears in 3 sites |

### §1.2 — bbnf-simd Layer-0 / Layer-1 vocabulary at HEAD

Per §8 PASS-2-RESEARCH item 3 + `[general-infra-crates]`, bbnf-simd carries a Layer-0 vendored substrate (the `core::arch::aarch64::*` intrinsics + `core::arch::asm` UDOT/SDOT for `dotprod`; the x86_64 isa-feature shells at `skinny/crates/bbnf-simd/src/x86_64/`) and a Layer-1 bbnf primitive vocabulary. The Layer-1 inventory at HEAD:

| primitive | aarch64 file:line | scalar reference file:line | scope |
|---|---|---|---|
| `byte_class_from_eq_set_64` | `aarch64/byte_class_from_eq_set_64.rs:1` | `scalar/byte_class_from_eq_set_64.rs:1` | 64-byte block, ≤8-element ASCII set membership mask |
| `byte_class_from_table_64` | `aarch64/byte_class_from_table_64.rs:1` (currently scalar wrap) | `scalar/byte_class_from_table_64.rs:1` | 64-byte block, 256-bit alphabet table → 64-bit mask |
| `classify_tbl4::classify_chunk_from_table` | `aarch64/classify_tbl4.rs:22` | none (Layer-1 only) | 16-byte vqtbl4q_u8 structural+quote+escape+control multi-class |
| `classify_tbl4::classify_block_from_table` | `aarch64/classify_tbl4.rs:47` | none | 64-byte / 4-chunk wrap |
| `classify_tbl4::classify_structural_terminator_*` | `aarch64/classify_tbl4.rs:75`, `:89` | none | 2-class (structural + terminator) wrap |
| `string_block::scan_string_special_block` | `aarch64/string_block.rs:57` | `aarch64/string_block.rs:31` (`_scalar`) | 16-byte 4-mask (`"`, `\`, control, non-ASCII) probe |
| `match_tiny_plain_string_neon` | `aarch64/match_tiny_plain_string.rs:81` | `aarch64/match_tiny_plain_string.rs:38` (`_scalar`) | 16-byte vqtbl4q_u8 alphabet-member mask + first-set index |
| `unescape_uxxxx_neon` | `aarch64/unescape_uxxxx.rs:74` | `aarch64/unescape_uxxxx.rs:40` (`_scalar`) | single `\uXXXX` quartet via vqtbl1q_u8 nibble LUT |
| `unescape_uxxxx_x4_neon` | `aarch64/unescape_uxxxx.rs:125` | none directly (composed from scalar) | four `\uXXXX` quartets per register |
| `join_surrogates` / `join_surrogate_pair_neon` | `aarch64/unescape_uxxxx.rs:54`, `:169` | same (scalar function) | surrogate-pair codepoint join |
| `digit_mac::parse_4_digits_dotprod` | `aarch64/digit_mac.rs:27` | `aarch64/digit_mac.rs:5` (scalar branch) | UDOT-based 4-digit pack (dotprod target_feature) |
| `digit_mac::dot4_i8` | `aarch64/digit_mac.rs:53` | none — Layer-1 only | SDOT 4-byte dot product (generic) |
| `utf8::validate_block` | `aarch64/utf8/validate_block.rs:91` (intrinsic body) | `aarch64/utf8/validate_block.rs:35` (`ValidateStatus`) + scalar driver | 16-byte UTF-8 prefix validator with continuation-state report |
| `movemask_u8x16` | `aarch64/movemask.rs:4` | n/a — pure NEON shape | `vshrn_n_u16::<4>`-fused 16-lane → 16-bit movemask |
| `bitmap_prefix_xor_64` | `aarch64/bitmap_prefix_xor_64.rs` | `scalar/bitmap_prefix_xor_64.rs:1` | 64-bit XOR-scan (for quoted-string carry) |
| `bitmap_next_set_bit` | `aarch64/bitmap_next_set_bit.rs` | `scalar/bitmap_next_set_bit.rs:1` | first-set-≥-cursor (RBIT+CLZ shape) |
| `bulk_emit_positions_64` | `aarch64/bulk_emit_positions_64.rs:2` | `scalar/bulk_emit_positions_64.rs:1` | tape-position bulk writer |
| `eob_pad_clamp` | `aarch64/eob_pad_clamp.rs:1` | `scalar/eob_pad_clamp.rs:1` | end-of-buffer 64-byte pad |

The Layer-1 contract: every Layer-1 primitive carries a scalar reference function bit-identical to the NEON body per Lock 16 + `[no-warm-benches]` + the dav1d checkasm discipline (P2-B). The contract is monotonic — when a Layer-1 primitive is added, both the scalar and the NEON body land in the same commit, and `tests/checkasm_parity.rs` covers it.

### §1.3 — Lock-1 substrate-union constraint for any new primitive

Every gap below MUST emit, if it carries a position-returning interface, into the SAME tape that `bulk_emit_positions_64` feeds (per `skinny/crates/bbnf-simd/src/lib.rs:227` `compact_mask`). A new primitive that proposes a separate position-table, a parser-local cursor, or a sidecar event vector violates Lock 1 (substrate-union); per P2-D and CH5, such a candidate is REJECTed. This artefact's gap list is constructed to be substrate-union-compatible: every entry below either (a) does not emit positions (string-scan tail length, digit-run length, escape index relative to slash) or (b) returns a bitmask that the existing `compact_mask` consumer can fold into the shared tape.

### §1.4 — REDRESS pre-block surface relevant to parse-that gaps

Per CH3 binding + dispatch context §0, the routes pre-blocked at HEAD that this artefact must NOT reopen:

- **REDRESS 28 + 33** — Class A NEON tiny-string `match_tiny_plain_string` wiring at the bench layer. The kernel exists at `aarch64/match_tiny_plain_string.rs:81` (admitted Wave 1 body); REDRESS 28/33 closed the global cap-16 policy. P2-E gap 1 below is the unwired alphabet-driven tiny-plain-string fast lane into the parse-that-regex string-content prefilter — a wiring at the parse-that consumer layer, not a re-issue of the global cap policy.
- **REDRESS 50–55** — UTF-8 fusion routes (sink-local decoded stats, source-method digest folds, quote-source streaming hash). Gap 4 below proposes a multi-codepoint UTF-8 prefix validator and is explicitly NOT a fused materialization route — it returns codepoint-count + valid-byte-prefix, not decoded characters.
- **REDRESS 60–72** — Retained-parse + sidecar producers + cap-16 routes; gap 1 wiring discipline avoids reopening these.
- **REDRESS 80** — canada mantissa-widen plan; gap 5 (Eisel-Lemire mantissa-widen window) carries a fresh material differential per P2-C ARM SVE / scalar 128-bit-mul route, but at REDRESS 80 the route was rejected on overfit grounds; per dispatch context §0 the REDRESS pre-block stands and gap 5 is flagged with the REDRESS 80 differential expectation — S-P3, not S-P2, decides whether the differential is sufficient.
- **REDRESS 82–84** — single-quartet unicode classifier (REDRESS 82), StringBlock16 tiny probe (REDRESS 83), object-pair compaction (REDRESS 84). Gap 2 below (unescape eight-quartet path) is a DOUBLING of `unescape_uxxxx_x4_neon`, not a re-issue of REDRESS 82's single-quartet classifier route.
- **REDRESS 88** — PMULL prefix-XOR as the hot body (vs. the existing scalar `bitmap_prefix_xor_64`). aarch64 — P2-C's primary architecture, but PMULL routing is P2-C scope; P2-E does NOT propose a PMULL primitive.
- **REDRESS 89** — CSSC CTZ next-bit bulk consumer. Same posture as REDRESS 88 — P2-C scope; P2-E does NOT propose a CSSC bulk-extractor.
- **REDRESS 96 / 97 / 98** — Union-substrate / class-column substrate routes. Gap 6 (multi-class structural+quote+escape+control prefix-XOR fold) explicitly composes with `classify_tbl4::classify_block_from_table` at `aarch64/classify_tbl4.rs:47` (already in the substrate-union scheme), NOT a new column substrate.

Per `[no-workarounds]` + `[no-orthogonal-codepaths]`, every gap below extends the existing Layer-1 vocabulary; no gap proposes an orthogonal subsystem.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Eight gaps surfaced. Each carries: (1) the missing-primitive name + concrete shape; (2) the scalar reference sketch (paste-ready function bodies kept brief — full bodies under S-P3 admission, but the algorithmic kernel is named); (3) Layer-0 substrate vs Layer-1 primitive placement; (4) the S-P1 hot-leaf antecedent; (5) the parse-that-regex consumer site that today calls inline SWAR or scalar and would call the new Layer-1 primitive after admission.

### Gap 1 — `bbnf_simd::aarch64::string_special_block_sweep` (64-byte chunked string-tail sweep)

**Shape.** Drop-in replacement for the SWAR-8 inner loops in `parse-that-regex/src/lib.rs:510-530` (`skip_string_plain` 8-byte path) + `parse-that-regex/src/lib.rs:565-572` (`skip_string_plain_trusted` 8-byte path) + `parse-that-regex/src/lib.rs:814-820` (`find_next_escape_or_control` 8-byte path) when the host carries aarch64 but the 16-byte `scan_string_special_block` is being called in a tight loop — the gap is a `_sweep` driver that takes a `*const u8 + len` and returns the offset of the first interesting byte (terminator | escape | control | non-ASCII per mode) plus a `StringSpecialBlock`-compatible flag set, processed 64 bytes at a time across 4 NEON registers (one `classify_tbl4`-style probe per chunk; mask-fold into a single `u64`). Returns `(offset, StringSpecialBlock-equivalent)` — caller advances cursor or branches into the slow lane.

**Scalar reference.** A 64-byte tight loop calling `scan_string_special_block_scalar` (`bbnf-simd/src/aarch64/string_block.rs:31`) four times and OR-folding the masks into a `u64`; first-interesting byte computed by `trailing_zeros` on the OR-fold. Bit-identical to four sequential 16-byte calls.

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::string_block::scan_string_special_block_sweep_64`. Uses Layer-0 `vld1q_u8` × 4 + `vceqq_u8` / `vcltq_u8` / `vcgeq_u8` + the existing Layer-1 `movemask_u8x16` (per `bbnf-simd/src/aarch64/movemask.rs:4`). x86_64: Layer-1 equivalent dispatches to AVX-512 `vpternlogq` 64-byte compare + `kmovd` lane-mask compose at `x86_64/avx512_kmask/`; scalar fallback uses the 8-byte SWAR `string_special_mask` body verbatim from `parse-that-regex/src/lib.rs:577-587`.

**P1 antecedent.** `parse_that_regex::skip_string_plain_trusted` (heavily inlined; per dispatch context §1 envelope-masked by `dispatch_value` — F-V2-P1ABC-RERECORD re-records this site under `runtime/parse-attribution`). Surfaces in 14/17 corpora once envelope is cracked; today only the 16-byte NEON path goes via bbnf-simd (per `lib.rs:469`), the 8-byte SWAR continuation stays inline.

**Consumer.** `parse-that-regex/src/lib.rs:469-507` collapse to a single `_sweep` call; the inline SWAR path at `:510-530` deletes; `find_next_escape_or_control` calls the same `_sweep` with a 2-class mask (escape + control, no terminator). One Layer-1 primitive replaces three open-coded SWAR loops.

**Substrate-union (Lock 1).** Returns offset + flags; emits no positions; no parallel substrate. The caller's tape emit at `runtime/src/grammars/json/scan.rs` is unchanged.

### Gap 2 — `bbnf_simd::aarch64::unescape_uxxxx_x8_neon` (eight-quartet hex decode)

**Shape.** Double-wide extension of `unescape_uxxxx_x4_neon` (`bbnf-simd/src/aarch64/unescape_uxxxx.rs:125`). Takes `&[u8; 32]` of packed quartets; decodes 8 codepoints in parallel via two `vqtbl1q_u8` over the existing 16-entry nibble LUT (`HEX_NIBBLE_LUT` at `unescape_uxxxx.rs:201`) with one `vminvq_u8` per half for poison detection. Returns `Option<[u32; 8]>`.

**Scalar reference.** Loop calling `unescape_uxxxx_scalar` (`unescape_uxxxx.rs:40`) 8 times into a `[u32; 8]`; `None` if any returns `None`. Bit-identical.

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::unescape_uxxxx`. Uses Layer-0 `vld1q_u8` × 2 + the existing Layer-1 `HEX_NIBBLE_LUT`; reuses the digit/upper/lower range masks; `vminvq_u8` is Layer-0. Generalizes to AVX-512 `vpermi2b` over a 32-entry nibble LUT.

**P1 antecedent.** `read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:945`) at 100% self-time on y_string_unicode parse-only (per P1-E §2.1). The aarch64 `_x4` body is admitted but only fires on runs of exactly 4 consecutive `\uXXXX` (per `lib.rs:387`). Densely-unicode-escape-heavy corpora (Confucian Analects, CJK literature, mathematical Unicode dumps — none in the 17 SK-V14 corpora but absent-corpus generalization per dispatch context §1 CSS L4 spec posture) carry runs of 8 or more.

**Consumer.** `parse-that-regex/src/lib.rs:386` (the `_x4` driver) extends to an `_x8` first-pass, falling back to `_x4` then scalar. `unescape_string` at `lib.rs:775-783` adds the doubling branch.

**Substrate-union (Lock 1).** Returns `[u32; 8]` (codepoints); no position emit; no substrate touch.

**REDRESS 82–84 distinction.** REDRESS 82 was a single-quartet *classifier* (NEON range-test only, no decode); REDRESS 83 was a `StringBlock16` tiny probe at the bench layer; REDRESS 84 was object-pair compaction. Gap 2 is a doubling of the admitted `_x4` decoder body — a Layer-1 primitive widening, not a route family reopen.

### Gap 3 — `bbnf_simd::aarch64::ascii_whitespace_skip_64` (64-byte whitespace sweep)

**Shape.** Drop-in replacement for `parse-that-regex/src/lib.rs:128-147` (`skip_ascii_spaces` 8-byte SWAR). Takes `*const u8 + len`; returns the offset of the first non-`{0x20, 0x09, 0x0a, 0x0d}` byte (the JSON whitespace set per RFC 8259), processed 64 bytes at a time. Uses the existing `byte_class_from_eq_set_64` (`bbnf-simd/src/lib.rs:282`) — the set `[0x20, 0x09, 0x0a, 0x0d]` has size 4 ≤ 8, satisfying the kernel's contract. Returns offset of the first NON-member by inverting the returned mask: `(!mask).trailing_zeros() as usize`.

**Scalar reference.** Same `byte_class_from_eq_set_64_scalar` (`bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1`) with the 4-byte set; the bbnf-simd scalar reference IS the scalar reference for the new primitive. Wire as a thin Layer-1 wrapper.

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::string_block::ascii_whitespace_skip_64` (a sibling of `scan_string_special_block_sweep_64` gap 1; new sub-module if either gap lands first). x86_64 / scalar: drives through the existing `byte_class_from_eq_set_64` dispatch table at `bbnf-simd/src/lib.rs:209`.

**P1 antecedent.** `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:113`) is called at every JSON value position (object-value-position, array-element-position, top-level scan); per P1-E §1.3 the JSON value-position prelude routes through `dispatch_value` which inlines `skip_ascii_whitespace` per the cfg_attr at `runtime/src/grammars/json/generated.rs:43-44`. Envelope-masked at SK-V14 dispatch; F-V2-P1ABC-RERECORD will surface this site.

**Consumer.** `parse-that-regex/src/lib.rs:113-125` collapses to one Layer-1 call. The grammar-agnostic byte-set parameterization means CSS L4 whitespace `[0x20, 0x09, 0x0a, 0x0d, 0x0c]` (5-byte set, ≤8) and Sheets whitespace (varies) reuse the same primitive — Lock 14 generalization is automatic.

**Substrate-union (Lock 1).** Returns offset; no positions emitted.

### Gap 4 — `bbnf_simd::aarch64::utf8::validate_block_streaming` (multi-block UTF-8 with continuation state)

**Shape.** Streaming-mode extension of `validate_block` at `bbnf-simd/src/aarch64/utf8/validate_block.rs`. Takes `*const u8 + len + &mut ContinuationState`; processes the entire buffer 16 bytes at a time, carrying `ValidateStatus::continues + complete_bytes` state across chunks. Returns the byte offset of the first invalid byte or `len`. The existing `validate_block` returns one chunk's worth of status; the caller at `parse-that-regex/src/lib.rs:496-503` then has to compose chunk-state manually with `validate_utf8_codepoint` per-cursor in a hand-loop.

**Scalar reference.** `parse-that-regex/src/lib.rs:843` (`validate_utf8_codepoint`) iterated to end-of-buffer with the explicit per-codepoint width dispatch from `:843-911`. Bit-identical to the Hoehrmann-style state machine — the scalar function IS the reference.

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::utf8::validate_block_streaming` (new sibling of `validate_block`). Uses Layer-0 `vqtbl1q_u8` for the Lemire/Keiser shape (per cite in `validate_block.rs:1` comment) and the existing Layer-1 `movemask_u8x16`. Composes with `validate_block`'s `ValidateStatus` per-chunk return.

**P1 antecedent.** Composite: `parse_that_regex::skip_string_plain` UTF-8 path at `lib.rs:489-505` (envelope-masked behind `dispatch_value`) + `validate_utf8_codepoint` at `lib.rs:843` (called inside `validate_utf8_prefix` at `lib.rs:602`). Per P1-E §2.2, `parse_that_regex::unescape_string` surfaces as a cleanly-attributed primitive on unicode_escapes/direct_to_struct at 46.7%; the streaming UTF-8 path is the inverse problem (validate without decode), same shape.

**Consumer.** `parse-that-regex/src/lib.rs:489-505` (the NEON UTF-8 block + manual `complete_bytes` carry) collapses to one streaming call. The grammar-agnostic byte-stream interface means CSS L4 declaration-values, Sheets text strings, BBNF-self comment text all reuse the same primitive — pure Lock 14 generalization (UTF-8 validation belongs in NO grammar).

**Substrate-union (Lock 1).** Returns offset; emits no positions.

**REDRESS 50–55 distinction.** REDRESS 50–55 closed sink-local decoded-stats / source-method digest routes — i.e. FUSED materialization. Gap 4 is the inverse: validate-only, no decode, no sink. Material differential.

### Gap 5 — `bbnf_simd::aarch64::digit_mac::parse_16_digits_dotprod` (16-digit UDOT pack)

**Shape.** Extension of `parse_4_digits_dotprod` (`bbnf-simd/src/aarch64/digit_mac.rs:27`) to 16 digits per UDOT cycle. Takes `&[u8; 16]` of ASCII digits, subtracts `b'0'` lane-wise, multiplies by weights `[10^15, 10^14, …, 10^0]` (constrained to fit in u32 lanes via two-stage fold), returns `u64` mantissa for the 16-digit run. The current `parse_4_digits_dotprod` is a 4-digit body that wastes 12 of 16 NEON lanes per UDOT.

**Scalar reference.** `parse_eight_digits` (`parse-that-regex/src/number/mod.rs:214`) called twice with a `* 10_000_000_000` multiply between; or the simpler hand-loop `acc = acc * 10 + (b - b'0')`. Bit-identical at the result level; the scalar reference need not preserve UDOT lane order.

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::digit_mac::parse_16_digits_dotprod`. Uses Layer-0 `core::arch::asm` UDOT (existing posture at `digit_mac.rs:39-46`) + `vmulq_u32` for the upper-lane × 10^8 fold. `target_feature(enable = "dotprod")` per existing posture. x86_64: AVX-512 `vpdpwssd` (VNNI) equivalent at `x86_64/avx512_vnni/`.

**P1 antecedent.** `number::scan_digit_run` (`parse-that-regex/src/number/mod.rs:106`) — the 8-byte SWAR `parse_eight_digits` is the fastest available scalar; for float-heavy corpora (`mesh`, `canada`, `numbers` per P1-E §2.4 mode-III SIMD ratios 5.04x / 5.01x / 4.96x), digit runs of 10–17 digits dominate, and `parse_eight_digits` followed by `parse_four_digits` followed by `parse_two_digits` is 3 SWAR cycles for a 14-digit mantissa. A 16-digit UDOT collapses this to 1 vector cycle.

**Consumer.** `parse-that-regex/src/number/mod.rs:113-123` (the 8-byte SWAR branch) extends to a 16-byte UDOT first-pass at `:113` when the `dotprod` target_feature is available; the 8/4/2/1 SWAR ladder remains for the tail. `parts.can_push_eight_digits()` extends to `can_push_sixteen_digits()` with mantissa-fit check at `digit_count <= 3`.

**Substrate-union (Lock 1).** Returns u64 mantissa; no positions; no substrate touch.

**REDRESS 80 distinction.** REDRESS 80 was a canada-specific mantissa-widen plan with a per-corpus float-overfit posture. Gap 5 is grammar-neutral: every grammar with numeric literals (JSON, CSS L4 `<number>` per CSS Values L4 §4.1, Sheets formula numerals, BBNF-self regex `{n,m}` counts) uses the same digit-run primitive. The material differential: a Layer-1 primitive, not a hot-path corpus tweak. **Flagged for S-P3 decision** per dispatch context §0; S-P2 does not bypass REDRESS 80, only records the differential.

### Gap 6 — `bbnf_simd::aarch64::string_block::scan_string_with_carry_64` (in-string state with backslash-escape carry)

**Shape.** Composition of `string_block::scan_string_special_block_sweep_64` (gap 1) with the simdjson-style backslash-carry shape: 64-byte block returns the cumulative `quote_mask` (with backslash-escape carry already applied — the `prefix_xor_64` fold), bypassing the per-block branch in the existing `bitmap_prefix_xor_64` + manual carry compose in `parse-that-regex/src/lib.rs:170-206` (well: this is `bbnf-simd/src/lib.rs:175` `escape_mask_64` actually). The current `bbnf_simd::escape_mask_64` at `lib.rs:175` returns the escape mask; the caller has to drive the prefix-XOR separately.

**Scalar reference.** Compose `scan_string_special_block_sweep_64` (gap 1's scalar) with `bitmap_prefix_xor_64_scalar` (`scalar/bitmap_prefix_xor_64.rs:1`) and the `escape_mask_64` body at `bbnf-simd/src/lib.rs:175-206` for the even/odd backslash carry. Bit-identical to the simdjson `prev_in_string` carry shape (cite: simdjson 3.x `find_quote_mask_and_bits` in `include/simdjson/arm64/simd.h`).

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::string_block::scan_string_with_carry_64`. Composes existing Layer-1 primitives (`classify_tbl4::classify_block_from_table` from `aarch64/classify_tbl4.rs:47` + `bitmap_prefix_xor_64` + `escape_mask_64` body promoted from `lib.rs:175` into the module). Pure Layer-1 composition — no Layer-0 substrate add.

**P1 antecedent.** `parse_that_regex::skip_string_plain_trusted` (envelope-masked) + the `scan_structurals` consumer at `runtime/src/grammars/json/scan.rs:22` (P1-E §2.4 SIMD probe rank-1 on 16/17 corpora at 48.7–87.5% self-time). The current scan pipeline computes the quote mask, escape mask, and prefix-XOR carry in three separate steps; gap 6 collapses to one Layer-1 call.

**Consumer.** `runtime/src/grammars/json/scan.rs` `resolve_string_masks_64` (per P1-E §1.2 grep at `scan.rs:164`) — the existing 3-step compose collapses to one call. Lock 14: CSS L4 quoted strings (`"...string..."` in `<string>` per CSS Values L4 §3.4), Sheets quoted text (`"..."` with `""` escape), BBNF-self regex literal strings all use the same shape; the primitive is grammar-neutral.

**Substrate-union (Lock 1).** Returns the cumulative quote_mask + escape_mask; the caller fold into `compact_mask` (`bbnf-simd/src/lib.rs:227`) drives the shared tape. Pure Lock-1-conformant composition.

**REDRESS 88 distinction.** REDRESS 88 was a PMULL prefix-XOR as the hot body of `bitmap_prefix_xor_64` (replacing the scalar EVEN_BITS/ODD_BITS fold at `lib.rs:177-205`). Gap 6 does NOT propose PMULL; it composes the existing `bitmap_prefix_xor_64` (whatever its internal body) with the classifier + escape-mask in a Layer-1 primitive. P2-C scope for the PMULL question; P2-E only composes.

### Gap 7 — `parse_that_regex::number::scan_digit_run_simd_64` (NEON digit-run scan replacing SWAR ladder)

**Shape.** Layer-1 (in parse-that-regex, calling Layer-0 NEON) digit-run scan that takes `*const u8 + len + &mut NumberParts` and processes 64 bytes per iteration, returning the cumulative cursor advance + mantissa update. The current ladder at `parse-that-regex/src/number/mod.rs:106` processes 8 → 4 → 2 → 1 bytes per iteration; the SIMD variant processes 64 → 32 → 16 → tail. Uses `byte_class_from_eq_set_64` with set `[b'0', b'1', ..., b'9']` (10 elements — exceeds the 8-element contract limit of `byte_class_from_eq_set_64`; needs a **second** Layer-1 primitive `byte_class_from_range_64` with the inclusive `[b'0'..=b'9']` range — gap 7.5 below).

**Scalar reference.** `scan_digit_run` at `parse-that-regex/src/number/mod.rs:106` IS the scalar reference; the SIMD variant must produce the same mantissa + decimal_exp + digit_count side-effects.

**Layer placement.** Layer-1 in `parse-that-regex/src/number/scan_digit_run_simd.rs`. Consumes Layer-1 `byte_class_from_range_64` (gap 7.5) + Layer-1 `parse_16_digits_dotprod` (gap 5).

**P1 antecedent.** `number::scan_digit_run` per P1-E mode-III §2.4 — float-heavy corpora 4.96x–5.04x.

**Consumer.** `parse-that-regex/src/number/mod.rs:106` ladder replaced by SIMD-first branch when `dotprod` available.

**Substrate-union (Lock 1).** Same as Gap 5.

### Gap 7.5 — `bbnf_simd::aarch64::byte_class_from_range_64` (inclusive range membership)

**Shape.** Sibling of `byte_class_from_eq_set_64` (`bbnf-simd/src/lib.rs:282`) but for inclusive byte ranges instead of small sets. Takes `&[u8; 64]` + `(low, high)`; returns 64-bit mask where bit i is set iff `low <= src[i] <= high`. Uses two `vcgeq_u8` + one `vandq_u8` per chunk; `movemask_u8x16` per chunk; OR-fold 4 chunks into u64.

**Scalar reference.** Direct: `for i in 0..64 { if (low..=high).contains(&src[i]) { mask |= 1 << i; } }`. Layer-1 in `bbnf-simd::scalar::byte_class_from_range_64`.

**Layer placement.** Layer-1 in `bbnf-simd::aarch64::byte_class_from_range_64`. Layer-0 substrate: `vcgeq_u8` × 8 + `vandq_u8` × 4 + the existing Layer-1 `movemask_u8x16`. Pure NEON composition.

**P1 antecedent.** Same as Gap 7 (digit-run); also generalizes to UTF-8 continuation `[0x80..=0xbf]` per `is_utf8_continuation` (`parse-that-regex/src/lib.rs:914`), CSS L4 hex-digit `[0-9a-fA-F]` (two ranges OR-folded — extends to `_two_ranges_64`), and BBNF-self identifier `[a-zA-Z_]`.

**Consumer.** Gap 7's `scan_digit_run_simd_64` first; downstream Lock 14 generalizations follow.

**Substrate-union (Lock 1).** Returns mask; the consumer either emits positions via `compact_mask` (substrate-union conformant) or consumes the mask directly.

### Gap 8 — `parse_that_regex::unicode::utf8_codepoint_scan_64` (multi-codepoint UTF-8 width scan)

**Shape.** Layer-1 (in parse-that-regex/src/unicode/utf8_block.rs — the file already exists, currently empty per `ls`). Takes `*const u8 + len`; returns a `[u8; N]` of codepoint widths (1–4) for the next N codepoints up to a 64-byte horizon. Equivalent to running `validate_utf8_codepoint` (`parse-that-regex/src/lib.rs:843`) N times but vectorized via the Hoehrmann-style class table.

**Scalar reference.** Iterate `validate_utf8_codepoint` and record widths. Bit-identical.

**Layer placement.** Layer-1 in `parse-that-regex/src/unicode/utf8_block.rs` (file exists; currently no implementation). Consumes Layer-1 `bbnf_simd::aarch64::utf8::validate_block` (the per-chunk continuation-state primitive that already exists) + new Layer-0 NEON `vqtbl1q_u8` over a 16-entry leading-byte class table for width-class extraction.

**P1 antecedent.** Same composite as Gap 4: `parse_that_regex::skip_string_plain` UTF-8 path + `validate_utf8_codepoint` per-cursor calls. Gap 4 returns "validate-only, offset of first invalid"; Gap 8 returns "validate + codepoint width-stream", which the `unescape_string` (`lib.rs:718`) and `decode_unicode_escape` (`lib.rs:302`) consumers need for cursor advance bookkeeping.

**Consumer.** `parse-that-regex/src/lib.rs:602-627` (`validate_utf8_prefix`) + `:843-911` (`validate_utf8_codepoint`) collapse to one driver + the new Layer-1 primitive. CSS L4 declaration-value strings, Sheets text, BBNF-self comment text all need UTF-8-aware advance — Lock 14 generalization.

**Substrate-union (Lock 1).** Returns width array; no positions; the consumer drives cursor advance per width.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

Per CH2 binding + Lock 14 + dispatch context §2.1 (P2-F precedent), the grammar-neutrality verdict for each gap. P2-F (parallel-dispatched) consumes this section as input for its full grammar-neutral primitive matrix; the verdicts below are P2-E's local CH2 reasoning, not P2-F's cross-grammar abstraction.

| Gap | JSON | CSS L4 | Sheets | BBNF-self | Verdict |
|---|---|---|---|---|---|
| 1 `scan_string_special_block_sweep_64` | yes (RFC 8259 string) | yes (`<string>` token per CSS Syntax L3 §4.3.5) | yes (formula text `"..."`) | yes (regex literal string) | **grammar-neutral**; the special set `{terminator, escape, control, non-ASCII}` is the universal quoted-string vocabulary |
| 2 `unescape_uxxxx_x8_neon` | yes (`\uXXXX`) | yes (`\HEXHEX` per CSS Syntax L3 §4.3.7 — variable-width escape, the 1-to-6-nibble form folds into the 4-nibble path with mask) | partial (Sheets `CHAR()` formula; per-formula) | yes (BBNF `\u{...}` in regex literals) | **grammar-neutral with classifier**; the nibble LUT is universal; the surrounding escape grammar is per-language but the decoder primitive is shared |
| 3 `ascii_whitespace_skip_64` | yes (`{0x20, 0x09, 0x0a, 0x0d}`) | yes (`<whitespace-token>` per CSS Syntax L3 §4.3.1 — same set + `0x0c` form feed) | yes (formula whitespace) | yes (BBNF whitespace) | **grammar-neutral**; the byte-set is parameterized (3 ≤ set-size ≤ 5 across grammars; ≤8 contract holds) |
| 4 `utf8::validate_block_streaming` | yes (RFC 8259 requires UTF-8) | yes (CSS Syntax L3 §3.2 requires UTF-8) | yes (Sheets text values are UTF-8) | yes (BBNF source files are UTF-8) | **grammar-neutral**; UTF-8 belongs in NO grammar — it is the substrate |
| 5 `parse_16_digits_dotprod` | yes (JSON number mantissa) | yes (`<number>` per CSS Values L4 §4.1; `<integer>` per §4.2) | yes (formula numerals) | yes (`{n,m}` regex counts, BBNF rule counts) | **grammar-neutral**; the digit-pack shape is universal; per-grammar policy (negative sign, decimal separator, exponent) lives at the consumer |
| 6 `scan_string_with_carry_64` | yes | yes (CSS Syntax L3 string token) | yes (Sheets `""` doubled-quote escape — different escape policy, same backslash-carry shape with `'"'` as the escape byte) | yes | **grammar-neutral**; the carry-shape is the universal even/odd-backslash invariant; escape byte is a parameter |
| 7 `scan_digit_run_simd_64` (parse-that-regex; depends on 7.5) | yes | yes (CSS number digit-run) | yes (Sheets numerals) | yes (BBNF counts) | **grammar-neutral via 7.5** |
| 7.5 `byte_class_from_range_64` | yes (`0..=9`) | yes (`0..=9`, `a..=f`, `A..=F` for hex colors) | yes (Sheets `A..=Z` for column letters) | yes (`a..=z`, `A..=Z`, `_` for identifiers) | **grammar-neutral**; pure range-test primitive; extends to two-range OR-fold for hex/identifier vocabularies |
| 8 `utf8_codepoint_scan_64` | yes | yes | yes | yes | **grammar-neutral**; same as Gap 4 — UTF-8 is the substrate, not the grammar |

All 8 gaps are grammar-neutral; none requires per-grammar specialization at the primitive layer. Per Lock 14 + dispatch context §3 P-3 ("Grammar lives in the grammar"), the per-grammar specialization (e.g. CSS hex-color `#RRGGBB` parses, JSON `\uXXXX` parses, Sheets `""` escape) happens at the **consumer** layer (the codegen template), not at the bbnf-simd Layer-1 primitive. The byte-set, range, and special-character parameters are runtime arguments to the Layer-1 primitive.

CSS L4 generalization made from spec evidence (per CH2 F2 + dispatch context §1: zero CSS L4 grammar-neutral primitive evidence in P1 profile — only `declaration_values` renders as a parse-result row). Cited CSS specs above: CSS Syntax L3, CSS Values L4. The argument is spec-grounded; profile corroboration is unavailable at SK-V14 and is properly the burden of T-P1 + future CSS L4 corpus capture.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

Per CH3 + §1.4 above, the pre-block surface and the per-gap material differentials this artefact carries:

### §4.1 — Pre-blocks the gap list does NOT reopen (per-gap audit)

- **REDRESS 28, 33** (global tiny-string cap-16 policy) — Gap 1 wires the parse-that-regex consumer to call `_sweep_64`, not a global cap policy. The 16-byte body at `aarch64/string_block.rs:57` is already Wave-1-admitted.
- **REDRESS 50, 51, 52, 53, 54, 55** (UTF-8 fusion routes; sink-local decoded stats; source-method digest folds; quote-source streaming hash) — Gap 4 + Gap 8 return validate-only / width-only; no fused materialization; no sink interaction. Material differential: pure validation primitive, no decoded-stream sink.
- **REDRESS 60–72** (retained-parse + sidecar producers + cap-16 routes) — Gap 1 + Gap 6 do not propose a retained parse or a sidecar producer; they wire NEON sweep primitives that the existing one-shot bench harness consumes. No retention. No second source scan.
- **REDRESS 80** (canada mantissa-widen plan) — Gap 5 carries the differential per §2 Gap 5: Layer-1 grammar-neutral primitive vs. per-corpus hot-path tweak. **Flagged for S-P3 decision; S-P2 does not bypass REDRESS 80**; the differential is the primitive-vs-tweak distinction, but the burden of proof remains S-P3's.
- **REDRESS 82, 83, 84** (single-quartet unicode classifier; StringBlock16 tiny probe; object-pair compaction) — Gap 2 doubles the admitted `_x4` decoder body; it is not a new classifier and not a tiny-probe / compaction route.
- **REDRESS 88** (PMULL prefix-XOR hot body) — Gap 6 composes the existing `bitmap_prefix_xor_64` whatever its internal body; PMULL routing is P2-C scope.
- **REDRESS 89** (CSSC CTZ next-bit bulk consumer) — Gap 6 uses `trailing_zeros` on the OR-fold; no CSSC-specific dispatch. P2-C scope.
- **REDRESS 96, 97, 98** (union-substrate / class-column substrate routes) — Gap 6 explicitly composes with `classify_tbl4::classify_block_from_table` at `aarch64/classify_tbl4.rs:47` (the existing in-substrate-union classifier). No new column substrate; no allocation-free streaming cursor; no parser-owned structural projection.

### §4.2 — Lock-1 substrate-union risk (CH5 binding)

Per CH5 + P2-D's expected verdict (substrate-union holds): every gap above either returns a mask (Gap 1, 3, 6, 7.5) folded into the existing `compact_mask` consumer (`bbnf-simd/src/lib.rs:227`) or returns an offset / width / value (Gap 2, 4, 5, 7, 8) with no position emit. Zero gaps introduce a second position vector, a parallel cursor, or a sidecar event ring. **CH5-compliant by construction.**

### §4.3 — Lock-14 grammar-neutrality risk (CH2 binding)

Per §3 above, all 8 gaps verify as grammar-neutral with parameter-driven byte sets / ranges / classes. The risk: a gap that LOOKS grammar-neutral but encodes JSON policy in the default parameters (e.g. defaulting the whitespace set to JSON's `{0x20, 0x09, 0x0a, 0x0d}` and forgetting the parameter). The mitigation per `[hybrid-grammar-host]`: Layer-1 primitives carry NO defaults — the byte-set / range parameter is mandatory; the per-grammar default lives at the codegen template (the runtime/src/grammars/{json,css_l4,sheets}/scan.rs site).

### §4.4 — Same-wave-consumer risk (CH4 binding; §8 ORCHESTRATOR.md non-negotiable)

Per CH4 + §8 ORCHESTRATOR ("no kernel ships without a same-wave consumer"), every gap above carries a named consumer at parse-that-regex or runtime/src/grammars/json. The consumer commit MUST land in the same wave as the kernel commit. The risk: a kernel lands without the consumer wiring, leaving an "orphan primitive". S-P3 binds the consumer-pairing in its synthesis-plan output; S-P2 records the consumer-pair expectation per gap.

### §4.5 — Scalar-reference-first risk (CH4 binding; Lock 16)

Per CH4 + Lock 16 + dav1d/checkasm discipline (P2-B), every gap MUST land its scalar reference function FIRST, with bit-identical output to the NEON intrinsic body. The risk: a gap whose scalar reference is the inline SWAR code at the consumer site (e.g. Gap 1's scalar reference is "call `scan_string_special_block_scalar` four times" — that's a sufficient reference). The mitigation: every §2 gap entry above names the scalar reference function explicitly; S-P3's primitive-admission process per P2-B's checkasm discipline rejects any gap whose scalar reference is "inline at consumer".

### §4.6 — Generated-code line budget (per `[generated-size-budget]`)

The 8 gaps add an estimated 250–350 lines of new aarch64 / scalar code in `bbnf-simd::aarch64::*` plus 100–150 lines of scalar references plus 60–80 lines of checkasm parity tests. Estimated total: 410–580 lines. Per `[generated-size-budget]`, this is well under the per-tranche budget (the wave can absorb 8 primitives in one tranche). The parse-that-regex consumer-wiring deletions (the inline SWAR loops at `lib.rs:113`, `:128`, `:510-530`, `:565-572`, `:813-820`, `:843-911`) recover ~120 lines.

### §4.7 — F-V2-P1ABC-RERECORD dependency

Per dispatch context §1, the SK-V14 P1-A/B/C profile under-attributes string + number primitives because `dispatch_value` + `parse_object_value_at_direct` + `parse_array_element_at_direct` inline the inner primitives. F-V2-P1ABC-RERECORD (heavy deferred packet) re-records P1-A/B/C with `runtime/parse-attribution` enabled, cracking the envelope. Gaps 1, 3, 4, 5, 6, 7 all depend on the rerecord for their final c/B numbers; the **shape** of the gap (the primitive's algorithmic kernel) does NOT depend on rerecord, only the **per-row admit gate** at S-P3 does. P2-E binds: design now against the existing envelope-masked P1 profile + the named inner-leaf list at dispatch context §1; admit per-row at S-P3 against the F-V2-P1ABC-RERECORD-refined profile.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

### §5.1 — bbnf-lang source-of-truth (path:line on every claim)

- `skinny/Cargo.toml:10` (parse-that-regex member), `:31` (workspace dep)
- `skinny/crates/parse-that-regex/Cargo.toml:1` (crate name)
- `skinny/crates/parse-that-regex/src/lib.rs:113` (`skip_ascii_whitespace`), `:128` (`skip_ascii_spaces`), `:162` (`match_string_at_quote_trusted_utf8`), `:212` (`match_string`), `:228` (`match_string_at_quote`), `:284` (`validate_string_escape`), `:302` (`decode_unicode_escape`), `:347` (`validate_unicode_escape_run`), `:386` (`unescape_four_unicode_escapes` driver), `:462` (`skip_string_plain`), `:489-505` (NEON UTF-8 block + manual carry), `:547` (`skip_string_plain_trusted`), `:577` (`string_special_mask`), `:602` (`validate_utf8_prefix`), `:630` (`zero_byte_mask`), `:634` (`classify_string_content`), `:698` (private NEON `movemask_u8x16` re-impl), `:718` (`unescape_string`), `:813` (`find_next_escape_or_control`), `:832` (`string_escape_control_mask`), `:843` (`validate_utf8_codepoint`), `:914` (`is_utf8_continuation`), `:945` (`read_hex_unit_scalar`), `:959` (`hex_nibble`), `:969`/`:974` (`is_high_surrogate`/`is_low_surrogate`)
- `skinny/crates/parse-that-regex/src/number/mod.rs:32`, `:38`, `:106`, `:113-123`, `:165`, `:176`, `:187`, `:198`, `:207`, `:214`, `:226`, `:247`, `:261`
- `skinny/crates/parse-that-regex/src/unicode/utf8_block.rs` (currently empty — Gap 8 target)
- `skinny/crates/parse-that-regex/src/unicode/utf8_hoehrmann.rs` (Hoehrmann state machine — context)
- `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs` (hook to bbnf-simd)
- `skinny/crates/bbnf-simd/src/lib.rs:9` (`select_classifier`), `:107` (`scan_dispatch`), `:170` (`prefix_xor_64`), `:175` (`escape_mask_64`), `:209` (`find_ascii_set_member64`), `:227` (`compact_mask`), `:251-293` (`prim::*`), `:282` (`byte_class_from_eq_set_64`)
- `skinny/crates/bbnf-simd/src/dispatch.rs:42` (`select_classifier`), `:50` (`PrimitiveKernels`), `:63` (`select_primitive_kernels`)
- `skinny/crates/bbnf-simd/src/aarch64/mod.rs:1-32` (Layer-1 module declarations)
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` (`movemask_u8x16`)
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31` (`_scalar`), `:57` (`scan_string_special_block`)
- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:8` (`build_lo6_table`), `:22` (`classify_chunk_from_table`), `:47` (`classify_block_from_table`), `:75`/`:89` (structural+terminator variants)
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:38` (scalar), `:81` (NEON), `:130` (`build_class_table_lo6`)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40` (scalar), `:74` (NEON), `:125` (`_x4_neon`), `:169` (`join_surrogate_pair_neon`), `:201` (`HEX_NIBBLE_LUT`)
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5` (scalar), `:27` (`parse_4_digits_dotprod`), `:53` (`dot4_i8`)
- `skinny/crates/bbnf-simd/src/aarch64/utf8/validate_block.rs:5-90` (`ValidateStatus`), `:91+` (intrinsic body)
- `skinny/crates/bbnf-simd/src/scalar/mod.rs:1-9` (Layer-1 scalar references)
- `skinny/crates/runtime/src/grammars/json/generated.rs:43-44` (`cfg_attr(parse-attribution, inline(never))`), `:45-237` (envelope-folded `dispatch_value` + 14 admin-feature-gated functions), `:466` (`parse_object_value_at_direct`), `:506` (`parse_array_element_at_direct`)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22` (`scan_structurals`), `:107` (`scan_tail`), `:164` (`resolve_string_masks_64`)
- `skinny/RESULTS.md:1-185` (bench-gate authority)
- `skinny/REDRESS.md:2000-2050` (REDRESS 28/33 surface), `:1730` (REDRESS 50–55 surface), `:1873` (REDRESS 54/55), `:2426`/`:2461` (REDRESS 60–72), `:2853-2916` (REDRESS 96/97/98), `:4420-4438` (REDRESS 88/89 surface)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (full hot-leaf census; §1.3 primitive-classification table; §2.1 parse-only 17/17; §2.2 direct 17/17; §2.4 mode-III 17/17 SIMD/scalar ratios; §4.1 CH2 Lock-14 mis-attribution census; §4.4 substrate-union skip_value observation)
- `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md` (S-P1 dispatch context; §0-§5)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md` (S-P2 dispatch context; §0-§5; §1 F-V2-P1ABC-RERECORD; §1 CH2 F1/F2; §2 hard caps; §3 output structure)
- `restart/prompts/skinny/PASS-2-RESEARCH.md` (S-P2 contract; §2 scope matrix row P2-E; §2.1 frontmatter; §3 CH1-CH6; §7 hard caps; §8 bbnf-lang axes 1-6)
- `restart/locks/LOCKS.md` (Lock 1 substrate union; Lock 14 grammar-neutrality; Lock 16 scalar-reference-first)

### §5.2 — External / comparator sources

- **simdjson** — `find_quote_mask_and_bits` shape (cite for Gap 6 carry-fold): the simdjson aarch64 implementation at `include/simdjson/arm64/numberparsing.h` + `include/simdjson/arm64/simd.h` (simdjson 3.x); the prev-in-string carry is the universal SIMD-JSON shape per Lemire & Langdale "Parsing Gigabytes of JSON per Second" (VLDB Journal, 2019). The bbnf-lang current `bitmap_prefix_xor_64` + manual `escape_mask_64` compose mirrors this shape but exposes the steps separately.
- **sonic-rs** — strict-vs-strict comparator authority per dispatch context §8 + `[beat-lightningcss-target]`. sonic-rs's `src/parser.rs` + `src/visitor.rs` show the single-classifier-into-tape consumer shape; the bbnf-simd `classify_tbl4::classify_block_from_table` mirrors this and gap 6 composes it with prefix-XOR.
- **yyjson** — reference JSON parser with hand-written SIMD (asmjson-comparable). The `yyjson/src/yyjson.c` `read_string` shape inlines the special-byte sweep + carry; Gap 1 + Gap 6 collapse to one Layer-1 primitive matching yyjson's shape.
- **simdutf** — `validate_utf8` body cite for Gap 4 (cite: simdutf 5.x `src/arm64/arm_validate_utf8.cpp` + `src/scalar/utf8.h`). The Hoehrmann-style continuation-state shape is the reference for `validate_block_streaming`.
- **fast_float (Lemire)** — Eisel-Lemire f64 parsing cite (already used in `parse-that-regex/src/number/eisel_lemire/`). The Gap 5 UDOT 16-digit pack is upstream of Eisel-Lemire (mantissa accumulation), not a fast_float replacement.
- **dav1d / checkasm** — primitive-admission discipline per P2-B; cite for §4.5 scalar-reference-first risk. The dav1d `checkasm` harness shape is documented in `dav1d/tests/checkasm/checkasm.c` (cite: dav1d 1.4.x).
- **Arm Architecture Reference Manual** for A-profile architecture (Issue J.a, 2024) — Layer-0 substrate cite for `vqtbl1q_u8` (TBL), `vqtbl4q_u8` (TBL with 4-register table), `vshrn_n_u16` (SHRN), `vceqq_u8` (CMEQ), `vcgeq_u8` (CMHS), `vminvq_u8` (UMINV), UDOT (UDOT — DotProd extension per `FEAT_DotProd`), SDOT. These are the Layer-0 substrate primitives every Gap above composes.
- **Lemire, Mula** — "Faster parsing on commodity processors" (2019); cite for the `vqtbl4q_u8` low-6-bit classifier shape (the existing `bbnf-simd::aarch64::classify_tbl4` body cites this; Gap 6 extends).
- **CSS Syntax Module Level 3** — W3C CR-css-syntax-3 §3.2 (UTF-8 encoding), §4.3.1 (whitespace tokens), §4.3.5 (string tokens), §4.3.7 (escape decoding). Cite for §3 Gap 1/3/4/6 grammar-neutrality verdicts.
- **CSS Values and Units Module Level 4** — W3C ED-css-values-4 §3.4 (`<string>` definition), §4.1 (`<number>`), §4.2 (`<integer>`). Cite for §3 Gap 5/7 grammar-neutrality verdicts.
- **RFC 8259** — JSON spec; cite for §3 Gap 1/3 grammar-neutrality (the JSON-side reference for whitespace and string vocabularies).

### §5.3 — Prior tranche evidence (SK-V14 dispatch context binding)

- SK-V13 P1-A V1 + V2 save-only + sidecar profile artefacts under `/tmp/skv13-p1/` and `/tmp/skv13-p1-v2/` (path inherited per dispatch context §1; carry-through into SK-V14 baseline)
- SK-V13 P1-E V1 hot-leaf attribution synthesis (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`); SK-V14 P1-E supersedes for CH2 binding.
- SK-V13 REDRESS authority `skinny/REDRESS.md:1-5041` (full close-state route ledger; CH3 binding).
- SK-V14 audit-overlay synthesis `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (74 findings; 3 architectural sequencing constraints; binding row-falsification list).
- SK-V14 dispatch context §1 carry-forward bindings: F-V2-P1ABC-RERECORD (parse-attribution); CH2 F1 (cargo feature plumbing path); CH2 F2 (zero CSS L4 grammar-neutral primitive evidence at SK-V14); substrate-union finding (CH5 V3 verification at HEAD).
