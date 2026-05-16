# SK-V5 D6 — Class A/B kernel novelty audit

Hard cap interrogation of the claim "Class A (`match_tiny_plain_string`) and
Class B (`unescape_uxxxx`) NEON kernels need consumer wiring". Outcome up
front, citations below.

**Verdict** —

- Class A `match_tiny_plain_string_neon`: **WRONG-DIAGNOSIS for the B1 hot
  leaf.** The kernel is grammar-generic, parity-correct, and authored against
  the Wave-2-asm scalar 8-byte PC band (`generated.rs:161–172`, PCs
  0x2734/0x3158). That band is **not** the B1-identified bound — B1 names
  `skip_json_string_plain`'s fall-through to `validate_utf8_codepoint` as the
  real cost, which is a different function in a different crate
  (`parse-that-regex/src/lib.rs:420`) and already has a NEON path. Wiring
  Class A into the existing 8-byte early-out slot was attempted, regressed
  twitter by ~25%, and was reverted (`REDRESS.md:301-313`). So the "awaiting
  wiring" framing is incorrect: the kernel was wired once, the wiring shape
  was wrong, the kernel is good but lives on the wrong layer to close B1.

- Class B `unescape_uxxxx_neon`: **READY-FOR-WIRING and partially wired
  already.** `parse-that-regex/src/lib.rs:728` already dispatches to it from
  `read_hex_unit_with_error_offset`. The B2 4-nibble-batched and
  surrogate-pair-pipeline extensions are the still-open work; the kernel as
  written targets exactly the right pathology.

---

## §1. `match_tiny_plain_string.rs` — current state and references

File: `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`
(162 LOC after working-tree edits; +75 / -11 vs HEAD).

**Public surface:**

- `match_tiny_plain_string_scalar(haystack: &[u8;16], is_member: &[bool;256]) -> u16`
  (`match_tiny_plain_string.rs:38`) — scalar parity anchor.
- `first_match_scalar(...)` (`:54`) — LSB-first index of first hit.
- `match_tiny_plain_string_neon(haystack: *const u8, table: uint8x16x4_t) -> (u16, Option<u8>)`
  (`:81`) — three-µop `vqtbl4q_u8 → vshrn movemask → rbit/clz` body.
  **Working tree fills in the Wave 1 stub**: HEAD had `unimplemented!()`
  (`git diff` shows the body went from "Wave 1 Agent 2 will fill in" to
  three-µop intrinsics). This is genuinely the Wave 1 admission body.
- `match_json_string_specials_neon(haystack: *const u8) -> (u16, Option<u8>)`
  (`:110`) — JSON-grammar-leak duplicate (cited as a grammar leak by
  `skv5-A5-grammar-generalization.md:54`).
- `build_class_table_lo6(alphabet: &[u8]) -> [u8; 64]` (`:156`) — table
  builder for the TBL kernel.

**Module wiring (production):** `bbnf-simd/src/aarch64/mod.rs:12` declares
`pub mod match_tiny_plain_string;`, so symbols are exported from the crate.

**Call sites (production runtime):** ZERO.
- The public crate entry `bbnf_simd::match_json_tiny_plain_string`
  (`bbnf-simd/src/lib.rs:190`) is an **8-byte scalar loop**:
  ```
  pub fn match_json_tiny_plain_string(input: &[u8], offset: usize) -> Option<usize> {
      match_json_tiny_plain_string_scalar(input, offset)  // :191
  }
  pub fn match_json_tiny_plain_string_scalar(input, offset) {
      let mut cursor = offset + 1;
      let limit = (cursor + 8).min(input.len());
      while cursor < limit { match input[cursor] { ... } }   // :195-206
  }
  ```
  There is no `#[cfg(target_arch = "aarch64")]` branch dispatching to
  `aarch64::match_tiny_plain_string::*`. The NEON kernel file is exported
  but the public entry never reaches it.
- `runtime/src/grammars/json/generated.rs:161-163` shadows that public
  entry as a thin trampoline `fn match_tiny_plain_string(input, offset) ->
  bbnf_simd::match_json_tiny_plain_string(input, offset)`. Same thing in
  `generated_eventcursor.rs:207`, `codegen/src/json_templates/generated.rs:161`,
  `bbnf-bench/src/track2/json.rs:309`.

**Call sites (tests):** `tests/checkasm_parity.rs:436, 440, 577, 588-596,
624-625, 644` — the kernel is exercised under checkasm differential parity
and is parity-green per `REDRESS.md:301-307`.

So today the NEON kernel **exists, is parity-correct, is grammar-generic
(takes a `uint8x16x4_t` table), but is unreachable from any non-test code
path**.

---

## §2. `unescape_uxxxx.rs` — current state and references

File: `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` (152 LOC after
working-tree edits; +63 vs HEAD).

**Public surface:**
- `unescape_uxxxx_scalar(quartet: &[u8;4]) -> Option<u32>` (`:40`) — scalar
  parity anchor.
- `join_surrogates(high: u16, low: u16) -> u32` (`:54`) — RFC-specific
  surrogate combine helper.
- `unescape_uxxxx_neon(ptr: *const u8) -> Option<u32>` (`:74`) — the
  intrinsic body decoding all four nibbles via `vqtbl1q_u8` + ASCII-range
  masks + manual lane extract. Working tree fills in what HEAD had as a
  stub.
- `hex_nibble` (`:124`), `HEX_NIBBLE_LUT` (`:139`) — supporting tables.

**Call sites (production runtime):** **ONE — and it is correct.**
- `parse-that-regex/src/lib.rs:728` —
  `let unit = unsafe { bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon(hex.as_ptr()) }`
  inside `read_hex_unit_with_error_offset`, which is the per-`\uXXXX`-unit
  decoder called from `decode_json_unicode_escape`
  (`parse-that-regex/src/lib.rs:362`).
- Audit confirmation: `skv5-A3-parse-that-gaps.md:319` lists
  `read_hex_unit_with_error_offset` as "already correctly dispatches… leave
  as-is; this is the model".

**Call sites (tests):** `tests/checkasm_parity.rs:446, 450, 454, 689-691`.
Parity-green per `REDRESS.md:301-307`.

So Class B is **already wired** for the single-nibble-quartet case. The
"awaiting wiring" status in `skv5-A6-research-ledger.md:165` is stale; what
is genuinely missing is the **4-quartet-batched** decode + the
surrogate-pair fast path that B2 names
(`skv5-B2-direct-attribution.md:340`).

---

## §3. `parse-that-regex/src/lib.rs:331` vs `skip_json_string_plain` —
same function or different?

**Line 331** sits in `match_string_at_quote`
(`parse-that-regex/src/lib.rs:295-347`). The relevant body:

```
let mut cursor = skip_json_string_plain(input, offset + 1);   // :301
loop {
    let Some(byte) = input.get(cursor).copied() else { break; };
    match byte {
        b'"'        => { ... return Ok(...); }                // :309
        b'\\'       => { cursor = validate_json_string_escape(...)?; }     // :319
        0x00..=0x1f => { return Err(...); }                   // :324
        0x80..=0xff if mode != StringMode::ByteString => {
            flags.insert(StringFlags::HAS_NON_ASCII);
            cursor = validate_utf8_codepoint(input, cursor)?; // :333
            cursor = skip_json_string_plain(input, cursor);   // :334
        }
        _ => { cursor += 1; cursor = skip_json_string_plain(...); }   // :337-338
    }
}
```

`skip_json_string_plain` itself (`parse-that-regex/src/lib.rs:420-446`)
**already has NEON dispatch** for the 16-byte stride:

```
#[cfg(target_arch = "aarch64")]
unsafe {
    while cursor + 16 <= input.len() {
        let block = bbnf_simd::aarch64::string_block::scan_string_special_block(
            input.as_ptr().add(cursor), b'"', b'\\', 0x20);
        if let Some(offset) = block.first_interesting() { return ...; }
        cursor += 16;
    }
}
```

That block stops on **any** byte where `byte == '"' || byte == '\\' ||
byte < 0x20`. Crucially, the scalar fallback (`:437-444`) also stops on
`HIGH_BITS` (any byte ≥ 0x80). So **non-ASCII bytes exit the fast loop**,
and the outer dispatcher at `:331` then runs `validate_utf8_codepoint` and
restarts the scan. B2's defect — "exits the 16-byte NEON loop on every
0x80..=0xff byte" — is precisely this: the NEON `string_block` early-exits
on the high bit (the `HIGH_BITS` term in `json_string_interesting_mask`
at `:454-460`), routing through scalar UTF-8 validation per code-point
boundary instead of consuming a NEON stride.

**Same kernel as Class A?** NO. Class A is `match_tiny_plain_string` in
`bbnf-simd/aarch64/match_tiny_plain_string.rs`, a 16-byte byte-class TBL
membership test with `vqtbl4q_u8` over a 64-byte alphabet table. The hot
loop body B1 names lives in `parse-that-regex/src/lib.rs:295-347` and uses
`bbnf-simd::aarch64::string_block::scan_string_special_block` (a totally
different kernel at `bbnf-simd/src/aarch64/string_block.rs:65`), then
falls through to `validate_utf8_codepoint`. There is no path from
`match_string_at_quote` into `match_tiny_plain_string` at any cursor depth.

The names overlap because of the SK-V4-era "tiny plain string" framing
(Wave-2-asm PROFILE-REPORT picked the symbol `match_tiny_plain_string`
because that's what was inlined twice into `parse_value_at` and dominated
self-time at PCs 0x2734/0x3158 — see `wave2-asm/PROFILE-REPORT.md:22-30`).
That symbol IS `bbnf_simd::match_json_tiny_plain_string_scalar`'s body
inlined — an 8-byte fast-path early-out before `match_json_string_at_quote`
takes over. So "tiny plain string" in SK-V4 ≠ the body B1 named.

---

## §4. Class A diagnosis — was it right or wrong given B1?

**The kernel's stated target** (per the kernel docstring at
`match_tiny_plain_string.rs:18-23`): "Replaces in asmjson the
`cmp_eq_byte_x8` ladder used by the SK-V2 monolithic scanner to match short
field-name strings (`"id"`, `"name"`, `"value"`, …). asmjson emitted ~8
vceqq + ORR fan-ins per call; the NEON TBL path collapses them into a
single 4-register table lookup."

**The B1 finding** (`skv5-B1-parse-attribution.md:164-169, 330-331`):
"region 04 (tiny_plain_string_scalar) is at most 7.9% on random; the real
bound is one layer below, in `skip_json_string_plain`'s fall-through" to
`validate_utf8_codepoint`.

**Diagnosis:** the kernel was authored against the **SK-V4 Wave-2-asm
hypothesis** — that the 8-byte scalar early-out (`bbnf-simd/src/lib.rs:195`,
sampled at `generated.rs:161-172` and inlined into PCs 0x2734/0x3158) was
the dominant cost. SK-V5 B1 invalidated that hypothesis with PC-level
attribution: the early-out is at most 7.9% of self-time on random. The
real bound is one call layer below — inside `match_string_at_quote`'s body,
on the `0x80..=0xff` path that re-enters `skip_json_string_plain` after a
UTF-8 codepoint validation.

The kernel itself is **generic**: it takes any `uint8x16x4_t` table and
matches any 16-byte alphabet, so it is technically reusable for many byte
classes including the JSON string-specials class. But:

1. The B1 hot leaf is **inside** `skip_json_string_plain` — a function that
   **already has** a NEON 16-byte stride using a different primitive
   (`scan_string_special_block`). The Class A kernel is a different shape:
   it gives an alphabet-table-driven hit mask; `scan_string_special_block`
   gives a triple-membership `(quote|slash|control)` mask. Replacing
   `scan_string_special_block` with Class A would not change ops/byte — both
   are one TBL/cmp + one movemask + one ctz.

2. The B1 cost is **the fall-through after the 16-byte loop exits**, not
   the loop body itself. The fix is not a faster byte-class kernel; it is
   batching `validate_utf8_codepoint` across the high-bit stride instead of
   one codepoint at a time. That is a **UTF-8 NEON pipeline**, not a TBL
   membership primitive.

3. Empirical confirmation: when the Class A 16-byte helper was routed into
   Track 1/Track 2, "Criterion showed a real `twitter` regression of
   roughly 25% on both tracks, so the active parser remains on the 8-byte
   scalar tiny recognizer" (`REDRESS.md:308-311`). The kernel WAS wired and
   it regressed — the call shape was wrong because the 8-byte scalar
   early-out was already a winning gate against the heavier
   `match_string_at_quote` call; replacing it with a 16-byte NEON probe
   loses the fast-fail.

**Class A is WRONG-DIAGNOSIS.** The kernel targets a layer (the 8-byte
early-out / tiny-string membership) that is NOT the B1-named bound, and
the only place it would be useful (inside `skip_json_string_plain`) is
already saturated by `scan_string_special_block`.

---

## §5. Class B diagnosis — was it right or wrong?

**The kernel's stated target** (per the docstring at `unescape_uxxxx.rs:22-28`):
"Replaces `unescape_uXXXX_scalar` in the SK-V2 string materializer, which
decoded each `\uXXXX` byte via a 16-way switch + integer multiply."

**B2 finding** (`skv5-B2-direct-attribution.md:54`): on `y_string_unicode`,
`unescape_uxxxx_neon` shows up at 2.18% self-time;
`decode_json_unicode_escape` 14.34%; `SinkParser::string` 19.14%. The
existing wiring is correct; the kernel is doing its job. What's missing
(`skv5-B2-direct-attribution.md:340`) is "**NEON `\uXXXX` TBL hex decode +
surrogate-pair fast path**", projected at +25-50% on `unicode_escapes`.

**Diagnosis:** Class B was authored against the correct pathology — per-
`\uXXXX` hex decode. The kernel itself is correct, parity-green, and
already wired through `read_hex_unit_with_error_offset`
(`parse-that-regex/src/lib.rs:728`).

The B2 residual cost is **caller-shape**:
- 4-byte-at-a-time entry (the current call shape) loads 4 bytes per quartet
  with `vld1q_lane_u8::<0..3>` (`unescape_uxxxx.rs:76-79`) — four dependent
  scalar lane loads, defeating the wide-stride win of the TBL.
- Surrogate-pair join (`unescape_uxxxx.rs:54`) is a scalar helper outside
  the NEON body; the high/low pairing happens in `decode_json_unicode_escape`
  via scalar control flow (`parse-that-regex/src/lib.rs:373-389`).

**Class B is READY-FOR-WIRING (for the batched form).** The 4-nibble
single-quartet body is correct and wired. The B2 win requires (a) batched
4-quartet entry (16 bytes = 4 codepoints in one NEON pass) and (b) a
surrogate-pair branch-free pipeline. Neither exists today, but the existing
kernel is the building block, not a misdiagnosis.

---

## §6. Final verdict + concrete remediation pattern

| Kernel | Verdict | Pattern |
|---|---|---|
| Class A `match_tiny_plain_string_neon` | **WRONG-DIAGNOSIS** | The kernel targets the 8-byte scalar early-out; that layer is NOT the B1 hot leaf. The only B1-relevant rewrite is **inside** `parse-that-regex/src/lib.rs:295-347` — replace the `0x80..=0xff` per-byte fall-through with a NEON UTF-8 codepoint scanner that consumes multiple multibyte runs per pass, and keep `scan_string_special_block` as the body kernel. Class A is *useful* as a grammar-generic vocabulary item per A5 §grammar-generalization, but is **not** the kernel that closes B1. The "awaiting wiring" ledger row is stale — the wiring was attempted and reverted at `REDRESS.md:308-311`. |
| Class B `unescape_uxxxx_neon` | **READY-FOR-WIRING (batched extension)** | Single-quartet body is correct and already wired (`parse-that-regex/src/lib.rs:728`). The B2 win requires: (1) a 4-quartet batched entry `unescape_uxxxx_x4_neon(*const u8) -> [Option<u32>; 4]` that does one 16-byte `vld1q_u8` and runs the TBL chain over all 16 nibbles in parallel; (2) a NEON surrogate-pair join over the resulting `[u32; 4]` so the scalar control flow at `parse-that-regex/src/lib.rs:373-389` disappears for the dense `\uXXXX\uXXXX…` case (the `y_string_unicode` and `unicode_escapes` workloads). |

**On the escape_mask_64 NEON bug:** the working-tree edits to
`match_tiny_plain_string.rs`, `unescape_uxxxx.rs`, and `checkasm_parity.rs`
are the Wave-1 stub-to-real fills (HEAD had `unimplemented!()`; working
tree has the three-µop body). They are **independent** of the
`escape_mask_64` correctness fix called out in `HANDOFF.md:54(d)`. The
`escape_mask_64` fix is "state-handoff confusion between `escape_mask_64`'s
`new_carry` and `scan_json_tail`'s `escaped` arg" — neither of those names
appears in the three working-tree files. The `escape_mask_64` repro
(`xorshift seed 0xCAFEF00DBAADF00D, iter 0, 128-byte JSON-pool buffer`) is
separate redress work and is not on the diff for these three files.

**On novelty of the diagnosis itself:**

- "Class A needs consumer wiring" — **NOT NEW**. It was attempted at
  SK-V3 Wave 0/1, regressed twitter 25%, and was reverted
  (`REDRESS.md:301-313`). What is new from B1 is the explanation: the
  wiring layer was wrong because the 8-byte scalar early-out was a fast-
  fail gate, and the actual bound is inside `match_string_at_quote`'s
  UTF-8 fall-through, not the gate.

- "Class B needs consumer wiring" — **PARTIALLY NEW**. The single-quartet
  wiring exists today. What is new from B2 is the batched 4-quartet entry
  + surrogate-pair NEON pipeline, which has never been authored or wired.

**Recommendation:** retire the "Class A awaiting wiring" entry from the
SK-V5 still-open list; replace with two new items —

1. **B1 leaf fix**: NEON UTF-8 codepoint pipeline inside
   `parse-that-regex/src/lib.rs:331-339`, consuming multiple multibyte runs
   per pass rather than one codepoint at a time.

2. **B2 batched Class B**: 4-quartet `unescape_uxxxx_x4_neon` +
   NEON surrogate-pair join, replacing the scalar dispatch at
   `parse-that-regex/src/lib.rs:373-389`.

These are the kernels SK-V5 actually needs. Class A as-written remains a
parity-green grammar-generic primitive in the Layer-1 vocabulary; it is
not on the critical path for closing the B1/B2 corpora.
