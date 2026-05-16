# SK-V7 Wave 5 R2 - SIMD string block checkasm plan

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Output date: 2026-05-16
Scope: read-only research artifact. No source or test file is modified here.

## 1. Inspected surface

Primary Wave 5 authority:

- `restart/skinny/tranches/sk-v7/SPEC.md` section 7 requires the B2 NEON
  16-byte plain-string scan for `match_tiny_plain_string_with_cap::<16>`,
  scalar reference plus checkasm parity, same-wave wiring, and no touch to the
  rejected `parse-that-regex` dispatcher/UTF-8 validation route.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` section 3 blocks the old retained
  string-scan and "Class A NEON tiny-string wiring as parse-G fix" routes. W5
  has to stay scoped to the generated tiny-string per-quote-pair probe.

Primary primitive:

- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`

Nearby tests and harnesses:

- `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- Dedicated sibling checkasm files under `skinny/crates/bbnf-simd/tests/`

Related call sites:

- `skinny/crates/runtime/src/grammars/json/generated.rs:171-183`
- `skinny/crates/parse-that-regex/src/lib.rs:594-715`

## 2. Existing primitive contract

`string_block.rs` already has the needed 16-byte NEON body:

- `StringSpecialBlock` carries four independent lane masks:
  `terminator_mask`, `escape_mask`, `control_mask`, and `non_ascii_mask`.
- `scan_string_special_block_scalar(&[u8; 16], terminator, escape,
  control_limit)` is the executable scalar reference.
- `unsafe scan_string_special_block(ptr, terminator, escape, control_limit)`
  does one `vld1q_u8(ptr)`, compares against the caller-supplied terminator and
  escape bytes, checks `byte < control_limit`, checks `byte >= 0x80`, and
  converts each compare vector through `movemask_u8x16`.

For JSON strings the parameters are exactly `(b'"', b'\\', 0x20)`. This matches
the W5 alphabet of quote, backslash, and control-byte rejection.

Important API caveat: `StringSpecialBlock::interesting_mask()` and
`first_interesting()` include `non_ascii_mask`. That is correct for generic
string-special reporting, but it is wrong for the generated tiny plain JSON
string helper because the generated parser receives `&str` bytes and the
plain-string fast path must allow non-ASCII content. A W5 helper must use only:

```text
terminator_mask | escape_mask | control_mask
```

## 3. Reuse decision

Reuse `scan_string_special_block`; do not write a new NEON compare kernel.

The missing piece is a small JSON-specific wrapper over the existing primitive.
The wrapper should encode the current scalar `match_tiny_plain_string_with_cap`
semantics for a full 16-byte content block:

- If the first special byte is `"`: return the advance past the closing quote.
- If the first special byte is `\` or `< 0x20`: return `None`.
- If there is no quote/backslash/control byte in the 16-byte block: return
  `None`.
- Ignore `non_ascii_mask` for the wrapper decision.

Sketch of the intended shape, not an implementation requirement:

```rust
#[cfg(target_arch = "aarch64")]
#[inline(always)]
pub unsafe fn match_plain_json_string_16(ptr: *const u8) -> Option<u8> {
    let block = scan_string_special_block(ptr, b'"', b'\\', 0x20);
    let special = block.terminator_mask | block.escape_mask | block.control_mask;
    if special == 0 {
        return None;
    }
    let lane = special.trailing_zeros() as u8;
    if (block.terminator_mask & (1u16 << lane)) != 0 {
        Some(lane + 1)
    } else {
        None
    }
}
```

The caller still needs the full-block readability guard. For the generated
`CAP=16` path, only call the wrapper when `offset + 1 + 16 <= input.len()`;
otherwise keep the scalar tail behavior so a short buffer can still find a
closing quote before EOF. The direct `CAP=8` path should remain scalar unless
that path gets a separate gate.

This does not need to touch `parse-that-regex/src/lib.rs:331` or the existing
trusted string scanner. That scanner already uses `scan_string_special_block`
for long trusted scans; W5 is about the generated tiny-string leaf in
`runtime/src/grammars/json/generated.rs`.

## 4. Scalar parity requirements

Two scalar contracts should be tested separately:

1. Raw primitive parity:
   `scan_string_special_block(ptr, b'"', b'\\', 0x20)` must equal
   `scan_string_special_block_scalar(&block, b'"', b'\\', 0x20)` for all four
   masks. This catches movemask, lane-order, control-boundary, and high-bit
   mask bugs.

2. Wrapper parity:
   the JSON helper must match a scalar helper equivalent to the current
   generated loop over exactly the first 16 content bytes. Non-ASCII bytes are
   not special for this scalar reference. A quote at lane 15 should return an
   advance of 16; a backslash or control byte before a later quote should return
   `None`.

The scalar wrapper oracle should not be `first_interesting()` because that
would reject non-ASCII before a quote and silently change accepted strings.

## 5. Existing coverage

Current coverage is useful but too thin for W5 admission:

- `aarch64_primitives.rs` has two focused `string_block` tests: one checks
  individual escape/control masks, and one compares a mixed block to
  `scan_string_special_block_scalar`.
- `checkasm_parity.rs` includes one AArch64 alignment sweep for
  `scan_string_special_block`, but the case is fixed and only proves one mixed
  layout.
- `checkasm_parity.rs` also has a separate generic `match_tiny_plain_string`
  low-6 table kernel. That is not the W5 primitive path and should not be used
  as the admission evidence for JSON quote/backslash/control scanning.

## 6. Required checkasm cases

Recommended admission coverage:

- Alignment sweep: call the NEON primitive at offsets `0..64` with at least 16
  readable bytes after the pointer. Use `checkasm_common::guarded_call`; on
  AArch64 also use the callee-saved register sentinel wrapper if the test body
  is promoted to full checkasm hardening.
- Lane sweep for each special class: quote, backslash, `0x00`, and `0x1f` at
  every lane `0..15`.
- Boundary bytes: prove `0x20` and `0x7f` are allowed, `0x00..=0x1f` are
  control, and `0x80..=0xff` only affect `non_ascii_mask`.
- Precedence cases: quote before slash/control returns `Some`; slash/control
  before quote returns `None`; no special returns `None`; quote at lanes 0 and
  15 return the correct advances.
- Non-ASCII cases: high-bit bytes before and after a quote must not change the
  wrapper result, while raw primitive parity still verifies `non_ascii_mask`.
- Randomized JSON-ish blocks: compare all raw masks and wrapper results against
  scalar over deterministic xorshift input, with injected quotes, slashes,
  controls, spaces, ASCII letters, and high-bit bytes.
- Tail behavior: cover in the generated/runtime helper test, not the raw
  pointer primitive, because the primitive's unsafe precondition is a full
  readable 16-byte block.

If this lands as a new `tests/checkasm_string_block.rs`, add it to the
hard-coded `primitive-checkasm` list in `skinny/xtask/src/main.rs`; Cargo may
autodiscover the file, but the gate command currently names each checkasm test
explicitly.

## 7. cfg and target-feature constraints

- `bbnf_simd::aarch64::string_block` is exported only under
  `#[cfg(target_arch = "aarch64")]` via `aarch64/mod.rs`.
- `string_block.rs` imports `core::arch::aarch64::*`, so tests that include or
  call it should be `#[cfg(target_arch = "aarch64")]` or should keep all NEON
  calls behind inner cfg blocks. Do not include this file unguarded on x86.
- No extra `target_feature` gate is needed for NEON/AdvSIMD on normal AArch64
  Rust targets. Existing code uses only `target_arch = "aarch64"` for this
  module; `target_feature = "dotprod"` in nearby tests is unrelated.
- The generated runtime already depends on `bbnf-simd`, so a cfg-gated direct
  call from `generated.rs` is dependency-valid. Non-AArch64 builds must keep the
  existing scalar loop.
- The wrapper inherits the primitive's unsafe precondition: `ptr` must point to
  16 readable bytes. The call-site guard is part of correctness, not an
  optimization.

## 8. Recommendation

Admit W5 Phase 1 as a wrapper-and-checkasm task around the existing
`scan_string_special_block` primitive. Do not reopen the generic low-6
`match_tiny_plain_string` kernel, the `parse-that-regex` UTF-8 dispatcher, or
the rejected retained 64-byte string-scan routes. The only SIMD behavior needed
for the generated tiny JSON string leaf is first quote/backslash/control within
one readable 16-byte block, with non-ASCII ignored for the fast-path decision
and verified only as a raw mask parity output.
