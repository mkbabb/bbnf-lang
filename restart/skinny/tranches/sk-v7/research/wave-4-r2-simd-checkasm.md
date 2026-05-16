# SK-V7 Wave 4 R2 - SIMD unicode escape checkasm plan

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Output date: 2026-05-16
Scope: read-only research artifact. No source or test file is modified here.

## 1. Inspected surface

Primary primitive file:

- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`

Existing checkasm/test files inspected:

- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`
- Existing sibling checkasm files under `skinny/crates/bbnf-simd/tests/checkasm_*.rs`

Related manifest fact:

- `skinny/crates/bbnf-simd/Cargo.toml:25-27` explicitly names
  `checkasm_parity`, but the package does not set `autotests = false`, so a
  future `tests/checkasm_unicode_escape.rs` should still be Cargo-discovered.
  CI or local gate commands that hard-code `--test checkasm_parity` will not run
  the new file unless the command is expanded.

## 2. Scalar reference

The scalar oracle is `unescape_uxxxx_scalar` at
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-47`.

Contract:

- Input is a `[u8; 4]` containing only the four hex bytes after the JSON
  `\u` prefix.
- Each byte is decoded through `hex_nibble` at
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:186-193`.
- Accepted domains are exactly `0..=9`, `A..=F`, and `a..=f`.
- Return is `Some(codepoint)` for a decoded UTF-16 code unit in
  `0x0000..=0xffff`; return is `None` if any lane is non-hex.
- Packing order is big-endian nibble order: `n0 << 12 | n1 << 8 | n2 << 4 | n3`.

There are two surrogate helpers:

- `join_surrogates(high: u16, low: u16) -> u32` at
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:54-56`. This is an
  algebra-only scalar helper. It assumes the caller has already proved
  `high in D800..=DBFF` and `low in DC00..=DFFF`.
- `join_surrogate_pair_neon(high: u32, low: u32) -> Option<u32>` at
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:169-175`. Despite the
  suffix, this is a scalar range-checking helper, not an intrinsic body.

The scalar reference is already compile-anchored in
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:446-458` with:

- one valid mixed-case quartet: `00aF -> 0x00af`
- one invalid quartet: `00XX -> None`
- one surrogate-pair combine: `D83D DE00 -> 0x1f600`

## 3. NEON entry points

### 3.1 Single quartet: `unescape_uxxxx_neon`

Entry point:

- `unsafe fn unescape_uxxxx_neon(ptr: *const u8) -> Option<u32>`
- Defined at `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74-121`.
- Exported through `skinny/crates/bbnf-simd/src/aarch64/mod.rs:30`.

Shape:

- Loads exactly four bytes via `vld1q_lane_u8::<0..=3>` from `ptr`,
  `ptr.add(1)`, `ptr.add(2)`, and `ptr.add(3)`.
- Uses `low_nibbles = bytes & 0x0f`.
- Uses `vqtbl1q_u8` against `HEX_NIBBLE_LUT`.
- Separately computes ASCII masks for digit, uppercase hex, and lowercase hex.
- Applies `alpha_adjust = is_alpha & 9`, so the LUT only needs digit low-nibble
  values; alpha lanes become `1..=6 + 9`.
- Rejects if any of the first four `is_hex` lanes is zero.
- Packs the first four decoded nibble lanes into a `u32`.

The important parity hazard is the low-nibble collision class. Bytes such as
`'1'` and `'A'` both select low-nibble slot `0x01`; correctness relies on the
range masks and `alpha_adjust`, not the LUT alone.

### 3.2 Four quartets: `unescape_uxxxx_x4_neon`

Entry point:

- `unsafe fn unescape_uxxxx_x4_neon(quartets: &[u8; 16]) -> Option<[u32; 4]>`
- Defined at `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125-166`.

Shape:

- Loads all 16 bytes with one `vld1q_u8`.
- Reuses the same low-nibble TBL and digit/upper/lower ASCII range masks.
- Rejects the whole batch if `vminvq_u8(is_hex) == 0`.
- Stores nibble lanes into a local `[u8; 16]`.
- Uses scalar `pack_quad` four times.

This is not a new algorithm; it is the same nibble classification widened from
one quartet to four contiguous quartets.

### 3.3 Surrogate pair helper

Entry point:

- `join_surrogate_pair_neon(high: u32, low: u32) -> Option<u32>`
- Defined at `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:169-175`.

This helper should be included in unicode escape checkasm coverage because it
is the public helper currently used with decoded units, but it should not drive
any new intrinsic work. It is a scalar range check plus algebraic combine.

## 4. Existing parity coverage

### 4.1 Current single-quartet coverage

`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:513-675` contains
`sk_v3_intrinsic_parity_aarch64`.

For `unescape_uxxxx_neon`, the active coverage is:

- `valid_hex_cases()` at `checkasm_parity.rs:556-561` with 10 hand-picked valid
  quartets: `0000`, `0001`, `00aF`, `09fF`, `D83D`, `DE00`, `ffff`, `FFFF`,
  `aBcD`, and `1234`.
- `invalid_hex_cases()` at `checkasm_parity.rs:563-584`, which starts from
  `1A2b` and replaces each of the four positions with every non-ASCII-hex
  byte, then adds a few targeted invalid examples.
- Alignment offsets `0..64`, using backing storage and passing
  `backing.as_ptr().add(align)`, at `checkasm_parity.rs:651-665`.
- The expected value is always `unescape_uxxxx_scalar(quartet)`.

This is good coverage for invalid-byte rejection and pointer alignment. It is
not exhaustive over the valid nibble domain.

### 4.2 Current x4 coverage

`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58-68` contains
`unescape_uxxxx_x4_matches_scalar`, but the name overstates the check:

- It uses one packed valid example: `0041 d83d de00 00e9`.
- It asserts a fixed decoded output: `[0x0041, 0xd83d, 0xde00, 0x00e9]`.
- It checks one valid surrogate join through `join_surrogate_pair_neon`.
- It does not call `unescape_uxxxx_scalar` for the four constituent quartets.
- It has no invalid-lane sweep.
- It has no alignment sweep.

### 4.3 Current checkasm hardening around unicode escape calls

`checkasm_common.rs` already provides:

- `Xorshift64` at `checkasm_common.rs:3-31`
- `guarded_call` and stack canary comparison at `checkasm_common.rs:33-53`
- AArch64 callee-saved register sentinels at `checkasm_common.rs:55-164`

The unicode escape calls in `checkasm_parity.rs` do not use those wrappers.
`checkasm_utf8_block.rs` also does not use them. As a result, the current
unicode escape coverage is parity-oriented, not DAV1D-grade call-hardening
coverage.

## 5. Parity coverage gaps

1. No standalone `checkasm_unicode_escape.rs`

   Unicode escape coverage is split between the large `checkasm_parity.rs`
   SK-V3 block and `checkasm_utf8_block.rs`. This makes it easy for the single
   quartet, x4 batch, and surrogate helper coverage to drift apart.

2. Valid-domain coverage is sparse

   The legal hex alphabet has 22 byte spellings:
   `0123456789ABCDEFabcdef`. The single-quartet legal domain is `22^4 =
   234,256` cases. Existing coverage checks 10 legal quartets. The low-nibble
   TBL design specifically needs broader valid-domain coverage because digit
   and alpha spellings collide in the LUT index and are disambiguated by range
   masks.

3. `_x4` lacks real scalar parity

   The existing x4 test asserts one fixed output and one surrogate join. It
   should instead compare `unescape_uxxxx_x4_neon(packed)` against four calls
   to `unescape_uxxxx_scalar`.

4. `_x4` lacks invalid-lane coverage

   The x4 path rejects the whole batch through `vminvq_u8(is_hex)`. A single
   invalid byte in any of the 16 lanes must return `None`. No current test
   checks that every lane participates in the reduction.

5. Surrogate boundary coverage is thin

   Current coverage checks only `D83D DE00`. It does not cover:

   - lower boundary: `D800 DC00 -> 0x10000`
   - upper boundary: `DBFF DFFF -> 0x10ffff`
   - rejected low-alone and high-alone shapes through `join_surrogate_pair_neon`
   - off-by-one invalids: `D7FF DC00`, `DC00 DC00`, `D800 DBFF`, `D800 E000`

6. Call-hardening wrappers are not applied

   `guarded_call` and `callee_saved_register_then` are available in
   `checkasm_common.rs`, but unicode escape NEON calls currently bypass them.
   A standalone checkasm file should wrap candidate calls through those helpers.

7. CI invocation risk

   The documented gate in prior handoff text often names
   `cargo test -p bbnf-simd --release --test checkasm_parity`. A new
   `checkasm_unicode_escape.rs` will not run under that exact command. The gate
   should either run `--tests` or explicitly add `--test checkasm_unicode_escape`.

## 6. Add `checkasm_unicode_escape` without new intrinsic bodies

Do not add new NEON bodies, dispatch layers, `prim` wrappers, or source
modules. The test should be a new integration test file only:

- New file in a follow-on implementation:
  `skinny/crates/bbnf-simd/tests/checkasm_unicode_escape.rs`
- Reuse existing implementation:
  `bbnf_simd::aarch64::unescape_uxxxx::{unescape_uxxxx_scalar,
  unescape_uxxxx_neon, unescape_uxxxx_x4_neon, join_surrogates,
  join_surrogate_pair_neon}`
- Reuse existing checkasm helpers:
  `mod checkasm_common;`
  `guarded_call`
  `callee_saved_register_then`
  `Xorshift64`

Suggested module import pattern:

```rust
mod checkasm_common;

#[cfg(target_arch = "aarch64")]
use bbnf_simd::aarch64::unescape_uxxxx as uxxxx;

#[cfg(not(target_arch = "aarch64"))]
mod uxxxx {
    include!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/aarch64/unescape_uxxxx.rs"
    ));
}
```

This preserves scalar compile coverage on non-AArch64 hosts and runs NEON
candidate parity only on AArch64.

Candidate wrappers:

```rust
#[cfg(target_arch = "aarch64")]
fn candidate_single(ptr: *const u8) -> Option<u32> {
    checkasm_common::callee_saved_register_then(|| {
        checkasm_common::guarded_call(|| unsafe { uxxxx::unescape_uxxxx_neon(ptr) })
    })
}

#[cfg(target_arch = "aarch64")]
fn candidate_x4(input: &[u8; 16]) -> Option<[u32; 4]> {
    checkasm_common::callee_saved_register_then(|| {
        checkasm_common::guarded_call(|| unsafe { uxxxx::unescape_uxxxx_x4_neon(input) })
    })
}
```

Reference helpers:

```rust
fn reference_x4(input: &[u8; 16]) -> Option<[u32; 4]> {
    Some([
        uxxxx::unescape_uxxxx_scalar(input[0..4].try_into().unwrap())?,
        uxxxx::unescape_uxxxx_scalar(input[4..8].try_into().unwrap())?,
        uxxxx::unescape_uxxxx_scalar(input[8..12].try_into().unwrap())?,
        uxxxx::unescape_uxxxx_scalar(input[12..16].try_into().unwrap())?,
    ])
}
```

Recommended tests:

1. `unicode_escape_scalar_reference_vectors_compile_all_hosts`

   Keep this host-independent. Assert known valid, invalid, and surrogate
   algebra examples against `unescape_uxxxx_scalar` and `join_surrogates`.

2. `unicode_escape_single_valid_domain_exhaustive`

   AArch64 only. Iterate all `22^4` legal spellings from
   `b"0123456789ABCDEFabcdef"` and assert
   `candidate_single(quartet.as_ptr()) == unescape_uxxxx_scalar(&quartet)`.
   This directly targets the TBL low-nibble collision class.

3. `unicode_escape_single_invalid_lane_sweep`

   AArch64 only. Start from `1A2b`. For each lane `0..4`, replace that lane
   with every byte `0..=255` that is not ASCII hex. Run over alignment offsets
   `0..64` and assert parity with the scalar reference.

4. `unicode_escape_single_alignment_curated_valids`

   AArch64 only. Use the existing curated legal cases from
   `checkasm_parity.rs:556-561`, plus boundary quartets `D800`, `DBFF`, `DC00`,
   `DFFF`, `7FFF`, `8000`, and `FFFF`, over alignments `0..64`. Also assert
   the backing bytes are unchanged after the candidate call.

5. `unicode_escape_x4_matches_four_scalar_calls`

   AArch64 only. Build packed 16-byte inputs from rotating curated quartets.
   For each packed input, assert `candidate_x4(&packed) == reference_x4(&packed)`.
   Include an alignment sweep by copying into `backing[align..align + 16]` and
   converting that window to `&[u8; 16]`.

6. `unicode_escape_x4_invalid_lane_sweep`

   AArch64 only. Start from a valid packed input such as
   `0041d83dde0000e9`. For each lane `0..16`, replace that lane with each
   non-hex byte and assert `candidate_x4(&packed) == None`. This proves every
   lane participates in the `vminvq_u8(is_hex)` rejection.

7. `unicode_escape_surrogate_join_boundaries`

   Host-independent for scalar algebra where valid; AArch64 import for the
   range-check helper when available. Cover:

   - `D800 DC00 -> 0x10000`
   - `D83D DE00 -> 0x1f600`
   - `DBFF DFFF -> 0x10ffff`
   - invalid pairs through `join_surrogate_pair_neon`: high below range, high
     in low range, low below range, low above range.

This design adds checkasm parity around existing code only. It deliberately
does not add:

- a new `unicode_escape_run_decode_utf8` intrinsic body
- a new `bbnf_simd::prim` dispatch wrapper
- a new parser/materializer call site
- new source-level surrogate vectorization

## 7. Risk notes

1. Name risk: `join_surrogate_pair_neon` is scalar

   The suffix may invite a new intrinsic body. Do not add one for this checkasm
   work. The helper is a scalar range-checking algebra function and should be
   tested as such.

2. Exhaustive valid single-quartet coverage is cheap; exhaustive x4 is not

   `22^4` single-quartet cases are practical. `22^16` x4 cases are not. The x4
   test should use scalar-per-quartet parity on curated and pseudo-random
   batches, plus a full invalid-lane sweep.

3. The single-quartet pointer API has a caller safety precondition

   `unescape_uxxxx_neon` reads four bytes from a raw pointer. Alignment sweeps
   prove unaligned loads, but they do not prove guard-page tail safety. The
   contract remains: caller must pass four readable bytes.

4. AArch64 CI is required for real candidate coverage

   The non-AArch64 include pattern can compile and run scalar anchors, but it
   cannot execute the NEON candidates. The parity gate must run on an AArch64
   host to cover the actual intrinsic bodies.

5. Existing hardening wrappers are partial

   `checkasm_common.rs` currently checks stack canary equality and AArch64
   general callee-saved registers. It does not provide vector callee-saved
   sentinels for `d8-d15`, and it does not provide a recoverable guard-page
   fault trampoline. The unicode escape test should reuse current helpers
   without expanding the hardening framework in the same patch.

6. Burying more checks in `checkasm_parity.rs` increases drift

   The existing file already combines classifier parity, bench reporting,
   scalar anchors, Class A string matching, Class B unicode escape, x86 scalar
   anchors, and ignored x86 intrinsic stubs. A dedicated
   `checkasm_unicode_escape.rs` gives the unicode primitive its own local
   oracle and keeps `_x4` coverage from being mistaken for UTF-8 block coverage.

7. Source docs mention poison folding, implementation uses explicit masks

   The module comments describe a poison-folding LUT shape, while the current
   implementation uses low-nibble LUT values plus explicit ASCII range masks
   and alpha adjustment. The test should validate behavior, not depend on the
   prose shape.

## 8. Minimal acceptance gate for the follow-on implementation

A follow-on implementation of `checkasm_unicode_escape.rs` is sufficient when:

- `cargo test -p bbnf-simd --release --test checkasm_unicode_escape` passes on
  AArch64.
- `cargo test -p bbnf-simd --test checkasm_unicode_escape` at least compiles
  and runs scalar anchors on non-AArch64 hosts.
- The new test file imports only existing primitive functions and
  `checkasm_common`; no intrinsic source files change.
- The gate command used by CI or tranche verification includes the new test
  target, not only `--test checkasm_parity`.
