# SK-V10 W7 Research - String Primitive Micro-Proof

Pass: Wave Research.
Cycle: W7.
Date: 2026-05-19.
Status: Read-only archive for SPEC Section 10.

## Scope

W7 is proof-only. It may select exactly one of:

- `C4-tiny-string-proof`
- `C5-full-string-proof`

No `RESULTS.md` row can move in W7, and no production caller behavior can be
rewired. W9 is the only later production wave that may consume an accepted W7
or W8 proof.

## Candidate Inventory

### C4 - Tiny String

The tiny-string route targets bounded plain-string endings in generated direct
or typed callers. P3-A names per-plane caps separately: generated direct cap 8,
typed parse cap 32, typed skip cap 96, and retained cap 16 excluded unless
explicitly targeted. Existing code has tiny-string helpers in
`direct_struct.rs`, Track 2, and `generated_real_typed.rs`, plus an aarch64
`match_tiny_plain_string` primitive in `bbnf-simd`.

Risk: the cap/plane split makes a W7 proof easy to over-generalize. A tiny
proof for one cap is not a proof for the other caps, and the retained cap-16
route remains blocked by REDRESS 28/33 and 83.

### C5 - Full String

The full-string route targets the current
`match_string_at_quote_trusted_utf8` caller in
`skinny/crates/parse-that-regex/src/lib.rs`. That caller uses
`skip_string_plain_trusted`, which already consumes
`bbnf_simd::aarch64::string_block::scan_string_special_block` on aarch64.
The scalar oracle exists as
`scan_string_special_block_scalar` in
`skinny/crates/bbnf-simd/src/aarch64/string_block.rs`; the call-site fallback
also has the existing 8-byte scalar SWAR loop in `skip_string_plain_trusted`.

Existing parity:

- `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs` checks the string block
  against the scalar reference.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` already sweeps alignment
  for `scan_string_special_block` in the shared checkasm harness.

Relevant rows and hot leaves:

- `unicode_mixed`: full string scan and escape leaves dominate the direct loss.
- `unicode_escapes`: full string scan and escape leaves dominate the direct
  loss.
- `unicode_basic`: already direct `A / GO`; useful as a maintain/proof slice,
  not as a new admission row.

Risk: C5 cannot claim row movement from microbench evidence. It can only prove
that the existing full-string caller has enough isolated headroom to justify a
later W9 production experiment. Any W9 row gate must still run full
direct/typed Criterion rows and maintain guards.

## Research Finding

Select C5 for W7 unless CHALLENGE rejects it. C5 has a single current caller,
a named scalar oracle, existing aarch64 primitive parity, and representative
unicode rows. C4 remains viable, but its cap/plane split makes it a better
later proof only after a specific direct or typed caller is selected.

The W7 proof artifact should bind:

- primitive: `scan_string_special_block`;
- caller: `match_string_at_quote_trusted_utf8` through
  `skip_string_plain_trusted`;
- feature gate: aarch64 NEON on `aarch64-apple-darwin`;
- representative slices: `unicode_mixed`, `unicode_escapes`, and
  `unicode_basic`;
- scalar oracle: `scan_string_special_block_scalar` plus scalar SWAR caller
  fallback semantics;
- differential harness: `checkasm_parity` string-special block sweep;
- threshold: caller microbench must show at least 1.08x throughput over a
  scalar-only caller mirror on the same slices before W9 may consume the proof.
