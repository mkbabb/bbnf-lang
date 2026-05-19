# SK-V10 W8 Research - Escape/Segment Micro-Proof

Pass: Wave Research.
Cycle: W8.
Date: 2026-05-19.
Status: Read-only archive for SPEC Section 11.

## Scope

W8 is proof-only. It may select exactly one of:

- `C6-hex-escape-proof`
- `C7-string-segment-fold`

W7 rejected the full-string proof under REDRESS 106, so W8 can dispatch only
with a primitive whose entry gate does not depend on accepted W7 string proof.
No production caller behavior or `RESULTS.md` row can move in W8.

## Candidate Inventory

### C6 - Hex Escape

The C6 route targets fixed-width JSON `\uXXXX` hex decode/classify through the
current `unescape_string` caller in `skinny/crates/parse-that-regex/src/lib.rs`.
The existing aarch64 body is
`bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon`, consumed by
`unescape_four_unicode_escapes` when four contiguous Unicode escapes are
present.

Relevant existing references:

- Scalar single-quartet oracle:
  `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_scalar`.
- Surrogate helper:
  `bbnf_simd::aarch64::unescape_uxxxx::join_surrogate_pair_neon`.
- Caller policy owner:
  `parse-that-regex` owns slash, `\u`, invalid hex, surrogate-pair, and
  materialized-output policy.
- Existing parity:
  `checkasm_utf8_block::unescape_uxxxx_x4_matches_scalar` covers one valid x4
  packed batch, and `checkasm_parity::sk_v3_intrinsic_parity_aarch64` covers
  the single-quartet primitive.

Risk: C6 must not replay REDRESS 82's per-quartet classifier route. The
microbench must compare the current caller-level x4 path with a scalar-only
caller mirror, not just a single-quartet primitive.

### C7 - String Segment Fold

C7 targets decoded segment folding or output-plane unescape/materialization.
That makes it more policy-heavy than C6: it must choose direct digest or typed
product output, preserve lazy/borrrowed semantics, and avoid REDRESS 66-69
scratch/materialization routes. It is not blocked forever, but it is a poor W8
choice after W7 because it can accidentally combine string scanning, escape
decode, and output-plane policy.

## Research Finding

Select C6 for W8 unless CHALLENGE rejects it.

C6 is independent of W7, names one existing caller (`unescape_string` through
`unescape_four_unicode_escapes`), has an existing scalar oracle in
`bbnf-simd`, and can be falsified by a same-host caller microbench on
unicode-heavy fixture slices. C7 should remain routed to a later output-plane
wave only if a direct or typed consumer contract is selected first.

The W8 proof artifact should bind:

- primitive: `unescape_uxxxx_x4_neon`;
- caller: `unescape_string` through `unescape_four_unicode_escapes`;
- feature gate: aarch64 NEON on `aarch64-apple-darwin`;
- representative slices: `unicode_escapes`, `unicode_mixed`, and
  `y_string_unicode`;
- scalar oracle: scalar hex-unit decode plus scalar JSON surrogate policy;
- differential harness: caller-level parity over valid BMP, valid surrogate,
  invalid hex, lone surrogate, and non-contiguous escape cases;
- threshold: caller microbench must show at least `1.08x` throughput over a
  scalar-only caller mirror on the aggregate representative slice set before
  W9 may consume the proof.
