# SK-V10 W8 Plan - Hex Escape Micro-Proof

Status: Phase 2 plan for SPEC Section 11.

## Selected Intervention

Select `C6-hex-escape-proof`.

The proof target is the current `unescape_string` caller in
`skinny/crates/parse-that-regex/src/lib.rs`, through its aarch64
`unescape_four_unicode_escapes` path. The primitive is
`bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon`.

W8 is independent of W7 because it does not consume the rejected full-string
scanner proof. It starts after a raw string slice already requires unescape and
measures only the escape decode/materialization caller. W8 does not wire new
production behavior and does not move `RESULTS.md`.

## Owner Paths

The owner paths are exactly SPEC Section 11:

- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs` (read-only evidence
  unless the same wave owns generator input and regeneration)
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (read-only evidence
  unless the same wave owns generator input and regeneration)
- `restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/`

## Proof Shape

Use the existing primitive scalar anchor and add caller-level proof evidence:

- primitive scalar oracle:
  `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_scalar`;
- caller scalar mirror: scalar JSON `\uXXXX` decode, surrogate-pair policy, and
  one-escape-at-a-time `unescape_string` semantics;
- differential harness: proof artifact cases for valid BMP, valid surrogate
  pair, invalid hex, lone surrogate, non-contiguous escape batches, and full
  fixture string contents;
- existing parity evidence:
  `checkasm_utf8_block::unescape_uxxxx_x4_matches_scalar` and
  `checkasm_parity::sk_v3_intrinsic_parity_aarch64`.

Add a same-host caller microbench artifact under
`restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/`. The
artifact must benchmark current `unescape_string` against the scalar-only
mirror on raw string contents containing `\u` escapes from:

- `unicode_escapes`
- `unicode_mixed`
- `y_string_unicode`

## Falsifiability Gate

Exit gate: `G-W8-ESCAPE-SEGMENT-MICROPROOF` from SPEC Section 11.

Operational threshold:

- Caller microbench throughput must be at least `1.08x` over the scalar-only
  caller mirror on the aggregate representative slice set.
- The proof artifact must record observed value, threshold, run id, host
  triple, build flags, feature gate, representative corpus slices, sample
  count, scalar oracle identity, differential harness identity, and the named
  current caller.
- Scalar oracle and differential harness tests must pass on the same host.
- JSON slash, `\u`, invalid hex, surrogate-pair, and materialized output policy
  remain in the caller/proof artifact; the generic SIMD primitive remains
  fixed-width hex decode only.
- No `RESULTS.md` row moves.

## Same-Wave Consumer

W8 is proof-only, so the same-wave consumer is the caller microbench artifact:
it invokes the current `unescape_string` production caller and a scalar-only
mirror of the same caller. W9 is the only later wave allowed to convert an
accepted W8 proof into production behavior.

## Budget And Risk

- LOC budget: 90-260 proof LOC.
- Risk: HIGH. Unicode escape policy has many invalid and surrogate cases, and
  REDRESS 82 blocks per-quartet classifier replay.
- Redress cap: <=90 minutes.

## Revert Protocol

On scalar parity failure, differential failure, policy leak, or caller
microbench below `1.08x`:

1. Save any proof/harness patch to `/tmp/skv10-waveW8-rejected.patch`.
2. Revert W8 proof/harness/microbench changes as one slice.
3. Record REDRESS with the observed failure and measured value versus
   threshold.

## Pre-Blocked Routes

- REDRESS 64 retained Unicode-escape run validation as shipped remains closed.
- REDRESS 66-69 scratch/materialization routes remain closed.
- REDRESS 82 per-quartet Unicode classifier replay remains closed.
- REDRESS 83 StringBlock16 tiny wrapper remains closed.
- PMULL/CTZ defaults from REDRESS 88/89 remain closed.
- W8 cannot combine proof with W9 production wiring.
