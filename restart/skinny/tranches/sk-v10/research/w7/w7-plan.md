# SK-V10 W7 Plan - Full String Primitive Micro-Proof

Status: Phase 2 plan for SPEC Section 10.

## Selected Intervention

Select `C5-full-string-proof`.

The proof target is the existing `match_string_at_quote_trusted_utf8` caller in
`skinny/crates/parse-that-regex/src/lib.rs`, through its current
`skip_string_plain_trusted` fast path. The candidate primitive is
`bbnf_simd::aarch64::string_block::scan_string_special_block`. W7 does not move
`RESULTS.md`, does not wire any new production behavior, and does not claim a
parse-only SOTA admission.

`C4-tiny-string-proof` remains rejected for W7 because its cap and output-plane
surface is split across retained cap 16, generated direct cap 8, typed parse
cap 32, and typed skip cap 96. A tiny-string proof would need a narrower caller
than W7 currently owns.

## Owner Paths

The owner paths are exactly SPEC Section 10:

- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs` (read-only evidence
  unless the same wave owns generator input and regeneration)
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (read-only evidence
  unless the same wave owns generator input and regeneration)
- `restart/skinny/tranches/sk-v10/research/p3/string-primitive-proof/`

## Proof Shape

Use the existing scalar oracle and parity surface:

- scalar oracle:
  `bbnf_simd::aarch64::string_block::scan_string_special_block_scalar`.
- caller scalar mirror: the current scalar SWAR semantics in
  `skip_string_plain_trusted`, specifically the 8-byte `string_special_mask`
  fallback loop.
- differential harness:
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` string-special alignment
  sweep plus `aarch64_primitives::string_special_block_matches_scalar_reference`.

Add a same-host caller microbench artifact under
`restart/skinny/tranches/sk-v10/research/p3/string-primitive-proof/`. The
artifact must benchmark the current trusted-UTF8 string caller against a
scalar-only caller mirror on representative slices extracted from:

- `unicode_mixed`
- `unicode_escapes`
- `unicode_basic`

## Falsifiability Gate

Exit gate: `G-W7-STRING-MICROPROOF` from SPEC Section 10.

Operational threshold:

- Caller microbench throughput must be at least `1.08x` over the scalar-only
  caller mirror on the aggregate representative slice set.
- The proof artifact must record observed value, threshold, run id, host
  triple, build flags, feature gate, representative corpus slices, sample
  count, scalar oracle identity, differential harness identity, and the named
  current caller.
- Scalar oracle and differential harness tests must pass on the same host.
- No `RESULTS.md` row moves.

## Same-Wave Consumer

W7 is proof-only, so the same-wave consumer is the caller microbench artifact:
it invokes the existing production caller and a scalar-only mirror of that same
caller. W9 is the only later wave allowed to convert an accepted proof into
production behavior.

## Budget And Risk

- LOC budget: 90-260 proof LOC.
- Risk: MEDIUM-HIGH. The main risk is paper-closing an already-wired primitive
  without proving caller-level benefit, so the caller microbench is binding.
- Redress cap: <=90 minutes.

## Revert Protocol

On scalar parity failure, checkasm differential failure, or caller microbench
below `1.08x`:

1. Save any proof/harness patch to `/tmp/skv10-waveW7-rejected.patch`.
2. Revert W7 proof/harness/microbench changes as one slice.
3. Record REDRESS with observed microbench value versus threshold.

## Pre-Blocked Routes

- REDRESS 28/33 active retained tiny-string NEON wiring remains closed.
- REDRESS 60-62 retained string widening routes remain closed.
- REDRESS 72 global cap-16 policy remains closed.
- Parse-only SOTA admission remains closed.
- W7 cannot combine proof with W9 production wiring.
