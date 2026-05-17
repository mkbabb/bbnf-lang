# SK-V7 W10b R1 - PMULL Prefix-XOR Failure Envelope

Date: 2026-05-16.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: research-only follow-up to W10 REDRESS item 88. No source files were
changed.

## Finding

Default PMULL prefix-XOR is rejected for W10b. The primitive was correct, was
visible in host assembly, and passed checkasm, but it failed the W10 no-row-
regression gate when installed as the default hot AArch64
`bitmap_prefix_xor_64` body.

## Why the Default PMULL Body Failed

The intended W10 route was mechanically valid:

- `restart/skinny/tranches/sk-v7/SPEC.md` section 12 explicitly asked for an
  AArch64 PMULL body for `BITMAP_PREFIX_XOR_64`, scalar references, checkasm
  parity, same-wave scan-path consumer wiring, and no row regressions.
- The existing consumer is narrow but hot: `runtime/src/grammars/json/scan.rs`
  calls `bbnf_simd::prefix_xor_64(real_quotes_fast, in_string)` while resolving
  quote/string-body masks in the AArch64 JSON structural scan.
- `bbnf_simd::prefix_xor_64` dispatches through
  `prim::bitmap_prefix_xor_64`, and AArch64 dispatch selects
  `aarch64::bitmap_prefix_xor_64::bitmap_prefix_xor_64_neon`.
- The rejected implementation computed the low 64 bits of
  `vmull_p64(mask, u64::MAX)` and inverted the result when `carry_in` was true,
  matching the scalar inclusive prefix-XOR contract.

The failure was therefore not semantic. REDRESS item 88 records that the
candidate passed release checkasm for `checkasm_bitmap_prefix_xor_64`,
`checkasm_bitmap_next_set_bit`, `checkasm_bulk_emit_positions_64`,
`checkasm_byte_class_from_eq_set_64`, and `checkasm_parity`; passed
`primitive-checkasm`; passed `cargo test --workspace`; and produced explicit
host assembly evidence for `pmull.1q` and `ctz`.

The failure was performance placement. Prefix-XOR is on the production quote
mask path, not an isolated primitive benchmark. Replacing the scalar shift-XOR
ladder with PMULL as the default body made several parse rows slower enough to
breach W10's "no row regresses" gate. The `simd_scan` Criterion row family was
mostly stable, so the regression appears in the full JSON parse integration
surface where instruction latency/port pressure, branch/carry handling, and the
surrounding quote/escape path matter more than the primitive's standalone
instruction count.

## Rows That Falsified the Candidate

REDRESS item 88 names the coherent final `bench-json --advisory` hard-row
regressions that stopped admission before a refreshed `RESULTS.md` could be
accepted:

| Row | Measurement | Throughput delta |
|---|---:|---:|
| `instruments/track1_generated` | 103.38 us | -4.62% |
| `instruments/track2_handcoded` | 148.04 us | -4.19% |
| `numbers/track1_generated` | 64.465 us | -10.04% |
| `unicode_escapes/track1_generated` | 670.99 us | -12.66% |
| `unicode_escapes/track2_handcoded` | 678.12 us | -15.52% |

These rows falsify default PMULL because W10 required "No row regresses", and
the W10 plan's local comparison threshold allowed at most a small noise-band
drop, not 4-15% losses. The most important rows are `numbers` and
`unicode_escapes`: `numbers` is a must-maintain direct/pass-sensitive row in
the SK-V7 close table, and `unicode_escapes` is already a W4/W10-sensitive
escape-heavy path where prefix/escape interaction is especially hot.

## Remaining PMULL Viability

No default or broadly enabled PMULL consumer remains viable under the current
evidence. The production hot path must keep scalar prefix-XOR unless a new
candidate proves same-row non-regression.

A narrowly gated PMULL consumer is only theoretically viable if all of these
conditions are met in the same wave:

- It is not orphaned: a non-test runtime or generated hot path consumes the
  PMULL body in the same commit series.
- It is not the default `prefix_xor_64` body for all AArch64 JSON scan traffic.
- Its gate is based on fresh same-row measurement, not the old instruction-count
  hypothesis.
- It avoids the falsified row family, or proves those exact rows do not regress:
  at minimum `instruments`, `numbers`, and `unicode_escapes` Track 1/Track 2
  need before/after evidence.
- It does not require parser-shape rewrites, capacity-plan default changes, or
  a synthetic consumer whose only purpose is to make the primitive admissible.

Possible shapes that are not pre-rejected but remain unproven:

- A cold/advisory PMULL helper reachable only from a diagnostic, benchmark, or
  explicitly selected experiment. This is not an admitted primitive body for
  SK-V7 close because it lacks a production same-wave consumer.
- A feature-gated production PMULL path selected by a measured corpus predicate,
  such as a quote-density/escape-density envelope. This would need a same-wave
  hot consumer and direct no-regression proof on the falsifying rows.
- A future fused quote/escape scanner where PMULL is hidden behind a larger
  measured win. That is V8-style fusion work, not a W10b primitive admission,
  and still must respect REDRESS pre-blocks.

Given REDRESS item 88, the W10b admissible shape is: keep prefix-XOR scalar on
the production hot path; retain or re-land B6 canary hardening and the
CSSC/next-bit consumer only if independently measured; do not count PMULL
toward the two admitted W10 primitive bodies unless a narrower measured
consumer clears the same-row gate.

## Routes Pre-Blocked

The following routes must not be used to rescue PMULL:

- Reopening HANDOFF section 3 families: REDRESS 50-55 UTF-8 fusion routes,
  REDRESS 60-72 retained-parse/direct-materialization routes, REDRESS 28+33
  Class A tiny-string wiring, 12-byte token width churn, pair-token fusion,
  function-pointer dispatch-table churn, capacity prescan, generic SWAR
  whitespace, separator elision, raw f64 shortcut, PSI/DTA Rust-codegen
  automaton, or EventCursor parallel prepass.
- Changing parser defaults or capacity-plan behavior solely to manufacture a
  PMULL consumer. W10 R4 found that the parser default does not consume the SIMD
  structural index; the safe W10 consumer was the existing scan function, not a
  parser-shape rewrite.
- Admitting PMULL as checkasm-only. The tranche rules require scalar reference,
  checkasm parity, and a same-wave runtime/generated consumer before a primitive
  ships.
- Treating `simd_scan` stability as sufficient. REDRESS item 88 shows the scan
  bench did not catch the full parse-row regressions; W10b must use full
  `bench-json`/row evidence for any production PMULL path.
- Broad target-feature gating such as "AArch64 + aes" without row evidence.
  The rejected patch already used the AES/PMULL path and still regressed rows.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md` section 12.
- `restart/skinny/tranches/sk-v7/HANDOFF.md` section 3 and W10 close rules.
- `restart/skinny/tranches/sk-v7/research/wave-10-plan.md`.
- `restart/skinny/tranches/sk-v7/research/wave-10-r1-pmull-prefix-xor.md`.
- `restart/skinny/tranches/sk-v7/research/wave-10-r4-consumer-bench.md`.
- `skinny/REDRESS.md` item 88.
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`.
- `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`.
- `skinny/crates/bbnf-simd/src/lib.rs`.
- `skinny/crates/bbnf-simd/src/dispatch.rs`.
- `skinny/crates/runtime/src/grammars/json/scan.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`.
- `/tmp/skv7-wave-10-rejected.patch`.
