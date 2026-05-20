# SK-V11 W3-R4: DotProd Micro-Proof Feasibility

Date: 2026-05-20.
Scope: aarch64 SIMD/DotProd/UDOT feasibility for digit run/accumulate.
Output: this file.

## §1 — Findings

- W3 is `C4 + D4, optional UDOT`; it is not UDOT-first (`SPEC.md:194`,
  `SPEC.md:434`, `SPEC.md:459`).
- The current parser number path already uses scalar/SWAR 8/4/2 digit chunking,
  with sign/fraction/exponent policy in `match_number_span_from_first`
  (`skinny/crates/parse-that-regex/src/number/mod.rs:38`,
  `skinny/crates/parse-that-regex/src/number/mod.rs:106`).
- Existing AArch64 DotProd code is a narrow 4-digit helper. `parse_4_digits`
  validates four bytes and calls inline `udot` only under `target_feature =
  "dotprod"` (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5`,
  `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:25`,
  `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:40`).
- Current digit tests are smoke-level, not checkasm-grade digit-span proof:
  `aarch64_primitives` covers `parse_4_digits("2026")`, a non-digit case, and
  `dot4_i8`; `checkasm_parity` only carries an x86 VNNI scalar digit anchor
  (`skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:168`,
  `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:179`,
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:500`).
- S-P2 says `DIGIT_SPAN_UDOT` lacks an admitted scalar UDOT-span oracle today
  and must first write `digit_span_ref(bytes, max)`; proving only a 4-digit MAC
  helper is a reject condition (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:271`).
- UDOT is allowable only behind feature gating, scalar fallback, strict parity
  over boundaries/alignments/lengths/overflow, and a same-wave generated
  direct/typed numeric consumer (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:36`,
  `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:49`).

## §2 — Recommendations

- W3 should be scalar-only unless a plan records a same-host caller microbench
  proving UDOT useful before redress. Isolated `parse_4_digits` is insufficient.
- If UDOT is later attempted, add candidate-specific strict parity and run under
  `BBNF_SIMD_STRICT=1`; require useful row movement with no guard regression.

## §3 — Risks

- Do not move f64 fallback, mantissa widening, leading-zero/sign/exponent,
  suffix, or conversion policy into the primitive (`SPEC.md:474`, `SPEC.md:793`).
- Do not count parse-only numeric evidence or W0-clamped positive deltas as
  admission (`SPEC.md:31`, `SPEC.md:485`).
- Do not reopen W3 substrate/class-column/streaming-cursor/sidecar families
  (`SPEC.md:39`, `SPEC.md:777`).

## §4 — Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
