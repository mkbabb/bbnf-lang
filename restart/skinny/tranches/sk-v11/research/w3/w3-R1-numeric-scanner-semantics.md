# SK-V11 W3-R1: Numeric Scanner Semantics

Date: 2026-05-20.
Scope: parse-that numeric scanner semantics for W3 Phase 1.
Output: this file.

## §1 — Findings

- W3 owns `skinny/crates/parse-that-regex/src/number/mod.rs` only for a scalar
  digit-run/span/accumulation oracle that preserves number grammar policy
  (`SPEC.md:441`, `SPEC.md:459`).
- `NumberSpan` is the exact semantic contract: `start`, `end`, `is_integer`,
  `negative`, `digit_count`, `decimal_exp`, `mantissa`, and
  `mantissa_overflow` must remain byte-for-byte stable for the same input
  (`skinny/crates/parse-that-regex/src/number/mod.rs:5`).
- `match_number_span_from_first` accepts an optional sign, then either a single
  `0` or a `1..=9` digit run; leading-zero tails stay outside the span
  (`skinny/crates/parse-that-regex/src/number/mod.rs:38`).
- Fraction and exponent entry are all-or-none: `.` requires at least one
  fractional digit, and `e/E` allows an optional sign but requires at least one
  exponent digit (`skinny/crates/parse-that-regex/src/number/mod.rs:62`,
  `skinny/crates/parse-that-regex/src/number/mod.rs:72`).
- `scan_digit_run` already uses scalar/SWAR 8/4/2 chunk paths before the scalar
  tail, bounded by the 19-digit mantissa budget
  (`skinny/crates/parse-that-regex/src/number/mod.rs:106`).
- Mantissa accumulation records the first 19 digits; after that,
  `mantissa_overflow` changes only when a discarded digit is nonzero
  (`skinny/crates/parse-that-regex/src/number/mod.rs:305`).
- `materialize_i64`, `materialize_u64`, and `materialize_f64` are policy
  boundaries. Integer materializers fall back to checked raw parsing only when
  needed, and f64 uses Eisel-Lemire before `str::parse::<f64>()`
  (`skinny/crates/parse-that-regex/src/number/mod.rs:226`,
  `skinny/crates/parse-that-regex/src/number/mod.rs:247`,
  `skinny/crates/parse-that-regex/src/number/mod.rs:261`).
- Generated direct number consumers depend on `span.end` for cursor movement
  and `is_integer`/`negative` for sink dispatch; span drift is output drift
  (`skinny/crates/runtime/src/grammars/json/generated.rs:652`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:699`).
- REDRESS already validated span-native 8/4/2 digit paths and later rejected
  fallback/mantissa widening because canada attribution found zero mantissa
  overflows and zero `str::parse::<f64>()` fallbacks (`skinny/REDRESS.md:633`,
  `skinny/REDRESS.md:2217`).

## §2 — Recommendations

- Build a scalar `DigitRun`/span oracle only if it exactly reproduces every
  `NumberSpan` field and every `None` result.
- Add fixtures for `0`, `-0`, leading-zero tails, `1.`, `1e`, `1e+`,
  `-123.45e2`, 19-digit limits, 20+ digit nonzero overflow, and 20+ trailing
  zero cases.
- Keep materialization separate from scanning. Falsify on any changed f64 bits,
  integer error class, fallback rate, suffix ownership, or sign/exponent rule.
- Select one or two rows unless CHALLENGE accepts broader same-host microbench
  evidence. W3 row floors are `canada >= 10637`, `mesh >= 8675`,
  `numbers >= 2425`, and `instruments >= 8969` (`SPEC.md:117`).

## §3 — Risks

- Do not reopen numeric fallback/mantissa widening, generic number policy,
  parse-only evidence, or W0 clamp admission (`SPEC.md:485`).
- Do not normalize the current `mantissa_overflow` rule for trailing zeroes.
- Do not move suffix validation into the primitive; full-span validation lives
  at the caller boundary (`skinny/crates/parse-that-regex/src/lib.rs:149`).
- Generic parse-that/codegen changes require CSS/Sheets compatibility evidence
  (`SPEC.md:463`).

## §4 — Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/parse-that-regex/src/number/integer.rs`
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
