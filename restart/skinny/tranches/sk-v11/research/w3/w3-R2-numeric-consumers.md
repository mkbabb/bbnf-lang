# SK-V11 W3-R2: Numeric Consumers

Date: 2026-05-20.
Scope: generated runtime and bench consumers for numeric direct rows.
Output: this file.

## §1 — Findings

- W3 targets `C4` digit span/accumulation, `number_span_emit_slot`, and
  `pt_digit_run_span_accumulate`; owner paths include generated JSON runtime,
  `direct_struct.rs`, generated typed consumers, and `json_parity.rs`
  (`SPEC.md:434`, `SPEC.md:436`, `SPEC.md:441`).
- Generated JSON direct has a clear numeric choke point. Root, object, and
  array paths call `match_number_span_from_first`, advance to `span.end`, and
  emit through `JsonSink` (`skinny/crates/runtime/src/grammars/json/generated.rs:645`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:667`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:682`).
- `emit_number_*` dispatches to i64, u64, or f64 sink slots and preserves `-0.0`
  handling (`skinny/crates/runtime/src/grammars/json/generated.rs:696`).
- Track 1 direct-to-struct consumes generated `parse_direct`, while the sink has
  scalar, array, and object number slots
  (`skinny/crates/bbnf-bench/src/direct_struct.rs:259`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:316`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:346`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:376`).
- Track 2 is useful as a throughput/oracle backstop, but it also uses
  `match_number_span_from_first`; numeric policy independence therefore needs
  serde/sonic and exact number-class parity in the gate
  (`skinny/crates/bbnf-bench/src/direct_struct.rs:578`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:89`).
- `mesh/real_typed_struct` is a strong typed numeric guard consumer: generated
  typed vector loops repeatedly call `parse_f64()` and `parse_u32()`
  (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:86`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs:796`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs:827`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1343`).
- Typed numeric materialization is centralized in `number_span()`, which calls
  `match_number_span_from_first` and advances the cursor
  (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1703`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1723`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1729`).
- `json_parity.rs` already checks parse, direct-to-struct, and real typed
  parity before measuring rows (`skinny/crates/bbnf-bench/benches/json_parity.rs:15`,
  `skinny/crates/bbnf-bench/benches/json_parity.rs:181`,
  `skinny/crates/bbnf-bench/benches/json_parity.rs:261`).

## §2 — Recommendations

- First target: generated runtime `parse_number_*` plus `emit_number_*` for
  `direct_to_struct`, selecting one or two rows from `mesh`, `canada`,
  `numbers`, and `instruments`.
- Use `mesh/real_typed_struct` as a typed guard consumer if the redress touches
  generated typed numeric helpers.
- Keep the primitive span-first: return existing span/cursor facts, and leave
  f64 conversion and overflow behavior in existing materializers.

## §3 — Risks

- `numbers` and `instruments` are W0-clamped; passing current floors is not
  admission without measured behavior provenance (`SPEC.md:31`).
- A primitive shared by Track 1 and hand Track 2 weakens scanner-policy
  independence; retain serde/sonic parity and exact number class tests.
- Do not reopen fallback/mantissa widening, generic number policy, parse-only
  numeric evidence, or W0-clamp admission (`SPEC.md:485`).

## §4 — Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
