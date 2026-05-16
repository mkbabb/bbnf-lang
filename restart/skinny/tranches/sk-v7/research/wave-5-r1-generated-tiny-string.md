# Wave 5 R1 - Generated Tiny-String Fast Path

## Scope Read

- `restart/skinny/tranches/sk-v7/SPEC.md:247-280` defines W5 as B2 NEON 16-byte plain-string scan widening, specifically through `generated.rs:173`, with same-wave wiring and a hard guard against the refuted V5 W3 validation-fold family.
- `restart/skinny/tranches/sk-v7/HANDOFF.md:66-83` pre-blocks the SK-V5 UTF-8 fusion routes, SK-V6 retained/direct-materialization routes, and REDRESS 28+33 Class A tiny-string wiring as a parse-G fix. `HANDOFF.md:174-176` repeats that no primitive ships without scalar reference, checkasm parity, and same-wave consumer.
- The owner path is `skinny/crates/runtime/src/grammars/json/generated.rs:90-103`, `:142-182`, and `:609-640`.

## Current Generated Fast Path

- Retained generated parse has two hot consumers before the full string matcher: object keys in `parse_key_colon` call `match_tiny_plain_string(state.bytes, start)` at `skinny/crates/runtime/src/grammars/json/generated.rs:90-103`, and string values do the same at `:142-156`.
- `match_tiny_plain_string` is only a wrapper to `match_tiny_plain_string_with_cap::<16>` at `generated.rs:159-163`. The current body at `generated.rs:171-182` is a scalar byte loop bounded by `let limit = (cursor + CAP).min(input.len())`; it returns `Some(close + 1)` only when a closing quote appears before any backslash/control byte, returns `None` on `b'\\' | 0x00..=0x1f`, and returns `None` when the cap is exhausted.
- Direct generated `SinkOnly` intentionally remains separate: `match_tiny_plain_string_direct` uses `CAP=8` at `generated.rs:165-168`, and `parse_string_direct` consumes it at `generated.rs:609-640`.
- Non-ASCII bytes are not special in this tiny path. That matches the trusted-UTF-8 contract: the parser input is already `&str`, and the fallback full matcher is `match_json_string_at_quote_trusted_utf8` at `generated.rs:186-197`.

## CAP=16 Versus CAP=8

- The `CAP` distinction is semantic policy, not just a tunable constant. REDRESS 72 admitted `CAP=16` only for generated retained `OffsetTape` parsing and explicitly restored `CAP=8` for generated direct `SinkOnly`, hand retained Track 2, and hand direct Track 2 after direct/guard regressions (`skinny/REDRESS.md:2045-2053`).
- Therefore W5 must not become a global tiny-string cap change. The same-wave consumer is the retained generated path at `generated.rs:95` and `generated.rs:147`; the direct path at `generated.rs:615` is a guard surface and should keep the `CAP=8` behavior unless a separate falsifier overturns REDRESS 72.

## Same-Wave Consumer

- A SIMD primitive or checkasm-only admit is insufficient. W5's consumer must be the generated retained parser path named by SPEC: `match_tiny_plain_string_with_cap::<16>` at `generated.rs:171-182`, reached from `parse_key_colon` and `parse_string`.
- `bbnf-simd::aarch64::string_block::scan_string_special_block` already provides a 16-byte quote/backslash/control scanner with scalar reference (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30-72`) and checkasm parity coverage (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:623-645`). If reused, the consumer must use `terminator_mask | escape_mask | control_mask`, not `interesting_mask`, because `non_ascii_mask` is allowed in trusted strings.
- The older low-6 table `match_tiny_plain_string_neon` primitive is parity-green but was invalidated as the parse-G fix by REDRESS 33 (`skinny/REDRESS.md:394-413`). W5 should not relabel that route as the consumer unless the generated retained call path above is the measured beneficiary.

## Refuted Route Boundaries

- Do not edit `parse-that-regex` for the primary W5 route. SPEC cites `parse-that-regex/src/lib.rs:295-347` and a `0x80` early-exit around line 331, but in this checkout `line 331` is the post-byte caller loop inside `match_json_string_at_quote_trusted_utf8` (`skinny/crates/parse-that-regex/src/lib.rs:298-333`).
- The actual long trusted fallback already has an AArch64 16-byte block scan at `parse-that-regex/src/lib.rs:679-705`; it is reached only after the tiny generated fast path misses. The validating string scan and UTF-8 fold live in `skip_json_string_plain` at `parse-that-regex/src/lib.rs:593-676`, with high-byte validation branches at `:621-637`, `:656-669`, and `validate_utf8_prefix` at `:734-759`.
- Touching those validation/fallback paths would reopen the SK-V5 W3 UTF-8 fusion family blocked by `HANDOFF.md:71-83` and summarized in `skinny/REDRESS.md:1331-1342`. W5 also must not widen Unicode escape validation into a four-quartet contiguous route; SPEC calls that REDRESS 64 boundary out at `SPEC.md:256-259`.

## Falsifiable Implementation Implication

An admissible W5 patch should be mechanically falsifiable as follows: on AArch64, only the `CAP=16` retained generated helper may take a 16-byte block path when `offset + 1 + 16 <= input.len()`. It should compute the first quote/backslash/control lane in that block; return `Some(cursor + lane + 1)` only when the first special is `b'"'`; return `None` for first `b'\\'`, first control byte, no special within 16 bytes, or insufficient bytes after falling back to the scalar tail. `CAP=8` direct behavior and `parse-that-regex` should be byte-identical. A failing implementation would be easy to spot: `rg` would show `match_tiny_plain_string_direct` no longer routes through `<8>`, `parse-that-regex/src/lib.rs` would change, or W5 profiling would not move at least 4 of the 6 SPEC rows without a >=3% regression (`SPEC.md:264-277`).
