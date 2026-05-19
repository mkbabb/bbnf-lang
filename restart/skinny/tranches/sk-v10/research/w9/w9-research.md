# SK-V10 W9 Research - Existing-Call-Site Kernel Production

Pass: Wave Research.
Cycle: W9.
Date: 2026-05-19.
Scope: read-only check of whether the W8 C6 proof exposes a legitimate W9
production delta.

## Binding

W9 can consume only the accepted W8 C6 proof: `unescape_uxxxx_x4_neon` in the
current `unescape_string` caller. The wave is direct/typed only and cannot
claim parse-only row movement.

## Current Wiring

The production caller is already wired before W9:

- `skinny/crates/parse-that-regex/src/lib.rs:386` defines
  `unescape_four_unicode_escapes`.
- `skinny/crates/parse-that-regex/src/lib.rs:402` calls
  `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon`.
- `skinny/crates/parse-that-regex/src/lib.rs:718` defines `unescape_string`.
- `skinny/crates/parse-that-regex/src/lib.rs:778` calls the x4 helper from
  `unescape_string` on aarch64.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:558` already routes decoded
  direct strings through `unescape_string`.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1667` and
  `skinny/crates/codegen/src/typed_direct.rs:498` already route generated
  typed decoded strings through `unescape_string`.

The W8 proof measured this already-wired caller. It did not leave an unlanded
production primitive to wire.

## Row Surface

The row-moving direct candidates are the fixed-width Unicode escape rows:

| Corpus | Current Track 1 | Current Track 2 | Floor | State |
|---|---:|---:|---:|---|
| `unicode_escapes` | 5108 | 5083 | 12527 | far below floor |
| `y_string_unicode` | 5020 | 3709 | 8027 | far below floor |

The historical retained Unicode escape production route reached the same
conclusion under REDRESS 64 / SK-V7 W4: `unicode_escapes` improves in isolation,
but direct rows remained far below threshold and `y_string_unicode` regressed or
failed Track 2. That history does not block W8's micro-proof; it does block
pretending the already-wired primitive is a row admission.

## Research Finding

W9 has no admissible same-commit production wiring under the current SPEC. A
source edit that merely wraps or re-gates the already-live call would be a
formal no-op, not an idiomatic production delta. A source edit that changes
direct or typed output semantics would leave the accepted W8 proof's exact
primitive/caller boundary.

The only honest W9 plan is therefore a measured redress rejection: preserve W8
as a proof artifact, run parity and fresh targeted direct measurements for the
eligible rows, record that no `RESULTS.md` row moves, and close W9 as rejected
unless CHALLENGE supplies a legitimate replacement production delta.
