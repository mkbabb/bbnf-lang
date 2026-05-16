# SK-V7 W6 R3 - SIMD Key-Run Scan Preflight

Date: 2026-05-16.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: read-only diagnosis for SPEC section 8 Wave 6. No source file was
modified for this report.

## Executive finding

W6 should not start by adding a new key-byte run scan primitive.

The current evidence points to scalar parser-control overhead, not a missing
byte-class primitive. `citm_catalog` and `instruments` are the W6 rows because
their parse profiles invert toward container/key bookkeeping:
`consume_container_next`, `consume_array_next`, `parse_key_colon`,
`consume_structural`, whitespace checks, and offset emits. The byte-scanning
piece of key parsing is already split between the generated tiny-string helper
and the trusted string matcher. W5 owns the remaining scalar tiny-string leaf.
W6 should first compact the existing scalar control/key path, measure citm and
instruments on the current baseline, and only admit a SIMD primitive if that
fresh profile names a true repeated byte-run scan as the hot PC.

If a primitive is used later, it must follow the normal primitive discipline:
new scalar Rust reference, dedicated checkasm parity before wiring, alignment
and tail coverage, AArch64 guarded calls, and a same-wave generated parser
consumer. No checkasm-only kernel, no new substrate, no new BBNF directive, and
no new BIR variant.

## Authority read

- `restart/skinny/tranches/sk-v7/SPEC.md` section 8 scopes W6 to
  `runtime/src/grammars/json/generated.rs` and possibly
  `bbnf-simd/src/aarch64/`, with tasks to profile citm/instruments, optimize
  per-key dispatch, and bench those rows.
- SPEC section 1 forbids new BBNF directives, new BIR variants, and new
  substrate; it also requires scalar reference plus checkasm parity for every
  NEON/ASM primitive before wiring.
- `restart/skinny/tranches/sk-v7/research/skv7-C1-parse-profile.md` classifies
  citm/instruments as the container/key bookkeeping outliers.
- `skinny/crates/bbnf-simd/src/aarch64/` already has 16-byte string special
  masks, 64-byte structural classifiers, movemask, block load, context-shift,
  eob clamp, and primitive dispatch.
- Current primitive tests already establish the expected admission shape:
  scalar oracle, guarded candidate call, alignment/random sweeps, corpus or
  lane coverage, and AArch64-only intrinsic checks.

## Current generated control/key surface

Generated retained parsing has a narrow key-control sequence:

- `parse_key_colon` consumes the key opening quote, tries
  `match_tiny_plain_string`, falls back to `match_string_at_quote`, checks the
  colon with an inline byte check or `skip_json_whitespace`, then skips
  whitespace after the colon
  (`skinny/crates/runtime/src/grammars/json/generated.rs:90`).
- The retained tiny helper is `CAP=16`; direct `SinkOnly` is intentionally
  `CAP=8` (`generated.rs:161`, `generated.rs:166`).
- The retained container exits are scalar control branches:
  `consume_container_next` checks comma/close, optional whitespace, emits the
  closing offset, and returns whether the object loop continues
  (`generated.rs:309`).
- `consume_array_next` repeats the same shape for arrays and also returns the
  next value byte after a comma (`generated.rs:347`).
- `consume_structural` and `consume_delimiter` are scalar byte checks with a
  whitespace fallback (`generated.rs:245`, `generated.rs:291`).

Generated direct and generated typed direct have the same logical control
shape, but with different guard policies:

- `parse_object_direct` parses a string key, calls `sink.key_source`, skips
  whitespace, consumes `:`, parses a value, then checks `,` or `}` with scalar
  `take_direct`/`consume_direct` (`generated.rs:547`).
- Typed direct generation emits `let key = parser.parse_string()?; ... match
  key.as_ref()` for struct fields and map entries
  (`skinny/crates/codegen/src/json_typed_direct.rs:93`,
  `json_typed_direct.rs:329`, `json_typed_direct.rs:346`).
- The generated typed parser runtime already has `tiny_plain_string_end` with
  a 32-byte scalar cap and `skip_plain_string_end` with a 96-byte scalar cap
  (`json_typed_direct.rs:470`, `json_typed_direct.rs:632`,
  `json_typed_direct.rs:646`). This is a separate real-typed surface, not a W6
  retained parse primitive admission.

Important gate ambiguity: SPEC section 8 says citm Track 2 must close, but the
owner path names generated runtime. The independent Track 2 parser has the same
key/colon and container-next scalar shape in `skinny/crates/bbnf-bench/src/track2/json.rs:97`
and `:273`, plus an 8-byte tiny helper at `:316`. If W6's actual gate remains
Track 2, a generated-runtime-only patch cannot close it. Phase 1 should make
the profile target explicit before implementation.

## SIMD inventory relevant to key scanning

Existing AArch64 code is enough for any first-order byte-mask question:

- `string_block::scan_string_special_block_scalar` and
  `scan_string_special_block` return quote, backslash, control, and non-ASCII
  masks for one readable 16-byte block
  (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30`,
  `string_block.rs:57`).
- `classify_tbl4` can classify a 64-byte block against the JSON structural set
  and separately return a terminator mask
  (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:47`,
  `classify_tbl4.rs:89`).
- The runtime scanner already consumes the 64-byte structural classifier in
  `runtime/src/grammars/json/scan.rs:207`.
- `bbnf_simd::prim` exposes general primitive dispatch for
  `byte_class_from_table_64`, `bitmap_prefix_xor_64`,
  `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`, and
  `byte_class_from_eq_set_64` (`skinny/crates/bbnf-simd/src/lib.rs:231`).

A new "key-byte run scan" would need to do more than classify bytes. To beat
the scalar code, it would have to combine: quote/body validation, colon
resolution, whitespace skip, comma/close resolution, and correct offset emit
semantics. That crosses from a byte primitive into parser control. That is
exactly where prior retained parser-control routes were rejected.

## Recommendation

Phase 1 should avoid SIMD and choose a scalar/generated-control compaction
route unless profiling disproves this.

Preferred W6 Phase 1 shape:

1. Profile citm and instruments on the current W6 baseline with
   `parse-attribution`, separating generated retained, hand Track 2, and direct
   rows. Do not transfer W5 or SK-V6 hypotheses.
2. If generated retained is the target, compact the existing scalar control
   sites first:
   `parse_key_colon`, `consume_container_next`, `consume_array_next`,
   `consume_structural`, and their whitespace fallback pattern.
3. Keep generated direct, typed direct, and Track 2 cap policies untouched
   unless their own same-run profile is the target.
4. Bench only citm and instruments for W6 close, with guard rows for the known
   cap/control regressions.

Admissible examples are small scalar changes such as avoiding repeated
length/byte loads around delimiter checks, folding comma/close decisions
without introducing carried parser state, or inlining a local whitespace
result where it is already computed. The line is clear: no carried key quote
state, no side table, no parallel structural cursor, no new tape facts, and no
host/schema semantic facts.

## If a primitive is still proposed

The primitive must be admitted as a new primitive, not as an incidental helper.
Minimum obligations:

- A scalar Rust reference in `bbnf-simd/src/scalar/` or a scalar module beside
  the AArch64 code. It must encode the full observable contract: input window,
  result enum/offsets, EOF/tail behavior, quote/backslash/control precedence,
  whitespace semantics if included, and failure modes.
- Dedicated checkasm file under `skinny/crates/bbnf-simd/tests/`, added to the
  primitive-checkasm gate if that gate is explicit.
- Coverage for alignment `0..64`, EOF/tail guards, quote-before-bad-byte,
  bad-byte-before-quote, colon adjacent to key, whitespace after key, comma vs
  close, invalid control bytes, and non-ASCII trusted-string behavior.
- Candidate calls wrapped with the current checkasm guards
  (`checkasm_common::guarded_call`; AArch64 callee-saved guard where
  applicable).
- Same-wave consumer in the generated parser path named by the W6 profile. A
  primitive that only lands tests or dispatch plumbing is an orphan and must be
  rejected.
- No generic JSON leak beyond the existing JSON runtime/templates. A generic
  primitive may talk in bytes/masks, not "JSON key" semantics.

The likely primitive shape, if one survives profiling, is not a broad "key-run
scanner." It would be a tiny, grammar-neutral byte-window primitive over
already-readable bytes, with the parser still owning state transitions. If the
contract has to return parser states such as "after key colon ready for value,"
it is no longer a primitive-scale change for W6.

## Prior rejected-route risks

Routes that W6 must not relabel:

- Object next-key carry was already measured and rejected. It produced only
  citm +0.36%, instruments -1.06%, random -1.21%, against gates that needed
  real positive lift. Reintroducing carried quote/key state is the same family.
- Raw key byte dispatch for typed fields was rejected inside Candidate 12:
  the scout dropped update_center from 11,537 Mbps to 11,273 Mbps. LLVM's
  string `match` lowering beat a hand-emitted byte if-chain.
- Global cap widening is rejected. Cap 16 is admitted only for generated
  retained; direct `SinkOnly` and Track 2 stay cap 8 because global/direct
  variants regressed instruments, distinct_values, y_string_unicode, and
  Track 2 guard rows.
- Parser-local structural-mask cursors, EventCursor-style prepasses, retained
  projection side tables, and byte-class whitespace cursor routes are rejected
  substrate work. A key-run primitive must not introduce any sidecar or second
  source pass.
- Function-pointer dispatch tables are rejected. Do not make W6 a dispatch
  table rewrite; `skinny/RESULTS.md` still records the alternate dispatch table
  probe as invalid because the real table regressed.
- Direct source hooks, parser-owned decoded scratch, byte-output unescape, and
  semantic string facts are direct-materialization rejects. They are not W6
  control/key compaction.
- Long or delayed-wide retained string scanning is rejected. W6 should not
  compensate for a weak key/control patch by widening the trusted full-string
  scanner.

## Final route statement

W6 Phase 1 can avoid new SIMD. The currently defensible route is a
profile-first scalar compaction of generated parser control/key bookkeeping,
with Track 2/direct surfaces treated as separate targets if the gate names
them. A new key-byte run scan primitive is only admissible after a fresh W6
profile proves that byte-run scanning, not parser control, is the remaining
hot leaf, and then it must ship with scalar reference, checkasm parity, and a
same-wave consumer without adding substrate, directives, or BIR.
