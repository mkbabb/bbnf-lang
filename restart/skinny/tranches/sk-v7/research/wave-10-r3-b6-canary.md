# SK-V7 Wave 10 R3 - B6 Stack Canary Stage 1

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: research only; recommended Stage 1 stack-canary XOR-fold hardening for
`bbnf-simd` checkasm tests.

## Findings

1. W10 explicitly combines two admitted `bbnf.asm` body fills with B6 Stage 1
   hardening. SPEC section 12 names the PMULL body for `BITMAP_PREFIX_XOR_64`, the CSSC
   CTZ body for `BITMAP_NEXT_SET_BIT`, scalar references, checkasm parity, a
   same-wave OffsetTape consumer, and "B6 hardening Stage 1: stack canary
   XOR-fold compare" as W10 tasks; the exit gate requires both primitives
   admitted, checkasm green, the same-wave consumer wired, B6 Stage 1 landed, and
   no row regressions. Sources:
   `restart/skinny/tranches/sk-v7/SPEC.md:366`,
   `restart/skinny/tranches/sk-v7/SPEC.md:374`,
   `restart/skinny/tranches/sk-v7/SPEC.md:381`.

2. The SK-V7 handoff makes W10 dependent on the W9 CostFacts substrate and
   closes SK-V7 only when two new bbnf.asm primitive bodies are admitted with
   same-wave consumers. It also preserves the broader primitive discipline: no
   primitive ships without scalar reference, checkasm parity, and same-wave
   consumer. Sources:
   `restart/skinny/tranches/sk-v7/HANDOFF.md:59`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:112`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:127`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:174`.

3. The shared `checkasm_common` wrapper is not currently the silent prefill
   described by the older B6 design note. Today `guarded_call` delegates to
   `stack_canary_then`, which allocates a fixed `[0xDE; 1024]`, snapshots it,
   black-boxes the snapshot, runs the closure, black-boxes the canary, and
   asserts full-array equality. This is already stronger than "no compare", but
   it is not the requested XOR-fold Stage 1 and it has no per-call label.
   Sources:
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:33`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:41`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:46`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:51`,
   `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:32`,
   `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:116`.

4. A central change behind `guarded_call` immediately covers the thin primitive
   tests, including the two W10 primitives. Current `guarded_call` call sites:
   `checkasm_bitmap_prefix_xor_64.rs:18` and `:32`;
   `checkasm_bitmap_next_set_bit.rs:10` and `:24`;
   `checkasm_byte_class_from_table_64.rs:24` and `:43`;
   `checkasm_eob_pad_clamp.rs:13` and `:22`;
   `checkasm_bulk_emit_positions_64.rs:41` and `:56`;
   `checkasm_structural_terminator_64.rs:44` and `:58`.

5. A central `checkasm_common.rs` edit alone does not harden every canary path in
   the explicit `primitive-checkasm` gate. `checkasm_byte_class_from_eq_set_64.rs`
   duplicates `Xorshift64`, `signal_guard`, and a private `stack_clobber_then`;
   that wrapper only read-volatile touches a fixed `[0xDE; 1024]` before and
   after the candidate and never compares the bytes. It is used by the shared
   parity helper and by the twitter corpus loop. Sources:
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:19`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:127`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:131`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:135`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:166`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:359`.

6. `checkasm_parity.rs` has the same private silent canary shape. Its
   `stack_clobber_then` allocates `[0xDE; 1024]`, uses volatile reads before and
   after the closure, and returns without comparing. It wraps the main candidate
   path in `check_parity_at` and the bench candidate path. Sources:
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:175`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:180`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:187`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:213`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:712`.

7. `checkasm_utf8_block.rs` is part of the primitive gate but is currently
   unguarded: `neon_status` calls the AArch64 UTF-8 candidate directly, and
   `unescape_uxxxx_x4_matches_scalar` calls the NEON quartet helper directly.
   This is outside the narrow "current canary wrapper" slice, but it should be
   named as a residual if W10 claims all checkasm targets are canary-hardened.
   Sources:
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:7`,
   `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:61`,
   `skinny/xtask/src/main.rs:292`,
   `skinny/xtask/src/main.rs:302`.

8. The gate is explicit, not just Cargo autodiscovery. `primitive_checkasm`
   iterates a hard-coded list of nine checkasm test targets, sets
   `BBNF_SIMD_STRICT=1`, removes `BBNF_SIMD_INJECT_BUG`, and runs each target in
   release mode. Sources:
   `skinny/xtask/src/main.rs:292`,
   `skinny/xtask/src/main.rs:304`,
   `skinny/xtask/src/main.rs:306`,
   `skinny/xtask/src/main.rs:308`.

9. Register sentinels and raw ABI shims are not Stage 1. The current
   `callee_saved_register_then` is AArch64-only in `checkasm_common`, is a no-op
   on non-AArch64, and REDRESS already records that wrapping arbitrary Rust
   closures with callee-saved sentinels is an unsound checkasm shape because the
   compiler may use those registers inside the closure frame. Sources:
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:55`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:86`,
   `skinny/REDRESS.md:1268`,
   `skinny/REDRESS.md:1272`.

## Recommended Minimal Stage 1 Intervention

Use a precise canonical API name:
`checkasm_common::with_stack_canary_xor_fold(label: &'static str, f: F) -> R`.

Keep compatibility wrappers so the implementation stays small and existing test
call sites do not churn:

- `guarded_call(f)` remains public and calls
  `with_stack_canary_xor_fold("guarded_call", f)`.
- `stack_canary_then(f)` remains public as a compatibility alias and calls
  `with_stack_canary_xor_fold("stack_canary_then", f)`.
- The two private legacy `stack_clobber_then` helpers should either be removed
  and their call sites changed to `checkasm_common::guarded_call`, or retained as
  local aliases that forward to
  `checkasm_common::with_stack_canary_xor_fold("checkasm_parity", f)` and
  `checkasm_common::with_stack_canary_xor_fold("byte_class_from_eq_set_64", f)`.

Implementation shape:

- Generate the 1 KiB canary from `Xorshift64` instead of fixed `0xDE`.
- Compute `pre_fold = canary.iter().copied().fold(0u8, |a, b| a ^ b)`.
- Keep a `before` copy for diagnostics and as a no-regression backstop.
- `black_box(&canary)`, call the candidate, `black_box(&canary)`, then compute
  `post_fold`.
- Fail if `pre_fold != post_fold` or `canary != before`, reporting `label`,
  both folds, and the first divergent byte.

The full-array backstop is intentional. A fold-only check would be weaker than
the current `checkasm_common` full equality and can miss balanced XOR
collisions; the Stage 1 requirement is satisfied by making the XOR fold the
fast explicit guard while retaining exact comparison for collision safety.

Do not expand Stage 1 into signal recovery, cycle counters, x86_64/AArch64 raw
ABI checked-call shims, or register sentinels. The B6 design defers those axes,
and W10 only needs the stack canary XOR-fold slice to close this hardening item.
Sources:
`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:353`,
`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:373`,
`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:390`,
`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:415`.

## Recommended Gate

1. Static wrapper audit after the source patch:

   ```sh
   rg -n "let mut canary = \\[0xDEu8; 1024\\]|let canary = \\[0xDEu8; 1024\\]|read_volatile\\(canary" skinny/crates/bbnf-simd/tests/checkasm_*.rs
   ```

   Expected result: no stale private canary implementation outside the new
   `checkasm_common::with_stack_canary_xor_fold` path.

2. Call-site audit after the source patch:

   ```sh
   rg -n "guarded_call\\(|stack_canary_then\\(|stack_clobber_then\\(" skinny/crates/bbnf-simd/tests/checkasm_*.rs
   rg -n "with_stack_canary_xor_fold" skinny/crates/bbnf-simd/tests/checkasm_*.rs
   ```

   Expected result: every guarded call chain resolves to
   `checkasm_common::with_stack_canary_xor_fold`. Any remaining
   `stack_clobber_then` definition must be a local compatibility forwarder, not
   a canary implementation.

3. Positive gate:

   ```sh
   cargo run -p xtask --release -- primitive-checkasm
   ```

   This runs the explicit strict release list in `skinny/xtask/src/main.rs:292`.

4. Negative canary-reach gate:

   Temporarily inject `canary[0] ^= 1;` immediately after the candidate returns
   inside `with_stack_canary_xor_fold`, then run each guarded target from the
   xtask list directly. Expected failing targets are
   `checkasm_byte_class_from_eq_set_64`, `checkasm_byte_class_from_table_64`,
   `checkasm_bulk_emit_positions_64`, `checkasm_structural_terminator_64`,
   `checkasm_bitmap_prefix_xor_64`, `checkasm_bitmap_next_set_bit`,
   `checkasm_eob_pad_clamp`, and `checkasm_parity`. If Stage 1 also wraps
   `checkasm_utf8_block`, that target must fail under the same injection too.

## Risks and Pre-Blocked Routes

- A fold-only replacement would regress the current shared helper's exact
  byte-for-byte compare. Retain exact comparison as a collision backstop.
- Updating only `checkasm_common.rs` leaves `checkasm_byte_class_from_eq_set_64`
  and `checkasm_parity` on silent private canary wrappers.
- Claiming "all checkasm hardened" while leaving `checkasm_utf8_block` unwrapped
  is too broad. Either keep the claim to "all currently guarded checkasm calls"
  or wrap that file as a separate small follow-up.
- Do not reopen register-sentinel wrapping of Rust closures; REDRESS records
  the false-positive ABI issue. Raw checked-call shims are future work for an
  explicit extern boundary.
- Do not use W10 hardening as permission to admit orphan primitives. REDRESS
  keeps the same-wave-consumer rule non-negotiable and blocks unconsumed bodies
  until real generated/runtime consumers exist. Sources:
  `skinny/REDRESS.md:1243`, `skinny/REDRESS.md:1255`,
  `skinny/REDRESS.md:1263`.
- Do not reopen HANDOFF section 3 pre-blocked retained/direct-materialization routes.
  W9 explicitly records evidence without reopening REDRESS 50-72, REDRESS
  28+33, W5 StringBlock16, W6 object-pair value-byte compaction, or any
  HANDOFF section 3 pre-blocked route. Sources:
  `restart/skinny/tranches/sk-v7/HANDOFF.md:66`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:84`,
  `skinny/REDRESS.md:2502`.
- The current `RESULTS.md` overall state remains `N-direct / NoGo`; this Stage
  1 is a checkasm-hardening precondition, not a performance close by itself.
  Source: `skinny/RESULTS.md:216`.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_eob_pad_clamp.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_structural_terminator_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`
