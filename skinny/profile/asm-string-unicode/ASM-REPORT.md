# ASM attribution — string / Unicode / escape paths

Companion to the earlier `profile/ASM-REPORT.md` (which covered the tape
dispatch + structural-scan core). This pass targets the **string
recognizer, the regex DFA myth, and the unescape + surrogate-pair
machinery** — the components that the structural-index-driven codegen
template will replace or repoint.

All assembly produced with `cargo asm -p <crate> --lib <sym>` against
`profile.release` (`opt-level=3, lto=thin, codegen-units=1, debug=true`).
ARM64 / Apple darwin. Each raw dump is the named symbol plus subsequent
symbols in source order; `.trimmed.s` files slice each dump to its own
body.

`[N]` from cargo-asm's listing is the whole-block instruction tally
including prologue, `.loh` macro markers, and `.cfi` directives.
The `insns` column below counts only actual mnemonic lines
(`^\t[a-z][a-z0-9.]*` with whitespace following).

## (a) Per-function metrics

| Function | lines | insns | cond_br | uncond_b | calls (bl) | indirect | bounds | misc-panic |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `runtime::generated_json::generated::parse_value` | 882 | 688 | 122 | 33 | 19 | 0 | 1 | 6 |
| `runtime::generated_json::generated::parse_string` | 226 | 160 | 21 | 8 | 6 | 0 | 1 | 2 |
| `runtime::generated_json::generated::parse_literal` | 131 | 81 | 7 | 1 | 5 | 0 | 0 | 2 |
| `runtime::generated_json::generated::consume_structural` | 138 | 96 | 16 | 3 | 2 | 0 | 0 | 1 |
| `<JsonNodeKind>::at_cursor` | 101 | 61 | 4 | 9 | 3 | **1 (br)** | 1 | 2 |
| `parse_that_regex::match_json_string` | 138 | 113 | 21 | 2 | 0 | 0 | 0 | 0 |
| `parse_that_regex::unescape_json_string` | 565 | 453 | 62 | 38 | 20 | 0 | 0 | 2 |
| `parse_that_regex::classify_json_string_content` | 102 | 69 | 8 | 3 | 1 | 0 | 0 | 1 |
| `parse_that_regex::read_hex_unit` | 152 | 115 | 20 | 5 | 2 | 0 | 0 | 2 |

`uncond_b` = `b LBBxx` (intra-function jumps to merge points / epilogue
trampolines). `cond_br` = `b.<cc>`, `cbz`, `cbnz`, `tbz`, `tbnz`. `bl`
counts subroutine calls; `blr`/`br` are indirect.

**Cross-pass parity:** `parse_value` (688 insns), `parse_string` (160),
`parse_literal` (81), `at_cursor` (61) match the prior pass within
±2 mnemonics (rounding around prologue ordering changes).

## (a.1) parse_string no-escape happy path

Source bytes (parse_string.trimmed.s):

- L23-26 — load state, call `consume_structural(state, b'"')`. **One
  external call.** consume_structural itself is the hot subroutine
  (96 insns, 16 cond branches, 2 inline whitespace-skip loops).
- L28-31 — load `state.structural_offsets.len()` and
  `state.structural_cursor`, compare; jump to recovery if cursor past
  end.
- L32-41 — `sync_structural` inline: 5-instruction loop body
  (`ldr w12, [x10, x8, lsl #2]; cmp; b.ls; add; str; cmp; b.ne`).
  This is the **2nd whitespace-skip-style index walk** parse_string
  contains (consume_structural already walked the same offsets).
- L48-67 — `state.structural_offsets[cursor]` and `bytes[offset]==b'"'`
  check; advances structural_cursor.
- L62-76 — `emit_offset` inline (tape push: cap check, `RawVec::grow_one`
  cold call, store, len += 1, cursor = close+1).
- L77-91 — `string_control_offsets` scan loop (4 insns/iter, same shape
  as sync_structural). **3rd index walk.**
- L117-128 — `string_escape_offsets` scan loop **+ branch**: if no
  escape offset within `[content_start, content_end)`, `b.ls LBB52_22`
  takes us to the happy-path emit at L168-174 (which actually re-emits
  state via the same shared epilogue stub).

Counting only mnemonics actually executed on the no-escape path
(LBB52_3 single-iter + LBB52_6 + LBB52_12 + LBB52_14 single-iter +
LBB52_22 + LBB52_30 + epilogue):

- inline body: **~52 instructions**
- one external call to `consume_structural` (96 insns ≈ 30 retired w/
  one-byte structural and direct cursor hit) = **~30 instructions**
- one `RawVec::grow_one` check (taken/not taken — assume not taken
  steady-state) = **~6 instructions**
- per-call epilogue/prologue (parse_string itself): **~22 instructions**

Total cold per-byte equivalent for a short ascii string: **~110
mnemonics**, dominated by three independent `structural_offsets` walks
(consume_structural's sync, parse_string's sync, parse_string's
control/escape index walks). **sonic-rs's `parse_string_inplace`
happy-path is ~22 NEON-aided instructions for a 16-byte aligned chunk
(one load, one vceqq, one vcltq, one movemask, three branches, one
store).** Skinny is doing ~5x the work because the structural index
forces a parallel `cursor` reconciliation on every string.

## (a.2) match_json_string is not a DFA

The function name suggests a regex DFA but the body
(`match_json_string.trimmed.s` L42-52) is a hand-written scalar loop:

```
LBB2_8:
        add x15, x0, x13     ; ptr = base + cursor
        ldrb w14, [x15]      ; load byte
        cmp w14, #92         ; vs '\\'
        b.eq LBB2_12
        cmp w14, #34         ; vs '"'
        b.eq LBB2_28
        cmp w14, #32         ; vs 0x20 (control byte)
        b.lo LBB2_29
        add x14, x13, #1
        b LBB2_7             ; tail jump → cursor advance
```

**Per loop iter: 7 mnemonics, 3 conditional branches, no bounds check,
no panic edge.** The function has zero `bl` (no calls at all) and zero
indirect dispatch. There is **no DFA, no NFA, no VM, no transition
table**. The escape branch (LBB2_12) reads the next byte and
dispatches on a precomputed bitmask (`mov w12, #4113; movk #5, lsl #16`
encodes the set of valid escape chars `b/f/n/r/t/"/\/`).
For `\u` it unrolls 4 hex-digit checks (LBB2_19-23). Surrogate-pair
decoding is **not** in this function — it lives in
`unescape_json_string` because match_json_string only reports
`needs_unescape` and trusts the index.

This refutes "regex DFA call carries per-character bounds checks?" —
**no, there is no DFA**, and bounds is one cmp+b.hs against the end
pointer per byte, with the failure path going to a result struct, not
a `panic_bounds_check` call.

## (a.3) unescape_json_string surrogate-pair cycles

`unescape_json_string.trimmed.s` L34-50: input >15 bytes hits the
`memchr_aligned` pre-scan to locate the first `\`. Below 15 bytes a
scalar loop (L39-44) does the same. **No allocation occurs unless
`\` is found.**

Surrogate pair flow (single `𝄞` → U+1D11E):

1. Top of loop at LBB3_14 — byte read, hit `\` branch (L92-93).
2. L107-113 — `u` dispatch falls to `read_hex_unit` (call) which
   parses 4 hex digits (read_hex_unit.trimmed.s is 115 insns, ~25
   retired in the happy case: 4 hex-digit validate-and-shift unrolled
   plus 1 bounds cmp).
3. L114-126 — first u16 returned; mask & 0xfc00 == 0xD800 detects
   high surrogate, branches to LBB3_71.
4. L319-348 — verifies next two bytes are `\u`, second `read_hex_unit`
   call, mask & 0xfc00 == 0xDC00, computes
   `0x10000 + ((first - 0xd800) << 10 | (second - 0xdc00))` via
   `add w8, w8, w25, lsl #10; add w25, w8, w9`.
5. LBB3_79 onwards — runs through the UTF-8 encode pipeline
   (LBB3_64-69: cset width to 1/2/3/4, bfxil/orr to produce continuation
   bytes, strb pairs).

Steady-state per-surrogate-pair after the leading memchr/alloc:
~85 mnemonics (two read_hex_unit calls of ~40 each via call+return,
plus ~5 for the merge, plus ~40 for UTF-8 encoding). The two
`read_hex_unit` calls cannot be inlined because the function was kept
out-of-line — its address-of-PC is taken indirectly through `bl`. The
call/return pair is the largest single cost: **the two BLs alone are
~10 cycles each on M-series cores** plus the prologue spill/reload
saving 4 calleesaved regs.

In total: ~85 retired insns + 20 cycles of call overhead =
**conservatively 80-100 cycles per surrogate pair**, dominated by call
overhead and the cset/bfxil/orr encoder. Comparable to sonic-rs's
`parse_escaped_unicode` (~70 cycles) but skinny pays the read_hex_unit
call cost twice.

## (b) Inlined functions

These appeared in source as targets of the spec or in source-file
function declarations, but produced no top-level symbol under
`cargo asm` for parse-that-regex:

- `parse_that_regex::match_json_number` — fully inlined into
  parse_number / parse_value
- `parse_that_regex::skip_json_whitespace` — fully inlined into
  parse_value (the `9728` whitespace bitmask seen at
  parse_value.trimmed.s L29-30 and consume_structural.trimmed.s L52-53)
- `parse_that_regex::validate_json_string` / `validate_json_number` —
  validation wrappers; never called from the hot json pipeline
- `parse_that_regex::is_high_surrogate` / `is_low_surrogate` /
  `hex_value` — folded into unescape_json_string
- `parse_that_regex::neon_classify_json_string_content` — fully
  inlined into `classify_json_string_content`; the NEON `vld1q_u8 /
  vceqq / movemask` triple is visible in classify_json_string_content.s
  (NEON 16-byte chunked loop with scalar tail fall-through)

Runtime side, generated.rs:

- `parse_json` (outer entry), `parse_object`, `parse_array`,
  `parse_pair`, `parse_number`, `skip_ws`, `peek`, `consume`,
  `sync_structural`, `contains_indexed_offset`, `error` — all fully
  inlined into `parse_value` / `parse_string`.

That explains parse_value's bulk (688 insns): it has inlined three
whitespace skip loops, three structural-index walks, two
`contains_indexed_offset` scans, two recursive call dispatchers,
plus the literal/number tail bodies.

## (c) Bounds-check / panic-call density

Every count is a unique `bl` to a panic helper.

| function | `panic_bounds_check` | `unwrap_failed` | `expect_failed` | `panic_fmt` | other |
|---|---:|---:|---:|---:|---:|
| `parse_value` | 1 | 6 | 0 | 0 | 0 |
| `parse_string` | 1 | 2 | 0 | 0 | 0 |
| `parse_literal` | 0 | 2 | 0 | 0 | 0 |
| `consume_structural` | 0 | 1 | 0 | 0 | 0 |
| `at_cursor` | 1 | 0 | 1 | 1 | 0 |
| `match_json_string` | 0 | 0 | 0 | 0 | 0 |
| `unescape_json_string` | 0 | 0 | 0 | 0 | `slice_error_fail` 1, `handle_error` 1 |
| `classify_json_string_content` | 0 | 0 | 0 | 0 | `slice_index_fail` 1 |
| `read_hex_unit` | 0 | 0 | 0 | 2 | 0 |

`parse_value` carries **6 `unwrap_failed` sites** — one per
`u32::try_from(usize).expect("input offset fits u32")` in the
generated dispatch (one per branch + recursive sites). LLVM cannot
elide them because there is no input-length precondition on the
generated function. The single `panic_bounds_check` is on
`state.structural_offsets[cursor]` in the recovery path.

`parse_string` has **2 `unwrap_failed`** for the offset narrowing
and **1 `panic_bounds_check`** for `structural_offsets[cursor]`.

`match_json_string` is the **only string-side hot function with
zero panic edges** — failure goes through the result-struct return
path. This is the right shape; everything else needs to imitate it.

## (d) Indirect calls / dispatch tables

- `parse_value`: 0 indirect. **Dispatch is a cmp/b.eq chain** (lines
  73-94 of parse_value.trimmed.s) — `<= 'e' → number/string/array
  group → '\"' / '-' / '['`, `> 's' → 't' / true literal`, etc. This
  is not a jump table; the compiler chose binary search over the
  7-way switch. Predictable but each branch consumes a slot in the
  branch predictor.
- `at_cursor`: **1 `br x11`** — true jump table indexed by `byte -
  0x22`. Each arm is a 2-instruction stub (`mov w0, #N; b LBB16_15`)
  that loads the constant `JsonNodeKind` discriminant. This is the
  *only* indirect dispatch in the entire json hot path. The table is
  at `LJTI16_0` and contains 9 entries packed as bytes.
- All other functions: 0 indirect. The recursive `parse_value` calls
  are direct `bl runtime::generated_json::generated::parse_value`.

There is no `blr` (register-indirect call) anywhere in the hot path —
no vtable, no dyn Trait, no enum-via-pointer.

## (e) UTF-8 validation site

**There is no eager UTF-8 validation in the hot json pipeline.**

- `simdutf8` is not a dependency of skinny (`grep -r simdutf8 crates/`
  returns nothing).
- `core::str::converts::from_utf8` is called only from
  `parse_literal.trimmed.s` L38-41 — and only on the **error path**
  when the literal memcmp fails, used to build the error display
  string. Happy path skips it entirely.
- `core::str::slice_error_fail` appears in `unescape_json_string`
  (L373 of source) when the slice indexing in
  `raw_content[cursor..].chars().next()` straddles a UTF-8 boundary
  badly — this is the inner `chars()` fallback for non-ASCII bytes
  inside an unescaped string. It is reachable only when the input
  contains non-ASCII content and is not pure ASCII or `\u`-escaped.
- `view.rs` defers `str::from_utf8` to read time (`runtime::view::
  string` per the source). The parse never validates; the cursor
  pass simply records offsets and trusts the bytes.

Practical consequence: skinny is **strictly faster than sonic-rs for
ASCII-only inputs** since sonic-rs validates UTF-8 inline, but
**deferred-UTF-8 is a correctness wart**: a malformed UTF-8 string
will parse without error and only blow up on `view::string()`. Any
downstream consumer must either re-validate or call `from_utf8` on
materialization.

## (f) Suspicious top-5 functions

1. **`parse_value` (688 insns, 122 cond branches, 6 unwrap_failed)** —
   structural dispatch is a cmp/b.eq cascade rather than a tight jump
   table, three inline whitespace-skip loops, and six redundant
   `u32::try_from` narrowing panics. The unwrap_failed sites alone
   pin 18 instructions of cold-code outline per site (PAGE/PAGEOFF
   adrp/add quartets). The structural-index codegen template should
   collapse the cascade into a single jump-table dispatch keyed by
   the structural-byte map (the alphabet is `b"{}[],:\""` so 6 entries
   plus default), eliminate the redundant per-branch try_from by
   threading the `start` value through the structural-index emission
   (which is already u32-typed at the index level), and inline the
   3 whitespace skips into one pre-loop epilogue. Conservative
   target: **~250 insns, 40 cond branches, 0 try_from panics.**

2. **`parse_string` (160 insns, 21 cond branches, 3 panic edges)** —
   redundant: calls `consume_structural` (which has its own
   sync_structural loop), then runs sync_structural inline a second
   time (LBB52_3), then walks `string_control_offsets` (3rd index
   walk), then walks `string_escape_offsets` (4th index walk). The
   structural-index codegen template should fold all four walks into
   a single pass that emits a tagged span when the structural cursor
   passes a `"`-terminator. Cursor reconciliation is the elephant in
   the room — sonic-rs avoids this by *being* the cursor.

3. **`consume_structural` (96 insns, 16 cond branches)** — called 6×
   per parse_value invocation. Carries two inline whitespace skip
   loops (the second only fires when state.cursor != structural_offset
   AND skip_whitespace didn't bridge the gap — a defensive double
   check that the index invariant should make unreachable). Either
   prove the post-skip equality at codegen time or strip the second
   loop entirely.

4. **`unescape_json_string` (453 insns, 62 cond branches, 20 calls)** —
   the elephant. 8 cold `__rust_alloc` / `RawVecInner::reserve` call
   sites (one per escape kind: `\"`, `\\`, `\/`, `\b`, `\f`, `\n`,
   `\r`, `\t`), each carrying its own `bl` + clobber-save frame. LLVM
   refused to fold them into a single hot allocation-check trampoline.
   Either pre-size `String::with_capacity(content_len)` (already
   present in source — but LLVM doesn't prove the steady-state) or
   build a single owned-buffer reserve at the top and remove the
   per-escape-kind allocator interaction.

5. **`read_hex_unit` (115 insns, 20 cond branches, 2 calls)** — the
   function is called twice per surrogate pair and once per `\u`
   escape. **It is not inlined despite having no recursion and a
   short body.** Two `panic_fmt` cold sites for the
   `unreachable!("caller validates hex digits")` in `hex_value`. The
   right shape is `#[inline]` plus a debug_assert; the
   `unreachable_unchecked` arm should be promoted to the release
   profile to elide the two panic edges.

## (g) Issues the structural-index-driven codegen template must
##     address beyond what the prior ASM-REPORT already documented

The earlier report (under `profile/ASM-REPORT.md`) covered the tape
emission, the simd-scan inner loops, and the per-function panic-edge
toll. This pass surfaces five **string- and Unicode-specific** items
that the prior pass did not call out:

1. **`match_json_string` is the only correctly-shaped recognizer
   in the json pipeline.** Zero calls, zero panic edges, single
   bounds variable, exit via tagged result struct. The structural
   codegen template should emit *all* recognizers in this shape —
   `parse_string` and `parse_literal` both diverge from it (1 + 3
   panic edges).

2. **Triple-walk redundancy on `structural_offsets`.** parse_string
   walks the array three times (consume_structural's sync, its own
   sync, control-byte scan, escape-byte scan). The codegen template
   must materialize a single fused index pass: structural cursor,
   control cursor, escape cursor advance together against the same
   array, branching only on the kind tag.

3. **The dispatch in parse_value is a cmp/b.eq cascade, not a
   jump table.** Despite the structural alphabet being only 7 bytes
   wide, LLVM emitted a 7-way branch chain because the source uses
   `match peek(state)` with character-class patterns
   (`b'-' | b'0'..=b'9'`). A direct lookup table — possibly reusing
   the same structural alphabet table that scan_dispatch uses —
   would cut the worst-case branch-predictor pressure from 7 to 1.
   `at_cursor` already does this and is the *only* indirect dispatch
   in the entire json hot path, proving the template can emit it.

4. **There is no UTF-8 validation.** All sonic-rs / simd-json
   comparators run inline UTF-8 validation. Skinny defers it to view
   time. This is faster but unsound for downstream consumers that
   don't re-validate. The structural codegen template either needs to
   commit to "string spans are bytes, caller validates" (current
   stance, but must be documented at the API level) or fold a NEON
   UTF-8 validator into the structural scan. The neon
   `classify_json_string_content` already has the NEON harness; a
   sibling `validate_utf8_in_string_span` would slot in at the same
   call site for ~5 NEON instructions per 16-byte chunk.

5. **`unescape_json_string` has 8 redundant allocator call sites.**
   Each single-byte escape (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`,
   `\t`) gets its own `RawVecInner::reserve::do_reserve_and_handle`
   call site, each with its own 12-instruction prologue and clobbered
   register set. Either the source needs to write to a pre-grown
   buffer (a `Vec::with_capacity(content_len + 16)` and `unsafe
   ptr::write` on the tail) or the codegen template needs to emit
   a single grow-trampoline that handles all escape kinds. Either
   way, the current 8-per-function allocator surface is more than
   8% of the function's total instruction count.

6. **`read_hex_unit` is the wrong inline boundary.** It's called twice
   for surrogate pairs and once per `\u` escape but stays out-of-line.
   At 115 instructions and two `panic_fmt` cold sites, marking it
   `#[inline]` and using `unreachable_unchecked` for the hex_value
   `_ => unreachable!()` arm would eliminate ~30 cycles per
   surrogate pair.

## File map

```
profile/asm-string-unicode/
├── ASM-REPORT.md                              (this file)
├── at_cursor.s / at_cursor.trimmed.s
├── classify_json_string_content.s / .trimmed.s
├── consume_structural.s / .trimmed.s
├── match_json_string.s / .trimmed.s
├── parse_literal.s / .trimmed.s
├── parse_string.s / .trimmed.s
├── parse_value.s / .trimmed.s
├── read_hex_unit.s / .trimmed.s
└── unescape_json_string.s / .trimmed.s
```

`.s` files are the raw cargo-asm output (starting at the requested
symbol, trailing into subsequent symbols in source order). `.trimmed.s`
files are sliced to the body of the named symbol; metric tables above
use the trimmed forms.
