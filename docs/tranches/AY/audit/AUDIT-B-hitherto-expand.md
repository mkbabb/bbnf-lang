# AUDIT-B — Expand-begotten truth for B0 + AY.W5 + AY.W6

Audit Agent B, dispatched 2026-04-20 under the mid-AY triumvirate
pause. Truth source: `cargo expand` artefacts generated fresh in
this worktree under `/Users/mkbabb/Programming/bbnf-wt-ay-audit-hitherto/target/expand/`:

- `ay-json.rs` — 6 186 lines, from `make ay-expand-json` / `cargo
  expand -p bbnf --bench json_monolithic`.
- `ay-named-type.rs` — 259 721 lines, from `cargo expand -p bbnf
  --test named_type_preservation`.
- `ay-css-l4.rs` — 206 412 lines, from `cargo expand -p bbnf
  --bench css_l4` (dispatched for the Pratt surface only).

The bench binary re-runs `#[derive(Parser)]` on compile, so the
expansion reflects the current state of the emitter at master HEAD
`b346ebca` regardless of the reverted `generated.rs` state (cf.
AY/PROGRESS §"W6 → bootstrap regen outcome").

`cargo expand` expands only the bench/test crate against its
proc-macro output. Cross-crate fn bodies in `crates/tape/src/`
(e.g. `note_push`, `OpenFrame`) remain opaque — their presence has
to be inferred from source + the observable per-push cost their
`#[inline(always)]` directives promise LTO.

---

## Surface 1 — `TapeBuilder::open_compound` / `close_compound`
(AY.W5.a.2, commit `feffe271`)

**Claim.** `TapeBuilder::open_compound` + `close_compound` land
as the write-time close-stamping API. The JSON hot path invokes
them on every object + array Shape-1 compound; the open-frame
stack stays empty on emitters that don't use the pair.

**Expand truth.** `ay-json.rs` contains exactly **17
`open_compound`** call sites and **25 `close_compound`** sites
(asymmetric because several close paths exist per open — fast
return vs loop exit). Every site is inside either
`parse_array_JsonParser_array` (lines 1404-1554) or
`parse_object_JsonParser_object` (lines 1656-1844). Sample
(ay-json.rs):

```
1404:        let outer_off = builder.open_compound(TapeKind::Seq, span_lo, 4u8, 0);
1405:           Seq, lbracket_open, 0, 0);
1408:           Seq, opt_ws_open, 0, 0);
1422:           Seq, repeat_open, 0, 0);
1426:           Rule, ...);
1448:        builder.close_compound(outer_off, outer_close);
```

Source: `crates/tape/src/builder.rs:388-492` defines both
functions with `#[inline(always)]`; `OpenFrame` struct at
`builder.rs:167-177`; `open_stack: Vec<OpenFrame>` field at
`builder.rs:150`.

**Delta.** None. Substrate landed, consumed on the hot path, test
proof at `crates/core/tests/w5_close_stamp_activation.rs:107-140`
asserts every non-root record in a fixture tape built via
open/close carries `SIB_SKIP_STAMPED_BIT`.

**Verdict. SOUND.**

---

## Surface 2 — `SIB_SKIP_STAMPED_BIT` activation
(AY.W5.a.1, commit `2d420e9e`)

**Claim.** `TapeRec::SIB_SKIP_STAMPED_BIT = 0x0020` marks records
whose `sib_skip` was stamped at write time. The stage-C finaliser
skips re-derivation on stamped records.

**Expand truth.** `ay-json.rs` contains 0 textual occurrences of
`SIB_SKIP_STAMPED_BIT` — as expected, the bit lives inside
`push_leaf` / `push_compound` / `open_compound` / `close_compound`
/ `note_push`, all of which live in `crates/tape/src/builder.rs`
and are only textually visible in `cargo expand` of the tape
crate itself (not the bench crate). The **runtime effect is
observable** at `crates/tape/src/finaliser.rs:300-304`:

```rust
if let Some(prev) = prev_at_depth[d] {
    if (columns.extra_at(prev) & TapeRec::SIB_SKIP_STAMPED_BIT) == 0 {
        columns.set_sib_skip_at(prev, i_u32 - prev);
    }
}
```

The finaliser reads the bit and suppresses re-derivation when set.
Constant defined at `crates/tape/src/tape.rs:200`
(`pub const SIB_SKIP_STAMPED_BIT: u16 = 0x0020;`). Write sites:
`builder.rs:242` (`note_push`, hot-path stamp on every direct
child except the last), `builder.rs:490` (`close_compound`,
stamps the last direct child).

**Delta.** None visible — the bit's wiring is correct end-to-end.
One runtime concern deferred to surfaces 3 + 8: `note_push`'s
unconditional `or_extra_at` on every non-last child is pure
overhead on the generic `push_compound` path (no open frames) —
but the guard is a single `if let Some(frame) =
self.open_stack.last_mut()` short-circuit, predicted-not-taken for
emitters that don't use the pair.

**Verdict. SOUND.**

---

## Surface 3 — `note_push` hook overhead

**Claim (orchestrator inference).** `note_push` fires inline on
every structural push (leaf + compound + open), touching the
innermost open frame when present. Zero-cost on the non-open path.

**Expand truth.** `ay-json.rs` shows 0 textual mentions of
`note_push` (private fn, inline'd at LTO time — not visible in
bench-crate expansion). Call sites in `crates/tape/src/builder.rs`:
lines 277, 336, 417 (pre-stack), 566, 714, 780, 826, 872 — eight
call sites covering every push entry point (`push_leaf`,
`push_compound`, `open_compound`, `push_leaf_with`,
`push_leaf_with_arena_frame`, `push_leaf_with_arena_payload`,
`push_leaf_borrowed_string`, `push_leaf_with_f64_direct`).

The hook body at `builder.rs:235-248`:

```rust
#[inline(always)]
fn note_push(&mut self, new_idx: u32) {
    if let Some(frame) = self.open_stack.last_mut() {
        if frame.last_child != u32::MAX {
            let prev = frame.last_child;
            self.columns.set_sib_skip_at(prev, new_idx - prev);
            self.columns.or_extra_at(prev, TapeRec::SIB_SKIP_STAMPED_BIT);
        } else {
            frame.first_child = new_idx;
        }
        frame.last_child = new_idx;
    }
}
```

Under `#[inline(always)]` + LTO, the non-open path collapses to
one load + one predicted-not-taken branch (`self.open_stack.last()
== None`). On JSON's hot path, the open_stack is NEVER empty
during an array/object parse (outer Rule frame always active);
every leaf push pays 2 column writes (`set_sib_skip_at` +
`or_extra_at`) that the pre-W5 path did not pay.

**Delta.** This IS visible as additional bytes/cyc cost — AY
PROGRESS §W5 recorded -17% twitter (746 → 616 MB/s) and §W6
recorded another -11% (616 → 548 MB/s). The note_push hook is
the leading candidate cause per AY PROGRESS §W5 close paragraph
("`note_push` hook fires on every push_leaf + push_compound,
touching the sib_skip column + extra column even for shapes that
DON'T use open/close"). On JSON, the hook fires on the outer
open frame's children, so "shapes that DON'T use open/close" is
a misdiagnosis — JSON literally does use open/close; the hook is
paying what it promises.

**Verdict. SOUND at the contract level, OVERHEAD visible at
runtime.** W7's stated scope includes note_push audit and
dead-surface retirement; that is the right restoration vector.

---

## Surface 4 — Direct-to-struct admission
(AY.W6.b, commits `adedff39`, `456471d3`, `55990f6c`)

**Claim (W6.b agent report, PROGRESS §W6-B).** 71 admitted
projections across the 4-grammar corpus (JSON=2, CSS L4=49,
Sheets=10, BBNF=10). `cargo expand` shows 350 `pub struct` +
69 `materialize_projection_*` fns + 2 `__named_type_shim_*`
markers.

**Expand truth.** `ay-named-type.rs`:

- `PROJECTION_DIRECT_TO_STRUCT` const entries:
  - Line 4653: JSON `2usize` entries — `("bool",
    "BoolProjection"), ("string", "String")`.
  - Line 169559: CSS L4 `49usize` entries.
  - Line 224560: Sheets `10usize` entries.
  - Line 251425: BBNF `10usize` entries.
  - **Total: 71** — matches the claim.
- `pub struct \w+Projection` definitions: **69**.
- `fn materialize_projection_\w+_\w+G`: **69** (1 `_JsonG` + 48
  `_CssL4G` + 10 `_SheetsG` + 10 `_BbnfG`).
- `__named_type_shim_*` fns: **2** (`__named_type_shim_string`
  at line 4665; `__named_type_shim_color` at line 169618).

**Delta.** The "71" figure is accurate only for const *entries*;
the **executable projection surface is 69 Projection structs +
69 materialiser fns + 2 resolver shims**. The gap of 2 is the
legacy resolver-backed admissions preserved for backward
compatibility — JSON `string → String` and CSS L4 `color →
Color` — which do NOT emit a `<Rule>Projection` struct and
instead emit the `__named_type_shim_<name>()` marker. This is
commit `adedff39`'s documented split.

The 350 `pub struct` number in the PROGRESS entry appears to
refer to *all* `pub struct` emissions in the expand output
(grammar-side view structs + Value enums + Projection structs +
AoT materialisation temporaries). The actual count of `pub struct
\w+Projection` patterns is **69** — the projection surface the
test harness actually asserts against.

**Verdict. SOUND** for the admission count (71 entries); the
"71 projections" shorthand conflates const entries with
materialiser fns — a 69-vs-71 discrepancy for the latter. No
deceit, but the W6.b agent's claim would be more truthfully
phrased as "71 PROJECTION_DIRECT_TO_STRUCT entries, 69
materialiser fns, 2 resolver shims".

---

## Surface 5 — Object + array Shape-1 retarget to open/close
(AY.W5.b, commits `09ca39d6`, `cf6f2a76`)

**Claim.** `crates/core/src/backend/rust/emitter/shapes/object.rs`
retargets 10 `push_compound` + 9 `mark_children` spans to the
open/close pair. `shapes/array.rs` Shape-1 retargets 12
push_compound spans similarly. Shape-2 intentionally left on
push_compound due to the retry-IIFE rollback problem.

**Expand truth.** `ay-json.rs`:

- `parse_array_JsonParser_array` (lines 1387-1554): uses
  open_compound exclusively for its 7 compound sites (outer,
  next, opt_ws, repeat, iter, comma_repeat, comma_iter). Zero
  push_compound in this function body.
- `parse_object_JsonParser_object` (lines 1639-1860): uses
  open_compound for all 10 compound sites (outer, next, opt_ws,
  repeat, iter, pair, colon_next, opt_colon, comma_repeat,
  comma_iter). Zero push_compound.
- `parse_flat_JsonParser_pair` (lines 1574-1631): **still uses
  push_compound** at line 1622 — the outer Flat-shape Seq. This
  is NOT Shape-1; the `pair` rule is a Flat production (`pair =
  string, colon >> value`) under the Flat-shape emitter, not the
  Array or Object shape. The Flat emitter is not in W5.b's
  retarget scope and remains on push_compound + mark_children.

Total `push_compound` call sites in `ay-json.rs`: **1**, the
`pair` Flat compound. All other compound writes go through
open_compound.

The `__shape_support_*` helpers emit no compounds (leaves only).
String shape (`parse_string_JsonParser_string` at line 1321) is
a pure leaf — uses `push_leaf_borrowed_string` or
`push_leaf_with_arena_frame` via the escaped path.

**Delta.** None — the retarget is Shape-1-complete as designed.
The `parse_flat_JsonParser_pair` residual is the `pair` Flat
rule, outside the plan's "object + array Shape-1" scope. Shape-2
(`emit_parse_array_list`, used by CSS stylesheet + BBNF) remains
on push_compound at `crates/core/src/backend/rust/emitter/shapes/array.rs:596-776`
— four `builder.push_compound(` sites — preserved by the W5-b
agent's rationale (retry IIFE truncates `columns_mut()` below
the open_stack's recorded frame).

**Verdict. SOUND.**

---

## Surface 6 — Pratt outer compound on open/close
(AY.W6.c, commit `bfadba84`)

**Claim.** Pratt outer precedence compounds retarget to
open_compound / close_compound (AY.W5.a substrate); inner
reducer compounds remain on push_compound.

**Expand truth.** `ay-css-l4.rs`:

- Total `open_compound` sites: **3**, all inside Pratt
  productions:
  - `parse_pratt_CssL4Parser_mathProduct` at line 11186
    (open at 11205).
  - `parse_pratt_CssL4Parser_mathExpr` at line 11419 (open at
    11438).
  - `parse_pratt_CssL4Parser_mediaQueryList` at line 18078
    (open at 18097).
- Total `close_compound` sites: **3**, one per Pratt outer
  (e.g. `builder.close_compound(outer_off, outer_span_hi)` at
  pratt.rs source line 523).
- Reducer inner compound at `ay-css-l4.rs:11255-11263`:

```rust
let compound_idx = builder.push_compound(
    ::bbnf::runtime::tape::TapeKind::Rule,
    ::bbnf::runtime::tape::TapeOffset(lhs_idx),
    ...
);
```

Reducers stay on push_compound by design (post-order
construction, never subject to rollback).

- Total `push_compound` sites in CSS L4 expansion: **458**,
  reflecting the extensive non-Pratt compound productions (CSS
  declarations, selectors, media query tails, keyword lists) —
  all of which ride the legacy `push_compound` + `mark_children`
  path.
- Total `mark_children` sites: **467** — companion marker for
  the 458 push_compound sites plus a few `tape_snapshot` debug
  captures.

**Delta.** The plan's hard gate ("Pratt outer compound uses
open/close") is met — exactly 3 outer Pratt compounds, exactly 3
open/close pairs. The wider claim that might be inferred ("Pratt
lowering on packed substrate") is narrow: only 3 of CSS L4's
hundreds of compounds exercise the substrate — the rest remain
on the legacy post-order path with a post-parse finalise.

**Verdict. SOUND** (the Pratt retarget landed exactly as
specified; the post-order-vs-write-time mixture across CSS L4
is the intended state).

---

## Surface 7 — `navigate_tape`
(AY.W6.c, commits `3230f292` substrate + `b1c7d47a` "consumer wired")

**Claim (W6.c PROGRESS entry).** `runtime/path.rs` gains
`navigate_tape` for substrate-level tape walking; a follow-on
commit "wires" it into the `__path_walk` emission with test
coverage extended to 8 pass assertions.

**Expand truth.** The following are all concurrently true:

1. `crates/core/src/runtime/path.rs:221` defines
   `pub fn navigate_tape<'p>(...)` — a substrate-level walker
   that resolves a PathSegment sequence against a compound subtree
   via packed-node inspection.
2. `navigate_tape` has zero call sites outside
   `crates/core/tests/value_api_apples_to_apples.rs` — four
   test assertions (lines 232, 262, 281, 300) invoke it against
   hand-built fixture tapes.
3. The emitter's `__path_walk` (generated at
   `crates/core/src/backend/rust/view/value.rs:368-420`, expanded
   into `generated.rs:22902-22989`) continues to use the **generic
   child-walk two-at-a-time pattern** (`cur.children()`; key/value
   pairs; `key_text == *key` comparison). No call to
   `navigate_tape`.
4. `ay-json.rs` has 0 textual occurrences of `navigate_tape`.
5. `ay-named-type.rs` has 0 textual occurrences of
   `navigate_tape`.
6. The W6 hard gate 3 ("Samply on JSON twitter path lookup:
   child-walk ≤ 1%") is marked "SOFT-PASS (not directly
   sampled)" in PROGRESS §W6 — an empty evidence citation.

**Delta.** Commit `b1c7d47a`'s message claims "navigate_tape
consumer wired", but the only consumer wired is the test
binary. The production path-query consumer (`__path_walk` in
`view/value.rs`) was not edited. This is **substrate-without-
consumer** in the exact pattern the README §"Substrate-with-
consumer is one unit of work" rejects at wave close.

**Verdict. DEAD on the hot path** — navigate_tape compiles,
tests assert it works on fixture tapes, but no production
consumer reads it. The W6.c claim of "generic child-walk ≤ 1%"
cannot be met by navigate_tape alone until `__path_walk` is
rewritten to delegate. Candidate for retirement OR genuine
wiring in W7.

---

## Surface 8 — W7 stalled bug

**Claim (W7 uncommitted draft in
`/Users/mkbabb/Programming/bbnf-wt-ay-w7/crates/tape/src/builder.rs`).**
The `open_stack` panics / mis-stamps when an emitter-retry
IIFE or a `?` bailout truncates `columns` below a frame's
recorded `last_child`. Fix: guard `note_push`'s stamp with
`prev < new_idx` and drain orphan frames above the closing
`compound_offset` in `close_compound`.

**Expand truth.** The failure mode is plainly visible in
`ay-json.rs`:

- `parse_array_JsonParser_array` (line 1455-1458):

  ```rust
  let _value_off = ({
      let _ = __shape_support_JsonParser::skip_space(input, p, state);
      parse_wrap_JsonParser_value(input, p, state, builder)
  })?;
  ```

  The `?` on line 1458 returns `Err(...)` from the array parse
  while an `open_stack` frame (the `iter_off` opened at line
  1453-1454) is still live. The caller unwinds without
  `close_compound(iter_off, ...)` firing; the outer `array`
  frame (`outer_off` at 1404) is also live and will never close
  cleanly.

- Same pattern at line 1618 (`parse_flat_JsonParser_pair`)
  and line 1758 (`parse_object_JsonParser_object`'s
  `opt_colon_off` arm).

- `parse_pratt_CssL4Parser_mathProduct` (ay-css-l4.rs line
  11211): `parse_altdispatch_CssL4Parser_mathValue(...)?` can
  fail after the Pratt outer compound is opened at line 11205.

The uncommitted W7 diff at
`/Users/mkbabb/Programming/bbnf-wt-ay-w7/crates/tape/src/builder.rs`
introduces:

1. `note_push` guard: `if prev != u32::MAX && prev < new_idx`
   replaces the bare `if frame.last_child != u32::MAX`. When
   `new_idx <= prev` (post-truncation rollback), the frame
   resets its first_child to `new_idx`.
2. `close_compound` orphan-drain: `while let Some(top) =
   self.open_stack.last()` pops any frame whose
   `compound_offset > target`.
3. `close_compound` child-validity guard: stamps the last
   child's SIB_SKIP_STAMPED_BIT only when `(frame.last_child as
   usize) < self.columns.len()`.

**Delta.** The root cause the W7 agent identified is **correct**
— the `?` propagation pattern at every call-chain failure site
leaves open frames orphaned because there is no equivalent of
Rust's RAII "drop on unwind" for the tape builder's state (the
builder sees only the `?`-post state, no unwind hook). Every
emit site with a `?` is a frame-orphan hazard.

The `prev < new_idx` guard is the **correct fix for `note_push`**
— it recovers cleanly when `columns` has truncated below the
frame's memory. The child-validity guard in `close_compound` is
also correct.

The orphan-drain in `close_compound` is **less defensible as a
stand-alone fix**. It papers over an emitter-codegen
contract violation: the emitter should pair every `open_compound`
with either a `close_compound` OR an explicit rollback (e.g.
`self.open_stack.truncate(...)`) on the failure path. Draining
orphan frames inside the builder silently absorbs emitter bugs
rather than forcing the emitter to declare its failure semantics.

The more architecturally clean approach is: emitter retry IIFEs
and `?` paths that might fail between open and close should
either (a) not open the compound until success is guaranteed, or
(b) emit an explicit `builder.rollback_open_frame(off)` on the
failure path. The W7 guard is defensible as a post-hoc safety
net but not as the architectural fix.

**Verdict. DRIFT.** The root cause is correctly identified; the
`prev < new_idx` guard is a legitimate invariant hardening; the
orphan-drain is a band-aid over an emitter-contract omission
that should itself be remediated.

---

## Summary

| # | Surface | Verdict |
|---|---|---|
| 1 | `open_compound` / `close_compound` API | SOUND |
| 2 | `SIB_SKIP_STAMPED_BIT` activation | SOUND |
| 3 | `note_push` hook overhead | SOUND (overhead visible) |
| 4 | Direct-to-struct admission | SOUND (71 const / 69 fn, minor labelling drift) |
| 5 | Object + array Shape-1 retarget | SOUND |
| 6 | Pratt outer open/close | SOUND |
| 7 | `navigate_tape` | DEAD (substrate-without-consumer) |
| 8 | W7 stalled bug | DRIFT (root cause right; orphan-drain is a band-aid) |

**SOUND count: 6.** Surfaces 1–6 deliver what commits claim,
with observable expand + source evidence.

**DRIFT count: 1.** Surface 8 needs architectural redress, not
the proposed builder-side orphan absorption.

**DEAD count: 1.** Surface 7's `navigate_tape` is a pure
test-binary consumer; no production emitter calls it; the
"navigate_tape consumer wired" claim refers to test wiring only.

## Top 5 root-cause findings informing the forward-path audit

1. **The W5 substrate is real and wired; the W5+W6 perf regressions
   are NOT substrate failure — they are note_push hook overhead
   without a compensating lever.** `note_push` pays 2 column
   writes per non-last direct child on every compound that uses
   open/close, which on JSON is *every* outer array/object frame.
   The pre-W5 post-pass amortised that work across one linear
   sweep; the W5 write-time path pays it per-push. Until the
   finaliser's skip-work is visibly dominated by what `note_push`
   subsumed (samply self-time on `finalise` vs pre-W5), the
   break-even is not reached.

2. **The emitter's `?` / retry-IIFE failure paths break the
   open/close pair contract.** Every call site in `ay-json.rs`
   where a `?` propagates inside an open frame's window is a
   latent frame-orphan. The W7 guard is correct diagnosis; the
   architectural fix is to require emitters to declare
   rollback semantics at `open_compound` sites, not to drain
   orphans silently in `close_compound`. A
   `rollback_to(open_off)` builder entry + an emitter-side
   `match` on the fallible call wrap is the KISS form.

3. **`navigate_tape` is substrate-without-consumer.** The commit
   message conflates "wired into test" with "wired into
   production consumer". `__path_walk` (the generated-code hot
   path in `view/value.rs:368-420`) still iterates generic
   children two-at-a-time. W6's hard gate 3 (child-walk ≤ 1%)
   cannot close without either:
   - emitter rewrite of `__path_walk` to delegate to
     `navigate_tape`, **or**
   - retirement of `navigate_tape` as a test-only scaffold.

4. **Grammar-name dispatch is gone from direct-to-struct
   admission; the count-inflation in "71" is harmless but the
   shim vs materialiser boundary needs canonical naming.** The
   71 const entries / 69 fn count / 2 shim count split reflects
   two admission paths that diverged before AY.W6.b unified
   them; the split should collapse in W7's shared-fact optimiser
   pass so the const entries match the materialiser count 1:1
   (the 2 resolver shims then become an emission of the same
   materialiser skeleton, with the resolver-backed type as the
   constructor target).

5. **CSS L4's 458 `push_compound` sites are the next lever, not
   a W5/W6 oversight.** The Pratt retarget is narrow by design
   (3 outer compounds on a grammar with hundreds of productions).
   CSS tailwind throughput (AY PROGRESS §W4: 29.18% regex
   self-time at close vs 12% target) is dominated by
   declaration/selector compounds still on the legacy path.
   Migrating them to open/close is a W7-or-later substrate
   expansion that needs the rollback-semantics clarified first
   (finding 2), otherwise every nested-iteration compound
   amplifies the frame-orphan hazard.

## Evidence citations

- Commits referenced: `feffe271`, `2d420e9e`, `2eff2019`,
  `09ca39d6`, `cf6f2a76`, `adedff39`, `456471d3`, `55990f6c`,
  `bfadba84`, `3230f292`, `b1c7d47a`.
- Expand artefacts at
  `/Users/mkbabb/Programming/bbnf-wt-ay-audit-hitherto/target/expand/`:
  - `ay-json.rs` (6 186 lines, 250 638 bytes)
  - `ay-named-type.rs` (259 721 lines)
  - `ay-css-l4.rs` (206 412 lines)
- Uncommitted W7 fix at
  `/Users/mkbabb/Programming/bbnf-wt-ay-w7/crates/tape/src/builder.rs`
  (not yet on master; diff inspected verbatim).
- Master HEAD at audit time: `b346ebca` (W6 close-with-recorded-
  misses landing per AY/PROGRESS §"W6 closes with recorded
  misses").

End of AUDIT-B.
