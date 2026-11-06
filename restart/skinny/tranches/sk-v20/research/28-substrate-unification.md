# 28 — Substrate Unification: the three-builder problem, re-censused at HEAD

## Verdict

The substrate cluster (W0-W2 of the SK-V19 SPEC) is the single largest piece of
un-folded skinny architecture remaining in `crates/core`. At HEAD `36ca10646`
the core fleet still runs **three structurally distinct eager builder families**
plus an **unwired AoS tape** — a textbook Lock-1 violation latent in the tree.
The skinny SoA tape (`offsets/flag_cursors/flag_values/payloads`, pure
commit-by-construction) is the proven end-state and is portable as-is. But the
SPEC's W1/W2 prose carries **at least five stale counts** that will mislead any
executor who takes them literally. The substrate end-state is sound; the SPEC's
arithmetic is not.

## What actually exists at HEAD (re-verified)

Three eager builder families, by structural shape — not the SPEC's loose roster:

| Family | Shape | Grammars | StructBuilder impl |
|---|---|---|---|
| OpenFrame (bespoke enum) | per-grammar `enum OpenFrame` + own `*StructBuilder` struct | json, css_l4, bbnf | 3 hand-written impls |
| Frame (sheets) | `struct Frame` + `SheetsStructBuilder` | google_sheets | 1 hand-written impl |
| SimpleStructBuilder (generic) | `SimpleStructBuilder<V,C>` over `Frame<V>` + per-grammar Value/Compound traits | bnf, math, ebnf, csv, css_pretty | 1 generic impl, 5 type aliases |

Evidence: `json/builder.rs:9,16`, `css_l4/builder.rs:16,66`, `bbnf/builder.rs:9,17`,
`google_sheets/builder.rs:9,16`, `builder_template.rs:85,121`,
`bnf/builder.rs:36`, `math/builder.rs:36`, `ebnf/builder.rs:36`, `csv/builder.rs:36`,
`css_pretty/builder.rs:36`. Five `impl StructBuilder` blocks total
(`tape/mod.rs:133`, `json/builder.rs:117`, `bbnf/builder.rs:79`,
`css_l4/builder.rs:200`, `google_sheets/builder.rs:78`) — four eager + one for the
unwired tape.

The fourth substrate — the AoS `TapeStructBuilder` (`tape/mod.rs:58`) over the
16-byte `#[repr(C)]` `TapeRec` (`tape/record.rs:103-118`) — is **wired into nothing
but a test**. `rg TapeStructBuilder crates/` outside `runtime/tape/` returns only
`tests/tape_substrate.rs` (8 hits). No generated parser, no `parse()` body, no
runtime path references it. The SPEC's "unwired AoS TapeStructBuilder" framing is
correct and the diagnosis is sharp: the moment a real SoA substrate wires, this AoS
record becomes a live dual-encoding (Lock-1) breach. Today it is dead weight, not a
breach — but it is a trap laid for W2.

The eager value/document layer (W3's target, downstream of substrate) is **17 files**:
`value.rs` + `document.rs` per grammar (sheets has only `value.rs`). Evidence:
`find runtime -name value.rs -o -name document.rs` = 17.

## Stale-count corrections (the SPEC's load-bearing arithmetic is wrong)

1. **"delete the 11 eager StructBuilder families" (§2/W1, line 62).**
   FALSE arithmetic. There are **3 structural families** (OpenFrame, Frame,
   SimpleStructBuilder), instantiated across **9 grammars**. The "11" appears to
   sum 3 OpenFrame + 1 Frame + a phantom "6-cohort". Corrected: 3 families / 4
   hand-written StructBuilder impls (json, css_l4, bbnf, sheets) + 1 generic impl.

2. **"SimpleStruct the 6-cohort" (§2, line 63).**
   FALSE. The SimpleStructBuilder cohort is **5 grammars**: bnf, math, ebnf, csv,
   css_pretty (`pub type *StructBuilder = SimpleStructBuilder<...>`, five aliases).
   The W1 sub-clause elsewhere writes "the 5-grammar simple cohort" (consistent with
   the assignment) — the SPEC is **internally contradictory**, saying both 5 and 6.

3. **"the 5 RuntimeStyle arms collapse to one" (§2, line 64).**
   FALSE. `regen_simple_runtime.rs:32-39` defines **4** `RuntimeStyle` variants:
   `Simple, TypedFormula, TypedBbnf, TypedJson`. The dispatch at lines 119-123 has
   **4 arms**, not 5. (css_l4/css_pretty are emitted by `regen_css.rs`, a separate
   generator the SPEC's W1 also names — so the generator surface is 2 files, not the
   one-collapse the prose implies.)

4. **"rg JsonStructBuilder|... == 0 (was 47)" (§2, line 67).**
   FALSE baseline. The actual current hit total for those four names across `crates/`
   is **~1080** (`JsonStructBuilder` 121, `CssStructBuilder` 741, `SheetsStructBuilder`
   197, `SimpleStructBuilder` 21). The bulk lives in generated parser bodies
   (`grammar/generated/*.rs`), which the regen rewrites — but "was 47" understates the
   delete surface by ~23×. A W1 executor budgeting against 47 will be blindsided.

5. **"rg TapeRec crates/ == 0" close-condition (§0 close-cond 2, §3/W2 line 78).**
   Current count is **41** (`rg TapeRec crates/`). This is the *target-is-0* gate, so
   41 is the work-remaining, not an error — but the SPEC nowhere states the starting
   value, so the close gate reads as already-near-zero when it is not.

6. **W0 manifest line refs "strategy.rs PRODUCTION_MANIFEST_TABLE (:134-189)"
   (§1, §7/W6a).** STALE. `strategy.rs` is now **284 lines** total; the
   const-table-with-grammar-names was already deleted by the W6abe fold
   (`36ca10646`) — `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
   crates/ir/src/` returns 0. The table is now `ManifestStrategyEntry` rows read from
   `[workspace.metadata.bbnf-strategy]` (`strategy.rs:235-284`). **W6a is already
   landed**; the W0 gate that classifies "against strategy.rs:134-189" points at a
   line range that no longer exists.

### Wave-status drift (the SPEC describes a pre-fold tree)

The SPEC header pins HEAD `a97f5f365`; actual HEAD is `36ca10646`, two folds later.
The interposed commits (`f4633d468` W6c, `36ca10646` W6abe) **already executed
W6a, W6b, W6c**: `css_types.rs` is deleted and relocated to `runtime/css_l4/host.rs`
(`rg css_types crates/core/src/` = 0); the BBNF shape recognizer is re-keyed off
structure. So §7's W6a/b/c are DONE, but **W0-W2 (substrate) and W3-W5 (value plane
+ engine) are entirely untouched** — the eager builders were last modified by
`1c5bd7a25` ("add shared flat-tape runtime substrate", the SK-V16 commit that
introduced the *unwired* AoS tape). The leak-collapse landed ahead of the substrate
it was sequenced to follow. SK-V20 must re-census the wave manifest before dispatch.

## The single-substrate end-state

ONE SoA tape, ported verbatim from skinny `runtime/src/tape/`:

- **Columns** (`skinny tape/mod.rs:92-99`): `offsets: Vec<u32>`,
  `flag_cursors: Vec<u32>`, `flag_values: Vec<u8>`, `payloads: PayloadArena`. The
  flag side-channel is a sparse cursor→flag map (binary-searched at read,
  `flags_at` line 142), so the common no-flag node costs exactly one `u32` offset —
  the densest encoding in the campaign.
- **Builder** = skinny `TapeBuilder` (`assembler.rs:42-120`): `push_plain_offset`
  is a branch-predicted unchecked write with a `#[cold]` grow path; **no checkpoint,
  no rollback, no `stack.clone()`**. This is the commit-by-construction discipline
  the SPEC names. It is the right target and it is already proven.
- **Cursor/value plane** = skinny `Cursor` + `ValueRef<K>` (`tape/mod.rs:173`,
  `tape/cursor.rs`), the W3 layer.

The asymmetry W1 must close is real and measurable: every eager builder does
`stack: self.stack.clone()` at `checkpoint` (`builder_template.rs:223`,
`json/builder.rs:124`, `css_l4/builder.rs:210`, `google_sheets/builder.rs:84`) —
O(depth) speculative clone per parse alternative. The skinny builder has **zero**
checkpoint surface; speculation lifts upstream of the tape onto the offset
side-array, truncated in O(1). The SPEC's "throughput WIN, not a risk" claim is
sound. **Caveat the SPEC omits:** the unwired AoS `TapeStructBuilder` *already*
implements O(1) length-truncation checkpoint/rollback (`tape/mod.rs:137-156`) — so
the "removes O(depth) clone" win is over the *eager* families, not over the AoS
tape, which already had it. The honest framing is: SoA wins over AoS on cache
density and column-decoupling, not on the rollback cost (both are O(1) there).

## Retire order

The substrate retire is a single fused landing, not a sequence of safe intermediate
states — there is no point at which "half SoA" parses correctly. The order is
construction-driven:

1. **Port the SoA tape into core first, behind no consumer** (greenfield, mirrors
   skinny `tape/`). Replaces the AoS `record.rs`/`mod.rs`/`cursor.rs` triplet. The
   AoS `TapeRec` and its `tape_substrate.rs` test are **deleted in this step**, not
   retained — it is the only AoS consumer (187-line test), so its deletion is the
   exactly-one-encoding closure (the 41 `TapeRec` hits go to 0 here).
2. **Re-point the codegen substrate splice** in `regen_css.rs::emit_builder` and
   `regen_simple_runtime.rs` (the 4 RuntimeStyle arms) so every grammar's `parse()`
   body emits against the one `TapeBuilder`. The per-grammar Value/Compound traits
   and the `SimpleValue`/`SimpleCompound` template indirection collapse here.
3. **Regen all 9 parsers**; the ~1080 `*StructBuilder` references rewrite to the one
   generic path. Delete the 9 per-grammar `builder.rs` files + `builder_template.rs`.
4. The eager value/document layer (17 files) survives this wave and is retired by W3
   (the C fold) over the now-SoA tape — substrate and value plane are co-fenced.

The cursor is currently AoS-coupled (`tape/cursor.rs:50` indexes
`self.tape.records[off]` and reads `rec.child_off`/`rec.span_lo` directly), so it is
**rewritten, not adapted** — there is no shared cursor across the two encodings, and
attempting to keep one would itself be a dual-encoding smell.

## Recommendation (gestalt, not patch)

Do not "delete the AoS tape, then build SoA" as two waves — that briefly leaves
core with *zero* tape and a half-wired regen. Port skinny's `tape/` module
wholesale as the introduce, wire the regen splice, regen, and delete the eager
families + AoS record **in one fenced landing** gated on full 9-grammar parity. The
single substrate is `skinny/crates/runtime/src/tape/` transplanted; the only
genuinely new core work is the StructBuilder-trait shim that lets the generated
descent push offsets/flags/payloads through the skinny `TapeBuilder` API. Before any
of this, SK-V20 must **rewrite the W0-W2 SPEC counts** (the six corrections above)
and re-pin the wave manifest to HEAD `36ca10646` — the executor's budget, falsifier
line-refs, and close gates are all keyed to numbers that the W6 fold already
invalidated.
