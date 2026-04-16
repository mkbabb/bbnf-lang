# Research 01 — DTA driver design (W1 walker contract)

Tranche AW, W1. Angle: `bbnf_tape::driver::dta_run` replacing
`fn __<rule>` recursion, closing skeleton → PSI → Stage-C (substrate at
`crates/bbnf-tape/src/{dta.rs:87-216, psi.rs:153-311, finaliser.rs:146-230}`).

## 1 — Walker body

`dta_run` is one loop indexed by a working `DtaStateId`. Each
`DtaState` variant (`dta.rs:93-161`) is one arm: arms (a) consume
input and emit structural columns, (b) push/pop a frame and transition,
or (c) enqueue a `PayloadJob`.

```rust
// crates/bbnf-tape/src/driver.rs (new, AW.1.1)
pub fn dta_run(
    table: &DtaTable, profile: &GrammarProfile,
    input: PaddedView<'_>, columns: &mut Columns,
    psi: &mut PayloadStream, frame_depth: &mut Vec<u8>,
) -> Result<TapeOffset, ParseErr> {
    let bytes = input.bytes();
    let mut pos: u32 = 0;
    let mut stack = FrameStack::new(table.max_nesting_depth);
    let mut state = table.rule_entries[0].state;        // dta.rs:201
    let root_mark = columns.len() as u32;

    'dispatch: loop {
        match table.states[state.0 as usize] {
            DtaState::Seq { children, frame } => {
                let parent_rec = columns.reserve_structural(pos);
                frame_depth.push(stack.depth());
                stack.push(Frame { kind: frame, children, cursor: 0,
                    parent_rec, child_mark: columns.len() as u32, ..});
                state = children[0]; continue;
            }
            DtaState::ByteDispatch { table: disp, fallback } => {
                // AU.2.7 bitmap already advances to structural bytes;
                // `disp[bytes[pos]]` is the typed u8 jump table.
                let b = bytes[pos as usize];
                let next = disp[b as usize];
                state = if next != DtaStateId::NONE { next } else { fallback };
                if state == DtaStateId::NONE {
                    return Err(ParseErr::Syntax { offset: pos, rule: None });
                }
                stack.top_mut().cursor = (next.0 as u32); // variant_idx stamp
                continue;
            }
            DtaState::Literal { text } => {
                if !bytes[pos as usize..].starts_with(text.as_bytes()) {
                    return Err(ParseErr::Syntax { offset: pos, rule: None });
                }
                let lo = pos; pos += text.len() as u32;
                let rec = columns.push_structural_leaf(lo, pos);
                frame_depth.push(stack.depth());
                state = advance_or_pop(&mut stack, columns, frame_depth, pos)?;
                continue;
            }
            DtaState::Regex { pattern } => {
                let lo = pos;
                let hi = scan_regex(pattern, &bytes[lo as usize..])
                    .ok_or(ParseErr::Syntax { offset: pos, rule: None })?;
                pos = lo + hi;
                let rec = columns.push_structural_leaf(lo, pos);
                psi.push(PayloadJob::new(rec, lo, pos,
                    PayloadKind::from_state(state), next_rank(columns, state)));
                frame_depth.push(stack.depth());
                state = advance_or_pop(&mut stack, columns, frame_depth, pos)?;
                continue;
            }
            // Repeat, Ref, AltLinear, ShuntingYard, Epsilon elided.
        }
    }
}
```

Walk `{"a": 1}` (JSON, ~25 states):

| pos | byte | state | action |
|----|------|-------|--------|
| 0 | `{` | Seq(object)| reserve parent row, depth 0; push Frame |
| 0 | `{` | Literal("{") | leaf(0,1) depth 1 |
| 1 | `"` | ByteDispatch| `disp[b'"'] → string` |
| 1..4 | `"a"` | Seq+Regex | leaf depth 2; `PayloadJob{String, col 0}` |
| 4 | `:` | Literal(":") | leaf |
| 5..6 | ws | trim | skipped via structural_alphabet |
| 6 | `1` | Regex(number)| leaf; `PayloadJob{F64, col 0}` |
| 7 | `}` | Literal("}") | pop pair + object; return `root_mark` |

Result: 5 structural rows, `frame_depth = [0,1,2,2,2,1,0]`, 2 PSI jobs.
Tape parity against fn-per-rule is the AW.0.1 regression gate
(`AW.md:409-412`).

## 2 — Frame stack contract

```rust
#[repr(C)] struct Frame {
    kind: DtaFrameKind,                // dta.rs:53-63
    children: &'static [DtaStateId],
    cursor: u32,                       // child index | branch | iter count
    parent_rec: u32, child_mark: u32,  // reserved row, columns.len() at push
    counter_idx: u8,                   // slot in FrameStack.counters
}
struct FrameStack {
    hot: [Frame; 64],                  // STACK_DEPTH_HINT (finaliser.rs:114)
    overflow: Vec<Frame>,
    counters: SmallVec<[u32; 16]>,     // AW.md:773
    depth: u8,
}
```

- **Push** on `Seq`/`Repeat`/`ShuntingYard` entry; captures
  `columns.len()` as `child_mark` (the AV `mark_children` offset,
  `builder.rs:142-144`) and reserves the parent row with `span_lo` only.
- **Pop** via `advance_or_pop` on every leaf emit: when cursor ==
  children.len() (Seq) etc., stamp `span_hi ← pos`, `has_children ← 
  (columns.len() > child_mark)`, `child_off ← TapeOffset(child_mark)`.
  Collapses the empty-compound NONE compare (`AW.md:296`,
  `builder.rs:216-222`) to one `>` test.
- **Counter registers.** Parallel column `SmallVec<[u32; 16]>`; each
  frame names its slot via `counter_idx`. Nested Repeats reuse counter
  memory after the enclosing frame pops. List-rule iteration counts
  (`AV.md:1037`) read the column directly; shunting-yard precedence
  uses a second slot. 16-inline bounds the fast path to one 64 B line,
  isomorphic to `DtaSnapshot.counter_regs` (`AW.md:773`).

## 3 — `frame_depth` emission

DTA writes `frame_depth[i]` at *the instant of row push* — both
`push_structural_leaf` and `reserve_structural` call
`frame_depth.push(stack.depth())`. One 1 B store per push, in the cache
line the stack counter already occupies (L1 hit).

Cost vs `derive_frame_depth` (`finaliser.rs:254-302`):

- **Post-AV:** forward scan + per-compound backward walk following
  `child_off`; two chained dependent loads per compound, prefetcher-
  hostile (reverse direction).
- **Post-W1:** one byte store per push; `Stage-C` reads the
  pre-populated `&[u8]` — `finalise(columns, &frame_depth)`
  (`finaliser.rs:146`) already accepts the slice. The
  `has_inline_frame_depth` flag (`AW.md:414-419`) flips permanently
  true; `derive_frame_depth` deletes.

## 4 — Stage-A → Stage-B → Stage-C pipeline

```rust
// emitter/grammar.rs post-W1 (AW.1.2) — replaces .rs:503-543
pub fn parse(input: &str) -> Result<Parsed<'_, Self>, ParseErr> {
    let state = ParserState::new(input);            // state.rs:379
    let mut columns = Columns::with_capacity(
        GRAMMAR_PROFILE.capacity_for(input.len())); // profile.rs:260
    let mut psi = PayloadStream::with_capacity_for(
        &GRAMMAR_PROFILE, input.len());             // psi.rs:258
    let mut frame_depth: Vec<u8> =
        Vec::with_capacity(columns.kinds.capacity());

    let root = dta_run(&DTA_TABLE, &GRAMMAR_PROFILE,
        state.padded(), &mut columns, &mut psi, &mut frame_depth)?;
    psi.fill_columns(state.padded().bytes(), &mut columns,
        &GRAMMAR_PROFILE);                          // psi.rs:332
    finalise(&mut columns, &frame_depth);           // finaliser.rs:146

    Ok(Parsed::new(Tape { columns }, input, root))
}
```

- `dta_run` returns `Ok(TapeOffset)` to the root row; `columns`, `psi`,
  `frame_depth` are fully populated.
- `PayloadStream::fill_columns` already branches on `should_parallelise`
  (`psi.rs:303-311`); W1 defaults sequential (AW.1.5), rayon activates
  per grammar at W2.
- `frame_depth` is ephemeral — a stack `Vec<u8>` dropped after
  Stage-C; `Columns` does not absorb it, so AV.2's layout is unchanged.

## 5 — Replay substrate zero-cost sketch

Cleanest zero-cost route: feature-gated *signature variance*. When
`dta-replay` is off, `dta_run` never takes the `Option` — LLVM has no
branch to hoist.

```rust
#[cfg(feature = "dta-replay")] pub fn dta_run(..., 
    decision_log: Option<&mut Vec<u8>>, resume: Option<&DtaSnapshot>)
    -> Result<TapeOffset, ParseErr> { ... }
#[cfg(not(feature = "dta-replay"))] pub fn dta_run(...)
    -> Result<TapeOffset, ParseErr> { /* no optionals */ }
```

With the feature on + `None`, hot path is:

```rust
if let Some(log) = decision_log.as_deref_mut() { log.push(state.0 as u8); }
```

Expected asm (release + `lto=fat`, cargo asm `bbnf_tape::driver::dta_run`):

1. One `testq %rdi, %rdi; je .LBB_tail` per iteration targeting the
   next state load — no instructions between test and tail.
2. No `callq Vec::push` speculatively scheduled inside the predicted-
   taken block. The log-write bb must be out-of-line (compiler
   hoists via block-frequency annotation on `Option::None` bias).

If asm shows `callq` inside the hot bb, add `#[cold]` on the log-write
helper or `core::intrinsics::unlikely` — bench gate 18 (`AW.md:1413`)
fails otherwise.

## 6 — Packrat retirement

`MemoStore` (`parse-that/.../state.rs:15-79`) is per-parse, dropped
with state; reached only through `memoize_state`
(`combinators/methods/mod.rs:39-66`). Grep across `crates/`: zero
production call sites. Packrat exists to avoid exponential re-probe
in PEG combinators; the DTA's counter-DFA visits each byte and each
state exactly once per fingerprint — no re-entry, no backtracking.
`ParserState::memo` (`state.rs:336`), `MemoStore`, and `memoize_state`
all delete. `memoize()` (`combinators/methods/mod.rs:77-113`) survives
because it's in the `Parser<'a, Output>` world gorgeous still consumes.
Only `parse-that`'s module-inline `#[cfg(test)]` exercises memo
storage; port to a DTA deterministic-replay fixture or delete.

## 7 — Migration risks

- **Pre- vs post-order.** Forward input walk yields pre-order tape
  layout (parent row first, children pushed into the gap, span_hi
  stamped on pop). AV.2 kept post-order (`AV.md:107`). Stage-C
  (`finaliser.rs:32-71`) was written for post-order; under pre-order
  its per-depth tracking simplifies but must be re-audited. W1.10 hard
  gate (`AW.md:844-846`) admits either answer; recommend pre-order
  + rewrite `finalise` inside W1 rather than deferring to AX.
- **Recursion limit.** fn-per-rule: 4 KB Rust stack; DTA `[Frame; 64]`
  inline at ~32 B/frame = 2 KB + heap overflow. Corpus depths (JSON ≤
  8, CSS L4 ≤ 12 per `AW.md:777`) fit inside the `max_nesting_depth`
  budget (`dta.rs:215`, `generated.rs:2169`).
- **Error shape.** Fn-per-rule `Option<TapeOffset>` → `parse()`
  converts `None` to `ParseErr::Syntax{ offset, rule: None }`
  (`generated.rs:27995`). DTA carries `DtaDiagnostic` natively
  (`dta.rs:265-305`) — `furthest_offset`, `failing_state`,
  `failing_rule` in one pass (`AV.md:700-704`). `dta_run` returns
  `Result<TapeOffset, ParseErr>` with `rule: Some(failing_rule.0)`,
  drop-in-compatible with existing callers.

## Open questions for the orchestrator

1. **Tape layout.** Pre-order (W1.10 accept) with Stage-C rewrite
   in-wave, or post-order (route O(1) `child(0)` to AX)? W1 cannot
   dispatch until this is decided — determines whether `finalise` is
   rewritten inside or outside the wave.
2. **`Frame` ABI location.** Private in `driver.rs` or promoted to
   `dta.rs` so `DtaSnapshot { frame_stack: SmallVec<[Frame; 64]> }`
   (`AW.md:771-775`) reuses the type? AX's substrate wants the latter;
   W1's encapsulation wants the former.
3. **`next_rank` ownership.** Per-kind monotonic column-rank
   allocation (`psi.rs:483-487`) — inside `dta_run` (one counter per
   `PayloadKind`) or a shared `ColumnRanks` struct promoted to
   `Columns`? PSI disjointness (`psi.rs:401-405`) requires monotonic
   per-kind; the walker needs a cheap counter.
