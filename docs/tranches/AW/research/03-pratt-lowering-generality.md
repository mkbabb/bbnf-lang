## Pratt lowering generality — W4.6 precedence tower

The six-level Sheets tower (`__formula → … → __unary_expr`)
is already detected at lift time by
[`collect_precedence_chain`](../../../../crates/ir/src/passes/recognizers/dta.rs#L592)
and collapses to one
[`DtaState::ShuntingYard`](../../../../crates/bbnf-tape/src/dta.rs#L155)
whose `precedence: &'static [DtaPrecedenceEntry]` slice is
emitted as `__DTA_SY_73_PREC` (`generated.rs:171`). What
remains is the runtime consumer — today the DTA driver
treats `ShuntingYard` as opaque, so every chain rung still
pushes through the generic Seq/Repeat path and emits one
`push_compound` per *rung* regardless of whether the rung's
operator fires. W4.6 lands the inline Pratt loop inside the
DTA driver's `ShuntingYard` arm and elides empty wrappers.

### 1. Precedence-LUT bit layout

The detector produces one
[`PrecedenceEntry`](../../../../crates/ir/src/passes/recognizers/dta.rs#L195)
per operator byte (≤ 16 per grammar). W4.6 re-materialises
it as a dense `[u8; 256]` byte-LUT for O(1) dispatch,
keeping the sparse slice alongside for second-byte +
op-rule + discriminant metadata.

```
pub const PRECEDENCE_LUT: [u8; 256]
  // bits 0..4 = precedence     (0..15; 0 = not an operator)
  // bit  4    = associativity  (0=Left, 1=Right)
  // bits 5..6 = arity          (0=binary, 1=prefix,
  //                             2=postfix, 3=reserved)
  // bit  7    = two-byte op — consult sparse table for
  //             second_byte + discriminant (`<=`,`<>`,`>=`).
```

Sheets worked example — precedence ordering falls out of
chain depth (detector gives rung `k` precedence
`num_rungs - k`):

| op   | byte | prec | assoc | 2B | LUT byte |
|------|------|------|-------|----|----------|
| `<`, `<=`, `<>` | 0x3C | 3 | L | 1 | 0x83 |
| `>`, `>=`       | 0x3E | 3 | L | 1 | 0x83 |
| `=`  | 0x3D | 3 | L | 0 | 0x03 |
| `&`  | 0x26 | 4 | L | 0 | 0x04 |
| `+`  | 0x2B | 5 | L | 0 | 0x05 |
| `-`  | 0x2D | 5 | L | 0 | 0x05 |
| `*`  | 0x2A | 6 | L | 0 | 0x06 |
| `/`  | 0x2F | 6 | L | 0 | 0x06 |
| `^`  | 0x5E | 7 | R | 0 | 0x17 |

Hot-path lookup: one byte-load + shift-mask pair. Every
non-operator byte reads `0x00`, terminating the loop.

### 2. Pratt loop pseudocode

Pratt dispatch lives inside the DTA driver's `ShuntingYard`
match arm — **no separate `fn __pratt`**, satisfying the
"one codegen path" invariant.

```rust
// DtaState::ShuntingYard arm inside the DTA driver.
#[inline]
fn pratt_dispatch(
    state: &mut DtaState, tape: &mut Columns,
    head: DtaStateId,
    prec_lut: &[u8; 256],
    prec_sparse: &[DtaPrecedenceEntry],
    min_prec: u8,
) -> Option<TapeOffset> {
    let mut lhs = dta_run(state, tape, head)?;  // innermost operand
    loop {
        let b = state.peek_byte();
        let packed = prec_lut[b as usize];
        let prec = packed & 0x0F;
        if prec == 0 || prec < min_prec { return Some(lhs); }

        let is_right = (packed & 0x10) != 0;
        let two_byte = (packed & 0x80) != 0;
        let entry = if two_byte {
            resolve_two_byte(prec_sparse, state)?
        } else {
            prec_sparse.iter().find(|e| e.byte == b)?
        };
        state.advance(entry.width());
        ws_skip(state);

        // Right-assoc keeps rhs prec; left-assoc bumps +1.
        let next_min = prec + u8::from(!is_right);
        let rhs = pratt_dispatch(state, tape, head,
                                 prec_lut, prec_sparse, next_min)?;

        // ONE push_compound per operator that actually fires.
        lhs = tape.push_compound_binary(
            entry.op_rule, entry.op_discriminant, lhs, rhs,
        );
    }
}
```

Operand parsing calls back into the regular DTA driver at
`head` (innermost rung, `unary_expr` for Sheets) — unary
prefixes + primaries + postfix `%` flow through the
unchanged Seq/Alt dispatch. `min_prec` recursion collapses
six stack frames to exactly the operators that fire.
`push_compound_binary` writes `op_rule`'s `variant_idx` +
`op_discriminant` into the tape record — identical shape to
the uncollapsed emission, minus empty rung wrappers.

### 3. CSS value expressions

Two distinct surfaces:

**(a) `calc()` math.** `mathExpr = mathProduct , (("+" | "-")
>> mathProduct) *` paired with `mathProduct = mathValue ,
(("*" | "/") >> mathValue) *` is a textbook two-rung chain
and fits the existing `match_operator_chain_rule` shape test
directly. `calcFunction`, `minFunction`, `maxFunction`,
`clampFunction` call into `mathExpr`, which lifts to the
same `ShuntingYard` shape as Sheets. Parenthesised sub-
expressions (`mathValue = … | "(" >> mathExpr << ")"`)
recurse through the paren-closing routine unchanged.

**(b) Comma-separated value lists (`rgb(255,128,0)`,
varFallback).** **Not Pratt-handled.** Comma is a list
separator, not an operator — no LHS/RHS binding; the list
is a homogeneous sequence. Route through the **list-rule
recogniser (W4.1)**: `varFallback = value , ("," >> value)
*` matches the list candidate shape (Repeat over
Alt/single). Inside one argument, `mathExpr` still
dispatches to the Pratt loop — so `rgb(2*r+8, g, b)` goes
`list of three Pratt expressions`. The LUT's precedence
byte is `0` for `,`, so the Pratt loop terminates at
argument boundaries naturally.

### 4. BBNF binary expressions

BBNF has **one** relevant tower: `value_or → value_and →
value_cmp → value_add → value_mul → value_unary` in
`grammar/bbnf/expressions.bbnf:26-30`. Six rungs, disjoint
operator bytes — picks up `collect_precedence_chain` free,
produces one `ShuntingYard` state. The value-expression DSL
becomes a single Pratt loop end-to-end.

The grammar-surface operators (`|`, `,`, `?*+`, `<<`, `>>`)
are **not** Pratt candidates:

- `|` and `,` are list-separator-shaped (`alternation =
  (concatenation ?w , "|" ?) +` etc.) — Repeat over Seq,
  not operator chains.
- `?*+` are unary-**postfix** quantifiers on `modifier =
  "?w" | "?" | "*" | "+"`, attached to `factor` with no
  RHS; Pratt's RHS recursion would invent a second operand.
  The current `factor = … , modifier ?` Seq handles it.
- `binary_factor = mapped_factor , (binary_operators ?w ,
  mapped_factor) *` is a single-rung chain. The detector
  rejects single-rung chains (≥ 2 rungs required per
  [dta.rs:618](../../../../crates/ir/src/passes/recognizers/dta.rs#L618));
  the generic Seq+Repeat path handles it identically.

### 5. Grammar-mining contract

The detector emits the right shape; W4.6's contract is
**LUT materialisation** plus
[`GrammarProfile`](../../../../crates/bbnf-tape/src/profile.rs)
wiring:

- Emitter `emitter/dta.rs` gains
  `emit_precedence_lut(state_id, table)` writing
  `static __DTA_SY_{idx}_LUT: [u8; 256]` alongside the
  sparse slice. `DtaState::ShuntingYard` grows a
  `lut: &'static [u8; 256]` field next to `precedence`.
- No new IR data. `PrecedenceEntry` already carries every
  field (`byte`, `second_byte`, `precedence`,
  `associativity`, `op_rule`, `op_discriminant`); packing
  is a pure emitter-side function.
- `GRAMMAR_PROFILE.shunting_yard_lut_count: u16` (new)
  records lifted chains — bench harness asserts "≥ 1 chain
  collapsed" per expected grammar (Sheets 1, CSS-L4 1,
  BBNF 1).
- Arity bits unused — detector emits binary chains only.
  Unary prefix (`unary_prefix *`) and postfix (`"%" *`) are
  handled inside the `head` state. W4.6 writes `0b00`.

### 6. Healing `test_let_parses_as_let_call`

The failure (`crates/gorgeous/src/google_sheets.rs:51`): LET
parses as `func_call` instead of `let_call`. Root cause:
`primary = let_call | lambda_call | func_call | …` routes
through DTA `ByteDispatch` on first byte `L`, and the table
points at `func_call` because `let_call` uses a regex
(`/[lL][eE][tT]\(/`) rather than a literal prefix — the
first-byte index doesn't distinguish "L followed by ET("
from "L followed by arbitrary identifier chars then (".

Pratt lowering heals this **indirectly**: W4.6 rebuilds the
chain-head state (`unary_expr → postfix_expr → primary`).
Re-lifting forces the Alt lifter to re-examine first-byte
collisions across all operator bytes plus the head's first-
byte set (disjoint-first-byte validation at
[dta.rs:641](../../../../crates/ir/src/passes/recognizers/dta.rs#L641)).
The LET-branch regex has a lookahead prefix (`let(`/`LET(`/
mixed case) and — combined with `shape_dict_bbnf` already in
the lift pipeline — is promoted to a keyword-table entry
(`GrammarProfile::keyword_tables`) emitted as a SIMD-wide-
compare prefix match at `ByteDispatch` build time. The
keyword-table entry beats the plain-identifier default.

If the keyword-table promotion doesn't land in W4.6's touch
(per-letter case-folding is non-trivial), route the test to
**AW.5.5-adjacent Category A** with a parallel ticket under
AW.5 keyword-dispatch extension. Either way the fix is
classifier-level, not a special-case in `primary`'s Alt.

### 7. Parity verification

Three canonical inputs:

| Input        | Expected AST                | Assertion |
|--------------|-----------------------------|-----------|
| `1 + 2 * 3`  | `Add(1, Mul(2,3))`          | `BinExpr{+}` at depth 0, `BinExpr{*}` at depth 1; no rung wrappers. |
| `1 + 2 + 3`  | `Add(Add(1,2), 3)`          | Two `BinExpr{+}`, depth-2 left-heavy; `L`-assoc → `next_min=prec+1`. |
| `2 ^ 3 ^ 4`  | `Pow(2, Pow(3,4))`          | `R`-assoc → `next_min=prec` → right-heavy tree. |

Harness: `crates/core/tests/pratt_parity.rs` (new) diffs the
tape emitted by the W4.6 `ShuntingYard` path against a
fixture captured pre-W4.6 with all tower wrappers retained.
Fixture comparison filters empty rung compounds (any
`push_compound` whose child range is exactly one compound
and whose rule-id is a chain-internal rung —
`DtaTable::shunting_yard_chains` keys) from the baseline.
Shape-for-shape equality is the pass condition.

W4.6 bench checkpoint: Sheets `parse_simple ≥ 250 MB/s` (AW
hard gate, `AW.md:1146`). Pratt lowering is the last
unlanded lever against that gate (`docs/benchmarks/post-
AV.json` baseline).

---

**Citations.** Chain detector + precedence lift:
[`crates/ir/src/passes/recognizers/dta.rs`](../../../../crates/ir/src/passes/recognizers/dta.rs)
lines 155–165, 182–216, 565–660. Runtime DTA types:
[`crates/bbnf-tape/src/dta.rs`](../../../../crates/bbnf-tape/src/dta.rs)
lines 54–181. Existing emission:
[`crates/core/src/backend/rust/emitter/dta.rs`](../../../../crates/core/src/backend/rust/emitter/dta.rs)
lines 394–414. Generated output:
[`crates/core/src/grammar/generated.rs`](../../../../crates/core/src/grammar/generated.rs)
lines 171–212. Sheets grammar:
[`grammar/google-sheets/google-sheets.bbnf`](../../../../grammar/google-sheets/google-sheets.bbnf)
lines 94–164. CSS math:
[`grammar/css/l4/values.bbnf`](../../../../grammar/css/l4/values.bbnf)
lines 36–55. BBNF value-expression tower:
[`grammar/bbnf/expressions.bbnf`](../../../../grammar/bbnf/expressions.bbnf)
lines 21–34. Failing test:
[`crates/gorgeous/src/google_sheets.rs`](../../../../crates/gorgeous/src/google_sheets.rs)
line 51.
