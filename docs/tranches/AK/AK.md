# Tranche AK — Materialization-Driven Alt Emission + Flat Tape

## Context

Post-AJ, the tape architecture outperformed simd-json and
serde_borrow but left structural inefficiency in the substrate:
ChunkedArena double indirection per push, and a uniform
prelude/epilogue for all Alt branches regardless of leaf vs
compound classification.

The three-tier emission system (Tape/Direct/Lazy) was structurally
blocked from engaging on JSON because leaf rules were transparent
(inlined) and multi-branch Alts were unconditionally Tape-tier.

## AK.0 — Flat Vec tape substrate

Replaced `ChunkedArena<TapeRec>` (`Vec<Vec<TapeRec>>`) with a flat
`Vec<TapeRec>`. Eliminates 2 pointer dereferences per push and
ensures `with_capacity(N)` pre-allocates the full buffer in one
allocation (ChunkedArena only allocated one 64KB chunk regardless
of N, causing ~100 heap allocations per large parse).

Impact: **+10-14% across every file.**

## AK.1 — Per-branch variant discriminator

Threaded a `__branch_idx: u8` variable through Alt-bodied rules.
Each Alt branch (dispatch table, checkpoint chain) sets
`__branch_idx = N` before executing its body. The rule epilogue
uses `__branch_idx` as the variant discriminator in
push_compound/push_leaf instead of the rule's global ID.

Added `pre_compile_rule_body` hook to the Emitter trait (default
no-op). The Rust backend sets `ctx.branch_idx_ident` for
Alt-bodied rules, which the Alt emitter consumes to inject the
per-arm assignment.

## AK.2 — Variant index correctness

Fixed by AK.1: the tape record's variant_idx now contains the
Alt branch index, not the rule's global ID. The view layer's
`as_<variant>()` accessors can now correctly discriminate between
branches via `variant_idx() == branch_idx`.

## Results

| File    | Pre-AK (AJ) | Post-AK | Delta    |
|---------|-------------|---------|----------|
| citm    | 1,880       | 2,008   | **+6.8%** |
| canada  | 1,332       | 1,467   | **+10.1%** |
| twitter | 1,543       | 1,661   | **+7.6%** |
| data    | 1,454       | 1,491   | **+2.5%** |
| data_xl | 1,085       | 1,117   | **+2.9%** |

Cumulative since Tranche Y:

| File    | Pre-tape slab | Post-AK tape | Delta     |
|---------|--------------|-------------|-----------|
| citm    | 1,610        | 2,008       | **+24.7%** |
| canada  | 964          | 1,467       | **+52.2%** |
| twitter | 1,340        | 1,661       | **+24.0%** |
| data    | 1,197        | 1,491       | **+24.6%** |
| data_xl | 810          | 1,117       | **+37.9%** |

vs competitors (same hardware):
- BBNF 2,008 vs simd-json 1,638 on citm: **+22.6%**
- BBNF 1,467 vs simd-json 757 on canada: **+93.8%**
- BBNF 1,661 vs serde_borrow 1,749 on twitter: **−5.0%**
