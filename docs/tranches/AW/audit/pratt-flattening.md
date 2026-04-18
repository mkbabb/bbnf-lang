# Pratt precedence flattening — AW-V audit

## 1. Angle headline

The six-rung Sheets operator tower does **not** cost six function calls per operator on master. The chain detector at lift time collapses all six rungs into **one** `DtaState::ShuntingYard` state (state 82, `__DTA_SY_82_PREC` carries all 13 operators — `=`, `<>`, `<=`, `>=`, `<`, `>`, `&`, `+`, `-`, `*`, `/`, `^`, `%`; `crates/core/src/grammar/generated.rs:5278`). The 93–95% Sheets regression (95/128/121 → 6/7/6 MB/s) is **not** a Pratt-mechanism cost; Sheets is walker-routed because `has_shape_dispatcher_entrypoint` rejects Seq-rooted `formula`, and the walker pays the generic `dispatch_one` → `advance_or_pop_with` → per-leaf `emit_leaf_with_payload` tax on every operand — not on the precedence climb. "Pratt precedence flattening" in the AX-scope context therefore means interpretation **(c) codegen flattening** — emit one monolithic `parse_pratt_google_sheets_<rule>` (substrate already exists at `crates/core/src/backend/rust/emitter/shapes/pratt.rs` but is never called on Sheets); grammar-rewrite G8 (interpretation (a)) is the W11 canonicalizer that makes the detector robust across wrapper zoo, **not** the performance lever.

## 2. Current Pratt architecture on master

**Detection** — `crates/ir/src/passes/recognizers/shape_dispatch/pratt.rs:58-86`. One flag check: reads `ir.pattern_annotations[rule_id].is_operator_chain` (set by `mine_recognizers` Phase 1 when a top-level Seq matches `Seq(operand, Repeat(Seq(op, rhs)))`); falls back to `ir.node_facts[node_id].operator_chain` via DAG lookup after `unwrap_map_ow`. AW-V.W4-fix-pratt commits `3b2d7dbb`/`61956c64`/`1e404ffa` widened admission to Seq/Next/Skip/Map/OW wrappers. Sheets classifies 7 rules (comparison_expr, concat_expr, add_expr, mul_expr, exp_expr, array_row, array_rows) per FINAL-V.md:75.

**Mining** — `crates/ir/src/passes/recognizers/operator_chain.rs:146-192`. Reads `DtaTable::shunting_yard_chains` *after* the DTA lift has collapsed the rung tower into **one** `ShuntingYard` state per chain. Emits `OperatorChainFacts { entries: Vec<OperatorChainEntry>, chain_heads }` — one `{byte, second_byte, precedence(1..=15), associativity, arity, op_rule, op_discriminant}` row per operator byte. Stored on `GrammarIR` (per-grammar), consumed by `backend/rust/emitter/precedence` to lower `pub const PRECEDENCE_LUT: [u8; 256]` + `PRECEDENCE_ENTRIES` slice (`generated.rs:7597`).

**Emission — tape path** — `shapes/pratt.rs:66-362`. Emits `parse_pratt_<grammar>_<rule>` monolithic body: reserve outer Rule compound (`builder.mark_children()`), dispatch leftmost operand via `<dispatcher>__value` (W5.2 per-Ref helper lifts to a direct Ref call when `operand_ref` resolves — `pratt.rs:94-99`), inline reducer loop over `PRECEDENCE_LUT[op_byte]` (one indexed byte load + three shifts for 1-byte op; linear walk over `PRECEDENCE_ENTRIES` only when bit 7 is set), local `LocalOpEntry` stack (Vec capacity 4), reducer compounds via `builder.push_compound(TapeKind::Rule,...)`. Algorithmically equivalent to the walker's inline SY arm.

**Emission — walker path** — `generated.rs:60591` (`__dta_walker_inline::run` inline SY arm; emitted by `backend/rust/emitter/dta_walker/lower_state.rs::emit_shunting_yard_arm` at AW-IV.W3.4). Reads `PRECEDENCE_LUT[__op_byte as usize]`, pops via `emit_reducer_compound` (`crates/bbnf-tape/src/driver.rs:2278`, fused SoA write), pushes op via `emit_leaf_with_payload`, returns `StepResult::Next(head_state)` where `head_state = 81` (leaf rung). Walker loop re-dispatches `head_state` — one scheduler tick per operand boundary.

**Runtime surfaces** — `crates/bbnf-tape/src/driver.rs:2083` (`DtaState::ShuntingYard` in `dispatch_one`, entry-side), `2624` (`DtaFrameKind::ShuntingYard` in `advance_or_pop_with`, reducer-side — **cold-path only** per comment at 2311-2332), `2278` (`emit_reducer_compound`, `#[inline]`), `2335` (`lookup_precedence` — `#[cold] #[inline(never)]`, LTO drops it from bench binaries when the walker never references it). Hot path uses `PRECEDENCE_LUT[byte]` one-byte-load + shifts — no function-call indirection.

**Sheets call graph on master** — `parse_google_sheets(input) → __dta_walker_inline::run(state 0) → … → state 82 = ShuntingYard{head=81, prec=__DTA_SY_82_PREC} → push SY frame → Next(81) → walker parses unary_expr → postfix_expr → primary via nested Seq/Alt states → operand complete → SY reducer arm peeks byte, LUT-dispatches, reduces-or-pushes, returns Next(81)`. **Single SY state across all 6 rungs**; one reducer tick per operator boundary. No function-call per rung — the rungs are folded by the chain detector at lift time.

## 3. Where Sheets is slow today

Post-AW-V: parse_simple 78768 ns / 6 MB/s, parse_nested 192631 ns / 7 MB/s, parse_stress 291659 ns / 6 MB/s (`docs/benchmarks/post-AW-V.json:28-32`). Post-AU baseline 5271/11333/15121 ns, 95/128/121 MB/s. Ratios 0.063 / 0.055 / 0.050. All 5 JSON entries gained +61–73% over AW-IV (JSON flipped to shape-dispatch at W3). Sheets got `+0.0% (6→6)`, `+0.0% (7→7)`, `+0.0% (6→6)`.

The ≈20× regression is **not** SY-reducer cost. It is generic walker overhead per leaf: a formula takes hundreds of `dispatch_one` ticks — every `number`, `cell_ref`, `identifier`, `string`, `boolean`, `error_literal`, function-call arg takes a full state-machine tick (indirect table load + `emit_leaf_with_payload` call + frame push/pop for enclosing Seq/Alt). JSON recovered at W3 because `value` became `parse_value_JsonParser`; Sheets is still in the generic `__dta_walker_inline::run` loop. The Pratt reducer arm itself is 14 lines of inline LUT-dispatched code at `generated.rs:60591-60700` — unambiguously the cheapest part of the Sheets call graph.

## 4. "Flattening" interpretations

**(a) Grammar-level flattening (G8 OperatorChain canonicalisation)** — `aw5-r6-depart-egraph-compile.md:69-74`. E-graph rewrite `Seq([left, Op_high, right, Op_low,...]) ≡ OperatorChain([left, right,...], [Op_high, Op_low])`. Makes Pratt detector 1 LOC (`extracted_node.is_operator_chain()`) vs today's 40-LOC wrapper-zoo matcher. **Detector ergonomics, zero Sheets throughput impact**: today's detector already classifies all 7 Sheets rungs post-W4-fix-pratt.

**(b) Runtime-state flattening** — replace per-tick SY dispatch through `dispatch_one`/`advance_or_pop_with` with a table-driven inline walk. **Already landed** — `driver.rs:2311-2322` explicitly marks the generic `advance_or_pop_with::ShuntingYard` arm cold-path-only; hot path is the inline arm at `generated.rs:60591`.

**(c) Codegen flattening (monolithic `parse_expression`)** — one `parse_pratt_<grammar>_<rule>` per Pratt rule that skips the walker scheduler. `shapes/pratt.rs:114-361` already emits this shape (inline outer compound + inline reducer loop + direct `#operand_call` resolved via `emit_ref_call_tape` to concrete `parse_<target>_<grammar>` functions). **Not reached by `parse()` on Sheets** because `has_shape_dispatcher_entrypoint` rejects Seq-rooted `formula` (FINAL-V.md:164). Substrate sits unconsumed.

**Right interpretation for AX = (c).** W0a's routing fix (AX.md:167-171) is the lever: admit Seq-rooted `formula` via entry-shape dispatch in `parse()` that delegates to `parse_flat_google_sheets_formula → parse_pratt_google_sheets_comparison_expr` instead of `__dta_walker_inline::run(0)`. G8 is cosmetic retirement of detector LOC in W11.

## 5. Comparator literature

**v8's TurboFan Pratt** (`src/parsing/parser-base.h::ParseBinaryExpression`) — templated recursive `ParseBinaryExpression<MinPrec>`; 8-bit precedence table keyed by token kind; each operator tail-calls with `new_min_prec`. Operand dispatch is `switch(Tok.getKind())` — branch-prediction-friendly, one indirect jump, not a state-machine lookup.

**Clang's `Parser::ParseRHSOfBinaryExpression`** (`clang/lib/Parse/ParseExpr.cpp`) — classic Pratt: `while (NextTokPrec >= MinPrec)`, consume op, `ParseCastExpression` (operand) through `switch(Tok.getKind())`, recurse with `new_min_prec`. Uses C call stack as the op stack via precedence-bounded recursion. No Vec-based op stack.

Both confirm: **the reducer mechanism is a loop over a precedence LUT; the throughput differentiator is operand-dispatch shape (switch vs generic scheduler), not the reducer.** bbnf's `shapes/pratt.rs` emission is topologically identical to v8/clang. Sheets is slow because every operand takes a walker tick; v8/clang's dispatch switch is isomorphic to bbnf's `parse_<grammar>__value` dispatcher, which already exists but isn't called on Sheets.

## 6. Concrete AX-scope proposal

**W0a is the Pratt lever** (`docs/tranches/AX/AX.md:73, 166-171`). Narrow `has_w4_classified` to `Pratt | Unordered`; emit entry-shape `parse()` routing for Seq-rooted grammars: Sheets `formula = /=?/ , expression` (where expression = comparison_expr) becomes `parse_flat_google_sheets_formula`, whose body calls `parse_pratt_google_sheets_comparison_expr` as the operand of the flat Seq. No Pratt-mechanism change.

**Architecture prediction**: `shapes/pratt.rs:150` is `#[inline(always)]`; emitted body uses direct `#operand_call` (resolved at emitter time to e.g. `parse_pratt_google_sheets_concat_expr`); operand recursion goes through `parse_<grammar>__value` which dispatches to per-shape leaf fns (number, string-as-hregex, cell_ref-as-hregex, func_call-as-arglist). Same call topology as v8/clang. Post-W0a, Sheets parse_simple recovers to ≥ post-AU 95 MB/s without a Pratt-mechanism change.

**If W0a alone does not reach parse_stress 121 MB/s**, the residual is per-shape emitter quality (ArgList for `func_call`, HRegex for `cell_ref`/`identifier`, Wrap for `range_end`) — NOT the Pratt reducer. Flattening the operator tower differently will not help. The AX intervention becomes operand-dispatch inlining: emit the Sheets `primary` 10-branch switch (let_call, lambda_call, func_call, number, string, boolean, error_literal, cell_or_range, identifier, paren_expr) monomorphized inside `parse_pratt_*` bodies so LLVM sees the full call tree — exactly v8's shape. This sits under W5/W6 shape-emitter polish, not a Pratt wave.

**Gate for the Pratt fix**: `nm target/release/deps/google_sheets_monolithic-*` shows `parse_pratt_google_sheets_*` symbols and does NOT show `__dta_walker_inline`; parse_stress ≥ 121 MB/s = post-AU parity; samply hottest self-time = `parse_pratt_google_sheets_comparison_expr` or an operand shape fn, not `dispatch_one`/`advance_or_pop_with`.

## 7. AX wave placement + sequencing

**W0a** (AX.md:73, Block A) — routing fix. This is where Sheets Pratt unblocks — substrate exists; the gate flips. No Pratt-internal work. Two agents serial.

**W11** (AX.md:91) — G8 OperatorChain e-graph rewrite. Detector-architecture cleanup: retires `shape_dispatch/pratt.rs`'s 86-LOC matcher, replacing with a 1-LOC `extracted_node.is_operator_chain()` query. **Not a Sheets throughput lever.** Gating Sheets on W11 would be a 10-wave deferral of what W0a closes.

**Dependencies**: W0a depends on nothing Pratt-specific (just `has_w4_classified` narrowing + Seq-rooted `parse()` dispatch); W11 depends on W10 (universal rewrites G1-G4 + e-graph infrastructure). Ordering: W0a → parse() routing → Sheets recovers → W10/W11 retires detector LOC without affecting throughput. The AX.md ordering is correct. Do not move G8 earlier thinking it is the Sheets lever.

**Sheets Pratt mechanism is correct on master. The emitter is correct. `PRECEDENCE_LUT` is correct. What is missing is consumer wiring from `parse()` to `parse_pratt_*`. That is W0a — not a Pratt flattening wave.**
