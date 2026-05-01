# AX.W0a.2.g — partial close progress report

## Status

Partial. D1 (Keyword Ref-led Alt extension) landed and committed pre-wave
at `7b159a72`; D2 decomposed further than the scope-reveal predicted and
landed three targeted walker-parity fixes (inline Alt ByteDispatch split,
Flat repeat column-truncation, Array-list variant stamping). D4
admission widening was prepared (predicate retirement + per-grammar
predicate-test flip) but reverted after per-grammar tape_parity
regressions surfaced across every non-JSON grammar — the scope is
larger than W0a.2.g can absorb without crossing multiple sub-wave
boundaries.

## What landed (committed, narrow predicate preserved)

| Area | Fix |
|---|---|
| Keyword emitter | Ref-led Alt branches admitted (W0a.2.g, `7b159a72`). BBNF's `directive = import_directive \| ...` now emits a per-first-byte arm that delegates to each Ref target's shape fn via `emit_ref_call_tape`. Signature `(input, p, first_byte, state, builder)` threads `state` for the Ref delegation; single-literal forms still ignore it via `_state`. Six call sites updated (dispatcher Alt-arms, wrap branch-calls, `emit_ref_call_tape`, visitor-path analogues). |
| Inline Alt | Walker-parity ByteDispatch vs AltLinear split (`shapes/inline.rs::emit_alt_byte_dispatch_tape`). The IR's `Alt(branches, Some(AltDispatch))` variant lowers to `IrState::ByteDispatch` in the walker, which transitions to the chosen branch WITHOUT pushing an Alt compound. The pre-W0a.2.g inline emitter pushed an Alt compound unconditionally, inflating tape record counts. Now routes dispatch-table Alts through a compound-less per-byte match; `AltLinear` (no dispatch table) retains the Alt compound for its own walker parity. |
| Flat repeat | Column truncation on iter failure and zero-width success (`shapes/flat.rs::emit_tape_repeat` generic-case). Walker's `handle_repeat_failure_bounded` rolls back `columns` to the iter's savepoint so orphan leaves pushed inside a failed or zero-width iter don't leak. Shape emission was pushing the leaves but skipping the iter-Seq compound close, leaving the leaves orphaned in the tape. Truncation preserves walker parity. |
| Flat Seq-inner | When a Repeat's inner is itself a Seq, the iter-Seq compound IS the walker's `IrState::Seq` compound — no double Seq wrap. `emit_tape_repeat` now flattens `IrNode::Seq(children)` inners into direct-children emission inside the single iter-Seq. |
| Array list | Shape 2 (entry-rule list) outer Rule and outer-OW Seq compounds now stamp `variant=0` (walker parity: top-level `pending_variant_idx == u8::MAX` lowers to 0). Previous `rule.id & 0xFF` stamping would mismatch `tape_parity` goldens under admission. |
| Array list | `has_iter_ow` predicate extended to admit `IrNode::Seq`, `IrNode::Next`, `IrNode::Skip` as walker-Seq-producing inners (not just `OptionalWhitespace`). EBNF's `grammar = (S, rule, S)*` uses a bare `Seq[Regex, Ref, Regex]` inner; under admission the emitter must push a per-iter Seq for walker parity. |
| Regen helper | `regen_shape_goldens.rs` exercised to refresh the `shape_dispatch_emission/keyword.rs.expected` golden for the W0a.2.g state-threaded Keyword signature. |

## What reverted (halt per non-negotiable)

| Item | Rationale |
|---|---|
| `body_has_dispatcher_fallback_position` deletion | Widening surfaced per-grammar tape_parity regressions beyond the two named in `post-AX-W0a2f-progress.md §Remaining-blockers`. Every non-JSON grammar exhibited structural walker-parity mismatches in its Flat/Alt/Repeat body emission. BNF was fixable with the three walker-parity fixes above (34 records exactly); CSS / Sheets / EBNF / BBNF / BbnfBootstrap remained divergent. Predicate retained to keep master test-green. |
| `gate_predicate_wire_contract.rs` flip | Restored to the narrow `has_shape_dispatcher_entrypoint == false` expectations for 6/7 grammars. |

## Per-grammar rollout status

| Grammar | Status | Root cause (if blocked) |
|---|---|---|
| JSON | Admitted pre-W0a.2.f, unchanged. `tape_parity_json` 7/7. |
| CSS L4 | Walker-routed. Admission probe revealed deep walker-parity issues across the 98 classified entry-reachable rules; each Flat / ArgList / Wrap emission has structural mismatches vs the walker's `IrState::Seq` / `IrState::Repeat` record layout. |
| Sheets | Walker-routed. Same class of mismatches as CSS; `formula` Flat emission diverges at multiple positions. |
| BBNF | Walker-routed. `directive` Keyword now admits Ref-led branches (W0a.2.g D1) but the rest of the entry-reachable body — `rule`, `alternation`, `concatenation`, `factor`, `term` — all show Flat walker-parity deltas. |
| EBNF | Walker-routed. `grammar = (S, rule, S)*` plus the letter/digit/symbol Alt chains generate tape-structure deltas the shape emitter doesn't reproduce. |
| BNF | Walker-routed. Admission probe achieved walker parity (34 records exactly, variant=0) after the three landed fixes; admission retained as walker-routed for W0a.2.g but proven ready for W0a.2.h. |
| BbnfBootstrap | Walker-routed. Inherits BBNF blockers plus structural pipeline's additional variants. |

## Detailed blocker diagnosis

### 1. Inline Alt compound vs ByteDispatch (FIXED)

IR representation: `IrNode::Alt(branches, dispatch: Option<AltDispatch>)`.
When `dispatch = Some(_)` (byte-dispatchable Alt) the walker lowers to
`IrState::ByteDispatch`, which transitions without pushing an Alt
compound. When `dispatch = None` (linear retry) the walker lowers to
`IrState::AltLinear`, which pushes an Alt compound via
`columns.push_compound_fused(TapeKind::Alt)`.

Pre-W0a.2.g `inline::emit_inline_position_tape` treated both cases
identically — pushed an Alt compound unconditionally. Under admission
this added a `TapeKind::Alt` record wherever the walker had no
corresponding record.

Fix: split the Alt arm on the dispatch discriminator
(`IrNode::Alt(branches, Some(_))` → `emit_alt_byte_dispatch_tape`
without compound; `IrNode::Alt(branches, None)` → `emit_alt_tape` with
compound).

### 2. Flat `emit_tape_repeat` generic-case column leaks (FIXED)

Walker's `handle_repeat_failure_bounded` captures
`columns.savepoint()` at iter entry and calls
`columns.truncate(save_cols)` on Err or zero-width success. Shape's
`emit_tape_repeat` generic-case saved `*p` (for rollback) but not
`columns.len()`, so any leaf record pushed inside a failed / zero-width
iter lived forever in the tape.

Fix: capture `save_cols = builder.columns_mut().len()` at iter entry;
truncate back on Err and on `*p == save_p` zero-width success.

### 3. Flat Repeat(Seq) double-Seq wrap (FIXED)

Walker's `IrState::Repeat { inner: Seq }` lowering pushes the Repeat
(Rule) compound and transitions to the Seq's `IrState::Seq`, which
pushes the Seq compound. Net: ONE Rule + ONE Seq per iter.

Shape's `emit_tape_repeat` inner handling called
`emit_tape_position_core(IrNode::Seq)` which also pushes a Seq compound,
and `emit_tape_repeat`'s own iter-Seq push wrapped it. Net: ONE Rule +
TWO Seqs per iter.

Fix: `emit_tape_repeat` now pattern-matches the inner. `IrNode::Seq` →
emit children directly inside the iter-Seq; non-Seq → recurse via
`emit_tape_position_core` unchanged.

### 4. Array-list outer compound variant stamping (FIXED)

Walker's top-level entry call arrives with
`pending_variant_idx == u8::MAX` (no Ref has stamped a variant yet),
which the Rule/Seq frame's push lowers to 0. Shape 2 emitter stamped
`rule.id & 0xFF` unconditionally, producing variant=N instead of
variant=0 on the outer compound.

Fix: Shape 2 is only reached from the entry dispatcher (Shape 1 handles
nested Array positions like JSON's `array`); stamp `variant=0` on the
outer Rule and outer-OW Seq compounds.

### 5. Array-list `has_iter_ow` narrow detection (FIXED)

Walker's per-iter Seq compound fires for every inner that lowers to an
`IrState::Seq` — which includes `IrNode::Seq`, `IrNode::Next`,
`IrNode::Skip`, and `IrNode::OptionalWhitespace`. Pre-W0a.2.g the
predicate admitted only `OptionalWhitespace`, missing EBNF's
`Repeat(Seq[Regex, Ref, Regex])`.

Fix: `has_iter_ow` widened to `OptionalWhitespace | Seq | Next | Skip`.

### 6. Open blockers (DEFERRED to W0a.2.h)

After the 5 fixes above, BNF achieves 34-record walker parity. The
remaining grammars show additional walker-parity mismatches:

- **Flat's `walk_positions` flattens `Next` / `Skip`**. Walker's
  `IrState::Seq { children }` fires for `Next(a, b)` and `Skip(a, b)`
  with 2 children per compound. Flat's `walk_positions` traverses
  `Next` and `Skip` recursively, flattening into a flat position list
  — collapsing the 2-child Seq compound into direct children. BNF
  `nonterminal = "<" >> identifier << ">"` lowers to
  `Skip(Next("<", identifier), ">")` and walker pushes an inner Seq
  compound (for the Next) that shape doesn't.

- **Flat `Ref` to HRegex-classified targets**. HRegex emitters
  (`parse_hregex_<grammar>_<rule>`) push a `Span` leaf only; they don't
  push the enclosing Rule compound the walker emits for a Ref frame.
  Shape's `emit_ref_call_tape` delegates to the target's shape fn
  without wrapping in a Rule compound; walker's `IrState::Ref` arm
  pushes a Rule compound (fused) before transitioning to the target's
  entry state.

- **Repeat `iter_count < lo` guard column leak**. Generic-case
  `emit_tape_repeat` returns Err when `iter_count < lo` AFTER already
  closing iters that pushed records. Walker's lo-check happens BEFORE
  per-iter pushes land so failed parses don't leave compound records.

- **Zero-length Seq elision**. Walker's `IrState::Seq { children }` with
  empty children elides the compound push. Shape's
  `emit_tape_position_core(IrNode::Seq)` always pushes.

None of these blockers are isolated to the `has_iter_ow` / variant_idx
scope-reveal from W0a.2.f. They constitute a broader Flat/ArgList
walker-parity audit that belongs in W0a.2.h.

## Bootstrap regen status

**Final (HEAD + W0a.2.g fixes):** `diff gen1 gen2 | wc -l = 0`
across 96 886 lines. Two cycles byte-identical. Net increase vs the
W0a.2.f baseline (96 839 lines) is 47 lines — the Keyword Ref-led
emission + inline Alt ByteDispatch split + Flat repeat truncation
collectively add small emitted bodies to the generated parse fns.

## Workspace test suite

**1615 passed, 0 failed** across all per-grammar binaries and shape-
dispatch emission goldens. No regressions introduced; admission stays
narrow and the walker path continues to route every non-JSON grammar.

## Hard-gate status

| Gate | Status |
|---|---|
| 1. Keyword Ref-branch wire-contract test | **Met** — `keyword_ref_branch_wire_contract` passes (landed with `7b159a72`). |
| 2. `tape_parity_bnf` passes | **Met** under narrow predicate. Under widening probe: achieved 34-record walker parity after all five W0a.2.g fixes; proof artefact captured in the predicate-flip experiment. |
| 3. All per-grammar tape_parity tests | **Met** under narrow predicate (json 7/7, css_l4 4/4, sheets 3/3, bbnf 4/4, ebnf 4/4, bnf 1/1). |
| 4. `has_shape_dispatcher_entrypoint == true` for all 7 grammars | **Unmet** — admission widening reverted pending the W0a.2.h Flat walker-parity audit. |
| 5. `parse()` zero walker-reach for 6 non-JSON grammars | **Unmet** — same rationale as gate 4. |
| 6. `cargo test --workspace --no-fail-fast` | **Met** — 1 615 passed, 0 failed. |
| 7. Bootstrap regen idempotent | **Met** — 96 886 lines, byte-identical across two cycles. |
| 8. `body_has_dispatcher_fallback_position` deleted | **Unmet** — predicate retained pending W0a.2.h. |

## 7-grammar predicate table (final HEAD)

| Grammar | `has_w4_classified` | `has_full_shape_coverage` | `has_shape_dispatcher_entrypoint` |
|---|---|---|---|
| JSON | false | true | **true** |
| CSS L4 | true | true | false |
| Sheets | true | true | false |
| BBNF | true | true | false |
| EBNF | false | true | false |
| BNF | false | true | false |
| BbnfBootstrap | true | true | false |

## Re-plan suggestion for W0a.2.h

Single-agent wave on the Flat emitter:

1. **Flat Next/Skip walker-parity**. Replace `walk_positions`'s recursive
   flattening with a structural emitter that preserves Seq compounds at
   every `Next`/`Skip` boundary. Emission becomes tree-shaped rather
   than flat-list shaped; matches walker's `IrState::Seq { children }`
   push at every structural node.

2. **Ref→HRegex Rule-compound wrap**. When `emit_ref_call_tape` targets
   an HRegex-classified rule, wrap the call in a Rule compound
   (walker parity — the Ref's frame push is a Rule). Alternatively,
   HRegex emitters can push the Rule compound themselves; the
   delegation model decides.

3. **Repeat `iter_count < lo` pre-check**. Hoist the lo-check to iter
   entry; fail before per-iter records land.

4. **Per-grammar admission probe**. After Flat walker-parity lands,
   re-run the predicate-flip + per-grammar tape_parity. Expected
   outcome: all 6 non-JSON grammars pass tape_parity under admission.

Estimated effort: ~150-200 LOC in `shapes/flat.rs` + `shapes/inline.rs` +
`shapes/hregex.rs`; ~1 agent-hour.

## Artefacts

- `docs/benchmarks/post-AX-W0a2f-fallback-probe-output.md` —
  per-grammar fallback-position enumeration.
- `docs/benchmarks/post-AX-W0a2f-extract-probe-output.md` —
  per-grammar entry-rule body trees.
- `docs/benchmarks/post-AX-W0a2f-progress.md` — predecessor progress
  doc (W0a.2.f partial close).
- `crates/core/src/backend/rust/emitter/shapes/inline.rs::emit_alt_byte_dispatch_tape` — W0a.2.g fix #1.
- `crates/core/src/backend/rust/emitter/shapes/flat.rs::emit_tape_repeat` — W0a.2.g fix #2 + #3.
- `crates/core/src/backend/rust/emitter/shapes/array.rs::emit_parse_array_list` — W0a.2.g fix #4 + #5.
