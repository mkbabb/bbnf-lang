# AW-V R5 — Departure Thesis A: Rip the DTA Interpreter

## 1. Angle headline

**The W2.1 prototype is already Path B for JSON. Generalise its architecture — fn-per-rule over `bbnf-simd-scan` + `bbnf-tape::Columns` — to all four grammars via the shape-emitter classifier. Delete the DTA interpreter (`driver.rs` + `dta.rs` + `dispatch_one` + `dta_walker/`) from the hot codegen path, keep only the data types (`DtaSnapshot`, `DtaState`, `DtaTable`) and a vestigial `dta_run_cold` for AX replay. R6's §7 prediction — that Path A's W5.6 is Path B wearing a badge — is now evidence-backed.**

## 2. Motivation: W2.1 evidence validates R6's §7

W2.1 (`crates/bbnf-json-prototype/`, 2,246 LOC) beats sonic-rs on all five twin-pair entries (0.88–0.94×; `docs/benchmarks/post-AW-V-W2-prototype.json`). The `nm` probe on the bench binary is empty for `dispatch_one | try_branch | advance_or_pop_with | __dta_walker_inline | DtaState | FrameStack` (`docs/tranches/AW/AW-V-W2-close.md` §Symbol-presence verification). Samply: 91.15% self-time on one monomorphised `parse_value::<ValueVisitor>` symbol — sonic-rs's hand-tuned twin sits at ≤88% across two symbols; the prototype is more inlined than sonic itself.

The prototype consumes `bbnf_tape::Columns` via `TapeVisitor` (`crates/bbnf-json-prototype/src/visitor.rs:351+`) and `bbnf-simd-scan`'s `nospace64` + `first_quote_or_backslash` kernels (§simd.rs). **Both routes are AX-compatible**: `TapeVisitor.finish()` returns a `bbnf_tape::Tape`, bit-identical to what `dispatch_one` replay would produce. This is the key empirical load-bearing finding — the substrate proves sufficient without the interpreter.

R6 (§7) predicted: *"The difference between 'inline the DtaState match per grammar' (W5.6) and 'emit a function per rule' (Path B) is semantic, not architectural. Path A's W5.6 is admitting Path B; it keeps DTA branding for continuity."* Current master has shipped W5.6 as `dta_walker/` (3,875 LOC emitter, per `crates/core/src/backend/rust/emitter/dta_walker/*.rs`). Inspect `generated.rs:4199` — every emitted per-state function reads `let table: &::bbnf::runtime::tape::DtaTable = &DTA_TABLE;` then dispatches on `table.states[N]`. That is runtime indirection through a const the emitter knows at codegen time — the `HOIST-EMITTER-KNOWN-DATA-INTO-EMITTED-CODE` invariant (README.md §Architecture invariants) is violated. This is the direct cause of AW-IV's 70.9–78.9% `try_branch` self-time floor on CSS L4.

## 3. Updated delete manifest (master as of 2026-04-18)

| Category | File | LOC | Fate |
|---|---|---:|---|
| Interpreter runtime | `crates/bbnf-tape/src/driver.rs` | 3,323 | Split: keep `dta_run_cold` + helpers + `DtaSnapshot` (~500 LOC), delete `dispatch_one` hot-path pollution (~2,800 LOC) |
| Interpreter types | `crates/bbnf-tape/src/dta.rs` | 550 | Keep (data only; `DtaState` + `DtaTable` + `DtaStateId` are still AX replay substrate) |
| W4.b per-state emitter | `crates/core/src/backend/rust/emitter/dta_walker/` | 3,875 | Delete entirely |
| W5.6 dta emitter | `crates/core/src/backend/rust/emitter/dta.rs` | 935 | Delete (superseded by shape-emitter) |
| IR DTA lifter | `crates/ir/src/passes/recognizers/dta.rs` | 1,513 | Keep (shape classifier needs `DtaState` facts as IR input) |
| **Hot-path emitter path deleted** | — | **~7,610** | — |
| Emit fresh | `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar,pratt,unordered,arglist,flat,wrap,hregex}.rs` | ~2,000 (est.) | **NEW**, per W3.2 + W4 |

Net delta: **−5,600 LOC** (and the deleted LOC is the code causing the performance floor).

## 4. AX impact — per-consumer survival analysis (AX.md X0–X10)

| Consumer | Requirement | Survives DTA demotion? |
|---|---|---|
| **X0.1 inspect-log** | parses with `dta-replay` enabled, dumps `(offset, dfa_state_id, transition_taken, frame_depth)` (AX.md:167-170) | **Yes.** `dta_run_cold` + `DTA_TABLE` preserved verbatim as cold-path replay. Inspector binary calls `dta_run_cold` with a log-collector regex adapter. |
| **X0.2 minimiser** | binary-search shrink via decision log | **Yes.** Uses same cold-path dispatch as X0.1; log is deterministic replay of `dta_run_cold`. |
| **X0.3 log-round-trip harness** | assert cold-parse tape == log-replay tape (AX.md:180-186) | **Yes** — but semantics shift: the "cold parse" IS the replay (since hot path no longer uses DTA). The harness asserts `dta_run_cold(input) == dta_run_cold(replay(log))`, which is a property of `dta_run_cold` alone. |
| **X1 snapshot persistence** | `DtaSnapshot` serde + `parse_resume(snapshot, input)` entrypoint | **Conditional.** Option A (minimal): resume from snapshot reads `dta_run_cold`; cold-parse does the incremental work at interpreter speed, not hot-path speed. Option B (see §7): re-architect onto RD-checkpoint entry points generated per shape. |
| **X2 incremental re-parse** | edit-localisation + subtree re-walk + Columns splice (AX.md:188-239); gate ≤200 µs on 100 KB CSS edit | **Conditional.** The **snapshot metadata column** (per-record `(TapeOffset, DtaSnapshot)` overlay) can be populated either during the hot parse (shape-emitter stamps `DtaSnapshot`-shaped checkpoint records at shape-begin) OR from cold-replay (slower but AX-correct). Gate needs shape emitter to emit checkpoint stamps at shape boundaries — mechanical. |
| **X3 recovery** | structural-default + `@recover` override | **Yes.** Sync-byte skip logic inlines into shape emitters (next structural byte from `DelimScanMiner` output); the driver-level frame walk moves into generated code. `dta_run_cold` remains the ground-truth fallback. |
| **X4–X7** (closures / analysis / gorgeous / imports) | subsystem test closures, no DTA coupling | **Yes, orthogonal.** |
| **X8 LazyValue** | `LazyRef` tape kind + `should_descend` visitor hook + shape-cached re-entry via `parse_into::<V>()` (AX.md §Phase 8) | **Yes, and BETTER.** X8 already re-uses shape-dispatch (AX.md:383-386: *"Grammar-agnostic because shape-dispatch is grammar-agnostic"*). The shape-emitter IS the re-entry substrate. DTA interpreter plays no role in X8. |
| **X9 speculative parsing** | shape-transition Markov predictor + rollback scratchpad; gate CSS ≥0.75 hit-rate | **Yes, and BETTER.** X9's predictor mines shape transitions (AX.md:412-414). The shape emitter is the natural host. Rollback uses `Columns::reserve/rewind` — already works against the existing substrate, no DTA dependence. |
| **X10 user multi-visitor** | `#[derive(Visitor)] #[emit_paired_with]` macro | **Yes, and BETTER.** Monomorphises shape emitters per visitor pair; macro targets `emitter/shapes/*.rs` not `emitter/dta.rs`. |

**Verdict**: AX consumers X0, X3–X10 survive cleanly with `dta_run_cold` as cold-only. X1 and X2 need the per-record snapshot-metadata column to be populated by the shape emitter at shape-begin boundaries — that's a 50-line addition per shape (one stamp instruction at compound open), not an architectural barrier.

## 5. Bootstrap recipe — avoiding the circular dependency

README.md §Self-host circular-dependency escape names the hazard: `generated.rs` is produced by the macro via the **compiled** `bbnf` lib's `BbnfBootstrap::parse`. Swapping emitters from `dta_walker/` to `shapes/` triggers the cycle if the post-rewrite `BbnfBootstrap::parse` cannot parse `bbnf.bbnf` before the shape emitter regenerates it.

Recipe (adapting AW-I.W4ζ pattern, commits `87f65214` + `49656fd4`):

1. Keep `dta_walker/` emitter for **only BBNF's generated.rs** one commit before deletion; new `shapes/` emitter drives every other grammar.
2. Run `scripts/bootstrap-bbnf.sh`: old walker parses `bbnf.bbnf` into AST, new shape emitter produces the BBNF grammar's `generated.rs` (shape-shaped).
3. Verify `cargo check -p bbnf --lib` compiles against the new shape-shaped BBNF generated.rs.
4. Re-run bootstrap: now the BBNF lib has shape parser, emitter is still shape-based → output stabilises.
5. Delete `dta_walker/`.

**Risk**: if the shape classifier gives BBNF `<75%` coverage (AW-V.md projects 75%), the `ShapeTag::None` fallback still needs a runtime — but the fallback is `dta_run_cold` (already compiled), which is the interpreter route the README.md explicitly preserves. No circularity.

## 6. Generalisation risk — shape count vs rule count vs state count

| Grammar | Rules | DTA states | Shape categories (H1, 11) | Rules → shape fns |
|---|---:|---:|---:|---|
| JSON | 6 | ~22 | 6 | 6 fns (prototype proves this works) |
| Sheets | 32 | ~110 | 10 | ~32 fns |
| BBNF | 38 | ~180 | 9 | ~38 fns |
| CSS L4 | 165 | ~800 | 11 | ~165 fns |

Path A's W5.6 inlining risk (R5 §4) was: *"CSS L4's state count is ~800; even per-state arm inlining could exceed LLVM's default threshold, causing function-call-per-state that re-introduces the dispatch floor."* That risk applies to **state-level** inlining. Path B emits **rule-level** functions; per H1 `crates/ir/src/passes/recognizers/shape_dispatch/{object,array,string,...}.rs`, 165 CSS rules fit into 11 shape templates. Each rule's body is bounded by its shape template (~80–200 LOC instead of 800 states × per-state arm sizes).

Concretely: JSON's 6 rules emit into 6 ~80-LOC fns inlined into `parse_value` (prototype evidence, `cargo expand` shows all 5 shape fns `#[inline(always)]` bodied into one 91.15%-self-time symbol). For CSS L4's 165 rules over 11 shapes, the emitter generates 165 fns partitioned by shape; each fn's body is the corresponding shape template specialised to that rule's IR miner output (FIRST set, structural alphabet, child-rule refs, payload type). LLVM's inline budget applies *per call site*, not across sibling fns — a function not called from the hot path never enters the inliner's decision. `parse_stylesheet` inlines `parse_rule_block` (Object-shape); `parse_rule_block` inlines `parse_declaration` (Flat-shape); call depth is grammar-structural (max 12 per AW-V.md §FrameStack), not 800.

**Verdict: scales.** The shape emitter is a codegen-time tree of specialised inlines shaped by the grammar's call graph. State count is an interpreter metric; rule count is the compiler's scope; shape count is LLVM's alphabet. 11 shapes × grammar-specific bodies ≪ 800 DTA state arms.

## 7. Honest alternative — should AX rework onto RD-checkpoint?

If Path B is strictly cleaner than "Path B + vestigial DTA replay," the only defensible reason to keep `dta_run_cold` is AX's X0 decision-log replay. Let's be honest about whether AX can live without the DTA interpreter entirely.

**The DTA decision log is intrinsic to the DTA interpreter shape.** `dispatch_one` emits one log entry per state visit; replaying means re-running the state machine with the log as tape. If the hot path never runs `dispatch_one`, the log's fidelity is a replay-of-an-alternate-parser property — useful as a debugging sketch, not as ground-truth bijection with the hot path.

**RD-based decision log is the honest formulation** (R6 §6): instrument shape emitters to emit `record_decision(rule_id, shape_branch_idx)` at each Alt/Wrap/Keyword-dispatch point. The log's entries map 1:1 to the hot parse's decisions. Replay = call `parse_<shape>_<rule>` with branch-hints injected. Snapshot = `(offset, frame_depth, shape_stack)` at any shape boundary (natural list-item checkpoints that already exist). The AX substrate that moves: `DtaSnapshot` → `ShapeSnapshot` (same shape, different field names); `dispatch_one`-based replay → shape-emitter hint-path.

**AX rework cost estimate** (per AX.md critical files):
- `crates/bbnf-tape/src/snapshot.rs` (NEW, ~300 LOC): `ShapeSnapshot` shape + serde
- `crates/bbnf-tape/src/replay.rs` (NEW, ~200 LOC): log-record/replay against shape emitter
- `crates/bbnf-tape/src/incremental.rs`: unchanged approach, different snapshot type
- Retire `dta_run_cold` + `dispatch_one` + `DtaTable` (~3,300 LOC deleted)

Net: AX gains ~500 LOC of cleaner code, loses 3,300 LOC of interpreter runtime. X0's behavior is preserved bit-for-bit; every other X consumer is unaffected or improved.

**Recommendation**: delete `dta_run_cold` in W6. The cold-path-replay line item in AW-V.md §invariants.3 is vestigial — it was a continuity hedge, and the W2.1 evidence plus the X8/X9/X10 design (which all use shape-dispatch) show AX never actually needed it.

## 8. Recommended AW-V W3+ shape — rename, retire, re-invariant

Concrete rewrites to `docs/tranches/AW/AW-V.md`:

1. **§"Compile DTA into hot-path code — not abrogate"**: replace title and table with *"Emit fn-per-rule over shape templates. DTA substrate is IR input, not runtime."* The shape-mined row becomes the only row.

2. **§Invariants.3**: strike *"AX replay-surface preserved. `bbnf_tape::driver::dispatch_one` + helpers + `DtaState` variants + `DTA_TABLE` + the cold-path table-interpretive path continue to exist."* Replace with: *"AX substrate is `ShapeSnapshot` + shape-emitter hint-path replay. `DtaState`/`DtaTable` are retired after W6."*

3. **§Wave schedule W3.2**: rename `parse_<shape>_<grammar>_<rule><V: ShapeVisitor>` to what it is — `parse_<rule><V>` with a shape-selected body template. Drop the `<shape>` infix; it's a codegen internal, not a runtime identity. The emitter looks up `ShapeAssignments::get(rule) → ShapeTag::Object` and splices the Object template body into `parse_<rule>`. Fn-per-rule; bodies selected by shape.

4. **§Wave schedule W6**: add a new line item "Retire `dta_run_cold` / `DtaTable` / `dispatch_one` / `dta_walker/`" — alongside the parity harnesses. This is the honest close.

5. **Delete manifest**: `emitter/dta_walker/` (3,875), `emitter/dta.rs` (935), `driver.rs`'s `dispatch_one` + `try_branch` + `advance_or_pop_with` (~2,800). **Net −7,600 LOC**; keep only `DtaSnapshot` data type, `dta_run_cold` (500 LOC) through W5 as bootstrap bridge, then delete in W6.

The architectural thesis becomes: *"fn-per-rule over shape templates; the DTA was a scaffold for shape-mining; the scaffold is the consumer's mirror, and once the consumer is emitted, the scaffold goes."*

---

**Summary of evidence**: W2.1 beats sonic-rs using zero DTA substrate (nm-verified). Shape classifier (`crates/ir/src/passes/recognizers/shape_dispatch/mod.rs`) already exists and mines all 6 W3 shapes. `emitter/shapes/` is the unshipped step. AX's X0 is the only AX consumer with a residual DTA dependency, and X0's requirements survive with either a minimal `dta_run_cold` (R6 hedge) or a cleaner RD-checkpoint rework (§7 alternative). R6's §7 prediction is now evidence-backed: Path A's W5.6 (shipped as `dta_walker/`) IS Path B with continuity branding. **Rename `dta_walker/` → `shapes/`, delete the interpreter, ship the honest formulation.**
