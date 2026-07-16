# LOCK-4 · Family 4C — THE VM-CORE · pass-3 research

*Combinators lower to a compact bytecode VM: one instruction set shared by Rust and TS
(layout isomorphism by construction); packrat and diagnostics are VM facilities; the design
is measured against the closure core, never assumed to beat it.*

---

## 0. The thesis in one paragraph

A working grammar VM already exists and ships — `crates/ir/src/vm/` (~2090 LOC): a
33-variant `Op` instruction set (`bytecode.rs:45`), an `IrNode`→bytecode compiler
(`compiler/`, ~668 LOC), and a stack-machine interpreter (`interpreter/`, ~786 LOC). Both
`Op` and `BytecodeProgram` derive `Serialize`/`Deserialize`, so the **serializable-plan
observable is structural, not aspirational**. The VM is *recognizer-only today*: its `Value`
is `Span|Tagged|Array|Nil` and `IrNode::Map { inner, .. } => compile_node(inner)`
(`compiler/node.rs:31`) **discards the projection FnId** — the value plane the `->` grammar
authors is silently dropped. Family 4C's mechanism is (a) close that value-plane gap with a
serializable `Op::ApplyFn(FnId)` + a small descriptor evaluator, making the plan a *complete*
recognizer+projection program; (b) mirror the flat `Op` stream into a TS decoder so one plan
drives two lowerings to byte-identical parses; (c) home packrat and diagnostics — already
present as VM facilities — as the reference design. The honest framing (constitution vii, the
charter): the interpreted VM is **not** the throughput champion — SPEC §0 shows the
codegen/monomorphized path at JSON-tape 2.2× sonic — so 4C is measured as the **shared-ISA
correctness spine, the portable cross-language substrate, and the code-size-budget escape
hatch** when comptime-monomorphization would blow the `.rlib`/`dist` budget.

---

## 1. The mechanism made concrete

### 1.1 The instruction set (the shared ISA)

The extant `Op` enum (`crates/ir/src/vm/bytecode.rs:45`) is the ISA, and it is already
LPeg-shaped. Grouped:

- **Leaves** — `MatchString(StringId)`, `MatchRegex(StringId)`, `Epsilon`.
- **Control** — `Jump/JumpIfFail/JumpIfOk(u32)`, `Call(RuleId)`, `Return`, `Halt`, `Nop`.
- **Backtrack state** — `SaveState`, `RestoreState`, `DropState` (≅ LPeg `IChoice`/`IFail`/
  `ICommit`; ≅ parse-that closure `.or`/`.opt` checkpoint save/restore, R4 §2).
- **Whitespace** — `TrimWs`, `TrimWsPattern(StringId)`.
- **EBNF exceptions** — `Minus`, `Negate`.
- **Dispatch** — `Dispatch(u16)` (128-entry byte→branch side table `DispatchData`; interpreter
  hot arm `mod.rs:233`) and `DispatchToken(u16)`. This is the *same* O(1) first-byte dispatch
  parse-that TS builds as an `Int8Array(128)` table (`leaf.ts:100-151`) — a direct isomorphism
  anchor, already realized on both sides in different substrates.
- **Repetition** — `RepeatBegin{lo,hi,body_end}`, `RepeatEnd`.
- **Value construction** — `CaptureSpan`, `SaveValueDepth`, `DiscardRight`/`DiscardLeft`
  (the `<<`/`>>` value selection), `MakeArray(u32)`, `MakeTagged(StringId)`.
- **Packrat** — `MemoCheck{rule_id,hit_offset}`, `MemoStore(rule_id)` (`bytecode.rs:137`).
- **Diagnostics/debug** — `DebugBreak{rule_id,is_entry}` + `source_map` + `follow_sets`.

The single missing arm is the value-plane fold. 4C adds exactly one opcode:

    ApplyFn(FnId)   // pop the recognizer Value, evaluate fns[FnId] over its span, push the typed result

plus a `BytecodeProgram.fns: Vec<FnDescriptor>` side table cloned from `GrammarIR::fns`. This
is not speculative: a prototype patch already implements it (§4). The value-discard site
becomes `IrNode::Map { inner, fn_id } => { compile_node(inner); emit(Op::ApplyFn(fn_id)) }`.

### 1.2 The descriptor evaluator (Q6's Hex/Number/Enum home)

The internal descriptors `FnDescriptor::{HexConvert, NumberConvert, EnumWrap, BoxWrap,
Expr(MapExpr)…}` (`crates/ir/src/types/fn_descriptor.rs`) are today evaluated only on the
**codegen** path (`passes/payload/`, `passes/recognizers/grammar_facts.rs`). 4C homes their
*runtime* evaluation in the VM as `eval_descriptor(desc, matched_text, orig) -> Value`: a
small tree-walk over `MapExpr`/`MapBinOp`/`MapUnaryOp` — hex parse, `f64` parse, enum-tag
wrap. This is a **LOCK-3 IR-VM lowering**, not LOCK-7 host dispatch, because these descriptors
are MapExpr-expressible internal projections, not arbitrary host closures. LOCK-7's lattice
owns only the `Opaque(HostFnId)` residue (Q2/Q3) — those cannot serialize and need a TS-side
registry keyed by the same `FnId` to cross the language boundary.

### 1.3 Layout isomorphism by construction (Q5/Q6)

Because the plan is a flat `Vec<Op>` + typed side tables (all `Serialize`), a TS decoder
mirrors the `Op` tags 1:1 — structural drift is impossible where the bytes are shared. This is
LOCK-6's `6B` spec-generated-skeleton applied to the `Op` enum: generate the TS `Op` union and
the decoder switch from the Rust enum. The one non-serialized field, `compiled_regexes:
Vec<Option<Dfa>>` (`#[serde(skip)]`, `bytecode.rs:183`), is rebuilt on the decode side by
`prepare_regexes()` (`bytecode.rs:220`) — the clean cross-language seam: the plan carries regex
*patterns* (StringIds); each side compiles its own matcher (Rust bbnf-regex DFA; TS native
`RegExp`, R4 §3). **Bound (XL-D):** the fast TS face is the build-time emitter; a TS
*interpreter* over the Op stream is an isomorphism/correctness ORACLE, not the throughput face.
So "one plan, two lowerings, byte-identical parses" holds for the **opcode plane and
Value-tree** (correctness), while production TS throughput stays with the emitter.

### 1.4 Packrat & diagnostics as VM facilities (already realized)

Packrat is in the ISA: `MemoCheck`/`MemoStore` + `Interpreter.memo:
FxHashMap<(RuleId,u32),…>` + the `memo_enabled` program flag, set only when a rule has direct
left recursion (`compiler/mod.rs:99`). This is the same opt-in discipline parse-that TS uses
(packrat OFF by default, arms at first `memoize()`, `packrat.ts:290`) — the VM's `memo_enabled`
is the Rust mirror. Diagnostics are *already per-parse scoped* in the VM: `diagnostics: Vec` on
the `Interpreter` (not a module-global flag), furthest-offset tracking (`track_furthest`,
`mod.rs:327`), FOLLOW-set expected-token emission (`emit_furthest_diagnostic`, `mod.rs:335`).
This is the reference design PT-E1 asks parse-that TS to become (R4 §5: TS diagnostics gated
behind the module-global `diagnosticsEnabled`, `utils.ts:6`) — 4C's VM demonstrates the scoped
form the TS backbone should converge to.

---

## 2. The substrate & integration seams (path:line)

- `crates/ir/src/vm/bytecode.rs:45` — `Op` enum (the ISA); `:155` `BytecodeProgram`
  (Serialize/Deserialize; `entries`, `strings`, `dispatch_tables`, `follow_sets`,
  `source_map`); `:183` `compiled_regexes` `#[serde(skip)]`; `:220` `prepare_regexes` (the
  decode-side regex rebuild seam).
- `crates/ir/src/vm/interpreter/mod.rs:114` — `Interpreter::new`: **8 Vecs + FxHashMap** (Q7 —
  `values`.cap 64, `call_stack` 32, `checkpoints` 32, `repeats` 16, `memo_starts` 8,
  `value_depth_stack` 8, `rule_stack` 16, `diagnostics` lazy, `memo` lazy). `:144` `run` (the
  33-arm match dispatch loop). `:319` `collect_values_from` (value-stack→slice). `:335`
  per-parse diagnostic.
- `crates/ir/src/vm/compiler/node.rs:31` — `IrNode::Map { inner, .. } => compile_node(inner)`:
  **the value-discard defect** (violates the typed-materialization invariant); the site
  `Op::ApplyFn` repairs.
- `crates/ir/src/vm/interpreter/value.rs:52` — `Value` enum (Span/Tagged/Array/Nil):
  recognizer-only; the value plane needs typed scalar/variant arms (`U32`, `F64`, `Enum`).
- `crates/ir/src/vm/compiler/mod.rs:31` — `compile(ir) -> BytecodeProgram`; `:99`
  `grammar_needs_memo` (packrat trigger).
- `crates/ir/src/types/fn_descriptor.rs` — `FnDescriptor` (the descriptor set the VM evaluator
  consumes); `crates/ir/src/types/node.rs` — `IrNode` (the near-bijection the compiler eats).
- **Live consumers (constitution viii satisfied):** `crates/lsp/src/dap/adapter.rs:150` (the
  DAP debugger runs `Interpreter` over `BytecodeProgram` — the VM's raison d'être is the
  debug/interpret path); `crates/core/benches/json/vm.rs` (cold per-parse VM bench across
  data/twitter/citm/canada/data_xl). The VM is NOT dead surface — but it is the *interpret*
  path, distinct from the codegen fast path.
- **Cross-language:** parse-that TS `typescript/src/parse/leaf.ts:100-151` (`dispatch`
  Int8Array(128) ≅ `Op::Dispatch` `DispatchData`); `parser.ts` `.then/.or/.opt` checkpoint
  logic ≅ `SaveState`/`RestoreState`; `packrat.ts:290` (opt-in arming ≅ `memo_enabled`). TS has
  NO bytecode/VM today — the shared-ISA claim requires a new TS decoder, oracle-bounded (XL-D).
- **Reusable prototype seed:** `locks/LOCK-2/pass-1/proto-2-wf30.patch` already adds
  `Op::ApplyFn(FnId)`, `BytecodeProgram.fns: Vec<FnDescriptor>`, `interpreter/apply_fn.rs`
  (`eval_descriptor` MapExpr tree-walk), and `tests/lock4_value_plane_probe.rs` (VM vs closure
  on hex-color→U32 and recognizer-only, cold paired N=400, two windows, both orders). It also
  repoints `compiled_regexes` from `parse_that::regex::Dfa` → `bbnf_regex::Dfa` — aligning the
  VM onto the XL-C vendored regex substrate.

---

## 3. The steering questions, answered for 4C

- **Q5 (serializable-plan observable) — YES, structural.** `BytecodeProgram` is `Serialize`;
  the distinguishing observable is: bincode/JSON a program, decode in TS, run both, assert
  opcode-stream + Value-tree identity. `compiled_regexes` `#[serde(skip)]` + `prepare_regexes`
  is the clean seam. Bound by XL-D: the TS lowering is an oracle, not the fast face.
- **Q6 (isomorphism-of-record + evaluator home).** Dual oracle: (1) *opcode-stream identity* —
  serialize the Rust `BytecodeProgram`; the `Vec<Op>` is the live golden (not a hand-faked
  blob); the TS decoder must reproduce it byte-for-byte. (2) *Value-tree identity* — vs the
  live Rust `Interpreter::run().value` over json/css/value.js-hex corpora. The Hex/Number/Enum
  descriptor evaluator is a **LOCK-3 IR-VM lowering** (`Op::ApplyFn` + `eval_descriptor`), not
  LOCK-7 host dispatch.
- **Q7 (setup vs value-plane vs recognizer cost).** `Interpreter::new` is 7 eager
  `Vec::with_capacity` heap allocs (memo/diagnostics lazy). On a small parse this setup
  dominates and makes the VM look artificially slow. Elimination: a pooled `Interpreter` with
  `reset()` (truncate Vecs, `memo.clear()`, keep capacity) or a thread-local pool amortizes
  setup to ≈0 across parses. The extant `lock4_value_plane_probe` already measures
  recognizer-only vs projection-dense cold per-parse — extend it with the pooled/`new` arm to
  produce the amortization curve and the clean recognizer / `ApplyFn` / setup split.
- **Q4 (fusion policy / blowup budget).** The VM is the `{enum-node}` / `{box-here}` arm of the
  pluggable cost model: one `Op` enum, one dispatch loop, **O(program) code size, zero
  monomorphization blowup**. Named contribution: when `fuse-arity × dispatch-fanout` on a site
  would push 4B's monomorphized chains past the generated-size / incremental-compile budget on
  CSS or value.js, lower that region to bytecode (constant code size) instead. The measured cap
  is the crossover point; the ~33-arm `match` branch-predictor cost is the price for that
  ceiling.
- **Q2/Q3 (value plane / opaque barrier) — for 4C's serializability.** MapExpr-expressible
  descriptors cross the plan boundary (they serialize as `FnDescriptor`); `Opaque(HostFnId)`
  closures do not — they mark the barrier the VM plan cannot see through and the TS decoder
  cannot execute without a host-fn registry (LOCK-7). 4C's serializable-plan claim is exactly
  bounded by the non-opaque fraction of `->` sites.
- **Q1 (runtime graph → comptime).** Orthogonal to 4C: the VM *is* the runtime-plan answer — it
  interprets a comptime-compiled `BytecodeProgram`; it does not monomorphize. 4C is the route
  that *declines* comptime type-monomorphization in favour of a shared interpreted ISA.

---

## 4. Prior art (citations)

- **LPeg** (Ierusalimschy, *"A Text Pattern-Matching Tool based on PEGs"*, Softw. Pract.
  Exper. 2009) — the canonical parsing VM: PEG → bytecode → tight C interpreter with a
  backtrack stack. Its `IChar/ITestChar/IChoice/ICommit/ICall/IReturn/IFail` map almost 1:1 to
  the VM's `MatchString/Dispatch/SaveState/DropState/Call/Return/RestoreState`. LPeg is the
  existence proof that a PEG bytecode VM is competitive with hand-written parsers — the
  strongest anchor for 4C, and the standard it must be measured against.
- **Rust `regex-automata`** — the `PikeVM` and bounded-backtracker are parsing VMs over an
  `Inst` set; the `Op` enum echoes them. Relevant for the `MatchRegex` sub-VM and thread-state
  stacks.
- **CPython `ceval`** (stack machine) and **Lua 5** (register machine) — the stack-vs-register
  tradeoff; a register variant could cut `Op` count on the value-construction ops (named future
  probe).
- **Crafting Interpreters** (Nystrom), computed-goto/threaded dispatch — Rust lacks computed
  goto; a jump table or guaranteed tail-call dispatch is the equivalent. Named risk below.
- **tree-sitter** — serialized parse tables as a portable artifact; validates the "one
  serialized plan, many hosts" idiom (though LR, not PEG-VM).

---

## 5. Named risks & counterexamples

1. **The VM is the slow path by construction.** An interpreted `Op` loop cannot beat the
   codegen'd monomorphized parser (SPEC §0: JSON tape 2.2× sonic, CSS 2.4–4.2× lightningcss).
   Claiming the VM out-throughputs the closure/codegen core would be the two-arm self-deception
   the constitution forbids (SPEC §0/09 refutation). 4C's *honest* value = shared ISA +
   portability + debug + code-size ceiling; measured against the closure core per the charter,
   its expected verdict on raw throughput is a *deficit* paired with a *code-size/isomorphism
   credit*. The prototype must report the deficit, not bury it.
2. **Value-discard law violation is live today.** `compiler/node.rs:31` drops every `->`
   projection — a standing invariant breach (every `->` must reach materialization). The
   `Op::ApplyFn` repair must be proven to reach EVERY Map site with value-tree parity vs the
   codegen path, or the VM produces a structurally-correct but semantically-empty tree.
3. **Rust match-loop dispatch vs LPeg computed-goto.** The 33-arm `match` in `run` may lose
   branch-prediction to LPeg's threaded C loop. Counter-probe: measure `match` vs a
   jump-table/tail-call dispatch before concluding the VM's interpret cost.
4. **Setup tax poisons micro-benchmarks (Q7).** Unpooled `Interpreter::new` (7 allocs) makes
   small-input parses look 2–5× worse than they are. A fair measurement REQUIRES the pooled
   arm; the `vm.rs` bench currently constructs a fresh `Interpreter` per parse and must gain a
   pooled variant.
5. **Opaque host closures break the serializable plan.** Any `->` that lowers to
   `Opaque(HostFnId)` cannot cross to TS; the byte-identical-parse claim is void on those sites
   unless a shared host-fn registry (LOCK-7) exists. Census the opaque fraction first (Q3's
   ≤2% floor) — above it, the cross-language plan is partial.
6. **`follow_sets: HashMap<RuleId,CharSet128>` and `source_map` bloat the serialized plan.**
   These are diagnostics/debug side tables; a production cross-language plan should gate them
   (`#[serde(skip_serializing_if)]`) so the recognizer plan stays compact.

---

## 6. What a prototype must test to move family 4C

1. **Land `Op::ApplyFn` + `fns` table + `eval_descriptor`** (seed: the LOCK-2 proto patch);
   prove Value-tree parity VM-vs-codegen on value.js hex/number/enum `->` shapes AND json/css
   corpora. Gate: every `->` site materializes (no value discard).
2. **Cross-language byte-identity:** bincode a `BytecodeProgram`, decode in a new TS
   interpreter over a generated `Op` union; run both on the conformance corpus; assert
   opcode-stream identity + Value-tree identity. Frame the TS interpreter as oracle (XL-D).
3. **Pooled interpreter (Q7):** add `Interpreter::reset()`; measure setup-alloc share of a
   single small parse, the amortization curve (`new` vs pooled across N parses), and the
   recognizer / `ApplyFn` / setup three-way split. Extend `lock4_value_plane_probe`.
4. **Head-to-head vs closure core (charter mandate):** recognizer-only + projection-dense, cold
   paired N≥200, two windows, both arm orders (the probe's method). Report the VM's throughput
   deficit honestly and its code-size/portability credit.
5. **Cost-model crossover (Q4):** measure constant VM code size vs 4B's monomorphized-chain
   size as fuse-arity grows on CSS + value.js; locate the fuse-arity/dispatch-fanout threshold
   where the enum-node VM undercuts the monomorphization `.rlib`/`dist` budget. That threshold
   is the pluggable cap 4C contributes to the shared cost model.
