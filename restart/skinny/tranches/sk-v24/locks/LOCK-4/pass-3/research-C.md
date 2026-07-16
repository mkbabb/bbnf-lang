# LOCK-4 pass-3 · research-C — Family 4C: THE VM-CORE

*Combinators lower to a compact bytecode VM — ONE instruction set shared by Rust and
TS, layout-isomorphic by construction; packrat and diagnostics are VM facilities;
measured against the closure core, not assumed.*

---

## 0. The decisive fact: the VM-CORE already exists (grammar-side), fully

Family 4C is **not** speculative. A complete, production-grade parsing-machine VM is
live in bbnf-lang at `crates/ir/src/vm/` (1772 src LOC across `bytecode.rs`,
`compiler/{mod,emit,node,rule,compound}.rs`, `interpreter/{mod,leaves,control,repeat,
construct,memo,value}.rs`, `debug.rs`). It compiles the combinator-isomorphic
`bbnf_ir::IrNode` (`crates/ir/src/types/node.rs:30`) to a serializable
`BytecodeProgram` and executes it in a tight explicit-stack loop. It is already
benched cold per-parse over the shared corpora at `crates/core/benches/{json,css,
google_sheets}/vm.rs`. The family's question is therefore **not "can a shared VM
exist" but "does parse-that's combinator core lower to THIS VM at a cost that beats
its closure core, and is the VM byte-identical across Rust and TS?"** — a measurement,
per the charter.

The three planes of the near-bijection this family rests on:

| Combinator (parse-that) | IrNode (`node.rs`) | VM Op (`bytecode.rs:45`) |
|---|---|---|
| `string` / `leaf.ts:string` | `Literal(StringId)` :33 | `MatchString(StringId)` |
| `regex` | `Regex(StringId)` :36 | `MatchRegex(StringId)` + DFA |
| `then`/`all`/`seq!` | `Seq(Vec<IrNode>)` :43 | inline sequence |
| `or`/`alt!` | `Alt(Vec, Option<AltDispatch>)` :48 | `SaveState`/`JumpIfFail`/`RestoreState`/`DropState` |
| `dispatch` (O(1) first-char) | `Alt` + `AltDispatch{table}` :128 | `Dispatch(u16)` + `dispatch_tables` |
| `many`/`sepBy`/`opt` | `Repeat{inner,lo,hi}` :54 | `RepeatBegin{lo,hi,body_end}`/`RepeatEnd` |
| `skip`(`<<`)/`next`(`>>`) | `Skip`/`Next` :65,68 | `SaveValueDepth`/`DiscardRight`/`DiscardLeft` |
| `minus`(EBNF `-`) | `Minus` :71 | `Minus` |
| `not`/`negate` | `Negate` :75 | `Negate` |
| rule reference / `lazy` | `Ref(RuleId)` :61 | `Call(RuleId)`/`Return` |
| `trim`/`@ws` | `OptionalWhitespace` :84 | `TrimWs`/`TrimWsPattern(StringId)` |
| token-led alt fusion | `TokenDispatch{token,arms,fallback}` :90 | `DispatchToken(u16)` |
| `map`/`->` projection | `Map{inner,fn_id}` :79 | **NONE — dropped at compile** (§4) |
| `memoize`/packrat | (rule attr) | `MemoCheck`/`MemoStore` :138 |
| `@debug`/diagnostics | (rule attr) | `DebugBreak`/furthest-offset (§3) |

The Op set (~33 instructions) is a direct descendant of the LPeg parsing machine
(§5). The combinator↔IrNode leg is the LOCK-4 hand-forward's "14-variant
near-bijection"; the IrNode↔Op leg is this VM's compiler. 4C's thesis is that the
**Op set is the shared terminal ISA** for both repos — Rust interprets it today; a TS
interpreter over the *same serialized `BytecodeProgram`* is the missing half.

---

## 1. The mechanism made concrete

**Rust side (exists).** `bbnf_ir::compiler::compile(&GrammarIR) -> BytecodeProgram`
(`compiler/mod.rs:32`); `Interpreter::new(&program, &input).run()`
(`interpreter/mod.rs:113,152`). The run loop (`mod.rs:152+`) is a `match
self.program.code[pc]` direct-dispatch over `Op`, borrowing the op by ref and copying
small payloads (comment at the match: "avoids cloning Arc/Vec for Dispatch ops").
This is a classic switch-threaded bytecode interpreter (Ertl & Gregg §5).

**The parse-that lowering (the family's build).** parse-that's public API constructs a
*runtime* combinator graph (`Parser<'a,O>{parser_fn: SmallBox<dyn ParserFn, S32>}`,
`rust/parse_that/src/parse.rs:61`; TS `Parser<T>{parser: ParserFunction}`,
`typescript/src/parse/parser.ts:24`). 4C interposes a reification step: the combinator
graph is built as an `IrNode` (not an opaque closure), then `compile()`'d to a
`BytecodeProgram`, then interpreted. Two admissible timings:
- **Runtime compile** (the distinctive 4C claim): `compile()` is a pure function of a
  runtime-built `IrNode`; nothing binds it to comptime. First `.parse()` triggers
  compile-once (amortized exactly as packrat arms on first `memoize()`,
  `packrat.ts:290`), then all parses interpret. Preserves parse-that's runtime API.
- **Build-time compile** (the 4B overlap): grammar-text→IrNode→BytecodeProgram at
  build, ship the program. This is what the bbnf benches do (`json/vm.rs:19-21`:
  `compile_grammar` → `compile_bytecode`). 4C's differentiator vs 4B is that the
  *terminal is a runnable shared VM*, not monomorphized native — so the same
  `BytecodeProgram` bytes drive both languages (§2).

**TS side (the missing half).** A hand-written `switch(op.tag)` interpreter over a
`BytecodeProgram` deserialized from Rust JSON/MessagePack. Because `BytecodeProgram`
derives `Serialize/Deserialize` (`bytecode.rs:145`) and `Op` is a plain tagged union
(`bytecode.rs:45`), the program crosses as data. The TS interpreter mirrors
`interpreter/{leaves,control,repeat,construct,memo}.rs` arm-for-arm. Layout
isomorphism is *by construction*: there is one instruction set, defined once.

---

## 2. Q5 / Q6 — the serializable plan and isomorphism-of-record

**Q5 (serializable plan → one plan, two lowerings, byte-identical parses): YES, and
the observable already has a physical carrier.** The `BytecodeProgram`
(`bytecode.rs:145-200`) IS the serializable plan: `code: Vec<Op>`, `entries`,
`strings`, `dispatch_tables: Vec<DispatchData>`, `token_dispatch_tables`,
`follow_sets`, `rule_names`, `source_map` — all `#[derive(Serialize)]` /
`#[serde(default)]`. It is not intrinsic to the Rust type system (contrast 4B, whose
fused region *is* a monomorphized Rust type with no serializable residue). The
distinguishing observable the charter asks for: **serialize one `BytecodeProgram`,
run the Rust interpreter and a TS interpreter over it, assert identical
(offset, Value-tree)** — one plan, two lowerings, byte-identical parse.

**The one serialization seam (a named risk, not a blocker):** `compiled_regexes:
Vec<Option<parse_that::regex::Dfa>>` is `#[serde(skip)]` (`bytecode.rs:180`). The
DFA products do not cross; the TS side must recompile regexes from the interned
`strings` table (or run native `RegExp`, as the TS closure core does today —
`leaf.ts:317`). This is the exact XL-C two-tier bridge: opaque `Regex(StringId)`
crosses as a string; the rich engine tier is re-derived per side. It does not break
opcode-stream identity (the `MatchRegex(sid)` op is identical), only the cached
automaton is side-local.

**Q6 (isomorphism-of-record dual oracle): the VM makes both halves cheap and *live*.**
- *Opcode-stream identity* — the **live Rust golden** is `compile()`'s
  `program.code` (a `Vec<Op>`), serialized. Not a hand-faked blob: it is emitted by
  the same compiler the Rust interpreter runs. TS decodes and asserts equality.
- *Value-tree identity* — the VM's `Value` enum (`value.rs:52`) is small and total:
  `Span(u32,u32)`, `Tagged{tag,span,children}`, `Array(ValueSlice)`, `Nil`. The
  oracle is `Value`-tree equality between the Rust `Interpreter` and the TS
  interpreter over the same program+input. `ParseResult{success,offset,value,
  diagnostics}` is the record.
- *Corpus set*: the three already-benched grammars — `grammar/json/json.bbnf`,
  the CSS L4 grammar, `google_sheets` — plus parse-that's own JSON/CSV/CSS domain
  parsers as the combinator-side corpus.

**Q6 tail (who owns the Hex/Number/Enum internal-descriptor evaluator):** the VM
cleaves the plane exactly. The **structural** value plane (`Span/Tagged/Array`
construction via `CaptureSpan`/`MakeTagged`/`MakeArray`, `bytecode.rs`;
`construct.rs`) is **LOCK-3 IR→VM lowering** — it lives inside the interpreter. The
**descriptor** plane (Hex/Number/Enum decode from a captured `Span`) is *not in the
VM today* (§4) and is therefore **LOCK-7 host dispatch** unless promoted to a VM op.
4C reports this seam; it does not resolve it — but note that promoting it (an
`Op::Apply(FnId)`) is what would let the VM be the *terminal* rather than a recognizer
feeding a separate evaluator.

---

## 3. Packrat & diagnostics as VM facilities (charter clause — confirmed live)

- **Packrat** is `Op::MemoCheck{rule_id,hit_offset}` / `Op::MemoStore(rule_id)`
  (`bytecode.rs`), interpreted in `interpreter/memo.rs`, keyed
  `memo: FxHashMap<(RuleId,u32),(u32,Value,bool)>` (`mod.rs:84`). It is emitted only
  when `grammar_needs_memo(ir)` (`compiler/mod.rs:45`, direct-left-recursion
  detection) — i.e. **off by default, armed by analysis**, exactly matching
  parse-that's opt-in packrat law (`packrat.ts:156/290`; PACKRAT_ARMED a no-op until
  first `memoize()`). Isomorphic semantics for free.
- **Diagnostics** are VM state, not a global latch: `furthest_offset`/`furthest_rule`
  (`mod.rs`), `diagnostics: Vec<ParseDiagnostic>` with FOLLOW-set `expected` bytes
  (`value.rs:68`, `follow_sets` in the program). This is structurally the *cure*
  PT-E1 asks of the TS closure core (make diagnostics per-parse, not a module-global
  `diagnosticsEnabled` — `utils.ts:6`): in the VM, diagnostics are per-`Interpreter`
  by construction, since the interpreter IS the per-parse object.
- **Debug** is `Op::DebugBreak{rule_id,is_entry}` + `SourceMapEntry{pc,rule_id,span}`
  (`bytecode.rs:203`) — a no-op when `debug_state: None`. Source maps and breakpoints
  are VM facilities, isomorphic across backends because they are data in the program.

---

## 4. Q2 / Q3 — the value-plane barrier (a real, counted finding)

**The VM discards host projections.** `compiler/node.rs:31`:
`IrNode::Map{inner,..} => self.compile_node(inner, ir)` — the `fn_id` is **thrown
away**; there is no `Op` for host application. The VM therefore produces a purely
*structural* `Value` tree (spans + tags + arrays); every `->` projection, and every
arbitrary host closure, is applied by a **separate downstream pass over that tree**,
outside the VM.

Consequences for the directive questions:
- **Q3 (opaque-barrier size):** today the barrier is *100% of Map sites* to the VM —
  it sees through none of them (it deletes them). This is worse than the ≤2% floor
  *as an in-VM property*, but it is not an analysis-blindness in the classical sense:
  the VM simply doesn't carry the plane. The honest number a prototype must produce is
  the Map-site fraction per corpus (value.js, latex-paper, keyframes) — the count of
  `->` sites that would need an in-VM `Op::Apply` for the VM to be the terminal.
- **Q2 (arbitrary host closures):** the VM's `Value` set (Span/Tagged/Array/Nil) can
  express any *structural* projection but no *computed* one (Hex→u32, Number→f64,
  Enum→variant). To make the VM the value-plane home, add `Op::Apply(FnId)` calling a
  **host-fn table indexed by FnId, present identically on both sides** (the LOCK-7
  host-fn lattice). MapExpr-expressible projections could instead lower to a tiny
  arithmetic sub-ISA (`Op::DecodeHex`, `Op::DecodeF64`, `Op::MakeEnum(u16)`) that
  stays byte-identical across repos; only genuinely opaque closures need the FnId
  escape hatch (and only those constitute a true isomorphism barrier — the TS side
  must ship the same closure). A serializable `HostClosure` descriptor does not exist
  today (`FnDescriptor` has no such variant); the prototype must census which `->`
  sites are non-MapExpr and thus force the FnId barrier vs. the sub-ISA.

This is the central architectural fork for 4C: **VM-as-recognizer** (value plane
stays a post-pass, LOCK-7 owns it) vs **VM-as-terminal** (promote projections into the
ISA, LOCK-3 owns the MapExpr subset, LOCK-7 owns only the opaque residue). The family
should carry both and let the barrier census decide.

---

## 5. Q7 — setup vs value-plane vs recognizer cost (the VM's own tax)

`Interpreter::new` (`mod.rs:113-140`) allocates **eight Vecs + one FxHashMap** per
parse: `values(cap 64)`, `call_stack(32)`, `checkpoints(32)`, `repeats(16)`,
`memo(FxHashMap)`, `memo_starts(8)`, `value_depth_stack(8)`, `rule_stack(16)`,
`diagnostics(Vec::new)`. This is a fixed per-parse setup tax that the bench's
`bench_values(|input| { Interpreter::new(...).run() })` (`json/vm.rs:44`) pays on
every sample — it is *in* the cold per-parse number. The clean split the charter
wants:
- **recognizer cost** = the run-loop instruction dispatch + `MatchString`/`Dispatch`
  work;
- **value-plane cost** = `values`/`value_depth_stack` pushes + `MakeArray`/
  `MakeTagged` (`construct.rs`) — larger than the closure core's direct-return model,
  which never stages values on a heap Vec;
- **setup cost** = the nine allocations above, amortizable to ~zero by **pooling the
  `Interpreter`** (reset the Vecs' `len` to 0 and `memo.clear()` in place, reuse
  across parses) or bump-arena'ing the value stack. This is the single cleanest VM win
  and a required prototype measurement (setup-alloc share + the amortization curve as
  the interpreter is pooled). Pooling is trivially sound here (the interpreter borrows
  `&'prog program`, `&'prog input` — a reset-and-reuse needs only re-`new`ing the two
  borrows), unlike the parse-that closure-arena trap (`'a`-parameterized `ParserFn`
  cannot live in a `'static` pool).

---

## 6. Prior art (citations)

- **LPeg parsing machine** — R. Ierusalimschy, *"A Text Pattern-Matching Tool based
  on Parsing Expression Grammars"*, Software: Practice & Experience 39(3), 2009. THE
  canonical combinator/PEG bytecode VM. Its ISA (Char, Any, Set, TestChar, Choice,
  Jump, Call, Return, **Commit/PartialCommit/BackCommit**, Fail/FailTwice) maps
  one-to-one onto the bbnf Op set: Choice≈`SaveState`, Commit≈`DropState`+`Jump`,
  Fail≈`RestoreState`, Call/Return identical, TestChar≈`Dispatch`. The bbnf VM is a
  value-producing superset (LPeg captures ≈ `CaptureSpan`/`MakeTagged`). This is the
  direct precedent that a combinator core lowers to a compact VM *at competitive
  speed* — LPeg beats hand-written Lua parsers.
- **Rosie Pattern Language** (J. Jennings et al.) — LPeg-VM at production scale over
  log data; evidence the VM model holds under real corpora, not just microbenches.
- **Ertl & Gregg**, *"The Structure and Performance of Efficient Interpreters"*,
  JILP 2003 — the switch-vs-threaded-dispatch cost model. Governs the Rust run-loop
  (match-threaded today; direct/indirect-threaded via computed-goto is a Rust-side
  option) and warns the TS side: V8 cannot do computed-goto, and a >~30-arm `switch`
  in a hot loop risks a jump-table→linear deopt — the named TS risk (§7).
- **pest** (Rust PEG, `pest.rs`) — the CONTRAST: pest compiles to Rust *code* via a
  derive macro (no runtime VM), closer to family 4B. Cite as the "why not just
  codegen" foil: codegen wins single-language speed but forfeits the one-plan-two-
  lowerings isomorphism 4C buys.
- **tree-sitter** — table-driven GLR automaton serialized as data and interpreted by a
  shared C core with per-language bindings. Precedent that "the parser is data + one
  interpreter, many language faces" is a shipping architecture; its `parser.c` tables
  are the moral equivalent of `BytecodeProgram`.
- **CPython `sre` / Rust `regex` NFA-bytecode** — regex-as-bytecode-VM, the
  sub-language precedent (ties XL-C: the VM already holds `compiled_regexes` DFAs).
- **egg / egglog** (Willsey et al., POPL 2021) — the IrNode carries a
  path-dep-consumed e-graph; bytecode is a *superoptimization* target (peephole over
  `Op` sequences: fuse `SaveState`…`DropState` around a non-backtracking branch, elide
  `Dispatch` fallthrough). MLIR-style multi-level lowering (combinator dialect → IR
  dialect → bytecode dialect) is the framing for the compile pipeline.

---

## 7. Named risks & counterexamples

1. **The refutation risk (central).** A bytecode VM adds one instruction-dispatch
   indirection *per op* that a monomorphized/fused closure chain (4B) avoids. If the
   VM's per-op dispatch exceeds the closure core's per-combinator vtable/IC dispatch,
   4C loses on speed. Counterexample to *assuming* the win: PROVE-11's dispatch lane
   claimed +10.2%, re-measured **~+5%** — over-claiming is the house disease. 4C must
   be measured paired cold per-parse against the closure core, never asserted. The
   fair comparison is *VM-switch-dispatch vs vtable/IC-dispatch* (both indirect), NOT
   VM vs inlined native — parse-that's closure core also pays a megamorphic
   `this.parser(state)` IC (`parser.ts` §6c of R4) and a `SmallBox<dyn ParserFn>`
   virtual call (`parse.rs:61`) per combinator, so the VM may consolidate N
   megamorphic sites into ONE hot switch.
2. **TS switch deopt.** A ~33-arm `switch` on `op.tag` in V8's hot loop can fall off
   the jump-table fast path. Mitigation: split cold arms (Debug/Memo/Trim) behind a
   rare-guard, keep the hot arms (Match/Seq/Alt/Dispatch/Repeat) in a dense
   small-integer switch; measure IC state. This is the TS-specific make-or-break.
3. **Value-stack allocation.** The VM stages values on a heap `Vec<Value>`
   (`MakeArray`/`MakeTagged`) where the closure core returns them directly. Without
   pooling (Q7) the VM can be *more* allocation-heavy per parse (the 5390 per-parse
   allocs the closure core already carries are Vec growth in `many`/`sep_by` — the VM
   would add its own value stack on top). The pooled-interpreter measurement is
   load-bearing.
4. **Value-plane amputation (§4).** If projections stay a post-pass, 4C is a
   recognizer, not the terminal — and the "one plan, two lowerings" claim only covers
   recognition, not the value plane. The `Op::Apply`/sub-ISA promotion is what closes
   this, at the cost of dragging the LOCK-7 host-fn table into the isomorphism
   contract.
5. **Regex non-crossing (§2).** `compiled_regexes` is `#[serde(skip)]`; a TS side that
   forgets to re-derive will silently `MatchRegex`-fail. The oracle must exercise a
   regex-heavy grammar (CSS) to catch it.
6. **Left-recursion / memo divergence.** `grammar_needs_memo` heuristic
   (`compiler/mod.rs`) must fire identically on both sides or the packrat facility
   desynchronizes the opcode stream — a Q6 isomorphism trap. Emit memo decisions into
   the serialized program (they already are: `memo_enabled` field, `bytecode.rs:167`),
   never recompute TS-side.

---

## 8. What a prototype must test (the family-mover)

Two paired measurements settle 4C; both use the existing scratch-copy recipe (R3 §8)
and the cold per-parse paired-interleaving law (no warm benches; PROVE-11 discipline).

1. **Rust: VM vs closure core, same JSON grammar, paired.** The bbnf VM bench
   (`crates/core/benches/json/vm.rs`) already gives the VM arm; add the parse-that
   closure-core arm (`rust/parse_that/benches/parse_that/combinator.rs`) over the
   *same* `data/json/*.json`. Report cyc/B for {VM cold-with-`new`, VM pooled,
   closure core} per corpus, two windows, N≥200. Decompose the VM number into
   recognizer / value-plane / setup (Q7). **Pass = VM (pooled) within noise of, or
   ahead of, the closure core on ≥3 of 5 corpora**; a loss is a *named negative
   result*, not a hidden one.
2. **TS: cross-repo isomorphism + speed.** Serialize the JSON `BytecodeProgram` from
   Rust; hand-write the ~33-arm TS switch interpreter; assert (a) opcode-stream
   identity vs the live Rust golden, (b) `Value`-tree + offset identity over
   `data/json`, (c) paired cold-per-op ns vs the TS closure core (`parser.ts`), IC
   state probed (`--allow-natives-syntax` `%HaveSameMap` on the hot state object).
   **Pass = byte-identical parses + the TS switch not deopting** (arms 2–3 are the
   real question; speed parity is the stretch).
3. **Barrier census (Q2/Q3).** Count `->`/`Map` sites across value.js, latex-paper,
   keyframes; classify MapExpr-expressible (→ arithmetic sub-ISA) vs opaque (→ FnId
   host-table barrier). Output the per-corpus barrier fraction that decides
   VM-as-recognizer vs VM-as-terminal.

**Q1 answer, stated for the registry:** the runtime combinator API reaches the VM
**only through a `compile()` step over a reified `IrNode` graph** — there is no
type-level monomorphization path from a runtime closure graph (that is 4B's mechanism,
and it needs a comptime-known graph or codegen-reflection). 4C's `compile()` is a
runtime function, so parse-that *can* keep its runtime API and still reach the VM
(compile-once-on-first-parse); what a monomorphizing lowering would demand of the API —
comptime-known arities and const grammar — is exactly what 4C does **not** require,
which is the family's distinguishing virtue and its speed liability (interpretation,
not native codegen). The prototype's Rust arm quantifies that liability against the
closure core.
