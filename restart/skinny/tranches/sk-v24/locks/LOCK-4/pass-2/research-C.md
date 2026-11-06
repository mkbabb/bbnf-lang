# LOCK-4 · Family 4C — THE PORTABLE-PROGRAM ISOMORPHISM LAYER (VM-CORE)

Combinators lower to a compact bytecode program that crosses **as data** — serialized in
Rust, decoded by a small TS switch-loop — so the rust/ts isomorphism is by construction:
one instruction set, one operand encoding, one linear program. The runtime that survives is
neither an opaque closure tree nor a monomorphized struct, but a portable opcode/operand
stream over a shared interner. This family's decisive question is not *whether* a VM is
possible — one is already live in-tree — but *what a portable, value-producing VM must add
to the existing contract*, and *whether it survives crossing byte-identical over a shared
corpus*.

---

## 1. The substrate already exists — enumerate before you invent

The bbnf-lang side already ships a **live, value-producing bytecode VM**, and it is the
abrogate-before-patch anchor for this whole family. Do not mint a private ISA against it
without proving why.

- **The ISA.** `bbnf_ir::bytecode::Op` (`crates/ir/src/vm/bytecode.rs:45-152`) — a **30-opcode**
  enum: leaves (`MatchString(StringId)`, `MatchRegex(StringId)`, `Epsilon`), control
  (`Jump`/`JumpIfFail`/`JumpIfOk`/`Call(RuleId)`/`Return`), backtrack state
  (`SaveState`/`RestoreState`/`DropState`), whitespace (`TrimWs`/`TrimWsPattern(StringId)`),
  EBNF combinators (`Minus`/`Negate`), O(1) byte dispatch (`Dispatch(u16)` +
  `DispatchToken(u16)` over side tables `DispatchData`/`TokenDispatchData`, `:12-41`),
  repetition (`RepeatBegin{lo,hi,body_end}`/`RepeatEnd`), value construction (`CaptureSpan`,
  `SaveValueDepth`, `DiscardRight`/`DiscardLeft` for `<<`/`>>`, `MakeArray(u32)`,
  `MakeTagged(StringId)`), packrat (`MemoCheck{rule_id,hit_offset}`/`MemoStore(RuleId)`),
  diagnostics/debug (`DebugBreak{rule_id,is_entry}`), `Nop`, `Halt`.
- **The container is already serializable.** `BytecodeProgram` (`bytecode.rs:154-199`) is
  `#[derive(Serialize, Deserialize)]`: `code: Vec<Op>`, `entries: Vec<u32>` (per-rule entry
  offsets), `strings: Vec<String>` (the interner — the operand namespace), `entry: RuleId`,
  `dispatch_tables`, `token_dispatch_tables`, `follow_sets`, `rule_names`, `source_map`.
  The only non-serialized field is `compiled_regexes: Vec<Option<parse_that::regex::Dfa>>`
  (`bytecode.rs:183-184`, `#[serde(skip)]`) — re-derived on load by `prepare_regexes()`
  (`bytecode.rs:220-242`) from the serialized pattern strings. **The regex crossing is
  therefore already data: `MatchRegex(sid) → strings[sid] → new RegExp` on the far side**,
  exactly the native-`RegExp` path R4 §3 documents (`leaf.ts:317-369`).
- **The MessagePack bus is live.** `crates/ir/src/lib.rs:7` ("serializable via MessagePack
  for WASM boundary transfer"); `GrammarIR::to_msgpack`/`from_msgpack` round-trip
  (`crates/ir/src/types/grammar.rs`, `rmp_serde::to_vec`/`from_slice`); the `TypeDescInterner`
  round-trips through the same bus (`crates/ir/tests/egraph/type_desc_interner.rs`).
- **The interpreter.** `Interpreter::run()` (`crates/ir/src/vm/interpreter/mod.rs:144-313`) is
  a **single `match self.program.code[pc]` tight loop** with explicit stacks (`values`,
  `call_stack`, `checkpoints`, `repeats`, `memo`) — a textbook switch-threaded VM. Public
  entry `parse_with_ir(ir, input)` at `mod.rs:373-377` compiles then runs. The compiler is
  `bbnf_ir::vm::compiler::compile(&GrammarIR) → BytecodeProgram`
  (`crates/ir/src/vm/compiler/mod.rs:31`), a per-`IrNode` fan-out (`compiler/node.rs:9-54`).
- **It has live consumers already** (Y.13 satisfied for the substrate): the LSP DAP debug
  adapter holds a `program: BytecodeProgram` (`crates/lsp/src/dap/adapter.rs`), and a JSON
  benchmark `compiled_vm() → (GrammarIR, BytecodeProgram)` exists at
  `crates/core/benches/json/vm.rs` — **the closure-vs-VM measurement seam this family needs
  already has a home.**

The combinator node set is a verified near-bijection with `bbnf_ir::IrNode` (14 variants,
`crates/ir/src/types/node.rs:33-96`: Literal, Regex, Epsilon, Seq, Alt, Repeat, Ref, Skip,
Next, Minus, Negate, Map, OptionalWhitespace, TokenDispatch). The VM's 30 opcodes are the
**lowering** of those 14 nodes; the near-bijection is at the tree level, the portable crossing
is at the linear-program level. This family crosses the **program**, not the tree — which is
the distinction that makes it isomorphic-by-construction (a linear `Vec<Op>` + a string table
has no recursive ownership to mirror; a TS decoder is a `for`-loop over a flat array).

---

## 2. THE LOAD-BEARING GAP — the in-tree VM is value-STRUCTURAL, not value-PRODUCING

This is the finding that decides the family. **The VM silently discards every `->`
projection.** At `crates/ir/src/vm/compiler/node.rs:31`:

```
IrNode::Map { inner, .. } => self.compile_node(inner, ir),
```

The `fn_id` is dropped; only `inner` is compiled. There is **no `Op` for value projection** —
the value plane is limited to `CaptureSpan`/`MakeArray`/`MakeTagged`, and the `Value` enum
(`interpreter/value.rs:56-71`) has exactly four shapes: `Span(u32,u32)`, `Tagged{tag,span,
children}`, `Array`, `Nil`. The VM produces a **structural parse tree keyed by offsets and
rule tags** — it does not run `NumberConvert`, `HexConvert`, `EnumWrap`, or any user
`Expr(MapExpr)`. This directly violates the standing no-value-discard law and is the exact
"structural spine inlines, value plane confined to leaves" fact the LOCK-4 seed notes.

The consequence for 4C: **a portable VM that reuses this substrate is not a patch — it is a
value-plane completion.** The substrate is sound as a recognizer; making it a *combinator
core* (which must carry `.map`) requires adding the value-projection instruction the compiler
today throws away. The good news is that the projection representation is **already portable
data**: `FnDescriptor` (`crates/ir/src/types/fn_descriptor.rs`) is `#[derive(Serialize,
Deserialize)]`, and its user-facing `Expr { expr: MapExpr, return_type }` variant carries a
fully-serializable `MapExpr` tree "transparent to IR passes" (its own doc). So the extension
is mechanical and stays on the existing contract:

1. Add one opcode `Op::ApplyFn(FnId)` (and lower `IrNode::Map{inner, fn_id}` to
   `compile_node(inner)` followed by `ApplyFn(fn_id)` — the exact site is
   `compiler/node.rs:31`).
2. Add one serialized side table `fns: Vec<FnDescriptor>` to `BytecodeProgram`
   (mirroring `dispatch_tables`, `#[serde(default)]`), so the projection plane crosses as
   data alongside the opcodes.
3. Extend `Value` with the scalar leaves projections yield (a `F64`/`U32`/`Str`/`Variant`
   arm — the "no host-closure variant in FnDescriptor" gap the seed names is the same gap on
   the value side).
4. The interpreter's `ApplyFn` handler is a `match FnDescriptor` — a **small MapExpr
   evaluator** — that both Rust and TS implement over the *same* serialized descriptor. This
   is where LOCK-7's host-fn lattice and LOCK-5's regex-class specializations (`NumberConvert`,
   `HexConvert` are regex-fused) land as **VM facilities**, exactly the charter's frame.

This is the abrogate-before-patch adjudication in full: **reuse the ISA, the container, the
interner, the MessagePack bus, the interpreter loop; extend the value plane with one opcode
and one side table.** A from-scratch private ISA is unjustified — it would re-derive
30 opcodes, a serde container, a string interner, and a serialization bus that all exist and
have consumers. The one honest counter-argument (§8 R3) is the dependency-cycle risk, which
is a *homing* question, not an ISA-invention question.

---

## 3. The mechanism made concrete — the cross-language crossing

The 4C directive's first question: *can a language-neutral opcode/operand stream cross as
data and yield a byte-identical opcode stream AND identical parse over a shared corpus?* The
mechanism:

**Rust side (already present):** `let program = compile(&ir); let bytes =
rmp_serde::to_vec_named(&program)?;` — a `BytecodeProgram` with the §2 extension. `to_vec_named`
(field-named MessagePack) makes the wire format self-describing and language-neutral; the
`Op` enum serializes as externally-tagged variants (`{"MatchString": 3}`), which is a
**stable, decoder-friendly tag stream**.

**TS side (the ~200 LOC deliverable):** a `decode(bytes) → Program` using `@msgpack/msgpack`
`decode()` (or a hand-rolled reader over the well-known msgpack subset), then a `run(program,
input)` that is the **line-by-line mirror of `interpreter/mod.rs:144-313`** — a `while (pc <
code.length) switch (op.tag)` loop with the same five explicit stacks (`values`,
`callStack`, `checkpoints`, `repeats`, `memo`), the same offset/`isError` registers,
`MatchRegex(sid) → new RegExp(strings[sid], "y")` (R4's sticky-`test()` zero-alloc path,
`leaf.ts:344-358`), and the same `Value` shapes. The V8 discipline for the loop is a
**monomorphic op object** — decode into a `{tag: number, a?: number, b?: number}` struct with
a fixed hidden class (LOCK-8/8A shape-stability), so the central `switch` is a
jump-table-friendly integer dispatch, not a megamorphic property read.

**The dual oracle (both must hold):**
- **Stream identity.** Decode the bytes in TS, re-encode (or structurally compare the decoded
  `Op[]` against a Rust-emitted golden JSON of the same program): the opcode/operand stream
  is byte-for-byte the same object graph. This proves the *encoding* is language-neutral.
- **Parse identity.** Run the Rust `Interpreter` and the TS `run()` over the **same corpus
  with the same leaf substrate** (the six R3 JSON corpora + value.js's CSS shapes), and
  assert the produced `Value` trees are node-for-node equal — the differential-oracle
  discipline of PROVE-14 (SPEC §0/14: "byte-exact node-for-node across 320 cold children"),
  reused here for the *parser* crossing rather than the *tape* crossing.

**Critical distinction from PROVE-14 / GN-4.W4 / LOCK-6.** SPEC §0/14 crosses the
**offset-tape** (the *output* of a parse) to TS; 4C crosses the **bytecode program** (the
*parser itself*) to TS. These are orthogonal and complementary: 14 ships parsed data behind a
monomorphic tape decoder whose oracle is `JSON.parse`; 4C ships a grammar-general parser whose
oracle is a second interpreter. A grammar authored once in bbnf, compiled once to
`BytecodeProgram`, then *executed on both sides from the same bytes* — that is the strongest
statement of "rust/ts isomorphism by construction" the constitution's clause (iii) can carry,
because there is nothing to keep in sync: the parser is data.

---

## 4. Packrat & diagnostics as VM facilities (the charter clause)

Both are already opcodes, not bolt-ons — the charter's "packrat and diagnostics are VM
facilities" is *already true in the substrate*, and this is a point in the family's favour:

- **Packrat.** `MemoCheck{rule_id, hit_offset}`/`MemoStore(RuleId)` (`bytecode.rs:133-140`)
  with the interpreter's `memo: FxHashMap<(RuleId,u32),(u32,Value,bool)>` (`interpreter/mod.rs:84`).
  It is emitted only when `grammar_needs_memo` detects direct left recursion
  (`compiler/mod.rs:99-116`), so the LL(1) path pays nothing — mirroring the TS `PACKRAT_ARMED`
  latch (R4 §6g, `packrat.ts:156/290`, "true no-op until first `memoize()`"). The TS decoder
  inherits this: memo opcodes are simply present-or-absent in the crossed program; the far
  side needs one `Map` keyed on `(ruleId*OFFSET + offset)` — R4's exact `getCijKey` float-safe
  arithmetic (`packrat.ts:71-100`).
- **Diagnostics.** `DebugBreak{rule_id,is_entry}` opcodes + a `source_map: Vec<SourceMapEntry>`
  (`bytecode.rs:201-210`, PC→grammar-span) + `follow_sets` for furthest-failure expected-token
  reporting (`interpreter/mod.rs:335-356`). This is a whole DWARF-like plane already compiled
  into the program and crossing as data — the TS side gets source-mapped diagnostics **for
  free from the same bytes**, which is precisely PT-3.W2's per-parse-scoped-diagnostics ask
  (R4 §5 PT-E1) satisfied structurally rather than by a module-global latch.

---

## 5. Integration seams (path:line, where extant code serves)

| Seam | Path | Role for 4C |
|---|---|---|
| ISA | `crates/ir/src/vm/bytecode.rs:45-152` | the 30-opcode `Op` enum to reuse verbatim |
| Container | `crates/ir/src/vm/bytecode.rs:154-199` | serde `BytecodeProgram`; add `fns` side table here |
| Interpreter loop | `crates/ir/src/vm/interpreter/mod.rs:144-313` | the switch loop the TS decoder mirrors |
| Compiler fan-out | `crates/ir/src/vm/compiler/node.rs:9-54` | add `ApplyFn` lowering at `:31` (the Map-drop site) |
| Compile entry | `crates/ir/src/vm/compiler/mod.rs:31` | `compile(&GrammarIR) → BytecodeProgram` |
| Value plane | `crates/ir/src/vm/interpreter/value.rs:56-71` | extend `Value` with scalar-projection arms |
| Projection data | `crates/ir/src/types/fn_descriptor.rs` | `FnDescriptor`/`Expr(MapExpr)` — already serde, crosses as-is |
| MessagePack bus | `crates/ir/src/lib.rs:7`; `types/grammar.rs` `to/from_msgpack` | the wire format; reuse for `BytecodeProgram` |
| Closure baseline | `crates/core/benches/json/vm.rs` `compiled_vm()` | the extant VM-vs-closure measurement home |
| Regex crossing | `bytecode.rs:220-242` `prepare_regexes` + R4 `leaf.ts:317-369` | `MatchRegex → strings[sid] → RegExp`, both sides |
| Diagnostics plane | `bytecode.rs:201-210` source_map; `interpreter/mod.rs:335-356` | DWARF-like plane crosses as data |
| parse-that closure core (Rust) | `parse-that/rust/parse_that/src/parse.rs:61` `Parser{parser_fn:SmallBox<dyn ParserFn>}` | the arm the VM is measured *against* |
| parse-that closure core (TS) | `parse-that/typescript/src/parse/parser.ts:24-32`; leaf.ts | the TS arm; the megamorphic `this.parser(state)` IC (R4 §6c) the VM replaces with one switch |

Scratch-copy law (R3 §8 / R4 §8): parse-that is READ-ONLY; all prototyping is `rsync` scratch
copies with `CARGO_TARGET_DIR` isolated, external patch paths repointed absolute.

---

## 6. Prior art (with citations)

- **CPython 3.11 specializing adaptive interpreter** (PEP 659) and the **register-VM turn** of
  Lua 5 — the canonical evidence that a compact bytecode + tight switch-threaded loop beats a
  tree-walk/closure interpreter *when dispatch overhead is amortized over enough work per op*.
  The relevance and the risk are the same: the VM wins only if per-op work exceeds dispatch
  cost — measured, not assumed (charter mandate + §7 R1).
- **Parsing-machine lineage.** Medeiros & Ierusalimschy, *A Parsing Machine for PEGs* (2008),
  and **LPeg**'s bytecode ISA (`IChar`, `ITestChar`, `IChoice`, `ICommit`, `ICall`, `IRet`,
  `IFail`) — a near-exact analog of this family's `MatchString`/`Dispatch`/`SaveState`/`Call`/
  `Return`/`JumpIfFail` set, proving the combinator→bytecode lowering is a settled technique
  with a published, portable ISA. LPeg's `re` module shows the same "grammar compiles to
  program, program is the runtime" shape.
- **Portable-program crossing.** WebAssembly and Java bytecode are the existence proofs that a
  language-neutral opcode/operand stream is decoded identically across implementations;
  **regex-automata**'s serialized DFA (`DFA::from_bytes`) is the closer analog — a compiled
  matcher shipped as a `&[u8]` and executed without recompilation, exactly the "parser as
  data" claim, and the same crate parse-that's `bbnf-regex` re-implements (R3 §3).
- **tree-sitter** compiles grammars to a table-driven parser + a serialized language artifact
  loaded by a generic runtime — the same "author once, execute from a portable artifact"
  architecture, cross-language by construction.
- **Negative prior art (binds this family):** SPEC §0/09's refuted shim and PROVE-11's halved
  dispatch claim (VERIFY: +10.2% → ~+5%) — the standing lesson that "obvious" wins evaporate
  under paired interleaving. A VM's *plausibility* is not its *speedup*; the family owes a
  paired-cold number against the closure core on the same substrate, or it names its negative
  result (§7).

---

## 7. Named risks & counterexamples

- **R1 — dispatch tax swamps the win (the primary refutation risk).** A switch-loop VM pays a
  per-op indirect branch that inlined closures and monomorphized structs (4B) do not. On an
  LL(1), dispatch-dominated grammar (JSON), the closure/`dispatch`-table core may *win*, and
  the VM's honest result is a **negative** one. *Counterexample to hunt:* JSON `combinator`
  bench where the VM's cyc/B exceeds the closure core's despite identical leaves. The
  cross-cutting keystone demands this be *measured* on the common baseline
  (`crates/core/benches/json/vm.rs` already stands one up), not inferred. Name the negative
  result if the LL(1) shape favours closures (the directive explicitly invites this).
- **R2 — value materialization tax.** The §2 `ApplyFn`/`MapExpr` completion makes the VM
  value-producing but interposes a descriptor-dispatch per projection — precisely the
  materialization tax the 4D question warns of on projection-dense grammars (value.js's 59
  `all()`-of-mapped-parts). *Counterexample:* a projection-heavy CSS grammar where the VM's
  per-`Map` `ApplyFn` dispatch loses to inlined scalar leaves. The paired bench must be
  **projection-dense, not JSON-only.**
- **R3 — the dependency cycle (the homing risk, not the ISA risk).** `bbnf_ir` depends on
  `parse_that = "0.4"` (`crates/ir/Cargo.toml:13`, for `regex::Dfa`). A *parse-that-side*
  combinator VM that reuses `bbnf_ir::vm::Op` would invert into `parse_that → bbnf_ir`, a
  cycle. Resolution is a LOCK-3/LOCK-6 seam: hoist `Op`/`BytecodeProgram`/the interner into a
  **third dependency-free crate** both consume, or parse-that vendors the ISA under a sync
  gate. This is the one place a "private ISA" argument has teeth — but the answer is
  *rehoming the shared ISA*, not inventing a second one.
- **R4 — enum-tag wire fragility.** MessagePack externally-tagged `Op` variants are
  index-order-sensitive; a reordered `Op` enum silently breaks the crossing. *Guard:* the dual
  oracle's stream-identity clause catches it, and `to_vec_named` + explicit variant tags pin
  the contract. A regen-idempotence gate (encode∘decode∘encode = encode) is the born-RED test.
- **R5 — regex-engine divergence across the boundary.** Rust re-derives `parse_that::regex::Dfa`
  from the pattern; TS uses native `RegExp`. Semantics can differ (backtracking, Unicode
  classes). *Counterexample:* a pattern the bespoke DFA and V8's `RegExp` disagree on. *Guard:*
  restrict the crossed regex surface to the grammar-lowerable class LOCK-5 certifies, and add
  a per-pattern differential oracle.

---

## 8. What a prototype must test to move this family

1. **The ~200 LOC TS switch-loop decoder** over a `to_vec_named`-serialized `BytecodeProgram`
   for a JSON grammar, with the **dual oracle**: (a) decoded `Op[]` structurally identical to
   a Rust-emitted golden; (b) TS `Value` tree node-for-node equal to the Rust `Interpreter`
   over the six R3 corpora. *This is the family's terminal existence proof — no terminal can
   be named without a first honest cross-language number.*
2. **The value-plane completion** (`ApplyFn` + `fns` side table + `Value` scalar arms) landing
   `IrNode::Map` so the VM produces value-projected output byte-identical to the closure core's
   `.map` results — proving the substrate becomes a *combinator* core, not merely a recognizer.
   Gate: differential oracle against `parse_with_ir`-plus-projection vs parse-that's closure
   parse on a projection-dense grammar.
3. **The common-baseline runtime fork, measured.** The VM interpret path vs the parse-that
   closure core over the **same grammar and same leaf substrate**, paired-cold interleaved
   (A,B,A,B, N≥200, two windows, both arm orders, median + dispersion — the §0/09 discipline),
   on **both** a dispatch-dominated grammar (JSON) and a projection-dense one (CSS/value.js).
   Report the ratio and **name the negative result** if closures win on the LL(1) shape.
4. **The dependency-homing spike (R3):** prove `Op`/`BytecodeProgram`/interner extract into a
   dependency-free crate that both `bbnf_ir` and a parse-that scratch consume without a cycle —
   a `cargo tree` with no `parse_that → bbnf_ir → parse_that` edge.
5. **Wire-contract regen-idempotence:** encode∘decode∘encode = encode across the corpus, with
   `Op` variant tags pinned — the born-RED guard against R4.

The family's strength is that its substrate is not speculative: a serde-derived, MessagePack-
bussed, 30-opcode value-tree VM with a compiler, an interpreter, a DAP consumer, and a JSON
bench already ship in-tree. Its honesty burden is equally concrete: the VM is value-structural
today (the `Map`-drop at `node.rs:31`), and its speedup against the closure core is unproven
and refutable — the family advances on the first cross-language number and the first paired
common-baseline number, or it banks a named negative result.
