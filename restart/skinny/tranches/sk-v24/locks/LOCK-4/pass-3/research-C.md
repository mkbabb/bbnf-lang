# LOCK-4 · Family 4C — THE PORTABLE-PROGRAM ISOMORPHISM VM (pass-3 research-C)

*Measurement HEAD (XL-H): `c43126422423bf78e74e2b415ffd6ee16444f2c3`, a descendant of the
pass-3 base-pin `ce51c36fc`. Every enumeration below is `rg`/`Read`-verified on this tree; no
build ran. parse-that is READ-ONLY (R3 §8 / R4 scratch-copy law).*

4C's role is fixed by the pass architecture and by two prior measured refutations: the VM is
**not** a fast terminal — it is (a) the **interpret terminal** serving the DAP/LSP/dynamic-grammar
lane and (b) the **cross-language isomorphism-of-record oracle** — the one construction under
which rust/ts isomorphism is a property of the *representation*, not of a mirrored-source linter,
because the parser itself crosses **as data**. This pass answers the value-plane-home question
(Q6), the serializable-plan observable (Q5), and the setup/value/recognizer cost split (Q7), and
it corrects a load-bearing over-claim carried into pass-3.

---

## 0. THE CORRECTION THIS PASS OWES — the value plane is NOT built on the tree of record

Pass-3 FAMILY-SPECS states 4C's "value completion is built and byte-identical (`Op::ApplyFn(FnId)`
… `apply_fn.rs` … PROTO-2 oracle GREEN)." **On HEAD `c43126422` this is false.** The census:

- `crates/ir/src/vm/bytecode.rs:45` `enum Op` has **exactly 30 variants** (MatchString, MatchRegex,
  Epsilon, Jump, JumpIfFail, JumpIfOk, Call, Return, SaveState, RestoreState, DropState, TrimWs,
  TrimWsPattern, Minus, Negate, Dispatch, DispatchToken, RepeatBegin, RepeatEnd, CaptureSpan,
  SaveValueDepth, DiscardRight, DiscardLeft, MakeArray, MakeTagged, MemoCheck, MemoStore,
  DebugBreak, Nop, Halt). **No `ApplyFn`.** `rg ApplyFn crates/ir/src` → 0 hits.
- `crates/ir/src/vm/interpreter/` holds construct/control/leaves/memo/mod/repeat/value.rs —
  **no `apply_fn.rs`.**
- `crates/ir/src/vm/interpreter/value.rs:52` `enum Value` has **4 shapes only**: `Span(u32,u32)`,
  `Tagged{tag,span,children}`, `Array(ValueSlice)`, `Nil`. No scalar (`F64`/`U32`/`Str`/`Variant`) arm.
- `crates/ir/src/vm/compiler/node.rs:31`: `IrNode::Map { inner, .. } => self.compile_node(inner, ir)`
  — the `fn_id` is **still silently dropped**; every `->` projection is discarded at compile time.
- `crates/ir/src/vm` totals **1772 LOC** (pass-3 FAMILY-SPECS's "~2090" and "33-variant" figures
  are stale — likely counting a prototype patch as landed).

**What actually exists as evidence.** PROTO-2 (pass-3) built a *type-plane* census — it added
`FnDescriptor::HostClosure` to `crates/ir/src/types/fn_descriptor.rs` and proved the **emitter**
(4B/4D) fuses one arbitrary closure into a straight-line region with 1 named call-out; it did
**not** add `Op::ApplyFn` to the VM. The "value completion built" line conflates the emitter's
HostClosure census with a VM value plane that was only ever a pass-2 probe patch and was never
landed. **The honest disposition: the in-tree VM is a value-STRUCTURAL recognizer** (offsets +
rule tags), and making it a *combinator* core — one that carries `.map` — remains an unbuilt
value-plane **completion**, the exact no-value-discard violation the seed names. This is the
family's central unbuilt claim, and it is mechanical, not a missing primitive (§2).

The substrate that *is* real and load-bearing (all HEAD-verified):

| Fact | Path:line |
|---|---|
| 30-opcode LPeg-shaped ISA, `#[derive(Serialize,Deserialize)]` | `crates/ir/src/vm/bytecode.rs:44-152` |
| serde `BytecodeProgram`: `code`/`entries`/`strings`(interner)/`dispatch_tables`/`follow_sets`/`source_map`/`rule_names`, all `#[serde(default)]` | `bytecode.rs:155-199` |
| the ONLY non-serialized field: `#[serde(skip)] compiled_regexes: Vec<Option<parse_that::regex::Dfa>>`, re-derived by `prepare_regexes()` | `bytecode.rs:183-184`, `:220-242` |
| switch-threaded interpreter loop (`match self.program.code[pc]`), 5 explicit stacks + memo | `crates/ir/src/vm/interpreter/mod.rs:144-313` |
| `Interpreter::new` — 8 `Vec::with_capacity` + `Vec::new` + `FxHashMap::default()` | `mod.rs:114-130` |
| compiler fan-out `compile(&GrammarIR)→BytecodeProgram` | `crates/ir/src/vm/compiler/mod.rs`, `compiler/node.rs:9-54` |
| MessagePack bus (`to/from_msgpack`, `rmp_serde`) for the WASM boundary | `crates/ir/src/lib.rs:7`; `types/grammar.rs` |
| live consumers (Y.13 for the substrate): DAP adapter holds a `BytecodeProgram`; JSON VM bench | `crates/lsp/src/dap/adapter.rs`; `crates/core/benches/json/vm.rs` |

---

## 1. Q1 CONSEQUENCE FOR 4C — the VM is the third lowering, the interpret terminal (measured adverse)

The lock's staging boundary (RF-1, resolved NO) leaves the runtime combinator graph exactly two
lowerings: **interpret** (this VM) or **serialize-then-offline-codegen** (4B). 4C *is* the
interpret arm. Two prior measured facts, carried as law, fix its disposition:

- **PROTO-1 (pass-3): a TS runtime `switch(node.tag)` decoder is −22%** against the build-time
  emitter — the runtime-decoder-as-fast-TS-face is BANKED-by-measurement dead (this is the same
  switch-loop shape 4C's TS decoder is). The emitter is the fast TS face (×1.23).
- **PROTO-2 (pass-2): the interpret path floors at 0.021–0.46×** the closure core — the VM is
  measured-refuted twice as a *fast* terminal.

So 4C does not compete for throughput. Its value is **flexibility + verification**: the VM is the
only route that runs a grammar assembled *at runtime* (dynamic grammars, the LSP live-edit loop,
the DAP debugger stepping opcodes with `DebugBreak` + `source_map`), and it is the only route
whose output can serve as the **live golden** against which every other route (4A/4B/4D, Rust
and TS) is differentially checked. The correct reading of Q1 for this family: *the VM is not a
patch on the missing runtime→comptime bridge; it is the acknowledged runtime terminal that exists
because that bridge is a category error.*

---

## 2. THE VALUE-PLANE COMPLETION — the one unbuilt mechanism, made concrete (and why it is not a primitive of equal difficulty)

To become a combinator core the VM must carry `.map`. The completion is four mechanical edits on
data structures that already serialize:

1. **One opcode.** Add `Op::ApplyFn(FnId)` to `bytecode.rs:45`. Lower `IrNode::Map{inner, fn_id}`
   at `compiler/node.rs:31` to `compile_node(inner)` **then** `emit(ApplyFn(fn_id))` — stop
   dropping `fn_id`.
2. **One serialized side table.** Add `#[serde(default)] fns: Vec<FnDescriptor>` to
   `BytecodeProgram` (`bytecode.rs:155-199`), mirroring `dispatch_tables`. `FnDescriptor`
   (`crates/ir/src/types/fn_descriptor.rs:18`) is **already `#[derive(Serialize,Deserialize)]`**;
   its `Expr{expr: MapExpr, return_type}` variant (`:54`) carries a fully-serializable `MapExpr`
   tree "transparent to IR passes." So the projection plane crosses as data with zero new
   serialization work.
3. **Value scalar arms.** Extend `Value` (`value.rs:52`) with `F64`/`U32`/`Str`/`Variant` — the
   leaves a projection yields.
4. **One interpreter handler + its TS mirror.** `ApplyFn` pops the child `Value`, evaluates the
   `FnDescriptor` (a small `MapExpr` evaluator ≈ 150 LOC), pushes the scalar. Both Rust and TS
   run the **same serialized descriptor** — this is the point at which LOCK-5's regex-fused
   `NumberConvert`/`HexConvert` and LOCK-7's `HostClosure` land as **VM facilities**, exactly the
   charter frame.

This is abrogate-before-patch discharged: reuse the ISA, the container, the interner, the
MessagePack bus, the interpreter loop; add one opcode + one side table + four Value arms. A
from-scratch private ISA would re-derive all of that and its consumers. It is **not** a missing
primitive of equal difficulty to the original problem (the BLOCKED-candidate test) — the
descriptor evaluator is bounded, and the descriptor already crosses as data; it is a build task.

---

## 3. Q5 — THE SERIALIZABLE-PLAN OBSERVABLE (one plan, two interpreters, byte-identical)

The 4B/4C distinction is the crux of Q5, and it must be stated precisely, because "one plan, two
lowerings" means **different things** on the two families:

- **4B's observable** is *one plan → two source EMITTERS → two straight-line parsers* whose
  **parse outputs** are differentially equal. The plan (the comptime `IrNode` with annotations)
  is shared; the artifacts (Rust monomorphized `fn`s vs TS emitted `function`s) are two different
  mechanisms with **no shared runtime**. The observable is a differential oracle, never a shared
  binary (pass-3's Q1/Q5 reconciliation).
- **4C's observable is strictly stronger and simpler**: *one plan → ONE artifact (the serialized
  `BytecodeProgram` bytes) → two INTERPRETERS executing the identical bytes.* Nothing is emitted
  per language; the `Vec<Op>` + string table is a flat linear program with no recursive ownership
  to mirror, so the TS side is a `while(pc<code.length) switch(op.tag)` loop over the same array.
  **There is nothing to keep in sync — the parser is data.** This is the only place in LOCK-4
  where constitution-clause-iii ("rust/ts isomorphism") is satisfiable *by construction* rather
  than by a mirror-linter, because the isomorphism is a fact about the wire format.

The distinguishing observable Q5 asks for is therefore **derivable and cheap**: serialize a
`BytecodeProgram` with `rmp_serde::to_vec_named` (field-named MessagePack; `Op` serializes as
externally-tagged variants `{"MatchString": 3}` — a stable, decoder-friendly tag stream), ship
the bytes, decode in TS, and assert both interpreters produce identical `Value` trees. The plan
that drives 4B's emitter (the same `IrNode`/`BytecodeProgram`) *also* drives this crossing — one
plan, but the 4C lowering is **interpret-on-both-sides from one artifact**, the strongest form of
the observable, at the cost of being the slow terminal. **The two are complementary, not rival:
4C is the correctness backstop under which 4B's two fast emitters are proven equal.**

---

## 4. Q6 — ISOMORPHISM-OF-RECORD STANDARD + the value-plane home

### 4a. The dual oracle (both clauses must hold; the golden must be LIVE)

For a program crossing as data (serialize Rust, decode TS), isomorphism-of-record is:

- **Clause A — opcode-stream identity.** Decode the bytes in TS into `Op[]`; assert structural
  equality against a **Rust-emitted live golden** — a `serde_json::to_string` of the *same*
  `BytecodeProgram` the Rust interpreter is about to run, emitted in the same process, not a
  hand-authored blob. The differential-testing literature (McKeeman, *Differential Testing for
  Software*, 1998; Csmith, PLDI'11; regex-automata's `DFA::from_bytes` round-trip) warns that a
  fabricated golden hides exactly the divergences the oracle exists to catch — a stale/hand-faked
  golden is a silent tautology. The regen-idempotence form (encode∘decode∘encode = encode, `Op`
  variant tags pinned) is the born-RED guard against enum-reorder wire fragility.
- **Clause B — Value-tree identity.** Run the Rust `Interpreter::run` (`mod.rs:144`) and the TS
  `run()` over the **same corpus with the same leaf substrate**; assert the produced `Value` trees
  are node-for-node equal. This is PROVE-14's differential discipline (SPEC §0/14: "byte-exact
  node-for-node across 320 cold children") reused for the *parser* crossing rather than the *tape*
  crossing.

**Corpus set.** The six R3 JSON corpora (`data`, `twitter`, `citm`, `canada`, `apache-builds`,
`citm_catalog`) for the recognizer plane; plus a **projection-dense** grammar (value.js's CSS
shapes / the math Pratt folds) once the §2 value plane lands, so Clause B exercises `ApplyFn`, not
only `MakeArray`/`MakeTagged`. `canada` is the mandatory adversarial member: PROTO-1 showed the
f64 numeric leaf is where routes diverge (+56% on the emitter), so it is where a VM/emitter/closure
`Value` mismatch is likeliest.

**Critical distinction from PROVE-14 / GN-4.W4 / LOCK-6.** SPEC §0/14 crosses the *offset-tape*
(the **output** of a parse) behind a monomorphic decoder whose oracle is `JSON.parse`. 4C crosses
the *bytecode program* (the **parser itself**) whose oracle is a second interpreter. Orthogonal
and complementary: a grammar authored once in bbnf, compiled once to `BytecodeProgram`, executed
on both sides from the same bytes. This is the isomorphism-of-record 4C uniquely provides, and it
is the empirical content behind INCOMPAT-2's export to LOCK-6 — 4C's crossing is the *behavioral*
(6D) iso that survives even though the fast mechanisms (monomorphization vs emission) do not mirror.

### 4b. The value-plane home (the double-home Q6 asks to split)

`FnDescriptor` has two populations (`fn_descriptor.rs:14-56`):

- **Internal specializations** (`EnumWrap`, `BoxWrap`, `SpanCapture`, `NumberConvert`,
  `HexConvert`) — compiler-produced, no user body. Their evaluator is **IR-VM lowering → LOCK-3
  (3A/3B)**. They are regex-fused (`NumberConvert`/`HexConvert` come from LOCK-5's classify) and
  are pure functions of the recognizer; they belong on the IR-lowering axis, not the host-dispatch
  axis.
- **The user-facing `Expr{MapExpr}`** — and its future sibling `HostClosure{fn_id, arg, ret}`
  (added type-plane-only by PROTO-2) — touch user semantics. Their signature is declared/typed but
  their body (for `HostClosure`) is opaque. This is the **LOCK-7 host-fn lattice** territory.

**4C's recommended split (for GESTALT-Ω ratification, per Q8 report-only discipline):**
internal-descriptor evaluator (Hex/Number/Enum) → **LOCK-3 IR-VM lowering**; user `MapExpr::FnCall`
+ `HostClosure` → **LOCK-7 host dispatch**. The `Op::ApplyFn` opcode and the `fns: Vec<FnDescriptor>`
side table are IR-VM machinery landed in `crates/ir` (LOCK-3's home) under a LOCK-4 probe; only the
*resolution of an opaque `HostClosure` body* reaches LOCK-7. This keeps the recognizer plane
entirely analyzable — the opaque barrier is `HostClosure` alone.

---

## 5. Q7 — SETUP vs VALUE-PLANE vs RECOGNIZER COST (the interpret terminal's honest split)

The interpret terminal pays a per-parse setup the codegen fast face (born with a `BumpSlab`, zero
constructed graph) does not. `Interpreter::new` (`mod.rs:114-130`) allocates, **every parse**:

| structure | init | `mod.rs` |
|---|---|---|
| `values` | `Vec::with_capacity(64)` | :122 |
| `call_stack` | `Vec::with_capacity(32)` | :123 |
| `checkpoints` | `Vec::with_capacity(32)` | :124 |
| `repeats` | `Vec::with_capacity(16)` | :125 |
| `memo` | `FxHashMap::default()` | :126 |
| `memo_starts` | `Vec::with_capacity(8)` | :127 |
| `value_depth_stack` | `Vec::with_capacity(8)` | :128 |
| `rule_stack` | `Vec::with_capacity(16)` | :129 |
| `diagnostics` | `Vec::new()` | :130 |

That is **8 pre-sized `Vec`s + 1 empty `Vec` + 1 `FxHashMap`** = up to 10 allocations before a byte
is read. On small inputs (`data`, 35 KB) this setup is a materially larger share of single-parse
cost than on `canada` (2.2 MB), where it amortizes to noise — which is precisely why PROTO-2's
interpret floor spans **0.021–0.46×**: the low end is setup-dominated small inputs, the high end is
recognizer-dominated large inputs.

**The pooling/arena-reuse that eliminates it.** The setup is trivially poolable because every
structure is `.clear()`-reusable across parses of the same program:

- Add `Interpreter::reset(&mut self, program, input)` that `.clear()`s the nine Vecs and the
  FxHashMap (retaining capacity) instead of reallocating — the standard object-pool / arena-reuse
  pattern. A thread-local or `parse_with_ir`-owned pooled `Interpreter` reused across calls drops
  the per-parse allocation to **zero** after the first parse.
- This is the pass-3 D-rescue, **correctly scoped**: research-B established the *fast face has no
  setup to pool* (the emitter is born with the arena), so pooling helps **only the interpret
  oracle** — it makes the correctness backstop cheap enough to run over the full corpus in CI, but
  it **cannot** make the interpret path a competitive terminal (the dispatch tax and the
  `Value`-heap materialization dominate once setup is free). Its decision-value is oracle
  viability, not a throughput win — carry this honestly.

**The clean three-way split the Q7 evidence must produce** (a dhat/allocation probe over one small
+ one large corpus, paired-cold):
1. **Setup cost** = the 10 allocations of `Interpreter::new` — pooled to ~0 by `reset()`.
2. **Recognizer cost** = the dispatch loop over `match code[pc]` + the leaf matches (the
   irreducible interpret tax, ~2.2× adverse even amortized).
3. **Value-plane cost** = the `Value`-tree heap materialization (`MakeArray`/`MakeTagged`, and
   post-§2 `ApplyFn`) — the tax the 4D question warns of on projection-dense grammars.

The amortization curve is the deliverable: plot setup-share vs input size, showing setup dominates
below ~50 KB and vanishes above ~1 MB, and that pooling collapses the small-input penalty while
leaving (2) and (3) untouched. **Name the negative result plainly:** even fully pooled, the
interpret terminal loses to codegen structurally — the pooling closes the oracle's cost, not a
competitive terminal.

---

## 6. Packrat & diagnostics as VM facilities (the charter clause — already true)

Both are **already opcodes**, not bolt-ons — a genuine point in the family's favour:

- **Packrat.** `MemoCheck{rule_id,hit_offset}`/`MemoStore(RuleId)` (`bytecode.rs`, the Memo* arms)
  + the interpreter's `memo: FxHashMap<(RuleId,u32),(u32,Value,bool)>` (`mod.rs:84`). Emitted
  **only** when the compiler detects direct left recursion (`compiler/mod.rs` `grammar_needs_memo`),
  so the LL(1) path pays nothing — mirroring TS's `PACKRAT_ARMED` latch (R4 §6g, `packrat.ts:156/290`,
  "true no-op until first `memoize()`"). The TS decoder inherits this for free: memo opcodes are
  present-or-absent in the crossed program; the far side needs one `Map` keyed on R4's float-safe
  `getCijKey` arithmetic (`packrat.ts:71-100`).
- **Diagnostics.** `DebugBreak{rule_id,is_entry}` opcodes + `source_map: Vec<SourceMapEntry>`
  (`bytecode.rs`, PC→grammar-span) + `follow_sets` for furthest-failure expected-token reporting
  (`mod.rs:335-356`). A whole DWARF-like plane compiled into the program and crossing **as data** —
  the TS side gets source-mapped diagnostics from the same bytes, which is PT-3.W2's per-parse-scoped
  diagnostics (R4 §5 PT-E1) satisfied *structurally* (the scope is the parse of one program), not
  by a module-global latch. This is the 4C answer to why the crossing subsumes PT-E1's ask.

---

## 7. Q3 (shared) — the opaque barrier, at the node level

PROTO-2 (pass-3) built the node-level census: value.js math grammar in isolation = **4.839%**
opaque nodes (6 HostClosure leaves / 124 nodes, K=13.78 site→node dilution); repo-wide projection
= **1.857%** (25.58% of sites → sub-2% of nodes). **The barrier is repo-wide-defensible but
fragile locally** — closure-dense sub-grammars (math) exceed 2%. For 4C this matters because the
opaque `HostClosure` is the ONE `Value`/`ApplyFn` the interpreter cannot see through for analysis
(its body is a named call-out); the recognizer plane (MatchString/Dispatch/Repeat/Seq/Alt) stays
fully analyzable and fully crossable. The barrier does not obstruct the crossing (an opaque
`ApplyFn(FnId)` still serializes and still dispatches identically on both sides) — it obstructs
value-plane *type inference*, which is LOCK-1/LOCK-7 territory, not 4C's.

---

## 8. Q8 (report-only) — predecessor shapes as inputs to 4C

- **LOCK-5 (shared-substrate semantics).** XL-C has retired the regex *functor*: two domain-fit
  types (opaque `Regex(StringId)` grammar tier; rich `Hir` engine tier) with a cheap bridge; the
  shared-IR question no longer hinges on ONE-HIR. **Input to 4C:** the crossing already treats
  regex as data — `MatchRegex(sid) → strings[sid] → new RegExp(pattern,"y")` on the far side
  (R4 `leaf.ts:317-369` sticky-`test()`), Rust re-deriving `parse_that::regex::Dfa` via
  `prepare_regexes` (`bytecode.rs:220-242`). The open sub-question 4C **inherits** (does not
  settle): a pattern the bespoke DFA and V8's `RegExp` disagree on (backtracking, Unicode classes)
  — LOCK-5 must certify the grammar-lowerable regex class the crossing is restricted to, with a
  per-pattern differential oracle. Disposition: OPEN at LOCK-5, bounds 4C's crossed regex surface.
- **LOCK-7 (host-fn lattice).** Owns the typed shape of `HostClosure` for value.js's ~5 Pratt
  folds (the 33 opaque sites, ~1.86% of nodes). **Input to 4C:** the `fns: Vec<FnDescriptor>` side
  table's `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` entries are LOCK-7's to type; 4C's
  `ApplyFn` handler dispatches them but does not define their lattice. Disposition: OPEN at LOCK-7;
  4C consumes the verdict, the recommended internal(LOCK-3)/user(LOCK-7) split (§4b) is the
  GESTALT-Ω ratification input.

---

## 9. Prior art (with citations)

- **LPeg / parsing-machine lineage.** Medeiros & Ierusalimschy, *A Parsing Machine for PEGs*
  (2008); LPeg's ISA (`IChar`, `ITestChar`, `IChoice`, `ICommit`, `ICall`, `IRet`, `IFail`) is a
  near-exact analog of this family's `MatchString`/`Dispatch`/`SaveState`/`Call`/`Return`/
  `JumpIfFail` — combinator→bytecode lowering is a settled technique with a published portable ISA;
  LPeg's `re` module is the same "grammar compiles to program, program is the runtime" shape.
- **Specializing/adaptive interpreters.** CPython 3.11 PEP 659 and Lua 5's register-VM turn — the
  canonical evidence a compact bytecode + tight switch loop beats tree-walk *only when per-op work
  amortizes dispatch cost*. The relevance and the risk are identical: measured, not assumed (§10 R1).
- **Portable-program crossing.** WebAssembly and Java bytecode: existence proofs that a
  language-neutral opcode/operand stream decodes identically across implementations.
  **regex-automata's `DFA::from_bytes`** is the closer analog — a compiled matcher shipped as
  `&[u8]` and executed without recompilation ("parser as data"), the same crate `bbnf-regex`
  re-implements (R3 §3). **tree-sitter** compiles grammars to a serialized language artifact loaded
  by a generic runtime — author once, execute from a portable artifact, cross-language by construction.
- **Differential testing (binds the oracle).** McKeeman 1998; Csmith (PLDI'11); the regex-automata
  serde round-trip suite — the discipline that a golden must be *live*, and that encode∘decode∘encode
  fixpoints catch wire-tag reorder.
- **Negative prior art (binds this family).** PROTO-1 (decoder −22%, emitter ×1.23), PROTO-2
  (interpret 0.021–0.46×), SPEC §0/09's refuted shim, PROVE-11's halved dispatch (+10.2%→~+5%) —
  the standing lesson that "obvious" wins evaporate under paired interleaving. A VM's *plausibility*
  is not its *speedup*; 4C's honest terminal is the oracle, not a fast path.

---

## 10. Named risks & counterexamples

- **R1 — dispatch tax swamps the win (confirmed, twice).** The switch-loop pays a per-op indirect
  branch inlined closures/monomorphized structs do not. *Already measured negative* (PROTO-2
  0.021–0.46×); the risk now is **misreporting** the VM as anything but the interpret/oracle lane.
  Guard: 4C carries zero throughput claim; its gates are correctness (dual oracle) and oracle-cost
  (pooled setup), never a beat.
- **R2 — value-materialization tax (unbuilt, unmeasured).** The §2 `ApplyFn` completion interposes
  a descriptor dispatch per projection; on projection-dense grammars (value.js's 59 `all()`-of-
  mapped-parts) the `Value`-heap + descriptor eval could dwarf the recognizer. *Counterexample to
  hunt:* a CSS/math grammar where `ApplyFn` dispatch + `Value::F64` boxing loses hardest. The
  cost-split (§5) must isolate plane (3) from plane (2).
- **R3 — the golden is faked (the oracle's own failure mode).** A hand-authored `Op[]` blob or a
  stale golden turns Clause A into a tautology. *Guard:* the golden is `serde_json::to_string` of
  the live `BytecodeProgram` in the same process; regen-idempotence (encode∘decode∘encode) with
  pinned variant tags is born-RED.
- **R4 — enum-tag wire fragility.** MessagePack externally-tagged `Op` variants are index-order-
  sensitive; a reordered `Op` enum silently breaks the crossing. *Guard:* `to_vec_named` + explicit
  variant tags + the idempotence gate.
- **R5 — regex divergence across the boundary.** Rust re-derives `parse_that::regex::Dfa`; TS uses
  native `RegExp`; semantics can differ. *Counterexample:* a pattern the DFA and V8 disagree on.
  *Guard:* restrict the crossed regex to LOCK-5's certified grammar-lowerable class + per-pattern
  differential oracle (§8).
- **R6 — the dependency-homing cycle (homing, not ISA, risk).** `bbnf_ir` depends on
  `parse_that = "0.4"` (`crates/ir/Cargo.toml`, for `regex::Dfa`). A parse-that-side VM reusing
  `bbnf_ir::vm::Op` inverts into a `parse_that → bbnf_ir` cycle. *Resolution* (LOCK-3/LOCK-6 seam):
  hoist `Op`/`BytecodeProgram`/interner into a third dependency-free crate both consume, or
  parse-that vendors the ISA under a sync gate — **rehome the shared ISA, do not invent a second.**
  Note gate-0 (4D) already severed the cycle mechanically via a `pub use bbnf_regex::*` re-export.

---

## 11. What a prototype must test to move this family

1. **The value-plane completion, LANDED (§2).** `Op::ApplyFn` + `fns: Vec<FnDescriptor>` + `Value`
   scalar arms + the `MapExpr` evaluator, replacing the `Map`-drop at `compiler/node.rs:31`.
   Gate: the VM produces value-projected output **byte-identical to the closure core's `.map`** on
   a projection-dense grammar (differential oracle vs `parse_with_ir`+projection vs parse-that's
   closure parse). *This converts the substrate from recognizer to combinator core — the family's
   central unbuilt claim; the pass-3 "already built" line is false on the tree of record.*
2. **The ~200-LOC TS switch decoder + the DUAL ORACLE (G-C1, RF-2).** A live Rust `to_vec_named`
   serialize → TS decode → (a) `Op[]` structurally identical to a **live** Rust golden, (b) TS
   `Value` tree node-for-node equal to the Rust `Interpreter` over the six R3 corpora + one
   projection-dense grammar. This is simultaneously the 4D-TS **correctness** graft and the
   behavioral-iso (6D) evidence LOCK-6 needs. Cheapest high-value experiment in the lock.
3. **The setup/recognizer/value cost split, pooled (Q7 / §5).** dhat allocation + paired-cold time
   over one small (`data`) + one large (`canada`) corpus; add `Interpreter::reset()` object-pool;
   plot the amortization curve; report setup-share collapsing to ~0 while (2)+(3) hold. **Name the
   negative result:** pooled interpret still loses to codegen structurally — pooling closes the
   *oracle's* cost, not a terminal.
4. **Wire-contract regen-idempotence (G-C4).** encode∘decode∘encode = encode across the corpus,
   `Op` variant tags pinned — born-RED against R4.
5. **The dependency-homing spike (R6).** Prove `Op`/`BytecodeProgram`/interner extract into a
   dependency-free crate both `bbnf_ir` and a parse-that scratch consume — a `cargo tree` with no
   `parse_that → bbnf_ir → parse_that` edge.

The family's strength is that its substrate is not speculative: a serde-derived, MessagePack-bussed,
30-opcode value-tree VM with a compiler, an interpreter, a DAP consumer, and a JSON bench ship
in-tree. Its honesty burden is equally concrete and **larger than pass-3 recorded**: the VM is
value-STRUCTURAL today (the `Map`-drop at `node.rs:31`, `Value` at 4 shapes, no `ApplyFn`), its
value plane is an *unbuilt* completion, and its throughput against every fast face is measured-
adverse. 4C advances on the first landed value plane and the first live dual-oracle number — or it
banks, honestly, as the interpret terminal and the cross-language isomorphism-of-record oracle,
which is a real and needed role, not a fast one.
