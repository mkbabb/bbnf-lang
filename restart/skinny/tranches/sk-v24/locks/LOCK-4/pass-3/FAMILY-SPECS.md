# LOCK-4 — THE COMBINATOR CORE · PASS-3 FAMILY SPECS

*Synthesis agent, pass 3 (Fable-tier judgment). One cogent spec per family while routes remain
incompatible, then the incompatibility map and the BLOCKED-candidate determinations. Inputs folded:
pass-3 research-{A,B,C,D,priorart}, **PROTO-1 (Futamura codegen → recursive JSON, RAN/MEASURED/MIXED)**,
**PROTO-2 (general emitter through GAP-1 + node-level barrier census, RAN — PART 2 PASS, PART 1
QUALIFIED/FAIL)**, the pass-2 REGISTRY (status floors, keystone set, CROWDING), CROSS-LOCK (XL-A..H),
PORTFOLIO/CONSTITUTION. Prior measured events carried as facts: pass-2 PROTO-1 (TS face — emitter
×1.23 / decoder −22%), pass-2 PROTO-2 (gate-0 PASS + interpret floor 0.021–0.46×). **HEAD of record for
correction: `c43126422` (descendant of base-pin `ce51c36fc`); both protos base-pinned `ce51c36fc`,
ancestry GREEN, no wrong-artifact-risk.***

---

## THE PASS-3 SHAPE-CHANGE (the finding that reorganizes the lock — now with two builds behind it)

Pass 2 exported the terminal election and left seven keystones, the sharpest being **RF-1** — "does
the *runtime* combinator API reach comptime monomorphization, or only via the generator's codegen
path?" — flagged as the tower's *incoherent middle*, SPEC-ONLY.

**Pass-3 resolves RF-1 analytically AND grounds the resolution on running artifacts.** The answer is
**NO — a category error in Rust — and the fast face that replaces it is now BUILT and MEASURED, not
merely proven to exist for regex.**

- A `SmallBox<dyn ParserFn>` (`parse.rs:74`) is **type-erased at construction**; a comptime type
  (`Then<Str,Regex>`) is unrecoverable from it (research-B §1, research-A Q1, research-D §1). The only
  bridge from a grammar *value* to monomorphized code the literature knows is the **First Futamura
  Projection** — build-time codegen over a **comptime-known** graph (research-priorart §1; Jonnalagedda
  OOPSLA'14, flap PLDI'23). The runtime combinator graph therefore has **exactly two** lowerings:
  *interpret* (the VM — measured ~2.2× adverse even amortized, pass-2 PROTO-2) or
  *serialize-then-offline-codegen*. Monomorphization belongs to the codegen front, never the runtime API.

**The lock has a confirmed STAGING BOUNDARY in its middle — two fronts, not four rivals.**
- **The RUNTIME front** (`str("a").then(b)` → a runtime `IrNode`): ergonomic public API + correctness
  oracle. Its only fast option is to be reified and handed offline to codegen; run at runtime it is the
  interpret terminal (the debug/LSP/dynamic-grammar lane, slow-by-construction).
- **The CODEGEN front** (comptime-known grammar/`IrNode` → emitted straight-line source): the **fast
  face, on BOTH languages.** XL-D settled this for TS (emitter, not decoder); pass-3 confirms it is *also
  the Rust story*.

**THE THREE THINGS THAT RAN THIS PASS (the pass-2 "0% built" keystones, now with data):**

1. **[PROTO-1 — the codegen fast face GENERALIZES to recursive JSON, and is measured against the shipped
   incumbent. RAN/MEASURED/MIXED.]** The pass-2 claim was "the emitter ships for *regex* (`generated.rs`);
   generalizing to an arbitrary grammar is unbuilt." Pass-3 built it: `grammar/json/json.bbnf` (recursive,
   value-bearing) emitted by the *same* `crates/core/src/backend/rust/emitter/` path to
   `crates/core/src/grammar/generated/json.rs` (3505 LOC). **GATE A PASS** — hot-path census: **0
   `SmallBox` / 0 `dyn ParserFn` / 0 `Rc`/`Arc` / 0 fn-ptr fields**; value recursion lowers to
   **direct, byte-dispatched `fn` calls** (`b'{' => parse_object_…`, `b'[' => parse_array_…`), ZST
   `JsonParser`, `'a`-scoped by construction (the lone `Box<` is the escaped-string cold branch,
   `json.rs:1439`). The "JSON has left-recursion, regex does not" objection **dissolves on a running
   artifact.** **GATE B (vs the SHIPPED, tuned `json_parser()` — the mandatory production-dilution arm,
   G-B1 — same parse_that core both arms):** two windows, both orders, cold-paired divan, agree exactly:
   **data 0.65–0.73× (emit WINS), twitter 0.75–0.95× (WINS), citm ~1.0–1.14× (TIE), canada 1.50–1.60×
   (SLOWER).** The win concentrates on string/structure/small; the loss is **entirely localized to the
   f64 numeric leaf** — canada is a wall of floats, and the incumbent's tuned number path (eisel_lemire +
   SIMD `dispatch_byte_multi`) outruns the emitted `-> f64`. **The strict election gate ("beats SOTA
   everywhere") is NOT cleared; the refutation form ("below the closure core ⇒ interpret is the only Rust
   story") is ALSO refuted.** The honest read: **4B is architecturally proven and competitive; a single
   named lever — the numeric-leaf fast-path — stands between "mixed" and "uniform beat / elect."**

2. **[PROTO-2 PART 2 — GAP-1 DISCHARGED. The general emitter survives an arbitrary host closure. PASS.]**
   The value-plane wall both prior probes dodged (they routed MapExpr-expressible projections) is now
   crossed. `FnDescriptor::HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` added (sibling to
   `NumberConvert`/`HexConvert`), all 7 exhaustive `crates/ir` match arms wired, **`bbnf-ir` compiles
   clean (exit 0, 2.31s)**. The emitted `lerp` fused region is a **straight-line recognizer (0
   SmallBox/dyn/vtable)** with the arbitrary closure fused as **exactly one named call-out**
   (`host_closure_15(inner)`) — the body opaque, the `arg→ret` signature keeping the node an analyzable
   graph leaf. This is not a MapExpr toy; the same call lowering carries an arbitrary closure. *(Caveat
   of record: the `crates/core` emission arms — `driver/map.rs` + the `emit_host_closure` default trait
   method — ship as a patch, compile-plausible via the default-method precedent but NOT cold-built in the
   cap; the type-plane change is compile-proven, the emission dispatch is patch-plausible.)*

3. **[PROTO-2 PART 1 — the node-level barrier census is BUILT. QUALIFIED-PASS repo-wide / FAIL math-only.]**
   Pass-2 asserted "≤2% is a NODE ratio, not a SITE ratio" but never measured the collapse. Pass-3
   measured it: value.js **math grammar in isolation = 4.839% opaque nodes (6 HostClosure leaves / 124
   nodes)**, site→node expansion **K = 13.78**; applying K to research-D's repo-wide 25.58% opaque *sites*
   → **1.857% opaque nodes repo-wide.** So the ≤2%-of-nodes floor is **cleared repo-wide but by only a
   7.7% cushion (K=13.78 vs the 12.79 needed), and FAILED locally in closure-dense sub-grammars (math
   4.84%).** Consequence: recognizer-plane analysis (fusion, `AltDispatch`/`TokenDispatch`, prefilter,
   span — every SPEC §0 perf win) is **unblocked in aggregate**, but the "analyzable graph" reading is a
   **repo aggregate, not a per-grammar invariant** — math-dense regions read as "skeleton with opaque
   leaves."

**Net pass-3 disposition.** No family converges; the terminal remains un-electable inside LOCK-4 (the
Q-ownership fork is LOCK-5's, the iso-verdict LOCK-6's). But the lock's *architecture* is now settled AND
partly built: **a two-front core split by a staging boundary, whose fast face on both languages is the
build-time emitter — generalized to recursive JSON, measured competitive-but-not-uniform against the
tuned incumbent, and proven to fuse through an arbitrary host closure.** The families are no longer four
rival terminals — they are the named parts of this one architecture, plus one predecessor-blocked
ownership question and one still-UNBUILT correctness backbone (4C's value plane — see the correction).

---

## THE CORRECTION THIS PASS OWES (folded from research-C, tree-verified on HEAD c43126422)

The pass-3 FAMILY-SPECS draft that preceded the protos stated 4C's "value completion is built and
byte-identical (`Op::ApplyFn(FnId)` … `apply_fn.rs` … oracle GREEN)" and cited "~2090 LOC / 33-variant
Op." **All four are FALSE on the tree of record**, verified this session:
- `crates/ir/src/vm/bytecode.rs` `enum Op` has **30 semantic variants; NO `ApplyFn`** (`rg ApplyFn
  crates/ir/src` = 0 hits).
- `crates/ir/src/vm/interpreter/` = construct/control/leaves/memo/mod/repeat/value.rs — **no `apply_fn.rs`.**
- `Value` (`value.rs:52`) = **4 shapes** (`Span`, `Tagged`, `Array`, `Nil`) — **no scalar arm.**
- `compiler/node.rs:31`: `IrNode::Map { inner, .. } => self.compile_node(inner, ir)` — **the `fn_id` is
  silently dropped; every `->` projection discarded at compile.** `crates/ir/src/vm` = **1772 LOC.**

The conflation: pass-3 PROTO-2 added `FnDescriptor::HostClosure` to the **type/emitter** plane (and it is
*not* landed on base HEAD either — `rg HostClosure fn_descriptor.rs` = 0). It did **not** add `Op::ApplyFn`
to the VM. **The in-tree VM is a value-STRUCTURAL recognizer (offsets + rule tags); making it a combinator
core that carries `.map` is the family's central UNBUILT completion** — the exact no-value-discard
violation the seed names. It is a build task (4 mechanical edits, §4C), **not** a missing primitive.

---

## 4A — CLOSURE-TO-GRAPH · role: the RUNTIME-front reifier + pre-codegen pass host

**Architecture.** Combinator constructors become **smart constructors returning graph nodes**:
`string("a")→Node::Literal`, `a.then(b)→Node::Seq`, `a.map(f)→Node::Map{fn_id}`. The graph is not
greenfield — `bbnf_ir::IrNode` (`crates/ir/src/types/node.rs:30`) is a live serde-serializable **14-variant
near-bijection** with the visitor (`for_each_child`, `:145`), hash-consed `GrammarDag` (`dag/mod.rs:62`),
and the fusion/dispatch/LR passes (`passes/transform.rs`, `passes/sets.rs`, `passes/lr.rs`) already built
and **consuming** (not cloning) the e-graph. parse-that's own `span_parser/` (enum-dispatched, vtable-free
partial node graph) is the native seed.

**Mechanism / role after RF-1.** Construction is pure tree-building; no parse logic runs until a lowering
consumes the graph. 4A is **the runtime-front reifier and the home of the pre-codegen passes — NOT a
terminal.** Its graph feeds either the interpret terminal (VM) or, reified and handed offline, the codegen
fast face (4B). Fusion, dispatch-table derivation, and packrat placement are graph passes that run *before*
the staging boundary. PROTO-1 grounds the downstream: the codegen front that consumes a comptime-known
graph is now proven to emit a zero-box recursive parser — so 4A's "erase the graph into AOT rather than
run it" role is the correct one, and PROTO-2's adverse interpret number (pass-2) supports it.

**Gates it would carry.**
- **G-A1 (owed FOUR passes, still 0% built):** `fuse(Graph)→Graph` on a **combinator-constructed**
  `IrNode` (not `.bbnf`-text IR) byte-identical to a hand-fused arena (structural-equality oracle) AND
  JSON/CSS cold-paired ≥0.98×. The passes exist for the *generator's* IrNode; pointing them at a combinator
  front and proving byte-identity is the unbuilt claim.
- **G-A3 (Q-A2) — NOW BUILT, thin:** the opaque-node ratio is measured (PROTO-2 PART 1): 1.857% repo-wide
  (QUALIFIED, K=13.78) / 4.84% math-only (FAIL). Node-thin in aggregate, fragile locally.

**Exact open questions.**
1. Does `fuse(Graph)→Graph` on a combinator-constructed graph match a hand-fused arena byte-for-byte? (The
   one novel 4A claim, zero running evidence in four passes.)
2. **[RF-1 consequence, unbuilt]** 4A-as-source-front is coherent only if the reified *runtime* graph can be
   serialized and fed to the **same** emitter as the grammar-text path, producing graph-of-record parity.
   PROTO-1 proved the grammar-TEXT→emitter path generalizes; the runtime-graph→serialize→same-emitter
   equivalence is still unbuilt.
3. **[cross-family, folded]** pass-2 PROTO-1's runtime-switch refutation (−22%) also refutes a 4A-TS
   `switch(node.tag)` graph interpreter — 4A-TS must lower through the 4B-TS emitter. 4A is a source front
   on both languages, never a runtime terminal.

---

## 4B — COMPTIME-FUSION · role: the CODEGEN fast face (Rust monomorphization + TS emitter) · the pass's strongest family, now with a measured election lever

**Architecture.** Fusion is the **First Futamura Projection**: partially-evaluate the combinator interpreter
against a comptime-known grammar; a straight-line parser falls out — the vtable, the per-construction box,
and the setup graph *deleted by construction*. This is not a redesign of the box; it is the box's deletion —
and PROTO-1 proves it **ships and generalizes**: the regex `generated.rs` shape extends to recursive,
alt-dispatched, value-bearing JSON with 0 boxes on the hot path. The three in-tree substrates ARE the three
arms of the Q4 cost model: closure core = {box-here}; `span_parser/` enum tier = {enum-node} at runtime;
`generated.rs`/`json.rs` = {inline-monomorphize + box-at-back-edge} by codegen.

**Mechanism.** ONE serializable plan — the comptime `IrNode` with SCC/arity/projection annotations — drives
(i) a Rust source emitter (`fn __rule(state)->Option<Enum>` + `BumpSlab`, the `generated.rs` shape) and (ii)
a TS source emitter (direct calls, the megamorphic `.parser` IC gone). **Different mechanisms** (Rust
monomorphization has no V8 analog; TS emission has no Rust analog) but **one plan** — the reconciliation of
Q1 and Q5: the shared object is the *plan*, not the artifact; the observable is a differential oracle, not a
shared binary.

**Integration seams.** `parse_that/src/regex/generated.rs` + `crates/core/src/grammar/generated/json.rs`
(the two existence proofs); the bbnf emitter `crates/core/src/backend/{rust,ts}/emitter/`; cost-model homes
`crates/ir/src/cost.rs`, `egraph/cost.rs`, `passes/csp_strategy/mod.rs`; concrete firing-number anchors
(research-B §11): `fuse_single_use` gated by `is_composite_seq`(Seq≥2)/`body_has_map`/`is_consumer_pinned`
(`passes/transform/fuse.rs:44/201/220/249`), token-dispatch `try_factor_alt` **fires at `branches.len() >=
4`** (`fuse_token/factor.rs:23` — a concrete in-tree dispatch-fanout number), map fusion `compile_map` folds
`Map(Map(x))` (`driver/map.rs:16`). The numeric-leaf incumbent to match: eisel_lemire + `dispatch_byte_multi`
(parse_that json parsers).

**Gates it would carry.**
- **G-B0 (the generalization) — PASS (PROTO-1 GATE A).** Recursive JSON emitted, 0 SmallBox/dyn/fn-ptr,
  value recursion → direct byte-dispatched `fn` calls. *(One un-instrumented sub-claim: construction
  allocs <6 / per-parse =1 were not dhat-isolated in the cap; source census + clean fat-LTO compile is the
  evidence carried.)*
- **G-B1 (production dilution vs the SHIPPED `json_parser()`) — MEASURED, MIXED (PROTO-1 GATE B).** data/twitter
  WIN (0.65–0.95×), citm TIE, canada REGRESS (1.50–1.60×). The 16–27% was an upper bound on a *stripped*
  grammar; the real shrink is workload-dependent, +35%↔−56%, loss confined to the f64 leaf.
- **G-B3 (general emitter through GAP-1) — PASS (PROTO-2 PART 2).** Straight-line recognizer through an
  arbitrary host closure, 1 named call-out, `bbnf-ir` compile-proven. *(core emission arms patch-plausible,
  not cold-built.)*
- **G-B4 (the gate-that-cannot-fail, still UNSET across THREE passes):** a firing budget — named fuse-arity
  (TS `fuseAll` flips unroll→loop at 3; Rust `seq!/alt!` unroll to 8), dispatch-fanout (grounded:
  `try_factor_alt` at 4), boxing-depth (topological: exactly one box per SCC back-edge), and a `.rlib`/`dist`
  + incremental-compile budget measured on CSS **and** value.js. Anchors exist; a measured number does not.

**Exact open questions.**
1. **[THE election lever, named by PROTO-1]** Can the emitted `-> f64` numeric leaf be brought to incumbent
   parity (route it through the same eisel_lemire + SIMD kernel), turning canada's +56% regression into a
   ≤1.05× tie? This is the single lever between "mixed" and "elect 4B as the Rust fast face."
2. What is G-B4's *number*? (Anchors exist; a measured budget does not — three passes owed.)
3. **[extant-tranche, owed]** 4B's confirmed Rust perf claim (indirect-call + codegen axis) vs PT-2's
   PROVE-09 zero-claim posture (SIMD/arena axis) — reconcile as a NAMED supersession (XL-E covers the
   principle; no PT-tranche home named).

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · role: the interpret terminal + the cross-language iso-of-record oracle · value plane UNBUILT (corrected)

**Architecture.** A working grammar VM ships — `crates/ir/src/vm/` (**1772 LOC**, tree-verified): a
**30-variant** `Op` ISA (`bytecode.rs`, LPeg-shaped), an `IrNode`→bytecode compiler, a stack-machine
interpreter, a live DAP consumer (`crates/lsp/src/dap/adapter.rs`) + JSON VM bench. Both `Op` and
`BytecodeProgram` derive Serialize — the program **crosses as data**; isomorphism is a property of the
*representation*, not a mirrored-source linter. After pass-2 PROTO-1/PROTO-2 (switch-VM −22%; interpret
0.021–0.46×), the VM is **measured-refuted twice as a fast terminal** and survives as (a) the interpret
terminal (DAP/LSP/dynamic-grammar lane) and (b) the **cross-language iso-of-record oracle**.

**Mechanism — the value plane is UNBUILT, not built (correction).** Today the VM is value-STRUCTURAL: `Map`
is compiled to `compile_node(inner)` with the `fn_id` **dropped** (`compiler/node.rs:31`); `Value` has 4
shapes (no scalar); there is **no `Op::ApplyFn`.** The completion is four mechanical edits on structures
that already serialize (research-C §2): **(1)** add `Op::ApplyFn(FnId)` and lower `IrNode::Map` to
`compile_node(inner)` **then** `emit(ApplyFn(fn_id))`; **(2)** add `#[serde(default)] fns: Vec<FnDescriptor>`
to `BytecodeProgram` (`FnDescriptor` already derives Serialize; its `Expr{MapExpr}` variant crosses as data);
**(3)** extend `Value` with `F64`/`U32`/`Str`/`Variant`; **(4)** one interpreter handler + its TS mirror
running the **same serialized descriptor** (~150-LOC MapExpr evaluator). This is abrogate-before-patch
discharged (reuse ISA/container/interner/MessagePack bus/loop) — a build task, not a primitive of equal
difficulty.

**Value-plane home (the double-home to split).** Internal specializations (`EnumWrap`/`BoxWrap`/`SpanCapture`/
`NumberConvert`/`HexConvert`, `fn_descriptor.rs:14-49`) are compiler-produced, regex-fused, pure functions of
the recognizer → **LOCK-3 IR-VM lowering**. Only user-facing `MapExpr::FnCall` + future `HostClosure` touch
**LOCK-7 host dispatch**. `Op::ApplyFn` + the `fns` side table are IR-VM machinery in `crates/ir` (LOCK-3's
home) under a LOCK-4 probe; only opaque-`HostClosure`-body *resolution* reaches LOCK-7. Recommended split for
GESTALT-Ω ratification.

**Gates it would carry.**
- **G-C0 (the value-plane completion — the family's central UNBUILT claim):** land the 4 edits; gate = the VM
  produces value-projected output **byte-identical to the closure core's `.map`** on a projection-dense
  grammar (differential oracle vs `parse_with_ir`+projection vs parse-that's closure parse). *This converts
  the substrate from recognizer to combinator core; the prior "already built" line was false on HEAD.*
- **G-C1 (RF-2, the surviving axis, UNBUILT-of-record — the cheapest high-value experiment in the lock):** a
  *live* Rust `to_vec_named` serialize → TS ~200-LOC switch decode → **dual oracle** (opcode-stream identity
  vs the LIVE golden — not a hand-faked blob — AND `Value`-tree identity vs the Rust `Interpreter`) over the
  six R3 corpora + one projection-dense grammar (post-G-C0). Simultaneously the 4D-TS **correctness** graft
  and the **behavioral-iso (6D) evidence LOCK-6 needs.** `canada` is the mandatory adversarial member
  (PROTO-1's f64 leaf is where routes diverge).
- **G-C4:** wire-contract regen-idempotence (encode∘decode∘encode = encode; `Op` variant tags pinned —
  born-RED against MessagePack externally-tagged-variant reorder fragility).

**Exact open questions.**
1. Does the value plane complete to byte-identity with the closure core `.map`? (0% built; central claim.)
2. Does the program cross byte-identically over six corpora against a *live* golden? (pass-2 PROTO-1's perf
   PASS is not this; the differential-testing literature — research-priorart §4 — warns a fabricated golden
   hides exactly the divergences the oracle exists to catch.)
3. Is 4C the {enum-node} escape hatch when 4B's monomorphization would blow G-B4's budget? (O(program) code
   size, zero blowup — decision-relevant only once G-B4 has a number.)

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · role: the ownership decision + value-plane completion · predecessor-BLOCKED · GAP-1 type-plane DISCHARGED

**Architecture.** Combinators are a construction API over the *same* `bbnf_ir::IrNode` the grammar plane
uses; parse-that keeps its public API and **inherits the generator's optimizer** (`crates/ir/` passes,
rewrites, e-graph, DAG, decision driver). The distinction from 4A: 4A stops at "analyzable graph"; 4D fixes
that graph to *be* `bbnf_ir::IrNode` so the inheritance is literal.

**Mechanism.** The reification seam is 4A's; 4D adds the *ownership* commitment (parse-that's runtime value
IS `bbnf_ir::IrNode`) and the value-plane completion (shared with 4C's G-C0). The two-tier value plane
(research-D §2): reify opportunistically where the closure is MapExpr-shaped (latex-paper 100% of 86 sites,
value.js's 64+19 sites), and — **now compile-proven (PROTO-2 PART 2)** — box the rest as
`FnDescriptor::HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}`, one new typed variant sibling to
`NumberConvert`/`HexConvert`, keeping the closure API unchanged. research-D adds a *tier-1* refinement worth
carrying: ~47% of latex-paper's non-projection sites are object-literal/array/ternary constructions
recoverable as analyzable **data** via `MapExpr::Array` + `MapExpr::Select` + `FnDescriptor::Record`,
leaving `Opaque(HostFnId)` for the true closures only (value.js easing/color) — split the barrier, do not
smuggle one `Box<dyn Fn>` per `->`.

**Gates it would carry.**
- **Gate-0 (DONE, pass-2 PROTO-2 + negative control):** the `parse_that ↔ bbnf_ir` Cargo cycle is severed by
  a 4-line re-export swap (`parse_that::regex` is literally `pub use bbnf_regex::*`); mechanical, not
  LOCK-5-gated.
- **G-D-GAP1 (DONE, PROTO-2 PART 2):** `FnDescriptor::HostClosure` compiles into the IR type-plane (7 arms,
  `bbnf-ir` clean 2.31s) and the emitter fuses an arbitrary closure as 1 named call-out. The value-plane
  wall is crossed on the emitter side.
- **G-D-rescue (DEMOTED by the pass-3 finding):** pool/arena-reuse `Interpreter::new` (7–8 Vecs + FxHashMap)
  and re-run the value floor. research-B/C §5: the *fast face has no setup to pool* (the generated route is
  born with the `BumpSlab`, zero constructed graph); pooling helps **only the interpret oracle** — it makes
  the correctness backstop cheap enough to run over the full corpus in CI, but even a rescued interpret path
  loses to codegen structurally. Its decision-value is oracle viability, not a competitive terminal.

**The block (confirmed, three horns).**
- **Horn (i):** a full `GrammarIR`+interpreter+optimizer port to TS = re-implementing the grammar plane's
  engine in a second language — a primitive of **equal-or-greater difficulty than the original LOCK-4
  problem.** BLOCKED-candidate by the rule's own text. *(But XL-D retires the NEED: the fast TS face is
  per-language emission, not a shared runtime — so 4D's live role does not require this primitive.)*
- **Horn (ii):** the wasm crossing — PROVE-14 measured full-walk value materialization at **0.91×, a LOSS**,
  on exactly the workload a value-producing combinator parser IS.
- **Horn (iii):** the shared-decoder graft is measured-adverse on V8 (−22%); its *fast* form needs the
  build-time emitter, i.e. the same per-language primitive. **4D's thesis "one VM, two sides, for free" is
  refuted for the fast path** (correct + isomorphic as data, slow to run on V8).

**Exact open questions (all predecessor-owned, report-only per Q8).**
1. LOCK-5's substrate ruling: is `bbnf_ir` the genuinely shared HIR (**5A**, "inherit for free") or a lawful
   functor across separate HIRs (**5B**, which evaporates the inheritance)? gate-0-compiles is mechanical/done;
   shared-substrate-*semantics* is open and DAG-blocking.
2. LOCK-7's host-fn lattice: the typed shape + per-subsystem floor for `HostClosure` (value.js's ~5 Pratt
   folds; the tier-1 `Record`/`Array`/`Select` boundary).
3. The node-ratio is BUILT but thin (1.857% repo / 4.84% math) — is the fusion-inheritance claim safe on
   closure-dense sub-grammars, or only in aggregate?

---

## THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The pass-3 staging-boundary finding **collapses most apparent rivalry into composition** and leaves **three
genuine, named forks** — none of which LOCK-4 can elect alone.

**Compositions, NOT incompatibilities (pass-3 corrects the pass-2 "four rivals" framing):**
- **4A ⊕ 4B** compose *across* the staging boundary: 4A reifies the runtime front and runs the pre-codegen
  passes; 4B is the offline codegen of that reified plan. RF-1 makes them *sequential* (reify → fuse →
  serialize → emit), coherent **only** for the codegen front, never the live combinator API as a runtime
  terminal. PROTO-1 grounds 4B's half on a running artifact.
- **4C ⊕ 4D** share the *identical* value-plane completion (`Op::ApplyFn` + `fns` + `Value`-scalar, **both
  UNBUILT**) and the *identical* crossing mechanism; 4C is the iso-oracle, 4D is the ownership commitment.
  Not rivals.
- **4B-TS emitter ⊕ {4A,4C,4D}-TS:** every TS terminal must lower through the build-time emitter (pass-2
  PROTO-1: decoder −22%, emitter ×1.23). Runtime-decoder-as-fast-TS-face is BANKED-by-measurement across all
  three; 4B-TS is the single live TS execution face.

**INCOMPAT-1 — Q-OWNERSHIP: parse-that-private isomorph (4A/4B/4C) vs shared `bbnf_ir` (4D).** Cannot both be
the terminal representation. gate-0 (Cargo cycle) is *mechanically* severed and done; the *semantic* question
(is `bbnf_ir` the shared type, 5A, or a lawful functor, 5B?) is **LOCK-5's to rule.** If 5B, 4D's "inherit
for free" evaporates and the private route wins by default; if 5A, 4D's parsimony dominates *conditional on*
the block clearing. **Irreducible inside LOCK-4; exported to LOCK-5.**

**INCOMPAT-2 — THE FAST TERMINAL, cross-language: Rust monomorphization vs TS emission — different mechanisms,
no shared isomorphic terminal.** Resolved by measurement *in the emitter's favour on TS* (pass-2 PROTO-1) —
but pass-3 PROTO-1 shows the Rust fast face is **also codegen** (the `generated.rs`/`json.rs` monomorphized
shape), and its win is **workload-dependent, not uniform** (data/twitter win, canada regress). The fast Rust
mechanism (monomorphization) and the fast TS mechanism (emission) remain **different mechanisms with no
shared isomorphic terminal** — a standing counterexample to source-level rust/ts isomorphism for the
combinator core. **Exported to LOCK-6**, forcing 6C (wasm-core) or 6D (behavioral/oracle iso, layout free per
language) and away from 6A/6B source-mirroring. The terminal cannot freeze ahead of LOCK-6's verdict. Route
PROTO-1's emitter/V8-discipline result to **LOCK-8** so it is not re-derived.

**INCOMPAT-3 — Q-RUNTIME (reframed by RF-1 + measured by PROTO-1): codegen fast face (4B) vs interpret
terminal (4C-VM / 4A-graph-walk) — NOT incompatible; two terminals for two use-cases.** 4A-interpret and
4C-VM are the *same* runtime-interpret arm (both measured adverse); 4B-codegen cannot express a runtime graph
(Q1). So the fork is binary — *codegen (fast, staged) vs interpret (slow, runtime/debug/dynamic)* — selected
by the cost model, not mutually exclusive. **The genuinely decision-relevant comparison is no longer the
abstract three-way fork but the generalized codegen route vs the SHIPPED closure core — which PROTO-1
MEASURED (MIXED): the codegen route is competitive but numeric-leaf-gated.** The abstract three-way fork is
retired; the concrete open number is the numeric-leaf parity (4B open-question 1).

**The value-plane home (double-home, un-adjudicated).** `Op::ApplyFn` + MapExpr evaluator + `Value` scalar
arms (both UNBUILT) is IR-VM lowering (LOCK-3 3A) that would land in `crates/ir` under a LOCK-4 probe;
internal descriptors (Hex/Number/Enum) → LOCK-3, user `HostClosure` → LOCK-7. **GESTALT-Ω owes the
ratification**; the split above is the recommended disposition, not a settled fact.

---

## BLOCKED-CANDIDATE DETERMINATIONS (the missing-primitive test)

*"A family stalled at a missing primitive equal in difficulty to the original problem is BLOCKED-candidate."*

- **4A — NOT BLOCKED-candidate.** Its keystone (`fuse(Graph)→Graph` byte-identity) is unbuilt for a fourth
  pass, but the passes *exist*; the missing work is pointing them at a combinator front — a build task, not a
  primitive of equal difficulty. Terminal role refuted (RF-1); source-front role coherent and cheap.
- **4B — NOT BLOCKED-candidate; the strongest family, now measured.** The general emitter is BUILT and
  generalizes to recursive JSON (PROTO-1 GATE A) and fuses through an arbitrary closure (PROTO-2 PART 2). The
  gap is the numeric-leaf fast-path + a firing budget — measurable, none equal-difficulty-to-the-original.
- **4C — NOT BLOCKED-candidate; BANKED as the interpret/oracle lane.** Its value plane (G-C0) and iso-oracle
  (G-C1) are unbuilt but *cheap* (4 mechanical edits + a ~200-LOC decoder + a differential test, research-C
  §2 proves the descriptor already crosses as data). No missing primitive. **The pass-3 draft's "already
  built" was a factual error, now corrected.**
- **4D — BLOCKED, split.** Horn (i) — a full grammar-engine port to TS — **is** a primitive of equal-or-greater
  difficulty, satisfying the rule's own text; **but XL-D retires the need for it** (fast TS face = per-language
  emission, not a shared runtime). So: **4D-as-fast-shared-runtime is BLOCKED-candidate (dead by the
  missing-primitive rule AND by XL-D); 4D-as-shared-ownership is DAG-BLOCKED on LOCK-5 (5A-vs-5B) + LOCK-7
  (lattice), with GAP-1's type-plane now discharged.** Do not carry "4D dominates on parsimony ⇒ elect it" —
  dominance is conditional on LOCK-5 ruling 5A, and its Rust runtime is a measured loss.

---

## WHAT REMAINS UN-ELECTABLE INSIDE LOCK-4 (hand-off to the gestalts)

1. **Q-ownership → LOCK-5** (5A shared type vs 5B lawful functor; INCOMPAT-1).
2. **The fast-terminal cross-language iso → LOCK-6** (Rust monomorphization ≠ TS emission ⇒ 6C/6D; INCOMPAT-2).
   Route PROTO-1's emitter result to **LOCK-8** so its V8/codegen discipline is not re-derived.
3. **The value-plane home → GESTALT-Ω** (LOCK-3 internal-descriptor vs LOCK-7 host-closure; recommended split
   above; **note the value plane itself is UNBUILT on the VM**).
4. **4B's perf-claim supersession → PT-tranche** (XL-E principle; no home named).

What IS settled/built inside LOCK-4 this pass: **the architecture** — a two-front core split by a staging
boundary; the codegen fast face **generalizes to recursive JSON (PROTO-1) and fuses through an arbitrary host
closure (PROTO-2)**, measured competitive-but-numeric-leaf-gated against the shipped incumbent; the
opaque-node barrier is **built (1.857% repo / 4.84% math — repo aggregate, not per-grammar invariant)**. What
is corrected of record: **4C's value plane is UNBUILT** (the "built + byte-identical" claim was false on
HEAD). The election is a gestalt act; the two prototype briefs below are the sharpest levers toward it.

---

## PROTOTYPE BRIEFS FOR THE NEXT PASS (the two sharpest uncertainties)

**BRIEF 1 — the numeric-leaf election lever (family 4B; kind bbnf-worktree).** PROTO-1 measured the emitted
codegen JSON parser regressing +50–60% on `canada` (all-f64) against the shipped `json_parser()`, the loss
localized entirely to the `-> f64` numeric leaf (incumbent = eisel_lemire + SIMD `dispatch_byte_multi`). This
is the single lever between "mixed" and "elect 4B as the Rust fast face." Route the emitted number leaf
through the incumbent's numeric kernel; re-run canada, cold-paired, two windows both orders. **PASS/FAIL:
canada emit/incumbent fastest-sample ratio ≤ 1.05× (from 1.50–1.60×), no regression on data/twitter/citm.**

**BRIEF 2 — the 4C value plane + the live dual iso-oracle (families 4C/4D; kind bbnf-worktree).** Correct the
record and build the never-built correctness backbone in one probe: land the 4 mechanical edits (`Op::ApplyFn`,
`fns: Vec<FnDescriptor>`, `Value` scalar arms, ~150-LOC MapExpr evaluator), then a live Rust `to_vec_named`
serialize → TS ~200-LOC switch decode → dual oracle. **PASS/FAIL:** VM `ApplyFn` output byte-identical to the
closure core `.map` on a projection-dense grammar AND opcode-stream identity vs a LIVE Rust golden + `Value`-tree
node-for-node identity across ≥6 corpora (canada mandatory). See the briefs returned in the structured output.
