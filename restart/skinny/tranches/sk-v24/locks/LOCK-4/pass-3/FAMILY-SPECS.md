# LOCK-4 — THE COMBINATOR CORE · PASS-3 FAMILY SPECS (re-synthesis, post-probe)

*Synthesis agent, pass 3 (Fable-tier judgment). One cogent spec per family while routes remain
incompatible, then the incompatibility map and the BLOCKED-candidate determinations. This
supersedes the 15:22 draft of this file, which the REGISTRY correctly declares STALE: that draft
froze **before** its own two closing briefs ran, and its convergence framing ("single numeric
lever", "0% built") is dead. Inputs folded: pass-3 refreshed research-{A,B,C,D,priorart};
**PROTO-1** (Futamura codegen → recursive JSON — RAN/MEASURED/MIXED); **PROTO-1-numeric-leaf**
(BRIEF-1, the election lever — RAN/MEASURED/**FAIL**); **PROTO-2** (general emitter through GAP-1
+ node-barrier census — RAN, PART 2 PASS / PART 1 QUALIFIED); **PROTO-2-vmvalue** (BRIEF-2, the 4C
value plane + live dual iso-oracle — RAN/**BUILT-IN-WORKTREE**/8-8-GREEN); CRIT-1 (within-lock),
CRIT-2 (cross-lock); the pass-2 REGISTRY; CROSS-LOCK (XL-A..H, binding); PORTFOLIO/CONSTITUTION.*

**Tree-of-record HEAD, spot-checked this session on `061c79fa7`** (descendant of base-pin
`ce51c36fc`; both probe families base-pin `ce51c36fc`): **`rg ApplyFn crates/ir/src` = 0**,
**`rg HostClosure crates/core crates/ir` = 0** — the value plane and the `FnDescriptor::HostClosure`
pre-seed are **WORKTREE-ONLY, UNBUILT of record**. `crates/ir/src/vm/bytecode.rs:184`
`compiled_regexes: Vec<Option<parse_that::regex::Dfa>>` + `:238 Dfa::compile` — **CONFIRMED**
(grounds the 4D gate-0 correction below).

---

## THE PASS-3 SHAPE-CHANGE — the architecture is SETTLED and PARTLY BUILT; the election lever FAILED and is EXOGENOUS

Pass 2 exported the terminal election and left RF-1 as the sharpest keystone: *does the runtime
combinator API reach comptime monomorphization, or only via the generator's codegen path?*

**Pass 3 resolves RF-1 — NO, a category error in Rust — analytically AND on running artifacts.** A
`SmallBox<dyn ParserFn>` (`parse.rs:74`) is type-erased at construction; a comptime type
(`Then<Str,Regex>`) is unrecoverable from it (research-A/B/D §1 concur). The only bridge from a
grammar *value* to monomorphized code is the **First Futamura Projection** — build-time codegen
over a **comptime-known** graph. The runtime combinator graph therefore has **exactly two**
lowerings: *interpret* (the VM — measured ~2.2× adverse even amortized) or
*serialize-then-offline-codegen*. Monomorphization belongs to the codegen front, never the runtime
API.

**The lock has a confirmed STAGING BOUNDARY — two fronts, not four rivals.**
- **RUNTIME front** (`str("a").then(b)` → a runtime `IrNode`): the ergonomic public API + the
  correctness/dynamic lane. Its only fast option is to be reified and handed offline to codegen;
  run at runtime it is the interpret terminal (debug/LSP/dynamic-grammar, slow-by-construction).
- **CODEGEN front** (comptime-known grammar/`IrNode` → emitted straight-line source): the **fast
  face on BOTH languages.** XL-D ratified this for TS (emitter, not decoder); PROTO-1 confirms it
  is also the Rust story.

**Four probes bear on this pass. What they decided:**

1. **PROTO-1 — the codegen fast face GENERALIZES to recursive JSON, measured vs the shipped
   incumbent (MIXED).** `grammar/json/json.bbnf` emitted by the *same* `backend/rust/emitter/`
   path. **GATE A PASS** — 0 SmallBox / 0 dyn ParserFn / 0 Rc/Arc / 0 fn-ptr fields; value recursion
   → direct byte-dispatched `fn` calls; ZST parser, `'a`-scoped. "JSON has left-recursion, regex
   does not" dissolves on a running artifact. **GATE B vs the tuned `json_parser()`** (identical
   parse_that core both arms, two windows, both orders, cold-paired divan): **data 0.65–0.73× WIN,
   twitter 0.75–0.95× WIN, citm ~1.0–1.14× TIE, canada 1.50–1.60× REGRESS.** The win is
   string/structure/small; the loss is **entirely the f64 numeric leaf** (canada is a wall of
   floats; the incumbent's eisel_lemire + SIMD `dispatch_byte_multi` outruns the emitted `-> f64`).
   *Accuracy note (CRIT-1): the json artifact pre-existed in-tree at base-pin; pass-3 MEASURED it —
   "built this pass" would overstate.*

2. **PROTO-1-numeric-leaf — BRIEF-1, the election lever — FAIL.** Ported the incumbent's SWAR
   8-digit mantissa fold + integer fast-path into the emitted number leaf, regen clean. **canada
   UNCHANGED at 1.56×** (gate ≤1.05×), both windows both orders; data/twitter/citm still
   PASS/WIN/TIE. **The lever did not fire:** canada's floats have short digit runs (1–3 int / 6–9
   frac), so the `*p+8<=len` guarded probe straddles the delimiter and always falls to the scalar
   tail; the integer fast-path never fires (all floats). **The residual — a 16-byte SIMD digit
   stripe + `get_unchecked` on a NUL-padded view — is gated on a padded-input `parse()` contract**
   that re-bases `len`/EOF across every leaf: a re-architecture, "the next probe, not this one."
   **This kills the "single lever between mixed and elect" framing**, and (CRIT-2, governing) that
   residual `number_simd` kernel is **LOCK-9's 9B NATIVE-REBIRTH plane, un-exported.** 4B cannot
   elect inside LOCK-4 even in principle — its ceiling is exogenous.

3. **PROTO-2 — GAP-1 discharged + the node-barrier census (PART 2 PASS / PART 1 QUALIFIED).** PART
   2: `FnDescriptor::HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` added to the **type/emitter**
   plane, 7 exhaustive `crates/ir` arms wired, `bbnf-ir` compiles clean; the emitted `lerp` region
   is a straight-line recognizer with the arbitrary closure fused as **1 named call-out**
   (`host_closure_15(inner)`), body opaque, `arg→ret` keeping the node an analyzable leaf. *(core
   emission arms `driver/map.rs` + `emit_host_closure` default — patch-plausible, NOT cold-built.)*
   PART 1: value.js math grammar = **4.839% opaque nodes** (6/124), site→node K=13.78, repo-wide
   **1.857% opaque nodes** (25.58% sites × 1/K) — clears ≤2%-of-nodes repo-wide by a 7.7% cushion,
   **FAILS locally** (math 4.84%). Analyzable-graph is a **repo aggregate, not a per-grammar
   invariant.**

4. **PROTO-2-vmvalue — BRIEF-2, the 4C value plane + live dual iso-oracle — BUILT-IN-WORKTREE,
   8/8 GREEN.** Four mechanical edits landed in a worktree: `Op::ApplyFn(FnId)` (30→31), `IrNode::Map`
   fn_id **emitted** not dropped, `#[serde(default)] fns: Vec<FnDescriptor>`, `Value` 4→8
   (F64/U32/Str/Variant), a ~150-LOC MapExpr evaluator + a ~200-LOC TS mirror. Oracle A byte-identical
   GREEN, Oracle B crossing GREEN (opcode-stream 0 divergences; 9 descriptors crossed node-for-node
   Rust↔TS from the same wire). **This discharges *mechanism buildability* — a real uplift over "0%
   built" — BUT the probe tested self-consistency, not the sharpest uncertainty (see the McKeeman
   correction).**

**Net.** No family converges; the terminal is un-electable inside LOCK-4. The shape-change is that
the architecture is **settled AND partly built** — a two-front core split by a staging boundary,
whose codegen fast face generalizes to recursive JSON, fuses through an arbitrary closure, and is
measured competitive-but-numeric-leaf-gated. What remains is **not four rival terminals** but
**five cross-lock DAG dependencies** (hand-off list below) plus **one still-UNBUILT-of-record
correctness backbone** (the 4C value plane — worktree-built, real gate untested).

---

## THE THREE CORRECTIONS THIS RE-SYNTHESIS OWES (folded from the critics; all load-bearing)

- **[CRIT-1, governing within-lock] The 15:22 draft is STALE against its own two follow-on probes.**
  BRIEF-1 FAILED (canada 1.56× unchanged); BRIEF-2 BUILT (worktree). The "single lever" and "0%
  built" framings are **both dead**. The 4C correction the draft *did* fold (value plane UNBUILT on
  record) is tree-accurate and stands.
- **[CRIT-1, the McKeeman trap] PROTO-2-vmvalue's REAL gate was NOT tested.** The 4C ask (G-C0)
  requires VM output **byte-identical to parse-that's LIVE closure `.map`**. Oracle A asserted
  identity against an **author-written golden** — the fabricated golden the differential-testing
  literature (research-C §4, priorart §7.11 / McKeeman 1998) forbids; Oracle B proves only Rust↔TS
  crossing consistency. *Does the VM compute the SAME values the shipped combinator does?* — never
  ran. Worse, the hardest case is a **pass-through stub** (`apply_fn` forwards `HostClosure`, body
  opaque), so byte-identity there is **un-testable by construction**; and canada / the full-corpus
  crossing was **UNRUN** (blocked on porting `parse_that::regex::Dfa` to TS).
- **[CRIT-2, governing cross-lock] The hand-off list is INCOMPLETE — three live gaps are
  double-homed as "internal next probes."** (1) The numeric-leaf ceiling is **LOCK-9's `number_simd`
  kernel** (9B) + a LOCK-6/8 padded-input `parse()` contract — mis-filed as "4B open-question 1";
  the G-B1 baseline **moves under LOCK-9** the moment 9B lands its `vshrn`/`vpaddq` rewrite. (2) The
  VM value plane is an **ISA + value-model change in `crates/ir/src/vm/`** — LOCK-3's tower-shape
  home per **XL-B** — routed straight to GESTALT-Ω, skipping the owning lock. (3) The
  `FnDescriptor::HostClosure` diff is an **unattributed pre-seed in LOCK-1's TypeDesc plane** (both
  G-B3 and G-D-GAP1 ride it; `rg HostClosure`=0 on record; constitution-v zero-runtime-cost
  untested for it).

---

## 4A — CLOSURE-TO-GRAPH · role: the RUNTIME-front reifier + pre-codegen pass host (NOT a terminal)

**Architecture.** Combinator constructors become **smart constructors returning graph nodes**:
`string("a")→Node::Literal`, `a.then(b)→Node::Seq`, `a.map(f)→Node::Map{fn_id}`. The graph is not
greenfield — `bbnf_ir::IrNode` (`types/node.rs:30`) is a live serde-serializable **14-variant
near-bijection** with a uniform visitor (`for_each_child`, `:145`), a hash-consed `GrammarDag`
(`dag/mod.rs:62`), and fusion/dispatch/LR passes (`passes/{transform,sets,lr}.rs`) already built
and **consuming** (not cloning) the e-graph. parse-that's own `span_parser/` (enum-dispatched,
vtable-free partial node graph) is the native seed.

**Mechanism / role after RF-1.** Construction is pure tree-building; no parse logic runs until a
lowering consumes the graph. 4A is the **runtime-front reifier and the home of the pre-codegen
passes — NOT a terminal.** Its graph feeds either the interpret terminal (VM) or, reified offline,
the codegen fast face (4B). PROTO-1 grounds the downstream (the codegen front emits a zero-box
recursive parser), so 4A's "erase the graph into AOT rather than run it" role is confirmed correct.

**Gates.**
- **G-A1 — the ONE novel 4A claim, 0% built for a FOURTH pass.** `fuse(Graph)→Graph` on a
  **combinator-constructed** `IrNode` (not `.bbnf`-text IR) byte-identical to a hand-fused arena
  (structural-equality oracle) AND JSON/CSS cold-paired ≥0.98×. "The passes already exist" is true
  for the grammar-TEXT front and **unbuilt for the combinator front** — the elegant-reduction trap,
  the family's whole risk.
- **G-A3 (node-barrier census) — BUILT, thin (PROTO-2 PART 1).** 1.857% repo-wide (QUALIFIED,
  K=13.78) / **4.84% math-only (FAIL)** — node-thin in aggregate, fragile locally, and it FAILS on
  value.js, LOCK-8's flagship consumer.

**Open questions.**
1. Does `fuse(Graph)→Graph` on a combinator-constructed graph match a hand-fused arena byte-for-byte?
   (Zero running evidence, four passes.)
2. **[LOCK-3 obligation, XL-B — not self-dischargeable]** reify→serialize→**same-emitter** parity
   (open-Q2) is a LOCK-3 tower-parity build, not a within-4A probe. PROTO-1 proved the
   grammar-TEXT→emitter path generalizes; the runtime-graph→serialize→SAME-emitter→graph-of-record
   parity path never ran, and its home is LOCK-3's.
3. **[folded]** the pass-2 runtime-switch refutation (−22%) also refutes a 4A-TS `switch(node.tag)`
   interpreter — 4A-TS lowers through the 4B-TS emitter (XL-D). 4A is a source front on both
   languages, never a runtime terminal.

**BLOCKED-candidate?** **NO.** The missing work is pointing extant passes at a combinator front — a
build task, not a primitive of equal difficulty. Terminal role refuted (RF-1); source-front role
coherent.

---

## 4B — COMPTIME-FUSION · role: the CODEGEN fast face (Rust monomorphization + TS emitter) — strongest family, election ceiling EXOGENOUS

**Architecture.** Fusion is the First Futamura Projection: partially-evaluate the combinator
interpreter against a comptime-known grammar; a straight-line parser falls out — the vtable, the
per-construction box, and the setup graph deleted by construction. PROTO-1 proves it **ships and
generalizes**: the regex `generated.rs` shape extends to recursive, alt-dispatched, value-bearing
JSON with 0 boxes on the hot path. The three in-tree substrates are the three arms of the Q4 cost
model: closure core = {box-here}; `span_parser/` enum tier = {enum-node} at runtime;
`generated.rs`/`json.rs` = {inline-monomorphize + box-at-back-edge} by codegen.

**Mechanism.** ONE serializable plan — the comptime `IrNode` with SCC/arity/projection annotations
— drives (i) a Rust source emitter and (ii) a TS source emitter. **Different mechanisms** (Rust
monomorphization has no V8 analog; TS emission has no Rust analog), **no shared isomorphic
terminal** (INCOMPAT-2) — the shared object is the *plan*, not the artifact; the observable is a
differential oracle, not a shared binary.

**Seams.** `parse_that/src/regex/generated.rs` + `crates/core/src/grammar/generated/json.rs` (the
two existence proofs); the emitter `backend/{rust,ts}/emitter/`; cost-model homes `ir/src/cost.rs`,
`egraph/cost.rs`, `passes/csp_strategy/`; firing-number anchors `try_factor_alt` **fires at
`branches.len() >= 4`** (`fuse_token/factor.rs:23`), `fuse_single_use` gated on Seq≥2, `compile_map`
folds `Map(Map(x))`.

**Gates.**
- **G-B0 (generalize to recursive JSON) — PASS (PROTO-1 GATE A).** 0 SmallBox/dyn/fn-ptr, ZST
  parser. *(alloc sub-claim construction<6 / per-parse=1 is source-census, not dhat-isolated.)*
- **G-B1 (production dilution vs the SHIPPED `json_parser()`) — MEASURED, MIXED.** data/twitter
  WIN, citm TIE, **canada 1.50–1.60× REGRESS.** "Beyond-SOTA" is met on string/structure/small
  **only**; citm is a tie, canada a loss.
- **G-B3 (general emitter through GAP-1) — PASS (PROTO-2 PART 2).** Arbitrary host closure fused as
  1 named call-out, `bbnf-ir` compile-proven. *(rides an unattributed LOCK-1-TypeDesc pre-seed;
  core emission arms patch-plausible, not cold-built.)*
- **G-B4 (the gate-that-cannot-fail) — NO NUMBER, THIRD pass.** The firing budget (fuse-arity 8,
  `try_factor_alt` at 4, one box per SCC back-edge, `.rlib`/`dist` + incremental-compile) has
  anchors but no measured number, owed on CSS **and** value.js.

**Open questions.**
1. **[election ceiling — EXOGENOUS, corrected from "single lever"]** canada 1.56× is unchanged
   after the numeric probe; the fix (`number_simd` 16-byte SIMD stripe + `PaddedView` +
   `get_unchecked`) is **LOCK-9's 9B plane** + a **LOCK-6/8 padded-input `parse()` contract** — not
   a LOCK-4 probe. 4B is workload-conditional; its election is **three-lock-away**, and the G-B1
   baseline is cross-lock-unstable (LOCK-9 is rewriting the very `number_simd` the incumbent uses).
2. What is G-B4's *number*? (Anchors exist; a measured budget does not — three passes owed.)
3. **[extant-tranche, owed]** XL-E settles the *principle* (4B's compile-time win ≠ PROVE-09's
   runtime refutation); the *placement* in a PT-tranche letter is an open constitution-ix debt →
   GESTALT-P.

**BLOCKED-candidate?** **NO** — the strongest family. The gap is exogenous (LOCK-9 kernel +
padded-input re-arch) + a firing budget; none is a primitive of equal difficulty. But do not carry
"one probe from election."

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · role: the interpret terminal + the cross-language iso-of-record oracle · value plane BUILT-in-worktree, real gate UNTESTED

**Architecture.** A working grammar VM ships — `crates/ir/src/vm/` (**1772 LOC** on record): a
**30-variant** `Op` ISA (LPeg-shaped), an `IrNode`→bytecode compiler, a stack-machine interpreter,
a live DAP consumer (`crates/lsp/src/dap/adapter.rs`) + JSON VM bench. Both `Op` and
`BytecodeProgram` derive Serialize — the program **crosses as data**; isomorphism is a property of
the *representation*, not a mirrored-source linter (the only place in LOCK-4 where constitution
clause iii is satisfiable by construction). Measured-refuted twice as a *fast* terminal (switch-VM
−22%; interpret 0.021–0.46×) — it survives as (a) the interpret terminal (DAP/LSP/dynamic-grammar)
and (b) the cross-language iso-of-record oracle.

**Mechanism — value plane BUILT-in-worktree, NOT on record, and the REAL gate untested (the
correction).** On HEAD the VM is value-STRUCTURAL: `Map` compiled to `compile_node(inner)` with the
`fn_id` **dropped** (`compiler/node.rs:31`); `Value` = 4 shapes; **no `Op::ApplyFn`.**
PROTO-2-vmvalue built the completion in a worktree (Op 30→31, Value 4→8, `fns` side table, fn_id
emitted, evaluator + TS mirror, 8/8 GREEN) — this is **buildability + self-consistency**, and the
descriptor already crosses as data, so it is a build task, not a primitive of equal difficulty. But
three qualifications sit on exactly what drove the rise:
- **(a)** G-C0's REAL gate (VM `ApplyFn` vs parse-that's **live** `.map`) was NOT tested — Oracle A
  used an author golden; semantic correctness of record is **open**.
- **(b)** the HostClosure value case is a **pass-through stub** — the VM evaluates NO arbitrary
  closure; the closed descriptors (Number/Hex/Enum/MapExpr) compute, the open one forwards; "value
  plane complete" overstates.
- **(c)** the build is **worktree-only** (`rg ApplyFn`=0 on record); G-C1 canada / full-corpus
  crossing **UNRUN**, DAG-blocked on porting `parse_that::regex::Dfa` (`bytecode.rs:184/238`,
  tree-confirmed) to TS.

**Value-plane home (the double-home to split, corrected to route LOCK-3-FIRST).** Internal
specializations (`EnumWrap`/`BoxWrap`/`SpanCapture`/`NumberConvert`/`HexConvert`) are
compiler-produced, regex-fused, pure functions of the recognizer → **LOCK-3 IR-VM lowering**. Only
user-facing `MapExpr::FnCall` + future `HostClosure` touch **LOCK-7 host dispatch**. `Op::ApplyFn`
+ the `fns` side table are an **ISA + value-model change** in `crates/ir/src/vm/` — LOCK-3's
tower-shape home per **XL-B**; it routes to **LOCK-3 FIRST**, GESTALT-Ω arbitrating only the
internal-descriptor (→LOCK-3) vs host-closure (→LOCK-7) split. (Corrected from "GESTALT-Ω whole".)

**Gates.**
- **G-C0 (the value-plane completion) — the REAL gate, UNTESTED.** VM value-projected output
  **byte-identical to the closure core's `.map`** on a projection-dense grammar (differential
  oracle vs parse-that's live closure parse — an author golden does NOT discharge it) + **at least
  one non-stub HostClosure evaluation**. This is the family's central untested-of-record claim.
- **G-C1 (RF-2, the crossing) — UNRUN over the corpus.** A *live* Rust `to_vec_named` serialize →
  TS ~200-LOC switch decode → dual oracle (opcode-stream identity vs the LIVE golden AND `Value`-tree
  identity) over the six R3 corpora incl. **canada** (mandatory adversarial member) + one
  projection-dense grammar. Simultaneously the 4D-TS correctness graft and the behavioral-iso (6D)
  evidence LOCK-6 needs.
- **G-C4 — UNEXERCISED on its real bus.** Wire-contract regen-idempotence (encode∘decode∘encode =
  encode, `Op` variant tags pinned) fired on `serde_json`, **not** `rmp_serde::to_vec_named`; the
  born-RED MessagePack externally-tagged-reorder guard never fired on the bus it ships on.

**Open questions.**
1. Does the value plane compute byte-identity with the closure core `.map` (the REAL gate), and can
   a non-stub HostClosure be evaluated? (worktree-built ≠ correct of record; hardest case stubbed.)
2. Does the program cross byte-identically over six corpora incl. canada against a **live** golden,
   with `rmp_serde` idempotence? (Oracle B crossed 9 descriptors self-consistently — not this.)
3. The value plane has **no production-execution consumer** by construction (codegen is the fast
   face) — "built" ≠ "on the hot path" (note for GESTALT-Ω).

**BLOCKED-candidate?** **NO** — BANKED as the interpret/oracle lane. G-C0/G-C1 are cheap (edits +
a decoder + a differential test); no missing primitive.

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · role: the ownership decision · BLOCKED (split) · GAP-1 type-plane DISCHARGED on an unattributed pre-seed

**Architecture.** Combinators are a construction API over the *same* `bbnf_ir::IrNode` the grammar
plane uses; parse-that keeps its public API and **inherits the generator's optimizer**. The
distinction from 4A: 4A stops at "analyzable graph"; 4D fixes that graph to *be* `bbnf_ir::IrNode`
so the inheritance is literal.

**Mechanism.** The reification seam is 4A's; 4D adds the *ownership* commitment plus the value-plane
completion (shared with 4C's G-C0 — built once for both by PROTO-2-vmvalue). research-D's
split-the-barrier finding is a genuine sharpening: **44 of 84 (~52%) of latex-paper's non-projection
sites are object-literal/array/ternary constructions recoverable as analyzable data** via
`MapExpr::Array` + `MapExpr::Select` + `FnDescriptor::Record` — still serializable, still
`constant_fold`-total — leaving `Opaque(HostFnId)` for the true closures only (value.js's **exactly
3 easing closures**, `easing.ts:124/142/153`, which *return* a host function — beyond any
serializable descriptor). Do not smuggle one `Box<dyn Fn>` per `->`; add the three data nodes
first, let `Opaque` carry only the residue.

**Gates.**
- **Gate-0 — DONE for the regex arm, LOCK-5-gated for the VM arm (corrected).** The
  `parse_that ↔ bbnf_ir` Cargo cycle's regex arm is severed by a 4-line re-export swap
  (`parse_that::regex` is `pub use bbnf_regex::*`) — mechanical. **BUT the VM's `Dfa` arm remains a
  LOCK-5 extraction:** `bytecode.rs:184/238` deps `parse_that::regex::Dfa` (tree-confirmed). The
  15:22 draft's "not LOCK-5-gated" is too strong; the honest disposition follows research-D — gate-0
  is **partly** LOCK-5-gated.
- **G-D-GAP1 — DISCHARGED compile-proven (PROTO-2 PART 2), on an unattributed pre-seed.**
  `FnDescriptor::HostClosure` compiles into the IR type-plane (7 arms); the emitter fuses an
  arbitrary closure as 1 named call-out. But `rg HostClosure`=0 on record; the diff is a LOCK-1
  TypeDesc-plane pre-seed neither probe owns, and constitution-v is untested for it.
- **G-D-rescue — DEMOTED.** Pooling `Interpreter::new` (7–8 Vecs + FxHashMap) helps **only the
  interpret oracle** — the fast face is born with the `BumpSlab`, no constructed graph to pool.
  Decision-value is oracle viability, not a competitive terminal.

**The block (confirmed, three horns).**
- **Horn (i):** a full `GrammarIR`+interpreter+optimizer port to TS = a primitive of
  **equal-or-greater difficulty than the original LOCK-4 problem** — BLOCKED-candidate by the rule's
  text. *But XL-D retires the NEED* (fast TS face = per-language emission, not a shared runtime).
- **Horn (ii):** the wasm crossing — PROVE-14 measured full-walk value materialization at **0.91×,
  a LOSS**, on exactly the value-producing-combinator workload.
- **Horn (iii):** the shared-decoder graft is **−22% on V8**; its fast form needs the build-time
  emitter (the same per-language primitive). **"one VM, two sides, for free" is refuted for the fast
  path** — correct + isomorphic as data, slow to run on V8.

**Open questions (predecessor-owned, report-only per Q8).**
1. **[INCOMPAT-1 → LOCK-5]** is `bbnf_ir` the genuinely shared HIR (**5A**, "inherit for free") or a
   lawful functor across separate HIRs (**5B**, which evaporates the inheritance)? gate-0-compiles is
   mechanical; shared-substrate *semantics* is DAG-blocking, and the VM's `Dfa` arm is a LOCK-5
   extraction.
2. **[→ LOCK-7]** the typed shape + per-subsystem floor of `HostClosure`/`Opaque(HostFnId)` (the
   3-site value.js residue; the tier-1 `Record`/`Array`/`Select` boundary). The pre-seeded
   type-plane diff's home (LOCK-1 TypeDesc) is also un-adjudicated.
3. node-ratio thin AND FAILS on LOCK-8's consumer (1.857% repo / 4.84% math) — fusion-inheritance
   safe in aggregate, fragile on value.js.

**BLOCKED-candidate?** **YES, split.** 4D-as-fast-shared-runtime is BLOCKED-candidate (dead by the
missing-primitive rule AND by XL-D); 4D-as-shared-ownership is DAG-BLOCKED on LOCK-5 (5A vs 5B) +
LOCK-7 (lattice). Do NOT carry "4D dominates on parsimony ⇒ elect" — dominance is conditional on 5A
and its Rust runtime is a measured loss.

---

## THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The staging-boundary finding **collapses most apparent rivalry into composition** and leaves
**two live forks** (both exported) + **one retired**.

**Compositions, NOT incompatibilities (correcting the pass-2 "four rivals" framing):**
- **4A ⊕ 4B compose across the staging boundary** (reify → fuse → serialize → emit). RF-1 makes
  them *sequential*, coherent only for the codegen front, never the live combinator API as a runtime
  terminal. PROTO-1 grounds 4B's half.
- **4C ⊕ 4D share the identical value-plane completion** (`Op::ApplyFn` + `fns` + `Value`-scalar,
  worktree-built once for both) and the identical crossing mechanism; 4C is the iso-oracle, 4D the
  ownership commitment. Not rivals.
- **4B-TS emitter ⊕ {4A,4C,4D}-TS** — every TS terminal lowers through the build-time emitter
  (XL-D: decoder −22%, emitter ×1.23). Runtime-decoder-as-fast-TS-face is BANKED-by-measurement
  across all three.

**INCOMPAT-1 — Q-OWNERSHIP → LOCK-5.** parse-that-private isomorph (4A/4B/4C) vs shared `bbnf_ir`
(4D) cannot both be the terminal representation. gate-0 is *partly* severed (regex arm mechanical;
VM `Dfa` arm LOCK-5-gated); the *semantic* question (5A shared type vs 5B lawful functor) is
LOCK-5's. **Irreducible inside LOCK-4.**

**INCOMPAT-2 — THE FAST TERMINAL, cross-language → LOCK-6 (+ evidence to LOCK-8).** Rust
monomorphization ≠ TS emission — different mechanisms, no shared isomorphic terminal. Resolved in
the emitter's favour on TS (measured); the fast Rust face is *also* codegen with a workload-dependent
win. **Export the EVIDENCE and let LOCK-6 rule** — the 15:22 draft's "away from 6A/6B" is a **soft
cross-lock overreach** (declaring two of LOCK-6's families dead is LOCK-6's election, not LOCK-4's;
CRIT-2). Route PROTO-1's emitter/V8-discipline result to LOCK-8 so it is not re-derived.

**INCOMPAT-3 — Q-RUNTIME: RETIRED as an abstract fork.** codegen fast face (4B) vs interpret
terminal (4C-VM ≈ 4A-graph-walk, the same measured-adverse arm) is NOT incompatible — two terminals
for two use-cases, cost-model-selected. The decision-relevant comparison is the generalized codegen
route vs the SHIPPED closure core, which PROTO-1 MEASURED (MIXED, numeric-leaf-gated); the concrete
open number (canada parity) is exogenous (LOCK-9's).

---

## THE CORRECTED HAND-OFF LIST (FIVE items, not four — CRIT-2's governing correction)

1. **Q-ownership → LOCK-5** (5A shared type vs 5B lawful functor; INCOMPAT-1). Note: gate-0's VM
   `Dfa` arm is a LOCK-5 extraction (corrected from "not LOCK-5-gated").
2. **The fast-terminal cross-language iso → LOCK-6** (Rust monomorphization ≠ TS emission ⇒ 6C/6D;
   INCOMPAT-2). Export the EVIDENCE, do not pre-declare 6A/6B dead. Route PROTO-1's
   emitter/V8-discipline result to **LOCK-8**.
3. **The value-plane ISA home → LOCK-3 (XL-B) FIRST, then GESTALT-Ω.** `Op::ApplyFn` + `Value`
   scalar arms are a tower-shape change LOCK-3 owns pass-3+; GESTALT-Ω arbitrates only the
   internal-descriptor (→LOCK-3) vs host-closure (→LOCK-7) split. (Corrected from "GESTALT-Ω whole.")
4. **4B's perf-claim supersession → PT-tranche (GESTALT-P).** XL-E settles the principle; the
   *placement* is an open constitution-ix debt.
5. **[the double-home the 15:22 draft mis-filed as internal] The 4B numeric-leaf ceiling → LOCK-9**
   (the `number_simd` kernel, 9B, actively rebirthing) **+ LOCK-6/8** (the padded-input `parse()`
   contract). 4B cannot elect inside LOCK-4 even in principle.

**Plus two provenance/ownership items owed to the gestalts:**
- **The `FnDescriptor::HostClosure` type-plane pre-seed is unattributed and homed in LOCK-1's
  TypeDesc plane.** Two gate PASSes (G-B3, G-D-GAP1) ride a diff neither probe owns (`rg
  HostClosure`=0). Attribute it, home it (LOCK-1), verify constitution v.
- **The G-B1 baseline is cross-lock unstable** — LOCK-9 9B is moving the incumbent's `number_simd`;
  the MIXED verdict must be re-based when 9B lands (base-law-adjacent, beyond the snapshot
  discipline).

---

## WHAT IS SETTLED / BUILT vs UN-ELECTABLE INSIDE LOCK-4

**Settled/built:** the **architecture** — a two-front core split by a staging boundary; the codegen
fast face **generalizes to recursive JSON (PROTO-1 GATE A) and fuses through an arbitrary host
closure (PROTO-2 PART 2)**, measured competitive-but-numeric-leaf-gated against the shipped
incumbent; the opaque-node barrier is **BUILT** (1.857% repo / 4.84% math — aggregate, not
invariant); the 4C value plane is **buildable + self-consistent in a worktree** (mechanism
discharged).

**Un-electable / un-built-of-record:** the terminal (five cross-lock DAG dependencies above); and
the 4C value plane's **correctness of record** — the REAL gate (VM vs live `.map`) untested, the
hardest case stubbed, the canada crossing unrun, the build worktree-only. Election is a gestalt act;
the two briefs below are the sharpest levers toward the next pass.

---

## PROTOTYPE BRIEFS FOR THE NEXT PASS (the two sharpest uncertainties)

The two briefs are returned in the structured output. In précis:

- **BRIEF A — 4A's `fuse(Graph)→Graph` byte-identity (G-A1; bbnf-worktree).** The one novel 4A
  differentiator, SPEC-ONLY for FOUR passes, on a *combinator-constructed* graph. Build it once:
  does the extant fuse pipeline, run over an `IrNode` built by a combinator-style builder (not
  grammar text), produce a graph structurally byte-identical to a hand-fused arena, and parse
  JSON/CSS cold-paired ≥0.98×?

- **BRIEF B — 4C's G-C0 REAL gate + one non-stub HostClosure (bbnf-worktree).** The McKeeman trap
  the worktree probe fell into: land the value plane on record and prove VM `ApplyFn` output
  **byte-identical to parse-that's LIVE closure `.map`** (not an author golden) on a projection-dense
  grammar, with **at least one arbitrary HostClosure actually evaluated** (not a pass-through stub).
