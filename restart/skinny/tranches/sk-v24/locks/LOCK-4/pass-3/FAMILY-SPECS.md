# LOCK-4 — THE COMBINATOR CORE · PASS-3 FAMILY SPECS

*Synthesis agent, pass 3 (Fable-tier judgment). One cogent spec per family while routes remain
incompatible, then the incompatibility map and the BLOCKED-candidate determinations. Inputs folded:
pass-3 research-{A,B,C,D,priorart}, the pass-2 REGISTRY (status floors, keystone set, CROWDING),
CROSS-LOCK (XL-A..H), PORTFOLIO/CONSTITUTION. Prior measured events carried as facts: PROTO-1
(TS face — emitter ×1.23 / decoder −22%), PROTO-2 (gate-0 PASS + value-plane floor 0.021–0.46×,
oracle GREEN). No new build ran this pass; the pass-3 movement is an **analytic resolution + the
first real census**, not a prototype.*

---

## THE PASS-3 SHAPE-CHANGE (the finding that reorganizes the lock)

Pass 2 exported the terminal election and left seven keystones, the sharpest being **RF-1** — "does
the *runtime* combinator API reach comptime monomorphization, or only via the generator's codegen
path?" — flagged as the tower's *incoherent middle*, SPEC-ONLY.

**Pass-3 resolves RF-1, unanimously and analytically. The answer is NO, and it is a category error
in Rust.** All four family researchers plus prior art converge on the identical mechanism trace with
zero dissent:

- A `SmallBox<dyn ParserFn>` (`parse.rs:74`) is **type-erased at construction** — the concrete
  closure types are already collapsed to one vtable pointer before the graph exists; a comptime type
  (`Then<Str,Regex>`) is unrecoverable from it (research-B §1, research-A Q1, research-D §1).
- The only bridge from a grammar *value* to monomorphized code the literature knows is the **First
  Futamura Projection**: build-time codegen over a **comptime-known** graph (grammar-text/`IrNode` →
  emitted source). Staging, not a free lowering (research-priorart §1; Jonnalagedda OOPSLA'14,
  Moleike, flap PLDI'23).
- Therefore the runtime combinator graph has **exactly two** lowerings: *interpret* (the VM —
  measured ~2.2× adverse even amortized, PROTO-2) or *serialize-then-offline-codegen*.
  Monomorphization belongs to the codegen front, never the runtime API.

**Consequence — the lock has a confirmed STAGING BOUNDARY in its middle, and it is load-bearing.**
The pass-2 "tower whose spine is `bbnf_ir::IrNode`" is really **two fronts separated by staging**:

- **The RUNTIME front** (`str("a").then(b)` → a runtime `IrNode`): ergonomic public API + correctness
  oracle. Its only fast option is *to be reified and handed offline to codegen*; run at runtime it is
  the interpret terminal (slow-by-construction, the debug/LSP/dynamic-grammar lane).
- **The CODEGEN front** (comptime-known grammar/`IrNode` → emitted straight-line source): the **fast
  face, on BOTH languages.** XL-D already settled this for TS (emitter, not decoder); pass-3 confirms
  it is *also the Rust story* — and, decisively, **it already exists in-tree for regex.**

**The second pass-3 finding — the codegen fast face is not hypothetical; it ships.**
`rust/parse_that/src/regex/generated.rs` (research-B §0) is `@generated` from `bootstrap/regex.bbnf`
by the bbnf generator and is a straight-line recursive-descent parser with **zero `SmallBox<dyn
ParserFn>` on its hot path**: a ZST `RegexParser` (`:7`), free `fn` back-edges cutting recursion
(`__quantifier`/`__alternation`/`__concat`/`__regex`, `:215/390/434/2276`), one value enum
`RegexParserEnum<'a>` in a parse-scoped `BumpSlab` arena constructed once at entry
(`__RegexParserEnum_alloc`, `:205`), **`'a`-scoped by construction** — no retrofit. This is 4B's
Futamura projection, *realized*, for one grammar. **4B's "general emitter" gap is now a
generalization (regex → arbitrary bbnf grammar), not an invention** — and it sidesteps PROVE-09's W4
arena refutation entirely: W4 failed retrofitting a `'a`-arena onto the *closure* core (the unsound
`'static`-holds-`'a` trap); the generated route is **born with the arena**.

**The third pass-3 finding — GAP-1 is censused for the first time (research-D §2).** Prior probes
dodged the arbitrary-host-closure wall; research-D read the live consumer sources:

| consumer | map/chain sites | MapExpr-reifiable | multi-stmt `=>{}` (arbitrary) | note |
|---|---:|---:|---:|---|
| **latex-paper** | 86 | 86 (100%) | 0 | fully reifiable |
| **value.js** | 129 | 64 + 19 FnCall | **33 (~26%)** | the Pratt binary-expr folds |

**The reframe that matters: the ≤2% floor is a NODE ratio, not a SITE ratio.** Each opaque `Map{fn_id}`
is a *thin leaf over a fat transparent recognizer subtree*; value.js's 33 opaque leaves over a
recognizer graph of hundreds-to-thousands of `IrNode`s sit **well under 2% at the node level**, and
their **location** is the value-plane *root* of expression grammars — so **every recognizer-plane
analysis (fusion, `AltDispatch`/`TokenDispatch`, prefilter, span — every SPEC §0 perf win) is
entirely unblocked**; only value-plane *type* inference stops at value.js's ~5 Pratt folds. This is a
real advance on Q-A2/GAP-1 — **but it is a SITE census from source-reading; the <2% node ratio is
asserted, not built.** GAP-1 is *characterized*, not *discharged*: no family has built the
`HostClosure` descriptor or emitted a fused region *through* one arbitrary closure.

**Net pass-3 disposition.** No family converges; the terminal remains un-electable inside LOCK-4
(the Q-ownership fork is LOCK-5's, the iso-verdict LOCK-6's). But the lock's *architecture* is now
settled: **a two-front core split by a staging boundary, whose fast face on both languages is the
build-time emitter, whose emitter already exists for regex, and whose one un-surveyed hazard is the
value plane through an arbitrary host closure.** The families are no longer four rival terminals —
they are the named parts of this one architecture, plus one predecessor-blocked ownership question.

---

## 4A — CLOSURE-TO-GRAPH · role: the RUNTIME-front reifier + pre-codegen pass host

**Architecture.** Combinator constructors become **smart constructors returning graph nodes**, not
opaque closures: `string("a")→Node::Literal`, `a.then(b)→Node::Seq`, `a.map(f)→Node::Map{fn_id}`.
The graph is not greenfield — it is `bbnf_ir::IrNode` (`crates/ir/src/types/node.rs:30`), a live
serde-serializable **14-variant near-bijection** of the combinator set, with the visitor
(`for_each_child`, `:145`), the hash-consed canonical form (`GrammarDag`, `dag/mod.rs:62`), and the
fusion/dispatch/LR passes (`passes/transform.rs`, `passes/sets.rs`, `passes/lr.rs`) already built and
**consuming** (not cloning) the e-graph. 4A's job is to make parse-that *construct* `IrNode` and prove
the analyses ride cheaply — parse-that's own `span_parser/` (an enum-dispatched, vtable-free partial
node graph) is the native seed.

**Mechanism.** Construction is pure tree-building; no parse logic runs until a lowering consumes the
graph. Pass-3 fixes 4A's *role* precisely: after RF-1, 4A is **the runtime-front reifier and the home
of the pre-codegen passes** — NOT a terminal. Its graph feeds either the interpret terminal (VM) or,
reified and handed offline, the codegen fast face (4B). Fusion, dispatch-table derivation, and packrat
placement are graph passes that run *before* the staging boundary, on the reified graph, whichever
terminal follows.

**Integration seams.** `node.rs:30/145` (graph+visitor); `dag/mod.rs:62` (hash-consing);
`passes/{transform,sets,lr}.rs` (the passes); parse-that closure sites replaced: `parse.rs:61,74`
(`SmallBox`), `lazy.rs:21` (`Box<dyn LazyParserFn>`); native seed `parse_that/src/span_parser/`.

**Gates it would carry.**
- **G-A1 (owed FOUR passes, still 0% built):** `fuse(Graph)→Graph` on a **combinator-constructed**
  `IrNode` (not `.bbnf`-text IR) is byte-identical to a hand-fused arena (structural-equality oracle)
  AND JSON/CSS cold-paired ≥0.98×. The passes exist for the *generator's* IrNode; pointing them at a
  combinator front and proving byte-identity is the unbuilt claim.
- **G-A3 (Q-A2):** the `Opaque(HostFnId)` barrier surface, counted at the **node** level on
  value.js/latex-paper, ≤2% of nodes (research-D asserts it; unbuilt).

**Exact open questions.**
1. Does `fuse(Graph)→Graph` on a combinator-constructed graph actually match a hand-fused arena
   byte-for-byte? (The one novel 4A claim, zero running evidence in four passes.)
2. Is the asserted <2% **node** ratio real once the graph is built? (Site census done; node census not.)
3. **[RF-1 RESOLVED, consequence for 4A]** 4A-as-terminal is refuted (no runtime→comptime); 4A-as-
   source-front-for-codegen is coherent *only if* the reified runtime graph can be serialized and
   fed to the SAME emitter as the grammar-text path, producing graph-of-record parity. That
   serialize→offline-codegen equivalence is unbuilt.
4. **[cross-family, folded]** PROTO-1's runtime-switch refutation (−22%) also refutes a 4A-TS
   `switch(node.tag)` graph interpreter — so 4A-TS must *also* lower through the 4B-TS emitter. 4A is
   a source front on both languages, never a runtime terminal.

---

## 4B — COMPTIME-FUSION · role: the CODEGEN fast face (Rust monomorphization + TS emitter) · the pass's strongest family

**Architecture.** Fusion is the **First Futamura Projection**: partially-evaluate the combinator
interpreter against a comptime-known grammar and a straight-line parser falls out — the vtable, the
per-construction box, and the setup graph *deleted by construction*, not redesigned around. **This is
not a redesign of the box; it is the box's deletion — and it ships today for regex** (`generated.rs`).
The three in-tree substrates ARE the three arms of the Q4 cost model: closure core = {box-here};
`span_parser/` enum tier = {enum-node} materialized at runtime; `generated.rs` = {inline-monomorphize
+ box-at-back-edge} materialized by codegen. 4B promotes the generated route from regex to any bbnf
grammar as the fast face on both languages.

**Mechanism.** ONE serializable plan — the comptime `IrNode` with SCC/arity/projection annotations —
drives (i) a Rust source emitter (`fn __rule(state)->Option<Enum>` + `BumpSlab`, the `generated.rs`
shape) and (ii) a TS source emitter (`function parseRule(state){…}`, direct calls, the megamorphic
`.parser` IC gone). The two are **different mechanisms** (Rust monomorphization has no V8 analog; TS
emission has no Rust analog) but **one plan** — this is the reconciliation of Q1 and Q5: the shared
object is the *plan*, not the artifact; the observable is a differential oracle, not a shared binary.

**Integration seams.** `parse_that/src/regex/generated.rs` (the existence proof); the bbnf emitter
`crates/core/src/backend/{rust,ts}/emitter/`; the cost-model homes `crates/ir/src/cost.rs`,
`egraph/cost.rs`, `passes/csp_strategy/mod.rs`; the concrete firing-number anchors (research-B §11):
`fuse_single_use` gated by `is_composite_seq`(Seq≥2)/`body_has_map`/`is_consumer_pinned`
(`passes/transform/fuse.rs:44/201/220/249`), token-dispatch `try_factor_alt` **fires at
`branches.len() >= 4`** (`fuse_token/factor.rs:23` — a concrete in-tree dispatch-fanout number),
map fusion `compile_map` folds `Map(Map(x))` (`driver/map.rs:16`).

**Gates it would carry.**
- **G-B0 (the generalization, THE deliverable):** emit a small bbnf grammar (JSON / 3-rule toy) as
  straight-line `fn`s + one value enum + `BumpSlab`, the `generated.rs` shape. Gate: **zero
  `SmallBox`/`dyn ParserFn` on the hot path**; construction allocs < 6 (PROVE-09 baseline); per-parse
  recognizer allocs = one `BumpSlab` reserve; ~400 parse-that tests as parity oracle; cold-paired divan
  vs the closure core (honest no-regression, SPEC §0/09).
- **G-B1 (production dilution, MANDATORY — converts mechanism→deliverable):** fused emit vs the
  **shipped** `json_parser()` with `dispatch_byte_multi` + SIMD on *both* arms; report the real shrink
  (the 16–27% is an upper bound on a *stripped* grammar).
- **G-B3 (general emitter through GAP-1):** the emitter over a region containing an **arbitrary host
  closure**, not a MapExpr projection; grep every emitted region for `.parser(`/vtable (must be 0);
  isolate dispatch- from allocation-elimination (PROVE-11 bounds dispatch ~+5%).
- **G-B4 (the gate-that-cannot-fail, still unset across THREE passes):** a firing budget — named
  fuse-arity (TS `fuseAll` flips unroll→loop at 3; Rust `seq!/alt!` unroll to 8), dispatch-fanout
  (grounded: `try_factor_alt` at 4), boxing-depth (topological: exactly one box per SCC back-edge),
  and a `.rlib`/`dist` + incremental-compile budget measured on CSS **and** value.js.

**Exact open questions.**
1. Does the generated route generalize past regex to a **recursive, alt-dispatched, value-bearing**
   grammar with the same zero-box / born-arena properties? (Regex has no left-recursion; JSON does.)
2. Does it beat the *shipped, tuned* `json_parser()` — not the stripped grammar? (Beyond-SOTA vs the
   incumbent is unestablished.)
3. Does the emitter survive an arbitrary host closure (GAP-1) — the value-plane hazard both prior
   probes dodged? (research-D characterized the surface; nobody emitted through it.)
4. What is G-B4's *number*? (Anchors exist; a measured budget does not.)
5. **[extant-tranche, owed]** 4B's confirmed Rust perf claim (indirect-call axis) vs PT-2's PROVE-09
   zero-claim posture (SIMD/arena axis) — reconcile as a NAMED supersession (XL-E covers the
   principle; no PT-tranche home named).

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · role: the interpret terminal + the cross-language iso-of-record oracle

**Architecture.** A working grammar VM ships — `crates/ir/src/vm/` (~2090 LOC): a 33-variant `Op` ISA
(`bytecode.rs:45`, LPeg-shaped), an `IrNode`→bytecode compiler, a stack-machine interpreter. Both `Op`
and `BytecodeProgram` derive Serialize — so the program **crosses as data**; isomorphism is a property
of the *representation*, not a mirrored-source linter. Pass-3 fixes 4C's role: after PROTO-1/PROTO-2
(switch-VM −22%; interpret 0.021–0.46×), the VM is **measured-refuted twice as a fast terminal** and
survives as (a) the interpret terminal (the DAP/LSP/dynamic-grammar lane — a live consumer,
`crates/lsp/src/dap/adapter.rs:150`) and (b) the **cross-language iso-of-record oracle**.

**Mechanism.** Value completion is built and byte-identical (`Op::ApplyFn(FnId)` at the Map-drop site
`compiler/node.rs:31`, `fns: Vec<FnDescriptor>` side table, `Value` scalar arms, `apply_fn.rs`
descriptor evaluator; PROTO-2 oracle GREEN). The plan carries regex *patterns* (StringIds); each side
compiles its own matcher (`compiled_regexes` `#[serde(skip)]` + `prepare_regexes`, `bytecode.rs:220`)
— the clean cross-language seam. The internal-descriptor evaluator (Hex/Number/Enum) is **LOCK-3 IR-VM
lowering** (compiler-produced specializations, `fn_descriptor.rs:14-16`), not LOCK-7 host dispatch —
only the user-facing `MapExpr::FnCall{name}` + future `HostClosure` touch LOCK-7. This is the pass-3
disposition for the value-plane double-home GESTALT-Ω must ratify.

**Gates it would carry.**
- **G-C1 (RF-2, the surviving axis, UNBUILT-of-record — now the cheapest high-value experiment in the
  lock):** a *live* Rust `to_vec_named` serialize → TS ~200-LOC switch decode → **dual oracle** (opcode-
  stream identity vs the live golden — NOT a hand-faked blob — AND `Value`-tree identity vs the Rust
  `Interpreter`) over the six R3 corpora. This is simultaneously the 4D-TS **correctness** graft.
- **G-C4:** wire-contract regen-idempotence (encode∘decode∘encode = encode; variant tags pinned).

**Exact open questions.**
1. Does the program cross byte-identically over six corpora against a *live* golden? (PROTO-1's perf
   PASS is not this; the differential-testing literature (research-priorart §4) warns a fabricated
   golden hides exactly the divergences the oracle exists to catch.)
2. Is 4C the {enum-node} escape hatch when 4B's monomorphization would blow G-B4's budget? (O(program)
   code size, zero blowup — but only decision-relevant once G-B4 has a number.)

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · role: the ownership decision + value-plane completion · predecessor-BLOCKED

**Architecture.** Combinators are a construction API over the *same* `bbnf_ir::IrNode` the grammar
plane uses; parse-that keeps its public API and **inherits the generator's optimizer** (`crates/ir/`
passes, rewrites, e-graph, DAG, the decision driver). The distinction from 4A: 4A stops at "analyzable
graph"; 4D fixes that graph to *be* `bbnf_ir::IrNode` so the inheritance is literal, not structural.

**Mechanism.** The reification seam is 4A's; 4D adds the *ownership* commitment (parse-that's runtime
value IS `bbnf_ir::IrNode`) and the value-plane completion (shared with 4C, byte-identical). The two-
tier value plane: reify opportunistically where the closure is MapExpr-shaped (latex-paper 100%,
value.js's 64+19 sites), box the rest as `FnDescriptor::HostClosure{fn_id, sig:TypeDesc→TypeDesc}` —
one new typed variant, sibling to `NumberConvert`/`HexConvert`, keeping the closure API unchanged.

**Gates it would carry.**
- **Gate-0 (DONE, PROTO-2 + negative control):** the `parse_that ↔ bbnf_ir` Cargo cycle is severed by
  a 4-line re-export swap (`parse_that::regex` is literally `pub use bbnf_regex::*`); mechanical, not
  LOCK-5-gated.
- **G-D-rescue (the pass-2 directive, now DEMOTED by the pass-3 finding):** pool/arena-reuse
  `Interpreter::new` (8 Vecs + FxHashMap) and re-run the value floor. **Pass-3 reframe:** research-B §5
  shows the *fast face has no setup to pool* (the generated route is born with the arena, zero
  constructed graph); the pooling rescue only helps the **interpret oracle** — and even a rescued
  interpret path cannot beat codegen structurally. So the rescue closes correctness/oracle viability,
  NOT a competitive terminal. Its decision-value dropped.

**The block (confirmed, three horns).**
- **Horn (i):** a full `GrammarIR`+interpreter+optimizer port to TS = re-implementing the grammar
  plane's engine in a second language — a primitive of **equal-or-greater difficulty than the original
  LOCK-4 problem**.
- **Horn (ii):** the wasm crossing — PROVE-14 measured full-walk value materialization at **0.91×, a
  LOSS**, on exactly the workload a value-producing combinator parser IS.
- **Horn (iii):** the shared-decoder graft is measured-adverse on V8 (−22%); its *fast* form needs the
  build-time emitter, i.e. the same per-language primitive. **4D's thesis "one VM, two sides, for free"
  is refuted for the fast path.**

**Exact open questions (all predecessor-owned, report-only per Q8).**
1. LOCK-5's substrate ruling: is `bbnf_ir` the genuinely shared HIR (**5A**, "inherit for free") or a
   lawful functor across separate HIRs (**5B**, which evaporates the inheritance)? gate-0-compiles is
   mechanical/done; shared-substrate-*semantics* is open and DAG-blocking.
2. LOCK-7's host-fn lattice: the typed shape of `HostClosure` for value.js's ~5 Pratt folds.
3. Is the asserted <2% node ratio real once built? (Shared with 4A G-A3.)

---

## THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The pass-3 staging-boundary finding **collapses most apparent rivalry into composition** and leaves
**three genuine, named forks** — none of which LOCK-4 can elect alone.

**Compositions, NOT incompatibilities (pass-3 corrects the pass-2 "four rivals" framing):**
- **4A ⊕ 4B** compose *across* the staging boundary, not against it: 4A reifies the runtime front and
  runs the pre-codegen passes; 4B is the offline codegen of that reified plan. RF-1 does not make them
  incompatible — it makes them *sequential* (reify → fuse → serialize → emit), coherent **only** for
  the codegen front, never the live combinator API as a runtime terminal.
- **4C ⊕ 4D** share the *identical* value-plane completion (`Op::ApplyFn` + `fns` + `Value`-scalar) and
  the *identical* crossing mechanism; 4C is the iso-oracle, 4D is the ownership commitment. Not rivals.
- **4B-TS emitter ⊕ {4A,4C,4D}-TS:** every TS terminal must lower through the build-time emitter
  (PROTO-1: decoder −22%, emitter ×1.23). The runtime-decoder-as-fast-TS-face is BANKED-by-measurement
  across all three; 4B-TS is the single live TS execution face.

**INCOMPAT-1 — Q-OWNERSHIP: parse-that-private isomorph (4A/4B/4C) vs shared `bbnf_ir` (4D).** These
cannot both be the terminal representation. gate-0 (the Cargo cycle) is *mechanically* severed and
done — but the *semantic* question (is `bbnf_ir` the shared type, 5A, or a lawful functor, 5B?) is
**LOCK-5's to rule**, not LOCK-4's. If 5B, 4D's "inherit for free" evaporates and the private route
wins by default; if 5A, 4D's parsimony dominates *conditional on* the block clearing. **Irreducible
inside LOCK-4; exported to LOCK-5.**

**INCOMPAT-2 — THE FAST TS TERMINAL: build-time emitter (4B-TS) vs any runtime decoder (4A-TS switch,
4C-VM decode, 4D shared-decoder).** RESOLVED by measurement (PROTO-1) *in the emitter's favour* — but
the resolution forces a constitution-iii break: the fast Rust mechanism (monomorphization) and the
fast TS mechanism (emission) are **different mechanisms with no shared isomorphic terminal.** This is
a standing counterexample to source-level rust/ts isomorphism for the combinator core — **exported to
LOCK-6**, forcing 6C (wasm-core) or 6D (behavioral/oracle iso, layout free per language) and away from
6A/6B source-mirroring. The terminal cannot freeze ahead of LOCK-6's verdict.

**INCOMPAT-3 — Q-RUNTIME (reframed by RF-1's resolution): the codegen fast face (4B) vs the interpret
terminal (4C-VM / 4A-graph-walk).** Pass-2 posed this as a three-way throughput fork "still unbuilt."
Pass-3 sharpens it: 4A-interpret and 4C-VM are the *same* runtime-interpret arm (both measured ~adverse
in PROTO-2); 4B-`Seq2` is a hand-written type-combinator that **cannot express a runtime graph** (Q1).
So the meaningful fork is binary — *codegen (fast, staged, generated.rs shape)* vs *interpret (slow,
runtime, the debug/dynamic lane)* — and they are **not incompatible; they are two terminals for two
use-cases** (AOT throughput vs runtime flexibility) selected by the cost model. The genuinely
unmeasured comparison is not the abstract three-way fork but **the generalized codegen route vs the
shipped closure core** (G-B0+G-B1), which is decision-relevant where the three-way abstraction was not.

**The value-plane home (double-home, un-adjudicated).** `Op::ApplyFn` + `apply_fn.rs` MapExpr
evaluator + `Value` scalar arms is IR-VM lowering (LOCK-3 3A) landed in `crates/ir` under a LOCK-4
probe; internal descriptors (Hex/Number/Enum) → LOCK-3, user `HostClosure` → LOCK-7. **GESTALT-Ω owes
the ratification**; the pass-3 disposition (above) is the recommended split, not a settled fact.

---

## BLOCKED-CANDIDATE DETERMINATIONS (the missing-primitive test)

*"A family stalled at a missing primitive equal in difficulty to the original problem is
BLOCKED-candidate."* Applied honestly:

- **4A — NOT BLOCKED-candidate.** Its keystone (`fuse(Graph)→Graph` byte-identity) is unbuilt for a
  fourth pass, but the passes *exist*; the missing work is pointing them at a combinator front — a
  build task, not a missing primitive of equal difficulty. Its *terminal* role is refuted (RF-1); its
  *source-front* role is coherent and cheap.
- **4B — NOT BLOCKED-candidate; the strongest family.** Its "general emitter" is unbuilt, but the regex
  case **proves the primitive exists in-tree** (`generated.rs`). The gap is generalization + a firing
  budget + production dilution — all measurable, none equal-difficulty-to-the-original.
- **4C — NOT BLOCKED-candidate; BANKED.** Its iso-oracle is unbuilt but *cheap* (a live serialize + a
  ~200-LOC decoder + a differential test). No missing primitive.
- **4D — BLOCKED, and its *performance-terminal* form is a formal BLOCKED-candidate.** Horn (i) — a full
  grammar-engine port to TS — **is** a primitive of equal-or-greater difficulty than the original
  problem, satisfying the rule's own text. **But note:** XL-D *retires the need for that primitive* (the
  fast TS face is per-language emission, not a shared runtime), so 4D's *live* role (shared ownership +
  value-plane completion, correctness/parsimony) is BLOCKED on **predecessors** (LOCK-5 5A-vs-5B +
  LOCK-7 lattice), not on an unreachable primitive. The precise status: **4D-as-fast-shared-runtime is
  BLOCKED-candidate (dead by the missing-primitive rule *and* by XL-D); 4D-as-shared-ownership is
  DAG-BLOCKED on LOCK-5.** Do not carry "4D dominates on parsimony ⇒ elect it" — dominance is
  conditional on LOCK-5 ruling 5A and its Rust runtime is a measured loss.

---

## WHAT REMAINS UN-ELECTABLE INSIDE LOCK-4 (hand-off to the gestalts)

1. **Q-ownership → LOCK-5** (5A shared type vs 5B lawful functor; INCOMPAT-1).
2. **The fast-TS terminal / rust-ts iso → LOCK-6** (emitter confirmed, but no shared mechanism ⇒ 6C/6D;
   INCOMPAT-2). Route PROTO-1's emitter result to **LOCK-8** so its V8 discipline is not re-derived.
3. **The value-plane home → GESTALT-Ω** (LOCK-3 internal-descriptor vs LOCK-7 host-closure; recommended
   split above).
4. **4B's perf-claim supersession → PT-tranche** (XL-E principle; no home named).

What IS settled inside LOCK-4 this pass: **the architecture** — a two-front core split by a staging
boundary, fast face = build-time emitter on both languages (existing for regex), value-plane barrier
characterized as node-thin and recognizer-transparent. The election is a gestalt act; the two
prototypes below are the sharpest levers toward it.
