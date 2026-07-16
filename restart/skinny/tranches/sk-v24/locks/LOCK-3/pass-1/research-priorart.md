# LOCK-3 — THE IR TRANSPOSITION SPINE · pass-1 PRIOR-ART RESEARCH

Charter (PORTFOLIO §LOCK-3): the IR tower and its transpositions — grammar AST → typed HIR →
mid-IR → backend shapes → emission; lowering contracts and invariants; residualization
(PROVE-12) generalized; the decision driver's seat; multi-backend (Rust now, TS later) over a
backend-agnostic middle. Standing laws: no value-discard, no silent epsilon, preserve rich AST.
Families: **3A CONTRACT-TOWER**, **3B SINGLE-IR-MULTIPASS**, **3C GRAMMAR-HOMOIOUSIA**,
**3D STAGED-RESIDUAL**. No family allegiance held here — this sweeps the whole problem class.

Method: web + codebase. Constitution binds every judgment (concrete artifacts or SPEC-ONLY;
consumer-less substrate forbidden; type plane costs zero at runtime; colocation + parsimony;
extant GN codex is the baseline, never silently contradicted).

---

## 0. WHAT EXISTS AT HEAD (the tower we already have — the baseline the gestalt re-cuts)

The skinny IR tower is **not hypothetical**; it is four concrete levels, verified by R1/R6 at
`path:line`:

1. **`GrammarIr` (front IR)** — `skinny/crates/ir/src/lib.rs:33-271`. Arena of `Expr` (`ExprKind`:
   Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation/**Project**/Discard) indexed by `ExprId`;
   `nullability` is a Changed-flag fixpoint (`ir/src/lib.rs:320-349`) — already LLVM-shaped, not a
   structural hash. `ExprKind::Project { body, target, ty: Option<String> }` (`:261-265`) is the
   `->` carrier.
2. **Typed pass layer** — `passes` infers a structural `Type` (Seq/Alt/List) but **discards
   `Project.ty`** at `passes/src/lib.rs:203` (`infer_expr` returns `self.infer_expr(*body)`). This
   is the load-bearing defect the whole type-plane story turns on (R6 §5a).
3. **`BackendIr` (mid-IR)** — `ir/src/lib.rs:351-413`. `BackendExpr` (Entry/Seq/Alt/RepeatLoop/…/
   `TapeEmit{kind}`/`DirectBuild{shape,fields}`/**`ValueProject{body,target,ty}`**/Return) +
   `Recognizer::SimdScan{mode,alphabet,site}` (`:416-422`) + `ShapeFacts`. `BackendShape` is a
   5-variant enum (`:361`: EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage).
4. **Backend shapes → emission** — `codegen` lowers `BackendIr` → Rust via `lower/` (per-shape
   emitters) + `emit/` (json_sink_direct, css_scan_direct, general_descent) + `templates/`
   (`runtime_generator.rs` raw-string spine). Entry `codegen/src/lib.rs:101,130`.

**The rich schema IR already exists but is starved**: `DirectBuildTarget{ type_ref, presence,
cardinality, representation, decode }` (`ir/src/lib.rs:483-535`) and `DirectSchemaSet`/
`DirectTypeRef` (`codegen/src/direct_schema.rs:4-120`) are a real type lattice — but
`DirectBuildField.target` is `None` on every offset-tape descriptor (R6 §5c) and the only
schema *producer* is the 1014-LOC hand file `xtask/src/real_typed_schema.rs:7` (R6 §5d). The
transposition tower is built; the **arrow-threading and the schema-derivation seams are the
open holes** (R6 §8b names the exact two edits).

**Verdict-relevant fact**: this is *3B-shaped today* — one rich `BackendIr` refined by passes,
`BackendShape` as an attribute, the fewest transpositions that emit. LOCK-3's job is to decide
whether that shape is kept and hardened (3A/3B) or re-founded (3C/3D), each with a *named
supersession* of the GN codex, never a silent contradiction (Constitution ix).

---

## 1. FAMILY 3A — CONTRACT-TOWER (small fixed tower, each lowering a total function w/ checkable contract)

### Prior art — what won
- **The Nanopass Framework** (Sarkar/Waddell/Dybvig 2004; Keep/Dybvig 2013, *A Nanopass Framework
  for Commercial Compiler Development*, ICFP). The canonical win for this family: `define-language`
  formally specifies each IR's grammar; `define-pass` is a total map from one language to the next;
  the framework **auto-verifies output well-formedness against the target language** and fills
  boilerplate. Chez Scheme ships ~50 nanopass IRs in production — the *commercial* qualifier in the
  title is the proof it scales past pedagogy. This is CONTRACT-TOWER's existence proof.
  <https://nanopass.org/> · <https://andykeep.com/pubs/dissertation.pdf>
- **rustc's HIR→THIR→MIR** — three named IRs, each a checkable lowering, THIR the type-annotated
  bridge, MIR the CFG for borrowck/opt. The industrial "small fixed tower" that is *not* nanopass-
  many. <https://rustc-dev-guide.rust-lang.org/backend/codegen.html>

### What lost / the trap
- **Nanopass's cost is transposition tax**: many IRs means many near-identity pass arms and a
  materialization per level. Keep's own thesis measures the boilerplate reduction but the
  representation-copy cost is real; the framework mitigates by auto-generating catamorphism
  boilerplate, *not* by eliminating the copy. For a **hot, cold-per-parse generator** (our §0 law:
  warm benches are disinformation) a many-IR tower risks compile-time blowup and — if any level
  leaks into runtime — the zero-cost invariant. The GN codex already chose "fewest transpositions"
  (3B-lean); a 3A answer must justify each *new* level against `generated_size_budget`
  (MEMORY: generated-size-budget) and the LOC ledgers (GN-3 is net +35 LOC total).
- **Trap named by our own memory**: `no-silent-epsilon` — nanopass total-pass arms map cleanly to
  our law that "lowering catch-alls panic on unknown rule_kinds, never silently return Epsilon".
  A CONTRACT-TOWER here *is* that law made structural. GN-3's `DeriveError::Unify` (never a silent
  fallback leaf, GN-3 §1.3 P56) is the same principle already in the codex.

### Constraints our evidence imposes on 3A
- Contracts must be **CI-enforced artifacts, not documents** (GN-1 §1.2 P19; GN-3 W1-W5 every gate
  is a `cargo test`/regen byte-diff). The nanopass "output matches target language" check is
  exactly the round-trip/parity oracle 3A demands — and we already have its instances: `regen ∘
  regen = regen` byte-identity (P53, clean-regen-discipline), checkasm NEON==scalar parity, the
  `CSS_CANON_PROFILE` XOR-fold parity checksum (PROVE-12 §5), and the derived≡hand schema
  byte-diff (GN-3 W3). A 3A tower's per-level oracle is a *generalization of instruments that
  already exist*, not net-new ceremony.

---

## 2. FAMILY 3B — SINGLE-IR-MULTIPASS (one rich IR refined in place; the LLVM shape)

### Prior art — what won
- **LLVM IR** — the reference single-IR-multipass: one SSA IR, passes annotate/transform in place,
  target lowering at the end. Won the general-purpose-compiler world for two decades.
- **Sea-of-nodes / GraalVM** — one graph IR refined by rewrites; the second-Futamura-projection JIT
  (Truffle) rides it. Formal semantics exist (*A Formal Semantics of the GraalVM IR*,
  arXiv:2107.01815) — evidence the single-graph-IR is analyzable, not just pragmatic.

### What lost / the trap (this is the crux the field litigated and settled)
- **MLIR is the field's verdict *against* monolithic single-IR** for multi-abstraction pipelines:
  "High-level semantics (shapes, parallel loops, tensor semantics, sparsity, control-flow
  abstractions) must be lowered into LLVM IR early, **losing information** useful for later
  optimizations; representing high-level constructs forces many brittle intrinsics/metadata that
  hamper composability." MLIR's progressive lowering with dialects + **attributes/interfaces on ops
  to encode invariants** is the direct answer. <https://mlir.llvm.org/docs/Dialects/Vector/> ·
  <https://www.lei.chat/posts/compilers-and-irs-llvm-ir-spirv-and-mlir/>
- **The trap for LOCK-3**: 3B's "BackendShape as attributes" is *exactly the intrinsic/metadata
  pattern MLIR indicts* if the single IR is asked to carry too many abstraction levels. Our own
  evidence base already shows the symptom: `TapeEmit{kind}` is a **coarse 9-variant tag**
  (`ir/src/lib.rs:452-463`) that *collapses* the rich `DirectBuildTarget` — R6 §5c: "the rich
  per-field schema exists but on the offset-tape path the `target` field is `None` for every
  descriptor." That is precisely "lowered too early, information lost." A 3B answer must prove the
  single IR carries the `->` type through to emission **without** the coarse-tag collapse
  (`typed-materialization-invariant` in MEMORY: "every `->` must reach the tape emitter; inference
  composes types, never loses them").

### Constraints our evidence imposes on 3B
- The GN codex is *already committed to 3B-lean* ("the fewest transpositions that suffice", PORTFOLIO
  3B; GN-1's tower is one `BackendIr`). So a 3B pass here is **hardening the extant shape**, and its
  supersession budget is small — but it must *close the collapse*: thread `Project.ty` (`passes/src/
  lib.rs:203`) into `DirectBuildTarget.type_ref` and retire the lossy 4-way `ProjectionKind` fold
  (`general_descent/view.rs:20-46`), which is GN-3.W2 verbatim. **3B's win is measured by the
  no-value-discard law** (MEMORY: `no-value-discard`, "codegen must never discard computed values
  (.map(|_| ())); direct projection captures everything").
- **Refutation guard**: the type plane is compile-time only (Constitution v; GN-3 §1.4). A 3B
  "annotation pass" that leaks a runtime instruction is refuted by `git diff crates/runtime/`
  (GN-3's every-wave invariant gate). Single-IR-multipass must annotate *the generator's* IR, never
  emit a runtime type tag — the `stored discriminator` temptation (R6 §2, §8a) is a **GN-4 value-
  fabric** question (a 1-byte kind column), explicitly *not* a type-plane one.

---

## 3. FAMILY 3C — GRAMMAR-HOMOIOUSIA (the IR is itself bbnf-specified; transpositions are grammar-to-grammar)

### Prior art — what won
- **Nanopass `define-language`** *is* grammar-specified IR — each IR is a declared grammar, and this
  is the winning half of nanopass (the auto-verification depends on it). This is the strongest
  positive evidence for 3C: grammar-specified IR is *proven commercial technology* in Chez Scheme.
  <https://nanopass.org/>
- **Self-hosting / metacircular bootstrap** — McCarthy's Lisp-in-Lisp (1960), Hart-Levin self-
  hosting compiler (1962) established "define the language formally, implement in a host, compile
  itself." **Sysmel** (arXiv:2309.15416, *Design and Implementation of an Extensible System Meta-
  Programming Language*) is a live 2023 datapoint: metacircular definition, bootstrapped through
  **three full self-compilation cycles** — direct evidence a self-describing IR *converges* under a
  fixpoint gate. This maps onto our LOCK-7 `regen∘regen=regen` byte-identity fixpoint (SPEC §2 GN-1).

### What lost / the trap
- **Bootstrap staging cost & the chicken-egg**: a grammar-specified IR needs a stage-0 committed
  artifact to parse the IR grammar that describes the IR. Our memory already codified the discipline
  (`clean-regen-discipline`: generated files are always fresh regen output, never hand-patched;
  LOCK-7 7A STAGED-BOOTSTRAP: committed stage-0, minimal bootstrap surface, maximal auditability).
  The trap is **auditability collapse**: if transpositions become grammar-to-grammar maps that are
  themselves generated, a bug is two meta-levels from the symptom. Sysmel's answer is the 3-cycle
  fixpoint; ours must be the same byte-identity gate.
- **Constitution i vs. parsimony**: "everything is grammar-driven … save primitives and combinators."
  3C is the maximal reading. But `no-god-modules` + `extreme parsimony` (Constitution vi) and
  `general-infra-crates` (MEMORY) constrain it: an IR-grammar plane is only lawful if it has a
  **same-wave consumer** (Constitution viii, Y.13) — a grammar-specified IR with no generator eating
  it is consumer-less substrate, *forbidden*. GN codex has **no GN-wave that consumes an IR grammar**
  (GN-3 explicitly rules CSP/e-graph out of scope; there is no IR-homoiousia wave). So 3C is a
  LOCK-7-adjacent design input **without a GN home today** — the same status GN-3 §1.5 gives full-
  inference CSP/e-graph hybrids. A 3C answer must either *name the consuming wave it adds* or accept
  SPEC-ONLY status.

### Constraints our evidence imposes on 3C
- **True isomorphism to the regex HIR** (Constitution ii; LOCK-5 decides the lawful form, every
  other lock *consumes its verdict*). 3C for LOCK-3 cannot unilaterally define the IR grammar — it
  must be a *functor-compatible* proposal that LOCK-5's ONE-HIR / LAWFUL-FUNCTOR verdict can consume.
  Coupling risk named: LOCK-3's 3C and LOCK-5's 5A ("regex HIR is a sub-language of bbnf HIR — a
  single type in a single home") are the same claim from two seats.

---

## 4. FAMILY 3D — STAGED-RESIDUAL (IR as explicitly two-stage; residualization = erasure of stage 0)

### Prior art — what won (this is the deepest, most-cited vein for our exact problem class)
- **Futamura projections** (1971): specialize an interpreter w.r.t. a program → a compiled program;
  the residual is "leaner, faster, optimized for unknown inputs." **A parser generator emitting a
  specialized parser from a grammar IS the first Futamura projection** — grammar = static input,
  input-bytes = dynamic. PROVE-12's const-fold of the delimiter mask is a *hand-rolled instance*:
  the grammar-constant delimiter set (static) is residualized to an immediate `and` mask, the
  runtime `<[u8]>::contains` (dynamic dispatch over static data) erased. <https://en.wikipedia.org/
  wiki/Partial_evaluation> · <https://arxiv.org/pdf/1611.09906>
- **PyPy/RPython + GraalVM/Truffle** — production JITs built *on* the (first/second) Futamura
  projection; the field's proof that staged specialization is not academic.
  <https://dl.acm.org/doi/10.1145/3359061.3361077>
- **Lightweight Modular Staging (LMS)** (Rompf/Odersky, CACM 2012) — "uses **only types** to
  distinguish the two stages of execution … stage-1 executes partially to generate an optimized
  intermediate program; stage-2 runs it." This is *precisely* PORTFOLIO 3D: "a two-stage program
  (comptime/runtime split in the types); residualization is erasure of stage 0." LMS is the named
  prior art for encoding the stage split **in the type system** rather than in syntax
  (quasiquotation). <https://cacm.acm.org/research/lightweight-modular-staging/>
- **Terra** (DeVito et al., PLDI 2013) — multi-stage; Lua metaprogram (stage 0) generates
  low-level Terra (stage 1). The template-as-generator model our `runtime_generator.rs` raw-string
  spine already is, but typed. <https://www.researchgate.net/publication/262238598>

### What lost / the trap
- **The BFA (binding-time analysis) precision cliff**: partial evaluation only wins where the
  static/dynamic split is *precise*; imprecise BTA residualizes almost nothing or, worse, over-
  specializes and **code-bloats** (monomorph explosion). PROVE-12 shows the *good* case (+18
  generated LOC, one surviving monomorph symbol, the rest fully inlined) — but it warns: our
  `generated-size-budget` law (MEMORY) exists *because* staged specialization's failure mode is
  exactly O(N) generated-LOC blowup. A 3D answer must carry a per-site staging *decision* (which
  LOCK-2's CSP/cost engine owns — PORTFOLIO 3D: "per-site staging solved by LOCK-2's engine"), not
  a blanket "residualize everything."
- **The self-deception trap our own §0 canonized**: PROVE-09 (portable_simd shim) — "the 1.34-1.57×
  did not reproduce; **two-arm window asymmetry is the classic self-deception; paired interleaving
  is mandatory even for obvious wins**." A staged-residual claim ("const-folding must be faster")
  is refuted at the seam unless cold-paired-PMU measured (PROVE-12 did this correctly: baseline vs
  modified binaries interleaved back-to-back, N=3×800, ins/B the load-proof floor). **STAGED-RESIDUAL
  is the family most prone to unmeasured plausibility** — the constitution's clause vii (concrete
  artifacts or SPEC-ONLY) bites hardest here.

### Constraints our evidence imposes on 3D
- **PROVE-12 makes 3D partly born-verified**: residualization is *already proven* on the CSS
  delimiter scan (GO, CONFIRMED; every corpus/lane ins/B −3.7…−6.4%, cyc/B 0.81-0.98×,
  regen bit-for-bit). PROVE-12 §7 explicitly names the generalization: "the same `const fn mask`
  + const-generic seam" applies "to any grammar-derived delimiter/STOP-set scan (JSON's structural
  scan, general-descent)." So 3D's kernel is **not speculation — it is a landed pattern seeking
  generalization**, which is the SPEC's whole thesis (most waves begin from working patches).
- **The type-plane-as-stage-0 bridge**: GN-3's type plane is *definitionally* stage-0 — "emits no
  runtime instruction; its runtime delta is identically zero by construction" (GN-3 §1.1, the
  verified ZERO-RUNTIME-COST invariant). This is the cleanest existing instance of "erasure of
  stage 0": the schema is computed at generation time and *fully erased* before runtime. A 3D
  reading unifies GN-3 (type erasure) and PROVE-12 (constant erasure) under one staged-IR account —
  PORTFOLIO 3D's "PROVE-12 becomes a corollary." **This is the strongest cross-family synthesis
  available**: 3D subsumes the type plane and the residualizer as two erasures of the same stage.

---

## 5. CROSS-CUTTING CONSTRAINTS (bind every family — the evidence base's hard walls)

- **Multi-backend over a backend-agnostic middle** (LOCK-3 charter; LOCK-6 decides "reasonable").
  Prior art: CrossTL's unified IR (arXiv:2508.21256 — language-specific frontends + dedicated
  CodeGen backends over one CrossGL IR) and MLIR's dialect-conversion-to-target model. Our memory
  binds the shape: `backend-agnostic-types` (grammar types are abstract `TypeDesc::Named`; each
  backend resolves to native types), `no-ts-ir` (**no IR layer for TS; the AST IS the combinator** —
  a hard constraint: the TS backend does *not* get its own mid-IR), `multi-backend` (shared decision
  driver + Emitter trait for Rust/TS/WASM). PROVE-14 proved the **byte-exact offset-tape crossing**
  to TS (CONFIRMED, 4.4× stop-early; full-walk 0.91× honestly unmet). So the backend-agnostic middle
  is **the tape layout + the schema IR**, not a shared instruction IR — TS consumes the tape and the
  `DirectSchemaSet`, it does not re-lower a mid-IR. Any LOCK-3 answer that gives TS its own IR tower
  contradicts `no-ts-ir`.
- **The decision driver's seat** — LOCK-3 charter names it; PORTFOLIO 3C/2C put the CSP there.
  Constraint: `pluggable-components` (MEMORY — cost model, pattern registry, rewrite rules must be
  pluggable, not hardcoded branches). `BackendShape` selection lives in `passes::derive_backend_shape`
  (`passes/src/lib.rs:398`) fed by `CostFacts` (`ir/src/cost.rs`); the driver is *shared across
  backends*, decided once in the middle. LOCK-3 owns *where the driver sits in the tower*; LOCK-2
  owns *what engine it is*. Do not merge the seats.
- **Standing laws restated (LOCK-3 charter names three)**: `no-value-discard`, `no-silent-epsilon`,
  `preserve-rich-ast` (MEMORY: `preserve-rich-ast` — "never flatten typed grammar rules for speed;
  rich AST parity with lightningcss is non-negotiable"). These three are *the CONTRACT-TOWER
  invariants already in force*; any family's per-level contract must include them.
- **Extant-tranche consciousness (Constitution ix)**: GN-1 (the tower's port + directory-module
  recut — `ir/src/{cost,recognizer,shape}/`, `codegen/src/{lower,emit,templates,schema}/`), GN-3
  (the type-plane, walls 1-3, the arrow-thread + sum node + hand-schema death) are the baseline.
  LOCK-3 **re-cuts** them into a coherent spine; it must name every divergence as a supersession
  (e.g. if 3A adds an IR level GN-3 did not have, that is a named super­session with a LOC-budget
  justification, not a silent contradiction).

---

## 6. SYNTHESIS — the settled adjudications this pass hands forward

1. **The tower is 3B today** (one rich `BackendIr`, `BackendShape` as attribute, fewest
   transpositions). The GN codex is committed to 3B-lean. The *open holes* are the arrow-thread
   (`passes/src/lib.rs:203`) and the schema-derivation producer (R6 §8b) — both are GN-3 waves.
   **3B's burden is to close the coarse-tag collapse** (`TapeEmit{kind}` starving
   `DirectBuildTarget`), measured by the typed-materialization-invariant + no-value-discard.
2. **MLIR is the field's warning against 3B-maximal**: single IR carrying too many abstraction
   levels loses information "lowered too early." Our own `TapeEmit` collapse is that symptom in
   miniature. This is the strongest argument for a **3A discipline layered on the 3B substrate** —
   keep one IR, but give each *pass* a nanopass-style checkable output contract (we already have the
   oracle instruments: regen byte-identity, checkasm parity, XOR-fold checksum, derived≡hand diff).
3. **3D is the most-evidenced synthesis vein**: it *subsumes* GN-3 (type erasure) and PROVE-12
   (constant erasure) as two erasures of stage 0, with LMS/Futamura/Terra as named prior art and
   **two landed born-verified instances already in hand**. PROVE-12 §7 hands 3D its generalization
   target verbatim. But 3D's failure mode (BTA imprecision → monomorph bloat) is exactly why
   `generated-size-budget` exists, and per-site staging is LOCK-2's engine, not LOCK-3's to hardcode.
4. **3C (grammar-specified IR) has no GN consumer today** and is LOCK-5/LOCK-7-coupled (the IR
   grammar must be functor-compatible with the regex HIR verdict LOCK-5 owns). It is a real design
   input but, absent a same-wave consumer, is SPEC-ONLY / consumer-less-substrate-forbidden until a
   consuming wave is named. Nanopass proves the *positive* (grammar-specified IR is commercial-grade);
   Sysmel's 3-cycle fixpoint proves the bootstrap converges under our exact `regen∘regen=regen` gate.
5. **Named traps carried forward**: (a) MLIR's "lower-too-early" information loss — already present
   as the `TapeEmit` collapse; (b) LMS/PE binding-time imprecision → generated-LOC blowup, guarded
   by the LOC budget; (c) PROVE-09's two-arm-window self-deception — every residualization claim is
   cold-paired-PMU or it is refuted; (d) consumer-less substrate — any new IR level or IR grammar
   needs a same-wave reader; (e) TS-gets-no-IR (`no-ts-ir`) — the backend-agnostic middle is the
   tape + schema, not a shared instruction IR.

---

## Citations

**Internal (absolute paths).** IR tower: `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/ir/src/lib.rs`
(`ExprKind::Project` :261, `BackendExpr`/`ValueProject` :375-413, `Recognizer::SimdScan` :416,
`TapeEmit`/`TapeKind` :452, `DirectBuildTarget` :483, `nullability` :320). Arrow-discard defect:
`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/passes/src/lib.rs:203`. Schema IR + hand producer:
`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/direct_schema.rs`,
`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/json_typed_direct.rs`,
`/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/real_typed_schema.rs:7`. Emit spine:
`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/lib.rs:101,130`,
`.../codegen/src/runtime_generator.rs`. Evidence: SPEC §0 + §2 GN-3
`/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v24/SPEC.md`; PORTFOLIO Constitution
`.../sk-v24/locks/PORTFOLIO.md`; R1 `.../research/R1-skinny-cartography.md`; R6
`.../research/R6-value-type-plane.md`; PROVE-12 REPORT `.../prototypes/PROVE-12/REPORT.md`; tranche
codex `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/sk-v24/{INDEX,GN-1,GN-3}.md`.

**External (URLs).**
- Nanopass Framework: <https://nanopass.org/> · Keep dissertation <https://andykeep.com/pubs/dissertation.pdf>
- MLIR progressive lowering: <https://mlir.llvm.org/docs/Dialects/Vector/> · <https://www.lei.chat/posts/compilers-and-irs-llvm-ir-spirv-and-mlir/>
- Partial evaluation / Futamura: <https://en.wikipedia.org/wiki/Partial_evaluation> · <https://arxiv.org/pdf/1611.09906> · 2nd-projection practical <https://dl.acm.org/doi/10.1145/3359061.3361077>
- Lightweight Modular Staging (types-distinguish-stages): <https://cacm.acm.org/research/lightweight-modular-staging/>
- Terra multi-stage: <https://www.researchgate.net/publication/262238598>
- Sysmel metacircular 3-cycle bootstrap: <https://arxiv.org/pdf/2309.15416>
- GraalVM IR formal semantics: <https://arxiv.org/pdf/2107.01815>
- CrossTL unified-IR multi-backend: <https://arxiv.org/html/2508.21256>
- rustc HIR/THIR/MIR tower: <https://rustc-dev-guide.rust-lang.org/backend/codegen.html>
