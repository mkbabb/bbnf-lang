# LOCK-4 · THE COMBINATOR CORE — PASS-2 RESEARCH DIRECTIVES

One combinator algebra, four candidate compilation strategies, decided on two open axes: **Q-runtime**
(what survives to runtime — a runtime graph, a bytecode program, or nothing) × **Q-ownership** (whose
representation `.then()` builds — parse-that-private or the shared `bbnf_ir`). Every question below is
open; none carries a verdict. Paired-bench discipline for all evidence: **cold, sample-interleaved
A,B,A,B, N≥200, two load windows, both arm orders, same leaf substrate, median + dispersion.**

**Settled substrate facts (build on these; do not re-derive):**
- The combinator node set is a verified near-bijection with `bbnf_ir::IrNode` (14 variants); `parse_with_ir` is live at `interpreter/mod.rs:373`.
- `bbnf_ir` already ships a live value-producing bytecode VM (`vm/{bytecode,compiler,interpreter}`) plus a MessagePack serialization bus (`lib.rs`).
- `FnDescriptor` carries **no** host-closure variant today — a code-verified vocabulary gap.
- In the current value core, `Map`/value-plane `Box<dyn Fn>` indirection confines to scalar leaves; the structural spine inlines.

## 4A — closure-to-graph
- Does a *general* analyzable interpreter tie the closure core on a **projection-heavy** grammar (`all()`-of-mapped-parts), or only on structure-dominated work? *Evidence:* paired-cold, projection-dense vs structure-dominated grammars.
- On real grammars, how large do fused regions grow — enough to amortize per-region dispatch overhead? *Evidence:* measured region-size distribution across JSON/CSS/value.js.
- Can a `fuse(Graph)→Graph` pass emit output byte-identical to a hand-fused arena, at parity cold? *Evidence:* structural-equality oracle + ≥0.98× paired-cold.
- Does that pass **consume** an existing in-tree equality-saturation engine, or clone one? *Evidence:* a dependency map proving reuse. (cross-lock steering, LOCK-2.)

## 4B — comptime-fusion
- On Rust, monomorphized `Seq/Alt` structs remove the box-dispatch tax with the vtable indirect call provably gone (disasm-gated) — treat as mechanism, not mandate. Does that speedup **survive** when production leaf kernels (SIMD, multi-byte dispatch) run on **both** arms? *Evidence:* end-to-end paired bench vs the shipped tuned parser; report the shrink.
- Does the monomorphization mechanism have a direct V8 analog, or must the TS face be a *different* build-time-emitter mechanism over a shared plan? *Evidence:* priorart citation (nom/winnow class) + a first TS paired number proving the emitted region carries no per-parser indirection, dispatch- vs allocation-elimination isolated.
- What fusion **policy** bounds monomorphization / generated-size blowup across the full consumer surface (fuse arity, dispatch fan-out, boxing depth-cap)? *Evidence:* size + incremental-compile numbers on CSS/value.js against a named budget, not JSON alone.
- Is this strategy best formulated as a standalone terminal runtime, or as the Rust lowering of a representation-crossing terminal? *Evidence:* the observable that would distinguish the two.

## 4C — portable-program isomorphism layer
- Can a language-neutral opcode/operand stream cross **as data** (serialize in Rust, decode in TS) and yield a byte-identical opcode stream **and** identical parse over a shared corpus? *Evidence:* a small (~200 LOC) TS switch-loop decoder over a serialized bytecode program; dual oracle (stream identity + parse identity).
- If a private bytecode ISA is proposed, what justifies it against the existing in-tree VM contract? *Evidence:* an abrogate-before-patch adjudication. (cross-lock steering, LOCK-3.)

## 4D — grammar-sugar / shared-IR ownership
- Does arbitrary `.map`/projection output reconcile cheaply with a tape-first runtime, or does materialization impose a tax? *Evidence:* projection-dense grammar; paired-cold materialization-tax + net staged-vs-closure ratio; **name the negative result** if closures win on the LL(1) shape.
- A minimal Rust combinator front building the shared IR for JSON: byte-identical trees vs hand + closure parsers, then paired-cold interpret **and** compile modes over six corpora. *Evidence:* the decisive paired number.
- Can the shared serialized program drive a value-producing parse through a small TS decoder (the 4C graft) at a stated no-regression floor vs today's TS closures? *Evidence:* the graft bench.
- Where do state-effecting combinators (`peek`/`lookAhead`/`recover`/`mapState`) and host-closure semantics live in the IR vocabulary? What minimal lawful variant set — **measured, not guessed**? *Evidence:* enumeration + a paired regression lane per variant.
- Track LOCK-5's substrate ruling and LOCK-7's host-fn-lattice shape as hard DAG predecessors — report their shape, do not resolve here. (cross-lock steering.)

## Cross-cutting keystones (a single front may take several numbers at once)
- **Common-baseline runtime fork:** the candidate runtimes over the **same** grammar with the **same** leaf substrate, so the runtime fork is *measured*, not inferred — it is currently under-determined.
- **First cross-language number:** the rust/ts execution face is unbuilt across all formulations; a first honest TS paired number is high-leverage for any front, and no terminal can be named without one.
