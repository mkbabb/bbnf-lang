# LOCK-4 — THE COMBINATOR CORE · PASS-3 RESEARCH DIRECTIVES

*Steering questions only; each names required evidence. Base law (XL-H): every measurement names its HEAD; worktree prototypes base-pin. Report predecessor-lock shapes; never resolve them here.*

## Settled cross-lock facts (binding context — convey, do not re-litigate)
- **TS face (XL-D):** the fast TS face is the build-time emitter; runtime decoders (graph-walk / switch-loop) survive only as isomorphism/correctness oracles. Frame TS-throughput questions against this — never re-measure a decoder as the fast face.
- **Regex (XL-C):** two domain-fit types (opaque `Regex(StringId)` grammar tier; rich `Hir` engine tier) with a cheap bridge; the shared-IR question no longer hinges on ONE-HIR; the regex functor is retired. Vendored parse-that-regex in skinny is the substrate.
- **Substrate (XL-A):** skinny@HEAD is the only living implementation and measurement substrate.
- **Hand-forward:** `bbnf_ir::IrNode` is a live serializable 14-variant near-bijection of the combinator set, carrying fusion passes + a path-dep-**consumed** (not cloned) e-graph. A comptime-monomorphization mechanism (Rust) and a build-time-emitter mechanism (TS) each exist — treat them as **two distinct mechanisms**, not one shared terminal; infer no election from either.

## Q1 — Runtime graph → comptime monomorphization
Does the **runtime** combinator API (it builds a runtime `IrNode`) reach comptime type-monomorphization at all, or only via a build-time codegen step over a comptime-known graph (grammar-text→IR)? **Evidence:** a mechanism trace of the exact runtime→comptime step, or a demonstration none exists absent codegen-reflection, plus what a monomorphizing lowering demands of the API.

## Q2 — Value plane through an arbitrary host closure
Prior probes routed only MapExpr-expressible projections. Census the arbitrary-host-closure surface across value.js / latex-paper / keyframes: how many `->` sites are NOT MapExpr-expressible, their typed shapes, and what a serializable descriptor must carry (today's `FnDescriptor` has no `HostClosure`). **Evidence:** per-corpus non-projection counts + proposed typed shape + implied host-fn lattice.

## Q3 — Opaque-barrier surface size
How large is the `Opaque(HostFnId)` analysis barrier on value.js / latex-paper — fraction of nodes an analysis cannot see through? **Evidence:** counted barrier ratio per corpus vs the ≤2% floor.

## Q4 — Fusion-policy / blowup budget (a firing number)
A pluggable {inline-monomorphize | enum-node | box-here} cost model needs named caps: fuse-arity, dispatch-fanout, boxing-depth, and a `.rlib`/`dist` + incremental-compile budget on CSS **and** value.js. **Evidence:** thresholds grounded in measured code-size / compile-time growth vs fuse arity across the full consumer surface.

## Q5 — Serializable-plan observable
Is a fused region derivable from one **serializable plan** that also drives the TS emitter (⇒ one plan, two lowerings, byte-identical parses), or intrinsic to the Rust type system? **Evidence:** the distinguishing observable — one plan driving both lowerings to identical output — or a proof against it.

## Q6 — Isomorphism-of-record standard + value-plane home
For a program crossing as data (serialize Rust, decode TS), define isomorphism-of-record evidence: the dual oracle — opcode-stream identity vs a **live** Rust golden (not a hand-faked blob) AND `Value`-tree identity vs the Rust interpreter — and its corpus set. Also report which lock owns the internal-descriptor evaluator (Hex/Number/Enum): LOCK-3 IR-VM lowering or LOCK-7 host dispatch?

## Q7 — Setup vs value-plane tax vs recognizer cost
Characterize per-parse `Interpreter::new` setup (8 Vecs + FxHashMap) and pooling/arena-reuse that could eliminate it. **Evidence:** setup-allocation share of single-parse cost, the amortization curve as setup is pooled, and a clean split of recognizer / value-plane / setup cost.

## Q8 — Predecessor shapes (report only)
Report the current shape and open sub-questions of LOCK-5 (shared-substrate semantics: shared type vs lawful functor — XL-C already retired the regex functor) and LOCK-7 (host-fn lattice), framed as inputs to LOCK-4. **Evidence:** each lock's disposition; no attempt to settle it here.
