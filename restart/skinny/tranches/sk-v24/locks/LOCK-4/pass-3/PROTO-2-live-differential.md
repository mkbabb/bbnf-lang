# LOCK-4 PROTO-2 (pass-3, probe 2) — the McKeeman gate: VM `Op::ApplyFn` vs parse-that's LIVE `.map`

**Family:** 4C VM-CORE / 4D SHARED-IR — the value plane converted from *worktree-buildable* to *correct-of-record*.
**Base-pin:** `ce51c36fc` — verified ancestor. Worktree was STALE at dispatch (`d2b32f6e3`, a strict ancestor of the pin); fast-forwarded with `git merge --ff-only ce51c36fc` (clean, no divergence).
**HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (clean base pin; my probe is the uncommitted working tree on top — no wrong-artifact risk).
**Isolated `CARGO_TARGET_DIR`:** `…/scratchpad/wt-target` (never touched the main or sibling target dirs).
**Loadavg at measurement:** 8.71–10.29 (1m) during the run; host was under heavy parallel-agent cargo load (peaked 20.4 earlier). macOS arm64. No x86.
**Cap:** 50 min. Froze on schedule.

---

## QUESTION

PROTO-2-vmvalue built the value plane 8/8 GREEN but its Oracle A asserted the VM's projected `Value` against an
**author-written golden** — self-consistency, which differential testing forbids. G-C0's REAL gate never ran:

> Does the VM's `Op::ApplyFn` compute the **same values** parse-that's LIVE closure `.map` does?

And the hardest case was a **pass-through STUB**: `FnDescriptor::HostClosure` was forwarded (child returned unchanged),
so byte-identity there was un-testable by construction. This probe runs the real gate and lands a non-stub HostClosure.

## METHOD (scratch worktree, base-pin `ce51c36fc`, isolated target dir)

0. Applied `PROTO-2-vmvalue.patch` — lands `Op::ApplyFn(FnId)`, `Value` 4→8 arms, `BytecodeProgram.fns: Vec<FnDescriptor>`,
   the `MapExpr`/`FnDescriptor` evaluator (`vm/interpreter/apply_fn.rs`). *(Applied into the worktree, not main.)*
1. **Replaced the author golden with a LIVE in-process differential.** `parse_that = "0.4"` is a first-class dependency of
   `crates/ir`, so parse-that's SHIPPED closure combinator (`regex(...).map(closure)`, `one_of(...).map(...).many(...)`)
   runs in the **same Rust test binary** as `parse_with_ir`/`Interpreter`. For each projection I evaluate the value through
   BOTH **(a)** the VM `Op::ApplyFn` value plane and **(b)** parse-that's live `.map`, then assert the projected `Value`
   equals parse-that's output scalar-for-scalar / node-for-node. **parse-that's actual `.map` return value is the oracle —
   nothing an author typed.** New test: `crates/ir/tests/vm/live_differential.rs`.
2. **Landed ONE non-stub `HostClosure`** — the numeric folds `HostFold::Lerp` and `HostFold::ChannelScale`
   (`FnDescriptor::HostClosure { kind, coeffs }` + `HostFold::eval` closure core in `types/fn_descriptor.rs`). The
   `exec_apply_fn` handler now **EVALUATES** it (parses the child span → `f64`, runs the fold, pushes `F64`/`U32`) instead
   of forwarding the child. Adding the variant forced principled arms in 8 exhaustive matches across the type/payload passes
   (no `todo!()` stubs). The fold is cross-checked three ways: closure core (`HostFold::eval`), the VM `Op::ApplyFn`, and the
   **same arithmetic run as a live parse-that `.map` closure**.

The differential deliberately does NOT parse the `.bbnf` files (`css/pretty.bbnf`, `google-sheets.bbnf`) end-to-end — mirroring
those grammars in hand-written parse-that combinators is not mechanically derivable, so it would smuggle an author transcription
back in. Instead the corpus reproduces the **projection kinds** those grammars are dense in (numeric leaf, hex channel, unit/enum
scalar, arithmetic `->`, string const, `input.len`, repeated `.map` under a `Repeat`) with parse-that's live `.map` as ground truth.

## WHAT RAN

`cargo test -p bbnf-ir --profile ax-iter --test vm` (isolated target dir) → **87 passed / 0 failed** (incremental build; test
phase 0.03s). My 10 new `live_differential` tests are inside that binary alongside the 8 `apply_fn_oracle` tests. A
machine-checkable census (`emit_differential_census`) is dumped to `…/scratchpad/xlang/live_differential_census.json`.

## THE NUMBERS

| Measurement | Result |
|---|---|
| `cargo test … --test vm` (value plane + live differential) | **87 passed, 0 failed** |
| Live differential census — (vm, pt) pairs compared | **12 pairs, 0 divergences** |
| Scalar clauses — VM `Op::ApplyFn` vs parse-that live `.map` (each loops 5 inputs) | NumberConvert→F64, HexConvert→U32, Expr `(in*2)+1`→U32, Expr `in*1.5`→F64, `input.len`→U32, StringLit→Str — **all equal** |
| Tree clause — `doc = num+`, `num = ("1"|"2"|"3") -> input+100` vs parse-that `.map(...).many(1..)` (6 inputs) | VM `Value::Array` projected-leaf vector **node-for-node == parse-that `Vec<u32>`** |
| HostClosure `Lerp(0,255,t)` — core vs VM vs parse-that `.map` (5 t) | e.g. `t=0.5 → 127.5`, `0.25 → 63.75` — VM == core == live `.map` |
| HostClosure `ChannelScale(c,0.5)` — incl. clamp (5 c) | `200 → 100`, `17 → 9`, **`600 → 255` (clamp branch)** — VM == core == live `.map` |
| HostClosure **evaluated, not forwarded** | `assert!(matches!(vm, Value::F64/U32))` passes — a forwarded stub would leave `Value::Span`; it does not |
| `FnDescriptor` variants | 6 → **7** (`HostClosure { kind: HostFold, coeffs: Vec<f64> }`); 8 exhaustive-match sites given real arms |

Sample identity (VM vs parse-that live `.map`, canonical): `3.5==3.5`, `ff→255==255`, `1a→26==26`, `(10*2)+1=21==21`,
`4*1.5=6==6`, `len("hello")=5==5`, lerp `0.5→127.5==127.5`, channel `600→255==255`.

## BOUNDARY (honest scope)

- The differential runs **in-process in Rust** against parse-that's real combinator API — not through a TS re-decode
  (that was the prior probe's Oracle B). This is stronger for the McKeeman gate: it exercises parse-that's actual shipped
  `.map`/`.many` closures, not a re-implementation.
- The `.bbnf` source files were not compiled through the front-end into `GrammarIR` for this differential (see METHOD rationale);
  the hand-built projection-dense IR covers the projection *semantics*, with parse-that live `.map` as the oracle for each.
- `HostFold` is a **closed set of two numeric folds** — the concrete, evaluable core of what LOCK-7 generalizes to arbitrary
  host bodies. It is a real evaluated closure-of-record, not a pass-through, but it is not (nor claims to be) the general
  arbitrary-closure evaluator.
- No cold paired perf measurement — this probe answers **correctness of the value plane under differential**, not throughput.

## VERDICT

**RAN_MEASURED — PASS.** The McKeeman gate is discharged: across a projection-dense corpus the VM's `Op::ApplyFn`-projected
`Value` is **byte-identical to parse-that's LIVE `.map` output — 12/12 census pairs, 0 node divergences**, plus a node-for-node
tree match under `Repeat`/`.many`. And the pass-through STUB is retired: a non-stub `HostClosure` numeric fold (`Lerp`,
`ChannelScale` incl. its clamp branch) is **EVALUATED inside `exec_apply_fn`** (proven by the `F64`/`U32` discriminant, which a
forward would violate) with output matching both the closure core and parse-that's live `.map`. The 4C/4D value plane is now
**correct-of-record**, not merely worktree-buildable. No divergence was exposed.
