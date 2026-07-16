# PROTO-2 — LOCK-4 pass-2, probe 2: Cycle-break (gate-0) + shared-IR value-plane floor (F-4, family 4D/4C)

**Verdict line:** GATE-0 = **PASS** (Q-ownership axis decided: the Cargo cycle is
breakable and `bbnf-ir` compiles without `parse_that`). VALUE-PLANE FLOOR = **RAN,
NEGATIVE on the literal >=0.98x criterion** — the value-completed shared VM does NOT
reach closure throughput on the LL(1)/projection shape at these input sizes (ratio
0.021x-0.46x). The honest decomposition: the value-plane *tax proper* (`Op::ApplyFn`)
is small (~5% over the recognizer tape); the dominant gap is the VM's **per-parse setup
allocation** (a recognizer/tape-substrate property, not the value floor), which amortizes
~20x on a realistic single parse.

- **BASE-PIN:** measured on HEAD `7cdbff4ef411154dd9bf4b80731c0616b1579d22`. Worktree was
  stale at `d2b32f6e3`; fast-forwarded with `git merge --ff-only 7cdbff4ef` (clean,
  descends-OK verified). No wrong-artifact-risk.
- **Host:** darwin arm64, 18 cores. Load average during measurement 16.3 -> 28.0 (high,
  contended - magnitudes are pessimistic/soft, ranks are robust). Isolated
  `CARGO_TARGET_DIR` under scratchpad; sccache shared wrapper.

---

## Q1 (gate 0) - Can the `parse_that <-> bbnf_ir` cycle be broken so a shared IR/VM can live in `bbnf-ir`?

**Method.** In the bbnf-lang worktree, moved the only four `parse_that` usages in
`crates/ir` onto `bbnf_regex` (which already re-exports the identical types - `parse_that::regex`
is literally `pub use bbnf_regex::*`):
- `crates/ir/src/vm/bytecode.rs`: `parse_that::regex::Dfa` -> `bbnf_regex::Dfa` (type + `::compile`).
- `crates/ir/src/passes/inspect/leading.rs` & `passes/recognizers/key_dispatch.rs`:
  `parse_that::regex::classify::{RegexClass, classify_regex}` -> `bbnf_regex::classify::{...}`.
- Deleted `parse_that = "0.4"` from `crates/ir/Cargo.toml:13`.

Then a read-only parse-that **scratch** (rsync copy per R3 sec 8, isolated target dir),
adding a `parse_that -> bbnf-ir` dependency edge and repointing the `bbnf-ir` patch at
the MODIFIED worktree.

**What ran / numbers.**
- `cargo build -p bbnf-ir` (isolated target) -> **Finished, exit 0, 19.38s** (gate-0 only),
  then **5.57s** again after the value-plane additions. Compiles.
- `cargo tree -p bbnf-ir` in the scratch (with the reverse edge present): bbnf-ir's forward
  tree contains **NO `parse_that`** (only `bbnf-regex`, `csp-solver`, `egraph`,
  `egraph-derive`, rayon, rmp-serde, ron, rustc-hash, serde, serde_json, smallvec).
  Resolution succeeds - **no cyclic-package error**. Edge realized as
  `parse_that -> bbnf-ir -> bbnf-regex` (acyclic).
- **Negative control:** repoint `bbnf-ir` at the UNMODIFIED main repo (still
  `parse_that = "0.4"`) -> `cargo tree` errors: **`error: cyclic package dependency: package
  bbnf-ir ... depends on itself ... which satisfies dependency parse_that = "^0.4"`.** Proves the
  gate is load-bearing: the cycle exists without the change and is severed by it.

**Answer.** YES. Q-ownership gate-0 = **PASS**. The IR/VM can own the shared substrate in
`bbnf-ir`; a downstream `parse_that -> bbnf_ir` consumption edge is acyclic.

---

## Q2 (F-4) - Value-plane floor: does the value-completed shared VM interpret within 0.98x of the closure core?

**Method - implementation (value completion PROBE-1 never touched).**
- `Op::ApplyFn(FnId)` added; emitted at the Map-drop site
  (`vm/compiler/node.rs`, previously `IrNode::Map{inner,..} => compile_node(inner)` - `fn_id` dropped).
- `fns: Vec<FnDescriptor>` (`#[serde(default)]`) added to `BytecodeProgram`, cloned from `GrammarIR::fns`.
- `Value` extended with `F64(f64) / U32(u32) / Str(String) / Variant{tag,inner}` arms.
- `Op::ApplyFn` handler = a small `MapExpr` tree-walk evaluator
  (`vm/interpreter/apply_fn.rs`): pops the recognizer result, folds its span text through
  the `FnDescriptor` (`HexConvert/NumberConvert/EnumWrap/BoxWrap/SpanCapture/Expr`) and the
  `MapExpr` grammar (literals, `Input`, `InputProp`, `FnCall` builtins, `BinOp`, `UnaryOp`).

**Method - measurement.** Cold paired A(VM)/B(closure), N=400 samples, two windows, both
orders (A,B then B,A), median + IQR, in a `tests/` probe. A = value-completed VM
(`Interpreter::new`+`run`, cold per parse); B = a hand-written zero-alloc closure combinator
(the parse-that closure-core shape: `recognize |> map(parse_hex)`). Shapes: projection-dense
(value.js hex-color -> `U32`, the `->` projection shape), recognizer-only (JSON free-tape:
`Span` push, no `ApplyFn`), and an amortized single-parse (one interpreter over 384 bytes).

**Oracle (ran first).** `ORACLE byte_identical_scalar = true` - for all 64 tokens the VM's
`ApplyFn`->`Value::U32(x)` equals the closure's `u32`, and recognizer spans match. Value
payloads are byte-identical (the VM wraps in `Value::U32`; the scalar is identical).

**Numbers (median of 800 cold samples each; ns; load 16->28, soft magnitudes).**

| shape | VM (A) median | closure (B) median | ratio closure/vm |
|---|---|---|---|
| PROJECTION-DENSE (hex->U32, ApplyFn), per-token cold parse | 12500 ns / 64 tok | 542 ns | **0.043x** |
| RECOGNIZER-ONLY (JSON free-tape), per-token cold parse | 11917 ns / 64 tok | 250 ns | **0.021x** |
| AMORTIZED single-parse (1 interpreter, 384 B) | 541 ns | 250 ns | **0.462x** |

**Reading.**
- Literal criterion "interpret ratio >=0.98x on JSON": **FAILS** (0.021x per-token, 0.46x
  amortized). Per the brief, this is **4D's honest negative - closures win the LL(1)/projection
  shape** at these input sizes.
- Decomposition (the load-bearing nuance): the **value-plane tax proper** is small -
  projection VM (12500) over recognizer VM (11917) is **~5%**, near noise. The value plane
  (`ApplyFn` + `MapExpr` eval) rides nearly free on the tape. The **dominant** gap is the VM's
  per-parse **setup allocation** - `Interpreter::new` allocates 8 `Vec`s + an `FxHashMap` per
  parse. Amortizing that over one 384-byte parse moves the ratio 0.021x -> **0.462x** (a ~22x
  jump), which is a recognizer/tape-substrate cost, i.e., PROBE-1's STRUCTURE plane, not the
  value floor F-4 asked about.
- Even amortized, the bare-inlined closure stays ~2.2x ahead - but B is a best-case zero-alloc
  monomorphized function; a real parse-that closure parser carries its own combinator state.

**Answer.** The value-completed shared VM **runs and is byte-identical**, but does **not** meet
the >=0.98x throughput bar on this shape -> **name-negative for 4C+4D viability on the pure
value-plane-vs-closure throughput question**. The blocker is NOT the value plane (it is ~free
over the tape); it is VM per-parse setup allocation. F-4 floor = measured; the recommendation
that follows is that shared-VM viability turns on amortizing/eliminating per-parse
`Interpreter` setup (tape-substrate work), not on the value plane.

---

## Deliverables & disposition
- Code patch: `PROTO-2.patch` (git diff vs `7cdbff4ef`; 11 files, +432/-9). Gate-0 source
  swaps + Cargo.toml edit; `Op::ApplyFn` + `fns` + `Value` arms + `apply_fn.rs` evaluator +
  compiler wiring; `tests/lock4_value_plane_probe.rs` harness.
- Everything RAN (no SPEC_ONLY): gate-0 build + cargo-tree cycle proof + negative control;
  value-plane oracle + cold paired timing. Verdict = **RAN_MEASURED**.

## Caveats (honest)
- Timings taken under load 16->28 on an 18-core host - absolute ns are noisy; the VM's larger
  working set is more scheduler-sensitive than a bare fn, so the ratios are pessimistic. Ranks
  (closures faster; setup dominates; ApplyFn ~free over tape) are robust across two runs.
- The closure core (B) is a hand-written stand-in for the parse-that closure shape on the
  projection, not parse_that's actual generated combinator; it is a best-case zero-alloc
  lower bound for closures. A parse_that-native A/B would tighten the amortized ratio.
- `divan` was not used (bbnf-ir carries no divan dev-dep; adding one exceeds the probe's
  parsimony budget). The cold/paired/two-window/both-order/median+dispersion methodology the
  divan-cold mandate requires is honored by the hand-rolled harness.
