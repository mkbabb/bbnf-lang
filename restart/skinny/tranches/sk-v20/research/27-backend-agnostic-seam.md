# 27 — The Backend-Agnostic Seam: Census and Totality Design

## Verdict

There are **two unrelated codegen trees**, each with its own answer to "where is the
backend-agnostic seam", and **neither is whole**:

- The full/totality tree (`crates/core/src/backend/`) carries a genuine, richly-
  factored `Emitter` trait + shared decision driver. But the **Rust backend — the
  only backend anyone parses with — overrides none of its per-node `emit_*` methods**.
  The Rust parse path is a *second, parallel* codegen path (the "shape dispatcher")
  that the driver's per-node traversal feeds and then throws away. The trait seam is
  live only for the dead TS/WASM scaffolding.
- The skinny/proving tree (`skinny/crates/codegen/`) has a real backend-agnostic IR
  (`BackendExpr`/`BackendIr` with a `BackendShape` cost model) but **no emitter trait
  at all**. `runtime_generator.rs` re-lowers that IR into ad-hoc `String` programs and
  stitches Rust source from `include_str!` templates. The CSS path bypasses
  `BackendExpr` outright and re-walks the raw grammar AST — a third codegen path.

So the one-codegen-path precept is violated three ways at once: (1) Rust-driver-seam
vs Rust-shape-dispatcher in the full tree; (2) full-tree `Emitter` trait vs skinny
`runtime_generator` String emit; (3) skinny `SinkOnly`/`GeneralDescent` lowering vs
skinny `css_scan` raw-AST derivation.

---

## Seam Census — Full Tree (`crates/core/src/backend/`)

### What is real

`crates/core/src/backend/emitter.rs:31` defines `trait Emitter { type Output; type Ctx;
… }` with ~40 `emit_*` methods spanning leaves, Seq, Alt, Repeat, Ref, binary ops,
value-manipulation, token-dispatch, delim-scan, rule-level, and a full prettify channel.
`crates/core/src/backend/driver/mod.rs:261` `compile_grammar<E: Emitter>` is the shared,
target-agnostic walk: it makes structural decisions (call strategy, alt strategy, sep_by
detection, materialization class) and calls `emit_*`. This is the textbook
IR→decision-layer→emitter shape. The shared driver state (`DriverState`, driver/mod.rs:52)
is genuinely backend-neutral.

The TS and WASM emitters honor the seam: `crates/core/src/backend/ts/emitter/mod.rs:49,78,
90,132,166` and `crates/core/src/backend/wasm/emitter/mod.rs:23,50,60,100,132` override
`emit_literal_match`, `emit_seq_grouped`, `emit_alt_dispatch`, `emit_repeat_many`,
`emit_call` — the full per-node surface.

### What is vestigial

**TS and WASM are dead weight.** The skinny proving tree (the campaign's actual forward
direction) has zero TS/WASM: `rg typescript|wasm|TsCode` over `skinny/crates/codegen/src/`
returns nothing. The full-tree `backend/ts/` (12 files) and `backend/wasm/` (12 files)
exist *only* so the per-node `Emitter` seam has more than one real implementor. They are
scaffolding that justifies the trait, not consumers of a shipping product.

**The Rust backend hollows the seam.** `crates/core/src/backend/rust/emitter/mod.rs:114`
`impl Emitter for RustEmitter` overrides only `emit_recognizer_family_kernel`,
`emit_fused_number_rule`, the pre/post hooks, `emit_rule_function`,
`emit_type_definitions`, `emit_grammar`, and the prettify family. **It overrides not one
of the per-node parse methods** (emit_literal_match, emit_seq_grouped, emit_alt_dispatch,
emit_repeat_many, emit_call, emit_skip/next, emit_map_expr, …). Those all inherit the
`Self::Output::default()` no-op the trait carries (emitter.rs:41, documented at
emitter.rs:19-26). The trait docstring itself confesses this: "The Rust backend routes
`parse()` through `dta_run` wholesale and discards per-rule bodies at
`emit_rule_function_impl`; it overrides none of these defaults."

The smoking gun is in `crates/core/src/backend/rust/emitter/grammar.rs`:
- `emit_rule_function_impl` (grammar.rs:85) returns `TokenStream::new()` — "Per-rule
  function emission is a no-op" (grammar.rs:77). The driver's painstaking
  `compile_node(&rule.body, …)` result (driver/mod.rs:333) is computed and dropped.
- `emit_type_definitions_impl` (grammar.rs:96) also returns `TokenStream::new()`.
- `emit_grammar_impl` (grammar.rs:119) ignores its `rule_functions` parameter:
  `let _ = rule_functions;` (grammar.rs:247). Instead it calls
  `super::shapes::emit_shapes_for_grammar(...)` (grammar.rs:233) and routes `parse()`
  through `emit_dispatcher` + per-`ShapeTag` functions.

So for Rust there are **two codegen paths**: the shared `Emitter`-driver path (whose
output is discarded), and the `shapes/` shape dispatcher (which actually emits the
parser). The shape dispatcher keys on `ShapeTag::{Object, Array, String, Number,
Keyword, Scalar, Pratt, Unordered, ArgList, Flat, Wrap, HRegex, AltDispatch}`
(`shapes/dispatcher/mod.rs:59-76`). That is the real Rust codegen, and it does not share
the per-node seam with TS/WASM. The driver still *runs* the per-node traversal for Rust —
burning compile time producing token streams that `emit_rule_function` throws on the floor.

This is precisely the failure mode the seam was meant to prevent: the seam exists, but the
backend whose performance matters bypassed it. The `where Self::Output: Default` bound
(emitter.rs:41) and the no-op defaults are the architectural scar tissue that *admits* the
bypass rather than forbidding it.

---

## Seam Census — Skinny Tree (`skinny/crates/`)

### What is real

`skinny/crates/ir/src/lib.rs:376` defines `enum BackendExpr` (Seq/Alt/RepeatLoop/
OptionalBranch/ByteLiteral/RegexProgram/CallRule/SpanMark/TapeEmit/DirectBuild/
ValueProject/Return) and `:352` `struct BackendIr` (`grammar_name, entry_rule,
recognizers, rules, shape_facts`). This **is** a backend-agnostic IR: no Rust tokens, no
target syntax. The lowering chain `ExprKind → BackendExpr` lives in
`skinny/crates/passes/src/lib.rs` (`fn lower_expr(grammar, expr_id) -> BackendExpr`).

A genuine decision layer exists: `BackendShape` (ir/src/lib.rs:361) enumerates
`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, and `CostFacts`
(`skinny/crates/ir/src/cost.rs:5`, fields `chosen: BackendShape` + projection) plus the
e-graph cost pass (`skinny/crates/passes/src/backend_egraph.rs`) pick the shape. This is
the "decision engine selecting an emitter specialization" the design wants — and it
already exists in skinny, not in the full tree.

### What is vestigial / broken

**There is no Emitter trait in skinny.** The decision layer chooses a `BackendShape`, but
emission is not a trait — it is `runtime_generator.rs` matching a hand-rolled
`LoweredArtifact` enum (`skinny/crates/codegen/src/runtime_generator.rs:19`) with only two
variants, `SinkOnly` and `Scan`, and rendering `String`:
- `render` (runtime_generator.rs:78) dispatches `SinkOnly → emit_sink_only`,
  `Scan → emit_scan`.
- `emit_sink_only` (runtime_generator.rs:89) sub-dispatches *again* on grammar shape:
  `if !sink_only.has_rule("value") → emit_general_descent` else JSON. JSON output is
  `include_str!("json_templates/generated.rs")` plus `json_sink_direct::render(...)`
  which returns `Result<String, String>` and builds source by `push_str`/`format!`
  (`skinny/crates/codegen/src/json_sink_direct/mod.rs:30`).

So the skinny "emitter" is **String concatenation of template files**, not a typed
emission seam. `BackendExpr` is re-lowered into `SinkOnlyExpr`
(`skinny/crates/codegen/src/lower/sink_only.rs:69`) — a *third* expression IR — before
String emit. The five `BackendShape` variants are not five emitter specializations; only
`SinkOnly` and a `GeneralDescent` String path are wired; `EagerTape`/`OffsetTape`/
`EventTape`/`CollapsedStage` have lowering stubs (`lower/eager_tape.rs`,
`offset_tape.rs`, `event_tape.rs`, `collapsed_stage.rs`) but the live `render` only
admits `SinkOnly` and `Scan`.

**The CSS path is a fork of the fork.** `lower_for_runtime` (runtime_generator.rs:40)
selects `Scan` by probing `css_scan::derive_program`, which "walks the raw parsed grammar
AST of `stylesheet.bbnf` … NOT the `BackendIr`/`SinkOnlyProgram`"
(`skinny/crates/codegen/src/lower/css_scan.rs:7-8`, `:128 derive_program`). CSS therefore
never enters `BackendExpr` — it is a parallel hand-written derivation keyed on CSS
structural rule names (`ruleBlock`/`atRuleBody`/`cssString`). This is exactly the
grammar-specific overfit Lock 14 forbids, wearing a "shape-derived, never a grammar name"
disclaimer (runtime_generator.rs:44-51) that the code contradicts: the probe succeeds only
for the CSS structural alphabet and fails on everything else.

---

## The Lowering Chain, End to End

**Full tree (Rust live path):**
`AST → GrammarIR → [driver compile_node → emit_* (discarded)] + [shapes dispatcher →
TokenStream]`. The bracketed first arm is vestigial; the second arm is the product.

**Full tree (TS/WASM dead path):**
`GrammarIR → driver compile_node → Emitter::emit_* → TsCode/String`. Honors the seam,
ships nothing.

**Skinny tree (live):**
`ExprKind → BackendExpr/BackendIr → [BackendShape cost decision] → SinkOnlyExpr |
CssScanProgram → String`. Agnostic through `BackendExpr`; collapses to String with no
emitter trait; CSS bypasses `BackendExpr` entirely.

Only `BackendExpr` (skinny) and the `Emitter` trait + driver (full) are genuinely
backend-agnostic. They live in different trees and do not compose. Everything downstream
of `BackendExpr` in skinny, and the entire live Rust path in the full tree, is Rust-
flavored String/TokenStream emission with no specialization seam.

---

## Where the Rust+ASM Hybrid Backend Slots In (Totality Design)

The ASM kernels already exist and are *already* backend-agnostic in the right way: the
SIMD primitives (`skinny/crates/bbnf-simd/src/aarch64/*`, scalar-ref + checkasm parity
per MASTER-PLAN H.W2) are leaf recognizers invoked by emitted code, not a forked codegen.
`BackendExpr::RegexProgram`/`SpanMark` and `Recognizer::SimdScan { mode, alphabet, site }`
(ir/src/lib.rs:416) are the agnostic representation of "a SIMD-accelerated scan at this
site." A Rust+ASM hybrid is therefore **not a new backend** — it is a *specialization of
the Rust emitter selected by the cost model*, sharing the same lowered IR.

The idiomatic transposition — one IR, one decision layer, emitter specializations:

```
                 BackendExpr / BackendIr        (the ONE agnostic IR — skinny owns it)
                          │
              ┌───────────┴────────────┐
              ▼                         ▼
        CostFacts / BackendShape   Recognizer::SimdScan facts
        (decision engine)          (kernel-applicability facts)
              │
              ▼
        trait Emitter { type Output; emit_<node>(…); }   ← unify on skinny's BackendExpr,
              │                                             NOT full-tree GrammarIR
   ┌──────────┼───────────────┬──────────────┐
   ▼          ▼               ▼              ▼
RustEmitter  RustAsmEmitter   TsEmitter    WasmEmitter
(scalar)     (Rust + asm-     (deferred)   (deferred)
             kernel splice)
```

The Rust+ASM emitter is `RustEmitter` with `emit_regex_program` / `emit_span_mark` /
`emit_recognizer` overridden to splice a NEON kernel call (or inline `core::arch::aarch64`
intrinsics) where the cost model + a target-feature predicate
(`target.avx512bw`-style gate already present in skinny passes) admit it, and to fall
through to the scalar body otherwise. It shares `emit_seq`/`emit_alt`/`emit_call`
verbatim. No codegen fork: the kernel splice is *one method override on the emitter*, the
same way TS overrides `emit_literal_match`.

### Required consolidations to reach totality

1. **Delete the full-tree shape dispatcher OR make it the emitter.** The Rust
   `shapes/` path (grammar.rs:233) must become `RustEmitter`'s per-node `emit_*` bodies,
   so the driver's `compile_node` output is *used*, not discarded. Remove the
   `where Self::Output: Default` no-op defaults (emitter.rs:41) — every emitter must
   implement every method or the seam is a lie. The `let _ = rule_functions;`
   (grammar.rs:247) and the `TokenStream::new()` shims (grammar.rs:85,116) are the
   markers of the bypass to excise.

2. **Promote `BackendExpr` to THE IR; introduce the Emitter trait in skinny.** Skinny's
   `BackendExpr` is the better agnostic IR (it already carries `SimdScan` recognizers and
   a cost model). The String-template emission in `runtime_generator.rs`/`json_sink_direct`
   should become a `trait Emitter { emit_byte_literal, emit_seq, emit_alt, emit_call,
   emit_regex_program, emit_span_mark, emit_tape_emit, emit_direct_build, … }` keyed 1:1 on
   `BackendExpr` variants, with `RustEmitter` and `RustAsmEmitter` as the first two impls.
   `BackendShape` selects emission *strategy parameters* (which tape), not which String
   template file to `include_str!`.

3. **Fold CSS into `BackendExpr`.** `css_scan::derive_program`'s raw-AST walk must be
   replaced by ordinary `ExprKind → BackendExpr` lowering plus a `Recognizer::SimdScan`
   over the CSS structural alphabet. CSS becomes a grammar that lowers like any other; the
   "balanced byte-skip" is `RepeatLoop` + a kernel-recognizer, not a bespoke
   `CssScanProgram`. This is the Lock-14 fix: zero CSS-named structural rules in the
   generic codegen.

4. **One tree.** The full-tree `Emitter`/driver and the skinny `BackendExpr`/cost model
   are two halves of one design built in two places. The full tree has the trait shape
   right and the IR wrong (GrammarIR is too Rust-coupled, hence the bypass); skinny has the
   IR right and no trait. Totality is: skinny's `BackendExpr` + cost model, carrying the
   full tree's `Emitter` trait pattern (minus the no-op defaults), with TS/WASM as deferred
   impls and Rust+ASM as the priority second impl beside scalar Rust.

---

## Bottom Line

The "shared decision driver + Emitter trait" the history claims does exist — but split
across two trees, with the live Rust backend bypassing it in one and no trait at all in the
other. The seam is real for nobody who ships. The Rust+ASM hybrid does not need a new
codegen path; it needs the existing one to stop forking. The work is consolidation, not
addition: one IR (`BackendExpr` + `SimdScan` recognizers), one decision layer (`CostFacts`/
`BackendShape` + a target-feature gate), one `Emitter` trait with no escape-hatch defaults,
and Rust-scalar / Rust+asm as sibling method-override specializations.
