# Tranche BA — Lazy Typed Pointer-Path Queries over Struct Trees

BA opens on AZ's settled substrate. Where AZ dissolved the tape in
favor of direct-to-struct materialization — every `->` producing a
typed node in a grammar-derived struct tree via `project_types` and
a non-empty `StructRegistry` — BA layers lazy typed pointer-path
queries on top of that struct-only substrate. The ergonomic target
is sonic-rs's `pointer!` macro; the laziness discipline is simdjson
OnDemand's forward-skip; the strength is compile-time path
typechecking against the `StructRegistry`, isomorphic across Rust,
TypeScript, and Python bindings.

## Architectural thesis

1. **Pointer paths are grammar-typed at compile time.** A path
   expressed in the host language resolves to a typed accessor via
   IR type inference consulting the `StructRegistry` produced by
   AZ. Invalid paths (keys that do not exist at the grammar
   position, index into a non-list, type mismatch at the terminal)
   fail to compile with a grammar-aware error. This is strictly
   stronger than sonic-rs, which resolves `pointer!` at compile
   time against untyped JSON and fails at runtime on a bad path.
2. **The substrate is the struct tree, not a tape.** AZ's
   direct-to-struct pivot dissolved the 16-byte TapeRec and the
   backward container pointer. Paths navigate grammar-derived
   struct nodes whose field layout is the `project_types` return
   type. There is no tape cursor, no TapeOffset, no paired
   open/close record walk. Descent is field projection; ascent
   (where needed) resolves via a chosen parent-pointer strategy
   rather than a child-off/parent-off column.
3. **Laziness is lazy tail-clone, not lazy parse.** AZ materializes
   structs eagerly by default. OnDemand-style forward-skip survives
   as laziness of the *path tail*: an intermediate `NodeView<'p, T>`
   borrows into the already-materialized struct tree and only
   performs a payload read or sub-tree clone at the terminal.
   Where AZ retains a lazy-field mode (deferred `Value::String`
   slicing from the input, for example), BA's path evaluator skips
   unread siblings by never projecting their fields.
4. **Path construction is ergonomic and backend-agnostic.** The
   host-facing form is a `path!` macro (Rust proc-macro, TS
   template-literal tag, Python callable); the IR-level form is a
   typed `Path<Grammar, Target>` value. The three binding
   frontends share a single path IR, a single type checker, and
   a single traversal engine, per `feedback_isomorphic-api`.
5. **Paths compose with the egraph.** A path is rewritten at
   compile time under duplicate-prefix elimination, redundant
   downcast removal, and adjacent-accessor fusion. BB later
   extends that rewrite surface with inferred rules; BA ships
   the initial small, hand-authored rule set.

## AZ dependency (hard opening gate)

BA does NOT open until AZ closes. AZ is the tranche that dissolves
the tape, activates `project_types` across the fleet, populates
`StructRegistry` for every production grammar, and lands the
direct-to-struct emitter as the singular materialization path. BA
inherits that substrate and adds navigation ergonomics atop it.

AZ's handoff contract that BA consumes:

1. `StructRegistry` populated for JSON, CSS L4, Sheets, BBNF. A
   partial registry at AZ close **blocks** BA open; the remaining
   coverage lands under an AZ-carry wave, not a BA hedge.
2. Every `->` in every production grammar reaches the direct-to-
   struct emitter (`push_*` on the struct builder, not on a tape).
   An IR audit pass holds 100% coverage.
3. The tape path is fully deleted — not feature-gated, not
   conditional. No orthogonal codepath remains
   (`feedback_no-orthogonal-codepaths`).
4. 17-entry AU-baseline matrix at or above AU floor on the
   direct-to-struct substrate; lightningcss / sonic-rs / simdjson /
   serde_json / cssparser parity harnesses green.

If any of the above is missing at AZ close, BA remains closed and
re-plans against the residual gap. BA does not open on a partial
substrate.

## Invariants

1. **No runtime path error.** Every path that compiles returns a
   valid accessor for a parsed document matching the grammar.
2. **Zero heap allocation during traversal** (dhat-verified).
   Intermediate `NodeView` state borrows into the struct tree.
   The terminal may allocate only if the caller requests an owned
   clone; default return is borrow-shaped.
3. **No hand-written path resolver per grammar.** The resolver is
   derived from `StructRegistry` + grammar IR. One traversal engine
   serves every production grammar.
4. **No duplicate traversal logic.** A single path executor. No
   second "untyped fallback" path; a path that cannot typecheck
   does not run.
5. **Cross-binding isomorphism.** The Rust `path!`, the TS
   template-literal tag, and the Python callable share identical
   signatures up to host-language syntax. `feedback_isomorphic-api`
   is enforced.
6. **Typed-materialization invariant preserved.** Every `->` in
   the grammar reaches the emitter under AZ; BA only *consumes*
   the struct tree, it never bypasses or flattens it.
   `feedback_typed-materialization-invariant` in force.
7. **Measurement at every wave boundary.** Lazy-path micro-bench
   suite runs alongside the 17-entry AU-baseline matrix; regression
   on either blocks wave close.

## Hard gates

**Path-specific gates:**

- `path!("$.pair[0].number")` against the JSON grammar returns an
  `f64`-typed accessor at compile time.
- An invalid path (`path!("$.foo.nope")` where `foo` has no field
  `nope` on the resolved struct) fails to compile with a
  grammar-aware error naming the offending segment, the struct
  type it was resolved against, and the valid alternatives at
  that position.
- Lazy-path micro-bench suite beats sonic-rs `pointer!` by ≥ 20%
  on "extract 3 fields from citm.json" and reaches parity or
  better on "extract 30 fields". Parity or better against
  simdjson OnDemand on a matched 3-field extraction workload.
- Zero heap allocations during path traversal (dhat-verified on
  every fixture in the micro-bench suite).
- Compile-time error messages include the full path segment chain
  and the struct position where resolution failed; reviewed
  against the IR audit coverage matrix.

**Regression gates:**

- 17-entry AU-baseline matrix: no regression against AZ close.
- lightningcss / sonic-rs / simdjson / serde_json / cssparser
  parity harnesses green at every wave boundary.
- Workspace suite: pass count ≥ AZ close, fail count ≤ AZ close.

**Coverage gates:**

- Every Named rule in every production grammar has a compilable
  path accessor.
- `path!` macro works from Rust, TS binding, and Python binding
  with isomorphic signatures.

## Wave structure

Four waves. Every wave ships a same-commit runtime consumer; no
ledger-only close.

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Path IR + type checker + parent-pointer micro-bench | AZ close | planned |
| **W1** | [waves/W1.md](waves/W1.md) | Lazy traversal engine + `path!` macro + per-grammar bench | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | Host-binding isomorphism (TS + Python) + e2e integration | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | FINAL — measurement matrix + parity harness + handoff to BB | W2 | planned |

### W0 — Path IR + type checker + parent-pointer decision

A `Path` is an IR value: a sequence of `PathSegment` (key, index,
wildcard) typed against a grammar's `StructRegistry`. The type
checker resolves each segment against the registry and produces a
`TypedPath<Grammar, Terminal>` with the terminal rule's `TypeDesc`
attached. W0 also runs a parent-pointer micro-bench across the
three candidate strategies (in-struct pointer, root-traversal,
sidecar index) on citm / tailwind / sheets fixtures and selects
the default; the pick is measurement-driven, not declared.

No path *execution* yet — W0 ships types, type errors, and the
parent-pointer decision only.

### W1 — Lazy traversal engine + `path!` macro

The traversal engine advances a `TypedPath` over the struct tree:
field projection for descent, parent-pointer strategy for ascent.
The `path!` proc-macro expands at the call site to a typed
accessor consuming a `NodeView`. Micro-bench suite per grammar
(citm, tailwind, sheets, bbnf) records ns/access at 3 / 10 / 30
field extraction.

### W2 — Host-binding isomorphism

The TypeScript binding exposes a template-literal tag (`` path`$.a.b` ``)
and the Python binding a callable (`path("$.a.b")`). Both share the
Rust path IR via wasm-bindgen / PyO3. A round-trip e2e test exercises
identical fixtures across all three host bindings and asserts
identical response shapes.

### W3 — FINAL

Full measurement matrix: JSON / CSS / Sheets / BBNF × 3 / 10 / 30
fields × cold / warm. Pointer-parity harness asserts bbnf's pointer
output matches sonic-rs / simdjson / cssparser / lightningcss
pointer output for every compilable path. `FINAL.md` records
deltas, reversals, and any work routed to BB.

## Reversal criteria

Inheriting AZ's discipline:

1. **Wave-local 20% rule.** A wave that misses its declared gate
   by more than 20% reverts its own substrate at wave close; it
   does not ship a hedged substrate that a later wave "fixes".
2. **No regression on AZ close.** Any regression of the 17-entry
   matrix reverts the responsible substrate immediately at the
   offending commit.
3. **No hedging forward.** A wave does not route its miss to a
   later BA wave or to BB. Misses reset the wave.
4. **Path-complexity reversal.** If path resolution compile-time
   grows super-linear in grammar size, the resolution algorithm
   reverts. BA's path compiler is bounded by
   `O(path_depth × grammar_rule_count)` with egraph normalization;
   anything worse reverts.

## Q1 resolution — parent-pointer design

Ascent from a `NodeView` to its container is required for
wildcard-sibling operators and for path-fusion rewrites that
anchor at a known ancestor. Under the tape substrate the answer
was "backward container pointer in TapeRec" (a column). Under
the struct tree there are three candidates:

1. **In-struct parent pointer.** Each struct node carries
   `parent: Option<&Parent>` or `parent_id: NodeId`.
   *Pro*: O(1) ascent, natural locality.
   *Con*: inflates every node by 8 B (pointer) or 4 B (NodeId)
   whether the feature is used or not; cycle / lifetime
   gymnastics under borrow-checker rules; violates "no bloat when
   the feature is unused."
2. **Root traversal.** Every path resolves from root; ascent
   is not offered. Siblings reached via ancestor memoization.
   *Pro*: zero node bloat; matches AZ's bare struct discipline.
   *Con*: descent cost on every path access; memoization is
   per-query, not per-document.
3. **Hybrid sidecar.** A parallel index (`HashMap<NodeId, NodeId>`
   or `Vec<NodeId>` indexed by depth-first order) maintained
   alongside the struct tree, populated only when a query
   requests ascent. Preserves Q1's original "sidecar column"
   intent translated to struct-tree: a sidecar tree / parallel
   index parallel to the struct, not embedded in every struct
   node.
   *Pro*: zero node bloat, O(1) ascent when the feature is
   active, matches the "no bloat when feature unused" discipline
   the user preserved through the rename.
   *Con*: index build cost on first ascent query; eviction
   policy to manage.

**Recommendation**: hybrid sidecar. It is the natural translation
of the Q1 user decision to the struct-tree substrate. Final pick
deferred to BA.W0's parent-pointer micro-bench on citm / tailwind /
sheets fixtures; the micro-bench results land in the W0 close
commit. If the micro-bench shows root traversal dominating for the
common-path distribution, BA adopts root traversal and documents
the inversion.

## Q2 resolution — StructRegistry close

The `StructRegistry` must be populated for JSON, CSS L4, Sheets,
and BBNF at AZ close. BA's type checker dispatches every path
segment through it; a missing registry entry is a hard compile
error, not a fallback. If AZ.W1 closes with any production grammar
uncovered, BA.W0 does not open. The remaining grammar lands under
an AZ-carry wave and BA re-plans the schedule.

## Cross-binding `path!` macro

The `path!` surface is the user's sole ergonomic entry point. The
architecture is a three-frontend, shared-IR, shared-executor
design:

- **Rust proc-macro** (`crates/derive/src/path_macro.rs`): expands
  `path!("$.users[0].name", Users)` at compile time to a typed
  accessor. The macro parses the path string with the bespoke
  `bbnf-regex` parser, resolves against the compile-time
  `StructRegistry` from the derive input, and emits a
  `TypedPath<Users, StrSlice>` literal plus a call site against
  `NodeView::project<TypedPath>`.
- **TypeScript template-literal tag** (`crates/bbnf-path-ts/`):
  a thin WASM crate exposes the same path IR + type checker via
  `wasm-bindgen`. The TS frontend is `` path`$.users[0].name` ``
  with the tag invoking the WASM-exported `compile_path` at load
  time (build-time for bundlers that constant-fold) and producing
  a typed accessor closure over the shared struct-tree view.
- **Python callable** (`crates/bbnf-path-py/`): PyO3 binding
  exposing `path("$.users[0].name")` returning a typed path
  object. Runtime-typed in Python per the language's idioms, but
  the underlying validation and execution are the same Rust path
  IR.

All three frontends are thin adaptors. The path IR, type checker,
and executor live in `crates/core/src/path/` and are compiled
once. Signature isomorphism per `feedback_isomorphic-api`: same
argument shape (path string, optional target type), same return
shape (typed accessor over `NodeView`), same error taxonomy
(grammar-aware compile error vs. language-native equivalent).
`feedback_wasm-subcrate-pattern` places the WASM binding as a
workspace sub-crate under `crates/bbnf-path-ts/`, isomorphic to
the Python binding location.

## Critical files

| File | Role |
|---|---|
| `crates/core/src/path/ir.rs` | Path IR — `Path`, `PathSegment`, `TypedPath` |
| `crates/core/src/path/type_check.rs` | Type checker consuming `StructRegistry` |
| `crates/core/src/path/executor.rs` | Lazy traversal engine over struct tree |
| `crates/core/src/path/ascent.rs` | Parent-pointer strategy (hybrid sidecar per W0 pick) |
| `crates/ir/src/passes/path_check.rs` | IR pass validating all compile-time paths against the grammar |
| `crates/derive/src/path_macro.rs` | Rust `path!` proc-macro |
| `crates/bbnf-path-ts/` | WASM sub-crate: TS template-literal tag frontend |
| `crates/bbnf-path-py/` | PyO3 sub-crate: Python callable frontend |
| `benches/path_extract.rs` | Per-grammar lazy-path micro-bench |
| `tests/path_parity.rs` | Parity harness vs. sonic-rs / simdjson / cssparser / lightningcss |
| `tests/path_isomorphic.rs` | Round-trip across Rust / TS / Python bindings |

## Risk register

1. **Path explosion under nested pattern matching.** Wildcard (`[*]`)
   segments across deep CSS / tailwind structs can expand to large
   typed path sets at compile time. Mitigation: path-fusion rewrites
   in the egraph; wildcard depth cap in the type checker with a
   grammar-aware diagnostic.
2. **Macro hygiene across three host languages.** Rust proc-macro
   hygiene diverges from TS template-tag identifier resolution and
   from Python's runtime-only binding. Mitigation: shared
   compile-time validator in WASM; each frontend is a thin adaptor
   that cannot reintroduce unhygienic substitution.
3. **Partial `StructRegistry` coverage edge cases.** A Named rule
   present in grammar but absent from registry breaks path
   resolution silently if not caught. Mitigation: `path_check.rs`
   IR pass holds hard coverage; missing entries fail the build at
   AZ close — not here.
4. **Parent-pointer strategy reversal mid-tranche.** W0's micro-bench
   pick may prove wrong under a broader W1 workload. Mitigation:
   the parent-pointer module (`ascent.rs`) is strategy-pluggable
   per `feedback_pluggable-components`; a reversal swaps the
   strategy without disturbing the executor.

## Defensible floor

Minimum BA delivers, even if stretch scope slips:

1. Path typechecking at compile time for JSON and CSS L4 in Rust
   only.
2. Lazy traversal engine with zero heap allocations on those two
   grammars.
3. ≥ 20% win over sonic-rs on 3-field citm.json extraction.
4. No regression on the AZ-close 17-entry AU-baseline matrix.

Sheets / BBNF grammar support and TS / Python binding isomorphism
are stretch. The floor is the useful-in-Rust promise; the stretch
is the full isomorphic surface.

## External SOTA grounding

- **sonic-rs `pointer!` macro** — compile-time path construction,
  runtime type validation. BA strengthens to compile-time type
  resolution against `StructRegistry`.
- **simdjson OnDemand** — lazy forward-only iteration over a
  structural bitmap. BA applies the same skip discipline to the
  struct tree rather than to input bytes: paths never project
  fields they do not reach.
- **JSONPath RFC 9535** — BA's `path!` syntax is compatible with
  a subset (key, index, wildcard) and extensible beyond it where
  the grammar supports richer indexing.
- **lightningcss typed accessors** — structural parity target
  for the CSS L4 path surface; BA's paths resolve to the same
  typed shapes lightningcss exposes on its AST,
  `feedback_beat-lightningcss-target` in force.

## Indefatigability

When BA closes correctly, bbnf exposes a grammar-typed pointer API
that is ergonomically on par with sonic-rs and structurally
stronger: compile-time path typechecking, zero allocation on
traversal, cross-binding isomorphism, and parity-or-better on
every measured extraction workload. The struct-only substrate from
AZ now supports both full materialization (AZ) and lazy extraction
(BA) from the same substrate, with no alternate code path.
