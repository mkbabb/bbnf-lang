# Tranche BA — Lazy Typed Pointer-Path Queries over Struct Trees

> **Refined 2026-04-28** — per `docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`
> §2 + Audit-5 Proposal D, BA collapses to one wave: Path IR + type
> checker + `path!` macro + zero-allocation traversal + host bindings
> (Rust + TS + Python) under one isomorphic-API close. The
> parent-pointer-vs-root-traversal micro-bench folds into the opening
> preflight (15-min preface), retiring the separate BA.W-1 / BA.W0
> wave letters.

BA opens on AZ-II's settled substrate. Where AZ-II dissolved the tape in
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
2. **The substrate is the struct tree, not a tape.** AZ-II's
   close dissolved the 16-byte TapeRec and the backward container
   pointer fleet-wide (AZ-I dissolved it for JSON / CSS L4 /
   Sheets; AZ-II extended the dissolution to BBNF self-hosting).
   Paths navigate grammar-derived struct nodes whose field layout
   is the `project_types` return type. There is no tape cursor, no
   TapeOffset, no paired open/close record walk. Descent is field
   projection; ascent (where needed) resolves via a chosen
   parent-pointer strategy rather than a child-off/parent-off
   column.
3. **Laziness is lazy tail-clone, not lazy parse.** AZ materialises
   struct trees eagerly for every grammar by default (JSON / CSS
   / Sheets at AZ-I close; BBNF at AZ-II close). OnDemand-style
   forward-skip survives as laziness of the *path tail*: an
   intermediate `NodeView<'p, T>` borrows into the already-
   materialized struct tree and only performs a payload read or
   sub-tree clone at the terminal. Where AZ-I retains a lazy-
   field mode (deferred `Value::String` slicing from the input,
   for example), BA's path evaluator skips unread siblings by
   never projecting their fields.
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

BA does NOT open until AZ-II closes. AZ-II is the tranche that dissolves
the tape, activates `project_types` across the fleet, populates
`StructRegistry` for every production grammar, and lands the
direct-to-struct emitter as the singular materialization path. BA
inherits that substrate and adds navigation ergonomics atop it.

AZ-II's handoff contract that BA consumes:

1. `StructRegistry` populated for JSON, CSS L4, Sheets, BBNF. A
   partial registry at AZ-II close **blocks** BA open; the remaining
   coverage lands under an AZ-II carry wave, not a BA hedge.
2. Every `->` in every production grammar reaches the direct-to-
   struct emitter (`push_*` on the struct builder, not on a tape).
   An IR audit pass holds 100% coverage.
3. The tape path is fully deleted — not feature-gated, not
   conditional. No orthogonal codepath remains
   (`feedback_no-orthogonal-codepaths`).
4. 17-entry AU-baseline matrix at or above AU floor on the
   direct-to-struct substrate; lightningcss / sonic-rs / simdjson /
   serde_json / cssparser parity harnesses green.

If any of the above is missing at AZ-II close, BA remains closed and
re-plans against the residual gap. BA does not open on a partial
substrate.

## BA.W-1 — opening verification

BA gains a pre-wave handoff verification before BA.W0 creates any new
path API. This is not a runtime implementation wave; it is the gate
that proves BA is not layering typed paths over the legacy tape path
surface.

BA.W-1 must prove:

1. No tape imports or tape runtime re-exports remain in the public
   parse/path surface.
2. `StructRegistry` and `StructLayout` exist for JSON, CSS L4,
   Sheets, and BBNF.
3. Every production grammar reaches struct-only parse output.
4. BBNF bootstrap reproducibility is permanent after AZ-II close.
5. Legacy `runtime::path` is renamed, retired, or explicitly marked
   internal so BA's `TypedPath` cannot coexist ambiguously with the
   old tape path API.

Command packet:

```bash
rg -n 'struct TypedPath|enum PathError|PathSegment::Wildcard|fn type_check|AscentStrategy|path_check|parent_pointer_strategies' crates/core/src crates/ir/src crates/bbnf-path/src
cargo test --profile ax-iter -p bbnf --test path_type_errors
cargo bench -p bbnf --bench parent_pointer_strategies
```

If these commands cannot exist by BA open, BA stays closed and an
AZ-II carry wave owns the missing substrate. BA does not create a
fallback untyped resolver.

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

- 17-entry AU-baseline matrix: no regression against AZ-II close.
- lightningcss / sonic-rs / simdjson / serde_json / cssparser
  parity harnesses green at every wave boundary.
- Workspace suite: pass count ≥ AZ-II close, fail count ≤ AZ-II close.

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
| **W0** | [waves/W0.md](waves/W0.md) | Path IR + type checker + parent-pointer micro-bench | AZ-II close | planned |
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
2. **No regression on AZ-II close.** Any regression of the 17-entry
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
and BBNF at AZ-II close. BA's type checker dispatches every path
segment through it; a missing registry entry is a hard compile
error, not a fallback. If AZ-I.W1 closes with any production grammar
uncovered, BA.W0 does not open. The remaining grammar lands under
an AZ-II carry wave and BA re-plans the schedule.

## Cross-binding `path!` macro

The `path!` surface is the user's sole ergonomic entry point. The
architecture is a three-frontend, shared-IR, shared-executor
design. The shared core is `crates/core/src/path/`; each host
binding is a thin adaptor compiling against that same core.

### Frontend surfaces

- **Rust proc-macro** (`crates/bbnf-path/src/path_macro.rs`): expands
  `path!("$.users[0].name", Users)` at compile time to a typed
  accessor. The macro parses the path string with the bespoke
  `bbnf-regex` parser, resolves against the compile-time
  `StructRegistry` from the derive input, and emits a
  `TypedPath<Users, StrSlice>` literal plus a call site against
  `NodeView::project<TypedPath>`. A malformed path fails the
  surrounding `cargo build` with a `proc_macro2::Span`-anchored
  diagnostic pointing at the offending segment token.
- **TypeScript template-literal tag** (`crates/bbnf-path-ts/`):
  a `cdylib` WASM sub-crate exposes `compile_path(path, grammar)`
  and `execute_path(typed_path, node_view)` via `wasm-bindgen`.
  The TS frontend is `` path`$.users[0].name` `` with the tag
  invoking the WASM-exported validator at load time (build-time
  under bundlers with constant folding — Vite, esbuild, SWC) and
  producing a typed accessor closure over the shared struct-tree
  view. A malformed path throws a `PathError` carrying identical
  diagnostic fields to the Rust frontend.
- **Python callable** (`crates/bbnf-path-py/`): PyO3 binding
  exposing `path("$.users[0].name", grammar=Users)` returning a
  typed path object. Runtime-typed in Python per the language's
  idioms, but the underlying validation and execution dispatch
  through the same Rust path IR. A malformed path raises
  `bbnf_path.PathError` carrying identical diagnostic fields.

### Shared core

| Module | Responsibility |
|---|---|
| `crates/core/src/path/ir.rs` | `Path`, `PathSegment`, `TypedPath<G, T>` — the IR shared across all three frontends |
| `crates/core/src/path/type_check.rs` | Single type-checker entry point; consumes `StructRegistry` |
| `crates/core/src/path/executor.rs` | Single traversal executor over the struct tree |
| `crates/core/src/path/ascent.rs` | `AscentStrategy` trait + picked default (hybrid sidecar per W0) |
| `crates/core/src/path/error.rs` | `PathError` with segment + struct + alternatives fields; rendered identically across frontends |

### Isomorphism contract

Signature isomorphism per `feedback_isomorphic-api` is enforced at
the signatures-test level. For every public call:

- Argument shape: (path string, optional grammar/target type).
- Return shape: typed accessor over `NodeView`, or a typed
  result in the Python case.
- Error taxonomy: `PathError { segment, struct_name, alternatives,
  reason }` — identical field set across Rust / TS / Python,
  rendered in each host's native error surface.

`feedback_wasm-subcrate-pattern` places the WASM binding as a
workspace sub-crate under `crates/bbnf-path-ts/`, isomorphic to
the Python binding location at `crates/bbnf-path-py/`. Neither
binding imports the other; both import the shared core.

## Critical files

| File | Role | Wave |
|---|---|---|
| `crates/core/src/path/ir.rs` | Path IR — `Path`, `PathSegment`, `TypedPath<G, T>` | W0 |
| `crates/core/src/path/type_check.rs` | Type checker consuming `StructRegistry` | W0 |
| `crates/core/src/path/ascent.rs` | Parent-pointer strategy (`AscentStrategy` trait + three impls; default picked per W0 micro-bench) | W0 |
| `crates/core/src/path/error.rs` | `PathError` with segment / struct / alternatives / reason — rendered identically across frontends | W0 |
| `crates/ir/src/passes/path_check.rs` | IR pass validating all compile-time paths against the grammar | W0 |
| `crates/core/src/path/executor.rs` | Lazy traversal executor over the struct tree | W1 |
| `crates/core/src/path/view.rs` | `NodeView<'p, T>` — borrowed cursor state | W1 |
| `crates/bbnf-path/src/path_macro.rs` | Rust `path!` proc-macro | W1 |
| `benches/path_extract.rs` | Per-grammar lazy-path micro-bench | W1 |
| `tests/path_parity.rs` | Parity harness vs. sonic-rs / simdjson / cssparser / lightningcss | W1 |
| `crates/bbnf-path-ts/` | WASM sub-crate: TS template-literal tag frontend | W2 |
| `crates/bbnf-path-py/` | PyO3 sub-crate: Python callable frontend | W2 |
| `tests/path_isomorphic.rs` | Round-trip across Rust / TS / Python bindings | W2 |
| `docs/tranches/BA/FINAL.md` | BA close summary + handoff contract to BB | W3 |

## Risk register

1. **Path explosion under nested pattern matching.** Wildcard (`[*]`)
   segments across deep CSS / tailwind structs can expand to large
   typed path sets at compile time; a naive type checker visits
   every resolved variant. Mitigation: path-fusion rewrites in the
   egraph unify shared prefixes; wildcard depth cap in the type
   checker with a grammar-aware diagnostic that names the offending
   depth; the W0 bench includes a wildcard-heavy tailwind path to
   expose explosion before it reaches W1.
2. **Macro hygiene across three host languages.** Rust proc-macro
   hygiene diverges from TS template-tag identifier resolution and
   from Python's runtime-only binding. A path-string literal that
   contains identifier-like fragments could resolve differently in
   each host. Mitigation: path parsing happens entirely inside the
   shared WASM validator; each frontend is a thin adaptor that
   passes the raw string to that validator and cannot reintroduce
   host-specific substitution. The signatures test in W2 asserts
   equivalent resolution across frontends on an adversarial fixture.
3. **Partial `StructRegistry` coverage edge cases.** A Named rule
   present in grammar but absent from registry breaks path
   resolution silently if not caught. Mitigation: `path_check.rs`
   IR pass holds hard coverage; missing entries fail the build at
   AZ-II close, not at BA compile time. BA treats a missing registry
   entry as a hard-fail, never a fallback. `feedback_no-workarounds`
   in force.
4. **Parent-pointer strategy reversal mid-tranche.** W0's micro-bench
   pick may prove wrong under a broader W1 workload. Mitigation:
   the parent-pointer module (`ascent.rs`) is strategy-pluggable
   per `feedback_pluggable-components`; the `AscentStrategy` trait
   is the reversal seam. A W1-triggered reversal swaps the
   implementation without disturbing the executor or any frontend;
   the reversal commit records the W1 measurement that forced it.
5. **Host-binding build drift.** The WASM and PyO3 sub-crates can
   drift out of sync with the core IR if their test harnesses are
   run only at W2 close. Mitigation: `tests/path_isomorphic.rs`
   runs on every wave boundary from W1 onward (smoke) with full
   enforcement at W2; signature drift is caught the commit it
   lands.

## Defensible floor

Minimum BA delivers, even if stretch scope slips:

1. Path typechecking at compile time for JSON and CSS L4 in Rust
   only. Every Named rule in those two grammars has a compilable
   `path!` accessor.
2. Lazy traversal engine with zero heap allocations on those two
   grammars (dhat-verified).
3. ≥ 20% win over sonic-rs on 3-field citm.json extraction;
   parity or better on 30-field.
4. Grammar-aware compile-time diagnostics on malformed paths:
   offending segment + struct + valid alternatives.
5. No regression on the AZ-close 17-entry AU-baseline matrix.

Sheets and BBNF grammar support are stretch. TS and Python
binding isomorphism are stretch. The floor is the useful-in-Rust
promise across two production grammars; the stretch is the full
isomorphic surface across four grammars and three host languages.
BA does not ship floor-only as a rename of the full tranche — a
floor-only close is an explicit reversal, not a ledger-ended
partial success.

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
