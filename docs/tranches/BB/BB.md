# Tranche BB — Lazy Typed Pointer-Path Queries

BB opens on BA's settled substrate. BA has populated `StructRegistry`
for every production grammar, landed the backward container pointer,
and proved every `->` reaches the tape emitter under a same-commit
runtime consumer. BB layers lazy typed pointer-path queries on top
of that substrate — sonic-rs's `pointer!` ergonomics with compile-time
type validation and simdjson OnDemand's forward-skip laziness, but
derived from the grammar rather than declared by the user.

## Architectural thesis

1. **Pointer paths are grammar-typed at compile time.** A path
   expressed in the host language resolves to a typed accessor via
   IR type inference. Invalid paths (keys that don't exist at the
   grammar position, index into a non-list, type mismatch at the
   terminal) fail to compile. This is strictly stronger than
   sonic-rs, where the `pointer!` macro produces a runtime error
   on a bad path.
2. **Lazy skip uses BA's backward + forward container pointers.**
   A pointer descent skips off-path containers in O(1) via
   `child_off` (forward). A pointer ascent from an accessor up to
   its root uses `parent_off` (backward). No re-scan of document
   bytes for path traversal; all navigation is over the tape.
3. **Zero runtime allocation on traversal.** A path query's
   intermediate state is borrowed from the tape; no `Vec<NodeId>`
   is constructed. The terminal accessor returns a
   `NodeView<'p, TargetRule>` or the typed payload.
4. **Path construction is ergonomic and backend-agnostic.** The
   host-facing form uses a `path!` macro; the IR-level form is
   a typed `Path<Grammar, Target>` value. Different host backends
   (Rust, TS, Python bindings) receive isomorphic signatures
   per `feedback_isomorphic-api`.
5. **Pointer paths compose with egraph normalization.** A path
   rewrites during compilation: duplicate prefix elimination,
   redundant downcast elimination, path-fusion with adjacent
   accessors. BC later extends this surface with inferred rules.

## BA dependency

BB does not open until BA's handoff contract is fully met:

1. `StructRegistry` populated for JSON / CSS / Sheets / BBNF.
2. Backward container pointer landing with an active consumer.
3. IR audit pass at 100% `->` coverage.
4. 17-entry AU-baseline matrix at or above AU floor.
5. lightningcss / sonic-rs / simdjson parity harnesses green.

If BA's W3 backward-pointer decision lands on "sidecar column
rather than in-record", BB absorbs the change at W0 and the path
traversal consults the sidecar. If BA's `StructRegistry` is partial
at close, BB does not open; the remaining coverage lands under a
BA-carry wave, not a BB hedge.

## Invariants

1. No runtime path error. Every path that compiles returns a
   valid accessor for a parsed document matching the grammar.
2. No allocation during path traversal. Borrowed cursor state
   only.
3. No hand-written path resolver per grammar. The resolver is
   derived from the `StructRegistry` + grammar IR.
4. No duplicate traversal logic. The path resolver is the single
   consumer of the backward / forward pointer surface; no second
   parent-walk path exists.
5. Measurement at every wave boundary. A lazy-path micro-bench
   suite runs with the 17-entry matrix; regressions on either
   block wave close.

## Operational posture

1. Every wave opens with a measurement surface. The lazy-path
   micro-bench suite (extract 3 / 10 / 30 fields from citm /
   tailwind / sheets fixture) runs on every wave boundary with
   recorded deltas.
2. Every substrate addition ships with a same-commit consumer.
   BB does not ship a path compiler without a path consumer, and
   does not ship a path runtime without a path compiler.
3. The 17-entry AU-baseline matrix runs on every wave boundary.
   Path infrastructure must not regress the full-parse throughput
   that BA established.
4. Samply profiles land under `docs/benchmarks/profiles/BB/<wave>/`
   before and after.

## Hard gates

**Path-specific gates:**

- `Path::compile("$.pair[0].number")` against the JSON grammar
  returns an `f64` typed accessor at compile time.
- An invalid path (`$.foo.nope`) fails to compile with a grammar-
  aware error message.
- The lazy-path micro-bench suite beats sonic-rs `pointer!` by
  ≥ 20% on the "extract 3 fields from citm.json" benchmark and
  is at parity or better on "extract 30 fields".
- Zero heap allocations during path traversal (measured via
  `dhat`).

**Regression gates:**

- 17-entry AU-baseline matrix: no regression against BA close.
- lightningcss / sonic-rs / simdjson parity harnesses green.
- Workspace: pass ≥ BA close pass count, fail ≤ BA close fail
  count.

**Coverage gates:**

- Every Named rule in every production grammar has a compilable
  path accessor.
- The `path!` macro works from Rust, TS binding, and Python
  binding with isomorphic signatures.

## Reversal criteria

Inheriting BA's discipline:

1. **Wave-local 20% rule.** A wave that misses its declared gate
   by more than 20% reverts its own substrate at wave close.
2. **No regression on BA close.** Any regression of the 17-entry
   matrix reverts the responsible substrate immediately.
3. **No hedging forward.** A wave does not route its miss to a
   later wave of BB or to BC.
4. **Path complexity triggers reversal.** If path resolution
   compile-time grows super-linear in grammar size, the
   resolution algorithm reverts. BB's path compiler is bounded
   by `O(path_depth × grammar_rule_count)` with egraph
   normalization; anything worse reverts.

## Wave structure

Four waves. Every wave has a runtime call site at its landing
commit.

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Path IR + compile-time type check | BA close | planned |
| **W1** | [waves/W1.md](waves/W1.md) | Lazy traversal over backward/forward pointers | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | `path!` macro + host-binding isomorphism | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | FINAL — path-bench closure + parity harness extension | W2 | planned |

### W0 — Path IR + compile-time type check

A `Path` is an IR value: a sequence of `PathSegment` (key, index,
wildcard) typed against a grammar's `StructRegistry`. The compile
pass resolves each segment against the registry and produces a
`TypedPath<Grammar, Terminal>` with the terminal rule's
`TypeDesc` attached.

Runtime call site: at least one accessor in the JSON view layer
(`JsonValue::pointer_get<T>`) consumes a compile-time-resolved
`TypedPath` and returns `Option<T>`. The alternative runtime
path-resolution code path does not exist.

Bench delta gate: the accessor compiles in < 1 ms on the JSON
grammar for paths of depth ≤ 8.

### W1 — Lazy traversal

`TypedPath::traverse(tape: &Tape)` advances through the tape
using `child_off` to skip off-path containers and `parent_off`
(sparingly; lazy paths descend by default) to unwind. Terminal
accessors read the payload directly from the tape record.

Runtime call site: the path-extraction micro-bench
(`benches/path_extract.rs`) exercises lazy traversal on citm,
tailwind, and sheets fixtures and records ns / access.

Bench delta gate: extract-3-fields-from-citm beats sonic-rs
pointer! by ≥ 20%. Extract-30-fields is at parity or better.
17-entry AU-baseline matrix does not regress.

### W2 — `path!` macro + host-binding isomorphism

The Rust host exposes `path!["foo", "bar", 1]` with compile-time
type resolution; the TS binding exposes
`path(["foo", "bar", 1])` with the same signature shape under the
PyO3 / wasm-bindgen constraints; the Python binding exposes
`path(["foo", "bar", 1])` with runtime-typed response.
`feedback_isomorphic-api` in force.

Runtime call site: a round-trip end-to-end test at
`tests/path_isomorphic.rs` exercises all three host bindings
against the same fixture and asserts identical response shapes.

Bench delta gate: no regression; all three host bindings complete
the round-trip test without deviation.

### W3 — FINAL

Path-bench closure: the lazy-path micro-bench suite runs with a
full cross-grammar matrix (JSON / CSS / Sheets / BBNF × 3 / 10 /
30 fields × cold / warm). The parity harness extends with a
`pointer_parity` suite that asserts bbnf's pointer output matches
sonic-rs / simdjson / cssparser pointer output for every
compilable path. `FINAL.md` records deltas, reversals taken, and
any follow-on work routed to BC.

## External SOTA grounding

- **sonic-rs `pointer!` macro** — compile-time path construction
  with runtime traversal. BB strengthens to full compile-time
  type resolution. See
  [sonic-rs pointer docs](https://docs.rs/sonic-rs/latest/sonic_rs/macro.pointer.html).
- **simdjson OnDemand** — lazy forward-only iteration over a
  structural bitmap. BB applies the same skip discipline to
  bbnf's tape rather than to input bytes. See
  [OnDemand vs DOM performance](https://github.com/simdjson/simdjson/discussions/2201)
  and [Keiser 2024 ondemand paper](https://onlinelibrary.wiley.com/doi/10.1002/spe.3313).
- **JSONPath RFC 9535** — the IETF standard for JSON pointer
  paths. BB's `path!` syntax is compatible with a subset (key,
  index, wildcard) and is extensible beyond it where the grammar
  supports richer indexing. See
  [RFC 9535 — JSONPath](https://datatracker.ietf.org/doc/rfc9535/).

## BB handoff contract

BB does not close until all of the following are true:

1. Every Named rule in every production grammar has a compilable
   path accessor under the `path!` macro.
2. Lazy traversal beats sonic-rs `pointer!` by ≥ 20% on the
   3-field extraction benchmark across JSON / CSS / Sheets.
3. Invalid paths fail at compile time with grammar-aware errors.
4. Zero heap allocations during traversal (dhat-verified).
5. The 17-entry AU-baseline matrix at or above BA close.
6. `path!` macro isomorphic across Rust / TS / Python host
   bindings.

## Defensible floor

1. Path type-checking at compile time for JSON / CSS / Sheets /
   BBNF.
2. Lazy traversal with zero heap allocations.
3. At least 20% win over sonic-rs on small-subset extraction.
4. No regression on BA-close bench matrix.

Anything less is pointer paths without the ergonomic or
performance story that motivates them.

## Post-tranche review candidates

Decision at W3 close, not mid-wave:

- Whether `path!` should absorb richer JSONPath operators
  (`..`, `?(filter)`, `[*]`) in a successor tranche.
- Whether path fusion should extend into the egraph pass at BC
  open, or remain a BB-local optimisation.
- Whether the TS / Python bindings should share a single
  path-resolution binary via wasm.

## Indefatigability

When BB closes correctly, bbnf exposes a grammar-typed pointer
API that is both ergonomically parity with sonic-rs and
structurally stronger (compile-time errors, zero allocation,
cross-binding isomorphism). The tape-first substrate from Era IV
now supports both full parse (BA) and lazy extraction (BB) from
the same substrate, with no alternate code path.
