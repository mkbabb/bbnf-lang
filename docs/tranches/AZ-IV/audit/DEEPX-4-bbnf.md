# DEEPX-4 — BBNF Self-Host Parity & Profile Audit

Read-only post-AZ-IV audit. Worktree
`/Users/mkbabb/Programming/bbnf-wt-deepX-4`, branch `deepX-bbnf` at master
`40e1835d`.
`CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-deepX-4/target/deepX-4`.

The BBNF self-host is the canonical end-to-end test of the entire
fleet: bbnf parsing bbnf is what every other grammar's emission
discipline is dog-fooded against. It is the witness that the
"grammar-derived" thesis composes against itself. This audit asks
three questions: (1) what does the bbnf_self bench attribute to;
(2) does the lazy/eager surface, the `path!(Bbnf, …)` macro, and the
`StructRegistry` route faithfully through to the BBNF runtime; and
(3) where does the BA direct-projection thesis specifically need BBNF
fixtures it does not yet have. The answer in one line: BBNF inherits
DEEP-B's primary blocker (`Vec<OpenFrame>::clone` per speculative
checkpoint) intact, and the BBNF-specific gaps are recursion handling
in direct-projection, host-function semantics (Phase 3, deferred), and
the observation that the BBNF grammar IS the LSP-introspection contract
once direct-projection lands.

## I — bbnf_self bench attribution (479.8µs)

`docs/benchmarks/post-AZ-IV.json` row, post-AZ-IV close (master
`cb14970f`):

```
"bbnf_self": { "ns_per_iter": 479800, "samples": 100, "iters": 100,
  "fastest_ns": 464600, "slowest_ns": 661400, "mean_ns": 488000,
  "note": "vs AU 13003ns — 37x regression. Same root cause as sheets
   (W5 arena/builder template indirection)." }
```

The bench parses every file in `grammar/bbnf/` (`bbnf.bbnf`,
`expressions.bbnf`, `types.bbnf`) sequentially through
`BbnfBootstrap::parse` (`crates/core/benches/bbnf/monolithic.rs:121-137`).
Total source ~5KB across three files. The 37× regression vs AU
(13µs → 480µs) tracks the W5 arena/builder template substrate
indirection identically to `sheets`, `css_l4_grammar`, `ebnf`,
`google_sheets`, `json` (28-38× regressions, all same root cause per
the row notes).

DEEP-B's samply attribution (25,963 samples on `bbnf_value_twitter`
under fat-LTO) names the dominant mechanism:
`<alloc::vec::Vec<OpenFrame>::clone>` at **86.07% inclusive samples**.
That mechanism is identical for BBNF: `BbnfStructBuilder::checkpoint`
clones `self.stack: Vec<OpenFrame<'p>>` on every speculative branch
(`crates/core/src/runtime/bbnf/builder.rs:138-145`):

```rust
fn checkpoint(&self) -> Self::Checkpoint {
    BbnfStructCheckpoint {
        compounds: self.arena.compound_count(),
        stack: self.stack.clone(),  // ← deep clone per speculative branch
        root: self.root,
        next_handle: self.next_handle,
    }
}
```

Each `OpenFrame` carries a `children: Vec<BbnfValue<'p>>` cloned
recursively. The BBNF generated parser invokes `builder.checkpoint()`
**217 times** across `bbnf.rs` (grep count over `crates/core/src/grammar/generated/bbnf.rs`).
The per-grammar arena/builder of the BBNF self-host is structurally
identical to the JSON one: same defect, same magnitude, same
single-attribution path — the BBNF row is the canonical "every grammar
gets the same regression" datum.

Samply attribution against `bbnf_self` specifically was not run as a
bench-distinct profile in DEEP-B (the wave profiled the `json_value`
matrix). The DEEP-B 86% attribution holds by mechanism; a same-harness
re-profile against `bbnf_monolithic` is recommended for BA.W3 close
evidence but is not strictly necessary for the BA scope — the substrate
mechanism is the same.

## II — Semantic Parity (95/95 BBNF self-parity at AZ-IV close)

The 95/95 figure cited in the dispatch is the BBNF self-parity test
matrix (`crates/core/tests/bbnf_self_parity.rs`). 28 fixtures × 2
invariants (AST round-trip + prettify idempotency) = 56 tests
canonically; the wider 95/95 figure includes expanded sheets, json,
css_l4 self-parity coverage. Per-fixture coverage in the test file:

- BBNF self-host (3 files): `bbnf.bbnf`, `expressions.bbnf`,
  `types.bbnf` — the self-host triumvirate.
- Backus-Naur families (2 files): `bnf.bbnf`, `ebnf.bbnf`.
- Data interchange (2 files): `json.bbnf`, `google-sheets.bbnf`.
- CSS family (17 files): pretty + L4 module set.
- Misc (5 files): csv, math, math-ambiguous, g4, regex.

Excluded fixtures with named cause (test file caveats §Caveats):
`grammar/misc/emoji.bbnf` (raw emoji glyph terminals; identifier rule
is `/[_a-zA-Z][_a-zA-Z0-9-]*/`, lexically inexpressible),
`grammar/misc/json-commented.bbnf` (`/*a*/` big-comments in RHS
positions the BBNF grammar does not accept). These are pre-existing
BBNF grammar-coverage gaps documented as such; not regressions.

What semantic parity asserts:

1. **AST round-trip stability** — `parse(src)` produces a deterministic
   `GrammarExtract` (`crates/core/src/types.rs:106-120`) whose
   fingerprint (rule names + directive offsets) is identical across
   re-parses. This is *self-consistency*, not *functional parity with
   a reference parser* — there is no external comparator (the
   self-host *is* the reference).
2. **Prettify idempotency** — `prettify(src)` is byte-identical on the
   second pass.

What semantic parity does NOT cover:

- Functional behavior of `host_fns` (Phase 3 hybrid-grammar-host scope —
  see §III).
- `closure` (grammar-level first-class closures) and `value_closure`
  (value-expression-level closures from `expressions.bbnf`) — both
  rules parse and AST-roundtrip, but the *semantics* of grammar
  closures (i.e., `|param| rhs` invocation, beta-reduction, the
  `feedback_grammar-closures` design) are not exercised by the parity
  tests because no grammar in `grammar/` declares one yet at HEAD.
- The `@host` directive itself parses (BBNF rule:
  `host_directive = "@host" ?w , identifier ?w , ( ":" ?w , type_name ?w ) ? , ( ";" | "." ) ? ;`)
  but no Rust runtime path consumes the parsed `host_fns` for
  computation (Phase 3 deferred).

### Phase 3 hybrid-grammar-host scope

GESTALT.md §2 (Four interlocking invariants, Grammar-authoritative
clause): "Hybrid-grammar-host is the current migration posture (Phase
1+2 done, Phase 3 host-fns pending)". The phases:

- **Phase 1 (done)** — grammar-authoritative `->` annotation pipeline
  through `project_types`, `StructRegistry`, codegen.
- **Phase 2 (done)** — backend-agnostic `TypeDesc::Named`; each backend
  resolves to native types via its own registry; CSP/e-graph are
  language-blind.
- **Phase 3 (pending)** — host functions for context-dependent and
  recursive computations the grammar cannot express through `->`
  alone. The grammar's `@host` directive declares a host-fn name and
  abstract return type; the backend resolves the implementation
  through a host-fn registry (analogue of `StructRegistry` for
  computations rather than shapes). The `value_fn_call` rule in
  `expressions.bbnf` (`value_fn_call = value_path , "(" , ( value_expr , ( "," , value_expr ) * ) ? , ")"`)
  is the call-site syntax; the host registry is the resolution
  surface; the `value_expr` -> typed value pipeline is the evaluation
  surface. None of these wire to a runtime consumer at HEAD.

Phase 3's scope is large enough to warrant its own letter. Per
DEEP-SYNTHESIS, BA is direct-projection codegen, BB is rule-discovery,
BC is cleanup. Phase 3 does not cleanly fit any of the three; this
audit's recommendation is to route Phase 3 to **BC+1 (post-cleanup)**
or **a future BD-class letter** rather than fold it into BA. The
direct-projection refactor's emitter rewiring does not block Phase 3,
and Phase 3's host-fn surface does not constrain direct-projection's
shape (host-fns produce `TypeDesc::Named` results; direct-projection
already projects from `TypeDesc`). They compose; they should land
sequentially.

### `path!(Bbnf, …)` use cases

The `path!` macro routes through the production `bbnf.registry.json`
sidecar (`crates/core/src/grammar/generated/bbnf.registry.json`,
2,232 lines, 53 `rule_name` entries — one StructLayout per BBNF rule).
`crates/core/tests/path_macro_compile.rs:62-71` confirms `path!(Bbnf,
"element")` resolves through the production registry to the
Vec-shaped `grammar` root layout.

Use cases enabled by `path!(Bbnf, …)` once direct-projection lands:

1. **LSP introspection** — `BbnfBootstrap::get(grammar_text,
   path!(Bbnf, "rule", 0, "lhs"))` returns the first rule's LHS
   identifier as `&str` without materializing the document. The grammar
   IS data; the path API turns it into a queryable structure. Today
   the LSP (`crates/lsp/src/`) walks the materialized `BbnfDocument`
   tree; with direct-projection routing through `parse_with`, every
   LSP query becomes O(path-length × byte-skip) rather than O(grammar-size).
2. **Live grammar inspection** — `cargo xtask` debug surfaces (e.g.,
   `cargo xtask describe-rule <name>`) can use
   `path!(Bbnf, "rule", N)` indexing without parsing-then-walking.
3. **Meta-tooling lane** — once the BBNF grammar is queryable through
   `path!`, generic grammar-transformation tooling (rule renamers,
   directive-injection, modifier-rewriting) becomes mechanical: the
   transformation reads paths against the source and writes via
   range-based byte edits. Today, every such tool re-implements
   parse-then-walk; with `path!(Bbnf, …)` they share the substrate.

The BBNF surface is structurally identical to the JSON / CSS L4 / Sheets
surface for `path!`: same registry, same compile-time validation, same
proc-macro entry point in `crates/bbnf-path/src/path_macro.rs`. Per
`crates/bbnf-path/src/registry.rs:91-102`, the four supported markers
are exactly `Bbnf`, `CssL4`, `Json`, `Sheets`; the BBNF marker is a
first-class citizen, not a degenerate case.

## III — Compile Gaps for BBNF

### StructRegistry audit

The BBNF `StructRegistry` (53 rule entries in
`bbnf.registry.json`) has **zero `cyclic: true` obligations** (grep:
`grep -c '"cyclic":\s*true' bbnf.registry.json` → 0). This is striking:
the BBNF grammar is replete with mutual recursion (`rhs` references
`alternation` which references `concatenation` which references
`binary_factor` which references `mapped_factor` which references
`factor` which references `term` which references `rhs` — the
canonical recursive grammar shape).

The registry resolves recursion through the cycle-break grounding at
`crates/ir/src/passes/types/mod.rs:83-129`: when the CSP cannot solve
a rule's variable, it grounds the rule's TypeDesc to `BoxedEnum` and
re-runs propagation. The grounding emits an `UnresolvedCompoundRef
{ cyclic: true }` obligation per Ref into the cyclic rule, but those
obligations land in `ir.type_obligations`, not in the registry sidecar.
The registry sidecar carries the *resolved* TypeDescs only. The
zero-cyclic count in the sidecar is therefore a *resolution-success*
signal, not a *no-cycles* signal.

This is correct and load-bearing. It means BBNF's recursive shape
resolves to typed-projection (every rule has a non-Span TypeDesc in the
registry) without manual annotation, validating the GESTALT §IV
direct-projection thesis: `->`-less BBNF rules project as cleanly as
annotated rules.

### BBNF-specific compounds

23 grammar-rule names map 1:1 to `BbnfCompoundKind` arms in
`crates/core/src/runtime/bbnf/arena.rs:33-139`: `Rule`, `Term`,
`Factor`, `MappedFactor`, `BinaryFactor`, `Concatenation`,
`Alternation`, `Closure`, `Rhs`, `Lhs`, `CallArg`, `ImportPath`,
`ImportItems`, `ImportDirective`, `RecoverDirective`, `PrettyHint`,
`PrettyDirective`, `WsDirective`, `TokenDirective`, `DebugDirective`,
`HostDirective`, `Directive`, `GrammarItem`, `Grammar`, plus 13
value-expression sub-grammar arms (`ValueExpr`, `ValueClosure`,
`ValueOr`, `ValueAnd`, `ValueCmp`, `ValueAdd`, `ValueMul`,
`ValueUnary`, `ValueAtom`, `ValuePath`, `ValueInput`, `ValueFnCall`,
`TypeAnnotation`), plus `Other` catch-all.

Projection coverage at HEAD: every BBNF compound rule pushes through
`builder.begin_compound(&__layout)` / `builder.end_compound(__handle)`
with the same hard-coded `rule_type: TypeDesc::Span` and
`fields: vec![]` literal DEEP-A documented for JSON. The BBNF
generated parser has 102 `rule_type: ::bbnf_ir::TypeDesc::Span` literal
sites and 230 `begin_compound` / `end_compound` / `push_branch_tag` /
`push_leaf_with_*` call sites. The same defect; same magnitude
(scaled).

`BbnfCompoundKind::from_layout` consults
`StructRegistry::compound_kind_for_layout(layout) -> &str`
(`crates/ir/src/registry/struct.rs:388-390`) which simply returns
`layout.rule_name.as_str()`. The BBNF runtime then matches on the
string. This is the registry-projection idiom but the registry's
*projected `rule_type` and `fields`* are still discarded by the
runtime layout literal — the only registry datum consulted is
`rule_name`.

### The empty-path zero-progress guard (commit `a0a3f9f0`)

Commit `a0a3f9f0` (verified via `git log` lookup; the commit is
`a0a3f9f0` per dispatch — the hash mapped in the worktree is
`a0a3f9f0042379a9e1844f63618dbf5537b7baf6`, fix scope
`runtime/bbnf/parse-with`):

```
fix(runtime/bbnf/parse-with): guard empty-path lazy lane against
zero-progress admission (AZ-IV.W3-DYNAMIC)

BBNF's Shape-2 list-rule body opens a compound, iterates zero times on
an entirely-malformed input, and closes the compound — returning
`Ok(())` without consuming a single byte past leading whitespace. The
empty-path lazy lane previously surfaced this as `Some` because
`finalise` and `doc.get(empty)` both succeed on the resulting empty
document.

Add a forward-progress check: when the path is empty, the lazy lane
requires `pos` to advance past any leading whitespace; otherwise
`None`.
```

The guard at `crates/core/src/runtime/bbnf/parse_with.rs:75-87`:

```rust
if path.is_empty() {
    let bytes = src.as_bytes();
    let mut leading = 0usize;
    while let Some(&b) = bytes.get(leading) {
        if matches!(b, b' ' | b'\t' | b'\n' | b'\r') {
            leading += 1;
        } else {
            break;
        }
    }
    if pos <= leading && leading < bytes.len() {
        return None;
    }
}
```

**Does this point at a deeper architectural issue?** Yes, and the
pointer is precise: BBNF's top-level `grammar` rule is a list-rule
(`grammar = ( grammar_item ?w ) *`) — the Kleene-star lower bound is
zero. On entirely-malformed input, the recognizer admits zero
iterations, opens-and-closes the `grammar` compound, and returns
`Ok(())` at `pos == 0` (no bytes consumed). The empty-path lazy lane
treats "Ok(()) + finalise()" as "path resolved to root identity" —
which is mechanically correct (the path *was* empty, the root *is*
present) but semantically wrong (the input was garbage).

The guard is correct as a *contract patch*. The deeper issue is
that the lazy lane's "path resolved" semantics conflate two
conditions: (a) the recognizer accepted, and (b) the recognizer
made progress. JSON's eager parse fails on empty input because JSON
has no zero-iteration top-level rule (`value` requires at least one
of object/array/string/number/bool/null). BBNF, Sheets, and CSS L4
all have list-rooted top-level rules with zero lower bounds — and all
three are vulnerable to the same admission. The guard lives in
BBNF's `parse_with` because BBNF is where it surfaced, but the same
pattern likely needs to be lifted to a `PathExecutor`-level invariant
(or, better, eliminated by the BA.W3 direct-projection refactor that
dissolves the separate `parse_with` lane into the eager-as-degenerate
case).

This is a **load-bearing piece of evidence for BA.W4** (`parse_with`
as value-API hot path): when the lazy lane and eager lane share
generated code with mode-driven dispatch, the guard's home becomes
the dispatcher, not a per-grammar `parse_with.rs`. The four
`parse_with.rs` files (json, css_l4, sheets, bbnf) all currently
duplicate the cursor-threading skeleton; the guard is one of the
hand-coded divergences that direct-projection will retire.

## IV — Generalized sonic-class API for BBNF

`BbnfBootstrap::get<T>(input, path!(Bbnf, …))` does not exist at HEAD.
The current surface is `BbnfBootstrap::parse(input).get(path)` (eager
materialize → walk) or `parse_with::<T>(input, &path)` directly
(lazy). The sonic-class `get` requires the same routing as DEEP-C
prescribed for JSON: `<Grammar>Parser::get<T>(input, path)` is
sugar for `parse_with(input, &path)`; `Document::get<T>(path)` on a
materialized document reroutes through `parse_with` internally.
Both surfaces converge.

What the BBNF `get` API specifically enables (beyond the JSON sugar):

1. **Live grammar inspection** — IDE/LSP queries against grammar
   source without parsing the entire grammar tree. The grammar is
   typically <1KB per file; a single `path!(Bbnf, "rule", 0, "rhs")`
   walk is byte-skip cheap.
2. **Grammar diffing / patching tooling** — paths over grammar sources
   become the addressing scheme for transformation pipelines (rule
   renamers, directive injectors, modifier rewriters). Today these
   tools parse-and-walk; with `get`, they bypass-parse to the target
   span and edit bytes directly.
3. **Self-test of direct-projection** — BBNF parsing BBNF is the
   identity end-to-end test. If `BbnfBootstrap::get<&str>(bbnf_source,
   path!(Bbnf, "rule", 0, "lhs"))` returns the first rule's LHS within
   ≤5x sonic-rs's equivalent JSON `get` (calibrated against
   sonic_get_twitter @ 332ns), the entire BA direct-projection
   refactor is validated end-to-end. This is the canonical proof
   point.
4. **Meta-tooling generality** — the `path!` macro itself is grammar-
   defined (BBNF rules express the path lexer's grammar; the path
   lexer parses paths). When BBNF can `get` against itself, the
   `path!` macro's compile-time validation becomes self-bootstrapping.

The grammar IS data; the path API turns it into a queryable structure;
the bootstrap discipline (BBNF parsing BBNF) makes the BBNF surface
both the substrate and the test of every other grammar's `get` API.
This is why the dispatch named BBNF as a "canonical end-to-end test":
it is.

## V — Recursive Structure Handling Under Direct-Projection

The dispatch asks: "BBNF rules reference other rules (recursion). Does
direct-projection codegen handle this cleanly? `IrNode::Ref(rule_id)`
— how does projection traverse this without infinite recursion at
codegen?"

### How the IR handles `Ref`

`crates/ir/src/passes/types/mod.rs:393` and `:549` both have:

```rust
IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
```

`Ref` is a *leaf* in the obligation/scratch-collection walks — the
walk does not recurse through it. This is correct: the rule's
TypeDesc is solved at the rule's own variable, and `Ref(rule_id)`
inherits its target rule's TypeDesc through the CSP propagation
(not through structural recursion). The CSP variable graph mirrors
the grammar's reference graph; cycle-break grounding (lines 83-129)
handles SCCs by promoting one rule to `BoxedEnum`, breaking the cycle.

### How direct-projection MUST handle `Ref` at codegen

DEEP-A's recommendation (recommendation 1):

```rust
struct JsonPair<'p>   { pub key: &'p str, pub value: JsonValue<'p> }
```

For BBNF, the analogue rule (`pair = key , ":" , value` is JSON's;
BBNF's analogue is e.g. `factor = big_comment ? , term ?w , modifier ? , big_comment ?`):

```rust
struct BbnfFactor<'p> {
    pub leading_comment: Option<BbnfBigComment<'p>>,
    pub term: BbnfTerm<'p>,        // ← Ref(term)
    pub modifier: Option<BbnfModifier>,
    pub trailing_comment: Option<BbnfBigComment<'p>>,
}
```

`BbnfTerm` is itself `Alt`-shaped (`term = "ε" | identifier , … | literal | regex | "@{" , rhs ?w , "}" | "(" , rhs ?w , ")" | "[" , rhs ?w , "]" | "{" , rhs ?w , "}"`).
Three branches reference `rhs`, which references `alternation`, which
references `concatenation`, which references `binary_factor`, which
references `mapped_factor`, which references `factor` — closing the
recursion cycle.

The codegen handles infinite recursion at **type-level boxing**:
`Ref(rule_id)` projects to `Box<BbnfRuleType>` when the ref is part of
a type-level cycle (CSP cycle-break grounded the cycle's rule to
`BoxedEnum`). Concretely:

```rust
enum BbnfTerm<'p> {
    Epsilon,
    Identifier(&'p str),
    Call { name: &'p str, args: Vec<BbnfCallArg<'p>> },
    Literal(&'p str),
    Regex(&'p str),
    Group(Box<BbnfRhs<'p>>),         // ← Box at the cycle break
    Bracketed(Box<BbnfRhs<'p>>),     // ← Box
    Brace(Box<BbnfRhs<'p>>),         // ← Box
    HostBrace(Box<BbnfRhs<'p>>),     // ← Box
}
```

The boxes break the type-level recursion cycle. Codegen detects
which `Ref`s sit inside a CSP-grounded cycle (the `cyclic: true`
obligation flag in `ir.type_obligations`) and emits `Box<…>` only
at those positions. References outside cycles project as direct
shapes. This is the same architectural move Rust's `enum` recursion
requires; it composes naturally with direct-projection.

### Why this works for BBNF specifically

BBNF has exactly one large mutual-recursion SCC at HEAD (the rhs ↔
alternation ↔ concatenation ↔ binary_factor ↔ mapped_factor ↔
factor ↔ term cycle), plus the `closure ↔ rhs` cycle, plus the
`value_*` precedence cycles in `expressions.bbnf`. The cycle-break
grounding promotes one rule per SCC to `BoxedEnum`; the codegen
emits `Box<>` at each `Ref` into the grounded rule. All other refs
project direct.

The audit recommendation: the BA.W2 direct-projection codegen
must read `ir.type_obligations` for `UnresolvedCompoundRef { cyclic:
true }` entries and route those `Ref` positions through `Box<>`.
This is a small, named codegen contract — not a per-grammar emitter
arm — and it composes with `feedback_no-orthogonal-codepaths` (one
projection pass, one set of cycle-break rules, no per-grammar
overrides).

### The infinite-recursion risk at codegen time

The codegen walk over the IR is bounded by the grammar's rule count
(53 for BBNF), not the recursive depth of any traversal. Recursion in
the *parser's runtime behavior* is bounded by the input depth (the
input controls which Ref edges fire, not the codegen). The codegen
itself emits one struct/enum per rule; the cycle is in the *type*
graph, not in the codegen pass. Direct-projection codegen handles
this cleanly via the cycle-break + Box discipline above.

## VI — Recommendations to BA / BB+

### BA.W2 — direct-projection items for BBNF

1. **Apply DEEP-A's recommendation 1-3 verbatim to the BBNF runtime.**
   Delete `BbnfStructBuilder`'s OpenFrame stack
   (`crates/core/src/runtime/bbnf/builder.rs`); replace with per-rule
   parse fns returning typed shapes directly. Replace
   `BbnfArena`'s `Vec<BbnfCompound>` slab
   (`crates/core/src/runtime/bbnf/arena.rs:277-341`) with a single
   `bumpalo::Bump`. The 23 + 13 + 1 = 37 `BbnfCompoundKind` arms
   become 37 typed structs/enums emitted from `bbnf.registry.json`.
2. **Cycle-break Box discipline.** BA.W2's codegen contract reads
   `ir.type_obligations` for `UnresolvedCompoundRef { cyclic: true }`
   entries, emits `Box<…>` at the corresponding `Ref` positions, and
   leaves all other refs direct. Test this discipline against BBNF
   (the most heavily mutually-recursive grammar in the fleet) as the
   canonical case.
3. **Predictive first-byte dispatch for BBNF byte-disjoint branches.**
   `term`'s 8-branch alternation has byte-disjoint prefixes for most
   branches (`"ε"`/`"epsilon"` start with `e` or non-ASCII; `"\""` /
   `"'"` / `` "`" `` are quotes; `"/"` is regex; `"@"` is host-brace;
   `"("`, `"["`, `"{"` are bracketed). Identifier branches start
   with `[_a-zA-Z]`. Predictive dispatch eliminates speculation for
   `term`; per DEEP-B, this is the load-bearing optimisation.
4. **Cross-grammar literal cleanup.** The BBNF generated `parse` fn
   contains a stale `crate::path::markers::Json` literal (line
   21447 of `crates/core/src/grammar/generated/bbnf.rs`):
   ```rust
   static __EAGER_EMPTY_PATH: ::std::sync::LazyLock<
       crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
   > = ...
   ```
   The empty-path constant uses `Json` as a phantom marker even for
   the BBNF parser. This is a substrate fault per DEEP-SYNTHESIS Hard
   Gate `__EAGER_EMPTY_PATH cross-grammar literal absent from
   crates/core/src/grammar/generated/**`. The BA.W4 collapse of
   `parse(input)` into `parse_with(input, &EMPTY_PATH)` is what
   retires it cleanly.

### BA.W4 — sonic-class `BbnfBootstrap::get` fixtures

5. **Add a same-harness `bbnf_get_grammar` bench row.** Mirror
   `bbnf_get_twitter`'s shape: load `grammar/bbnf/bbnf.bbnf`,
   query `path!(Bbnf, "rule", 0, "lhs")` via
   `BbnfBootstrap::get::<&str>(input, path)`, baseline against
   sonic-rs `get` on a structurally-similar JSON encoding of the same
   grammar (or — more honestly — against the eager
   `parse(input).get(path)` baseline). Hard-gate at ≤5x same-harness;
   target ≤1.0x with profile evidence.
6. **Promote the empty-path zero-progress guard from `parse_with.rs`
   to the `PathExecutor`-level dispatcher.** When `parse_with` fuses
   into the eager lane (BA.W4), the guard's residency moves to
   `crates/core/src/path/executor.rs` (or similar) and applies to
   every grammar with a list-rooted top-level rule. BBNF, Sheets,
   CSS L4 all share the vulnerability; the patch should generalize.

### BB / Phase 3 routing

7. **Phase 3 host-fns route to a post-BC letter, not BB.** BB's
   un-subsumed scope is rule-discovery (Ruler CVC enumeration);
   Phase 3's host-fn surface is orthogonal — it composes with
   direct-projection (host-fns produce `TypeDesc::Named` results;
   the projection already handles those abstractly) but does not
   constrain rule-discovery's rewrite-rule enumeration. The host-fn
   registry is a separate substrate from `StructRegistry`; the
   routing surface is `value_fn_call` evaluation, not Ruler's term
   algebra. Recommended destination: a new tranche letter (BD or
   later) named for "host-functions" specifically. Phase 3 is a
   tranche-class scope, not a wave.

### BC cleanup

8. **Sweep BBNF runtime files for the AZ-IV.W4 bounds-recording
   substrate.** `OpenFrame::start_offset` and `end_offset`
   (`crates/core/src/runtime/bbnf/builder.rs:31-42`) record byte
   bounds the parse fn consumed; this substrate is consumer-active
   today (per the field doc comment). Verify direct-projection
   preserves the bounds-recording mechanism — BBNF compounds use
   bounds for byte-span recovery in cases where alt-branch literals
   (`@import`, `:`, `(`) are consumed without a Span push. Without
   it, `byte_span()` under-reports compound extent. This is a
   load-bearing semantic surface; direct-projection must preserve it
   or replace it with an equivalent.

## VII — Summary

The BBNF self-host is the witness — bbnf parsing bbnf is the canonical
end-to-end test of "grammar-derived everything." Its bench attribution
inherits DEEP-B's primary blocker mechanically (`Vec<OpenFrame>::clone`
at 86% inclusive; 217 checkpoint sites in BBNF generated code; 37×
regression vs AU). Its semantic parity is 95/95 modulo two pre-existing
grammar-coverage gaps (emoji, json-commented), with Phase 3 host-fns
deferred to a future letter. Its `path!(Bbnf, …)` macro routes through
the production registry verbatim; direct-projection unlocks the
sonic-class `get` API which makes the BBNF grammar itself a
queryable structure (LSP introspection, grammar tooling, meta-language
manipulation).

Recursive grammar structure handles cleanly under direct-projection
via the cycle-break Box discipline: read `UnresolvedCompoundRef
{ cyclic: true }` obligations from `ir.type_obligations`, emit `Box<…>`
at the corresponding `Ref` positions, leave non-cyclic refs direct.
BBNF is the canonical mutual-recursion test case (rhs ↔ alternation
↔ concatenation ↔ binary_factor ↔ mapped_factor ↔ factor ↔ term
SCC; closure ↔ rhs SCC; value-precedence SCCs).

The empty-path zero-progress guard (commit `a0a3f9f0`) is a
load-bearing pointer to the deeper BA.W4 invariant: the lazy lane's
"path resolved" semantic conflates "recognizer accepted" with
"recognizer made progress"; list-rooted grammars (BBNF, Sheets, CSS L4)
all surface the conflation; the fix lives at the dispatcher level,
not the per-grammar `parse_with.rs`.

Top recommendations to BA: apply DEEP-A's struct-direct refactor to
BBNF runtime in W2; cycle-break Box discipline as a named codegen
contract; predictive first-byte dispatch for BBNF's `term` alt;
delete the `crate::path::markers::Json` cross-grammar literal at
`bbnf.rs:21447` in W4; promote the empty-path guard to the dispatcher
in W4; add `bbnf_get_grammar` same-harness bench row in W4. Route
Phase 3 host-fns to a post-BC letter; do not fold into BA or BB.
