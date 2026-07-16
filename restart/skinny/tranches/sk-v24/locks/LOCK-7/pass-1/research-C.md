# LOCK-7 / family 7C — THE HOST-FUNCTION LATTICE (pass 1, research-C)

Totality is not a slogan measured against 100% grammar coverage; it is a *bounded* quantity —
the fraction of every subsystem's leaf and context semantics expressed by grammar (`->`
projections, `@{}` captures, class-run scans) versus the fraction that legitimately lives
behind a TYPED host-function interface. This family's thesis: draw that boundary with a
declared, checkable host-fn signature lattice, measure the fraction behind it per subsystem,
name a floor for each, and make the escape hatch impossible to abuse as a grammar-name gate in
disguise. The regex-grammar bootstrap and self-hosting depth (the sibling families' centers)
are consumed here as *consumers of the boundary* — the bbnf and regex grammars are themselves
subsystems whose host-fn floor must be measured.

## 1. The mechanism made concrete — what exists, and where the boundary lives today

The host boundary already has three physical forms in the tree, unequal in maturity.

**Declaration surface.** `@host name : ReturnType ;` is grammar-defined at
`grammar/bbnf/bbnf.bbnf:73` (`host_directive = "@host" ?w , identifier ?w , ( ":" ?w ,
type_name ?w ) ?`), with the return-type vocabulary in `grammar/bbnf/types.bbnf:3-7`
(`type_name` = the numeric-width primitives plus an open `[_a-zA-Z][_a-zA-Z0-9]*` identifier for
`IrNode`, `Span`, user structs). It parses through `decode_host` at
`crates/core/src/grammar/host.rs:424` into `HostFnDecl { name, return_type: Option<Cow<str>> }`
(`crates/core/src/types.rs:81`) and lands in `DirectiveMaps::host_map: HashMap<String,
Option<String>>` (`crates/core/src/pipeline/directives.rs:34,52`) — a name→return-type map,
**nothing more**. There is no parameter-type list, no arity, no purity/effect class, no
staging annotation.

**Call surface.** Host functions are invoked only inside `->` value expressions as
`MapExpr::FnCall { name: StringId, args: Vec<MapExpr> }` (`crates/ir/src/types/map_expr.rs:31`).
The value grammar admits the call form at `grammar/bbnf/expressions.bbnf:14`
(`value_fn_call = value_path , "(" , ( value_expr , … ) ? , ")"`). Return-type propagation from
the declaration to the `-> T` map arrow is `extract_value_func_name`
(`crates/core/src/lower/value_expr/unwrap.rs:199`): the recovered call name is looked up in the
host-fn table; a miss is tolerated silently (non-host names just fail the table check).

**The already-typed inner lattice.** The load-bearing discovery: the compiler already carries a
closed, typed classification of derived semantic actions — `FnDescriptor`
(`crates/ir/src/types/fn_descriptor.rs`) with six variants: `EnumWrap{variant}`, `BoxWrap`,
`NumberConvert{allow_leading_dot}`, `HexConvert{fn_path}`, `SpanCapture`, and the catch-all
`Expr{expr: MapExpr, return_type: Option<TypeDesc>}` into which all `->` syntax lowers. This IS
a lattice in the algebraic sense — each specialization (`NumberConvert`, `HexConvert`) is a join
of `Expr` with a recognized inner regex class, refining an untyped user expression into a fused
kernel. `@{expr}` span-capture (`grammar/bbnf/bbnf.bbnf:33`) lowers to `FnDescriptor::SpanCapture`
— validate-then-discard-returning-Span, used by CSS `colorFunction`
(`grammar/css/l4/color.bbnf:220`) and `urlFunction` (`grammar/css/l4/values.bbnf:69`). The family's
work is to make the *outer* `@host` declaration as typed as the inner `FnDescriptor` lattice
already is, and to unify them: `@host` becomes the user-facing constructor for the `Expr`-with-
declared-signature node.

**The cost boundary (where totality costs runtime).** A parse-time host decode forces the eager
tape shape: `ShapeRationale::HostFnParseTime` (`crates/ir/src/cost.rs:255`) is chosen by
`has_parse_time_host_decode` (`crates/passes/src/lib.rs:794`, consumed at `:620` and `:764`), a
structural recursion over `ExprKind`. This is the crux the lattice must own: **a pure leaf-decode
host fn should not tax the tape, but a context-carrying one legitimately does** — and today the
distinction is a heuristic structural walk, not a declared purity class.

**The skinny slice (the greenfield floor).** The skinny compiler is deliberately host-fn-*minimal*:
`skinny/crates/runtime/src/grammars/json/host.rs` is empty ("JSON is host-fn-free in the skinny
compiler slice"); the workspace metadata declares `host_registry = skinny-none`. Skinny does carry
a leaner runtime `@{}` mechanism — `RuntimeHostCapture` (`skinny/crates/grammar/src/lib.rs:170`,
scanned by `parse_runtime_host_capture:799`) — but tags it `BBNF-UNSUPPORTED-HOST-CAPTURE`
(`:222`) and gates consumers behind a `host_capture: bool` requirement
(`skinny/crates/codegen/src/grammar_provider.rs:45,267`, `frontend_missing("host capture")`). The
greenfield inherits an almost-blank host plane — which is the opportunity: the lattice is
*designed*, not retrofitted around a legacy surface.

## 2. The substrate — the typed host-fn signature the lattice needs

The concrete extension, minimal and grammar-first (self-hosting law: augment BBNF, never work
around):

**A full signature grammar.** Replace `@host name : ReturnType` with a declared signature:

```
host_directive = "@host" ?w , identifier ,
                 ( "(" ?w , ( host_param , ( "," ?w , host_param )* ) ? , ")" ) ? ?w ,
                 ( ":" ?w , type_name ?w ) ? ,
                 ( "@" , purity_class ) ? , ( ";" | "." ) ? ;
host_param     = type_name ;          // Span | ValueRef | u32 | f64 | Capture | …
purity_class   = "pure" | "context" | "comptime" ;
```

- **Param types** name what the host fn consumes: `Span` (raw leaf text), a typed `ValueRef`
  cursor, a numeric width, or a `Capture` group. Arity + param types are checkable against every
  `MapExpr::FnCall` site — today entirely unchecked.
- **`return_type`** already exists (`HostFnDecl.return_type`); it threads to the `-> T` arrow via
  `extract_value_func_name`.
- **`purity_class`** is the totality-boundary discriminant, projecting directly onto the cost
  model: `pure` (a leaf decode — hex→u32, base64, checksum; no parse state) must select a
  *non-eager* shape; `context` (needs cursor/nesting depth — the tree-sitter-external-scanner
  case) legitimately routes `ShapeRationale::HostFnParseTime`; `comptime` is a constant-foldable
  decode (LOCK-3 staged residual — erased at stage 0). `has_parse_time_host_decode` becomes a
  *lookup of the declared class*, not a structural guess.

**The signature store.** `DirectiveMaps::host_map` widens from `HashMap<String, Option<String>>`
to `HashMap<String, HostFnSig>` where `HostFnSig { params: Vec<TypeDesc>, ret: Option<TypeDesc>,
purity: PurityClass }`. `TypeDesc` is backend-agnostic (memory: backend-agnostic-types) so the
Rust and TS backends each resolve it natively.

**Unification with `FnDescriptor`.** A declared `@host` fn is `FnDescriptor::Expr` carrying its
signature; the existing specializations (`HexConvert`, `NumberConvert`) become the *inferred*
refinements of a `pure`-classed host call whose inner arg is a recognized regex class. One
lattice: user-declared signatures at the top, compiler-inferred kernel specializations as joins
below. The `SpanCapture`/`@{}` form is the `pure : Span` corner.

**The totality census (the measured percentage — the charter's quantitative demand).** An xtask
subcommand `census-totality` walks each grammar's IR and reports, per subsystem: leaf semantics
expressed by grammar (`->` MapExpr with no `FnCall`, `@{}` SpanCapture, class-run/regex scans)
versus behind a host `FnCall`, and classifies each host call by purity. Output is a table with a
**named floor per subsystem** — the charter's deliverable. Current readings, from the tree:
JSON = 100% grammar (host.rs empty); CSS carries two `@{}` span-captures (both `pure : Span`, not
true host escapes); sheets/csv/toml floors to be measured. The floor is a committed invariant a CI
gate defends (a regression below floor fails).

## 3. Integration seams (path:line where extant code serves)

- Declaration grammar: `grammar/bbnf/bbnf.bbnf:73` (`host_directive`), `grammar/bbnf/types.bbnf:3-7`
  (`type_name` — the param/return vocabulary to widen).
- Parse → typed decl: `crates/core/src/grammar/host.rs:424` (`decode_host`), sink at `:114`
  (`push_host_fn`); `HostFnDecl` at `crates/core/src/types.rs:81`.
- Signature store: `crates/core/src/pipeline/directives.rs:34,52` (`host_map` + `host_map_mut`).
- Call node: `crates/ir/src/types/map_expr.rs:31` (`MapExpr::FnCall`).
- The inner typed lattice to unify with: `crates/ir/src/types/fn_descriptor.rs` (six variants).
- Return-type propagation: `crates/core/src/lower/value_expr/unwrap.rs:199`
  (`extract_value_func_name`); atom lowering `crates/core/src/lower/value_expr/atom.rs:414`
  (`lower_fn_call_atom`, `:426` `collect_fn_call_args`).
- Cost/totality boundary: `crates/ir/src/cost.rs:255` (`ShapeRationale::HostFnParseTime`);
  `crates/passes/src/lib.rs:794` (`has_parse_time_host_decode`, the structural heuristic to
  replace with a purity lookup), consumed `:620,:764`.
- Multi-backend resolution: Rust `host_registry` metadata at
  `skinny/crates/codegen/src/grammar_provider.rs:27`; TS `declare function` stub emission at
  `crates/core/src/backend/ts/emitter/grammar.rs:236` (`collect_host_fn_names`).
- Skinny slice to grow: `skinny/crates/grammar/src/lib.rs:170` (`RuntimeHostCapture`),
  `grammar_provider.rs:45,267` (`host_capture` requirement); empty
  `skinny/crates/runtime/src/grammars/json/host.rs`.
- Closures (the recursive-context sibling mechanism, adjacent but distinct):
  `crates/core/src/pipeline/compile/closure_partition.rs` (first-class grammar functions expanded
  inline; `partition_closures:21`, `collect_closure_param_names:116`) — grammar functions carry
  their own param lattice and are the in-grammar answer to context the host boundary would
  otherwise absorb.
- Gate to extend: `docs/tranches/sk-v24/GN-5.md` W3 `check-name-gates` (the AST audit that the
  host-fn declaration surface must also traverse; R2 §7 names this obligation explicitly).

## 4. Prior art (with citations)

- **ANTLR semantic predicates + actions** — inline host code embedded in productions to
  disambiguate context-sensitive syntax; powerful but untyped and unbounded. The 7C lattice is the
  disciplined typed analogue: `->` for pure leaves, a *declared-signature* `@host` for context —
  the predicate's power without the unbounded inline-code coupling.
  ([Comparison of Leading Language Parsers](https://www.researchgate.net/publication/376883410_Comparison_of_Leading_Language_Parsers_-_ANTLR_JavaCC_SableCC_Tree-sitter_Yacc_Bison))
- **tree-sitter external scanners** — arbitrary C for non-regular tokens (indentation, heredocs,
  raw strings). The unbounded, untyped escape hatch. 7C's contribution over it: a *typed
  signature* + *purity class* + *measured floor* replaces "write whatever C you need" — the
  boundary is declared and audited, not a blank function pointer.
  ([External Scanners — Tree-sitter](https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html))
- **Menhir semantic actions** — typed OCaml expressions attached to productions; the type
  discipline bbnf's `-> value_expr : type` already mirrors. Difference: Menhir actions are
  arbitrary Turing-complete inline code; 7C bounds them behind a *named, signed* interface so the
  grammar stays analyzable (constant-folding, type projection over `MapExpr`).
  ([Menhir Reference Manual](https://gallium.inria.fr/~fpottier/menhir/manual.html),
  [Parsing with OCamllex and Menhir](https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html))
- **Marpa's staged evaluation** — events during recognition, actions during a separate evaluation
  phase; the exact comptime/runtime split the `purity_class` `comptime` corner encodes (align with
  LOCK-3 staged-residual: a `comptime` host fn is erased at stage 0).
  ([Marpa FAQ](http://savage.net.au/Perl-modules/html/marpa.faq/faq.html))
- **rustc/GHC effect & purity typing** — the precedent for classifying a function by what state it
  touches; `purity_class` is the parser-generator instance (pure/context/comptime as a coarse
  effect lattice driving the tape-shape decision).

## 5. Named risks and counterexamples

- **R1 — the typed host fn as a name-gate in disguise (GN-5 R2, binding).** `@host
  parse_toml_datetime` hard-coding one grammar's leaf is the master pathology: it *passes* a
  string-literal name-gate audit while being an overfit shim. Counter: `check-name-gates` must
  traverse the `@host` *declaration surface* and flag any host-fn name/signature encoding a
  grammar name; and the totality census must show a *high grammar floor* — a subsystem whose leaves
  are mostly host-shimmed fails its floor. The self-hosting law bites here: TOML datetime must be
  proven grammar-expressible (class-run scan, the WALL-B idiom) before a host shim is admitted.
- **R2 — the escape hatch defeats the measurement.** If `@host` admits an untyped fn with no
  signature, "percentage behind grammars" is meaningless (arbitrary code hides behind one name).
  Counter: a host fn with no declared signature is a *hard error*, not a warning — no untyped
  escape. The floor is only credible if every escape is signed.
- **R3 — cross-backend non-portability.** A Rust host fn has no TS body; the TS backend emits only
  `declare function` stubs (`backend/ts/emitter/grammar.rs:236`). A grammar leaning on host fns is
  not isomorphic across backends (constitution iii). Counter: the signature is backend-agnostic
  (`TypeDesc`), and a portability lint reports every host fn lacking a declared impl in a target
  backend; `pure`/`comptime` classes are portable by construction (side-effect-free decode),
  `context` classes are flagged as backend-specific.
- **R4 — purity misdeclaration silently corrupts the tape shape.** Declaring a genuinely
  context-dependent fn `pure` would select a non-eager shape that cannot supply the state the fn
  needs. Counter: the lowering verifies a `pure`-declared fn's arg list references no parse-state
  (`Input`/cursor) type; a `pure` fn taking a cursor param is a compile error. The declaration is
  checked against the call, not trusted.
- **R5 — the closure/host boundary blurs.** First-class grammar closures
  (`closure_partition.rs`) and host fns both absorb context; overlap invites a god-mechanism.
  Counter: the rule is sharp — closures are *in-grammar* (expanded inline, fully analyzable), host
  fns are *out-of-grammar* (opaque, signed). Recursive/context semantics expressible as a closure
  MUST be a closure; host is the last resort, and the census counts closures as *grammar* coverage,
  not host coverage — so pushing context into closures *raises* the floor.

## 6. Exactly what a prototype must test to move this family

1. **Signature parse + arity check.** Extend `@host` to the full signature grammar; regenerate the
   bbnf self-parser; prove a `@host decode(Span) : u32 @pure` declaration parses, threads into
   `host_map` as a `HostFnSig`, and that a `MapExpr::FnCall` site with wrong arity/param type
   throws a diagnostic naming the grammar span (today: unchecked). Success = an arity mismatch is a
   compile error, not a silent miss.
2. **The totality census with a named floor.** Implement `xtask census-totality` over the five
   greenfield grammars (json/css/csv/toml/sheets); emit the per-subsystem grammar-vs-host table with
   purity breakdown. Success = a committed floor per subsystem (JSON 100%, CSS's two `@{}` counted
   as grammar, the rest measured) that a CI gate defends against regression.
3. **Purity → tape-shape wiring.** Replace `has_parse_time_host_decode`'s structural heuristic with
   a `purity_class` lookup; prove on a real grammar that a `pure` hex-decode host fn selects a
   non-eager (offset/sink) shape while a `context` fn routes `HostFnParseTime` — measured cyc/B: the
   `pure` path must NOT carry the eager-tape tax. Success = a purity reclassification measurably
   changes the selected shape and the cost, cold-paired.
4. **The name-gate falsification for host fns.** Plant `@host parse_csv_field` and
   `@host toml_datetime` and assert the extended `check-name-gates` trips on grammar-name-encoding
   host declarations; assert the clean five-grammar tree passes. Success = the gate distinguishes a
   general host fn (`decode_hex`) from an overfit one (`parse_toml_datetime`) — the GN-5 R2
   obligation discharged.
5. **The TOML datetime falsification (the self-hosting proof).** Prove TOML's datetime leaf is
   grammar-expressible (class-run scan) with NO host fn — the concrete demonstration that the floor
   is achievable, not aspirational. Success = TOML round-trips against the `toml` crate with its
   datetime leaf fully grammar-expressed, host_map empty for that subsystem.

The family moves when (2) and (3) land together: a *measured* totality percentage with a *named
floor*, and a purity lattice that makes the totality boundary cost-honest rather than a structural
guess. That pair is the charter's irreducible core — everything else (signature checking, the
name-gate audit, backend portability) is the enforcement scaffolding around it.
