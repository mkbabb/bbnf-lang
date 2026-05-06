# Deferral audit #2 — Function and value system

Greenfield audit, V6-READY corpus. Cluster owned: function values, lambdas,
closures, composition, currying, pattern-matching, sum/product values, function
types in the `Type` non-terminal. Sibling audit #1 owns higher-rank, GADTs, row
polymorphism, and the broader type-system shape; cross-cuts called out in §5.

The mandate is to surface what V1 currently lacks under "audacious, SOTA,
functional-in-nature, Rust-like ergonomics" and to settle the ffuzzy
finite-transducer question without resorting to a new `@directive`. The locking
question for the cluster is whether ordinary first-class generic host functions,
suitably extended with the surface this audit recommends, suffice to express
ffuzzy. The answer is yes, with three folds.

## §1 — Scope and corpus references

| Surface | Reference |
|---|---|
| Settled BBNF surface (V1) | `restart/README.md:121-184`; `restart/audit/pass-1-substrate/PASS-1.md:24-37`, `restart/audit/pass-1-substrate/PASS-1.md:194-233`. |
| Type system contract (V1) | `restart/README.md:258-272`; `restart/ARCHITECTURE.md:1137-1207`; `restart/audit/pass-1-substrate/PASS-1.md:73-78`. |
| Closure semantics (narrow) | `restart/ARCHITECTURE.md:1187-1208` — four kinds (host-chain, map, predicate, recovery); explicit "narrow" framing; forbidden behavior table. |
| Formal grammar `Type` production | `restart/audit/pass-1-substrate/PASS-1.md:222`: `Type = Ident GenericArgs? \| TupleType \| RecordType \| BorrowType`. There is no `fn(T) -> U` arrow type. |
| Formal grammar `Closure` and `Primary` | `restart/audit/pass-1-substrate/PASS-1.md:214-218`: `Closure = "\|" Params? "\|" Expr`; `Primary` admits `Closure`. The closure is admitted into rule expressions but is not expressible as a `Type` and has no first-class value-of-function-type semantics. |
| Generic rules | `restart/README.md:168-170`; `restart/ARCHITECTURE.md:1155`; rank-1 parametric, monomorphised at finite call sites, no GADT/local-equality. |
| Multi-function chaining | `restart/audit/pass-1-substrate/PASS-1.md:231`; `restart/ARCHITECTURE.md:1117-1121`, `restart/ARCHITECTURE.md:1157`. Rule-level `->` chain only; method-chain only inside `@host fn` body. No `fn`-typed parameter, no point-free composition, no partial application. |
| Bidirectional research | `restart/research/topic-2-bidirectional.md:104-200` (Pierce-Turner); §2.3 (Dunfield-Krishnaswami higher-rank); restart commits to local check/synth in V1 and routes higher-rank to a future proof gate. |
| HM research | `restart/research/topic-1-hm-foundations.md:142-215` (Milner 1978 / Damas-Milner 1982; principal-scheme rank-1 commitment). |
| ffuzzy primitive analysis | `docs/ffuzzy.md:1-622`; specifically the host-walker resolution at `docs/ffuzzy.md:594-614` ("That walker IS the finite-state transducer. It's 30 lines. It doesn't need to be a bbnf feature."). |
| Hardening readiness | `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:86-94` (Topics 1-3 closed; rank-1 HM + local check/synth + finite CSP frozen for V1). |

## §2 — Identified absences and deferrals

The cluster reveals nine items absent from the V6-READY surface. Each row carries
the same shape as sibling audit #1.

| Item | Source | Current language | V1 fold proposal | Implementation impact | Risk | Greenfield value | Recommendation |
|---|---|---|---|---|---|---|---|
| **F1. Function arrow in `Type` production** | `restart/audit/pass-1-substrate/PASS-1.md:222` lists `Ident\|TupleType\|RecordType\|BorrowType`; no `fn(T,...)->U`. | A function cannot be denoted as a type. Parameters, host-fn return types, and chain-step targets cannot type-annotate "a function expecting T producing U". | Add `FnType ::= "fn" "(" TypeList? ")" "->" Type` to `Type`. Rank-1 only — `fn` types may not appear inside generalized HM schemes as quantified positions; they appear as concrete first-order types instantiated at use. | One grammar production; one `Type` IR variant; checker treats arrow as a binary type constructor under first-order unification (Pottier-Rémy decomposition). No new solver. | Low. Arrow is the canonical first-order type constructor since Milner 1978 (`restart/research/topic-1-hm-foundations.md:147-158`); the equality-constrained core handles arrow without extension. The HM principal-scheme guarantee is preserved as long as arrows are not generalized polymorphically inside arrow positions (rank-1 fence). | High. Without `fn`, every higher-order combinator (`map`, `filter`, `fold`, `compose`, transducer) is expressible only as a magic-name-resolved chain step, which is exactly the contrivance the user has flagged. | **FOLD V1.** |
| **F2. Function values as first-class** | `restart/audit/pass-1-substrate/PASS-1.md:217-220`: `ChainExpr = Ident { "->" Ident }` — chain steps are bare identifiers resolved to `Map`/`HostCall`. There is no expression form whose value is "the function `f`." | An identifier `f` in chain position dereferences a function; an identifier `f` in host-call position invokes it (`@f(x)`). The two never converge: a function cannot be returned from a chain, stored, or passed as argument. | Promote `Ident` resolved to a `@host fn` to a first-class value of arrow type when used outside call position. Add `FnRef ::= "@" Ident` (or reuse `HostCall` with empty arg list) as a no-call reference; checker assigns its arrow type. | One additional `Primary` form; one `HostFnRef` Grammar IR variant; codegen emits a function-pointer or zero-sized-type proxy depending on `host` registry binding (Rust handles natively; WASM via dispatch table index). | Low. Function values were second-class in early ML; first-class is the modern norm (`restart/research/topic-1-hm-foundations.md:201-218`, OCaml/Roc/Idris2 evidence). | High. Without F2, F1 is decorative — there are no values to inhabit `fn(T)->U`. F1 + F2 together unlock combinators. | **FOLD V1, paired with F1.** |
| **F3. Function-typed parameters in `@host fn` and rules** | `restart/audit/pass-1-substrate/PASS-1.md:204` and `restart/ARCHITECTURE.md:1090-1092`: `Param ::= Ident ":" Type`. Combined with the missing `fn(...)` form (F1), `Param` cannot today carry a function type. | `@host fn map<T, U>(f: ???, xs: [T]) -> [U]` is not expressible. Combinators that take a function reduce to per-monomorphisation hand-written primitives in `host::primitives`, defeating the "everything is grammar-derived" anthem (`restart/README.md:9-13`). | Once F1 and F2 land, `Param ::= Ident ":" Type` admits `Type = FnType` mechanically — no further grammar change. The change is purely in resolver/checker: bind parameter `f` to a value of arrow type, accept `f(x)` in body, propagate types through application. | None beyond F1+F2 at the grammar surface. Resolver gains a "parameter is a callable" branch already structurally identical to the host-call branch. | Low. The trick that makes this safe is rank-1: `f`'s arrow type is concrete at the monomorphisation site (per `restart/ARCHITECTURE.md:1155` finite `(RuleId, TypeArgs)` rule); no nested generalization needed. | Audacious. This single fold replaces the ffuzzy walker (§4 below), the JSON visitor combinators, the CSS rewrite passes, and the workspace-metadata "host route" matrix with one combinator surface. | **FOLD V1.** Trivially derives once F1/F2 land. |
| **F4. Anonymous functions (lambdas) as values** | `restart/audit/pass-1-substrate/PASS-1.md:218`: `Closure = "\|" Params? "\|" Expr` — present at the `Primary` site but `restart/ARCHITECTURE.md:1187-1208` confines `Closure` to four typed roles, none of which is "an arbitrary value of arrow type." | Inline lambdas exist syntactically but are restricted to map/predicate/recovery/host-chain bodies. They cannot be assigned, passed as arguments, or returned. | Lift the four "narrow closure" roles into one form: a closure expression has type `fn(T1,...)->U` synthesised from parameter annotations and body (Pierce-Turner check/synth — `restart/research/topic-2-bidirectional.md:120-135`). The four current roles become consumers of a closure value, not separate syntaxes. | One simplification of `ARCHITECTURE.md` §8.4 (collapse the four-role table to "any closure has arrow type"); IR `Closure` variant carries body + captured environment; checker uses bidirectional check when an expected arrow type flows in (e.g., from a `fn`-typed parameter), synthesis otherwise. | Low. The current narrow framing is already implementable as the unified form plus four consumer-side type checks. The Rust analogue is `Fn`/`FnMut`/`FnOnce`; for V1 a single `fn` form (no mutability tracking) suffices. | High. Without F4, every combinator call requires a named `@host fn` definition out-of-line, defeating the Rust-like ergonomics goal. | **FOLD V1.** Unify the four-role closure into one form; push role semantics to consumer site. |
| **F5. Closure capture of lexical environment** | `restart/ARCHITECTURE.md:1187-1191`: closures are "intentionally narrow ... to model host chains and typed grammar mappings without turning BBNF into a general programming language." Capture is implicitly allowed (host-chain closure captures previous host result; map closure captures named captures) but the capture rules are role-specific. | A closure inside a chain step captures the prior step's value; a closure inside `@host fn` body captures the rule's named bindings; a closure inside another closure's body — undefined. Closures cannot be passed across rule boundaries because their capture set is not part of their type. | Adopt the standard rule: a closure captures its lexical environment by reference (`&'i Tape<'i>`-friendly) or by value for `Copy` scalars. The arrow type (F1) carries the closure type; the capture set is hidden. For `'i` captures the closure inherits the input lifetime — the closure type is `fn(T) -> U + 'i` (Rust's `impl Fn(T) -> U + 'i` lowering). | Lifetime variable on closure types. The bidirectional checker propagates `'i` through arrow positions; the codegen emits closures as Rust closures (which Rust borrowck validates) on the Rust backend, and as captured-environment structs on the WASM backend. | Medium. Closure capture interacts with the slice-borrow contract (`restart/README.md:298-318`). The fence: closures may not capture mutable parser state; this matches the existing "Forbidden closure behavior" row "Mutating parse input" (`restart/ARCHITECTURE.md:1205`). | Audacious. With F5, transducers can carry rule-set context as a captured value; combinators compose without trampolining through global state. | **FOLD V1.** Capture-by-reference on `'i`; capture-by-value on `Copy`. |
| **F6. Function composition (`>>` or `compose`)** | No production; chains exist only as `->` between named bindings, not as a value-level binary operator on functions. | `f1 >> f2` is unutterable; `compose(f1, f2)` requires F1+F2+F3. The existing rule-level chain `Expr -> f1 -> f2` is composition specialised to a parsed value as the left operand. | Once F3 lands, `compose` is a one-line `@host fn` in the standard library: `@host fn compose<A, B, C>(f: fn(A)->B, g: fn(B)->C) -> fn(A)->C { \|x\| g(f(x)) }`. No new grammar. Optionally surface a `>>` operator as syntactic sugar; sugar is not required for V1. | None at grammar level; one entry in `host::primitives`. | None. | High via reuse: every other combinator built atop F1-F5 inherits composition for free. | **FOLD V1 as library, not syntax.** Sugar (`>>`/`<<`) deferred. |
| **F7. Currying / partial application** | The current chain semantics (`restart/audit/pass-1-substrate/PASS-1.md:231`) requires every chain step to accept the prior value as its first argument; there is no notion of a function taking N args returning a function-of-(N-1) args. | Today `f(x, y)` is a host-call with both args given simultaneously; `f(x)` with `f: fn(A,B)->C` is a type error. There is no partial application. | Two equivalent paths: (a) auto-currying — every multi-arg `@host fn` is implicitly its curried form (Haskell convention); (b) explicit partial application via closure-wrapping at the call site (`\|y\| f(x, y)`). Path (b) is sufficient given F4 and is the Rust convention. | Path (b): nothing beyond F4. Path (a): the host-call resolver lowers `f(x)` with `f: fn(A,B)->C` to a closure of type `fn(B)->C` capturing `x`; finite CSP picks between the under-application and an arity-mismatch error using existing host-overload selection (`restart/audit/pass-1-substrate/PASS-1.md:73`). | Low for path (b); medium for path (a) — auto-currying interacts with overload resolution and is not Rust-like. | Path (b) preserves Rust ergonomics; path (a) imports an ML idiom that may surprise Rust-trained authors. | **FOLD V1 as path (b)**: closure-wrap at the call site. Path (a) deferred unless authoring evidence demands it. |
| **F8. Pattern-matching as an expression** | `restart/audit/pass-1-substrate/PASS-1.md:194-216`: no `match`/`case` production. Destructuring exists implicitly through generated typed-record field projection (`restart/README.md:113-118` deep-enum + visitor surface) but only at the consumer side, not inside `@host fn` bodies. | Inside an `@host fn` body, an author wanting to dispatch on the variant of a typed value must call out to a Rust-side helper — exactly the contrivance the anthem rejects (`restart/README.md:9-13`). | Add a `MatchExpr` production: `Match ::= "match" Expr "{" Arm+ "}"`; `Arm ::= Pattern "=>" Expr ","?`; `Pattern ::= Literal \| Ident \| "_" \| Constructor "(" Pattern* ")" \| "(" Pattern ("," Pattern)* ")"`. Exhaustiveness is checked against the synthesized type's variant set (typed-record narrowing already exists per `restart/ARCHITECTURE.md:1153, 1168-1171`). | One IR variant (`Match`), one type-checker rule (each arm checks the scrutinee type's branch; result type unifies across arms). Codegen lowers to Rust `match` directly; WASM emits a dispatch table. | Medium. Exhaustiveness on generated enums needs the variant set to be available at type-check time — already true since enums are grammar-derived (`restart/README.md:113-118`). Nested pattern-match is an `@error(BBNF-PATTERN-NONEXHAUSTIVE)` candidate. | High. Pattern-match in `@host fn` bodies is the canonical functional-language tool for variant dispatch. Without it, every Alt-derived enum requires either a host-side helper or a chain of `if-let`-style probes; the Rust-like ergonomics target collapses. | **FOLD V1.** Match without guards; nested patterns; literal/wildcard/constructor/tuple. Or-patterns and guards deferred. |
| **F9. Tuple values inside expressions** | `restart/audit/pass-1-substrate/PASS-1.md:222`: `TupleType` exists. Architecture §8 forbids closures from "Capturing arbitrary host process state" (`restart/ARCHITECTURE.md:1204`) but does not provide a literal tuple expression. | Tuples can be types but cannot be constructed inside an `@host fn` body. The `tuple_to_color` example at `restart/README.md:164` ("`color = "#" (hex_byte hex_byte hex_byte) -> tuple_to_color`") is grammar-side construction; the inverse — building a tuple value from individual components — is unutterable. | Add `Tuple ::= "(" Expr ("," Expr)+ ")"` and `Pattern ::= "(" Pattern ("," Pattern)+ ")"`. Checker synthesises tuple type from component types; pattern-match destructures (F8). | One `Primary` form; one `Pattern` form; one `Tuple` IR variant; checker uses the existing `TupleType`. | Low. | High. Tuples are the universal product type; with F8 pattern-match, they fully replace ad-hoc multi-return idioms. | **FOLD V1.** Trivial extension; pairs with F8. |

Summary count: nine items, all FOLD V1. Sibling audit #1 is expected to also
recommend folds; the count discipline of `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:62-69`
says zero V6 hardener found a REINVENT/DISCARD/amendment row. Adding nine items
amounts to a research-fold amendment of equivalent shape to the V6 fold itself.

## §3 — Worked examples

Each example assumes F1-F9 folded. Code uses the BBNF surface as defined by
`restart/audit/pass-1-substrate/PASS-1.md` §6 plus the proposed productions.

### §3.1 — Function values + arrow type (F1+F2)

```bbnf
@host fn double(n: i32) -> i32 { n * 2 }
@host fn triple(n: i32) -> i32 { n * 3 }

# F2: bare reference is a value of arrow type
@host fn pick<F: fn(i32) -> i32>(use_double: bool) -> fn(i32) -> i32 {
    if use_double { double } else { triple }
}
```

The bidirectional checker (`restart/research/topic-2-bidirectional.md:153-167`)
handles this: the `if` branches synthesize `fn(i32)->i32` (each is a function
reference); the result type checks against the declared return.

### §3.2 — Function-typed parameters (F3) — the `map` combinator

```bbnf
@host fn map<T, U>(f: fn(T) -> U, xs: [T]) -> [U] {
    xs.iter().map(f).collect()
}
```

The body's `.iter().map(f).collect()` is method-chain syntax (legal inside
`@host fn` body per `restart/audit/pass-1-substrate/PASS-1.md:231`). The
`map` host-fn is the universal grammar-derived sequence transformer; it
replaces the per-grammar `Vec<T> -> Vec<U>` adapters that today live in
`host::primitives`.

### §3.3 — Anonymous functions with capture (F4+F5)

```bbnf
@host fn add_offset(offset: i32, xs: [i32]) -> [i32] {
    map(|x| x + offset, xs)
}
```

The closure `|x| x + offset` synthesises type `fn(i32) -> i32` (offset is `i32`
by capture). The closure inherits `'i` if any argument carries a borrowed slice;
the slice-borrow primary lifetime contract (`restart/README.md:298-314`) is
preserved because closures may not capture mutable state.

### §3.4 — Composition (F6) — point-free style

```bbnf
@host fn compose<A, B, C>(f: fn(A) -> B, g: fn(B) -> C) -> fn(A) -> C {
    |x| g(f(x))
}

# Usage
hex_to_rgb = /[0-9a-fA-F]{6}/ -> compose(parse_hex, hex_to_color);
```

The chain-edge rule (`restart/audit/pass-1-substrate/PASS-1.md:231`) accepts
`compose(parse_hex, hex_to_color)` as a single chain step whose arrow type is
`fn(&'i str) -> Color`. Point-free style is now expressible without dropping
out of the chain syntax.

### §3.5 — Pattern-match in `@host fn` body (F8+F9)

```bbnf
@host fn classify_token(t: Token) -> Category {
    match t {
        Token::Number(_) => Category::Numeric,
        Token::String(_) => Category::Textual,
        Token::Ident(s)  => if is_keyword(s) { Category::Keyword } else { Category::Identifier },
        _ => Category::Other,
    }
}
```

Exhaustiveness check uses `Token`'s variant set (grammar-derived per
`restart/README.md:115`). The wildcard `_` arm absorbs unmatched cases.

## §4 — The transducer worked example

The §3 of the orchestrator prompt asks: with the V1 folds this audit surfaces,
can a finite-state transducer be expressed as ordinary first-class generic host
functions? The answer is yes. Here is the complete derivation.

### §4.1 — The signature

```bbnf
@host fn transducer<I, O>(rules: [Rule<I, O>], input: I) -> O {
    # body in §4.3
}
```

`Rule<I, O>` is a generic rule (V1, per `restart/README.md:168-170`) with three
fields:

```bbnf
Rule<I, O> = struct {
    matches: fn(&I, usize) -> Option<usize>,    # (input, position) -> match length
    produces: fn(&I, usize, usize) -> O,        # (input, start, end) -> output fragment
    advance:  fn(usize, usize) -> usize,        # (cursor, match_len) -> new cursor
};
```

This needs F1 (`fn` arrow type), F3 (function-typed fields — admitted once F1
lands and `Type` may appear inside record types), F9 (tuple-position destruct
in records). It does not need a new `@directive`.

### §4.2 — The walker

```bbnf
@host fn walk<I, O>(rules: [Rule<I, O>], input: &I, init: O, append: fn(O, O) -> O) -> O {
    let mut pos = 0;
    let mut acc = init;
    while pos < input.len() {
        match first_match(rules, input, pos) {
            Some((rule, len)) => {
                acc = append(acc, rule.produces(input, pos, pos + len));
                pos = rule.advance(pos, len);
            },
            None => {
                acc = append(acc, default_emit(input, pos));
                pos = pos + 1;
            },
        }
    }
    acc
}

@host fn first_match<I, O>(rules: [Rule<I, O>], input: &I, pos: usize) -> Option<(Rule<I, O>, usize)> {
    for rule in rules {
        match rule.matches(input, pos) {
            Some(len) => return Some((rule, len)),
            None => continue,
        }
    }
    None
}
```

The walker is 18 lines of BBNF host-fn code. It compiles to ~30 lines of Rust
(matching the ffuzzy doc's "30 lines" estimate at `docs/ffuzzy.md:614`) and
applies to every transducer use case in `docs/ffuzzy.md:46-77`: trigraphs,
preprocessing, codemods, CSS vendor-prefix rewrite, lexer desugaring, ICU
phonetics, etc.

### §4.3 — The end-to-end use

```bbnf
@host fn transducer<I, O>(rules: [Rule<I, O>], input: I) -> O
    where O: Monoid                # provides empty + append
{
    walk(rules, &input, O::empty(), |a, b| a.append(b))
}
```

The `where O: Monoid` clause is a bounded-coercion obligation
(`restart/ARCHITECTURE.md:1153`); `Monoid` is a host trait registered in
`host::primitives`. CSP selects the witness via finite host-overload selection
(`restart/audit/pass-1-substrate/PASS-1.md:73`).

### §4.4 — A concrete instance (trigraph expansion)

```bbnf
@host fn trigraph_rules() -> [Rule<&str, String>] {
    [
        Rule { matches: |s, p| if s[p..].starts_with("??=") { Some(3) } else { None },
               produces: |_, _, _| "#".to_string(),
               advance: |p, len| p + len },
        Rule { matches: |s, p| if s[p..].starts_with("??(") { Some(3) } else { None },
               produces: |_, _, _| "[".to_string(),
               advance: |p, len| p + len },
        # ... seven more rules
    ]
}

@host fn expand_trigraphs(input: &str) -> String {
    transducer(trigraph_rules(), input)
}
```

Total surface: one generic `transducer` host-fn (reused for every transducer),
one `Rule<I, O>` record type (reused), and per-instance rule-set tables that
are pure data. No `@transducer` directive. No new IR node. No grammar-level
rewrite-mode.

The original ffuzzy proposal at `docs/ffuzzy.md:25-30` introduced
`@transducer <name> { rules }` as a new BBNF construct with a new `Transducer`
IR node, a new `TransducerDispatch` payload, and new dispatch-table compilation
machinery (`docs/ffuzzy.md:84-150`). The host-walker resolution at
`docs/ffuzzy.md:594-614` retreated to "30 lines on our side" but kept the host
walker outside the grammar — i.e., outside the "everything is grammar-derived"
anthem.

The audit's resolution: with F1-F9 folded, the transducer is expressible
inside a `.bbnf` source file as ordinary first-class generic host functions,
with no `@directive`, no new IR node, and no per-grammar host walker. The same
combinator surface that gives V1 `map`/`filter`/`fold` gives V1 transducers as a
free derivative.

## §5 — Cross-cutting concerns

### §5.1 — With sibling audit #1 (type system)

The sibling audit owns higher-rank, GADTs, row polymorphism. F1-F9 are
deliberately rank-1: arrows appear at first-order positions, generic parameters
quantify monomorphic types, no quantifier appears under an arrow. This
preserves the principal-scheme guarantee
(`restart/research/topic-1-hm-foundations.md:160-175`,
`restart/audit/pass-1-substrate/PASS-1.md:73-75`).

If sibling audit #1 recommends folding higher-rank polymorphism (rank-2
quantification), F3's `f: fn(T) -> U` would generalise to `f: forall T. fn(T) -> U`
permitting the `map` body to instantiate `f` at multiple monomorphic types
within one call. Dunfield-Krishnaswami algorithmic typing
(`restart/research/topic-2-bidirectional.md:136-150`) is the proof discipline.
The cluster does not require it for V1 — every example in §3 and §4 is rank-1
sound — but it is a natural extension if the sibling audit folds higher-rank.

If sibling audit #1 recommends row polymorphism (Leijen 2005 —
`restart/research/topic-1-hm-foundations.md:236-243`), generated typed records
gain open-row variance; F8's pattern-match would then need to handle the open
case (typically through an explicit `..` row-rest binding). For V1, finite
generated-shape coercion remains the contract per
`restart/ARCHITECTURE.md:1168-1171`.

### §5.2 — With the slice-borrow contract

Closure capture (F5) interacts with `&'i str` source slices
(`restart/README.md:298-318`). The fence: closures may capture by-reference on
`'i`; the closure type carries the `'i` lifetime; the existing forbidden
behavior table (`restart/ARCHITECTURE.md:1204-1208`) gains no new row beyond
"closures may not extend the lifetime of captured slice references beyond the
parser frame in which they are produced". This is mechanically enforced by
Rust's borrowck on the Rust backend; on the WASM backend, the codegen emits the
captured environment as an owned struct and a function pointer (the standard
fat-closure ABI), eliminating the lifetime question at the boundary.

### §5.3 — With the `@host fn` directive

F1-F9 are additive within the existing `@host fn` block-bodied form
(`restart/audit/pass-1-substrate/PASS-1.md:225`). No new directive. The
"directive count" stays at three (`@host fn`, `@error`, `@layout`) per
`restart/ARCHITECTURE.md:1070-1074`. The user's mandate ("solve ffuzzy
WITHOUT a new @directive") is satisfied.

### §5.4 — With the closure-semantics narrow framing

The ARCHITECTURE §8.4 four-role table (`restart/ARCHITECTURE.md:1193-1198`)
must be amended. The current framing — "Closure semantics are intentionally
narrow ... to model host chains and typed grammar mappings without turning
BBNF into a general programming language" (`restart/ARCHITECTURE.md:1187-1191`)
— is the load-bearing constraint that today blocks F4-F5.

The proposed amendment: the four roles become consumer-side type checks
(receiver expects `fn(T)->U`; the closure expression checks against that arrow)
rather than four distinct closure forms. The "without turning BBNF into a
general programming language" hedge is preserved through three fences kept
intact: (a) closures may not mutate parser input; (b) closures may not capture
arbitrary host process state; (c) closures may not emit runtime code from
Grammar IR. None of F1-F9 violates these; the unification is purely syntactic.

### §5.5 — With the cost model

Function-typed values (F2, F3) gain a `CostFacts` row keyed on the closure's
arity, capture set size, and indirection class
(`restart/audit/pass-1-substrate/PASS-1.md:79`). The cost model already scores
host calls (`restart/README.md:213-218`); the closure case extends the score
with a small indirection penalty plus the standard host-call cost. Pattern-match
(F8) extends `CostFacts` with arm count and dispatch-table candidacy — exactly
the existing Alt-dispatch cost shape (`restart/README.md:235`).

### §5.6 — With Lock 14 and onboarding

The two onboarding surfaces (`restart/README.md:9-13`) — grammar source plus
workspace metadata — gain richness without growing in count. With F1-F9, a new
grammar that needs a transducer or a higher-order combinator declares it inside
the `.bbnf` source as a `@host fn`; no per-grammar declaration crate is
required. The Lock 14 declaration-crate fence
(`restart/audit/pass-1-substrate/PASS-1.md:86-97`) stays empty for the nine
extant grammars and the yaml onboarding probe.

## §6 — Recommended V1 folds (sorted by greenfield value)

Ordered by audacity-times-impact, roughly. Each row notes the dependency edge.

| Order | Item | Depends on | Greenfield value | Implementation cost |
|---|---|---|---|---|
| 1 | **F1 — `fn(T) -> U` in `Type`** | none | enabling | tiny (one production, one IR variant) |
| 2 | **F2 — function values as first-class** | F1 | enabling | small |
| 3 | **F3 — function-typed parameters** | F1, F2 | apotheosis (replaces every per-grammar adapter; settles ffuzzy) | none beyond F1+F2 |
| 4 | **F4 — closures unified to one form** | F1 | apotheosis (Rust-like ergonomics) | small (collapse §8.4 four-role table; consumer-side type checks remain) |
| 5 | **F8 — `match` expression** | F9 (tuple); independent of F1-F5 | apotheosis (canonical variant dispatch) | small (one IR variant; exhaustiveness uses existing variant tables) |
| 6 | **F5 — closure capture by reference** | F4 | high (transducer rule-set context, point-free chains) | small (lifetime variable on arrow type) |
| 7 | **F9 — tuple expressions** | none | high (with F8: universal product type) | tiny |
| 8 | **F6 — composition as library** | F1, F2, F3, F4 | high (free derivative) | none — one entry in `host::primitives` |
| 9 | **F7 — partial application via closure-wrap** | F4 | medium (Rust convention) | none — derives from F4 |

All nine fold V1. None requires reopening the rank-1 HM commitment, the
Pierce-Turner local check/synth posture, the finite CSP scope, or the slice-borrow
contract. The combinator surface lights up in a single architectural movement.

## §7 — Open questions for synthesis

These do not block V1 fold but await synthesis ratification.

### §7.1 — Closure ABI on the WASM backend

The Rust backend emits closures as Rust closures and inherits Rust's
fat-closure ABI. The WASM backend's host-fn boundary
(`restart/audit/pass-1-substrate/PASS-1.md:65-71`, "Host/layout/error" row)
must marshal closure values across the host boundary. Two candidates: (a)
function-pointer index + captured-environment buffer (the standard wasm-bindgen
shape); (b) JavaScript-side closure with a host-fn shim. PASS-2 owns this
decision; the audit recommends (a) because it preserves the "no per-grammar
host crate" anthem.

### §7.2 — Mutable closures (`FnMut`) for V1?

V1 commits to `Fn` (immutable capture) only. A mutable-closure surface (`FnMut`
in Rust, `mut` capture in OCaml) would let `walk` accumulate by mutation rather
than by `Monoid::append`. The ergonomic gain is real; the type-system cost is
a `mut`/`Fn`/`FnMut`/`FnOnce` mode lattice. Recommendation: defer to a later
amendment unless authoring evidence demands it; `Monoid::append` is sufficient
for §4's transducer.

### §7.3 — Or-patterns and guards in `match`

V1's F8 admits literal/wildcard/constructor/tuple patterns with no guards and
no `|`-alternation across patterns. Or-patterns (`Foo(_) | Bar(_) => ...`) and
guards (`x if x > 0 => ...`) are standard Rust features but not load-bearing for
V1 transducers or combinators. Recommendation: defer.

### §7.4 — Effect tracking

A natural step beyond F4-F5 is to track which effects a closure may perform
(allocate, log, fail). Rust does this via `Result<T, E>` return types; V1 can
adopt the same convention without dedicated effect machinery. Recommendation:
no V1 effect lattice; failure is `Result<T, E>` (already implicit in the
`@error` directive's lowering).

### §7.5 — Sum types beyond grammar-derived enums

Grammar `Alt` rules generate enums (`restart/README.md:115`). A user-declared
`enum Foo { A, B, C }` inside an `@host fn` body would be a non-grammar enum,
unattached to a parsing rule. The audit does not recommend this for V1; every
sum type should trace to a grammar `Alt` (preserving "everything is
grammar-derived"). If `host::primitives` needs a non-grammar enum, it lives in
the host registry, not in the BBNF surface.

### §7.6 — `let` and recursive `let` in `@host fn` bodies

The `@host fn` body uses Rust-style `.method()` chaining
(`restart/audit/pass-1-substrate/PASS-1.md:225`); the body's exact statement
grammar is not pinned. The audit assumes Rust-shaped `let` (immutable),
`let mut` (V1: deferred per §7.2), `if`/`else`, `match`, and method-chain
expression — i.e., an expression-oriented subset of Rust. Recursive `let-rec`
is unnecessary for V1: top-level `@host fn` definitions are all mutually
visible inside the same grammar (`restart/README.md:155`), so direct and
mutual recursion are expressible without an inline `let-rec`.

### §7.7 — Pinning the surface in the formal grammar

If §6's folds land, `restart/audit/pass-1-substrate/PASS-1.md:194-223` and
`restart/ARCHITECTURE.md:1077-1112` need amendment:

| Production | Change |
|---|---|
| `Type` | add `\| FnType` |
| `FnType` | new: `"fn" "(" TypeList? ")" "->" Type` |
| `Primary` | add `\| FnRef \| Match \| Tuple` |
| `FnRef` | new: `"@" Ident` outside call position (or reuse `HostCall` with no parens) |
| `Match` | new: `"match" Expr "{" Arm+ "}"` |
| `Arm` | new: `Pattern "=>" Expr ","?` |
| `Pattern` | new: `Literal \| Ident \| "_" \| Constructor "(" Pattern* ")" \| "(" Pattern ("," Pattern)* ")"` |
| `Tuple` | new: `"(" Expr ("," Expr)+ ")"` (at least two elements; one-element is grouping) |

The `Closure` production at `restart/audit/pass-1-substrate/PASS-1.md:218`
needs no change; its consumer site changes (any `Primary` site accepting an
expected arrow type accepts `Closure`).

---

The cluster yields nine folds, all V1, all rank-1, all preserving the
slice-borrow contract and the rank-1 HM commitment. The ffuzzy transducer
problem is solvable without a new `@directive` — a single `transducer<I, O>`
host-fn plus a `Rule<I, O>` record type plus the F1-F9 surface suffices,
collapsing the original `@transducer` proposal at `docs/ffuzzy.md:25-150` to
ordinary functional composition.
