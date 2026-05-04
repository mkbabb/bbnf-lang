# Restart-of-the-Restart — Interrogation

Date: 2026-05-04
Status: questions for the user; awaiting answers.
Audience: the user (decision-maker) + this conversation's continuation (the agent that drafts new prompts + new structure + new migration plan once these settle).

A note on count: I said "31 questions" in conversation; the actual total below is **35**. The miscount was mine. The questions are unculled — every one was load-bearing enough to surface.

---

## Gestalt as I distill it from the corpus

A **grammar-driven parser generator** producing SOTA-class typed parsers across multiple backends (Rust now; TS, WASM later). The user-facing API is **familiar** — sonic-rs lazy-value idioms, lightning-css visitor idioms, jq-style path access. The internals are the apotheosis: a CSP-backed bidirectional type system; an e-graph-driven rewrite engine; a shape miner that auto-detects Pratt and SIMD opportunities; a cost model unified across the parser and the regex engine; an IR + per-backend lowerer per Lock 5. **Everything is grammar-derived** — the value type, the document type, the runtime module, the parser code, the host-fn signatures. Per-grammar artefacts emerge from the grammar source + workspace metadata + (optionally) host-fn implementations expressed as composition of generic primitives. No parallel hand-written Rust mirrors of grammar shape. No per-grammar match arms in generic crates. No tape rebranding. Slice-borrow primary; bumpalo opt-in; owned escape.

The substrate identity: source → IR → optimised IR → typed-IR (per Lock 5) → per-backend source. Each phase has explicit input and output types; each transformation is a composable pass; the e-graph is the rewrite substrate; the CSP is the inference substrate; the cost model picks emission shapes.

## Observations from the corpus that inform these questions

1. **Lock 1 + Lock 13 + Lock 14 do not retire as one** (Stage-2 PASS-B finding). Lock 13 (god-directories, file-size discipline) is mechanical, lands first. Locks 1 + 14 land together at the runtime template + grammar-derived value type. Sequencing the stagger is consequential.

2. **The 168K-LOC generated tree is grammar inflation, not necessity.** Five trivial cohort grammars carry hand-written runtime modules near-identical to one another; CSS L4's 107K LOC includes a 14-variant builder enum that's per-construct overfit. A single grammar-agnostic generator template should subsume both.

3. **The "tape dies" lock retracted the NAME, not the structural insight.** simdjson's contiguous-token-stream-with-offset-references is a real cache-locality win. Lock 1 forbids tape rebranding; it does not forbid contiguous output. The greenfield must distinguish.

4. **The IR is currently fractured incoherently** — bbnf-ir carries types AND passes AND registry AND strategy. Pass A's fracture proposal (`bbnf-ir` types only, `bbnf-passes` transformations only, `bbnf-vm` bytecode VM) is sound but needs sharpening: where do egraph rewrites live? Where does shape mining live?

5. **The cost model has no canonical home today.** It's implicit in emission strategy selection. Greenfield surfaces it as a first-class crate (or sub-crate) consumed by codegen + regex + path-DSL.

6. **CSP propagation is incomplete.** The audit found 9-arm allow-lists in `bbnf-ir/registry/strategy.rs` that *should* derive from CSP. The greenfield has the chance to make this universal.

7. **Per-grammar declaration crates are overfit** (Amendment 01). Per-grammar host-fn implementations decompose into composition of generic primitives + workspace metadata. No `crates/<grammar>/` is needed for any of the 9 extant grammars.

8. **Path-DSL is currently underspecified**. sonic-rs `pointer![]` and lightning-css `Visitor + VisitTypes` solve different problems (random access vs traversal). The greenfield needs both, ideally with a shared substrate.

---

## The interrogation — 35 questions

### Group A — Crate & Module Architecture

**1. What is the canonical workspace partition?** The audit converged on ~24 crates; my synthesis would consolidate further. Do you favour:

- (i) **maximalist split** — one crate per concern (bbnf-error / bbnf-pipeline / bbnf-source / bbnf-grammar / bbnf-ir / bbnf-passes / bbnf-egraph / bbnf-csp / bbnf-cost / bbnf-codegen-ir / bbnf-codegen-rust / bbnf-codegen-ts / bbnf-codegen-wasm / bbnf-runtime / bbnf-runtime-template / bbnf-host-prims / path / path-core / path-ts / parse-that / bbnf-regex / simd-scan / bbnf-bench / bbnf-language-server / bbnf-cli / bbnf — ~26)
- (ii) **balanced split** — ~12-15 crates with cohesive concerns (bbnf-source-parse-ir merged; bbnf-codegen carries all backends; bbnf-runtime carries template + per-grammar dirs)
- (iii) **monolithic core + sister optimisers** — one `bbnf` core crate carrying source/parse/IR/codegen/runtime; sister crates for egraph, csp-solver, parse-that, bbnf-regex, simd-scan only

I lean (ii). The optimizer-orthogonal-composition (Lock 4) is real but doesn't require crate boundaries; module boundaries within `bbnf-passes` suffice. Maximalism creates 26-way `cargo check` overhead and version-skew fault surface for marginal architectural gain.

**2. Where does the grammar source tree live?** Three options:

- (i) `grammar/` at workspace root (current state)
- (ii) `bbnf-grammar/grammar/` under a crate (co-located with grammar metadata reading)
- (iii) `bbnf-test-fixtures/grammar/` (treated as a fixture, not source)

The ramification: grammar files are *inputs* to the build, not Rust source. (i) is least surprising; (ii) couples them to a crate; (iii) is wrong (grammars aren't tests). I lean (i).

**3. What is the dependency DAG ceiling?** The deepest path under (ii) is roughly: bbnf-cli → bbnf → bbnf-codegen → bbnf-passes → bbnf-egraph → bbnf-ir → bbnf-error. Six hops. Do you accept that depth, or want flatter (e.g., a single trait-based plugin registry that lets passes register without bbnf-codegen knowing each pass)?

**4. Should there be a single user-facing `bbnf` aggregator crate, or do users compose multiple crates?** sonic-rs ships one crate; lightning-css ships one crate; serde ships multiple. I lean one (`bbnf`) for users + the granular crates for internal composition. The aggregator re-exports `bbnf::Parser`, `bbnf::Value`, `bbnf::Document`, `bbnf::Visitor`, `bbnf::pointer!`.

**5. Where do per-grammar artefacts emit?** Under Amendment 01: template-emitted subdirs at `bbnf-runtime/src/grammars/<name>/{generated.rs, runtime.rs}`. But the parser itself emits where? Three options:

- (i) `bbnf-runtime/src/grammars/<name>/parser.rs` (co-located with runtime)
- (ii) `bbnf-codegen/src/generated/<name>.rs` (under the codegen crate)
- (iii) `bbnf-parsers/src/<name>.rs` (a dedicated crate for emitted parsers)

I lean (i) — co-locating parser + runtime per grammar inside `bbnf-runtime` keeps the grammar's footprint cohesive. Emit *output* lives near runtime *consumption*.

**6. Does `bbnf-host` exist as a separate crate, or fold into `bbnf-host-prims`?** The host-fn registry mechanism is generic (dispatch + lookup); the primitives (parse_int_radix, parse_hex_pair, etc.) are the implementations. They could be one crate or two. I lean one (`bbnf-host`) with `prims` as a module — fewer crates is cohesion.

**7. What lives in `bbnf-vm`?** Pass A proposed it. But: do we actually have a bytecode VM? Or is the runtime substrate enough? If `bbnf-vm` is "the parser execution engine" (state machine + cursor + stack), it might fold into `bbnf-runtime`. If it's "a typed IR interpreter for testing without codegen," it's separate. Which?

**8. Sister crates: which path-deps, which workspace members, which external?** Today: `egraph`, `egraph-derive`, `csp-solver`, `simd-scan`, `parse-that`, `bbnf-regex`. Lock 11 says path-deps until stable, then publication. Do we publish *all* of them eventually, or is `parse-that` permanently private (per BC.W5c)? Does `bbnf-regex` keep its `bbnf-` prefix or become a generic `pcre-rust`-class regex crate?

### Group B — IR & "Everything Grammar-Derived"

**9. How many IRs are there?** Three candidates:

- **Grammar IR** (parsed `.bbnf` → AST of rules)
- **Optimised IR** (post-passes; CSP-typed; cost-annotated)
- **Typed IR** (per Lock 5; the codegen contract; 22-variant table from BC.W0)

Are these three distinct types, or one type with three usage modes? sonic-rs has zero IRs (hand-written); chumsky has typed combinators (also no IR); lalrpop has tables. We are unique in having *typed grammar IR* as the substrate. I lean **two IRs**: Grammar IR (input + optimization domain) and Backend IR (codegen contract). The "optimised IR" is just Grammar IR with extra metadata, not a distinct type.

**10. The grammar-derived value type** — how is it expressed? Four options:

- (i) **Tagged enum per rule** — `JsonValue<'i> = Object(...) | Array(...) | String(Cow<'i,str>) | …`; one enum per top-level rule
- (ii) **Generic typed wrapper** — `Value<'i, G: Grammar>` where the type tag carries grammar identity
- (iii) **Sum-of-fields** — every rule generates a struct with named fields; types compose
- (iv) **Hybrid** — leaf rules become slice/scalar wrappers; compound rules become structs; alternation becomes enum

I strongly lean (iv). simdjson's tape doesn't model semantic types; sonic-rs's serde-derive-style approach uses (iii); chumsky's `Parser<I, O>` is essentially (iv). Grammar shape dictates which.

**11. What is NOT grammar-derived?** The anthem demands a small, explicit list. Candidates:

- The 14 locks themselves (architectural commitments)
- Workspace metadata schema (the TOML grammar that declares grammars)
- Generic primitives (`parse_int_radix`, `parse_hex_pair`, `cow_unescape`, etc.)
- The host-fn dispatch mechanism (generic)
- The IR types (grammar-IR is generic; only its instances are grammar-specific)
- The egraph / CSP / cost-model substrates (generic)
- The error type vocabulary (generic; per-grammar errors carry grammar ident as data)

If anything else is hand-written per-grammar, it's an overfit. Confirm this list, or extend.

**12. Host functions — how do they plug in without breaking the anthem?** Four options:

- (i) **Composition in workspace metadata** — `parse_hex_color = compose(regex("#[0-9a-fA-F]{6}"), parse_hex_pair, parse_hex_pair, parse_hex_pair, Color::Rgb)`. Declarative, no Rust code per grammar.
- (ii) **Extended-BBNF directives** — `@host fn parse_hex_color(s: regex("#[0-9a-fA-F]{6}")) -> Color { Color::Rgb(parse_hex_pair($1[1..3]), …) }`. In-grammar, declarative, expressive.
- (iii) **Optional declaration crates** — `crates/<grammar>/src/host.rs` carries `pub fn parse_hex_color(...) -> Color`. Lock 14 escape valve.
- (iv) **All three** — composition first; directive second; declaration crate as last resort.

I strongly lean (iv) but with explicit thresholds: composition is preferred; directive when composition syntax becomes unreadable; declaration crate only when a host fn requires linking external C libraries or non-pure logic.

**13. The grammar source itself — does BBNF need extension?** Today's BBNF supports `->` for type annotation, alternation, sequence, repeat, optional, charclass, regex literals. Greenfield candidates:

- `@host fn ...` — host-fn directive (per Q12)
- `@layout(struct | enum | tuple)` — explicit layout hint when inference is ambiguous
- `@cost(low | medium | high)` — cost-model hint (or auto-derive only?)
- `@simd(...)` — Lock 10 forbids; auto-detect only
- `@pratt(...)` — Lock 10 forbids; auto-detect only
- `@error(skip | recover | halt)` — error-recovery directive
- `@lazy(true | false)` — lazy materialisation hint
- Generic rules — `Object<V> = "{" pair<V> ("," pair<V>)* "}"` — currently absent from BBNF

Which, if any, do we add? Generic rules + @host fn + @layout + @error are the four I'd consider. Confirm or amend.

### Group C — Optimization Apotheosis

**14. The pass ordering — what is the canonical pipeline?** I propose:

1. **Parse** (`.bbnf` → Grammar IR)
2. **Validate** (well-formedness; reachability; left-recursion classification)
3. **Type inference** (CSP-backed; bidirectional; produces TypedGrammarIR)
4. **Shape mining** (recogniser miners; identify Pratt operators, SIMD scanners, PHF keywords)
5. **E-graph saturation** (rewrite rules; algebraic simplification; commutativity; absorption)
6. **Cost-model extraction** (e-graph → optimal-cost AST per cost model)
7. **Lower to backend IR** (TypedGrammarIR → 22-variant Typed IR)
8. **Per-backend lower** (Typed IR → Rust source / TS source / WASM bytes)
9. **Regen-equality verification** (xtask --check; byte-identical re-emission)

Is this the right order? Particularly: should validation happen before *or* after type inference? Type inference depends on validation (left-recursion class). Validation might depend on type inference (typed left-recursion). They may need fixed-point co-iteration.

**15. The cost model — local vs global, what's the unit?** Options:

- (i) **Per-construct** — every Alt, Seq, Repeat carries a cost; cost model picks emission shape per node
- (ii) **Per-rule** — every rule's cost is the sum of its body's costs; choices made at rule boundary
- (iii) **Per-path** — costs propagate along grammar-DFS paths; choices made at fork points
- (iv) **Hybrid** — local costs per construct; global cost rolls up per rule; per-path used for cycle-cost amortisation in left-recursive operator chains

I lean (iv). Local costs feed the e-graph extraction (per-construct cost picks per-construct shape); global costs feed the strategy resolver (which rules emit Pratt vs descent); per-path costs handle Pratt LUT propagation.

**16. The regex cost model — how does it integrate?** `bbnf-regex` has its own cost model (DFA size; backtrack risk; state count). The parser cost model picks emission shapes (PHF vs DFA vs scan). Are these:

- (i) **Two separate cost models** with bridging (regex tells parser "scan this is X cheap" / "scan this is Y expensive")
- (ii) **One unified cost model** with both regex and parser concerns folded into a single vocabulary
- (iii) **Cost model as a trait** — both implement; specifics differ; trait gives shared analytical surface

I lean (iii) — `Cost` is a trait with `score(&self, ctx: &Context) -> u64` and `branches(&self) -> impl Iterator<Item = (Choice, u64)>`; regex implements; parser implements; cost-model crate provides comparison logic.

**17. The e-graph rewrite system — what rewrites?** Concrete categories:

- **Algebraic** — `(a | a)` → `a`; `(a, ε)` → `a`; `(a*)?` → `a*`; `(a+)*` → `a*`; ε-elimination
- **Charclass merging** — `[a-z] | [A-Z]` → `[a-zA-Z]`
- **Keyword sets** — alternation of literals → PHF candidate
- **Operator-chain detection** — left-recursive `expr := expr "+" expr | term` → Pratt
- **Repeat-loop hoisting** — `(item separator)*` → repeat-with-separator construct
- **Tail-call elimination** — `rule := A rule` → loop
- **Non-progressing-Alt removal** — alternatives whose FIRST sets prove empty

Which subset is table-stakes for V1? I'd argue algebraic + charclass-merging + keyword-set detection + operator-chain detection — the four highest-leverage. Repeat-loop hoisting and tail-call elimination defer to later tranches.

**18. The CSP solver — what does it solve?** Today's CSP carries strategy variables (per-rule emit choice). Greenfield expansions:

- **Type inference** (per-rule layout; per-binding type; bidirectional propagation)
- **Cost minimisation** (extraction cost from e-graph)
- **Lifetime inference** (per-binding `'i` vs `'static` vs owned vs Cow)
- **Layout selection** (per-rule struct vs enum vs tuple vs slice)
- **Backend constraints** (per-rule "this rule cannot SIMD because backend X doesn't support Y")

The CSP becomes the central inference substrate. Is this the right scope, or is it too much (CSPs are NP-hard; saturation-style egraph extraction has different tractability)?

**19. Shape mining — what's the API?** A shape miner observes Grammar IR + emits hints:

```rust
trait ShapeMiner {
    fn name(&self) -> &str;
    fn observe(&self, ir: &GrammarIR, ctx: &Context) -> Vec<Hint>;
}
```

Where `Hint` is `{ rule_id, kind: HintKind, weight: u64 }`. The cost model consumes hints. Plug-in via inventory or `linkme`. New miners (per Lock 14) register without code edit in generic crates. Confirm shape, or amend.

### Group D — Type System & Bidirectional Inference

**20. The canonical bidirectional inference algorithm.** Three plausible:

- **Pierce-Turner local type inference** (the classic; modest power; pleasant errors)
- **OutsideIn(X)** (GHC's flavour; elaborate; supports type classes / traits)
- **Algebraic subtyping** (Stephen Dolan's work; powerful; row polymorphism)

For BBNF, types are: products (Seq), sums (Alt), repetitions (Repeat → Vec), optionals (Optional → Option), slices (terminals). No higher-kinded polymorphism. No type classes (host-fn dispatch is by name + signature, not class). Pierce-Turner is sufficient. OutsideIn(X) overkill. Algebraic subtyping interesting if grammar-rule-as-row is a model — but probably yagni.

I lean **Pierce-Turner adapted to grammar shape**, with CSP backing for the constraint-collection phase.

**21. Type annotation surface in `.bbnf` files.** Today: `rule -> TypeName`. Greenfield expansions:

- (i) Stay terminal-only — `rule -> u32`, `rule -> Color`, etc. Types are leaf-level only
- (ii) Allow compound — `rule -> Vec<u32>`, `rule -> Option<String>`. Types are first-class
- (iii) Allow user-defined — `type Foo = struct { a: u32, b: String }; rule -> Foo`. Types as named declarations
- (iv) Pure inference — no annotations needed; types fully derive from rule shape

I lean (iv) by default, (i) for terminals where inference is ambiguous. (ii) and (iii) introduce parallel hand-written type declarations the anthem forbids. The leaf annotation `rule -> u32` is the *parse-result type* of the leaf, which CANNOT be derived from grammar shape (the grammar says "regex `[0-9]+`"; the type system needs to know "this is an integer, parse to u32"). So leaf annotations stay.

**22. Generic rules — yes or no?** `Object<V> = "{" pair<V> ("," pair<V>)* "}"; pair<V> = String ":" V`. Today's BBNF doesn't support this; every grammar repeats object structure per value type. Adding generics:

- Pros: DRY; faster grammar authoring; smaller grammar source
- Cons: type system complexity; CSP variables proliferate; codegen monomorphisation (Rust handles; TS handles via type parameters; WASM unclear)

I lean *yes* but defer to a later tranche — V1 ships without generics, V2 adds. The cost is one generic per common pattern (Object, Array, KeyValue) repeated across grammars. Not zero, but not blocking.

**23. Subtyping & coercion.** Does `i32` coerce to `i64`? Does `&'a str` coerce to `Cow<'a, str>`? Three options:

- (i) No coercion — explicit; verbose; safe
- (ii) Numeric tower (i32 → i64 → f64) + lifetime coercion (slice → cow → owned)
- (iii) Full Hindley-Milner-style inference with subsumption

I lean (ii). Coercions cover the cases users actually want; full inference is yagni. The CSP supports coercion as constraint relaxation.

### Group E — Value API & Path DSL

**24. The path DSL syntax.** Sonic-rs uses `pointer![key1, key2, 0, key3]`. Lightning-css uses CSS-selector-style `>>` for descendant matching. JSONPath uses `$.foo[0].bar`. We have three viable substrates:

- (i) **JSONPath-inspired** — string literal at runtime: `value.get("$.foo[0].bar")`
- (ii) **Sonic-rs pointer macro** — compile-time AST: `pointer![Json, "foo", 0, "bar"]`
- (iii) **Selector DSL** — XPath/CSS-style: `select![Css, "rule > declaration[property=color]"]`
- (iv) **Type-driven access** — Rust syntax: `value.foo[0].bar` via Index/IntoIterator (limited but elegant)

For a generic grammar, what does "path" mean? JSON's path is `key + index`. CSS's path is `selector tree pattern`. They are different paradigms. We need both:

- **Pointer access** (compile-time-typed; key/index into known structure) — like sonic-rs
- **Visitor traversal** (runtime; bitflag-pruned; pattern-matched) — like lightning-css

Two surfaces, one substrate. I lean (ii) + (iii) both supported, both compile-time, both grammar-derived. The macro `pointer!` works for any grammar by reading its registry; the macro `select!` parses a selector DSL against the grammar's structure. Confirm, or amend.

**25. Lazy materialisation strategy.** sonic-rs's `LazyValue<'a>` borrows a slice; materialisation happens on `.as_str()`, `.as_i64()`, `.as_<T>()`. Three flavours:

- (i) **Bytewise lazy** — value is `&'i str` until materialised
- (ii) **Token-lazy** — value carries a token-offset reference into a token stream; materialises on demand
- (iii) **Tape-lazy** (the simdjson model) — value carries an offset into a contiguous tape; materialises on demand

(iii) is the fastest for batch parse + selective access; (i) is simplest. Lock 1 forbids tape rebranding but allows the underlying contiguous representation. Are we OK with token-stream-with-offset-references as long as it's not called tape?

I lean yes — call it **TokenStream** or **ParseStream**; structurally it is what simdjson invented; the name "tape" carries the AU-era baggage but the structural insight is sound.

**26. The materialisation API surface.** `value.as_str()`, `value.as_i64()`, `value.as_<T>()` — sonic-rs / chumsky idiom. Also `value.try_into()` (TryFrom-based). Also typed-property access (`value.foo.bar`). Also visitor (`visitor.visit(value)`).

For each grammar, what's the right combination? JSON: as_str / as_i64 / as_array / as_object + pointer + visitor + typed-property. CSS L4: visitor (lightning-css idiom; selector-pattern; mutation). Sheets: typed-property + visitor.

I lean: **all four surfaces** uniformly across grammars; the cost is per-grammar codegen size; the user picks per use-case. The codegen synthesises all four from grammar shape.

**27. Mutation API.** Read-only or read-write? sonic-rs is read-only on parsed values. lightning-css visitor is read-write. JSON typically wants read-write for transformations. CSS always wants read-write (the visitor IS for mutation).

I lean: **write through visitor only.** Direct typed-property mutation is unsound under slice-borrow (mutation could invalidate borrowed slices). The visitor pattern with explicit `&mut Value` works because the visitor controls lifetime. Sonic-rs avoids this by being read-only; lightning-css addresses it by making the visitor the canonical mutation surface. Confirm.

### Group F — Performance & Backend Ecosystem

**28. Performance gates per backend.** Rust gates anchor against sonic-rs / simdjson / lightning-css per Lock 8. TS and WASM gates anchor against …? Options:

- TS: **simdjson-node**, **JSON.parse**, **fast-xml-parser**
- WASM: **simdjson-wasm**, **rust-wasm baseline**, **AssemblyScript implementations**

What's the gate? Beat the JS-native? Beat the wasm-bindgen overhead? BD's existing draft anchors at NAPI-bound 8ms / WASM-baseline 2.5ms for twitter.json. Confirm, or set sharper targets.

**29. SIMD on which platforms?** ARM NEON (M1/M2/M3) + x86 AVX2/AVX-512 + portable scalar fallback. WASM SIMD (wasm-simd128) emerging. Decision: which are first-class, which are best-effort?

I lean: **NEON + AVX2 first-class** (covers M-series Mac + most x86); AVX-512 best-effort (server-class); WASM-SIMD opportunistic (when shipped); scalar fallback always.

**30. Incremental parsing — yes, no, or deferred?** Treesitter's incremental edits are valuable for LSP; cost is tree-as-substrate (every parsed node has stable identity; edits diff against prior tree). simdjson does NOT do incremental. sonic-rs does NOT do incremental. lightning-css does NOT do incremental.

For a parser-generator targeting batch + LSP scenarios, incremental is desirable but expensive. Three options:

- (i) **No incremental** — V1 doesn't ship it; LSP relies on full re-parse + diff
- (ii) **Incremental as opt-in feature** — generated parsers carry an `--incremental` mode; trades compile speed + binary size for incremental parse
- (iii) **Incremental always** — every generated parser supports incremental; cost is universal

I lean (i) for V1 + (ii) for V2. Treesitter's value is real; not table-stakes for V1.

### Group G — SOTA Synthesis

**31. Beyond sonic-rs / simdjson / lightning-css, what other projects to study?** My list:

- **Treesitter** (Cargill / GitHub) — incremental parsing; error recovery; query DSL
- **rust-analyzer** — Salsa-style query system; incremental computation; LSP; ungrammar declarative grammars
- **swc** — hand-written parser; transformer; codegen separation; NAPI bindings; fast iteration
- **chumsky** — typed parser combinators; error recovery; Pratt parsing; Rust-idiomatic
- **lalrpop** — LALR parser-generator with codegen; type-driven; mature
- **pest** — PEG parser-generator; derive-macro surface
- **nom** — combinator parsing; streaming; error recovery
- **rowan** — syntax tree library used by rust-analyzer; lossless; concrete syntax tree
- **logos** — fast lexer-generator with derive-macro; SIMD-aware
- **regex-automata** — DFA / NFA / hybrid regex engines; the de-facto Rust regex library
- **egg** (egraph library) — the e-graph substrate sister crate
- **z3** — SMT solver; reference for CSP propagation algorithms
- **antlr4** — older but widely-used; LL(*) parsing; multi-target codegen (Java/C#/Python/JS); error reporting
- **megaparsec / parsec** (Haskell) — Hindley-Milner-typed combinators; reference for type system
- **GLR / Earley** parsers — for ambiguous grammars; not our target but informs the recogniser space

Which of these are deep-dive material vs awareness-only? I'd deep-dive: **rust-analyzer** (Salsa, ungrammar, rowan), **chumsky** (types-out-the-back, Pratt), **logos** (lexer codegen idioms), **regex-automata** (regex sharing), **egg** (e-graph).

**32. From treesitter, which ideas to adopt?** Concrete candidates:

- **Error recovery via "MISSING" / "ERROR" nodes** — V1 desirable; aligns with LSP needs
- **Query DSL for tree pattern matching** — pairs with our path-DSL question (Q24)
- **Incremental parsing** — V2 (per Q30)
- **Concrete vs abstract syntax tree distinction** — rowan's lossless model; preserves whitespace + comments — required for prettify (gorgeous-style)
- **External scanners** — escape hatch for grammars whose lexing exceeds BBNF expressiveness — likely needed for CSS / Sheets

I lean: **error recovery + lossless CST + external scanners** for V1; query DSL folds into our select! macro (Q24); incremental defers to V2.

**33. From simdjson, which ideas to adopt?** Concrete:

- **Tape representation** — Lock 1 retracts the *name*; the structural insight (contiguous-token-stream-with-offset-references) is sound. Adopt, rename to TokenStream / ParseStream.
- **Two-pass parse (structural scan + value materialisation)** — relevant for SIMD scanner stage
- **On-demand API** — simdjson's lazy API; aligns with our LazyValue surface (Q25)
- **Escape-handling SIMD** — bytewise algorithmic primitives; goes into `bbnf-host-prims` / `simd-scan`
- **NUMA-aware allocation** — too platform-specific; defer

I lean: **adopt all four** structural insights; rename tape → TokenStream; absorb on-demand idioms into LazyValue.

### Group H — Process & Restart Sequencing

**34. Restart-of-restart sequencing.** Three options:

- (i) **Archive first, plan from blank** — `restart/` becomes `restart-archive-2026-05-04/`; new prompts produce new tranches from scratch with BA/BB/BC/BD inheritance as reference only
- (ii) **Plan first, archive at execution** — keep current `restart/` accessible during planning; archive when V2 master plan ratifies
- (iii) **Side-by-side** — both `restart/` and `restart-v2/` (or whatever name) coexist; eventual collapse

I strongly lean (i). The compounded `restart/` is itself a contrivance the user has flagged. Archive cleanly. Start `restart/` again with the lessons but not the artefacts.

**35. Prompt-suite shape under the new gestalt.** Five prompts: 3 PASS + 1 synthesis + 1 hardening. The current 3-PASS (parse-front / codegen-mid / periphery) partition is *crate-scope-based*. A better partition might be *concern-based*:

- **PASS-1 — Substrate** — IR types, type system, CSP, e-graph, shape mining, cost model, grammar source schema, host-fn primitives, error vocabulary
- **PASS-2 — Codegen & Backends** — codegen IR, per-backend lowerers (Rust + TS + WASM), runtime template, generated-output regen, backend ABI
- **PASS-3 — User Surface & Ecosystem** — value API, path/select DSL, visitor surface, lazy materialisation, error reporting, LSP, CLI, docs, fixture infrastructure

OR a *risk-based* partition:

- **PASS-1 — Greenfield Audit** — what survives, what dies, what reinvents from current bbnf-lang corpus
- **PASS-2 — Architecture Design** — the 14 locks applied; SOTA synthesis; concrete crate + module shape
- **PASS-3 — Migration Plan** — file-by-file disposition; tranche allocation; risk/calendar/dependency

OR a *layer-based* partition:

- **PASS-1 — Bottom (substrate + IR + types)**
- **PASS-2 — Middle (optimisation + codegen)**
- **PASS-3 — Top (runtime + API + ecosystem)**

I lean **layer-based** for clarity; or **concern-based** for cohesion. NOT crate-scope-based (the prior PASS-A/B/C were that, and the audit found they over-partitioned).

---

## After your answers

When the 35 questions resolve into settled positions, I will:

1. Archive `restart/` to `restart-archive-2026-05-04/`
2. Write a fresh `restart/` containing:
   - **README.md** — gestalt synthesis (one document; ~200-400 lines)
   - **architecture/** — the new crate structure + module structure + dependency DAG + per-file rationale (~500-1,000 lines)
   - **migration.md** — formalised plan for every file in current `crates/` (abrogate / delete / rewrite / add / move) (~800-1,500 lines)
   - **prompts/** — 3 PASS + 1 synthesis + 1 hardening (~1,500-2,500 lines)
   - **inheritance/** — BA / BB / BC / BD wave-by-wave summary of what survives (~500-800 lines)

Then dispatch the prompts. Then synthesis. Then hardening. Then tranches. No double-back, no Stage-2, no meta-meta. One round.

Answer the 35 questions in any order. Skip those you don't want to settle yet (they become questions the prompts themselves surface). Direct disagreement with my leans is welcome — they're prompts for your decision, not concluded positions.
