# LAYOUT-HM-REVIEW: adversarial second reading of LAYOUT-HM.md

Second reader, 2026-11-15. Read-only on code; nothing compiled or run. Subject: `LAYOUT-HM.md` at `f52cf1f09` (cited as `D§n`). Read in full: `D`, `restart/locks/LOCKS.md:71-662`, `restart/ARCHITECTURE.md` §7.1-§7.3, §7.5 head, §8 (`ARCH`), `restart/audit/pass-1-substrate/PASS-1.md:89-130`, `:238-265` (`PASS-1`). Code opened: `crates/core/src/compiler/{mod,typecheck,analysis,budget,emit}.rs`, `grammar/bbnf/{bbnf,expressions,types}.bbnf`, the `->` lines of all 33 corpus grammars, `skinny/crates/passes/src/lib.rs:88-125`, `SK/LEDGER.md:465`, `:519`.

Every counterexample below was walked by hand against the rules as `D` writes them. Where the walk depends on a reading of `D`, the reading is stated.

Ranking: **F** fatal (the design as written cannot meet its own gate or claim), **M** must-fix before any code, **S** should-fix, **N** note.

---

## 1. Findings

### Fatal

**F1. Rule ALT has no principal types and rejects typeable programs. Attacks D§4.2 (ALT), D§4.6 (the claim), D§6.1 (L-PRINCIPAL).**

(a) Not closed under substitution. F04 `alt2 = |x, y| x | y` gets `∀αβ. fn(P α, P β) → P Sum[α, β]`. The typing `fn(P Span, P Span) → P Span` is derivable by the same rules at those monotypes (it is G04). It is not an instance of the scheme: `[α, β := Span]` gives `P Sum[Span, Span]`. Two valid typings, no common generalisation. Consequence the user sees: `r = alt2("a", "b")` is `Sum[Span, Span]`, and `r = "a" | "b"` is `Span`. Naming an alternation changes its type.

(b) Incomplete. `pick = |p, q| (p | q) -> |s| len(s) ;`. Walk: `p : P α`, `q : P β`, `Alt(δ; α, β)`; `len(s)` gives `δ = Span`. Step 1: `α`, `β` are not identical. Step 2: `δ := Sum[α, β]`. Step 3: `Span = Sum[α, β]`, clash, type error. The typing `α = β = Span` exists and is what the inlined program gets. A second one, with the error an occurs failure the user cannot read: `k = |f, x| f(x | f(x)) ;` gives `δ = α`, then `α := Sum[α, β]`. Information flows from branches to result only; a constraint on the result never reaches the branches.

(c) D§4.6 answers (a) by defining "principal" as "principal in the declarative system that includes ALT's resolution order". No declarative system is written anywhere in `D`; the only definition is the algorithm. A scheme is trivially principal among the outputs of the algorithm that produced it. L-PRINCIPAL (b), "every enumerated typing is an instance of the inferred one", is false on F04 for any honest enumerator and vacuous for one that re-runs the algorithm.

Confluence itself: I could not break it. For a fixed equality set, step 1 is a least fixed point of a monotone operator and step 2 is simultaneous, so traversal order does not change the answer. The defect is not order-dependence; it is that the answer is not stable under instantiation and not complete.

*Smallest change.* Type parametric rules at their instances, not at their definitions. Layer (ii) already monomorphises by memoised instance (D§5.2), so every alternation the generator emits has ground branch types and ALT becomes "identical ground types collapse", which is trivially confluent, complete and principal. Keep HM schemes for the value layer, which has no ALT. If definition-site schemes are kept for early errors, write the declarative rule, state in D§4.6 that ALT forfeits principality for any definition with an alternation over a type variable, and delete L-PRINCIPAL (b) for those.

**F2. The design's own verifier law rejects the design's own expected outputs. Attacks D§4.9 (`AltResolved`), D§4.4 (DEFAULT), D§6.1 (L-CERT).**

`AltResolved`: "a collapsed alternation has identical branch types; an uncollapsed one does not." Three outputs `D` itself requires or implies violate it:
- F04's instance `r`: `Sum[Span, Span]`, uncollapsed, identical branches.
- `e = ("a" -> nil()) | ("b" -> nil()) ;`: branches `List α₁`, `List α₂`, distinct variables, step 2 gives `Sum`, then DEFAULT gives `Sum[List Unit, List Unit]`. One branch alone (X01) gives `[Unit]`.
- `r = ("a" -> |v| loop(0)) | ("b" -> |v| loop(0)) ;` with X02's `loop`: `Sum[Unit, Unit]`.

So L-CERT ("the checker accepts every solver output") fails on the corpus, or `AltResolved` must be weakened to "at the moment of resolution", which a re-reading verifier cannot check without re-running resolution, which D§4.9 forbids ("no unification, no search").

*Smallest change.* With F1's cure the law holds as written. Without it: record per alternation node the pre-default branch terms in the certificate and state the law on those.

**F3. The typing rules give no meaning to the non-constant `->` forms the generated grammars actually use; G-DIGEST cannot pass as written. Attacks D§4.2 (`e -> v`), D§4.3, D§4.6 ("never trusted"), D§7 H2 gate (d).**

Corpus lines, re-read: `number = /…/ -> f64 ;` (`grammar/json/json.bbnf:4`, `css/l4/value-unit.bbnf:15`, `google-sheets.bbnf:6`); `integer = /…/ -> i64 ;` (`value-unit.bbnf:16`); `-> input : Color` ×3 (`css/l4/color.bbnf:228`, `:255`, `:303`); `-> crate::css_types::parse_hex_color(input) : u32` (`color.bbnf:190`); `-> decode_json_string_to_arena(input) : String` (`json.bbnf:10`); `int_lit … -> i64` in the grammar of record itself (`expressions.bbnf:6`).

Under `D`: "a bare path `-> f` means `f(input)`", so `-> f64` is a call to an undeclared function named `f64`; the prelude (D§4.3) lists four functions, none a conversion. `input : Color` has `input` of a product type checked against `Color`: by D§4.6 an annotation "is checked against the inferred type, never trusted", so it is a type error. The two host paths have no `@host` declaration (0 corpus uses, D§0.3), so there is nothing to instantiate. `String` and `Color` are not in τ; `f32` and `usize` are in `types.bbnf:3-7` and not in B. D§2 keeps the `Value(name)` diff withdrawn because it "types a mapping from its written `: Type`", which is exactly what these eight sites need.

JSON, CSS L4, Sheets and BBNF-self therefore either fail to type or need a rule `D` does not have. H2 gate (d) compares against root's column for those grammars.

*Smallest change.* Add to D§4.2: (i) a bare base-type name after `->` is a prelude conversion `fn(Span) → B`, `Span` is `@{ }`; (ii) `v : T` where `v` is an undeclared host path or `input` and `T` is not derivable is an **opaque host type** `Host(T)` introduced by the annotation, a declaration in the sense of D§4.6's fourth row, and say so in R-INF: these sites are annotated and stay annotated; (iii) ARCH §8.2's bounded-coercion row is where numeric and decode obligations live. This is an owner decision (O2 below), because it qualifies "full inference".

### Must-fix

**M1. SCC-monomorphic typing rejects grammars the macro semantics it replaces accepts, and the cure cannot be written. Attacks D§4.4, D§4.6 row 1.**

No corpus grammar needs polymorphic recursion: `git grep` for `name = |…|` over `grammar/**/*.bbnf` returns 0 definitions, confirming D§1.3. But two ordinary patterns do:
- Alternating list, finite (2 instances): `alt = |x, y| x , ("," , alt(y, x)) ? ; r = alt(/[0-9]+/, ("a" , "b")) ;`. The recursive call forces `α = β`; `r` is rejected `Span ≠ Span × Span`.
- A helper that mentions a concrete rule that calls it: `list = |x| "[" , (x | expr) * , "]" ; expr = list(atom) | atom ; atom = /[a-z]+/ ; nums = list(/[0-9]+/ , "!") ;`. `{list, expr}` is one component; inside it `α := Span`; `list` is no longer polymorphic; `nums` is rejected. `[C1C]` `functions::expand` accepts this (no function recursion).

D§4.6 says "a signature on that rule". No signature for either is writable: the locked `Type` has no quantifier (D says so) and **no `P τ`** (D does not say so), and the grammar of record has no parameter annotations at all (`bbnf.bbnf:51`).

*Smallest change.* F1's cure removes both (instances are ground). Independently: a reference to a parameterless rule on a cycle is `P R` with `R` known before its body is solved (rule NOM), so it is not a typing dependency; drop those edges when building binding groups. That alone cures `list`. Add both programs to the corpus with their expected verdicts.

**M2. NUM and DEFAULT make typeability depend on the component partition. Attacks D§4.3, D§4.4 step 3.**

`add = |a, b| a + b ; k = "a" -> add(1u8, 2u8) ;`: `add` is defaulted to `fn(i64, i64) → i64` when its component closes; `k` is rejected. `1u8 + 2u8` inline is accepted. If `add` and its caller were mutually recursive, ν would meet `u8` first and it would be accepted. Two valid typings (`u8`, `i64`) with no common generalisation; which one wins is decided by the dependency graph.

`empty = "[]" -> nil() ; bytes = empty -> |xs| cons(1u8, xs) ;`: `empty` is defaulted to `P [Unit]`; `bytes` is rejected with a `Unit` the author never wrote.

Also: an unsuffixed literal is `i64` outright (D§4.3), so `"px" -> 0 | "em" -> 1u8` is `Sum[i64, u8]` and `1 + 2u8` is an error; V04's "(NUM default)" label is wrong, nothing defaults there.

*Smallest change.* State both as accepted costs with corpus programs N14, N15 and a diagnostic that names the defaulting site; or give unsuffixed literals a numeric variable (then V04 really defaults) and generalise parameterless rules whose body type has free variables as generic rules instantiated per `(RuleId, TypeArgs)`, which is ARCH §8.2's row verbatim.

**M3. STAGE has a hole: mixed-stage functions make the generator evaluate value code with no budget. Attacks D§4.5, D§5.1, D§5.2.**

D§5.1 names two kinds of function, all-`P` and `P`-free. STAGE only forbids `P` under `P`. `count = |p, n| match n { 0 => ε, _ => p , count(p, n - 1) } ;` has type `fn(P α, i64) → P Count[α]` and passes STAGE. The generator must then decide `n` at compile time. The instance key is structural (D§5.2), so `count(p, 3 - 1)` and `count(p, 2)` are distinct unless arguments are evaluated; if they are evaluated, `r = count("a", spin(0))` with `spin = |n| spin(n)` hangs the generator without creating one instance, so the `rules`/`nodes` budget never fires. Layer (ii) is then Turing complete, against D§1.4's conclusion.

Second hole: `r = "a" -> |v| |n| n + 1 ;` is `P (fn(i64) → i64)`, STAGE-clean; a closure is a parse result. `has_type`, `obs()`, byte-determinism and TS have no reading for that.

*Smallest change.* STAGE clause 2: in a function type either every parameter and the result are under `P` or none is, diagnostic `BBNF-STAGE`. Clause 3: a rule's result type contains no `fn`. Corpus N16, N17.

**M4. `match` cannot destructure an alternation. Attacks D§4.1 (`Sum`), D§4.3 (`match`), D§5.3.**

`Sum[τ₁ … τₙ]` is positional and has no constructor names. `Pattern = Literal | Ident | "_" | Constructor | TuplePattern`, `Constructor = Ident (…)?` (`PASS-1:247-248`); PASS-1 checks exhaustiveness "against grammar-aware variant names" (`PASS-1:89`). `D` gives no rule that names the branches of a `Sum`, and none that introduces `None`/`Some` for `Option`. So the one thing a transform wants to match on, a parsed alternation, has no pattern. T-TC-2 ("a grammar that parses a two-counter program into a list of instructions", then `run` dispatches on the instruction) cannot be written.

*Smallest change.* Say where variant names come from (the rule name of a nominal branch, else an ordinal `_0 … _n`), make `None`/`Some` built-in constructors of `Option`, and add a corpus program that matches on a `Sum`.

**M5. The Turing-completeness proof is not walkable as written. Attacks D§5.3, D§6.3 T-TC-1.**

- *Parses nowhere.* A top-level closure body is `rhs`, a grammar expression (`bbnf.bbnf:51-52`); call arguments are `binary_factor`s (`:27`). `q_j(cons(true, a), b)` reads `true` as a rule reference; `inc = |n| n + 1` reads `n +` as a repetition. Named value functions are new surface, which D§7 puts in H4, yet H2's gate (a) lists V08 and V10.
- *`match` is locked, but narrowly.* It is in PASS-1 §6 `Primary` (`PASS-1:238-249`) and Lock 4's refinement clause presupposes arms. It is not in ARCH §8.1's production (`ARCH:1741`), and PASS-1 describes it as "the canonical variant-dispatch surface inside `@host fn` bodies" (`PASS-1:265`). Using it at rule level is permitted by the production; `D` should cite that sentence too. Verdict: implementing a lock, not new surface, provided C3 adopts PASS-1 §6 over ARCH §8.1.
- *The type walk.* Checks, with one error: the machine is **not** one component. `q_h` calls nothing; it is typed first as `∀αβ. fn(α, β) → α × β` and instantiated. The stated type holds only if both counters are touched by some state on the cycle. Harmless, but "the whole machine is one strongly connected component, typed monomorphically" is false.
- *T-TC-1 as specified does not exist, to my knowledge.* A two-counter machine started on `(3, 4)` cannot halt on `(0, 12)`: adding `b` to an accumulator `a` times needs `b` preserved, and there is no third register (two-counter universality is via the `2^a 3^b` encoding, not on raw inputs). Use three counters; the types generalise unchanged.
- *"Turing complete" under a budget.* `RuntimeBudget.work` defaults to 1,048,576 and `nesting` to 256 (`budget.rs:60-63`). With any fixed budget the layer computes a finite class. The claim is true only of the semantics with the budget as a free parameter; write it that way. Also every machine step is a call: with no tail-call guarantee in the emitted Rust, a run longer than the nesting limit (or the host stack) faults before `work` does. The proof needs "a component of value functions whose calls are all in tail position compiles to a loop", or it proves completeness for runs of at most 256 steps.
- *One counter for two things.* `charge_work` increments a single `limits.work` (`emit.rs:454-463`). Transforms charging it means an input accepted with identity transforms can fault with real ones. L-MATCH-INVARIANT then fails by construction, and if transforms run "beside" recognition (D§1.4) a looping transform in a branch that would have been backtracked faults a parse that should succeed.

*Smallest change.* Transforms run only on the committed result, charge their own counter, and L-MATCH-INVARIANT is stated modulo `Fault`. Fix T-TC-1 to three counters. Move V08, V10 to H4 or the surface to H2.

**M6. Lock 2 is not met by what H2 calls its "Lock 2 gate". Attacks D§3.1, D§4.10, D§7 H2 (e).**

Lock 2: "The IR module is `passes::layout` (… under the `passes` crate); the IR record is `Layout`; the trait that consumes it is `LayoutSink`" (`LOCKS.md:160`). v+1: "A wave may not claim Lock 2 closure by pointing only to `LayoutFacts` while the public `Layout` / `LayoutSink` names remain absent" (`:162-166`), repeated at `:587`. `D` puts the module at `crates/core/src/compiler/layout/`, creates no `LayoutSink`, and gate (e) reads "`LayoutFacts` is exported". The SK-V17 clause prices two routes for `StructLayout`, rename or side-table, and says "Route selection is an SK-V18 wave decision" (`:616`); `D` takes a third (root retires, the 962 sites die with it) without naming it as a route.

*Smallest change.* Rename gate (e) "Lock 2 partial: `TypeFacts` private, `LayoutFacts` exported"; add `Layout` and `LayoutSink` to H2 or H3 with a gate; record the crate-path deviation and the third route as owner item O3.

**M7. C5 (a), "a typed function table beside the existing variants", is a second expression IR. Attacks D§4.8, D§8 C5, D§3.2, D§3.4.**

The table's rows must encode lambda, application, `match`, tuple, literal, operator and primitive call, and "each backend lowers the table". That is a node alphabet with a per-backend lowerer: what Lock 5 means by "the IR is the contract" and what ARCH §7 excludes ("two IRs plus side tables … not a third core IR", `ARCH:870`, `:1070`). It also re-purposes `CallHost`, whose payload is "Host function ID … Emits registry dispatch … Calls host shim" (`ARCH:994`), to call code that is not a host function, and it owes the invariant "VM can replay all BIR variants" (`ARCH:1057`) an interpreter `D` does not size. The Governance Boundary bars using wording "as permission to add … a BIR variant" (`LOCKS.md:656-658`); asking the owner to rule that a table of expression nodes "is not a variant" is that.

It is not confined to H4: V01-V07 (`-> |s| len(s)`, `-> 1 + 2`) are in H2, and H3 lowers them "to `CallHost`", which has no slot for a closure or an operator. C5 blocks H3.

*Smallest change.* Present C5 as: (b) G-Omega amendment, sized honestly (expression nodes × Rust lowerer × VM), or (c) value-function bodies are not lowered through BIR in V1: only declared host functions and constants are callable from `->`, R-TC is deferred with H4. Drop (a) or label it as an amendment.

**M8. The instance key contradicts the locked one and its diagnostic's text. Attacks D§5.2, D§0 row 3.**

ARCH: "admitted to codegen only after finite `(RuleId, TypeArgs)` monomorphisation evidence exists" (`ARCH:1823`); `BBNF-GENERIC-CYCLE`: "unbounded `(RuleId, TypeArgs)` instance set" (`ARCH:1613`), message "for type arguments {args}" (`PASS-1:128`). `D` keys instances by argument **expressions**. In N10 every instance has `TypeArgs = (Span)`: the locked set has one element and the locked diagnostic's text is false. PASS-1 also says "recursive generic cycles require an explicit return annotation, or rejection … The two-path validation … is the V1 contract" (`PASS-1:93`); F06 and F09 are recursive, unannotated and accepted. `D` cites that line twice and does not say it overrides it.

D§0 row 3 answers "New? No" by Lock 10. Lock 10 gives lambdas, capture and `fn(T) -> U`. Parser-valued parameters, functions over functions over parsers (F07, F10), named value functions and recursion in them are not in Lock 10, not in the `Type` non-terminal, and outside ARCH §8.4's four closure forms. That row should read "partly new".

What the user sees for an argument-growing recursion: the occurs check catches only type-growing arguments. Type-preserving wrappers pass it: `grow = |x| x | grow(x << "a")`, `grow(x ?w)`, `grow(x - "a")`, `grow(wrap(x))`, `grow(x | x)`. With defaults `rules = 4,096`, `nodes = 65,536` (`budget.rs:27-28`) the generator builds a few hundred to 4,096 instances and then prints a chain of that length under a message about type arguments. The same code fires for a large legitimate grammar that crosses 4,096 rules after instantiation; "unbounded" and "large" are indistinguishable. Deterministic, yes; the budget is a `CompileOptions` field, so acceptance also varies with the caller's options.

*Smallest change.* A separate `instances` budget with its own diagnostic text; stop at the first instance whose argument strictly contains its requester's argument on the same parameter (depth 2, not 4,096), and keep the budget as backstop. Amend PASS-1:93/128 by name in C4.

**M9. H2 deletes a file three later stages still read, and adds rules to a program that is frozen before types run. Attacks D§7 H2, D§5.2.**

Stage order today: `Program::resolve` → `DependencySolution::build` → `TypeSolution::solve` → `verify` → `AnalysisFacts::derive` → `LoweringPlan::build(&program, &types, &facts, …)` (`mod.rs:106-135`). Instances are created inside layout, after the dependency solution exists and after `Program` is fixed; N11 and N12 (in H2's gate) need `NullableRepeat` and `RecursiveRule` to read instance rules; `lower.rs` reads `&types`. So H2 must also: extend or rebuild `Program` and `DependencySolution` after instantiation, re-point `lower.rs`/`replay.rs`/`CompiledModule` from `TypeSolution`, and rewrite `verify.rs`'s exhaustive matches (five new type constructors do not compile until read, `verify.rs:19-23`) and its 61 mutants. None is in the 2,650.

§4.2 has no rule for three node kinds the spine types today: `Record`, `Projection`'s `keep`, and `Pratt` (`typecheck.rs:1165-1192`). Pratt matters for Lock 10: today a Pratt node's type is its atom's; a left-recursive source rule `e = e , "+" , t | t` types under NOM as `E ≜ Sum[E × Span × T, T]` and is then refused by `RecursiveRule` (`analysis.rs:650-668`; `math-ambiguous.bbnf` ×2 at `SK/LEDGER.md:519`). When Lock 10's miner turns that shape into a Pratt node, the rule's type changes from the `Sum` to the atom's unless a rule says otherwise. A mined optimisation must not change a type.

On item 2 as asked: the always-on occurs check cannot refuse a recursive **rule**, because a cyclic rule's type is a name before its body is read; I found no legitimate grammar it refuses. For parametric rules an occurs failure implies a type-growing argument, hence an infinite instance set. The refusals of legitimate grammars come from monomorphic recursion (M1), not the occurs check. D§4.4 step 2 ("give each member a fresh monotype") contradicts NOM for parameterless rules (a fresh variable would fail the occurs check on G11); say "a name" there. `R[ᾱ]`'s arity is fixed only at generalisation but the name is used inside the body; state that recursive uses are at the identity instantiation.

*Smallest change.* Add the Pratt, Record and Projection rows; fix the Pratt rule's type as the type of the source alternation and make Pratt lowering produce it, or mine before typing with a stated rule; put the restaging in H2's content and size.

### Should-fix

**S1. Field access has no rule.** `value_input = "input" , ("." , value_ident)*` (`expressions.bbnf:13`); root lowers it to `InputProp`. D§4.1 puts records in layout and excludes rows. `input.name` on a type that is still a variable (`f = |x| x -> input.name`) is the textbook principality breaker. Refuse `input.f` unless `input`'s type is a ground record at that node, with a diagnostic; say so.

**S2. Levels are dead machinery.** Every binding is top-level (D§4.4), so the environment holds only closed schemes and the current component's monotypes. No variable can be "free in the environment" at generalisation. L-PRINCIPAL's mutant "generalize a variable that is free in the environment" is an equivalent mutant; it cannot be killed. Delete levels (generalise = close over what remains), or name the construct that needs them.

**S3. Schemes in `LayoutFacts`.** D§4.8 exports "the scheme of every exported value function". A scheme is a type term with bound variables: `TypeFacts` content in the public table, against Lock 2's "`TypeFacts` as a public artefact is a fault". Export instance signatures (ground) only.

**S4. `layout.rs` hard-codes what the locks give the CSP.** "Layout representation" is a finite CSP choice (`ARCH:1822`); D§4.8 step 4 is "a total, deterministic function" until LW5.COMP-4. Outputs then move twice. State that step 4 is the CSP's default assignment and that H5 must be byte-neutral on the corpus, with a gate.

**S5. D§0's "robust types" row.** ARCH §8.2 has nine rule rows. H5 schedules four. Bounded coercion, bounded lookbehind, typed directives and the schema-mining miner (`ARCH:1871-1877`, "without explicit annotations in most cases") are scheduled nowhere. The last one also means "full inference" is not wholly new: the delta is "most" to "all of F0".

**S6. C3 and C4 collide.** C3 keeps `{ rhs }` (repetition). C4 defines `Block` as `"{" Expr "}"`. `|x| { e }` is then ambiguous between a lambda whose body is a repetition and one whose body is a block. D§0.3 already notes `{ }` against `{n,m}`.

### Notes

**N1.** `LayoutFacts` in ARCH §7.3 is specified with `HashMap` fields and a `hot_call_graph` (Lock 15, `LOCKS.md:438`); D§4.8 changes the container type of a public artefact. Correct for determinism; it is an ARCH edit and should be listed.
**N2.** `nil`/`cons` allocate at parse time. Lock 9 wants a stated reason for arena allocation; Lock 1 wants `substrate_target` on what is retained. One sentence each.
**N3.** Two nominal rules with equal bodies never collapse (`a = b | c ; b = "x" , "y" ; c = "x" , "y" ;` is `Sum[B, C]`). Likely a G-DIGEST difference against root; decide before the digest, not after.
**N4.** New diagnostics `BBNF-STAGE`, `BBNF-PREDICATE-RECURSIVE` are additions to ARCH §7.5's catalogue.
**N5.** `a = a ;` is typed `A ≜ A` before the facts stage refuses it; the layout step must not be reached.

---

## 2. The de-duplication table, row by row (D§0)

| Row | "Already locked" claim | Lock text | Verdict |
|---|---|---|---|
| HM | Lock 2, Lock 4, ARCH §8.2 | Lock 2 quote exact. Lock 4 names the domain "CSP type/layout inference" and "each lives in its own crate"; ARCH §8.2 supplies HM. | Holds, with two unlisted deviations: solver crate/path (M6), and Lock 4's wording that the type domain is the CSP's. |
| full inference | "Not required by any lock" | ARCH `:1846-1848`, `:1871-1877` record a mandate for annotation-free schemas "in most cases". | Overstated as wholly new (S5). |
| functional | Lock 10 | Lambdas, `&'i` capture, `fn` types only. | Partly false (M8). |
| Turing complete | new; contradicts ARCH §8.4 | Correct. | Holds. |
| robust types | ARCH §8.2 rules | Nine rows; five covered. | Incomplete (S5). |
| one facility | Lock 5, Lock 2 | Quotes exact. Lock 5 also defers TS and WASM post-V1; H3 gives TS work. | Holds; "no second typed core" is contradicted by C5 (a) (M7). |

New surface the design needs, collected: named value functions; `match` at rule level (locked in PASS-1 only); `P τ` and a quantifier in `Type` (for any signature); optional `Param` types and undeclared generics (C4); `Block`; variant names for `Sum`; prelude entries; two diagnostics; an `instances` budget; a function table or four variants (C5); `LayoutFacts` fields; `Layout`/`LayoutSink` owed. No new directive and no new `BackendShape`: confirmed.

---

## 3. The five owner choices

| | Real choice? | Finding |
|---|---|---|
| C1 | No. It is a theorem plus a schedule. | The owner cannot choose to infer higher-rank types. The real question hidden in it: do DK13, GADT refinements **and bounded coercion** stay in V1 now that the core is HM-only until LW5? Ask that. "L-INF byte-equal with the extension compiled in and out" assumes the extensions are conservative; coercion (numeric widening) is not, against NUM. |
| C2 | Yes. | Missing sub-questions: separate transform budget; transforms after recognition only; tail calls; mixed-stage functions (M3, M5). |
| C3 | Three choices sold as one. | The directive canon (`@ws`, `@recover`, `@debug`, bodyless `@host`) has nothing to do with type solving and reverses COMP1C-1 two days after it was ruled. The design needs only the third part (function surface). Split C3a/b/c; the recommendation to bundle is convenient for the design, not required by it. |
| C4 | Yes, G-Omega. | Collides with C3 (S6). Must also name PASS-1:93 and :128 as amended (M8). |
| C5 | Yes. | Recommendation (a) is self-serving: it avoids G-Omega by definition (M7). Option (c), defer bodies, is missing. |

Missing decisions the owner must make:
- **O1.** ALT, NOM and tagging change the public value type of user grammars (collapsed vs `Sum`; TS documents alternation as **untagged**, `typescript/VALUE-SEMANTICS.md:17-31`). D§8 lists these as "decided here". Each G-DIGEST row and each LG-b divergence it causes is an owner ruling.
- **O2.** What `-> f64`, `: T` on `input`, and undeclared host paths mean (F3). It decides whether "full inference" has an exception class.
- **O3.** The Lock 2 route: root retirement as the cure for 962 `StructLayout` sites, the crate path, and whether `Layout`/`LayoutSink` land or Pass Omega narrows the lock (M6).
- **O4.** Whether instance-site typing (F1's cure) is acceptable under the words "Hindley-Milner": the value layer stays HM; parametric rules become templates checked at instantiation.
- **O5.** Whether accepted recursive generics without annotation (F06, F09) override PASS-1's two-path contract.

---

## 4. Size and staging

- **H1, 900 lines.** It holds an Algorithm W with three extra rules, a small-step evaluator, a type-directed program generator, an exhaustive typing enumerator, `has_type`, and a runner. It also needs a reader for programs no front end parses (every ‡ program, V08, T-TC): its own parser or hand-built ASTs. 1,800-2,500 is nearer. The enumerator cannot be written until a declarative system exists (F1c).
- **H2, 2,650 lines.** `typecheck.rs` is 1,634 lines for first-order synthesis with certificates over 13 node kinds. The replacement adds unification, schemes, ALT, instances, layout and export in 3,050 including H4's share. Not counted: restaging (M9), `verify.rs` and its mutants, `lower.rs`/`replay.rs` re-pointing, resolve-stage node kinds for call, closure, mapping and span capture (LEDGER E7), dependency edges over calls and value functions, diagnostics. 4,500-6,000 plus consumer edits.
- **The 962 `StructLayout` hits and the seven re-derivation sites.** H1 and H2 touch none: 725 hits are generated output and 237 are root and its consumers (`git grep -c`, summed outside `generated` paths). The retired-term count does not move until LW4.COMP-1. H3's "+400 inside COMP-1's budget" then has to cover: the template that stops emitting `StructLayout`, the runtime that reads `layout.rule_id` (`LOCKS.md:614`), `bbnf-path` and `bbnf-path-ts` (they walk `StructRegistry` and `TypeDesc`), LSP hover, the xtask TOML projection and registry sidecar. Not credible at +400.
- **The nine generated grammars.** At H2 nothing moves: the spine does not generate them and `regen --check` and the MANIFEST digest are root-driven. That is also why H2 proves nothing about them. The spine accepts 31 of 70 MANIFEST entries today (`SK/LEDGER.md:519`), mostly those without `->`; gate (d) is read on the easy subset unless COMP-1c lands every form first. For the rest, gate (d) compares a 350-line `layout.rs` against the output of root's Span compression, tuple flattening, `effective_payload_type` and `decide_seq` fallbacks, the same non-lattice code whose lawful version changed 21 of 70 entries. Either `layout.rs` re-implements those heuristics bit for bit (then it is not 350 lines and not "total and deterministic from a ground type"), or there are tens of DIVERGENCE rows, each an owner ruling. At H3 every generated file changes; `regen --check` 9/9 must be re-baselined in one ruled step and Lock 6 v+1's byte-equivalence re-proved from the new generator. `D` does not say this.
- **TS.** No grammar functions (closures exist only as kept text inside value expressions, `typescript/src/grammar.ts:335`), no result types, `->` never evaluated, alternation untagged. `D` files two DIVERGENCE rows. Every F program, every V program's `obs()`, and every uncollapsed `Sum` diverge. Under LG-b's 0-divergence bar that is a class ruling, not two rows; and Lock 5 defers the TS backend post-V1, so the owner should say which governs.

---

## 5. The test plan

L-REF compares two implementations of one prose spec written by one seat; where the prose is ambiguous (ALT against DEFAULT order, F2) both will agree and both may be wrong. L-PRINCIPAL is undefined without a declarative system and false on F04 with one. L-ORDER permutes programs in which order cannot matter. Wrong implementations, and what kills them:

| # | Wrong implementation | Killed by |
|---|---|---|
| W1 | ALT resolved eagerly, in traversal order (L-ORDER's own named mutant) | **Nothing in the 64.** No corpus alternation becomes homogeneous through a later equality. Needs e.g. `w = |f, x, y| (x | y) , f(x) , f(y) ;` (expected collapse to `α`). |
| W2 | Binding groups in source order; a forward reference is monomorphic | **Nothing.** In F01, F04, F06, F08, F10, V09, V15 the polymorphic definition precedes its uses. Needs V15 with `id` last. |
| W3 | Instances keyed by `(rule, type arguments)`, the key ARCH actually names | **Nothing.** No program calls one rule at two same-typed arguments, and L-PARSE-TYPED runs on grammars with 0 functions. `r = many("a") , many("b")` would parse `"a"*` twice. Needs that program with an accept/reject assertion. |
| W4 | Members of a component generalised one at a time (accepts mutual polymorphic recursion) | **Nothing.** N09 is self-recursion only. |
| W5 | ALT equality up to nominal unfolding (`B ≡ C` when bodies are equal) | **Nothing** (N3's program is absent). |
| W6 | DEFAULT applied before ALT step 2 | **Nothing**; X01 has one branch. And the spec does not say which is right (F2). |
| W7 | Occurs check missing on the binding made in ALT step 3 | **Nothing**; N01, N02, N09 reach only ordinary unification. F1b's `k` reaches it. |
| W8 | `a - b` typed as `τ_b` | **Nothing**; G09 is `Span - Span`. |
| W9 | Two sum tags swapped at layout (L-PARSE-TYPED's own named mutant) | Only when the two payload types differ. On F04's expected `Sum[Span, Span]` `has_type` cannot see it. |
| W10 | "Generalize a variable free in the environment" (L-PRINCIPAL's named mutant) | Unkillable: equivalent mutant (S2). |
| W11 | ALT collapses when branches **unify** | F04. Killed. |
| W12 | Instantiate without fresh variables | V15. Killed. |
| W13 | Value recursion limited to bounded iteration | **Nothing**; T-TC-1 and T-TC-2 are terminating runs of fixed size. L-TC's only mutant is "forbid recursion". |

Ten of thirteen survive the named corpus; four of them are mutants the plan itself names as killed. Soundness (L-PRESERVE, L-PROGRESS) is in better shape than principality, but covers only the value layer; nothing tests that an **instance** recognises what its arguments say (W3).

---

## 6. Verdict

**Adopt with named changes; do not start H1 or H2 on the text as it stands.** The lock placement (private solver, public `LayoutFacts`, piped CSP), nominal rule types, component-wise generalisation for the value layer, the three-layer termination split and the certificate idea survive this reading. The following do not, and are conditions:

1. **F1, F2, M1:** parametric rules are typed at their instances (or ALT's loss of principality and completeness is written into R-INF with the counterexamples as corpus programs, and `AltResolved` is restated). Owner item O4.
2. **F3:** rules for bare type names, `: T` on host results and on `input`, as a declared exception to R-INF. Owner item O2.
3. **M3, M4, M5:** STAGE clauses 2 and 3; variant names for `Sum`; transforms after recognition on their own counter; T-TC-1 on three counters; the completeness claim worded with the budget as a parameter.
4. **M6, M7, M8:** the Lock 2 gate renamed and `Layout`/`LayoutSink` scheduled; C5 re-posed as amendment or deferral; the instance key, its budget and PASS-1:93/128 reconciled.
5. **M9 and §4:** Pratt, Record and Projection rules; restaging in H2; H1 and H2 re-sized; the H3 re-baseline of 9/9 stated.
6. **§3:** C1 re-posed, C3 split, C4 de-collided, O1-O5 added.
7. **§5:** the ten surviving mutants each get a corpus program before H1's gate is called born-RED.

If condition 1 is refused in both forms, the verdict is **redesign** of §4.2 and §4.6: the principal-type claim is then false and the gate that tests it cannot be built.
