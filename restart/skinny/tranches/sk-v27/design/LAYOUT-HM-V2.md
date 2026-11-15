# LAYOUT-HM-V2: addendum to LAYOUT-HM after the adversarial review

Design seat, 2026-11-15. Read-only on code; nothing compiled or run. An addendum, not a patch: `LAYOUT-HM.md` (cited `D§n`, at `f52cf1f09`) and `LAYOUT-HM-REVIEW.md` (cited `R`, at `58411f8f0`) stand unedited. Where this file and `D` disagree, this file is the design; everything in `D` not named here is unchanged.

Paths are relative to the repository root at `58411f8f0`. `[C1C]` is lane `lw3/comp-1c` at `a2dd184ef`. `ARCH` is `restart/ARCHITECTURE.md`; `PASS-1` is `restart/audit/pass-1-substrate/PASS-1.md`; `LOCKS` is `restart/locks/LOCKS.md`. Every code claim I rely on was re-read for this file and carries a file:line; claims I took from `D` or `R` without re-opening the source are listed in §12.

Binding: Locks 2, 4, 5, 10, 13, 14 and the Governance Boundary (`LOCKS:160`, `:179`, `:181`, `:269`, `:336`, `:349`, `:652-661`). This file proposes no directive, BIR variant, lock or `BackendShape` as decided. Where the design would need one, it is an owner choice in §8 and the design says what it does without it.

---

## 0. What changed, in one table

| Review item | Outcome |
|---|---|
| F1 | **Cured by the first form: parametric rules are typed at their instances.** Their definitions get a principal *sort* (how many parsers and templates they take), never a result type. §1, §2. |
| F2 | Cured as a consequence: every alternation is typed on ground branch types, so `AltResolved` holds as `D` wrote it. §3. |
| F3 | Cured: seven mapping forms cover all 585 `->` sites in `grammar/`; each has a rule and a file:line. §4. |
| M1 | Design change (follows from F1). |
| M2 | Design change for arithmetic (numeric variables generalise); **refused in part**: parameterless rules are not made generic. §5. |
| M3 | Design change: two STAGE clauses added. |
| M4 | Design change, plus owner choice O6. |
| M5 | Redone. **The proof does not walk in the grammar of record as it stands; R-TC is an owner choice (C3c).** A corrected walk is given for the locked surface. §6. |
| M6 | Design change; owner choice O3. |
| M7 | Accepted: C5 option (a) withdrawn; C5 re-posed. |
| M8 | Design change; **refused in part**: the review's structural-containment early stop refuses a finite program; replaced. §5. |
| M9 | Design change: instances are made before resolve, so nothing is added to a frozen program; rows added for Record, Projection, Pratt. |

---

## 1. F1: the choice, and the declarative system

### 1.1 The choice

`R` names two cures: type parametric rules at their instances, or keep definition-site schemes and write the loss of principality into the inference rule. **I choose the first.** Reasons:

1. *It makes the law true instead of weakening it.* With definition-site schemes, rule ALT is not closed under substitution (`R` F1a) and is incomplete (`R` F1b); the second cure keeps both defects and documents them. With instance-site typing every alternation has ground branch types, so "identical types collapse" needs no deferral, no ordering argument and no asterisk.
2. *The generator already does it.* `D§5.2` monomorphises by memoised instance. The types of a parametric rule were only ever consumed at instances; a definition-site result scheme was a second answer to a question the instance answers exactly.
3. *It removes machinery.* No deferred `Alt` constraints, no parametric nominal types `R[ᾱ]`, no levels, no re-staging of a frozen program (M9).
4. *What a definition can still be told early is kept.* A parametric rule's **sort** (§1.2) is inferred at its definition by textbook Hindley-Milner over one base constant, where the principal-type theorem holds with no extra rule. Arity errors, "not a function", "a function used as a parser" and self-application are still definition-site errors.

Costs, stated plainly:

- A type error inside a parametric rule's body (for example `len(s)` on a non-`Span`) is reported at the first instance that exhibits it, with the definition span and the instance chain. A parametric rule that is never called is sort-checked only.
- `ARCH:1823` says "Rule schemes are generalized at definition". Under this design what is generalised at definition is the sort scheme; result types exist per instance. Whether that still answers to the words "Hindley-Milner" is owner choice **O4**.
- Hindley-Milner proper (unification, schemes, generalisation over types) then lives in the value layer. On the grammar of record that layer has inline closures and prelude calls but no named value function (`grammar/bbnf/expressions.bbnf:33-34`; a top-level `|p| body` has a grammar body, `grammar/bbnf/bbnf.bbnf:51-52`), so **until named value functions exist (C3c) generalisation has no user program to act on**: only unification and instantiation of prelude schemes are exercised. This is true of `D` as well; `D` did not say it.

### 1.2 Syntax of types and sorts

```
B ::= Unit | Span | Bool | Str | u8 | u16 | u32 | u64 | usize
    | i8 | i16 | i32 | i64 | f32 | f64                      base types
τ ::= α | B | Host(T) | τ₁ × … × τₙ (n ≥ 2) | Sum[τ₁ … τₙ] (n ≥ 2)
    | Option τ | List τ | R | fn(τ̄) → τ                    value types
κ ::= any | eq | num          kinds of a type variable; num ⊂ eq ⊂ any
σ ::= ∀ᾱ:κ̄. τ                value scheme, prenex
s ::= par | (s̄) ⇒ s | ς      sorts; par is "a parser"
```

- `f32` and `usize` join `B` because the grammar of record names them (`grammar/bbnf/types.bbnf:3-7`). `i8`, `i16` are reached through that file's identifier branch.
- `Host(T)` is an opaque host type introduced by an annotation (§4). It has no structure and unifies only with itself.
- `R` is the nominal type of a concrete rule or of an instance. There is no `R[ᾱ]`: instances are ground.
- `num` holds the eleven numeric members of `B` (all but `Unit`, `Span`, `Bool`, `Str`); `eq` holds all fifteen. A variable of kind κ unifies only with a type in κ or a variable (the result takes the smaller kind). The chain has three points, so unification stays unitary.
- `P τ` is no longer a type constructor inside τ. A grammar expression has the judgment form `e : P τ`; a parser cannot occur inside a value type by construction of the syntax. That is `D§4.5`'s STAGE clause 1 made unwritable instead of checked.

### 1.3 The four judgments

**(S) Sorts of parametric rules: `Σ ⊢ e : s`.** Hindley-Milner over the single base sort `par`.

| Form | Rule |
|---|---|
| literal, regex, `ε`, reference to a parameterless rule | `par` |
| parameter `x` | `Σ(x)` |
| reference to a parametric rule `f` | a fresh instance of `f`'s sort scheme |
| `e₁ , … , eₙ`, `e₁ \| … \| eₙ`, `e?`, `e*`, `e+`, `[e]`, `{e}`, `e ?w`, `a << b`, `a >> b`, `a - b`, `@{ e }`, `e -> v` | every operand `par`; result `par` |
| `f(a₁ … aₙ)` | `f : (s₁ … sₙ) ⇒ s`, `aᵢ : sᵢ`; result `s` |
| `\|x₁ … xₙ\| body` | `(ς₁ … ςₙ) ⇒ s` with `body : s` under `xᵢ : ςᵢ` |

Top-level parametric rules are typed per strongly connected component of the call graph, monomorphic inside the component, generalised over their free sort variables after it (the Damas-Milner `letrec` rule). The occurs check is on. A parameterless rule must have sort `par`.

**(I) Instantiation.** Given a sort-correct program, the *closed program* is the least set of concrete rules containing every parameterless rule and closed under: a call `f(ā)` in a concrete body, with every argument closed, denotes the instance `f⟨ā⟩`, whose body is `f`'s body with each parameter replaced by a reference to its argument. An argument that is not a bare reference is interned once, by structure, as an anonymous concrete rule. Two calls with structurally equal argument lists denote the same instance. The instance key is `(rule, argument rules)`, which the instance table records beside the derived `(RuleId, TypeArgs)` of `ARCH:1823`. Instantiation is syntactic: it never evaluates a value expression and never reads a type.

**(G) Grammar typing of the closed program: `Δ ⊢ e : P τ`**, where `Δ` gives each concrete rule its reference type. τ is always ground.

| Form | Type |
|---|---|
| literal, regex | `P Span` |
| `ε` | `P Unit` |
| `e₁ , … , eₙ` (n ≥ 2) | `P (τ₁ × … × τₙ)` |
| `e₁ \| … \| eₙ` | **ALT:** `P τ₁` if `τ₁ … τₙ` are the identical term; otherwise `P Sum[τ₁ … τₙ]`. Names are compared by name; no unfolding. |
| `e?`, `[e]` | `P (Option τ)` |
| `e*`, `e+`, `{e}` | `P (List τ)` |
| `e ?w` (the spine's `Trim`) | `P τ` |
| `a << b` (`Projection`, keep left) | `P τₐ` |
| `a >> b` (`Projection`, keep right) | `P τ_b` |
| `a - b` (`Difference`) | `P τₐ` |
| `@{ e }` | `P Span` |
| reference to concrete rule or instance `r` | `P Δ(r)` |
| `e -> v` | rule MAP, §4 |
| record (the spine's `Record { owner, slots }`, `crates/core/src/compiler/typecheck.rs:1172-1175`, `:1277-1300`) | the nominal type of `owner`, defined as the record of its `Field` slots' types; `Discard` slots are typed and dropped. Unchanged from the spine. |
| Pratt node | **PRATT:** the type of the source alternation it replaces. See §5 M9. |

**NOM (what `Δ(r)` is).** A named parameterless rule on a reference cycle is nominal: `Δ(r) = R`, with `R ≜` its body type. A named parameterless rule off every cycle is nominal unless its body type is a base type, in which case it is transparent (`Δ(r)` is the body type). An instance is transparent unless it lies on a cycle, in which case it is nominal and named `f⟨k⟩`, k its ordinal among `f`'s instances in creation order. An anonymous argument rule is always transparent. This is well founded: every reference cycle passes through a named rule or an instance, never through anonymous rules alone, because an anonymous rule is referenced only from an instance body. Cycle membership is read off the dependency condensation the spine already computes and verifies (`DependencyFirstOrder`, `ComponentsStronglyConnected`, `crates/core/src/compiler/verify.rs:74-117`).

**(V) Value typing: `Γ ⊢ v : τ`.** Damas-Milner, syntax-directed, with kinded variables.

| Form | Rule |
|---|---|
| variable, prelude or host name `x` | an instance of `Γ(x)`, kinds respected |
| integer literal with suffix `u8` … | that type. Without a suffix: a fresh variable of kind `num`. A suffix naming no numeric base type is refused. |
| float literal | `f64` unless suffixed |
| `true`, `false`; string literal | `Bool`; `Str` |
| `f(v₁ … vₙ)` | `f : fn(τ₁ … τₙ) → ρ`, `vᵢ : τᵢ`; result ρ |
| `\|x₁ … xₙ\| b` | `fn(α₁ … αₙ) → ρ`, αᵢ fresh monotypes |
| `+ - * / %`, unary `-` | `∀ν:num. fn(ν, ν) → ν` (unary: `fn(ν) → ν`) |
| `< <= > >=` | `∀ν:num. fn(ν, ν) → Bool` |
| `== !=` | `∀α:eq. fn(α, α) → Bool` |
| `&& \|\| !` | over `Bool` |
| `v : T`, T a base-type name | `v : T` is checked (unified) and the result is `T` |
| `input.f` | `input`'s type is ground at every mapping (it comes from judgment G); it must be a record type with a field `f`, else refused. `R` S1 is answered by construction. |
| tuple, `match` (locked surface, C3c) | tuple: the product. `match`: scrutinee τ; each pattern checked against τ; arms share one type; `None`, `Some(p)` are the built-in constructors of `Option`; sum patterns per §5 M4. |

Named value functions (C3c) are typed per strongly connected component of the value call graph, monomorphic inside, generalised after over every remaining variable **with its kind kept**: `add = |a, b| a + b` is `∀ν:num. fn(ν, ν) → ν`. Because every binding is top-level, the environment holds only closed schemes and the current component's monotypes, so "generalise" is "close over what remains". There are no levels (`R` S2 accepted).

**MAP and CLOSE (the only step that is not most general).**

```
Δ ⊢ e : P τ      ρ = the principal type of ⌈v⌉ under Γ, input : τ
────────────────────────────────────────────────────────────────── MAP
Δ ⊢ e -> v : P close(ρ)
```

`⌈v⌉` is the desugaring of §4. `close` replaces each variable still free in ρ: kind `num` by `i64`, any other by `Unit`. The result must contain no `fn` (STAGE clause 3, §5 M3). CLOSE is applied at the mapping node, bottom-up, so every grammar node has a ground type when its parent is read; there is no ordering between CLOSE and ALT to choose (`R` F2, W6).

### 1.4 The principal-type law, as a statement

**PT-1 (sorts).** For every component of parametric rules, either no sort assignment satisfies (S), or there is a sort scheme of which every satisfying assignment is an instance, and inference returns it. This is the Damas-Milner theorem for a calculus with one base constant; nothing non-textbook is involved.

**PT-2 (values).** For every Γ and value expression v: if `Γ ⊢ v : τ` for some τ, there is τ₀ with `Γ ⊢ v : τ₀` such that every such τ equals `S τ₀` for a kind-respecting substitution S; inference returns τ₀ up to renaming. Likewise each component of named value functions has a principal scheme. This is Damas-Milner with order-sorted unification over a three-point chain (the result for HM with sorted variables is Nipkow and Prehofer's; cited from memory, §12). It is a theorem I rely on, and L-REF plus L-PRINCIPAL (§9) are its test.

**PT-3 (grammar).** If `Δ ⊢ e : P τ` and `Δ ⊢ e : P τ′` then `τ = τ′`. Every rule of (G) is syntax-directed with one conclusion, MAP's premise is unique by PT-2, and `close` is a function. A unique type is trivially principal.

**PT-4 (templates are transparent).** For an instance `f⟨ā⟩` that is on no cycle, the type of a call `f(ā)` equals the type of `f`'s body with the arguments written in place of the parameters. Immediate from (I) and NOM. This is the property `R` F1a showed `D` lacked: naming an expression does not change its type.

**What is not most general, and is said so.** CLOSE fixes a mapping's leftover variables where the mapping is written. A later use that wanted another type is refused (§5 M2, programs N14, N15). The alternative (generic parameterless rules) is refused in §5 M2 with the reason.

### 1.5 The review's three counterexamples, by hand

**`alt2 = |x, y| x | y ; r = alt2("a", "b") ;`**
(S): `x : ς₁`, `y : ς₂`; alternation forces both `par`; `alt2 : (par, par) ⇒ par`. (I): one instance `alt2⟨"a","b"⟩` with body `"a" | "b"`. (G): branches `Span`, `Span`, identical, so ALT gives `P Span`. The instance is on no cycle, so transparent; `r`'s body is `P Span`, a base type, so `r` is transparent: **`r : P Span`**, the same as `r = "a" | "b"` (`D` G04). `D` gave `Sum[Span, Span]`.

**`pick = |p, q| (p | q) -> |s| len(s) ; r = pick("a", "b") ;`**
(S): `pick : (par, par) ⇒ par`. (I): `pick⟨"a","b"⟩`, body `("a" | "b") -> |s| len(s)`. (G): the alternation is `P Span`. MAP: `input : Span`; the closure is applied to `input`, so `s : Span`; `len : fn(Span) → i64`; ρ = `i64`, nothing to close. **`r : P i64`. Accepted.** `D` rejected it. A second instance `pick("a", ("b" , "c"))` has alternation type `Sum[Span, Span × Span]`, and `len(s)` is refused there with `Sum[Span, Span × Span] ≠ Span`, citing the span of `len(s)` and the chain `pick⟨"a", ("b","c")⟩`. That refusal is correct: the same program written inline fails the same way.

**`k = |f, x| f(x | f(x)) ;`**
(S): `x : ςₓ`; `f(x)` gives `f : (ςₓ) ⇒ ς₂`; the alternation `x | f(x)` forces `ςₓ = par` and `ς₂ = par`; the outer call checks `f : (par) ⇒ par` again. **`k : ((par) ⇒ par, par) ⇒ par`.** No occurs failure. With `opt = |y| y? ; r = k(opt, "a") ;`: (I) creates `opt⟨"a"⟩`, the anonymous rule `A₁ = "a" | opt⟨"a"⟩`, `opt⟨A₁⟩`, and `k⟨opt,"a"⟩`. (G): `opt⟨"a"⟩ : P (Option Span)`; `A₁ : P Sum[Span, Option Span]`; `opt⟨A₁⟩ : P (Option Sum[Span, Option Span])`. **`r : P R`, `R ≜ Option Sum[Span, Option Span]`. Accepted**, and the type is one a user can read. `D` failed with `α := Sum[α, β]`.

---

## 2. Layer (ii) under instance typing

- **Where it runs.** Sort inference and instantiation run on the source module, before `Program::resolve`. The stage order of record is `Program::resolve` → `DependencySolution::build` → `TypeSolution::solve` → `verify` → `AnalysisFacts::derive` → `LoweringPlan::build` (`crates/core/src/compiler/mod.rs:106-135`). With instances made first, every later stage sees a closed program of concrete rules: the dependency solution, `NullableRepeat` (`crates/core/src/compiler/analysis.rs:548-567`) and `RecursiveRule` (`analysis.rs:625-670`) read instances with no change, and nothing is added to a program after it is frozen. That is `R` M9's first half, removed rather than staged.
- **Relation to `[C1C]`.** `functions::expand` is already a source-to-source pass before resolve (`[C1C] crates/core/src/compiler/functions.rs:1-13`, `:33-86`). Its position is right. Its meaning differs in two ways: it copies the argument at each use and deletes the function rules, where this design makes one shared instance rule per distinct argument list; and it refuses recursion, where this design memoises. `D§7` H0 hold (1) is amended accordingly in §10.
- **Termination.** The instance set must be finite. It is bounded by a dedicated `instances` count in `CompileOptions` (a new field beside `rules = 4,096` and `nodes = 65,536`, `crates/core/src/compiler/budget.rs:24-28`), so that "too many instances" and "a large grammar" are different refusals (`R` M8). The text of the refusal is owner choice O5; without a ruling it is the existing `ResourceLimit` diagnostic (`crates/core/src/compiler/diagnostic.rs`, code list) naming the instance chain.
- **Early stop.** See §5 M8.
- **Layer (ii) stays total and is not Turing complete**, now by construction: instantiation evaluates no value code, and STAGE clause 2 (§5 M3) refuses any function that mixes parser and value parameters.

---

## 3. F2: `AltResolved` against the design's own outputs

The law as `D§4.9` wrote it: a collapsed alternation has identical branch types; an uncollapsed one does not. Under §1.3 it is checkable by re-reading the children's ground types, with no unification and no search. The three outputs `R` listed:

| Program | Branch types | Result | Law |
|---|---|---|---|
| `r = alt2("a", "b")` | `Span`, `Span` | collapsed, `P Span` | holds |
| `e = ("a" -> nil()) \| ("b" -> nil()) ;` | each MAP: ρ = `List α`, closed to `List Unit` at its own node | collapsed, `P (List Unit)`; one branch alone (`D` X01) is also `List Unit` | holds |
| `r = ("a" -> \|v\| loop(0)) \| ("b" -> \|v\| loop(0)) ;` with `loop : ∀αβ. fn(α) → β` | each MAP: ρ = β, closed to `Unit` | collapsed, `P Unit`, as `D` X02 expects for one branch | holds |

The verifier laws of `D§4.9` are replaced by this list. Each is a re-reading of recorded rows; each gets at least three mutants.

| Law | Statement |
|---|---|
| `AltResolved` | as above, on ground types |
| `GroundAtNode` | every grammar node's type has no variable and no `fn` |
| `MappingClosed` | a mapping node records ρ, and the closing substitution maps exactly the variables free in ρ, each by its kind |
| `ValueDerivation` | each value node's recorded type is what its rule yields from its children's recorded types and its recorded scheme instantiation (`InstanceOfScheme` folded in). This checks that the derivation is valid, **not** that ρ is principal; principality is L-REF's and L-PRINCIPAL's job. |
| `SchemeClosed` | a named value function's scheme has no unbound variable (C3c only) |
| `NominalOnCycle`, `TransparentOffCycle` | NOM, both directions |
| `InstanceIsSubstitution` | an instance body equals its template body with parameters replaced by its recorded arguments |
| `InstanceTableClosed` | every call in every concrete body names a table row |
| `VariantIsBranch` | variant i of a `Sum` is the alternation's i-th child (kills a tag swap that `has_type` cannot see) |
| `HostSignatureAgrees` | every site of one host path has the same ground signature |

---

## 4. F3: every `->` form the grammars use

Census, re-run for this file over all 33 `.bbnf` files under `grammar/` (a script that extracts the right side of every `->` outside comment lines): **585 sites, seven forms, nothing unclassified.** The 586th textual hit is the production itself, `grammar/bbnf/bbnf.bbnf:42`.

| # | Form | Sites | Where (all, or first of each file) | Rule |
|---|---|---|---|---|
| 1 | suffixed integer or hex literal | 560 (410 `u8`, 150 hex `u32`) | `grammar/json/json.bbnf:1`; `grammar/css/l4/color.bbnf:36-…`, `:197-…`; `grammar/css/l4/keywords.bbnf` | value rule for literals: the suffix's type |
| 2 | `true`, `false` | 4 | `grammar/json/json.bbnf:2` (×2); `grammar/google-sheets/google-sheets.bbnf:17`, `:18` | `Bool` |
| 3 | bare `Span` | 6 | `grammar/bbnf/bbnf.bbnf:9`, `:13`, `:15`, `:17`, `:18`; `grammar/bbnf/expressions.bbnf:9` | **SPAN:** `e -> Span` is `@{ e }` for any `e : P τ`; result `P Span`. Four of the six inputs are products or sums of products (`bbnf.bbnf:11-13`, `:15`, `:17`, `:18`), so this cannot be a conversion from `Span`. |
| 4 | bare numeric type name | 6: `f64` ×4, `i64` ×2 | `f64`: `grammar/json/json.bbnf:4`, `grammar/css/l4/value-unit.bbnf:15`, `grammar/google-sheets/google-sheets.bbnf:6`, `grammar/bbnf/expressions.bbnf:7`. `i64`: `value-unit.bbnf:16`, `expressions.bbnf:6` | **CONV:** a mapping whose whole right side is the name of a numeric base type `B` requires `input : Span` and has type `P B`. It denotes the prelude conversion `fn(Span) → B`. All six inputs are a single regex. What the conversion does on text that is not a `B` is part of O2. |
| 5 | `input : Span` | 4 | `grammar/google-sheets/google-sheets.bbnf:12`, `:52`, `:63`, `:90` | `input` then the checked ascription of §1.3: the input is a regex, `Span = Span`. No special rule. |
| 6 | `input : T`, T not a base type | 3, all `Color` | `grammar/css/l4/color.bbnf:228` (input is `@{ … }`, `Span`), `:255`, `:303` (inputs are products) | **HOST-ASCRIBE:** result `P Host(T)`. The site declares a host builder from the input's ground type to `T`, supplied by the grammar's declaration crate (Lock 14 surface (c)). The grammar itself says this is a declaration: "The rule-level `-> input : Color` annotation declares the rule's output type" (`color.bbnf:217-219`). |
| 7 | call of an undeclared path with `: T` | 2 | `grammar/css/l4/color.bbnf:190` (`crate::css_types::parse_hex_color(input) : u32`); `grammar/json/json.bbnf:10` (`decode_json_string_to_arena(input) : String`) | **HOST-CALL:** a path bound by nothing in the grammar or prelude, called with an annotation, is a host function declared at that site: `f : fn(τ̄) → T′`, τ̄ the arguments' ground types, `T′ = T` if T names a base type, else `Host(T)`. Every site of `f` must agree (`HostSignatureAgrees`). Without an annotation it is refused. |

Desugaring `⌈v⌉` at a mapping, decided by syntax and name resolution in this order: a closure literal `|x| b` is applied to `input` (with n ≥ 2 parameters the input must be an n-product and is bound componentwise; this differs from root's reading, which I did not re-open, and has zero corpus weight); a bare name that is a parameter or `input` is itself; a bare base-type name is SPAN or CONV; a bare name bound to a prelude, host or named value function `f` is `f(input)`; any other bare name needs form 6 or 7; anything else is an expression over `input`.

**The exception class this adds to R-INF, E-HOST.** Forms 6 and 7 are annotated and stay annotated: the annotation is not checked against an inferred type, it *is* the declaration of something implemented outside BBNF. `D§4.6` already had this row for `@host`; the corpus declares at the use site instead (0 `@host` uses, 5 use-site declarations). `D§2`'s withdrawn diff `SemanticTypeData::Value(name)` stays withdrawn as a general mechanism, but its content is what `Host(T)` is; the difference is that `Host(T)` arises only from forms 6 and 7 and form 5 is checked. Whether E-HOST is acceptable, or the corpus must be migrated to declared host functions, is owner choice **O2**.

`String` (`json.bbnf:10`) is `Host(String)`, not `Str`: nothing in the grammar says what an arena string is. Commented-out sites of form 7 (`google-sheets.bbnf:11`, `:62`) take the same rule if restored.

---

## 5. M1 to M9

**M1 (monomorphic recursion rejects ordinary grammars). Design change.** Under §1.3 instances are ground and each is its own concrete rule.
- `alt = |x, y| x , ("," , alt(y, x)) ? ; r = alt(/[0-9]+/, ("a" , "b")) ;` gives two instances, `alt⟨d, ab⟩` and `alt⟨ab, d⟩`, each referring to the other; both lie on a cycle, so both are nominal: `alt⟨0⟩ ≜ Span × Option (Span × alt⟨1⟩)`, `alt⟨1⟩ ≜ (Span × Span) × Option (Span × alt⟨0⟩)`. Accepted.
- `list = |x| "[" , (x | expr) * , "]" ; expr = list(atom) | atom ; atom = /[a-z]+/ ; nums = list((/[0-9]+/ , "!")) ;` gives `list⟨atom⟩` and `list⟨A₁⟩`. Accepted. (As `R` wrote it, `list(/[0-9]+/ , "!")` is a two-argument call: a call argument is a `binary_factor`, `grammar/bbnf/bbnf.bbnf:24-27`. The parenthesised form is the intended program.)
- `R`'s independent suggestion, dropping edges to cyclic parameterless rules when forming binding groups, is moot: the grammar layer has no binding groups for types. The wording fault it names (`D§4.4` step 2 "fresh monotype" against NOM) disappears with them.
Both programs join the corpus as M1a, M1b.

**M2 (NUM and DEFAULT make typeability depend on the partition). Design change for NUM; refused for parameterless rules.**
- *Changed.* A numeric variable is generalised with its kind (§1.3): `add = |a, b| a + b` is `∀ν:num. fn(ν, ν) → ν`, and `add(1u8, 2u8)` is accepted whichever component `add` is in. An unsuffixed integer literal is a `num` variable, so `1 + 2u8` is `u8`, and `D` V04 (`"a" -> 1 + 2`) is `P i64` by CLOSE, which makes its "(NUM default)" label true.
- *Refused:* generalising a parameterless rule whose body type has free variables as a generic rule per `(RuleId, TypeArgs)`. Reasons. (1) It puts type variables back into branch types, and `e = ("a" -> nil()) | ("b" -> nil())` is then F1 again: `Sum[List α₁, List α₂]` whose instance at `Unit` is not what the instantiated program gets. (2) A rule's type is public (`LayoutFacts`, the entry rule, `obs()`); the entry rule has no user to instantiate it, so a default is needed there in any case, and a rule would then have one type as an entry and another as a callee. (3) The instance key would need types before typing has run.
- *Accepted cost, with programs and a diagnostic.* N14: `empty = "[]" -> nil() ; bytes = empty -> |xs| cons(1u8, xs) ;` is refused, `List Unit ≠ List u8`. N15: `k = "a" -> 1 ; j = k -> |v| v + 1u8 ;` is refused, `i64 ≠ u8`. The certificate records, for each closed variable, the mapping node that closed it (`MappingClosed`), so the message says "`Unit` was fixed here because nothing in this mapping constrains it" with the span of `-> nil()`. The author's cure is local: a suffix or a checked ascription at that mapping.

**M3 (STAGE has a hole). Design change.** Clause 1 is unwritable (§1.2). **Clause 2:** a top-level `|params| body` is either a parametric rule (every parameter and the result have sorts built from `par`) or a value function (every parameter and the result are value types); a definition that mixes them is refused with `BBNF-STAGE`. `count = |p, n| match n { 0 => ε, _ => p , count(p, n - 1) }` is refused (N16). **Clause 3:** the type of a mapping, and so of any rule, contains no `fn`; `r = "a" -> |v| |n| n + 1` is refused (N17). `BBNF-STAGE` is an addition to ARCH §7.5's catalogue (§7).

**M4 (`match` cannot destructure an alternation). Design change, and owner choice O6.**
- `None` and `Some(p)` are built-in constructors of `Option`; they determine their type, so they cost no principality.
- A `Sum[τ₁ … τₙ]` gets variant names: branch i is named after the rule it refers to if the branch is a bare rule reference and that name is unique among the branches; otherwise `_i`. This follows `PASS-1:89` ("grammar-aware variant names").
- A pattern `name(p)` does not determine which `Sum` it belongs to (one rule can be a branch of many alternations). So a `match` with sum patterns is typed only where the scrutinee's type is already known: at a mapping, where `input` is ground; or in a named value function, after the component's equalities are solved, if the scrutinee's type then has a `Sum` head. If it is still a variable, the `match` is refused and the cure is a parameter annotation (which needs C4a). This is Standard ML's treatment of unresolved record patterns, and it is a second stated exception to R-INF (E-SUM). A fully inferred alternative exists (constructors qualified by their rule, `value.object(p)`), but it needs new pattern syntax: that is O6.
- Corpus: V18 ‡ `v = a | b ; a = "x" ; b = "y" , "z" ; n = v -> |s| match s { a(t) => 0u8, b(u) => 1u8 } ;` expects `n : P u8`.

**M5 (the Turing-completeness proof does not walk). Redone in §6; outcome: owner choice.**

**M6 (Lock 2 is not met by the "Lock 2 gate"). Design change.** `D§7` H2 gate (e) is renamed **"Lock 2 partial: type carrier private, `LayoutFacts` exported"** and closes nothing. `Layout` (the record: one per rule and node, the output of the lay-out step) and `LayoutSink` (the trait a consumer implements to receive them) are scheduled in H2a with their own gate: both names public, and the emitter, the registry-sidecar renderer and the plan serialiser implement `LayoutSink` by H3. Two deviations are recorded, not hidden: Lock 2 puts the module at `passes::layout` under a `passes` crate, and root has no `passes` crate (`ls crates`: `analysis bbnf-path bbnf-path-ts bootstrap core corpus gorgeous ir lsp oracle ser`; only `skinny/crates/passes` exists); and the SK-V17 clause prices two routes for `StructLayout` (`LOCKS:616`) while `D` took a third. Both are owner choice O3.

**M7 (C5 option (a) is a second expression IR). Accepted.** Option (a) is withdrawn: a function table with its own node alphabet and a per-backend lowerer is an IR by another name, and asking the owner to rule that it is "not a variant" is what the Governance Boundary bars (`LOCKS:652-661`; "new BIR variant … remains G-Omega gated", `LOCKS:271-274`). C5 is re-posed in §8 with two options and their costs. Consequence for staging: `D`'s H3 said value closures lower "to `CallHost`"; `CallHost` carries a host function id (`ARCH:994`), not a closure. H2 may *type* inline closures; lowering them waits for C5.

**M8 (the instance key contradicts the locked one). Design change; one part refused.**
- *Key.* §1.3 (I): the key is `(rule, argument rules)`; the locked `(RuleId, TypeArgs)` set (`ARCH:1823`, `:1613`) is its projection and is recorded in the table. A finite instance set implies a finite `(RuleId, TypeArgs)` set, so the locked evidence exists whenever compilation succeeds. The converse fails (`D` N10 has one `TypeArgs` and infinitely many instances), which is why the locked diagnostic's text is wrong for that case.
- *Budget and text.* A separate `instances` budget (§2). New diagnostic text for "the instance set grows without bound" amends `PASS-1:128`; accepting recursive parametric rules with no annotation (`D` F06, F09) overrides `PASS-1:93`'s two-path contract. Both are owner choice O5; `D` cited those lines without saying it overrode them.
- *`D§0` row 3* should read "partly new": Lock 10 gives lambdas, `&'i` capture and `fn(T) -> U` (`LOCKS:269`); parser-valued parameters, functions over functions over parsers, named value functions and recursion in them are not in it.
- *Refused: the review's early stop as stated* ("stop at the first instance whose argument strictly contains its requester's argument on the same parameter"). Counterexample: `f = |x| x , ("," , f(("a" , "b")))? ; r = f("a") ;`. `f⟨"a"⟩` requests `f⟨("a","b")⟩`, whose argument strictly contains `"a"`; the rule refuses. But `f⟨("a","b")⟩` requests itself, the set has two instances, and the recursion consumes input, so the grammar is legitimate. The containment is a coincidence of spelling. *Replacement:* stop when an instance of `f` requests an instance of `f` whose argument at some position was **built by substituting** the requester's argument at that position into a strictly larger expression written in a template body (provenance carried by the substitution, not compared after the fact). For a direct self-call this is sound: a parametric rule cannot inspect its arguments (clause 2 of STAGE), so the same call site fires again from the new instance with a strictly larger argument, forever. For chains through other templates and through template-valued parameters I believe the same argument holds and have not proved it (§12); the gate is a property test: over 10⁵ generated template programs, an early-stop refusal implies a budget refusal with the early stop switched off.

**M9 (H2 deletes a file later stages read; rules missing). Design change.**
- *Restaging:* removed by §2. What remains is real and is now in H2a's content and size (§10): `lower.rs` reads `TypeSolution` at five places (`crates/core/src/compiler/lower.rs:475`, `:598`, `:610`, `:623`, `:649`), `replay.rs` stores and serialises it (`replay.rs:38`, `:66`, `:220`), `verify.rs` reads it (`verify.rs:45`, `:363-373`), and the public stage list names it (`mod.rs:44`).
- *`typecheck.rs` is not deleted.* Under instance typing the grammar layer is ground, bottom-up synthesis with certificates, which is what that file does (`typecheck.rs:1144-1201`). It is split under the 500-line rule (Lock 13) and gains ALT's collapse, NOM's transparency and the mapping row. `D§4.10`'s `infer_grammar.rs` and `alt.rs` are withdrawn.
- *Mapping needs no new node kind.* The program already has `NodeKindData::Transform { child, transform, .. }`, typed today as its child (`typecheck.rs:1176-1181`), and events already carry transform ids (law `TransformSites`, in the list at `verify.rs:74-117`). A mapping resolves to that node kind; its type becomes MAP's. No new plan op; RULINGS 15a's "new spine node kinds" stay held.
- *Record, Projection:* rows in §1.3 (G). `ProjectionKeep` has exactly `Left` and `Right` (`crates/core/src/compiler/syntax.rs:111-114`), which are `<<` and `>>`.
- *Pratt.* Rule PRATT: mining is type-preserving. A rule's type is fixed by its source alternation under (G) and NOM; a miner that later recognises the shape must produce values of that type. Stated as law L-PRATT-TYPE. What I could not resolve: today's spine Pratt node is built from an instance-declared table and wraps the rule's root as its atom (`syntax.rs:800-826`), and is typed as that atom (`typecheck.rs:1391-1404`). A parse of `1 + 2` is not a value of the atom's type. That is an existing gap between the spine's types and its values, not one this design opens, and it cannot be closed without knowing which Pratt source form is the language (Lock 10's mined form, or RULINGS COMP1C-2's "declarations"). Until then Pratt rules are excluded from L-PARSE-TYPED by name. Listed in §12.
- *Occurs check:* `R` found no legitimate grammar it refuses; agreed. In this design it lives in sort inference and in the value layer.

**Should-fix and notes, briefly.** S1: answered in (V). S2: accepted, levels deleted. S3: accepted; `LayoutFacts` exports ground instance signatures only, schemes stay in the private carrier. S4: accepted; the lay-out step is defined as the CSP's default assignment, and H5 carries a gate that turning the CSP on is byte-neutral on the corpus. S5: accepted as a correction to `D§0`: bounded coercion, bounded lookbehind, typed directives and the schema-mining miner (`ARCH:1869-1877`) are scheduled nowhere by `D` or by this file, and "full inference" is a move from the miner's "most cases" to "all of F0", not wholly new. S6: C4b. N1: `LayoutFacts` with ordered maps is an edit to ARCH §7.3; listed in §7. N2: `nil`/`cons` allocate in the parse arena; the Lock 9 reason is that a list built by a transform has no slice of the input to borrow. N3: decided in ALT (no unfolding) and exposed as O1. N4: catalogue additions listed in §7. N5: `a = a ;` is refused by `RecursiveRule` in the facts stage; the lay-out step runs after facts or not at all for a refused program, which the stage order already guarantees (`mod.rs:118-140`).

---

## 6. M5: Turing completeness, redone

### 6.1 The grammar of record cannot do it

The value layer of record (`grammar/bbnf/expressions.bbnf:5-34`) has literals, paths, `input`, calls, unary and binary operators and an inline closure. It has no conditional on data other than `&&` and `||`, no binding form, and no way for a value expression to name itself: the only top-level definitions are rules, and a top-level `|p| body` has a grammar body (`grammar/bbnf/bbnf.bbnf:51-52`), so `inc = |n| n + 1` reads `n +` as a repetition (`R` M5, confirmed). Every evaluation is therefore bounded by the size of the expression, apart from what host functions do. **The value layer of the grammar of record is not Turing complete, and no typing discipline changes that.** Self-application is refused by the occurs check, as it must be.

There are two ways to make it so. Neither is free, so this is owner choice **C3c**.

- **Route A: the locked function surface.** `match` (`PASS-1:243-249`; Lock 4 presupposes arms, `LOCKS:179`), tuples (`PASS-1:250`), and a named function with a BBNF body, for which the locked form is block-bodied `@host fn` (`PASS-1:258`). No lock is edited. Three things are still needed: these forms are not in the grammar of record, which RULINGS COMP1C-1 made "the only surface"; `Block` is defined nowhere (`D§0.3`); and recursion inside such a body is addressed by no lock and contradicts ARCH §8.4 (`ARCH:1900-1902`), which is C2.
- **Route B: control in the prelude.** Add `cond : ∀α. fn(Bool, fn(Unit) → α, fn(Unit) → α) → α` and a fixed-point primitive `fix : ∀αβγ. fn(fn(fn(α, β) → γ, α, β) → γ, α, β) → γ`, plus list primitives. No syntax and no lock text changes, and the machine below can be written in today's `value_expr`. But the recursion is then performed by a host primitive, and that any host function can compute anything was never in doubt. Route B proves the host interface is Turing complete, not the language. I do not offer it as meeting the owner's sentence.

### 6.2 The walk, on Route A

A counter machine with four registers (Minsky 1967: counter machines are Turing complete; two registers suffice only under an exponential input encoding, which is why `D`'s two-counter multiplier on raw inputs could not exist, as `R` said). Registers are lists; a register's value is its length. The program multiplies: on `(a, b, 0, 0)` it halts with `(0, b, a·b, 0)`.

```
@host fn q0(a, b, c, t) { match uncons(a) {
    None          => (a, b, c, t),
    Some((h, a1)) => q1(a1, b, c, t) } }
@host fn q1(a, b, c, t) { match uncons(b) {
    None          => q2(a, b, c, t),
    Some((h, b1)) => q1(a, b1, cons(h, c), cons(h, t)) } }
@host fn q2(a, b, c, t) { match uncons(t) {
    None          => q0(a, b, c, t),
    Some((h, t1)) => q2(a, cons(h, b), c, t1) } }
r = "go" -> |v| q0(cons(true, cons(true, cons(true, nil()))),
                   cons(true, cons(true, cons(true, cons(true, nil())))), nil(), nil()) ;
```

(Written without parameter annotations, which needs C4a; with them it is writable today only if `TupleType` is defined, C4b.)

*Components.* Call edges: `q0 → q1`; `q1 → q1, q2`; `q2 → q2, q0`. One strongly connected component `{q0, q1, q2}`; halting is the `None` arm of `q0`, so there is no separate halt state outside the component (the error `R` found in `D`). `uncons`, `cons`, `nil` are prelude schemes. `r` is its own component, after.

*Types.* In `q0`: `uncons(a)` gives `a : List e₁`; the `None` arm gives the result type `List e₁ × β₂ × β₃ × β₄`; the call fixes `q1`'s parameters to `q0`'s. In `q1`: `uncons(b)` gives `b : List e₂`; `cons(h, c)` and `cons(h, t)` give `c, t : List e₂`. In `q2`: `uncons(t)` agrees; `cons(h, b)` agrees. Solved and closed over what remains: each is `∀αβ. fn(List α, List β, List β, List β) → List α × List β × List β × List β`. No annotation. At `r` both variables are `Bool`; `r : P R`, `R ≜ List Bool × List Bool × List Bool × List Bool`, lengths `(0, 4, 12, 0)`.

*Run.* Each outer round: one `q0` step, `b` steps of `q1` plus its exit, `b` steps of `q2` plus its exit. For `(3, 4)`: 3 × (1 + 5 + 5) + 1 = 34 calls.

*Tail calls.* Every call inside the component is the whole body of a `match` arm that is the whole body of its function. **Lowering obligation L-TAIL:** a component of value functions in which every intra-component call is in tail position lowers to one loop over a state value; it adds nothing to nesting. Without it, each machine step is a host stack frame and a nested call, and the run is cut at `RuntimeBudget.nesting = 256` (`crates/core/src/compiler/budget.rs:57-66`, enforced by `with_depth`, `crates/core/src/compiler/emit.rs:465-478`): the layer would compute runs of at most 256 steps. A non-tail recursion (`D` V11, factorial) does consume nesting and faults at the limit; that is stated, not hidden.

*Budget.* Each value-function call and each `cons` charges one unit. **The claim, worded correctly:** for every counter machine M and input w on which M halts, there is a budget W such that the program above for M, run with transform budget W, returns M's result; for every fixed W the layer computes a finite class of functions, as every real machine does. "Turing complete" is a property of the semantics with the budget as a parameter.

*The shared work counter.* Today one counter serves everything: `charge_work` increments a single `limits.work` (`emit.rs:454-463`) and is called on entry to every node, Pratt table and rule (`emit.rs:753`, `:924`, `:1023`). If transforms charged it, an input accepted with identity transforms could fault with real ones. So: transforms charge their own counter (a new `transform_work` field of `RuntimeBudget`, which is `#[non_exhaustive]`, `budget.rs:48-49`) and fault with their own kind. And **COMMIT:** a transform is evaluated only on a result that survived to the end of recognition, never during it. Then L-MATCH-INVARIANT is exact, not "modulo fault": the recognition outcome (matched, mismatched or a recognition fault, and the cursor) is identical with all transforms replaced by the identity; only the *value* can additionally be a transform fault. The spine emits an event stream with transform ids and does not evaluate transforms at all today (`emit.rs:271-361` per `D§1.1`, not re-opened), so COMMIT constrains the consumer that is yet to be written, not existing code.

*Universality inside one grammar* (`D`'s T-TC-2) does not need sum patterns: parse each instruction to a tuple `(u8 opcode, register, target, target)` with ordinary mappings and dispatch with `match op { 0u8 => …, 1u8 => …, _ => … }` on literals, which is fully inferred. Writing it over a parsed `Sum` instead falls under E-SUM (M4).

### 6.3 What this needs from the owner

C2 (Turing complete at all, and where), C3c (which route), C4a/C4b (annotations optional, `Block`, `TupleType`), C5 (how bodies are emitted). If C5 is answered "bodies are not lowered in V1", R-TC is deferred whatever the other answers are.

---

## 7. De-duplication against the locks

One row per design element. "Covered" means the lock already requires it and this file adds only a gate; nothing in the third column is decided here.

| Design element | Lock that already covers it | If not: status |
|---|---|---|
| Type solver is a private subroutine of layout lowering; no public type stage | Lock 2 (`LOCKS:160`) | covered |
| `LayoutFacts` is the one public side table | Lock 2 | covered |
| `Layout` record and `LayoutSink` trait exist and are public | Lock 2 and its v+1 note (`LOCKS:162-166`) | covered, unbuilt; scheduled H2a/H3 |
| Module path `passes::layout` in a `passes` crate | Lock 2 | covered; **deviation** (spine lives in `crates/core/src/compiler/`): O3 |
| `StructLayout` reconcile (962 sites) | Lock 2 SK-V17 clause (`LOCKS:616`): two priced routes | **third route**, owner choice O3 |
| HM result piped into a finite CSP; never fused with the e-graph | Lock 4 (`LOCKS:179`), Lock 6 tail (`LOCKS:183`) | covered |
| Higher-rank (DK13) and GADT refinements in V1 | Lock 4 | covered; not designed here; timing is C1 |
| Closures capture by `&'i` only | Lock 4, Lock 10 | covered |
| Lambda literals; `fn(T) -> U` in `Type` | Lock 10 (`LOCKS:269`) | covered |
| `match` arms | Lock 4 (presupposed), `PASS-1:243-249` | covered by the pass contract, absent from the grammar of record: C3c |
| Parser-valued parameters; functions over functions over parsers; instance key by argument | none (`ARCH:1823` keys by `TypeArgs`) | new: O4, O5 |
| Sort inference for parametric rules | none | new, internal to layout; rides O4 |
| ALT collapse, NOM transparency, CLOSE, kinds `num`/`eq` | none (ARCH §8.2 names HM equality only) | new; they fix public value types: O1 |
| Mapping forms SPAN, CONV, HOST-ASCRIBE, HOST-CALL | Lock 14 surface (c) covers host implementations in a declaration crate; nothing covers use-site declaration | new: O2 |
| Recursion in value functions; Turing completeness | none; ARCH §8.4 says the opposite | new: C2, C3c |
| Transform budget, COMMIT, L-TAIL | none | new; riders of C2 |
| Sum variant names in patterns | `PASS-1:89` (names are "grammar-aware") | partly; pattern resolution is O6 |
| Lowering of value-function bodies | Lock 5 (`LOCKS:181`): the IR is the contract; new variant G-Omega gated (`LOCKS:271-274`, `:652-661`) | not covered by any existing variant: C5 |
| Six directives; none added | Lock 10 | covered; this design adds no directive |
| Pratt mined, never annotated; L-PRATT-TYPE | Lock 10 | covered; the law is a gate, not a directive |
| Five `BackendShape` variants | Lock 10 | untouched |
| No file over 500 lines; one concern per directory | Lock 13 (`LOCKS:336`) | covered; `typecheck.rs` (1,634 lines) is split in H2a |
| No grammar or rule name in generic code | Lock 14 (`LOCKS:349`) | covered; `Host(T)` names come from grammar source |
| No proc-macro; generated files not hand-edited | Lock 6 | covered |
| `nil`/`cons` allocate | Lock 9 (reason required) | covered; reason given (§5 N2) |
| TS backend work in H3 | Lock 5: TS defers post-V1 | **conflict** with LG-b parity; inside O1 |
| New diagnostics `BBNF-STAGE`, instance-growth text, `BBNF-PREDICATE-RECURSIVE` (`D§5.4`), host-undeclared refusal | none (ARCH §7.5 catalogue) | catalogue edits; owner (O5 for the growth text; the rest ride C2, O2) |
| `LayoutFacts` with ordered maps and an instance table | ARCH §7.3 specifies the fields | ARCH edit; owner, rides O3 |

Corrections to `D§0`'s table: row "functional" is "partly new" (M8); row "full inference" is "new beyond the miner's 'most cases'" (S5); row "robust types" covers five of ARCH §8.2's nine rule rows (S5).

---

## 8. Owner choices

Each: the question in plain words, the options, what each costs, a recommendation. C3 is split in three and C4 in two; O1 to O6 are added.

**C1. Do the annotated type features stay in V1?** Lock 4 puts higher-rank types (DK13) and GADT refinements in V1; ARCH §8.2 adds bounded coercion. None can be inferred without annotations (`D§4.6`; that part is a theorem, not a choice). The choice is timing.
- (a) Keep them in V1, built after the core, each entered only by writing its annotation. Cost: about 2,500 lines in LW5 (`D`'s estimate, not re-made); and one restriction this design needs: numeric widening applies only at a written annotation, never at an operator operand, or it collides with numeric variables (`1u8 + x`).
- (b) Move them after V1. Cost: a lock edit through Pass Omega; saves the LW5 work.
- *Recommend (a)* with the restriction.

**C2. Is BBNF allowed to be Turing complete, in transforms only?** ARCH §8.4 says closures exist "without turning BBNF into a general programming language"; the owner's sentence says "turing complete".
- (a) Yes: amend that sentence. Recognition and the generator stay total. Riders, each needed for (a) to be safe: transforms run after recognition (COMMIT); they have their own budget and fault; tail-recursive components compile to loops (L-TAIL); mixed-stage functions are refused (STAGE clause 2). Cost: the riders are about 400 lines across H3 and H4, and one ARCH sentence.
- (b) No: ARCH §8.4 stands; the value layer keeps inline closures and host calls. Cost: the owner's sentence is not met; H4 shrinks to `match` without recursion or disappears.
- *Recommend (a) with all four riders.*

**C3a. Which directive set is the language?** RULINGS COMP1C-1 accepts `@ws`, `@recover`, `@debug` and bodyless `@host` (they are in `grammar/bbnf/bbnf.bbnf:65-73`); Lock 10 retires or reshapes all four.
- (a) Leave COMP1C-1 standing. Cost: a standing contradiction with Lock 10 that someone must resolve later.
- (b) Lock 10 governs; migrate the corpus (2 + 1 + 0 + 0 files per `D§0.3`). Cost: reverses a ruling two days old; a migration.
- *The type design needs neither.* One interaction: bodyless `@host name : type` (`bbnf.bbnf:73`) is the only host declaration form that exists today, which matters to O2 option (b). *Recommend (a) for now*, decided with Lock 10's owner rather than inside this design.

**C3b. Do the operators the locks never name stay?** `?w`, `<<`, `>>`, `-`, `[ ]`, `{ }`, `@{ }`, per-factor `->`, the `.` terminator (`bbnf.bbnf:29-46`, `:56`).
- (a) They stay; the locked productions are amended to include them. Cost: a G-Omega amendment of text; `{ }` repetition collides with the locked `{n,m}` quantifier and with `Block` (C4b).
- (b) They go; the corpus is rewritten to the locked forms. Cost: every generated grammar changes; some forms (`@{ }`, per-factor `->`) have no locked equivalent.
- *Recommend (a).* §1 and §4 are written on these operators.

**C3c. Does the function surface enter the language, and by which route?** (§6.1.)
- (a) Route A: add `match`, tuples and block-bodied named functions to the grammar of record. Cost: amends COMP1C-1's "only surface"; needs C4b; H4, 1,800 to 2,400 lines plus C5.
- (b) Route B: `cond` and `fix` in the prelude. Cost: small (about 200 lines), but the recursion is a host primitive and programs are unreadable; I do not think it meets the owner's sentence.
- (c) Neither in V1. Cost: R-TC unmet; generalisation in the value layer has no user program.
- *Recommend (a).*

**C4a. May parameter and return annotations be left out?** Locked: `Param ::= Ident ":" Type` (`ARCH:1730`), declared generics (`PASS-1:93`).
- (a) Optional; a written one is checked. Cost: a G-Omega amendment of two productions.
- (b) Mandatory as locked. Cost: "full inference" is false for every function; and the grammar of record has no parameter annotation syntax at all (`bbnf.bbnf:51`), so (b) also requires adding it.
- *Recommend (a).*

**C4b. What are `Block` and `TupleType`?** Both are used by locked productions and defined nowhere. `{ … }` is repetition in the grammar of record (`bbnf.bbnf:36`), so `|x| { e }` is ambiguous (`R` S6).
- (a) `Block = "{" Expr "}"`, admitted only as the body of `@host fn`; in a lambda body braces keep their repetition meaning. `TupleType = "(" Type "," Type { "," Type } ")"`. Cost: `LambdaExpr`'s `| Block` alternative goes unused; one G-Omega amendment.
- (b) `Block` also in lambda bodies; a repetition there must be parenthesised. Cost: a silent change of meaning for any existing `|x| { e }` (0 corpus uses, since the corpus has no function definitions).
- (c) Retire `{ }` repetition in favour of `*`. Cost: corpus rewrite; conflicts with C3b (a).
- *Recommend (a).*

**C5. How does a function body reach generated code?** The locked Backend IR has no variant for a lambda, an application or a `match`; ARCH forbids emitting from Grammar IR (`ARCH:1917`).
- (b) Amend the IR through G-Omega: new variants for closure creation, application, `match` and tuple (literals, operators and primitives may fit existing variants; I did not verify that). Cost: a governance amendment; each variant needs a Rust lowerer and VM replay ("VM can replay all BIR variants", `ARCH:1057`); my guess is 2,000 to 3,500 lines, made without reading the BIR code. Gain: the owner's sentence is met in V1.
- (c) Do not lower bodies in V1: a mapping may be a constant, a conversion, or a declared host function; inline closures are typed and then refused at lowering with a named diagnostic. Cost: R-TC and the functional layer are typed but do not run in V1; H4 is deferred. Gain: the 20-variant target is untouched and all 585 corpus mappings still work, since none is a closure (§4).
- *Recommend (b)*, because (c) leaves the owner's explicit order unshipped; but (c) is the cheaper and safer option and loses nothing the corpus uses today. `D`'s option (a) is withdrawn (M7).

**O1. Are these the public value types of user grammars?** ALT (all branches identical: that type; otherwise a tagged sum, no partial merge, no unfolding of names), NOM (a rule is a named type unless its body is a base type), and tagging. They differ from root's output on real grammars (the lawful join alone moved 21 of 70 MANIFEST entries, per `D§1.1`), and the TS package documents alternation as untagged (per `R` §3).
- (a) Adopt them; H2a produces a difference report per MANIFEST entry against root's column, and the owner rules on the report as a class. Cost: `regen --check` 9/9 is re-baselined once at H3; TS diverges on every uncollapsed sum, and Lock 5 defers TS past V1 while LG-b demands parity, so the owner must say which governs.
- (b) Reproduce root's types bit for bit. Cost: the lay-out step re-implements root's Span compression, tuple flattening and payload heuristics, which are the non-lattice code BR3 condemned.
- *Recommend (a).*

**O2. What do `-> f64`, `: T` on `input`, and undeclared host paths mean?** (§4, forms 4, 6, 7.)
- (a) As §4: CONV, HOST-ASCRIBE, HOST-CALL; "full inference" has the stated exception E-HOST. Cost: an annotation at five corpus sites is trusted as a declaration.
- (b) Require a host declaration (`@host name : type`, or Lock 10's block-bodied form) and migrate the five sites; annotations at uses are then always checked. Cost: a corpus edit in CSS and JSON; depends on C3a.
- Either way the owner must say what CONV does on text that is not a number of that type (`expressions.bbnf:6` matches `12abc`): a transform fault, or a total conversion with a defined value. I did not find root's behaviour.
- *Recommend (a) now, (b) as the migration target; conversion failure is a transform fault.*

**O3. Which Lock 2 route?**
- (a) Root's `StructLayout` sites are not renamed; they retire with root at LW4.COMP-1, and the spine builds `Layout`, `LayoutSink`, `LayoutFacts` under `crates/core/src/compiler/layout/`, to move into a `passes` crate when the workspace shape lands. Cost: Lock 2 stays open until LW4.COMP-1; the retired-term count does not move before then.
- (b) Rename now (962 hits, 78 files). Cost: churn in code that is scheduled to be deleted.
- (c) Pass Omega narrows Lock 2 to `LayoutFacts`. Cost: a lock edit.
- *Recommend (a).*

**O4. Is instance-site typing acceptable under the words "Hindley-Milner"?** (§1.1.) Parametric rules get principal sorts at definition and types at instances; HM over types is the value layer.
- (a) Yes. Cost: template bodies' type errors surface at instances; `ARCH:1823`'s "generalized at definition" is read as "sort-generalised".
- (b) No: definition-site type schemes for parametric rules. Cost: rule ALT then has no principal types and rejects typeable programs (`R` F1), and that must be written into the language definition with `R`'s counterexamples as accepted behaviour.
- *Recommend (a).*

**O5. May a recursive parametric rule be accepted with no annotation, and what does unbounded growth say?** `PASS-1:93` requires an annotation or rejection; `PASS-1:128`'s message speaks of type arguments.
- (a) Accept when the instance set is finite; a separate `instances` budget; new message text naming the growing argument chain. Cost: amends two PASS-1 lines and the ARCH catalogue.
- (b) Keep PASS-1: recursion needs a return annotation. Cost: there is no syntax for one in the grammar of record, and under O4 (a) a return annotation has nothing to check that instantiation does not already compute.
- *Recommend (a).*

**O6. How does a pattern name a branch of an alternation?** (M4.)
- (a) By rule name or ordinal, resolved from the scrutinee's known type; a `match` on a parameter of unknown type needs an annotation (E-SUM). Cost: a second exception to full inference. No new syntax.
- (b) Constructors qualified by their rule (`value.object(p)`). Cost: new pattern syntax beyond `PASS-1:247-248`; fully inferred.
- *Recommend (a) for V1.*

---

## 9. Test plan: the thirteen wrong implementations

L-PRINCIPAL is restated so that it is neither vacuous nor false. The type-directed generator builds each value term **together with a derivation at some type τ_gen**, independently of any solver. (a) Every generated term is accepted. (b) τ_gen is an instance of the inferred type. A solver that returns a less general type fails (b) on some term; one that rejects a typeable term fails (a). No enumerator is needed, and the declarative system it tests against is §1.3 (V). L-REF's reference is textbook Algorithm W with kinded variables. L-TEMPLATE tests PT-4: for generated acyclic template programs, the type of each call equals the type of the hand-inlined program.

New corpus programs (K-series), each a file with its expected output:

| Id | Program | Expected |
|---|---|---|
| K01 | `opt = \|x\| x? ; w = \|f, x, y\| (x \| y) , f(x) , f(y) ; r = w(opt, "a", "b") ;` | `r : P R`, `R ≜ Span × Option Span × Option Span` |
| K02a | `r = sep("a") , sep(("b" , "c")) ; sep = \|x\| x , ("," , x)* ;` | 2 instances; `R ≜ (Span × List (Span × Span)) × ((Span × Span) × List (Span × (Span × Span)))` |
| K02 ‡ | `D` V15 with `id` defined after `r` | as V15 |
| K03 | `many = \|x\| (x , many(x)) \| ε ; r = many("a") , many("b") ;` | 2 instances; accepts `aabb`; a parser that rejects it is wrong |
| K04 ‡ | `@host fn f(x) { (g(1u8), g(true)) } @host fn g(y) { match f(y) { _ => y } }` | refused: `u8 ≠ Bool` inside component `{f, g}` |
| K05 | `a = b \| c ; b = "x" , "y" ; c = "x" , "y" ;` | `a : P A`, `A ≜ Sum[B, C]` |
| K06 | `e = ("a" -> nil()) \| ("b" -> nil()) ;` | `P E`, `E ≜ List Unit` |
| K08 | `a = ( /[a-z]+/ , "!" ) - "if" ; b = "x"? >> ("y" , "z") ; c = "x"? << ("y" , "z") ;` | `A ≜ Span × Span`; `B ≜ Span × Span`; `C ≜ Option Span` |
| K09 | `x = "a" \| "b" \| ("c" , "d") ;` on inputs `a`, `b` | `X ≜ Sum[Span, Span, Span × Span]`; the value's tag is 0 for `a`, 1 for `b`, compared with the branch the reference interpreter took |
| K12 | `r = ("a" -> nil()) , ("b" -> cons(1u8, nil())) ;` | `R ≜ List Unit × List u8` |
| K13 ‡ | §6.2's machine on `(40, 40)` (3,321 calls), and at transform budgets 2¹², 2¹⁶, 2²⁰ with inputs sized to need at least half of each | halts with `c = 1600` under the default budgets; under each budget halts when the run fits and faults with the transform kind, never `Nesting`, when it does not |

| # | Wrong implementation (`R` §5) | Killed by |
|---|---|---|
| W1 | ALT resolved eagerly, in traversal order | **Not a mutant any more.** Branch types are ground when an alternation is read, so eager resolution is the specification. The mutant it stood for, "decide an alternation at the definition of a parametric rule", is killed by **K01** (it yields `Sum[Span, Span]` in the first component). |
| W2 | Binding groups in source order; a forward reference is monomorphic | **K02a** in H2a (a template used before its definition, at two types); **K02** in H4 for value functions. |
| W3 | Instances keyed by `(rule, type arguments)` | **K03**: both instances have `TypeArgs = (Span)`; the wrong key parses `"a"*` twice and rejects `aabb`. |
| W4 | Members of a component generalised one at a time | **K04**: generalising `g` before `f` accepts it. |
| W5 | ALT equality up to nominal unfolding | **K05**: unfolding collapses it to `B`. |
| W6 | DEFAULT applied before or after ALT | The order is now forced by structure; the surviving mutant is "close at the rule, not at the mapping". **K06**: late closing gives `Sum[List Unit, List Unit]`. |
| W7 | Occurs check missing on the binding made in ALT step 3 | **Equivalent mutant:** ALT performs no unification, so that binding does not exist. The occurs check itself is killed by `D` N01 (`omega = \|x\| x(x)`), now an occurs failure on sorts in the same unifier. |
| W8 | `a - b` typed as `τ_b` | **K08** (and it pins `<<` and `>>` with unequal operand types, which `D` G08 did not). |
| W9 | Two sum tags swapped at layout | **K09** at H3, by tag, where `has_type` is blind; and verifier law `VariantIsBranch` with a mutant at H2a. |
| W10 | Generalise a variable free in the environment | **Equivalent mutant** (`R` S2): every binding is top-level, no such variable exists; levels are deleted and the mutant is removed from L-PRINCIPAL's list. |
| W11 | ALT collapses when branches unify | **Equivalent mutant on ground types** (ground terms unify exactly when identical), provided branch types are ground. That proviso is law `GroundAtNode`, whose mutant is killed by **K06**. `D` F04 no longer kills it and is re-expected as `P Span`. |
| W12 | Instantiate without fresh variables | **K12** in H2a (two uses of `nil`); `D` V15 in H4. |
| W13 | Value recursion limited to bounded iteration | **K13**: any bound below the budget fails one of the three sizes; a nesting-bounded implementation faults with the wrong kind. |

Eleven of thirteen are killed by a named program; W7 and W10 are declared equivalent with the reason; W11 is equivalent only under a law that is itself mutant-tested.

Other changes to `D§6`. Expected outputs of F01-F10 become a sort scheme for the definition plus instance types (F04's `r` is `P Span`; F06's nominal type is `many⟨0⟩`). N01, N02 are occurs failures on sorts. N09 and N10 are instance-growth refusals at depth 2. N03-N05 are sort mismatches. V08-V16, X02, T-TC-1, T-TC-2 are ‡ (they need C3c's surface; `D` gated V08 and V10 in H2, which `R` showed cannot parse there). V09 is split: `id` cannot be both a template and a value function. New: M1a, M1b, N14-N17, V18 ‡, and H01-H07, one program per mapping form of §4 taken verbatim from the cited corpus line. L-ALT-STABLE is replaced by L-TEMPLATE. L-ORDER is kept for certificate order only. L-PARSE-TYPED excludes Pratt rules by name (M9) and gains the tag comparison. G-DIGEST becomes a report for O1, not a pass/fail gate, until O1 is ruled.

---

## 10. Waves, re-sized

Source lines, tests excluded, each an estimate from reading, not from a prototype. "`StructLayout`" is the 962-hit count (78 files; 237 hits in 69 files outside paths named `generated`; **0 under `crates/core/src/compiler/`**, all re-counted with `git grep -c` for this file).

| Wave | Content | Size | `StructLayout` hits it touches | Blocked by |
|---|---|---|---|---|
| **H0** hold, records only | As `D§7`, amended: hold (1) now holds `functions.rs`'s copy-per-use meaning and its recursion refusal, **not** its position before resolve, which §2 keeps. Holds (2)-(6) stand. | 0 | 0 | none |
| **H1** oracle and corpus (`crates/oracle`) | Reference instantiator (250), ground typer (250), Algorithm W with kinds (350), small-step evaluator (350), type-directed generator that emits derivations (400), `has_type` and runner (200), a reader for ‡ programs no front end parses yet (300); the corpus files. | **2,100 ± 300** (`D`: 900; `R`: 1,800-2,500) | 0 | expected outputs wait on O1, O2, O4; ‡ programs on C3c |
| **H2a** ground layer in the spine | Sorts and instances before resolve (600, reshaping `[C1C]` `functions.rs`); mappings onto the existing `Transform` node with a value-expression arena (500); `typecheck.rs` split under Lock 13 with ALT, NOM, MAP for forms 1-7 (500 new, 1,634 moved); verifier laws and the exhaustive matches for the new base and host types (400, plus mutants in tests); re-pointing `lower.rs`, `replay.rs`, `verify.rs`, `mod.rs` and renaming the public stage (300); lay-out step, `Layout`, `LayoutSink`, `LayoutFacts` export (700); diagnostics (100). This wave types all 585 corpus mappings and uses no unifier on types. | **3,100 new + 1,634 moved** | 0 | O4, O1, O2, O3, O5 |
| **H2b** value HM on today's closures | Term arena and unifier with kinds (550), value inference for the forms of `expressions.bbnf` (350), prelude schemes and instantiation, CLOSE (250), BR3's script re-pointed at the unifier (`D§4.7` stands, with one wording change: root's `TypeDomain` is replaced at root's retirement, not cured). | **1,150-1,400** | 0 | O4; lowering of what it types waits on C5 |
| H2 total | | **4,250-4,500 new + 1,634 moved** (`D`: 2,650; `R`: 4,500-6,000 plus consumer edits) | 0 | |
| **H3** emitters read the facility (inside LW4.COMP-1) | Emitter takes every type from `Layout`; registry sidecar, `bbnf-path`, `bbnf-path-ts`, LSP hover and the xtask projection re-pointed; COMMIT and the transform counter; `regen --check` 9/9 re-baselined in one ruled step. | **1,500-2,500 beyond COMP-1's own budget** (`D`: +400). Not sized file by file. | **All 962 are decided here and nowhere earlier.** 725 in generated paths: regenerated away when the template stops emitting the name. Of the 237 others, by `git grep -c`: `crates/core/src/runtime` 47, `crates/core/src/backend` 37, `crates/ir/src/passes` 20, `crates/ir/src/registry` 18, `crates/core/src/path` 9, other root sources 4 (135 that retire with root); `crates/bbnf-path` 5 and `crates/bbnf-path-ts` 17 (22 to re-point); `crates/core/tests` 74 and `crates/ir/tests` 3 (77 to rewrite or retire); 3 in rows my listing cut. | C5, O1, O3 |
| **H4** the function surface | `match`, tuples, named functions in the grammar of record and front end (700); component-wise generalisation, sum patterns, exhaustiveness (600); L-TAIL lowering and body emission per C5 (500-1,100). | **1,800-2,400**, plus C5 (b)'s IR work (2,000-3,500, a guess) | 0 | C2, C3c, C4a, C4b, C5, O6 |
| **H5** Lock 4 extensions (LW5.COMP-4) | As `D§7`; plus the gate that enabling the CSP is byte-neutral on the corpus (S4). | 2,500 (`D`'s figure, not re-estimated) | 0 | C1 |

Totals: H1 + H2 is about **6,350-6,900 new lines plus 1,634 moved**, against `D`'s 3,550. The whole programme is about 12,000-17,000 depending on C5, against `D`'s 7,600.

What H2 proves about the nine generated grammars: nothing by generation, since the spine does not generate them and the MANIFEST digest is root-driven (`R` §4). H2a's evidence is the O1 difference report over the entries the spine accepts. Every generated file changes at H3.

---

## 11. What a build seat may start before the owner rules

Only work that no open choice in §8 can invalidate. It is a short list.

1. **The mapping census as a gate.** A test under `crates/oracle` (or an xtask check) that reads every `.bbnf` under `grammar/`, extracts each `->` right side and classifies it into the seven forms of §4, failing on an eighth form or on a changed count (585 today). It asserts no semantics, so O2 cannot invalidate it; it keeps §4's coverage true while the corpus moves.
2. **The reference unifier and its lattice laws**, in `crates/oracle`: a term arena, union-find, occurs check, the three-point kind chain, and BR3's four laws (commutative, associative, idempotent, antisymmetric up to renaming) over generated term pairs. Every option in O4, C3c and C5 still types value expressions by unification, and the owner's sentence names Hindley-Milner, so no ruling removes its consumer (H1's reference checker). It goes in the oracle, not the spine, so it is not substrate without a consumer.
3. **The count gates of record**, as a script with committed output: `StructLayout` 962 / 78, 237 / 69 outside generated paths, 0 under `crates/core/src/compiler/`; `LayoutFacts`, `LayoutSink` 0 in `crates/`. They are the baseline O3 is ruled against.
4. **Records:** H0's amended holds on lane `lw3/comp-1c`; LEDGER rows for this file.

Not on the list, and why: the corpus's expected outputs (O1, O2, O4 change them); splitting `typecheck.rs` (wasted if O4 is answered (b)); the instantiator (O4, O5); anything in the spine that touches a mapping (O2); the evaluator (C2, C3c); `Layout` and `LayoutSink` (O3).

---

## 12. What I did not verify, and what I could not resolve

Not verified:
- Nothing was compiled or run. Every walk in §1.5, §3, §5 and §6.2 is by hand.
- PT-2 rests on the principal-type theorem for HM with order-sorted variables, cited from memory (Nipkow and Prehofer); I did not re-open the paper. L-REF and L-PRINCIPAL test it; they do not prove it.
- Taken from `D` or `R` without re-opening the source: every `SK/LEDGER.md`, `SK/PLAN.md` and `RULINGS.md` line other than `RULINGS.md:386-392`; all TypeScript-package and root-backend citations; "the spine accepts 31 of 70 MANIFEST entries"; "21 of 70 entries change under the lawful join"; `emit.rs:271-361`; root's reading of a multi-parameter closure; the corpus counts for `@ws`, `@recover`, `@debug`, `@host`.
- Whether `crates/core/src/runtime` (47 `StructLayout` hits) is hand-written or generated without the word "generated" in its path. If generated, those 47 move from "retire with root" to "regenerate".
- Whether literals, operators and primitive calls fit existing BIR variants under C5 (b). I did not read the BIR code; the C5 (b) size is a guess.
- H3's size. No file was sized.
- Whether the spine's `%{ … }` record form survives COMP-1c. The Record row in §1.3 describes the node kind as it is today.

Could not resolve:
- **The type of a Pratt rule as the spine builds it today** (§5 M9). The rule for a mined Pratt node is given; the instance-declared node's type (its atom's) is not the type of what it parses, and fixing it needs a decision between Lock 10's mined form and COMP1C-2's declared form that is outside this design.
- **Soundness of the provenance early stop** for chains through other templates and through template-valued parameters (§5 M8). Proved for a direct self-call only; a property test is specified for the rest. If the test finds a finite program it refuses, the early stop is restricted to direct self-calls and the `instances` budget carries the rest.
- **What CONV does on unconvertible text** (O2). Root's behaviour not found.
- **TS.** Under instance typing and O1 (a) the TS package diverges on every uncollapsed sum and has no parametric rules or mappings with effect. Whether Lock 5's deferral or LG-b's parity bar governs is the owner's; this file designs nothing for TS.
- **R-TC in the grammar of record as it stands** (§6.1): it cannot be made true without C3c. That is a finding, not an open defect of the design.
