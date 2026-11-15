# LAYOUT-HM: the Hindley-Milner subroutine of layout lowering, and the functional layer

Design and audit seat, 2026-11-15. Read-only on code. Answers RULINGS 2026-11-15b OWN-HM (`RULINGS.md:386`) under the sixteen locks (`restart/locks/LOCKS.md:71-661`).

Paths are relative to the repository root at `sk-v27-lw3` `eaae0cc7b`. `[C1C]` marks lane `lw3/comp-1c` at `a2dd184ef`. `SK/` is `restart/skinny/tranches/sk-v27/`. `ARCH` is `restart/ARCHITECTURE.md`; `PASS-1` is `restart/audit/pass-1-substrate/PASS-1.md`. Nothing was built or run; every claim about behaviour is from reading, and says so where that matters.

The owner's order, verbatim: "Ensure our type solving using hindley miller is perfected with full inference--ensure we have a functional and turing complete language, too, with robust types in a generalized facility." Also: `@inline` and `@no_collapse` are removed. And, 15 Nov: "We have many requirements, LOCKS, which should be pursuant to the above. Read that and ensure no duplication."

---

## 0. De-duplication: what the sentence asks, what a lock already requires, what is new

| Asked | Lock clause that already requires it | In code today | New? |
|---|---|---|---|
| Hindley-Milner type solving | Lock 2: "HM/CSP type checking is a subroutine of layout lowering, never a public peer pass" (`LOCKS.md:160`). Lock 4: per-domain solvers composed by output-piping (`LOCKS.md:179`). ARCH §8.2: "rank-1 Hindley-Milner principal-scheme core … Fresh type variables, scheme instantiation, generalization, and first-order unification" (`ARCH:1807-1819`). SK plan row A-26, LW5.COMP-4: "HM and bidirectional inference" pending owner ask O-5 (`SK/PLAN.md:363`, `SK/AMENDMENTS-FROM-THE-DIG.md:204`). | **None.** No unifier, substitution, scheme, generalization or instantiation in any tree (§1.2). `skinny/crates/passes/src/lib.rs:134-147` declares `Substitution` and `TypeVarId`; `Type::Var` has 0 uses. | **No.** Cite the locks; add a gate (§7). The sentence answers O-5: full HM. |
| "perfected … with full inference" | Not required by any lock. Lock 4 goes the other way: DK13 higher-rank and GADT local equalities "with explicit annotation rules for non-principal programs" (`ARCH:1832-1835`); the locked productions make every parameter annotation mandatory (`Param ::= Ident ":" Type`, `ARCH:1730`). | Grammar of record already writes unannotated parameters (`grammar/bbnf/bbnf.bbnf:51`, `grammar/bbnf/expressions.bbnf:33`). | **Yes: NEW.** Requirement R-INF (§4.6). Pulls against Lock 4: owner choice C1. |
| a functional language | Lock 10: "function values + lambda literals (`\|x\| body`) + closure capture by `&'i`; function types `fn(T) -> U` are first-class members of the `Type` non-terminal" (`LOCKS.md:269`). PASS-1 §6: `LambdaExpr`, `MatchExpr`, `TupleExpr`, `FnType` (`PASS-1:242-256`). | Functions are not values in any tree. Root: compile-time beta-reduction on CST, "never reach the IR" (`crates/core/src/lower/mod.rs:38-42`). `[C1C]`: macro expansion before resolve (`crates/core/src/compiler/functions.rs:33-86`). No `match`, no `fn` type in the grammar of record. | **No.** Cite Lock 10; gate in §7. |
| Turing complete | Not required. ARCH §8.4 says the opposite: closures exist "without turning BBNF into a general programming language" (`ARCH:1900-1902`). | Not Turing complete at any layer (§1.3). | **Yes: NEW.** Requirement R-TC (§5.3). Contradicts ARCH §8.4: owner choice C2. |
| robust types | ARCH §8.2 type rules (`ARCH:1815-1827`); Lock 2's `LayoutFacts` as the one public side table (`LOCKS.md:160`). SK: LW3.COMP-6 lattice laws and stage verifier (`SK/LW3-ORDER.md:115`). | Spine: sound but first-order synthesis with certificates (§1.1). Root: `TypeDomain::join` is not a lattice join; gate BR3 RED, 54,531 of 100,000 cases (`SK/LEDGER.md:468`). | **No** as a requirement. The testable content of "robust" is §6; BR3's cure is this design (§4.7). |
| one generalized facility | Lock 5: "Codegen emits a backend-agnostic typed IR; per-backend lowerers produce native source … The IR is the contract" (`LOCKS.md:181`). Lock 2: `LayoutFacts` public, `TypeFacts` internal (`ARCH:1073-1076`, `ARCH:1092`). | Violated: three type derivations (spine, root, skinny), plus ≥ 8 backend sites that re-derive (§1.1). `LayoutFacts` is grep-zero in `crates/`. | **No.** No second typed core is designed here. §4.8 says how HM results reach `LayoutFacts` and the Lock 5 IR. |

So exactly two new requirements enter: **R-INF** (full inference as a stated, tested property) and **R-TC** (Turing completeness, located in one layer). Everything else is an existing lock that has no implementation and no gate; this file gives each a gate, not a restatement.

### 0.1 Re-measured lock state (2026-11-15, `git grep` counts at `eaae0cc7b`)

| Lock term | Count | Reading |
|---|---|---|
| `StructLayout` in `crates/` | 962 hits, 78 files (237 hits outside generated paths) | SK-V17 addendum said 960 (`LOCKS.md:616`). Lock-2-retired name, live. |
| `LayoutFacts` or `backend_shape` in `crates/` | 0 | Unchanged from the addendum. |
| `LayoutSink` in `crates/` and `skinny/crates/` | 0 and 0 | Lock 2's trait exists nowhere. |
| `LayoutFacts` in `skinny/crates/` | 12 hits; defined `skinny/crates/passes/src/lib.rs:91` | The only tree with the Lock 2 shape. |
| `TypeDesc` / `TypeMap` / `TypeSolution` in `crates/` | 3,540 / 58 / 25 | Retired terms, live. `TypeSolution` is a public spine stage (`crates/core/src/compiler/mod.rs:41-46`, `:118`; `SK/PLAN.md:72`). |
| DK13, higher-rank, `forall` | 0 in `crates/`, `skinny/crates/`, `typescript/src` | Lock 4's higher-rank: no code. |
| OutsideIn, implication constraints for types, `LOCAL-EQUALITY` | 0 (the 19 `Implication` hits are `csp_strategy`'s dispatch constraint, `crates/ir/src/passes/csp_strategy/mod.rs:33`) | Lock 4's GADT surface: no code, no syntax in the grammar of record. |
| non-generated `.rs` files over 500 lines under `crates/` | 60 (tests included) | Lock 13. The spine has 8, among them `typecheck.rs` 1,634 and `analysis.rs` 1,736. |

### 0.2 Rulings of 2026-11-14 and 2026-11-15 that contradict a lock

| Ruling | Says | Lock it contradicts |
|---|---|---|
| 14i COMP1C-1 (`RULINGS.md:343`) | "The grammar of record (`grammar/bbnf/bbnf.bbnf` plus `docs/bbnf/`) is the only surface"; the front end must **accept** `@ws`, `@recover`, bodyless `@host`. | Lock 10: `@ws` folds into `@layout(ws = ...)`, standalone `@recover` retires, six directives only (`LOCKS.md:269`); the `directive-canon` lint rejects both (`ARCH:1778-1781`); "Bodyless host declarations have no production" (`ARCH:1762`). |
| 14i/14j COMP1C-2 (`RULINGS.md:344`, `:357`) | The instance is derived from "the `@ws` pattern"; "Pratt tables from the grammar's own declarations". | Lock 10: `@ws` retired. If "declarations" means anything an author writes to mark Pratt, Lock 10: "Any plan that requires grammar authors to annotate Pratt … is a fault". If it means the mined rule shape, no conflict; the ruling does not say which. |
| 14k COMP1C-5 (`RULINGS.md:368`) | `@host` (bodyless form, `bbnf.bbnf:73`) is added to the docs' directive table; the table keeps `@debug`, `@ws`, `@recover`. | Lock 10: `@host fn` is block-bodied; "`@debug` is a host primitive, not a directive". |
| 15a ORDER-1115a (`RULINGS.md:384`) | "meaning for `->` and `@{ }` through types, facts, lowering, events and the verifier as new spine node kinds". | Lock 2: types are not a public stage. Governance Boundary: a new plan op is the spine's analogue of a BIR variant (`LOCKS.md:652-661`); see C5. Already held by 15b. |
| 14e/14f PERF1D-B, -B2 (`RULINGS.md:307`, `:315`) | Kernels relocated into `parse-that-simd`. | Lock 16 SK-V18 clause names `bbnf-simd` as the one scan substrate (`LOCKS.md:641`). Outside this file's subject; listed because it was asked. |
| 15b OWN-HM (`RULINGS.md:386`), the owner | "functional and turing complete". | Not a lock, but ARCH §8.4 (`ARCH:1900-1902`). Owner choice C2. |
| 15b OWN-INLINE | `@inline`, `@no_collapse` removed. | **Agrees** with Lock 10 (neither is among the six). |

Not contradicted: COMP1C-3 (progress law; ARCH's Grammar IR says "Nullable body is rejected or guarded", `ARCH:906`), COMP1C-2E, COMP1C-6, COMP1C-7, the S3CPP rows, RXEF rows, GENREC-Q. Older rulings were not audited here; two that touch types are noted in §3.

### 0.3 Grammar of record, docs and Lock 10: every gap, both directions

**In `grammar/bbnf/bbnf.bbnf` (and its imports) but not in Lock 10 / PASS-1 §6:**

| Form | Where | Lock 10 status |
|---|---|---|
| `@ws /re/` | `bbnf.bbnf:70`; 2 corpus files | retired into `@layout(ws = ...)` |
| `@recover rule sync` | `bbnf.bbnf:65`; 1 corpus file, 3 lines | retired into `@error(recover = ...)` |
| `@debug` | `bbnf.bbnf:72`; 0 corpus uses | "a host primitive, not a directive" |
| bodyless `@host name : type` | `bbnf.bbnf:73`; 0 corpus uses | only block-bodied `@host fn` |
| `@import "path"` without a name list | `bbnf.bbnf:58-62` | only `@import { … } from "…"` (`ARCH:1716`) |
| `@pretty *` and an open hint identifier | `bbnf.bbnf:67-68`; `[C1C]` reads `blankline`, `softbreak`, `nobreak`, `fast`, `off` | closed vocabulary `compact`, `group`, `indent`, `hardbreak`, `sep(...)`, `block` |
| `?w`, `<<`, `>>`, `-`, `[ ]`, `{ }`, `ε`, `@{ }`, `.` terminator, optional terminator | `bbnf.bbnf:29-46`, `:56` | no production; `{ }` collides with the locked `{n,m}` quantifier (`ARCH:1740`) |
| per-factor `-> value_expr : Type` | `bbnf.bbnf:42`; `expressions.bbnf:6-37` | only a rule-level `MapTail = "->" Ident { "->" Ident }` (`PASS-1:252-253`) |
| rule-level `\|p, q\| rhs`, call `f(a, b)` | `bbnf.bbnf:51-52`, `:27-30` | `LambdaExpr` at `Primary`, `RuleParams`, `GenericParams` |
| `type_name` as one identifier | `grammar/bbnf/types.bbnf:3-7` | `Type = Ident GenericArgs? \| TupleType \| RecordType \| BorrowType \| FnType` |

**In Lock 10 / PASS-1 §6 but not in the grammar of record:** `@layout`, `@error`, block-bodied `@host fn` with generics, the `rule` keyword, `GenericParams`, `RuleParams`, `ReturnType`, lookbehind `|<` `|<!`, lookahead `&` `!`, `{n,m}`, `HostCall = "@" Ident "(" … ")"`, `LambdaExpr` at `Primary`, `MatchExpr`, `TupleExpr`, the `->` chain, `FnType` and the structured `Type`. `Block`, `HostAttrs`, `LayoutBody`, `ErrorBody`, `TupleType`, `RecordType`, `BorrowType` are used by the locked productions and **defined nowhere** (`grep` over ARCH, PASS-1, README: no `Block =` or `Block ::=`). Lock 4 cites the arm form `Pattern @ where T = U -> Block`; PASS-1 writes `Arm = Pattern Refinement? "=>" Expr` (`PASS-1:244`); ARCH §8.1's sketch has no `MatchExpr` at all (`ARCH:1741`). Lock 4 says implication constraints are solved at `passes/types/`; Lock 2 says there is no public type pass. Read here as `passes::layout::types`, the path skinny uses (`skinny/crates/passes/src/lib.rs:123`).

**In `docs/bbnf/` but in neither:** `@no_collapse`, `@inline` (`docs/bbnf/grammar-syntax.md:107`, `:109`; removed on `[C1C]`); the legacy `=>` operator (`grammar-syntax.md:142`; 1 corpus file, 3 lines; no production in `bbnf.bbnf`); the closure example `|s: Span| -> f64 { … }` (`grammar-syntax.md:131`), which `expressions.bbnf:33` cannot parse. The docs say nothing about grammar functions. `docs/bbnf/lsp.md:146` claims LSP support for `@inline`; none exists.

The grammar of record and the locked surface are two different languages that share a core. COMP1C-1 chose the first; Lock 10 is the second. That is owner choice C3.

---

## 1. Audit: what is true today

### 1.1 Where types are computed

| Site | What it is | Evidence |
|---|---|---|
| **Spine** `crates/core/src/compiler/typecheck.rs` (1,634 lines) | Bottom-up, syntax-directed synthesis. One `TypeConstraint` per node read off its form (`:1144-1201`); `derive_node_type` walks containment depth-first and interns structurally (`:1204-1411`, `:1476-1488`). Type language `SemanticTypeData`: `Unit`, `Span`, `Named(RuleIx)`, `Product`, `Sum`, `Option`, `List`, `Record{owner, fields}` (`:17-29`). Transform, projection, difference, trim and Pratt pass a child's type through (`:1313-1404`). | No type variable, no unification, no arrow type. |
| Spine, recursion | Iso-recursive and nominal: a rule reference is `Named(rule)` and never unfolds (`:1235-1240`). The "occurs-check" is a cycle check on node containment, not on type terms (`:1217-1226`); its test builds the cycle by hand (`:1597-1621`). | |
| Spine, certificates | Every node carries an `InferenceRuleData` witness (`:194-235`); `TypeSolution::verify` re-derives each conclusion from its premises without re-running the solver (`:783-864`, `:913-1052`). Order-independence is tested over 16 permutations (`:1568-1594`). | This is the part worth keeping. |
| Spine, stage verifier (LW3.COMP-6) | `crates/core/src/compiler/verify.rs`: 42 laws in stage order; type laws are `RowPerProgramRow`, `NamedBinder`, `RootConclusion`, `ArenaCanonical`, `TypeInRange` (`verify.rs:74-117`). Every view enum is matched exhaustively, so a new type constructor does not compile until the verifier reads it (`verify.rs:19-23`). 61 of 61 mutants rejected (`SK/LEDGER.md:470`). | |
| Spine, what it emits | An event-stream parser (`ScalarEvent` with rule, node, field, transform ids; `crates/core/src/compiler/emit.rs:271-361`). Types travel in the plan (`lower.rs:463`, `:682`) and are not yet turned into Rust types. | |
| **Root** `crates/ir/src/passes/types/` (~3,350 lines) | A CSP solved by AC-3: one variable per IR node and rule, domain `TypeDomain = Option<TypeDesc>` (`constraint/domain.rs:13-15`). `join` overwrites a decided type with a different one (`domain.rs:66-79`); `revise::assign` does the same (`constraint/revise.rs:27-35`). | Variables hold ground types only; nothing unifies terms. |
| Root, recursion | A rule still unsolved after propagation is set to `BoxedEnum` and propagation runs again (`passes/types/mod.rs:69-131`). `BoxedEnum` is both a placeholder and a real type. | |
| Root, the type language | `TypeDesc`: `Span`, `F64`, `Bool`, `I8`…`U64`, `Option`, `Vec`, `Tuple`, `BoxedEnum`, `Enum`, `Named(StringId)`, `HeterogeneousAltJoin(Vec)` (`crates/ir/src/types/type_desc.rs:13-63`). It mixes semantic type and layout: sequences are Span-compressed and `(T, Vec<T>)` is flattened (`revise.rs:46-…`); an alternation whose branches are equal takes that type, otherwise `HeterogeneousAltJoin` (`revise.rs:116-141`). | |
| **BR3** | `TypeDomain::join` is not commutative; 54,531 of 100,000 cases break the law; the lawful cure changes 21 of 70 MANIFEST outputs (`SK/LEDGER.md:463-468`). The recorded refinement order: 672 overwrites, 39 distinct pairs, **every old type is `BoxedEnum`**, 0 antisymmetry violations, 0 cycles (`SK/evidence/LW3.COMP/comp6/br3/refinement-analysis.txt:1-3`). Carried PARTIAL to LW5.COMP-1 "TypeDomain on its real refinement order" (`RULINGS.md:241`). | See §4.7. |
| **Root generator** | Emits no per-rule Rust type itself (`crates/core/src/backend/rust/emitter/grammar.rs:96-110`); value types are generated by xtask from a TOML projection plus the registry sidecar (`xtask/src/regen_simple_runtime.rs:303-306`, `xtask/src/regen.rs:699-722`). | |
| Root generator, re-derivation | Sites that derive types beside the pass: `resolve_branch_type` (`backend/rust/emitter/mod.rs:53-80`); `collect_alt_payload_types` (`:101-112`); `RustNamedTypes::infer_named_shape` (`backend/rust/view/named_types.rs:152-205`); `decide_seq` with `Span` fallbacks (`backend/types/decisions.rs:56-114`); `PLACEHOLDER_TY = Span` (`backend/driver/seq.rs:32`, `repeat.rs:30`, `alt.rs:114`); synthesised `StructLayout { rule_type: Span, fields: [] }` in 8 shape emitters (e.g. `shapes/arglist.rs:209`); `lower_map_arrow` (`crates/core/src/lower/expression/wrap.rs:545-644`). | Each is a Lock 5 violation: a backend deciding a type. |
| **Root TS emitter** | `type_desc_to_ts` (`crates/core/src/backend/ts/projection.rs:11-46`) reads `ir.types`; emits `{tag; value}` unions and casts through `as unknown as` (`backend/ts/emitter/grammar.rs:89-127`). | |
| **TS package** | No result-type inference. The `.d.ts` types an entry as its action's return type, else `unknown` (`typescript/src/emit.ts:596-636`). Value shapes are prose (`typescript/VALUE-SEMANTICS.md:17-31`): sequence is a positional array, alternation is the first success's value **untagged**. | |
| **gorgeous** | Consumes no types: generated `*_prettify()` parsers, or an untyped `Value` (Nil / Span / Tagged) in the VM (`crates/gorgeous/src/vm.rs:7-8`, `:42-72`). | |
| **Other consumers** | LSP hover prints `ir.types` (`crates/analysis/src/state/diagnostics/ir_analysis.rs:105-134`); `bbnf-path` and `bbnf-path-ts` walk `StructRegistry` and `TypeDesc` (`crates/bbnf-path/src/registry.rs:58-82`, `:188-196`). | |
| **skinny** | The Lock 2 shape and nothing behind it: `layout::run` copies an internal `types::TypeFacts` into `LayoutFacts.rule_types` / `node_types` (`skinny/crates/passes/src/lib.rs:91-121`); `infer_expr` is bottom-up synthesis over 10 node kinds (`:175-209`). `LayoutFacts` uses `HashMap` (`:92-99`). | |

### 1.2 Is there Hindley-Milner anywhere? No.

| HM ingredient | Spine | Root | skinny | TS |
|---|---|---|---|---|
| type variables | none | CSP variables over ground types | declared, 0 uses | none |
| unification of terms | none | none (`join` assigns) | none | none |
| substitution | none | none | struct declared, never written | none |
| generalization, schemes, instantiation | none | none | none | none |
| occurs check on types | a containment cycle check | none | none | none |
| function type | none | none | none | none |
| constraint solver | none | AC-3 over a non-lattice | none | none |

`grep -E 'unify|Unif'` over `crates/**/*.rs` finds two e-graph test strings and nothing else.

### 1.3 The functional layer today

| Form | Parsed | Resolved and typed | Lowered | First-class? Recursion? |
|---|---|---|---|---|
| grammar function `r = \|p, q\| rhs`, call `f(a, b)` | `bbnf.bbnf:27-30`, `:51-52`. `[C1C]` reads them to `Call` / `Closure` source nodes (`source.rs:115-124`). **TS: not in its grammar at all** (`typescript/src/grammar.ts:270-282`). | **Root:** closures are split out before validation, dependency analysis and SCC, and "never reach the IR" (`crates/core/src/pipeline/compile/pipeline.rs:53-74`; `lower/mod.rs:38-42`); a call is beta-reduced on CST views with each argument re-lowered at each use in the callee's environment stack (`lower/expression/closures.rs:28-29`, `:48-90`). Arity mismatch is an `assert_eq!` panic (`closures.rs:60-67`). **`[C1C]`:** `functions::expand` copies the body per call before resolve and deletes the function rules (`functions.rs:33-86`). | Disappears. | Not values. Root has **no recursion check**: no visited set, no depth counter (`closures.rs:48-77`), so by reading a recursive function overflows the host stack (not run). `[C1C]` refuses recursion as `GrammarFunction` (`functions.rs:254-265`). |
| `e -> value : Type` | `bbnf.bbnf:42`; `expressions.bbnf`: literals, paths, `input`, calls, unary and binary operators, `\|a, b\| expr` closures. No conditional, no binding form, no recursion. | **Root:** the type is taken, in order, from the `: T` annotation, a bare type name, a numeric suffix, a bool literal, an `@host` return type, else nothing (`lower/expression/wrap.rs:545-644`). Nothing checks the expression against its input. **`[C1C]`:** refused at resolve as `FormWithoutMeaning` (`syntax.rs:648-668`). **TS:** kept as source text, never evaluated (`typescript/src/types.ts:31-37`). | Root: `FnDescriptor::Expr { MapExpr, return_type }` (`crates/ir/src/types/fn_descriptor.rs:54-57`); `MapExpr` has no closure node (`map_expr.rs:17-40`). A value closure's first parameter becomes `Input`, the rest `InputProp`; the closure does not survive (`lower/value_expr/simple_kinds.rs:201-219`). | No. |
| `@{ e }` | `bbnf.bbnf:33`; 3 corpus files | Root: `Map + FnDescriptor::SpanCapture`. `[C1C]`: refused. TS: parsed, **no runtime effect** (`typescript/src/emit.ts:369`). | | n/a |
| `@host name : T` | `bbnf.bbnf:73`; 0 corpus uses | Root: supplies a return type to `lower_map_arrow` only (`wrap.rs:624-632`). | TS emits `declare function f(__input: any): any`. | n/a |

Corpus census (33 `.bbnf` files under `grammar/`): **0** grammar-function definitions; `->` in 18 files, of which all but about 20 lines are constants (`0u8` … `18u8`, hex `u32`, `true`/`false`), bare type names (`Span`, `f64`, `i64`) or `input : T`; 3 host calls (`decode_json_string_to_arena(input…)` and two more). The functional layer has no corpus weight: its semantics can be set without moving a MANIFEST line.

**What can be expressed today:** non-recursive, first-order macros over grammar expressions; constants; calls to host functions whose types nobody checks. **What cannot:** a function as a value, a function returning a function, any recursion in a function, a conditional, a data-dependent computation. No layer is Turing complete.

### 1.4 What "Turing complete" can mean here, and where it must not leak

Three layers, three answers.

| Layer | What general recursion would do | Laws it would break |
|---|---|---|
| **(i) Recognition**: rules, references, repetition. Decides what is accepted. | Rules are already mutually recursive (`docs/bbnf/grammar-syntax.md:75`). Recognition stays terminating because of two facts-stage laws: a repetition over a nullable body is refused (`NullableRepeat`, `crates/core/src/compiler/analysis.rs:548-567`) and a call cycle that can re-enter without consuming is refused (`RecursiveRule`, `analysis.rs:625-670`). Letting a computed value decide what a rule matches (a semantic predicate over an arbitrary function) makes recognition undecidable. | Progress law (COMP1C-3, `RULINGS.md:345`); G1 / LG-3.1 (`SK/LW3-ORDER.md:224`): the reference interpreter and the independent oracles could no longer agree by construction; FIRST-set dispatch; every `obs()` class. |
| **(ii) Compile-time functions over grammars**: parametric rules. Run inside the generator. | Unbounded unfolding: the generator does not halt, or halts at a host-dependent stack depth. | Compile-time termination; `regen --check` 9/9 and G6 byte-determinism (`SK/designs/compiler.md:150`); the MANIFEST digest (one process per entry, `SK/LEDGER.md:465`); IF-2 "Budget exhaustion returns … an `Exhausted` certificate. Silent fallback is forbidden" (`SK/designs/programme.md:233-236`); QX-1 clause 8 is the same disease in root's driver (`RULINGS.md:383`). |
| **(iii) Transforms over parse results**: the right side of `->`, value functions. Run at parse time, after or beside recognition. | The transform may not return. Recognition is untouched **provided** "a mapping never changes what the factor matches" (`docs/bbnf/grammar-syntax.md:140`). | Generated-parser termination, which the spine already states as a budget, not as a proof: `RuntimeBudget { input_bytes, nesting, logical_events, work }` (`crates/core/src/compiler/budget.rs:50-54`) and a `charge_work` / `resource_fault` path in emitted code (`emit.rs:447-465`). Nothing else. |

Conclusion: Turing completeness belongs in layer (iii) and only there. Layer (ii) may be recursive but must stay total by construction or by a deterministic budget. Layer (i) keeps every law it has. I7 (`SK/designs/compiler.md:126`) is untouched by all three: no part of this design reads a rule or grammar name.

---

## 2. What the owner's order touches: ids

| Id | Where | How it is touched |
|---|---|---|
| Lock 2, Lock 4, Lock 5, Lock 10, Lock 13, Lock 14, Lock 6, Governance Boundary | `LOCKS.md:160`, `:179`, `:181`, `:269`, `:336`, `:349`, `:183`, `:652` | Bind the design (§0, §4.8, §7). |
| ARCH §8.4 | `ARCH:1900-1902` | Contradicted by "Turing complete" (C2). |
| ARCH §8.2 "internal `TypeFacts`", §7.3 | `ARCH:1819`, `:1073-1092` | Followed. |
| PASS-1 `BBNF-GENERIC-CYCLE` | `PASS-1:93`, `:128` | Reused as the layer-(ii) budget diagnostic (§5.2). |
| RULINGS 15b OWN-HM, OWN-INLINE | `RULINGS.md:386-…` | The order itself. |
| RULINGS 14i COMP1C-1 | `RULINGS.md:343` | "where a later stage cannot yet give one a meaning, that stage refuses with a named diagnostic": kept. Its acceptance of `@ws`, `@recover`, bodyless `@host` contradicts Lock 10 (C3). |
| RULINGS 14i COMP1C-2, 14j, 14k COMP1C-2E | `RULINGS.md:344`, `:357`, `:367` | Entry = last rule: untouched. `@ws` derivation: C3. |
| RULINGS 14i COMP1C-3 | `RULINGS.md:345` | Progress law kept verbatim; now also read on instances of parametric rules (corpus N11, N12). |
| RULINGS 14k COMP1C-5 | `RULINGS.md:368` | Closed by OWN-INLINE. |
| RULINGS 15a ORDER-1115a | `RULINGS.md:384` | Its "new spine node kinds" for `->` and `@{ }` are held; §7 says what replaces them. |
| RULINGS 13a COMP6-Q, 13b COMP6-T; gate LW3.COMP-6-BR3 | `RULINGS.md:232`, `:241`; `SK/LEDGER.md:468` | BR3's cure of record, option (a), is delivered by §4.7, earlier than LW5.COMP-1. |
| LW3.COMP-6 lattice laws, stage verifier | `SK/LW3-ORDER.md:115`; `verify.rs` | The verifier gains a certificate check (§4.9). |
| LW5.COMP-1 | `SK/PLAN.md:226` | Loses "TypeDomain on its real refinement order" (moves to §7 stage H2). Its scope text (e-graph, CSP, cost model) is unchanged. |
| LW5.COMP-4, A-11, A-26, DIG-CSP-B7, DIG-C9, O-5 | `SK/PLAN.md:320`, `:363`; `SK/AMENDMENTS-FROM-THE-DIG.md:56`, `:166`, `:204` | O-5 is answered: full HM. "inference on the CSP substrate" is corrected: unification is not AC-3; the CSP takes HM's output and solves the finite choices, which is what Lock 4 and `ARCH:1822` say. |
| DIG-CSP-B4 "Types … on one `propagate()`" | `SK/AMENDMENTS-FROM-THE-DIG.md:52` | Contradicts Lock 4's output-piping for the type part. Types leave that list; FIRST, FOLLOW, nullable and dispatch stay. |
| LW3.COMP-1c | `SK/LW3-ORDER.md:104`; `SK/LEDGER.md:518-530` | E7 ("`NodeKind` has no mapping, span … carrier") and step 1 are re-read in §7 H0. |
| LW3.COMP-3b reference interpreter "over Program + TypeSolution" | `SK/LW3-ORDER.md:110` | Reads the layout carrier under its Lock 2 name. |
| LW4.COMP-1, LW4.COMP-4, LW4.COMP-6 | `SK/PLAN.md:204`, `:323`, `:316` | Emitters read `LayoutFacts` only (§7 H3). |
| I7, I8, LG-3.5 | `SK/designs/compiler.md:126-127`; `SK/LW3-ORDER.md:228` | Kept. The reference checker of §6 is an I8 oracle. |
| QX-1 clauses 1-8, X-1 | `RULINGS.md:208`, `:233`, `:250`, `:355`, `:383` | No root edit is proposed. Root's type pass is retired with root at LW4.COMP-1 (`SK/LEDGER.md:268`), not cured. |
| `regen --check` 9/9, MANIFEST digest (70 entries) | `SK/LW3-ORDER.md:226`; `SK/LEDGER.md:465`, `:519` | Gate G-DIGEST (§7). |
| LG-b, R-b TS parity | `SK/PLAN.md:86`; `RULINGS.md:28` | TS has no grammar functions and no `@{ }` effect: two new DIVERGENCE rows, owner LW4.COMP-4. |
| `[C1C]` laws `functions::tests::*` (4), diagnostic `GrammarFunction` | `functions.rs:440-547`; `diagnostic.rs:29-33` | Withdrawn with the module (§7 H0); the four misuse cases become type errors N03-N05 and budget case N10. |
| Withdrawn diff `SemanticTypeData::Value(name)` | `~/.dev-logs/bbnf-lockstep/lw3/comp-1c/withdrawn/mapping-spancapture-meaning.diff` | Stays withdrawn: it types a mapping from its written `: Type` text, the opposite of inference. |

---

## 3. Design constraints taken from the locks

1. **No public type pass.** The solver is `layout::types`, crate-private; its output type is `TypeFacts`, crate-private; the public artefact is `LayoutFacts` (Lock 2).
2. **No second typed core.** Lock 5's Backend IR plus `LayoutFacts` is the generalized facility.
3. **Solvers are piped, not fused.** HM runs first and alone. The CSP then solves finite choices over HM's output. The e-graph never sees a type variable (Lock 4, Lock 6).
4. **No new directive, BIR variant, lock or BackendShape.** Where the design needs one, it is listed as an owner decision (§8) and the design states what it does without it.
5. **No file over 500 lines, no grammar name in generic code, no proc-macro** (Locks 13, 14, 6). §4.10 gives the module layout.

---

## 4. The type language and the inference algorithm

### 4.1 Types

```
τ ::= α                      type variable
    | Unit | Span | Bool | i8…u64 | f64 | Str        base types B
    | τ₁ × … × τₙ   (n ≥ 2)  product (a sequence)
    | Sum[τ₁ … τₙ]  (n ≥ 2)  ordered sum (an alternation)
    | Option τ | List τ
    | R[τ̄]                   the nominal type of rule R, with R's type parameters
    | fn(τ̄) → τ              function
    | P τ                     a parser that yields τ
σ ::= ∀ᾱ. τ                  scheme, prenex only
```

Records: closed and nominal only. A record is a layout decision over a product (`ShapeFacts`, `DirectFieldFacts`), not an HM type; row polymorphism "remains internal … defers to a later type-system research gate" (`ARCH:1858-1860`). Nothing here needs it, and leaving it out keeps unification first-order.

Spans: `Span` is a base type. `@{ e }` has type `P Span` for any `e : P τ`. `e -> Span` is the same thing written the old way (`type_desc.rs:74-77`).

### 4.2 Typing rules for grammar expressions

| Form | Type |
|---|---|
| literal, regex | `P Span` |
| `ε` | `P Unit` |
| `e₁ , … , eₙ` | `P (τ₁ × … × τₙ)` |
| `e₁ \| … \| eₙ` | rule ALT below |
| `e?`, `[e]` | `P (Option τ)` |
| `e*`, `e+`, `{e}` | `P (List τ)` |
| `e ?w` | `P τ` |
| `a << b`, `a - b` | `P τₐ` |
| `a >> b` | `P τ_b` |
| `@{ e }` | `P Span` |
| reference to a rule `r` | rule NOM below |
| call `f(a₁ … aₙ)` | `f : fn(τ₁ … τₙ) → ρ`, each `aᵢ : τᵢ`; the call has type `ρ` |
| `e -> v` | `e : P τ`; `v` is typed with `input : τ`; result `P ρ`. A bare path `-> f` means `f(input)`; a closure literal `-> \|x\| b` is applied to `input`; anything else is an expression over `input`. This is decided by syntax, as root does today (`lower/value_expr/atom.rs:554-560`, `simple_kinds.rs:201-219`), so it costs no principality. |
| `\|x₁ … xₙ\| body` | `fn(α₁ … αₙ) → ρ`, the `αᵢ` fresh monotypes |

**Rule NOM (when a rule is a name, when it is its body).**
- A rule with no parameters has the nominal type `R` and a reference to it has type `P R`, with `R` defined as its body type. Exception: a rule alone in an acyclic component whose solved body type is a base type is **transparent**: a reference has the body type. (`digit = /[0-9]/` is `P Span`, as root's scalar-propagating reference has it today.) Both conditions are known when the rule's component is solved, before any later component reads it (the dependency-first order already exists and is verified: laws `ComponentsStronglyConnected`, `DependencyFirstOrder`, `verify.rs:96-99`).
- A rule with parameters (a parametric rule, today's "grammar function") is transparent: a call has the instantiated body type. Exception: if it lies on a cycle of the call graph it is nominal, `R[ᾱ]` with one type parameter per generalized variable, because only a name can close a recursive type under an occurs check.

This is iso-recursive: `R` and its definition are different types; the fold is implicit at the rule boundary and the unfold happens only at a `match` scrutinee. Equi-recursive types are refused: they would turn the occurs check off and make `x(x)` typeable.

**Rule ALT (the one rule beyond textbook HM).** Each alternation node gets a fresh result variable δ and a deferred constraint `Alt(δ; τ₁ … τₙ)`. After the equality constraints of the binding group are solved: (1) repeat until nothing changes: every deferred `Alt` whose branch types are identical terms is resolved `δ := τ₁`; (2) every remaining `Alt` is resolved at once, `δ := Sum[τ₁ … τₙ]`; (3) the resulting equalities are solved; a clash is a type error. Step 1 is a monotone closure (unifying only makes more terms equal), so it is confluent, and step 2 is simultaneous, so the result does not depend on traversal order. Variables count as terms: `x | x` collapses, `x | y` does not, at any later instantiation.

Why not plain HM here: "all branches unify" rejects `value = object | array | string`; "always `Sum`" makes `"px" -> 0u8 | "em" -> 1u8` a `Sum[u8, u8]` where root, the registries and every host signature have `u8` (`revise.rs:116-123`; 400-odd constant-map lines in the corpus). ALT is root's first join rule, made order-independent. Root's second rule (`effective_payload_type`, `revise.rs:125-133`), Span compression and tuple flattening are **layout**, not typing, and move to the layout step (§4.8).

### 4.3 Typing rules for value expressions

Literals: an integer literal with a suffix has that type; without one, `i64`. A float literal: `f64`. `true`/`false`: `Bool`. A string literal: `Str`.

Operators: `&&`, `||`, `!` are over `Bool`. Comparison operators are `fn(τ, τ) → Bool` over base types. Arithmetic operators are overloaded over the numeric base types, and overloading has no principal types in HM. Rule NUM: an arithmetic operator's operands and result share one type variable ν flagged *numeric*; ν unifies only with a numeric base type or another numeric variable; a numeric variable still free when its binding group is generalized is set to `i64` and is **not** generalized. So `|n| n + 1` is `fn(i64) → i64`, not a scheme over number types. This is Standard ML's rule. It is the single place where the inferred type is not the most general one, and the reason is stated here rather than hidden: the alternative is type classes, which is a larger system than the order asks for.

`match` (locked surface, `PASS-1:243-249`): the scrutinee has type τ; each pattern is checked against τ and binds monotypes; all arm bodies have one type ρ; the `match` has type ρ. A scrutinee of nominal type `R` is unfolded once. Exhaustiveness is checked after solving (`BBNF-PATTERN-NONEXHAUSTIVE`, `PASS-1` §3).

Host primitives come from a prelude with declared schemes (`ARCH:1886-1890`); the ones the corpus and the proofs below use: `len : fn(Span) → i64`, `nil : ∀α. fn() → List α`, `cons : ∀α. fn(α, List α) → List α`, `uncons : ∀α. fn(List α) → Option (α × List α)`.

### 4.4 Binding groups, generalization, recursion

Top-level definitions (rules, parametric rules, value functions) are the only bindings; there is no local `let`. They are mutually recursive, so:

1. Build the dependency graph over all top-level names (references and calls). The spine already computes its condensation in dependency-first order (`crates/core/src/compiler/dependency.rs`).
2. For each strongly connected component, in that order: give each member a fresh monotype; infer every body; solve equalities; resolve ALT; apply NUM; **then** generalize each member over the variables not free in the environment. Inside its own component a definition is monomorphic (the Damas-Milner `letrec` rule). After it, every use instantiates the scheme freshly.
3. A rule with no parameters must end ground. A variable left over (for example the element type of `nil()`, or the result of a transform that never returns) is set to `Unit`. This is sound by parametricity: no value of an unconstrained type can be inspected. It is the second and last defaulting rule (DEFAULT).

Generalization is by levels (each variable carries the depth of the component that made it; generalize those deeper than the environment), which is the standard efficient form and is deterministic.

Value restriction: none is needed. The language has no mutable reference; closures capture by `&'i` only and "Mutating parse input" is forbidden (`ARCH:1916`). Law L-PURE (§6) pins that: if a mutable cell is ever added, generalization must be restricted to syntactic values in the same change.

Occurs check: on every variable binding in unification, always on. It is what rejects self-application (N01, N02) and, as a useful consequence, most runaway parametric recursion: `grow = |x| x | grow(x , "a")` needs `α = α × Span` (N09).

### 4.5 Stage discipline

`P τ` and functions over parsers exist at compile time only; a parser cannot be a parse result. Law STAGE: after solving, in every `P τ` the τ contains no `P`; a violation is the diagnostic `BBNF-STAGE` (N13). Today the two syntactic classes (`rhs` and `value_expr`) already keep the stages apart; the law is what keeps them apart if the locked merged surface (`LambdaExpr` at `Primary`) is adopted.

### 4.6 R-INF: which programs get principal types with zero annotations

**The fully inferred fragment F0** is everything in §4.2-§4.5: grammar expressions; rules; parametric rules of any order (functions over parsers, over functions over parsers); value functions, closures, closures returning closures; `match` without refinements; tuples; top-level recursion and mutual recursion; the prelude.

**Claim (to be tested, §6, not assumed).** Every F0 program either has a type error or receives, for every top-level name, a scheme that is the most general one derivable in the declarative system of §4.2-§4.4, with no annotation written anywhere. The two defaulting rules NUM and DEFAULT, and ALT's resolution order, are part of that declarative system; "principal" means principal in it. A reader who wants textbook principality with no asterisk must give up overloaded arithmetic (write `+.` for floats) and homogeneous-alternation collapse; I recommend neither.

**Where an annotation is needed, and why.** Each of these is outside rank-1 HM for a known theoretical reason, and each is already an annotated feature in the locks:

| Feature | Why inference cannot be complete | Lock source | Annotation |
|---|---|---|---|
| Polymorphic recursion: a parametric rule that calls itself at a different type | Typability is undecidable (Henglein 1993; Kfoury, Tiuryn, Urzyczyn 1993). | `PASS-1:93`: "recursive generic cycles require an explicit return annotation, or rejection with `BBNF-GENERIC-CYCLE`" | a signature on that rule |
| Higher-rank: a parameter used at two types inside one body | Rank-2 typability is decidable but has no principal types; rank 3 and above is undecidable (Kfoury and Wells 1994; Wells 1999). DK13 infers only monotypes for unannotated parameters and is complete only relative to annotations. | Lock 4, `ARCH:1832-1835` | a polymorphic type on that parameter. **The locked `Type` grammar has no quantifier**, so today this cannot even be written (`PASS-1:254-256`). ARCH's example `map<I, O>(f: fn(I) -> O, xs: [I])` (`ARCH:1789-1791`) is rank 1 and F0 infers it (corpus V14). |
| GADT branch-local equalities | No principal types without annotations; OutsideIn(X) is sound and complete only for programs annotated at the `match` (Vytiniotis, Peyton Jones, Schrijvers, Sulzmann 2011). | Lock 4; `BBNF-LOCAL-EQUALITY-ANNOTATION` (`PASS-1:129`) | `@ where T = U` on the arm |
| Host functions implemented outside BBNF | Nothing to infer from. This is a declaration, not a failure of inference. | Lock 10 `@host fn` | the signature |

**What breaks R-INF in the locked surface and should be loosened** (owner choice C4, because it edits locked productions): `Param ::= Ident ":" Type` makes every parameter annotation mandatory (`ARCH:1730`); `ReturnType` is mandatory on `@host fn` even when its block is BBNF code; `GenericParams` must be declared "since the parser must know the rule is generic" (`PASS-1:93`), which HM generalization makes unnecessary. Under R-INF all three become optional; a written annotation is checked against the inferred type, never trusted in its place (`ARCH:1820` already says "annotations check inferred types rather than bypassing them").

### 4.7 BR3: "TypeDomain on its real refinement order" is first-order unification

The recorded order is "provisional seed ⊑ any structural type", height 1, no cycle, the seed always `BoxedEnum` (`SK/LEDGER.md:468`). That is the order of a type variable under substitution: a variable is below every term. First-order terms modulo renaming form a lattice under instantiation (Plotkin 1970, Reynolds 1970); unification computes the least common instance, failure is ⊤. On that structure `join = unify` is commutative, associative and idempotent up to renaming, and propagation is monotone. So option (a) of COMP6-T, "the least upper bound of two types in that order … with ⊤ only for types the order cannot reconcile" (`SK/LEDGER.md:466`), is delivered by replacing `Option<TypeDesc>` with a term that may contain variables and replacing `assign` with `unify`. The placeholder `BoxedEnum` becomes what it always was, a variable; here it is not even needed, because rule NOM gives a recursive rule its name before its body is read.

BR3's own script (`SK/evidence/LW3.COMP/comp6/br3/br3.sh`) is re-pointed at the unifier: commutativity, associativity, idempotence and antisymmetry up to renaming over 10⁵ generated term pairs. RED today (there is no unifier), GREEN at H2. Root's `TypeDomain` is not cured in place: root retires at LW4.COMP-1, and a root edit would need a QX-1 clause it does not have.

### 4.8 The generalized facility: how HM reaches `LayoutFacts` and the Lock 5 IR

```
Program ──► layout lowering (public stage, Lock 2)
              1. types::infer     HM, §4            → TypeFacts      (crate-private)
              2. instances        parametric rules → finite instance set (§5.2)
              3. csp choices      finite, piped     → CspSolution    (LW5.COMP-4)
              4. lay out          TypeFacts → Layout per rule and node
              5. export                             → LayoutFacts    (public)
         ──► AnalysisFacts ──► … ──► Backend IR ──► one lowerer per backend (Lock 5)
```

- **`TypeFacts`** (internal): the type-term arena, the solved type of every node, the scheme of every top-level name, the instantiation record of every use, and the certificate of §4.9. It never crosses the stage boundary (`ARCH:1092`).
- **Lay out** is a total, deterministic function from a ground type to a `Layout`: where root's Span compression, `(T, Vec<T>)` flattening, boxing of recursive names and enum representation go. It introduces no type the HM step did not produce, and it is the only place those four decisions are made. `Sum[τ₁ … τₙ]` → a tagged enum; `R` on a cycle → a boxed reference; `P τ` and `fn` never reach it (STAGE) except value closures, laid out as `&'i` closures (Lock 4).
- **`LayoutFacts`** (public) gains, beside skinny's `rule_types` and `node_types` (`skinny/crates/passes/src/lib.rs:92-93`): the instance table (which parametric rule, at which arguments, became which concrete rule) and the scheme of every exported value function. Maps are ordered (`BTreeMap` or dense arenas); skinny's `HashMap` fields would break `regen --check`.
- **Lock 5 IR.** Rules and instances lower to the existing variants. `e -> f` lowers to `CallHost` (a chain is a `Seq` of `CallHost`, `ARCH:951`); `@{ e }` to `SpanMark`; a typed result to `DirectBuild` / `ValueProject`. No new variant. The **bodies** of value functions written in BBNF have no home in the locked IR: ARCH forbids "Emitting runtime code directly from Grammar IR" (`ARCH:1917`) and lists no variant for a lambda, an application or a `match`. That is owner choice C5.
- **What is replaced.** The spine's `TypeSolution` (public, `mod.rs:41-46`) becomes the internal `TypeFacts`; its certificates stay. Root's `passes/types`, `TypeMap`, `TypeDesc` and the 8+ backend re-derivation sites of §1.1 are not ported: they end with root at LW4.COMP-1. The TS emitter reads types from `plan.json` (USE-3, `SK/PLAN.md:197`). gorgeous reads nothing today and needs nothing until LW4.PP-1 re-bases it. `bbnf-path`'s registry is rendered from `LayoutFacts`.

### 4.9 What the stage verifier checks afterward

Inference is hard; checking a finished typing is easy, and that is what the verifier does. `TypeFacts` carries, per node, the rule applied and, per use of a scheme, the instantiation chosen. The verifier (a `Spine::check_*` method, as `verify.rs:19-21` anticipates) re-reads each node: given the children's types and the recorded instantiation, is the node's type the one the rule yields? No unification, no search. New laws, each with a mutant: `SchemeClosed` (a scheme's body has no variable that is neither bound nor in the environment), `InstanceOfScheme` (every recorded use is a substitution instance of its scheme), `NoFreeVariableInRule`, `AltResolved` (a collapsed alternation has identical branch types; an uncollapsed one does not), `NominalOnCycle` (every cyclic component member is nominal), `StageClean`, `InstanceTableClosed` (every call in every instance body names an instance in the table).

### 4.10 Where the code lives (Locks 13, 14, 6)

In the spine, the compiler of record (X-1): `crates/core/src/compiler/layout/`, replacing `typecheck.rs` (1,634 lines, itself a Lock 13 fault). Each file under 500 lines, each exporting one thing:

| File | Content | Rough size |
|---|---|---|
| `term.rs` | type terms, arena, interning | 250 |
| `unify.rs` | union-find, occurs check, numeric flag | 300 |
| `infer_grammar.rs` | constraints for grammar expressions | 400 |
| `infer_value.rs` | constraints for value expressions and `match` | 400 |
| `generalize.rs` | levels, schemes, instantiation, defaults | 250 |
| `alt.rs` | deferred ALT resolution | 150 |
| `instances.rs` | memoized instantiation of parametric rules, budget | 350 |
| `layout.rs` | ground type → `Layout` | 350 |
| `facts.rs` | `LayoutFacts` export | 200 |
| `certify.rs` | certificate rows and the checker | 400 |

About 3,050 lines of source and about as much again in tests. No rule or grammar name appears (I7, Lock 14); the reference checker of §6 lives in `crates/oracle` and shares no code with it (I8). Nothing is a proc-macro; nothing generated is hand-edited. The spine directory already has 18 children and 8 files over 500 lines; this adds one child and removes one oversized file. The spine's wider Lock 13 debt is not this unit's.

---

## 5. The functional layer made real

### 5.1 What a function is

One notion: a top-level definition whose right side is `|params| body`. Its type says which layer it lives in: every parameter and the result under `P` means it runs in the generator (layer ii); none under `P` means it runs at parse time (layer iii). It is a value in the type system: it can be passed (`ap(opt)`, F10) and returned (`compose`, V10). The one restriction is STAGE: a function over parsers cannot be a parse result. `functions.rs`'s refusals disappear into ordinary typing: wrong arity and "not a function" are unification failures (N03, N04); "a function named as a rule" is `fn … ≠ P τ` (N05); "a definition that is not a rule's whole right side" is no longer an error at all, since a lambda is an expression (Lock 10).

### 5.2 Layer (ii): recursion allowed, compile-time termination kept

A call to a parametric rule is not expanded in place. It is an **instance**: the pair (rule, argument tuple), where arguments are compared structurally after their own instantiation. The first request for an instance creates a concrete rule and lowers its body; a later request for the same instance, including from inside its own body, is a reference to that rule. So `many = |x| (x , many(x)) | ε` at `many("a")` is one recursive concrete rule (F06), and it then meets the same progress and left-recursion laws as any rule (N11, N12).

Termination: the instance set must be finite. It is not always: `grow = |x| x | grow(@{ x })` asks for `grow("b")`, `grow(@{"b"})`, … (N10). Two candidate rules:

- *Structural recursion only.* Not meaningful here: a parser is not inductive data; there is nothing to recurse on.
- *A deterministic budget.* The compiler already has one: `CompileOptions.rules = 4,096` and `nodes = 65,536` (`budget.rs:20-41`). Instances are rules and their bodies are nodes, so exceeding either stops instantiation.

**I recommend the budget**, with the locked diagnostic `BBNF-GENERIC-CYCLE` (`PASS-1:128`: "produces an unbounded monomorphization set"), reporting the chain of instances that grew. The budget is a constant, not a clock or a stack depth, so the result is the same on every host and `regen --check` and the MANIFEST digest are unaffected. The occurs check has already removed the common growth pattern (N09), so the budget is the backstop, not the usual path. Layer (ii) is therefore **total** and deliberately not Turing complete in effect.

### 5.3 R-TC: layer (iii) is Turing complete; proof sketch

The untyped lambda calculus cannot be encoded directly: self-application fails the occurs check (N01, N02), and that is the price of decidable inference. Recursion is supplied instead by **named recursive top-level definitions**, typed by the `letrec` rule of §4.4 (monomorphic inside the component, generalized after). That rule is the standard, sound way to add a fixed point to HM; no recursive type and no `fix` operator in the surface is needed.

*Claim.* The value layer with (a) top-level mutual recursion, (b) `match`, (c) `List` with `nil`, `cons`, `uncons` computes every partial recursive function.

*Sketch.* Take any two-counter machine (Minsky 1967: Turing complete) with states `q₀ … qₖ`. Represent a counter as `List Bool` (its length). For each state write one definition:

```
// qᵢ : increment a, go to qⱼ
q_i = |a, b| q_j(cons(true, a), b) ;
// qᵢ : if a = 0 go to qₖ, else decrement a and go to qⱼ
q_i = |a, b| match uncons(a) { None => q_k(a, b), Some((h, t)) => q_j(t, b) } ;
// halt
q_h = |a, b| (a, b) ;
```

Every definition has the type `fn(List Bool, List Bool) → List Bool × List Bool` in F0, with no annotation; the whole machine is one strongly connected component, typed monomorphically. A run of the machine is a run of `q₀`. ∎

Corpus program T-TC-1 (§6.3) is this construction for a machine that multiplies; T-TC-2 is an interpreter that takes the machine as a parsed list of instructions, which shows universality inside one grammar.

What this needs that the grammar of record lacks: a conditional. `expressions.bbnf` has none. `match` is already locked (`PASS-1:243`), so adding it implements a lock; it invents nothing. Without `match`, the value layer is not Turing complete under any typing discipline, and no amount of inference changes that.

### 5.4 Where termination is and is not guaranteed

| Layer | Guarantee | Mechanism |
|---|---|---|
| (i) recognition | terminates on every input | progress law, `RecursiveRule`, unchanged; now also on instances |
| (ii) generator | terminates on every grammar | instance memo + `rules` / `nodes` budget → `BBNF-GENERIC-CYCLE` |
| inference itself | terminates | first-order unification with occurs check; ALT closure is bounded by the number of alternation nodes |
| (iii) transforms | **not guaranteed**; bounded | every call of a value function charges `RuntimeBudget.work` (`budget.rs:50-54`; `emit.rs:454`); exhaustion is the existing resource fault, never a hang |

Two fences keep (iii) from leaking into (i):

- **L-MATCH-INVARIANT.** A mapping never changes what is accepted (`grammar-syntax.md:140`). Tested by parsing every corpus input with all transforms replaced by the identity and comparing accept/reject and consumed length.
- **L-PREDICATE-TOTAL.** ARCH's "predicate closure" (`ARCH:1908`) does let a function decide a match. A function reachable from a predicate position must lie in an acyclic part of the call graph (no recursion, so it terminates). Checked on the dependency graph; diagnostic `BBNF-PREDICATE-RECURSIVE`. Until a predicate form exists in the grammar of record this law has nothing to check; it is written now so that the form cannot arrive without it.

---

## 6. Soundness obligations as testable laws

### 6.1 Laws

| Law | Statement | Test | Mutant it must kill |
|---|---|---|---|
| L-REF | The production solver and an independent reference agree. | Reference: a naive, substitution-based Algorithm W plus ALT, NUM, DEFAULT, in `crates/oracle/` (≤ 500 lines, no shared code). For 10⁵ generated programs (type-directed generator, plus mutation of corpus programs so that ill-typed ones occur): both reject, or both accept with schemes equal up to renaming. | drop the occurs check; generalize before solving the component; instantiate without refreshing variables; unify arguments in the wrong arity order |
| L-PRINCIPAL | The inferred scheme is most general. | For each accepted generated program and each top-level name: (a) the reference's exhaustive enumeration of typings up to a size bound contains no scheme strictly more general; (b) every enumerated typing is an instance of the inferred one. | generalize a variable that is free in the environment; default a variable that should be generalized |
| L-GEN | A definition is polymorphic after its component and monomorphic inside it. | Corpus V15 (two uses at two types) accepts; N09 rejects. | treat the component as already generalized (accepts polymorphic recursion); never generalize (rejects V15) |
| L-OCCURS | No cyclic term is ever built. | N01, N02, N09; and a walk of the solved arena finds no cycle (already a verifier habit: `RuleBodiesAreTrees`). | bind a variable without the check |
| L-ORDER | The result is independent of constraint discovery order. | 16 permutations per corpus program, as `typecheck.rs:1568-1594` does today, over nodes and over ALT resolution order. | resolve ALT in traversal order |
| L-UNIFY-LATTICE | BR3 on the unifier (§4.7). | `br3.sh`, 10⁵ pairs. | first-writer-wins binding |
| L-ALT-STABLE | Instantiating a scheme never changes a collapse decision made in it. | F04, F05 and their instances. | re-run ALT after instantiation |
| L-NUM, L-DEFAULT | Defaults fire only on variables still free at generalization, and are never generalized. | V04, V08, X01, X02. | default before solving; generalize a numeric variable |
| L-STAGE | No `P` under `P`. | N13. | skip the check |
| L-PURE | The value language has no mutable cell. | A grep-level law over the value grammar and prelude: no primitive of type `fn(…) → Ref τ`. | add one |
| L-PRESERVE | Evaluation keeps types. | A small-step reference evaluator for value expressions in `crates/oracle/`. For 10⁵ generated well-typed closed terms: after every step, the term re-infers to a type of which the original is an instance. | an evaluator step that swaps `match` arms; a prelude primitive with a wrong scheme |
| L-PROGRESS | A well-typed closed term is a value, or steps, or exhausts the budget; it is never stuck. | Same run: no term reaches a state with no rule (a non-function applied, a `match` with no arm). | remove the exhaustiveness check |
| L-PARSE-TYPED | A parse of rule `r` yields a value of type `R`. | For every corpus grammar and the G1 fuzz inputs (≥ 10⁶ per grammar, `LW3-ORDER.md:224`): the reference interpreter's `obs(tree)` value is checked against the inferred type by a 60-line `has_type`. | lay out a sequence with one field dropped; swap two sum tags |
| L-CERT | The certificate checker accepts every solver output and rejects every tampered one. | §4.9's seven laws, each with ≥ 3 mutants, added to `verify/mutants.rs`'s 61. | one per law |
| L-INSTANCE | The same instance is created once; the instance set is closed; a growing chain stops at the budget with the chain printed. | F06, F09, N10. | compare arguments by node identity instead of structure (creates duplicates, then overflows) |
| L-MATCH-INVARIANT, L-PREDICATE-TOTAL | §5.4. | §5.4. | a transform that returns a parser failure |
| L-DETERMINISM | `LayoutFacts` is byte-equal across 2 runs and 2 hosts. | COMP-7's reading (`LW3-ORDER.md:116`). | a `HashMap` in the export path |
| L-TC | The value layer runs a two-counter machine. | T-TC-1, T-TC-2. | forbid recursion in value functions |
| L-INF | No corpus program in F0 contains a type annotation, and each is accepted with the expected scheme. | The corpus runner strips and refuses `:` annotations in the positive F0 set. | make a parameter annotation mandatory |

### 6.2 Notation for the corpus

`P τ` parser of τ; `[τ]` is `List τ`; `τ?` is `Option τ`; `R ≜ τ` gives a nominal type's definition. Programs marked ‡ use `match`, which is locked (PASS-1 §6) but not yet in the grammar of record. Prelude as in §4.3.

### 6.3 Corpus: 64 named programs

**Grammar layer (20).**

| Id | Program | Expected |
|---|---|---|
| G01 | `a = "x" ;` | `a : P Span` (transparent) |
| G02 | `a = ε ;` | `P Unit` |
| G03 | `a = "x" , /[0-9]+/ ;` | `P A`, `A ≜ Span × Span` |
| G04 | `a = "x" \| "y" ;` | `P Span` (ALT collapses) |
| G05 | `a = "x" \| ("y" , "z") ;` | `P A`, `A ≜ Sum[Span, Span × Span]` |
| G06 | `a = "x"? ;` | `P A`, `A ≜ Span?` |
| G07 | `a = "x"* ;` | `P A`, `A ≜ [Span]` |
| G08 | `a = "(" >> /[a-z]+/ << ")" ;` | `P Span` |
| G09 | `a = /[a-z]+/ - "if" ;` | `P Span` |
| G10 | `a = @{ "x" , "y"* } ;` | `P Span` |
| G11 | `v = "[" , (v , ("," , v)*)? , "]" \| "n" ;` | `P V`, `V ≜ Sum[Span × (V × [Span × V])? × Span, Span]` |
| G12 | `a = "(" , b , ")" \| "x" ; b = a , ("," , a)* ;` | `a : P A`, `b : P B`, `A ≜ Sum[Span × B × Span, Span]`, `B ≜ A × [Span × A]` |
| G13 | `d = /[0-9]/ ; n = d+ ;` | `d : P Span`; `n : P N`, `N ≜ [Span]` |
| G14 | `u = "px" -> 0u8 \| "em" -> 1u8 ;` | `P u8` |
| G15 | `n = /[0-9]+/ -> 1i64 ; m = n , n ;` | `n : P i64`; `m : P M`, `M ≜ i64 × i64` |
| G16 | `b = "true" -> true \| "false" -> false ;` | `P Bool` |
| G17 | `x = ("a" -> 1u8) \| "b" ;` | `P X`, `X ≜ Sum[u8, Span]` |
| G18 | `x = ("a" \| "b")* ;` | `P X`, `X ≜ [Span]` |
| G19 | `x = ["a"] , {"b"} ;` | `P X`, `X ≜ Span? × [Span]` |
| G20 | `x = "a" ?w ;` | `P Span` |

**Parametric rules (10).**

| Id | Program | Expected |
|---|---|---|
| F01 | `sep = \|x\| x , ("," , x)* ; nums = sep(/[0-9]+/) ;` | `sep : ∀α. fn(P α) → P (α × [Span × α])`; `nums : P N`, `N ≜ Span × [Span × Span]` |
| F02 | `opt = \|x\| x? ;` | `∀α. fn(P α) → P α?` |
| F03 | `pair = \|x, y\| x , y ;` | `∀αβ. fn(P α, P β) → P (α × β)` |
| F04 | `alt2 = \|x, y\| x \| y ; r = alt2("a", "b") ;` | `alt2 : ∀αβ. fn(P α, P β) → P Sum[α, β]`; `r : P R`, `R ≜ Sum[Span, Span]` (no collapse after instantiation) |
| F05 | `same = \|x\| x \| x ;` | `∀α. fn(P α) → P α` |
| F06 | `many = \|x\| (x , many(x)) \| ε ; r = many("a") ;` | `many : ∀α. fn(P α) → P Many[α]`, `Many[α] ≜ Sum[α × Many[α], Unit]`; `r : P R`, `R ≜ Many[Span]`; 1 instance |
| F07 | `twice = \|f, x\| f(f(x)) ;` | `∀α. fn(fn(P α) → P α, P α) → P α` |
| F08 | `wrap = \|x\| "(" >> x << ")" ; e = wrap(e) \| "v" ;` | `wrap : ∀α. fn(P α) → P α`; `e : P E`, `E ≜ Sum[E, Span]` |
| F09 | `sepby = \|x, s\| x , rest(x, s) ; rest = \|x, s\| (s , x , rest(x, s)) \| ε ;` | `rest : ∀αβ. fn(P α, P β) → P Rest[α, β]`, `Rest[α, β] ≜ Sum[β × α × Rest[α, β], Unit]`; `sepby : ∀αβ. fn(P α, P β) → P (α × Rest[α, β])` |
| F10 | `opt = \|x\| x? ; ap = \|f\| f("x") ; r = ap(opt) ;` | `ap : ∀β. fn(fn(P Span) → P β) → P β`; `r : P R`, `R ≜ Span?` |

**Value layer (17).**

| Id | Program | Expected |
|---|---|---|
| V01 | `n = /[0-9]+/ -> \|s\| len(s) ;` | `P i64` |
| V02 | `x = "a" -> \|v\| v ;` | `P Span` |
| V03 | `x = ("a" , "b") -> \|p\| p ;` | `P X`, `X ≜ Span × Span` |
| V04 | `k = "a" -> 1 + 2 ;` | `P i64` (NUM default) |
| V05 | `k = "a" -> 1.5 ;` | `P f64` |
| V06 | `k = "a" -> 1 < 2 && true ;` | `P Bool` |
| V07 | `k = /[0-9]+/ -> input ;` | `P Span` |
| V08 | `inc = \|n\| n + 1 ;` | `fn(i64) → i64` |
| V09 | `id = \|v\| v ; r = id("a") ; q = "a" -> id ;` | `id : ∀α. fn(α) → α`; `r : P Span`; `q : P Span` |
| V10 | `compose = \|f, g\| \|x\| f(g(x)) ;` | `∀αβγ. fn(fn(β) → γ, fn(α) → β) → fn(α) → γ` |
| V11 ‡ | `fact = \|n\| match n { 0 => 1, _ => n * fact(n - 1) } ;` | `fn(i64) → i64` |
| V12 ‡ | `even = \|n\| match n { 0 => true, _ => odd(n - 1) } ; odd = \|n\| match n { 0 => false, _ => even(n - 1) } ;` | both `fn(i64) → Bool` |
| V13 ‡ | `count = \|xs\| match uncons(xs) { None => 0, Some((h, t)) => 1 + count(t) } ;` | `∀α. fn([α]) → i64` |
| V14 ‡ | `map = \|f, xs\| match uncons(xs) { None => nil(), Some((h, t)) => cons(f(h), map(f, t)) } ;` | `∀αβ. fn(fn(α) → β, [α]) → [β]` |
| V15 | `id = \|v\| v ; r = ("a" -> id) , (("b" -> 1u8) -> id) ;` | `r : P R`, `R ≜ Span × u8` |
| V16 ‡ | `foldl = \|f, z, xs\| match uncons(xs) { None => z, Some((h, t)) => foldl(f, f(z, h), t) } ;` | `∀αβ. fn(fn(β, α) → β, β, [α]) → β` |
| V17 | `k = /[0-9]+/ -> 3u8 ; j = k -> \|v\| v ;` | `k : P u8`; `j : P u8` |

**Defaults and Turing witnesses (4).**

| Id | Program | Expected |
|---|---|---|
| X01 | `e = "a" -> nil() ;` | `P E`, `E ≜ [Unit]` (DEFAULT) |
| X02 | `loop = \|n\| loop(n) ; r = "a" -> \|v\| loop(0) ;` | `loop : ∀αβ. fn(α) → β`; `r : P Unit` (DEFAULT); at run time, resource fault on `work` |
| T-TC-1 ‡ | the two-counter multiplier of §5.3, 6 states | each state `fn([Bool], [Bool]) → [Bool] × [Bool]`; `q₀` on lengths (3, 4) halts with (0, 12) |
| T-TC-2 ‡ | a grammar that parses a two-counter program into a list of instructions, and `run = \|prog, pc, a, b\| …` | `run : fn([I], i64, [Bool], [Bool]) → [Bool] × [Bool]`; runs T-TC-1's program to the same answer |

**Negative (13).** Each must be refused with the named diagnostic and no other. N01-N09 are type errors; N10-N13 pass unification and are refused by a later law.

| Id | Program | Expected refusal |
|---|---|---|
| N01 | `omega = \|x\| x(x) ;` | occurs check: `α = fn(α) → β` |
| N02 | `y = \|f\| (\|x\| f(x(x)))(\|x\| f(x(x))) ;` | occurs check |
| N03 | `pair = \|x, y\| x , y ; r = pair("a") ;` | mismatch: 2 parameters, 1 argument |
| N04 | `a = "x" ; r = a("y") ;` | mismatch: `P Span` is not a function |
| N05 | `opt = \|x\| x? ; r = opt , "a" ;` | mismatch: a sequence element must be `P τ`, found `fn(P α) → P α?` |
| N06 | `k = "a" -> 1 + true ;` | mismatch: `Bool` is not numeric |
| N07 | `k = "a" -> \|v\| v + 1 ;` | mismatch: `Span` is not numeric |
| N08 ‡ | `f = \|n\| match n { 0 => 1, _ => true } ;` | mismatch between arms: `i64`, `Bool` |
| N09 | `grow = \|x\| x \| grow(x , "a") ;` | occurs check: `α = α × Span` (polymorphic recursion needs a signature) |
| N10 | `grow = \|x\| x \| grow(@{ x }) ; r = grow("b") ;` | types accept; `BBNF-GENERIC-CYCLE` at the instance budget, chain printed |
| N11 | `star = \|x\| x* ; r = star("a"?) ;` | types accept; `NullableRepeat` on the instance (progress law) |
| N12 | `l = \|x\| (l(x) , "a") \| x ; r = l("b") ;` | types accept; `RecursiveRule` on the instance |
| N13 | `opt = \|x\| x? ; r = "a" -> \|v\| opt ;` | `BBNF-STAGE`: a parse result contains a parser function |

Total 64 programs. Each is one file with its expected output beside it; the runner prints schemes with variables renamed in order of first appearance.

---

## 7. Staged plan

Sizes are source lines, tests excluded, rough. Every stage has a gate that is RED before its first commit.

| Stage | Wave and phase | Content | Born-RED gate | Size |
|---|---|---|---|---|
| **H0** hold | LW3 Phase 3b1, on lane `lw3/comp-1c`, records only | What COMP-1c must **not** bake in now: (1) `functions.rs` expansion-before-resolve as the meaning of a call (already held, `SK/LEDGER.md:554`); (2) the four `functions::tests` laws and `GrammarFunction` as language laws; (3) `SemanticTypeData::Value(name)` or any type read from a written `: Type`; (4) new public surface on `TypeSolution`; (5) new spine plan ops for `->` and `@{ }` (15a) before C5 is ruled; (6) any new law that says standalone `@ws`, `@recover`, `@debug` or bodyless `@host` **is the language** (C3). COMP-1c keeps: reading every form to a typed source node; `FormWithoutMeaning` at resolve; entry rule; staged gate; deleting `frontend.rs`; rewriting the `%{ }` / `@name(…)` laws. | none (a hold) | 0 |
| **H1** corpus and oracle | LW3 Phase 3b2, new unit **LW3.LAYOUT-1**, seat beside S8 (it touches `crates/oracle` only) | The 64 corpus files with expected outputs; the reference checker and evaluator in `crates/oracle/`; the generators for L-REF, L-PRINCIPAL, L-PRESERVE, L-PROGRESS. | `layout_corpus` runs the spine on the corpus. RED today by reading: no `F`, `V`, `X` or `T` program can pass (calls, closures and `->` are refused at resolve), `G04` and `G18` come out as `Sum` where ALT collapses, and the `N` programs are refused with the wrong diagnostic; the exact count is read at the unit's first run. The reference itself is gated by the corpus: 64 of 64. | 900 |
| **H2** HM core in layout lowering | LW3 Phase 3b2, new unit **LW3.LAYOUT-2**, seat S7c (owner of `typecheck`, `syntax`, `mod` per `LW3-ORDER.md:200`), after COMP-1c merges | `compiler/layout/` per §4.10 without `infer_value.rs`'s `match`: grammar layer, parametric rules with instances and budget, `->` with constants, paths, closures and prelude calls, `@{ }`. `typecheck.rs` and `[C1C]` `functions.rs` deleted. The public stage is renamed to layout; `TypeSolution` leaves the public surface. Certificates and the seven verifier laws. | (a) corpus G, F, V01-V10, V15, V17, X01, N01-N07, N09-N13: RED → GREEN; (b) L-REF at 10⁵; (c) **BR3** re-pointed at the unifier, RED → GREEN, and its LEDGER row moves from LW5.COMP-1 to this unit; (d) **G-DIGEST**: for each of the 70 MANIFEST entries the spine accepts, the rule types rendered from `LayoutFacts` equal root's "Rust-prepared rule types" column, or the difference has a DIVERGENCE row; (e) Lock 2 gate: `git grep -n 'pub.*TypeSolution\|TypeFacts' -- crates/core/src/lib.rs crates/core/src/compiler/mod.rs` reads 0 and `LayoutFacts` is exported; (f) 16-permutation L-ORDER; (g) every mutant of §6.1 for these laws refused ×2. | 2,650 |
| **H3** emitters read the facility | **LW4.COMP-1, widened** | The plan-driven Rust emitter takes every type from `LayoutFacts`; parse-time calls lower to `CallHost`; each call charges `work`. Root's re-derivation sites are not ported. TS reads types from `plan.json`. The registry sidecar is rendered from `LayoutFacts`. | L-PARSE-TYPED over the 9 + 3 grammars at the G1 fuzz count; a Lock 5 gate: a scan of the emitter for any function that builds a type from a node kind reads 0 (the §1.1 list is the RED set, carried to the spine emitter as a mutant: re-introduce one, the gate refuses). L-MATCH-INVARIANT. | inside COMP-1's budget; +400 |
| **H4** the Turing layer | **LW4** after C3 and C5 are ruled; unit **LW4.LAYOUT-3** | `match`, tuples and named value functions in the grammar of record and the front end; `infer_value.rs` complete; exhaustiveness; bodies emitted per C5. | Corpus ‡ programs and T-TC-1, T-TC-2 RED → GREEN; L-PRESERVE and L-PROGRESS at 10⁵; X02 faults on `work` and does not hang (wall-clock bound in the test). | 1,200 |
| **H5** the Lock 4 extensions | **LW5.COMP-4** (its row exists: `SK/PLAN.md:320`, `:363`), scope now exact | Bidirectional checking of written annotations; finite CSP choices piped from HM (host overloads, layout representation); DK13 for rank-n annotations; OutsideIn implication constraints for `@ where`. Each entered only where the user writes the annotation (C1). | For DK13 and OutsideIn: their own corpora, born RED; plus **L-INF re-read**: the whole F0 corpus still passes with the extension code compiled in and with it compiled out, byte-equal `LayoutFacts`. | 2,500 |

What moves: BR3 from LW5.COMP-1 to LW3.LAYOUT-2 (earlier). LW5.COMP-1 is otherwise unchanged. LW5.COMP-4 keeps its home and loses its vagueness. LW4.COMP-1 widens by the Lock 5 gate. COMP-3b reads the layout carrier. Two TS DIVERGENCE rows are filed (no grammar functions; `@{ }` without effect), owner LW4.COMP-4.

Total new source: about 7,600 lines across four waves, none in a file over 500, plus tests of similar size. H1 and H2, the part that makes "Hindley-Milner with full inference" true for everything the language can write today, is about 3,550.

---

## 8. Owner choices (five)

**C1. Full inference against Lock 4.** Lock 4 puts higher-rank types and GADT equalities in V1; neither can be fully inferred, by theorem (§4.6). *Recommendation:* the HM core F0 is the language; it is fully inferred and that is a tested law (L-INF). Higher-rank and local equalities stay as Lock 4 says, but as extensions a program enters only by writing the annotation, built after the core (H5). A program that uses neither never needs an annotation. This satisfies both sentences. The alternative, dropping them from Lock 4, is a lock edit and gains nothing.

**C2. Turing complete against ARCH §8.4.** ARCH says closures exist "without turning BBNF into a general programming language". *Recommendation:* amend that sentence, not the architecture: Turing completeness lives in the transform layer only, bounded by the runtime work budget; recognition and the generator stay total (§5.4). The alternative readings are worse: Turing-complete recognition breaks the progress law and G1; a Turing-complete generator breaks `regen --check`.

**C3. Which surface is the language.** `grammar/bbnf/bbnf.bbnf` (COMP1C-1) or Lock 10 with PASS-1 §6 (§0.3). *Recommendation:* Lock 10 governs directives: `@ws`, standalone `@recover`, `@debug` and bodyless `@host` are migrated out (2 + 1 + 0 + 0 corpus files, plus the TS fixtures), and COMP1C-1's acceptance of them is withdrawn. The grammar of record governs operators the locks never name and 70 MANIFEST entries use (`?w`, `<<`, `>>`, `-`, `[ ]`, `{ }`, `@{ }`, per-factor `->`). The locked function surface (`match`, tuples, lambda as an expression, `fn` types) is added to the grammar of record. One written list, ruled once.

**C4. Optional annotations and the undefined `Block`.** Full inference needs `Param ::= Ident (":" Type)?`, an optional return type on a BBNF-bodied function, and undeclared type parameters; and `Block` has no production anywhere. These edit locked productions, so they are G-Omega. *Recommendation:* make all three optional; define `Block` as `"{" Expr "}"` (an expression in braces, nothing more) until a need for statements is shown.

**C5. Where a function body crosses Lock 5's IR.** The locked Backend IR has no variant for a lambda, an application or a `match`, and ARCH forbids emitting from Grammar IR. *Recommendation (a):* no new variant. Bodies travel in a typed function table that is part of the IR module and is referenced by `CallHost`'s function id; each backend lowers the table. This needs the owner's word that a table beside the variants is not "a BIR variant" in the Governance Boundary's sense. *Alternative (b):* four new variants (lambda, apply, match, tuple), which is a G-Omega amendment. The spine's analogue, new plan ops for `->` and `@{ }` (RULINGS 15a), should wait for this ruling.

Not owner choices, decided here and open to challenge: rule ALT (§4.2), rule NOM (§4.2), NUM and DEFAULT (§4.3, §4.4), the instance budget over structural recursion (§5.2).

---

## 9. What I did not verify

- Nothing was compiled or run. "Root overflows the stack on a recursive grammar function" is from reading `closures.rs:48-77`.
- The principal-type claim of §4.6 is a claim about a system with three non-textbook rules (ALT, NUM, DEFAULT). I believe it; I have not proved it. L-REF and L-PRINCIPAL are the test, and ALT's confluence argument is the part most worth a second reader.
- G-DIGEST may show differences between ALT plus layout and root's two join rules on real grammars; the 21 entries BR3 lists are the likely place. Each would need a row.
- The TS package and gorgeous were read by a helper, not by me; the citations were spot-checked, not all re-opened.
- `LOCKS.md:372-580` (Locks 14 tail, 15, 16) was skimmed for type and directive content, not read line by line; it concerns build profile and SIMD.

---

## 10. Summary for the owner (one page)

**What is true today.** There is no Hindley-Milner in BBNF. The spine reads each rule's type straight off its shape: sound, certified, but no type variables, no unification, no function types. The old root pipeline uses a constraint solver whose merge rule is "last writer wins"; that is the open defect BR3. The lab tree has the right names (`LayoutFacts`) and empty stubs. Types are derived in three places and re-derived in at least eight backend sites. Functions are macros: root expands them with no recursion check; the new lane refuses recursion; the TypeScript package does not have them. Nothing in the language is Turing complete, and the corpus contains no function definition at all, so their meaning can be set freely.

**What your sentence adds.** Against the locks, four of your six asks are already required and simply unbuilt: HM (Locks 2, 4), a functional language (Lock 10), robust types (ARCH §8.2), one facility (Lock 5). They get gates here, not new requirements. Two are new: **full inference** and **Turing complete**.

**The design, in five lines.**
1. HM runs as a private subroutine of layout lowering; the only public result is `LayoutFacts`; every backend reads that and nothing else. No second IR.
2. Everything writable without annotations gets a principal type: rules, parametric rules, functions as values, recursion. Rules are named recursive types; mutually recursive definitions are typed per strongly connected component and generalized after. The occurs check is always on.
3. BR3 is cured by this, a wave early: the "real refinement order" is a type variable being filled in, which is unification.
4. Recursion is allowed in functions. Functions over grammars are instantiated once per distinct argument, under the compiler's existing rule and node budget, so the generator always halts. Functions over parse results may loop; each call charges the existing runtime work budget.
5. Turing completeness is in the transform layer only, proved by encoding a two-counter machine with recursion, `match` and lists. Recognition keeps every law it has.

**Proof it is real, not asserted.** A second, independent checker; 10⁵ generated programs compared; 64 named programs with expected types, 13 of them refusals; type preservation and progress as executable tests; a mutant for every law.

**Cost and order.** About 7,600 lines of source over four waves. The part that makes "HM with full inference" true for today's language is about 3,550 lines in LW3 Phase 3b2 (units LW3.LAYOUT-1, LW3.LAYOUT-2). Emitters follow in LW4; `match` and the Turing witness in LW4; the annotated extensions in LW5.COMP-4.

**Five decisions I need from you.**
- **C1** Full inference versus Lock 4's higher-rank and GADT features, which cannot be fully inferred by theorem. *Recommend:* fully inferred core as the language; those two as opt-in, annotated extensions, built later.
- **C2** "Turing complete" versus the architecture's "not a general programming language". *Recommend:* amend that sentence; Turing complete in transforms only, bounded by budget.
- **C3** Which surface is the language: today's grammar file, or Lock 10. *Recommend:* Lock 10 for directives (retire `@ws`, `@recover`, `@debug`, bodyless `@host`), today's operators kept, the locked function surface added. This withdraws part of ruling COMP1C-1.
- **C4** Make parameter and return annotations optional in the locked grammar, and define the undefined `Block`. *Recommend:* yes; `Block` is an expression in braces.
- **C5** How function bodies cross the backend IR. *Recommend:* a typed function table beside the existing variants, no new variant.

**Held now.** On lane `lw3/comp-1c`: the macro expansion of functions, its recursion refusal, any type taken from a written annotation, and new plan ops for `->` and `@{ }`, until C3 and C5 are ruled.
