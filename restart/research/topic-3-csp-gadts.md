# Topic 3 — CSP-backed unification, GADTs, parametric polymorphism, and generic rules

Status: Phase 1 research deep-dive.
Scope: `restart/research/topic-3-csp-gadts.md` only.
Decision: CSP-backed unification must be split into HM(X)-style constraint generation for ordinary polymorphism, OutsideIn(X)-style implication/equality handling only where local type assumptions exist, and bbnf's orthogonal finite-domain CSP solver for layout, host, backend, and recognizer choices. Plain `Object<V>` is parametric polymorphism; HM handles it. CSP earns its keep only when `Object<V>` interacts with finite choices, overloaded host primitives, layout directives, backend erasure, or branch-local equality facts.

## §1 — Settled position in the restart

1. Output contract.
2. Current research artifacts must carry §1 through §7, with §1 giving restart claims by path:line, §2 using 5-15 primary sources, §3/§4 proving convergence/divergence from both restart and SOTA, §5 giving fold refinements, §6 giving adversarial findings, and §7 giving concrete surgery proposals (`restart/research/INDEX.md:18`-`restart/research/INDEX.md:34`).
3. The fold cycle absorbs §5 and §7 into trio plus PASS surfaces, while §6 can trigger escalation when SOTA contradicts a settled lock (`restart/research/INDEX.md:5`).
4. Every research agent must surface at least one adversarial finding or explain why none contradicts a lock (`restart/research/INDEX.md:149`-`restart/research/INDEX.md:153`).
5. Voice and discipline are calibrated, direct, archaic-permissive, path-cited, and hostile to placeholder wording, quick solutions, and uncontested legacy code (`restart/research/INDEX.md:155`-`restart/research/INDEX.md:158`; `docs/precepts/instructions/STYLE.md:5`-`docs/precepts/instructions/STYLE.md:16`).
6. Process discipline says research needs challenge before synthesis (`docs/precepts/instructions/LESSONS-LEARNED.md:38`-`docs/precepts/instructions/LESSONS-LEARNED.md:45`), substrate changes need consumers (`docs/precepts/instructions/LESSONS-LEARNED.md:17`-`docs/precepts/instructions/LESSONS-LEARNED.md:26`), and contracts need producer plus consumer gates (`docs/precepts/instructions/LESSONS-LEARNED.md:74`-`docs/precepts/instructions/LESSONS-LEARNED.md:80`).

### Restart claim table

| Claim ID | Current text or near-verbatim position | Citation |
|---|---|---|
| R1 | The greenfield internals are the apotheosis: CSP, e-graph, shape mining, cost model, bidirectional inference, and grammar-derived everything. | `restart/README.md:5` |
| R2 | The generic substrate includes `csp-solver`, `egraph`, `parse-that`, and other sister crates without the public `bbnf-` prefix. | `restart/README.md:31`-`restart/README.md:60` |
| R3 | Optimization sister crates compose by output-piping; CSP and egraph are siblings, not a unified graph. | `restart/README.md:92`; `restart/locks/LOCKS.md:40` |
| R4 | Grammar IR is typed and cost-annotated through side tables; optimized IR is not a third tree. | `restart/README.md:113`-`restart/README.md:114` |
| R5 | `@host fn` bodies use lexical scoping and propagate type variables through bidirectional inference. | `restart/README.md:145`-`restart/README.md:156` |
| R6 | Multi-function chains type projections; CSP backs constraint collection. | `restart/README.md:159`-`restart/README.md:162` |
| R7 | Generic rules are in V1: `Object<V>` and `pair<V>` carry type variables; CSP propagates; codegen monomorphizes per call site. | `restart/README.md:168`-`restart/README.md:170` |
| R8 | Pass 3 is type inference: CSP plus bidirectional plus Hindley-Milner produces `TypedGrammarIR`. | `restart/README.md:192`-`restart/README.md:196` |
| R9 | Type inference and validation co-iterate to fixed point, because typed left-recursion depends on validation and validation depends on the inference domain. | `restart/README.md:209` |
| R10 | CSP is the central inference substrate; e-graphs are rewrite and extraction substrate. | `restart/README.md:219`-`restart/README.md:227` |
| R11 | The type-system line says HM, Pierce-Turner bidirectional check/synth, and CSP-backed unification are composed. | `restart/README.md:258`-`restart/README.md:260` |
| R12 | Explicit annotations narrow inference; multi-function chains flow types through stages with bidirectional checks. | `restart/README.md:262` |
| R13 | Generic rules in §7 repeat `Object<V>` and say CSP propagates type variables and codegen monomorphizes per call site. | `restart/README.md:264` |
| R14 | Subtyping is described as full HM with subsumption; CSP relaxes constraints; coercion is constraint relaxation. | `restart/README.md:266` |
| R15 | Lookbehind's left operand is a context constraint, not a value capture. | `restart/README.md:268` |
| R16 | Lock 4 forbids a unified hypergraph and requires per-domain orthogonal optimization by output-piping. | `restart/locks/LOCKS.md:40` |
| R17 | Lock 10 in the lock file is Pratt + SIMD auto-detection, with no `@pratt` or `@simd` directives. | `restart/locks/LOCKS.md:52` |
| R18 | Lock 14 forbids grammar-specific code in generic crates and admits new grammars through grammar source, metadata, and rare fenced declarations. | `restart/locks/LOCKS.md:60` |
| R19 | Architecture says the csp-solver is a generic sister crate used by type inference, layout choices, and extraction facts. | `restart/ARCHITECTURE.md:60` |
| R20 | Architecture keeps `egraph` and `csp-solver` generic, publishable or path-dep incubated without grammar concepts. | `restart/ARCHITECTURE.md:176` |
| R21 | Pipeline order currently runs type inference, shape mining, recognizer mining, egraph rewrite, CSP solve, cost extraction, Backend IR, lowerers, and regen equality. | `restart/ARCHITECTURE.md:768`-`restart/ARCHITECTURE.md:784` |
| R22 | Type inference annotates Grammar IR and does not mutate grammar syntax; HM, bidirectional, and CSP run as a subroutine inside `passes::layout`. | `restart/ARCHITECTURE.md:791`-`restart/ARCHITECTURE.md:797` |
| R23 | Egraph and CSP exchange facts through bridge tables, not through a fused representation. | `restart/ARCHITECTURE.md:799` |
| R24 | Grammar IR `Rule` carries generic parameters, signatures, annotations, and body. | `restart/ARCHITECTURE.md:824`-`restart/ARCHITECTURE.md:829` |
| R25 | Grammar IR `Ref` stores type arguments and resolves only after generics instantiate. | `restart/ARCHITECTURE.md:855` |
| R26 | Host calls resolve through generic primitives or declared `@host fn` signatures, with host signatures unifying inside layout lowering. | `restart/ARCHITECTURE.md:872` |
| R27 | `TypeFacts` are internal scratch; downstream passes read `LayoutFacts`; `CspSolution` is public only where produced for cost extraction, layout, or host chain typing. | `restart/ARCHITECTURE.md:975`-`restart/ARCHITECTURE.md:994` |
| R28 | Architecture §8 says the BBNF surface supports lookbehind, block-bodied `@host fn`, multi-function chaining, generics, `@error`, and `@layout`. | `restart/ARCHITECTURE.md:1045`-`restart/ARCHITECTURE.md:1050` |
| R29 | The formal grammar sketch includes generic parameters, type arguments, and rule-level chain expressions. | `restart/ARCHITECTURE.md:1059`-`restart/ARCHITECTURE.md:1088` |
| R30 | Architecture says a generic rule can chain as `Object<V> = Expr -> f1 -> f2;`, with `Object<V>` parsed through `Ref`, `GenericParams`, and `TypeArgs`. | `restart/ARCHITECTURE.md:1095`-`restart/ARCHITECTURE.md:1098` |
| R31 | Architecture §8.2 says the type system is HM plus bidirectional checks and CSP constraints. | `restart/ARCHITECTURE.md:1115`-`restart/ARCHITECTURE.md:1121` |
| R32 | Architecture type rules say inference is grammar-wide and host chains compose left-to-right. | `restart/ARCHITECTURE.md:1123`-`restart/ARCHITECTURE.md:1132` |
| R33 | PASS-1 keeps HM + bidirectional + CSP-backed constrained unification. | `restart/audit/pass-1-substrate/PASS-1.md:12` |
| R34 | PASS-1 says host/chains/generics carry forward. | `restart/audit/pass-1-substrate/PASS-1.md:18` |
| R35 | PASS-1 says type-system algorithm: HM generates core constraints, bidirectional checking handles explicit signatures/directives, CSP-backed constrained unification solves finite choices for host overload, layout representation, recognizer eligibility, materialization, recovery, and backend plan. | `restart/audit/pass-1-substrate/PASS-1.md:71` |
| R36 | PASS-1 says canonical chains thread each step's output type into the next step's first argument and fail with `BBNF1401` on mismatch. | `restart/audit/pass-1-substrate/PASS-1.md:220` |
| R37 | PASS-1's future onboarding proof permits grammar source and metadata, forbids generic-crate match arms, and emits generated output without a manual Rust registry. | `restart/audit/pass-1-substrate/PASS-1.md:224`-`restart/audit/pass-1-substrate/PASS-1.md:232` |
| R38 | PASS-1 closes by saying CSP and e-graph are bridged and domain-scoped. | `restart/audit/pass-1-substrate/PASS-1.md:279`-`restart/audit/pass-1-substrate/PASS-1.md:281` |
| R39 | V4 hardening says the cohort was READY and carried no open punch items. | `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:106`-`restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:108` |
| R40 | V5 consolidation keeps `LayoutFacts` / `passes::layout` public and `TypeFacts` internal. | `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:146` |
| R41 | V5.1 synthesis returns READY after closing formal BBNF grammar reconciliation and stale cleanup. | `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:49`-`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:69` |
| R42 | Topic 3 in the research index says the anchor locks are Lock 4 and Lock 10, and labels Lock 10 as BBNF V1 generics `Object<V>`. | `restart/research/INDEX.md:66`-`restart/research/INDEX.md:70` |

### Immediate reading of the restart position

1. The strongest restart interpretation is not "replace HM unification with a CSP solver."
2. The strongest interpretation is "generate ordinary type constraints through HM and bidirectional checking, then use the csp-solver for finite choices that HM does not model as first-order type equations."
3. PASS-1 already says this more accurately than README §7: CSP solves finite choices for host overload, layout representation, recognizer eligibility, materialization, recovery, and backend plan (`restart/audit/pass-1-substrate/PASS-1.md:71`).
4. README §7 uses the riskier phrase "CSP-backed unification" (`restart/README.md:260`), which can be misread as a global constraint-programming replacement for unification.
5. Plain generic rules are not a GADT feature. `Object<V>` is a parametric rule scheme; under the current grammar sketch it is a `Rule` with `GenericParams` and a `Ref` with `TypeArgs` (`restart/ARCHITECTURE.md:824`-`restart/ARCHITECTURE.md:855`, `restart/ARCHITECTURE.md:1067`-`restart/ARCHITECTURE.md:1084`).
6. GADT pressure appears only if BBNF introduces branch-local equality assumptions, existential rule payloads, indexed return refinements, or host constructors that refine a type variable inside a branch. The restart gestures at GHC's `OutsideIn(X)` in README §7 (`restart/README.md:260`) but does not yet define that local-equality surface.
7. The research index's Lock 10 mapping is inconsistent with the lock file. Lock 10 is Pratt + SIMD auto-detection (`restart/locks/LOCKS.md:52`), not generics. Generics live in README §5/§7 and Architecture §8, with Lock 14 as the closest generality lock.

## §2 — SOTA literature deep-dive

### Source registry

| ID | Primary source | URL or official reference | Role in this topic |
|---|---|---|---|
| S1 | François Pottier and Didier Rémy, "The Essence of ML Type Inference," 2005. | https://gallium.inria.fr/~fpottier/publis/emlti-final.pdf | Constraint presentation of ML inference; HM(X) adjacent foundation; ordinary parametric polymorphism and principal schemes. |
| S2 | Dimitrios Vytiniotis, Simon Peyton Jones, Tom Schrijvers, Martin Sulzmann, "OutsideIn(X): Modular type inference with local assumptions," JFP 2011. | https://simon.peytonjones.org/assets/pdfs/outsideinx.pdf and Cambridge DOI 10.1017/S0956796811000098 | Local assumptions, GADTs, implication constraints, principal-type discipline, GHC implementation frame. |
| S3 | Sulzmann, Schrijvers, Stuckey, "Type inference for GADTs via Herbrand constraint abduction," KU Leuven CW 507, 2008. | https://www.cs.kuleuven.be/publicaties/rapporten/cw/CW507.pdf | GADT inference hardness; Herbrand constraints; infinite maximal types; abduction as CSP-adjacent pressure. |
| S4 | Tom Schrijvers and Maurice Bruynooghe, "Polymorphic algebraic data type reconstruction," PPDP 2006. | https://dtai.cs.kuleuven.be/people/dtaiMembers/dtai-publications/view?fromnr=2361&pubsonpage=20&pubtype=&sortby=screator and DOI 10.1145/1140335.1140346 | Rule-based constraint rewriting for reconstructing polymorphic ADT definitions and expression types. |
| S5 | GHC official source docs for `compiler/typecheck`: `TcSimplify`, `TcCanonical`, `TcRnTypes`. | https://downloads.haskell.org/~ghc/8.6.5/docs/html/libraries/ghc-8.6.5/src/TcSimplify.html, https://downloads.haskell.org/~ghc/8.2.1/docs/html/libraries/ghc-8.2.1/src/TcCanonical.html, https://downloads.haskell.org/ghc/8.8.1/docs/html/libraries/ghc-8.8.1/TcRnTypes.html | Production evidence: wanted constraints, implications, canonicalization, OutsideIn floating restrictions. |
| S6 | Luca Cardelli and Peter Wegner, "On Understanding Types, Data Abstraction, and Polymorphism," ACM Computing Surveys 1985. | https://dl.acm.org/doi/10.1145/6041.6042; bibliographic mirror https://cir.nii.ac.jp/crid/1362544418451250432 | Parametric, inclusion, overloading, coercion, type abstraction, existential abstraction distinctions. |
| S7 | Martin Sulzmann, Gregory J. Duck, Simon Peyton Jones, Peter J. Stuckey, "Understanding functional dependencies via Constraint Handling Rules," JFP 2007. | https://www.cambridge.org/core/services/aop-cambridge-core/content/view/49E533CD7975431B5339456255DA9BE5/S0956796806006137a.pdf | CHRs as an explicit formal device for improvement rules, typeclass constraint propagation, and decidable inference under restrictions. |
| S8 | GHC User's Guide, latest official PDF, sections on GADTs/let-generalisation as implementation documentation. | https://downloads.haskell.org/~ghc/latest/docs/users_guide.pdf | Canonical project documentation. Used only as support for GHC source provenance, not as the main evidence. |

### Provenance gap

1. The requested source "Schrijvers & Stuckey 2008, `Polymorphic algebraic data type reconstruction`" could not be verified as stated.
2. The verified `Polymorphic algebraic data type reconstruction` record is Schrijvers and Bruynooghe, PPDP 2006, ACM Press, pp. 85-96; DTAI's publication record states those authors, venue, year, and the rule-based constraint-rewriting abstract (S4).
3. The verified 2008 source involving Stuckey and directly bearing on GADTs is Sulzmann, Schrijvers, and Stuckey, KU Leuven CW 507, `Type inference for GADTs via Herbrand constraint abduction` (S3).
4. This artifact therefore cites both: S4 for ADT reconstruction and S3 for the 2008 Stuckey/GADT/CSP-style inference pressure. It does not assert the unverified combined citation.

### S1 — Pottier and Rémy 2005

1. Load-bearing claim: ML type inference is decidable, principal-type-oriented, and relies on first-order unification, but a constraint presentation separates generation from solving.
2. The paper opens by defining "ML-the-type-system" as simply typed lambda calculus plus let-polymorphism, whose algorithms rely on first-order unification and efficient principal schemes.
3. The key architectural point for bbnf is the split: generate constraints from syntax, then solve them. This split supports separate reasoning about program correctness and checking mechanics.
4. Pottier and Rémy also point out that production implementations often update terms in place rather than manipulate explicit substitutions, which matters for bbnf's planned side-table strategy.
5. The paper's constraint view is wide enough for rows, recursive types, records, and variants. It is not automatically wide enough for local equality assumptions from GADTs.
6. Design tradeoff: HM gives principal types and ergonomic inference for ordinary let-polymorphism; richer constraint domains demand solver laws and explicit residual constraints.
7. Evidence: formal development across the chapter plus an accompanying implementation referenced from Pottier's site.
8. bbnf implication: `Object<V>` can be an HM-style type scheme. A grammar rule scheme with type parameter `V` does not require a finite-domain CSP solver or OutsideIn unless some grammar construct generates extra constraints beyond first-order equations.

### S2 — OutsideIn(X) 2011

1. Load-bearing claim: HM(X) is not enough for GADTs, type classes, and type families where local assumptions enter through pattern matches or signatures.
2. OutsideIn(X) is parameterized over an underlying constraint domain X, but it adds implication constraints to express local assumptions.
3. The paper explicitly says GADTs introduce local constraints that hold in one branch and not outside that branch.
4. The principal-type problem is central. The authors prefer rejecting unannotated programs without principal types over choosing arbitrary types.
5. Their algorithm is stratified: source-language-independent inference plus a solver for X. That maps well to bbnf only if bbnf names the boundary between type generation and finite-domain solver decisions.
6. The concrete X in the paper handles type classes, GADTs, and type families.
7. The paper also rejects local let-generalization in the rich local-assumption setting, thereupon a direct warning against assuming HM's let behavior survives every extension.
8. Evidence: the paper reports that the solver was implemented and distributed in GHC 7, and its implementation section separates voluminous constraint generation from a smaller but subtler solver.
9. bbnf implication: if grammar alts or host constructors introduce branch-local equalities over `V`, bbnf needs implication constraints or explicit annotations. Ordinary CSP propagation over variables is insufficient.

### S3 — Sulzmann, Schrijvers, and Stuckey 2008

1. Load-bearing claim: recent extensions to HM such as GADTs force inference beyond ordinary HM constraint solving.
2. The report uses Herbrand constraint abduction to infer missing facts as conjunctions of type equations.
3. It gives examples of GADT programs with infinitely many maximal types, showing that complete and decidable GADT inference cannot be obtained by a naïve "solve constraints" story.
4. It states that GADTs lose the property that principal typing for an expression follows compositionally from principal typings of subexpressions.
5. The method is more predictable than heuristics, but it succeeds by restricting the program set and by ruling out non-intuitive solutions.
6. Evidence: formal examples and a type-inference method over Herbrand constraints.
7. bbnf implication: if the restart uses "CSP-backed" to mean "search over all types until a solution appears," S3 is adversarial. GADT-style features can yield infinitely many incomparable maximal types; bbnf needs a restriction, annotation rule, or no-GADT claim.

### S4 — Schrijvers and Bruynooghe 2006

1. Load-bearing claim: polymorphic algebraic data type definitions and expression types can be reconstructed through a rule-based constraint rewriting algorithm.
2. This is not GADT local-equality solving. It reconstructs uniform polymorphic ADT definitions and expression/function types, including polymorphic recursion pressure.
3. The paper's value for bbnf is generic grammar typing: it shows that reconstructing type declarations is a constraint-rewriting problem, not a grammar-specific template problem.
4. Evidence: DTAI's canonical record states the algorithm reconstructs both declarations and definitions, and the paper's abstract names soundness, completeness, and optimality properties.
5. Design tradeoff: it improves rapid prototyping but works within a typed language's boundaries; bbnf should similarly infer where possible and require annotations where constraints exceed the intended language.
6. bbnf implication: generic rules may infer missing result shapes, but the restart should not conflate this with local equality constraints for GADTs.

### S5 — GHC `compiler/typecheck` source

1. Load-bearing claim: production OutsideIn-style inference uses explicit constraint structures, implication constraints, canonicalization, and careful floating/defaulting restrictions.
2. `TcRnTypes` defines `WantedConstraints` with simple constraints and implication constraints; it defines `Implication` with typechecking level, skolems, givens, wanted constraints, and evidence bindings.
3. `TcSimplify.simplifyInfer` decides quantification over variables and constraints, runs `solveWanteds`, and emits residual implications when constraints cannot be fully discharged.
4. `TcSimplify` contains the direct OutsideIn warning: while inferring most-general types, it does not float constraints out when an implication binds equality constraints, because doing so would infer non-principal types.
5. `TcCanonical` canonicalizes dictionaries, irreducibles, type equalities, and type-family equalities, showing the solver is not a generic finite-domain CSP searcher; it is a typed constraint canonicalizer and interaction engine.
6. Evidence: official generated source docs from `downloads.haskell.org`, under the old `compiler/typecheck` module names.
7. bbnf implication: an implementation note saying "CSP-backed unification" is weaker than a GHC-style type-checker contract. If bbnf needs local equalities, it must name wanted/given/evidence/implication structure or consciously avoid the feature.

### S6 — Cardelli and Wegner 1985

1. Load-bearing claim: polymorphism is not one thing. The paper distinguishes mechanisms such as overloading, coercion, subtyping/inclusion, and parametric abstraction.
2. Universal quantification models generic functions with type parameters.
3. Existential quantification and packaging model abstract data types and information hiding.
4. Bounded quantification models subtypes and inheritance.
5. The paper's generic-stack examples combine universal parameterization with existential representation hiding. This distinction matters for `Object<V>` and any proposed typed-record narrowing.
6. Evidence: ACM Computing Surveys publication record, ACM DOI, and the paper's typed-lambda-calculus development.
7. bbnf implication: `Object<V>` is parametric polymorphism. Host overloads and coercions are ad-hoc/inclusion/coercive polymorphism. Treating all of these as "CSP propagation" erases distinctions that the type checker must preserve.

### S7 — Sulzmann et al. 2007

1. Load-bearing claim: functional dependencies in Haskell-style type classes can be reformulated as Constraint Handling Rules.
2. CHRs make improvement rules explicit; e.g. a collection type can determine an element type, reducing ambiguous constraints.
3. The paper gives sufficient conditions under which functional dependencies allow sound, complete, and decidable type inference; it also shows those conditions are restrictive and can be safely relaxed in some directions.
4. Evidence: Cambridge JFP paper with DOI 10.1017/S0956796806006137 and formal CHR translation.
5. bbnf implication: if `host::primitives` has overloaded signatures where one argument determines another type, CHR-style improvement is the precise analogue. A blind CSP solver may find a value, but CHR-style rules explain why a type variable improves and when inference remains decidable.

### S8 — GHC User Guide

1. Role: support only. The implementation evidence comes from S5.
2. The guide is useful as canonical project documentation that GHC users are directed to for GADT and let-generalisation behavior.
3. bbnf implication: if the restart cites GHC's `OutsideIn(X)`, the nearest implementation-facing contract is the source plus GHC's own guide, not a tertiary summary.

### SOTA answer to the engagement question

1. HM(X): yes for the ordinary constraint-based HM layer.
2. OutsideIn(X): yes only if bbnf has GADT-like local assumptions or branch-local equality constraints.
3. Orthogonal bbnf CSP solver: yes for finite non-type choices that HM and OutsideIn do not solve by first-order unification.
4. `Object<V>` alone: HM parametric polymorphism is enough.
5. `Object<V>` plus host overloads, layout alternatives, backend erasure, rule-result narrowing, or branch-local equality assumptions: CSP or CHR-like improvement has work beyond HM.
6. `Object<V>` plus real GADT branch refinement: OutsideIn-style implications are the minimum credible shape; simple CSP propagation is not enough.

## §3 — Convergence points

### C1 — Constraint generation and solving are separate

Restart side: README says HM is the inference engine, bidirectional check/synth is the algorithmic style, and CSP backs constraint collection plus unification (`restart/README.md:260`). PASS-1 sharpens this: HM generates core constraints, bidirectional checking handles signatures and directives, and CSP solves finite choices (`restart/audit/pass-1-substrate/PASS-1.md:71`).

SOTA side: Pottier and Rémy split constraint generation from constraint solving in the ML setting (S1). OutsideIn(X) stratifies inference from the constraint domain X (S2).

Verdict: convergent, with PASS-1 wording stronger than README wording.

### C2 — Principal types remain the standard for ordinary polymorphism

Restart side: generic rules are parametric (`Object<V>`) and should be monomorphized per call site (`restart/README.md:168`-`restart/README.md:170`; `restart/README.md:264`).

SOTA side: HM and HM(X) are built around principal schemes for let-polymorphism (S1). OutsideIn(X) keeps the principal-type standard and rejects ambiguous local-assumption programs without annotations (S2).

Verdict: convergent if bbnf treats plain generics as HM schemes and records annotation requirements when principality fails.

### C3 — Local equality assumptions require a richer discipline than HM

Restart side: README explicitly points the formal deep dive toward GHC's `OutsideIn(X)` (`restart/README.md:260`). Architecture says host signatures and generic references unify inside layout lowering (`restart/ARCHITECTURE.md:872`, `restart/ARCHITECTURE.md:1127`-`restart/ARCHITECTURE.md:1130`).

SOTA side: OutsideIn(X) identifies GADTs as local-assumption generators and adds implication constraints (S2). The Herbrand-abduction report shows GADT inference can lack principal types and even finite maximal-type sets (S3).

Verdict: convergent at the citation level, under-specified at the restart algorithm level.

### C4 — Finite choices belong outside HM proper

Restart side: PASS-1 says CSP-backed constrained unification solves host overload, layout representation, recognizer eligibility, materialization, recovery, and backend plan (`restart/audit/pass-1-substrate/PASS-1.md:71`).

SOTA side: HM handles type equations and principal schemes; CHRs model improvement rules for typeclass-like dependencies; OutsideIn delegates to a solver X rather than making source inference a global search problem (S1, S2, S7).

Verdict: convergent. This is the best interpretation of bbnf's CSP-backed claim.

### C5 — Side tables match implementation practice

Restart side: optimized IR is side-table metadata (`restart/README.md:113`-`restart/README.md:114`), and `TypeFacts` are internal to `passes::layout` while `LayoutFacts` are public (`restart/ARCHITECTURE.md:975`-`restart/ARCHITECTURE.md:994`).

SOTA side: GHC carries wanted constraints, implication constraints, evidence, and canonical forms in explicit internal data structures (S5).

Verdict: convergent. The restart should keep type-solver evidence internally rich but expose stable layout facts to downstream passes.

### C6 — Grammar-authoritative genericity matches ADT reconstruction pressure

Restart side: generic crates must not hardcode grammar names (`restart/locks/LOCKS.md:60`); onboarding is grammar source plus metadata, with generated output not counted as a third authoring surface (`restart/audit/pass-1-substrate/PASS-1.md:224`-`restart/audit/pass-1-substrate/PASS-1.md:232`).

SOTA side: Schrijvers and Bruynooghe reconstruct polymorphic ADT definitions and expression types through constraints rather than hand-authored declarations (S4).

Verdict: convergent. The restart's "grammar-derived" posture is compatible with constraint-based reconstruction.

### C7 — CHR-style improvement is the right analogue for host overload dependencies

Restart side: host functions and generic primitives share the same checker (`restart/ARCHITECTURE.md:1129`), and host calls resolve through primitives or declared `@host fn` signatures (`restart/ARCHITECTURE.md:872`).

SOTA side: Sulzmann et al. use CHRs to expose functional-dependency improvement rules and prove decidable inference under conditions (S7).

Verdict: convergent if bbnf models overload dependencies explicitly, not as opaque CSP magic.

## §4 — Divergence points

### D1 — "CSP-backed unification" is too broad

Restart side: README says "CSP-backed unification" and "CSP backs the constraint-collection + unification phase" (`restart/README.md:260`).

SOTA side: HM uses unification for type equations (S1). OutsideIn uses implication constraints for local assumptions (S2). CHRs model improvement rules for class-like constraints (S7). A finite-domain CSP solver solves finite choices, not arbitrary principal type inference.

Reason: the restart compresses three mechanisms into one phrase. It should say "HM unification plus CSP-backed constrained choice."

### D2 — The restart cites GADTs without specifying a GADT surface

Restart side: README names GHC's `OutsideIn(X)` as a formal deep-dive target (`restart/README.md:260`), while Architecture §8 exposes generic parameters and type arguments but does not define branch-local equality introductions (`restart/ARCHITECTURE.md:1059`-`restart/ARCHITECTURE.md:1088`).

SOTA side: OutsideIn is needed because pattern matches and signatures introduce local assumptions (S2). S3 shows such assumptions can destroy principality.

Reason: this is an unconsidered divergence. BBNF generics do not by themselves justify GADT machinery.

### D3 — Lock 10 is mis-anchored in the research index

Restart side: Topic 3 labels Lock 10 as BBNF V1 generics `Object<V>` (`restart/research/INDEX.md:68`-`restart/research/INDEX.md:70`).

Lock side: Lock 10 actually says Pratt and SIMD are auto-detected and no `@pratt`/`@simd` directives exist (`restart/locks/LOCKS.md:52`).

SOTA side: no primary type-system source makes Pratt/SIMD auto-detection relevant to `Object<V>`.

Reason: this is a local restart catalogue divergence, not a literature disagreement. The correct genericity lock pressure is Lock 14 plus README/Architecture extension surfaces.

### D4 — Subtyping is not HM

Restart side: README says "full Hindley-Milner with subsumption" and "CSP relaxes constraints" (`restart/README.md:266`).

SOTA side: Cardelli and Wegner distinguish parametric polymorphism, inclusion polymorphism, overloading, and coercion (S6). HM by itself does not include subsumption or coercion.

Reason: principled divergence is possible, but the wording is too strong. bbnf can support coercions, but they should be a named constraint domain layered over HM, not "full HM."

### D5 — Monomorphization needs a finite-instance boundary

Restart side: generic rules monomorphize per call site (`restart/README.md:170`, `restart/README.md:264`).

SOTA side: polymorphic recursion and GADT inference can make inference undecidable or non-principal without annotations (S3, S4).

Reason: bbnf probably has a finite grammar call graph after validation, but the restart should state the finite-instance rule. Otherwise `Object<Object<V>>`, polymorphic recursion, or recursive generic rule cycles can become a silent codegen explosion.

### D6 — Pipeline order risks saying CSP solve is after inference

Restart side: Architecture pipeline lists type inference first and CSP solve later (`restart/ARCHITECTURE.md:768`-`restart/ARCHITECTURE.md:779`), while invariants say HM/bidirectional/CSP run as a subroutine inside layout lowering (`restart/ARCHITECTURE.md:796`).

SOTA side: OutsideIn-style constraint solving is part of type inference, while finite extraction choices can be a later solve (S2, S5).

Reason: the restart uses "CSP solve" for two different moments: type/layout constrained choice during inference and global extraction/optimization after egraph. They need separate names.

## §5 — Refinements to fold

### F1 — Rename README §7 "CSP-backed unification"

Target: `restart/README.md:260`.

Current text: "Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + CSP-backed unification."

Proposed text: "Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + HM unification plus CSP-backed constrained choice."

Rationale: S1 gives ordinary HM unification and principal schemes; S2 adds implication constraints for local assumptions; PASS-1 already narrows CSP to finite choices (`restart/audit/pass-1-substrate/PASS-1.md:71`).

### F2 — Add a mechanism split after README §7 first paragraph

Target: `restart/README.md:260`.

Current text: first paragraph compresses HM, bidirectional, and CSP.

Proposed text: "Mechanism split: HM unification solves first-order type equations and produces principal schemes for ordinary generic rules. OutsideIn-style implication constraints are admitted only for branch-local equality assumptions introduced by future indexed/GADT-like grammar constructs. The `csp-solver` handles finite constrained choices that HM does not model: host overload selection, layout representation, materialization mode, recognizer eligibility, recovery strategy, backend erasure, and extraction legality."

Rationale: S2 says HM(X) is not enough for local assumptions, while S7 shows improvement constraints need explicit rules. This prevents the phrase "CSP-backed" from becoming a global theorem-prover promise.

### F3 — Clarify `Object<V>` as ordinary parametric polymorphism

Target: `restart/README.md:264`.

Current text: "`Object<V> = ...`. CSP propagates type variables; codegen monomorphises per call site."

Proposed text: "`Object<V> = ...`. Plain generic rules are HM parametric schemes: `V` is generalized at the rule definition and instantiated at each call site. CSP participates only when the instantiated rule also carries finite layout, host-overload, backend-erasure, recognizer, recovery, or local-equality constraints."

Rationale: S1 and S6 distinguish parametric polymorphism from overloading/coercion. `Object<V>` alone does not need a finite-domain solver.

### F4 — Add a finite monomorphization invariant

Target: `restart/README.md:264` and `restart/ARCHITECTURE.md:1123`-`restart/ARCHITECTURE.md:1132`.

Current text: codegen monomorphizes per call site.

Proposed text: "Generic rule monomorphization is finite after validation: every `(RuleId, TypeArgs)` instance must be reachable from a concrete grammar entry or metadata-declared export; recursive generic cycles require either a decreasing structural argument, an explicit return annotation, or rejection with a generic-cycle diagnostic."

Rationale: S3 shows GADT/recursive inference can lose finite principal behavior; S4 admits polymorphic recursion pressure. The codegen budget needs an explicit finite-instance gate.

### F5 — Repair Architecture pipeline naming

Target: `restart/ARCHITECTURE.md:768`-`restart/ARCHITECTURE.md:779`.

Current text: pipeline has `type inference` and later `CSP solve`.

Proposed text: replace later `CSP solve` with `global CSP extraction solve`, and add a note: "The type/layout CSP subroutine runs inside layout lowering; this later solve handles extraction-time finite legality and optimization choices."

Rationale: S2/S5 place constraint solving inside type inference for type constraints. PASS-1 also uses CSP for finite non-type choices (`restart/audit/pass-1-substrate/PASS-1.md:71`).

### F6 — Add local-equality surface boundary

Target: `restart/ARCHITECTURE.md:1115`-`restart/ARCHITECTURE.md:1132`.

Current text: HM plus bidirectional checks and CSP constraints, with generic grammar-wide inference.

Proposed text: "No BBNF V1 construct currently introduces GADT branch-local type equalities except a future indexed-rule extension recorded by amendment. If such a construct lands, it must use OutsideIn-style implication constraints with explicit annotations where principal types are absent."

Rationale: S2 and S3 are directly adversarial to unannotated GADT inference. This keeps generics from accidentally inheriting GADT obligations.

### F7 — Add CHR/improvement vocabulary for host primitives

Target: `restart/ARCHITECTURE.md:1129` and `restart/audit/pass-1-substrate/PASS-1.md:71`.

Current text: host functions share the checker; CSP solves host overload.

Proposed text: "Host overloads with determining arguments are represented as explicit improvement constraints, CHR-shaped where applicable, before finite CSP selection runs."

Rationale: S7 provides the primary formal account for typeclass-like functional dependencies and improvement rules.

### F8 — Refine subtyping wording

Target: `restart/README.md:266`.

Current text: "Subtyping: full Hindley-Milner with subsumption."

Proposed text: "Subtyping/coercion: HM remains the core inference discipline; subsumption and coercion are explicit constraint-domain extensions checked by bidirectional mode and solved through typed improvement/coercion rules."

Rationale: S6 separates parametric polymorphism from inclusion polymorphism and coercion. This wording keeps HM's role defensible.

### F9 — Repair Topic 3 lock anchor in the research index during fold

Target: `restart/research/INDEX.md:68`-`restart/research/INDEX.md:70`.

Current text: "Lock 4 + Lock 10 (BBNF V1 generics: `Object<V>`)."

Proposed text: "Lock 4 + Lock 14, plus README/Architecture BBNF V1 generic-rule surface (`Object<V>`). Lock 10 is relevant only where recognizer/SIMD finite choices interact with type/layout CSP."

Rationale: the lock file's Lock 10 is Pratt/SIMD auto-detection (`restart/locks/LOCKS.md:52`), not generic rules. This is a restart-internal provenance fault.

## §6 — Adversarial findings

### A1 — README overstates CSP as unification

Contradicted lock or settled claim: README §7's "CSP-backed unification" (`restart/README.md:260`) and README §5/§7 "CSP propagates type variables" for generic rules (`restart/README.md:170`, `restart/README.md:264`).

SOTA evidence: S1 assigns ordinary HM inference to first-order unification and principal schemes. S2 assigns local assumptions to implication constraints over a solver X. S5 shows GHC production code uses wanted constraints, implications, canonicalization, and OutsideIn floating restrictions, not a generic finite-domain CSP as a unification replacement.

Finding: the phrase is too strong. It invites implementers to push type-variable propagation into `csp-solver`, where principality and generalization become under-specified.

Proposed amendment: adopt F1/F2/F3. Say HM unification solves type equations; CSP solves finite constrained choices after or around typed facts.

Receiving phase: Phase 2 fold into README §7, Architecture §8.2, PASS-1 §2.

Severity: high because it controls implementation architecture.

### A2 — `Object<V>` does not justify GADT/OutsideIn machinery

Contradicted lock or settled claim: Topic 3 bundles CSP-backed unification, GADTs, parametric polymorphism, and generic rules as one research surface (`restart/research/INDEX.md:66`-`restart/research/INDEX.md:70`); README says generic rules use CSP propagation (`restart/README.md:264`).

SOTA evidence: S1 and S6 classify `Object<V>` as ordinary parametric polymorphism. S2 requires OutsideIn only for local assumptions, and S3 shows why GADT inference is hard.

Finding: the restart currently risks importing a GADT problem into a plain generic-rule feature. That will add proof burden and user-visible annotation rules with no benefit for `Object<V>`.

Proposed amendment: adopt F3 and F6. Keep V1 generics HM-parametric; admit OutsideIn only when a named construct introduces local equalities.

Receiving phase: Phase 2 fold into README §5/§7 and Architecture §8.2.

Severity: medium-high because it can inflate the type-system implementation.

### A3 — GADT-style inference can lack principal and finite maximal types

Contradicted lock or settled claim: any interpretation that says CSP can simply search type assignments for GADT-like grammar branches.

SOTA evidence: S3 gives GADT programs with infinitely many maximal types and states that complete decidable inference in general is out of reach without restrictions. S2 chooses rejection or annotations for non-principal unannotated programs.

Finding: if bbnf introduces indexed grammar rules, branch-refined returns, or existential payload constructors, it must choose a restriction. "CSP-backed" is not an answer.

Proposed amendment: add an OutsideIn local-equality boundary, annotation requirement, and diagnostic.

Receiving phase: Phase 2 fold into Architecture §8.2 and PASS-1 diagnostics.

Severity: high for future GADT surface; medium for current V1 if no such surface exists.

### A4 — Lock 10 is misidentified as generics in the research index

Contradicted lock or settled claim: Topic 3 anchor row says Lock 10 is BBNF V1 generics `Object<V>` (`restart/research/INDEX.md:68`-`restart/research/INDEX.md:70`).

SOTA evidence: not a SOTA contradiction; restart-internal evidence is enough. Lock 10 is Pratt + SIMD auto-detection (`restart/locks/LOCKS.md:52`). Lock 14 is the genericity/overfitting lock (`restart/locks/LOCKS.md:60`).

Finding: the research index's anchor is stale or miscoded. It routes the generic-rule question to the recognizer lock.

Proposed amendment: adopt F9.

Receiving phase: Phase 2 fold, index repair after parallel research files are merged.

Severity: medium. It does not break the type-system claim, but it will confuse fold routing.

### A5 — Subtyping wording collapses polymorphism categories

Contradicted lock or settled claim: README's "full Hindley-Milner with subsumption" (`restart/README.md:266`).

SOTA evidence: S6 separates parametric polymorphism, overloading, coercion, and inclusion/subtyping. S7 gives a formal route for typeclass-like dependencies; S1 gives HM core.

Finding: "full HM with subsumption" is not a precise type-system name. It papers over whether bbnf has bounded quantification, coercive subtyping, row polymorphism, or a small bidirectional subsumption rule.

Proposed amendment: adopt F8.

Receiving phase: Phase 2 fold into README §7 and Architecture §8.2.

Severity: medium.

### A6 — Monomorphization lacks an explicit finiteness gate

Contradicted lock or settled claim: generic rules monomorphize per call site (`restart/README.md:170`, `restart/README.md:264`) and generated-code budgets are enforced elsewhere (`restart/audit/pass-1-substrate/PASS-1.md:242`-`restart/audit/pass-1-substrate/PASS-1.md:252`).

SOTA evidence: S3 shows infinite maximal-type pressure in GADTs; S4 notes polymorphic recursion pressure in ADT reconstruction.

Finding: without a finite `(RuleId, TypeArgs)` instance set, generic monomorphization can become a codegen budget failure.

Proposed amendment: adopt F4.

Receiving phase: Phase 2 fold into Architecture §8.2, PASS-1 validation, PASS-2 generated-code budget gates.

Severity: medium-high.

### A7 — The verified-source gap must be preserved

Contradicted lock or settled claim: research index key source row for Topic 3 lists "Schrijvers & Stuckey 2008, Polymorphic algebraic data type reconstruction" (`restart/research/INDEX.md:71`-`restart/research/INDEX.md:77`).

SOTA evidence: verified records show `Polymorphic algebraic data type reconstruction` is Schrijvers and Bruynooghe 2006 (S4). The verified 2008 Stuckey/GADT report is Sulzmann, Schrijvers, and Stuckey (S3).

Finding: the source row appears conflated. The fold must not bake the conflation into bibliography or architecture claims.

Proposed amendment: replace the row with two separate source rows.

Receiving phase: Phase 2 fold into `restart/research/INDEX.md` after all topic workers are integrated.

Severity: low-medium for architecture, high for citation hygiene.

## §7 — Surgery proposals

### Srg1 — README type-system mechanism split

Target: `restart/README.md:260`.

Surgery directive: replace the first bold sentence with the F1 text and append the F2 mechanism paragraph.

Acceptance gate: `rg -n "CSP-backed unification|HM unification plus CSP-backed constrained choice|OutsideIn-style implication" restart/README.md` shows no bare positive "CSP-backed unification" and shows the new mechanism split.

Dependency: §5 F1/F2 and §6 A1/A3.

### Srg2 — README generic-rule clarification

Target: `restart/README.md:168`-`restart/README.md:170` and `restart/README.md:264`.

Surgery directive: replace "CSP propagates type variables" with "HM generalizes and instantiates type variables; CSP participates only for finite constrained choices attached to the instantiated rule."

Acceptance gate: `rg -n "Object<V>|HM parametric|finite constrained choices|CSP propagates type variables" restart/README.md` shows `Object<V>` plus the HM/CSP split and no old propagation-only sentence.

Dependency: §5 F3 and §6 A2.

### Srg3 — Architecture local-equality boundary

Target: `restart/ARCHITECTURE.md:1115`-`restart/ARCHITECTURE.md:1132`.

Surgery directive: add a short paragraph after the type-rules table: "V1 generic rules are parametric HM schemes. No V1 construct introduces GADT branch-local type equalities. Any future indexed-rule or GADT-like construct must land by architecture amendment with OutsideIn-style implication constraints, explicit annotation rules for non-principal programs, and diagnostics."

Acceptance gate: `rg -n "branch-local type equalit|OutsideIn-style implication|V1 generic rules are parametric" restart/ARCHITECTURE.md`.

Dependency: §5 F6 and §6 A2/A3.

### Srg4 — Architecture pipeline disambiguation

Target: `restart/ARCHITECTURE.md:768`-`restart/ARCHITECTURE.md:779`.

Surgery directive: rename the later `CSP solve` step to `global CSP extraction solve`; add one sentence below the pipeline that type/layout CSP runs inside layout lowering and the later solve is extraction-time legality/optimization.

Acceptance gate: `rg -n "global CSP extraction solve|type/layout CSP subroutine" restart/ARCHITECTURE.md`.

Dependency: §5 F5 and §4 D6.

### Srg5 — PASS-1 algorithm row sharpen

Target: `restart/audit/pass-1-substrate/PASS-1.md:71`.

Surgery directive: replace "CSP-backed constrained unification solves finite choices" with "HM unification solves type equations; OutsideIn-style implication constraints are reserved for local equality assumptions; CSP-backed constrained choice solves finite host/layout/recognizer/materialization/recovery/backend alternatives."

Acceptance gate: `rg -n "HM unification solves type equations|CSP-backed constrained choice|local equality assumptions" restart/audit/pass-1-substrate/PASS-1.md`.

Dependency: §5 F1/F2/F6 and §6 A1/A3.

### Srg6 — Add generic monomorphization gate

Target: `restart/ARCHITECTURE.md:1123`-`restart/ARCHITECTURE.md:1132` and PASS-2 generated-budget gate receiver.

Surgery directive: add finite `(RuleId, TypeArgs)` instance-set validation and a generic-cycle diagnostic. Route generated LOC budget pressure to PASS-2.

Acceptance gate: a future gate command names the instance-set report and generated-code budget report; for now, docs gate is `rg -n "RuleId, TypeArgs|generic-cycle|monomorphization is finite" restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md`.

Dependency: §5 F4 and §6 A6.

### Srg7 — Host improvement constraints

Target: `restart/ARCHITECTURE.md:1129` and `restart/audit/pass-1-substrate/PASS-1.md:71`.

Surgery directive: add "improvement constraints" and "CHR-shaped where applicable" to host-overload typing.

Acceptance gate: `rg -n "improvement constraint|CHR-shaped|host overload" restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md`.

Dependency: §5 F7 and §3 C7.

### Srg8 — Subtyping wording repair

Target: `restart/README.md:266`.

Surgery directive: replace "full Hindley-Milner with subsumption" with the F8 wording.

Acceptance gate: `rg -n "full Hindley-Milner with subsumption|subsumption and coercion are explicit constraint-domain extensions" restart/README.md` shows only the new wording.

Dependency: §5 F8 and §6 A5.

### Srg9 — Research index source and lock repair

Target: `restart/research/INDEX.md:68`-`restart/research/INDEX.md:77`.

Surgery directive: after parallel research files merge, change Topic 3 anchor lock to Lock 4 + Lock 14 plus README/Architecture generic-rule surface; split the Schrijvers source into Schrijvers/Bruynooghe 2006 ADT reconstruction and Sulzmann/Schrijvers/Stuckey 2008 Herbrand GADT abduction.

Acceptance gate: `rg -n "Topic 3|Lock 14|Schrijvers.*Bruynooghe|Herbrand constraint abduction|Polymorphic algebraic data type reconstruction" restart/research/INDEX.md`.

Dependency: §6 A4/A7.

### Srg10 — Add one diagnostic for generic-cycle / local-equality boundary

Target: `restart/ARCHITECTURE.md:1008`-`restart/ARCHITECTURE.md:1031` and PASS-1 diagnostic table `restart/audit/pass-1-substrate/PASS-1.md:97`-`restart/audit/pass-1-substrate/PASS-1.md:106`.

Surgery directive: add:

```text
BBNF-GENERIC-CYCLE: generic rule {rule} produces an unbounded monomorphization set for type arguments {args}; add a return annotation, break the recursive type argument, or route the recursion through a concrete rule.
BBNF-LOCAL-EQUALITY-ANNOTATION: rule {rule} introduces branch-local type equality {equality}; add an explicit return annotation because the inferred type is not principal.
```

Acceptance gate: `rg -n "BBNF-GENERIC-CYCLE|BBNF-LOCAL-EQUALITY-ANNOTATION" restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md`.

Dependency: §6 A3/A6.

### Closing answer

1. CSP-backed unification should not mean a global CSP replacement for HM.
2. It should mean HM(X)-style constraint generation for ordinary polymorphism, OutsideIn(X)-style implication constraints only for actual local equality assumptions, and bbnf's own CSP solver for finite choices around layout, host overload, backend plan, recognizer eligibility, recovery, materialization, and extraction.
3. For `Object<V>`, HM alone supplies the type parameter discipline. CSP adds value only when the generic rule is tied to finite implementation choices or non-HM constraints.
4. The immediate fold should therefore narrow the README wording, add a local-equality boundary, add a finite monomorphization gate, and repair the Topic 3 lock/source provenance rows.
