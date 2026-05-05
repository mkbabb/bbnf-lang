# Topic 2 - Bidirectional + Pierce-Turner + Dunfield-Krishnaswami

## §1 — Settled position in the restart

### §1.1 Contract and topic authority

1. `restart/research/INDEX.md:18-24` requires this artefact to carry sections §1 through §7, with §1 rendering settled restart claims verbatim or near-verbatim and citing path:line evidence.
2. `restart/research/INDEX.md:26-34` requires convergence, divergence, fold refinements, adversarial findings, and concrete surgery proposals to cite restart claims and primary-source evidence.
3. `restart/research/INDEX.md:52-56` names the topic as "Bidirectional + Pierce-Turner + Dunfield-Krishnaswami" and asks whether restart bidirectionality is Pierce-Turner local inference, Dunfield-Krishnaswami higher-rank completeness, or both.
4. `restart/research/INDEX.md:57-64` names the expected primary-source set: Pierce & Turner, Dunfield & Krishnaswami 2013, Dunfield & Krishnaswami 2019, Norman Ramsey, Idris 2, Roc, and Herbelin/Lemay if relevant.
5. `restart/research/INDEX.md:149-153` requires at least one adversarial pressure point, even where the research mostly converges with the restart.
6. `restart/research/INDEX.md:155-158` fixes voice and discipline: direct prose, archaic-permissive diction where apt, path:line citations, no placeholder wording, no quick solutions.

### §1.2 Gestalt and type-system claim

7. `restart/README.md:5` says the internals include "CSP + e-graph + shape mining + cost model + bidirectional inference + grammar-derived everything."
8. `restart/README.md:155` says `@host fn` has closure semantics, lexical scoping, type-variable propagation through bidirectional inference, and compile-time checking against generic primitives.
9. `restart/README.md:161` says multi-function chaining extends terminal-side `-> Type` with first-class bidirectional inference; types flow through each stage; CSP backs constraint collection.
10. `restart/README.md:170` says generic rules carry type variables, CSP propagates them, and codegen monomorphises per call site.
11. `restart/README.md:178` says `@layout` is an optional override when type inference is ambiguous; inference is default.
12. `restart/README.md:195` places "Type inference (CSP + bidirectional + Hindley-Milner)" as pipeline step 3, producing `TypedGrammarIR`.
13. `restart/README.md:260` states the central claim: "Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + CSP-backed unification."
14. `restart/README.md:260` further states: "The three are composed, not exclusive - Hindley-Milner is the inference engine; bidirectional check/synth is the algorithmic style at each grammar node; CSP backs the constraint-collection + unification phase."
15. `restart/README.md:260` says PASS-1's type-system sub-agent must do the formal deep dive and cite "Dunfield-Krishnaswami's bidirectional papers" and "Pierce-Turner's local inference."
16. `restart/README.md:262` states the annotation surface is hybrid: pure inference by default, explicit annotations welcome, and chains check bidirectionally at each stage.
17. `restart/README.md:264` states generic rules are V1 and are monomorphised per call site.
18. `restart/README.md:266` states "Subtyping: full Hindley-Milner with subsumption."
19. `restart/README.md:266` also states "CSP relaxes constraints; coercion is a constraint relaxation."
20. `restart/README.md:266` gives three coercion examples: numeric widening, lifetime coercion, and typed-record narrowing.
21. `restart/README.md:268` states lookbehind's left operand is a constraint on context, not a capture of value.

### §1.3 Lock and layout authority

22. `restart/locks/14-LOCKS.md:36` states Lock 2: layout lowering is the canonical public pass name; HM/CSP type checking is a subroutine of layout lowering, never a public peer pass.
23. `restart/locks/14-LOCKS.md:40` states Lock 4: CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, and cost model compose by output-piping; no unified hypergraph.
24. `restart/locks/14-LOCKS.md:40` says any plan fusing CSP and e-graph into one solver is a fault.
25. `restart/locks/14-LOCKS.md:52` states Lock 10: Pratt and SIMD are auto-detected; no `@pratt` or `@simd` directives survive.
26. `restart/locks/14-LOCKS.md:60` states Lock 14: generic crates carry zero grammar-specific code; grammars enter through source plus workspace metadata, with rare fenced declaration crates only.
27. `restart/research/INDEX.md:54` labels Topic 2's anchor lock as "Lock 4."
28. `restart/research/INDEX.md:54` and `restart/locks/14-LOCKS.md:40` now point to different meanings: the research index calls Lock 4 a type-stack lock, while the current lock text defines per-domain optimization.

### §1.4 Architecture authority

29. `restart/ARCHITECTURE.md:796` says type inference annotates Grammar IR and does not mutate grammar syntax; HM + bidirectional + CSP run inside `passes::layout`.
30. `restart/ARCHITECTURE.md:872` says host calls resolve through generic primitives or declared `@host fn` signatures and unify inside layout lowering.
31. `restart/ARCHITECTURE.md:975-983` says optimized IR is side-table data; `TypeFacts` is internal to `passes::layout`; downstream passes read `LayoutFacts`.
32. `restart/ARCHITECTURE.md:987` says `LayoutFacts` folds HM + bidirectional + CSP into layout decisions.
33. `restart/ARCHITECTURE.md:991` says `CspSolution` may be public for cost extraction but internal inside layout lowering.
34. `restart/ARCHITECTURE.md:994` says `TypeFacts` is produced by the HM + bidirectional checker and consumed only by `passes::layout`.
35. `restart/ARCHITECTURE.md:1047-1051` says BBNF supports lookbehind, block-bodied `@host fn`, multi-function chaining, generics, `@error(recover = ...)`, and `@layout`; rewrite-mode and grammar-level Unicode class algebra are out.
36. `restart/ARCHITECTURE.md:1087-1088` defines rule-level chains as `MapTail ::= "->" ChainExpr` and `ChainExpr ::= Ident ("->" Ident)*`.
37. `restart/ARCHITECTURE.md:1095-1101` says rule-level chains use `Expr -> f1 -> f2`, method-chain syntax is legal only inside `HostFn` bodies, and bodyless host declarations have no production.
38. `restart/ARCHITECTURE.md:1117-1121` says the type system is Hindley-Milner plus bidirectional checks and CSP constraints, with README and PASS-1 as authority.
39. `restart/ARCHITECTURE.md:1127` says inference is grammar-wide, host calls unify inside `passes::layout`, downstream passes consume `LayoutFacts`, and `TypeFacts` is not exported.
40. `restart/ARCHITECTURE.md:1128` says annotations narrow inferred types; contradictions are diagnostics.
41. `restart/ARCHITECTURE.md:1129` says block-bodied `@host fn` definitions and generic primitives share the same checker.
42. `restart/ARCHITECTURE.md:1130` says chains compose left-to-right: output of one host call is input to the next.
43. `restart/ARCHITECTURE.md:1131` says lookbehind must be bounded.
44. `restart/ARCHITECTURE.md:1132` says layout and error directives are typed side effects that produce facts, not ad hoc codegen flags.
45. `restart/ARCHITECTURE.md:1136-1140` says host functions decompose through generic primitives, workspace metadata, and block-bodied `@host fn` definitions.
46. `restart/ARCHITECTURE.md:1154-1157` says host chain closures capture previous host result and explicit args; segment N output unifies with segment N+1 input; chains lower to `HostChain`.

### §1.5 PASS-1 and master-plan authority

47. `restart/audit/pass-1-substrate/PASS-1.md:12` says the type-system decision is KEEP HM + bidirectional + CSP-backed constrained unification.
48. `restart/audit/pass-1-substrate/PASS-1.md:18` says host functions, chains, and generics are KEEP.
49. `restart/audit/pass-1-substrate/PASS-1.md:35` puts chain steps, argument ids, and expected type into `Map` / `HostCall` payloads.
50. `restart/audit/pass-1-substrate/PASS-1.md:51` says the host/layout/error family carries typed host chains and layout facts.
51. `restart/audit/pass-1-substrate/PASS-1.md:71` says "HM inference generates core constraints; bidirectional checking handles explicit signatures/directives; CSP-backed constrained unification solves finite choices."
52. `restart/audit/pass-1-substrate/PASS-1.md:71` names finite choices: host overload, layout representation, recognizer eligibility, direct/tape materialization, recovery strategy, and backend plan.
53. `restart/audit/pass-1-substrate/PASS-1.md:73` says e-graph does equivalence and rewrite saturation; CSP does finite legality/choice; cost scores legal alternatives.
54. `restart/audit/pass-1-substrate/PASS-1.md:77` says lookbehind, `@host fn`, chains, generics, `@error`, and `@layout` are first-class.
55. `restart/audit/pass-1-substrate/PASS-1.md:101-106` owns diagnostics for lookbehind width, host signature mismatch, layout conflict, chain-step type failure, Pratt rejection, and SIMD rejection.
56. `restart/audit/pass-1-substrate/PASS-1.md:110` says method chains desugar to nested typed host/map calls with preserved spans and chain-step metadata.
57. `restart/audit/pass-1-substrate/PASS-1.md:121` still lists both `types/` and `layout/` under `passes`, which matters because later authority makes `passes::layout` public.
58. `restart/audit/pass-1-substrate/PASS-1.md:140` explains `types/` as HM + bidirectional checking and `layout/` as `@layout` lowering and layout-fact production.
59. `restart/audit/pass-1-substrate/PASS-1.md:179-220` gives the formal BBNF grammar and chain-step type-flow rule.
60. `restart/audit/pass-1-substrate/PASS-1.md:220` says the sole rule-level chain form is `Expr -> f1 -> f2 -> f3`.
61. `restart/audit/pass-1-substrate/PASS-1.md:220` says each `fi` is a typed function reference resolvable as a grammar `Map` step or `@host fn`.
62. `restart/audit/pass-1-substrate/PASS-1.md:220` says type flow threads each step's output type into the next step's first argument.
63. `restart/audit/pass-1-substrate/PASS-1.md:220` says type checking runs left-to-right and fails at the first mismatch with `BBNF1401`.
64. `restart/MASTER-PLAN.md:303-305` says Tranche C inherits the README two-IR decision, PASS-1 IR/type commitments, and Lock 4's output-piped bridge crates.
65. `restart/MASTER-PLAN.md:312-317` says C.W0-C.W5 create Grammar IR, layout/type facts, recognizer facts, CSP/egraph bridge tables, and cost facts.
66. `restart/MASTER-PLAN.md:313` says C.W1 implements HM + bidirectional + CSP type-checking as a subroutine inside `passes::layout`, with public `LayoutFacts` and internal `TypeFacts`.
67. `restart/MASTER-PLAN.md:337-349` says Tranche D implements the settled BBNF extension surface, including generics, block-bodied host functions, and multi-function chaining.
68. `restart/MASTER-PLAN.md:348` says D.W3 owns the multi-function chaining type/runtime contract and requires chain results to feed later parser expressions.

### §1.6 Carry baseline and hardening authority

69. `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:104-112` returns V4 READY with all V1 cross-target conflicts closed.
70. `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:116-118` says the amended docs preserve voice, path:line citations, no stale TBD without receivers, and no soft hedging.
71. `restart/audit/hardening/HARDENING-PASS-1-V5.md:13-15` says V5 found PASS-1 internally strong but stricter about cross-document surfaces.
72. `restart/audit/hardening/HARDENING-PASS-1-V5.md:24` says PASS-1 owns the precise BBNF grammar surface and Architecture previously contradicted it.
73. `restart/audit/hardening/HARDENING-PASS-1-V5.md:45` records a remaining test-design gap: generic host/rule typing under chain flow is described but not stress-tested under recovery values and host overload selection.
74. `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:49-53` says V5.1 closed the formal grammar reconciliation: block-bodied `@host fn`, infix lookbehind, and `->` chains are current.
75. `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:76-87` says the grammar sketch now requires a host block, keeps rule mapping behind `MapTail`, and scopes method chains to `HostFn` bodies.

### §1.7 Settled interpretation for this report

76. The restart currently uses `bidirectional` in the Pierce-Turner check/synth sense at grammar nodes: `restart/README.md:260`, `restart/README.md:262`, `restart/ARCHITECTURE.md:1117-1132`, and `restart/audit/pass-1-substrate/PASS-1.md:71`.
77. The restart also cites Dunfield-Krishnaswami as the proof-obligation source for the formal deep dive: `restart/README.md:260` and `restart/research/INDEX.md:57-60`.
78. The current restart text does not yet commit to a Dunfield-Krishnaswami higher-rank surface in BBNF source; generic rules are rank-1 `Object<V>`-style grammar parameters per `restart/README.md:170` and `restart/README.md:264`.
79. "Pierce-Turner-style" currently commits to check/synth, local expected-type propagation, explicit annotations as control points, and subsumption/coercion gates; it does not commit to PT's full local type-argument synthesis algorithm unless the fold makes that explicit.
80. "Dunfield-Krishnaswami-style" is a candidate proof discipline for future higher-rank or existential/indexed surfaces; present restart text mentions it as research/proof obligation rather than implementation commitment.

## §2 — SOTA literature deep-dive

### §2.1 Primary-source ledger

81. [S1] Benjamin C. Pierce and David N. Turner, "Local Type Inference," POPL 1998 / TOPLAS revised version, official PDF: <https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf>.
82. [S2] Jana Dunfield and Neelakantan R. Krishnaswami, "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism," ICFP 2013, official author page and PDF: <https://research.cs.queensu.ca/home/jana/papers/bidir/> and <https://www.cl.cam.ac.uk/~nk480/bidir.pdf>.
83. [S3] Jana Dunfield and Neelakantan R. Krishnaswami, "Sound and Complete Bidirectional Typechecking for Higher-Rank Polymorphism with Existentials and Indexed Types," arXiv/PACMPL primary page: <https://arxiv.org/abs/1601.05106>.
84. [S4] Idris 2 official source, commit `214eb45472e15187e6f932c6820a0f0d5542a18e`: `src/Core/Unify.idr` and `src/TTImp/Elab/Check.idr`, e.g. <https://github.com/idris-lang/Idris2/blob/214eb45472e15187e6f932c6820a0f0d5542a18e/src/Core/Unify.idr>.
85. [S5] Roc official compiler source, commit `ee0fc49dc6a77410c561711da3dff89c4adecf2c`: compiler README, `can/src/constraint.rs`, `solve/src/module.rs`, `solve/src/solve.rs`, and `unify/src/unify.rs`, e.g. <https://github.com/roc-lang/roc/tree/ee0fc49dc6a77410c561711da3dff89c4adecf2c/crates/compiler>.
86. [G1] Norman Ramsey provenance gap: the official supplement at <https://www.build-prove-compare.net/supplement-2022-10-11.pdf> is accessible, but this pass did not verify a distinct bidirectional implementation in the text/source. No Ramsey claim is used as evidence below.
87. [G2] Herbelin/Lemay provenance gap: no primary Herbelin/Lemay bidirectional subtype-coercion note was verified in this pass. No Herbelin/Lemay claim is used as evidence below.

### §2.2 Pierce & Turner 1998 / TOPLAS local type inference

88. [S1] studies two partial type-inference methods for a language with subtyping and impredicative polymorphism.
89. [S1] defines "local" strictly: omitted annotations are recovered from adjacent syntax-tree nodes, without long-distance constraints.
90. [S1] is not a Hindley-Milner completeness paper; it explicitly trades complete inference for simpler methods that tolerate subtyping and impredicative polymorphism.
91. [S1] separates external language from internal language: the programmer-facing language may omit annotations, while the internal language carries explicit annotations and type applications.
92. [S1] has two core mechanisms: local synthesis of type arguments and bidirectional checking.
93. [S1] local type-argument synthesis compares actual argument types with expected parameter types and chooses type arguments satisfying local subtyping constraints.
94. [S1] bidirectional checking uses synthesis mode where type information flows upward and checking mode where expected type information flows downward.
95. [S1] says unannotated function abstractions cannot synthesize under only local information, but can be checked when an expected function type is available.
96. [S1] application synthesis infers the function type, then checks arguments against parameter types.
97. [S1] checking mode performs a final subtype check between actual and expected result type.
98. [S1] omits the ordinary subsumption rule from many algorithmic presentations and folds the subtype check into directed check/synth transitions.
99. [S1] therefore supports the restart's "subsumption" vocabulary only if the implementation specifies exactly where subsumption is invoked.
100. [S1] gives local completeness only relative to its local synthesis specification; it does not give global type inference completeness.
101. [S1] is a strong match for BBNF chains: each chain segment has an adjacent producer and consumer type, so local expected-type propagation is enough for many host-chain errors.
102. [S1] is a weak match for unrestricted grammar-wide inference: it avoids long-distance unification and expects annotations where local context is insufficient.

### §2.3 Dunfield & Krishnaswami 2013 higher-rank bidirectionality

103. [S2] defines bidirectional typechecking as two modes: terms synthesize a type or are checked against a known type.
104. [S2] motivates bidirectionality by scalability, error reporting, and ease of implementation, with decidability for expressive type systems where Damas-Milner inference is not enough.
105. [S2] is specifically about predicative higher-rank polymorphism.
106. [S2] gives a declarative bidirectional account, then an algorithm that is sound and complete with respect to that account.
107. [S2] does not simply reuse Pierce-Turner local inference; it adds an application judgment and ordered algorithmic contexts with existential type variables.
108. [S2] uses subtyping to model polymorphic instantiation and confines subsumption to a controlled transition from synthesis to checking.
109. [S2] treats explicit type annotations as essential bridges between checking and synthesis.
110. [S2] says completeness lets programmers avoid explicit type applications and predicts where annotations are needed.
111. [S2] omits special let-generalization; let bindings can be restored without generalization, and polymorphic bindings need annotations.
112. [S2] requires ordered contexts, context extension, existential solving, and proof obligations for decidability, soundness, and completeness.
113. [S2] is relevant to BBNF only where BBNF actually needs higher-rank polymorphism, first-class polymorphic functions, or type-level instantiation under arrows.
114. [S2] is not needed to justify ordinary rank-1 generic grammar rules such as `Object<V>`.

### §2.4 Dunfield & Krishnaswami 2019 indexed types and existentials

115. [S3] extends the 2013 programme to higher-rank polymorphism with first-class existential types and indexed types/GADTs.
116. [S3] frames GADTs through existentials and equality constraints.
117. [S3] uses focalization and polarized subtyping; universals and existentials are treated differently by polarity.
118. [S3] uses bidirectional typechecking and principality tracking to state when annotations are required.
119. [S3] says full type inference for definitions using GADTs requires polymorphic recursion and is undecidable; mandatory annotations remain.
120. [S3] supports nested pattern matching and coverage checking for GADTs, with principality conditions on scrutinees.
121. [S3] gives algorithmic typing rules and proves decidability, determinism, soundness, and completeness.
122. [S3] emphasizes that soundness and completeness proofs become mutually recursive because of the richer type system.
123. [S3] is a useful warning for bbnf generics: as soon as generic grammar rules become indexed datatypes or GADT-like refinements, annotations and principality gates become part of the contract.
124. [S3] is currently more than the restart needs for `Object<V>`, host chains, and layout hints.

### §2.5 Idris 2 elaborator and unifier

125. [S4] `src/Core/Unify.idr` defines a unifier returning new constraints, whether holes were solved, and lazy/force information.
126. [S4] `src/Core/Unify.idr` exposes `unify` and `unifyWithLazy` over terms and environments, which is dependent-type unification rather than HM alone.
127. [S4] `src/Core/Unify.idr` implements occurs-check machinery and metavariable solving, including cyclic-meta failure.
128. [S4] `src/Core/Unify.idr` postpones equations when conversion or metas cannot be resolved immediately.
129. [S4] `src/TTImp/Elab/Check.idr` defines the main checker interface used by construct checkers.
130. [S4] `src/TTImp/Elab/Check.idr` carries an expected type through `checkExp`; if an expected type exists, it calls conversion/unification against the inferred type.
131. [S4] shows production elaboration as a cooperation among expected types, implicit arguments, holes/metas, postponement, and unification.
132. [S4] is relevant to bbnf as a high-end dependent elaboration reference, not as a direct implementation template for rank-1 grammar typing.
133. [S4] presses against a too-simple "CSP backs unification" story: production elaborators have postponement, meta solving, and conversion layers that need precise diagnostic ownership.

### §2.6 Roc compiler type checking

134. [S5] `crates/compiler/README.md` lists compiler phases and identifies type checking as `solve/src/module.rs: run_solve`.
135. [S5] the same README tells contributors debugging typechecking errors to inspect `solve_expr` and `ROC_PRINT_UNIFICATIONS`.
136. [S5] `can/src/constraint.rs` defines a `Constraint` enum with equality, lookup, let, pattern, and other constraint forms.
137. [S5] `solve/src/module.rs` wraps `Subs` in `Solved<Subs>` and says the only way to obtain it is by running the solver.
138. [S5] `solve/src/solve.rs` states the type checker processes constraints, uses unification, and accepts the program if no unification errors occur.
139. [S5] `solve/src/solve.rs` tracks ranks for let-generalization, with variables under deeper lets generalized only if they do not escape.
140. [S5] `unify/src/unify.rs` returns `Unified::Success` or `Unified::Failure`, carrying ability obligations and lambda-set specialization work.
141. [S5] uses constraints and unification as the production type-checking center; this is relevant to bbnf's CSP-backed constrained unification.
142. [S5] does not verify the research index's phrase "modern Rust-implemented bidirectional" as a source claim. Roc is best cited here as constraint/unification production evidence.

### §2.7 Source synthesis

143. [S1] is the source for Pierce-Turner-style local bidirectional checking with subtyping.
144. [S2] is the source for algorithmic completeness for higher-rank polymorphism.
145. [S3] is the source for higher-rank plus existentials/indexed types, principality, and GADT pressure.
146. [S4] is the source for production dependent elaboration with holes, metas, postponement, and expected-type checks.
147. [S5] is the source for production constraint/unification architecture in a Rust compiler.
148. [G1] and [G2] stay gaps, not evidence.
149. The restart may use both Pierce-Turner and Dunfield-Krishnaswami, but the uses occupy different layers.
150. Pierce-Turner names the local operational algorithmic posture.
151. Dunfield-Krishnaswami names a proof discipline for higher-rank completeness if bbnf admits that surface.
152. Roc and Idris 2 show implementation pressure: practical solvers need phase ownership, postponed work, and diagnostic structure.

## §3 — Convergence points

153. C1. Check/synth mode converges with Pierce-Turner.
154. Restart claim: bidirectional check/synth is the algorithmic style at each grammar node (`restart/README.md:260`).
155. SOTA evidence: [S1] splits synthesis mode and checking mode, with upward and downward type information.
156. Match: bbnf chains and host calls naturally fit mode selection because each chain step has an adjacent input and output type.
157. Fold note: keep "check/synth" explicit everywhere; avoid using `bidirectional` as a free-standing adjective.

158. C2. Local expected-type propagation converges with chains.
159. Restart claim: chain type flow threads previous step output into next step input (`restart/audit/pass-1-substrate/PASS-1.md:220`).
160. SOTA evidence: [S1] checks unannotated lambdas only when the expected function type is available, and applications use inferred function types to check arguments.
161. Match: rule-level `Expr -> f1 -> f2` is exactly the kind of local adjacent context Pierce-Turner is built to exploit.
162. Fold note: chain diagnostics should show "produced T, expected U" at the adjacent edge.

163. C3. Controlled subsumption converges with Pierce-Turner and DK.
164. Restart claim: subtyping includes subsumption and coercion (`restart/README.md:266`).
165. SOTA evidence: [S1] and [S2] both control subsumption through directed check/synth transitions rather than a global arbitrary rule.
166. Match: bbnf can keep numeric, lifetime, and record coercion only where a checking edge has an expected type.
167. Fold note: "subsumption" must name a rule site.

168. C4. Explicit annotations as control points converge with all three papers.
169. Restart claim: pure inference is default; explicit annotations are welcome where the author wants control (`restart/README.md:262`).
170. SOTA evidence: [S1] infers only local omissions; [S2] says annotations mediate check/synth; [S3] requires annotations for GADT/polymorphic recursion cases.
171. Match: `rule -> Type`, generic parameters, and `@layout` hints are legitimate author control surfaces.
172. Fold note: error messages should tell authors when an annotation is the intended remedy.

173. C5. Public `LayoutFacts`, internal `TypeFacts` converges with production compilers.
174. Restart claim: `TypeFacts` is internal to layout lowering and downstream passes read `LayoutFacts` (`restart/ARCHITECTURE.md:975-994`).
175. SOTA evidence: [S5] separates canonicalization, constraint creation, solve, specialization, and codegen; [S4] separates elaboration state and unification work.
176. Match: bbnf should keep type solving as an internal producer of layout and diagnostic facts.
177. Fold note: no consumer outside `passes::layout` should depend on internal type variables.

178. C6. CSP/e-graph separation survives this topic.
179. Restart claim: Lock 4 says CSP, e-graph, miners, shape analysis, and cost model compose by output-piping (`restart/locks/14-LOCKS.md:40`).
180. SOTA evidence: [S1], [S2], [S3], [S4], and [S5] all distinguish typing/unification from other compiler tasks; none argues for a fused e-graph/CSP hypergraph.
181. Match: type constraints can feed recognizer and cost decisions as facts.
182. Fold note: this topic does not contradict actual Lock 4; it clarifies type-checking facts entering that bridge.

183. C7. Higher-rank papers are relevant as proof obligations.
184. Restart claim: PASS-1 must commit to a specific algorithm and formal proof obligations with DK citations (`restart/README.md:260`).
185. SOTA evidence: [S2] and [S3] show proof obligations around ordered contexts, existentials, principality, soundness, completeness, and decidability.
186. Match: if bbnf admits higher-rank host functions, first-class polymorphic host values, or indexed grammar types, DK becomes the right proof source.
187. Fold note: until that surface exists, cite DK as a guardrail, not as current implementation proof.

188. C8. Idris 2 validates postponement as a real implementation concern.
189. Restart claim: CSP-backed constrained unification solves finite choices and host overloads (`restart/audit/pass-1-substrate/PASS-1.md:71`).
190. SOTA evidence: [S4] postpones unification when holes or conversion cannot settle immediately.
191. Match: bbnf can start with finite CSP, but must still assign ownership for deferred host overload and recovery-placeholder constraints.
192. Fold note: add a "postponed type obligation" diagnostic or internal state if finite CSP choices cannot settle in a single pass.

## §4 — Divergence points

193. D1. The restart overuses "full Hindley-Milner with subsumption."
194. Restart claim: `restart/README.md:266` says "Subtyping: full Hindley-Milner with subsumption."
195. SOTA pressure: [S1] explicitly avoids full type inference for the combination of subtyping and impredicative polymorphism; [S2] avoids standard HM let-generalization for higher-rank completeness.
196. Divergence reason: bbnf likely means "HM core constraints plus directed subsumption gates," not full HM plus unrestricted subtyping.
197. Phase 2 should replace the phrase with a scoped rule.

198. D2. The research index's lock label drifts from the current locks file.
199. Restart claim: Topic 2 says anchor lock is "Lock 4" and describes a type-stack lock (`restart/research/INDEX.md:54`).
200. Current lock: `restart/locks/14-LOCKS.md:40` defines Lock 4 as per-domain orthogonal optimization.
201. Divergence reason: the topic catalogue likely retained an older lock label.
202. Phase 2 should amend the research index or add a note that Topic 2 engages README §7 plus Lock 2 and actual Lock 4.

203. D3. DK completeness is not committed by current BBNF syntax.
204. Restart claim: `restart/README.md:260` cites Dunfield-Krishnaswami papers for the formal deep dive.
205. SOTA pressure: [S2] completeness depends on ordered existential contexts; [S3] adds principality and polarized subtyping for indexed/existential types.
206. Divergence reason: current BBNF examples are rank-1 grammar generics and host-chain typing, not higher-rank terms.
207. Phase 2 should say DK is a conditional proof path for future higher-rank or indexed features.

208. D4. Roc is not verified as bidirectional evidence.
209. Restart claim: `restart/research/INDEX.md:63` calls Roc a modern Rust-implemented bidirectional source with type aliases and records.
210. Source pressure: [S5] verifies constraints, solve, ranks, and unification; this pass did not verify check/synth bidirectional mode in Roc's source.
211. Divergence reason: Roc remains relevant to constraints and unification, but not as evidence for Pierce-Turner or DK bidirectionality.
212. Phase 2 should recast Roc as "production constraint/unification reference" unless a Roc bidirectional source path is later verified.

213. D5. Idris 2 is stronger than the restart's initial need.
214. Restart claim: `restart/research/INDEX.md:62` asks for Idris 2 elaborator/unifier as bidirectional + dependent-type integration.
215. SOTA pressure: [S4] includes metas, holes, conversion, lazy/force handling, and postponed constraints.
216. Divergence reason: BBNF V1 does not need dependent elaboration, but Idris 2 reveals what happens if `@host fn` grows implicit arguments or dependent indices.
217. Phase 2 should cite Idris 2 only for elaborator architecture hazards, not as the baseline algorithm.

218. D6. PASS-1's `types/` child conflicts mildly with layout-lowering public vocabulary.
219. Restart claim: `restart/audit/pass-1-substrate/PASS-1.md:121` lists both `types/` and `layout/`; `restart/audit/pass-1-substrate/PASS-1.md:140` says `types/` owns HM + bidirectional checking.
220. Later authority: `restart/locks/14-LOCKS.md:36` and `restart/ARCHITECTURE.md:975-994` make `passes::layout` the public owner and `TypeFacts` internal.
221. Divergence reason: hardening accepted this as mostly closed, but topic 2's wording should avoid re-publicising `passes::types`.
222. Phase 2 should add one sentence that `types/` is an internal module under layout or a private submodule, if retained.

223. D7. "CSP relaxes constraints" is too informal.
224. Restart claim: `restart/README.md:266` says CSP relaxes constraints and coercion is constraint relaxation.
225. SOTA pressure: [S1] has explicit lower/upper subtyping constraints; [S2] has ordered existential instantiation; [S5] has concrete constraint variants and unification outcomes.
226. Divergence reason: "relaxation" hides whether the system solves inequalities, inserts coercions, or selects an overload.
227. Phase 2 should require a `SubsumptionEdge` / `CoercionCandidate` fact with producer and diagnostic site.

228. D8. Higher-rank and GADT warnings are missing from the BBNF generics story.
229. Restart claim: `restart/README.md:170` and `restart/README.md:264` accept generic rules V1.
230. SOTA pressure: [S3] says GADT definitions and polymorphic recursion require annotations and principality gates.
231. Divergence reason: `Object<V>` is harmless rank-1 genericity; indexed grammar families would cross into DK19 territory.
232. Phase 2 should add a guardrail: generic rules are rank-1 unless a later tranche admits indexed/higher-rank grammar types.

## §5 — Refinements to fold

233. R1 target: `restart/README.md:260`.
234. R1 current text: "Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + CSP-backed unification."
235. R1 proposed text: "Hindley-Milner core constraints + bidirectional check/synth with explicit subsumption gates in Pierce-Turner local-inference style + CSP-backed constrained unification."
236. R1 rationale: [S1] makes local check/synth and local subtyping constraints the Pierce-Turner commitment; this wording avoids implying unrestricted HM plus subtyping.

237. R2 target: `restart/README.md:260`.
238. R2 current text: "Dunfield-Krishnaswami's bidirectional papers."
239. R2 proposed text: "Dunfield-Krishnaswami's higher-rank bidirectional papers, used as conditional proof obligations if BBNF admits higher-rank, existential, or indexed type surfaces."
240. R2 rationale: [S2] and [S3] are about higher-rank completeness and indexed/existential types, not ordinary rank-1 chain typing.

241. R3 target: `restart/README.md:266`.
242. R3 current text: "Subtyping: full Hindley-Milner with subsumption."
243. R3 proposed text: "Subtyping: directed subsumption over HM-derived types at checking edges; coercions are explicit solver candidates with diagnostics."
244. R3 rationale: [S1] and [S2] control subsumption through check/synth transitions; [S5] shows production solvers need explicit outcomes.

245. R4 target: `restart/ARCHITECTURE.md:1117-1121`.
246. R4 current text: "The type system is Hindley-Milner plus bidirectional checks and CSP constraints."
247. R4 proposed text: "The V1 type system is rank-1 Hindley-Milner core inference plus bidirectional checks and CSP constraints; higher-rank or indexed surfaces require a separate DK-style proof gate."
248. R4 rationale: [S2] and [S3] require ordered existential contexts, principality, and completeness proofs not yet named in Architecture §8.2.

249. R5 target: `restart/ARCHITECTURE.md:1128`.
250. R5 current text: "Annotations narrow, not bypass, inferred types."
251. R5 proposed text: "Annotations narrow inferred types and select checking mode; contradictions are diagnostics, and polymorphic annotations are mandatory for any future higher-rank or indexed surface."
252. R5 rationale: [S1] uses annotations as local inference boundaries; [S2] and [S3] require annotations at polymorphic or GADT-sensitive boundaries.

253. R6 target: `restart/audit/pass-1-substrate/PASS-1.md:71`.
254. R6 current text: "bidirectional checking handles explicit signatures/directives."
255. R6 proposed text: "bidirectional checking handles explicit signatures/directives and every adjacent chain/subsumption edge; it is Pierce-Turner local inference unless a separate DK higher-rank gate is opened."
256. R6 rationale: This answers the engagement question directly and prevents future workers from treating `bidirectional` as a single undifferentiated source family.

257. R7 target: `restart/audit/pass-1-substrate/PASS-1.md:101-106`.
258. R7 current text: diagnostics include `BBNF1401` for chain-step type failure.
259. R7 proposed text: add a companion diagnostic alias `BBNF-HOST004` / `BBNF-SUBSUMPTION-EDGE`: "chain step {step} requires coercion {from} -> {to}, but no directed subsumption rule is registered at this edge."
260. R7 rationale: [S1] and [S2] make subsumption rule placement load-bearing; README currently gives coercion examples without a rule-site diagnostic.

261. R8 target: `restart/MASTER-PLAN.md:313`.
262. R8 current text: "HM + bidirectional + CSP type-checking subroutine inside `passes::layout`."
263. R8 proposed text: "HM core constraints + Pierce-Turner local check/synth + CSP type-checking subroutine inside `passes::layout`; DK higher-rank proof gate remains closed for V1 rank-1 grammar generics."
264. R8 rationale: C.W1 should implement the committed V1 surface, not a general higher-rank solver by accident.

265. R9 target: `restart/research/INDEX.md:54`.
266. R9 current text: "Anchor locks: Lock 4."
267. R9 proposed text: "Anchor surfaces: README §7, Lock 2 (`passes::layout`), actual Lock 4 (per-domain output-piped CSP/egraph composition), and PASS-1 §3 chain-step type-flow rule."
268. R9 rationale: The current Lock 4 line is a numbering drift relative to `restart/locks/14-LOCKS.md:40`.

269. R10 target: `restart/research/INDEX.md:63`.
270. R10 current text: "Roc's type checker ... modern Rust-implemented bidirectional..."
271. R10 proposed text: "Roc's type checker ... modern Rust-implemented constraint/unification reference; verify bidirectional check/synth before citing it as bidirectional evidence."
272. R10 rationale: [S5] verifies constraints and unification, not a Pierce-Turner or DK check/synth implementation in this pass.

273. R11 target: `restart/audit/pass-1-substrate/PASS-1.md:220`.
274. R11 current text: type checking runs left-to-right and fails at first mismatch.
275. R11 proposed text: append "A step may pass through directed subsumption only if a registered coercion candidate exists at that edge; otherwise the failure is `BBNF1401` plus `BBNF-SUBSUMPTION-EDGE`."
276. R11 rationale: Prevents informal coercion towers and binds README coercion examples to diagnostics.

277. R12 target: new paragraph after `restart/ARCHITECTURE.md:1132`.
278. R12 current text: no explicit higher-rank guardrail.
279. R12 proposed text: "Higher-rank, existential, or indexed grammar types are out of V1 unless a later tranche opens a DK-style algorithmic-completeness gate with ordered existential contexts, principality tracking, decidability, soundness, and completeness evidence."
280. R12 rationale: [S2] and [S3] show the extra machinery required; current V1 examples do not require it.

## §6 — Adversarial findings

### §6.1 Finding A - lock numbering drift

281. Contradicted surface: `restart/research/INDEX.md:54` says Topic 2 anchors Lock 4 as the type-system stack.
282. Actual lock: `restart/locks/14-LOCKS.md:40` defines Lock 4 as per-domain orthogonal optimization.
283. SOTA evidence: none needed; this is internal provenance drift, not a literature contradiction.
284. Amendment: rewrite the Topic 2 anchor line as R9.
285. Receiving phase: Phase 2 research fold before any topic index is used as lock authority.
286. Severity: adversarial because a future worker could cite "Lock 4" and argue type-stack details from the wrong lock.

### §6.2 Finding B - "full HM with subsumption" is too strong

287. Contradicted surface: `restart/README.md:266` says "full Hindley-Milner with subsumption."
288. SOTA evidence: [S1] trades complete inference for partial local methods in the presence of subtyping and impredicative polymorphism; [S2] avoids general HM-style let-generalization for higher-rank completeness.
289. Proposed amendment: replace with R3.
290. Receiving phase: Phase 2 README and Architecture fold.
291. Lock impact: actual Lock 4 survives because output-piped CSP/egraph composition is unaffected; README type-system wording is too broad.
292. Adversarial point: if left unchanged, implementers may try to build a global HM+subtyping inferencer instead of a directed check/synth system.

### §6.3 Finding C - DK completeness is cited before its surface exists

293. Contradicted surface: `restart/README.md:260` cites DK papers as part of the type-system formal deep dive without naming the feature gate.
294. SOTA evidence: [S2] needs ordered contexts and existential solving; [S3] adds principality, polarized subtyping, and indexed-type coverage rules.
295. Proposed amendment: add R2, R4, and R12.
296. Receiving phase: Phase 2 Architecture §8.2 and MASTER-PLAN C.W1 fold.
297. Lock impact: no settled lock is invalidated; the lock should state when DK machinery is required.
298. Adversarial point: calling ordinary rank-1 generics "DK-style" would import proof obligations the plan does not budget.

### §6.4 Finding D - coercion examples need rule sites

299. Contradicted surface: `restart/README.md:266` gives numeric, lifetime, and typed-record coercions but no insertion sites.
300. SOTA evidence: [S1] and [S2] confine subsumption to check/synth transitions; [S5] returns explicit success/failure outcomes and obligations from unification.
301. Proposed amendment: add `SubsumptionEdge` / `CoercionCandidate` facts and diagnostics per R7 and R11.
302. Receiving phase: Phase 2 PASS-1 and Architecture diagnostic fold.
303. Lock impact: Lock 2 benefits because `passes::layout` becomes the single writer of coercion facts.
304. Adversarial point: without rule sites, "constraint relaxation" can become an informal conversion tower.

### §6.5 Finding E - Roc source role is overclaimed

305. Contradicted surface: `restart/research/INDEX.md:63` calls Roc a modern Rust-implemented bidirectional source.
306. SOTA evidence: [S5] verifies Roc constraint storage, `run_solve`, ranks, and unification; no check/synth bidirectional interface was verified in this pass.
307. Proposed amendment: rewrite the Roc source row as R10 unless a later worker verifies specific bidirectional code.
308. Receiving phase: Phase 2 research index fold.
309. Lock impact: none; Roc remains relevant to CSP-backed unification and production diagnostics.
310. Adversarial point: tertiary claims about Roc's algorithm must not stand in for source evidence.

## §7 — Surgery proposals

### §7.1 Surgery P1 - answer the engagement question in README §7

311. Target: `restart/README.md:260`.
312. Surgery directive: replace the first bold sentence with R1.
313. Acceptance gate: `rg -n "full Hindley-Milner with subsumption|Pierce-Turner style" restart/README.md restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md` shows no unscoped "full HM with subsumption" and any "Pierce-Turner" hit names local check/synth/subsumption.
314. Dependency: §5 R1 and §6 Finding B.

### §7.2 Surgery P2 - scope DK to a conditional higher-rank gate

315. Target: `restart/README.md:260` and `restart/ARCHITECTURE.md:1117-1132`.
316. Surgery directive: add R2, R4, and R12.
317. Acceptance gate: `rg -n "Dunfield|Krishnaswami|higher-rank|indexed|existential|principality|ordered context" restart/README.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md` shows DK named only with higher-rank/indexed/existential proof obligations.
318. Dependency: §5 R2/R4/R12 and §6 Finding C.

### §7.3 Surgery P3 - make subsumption a fact with diagnostics

319. Target: `restart/audit/pass-1-substrate/PASS-1.md:101-106`, `restart/audit/pass-1-substrate/PASS-1.md:220`, and `restart/ARCHITECTURE.md:1127-1132`.
320. Surgery directive: add `SubsumptionEdge` or `CoercionCandidate` as a layout-internal fact; add `BBNF-SUBSUMPTION-EDGE` diagnostic text from R7; append R11 to the chain-flow rule.
321. Acceptance gate: a focused text gate finds `SubsumptionEdge|CoercionCandidate|BBNF-SUBSUMPTION-EDGE` in PASS-1 and Architecture, with producer `passes::layout`.
322. Dependency: §5 R7/R11 and §6 Finding D.

### §7.4 Surgery P4 - repair Topic 2 lock anchor

323. Target: `restart/research/INDEX.md:54`.
324. Surgery directive: replace "Anchor locks: Lock 4" with R9.
325. Acceptance gate: `rg -n "Topic 2|Anchor locks|Lock 4" restart/research/INDEX.md restart/locks/14-LOCKS.md` no longer implies that actual Lock 4 is the type-stack lock.
326. Dependency: §5 R9 and §6 Finding A.

### §7.5 Surgery P5 - recast Roc source row

327. Target: `restart/research/INDEX.md:63`.
328. Surgery directive: replace the Roc row with R10.
329. Acceptance gate: the Roc row says "constraint/unification reference" unless a cited Roc source path proves check/synth bidirectional structure.
330. Dependency: §5 R10 and §6 Finding E.

### §7.6 Surgery P6 - MASTER-PLAN C.W1 implementation gate

331. Target: `restart/MASTER-PLAN.md:313`.
332. Surgery directive: replace the C.W1 scope sentence with R8.
333. Acceptance gate: C.W1 names the V1 type system as rank-1 HM core plus local check/synth and CSP; DK higher-rank is not part of the C.W1 close gate.
334. Dependency: §5 R8 and §6 Finding C.

### §7.7 Surgery P7 - add chain/recovery stress gate

335. Target: follow-up to `restart/audit/hardening/HARDENING-PASS-1-V5.md:45` and PASS-1 chain diagnostics.
336. Surgery directive: add a test-design row: "generic chain typing where a prior step yields a recovery placeholder or host-overload candidate must settle through `passes::layout` or emit `BBNF1401` plus `BBNF-SUBSUMPTION-EDGE`."
337. Acceptance gate: `rg -n "recovery placeholder|host-overload|BBNF-SUBSUMPTION-EDGE|BBNF1401" restart/audit/pass-1-substrate/PASS-1.md restart/MASTER-PLAN.md` shows a receiving gate.
338. Dependency: §5 R7/R11 and §6 Finding D.

### §7.8 Surgery P8 - source provenance note for gaps

339. Target: `restart/research/INDEX.md:61-64`.
340. Surgery directive: add a note that Ramsey and Herbelin/Lemay are optional only when primary source text/source is verified; otherwise topic reports must mark a provenance gap.
341. Acceptance gate: the index uses "if verified" for Ramsey source code and Herbelin/Lemay notes.
342. Dependency: §2 G1/G2.

### §7.9 Surgery P9 - layout vocabulary closure

343. Target: `restart/audit/pass-1-substrate/PASS-1.md:121` and `restart/audit/pass-1-substrate/PASS-1.md:140`.
344. Surgery directive: clarify whether `types/` is an internal child of `passes::layout` or a private helper module; public pass vocabulary remains `passes::layout`, `LayoutFacts`, and internal `TypeFacts`.
345. Acceptance gate: `rg -n "passes::types|passes/src/types|TypeFacts" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md` shows no public `passes::types` promise and every `TypeFacts` hit says internal.
346. Dependency: §4 D6.

### §7.10 Surgery P10 - add source-role legend

347. Target: new note after `restart/research/INDEX.md:57-64`.
348. Surgery directive: add a four-row legend for source roles: "algorithmic posture", "higher-rank proof gate", "production elaborator pressure", and "production constraint solver pressure."
349. Acceptance gate: Topic 2 source rows classify Pierce-Turner as algorithmic posture, DK13/DK19 as proof gate, Idris 2 as elaborator pressure, and Roc as constraint-solver pressure.
350. Dependency: §2.7 source synthesis and §4 D4/D5.

### §7.11 Surgery P11 - add proof-obligation checklist

351. Target: `restart/MASTER-PLAN.md:313` or receiving C.W1 detail in the full tranche spec.
352. Surgery directive: add a checklist item: "If higher-rank gate opens, implementation names ordered context form, existential-variable solving, principality criterion, decidability theorem target, soundness theorem target, and completeness theorem target."
353. Acceptance gate: `rg -n "ordered context|existential-variable|principality|decidability|soundness|completeness" restart/MASTER-PLAN.md restart/ARCHITECTURE.md` returns a concrete gate, not only a citation list.
354. Dependency: §6 Finding C.

### §7.12 Surgery P12 - add local-inference negative fixture

355. Target: receiving C.W1 or D.W3 type-checking gate.
356. Surgery directive: add one negative fixture where a chain contains an unannotated polymorphic host function whose input cannot be recovered from adjacent context; expected result is a diagnostic asking for an annotation.
357. Acceptance gate: test name contains `chain_requires_annotation` or equivalent and expected output cites `BBNF1401` plus the future annotation diagnostic.
358. Dependency: §3 C4 and §6 Finding B.

### §7.13 Surgery P13 - add successful local chain fixture

359. Target: receiving D.W3 multi-function chaining test gate.
360. Surgery directive: add one positive fixture `hex_byte -> parse_hex_pair -> u8` showing adjacent local flow from regex capture to host primitive to final annotation.
361. Acceptance gate: fixture typechecks without higher-rank machinery and records `LayoutFacts`/chain facts only.
362. Dependency: §3 C2.

### §7.14 Surgery P14 - add coercion-site fixture

363. Target: receiving C.W1 or D.W3 type-checking gate.
364. Surgery directive: add one fixture where `&'i str` flows into a host step expecting `Cow<'i, str>` and succeeds only through a registered lifetime coercion at the checking edge.
365. Acceptance gate: trace or snapshot contains one `CoercionCandidate` and no global coercion search.
366. Dependency: §6 Finding D.

### §7.15 Surgery P15 - add coercion-failure fixture

367. Target: receiving C.W1 or D.W3 type-checking gate.
368. Surgery directive: add one fixture where typed-record narrowing is requested without a registered directed subsumption edge.
369. Acceptance gate: compiler emits the proposed `BBNF-SUBSUMPTION-EDGE` diagnostic and does not silently insert a conversion.
370. Dependency: §6 Finding D.

### §7.16 Routed residue

371. Ramsey remains a provenance gap for this pass; no implementation claim from *Programming Languages: Build, Prove, and Compare* is folded.
372. Herbelin/Lemay remains a provenance gap for this pass; no subtype-coercion note is folded.
373. Roc remains useful as [S5] constraint/unification evidence, but a later worker must verify a specific Roc check/synth path before using it as bidirectional evidence.
374. Idris 2 remains useful as [S4] elaborator pressure, but V1 bbnf should not inherit dependent-type complexity by citation alone.
375. The actual Lock 4 is not contradicted; the topic index's Lock 4 label is the drift.
376. The biggest implementation residue is the missing rule-site representation for subsumption and coercion.
377. The biggest proof residue is the missing DK gate, which should stay closed for rank-1 grammar generics.

### §7.17 Closing answer

378. The restart currently uses bidirectional primarily in the Pierce-Turner sense: check/synth modes, local expected-type propagation, explicit annotations, and directed subsumption.
379. The restart also cites Dunfield-Krishnaswami, but current V1 syntax only needs DK as a conditional proof guardrail for future higher-rank, existential, or indexed grammar-type surfaces.
380. "Pierce-Turner-style" commits to local bidirectional checking with subtyping/subsumption edges; it does not commit to global type inference completeness and does not, by itself, commit to DK higher-rank algorithmic completeness.
381. Phase 2 should fold the language above so implementers know which algorithm they are building: rank-1 HM core constraints, Pierce-Turner local check/synth, CSP finite choices, and a closed DK gate unless the grammar type surface grows.
