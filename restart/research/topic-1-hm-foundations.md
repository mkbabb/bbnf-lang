# Topic 1 - Type system foundations: HM + Algorithm W + Damas-Milner

Source count: 9 primary or canonical sources.

Adversarial finding count: 3.

Engagement verdict: the stack survives decomposition, but only if the fold states the layer boundaries more sharply.

Short answer: HM/Algorithm W is the principal-scheme foundation for the equality-constrained core. Bidirectional checking is the expected-type interface for annotations, directives, host-chain steps, and syntax nodes whose type flows from context. Pierce-Turner local inference is a local, partial technique for subtyping plus polymorphism; it must not be treated as the same mechanism as global HM reconstruction. CSP-backed solving is admissible for finite grammar choices and diagnostic ordering, but the phrase "CSP-backed unification" needs surgery because HM unification itself is first-order equality unification, not a general CSP.

Primary-source inventory:

1. Milner 1978, "A Theory of Type Polymorphism in Programming", DOI [10.1016/0022-0000(78)90014-4](https://doi.org/10.1016/0022-0000(78)90014-4), publisher PDF via Edinburgh Research Explorer [PDF](https://www.pure.ed.ac.uk/ws/files/15143545/1_s2.0_0022000078900144_main.pdf).
2. Damas and Milner 1982, "Principal type-schemes for functional programs", DOI [10.1145/582153.582176](https://doi.org/10.1145/582153.582176), accessible re-keyed paper [PDF](https://steshaw.org/hm/milner-damas.pdf).
3. Pierce 2002, Types and Programming Languages official contents [PDF](https://www.cis.upenn.edu/~bcpierce/tapl/contents.pdf).
4. Pierce and Turner 2000, "Local Type Inference", official author PDF [PDF](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf), DOI [10.1145/345099.345100](https://doi.org/10.1145/345099.345100).
5. OCaml typer, `typing/typecore.ml`, official OCaml repository at commit `8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62`, [source](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml).
6. SML/NJ elaborator, `compiler/Elaborator/elaborate/elabcore.sml`, official SML/NJ repository at commit `cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508`, [source](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml).
7. Leijen 2005, "Extensible records with scoped labels", Microsoft Research publication page [page](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/) and [PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf).
8. Heeren 2005, "Top Quality Type Error Messages", Utrecht repository [PDF](https://dspace.library.uu.nl/bitstream/handle/1874/7297/full.pdf) and author page [page](https://cs.ou.nl/members/bastiaan/phdthesis/index.html).
9. Pottier and Remy 2005, "The Essence of ML Type Inference", canonical OCaml papers entry [page](https://ocaml.org/papers).

Provenance gaps:

- Damas and Milner 1982 is verified through the ACM DOI and an accessible re-keyed copy. Direct ACM full text may be access controlled; the re-keyed copy preserves the ACM copyright notice and paper text, but the fold should keep the DOI as the canonical citation.
- TAPL chapter internals were not used as quoted evidence. The official contents PDF verifies Chapter 16 as metatheory of subtyping and Chapter 22 as type reconstruction with constraints, unification, principal types, and let-polymorphism.
- SML/NJ `elabcore.sml` is an elaborator file that constructs typed abstract syntax and tracks type variables; deeper unification machinery lives in neighboring Elaborator modules. This source is still cited because the dispatch specifically names it.

## §1 — Settled position in the restart

The research catalogue requires this topic to test the HM foundation rather than treating "type system" as one fused slogan.

Claim R1: `restart/research/INDEX.md:38-42` names Topic 1 as "Type system foundations: HM + algorithm W + Damas-Milner" and asks whether HM + Algorithm W are positioned as the foundation on which bidirectional + Pierce-Turner build.

Claim R2: `restart/research/INDEX.md:44-50` names Damas-Milner 1982, Milner 1978, TAPL Chapters 22 and 16, OCaml `typecore.ml`, SML/NJ `elabcore.sml`, Leijen records, and Heeren diagnostics as the key source set for this topic.

Claim R3: `restart/research/INDEX.md:149-153` requires at least one adversarial finding where SOTA presses against the settled position, even when the topic mostly converges.

Claim R4: `restart/research/INDEX.md:155-157` locks the voice: "Calibrated, direct prose", "Path:line citations", "No placeholder wording", "No quick solutions", and "No legacy code uncontested."

Claim R5: `restart/README.md:258-260` states: "**Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + CSP-backed unification.** The three are composed, not exclusive - Hindley-Milner is the inference engine; bidirectional check/synth is the algorithmic style at each grammar node; CSP backs the constraint-collection + unification phase."

Claim R6: `restart/README.md:260` also routes PASS-1's formal work to "the specific algorithm + the formal proof obligations + citations."

Claim R7: `restart/README.md:262` states: "**Annotation surface: hybrid.** Pure inference is default. First-class explicit annotations welcome where the author wants control (`rule -> u32`, `rule -> Color`, generic-rule type parameters). Multi-function chaining (`-> f1 -> f2 -> f3`) flows types through stages with bidirectional check at each."

Claim R8: `restart/README.md:264` states: "**Generic rules: V1.** `Object<V> = "{" pair<V> ("," pair<V>)* "}"; pair<V> = String ":" V`. CSP propagates type variables; codegen monomorphises per call site (Rust handles natively; WASM via type erasure + dispatch)."

Claim R9: `restart/README.md:266` states: "**Subtyping: full Hindley-Milner with subsumption.** CSP relaxes constraints; coercion is a constraint relaxation. Numeric coercion (`i32 -> i64 -> f64`); lifetime coercion (`&'i str -> Cow<'i, str> -> String`); typed-record narrowing..."

Claim R10: `restart/README.md:268` states that lookbehind's left operand is a context constraint and contributes no value type, while the right operand carries the value.

Claim R11: `restart/README.md:161` says multi-function chains extend terminal-side `-> Type` with first-class bidirectional inference, type inference flows through each stage, and CSP backs constraint collection.

Claim R12: `restart/README.md:170` says generic rules carry type variables, CSP propagates them, and codegen monomorphises per call site.

Claim R13: `restart/README.md:188-209` fixes the pass order: parse, validate, type inference, shape mining, e-graph saturation, cost extraction, Backend IR, lowerers, regen equality, with validation and inference co-iterating.

Claim R14: `restart/README.md:195` spells the type inference pass as "CSP + bidirectional + Hindley-Milner; produces TypedGrammarIR."

Claim R15: `restart/README.md:221-227` says CSP is the central inference substrate, e-graphs are the rewrite + extraction substrate, and the two compose through a bridge rather than a fused type.

Claim R16: `restart/README.md:386` says Lock 4 is honoured by composing `egraph`, `csp-solver`, and `cost-model` by output-piping, with CSP/egraph bridged rather than fused.

Claim R17: `restart/README.md:473` closes by naming the type system as "Hindley-Milner + bidirectional + Pierce-Turner-styled."

Claim R18: `restart/locks/LOCKS.md:36` states Lock 2: "HM/CSP type checking is a subroutine of layout lowering, never a public peer pass; `LayoutFacts` is the public side-table."

Claim R19: `restart/locks/LOCKS.md:40` states Lock 4: "CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, and cost model compose by output-piping. No unified hypergraph."

Claim R20: `restart/locks/LOCKS.md:60` states Lock 14: every grammar plugs in through grammar source, workspace metadata, and an optional fenced declaration crate; generic crates carry zero grammar-specific code.

Claim R21: `restart/ARCHITECTURE.md:796` states: "Type inference annotates Grammar IR; it does not mutate grammar syntax", and names `passes::layout` as the owner where HM + bidirectional + CSP run as a subroutine.

Claim R22: `restart/ARCHITECTURE.md:979-982` says the layout-lowering pass is the public surface, HM/CSP type checking is its internal subroutine, `TypeFacts` is scratch, and downstream passes read `LayoutFacts`.

Claim R23: `restart/ARCHITECTURE.md:987-994` distinguishes public `LayoutFacts`, `ShapeFacts`, `RecognizerFacts`, `EGraphFacts`, `CspSolution`, `CostFacts`, `RecoveryFacts`, and internal `TypeFacts`.

Claim R24: `restart/ARCHITECTURE.md:1047-1051` says BBNF supports lookbehind, block-bodied `@host fn`, multi-function chaining, generics, `@error(recover = ...)`, and `@layout`, while excluding rewrite-mode and grammar-level Unicode class algebra.

Claim R25: `restart/ARCHITECTURE.md:1095-1101` says rule-level chain form is `Expr -> f1 -> f2`; method-chain syntax is legal only inside `HostFn` block bodies; bodyless host declarations have no production.

Claim R26: `restart/ARCHITECTURE.md:1117-1121` states: "The type system is Hindley-Milner plus bidirectional checks and CSP constraints" and says README sets HM, bidirectional typing, CSP use, explicit annotations, generic rules, subtyping/coercion, and lookbehind types as in-scope.

Claim R27: `restart/ARCHITECTURE.md:1127-1132` sets type rules: inference is grammar-wide; annotations narrow; host functions are typed; chains compose left-to-right; lookbehind must be bounded; layout and error directives produce facts rather than ad hoc codegen flags.

Claim R28: `restart/ARCHITECTURE.md:1154-1159` states closure type rules: host chain output of segment N unifies with segment N+1 input; map result unifies with rule output shape; predicate closure returns boolean-like type; recovery closure returns registered recovery code or hint.

Claim R29: `restart/audit/pass-1-substrate/PASS-1.md:12` keeps "HM + bidirectional + CSP-backed constrained unification."

Claim R30: `restart/audit/pass-1-substrate/PASS-1.md:24-37` puts `Map` / `HostCall` in Grammar IR with chain steps, argument ids, and expected type; the consumer includes host inference and layout lowering.

Claim R31: `restart/audit/pass-1-substrate/PASS-1.md:71` states: "HM inference generates core constraints; bidirectional checking handles explicit signatures/directives; CSP-backed constrained unification solves finite choices for host overload, layout representation, recognizer eligibility, direct/tape materialization, recovery strategy, and backend plan."

Claim R32: `restart/audit/pass-1-substrate/PASS-1.md:97-106` defines diagnostic strings including `BBNF1201` for host signature failure and `BBNF1401` for chain-step mismatch.

Claim R33: `restart/audit/pass-1-substrate/PASS-1.md:121-123` gives `passes` children including `types` and `layout`, and `host` children including `signature`, `registry`, and `chain`.

Claim R34: `restart/audit/pass-1-substrate/PASS-1.md:140` says `passes/types` carries HM + bidirectional checking, while `passes/layout` owns `@layout` lowering and layout-fact production.

Claim R35: `restart/audit/pass-1-substrate/PASS-1.md:142` says `host/chain` owns chain-step type flow and dispatch; per-grammar declaration crates live nowhere there.

Claim R36: `restart/audit/pass-1-substrate/PASS-1.md:214` says `HostFn` is block-bodied and bodyless host declarations do not exist.

Claim R37: `restart/audit/pass-1-substrate/PASS-1.md:220` says every chain step must accept the previous step's output as first argument, checking runs left-to-right, and `BBNF1401` names the first mismatch.

Claim R38: `restart/MASTER-PLAN.md:36-40` says lookbehind, `@host fn`, multi-function chaining, generics, `@error`, and `@layout` are in; rewrite-mode is out; Unicode class algebra routes to regex.

Claim R39: `restart/MASTER-PLAN.md:94-104` names Architecture §8.2 as governing generics, §8.3-§8.4 as governing host functions and chains, and D/F as owner tranches for host type/runtime dispatch.

Claim R40: `restart/MASTER-PLAN.md:180-185` assigns C to PASS-1 IR/type/bridge and D to BBNF parser, typing, host, layout/error facts.

Claim R41: `restart/MASTER-PLAN.md:313` makes C.W1 the wave for "HM + bidirectional + CSP type-checking subroutine inside `passes::layout`; `LayoutFacts` as the public side-table."

Claim R42: `restart/MASTER-PLAN.md:346-348` makes D.W1 generic rules and annotations, D.W2 block-bodied `@host fn`, and D.W3 multi-function chaining type/runtime contract.

Claim R43: `restart/MASTER-PLAN.md:355` includes `cargo test -p passes host_generics lookbehind` in the Tranche D hard close.

Claim R44: `restart/MASTER-PLAN.md:579` routes `[workspace.metadata.bbnf.host_fns]` to A/D/F for host registry typing/runtime.

Claim R45: `restart/MASTER-PLAN.md:750` flags the failure mode: "`@host fn` becomes a hidden declaration-crate requirement", and demands D host tests prove generic primitives and metadata first.

Claim R46: `restart/MASTER-PLAN.md:776` carries PASS-1 reconciliation to C/D and requires Architecture §8.1 to match PASS-1 §6 on block-bodied `@host fn`, infix lookbehind, and rule-level `->` chains.

Claim R47: `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:104-118` records the V4 READY posture, no open punch items, and the requirement that amended docs preserve path:line citations and receiver/blocker/gate discipline.

Claim R48: `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:471-489` names formal-fragment drift, closure bias after READY, matrix satisfaction, and citation confidence as the main pathology classes.

Claim R49: `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:49-53` verifies that the BBNF grammar reconciliation is closed and the stale declaration-only host syntax is absent.

Claim R50: `restart/audit/hardening/HARDENING-PASS-1-PASS-2-V5.1A.md:73-75` states V5.1A closed remaining PASS-local citation hygiene and the corpus can proceed to Phase 1 research, with V6 still obligated to re-check folded text.

Local reading verdict for §1:

- The restart does not wholly conflate the layers. README, Architecture, PASS-1, and Master Plan all try to distinguish HM, bidirectional checking, CSP choices, and layout facts.
- The phrase "Hindley-Milner is the inference engine" is correct for the core only.
- The phrase "CSP backs the constraint-collection + unification phase" is too broad and should be narrowed.
- The phrase "full Hindley-Milner with subsumption" is the largest technical risk.
- The stack survives decomposition if the fold turns the slogan into this pipeline: HM core constraints -> equality unification/principal scheme -> expected-type bidirectional checks -> finite coercion/subsumption layer -> CSP choices for host/layout/materialization/backend decisions.

## §2 — SOTA literature deep-dive

Source S1: Milner 1978.

- Citation: Robin Milner, "A Theory of Type Polymorphism in Programming", Journal of Computer and System Sciences 17(3), 348-375, 1978; DOI [10.1016/0022-0000(78)90014-4](https://doi.org/10.1016/0022-0000(78)90014-4).
- Verification path: publisher PDF is available through Edinburgh Research Explorer [PDF](https://www.pure.ed.ac.uk/ws/files/15143545/1_s2.0_0022000078900144_main.pdf).
- Load-bearing claim: the work presents a formal polymorphic type discipline plus compile-time Algorithm W.
- Load-bearing claim: the semantic theorem states that well-typed programs do not hit type errors in the formal semantics.
- Load-bearing claim: the syntactic theorem states that Algorithm W acceptance yields a well-typed program.
- Load-bearing claim: the paper treats coercions and overloading as orthogonal to compile-time polymorphic inference rather than as part of the HM core.
- Load-bearing claim: the inference story rests on type constraints from primitive operators, variable declaration/use, substitution, and unification.
- Design tradeoff: W is practical for ML-style implicit polymorphism because polymorphic procedures receive schemes, and each occurrence can be instantiated.
- Design tradeoff: the paper already warns that richer language features such as assignment complicate semantic soundness.
- Design tradeoff: the implementation story uses an efficient W-like algorithm with side-effected substitution; principality is still the theoretical load-bearing surface.
- Evidence for restart: supports HM as foundation for core rule and host-chain type flow.
- Pressure on restart: does not license treating general subtyping or coercion relaxation as plain HM.
- Pressure on restart: does not license replacing first-order unification with a generic CSP without preserving principal-scheme obligations.
- Fold implication: bbnf should state HM as "core equality-constrained inference", not "the entire type system."

Source S2: Damas and Milner 1982.

- Citation: Luis Damas and Robin Milner, "Principal type-schemes for functional programs", POPL 1982; DOI [10.1145/582153.582176](https://doi.org/10.1145/582153.582176); accessible text [PDF](https://steshaw.org/hm/milner-damas.pdf).
- Provenance note: the DOI is canonical; the accessible PDF is a re-keyed copy. The report uses the paper's theorem structure and avoids long quotation.
- Load-bearing claim: the paper answers whether the type assignment algorithm finds the most general type for every expression and declaration in the purely applicative ML core.
- Load-bearing claim: the answer is yes for that core; well-typedness is decidable there.
- Load-bearing claim: a principal type scheme is one whose other valid schemes are generic instances.
- Load-bearing claim: Algorithm W computes a substitution plus a type and is shown sound and complete against the inference system.
- Load-bearing claim: the paper generalizes Hindley's principal-scheme result for combinatory logic.
- Design tradeoff: the theorem is scoped; it is not a blanket proof for every practical extension.
- Design tradeoff: the theorem depends on scheme instantiation/generalization boundaries and first-order type unification.
- Evidence for restart: "HM is the inference engine" is valid for the core if bbnf keeps principal-scheme tests.
- Pressure on restart: "full HM with subsumption" conflicts with the principal-scheme framing unless subsumption is sharply limited.
- Pressure on restart: CSP may schedule, defer, or explain constraints, but Algorithm W's principal result is about most-general unifiers and generic instances.
- Fold implication: every type-system gate should distinguish "principal HM core" from "extension decisions".

Source S3: TAPL, Pierce 2002.

- Citation: Benjamin C. Pierce, Types and Programming Languages, official contents [PDF](https://www.cis.upenn.edu/~bcpierce/tapl/contents.pdf).
- Provenance note: official contents verify chapter topics and page ranges. Chapter text was not treated as accessible evidence.
- Load-bearing claim from contents: Chapter 16 is "Metatheory of Subtyping" and contains algorithmic subtyping, algorithmic typing, joins and meets, and bottom type.
- Load-bearing claim from contents: Chapter 22 is "Type Reconstruction" and contains type variables/substitutions, constraint-based typing, unification, principal types, implicit annotations, and let-polymorphism.
- Design tradeoff: TAPL separates subtyping metatheory from type reconstruction.
- Design tradeoff: the book's organization itself warns against folding HM reconstruction and subtyping into one unqualified phrase.
- Evidence for restart: using bidirectional or algorithmic checking for annotations is compatible with a type reconstruction chapter that also teaches constraints and unification.
- Pressure on restart: subtyping/subsumption belongs to a distinct algorithmic layer; the fold should not call it "full HM" without qualification.
- Fold implication: README line 266 and Architecture §8.2 should separate "subtyping/coercion layer" from "HM foundation."

Source S4: Pierce and Turner 2000.

- Citation: Benjamin C. Pierce and David N. Turner, "Local Type Inference", TOPLAS 22(1), 2000; author PDF [PDF](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf); DOI [10.1145/345099.345100](https://doi.org/10.1145/345099.345100).
- Load-bearing claim: local type inference is partial.
- Load-bearing claim: missing annotations are recovered from adjacent syntax-tree nodes rather than long-distance unification variables.
- Load-bearing claim: one method infers type arguments in polymorphic applications using a local constraint solver.
- Load-bearing claim: another method infers function-bound annotations by propagating constraints downward from enclosing application nodes.
- Load-bearing claim: the motivating problem is subtyping plus impredicative polymorphism, for which full inference is not the HM problem.
- Design tradeoff: local inference sacrifices completeness to gain simple behavior in richer type systems.
- Design tradeoff: its locality is a user-facing predictability feature.
- Evidence for restart: "bidirectional check/synth at each grammar node" fits the local propagation style, especially for `rule -> Type`, `@layout`, `@host fn`, and chain steps.
- Pressure on restart: Pierce-Turner should not be used as a synonym for Algorithm W.
- Pressure on restart: if bbnf invokes Pierce-Turner, the algorithm must state which information is propagated downward, which type is synthesized upward, and where local constraint solving stops.
- Fold implication: move "Pierce-Turner-style" from the foundation phrase into the expected-type/local propagation clause.

Source S5: OCaml `typing/typecore.ml`.

- Citation: official OCaml source at commit `8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62`, [typecore.ml](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml).
- Source anchor: `unify_exp_types` calls `unify` and maps failures to expression type clashes at [L489-L499](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L489-L499).
- Source anchor: `type_exp` delegates to `type_expect` with a fresh variable at [L4246-L4248](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L4246-L4248).
- Source anchor: `type_expect` records the expression type before unifying with the expected type at [L4250-L4279](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L4250-L4279).
- Source anchor: `if` expressions push expected types into both branches and then unify branch results at [L4813-L4836](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L4813-L4836).
- Source anchor: coercion code tries unification and subtyping through `subtype`, `enlarge_type`, and `Ctype.unify` at [L5364-L5415](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L5364-L5415).
- Load-bearing claim: OCaml's production checker is expected-type driven, with unification as a core operation.
- Load-bearing claim: coercion/subtyping is separate machinery, not mere equality unification.
- Design tradeoff: production ML-family systems compose an HM-like core with local expected types, principal warnings, GADT traces, labels, objects, modules, and coercions.
- Evidence for restart: `type_exp` plus `type_expect` is strong production evidence for "check/synth style at grammar nodes."
- Pressure on restart: OCaml's code supports decomposition, not a fused `HM + Pierce-Turner + CSP` operation.
- Fold implication: `passes/types` should have a `type_exp`/`type_expect` style public internal API, with coercion checks behind a bounded, named layer.

Source S6: SML/NJ `elabcore.sml`.

- Citation: official SML/NJ source at commit `cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508`, [elabcore.sml](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml).
- Source anchor: expression elaboration enters at [L512-L514](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L512-L514).
- Source anchor: applications elaborate function and argument and emit `APPexp` with joined type-variable tracking at [L587-L592](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L587-L592).
- Source anchor: type constraints from source annotations become `CONSTRAINTexp` at [L593-L597](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L593-L597).
- Source anchor: let expressions elaborate declarations and then elaborate the body under the extended environment at [L609-L615](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L609-L615).
- Source anchor: overload declarations require monomorphic ground instances of a known type scheme at [L790-L805](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L790-L805).
- Source anchor: value declarations track explicit type variables and update local type-variable sets at [L831-L858](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L831-L858).
- Load-bearing claim: the SML/NJ elaborator preserves source annotations and type-variable sets as first-class compiler data before full type checking.
- Load-bearing claim: let environment extension and type-variable tracking remain explicit, echoing the W/generalization structure.
- Design tradeoff: production SML implements HM-family ideas through elaboration plus separate type checking, not a single textbook Algorithm W pass.
- Evidence for restart: bbnf should carry type-variable provenance for diagnostics and explicit grammar annotations.
- Pressure on restart: a source-elaboration phase with clear type-variable accounting is a missing named artefact in current restart prose.
- Fold implication: Phase 2 should add an internal `TypeObligation` or `TypeConstraint` side record that keeps source spans and explicit type variables before layout lowering erases `TypeFacts`.

Source S7: Leijen 2005.

- Citation: Daan Leijen, "Extensible records with scoped labels", Microsoft Research [page](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/) and [PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf).
- Load-bearing claim: the record extension is designed to work with standard Hindley-Milner, qualified types, and MLF.
- Load-bearing claim: the design defines a new equality notion between monotypes plus an extended unification algorithm.
- Load-bearing claim: duplicate labels and scoped labels are retained, while field selection can remain efficient for predicative systems.
- Design tradeoff: record polymorphism can preserve an HM-style foundation only by changing equality/unification or adding row machinery deliberately.
- Evidence for restart: generic rules such as `Object<V>` can fit HM if they are parametric and monomorphised at call sites.
- Pressure on restart: "typed-record narrowing" is not free HM subsumption. If bbnf needs structural record narrowing, the fold must choose a record typing discipline.
- Fold implication: typed-record narrowing should be marked as finite layout-shape subsumption in bbnf V1, with row-polymorphism routed unless Phase 2 accepts a record-type extension.

Source S8: Heeren 2005.

- Citation: Bastiaan J. Heeren, "Top Quality Type Error Messages", Utrecht repository [PDF](https://dspace.library.uu.nl/bitstream/handle/1874/7297/full.pdf) and author page [page](https://cs.ou.nl/members/bastiaan/phdthesis/index.html).
- Load-bearing claim: the thesis maps typing problems to type constraints and sends them to a specialized constraint solver.
- Load-bearing claim: no single type inference algorithm suits all users; the framework is parameterized.
- Load-bearing claim: directives can change error reporting while soundness of the underlying type system remains intact in the relevant class.
- Load-bearing claim: the constraint-based algorithm generalizes W and M and proves correctness with respect to Hindley-Milner rules.
- Load-bearing claim: constraints carry information such as origin and reason, later used by heuristics and diagnostics.
- Design tradeoff: separating generation, ordering, and solving of constraints improves diagnostics.
- Evidence for restart: "CSP-backed" is strongest when it means constraint ordering, finite-domain choice, and diagnostic explanation.
- Pressure on restart: a CSP diagnostic layer must not silently alter the underlying type system unless the fold states the soundness boundary.
- Fold implication: bbnf should keep diagnostic metadata on every generated type obligation, especially host chains and layout decisions.

Source S9: Pottier and Remy 2005 via OCaml canonical papers.

- Citation: OCaml papers page entry for "The Essence of ML Type Inference" [page](https://ocaml.org/papers).
- Provenance note: the OCaml page is canonical project documentation for OCaml papers; the full chapter itself may require MIT Press access.
- Load-bearing claim from the OCaml entry: the chapter describes Core ML type inference as a constraint generator that produces type equations plus a constraint solver presented as rewrite rules.
- Load-bearing claim from the OCaml entry: it is an in-depth Core ML account with emphasis on type inference.
- Design tradeoff: constraint generation and constraint solving are separate phases even when the result is still HM-family inference.
- Evidence for restart: the restart's separation into Grammar IR annotation, `TypeFacts`, `LayoutFacts`, and CSP solution can be made SOTA-aligned.
- Pressure on restart: "CSP-backed unification" should become "constraint generation plus dedicated solvers", with equality unification named separately.
- Fold implication: C.W1 and D.W3 gates should test both generated constraints and solved facts, not just final type success.

SOTA synthesis for §2:

- HM and Algorithm W are the proof-bearing core for principal type schemes.
- Algorithm W is not identical to production ML checkers, but production checkers still preserve its key moves: fresh variables, instantiation, let-generalization, expected-type unification, and source-located errors.
- Bidirectional checking and Pierce-Turner local inference solve a different problem: controlled local recovery of omitted annotations in richer systems.
- Subtyping is a separate algorithmic layer, not a feature that can be declared "full HM" without losing principality or predictability.
- Constraint-based HM is legitimate and well-studied, but "constraint" here must retain solver-specific boundaries.
- CSP is plausible for bbnf's finite choices: host overload, layout representation, recognizer eligibility, materialization, recovery strategy, and backend plan.
- The current restart is closest to SOTA when it says TypeFacts are internal and LayoutFacts are public.
- The current restart is weakest when it uses "unification" for both first-order equality unification and finite-domain CSP choices.

## §3 — Convergence points

Convergence C1: HM as foundation.

- Restart claim: README says HM is the inference engine at `restart/README.md:260`.
- SOTA evidence: Milner 1978 gives Algorithm W as the compile-time polymorphic checker; Damas-Milner 1982 proves W computes principal schemes for the applicative ML core.
- Match: bbnf's pure grammar-rule core can use HM-style principal inference for rule references, generic rule variables, and host-free expression shapes.
- Constraint: the match is core-scoped.
- Fold note: keep "foundation", remove any implication that HM alone decides layout, backend plan, or recovery strategy.

Convergence C2: explicit annotations as check mode.

- Restart claim: annotations are hybrid and first-class at `restart/README.md:262`.
- SOTA evidence: OCaml `type_exp` delegates to `type_expect` with a fresh expected variable at `typecore.ml` [L4246-L4248](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L4246-L4248).
- SOTA evidence: OCaml unifies typed expressions with expected types at `typecore.ml` [L4250-L4279](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L4250-L4279).
- Match: `rule -> u32`, `rule -> Color`, and host signatures naturally enter check mode.
- Fold note: model bbnf with `infer_expr` and `check_expr`/`expect_expr` internals.

Convergence C3: chains as local expected-type propagation.

- Restart claim: PASS-1 says chain output type feeds the next step and diagnostics fail at first mismatch at `restart/audit/pass-1-substrate/PASS-1.md:220`.
- SOTA evidence: Pierce-Turner local inference propagates type constraints downward from adjacent application nodes and solves locally.
- SOTA evidence: OCaml application/branch checking pushes expected type into local subterms.
- Match: `Expr -> f1 -> f2 -> f3` can be typed as a local sequence where each synthesized output becomes the next expected first argument.
- Fold note: chain typing should not require global search; left-to-right local obligations preserve diagnostic clarity.

Convergence C4: internal TypeFacts, public LayoutFacts.

- Restart claim: Architecture makes `TypeFacts` internal and `LayoutFacts` public at `restart/ARCHITECTURE.md:979-994`.
- SOTA evidence: production checkers keep rich internal type state while exposing typed trees, warnings, and diagnostics.
- SOTA evidence: Heeren shows constraints carrying source/reason metadata can serve diagnostics without becoming user-facing type theory.
- Match: bbnf should compile through internal obligations and then emit layout/materialization facts.
- Fold note: this is a strong design; preserve it.

Convergence C5: constraint generation plus solving as a valid HM implementation style.

- Restart claim: PASS-1 says HM inference generates core constraints at `restart/audit/pass-1-substrate/PASS-1.md:71`.
- SOTA evidence: Pottier-Remy via OCaml papers presents Core ML inference as a constraint generator plus rewrite-rule solver.
- SOTA evidence: Heeren gives a correctness proof for constraint-based HM against Hindley-Milner rules.
- Match: bbnf can be constraint-based without abandoning HM.
- Fold note: name equality constraints and finite-choice constraints separately.

Convergence C6: finite host/layout/materialization choices are a good CSP fit.

- Restart claim: PASS-1 lists host overload, layout representation, recognizer eligibility, materialization, recovery, and backend plan as CSP finite choices at `restart/audit/pass-1-substrate/PASS-1.md:71`.
- SOTA evidence: Heeren's Top separates constraint generation, ordering, and solving to improve diagnostics and customization.
- SOTA evidence: Leijen's records show that once extra type equality or record rules enter, the extension must be explicit.
- Match: bbnf's finite grammar-authoritative choices are outside pure W and can sit in a CSP solver.
- Fold note: CSP should solve finite alternatives after HM produces obligations, not replace principal unification.

Convergence C7: generic rules fit HM parametric polymorphism when scoped.

- Restart claim: README's `Object<V>` generic rule is V1 at `restart/README.md:264`.
- SOTA evidence: Damas-Milner's type schemes are outer-quantified; each occurrence can instantiate the scheme.
- SOTA evidence: SML/NJ tracks explicit type variables in value declarations at `elabcore.sml` [L831-L858](https://github.com/smlnj/smlnj/blob/cc7f4a3c59e89d36c6b6ef93b1ae0f159808f508/compiler/Elaborator/elaborate/elabcore.sml#L831-L858).
- Match: `Object<V>` and `pair<V>` can elaborate to scoped type variables and instantiate at use sites.
- Fold note: add gates for explicit type-variable provenance and duplicate generic parameter rejection.

Convergence C8: diagnostic strings need type-obligation provenance.

- Restart claim: PASS-1 defines `BBNF1201` and `BBNF1401` at `restart/audit/pass-1-substrate/PASS-1.md:97-106`.
- SOTA evidence: Heeren says constraints carry origin/reason metadata and this feeds heuristics and final messages.
- Match: host signature and chain-step errors should cite source spans and expectation provenance.
- Fold note: every type obligation needs `source_span`, `expected_from`, and `blame_rank` fields before CSP ordering.

Convergence C9: subtyping must be algorithmic when present.

- Restart claim: README admits subtyping/coercion at `restart/README.md:266`.
- SOTA evidence: TAPL separates metatheory of subtyping and algorithmic typing from type reconstruction.
- SOTA evidence: OCaml's coercion path uses `subtype`, `enlarge_type`, and `Ctype.unify` as distinct operations at `typecore.ml` [L5364-L5415](https://github.com/ocaml/ocaml/blob/8eb5b83dc3a926eb6b7bb33e1dd63e5730e4be62/typing/typecore.ml#L5364-L5415).
- Match: bbnf can include numeric/lifetime coercions if they are algorithmic and finite.
- Fold note: call this "bounded subsumption/coercion", not "full HM."

Convergence C10: the restart's non-fused optimizer lock is compatible with type inference.

- Restart claim: Lock 4 forbids a unified hypergraph at `restart/locks/LOCKS.md:40`.
- SOTA evidence: Heeren and Pottier-Remy both support phase separation: generate constraints, solve them, retain proof obligations.
- Match: bbnf's CSP/egraph bridge remains orthogonal to HM foundations.
- Fold note: do not fuse type equality, e-graph equivalence, and finite CSP choice into one solver.

## §4 — Divergence points

Divergence D1: "full Hindley-Milner with subsumption" overstates the SOTA.

- Restart text: `restart/README.md:266` says "Subtyping: full Hindley-Milner with subsumption."
- SOTA pressure: TAPL separates subtyping metatheory from type reconstruction; Milner 1978 treats coercions and overloading as orthogonal to polymorphic type inference.
- SOTA pressure: Pierce-Turner local inference was motivated by subtyping plus polymorphism because full global inference is not the simple HM problem.
- Why this matters: "full HM with subsumption" sounds like principal Algorithm W survives arbitrary subsumption.
- Technical risk: principal types may disappear or become unintuitive with unrestricted subtyping.
- Recovery path: state "HM core plus bounded subsumption/coercion layer."

Divergence D2: "CSP-backed unification" conflates equality unification with finite-domain solving.

- Restart text: `restart/README.md:260` says CSP backs "constraint-collection + unification."
- Restart text: `restart/audit/pass-1-substrate/PASS-1.md:71` says CSP-backed constrained unification solves finite choices.
- SOTA pressure: Damas-Milner Algorithm W relies on most-general first-order unifiers.
- SOTA pressure: Heeren's constraint-based framework still proves correctness against HM rules; constraints carry metadata and ordering, but the underlying type equality story remains accountable.
- Why this matters: a CSP can choose among overload/layout/materialization alternatives without being the unifier that grants principality.
- Recovery path: state "HM emits equality constraints solved by unification; CSP solves finite non-principal choices and orders diagnostics."

Divergence D3: "bidirectional check/synth is the algorithmic style at each grammar node" is too broad.

- Restart text: `restart/README.md:260` says bidirectional check/synth is the algorithmic style at each grammar node.
- SOTA pressure: OCaml's production checker uses expected-type typing, but not every node is equally bidirectional; `type_exp` can synthesize through a fresh variable and `type_expect` checks when context exists.
- SOTA pressure: Pierce-Turner locality is specifically adjacent-node local inference, not a label applied to every inference decision.
- Why this matters: implementers may force every AST node into check/synth form and lose simpler W-style inference where no expected type exists.
- Recovery path: state "nodes synthesize by default; annotations, host signatures, chain steps, branch contexts, and directive payloads enter check mode."

Divergence D4: generic rule monomorphisation needs an instantiation boundary.

- Restart text: `restart/README.md:264` says CSP propagates type variables and codegen monomorphises per call site.
- SOTA pressure: Damas-Milner instantiates schemes per occurrence; SML/NJ tracks explicit type variables during elaboration.
- Why this matters: "CSP propagates type variables" can obscure where generalization happens and where codegen monomorphisation reads final schemes.
- Recovery path: add an explicit `GenericScheme` / `InstantiationSite` obligation in C.W1 or D.W1.

Divergence D5: typed-record narrowing is not settled by the cited HM sources.

- Restart text: `restart/README.md:266` includes "typed-record narrowing (a struct with optional fields can subsume a struct with fewer fields)."
- SOTA pressure: Leijen shows record polymorphism can integrate with HM, but only via a deliberate record equality/unification extension.
- SOTA pressure: TAPL routes record subtyping through subtyping metatheory, not through HM reconstruction.
- Why this matters: bbnf value shapes and layout records could accidentally grow a structural record calculus without a proof obligation.
- Recovery path: limit V1 to finite layout-shape subsumption for generated records; route row-polymorphism or structural record subtyping to a later research gate.

Divergence D6: PASS-1's `passes/types` and `passes/layout` split is useful but conflicts softly with Lock 2 phrasing.

- Restart text: Lock 2 says HM/CSP type checking is a subroutine of layout lowering at `restart/locks/LOCKS.md:36`.
- Restart text: PASS-1 crate tree gives both `passes/types` and `passes/layout` at `restart/audit/pass-1-substrate/PASS-1.md:121`.
- Restart text: PASS-1 rationale says `passes/types` carries HM + bidirectional checking while `passes/layout` owns layout lowering at `restart/audit/pass-1-substrate/PASS-1.md:140`.
- SOTA pressure: production systems commonly separate elaboration/typechecking from later layout/codegen facts.
- Why this matters: if "layout lowering" owns all type checking, the type-system implementation may be hidden inside a pass whose name does not match its proof obligations.
- Recovery path: keep `passes/types` as an internal child module invoked by `passes/layout`, and state that only `LayoutFacts` crosses the pass boundary.

Divergence D7: TAPL access is partial.

- Restart source list asks for TAPL Chapters 22 and 16.
- Verified source: official contents proves chapter topics only.
- Unverified source: detailed TAPL chapter arguments were not accessible through the official URL used here.
- Why this matters: the fold cannot cite TAPL line/page claims beyond the official contents unless a licensed/local source is added.
- Recovery path: cite TAPL contents for chapter boundaries, and use Milner/Damas-Milner/Pierce-Turner/OCaml for substantive claims.

## §5 — Refinements to fold

Refinement F1: README §7 opening sentence.

- Target: `restart/README.md:260`.
- Current text: "Hindley-Milner inference + bidirectional check/synth in Pierce-Turner style + CSP-backed unification."
- Proposed text: "Hindley-Milner principal-scheme inference for the core, expected-type check/synth for annotations and local flows, Pierce-Turner-style local propagation where subtyping/polymorphism needs partial inference, and CSP-backed finite-choice solving."
- Rationale: Damas-Milner proves principal schemes for the core; Pierce-Turner is local and partial; Heeren/Pottier-Remy support constraint generation plus solving without erasing solver boundaries.

Refinement F2: README §7 composition explanation.

- Target: `restart/README.md:260`.
- Current text: "Hindley-Milner is the inference engine; bidirectional check/synth is the algorithmic style at each grammar node; CSP backs the constraint-collection + unification phase."
- Proposed text: "HM generates the equality-constrained core and principal schemes; nodes synthesize unless an annotation, directive, host signature, branch context, or chain step supplies an expected type; first-order unification solves equality constraints; CSP solves bounded non-HM choices such as overload, layout, materialization, recovery, and backend selection."
- Rationale: OCaml `type_exp`/`type_expect` and Damas-Milner W require this separation.

Refinement F3: README subtyping clause.

- Target: `restart/README.md:266`.
- Current text: "Subtyping: full Hindley-Milner with subsumption."
- Proposed text: "Subtyping/coercion: HM core plus a bounded subsumption layer."
- Rationale: TAPL Chapter 16 and Pierce-Turner separate subtyping from HM reconstruction; Milner treats coercions as orthogonal.

Refinement F4: README coercion examples.

- Target: `restart/README.md:266`.
- Current text: "CSP relaxes constraints; coercion is a constraint relaxation."
- Proposed text: "The coercion layer is finite and explicit: numeric widening, lifetime-owned escalation, and generated-record shape narrowing produce named obligations; each obligation either lowers to an explicit coercion or fails before Backend IR."
- Rationale: avoids implying arbitrary subtyping; matches OCaml's distinct coercion/subtype path.

Refinement F5: README generic rules.

- Target: `restart/README.md:264`.
- Current text: "CSP propagates type variables; codegen monomorphises per call site."
- Proposed text: "The type checker generalizes a rule scheme, instantiates it at each `Ref`/call site, records instantiation obligations with source spans, and hands codegen a finite monomorphisation set."
- Rationale: Damas-Milner scheme instantiation and SML/NJ explicit type-variable tracking demand a named boundary.

Refinement F6: README chain typing.

- Target: `restart/README.md:262`.
- Current text: "Multi-function chaining (`-> f1 -> f2 -> f3`) flows types through stages with bidirectional check at each."
- Proposed text: "Multi-function chaining synthesizes the value type of the left expression, checks each step against the previous step's output as its first expected argument, and synthesizes the final chain result."
- Rationale: PASS-1 already says this at `restart/audit/pass-1-substrate/PASS-1.md:220`; source-level README should carry it.

Refinement F7: Architecture §8.2 first sentence.

- Target: `restart/ARCHITECTURE.md:1117`.
- Current text: "The type system is Hindley-Milner plus bidirectional checks and CSP constraints."
- Proposed text: "The type system has an HM principal-scheme core, an expected-type check/synth interface, bounded subsumption/coercion obligations, and CSP constraints for finite grammar-derived choices."
- Rationale: exact decomposition; preserves settled stack while removing conflation.

Refinement F8: Architecture §8.2 type-rule table.

- Target: `restart/ARCHITECTURE.md:1127-1132`.
- Current text: table does not name first-order equality unification separately.
- Proposed text: add row: "`HM equality constraints` | Fresh type variables, instantiation, generalization, and first-order unification produce internal `TypeFacts`; failures preserve source-span and expected-from metadata."
- Rationale: Damas-Milner and OCaml evidence.

Refinement F9: Architecture §8.2 type-rule table.

- Target: `restart/ARCHITECTURE.md:1127-1132`.
- Current text: "`Annotations narrow, not bypass, inferred types.`"
- Proposed text: "`Annotations check, not bypass, inferred types.` Contract: annotations provide expected types; the checker unifies or emits a typed diagnostic with the annotation span."
- Rationale: OCaml `type_expect` uses expected types and unification.

Refinement F10: Architecture side tables.

- Target: `restart/ARCHITECTURE.md:987-994`.
- Current text: `TypeFacts` is internal scratch artefact.
- Proposed text: keep internal status, but add "Type obligation logs are retained for diagnostics until `LayoutFacts` and `RecoveryFacts` are emitted; they are not a public pass artefact."
- Rationale: Heeren's constraint metadata supports diagnostics without exposing `TypeFacts`.

Refinement F11: PASS-1 type algorithm line.

- Target: `restart/audit/pass-1-substrate/PASS-1.md:71`.
- Current text: "HM inference generates core constraints; bidirectional checking handles explicit signatures/directives; CSP-backed constrained unification solves finite choices..."
- Proposed text: "HM inference generates equality constraints and principal schemes; expected-type checking handles explicit signatures, annotations, directives, and chain steps; first-order unification solves equality constraints; CSP solves finite choices..."
- Rationale: primary SOTA requires unification to stay distinct from finite-domain solving.

Refinement F12: PASS-1 pass tree rationale.

- Target: `restart/audit/pass-1-substrate/PASS-1.md:140`.
- Current text: "`passes` | `normalize/` ... `types/` HM + bidirectional checking; `layout/` `@layout` lowering..."
- Proposed text: "`types/` is an internal child invoked by `layout/`; it owns HM schemes, expected-type checking, type obligations, and diagnostic provenance. `layout/` is the pass boundary and emits public `LayoutFacts`."
- Rationale: reconciles Lock 2 with production compiler separation.

Refinement F13: Master Plan C.W1.

- Target: `restart/MASTER-PLAN.md:313`.
- Current text: "HM + bidirectional + CSP type-checking subroutine inside `passes::layout`; `LayoutFacts` as the public side-table. `TypeFacts` lives only inside the layout pass."
- Proposed text: "HM principal-scheme core plus expected-type checking inside `passes::layout`, with finite CSP choices factored after equality unification; `LayoutFacts` is public, while `TypeFacts` and `TypeObligationLog` remain internal."
- Rationale: C.W1 needs proof gates for generated constraints and solved facts.

Refinement F14: Master Plan D.W1.

- Target: `restart/MASTER-PLAN.md:346`.
- Current text: "Generic rules and annotations."
- Proposed text: "Generic rules, annotations, scheme instantiation, and monomorphisation-set evidence."
- Rationale: generic V1 needs an instantiation boundary.

Refinement F15: Master Plan D.W3.

- Target: `restart/MASTER-PLAN.md:348`.
- Current text: "Multi-function chaining type/runtime contract."
- Proposed text: "Multi-function chaining type/runtime contract, including left-to-right expected-argument obligations and first-mismatch diagnostics."
- Rationale: connects PASS-1 `BBNF1401` to implementation.

Refinement F16: Master Plan hard close.

- Target: `restart/MASTER-PLAN.md:353-356`.
- Current text: `cargo test -p passes host_generics lookbehind`.
- Proposed text: add `cargo test -p passes type_obligations principal_core chain_expected_flow`.
- Rationale: gates must prove HM principal core and chain-local checking.

Refinement F17: diagnostic vocabulary.

- Target: `restart/audit/pass-1-substrate/PASS-1.md:97-106`.
- Current text: `BBNF1201` and `BBNF1401` strings are present.
- Proposed text: add diagnostic metadata contract after the table: "Each type diagnostic records `expected_from`, `actual_from`, `obligation_id`, and `solver_stage` (`hm-unify`, `check`, `coerce`, or `csp-choice`)."
- Rationale: Heeren-style source/reason constraints make diagnostics inspectable.

Refinement F18: TAPL citation hygiene.

- Target: any fold text that cites TAPL Chapter 16/22.
- Current text: no direct TAPL detailed claims in target docs yet.
- Proposed text: cite official contents for chapter placement only unless a licensed/local TAPL chapter source is added.
- Rationale: avoid V5-style citation confidence faults.

## §6 — Adversarial findings

Finding A1: "Full Hindley-Milner with subsumption" is too strong.

- Contradicted lock/claim: README §7 type-system claim at `restart/README.md:266`.
- SOTA evidence: Milner 1978 presents coercions and overloading as orthogonal to the compile-time polymorphic discipline; TAPL separates subtyping metatheory from type reconstruction; Pierce-Turner local inference exists because subtyping plus richer polymorphism is not solved by plain global HM.
- Fault: the phrase tells implementers that HM and subsumption are one theory.
- Risk: the implementation could accept arbitrary structural subsumption, then lose principal types or produce order-dependent diagnostics.
- Proposed amendment: replace "full Hindley-Milner with subsumption" with "HM core plus bounded subsumption/coercion layer."
- Receiving phase: Phase 2 fold, with D.W1/D.W3 gates and V6 hardening re-check.
- Acceptance gate: tests prove principal schemes for a host-free seed grammar and reject unbounded structural record subtyping unless an explicit finite coercion exists.

Finding A2: "CSP-backed unification" confuses the solver contract.

- Contradicted lock/claim: README line `restart/README.md:260`; PASS-1 line `restart/audit/pass-1-substrate/PASS-1.md:71`.
- SOTA evidence: Damas-Milner Algorithm W is proven around substitution and most-general unification; Heeren's constraint-based system still proves correctness against Hindley-Milner and treats constraint information/order as a separate facility.
- Fault: "CSP-backed unification" can mean the CSP solver chooses types in a way that invalidates principal unifiers.
- Risk: diagnostics and materialization choices could become solver-order artifacts rather than derivable type facts.
- Proposed amendment: "first-order unification solves HM equality constraints; CSP solves finite non-HM choices and may order diagnostics."
- Receiving phase: Phase 2 fold into README, Architecture §8.2, PASS-1 §2, and Master C.W1.
- Acceptance gate: generated type-obligation snapshot separates `Eq`, `Expected`, `Coerce`, and `FiniteChoice` obligations.

Finding A3: typed-record narrowing lacks a selected record type theory.

- Contradicted lock/claim: README subtyping examples at `restart/README.md:266`.
- SOTA evidence: Leijen integrates extensible records with HM by introducing a new equality and extended unification algorithm; TAPL routes record subtyping through subtyping metatheory.
- Fault: the restart lists typed-record narrowing as if it were a simple coercion equivalent to numeric widening.
- Risk: generated direct-to-struct shapes may smuggle in open row polymorphism or structural width subtyping without a proof.
- Proposed amendment: V1 admits only finite generated-shape narrowing where source and target shapes are both known at compile time; row-polymorphism is routed to a later research gate.
- Receiving phase: Phase 2 fold into README §7 and Architecture §8.2.
- Acceptance gate: a test shows `{a,b?}` can narrow to `{a}` only through a named generated coercion, while open record variables fail with a routed diagnostic.

Adversarial rationale:

- None of these findings force a redraft of Lock 4.
- They do force wording surgery because the current prose is broad enough to mislead implementation.
- The lock that survives is orthogonal composition: HM core, check/synth interface, finite CSP, and non-fused egraph remain separate.
- The research result is therefore AMEND, not RE-DRAFT.

Residual risks routed to sibling topics:

- Higher-rank polymorphism belongs to Topic 2 and Dunfield-Krishnaswami.
- GADTs and OutsideIn(X) belong to Topic 3.
- General semantic subtyping and algebraic subtyping belong to a future subtyping-specific research pass if bbnf V1 wants more than finite coercions.
- Full record row polymorphism belongs to a record/layout research gate, not this HM foundation report.

## §7 — Surgery proposals

Surgery S1: README type-system header.

- Target: `restart/README.md:260`.
- Directive: replace the bold header sentence with the F1 proposed text.
- Acceptance gate: `rg -n "full Hindley-Milner with subsumption|CSP-backed unification" restart/README.md` returns zero, unless the hit is in a provenance note naming this surgery.
- Dependency: §5 F1 and §6 A2.

Surgery S2: README type-system explanatory paragraph.

- Target: `restart/README.md:260`.
- Directive: insert the F2 decomposition after the header sentence.
- Acceptance gate: README names `first-order unification`, `finite-choice CSP`, and `expected type` in §7.
- Dependency: §5 F2.

Surgery S3: README annotation surface.

- Target: `restart/README.md:262`.
- Directive: replace the final sentence with F6 chain typing text.
- Acceptance gate: `rg -n "synthesizes the value type|first expected argument|final chain result" restart/README.md`.
- Dependency: §5 F6.

Surgery S4: README generic rules.

- Target: `restart/README.md:264`.
- Directive: replace "CSP propagates type variables; codegen monomorphises per call site" with F5 scheme-instantiation wording.
- Acceptance gate: generic-rule test plan names `GenericScheme`, `InstantiationSite`, or equivalent artefacts.
- Dependency: §5 F5.

Surgery S5: README subtyping.

- Target: `restart/README.md:266`.
- Directive: replace the paragraph with F3 and F4, retaining the numeric/lifetime/generated-record examples as bounded coercions.
- Acceptance gate: README no longer says "full Hindley-Milner with subsumption."
- Dependency: §6 A1 and §6 A3.

Surgery S6: Architecture type-system intro.

- Target: `restart/ARCHITECTURE.md:1117-1121`.
- Directive: replace with F7 decomposition and retain citations to README §7 and PASS-1 §2.
- Acceptance gate: Architecture §8.2 names all four layers: HM core, expected-type check/synth, bounded coercion, finite CSP.
- Dependency: §5 F7.

Surgery S7: Architecture type-rule table additions.

- Target: `restart/ARCHITECTURE.md:1123-1132`.
- Directive: add rows for `HM equality constraints`, `Expected-type checking`, `Bounded coercion`, and `Finite-choice CSP`.
- Acceptance gate: each row has producer, consumer, and failure diagnostic.
- Dependency: §5 F8 and §5 F9.

Surgery S8: Architecture side-table note.

- Target: `restart/ARCHITECTURE.md:987-994`.
- Directive: add the `TypeObligationLog` internal diagnostic-retention note from F10.
- Acceptance gate: `TypeObligationLog` appears only as internal, never public.
- Dependency: §5 F10.

Surgery S9: PASS-1 type algorithm paragraph.

- Target: `restart/audit/pass-1-substrate/PASS-1.md:71`.
- Directive: replace the line with F11.
- Acceptance gate: PASS-1 distinguishes equality unification from CSP finite choices.
- Dependency: §6 A2.

Surgery S10: PASS-1 crate-tree rationale.

- Target: `restart/audit/pass-1-substrate/PASS-1.md:140`.
- Directive: add that `passes/types` is an internal child invoked by `passes/layout`, while `layout` remains the pass boundary.
- Acceptance gate: Lock 2 remains satisfied; no public `TypeFacts` appears.
- Dependency: §5 F12.

Surgery S11: PASS-1 diagnostics metadata.

- Target: `restart/audit/pass-1-substrate/PASS-1.md:97-106`.
- Directive: add the diagnostic metadata contract from F17 after the diagnostic strings table.
- Acceptance gate: `BBNF1201` and `BBNF1401` tests assert `solver_stage` and `obligation_id`.
- Dependency: §5 F17.

Surgery S12: Master Plan C.W1.

- Target: `restart/MASTER-PLAN.md:313`.
- Directive: replace with F13.
- Acceptance gate: C.W1 consumer gate verifies host-free seed grammar principal scheme plus downstream `LayoutFacts`.
- Dependency: §5 F13 and §6 A2.

Surgery S13: Master Plan D.W1.

- Target: `restart/MASTER-PLAN.md:346`.
- Directive: replace with F14.
- Acceptance gate: D.W1 gate records monomorphisation-set evidence.
- Dependency: §5 F14.

Surgery S14: Master Plan D.W3.

- Target: `restart/MASTER-PLAN.md:348`.
- Directive: replace with F15.
- Acceptance gate: D.W3 includes a negative chain fixture that fails at the first mismatching step with `BBNF1401`.
- Dependency: §5 F15.

Surgery S15: Master Plan hard close.

- Target: `restart/MASTER-PLAN.md:353-356`.
- Directive: add `cargo test -p passes type_obligations principal_core chain_expected_flow`.
- Acceptance gate: command appears in the hard close and V6 confirms it is not a placeholder.
- Dependency: §5 F16.

Surgery S16: TAPL provenance rule.

- Target: future fold citations touching TAPL.
- Directive: cite TAPL official contents only for chapter topics unless a licensed local chapter source is added.
- Acceptance gate: `rg -n "TAPL.*Chapter" restart/README.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md` shows only chapter-topic claims or a local source reference.
- Dependency: §4 D7.

Surgery S17: record narrowing fence.

- Target: `restart/README.md:266` and `restart/ARCHITECTURE.md:1127-1132`.
- Directive: state that V1 record narrowing is finite generated-shape coercion; row-polymorphism and open structural record subtyping are routed.
- Acceptance gate: a negative fixture rejects open record narrowing and points to the routed research gate.
- Dependency: §6 A3.

Surgery S18: research-fold summary row.

- Target: `restart/research/INDEX.md` is denied to this worker, but Phase 2 may touch it if the orchestrator permits.
- Directive: add a fold-status row for Topic 1 after Phase 2 surgery, recording AMEND with no lock redraft.
- Acceptance gate: only Phase 2 touches INDEX; this worker does not.
- Dependency: §6 adversarial findings.

Phase 2 acceptance bundle:

- Gate G1: README, Architecture, PASS-1, and Master all avoid the phrase "full Hindley-Milner with subsumption."
- Gate G2: all four surfaces distinguish equality unification from finite CSP solving.
- Gate G3: chain typing has a first-mismatch negative fixture and source-span diagnostic.
- Gate G4: generic rules have scheme instantiation and monomorphisation-set evidence.
- Gate G5: TypeFacts remains internal; LayoutFacts remains public; TypeObligationLog, if named, is internal diagnostic evidence only.
- Gate G6: TAPL citations are provenance-clean.
- Gate G7: V6 hardening checks F/G/H pathologies against the folded text.

Closing posture:

- The restart's type stack survives decomposition.
- The foundation is HM/Algorithm W's principal-scheme discipline, scoped to the core.
- The implementation posture should be OCaml-like: synthesize with fresh variables, check against expected types when context exists, unify equality constraints, run a bounded coercion layer, then solve finite CSP choices.
- The adversarial surgery is textual and gate-level, not architectural redraft.
