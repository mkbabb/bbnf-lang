# RESEARCH INDEX — Wave 5+ SOTA Deep-Dive Catalogue

This index defines the eight research deep-dives that ground the bbnf-lang restart's SOTA-asserted architectural commitments in primary literature. Each topic maps to one or more of the 14 settled locks, names the specific architectural surface the research must engage, and prescribes the deliverable shape.

The dispatched research agent for each topic produces `restart/research/<topic>.md` (~500-1000 lines). The fold cycle (Phase 2 of `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md`) then absorbs §5 (refinements) + §7 (surgery proposals) into the existing trio + PASS surfaces. §6 (adversarial findings) of any artefact may trigger Phase 2 escalation if SOTA contradicts a settled lock.

## §1 — Required reading (per research agent, before topic-specific dispatch)

1. `restart/README.md` — gestalt anchor; settled positions; 14 locks; BBNF V1 extensions; tape + direct-to-struct union; SOTA synthesis.
2. `restart/locks/14-LOCKS.md` — settled architectural commitments; the agent's research must engage at least one named lock.
3. `restart/ARCHITECTURE.md` — primary trio surface; the anchor sections per topic are listed below.
4. `restart/MASTER-PLAN.md` — tranche-level context; the topic's evidence rows live here.
5. `restart/audit/pass-{1-substrate,2-codegen,3-runtime}/PASS-{1,2,3}.md` — the surfaces a fold may touch.
6. `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md` — V4 cohort verdict (READY); the carry-baseline.
7. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` — voice + discipline locks.
8. The topic's key sources (per §3 catalogue below).

## §2 — Output contract (every research artefact)

Every `restart/research/<topic>.md` carries §1-§7:

§1 — **Settled position in the restart.** Cite path:line for every settled claim the topic engages. Render the current claim verbatim or near-verbatim; do not paraphrase.

§2 — **SOTA literature deep-dive.** Each citation is a primary source (paper / canonical codebase commit / benchmark report). The agent walks the source, extracting the load-bearing claims, the experimental evidence, and the design tradeoffs. Minimum citation count per topic: 5 primary sources. Maximum: 15 (further citations route to §6 only if they surface contradictions).

§3 — **Convergence points.** Where the restart's settled position matches SOTA, name the match precisely. Convergence is verified, not asserted: cite both the restart claim and the SOTA evidence.

§4 — **Divergence points.** Where the restart departs from SOTA, name the divergence + the reason. Divergences may be principled (bbnf's grammar-authoritative posture demands a different shape than what egg or simdjson does) or unconsidered (the LLM that wrote the restart pattern-matched to a familiar shape without engaging the alternative).

§5 — **Refinements to fold.** Specific text changes for the existing trio + PASS surfaces. Each refinement: target file:line / current text / proposed text / rationale (with SOTA citation). Refinements that survive fold-side verification land in Phase 2.

§6 — **Adversarial findings.** Where SOTA contradicts a settled lock, or proves a settled claim too weak / too strong / unfounded. Each finding: contradicted lock / SOTA evidence (primary citation) / proposed amendment / receiving phase. Findings escalate through Phase 2 escalation per the orchestrator.

§7 — **Surgery proposals.** Concrete edits Phase 2 would land if the orchestrator approves. Each proposal: target file:line / surgery directive (verbatim text or surgical description) / acceptance gate / dependency on §5 vs §6.

## §2.1 — Source classification and current lock hygiene

The catalogue below governs which research leads may support evidence-bearing
fold claims. Topic source lists remain dispatch prompts; the topic artefacts are
the authority for verified sources. Role-unclear or unverified leads stay out of
proof language until a primary paper, official documentation page, canonical
repository, or benchmark report is cited.

| Topic | Verified source slots | Provenance / role hygiene | Current lock binding |
|---|---:|---|---|
| 1 HM foundations | 9 | Primary/canonical sources support rank-1 HM principal schemes, Algorithm W lineage, implementation references, and diagnostics. | Lock 2 for layout/type boundary; Lock 4 for keeping type/CSP/egraph domains separated. |
| 2 Bidirectional | 5 | Roc is verified as modern constraint/unification implementation evidence, not as Pierce-Turner or Dunfield-Krishnaswami proof. Ramsey and Herbelin/Lemay are provenance gaps unless primary source text is verified. | Lock 2 and Lock 4. |
| 3 CSP / parametric generics | 8 | The verified Stuckey/GADT pressure source is Sulzmann/Schrijvers/Stuckey 2008 on Herbrand constraint abduction; the combined Schrijvers/Stuckey ADT-reconstruction citation is not used as evidence. GADT-like or higher-rank surfaces are not folded into V1. | Lock 2, Lock 4, and Lock 14. |
| 4 Egraphs | 10 | The verified egglog source is Zhang et al. 2023, *Better Together: Unifying Datalog and Equality Saturation*; the older "Yang et al. 2024" title is a provenance gap. egglog is a fusion counterargument, not a V1 adoption. | Lock 4. |
| 5 Cost models | 9 | Almomany and exact Deb 2014 remain provenance gaps. Pareto/frontier and solver-backed claims must use verified cost/extraction, LLVM/Cranelift, Z3, or multi-objective sources from the topic artefact. | Lock 4 and Lock 8. |
| 6 Tape/direct | 9 | Hubbard comparative-study wording remains a provenance gap unless a primary URL/DOI is supplied. simdjson, sonic-rs, yyjson, RapidJSON, and UTF-8 validation sources carry the evidence. | Lock 1 and Lock 8. |
| 7 Green/red incremental | 8 | Ungar/Adams and HelpMate remain optional provenance gaps for this research slot. rowan, rust-analyzer, Wagner/Graham, tree-sitter, and Salsa carry the evidence. | Lock 1 and Lock 14. |
| 8 SIMD/DFA/regex | 8 | Vectorscan is verified as official source evidence for production SIMD regex lineage. Hyperscan may be cited only when primary source/docs are verified in the consuming artefact. | Lock 1, Lock 8, and Lock 10. |

## §3 — The eight topics

### Topic 1 — Type system foundations: HM + algorithm W + Damas-Milner

**Anchor locks**: Lock 2 (layout/type boundary) + Lock 4 (separate type, CSP, egraph, miner, and cost domains).
**Anchor sections**: `restart/README.md` §7; `restart/ARCHITECTURE.md` §8; `restart/audit/pass-1-substrate/PASS-1.md` §3 (HostFn + chain typing).
**Engagement question**: does the restart correctly position HM + algorithm W as the *foundation* on which bidirectional + Pierce-Turner build, or does it conflate the layers? Does the restart's "HM + bidirectional + Pierce-Turner + CSP-backed" stack survive when the four are decomposed?
**Key sources**:
- Damas, L. & Milner, R. (1982). *Principal type-schemes for functional programs.* POPL.
- Milner, R. (1978). *A Theory of Type Polymorphism in Programming.* JCSS.
- Pierce, B. C. (2002). *Types and Programming Languages.* Chapter 22 (HM with let-polymorphism); Chapter 16 (subtyping).
- The OCaml typer (`typing/typecore.ml`) — canonical algorithm-W with constraint-based extensions.
- The SML/NJ typer (`compiler/Elaborator/elaborate/elabcore.sml`) — early-1990s reference.
- Daan Leijen's "Extensible records with scoped labels" (2005) — relevant if bbnf's records / generic-rule surface bears on the topic.
- Heeren, B. (2005). *Top quality type error messages.* Heeren-Hage-Swierstra constraint-based diagnostics — the topic's friction-forecast bridge.

### Topic 2 — Bidirectional + Pierce-Turner + Dunfield-Krishnaswami

**Anchor locks**: Lock 2 + Lock 4.
**Anchor sections**: same as Topic 1; PASS-1 §3 chain-step type-flow rule.
**Engagement question**: is "bidirectional" in the restart used in the Pierce-Turner sense (synthesise + check modes; subtyping subsumption) or the Dunfield-Krishnaswami sense (algorithmic completeness for higher-rank polymorphism)? Or both? When the restart says "Pierce-Turner-style" what does it commit to?
**Key sources**:
- Pierce, B. C. & Turner, D. N. (1998). *Local Type Inference.* TOPLAS — the foundational bidirectional paper.
- Dunfield, J. & Krishnaswami, N. R. (2013). *Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism.* ICFP.
- Dunfield, J. & Krishnaswami, N. (2019). *Sound and Complete Bidirectional Typechecking for Higher-Rank Polymorphism with Existentials and Indexed Types.* PACMPL.
- Norman Ramsey's bidirectional implementation in *Programming Languages: Build, Prove, and Compare* (optional only; provenance gap unless primary implementation text is verified).
- The Idris 2 elaborator (`src/Core/Unify.idr`) — bidirectional + dependent-type integration.
- Roc's type checker (Rust source, `crates/compiler/load_internal/src/file.rs` and friends) — modern Rust constraint/unification implementation evidence; not Pierce-Turner or Dunfield-Krishnaswami proof.
- Hugo Herbelin & Stéphanie Lemay's bidirectional subtype coercion notes — optional only; provenance gap unless primary source text is verified.

### Topic 3 — CSP-backed finite choices + parametric polymorphism + generic rules

**Anchor locks**: Lock 2 + Lock 4 + Lock 14.
**Anchor sections**: `restart/ARCHITECTURE.md` §8; PASS-1 §3 generic-rule typing; `restart/README.md` §5 BBNF extensions (generics).
**Engagement question**: the restart commits to CSP alongside HM + bidirectional. Does this mean (a) constraint-based HM (HM(X) Pottier-Rémy), (b) GADT-style local-equality pressure that V1 rejects, or (c) an orthogonal finite-domain CSP solver for host, layout, backend, recognizer, materialisation, recovery, and extraction choices? When generic rules `Object<V>` are introduced, what does CSP do that HM alone cannot?
**Key sources**:
- Pottier, F. & Rémy, D. (2005). *The Essence of ML Type Inference.* (Chapter from *Advanced Topics in Types and Programming Languages*) — HM(X) constraint framework.
- Vytiniotis, D., Peyton Jones, S., Schrijvers, T., Sulzmann, M. (2011). *OutsideIn(X): Modular type inference with local assumptions.* JFP — adversarial GADT/local-equality pressure; not a V1 implementation commitment.
- Sulzmann, M., Schrijvers, T., Stuckey, P. (2008). *Type inference for GADTs via Herbrand constraint abduction.* KU Leuven CW 507 — CSP-adjacent GADT inference pressure.
- Schrijvers, T. & Bruynooghe, M. (2006). *Polymorphic algebraic data type reconstruction.* PPDP — ADT reconstruction pressure, not GADT local-equality solving.
- The GHC type checker (`compiler/typecheck/`) — production-grade OutsideIn(X) implementation.
- Cardelli, L. & Wegner, P. (1985). *On Understanding Types, Data Abstraction, and Polymorphism.* Computing Surveys — for the parametric/ad-hoc polymorphism distinction.
- Sulzmann, M., Duck, G. J., Peyton Jones, S., Stuckey, P. J. (2007). *Understanding functional dependencies via Constraint Handling Rules.* JFP.

### Topic 4 — E-graphs + equality saturation + bridge-vs-union design

**Anchor locks**: Lock 4.
**Anchor sections**: `restart/README.md` §6 optimization apotheosis; `restart/ARCHITECTURE.md` §10; `restart/MASTER-PLAN.md` D-tranche optimization rows.
**Engagement question**: the restart commits to "CSP + e-graph (bridged, not unioned) + shape mining + cost-model trait shared with regex". Why bridged? What does the bridge look like operationally? Has anyone in literature done the bridge before, and what did they learn?
**Key sources**:
- Tate, R., Stepp, M., Tatlock, Z., Lerner, S. (2009). *Equality Saturation: A New Approach to Optimization.* POPL — the original.
- Willsey, M., Nandi, C., Wang, Y. R., Flatt, O., Tatlock, Z., Panchekha, P. (2021). *egg: Fast and Extensible Equality Saturation.* PACMPL — the foundational modern Rust implementation.
- Zhang, Y., et al. (2023). *Better Together: Unifying Datalog and Equality Saturation.* PLDI — canonical egglog source for the saturation-as-datalog framing. "Yang et al. 2024 / egglog" remains a provenance-gap shorthand, not evidence wording.
- Flatt, O., Coward, S., Willsey, M., Tatlock, Z., Panchekha, P. (2022). *Small Proofs from Congruence Closure.* FMCAD.
- The egg crate documentation and case studies (`docs.rs/egg`).
- The Cranelift mid-end (post-2023) — production e-graph optimization in a JIT compiler.
- The Lean 4 simp tactic — a different e-graph application surface.

### Topic 5 — Cost models + Pareto extraction + SMT-backed cost composition

**Anchor locks**: Lock 4 + Lock 8.
**Anchor sections**: same as Topic 4; specifically MASTER-PLAN's D/E/F tranches that consume cost decisions.
**Engagement question**: the restart commits to "cost-model trait shared with regex". What does "shared" mean operationally — same trait, different instances? One cost function across both BIR and regex? When the trait is a single Rust trait, what's the shared shape?
**Key sources**:
- Wang, Y. R., et al. (2020). *Spores: Sum-Product Optimization via Relational Equality Saturation for Large Scale Linear Algebra.* — for cost-aware extraction.
- The egg analysis trait (`Analysis<L>`) source code + tests.
- Meurer, A., et al. (2017). *SymPy: symbolic computing in Python.* — for symbolic-cost composition.
- Almomany, A., et al. (2014). *Cost-aware code motion in Java.* — provenance gap unless primary source is verified; not fold evidence.
- The LLVM cost model (`CodeMetrics.cpp`) — production cost decisions, for contrast.
- The Cranelift cost model (post-egg integration) — Rust-native cost decisions.
- Multi-objective optimisation literature: use verified Deb/KanGAL or equivalent primary-source material for Pareto/frontier framing; exact "Deb 2014" remains a provenance gap unless verified.

### Topic 6 — Tape encoding + direct-to-struct union design

**Anchor locks**: Lock 1 (tape + direct-to-struct union; properly implemented).
**Anchor sections**: `restart/README.md` §8 substrate; `restart/ARCHITECTURE.md` §11; `restart/locks/14-LOCKS.md` Lock 1 reframe; PASS-3 §6 runtime crate tree.
**Engagement question**: the restart commits to "tape + direct-to-struct UNION" — every rule has `TapeShape` and `ValueShape`; typed values borrow `&'i Tape<'i>` plus node id. What does the union really mean — both representations co-exist, or one materialises to the other? How does sonic-rs's tape compare? What does simdjson's two-stage parsing give that bbnf's union doesn't, and vice versa?
**Key sources**:
- Langdale, G. & Lemire, D. (2019). *Parsing Gigabytes of JSON per Second.* VLDB Journal — the simdjson paper.
- Lemire, D. & Langdale, G. (2020). *On the Performance of UTF-8 Validation in Software.* (relevant for the tape's UTF-8 path).
- The sonic-rs crate documentation + source (`bytedance/sonic-rs`).
- The yyjson source (`ibireme/yyjson`) — comparable C reference.
- The rapidjson source (`Tencent/rapidjson`) — earlier reference design.
- Rapidjson's *In-situ Parsing* technical note — for direct-to-struct comparison.
- Hubbard, M. et al. (2020). *Parsing Through Other People's Eyes: A Look at JSON Parsing.* — provenance gap unless primary source is verified; do not use as evidence.

### Topic 7 — Green/red trees + incremental parsing + fault tolerance

**Anchor locks**: Lock 1 (substrate) + carry-incremental (LSP fallback); Lock 14 (yaml two-surface implies incremental graceful when adding grammar).
**Anchor sections**: `restart/README.md` §8; PASS-3 §3 (incremental + LSP); MASTER-PLAN incremental carries.
**Engagement question**: the restart positions tape + direct-to-struct as a single substrate. Does this position survive contact with rust-analyzer's green/red tree separation, treesitter's incremental edit story, or rowan's parent-pointer red layer? When yaml's grammar onboards and the LSP must handle a syntax error, what story does bbnf's substrate carry that competing substrates do not?
**Key sources**:
- The rowan crate documentation + source (`rust-analyzer/rowan`).
- Rust-analyzer architecture documentation (`rust-analyzer/docs/dev/architecture.md`).
- Ungar, D. & Adams, S. R. (1994). *Eliminating Data Fetch Stalls on Pipelined Architectures.* — optional cache-locality lead only; provenance gap for this fold unless directly verified.
- Brand, M., et al. (2003). *The HelpMate Parsing Framework.* — optional early incremental-parsing lead only; provenance gap unless primary source is verified.
- Wagner, T. A. & Graham, S. L. (1998). *Efficient and Flexible Incremental Parsing.* TOPLAS — the seminal incremental-parsing paper.
- The treesitter parsing algorithm paper (Brand & Visser-style; see Treesitter docs).
- The Salsa crate (rust-analyzer's incremental computation framework) — for query-based incremental.

### Topic 8 — SIMD scanning + DFA construction + bespoke regex HIR

**Anchor locks**: Lock 1 + Lock 8 + Lock 10.
**Anchor sections**: `restart/README.md` §6 + §8; `restart/ARCHITECTURE.md` §10 + bbnf-regex sections; PASS-2 SIMD/Pratt detection rows.
**Engagement question**: the restart commits to a bespoke `parse-that` regex with NFA→DFA construction + DFA codegen in bbnf, replacing the regex crate. What does this buy over `regex-automata`? When the SIMD scanner matches something the DFA doesn't, what's the contract? Does the restart's "SIMD-first" posture survive when bbnf-regex carries cases the SIMD path cannot accelerate?
**Key sources**:
- Cox, R. (2007). *Regular Expression Matching: the Virtual Machine Approach.* swtch.com — foundational.
- Cox, R. (2007). *Regular Expression Matching Can Be Simple And Fast.* swtch.com — RE2 design rationale.
- Langdale, G. & Lemire, D. (2019). *Parsing Gigabytes of JSON per Second.* VLDB — covers the SIMD scan kernel design.
- Owens, S. (2009). *Regular-expression derivatives reexamined.* JFP.
- The `regex-automata` crate (BurntSushi) source + documentation.
- The Vectorscan source (Intel Hyperscan fork) — verified official-source evidence for production SIMD regex lineage; cite Hyperscan itself only from verified primary docs/source.
- The logos crate (Maciej Hirsz) — Rust-native lexer DSL with tight code generation.

## §4 — Adversarial-finding obligation

Every research agent is obligated to surface at least one §6 adversarial finding even if the topic ultimately converges with SOTA. The role of §6 is not to fabricate disagreement but to honestly engage where the SOTA literature presses against the restart's position. If §6 is empty, the agent restates why; an empty §6 with no rationale is a failed audit.

The obligation guards against the LLM bias toward synthesis-and-agreement: the research deep-dive should at minimum identify the two or three places where reasonable architects looking at the SOTA would disagree with the restart's position.

## §5 — Voice + discipline locks (per `restart/README.md` §13)

Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage; never cite "the prompt said" or "the user asked". Path:line citations on concrete claims about target text. Tables liberal where they serve. Per-X tables for "all grammars" / "all backends" / "all topics" claims. No placeholder wording. No quick solutions. No legacy code uncontested.

## §6 — Closing posture

Eight research deep-dives ground the restart's SOTA assertions in primary literature and surface where SOTA presses against the locks. The artefacts feed Phase 2 fold; adversarial findings escalate through Phase 2 escalation. The terminal V6 hardening verifies the folded corpus is still cohort-coherent.

Hereupon the dispatched research agents read this index, the orchestrator, the locks, and the gestalt anchor; then proceed to topic-specific deep-dive.
