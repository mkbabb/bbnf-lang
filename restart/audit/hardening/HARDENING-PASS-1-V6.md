# HARDENING-PASS-1-V6 - research-fold verification

## §1 Target identification and commits audited

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` |
| Report | `restart/audit/hardening/HARDENING-PASS-1-V6.md` |
| Audit target label | PASS-1 |
| Current workspace HEAD audited | `c5e3aab741ab2354486593e0cfbed97972ab1225` |
| Verdict | READY |
| Write scope used | this report only |
| Source surfaces edited | none |
| Research-fold focus | Topics 1-8, with PASS-1-owned substrate, type, bridge, cost, grammar-surface, and recovery-producer consequences centered |

Commit history in scope:

| Commit | Surface | Audit use |
|---|---|---|
| `0c72433b` | PASS-1 wave-5-fold amendment | Current PASS-1 text under V6 review. |
| `88d22b78` | Earlier PASS-1 research-fold amendment | Recent history anchor named by dispatch. |
| `c5e3aab7` | Current HEAD during this pass | Includes PASS-3 V6 report; this worker did not alter it. |
| `5ea41850` | PASS-2 V6 report | Calibration and sibling READY comparison. |
| `b64a18a1` | PASS-1/PASS-2 V5.1 narrow amendment | Recognizer diagnostics, yaml, WASM, and rare-fence baseline. |
| `HARDENING-PASS-1-PASS-2-V5.1A.md` | V5.1A verification | READY baseline before research fold. |

Required governing text read:

| Source | Load-bearing rule applied |
|---|---|
| `restart/prompts/HARDENING.md:35-42` | Pro / Con / Explication / Challenge discipline. |
| `restart/prompts/HARDENING.md:54-105` | Nine-lane audit contract; Lane 2 applies only to multi-wave targets. |
| `restart/prompts/HARDENING.md:109-147` | Output shape: target identification, lane table, punch list, final readiness. |
| `restart/README.md:450-452` | Voice, path:line citations, no soft hedging, no unreceivered future work. |
| `restart/locks/14-LOCKS.md:34-60` | Locks 1, 2, 4, 5, 8, 10, 13, and 14 applied against PASS-1 scope. |
| `restart/audit/pass-1-substrate/PASS-1.md:1-298` | Direct target, read end-to-end. |
| `restart/research/fold-pass-1.md:1-235` | Fold ownership, adversarial routing, accepted amendment plan, no-escalation verdict. |
| `restart/research/topic-1-hm-foundations.md:492-728` | HM/CSP split, `LayoutFacts` / `TypeFacts` boundary, diagnostic metadata. |
| `restart/research/topic-2-bidirectional.md:293-532` | Directed subsumption, `BBNF-SUBSUMPTION-EDGE`, higher-rank gate closure. |
| `restart/research/topic-3-csp-gadts.md:384-593` | `Object<V>` HM-parametric boundary, finite monomorphization, local-equality diagnostics. |
| `restart/research/topic-4-egraphs.md:736-924` | Stable-key bridge, no representative leakage, `BridgeJustification`. |
| `restart/research/topic-5-cost-models.md:736-894` | `CostDecision`, objective vectors, Pareto/frontier, solver-backed extraction. |
| `restart/research/topic-6-tape.md:366-504` | Tape/direct pressure routed to PASS-2/PASS-3/SYNTHESIS, not PASS-1 amendment. |
| `restart/research/topic-7-green-red-incremental.md:618-747` | Recovery facts and yaml fault-tolerance pressure; PASS-1 owns only producer facts. |
| `restart/research/topic-8-simd-dfa.md:721-931` | SIMD/DFA pressure routed to PASS-2/SYNTHESIS; no PASS-1 syntax widening. |
| `restart/audit/hardening/HARDENING-PASS-2-V6.md` | V6 report shape and sibling READY comparison. |
| `restart/audit/hardening/HARDENING-PASS-3-V6.md` | V6 report shape, gate rerun style, residual routing. |
| `restart/audit/hardening/HARDENING-PASS-1-PASS-2-V5.1A.md:63-71` | PASS-1/PASS-2 READY posture before research fold. |

Core target facts after the research fold:

| PASS-1 site | Current content |
|---|---|
| `PASS-1.md:24-39` | Grammar IR is semantic, keyed by `RuleId` / `NodeId`, and never stores e-node representatives, regex internals, scanner choices, lowerer policy, or BIR refinements. |
| `PASS-1.md:41-57` | Backend IR alphabet is owned by `ir/src/backend_ir/`; PASS-2 refines payloads but cannot re-own or redefine variants. |
| `PASS-1.md:59` | OpenFrame clone stacks are deletion archaeology; generated BIR builder frames plus `TapeBuilder` checkpoints are the replacement. |
| `PASS-1.md:73-75` | HM equality, expected-type check/synth, finite CSP choices, `Object<V>` HM-parametricity, finite monomorphization, and no V1 GADT surface are now explicit. |
| `PASS-1.md:77-79` | CSP/egraph bridge uses stable ids and bridge justifications; cost extraction records `CostDecision`, objectives, selected/rejected/dominated alternatives, and scalarization profile. |
| `PASS-1.md:81-83` | BBNF keeps lookbehind, block-bodied `@host fn`, chains, generics, `@error`, and `@layout`; rewrite-mode and grammar Unicode algebra stay out. |
| `PASS-1.md:85-97` | Rare declaration-crate escape valve is fenced, empty for extant grammars, non-importing, deletion-bound, and synchronized to Architecture review form. |
| `PASS-1.md:99-117` | PASS-1 owns diagnostic strings, type-obligation metadata, and producer-side `RecoveryFacts`. |
| `PASS-1.md:151` | `types/` is internal to the layout boundary; `layout/` emits public `LayoutFacts`; `TypeFacts` stays internal. |
| `PASS-1.md:192-231` | Formal BBNF syntax excludes rewrite-mode and grammar Unicode algebra, uses infix finite-width lookbehind, block-bodied host functions, and rule-level `->` chains only. |
| `PASS-1.md:235-251` | yaml onboarding remains two author inputs: `grammars/yaml.bbnf` plus workspace metadata; generated outputs are derivative and budgeted. |

The V6 question is narrow: did the research fold introduce a PASS-1-blocking amendment, or is PASS-1 still READY? It is still READY. The fold sharpened PASS-1's type, bridge, cost, recovery, and rare-fence contracts without widening grammar syntax or reviving retired surfaces.

## §2 Research-fold evidence map

### §2.1 Fold ownership map

| Research source | PASS-1-relevant pressure | Current PASS-1 evidence | V6 verdict |
|---|---|---|---|
| Topic 1 HM foundations | "CSP-backed unification" must not replace HM equality unification; type diagnostics need inspectable obligation metadata. | `PASS-1.md:73` separates HM equality from finite CSP choices; `PASS-1.md:115` records `expected_from`, `actual_from`, `obligation_id`, and `solver_stage`; `PASS-1.md:151` keeps `TypeFacts` internal and `LayoutFacts` public. | KEEP |
| Topic 2 bidirectional | Bidirectional must mean local expected-type check/synth and directed subsumption, not unbudgeted higher-rank completeness. | `PASS-1.md:73` scopes expected-type flow to signatures, annotations, directives, chains, and subsumption edges; `PASS-1.md:109` and `:231` bind `BBNF-SUBSUMPTION-EDGE`; no higher-rank public pass appears. | KEEP |
| Topic 3 CSP/GADTs | `Object<V>` is rank-1 HM parametric; no GADT/local-equality surface should appear without amendment; monomorphization must be finite. | `PASS-1.md:75` states `Object<V>` HM-parametricity, finite `(RuleId, TypeArgs)` validation, `BBNF-GENERIC-CYCLE`, no V1 GADT surface, and `BBNF-LOCAL-EQUALITY-ANNOTATION` for any future local-equality amendment. | KEEP |
| Topic 4 egraphs | Bridge products must be stable facts and justifications, not e-node representative authority. | `PASS-1.md:39` rejects representative leakage into Grammar IR; `PASS-1.md:77` exchanges monotone facts keyed by stable ids and emits bridge justifications. | KEEP |
| Topic 5 cost models | Scalar-only cost loses objective evidence, Pareto residue, DAG sharing, and regex opacity. | `PASS-1.md:79` records `CostDecision`, objective vectors, legality, child links, rejected/dominated alternatives, scalarization profile, and bridge justification; `PASS-1.md:154` adds `frontier/`, `solve/`, and dominated evidence to `cost-model`. | KEEP |
| Topic 6 tape | Tape/direct materialization and benchmark metadata pressure belongs to runtime/codegen/synthesis unless PASS-1 directly owns a field. | Fold-pass-1 marks Topic 6 out of scope for PASS-1 at `fold-pass-1.md:129` and §3 `T6 A1-A6` at `fold-pass-1.md:168`; PASS-1 keeps only substrate/BIR contract and routes value/path API to PASS-3 at `PASS-1.md:177`, `:186`. | KEEP / routed |
| Topic 7 green/red incremental | Recovery/fault-tolerance needs typed producer facts, but runtime identity and incremental maps are PASS-3/SYNTHESIS owned. | `PASS-1.md:117` emits `RecoveryFacts` with recovery kind, diagnostic code, sync token, typed-placeholder policy, and source span; it explicitly creates no second parse substrate or runtime recovery API. | KEEP |
| Topic 8 SIMD/DFA | Exact/prefilter SIMD, DFA/VM policy, and regex-oracle parity are PASS-2/SYNTHESIS owned; PASS-1 must not add author directives or grammar Unicode algebra. | Fold-pass-1 marks Topic 8 out of scope at `fold-pass-1.md:136`, `:173`; `PASS-1.md:52`, `:68`, `:81`, `:227` keep Pratt/SIMD auto-detected and Unicode below BBNF without adding `@pratt`, `@simd`, or grammar Unicode algebra. | KEEP / routed |

### §2.2 Adversarial reconciliation

| Finding family | Research pressure | PASS-1 fold result | V6 classification |
|---|---|---|---|
| HM + CSP split | Topic 1 A2 and Topic 3 A1 reject "CSP-backed unification" as a solver contract. | `PASS-1.md:73` now says first-order unification solves equality constraints and CSP solves finite non-HM choices. | FOLD-SATISFIED |
| Subsumption edge discipline | Topic 2 D requires rule sites for coercion/subsumption. | `PASS-1.md:109` and `:231` require registered `CoercionCandidate` edges and fail as `BBNF1401` plus `BBNF-SUBSUMPTION-EDGE`. | FOLD-SATISFIED |
| Higher-rank / DK proof burden | Topic 2 C and Topic 3 A3 warn against unbudgeted DK or GADT machinery. | `PASS-1.md:75` keeps V1 rank-1 and reserves local equality to future amendment with annotation diagnostics. | FOLD-SATISFIED |
| Finite monomorphization | Topic 3 A6 says generic monomorphization can become a codegen budget failure. | `PASS-1.md:75` validates a finite `(RuleId, TypeArgs)` instance set and names `BBNF-GENERIC-CYCLE`. | FOLD-SATISFIED |
| Bridge representative instability | Topic 4 A1 says representative promotion is unstable. | `PASS-1.md:39` and `:77` use stable ids and postpone representative choice to extraction. | FOLD-SATISFIED |
| Bridge proof payload | Topic 4 A4 asks for proof/explanation payloads. | `PASS-1.md:77` and `:79` name bridge justifications in extraction evidence. | FOLD-SATISFIED |
| Scalar-only cost | Topic 5 A1/A5 reject one scalar score as durable cost evidence. | `PASS-1.md:79` and `:154` keep objective vectors, scalarization profile, Pareto/frontier, solver-backed extraction, and dominated alternatives. | FOLD-SATISFIED |
| Regex opacity in cost | Topic 5 A3 warns parser extraction must not inspect regex internals. | PASS-1 names regex as a cost pressure and keeps regex internals out of Grammar IR at `PASS-1.md:39`; concrete `RegexCostSummary` is PASS-2/SYNTHESIS. | PASS-1-SATISFIED |
| Tape/direct operational details | Topic 6 A1-A6 require `TapeShape`/`ValueShape`, one identity, benchmark metadata, and entry-point precision. | Fold-pass-1 routes these out at `fold-pass-1.md:129`, `:168`; PASS-2/PASS-3 V6 reports classify them closed for sibling ownership. | RESIDUAL-NON-BLOCKING |
| Recovery nodes and fault tolerance | Topic 7 A3 says recovery must not be untyped syntax debris. | PASS-1 emits producer-side `RecoveryFacts`; PASS-3 owns runtime node shape. | FOLD-SATISFIED-FOR-PASS-1 |
| SIMD/DFA exactness | Topic 8 A1-A5 require exact/prefilter, regex oracle, and no user `@simd`. | Fold-pass-1 routes out at `fold-pass-1.md:136`, `:173`; PASS-1 contains no positive `@simd`, `@pratt`, or grammar Unicode algebra surface. | RESIDUAL-NON-BLOCKING |
| Source/index provenance gaps | Topics 1-8 preserve several INDEX/source hygiene issues. | Fold-pass-1 classifies them as DEFER/OUT-OF-SCOPE with no PASS-1 evidence dependence. | RESIDUAL-NON-BLOCKING |

No §6 research finding is marked ESCALATE for PASS-1. Fold-pass-1 says no lock-file structural change is required at `restart/research/fold-pass-1.md:175-180` and closes with "No accepted PASS-1 fold requires a lock-file structural change" at `restart/research/fold-pass-1.md:230-235`.

## §3 Nine-lane verification table

| Lane | Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|---|
| 1 Lock-Adherence | `PASS-1.md:7-20`, `:73-83`, `:85-97`, `:235-251` | PASS-1 must honor the locks it touches: tape/direct substrate, layout vocabulary, bridged optimization, BIR ownership, no directive-forced Pratt/SIMD, no default declaration crates, and two-surface grammar onboarding. | The fold preserves tape/direct, public `LayoutFacts`, internal `TypeFacts`, bridged CSP/egraph, BIR-only codegen, auto-detected Pratt/SIMD, rare declaration-crate fence, and yaml two-surface proof. | PASS-1 still contains deletion archaeology terms like `OpenFrame` and folded terms like WASM/SIMD in BIR rows. | The strict counterposition says any term hit is a relapse. PASS-1 defeats it because each such hit is a negative invariant, lowerer obligation, or sibling handoff, not a public grammar surface. | KEEP / READY |
| 2 Sequencing-Discipline | `PASS-1.md:170-188`, `fold-pass-1.md:129-136` | PASS-1 is a pass synthesis, not a multi-wave tranche plan; sequencing is judged through handoff tables and receiver gates. | PASS-1 names PASS-2 and PASS-3 receivers, blockers, and receiving gates for BIR, cost, host metadata, tape/direct, recovery facts, path/value, WASM, and TS deferral. | Lane 2 cannot be fully judged within a PASS-only artifact. | The challenge is substrate-first/consumer-later. PASS-1 answers by routing every substrate contract to PASS-2, PASS-3, Architecture, or Master gates. | N/A for wave sequencing; PASS for receiver discipline |
| 3 Cohesion | `PASS-1.md:24-121`, `:142-166`, `:170-188` | Every claim should be verifiable from PASS-1 or a named consumer surface. | Grammar IR, BIR, type algorithm, bridge, cost model, diagnostics, crate tree, and handoffs all name owners and consumers. | Some implementation commands are future gates because crates do not exist yet. | The challenge says future gates are orphan claims. In a restart synthesis, gate text is valid when receiver, blocker, and acceptance surface are named. | KEEP / READY |
| 4 SOTA-Anchoring | `PASS-1.md:14`, `:79`, `:253-263`; sibling V6 SOTA rows | PASS-1 owns cost-evidence shape, not parse-throughput benchmark wins. | It avoids claiming Lock 8 wins from mechanism rows and routes SOTA/budget evidence to PASS-2/PASS-3/Master. | PASS-1 does not itself repeat sonic-rs/simdjson/lightning-css rows. | The challenge demands competitor rows in every PASS. Hardening Lock 8 applies to throughput gates, and PASS-1 has no standalone throughput gate. | KEEP / READY |
| 5 Grammar-Authoritative | `PASS-1.md:81-83`, `:192-243`, `:245-251` | PASS-1 must not widen BBNF syntax, add per-grammar code paths, or make declaration crates default. | Rewrite-mode and grammar Unicode algebra are excluded; host functions are block-bodied; rule chains use `->`; yaml onboarding is grammar source plus metadata only; declaration crates are rare and empty for extant grammars. | PASS-1 names seed grammars in per-X tables and rare-fence rows. | The challenge says seed rows can train overfit implementation. The table is proof evidence required by hardening, and the normative route remains metadata/generator driven. | KEEP / READY |
| 6 Generated-Code + LOC Budget | `PASS-1.md:253-263`, `:75`, `:174` | PASS-1 must define the budget schema and prevent type/generic explosion from becoming codegen output. | It defines `baseline_loc`, `projected_loc`, `allowed_delta`, `pressure_source`, `regen_wall_ms`, and `evidence`; finite `(RuleId, TypeArgs)` validation blocks unbounded generic emission; PASS-2 consumes `CostDecision` evidence. | PASS-1 does not give concrete per-grammar LOC numbers; PASS-2/Master own those. | The challenge says budget schema without numbers is weak. PASS-1 is the substrate owner; PASS-2/Master V6 carry the numeric budget rows. | KEEP / READY |
| 7 Friction-Forecast | `PASS-1.md:99-117`, `:225-231`, `:235-243`, `MASTER-PLAN.md:799-805` | PASS-1-owned user friction is diagnostic vocabulary, not runtime cookbook prose. | It gives verbatim messages for lookbehind, host signature, layout conflict, chain mismatch, subsumption edge, generic cycle, local equality annotation, Pratt, and SIMD non-selection; yaml onboarding and chain syntax are explicit. | Runtime path macros, recovery UX, WASM packaging, and yaml cookbook are sibling/synthesis surfaces. | The challenge says friction is incomplete without full worked examples. That is true for consolidation, but not PASS-1-blocking because the receiver rows are named. | KEEP / READY |
| 8 Carry + Deferral | `PASS-1.md:170-188`, `fold-pass-1.md:230-235` | Carries must name receiver, blocker, and receiving gate; residuals must not hide PASS-1 contradictions. | PASS-1 handoffs do so; fold-pass-1 routes README, Architecture, Master Plan, locks, INDEX, PASS-2, PASS-3, Topic 6, and Topic 8 residue away from PASS-1. | Some residuals are real, especially research INDEX/source hygiene and synthesis wording. | The challenge says all residuals should block. V6 only blocks when the target owns the repair or depends on faulty evidence; PASS-1 does neither. | KEEP / READY |
| 9 Greenfield-Discipline | `PASS-1.md:8`, `:59`, `:81`, `:148`, `:157`, `:292-298` | PASS-1 must replace old failure modes rather than patch around them. | ParseStream is dissolved into source normalization; OpenFrame is deletion archaeology; rewrite-mode and grammar Unicode algebra are absent; closure reuse is research signal only; egraph/CSP/cost stay domain-scoped. | The document carries many inherited terms as negative evidence. | The challenge says negative evidence can become authority. PASS-1 consistently labels it DISCARD, deletion archaeology, regex-layer route, or synthesis receiver. | KEEP / READY |

Cohort count for this PASS-1 V6 audit:

| Class | Count |
|---|---:|
| KEEP / READY rows | 8 |
| N/A with receiver-discipline pass | 1 |
| AMENDMENT-REQUIRED rows | 0 |
| RE-DRAFT rows | 0 |

Lane result: PASS-1 remains the canonical substrate surface after the research fold. No lane produces a PASS-1-local surgery.

## §4 Focused V6 checks

| Check | PASS-1 result | Classification |
|---|---|---|
| HM/bidirectional/CSP research does not widen BBNF grammar syntax | `PASS-1.md:73-75` confines type machinery to layout/type checking, and `PASS-1.md:192-231` keeps the grammar surface fixed. | PASS |
| Egraph/cost-model research does not expose pass-local optimizer structures | `PASS-1.md:39`, `:77`, and `:79` use stable keys, bridge justifications, and extraction evidence without storing e-node representatives in Grammar IR. | PASS |
| Tape research does not revive OpenFrame or split substrate authority | `PASS-1.md:59` replaces OpenFrame clone stacks; Topic 6 fold is routed to sibling surfaces. | PASS |
| SIMD/DFA research does not add `@simd`, `@pratt`, grammar Unicode algebra, or verifier-less tape emission | PASS-1 has no positive `@simd` / `@pratt`; `PASS-1.md:81` and `:227` route Unicode below grammar. Exact/prefilter verifier details are PASS-2/SYNTHESIS. | PASS |
| Rare declaration-crate fence stays rare | `PASS-1.md:85-97` requires approval, failure proof, location, no generic import, deletion path, reviewer, empty extant grammar table, verification, and Architecture review-form sync. | PASS |
| `LayoutFacts` / `passes::layout` vocabulary is coherent | `PASS-1.md:151` states `layout/` is public and emits `LayoutFacts`; `TypeFacts` and type-obligation logs stay internal. `ARCHITECTURE.md:995-1013` and `MASTER-PLAN.md:313` agree. | PASS |
| Lookbehind and diagnostics stay PASS-1-owned and finite-width | `PASS-1.md:34`, `:105`, `:229` bind `Lookbehind`, `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, and `LookbehindWidth`; codegen never sees unbounded lookbehind. | PASS |
| `@host fn` stays block-bodied and chains stay routed | `PASS-1.md:197`, `:225`, `:231` keep block-bodied `@host fn`, method chains inside host bodies only, and rule-level `->` chains. | PASS |
| `@error(recover)`, WASM, diagnostics, yaml, `pointer!`, and `select!` route to correct owners | PASS-1 emits `RecoveryFacts` and BIR/WASM obligations; PASS-2/PASS-3/MASTER own host ABI, path macros, runtime diagnostics, and yaml consumer gates. | PASS |
| No positive `path!`, rewrite-mode, OpenFrame, ParseStream runtime, `@pratt`, `@simd`, `Unicode class algebra`, or per-grammar declaration crate default | Required scans find only deletion, rejection, fold-routing, diagnostic, or sibling-owner contexts. | PASS |

## §5 Gate-rerun results

Minimum commands were run before commit.

| Command | Result |
|---|---|
| `git status --short` | Clean before report creation; after report creation, dirty only `restart/audit/hardening/HARDENING-PASS-1-V6.md`. |
| `git log --oneline -- restart/audit/pass-1-substrate/PASS-1.md` | Latest PASS-1 target commit is `0c72433b docs(restart/pass-1): wave-5-fold amendment - research grounding fold`; earlier fold context includes `88d22b78`. |
| Required retired-surface / routing `rg` command over PASS-1, fold-pass-1, Architecture, Master, and Migration | Completed. Hits classify as accepted PASS-1 evidence, sibling/synthesis ownership, deleted archaeology, or routed residue. No positive `path!`, `@pratt`, `@simd`, rewrite-mode, grammar Unicode algebra, OpenFrame substrate, ParseStream runtime, or default declaration-crate surface is introduced by PASS-1. |
| Required `AMENDMENT|RE-DRAFT|READY|RESIDUAL|punch|receiver|blocker|receiving gate` scan | Completed. It finds V5.1A READY, PASS-1 handoff headers, and fold-pass-1 FOLD/DEFER/OUT-OF-SCOPE classifications; no PASS-1 V6 amendment row appears. |
| `rg -n "TypeFacts|LayoutFacts|passes::layout|CostDecision|RecoveryFacts|BBNF-SUBSUMPTION-EDGE|BBNF-GENERIC-CYCLE|BBNF-LOCAL-EQUALITY-ANNOTATION"` over PASS-1/ARCH/MASTER/MIGRATION | Completed. `TypeFacts` appears only as internal scratch/public-boundary denial; `LayoutFacts`, `CostDecision`, and `RecoveryFacts` appear as intended public/pass facts. |
| `git diff --check` | Clean after report creation. |
| `git status --short` | Dirty only the report before staging; clean after commit. |

Observed classification from the required retired-surface scan:

| Pattern family | PASS-1 / fold classification |
|---|---|
| `path!` | No PASS-1 positive surface; `pointer!` / `select!` are PASS-3/Master path surfaces. |
| `@pratt` / `@simd` | No PASS-1 positive surface; V5.1/V6 sibling reports confirm PASS-2 diagnostics no longer teach directive forcing. |
| `rewrite-mode` | PASS-1 explicitly rejects it at `PASS-1.md:16`, `:81`, `:192`, `:227`, `:282`, `:292`. |
| `Unicode class algebra` | Routed to regex at `PASS-1.md:17`, `:81`, `:157`, `:192`, `:227`, `:274`, `:292`; no BBNF production. |
| `OpenFrame` | Deletion archaeology and replacement only at `PASS-1.md:50`, `:59`, `:298`. |
| `ParseStream` | Rejected rename/source-normalization route at `PASS-1.md:8`, `:148`, `:270`, `:292`. |
| `LayoutFacts` / `TypeFacts` / `passes::layout` | `PASS-1.md:115`, `:151` and ARCH/MASTER agree: `LayoutFacts` public, `TypeFacts` internal. |
| `LookbehindWidth` / `BBNF-LOOKBEHIND-WIDTH` / `BBNF1004` | PASS-1-owned diagnostic chain at `PASS-1.md:99`, `:105`, `:229`. |
| `@host fn` | Block-bodied grammar surface, host-body method-chain owner, metadata/generic primitive route. |
| `WASM` | BIR/lowerer obligation and PASS-2/PASS-3 host ABI route, not a PASS-1 grammar syntax expansion. |
| `yaml` | Two-surface onboarding proof and sibling generated-output consumers. |
| `rare` / `declaration` | Rare fence only; no default per-grammar declaration crates. |

## §6 Cross-document binding ledger

| Binding | PASS-1 anchor | Cross-document anchor | V6 result |
|---|---|---|---|
| Grammar IR stable keys | `PASS-1.md:24-39` | ARCH Grammar IR table and MASTER PASS-1 reconciliation row (`MASTER-PLAN.md:783`) | CLOSED |
| Backend IR ownership | `PASS-1.md:41-57` | PASS-2 BIR-only lowerer contract and import-deny rows | CLOSED |
| OpenFrame deletion | `PASS-1.md:59`, `:298` | ARCH/MASTER/MIGRATION deletion gates for no OpenFrame clone stack | CLOSED |
| Type algorithm | `PASS-1.md:73` | ARCH internal layout typing rows (`ARCHITECTURE.md:995-1013`); MASTER C.W1 (`MASTER-PLAN.md:313`) | CLOSED |
| Generic rules and local-equality boundary | `PASS-1.md:75`, `:110-111` | ARCH future GADT/higher-rank gate (`ARCHITECTURE.md:1161-1164`) | CLOSED |
| CSP/egraph bridge | `PASS-1.md:77` | ARCH `BridgeJustification` row (`ARCHITECTURE.md:1008`); MASTER C.W4/C.W5 bridge gates | CLOSED |
| Cost model evidence | `PASS-1.md:79`, `:154`, `:174` | PASS-2 `CostDecision` and generated budget rows; MASTER C.W5/H/J evidence rows | CLOSED |
| Rare declaration-crate fence | `PASS-1.md:85-97` | ARCH review fields (`ARCHITECTURE.md:747-756`); MASTER rare valve row (`MASTER-PLAN.md:771`) | CLOSED |
| Diagnostics | `PASS-1.md:99-117` | ARCH diagnostic catalogue (`ARCHITECTURE.md:1017-1060`); PASS-2/PASS-3 diagnostic ledgers | CLOSED |
| Recovery producer facts | `PASS-1.md:117`, `:184` | PASS-3 runtime recovery rows; MASTER I.W0 (`MASTER-PLAN.md:522`) | CLOSED |
| BBNF grammar surface | `PASS-1.md:192-231` | ARCH accepted grammar surface (`ARCHITECTURE.md:1070-1073`, `:1121-1135`); MASTER PASS-1 reconciliation row (`MASTER-PLAN.md:783`) | CLOSED |
| yaml onboarding | `PASS-1.md:235-243` | README two surfaces (`README.md:13`); PASS-2/3 yaml rows; ARCH yaml walkthrough (`ARCHITECTURE.md:1336-1376`); MASTER yaml trajectory (`MASTER-PLAN.md:215-224`) | CLOSED |
| Generated budget schema | `PASS-1.md:253-263` | PASS-2 generated LOC table; MASTER generated budget rows (`MASTER-PLAN.md:654-691`) | CLOSED |
| Sibling ownership | `PASS-1.md:170-188`, `fold-pass-1.md:230-235` | PASS-2 V6 and PASS-3 V6 READY reports | CLOSED |

No binding ledger row requires PASS-1 surgery. Remaining synthesis/reporting residue is visible but outside this target.

## §7 V5/V5.1-to-V6 history note

| Cycle | PASS-1 posture |
|---|---|
| V2-V4 | PASS-1 moved from amendment-required to READY after BIR ownership, grammar surface, OpenFrame deletion, yaml proof, generated budgets, and handoffs landed. |
| V5 | PASS-1 itself was internally coherent, but the cohort was AMENDMENT-REQUIRED because downstream Architecture grammar sketch and PASS-2 optimizer diagnostics conflicted with PASS-1 / Lock 10. |
| V5.1 | Narrow PASS-1/PASS-2 amendment removed retired recognizer directive language, strengthened yaml/WASM/rare-fence routes, but left shifted line-citation residue. |
| V5.1A | Citation hygiene residue closed; PASS-1/PASS-2 route returned READY (`HARDENING-PASS-1-PASS-2-V5.1A.md:63-71`). |
| V6 | Research fold adds Topic 1-8 pressure. PASS-1 absorbs owned parts: HM equality vs finite CSP split, local check/synth, subsumption diagnostics, finite generic monomorphization, GADT boundary, bridge justifications, `CostDecision`, `LayoutFacts`/`TypeFacts` boundary, and producer-side `RecoveryFacts`. |

V6 does not overturn V5.1A. It re-tests the READY surface after the research fold and finds the target still READY.

## §8 Punch list

PASS-1-blocking punch list:

| Path:line | Surgery | Acceptance gate | Origin | V6 status |
|---|---|---|---|---|
| none | none | no PASS-1 amendment required | V6 nine-lane audit plus gate rerun | READY |

Residual non-PASS-1 items:

| Path:line | Surgery | Acceptance gate | Origin | PASS-1 blocking? |
|---|---|---|---|---|
| `restart/research/INDEX.md` Topic 1-8 source rows | Repair or mark provenance gaps: lock-number drift, Roc bidirectional role, Schrijvers/Stuckey split, Hubbard, Ungar/Adams, HelpMate, Almomany, Deb, Hyperscan/Vectorscan role. | INDEX rows classify each source as verified primary source or explicit provenance gap. | Topic §6 findings; fold-pass-1 DEFER rows. | No. PASS-1 does not cite these gaps as evidence. |
| README / Architecture type-system explanatory prose | Keep the HM core, local check/synth, finite CSP, bounded coercion, and future DK/GADT gate aligned with PASS-1. | `rg -n "full Hindley-Milner with subsumption|CSP-backed unification"` finds no unscoped active positive wording outside provenance. | Topics 1-3 DEFER/SYNTHESIS rows. | No. PASS-1 already carries the precise contract. |
| MASTER / tranche implementation gates | Ensure C.W1/C.W4/C.W5/D.W3 include type-obligation, bridge-justification, rewrite-budget, and cost-evidence tests. | Tranche close gates name principal schemes, finite choices, bridge facts, representative stability, objective vectors, and dominated alternatives. | Topics 1/4/5 fold residue. | No. This is implementation/synthesis closure. |
| PASS-2 / SYNTHESIS SIMD/regex route | Keep exact/prefilter verifier-before-tape, regex-oracle parity, and no `@simd` / `@pratt` directives. | PASS-2/SYNTHESIS scans show no force directives and scanner tests include exact parity and verifier route. | Topic 8 routed residue. | No. Not a PASS-1-owned surface. |
| PASS-3 / SYNTHESIS tape and incremental route | Keep one tape identity, `TapeShape`/`ValueShape`, snapshot `TapeId`, red-like transient views, recovery node shape, and yaml syntax-error LSP behavior. | PASS-3 and MASTER gates prove runtime identity and recovery consumers. | Topics 6/7 routed residue. | No. PASS-1 only emits producer facts. |

These residuals are not papered over. They are non-blocking for PASS-1 because the target has no write ownership and no accepted fold depends on their unresolved form.

## §9 Final verdict

Verdict: READY.

Grounds:

| Gate | Result |
|---|---|
| Research-fold coherence | PASS. Topics 1-5 and 7 PASS-1-owned deltas are present; Topics 6 and 8 are correctly routed. |
| Nine-lane audit | PASS. Eight KEEP/READY rows, one N/A with receiver-discipline pass, zero amendment rows. |
| Retired-surface scan | PASS. No positive `path!`, `@pratt`, `@simd`, rewrite-mode, grammar Unicode algebra, OpenFrame substrate, ParseStream runtime, or default declaration-crate route. |
| Layout/type vocabulary | PASS. `LayoutFacts` is public; `TypeFacts` and type-obligation logs remain internal. |
| Rare declaration crate | PASS. Rare fence remains fenced, non-default, deletion-bound, and empty for extant grammars. |
| YAML onboarding | PASS. PASS-1 still counts only grammar source plus workspace metadata as author inputs. |
| Gate rerun | PASS. Required scans classify all hits; `git diff --check` is clean. |
| Punch list | PASS. No PASS-1-local surgery. |

Re-draft thresholds met: zero.

Amendment-required threshold for PASS-1 met: zero.

Residuals remain outside PASS-1: research-index/source hygiene, synthesis wording/gate detail, PASS-2 SIMD/regex verifier policy, PASS-3 runtime identity and recovery details. They should stay visible to consolidation, but they do not block PASS-1.

## §10 Closing posture

PASS-1 is fit for V6 consolidation as READY.

The research fold made the substrate sharper rather than wider. HM equality unification, Pierce-Turner local check/synth, finite CSP choice, `Object<V>` rank-1 parametricity, no V1 GADT surface, stable-id bridge facts, `CostDecision` evidence, public `LayoutFacts`, internal `TypeFacts`, producer-side `RecoveryFacts`, block-bodied `@host fn`, infix finite lookbehind, rule-level chains, rare declaration-crate fencing, and yaml two-surface onboarding all cohere.

Hereupon the consolidator should treat PASS-1 as READY, route the non-PASS-1 residuals by receiver, and avoid a redundant PASS-1 amendment pass.
