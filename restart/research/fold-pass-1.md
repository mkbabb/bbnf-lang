# PASS-1 research fold classification

## §1 Target and source artefacts read

Target surface: `restart/audit/pass-1-substrate/PASS-1.md`.

Classification output: `restart/research/fold-pass-1.md`.

Allowed amendment surface for this worker after classification:
`restart/audit/pass-1-substrate/PASS-1.md` only.

Primary fold scope:

| Topic | Fold scope for PASS-1 |
|---|---|
| Topic 1 | Full: HM foundation, Algorithm W, constrained diagnostics, `TypeFacts` / `LayoutFacts` boundary. |
| Topic 2 | Full: bidirectional local inference, directed subsumption edges, higher-rank guardrails. |
| Topic 3 | Full: HM(X), OutsideIn boundary, parametric generics, finite monomorphization. |
| Topic 4 | Partial: only e-graph/CSP bridge evidence that touches Grammar IR producer keys or PASS-1 bridge rows. |
| Topic 5 | Partial: only cost evidence and extraction API shape owned by PASS-1 `cost-model` rows. |
| Topic 6 | Out of scope unless a directly cited PASS-1 obligation exists. None was found. |
| Topic 7 | Partial: only Grammar-IR-side recovery/fault-tolerance facts that PASS-3 consumes. |
| Topic 8 | Out of scope unless a directly cited PASS-1 obligation exists. None was found. |

Required artefacts read:

| Artefact | Use in this classification |
|---|---|
| `restart/research/topic-1-hm-foundations.md` | Type stack decomposition, diagnostics, `passes/types` internality, record-subtyping routing. |
| `restart/research/topic-2-bidirectional.md` | Pierce-Turner local inference, DK higher-rank proof gate, subsumption-edge diagnostics. |
| `restart/research/topic-3-csp-gadts.md` | HM unification vs CSP finite choices, `Object<V>` parametricity, OutsideIn boundary, generic-cycle diagnostics. |
| `restart/research/topic-4-egraphs.md` | Stable Grammar IR keys, e-class representative leakage, bridge justification. |
| `restart/research/topic-5-cost-models.md` | `CostModel` / `CostDecision`, objective vectors, Pareto/frontier evidence, solver-backed extraction. |
| `restart/research/topic-6-tape.md` | No PASS-1-owned §5/§7 item found; tape/value refinements route to PASS-2/PASS-3/SYNTHESIS. |
| `restart/research/topic-7-green-red-incremental.md` | Recovery nodes and typed placeholder pressure; only PASS-1 `ErrorDirective` / recovery fact producer wording is in scope. |
| `restart/research/topic-8-simd-dfa.md` | No PASS-1-owned §5/§7 item found; SIMD/regex refinements route to PASS-2/SYNTHESIS. |
| `restart/research/INDEX.md` §2/§3 | Research output contract, topic routing, adversarial-finding obligation. |
| `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` Phase 2 | PASS-1 fold routing and §6 escalation rule. |
| `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` §1 | Verify-then-patch discipline. |
| `restart/README.md` gestalt and §13 | Voice, lock posture, current risky type wording, two-surface grammar authority. |
| `restart/locks/LOCKS.md` | Lock 1, Lock 2, Lock 4, Lock 10, Lock 13, Lock 14 boundaries. |
| `restart/audit/pass-1-substrate/PASS-1.md` | Current target text and amendment anchor lines. |
| `HARDENING-PASS-1-V5.md` | PASS-1 V5 blockers and watch items. |
| `HARDENING-CONSOLIDATED-V5.md` | V5 bundle map and narrow-amendment posture. |
| `HARDENING-PASS-1-PASS-2-V5.1.md` | PASS-1/PASS-2 V5.1 residue and H-class citation rule. |
| `HARDENING-PASS-1-PASS-2-V5.1A.md` | V5.1A READY posture and section-scoped citation preference. |

Current PASS-1 anchor rows verified before patch planning:

| PASS-1 site | Current claim |
|---|---|
| `PASS-1.md:24-37` | Grammar IR semantic variants and stable keys. |
| `PASS-1.md:71` | HM inference, bidirectional checking, CSP-backed constrained unification. |
| `PASS-1.md:73` | CSP/e-graph bridge as fact exchange plus extraction decisions. |
| `PASS-1.md:75` | `AnalysisCost`-style cost trait and selected/rejected alternatives. |
| `PASS-1.md:97-106` | Diagnostic strings including lookbehind, host signature, chain, Pratt, SIMD. |
| `PASS-1.md:140` | `passes/types` and `passes/layout` split. |
| `PASS-1.md:172-174` | Error vocabulary hand-off to PASS-3. |
| `PASS-1.md:220` | Chain flow and `BBNF1401` first-mismatch rule. |

## §2 Item routing table

| Source item | Target | Route | Rationale |
|---|---:|---|---|
| T1 §5 F1-F6 | README §7 | DEFER | Correct but outside this worker's write scope. PASS-1 receives the same mechanism through F11/F12/F17. |
| T1 §5 F7-F10 | Architecture §8 / side tables | DEFER | Architecture/SYNTHESIS work; no PASS-1 edit allowed in first commit or synthesis surfaces in second. |
| T1 §5 F11 | `PASS-1.md:71` | FOLD | PASS-1 must distinguish equality unification from finite CSP choices. |
| T1 §5 F12 | `PASS-1.md:140` | FOLD | Reconciles Lock 2 by keeping `types/` internal and `layout/` as the public pass boundary. |
| T1 §5 F13-F16 | Master Plan | DEFER | Tranche gates and hard-close commands are outside PASS-1 scope. |
| T1 §5 F17 | `PASS-1.md:97-106` | FOLD | Type diagnostics need inspectable obligation metadata without exposing public `TypeFacts`. |
| T1 §5 F18 | Future TAPL citation hygiene | DEFER | No TAPL citation is introduced in PASS-1; keep as V6/provenance gate. |
| T1 §7 S1-S8 | README/Architecture | DEFER | Correct receiving surfaces, but not this worker's amendment surface. |
| T1 §7 S9 | `PASS-1.md:71` | FOLD | Same accepted delta as T1 F11. |
| T1 §7 S10 | `PASS-1.md:140` | FOLD | Same accepted delta as T1 F12. |
| T1 §7 S11 | `PASS-1.md:97-106` | FOLD | Add diagnostic metadata contract after the table. |
| T1 §7 S12-S15 | Master Plan / test gates | DEFER | Test names and tranche gates route to C/D implementation surfaces. |
| T1 §7 S16 | Future citation rule | DEFER | No PASS-1 TAPL text is folded. |
| T1 §7 S17 | README/Architecture record fence | DEFER | The concept informs PASS-1 coercion wording, but the named targets are not PASS-1. |
| T1 §7 S18 | Research index | DEFER | INDEX edits are denied. |
| T2 §5 R1-R3 | README §7 | DEFER | Outside write scope; PASS-1 receives local-inference consequences via R6/R7/R11. |
| T2 §5 R4-R5 | Architecture §8 | DEFER | SYNTHESIS fold owns the Architecture higher-rank gate. |
| T2 §5 R6 | `PASS-1.md:71` | FOLD | Bidirectional checking must mean Pierce-Turner local expected-type flow at signatures, directives, chains, and subsumption edges. |
| T2 §5 R7 | `PASS-1.md:101-106` | FOLD | PASS-1 owns diagnostic vocabulary; add a directed-subsumption failure alias instead of informal coercion. |
| T2 §5 R8 | Master Plan C.W1 | DEFER | Tranche implementation gate. |
| T2 §5 R9-R10 | Research index | DEFER | INDEX edits are denied and provenance cleanup is out of scope. |
| T2 §5 R11 | `PASS-1.md:220` | FOLD | Chain flow must admit coercion only through registered directed candidates and fail otherwise. |
| T2 §5 R12 | Architecture §8 | DEFER | Higher-rank guardrail belongs in SYNTHESIS; PASS-1 gets a local boundary note through T3 items. |
| T2 §7 P1-P2 | README/Architecture | DEFER | Correct scope is SYNTHESIS. |
| T2 §7 P3 | `PASS-1.md:101-106`, `PASS-1.md:220` | FOLD | PASS-1 slice of `CoercionCandidate` / `BBNF-SUBSUMPTION-EDGE` is accepted. |
| T2 §7 P4-P5 | Research index | DEFER | INDEX edits are denied. |
| T2 §7 P6 | Master Plan C.W1 | DEFER | Tranche gate. |
| T2 §7 P7 | PASS-1 chain diagnostic receiver | FOLD | Fold as a compact chain/recovery/host-overload stress note in PASS-1 diagnostics/chain wording. |
| T2 §7 P8-P11 | INDEX/Master/source legend | DEFER | Out of PASS-1 amendment scope. |
| T2 §7 P12-P15 | Future fixtures | DEFER | Test fixtures are future implementation gates, not PASS-1 prose. |
| T3 §5 F1-F3 | README §7 | DEFER | README edits are outside scope; PASS-1 gets the mechanism split. |
| T3 §5 F4 | Generic monomorphization invariant | FOLD | PASS-1 producer must state finite `(RuleId, TypeArgs)` instance validation and rejection diagnostics. |
| T3 §5 F5-F6 | Architecture pipeline / local equality | DEFER | SYNTHESIS owns pipeline naming and Architecture guardrail; PASS-1 receives diagnostic boundary only. |
| T3 §5 F7 | `PASS-1.md:71` | FOLD | Host overloads with determining arguments should be explicit improvement constraints before finite CSP choice. |
| T3 §5 F8 | README subtyping | DEFER | Outside scope; PASS-1 subsumption-edge wording covers the local obligation. |
| T3 §5 F9 | Research index | DEFER | INDEX edits are denied. |
| T3 §7 Srg1-Srg4 | README/Architecture | DEFER | Not PASS-1 surfaces. |
| T3 §7 Srg5 | `PASS-1.md:71` | FOLD | Replace "CSP-backed constrained unification" with HM unification, reserved OutsideIn local equalities, and finite CSP choice. |
| T3 §7 Srg6 | PASS-1 finite gate evidence | FOLD | Add generic monomorphization finiteness and generic-cycle diagnostic ownership in PASS-1. |
| T3 §7 Srg7 | `PASS-1.md:71` | FOLD | Add improvement-constraint wording for host overload typing. |
| T3 §7 Srg8-Srg9 | README / INDEX | DEFER | Outside write scope. |
| T3 §7 Srg10 | `PASS-1.md:97-106` | FOLD | Add generic-cycle and local-equality annotation diagnostics. |
| T4 §5 R1-R2 | README bridge | DEFER | README owns bridge prose; PASS-1 receives only stable-key/no-representative leakage via T4 S12. |
| T4 §5 R3-R4 | Architecture bridge | DEFER | SYNTHESIS owns public bridge API rows. |
| T4 §5 R5-R7 | Master/README rewrite budgets | DEFER | Tranche/rewrite-gate work is outside PASS-1. |
| T4 §5 R8-R9 | README/Architecture cost constraints | DEFER | Cost facts are folded from Topic 5 into PASS-1; broader SYNTHESIS remains deferred. |
| T4 §5 R10-R12 | INDEX/Master/Architecture | DEFER | Outside PASS-1 scope except T4 S12. |
| T4 §7 S1-S7 | README/Architecture/Master | DEFER | Correct receiving surfaces are SYNTHESIS. |
| T4 §7 S8 | Locks / Architecture rationale | DEFER | Lock edits are denied; no structural contradiction found. |
| T4 §7 S9-S11 | INDEX/Master | DEFER | Outside write scope. |
| T4 §7 S12 | `PASS-1.md:31-52`, bridge rows | FOLD | Add PASS-side receiver note: Grammar IR stable keys feed bridge/egraph/cost consumers; no e-node representative or lowerer policy leaks into Grammar IR. |
| T5 §5 R1-R2 | README cost trait | DEFER | README/SYNTHESIS owns public explanation; PASS-1 gets API shape via R3/R4. |
| T5 §5 R3 | `PASS-1.md:75` | FOLD | Split analysis facts, cost scoring, and solver-backed extraction; name `CostDecision` evidence. |
| T5 §5 R4 | `PASS-1.md:143` | FOLD | Add `frontier/` and `solve/`; evidence includes dominated alternatives. |
| T5 §5 R5-R6 | Architecture/Master | DEFER | SYNTHESIS fold. |
| T5 §5 R7-R9 | PASS-2/PASS-3 | OUT-OF-SCOPE | Sibling fold workers own those surfaces. |
| T5 §5 R10 | Research index | DEFER | INDEX edits are denied. |
| T5 §7 S1 | README | DEFER | Outside scope. |
| T5 §7 S2 | `PASS-1.md:75` | FOLD | Same accepted delta as T5 R3. |
| T5 §7 S3 | `PASS-1.md:143` | FOLD | Same accepted delta as T5 R4. |
| T5 §7 S4-S5 | Architecture/Master | DEFER | SYNTHESIS fold. |
| T5 §7 S6-S8/S11 | PASS-2/PASS-3 | OUT-OF-SCOPE | Sibling fold workers own those surfaces. |
| T5 §7 S9-S10 | Research index | DEFER | INDEX edits are denied. |
| T5 §7 S12 | Master carry row | DEFER | Tranche carry ledger is outside PASS-1. |
| T6 §5/§7 all items | README/Architecture/PASS-2/PASS-3/INDEX/future gates | OUT-OF-SCOPE | No directly cited PASS-1 obligation exists; tape materialization routes to sibling workers. |
| T7 §5 R1-R5 | README/PASS-3 | OUT-OF-SCOPE | Runtime identity and reparse plans are PASS-3/SYNTHESIS work. |
| T7 §5 R6 | PASS-3 recovery node shape | FOLD | PASS-1 owns `ErrorDirective` producer facts; fold only recovery fact metadata consumed by PASS-3. |
| T7 §5 R7-R8 | Master Plan | DEFER | Tranche/fault-friction rows are outside PASS-1. |
| T7 §7 S1-S5 | README/PASS-3 | OUT-OF-SCOPE | Runtime/incremental surface. |
| T7 §7 S6 | PASS-3 recovery node shape | FOLD | PASS-1 adds the producer-side `RecoveryFacts` obligation, not the runtime node API. |
| T7 §7 S7-S9 | Master/INDEX | DEFER | Outside PASS-1 scope. |
| T8 §5/§7 all items | README/Architecture/PASS-2/agent docs/Master | OUT-OF-SCOPE | No directly cited PASS-1 obligation exists; SIMD/regex fold belongs to PASS-2/SYNTHESIS. |

## §3 §6 adversarial finding reconciliation

| Finding | Preserved adversarial text | Classification | Rationale |
|---|---|---|---|
| T1 A1 | "`Full Hindley-Milner with subsumption` is too strong." | DEFER | Contradicts README wording, not lock file text. PASS-1 folds bounded/direct subsumption edges; README/Architecture wording routes out. |
| T1 A2 | "`CSP-backed unification` confuses the solver contract." | FOLD | PASS-1 line 71 uses that contract; locks survive if PASS-1 distinguishes HM equality unification from finite CSP choices. |
| T1 A3 | "Typed-record narrowing lacks a selected record type theory." | DEFER | Named amendment targets README/Architecture. PASS-1 folds only directed `CoercionCandidate` and rejects informal open structural subtyping by implication. |
| T2 A | "Lock numbering drift" in the research index. | DEFER | INDEX edit is denied; actual lock file is not contradicted. |
| T2 B | "`full HM with subsumption` is too strong." | DEFER | README wording weakness; lock file survives. PASS-1 receives local subsumption-edge mechanics. |
| T2 C | "DK completeness is cited before its surface exists." | DEFER | README/Architecture/Master proof-gate wording. PASS-1 folds no higher-rank implementation promise. |
| T2 D | "Coercion examples need rule sites." | FOLD | PASS-1 owns chain-step diagnostics and can name registered directed subsumption candidates. |
| T2 E | "Roc source role is overclaimed." | DEFER | Research INDEX/source hygiene only. |
| T3 A1 | "README overstates CSP as unification." | FOLD | PASS-1 has the same dangerous phrase class at line 71; fold the precise solver split. |
| T3 A2 | "`Object<V>` does not justify GADT/OutsideIn machinery." | FOLD | PASS-1 can state V1 generics are HM-parametric and OutsideIn local equalities are reserved for future constructs. |
| T3 A3 | "GADT-style inference can lack principal and finite maximal types." | FOLD | Add annotation/rejection diagnostic boundary for future local equalities and generic-cycle finiteness. |
| T3 A4 | "Lock 10 is misidentified as generics in the research index." | DEFER | INDEX fault; active lock file is not contradicted. |
| T3 A5 | "Subtyping wording collapses polymorphism categories." | DEFER | README/Architecture wording; PASS-1 folds explicit edge diagnostics only. |
| T3 A6 | "Monomorphization lacks an explicit finiteness gate." | FOLD | PASS-1 is the producer/validator surface for generic-rule instance sets. |
| T3 A7 | "Verified-source gap must be preserved." | DEFER | Bibliography/INDEX cleanup. |
| T4 A1 | "Representative promotion is unstable." | FOLD | Lock 4 survives; PASS-1 can strengthen stable-key/no-representative bridge producer wording. |
| T4 A2 | "Lock 4 lacks the egglog counterargument." | DEFER | Lock/Architecture rationale outside scope; no structural lock contradiction. |
| T4 A3 | "Seven rewrite categories need budget gates." | DEFER | README/Master rewrite gates, not PASS-1. |
| T4 A4 | "Bridge facts need proof/explanation payloads." | FOLD | PASS-1 bridge row can name justification evidence without exposing generic egraph internals. |
| T4 A5 | "Source catalogue provenance gap." | DEFER | Research INDEX/source hygiene. |
| T5 A1 | "Scalar trait is too strong." | FOLD | PASS-1 cost trait line currently says `AnalysisCost`; fold multi-objective `CostDecision` evidence. |
| T5 A2 | "Branch iterator double-counts DAG sharing." | FOLD | PASS-1 extraction/evidence wording can name stable child identities and optional solver-backed extraction. |
| T5 A3 | "Shared-with-regex can violate domain opacity." | FOLD | PASS-1 cost API can accept opaque regex summaries as evidence without importing regex internals. |
| T5 A4 | "Topic 5 lock pointer is stale." | DEFER | Research INDEX cleanup only. |
| T5 A5 | "SMT-backed cost composition is under-specified." | FOLD | PASS-1 cost model row can name solver-backed extraction and objective evidence. |
| T5 A6 | "Named source provenance gaps must not become evidence." | DEFER | Bibliography/INDEX cleanup. |
| T6 A1-A6 | Tape/materialization/benchmark/source-entry findings. | OUT-OF-SCOPE | No PASS-1 target; sibling PASS-2/PASS-3/SYNTHESIS folds own them. |
| T7 A1-A2 | One representation and stable identity underspecified. | OUT-OF-SCOPE | Runtime/SYNTHESIS wording, not PASS-1. |
| T7 A3 | "bbnf must not claim unique value from ERROR/MISSING nodes." | FOLD | Only the PASS-1 producer side: `ErrorDirective` emits recovery facts with placeholder/diagnostic policy. |
| T7 A4 | "E-graph cache survival is too parser-local." | DEFER | README/PASS-3/Tranche I query invalidation gates. |
| T7 A5 | "Ungar/Adams and HelpMate are provenance gaps." | DEFER | Bibliography/INDEX cleanup. |
| T8 A1-A5 | SIMD/DFA/regex findings. | OUT-OF-SCOPE | PASS-2/SYNTHESIS surfaces; no PASS-1 amendment. |

Escalation classification:

No §6 item is marked ESCALATE. The lock file itself is not structurally
contradicted by the accepted PASS-1 fold. The adversarial findings weaken
README, Architecture, INDEX, PASS, or gate wording, but Lock 1, Lock 2, Lock 4,
Lock 10, Lock 13, and Lock 14 survive.

## §4 Accepted amendment plan for PASS-1

1. Replace the type-system algorithm sentence at `PASS-1.md:71` with a four-part contract: HM equality constraints/principal schemes; expected-type check/synth for signatures, annotations, directives, chains, and subsumption edges; reserved OutsideIn-style implications only for future local equality constructs; finite CSP choices for host/layout/recognizer/materialization/recovery/backend/extraction.
2. Strengthen the bridge sentence at `PASS-1.md:73`: bridge facts are keyed by stable Grammar IR node IDs / e-class IDs / CSP variables; no e-node representative or lowerer policy leaks into Grammar IR; extraction consumes solved assignments and bridge justifications.
3. Replace the cost model line at `PASS-1.md:75` with `Analysis` facts, `CostModel` scoring, `CostDecision` evidence, objective vectors, legality, selected/rejected/dominated alternatives, and optional solver-backed extraction.
4. Add PASS-1-owned diagnostics after the existing table: directed subsumption failure, generic-cycle/unbounded monomorphization, local-equality annotation requirement, and type-diagnostic metadata (`expected_from`, `actual_from`, `obligation_id`, `solver_stage`).
5. Clarify the `passes` rationale at `PASS-1.md:140`: `types/` is internal to the layout-lowering boundary; `layout/` emits public `LayoutFacts`; `TypeFacts` and type-obligation logs stay internal.
6. Expand the `cost-model` crate rationale at `PASS-1.md:143` with `frontier/`, `solve/`, objective scalarizers, Pareto/lexicographic filtering, and dominated-alternative evidence.
7. Add Grammar-IR producer notes where appropriate: generic rule monomorphization is finite by validated `(RuleId, TypeArgs)` sets; recursive generic cycles require decreasing structure, explicit return annotation, or rejection.
8. Add the PASS-1 side of Topic 7: `ErrorDirective` / recovery validation emits `RecoveryFacts` with diagnostic code, sync token, recovery kind, and typed-placeholder policy for PASS-3 consumption; no second substrate is created.
9. Amend chain-flow prose at `PASS-1.md:220`: directed subsumption/coercion is permitted only through a registered candidate at that edge; otherwise emit `BBNF1401` plus the subsumption-edge diagnostic.

## §5 Gate plan

Before PASS-1 amendment commit:

| Gate | Expected result |
|---|---|
| `git status --short` | Dirty only in `restart/audit/pass-1-substrate/PASS-1.md` after commit 1. |
| Required `rg` scan over PASS-1 and fold report | Hits classified as accepted terminology, routed residue, or absent stale text. |
| `git diff --check` | Clean. |
| `git diff --cached --check` | Clean after staging only PASS-1. |

Additional local checks:

| Check | Purpose |
|---|---|
| `rg -n "CSP-backed constrained unification|AnalysisCost|passes/types|BBNF-SUBSUMPTION-EDGE|BBNF-GENERIC-CYCLE|BBNF-LOCAL-EQUALITY-ANNOTATION|CostDecision|frontier/|solve/|RecoveryFacts" restart/audit/pass-1-substrate/PASS-1.md` | Verify accepted terms landed and old overloaded wording is gone or quarantined. |
| `rg -n "TypeFacts|LayoutFacts|row-polymorphism|structural record|OutsideIn|GADT|higher-rank" restart/audit/pass-1-substrate/PASS-1.md` | Verify internal/public type fact boundary and no accidental higher-rank/GADT commitment. |
| `rg -n "@pratt|@simd|path!|Wave 4" restart/audit/pass-1-substrate/PASS-1.md restart/research/fold-pass-1.md` | Confirm no retired recognizer directives or stale wave wording appears outside classified report context. |

## §6 Classification verdict

Verdict: AMEND PASS-1, no lock escalation.

Accepted fold:

| Area | Accepted PASS-1 result |
|---|---|
| Type system | HM foundation, expected-type checking, finite CSP, reserved local-equality implications. |
| Subsumption | Directed `CoercionCandidate` / `BBNF-SUBSUMPTION-EDGE`; no informal coercion tower. |
| Generics | V1 `Object<V>` as HM-parametric; finite monomorphization; generic-cycle rejection. |
| GADT boundary | No V1 GADT surface; future local equalities require annotation diagnostics and OutsideIn-style gate. |
| Type facts | `TypeFacts` and type-obligation logs internal; `LayoutFacts` public. |
| Bridge | Stable IDs and bridge justifications; no e-node representative leakage into Grammar IR. |
| Cost model | `CostDecision`, objective vectors, Pareto/frontier, solver-backed extraction, dominated alternatives. |
| Recovery | PASS-1 emits recovery facts for PASS-3; no second substrate. |

Routed residue:

README, Architecture, Master Plan, locks, research INDEX, PASS-2, PASS-3,
Topic 6 tape details, and Topic 8 SIMD/regex details remain with their sibling
fold workers or later synthesis routes. No accepted PASS-1 fold requires a
lock-file structural change.
