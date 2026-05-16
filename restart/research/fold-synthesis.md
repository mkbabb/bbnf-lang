# SYNTHESIS Fold Classification - Topics 1-8

## §1 - Target and source artefacts read

Target surfaces:

| Surface | Fold role |
|---|---|
| `restart/ARCHITECTURE.md` | Primary synthesis receiver: §4.4 regex crate tree, §6 pipeline, §7.3 side tables, §7.4 diagnostics, §8.2 type system, §9 runtime, §10 lowerers, §11 performance metadata. |
| `restart/MIGRATION.md` | Corpus-citation and migration-posture receiver: sister crate disposition, new-facility rows, tranche sequence, migration gates, corpus hygiene. |
| `restart/MASTER-PLAN.md` | Tranche evidence receiver: C.W1-C.W5, D.W1-D.W4, H.W1, I.W1, lock ownership, carry/friction ledger. |

Research artefacts read end-to-end for fold routing:

| Topic | Artefact | Fold pressure |
|---:|---|---|
| 1 | `restart/research/topic-1-hm-foundations.md` | HM core, first-order equality, finite coercion, generic instantiation, chain checking. |
| 2 | `restart/research/topic-2-bidirectional.md` | Pierce-Turner local check/synth, DK higher-rank gate, subsumption rule sites. |
| 3 | `restart/research/topic-3-csp-gadts.md` | HM/CSP split, `Object<V>` as rank-1 parametric scheme, OutsideIn only for local equality. |
| 4 | `restart/research/topic-4-egraphs.md` | Stable bridge IDs, monotone bridge facts, proof refs, representative choice at extraction. |
| 5 | `restart/research/topic-5-cost-models.md` | `CostDecision`, objective vectors, Pareto/frontier evidence, solver-backed extraction posture. |
| 6 | `restart/research/topic-6-tape.md` | One authoritative tape identity, typed projections, payload policy, benchmark metadata. |
| 7 | `restart/research/topic-7-green-red-incremental.md` | Snapshot-scoped `TapeId`, reuse maps, query invalidation, typed recovery posture. |
| 8 | `restart/research/topic-8-simd-dfa.md` | `RegexProgram` verifier, exact/prefilter SIMD modes, `regex-automata` oracle, conditional SIMD posture. |

Shared authority read:

| Source | Use |
|---|---|
| `restart/research/INDEX.md` §2/§3 | Output contract, topic anchors, source-catalogue drift. |
| `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` Phase 2 | Fold/escalation boundary: §5/§7 fold, §6 escalates only on lock contradiction. |
| `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` §1 | Verify-then-patch discipline. |
| `restart/README.md` and §13 | Gestalt, locks summary, voice discipline, out-of-scope README edits. |
| `restart/locks/LOCKS.md` | Actual lock text, especially Lock 1, Lock 4, Lock 10, Lock 14. |
| PASS-1/PASS-2/PASS-3 cited rows | Scope classification for PASS-owned edits. |
| `HARDENING-MASTER-PLAN-V5.md`, `HARDENING-CONSOLIDATED-V5.md`, `HARDENING-SYNTHESIS-V5.1.md` | V5/V5.1 posture, current trio status, no-redraft baseline. |

Current trio anchors verified before classification:

| Surface | Current line(s) | Current issue |
|---|---:|---|
| Architecture pipeline | `restart/ARCHITECTURE.md:768-800` | Later `CSP solve` needs extraction-time disambiguation. |
| Architecture side tables | `restart/ARCHITECTURE.md:975-994` | `CostFacts` and `TypeFacts` need research evidence detail without exporting `TypeFacts`. |
| Architecture diagnostics | `restart/ARCHITECTURE.md:1021-1022` | SIMD rejection says only dispatch cost. |
| Architecture type system | `restart/ARCHITECTURE.md:1117-1132` | HM/bidirectional/CSP wording is still compressed. |
| Architecture runtime | `restart/ARCHITECTURE.md:1179-1202` | Tape lacks payload child and direct identity/payload-slot invariant. |
| Architecture performance | `restart/ARCHITECTURE.md:1264-1271` | Benchmark metadata lacks validation/source/materialisation modes. |
| Master C/D | `restart/MASTER-PLAN.md:313-317`, `:346-356` | Type, bridge, and cost evidence gates are under-specified. |
| Master H/I | `restart/MASTER-PLAN.md:475`, `:519` | SIMD scanner and incremental fallback gates are under-specified. |
| Master locks/carry | `restart/MASTER-PLAN.md:698`, `:771-781`, `:798` | Lock 4 close proof and carry rows need folded evidence. |
| Migration | `restart/MIGRATION.md:452-493`, `:563-581`, `:657-667`, `:724-770` | Corpus citations and migration posture need regex/runtime/SIMD/type specificity. |

## §2 - Item routing table

Legend: FOLD means accepted into the SYNTHESIS trio amendment. DEFER means valid but denied to this worker or future implementation/docs. OUT-OF-SCOPE means PASS/README/research-index/lock/crate edits outside the allowed files. ESCALATE is reserved for an actual structural lock contradiction; no such item is accepted in this report.

### Topic 1 - HM foundations

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T1-F1..F6 | OUT-OF-SCOPE | README §7 | README edits are denied; concepts are folded through Architecture/Master. |
| T1-F7 | FOLD | ARCH `1117` | Replace compressed stack with HM core, expected check/synth, bounded coercion, finite CSP. |
| T1-F8 | FOLD | ARCH `1127` | Add first-order equality/unification row. |
| T1-F9 | FOLD | ARCH `1128` | Annotation language becomes expected-type checking, not narrowing bypass. |
| T1-F10 | FOLD | ARCH `980-994` | Retain type obligation logs internally for diagnostics. |
| T1-F11..F12 | OUT-OF-SCOPE | PASS-1 | PASS-1 worker owns algorithm and crate-tree wording. |
| T1-F13 | FOLD | MASTER `313` | C.W1 must split HM equality, expected checking, and finite CSP. |
| T1-F14 | FOLD | MASTER `346` | D.W1 adds scheme instantiation and monomorphisation evidence. |
| T1-F15 | FOLD | MASTER `348` | D.W3 adds first-mismatch chain diagnostics. |
| T1-F16 | FOLD | MASTER `353-356` | Hard close gains type-obligation/principal-core/chain-flow tests. |
| T1-F17 | OUT-OF-SCOPE | PASS-1 | Diagnostic metadata table is PASS-owned. |
| T1-F18 | FOLD | ARCH/MIGRATION citations | TAPL claims stay chapter-topic only unless locally sourced. |
| T1-S1..S5 | OUT-OF-SCOPE | README | Denied surface. |
| T1-S6..S8 | FOLD | ARCH `975-1132` | Type decomposition and internal `TypeObligationLog` are SYNTHESIS-owned. |
| T1-S9..S11 | OUT-OF-SCOPE | PASS-1 | PASS worker owns. |
| T1-S12..S15 | FOLD | MASTER `313`, `346-356` | C/D tranche evidence gates. |
| T1-S16 | FOLD | ARCH/MIGRATION | Citation hygiene rule affects trio wording. |
| T1-S17 | FOLD | ARCH `1127-1132` | Record narrowing is finite generated-shape coercion; open rows routed. |
| T1-S18 | DEFER | Research index | Research index denied. |

### Topic 2 - Bidirectional typechecking

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T2-R1..R3 | OUT-OF-SCOPE | README | Denied surface; mechanism folded in ARCH. |
| T2-R4 | FOLD | ARCH `1117-1121` | V1 is rank-1 HM plus local check/synth; DK gate closed. |
| T2-R5 | FOLD | ARCH `1128` | Annotations select checking mode and contradictions diagnose. |
| T2-R6..R7 | OUT-OF-SCOPE | PASS-1 | PASS-owned diagnostics. |
| T2-R8 | FOLD | MASTER `313` | C.W1 closes DK higher-rank gate for V1. |
| T2-R9..R10 | DEFER | Research index | Index edits denied. |
| T2-R11 | OUT-OF-SCOPE | PASS-1 | PASS-owned chain diagnostics. |
| T2-R12 | FOLD | ARCH `1132` | Higher-rank/existential/indexed surfaces need later proof gate. |
| T2-P1 | OUT-OF-SCOPE | README | Denied surface. |
| T2-P2 | FOLD | ARCH `1117-1132`, MASTER `313` | Scope DK as conditional higher-rank proof gate. |
| T2-P3 | FOLD | ARCH `1127-1132` | Add coercion/subsumption fact vocabulary; PASS diagnostic part out-of-scope. |
| T2-P4..P5 | DEFER | Research index | Denied surface. |
| T2-P6 | FOLD | MASTER `313` | C.W1 implementation gate. |
| T2-P7 | DEFER | PASS/Master future gate | Primary PASS row out-of-scope; Master may carry a generic chain-recovery gate. |
| T2-P8..P10 | DEFER | Research index | Denied surface. |
| T2-P11 | FOLD | ARCH/MASTER | Proof-obligation checklist folded as closed DK gate. |
| T2-P12..P15 | FOLD | MASTER `313`, `348` | Local chain/coercion fixtures belong in C/D gate text. |

### Topic 3 - CSP/GADTs/generics

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T3-F1..F3 | OUT-OF-SCOPE | README | Denied; HM/CSP split folds in ARCH/Master. |
| T3-F4 | FOLD | ARCH `1127-1132` | Finite monomorphisation invariant. |
| T3-F5 | FOLD | ARCH `768-800` | Later CSP solve renamed extraction/global solve. |
| T3-F6 | FOLD | ARCH `1117-1132` | Local-equality/OutsideIn boundary. |
| T3-F7 | FOLD | ARCH `1129`, MASTER `313` | Host overloads become explicit improvement constraints. |
| T3-F8 | OUT-OF-SCOPE | README | Denied. |
| T3-F9 | DEFER | Research index | Denied. |
| T3-Srg1..Srg2 | OUT-OF-SCOPE | README | Denied. |
| T3-Srg3 | FOLD | ARCH `1117-1132` | V1 generic rules are HM parametric schemes; GADT-like construct requires amendment. |
| T3-Srg4 | FOLD | ARCH `768-800` | Pipeline CSP disambiguation. |
| T3-Srg5 | OUT-OF-SCOPE | PASS-1 | PASS-owned. |
| T3-Srg6 | FOLD | ARCH `1127-1132`, MASTER `346` | Finite instance-set validation and generic-cycle diagnostic gate. |
| T3-Srg7 | FOLD | ARCH `1129`, MASTER `313` | Improvement constraints for host overloads. |
| T3-Srg8 | OUT-OF-SCOPE | README | Denied. |
| T3-Srg9 | DEFER | Research index | Denied source/source-row cleanup. |
| T3-Srg10 | FOLD | ARCH `1008-1031`, MASTER `313` | Generic-cycle/local-equality diagnostics named at synthesis level. |

### Topic 4 - E-graphs and bridge design

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T4-R1..R2 | OUT-OF-SCOPE | README | Denied, but stable-ID bridge concept folds into ARCH/Master. |
| T4-R3 | FOLD | ARCH `799` | Bridge exchanges monotone facts; CSP search state stays internal. |
| T4-R4 | FOLD | ARCH `517-522`, `990-992` | Add `BridgeJustification` proof refs via egraph/CSP explanations. |
| T4-R5 | FOLD | MASTER `316` | C.W4 stable map, monotone exchange, guards, justifications. |
| T4-R6 | FOLD | MASTER `317` | C.W5 extraction legality and bridge proof refs. |
| T4-R7 | DEFER | README/MASTER | README denied; Master carries C.W4 rewrite-budget evidence. |
| T4-R8..R9 | FOLD | ARCH `456-478`, `991-992` | Cost-model owns extraction constraints and profile metadata. |
| T4-R10 | DEFER | Research index | Denied. |
| T4-R11 | FOLD | MASTER `698` | Lock 4 close proof adds representative stability and justification round-trip. |
| T4-R12 | FOLD | ARCH `768-800` | C.W4 bridge consumes public solved facts only. |
| T4-S1 | OUT-OF-SCOPE | README | Denied. |
| T4-S2..S3 | FOLD | ARCH `799`, `990-992` | Bridge invariant and justification API. |
| T4-S4..S5 | FOLD | MASTER `316-317` | C.W4/C.W5 scope. |
| T4-S6 | FOLD | MASTER `316`, `698` | Rewrite budget gates. |
| T4-S7 | FOLD | ARCH `456-478`, `991-992` | Extraction constraints beside cost evidence. |
| T4-S8 | FOLD | ARCH `10-30`, MASTER `698` | egglog rationale folded into Architecture/Master, not lock file. |
| T4-S9..S10 | DEFER | Research index | Denied catalogue/source cleanup. |
| T4-S11 | FOLD | MASTER `698` | Lock ownership close proof. |
| T4-S12 | OUT-OF-SCOPE | PASS-1 | PASS bridge receiver note belongs to PASS worker. |

### Topic 5 - Cost models

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T5-F1..F2 | OUT-OF-SCOPE | README | Denied; accepted shape folds into ARCH/Master/Migration. |
| T5-F3..F4 | OUT-OF-SCOPE | PASS-1 | PASS worker owns cost-model child layout. |
| T5-F5 | FOLD | ARCH `991-992` | `CostFacts` stores `CostDecision`, objective vectors, Pareto/frontier evidence. |
| T5-F6 | FOLD | MASTER `317` | C.W5 objective profiles, Pareto extraction, solver-backed skeleton. |
| T5-F7..F9 | OUT-OF-SCOPE | PASS-2/PASS-3 | PASS workers own. |
| T5-F10 | DEFER | Research index | Denied stale lock-pointer cleanup. |
| T5-S1 | OUT-OF-SCOPE | README | Denied. |
| T5-S2..S3 | OUT-OF-SCOPE | PASS-1 | PASS worker owns. |
| T5-S4 | FOLD | ARCH `991-992` | Objective vectors, domination, extraction method in side-table contract. |
| T5-S5 | FOLD | MASTER `317`, hard close | C.W5 cost-model facts/frontier/solve gate. |
| T5-S6..S8 | OUT-OF-SCOPE | PASS-2/PASS-3 | PASS workers own. |
| T5-S9..S10 | DEFER | Research index | Denied source and lock routing cleanup. |
| T5-S11 | OUT-OF-SCOPE | PASS-2/PASS-3 | PASS diagnostic strings. |
| T5-S12 | FOLD | MASTER `765-784` | Carry row for selected/rejected/dominated cost evidence. |

### Topic 6 - Tape/direct union

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T6-R1..R2 | OUT-OF-SCOPE | README/PASS-3 | Denied or PASS-owned; ARCH runtime folds equivalent. |
| T6-R3 | OUT-OF-SCOPE | PASS-2 | PASS-owned `TapeShape` detail. |
| T6-R4 | FOLD | ARCH `1198-1202` | Direct scalar fields are caches over declared payload slots. |
| T6-R5..R6 | OUT-OF-SCOPE | PASS-3 | PASS-owned materialisation and identity wording. |
| T6-R7 | FOLD | ARCH `1264-1271` | Benchmark metadata adds validation, materialisation, source/string ownership modes. |
| T6-R8 | OUT-OF-SCOPE | PASS-3 | PASS-owned bench report schema. |
| T6-R9 | FOLD | ARCH `1179-1187` | Add `payload/` child under tape. |
| T6-R10 | OUT-OF-SCOPE | PASS-2 | PASS-owned BIR payload. |
| T6-R11 | DEFER | Research index | Hubbard provenance cleanup denied. |
| T6-R12 | OUT-OF-SCOPE | README | Denied. |
| T6-S1..S2 | OUT-OF-SCOPE | README/PASS-2 | Denied/PASS-owned. |
| T6-S3 | FOLD | ARCH `1198-1202` | No second authoritative tree. |
| T6-S4..S5 | OUT-OF-SCOPE | PASS-3 | PASS-owned. |
| T6-S6 | FOLD | ARCH `1264-1271` | Benchmark required metadata. |
| T6-S7 | OUT-OF-SCOPE | PASS-3 | PASS-owned. |
| T6-S8 | FOLD | ARCH `1179-1187` | `runtime/src/tape/payload`. |
| T6-S9 | OUT-OF-SCOPE | PASS-2 | PASS-owned. |
| T6-S10 | OUT-OF-SCOPE | README | Denied. |
| T6-S11 | DEFER | Research index | Denied. |
| T6-S12..S16 | DEFER | Future implementation gates | Accepted as Master/Migration posture only where receiver exists. |

### Topic 7 - Green/red incremental

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T7-R1 | OUT-OF-SCOPE | README | Denied; ARCH runtime allows red-like views through identity wording. |
| T7-R2..R3 | OUT-OF-SCOPE | README | Denied; Master I and Migration gates carry snapshot/reuse concept. |
| T7-R4..R6 | OUT-OF-SCOPE | PASS-3 | PASS-owned. |
| T7-R7 | FOLD | MASTER `519` | I.W1 gate adds named fallback reason and reuse-map absence. |
| T7-R8 | FOLD | MASTER `798` | Add yaml syntax-error friction row and typed recovery posture. |
| T7-S1..S3 | OUT-OF-SCOPE | README/PASS-3 | Denied/PASS-owned. |
| T7-S4..S6 | OUT-OF-SCOPE | PASS-3 | PASS-owned. |
| T7-S7 | FOLD | MASTER `519` | Tranche I fallback ledger gate. |
| T7-S8 | FOLD | MASTER `798` | yaml syntax-error friction row. |
| T7-S9 | DEFER | Research index | Denied provenance cleanup. |

### Topic 8 - SIMD/DFA/regex

| Item | Route | Target line | Rationale |
|---|---|---:|---|
| T8-R1..R3 | OUT-OF-SCOPE | README | Denied; fold concepts into ARCH/Master/Migration. |
| T8-R4 | FOLD | ARCH `535-540` | Expand `parse-that` regex tree into HIR/NFA/DFA/VM/prefilter. |
| T8-R5 | FOLD | ARCH `563-588` | Regex scratch state includes HIR caches, lazy-DFA policy, prefilter plans. |
| T8-R6 | FOLD | ARCH `920-921` | `RegexProgram` semantic verifier; `SimdScan` exact/prefilter. |
| T8-R7 | FOLD | ARCH `949` | Exact scans require scalar parity; prefilters require verifier. |
| T8-R8 | FOLD | ARCH `1022` | SIMD rejection mentions cost, Unicode semantics, missing verifier route. |
| T8-R9..R12 | OUT-OF-SCOPE | PASS-2/agent file | PASS workers own. |
| T8-S1 | FOLD | ARCH `920-921` | `SimdScanMode` and verifier-before-tape contract. |
| T8-S2 | OUT-OF-SCOPE | PASS-2 | PASS-owned scanner tests. |
| T8-S3 | FOLD | MASTER `765-784`, MIGRATION `579-580` | `regex-automata` oracle lane and grammar-owned delta. |
| T8-S4 | FOLD | ARCH `535-540` | parse-that tree expansion. |
| T8-S5 | OUT-OF-SCOPE | PASS-2 | PASS-owned BIR naming. |
| T8-S6 | OUT-OF-SCOPE | README | Denied; Master/ARCH use verifier-bound/cost-selected posture. |
| T8-S7 | FOLD | ARCH `1022` | Diagnostic wording. |
| T8-S8 | OUT-OF-SCOPE | PASS-2 agent file | Denied. |
| T8-S9 | FOLD | MASTER `765-784` | Receiver row for regex oracle parity against `regex-automata`. |
| T8-S10 | FOLD | ARCH `1220-1238` | SIMD lowerer consumes validated exactness/verifier route. |
| T8-S11..S12 | OUT-OF-SCOPE | PASS-2/README | Denied/PASS-owned. |

## §3 - Adversarial finding reconciliation

| Finding | Preserved claim | Classification | Reconciliation |
|---|---|---|---|
| T1-A1 | "Full Hindley-Milner with subsumption" is too strong. | FOLD | Lock survives; Architecture/Master weaken prose to HM core plus bounded coercion. |
| T1-A2 | "CSP-backed unification" confuses the solver contract. | FOLD | Equality unification and finite CSP split in Architecture/Master. |
| T1-A3 | typed-record narrowing lacks a selected record type theory. | FOLD | V1 record narrowing becomes finite generated-shape coercion; row polymorphism deferred. |
| T2-A | research-index Lock 4 numbering drift. | DEFER | Research index denied; actual lock text is not contradicted. |
| T2-B | "full HM with subsumption" is too strong. | FOLD | Same as T1-A1, no lock redraft. |
| T2-C | DK completeness is cited before its surface exists. | FOLD | DK is conditional higher-rank/indexed proof gate. |
| T2-D | coercion examples need rule sites. | FOLD | Architecture names bounded coercion/improvement obligations; PASS diagnostics out-of-scope. |
| T2-E | Roc source role is overclaimed. | DEFER | Research index/source row denied. |
| T3-A1 | README overstates CSP as unification. | FOLD | Architecture/Master split HM unification from finite CSP. |
| T3-A2 | `Object<V>` does not justify GADT/OutsideIn machinery. | FOLD | V1 generics are HM parametric schemes. |
| T3-A3 | GADT-style inference can lack principal and finite maximal types. | FOLD | Future indexed/GADT-like constructs need amendment and annotations. |
| T3-A4 | Lock 10 is misidentified as generics in the research index. | DEFER | Index denied; actual locks survive. |
| T3-A5 | Subtyping wording collapses polymorphism categories. | FOLD | Bounded coercion/improvement language. |
| T3-A6 | Monomorphization lacks an explicit finiteness gate. | FOLD | Architecture/Master add finite instance-set gate. |
| T3-A7 | Verified-source gap must be preserved. | DEFER | Bibliography cleanup denied. |
| T4-A1 | representative promotion is unstable. | FOLD | Stable-ID facts and extraction-time representative choice. |
| T4-A2 | Lock 4 lacks the egglog counterargument. | FOLD | No lock contradiction; Architecture/Master add bridge-vs-fusion rationale. Lock-file wording deferred. |
| T4-A3 | seven rewrite categories need budget gates. | FOLD | Master C.W4 and lock close proof add budget evidence. |
| T4-A4 | bridge facts need proof/explanation payloads. | FOLD | `BridgeJustification` folded. |
| T4-A5 | source catalogue provenance gap. | DEFER | Research index denied. |
| T5-A1 | scalar trait is too strong. | FOLD | `CostDecision` preserves objective vectors and profile. |
| T5-A2 | branch iterator double-counts DAG sharing. | FOLD | Child links and solver-backed DAG extraction evidence. |
| T5-A3 | shared-with-regex can violate domain opacity. | FOLD | `RegexCostSummary` is opaque within shared decision envelope. |
| T5-A4 | Topic 5 lock pointer is stale. | DEFER | Research index denied. |
| T5-A5 | SMT-backed cost composition is under-specified. | FOLD | Objective mode and extraction method recorded. |
| T5-A6 | named source provenance gaps must not become evidence. | DEFER | Bibliography cleanup denied; trio avoids unverifiable Almomany/Deb claims. |
| T6-A1 | scalar materialisation cost is overstated. | FOLD | Architecture/Master metadata split payload/cache/lazy parse classes; README/PASS specifics out-of-scope. |
| T6-A2 | "union" can be misread as two trees. | FOLD | One authoritative `(TapeId, node id, payload class)` identity. |
| T6-A3 | On-Demand forward-only semantics conflict with tooling. | FOLD | Laziness is payload conversion over validated tape identity, not cursor-only parsing. |
| T6-A4 | in-situ competitors can make benchmark rows unfair. | FOLD | Benchmark metadata adds validation/source ownership/materialisation modes. |
| T6-A5 | Hubbard comparative study is a provenance gap. | DEFER | Index denied; not used as trio evidence. |
| T6-A6 | UTF-8 validation entry point needs explicit split. | FOLD | Benchmark/API metadata distinguishes `parse(&str)` from byte/file paths. |
| T7-A1 | "one representation" is too strong unless scoped to ownership. | FOLD | Architecture runtime permits views over one owning identity; README edit denied. |
| T7-A2 | stable identity "per parsed token" is underspecified. | FOLD | Master I.W1 names snapshot/reuse-map evidence. |
| T7-A3 | bbnf must not claim unique value from ERROR/MISSING nodes. | FOLD | yaml syntax-error row states typed recovery differentiator. |
| T7-A4 | e-graph cache survival is too parser-local. | FOLD | Master/Migration route query invalidation over `DocumentSnapshot` and reuse maps. |
| T7-A5 | Ungar/Adams and HelpMate provenance gaps. | DEFER | Research-index cleanup denied. |
| T8-A1 | SIMD positive versus DFA negative is under-specified. | FOLD | Exact/prefilter mode and verifier-before-tape contract. |
| T8-A2 | full DFA codegen cannot be mandatory for rich Unicode regex. | FOLD | `RegexProgram` execution plans cover VM/lazy/full DFA. |
| T8-A3 | bespoke regex risks reimplementing `regex-automata` without a clear delta. | FOLD | Master/Migration add oracle lane and grammar-owned delta. |
| T8-A4 | "SIMD-first everywhere" can train over-selection. | FOLD | Trio uses verifier-bound, cost-selected SIMD posture. |
| T8-A5 | Hyperscan/Vectorscan expectations may be a false friend. | DEFER | No lock contradiction; future benchmark comparison only. |

Escalation scan:

| Class | Result |
|---|---|
| Actual lock structurally contradicted | none |
| Actual lock text stale but not structurally contradicted | T4-A2 rationale gap; fold into Architecture/Master, lock-file edit deferred. |
| Research-index lock/source drift | T2-A, T3-A4/A7, T4-A5, T5-A4/A6, T6-A5, T7-A5; all deferred. |
| README-only wording faults | Accepted concepts folded through trio; README edits denied. |

## §4 - Accepted amendment plan for the trio

Architecture amendment plan:

| Area | Edit |
|---|---|
| §4.4 sister crate tree | Expand `parse-that` regex children to `regex/hir`, `regex/nfa`, `regex/dfa`, `regex/vm`, `regex/prefilter`, `unicode`, `literal`. |
| §4.4 cost model tree | Add frontier/solve/evidence vocabulary only if child count remains within Lock 13. |
| §6 pipeline | Rename late `CSP solve` to `global CSP extraction solve`; add note that type/layout CSP runs inside layout. |
| §7.3 side tables | Add internal `TypeObligationLog`; expand `CspSolution`, `CostFacts`, bridge justifications, objective evidence. |
| §7.4 diagnostics | Add generic-cycle/local-equality/coercion/SIMD verifier rejection language. |
| §8.2 type system | Decompose HM core, expected check/synth, bounded coercion, finite CSP; close DK/GADT gate for V1. |
| §9 runtime | Add `payload` module; define one authoritative tape identity plus typed projections and scalar caches over payload slots. |
| §10 lowerers | Add verifier-bound `SimdScan` contract. |
| §11 performance | Add validation mode, source ownership mode, materialisation mode, string/scalar-cache policy, and verifier/prefilter metadata. |

Migration amendment plan:

| Area | Edit |
|---|---|
| Sister crates | Record `parse-that/regex` as grammar-HIR/verifier integration over `regex-automata` oracle lane; keep `simd-scan` as exact/prefilter dispatch API. |
| New facilities | Update `crates/parse-that`, `crates/cost-model`, `crates/passes`, and runtime descriptions with HM/CSP split, bridge facts, payload policy, and regex oracle posture. |
| Tranche sequence | Clarify C/D/H/I migration rows with type obligations, objective evidence, verifier-bound SIMD, snapshot/reuse recovery. |
| Runtime gate | Add `TapeId`, `payload class`, verifier-before-tape, validation/source ownership metadata. |
| Citation posture | Do not cite Hubbard, Almomany, unverified Deb, HelpMate, or Ungar/Adams as evidence; keep corpus citations local and verified. |

Master amendment plan:

| Area | Edit |
|---|---|
| C.W1 | HM principal core, expected checking, bounded coercion, finite CSP; internal type obligation log. |
| C.W4 | Stable bridge maps, monotone facts, rewrite guards, budgets, bridge justifications. |
| C.W5 | `CostDecision`, objective profiles, Pareto/frontier evidence, solver-backed extraction, rejected/dominated candidates. |
| D.W1/D.W3 | Generic scheme instantiation, finite monomorphisation set, chain first-mismatch and coercion-site fixtures. |
| D hard close | Add type obligation, principal core, monomorphisation, and chain expected-flow tests. |
| H.W1 | SIMD exact/prefilter scanner contract, scalar parity, verifier-before-tape, `regex-automata` oracle parity. |
| I.W1 | Snapshot-scoped `TapeId`, reuse-map absence/fallback reason, query invalidation, silent LSP behavior. |
| Lock ownership | Add representative stability, rewrite-budget, bridge-justification, objective evidence to Lock 4/8/10 close proofs. |
| Carry/friction ledger | Add regex oracle, cost evidence, runtime materialisation metadata, yaml syntax-error recovery row. |

## §5 - Gate plan

Required gates before commit 2:

| Gate | Expected result |
|---|---|
| `git status --short` | Only the intended trio files dirty after commit 1. |
| Required `rg` command over trio + report | New terms classify as folded evidence; stale `full Hindley-Milner with subsumption`, `SIMD-first`, broad `DFA` claims are absent or historical. |
| `git diff --check` | No whitespace errors. |
| `git diff --cached --check` | No staged whitespace errors. |

Text-specific acceptance gates:

| Gate | Check |
|---|---|
| Type split | Architecture names HM core, expected-type check/synth, bounded coercion, finite CSP, and closed DK/GADT gate. |
| Internal facts | `TypeFacts` remains internal; `TypeObligationLog` is internal diagnostic evidence only. |
| Bridge | Architecture/Master name stable IDs, monotone bridge facts, extraction-time representative selection, and bridge justification. |
| Cost | `CostFacts` and C.W5 name objective vector, Pareto/frontier, profile, selected/rejected/dominated alternatives. |
| Runtime | Architecture names `payload/`, `(TapeId, node id, payload class)`, scalar-cache policy, source ownership metadata. |
| SIMD/regex | `SimdScan` has Exact/Prefilter mode; prefilter requires `RegexProgram` or scalar verifier before tape emission; `regex-automata` is oracle/reference. |
| Migration | Migration cites verified corpus paths and avoids unresolved source claims as evidence. |

## §6 - Classification verdict

Verdict: **AMENDMENT-REQUIRED, FOLD-ONLY, NO ESCALATION**.

The research strengthens the settled locks rather than overturning them. Lock 1 survives by sharpening ownership identity; Lock 4 survives by adding bridge-vs-fusion rationale and proof payloads; Lock 10 survives by recasting SIMD as verifier-bound and cost-selected; Lock 14 survives by keeping regex/yaml/runtime evidence grammar-derived.

Escalation candidates: none. Deferred items are research-index, README, lock-file, PASS, bibliography, and future implementation receivers outside this worker's write scope.

Accepted fold scope for the second commit: only `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, and `restart/MASTER-PLAN.md`.
