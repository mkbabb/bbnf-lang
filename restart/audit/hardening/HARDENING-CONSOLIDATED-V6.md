# HARDENING-CONSOLIDATED-V6 - research-fold consolidated verdict

## §1 Target Identifications

V6 reopens the V5.1-ready corpus only for the research-folded material. It asks
whether Topics 1-8 introduced a new blocking amendment after the fold commits,
not whether the earlier V5 defects remain open. The answer across all four
targets is READY.

| Target | Audited surface | V6 report | Report commit | Lines | Verdict |
|---|---|---|---|---:|---|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` | `restart/audit/hardening/HARDENING-PASS-1-V6.md` | `a745f12e` | 255 | READY |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` | `restart/audit/hardening/HARDENING-PASS-2-V6.md` | `5ea41850` | 428 | READY |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` | `restart/audit/hardening/HARDENING-PASS-3-V6.md` | `c5e3aab7` | 424 | READY |
| SYNTHESIS | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-SYNTHESIS-V6.md` | `4fe06344` | 246 | READY |

Current consolidation baseline:

| Commit | Role |
|---|---|
| `4fe06344` | SYNTHESIS V6 hardening report, verdict READY. |
| `a745f12e` | PASS-1 V6 hardening report, verdict READY. |
| `c5e3aab7` | PASS-3 V6 hardening report, verdict READY. |
| `5ea41850` | PASS-2 V6 hardening report, verdict READY. |
| `00c51814` | Synthesis research-fold amendment over Architecture, Migration, and Master Plan. |
| `d1096c21` | PASS-2 research-fold amendment. |
| `0c72433b` | PASS-1 plus synthesis research-fold amendment. |
| `b04f7ce1` | PASS-3 research-fold amendment. |

Parallel-audit note: several V6 reports record an audited HEAD that predates
later sibling hardening-report commits. This is not a target conflict. No target
surface changed after the reports began; only the V6 reports themselves landed
in parallel. Consolidation uses the report commits above as the authoritative
four-target cohort.

## §2 Cohort Verdict

Final consolidated verdict: READY.

The research fold sharpened the restart corpus without changing the executable
architecture. The accepted changes preserve the current BBNF surface, make type
and layout boundaries more precise, record bridge and cost evidence, split exact
SIMD from prefilter candidates, require verifier-before-tape for unsafe scanner
paths, strengthen tape identity and materialisation metadata, and keep weak
research leads out of evidence-bearing claims.

| Lane | PASS-1 | PASS-2 | PASS-3 | SYNTHESIS | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | READY | READY | READY | READY | READY |
| 2 Sequencing Discipline | N/A with receiver-discipline pass | N/A for PASS-level sequencing; carry rows adequate | N/A with precise sequencing classification | READY | READY |
| 3 Cohesion | READY | READY | READY | READY | READY |
| 4 SOTA-Anchoring | READY | READY with implementation measurement routed | READY | READY | READY |
| 5 Grammar-Authoritative Discipline | READY | READY | READY | READY | READY |
| 6 Generated-Code And LOC Budget | READY | READY | READY | READY | READY |
| 7 Friction Forecast | READY | READY | READY | READY | READY |
| 8 Carry And Deferral | READY | READY with non-blocking research-index residue | READY | READY | READY |
| 9 Greenfield Discipline | READY | READY | READY | READY | READY |

Report-local count rollup:

| Count family | PASS-1 | PASS-2 | PASS-3 | SYNTHESIS | Total |
|---|---:|---:|---:|---:|---:|
| KEEP / READY rows | 8 | 30 | 8 | 50 | 96 |
| N/A lanes with receiver-discipline pass | 1 | 1 | 1 | 0 | 3 |
| REINVENT rows | 0 | 0 | 0 | 0 | 0 |
| DISCARD rows | 0 | 0 | 0 | 0 | 0 |
| Blocking amendment rows | 0 | 0 | 0 | 0 | 0 |
| Re-draft rows | 0 | 0 | 0 | 0 | 0 |

The count rows are report-local, not normalized to identical table granularity.
They are useful as a drift signal only: no V6 hardener found a REINVENT,
DISCARD, amendment-required, or re-draft row.

### §2.1 Research-Agent Issuance And Staggered Fold Recap

Issuance clarification: the committed trail shows one research agent per Index
topic, eight total, not sixteen-plus independent research-agent files. The
sixteen-plus coverage exists at source/item level: the eight agents walked 66
verified primary, canonical, or official source slots, plus separately tracked
provenance gaps that stayed out of evidence-bearing claims. That is the shape
required by `restart/research/INDEX.md` §2, which sets a 5-15 primary-source
floor and ceiling per topic. No Index topic is missing.

| Topic | Research commit | Verified source slots | Direct SOTA question | Fold receivers | V6 disposition |
|---|---|---:|---|---|---|
| 1 HM foundations | `62d3ac76` | 9 | Hindley-Milner, Algorithm W, Damas-Milner, and whether the restart conflates HM equality with bidirectional and CSP layers. | PASS-1, SYNTHESIS | CLOSED: HM equality stays foundational and internal `TypeFacts` do not become public architecture. |
| 2 Bidirectional | `a2b4471e` | 5 | Pierce-Turner local check/synth versus Dunfield-Krishnaswami higher-rank proof discipline. | PASS-1, PASS-3 diagnostics, SYNTHESIS | CLOSED: V1 uses local check/synth and directed subsumption; higher-rank DK machinery remains future proof gate. |
| 3 CSP/GADTs | `5f313187` | 8 | CSP-backed unification, HM(X), OutsideIn(X), GADT pressure, parametric polymorphism, and generic-rule finiteness. | PASS-1, PASS-2, SYNTHESIS | CLOSED: finite `(RuleId, TypeArgs)` proof and local equality constraints are required before lowerers emit instances. |
| 4 E-graphs | `4d24cedb` | 10 | Equality saturation, e-graphs, egg/egglog pressure, and bridge-vs-union design. | PASS-1, PASS-2, SYNTHESIS | CLOSED: stable bridge facts and `BridgeJustification` survive; representative leakage and egglog fusion pressure do not rewrite Lock 4/6. |
| 5 Cost models | `101516fd` | 9 | Pareto extraction, scalarization, SMT/objective evidence, and shared regex/BIR cost-model shape. | PASS-1, PASS-2, PASS-3, SYNTHESIS | CLOSED: `CostDecision` keeps objective vectors, selected/rejected/dominated alternatives, and scalar fast-path evidence. |
| 6 Tape/direct | `ad3a5e63` | 9 | Tape encoding, direct-to-struct union, simdjson/sonic-rs/yyjson pressure, and one owning identity. | PASS-2, PASS-3, SYNTHESIS | CLOSED: `TapeShape`, `ValueShape`, `ValueRef`, typed roots, and red-like projections ride one snapshot identity. |
| 7 Green/red incremental | `5e3077a9` | 8 | Green/red trees, incremental parsing, fault tolerance, recovery facts, and LSP fallback. | PASS-1 producer facts, PASS-3, SYNTHESIS | CLOSED: `RecoveryFacts`, `ReparsePlan`, reuse maps, fallback reasons, invalidated queries, and silence policy are receiver-gated. |
| 8 SIMD/DFA/regex | `5e947540` | 8 | SIMD scanning, DFA construction, bespoke regex HIR, oracle parity, exact scans, and verifier-before-tape prefilters. | PASS-2, PASS-3, SYNTHESIS | CLOSED: `RegexProgram` exact/prefilter modes remain compatibility-routed; no author `@simd` or force directive returns. |

Source-count total: 66 verified source slots across the eight topic agents.
Known weak or unverified leads are not counted in that 66 and are routed in R1
instead of used as proof.

The staggered fold path is likewise explicit in the commit trail:

| Stage | Commits | What landed |
|---|---|---|
| Phase 0 research orchestration | `9e19df30`, `fb030b3d` | The orchestrator and Index defined the eight topic-agent shape, the per-topic source floor, the fold reports, escalation path, and terminal V6 hardening. |
| Phase 1 topic research | `62d3ac76`, `a2b4471e`, `5f313187`, `4d24cedb`, `101516fd`, `ad3a5e63`, `5e3077a9`, `5e947540` | Eight topic-level SOTA deep-dives, each mapped to the Index item and source floor. |
| Phase 2 fold classification | `8829a0b5`, `7064b764`, `048e227e`, `14c8fde1` | Topic outputs were split by receiver: PASS-1 gets Topics 1-3, PASS-2 gets Topics 4-6 and 8, PASS-3 gets Topics 6-8, and SYNTHESIS gets Topics 1-8. |
| Phase 2 fold amendments | `88d22b78`, `0c72433b`, `d1096c21`, `b04f7ce1`, `00c51814` | Surviving research refinements were folded into PASS-1, PASS-2, PASS-3, Architecture, Migration, and Master Plan. |
| Phase 3 V6 hardening | `a745f12e`, `5ea41850`, `c5e3aab7`, `4fe06344` | Four target-specific hardeners rechecked the folded surfaces and returned READY. |
| Phase 4 consolidation | `003a6410` plus this recap | V6 binds the target reports, closes the research-fold topics, and routes bibliography/source hygiene as non-blocking residue. |

Escalation note: no `restart/research/escalation-summary.md` exists because
the fold-classification reports found zero structural ESCALATE items. The
absence is intentional under the orchestrator path, not a missing research
artifact.

Commit-scope note: `88d22b78` has a PASS-1 subject but touched PASS-2, while
`0c72433b` has a PASS-1 subject and touched PASS-1 plus the synthesis trio.
This consolidation binds the actual touched receiver surfaces and the later V6
reports, not only the shorthand commit subjects.

Therefore Hindley-Milner, CSP/GADT, and e-graph coverage are not inferred from
general prose: they have named topic agents, committed source walkdowns, fold
receiver commits, V6 target hardening, and this consolidation ledger. If a
future rerun requires one separate agent per individual source rather than one
agent per Index topic, that is a different orchestration shape and should be
recorded as a new prompt before execution.

## §3 Research-Fold Binding Ledger

| Topic | PASS-1 binding | PASS-2 binding | PASS-3 binding | SYNTHESIS binding | Consolidated result |
|---|---|---|---|---|---|
| 1 HM foundations | HM equality and finite CSP are split; `TypeFacts` stays internal and `LayoutFacts` is public. | Generic monomorphisation stays finite and PASS-2 does not design type inference. | Type/value diagnostics expose cause without publishing a type pass. | Architecture decomposes HM equality, expected checking, bounded coercion, finite CSP, and future proof gates. | CLOSED |
| 2 Bidirectional | Local check/synth and directed subsumption are scoped; higher-rank machinery is not in V1. | Sibling-routed; PASS-2 cites diagnostic ownership only. | Diagnostic causality only; no public DK or higher-rank surface. | Pierce-Turner check/synth is local; DK-style machinery is a future proof gate. | CLOSED |
| 3 CSP/GADTs | `Object<V>` stays rank-1 HM parametric; finite `(RuleId, TypeArgs)` proof is required. | Lowerers emit only after finite instance validation. | Sibling/synthesis routed except generic diagnostic context. | Local equality and GADT-like surfaces are future-gated; generic cycles are diagnostic-bearing. | CLOSED |
| 4 E-graphs | Stable ids and `BridgeJustification` replace representative leakage. | `passes::extract` and bridge evidence are lowerer input. | Runtime API does not absorb egraph ownership. | Stable bridge facts, representative-stability gates, and egglog counterargument rationale are recorded. | CLOSED |
| 5 Cost models | `CostDecision` includes objective vectors, selected/rejected/dominated alternatives, and scalarization profile. | Scalar cost is only a fast path when the full evidence record survives. | Materialisation cost and optimizer notes reach user/runtime gates. | Master carries objective mode, target, profile, extraction method, and rejected/dominated evidence. | CLOSED |
| 6 Tape/direct | PASS-1 routes operational tape details to sibling owners and keeps substrate/BIR facts. | `TapeShape` and `ValueShape` share one tape identity and node id. | `ValueRef`, typed roots, red-like views, DAP, CLI, LSP, and playground all ride one snapshot identity. | Architecture names one authoritative tape/direct identity and benchmark metadata. | CLOSED |
| 7 Green/red incremental | PASS-1 emits producer-side `RecoveryFacts`. | PASS-2 emits only optional incremental metadata. | `ReparsePlan` has reuse maps, fallback reason, invalidated queries, typed recovery nodes, and LSP silence policy. | Master binds snapshot `TapeId`, reuse maps, query invalidation, fallback ledger, and YAML syntax-error friction. | CLOSED |
| 8 SIMD/DFA/regex | PASS-1 does not add `@simd`, `@pratt`, grammar Unicode algebra, or scanner force controls. | `RegexDfa` is compatibility spelling; exact scans prove scalar parity; prefilters verify before tape; `regex-automata` remains oracle. | `BBNF-OPT002` exposes exactness and verifier-before-tape routing without force directives. | Architecture and Master bind `RegexProgram`, exact/prefilter modes, oracle parity, and H/J measurement gates. | CLOSED |

No topic produces an escalation row against a V6 target. Topic pressure becomes
implementation evidence, receiver-gated residue, or bibliography hygiene.

## §4 Cross-Target Conflicts

Blocking conflicts: none.

| Potential conflict | Sources | Per-target verdicts | Resolution |
|---|---|---|---|
| Parallel V6 reports audited slightly different HEADs. | PASS-1 and SYNTHESIS reports record earlier hardening-report HEADs; current consolidation HEAD includes all four V6 reports. | All four target surfaces are unchanged after the research-fold commits; only sibling reports landed. | Non-conflict. Consolidation binds by final report commit and current clean worktree. |
| `TypeFacts`, `LayoutFacts`, `LayoutSink`, and `passes::layout` vocabulary could drift. | PASS-1 V6 §4/§6, PASS-2 V6 §6, PASS-3 V6 §6, SYNTHESIS V6 §3. | All READY. | `LayoutFacts` is public side-table; `passes::layout` is public pass vocabulary; `LayoutSink` is PASS-2/BIR consumer vocabulary; `TypeFacts` and obligation logs remain internal. |
| BBNF grammar surface could re-open retired syntax. | PASS-1 formal grammar, PASS-2 recognizer diagnostics, PASS-3 runtime diagnostics, SYNTHESIS formal grammar. | All READY. | Accepted syntax remains block-bodied `@host fn`, infix finite lookbehind, generics, rule-level chains, `@error(recover = ...)`, and `@layout`; `path!`, `@pratt`, `@simd`, rewrite-mode, and grammar Unicode algebra are not positive surfaces. |
| Tape/direct could become two authorities. | PASS-2 `TapeShape`/`ValueShape`, PASS-3 `ValueRef` and snapshot identity, SYNTHESIS one identity rows. | All READY. | One owning tape identity carries node id and payload class; direct roots, red-like views, visitors, and value cursors are projections. |
| SIMD/DFA research could force implementation through familiar names. | PASS-2 exact/prefilter rows, PASS-3 `BBNF-OPT002`, SYNTHESIS regex/SIMD rows. | All READY. | Exact mode requires scalar parity; prefilter mode requires verifier acceptance before tape emission; `RegexDfa` is compatibility spelling and does not mandate full DFA; no author `@simd`/`@pratt` route exists. |
| YAML proof could become fixture-first or per-grammar special casing. | PASS-1 yaml input rows, PASS-2 generated-output proof, PASS-3 runtime recovery, SYNTHESIS A->F->J trajectory. | All READY. | YAML author inputs are only `grammars/yaml.bbnf` plus one metadata block; generated files and fixtures are derivative gates. |
| H.W3 WASM placeholders could be pseudo-precision. | SYNTHESIS V6 §1/§12, PASS-2 WASM ABI route, PASS-3 runtime route. | All READY with routed measurement. | `{N}` and `{M}` remain measured placeholders with H.W3 owner, blocker, benchmark context, ABI matrix, and scalar/SIMD parity gate. |
| Weak research sources could contaminate evidence. | PASS-1/PASS-2/PASS-3 residual tables and SYNTHESIS §5/§12. | All READY with residue. | Weak or unverified leads stay out of target evidence and are routed to research-index/bibliography hygiene. |

No report ratifies something another report blocks. The only repeated issues are
deduped non-blocking residue, listed next.

## §5 Punch List Consolidation

Blocking V6 punch list:

| # | Target | Surgery | Acceptance gate | Status |
|---:|---|---|---|---|
| 1 | none | No V6 target-local amendment required. | All four V6 reports return READY, with zero blocking amendment rows. | CLOSED |

Residual non-blocking ledger:

| # | Residue | Receiver | Blocker | Acceptance gate | Blocking? |
|---:|---|---|---|---|---|
| R1 | Research-index and bibliography hygiene for weak or role-unclear sources: Hubbard, Almomany, Deb, Yang/egglog, Roc, Ungar/Adams, HelpMate, Hyperscan/Vectorscan, and lock-number drift. | Future research-index cleanup. | Several topic reports preserved provenance gaps but the V6 targets do not cite those gaps as evidence. | `restart/research/INDEX.md` classifies each source as verified primary/local evidence or explicit provenance gap. | No |
| R2 | README precision for scalar materialisation, one owning identity with red-like projections, snapshot-scoped `TapeId`, and HM/check-synth/CSP wording. | Future README cleanup. | V6 write scopes were hardening reports, not README edits; PASS-3 and SYNTHESIS already carry precise target contracts. | README avoids misleading constant-time scalar wording, permits transient projections over one owning identity, names reuse/query invalidation, and decomposes HM/check-synth/finite-CSP prose. | No |
| R3 | Hardening command-harness precision from older consolidated checklist rows #10 and #16. | Future hardening-harness cleanup or next rerun checklist. | Old commands were case-sensitive or omitted target files; V6 reports used tightened classifications. | Receiver scans include `Receiver|receiver|Blocker|blocker|Receiving gate|receiving gate`; benchmark metadata scans include Architecture and PASS-2 or split PASS-2 metadata floor from Master metadata. | No |
| R4 | Optional Lock 4 rationale hygiene for egglog-style fusion pressure. | Future lock-rationale or synthesis-hygiene pass if authorized. | V6 finds no structural lock contradiction; current plan records the counterargument and keeps fusion post-V1. | Lock rationale explains why bridge-vs-fusion survives egglog pressure without changing settled Lock 4 semantics. | No |
| R5 | Rewrite-budget implementation detail across C.W4/C.W5. | Tranche C/E implementation specs. | V6 synthesis and PASS-2 route the detail, but implementation tests do not exist yet. | Egraph/rewrite-budget tests name categories, node/iteration limits, representative stability, and cost/bridge evidence. | No |
| R6 | H.W3 WASM latency placeholders `{N}` and `{M}`. | H.W3, then J.W3. | Lightning-css WASM comparison and host/browser measurement have not run. | H.W3 records measured values, host/browser/runtime, fixture hash, ABI matrix, scalar/SIMD parity, and competitor baseline before acceptance. | No |
| R7 | Full per-wave tranche specifications. | Next drafting phase. | Synthesis intentionally ends at A-J stub/close-gate level. | Detailed tranche specs are drafted from this READY corpus. | No |

The residual ledger is not an amendment queue for V6. It is a routing ledger so
the next phase does not confuse hygiene with readiness.

## §6 Verification Evidence

Consolidation checks performed before this report was written:

| Command | Result |
|---|---|
| `git status --short` | Clean before consolidation work. |
| `git log --oneline --decorate -8` | Confirmed latest four hardening commits: SYNTHESIS V6, PASS-1 V6, PASS-3 V6, PASS-2 V6. |
| `wc -l restart/audit/hardening/HARDENING-PASS-1-V6.md restart/audit/hardening/HARDENING-PASS-2-V6.md restart/audit/hardening/HARDENING-PASS-3-V6.md restart/audit/hardening/HARDENING-SYNTHESIS-V6.md` | Confirmed 1,353 total V6 report lines. |
| `rg -n "^# |^Verdict|Verdict:|Decision|READY|AMENDMENT|RE-DRAFT|PASS-.*blocking|blocking punch|punch list|Routed residue|RESIDUAL|non-blocking|ready for V6 consolidation|fit for Phase 6|consolidation" ...` over the four V6 reports | Confirmed every V6 report returns READY, every blocking punch list is empty, and residue is non-blocking or routed. |
| `rg -n "Lane [0-9]|Lane verdict|Lane [0-9] verdict|KEEP|REINVENT|DISCARD" restart/audit/hardening/HARDENING-PASS-2-V6.md` | Derived PASS-2 report-local lane row counts and confirmed zero REINVENT/DISCARD rows. |

V6 per-target gate summaries:

| Target | Gate summary |
|---|---|
| PASS-1 | Required retired-surface/routing scan passed; readiness/carry scan found no PASS-1 V6 amendment row; `TypeFacts` internal and `LayoutFacts` public; `git diff --check` clean. |
| PASS-2 | Required research/PASS-2 scans passed; tightened 16-command checklist passed with two harness-only residuals; no retired recognizer surface; `git diff --check` clean. |
| PASS-3 | PASS-3/fold/topic scan passed; tightened 16-command subset passed with distributed-metadata note only; no PASS-3-local surgery; `git diff --check` clean. |
| SYNTHESIS | Broad token scan and carry/readiness scan passed; generic match-arm scan returned zero; retired syntax scan found only forbidden/deletion contexts; `git diff --check` clean. |

## §7 Lane Crosswalk

The tables below preserve the adversarial question each V6 hardener asked. They
are not new audits; they consolidate where the reports agree and where they
intentionally route residue away from readiness.

### §7.1 Lane 1 - Lock-Adherence

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Locks touched by substrate, layout, BIR, recognizer, declaration-crate, and yaml rows remain honored. | Any retained word hit for `OpenFrame`, SIMD, or WASM could be treated as relapse. | KEEP. Hits are negative invariants, lowerer obligations, or sibling handoffs. |
| PASS-2 | BIR-only lowerers, one tape identity, generated budgets, disable-only SIMD metadata, and two-surface yaml proof survive. | Current source still has old `GrammarIR` walkers as migration violations. | KEEP. PASS-2 cites them as violation targets, not permission. |
| PASS-3 | Runtime-visible nodes use snapshot tape identity; stale prompt/README clauses are cited as stale, not adopted. | Stale upstream wording could invalidate PASS-3. | KEEP. PASS-3 has no positive public `ParseStream`, rewrite-mode, grammar Unicode algebra, `@pratt`, or `@simd`. |
| SYNTHESIS | Locks 1, 2, 5, 8, 14 and related generated-budget locks remain coherent across Architecture/Migration/Master. | Any `{N}`/`{M}` or `ParseStream` hit could be treated as lock failure. | KEEP. Placeholders are measured gates; stale names are deletion or conflict contexts. |

### §7.2 Lane 2 - Sequencing Discipline

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Handoff tables name PASS-2/PASS-3/Architecture/Master receivers for BIR, cost, recovery, path/value, WASM, and TS. | PASS-only artifact cannot prove tranche sequencing. | N/A with receiver-discipline pass. |
| PASS-2 | Carry rows preserve inheritance and name receivers, blockers, and gates. | Carry rows might be mistaken for execution schedule. | N/A with adequate carry discipline. |
| PASS-3 | Runtime receiver rows bind generated metadata, recovery, DAP/debug, scanner, and yaml consumers. | PASS-3 cannot prove producer ordering alone. | N/A with precise receiver classification. |
| SYNTHESIS | A-J stubs, carry matrix, yaml trajectory, C consumers, F generation, and H recognizer/WASM rows name downstream consumers. | Stub-level plan might need full tranche specs before readiness. | READY. Full specs are the next phase, not a prerequisite to V6 readiness. |

### §7.3 Lane 3 - Cohesion

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Grammar IR, BIR, type algorithm, bridge, cost, diagnostics, crate tree, and handoffs name owners and consumers. | Future test commands are not runnable yet. | KEEP. Restart syntheses define close gates before crates exist. |
| PASS-2 | BIR table, lowerer gates, PASS-3 consumers, PASS-1 handoffs, and research evidence are aligned. | PASS-2 could over-freeze sibling APIs. | KEEP. Cross-pass conflicts return to SYNTHESIS, not unilateral lowerer edits. |
| PASS-3 | Runtime API, path/select, visitor, incremental parse, recovery, yaml, DAP, and diagnostics share one identity. | Typed roots, red-like cursors, and AST adapters could become rival products. | KEEP. PASS-3 makes them projections over one owning tape identity. |
| SYNTHESIS | Topics 1-8 are locally folded into Architecture/Master/Migration without using weak leads as evidence. | README or research index residue could be treated as target failure. | KEEP. The trio carries precise contracts; residue is outside target write scope. |

### §7.4 Lane 4 - SOTA-Anchoring

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | PASS-1 owns cost-evidence shape, not parse-throughput wins. | Every PASS might need competitor rows. | KEEP. Lock 8 applies to throughput gates; PASS-1 routes measurement surfaces. |
| PASS-2 | SOTA trajectory rows name competitors, datasets, targets, platform, metadata, and measurement commands. | Benchmark values are not freshly rerun now. | KEEP. PASS-2 sets trajectory and metadata floor; implementation tranches measure. |
| PASS-3 | Throughput rows name competitor/dataset/platform; non-throughput rows disclaim Lock 8 claims. | Distributed metadata could thin provenance. | KEEP. PASS-3 points to Architecture/Master metadata receivers. |
| SYNTHESIS | Architecture and Master carry Lock 8 rows; H.W3 placeholders have owner/blocker and ABI matrix. | Any `TBD` can be viewed as pseudo-precision. | KEEP. The only placeholder is an unmeasured future value, not an asserted result. |

### §7.5 Lane 5 - Grammar-Authoritative Discipline

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Formal syntax excludes rewrite-mode and grammar Unicode algebra; declaration crates are rare; yaml has two author inputs. | Per-X proof tables can train overfit implementation. | KEEP. Tables are evidence, not dispatch logic. |
| PASS-2 | yaml source and metadata are author inputs; runtime/path/visitor/host outputs are generated. | Generated paths could be mistaken for author surfaces. | KEEP. Generated output is derivative under Lock 6. |
| PASS-3 | `pointer!`/`select!` validate through generated metadata; hardcoded registries are deletion items. | Grammar rows in proof tables can look like per-grammar authority. | KEEP. The rows are proof evidence and forbid generic-crate match arms. |
| SYNTHESIS | YAML A->F->J trajectory and Architecture walkthrough keep source plus metadata as the only author surfaces. | YAML could become special implementation path. | KEEP. It is a future-grammar proof, not seed-grammar overfit. |

### §7.6 Lane 6 - Generated-Code And LOC Budget

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Budget schema and finite generic instance validation prevent unbounded emission. | Schema without concrete generated numbers is weak. | KEEP. PASS-1 is substrate owner; PASS-2/Master own numeric rows. |
| PASS-2 | Per-grammar generated LOC, non-generated LOC, child-count commands, and regen wall rows exist. | Provisional rows could invite drift. | KEEP. Provisional rows name owner and receiver. |
| PASS-3 | Visitor/path/projection/diagnostic/regen budget ties to PASS-2 W3 baselines and materialisation reports. | Report-only budgets might not constrain implementation. | KEEP. PASS-3 requires generated artefacts and regen gates. |
| SYNTHESIS | Master carries wave-level generated budgets, Lock 13 rows, and receiving gates. | Detailed schema files are not implemented. | KEEP. The plan defines acceptance gates for implementation. |

### §7.7 Lane 7 - Friction Forecast

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Verbatim diagnostic strings exist for lookbehind, host signature, layout, chains, subsumption, generics, equality, Pratt, and SIMD non-selection. | Runtime worked examples are elsewhere. | KEEP. PASS-1 owns producer diagnostics; PASS-3/Master own runtime cookbook. |
| PASS-2 | Codegen and recognizer diagnostics explain objective profile, fallback, exactness, and verifier-first route. | PASS-3 has richer public strings. | KEEP. PASS-2 still needs routing diagnostics. |
| PASS-3 | Pointer/select, lifetime, layout, optimizer, recovery, type/value, host, lowerer, and lookbehind diagnostics are present. | Cookbook strings can drift. | KEEP. PASS-3 ties codes to emitted runtime tests and cookbook receivers. |
| SYNTHESIS | Master friction ledger names user model, confusion, resolving artifact, and diagnostic. | Cookbook pages do not exist yet. | KEEP. The receiver and gate are explicit for drafting. |

### §7.8 Lane 8 - Carry And Deferral

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | Handoffs name receivers, blockers, and receiving gates; fold-pass-1 routes out-of-scope residue. | Any residual could be treated as blocker. | KEEP. PASS-1 neither owns nor depends on the unresolved residue. |
| PASS-2 | Carry ledger names TS, parity, publication, fixtures, path-ts, WASM ABI, PASS-1, and PASS-3 receivers. | Some receivers are broad. | KEEP. Broad rows still name gates and are enough for consolidation. |
| PASS-3 | Stale prompt/README/inheritance, registry deletion, PASS-2 metadata, diagnostic ledger, and benchmark rows are gated. | Residuals can hide behind deferral. | KEEP. V6 classifies only out-of-scope hygiene as residual. |
| SYNTHESIS | Master §24 is the single carry ledger; Migration points there instead of duplicating truth. | One large ledger can hide stale rows. | KEEP. Single ledger avoids contradictory carry truth. |

### §7.9 Lane 9 - Greenfield Discipline

| Target | Evidence classified READY | Main challenge tested | Consolidated result |
|---|---|---|---|
| PASS-1 | ParseStream is source-normalization archaeology; OpenFrame is deletion; egraph/CSP/cost stay scoped. | Negative evidence can become authority. | KEEP. PASS-1 labels inherited terms as discard/deletion/routing. |
| PASS-2 | PASS-2 is replacement, not patch; old direct Grammar IR and OpenFrame paths are violation targets. | Strong replacement posture increases migration work. | KEEP. Greenfield discipline rejects compatibility shims that preserve failure modes. |
| PASS-3 | Runtime surface rejects parallel substrates, hardcoded registries, arbitrary mutable values, and type-pass leakage. | Archaeology terms can re-enter as authority. | KEEP. PASS-3 marks them DISCARD, deletion, or conflict guards. |
| SYNTHESIS | The trio rejects retired syntax, overfit crates, benchmark invention, and workaround language. | Research additions could overcomplicate V1. | KEEP. Research pressure is folded into gates, not new public syntax. |

## §8 Topic Ownership Matrix

The matrix below is the deduped owner map that should guide tranche drafting.
Rows marked "receiver" do not need another amendment now; they identify which
future spec or implementation gate consumes the obligation.

| Obligation | PASS-1 | PASS-2 | PASS-3 | SYNTHESIS / Master | Next receiver |
|---|---|---|---|---|---|
| HM equality core | Owns producer/type contract. | Consumes finite instance proof. | Exposes diagnostic cause only. | Describes local check/synth and future proof gates. | C.W1 / D.W1 |
| Bidirectional expected checking | Owns local check/synth and subsumption diagnostics. | Does not design checker. | Mirrors user-facing value-shape failures. | Records bounded coercion and future higher-rank gate. | C.W1 / D.W3 |
| Finite generic monomorphisation | Owns finite `(RuleId, TypeArgs)` validation. | Emits only validated set. | Consumes generated projections. | Gates generic-cycle and budget proof. | D.W1 / F.W1 |
| GADT/local equality boundary | Rejects V1 GADT surface; reserves diagnostic for future amendment. | N/A. | N/A except public diagnostics. | Future proof gate only. | Post-V1 proof gate |
| Egraph/CSP bridge | Owns stable ids and bridge justifications. | Consumes extraction evidence. | Does not expose optimizer internals. | Carries representative-stability and rewrite-budget gates. | C.W4 / C.W5 / E.W1 |
| CostDecision evidence | Owns objective/rejected/dominated evidence shape. | Preserves full record across lowerers. | Exposes materialisation and optimizer diagnostics. | Carries objective mode, profile, target, and extraction method. | C.W5 / F.W3 / H |
| TapeShape/ValueShape | Defines substrate constraints and routes runtime details. | Owns emitted shapes and cache policy fields. | Consumes through `ValueRef`, views, visitors, DAP, CLI, LSP. | Defines one authoritative identity and metadata. | B / F / G / I |
| Snapshot `TapeId` | Produces recovery facts only. | Emits optional marker metadata. | Owns `ReparsePlan`, reuse maps, fallback reasons, invalidated queries. | Gates I.W1 and yaml syntax-error trace. | I.W1 / PASS-3 runtime specs |
| Error recovery | Produces `RecoveryFacts`. | Lowers `ErrorRecovery` shell and diagnostics. | Owns typed recovery nodes and user/runtime behavior. | Places recovery in I and docs friction rows. | I.W0 / I.W1 |
| Pointer/select | Not owned except grammar metadata handoff. | Emits path schema metadata. | Owns macro behavior and diagnostics. | Carries cookbook/friction and yaml trajectory gates. | G.W1 / G.W2 |
| SIMD exact mode | Rejects author force directives. | Owns scalar parity and target legality. | Exposes non-selection diagnostics. | Owns H/J measurement and lock evidence. | H.W1 / H.W4 |
| SIMD prefilter mode | Rejects author force directives. | Owns verifier-before-tape route. | Blocks tape emission before verifier acceptance. | Carries regex oracle parity and benchmark metadata. | H.W2 / H.W4 |
| Regex oracle | Keeps regex internals out of Grammar IR. | Uses `regex-automata` as oracle/parity route. | Exposes verifier diagnostic consequences. | Carries oracle and grammar-owned delta rows. | H / E parity |
| YAML onboarding | Proves grammar source plus metadata. | Emits generated outputs only. | Owns runtime/recovery/path behavior. | Owns A->F->J trajectory and two-surface proof. | A/F/G/I/J |
| WASM host route | Routes primitive facts to lowerer/runtime. | Owns ABI descriptor and parity evidence. | Owns runtime packaging/user behavior. | Owns H.W3 measured row and J.W3 publication gate. | H.W3 / J.W3 |
| Rare declaration crates | Owns producer fence and empty extant grammar table. | Emits only approved adapters, no generic imports. | Keeps registry/declaration route fenced. | Owns eight-field review form and deletion path. | A/J stability gate |

## §9 Verification Checklist For Future Reruns

V6 reports used the old 16-command family as a classification harness, but two
families need stricter future post-conditions. This consolidated version records
the corrected interpretation for the next hardening cycle.

| # | Command family | V6 interpretation | Future post-condition |
|---:|---|---|---|
| 1 | `ParseStream|rewrite-mode|Unicode class algebra` | Raw hits are allowed only in deletion/prohibition/conflict contexts. | Any active positive runtime or grammar surface fails. |
| 2 | `bbnf-path|bbnf-test-fixtures|path!` | Legacy package names are archaeology; public macro is `pointer!`. | `path!` positive surface count must be zero. |
| 3 | `codegen/src/backend_ir` | Documentation-only old paths may appear as denied ownership. | No new ownership path or lowerer source path may be asserted. |
| 4 | `fixtures/yaml` | Fixtures are parity evidence, not onboarding authority. | Onboarding section must name only grammar source plus metadata. |
| 5 | `@recover` | Standalone alias is rejected; accepted form is `@error(recover = ...)`. | Positive standalone `@recover` count must be zero. |
| 6 | `OpenFrame` | Allowed only as deletion archaeology or old perf pathology. | No runtime builder or substrate role may be assigned. |
| 7 | `GrammarIR` | PASS-1 producer and deny-gate contexts are valid. | Codegen lowerers must not import Grammar IR as emitter input. |
| 8 | `__EAGER_EMPTY_PATH|CursorDecision::Skip` | Cursor skip is a lowerer/runtime gate, not PASS-3 authority. | Gates must show cursor consultation and empty-path elision. |
| 9 | SOTA workload names | Must classify competitor, dataset, platform, and target rows. | Throughput claims without competitor/dataset/platform fail. |
| 10 | Receiver/blocker/gate | Case-sensitive lowercase grep is too soft. | Use `Receiver|receiver|Blocker|blocker|Receiving gate|receiving gate` or case-insensitive mode. |
| 11 | `yaml.bbnf|workspace.metadata.bbnf.grammars.yaml` | Two-surface proof must appear across author and consumer surfaces. | Both grammar source and metadata block must be present. |
| 12 | `generated_loc|regen_wall|xtask` | Generated and wall-budget rows are receiver-gated. | Rows must name owner, baseline category, and close command. |
| 13 | Diagnostic patterns | Diagnostic strings must not teach retired directives. | `BBNF-OPT*` rows must explain objective/exactness, not `@pratt`/`@simd`. |
| 14 | `child count|500 LOC|exception rationale` | Lock 13 rows may be distributed by target. | Exceptions need owner and rationale. |
| 15 | Declaration-crate review form | Rare fence fields may be spread across PASS/ARCH/MASTER. | Reason, owner, metadata failure, `@host fn` failure, location, no generic import, deletion path, reviewer, and gate must exist. |
| 16 | Benchmark metadata | Older checklist omitted Architecture and PASS-2. | Scan Architecture and PASS-2 or split PASS-2 metadata floor from Master metadata gate. |

## §10 V5.1-To-V6 History

| Cycle | Consolidated posture |
|---|---|
| V4 | Corpus returned READY after Wave 4.1 closed BIR ownership, lowerer deny gates, generated LOC tables, SOTA rows, visitor/path consumers, and OpenFrame deletion. |
| V5 | Carry-aware hardening reopened the corpus. It found real but narrow issues: formal grammar drift, retired recognizer directive wording, stale citations, sparse examples, and insufficient WASM/yaml/rare-fence routing. |
| V5.1 | Narrow amendments closed the substantive V5 defects. SYNTHESIS, PASS-3, and PASS-1/PASS-2 reports returned READY after citation hygiene. |
| Research fold | Topic reports 1-8 added pressure around HM, bidirectional checking, GADT boundaries, egraph/CSP bridge facts, cost evidence, tape identity, green/red incremental parsing, and SIMD/DFA safety. |
| V6 | Four hardeners rechecked the folded corpus. All four returned READY; no re-draft or target-local amendment row remains. |

The V6 result does not erase residue. It classifies residue by receiver and
blocks only on target-owned contradictions. No such contradiction survived.

## §11 Final Readiness Verdict

Decision: READY.

Readiness basis:

| Criterion | Consolidated result |
|---|---|
| Four target verdicts | PASS. PASS-1, PASS-2, PASS-3, and SYNTHESIS all return READY. |
| Research-fold absorption | PASS. Topics 1-8 are absorbed by the owner surfaces or routed as non-blocking residue. |
| Cross-target conflicts | PASS. No target ratifies a positive surface another target blocks. |
| Retired syntax and stale surfaces | PASS. `path!`, `@pratt`, `@simd`, rewrite-mode, grammar Unicode algebra, OpenFrame substrate, and runtime `ParseStream` survive only as deletion/prohibition/archaeology contexts. |
| Grammar authority | PASS. Formal BBNF syntax, YAML onboarding, generated derivative outputs, and rare declaration-crate fence remain consistent with Lock 14. |
| SOTA and benchmark discipline | PASS. Throughput rows carry competitor/dataset/platform where they claim Lock 8; placeholders are receiver-gated measurements, not invented numbers. |
| Carry discipline | PASS. Residue names receivers, blockers, and acceptance gates; none is a V6 readiness blocker. |
| Re-draft threshold | Not met. |
| Amendment-required threshold | Not met. |

Hereupon the restart corpus advances to per-tranche full-spec drafting from the
current research-folded synthesis trio and PASS syntheses. No V6 amendment pass
is required.
