# HARDENING-PASS-3-V6 - research-fold verification

## §1 Target identification and commits audited

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Report | `restart/audit/hardening/HARDENING-PASS-3-V6.md` |
| Audit target label | PASS-3 |
| Current workspace HEAD audited | `00c51814e5f679480c00cc037702192a85f64c63` |
| Verdict | READY |
| Write scope used | this report only |
| Source surfaces edited | none |
| Research-fold focus | Topics 1-8, with PASS-3-owned runtime/API/diagnostic consequences centered |

Commit history in scope:

| Commit | Surface | Audit use |
|---|---|---|
| `b04f7ce1` | PASS-3 wave-5-fold amendment | Current PASS-3 text under V6 review. |
| `048e227e` | PASS-3 research-fold classification | Routing baseline for Topic 1-8 PASS-3 fold. |
| `5e947540` | Topic 8 SIMD/DFA | Exact/prefilter and verifier-before-tape pressure. |
| `5e3077a9` | Topic 7 green/red incremental | Snapshot `TapeId`, reuse maps, typed recovery pressure. |
| `ad3a5e63` | Topic 6 tape | Materialisation, one identity, cache policy pressure. |
| `62d3ac76` | Topic 1 HM foundations | User-visible type diagnostic causality pressure. |
| `a2b4471e` | Topic 2 bidirectional | Higher-rank gate and subsumption diagnostic pressure. |
| `32c3dbf0` | PASS-3 V5.1 amendment | Runtime examples and diagnostic provenance baseline. |
| `11806d5d` | PASS-3 V4.1 amendment | Bench rows, W3 baseline anchors, yaml host route, visitor cookbook routing. |

Required governing text read:

| Source | Load-bearing rule applied |
|---|---|
| `restart/prompts/HARDENING-ORCHESTRATOR.md:55-77` | Phase 3 dispatches four target hardeners; each applies nine-lane audit and returns READY / AMENDMENT-REQUIRED / RE-DRAFT. |
| `restart/prompts/HARDENING-ORCHESTRATOR.md:79-118` | Phase 6 consolidates cross-target conflicts only after all hardening reports commit. |
| `restart/prompts/HARDENING.md:35-42` | Pro / Con / Explication / Challenge discipline is mandatory. |
| `restart/prompts/HARDENING.md:109-147` | Nine-lane table, gate rerun, binding ledger, punch-list, and final verdict shape. |
| `restart/prompts/HARDENING.md:169-177` | Hardening writes only its report and does not relitigate the settled locks. |
| `restart/prompts/AMENDMENT-DISPATCH.md:58-68` | Verify existing absorption before patching; route fixture-separation correctly to PASS-3 when needed. |
| `restart/README.md:450-452` | Path:line citations, no soft hedging, no `TBD`, receiver-gated deferrals. |
| `docs/precepts/instructions/STYLE.md:3-16` | Direct, economical, clear prose. |
| `docs/precepts/instructions/LESSONS-LEARNED.md:17-26` | Substrate without consumer is not progress. |
| `docs/precepts/instructions/LESSONS-LEARNED.md:74-80` | Producer and consumer gates must both close. |
| `docs/precepts/instructions/LESSONS-LEARNED.md:82-90` | Source claims are not runtime proof where generation or runtime behavior intervenes. |
| `docs/precepts/instructions/CONSUMING.md:13-20` | Precepts are read before local instructions; submodule pointer is source-of-truth. |

Core target facts:

| PASS-3 site | Current content |
|---|---|
| `restart/audit/pass-3-runtime/PASS-3.md:16-23` | Settled authority rejects `ParseStream`, rewrite-mode, grammar Unicode algebra, and prompt/inheritance stale clauses. |
| `restart/audit/pass-3-runtime/PASS-3.md:80` | Tape-backed `ValueRef` is the common cursor for path/select, visitors, debugger, CLI, LSP, and playground. |
| `restart/audit/pass-3-runtime/PASS-3.md:135-143` | Consumer acceptance gates include parse wrappers, `DocumentView` metadata, and materialisation cost artefacts. |
| `restart/audit/pass-3-runtime/PASS-3.md:184-186` | Snapshot-scoped tape identity, red-like views as transient projections, and DAP/debug identity are mandatory. |
| `restart/audit/pass-3-runtime/PASS-3.md:203-218` | `ReparsePlan` carries reuse maps, fallback reason, and invalidated query keys. |
| `restart/audit/pass-3-runtime/PASS-3.md:220-277` | Incremental recovery walkthrough includes `@error(recover = ...)`, recovery nodes, yaml syntax fallback, fallback ledgers, and LSP silence policy. |
| `restart/audit/pass-3-runtime/PASS-3.md:433-462` | Diagnostic ledger includes optimizer, pointer/select, recovery, type/value-shape, lookbehind, host, lowerer, and scanner-verifier routing. |
| `restart/audit/pass-3-runtime/PASS-3.md:476-512` | SOTA rows and generated budget rows now carry competitor, platform, metadata, and budget gates. |
| `restart/audit/pass-3-runtime/PASS-3.md:515-532` | Cross-pass hand-offs name receivers, blockers, and receiving gates. |
| `restart/audit/pass-3-runtime/PASS-3.md:566-581` | Remaining punch-list rows are receiver-gated implementation or synthesis work. |

The report was written after a clean `git status --short`. Other workers may create PASS-1, PASS-2, and MASTER-PLAN reports in parallel; this worker did not touch them.

## §2 Research-fold evidence map

| Research source | PASS-3 relevance | Fold evidence | V6 classification |
|---|---|---|---|
| Topic 1 HM foundations | PASS-3 should expose type/value diagnostic causes without publishing a type pass. | Topic 1 says HM/subsumption wording needs surgery but the research result is AMEND, not RE-DRAFT (`restart/research/topic-1-hm-foundations.md:550-585`). PASS-3 fold maps only diagnostic vocabulary to PASS-3 (`restart/research/fold-pass-3.md:73-75`, `:107-108`). | CLOSED in PASS-3: `BBNF-TYPE001` exposes expected/actual/value-shape causes, and PASS-3 states no public `TypeFacts` or higher-rank type pass leaks (`restart/audit/pass-3-runtime/PASS-3.md:454`, `:460`). |
| Topic 2 bidirectional | PASS-3 must not make DK or higher-rank machinery public; it may mirror user-facing subsumption/value-shape failure. | Topic 2 distinguishes Pierce-Turner local check/synth from DK higher-rank completeness (`restart/research/topic-2-bidirectional.md:383-390`, `:527-532`). Fold-pass-3 defers higher-rank gate wording and folds only diagnostic causality (`restart/research/fold-pass-3.md:76-78`, `:111-112`). | CLOSED for PASS-3: diagnostic row is value-shape/check-synth only; no public type pass. |
| Topic 3 CSP/GADTs | Generic-cycle/local-equality belongs to PASS-1/SYNTHESIS; PASS-3 must not invent checker codes. | Fold-pass-3 routes Topic 3 out of PASS-3 except already-covered materialisation/recovery and general TypeMismatch context (`restart/research/fold-pass-3.md:79-80`, `:114-115`). | N/A for PASS-3 amendment; no PASS-3 blocker. |
| Topic 4 egraphs | Bridge/egraph provenance belongs to PASS-1/SYNTHESIS, not PASS-3 runtime API. | Fold-pass-3 marks Topic 4 out-of-scope for PASS-3 (`restart/research/fold-pass-3.md:81`). Synthesis fold absorbs bridge/cost evidence in ARCH/MASTER (`restart/research/fold-synthesis.md:249-258`). | N/A for PASS-3; no runtime API change required. |
| Topic 5 cost models | PASS-3 owns materialisation-cost and optimizer-note evidence visible to runtime users. | Fold-pass-3 accepts materialisation objective evidence and optimizer wording (`restart/research/fold-pass-3.md:82-84`, `:116-120`). | CLOSED: materialisation cost gate includes scalar-cache, string-normalisation, repeated-access, objective vector, selected profile, and domination reason (`restart/audit/pass-3-runtime/PASS-3.md:141`, `:522`). |
| Topic 6 tape | One tape identity plus typed projections; no forward-only On-Demand semantics; benchmark metadata must name validation/source/materialisation mode. | Topic 6 says union is one identity plus projections (`restart/research/topic-6-tape.md:66-77`), and adversarial findings A1-A6 demand cost-class and identity precision (`restart/research/topic-6-tape.md:366-419`). Fold-pass-3 routes PASS-3 items to materialisation, identity, benchmark, and future tests (`restart/research/fold-pass-3.md:43-59`, `:90-95`). | CLOSED for PASS-3: one `(TapeId, node id, payload class)` trace, `TapeShape`/`ValueShape`, red-like transient views, benchmark metadata, and consumer gates are present (`restart/audit/pass-3-runtime/PASS-3.md:184`, `:476`, `:486`, `:522`). |
| Topic 7 green/red incremental | Snapshot-scoped `TapeId`, reuse maps, fault-tolerant yaml recovery, and query invalidation must be explicit. | Topic 7 demands snapshot-scoped identity and reuse maps (`restart/research/topic-7-green-red-incremental.md:560-590`) and records A1-A5 pressure (`restart/research/topic-7-green-red-incremental.md:616-667`). Fold-pass-3 maps this to PASS-3 identity, `ReparsePlan`, recovery node shape, yaml fallback, and debug/DAP (`restart/research/fold-pass-3.md:60-68`, `:96-100`, `:128-130`). | CLOSED: PASS-3 has snapshot identity, `reuse_map`, `fallback_reason`, `invalidated_queries`, typed recovery nodes, yaml fallback, and LSP silence (`restart/audit/pass-3-runtime/PASS-3.md:184`, `:203-218`, `:234-264`, `:277`). |
| Topic 8 SIMD/DFA | Exact SIMD must prove scalar parity; prefilter candidates require verifier acceptance before tape emission; no user `@simd`. | Topic 8 synthesis states SIMD positives are candidates unless exact, and DFA/VM/scalar verifier is acceptance authority (`restart/research/topic-8-simd-dfa.md:427-452`). A1-A5 preserve adversarial constraints (`restart/research/topic-8-simd-dfa.md:719-789`). Fold-pass-3 maps exact/prefilter consequences to PASS-3 diagnostics and runtime packaging (`restart/research/fold-pass-3.md:69-72`, `:101-105`, `:131`). | CLOSED: `BBNF-OPT002` states exact scalar parity and prefilter verifier routing; PASS-3 scanner rule blocks tape emission before acceptance (`restart/audit/pass-3-runtime/PASS-3.md:444`, `:462`). |

Fold report coherence:

| Fold report | PASS-3 result |
|---|---|
| `restart/research/fold-pass-1.md:138-180` | PASS-1 owns type algorithm, bridge, cost, and producer-side recovery facts. No ESCALATE item. |
| `restart/research/fold-pass-2.md:109-149` | PASS-2 owns `TapeShape`/`ValueShape`, exact/prefilter scanner, regex program, and benchmark metadata. No ESCALATE item. |
| `restart/research/fold-pass-3.md:86-123` | PASS-3 owns runtime-facing materialisation, identity, recovery, yaml fallback, diagnostics, and scanner diagnostic consequences. Zero ESCALATE items. |
| `restart/research/fold-synthesis.md:230-285` | Synthesis absorbs architecture/master/migration consequences; README/INDEX/lock-file hygiene is residual outside PASS-3. |

No research §6 item structurally contradicts Lock 1, Lock 8, Lock 10, or Lock 14 in a way that requires PASS-3 re-draft. The adversarial findings sharpen language and gates. They do not overturn the target.

Detailed V6 obligation closures:

1. One tape identity plus typed projections.
   - Explication: Topic 6 and Topic 7 allow direct roots and red-like views only as typed projections over one owning parse identity.
   - Pro: PASS-3 names `TapeId`, node id, payload class, `TapeShape`, and `ValueShape`, then sends path/select, visitor, DAP, CLI, LSP, and playground through `ValueRef`.
   - Con: README still says "One representation", which can be read as banning useful projections.
   - Challenge: The strictest reading would force a re-draft if projections were independent parse truth. PASS-3 blocks that by making them transient views over the same snapshot identity.
   - Acceptance: No PASS-3 amendment required; route README wording to consolidation hygiene.
2. Materialisation and cache policy.
   - Explication: Topic 6 rejects treating all scalar reads as constant-time, especially normalized strings and numeric conversion paths.
   - Pro: PASS-3 requires materialisation reports with scalar-cache, string-normalisation, repeated-access, selected profile, and domination reason.
   - Con: The exact generated schema is PASS-2-owned, so PASS-3 cannot prove final field names alone.
   - Challenge: A runtime doc that promises materialisation but never consumes it would be substrate without consumer. PASS-3 closes the receiver side with `DocumentView` and generated artefact gates.
   - Acceptance: Closed for PASS-3; PASS-2 remains producer owner.
3. Snapshot-scoped `TapeId`.
   - Explication: Topic 7 requires old/new identity to survive incremental reparses without pretending node ids are globally timeless.
   - Pro: PASS-3 states `TapeId` is snapshot-scoped and requires `reuse_map`, fallback reasons, and invalidated query keys in `ReparsePlan`.
   - Con: README does not yet carry the same precision.
   - Challenge: Incremental identity could become invisible if only benchmarks mention it. PASS-3 makes it part of runtime API, debug payload, and consumer gates.
   - Acceptance: Closed for PASS-3; README precision is residual.
4. Red-like cursor views.
   - Explication: Topic 7 permits red/green-style ergonomics only if they do not create a second authoritative tree.
   - Pro: PASS-3 says red-like cursor views are transient projections over the same tape snapshot.
   - Con: It does not prescribe a public red/green API type name.
   - Challenge: Lack of type-name prescription is acceptable because this pass owns runtime posture, not source API naming for every adapter.
   - Acceptance: Closed; no public dual-tree substrate appears.
5. YAML syntax-error recovery and fault tolerance.
   - Explication: Topic 7 and the V5 pathologies require yaml to exercise recovery without becoming a hardcoded grammar exception.
   - Pro: PASS-3 contains a yaml fallback trace, recovery node typing, fallback ledgers, LSP silence policy, and the two-surface source-plus-metadata onboarding rule.
   - Con: Fixture parity is intentionally future runtime evidence.
   - Challenge: The strongest objection is fixture-first yaml special casing. PASS-3 defeats it by making fixtures parity gates, not onboarding authority.
   - Acceptance: Closed for PASS-3; implementation fixture gates remain routed.
6. Pointer/select diagnostics.
   - Explication: Runtime diagnostics must name generated metadata failures rather than falling back to ad hoc path strings.
   - Pro: PASS-3 gives worked `pointer!` and `select!` paths and diagnostic rows for `BBNF-POINTER001/002` and `BBNF-GRAMMAR001`.
   - Con: The generated metadata producer remains PASS-2-owned.
   - Challenge: A consumer could still bypass metadata. PASS-3 rejects hardcoded registries and names generated metadata as the only validation source.
   - Acceptance: Closed for PASS-3 receiver behavior.
7. DAP/debug identity.
   - Explication: Debuggers must be able to correlate runtime values, spans, and snapshots without invented adapter-local identity.
   - Pro: PASS-3 requires DAP/debug events to reuse tape identity and include `SnapshotId`, `TapeId`, node kind, and source span.
   - Con: The actual DAP adapter implementation is later work.
   - Challenge: V5 hedging is gone; mandatory wording is sufficient at audit level because this pass is a planning target, not code.
   - Acceptance: Closed.
8. Exact scan parity and prefilter verifier routing.
   - Explication: Topic 8 divides exact SIMD from candidate prefilters; only exact paths may claim scalar parity, and candidate paths need verifier acceptance.
   - Pro: PASS-3 `BBNF-OPT002` explains exact scalar parity and prefilter verifier routing; scanner packaging blocks tape emission before acceptance.
   - Con: Low-level scanner generation is PASS-2-owned.
   - Challenge: A public `@simd` knob would violate Lock 10. PASS-3 has no positive `@simd` directive and routes optimization as compiler decision.
   - Acceptance: Closed for runtime diagnostics and packaging.
9. User-visible type/value diagnostics without a public type pass.
   - Explication: Topics 1 and 2 require honest user diagnostics while keeping HM/DK/CSP machinery internal to sibling surfaces.
   - Pro: PASS-3 exposes `BBNF-TYPE001` with expected shape, actual shape, source, and projection context, and states no public `TypeFacts` or higher-rank type pass leaks.
   - Con: README still needs type-system wording precision.
   - Challenge: PASS-3 would fail if it exported type inference as a runtime phase. It does not.
   - Acceptance: Closed for PASS-3; README wording is residual.
10. Fixture and yaml onboarding separation.
    - Explication: Fixture paths are runtime parity evidence, not grammar registration.
    - Pro: PASS-3 keeps `yaml.bbnf` and `workspace.metadata.bbnf.grammars.yaml` as the onboarding surfaces and defers fixtures to parity tests.
    - Con: Historical fixture text remains in other documents.
    - Challenge: If fixtures become the source of grammar truth, Lock 14 fails. PASS-3 prevents that by naming source and metadata surfaces.
    - Acceptance: Closed.
11. Benchmark and SOTA metadata.
    - Explication: Lock 8 permits SOTA claims only with competitor, dataset, and platform/procedure evidence.
    - Pro: PASS-3 throughput rows name competitor/dataset/platform and defer non-throughput claims to generated metadata artefacts.
    - Con: Some metadata fields are distributed across ARCHITECTURE and MASTER-PLAN.
    - Challenge: Distributed metadata can still pass if the receiving gate is explicit. PASS-3 names report metadata floors and cross-document consumers.
    - Acceptance: PASS_WITH_NOTE, not PASS-3-blocking.
12. Negative public-surface scan.
    - Explication: V6 must ensure research did not revive deleted or rejected public surfaces.
    - Pro: PASS-3 positive surfaces do not include `ParseStream`, rewrite-mode, grammar Unicode algebra, standalone `@recover`, `@pratt`, `@simd`, `OpenFrame`, `bbnf_ir::`, or `path!`.
    - Con: Some terms remain as archaeology or conflict guards.
    - Challenge: Archaeology becomes unsafe only if it is normative. PASS-3 marks these references as DISCARD, legacy alias, rejected form, or deletion gate.
    - Acceptance: Closed.

Research §6 adversarial disposition ledger:

| Source item | Adversarial pressure | PASS-3 treatment | Consolidation outcome |
|---|---|---|---|
| Topic 1 A/S set | HM/subsumption/CSP wording could overclaim solver power. | PASS-3 only exposes value-shape diagnostics and no public type pass. | Residual README wording, not PASS-3. |
| Topic 2 DK pressure | Higher-rank bidirectional completeness cannot be implied. | PASS-3 avoids higher-rank surface and routes only user-facing mismatch causes. | Residual README/future-proofing, not PASS-3. |
| Topic 6 A1 | Scalar materialisation is not uniformly constant-time. | PASS-3 materialisation-cost artefact records cache and conversion class. | Closed for runtime receiver. |
| Topic 6 A2 | One representation could wrongly ban useful projections. | PASS-3 says one identity plus typed projections and red-like transient views. | Closed; README wording residual. |
| Topic 6 A3 | Tape/direct union can hide ABI or wrapper drift. | PASS-3 uses `ValueRef` as common cursor and requires wrapper consumer gates. | Closed. |
| Topic 6 A4 | Debug identity can diverge from runtime identity. | PASS-3 requires DAP/debug to reuse `TapeId`/snapshot identity. | Closed. |
| Topic 6 A5 | Benchmarks can hide validation/source/materialisation modes. | PASS-3 SOTA rows include report metadata floor and materialisation profiles. | Closed with distributed-metadata note. |
| Topic 6 A6 | Source gaps should not become evidence. | Fold-pass-3 marks source-role gaps; PASS-3 does not rely on them. | Residual INDEX hygiene only. |
| Topic 7 A1 | Green/red phrasing can imply a second tree. | PASS-3 says red-like views are transient projections over one tape snapshot. | Closed. |
| Topic 7 A2 | Node identity across edits can become unscoped. | PASS-3 scopes identity to snapshot and requires `reuse_map`. | Closed. |
| Topic 7 A3 | Recovery can become untyped syntax debris. | PASS-3 has typed recovery nodes and diagnostic rows. | Closed. |
| Topic 7 A4 | Query caches can survive invalidating edits. | PASS-3 requires `invalidated_queries` and fallback reasons. | Closed. |
| Topic 7 A5 | Fault tolerance can silence real LSP errors. | PASS-3 defines fallback ledgers and LSP silence policy. | Closed. |
| Topic 8 A1 | SIMD exactness can be overclaimed. | PASS-3 ties exact paths to scalar parity. | Closed. |
| Topic 8 A2 | Prefilter positives can be treated as acceptance. | PASS-3 requires verifier routing before tape emission. | Closed. |
| Topic 8 A3 | Regex/DFA/VM ownership can drift to runtime user knobs. | PASS-3 exposes diagnostics, not `@simd` or `@pratt` forcing. | Closed. |
| Topic 8 A4 | Scanner diagnostics can hide optimizer provenance. | PASS-3 `BBNF-OPT002` explains exactness or verifier route. | Closed. |
| Topic 8 A5 | Benchmark parity can hide warmup/input/toolchain drift. | PASS-3 metadata floor plus ARCH/MASTER full rows preserve provenance. | Closed with note. |

## §3 Nine-lane verification table

| Lane | Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|---|
| 1 Lock-Adherence | `PASS-3.md:16`, `:184`, `:444`, `:476`, `:525` | PASS-3 honors tape/direct union, Lock 8 SOTA attribution, Lock 10 no-force SIMD/Pratt posture, and deletion of rewrite/Unicode/default declaration crates. | The PASS-3 text binds every runtime-visible node to snapshot-scoped tape identity and names verifier-before-tape for scanner paths. | README and prompt stale text still exists as inputs, and PASS-3 cites them as stale rather than editing them. | The strongest counterposition is that stale upstream phrasing should block PASS-3. PASS-3 defeats it by carrying conflict guards and no positive public `ParseStream`, rewrite-mode, grammar Unicode algebra, `@pratt`, or `@simd` surface. | KEEP / READY |
| 2 Sequencing-Discipline | `PASS-3.md:135-143`, `:515-532`, `restart/MASTER-PLAN.md:523` | PASS-3 is a pass synthesis, not a wave plan; sequencing applies through producer/consumer gates and Master tranche rows. | Consumer acceptance gates require PASS-2 emissions to compile under PASS-3 wrappers before close. | Lane 2 cannot be fully judged inside a PASS-only artefact. | The steelman says PASS-3 can still hide substrate-without-consumer risk. The receiving-gate table defeats it by naming runtime identity, generated metadata, scanner verifier, and diagnostic ledger consumers. | N/A for wave sequencing; PASS for receiver discipline |
| 3 Cohesion | `PASS-3.md:80`, `:184`, `:203-218`, `:407-431`, `:515-532` | Runtime API, path/select, visitors, incremental parse, DAP, yaml, and diagnostics all point to the same tape/direct identity and generated metadata. | The document now has worked examples rather than only tables: pointer/select, recovery, yaml fallback, and WASM route. | Exact ABI remains PASS-1/PASS-2-owned. | The challenge is that direct roots, red-like cursors, and AST adapters could become rival products. PASS-3 explicitly makes them transient projections over one owning tape. | KEEP / READY |
| 4 SOTA-Anchoring | `PASS-3.md:476-500`, `restart/locks/14-LOCKS.md:48`, `restart/README.md:328-336` | Throughput rows name competitor, dataset, and platform; non-throughput rows disclaim Lock 8 claims. | Benchmark metadata now records validation/source/materialisation/cache/profile/trace fields before non-throughput gates make claims. | CSS lightning-css rows still carry platform-ratification disclaimers inherited from README. | The challenge is that distributed metadata can thin provenance. PASS-3 cites Lock 8 and cross-document receiver rows, and Architecture/Master carry full metadata rows (`restart/ARCHITECTURE.md:1315-1320`, `restart/MASTER-PLAN.md:131-136`). | KEEP / READY |
| 5 Grammar-Authoritative | `PASS-3.md:124-133`, `:407-431`, `restart/README.md:13`, `restart/locks/14-LOCKS.md:60` | `pointer!`/`select!` validation uses generated metadata; yaml onboarding remains grammar source plus workspace metadata. | Hardcoded registries are deletion items, fixtures are parity evidence, and yaml has no Rust-per-grammar onboarding path. | PASS-3 names grammar rows for proof, which can look like per-grammar authority. | The challenge is overfitting to seed grammars. The per-grammar feeder table records generated outputs and forbids generic-crate match arms; it is evidence, not hand-written dispatch. | KEEP / READY |
| 6 Generated-Code-Budget | `PASS-3.md:502-513`, `restart/audit/pass-2-codegen/PASS-2.md:399-417` | PASS-3 anchors visitor/path/projection/diagnostic/regen budget to PASS-2 W3 baselines and +2 percent gates. | Budget rows separate LOC, sidecar bytes, wall time, and field/method count. | The budget is provisional until implementation generates materialisation reports. | The challenge says report-only budgets invite drift. PASS-3 requires `cargo xtask regen --check` wall gates and generated materialisation-cost artefacts as receiving gates. | KEEP / READY |
| 7 Friction-Forecast | `PASS-3.md:118-122`, `:439-458`, `:460`, `:532` | User-facing diagnostics cover pointer/select, lifetimes, layout, optimizer, recovery, type/value mismatch, host, lowerer import, and lookbehind. | `BBNF-TYPE001` exposes value-shape cause without `TypeFacts`; `BBNF-OPT002` explains verifier routing without `@simd`. | Cookbook receivers are future implementation/doc work. | The challenge is that diagnostic strings can drift across PASS, runtime, and cookbook. PASS-3 §8 makes every code appear in cookbook table-of-contents and runtime emit tests. | KEEP / READY |
| 8 Carry-Deferral | `PASS-3.md:515-532`, `:566-581`, `restart/research/fold-pass-3.md:135-139` | Carries name receiver, blocker, and receiving gate; denied edits are clearly routed. | Stale prompt/README/inheritance, registry deletion, PASS-2 metadata, diagnostic ledger, and benchmark rows all have gates. | README/INDEX hygiene remains outside this worker. | The challenge is that residuals can hide behind deferral. V6 classifies only out-of-scope hygiene as residual and finds no PASS-3-blocking surgery. | KEEP / READY |
| 9 Greenfield-Discipline | `PASS-3.md:31-38`, `:184`, `:407-412`, `:548-564`, `restart/research/fold-pass-3.md:123` | PASS-3 rejects parallel substrates, default declaration crates, rewrite-mode, grammar Unicode algebra, hardcoded registries, `TypedPath<..., ()>` placeholders, and arbitrary mutable values. | The runtime surface is one identity, generated metadata, visitor/edit-builder mutation, and no public type pass. | Some old terms survive as archaeology/conflict text. | The challenge says archaeology can become authority. PASS-3 labels stale terms as DISCARD, deletion, or conflict-guard context. | KEEP / READY |

Cohort count for this PASS-3 V6 audit:

| Class | Count |
|---|---:|
| KEEP / READY rows | 8 |
| N/A with precise classification | 1 |
| AMENDMENT-REQUIRED rows | 0 |
| RE-DRAFT rows | 0 |

Lane challenge-resolution notes:

1. Lock-Adherence.
   - Pro: PASS-3 is aligned with settled locks where it speaks normatively.
   - Con: Stale input text still exists outside this report's write scope.
   - Explication: The target's job is to classify stale input as non-authoritative and preserve runtime surfaces, not to edit README or prompts.
   - Challenge answer: No stale token becomes a positive runtime API in PASS-3.
2. Sequencing-Discipline.
   - Pro: PASS-3 names receivers, blockers, and gates for every cross-pass dependency it owns.
   - Con: It is not itself a wave ordering plan.
   - Explication: The lane is therefore N/A for direct tranche ordering but applicable to receiver discipline.
   - Challenge answer: Consumer gates prevent substrate-without-consumer closure.
3. Cohesion.
   - Pro: Runtime cursors, recovery, yaml, debug, and diagnostics all share identity and metadata language.
   - Con: Generated ABI details live in PASS-2.
   - Explication: Cohesion is judged by boundary alignment, not duplication of PASS-2 producer schema.
   - Challenge answer: PASS-3 consistently consumes `TapeShape`, `ValueShape`, and generated metadata instead of inventing parallel registries.
4. SOTA-Anchoring.
   - Pro: Throughput claims identify competitor, dataset, and platform.
   - Con: Some procedural metadata is cross-document rather than repeated inline.
   - Explication: Lock 8 requires evidence, and cross-document ledgers can satisfy that when receivers are named.
   - Challenge answer: The PASS_WITH_NOTE is about distributed metadata, not missing PASS-3 evidence.
5. Grammar-Authoritative.
   - Pro: `yaml.bbnf` plus workspace metadata remain the onboarding authority.
   - Con: Fixtures are frequently named because runtime parity must test yaml.
   - Explication: Evidence fixtures are not grammar registration.
   - Challenge answer: PASS-3 explicitly keeps fixtures as parity-phase inputs.
6. Generated-Code-Budget.
   - Pro: PASS-3 consumes PASS-2 W3 budget baselines and adds runtime materialisation gates.
   - Con: Final generated counts do not exist before implementation.
   - Explication: A planning pass can require the budget artefact and close only when it exists.
   - Challenge answer: `regen_wall`, generated LOC, and materialisation reports are explicit acceptance gates.
7. Friction-Forecast.
   - Pro: PASS-3 gives concrete diagnostic codes and user-facing failure causes.
   - Con: Future docs/cookbook text can still drift.
   - Explication: PASS-3 owns the diagnostic ledger and receiving tests, not all prose in future cookbook pages.
   - Challenge answer: The ledger and test receivers are sufficient for this audit.
8. Carry-Deferral.
   - Pro: Every carry has a receiver or blocker.
   - Con: Residual hygiene remains for README, INDEX, and optional lock rationale.
   - Explication: Deferral is acceptable only when the target no longer owns the edit.
   - Challenge answer: No deferred row hides a PASS-3-local contradiction.
9. Greenfield-Discipline.
   - Pro: PASS-3 rejects legacy crate names, registries, force directives, and parallel trees.
   - Con: Deletion archaeology remains visible by design.
   - Explication: Greenfield discipline is about the planned public substrate, not forgetting why old paths were deleted.
   - Challenge answer: Archaeology is fenced as conflict or deletion evidence.

## §4 Sixteen-command gate-rerun results

Minimum commands were run:

| Command | Result |
|---|---|
| `git status --short` | Clean before report creation. |
| `wc -l restart/audit/pass-3-runtime/PASS-3.md restart/research/fold-pass-3.md restart/research/topic-{1,2,6,7,8}-*.md` | PASS-3 585 lines; fold-pass-3 161; topics 1/2/6/7/8 total 3,453 topic lines; total command output 4,199 lines. |
| `rg -n "TapeId|red-like|green|incremental|fault|recover|@error|DAP|debug|yaml|TapeShape|ValueShape|materiali|SIMD|DFA|prefilter|scalar parity|pointer!|select!|TypeMismatch|higher-rank|public type pass|@pratt|@simd|path!|Wave 4|ESCALATE|DEFER" ...` | Expected evidence found in PASS-3/fold/topic 6/7/8. No unclassified positive `@pratt`, `@simd`, `path!`, or Wave-4 runtime surface. |
| `git diff --check` | Clean before report creation. |
| `git diff --cached --check` | Clean before report creation. |

Tightened V5 16-command subset rerun across README, ARCHITECTURE, MIGRATION, MASTER-PLAN, PASS-1, PASS-2, and PASS-3:

| # | Command family | Observed result | V6 classification |
|---:|---|---|---|
| 1 | `ParseStream|rewrite-mode|Unicode class algebra` | PASS-3 hits are stale conflict / DISCARD / guard rows (`PASS-3.md:16-23`, `:32`, `:525`, `:570`, `:585`). README, ARCH, MASTER, MIGRATION also carry conflict/deletion or route text. | PASS for PASS-3; residual non-blocking README/prompt/inheritance hygiene remains. |
| 2 | `bbnf-path|bbnf-test-fixtures|path!` | PASS-3 has legacy evidence only at `PASS-3.md:84` and a naming deletion gate at `PASS-3.md:573`; no `path!` public surface. | PASS |
| 3 | `codegen/src/backend_ir` | PASS-2 hits are documentation-only boundary rows (`PASS-2.md:200`, `:237`, `:252`). | PASS; not PASS-3-owned. |
| 4 | `fixtures/yaml` | PASS-3 confines hits to parity-phase prose and explicitly excludes onboarding authority (`PASS-3.md:407-412`, `:429`, `:580`). | PASS |
| 5 | `@recover` | PASS-3 and ARCH classify standalone `@recover` as legacy alias / rejected standalone form (`PASS-3.md:35`, `:190`; `ARCHITECTURE.md:1134`). | PASS |
| 6 | `OpenFrame` | Hits are deletion archaeology or negative gates in README/ARCH/MASTER/MIG/PASS-1/PASS-2; PASS-3 has no hit. | PASS |
| 7 | `GrammarIR` | Hits show lowerer-deny context and type inference references; no codegen ownership drift (`PASS-1.md:43`, `PASS-2.md:253-254`, `MIGRATION.md:729`). | PASS |
| 8 | `__EAGER_EMPTY_PATH|CursorDecision::Skip` | Cursor skip gates exist in PASS-2/ARCH/MASTER/MIG (`PASS-2.md:180`, `ARCHITECTURE.md:823-824`, `MASTER-PLAN.md:773`, `MIGRATION.md:743`). | PASS; not PASS-3-owned. |
| 9 | `twitter|canada|citm|bootstrap|animate|On-Demand` | PASS-3 rows include fallback datasets and SOTA gates (`PASS-3.md:272-273`, `:480-500`); ARCH/MASTER carry full metadata rows. | PASS |
| 10 | `receiver|blocker|receiving gate` | PASS-3 §8 and §10 carry receiver/blocker/gate rows (`PASS-3.md:143`, `:532`, `:575`, `:578`). | PASS |
| 11 | `yaml.bbnf|workspace.metadata.bbnf.grammars.yaml` | Two-surface yaml onboarding present across README, PASS-1/2/3, ARCH, MASTER, MIG (`README.md:13`, `PASS-3.md:407-431`, `ARCHITECTURE.md:1336-1376`). | PASS |
| 12 | `generated_loc|regen_wall|xtask` | PASS-3 generated budget and regen gates present (`PASS-3.md:502-512`), with PASS-2 generated budget backing. | PASS |
| 13 | `BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|BBNF-GRAMMAR|BBNF-POINTER|lookbehind|HostSignature` | PASS-3 diagnostic ledger is present; PASS-2 optimizer strings do not teach `@pratt`/`@simd` forcing (`PASS-3.md:439-458`, `PASS-2.md:554-555`). | PASS |
| 14 | `child count|500 LOC|exception rationale` | Lock 13 visible in README, PASS-2, ARCH, MASTER, MIG, and PASS-3 budget rows (`README.md:98`, `PASS-2.md:419-433`, `PASS-3.md:506`). | PASS |
| 15 | `declaration-crate review|why metadata|deletion path|reviewer` | Rare declaration-crate review fields are routed through PASS-3/ARCH/MASTER/MIG (`PASS-3.md:575`, `ARCHITECTURE.md:748-754`, `MASTER-PLAN.md:771`). | PASS |
| 16 | `CPU model|compiler flags|input hash|competitor version|warmup|sample` | PASS-3 names report metadata floor (`PASS-3.md:476`); ARCH/MASTER carry row-level metadata (`ARCHITECTURE.md:1315-1320`, `MASTER-PLAN.md:143-149`). | PASS_WITH_NOTE: distributed metadata is acceptable because PASS-3 points to receiver gates. |

Command-family interpretation notes:

1. The stale-token scan was treated as a positive-surface scan, not a raw-hit ban, because PASS-3 intentionally cites stale text while rejecting it.
2. The path scan distinguishes `bbnf-path` archaeology from public `path!`; only the latter would be PASS-3-blocking.
3. The `backend_ir` scan validates that PASS-3 does not absorb PASS-2 producer ownership.
4. The yaml fixture scan checks that fixtures remain parity evidence rather than onboarding authority.
5. The `@recover` scan confirms the accepted grammar form remains `@error(recover = ...)`.
6. The `OpenFrame` scan confirms deletion archaeology does not leak into PASS-3.
7. The `GrammarIR` scan confirms lowerer/type references do not create runtime codegen ownership drift.
8. The skip-sentinel scan is N/A for PASS-3 except as a generated-code consumer boundary.
9. The SOTA dataset scan confirms PASS-3 still carries throughput comparison anchors after the research fold.
10. The receiver scan is load-bearing because Phase 6 needs blocker and receiving-gate evidence, not only prose.
11. The yaml source scan is load-bearing because Lock 14 requires grammar-source authority and workspace metadata authority to stay separate from fixtures.
12. The generated-budget scan is load-bearing because PASS-3 closes only when generated artefacts can be checked.
13. The diagnostic-code scan confirms user-visible messages are explicit and do not teach force directives.
14. The child-count scan confirms Lock 13 budget exception logic remains visible to the runtime pass.
15. The declaration-crate scan confirms rare fenced review remains routed, not generalized into a default crate policy.
16. The benchmark-metadata scan produces the only PASS_WITH_NOTE because metadata is distributed but still named by receiving gates.

Additional focused V6 checks:

| Check | Result |
|---|---|
| `path!|@pratt|@simd|OpenFrame|bbnf_ir::|should reuse this identity|waves-v4|wave-4|Wave 4` against PASS-3 + fold-pass-3 | PASS-3 has zero hits; fold-pass-3 hits are command/prohibition context only (`fold-pass-3.md:83`, `:146`, `:161`). |
| Type leakage scan over PASS-3/fold/ARCH/MASTER/PASS-1 | PASS-3 has `TypeMismatch` and `BBNF-TYPE001` only as user diagnostics, while `TypeFacts` remains internal in PASS-1/ARCH/MASTER (`PASS-3.md:454`, `:460`; `ARCHITECTURE.md:1012-1013`; `MASTER-PLAN.md:313`). |
| Provenance-gap scan over Topic 1/2/6/7/8 and folds | Hubbard, Ungar/Adams, HelpMate, Roc, Almomany, Deb are classified as provenance gaps or source-role hygiene, not PASS-3 evidence (`fold-pass-3.md:94`, `:100`, `:113`, `:121`; `fold-synthesis.md:310`). |

No command result supports AMENDMENT-REQUIRED for PASS-3. Command 1 and provenance scans preserve non-PASS-3 residuals only.

## §5 F/G/H pathology regression scan

V5 pathology baseline:

| V5 lens | Prior PASS-3 concern | V6 evidence | Result |
|---|---|---|---|
| F - LLM bias / hedged runtime invariant | V5 flagged "debug and DAP should reuse tape identity" as hedged (`HARDENING-PASS-3-V5.md:128`, `:284`). | Current PASS-3 says Debug and DAP must reuse identity and requires `SnapshotId`, `TapeId`, node kind, and source span (`PASS-3.md:186`). V5.1 verified this closure (`HARDENING-PASS-3-V5.1.md:111`). | CLEAN |
| F - closure bias around examples | V5 flagged missing pointer/select and recovery examples (`HARDENING-PASS-3-V5.md:281-284`). | PASS-3 now has pointer/select worked path and failure diagnostics (`PASS-3.md:94-122`), plus incremental recovery and yaml fallback (`PASS-3.md:220-264`). | CLEAN |
| F - pseudo-precision | V5 noted fallback-rate precision needed bench owner/source context (`HARDENING-PASS-3-V5.md:130`). | PASS-3 binds fallback rates to `incremental/edit_anchor` bench rows and LSP silence policy (`PASS-3.md:268-277`), and Master I.W1 owns reuse/fallback gates (`MASTER-PLAN.md:523`). | CLEAN |
| G - overfitting / old `path!` shape | V5 required settled `pointer!`/`select!` examples (`HARDENING-PASS-3-V5.md:282`). | PASS-3 uses `pointer!` and `select!` only, generated metadata only, and legacy `bbnf-path` as archaeology (`PASS-3.md:84-133`). | CLEAN |
| G - tree-sitter recovery mimicry | V5 flagged generic recovery without grammar-local trace (`HARDENING-PASS-3-V5.md:135`). | PASS-3 recovery trace includes BBNF directive, dirty range, anchors, `RecoveryKind`, `BBNF-RECOVERY001`, `VisitTypes::ERROR`, and yaml fallback (`PASS-3.md:220-264`). | CLEAN |
| G - yaml special casing | V5/V5.1 required yaml to remain two-surface and not fixture-first (`HARDENING-SYNTHESIS-V5.1.md:60-64`, `:135`). | PASS-3 keeps yaml source + metadata only, with fixture parity postponed (`PASS-3.md:407-431`). | CLEAN |
| G - `@pratt` / `@simd` drift | V5 flagged optimizer diagnostic drift in PASS-2/PASS-3 (`HARDENING-CONSOLIDATED-V5.md:14`, `HARDENING-PASS-3-V5.1.md:50`). | PASS-3 has no positive `@pratt` or `@simd`; `BBNF-OPT001/002` explain cost/objective/exactness failure (`PASS-3.md:443-444`). Fold-pass-3 only mentions the tokens in command/prohibition context. | CLEAN |
| H - wrong lock provenance | V5 flagged PASS-3 SOTA citation to wrong lock (`HARDENING-PASS-3-V5.md:138`, `:277`). | PASS-3 cites Lock 8 at `restart/locks/14-LOCKS.md:48` (`PASS-3.md:476`, `:488`). | CLEAN |
| H - stale crate prefix | V5 flagged `bbnf_ir::` lowerer diagnostic (`HARDENING-PASS-3-V5.md:278`). | PASS-3 lowerer diagnostic uses `ir::grammar_ir` and `ir::backend_ir` (`PASS-3.md:458`). | CLEAN |
| H - lookbehind alias provenance | V5 required `BBNF1004` / alias / kind binding (`HARDENING-PASS-3-V5.md:279`). | PASS-3 diagnostic row binds `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, and `LookbehindWidth` (`PASS-3.md:453`, `:460`). | CLEAN |
| H - source provenance gaps | V6 research surfaced Hubbard, Ungar/Adams, HelpMate, Roc, Almomany, Deb source-role problems. | PASS-3/fold-pass-3 do not cite these as evidence; folds mark them DEFER or source hygiene (`fold-pass-3.md:94`, `:100`, `:113`, `:121`; `fold-synthesis.md:310`). | CLEAN for PASS-3; residual INDEX hygiene |

Pathology summary:

| Lens | Conclusion |
|---|---|
| F | No hedged runtime invariant, pseudo-precision, or example-free closure remains in PASS-3. |
| G | PASS-3 does not overfit yaml, tree-sitter recovery, old path packages, or SIMD/Pratt directives. |
| H | PASS-3 no longer depends on wrong lock lines, stale crate prefixes, or unverified research leads. |

## §6 Cross-document binding ledger

| Binding | PASS-3 anchor | Cross-document anchor | V6 result |
|---|---|---|---|
| One tape/direct identity | `PASS-3.md:184-186` | Lock 1 (`restart/locks/14-LOCKS.md:34`); ARCH one identity smoke (`restart/ARCHITECTURE.md:1237`); MIG runtime gate (`restart/MIGRATION.md:751-753`) | CLOSED |
| `TapeShape` / `ValueShape` policy | `PASS-3.md:184`, `:522` | PASS-2 owns shape policy (`restart/audit/pass-2-codegen/PASS-2.md:36`, `:54`); Topic 6 close gate demands convergence (`topic-6-tape.md:498-504`) | CLOSED for PASS-3 receiver; PASS-2 owns producer details |
| Red-like cursor views | `PASS-3.md:184` | Topic 7 permits red-like views over one owning representation (`topic-7-green-red-incremental.md:618-627`); synthesis folds view permission (`fold-synthesis.md:266`) | CLOSED |
| Snapshot-scoped `TapeId` and reuse map | `PASS-3.md:184`, `:203-218` | Master I.W1 gate (`restart/MASTER-PLAN.md:523`); Migration incremental replacement (`restart/MIGRATION.md:188`, `:417`) | CLOSED |
| Recovery and yaml syntax-error tolerance | `PASS-3.md:220-264`, `:277`, `:407-431` | Architecture yaml walkthrough (`restart/ARCHITECTURE.md:1336-1376`); Master yaml syntax-error friction row (`restart/MASTER-PLAN.md:806`) | CLOSED |
| Pointer/select diagnostics | `PASS-3.md:94-133`, `:446-448` | Architecture diagnostic aliases (`restart/ARCHITECTURE.md:1044-1046`); Master friction row (`restart/MASTER-PLAN.md:799`) | CLOSED |
| DAP/debug identity | `PASS-3.md:186`, `:500` | Topic 6 settled debug claim (`topic-6-tape.md:60-61`); V5.1 verified mandatory wording (`HARDENING-PASS-3-V5.1.md:48`, `:111`) | CLOSED |
| Exact SIMD parity and prefilter verifier routing | `PASS-3.md:444`, `:462`, `:524` | PASS-2 scanner gate (`restart/audit/pass-2-codegen/PASS-2.md:106`, `:172`, `:587`); ARCH BIR rows (`restart/ARCHITECTURE.md:936`, `:964`, `:1276`); MASTER Lock 10 close (`restart/MASTER-PLAN.md:708`) | CLOSED |
| Public type/value diagnostics without public type pass | `PASS-3.md:454`, `:460`, `:532` | PASS-1 internal `TypeFacts` (`restart/audit/pass-1-substrate/PASS-1.md:151`); ARCH internal `TypeFacts`/`TypeObligationLog` (`restart/ARCHITECTURE.md:1012-1013`); MASTER C.W1 (`restart/MASTER-PLAN.md:313`) | CLOSED |
| Materialisation/cache policy | `PASS-3.md:141`, `:476`, `:486`, `:522` | PASS-2 materialisation table gate (`restart/audit/pass-2-codegen/PASS-2.md:359`); ARCH payload/materialisation metadata (`restart/ARCHITECTURE.md:1235`, `:1305-1307`); MASTER runtime materialisation row (`restart/MASTER-PLAN.md:778`) | CLOSED |
| Fixture and yaml onboarding separation | `PASS-3.md:407-412`, `:429-431` | README two surfaces (`restart/README.md:13`); Lock 14 (`restart/locks/14-LOCKS.md:60`); ARCH yaml row (`restart/ARCHITECTURE.md:1400`) | CLOSED |
| Rare declaration-crate fence | `PASS-3.md:575` | PASS-1 review form pointer (`restart/audit/pass-1-substrate/PASS-1.md:94-97`); ARCH review fields (`restart/ARCHITECTURE.md:748-754`); MASTER carry (`restart/MASTER-PLAN.md:771`) | CLOSED |
| SOTA benchmark metadata | `PASS-3.md:476-500` | ARCH SOTA metadata table (`restart/ARCHITECTURE.md:1315-1320`); MASTER SOTA rows (`restart/MASTER-PLAN.md:131-149`) | CLOSED with distributed-metadata note |

No cross-document binding ledger row requires PASS-3 surgery.

## §7 Punch list

PASS-3-blocking punch list:

| Path:line | Surgery | Acceptance gate | Origin | V6 status |
|---|---|---|---|---|
| none | none | no PASS-3 amendment required | V6 nine-lane audit + gate rerun | READY |

Residual non-PASS-3 items:

| Path:line | Surgery | Acceptance gate | Origin | PASS-3 blocking? |
|---|---|---|---|---|
| `restart/README.md:308` | Split constant-time string projection from digit-linear or payload-backed scalar materialisation. | `rg -n "Scalar methods|digit-linear|TapeShape|normalized string" restart/README.md` and no misleading constant-time scalar sentence. | Topic 6 A1/S1 (`topic-6-tape.md:366-374`, `:423-427`). | No. PASS-3 already requires materialisation cost classes and generated artefacts (`PASS-3.md:141`, `:522`). |
| `restart/README.md:314` | Replace "One representation" with one owning/authoritative identity language that permits transient red-like views. | `rg -n "one owning representation|red-like cursors|independent parse identity" restart/README.md`. | Topic 6 A2/S10 and Topic 7 A1/S1 (`topic-6-tape.md:375-383`, `:460-463`; `topic-7-green-red-incremental.md:618-627`, `:671-677`). | No. PASS-3 and ARCH already bind one identity and transient projections. |
| `restart/README.md:344-346` | Scope incremental identity to snapshot `TapeId`, reuse maps, and query invalidation reasons. | `rg -n "snapshot-scoped.*TapeId|old-to-new reuse|QueryInvalidationSet" restart/README.md restart/audit/pass-3-runtime/PASS-3.md`. | Topic 7 A2/A4/S2/S3 (`topic-7-green-red-incremental.md:629-656`, `:679-695`). | No. PASS-3 and MASTER already carry `ReparsePlan` reuse maps and I.W1 gates. |
| `restart/README.md:260`, `:266` | Decompose HM/subsumption/CSP wording into HM core, expected checking, bounded coercion, finite CSP; scope DK to future higher-rank/indexed surfaces. | `rg -n "full Hindley-Milner with subsumption|CSP-backed unification" restart/README.md` returns no active positive hit; DK hits name proof gates. | Topics 1/2 (`topic-1-hm-foundations.md:550-585`; `topic-2-bidirectional.md:383-390`). | No. PASS-3 exposes only user diagnostics and no public type pass. |
| `restart/research/INDEX.md:54`, `:63`, `:119`, `:126-133` | Repair source/lock-role hygiene for Topic 2 lock number, Roc source role, Hubbard, Ungar/Adams, HelpMate, and related provenance gaps. | Index rows classify unverified sources as gaps or supply primary URLs/DOIs. | Fold-pass-3 and synthesis residue (`fold-pass-3.md:94`, `:100`, `:113`, `:121`; `fold-synthesis.md:283`, `:310`). | No. PASS-3 does not cite these leads as evidence. |
| `restart/locks/14-LOCKS.md:40` / explanatory surroundings | Optional rationale note for Lock 4's egglog counterargument if a future lock-hygiene pass is authorized. | Lock hygiene pass states why bridge-vs-fusion survives egglog-style pressure without changing settled lock semantics. | Synthesis fold T4-A2 (`fold-synthesis.md:250`, `:281-282`). | No. No structural lock contradiction. |

No PASS-3-local surgery is recommended. The residual table is for consolidation so Phase 6 can distinguish true blockers from hygiene.

## §8 V5/V5.1-to-V6 history note

| Cycle | PASS-3 posture |
|---|---|
| V4 | PASS-3 READY after Wave 4.1 closed bench-row attribution, W3 baseline anchors, yaml host-route cell, and visitor cookbook routing (`HARDENING-PASS-3-V4.md:40-51`, `HARDENING-CONSOLIDATED-V4.md:104-112`). |
| V5 | PASS-3 AMENDMENT-REQUIRED because stale citations, weak debug/DAP wording, and sparse worked examples remained (`HARDENING-CONSOLIDATED-V5.md:15`, `HARDENING-PASS-3-V5.md:274-287`). |
| V5.1 | PASS-3 route READY after runtime examples, diagnostic provenance, mandatory DAP/debug identity, WASM route, alias binding, and rare-fence polish closed (`HARDENING-PASS-3-V5.1.md:54-62`, `:107-122`, `:188-192`). |
| V6 | Research fold adds Topic 1-8 pressure. PASS-3 absorbs its owned parts: materialisation/cache policy, one identity plus typed projections, snapshot `TapeId`, red-like views, yaml recovery, pointer/select diagnostics, DAP/debug identity, exact/prefilter scanner routing, and user-visible type/value diagnostics without public type pass. |

V6 does not overturn V5.1. It re-tests V5.1 with the research adversarial material included. The result remains READY.

## §9 Final verdict

Verdict: READY.

Grounds:

| Gate | Result |
|---|---|
| Research-fold coherence | PASS. All PASS-3-owned Topic 1/2/5/6/7/8 consequences are present. Topics 3/4 are sibling/synthesis scope for PASS-3 purposes. |
| Nine-lane audit | PASS. Eight KEEP/READY rows, one N/A with precise sequencing classification, zero amendment rows. |
| Sixteen-command rerun | PASS. One distributed-metadata note and one out-of-scope stale-input/hygiene class; no PASS-3-blocking failure. |
| F/G/H pathology scan | PASS. No new LLM-bias, overfitting, or provenance regression in PASS-3 or fold-pass-3. |
| Cross-document binding | PASS. PASS-3 agrees with PASS-1/PASS-2/SYNTHESIS receiver boundaries. |
| Punch list | PASS. No PASS-3-local surgery. |

Re-draft thresholds met: zero.

Amendment-required threshold for PASS-3 met: zero.

Residuals remain outside PASS-3: README wording precision, research-index/source hygiene, and optional lock-rationale hygiene. They should be visible to consolidation, but they do not block PASS-3.

## §10 Closing posture

PASS-3 is fit for Phase 6 consolidation as READY.

The runtime surface is coherent after V6: typed roots remain the default, `ValueRef` is the shared projection cursor, tape identity is snapshot-scoped, direct roots and red-like views do not own independent parse truth, recovery nodes are typed runtime state, yaml syntax faults stay within the two-surface onboarding path, pointer/select diagnostics are generated-metadata-driven, DAP/debug events carry tape identity, scanner fast paths cannot emit tape before exact or verifier acceptance, and type/value diagnostics expose causes without making type inference a public pass.

The consolidation worker should route the non-PASS-3 residuals by receiver. It should not reopen PASS-3 unless another target introduces a direct contradiction against the lines verified here.
