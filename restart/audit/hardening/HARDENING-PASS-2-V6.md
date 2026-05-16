# HARDENING-PASS-2-V6

## §1 Target identification and commits audited

Target: `restart/audit/pass-2-codegen/PASS-2.md`.

Output: `restart/audit/hardening/HARDENING-PASS-2-V6.md`.

Worker role: Phase 3 V6 hardening worker, `target = PASS-2`.

Write scope: this report only.

Initial worktree state: clean by `git status --short`.

Audited corpus head:

| Commit | Role |
|---|---|
| `00c51814` | Current HEAD during this hardening pass: `docs(restart/synthesis): wave-5-fold amendment — research grounding fold`. |
| `d1096c21` | Latest PASS-2 research-fold amendment touching `PASS-2.md`. |
| `7064b764` | PASS-2 fold classification: topics 4, 5, 6, and 8 routed into PASS-2. |
| `b64a18a1` | V5.1 PASS-1/PASS-2 narrow amendment: recognizer diagnostics and provenance. |
| `e647139c` | V5.1 verification report: amendment-required for shifted PASS-local citations. |
| V5.1A report | Closed the shifted local citation residue and returned READY. |

Files counted in the minimum preflight:

| File | Lines |
|---|---:|
| `restart/audit/pass-2-codegen/PASS-2.md` | 589 |
| `restart/research/fold-pass-2.md` | 216 |
| `restart/research/topic-3-csp-gadts.md` | 594 |
| `restart/research/topic-4-egraphs.md` | 926 |
| `restart/research/topic-5-cost-models.md` | 895 |
| `restart/research/topic-6-tape.md` | 504 |
| `restart/research/topic-8-simd-dfa.md` | 931 |

Required reading consumed:

| Artefact | Use in this report |
|---|---|
| `restart/prompts/sub-orchestrators/HARDENING.md` | Phase 3 lane shape and Phase 6 consolidation shape. |
| `restart/prompts/audit-specs/HARDENING-LENS-SET.md` | Nine-lane audit and Pro/Con/Explication/Challenge discipline. |
| `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` §1 | Verify-then-patch discipline and pre-fill caution. |
| `restart/README.md` | Gestalt, two-surface onboarding, pass order, SOTA, tape/direct, and voice locks. |
| `restart/locks/LOCKS.md` | Lock 1, 4, 5, 6, 8, 10, 13, and 14 checks. |
| V4/V5/V5.1/V5.1A hardening reports | Regression baseline for directive drift, citation drift, and F/G/H pathology. |
| Eight topic research artefacts | Topic §6 adversarial pressure and §5/§7 fold pressure. |
| Four fold reports | PASS-2 fold classification and sibling/out-of-scope routing. |
| `PASS-2.md` | Direct target. |
| `ARCHITECTURE.md`, `MASTER-PLAN.md`, `MIGRATION.md` | Binding ledger and gate-rerun cross-checks. |
| PASS-1/PASS-3 | BIR producer, diagnostic owner, runtime diagnostic, and scanner verifier bindings. |
| Precepts | Style, commit body, and consumer discipline. |

Target posture before V6:

PASS-2 is no longer the V5 target that taught `@pratt` and `@simd` as author
escape hatches. Current `PASS-2.md:554-555` names objective-profile and
exactness reasons, and says metadata may disable unsupported SIMD kernels but
cannot force SIMD. The recognizer directive regression is closed on the PASS-2
surface.

The V6 audit therefore asks a narrower and harder question: did the research
fold add the right evidence without reopening old surface drift, citation
confidence, genericity leaks, or SIMD/regex over-selection?

Short answer: yes for PASS-2. The remaining residue is research-index and
gate-harness hygiene outside the PASS-2 write surface.

## §2 Research-fold evidence map

### §2.1 Fold ownership map

| Research source | PASS-2-relevant pressure | Current PASS-2 evidence | V6 verdict |
|---|---|---|---|
| Topic 1 HM foundations | Generic monomorphisation must stay finite. | `PASS-2.md:417` requires a finite `(RuleId, TypeArgs)` set before lowerers emit instances. | KEEP |
| Topic 2 bidirectional | Mostly PASS-1/SYNTHESIS; no direct PASS-2 surgery. | PASS-2 cites PASS-1 diagnostic ownership by section and does not design type inference. | KEEP |
| Topic 3 CSP/GADTs | Monomorphisation can explode without a finite instance set. | `PASS-2.md:417` and generated LOC gate bind emission to PASS-1 validation. | KEEP |
| Topic 4 e-graphs | Extraction is the choice point; representative promotion is unstable. | `PASS-2.md:32`, `56-57`, and `378` make `passes::extract` and `BridgeJustification` evidence the lowerer input. | KEEP |
| Topic 5 cost models | Scalar-only cost is too weak; vectors, profile, Pareto residue, and regex opacity must survive. | `PASS-2.md:374-376` records scalar score, objective vector, selected profile, legality, child/e-class/BIR ids, dominated/Pareto evidence, and `RegexCostSummary`. | KEEP |
| Topic 6 tape | Tape/direct union can be misread as two authorities. | `PASS-2.md:36`, `54`, and `109` bind `TapeShape` and `ValueShape` to one tape identity, payload class, traversal policy, and scalar-cache policy. | KEEP |
| Topic 7 green/red incremental | Runtime snapshot identity is PASS-3/SYNTHESIS; no PASS-2 cache owner should appear. | PASS-2 only emits `incremental_marker` as optional metadata at `PASS-2.md:153`; it does not own green/red runtime identity. | KEEP |
| Topic 8 SIMD/DFA | Exact scans need scalar parity; prefilters need verifier-before-tape; full DFA cannot be mandatory. | `PASS-2.md:81`, `106`, `172`, `470`, and `578` bind regex-program compatibility, exact scalar parity, prefilter verifier route, and `regex-automata` oracle parity. | KEEP |

### §2.2 Adversarial reconciliation

| Finding family | Research pressure | PASS-2 fold result | V6 classification |
|---|---|---|---|
| E-graph representative instability | Topic 4 A1 says the selected alternative must not depend on a stale representative. | PASS-2 lowerers consume selected BIR plus solved facts and bridge-justification evidence. | FOLD-SATISFIED |
| Egglog counterargument | Topic 4 A2 asks Lock 4 to acknowledge fusion SOTA. | Fold-pass-2 classifies this as lock/Architecture rationale, outside PASS-2. | RESIDUAL-NON-BLOCKING |
| Rewrite budgets | Topic 4 A3 asks seven rewrite categories to carry budget gates. | PASS-2 consumes extraction evidence; rewrite scheduler/budget belongs PASS-1/MASTER. | RESIDUAL-NON-BLOCKING |
| Bridge explanation payloads | Topic 4 A4 asks bridge outputs to preserve proof refs. | `PASS-2.md:378` names `BridgeJustification` keyed by stable ids and proof refs. | FOLD-SATISFIED |
| Scalar-only cost | Topic 5 A1 rejects `u64` as the only durable cost evidence. | `PASS-2.md:374-376` allows scalar Cost only as a fast path when the full record survives. | FOLD-SATISFIED |
| DAG sharing | Topic 5 A2 warns branch iterators can double-count shared expressions. | PASS-2 requires stable child/e-class/BIR ids in the `CostDecision` record. | FOLD-SATISFIED |
| Regex opacity | Topic 5 A3 says parser cost must not inspect regex internals. | PASS-2 consumes opaque `RegexCostSummary` and forbids regex HIR/NFA/DFA/VM internals in the lowerer. | FOLD-SATISFIED |
| SMT objective policy | Topic 5 A5 says objective mode must be explicit. | PASS-2 accepts profile/objective evidence without designing the solver; Architecture/MASTER own objective-mode gates. | FOLD-SATISFIED-FOR-PASS-2 |
| Almomany/Deb provenance | Topic 5 A6 says named gaps must not become evidence. | PASS-2 has no word-boundary `Hubbard`, `Almomany`, or exact `Deb 2014` evidence. Fold report names them only as gaps. | FOLD-SATISFIED |
| Tape/direct two-authority risk | Topic 6 A2 is Lock-1 high pressure. | `PASS-2.md:36` says one tape identity and node id; scalar caches are declared by shape, not a second tree. | FOLD-SATISFIED |
| Benchmark fairness | Topic 6 A4/A6 asks validation/source-ownership/entry-point columns. | `PASS-2.md:462` records validation mode, source ownership mode, materialisation mode, scalar-cache policy, parse entry point, competitor flags, and input hash. | FOLD-SATISFIED |
| Hubbard provenance | Topic 6 A5 says the study is unverified. | No PASS-2 citation uses Hubbard as evidence. | RESIDUAL-NON-BLOCKING |
| SIMD exact/prefilter split | Topic 8 A1 says candidates must not emit tape directly in prefilter mode. | `PASS-2.md:106`, `172`, `578` require scalar parity for exact mode and verifier-before-tape for prefilters. | FOLD-SATISFIED |
| RegexDfa naming | Topic 8 A2 says full DFA cannot be mandatory. | `PASS-2.md:81` documents `RegexDfa` as compatibility spelling for regex-program payload: VM, lazy-DFA, full-DFA, literal prefilter, Unicode-table plans. | FOLD-SATISFIED |
| Regex oracle | Topic 8 A3 warns against unproved bespoke regex. | `PASS-2.md:470` requires `regex-automata` oracle parity until bespoke grammar-HIR/tape-offset integration proves parity. | FOLD-SATISFIED |
| SIMD-first over-selection | Topic 8 A4 warns first-class can become first-chosen. | `PASS-2.md:172` selects SIMD only when target legality, exactness/verifier route, and objective profile beat scalar. | FOLD-SATISFIED |
| Hyperscan false friend | Topic 8 A5 is MASTER/research footnote pressure. | PASS-2 does not adopt a multi-pattern database API. | RESIDUAL-NON-BLOCKING |

### §2.3 Evidence-grep disposition

`rg` over PASS-2 and fold-pass-2 shows:

| Pattern family | Result |
|---|---|
| `CostDecision`, objective vector, Pareto, scalar Cost | Present in PASS-2 at `32`, `56-57`, `93`, `374-376`, `587`. |
| `TapeShape`, `ValueShape`, scalar-cache, traversal policy | Present in PASS-2 at `36`, `54`, `109`, `462`, `587`. |
| `prefilter`, scalar parity, verifier-before-tape | Present in PASS-2 at `81`, `106`, `172`, `470`, `578`, `587`. |
| `regex-automata` | Present in PASS-2 at `81`, `470`; used as oracle/parity route. |
| `@pratt`, `@simd` | Zero PASS-2 hits; only fold report prohibition text. |
| `path!`, `Wave 4` | Zero PASS-2 hits; only fold report's recorded grep command mentions them. |
| `Hubbard`, `Almomany`, exact `Deb 2014` | Zero PASS-2 evidence hits; fold report names them as provenance gaps. |

## §3 Nine-lane verification table

### Lane 1 - Lock-Adherence

Lane standard: PASS-2 must honour the locks it touches without rewriting locks
or smuggling implementation convenience into codegen.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:36` / Lock 1 | Tape/direct is one materialisation plan. | `TapeShape` and `ValueShape` share tape identity and node id; scalar cache is declared, not separate ownership. | The phrase "both shapes" remains easy to misread. | Collapse `ValueShape` into `TapeShape` and remove the second noun. | KEEP: generated projection needs its own name, and the identity sentence defeats the two-tree reading. |
| `PASS-2.md:32`, `200`, `257` / Lock 5 | Backend IR is the codegen boundary. | Lowerers consume BIR/evidence snapshots; deny gate scans all `crates/codegen/src/`. | PASS-2 still cites current source that walks `GrammarIR`. | Allow direct Grammar IR lowerer during transition. | KEEP: the current source citation is a violation target, not permission. |
| `PASS-2.md:399-443` / Locks 6 and 13 | Committed codegen and file-shape budgets are explicit. | Generated LOC table, non-generated LOC table, and wall budgets exist with observed/provisional labels. | Some baselines remain provisional by implementation stage. | Reject the plan until all baseline timings are measured. | KEEP: provisional rows name owner/receiver and do not claim final measurement. |
| `PASS-2.md:172`, `554-555` / Lock 10 | Pratt/SIMD are automatic recognizers. | Diagnostics mention objective evidence, exactness, and disable-only metadata; no force directive exists. | Metadata disable path can be confused with control. | Remove metadata mention entirely. | KEEP: unsupported-kernel disable is a safety valve and the string explicitly says cannot force SIMD. |
| `PASS-2.md:389-395`, `489-502` / Lock 14 | Future grammar proof stays two-surface. | yaml source and metadata are author inputs; runtime/path/visitor/host outputs are generated. | Runtime emission table names seed grammars. | Remove per-grammar rows to avoid overfit. | KEEP: per-X proof is required by hardening; rows are evidence, not plan logic. |

Lane 1 verdict: KEEP. No lock-level PASS-2 blocker survives.

### Lane 2 - Sequencing Discipline

Lane standard: for a single PASS synthesis, this lane is N/A unless the PASS
asserts executable tranche order. PASS-2 has inheritance waves and carries, not
multi-wave implementation order.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:530-541` | Wave-by-wave carries are inheritance, not execution sequencing. | PASS-2 does not claim to execute BC/BD waves. | A reader can mistake carry rows for a tranche schedule. | Move every wave row to MASTER only. | KEEP/N/A: PASS-2 needs inheritance trace; MASTER owns actual sequencing. |
| `PASS-2.md:557-568` | Deferrals carry receiver, blocker, and receiving gate. | Eight rows bind TS, parity, publication, fixtures, path-ts, WASM ABI, PASS-1, and PASS-3 receivers. | Some receivers are broad tranche pairs. | Demand one exact tranche wave in every row. | KEEP: rows name receiving gates with enough specificity for consolidation. |

Lane 2 verdict: N/A for PASS-level sequencing; carry rows are adequate.

### Lane 3 - Cohesion

Lane standard: every PASS-2 claim must cite or produce evidence available to
the target or its named binding surfaces.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:50-81` | The BIR table and regex note define the lowerer payload contract. | `RegexDfa` compatibility note prevents full-DFA mandate and preserves BIR alphabet ownership. | `RegexDfa` remains a misleading name. | Rename to `RegexProgram` now. | KEEP: PASS-2 is payload refiner, not BIR re-owner; rename belongs PASS-1/SYNTHESIS. |
| `PASS-2.md:100-110` | Lowering test gates match PASS-1 variant-family obligations. | Scanner, Pratt/SIMD, tape/value, debug/path gates all have commands. | The commands are future implementation tests, not runnable now. | Strip commands until crates exist. | KEEP: PASS syntheses define gates; V6 audits contract shape, not future crate existence. |
| `PASS-2.md:336-364` | PASS-3 handoffs have consumer acceptance tests. | Parse signatures, visitor/selectors, cost table, path schema, diagnostics, and WASM ABI are command-backed. | PASS-3 can still change public names. | Make PASS-2 own public API. | KEEP: PASS-3 owns user surface; PASS-2 owns emitted evidence. |
| `PASS-2.md:366-383` | PASS-1 handoffs include cost/egraph/regex evidence. | CostDecision, RegexCostSummary, and BridgeJustification are named. | PASS-1 could diverge in exact API. | Freeze PASS-1 API in PASS-2. | KEEP: cross-pass conflict returns to SYNTHESIS, not unilateral PASS-2 edits. |

Lane 3 verdict: KEEP. The fold added evidence rather than orphan claims.

### Lane 4 - SOTA Anchoring

Lane standard: throughput rows cite competitor, dataset, platform, target, and
evidence. Mechanism rows do not pretend to be Lock-8 wins.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:448-462` | SOTA table binds JSON/CSS/simdjson gates to competitor numbers and metadata. | Rows name sonic-rs, simd-json, lightning-css, simdjson OD, datasets, M1 Pro/x86 targets, and bench commands. | Benchmark numbers come from local SOTA summaries, not direct fresh competitor runs. | Require direct benchmark rerun before plan readiness. | KEEP: PASS-2 states trajectory and metadata floor; implementation tranches measure. |
| `PASS-2.md:464-473` | Mechanism rows are separate from throughput claims. | OpenFrame deletion, Pratt auto-detection, regex oracle parity, and WASM parity are mechanism gates. | Mechanism gates can be overread as performance wins. | Delete mechanism table from performance section. | KEEP: explicit separation prevents Lock-8 inflation. |
| `PASS-2.md:462` | Benchmark metadata records validation/source ownership. | It handles Topic 6 in-situ and UTF-8 adversarial findings. | Metadata floor is broad and not a concrete schema file. | Require a TOML schema now. | KEEP: PASS-2 owns field floor; MASTER/bench implementation owns schema file. |

Lane 4 verdict: KEEP with implementation measurement routed downstream.

### Lane 5 - Grammar-Authoritative Discipline

Lane standard: no grammar-specific logic in generic crates; yaml proves future
grammar admission through source plus metadata only.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:385-395` | yaml onboarding smoke names two author inputs and generated derivatives. | Rejects manual Rust registry edits, handwritten yaml runtime files, fixture-only admission, and yaml declaration crate. | The runtime generated subdir exists as a path in the proof. | Treat any generated path as a third surface. | KEEP: generated committed output is derivative under Lock 6, not author input. |
| `PASS-2.md:487-502` | Runtime emission table lists ten grammars to prove generic template output. | Every per-grammar file is generated or data-only. | Seed grammar rows can train overfit thinking. | Replace with an abstract "all grammars" paragraph. | KEEP: hardening requires per-X tables; abstraction would weaken Lock 14 proof. |
| Gate scan | `@pratt`, `@simd`, `path!`, and Wave-4 stale wording do not appear in PASS-2. | V5/V5.1 directive pathology stays closed. | MASTER still has one rejected-scope row containing `@pratt`/`@simd`. | Treat any corpus hit as PASS-2 failure. | KEEP: MASTER row is an explicit rejected surface; PASS-2 is clean. |

Lane 5 verdict: KEEP. No PASS-2 genericity blocker.

### Lane 6 - Generated-Code and LOC Budget

Lane standard: generated growth, non-generated file size, child count, and regen
wall budgets must be auditable from PASS-2 or carried to MASTER.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:399-415` | Per-grammar generated LOC table has current, max, disposition, wall ceiling, and baseline. | 9 seed grammars plus yaml smoke have rows; total ceiling is explicit. | yaml row is provisional. | Remove yaml until measured. | KEEP: yaml is a future-grammar proof row with owner/receiver. |
| `PASS-2.md:417` | Generic monomorphisation budget gate prevents infinite generated output. | Lowerers cannot discover new generic shapes during emission. | Relies on PASS-1 validation to supply the set. | Let lowerers instantiate lazily. | KEEP: lazy discovery in codegen would violate BIR boundary and budget discipline. |
| `PASS-2.md:421-433` | Non-generated areas have LOC and child-count commands. | Covers `ir/backend_ir`, `codegen/lower`, runtime template, runtime, host, and xtask regen. | Commands are prose gates until crates exist. | Omit future paths. | KEEP: restart plan must state close gates before implementation. |
| `PASS-2.md:435-444` | Regen wall rows distinguish observed vs provisional. | Current content-equality write is kept; BIR/yaml rows name owners. | Some post-conditions remain target-dependent. | Defer all wall budgets to implementation. | KEEP: provisional with owner is better than silent budget. |

Lane 6 verdict: KEEP.

### Lane 7 - Friction Forecast

Lane standard: where users or implementers hit a confusing surface, diagnostics
or cookbook receivers must exist.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:544-555` | Diagnostic ledger includes codegen and recognizer messages. | `BBNF-OPT001`/`002` now explain objective profile, fallback, exactness, verifier-first route, and no force-SIMD. | PASS-3 has richer user-facing strings. | Delete PASS-2 strings and rely on PASS-3. | KEEP: codegen needs routing diagnostics; PASS-3 owns public polish. |
| `PASS-2.md:112`, `568` | WASM host primitive route stays ABI/lowerer, not grammar annotation. | It names exported functions, host-call shape, marshalling descriptors, scalar/SIMD parity, and packaging receiver. | No worked WASM host fixture appears in PASS-2. | Add a full example here. | KEEP: PASS-2 carries ABI evidence; PASS-3/MASTER cookbook owns worked user flow. |
| `PASS-2.md:462` | Benchmark metadata explains validation/source ownership traps. | It prevents unfair in-situ and non-validating comparisons. | Users may not read benchmark metadata prose. | Add runtime warnings in PASS-2. | KEEP: runtime warning strings belong PASS-3/bench harness. |

Lane 7 verdict: KEEP. No retired-syntax friction remains.

### Lane 8 - Carry and Deferral Audit

Lane standard: every deferral names receiver, blocker, and receiving gate.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:557-568` | Carry ledger is explicit. | Each row has item, receiver, blocker, receiving gate. | One row says SYNTHESIS Wave-2 and Tranche E; broad but bounded. | Require exactly one receiver. | KEEP: cross-pass reconciliation can legitimately have a synthesis receiver plus implementation gate. |
| `PASS-2.md:415` | Generated LOC table carry pointer names SYNTHESIS surfaces. | Prevents PASS-2 budget table from becoming orphan evidence. | The cited consolidated section is historical. | Remove hardening reference after fold. | KEEP-WITH-ROUTE: not PASS-2-blocking; future line-stability polish can cite sections. |
| Fold residuals | INDEX/source cleanups are deferred. | Fold-pass-2 classifies them honestly, with no PASS-2 citation contamination. | They remain real research hygiene. | Treat all provenance gaps as PASS-2 blockers. | KEEP: PASS-2 does not cite the gaps as evidence. |

Lane 8 verdict: KEEP with non-blocking research-index residue.

### Lane 9 - Greenfield Discipline

Lane standard: PASS-2 must replace old codegen/runtime substrate faults rather
than preserve compatibility layers or direct Grammar IR paths.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:5-7` | PASS-2 is replacement, not patch. | It discards ParseStream, grammar-level Unicode sets, rewrite-mode walker, OpenFrame checkpointing, and direct Grammar IR consumers. | Strong replacement posture increases migration work. | Keep compatibility shims for speed. | KEEP: greenfield discipline rejects shims that preserve the old failure mode. |
| `PASS-2.md:468`, `575` | OpenFrame appears as deletion pathology only. | TapeBuilder checkpoints plus BIR builder frame replace clone-stack rollback. | Agent report history still contains an old "useful" framing with a correction note. | Ban the word from all reports. | KEEP: current PASS-2 text is deletion-only; history is classified. |
| `PASS-2.md:585-589` | Closing posture binds research fold without widening architecture. | It summarizes CostDecision, TapeShape/ValueShape, RegexDfa, exact/prefilter SIMD, and benchmark metadata. | The summary is dense. | Split into more implementation steps. | KEEP: PASS-2 punch list already carries implementation steps. |

Lane 9 verdict: KEEP.

## §4 Sixteen-command gate-rerun results

### §4.1 Minimum commands

| Command | Result |
|---|---|
| `git status --short` | Clean before report creation. |
| `wc -l restart/audit/pass-2-codegen/PASS-2.md restart/research/fold-pass-2.md restart/research/topic-{3,4,5,6,8}-*.md` | Completed; counts recorded in §1. |
| Required research/PASS-2 `rg` command | Completed; matches classified in §2.3 and §4.3. |
| `git diff --check` | Clean before report creation. |
| `git diff --cached --check` | Clean before report creation. |

### §4.2 Tightened 16-command checklist

| # | Command family | PASS-2 result | Classification |
|---:|---|---|---|
| 1 | `ParseStream|rewrite-mode|Unicode class algebra` | PASS-2 hits are conflict/deletion rows at `7`, `13-15`, `470`, `580`, `589`; no positive surface. | PASS |
| 2 | `bbnf-path|bbnf-test-fixtures|path!` | PASS-2 target is N/A; PASS-3 legacy citations remain classified, current authored macro is `pointer!`. | N/A-PASS-2 |
| 3 | `codegen/src/backend_ir` | PASS-2 hits at `200`, `237`, `252` all say no ownership path or doc-only exception. | PASS |
| 4 | `fixtures/yaml` | PASS-2 has zero hits; ARCH/PASS-3 hits are parity-phase prose and explicit non-onboarding text. | PASS |
| 5 | `@recover` | PASS-2 target is N/A; ARCH/PASS-3 classify standalone `@recover` as alias-only or folded into `@error(recover = ...)`. | N/A-PASS-2 |
| 6 | `OpenFrame` | PASS-2 hits at `7`, `36`, `93`, `468`, `575` are deletion pathology or replacement wording. | PASS |
| 7 | `GrammarIR` | PASS-2 hits at `5`, `253-254` cite old source violation and the deny command. | PASS |
| 8 | `__EAGER_EMPTY_PATH|CursorDecision::Skip` | PASS-2 has its own Lock 3 ratification at `180`; documented command targets MASTER/MIGRATION and finds gates. | PASS |
| 9 | SOTA workload names | PASS-2 own SOTA rows at `454-460`; documented command targets MASTER/PASS-3 and finds numeric rows. | PASS |
| 10 | `receiver|blocker|receiving gate` | PASS-2 has exact capitalized header at `559`; documented lowercase regex is soft and misses some table headers. | PASS-WITH-HARNESS-RESIDUAL |
| 11 | `yaml.bbnf|workspace.metadata.bbnf.grammars.yaml` | PASS-2 proof at `389-395`, ARCH/MASTER/PASS-3 mirrors. | PASS |
| 12 | `generated_loc|regen_wall|xtask` | PASS-2 budget and wall rows at `399-444`, plus downstream generated API refs. | PASS |
| 13 | Diagnostic patterns | PASS-2 ledger at `548-555`; ARCH/PASS-3 ledger rows present. | PASS |
| 14 | `child count|500 LOC|exception rationale` | PASS-2 owns local rows at `421-433`; documented command targets ARCH/MASTER and finds Lock 13 tables. | PASS |
| 15 | declaration-crate review form | PASS-2 route at `302`; documented command targets ARCH/MIGRATION and finds review/deletion/reviewer fields. | PASS |
| 16 | benchmark metadata fields | PASS-2 owns full metadata floor at `462`; documented command omits ARCH and PASS-2, so exact command is soft. | PASS-WITH-HARNESS-RESIDUAL |

### §4.3 Additional V6 research-fold gates

| Gate | Evidence | Classification |
|---|---|---|
| CostDecision/objective vector | `PASS-2.md:32`, `56-57`, `93`, `374-376`, `587`. | PASS |
| Pareto/scalar fast path | `PASS-2.md:374-376` records dominated/Pareto evidence and scalar Cost as fast path only. | PASS |
| E-graph/cost extraction | `PASS-2.md:378` names `BridgeJustification`; `PASS-1.md:77-79` names stable ids and `CostDecision`. | PASS |
| TapeShape/ValueShape single identity | `PASS-2.md:36`, `54`, `109`, `587`. | PASS |
| Exact vs prefilter SIMD | `PASS-2.md:106`, `172`, `578`; `PASS-3.md:462` mirrors runtime rule. | PASS |
| Regex-automata oracle boundary | `PASS-2.md:81`, `470`; MASTER regex oracle row at `MASTER-PLAN.md:777`. | PASS |
| Validation/source-ownership metadata | `PASS-2.md:462`; ARCH metadata at `1304-1320`; MASTER Lock 8 row at `706`. | PASS |
| No retired recognizer surfaces | zero `@pratt`/`@simd` in PASS-2; MASTER row `204` is rejected-scope only. | PASS |

## §5 F/G/H pathology regression scan

Lens source: V5 found LLM-shaped defects in three classes: F directive/pseudo
precision bias, G overfit from familiar patterns, and H wrong-line or source
provenance drift.

| Lens | Site | V5/V6 pathology check | Result |
|---|---|---|---|
| F1 directive completion bias | `PASS-2.md:554-555` | No `@pratt`; no `@simd`; no force-SIMD hint. | PASS |
| F2 force-control bias | `PASS-2.md:555` | Text says metadata may disable unsupported kernels but cannot force SIMD. | PASS |
| F3 scalar-cost confidence | `PASS-2.md:374-376` | Scalar Cost survives only as fast extraction path when full evidence record survives. | PASS |
| F4 full-DFA confidence | `PASS-2.md:81` | Full DFA is an implementation plan among VM/lazy-DFA/full-DFA/prefilter choices. | PASS |
| F5 benchmark certainty | `PASS-2.md:473` | PASS-2 says it should not claim final perf wins until generated parsers run corpus. | PASS |
| G1 table-overfit onboarding | `PASS-2.md:389-395` | yaml proof distinguishes author inputs from generated output and rejects fixture-only admission. | PASS |
| G2 recognizer overfit | `PASS-2.md:172`, `554-555` | Pratt/SIMD selection is automatic, target/profile/exactness-sensitive. | PASS |
| G3 WASM parity overfit | `PASS-2.md:112`, `568` | WASM host route names ABI descriptor/parity evidence without invented numbers. | PASS |
| G4 regex-engine overfit | `PASS-2.md:81`, `470` | Bespoke regex remains under `regex-automata` oracle parity until delta is proven. | PASS |
| G5 path macro legacy | PASS-2 target | PASS-2 contains no `path!`; PASS-3 handles legacy citations. | N/A-PASS-2 |
| H1 stale PASS-local citations | `PASS-2.md:92`, `112`, `359` historical V5.1 residue | V5.1A removed brittle line ranges; current text uses section ownership. | PASS |
| H2 provenance gaps | `PASS-2.md` plus fold report | Hubbard/Almomany/Deb are not PASS-2 evidence; fold report marks gaps. | PASS |
| H3 command-harness softness | Gate #10/#16 | Documented rerun regexes are soft/case-sensitive or target-incomplete. | RESIDUAL-NON-BLOCKING |
| H4 local source overclaim | `PASS-2.md:83` | swc is not used as cardinality proof; local corpus proxy is named and limited. | PASS |

Pathology verdict: V5's PASS-2 blocker did not return. The only H-class
residue is gate-harness precision, not target prose.

## §6 Cross-document binding ledger

| Topic | PASS-2 binding | PASS-1 / PASS-3 binding | SYNTHESIS binding | V6 status |
|---|---|---|---|---|
| Backend IR ownership | `PASS-2.md:32`, `85-98`, `200`, `257` | `PASS-1.md:41-57` | ARCH diagnostic `BBNF-GEN001` at `1054`; MASTER Lock 5 at `703`. | COHERENT |
| Grammar IR import deny | `PASS-2.md:239-257` | PASS-1 says only BIR producer may import Grammar IR at `43`. | MIGRATION boundary test at `730-736`. | COHERENT |
| CostDecision | `PASS-2.md:374-376` | `PASS-1.md:79` records objective vectors, selected/rejected/dominated, scalarization profile. | MASTER carry row `776` requires selected/rejected/dominated/objective-mode provenance. | COHERENT |
| BridgeJustification | `PASS-2.md:378` | `PASS-1.md:77` rejects e-node representative commitment. | MASTER Lock 4 close proof `702`. | COHERENT |
| TapeShape/ValueShape | `PASS-2.md:36`, `54`, `109` | PASS-1 tape/direct row `54`; PASS-3 diagnostics expose ValueShape cause at `460`. | MIGRATION runtime substrate expected result `751-753`. | COHERENT |
| OpenFrame deletion | `PASS-2.md:468`, `575` | PASS-1 positive replacement `59`, deletion close `298`. | MASTER Lock 1 `699`; MIGRATION `749`. | COHERENT |
| Regex program boundary | `PASS-2.md:81`, `470` | PASS-1 `parse-that` regex ownership `157`; PASS-3 `BBNF-OPT002` verifier string `444`. | MASTER regex oracle row `777`. | COHERENT |
| SIMD exact/prefilter route | `PASS-2.md:106`, `172`, `578` | PASS-3 runtime rule `462`. | MASTER Lock 10 `708`. | COHERENT |
| Benchmark metadata | `PASS-2.md:462` | PASS-3 SOTA rows `492-497`. | ARCH `1304-1320`; MASTER `706`, `759`. | COHERENT |
| yaml onboarding | `PASS-2.md:389-395` | PASS-1 `239-243`; PASS-3 `407-431`. | ARCH `1336-1400`; MASTER `215-220`, `779`, `805`. | COHERENT |
| Diagnostics | `PASS-2.md:548-555` | PASS-1 strings `101-113`; PASS-3 strings `439-458`. | ARCH catalogue `1027-1058`; MASTER friction rows `797-806`. | COHERENT |
| Runtime template schema | `PASS-2.md:134-155`, `336-364` | PASS-3 consumer acceptance uses emitted metadata and diagnostics. | MASTER carry row `774`; MIGRATION generated equality `755-763`. | COHERENT |
| Generated LOC | `PASS-2.md:399-443` | PASS-3 generated API budget references PASS-2 anchors at `502-512`. | MASTER generated budget `654-656`, Lock 13 `714-724`. | COHERENT |
| Carry ledger | `PASS-2.md:557-568` | PASS-1 handoff tables `170-188`; PASS-3 carry rows `532-580`. | MASTER single ledger `762-791`; MIGRATION `790-801`. | COHERENT |

## §7 Punch list

### §7.1 PASS-2-blocking punch list

None.

PASS-2 has no V6 surgery required before consolidation. Its research-fold
amendments are coherent with the target and with the sibling bindings.

### §7.2 Residual non-blocking items

| # | Path:line | Surgery | Acceptance gate | Origin |
|---:|---|---|---|---|
| R1 | `restart/research/INDEX.md` source rows for Topics 3/4/5/6/7 | Mark Hubbard, Almomany, exact Deb 2014, Yang/egglog, Roc, Ungar/Adams, and HelpMate gaps as verified or provenance-gap entries. | `rg -n "\b(Hubbard|Almomany|Deb 2014|Yang et al\. 2024|Roc|Ungar|HelpMate)\b" restart/research/INDEX.md` classifies every row as verified primary source or explicit gap. | Topic §6 adversarial findings; fold-pass-2 §3. |
| R2 | `restart/audit/hardening/HARDENING-CONSOLIDATED.md:560-575` gate checklist | Tighten command #10 to `Receiver|receiver|Blocker|blocker|Receiving gate|receiving gate`, or make it case-insensitive. | Rerun command finds PASS-1/PASS-2/PASS-3 and MASTER carry table headers, not only lowercase prose. | V6 gate rerun. |
| R3 | `restart/audit/hardening/HARDENING-CONSOLIDATED.md:575` gate checklist | Add `restart/ARCHITECTURE.md` and `restart/audit/pass-2-codegen/PASS-2.md` to command #16, or split PASS-2 metadata floor from MASTER metadata gate. | Metadata scan finds CPU/OS/compiler/input/competitor/bbnf/warmup/sample plus validation/source-ownership/materialisation fields. | V6 gate rerun; Reviewer D §6 tightening. |
| R4 | `restart/ARCHITECTURE.md` / `restart/MASTER-PLAN.md` lock rationale | Add egglog-aware rationale to Lock 4 explanation if future synthesis edits lock rationale. | Lock 4 prose names egglog-style fusion as known SOTA and explains why V1 keeps output-piped crates. | Topic 4 A2; fold-pass-2 DEFER. |
| R5 | `restart/MASTER-PLAN.md` rewrite-budget gates | Ensure C.W4/C.W5 rewrite-budget tests cover all seven rewrite categories with node/iteration limits. | `cargo test -p passes egraph_budget` names one adversarial grammar per rewrite category. | Topic 4 A3; fold-pass-2 DEFER. |

These residuals are real, but none requires PASS-2 amendment. R1 is research
index hygiene; R2/R3 are hardening harness precision; R4/R5 are lock/MASTER
rationale and implementation-gate detail.

## §8 V5/V5.1-to-V6 history note

V4 baseline:

`HARDENING-PASS-2-V4.md` returned READY after Wave 4.1. It verified the V3
punch list and treated the then-new `@pratt`/`@simd` diagnostic text as a hint
surface rather than a forbidden directive surface.

V5 reopening:

`HARDENING-PASS-2-V5.md` correctly reopened PASS-2 because `BBNF-OPT001` and
`BBNF-OPT002` taught recognizer directives that Lock 10 rejects. V5 also found
stale PASS-2/generated LOC citations and missing worked-example routing.

V5.1 closure:

The V5.1 PASS-1/PASS-2 amendment removed the substantive recognizer syntax
fault. `PASS-2.md:554-555` now frames recognizer non-selection through objective
profile, fallback, exactness, and no force-SIMD semantics.

V5.1A closure:

V5.1A removed the shifted PASS-local line citations that remained after the
rare-fence insertion. The current PASS-2 cross-PASS diagnostic ownership text
uses section/table ownership rather than brittle line ranges.

V6 delta:

V6 does not reopen V5's architecture. It audits whether research-folded
concepts added new confidence drift. They did not. The new cost/egraph/tape/
SIMD/regex benchmark metadata text is evidence-preserving and scoped to PASS-2.

## §9 Final verdict

Verdict: **READY**.

Decision basis:

| Criterion | Result |
|---|---|
| Lock adherence | PASS-2 honours Locks 1, 4, 5, 6, 8, 10, 13, and 14 within its scope. |
| Research fold | Topics 3/4/5/6/8 PASS-2 folds are present and coherent; Topics 1/2/7 remain sibling-routed. |
| Retired syntax | `@pratt`, `@simd`, `path!`, and stale Wave-4 wording are absent from PASS-2. |
| Regex/SIMD safety | Full DFA is not mandatory; exact scans prove scalar parity; prefilters verify before tape. |
| Benchmark fairness | PASS-2 records validation, source ownership, materialisation, scalar-cache, parse-entry, competitor-flag, and input-hash metadata. |
| F/G/H regression | V5 directive and citation pathologies do not recur on PASS-2. |
| Gate rerun | 16-command subset passes for PASS-2; two soft checklist harness issues are non-blocking residuals. |
| Punch list | No PASS-2-blocking item. |

Re-draft threshold:

No source contradicts a settled lock structurally. No research §6 adversarial
finding requires a PASS-2 re-draft. No lock amendment is required.

Amendment threshold:

Not met for PASS-2. The remaining residue belongs to research-index hygiene,
lock/MASTER rationale, or hardening-command post-condition precision.

## §10 Closing posture

PASS-2 is ready for V6 consolidation.

The middle layer now carries the right constraints: BIR-only lowerers,
`CostDecision` evidence with objective vectors and Pareto residue, one
TapeShape/ValueShape identity, regex-program compatibility under
`RegexDfa`, exact/prefilter SIMD correctness, `regex-automata` oracle parity,
and benchmark metadata that can survive SOTA scrutiny.

Hereupon the consolidator should treat PASS-2 as READY, route the non-blocking
research-index and gate-harness residue outside PASS-2, and avoid a redundant
PASS-2 amendment pass.
