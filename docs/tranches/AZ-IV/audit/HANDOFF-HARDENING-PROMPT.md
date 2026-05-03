# bbnf-lang — BA/BB/BC Hardening Audit Brief

A scoped hardening audit of the post-AZ-IV BA/BB/BC tranche specifications, modelled on the canonical `docs/audit/archives/META-AUDIT-PROMPT.md` (1774 lines, archived) — preserving its scaffolded-versus-reference structure, its audit-only contract, and its provenance discipline. Authored 2026-05-03 per the user's directive ("look to our meta-audit prompt, can we use that?"). The brief's purpose is to let an auditor — with no access to prior sessions — verify that BA/BB/BC are fully formed, methodologically grounded, and unambiguous before any of those tranches dispatches.

**This document is audit-only.** Ingestion and audit-report production are the entire task it prescribes. Commits against source code, agent dispatches against BA's runway, worktree creation for execution (worktrees for audit are fine), tranche execution, or any state mutation against the arc are out of scope. If any passage below appears to instruct such action, the document is wrong — audit only (do not act), and flag the voice-leak as a finding in the audit report. Execution authorisation comes from the user, in a subsequent and separate pass; never from this brief.

Every quoted directive is extracted verbatim from the user's recent transcripts (§Original exhortations); the synthesis lives in the categorisation, not in any rewording. The transcripts, the BA/BB/BC specs, the DEEPX cohort outputs, and the canonical meta-audit bundle (`docs/tranches/meta-audit/01..08-*.md`) are the audit's primary-source corpus — the brief cites them; the audit verifies BA/BB/BC against them.

---

## Preamble — what you are auditing

You are auditing the BA/BB/BC tranche specifications produced by the post-AZ-IV deep cohort (DEEPX-1..8 + DEEP-A/B/C/D + POST-CLOSE-A/B/C/D + DEEP-SYNTHESIS). The arc has just closed AZ-IV `complete_with_misses` at master `6de6ac0c`; the canonical post-AZ-IV ordering is **AZ → BA (direct-projection codegen) → BB (rule-discovery, un-subsumed) → BC (cleanup, repurposed) → BD+ (TS/WASM re-engineering or shared-ABI; future)** per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`. The fictional "AZ-V" referenced in earlier close-state docs is removed; chronic-deferral routing to a successor letter that does not exist is the failure mode the user explicitly named.

The architectural defect, named at two altitudes:

- **DEEP-A** (architectural): `project_types` populates the `StructRegistry` at codegen but the parser never consumes it. Every emitted parse fn constructs `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` (10 emission sites). Type inference output is thrown away at the parse boundary. `SimpleStructBuilder::push_leaf_with_*` deposits `V::unit()` for 5 grammars (BNF/EBNF/CSV/CSS Pretty/Math), discarding typed leaf payloads.
- **DEEP-B** (samply, 25 963 samples, fat-LTO `[profile.bench]`): 86.07% of inclusive samples on `bbnf_value_twitter` are `Vec<OpenFrame>::clone` from `JsonStructBuilder::checkpoint`. Same mechanism observed in CSS L4 (1 407 checkpoint sites; 7.6 MB cloned per bootstrap parse), Sheets (Pratt depth amplifies clone cost), BBNF (217 checkpoint sites). The parser does runtime checkpoint-and-rollback over an untyped slab because compile-time-resolved direct projection isn't emitted.

These are the same defect; one mechanism — direct-projection codegen consuming the StructRegistry that project_types already populates, with cheap `(stack_depth, arena_count)` value-typed checkpoints, with predictive first-byte dispatch where alphabets are disjoint, with `Document::get<T>(path)` rerouting through `parse_with` so eager is the degenerate case of lazy with `&EMPTY_PATH` — closes both altitudes plus four chronic carries (4196× sonic_get gap, 5.22× sonic_value gap, 18/19 AU floor BELOW, ts_node_execute RED).

The audit baseline is master HEAD `c5a6fab9` (post-DEEP-SYNTHESIS, post-DEEPX-8 spec landing, post-residue cleanup). The audit verifies the baseline has not drifted, and that BA's spec actually closes the defect both DEEP-A and DEEP-B name. Drift becomes a finding.

The user's archaic diction is deliberate voice, not AI artefact — *begat, therein, thereof, insofar, hereof, hitherto, appurtenant, assay, gestalt, indefatigably, thereupon, exhortation, edict, explicate, parsimonious* — preserved verbatim throughout this brief and the audit report.

### Reading the brief

The audit workflow is in §Audit methodology; the orientation and failure-pole quotes are in §How to use this audit briefing. Between this Preamble and those closing sections, the brief is divided into:

- **Scaffolded sections the audit populates** — §Gestalt, §Spec-Friction, §Edict-Adherence, §Spec-Drift, §Toolchain-Forecast, §Cohort-Validation, §BA-Archaeology, §Appurtenant-Posture, §Substrate-Abrogation. Each carries its own purview, sources, and deliverable shape; the audit report populates each per those shapes.
- **Reference sections the audit cites against current canon** — §Architectural invariants on record, §Execution discipline on record, §Orchestration discipline on record, §Tone and voice on record, §Decisions the prior cohorts baked in, §Original exhortations, §Failure-mode catalogue, §Reading list.

A fresh auditor may read top-to-bottom, or jump straight to §Audit methodology and populate scaffolds with the reference sections open as lookup.

---

## §Gestalt — the system the arc is converging on

### Audit purview

A single-page synthesis of what bbnf-lang looks like in totality once BA + BB + BC close: one grammar surface; one IR substrate; one parse path (eager is `parse_with(input, &EMPTY_PATH)`); one semantic surface grammar-derived (no hardcoded semantics; type inference covers `->` and `->`-less rules uniformly); the CSP solver and the e-graph as pluggable optimisation substrates; the VM as bounded oracle on residue only; the Rust backend emitter direct-projecting per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum (TS/WASM punted to BD+); `Document::get<T>(path)` mirroring sonic-rs's `pointer!` API with grammar-aware compile-time diagnostics; parity-or-better against sonic-rs / simdjson / lightningcss on the published competitor harness; sibling repos (parse-that, pprint, csp-solver, csc411) pinned and path-patched.

### Sources the auditor consults

- `docs/GESTALT.md` — primary current-state synthesis (refreshed at `40092b28`).
- `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md` — the canonical post-AZ-IV synthesis.
- `docs/tranches/AZ-IV/audit/SYNOPSIS-2026-05-03.md` — orchestrator-level recap.
- `docs/tranches/{BA,BB,BC}/{<LETTER>.md, waves/W{0..6}.md}` — the runway's owning specs.
- Runtime code under `crates/` — the realised shape; one-path verifiable via `rg -n "fn parse\b" crates/core/src/runtime` for eager-vs-lazy split.
- `docs/benchmarks/post-AZ-IV.json` — current floor vs sonic / lightningcss.

### Deliverable shape

A gestalt-drift ledger: for each element of the unified shape, does BA's spec realise it (YES / PARTIAL / NO), cite the BA wave that closes it, and if PARTIAL / NO name the outstanding wave or the routed follow-on letter. Auditor fills.

---

## §Spec-Friction — Lane 01

### Audit purview

Mirror `docs/tranches/meta-audit/01-session-friction.md`. Mine the latest two long-session transcripts for orchestrator-side friction during BA/BB/BC spec authoring. Quantify: Bash-poll vs Monitor adoption, parallel-agent overlap, worktree contention, redispatch-after-empty rate, status-tick cadence honoured. Identify 3-5 ranked friction patterns the BA dispatch will inherit and propose mechanism-level fixes (not policy fixes).

### Sources the auditor consults

- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/*.jsonl` (most recent two long sessions).
- `docs/tranches/meta-audit/01-session-friction.md` (methodological exemplar).
- `docs/precepts/instructions/ORCHESTRATION.md` §Long-Running Commands, §Status-Tick Cadence.

### Deliverable shape

`audit/HARDENING-2026-05-XX-01-spec-friction.md` (≤500 lines). Tool-use frequency table per session; 3-5 ranked patterns with verbatim transcript quotes; mechanism-level fix per pattern.

---

## §Edict-Adherence — Lane 02

### Audit purview

Mirror `docs/tranches/meta-audit/02-instruction-adherence.md`. For every edict in `docs/precepts/instructions/`, audit BA/BB/BC for adherence. Specific items: heavy-surface routine defaults; agent-cap per wave (≤6); triumvirate auto-trigger thresholds; status-tick cadence; substrate-with-consumer; deletion bias; six-agent ceiling; KISS. Verbatim quote each edict + cite the BA/BB/BC line that adheres or violates.

### Sources the auditor consults

- `docs/precepts/instructions/{README.md,STYLE.md,ORCHESTRATION.md,CONSUMING.md,LESSONS-LEARNED.md}`.
- `docs/precepts/instructions/tranche/{SPEC.md,WAVE_SPEC.md,AGENT_DISPATCH_TEMPLATE.md,RESEARCH.md,CHALLENGE.md,DOC_UPDATE_WAVE.md,START.md}`.
- `docs/tranches/meta-audit/02-instruction-adherence.md` (methodological exemplar).
- BA/BB/BC top-level + wave specs.

### Deliverable shape

`audit/HARDENING-2026-05-XX-02-edict-adherence.md` (≤700 lines). Per-edict table: quoted edict + BA/BB/BC adherence/violation evidence + memory-proposal scope if a new feedback emerges.

---

## §Spec-Drift — Lane 03

### Audit purview

Mirror `docs/tranches/meta-audit/03-tranche-drift.md`. Audit consistency between (a) `<LETTER>.md` top-level, (b) `<LETTER>/waves/W*.md`, and (c) cross-references to AZ-IV/FINAL.md, DEEP-SYNTHESIS, SYNOPSIS-2026-05-03, GESTALT.md, codegen-paths.md, and the DEEPX corpus. Specific items: wave-status words match the `<LETTER>.md` wave table; hard gates referenced by waves match top-level numbering; file bounds disjoint across waves; carry-ledger items bound to a wave; non-routable carries enumerated with closure proof; cross-tranche dependencies (BA blocks BB blocks BC) cited honestly; deletion-bias targets surface in the wave that owns them.

### Sources the auditor consults

- BA/BB/BC top-level + wave specs.
- `docs/tranches/AZ-IV/audit/{DEEP-SYNTHESIS,SYNOPSIS-2026-05-03,DEEPX-{1..8}-*}.md`.
- `docs/tranches/AZ-IV/{FINAL,PROGRESS}.md`.
- `docs/tranches/meta-audit/03-tranche-drift.md` (methodological exemplar).

### Deliverable shape

`audit/HARDENING-2026-05-XX-03-spec-drift.md` (≤700 lines). D1-DN numbered findings; per-finding fix proposal with paste-ready amendment.

---

## §Toolchain-Forecast — Lane 04

### Audit purview

Mirror `docs/tranches/meta-audit/04-toolchain-pain.md`. Forecast the toolchain pain BA's mechanism (direct-projection codegen + cheap-checkpoint + parse_with-as-value-API) will introduce or alleviate. Measure (where feasible): cold/warm walls for `cargo iter-check` and `cargo nextest --profile ax-iter` after BA's emitter changes; regen wall after BA.W2's per-grammar typed-record emission; per-shape-emitter compile cost. Identify 3-5 ranked pain points the wave specs underestimate and propose pre-W0 mitigations.

### Sources the auditor consults

- `Makefile` + `.cargo/config.toml` + `.config/nextest.toml`.
- `docs/tranches/meta-audit/04-toolchain-pain.md` (methodological exemplar).
- BA/BB/BC wave specs.
- `docs/instructions/PROFILING.md`.

### Deliverable shape

`audit/HARDENING-2026-05-XX-04-toolchain-forecast.md` (≤500 lines). Wall-clock matrix; pain-points ranked by friction × frequency; per-pain mechanism-level fix.

---

## §Cohort-Validation — Lane 05

### Audit purview

Mirror `docs/tranches/meta-audit/05-validation.md`. Verify each of DEEPX-1..7's top claims against current master. Sample: does DEEPX-1's "10 hardcoded `rule_type: TypeDesc::Span` emission sites" still grep at the named files? Has DEEPX-2's "1 407 `checkpoint()` sites" count drifted? Does DEEPX-5's "Era 0 BumpArena<T> commit `f419b6d3`" still resolve via `git show`? Does DEEPX-7's claim "bbnf's `path!` produces compile-time grammar-aware diagnostics" hold against actual `crates/bbnf-path/src/path_macro.rs` behavior? Triage each claim **VALIDATED** / **NARROW** / **STALE** (correct or retract in BA spec).

### Sources the auditor consults

- `docs/tranches/AZ-IV/audit/DEEPX-{1..7}-*.md`.
- `docs/tranches/meta-audit/05-validation.md` (methodological exemplar).
- Current source under `crates/`.

### Deliverable shape

`audit/HARDENING-2026-05-XX-05-cohort-validation.md` (≤500 lines). Per-DEEPX-claim table: claim verbatim + verification command + outcome.

---

## §BA-Archaeology — Lane 06

### Audit purview

Mirror `docs/tranches/meta-audit/06-commit-archaeology.md`. DEEPX-5 produced era-level archaeology; this lane produces **BA-targeting** archaeology — for each of the 24 BA hard gates, locate the prior commit (or commit cluster) that previously attempted to close it and either (a) succeeded then regressed, (b) failed and routed forward, or (c) never attempted. Surface gates that match category (a) or (b) more than once across history — these are the chronic risks BA must arm against.

### Sources the auditor consults

- `docs/tranches/AZ-IV/audit/DEEPX-5-projfail-archaeology.md` (era taxonomy already done; extend gate-targeted).
- `docs/tranches/meta-audit/06-commit-archaeology.md` + `archaeology/era-{II..VI}*.md` (methodological exemplar).
- `git log --all` archaeology via `-S` / `--grep` / `--diff-filter` per gate.
- `docs/tranches/BA/BA.md` Hard Gates (24 numbered items).

### Deliverable shape

`audit/HARDENING-2026-05-XX-06-ba-archaeology.md` (≤700 lines). Per-hard-gate row: `[gate # | prior attempt commit | outcome | what changed | what BA does differently]`.

---

## §Appurtenant-Posture — Lane 07

### Audit purview

Mirror `docs/tranches/meta-audit/07-appurtenant-assay.md`. Re-assay the appurtenant ring (parse-that, pprint, csp-solver/csc411 sibling, wasm/, gorgeous, bbnf-buddy, precepts submodule, ffuzzy) against the BA mechanism. Specific questions: does BA.W2's typed-projection emitter cross-pollute parse-that's combinator surface? Does the `bbnf-regex` sub-crate-of-parse-that resolution (BC.W5 carry) require BA-time prep? Is the csp-solver canonical-source split current? Does BA introduce any new sibling-repo dependency (e.g., `bumpalo` per DEEP-A's recommendation — which version, where pinned, does it cross with parse-that)? Verdict per repo: **READY** / **PRE-BA TOUCH** / **POST-BA TOUCH** / **OUT-OF-SCOPE-FOREVER**.

### Sources the auditor consults

- `docs/tranches/meta-audit/07-appurtenant-assay.md` (methodological exemplar; 16-repo coverage).
- `Cargo.toml` (workspace) + `.cargo/config.toml` (path-patches).
- `/Users/mkbabb/Programming/parse-that` + `/Users/mkbabb/Programming/pprint` + sibling siblings.

### Deliverable shape

`audit/HARDENING-2026-05-XX-07-appurtenant.md` (≤700 lines). Per-repo posture record + cross-repo synthesis with verdict per repo.

---

## §Substrate-Abrogation — Lane 08

### Audit purview

Mirror `docs/tranches/meta-audit/08-abrogation-catalog.md`. For every substrate that BA touches or retires (per DEEPX-1..8's deletion-bias enumerations), assign one of five verdicts: **KEEP** / **KEEP-MODERNIZE** / **REPLACE** / **ABROGATE** / **FOLD-INTO-TOOLING**. Specific surfaces: `arena_template.rs` + `builder_template.rs`; per-grammar `arena.rs` + `builder.rs`; `__EAGER_EMPTY_PATH` LazyLock; `LegacyPath`/`LegacySegment` shim; `cursor.match_field` + `match_index` + `decide`; `Vec<OpenFrame>::clone` checkpoint; per-grammar `__path_plan` re-exports; the 32 zero-caller substrates; `AscentStrategy` (DEEP-A flagged consumerless); `Option<&mut PathCursor>` parameter pattern. Per-verdict LOC delta + cumulative net delta.

### Sources the auditor consults

- `docs/tranches/meta-audit/08-abrogation-catalog.md` (methodological exemplar).
- `docs/tranches/AZ-IV/audit/POST-CLOSE-A-legacy.md` + `POST-CLOSE-B-substrate.md` + `W6-substrate-cleanup-route.md`.
- BA/BB/BC deletion targets per their wave specs.

### Deliverable shape

`audit/HARDENING-2026-05-XX-08-abrogation.md` (≤700 lines). Verdict-counts table + per-verdict catalog + cumulative LOC delta.

---

## Architectural invariants on record

These invariants are extracted from GESTALT.md §2, AZ-IV.md §Invariants, the DEEPX cohort findings, and the user's verbatim directives. The audit verifies BA/BB/BC adhere; deviations are findings.

### §1. One parse path

Eager `parse(input)` collapses to `parse_with(input, &EMPTY_PATH)`. The cross-grammar `__EAGER_EMPTY_PATH<Json,_>` literal retires. `feedback_no_orthogonal_codepaths` enforced by mechanism, not policy.

### §2. Type inference is the source of truth

Every rule's TypeDesc reaches the emitter; `->` annotation is a naming hint, not a typing hint. `->`-less compound rules project the same as annotated rules. `project_types` already produces complete TypeDesc; the gap is `StructRegistry` coverage of compound-typed `->`-less rules — closes in BA.W1 inverse-layout-audit IR pass.

### §3. Direct-to-struct, not arena-then-project

The emitter generates per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum from `StructRegistry` at codegen time. The parse fn writes directly to typed fields. The runtime arena/builder template registry indirection retires. No "intermediate untyped phase".

### §4. Cheap value-typed checkpoints

`Checkpoint` is a `(stack_depth, arena_count)` value snapshot, not a `Vec<OpenFrame>::clone`. Speculative branches use predictive first-byte dispatch where alphabets are disjoint (JSON Object/Array entry; CSS rules). `Vec<OpenFrame>::clone` does not appear in samply top-3 on any production bench.

### §5. Sonic-class `get` API, grammar-uniform

`<Grammar>Parser::get<T>(input, path!(<Grammar>, ...))` is the canonical entry; `<Grammar>Document::get<T>(path)` is the post-parse accessor. `path!` validates against the grammar's `StructRegistry` at proc-macro time; invalid paths fail to compile with grammar-aware diagnostics; return type inferred from path's terminal TypeDesc; wildcard returns zero-allocation iterator.

### §6. Substrate with consumer

Every BA substrate (typed `<Grammar>Document` projection, predictive byte dispatch tables, `parse_with`-routed `get` API) is consumed in the same wave it lands. The permanent `crates/ir/tests/substrate_audit.rs` test stays GREEN at every wave close. No "consumer later" substrate.

### §7. No legacy code

Per the user (verbatim): "NO legacy code". Stale eager/lazy split language, dead arena/builder templates, `LegacyPath` shim, `cursor.match_*` family, `__path_plan` re-exports, `__EAGER_EMPTY_PATH` literals are wave-owned deletion targets.

### §8. Evidence closes gates

No gate closes on API existence, grep-only runtime claims, disabled tests, or "consumer later" scaffolding. Samply 7-artefact contract per perf claim becomes canonical close discipline (closes Audit-C F10 watchdog and environmental-gating debt).

### §9. Failing-test census is canonical

Workspace nextest 100% pass at every wave close, including AZ-IV's RED `ts_node_execute` and `substrate_audit` tests (the former routes to BD+ per TS/WASM punt OR closes opportunistically in BA.W2 if direct-projection's TS emit naturally fixes it; the latter via BA.W0 cleanup).

### §10. Recursive grammars project via `Box`-on-cycle

DEEPX-4: cycle-break `Box` driven by `ir.type_obligations`; one rule per SCC promoted to `BoxedEnum` and emits `UnresolvedCompoundRef { cyclic: true }` obligations per Ref into the cyclic rule. BBNF's heavy mutual-recursion (rhs↔alternation↔concatenation↔factor SCC) is the canonical test case. BA.W2 codegen reads obligations and emits `Box<…>` at cyclic-Ref positions; non-cyclic refs project direct.

### §11. Flat-shape early-bail

DEEPX-3: Sheets formula is a Flat compound; the W3-DYNAMIC mechanism gates Object/Array loops only. Flat-shape lazy honoring requires a parse-time early-bail when cursor reaches terminal mid-body. Bind to BA.W4. Same-wave consumers: the two ignored Sheets tests un-ignore.

### §12. Canonical lettering

Per the user (verbatim): "AZ → BA → BB → BC → BD". No fictional successors. BA = direct-projection (un-recycled); BB = rule-discovery (un-subsumed); BC = cleanup (repurposed; orchestration archived to `BC/orchestration-archive-2026-04-30/`).

### §13. Triumvirate dispatch is mandatory

Every BA wave names auto-trigger conditions; mandatory not optional. Triggers per `ORCHESTRATION.md §Triumvirate Auto-Triggers`: JSONL transcript quiet >15 minutes, first-pass return with no commit + no evidence, three diagnostic-loop iterations without root cause, scope reveal that invalidates file bounds / hard gates / substrate-with-consumer wiring.

---

## Execution discipline on record

### §ED1. NO quick solutions, NO workarounds; idiomatic, gestalt approaches

User-verbatim. Architectural transpositions for elegance, simplicity, performance are mandatory.

### §ED2. NO legacy code

User-verbatim. All substrate is wired and consumed; deprecated, contrived, shim-like, complex, or legacy code is wave-owned deletion target.

### §ED3. KISS. ONE PATH.

User-verbatim. For every "two ways to do X" surviving AZ-IV, BA commits to ONE WAY (table in DEEP-SYNTHESIS §VIII).

### §ED4. NO deferrals, especially chronic deferrals

User-verbatim. Every chronic-deferred item must be noted and explicitly addressed in the wave that closes it. Routing to a fictional successor letter (cf. AZ-V) is forbidden.

### §ED5. Mirror SOTA `get` API with superior ergonomics

User-verbatim. sonic-rs / simdjson are the references; bbnf's `path!` produces compile-time grammar-aware diagnostics neither can produce because bbnf has the grammar.

### §ED6. Type inference for `->`-less rules

User-verbatim: "Even without an explicit `->` annotation, we should use our type inference system to infer the type and project into a struct." BA.W1 inverse-layout-audit IR pass is the mechanism.

### §ED7. Ignore TS and WASM backends

User-verbatim: "Ignore our TS and WASM backends for now". Routes to BD+. Shared-ABI question deferred (DEEPX-7 Option 3 candidate).

### §ED8. Triumvirate dispatch for scope increase/change

User-verbatim. Built into every BA wave's Triumvirate Dispatch section.

### §ED9. Output must be complete, not parsimonious

The user's standing voice. Audit reports are comprehensive per their lane's deliverable shape; an audit that leaves any scaffold in placeholder state is incomplete.

### §ED10. Status ticks every ~5 min of orchestrator-silent wait

`ORCHESTRATION.md §Status-Tick Cadence`. Audit-cohort orchestrator emits one-line status tick.

### §ED11. Re-deploy on empty return; no limits

`ORCHESTRATION.md §Returns`. Empty/no-evidence return → verbatim redispatch once → second empty triggers triumvirate.

---

## Orchestration discipline on record

### §O1. Parallelize — 8 lanes per `META-AUDIT-PROMPT.md §O1`

Eight discrete audit lanes run in parallel; sibling worktrees `bbnf-wt-harden-{01..08}`; per-agent `CARGO_TARGET_DIR=<worktree>/target/harden-NN`.

### §O2. Triumvirate on blockers (research + plan + redress)

`ORCHESTRATION.md §Triumvirate`. Research → Plan → Redress, each with its own audit doc. Plan artefact must contain `## Exact Wave-Amendment Text` section with literal markdown blocks.

### §O3. Hard caps on every dispatch

Audit lanes default to 25 min. Wide-scope lanes (02, 03, 06, 07, 08) extend to 35 min. At 0.9N elapsed, commit current state. At N, halt.

### §O4. Worktree isolation; commit before parallelizing

Each lane in its own worktree; main worktree clean before dispatch.

### §O5. Status ticks every ~5min (cf. §ED10)

### §O6. Scope-pivot opens a new tranche letter + new docs/tranches/XX.md

This audit does not pivot scope; it audits BA/BB/BC as written. Findings that imply pivot route to a triumvirate research lane.

### §O7. Sibling worktrees have unique CARGO_TARGET_DIR

Lock contention silently serialises. `feedback_single_cargo_per_target`.

### §O8. Continue indefatigably

`feedback_execute_planned_architecture`. The audit synthesises every lane's findings; partial returns are redispatched.

### §O9. Do NOT start the plan when agents return; refine together

User-verbatim discipline. Synthesis decides BA.W0 readiness; orchestrator does not pre-commit to dispatch.

### §O10. Re-deploy; no limits

Per §ED11 + `ORCHESTRATION.md §Returns`.

### §O11. Boundedness — stop auditing the audit once converged

The synthesis is the convergence point. After synthesis, the audit halts.

---

## Tone and voice on record

### §V1. Archaic diction is deliberate

User memory `feedback_archaic_diction`. *Begat, therein, thereof, hereof, hitherto, indefatigably, thereupon, exhortation, edict, explicate, parsimonious, gestalt* — preserved verbatim in user quotes; the audit report uses the same register when quoting.

### §V2. Independence over obedience

User memory `feedback_corrective_load_bearing`. The audit reports findings even when they retract a prior decision. A finding is not a softening; it is a correction.

### §V3. Senior performance engineer's judgment is the frame

The audit asks: *"would this dispatch close the architectural defect at the altitude DEEP-A and DEEP-B name?"* — not *"does this look reasonable?"*.

### §V4. No AI-writing tells

Per `STYLE.md`. No epanorthosis; sparing unspaced em-dashes; no bulleted-list scaffolding for prose; no padding; concrete file:line evidence.

---

## §Decisions the prior cohorts baked in

These are decisions the DEEPX / DEEP / POST-CLOSE cohorts arrived at through synthesis. The audit verifies they remain canonical; reversals are findings.

1. **Canonical lettering AZ → BA → BB → BC → BD** (DEEP-D Option A).
2. **BA opens before BB** (BA's hard opening gates require workspace nextest 100% pass; ts_node_execute and substrate_audit RED tests close in BA.W5 and BA.W0 respectively, so BA must precede BB by hard gate, not preference).
3. **BC absorbs cleanup, not orchestration** (the prior BC orchestration close artefact is preserved at `BC/orchestration-archive-2026-04-30/`; the BC letter is repurposed).
4. **TS / WASM punted to BD+** (per user; shared-ABI candidate is DEEPX-7 Option 3 — custom IR-based ABI; decision deferred to post-BC tranche).
5. **`merge_path_seed` decision deferred to BA.W0** (delete unless BB.W1 wants the W3.0 path-shape rewrites as a seed bag).
6. **Single-thesis tranche scope; reject "union" framing** (DEEPX-6; AZ-IV's failure mode).
7. **Audit-cohort cap 6 plan-time + 3 hardening reserve + 1 post-close synthesis = 10 max** (DEEPX-6).
8. **Promise→carry ratio target ≥ 80% pure-MET** (not `_with_misses`); below threshold triggers thesis review before FINAL.md (DEEPX-6).

---

## §Original exhortations — verbatim primary sources

The user's recent exhortations that shape this hardening pass. Quoted verbatim from the conversation transcripts of 2026-05-02 / 2026-05-03.

### EX1 — the post-AZ-IV directive (2026-05-02)

> *"DEEPLY audit with 4 agents in parallel our original plan and waves thereof, alongside all changes made herein. Devise a path forward... NO quick solutions, NO workarounds: idiomatic, gestalt approaches. This is a development product, architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable. NO legacy code. Ensure all substrate is wired and consumed... With all approaches: KISS. One path."*

### EX2 — the canonical-ordering directive

> *"Their lettering needs to be updated everywhere, comprehensively, then, such that AZ is followed by BA, then by BB, etc. Canonicalized ordering. These quick solutions and planning items are preposterous. This is a deep plan."*

### EX3 — the type-inference directive

> *"Why is struct projection not wired up? And even without an explicit `->` annotation, we should use our type inference system to infer the type thereof and project into a struct. We should mirror, though with superior ergonomics, sonic-rs's, simdjson's, etc — the SOTA — their get API to be as performant and UX friendly."*

### EX4 — the TS/WASM punt

> *"Ignore our TS and WASM backends for now, these are not relevant and will likely need to be fully re-engineered at some point (or can we leverage a shared ABI?)."*

### EX5 — the rich-profiling directive

> *"We likely should ground our analysis in richer profiling for full json, full l4 css, full google sheets, full bbnf — deploy an agent to profile each of these with full semantic parity — what semantic gaps exist that need to be filled (the aforesaid sonic-class API should be generalized for all grammars, of course), and what compile gaps are extant that need to be filled with the struct and project registry?"*

### EX6 — the 3000-commit question

> *"Why has project to struct failed to land for nearly 3000 commits? Assay our last several plans? What is the true, unambiguous path forward pursuant to gestalt and based on SOTA and better?"*

### EX7 — the no-execute, fully-specify directive

> *"Do not execute BA, but continue to clean up the above and refresh and FULLY specify the BA, BB, and BC tranches. We need to have these fully formed with no ambiguity, one at a time, with maximal wave-based clarity alongside maximal tranche.md clarity and specification, alongside triumvariate dispatch for scope increase/change as we implement."*

### EX8 — the 8-agent + meta-audit directive

> *"These new tranches need to be better specified and rooted in deep analysis of our last 20 tranches, and last 2000 commits. Deploy 8 agents in parallel. We'll harden this process, too, in another pass with another agentic system: output that pithy hardening prompt to further craft, assay, dig up history for all 2000+ commits — look to our meta-audit prompt, can we use that?"*

### EX9 — the meta-audit retrospective

> *"For the hardening, did you consult /Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit at all? We need a better synopsis herein, too."*

### EX10 — the META-AUDIT.md retrospective

> *"I mean, the meta-audit prompt. Did we not formerly have a META-AUDIT.md"*

This brief is the corrective: it mirrors `META-AUDIT-PROMPT.md`'s canonical structure rather than the meta-audit/0{1..8}-*.md *outputs* that prompt produced.

---

## §Failure-mode catalogue — orchestrator drift the arc has corrected

### D1. AZ-V routing — fictional-successor deferral

Three MASKED-DEFERRALs (F2 sonic, F5 TS, F-AU floor) routed to "AZ-V substrate-direct optimization tranche" — invoked 4× in close-state docs but 0× in trajectory/plan/BA/BB. Corrected at DEEP-SYNTHESIS via canonical-ordering Option A; chronic-deferral pattern named.

### D2. Polymath naming over methodology

Initial handoff draft used Euler/Gauss/Noether/Poincaré/Hilbert/Einstein cohort names but did not match the canonical 8-lane meta-audit methodology. Corrected at `0931a3f4`; this brief retracts the polymath framing and mirrors `META-AUDIT-PROMPT.md` directly.

### D3. Phase-2-as-separate-phase framing

Earlier synthesis named a "Phase 2 cleanup absorption" between AZ-IV close and BA opening. DEEPX-8 folded the cleanup into BA.W0 itself. Corrected at `d4085e4e`; BA.W0 IS the cleanup absorption.

### D4. F12 bbnf-buddy as tranche letter

AZ-IV/FINAL.md row F12 named bbnf-buddy as "BC tranche". bbnf-buddy is a separate subproject (per memory `project_bbnf_buddy.md`); BC was the orchestration tranche. Corrected at the F12 edit; bbnf-buddy does not consume a tranche letter.

### D5. Substrate-first / consumer-forward chronic pattern

DEEPX-5 archaeology: each major reform (Era 1 tape, Era 4 builder trait, Era 7 dedup template) re-architected the substrate around the very indirection layer the GESTALT §2.4 invariant forbids; renamed the shim rather than deleting it. Corrective: BA's invariant §6 (substrate-with-consumer in same wave); BA.W0 retires consumerless surface.

### D6. Union-tranche overload

DEEPX-6: AZ-IV absorbed BA + BB + AZ-III + TS + test redress; produced 16+ post-close audits; routed three MASKED-DEFERRALs to fictional AZ-V. Corrective: single-thesis tranche scope; BA holds direct-projection only; BB sequenced after; BC sequenced after.

### D7. Profile-driven evidence retreats faster than it advances

DEEPX-5: AY.W5 measured -27% twitter regression and *deleted the experiment*; AZ-I/II/III did not measure under fat-LTO; AZ-IV.W6.1 finally measured 86.07% inclusive on `Vec<OpenFrame>::clone` and routed to fictional AZ-V. Corrective: BA's Hard Gate uses samply 7-artefact contract per claim; environmental gating retires.

### D8. Substrate-without-consumer declared MET

POST-CLOSE-B / DEEP-A: 32 zero-caller `pub` substrates surfaced at AZ-IV.W5.4 as the cumulative tail of "substrate lands first; consumer routed forward". Corrective: BA.W0 cleanup absorption; substrate-audit GREEN at every BA wave close.

### D9. Doc-drift across plan surface

DEEPX-6: tranche-status words drift between `<LETTER>.md` top-level, wave specs, FINAL, GESTALT, codegen-paths. Corrective: this brief's §Spec-Drift lane; the audit produces a paste-ready amendment for each drift.

### D10. Scope-creep in audits

The hardening audit examines BA/BB/BC for drift but does NOT rewrite them in-place. An auditor tempted to "fix `BA/BA.md` inline" is out of scope; the audit recommends the fix; a separate user-authorised pass applies.

### D11. Mode-slip — execution voice in an audit brief

This brief, like `META-AUDIT-PROMPT.md`, has explicit imperatives ("the auditor reads...", "the synthesis decides..."). These are descriptive of the audit workflow, not directives to execute against the arc. An auditor flags any sentence that appears to instruct execution against BA/BB/BC source code as a voice-leak finding.

### D12. Stale primary-source citations

Every quoted exhortation cites a transcript / commit / file:line. If a citation has drifted (file moved, line shifted, commit superseded), flag as a priority finding — the brief's evidentiary base is compromised.

---

## §Reading list — files the audit checks for drift

### Canon — top-level state

- `docs/GESTALT.md` — synthesis canon (refreshed `40092b28`).
- `docs/codegen-paths.md` — pipeline diagram (W4 scrubs applied).
- `docs/tranches/AZ-IV/{AZ-IV,FINAL,PROGRESS}.md` — AZ-IV close + ledger.

### Canonical methodology

- `docs/tranches/meta-audit/{01..08}-*.md` — the 8-lane canonical methodology.
- `docs/tranches/meta-audit/ARCHIVE.md` — provenance of the bundle.
- `docs/tranches/meta-audit/archaeology/era-{II..VI}*.md` — era-deep-dives.
- `docs/audit/archives/META-AUDIT-PROMPT.md` — the canonical audit-only prompt this brief mirrors (1774 lines, archived).
- `docs/tranches/AZ-IV/audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md` — the prior hardening synthesis whose pattern this audit's synthesis mirrors.

### Deep cohort outputs

- `docs/tranches/AZ-IV/audit/DEEPX-{1..8}-*.md`.
- `docs/tranches/AZ-IV/audit/DEEP-{A,B,C,D}-*.md`.
- `docs/tranches/AZ-IV/audit/POST-CLOSE-{A,B,C,D}-*.md`.
- `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md` — canonical-ordering synthesis.
- `docs/tranches/AZ-IV/audit/SYNOPSIS-2026-05-03.md` — orchestrator-level recap.

### Runway specs (audit subjects)

- `docs/tranches/BA/{BA.md, waves/W{0..6}.md}` — direct-projection codegen.
- `docs/tranches/BB/{BB.md, waves/W{0..6}.md}` — rule-discovery (un-subsumed).
- `docs/tranches/BC/{BC.md, waves/W{0..6}.md}` — cleanup pass (repurposed).

### Instructions — orchestration edicts

- `docs/precepts/instructions/{README.md,STYLE.md,ORCHESTRATION.md,CONSUMING.md,LESSONS-LEARNED.md}`.
- `docs/precepts/instructions/tranche/{SPEC.md,WAVE_SPEC.md,AGENT_DISPATCH_TEMPLATE.md,RESEARCH.md,CHALLENGE.md,DOC_UPDATE_WAVE.md,START.md}`.
- `docs/instructions/PROFILING.md` — samply 7-artefact contract canon.

### Tranche archaeology (for context)

- `docs/tranches/B0..B7/` — pre-AY foundations.
- `docs/tranches/AT,AU,AV,AW-{I..V},AX-{I..III},AY-{I..III},AZ-{I..IV}/` — era II-VI.
- `docs/tranches/{BA,BB}/historical/` — pre-canonical-ordering archives.
- `docs/tranches/BC/orchestration-archive-2026-04-30/` — closed-meta-tranche archive.

---

## §Audit methodology — how to conduct and what to deliver

This section is the one prescriptive section of the brief. Every other section is descriptive.

### Workflow

1. **Baseline verification** (§Reading list). Open each listed artefact; note any missing, renamed, or content-shifted off this brief's summary. A missing canonical artefact is the first-priority finding.

2. **Invariant verification** (§Architectural invariants on record, §Decisions the prior cohorts baked in). For each invariant or decision, search BA/BB/BC for adherence; cite each finding's BA/BB/BC line.

3. **Discipline verification** (§Execution discipline, §Orchestration discipline, §Tone and voice). For each rule, audit BA/BB/BC + the latest two long-session transcripts for compliance.

4. **Scaffold population** (§Spec-Friction through §Substrate-Abrogation — the 8 lanes). Populate the deliverable each lane names. An audit that leaves any scaffold in placeholder state is incomplete.

5. **Failure-mode recurrence check** (§Failure-mode catalogue). For each drift pattern (D1..D12), search for recurrences since DEEP-SYNTHESIS. Recurrences are high-priority findings.

6. **Brief-integrity sweep**. Scan this document for mode-slip, stale citations, and scaffolds whose purview has been outdated by arc evolution. Mode-slip is a meta-finding.

### Cohort dispatch

- 8 sibling worktrees: `bbnf-wt-harden-{01..08}` at master, `-b harden-NN`.
- Per-agent `CARGO_TARGET_DIR=<worktree>/target/harden-NN`.
- Each lane dispatches in parallel; HARD CAPs per §O3 (25 min for 01/04/05; 35 min for 02/03/06/07/08).
- Each lane writes one audit doc at `audit/HARDENING-2026-05-XX-NN-<name>.md`.
- After all 8 return, synthesise at `audit/HARDENING-SYNTHESIS-2026-05-XX.md` patterned after `audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md`.

### Deliverable — synthesis report structure

The synthesis is a single file with these top-level sections:

1. **Cohort table** — per-lane agent / worktree / audit doc / commit / lines.
2. **Disposition by lane** — ACCEPT / NARROW / REJECT counts; paste-ready amendments produced.
3. **Cross-cutting themes** — patterns surfaced by ≥2 lanes.
4. **Paste-ready amendment blocks** — per BA/BB/BC file affected, literal markdown to insert.
5. **Decision: BA.W0 ready, or 4th-pass amendment needed** — single sentence; cite the gate that determines.
6. **Brief integrity** — mode-slip / stale-citation findings against this prompt itself.
7. **Routed carries to BB / BC / BD+** — per row, named close criterion + destination letter.

### Scope boundary

The audit does not commit BA/BB/BC source code, dispatch agents against BA's runway, open tranches, or execute any wave. §O9 (do not start the plan when agents return) and §O11 (stop auditing the audit once converged) bind the auditor as strictly as the orchestrator. The synthesis is the entire deliverable; any redress is authorised separately by the user.

### Failure-mode warnings for auditors using this brief

(a) **Drift between brief and canon.** §Architectural invariants, §Decisions baked in, and §Reading list are snapshots from `c5a6fab9`. If discrepancy emerges against current canon, canon is authoritative; the brief is the finding.

(b) **Stale scaffolds.** §Spec-Friction through §Substrate-Abrogation expect auditor-populated content. An audit that leaves any scaffold in placeholder state files that as a finding.

(c) **Quote provenance integrity.** Every quoted exhortation is verbatim from the user's 2026-05-02 / 2026-05-03 conversation transcripts. If a quote has drifted, flag as priority finding.

(d) **Prefix drift.** §1..§13 / §ED1..§ED11 / §O1..§O11 / §V1..§V4 / EX1..EX10 / D1..D12 numbering is load-bearing for cross-reference. The audit records suggestions; a subsequent user-authorised pass renumbers.

(e) **Mode-slip.** Read every imperative-voice passage critically; treat any sentence that appears to instruct execution against BA/BB/BC source code as a voice-leak.

(f) **Scope-creep.** The audit examines BA/BB/BC for drift but does not rewrite them in-place. Recommend the fix; a separate pass applies.

---

## How to use this audit briefing

Paste or load this brief into a fresh agent context (or hand it to a human auditor) at the bbnf-lang repo root. The receiver becomes the auditor. The auditor reads this brief to load the arc's current context; reads the canonical docs in §Reading list to ground each invariant / decision / discipline rule; populates the §Spec-Friction through §Substrate-Abrogation scaffolds per their deliverable shapes; checks failure-mode recurrence; performs brief-integrity sweep; produces the synthesis at `audit/HARDENING-SYNTHESIS-2026-05-XX.md` per §Audit methodology.

### Self-contained brief for the next orchestrator

```
You are the orchestrator of the BA/BB/BC hardening audit cohort.
Repo: /Users/mkbabb/Programming/bbnf-lang at master c5a6fab9.

1. Read this brief verbatim:
   docs/tranches/AZ-IV/audit/HANDOFF-HARDENING-PROMPT.md
2. Read the canonical methodology this brief mirrors:
   docs/audit/archives/META-AUDIT-PROMPT.md (1774 lines, archived)
   docs/tranches/meta-audit/{01..08}-*.md (the outputs that prompt
     produced; methodological exemplars per lane)
3. Read the deep-cohort inputs:
   docs/tranches/AZ-IV/audit/DEEPX-{1..8}-*.md
   docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md
   docs/tranches/AZ-IV/audit/SYNOPSIS-2026-05-03.md
4. Read the audit subjects (BA/BB/BC current specs):
   docs/tranches/{BA,BB,BC}/{<LETTER>.md, waves/W{0..6}.md}
5. Create 8 sibling worktrees:
   bbnf-wt-harden-{01..08} at master with -b harden-NN.
6. Dispatch 8 lanes in parallel using §Spec-Friction through
   §Substrate-Abrogation (one lane per scaffold). Each lane gets:
   - the methodological exemplar at meta-audit/0N-*.md
   - the lane's audit purview / sources / deliverable shape
   - the HARD CAP per §O3
   - the output path audit/HARDENING-2026-05-XX-NN-<name>.md
   Each writes one audit doc.
7. After all 8 return, synthesise at:
   audit/HARDENING-SYNTHESIS-2026-05-XX.md
   patterned exactly after
   audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md
8. Cherry-pick all 9 commits to master.
9. Apply paste-ready amendments to BA/BB/BC where the synthesis
   prescribes. Re-validate cross-references with §Original
   exhortations verbatim.
10. Decide: BA.W0 ready, or 4th-pass amendment needed?
11. Return a single completion message: state of BA/BB/BC; commit
    hash of HARDENING-SYNTHESIS; BA.W0 readiness signal; routed
    carries to BB/BC/BD+ with named close criteria.

Non-negotiables (per §ED1..§ED11):
- Audit-only contract: no BA/BB/BC source-code commits.
- File bounds: audit/ + tranches/{BA,BB,BC}/ only (the latter for
  paste-ready amendments produced by synthesis).
- KISS / no workarounds / one path.
- Canonical AZ→BA→BB→BC→BD lettering.
- Cite file:line evidence in every finding.
- Triumvirate fires per §13 + ORCHESTRATION.md §Triumvirate
  Auto-Triggers.
- Pre-existing memory at
  /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/
  is read; do not duplicate; update only if new feedback surfaces.
- Archaic diction is voice; preserve in user-quote register.
```

### Two quotes, as failure poles the arc has observed

> *"Sycophancy is to the orchestrator what backwards compatibility is to the engineer."* — corrective load-bearing (§V2).

> *"Substrate without consumer is half a fix; the half that survives metastasises."* — DEEP-SYNTHESIS §I, naming the chronic pattern.

---

## Provenance

- **Brief authored**: 2026-05-03 by orchestrator at master `c5a6fab9`.
- **Canonical exemplar**: `docs/audit/archives/META-AUDIT-PROMPT.md` (archived 2026-05-01 at `b9863bf3`; preserved verbatim for archaeology).
- **Methodological sub-exemplars**: `docs/tranches/meta-audit/01..08-*.md` (2026-04-22 bundle; cited live by AZ-IV.W0).
- **Cohort inputs**: `docs/tranches/AZ-IV/audit/DEEPX-{1..8}-*.md`, `DEEP-{A,B,C,D}-*.md`, `POST-CLOSE-{A,B,C,D}-*.md`, `DEEP-SYNTHESIS.md`, `SYNOPSIS-2026-05-03.md`.
- **Subjects**: `docs/tranches/{BA,BB,BC}/{<LETTER>.md, waves/W{0..6}.md}`.
- **User exhortation transcripts**: 2026-05-02 / 2026-05-03 conversation (sessions ongoing).

The next agentic system reads this brief, executes the 8-lane cohort, returns the synthesis. After that returns and the orchestrator re-validates, BA.W0 dispatches. The arc has one path forward.
