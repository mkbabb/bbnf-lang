# bbnf-lang — Hardening Audit Prompt

A generalised hardening-audit brief for tranche specifications. The current subjects are **BA (direct-projection codegen)**, **BB (rule-discovery)**, and **BC (cleanup)** — the three planned tranches that succeed AZ-IV in the canonical AZ → BA → BB → BC → BD ordering. Future hardening passes reuse this brief by replacing the current-subjects table at the top of §Audit subjects.

**This document is audit-only.** Ingestion and audit-report production are the entire task it prescribes. Commits against source code, agent dispatches against any tranche's runway, and state mutation against the arc are out of scope. If any passage below appears to instruct such action, the document is wrong — audit only, and flag the voice-leak as a finding. Execution authorisation comes from the user, in a subsequent and separate pass.

The user's archaic diction is deliberate voice — *begat, therein, thereof, hereof, hitherto, indefatigably, thereupon, exhortation, edict, explicate, parsimonious, gestalt* — preserved verbatim in quotes throughout this brief. Audit reports preserve the same register when quoting user material.

---

## Preamble — what you are auditing

You are auditing whether the next tranche cycle reifies the work the arc has produced over its 3000-commit history. Concretely:

- whether the codebase aligns into a cohesive whole — one grammar surface, one IR substrate, one parse path, one semantic surface, derived from the grammar (not hardcoded), with no orthogonal codepaths surviving;
- whether the planned tranches finally close the chronic performance gap such that bbnf benches at parity-or-better against sonic-rs / simdjson / lightningcss on the same harness — not 4 196× behind, not 5.22× behind, not "routed to a successor letter";
- whether the planned tranches finally land a value/semantic UX superior to the SOTA — `path!(<Grammar>, ...)` with grammar-aware compile-time diagnostics neither sonic-rs nor simdjson can produce because neither has the grammar; type inference covers `->`-annotated and `->`-less rules uniformly so a grammar can be authored without ceremony and still produce typed output; the wildcard expression is the iterator, not a manual cursor; the path expression IS the API;
- whether the architectural defect that has metastasised across the arc — the parser doing runtime checkpoint-and-rollback over an untyped slab because compile-time-resolved direct projection isn't emitted — finally retires through mechanism, not patching.

The current architectural defect is named at two altitudes by the post-AZ-IV deep cohort. *Architectural*: `project_types` populates the `StructRegistry` at codegen but the parser never consumes it. Every emitted parse fn constructs `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` (10 emission sites). Type inference output is thrown away at the parse boundary. *Cost*: 86.07 % of inclusive samples on `bbnf_value_twitter` (samply, 25 963 samples, fat-LTO `[profile.bench]`) are `Vec<OpenFrame>::clone` from `JsonStructBuilder::checkpoint`. The same mechanism observed in CSS L4 (1 407 checkpoint sites; 7.6 MB cloned per bootstrap parse), Sheets, BBNF.

These are the same defect at two altitudes. One mechanism — direct-projection codegen consuming the `StructRegistry` that `project_types` already populates, with cheap `(stack_depth, arena_count)` value-typed checkpoints, with predictive first-byte dispatch where alphabets are disjoint, with `Document::get<T>(path)` rerouting through `parse_with` so eager is the degenerate case of lazy with `&EMPTY_PATH` — closes the defect at both altitudes plus four chronic carries (4196× sonic_get gap, 5.22× sonic_value gap, 18/19 AU floor BELOW, ts_node_execute RED).

The audit's task is to verify that the planned tranches actually close this defect through that mechanism, that no orthogonal codepath survives, that no chronic deferral routes to a fictional successor letter, and that the gestalt — the unified shape `docs/GESTALT.md` describes — is realised at the close of the audited cycle.

---

## §Audit subjects — current cycle

Replace this table per cycle.

| Letter | Thesis | Top-level | Wave specs | Hard gates |
|---|---|---|---|---|
| **BA** | Direct-projection codegen | `docs/tranches/BA/BA.md` | `docs/tranches/BA/waves/W{0..6}.md` | 24 |
| **BB** | Rule-discovery (un-subsumed) | `docs/tranches/BB/BB.md` | `docs/tranches/BB/waves/W{0..6}.md` | 18 |
| **BC** | Cleanup pass (repurposed) | `docs/tranches/BC/BC.md` | `docs/tranches/BC/waves/W{0..6}.md` | 15 |

Audit baseline: master HEAD `c5a6fab9` (post-DEEPX-8 spec landing, post-residue cleanup).

### Reading the brief

The audit workflow is in §Audit methodology. Between this Preamble and that section, the brief is divided into:

- **Scaffolded sections the audit populates** — §Gestalt, §Spec-Friction, §Edict-Adherence, §Spec-Drift, §Toolchain-Forecast, §Cohort-Validation, §Tranche-Archaeology, §Appurtenant-Posture, §Substrate-Abrogation. Each carries its own purview, sources, and deliverable shape.
- **Reference sections the audit cites against current canon** — §Architectural invariants, §Execution discipline, §Orchestration discipline, §Tone and voice, §Decisions on record, §Original exhortations, §Failure-mode catalogue, §Reading list.

A fresh auditor may read top-to-bottom, or jump straight to §Audit methodology and populate scaffolds with the reference sections open as lookup.

---

## §Gestalt — the system the arc is converging on

### Audit purview

A single-page synthesis of what bbnf-lang looks like in totality once the audited tranche cycle closes: one grammar surface; one IR substrate; one parse path (eager is `parse_with(input, &EMPTY_PATH)`); one semantic surface grammar-derived (no hardcoded semantics; type inference covers annotated and unannotated rules uniformly); the CSP solver and the e-graph as pluggable optimisation substrates; the VM as bounded oracle on residue only; the Rust backend emitter direct-projecting per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum; `Document::get<T>(path)` mirroring sonic-rs's `pointer!` API with grammar-aware compile-time diagnostics; parity-or-better against sonic-rs / simdjson / lightningcss on the published competitor harness; sibling repos (parse-that, pprint, csp-solver, csc411) pinned and path-patched.

The auditor verifies whether the audited tranche cycle realises each element of this shape, cites the wave that closes it, and names any element the cycle does not address.

### Sources the auditor consults

- `docs/GESTALT.md` — primary current-state synthesis.
- `docs/codegen-paths.md` — pipeline diagram.
- `docs/tranches/<LETTER>/<LETTER>.md` for each audit subject — runway specs.
- Runtime code under `crates/` — the realised shape; one-path verifiable via `rg -n "fn parse\b" crates/core/src/runtime` for eager-vs-lazy split.
- `docs/benchmarks/` — competitor parity / superiority evidence.

### Deliverable shape

A gestalt-drift ledger: per-element YES / PARTIAL / NO; cite evidence; if PARTIAL / NO name the wave or letter that would close the gap.

---

## §Spec-Friction

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/01-session-friction.md`. Mine the latest two long-session transcripts for orchestrator-side friction during spec authoring of the audit subjects. Quantify Bash-poll vs Monitor adoption, parallel-agent overlap, worktree contention, redispatch-after-empty rate, status-tick cadence honoured. Identify 3-5 ranked friction patterns the next dispatch will inherit and propose mechanism-level fixes.

### Sources the auditor consults

- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/*.jsonl` (most recent two long sessions).
- `docs/tranches/meta-audit/01-session-friction.md`.
- `docs/precepts/instructions/ORCHESTRATION.md` §Long-Running Commands, §Status-Tick Cadence.

### Deliverable shape

`audit/HARDENING-<DATE>-01-spec-friction.md` (≤ 500 lines). Tool-use frequency table per session; 3-5 ranked patterns with verbatim transcript quotes; mechanism-level fix per pattern.

---

## §Edict-Adherence

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/02-instruction-adherence.md`. For every edict in `docs/precepts/instructions/`, audit the audit subjects for adherence. Specific items: heavy-surface routine defaults; agent-cap per wave (≤ 6); triumvirate auto-trigger thresholds; status-tick cadence; substrate-with-consumer; deletion bias; six-agent ceiling; KISS. Verbatim quote each edict + cite the audit-subject line that adheres or violates.

### Sources the auditor consults

- `docs/precepts/instructions/{README.md,STYLE.md,ORCHESTRATION.md,CONSUMING.md,LESSONS-LEARNED.md}`.
- `docs/precepts/instructions/tranche/{SPEC.md,WAVE_SPEC.md,AGENT_DISPATCH_TEMPLATE.md,RESEARCH.md,CHALLENGE.md,DOC_UPDATE_WAVE.md,START.md}`.
- `docs/tranches/meta-audit/02-instruction-adherence.md`.
- Audit-subject top-level + wave specs.

### Deliverable shape

`audit/HARDENING-<DATE>-02-edict-adherence.md` (≤ 700 lines). Per-edict table; memory-proposal scope if a new feedback emerges.

---

## §Spec-Drift

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/03-tranche-drift.md`. Audit consistency between (a) `<LETTER>.md` top-level, (b) `<LETTER>/waves/W*.md`, and (c) cross-references to `GESTALT.md`, `codegen-paths.md`, the predecessor-tranche FINAL.md, and any cohort synthesis docs. Specific items: wave-status words match the top-level wave table; hard gates referenced by waves match top-level numbering; file bounds disjoint across waves; carry-ledger items bound to a wave; non-routable carries enumerated with closure proof; cross-tranche dependencies cited honestly; deletion-bias targets surface in the wave that owns them.

### Sources the auditor consults

- Audit-subject top-level + wave specs.
- Cohort synthesis docs in `docs/tranches/<predecessor>/audit/`.
- Predecessor `FINAL.md` and `PROGRESS.md`.
- `docs/tranches/meta-audit/03-tranche-drift.md`.

### Deliverable shape

`audit/HARDENING-<DATE>-03-spec-drift.md` (≤ 700 lines). D1-DN numbered findings; per-finding fix proposal with paste-ready amendment.

---

## §Toolchain-Forecast

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/04-toolchain-pain.md`. Forecast the toolchain pain the audited cycle's mechanism will introduce or alleviate. Measure (where feasible): cold/warm walls for `cargo iter-check` and `cargo nextest --profile ax-iter` after the cycle's emitter changes; regen wall after typed-record emission lands; per-shape-emitter compile cost. Identify 3-5 ranked pain points the wave specs underestimate and propose pre-W0 mitigations.

### Sources the auditor consults

- `Makefile` + `.cargo/config.toml` + `.config/nextest.toml`.
- `docs/tranches/meta-audit/04-toolchain-pain.md`.
- Audit-subject wave specs.
- `docs/instructions/PROFILING.md`.

### Deliverable shape

`audit/HARDENING-<DATE>-04-toolchain-forecast.md` (≤ 500 lines). Wall-clock matrix; pain-points ranked by friction × frequency; per-pain mechanism-level fix.

---

## §Cohort-Validation

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/05-validation.md`. Verify each top claim from the predecessor cohort's audits against current master. Sample for the current cycle: does the deep-cohort's "10 hardcoded `rule_type: TypeDesc::Span` emission sites" still grep at the named files? Has the count drifted? Do the era-archaeology commit citations still resolve? Does the SOTA-mirror claim hold against actual macro behaviour? Triage each claim **VALIDATED** / **NARROW** / **STALE**.

### Sources the auditor consults

- `docs/tranches/<predecessor>/audit/DEEPX-{1..N}-*.md` (or whichever cohort precedes this audit).
- `docs/tranches/meta-audit/05-validation.md`.
- Current source under `crates/`.

### Deliverable shape

`audit/HARDENING-<DATE>-05-cohort-validation.md` (≤ 500 lines). Per-claim table: claim verbatim + verification command + outcome.

---

## §Tranche-Archaeology

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/06-commit-archaeology.md`. For each hard gate in the leading audit subject (the one that opens first), locate the prior commit (or commit cluster) that previously attempted to close it and either (a) succeeded then regressed, (b) failed and routed forward, or (c) never attempted. Surface gates that match category (a) or (b) more than once across history — these are the chronic risks the audited cycle must arm against.

### Sources the auditor consults

- `docs/tranches/meta-audit/06-commit-archaeology.md` + `archaeology/era-{II..VI}*.md`.
- `git log --all` archaeology via `-S` / `--grep` / `--diff-filter` per gate.
- The leading audit subject's hard-gate list.
- Predecessor cohort's archaeology audits if any.

### Deliverable shape

`audit/HARDENING-<DATE>-06-tranche-archaeology.md` (≤ 700 lines). Per-hard-gate row: `[gate # | prior attempt commit | outcome | what changed | what the new cycle does differently]`.

---

## §Appurtenant-Posture

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/07-appurtenant-assay.md`. Re-assay the appurtenant ring (parse-that, pprint, csp-solver/csc411 sibling, wasm/, gorgeous, bbnf-buddy, precepts submodule, ffuzzy) against the audited cycle's mechanism. Specific questions: does the cycle's emitter cross-pollute parse-that's combinator surface? Does the cycle introduce new sibling-repo dependencies (which version, where pinned, does it cross with siblings)? Verdict per repo: **READY** / **PRE-CYCLE TOUCH** / **POST-CYCLE TOUCH** / **OUT-OF-SCOPE-FOREVER**.

### Sources the auditor consults

- `docs/tranches/meta-audit/07-appurtenant-assay.md`.
- `Cargo.toml` (workspace) + `.cargo/config.toml` (path-patches).
- Sibling repositories at `/Users/mkbabb/Programming/{parse-that,pprint}` and equivalents.

### Deliverable shape

`audit/HARDENING-<DATE>-07-appurtenant.md` (≤ 700 lines). Per-repo posture record + cross-repo synthesis with verdict per repo.

---

## §Substrate-Abrogation

### Audit purview

Methodological exemplar: `docs/tranches/meta-audit/08-abrogation-catalog.md`. For every substrate the audited cycle touches or retires, assign one of five verdicts: **KEEP** / **KEEP-MODERNIZE** / **REPLACE** / **ABROGATE** / **FOLD-INTO-TOOLING**. Per-verdict LOC delta + cumulative net delta. The current cycle's deletion targets include: `arena_template.rs` + `builder_template.rs`; per-grammar `arena.rs` + `builder.rs`; `__EAGER_EMPTY_PATH` LazyLock; `LegacyPath` / `LegacySegment` shim; `cursor.match_field` + `match_index` + `decide`; `Vec<OpenFrame>::clone` checkpoint; per-grammar `__path_plan` re-exports; the 32 zero-caller substrates; `AscentStrategy` (consumerless); `Option<&mut PathCursor>` parameter pattern.

### Sources the auditor consults

- `docs/tranches/meta-audit/08-abrogation-catalog.md`.
- Predecessor cohort's substrate audits.
- Audit-subject wave specs (deletion targets per wave).

### Deliverable shape

`audit/HARDENING-<DATE>-08-abrogation.md` (≤ 700 lines). Verdict-counts table + per-verdict catalog + cumulative LOC delta.

---

## §Architectural invariants

These invariants are extracted from `GESTALT.md §2`, predecessor invariants, the cohort findings, and verbatim user directives. The audit verifies the audit subjects adhere; deviations are findings.

### §1. One parse path

Eager `parse(input)` collapses to `parse_with(input, &EMPTY_PATH)`. The cross-grammar `__EAGER_EMPTY_PATH<Json,_>` literal retires. `feedback_no_orthogonal_codepaths` enforced by mechanism, not policy.

### §2. Type inference is the source of truth

Every rule's TypeDesc reaches the emitter; `->` annotation is a naming hint, not a typing hint. `->`-less compound rules project the same as annotated rules. `project_types` already produces complete TypeDesc; the gap is `StructRegistry` coverage of compound-typed `->`-less rules — closes via inverse-layout-audit IR pass.

### §3. Direct-to-struct, not arena-then-project

The emitter generates per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum from `StructRegistry` at codegen time. The parse fn writes directly to typed fields. The runtime arena/builder template registry indirection retires. No "intermediate untyped phase".

### §4. Cheap value-typed checkpoints

`Checkpoint` is a `(stack_depth, arena_count)` value snapshot, not a `Vec<OpenFrame>::clone`. Speculative branches use predictive first-byte dispatch where alphabets are disjoint. `Vec<OpenFrame>::clone` does not appear in samply top-3 on any production bench.

### §5. Sonic-class `get` API, grammar-uniform

`<Grammar>Parser::get<T>(input, path!(<Grammar>, ...))` is the canonical entry; `<Grammar>Document::get<T>(path)` is the post-parse accessor. `path!` validates against the grammar's `StructRegistry` at proc-macro time; invalid paths fail to compile with grammar-aware diagnostics; return type inferred from path's terminal TypeDesc; wildcard returns zero-allocation iterator. The grammar IS the API surface; competitors cannot match this because they do not have the grammar.

### §6. Substrate with consumer

Every substrate the audited cycle introduces is consumed in the same wave. The permanent `crates/ir/tests/substrate_audit.rs` test stays GREEN at every wave close. No "consumer later" substrate.

### §7. No legacy code

Stale eager/lazy split language, dead arena/builder templates, `LegacyPath` shim, `cursor.match_*` family, `__path_plan` re-exports, `__EAGER_EMPTY_PATH` literals are wave-owned deletion targets.

### §8. Evidence closes gates

No gate closes on API existence, grep-only runtime claims, disabled tests, or "consumer later" scaffolding. Samply 7-artefact contract per perf claim is the canonical close discipline. Environmental gating retires.

### §9. Failing-test census is canonical

Workspace nextest 100 % pass at every wave close. RED tests close in named waves OR route to a named successor letter — never to a fictional one.

### §10. Recursive grammars project via `Box`-on-cycle

Cycle-break `Box` driven by `ir.type_obligations`; one rule per SCC promoted to `BoxedEnum`; emits `UnresolvedCompoundRef { cyclic: true }` obligations per Ref into the cyclic rule. Codegen reads obligations and emits `Box<…>` at cyclic-Ref positions; non-cyclic refs project direct.

### §11. Flat-shape early-bail

Compounds whose shape is Flat (Sheets formula; CSS at-rules; some BBNF rules) honour lazy bail-out via `cursor.has_resolved()` between positions, closing the open compound frame and returning success when the terminal is reached. The Object/Array dynamic-decision mechanism does not gate Flat alone.

### §12. Canonical lettering

`AZ → BA → BB → BC → BD`. No fictional successors. Letter recycling is recorded explicitly in the recycled letter's archaeology (`historical/`); `SUBSUMED` banners are temporary and resolved by the next cycle's plan surgery.

### §13. Triumvirate dispatch is mandatory

Every wave names auto-trigger conditions; mandatory not optional. Triggers per `ORCHESTRATION.md §Triumvirate Auto-Triggers`: JSONL transcript quiet > 15 minutes; first-pass return with no commit + no evidence; three diagnostic-loop iterations without root cause; scope reveal that invalidates file bounds / hard gates / substrate-with-consumer wiring.

---

## §Execution discipline

### §ED1. NO quick solutions, NO workarounds; idiomatic, gestalt approaches

Architectural transpositions for elegance, simplicity, performance are mandatory.

### §ED2. NO legacy code

All substrate is wired and consumed; deprecated, contrived, shim-like, complex, or legacy code is wave-owned deletion target.

### §ED3. KISS. ONE PATH.

For every "two ways to do X" surviving the predecessor cycle, the audited cycle commits to ONE WAY.

### §ED4. NO deferrals, especially chronic deferrals

Every chronic-deferred item is noted and explicitly addressed in the wave that closes it. Routing to a fictional successor letter is forbidden.

### §ED5. Mirror SOTA `get` API with superior ergonomics

sonic-rs / simdjson are the references; the cycle's `path!` produces compile-time grammar-aware diagnostics neither can produce because they do not have the grammar.

### §ED6. Type inference for `->`-less rules

Even without an explicit `->` annotation, type inference infers the type and the emitter projects into a struct.

### §ED7. Ignore TS and WASM backends until route assigned

Routes to BD+. Shared-ABI question deferred until the assigned tranche opens.

### §ED8. Triumvirate dispatch for scope increase/change

Built into every wave's Triumvirate Dispatch section.

### §ED9. Output must be complete, not parsimonious

Audit reports are comprehensive per their lane's deliverable shape. An audit that leaves any scaffold in placeholder state is incomplete.

### §ED10. Status ticks every ~5 min of orchestrator-silent wait

`ORCHESTRATION.md §Status-Tick Cadence`.

### §ED11. Re-deploy on empty return; no limits

`ORCHESTRATION.md §Returns`. Empty/no-evidence return → verbatim redispatch once → second empty triggers triumvirate.

---

## §Orchestration discipline

### §O1. Parallelize — 8 lanes

Eight discrete audit lanes run in parallel; sibling worktrees `bbnf-wt-harden-{01..08}`; per-agent `CARGO_TARGET_DIR=<worktree>/target/harden-NN`.

### §O2. Triumvirate on blockers

Research → Plan → Redress, each with its own audit doc. Plan artefact must contain `## Exact Wave-Amendment Text` section with literal markdown blocks.

### §O3. Hard caps on every dispatch

Audit lanes default to 25 min. Wide-scope lanes (02, 03, 06, 07, 08) extend to 35 min. At 0.9N elapsed, commit current state. At N, halt.

### §O4. Worktree isolation; commit before parallelizing

Each lane in its own worktree; main worktree clean before dispatch.

### §O5. Status ticks every ~5 min

### §O6. Scope-pivot opens a new tranche letter + new docs/tranches/XX.md

This audit does not pivot scope; it audits the named subjects. Findings that imply pivot route to a triumvirate research lane.

### §O7. Sibling worktrees have unique CARGO_TARGET_DIR

Lock contention silently serialises.

### §O8. Continue indefatigably

The audit synthesises every lane's findings; partial returns are redispatched.

### §O9. Do NOT start the plan when agents return; refine together

Synthesis decides next-tranche dispatch readiness; orchestrator does not pre-commit.

### §O10. Re-deploy; no limits

Per §ED11.

### §O11. Boundedness — stop auditing the audit once converged

The synthesis is the convergence point. After synthesis, the audit halts.

---

## §Tone and voice

### §V1. Archaic diction is deliberate

User memory `feedback_archaic_diction`. Preserved verbatim in user quotes; the audit report uses the same register when quoting.

### §V2. Independence over obedience

The audit reports findings even when they retract a prior decision. A finding is not a softening; it is a correction.

### §V3. Senior performance engineer's judgment is the frame

The audit asks: *"would this dispatch close the architectural defect at both altitudes the cohort named?"* — not *"does this look reasonable?"*.

### §V4. No AI-writing tells

Per `STYLE.md`. No epanorthosis; sparing unspaced em-dashes; no bulleted-list scaffolding for prose; no padding; concrete file:line evidence.

---

## §Decisions on record

These decisions the predecessor cohort arrived at through synthesis. The audit verifies they remain canonical; reversals are findings.

1. **Canonical lettering** AZ → BA → BB → BC → BD.
2. **BA opens before BB** by hard gate (BA's opening gates require workspace nextest 100 % pass; AZ-IV's RED tests close in BA waves, so BA must precede BB by hard gate, not preference).
3. **BC absorbs cleanup, not orchestration** (the prior BC orchestration close artefact preserved at `BC/orchestration-archive-2026-04-30/`; the BC letter is repurposed).
4. **TS / WASM punted to BD+** (shared-ABI candidate is custom IR-based ABI; decision deferred to the assigned tranche).
5. **`merge_path_seed` decision deferred to BA.W0** (delete unless BB.W1 wants the W3.0 path-shape rewrites as a seed bag).
6. **Single-thesis tranche scope; reject "union" framing** (predecessor's failure mode; AZ-IV absorbed too much).
7. **Audit-cohort cap** 6 plan-time + 3 hardening reserve + 1 post-close synthesis = 10 max.
8. **Promise→carry ratio target ≥ 80 % pure-MET** (not `_with_misses`); below threshold triggers thesis review before FINAL.md.

---

## §Original exhortations

The user's exhortations on record. Quoted verbatim.

### EX1 — the no-workarounds directive

> *"NO quick solutions, NO workarounds: idiomatic, gestalt approaches. This is a development product, architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable. NO legacy code. Ensure all substrate is wired and consumed... With all approaches: KISS. One path."*

### EX2 — the canonical-ordering directive

> *"Their lettering needs to be updated everywhere, comprehensively, then, such that AZ is followed by BA, then by BB, etc. Canonicalized ordering. These quick solutions and planning items are preposterous. This is a deep plan."*

### EX3 — the type-inference + SOTA directive

> *"Why is struct projection not wired up? And even without an explicit `->` annotation, we should use our type inference system to infer the type thereof and project into a struct. We should mirror, though with superior ergonomics, sonic-rs's, simdjson's, etc — the SOTA — their get API to be as performant and UX friendly."*

### EX4 — the TS/WASM punt

> *"Ignore our TS and WASM backends for now, these are not relevant and will likely need to be fully re-engineered at some point (or can we leverage a shared ABI?)."*

### EX5 — the rich-profiling directive

> *"We likely should ground our analysis in richer profiling for full json, full l4 css, full google sheets, full bbnf — deploy an agent to profile each of these with full semantic parity — what semantic gaps exist that need to be filled (the aforesaid sonic-class API should be generalized for all grammars, of course), and what compile gaps are extant that need to be filled with the struct and project registry?"*

### EX6 — the 3000-commit question

> *"Why has project to struct failed to land for nearly 3000 commits? Assay our last several plans? What is the true, unambiguous path forward pursuant to gestalt and based on SOTA and better?"*

### EX7 — the no-execute, fully-specify directive

> *"Do not execute BA, but continue to clean up the above and refresh and FULLY specify the BA, BB, and BC tranches. We need to have these fully formed with no ambiguity, one at a time, with maximal wave-based clarity alongside maximal tranche.md clarity and specification, alongside triumvariate dispatch for scope increase/change as we implement."*

### EX8 — the substantive audit-task directive

> *"The task is to audit BA/BB/BC and our gestalt — how these tranches help to align our codebase into a cohesive whole and finally begin benching at a higher than sonic level, finally begin to have true, superior value/semantic ux to our competitors, finally reified our 3000+ commits of work."*

---

## §Failure-mode catalogue

### D1. Fictional-successor deferral

Routing chronic carries to a successor letter that does not exist in any plan document (cf. AZ-V). Corrective: every routed carry names a real letter in the trajectory, or stays in the current cycle.

### D2. Methodology-by-decoration

Dressing an audit cohort in evocative naming (polymath cohort, animal cohort, etc.) without matching a canonical methodology. Corrective: lanes are methodologically specific; each lane's exemplar is `docs/tranches/meta-audit/0N-*.md`.

### D3. Substrate-first / consumer-forward

A reform lands the substrate but routes the consumer to a future wave / future letter; the substrate metastasises as zero-caller surface. Corrective: substrate-with-consumer enforced in the same wave.

### D4. Union-tranche overload

A single tranche absorbs scope from multiple predecessor letters and produces a post-close audit cascade. Corrective: single-thesis tranche scope; sequence rather than absorb.

### D5. Profile-driven evidence retreat

Measuring a regression and *deleting the experiment*; not measuring under the production profile; routing the gap to a fictional successor instead of attributing to a mechanism. Corrective: samply 7-artefact contract per perf claim; environmental gating retires.

### D6. Phase-as-separate-phase framing

Naming a cleanup absorption as a separate phase between letters when it should be the leading wave's W0. Corrective: cleanup is a wave's scope; not an inter-letter phase.

### D7. Subproject-as-tranche-letter conflation

Naming a separate subproject as a tranche letter (cf. bbnf-buddy as "BC tranche"). Corrective: tranche letters are bbnf-lang code-tranche letters; subprojects track at their own location.

### D8. Doc-drift across plan surface

Tranche-status words drift between `<LETTER>.md` top-level, wave specs, FINAL, GESTALT, codegen-paths. Corrective: this brief's §Spec-Drift lane; the audit produces paste-ready amendments.

### D9. Scope-creep in audits

The audit examines subjects for drift but does NOT rewrite them in-place. An auditor tempted to "fix `<LETTER>.md` inline" is out of scope; the audit recommends; a separate pass applies.

### D10. Mode-slip — execution voice in an audit brief

Imperative-voice passages in an audit brief that imply execution against the arc rather than auditing. An auditor flags any sentence that appears to instruct execution against source code as a voice-leak finding.

### D11. Stale primary-source citations

Quoted exhortations whose transcript / commit / file:line has drifted. Flag as a priority finding.

### D12. Substrate-without-consumer declared MET

Declaring a hard gate MET when the substrate exists but no production caller does. Corrective: gate evidence requires a named consumer in production code.

---

## §Reading list

### Canon — top-level state

- `docs/GESTALT.md` — synthesis canon.
- `docs/codegen-paths.md` — pipeline diagram.
- `docs/HARDENING-AUDIT-PROMPT.md` — this brief.

### Canonical methodology

- `docs/tranches/meta-audit/{01..08}-*.md` — the 8-lane canonical methodology.
- `docs/tranches/meta-audit/ARCHIVE.md` — provenance of the bundle.
- `docs/tranches/meta-audit/archaeology/era-{II..VI}*.md` — era deep-dives.
- `docs/audit/archives/META-AUDIT-PROMPT.md` — predecessor canonical audit-only prompt (1774 lines, archived).
- Prior hardening syntheses under `docs/tranches/<predecessor>/audit/HARDENING-SYNTHESIS-*.md`.

### Predecessor cohort outputs (for the current cycle, AZ-IV → BA/BB/BC)

- `docs/tranches/AZ-IV/audit/DEEPX-{1..8}-*.md` — per-grammar profile + parity audits + spec landing.
- `docs/tranches/AZ-IV/audit/DEEP-{A,B,C,D}-*.md` — assay / profile / path / re-ordering.
- `docs/tranches/AZ-IV/audit/POST-CLOSE-{A,B,C,D}-*.md` — post-close audits.
- `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md` — canonical-ordering synthesis.
- `docs/tranches/AZ-IV/audit/SYNOPSIS-2026-05-03.md` — orchestrator-level recap.

### Runway specs (audit subjects)

- `docs/tranches/<LETTER>/{<LETTER>.md, waves/W{0..6}.md}` for each subject in §Audit subjects.

### Instructions — orchestration edicts

- `docs/precepts/instructions/{README.md,STYLE.md,ORCHESTRATION.md,CONSUMING.md,LESSONS-LEARNED.md}`.
- `docs/precepts/instructions/tranche/{SPEC.md,WAVE_SPEC.md,AGENT_DISPATCH_TEMPLATE.md,RESEARCH.md,CHALLENGE.md,DOC_UPDATE_WAVE.md,START.md}`.
- `docs/instructions/PROFILING.md` — samply 7-artefact contract canon.

### Tranche archaeology (for context)

- `docs/tranches/B0..B7/`.
- `docs/tranches/AT,AU,AV,AW-{I..V},AX-{I..III},AY-{I..III},AZ-{I..IV}/`.
- `docs/tranches/<letter>/historical/` archives where present.

---

## §Audit methodology — how to conduct and what to deliver

This section is the one prescriptive section of the brief. Every other section is descriptive.

### Workflow

1. **Baseline verification** (§Reading list). Open each listed artefact; note any missing, renamed, or content-shifted off this brief's summary. A missing canonical artefact is the first-priority finding.

2. **Invariant verification** (§Architectural invariants, §Decisions on record). For each invariant or decision, search the audit subjects for adherence; cite each finding's line.

3. **Discipline verification** (§Execution discipline, §Orchestration discipline, §Tone and voice). For each rule, audit the subjects + the latest two long-session transcripts for compliance.

4. **Scaffold population** (§Spec-Friction through §Substrate-Abrogation — the 8 lanes). Populate each lane's deliverable. An audit that leaves any scaffold in placeholder state is incomplete.

5. **Failure-mode recurrence check** (§Failure-mode catalogue). For each drift pattern (D1..D12), search for recurrences since the predecessor synthesis. Recurrences are high-priority findings.

6. **Brief-integrity sweep**. Scan this document for mode-slip, stale citations, and scaffolds whose purview has been outdated by arc evolution.

### Cohort dispatch

- 8 sibling worktrees: `bbnf-wt-harden-{01..08}` at master, `-b harden-NN`.
- Per-agent `CARGO_TARGET_DIR=<worktree>/target/harden-NN`.
- Each lane dispatches in parallel; HARD CAPs per §O3.
- Each lane writes one audit doc at `audit/HARDENING-<DATE>-NN-<name>.md`.
- After all 8 return, synthesise at `audit/HARDENING-SYNTHESIS-<DATE>.md`.

### Deliverable — synthesis report structure

The synthesis is a single file with these top-level sections:

1. **Cohort table** — per-lane agent / worktree / audit doc / commit / lines.
2. **Disposition by lane** — ACCEPT / NARROW / REJECT counts; paste-ready amendments produced.
3. **Cross-cutting themes** — patterns surfaced by ≥ 2 lanes.
4. **Paste-ready amendment blocks** — per audit-subject file affected, literal markdown to insert.
5. **Decision: leading subject's W0 ready, or another amendment pass needed** — single sentence; cite the gate that determines.
6. **Brief integrity** — mode-slip / stale-citation findings against this prompt itself.
7. **Routed carries** — per row, named close criterion + destination letter (not fictional).

### Scope boundary

The audit does not commit source code, dispatch agents against any tranche's runway, open tranches, or execute any wave. §O9 + §O11 bind the auditor as strictly as the orchestrator. The synthesis is the entire deliverable; redress is authorised separately by the user.

### Failure-mode warnings for auditors using this brief

(a) **Drift between brief and canon.** §Architectural invariants, §Decisions, and §Reading list are snapshots. If discrepancy emerges against current canon, canon is authoritative; the brief is the finding.

(b) **Stale scaffolds.** §Spec-Friction through §Substrate-Abrogation expect auditor-populated content. An audit that leaves any scaffold in placeholder state files that as a finding.

(c) **Quote provenance integrity.** Every quoted exhortation is verbatim. If a quote has drifted, flag as priority finding.

(d) **Prefix drift.** §1..§13 / §ED1..§ED11 / §O1..§O11 / §V1..§V4 / EX1..EX8 / D1..D12 numbering is load-bearing for cross-reference.

(e) **Mode-slip.** Read every imperative-voice passage critically.

(f) **Scope-creep.** Recommend the fix; a separate pass applies.

---

## How to use this audit briefing

Paste or load this brief into a fresh agent context (or hand it to a human auditor) at the bbnf-lang repo root. The receiver becomes the auditor. The auditor reads this brief to load the arc's current context; reads the canonical docs in §Reading list to ground each invariant / decision / discipline rule; populates the §Spec-Friction through §Substrate-Abrogation scaffolds per their deliverable shapes; checks failure-mode recurrence; performs brief-integrity sweep; produces the synthesis at `audit/HARDENING-SYNTHESIS-<DATE>.md` per §Audit methodology.

### Self-contained brief for the orchestrator

```
You are the orchestrator of the hardening audit cohort.
Repo: /Users/mkbabb/Programming/bbnf-lang.

1. Read this brief verbatim:
   docs/HARDENING-AUDIT-PROMPT.md
2. Read the canonical methodology:
   docs/tranches/meta-audit/{01..08}-*.md
   docs/audit/archives/META-AUDIT-PROMPT.md
3. Read the predecessor cohort outputs cited in §Reading list under
   "Predecessor cohort outputs", scoped to the current cycle.
4. Read the audit subjects (§Audit subjects table at the top of the
   brief).
5. Create 8 sibling worktrees:
   bbnf-wt-harden-{01..08} at master with -b harden-NN.
6. Dispatch 8 lanes in parallel using §Spec-Friction through
   §Substrate-Abrogation. Each lane gets the methodological exemplar
   at meta-audit/0N-*.md, the lane's audit purview / sources /
   deliverable shape, and the HARD CAP per §O3.
7. After all 8 return, synthesise at:
   audit/HARDENING-SYNTHESIS-<DATE>.md
8. Cherry-pick all 9 commits to master.
9. Apply paste-ready amendments to audit subjects where the
   synthesis prescribes. Re-validate cross-references with
   §Original exhortations verbatim.
10. Decide: leading subject's W0 ready, or another amendment pass
    needed?
11. Return a single completion message: state of subjects; commit
    hash of HARDENING-SYNTHESIS; W0 readiness signal; routed
    carries to the next letters with named close criteria.

Non-negotiables (per §ED1..§ED11):
- Audit-only contract: no source-code commits.
- File bounds: audit/ + tranches/<LETTER>/ only (the latter for
  paste-ready amendments).
- KISS / no workarounds / one path.
- Canonical lettering.
- Cite file:line evidence.
- Triumvirate per §13 + ORCHESTRATION.md.
- Memory at /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/
  is read; do not duplicate; update only if new feedback surfaces.
- Archaic diction is voice; preserve in user-quote register.
```

### Two quotes, as failure poles the arc has observed

> *"Sycophancy is to the orchestrator what backwards compatibility is to the engineer."* — corrective load-bearing (§V2).

> *"Substrate without consumer is half a fix; the half that survives metastasises."* — naming the chronic pattern.

---

## §Provenance

The 8 methodological lanes are the canonical pattern at `docs/tranches/meta-audit/01..08-*.md` (2026-04-22 bundle; cited live by AZ-IV.W0). The structural shape (audit-only contract; scaffolded vs reference sections; numbered invariants / discipline rules / exhortations / failure modes; reading list; methodology; self-contained brief) follows `docs/audit/archives/META-AUDIT-PROMPT.md` (the 2026-04-23 audit-only prompt that fed B1, B7, AZ-I, AZ-II planning; archived 2026-05-01). The current cycle's audit subjects, predecessor cohort, and baseline commit are recorded in §Audit subjects at the top of this brief. Subsequent cycles update §Audit subjects and the predecessor-cohort references in §Reading list; structure and reference sections are reusable verbatim.
