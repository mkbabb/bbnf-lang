# Tranche Creation Specification

Normative rules for authoring and executing a tranche plan. Composes
`README.md`, `PROFILING.md`, and `RESEARCH.md` into a single
tranche-authoring workflow. Applies to every tranche from
introduction forward. Earlier tranches are not retroactively
conformed.

Grounded in the 12-tranche retrospective corpus at
`docs/tranches/AW/audit/{AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV}-retro.md`
+ `SYNTHESIS.md`. Every rule below has an anti-pattern it
corrects.

## Document set

A tranche owns a directory `docs/tranches/{LETTER}/`:

**Required:**
- `{LETTER}.md` — the plan. Written **before** any `{LETTER}.N`
  commit. `{LETTER}.md` is a hard gate; no commit with the tranche's
  identifier lands without the plan on master.
- `PROGRESS.md` — dated execution log. Every wave boundary adds
  an entry. Records what landed, what committed, what shifted.
- `FINAL.md` — closing document. Per-phase recap with commit
  hashes; hard-gate status table; deferred-item ledger with named
  destination tranches; cross-tranche debt reconciled.

**Conditional:**
- `research/NN-topic.md` — research wave deliverables. Required
  when the plan carries open-ended design space; waived with
  rationale in the plan's §"Research artefacts".
- `audit/*.md` — in-flight audits of inherited state or friction.
- `audit/{LETTER}-retro.md` — retrospective analysis, post-close.

**Benchmark artefacts** live at `docs/benchmarks/post-{LETTER}.json`
(aggregate) plus optional `post-{LETTER}-W{N}.json` per wave.

## Before authoring the plan

1. **Read predecessor's FINAL.md** — what deferred into this
   letter; what chronic debt carries forward.
2. **Run the research wave.** Dispatch 3–6 parallel research
   agents per `RESEARCH.md` template. Artefacts land in
   `research/` before the plan's waves dispatch. Waive with
   rationale if the design space is tight.
3. **Identify inherited chronic debt.** Items re-proposed across
   5+ predecessors (per `SYNTHESIS.md`'s ledger) either land as
   named phases or formally retire with rationale — no sixth-
   tranche drift.

## Plan structure

`{LETTER}.md` contains, in order:

1. **Opening paragraph** (≤10 lines) — what the tranche achieves.
   Imperative voice. No hedging. No meta commentary.
2. **Architectural thesis** — invariants the tranche enforces and
   why they compose.
3. **Invariants** — numbered; cross-tranche preserved, tranche-
   specific added.
4. **Wave schedule** — table: wave, agent count, workspace state
   at close, bench gate.
5. **Phases** — per-wave detailed sub-phases. Sub-phase IDs
   `{LETTER}.W.N`. Concrete: file paths, type signatures,
   pseudocode where useful, hard gate phrasing.
6. **Critical files** — table: file path, owning wave, purpose.
   Audited against data flow, not phase narrative.
7. **Hard gates summary** — per-wave gate enumeration. Each gate
   closes on runtime evidence.
8. **Cross-tranche debt** — items inherited + items forwarded
   with destination tranche.
9. **Escape clause** — if the plan carries intentional
   unworkability between waves, declared here with named
   restoration wave.

Omit: meta commentary, "we adopt this voice", "it should be
noted", style notes.

## Wave stipulation

### Parallelism

- **Max 6 parallel agents per wave.** Larger sets degrade
  orchestrator conflict-resolution.
- **Disjoint file bounds.** No two agents in the same wave share
  write access to the same file. `mod.rs` entries are "disjoint
  hunks" when each agent edits non-overlapping module-declaration
  lines.
- **File-bound audit at plan time.** Wave description enumerates
  each agent's allow-list; orchestrator verifies non-overlap
  before dispatch.
- **Maximise parallelism.** When a wave's work decomposes into
  independent file bounds, prefer more agents over fewer.
  Ceremonial 1-agent-per-wave execution of a plan that declared
  6-agent parallelism violates the plan.

### Sequencing

- Waves sequence by dependency, not letter order.
- **Workspace green at every wave boundary**, unless the plan
  declares intentional unworkability for a specific window
  (delete-then-swap). The unworkability window carries a named
  restoration wave.

### Agent briefing

Every sub-agent receives a self-contained prompt:
- Worktree path (pre-created by orchestrator)
- Sub-phase ID + scope
- **Allow-list** + **forbidden-list** file paths
- Research artefact citations (specific doc, not "see `research/`")
- Hard gates the sub-agent verifies before reporting done
- Commit-at-milestone cadence instruction
- Return format (word cap, required fields)

Sub-agents do not regenerate `generated.rs` unless explicitly
named as owner of the regen. Orchestrator owns the regen window.

### Commit discipline

- Sub-agents commit inside worktrees at every natural milestone.
- Orchestrator cherry-picks accepted commits onto master.
- **Master clean before every wave dispatch.** Cherry-pick-then-
  dispatch. No in-flight concurrency on shared files.

## Phase structure

Each sub-phase declares:

1. **ID**: `{LETTER}.W.N` (e.g., `AW.W2.1`).
2. **Owner file(s)**: concrete paths.
3. **Scope**: what lands, specifically.
4. **Hard gate**: runtime-verifiable assertion closing the sub-
   phase. Gate phrasing cites the verification tool (samply,
   `cargo expand`, bench delta, tape-walk test, parity harness).
5. **Commit message template**.

## Hard gates

A hard gate is **runtime-verifiable** or **artefact-verifiable**.
Closes on:

- A bench entry meeting a numeric threshold, or
- A test assertion passing, or
- `cargo expand` output containing a specific code pattern the
  gate quotes, or
- A samply profile showing a self-time delta or call-site
  attribution, or
- A byte-comparison against a reference (parity harness), or
- A structural artefact landing with named content (new module
  with a specific type signature).

A gate closing on "grep finds the string" without runtime evidence
is insufficient.

### Activation-gate rule

A substrate addition — new field, enum variant, trait, const —
requires a **same-wave consumer** that calls the addition in
production code, plus a hard gate verifying the call fires at
runtime. Source-grep gates are supplementary, not load-bearing.

Substrate-without-activation is the #1 chronic pattern across
AK–AV (`SYNTHESIS.md` §"Recurring anti-patterns" #1). Every
unpaired substrate addition compounds across tranches.

### Gate-off commits

A feature shipped behind a `false` default flag is a deferral,
not a commit. Plan-time decides activation state. No commit
lands with its activation gate disabled unless the plan declares
the deferred-activation explicitly with a named restoration
wave.

AP.1 shipped structural-dispatch with `structural_mode = false`;
AQ.5 deleted the infrastructure. Gate-off is debt, not pragmatism.

### Runtime-evidence clause

Gates cite the verification tool directly. `grep -c pattern
generated.rs` is insufficient when the code emitted might be
dead. Prefer:

- `cargo expand | grep -A N` showing call-site context.
- Samply self-time delta cited with artefact path.
- Bench entry with before/after numbers.
- Dedicated test that exercises the path and fails without it.

AT.1 Phase-1 passed its grep gate while every typed payload
capture was a dead store. The verification tool decides the
gate.

## Bench contract

Per-wave cold sequential bench runs are **structural**, not
optional. The plan's `## Wave schedule` table declares, per
wave:

- Per-wave bench gate (numeric threshold), or
- Aggregated-at-close gate (single bench at wave N close), or
- Rationale-satisfied (when the wave's lever does not move its
  target bench; escape clause with written rationale).

Bench-omission is the structural lesson of AV (V10 was the first
bench; 2.5–4.5× regression across every entry invisible until
close). Silent bench-omission is a violation.

## Scope-reveal protocol

When execution reveals the plan's scope was under-estimated:

1. **Default: re-plan with more agents.** Dispatch additional
   sub-agents on disjoint file bounds; split the wave. Carry
   plan-declared intentional unworkability across wave boundaries
   if the plan supports it.
2. **Escalate to user only** for hard environmental blockers:
   compiler bug, authorization boundary, irrecoverable state.
3. **Never retreat to additive shadow-surfaces** (e.g.,
   `parse_dta()` alongside `parse()`), single-probe stubs,
   `Unsupported` branches, or "substrate only" landings that
   leave the consumer unwired. These are architectural debt
   dressed as pragmatism.
4. **Mid-tranche plan pivots open a new letter.** If the scope
   shift is too large for the current tranche to absorb without
   silent deferral, close the current tranche on what landed and
   author `{NEXT_LETTER}.md`. `SYNTHESIS.md` records AR
   (audit-driven replan kept under AR/) and AS (mid-stream re-
   plan of AR-audit leftovers) as the anti-examples.

## Closing ceremony

A tranche closes when:

1. `FINAL.md` exists under `docs/tranches/{LETTER}/`.
2. `docs/benchmarks/post-{LETTER}.json` covers the matrix (or is
   rationale-satisfied per the plan's escape clause).
3. `cargo test --workspace --no-fail-fast` returns 0 failures at
   master after all tranche commits land.
4. Every invariant in `{LETTER}.md` verified with artefact
   citation in FINAL.md.
5. Every planned item landed (commit hash) or appears in FINAL's
   deferred ledger with named destination tranche + rationale.

Intentional-unworkability windows declared in the plan's escape
clause lift before close.

## Orchestrator role

The orchestrator:

- **Owns master's HEAD.** Sub-agents never modify master directly.
- **Dispatches sub-agents** per the wave schedule.
- **Verifies sub-agent claims** against saved artefacts before
  cherry-picking. Trust-but-verify. When a claim and an artefact
  disagree, the artefact wins.
- **Cherry-picks** accepted commits; discards unaccepted with
  rationale.
- **Pre-creates worktrees** per wave.
- **Delegates implementation, test runs, profiling, bench,
  diagnostics, doc authoring** to sub-agents.
- **Preserves own context** for: orchestration, cherry-picking,
  scope management, agent-claim hardening, synthesis of sub-
  agent reports into plan adjustments, re-planning under
  contact.
- **Updates `PROGRESS.md`** at every wave boundary.
- **Handles cross-crate work** (sibling-repo patches to
  `parse-that/`, `pprint/`) from main when worktree isolation
  prohibits sub-agent scope.
- **Does not relinquish control** until closing ceremony
  satisfied.

The orchestrator does not run long benches, read 1000+-line
docs top-to-bottom, perform large-file greps, or execute test
suites. Those operations delegate to sub-agents with focused
context.

## Edicts — inherited + tranche-authoring-specific

Every tranche inherits `README.md` §"Code discipline". Key for
tranche authoring:

- **NO stubs at close.** Placeholder arms, single-probe
  implementations, `Unsupported` branches — none survive tranche
  close.
- **NO shims.** No additive `*_dta()`-style surfaces shadowing
  the primary path.
- **NO silent deferrals.** Every non-landed planned item appears
  in FINAL's deferred ledger with destination tranche.
- **NO gate-off commits.** Feature-flag-off shipping is
  deferral.
- **NO ghost tranches.** `{LETTER}.md` lands before any commit
  with `{LETTER}.N` identifier.
- **NO label collisions.** A tranche identifier belongs to one
  plan; mid-stream pivots open a new letter.
- **NO god modules.** At every level — crate, module, file —
  separate concerns.
- **NO hard-gate-via-grep.** Runtime evidence required.
- **NO ceremonial wave structure.** Declared waves execute.
- **Execute the plan.** Scope-reveal under contact is re-plan-
  with-more-agents, not escalation.
- **Indefatigability.** Orchestrator does not relinquish control
  until closing ceremony satisfied.

## Checklist — ready to dispatch wave N

- [ ] `{LETTER}.md` on master; wave N declared in schedule.
- [ ] Wave N's sub-agents enumerated with file allow-lists.
- [ ] File allow-lists verified disjoint within the wave.
- [ ] Research artefacts landed for wave N's consumers.
- [ ] Master clean (`git status --short` empty).
- [ ] Worktrees pre-created for wave N's sub-agents.
- [ ] Sub-agent prompts drafted as self-contained briefings.
- [ ] Hard gates for wave N phrased as runtime-verifiable
  assertions.
- [ ] Bench contract for wave N declared (per-wave / aggregated /
  rationale-satisfied).

## Checklist — ready to close tranche

- [ ] Every phase landed with commit citations, or appears in
  FINAL's deferred ledger with destination.
- [ ] Every invariant verified with artefact citation.
- [ ] Every hard gate closed with evidence (bench / expand /
  samply / test).
- [ ] `docs/benchmarks/post-{LETTER}.json` exists and covers the
  matrix.
- [ ] `cargo test --workspace --no-fail-fast` 0 failures.
- [ ] `FINAL.md` composed and committed.
- [ ] `PROGRESS.md` has close entry with tranche HEAD commit.
- [ ] All tranche-specific worktrees removed.
- [ ] No `#[allow(...)]` added to mask tranche work.
- [ ] No `#[ignore]` added beyond documented Category A items.
