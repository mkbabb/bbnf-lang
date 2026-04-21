# Tranche Creation Specification

Normative rules for authoring and executing a tranche plan. Composes
`../README.md` (operational directives), `../PROFILING.md` (profiling
workflow), `RESEARCH.md` (research wave protocol), and `WAVE_SPEC.md`
(per-wave sub-document format) into a single tranche-authoring
workflow. Applies to every tranche from introduction forward.
Earlier tranches are not retroactively conformed.

Grounded in the 12-tranche retrospective corpus at
`docs/tranches/AW/audit/{AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV}-retro.md`
+ `SYNTHESIS.md`. Every rule below has an anti-pattern it
corrects.

## Prelude annexes

A tranche may be preceded by a **narrow execution-runway annex**
when, and only when, that annex shortens the blocked tranche's
execution loop directly.

Rules:

1. A prelude annex owns **no parity-critical runtime architecture**.
2. A prelude annex exists only to remove command-surface, build,
   bench, or profiling drag that would otherwise obstruct the named
   tranche's next wave.
3. A prelude annex must be **bounded**: normally 2-3 waves, no
   successor debt tree of its own.
4. The blocked tranche must name the annex explicitly in its wave
   schedule (`opens after B0 close`, etc.).
5. A prelude annex is not a refuge for hard work. If an item is
   required to hit the blocked tranche's close gates, it stays in that
   tranche.
6. If the annex's scope grows enough to compete with the blocked
   tranche, the plan is wrong and must be rewritten.

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
4. **Decide whether a prelude annex is warranted.** Default answer:
   no. Use one only when measured build/bench/profiling drag would
   otherwise block the next named wave of the tranche being authored.

## Plan structure

`{LETTER}.md` contains, in order:

1. **Opening paragraph** (≤10 lines) — what the tranche achieves.
   Imperative voice. No hedging. No meta commentary.
2. **Architectural thesis** — invariants the tranche enforces and
   why they compose.
3. **Invariants** — numbered; cross-tranche preserved, tranche-
   specific added.
4. **Wave schedule** — table: wave, agent count, workspace state
   at close, bench gate, and current status.
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
- Prelude annexes, when used, close before the blocked tranche's named
  wave opens. They do not run in parallel with that wave.
- **Workspace green at every wave boundary**, unless the plan
  declares intentional unworkability for a specific window
  (delete-then-swap). The unworkability window carries a named
  restoration wave.
- **Wave status updates at every boundary.** When a wave opens,
  closes, blocks, or is superseded, update both `PROGRESS.md` and
  the wave's `**Status**` line (plus the parent table's status
  column) in the same change set.

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
- **N-agent shared-file consolidation.** When four-plus agents
  write disjoint hunks of one file (`mod.rs` module declarations,
  a shared trait-impl block, etc.), per-commit cherry-pick can hit
  3-way merge conflicts on line-number shifts after the first
  commits land. Plan an orchestrator-led consolidation commit in
  that wave's schedule: the first two commits cherry-pick cleanly,
  remaining agents' work lands via direct orchestrator surgery on
  master with attribution to each agent's worktree. AW-I.W4β's
  `47496993` is the template. Don't discover this at integration
  time — name the consolidator in the wave's plan entry.

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

### Gate floor-check at plan time

A gate declaring a numeric threshold (line count, state count,
hit-rate, self-time reduction) MUST carry a floor check at plan
time. The plan author computes the minimum achievable value from
structural facts (e.g. `generated.rs`'s mandatory view-accessor
lines + prettify emission + const tables) and sets the gate above
that floor. AW-I gate 9 ("generated.rs ≤ 12000 lines") shipped
without a floor check — the real floor was ~19k from view
accessors + prettify. The gate was structurally unreachable and
reclassified at close as "plan miscalibration".

If the floor analysis is hard (the metric depends on code not yet
written), declare the gate as a soft-target + rationale-satisfied
fallback rather than a hard numeric. Don't ship a gate that can't
close.

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

**Pre-regen vs post-regen evidence.** A gate closable via
source-grep or `cargo expand` against the current `generated.rs`
is **pre-regen** and does not require a bootstrap cycle. A gate
demanding byte-identical regen output or the post-regen emission
of new parse-fn bodies is **post-regen**. Phrase each gate
explicitly; do not conflate. AX.W0a.2 sub-waves ran ~20 regen
cycles (~5 h wall) against gates that could have closed pre-regen.

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

### Three-tier command surface

Routine iteration, profiling preparation, and final-proof runs
are intentionally separated command surfaces; see
`../PROFILING.md` §"Public fast-path commands" for the canonical
alias/target manifest. Routine correctness rides the `ax-iter`
profile via `cargo iter-check` / `make iter-check` /
`make iter-test-{leaf,grammar,ws}` plus the `ay-expand-*` and
`ay-test-*` families — that is the surface plan authors and sub-
agents call during iteration. Profiling preparation rides
`cargo prep-bench` / `make ay-prepare-profile-wave` /
`make ay-samply-*` against the `profiling-prep` profile, reusing
the prebuilt binaries per §"Prepared binary reuse". Final-proof
bench and close-gate test runs ride `cargo final-bench` /
`make ay-bench-close WAVE=close` / `make test-close` /
`make final-bench` — this is the heavy surface, reserved for
wave-close evidence and closing-ceremony artefacts. Wave plans
and gate phrasings name the tier explicitly; a gate that silently
defaults routine work onto `cargo check --workspace`,
`cargo test --workspace`, or `cargo bench` is a miscalibration
against this three-tier discipline.

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

### Multi-pass tranche split (LETTER-I / LETTER-II / … / LETTER-N)

When a tranche's scope reveal under contact produces a clean split
— "here is what landed honestly; here is the gestalt re-ordered
rest" — use a Roman-numeral-suffix pass rather than consuming the
next letter. The first pass closes on what it landed (FINAL.md per
its hard-gate readout, including recorded misses where honestly
owned). The second pass opens as `{LETTER}-II` with its own plan,
PROGRESS, waves, audit, and FINAL. Subsequent passes (III, IV, …)
open on the same pattern; pass count is unbounded — the tranche
continues as long as the architectural thesis holds and forward
motion is demonstrable, and the numbering never caps.

Directory layout:

```
docs/tranches/{LETTER}/        # pass I; renamed to {LETTER}-I
                               # when pass II opens
docs/tranches/{LETTER}-II/     # pass II
docs/tranches/{LETTER}-III/    # pass III
…
docs/tranches/{LETTER}-N/      # unbounded successor passes
```

Letter advances to successor tranches (B0 → BA → BB …) only after
every open pass of {LETTER} closes. A tranche with open pass
{LETTER}-N blocks every successor's open gate.

Split vs new letter — decision rule:

- **Split**: the architectural thesis stays. Pass N completes or
  refines scope within the same thesis and invariants.
- **New letter**: the thesis changes. The new letter's plan would
  contradict or supersede the prior letter's invariants.

An audit triumvirate per §Diagnostic-loop relinquish decides which
mode fits at the reveal.

Audit artefacts that inform a successor pass live in the
successor's `audit/` directory, not the predecessor's. Pre-existing
in-flight audit docs (wave diagnostics authored during the pass
they executed under) stay with their authoring pass. Cross-pass
references cite the absolute audit path via
`../{LETTER}-II/audit/*.md` to avoid directory-rename churn.

A pass-II plan document is named `{LETTER}-II.md` (pass I's plan
is renamed from `{LETTER}.md` to `{LETTER}-I.md` at the split).
Subsequent passes follow the same suffix. Wave numbering restarts
at W0 per pass; cross-pass wave references cite `{LETTER}-I.W5`
explicitly when needed.

### Diagnostic-loop relinquish

A sub-agent that finds itself in a multi-cycle diagnostic loop
(three-plus iterations without a commit, or ~30+ minutes wall
time without forward motion) **halts, reports, and relinquishes
to the orchestrator**. Indefatigability binds the orchestrator,
not the individual sub-agent: an agent ground down in the same
loop is not executing the plan, it is stalling it.

The halted agent's deliverable is its state — probe tests
checked into its worktree, symptom trace in its return report,
draft-fix diff if any (left uncommitted so the orchestrator can
discard cleanly). The orchestrator receives the state and
dispatches a **research + plan + redress triumvirate**:

1. **Research agent** — reads every relevant source file, the
   saved `cargo expand` artefacts, prior tranche lineage, and
   the halted agent's probe tests. Produces a root-cause
   attribution document under `docs/tranches/{LETTER}/audit/`.
2. **Plan agent** — authors a concrete fix plan: file-level
   diffs, ordered change set, declared invariant impact.
3. **Redress agent** — executes the plan on a fresh worktree.

The relinquish is not a deferral; the triumvirate is the
resumption. Halting is the correct move when iteration is not
moving the problem forward; grinding is the incorrect one.

### Two valid scope-reveal response modes

AW-I demonstrated both. Name the mode at the moment of reveal;
don't drift between them.

- **Absorb.** The revealed work fits inside the existing wave
  schedule. Move it forward (reorder), split a wave, or fold into
  an adjacent wave whose scope already borders the revealed work.
  Plan documents revise in place; PROGRESS.md records the
  absorb. AW-I.W2.3's SCC-recompute activation surfaced snapshot-
  migration work; AW-I absorbed it by moving W4.5's migration up
  into a new W2.5 sub-phase, keeping W2's "workspace green at
  close" invariant intact.
- **New letter.** The revealed work requires a fresh wave
  schedule and can't be absorbed without silent deferral. Close
  the current tranche on what landed (FINAL.md + deferred-ledger
  with named destination), author `{NEXT_LETTER}.md`. AW-I.W4ζ's
  lowering-pipeline migration was too broad to absorb into W4;
  opened AW-II as the immediate successor. Where a predecessor
  plan already named a next letter for unrelated work (e.g. the
  AW-II-as-optimisation authorship before the AW-I reveal), the
  reveal's letter shifts the numbered successor forward (old
  AW-II becomes AW-III).

The discriminator is mechanical: can the revealed work land in a
declared or slightly-extended wave of the current tranche without
silent deferral? Yes → absorb. No → new letter. If the revealed
work spans both substrate AND consumer changes that together
exceed a single wave's complexity budget, prefer new letter.

### Root-cause discipline under cascading reveals

When three-plus layers of symptoms trace to one root cause, stop
patching each symptom and trace the full data flow. AW-I.W4ζ's
offset-72 failure was masked by three surface symptoms (walker
state machine error, tape shape mismatch, consumer decoder
failure) before the root cause — walker `variant_idx` left at 0
on rule-entry compounds — surfaced. The root cause gave a single-
commit fix; chasing symptoms would have shipped three
work-around commits and left the underlying bug. When you find
yourself patching at a new layer for the same observable failure,
pause and walk the producer → tape → consumer path end-to-end.

### Parallel-probe on > 2 candidate blockers

When a scope-reveal diag enumerates more than two candidate
architectural blockers, dispatch parallel probe-agents on
disjoint file bounds — one per blocker — rather than serially
patching the first. AX.W0a.2.d–.g probed one blocker per sub-
wave (LLVM cycle → Keyword Ref gap → four Flat walker-parity
deltas), consuming four sequential wave cycles for work that
three parallel agents could have diagnosed in one.

### Transitional fallback during elimination waves

An in-transit fallback path whose elimination is the tranche's
principal work is work-in-progress, not a workaround. The
one-codegen-path invariant binds at tranche close, not at every
wave close. AX.W0a kept walker fallback green across seven sub-
waves precisely to eliminate it in the eighth. A wave may revert
an admission-widening commit to preserve master-green while the
follow-on wave lands the consumer-side fix; the revert is
Absorb-mode, not deferral, when the follow-on wave is named in
PROGRESS.md at revert time.

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
- **Updates wave-status surfaces** (`{LETTER}.md` summary table +
  `waves/W<N>.md` status line) at the same boundary.
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
- **NO heavy-surface routine defaults.** Routine iteration runs
  on `iter-check` / `iter-test-{leaf,grammar,ws}` / `ay-expand-*`
  / `ay-test-*`; profiling prep on `profiling-prep` +
  `ay-prepare-profile-wave` + `ay-samply-*`; heavy close-gate
  proof on `ay-bench-close WAVE=close` / `test-close` /
  `final-bench`. `cargo check --workspace`, `cargo test
  --workspace`, and `cargo bench` are the heavy close-proof
  surface, not the routine one; see §"Three-tier command
  surface" under §Bench contract.
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
