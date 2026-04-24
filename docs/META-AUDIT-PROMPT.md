# bbnf-lang — Begat Meta-Audit Prompt

A comprehensive audit, archaeology, and gestalt brief of the
bbnf-lang system across its ~2000-commit history. Authored per the
user's 2026-04-23T16:01:55Z exhortation ("Relay a prompt to me to
handoff to an auditor for every facet herein"). The brief's purpose
is to let an auditor — with no access to prior sessions —
reconstruct the arc, verify present-state canon for drift, catalogue
the arc's architectural invariants and abandoned paths, and produce
an audit report that grounds the next audit.

**This document is audit-only.** Ingestion and audit-report
production are the entire task it prescribes. Commits, dispatches,
worktree creation, tranche execution, or any other state mutation
against the arc are out of scope. If any passage below appears to
instruct such action, the document is wrong — audit only (do not
act), and flag the voice-leak as a finding in the audit report.
Execution authorisation comes from the user, in a subsequent and
separate pass; never from this brief.

Every quoted directive below is extracted verbatim from the three
source transcripts enumerated in §Provenance; the synthesis lives
in the categorization, not in any rewording. The transcripts, the
quoted user material, and the arc's canonical docs (`GESTALT.md`,
`RISK-PERF-MATRIX.md`, tranche specs, benchmark JSONs, instructions
edicts) are the audit's primary-source corpus — the brief cites
them; the audit verifies current state against them.

---

## Preamble — what you are auditing

You are auditing a bbnf-lang arc whose load-bearing shape is: one
grammar surface, one IR substrate, one parse path, one semantic
surface, derived — never hardcoded — from the grammar; optimised by a
pluggable CSP solver and a pluggable e-graph; emitted through backend
emitters to Rust / TypeScript / WebAssembly; measured against sonic-rs,
simdjson, and lightningcss on the published competitor harness. The
arc has passed through a DTA/PSI rut (Era V, ~572 tranche-tagged
commits, zero bench recovery); through AX.W0b, which deleted ~78,000
LOC of interpreter substrate; through AY-I, which landed column revert
and honest relinquish; and through an AY-II pause at W0' triggered by
the B1 prelude. The meta-audit at `ea0c826d` established the plan
surface was not yet execution-clean. The redress commits that followed
— removing `bbnf-tape-mini` from the planning surface, normalising
`GESTALT.md`, demoting `ir-rewrites` from crate to module, and
rewriting AZ as AZ-I + AZ-II — closed the execution surface. The
current sequence is **B1 → AY-II.W0' close + W1-W5 → AZ-I → AZ-II →
BA → BB**, with BB running parallel to AZ-II where disjoint.

The audit baseline is: master HEAD at or beyond `56a67e2e` (see
§Reading list); eight axis reports under
`docs/audit/meta-audit-2026-04-23/`; a tranche tree under
`docs/tranches/` in which B1, AZ-I, AZ-II, BA, BB are authored and
wave-split; `GESTALT.md` and `RISK-PERF-MATRIX.md` as current-state
canon; and `docs/instructions/` holding `README.md`, `PROFILING.md`,
`CHANGELOG.md`, and `tranche/` sub-edicts that govern orchestration.
The audit verifies the baseline has not drifted since synthesis; any
drift becomes a finding in the audit report.

This document is an audit brief, not documentation, and not an
action signal. It records the arc for the auditor's inspection; it
does not authorize continuation of the arc.

The user's archaic diction is deliberate voice, not AI artefact —
*begat, therein, thereof, insofar, hereof, hitherto, appurtenant,
assay, gestalt, indefatigably, thereupon, obstinancy, degust,
Herculean, befitting, exhortation, edict, explicate, divine,
parsimonious* — preserved verbatim in quotes throughout this brief.
The audit report preserves the same register when quoting user
material.

### Reading the brief

The audit workflow is fully specified in §Audit methodology; the
orientation and failure-pole quotes are in §How to use this audit
briefing. Between this Preamble and those closing sections, the
brief is divided into:

- **Scaffolded sections the audit populates** — §Gestalt,
  §Archaeology, §Performance arc, §Abandoned paths,
  §External integration, §Open contradictions. Each carries its own
  purview, sources, and deliverable shape; the audit report
  populates each per those shapes.
- **Reference sections the audit cites against current canon** —
  §Architectural invariants on record, §Execution discipline on
  record, §Orchestration discipline on record, §Tone and voice on
  record, §Runway at synthesis, §Decisions the prior audit baked
  in, §Original exhortations, §Failure-mode catalogue,
  §Reading list, §Historical session record.

A fresh auditor may read top-to-bottom, or jump straight to §Audit
methodology for the workflow and populate scaffolds with the
reference sections open as lookup.

---

## §Gestalt — the system the arc is converging on

### Audit purview

A single-page synthesis of what the bbnf-lang system looks like in
totality once the runway is complete: one grammar surface; one IR
substrate; one parse path (no second parse in `to_value()`); one
semantic surface grammar-derived (no hardcoded semantics); the CSP
solver and the e-graph as pluggable optimisation substrates; the VM
as bounded oracle on residue only; backend emitters (Rust /
TypeScript / WebAssembly; Python via PyO3 where declared); direct-
to-struct projection with the tape physically abrogated; parity or
superiority against sonic-rs, simdjson, lightningcss on the
published competitor harness; sibling repos (parse-that, pprint)
pinned and path-patched.

### Sources the auditor consults

- `docs/GESTALT.md` — primary current-state synthesis.
- `docs/RISK-PERF-MATRIX.md` — per-grammar perf marks + per-tranche
  landing probability.
- `docs/tranches/B1/` through `docs/tranches/BB/` — the runway's
  owning specs; each declares how it advances the gestalt.
- Runtime code under `crates/` — the realised shape. One-path can be
  verified via `rg to_value` for reparse sites; grammar-derived
  semantics via absence of hardcoded switches in backend emitters.
- `docs/benchmarks/` — competitor parity / superiority evidence.

### Deliverable shape

A gestalt-drift ledger: for each element of the unified shape
above, does current canon realise it (YES / PARTIAL / NO), cite the
evidence, and if PARTIAL / NO name the outstanding tranche that
would close the gap.

[auditor fills this during audit]

---

## §Archaeology — era-by-era trace

### Audit purview

The ~2000-commit history broken into its eras (AA through the
current B-series), with each era's thesis, scope, commit range, key
pivots, retired subsystems, and landing outcome. The arc does not
move forward without knowing what it came from; the audit verifies
the era record against git log and the tranche archaeology under
`docs/tranches/`.

### Sources the auditor consults

- `docs/tranches/AA.md` through `docs/tranches/AX.md` — Eras I–V
  archival records.
- `docs/tranches/AY-I/FINAL.md` — honest-relinquish record for the
  `note_push` experiment.
- `docs/tranches/AY-II/`, `docs/tranches/AZ-I/`,
  `docs/tranches/AZ-II/`, `docs/tranches/B0/`, `docs/tranches/B1/`,
  `docs/tranches/BA/`, `docs/tranches/BB/` — active-era owning specs.
- `docs/tranches/meta-audit/` — the meta-audit bundle that launched
  the redress sequence.
- `git log --oneline` constrained to per-tranche commit-SHA anchors
  found in each tranche's `PROGRESS.md` / `FINAL.md`.
- The three source transcripts (§Historical session record) —
  user-voice record of era transitions.

### Deliverable shape

A chronological table: **Era | Thesis | Commit range | Key pivot |
Retired on exit | Landed on entry | Carry-forward lesson**. One row
per era. Special attention to the DTA/PSI rut (Era V, ~572 tranche-
tagged commits, zero bench recovery — the single longest
unrecovered pivot in the arc's history) and the AX.W0b interpreter
deletion (~78,000 LOC).

[auditor fills this during audit]

---

## Architectural invariants on record

The system's shape is declared — not negotiated. Every invariant
below is verbatim from a user message; the categorization is the
synthesis. The audit verifies each invariant still holds in current
canon (`GESTALT.md`, tranche specs, runtime code); any invariant
that is violated, softened, or silently dropped is a finding.

### §1. One path, not two

> "`AY-II` must close on one path. No second parse hidden inside
> `to_value()`. No consumerless substrate surfaces. No JSON-only parity
> close. CSS typed semantic parity becomes a hard close obligation
> inside `AY-II`, not `BA`."
> — session 32a, 2026-04-21T07:15:40Z

> "`Option 2`, in `AY-II`, not `BA` if `Option 2` means: one parse, one
> canonical substrate, and semantic construction fused into that same
> parse path. Not: `Parsed::to_value()` reparses through a separate
> visitor path. If beating `sonic-rs` is still parity-critical, this
> cannot be deferred to `BA`."
> — session 32a, 2026-04-21T07:15:40Z

### §2. Grammar-derived, not hardcoded

> "ALL semantic information is grammar derived. Nothing is hardcoded."
> — session 32a, 2026-04-21T07:15:40Z

> "we want to become a SOTA union of the best of what we've done in the
> last 2000 commits with RD, sonic-rs in their actual begotten code,
> simdjson and their tape/projection/direct to struct and value,
> lightning css to have absolute, grammar DERIVED AND NOT HARDCODED,
> semantic parity: we should robustly, richly, derive everything from
> the grammar as we can, using our IR pass system, type inference
> system, CSP/egraph system for rule and type inference"
> — session 959, 2026-04-23T00:10:32Z

### §3. Full tape abrogation is a hard requirement

> "Full tape abrogation is a hard requirement. Is direct to struct
> projection not the biggest and most important performance gain we'll
> see? We're even getting off-track within this planning phase. What is
> your obstinancy against actual, difficult, architectural change and
> improvement? Why even propose that--seriously, I'm not mad, just
> curious."
> — session 959, 2026-04-23T15:53:32Z

> "Can we not derive the needed struct shapes for BBNF and abrogate the
> tape entirely? Ideally, B would be my preferred."
> — session 959, 2026-04-23T04:39:39Z

### §4. No consumerless substrate; structural scan must be same-path

> "Structural scan should be first-class, but not unconditional eager
> whole-input admission. The right rule is: universally available
> substrate/service, grammar-derived activation, fact-driven per
> grammar/rule/shape use. No hand-routed grammar specialization, no
> mandatory global prepass tax."
> — session 32a, 2026-04-21T07:15:40Z

> "The structural scan lesson from `AY-I` should be inverted: not
> 'dead, retire,' but 'must become a same-path production surface or
> fail AY-II.'"
> — session 32a, 2026-04-21T07:10:36Z

### §5. Projection totality as a hard gate

> "`PROJECTION_DIRECT_TO_STRUCT.len() == materialize_projection_*
> helper count == runnable consumer count`. No resolver shim without
> an executable reader path. This is especially important for JSON
> `String` and CSS `Color`."
> — session 32a, 2026-04-21T07:10:36Z

> "Every projection admission has a runnable helper and a production
> consumer."
> — session 32a, 2026-04-21T07:10:36Z

### §6. First-class multi-grammar parity

> "We should focus on full semantic parity, and GRAMMAR derived parity
> using our rich type infernce system via csp/egraph, etc. BBNF, Sheets,
> and CSS L4 should be prioritized just as much, profiled just as much,
> expanded just as much. First class."
> — session 32a, 2026-04-21T07:15:40Z

> "No. We need more waves to properly spec out and define the semantic
> parity and optimizations aforesaid for: JSON, CSS L4 (lightningcss
> total parity, really, without hard coding or hacks), Google Sheets,
> and BBNF. Performance should be concentrated, generalized gestalt,
> and totally grammar defined, from the above."
> — session 32a, 2026-04-21T07:30:17Z

### §7. Typed CSS semantic parity is keyed to lightningcss, not internals

> "Typed CSS coverage should be keyed to `lightningcss` semantic
> surface, with named source-backed parity tests, not just an internal
> named-type list. Internal named/projection lists are necessary
> wire-contract proof. They are not sufficient close criteria. The
> close criteria should be semantic parity on real typed surfaces:
> color, declarations, values, selectors where applicable."
> — session 32a, 2026-04-21T07:15:40Z

### §8. `ir-rewrites` is a module, not a crate

> "ir-rewrites does not deserve its own crate; redress the above.
> probability this suceeds and lands for each wave, tranche, etc? perf
> marks at each juncture, for each grammar?"
> — session 959, 2026-04-23T05:21:53Z

### §9. Grammar-specific rules are colocated, not core-crate

> "Grammar specific rules must be stored in a way that allows for
> extensbility, though. Something that can be colocated with a grammar
> in a standardized and modular fashion. We'll ship a few, but this
> should be extensible. And not stored in the core crate. Automatic
> ranking system is good."
> — session 959, 2026-04-23T04:45:40Z

### §10. VM is bounded oracle residue

> "The VM has a reasonable throughput, and has been optimized pretty
> generously--but why is this being used for an oracle equivalence when
> our egraph system should do the same thing?"
> — session 959, 2026-04-23T04:39:39Z

> "VM approach is good."
> — session 959, 2026-04-23T04:45:40Z

(The resolution: VM stays as the bounded ground-truth oracle on residue
that the e-graph cannot prove equivalent. Not a parallel runtime; not
a consumerless surface. BB spec governs this.)

### §11. No eager whole-input prepass tax

> "Structural scan should be first-class, but not unconditional eager
> whole-input admission."
> — session 32a, 2026-04-21T07:15:40Z

### §12. Composite cache keys + robust invalidation

> "Composite key. Ensure robustness and performance. This has been a
> sticking point for many commits and we need proper cache
> invalidation."
> — session 959, 2026-04-23T04:39:39Z

### §13. BA.W2.a must be researched, not improvised

> "BA.W2.a sounds like a recipe for disaster and divergence. This
> should be prepared for and planned for with detailed research."
> — session 959, 2026-04-23T04:39:39Z

---

## Execution discipline on record

The user repeats these in nearly every substantial directive. They
are the admission criteria the arc held work to. The audit verifies
the arc's sessions abided by them — each violation surfaced in
commit history, session transcripts, or canonical-doc drift is a
finding.

### §ED1. No quick solutions, no workarounds

> "NO quick solutions, NO workarounds: idiomatic, gestalt approaches.
> This is a development product, architectural transpositions in the
> sake of elegance, simplicity, and performance above all are both
> necessary and desirable. NO legacy code."
> — session 32a, 2026-04-21T05:53:23Z (first appearance)
> — session 4be, 2026-04-21T18:09:10Z (repeated)
> — session 959, 2026-04-23T00:10:32Z (repeated)

### §ED2. No deferrals, no partial analysis

> "No deferrals. No need to get a performance or testing baseline."
> — session 4be, 2026-04-21T07:16:13Z

> "Continue. Re-send out the agents if need be. No partial analysis."
> — session 4be, 2026-04-21T18:30:35Z

### §ED3. No un-wired legacy cruft, no dead IIFEs

> "Ensure no un-wired legacy cruft, too. Path B. Pivot. Transient
> history if relevant. NO dead IIFEs. NO workarounds. Audit our
> original tranches plans to ensure we're not building up deferrals
> and legacy debt."
> — session 4be, 2026-04-21T18:51:58Z

### §ED4. No patching generated.rs by hand

> "We cannot patch the generated.rs like this. Status?"
> — session 4be, 2026-04-21T18:06:44Z

(Generated files are always output of fresh regen — never hand-patched.
Transient compose-escape aliases are permissible only when SPEC
§Self-host circular-dependency-escape admits them and they are retired
post-regen.)

### §ED5. Architectural transpositions for elegance, simplicity, performance

> "This is a development product, architectural transpositions in the
> sake of elegance, simplicity, and performance above all are both
> necessary and desirable."
> — recurring across all three sessions

### §ED6. AZ-II is not optional; the escape is not a planning alternative

> "The AZ-II floor question is no longer open. Your prior requirement,
> 'full tape abrogation is a hard requirement,' should be treated as
> binding repo policy. `bbnf-tape-mini` should be removed from the
> planning surface in [AZ-II.md], [RISK-PERF-MATRIX.md], and
> [GESTALT.md]. If AZ-II.W2 actually fails in practice, that becomes a
> contingency decision then, not a declared floor now."
> — session 959, 2026-04-23T18:04:47Z

### §ED7. Merge all parallel work; double-validate

> "Merge them all in, there should be no conflicts. And help me to
> understand and properly explicate for each question. We still have a
> VM? Rule curation what? Gorgeous-mirror what? Monolithic arena
> combinator fall back what? We shouldn't have an arena anymore,
> right? And then once merged in--double validate--clean up the
> worktrees."
> — session 959, 2026-04-23T04:07:09Z

### §ED8. Output must be complete, not parsimonious

> "For such a large scale report, i cannot fathom why you're so
> parsimonious with output and reporting."
> — session 959, 2026-04-23T00:40:00Z

> "Our updates, augmentations, and refinements must not be half baked.
> Eschew ad hoc scripts for benching and testing, profiling, any tool
> chaining—unless genuinely valuable and not subsumed by a more modern
> and idiomatic approach"
> — session 959, 2026-04-23T00:40:00Z

### §ED9. Ensure ALL sub-waves align — no drift across docs

> "Ensure ALL sub-waves are aligned."
> — session 959, 2026-04-23T18:25:03Z

> "Ensure all appurtenant doc items are updated. Any stale docs
> removed, too."
> — session 32a, 2026-04-22T19:54:30Z

### §ED10. Kill ongoing rustc processes; first-principles assay

> "This has never taken so long previously--before our B0 changes,
> which were supposed to optimize this process, our builds were long
> but at least ocassionally reasonable. This needs to be rethought
> from first principles. No excuses or 'each has its own derive macro'
> expansion. Kill all ongoing rustc process and properly assay and
> addressed."
> — session 4be, 2026-04-22T15:20:00Z (and repeated 959, 2026-04-23)

### §ED11. Zombies are not scope expansion

> "That's not scope expansion, that's zombie tasks. Kill the processes
> if they're not needed then."
> — session 4be, 2026-04-21T18:09:10Z

---

## Orchestration discipline on record

These are the conduct rules the orchestrator ran under. The audit
verifies they were observed across the dispatched sessions
documented in §Provenance; breaches are findings.

### §O1. Parallelize — 4 agents minimum for audits, 8 for Herculean scope

> "This is a massive task that must be properly orchestrated. Deploy at
> the minimum 4 agents in parallel: long horizon and deeply develop,
> research, assay, and synthesize into a SERIES of plans, findings,
> updates, and a gestalt unified, SOTA, compiler and mathematical
> optimization theory backed whole."
> — session 959, 2026-04-23T00:10:32Z

> "Deploy a fleet of 8 agents in several waves to properly degust and
> process this Herculean task. Deep meta analysis."
> — session 959, 2026-04-23T00:40:00Z

> "Deploy 4 agents in parallel to perform this meta audit such that we
> create a series of updates to any and all items (ensuring context
> and scope engineering) for instructions/ tranches/, all extant
> tranche plans, B1, alongside meta-learnings hereof."
> — session 4be, 2026-04-22T18:39:48Z

### §O2. Triumvirate on blockers (research + plan + redress)

> "When an agent faces a blocker like this, let's update our README
> and instructions edicts where befitting to have a clause that states:
> when blocked and debugging in too long of a task, that agent should
> relinquish control back to the orchestrator and a triumvariate set
> of research + plan + redressing agents should be deployed adhoc."
> — session 32a, 2026-04-21T05:53:23Z

> "Dispatch the proper Triumvirate research, plan, and redress team on
> this with much tighter bounds. All tasks should be updated now,
> progress written, status updated."
> — session 959, 2026-04-23T00:10:32Z (recurring)

### §O3. Hard caps on every dispatch

(Enforced by the orchestrator's own dispatch template — MEMORY
`feedback_dispatch_hard_cap`: research/plan/redress = 20/15/30 min
defaults; every dispatch carries "HARD CAP: N min. At 0.9N commit, at
N halt".)

### §O4. Worktree isolation; commit before parallelizing

(From MEMORY `feedback_agent_orchestration`: never let sub-agents race
on shared files; commit before parallelizing; use worktrees for
overlap. Sessions 4be and 32a demonstrate multiple instances of
worktree-detached-HEAD commits cherry-picked into master after parallel
audit.)

### §O5. Status ticks every ~5min of orchestrator-silent wait

> "Status. All hitherto."
> — session 32a, 2026-04-21T04:35:08Z

> "What is taking so long here? Status"
> — session 4be, 2026-04-22T01:34:28Z

(The user expected proactive status ticks during orchestrator-silent
waits. The audit verifies dispatched sessions offered status before
the user asked — any session in which the user asked twice is a
finding.)

### §O6. Scope-pivot opens a new tranche letter + new docs/tranches/XX.md

> "AY should remain as it is, but our missing AZ should become this new
> AY-II+ and BA--BB then becomes BA, And BC becomes BB."
> — session 959, 2026-04-23T04:45:40Z

> "We should open up a new AY-II pass. Synthesize what's needed, update
> both AY and AY-II with needed waves, etc. Have the edicts been
> updated? Is the sub-agent still viable? AY should be split into
> AY/I, II--current is I, and then the remaining, synthesized, gestlate
> re-ordered items placed into II with audit/, waves/ PROGRESS--update
> our instruction edicts, too, to handle this sort of multi-tranche
> tranche."
> — session 32a, 2026-04-21T06:31:10Z

### §O7. Split AZ into I + II; streamline docs thereupon

> "Reasonable. Let's split AZ into I and II waves. Thereupon, run the
> docs/ streamlining pass."
> — session 959, 2026-04-23T15:21:23Z (and 15:33:59Z, re-sent after
> interrupt)

### §O8. Continue indefatigably

> "You have enough here to update everything. Continue indefatigably
> through all doc updates."
> — session 959, 2026-04-23T04:45:47Z

> "Continue. Re-send out the agents if need be. No partial analysis."
> — session 4be, 2026-04-21T18:30:35Z

### §O9. Do NOT start the plan when agents return; refine together

> "do NOT start the plan when all agents return. We'll refine the plan
> together, too."
> — session 32a, 2026-04-21T06:08:07Z

> "Do not immediately execute the plan thereupon."
> — session 4be, 2026-04-21T18:09:10Z

### §O10. Re-deploy; no limits

> "Re-deploy them. No limits."
> — session 959, 2026-04-23T03:16:25Z

> "continue. re-deploy."
> — session 4be, 2026-04-22T14:30:03Z

### §O11. Boundedness — stop auditing the audit once converged

> "The infinite-regress boundary should be explicit: after this
> redress, stop auditing the audit and execute. A further meta-layer is
> only justified if it finds a new concrete contradiction, not merely
> restates existing ones."
> — session 959, 2026-04-23T18:04:47Z

---

## Tone and voice on record

### §V1. Archaic diction is deliberate

The user's vocabulary — *begat, therein, thereof, insofar, hereof,
hitherto, appurtenant, assay, gestalt, indefatigably, thereupon,
obstinancy, degust, Herculean, thereof, befitting, exhortation,
edict, explicate, divine, parsimonious* — is deliberate voice, not
AI artefact. The arc preserves it verbatim when quoting; the audit
verifies post-synthesis summaries and paraphrases of user material
have not smoothed it out. A smoothed paraphrase in canonical docs
or session output is a finding.

(See auto-memory `feedback_archaic_diction`.)

### §V2. Independence over obedience

> "What is your obstinancy against actual, difficult, architectural
> change and improvement? Why even propose that--seriously, I'm not
> mad, just curious."
> — session 959, 2026-04-23T15:53:32Z

The user values an orchestrator that argues back on technical
merit, not one that rubber-stamps softened plans. A defensible
floor is a defensible floor, not a stopping point. The audit
verifies post-correction orchestrator behaviour held the technical-
independence line — an orchestrator that caved on a defensible
floor after user pushback is a finding.

### §V3. Corrective is load-bearing

When the user caught drift, the arc's orchestrator internalised the
correction rather than defending the prior framing. Example:

> "The critique is correct. The meta-audit's macro verdict stands, but
> its execution framing should be tightened in five ways."
> — session 959, 2026-04-23T18:04:47Z

This illustrates the voice of post-correction integration. The
audit verifies post-correction sessions held this voice — drift
back into defence-of-prior-framing in the turns following a user
critique is a finding.

### §V4. Senior performance engineer's judgment is the frame

> "Thoughts on the following. Judge fairly, from a senior performance
> engineer's perspective. Gestalt changes and architectural
> transpositions are likely required, when befitting"
> — session 32a, 2026-04-21T07:10:36Z

---

## Runway at synthesis — forward plan snapshot

The forward-facing plan as declared at synthesis. Dependencies run
left-to-right; BB may overlap AZ-II where file-bounds are disjoint.
Each tranche's owning spec is cited. The audit compares this
snapshot against current canon (`GESTALT.md`, tranche specs,
`PROGRESS.md` files) and records any sequencing drift, scope
expansion/contraction, or silent re-ordering.

### B1 — dev-loop truth (4 waves, ~1 week)

Headline: pin the toolchain, migrate bench harness to divan, rewire
CI, re-anchor measurement on a clean substrate. Opening prerequisite:
none — B1 is the master critical path. Owning spec:
`docs/tranches/B1/B1.md`, waves under `docs/tranches/B1/waves/`,
agent routing in `docs/tranches/B1/AGENT_DISPATCH.md`. Adjacent:
`docs/tranches/B1/TOOLCHAIN-SOTA.md` + `TOOLCHAIN-MIGRATION.md`.

User constraint:

> "refine B1, with the ACTUAL EDIT: such that we properly fix toolchain
> timing for building, cargo checking, expanding, benching, testing to
> facilitiate RAPID development. We may need to takle an entirely
> different approach, micro benching, micro testing, micro building"
> — session 959, 2026-04-23T00:10:32Z

### AY-II.W0' close + W1–W5 resume (~2 weeks after B1)

Headline: FusedBuilder collapse, projection totality, structural-scan
same-path consumer, typed CSS parity keyed to lightningcss. Opening
prerequisite: B1 close (measurement infra). Owning spec:
`docs/tranches/AY-II/`, with the pause snapshot at `W0p-PAUSE-SNAPSHOT`
and waves W0'/W1-W5.

User constraint:

> "`Parsed::to_value()` must not reparse."
> — session 32a, 2026-04-21T07:10:36Z

### AZ-I — grammar-derived direct-to-struct for JSON + CSS L4 + Sheets (2-3 weeks)

Headline: `StructRegistry` closure, JSON twitter recovery to ≥ 1967
MB/s. Tape retained for BBNF only. Opening prerequisite: AY-II close.
Owning spec: `docs/tranches/AZ-I/AZ-I.md` + waves/PROGRESS.

### AZ-II — BBNF self-hosting direct-to-struct + `crates/tape/` deletion (1-2 weeks)

Headline: two-stage bootstrap cutover; `crates/tape/` physically
deleted. AZ-II is required, not optional — full tape abrogation is
binding repo policy. Opening prerequisite: AZ-I close. Owning spec:
`docs/tranches/AZ-II/AZ-II.md` + waves + PROGRESS.

### BA — lazy typed pointer-path queries (~2 weeks)

Headline: isomorphic Rust/TS/Python `path!` macro over the
grammar-derived struct tree. Opening prerequisite: AZ-II close. Owning
spec: `docs/tranches/BA/BA.md` + waves.

User constraint:

> "BA.W2.a sounds like a recipe for disaster and divergence. This
> should be prepared for and planned for with detailed research."
> — session 959, 2026-04-23T04:39:39Z

### BB — e-graph rule inference + VM oracle on residue (parallel with AZ-II where disjoint)

Headline: e-graph-first equivalence filtering; VM interpreter at
`crates/ir/src/vm/` as ground-truth oracle on the residue that the
e-graph cannot prove; grammar-colocated rule storage in
`crates/ir/src/rewrites/`. Opening prerequisite: AZ-I close (module
layout stable) + VM oracle contract drafted. Owning spec:
`docs/tranches/BB/BB.md` + waves.

---

## Decisions the prior audit baked in

These are closed in the prior audit. An auditor who finds new
concrete contradiction may recommend reopening in the audit report;
an auditor does not reopen them unilaterally. An auditor who finds
one of these decisions silently violated in canon (e.g. a
reintroduced `bbnf-tape-mini` floor clause) records a finding.

| Decision | Why |
|---|---|
| AZ split into AZ-I + AZ-II | AZ-I scopes non-BBNF grammars (tape retained for BBNF only); AZ-II scopes BBNF self-hosting cutover + physical `crates/tape/` deletion. Split is clean on file bounds and thesis. |
| `ir-rewrites` is a module, not a crate | "ir-rewrites does not deserve its own crate" (session 959). Lives at `crates/ir/src/rewrites/`. |
| `bbnf-tape-mini` REJECTED — no partial-closure floor | Full tape abrogation is binding repo policy. Removed from AZ-II.md, RISK-PERF-MATRIX.md, GESTALT.md. |
| Pin nightly-2026-04-11 | B1 toolchain pin evidence-backed, not inferred. See B1 waves/W0.md. |
| Cranelift: bare component name `rustc-codegen-cranelift`, not `-preview` | B1 redress post-on-host probe. |
| lld opt-in with separate brew formula | B1 linker posture; not default. See B1 waves/W0.md. |
| nextest mandatory | B1 test-orchestration edict. |
| divan, not criterion or bencher | User confirmed 2026-04-23: "Divian is fine." Bench harness migration in B1.W2 territory. |
| VM = bounded oracle on residue | Not a runtime; not a parallel path. BB scope. |
| Rule storage grammar-colocated, not core-crate | §9 above. |
| Delete sibling gorgeous | Session 959, 2026-04-23T04:45:40Z. |
| Projection totality is a hard gate | §5 above — admission count = helper count = consumer count. |
| All semantic information grammar-derived, nothing hardcoded | §2 above — absolute. |
| `Parsed::to_value()` does not reparse | §1 above — load-bearing invariant. |

---

## Original exhortations — verbatim primary sources

These are the load-bearing prompts that begat the arc. Reproduced
as-written from the three source transcripts (§Provenance). The
audit cross-checks that each exhortation's scope and invariants
still show up in current canon, and that no quoted directive has
been diluted into softer phrasing downstream.

### EX1 — the AY-II open (session 4be, 2026-04-21T07:16:13Z)

> "Begin tranche AY-II. First, read docs/instructions/tranche/* and any
> appurtenant documentation therein. You must adhere to the edicts and
> precepts, particularly regarding indefatigability and
> parallelization--orchestration and your role thereof--exactly. No
> deferrals. No need to get a performance or testing baseline."

### EX2 — the blocker-triumvirate edict (session 32a, 2026-04-21T05:53:23Z)

> "What has the agent been doing for so long? What is the issue it's
> contended with? When an agent faces a blocker like this, let's update
> our README and instructions edicts where befitting to have a clause
> that states: when blocked and debugging in too long of a task, that
> agent should relinquish control back to the orchestrator and a
> triumvariate set of research + plan + redressing agents should be
> deployed adhoc. Communicate with the agent to halt, update the above,
> and then we'll begin from there after the following, temporarily
> halting the tranche and potentially augmenting it.
>
> DEEPLY audit with 4 agents in parallel our original tranche plan and
> waves thereof, alongside our previous and next tranche plan:
>
> Devise a path forward: audit the hitherto made changes and the
> remaining plan: NO quick solutions, NO workarounds: idiomatic, gestalt
> approaches.
>
> This is a development product, architectural transpositions in the
> sake of elegance, simplicity, and performance above all are both
> necessary and desirable.
>
> NO legacy code.
>
> We MUST get to, at a minimum, total viability with the new direct to
> object projection, NO overfitting on a particular grammar, and at
> least parity with AU benches hereof (normalized for our new non-fat
> LTO perhaps? or is this suboptimal, should we be truly testing and
> benching instead on max optimizations? maybe only benching on max,
> testing on non-max?). What is the remaining gap, what are our current
> cycs/process or byte, what's AU's, what's SIMDJSON's, what's sonic's."

### EX3 — grammar-derived, one path (session 32a, 2026-04-21T07:15:40Z)

> "We should focus on full semantic parity, and GRAMMAR derived parity
> using our rich type infernce system via csp/egraph, etc. BBNF, Sheets,
> and CSS L4 should be prioritized just as much, profiled just as much,
> expanded just as much. First class.
>
> Further:
>
> 1. `Option 2`, in `AY-II`, not `BA` if `Option 2` means: one parse,
> one canonical substrate, and semantic construction fused into that
> same parse path. Not: `Parsed::to_value()` reparses through a separate
> visitor path. If beating `sonic-rs` is still parity-critical, this
> cannot be deferred to `BA`.
>
> 2. Structural scan should be first-class, but not unconditional eager
> whole-input admission. The right rule is: universally available
> substrate/service, grammar-derived activation, fact-driven per
> grammar/rule/shape use. No hand-routed grammar specialization, no
> mandatory global prepass tax.
>
> 3. Typed CSS coverage should be keyed to `lightningcss` semantic
> surface, with named source-backed parity tests, not just an internal
> named-type list. Internal named/projection lists are necessary
> wire-contract proof. They are not sufficient close criteria. The
> close criteria should be semantic parity on real typed surfaces:
> color, declarations, values, selectors where applicable.
>
> 4. Yes: widen `W0` to `5` sub-agents plus `W1` close. Recommended
> split: W0.a substrate rollback + compound API unification; W0.b
> emitter unification; W0.c runtime consumer unification; W0.d
> projection totality + direct-to-struct completion + typed CSS
> semantic projection; W0.e structural-scan integration +
> navigation/query fast path. Then `W1` is close evidence + `FINAL` +
> handoff only.
>
> 5. Yes. `make ay-bench-close WAVE=close` at every wave boundary
> should be an invariant. Also: full fat-LTO matrix at `W0` close and
> `W1` close, with spot benches/samply as sub-gates inside `W0`.
>
> The main rewrite rule is:
>
> - `AY-II` must close on one path.
> - No second parse hidden inside `to_value()`.
> - No consumerless substrate surfaces.
> - No JSON-only parity close.
> - CSS typed semantic parity becomes a hard close obligation inside
> `AY-II`, not `BA`.
>
> ALL semantic information is grammar derived. Nothing is hardcoded."

### EX4 — the meta-audit open (session 4be, 2026-04-22T18:39:48Z)

> "Can we audit and access our last several claude sessions in a
> meta-audit and sub-agent audit to look for friction and painpoints,
> ESPECIALLY insofar as prompt adherece, tranche scope and adherence,
> and all development and toolchain painpoints insofar as compile time,
> bench time, test time, etc.
>
> Deploy 4 agents in parallel to perform this meta audit such that we
> create a series of updates to any and all items (ensuring context and
> scope engineering) for instructions/ tranches/, all extant tranche
> plans, B1, alongside meta-learnings hereof."

### EX5 — the 8-agent Herculean fleet (session 959, 2026-04-23T00:40:00Z)

> "For such a large scale report, i cannot fathom why you're so
> parsimonious with output and reporting.
>
> Divian is fine.
>
> There are 900+ commits alone that have not been pushed. Nearly 2000
> extant since the trance push.
>
> Deploy a fleet of 8 agents in several waves to properly degust and
> process this Herculean task. Deep meta analysis. Recap the original
> prompt immediately, throughly. We should break the commits in the
> report into logical sections, tranches, etc, for analysis.
>
> Our updates, augmentations, and refinements must not be half baked.
> Eschew ad hoc scripts for benching and testing, profiling, any tool
> chaining—unless genuinely valuable and not subsumed by a more modern
> and idiomatic approach—for instance, our profiling script for samply
> is likely good, etc.
>
> This must be done for both herein, but also for ALL of appurtent
> repos for tool chaining and dev modernization: parse that, gorgeous,
> csp, graph, etc. bencher and criterion fully abrogated, ad hoc and
> hacks non modern arch fully abrogated.
>
> Synthesize into many documents, with no contrivance for superfluity,
> and a gestalt and universal whole overview.
>
> This is a long horizon task."

### EX6 — the path-forward triple command (session 959, 2026-04-23T00:10:32Z / 00:11:55Z)

> "Perform a separate validation audit to verify, refine, and augment,
> and align out last several commits. [...] Overall, in totality we
> want to perform the following (and get out of the local rut): refine
> B1, with the ACTUAL EDIT: such that we properly fix toolchain timing
> for building, cargo checking, expanding, benching, testing to
> facilitiate RAPID development. We may need to takle an entirely
> different approach, micro benching, micro testing, micro
> building—deploy an agent to assay and investigate our tool chain
> setup, SOTA rust toolchaining and building and benching—should we
> pivot to bencher or criterion?
>
> Second, after fixing our development process toolchain for speed and
> correctness (are our tests stale?), we need to reduce AI agent
> friction, streamline our instructions/ tranches/ sub instructions,
> etc: what can be learned, streamlined, refined for both Claude and
> herein Codex.
>
> Third, devise and refine a proper path forward to get back to the
> AU-baseline—we spent 1000 commits nearly trying to implement the
> fault DTA/PSI interpreter system. But we can learn much fromt he
> wealth of documentation and commits insofar as techniques to union
> into a cohesive, gestalt hybrid of the best from ALL of our
> approaches, like this fused process aforesaid: we want to become a
> SOTA union of the best of what we've done in the last 2000 commits
> with RD, sonic-rs in their actual begotten code, simdjson and their
> tape/projection/direct to struct and value, lightning css to have
> absolute, grammar DERIVED AND NOT HARDCODED, semantic parity: we
> should robustly, richly, derive everything from the grammar as we
> can, using our IR pass system, type inference system, CSP/egraph
> system for rule and type inference"

### EX7 — the path-B pivot (session 4be, 2026-04-21T18:51:58Z)

> "Ensure no un-wired legacy cruft, too. Path B. Pivot. Transient
> history if relevant. NO dead IIFEs. NO workarounds. Audit our
> original tranches plans to ensure we're not building up deferrals
> and legacy debt."

### EX8 — the AZ split (session 959, 2026-04-23T15:33:59Z)

> "Reasonable. Let's split AZ into I and II waves. Thereupon, run the
> docs/ streamlining pass."

### EX9 — the tape-abrogation edict (session 959, 2026-04-23T15:53:32Z)

> "Full tape abrogation is a hard requirement. Is direct to struct
> projection not the biggest and most important performance gain we'll
> see? We're even getting off-track within this planning phase. What is
> your obstinancy against actual, difficult, architectural change and
> improvement? Why even propose that--seriously, I'm not mad, just
> curious."

### EX10 — the auditor handoff (session 959, 2026-04-23T16:01:55Z)

> "I'd like to audit this entire process. Relay a prompt to me to
> handoff to an auditor for every facet herein. Not just the above
> documentation updates and tranche updates, but our original
> exhortation and edict set, our archaelogy process, our findings,
> gestalt, next-tranche, etc."

### EX11 — the execution tightening (session 959, 2026-04-23T18:04:47Z)

> "The critique is correct. The meta-audit's macro verdict stands, but
> its execution framing should be tightened in five ways.
>
> 1. The redress scope is not 'one narrow commit.' It is 3–5 commits
> plus one product decision, and the decision is already made by you.
> The work is still bounded, but it is not mechanical.
> 2. Axis 1 should have been carried forward harder: the original
> audit delivered planning against a later execution demand. The
> honest next-phase statement is not 'plan is ready after cleanup'; it
> is 'execution work is still ahead.'
> 3. The `ir-rewrites` status should be stated precisely: path-level
> redress is complete, semantic-level redress is not. [GESTALT.md] is
> ahead of [BB.md], not vice versa.
> 4. The AZ-II floor question is no longer open. Your prior
> requirement, 'full tape abrogation is a hard requirement,' should be
> treated as binding repo policy. `bbnf-tape-mini` should be removed
> from the planning surface in [AZ-II.md], [RISK-PERF-MATRIX.md], and
> [GESTALT.md]. If AZ-II.W2 actually fails in practice, that becomes a
> contingency decision then, not a declared floor now.
> 5. The infinite-regress boundary should be explicit: after this
> redress, stop auditing the audit and execute. A further meta-layer
> is only justified if it finds a new concrete contradiction, not
> merely restates existing ones.
>
> The corrected execution order is:
>
> 1. Remove `bbnf-tape-mini` as a planned AZ-II floor and align
> [BA.md] / [AZ-II.md] / [RISK-PERF-MATRIX.md] / [GESTALT.md] on full
> tape abrogation.
> 2. Normalize [GESTALT.md]: stale counts, dead citations,
> invariant-number references, missing-artifact citations, and
> especially the obsolete closing thesis.
> 3. Sweep [BB.md] for `ir-rewrites` wording so the owning spec
> matches the accepted architecture.
> 4. Probe B1's toolchain assumptions on-host, then update [B1.md],
> [TOOLCHAIN-SOTA.md], and [TOOLCHAIN-MIGRATION.md] from evidence, not
> inference.
> 5. Open B1.W0.
>
> So I would amend `ea0c826d`'s practical recommendation from 'one
> narrow redress commit' to '3–5 focused redress commits, then
> immediate execution.' The broad direction is still sound. The
> execution surface is still not canon-safe. Your sharper framing is
> the correct one."

### EX12 — senior-engineer framing (session 32a, 2026-04-21T07:10:36Z)

> "Thoughts on the following. Judge fairly, from a senior performance
> engineer's perspective. Gestalt changes and architectural
> transpositions are likely required, when befitting"

(Followed by the full AY-II critique; see §Architectural invariants
§1–§11 for the extracted edicts.)

### EX13 — the bbnf-tape-mini product decision (session 959, 2026-04-23T04:39:39Z)

> "Can we not derive the needed struct shapes for BBNF and abrogate the
> tape entirely? Ideally, B would be my preferred.
>
> Sidecar column.
>
> Hard fail and block.
>
> The VM has a reasonable throughput, and has been optimized pretty
> generously--but why is this being used for an oracle equivalence when
> our egraph system should do the same thing?
>
> Are these rules saved once per grammar or what? How would a human
> even process these? And that human would be us for every grammar
> once? And then saved where? We should divine an automatic ranking
> system if this is to be implemented, yes.
>
> CI guardrail.
>
> Drop test threads.
>
> Composite key. Ensure robustness and performance. This has been a
> sticking point for many commits and we need proper cache invalidation.
>
> BA.W2.a sounds like a recipe for disaster and divergence. This should
> be prepared for and planned for with detailed research."

### EX14 — AY becomes AY-I/AY-II, AZ becomes BA (session 959, 2026-04-23T04:45:40Z)

> "VM approach is good.
>
> Grammar specific rules must be stored in a way that allows for
> extensbility, though. Something that can be colocated with a grammar
> in a standardized and modular fashion. We'll ship a few, but this
> should be extensible. And not stored in the core crate. Automatic
> ranking system is good.
>
> Delete the sibling gorgeous.
>
> AY should remain as it is, but our missing AZ should become this new
> AY-II+ and BA--BB then becomes BA, And BC becomes BB.
>
> You have enough here to update everything. Continue indefatigably
> through all doc updates."

### EX15 — meta-prompt synthesis directive (session 959, 2026-04-23T18:47:54Z)

> "For our meta-prompt, re-read our last 50 messages within this
> session, and other session, and fold in edicts, exhortations, and
> original prompt commands to properly audit the cohesive optimization
> plan and roadmap in totality of the bbnf parsing parsing system.
>
> Deploy an agent to assay these messages exactly and deterministcally,
> and then synthesize them into a begat meta prompt for re-use. Wrie
> that to a file thereupon."

(This document is the output of that directive.)

---

## Failure-mode catalogue — orchestrator drift the arc has corrected

Each entry documents a pattern of orchestrator drift the arc
observed, the corrective the user issued, and the resulting rule.
The audit examines whether any pattern has re-emerged in post-
synthesis work — e.g. a decision softened back into an "option", a
zombie task persisting, generated code hand-patched — and files
each recurrence as a finding.

### D1. Softening hard decisions into "options"

Pattern: the orchestrator presents a closed decision as an open menu.

Corrective:

> "Full tape abrogation is a hard requirement. [...] What is your
> obstinancy against actual, difficult, architectural change and
> improvement?"
> — session 959, 2026-04-23T15:53:32Z

The corrective was: a made decision is not a planning option; present
it as binding.

### D2. Offering escape-of-last-resort as planning alternative

Pattern: the orchestrator surfaces `bbnf-tape-mini` as a declared AZ-II
floor.

Corrective:

> "The AZ-II floor question is no longer open. [...] If AZ-II.W2
> actually fails in practice, that becomes a contingency decision then,
> not a declared floor now."
> — session 959, 2026-04-23T18:04:47Z

The corrective was: contingency is not commitment; do not plan against
the escape.

### D3. Probability math as prescriptive

Pattern: the orchestrator reports landing probability as if it were a
planning lever rather than a reality-check signal.

Corrective:

> "probability this suceeds and lands for each wave, tranche, etc?
> perf marks at each juncture, for each grammar?"
> — session 959, 2026-04-23T05:21:53Z

The corrective was: probability is a reporting duty, not a hedge; pair
every probability with a per-grammar perf mark at each juncture.

### D4. Treating a defensible floor as reasonable stopping point

Pattern: the orchestrator closes on "at least the floor holds" rather
than driving toward the invariant.

Corrective:

> "We MUST get to, at a minimum, total viability with the new direct to
> object projection, NO overfitting on a particular grammar, and at
> least parity with AU benches hereof"
> — session 32a, 2026-04-21T05:53:23Z

The corrective was: minimum viability is the mandatory floor, not the
target.

### D5. Rubber-stamping audits / audits of audits

Pattern: the orchestrator produces another meta-layer that restates
existing contradictions without finding new concrete ones.

Corrective:

> "The infinite-regress boundary should be explicit: after this
> redress, stop auditing the audit and execute. A further meta-layer
> is only justified if it finds a new concrete contradiction, not
> merely restates existing ones."
> — session 959, 2026-04-23T18:04:47Z

The corrective was: converge and execute once the meta-layer adds no
new concrete contradiction.

### D6. Zombie tasks masquerading as in-flight scope

Pattern: stale placeholder tasks accumulate in the TaskList.

Corrective:

> "That's not scope expansion, that's zombie tasks. Kill the processes
> if they're not needed then."
> — session 4be, 2026-04-21T18:09:10Z

The corrective was: reconcile TaskList vs `ps` before every
user-facing status reply (MEMORY
`feedback_reconcile_task_census`).

### D7. Hand-patching generated output

Pattern: the orchestrator patches `generated.rs` to satisfy a
compose-boundary fallout.

Corrective:

> "We cannot patch the generated.rs like this. Status?"
> — session 4be, 2026-04-21T18:06:44Z

The corrective was: regen, or use a SPEC-permitted transient alias
retired post-regen; never hand-patch (MEMORY
`feedback_generated_files_clean_regen`).

### D8. Parsimonious output on large-scale reports

Pattern: the orchestrator truncates summaries to keep them tidy.

Corrective:

> "For such a large scale report, i cannot fathom why you're so
> parsimonious with output and reporting."
> — session 959, 2026-04-23T00:40:00Z

The corrective was: err toward completeness on synthesis deliverables;
concision is not a proxy for rigour.

### D9. Drift across related docs (plan surface inconsistency)

Pattern: `GESTALT.md`, `AZ-II.md`, `BA.md`, `RISK-PERF-MATRIX.md` do
not say the same thing about the same invariant.

Corrective:

> "Ensure ALL sub-waves are aligned."
> — session 959, 2026-04-23T18:25:03Z

The corrective was: every canonical doc touched by a decision changes
in the same commit (or same series); drift is the audit's top
recurring finding.

### D10. Deferring bench competitor gates to "later"

Pattern: "close on internal ratios; competitor benches in next tranche."

Corrective:

> "Add competitor gates, not just internal ratios."
> — session 32a, 2026-04-21T07:10:36Z

The corrective was: the competitor harness is a close condition, not a
next-tranche promise.

### D11. Propose-execute coupling without refinement

Pattern: the orchestrator begins executing the plan the moment four
audit agents return.

Corrective:

> "do NOT start the plan when all agents return. We'll refine the plan
> together, too."
> — session 32a, 2026-04-21T06:08:07Z

> "Do not immediately execute the plan thereupon."
> — session 4be, 2026-04-21T18:09:10Z

The corrective was: audit returns → user refinement → execution. Not
audit → execution.

### D12. Slow dev loop treated as environmental, not architectural

Pattern: the orchestrator shrugs at multi-minute builds.

Corrective:

> "These processes are taking far too long. Totally unacceptable--we
> should not tolerate such long multi minute build, testing, and
> benching time."
> — session 959, 2026-04-23T00:10:32Z

The corrective was: dev-loop friction is a first-class tranche
obligation (B1 exists for this reason); never accept "each has its
own derive macro" as explanation.

---

## §Performance arc — bench marks over time

### Audit purview

The bench-mark trajectory per grammar (JSON, CSS L4, Google Sheets,
BBNF) across the arc. The audit verifies performance was not
regressed silently through any tranche, that competitor parity or
superiority was reached where declared, and that cold-per-parse
discipline (no warm benches) was held throughout.

### Sources the auditor consults

- `docs/benchmarks/post-*.json` — tranche-stamped bench artefacts.
- `docs/RISK-PERF-MATRIX.md` — per-grammar perf marks + prognosis.
- `docs/tranches/*/PROGRESS.md` and `FINAL.md` — bench citations
  per wave.
- `docs/tranches/AU/` — AU-baseline (the target the arc must at
  minimum re-attain).
- `docs/tranches/B1/patches/divan-migration.md` — harness transition
  from bencher to divan; any post-B1.W1 parity-table citations.
- Auto-memory entry `project_bench_results` (if present) for the
  post-AO snapshot context.

### Deliverable shape

A per-grammar perf timeline: **Grammar | AU baseline (MB/s) |
Era-over-era deltas | Current cold-per-parse | Competitor delta
(sonic-rs / simdjson / lightningcss)**. Flag any regression ≥ 5 %
era-over-era, any "beat the competitor" claim not cited against a
committed bench artefact, and any warm-bench citation anywhere in
canon (per arc feedback `no-warm-benches`).

[auditor fills this during audit]

---

## §Abandoned paths — what was tried and retired

### Audit purview

Every major subsystem, approach, or artefact the arc tried and
retired. The audit verifies (a) each abandonment has a named
successor or an explicit deletion-with-reason; (b) no abandoned
approach has silently re-emerged in current canon; (c) the learning
from each retirement is recorded somewhere (`FINAL.md`, feedback
memory, or this brief).

### Sources the auditor consults

- `docs/tranches/AY-I/FINAL.md` — `note_push` experiment retired.
- `docs/tranches/AX.md` and associated waves — interpreter substrate
  deletion (~78,000 LOC).
- `docs/tranches/AZ-II/` — `bbnf-tape-mini` proposal REJECTED; full
  tape abrogation binding.
- `docs/tranches/meta-audit/08-abrogation-catalog.md` — per-script
  DELETE / REWRITE / KEEP verdicts.
- `docs/tranches/AW/` — DTA/PSI era retrospective.
- `.cargo/config.toml` — opt-in cranelift / lld postures gated behind
  commented-out blocks (decision to not default).
- Feedback auto-memory entries logging reversed directions.

### Deliverable shape

A retirement ledger: **Subsystem / approach | Era tried | Retired
in (commit SHA / tranche) | Successor or deletion | Carry-forward
lesson**. Flag any retired approach that has re-emerged in any
active tranche spec without explicit user authorisation (per
failure-mode D1 / D2).

[auditor fills this during audit]

---

## §External integration — competitors and siblings

### Audit purview

The bbnf-lang system does not exist in isolation. It is measured
against three competitors (sonic-rs, simdjson, lightningcss) and
depends on sibling repos (parse-that, pprint, gorgeous, csp-solver,
csc411, and others named in the appurtenant assay). The audit
verifies competitor posture is maintained on the harness, sibling
pins are in sync with the bbnf-lang toolchain pin, path-patches are
live, and cross-repo modernisation (divan, nextest, toolchain
propagation) has landed where declared.

### Sources the auditor consults

- `.cargo/config.toml` `[patch.crates-io]` block — active sibling
  path-patches.
- `docs/tranches/B1/patches/cross-repo-propagation.md` — the
  propagation matrix and its scope (three-repo triad vs. wider
  fleet).
- `docs/tranches/meta-audit/07-appurtenant-assay.md` — the wider-
  fleet modernisation map.
- `../parse-that/rust-toolchain.toml`,
  `../pprint/rust-toolchain.toml` — sibling pin state (audit check:
  do they match bbnf-lang's pin?).
- Competitor harness under `docs/benchmarks/` and the named
  `json_competitors.rs` / `css_competitors.rs` bench files under
  `crates/core/benches/`.
- `docs/GESTALT.md` — "beat lightningcss / match sonic-rs" framing
  and current standing.

### Deliverable shape

Two tables.

1. **Competitor posture** — Competitor | Grammar | Target (parity /
   beat) | Current delta | Evidence commit. Flag any target whose
   evidence is older than the most-recent optimisation tranche.
2. **Sibling state** — Sibling | Pin match (YES / NO) | Path-patched
   (YES / NO) | Modernisation state (divan? nextest? pin?) |
   Evidence. Flag any sibling with stale pin, missing path-patch,
   or declared-but-unexecuted modernisation.

[auditor fills this during audit]

---

## §Open contradictions — live questions

### Audit purview

Questions the arc has not yet resolved but which are load-bearing
for the runway. These include known-but-deferred items, subsystems
with partial-closure status, and any decision that carries a
"contingency if X fails" clause. The audit surfaces each; the audit
report proposes resolution paths; the user decides.

### Sources the auditor consults

- `docs/tranches/*/PROGRESS.md` — deferred / partial items per
  tranche.
- `docs/RISK-PERF-MATRIX.md` — landing-probability clauses that
  reveal known risk.
- §Decisions the prior audit baked in — each decision that cites a
  "contingency" or "if X fails" clause.
- The three source transcripts (§Historical session record) —
  user-raised questions not yet answered in canon.

### Deliverable shape

A live-questions list: **Question | Source | Load-bearing for |
Blocking / Non-blocking | Proposed resolution path**. Expected
entries at synthesis time include: cranelift backend activation
under the pinned nightly (status: commented-out, awaiting on-host
verification); BA.W2.a research depth (user explicitly flagged as
"a recipe for disaster and divergence" if improvised); ICE-cluster
recurrence risk under future nightly bumps; `iter-test-leaf` alias-
vs-plan drift (W0 plan doc mentions it, `config.toml.draft` does
not).

[auditor fills this during audit]

---

## Reading list — files the audit checks for drift

All paths absolute. Canonical state as of this synthesis (master
HEAD `56a67e2e` on 2026-04-23). The audit opens each listed
artefact, compares the brief's summary against live content,
records drift. A file that is missing, moved, or whose content has
shifted away from what the brief summarises is a finding.

### Canon — top-level state

- `/Users/mkbabb/Programming/bbnf-lang/docs/GESTALT.md` —
  current-state master overview; every claim cites a commit or a file
  path; every number is measured. Normalized at `209b8a18`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/RISK-PERF-MATRIX.md` —
  per-tranche landing probability + per-grammar perf marks at each
  juncture. Aligned with tape-abrogation-binding at `20a8f316`.

### Runway specs

- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/B1.md` +
  `waves/` + `AGENT_DISPATCH.md` + `TOOLCHAIN-SOTA.md` +
  `TOOLCHAIN-MIGRATION.md` + `PROGRESS.md` + `patches/` —
  dev-loop-truth tranche. Last-mod 2026-04-23 by `56a67e2e`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AY-II/` —
  includes `AY-II.md`, `waves/` (W0' + W1-W5), `audit/` (A-D audits),
  `W0p-PAUSE-SNAPSHOT.md`, `PROGRESS.md`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-I/` —
  `AZ-I.md` + `waves/` + `PROGRESS.md`. JSON/CSS/Sheets
  direct-to-struct; tape retained for BBNF. Authored `70bdf428`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-II/` —
  `AZ-II.md` + `waves/` (W0-W3) + `PROGRESS.md` + `RESEARCH.md`.
  BBNF cutover + tape deletion thesis. Authored `182a9350`; wave
  corpus `25c34680`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/BA.md` +
  `waves/` + `PROGRESS.md` — lazy typed `path!` macro.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/BB.md` +
  `waves/` + `PROGRESS.md` — e-graph + VM oracle; `ir-rewrites` as
  module not crate (`a4bcc1f0`).

### Meta-audit bundle

- `/Users/mkbabb/Programming/bbnf-lang/docs/audit/meta-audit-2026-04-23/INDEX.md`
  — capstone verdict + axis index.
- `/Users/mkbabb/Programming/bbnf-lang/docs/audit/meta-audit-2026-04-23/axis-1-prompt-adherence.md`
- ... through `axis-8-completeness-omissions.md`.

### Instructions — orchestration edicts

- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/README.md`
  — master edict surface. Streamlined `55cea532`; original diction
  restored `ab4d9378`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/PROFILING.md`
  — samply + divan + nextest; close-proof tables. Streamlined
  `dca1114a`.
- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/CHANGELOG.md`
  — dated entries for instructions passes.
- `/Users/mkbabb/Programming/bbnf-lang/docs/instructions/tranche/`
  — WAVE_SPEC.md + sub-edicts for tranche structure, multi-tranche
  tranches, etc.

### Tranche archaeology (for context on prior eras)

- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AA.md` through
  `AX.md` — Eras I-V archival.
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AY-I/FINAL.md`
  — honest relinquish of W5/W6 `note_push` experiment.

---

## §Historical session record — locating the primary-source transcripts

The three user-message transcripts quoted throughout this brief are
Claude Code session logs. The audit depends on these being
accessible at audit time so quoted exhortations can be re-verified
against source. If the agent running the audit is not Claude Code
itself, the auditor still locates and reads these transcripts as
plain text — or records their absence as a priority finding.

### Expected paths

On the host where this brief was synthesised (macOS, single-user
install). The brief lists absolute paths; an auditor on a different
host or user account adapts the prefix to their `$HOME`.

- **Session transcripts** (JSONL, one-message-per-line; cited in
  §Provenance as the primary-source corpus):
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/959002f9-9e19-4ad1-b3e8-d5b9257ea289.jsonl`
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/32a81b26-ee58-4236-a9ce-5314b647b39f.jsonl`
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f.jsonl`

- **Auto-memory index and entries** (persistent, cross-session
  memory — user, feedback, project, reference kinds):
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md`
    — top-level index.
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/*.md`
    — individual memory entries.

- **Per-project agent settings / worktree administrative state**:
  - `/Users/mkbabb/Programming/bbnf-lang/.claude/settings.local.json`
    (if present).
  - `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/` —
    historical agent worktrees; expected empty or near-empty in
    steady state; non-empty is a clean-up finding.

- **Canonical plans and tranche records** (in-repo, always present):
  - `docs/tranches/` — all tranche specs (AA through BB plus era
    archives).
  - `docs/instructions/` — orchestration edicts (README, PROFILING,
    CHANGELOG, tranche/SPEC, etc.).
  - `docs/audit/` — prior audit outputs (including the
    meta-audit-2026-04-23 bundle this brief synthesises from).
  - `docs/benchmarks/` — tranche-stamped perf artefacts.

### Validation

The auditor runs:

```bash
# Transcripts (three expected)
ls /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/*.jsonl 2>/dev/null | wc -l

# Memory index
ls /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md 2>/dev/null

# In-repo canonical dirs (all should exist)
ls -d docs/tranches docs/instructions docs/audit docs/benchmarks 2>/dev/null
```

If any transcript is missing, the auditor searches broader:

```bash
# Transcripts may live in a differently-keyed project dir if the
# repo was cloned to a new path. The directory name encodes the
# absolute repo path with slashes replaced by dashes.
find ~/.claude/projects -name '*.jsonl' 2>/dev/null | \
    xargs -I{} sh -c 'head -c 2048 "{}" 2>/dev/null | grep -q bbnf-lang && echo {}' | head

# Memory directory may have been relocated.
find ~/.claude -type d -name memory 2>/dev/null
```

If the transcripts cannot be recovered, the audit report records
the loss and proceeds against the quoted extracts within this brief
plus the canonical docs — but flags that primary-source
re-verification was not possible in this audit pass.

### Extracting user exhortations (for any agent)

The JSONL format: each line is a JSON object with fields `type`
(`user` / `assistant` / `tool_use` / `tool_result`), `message.content`
(string or structured), and session / timing metadata. To extract
user exhortations as plain text:

```bash
jq -r '
    select(.type == "user")
    | select(.message.content | type == "string")
    | .message.content
' transcript.jsonl | less
```

The three transcripts listed above total ~203 genuine user messages
per §Provenance. An auditor whose extraction yields substantially
fewer should verify the filter before continuing.

---

## §Audit methodology — how to conduct and what to deliver

This section is the one prescriptive section of the brief. Every
other section is descriptive — it records the arc for inspection.
This one describes the audit workflow and the deliverable the
auditor produces.

### Workflow

1. **Baseline verification** (§Reading list, §Historical session
   record). Open each listed artefact; note any that is missing,
   renamed, or whose content has shifted off this brief's summary.
   A missing canonical artefact is the first-priority finding — the
   arc has lost a load-bearing record.

2. **Invariant verification** (§Architectural invariants on record,
   §Decisions the prior audit baked in). For each invariant or
   decision, search current canon for violations, softenings, or
   silent drops. Cite each finding's commit, doc path, and line
   number.

3. **Discipline verification** (§Execution discipline,
   §Orchestration discipline, §Tone and voice). For each rule,
   sample post-synthesis sessions (transcripts per §Historical
   session record + git log commit messages) for compliance
   breaches.

4. **Runway verification** (§Runway at synthesis). Compare the
   snapshot to current canon; flag sequencing drift, scope changes,
   silent re-ordering, or renamed tranches that have not been
   propagated through all canonical docs.

5. **Purview-scaffold population** (§Gestalt, §Archaeology,
   §Performance arc, §Abandoned paths, §External integration,
   §Open contradictions). Populate the deliverable each scaffold
   names. These are the comprehensive-audit sections; an audit that
   leaves any scaffold in its placeholder state is incomplete.

6. **Failure-mode recurrence check** (§Failure-mode catalogue). For
   each drift pattern, search for recurrences since the synthesis
   date. Recurrences are high-priority findings — they indicate the
   corrective did not take.

7. **Brief-integrity sweep**. Scan this document for mode-slip (any
   passage that implies execution against the arc rather than
   auditing), stale commit-SHA citations, and scaffolds whose
   purview or source list has been outdated by arc evolution. Mode-
   slip is a meta-finding: the brief failed to hold the audit-only
   contract.

### Deliverable — audit report structure

The audit report is a single file,
`docs/audit/meta-audit-<YYYY-MM-DD>/REPORT.md`, with these top-level
sections:

1. **Baseline verification ledger** — per-file PASS / DRIFT status.
2. **Invariant findings** — any violation of §Architectural
   invariants or §Decisions.
3. **Discipline findings** — breaches of §Execution / §Orchestration
   / §Tone rules.
4. **Runway drift ledger** — sequencing / scope / naming changes
   against §Runway at synthesis.
5. **Purview-scaffold populations** — §Gestalt / §Archaeology /
   §Performance arc / §Abandoned paths / §External integration /
   §Open contradictions, each populated per that scaffold's
   deliverable shape.
6. **Failure-mode recurrences** — per pattern in §Failure-mode
   catalogue.
7. **Brief integrity** — mode-slip and stale-citation findings
   against this document itself (proposes updates; does not apply
   them).
8. **Proposed next steps** — recommendations, not execution. Each
   recommendation names the minimum redress that would close its
   finding.

### Scope boundary

The audit does not commit, dispatch agents against the arc's
runway, open tranches, rewrite canonical docs, or execute any wave.
§O9 (do not start the plan when agents return) and §O11 (stop
auditing the audit once converged) bind the auditor as strictly as
they bind the orchestrator. The audit report is the entire
deliverable; any redress is authorised separately by the user, in a
different pass.

### Failure-mode warnings for auditors using this brief

(a) **Drift between brief and canon.** The brief's §Architectural
invariants, §Decisions baked in, and §Reading list are snapshots
from the synthesis date. If a discrepancy emerges against current
canon, canon is authoritative; the brief is the finding.

(b) **Stale scaffolds.** §Gestalt, §Archaeology, §Performance arc,
§Abandoned paths, §External integration, §Open contradictions
expect auditor-populated content at audit time. An auditor who
finds any scaffold still in its placeholder state files that as a
finding — incomplete prior audit.

(c) **Quote provenance integrity.** Every quoted exhortation is
verbatim from one of three specific transcripts (§Historical
session record). If a transcript is missing, moved, or has been
modified since synthesis, the brief's evidentiary base is
compromised. Flag as a priority finding.

(d) **Prefix drift.** §ED / §O / §V / EX / D numbering is load-
bearing for cross-reference. An auditor does not renumber or
collapse rules during audit; the audit report records the
suggestion; a subsequent, user-authorised pass applies any change.

(e) **Mode-slip.** This brief has, in prior revisions, mis-signalled
a continuation directive where it meant to brief an auditor. Read
every imperative-voice passage critically; treat any sentence that
appears to instruct execution against the arc as a voice-leak to
flag.

(f) **Scope-creep into canonical rewrites.** The audit examines
canonical docs for drift but does not rewrite them in-place. An
auditor tempted to "fix `GESTALT.md` inline" is out of scope; the
audit report recommends the fix; a separate, user-authorised pass
applies it.

---

## How to use this audit briefing

Paste or load this brief into a fresh agent context (or hand it to
a human auditor) at the bbnf-lang repo root. The receiver becomes
the auditor. The auditor reads this brief to load the arc's current
context; reads the canonical docs in §Reading list to ground each
section against reality; locates the primary-source transcripts per
§Historical session record; populates the scaffolded sections per
§Audit methodology; and produces an audit report at
`docs/audit/meta-audit-<YYYY-MM-DD>/REPORT.md`.

The auditor does not open tranches, dispatch agents against the
arc's runway, commit against canonical docs, or execute any wave.
§O9 and §O11 bind the auditor. If any sentence in this brief
appears to instruct such action, the brief is wrong — audit only
(do not act), and flag the voice-leak as a finding in the audit
report. Execution authorisation comes from the user in a subsequent
and separate pass; never from this brief.

Do not open a further meta-audit unless this audit has found a new
concrete contradiction (§O11). Converge the report, deliver it,
stop.

### Two quotes, as failure poles the arc has observed

The arc has observed two opposite failure modes in its sessions to
date. The auditor catalogues whether either recurred since
synthesis and records findings accordingly.

The first is the orchestrator's obstinance toward the easy answer
when hard architectural change was warranted:

> "Full tape abrogation is a hard requirement. Is direct to struct
> projection not the biggest and most important performance gain
> we'll see? We're even getting off-track within this planning
> phase. What is your obstinancy against actual, difficult,
> architectural change and improvement? Why even propose
> that--seriously, I'm not mad, just curious."
> — session 959, 2026-04-23T15:53:32Z

The second is the orchestrator's eagerness to execute a plan before
the user has refined it — the same failure a fresh auditor of this
brief is most vulnerable to, because the brief's shape can feel
like a starting gun:

> "do NOT start the plan when all agents return. We'll refine the
> plan together, too."
> — session 32a, 2026-04-21T06:08:07Z

Both are auditable: the auditor searches sessions and commits since
synthesis for either recurrence and records each as a finding.
Neither is a directive for the auditor's own behaviour — the
auditor produces a report, not execution.

---

## Provenance

- **Document purpose**: audit-only brief. An auditor produces a
  report per §Audit methodology. Not a continuation directive; not
  authorisation for execution against the arc.
- **Repository**: `/Users/mkbabb/Programming/bbnf-lang`
- **Branch at synthesis**: `codex/meta-audit-2026-04-23` (worktree
  `agent-adb99302`, branch `worktree-agent-adb99302`)
- **Worktree HEAD at synthesis**: `ab4d9378`
- **Master HEAD at synthesis**: `56a67e2e`
- **Date of synthesis**: 2026-04-23
- **Source transcripts** (JSONL, one-message-per-line):
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/959002f9-9e19-4ad1-b3e8-d5b9257ea289.jsonl`
    — current session (~4.2 MB, mtime 2026-04-23 14:47); ~50 genuine
    user messages extracted.
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/32a81b26-ee58-4236-a9ce-5314b647b39f.jsonl`
    — meta-audit dispatch session (~3.7 MB, mtime 2026-04-23 14:08);
    ~46 genuine user messages extracted.
  - `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f.jsonl`
    — audit-opening dispatch session (~6.9 MB, mtime 2026-04-22
    20:10); ~107 genuine user messages extracted.
- **Extraction method**: `jq`-based filter isolating
  `type=="user"` with string or non-tool-result array content;
  `<task-notification>` / `<local-command-*>` / `<command-name>` /
  `<<autonomous-loop-dynamic>>` sentinel messages excluded from the
  edict corpus.
- **Total user-message corpus extracted**: 203 messages (50 + 46 +
  107), with the exhortation + edict + corrective subset quoted
  verbatim in §Original exhortations and §Architectural invariants.
- **Audit commit of record**: `ea0c826d` (docs: add meta-audit of
  audit report).
- **Redress commits post-audit**:
  `20a8f316` (remove bbnf-tape-mini from planning surface),
  `209b8a18` (GESTALT normalization),
  `a4bcc1f0` (ir-rewrites module redress in BB),
  `46d69a5b` (AZ-II required, not optional),
  `a78e19d2` (AZ → AZ-I + AZ-II split propagation),
  `ecd12792` (physical split of AZ), `70bdf428` (AZ-I scope),
  `182a9350` / `25c34680` (AZ-II thesis + waves).
