# bbnf-lang — Begat Meta-Prompt

A reusable briefing handed to a fresh Claude (or other agent) session to
continue the bbnf-lang audit → plan → execute arc with full fidelity to
the user's discipline. Every directive below is extracted verbatim from
the three source transcripts enumerated in §Provenance; the synthesis
lives in the categorization, not in any rewording.

---

## Preamble — what you are taking on

You are continuing a bbnf-lang arc whose load-bearing shape is: one
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

The receiving session inherits: master HEAD at or beyond `56a67e2e`
(see §Reading list); eight axis reports under
`docs/audit/meta-audit-2026-04-23/`; a tranche tree under
`docs/tranches/` in which B1, AZ-I, AZ-II, BA, BB are authored and
wave-split; `GESTALT.md` and `RISK-PERF-MATRIX.md` as current-state
canon; and `docs/instructions/` holding `README.md`, `PROFILING.md`,
`CHANGELOG.md`, and `tranche/` sub-edicts that govern orchestration.

This document is a briefing, not documentation. It is the prompt.

The user's archaic diction is deliberate voice, not AI artefact —
*begat, therein, thereof, insofar, hereof, hitherto, appurtenant,
assay, gestalt, indefatigably, thereupon, obstinancy, degust,
Herculean, befitting, exhortation, edict, explicate, divine,
parsimonious* — preserve it when quoting and match the register when
responding.

---

## Architectural invariants (non-negotiable)

The system's shape is declared — not negotiated. Every invariant below
is verbatim from a user message; the categorization is the synthesis.

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

## Execution discipline

The user repeats these in nearly every substantial directive. They are
not guidance — they are the admission criteria for work landing.

### §E1. No quick solutions, no workarounds

> "NO quick solutions, NO workarounds: idiomatic, gestalt approaches.
> This is a development product, architectural transpositions in the
> sake of elegance, simplicity, and performance above all are both
> necessary and desirable. NO legacy code."
> — session 32a, 2026-04-21T05:53:23Z (first appearance)
> — session 4be, 2026-04-21T18:09:10Z (repeated)
> — session 959, 2026-04-23T00:10:32Z (repeated)

### §E2. No deferrals, no partial analysis

> "No deferrals. No need to get a performance or testing baseline."
> — session 4be, 2026-04-21T07:16:13Z

> "Continue. Re-send out the agents if need be. No partial analysis."
> — session 4be, 2026-04-21T18:30:35Z

### §E3. No un-wired legacy cruft, no dead IIFEs

> "Ensure no un-wired legacy cruft, too. Path B. Pivot. Transient
> history if relevant. NO dead IIFEs. NO workarounds. Audit our
> original tranches plans to ensure we're not building up deferrals
> and legacy debt."
> — session 4be, 2026-04-21T18:51:58Z

### §E4. No patching generated.rs by hand

> "We cannot patch the generated.rs like this. Status?"
> — session 4be, 2026-04-21T18:06:44Z

(Generated files are always output of fresh regen — never hand-patched.
Transient compose-escape aliases are permissible only when SPEC
§Self-host circular-dependency-escape admits them and they are retired
post-regen.)

### §E5. Architectural transpositions for elegance, simplicity, performance

> "This is a development product, architectural transpositions in the
> sake of elegance, simplicity, and performance above all are both
> necessary and desirable."
> — recurring across all three sessions

### §E6. AZ-II is not optional; the escape is not a planning alternative

> "The AZ-II floor question is no longer open. Your prior requirement,
> 'full tape abrogation is a hard requirement,' should be treated as
> binding repo policy. `bbnf-tape-mini` should be removed from the
> planning surface in [AZ-II.md], [RISK-PERF-MATRIX.md], and
> [GESTALT.md]. If AZ-II.W2 actually fails in practice, that becomes a
> contingency decision then, not a declared floor now."
> — session 959, 2026-04-23T18:04:47Z

### §E7. Merge all parallel work; double-validate

> "Merge them all in, there should be no conflicts. And help me to
> understand and properly explicate for each question. We still have a
> VM? Rule curation what? Gorgeous-mirror what? Monolithic arena
> combinator fall back what? We shouldn't have an arena anymore,
> right? And then once merged in--double validate--clean up the
> worktrees."
> — session 959, 2026-04-23T04:07:09Z

### §E8. Output must be complete, not parsimonious

> "For such a large scale report, i cannot fathom why you're so
> parsimonious with output and reporting."
> — session 959, 2026-04-23T00:40:00Z

> "Our updates, augmentations, and refinements must not be half baked.
> Eschew ad hoc scripts for benching and testing, profiling, any tool
> chaining—unless genuinely valuable and not subsumed by a more modern
> and idiomatic approach"
> — session 959, 2026-04-23T00:40:00Z

### §E9. Ensure ALL sub-waves align — no drift across docs

> "Ensure ALL sub-waves are aligned."
> — session 959, 2026-04-23T18:25:03Z

> "Ensure all appurtenant doc items are updated. Any stale docs
> removed, too."
> — session 32a, 2026-04-22T19:54:30Z

### §E10. Kill ongoing rustc processes; first-principles assay

> "This has never taken so long previously--before our B0 changes,
> which were supposed to optimize this process, our builds were long
> but at least ocassionally reasonable. This needs to be rethought
> from first principles. No excuses or 'each has its own derive macro'
> expansion. Kill all ongoing rustc process and properly assay and
> addressed."
> — session 4be, 2026-04-22T15:20:00Z (and repeated 959, 2026-04-23)

### §E11. Zombies are not scope expansion

> "That's not scope expansion, that's zombie tasks. Kill the processes
> if they're not needed then."
> — session 4be, 2026-04-21T18:09:10Z

---

## Orchestration discipline

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

(User expects proactive status; never make the user ask twice.)

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

## Tone and voice

### §V1. Archaic diction is deliberate

The user's vocabulary — *begat, therein, thereof, insofar, hereof,
hitherto, appurtenant, assay, gestalt, indefatigably, thereupon,
obstinancy, degust, Herculean, thereof, befitting, exhortation,
edict, explicate, divine, parsimonious* — is deliberate voice, not AI
artefact. Preserve it when quoting. Do not smooth it out.

(MEMORY `feedback_archaic_diction`.)

### §V2. Independence over obedience

> "What is your obstinancy against actual, difficult, architectural
> change and improvement? Why even propose that--seriously, I'm not
> mad, just curious."
> — session 959, 2026-04-23T15:53:32Z

The user values an orchestrator that will argue back on technical
merit, not one that rubber-stamps softened plans. Treat a defensible
floor as a defensible floor, not a stopping point.

### §V3. Corrective is load-bearing

When the user catches drift, internalise the correction — do not
defend the prior framing. Examples:

> "The critique is correct. The meta-audit's macro verdict stands, but
> its execution framing should be tightened in five ways."
> — session 959, 2026-04-23T18:04:47Z

This is the voice to adopt when catching one's own drift.

### §V4. Senior performance engineer's judgment is the frame

> "Thoughts on the following. Judge fairly, from a senior performance
> engineer's perspective. Gestalt changes and architectural
> transpositions are likely required, when befitting"
> — session 32a, 2026-04-21T07:10:36Z

---

## Runway — the canonical sequence

Dependencies run left-to-right; BB may overlap AZ-II where file-bounds
are disjoint. Each tranche's owning spec is cited.

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

## Decisions the audit baked in

These are closed; do not reopen without new concrete contradiction.

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

## Original exhortations — verbatim

These are the load-bearing prompts that begat the arc. Reproduced
as-written.

### E1 — the AY-II open (session 4be, 2026-04-21T07:16:13Z)

> "Begin tranche AY-II. First, read docs/instructions/tranche/* and any
> appurtenant documentation therein. You must adhere to the edicts and
> precepts, particularly regarding indefatigability and
> parallelization--orchestration and your role thereof--exactly. No
> deferrals. No need to get a performance or testing baseline."

### E2 — the blocker-triumvirate edict (session 32a, 2026-04-21T05:53:23Z)

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

### E3 — grammar-derived, one path (session 32a, 2026-04-21T07:15:40Z)

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

### E4 — the meta-audit open (session 4be, 2026-04-22T18:39:48Z)

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

### E5 — the 8-agent Herculean fleet (session 959, 2026-04-23T00:40:00Z)

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

### E6 — the path-forward triple command (session 959, 2026-04-23T00:10:32Z / 00:11:55Z)

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

### E7 — the path-B pivot (session 4be, 2026-04-21T18:51:58Z)

> "Ensure no un-wired legacy cruft, too. Path B. Pivot. Transient
> history if relevant. NO dead IIFEs. NO workarounds. Audit our
> original tranches plans to ensure we're not building up deferrals
> and legacy debt."

### E8 — the AZ split (session 959, 2026-04-23T15:33:59Z)

> "Reasonable. Let's split AZ into I and II waves. Thereupon, run the
> docs/ streamlining pass."

### E9 — the tape-abrogation edict (session 959, 2026-04-23T15:53:32Z)

> "Full tape abrogation is a hard requirement. Is direct to struct
> projection not the biggest and most important performance gain we'll
> see? We're even getting off-track within this planning phase. What is
> your obstinancy against actual, difficult, architectural change and
> improvement? Why even propose that--seriously, I'm not mad, just
> curious."

### E10 — the auditor handoff (session 959, 2026-04-23T16:01:55Z)

> "I'd like to audit this entire process. Relay a prompt to me to
> handoff to an auditor for every facet herein. Not just the above
> documentation updates and tranche updates, but our original
> exhortation and edict set, our archaelogy process, our findings,
> gestalt, next-tranche, etc."

### E11 — the execution tightening (session 959, 2026-04-23T18:04:47Z)

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

### E12 — senior-engineer framing (session 32a, 2026-04-21T07:10:36Z)

> "Thoughts on the following. Judge fairly, from a senior performance
> engineer's perspective. Gestalt changes and architectural
> transpositions are likely required, when befitting"

(Followed by the full AY-II critique; see §Architectural invariants
§1–§11 for the extracted edicts.)

### E13 — the bbnf-tape-mini product decision (session 959, 2026-04-23T04:39:39Z)

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

### E14 — AY becomes AY-I/AY-II, AZ becomes BA (session 959, 2026-04-23T04:45:40Z)

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

### E15 — meta-prompt synthesis directive (session 959, 2026-04-23T18:47:54Z)

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

## Failure-mode guardrails — orchestrator drift the user has corrected

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

## Reading list — where to pick up

All paths absolute. Canonical state as of this synthesis (master HEAD
`56a67e2e` on 2026-04-23):

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

## How to use this meta-prompt

Paste this document into a fresh Claude Code session at the bbnf-lang
repo root. The receiving session will have the minimum viable
briefing to continue the arc without re-deriving it. The receiving
session should still read the canonical docs in §Reading list — this
meta-prompt is not a replacement for them; it is the discipline
overlay that governs how they are interpreted and how work against
them is admitted.

If you are the receiving session: begin by verifying master HEAD, the
eight axis reports, `GESTALT.md`, `RISK-PERF-MATRIX.md`, and the
tranche-owning specs exist and have not drifted since synthesis. Then
open the next work item in runway order — which, at synthesis time, is
B1.W0. Do not open a further meta-layer unless you have found a new
concrete contradiction.

**One quote, if you must reduce to one**:

> "Full tape abrogation is a hard requirement. Is direct to struct
> projection not the biggest and most important performance gain we'll
> see? We're even getting off-track within this planning phase. What
> is your obstinancy against actual, difficult, architectural change
> and improvement? Why even propose that--seriously, I'm not mad, just
> curious."
> — session 959, 2026-04-23T15:53:32Z

The load-bearing word is *obstinancy*. The corrective the user wants,
always, is against the orchestrator's obstinance toward the easy
answer.

---

## Provenance

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
