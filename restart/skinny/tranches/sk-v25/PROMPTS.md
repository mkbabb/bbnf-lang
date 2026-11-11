# SK-V25 PROMPTS — the verbatim binding texts

These prompts are law as written. They are not summaries to be re-derived. Apply the
transposition table first; everything else is verbatim from the owner.

## The transposition table (binding)

| Where a prompt says | The Codex session reads |
|---------------------|-------------------------|
| Fable | ChatGPT Sol 5.6 |
| Opus | ChatGPT Luna 5.6 |
| the frontend design plugin (DesignSync) | the session's design-review facility (or the strongest available design-judgment route through Sol 5.6) |
| Claude Code harness mechanics (Workflow/journal/cron) | the Codex harness's equivalents, preserving the durability pattern (INVENTORY.md §F) |

Every spawn declares its model explicitly rather than inheriting the session's — this line
appears inside the prompts and is now ALSO a proven necessity (HANDOFF.md §0): inheritance
is how the entire prior campaign lost its judgment tier.

---

## Prompt I — Design and iteration: the convergent multiagent design loop

Resolve the stated design problem completely: a specification and wave set at 100% convergence that survives adversarial audit. We seek TRULY novel, elegant, greenfield approaches, developed in a highly multi-dimensional, multiagent, iterative manner. Track partial progress throughout; it counts toward the resolution only when it implies exactly the resolution.

### Round zero: the portfolio

Before the first pass, a Fable agent mints the approach portfolio; and for design, leverages the frontend design plugin exactly: genuinely orthogonal formulations of the problem. Different architectural centers, different substrates, different decompositions. Reject rewordings of one idea posing as several. Fable generates these orthogonal approaches and owns ALL orchestration logic; implementation is delegated to Opus for workflow fanout, and every spawn declares its model explicitly. Maintain an explicit registry of approach families, grouped by the design idea in use; two routes that share a mechanism share a family.

### The pass

Deploy each pass as its own workflow; dissect and synthesize upon each pass. A pass consists of the following, done sequentially in an iterative loop:

1. RESEARCH. Deploy up to 8 agents in parallel (batched 5-6 concurrent against the rate wall) across the web, the extant codebase, the tranches hitherto, and the sibling constellation. Early-round researchers receive the task statement and their family charter alone; the currently favored approach stays withheld.
2. SYNTHESIZE. One agent distills the results into a cogent specification and plan: one spec per family while routes remain incompatible.
3. PROTOTYPE. A fleet greenfield-brainstorms and test-implements against the begotten specification, sometimes with concrete implementation, sometimes with prototype-augmented specification. A prototype either runs or is marked spec-only.
4. CRITIQUE. An adversarial fleet hardens, challenges, and refines each begotten item against the failure-mode checklist: vacuous convergence, spec-cites-itself circularity, gates that cannot fail, the elegant-reduction trap (a spec whose load-bearing step reads "and then the hard part" remains unconverged), legacy aliases, masked fallbacks, unverified gestalt, consumer-less substrate. Each hardened item returns a percentage of convergence alongside a critical analysis enumerating the exact open gaps.
5. AGGLOMERATE. A final synthesizing agent folds the pass into the registry, advancing, banking, blocking, or retiring each route, and begins the loop again with the newfound contextual information.

### Model routing and concurrency

The core model (Fable) owns orchestration, synthesis, adjudication, and every cognitively complex call; ALL design routes through Fable and the frontend design plugin (DesignSync). Opus to take the workflow fanout and the mechanical sweeps, and every fanout spawn declares its model explicitly rather than inheriting the session's. Dispatch in batches of 5-6 concurrent agents to stay under the rate wall. Use Fable judiciously for all problems of complexity, novelty, and creativity. Opus for implementation only.

### Portfolio management

- Keep several incompatible routes alive through multiple passes. Cross-pollinate only after independent agents have developed each far enough to expose its real strengths and gaps.
- Elegance of the specification earns a route nothing on its own. A route that stalls at a missing primitive equal in difficulty to the original problem is BLOCKED: mark it, and assign further agents only when someone proposes a materially new mechanism, invariant, or construction.
- When many agents converge to one family, redirect some toward underexplored formulations.
- Require concrete artifacts: running prototypes, probes, measured numbers, named counterexamples to proposed designs. Reject status reports and vague optimism.
- The root agent repeatedly synthesizes, challenges, redirects, and launches new passes. A stalled pass demands a fresh formulation, and blocked families reopen only upon a genuinely new mechanism.

### Convergence

Convergence is earned. 100% means zero enumerated open gaps, survival of a fresh adversarial audit by agents who did not author the winning spec, and two consecutive clean passes. Whereupon 100% convergence: stop, and develop out that exact tranche plan and wave set(s) to implement, refine, and align, with born-RED gates, π obligations, and a disposition for every banked route.

### Floors and fences

- Run at least three full passes before contemplating convergence.
- Web research supplies background, prior art, and the state of the art; the design verdict comes from the codebase, the prototypes, and the critiques.
- Every design decision and every cognitively complex judgment routes through Fable and the frontend design plugin; every mechanical sweep routes to Opus
- Return the converged plan surviving audit. Failing that, return the strongest rigorously converged core and its exact remaining gap. A best-effort summary, or an explanation of why the problem is difficult, is an unacceptable return.

---

## Prompt II — Tranche formulation: the post-tranche audit and next-tranche development prompt

This is NOT an implementation phase. Tranche development only. No source edits land from this prompt. The deliverable is the next tranche, fully formed: plan folder, wave specs, gates, dispositions.

### Mission

DEEPLY audit our original plan and the waves thereof, alongside all changes made hitherto, with 32 agents. Devise the path forward: audit the landed changes and the remaining plan; recapitulate our original prompts, plans, and precepts; verify every one has been addressed or carries an explicit ledger row with an owner. Form the next tranche from what the audit surfaces.

### Standing edicts

- NO quick solutions, NO workarounds: idiomatic, gestalt approaches. This is a development product; architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable.
- NO legacy code. Clean breaks: no aliases, no migration shims, no dual paths, no masking fallbacks.
- Delineate every chronically deferred item and every deferred item and fold them into this tranche as DECIDED rows: build, fold, or retire with rationale. Re-booking is forbidden. A chronic that has ridden two or more closes un-decided is a disease row, and deciding it is a wave of its own.
- Recap ALL of our prompts and requests hitherto and ensure they've been addressed. An unaddressed ask becomes a registry row with an owning wave. Silent drops are forbidden.

### Orchestration

Treat the 32 agents as a steerable budget. Assignment follows the registry, round over round; leave no lens permanently staffed.

- Open with a genuinely diverse portfolio of audit lenses: plan-vs-landed diff, gate soundness (can each gate actually fail?), gestalt read against per-mechanism PASS, the chronic and disposition ledgers, prompt-recap completeness, consumer truth (import graph and registry both), performance, accessibility, doc and canon drift, dead-code and dual-path census, cross-repo asks and consumes.
- Withhold the tranche's favored success narrative from most auditors. Independence in the early rounds keeps the fleet from converging on a confirmation of the close.
- Maintain an explicit registry of finding families, grouped by the underlying defect mechanism. Two findings that share a mechanism share a family, however differently worded. When many auditors converge on one family, redirect the excess toward underexplored lenses.
- Audit adversarially throughout. Check every "done" claim against the known close-class lies: green-over-broken, vacuous-green gates, declared captures missing on disk, masked fallbacks, alias smuggling, re-booked chronics, per-mechanism green over gestalt broken.
- Require concrete deliverables: file:line evidence, a failing probe, a reproduction, a named defect row. Reject status reports, vague optimism, and any claim that an unverified global property is "routine."
- Decompose any finding equivalent in strength to "redo the tranche" into wave-shaped rows; only at that grain can it be scheduled.
- The root agent repeatedly synthesizes, challenges, redirects, and launches new rounds. Do not stop after the first sweep. The registry is stable when two consecutive passes surface nothing new.

### Model routing and concurrency

The core model (Fable) owns orchestration, synthesis, adjudication, and every cognitively complex call; ALL design routes through Fable and the frontend design plugin (DesignSync). Opus to take the workflow fanout and the mechanical sweeps, and every fanout spawn declares its model explicitly rather than inheriting the session's. Dispatch in batches of 5-6 concurrent agents to stay under the rate wall. Use Fable judiciously for all problems of complexity, novelty, and creativity. Opus for implementation only.

### Partial progress

Track partial progress in the registry; discard nothing. Folding is a decision. Every partial, banked, or abandoned item receives a terminal disposition: folded into a named wave, banked with a named re-trigger, or retired with rationale. Counting a partial as done is the close-class lie, and it is forbidden.

### Return contract

Return only when the next tranche is fully formed: plan folder; wave specs with acceptance gates, born RED wherever the defect is live; π and DELTA obligations for every visual claim; a disposition for every chronic, every deferred item, and every prompt-recap row. An inventory of problems without the tranche that resolves them is an incomplete return. If a genuine blocker prevents full formation, return the strongest rigorously converged tranche core and its exact remaining gap.

---

## Prompt III — The value.js tranche prompt (binds the dogfooding arm)

Develop out the next tranche, would should include coordination and specific waves for our library items that use value.js and keyframes.js—and the other consumers that leverage that repo, too—as there's a tight coupling between the following: value.js, keyframes.js, and parse-that. Each library should be uplifted and scrupulously analyzed. Look to the last many tranches within EACH repo as one large set of workflows with fable—our library and our demo set should be then another series of workflow and tranche development processes.

In particular, we've had several issues that were addressed ad hoc a few tranches hence: mixColors, parseCSSValue. value.js owns the core CSS, most modern as of July 2026 with the experimental and chrome-specific features that have stabilized: ensure total and complete specification coverage, idiomatic and performant parse-that usage: parse-that itself has had a few developmental tranche sets begat in bbnf-lang, like tape adoption for the TypeScript implementation—what might we do to reduce allocations and increase performance to leverage the modern v8 engine as of July 2026? Our extant parser seems to be a custom, non-parse-that implementation—why was this done, in what tranche, and what are the performance benefits/downsides to such an approach. To me, the extant parser is unreadable and could be greatly uplifted by proper parse-that adoption.

At least, that's how our parsing facilities should be. keyframes.js itself needs to be ruthlessly examined in its parsing facilities: value.js should own the core CSS spec, keyframes the animation and keyframes-specific components (or perhaps all of the parsing is done by value.js, and keyframes.js uses use as a library? if we do go the split parsing strategy, as we do now, we must overhaul the directory structure between the two libraries totally, in both the compiler/parser AND all places elsewhere: achieve isomorphism and directory perfection within keyframes.js, too).

Mark me now: keyframes.js AND value.js are to be ruthlessly scrutinized at the library level, but frontend work should focus on value.js for the below. For example, why does keyframes.js have a superfluous src/animation structure—if animation is the only top level, why have it? Most of the sub-modules of keyframes.js need to be completely re-organized and re-structured at every level: with an emphasis on coherence and isomorphism between value.js and keyframes.js in an abstract facility.

For example, this /Users/mkbabb/Programming/keyframes.js/src/animation/internal—I don't like this at all.

All keyframes.js specific waves should, too, reference and examine their most recent tranche set, too: this most recent value.js tranche will own and direct all keyframes.js library items herein, though—the next proper keyframes.js-owned tranche will adapt accordingly. Be not afraid of major architecture changes so long as core features are not lost.

And for value.js, and keyframes.js—I don't like things like subpaths/ as a module. Code smell supreme. NO SHIMS.

—

Further, our color specification implementation, adoption, and facilities should best SOTA. We should have near perfected zero-alloc color facilities for all spaces, and transforms betwixt the two; our iterative color out of gamut algorithm must be ruthlessly interrogated.

—

Ensure that extreme parsimony and fastidious care is made for every implementation: seek KISS-forward solutions that reduce complexity and suffuse fewer lines of code: consider the greater library and component picture. Adhere to the wave spec exactly. Spend little time on contrived gates or process and the majority of it on direct code implementation—always done through agent orchestration—and visual verification.

Any notes that are root-level glass-ui defects should be noted and batched, and then sent to the working glass-ui agent. Ensure a great deal of care is made to not interrupt that ongoing process until we've isolated large swaths of features, written fully precepts/ compliant wave addenda thereof, and targeted their exact defects hereof.

—

Our cards, our components, and the design affordances, design hierarchy, margins, paddings, dividing lines, small UI elements, should all be audited, challenged, and refined to have a sense of Aristotelian proportionality, alongside grand glass-ui suffusion and affordance. Mark, too, any superfluous, duplicative, or distracting UI elements that are rife for removal; Mark other items of the converse, whereof more affordance may be necessary.

Our prior tranche(s) were unable to properly validate and audit our frontend as it was broken pre glass UI 7.0.0—that must be carried forward (all previous items in the last many tranches) and audited now.

—

Further, our frontend structure--and this is a grand edict for ALL file directories--needs to be wildly and scrupulously analyzed: components should be COLOCATED with their sub-components, composables, skeletons, constants, etc (and this should be done recursively for nested components).

Composables that are truly module-level or global-level—and other dirs of that nature—can be found within a composables/ dir therein, but otherwise they're to be COLOCATED--same for styles, etc.

Long running dirs must and always be broken into common modules and encapsulated thereof.

Similar treatment and enforcement should be applied to all backend files, too—though abstracted and made befitting for those languages and implementations.

—

Better utilize and leverage fable for all work of complexity, design, and auditing. Including your begotten DAG (modules and components herein AND our constellation of deps like keyframes.js, parse-that, atlas, etc), library, and component analysis for the distillation, modularization, colocation, and perfection thereof.

Further, we must properly and maximally parallelize this auditing task. Agglomerate items as you see fit.

---

## Prompt IV — The structure edicts (library + component DAG perfection)

This includes deep library work: any and all deferred colocation and modularization work must be addressed--our massive explosion of module, file, and directory structure must be finally settled, with better grouping and encapsulation in both the library, and the frontend/demo components hereof; refer to glass-ui for the flattening and component structuring idioms

For example, long running directories should be pruned, or if those files are necessary, potentially agglomerated and better organized at the function/class level; long running directories should be grouped into logical sub-modules: too macro of granularity, and we get god-modules; too small of a granularity, and we get sand—we want to have a goldilocks of files and modules.

Grouped files in a module should always have their module name stripped in the file—as an abstract and generalized de-duplication mechanism: for example, if src/animation/compile/easing has sub-files of "easing-option" "easing-config", whose would be renamed as "option" and "config"

Tests should always be NOT co-located and found within source files—always displaced into a file structure isomorphic to the source, but for tests.

More code and complexity is likely not always better: be pithy, laconic, and fastidious in your analysis.

To accomplish this, a DAG of both our library and component structures must be created and ruthlessly analyzed: each and every node and edge, and cycles thereof—in batches of a dynamic and bespoke size pursuant to those graph clusters—shall be viewed thrice in the following fashion: two fresh perspectives of Fable agents, both which assume the graph structure is WRONG; the third, a Fable agent, serves as the adjudicator and proves or disproves those findings.

This should be done in an iterative and encapsulated fashion until library perfection, readability, and DIRIGIBLITY is reached.

- NO quick solutions, NO workarounds: idiomatic, gestalt approaches. This is a development product; architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable. Breaking library changes are allowed. Pruning and deletion of entire sectors and modules is granted totally IFF our findings are their vacuity or superfluity—consumer count is NOT enough.
- NO legacy code. Clean breaks: no aliases, no migration shims, no dual paths, no masking fallbacks.

---

## Prompt V — The re-adjudication order (the campaign's founding text)

It's become known that all Fable-subagent flows have hitherto never been fable. We seek to re-do that work in the following way: ALL of our workflows and work hitherto is to be analyzed with TWO fable critics that assume outright the work is wrong: a third fable adjudicator shall agglomerate and PROVE which facts are correct/incorrect, forming a lock/work item apotheosis.

This is to be done by another session, in Codex this time, with Fable replaced by Chat GPT Sol 5.6, and Opus by Chat GPT Luna 5.6. This is a long horizon session, and we MUST include the above NOT completed work, too, such that the tail does not fall off.

The parse-that uplift shall be done FIRST, and mark the most recent progress items done within that repo: parse-that and bbnf-lang are inextricably linked, mark that constellation. parse-that is inextricably linked with keyframes.js and value.js.

Mark these items from the most recent value.js tranche development, too, to see how we're planning to USE these parsers, and dogfood them insofar as developing and brainstorming new features into our perfected union.

For parse-that, isomorphism between the TS and Rust implementations is paramount--though the regex and lowlevel facilities should NOT be duplicated, for example.

For both bbnf and parse-that we must brainstorm and develop new, novel, elegant, and performant features.

But, too, achieve the most recent perfected module structure and DAG of those modules IN repo using the value.js and keyframes.js most recent's tranche development.
