# SK-V24 — HANDOFF: the greenfield novum

Date opened: 2026-07-15. This document is both the PASS-ALPHA charter of SK-V24 and the
self-contained dispatch for the session that executes it. A fresh orchestrator handed this file
alone must be able to run the campaign to freeze.

---

## 0. Standing of the world at handoff

- Repo: `~/Programming/bbnf-lang`, branch master. The SK-V23 freeze commit is f913ea350
  ("freeze(sk-v23): the dual-repo tranche codex — SPEC.md frozen at 96"); HEAD at handoff is
  fe3e615d3 — csp-solver canonical-diff re-vendor commits from the sibling csc411 tranche
  landed atop the freeze and are NOT this campaign's concern. Only pre-existing playground
  junk is dirty (`M playground/vite.config.ts`, `?? playground/test-results/`); the archival
  tranche dispositions it.
- Three frozen prior campaigns, all tranche-development-only, none executed:
  - **SK-V21** (`restart/skinny/tranches/sk-v21/SPEC.md`) — the convergence-loop re-founding:
    performance/architecture adjudications, TW manifest, research/01-41, prototypes P1-P7.
  - **SK-V22** (`restart/skinny/tranches/sk-v22/SPEC.md`) — graduation & great archival:
    GW0-GW6, the archive mechanics (umbrella tag + 9 navigational tags + 5 rescue checks),
    the load-accounted graduation baseline (research/04), PT-R/PT-T uplift tracks.
  - **SK-V23** (`restart/skinny/tranches/sk-v23/SPEC.md`, 1,121 lines) — the dual-repo codex:
    TB1-TB6 + PT-R1-3/PT-T4-5, x86 abrogation normative, greenfield delineation §1, precept
    codex research/07 (57 precepts), 70-row ledger re-home research/09, adjudication packet
    research/10, BBNF-PT-F letter Appendix A (~line 950), forward prompt (~line 1074).
- Memory: `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/` —
  `project_skv21_freeze.md`, `project_skv22_graduation.md`, `project_skv23_codex.md`,
  and now `project_skv24_handoff.md`.

## 1. The pivot (binding; supersedes in part SK-V22 and SK-V23)

Three directives from the owner, 2026-07-15, verbatim in intent:

1. **We seek to really, truly, develop novel and new features for a greenfield approach to
   this repo.** Novelty is first-class, not a triage afterthought. The SK-V23 feature-fold
   lists (research/04, research/05, SPEC §6) are seed corpus, a floor — not a ceiling.
2. **Skinny is to be retired.** The SK-V22 premise — skinny graduates to tree of record —
   is SUPERSEDED. Skinny was always "a playground proof of concept to GUIDE and INFORM our
   totality spec" (the original charge). Its measured facts, cured walls, and negative
   results carry as evidence; its code does not graduate wholesale. The SK-V23 §1
   carry-vs-greenfield delineation (5 CARRY-with-fixes, 1 REWRITE-SHRINK) RE-OPENS as design
   space: every carry must re-earn its place inside a greenfield architecture, or die.
3. **The grand archival is done once our tranches-to-guide are fully formalized.** SK-V23's
   TB1-first ordering (archival as the opening act) is SUPERSEDED: formalize the codex FIRST,
   archive SECOND. The archival's MECHANICS survive intact — SK-V22's tags-before-removals
   sequence, umbrella tag, rescue checks, ~202G reclaim, branch-not-delete for legacy crates —
   only its position in the order moves.

**And the root of it all: this must be rooted in detailed research, analysis — critically,
performance testing and profiling.** Every pass's prototype fleet produces measured numbers
under the profiling standing law (§4 tail). A spec-only prototype is marked spec-only and can
never anchor a performance-convergence claim.

What is NOT superseded: the x86 abrogation (normative, both repos), the non-clobber law and
letter-placement law (§6), the precept codex (sk-v23 research/07), the profiling standing law,
the settled empirical spine (§4), and the SK-V22 archive mechanics as mechanics.

## 1.5 The execution amendments (owner dispatch, 2026-07-16 — binding, sworn)

The owner dispatched the campaign with amendments that bind over any conflicting clause below:

1. **DISPATCHED — maximal parallelism.** The batch-of-three rate discipline is relaxed to
   the workflow concurrency cap; fleets go wide, resumable-by-design.
2. **The return condition**: a swath of GENUINELY PERFORMANCE-PROVEN features — implemented
   and measured, not specified — plus a concrete, well-defined path forward to uplift this
   repo AND parse-that into a perfected union of a parser-generator in Rust, with derivative
   novelties suffused into parse-that's TS variant (a TS backend hereof comes eventually).
3. **Implementation-majority process**: little time on contrived gates or process; the
   majority on direct code implementation — always through agent orchestration — and visual
   verification (inspect expanded/compiled output; cargo expand, cargo asm, real bench
   tables). Adhere to the wave spec exactly once cut.
4. **The colocation grand edict — ALL file directories, frontend AND backend, recursively.**
   Components colocate with their sub-components, composables, skeletons, constants, styles —
   recursively for nested components. Only truly module- or global-level items live in a
   composables/-style dir therein. Long-running dirs must always be broken into common
   modules and encapsulated. For backend languages the edict abstracts befittingly: directory
   modules that own their submodules, constants, and kernels; no flat sibling sprawl; no
   god modules at any level.
5. **Extreme parsimony, fastidious care, every implementation**: KISS-forward solutions that
   reduce complexity and suffuse fewer lines of code; always consider the greater library and
   component picture.

## 2. The mission (the original gestalt charge, melded current)

Our aim is a **fully generalized parser generator that bests every SOTA parser for our
targeted structured data languages like CSS, JSON, etc.** — by a perfected union between
backend-agnostic code and an emitter specializing in a hybrid of Rust and hand-rolled ASM,
inspired by notions from the DAV1D project and the video/audio encoder realm, wherein parse
speed and optimization is of first-order priority.

The original charge, updated to the world as it now stands:

- **Review what's been done hitherto** — the original tranches, the restart/ pivot, skinny.
  This review is BANKED: SK-V20/21's 32-agent deep audit and the 41-report corpus are the
  cached intelligence. Cite it; do not mechanically re-run it. The 32-agent depth mandate
  applies instead to the NEW question: what does a greenfield totality look like when skinny
  is a retired informant rather than a graduating substrate?
- **The cutover question is answered: now.** There is no further skinny maturation. The
  greenfield IS the totality implementation, and this campaign formalizes its tranches.
- **The overfitting audit is standing law.** Recall what happened before the restart — total
  divergence from spec, hardcoded grammars, hardcoded backend files. Skinny itself still
  carries hardcoding residue (WART-C name-gates and kin), no type system (monotone fold
  only), partial/dead SIMD kernels, a prototype-grade Value API. The new codex must prove
  generality by construction: walls cured by design, not patch; no grammar-name gates; no
  JSON/CSS over-emphasis — the generator serves the grammar plane, and JSON/CSS are merely
  its first proving targets alongside TOML, CSV, and the fleet.
- **The meta-process audit is live.** Three campaigns have now frozen plans without a line of
  implementation landing. The loop must confront directly whether plan-stacking is itself the
  local maximum: SK-V24's codex must terminate in a dispatchable implementation program with
  born-RED gates, and the freeze criterion includes an explicit "why this plan will be
  executed rather than re-planned" adjudication.
- **parse-that**: the one-regex-home adjudication stands (canonical regex engine lives in
  parse-that; bbnf grows no combinator surface). What folds in/out, the parsing primitives,
  and the totality crate/module structure are first-class deliverables of this campaign —
  every level separates concerns; no god modules.
- **The core ASM and Rust emission needs to be greatly refined.** Optimization research
  ruthlessly investigated to be far beyond SOTA for this class of parser, including FULL type
  resolution and the most powerful inference reasonably possible — CSP, e-graph,
  Hindley-Milner, and novel hybrids thereof.
- **A robust Value API is a first-class requirement**: match and beat lightningcss, simdjson,
  yyjson, sonic-rs — lazy AND eager evaluation, a unified tape and backend layout, a unified
  path API, and direct-to-struct.
- **What is novel with our approach — are we approaching SOTA, best-in-class? How can we
  further that?** The differentiator question gets a named answer in the frozen SPEC.
- **Delineate any chronically deferred items and fold them in.** The 70-row ledger re-home
  (sk-v23 research/09) plus the 7 wave-owned residuals (§8) enter as design inputs; nothing
  re-defers without a named owning wave.
- **Recap ALL prompts and requests hitherto and ensure they've been addressed** — a standing
  freeze-gate obligation.

NO quick solutions, NO workarounds: idiomatic, gestalt approaches. This is a development
product; architectural transpositions in the sake of elegance, simplicity, and performance
above all are both necessary and desirable. NO legacy code.

## 3. The method (the convergent multiagent design loop — operative protocol)

Resolve the stated design problem completely: a specification and wave set at 100%
convergence that survives adversarial audit. We seek TRULY novel, elegant, greenfield
approaches, developed in a highly multi-dimensional, multiagent, iterative manner. Track
partial progress throughout; it counts toward the resolution only when it implies exactly
the resolution.

### Round zero: the portfolio

Before the first pass, a Fable agent mints the approach portfolio: genuinely orthogonal
formulations of the problem. Different architectural centers, different substrates, different
decompositions. Reject rewordings of one idea posing as several. Fable generates these
orthogonal approaches and owns ALL orchestration logic; implementation is delegated to Opus
or Sonnet for workflow fanout, and every spawn declares its model explicitly. Maintain an
explicit registry of approach families, grouped by the design idea in use; two routes that
share a mechanism share a family. At least one family must be centered on features no prior
pass proposed (per §1 directive 1).

### The pass

Deploy each pass as its own workflow; dissect and synthesize upon each pass. A pass consists
of the following, done sequentially in an iterative loop:

1. **RESEARCH.** Deploy up to 8 agents in parallel (batched three concurrent against the rate
   wall) across the web, the extant codebase, the tranches hitherto, and the sibling
   constellation. Early-round researchers receive the task statement and their family charter
   alone; the currently favored approach stays withheld.
2. **SYNTHESIZE.** One agent distills the results into a cogent specification and plan: one
   spec per family while routes remain incompatible.
3. **PROTOTYPE.** A fleet greenfield-brainstorms and test-implements against the begotten
   specification, sometimes with concrete implementation, sometimes with prototype-augmented
   specification. A prototype either runs or is marked spec-only. Running prototypes are
   MEASURED under the profiling standing law; numbers, not adjectives.
4. **CRITIQUE.** An adversarial fleet hardens, challenges, and refines each begotten item
   against the failure-mode checklist: vacuous convergence, spec-cites-itself circularity,
   gates that cannot fail, the elegant-reduction trap (a spec whose load-bearing step reads
   "and then the hard part" remains unconverged), legacy aliases, masked fallbacks,
   unverified gestalt, consumer-less substrate. Each hardened item returns a percentage of
   convergence alongside a critical analysis enumerating the exact open gaps.
5. **AGGLOMERATE.** A final synthesizing agent folds the pass into the registry, advancing,
   banking, blocking, or retiring each route, and begins the loop again with the newfound
   contextual information.

### Portfolio management

- Keep several incompatible routes alive through multiple passes. Cross-pollinate only after
  independent agents have developed each far enough to expose its real strengths and gaps.
- Elegance of the specification earns a route nothing on its own. A route that stalls at a
  missing primitive equal in difficulty to the original problem is BLOCKED: mark it, and
  assign further agents only when someone proposes a materially new mechanism, invariant, or
  construction.
- When many agents converge to one family, redirect some toward underexplored formulations.
- Require concrete artifacts: running prototypes, probes, measured numbers, named
  counterexamples to proposed designs. Reject status reports and vague optimism.
- The root agent repeatedly synthesizes, challenges, redirects, and launches new passes. A
  stalled pass demands a fresh formulation, and blocked families reopen only upon a genuinely
  new mechanism.

### Convergence

Convergence is earned. 100% means zero enumerated open gaps, survival of a fresh adversarial
audit by agents who did not author the winning spec, and two consecutive clean passes.
Whereupon 100% convergence: stop, and develop out that exact tranche plan and wave set(s) to
implement, refine, and align, with born-RED gates, π obligations, and a disposition for every
banked route.

### Floors and fences

- Run at least three full passes before contemplating convergence.
- Web research supplies background, prior art, and the state of the art; the design verdict
  comes from the codebase, the prototypes, and the critiques.
- Every design decision and every cognitively complex judgment routes through Fable and the
  frontend design plugin; every mechanical sweep routes to Opus or Sonnet.
- Return the converged plan surviving audit. Failing that, return the strongest rigorously
  converged core and its exact remaining gap. A best-effort summary, or an explanation of why
  the problem is difficult, is an unacceptable return.

## 4. The empirical spine (settled facts; carry as evidence, re-measure to dispute)

Ratio-certified under the profiling standing law. A route disputing any of these must bring
paired measurements, not argument:

- **Graduation baseline** (sk-v22 research/04, load-accounted): parse_tape beats sonic-rs
  2.10×/1.41×/2.06× (twitter/canada/citm) cold cycles/byte; CSS beats lightningcss
  2.07–3.47×; canada eager fused LOSES 1.20–1.25× (Eisel–Lemire owns it). parse_tape 1.5654
  cyc/B twitter meets the AU 1.63 floor.
- **vshrn intrinsic obviates hand asm** for the mask-narrow idiom: +35% over scalar, beats
  the asm variant by ~25%; register-file crossings bind, not ALU. The dav1d discipline
  stands — intrinsics default, per-kernel asm gate, checkasm-liveness — but asm must EARN
  each kernel.
- **AoS over SoA** for tape nodes (SoA lost 60–76%). **Fold-over-lazy-cursor REFUTED**
  (2.3–4.9× behind sonic). **Pre-scan presize DELETE** (7th consecutive loss).
  **N\*=3 kernel crossover** for SIMD dispatch width.
- **shufti/truffle NEON prefilter**: 18–67× on skip-heavy inputs with fused GATE=16; dense
  inputs recover to 0.95–1.15×. **Mula/Lemire SIMD digit parse** +53.8% cyc/B feeding
  Eisel–Lemire.
- **LR/packrat NO-GO** (dated 2026-07-05). **V8 discipline** for TS surfaces: same-map,
  zero-dictionary-mode, monomorphic call sites.
- **The master pathology**: substrate-without-consumer, 14 instances across 18 original
  tranches; profile-driven work went 2/2, spec-driven 12/14 dead. Cure: Y.13
  consumer-invariant runtime gates — no substrate lands without its consumer in the same wave.
- **Sheets carve-out analysis**: the neutral NodeClass/Classify surface does not carry the
  39-kind sheets tape (13/13 byte-parity proof). Under retirement this is a design INPUT
  (the generality boundary it demonstrates), not a binding carry disposition.

**The profiling standing law** (replaces the retired loadavg<2 gate; the host runs loadavg
6–8 from the owner's own apps): PMU cycles/byte via proc_pid_rusage; paired per-sample
interleaving of the two arms; N=200 cold-per-parse (no warm/cached benches, ever);
multi-window agreement; per-block loadavg recorded. Ratios are certified; absolutes are
directional. Quiet-host certification is an execution-time gate, not a design-time blocker.

## 5. What re-opens, what stands

RE-OPENS (design space for the loop):
- The carry-vs-greenfield delineation, in totality (sk-v23 SPEC §1 is an opinion, not law).
- The tranche codex structure itself (TB1-TB6 ordering superseded; the new codex may keep,
  reshape, or discard the TB lineage — the archival tranche formalizes LAST and executes
  after freeze).
- The novel-feature surface: value plane, type plane (full inference: CSP/e-graph/HM
  hybrids), emit spine, SIMD dispatch, grammar-language features, tooling/QoL, the
  playground's greenfield successor concept.
- The parse-that proposal tranches (PT-R1-3/PT-T4-5) — re-cut as needed against the new
  ordering; the PT-R.5 repoint timing moves with the archival.

STANDS (law):
- x86 abrogation, normative both repos: aarch64/Apple-M only; 4-layer enforcement (structural
  token gate primary, workspace lints, aarch64 CI assertion, emitter stops stamping AVX2).
- One regex home: parse-that. bbnf grows no combinator surface.
- The precept codex (sk-v23 research/07, 57 precepts) and the process surgery: Y.13 consumer
  gates, one challenge wave, cold-only benches, no-legacy, born-RED gates.
- The archive MECHANICS (sk-v22 SPEC): umbrella tag pre-graduation + navigational tags +
  rescue checks; tags-before-removals; archive/legacy-crates as a BRANCH; ~202G reclaim.
- The backbone constraint: value.js, latex-paper, keyframes suites are close gates for any
  parse-that proposal.

## 6. Operational law (hard-won; every workflow script honors these)

1. **NOT an implementation phase. Tranche development only.** Prototypes are analysis
   instruments: worktree-isolated, measured, never landed.
2. **Model routing**: Fable (the core model) orchestrates, designs, synthesizes,
   adjudicates; Opus/Sonnet take the fan-out. Every spawn declares its model explicitly.
3. **Batches of three concurrent** against the rate wall, even when 8 are deployed per stage.
4. **The worktree write-trap**: worktree-isolated agents' Writes to main-repo absolute paths
   land in the worktree. Every worktree brief carries the delivery rule: after writing,
   `test -f MAINPATH || cp` from the worktree, verify with `ls -la`.
5. **No nested backticks** inside workflow template-literal prompts (they terminate the
   string and kill the script parse).
6. **StructuredOutput retry-cap deaths** often leave report files intact on disk — check
   before redispatching; instruct returns "strings under 80 words."
7. **Workflow resume**: TaskStop → edit script (unchanged agent prompts stay byte-identical
   for cache replay) → relaunch with `resumeFromRunId`.
8. **Cargo discipline**: one invocation per CARGO_TARGET_DIR at a time; >60s runs go
   run_in_background + Monitor (never poll); long outputs redirect to a file once, grep after.
9. **The non-clobber law**: ~/Programming/parse-that is READ-ONLY to this campaign. Active
   value.js/keyframes sessions carry untracked WIP there. Prototype against scratch COPIES.
   The single permitted artifact is one additive coordination letter (BBNF-PT-* lineage),
   placed ONLY by the owner or a parse-that-side session — never by a bbnf agent.
10. **Documentation home**: `restart/skinny/tranches/sk-v24/` — this HANDOFF, research/,
    prototypes/, critiques/, CONVERGENCE-LEDGER.md, and SPEC.md at freeze. Commit the ledger
    per pass; commit the SPEC at freeze; memory index updated at freeze.

## 7. The evidence corpus (cached intelligence — cite, don't re-derive)

- `restart/skinny/tranches/sk-v21/`: SPEC (adjudications), GESTALT-SYNTHESIS,
  DEFERRED-LEDGER, research/01-41 (incl. the archaeological dig and the 40K-line hand-seam
  census research/38), prototypes P1-P7, critiques.
- `restart/skinny/tranches/sk-v22/`: SPEC (graduation + archive mechanics), research/01-07
  (04 = the graduation baseline), PASS-ALPHA (the five owner corrections, incl. "uplift
  means a FULL SOTA AUDIT").
- `restart/skinny/tranches/sk-v23/`: SPEC, PASS-ALPHA, CONVERGENCE-LEDGER, research/01-10
  (01 x86 census; 02 SIMD frontier; 03 greenfield delineation; 04/05 feature brainstorms;
  06 coordination; 07 precept codex; 08 tranche architecture; 09 ledger re-home, 70 rows;
  10 adjudication packet), prototypes PROTO-PB-1..6+3b, critiques.
- Workflow journals: `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/*/workflows/`.
- Sibling constellation (READ-ONLY): ~/Programming/parse-that (rust/ + typescript/),
  ~/Programming/value.js, ~/Programming/latex-paper, ~/Programming/bbnf-buddy (EXISTS,
  active), ~/Programming/csc411/CSC411_HW2_ProgrammingQuestion (csp-solver canonical).

## 8. Gates and residuals carried

- **The 3 adjudication checkboxes** (sk-v23 research/10: letter contingent-framing,
  NEIGHBOR-GRACE, release.yml aarch64-only) are NOT ratified and must be RE-CUT by this
  campaign under the new ordering before the owner gate re-presents.
- **The BBNF-PT-F letter** (sk-v23 SPEC Appendix A) was never delivered. Re-derive it under
  SK-V24 (the PT-R.5 repoint timing moves with the archival re-sequencing); placement law
  unchanged.
- **7 wave-owned residuals** re-home as design inputs: PB-1 gate-cost re-measure, greenfield
  re-proof, retirement parity, aggregate falsifier, quiet-host re-runs, cold canada capture,
  archival audit vs research/09 pre-map.

## 9. The deliverable and the freeze criterion

The frozen `sk-v24/SPEC.md`: the complete tranche codex for the greenfield totality —
novel-feature folds first-class; the grand archival tranche formalized LAST in the document
and ordered last-opening in execution; parse-that coordination re-cut; every tranche with
born-RED gates and π obligations; a disposition for every banked route; a named answer to
the differentiator question; the all-prompts-recap audit; and THE FORWARD PROMPT at the tail
as the self-contained execution dispatch.

Freeze requires: ≥3 full passes; zero enumerated open gaps; a fresh adversarial audit by
non-authors; two consecutive clean passes; and the meta-adjudication of §2 — why this plan
gets executed rather than re-planned. Failing full convergence, the return is the strongest
rigorously converged core and its exact remaining gap — never a best-effort summary.
