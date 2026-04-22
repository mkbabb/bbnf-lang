# Research — Orchestrator Invocation for Architecture Panoplies

Use this file to launch an architecture research wave. A research
wave precedes plan-authoring when a tranche's scope is open-ended
enough that the design space needs exploration before phases and
hard gates can be committed. Six worktree-isolated sub-agents
brainstorm in parallel; the orchestrator peer-reviews and
synthesises.

Operational protocol lives in `docs/instructions/README.md`. This
file layers the research-specific contract on top — what each
sub-agent receives, what they must produce, what counts as a
novel finding, and how the orchestrator verifies against prior
tranches before folding findings into a plan.

## Sub-agent fan-out

Six sub-agents per wave, isolated worktrees, fastidiously disjoint
research angles. The concrete angles are chosen per tranche, but
the shape is canonical:

- **Four agents** deepen the current architectural substrate —
  for example caching and prefetching, SIMD and scanner kernels,
  bit-packing and information density, parallelism and
  algorithmic transpositions compatible with the current
  substrate.
- **Two agents** propose architectures that depart from the
  current substrate entirely, bringing a distinct thesis each.
  The orchestrator seeds them with different starting points so
  they do not converge on the same answer.

Every agent produces **at least one novel idea** grounded in
specific technical depth — ISA manuals, bit layouts, cache
uarch, measured profile evidence — not generic textbook advice.

## Orchestrator prompt

Paste the block below. Substitute `{LETTER}` and the research
angles. Angles are tranche-specific — this template lists the
standing set; the orchestrator swaps them as the tranche requires.

---

Launch an architecture research wave for tranche `{LETTER}`.

You are the orchestrator. Read `docs/instructions/README.md` and
this file before dispatching. Dispatch six parallel sub-agents in
isolated sibling worktrees, each with self-contained context and
explicit, fastidious file bounds. Every agent reads the current
state of the codebase and relevant prior tranches; every agent
produces one or more novel ideas grounded in a saved artefact,
an ISA primitive, or a specific bit-layout sketch. Idiomatic,
gestalt approaches only — no quick solutions, no workarounds.
Depart from the current architecture where elegance, simplicity,
or performance demands it; preserve it where the thesis does not
beat the baseline on evidence.

Canonical angles (adapt per tranche):

1. **Tape density + information pooling.** Bit-layout audit;
   pool the CSP solver, e-graph facts, and structural-mining
   signals into globally-informed tape decisions. How far beyond
   the current fixed-record can we go without losing flatness?
2. **Modern SIMD beyond memchr.** Higher-dimensional SIMD
   facilities the codebase is leaving on the table — structural
   bitmaps, CLMUL, PEXT/PDEP, vpshufb, NEON tbl multi-register
   lookup, SVE2 match, Apple AMX. ISA-level reasoning.
3. **Cache locality, prefetching, branch prediction, memory
   management.** Codegen-directed shaping — not generic textbook
   cache tuning.
4. **Parallelism and novel algorithmic approaches compatible
   with the current substrate.** Skeleton/payload fission,
   document-level fork points, speculative parsing, pipeline
   staging.
5. **Novel non-current-substrate architecture (thesis A).**
   Columnar, SoA, kind-partitioned, or another concrete
   substrate departure. Concrete data layout + access API.
6. **Novel non-current-substrate architecture (thesis B).** A
   different thesis from agent 5 — content-addressed DAG,
   e-graph-native, log-structured, persistent-rope, zero-copy
   schema. The orchestrator seeds agent 5 and agent 6 so they
   do not converge.

For each agent, the prompt includes: the angle headline, the
seeded invariants (tape-first materialisation, typed AST parity,
one access API, no fallbacks, no legacy, idiomatic), the specific
files to read, the set of prior tranches relevant to their angle,
and the required deliverable shape (one markdown document under
1200-1400 words: angle, motivation tied to saved evidence, the
novel idea as a concrete sketch, interaction with existing
subsystems, honest risks, measurement plan).

Save every sub-agent's verbatim deliverable to
`docs/tranches/{LETTER}/research/NN-topic.md`. Do not paraphrase
— the source material is what the agent actually produced.

After all six complete, peer-review fastidiously:

- Cross-score each novel idea on feasibility, elegance, and
  impact. Reject ideas unbacked by saved evidence.
- Archaeology: grep `docs/tranches/` and the git log for prior
  attempts of each idea. Name the commit that added and the
  commit that deleted the prior attempt. If the concept was
  tried and failed, identify the specific failure modes and
  state which of them the new proposal addresses.
- Surface duplication and subsumption between proposals. The
  synthesis does not carry six independent proposals — it
  picks the ones that compose and drops the ones that another
  proposal subsumes.
- Present the synthesis to the user for discussion before
  folding into a tranche plan. The research wave produces
  proposals; the next step authors or refines a plan, and that
  step requires user sign-off.

Report to the user at wave completion with: (a) the synthesis,
(b) the archaeology trail per proposal, (c) the composition map
showing which proposals stack and which subsume, (d) an honest
stance on which belong in the current tranche versus a follow-on.

---

## Sub-agent dispatch template

Each sub-agent receives a self-contained prompt. This is the
template; fill in the bracketed fields per angle.

```
You are an architecture research sub-agent for tranche
{LETTER}. Your angle: {ANGLE_HEADLINE}.

Deliverable: a tight, technical document (≤ 1200 words, up to
1400 if the angle demands ISA-level detail) containing at least
one novel, deeply grounded idea. No generic advice. Every claim
backed by a file on disk or a concrete sketch. Read-only
research — do not edit any tracked file, do not commit.

Seed invariants (non-negotiable): {INVARIANTS — copied from
the tranche document's architectural invariants section}.

Read first: {FILES — specific paths relevant to the angle,
cited with line ranges where possible}.

Prior context to consult: {TRANCHE DOCS — prior tranches that
touched this angle; the sub-agent must find and name prior
attempts, their deletion commits, and their failure modes}.

Angles to explore (pick and deepen; do not list — propose):
{3–5 concrete sub-angles the orchestrator wants the agent to
consider, phrased as questions the agent must answer}.

Demand at least one novel idea — something not obviously
present in {NAMED COMPARATORS: e.g., simdjson, sonic-rs,
serde_json, lightningcss, cssparser}. Strong candidates:
{2–3 stub directions the orchestrator thinks are promising,
to prime but not constrain the agent}.

Output format:
1. Angle headline.
2. Motivation grounded in saved wave artefacts and codebase
   files (cite paths + line numbers).
3. The novel idea in concrete form — bit layouts, type
   signatures, struct diagrams, algorithmic sketches.
4. Interaction with existing subsystems — not hand-wavy; actual
   data flow.
5. Honest risks, tradeoffs, portability cost.
6. Estimated impact cited against measured hotspots.

Forbidden: edits, commits, speculative throughput numbers,
inference-only conclusions, claims without artefact citations.
```

## Archaeology directive

Before the orchestrator accepts a proposal into a tranche plan,
it must check prior tranches for prior attempts. For every
proposal:

1. Grep `docs/tranches/` for the proposal's keywords.
2. `git log --all --oneline -S '<keyword>' -- '*.rs' docs/`
   for commit-level history.
3. If a prior attempt exists, read the deletion or disablement
   commit message. The failure mode named there is the
   guardrail for the new proposal.
4. Fold the archaeology trail into the proposal's research
   document as a "Prior-attempt context" appendix. Name the
   specific commits that added and deleted the prior attempt.

A proposal that resurrects a prior-attempted concept without
naming the archaeology and the new guardrails is not accepted
into the plan.

## Completion state

The research wave is complete when:

- Every sub-agent's verbatim deliverable sits in
  `docs/tranches/{LETTER}/research/NN-topic.md`.
- The orchestrator's synthesis is presented to the user and
  discussed.
- The resulting proposals either land in `{LETTER}.md` as
  phases, land in a follow-on tranche's plan, or are rejected
  with rationale recorded in `PROGRESS.md`.

A research wave does not produce a tranche by itself — it
produces the material for plan-authoring. The tranche-completion
requirements in `README.md` apply to the execution tranche that
follows, not to the research wave.
