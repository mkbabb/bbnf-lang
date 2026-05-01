# S4 - Precepts Integration Plan: Style Precept

This plan locates the style precept inside `docs/precepts/`, names its scope,
specifies the cross-references required to wire it into existing instruction
docs, codifies its enforcement, drafts a `LESSONS-LEARNED.md` entry, and
sequences the submodule + parent commits that land it.

The plan assumes S3 produced a `STYLE.md` draft body covering: voice, sparing
unspaced em-dashes, no epanorthosis, contractions allowed, no AI-writing
signs (per the Wikipedia article), no templated commit-body filler, no
motivational prose, no superfluity. This plan does not duplicate that body;
it places it.

## 1. File Location Decision

**Recommendation: `docs/precepts/instructions/STYLE.md`**, peer to
`ORCHESTRATION.md`, `CONSUMING.md`, and `LESSONS-LEARNED.md`.

Rationale:

- The style precept governs prose across the whole framework, not only
  tranche docs. Putting it under `instructions/tranche/` would mis-locate
  it. Tranche-spec sub-docs (`SPEC.md`, `WAVE_SPEC.md`,
  `AGENT_DISPATCH_TEMPLATE.md`) are tranche-lifecycle artefacts; style is
  cross-cutting.
- A new top-level `docs/precepts/style/` subdir, peer to `instructions/`
  and `audits/`, is structurally tidy but introduces a third top-level
  bucket for one document. `instructions/` already holds the agent
  contract; style is part of that contract. KISS says use the existing
  bucket.
- `STYLE.md` placed directly under `instructions/` matches the existing
  shape: shared rules at the root, tranche lifecycle nested. A reader
  scanning `instructions/` sees one prose-style document peer to one
  orchestration document peer to one consumer-setup document peer to one
  lessons ledger. The ranking is clear without a new directory.

Rejected alternatives:

- `instructions/tranche/STYLE.md` would imply tranche-only scope, which
  contradicts the `ORCHESTRATION.md` agent-return + `CONSUMING.md`
  REAME-style-applies-too reach.
- `style/README.md` as a new top-level subdir adds a directory for one
  file. Promote to a subdir only when STYLE grows companion files
  (examples, anti-patterns, glossary). Until then a single file under
  `instructions/` wins on cohesion.

## 2. Scope Of Application

In scope. The style precept governs:

1. All written documentation inside `docs/`: tranche plans, wave specs,
   audit docs, research docs, FINAL/PROGRESS docs, READMEs, consumer
   instructions, the precepts themselves.
2. Commit message bodies and prose subjects. Conventional Commits gives the
   subject shape; STYLE governs tone, em-dash discipline, no templated
   filler, no epanorthosis. The existing rule "no templated commit bodies"
   in `LESSONS-LEARNED.md` 2026-04-30 is a special case STYLE absorbs.
3. Code comments, when comments appear. The existing precept defaults to no
   comments unless the comment explains a non-obvious invariant; STYLE
   refines: when comments do appear, they obey the same prose rules
   (no AI-writing signs, no epanorthosis, sparing unspaced em-dashes).
4. Agent dispatch packets. The dispatch prompt prose is project prose. A
   prompt loaded with motivational filler or AI-writing signs wastes
   context budget and trains agents on the wrong register.
5. Agent return prose. Returns are read-mostly artefacts that feed into
   audit docs and PROGRESS logs; their prose is consumed by humans and
   other agents.
6. User-facing terminal status messages, status ticks, and orchestrator
   reply text. Status-tick cadence already requires concision; STYLE names
   the registers explicitly.

Out of scope (clarified):

- Generated code output. Generators emit by template; STYLE does not
  rewrite generator templates.
- External API responses, log output formats, machine-readable JSON or
  YAML. STYLE governs prose, not data formats.
- Vendor / dependency content, third-party READMEs, copied license text.
- Test fixtures, golden files, expected-output snapshots.

## 3. Cross-References To Add

Each existing precept doc receives one or two surgical lines pointing at
`STYLE.md`. Exact line text proposed below; orchestrator can adjust spacing
to match each file's existing list shape without changing the rule.

### 3.1 `instructions/README.md` §Edicts

Add as a new bullet, ordered alphabetically near the `Architectural
transposition` and `Evidence beats claims` bullets, or appended to the
list. Exact text:

```markdown
- **Voice and style.** Documentation, commit bodies, dispatch prompts, and
  agent returns follow `STYLE.md`. Direct assertions, sparing unspaced
  em-dashes, no epanorthosis, no AI-writing signs.
```

The same file's §Code Discipline already says "Documentation is part of
the change"; that rule does not need editing because STYLE is itself
documentation.

### 3.2 `instructions/ORCHESTRATION.md` §Returns

Append one sentence to the existing §Returns paragraph:

```markdown
Return prose follows `STYLE.md`; templated, motivational, or AI-styled
return text is rejected at integration the same way templated commit
bodies are.
```

Optionally add a sibling sentence to §Status that reads:

```markdown
Status ticks and orchestrator replies follow `STYLE.md` voice rules;
brevity is enforced by cadence, register is enforced by style.
```

### 3.3 `instructions/CONSUMING.md` §Local Instructions

Append one bullet to the existing list of permitted local content:

```markdown
- Local style refinements. Voice baseline lives in
  `docs/precepts/instructions/STYLE.md`; consumers may tighten further but
  may not override the shared rules.
```

### 3.4 `instructions/tranche/SPEC.md` §Plan Shape

After the `Do not include commentary, motivational prose, or duplicated
shared precepts.` line, append:

```markdown
Plan prose follows `STYLE.md`. The style precept is the source of truth
for register, em-dash discipline, and AI-writing-sign avoidance.
```

### 3.5 `instructions/tranche/WAVE_SPEC.md` §3 Scope

Append to the existing scope paragraph:

```markdown
Scope bullet text follows `STYLE.md`. Concrete change or deletion phrasing,
no filler.
```

Optionally a parallel line under §11 Archaeology if archaeology prose
drifts toward narrative; the §3 reference is the load-bearing one.

### 3.6 `instructions/tranche/AGENT_DISPATCH_TEMPLATE.md`

Two surgical edits:

1. Under `Read first:` add a numbered entry (renumbering or appending
   after current 6):

```markdown
N. `docs/precepts/instructions/STYLE.md`
```

2. In the `Non-negotiables:` block, append a bullet:

```markdown
- prose follows `STYLE.md`: no templated bodies, no AI-writing signs, no
  epanorthosis, sparing unspaced em-dashes;
```

This places STYLE under both inputs (read first) and outputs
(non-negotiables) of the dispatch contract.

### 3.7 `instructions/tranche/README.md` (read-order index)

Add `STYLE.md` to the parent read order by inserting at the top of the
existing list, since style applies before the tranche-specific docs:

```markdown
0. `../STYLE.md`
```

Or, more conservatively, append to step 1 with a parenthetical:

```markdown
1. `../README.md` (with `../STYLE.md` for prose register)
```

Recommend the parenthetical form; it does not renumber the existing list.

### 3.8 Top-level `docs/precepts/README.md` §Layout

Add `STYLE.md` to the layout tree:

```text
instructions/
  README.md                 Core agent and orchestrator rules
  ORCHESTRATION.md          Dispatch, work isolation, verification
  STYLE.md                  Prose voice and register
  CONSUMING.md              Reference setup
  LESSONS-LEARNED.md        Cross-tranche incident ledger
```

### 3.9 Cross-reference count

Eight files receive references. Count by lines added: ten one-line
references (some files take two: ORCHESTRATION.md takes §Returns and
optional §Status; AGENT_DISPATCH_TEMPLATE.md takes Read-first and
Non-negotiables). The orchestrator picks the eight mandatory edits and
treats §Status + §Archaeology as optional.

Mandatory edits (8):

1. `instructions/README.md` §Edicts new bullet
2. `instructions/ORCHESTRATION.md` §Returns sentence
3. `instructions/CONSUMING.md` §Local Instructions bullet
4. `instructions/tranche/SPEC.md` §Plan Shape sentence
5. `instructions/tranche/WAVE_SPEC.md` §3 Scope sentence
6. `instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` Read-first entry
7. `instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` Non-negotiables bullet
8. `docs/precepts/README.md` §Layout tree entry

Optional edits (2):

- `instructions/ORCHESTRATION.md` §Status (recommended)
- `instructions/tranche/README.md` read-order index entry (parenthetical
  form recommended)

## 4. Enforcement

The style precept enforces by document, by review, and by skill check; not
by linter. Markdown style linters cannot detect epanorthosis or AI-writing
signs reliably. The enforcement chain:

1. **Documented bans.** STYLE.md lists explicit bans:
   - no AI-writing signs (link to the Wikipedia article
     `https://en.wikipedia.org/wiki/Signs_of_AI_writing` or the canonical
     URL the user supplied);
   - no epanorthosis (the "...not X, but Y" or "...; rather, ..." rhetorical
     correction pattern);
   - em-dash discipline: sparing, unspaced (`—` not ` — `);
   - contractions allowed and preferred where natural;
   - no motivational prose, no superfluity, no narrative filler;
   - no templated commit bodies (absorbed from existing 2026-04-30 lesson).
2. **Dispatch-template non-negotiable.** AGENT_DISPATCH_TEMPLATE.md adds
   the bullet. Every agent dispatch carries this constraint.
3. **Integration check.** Orchestrator integration step in
   `ORCHESTRATION.md` §Integration already verifies docs changed when
   process changed; STYLE adds: agent return prose and commit bodies
   follow STYLE before integration. Drift is rejected at cherry-pick.
4. **Skill check.** The existing commit-discipline skill rejects bodies
   "whose only specific content is the subject line restated"
   (LESSONS-LEARNED 2026-04-30). Extend by reference: STYLE rules apply.
   No new skill required; existing rejector covers the templated case.
5. **Wave close.** DOC_UPDATE_WAVE.md doc reconciliation step reads STYLE
   alongside the doc-update task.

No automated linter rule is proposed. Human integration is the gate.
Markdown linters that exist (markdownlint, etc.) catch heading, list, and
trailing-whitespace issues; they do not catch register. STYLE is enforced
by the same eyes that reject templated commit bodies today.

## 5. LESSONS-LEARNED Entry

Proposed entry, dated to land with the submodule commit:

```markdown
## 2026-05-01 - Style Precept Absorbed

- **Source**: bbnf-lang AZ-III REAUDIT 2026-04-30 style audit; observed
  AI-styling drift in tranche audit docs and dispatch returns.
- **Failure**: documentation, agent returns, and commit bodies drifted
  toward AI-writing signs (epanorthosis, spaced em-dashes, motivational
  filler, "rather, ..." reversals). The drift was invisible per-document
  but visible across the corpus, and contradicted the user's voice.
- **Rule**: prose across the framework follows
  `instructions/STYLE.md`. Direct assertions, sparing unspaced em-dashes,
  no epanorthosis, no AI-writing signs, contractions allowed. The
  dispatch template, orchestration return rules, and tranche plan shape
  reference STYLE as their voice authority.
- **Check**: dispatch prompts include STYLE in read-first and
  non-negotiables; integration rejects templated, AI-styled, or
  epanorthotic returns and commit bodies the same way it rejects
  templated commit bodies today.
```

## 6. Integration Commit Plan

Submodule commits land first inside `docs/precepts/`; the parent bumps the
pointer. The redress lane that lands STYLE returns the submodule HEAD SHA
so the orchestrator can land the pointer commit immediately, per
`CONSUMING.md` §Update.

Recommended shape: **two submodule commits + one parent pointer commit.**
This is the smallest set that keeps `STYLE.md` introduction separate from
its cross-reference wiring, so reviewers can read the precept body once
without diff noise.

### Submodule commit 1: introduce STYLE

```text
docs(style): add prose voice precept

Adds instructions/STYLE.md as the source of truth for voice, em-dash
discipline, AI-writing-sign avoidance, and epanorthosis ban across all
prose surfaces (docs, commit bodies, dispatch prompts, returns, status
ticks). Body cites the existing 2026-04-30 templated-bodies lesson as the
absorbed special case.
```

Files touched: `instructions/STYLE.md` (new).

### Submodule commit 2: cross-reference STYLE + lessons entry

```text
docs(precepts): wire STYLE references and lessons entry

Cross-references STYLE from instructions/README.md (Edicts),
ORCHESTRATION.md (Returns, Status), CONSUMING.md (Local Instructions),
tranche/SPEC.md (Plan Shape), tranche/WAVE_SPEC.md (Scope),
tranche/AGENT_DISPATCH_TEMPLATE.md (Read-first, Non-negotiables),
tranche/README.md (read order), and top-level README (Layout). Adds
2026-05-01 lessons entry codifying the style absorption.
```

Files touched: 8 files plus `LESSONS-LEARNED.md`.

Rationale for two submodule commits, not one or three:

- **Not one** because the introduction commit and the wiring commit answer
  different review questions ("is the rule right?" vs "are the references
  surgical?"). Splitting keeps each diff small enough to read once.
- **Not three** because the lessons entry is mechanically tied to the
  cross-reference wiring; its check field literally references the
  cross-reference points. Splitting them into separate commits would
  produce a body that re-restates the wiring commit. KISS.
- A bodyless one-line wiring commit is rejected by the existing 2026-04-30
  bodyless-large-commits rule; the body above satisfies the why/what/
  evidence shape.

### Parent commit: bump submodule pointer

```text
docs(precepts): bump pointer to <SHA-prefix>

Pulls in STYLE.md and its cross-reference wiring. Body cites the
submodule HEAD SHA and lists the two submodule commits absorbed.
```

File touched: `docs/precepts` (gitlink only).

### Rejected: collapse into 1-2 commits

Collapsing to one submodule commit produces a 9-file diff plus a new file,
which is the exact shape the bodyless-large-commits rule warns against.
Even with a body, reviewers can't separate "rule wording" from "reference
placement" review. Two-commit shape costs nothing and keeps each surface
auditable.

## 7. Sequencing

1. S3 finalises STYLE.md draft body.
2. S4 (this plan) is the wiring authority; orchestrator dispatches a
   redress agent using this plan.
3. Redress agent lands the two submodule commits in
   `docs/precepts/` (sibling worktree on the submodule, or directly in
   submodule HEAD if the submodule branch policy permits).
4. Redress agent returns submodule HEAD SHA.
5. Orchestrator lands parent pointer commit immediately, not in a
   follow-up wave (per CONSUMING.md §Update).
6. PROGRESS.md and the wave's audit log record both commits and the
   pointer SHA.

## 8. File Bounds For The Redress Dispatch

When the redress agent runs, its file bounds:

May modify (inside submodule):

- `docs/precepts/instructions/STYLE.md` (create)
- `docs/precepts/instructions/README.md` (modify)
- `docs/precepts/instructions/ORCHESTRATION.md` (modify)
- `docs/precepts/instructions/CONSUMING.md` (modify)
- `docs/precepts/instructions/LESSONS-LEARNED.md` (modify)
- `docs/precepts/instructions/tranche/SPEC.md` (modify)
- `docs/precepts/instructions/tranche/WAVE_SPEC.md` (modify)
- `docs/precepts/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` (modify)
- `docs/precepts/instructions/tranche/README.md` (modify; optional edit)
- `docs/precepts/README.md` (modify)

May modify (parent worktree, after submodule SHA returns):

- `docs/precepts` (gitlink update only)

Do not touch:

- Any `crates/**` source.
- Any `docs/tranches/**` outside this audit's REAUDIT-2026-04-30/style/
  directory.
- Any other precept submodule file.

## 9. Hard Gate For The Redress Dispatch

- `STYLE.md` exists at `docs/precepts/instructions/STYLE.md`.
- All eight mandatory cross-references are present, each citing
  `STYLE.md` by relative path.
- 2026-05-01 LESSONS entry is present and follows the canonical
  entry format (Source / Failure / Rule / Check).
- Two submodule commits have bodies and the parent pointer commit cites
  the submodule SHA.
- `git diff --check` clean inside submodule and parent.
- A grep for `STYLE.md` inside `docs/precepts/instructions/` returns the
  expected references in each of the eight files.

## 10. Risks And Mitigations

- **Risk**: STYLE wording itself drifts toward AI-styling. **Mitigation**:
  the redress agent reads its own draft against STYLE rules before
  commit.
- **Risk**: cross-reference text becomes formulaic across eight files.
  **Mitigation**: each reference is tailored to the host file's
  existing list shape; no template paste.
- **Risk**: agents read STYLE as suggestion, not contract. **Mitigation**:
  STYLE lands in dispatch-template Non-negotiables, not just Read-first;
  the Non-negotiables block already enforces commit-body shape.
- **Risk**: the Wikipedia "Signs of AI writing" article is renamed or
  deleted. **Mitigation**: STYLE inlines the load-bearing list (em-dash
  abuse, "not just X but Y" structures, hedge stacking) rather than only
  linking out. Link is supplementary.
