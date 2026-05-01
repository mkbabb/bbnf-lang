# Style Precept REAUDIT 2026-05-01 — Synthesis

Eight read-only plan-mode lanes inspected the user's style guideline, the
Wikipedia signs of AI writing, the existing project documentation, the
codebase-doc isomorphism, and the precepts + consumer-config integration
surface. This synthesis consolidates findings into accept, narrow, and
speculation dispositions, names the path forward, and carries the
implementation dispatch packets.

The user's directive: codify the style guideline as a precept; ensure
coverage in both Claude and Codex consumers; abrogate AI-styling across
existing docs; align docs with the codebase; output orthogonal to the
Wikipedia signs of AI writing.

## Lane Inputs

| Lane | Deliverable | Headline |
|---|---|---|
| S1 — Wikipedia AI signs | `S1-wikipedia-ai-signs.md` | 51 sign rows across 7 categories plus ~60 banned words. Top tranche-doc violations: sentence-opening connectives, copula avoidance, epanorthosis, outline closers, Title Case headers. |
| S2 — User style distillation | `S2-user-style-distillation.md` | ~95 positive patterns across 10 sections. Caveat: agent did not have verbatim guideline inlined; distilled from project memory + landmarks. S3 has the verbatim authority. |
| S3 — Refined STYLE.md draft | `S3-refined-style-md.md` | 188-line proposed STYLE.md with verbatim preservation of About-me + Examples + Lineage plus new Mandate, Tone, Anti-patterns, Calibration spectrum. Triumvirate fired at 3.0x source-length; orchestrator decision: accept. |
| S4 — Precepts integration | `S4-precepts-integration-plan.md` | File location: `docs/precepts/instructions/STYLE.md` peer to ORCHESTRATION/CONSUMING/LESSONS-LEARNED. 8 mandatory cross-references plus 2 optional. Two-submodule-commit + one parent-pointer plan. |
| S5 — Consumer config | `S5-consumer-config-plan.md` | 6 touchpoints: repo `CLAUDE.md` (create), user `~/.claude/CLAUDE.md` (create), `~/.codex/AGENTS.md` (append), `~/.codex/skills/style/SKILL.md` (create), `~/.codex/memories/style.md` (create), auto-memory `MEMORY.md` index plus new `feedback_style_precept.md`. Pattern mirrors existing Codex commit-discipline triad. Highest leverage: repo `CLAUDE.md`. |
| S6 — Docs AI-styling audit | `S6-docs-ai-styling-audit.md` | 15 files audited. 0 CRITICAL/HIGH/MED, 5 LOW, 10 CLEAN. AI-styling drift is residual, not systemic. ~7-9 surgical edits clean the audited surface. Top hits: `comprehensive` 2x in META-AUDIT-PROMPT, `leverage`/`navigate` verbs in REMAINING-TRAJECTORY. |
| S7 — Codebase-doc isomorphism | `S7-codebase-doc-isomorphism.md` | 47 claims audited: 4 CRITICAL, 14 HIGH, 19 MED, 10 LOW. README has 3 CRITICAL stale claims (rust/ layout pre-monorepo; `#[derive(Parser)]` slab + span proc-macros documented but DELETED at B2.W2). GESTALT crate enumeration stale (tape/derive/json-prototype absent). Triumvirate fired: README needs wholesale rewrite. Zero per-crate READMEs exist. |
| S8 — Calibration corpus | `S8-calibration-corpus.md` | 33 negative + 15 positive entries. Calibration insight: legitimate em-dash for coordinate listing vs AI-sign em-dash for soft parenthetical. Cleanest in-codebase prose: precepts edicts plus recent commit bodies. |

## Accepted Findings

### A1 — Style precept lives at `docs/precepts/instructions/STYLE.md`

S4's location decision is right. Peer to ORCHESTRATION / CONSUMING /
LESSONS-LEARNED. New top-level subdir would add a directory for one
file; tranche/ subdir would mis-locate a cross-cutting precept.

### A2 — STYLE.md content is S3's draft, accepted at 3.0x source-length

The dispatch contract demanded verbatim preservation of About-me +
Examples + Lineage (~50 lines inviolable) plus new Anti-patterns +
Calibration spectrum (~60 new lines) plus the original positive
patterns (~25 lines). Sub-1.5x landed only by erosion. The triumvirate
fired correctly; the orchestrator accepts the 3.0x draft as the
honest minimum.

### A3 — Eight precept cross-references land

S4 names eight mandatory plus two optional cross-references. The
mandatory eight are surgical insertions of one bullet or one sentence
each; the two optional are minor read-order parenthetical updates.
Apply all eight mandatory; accept both optional. Total ~10 small
diffs across precept submodule files.

### A4 — Consumer config: 6 touchpoints, pattern parity with Codex commit-discipline

S5's 6-file plan is correct. Mirrors the existing Codex commit-discipline
triad (AGENTS.md + skill + memory). Highest leverage: repo `CLAUDE.md`
because it auto-loads in every Claude Code session opened in this repo
and transitively covers every dispatched sub-agent.

### A5 — Docs AI-styling cleanup is small surgical sweep, not rewrite wave

S6 shows the working corpus is mostly aligned with the precept already
because user voice (archaic diction, biblical cadence) is intentional
per `archaic-diction-is-voice` memory. Only ~7-9 surgical edits needed:
- META-AUDIT-PROMPT.md:3 and 1585 — drop "comprehensive"
- REMAINING-TRAJECTORY.md:444 — "leverage" → "reach"
- REMAINING-TRAJECTORY.md:392 — "navigate" → "traverse"
- codegen-paths.md:165 — staccato sentence pattern (optional)
- 2-3 more LOW hits in remaining files

### A6 — README warrants wholesale rewrite, separate dispatch lane

S7 fired triumvirate. Three CRITICAL stale claims about live API
surfaces that have been deleted from the codebase. Surgical edit list
is too long; rewrite is shorter and more honest. Open a dedicated
README rewrite agent that reads the actual current `crates/`,
`Cargo.toml`, `xtask/`, and `docs/codegen-paths.md` and writes a
README that matches code reality.

### A7 — GESTALT crate enumeration patches as part of doc-cleanup wave

S7 names GESTALT.md:205-206 as HIGH stale (lists tape/derive/json-prototype
as workspace members; all three absent). This is a surgical edit, not
a wholesale rewrite — the rest of GESTALT mostly matches code.

### A8 — Per-crate READMEs are not in scope for AZ-III

S7 notes zero per-crate READMEs exist. Adding 11 per-crate READMEs is
substantial new doc work, not a style precept patch. Route to a
post-AZ-III decision; do not absorb into this synthesis.

### A9 — Calibration corpus appendix lands inside STYLE.md

S8's 33 negative + 15 positive entries are the calibration appendix
that S3's draft already structured a placeholder for. Inline the
corpus into STYLE.md or attach as a separate `docs/precepts/instructions/style/CALIBRATION.md`.
Decision: separate file kept under `instructions/style/` subdir alongside
STYLE.md, so the main precept stays read-tight and the corpus is a
deeper reference.

### A10 — Style precept absorbs into LESSONS-LEARNED with 2026-05-01 entry

S4 proposes the entry. Source / Failure / Rule / Check format. Source:
this REAUDIT 2026-05-01 cycle. Failure: AI-styling drift in tranche
prose absent an explicit precept. Rule: every prose surface follows
STYLE.md. Check: doc reviews cite STYLE.md violations or absence
thereof.

## Narrowed Findings

### N1 — S2's pattern catalog is supplementary, not authoritative

S2 lacked the verbatim guideline. Its catalog is useful as cross-check
but S3's verbatim-preserving draft governs content disputes. Where S2
and S3 diverge, S3 wins.

### N2 — The unspaced em-dash convention is the new default; preserve user's existing spaced em-dashes

The user said "limit usage of em dashes, and when including them
typically have no space between the items." The user's existing
guideline mixes spaced and unspaced em-dashes (the spaced ones are
deliberate poetic-lilt punctuation). For the precept and going-forward
drafts, default to unspaced. For verbatim-preserved user content,
keep as the user wrote it. STYLE.md should make this calibration
explicit.

### N3 — Wholesale README rewrite has scope risk; bound it tightly

A README rewrite that's read-only on code but lengthy is itself a
trap. Bound the agent: must produce a README at most 1.0x the current
length (the current README is overlong; shorter is better); must cite
each claim against a current code path or doc artefact; must follow
STYLE.md.

## Speculation — not gates

### S1 — "All 11 crates need READMEs"

S7 flagged the absence as bonus finding, not as a gate violation. No
existing tranche promises per-crate READMEs. Treat as a future item;
do not absorb.

### S2 — "Codex skill enforcement vs document-only enforcement"

S4 proposes document-only enforcement plus dispatch non-negotiables.
S5 adds a Codex skill. Whether agents actually consume the skill at
dispatch time is harness-dependent and not currently testable. Treat
the skill as a hint-injection mechanism, not a guarantee.

## Path Forward

The redress wave has four parallel implementation lanes plus one
sequential README rewrite.

### Sequence

1. Orchestrator writes this synthesis (done).
2. Wave Redress R (parallel, four lanes):
   - **R1 — Submodule STYLE.md introduction**. Inside `docs/precepts/`. Land STYLE.md with S3's draft content; land calibration corpus at `instructions/style/CALIBRATION.md`; cross-reference from 8 precept files; add LESSONS-LEARNED entry; orchestrator bumps parent pointer.
   - **R2 — Consumer config touchpoints**. Land repo `CLAUDE.md`, user `~/.claude/CLAUDE.md`, `~/.codex/AGENTS.md` append, `~/.codex/skills/style/SKILL.md`, `~/.codex/memories/style.md`, auto-memory entries.
   - **R3 — Docs AI-styling surgical sweep**. ~7-9 edits across META-AUDIT-PROMPT, REMAINING-TRAJECTORY, codegen-paths, and minor others per S6's patch list.
   - **R4 — GESTALT + remaining HIGH/MED isomorphism patches**. Surgical patches for the 14 HIGH and 19 MED isomorphism gaps S7 flagged that are not the README. Write to GESTALT.md crate enumeration plus other named claims.
3. Wave Redress S (sequential after R1, depends on STYLE.md being available):
   - **S1 — README wholesale rewrite**. New agent reads current code reality and writes a README that matches. Bounded: ≤1.0x current length; every claim cited; STYLE.md compliant.
4. Orchestrator integrates each lane's commits, bumps submodule pointer for R1, validates `git diff --check`, and runs the close-honesty checklist.

### Wave Redress Dispatch Bounds

| Lane | May modify | Must not touch |
|---|---|---|
| R1 | `docs/precepts/instructions/STYLE.md` (create), `docs/precepts/instructions/style/CALIBRATION.md` (create), 8 cross-reference files inside submodule, `docs/precepts/instructions/LESSONS-LEARNED.md`, parent submodule pointer (orchestrator owns) | Anything outside the submodule, AZ-II/III plans, source code |
| R2 | `CLAUDE.md` parent (create), `~/.claude/CLAUDE.md` (create), `~/.codex/AGENTS.md` (append), `~/.codex/skills/style/` (create), `~/.codex/memories/style.md` (create), `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` (append) plus new `feedback_style_precept.md` leaf | Submodule, AZ-II/III plans, source code |
| R3 | `docs/META-AUDIT-PROMPT.md`, `docs/tranches/REMAINING-TRAJECTORY.md`, `docs/codegen-paths.md`, plus the 2-3 other LOW-severity files S6 names | Submodule, README.md, GESTALT.md, source, other tranche docs |
| R4 | `docs/GESTALT.md` (HIGH+MED isomorphism patches per S7), other named non-README files per S7's list | README.md (S1 owns), submodule, source, AZ-II/III plans |
| S1 (sequential) | `README.md` (wholesale rewrite) | Submodule, GESTALT, source, other docs |

### Hard Caps

- R1 — 35 min (STYLE.md introduction is the largest payload).
- R2 — 25 min (mostly file-creation; no scope reveal expected).
- R3 — 25 min (surgical sweep across known file:line list).
- R4 — 35 min (multiple files; isomorphism cross-checking).
- S1 — 35 min (README rewrite with code-reality citations).

Triumvirate auto-trigger applies per the precept: any lane that returns
empty, exceeds cap without commit, or reveals scope outside its bounds
dispatches a research / plan / redress triad before continuing.

## Open Routes Not Absorbed

- Per-crate READMEs (11 crates) — future tranche decision.
- Markdown-linter automation for STYLE.md compliance — not currently
  feasible; codify and enforce by document plus dispatch non-negotiables.
- Sibling repo (parse-that, pprint, gorgeous, bbnf-buddy) doc styling
  alignment — owned by AZ-III W0.6 sibling triage; not in scope for
  this style cycle.

The synthesis confirms the directive scope. Redress dispatches next.
