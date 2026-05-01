# S5 — Consumer Configuration Plan for STYLE Precept

Plan-mode artefact. No consumer file touched. All paths absolute.

## Inventory

| Path | Status | Type |
|---|---|---|
| `/Users/mkbabb/Programming/bbnf-lang/CLAUDE.md` | absent | repo Claude entry |
| `/Users/mkbabb/.claude/CLAUDE.md` | absent | user Claude entry |
| `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` | present, 15205 B | auto-memory index, always loaded |
| `/Users/mkbabb/.claude/commands/{ask,commit,develop}.md` | present | user-level slash commands |
| `/Users/mkbabb/.claude/agents/` | absent | no custom agents |
| `/Users/mkbabb/.claude/settings.json` | present, 1554 B | global settings |
| `/Users/mkbabb/Programming/bbnf-lang/.claude/settings.local.json` | present, 972 B | repo permissions |
| `/Users/mkbabb/Programming/bbnf-lang/.claude/agents/` | absent | no custom agents |
| `/Users/mkbabb/Programming/bbnf-lang/.claude/commands/` | absent | no repo commands |
| `/Users/mkbabb/.codex/AGENTS.md` | present, 569 B | global Codex preamble |
| `/Users/mkbabb/.codex/skills/{commit-discipline,codex-primary-runtime}/` | commit-discipline populated | Codex skill directories |
| `/Users/mkbabb/.codex/memories/commit-discipline.md` | present, 664 B | Codex always-loaded memory |
| `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/` | submodule, populated | shared precepts repo |

Style canonical source after S3 lands: `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`. Path is stable across consumers because the submodule sits at a fixed repo-relative location.

## Decisions

### D1 — Repo `CLAUDE.md` (CREATE)

Decision: **CREATE.** Single highest-leverage file. Every Claude Code session opened in `bbnf-lang` auto-loads it; no other mechanism reaches all repo agents. Cost: one short file pointing to the precept.

### D2 — User `~/.claude/CLAUDE.md` (CREATE, minimal)

Decision: **CREATE, minimal.** User works on multiple projects so the global file must not embed bbnf-lang specifics. Body is a one-paragraph forward to whichever project precepts STYLE.md the active repo provides; falls back gracefully when no submodule exists.

### D3 — `~/.codex/AGENTS.md` (EXTEND)

Decision: **APPEND a "Style" section.** Codex has no submodule awareness so the section uses an absolute path to the bbnf-lang submodule; non-bbnf Codex sessions ignore the path miss without error.

### D4 — `~/.codex/skills/style/SKILL.md` (CREATE)

Decision: **CREATE.** Codex skills surface as descriptioned tools the agent can dispatch by name. A `style` skill keeps the precept reachable from any prose-emitting moment, not only project-anchored sessions.

### D5 — `~/.codex/memories/style.md` (CREATE)

Decision: **CREATE.** Memories load on every Codex session unconditionally — second-highest leverage for Codex after AGENTS.md. Short pointer + the four hard rules so the agent does not need to read STYLE.md before its first emission.

### D6 — `/style-check` slash command (DEFER)

Decision: **DEFER.** No standing audit need yet; checking adherence is a normal `/ask` against STYLE.md. Revisit if STYLE drift becomes a recurring tranche cost.

### D7 — Custom Claude agents (N/A)

Decision: **N/A.** No agents directory exists at user or repo level; all agents are dispatched ad-hoc with explicit prompts. STYLE coverage runs through CLAUDE.md and MEMORY.md, which load before any sub-agent prompt is composed.

### D8 — Auto-memory `MEMORY.md` (EXTEND)

Decision: **EXTEND.** Add a `feedback_style_precept` index entry plus the backing leaf so every session reinforces the precept whether or not CLAUDE.md is loaded (e.g., agent threads operating outside the repo root).

### D9 — Consumption order (DOCUMENT)

Recorded below; orchestrator commit message references the order so the next reaudit can verify it.

### D10 — Insertion text (BELOW)

Each insertion below is the EXACT block to paste; orchestrator integrates after S3 lands `STYLE.md`.

## Consumption Order

Claude Code session start, in priority order:

1. Anthropic system prompt + Claude Code defaults (immutable).
2. `~/.claude/CLAUDE.md` (user-global).
3. `<repo>/CLAUDE.md` (repo, currently bbnf-lang only after D1).
4. `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` (auto-loaded; project-keyed).
5. Tranche-time reads explicitly invoked by the agent: `START.md` → tranche doc → wave doc → `docs/precepts/instructions/STYLE.md`.

Codex session start, in priority order:

1. Codex defaults + `~/.codex/AGENTS.md` (always).
2. `~/.codex/memories/*.md` (always loaded).
3. `~/.codex/skills/*/SKILL.md` (loaded on dispatch).
4. Project tree reads on demand.

Style precept reaches every Claude session at step 3 or 4, every Codex session at step 1 or 2.

## Files Proposed for Touch

Five files; orchestrator owns all writes. Plan does not modify any of them.

1. `/Users/mkbabb/Programming/bbnf-lang/CLAUDE.md` — create
2. `/Users/mkbabb/.claude/CLAUDE.md` — create
3. `/Users/mkbabb/.codex/AGENTS.md` — append section
4. `/Users/mkbabb/.codex/skills/style/SKILL.md` — create (parent dir create)
5. `/Users/mkbabb/.codex/memories/style.md` — create
6. `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` — append index entry plus backing `feedback_style_precept.md` leaf

Counted as **6 touches across 6 paths** (one is an append to MEMORY.md plus a new leaf in the same memory directory).

## Exact Insertion Text

### F1 — `/Users/mkbabb/Programming/bbnf-lang/CLAUDE.md` (new file, full body)

```markdown
# bbnf-lang Agent Brief

This repo wires its agent rules through a precepts submodule at
`docs/precepts/`. Read these on session start:

1. `docs/precepts/instructions/README.md` — core rules.
2. `docs/precepts/instructions/STYLE.md` — prose, doc, and commit voice.
3. `docs/precepts/instructions/ORCHESTRATION.md` — dispatch + verification.
4. `docs/precepts/instructions/CONSUMING.md` — wiring reference.

Every prose artefact you emit in this repo — commit subjects, commit bodies,
plan docs, audit docs, tranche docs, inline comments, agent return notes —
follows `STYLE.md`. No exceptions, no per-doc carve-outs.

For tranche work, also follow `docs/precepts/instructions/tranche/SPEC.md`
and the wave protocol in `docs/precepts/instructions/tranche/`.
```

### F2 — `/Users/mkbabb/.claude/CLAUDE.md` (new file, full body)

```markdown
# Cross-Repo Agent Brief

When a repository under work provides `docs/precepts/instructions/STYLE.md`
(typically as a submodule at `docs/precepts/`), that file governs prose voice,
doc structure, and commit language for every artefact emitted in that repo.

Apply it to: commit subjects, commit bodies, plan docs, audit docs, tranche
docs, inline code comments, agent return notes.

Repos without a precepts submodule fall back to the project-specific
guidance in their own `CLAUDE.md` if any, otherwise to standard practice.
```

### F3 — `/Users/mkbabb/.codex/AGENTS.md` (append; preserve existing Commit Discipline section)

Append at end of file, separated by one blank line from the existing block:

```markdown
## Style

When working inside a repository whose tree contains
`docs/precepts/instructions/STYLE.md` (e.g., bbnf-lang at
`/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`),
load that file before emitting prose and apply it to commit subjects, commit
bodies, plan docs, audit docs, tranche docs, inline comments, and agent
return notes. Direct, tight, unspaced em-dashes, no superfluity. Outside such
repos, default to the same voice without claiming repo-specific authority.
```

### F4 — `/Users/mkbabb/.codex/skills/style/SKILL.md` (new file, full body; create parent dir `style/`)

```markdown
---
name: style
description: Use whenever Codex emits prose for a repo carrying docs/precepts/instructions/STYLE.md — commit messages, plan docs, audit docs, tranche docs, inline comments, agent return notes. Direct, tight, unspaced em-dashes, no superfluity, no metalanguage references.
---

# Style

## Source of Truth

`docs/precepts/instructions/STYLE.md` in the active repo. For bbnf-lang the
absolute path is
`/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`.
Read it before composing any artefact whose voice matters.

## Hard Rules

- Direct and tight. No throat-clearing, no warm-up, no ceremonial framing.
- Em-dashes are unspaced (`like—this`). Never spaced.
- No metalanguage: do not name the plan, the conversation, the commit, the
  agent, or the tranche inside the artefact unless the artefact is itself
  about that object.
- No superfluity. If a sentence can be cut without loss, cut it.
- Archaic diction (`begets`, `therein`, `thereof`, `whereby`) is the user's
  voice, not an AI artefact. Preserve it when echoing or extending.
- Generated and tranche docs follow the same voice; no carve-outs.

## When to Invoke

On any non-mechanical prose emission. Mechanical patches (a one-line config
fix, a generated file regen) inherit the rules without explicit invocation.
```

### F5 — `/Users/mkbabb/.codex/memories/style.md` (new file, full body)

```markdown
# Style Memory

Every prose artefact follows the active repo's
`docs/precepts/instructions/STYLE.md` when present. For bbnf-lang the path
is `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`.

Hard rules:

- Direct, tight, no superfluity.
- Em-dashes unspaced.
- No metalanguage references inside the artefact.
- Preserve archaic diction when echoing the user.
- Same voice across commits, plans, audits, tranches, comments, return notes.
```

### F6a — `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` (insert into Feedback section)

Insert in the Feedback list, alphabetised among existing entries (between `archaic-diction-is-voice` and `bench-sequential-regression` is the natural seat):

```markdown
- [style-precept](feedback_style_precept.md) — All prose follows docs/precepts/instructions/STYLE.md; direct, tight, unspaced em-dashes, no metalanguage, no superfluity; same voice across commits, plans, audits, tranches, comments
```

### F6b — `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/feedback_style_precept.md` (new leaf)

```markdown
# style-precept

Every prose artefact emitted in bbnf-lang follows
`docs/precepts/instructions/STYLE.md`. The precept governs commit subjects,
commit bodies, plan docs, audit docs, tranche docs, inline code comments,
and agent return notes. No per-doc carve-outs.

Hard rules:

- Direct and tight. No throat-clearing or ceremonial framing.
- Unspaced em-dashes (`like—this`).
- No metalanguage: do not name the plan, the conversation, the commit, the
  tranche, or the agent inside an artefact unless the artefact is itself
  about that object.
- No superfluity. Cut anything that can be cut without loss.
- Archaic diction (`begets`, `therein`, `thereof`, `whereby`) is the user's
  voice; preserve it.
- Same voice across commits, plans, audits, tranches, inline comments,
  and agent return notes.

When in doubt, read STYLE.md before emitting.
```

## Integration Commit Plan

Sequence after S3 lands `docs/precepts/instructions/STYLE.md`:

1. Confirm `STYLE.md` exists in the precepts submodule and the submodule
   pointer is committed in bbnf-lang.
2. Land F1 + F2 + F6a + F6b in one commit:
   `docs(claude-config): wire STYLE precept into repo and user Claude entries`.
   Body lists the four files and the consumption order they enter at.
3. Land F3 + F4 + F5 in one commit:
   `docs(codex-config): wire STYLE precept into AGENTS.md, skill, memory`.
   Body lists the three files and notes the absolute submodule path used.
4. Repo-level commit lives in bbnf-lang. User-level files (F2, F4, F5) live
   outside any repo and are tracked via the user's dotfile workflow if any —
   note in the bbnf-lang commit body that user-level changes are out-of-tree.
5. After both commits, open a fresh Claude Code session in bbnf-lang and a
   fresh Codex session pointing at the same repo. Verify each agent recites
   the STYLE rules without being prompted to read STYLE.md. If either fails,
   the wiring is wrong, not the precept; redress before closing the wave.

## Highest-Leverage Insertion

`/Users/mkbabb/Programming/bbnf-lang/CLAUDE.md` (F1). One file, ~14 lines,
auto-loaded by every Claude Code session opened in the repo, covering every
sub-agent dispatched from that session. Single largest coverage gain per
edit byte across the plan.

## Confirmations

- Plan doc path: `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/style/S5-consumer-config-plan.md`.
- Files proposed for touch: 6.
- No consumer-config file touched by this plan; all writes deferred to the
  orchestrator post-S3.
