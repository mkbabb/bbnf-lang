# Style Precept REAUDIT 2026-05-01 — Synthesis

Five read-only plan-mode lanes inspected the user's style guideline, the
Wikipedia signs of AI writing, and the precepts integration surface. This
synthesis consolidates findings and names the path forward inside
`docs/precepts/`.

The user's directive: codify the style guideline as a precept; output
orthogonal to the Wikipedia signs of AI writing; abrogate AI-styling in
precept prose; refine precepts only.

## Lane Inputs

| Lane | Deliverable | Headline |
|---|---|---|
| S1 — Wikipedia AI signs | `S1-wikipedia-ai-signs.md` | 51 sign rows across 7 categories plus ~60 banned words. Top tranche-doc violations: sentence-opening connectives, copula avoidance, epanorthosis, outline closers, Title Case headers. |
| S2 — User style distillation | `S2-user-style-distillation.md` | ~95 positive patterns across 10 sections. Caveat: agent did not have verbatim guideline inlined; distilled from project memory + landmarks. S3 has the verbatim authority. |
| S3 — Refined STYLE.md draft | `S3-refined-style-md.md` | 188-line proposed STYLE.md with verbatim preservation of About-me + Examples + Lineage plus new Mandate, Tone, Anti-patterns, Calibration spectrum. Triumvirate fired at 3.0x source-length; orchestrator decision: accept. |
| S4 — Precepts integration | `S4-precepts-integration-plan.md` | File location: `docs/precepts/instructions/STYLE.md` peer to ORCHESTRATION/CONSUMING/LESSONS-LEARNED. 8 mandatory cross-references plus 2 optional. Two-submodule-commit + one parent-pointer plan. |
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

### A4 — Calibration corpus appendix lands as separate file alongside STYLE.md

S8's 33 negative + 15 positive entries land at
`docs/precepts/instructions/style/CALIBRATION.md`. The main precept
stays read-tight; the corpus is a deeper reference.

### A5 — Style precept absorbs into LESSONS-LEARNED with 2026-05-01 entry

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

## Path Forward

One redress lane inside the submodule.

### Sequence

1. Orchestrator writes this synthesis (done).
2. **R1 — Submodule STYLE.md introduction**. Inside `docs/precepts/`.
   Land STYLE.md with S3's draft content; land calibration corpus at
   `instructions/style/CALIBRATION.md`; cross-reference from 8 precept
   files; add LESSONS-LEARNED entry; orchestrator bumps parent pointer.
3. Orchestrator integrates R1's commits, bumps submodule pointer,
   validates `git diff --check`, and runs the close-honesty checklist.

### Wave Redress Dispatch Bounds

| Lane | May modify | Must not touch |
|---|---|---|
| R1 | `docs/precepts/instructions/STYLE.md` (create), `docs/precepts/instructions/style/CALIBRATION.md` (create), 8 cross-reference files inside submodule, `docs/precepts/instructions/LESSONS-LEARNED.md`, parent submodule pointer (orchestrator owns) | Anything outside the submodule; bbnf-lang source, README, GESTALT, tranche plans, consumer configs |

### Hard Cap

- R1 — 35 min (STYLE.md introduction is the largest payload).

Triumvirate auto-trigger applies per the precept: any lane that returns
empty, exceeds cap without commit, or reveals scope outside its bounds
dispatches a research / plan / redress triad before continuing.

## Out of Scope

The dispatch was precepts-only. The following items surfaced during plan-mode
lanes but fell outside scope and were not absorbed:

- Consumer-config touchpoints (Claude / Codex / repo-level CLAUDE.md): out of
  scope; precepts-only audit.
- bbnf-lang documentation edits (README, GESTALT, META-AUDIT-PROMPT,
  REMAINING-TRAJECTORY, codegen-paths): out of scope; precepts-only audit.
- Per-crate READMEs (11 crates): out of scope; future tranche decision.
- Markdown-linter automation for STYLE.md compliance: not currently
  feasible; codify and enforce by document plus dispatch non-negotiables.
- Sibling-repo doc styling alignment: out of scope.
