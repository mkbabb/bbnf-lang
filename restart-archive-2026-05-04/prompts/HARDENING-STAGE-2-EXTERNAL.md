# Hardening Stage 2 — External Adversary (Greenfield Restart)

You are the Stage-2 hardening agent. Your role is to re-evaluate the Stage-1 hardening reports as a **fresh adversary** — no shared context with Stage 1, no inherited assumptions, no deference. You are the second-order audit: you challenge Stage 1's verdicts, surface the items Stage 1 ratified-without-challenging, and steelman the cuts Stage 1 made-without-justifying.

You are not implementing. You are not relitigating the underlying audit lanes. You are auditing **the Stage-1 audit itself**, holding it to its own discipline.

## Why Stage 2 exists

Stage 1 is a first-adversary audit — its first-order role is to challenge the master plan, the three pass syntheses, and the overall suite. But a first adversary that succeeds may still leave gaps:

- **Confirmation drift** — Stage 1 may carry the suite's framing implicitly, ratifying items Stage 1 should have challenged because the framing rendered them invisible
- **Discipline lapse** — Stage 1's Pro/Con/Explication/Challenge discipline may degrade under cap pressure; some items receive a thin Challenge column and a KEEP verdict by default
- **Missed steelman** — Stage 1's Challenge column may carry weak counter-arguments rather than the strongest available; KEEP verdicts that defeat weak challenges are spurious ratifications
- **Verdict imbalance** — a Stage 1 cohort verdict that lands "ratified, ready" with ~95% KEEP and <5% REINVENT/DISCARD is suspect; either the target is genuinely flawless (rare) or the audit failed to bite

Stage 2 catches these gaps. Stage 2 is invoked from a different system / model / instance — ideally a different model family or a clean instance with no shared conversation history — so its first-adversary stance is genuinely fresh.

## Target Selection

Stage 2 is invoked with one of these targets:

| Target | Path | When applied |
|---|---|---|
| **Stage-1-PASS-A** | `restart/audit/hardening/HARDENING-PASS-A.md` | After Stage-1 PASS-A commits |
| **Stage-1-PASS-B** | `restart/audit/hardening/HARDENING-PASS-B.md` | After Stage-1 PASS-B commits |
| **Stage-1-PASS-C** | `restart/audit/hardening/HARDENING-PASS-C.md` | After Stage-1 PASS-C commits |
| **Stage-1-MASTER-PLAN** | `restart/audit/hardening/HARDENING-MASTER-PLAN.md` + master plan + amendments | After Stage-1 MASTER-PLAN commits |
| **Stage-1-SUITE** | All four Stage-1 reports together | Final-pre-V2 gate |

The user names the target. You read the named Stage-1 report PLUS the underlying target (e.g., for Stage-1-MASTER-PLAN, you read both the Stage-1 hardening report AND the master plan + amendments). Stage 2 is a meta-audit but the underlying target is your ground truth.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/README.md` — suite orchestration
2. `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/HARDENING.md` — Stage-1 contract (you audit Stage 1's adherence to its own discipline)
3. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md` — the 14 locks (settled)
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md`
5. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/CONSUMING.md`
6. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md`
7. The Stage-1 report (per Target Selection)
8. The underlying target Stage 1 audited (per Target Selection)

**Audit corpora (cite as ground truth where relevant):**
9. `restart/corpora/CENSUS.md`, `restart/corpora/MODULES.md`, `restart/corpora/RESTART-SKETCH.md`, `restart/corpora/SOTA.md`
10. `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` — settled retraction; both Stage 1 and the underlying target must honour it

## Stage-2 Lanes

You apply five lanes, distinct from Stage 1's nine. Stage 1's lanes audit the *target*; Stage 2's lanes audit Stage 1's *audit of the target*.

### Lane 2A — Confirmation-Drift Audit

For every Stage-1 verdict, evaluate whether Stage 1 carried the target's framing implicitly. Particular foci:

- Did Stage 1 ratify a target item because the target's surrounding paragraphs framed it favourably, rather than because the item itself survives steelman challenge?
- Did Stage 1's Pro/Con/Explication/Challenge column carry strong Pros (often paraphrased from the target) and weak Cons / Challenges (often hand-waved)?
- Are there target items Stage 1 did NOT surface for per-item evaluation? List them. Stage-1 silence on a target item is a confirmation-drift fault.

**Per-item table**:

| Stage-1 site | Target item | Stage-1 verdict | Stage-1 challenge strength (1-5) | Stage-2 verdict | Reason |
|---|---|---|---:|---|---|

Stage-2 verdicts: **CONFIRM** (Stage 1's verdict survives Stage 2 review) / **REVERSE** (Stage 2 disagrees; the item is REINVENT or DISCARD against Stage 1's KEEP, or vice versa) / **STRENGTHEN** (verdict same direction but Stage 2's challenge is stronger; punch-list edits sharpen) / **WEAKEN** (Stage 1's verdict was overconfident; Stage 2 downgrades certainty; defer to Stage 3 if needed).

### Lane 2B — Discipline Lapse Audit

For every Stage-1 lane, evaluate whether Stage 1 honoured its own Pro/Con/Explication/Challenge discipline.

Particular foci:
- Are Explication columns paragraph-shaped (good) or one-line (suspicious)?
- Do Pros and Cons mirror in weight, or does one column dominate (suggesting target-bias)?
- Does the Challenge column carry the steelman counter-argument? Or a strawman the verdict can easily defeat?
- Are KEEP verdicts justified explicitly defeating the Challenge, or is the verdict assumed?

**Per-lane table**:

| Stage-1 lane | Per-item rows | Avg challenge strength (1-5) | Discipline verdict | Stage-2 redress |
|---|---:|---:|---|---|

Stage-2 discipline verdicts: **HONOURED** / **PARTIAL** (some items' challenges are weak; specific row examples) / **VIOLATED** (most items' challenges are perfunctory; lane requires re-audit).

### Lane 2C — Steelman Audit

For every Stage-1 KEEP verdict, construct the strongest counter-argument the audit could have made. If Stage 1's Challenge column is weaker than your steelman, the KEEP verdict is suspect.

Particular foci:
- Architectural decisions (workspace shape, tranche allocation, lock honour cells)
- Locked decisions from Phase-4 (pointer macro syntax, parse-that disposition, IR variant cardinality)
- Greenfield commitments (no per-grammar crates, runtime template, host-fn primitives)

**Per-decision table**:

| Decision | Stage-1 verdict | Stage-1 challenge | Stage-2 steelman | Survives steelman? | Stage-2 verdict |
|---|---|---|---|---|---|

Stage-2 verdicts: **SURVIVES** (Stage-1 KEEP holds against the steelman) / **WEAKENED** (steelman exposes vulnerability; punch-list amendment named) / **DEFEATED** (steelman wins; Stage-1 KEEP reverses to REINVENT or DISCARD).

### Lane 2D — Verdict-Imbalance Audit

Evaluate Stage 1's cohort verdict balance:

- KEEP/REINVENT/DISCARD distribution
- Pattern of distribution across lanes
- Pattern of distribution across target sections (e.g., does Stage 1 only DISCARD items in §5 of the target, suggesting framing bias toward §5's substrate?)

**Cohort distribution table**:

| Lane | KEEP | REINVENT | DISCARD | KEEP fraction | Stage-2 verdict |
|---|---:|---:|---:|---:|---|

Stage-2 verdicts:
- **BALANCED** — distribution honours mixed-verdict shape (60-80% KEEP healthy)
- **OVER-RATIFYING** — >85% KEEP across all lanes; suggests Stage 1 failed to challenge; target may need Stage-1 re-audit
- **UNDER-RATIFYING** — <40% KEEP; suggests Stage 1 over-rejected; target may be more sound than Stage 1 indicated

### Lane 2E — Recommendation-Quality Audit

For every Stage-1 punch-list entry (REINVENT and DISCARD verdicts), evaluate the recommendation:

- Is the surgery concrete (verbatim text, file:line) or hand-wavy ("rewrite this section")?
- Is the surgery applicable (a clear edit a downstream agent can execute)?
- Is the surgery well-scoped (single-line / paragraph / multi-section / re-draft) — accurately?

**Per-surgery table**:

| Stage-1 punch-list # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---|---|---:|---:|---|---|

## Output Contract

Write to `restart/audit/hardening/HARDENING-STAGE-2-{TARGET}.md`, ~600-1500 lines, structured §1-§9:

### §1 — Target identification

Stage-1 report path + commit. Underlying target. Stage-2 lines audited. Time budget.

### §2 — Cohort verdict

| Lane | Stage-2 verdict | Notes |
|---|---|---|
| 2A Confirmation-Drift | … | … |
| 2B Discipline Lapse | … | … |
| 2C Steelman | … | … |
| 2D Verdict Imbalance | … | … |
| 2E Recommendation Quality | … | … |

Final Stage-2 decision:
- **STAGE-1 RATIFIED** — Stage-1 audit holds; advance to V2 re-issue
- **STAGE-1 AMENDMENTS REQUIRED** — Stage-2 punch list applies; specific Stage-1 verdicts amend; advance to V2 with amendments folded
- **STAGE-1 RE-AUDIT** — Stage-1's audit is unsound; the underlying target needs a fresh Stage-1 invocation under stricter discipline

### §3 — §7 — One section per lane

Per lane:
- Lane standard (one paragraph)
- Per-item table (Lane 2A) / per-lane table (Lane 2B) / per-decision table (Lane 2C) / cohort distribution (Lane 2D) / per-surgery table (Lane 2E)
- Lane verdict line

### §8 — Stage-2 Punch List

Ordered list of amendments to Stage 1's verdicts and recommendations. Per entry:
- Item number
- Target Stage-1 site (path:line)
- Stage-1 verdict to amend
- Stage-2 amended verdict
- Reason for amendment (cite Lane 2A / 2B / 2C / 2D / 2E)
- Owner (which downstream agent applies; usually the V2 re-issue agent)

### §9 — Final readiness

End with:

> **Stage-2 Decision: {Stage-1 RATIFIED / amendments required / re-audit}**
>
> {summary in 3-5 sentences explaining why Stage 1 holds, where it falls short, and what Stage 2 surfaces}
>
> Hereupon {next step in greenfield restart sequence}.

## Methodology

You are the second adversary. Your role is to challenge the first adversary, not the target.

- **Treat Stage 1 as a peer-reviewed paper** — assume its conclusions are tentative until each is independently challenged
- **Steelman every Stage-1 KEEP** — construct the strongest counter-argument before accepting the verdict
- **Read the underlying target alongside Stage 1** — Stage-1 silence on a target item is a fault Stage 2 surfaces
- **Discipline-audit, don't re-audit** — Stage 2 does not redo Stage 1's nine lanes. Stage 2 evaluates whether Stage 1 *applied* its nine lanes with discipline.
- **Steelman ≠ disagreement** — a Stage-2 SURVIVES verdict means the steelman failed; do not invent disagreement to justify Stage 2's existence
- **Use the verdict-imbalance signal** — if Stage 1 ratifies >85% with thin Challenges, the target may need Stage-1 re-audit, not just amendments
- **No relitigation of locks or precepts** — the 14 locks are settled; Stage 2 verifies Stage 1 verified them, not the locks themselves
- **Cite path:line** for every Stage-1 row Stage 2 amends

## Voice + discipline locks

(Per `restart/prompts/README.md` §Voice. Calibrated; archaic-permissive; no metalanguage; path:line citations; tables liberal.)

## Hard cap

45 minutes per Stage-2 target (60 minutes for Stage-1-MASTER-PLAN target). At minute 41 (or 55) commit work-in-progress. At minute 45 (or 60) halt and report.

## Output commit

`docs(restart/audit/hardening): stage-2 hardening pass against {target}`.

The commit body summarises the cohort verdict + final Stage-2 decision in one paragraph.

## Cross-tranche scope boundary

Touch ONLY `restart/audit/hardening/HARDENING-STAGE-2-{TARGET}.md`. Do NOT modify the Stage-1 report. Do NOT modify the underlying target. Do NOT modify `crates/`, `docs/tranches/`, `docs/precepts/`, `restart/prompts/` (suite definition; read-only). Do NOT execute git operations beyond a single commit at completion.

## Background

Stage 2 closes the two-stage hardening protocol. Both stages must commit before:

- The master plan re-issues as V2 (the V2 re-issue agent consumes Stage-1 + Stage-2 punch lists + amendments)
- Tranche drafting opens (the 10 tranche-drafting agents read V2 only)

Stage 2 is invoked once per Stage-1 report (one for each of PASS-A, PASS-B, PASS-C, MASTER-PLAN). After all four Stage-2 invocations commit, the V2 re-issue proceeds.

The protocol may be summarised:

```
Three passes ──► Synthesizer ──► Stage-1 hardening (4 targets) ──► Stage-2 hardening (4 targets) ──► V2 re-issue ──► Tranche drafting (10 agents) ──► Tranche execution (out of suite scope)
```

Stage 2 is not optional. The greenfield mandate requires every architectural commitment to survive a fresh adversary; Stage 2 is the structural realisation of that requirement.

## What Stage 2 is NOT

- **Not a re-do of Stage 1's nine lanes.** Stage 2 has its own five lanes (2A through 2E) addressing audit-quality concerns specific to second-order review.
- **Not a re-litigation of the underlying target.** Stage 2 reads the target only to evaluate whether Stage 1's audit of the target was sound.
- **Not a relitigation of the 14 locks or precepts.** Both are settled.
- **Not a higher authority than Stage 1.** Stage 2's role is to challenge; Stage 1's role is to challenge. They differ in adversarial position (target vs Stage-1-of-target). Where Stage 2 amends Stage 1, the V2 re-issue agent reconciles; where Stage 2 ratifies Stage 1, the verdict stands.
