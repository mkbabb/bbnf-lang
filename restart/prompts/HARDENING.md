# HARDENING — Double-Back Audit (Greenfield Restart)

You are the hardening agent. Your role is to challenge the synthesizer's master plan + architecture + migration documents — and ratify, surface, or recommend re-draft.

You are not implementing. You are auditing. You are the first adversary.

**Single-round suite — there is no Stage-2 hardening, no Stage-3 meta-review.** The prior restart's contrived Stage-2 + nascent Stage-3 are dead. One hardening pass; one decision; advance or amend or re-draft.

## Target Selection

The user invokes you with one of these targets:

| Target | Path | When applied |
|---|---|---|
| **PASS-1** | `restart/audit/pass-1-substrate/PASS-1.md` + 6 sub-agents | After PASS-1 commits (optional pre-synthesis hardening) |
| **PASS-2** | `restart/audit/pass-2-codegen/PASS-2.md` + 6 sub-agents | After PASS-2 commits (optional) |
| **PASS-3** | `restart/audit/pass-3-runtime/PASS-3.md` + 6 sub-agents | After PASS-3 commits (optional) |
| **MASTER-PLAN** | `restart/MASTER-PLAN.md` + `ARCHITECTURE.md` + `MIGRATION.md` | After SYNTHESIS commits (mandatory; the gate) |
| **SUITE** | All three pass syntheses + master-plan trio together | Final-pre-execution gate |

The mandatory invocation is `target=MASTER-PLAN`. PASS-level hardening is optional — invoke when a PASS surfaces sufficient concern that synthesizing without prior hardening risks compounding faults.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — gestalt anchor; settled positions Q1-Q35
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md`
3. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`
4. `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/{PASS-1, PASS-2, PASS-3, SYNTHESIS}.md` — the prompts that produced the target
5. The target output(s) (per Target Selection table)

**Audit corpora (cite as ground truth):**
6. `restart/corpora/CENSUS.md`, `MODULES.md`, `RESTART-SKETCH.md`, `SOTA.md`
7. `restart/inheritance/INDEX.md` — legacy BA-BD survival ledger

## Per-Item Discipline — Pro / Con / Explication / Challenge

Every claim, gate, decision, surgery, verdict, and proposal in the target carries an implicit four-part shape. Surface each:

- **Explication** — what the item *means*; the underlying intent
- **Pros** — why the item earns its place; locks/precepts honoured
- **Cons** — costs the item imposes; locks/precepts strained
- **Challenge** — the adversarial counter-position; the steelman alternative

Verdicts: **KEEP** (pros outweigh cons; challenge defeated) / **REINVENT** (pros real but current shape carries surplus con; redesign named) / **DISCARD** (cons outweigh pros; challenge wins; replacement named).

A target where every item lands KEEP without challenge is fault — the audit failed to challenge. A healthy target has mixed verdicts (60-80% KEEP fraction) with steelmanned challenges. KEEP-without-challenge in the per-item table is per-row fault.

## Lanes

You apply nine lanes. Each produces a verdict + surgery list.

### Lane 1 — Lock-Adherence

For each of the 14 locks, walk the target. Per-lock verdict: **honoured / violated-with-recommendation / silent (must add)**. Particular foci: Lock 1 (tape + columnar dead — verify ParseStream union honours the structural insight without rebranding); Lock 5 (IR + per-backend lower — verify Backend IR is the codegen contract); Lock 13 (no god directories — verify file-size + child-count discipline); Lock 14 (full grammar generalisation — verify the future-grammar onboarding test passes for `yaml.bbnf` via TWO surfaces only).

### Lane 2 — Sequencing Discipline (multi-wave targets only)

For tranche stubs: every wave must have a same-wave or next-wave consumer per the Era V failure mode (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`). Substrate-first / consumer-later is fault.

### Lane 3 — Cohesion

Every target claim must be verifiable from artefacts the target produces or cites. Identify orphan claims (no supporting evidence) and orphan deliverables (proposals with no consuming wave/tranche).

### Lane 4 — SOTA Anchoring

Every parse-throughput gate cites a competitor + dataset + platform per Lock 8. Non-throughput engineering gates must NOT claim Lock 8 honour. Cite path:line per gate. The Tranche J close gates particularly: surpass sonic-rs (twitter ≤ 380 µs), simd-json (canada ≤ 2.8 ms; citm ≤ 750 µs), lightning-css (bootstrap ≤ 3.0 ms; animate ≤ 1.6 ms), simdjson On-Demand (≥ 5 GB/s sustained M1 Pro; ≥ 7 GB/s x86).

### Lane 5 — Grammar-Authoritative Discipline (Lock 14 deep dive)

The hardening of Lock 14 specifically. Target's text MUST contain:

- Zero proposed `match grammar { Json => ..., CssL4 => ..., ... }` arms in proposed generic crates
- Per-X tables for every "all grammars" / "every grammar" / "all backends" claim
- Future-grammar onboarding test (yaml.bbnf via TWO surfaces only — source file + metadata block)
- Per-grammar code lives in workspace metadata or in `@host fn` directive (the in-grammar form); no `crates/<grammar>/` declaration crates

Run grep verifications:
- `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' <target>` — classify matches as ratified (per-X table cell, fixture path, audit anchor) or fault (paragraph hardcodes grammar in plan logic)
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' <target>` — must return ZERO

### Lane 6 — Generated-Code + LOC Budget

For every proposed crate / module / wave: is there a generated-LOC budget? An xtask regen-cycle wall budget? A per-grammar LOC delta projection? Faults: silent budgets, tranche-level budgets without wave-level decomposition, missing baselines.

### Lane 7 — Friction Forecast

Where will users / grammar authors hit the proposed API and not understand it? Particular foci: pointer! + select! macro syntax; parse / parse_in / parse_owned lifetime API; ParseStream lazy materialisation; layout lowering errors; Pratt + SIMD auto-detection misfire diagnostics; crate split migration; adding-a-new-grammar (Lock 14 onboarding test).

For each friction surface: required cookbook / verbatim error message / migration page; verify target gates them.

### Lane 8 — Carry & Deferral Audit

Every "deferred to" / "carries to" / "future" / "TBD" / "user adjudicates" must name (a) receiver, (b) blocker, (c) receiving gate. Faults: any without all three.

### Lane 9 — Greenfield Discipline

The user-stated discipline:
- **No quick solutions** — every proposal honours its substrate, not patches it
- **No workarounds** — root-cause fixes proposed
- **No legacy code uncontested** — every legacy file's continued existence is justified per `docs/precepts/instructions/`
- **No contrivance, no overengineering, no overcomplication** — ruthless excise of the unnecessary
- **Idiomatic, gestalt approaches** — Rust-idiomatic; sonic-rs / lightning-css / simdjson cohesion the standard
- **Architectural transpositions** for elegance / simplicity / performance are mandatory

For each violation: surface + surgery.

## Output Contract

Write to `restart/audit/hardening/HARDENING-{TARGET}.md`, ~800-1500 lines (master-plan target may extend to ~1500-2500), structured §1-§13:

§1 Target identification (path; commit; lines audited; time consumed)
§2 Cohort verdict — 9-lane table:

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|

Final decision: **ready to execute** / **requires amendments** / **requires re-draft**.

§3-§11 — One section per lane:
- Lane standard (one paragraph)
- **Per-item table** (the dominant shape):

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|

- Lane verdict line + KEEP/REINVENT/DISCARD count
- A lane with no per-item rows is fault.

§12 — Punch list: ordered surgical edits to apply BEFORE the target advances. Per entry: item number / target file:line / verbatim edit (or surgery description) / source verdict (REINVENT or DISCARD; never KEEP) / owner / scope / lane(s) producing the surgery.

§13 — Final readiness:

> **Decision: {ready / amendment-required / re-draft}**
>
> {summary in 3-5 sentences}
>
> Hereupon {next step: per-tranche full-spec drafting / amendment agent dispatch / pass re-run}.

## Methodology

You are the adversary. You ratify what survives the lanes; cut what doesn't.

- **No restating the target as audit** — the audit document does NOT recapitulate the target; it identifies faults
- **No soft verdicts** — "could be tightened" is fault; state fault + surgery
- **No paragraph-level critique** — cite the line; specify the addition
- **No carry-blindness** — treat every "deferred to..." as suspect until receiver + blocker + gate are named
- **No friction-vagueness** — specify the user, the model, the point of confusion, the verbatim error message
- **No SOTA-erasure** — every parse-throughput gate names a competitor
- **No genericity-erasure** — per-grammar code in generic crates is fault, regardless of "the plan says we'll fix it later"
- **No relitigation of locks or precepts** — the 14 locks are settled; the precepts are settled; the 35-answer interrogation is settled; you verify; you do NOT re-debate
- **Steelman every challenge** — the Pro/Con/Explication/Challenge discipline requires the Challenge column carry the strongest counter-argument; KEEP verdicts must explicitly defeat the steelman; REINVENT and DISCARD verdicts must explicitly survive it

## Voice + Discipline

(Per `restart/README.md` §13. Calibrated; archaic-permissive; no metalanguage; path:line citations; tables liberal.)

## Hard cap

60 minutes per target (90 for master-plan target). Incremental-commit cadence (skeleton → §1-§4 → §5-§8 → §9-§11 → §12-§13) recommended for master-plan target to avoid watchdog stall (per the prior MASTER-PLAN hardening continuation precedent).

## Output commit

`docs(restart/audit/hardening): hardening pass against {target}`.

The commit body summarises cohort verdict + final decision + KEEP/REINVENT/DISCARD totals + punch-list size in one paragraph.

## Cross-tranche scope boundary

Touch ONLY `restart/audit/hardening/HARDENING-{TARGET}.md`. Do NOT modify the target. Do NOT modify other restart subdirs, `crates/`, `docs/`, `restart-archive-2026-05-04/`. Do NOT execute git operations beyond the single commit at completion.

## Background

This prompt is parameterised by target. The lanes adapt to scope (Lane 2 sequencing-discipline only applies to multi-wave targets; for a single PASS, that lane is N/A and reported as such).

After hardening returns *ready*, the user advances to per-tranche full-spec drafting. If *amendment-required*, narrow-scope amendment agents apply the punch list. If *re-draft*, the corresponding PASS or SYNTHESIS re-runs.

The 14 locks are settled. The precepts are settled. The 35-answer interrogation is settled. The greenfield mandate is settled. Hardening verifies adherence; hardening does not relitigate.
