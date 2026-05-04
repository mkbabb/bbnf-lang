# AMENDMENT DISPATCH — Four-Wave Surgical Reconciliation (Greenfield Restart)

You are the amendment-dispatch orchestrator. The four-target hardening returned AMENDMENT-REQUIRED at `restart/audit/hardening/HARDENING-CONSOLIDATED.md`; the four reviewers (A consolidation-fidelity, B architectural-integrity, C Lock-14-greenfield, D executability) returned narrow-scope amendments. You sequence the amendment work across four waves, dispatch the appropriate agent(s) per wave, and trigger the hardening rerun. After rerun returns READY, the user advances to per-tranche full-spec drafting.

You are not a hardening agent. You are not an amendment agent. You orchestrate amendment + rerun.

## Required reading (mandatory; in order)

1. `/Users/mkbabb/Programming/bbnf-lang/restart/README.md` — gestalt anchor; settled positions
2. `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md`
3. `/Users/mkbabb/Programming/bbnf-lang/restart/prompts/HARDENING.md` + `HARDENING-ORCHESTRATOR.md` — for rerun reference
4. `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md`
5. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/HARDENING-CONSOLIDATED.md` (619 lines) — the 47-item punch list + routing matrix + gate rerun checklist
6. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/REVIEW-A-CONSOLIDATION-FIDELITY.md` — consolidation drift findings
7. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/REVIEW-B-ARCHITECTURAL-INTEGRITY.md` — three named architectural faults
8. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/REVIEW-C-LOCK-14-GREENFIELD.md` — two narrow Lock-14 additions
9. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/REVIEW-D-PUNCH-LIST-EXECUTABILITY.md` — four-wave plan + pre-fill discipline + routing repairs
10. `/Users/mkbabb/Programming/bbnf-lang/restart/ARCHITECTURE.md` (1259 lines) — primary amendment surface
11. `/Users/mkbabb/Programming/bbnf-lang/restart/MASTER-PLAN.md` (727 lines) — primary amendment surface
12. `/Users/mkbabb/Programming/bbnf-lang/restart/MIGRATION.md` (740 lines) — primary amendment surface
13. `/Users/mkbabb/Programming/bbnf-lang/restart/audit/pass-{1-substrate,2-codegen,3-runtime}/PASS-{1,2,3}.md` — secondary amendment surfaces

## §1 — Verify-Then-Patch Discipline (the central rule)

Reviewer D surfaced this. Seven punch-list items have **substantial pre-existing surgery already landed in the SYNTHESIS trio**: items 15, 21, 29, 30, 31, 40, 41, 44. The naive amendment dispatch would re-author them — wasting 4-6 hours and risking regressions from re-write churn.

The amendment-dispatch contract for every agent: **verify, then patch the delta.** The dispatch prompt for each amendment agent must include:

- The punch-list item's exact surgery directive verbatim
- The current state of the named amendment surface (read first; don't presume)
- An explicit "if the surgery already landed: commit a verification-only stub indicating the item is satisfied; do not re-author"
- The delta-patch language: "if the surgery is partial: patch only the missing portion; cite path:line of what survives"
- The full-author language: "if the surgery is absent: author per the punch-list directive verbatim"

Pre-fill items expected (cite Reviewer D §8.3):

| Item | Likely pre-fill state | Verification command |
|---|---|---|
| 15 (declaration-crate fence) | partial: ARCHITECTURE §5.6 has 5 fields (need 8); PASS-1 §2 has 6 (need 8) | `rg -n 'declaration_crate' restart/ARCHITECTURE.md restart/audit/pass-1-substrate/PASS-1.md` |
| 21 (Lock 13 verification table) | likely full | `rg -n 'child count\|500 LOC' restart/ARCHITECTURE.md restart/MASTER-PLAN.md` |
| 29 (SOTA table) | likely partial | `rg -n 'twitter\|canada\|citm\|bootstrap\|animate' restart/MASTER-PLAN.md` |
| 30 (delete final SOTA escape) | full per Reviewer C ("final SOTA escape closed") | `rg -n 'or formally routed\|may be missed' restart/MASTER-PLAN.md` returns 0 |
| 31 (early H thresholds) | likely partial | `rg -n 'H\.W3\|H\.W4\|H\.W5' restart/MASTER-PLAN.md` |
| 40 (B/C sequencing repair) | full per Reviewer C ("C.W2 ShapeFacts fixture" + B integration gap recording) | `rg -n 'ShapeFacts\|integration gap' restart/MASTER-PLAN.md` |
| 41 (C/E/H consumer repair) | full per Reviewer C ("C.W3 RecognizerFacts feed E-owned BIR snapshots not placeholder hints" + "C.W5 CostFacts feed E.W1 Backend IR builder") | `rg -n 'RecognizerFacts\|CostFacts\|E\.W1' restart/MASTER-PLAN.md` |
| 44 (archive citation correction) | full ("per Lock 12") | `rg -n 'per Lock 12\|per Lock 10' restart/MASTER-PLAN.md` (Lock 12 should hit; Lock 10 should not at archive) |

The amendment dispatch fails if these pre-fills are re-authored. Verify; patch the delta; commit a verification-only stub for full pre-fills.

## §2 — Reviewer Reconciliation Directives

Where reviewers disagreed, the amendment dispatch carries the reconciliation:

### B vs C on sequencing (#40 + #41)

Reviewer B says NOT absorbed. Reviewer C says ABSORBED. Reviewer D's pre-fill table says ABSORBED.

**Reconciliation: C and D are correct.** The sequencing absorption is in MASTER-PLAN. Reviewer B audited an earlier-state snapshot OR was reading PASS-2/PASS-3 (which legitimately don't carry MASTER-PLAN's sequencing). Amendment Wave 2 verifies the existing absorption + cleans residue text only.

### Reviewer A's citation-precision drift

Six punch-list items carry citations 4-15 lines off exact content rows (lane-verdict footers, not content rows). Non-blocking. **Optional citation-precision pass during Wave 4 hardening rerun.** Not a separate amendment.

### Reviewer D's hard mis-routing (#12)

Punch #12 (fixture separation) cites `ARCHITECTURE.md:1132-1138/1151-1162`. Reviewer D found this range is the SOTA gate / Generated LOC budget block, not the fixture allowance. The actual fixture surface lives at `PASS-3.md:272-289` (four-fixture-dir sketch).

**Reconciliation: re-route #12 to PASS-3, not ARCHITECTURE.** Wave 2 dispatches against PASS-3's fixture surface.

### Reviewer D's soft mis-routings (#6, #9)

#6 (block-bodied @host fn) and #9 (recovery directive) are co-routed — both carry SYNTHESIS implications + the primary edit is in PASS-1 / PASS-3 respectively.

**Reconciliation: PASS-1/PASS-3 owns the primary edit; SYNTHESIS gets a co-routed amendment to update its references.**

## §3 — Four-Wave Amendment Plan

Per Reviewer D §7. Each wave is a parallel-agent dispatch (where targets don't share write paths) or a serial dispatch (where they do).

### Wave 1 — Foundations (~3-4 hr; serial)

PASS-1 + PASS-2 share Backend IR ownership; serial.

| Order | Agent | Items | Primary surface |
|---|---|---|---|
| 1.1 | PASS-1 amendment | 1 (BIR ownership), 3 (Grammar IR schema), 4 (BIR payload + invariants) | `restart/audit/pass-1-substrate/PASS-1.md` + sub-agent correction notes |
| 1.2 | PASS-2 amendment | 1 (BIR ownership confirmation; verify-only stub since Wave 1.1 lands the surgery), 2 (lowerer import-deny gate), 4 (BIR payload refinement) | `restart/audit/pass-2-codegen/PASS-2.md` + sub-agent correction notes |

Wave 1 closes when PASS-1 + PASS-2 amendments commit.

### Wave 2 — Core surgeries (~3-5 hr; 4 parallel)

| Agent | Items | Primary surface |
|---|---|---|
| PASS-1 amendment (continuation) | 6 (block-bodied @host fn), 7 (lookbehind surface), 8 (chain syntax + type flow), 20 (per-crate rationale), 38 (delete independent-proceed clause), 45 (closure as research signal), 46 (OpenFrame deletion) | `restart/audit/pass-1-substrate/PASS-1.md` |
| PASS-2 amendment (continuation) | 5 (PASS-3 emission contract), 7 (lookbehind co-amendment), 14 (runtime emission table), 24 (per-grammar generated LOC table), 27 (non-generated LOC + child-count budgets), 28 (xtask wall baseline), 29 (SOTA table — verify + patch delta), 39 (carry ledger), 46 (OpenFrame retirement confirmation) | `restart/audit/pass-2-codegen/PASS-2.md` |
| PASS-3 amendment | 5 (consumer acceptance gates), 9 (@error(recover) consolidation), **12 (fixture separation — RE-ROUTED here from ARCHITECTURE per Reviewer D)**, 13 (per-X grammar proof table feeder), 17 (path crate naming), 18 (`pointer!` macro surface), 19 (`bbnf` aggregator child-count), 26 (PASS-3 generated-surface budget), 33 (BBNF self-host internal gate), 34 (compiler diagnostic ledger), 36 (incremental fallback reporting), 37 (PASS hand-off tables), 47 (registry deletion gate) | `restart/audit/pass-3-runtime/PASS-3.md` |
| SYNTHESIS amendment | 1 (BIR ownership in ARCHITECTURE — verify Wave 1.1 + ratify), 10 (Unicode + rewrite-mode normalisation), 11 (yaml two-surface proof — verify pre-fill + patch fixture-allowance delta), 12 (fixture separation — verify post-PASS-3 amendment), 13 (per-X grammar proof table — **NEW 10×9 architecture-owned table per Reviewer C Lane 2**), 15 (declaration-crate fence — **NEW 8-field expansion per Reviewer C Lane 3**), 21 (Lock 13 verification table — verify + patch delta), 22 (package-name routing), 24 (PASS-2 LOC table promotion), 25 (wave-level generated budget), 29 (SOTA table — verify + patch delta), 30 (delete final SOTA escape — verify-only stub), 31 (early H thresholds — verify + patch delta), 32 (benchmark metadata), 35 (cookbook + migration receivers), 39 (TS/parity/publication carry ledger), 40 (B/C sequencing — **verify-only stub per Reviewer C; do not re-author**), 41 (C/E/H consumer — **verify-only stub per Reviewer C; do not re-author**), 42 (migration crosswalk), 43 (branch/tag operation routing), 44 (archive citation — verify-only stub per Reviewer D), 47 (registry deletion gate consolidation) | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` |

Wave 2 closes when all four amendments commit.

### Wave 3 — Reviewer C narrow additions + B's `bbnf/src/` reconciliation (~1-2 hr; SYNTHESIS only)

The two Reviewer-C additions:

- **Per-X grammar 10×9 architecture-owned table** at `ARCHITECTURE.md` §12.1 — 10 grammars (bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, yaml) × 9 columns (typed root, ValueRef, runtime files, visitor, path schema, fixture manifest, host route, generated LOC, declaration-crate status). Replaces fragments at PASS-2 LOC table (9×2) + PASS-2 runtime emission (10×2) + PASS-1 broad-claim (3×3).
- **Declaration-crate fence — 8-field expansion** at `ARCHITECTURE.md` §5.6 — eight fields (reason, owner, why metadata + @host fn fail, declaration location, no generic import, deletion path, reviewer, receiving gate); reified as TOML keys under `[workspace.metadata.bbnf.grammars.<name>.declaration_crate]`.

Reviewer B's `bbnf/src/` aggregator-tree reconciliation: 3 sources name 3 different layouts. Canonical layout to settle:

- ARCHITECTURE §347-354: 7 children (incl `workspace/`)
- PASS-3 §6: 10 children (incl `tape/` / `visitor/` / `diagnostics/`)
- HARDENING-CONSOLIDATED punch #19: 8 children (incl `query/` / `metadata/`)

The amendment-dispatch picks ONE canonical layout (default: HARDENING-CONSOLIDATED's 8-children spec since the consolidated punch list is the executable source) + propagates to ARCHITECTURE + PASS-3.

Reviewer B's layout-vocabulary reconciliation: `TypeFacts` survives as peer side-table at ARCHITECTURE §7.3 / MASTER-PLAN C.W1 / PASS-1 §3. Per HARDENING-CONSOLIDATED §3 conflict #4, `TypeFacts` is internal-subroutine-only; `LayoutFacts` + `passes::layout` are public. Surgical edit at locks file (Lock 2 stale `bbnf-` prefix at `bbnf-ir/src/passes/layout/`) + ARCHITECTURE §7.3 + MASTER-PLAN C.W1 + PASS-1 §3.

Wave 3 closes when SYNTHESIS amendment for the three additions commits.

### Wave 4 — Hardening rerun (~1-2 hr; single agent)

Dispatches `restart/prompts/HARDENING-ORCHESTRATOR.md` against the amended trio (ARCHITECTURE + MIGRATION + MASTER-PLAN) + the amended PASS syntheses. The four-target hardening reruns; the consolidated verdict gates per-tranche full-spec drafting.

The Wave-4 orchestrator carries a tightened gate-rerun checklist: Reviewer D's 16 commands with **post-condition tightening** (count specs + target-file additions). Surgery per Reviewer D §6:

- 9 of 16 well-formed; rerun unchanged
- 8 need post-condition tightening:
  - Tighten `count = 0` → explicit count assertion (e.g., `wc -l == 1` for single-row expectations)
  - Add target-file additions where the rerun command should grep additional files

The dispatch prompt for Wave 4 must include the tightened checklist.

Wave 4 closes when `HARDENING-CONSOLIDATED-V2.md` (or the rerun's analog) commits with verdict READY.

If Wave 4 returns AMENDMENT-REQUIRED-RERUN, the orchestrator dispatches a narrow-scope amendment for the residual punch list, then re-runs Wave 4. If the residual is small (<5 items), this collapses to a single-agent fix-and-rerun cycle.

If Wave 4 returns RE-DRAFT, escalate to user — does not happen autonomously per the consolidated §5 re-draft thresholds (none currently met).

## §4 — Per-Wave Dispatch Prompts

You compose per-wave dispatch prompts at dispatch time. Each prompt carries:

1. **The wave number + agent role** (e.g., "Wave 2 — PASS-3 amendment agent")
2. **The verify-then-patch discipline** verbatim from §1 above
3. **The reviewer-reconciliation directives** verbatim from §2 above (only those relevant to this wave's items)
4. **The per-item table** for this agent: item # / source punch-list directive / target file:line / surgery type (full-author / patch-delta / verify-only-stub) / pre-fill verification command / acceptance gate
5. **The pre-fill verification step** as Step 1: read the named amendment surface; classify each item as full-author / patch-delta / verify-only-stub; commit the classification before any edits
6. **The voice + discipline locks** per `restart/README.md` §13
7. **The hard cap** (Wave 1: 60 min per agent; Wave 2: 75 min per parallel agent; Wave 3: 60 min single agent; Wave 4: 90 min single hardening orchestrator)
8. **The cross-tranche scope boundary** (touch ONLY the named amendment surface; do NOT modify other restart subdirs, locks, prompts)
9. **The output commit message format** (`docs(restart/{audit/pass-N or trio}): wave-{N} amendment — {scope}`)

The dispatch prompts are NOT pre-written here. You compose at dispatch time, parameterising for the specific wave + agent + items.

## §5 — Pre-Fill Discipline Examples

For each pre-fill item, the dispatch prompt includes:

```
Item 40 (B/C sequencing repair):
  Source: HARDENING-CONSOLIDATED.md punch #40
  Source surgery: "Move ShapeFacts before B.W3, split B.W3 into shell plus C-owned materialization, or change C.W2's consumer away from B direct builder."
  Pre-fill verification command:
    rg -n 'ShapeFacts|integration gap' restart/MASTER-PLAN.md
  Expected pre-fill state (per Reviewer C): MASTER-PLAN already carries
    "C.W2 ShapeFacts fixture in C with explicit B integration gap recording"
  Surgery type: VERIFY-ONLY-STUB
  Acceptance gate: rg returns ≥1 match; commit a one-line verification stub
    in PASS-amendment notes; DO NOT re-author MASTER-PLAN sequencing.
```

```
Item 11 (yaml two-surface proof):
  Source: HARDENING-CONSOLIDATED.md punch #11
  Source surgery: "Add a yaml.bbnf onboarding test that permits exactly a
    grammar source file and one [workspace.metadata.bbnf.grammars.yaml] block."
  Pre-fill verification command:
    rg -n 'yaml.bbnf|workspace.metadata.bbnf.grammars.yaml' restart/ARCHITECTURE.md restart/MASTER-PLAN.md
  Expected pre-fill state (per Reviewer C): MASTER-PLAN.md §11/§12/§24 +
    ARCHITECTURE §12 + MIGRATION §19.6 carry the proof; G.W4 has the gate
  Per-Reviewer-C residue: ARCHITECTURE §1170-1186 carries "fixtures/yaml/*"
    allowance; this is the post-amendment residue per HARDENING punch #12
  Surgery type: PATCH-DELTA
  Surgery: Remove "fixtures/yaml/*" from ARCHITECTURE §1170-1186 onboarding
    allowance; preserve the yaml proof itself
  Acceptance gate: rg -n 'fixtures/yaml/\*' restart/ARCHITECTURE.md returns 0
    in onboarding-allowance section; rg -n 'workspace.metadata.bbnf.grammars.yaml'
    still returns ≥1 in proof section
```

The amendment-dispatch contract is rich enough to prevent re-authoring + tight enough to leave no surgery undone.

## §6 — Acceptance Gate Per Wave

| Wave | Closes when | Verification |
|---|---|---|
| 1 | PASS-1 + PASS-2 amendments commit | `git log --oneline | head -5` shows two PASS amendment commits; rg verification of BIR ownership move |
| 2 | All 4 parallel amendments commit | 4 commits in `git log`; rg verification of every Wave 2 punch-list item's acceptance gate |
| 3 | SYNTHESIS amendment for Reviewer-C additions + Reviewer-B reconciliations commits | rg verification of the 10×9 table + 8-field fence + canonical `bbnf/src/` layout |
| 4 | `HARDENING-CONSOLIDATED-V2.md` commits with verdict READY | rerun verdict not AMENDMENT-REQUIRED, not RE-DRAFT |

## §7 — Closing Posture

You orchestrate four waves; you do not author amendments yourself. Each wave's dispatch prompt carries the verify-then-patch discipline + the reviewer-reconciliation directives + the per-item routing. The pre-fills are verified, not re-authored. The hardening reruns at Wave 4 against the amended trio + amended PASS syntheses; the rerun's READY verdict gates per-tranche full-spec drafting.

Total estimated wall-time: 6.5-9 hours across four waves with per-wave parallelism. Single-agent equivalent: 13-19 hours. Mean dispatch confidence per Reviewer D: 83%.

The 14 locks are settled. The precepts are settled. The 35-answer interrogation is settled. The greenfield mandate is settled. Amendment is surgical reconciliation, not relitigation.

Hereupon Wave 1 dispatches.
