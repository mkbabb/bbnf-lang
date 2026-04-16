# Tranche AO — Retrospective (R-AO)

Span: 2026-04-12 19:56 → 23:11 (3h15m; 5 commits). AP opened 2h40m
after AO's last code commit, absorbing every unlanded item.

## Commits

| Hash | Subject |
|---|---|
| `e64164e4` | AO plan |
| `8c4254ab` | expand AO plan with full phase specs |
| `4114695b` | `compute_structural_bytes` IR pass (AO.0.1) |
| `b2167e0e` | kernel scanner rename (AO.2.2) |
| `7198c974` | structural pre-scan + WS elision (AO.0.4-0.6) |

## 1. Scope vs plan

Plan: six phases (structural, padded buffer, parse-that gen, SIMD
widening, cost model + global CSP, correctness/polish), five waves,
15 agent-slots. Reality: Phase 0 sub-items 0.1/0.4/0.5/0.6 landed;
0.2/0.3 collapsed into `7198c974` with no independent review; 2.2
landed only as rename churn. **Phases 1, 3, 4, 5 never existed as
commits.** AO.md header confesses *"code complete for Phase 0,
never exercised end-to-end"*.

## 2. Silent vs declared deferrals

Silent. Phase 1, 3.1/3.2, 4.1/4.2/4.3, 5.1–5.4 vanished without
plan-time deferral. They re-surfaced as "absorbed into AP" via a
retroactive status block. Violates README §Code discipline — *NO
deferrals […] accepted only when the plan document declares the
deferral explicitly, with rationale, at plan time*.

## 3. Orchestration friction

No `PROGRESS.md` (directory structure mandating it landed only at
`36945f60`, post-AW). No dated wave log. Code commits carry no
"wave 2/3" handoff. Plan specified five waves; execution was
sequential-solo.

## 4. Agent-layer friction

Not observable — no `research/`, `audit/`, or `PROGRESS.md`
artefacts show sub-agent dispatch. The 3h15m span and five-commit
density suggest AO ran without the multi-agent parallelism the
plan's "Execution Waves (4 agents per wave)" structure required.

## 5. Edict adherence

- **One codegen path.** `7198c974` wires a structural path the
  proc-macro derive route never reaches — the *"additive
  shadow-surface that keeps a legacy path alive beside a partial
  replacement"* the edict forbids.
- **No deferrals.** Four phases silently punted.
- **Commit frequently.** Five commits for six phases; 0.2–0.6
  batched into one commit.
- **Tranche completion.** No `FINAL.md`, no `post-AO.json`, no
  recorded test pass. AP baseline marks AO parse impact **0%**.

## 6. Chronic deferrals IN / OUT

**In:** tailwind parse (AN.0.5 → AO 5.1 → AP.0.2), global CSP
(AN 5.3 → AO 4.2 → AP.6.5), cost calibration (AM.6 → AO 4.1 →
AP.6.4), SIMD quote parity (AO 0.3 → AP.3.4). **Out:** every
unlanded phase, plus the *structural-never-activated* regression
and the AO.2.2 rename that silently killed JSON payload projection.

## 7. Mid-tranche restructuring

None visible — execution never reached a decision point. The pivot
happened *between* tranches (AO closed → AP opened 2h40m later).
The post-hoc "STATUS: OPEN" stamp onto AO.md (`89f9fe3c`) is
administrative, not corrective.

## 8. Lessons

1. **Infrastructure without activation is zero.** AO ships a full
   IR pass, cursor, codegen hooks — all unreachable because the
   derive path doesn't call them. Plans must require end-to-end
   activation (one production grammar under `cargo expand`) as a
   hard gate, not a Phase 5 polish item.

2. **Rename-only commits are correctness hazards.** `b2167e0e`
   renamed `scan_number_f64` → `scan_json_number_span` as "AO.2.2
   consolidation", silently severing payload wiring in
   `emit_regex_match_impl`. AP.0.3 had to restore it. Rename
   refactors need a passing workspace test as their commit gate.

3. **A 3-hour tranche is a checkpoint, not a tranche.** AO's span
   and commit density match a single wave of a larger effort.
   Labelling it "Tranche AO" invoked completion apparatus
   (`FINAL.md`, `post-AO.json`) never going to be produced. Future
   scope-pivots mid-effort should open a NEW tranche letter (per
   `new-tranche-new-doc`), not retrofit "STATUS: OPEN" onto a
   retrospectively undersized one.
