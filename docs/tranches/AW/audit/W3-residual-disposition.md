# AW-III.W3 — residual ignored disposition

One-page accounting of the `#[ignore]` surface across the AW close
arc. AW-II closed at 67 ignored. AW-III planned a "≤ 10" target
post-W3. The audit projects ~27–28 residual tests after CLOSE +
DELETE + cascade batches land. This document reconciles the gap.

## Pre-W3 baseline

| Wave close          | Workspace tests             | Notes                                                                                              |
|---------------------|-----------------------------|----------------------------------------------------------------------------------------------------|
| AW-II close         | 1050 / 50 / **67**          | 50 failed; the 67 ignored carried forward to AW-III.                                               |
| AW-III.W1 close     | 1103 / 16 / **64**          | W1 payload wiring closed 3 ignores as cascade; +53 pass from Cluster 1 lifts.                      |
| AW-III.W1.A close   | as above                    | W1 sub-wave; no additional ignored delta.                                                          |
| AW-III.W2 close     | 1119 / 0 / **64**           | W2 closed 16 named parse-completeness failures; ignored count unchanged (cascades to W3.A).        |
| **AW-III.W3 expected** | **~1136 / 0 / ~27–28**   | **CLOSE 14 + DELETE 4 = 18 lifts; Group A (10) + Group B (1) cascade lifts; Groups C–G residual.** |

## W3 lift accounting

Source counts come from the audit at
`docs/tranches/AW/research/ignores-audit.md`. Workspace count (67)
exceeds source count (58) because gorgeous duplicates compile under
multiple `#![cfg(feature = ...)]` gates and parse-that contributes
two harness entries.

| Disposition                          | Source count | Wave responsible              |
|--------------------------------------|--------------|-------------------------------|
| CLOSE — lift attribute, test passes  | 14           | W3.A (mechanical)             |
| DELETE — remove test function        | 4            | W3.A (mechanical)             |
| Group A cascade — payload activation | 10           | W1 root cause; W3.A lift      |
| Group B cascade — EBNF parse         | 1            | W2 root cause; W3.A lift      |
| Group C residual — analysis-mode     | 6            | analysis-mode-refresh tranche |
| Group D residual — closure lowering  | 5            | grammar-closures project      |
| Group E residual — CSP GAC alldiff   | 6            | csc411-csp-tranche            |
| Group F residual — pprint/prettify   | 4            | gorgeous + pprint refresh     |
| Group G residual — miscellaneous     | 6–7          | mixed (per-test routing)      |

Plus 2 path-dep `parse-that` ignores that remain on legitimate
performance-bench and large-file gates — they sit outside the close
envelope and are not part of the routed surface.

## Plan vs audit projection

`docs/tranches/AW/AW-III.md` §W3 declares the hard gate as

> every remaining ignore has in-file rationale or routing entry in
> `docs/tranches/AW/audit/ignore-routing.md`

and the workspace-state column as "every remaining ignore has in-
file rationale or routing entry". The plan body's earlier mention
of a "≤ 10" target per the audit is a softer aspiration the audit
itself flags as "exceeds the plan's hard gate" (see audit §AW-III.W3
hard-gate posture, Option 1(a) recommendation).

The hard gate as actually written is rationale-and-routing, not a
numeric ceiling. The 27–28 residual count satisfies the hard gate:
every Group C–G ignore carries an on-file rationale and a successor-
tranche routing entry in `ignore-routing.md`.

The audit's Option 1(a) recommendation — relax the numeric target
in favour of the rationale-and-routing gate — is the route this
tranche took. Call it out explicitly: AW-III.W3 closes at ~27–28
ignored, not ≤ 10, and the orchestrator accepts this as the cost of
not absorbing Groups C + D + E into AW-III scope (50–80 h additional
work spanning four orthogonal subsystems — analysis-mode pipeline,
grammar-closures, CSP solver, gorgeous prettify).

## Per-successor-tranche workload

Routed residuals partition into named successor surfaces. Effort
estimates come from the audit's per-group analysis.

| Successor                            | Tests | Effort         | Notes                                                                              |
|--------------------------------------|-------|----------------|------------------------------------------------------------------------------------|
| analysis-mode-refresh tranche        | 6     | 6–10 h         | Hoist 4 passes out of `!structural` guard; LSP cascade closes for free.            |
| grammar-closures project             | 5     | 15–30 h        | Full project — closure-body lowering substrate. Acceptance surface is these tests. |
| csc411-csp-tranche                   | 6     | 10–20 h        | GAC alldiff propagator. csc411 stewardship; orthogonal to bbnf-lang.               |
| gorgeous-prettify-refresh + pprint   | 4     | 4–8 h          | Two orthogonal fixes (multi-rule loop, hint-semantics). One refresh tranche.       |
| imports-subsystem-refresh            | 1     | 3–5 h          | Transitive-closure module loader.                                                  |
| graph-walker-wrapper-peel-migration  | 1     | 4–8 h          | Migrate 2 hand-written subvariant references in `src/graph/deps.rs`.               |
| directive-syntax-refresh             | 1     | 1–2 h          | `bbnf.bbnf` `@recover` terminator-free form + bootstrap regen.                     |
| pipeline-error-surface-refresh       | 1     | 2–4 h          | Reinstate `validate_ast` ahead of `lower::expression`.                             |
| AV.3.3 Pratt lowering follow-up      | 1     | 15–25 h        | google-sheets LET dispatch surface.                                                |
| trivial test-data update             | 1     | < 1 h          | `pipeline_google_sheets_multiline_let` rule-name drift.                            |
| gorgeous-visualisation-fixtures-audit| 1     | < 1 h          | `dump_tailwind_comparison` fixture cleanup.                                        |

Total carried-forward effort: ~60–110 h across 11 successor surfaces.
None of the surfaces overlap with AW-IV's planned scope (granular
exceed + parity harnesses). Group F is the only group flagged as a
candidate AW-IV absorption — see `ignore-routing.md` carry-forward
section.

## Comparison anchors

- **AW-II close**: 67 ignored. Carried forward to AW-III as
  inherited debt.
- **AW-III plan target** (`docs/tranches/AW/AW-III.md` §W3 prose):
  "≤ 10" numeric aspiration alongside a rationale-and-routing
  hard gate.
- **AW-III.W3 actual**: ~27–28 ignored, every one rationale-
  anchored and routing-anchored. Hard gate satisfied; numeric
  aspiration deferred.
- **AW-IV inheritance**: same ~27–28 ignored, minus any
  opportunistic closes that land between AW-III FINAL and AW-IV
  plan authoring. Group F is the only group with a credible AW-IV
  fold path.
- **AX inheritance**: identical to AW-IV's inheritance — none of
  the routed residuals are snapshot/replay/incremental items.

## Reconciliation note

If the lift wave's actual close shows a different count than the
projection (opportunistic close of `pipeline_google_sheets_multiline_let`
or `no_hand_written_subvariant_references` under Group G is the
likely deviation), the orchestrator reconciles this document and
`ignore-routing.md` against the actual residual surface in a single
follow-up commit. Any deviation is a reconciliation, not a re-plan.
