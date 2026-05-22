# SK-V13 W7 CHALLENGE - CSP + Cascade Fail-Closed

Cycle: W7 CHALLENGE. Disposition: ACCEPT WITH CONSTRAINTS.

The first CHALLENGE pass returned REVISE on CH2, CH5, and CH6. The plan was
revised in commit `64ff00e8f` to add exact Lock 14 owner/scan obligations,
hash-checked CSS/Sheets/BBNF-self witnesses, generated-runtime diff artifacts
for any admit, priority/P1-P8 evidence-only status, fallback invocation
non-admission status, and explicit CSS/static-template plus JSON/sink-only
blocker fields. The rerun passed all six lenses.

## CH1 Correctness

PASS with binding constraints.

`SolveStats::budget_exceeded = true` is always non-admission even if
`csp-solver` returns a best-so-far solution. Empty solution, UNSAT, elapsed
solve time >1000 ms, missing selected rule decision, missing W6 active-cost
facts, missing backend shape, or missing cost facts must fail closed.

If a W6 selected shape remains legal under W7 hard constraints, CSP must
preserve it. Any CSP-induced shape change is non-admission unless it also
changes emitted executable JSON/CSS runtime code and moves or admits a row.

P1-P8 labels, `hard_pruned`, `priority_fired`, and `shape_rank` are evidence
only; legality comes from W7 hard constraints. Compatibility fallbacks may
exist only as `compat_non_admission`, and `fallback_invoked = true` rejects row
movement or admission.

## CH2 Generality / Lock 14

PASS with constraints.

Redress must add `SK_V13_W7_OWNER_PATHS` plus a `sk-v13-waveW7` parent-diff
matcher. Generic scan coverage must include every W7-touched generic module,
including the new `passes::decision_csp` module and codegen lowering surfaces.
Adding a generic file outside scan coverage is a CH2 fail.

CSS L4, Sheets, and BBNF-self witnesses must be artifact-backed, hash-checked,
and command-backed or named as generated-role evidence. Status-only witnesses
reject. Root `crates/csp-solver/` remains a dependency surface, not an edit
surface.

No new `BackendShape`, BIR variant, public substrate API, hidden `UnionTape`,
or JSON-specific generic branch may land.

## CH3 Regression / REDRESS

PASS.

REDRESS 84, 85-87/121, 114/115, 119/120, 136, and 137 remain blocked as
row-close authority. REDRESS-138 may record W7 evidence only; it cannot be used
as standalone row admission.

JSON/CSS guards must maintain or improve. `skinny/RESULTS.md` and
`restart/skinny/ROLLING-SOTA-DELTA.md` are touched only if a row moves or
admits. The revert protocol is sufficient: one-slice revert, rejected patch at
`/tmp/skv13-waveW7-rejected.patch`, and REDRESS-138 with the concrete failed
condition.

## CH4 Cost

PASS, narrowly.

W7 is cost-plausible as CSP finalization plus cascade/fallback fail-closed
evidence. Runtime row movement is a mandatory split unless the CSP/fail-close
slice lands far under budget and every gate predicate can still be implemented,
tested, and measured.

The redress cap remains 45 min implementation + 15 min measurement, with <=970
source/test/report LOC. CSP solve <=1 second per named grammar is plausible for
the current W6 seed surface, but W7 must measure wall-clock time with `Instant`
because `csp-solver` has no wall-clock timeout API.

## CH5 Hidden Coupling

PASS with constraints.

Priority/P1-P8 metadata is evidence-only. `hard_pruned`, `priority_fired`, and
`shape_rank` cannot prune the CSP domain, drive the objective, or admit a row.

The gate/report must require `fallback_invoked = false` for any row movement or
admit claim. Any compile-safety fallback that remains reachable must be
reported as `compat_non_admission`. `static_css_provider_status` and
`json_sink_only_status` are required to prove whether static CSS templates or
JSON sink-only rendering blocked runtime consumption.

`fused_solver_status` must be `not-fused`; path strings through
`lower_to_rust` are insufficient.

## CH6 Anti-Paper-Close

PASS.

Any `pass` or `admitted` status requires a saved, hash-checked,
gate-consumed generated-runtime diff:

- `generated_runtime_diff_status = present`
- `generated_runtime_diff_artifact_path`
- `generated_runtime_diff_sha256`

Without that diff, only the named measured block is accepted:

`JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`.

The gate must reject support-only, telemetry-only, scaffold-only, path-string,
future-consumer, cascade-retirement-only, or fallback-invoked closure.

## Accepted Redress Contract

- Add a direct skinny `csp-solver` dependency and `passes::decision_csp`.
- Run bounded CSP after W6 active-cost fact construction.
- Preserve W6-selected shapes when still legal; treat old priority data as
  evidence-only.
- Add `DecisionCspFacts` and the W7 report/gate/xtask surface.
- Fail-close production fallback surfaces or report compatibility fallback as
  non-admission.
- Add W7 Lock 14 owner paths and scan coverage.
- Produce W7 CSP problem/solution artifacts and CSS/Sheets/BBNF-self witness
  artifacts with SHA-256 hashes.
- Record row movement/admission only with a hash-checked generated-runtime diff
  and strict row measurement; otherwise record the named measured block.
- Run the verification commands from the revised W7 plan.
