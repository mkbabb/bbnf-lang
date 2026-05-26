# SK-V14 W5B-FRONTEND CHALLENGE V1 Consolidated

Date: 2026-05-26.
Wave: W5B-FRONTEND.
Cycle: V1.
Disposition: REVISE.
Acceptance: 2/7 lenses ACCEPT; five orphan REVISEs remain.

## Lens Results

| Lens | Disposition | Required folds |
|---|---:|---|
| CH1 Correctness | REVISE | Owner paths, cwd-explicit gates, full-table maintain proof, construct-by-construct lowering table |
| CH2 Generality | ACCEPT | NONE |
| CH3 Regression | REVISE | Fresh full-table maintain gate |
| CH4 Cost | REVISE | Narrow or split W5B-FRONTEND before redress; add per-file LOC budget |
| CH5 Hidden Coupling | REVISE | Reject modified provider/template paths; add provider-reachability and no-sidecar gates |
| CH6 Anti-Paper-Close | REVISE | Owner-path alignment, full-table maintain evidence, negative import tests |
| CH7 Overfit-Prune | ACCEPT | NONE |

## Consolidated Finding

V1 does not converge. The plan's architectural direction is acceptable under
CH2 and CH7: it keeps compatibility constructs as frontend lowering, forbids
public `@ws`, preserves provider/template topology, and avoids P-1 through P-7
overfit recurrence. It is not redress-ready because correctness, regression,
cost, hidden-coupling, and anti-paper-close lenses identify executable gaps.

The core fold is not a prose-only clarification. V2 must change the plan shape:
the gate commands must be executable from explicit working directories; owner
paths must match SPEC or SPEC must be amended; full-table maintain must be a
fresh measurable gate; import failure modes must have exact negative tests; and
the frontend work must be narrowed or sub-sliced so the stated hard cap and LOC
envelope are credible.

## Required Folds

1. Reconcile owner paths with SPEC Section 8B. If `skinny/RESULTS.md` remains
   row-attribution-only and byte-identical, say so explicitly and align the
   plan with the SPEC owner list.
2. Rewrite gate commands using explicit repo-root and skinny-manifest paths so
   no command changes meaning after `cd skinny`.
3. Add an executable full-table maintain gate against `SK-V14-open` within
   +/-1.0% on all rows, or route a SPEC amendment if the requirement is removed.
4. Add a construct-by-construct lowering table with target representation,
   owner file/type, positive test, and fail-closed test for imports, `@ws`,
   `@pretty`, `?w`, `>>`, `<<`, span capture, and typed projections.
5. Narrow or sub-slice W5B-FRONTEND before redress if the 30-minute cap remains
   binding. At minimum, V2 must not claim the full construct set can land under
   the cap without per-file LOC and consumer accounting.
6. Strengthen topology gates to reject modified provider/template files as well
   as add/delete/rename, except `grammar_provider.rs`.
7. Add positive reachability checks for `RuntimeProvider`, `GrammarProfile`, and
   `render_runtime_profile(profile, None)`.
8. Add exact negative import tests for missing import and import-cycle
   fail-closed behavior.
9. Add an explicit no-sidecar clause: frontend IR/facts are request-local only,
   not emitted, retained, parser-owned, or runtime-queryable.

## No-Fold Items

- CH2 accepts Lock 14 generality, non-JSON proof carry, and no new public syntax.
- CH3 accepts REDRESS-209/210/211 ordering and provider-free/deletion ownership
  separation.
- CH7 accepts that the plan avoids P-1 fake generated headers, P-3 fixture
  lookup, P-4 gate relabeling, P-5 scaffold-as-load-bearing, P-6 provider
  centralization, and P-7 Track 1/2 collapse.

## Next Action

Fold the V1 REVISE findings into `skv14-W5B-FRONTEND-plan.md`, commit the
fold, then dispatch W5B-FRONTEND CHALLENGE V2. Redress remains blocked until
the challenge reaches §3Z convergence: >=95% ACCEPT for two consecutive cycles
with zero orphan REVISEs.
