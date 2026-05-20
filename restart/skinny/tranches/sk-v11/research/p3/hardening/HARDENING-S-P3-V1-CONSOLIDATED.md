# SK-V11 S-P3 V1 CHALLENGE Consolidated Disposition

Pass: S-P3 Synthesis-Plan.
Cycle: V1 CHALLENGE.
Date: 2026-05-20.
Disposition: REVISE.

## Verdict

V1 does not converge. Four lenses accepted the packet and two lenses returned
load-bearing REVISE findings.

| Lens | Disposition | Artifact |
|---|---|---|
| CH1 correctness | REVISE | `V1/CH1-correctness.md` |
| CH2 generality / Lock 14 | ACCEPT | `V1/CH2-generality-lock14.md` |
| CH3 regression / REDRESS | ACCEPT | `V1/CH3-regression-redress.md` |
| CH4 cost / wave budget | REVISE | `V1/CH4-cost-budget.md` |
| CH5 hidden coupling | ACCEPT | `V1/CH5-hidden-coupling.md` |
| CH6 anti-paper-close | ACCEPT | `V1/CH6-anti-paper-close.md` |

## Accepted Ground

- V1 correctly retires parse-only SOTA movement and the SK-V9 W3 union/event
  substrate family.
- V1 correctly makes non-JSON generality a measured generated direct/typed
  parser requirement rather than a Lock 14 prose claim.
- V1 correctly pre-blocks sidecars, alternate substrates, direct-vs-typed
  relabeling, already-wired SIMD proof rebranding, output-sink masking, and
  generic JSON policy leaks.
- V1 correctly requires same-wave consumer, scalar/oracle proof, checkasm or
  differential where applicable, guard floors, and REDRESS-backed revert paths.

## Required V2 Folds

1. Use one guard-floor authority. V2 must reconcile P3-A with P3-C/SPEC and
   publish a single direct/typed maintain formula and table.
2. Use one telemetry identifier authority. V2 must reconcile SPEC §0.3 with
   P3-D, including comparator value/source and validation identifiers.
3. Make W1 solely responsible for the non-JSON baseline and W2 solely
   responsible for the first non-JSON intervention. W2 may not create the
   baseline and admit an intervention in the same redress wave.
4. Use one rounding rule for the non-JSON 1% improvement floor.
5. Reconcile `y_string_unicode`: it is a residual direct row, not an admitted
   guard. V2 must either make it a selectable W5 target or route it explicitly
   to W6/W8 with floor-bearing ownership.
6. Make the bracket budget explicit. V2 must account for W0-W9 plus only two
   available sub-wave slots before the skinny `> 12` escalation rule.
7. Split W1 or require an already-runnable generated non-JSON baseline plus
   independent oracle before W1 dispatch. The accepted fold is to split W1
   into a gate/report lane and a generated baseline/oracle lane.
8. Separate handwritten source/test/gate LOC budgets from regenerated output
   budgets and require named generator inputs for generated diffs.
9. Add row-count caps for W3-W7 redress plans and make W8 docs/gate-only by
   default. Any W8 source work requires a CHALLENGE-accepted W8a/W8b split
   while staying inside the bracket ceiling.
10. Align SPEC and DISPATCH so W1 challenge is mandatory and costed.

## Next Action

Fold these findings into an S-P3 V2 packet and run another six-lens CHALLENGE.
V1 is archived evidence only and is not dispatch authority.
