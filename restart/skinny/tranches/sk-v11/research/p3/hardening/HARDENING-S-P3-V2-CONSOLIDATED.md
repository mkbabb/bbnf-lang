# SK-V11 S-P3 V2 CHALLENGE Consolidated Disposition

Pass: S-P3 Synthesis-Plan.
Cycle: V2 CHALLENGE.
Date: 2026-05-20.
Disposition: REVISE.

## Verdict

V2 does not converge. Five lenses accepted the packet and CH1 returned a narrow
REVISE on remaining correctness drift in P3-A and one SPEC wording conflict.

| Lens | Disposition | Artifact |
|---|---|---|
| CH1 correctness | REVISE | `V2/CH1-correctness.md` |
| CH2 generality / Lock 14 | ACCEPT | `V2/CH2-generality-lock14.md` |
| CH3 regression / REDRESS | ACCEPT | `V2/CH3-regression-redress.md` |
| CH4 cost / wave budget | ACCEPT | `V2/CH4-cost-budget.md` |
| CH5 hidden coupling | ACCEPT | `V2/CH5-hidden-coupling.md` |
| CH6 anti-paper-close | ACCEPT | `V2/CH6-anti-paper-close.md` |

## Accepted Ground

- W1 is correctly split into W1a gate/report schema authority and W1b generated
  non-JSON baseline/oracle authority.
- W2 correctly consumes the W1b baseline and uses one rounding rule:
  `ceil(W1b_css_baseline_mbps * 1.01)`.
- SPEC, P3-C, P3-D, and DISPATCH agree on telemetry identifier authority and
  strict direct/typed comparator discipline.
- The 11-wave bracket has one spare split, with W8 source work isolated behind
  a W8a trigger.
- W3/substrate/parse-only pre-blocks remain closed, and no paper-close route is
  accepted.

## Required V3 Folds

1. Replace all stale P3-A typed guard floors with the P3-C/SPEC V2 maintain
   table: `twitter 17385`, `citm_catalog 29928`, `apache_builds 8308`,
   `github_events 11633`, `update_center 11613`, `mesh 9214`, and
   `marine_ik 11552` for Track 1, with Track 2 oracle guards from P3-C/SPEC
   when referenced.
2. Replace the P3-A §3 C6 summary that still says W0/P3-D may create the
   non-JSON performance floor. W1b is the only baseline authority and W2 is the
   first intervention consumer.
3. Remove SPEC W5 task wording that calls residual Unicode rows "guards" when
   the selected work is plain-string. They are residual rows monitored for
   regression and owned by W6/W8 unless selected.

## Next Action

Fold these three issues into an S-P3 V3 packet and rerun CHALLENGE. V2 remains
archived evidence only and is not dispatch authority.
