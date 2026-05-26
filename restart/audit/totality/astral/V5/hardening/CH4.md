# Pass Omega V5 CH4 Cost

Date: 2026-05-26.
Scope: V5 W5R cap and propagation cost.
Verdict: ACCEPT after fold.

## Initial Finding

CH4 returned REVISE because the first V5 packet risked expanding the C-1
envelope: W5A inherited the old `<=1.4k` W5 cap, W5B had no concrete source/test
LOC ceiling, and W6 retained `<=2.0k`.

## Fold

The V5 packet now binds:

- W5A: `<=1.0k` C-1 part-A source/test LOC;
- W5B: `<=400` C-1 part-A source/test LOC;
- W5A + W5B combined: `<=1.4k` C-1 part-A;
- W6: unchanged `<=2.0k` C-1 part-B aggregate;
- total C-1 envelope: unchanged `<=3.4k`;
- borrowing from W6 or exceeding the W5A/W5B sub-cap returns REVISE before
  dispatch.

Propagation remains bounded to MASTER-PLAN, SK-V14 SPEC/SYNTHESIS/ORCHESTRATOR/
DISPATCH/HANDOFF, HANDOFF, MIGRATION, and limited skinny corpus surfaces.

## Disposition

ACCEPT. The split is now budget-neutral.
