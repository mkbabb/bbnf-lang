# SK-V12 W1 CHALLENGE - V1 Disposition

Date: 2026-05-20.
Scope: adversarial review of
`restart/skinny/tranches/sk-v12/research/skv12-W1-plan.md` at commit
`ab21b518`.
Output: this file.

## Disposition

Overall disposition: REJECT / route back to plan.

The plan selected the right route, but it is not yet redressable. CH1 rejected
because the plan did not name enough exact paths and commands to make
`G-W1-GENERATED-NONJSON-BASELINE` fully measurable before source work. CH4
rejected because the plan did not provide a component LOC/time budget for the
expanded schema, Lock 14, runtime, bench, oracle, and equality slice.

The Sheets fallback remains eligible. The plan must be revised, then
CHALLENGE rerun before W1 redress.

## CH1 Correctness - REJECT

The selected row is exact and legal:
`sheets/formula/direct_to_struct/main`. The CSS skip is supported by the
committed CSS preflight, and the selected-target discipline is correct.

Reject reason: SPEC Section 4 requires the plan to name the generated Track 1
path, runtime module path, fixture corpus, independent oracle/Track 2 path,
strict equality command, gate command, and rollback slice. The plan named the
row and runtime directory but left fixture corpus, oracle path, generated Track
1 source path, and equality command too generic. The Lock 14 authorization
requirement was also load-bearing but not backed by a resolving file:line input
citation in the plan.

Required revision: add exact file paths, artifact paths, equality command, and
Lock 14 citation.

## CH2 Generality / Lock 14 - ACCEPT

No CH2 blocker. The plan legally skips CSS with a concrete owner-surface
failure and selects one Sheets direct/sink row only. It preserves Section 2.1
boundaries: no generic JSON policy, no JSON-provider clone, no directive, no
BIR/backend shape addition, generated runtime under `grammars/sheets`, and
independent oracle/gate consumption.

Carry-forward risk: source does not yet contain the SK-V12 W1 Lock 14
authorization, so redress must land that executable authorization in the same
measured slice.

## CH3 Regression / REDRESS - ACCEPT

No CH3 blocker. The plan carries regression and REDRESS risks explicitly: CSS
is skipped only after an owner-surface preflight failure, Sheets is the single
selected target, and redress cannot fall through to BBNF-self.

The plan blocks report-fixture baseline, stale `sheets_witness`,
JSON-provider clone, hand parser, generic JSON policy, typed shortcut, and
source-only baseline routes. The revert protocol is adequate in shape.

## CH4 Cost - REJECT

Reject reason: the plan requires codegen provider work, generated Sheets
runtime module, fixture corpus, independent oracle, Criterion bench, companion
schema/gate extension, Lock 14 authorization, and artifact-backed equality in
one redress, but it does not cost those components. That is not credible
against the <=480 non-generated LOC and <=75 minute redress caps.

Required revision: add a concrete component LOC/time table and narrow the
redress surface enough to fit, or route W1 to measured BLOCKED / S-P3 revision
under the no-split rule.

## CH5 Hidden Coupling - ACCEPT

No CH5 blocker. The plan requires generated Sheets Track 1, independent
same-plane oracle/Track 2, Criterion row, strict equality artifact, and gate
consumption in the same wave. It blocks hand parser, JSON-provider clone, and
report-only close.

Carry-forward risk: the live companion schema still omits several Section 0.4
fields and uses `deny_unknown_fields`; redress must actually extend it.

## CH6 Next-Tranche Impact - ACCEPT

No CH6 blocker. Rollback is explicit, W2 baseline needs are preserved if W1
admits, and a Sheets block routes W2/W3 to close/reroute rather than consuming
BBNF-self inside W1. The plan does not reopen JSON direct, parse-only, or
substrate routes.

## Required Plan Revision

The V2 plan must:

1. Name exact generated Track 1 source, runtime module, fixture corpus, oracle
   source, equality artifact, Criterion artifact, report artifact, gate command,
   and rollback slice.
2. Cite the executable Lock 14 freeze/parent-diff code that requires W1
   authorization.
3. Add a component LOC/time table whose non-generated total fits the Sheets
   <=480 LOC cap and <=75 minute redress cap.
4. Keep the selected target unchanged: `sheets/formula/direct_to_struct/main`.
