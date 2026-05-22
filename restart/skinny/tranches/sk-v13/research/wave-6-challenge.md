# SK-V13 W6 CHALLENGE - Bounded E-Graph + Active Cost

Cycle: W6 CHALLENGE. Disposition: ACCEPT WITH CONSTRAINTS.

The W6 plan is admissible under SPEC Section 9 only as a bounded active-cost
selector with a gate-consumed same-wave consumer. It is not admissible as an
e-graph scaffold, cost telemetry surface, or old cascade wrapper.

## CH1 Correctness

PASS with constraints.

The selected candidate must preserve current JSON and admitted CSS behavior
when no fresh row-moving candidate exists. The redress must test deterministic
selection and guard against any shape change caused by missing/stale evidence.

Unknown or stale candidate evidence cannot be treated as a lower cost. The
selected winner itself must be fresh or the wave must abrogate.

## CH2 Generality / Lock 14

PASS with constraints.

The e-graph language and active-cost facts must be grammar-neutral. Shape names,
capacity policies, and W5 FIRST facts are allowed; JSON row names, CSS feature
names, and grammar-specific branches inside generic crates are not.

The local root `crates/egraph` crate is accepted as a dependency surface if it
compiles through a direct skinny path dependency and no root IR coupling enters
skinny. If edition/MSRV/path-patch friction blocks that route, redress may use
the challenge-accepted equivalent selector representation, but the report must
still expose e-graph-equivalent node/class/iteration/memory bounds and active
cost evidence.

W6 must add Lock 14 owner paths for the exact generic files it touches.

## CH3 Regression / REDRESS

PASS with constraints.

REDRESS 84, 87, 114, 115, 119, 120, 121, and 136 remain binding. W6 may cite
them as material differentials; it may not replay them as local JSON fixes or
use them as row-close evidence.

JSON guards and admitted CSS rows must maintain. Any unrecovered guard
regression is a measured reject.

## CH4 Cost

PASS with constraints.

The redress cap is the W5-W9 decision-fold amendment: 45 min implementation +
15 min measurement. Source/test/report LOC must remain within the SPEC W6 budget
or split before commit.

The implementation must enforce:

- final nodes <=100,000.
- final nodes / initial nodes <=16.0.
- iterations <=64 by default and <=100 hard.
- memory estimate or measurement <1 GiB.
- stale cost rate <=30%.
- rewrite-order extraction cost variance <=10%.

Hitting any budget is an abrogate condition, not a reason to loosen the gate.

## CH5 Hidden Coupling

PASS with constraints.

W6 may replace the selection seam at `passes::recognizers`, extend
`ir::CostFacts`, and add a report/gate. It may touch codegen lowering only to
prove or expose consumption of `CostFacts.chosen`. It may not add a new
`BackendShape`, BIR variant, public substrate API, sidecar stream, parser-owned
cursor/list, hidden CSP solver, or grammar-specific branch.

The old P1-P8 cascade may be used only as historical comparison/diagnostic
input. It cannot be the admission path or a silent fallback.

## CH6 Anti-Paper-Close

PASS with constraints.

`G-W6-DECISION-ACTIVE-COST` must reject `support_only`, `gate_only`,
`telemetry_only`, `scaffold_only`, `wired`, `integrated`, `future_consumer`,
empty generated paths, stale-rate >30%, nondeterministic winners, hidden-CSP
states, and old-cascade admission.

If generated runtime code still does not consume the active selected candidate,
W6 may close only with the measured architectural block:

`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME`.

That block must be gate-consumed, recorded in REDRESS, and backed by tests plus
guard measurements. It is not G2 completion; W7 still owns CSP and cascade
fail-closed.

## Accepted Redress Contract

- Add bounded active-cost candidate selection at the `passes` backend-shape
  decision seam.
- Extend cost facts with W6 active-selection telemetry.
- Add `sk-v13-decision-active-cost-v1` report validation and xtask
  passthrough.
- Add W6 Lock 14 owner paths.
- Run the W6 verification commands from the plan.
- Record REDRESS as row admit/movement only if generated runtime consumption is
  proven; otherwise record the named measured architectural block.
