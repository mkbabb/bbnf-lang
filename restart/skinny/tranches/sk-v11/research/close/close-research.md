# SK-V11 W9 Close Research

Pass: Wave Research.
Cycle: W9 Close.
Date: 2026-05-20.
Gate: `G-W9-CLOSE-SK-V11`.

## Scope

Read-only reconciliation of the SK-V11 implementation envelope after W8.
No source, generated parser output, benchmark body, telemetry schema, gate
logic, `RESULTS.md`, or `REDRESS.md` change is made by this research phase.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/w1a/`
- `restart/skinny/tranches/sk-v11/research/w1b/`
- `restart/skinny/tranches/sk-v11/research/w2/`
- `restart/skinny/tranches/sk-v11/research/w3/`
- `restart/skinny/tranches/sk-v11/research/w4/`
- `restart/skinny/tranches/sk-v11/research/w5/`
- `restart/skinny/tranches/sk-v11/research/w6/`
- `restart/skinny/tranches/sk-v11/research/w7/`
- `restart/skinny/tranches/sk-v11/research/w8/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 119

## Wave Disposition Ledger

| Wave | Disposition | REDRESS | Row effect |
|---|---|---:|---|
| W0 | Closed by S-P1/W0 authority | W0 packet | None |
| W1a | Admitted gate/report schema lane | 111 | No JSON row movement; no generated non-JSON baseline |
| W1b | Rejected generated non-JSON baseline | 112 | None |
| W2 | BLOCKED non-JSON intervention route | 113 | None |
| W3 | Rejected numeric direct source route | 114 | None |
| W4 | Rejected container-tail direct source route | 115 | None |
| W5 | BLOCKED string-span route before source dispatch | 116 | None |
| W6 | BLOCKED escaped-segment route before source dispatch | 117 | None |
| W7 | BLOCKED output digest/host-sink route before source dispatch | 118 | None |
| W8 | Closed measured direct fixpoint | 119 | No direct row admission; all 13 residuals receive per-row fixpoint proof |

## Close-Gate Findings

`G-W9-CLOSE-SK-V11` can close only as a measured fixpoint and Alpha-feedback
packet.

- Every Section 0.4 residual direct row now has a REDRESS fixpoint proof in
  REDRESS 119. None moved to `A / GO`.
- The non-JSON generated direct/typed parser intervention axis did not admit.
  REDRESS 113 is the standing BLOCKED route; W8 carries it forward explicitly.
- Existing direct and typed guard rows remain unchanged because W5-W8 made no
  behavior source or `RESULTS.md` movement, and W3/W4 source patches were
  reverted after measurement.
- Parse-only remains diagnostic; no SK-V11 wave reopens the SK-V9 W3 union /
  class-column / streaming-cursor substrate.
- Close documents must state `N-direct / NoGo`, measured fixpoint, and
  grammar-generalization BLOCKED. Any direct `GO`, non-JSON success, or
  parse-only SOTA statement would be a paper close.

## Verification Commands For Redress

W9 redress should run:

```text
git diff --exit-code -- skinny/RESULTS.md
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory
git diff --check
```

## Research Verdict

Proceed to W9 plan. The selected close shape should be:

- Close SK-V11 as converged measured fixpoint under REDRESS 120.
- Preserve overall `N-direct / NoGo`.
- Carry the non-JSON generated-intervention axis as `BLOCKED` into Pass Alpha.
- Trigger G-Alpha SK-V11 -> SK-V12 only as a next-tranche feedback packet, not
  as a success declaration for direct `GO` or grammar generalization.
