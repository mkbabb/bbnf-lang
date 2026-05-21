# SK-V12 W1b-1 CHALLENGE V1 - Consolidated Disposition

Date: 2026-05-20.
Phase: W1b-1 CHALLENGE.
Scope: adversarial review of the W1b-1 CSS L4 generated Track 1 +
independent oracle scaffold plan.

## Disposition

REVISE before redress.

The plan selects the correct CSS L4 row, output plane, scalar-only surface,
generated Track 1 proof shape, independent oracle shape, and no-lightningcss
scaffold boundary. Four lenses accept that direction with hard redress
preconditions. Two lenses identify plan-time blockers that must be repaired
before source redress is legal:

- CH2: `lock14_baseline.rs` is missing from SPEC Section 6 ownership, but
  `gate-json` always runs the Lock 14 validator before consuming a SK-V12
  non-JSON report. W1b-1 therefore cannot legally add the W1b-1 frozen-root /
  parent-diff authorization the mandatory gate needs.
- CH4: the selected implementation plus measurement suite is too broad for the
  pinned <=30 min redress cap and has no hand-LOC slack under the <=360 cap.
  The plan must narrow to a cap-fit implementation slice or explicitly amend
  the SPEC cap before redress.

W1b-1 returns to plan. No source redress is authorized from this V1 challenge.

## Lens Results

| Lens | Verdict | Load-bearing result |
|---|---|---|
| CH1 correctness / generation / equality | ACCEPT | Correct row, output plane, fixture, generated-proof shape, oracle equality, and no W1b-1 lightningcss overclaim. |
| CH2 generality / Lock 14 | REVISE | Add `lock14_baseline.rs` ownership and W1b-1 Lock 14 authorization; keep CSS policy out of generic roots. |
| CH3 regression / REDRESS / JSON guard | ACCEPT WITH HARD REDRESS PRECONDITIONS | Strengthen executable non-JSON gate, record REDRESS 123, keep `RESULTS.md` unchanged, rerun JSON guards. |
| CH4 cost / LOC / generated size / deps | REVISE | Current plan exceeds the <=30 min redress wall and likely underbudgets hand LOC. |
| CH5 hidden coupling / oracle independence | ACCEPT-CONDITIONAL | Generated Track 1, oracle independence, and forbidden-coupling audits must be gate-consumed before PASS. |
| CH6 anti-paper-close | ACCEPT FOR REDRESS, conditional | The scaffold is measurable only if generated proof, retained fact streams, gate consumption, and no SOTA overclaim land. |

## Required Plan V2 Repairs

1. Amend SPEC Section 6 and PLAN ownership to include
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
2. Add a W1b-1-specific Lock 14 redress requirement:
   - authorize only the Section 6 CSS scaffold owner slice;
   - do not widen `runtime/src/tape`, `ir`, `grammar`, `passes`,
     `bbnf-simd`, public substrate APIs, directives, BIR variants, or
     `BackendShape`;
   - permit generic provider registration only, not CSS declaration/fact
     policy in generic roots.
3. Narrow W1b-1 to a cap-fit single redress slice:
   - fixture;
   - CSS-owned provider/profile + generated runtime reproducibility;
   - runtime export;
   - minimal generated Track 1 fact emission;
   - independent `cssparser` oracle/equality;
   - companion report/gate fields needed for this scaffold;
   - retained artifacts and REDRESS.
4. Keep W1b-1 scalar-only. No `bbnf-simd`, aarch64, ASM, `lightningcss`,
   root CSS runtime, JSON runtime reuse, `parse_that_regex`, new outcome
   variant, or main JSON `RESULTS.md` column is legal in this wave.
5. Make the executable gate consume the plan fields before any PASS:
   strictness, grammar/input checksums, input bytes, measured validation path,
   profile artifact, generated LOC, generated module bytes, grammar-size guard,
   Lock 14/16 status, scalar-reference status, parity status, retained Track 1
   facts, retained oracle facts, and JSON guard state.
6. Require `json_guard_state = refreshed:<run-id>:guards-pass` for W1b-1
   because codegen selection, runtime export, report/gate validation, and bench
   dependencies move.
7. If plan V2 still cannot fit the <=30 min cap, record that explicitly and
   amend the SPEC cap before redress rather than overrunning the pinned
   discipline.

## V1 Artifacts

- `CH1-correctness-generation-equality.md`
- `CH2-generality-lock14.md`
- `CH3-regression-redress-json-guard.md`
- `CH4-cost-loc-size-deps.md`
- `CH5-hidden-coupling-oracle-independence.md`
- `CH6-anti-paper-close.md`
