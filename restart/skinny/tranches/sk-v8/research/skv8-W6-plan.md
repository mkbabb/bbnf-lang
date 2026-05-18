# SK-V8 W6 Plan - Close And Alpha Feedback

Date: 2026-05-18.

Status: active plan; W6 is not closed until hardening reaches two consecutive
qualifying ACCEPT cycles and the close disposition is folded into
`restart/skinny/tranches/sk-v8/HANDOFF.md`.

## Entry Gate

W6 may proceed because W0-W5 have recorded dispositions:

- W0 closed by V11+V12 challenge convergence.
- W1 closed by CostFacts gate binding.
- W2 closed by V4+V5 challenge convergence; source/product parity admitted and
  benchmark row-table admission rejected/routed in REDRESS 91.
- W3 rejected/routed by V1 challenge on the pre-redress fit gate in REDRESS 92.
- W4 rejected/routed by V3+V4 challenge convergence in REDRESS 93.
- W5 closed by V4+V5 challenge convergence; only the named Lock 14
  provider-boundary cleanup is admitted.

## Scope

Owner paths:

- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-reconciliation-research.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-plan.md`
- W6 hardening artifacts under
  `restart/skinny/tranches/sk-v8/research/wave-6-hardening/`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md` after hardening accepts close

Conditional owner paths:

- `skinny/REDRESS.md` only if hardening finds a real close mismatch.
- `skinny/RESULTS.md` only if W6 must reconcile a documented row/report
  mismatch without changing source behavior.

Out of scope:

- parser, scanner, runtime, codegen, IR, SIMD, generated output, benchmark row
  refresh, and new profile rows;
- any new directive, BIR variant, substrate surface, sidecar, or Lock 1/Lock 14
  amendment;
- SK-V9 implementation dispatch.

## Falsifiability Gate

W6 passes only if all of the following are true:

1. Every SK-V8 wave has an admitted, rejected, or routed status in
   `restart/skinny/tranches/sk-v8/HANDOFF.md`.
2. The `skinny/RESULTS.md` manifest still contains 38 W0 telemetry rows and
   four measured `real_typed_struct A / GO` rows.
3. W2's Apache/CITM typed source rows are not represented as measured
   `skinny/RESULTS.md` rows; `canada/real_typed_struct` remains rejected for
   W2.
4. W3 and W4 target statuses agree across `skinny/REDRESS.md`,
   `skinny/RESULTS.md`, and `HANDOFF.md`: both are routed/rejected with no
   row-table admission.
5. W5 is described only as a named Lock 14 provider-boundary cleanup with no
   generated-output, row-table, or performance claim.
6. SC-6-L1-R1 is explicitly routed to Pass Omega because SK-V8 neither ratified
   it nor proved it under Lock 1 as written.
7. No accepted source change lacks the required same-wave consumer evidence:
   W1 CostFacts gate consumer, W2 source/product parity tests and report-gate
   fold, W5 Lock 14 audit/codegen/runtime checks, and W0 telemetry gate.
8. Repository-local document paths referenced by the W6 close packet resolve to
   files or directories, except for explicitly external evidence paths such as
   `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`.

## Verification Commands

From the repository root:

```text
git diff --exit-code HEAD -- skinny/RESULTS.md skinny/REDRESS.md
awk 'BEGIN{manifest=0;rt=0;rows=0} /^## SK-V8 W0 Telemetry Manifest/{manifest=1;next} manifest && /^## /{manifest=0} manifest && /^\| json\//{rows++; if ($0 ~ /real_typed_struct/) rt++} END{print "manifest_rows=" rows; print "real_typed_rows=" rt}' skinny/RESULTS.md
test -f /tmp/skv8-wave4-track2-scalar-fold-rejected.patch
cargo xtask regen --check
```

From `skinny/`:

```text
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
```

W6 does not run a performance refresh. If hardening discovers a row/report
mismatch, stop and fold the mismatch through a W6 REDRESS/RESULTS plan before
attempting close.

## Hardening Plan

Dispatch six W6 challengers against the research, plan, close artifact, and
HANDOFF fold:

- CH1: citation and path-resolution review.
- CH2: `RESULTS.md`/`REDRESS.md`/`HANDOFF.md` ledger consistency review.
- CH3: accepted-source proof review for W0, W1, W2, and W5.
- CH4: rejected/routed behavior-wave review for W2 benchmark rows, W3, and W4.
- CH5: Lock 14/Lock 15 and grammar-neutral close review.
- CH6: alpha-feedback and no-SK-V9-dispatch review.

Close only after >=95% ACCEPT in two consecutive cycles. If V1 returns REVISE,
fold the exact dispositions into V2 and continue. Each cycle gets its own
commit.

## Revert And Redress

There is no source revert in W6 by default. If close evidence mismatches:

- update `skinny/REDRESS.md` only when the mismatch is a real route/rejection
  record missing from the ledger;
- update `skinny/RESULTS.md` only for a documented row/report correction that
  does not change source behavior;
- otherwise reopen the producing wave or route the mismatch to SK-V9/Pass
  Omega with exact file paths and row ids.
