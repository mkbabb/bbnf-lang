# SK-V12 W1b-2b CHALLENGE V3 - Consolidated Disposition

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 lightningcss SOTA report gate.
Plan under review: `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.

## Disposition

ACCEPT.

PLAN-V3 fixes the CHALLENGE V2 blockers and is redressable. The redress agent
may implement only the PLAN-V3 owner slice and must preserve the two-command
evidence protocol.

## Lens Results

- CH1 correctness: ACCEPT.
- CH2 generality / Lock 14: ACCEPT.
- CH3 regression / REDRESS: ACCEPT.
- CH4 cost / budget: ACCEPT.
- CH5 hidden coupling: ACCEPT.
- CH6 anti-paper-close: ACCEPT.

## Accepted Corrections

1. CSS and JSON Criterion authorities are split. CSS SOTA validation consumes
   the CSS `nonjson_css_l4` Criterion root, while JSON guard/stale checking runs
   separately with `/tmp/skv12-w1a-json-guard-criterion` and no CSS report flag.

2. The report schema carries the SPEC Section 0.4 CSS telemetry required for
   W1b-2b, including cssparser oracle source, lightningcss command, measured
   validation artifact, and profile artifact.

3. Retained W1b fact/equality artifacts are consumed as files. The gate must
   verify fact-stream byte equality, SHA-256, row id, plane, input FNV, input
   byte count, stream FNV, equality `status=pass`, and the explicitly accepted
   retained W1b run id.

4. The lightningcss measurement lane remains isolated from cssparser oracle
   parsing. Redress records a focused source audit for `lightningcss_facts`.

5. W1b-2b is a companion-gate row disposition. It records REDRESS-125 and the
   CSS SOTA report, but does not move `skinny/RESULTS.md`; W5 owns close
   reconciliation.

6. The W1b-2b source budget correction to `<=330 report/gate/test LOC` is
   accepted as a budget estimate correction only. It does not expand owner paths
   beyond PLAN-V3 and does not change the 30-minute redress cap.

## Redress Conditions

- Implement `sk-v12-css-l4-sota-v1` validation and
  `--skv12-css-l4-sota-report <path>` in the existing gate path so Lock 14
  validation still runs first.
- Recompute Track 1, cssparser, lightningcss, threshold, margin, and sample
  count from live Criterion `new/` artifacts. Report-only Mbps cannot admit.
- Reject mixed companion reports, write/update flags, volatile probes, missing
  paths, flag-as-path values, and unrelated args.
- Keep `skinny/RESULTS.md` byte-identical in W1b-2b, including on
  `PASS-ADMIT-CANDIDATE`.
- Record `PASS-ADMIT-CANDIDATE`, `PASS-MEASURED-BASELINE`, or `BLOCKED/FAIL`
  in REDRESS-125 without claiming final SK-V12 campaign close.
- Save `/tmp/skv12-waveW1b-2b-rejected.patch` if source was attempted and the
  redress fails.

## Route

Proceed to W1b-2b redress. Any implementation that expands into RESULTS
rendering, broad benchmark harness work, generic artifact infrastructure,
directive/BIR/`BackendShape`/substrate surfaces, or SIMD/ASM admission returns
to plan before editing.
