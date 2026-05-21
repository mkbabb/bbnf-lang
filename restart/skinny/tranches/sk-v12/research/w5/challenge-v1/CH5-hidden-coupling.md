# SK-V12 W5 CHALLENGE CH5: Hidden Coupling

VERDICT: ACCEPT

W5 owner paths are docs/report-only: `skinny/RESULTS.md`,
`skinny/REDRESS.md`, SK-V12 tranche docs, the campaign-close doc, and W5
research. They exclude `skinny/crates/**`, generated runtime, codegen, SIMD,
benchmarks, Cargo manifests, and gate source.

SPEC Section 10 matches this boundary: W5 owns close reconciliation,
`RESULTS.md` / `REDRESS.md`, and campaign-close docs; its revert protocol says
docs/report-only and no behavior patch exists.

The plan avoids hidden gate coupling by using the existing CSS companion report
gate `--skv12-css-l4-sota-report` for CSS admission while explicitly not using
legacy `--check-results` to prove the manually appended CSS row. The JSON floor
AWK guard is keyed only to fixed JSON rows, so the CSS row is not smuggled into
JSON gate semantics.

Required changes: none.
