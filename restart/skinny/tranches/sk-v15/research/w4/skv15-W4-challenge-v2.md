# SK-V15 W4 CHALLENGE V2

Input plan: revised `skv15-W4-plan.md` after CHALLENGE V1.

Verdict: ACCEPT 7/7.

All V1 blockers are closed: cost/cap discipline is explicit; owner roots are
the 67 Pattern H include set; no-arg `check-runtime` is the close command;
`--grammar` is diagnostic; the check requires path-set equality,
projection-set validation, output-dir validation, directory-specific headers,
non-writing byte comparison, and dirty-slice staging checks.

Accepted guardrails for redress: no root runtime deletion; no
`LegacyPath`/`LegacySegment` deletion; generated runtime diffs count only if
no-arg `check-runtime` reproduces them; intrinsic-block instead of widening if
the check universe cannot be made fail-closed inside W4 scope and cap.
