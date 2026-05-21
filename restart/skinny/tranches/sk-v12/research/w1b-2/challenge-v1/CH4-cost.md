# SK-V12 W1b-2 CH4 - Cost

Verdict: REVISE.

Blockers:

- The V1 plan exceeds the <=300 hand/gate budget. It combines a lightningcss
  dependency, AST walk, independent source scanner, three-way equality, fact
  artifacts, new report schema, new gate flag, tests, and REDRESS/report
  updates.
- The planned sample-size-30 evidence is not clean. The existing bench writes
  report values before Criterion runs, using the helper's fixed quick timer.
  Setting `group.sample_size(30)` makes Criterion output compliant but does not
  make the report consume Criterion estimates unless report generation reads
  Criterion artifacts after the benchmark.
- Dependency weight is non-trivial: lightningcss alpha with default features
  disabled still pulls a large dependency set and a second cssparser version.
- The gate/report surface is broader than the plan states: a new companion flag
  needs argument parsing, exclusivity, JSON-check continuation, printed status,
  and tests.
- Commands must run from `skinny/` or use `--manifest-path skinny/Cargo.toml`.

Required revision:

- Split the evidence route into a bounded W1b-2 scope: dependency/compile probe,
  comparator/equality, report/gate, and Criterion artifact ingestion must be
  explicitly costed.
- Update commands to be runnable from the nested skinny workspace.
