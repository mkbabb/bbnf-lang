# SK-V13 Wave 7 Redress - Decision CSP Cascade

Gate: `G-W7-DECISION-CSP-CASCADE`
Disposition: `PASS-BLOCKED`

W7 imports the skinny `csp-solver` dependency into `passes`, adds a bounded
`passes::decision_csp` finalizer after W6 active-cost selection, and carries
the resulting CSP facts into `CostFacts`. `codegen::lower::rust::lower_to_rust`
now fails closed when backend shape, active-cost, or CSP facts are missing or
inconsistent, so the old silent fallback cascade is no longer an admission path.

The material differential from REDRESS 119/120/136/137 is that W7 is the first
decision-engine wave to make the CSP resolver a compile-time consumer and a
gate-checked fact. P1-P8 priority labels, `hard_pruned`, and `shape_rank` are
recorded as evidence only; they cannot prune the CSP domain, drive the
objective, or admit a row.

The production row movement remains architecturally blocked in this wave:
`JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`. The CSP solution is SAT
and reaches compile/lowering, but the generated JSON/CSS runtime providers are
still static-template/sink-only consumers and no hash-checked generated-runtime
diff exists. The W7 gate therefore accepts only the measured block and rejects
`pass`/`admitted` without a generated runtime diff artifact.

Retained artifacts:

- `restart/skinny/tranches/sk-v13/research/w7/csp-problem.json`
  SHA-256 `85289658887456a4d69bae6cc14b6794c194196a3125413b5012f348a75fed85`.
- `restart/skinny/tranches/sk-v13/research/w7/csp-solution.json`
  SHA-256 `147dad980a3068afab2c53030608dc4eb1719f1998972fbda5f97622265a2f72`.
- `restart/skinny/tranches/sk-v13/research/w7/css-l4-witness.json`
  SHA-256 `f2abe4d09b0fd8ad00c0e6b598f952f9eb36d0b56c691b3eea054046151d09ec`.
- `restart/skinny/tranches/sk-v13/research/w7/sheets-witness.json`
  SHA-256 `0c2677dbf3878eb25464d0408374b924a9c664ad4ef6b424025235045820cc29`.
- `restart/skinny/tranches/sk-v13/research/w7/bbnf-self-witness.json`
  SHA-256 `18929f980e4690e39148d83f4a611e57335ac3fde06b87bcaaffcb8108d949a2`.
- Gate report:
  `restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json`.

Verification:

- `cargo test -p passes decision_csp -- --nocapture`
- `cargo test -p passes -- --nocapture`
- `cargo test -p codegen bare_emit_fails_closed_without_pass_facts -- --nocapture`
- `cargo test -p codegen -- --nocapture`
- `cargo test -p bbnf-bench skv13_decision_csp_cascade_report -- --nocapture`
- `cargo test -p bbnf-bench --bin gate skv13_decision_csp_cascade_report -- --nocapture`
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_csp_cascade_report_flag -- --nocapture`
- `cargo test -p bbnf-bench w7 -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-decision-regex-report ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json --skv13-decision-active-cost-report ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json --skv13-decision-csp-cascade-report ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json`
