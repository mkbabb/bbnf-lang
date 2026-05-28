# SK-V15 W2-C - Gate Self-Exemption Audit

Scope: read-only audit of `skinny/xtask/src/main.rs`,
`skinny/crates/bbnf-bench/src/bin/gate.rs`, and report validators.

## Skip Paths

`xtask gate-json --check-results` validates `RESULTS.md` and returns without
spawning the bench gate. The bench gate is where Lock 14 runs
(`skinny/xtask/src/main.rs:290`, `skinny/xtask/src/main.rs:293`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:51`). This means the rolling gate
can consume results while skipping Lock 14/16 exclusion reporting.

`--with-cost-facts` diverts into `gate_json_cost_facts`, and direct bench-gate
argument parsing treats `--with-cost-facts` as satisfying companion JSON-check
intent without running xtask's result snapshot validator
(`skinny/xtask/src/main.rs:285`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1046`).

Update/write paths do not imply `--check-results`. `bench-json` can invoke
`gate_json(root, ["--update-results"])`, and W0 result validation only runs
when `--check-results` is present (`skinny/xtask/src/main.rs:270`,
`skinny/xtask/src/main.rs:291`).

Legacy companion reports can pass and return without `--check-results`: W1a,
SK-V12 non-JSON, and SK-V12 CSS L4 SOTA
(`skinny/crates/bbnf-bench/src/bin/gate.rs:56`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:67`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:78`). Because companion flags can be
mixed, an early legacy return can hide later stricter companion flags.

SK-V15 manifest validators currently parse Lock 14 / Lock 16 / exclusion
fields as strings and reject only empty or `self-exempting` substrings. They do
not parse the W2 dispatch schema
(`skinny/crates/bbnf-bench/src/report.rs:3923`,
`skinny/xtask/src/skv15_w0.rs:392`).

## W2 Fail-Closed Predicates

Successful W2 close must make every admission/update/companion path run a
Lock 14/16/exclusion consumer or reject with a clear reason. `--update-results`,
`--write-results`, `--skv14-existing-results-capture`, companion report flags,
and `--with-cost-facts` must not be hidden bypasses.

The gate must reject:

- `diagnostic:pre-W2-incomplete`
- `known-leak-roots`
- blanket Lock 16 `n/a` while source-present primitives exist
- self-exempting exclusion records
- W2 reports lacking included roots, excluded roots, reason, owner,
  self-scan status, primitive status, gate consumer, affected rows, or
  disposition
