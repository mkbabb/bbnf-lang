# SK-V14 W1B: Per-Iteration Equality

Date: 2026-05-24.
Scope: W1 R2 equality oracle inside the measured timing region.
Output: this file.

## §1 — Findings

- `restart/skinny/tranches/sk-v14/SPEC.md:414-417` requires equality verification inside the timing region per iteration; startup-only checksum parity is explicitly insufficient.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:15-26` asserts parse/direct/typed parity before `run_fixture`, not inside the Criterion closures.
- Timed JSON closures at `json_parity.rs:43-47`, `65-69`, `181-185`, `203-207`, `261-268`, and `286-293` only produce values and black-box them.
- `skinny/crates/bbnf-bench/src/report.rs:121-124` carries `per_iter_equality`, but the validators in `report.rs:3555` and `skinny/xtask/src/main.rs:490` only require non-empty text.
- Admission markers in `skinny/crates/bbnf-bench/src/bin/gate.rs:3375`, `3594`, and `3656` set `measured_validation_path = "measured-row"` without setting a structured equality pass.
- `skinny/crates/bbnf-bench/src/gate.rs:58` has no `StrictAdmissionEvidence` field for per-iteration equality.

## §2 — Recommendations

- Add a small equality helper in `bbnf-bench` that is constructed before a Criterion closure and invoked inside the measured closure.
- For W1, the helper should compare each timed Track output to a precomputed same-run strict reference digest/checksum and record `checks` and `mismatches`.
- The manifest value should be structured, for example `PASS:scope=criterion-timing;oracle=<id>;plane=<plane>;bench=<criterion-bench>;checks=<n>;mismatches=0`.
- Gate validation should reject empty, `legacy:*`, `not_admitted:*`, `startup:*`, `posthoc:*`, missing `scope=criterion-timing`, zero checks, or non-zero mismatches for any moved/admitted W1 row.

## §3 — Risks

- Running the full strict comparator inside each Track timing closure would contaminate throughput; W1 should compare to a precomputed same-run reference while still checking inside the measured closure.
- Criterion does not expose a simple total iteration count through `b.iter`; do not overclaim exact iteration totals. A lower-bound check such as `checks >= sample_count` is defensible.
- Parse_only equality must compare parse_only facts, not DOM values.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:414-417`
- `skinny/crates/bbnf-bench/benches/json_parity.rs:15-310`
- `skinny/crates/bbnf-bench/src/report.rs:121-124`
- `skinny/crates/bbnf-bench/src/report.rs:3555`
- `skinny/xtask/src/main.rs:490`
- `skinny/crates/bbnf-bench/src/bin/gate.rs:3375-3675`
- `skinny/crates/bbnf-bench/src/gate.rs:58`
