# SK-V8 W0 Hardening V4 - CH2

## Verdict

ACCEPT, confidence 96%.

Reviewed target: `077aadad8aacf95e3250ec157f30ba6ab873bf6b`
(`fix(sk-v8-wave0): fold hardening V3 gate blockers`).

## Scope

CH2 GENERALITY adversarial review of W0 after the V3 rejection/fold. I focused
on comparator-id allowlisting, native strict-only comparator admission,
`sidecar-same-run` rejection, `sonic_rs_lossy` / flaw-probe semantics, and
strict-vs-strict comparator isolation. I read ORCHESTRATOR CH2/CHALLENGE
governance (`restart/prompts/ORCHESTRATOR.md:74-88`,
`restart/prompts/ORCHESTRATOR.md:104-120`), the live packet docs, current
`skinny/RESULTS.md`, and the W0 report/gate implementation. I made no source
edits.

## Evidence

- Packet contract: SPEC admits strict evidence only for same-run strict anchors
  with matching plane and measured-row validation, makes `sonic-rs lossy` a
  flaw probe only, and forbids W0 `sidecar-same-run` without a structured
  manifest (`restart/skinny/tranches/sk-v8/SPEC.md:63-81`). W0 tasks/exit gates
  require sidecar freshness/source validation, malformed sidecar rejection, all
  38 rows populated, parse rows as `S`/hard failure, and `gate-json` consumption
  (`restart/skinny/tranches/sk-v8/SPEC.md:310-337`).
- Comparator validation now runs before strict-admission evaluation
  (`skinny/crates/bbnf-bench/src/report.rs:328-370`). The accepted comparator
  universe is explicit: sidecars are the six C++ ids, native strict ids are only
  `sonic_rs_strict` and `serde_json`, and `sonic_rs_lossy` is the only native
  flaw probe (`skinny/crates/bbnf-bench/src/report.rs:833-842`).
- Unknown comparator ids reject before admission
  (`skinny/crates/bbnf-bench/src/report.rs:981-1056`). Strict admission then
  iterates only populated native strict ids
  (`skinny/crates/bbnf-bench/src/report.rs:920-960`), and native strict
  source/plane/strictness/freshness are workload-exact
  (`skinny/crates/bbnf-bench/src/report.rs:1151-1213`).
- Sidecars are isolated as planning signals: recognized sidecars reject
  `sidecar-same-run` without a manifest and require historical/absent source
  shapes (`skinny/crates/bbnf-bench/src/report.rs:1101-1149`). The generic
  strict-admission primitive also rejects anything except
  `comparator_freshness=same-run-native` plus `sidecar_freshness=n/a`
  (`skinny/crates/bbnf-bench/src/gate.rs:135-175`).
- `sonic_rs_lossy` is emitted only for parse rows as `permissive`,
  `same-run-native`, `sidecar=n/a`
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:496-551`), and validation requires
  parse-only DOM flaw-probe semantics plus the exact `sonic_rs_lossy` Criterion
  source (`skinny/crates/bbnf-bench/src/report.rs:1068-1099`). The committed
  manifest demonstrates the intended shape while keeping sidecars historical or
  absent (`skinny/RESULTS.md:44-54`).
- Executed checks:
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-v4-target cargo test -p bbnf-bench --profile ax-iter`
    passed: 51 library tests, 8 gate-bin tests, 0 failures.
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
    passed and matched committed `skinny/RESULTS.md`.

## Findings

None material.

1. V3 CH2's unknown-comparator paper-admission path is folded. The test
   `w0_rejects_unknown_comparator_strict_admission_shape` now passes, and the
   implementation rejects unsupported ids before strict-admission evaluation.
2. Native strict admission is isolated to `sonic_rs_strict` and `serde_json`.
   Sidecars, unknown ids, and `sonic_rs_lossy` cannot satisfy the strict
   comparator loop.
3. `sidecar-same-run` is rejected both at recognized-sidecar validation and at
   the strict-admission primitive. W0 still has no sidecar manifest parser, which
   matches the packet posture.
4. `sonic_rs_lossy` is treated as a same-run flaw probe, not an anchor. Its
   `permissive` strictness makes it non-admissible for strict-vs-strict claims.

## Required Disposition If Rejected

Not applicable. CH2 accepts V4; no required fold from this lens.

## Residual Risks

- The default local `target/criterion` root failed closed on stale SIMD metadata
  (`twitter SIMD metadata invalid: SIMD metadata has unsupported capture policy`).
  I do not treat that as a CH2 blocker because the committed W0 evidence root
  `/tmp/skv8-w0-target` passes, and the failure mode is rejection rather than
  false admission.
- W0 still has no structured sidecar same-run manifest parser by design. Any
  later attempt to promote C++ sidecars must add a manifest parser and new
  negative tests before using them for strict admission.
