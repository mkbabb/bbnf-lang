# SK-V8 W0 Hardening V5 - CH2

## Verdict

ACCEPT, confidence 96%.

Reviewed target: `0c49fabd6d6facd136e1e69b8482aa4f239561ae`
(`fix(sk-v8-wave0): fold hardening V4 gate blockers`).

## Scope

CH2 GENERALITY adversarial review of W0 after the V4 rejection/fold. I focused
on comparator id allowlisting, native strict-only comparator admission,
`sidecar-same-run` rejection, `sonic_rs_lossy` / flaw-probe semantics,
strict-vs-strict comparator isolation, and whether the V4 `parse_utf8` /
`escape_complete` fold weakened comparator gates. I read ORCHESTRATOR CH2 and
convergence governance (`restart/prompts/ORCHESTRATOR.md:81-88`,
`restart/prompts/ORCHESTRATOR.md:104-123`), V4 consolidation
(`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:20-46`),
and the live SK-V8 packet docs. I edited only this artifact.

## Evidence

- Packet contract: strict admission is limited to same-run native strict anchors
  on the matching output plane, with measured-row UTF-8/control/escape
  validation; `sonic-rs lossy` is a flaw probe only; and W0 rejects any
  `sidecar-same-run` claim until a structured sidecar manifest parser exists
  (`restart/skinny/tranches/sk-v8/SPEC.md:63-81`). W0 also requires sidecar
  freshness/source validation, malformed sidecar rejection, all 38 rows
  populated, parse rows as `S` or hard failure, and `gate-json` consumption in
  the same slice (`restart/skinny/tranches/sk-v8/SPEC.md:310-337`).
- Live packet consistency: DISPATCH keeps strict-vs-strict gates and
  sidecar/permissive/lossy planning-only status as non-negotiables
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:186-193`); HANDOFF says all
  current rows are `Strictness=deferred` and W0 admits no structured same-run
  sidecar manifest (`restart/skinny/tranches/sk-v8/HANDOFF.md:40-48`);
  SYNTHESIS records the same deferred/view-boundary row posture and comparator
  classes (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:36-45`,
  `restart/skinny/tranches/sk-v8/SYNTHESIS.md:227-235`).
- Comparator validation executes before admission-boundary validation
  (`skinny/crates/bbnf-bench/src/report.rs:328-370`). The comparator universe is
  closed: C++ sidecars are six named ids, native strict ids are only
  `sonic_rs_strict` and `serde_json`, and `sonic_rs_lossy` is the only native
  flaw probe (`skinny/crates/bbnf-bench/src/report.rs:833-842`).
- Unknown comparator ids reject before the strict-admission loop
  (`skinny/crates/bbnf-bench/src/report.rs:991-1083`). Strict admission then
  iterates only populated native strict comparators
  (`skinny/crates/bbnf-bench/src/report.rs:951-978`), while the generic strict
  gate requires strict row/comparator status, `parse_utf8=measured-row`,
  `escape_complete=yes`, plane equality, `measured_validation_path=measured-row`,
  `comparator_freshness=same-run-native`, and `sidecar_freshness=n/a`
  (`skinny/crates/bbnf-bench/src/gate.rs:135-175`).
- Native strict source validation is workload-exact for parse, direct, and real
  typed rows (`skinny/crates/bbnf-bench/src/report.rs:1169-1230`). Sidecars are
  isolated as historical or absent planning signals and reject
  `sidecar-same-run` without a structured manifest
  (`skinny/crates/bbnf-bench/src/report.rs:1119-1167`).
- `sonic_rs_lossy` is emitted only for parse rows when present, with
  `strictness=permissive`, `freshness=same-run-native`, and `sidecar=n/a`
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:501-580`). Validation requires the
  parse-only DOM flaw-probe shape and the exact `sonic_rs_lossy` Criterion source
  (`skinny/crates/bbnf-bench/src/report.rs:1086-1117`). It is not in the native
  strict comparator allowlist.
- The V4 parse/escape fold tightens non-strict rows instead of weakening strict
  gates: non-strict rows now require `strictness=deferred`,
  `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
  `escape_complete=yes` before returning OK (`skinny/crates/bbnf-bench/src/report.rs:920-948`).
- Executed checks:
  - `cargo test -p bbnf-bench` passed 52 library tests, 8 gate-bin tests, and 0
    doctests.
  - `cargo test -p bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and
    8 gate-bin W0 tests.
  - `cargo test -p bbnf-bench gate::tests -- --nocapture` passed 13 strict/gate
    tests.
  - `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
    xtask gate-json --advisory --check-results` exited 0 and matched committed
    `skinny/RESULTS.md`.

## Findings Ordered By Severity

None material.

1. The V4 comparator allowlist blocker remains folded. Unsupported ids reject
   before strict-admission evaluation, including the unknown sidecar strict shape
   covered by `report::tests::w0_rejects_unknown_comparator_strict_admission_shape`.
2. Native strict admission is isolated to `sonic_rs_strict` and `serde_json`.
   Source artifact, plane, strictness, same-run-native freshness,
   `sidecar_freshness=n/a`, and finite Mbps are all validated per workload.
3. `sidecar-same-run` remains rejected under W0. Recognized C++ sidecars are
   historical or explicitly absent, and the strict-admission primitive rejects
   sidecar freshness as strict evidence.
4. `sonic_rs_lossy` remains a parse-only flaw probe. Its `permissive`
   strictness and exclusion from `SK_V8_NATIVE_STRICT_COMPARATORS` keep it out of
   the strict comparator loop.
5. The V4 `parse_utf8` / `escape_complete` fold did not weaken comparator gates.
   Comparator evidence validation still runs first, strict validation still
   requires measured-row validation, and the new non-strict guard only closes the
   deferred-row semantic drift found in V4.

## Required Disposition If Rejected

Not applicable. CH2 accepts V5; no required fold from this lens.

## Residual Risks

- W0 intentionally has no structured sidecar same-run manifest parser. Any future
  C++ sidecar promotion needs a manifest parser, source/freshness validation, and
  new negative tests before sidecars can participate in strict admission.
- Current W0 rows are still `Strictness=deferred`; this ACCEPT does not admit any
  W1-W6 behavior wave or strict SOTA row. ORCHESTRATOR Section 3Z still requires
  the challenge-cycle convergence rules before W0 can close.
- The synthetic report test helpers exercise comparator negatives but are not a
  replacement for the gate replay. The executable close evidence is the
  `/tmp/skv8-w0-target` `gate-json --advisory --check-results` replay above.
