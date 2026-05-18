# SK-V8 W0 Hardening V3 - CH2

Verdict: REJECT.

Confidence: 88%.

Reviewed commit: `61d5d30407d96ed176cc59e410f7884e30ed30ba`
(`fix(sk-v8-wave0): fold hardening V2 gate blockers`).

Scope: CH2 adversarial review of native and sidecar comparator evidence
semantics after the V2 fold. Lens: strict-vs-strict, exact
`source_artifact`, `comparator_freshness` / `sidecar_freshness`,
same-run-native, sidecar absence/populated shapes, finite Mbps, and no
sidecar-same-run paper close. I did not edit source.

## Blocker

1. **Strict admission is not bounded to an accepted comparator-id set before it
   evaluates comparator freshness.**

   The packet requires comparator evidence to be gate-consumed, with strict
   admission failing closed on strictness, freshness, and measured-path
   mismatch (`restart/skinny/tranches/sk-v8/SPEC.md:73`,
   `restart/skinny/tranches/sk-v8/SPEC.md:140`, `restart/skinny/tranches/sk-v8/SPEC.md:425`),
   and the W0 research explicitly says populated comparator cells need known
   comparator ids and unknown ids must reject
   (`restart/skinny/tranches/sk-v8/research/wave-0-telemetry-gate-research.md:156`).
   ORCHESTRATOR also keeps strict-vs-strict comparator evidence as a hard
   gate (`restart/prompts/ORCHESTRATOR.md:208`).

   The current report validator calls `validate_w0_admission_boundary` before
   `validate_comparator_evidence` (`skinny/crates/bbnf-bench/src/report.rs:369`,
   `skinny/crates/bbnf-bench/src/report.rs:370`). That admission pass iterates
   every populated comparator and returns success on the first comparator whose
   fields satisfy `gate::validate_strict_admission`
   (`skinny/crates/bbnf-bench/src/report.rs:931`,
   `skinny/crates/bbnf-bench/src/report.rs:950`). But
   `StrictAdmissionEvidence` carries no `comparator_id`
   (`skinny/crates/bbnf-bench/src/gate.rs:58`), and the strict validator accepts
   any evidence with `comparator_freshness=same-run-native` or
   `sidecar_freshness=sidecar-same-run` after the other field checks pass
   (`skinny/crates/bbnf-bench/src/gate.rs:172`,
   `skinny/crates/bbnf-bench/src/gate.rs:173`,
   `skinny/crates/bbnf-bench/src/gate.rs:177`).

   The later comparator validator only treats ids in
   `SK_V8_SIDECAR_COMPARATORS` as sidecars
   (`skinny/crates/bbnf-bench/src/report.rs:833`,
   `skinny/crates/bbnf-bench/src/report.rs:1017`) and only validates native
   exactness for `sonic_rs_strict` and `serde_json`
   (`skinny/crates/bbnf-bench/src/report.rs:1049`,
   `skinny/crates/bbnf-bench/src/report.rs:1050`). It requires the six sidecar
   slots to be present (`skinny/crates/bbnf-bench/src/report.rs:1051`,
   `skinny/crates/bbnf-bench/src/report.rs:1053`) but does not reject extra or
   unrecognized comparator ids. Its no-paper-close sidecar-same-run rejection is
   scoped to recognized sidecar ids only
   (`skinny/crates/bbnf-bench/src/report.rs:1083`,
   `skinny/crates/bbnf-bench/src/report.rs:1085`). Therefore an extra populated
   comparator id can carry arbitrary `source_artifact` plus
   `sidecar_freshness=sidecar-same-run` and satisfy the strict-admission
   freshness predicate before any id/source allowlist is applied. That violates
   the packet's unknown-id rejection and leaves a sidecar-same-run paper-close
   shape in the gate, even though the current generated manifest does not emit
   that shape.

## Folded Evidence

The specific V2 CH2 blockers for known native strict comparators are materially
folded. The producer emits workload-specific native evidence for
`sonic_rs_strict` and `serde_json`, with `comparator_strictness=strict`,
`comparator_freshness=same-run-native`, `sidecar_freshness=n/a`, finite Mbps,
and workload-specific Criterion source artifacts
(`skinny/crates/bbnf-bench/src/bin/gate.rs:496`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:516`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:525`). The validator now enforces the
expected plane, strictness, freshness, sidecar freshness, present Mbps, and exact
`criterion:json_{corpus}/{bench}/new/estimates.json` source for those two native
comparators (`skinny/crates/bbnf-bench/src/report.rs:1120`,
`skinny/crates/bbnf-bench/src/report.rs:1133`,
`skinny/crates/bbnf-bench/src/report.rs:1139`,
`skinny/crates/bbnf-bench/src/report.rs:1145`,
`skinny/crates/bbnf-bench/src/report.rs:1151`,
`skinny/crates/bbnf-bench/src/report.rs:1157`,
`skinny/crates/bbnf-bench/src/report.rs:1163`).

Known sidecar populated/absent shapes are also folded. The producer emits
historical populated sidecars as
`sidecar-profile:sk-v7-cpp:{corpus}:{id}` and absent sidecars as
`absence:w0:{corpus}:{workload}:{id}`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:546`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:554`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:561`). The validator requires DOM
sidecar plane, strict sidecar comparator label, matching comparator/sidecar
freshness, rejects recognized `sidecar-same-run` without a structured manifest,
and binds populated/absent source artifacts to the exact corpus/workload shape
(`skinny/crates/bbnf-bench/src/report.rs:1059`,
`skinny/crates/bbnf-bench/src/report.rs:1065`,
`skinny/crates/bbnf-bench/src/report.rs:1071`,
`skinny/crates/bbnf-bench/src/report.rs:1077`,
`skinny/crates/bbnf-bench/src/report.rs:1083`,
`skinny/crates/bbnf-bench/src/report.rs:1089`,
`skinny/crates/bbnf-bench/src/report.rs:1100`).

Finite comparator Mbps is checked for all populated comparator evidence
(`skinny/crates/bbnf-bench/src/report.rs:1009`,
`skinny/crates/bbnf-bench/src/report.rs:1010`), and the native strict comparators
cannot omit Mbps (`skinny/crates/bbnf-bench/src/report.rs:1157`). The committed
manifest demonstrates the intended good shapes for parse, direct, and typed rows
at `skinny/RESULTS.md:48`, `skinny/RESULTS.md:49`, and `skinny/RESULTS.md:50`.

## Commands Run

- `cargo test -p bbnf-bench w0_rejects_native_comparator_semantic_mismatch --profile ax-iter`
  - PASS: 1 test passed.
- `cargo test -p bbnf-bench w0_rejects_sidecar_source_and_freshness_mismatch --profile ax-iter`
  - PASS: 1 test passed.
- `cargo test -p bbnf-bench rejects_strict_plane_mismatch_and_stale_sidecar --profile ax-iter`
  - PASS: 1 test passed.
- `cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline --profile ax-iter`
  - PASS: 1 test passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  - PASS: exited 0 and regenerated the checked report without modifying the worktree.

These commands prove the intended happy path and the new focused negative tests
pass. They do not cover the remaining unrecognized-comparator strict-admission
paper-close shape.

## Required Fold

Reject W0 until the comparator validator either:

1. rejects any comparator id outside the accepted W0 set before strict admission
   runs, or
2. carries `comparator_id` into `StrictAdmissionEvidence` and refuses strict
   admission unless the id is an accepted strict native anchor or a structured
   manifest-backed sidecar same-run anchor.

Add a negative test that constructs a strict/measured row with an extra
unrecognized populated comparator carrying `sidecar_freshness=sidecar-same-run`
and proves `Report::validate_sk_v8_w0` rejects it before W0 can close.
