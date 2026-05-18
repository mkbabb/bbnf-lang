# SK-V8 W0 Hardening V10 CH3 - Regression

Date: 2026-05-18.

Target: `3a9fa32675cedb1f8a0d31247df229fe505068be`
(`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

## Verdict

ACCEPT.

Confidence: 95%.

The V9 CH3 blocker is folded. The required W0 manifest fields that were
previously accepted as non-empty producer text now flow through semantic
validation in `validate_sk_v8_w0()`, and the fold adds focused negative tests
that mutate the formerly weak field groups while preserving row identity,
baseline outcome/verdict, throughput, run id, and comparator evidence.

## Evidence

The previous blocker was that CostFacts, redress, substrate, Track 2
independence, and build/run metadata were rendered as gate-consumed telemetry
but only checked for presence. The V10 target still performs presence checks
for required fields, then calls `validate_w0_manifest_semantics(self)` before
comparator and admission validation (`skinny/crates/bbnf-bench/src/report.rs:275`,
`skinny/crates/bbnf-bench/src/report.rs:355`). That helper now rejects any
non-W0 CostFacts sentinel, redress entry, Track 2 independence value,
build/host/feature shape, or workload substrate tuple
(`skinny/crates/bbnf-bench/src/report.rs:1007`,
`skinny/crates/bbnf-bench/src/report.rs:1032`).

The semantic checks cover the exact V9 required fold. CostFacts must be exactly
`none:pre-W1` for rule id, chosen shape, and the single rejected alternative
(`skinny/crates/bbnf-bench/src/report.rs:1009`,
`skinny/crates/bbnf-bench/src/report.rs:1012`). `redress_entry` must be `none`
and `track2_independence_status` must be `independent_verified`
(`skinny/crates/bbnf-bench/src/report.rs:1019`,
`skinny/crates/bbnf-bench/src/report.rs:1025`). Build metadata must carry
`profile=bench`, `rustflags=-C target-cpu=native`, `target_cpu=native`, a
structured host triple with arch/cpu facts, and a feature mask with arch, os,
simd, and native target CPU (`skinny/crates/bbnf-bench/src/report.rs:1036`,
`skinny/crates/bbnf-bench/src/report.rs:1088`). Substrate telemetry is now
workload-bound: `parse_only` requires
`borrowed_view_over_offset_tape / discarded_after_capacity / one`,
`direct_to_struct` requires `sink_only_digest / n/a / zero_or_inert`, and
`real_typed_struct` requires `typed_direct_projection / n/a / zero_or_inert`
(`skinny/crates/bbnf-bench/src/report.rs:1091`,
`skinny/crates/bbnf-bench/src/report.rs:1118`).

The producer side emits those same facts from benchmark metadata/run facts and
the W0 substrate classifier. `gate-json` validates schema plus W0 before
rendering or comparing `skinny/RESULTS.md`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:332`). Run facts and row metadata
feed build flags, host triple, feature mask, sample count/cost, sentinel
CostFacts, redress, run id, substrate tuple, consumer, Track 2 status, and
comparator evidence (`skinny/crates/bbnf-bench/src/bin/gate.rs:383`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:497`). Capture metadata still rejects
missing fields, fixture drift, mixed capture, and SIMD capture/policy drift
before the report is rendered (`skinny/crates/bbnf-bench/src/bin/gate.rs:1075`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1112`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1385`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1443`).

Row identity, report schema, comparator source, and freshness remain consumed.
The report validator enforces exact W0 row count, duplicate rejection, known
baseline row ids, baseline outcome/verdict, and Track 1/Track 2 drift bounds
(`skinny/crates/bbnf-bench/src/report.rs:494`,
`skinny/crates/bbnf-bench/src/report.rs:525`). Row id must match rendered
corpus/workload (`skinny/crates/bbnf-bench/src/report.rs:1149`). Native
comparator source/plane/strictness/freshness are exact, sidecar source and
freshness are exact, and `sidecar-same-run` still rejects without a structured
manifest (`skinny/crates/bbnf-bench/src/report.rs:1168`,
`skinny/crates/bbnf-bench/src/report.rs:1252`,
`skinny/crates/bbnf-bench/src/report.rs:1288`,
`skinny/crates/bbnf-bench/src/report.rs:1399`). The manifest renderer exposes
the now-consumed fields in `skinny/RESULTS.md`
(`skinny/crates/bbnf-bench/src/report.rs:580`,
`skinny/crates/bbnf-bench/src/report.rs:608`), and the checked RESULTS manifest
shows the expected parse/direct/typed substrate tuples and sentinels
(`skinny/RESULTS.md:44`, `skinny/RESULTS.md:50`).

The new negative tests exercise the V9 blocker directly. The exact opening
baseline test first validates a complete W0 report, then mutates CostFacts rule
id, CostFacts rejected alternatives, redress, Track 2 independence, build
flags, host triple, feature mask, and substrate while leaving the rest of the
baseline report intact, and each mutation must fail
(`skinny/crates/bbnf-bench/src/report.rs:2087`,
`skinny/crates/bbnf-bench/src/report.rs:2120`). Existing focused tests also
continue to reject row-id/rendered identity mismatch, native comparator source
mismatch, native comparator semantic/freshness mismatch, sidecar source and
freshness mismatch, and unknown same-run sidecar shapes.

Focused verification passed:

- `cargo test -p bbnf-bench w0_ -- --nocapture` passed 20 W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture` passed 5 strict tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed 1 focused
  sidecar same-run test.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  passed.
- `git diff --check` passed.

## Blockers

None.

## Required Fold

None.
