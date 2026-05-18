# CH2 GENERALITY / Lock 14 - SK-V8 W0 Hardening V10

Verdict: ACCEPT

Confidence: 96%

Target reviewed: `3a9fa326` (`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

## Scope

CH2 reviewed the V10 fold after V9's telemetry-consumption rejection, with
focus on Lock 14, grammar neutrality, no new directive/BIR/substrate/
`BackendShape`/`UnionTape`, frozen behavior-surface diff, strict-vs-strict
discipline, non-JSON proof, and whether the new telemetry semantic validators
introduced generic JSON policy or behavior movement.

## Evidence

- V9 required exact W0 semantic validation for substrate telemetry by
  workload, CostFacts sentinels, redress/Track 2 status, and build/run metadata
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md`).
  The V10 implementation adds these checks inside the W0 report validator, not
  in grammar/runtime/codegen/substrate code:
  `validate_w0_manifest_semantics()` checks exact `none:pre-W1` CostFacts,
  `redress_entry=none`, `track2_independence_status=independent_verified`,
  build metadata, and substrate tuple dispatch
  (`skinny/crates/bbnf-bench/src/report.rs:1007`-`skinny/crates/bbnf-bench/src/report.rs:1033`).
- The substrate tuple validator is workload-local and exact:
  `parse_only` requires `borrowed_view_over_offset_tape /
  discarded_after_capacity / one`, `direct_to_struct` requires
  `sink_only_digest / n/a / zero_or_inert`, and `real_typed_struct` requires
  `typed_direct_projection / n/a / zero_or_inert`
  (`skinny/crates/bbnf-bench/src/report.rs:1091`-`skinny/crates/bbnf-bench/src/report.rs:1119`).
  This is telemetry classification, not a new substrate surface or generic
  substrate API.
- Build/run metadata is consumed semantically: `build_flags` must contain
  `profile=bench`, `rustflags=-C target-cpu=native`, and
  `target_cpu=native`; `host_triple` must include host plus `arch=` and
  `cpu=`; `feature_mask` must include `arch=`, `os=`, `simd=`, and
  `target_cpu=native`
  (`skinny/crates/bbnf-bench/src/report.rs:1036`-`skinny/crates/bbnf-bench/src/report.rs:1089`).
- The gate binary calls `validate_schema_v3().and_then(|_| validate_sk_v8_w0())`
  before rendering/writing results
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`-`skinny/crates/bbnf-bench/src/bin/gate.rs:327`),
  so the new fields are gate-consumed rather than producer-only text. The
  committed manifest shows the same exact values in `skinny/RESULTS.md`.
- The V8/V9 strictness protections remain intact. W0 rows are still forced to
  `strictness=deferred`, `measured_validation_path=view-boundary`,
  `parse_utf8=view-boundary`, and `escape_complete=yes`
  (`skinny/crates/bbnf-bench/src/report.rs:1121`-`skinny/crates/bbnf-bench/src/report.rs:1147`).
  Helper strict admission still rejects non-GO outcomes before strict evidence
  and still requires measured-row validation, matching output plane,
  same-run-native freshness, and `sidecar_freshness=n/a`
  (`skinny/crates/bbnf-bench/src/gate.rs:135`-`skinny/crates/bbnf-bench/src/gate.rs:175`).
- Comparator validation remains strict-vs-strict by plane. Native strict
  comparators are checked against expected workload-specific Criterion sources
  and planes, flaw probes stay permissive planning evidence, sidecar
  `sidecar-same-run` still rejects without structured manifest, and every
  sidecar slot must be present as historical or absent evidence
  (`skinny/crates/bbnf-bench/src/report.rs:1160`-`skinny/crates/bbnf-bench/src/report.rs:1378`).
- The V10 diff is behavior-frozen for CH2. `git diff --name-status
  00c3485a..HEAD` shows only archived V9 challenge documents plus
  `skinny/crates/bbnf-bench/src/report.rs`; `git diff --name-only
  00c3485a..HEAD -- <frozen behavior roots>` and `git diff --name-only
  0bd16f6d..HEAD -- <frozen behavior roots>` returned no paths.
- No forbidden surface was introduced by the V10 report diff. `git diff
  00c3485a..HEAD -- skinny/crates/bbnf-bench/src/report.rs | rg` for
  `UnionTape`, `union_tape`, `BackendShape`, `BIR`, `directive`,
  `StructuralAlphabet::json`, `JsonPolicy`, `json_policy`, new substrate,
  sidecar substrate, parallel substrate, and generic JSON policy returned no
  matches.
- The new validators are JSON-benchmark/W0-report scoped. The only
  `grammar_id == "json"` check remains in `bbnf-bench` telemetry validation
  (`skinny/crates/bbnf-bench/src/report.rs:322`-`skinny/crates/bbnf-bench/src/report.rs:327`);
  no generic crate gained JSON policy or grammar-name routing.
- Focused mutation negatives cover the V10 field groups while preserving row
  shape: bad CostFacts, bad rejected alternatives, bad redress, bad Track 2
  status, bad build flags, bad host metadata, bad feature mask, and bad
  substrate all fail `validate_sk_v8_w0()`
  (`skinny/crates/bbnf-bench/src/report.rs:2087`-`skinny/crates/bbnf-bench/src/report.rs:2120`).

## Commands

- `git status --short` was clean before writing this report.
- `git diff --shortstat 00c3485a..HEAD` reported `8 files changed, 1074 insertions(+), 13 deletions(-)`.
- `git diff --check 00c3485a..HEAD` returned no whitespace errors.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and
  8 gate-binary W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture` passed 5 focused strict tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed the
  sidecar same-run rejection test.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`
  passed the exact W0 opening-baseline test.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results` passed and validated the
  committed `RESULTS.md` path.

## Blockers

None.

## Required Fold

None. CH2 accepts V10: the telemetry semantic fold closes the V9
producer-only-field blocker without introducing generic JSON policy, behavior
surface movement, directive/BIR/substrate/`BackendShape`/`UnionTape` drift,
strict-vs-strict regression, or non-JSON proof regression.
