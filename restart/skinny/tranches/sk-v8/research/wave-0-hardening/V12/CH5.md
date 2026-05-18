# SK-V8 W0 Hardening V12 - CH5 HIDDEN COUPLING

Date: 2026-05-18.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Verdict

ACCEPT.

Confidence: 96%.

V12 is the required unchanged second qualifying CH5 cycle after V11. I found no
parallel substrate, sidecar producer, renamed scanner path, Track 1 / Track 2
dishonesty, parser-owned structural projection/cursor/facts, or telemetry
substitution path. W0 remains telemetry/report-local, and W3 remains blocked.

## Evidence

1. The governance target is the unchanged V11-accepted target. ORCHESTRATOR CH5
   rejects parallel substrate, sidecar producer, renamed-scanner Lock 1 drift,
   and Track 1 / Track 2 dishonesty
   (`restart/prompts/ORCHESTRATOR.md:74`, `restart/prompts/ORCHESTRATOR.md:87`).
   ORCHESTRATOR convergence still requires two consecutive >=95% ACCEPT cycles
   (`restart/prompts/ORCHESTRATOR.md:118`, `restart/prompts/ORCHESTRATOR.md:120`).
   V11 accepted 6/6 and recorded that one more qualifying ACCEPT was required
   before W0 could close
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:10`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:14`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:82`).
   V10 is the reset context, not an unresolved CH5 blocker: V10 consolidated
   rejected the cycle because CH4 found cost/metadata blockers while CH5 had
   accepted no hidden-coupling route
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:10`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:25`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:26`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:64`).
   The CH4 fold requirement was to reduce the post-V6 footprint, tighten
   host/feature metadata, add negative tests, and preserve accepted CostFacts,
   Track 2, substrate tuple, fingerprinting, frozen diff, and rollback evidence
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:94`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:103`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:107`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:111`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:115`).
   `git merge-base --is-ancestor 61d5cc3b4312883e026060174e876a0c18b34703 HEAD`
   exited 0, and `git diff --name-status 61d5cc3b4312883e026060174e876a0c18b34703..HEAD -- skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs restart/skinny/tranches/sk-v8/SPEC.md restart/skinny/tranches/sk-v8/HANDOFF.md restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
   returned no paths.

2. W0 scope remains telemetry/report-local. SPEC gives current dispatch
   authority only to W0 and blocks W1-W6 until W0 closes with fresh owner paths,
   row gates, challenge acceptance, and dispatch
   (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
   `restart/skinny/tranches/sk-v8/SPEC.md:35`,
   `restart/skinny/tranches/sk-v8/SPEC.md:36`). DISPATCH-PROMPT says W0 may
   touch only W0 owner paths, must implement only telemetry/gate validation, and
   must verify no parser, scanner, SIMD, asm, codegen, product-plane, or
   generated parser output behavior change
   (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:63`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:65`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:87`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`). HANDOFF repeats that
   W0 is telemetry-only and rejects parser, scanner, SIMD, asm, codegen, or
   product-plane behavior changes
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:127`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:139`). The target commit
   touched only `skinny/crates/bbnf-bench/src/report.rs`: `git show --stat --oneline
   61d5cc3b4312883e026060174e876a0c18b34703 -- skinny/crates/bbnf-bench/src/report.rs
   skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/gate.rs
   skinny/crates/bbnf-bench/src/lock14_baseline.rs` reports one file changed,
   `58 insertions / 109 deletions`, and `git diff --name-only
   61d5cc3b4312883e026060174e876a0c18b34703^
   61d5cc3b4312883e026060174e876a0c18b34703 -- <same paths>` prints only
   `skinny/crates/bbnf-bench/src/report.rs`.

3. No side substrate or parser-owned structural projection is admitted through
   W0 telemetry. SPEC non-negotiables forbid `UnionTape`, new substrate surface,
   public substrate API, parser-owned structural cursor/facts, parallel or
   sidecar substrate, and non-consumed representations
   (`restart/skinny/tranches/sk-v8/SPEC.md:193`,
   `restart/skinny/tranches/sk-v8/SPEC.md:196`,
   `restart/skinny/tranches/sk-v8/SPEC.md:197`,
   `restart/skinny/tranches/sk-v8/SPEC.md:200`,
   `restart/skinny/tranches/sk-v8/SPEC.md:201`,
   `restart/skinny/tranches/sk-v8/SPEC.md:206`). The current W0 generator emits
   only workload-specific telemetry tuples:
   `parse_only = borrowed_view_over_offset_tape / discarded_after_capacity / one`,
   `direct_to_struct = sink_only_digest / n/a / zero_or_inert`, and
   `real_typed_struct = typed_direct_projection / n/a / zero_or_inert`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:472`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:492`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:603`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:610`). The report validator requires
   exact matching tuples and rejects drift
   (`skinny/crates/bbnf-bench/src/report.rs:1063`,
   `skinny/crates/bbnf-bench/src/report.rs:1074`,
   `skinny/crates/bbnf-bench/src/report.rs:1083`), with a negative test for
   `substrate_surface = side_substrate`
   (`skinny/crates/bbnf-bench/src/report.rs:2053`,
   `skinny/crates/bbnf-bench/src/report.rs:2069`).

4. Sidecar values remain planning signals, not a same-run producer or strict
   anchor. SPEC says W0 has no structured sidecar same-run manifest and any
   `sidecar-same-run` claim rejects
   (`restart/skinny/tranches/sk-v8/SPEC.md:73`,
   `restart/skinny/tranches/sk-v8/SPEC.md:77`,
   `restart/skinny/tranches/sk-v8/SPEC.md:80`,
   `restart/skinny/tranches/sk-v8/SPEC.md:316`,
   `restart/skinny/tranches/sk-v8/SPEC.md:352`,
   `restart/skinny/tranches/sk-v8/SPEC.md:355`). The gate binary emits native
   strict comparators as `same-run-native` with `sidecar_freshness=n/a`, while C++
   sidecar slots are historical or absent with sidecar-profile/absence sources
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:526`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:557`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:565`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:576`). The report validator rejects
   sidecar freshness mismatch and `sidecar-same-run`
   (`skinny/crates/bbnf-bench/src/report.rs:1263`,
   `skinny/crates/bbnf-bench/src/report.rs:1281`,
   `skinny/crates/bbnf-bench/src/report.rs:1287`) and validates expected
   sidecar/absence sources (`skinny/crates/bbnf-bench/src/report.rs:1293`,
   `skinny/crates/bbnf-bench/src/report.rs:1304`).

5. Track 1 / Track 2 independence is still explicit rather than coupled.
   W0 report notes state Track 1 is `runtime::generated_json::parse`, Track 2 is
   the independent hand-coded parser over `runtime::tape`, and Track 2 never
   calls Track 1 (`skinny/crates/bbnf-bench/src/bin/gate.rs:307`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:312`). The W0 manifest must carry
   `track2_independence_status=independent_verified`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:495`,
   `skinny/crates/bbnf-bench/src/report.rs:1007`,
   `skinny/crates/bbnf-bench/src/report.rs:1013`), and the full-baseline
   negative test rejects changing it to `unverified`
   (`skinny/crates/bbnf-bench/src/report.rs:2053`,
   `skinny/crates/bbnf-bench/src/report.rs:2061`).

6. Telemetry consumption does not substitute for W3 production consumption.
   SPEC requires every emitted W0 field to be consumed by `gate-json` and rejects
   producer-only telemetry, W3 side substrate, and W3 telemetry substitution
   (`restart/skinny/tranches/sk-v8/SPEC.md:142`,
   `restart/skinny/tranches/sk-v8/SPEC.md:144`). The W0 same-wave consumer is
   exactly `gate_only` in emission and validation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:495`,
   `skinny/crates/bbnf-bench/src/report.rs:356`). W3 is separately blocked on W0
   and W1 admission, a fresh W3 plan, exact owner files, same-wave production
   consumer, measured-path proof, challenge acceptance, and a Lock 1 fork
   resolution (`restart/skinny/tranches/sk-v8/SPEC.md:525`,
   `restart/skinny/tranches/sk-v8/SPEC.md:527`,
   `restart/skinny/tranches/sk-v8/SPEC.md:528`,
   `restart/skinny/tranches/sk-v8/SPEC.md:535`). W3's only lead hypothesis must
   be representation replacement inside one retained `Tape`, fails beside the
   old offset path or parser-owned cursor/facts, and cannot count telemetry-only
   rows as its consumer (`restart/skinny/tranches/sk-v8/SPEC.md:542`,
   `restart/skinny/tranches/sk-v8/SPEC.md:550`,
   `restart/skinny/tranches/sk-v8/SPEC.md:573`). HANDOFF states the same
   boundary: SC-1..SC-6 authorize no W3 plan by themselves, W3 remains blocked on
   W0/W1 closure and challenge requirements, and `tape_vs_tape` is W0/W1
   telemetry only, not a W3 production consumer
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:95`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:98`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:101`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:236`).

7. Lock 14 still prevents renamed scanner/substrate drift. The baseline marks
   runtime/generated/tape/SIMD/codegen/Track 2 behavior roots read-only or frozen
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:129`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:147`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:195`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:255`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`). Its validation checks
   allowlist uniqueness/mutability, frozen git roots, and `BackendShape` surface,
   and forbids `UnionTape`
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:359`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`). `git diff --name-only
   0bd16f6d..61d5cc3b4312883e026060174e876a0c18b34703 -- <frozen behavior roots>`
   returned no paths.

8. The fresh V12 verification commands pass from the skinny workspace:
   `cargo test -p bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and 8
   gate-bin W0 tests; `cargo test -p bbnf-bench strict -- --nocapture` passed 5
   strict-admission tests; `cargo test -p bbnf-bench sidecar_same_run --
   --nocapture` passed 1 sidecar-same-run test; and
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
   xtask gate-json --advisory --check-results` exited 0 and rendered the W0
   telemetry manifest.

## Blockers

None.

## Required Fold If Rejecting

Not applicable: CH5 ACCEPTs V12. Preserve the V11-accepted shape: W0 stays
telemetry/report-local; sidecar signals stay historical/absent planning
evidence; strict admission remains same-run native strict only; Track 1 / Track 2
independence stays gate-consumed; W3 remains blocked on its own plan/challenge
and production same-wave consumer.

## Residual Risk

- This is a CH5 disposition only. W0 closure still depends on the complete V12
  six-lens consolidation meeting ORCHESTRATOR convergence.
- W1 still owns replacing `none:pre-W1` CostFacts sentinels before any behavior
  wave can cite route quality.
- W3 remains blocked until after W0/W1 admission and a fresh accepted W3 plan;
  W0 `gate_only` telemetry is not a structural-projection production consumer.
