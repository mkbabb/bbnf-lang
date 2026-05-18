# SK-V8 W0 Hardening V6 CH6

Date: 2026-05-18.

Target: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`
(`fix(sk-v8-wave0): fold hardening V5 row identity blockers`).

## Verdict

ACCEPT.

Confidence: 96%.

This is a CH6 anti-paper-close accept for the V6 fold only. The V5 blockers were
folded into executable report/gate checks: W0 now binds exact opening row
membership, outcome, verdict, and Track 1/Track 2 values; the Criterion
fingerprint is scoped to exact W0 row membership; required telemetry is consumed
by `gate-json`; and stale sidecar or missing measured-row strict evidence fails
closed. This does not close W0 or dispatch W1-W6. V5 was REJECT and reset the
counter (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:61`),
while ORCHESTRATOR requires two consecutive qualifying accept cycles before
advance (`restart/prompts/ORCHESTRATOR.md:118`).

## Reviewed Surfaces

- ORCHESTRATOR CH6 / convergence governance:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:88`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- SK-V8 packet and W0 gate requirements:
  `restart/skinny/tranches/sk-v8/SPEC.md:73`,
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:336`;
  `restart/skinny/tranches/sk-v8/HANDOFF.md:40`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:226`;
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:186`.
- Current evidence surfaces:
  `skinny/RESULTS.md:5`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:85`,
  `skinny/RESULTS.md:141`;
  `skinny/REDRESS.md:43`.
- Prior V5 consolidation:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:14`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:22`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:29`.
- W0 implementation code:
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/gate.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`.

## Commands And Evidence

- `git rev-parse HEAD`: `6c0bc15d44142abf0b965d9daee7070b1f32dd99`.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v6-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_ -- --nocapture`: PASS; 12 report W0 tests and 8 gate-bin W0 tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v6-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench sidecar_same_run -- --nocapture`: PASS; `rejects_sidecar_same_run_without_structured_manifest` passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS against committed `skinny/RESULTS.md`.
- Dynamic V5 CH4 repro check: copied `/tmp/skv8-w0-target/criterion` to `/tmp/skv8-ch6-v6-dyn.g6hDa8/criterion`, injected `/tmp/skv8-ch6-v6-dyn.g6hDa8/criterion/json_canada/sonic_rs_real_typed_struct/new/estimates.json`, then ran `CARGO_TARGET_DIR=/tmp/skv8-ch6-v6-dyn.g6hDa8 RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS. A valid fixture with an unvalidated W0 row input no longer perturbs `run_id`.
- `git diff --check`: PASS.
- `git status --short`: clean before writing this artifact.

## Findings

1. No blocker: V5 CH1 row-identity paper close is folded into gate-consumed
   code. `SkV8OpenBaseline` now stores `row_id`, `outcome_id`, `verdict`, Track
   1, and Track 2 (`skinny/crates/bbnf-bench/src/report.rs:646`), and
   `Report::validate_sk_v8_w0()` rejects count mismatch, duplicate/unknown row
   ids, outcome movement, verdict movement, missing rows, and Track 1/Track 2
   drift beyond +/-1% (`skinny/crates/bbnf-bench/src/report.rs:493`,
   `skinny/crates/bbnf-bench/src/report.rs:511`,
   `skinny/crates/bbnf-bench/src/report.rs:517`,
   `skinny/crates/bbnf-bench/src/report.rs:523`,
   `skinny/crates/bbnf-bench/src/report.rs:526`). The focused test now accepts
   only the exact opening tuple and rejects `twitter/parse_only` `S -> K` plus
   `twitter/direct_to_struct` `N-direct / NO-GO -> A / GO`
   (`skinny/crates/bbnf-bench/src/report.rs:1897`,
   `skinny/crates/bbnf-bench/src/report.rs:1949`,
   `skinny/crates/bbnf-bench/src/report.rs:1958`).

2. No blocker: V5 CH4 row-manifest fingerprinting is folded. The fingerprint
   collector accepts Criterion inputs only when the group's fixture is loaded
   and the derived workload has an exact `SK_V8_OPEN_BASELINE` row
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:733`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:745`). The unit test proves
   `json_probes_*`, `json_unvalidated_future`, and
   `json_canada/sonic_rs_real_typed_struct` are excluded while a true W0 row
   estimate changes the fingerprint
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1770`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1788`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1794`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:1800`). I reproduced the
   valid-fixture/unvalidated-row case through the live `gate-json` path.

3. No blocker: required telemetry is gate-consumed, not just rendered. The W0
   validator checks every required text field, grammar/domain, sample count,
   sample cost, profile artifact, hot leaf, CostFacts rejected alternatives, and
   same-wave consumer class (`skinny/crates/bbnf-bench/src/report.rs:275`,
   `skinny/crates/bbnf-bench/src/report.rs:317`,
   `skinny/crates/bbnf-bench/src/report.rs:322`,
   `skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:343`,
   `skinny/crates/bbnf-bench/src/report.rs:349`,
   `skinny/crates/bbnf-bench/src/report.rs:355`). `gate-json` validates schema
   and W0 semantics before writing or comparing `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`). The committed report has 38
   W0 telemetry manifest rows from `skinny/RESULTS.md:48` through
   `skinny/RESULTS.md:85`, and explicitly states the manifest is gate-consumed
   (`skinny/RESULTS.md:141`).

4. No blocker: stale sidecar and missing measured-validation paths fail closed.
   Non-strict W0 rows must remain `strictness=deferred`,
   `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
   `escape_complete=yes` (`skinny/crates/bbnf-bench/src/report.rs:1004`,
   `skinny/crates/bbnf-bench/src/report.rs:1008`,
   `skinny/crates/bbnf-bench/src/report.rs:1014`,
   `skinny/crates/bbnf-bench/src/report.rs:1020`,
   `skinny/crates/bbnf-bench/src/report.rs:1026`). Strict admission requires
   measured-row UTF-8, measured-row validation, matching plane, strict comparator
   evidence, same-run native freshness, and `sidecar_freshness=n/a`
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:151`,
   `skinny/crates/bbnf-bench/src/gate.rs:157`,
   `skinny/crates/bbnf-bench/src/gate.rs:160`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). Sidecar comparators must be
   historical or absent, and `sidecar-same-run` rejects without a structured
   manifest (`skinny/crates/bbnf-bench/src/report.rs:1203`,
   `skinny/crates/bbnf-bench/src/report.rs:1221`,
   `skinny/crates/bbnf-bench/src/report.rs:1227`);
   the focused strict-admission test covers the same rule
   (`skinny/crates/bbnf-bench/src/gate.rs:496`).

5. No blocker: downstream dispatch remains locked. SPEC grants current dispatch
   authority for W0 only and blocks W1-W6 until W0 closes plus exact wave gates
   and orchestrator/user dispatch (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
   `restart/skinny/tranches/sk-v8/SPEC.md:36`). DISPATCH-PROMPT repeats that
   W1-W6 require W0 close, fresh plans, owner paths, row gates, challenge where
   required, and dispatch authority (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`). HANDOFF states
   authority is W0 only and W1-W6 remain blocked
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:226`).

## Material Blockers

None found.

## Residual Risks

- This ACCEPT is the first possible qualifying cycle after V5 REJECT, not W0
  closure. W0 still needs another consecutive qualifying accept cycle with zero
  open critical defects before closure can be claimed
  (`restart/prompts/ORCHESTRATOR.md:120`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V5/HARDENING-W0-V5-CONSOLIDATED.md:61`).
- Pre-redress W0 research artifacts still contain stale `K` and old `run_id`
  planning language, for example `wave-0-plan.md`'s
  `run_id=sk-v8-open:<git-sha>:<criterion-root>` and parse-only `K`
  wording (`restart/skinny/tranches/sk-v8/research/wave-0-plan.md:85`,
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md:129`), plus older
  `K/NO-GO` seed rows in `wave-0-results-baseline-research.md`
  (`restart/skinny/tranches/sk-v8/research/wave-0-results-baseline-research.md:92`).
  I do not treat this as a V6 CH6 blocker because the live SPEC/HANDOFF/
  DISPATCH/RESULTS surfaces and executable gate use the current `S`/`L` W0
  posture and `criterion-fnv64` run id.
- `validate_w0_outcome()` still includes internal hard-failure ids `I`, `J`, and
  `M` in its W0 row-level allowlist (`skinny/crates/bbnf-bench/src/report.rs:952`),
  while SPEC Section 0.3 lists the current schema ids without them
  (`restart/skinny/tranches/sk-v8/SPEC.md:83`). Report-level exact-baseline
  validation rejects these for current W0 rows, so this is not a closure blocker,
  but a future schema cleanup should make the row-level allowlist match the live
  packet vocabulary.
