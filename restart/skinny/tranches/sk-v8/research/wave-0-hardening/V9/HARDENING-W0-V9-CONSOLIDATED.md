# SK-V8 W0 Hardening V9 Consolidated

Date: 2026-05-18.

Target: `00c3485a8774296e796c2f68b74fd3d559627f0a`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

## Verdict

REJECT.

Accept rate: 5/6 = 83.3%.

Convergence: not achieved. V9 cannot count as the first qualifying ACCEPT cycle
after the V8 reset because CH3 found a material W0 telemetry-consumption
blocker. The consecutive ACCEPT counter remains reset. W1-W6 remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | ACCEPT | 96% | The V8 strict hard-failure route is closed: W0 rows are frozen as deferred/view-boundary and strict admission rejects all non-GO outcomes before comparator evidence. |
| CH2 | ACCEPT | 96% | Lock 14, grammar-neutrality, no-new-surface, strict-vs-strict, and non-JSON proof checks held after the V8 fold. |
| CH3 | REJECT | 91% | Required W0 manifest fields remain under-consumed: several rendered fields are only non-empty checked, despite SPEC requiring `gate-json` to consume every emitted telemetry field in the same wave. |
| CH4 | ACCEPT | 95% | Cost governance, run-id/content fingerprinting, Criterion-root volatility, rollback simulation, and frozen behavior-surface checks held. |
| CH5 | ACCEPT | 95% | Falsifiability and no-paper-close checks held for strictness, comparator discipline, hidden substrate routes, and W1-W6 dispatch blocking. |
| CH6 | ACCEPT | 94% | Integration checks passed; `gate-json --check-results` and focused/full `bbnf-bench` tests were green from the skinny workspace. |

## Required V10 Fold

1. Add exact W0 semantic validation for substrate telemetry by workload:
   `parse_only` must be
   `substrate_surface=borrowed_view_over_offset_tape`,
   `structural_projection_status=discarded_after_capacity`, and
   `substrate_cardinality=one`; `direct_to_struct` must be
   `sink_only_digest / n/a / zero_or_inert`; `real_typed_struct` must be
   `typed_direct_projection / n/a / zero_or_inert`.
2. Add exact W0 sentinel validation for pre-W1 CostFacts:
   `costfacts_rule_id == "none:pre-W1"`,
   `costfacts_chosen_shape == "none:pre-W1"`, and
   `costfacts_rejected_alternative_ids == ["none:pre-W1"]`.
3. Add exact W0 validation for `redress_entry == "none"` and
   `track2_independence_status == "independent_verified"`.
4. Constrain build/run metadata enough to make it gate-consumed, not
   producer-only text: require `profile=bench`, `rustflags=-C target-cpu=native`,
   `target_cpu=native`, a non-empty host triple plus architecture, and a feature
   mask carrying `arch=`, `os=`, `simd=`, and `target_cpu=native`.
5. Add focused negative tests that mutate each field group above while
   preserving row id, outcome/verdict, throughput, run id, and comparator
   evidence, and assert `validate_sk_v8_w0()` fails.
6. Preserve the accepted V9 fixes and prior accepted evidence: W0 rows stay
   `strictness=deferred`, `measured_validation_path=view-boundary`,
   `parse_utf8=view-boundary`, `escape_complete=yes`; hard-failure/non-GO
   outcomes remain strict-admission ineligible; exact run id, row identity,
   outcome/verdict/throughput baseline, row-manifest Criterion filtering,
   sidecar freshness, Lock 14, frozen behavior-surface diff, cost accounting,
   and rollback protocol remain intact.

## Evidence To Rerun After Fold

- `cargo test -p bbnf-bench w0_ -- --nocapture`
- `cargo test -p bbnf-bench strict -- --nocapture`
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`
- `cargo test -p bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- dynamic Criterion mutation probes for future rows, valid-fixture/unvalidated
  rows, and run-id drift
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- frozen behavior-surface diff
- `git diff --check`

## Governance

V9 rejection preserves the reset consecutive ACCEPT counter. After the V10
fold, W0 must receive two consecutive challenge cycles at >=95% ACCEPT, with
zero critical defects and no unresolved REVISE, before W0 can close and W1-W6
can dispatch.
