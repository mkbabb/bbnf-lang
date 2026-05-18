# SK-V8 W0 Hardening V9 CH3 - Regression

Date: 2026-05-18.

Target: `00c3485a8774296e796c2f68b74fd3d559627f0a`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

## Verdict

REJECT.

Confidence: 91%.

The V9 fold closes the specific V8 hard-failure strict-admission blocker, but
W0 still has a material paper-closure risk in the CH3 lens area: several
required SK-V8 telemetry fields are rendered and advertised as gate-consumed,
yet `validate_sk_v8_w0()` only checks them for non-empty text, or only checks
the list is non-empty. That does not satisfy SPEC Section 0.4's same-wave
consumption contract and leaves telemetry-substitution paths open.

## Evidence

Required W0 telemetry is not optional prose. SPEC Section 0.4 allows the
required fields to be rendered in `skinny/RESULTS.md`, but says they "must be
consumed by `gate-json` in the same wave" (`restart/skinny/tranches/sk-v8/SPEC.md:105`,
`restart/skinny/tranches/sk-v8/SPEC.md:108`). The required list includes build,
host, feature, CostFacts, redress, substrate, structural-projection,
cardinality, consumer, and Track 2 independence fields
(`restart/skinny/tranches/sk-v8/SPEC.md:124`,
`restart/skinny/tranches/sk-v8/SPEC.md:139`). The same section says every
emitted field must be consumed, and malformed producer-only telemetry, W3 side
substrate, or telemetry substitution rejects the wave
(`restart/skinny/tranches/sk-v8/SPEC.md:142`,
`restart/skinny/tranches/sk-v8/SPEC.md:146`). SPEC Section 3 repeats the W0
exit promise: "`gate-json` consumes every emitted telemetry field and rejects
malformed/missing evidence in the same W0 slice"
(`restart/skinny/tranches/sk-v8/SPEC.md:360`,
`restart/skinny/tranches/sk-v8/SPEC.md:361`).

The current validator does consume row identity, baseline outcome/verdict,
throughput drift, run id, profile artifact, hot leaf, comparator ids,
comparator source paths, comparator planes, strictness, and freshness. Evidence:
`validate_sk_v8_w0()` enforces exact row count and known row ids
(`skinny/crates/bbnf-bench/src/report.rs:499`,
`skinny/crates/bbnf-bench/src/report.rs:515`), baseline outcome/verdict and
Track 1/Track 2 drift (`skinny/crates/bbnf-bench/src/report.rs:517`,
`skinny/crates/bbnf-bench/src/report.rs:530`), exact W0 run id
(`skinny/crates/bbnf-bench/src/report.rs:336`), profile/hot-leaf binding
(`skinny/crates/bbnf-bench/src/report.rs:349`,
`skinny/crates/bbnf-bench/src/report.rs:354`), row identity
(`skinny/crates/bbnf-bench/src/report.rs:1040`,
`skinny/crates/bbnf-bench/src/report.rs:1048`), native comparator semantics and
source paths (`skinny/crates/bbnf-bench/src/report.rs:1229`,
`skinny/crates/bbnf-bench/src/report.rs:1290`), sidecar freshness/source rules
(`skinny/crates/bbnf-bench/src/report.rs:1179`,
`skinny/crates/bbnf-bench/src/report.rs:1227`), and W0 deferred/view-boundary
admission boundaries (`skinny/crates/bbnf-bench/src/report.rs:1012`,
`skinny/crates/bbnf-bench/src/report.rs:1038`). The V8 required fold is also
closed at the helper level: `validate_strict_admission()` now rejects any
outcome whose verdict is not `GO` before comparator evidence is considered
(`skinny/crates/bbnf-bench/src/gate.rs:135`,
`skinny/crates/bbnf-bench/src/gate.rs:144`), and focused tests cover all non-GO
strict-admission outcomes (`skinny/crates/bbnf-bench/src/gate.rs:460`,
`skinny/crates/bbnf-bench/src/gate.rs:482`) plus the canada `L / NO-GO` strict
relabel (`skinny/crates/bbnf-bench/src/report.rs:1954`,
`skinny/crates/bbnf-bench/src/report.rs:1964`).

The blocker is the remaining manifest-field consumption gap. The required-text
loop checks `build_flags`, `host_triple`, `feature_mask`, `costfacts_rule_id`,
`costfacts_chosen_shape`, `redress_entry`, `substrate_surface`,
`structural_projection_status`, `substrate_cardinality`, and
`track2_independence_status` only for non-empty strings
(`skinny/crates/bbnf-bench/src/report.rs:277`,
`skinny/crates/bbnf-bench/src/report.rs:320`). The only additional CostFacts
check is that `costfacts_rejected_alternative_ids` is non-empty
(`skinny/crates/bbnf-bench/src/report.rs:355`,
`skinny/crates/bbnf-bench/src/report.rs:359`). The only exact non-empty
consumer check in this group is `same_wave_consumer_class == "gate_only"`
(`skinny/crates/bbnf-bench/src/report.rs:361`,
`skinny/crates/bbnf-bench/src/report.rs:365`). The rendered manifest then
presents those same fields as authoritative W0 telemetry
(`skinny/crates/bbnf-bench/src/report.rs:581`,
`skinny/crates/bbnf-bench/src/report.rs:614`), and `skinny/RESULTS.md` states
"gate-json consumes the manifest below" (`skinny/RESULTS.md:141`).

This is not just citation hygiene. The gate binary emits meaningful substrate
facts per workload: parse rows should be
`borrowed_view_over_offset_tape / discarded_after_capacity / one`, direct rows
`sink_only_digest / n/a / zero_or_inert`, and real-typed rows
`typed_direct_projection / n/a / zero_or_inert`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:603`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:613`). The report validator never
recomputes or checks that tuple against the row's workload/output plane. A
future or accidental W0 report path could substitute a side substrate,
structural projection, cardinality, CostFacts sentinel, redress marker, or
Track 2 independence claim while still satisfying the current non-empty tests.
That is the paper-close shape Section 0.4 explicitly forbids.

No admitted-row throughput regression or behavior-surface drift was found. The
V9 fold touches only `skinny/crates/bbnf-bench/src/gate.rs` and
`skinny/crates/bbnf-bench/src/report.rs`. The frozen behavior-surface diff from
`0bd16f6d..HEAD` over grammar input, runtime JSON/tape, SIMD, codegen,
generated/product helpers, Track 2, parity, scan, materialization, and the SIMD
scan hook is empty. Focused tests passed:

- `CARGO_TARGET_DIR=/tmp/skv8-v9-ch3-target cargo test -p bbnf-bench w0_ -- --nocapture`
  passed 20 W0 tests.
- `CARGO_TARGET_DIR=/tmp/skv8-v9-ch3-target cargo test -p bbnf-bench strict -- --nocapture`
  passed 5 strict tests.

## Blockers

1. W0 manifest fields remain under-consumed: `build_flags`, `host_triple`,
   `feature_mask`, `costfacts_rule_id`, `costfacts_chosen_shape`,
   `costfacts_rejected_alternative_ids`, `redress_entry`, `substrate_surface`,
   `structural_projection_status`, `substrate_cardinality`, and
   `track2_independence_status` need semantic validation, not just presence.
2. The rendered report claims `gate-json` consumes the manifest, but the report
   validator would accept semantically substituted values for the fields above.
   That is a paper-closure risk for later W1/W3 governance and for CH3 row
   identity/source/freshness auditability.

## Required V10 Fold

1. Add exact W0 semantic validation for substrate telemetry by workload:
   `parse_only` must be
   `substrate_surface=borrowed_view_over_offset_tape`,
   `structural_projection_status=discarded_after_capacity`,
   `substrate_cardinality=one`; `direct_to_struct` must be
   `sink_only_digest / n/a / zero_or_inert`; `real_typed_struct` must be
   `typed_direct_projection / n/a / zero_or_inert`.
2. Add exact W0 sentinel validation for pre-W1 CostFacts:
   `costfacts_rule_id == "none:pre-W1"`,
   `costfacts_chosen_shape == "none:pre-W1"`, and
   `costfacts_rejected_alternative_ids == ["none:pre-W1"]`, unless W1 has
   actually landed and the wave is no longer W0.
3. Add exact W0 validation for `redress_entry == "none"` and
   `track2_independence_status == "independent_verified"`, or replace them with
   a stricter enum/row-class contract if a different W0 value is intended.
4. Constrain `build_flags`, `host_triple`, and `feature_mask` to the same
   benchmark metadata/run facts already used to generate them, or at minimum
   reject malformed W0 strings that omit `profile=bench`,
   `rustflags=-C target-cpu=native`, `target_cpu=native`, host triple, arch/os,
   and active SIMD backend.
5. Add focused negative tests that mutate each field group above while
   preserving row id, outcome/verdict, throughput, run id, and comparator
   evidence, and assert `validate_sk_v8_w0()` fails.
6. Preserve the accepted V9 fixes: W0 rows remain frozen as
   `strictness=deferred`, `measured_validation_path=view-boundary`,
   `parse_utf8=view-boundary`, `escape_complete=yes`; hard-failure and
   non-GO outcomes remain strict-admission ineligible.
