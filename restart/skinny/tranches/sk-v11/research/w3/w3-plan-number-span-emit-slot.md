# SK-V11 Wave W3 Plan: Scalar Number Span Emit Slot

Date: 2026-05-20.

Inputs:

- `restart/skinny/tranches/sk-v11/research/w3/w3-R1-numeric-scanner-semantics.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R2-numeric-consumers.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R3-numeric-row-gates.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R4-dotprod-microproof.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R5-numeric-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v11/research/w3/w3-R6-grammar-neutral-compatibility.md`.
- `restart/skinny/tranches/sk-v11/SPEC.md` Section 7.

Intervention: factor the generated JSON direct number path into a scalar
`number_span_emit_slot` shape that scans one `NumberSpan` and emits through the
current root/object/array numeric sink slot without changing parse-that number
semantics, materialization policy, or output digest semantics.

## Owner Paths

Redress may touch only:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/sink_direct.rs` only if the source change must
  preserve the generated JSON direct template; no generic number policy or
  non-JSON codegen route may move
- `skinny/crates/parse-that-regex/src/number/mod.rs` only for parity tests or
  a semantic-preserving scalar helper; no policy edits
- `skinny/crates/bbnf-bench/src/direct_struct.rs` only for exact number-class
  parity tests if needed
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` only for guard
  measurement or exact parity tests if needed
- `skinny/crates/bbnf-bench/benches/json_parity.rs` only for named W3
  measurement support if needed
- `skinny/crates/bbnf-bench/src/bin/gate.rs` for W3 direct-row gate
  consumption, provenance marking, and tests
- `skinny/crates/bbnf-bench/src/report.rs` only if gate/report schema
  consumption needs W3 provenance metadata
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

No `bbnf-simd` owner is authorized in this plan. UDOT is explicitly out of the
W3 redress slice.

## Falsifiability Gate

Gate: `G-W3-NUMERIC-SEQUENCE-DIRECT`.

Primary target:

- `mesh/direct_to_struct`: Track 1 and Track 2 must both be `>= 8675` Mbps.

Optional measured target if the implementation remains within the cap:

- `numbers/direct_to_struct`: Track 1 and Track 2 must both be `>= 2425` Mbps.
  This row is W0-clamped, so admission requires W3 REDRESS provenance even if
  fresh measurement clears the floor.

Guard rows:

- Direct guard rows from SPEC §0.5 hold if measured:
  `citm_catalog` 18191/17431, `apache_builds` 11028/9996, `marine_ik`
  8759/9248, `unicode_basic` 2253/2182.
- Typed guard rows hold if measured. If `mesh/real_typed_struct` is measured,
  it must stay at or above 9214/7739.

The implementation is rejected if no selected target gains `>= 1.0%` in the
same-host caller or if either selected target misses its §0.4 floor in the
Criterion gate. A W0-clamped row cannot admit from a probe alone.

The `>= 1.0%` caller-movement check is calculated from native
`profile_direct` probes against the same executable mode and corpus before and
after the patch:

- `mesh track1` and `mesh track2` are mandatory.
- `numbers track1` and `numbers track2` are mandatory only if `numbers` is
  claimed.
- A selected row passes this sub-gate if at least one of its two modes improves
  by `>= 1.0%` Mbps and the other mode does not regress by more than `0.5%`.
- Probe movement is not row admission; Criterion plus `gate-json` still decide
  the row.

Gate consumption is part of W3. `gate-json --with-cost-facts --check-results`
must classify an admitted W3 direct row with strict measured-row provenance,
W3 wave id, REDRESS provenance, non-`gate_only` same-wave consumer metadata,
and no W0 no-admission clamp.

## Micro-Prove-First Record

Native probe build:

```sh
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Current caller probes on HEAD after REDRESS 113:

| Corpus | Mode | Iters | Mbps | ns/B | cycles/B | CPI |
|---|---|---:|---:|---:|---:|---:|
| `mesh` | `track1` | 2000 | 3473.104 | 2.303415 | 4.926057 | 0.148724 |
| `mesh` | `track2` | 2000 | 3419.223 | 2.339713 | 5.122278 | 0.159416 |
| `numbers` | `track1` | 5000 | 4824.035 | 1.658363 | 3.408001 | 0.154072 |
| `numbers` | `track2` | 5000 | 5038.780 | 1.587686 | 3.251573 | 0.142277 |
| `mesh` | `real_typed_track1` | 1000 | 3436.687 | 2.327823 | 4.865849 | 0.180860 |
| `mesh` | `real_typed_track2` | 1000 | 3190.178 | 2.507697 | 5.546278 | 0.193381 |

Probe interpretation:

- `mesh` is the primary target because it is near-floor and not W0-clamped.
- `numbers` is an optional W0-clamped target because the numeric direct surface
  is measurable and needs only a small Track 2 floor lift in the SK-V11-open
  table, but it still requires W3 Criterion provenance.
- These probes do not authorize UDOT or all-four-row selection.
- These probes do not authorize row movement without the post-redress
  Criterion gate and W3 `gate-json` consumption.

## Same-Wave Consumer

The same-wave consumer is generated JSON direct `parse_direct`:

- root numeric values through `parse_value_direct` -> number slot;
- object numeric values through `parse_object_value_at_direct` -> number slot;
- array numeric values through `parse_array_element_at_direct` -> number slot.

The hot caller is `bbnf_bench::direct_struct::track1_digest`, which invokes
`runtime::generated_json::parse_direct`. Track 2 and serde/sonic remain output
parity and strict comparator backstops. Track 2 is an independent caller, but
not an independent numeric scanner/materializer oracle: it shares
`parse_that_regex::match_number_span_from_first` and the parse-that numeric
materializers. REDRESS must record this coupling honestly.

## Implementation Shape

1. Preserve `match_number_span_from_first` and the existing `NumberSpan`
   semantics.
2. Replace the three generated direct number wrapper/emitter bodies with one
   scalar slot helper or equivalent zero-policy refactor.
3. Preserve exact `-0.0`, i64/u64/f64 class dispatch, cursor advancement, and
   `ParseErrorKind::InvalidNumber` behavior.
4. Add mandatory generated `parse_direct` root/object/array numeric-slot tests
   comparing exact digest behavior against serde for `-0`, `-0.0`, i64 max,
   i64 min, u64 max, u64 overflow-to-f64, subnormal, exponent forms, and
   representative invalid suffix ownership. These tests must cover class
   dispatch to `i64`, `u64`, and `f64` sink slots in root, object, and array
   placement.
5. If the generated runtime body changes, either preserve the generated source
   template in `sink_direct.rs` in the same wave or record a no-regeneration
   proof. Do not edit broader codegen templates or non-JSON routes without
   same-wave CSS/Sheets proof.
6. Add W3 gate tests proving `mesh` can admit at floor `8675`, optional
   `numbers` can admit at floor `2425` only with W3 provenance, and unrelated
   SK-V8-open `N-direct` rows remain clamped.

## Hard Cap

Redress cap: 90 min wall, with 60 min implementation and 30 min parity plus
selected Criterion measurement.

## Required Verification Commands

- `RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct`.
- `target/release/profile_direct 2000 mesh track1`.
- `target/release/profile_direct 2000 mesh track2`.
- Post-redress repeat of the two mesh probes above, plus `numbers` probes if
  `numbers` is claimed.
- `cargo test -p bbnf-bench --bin gate w3 -- --nocapture`.
- `cargo test -p bbnf-bench direct_struct::tests::generated_direct_number_slots_match_serde -- --nocapture`.
- `CRITERION_HOME=/tmp/skv11-w3-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(mesh|numbers)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'`.
- `CRITERION_HOME=/tmp/skv11-w3-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`.

## Revert Protocol

Revert the source slice as one patch and save it at
`/tmp/skv11-waveW3-rejected.patch` if:

- any number span/materialization parity test fails;
- generated direct output diverges from Track 2, serde, or sonic comparator;
- selected rows miss floors;
- no target row shows `>= 1.0%` useful caller movement;
- direct or typed guards regress beyond their floors;
- W2 BLOCKED is treated as non-JSON proof closure;
- the patch changes f64 fallback, mantissa policy, leading-zero/sign/exponent
  policy, suffix handling, or generic non-JSON number policy.

## Pre-Blocked Routes

- REDRESS 31 raw `parse::<f64>()` shortcut.
- REDRESS 39 materializer-gap reopening.
- REDRESS 46 local digit-scan redo without product row movement.
- REDRESS 80 mantissa/fallback widening.
- W0-clamp admission without W3-W8 measured provenance.
- Parse-only numeric evidence.
- UDOT/4-digit MAC proof without a full scalar digit-span oracle, strict
  checkasm, feature fallback, same-wave consumer, and row movement.
- W3 union/event/class-column/streaming-cursor/sidecar substrate family.

## W2 Blocked Route

W2 remains BLOCKED by REDRESS 113. W3 may dispatch only as a JSON direct-plane
numeric closure/fixpoint wave. It does not satisfy the non-JSON generated
intervention axis and must carry that route into W8/W9 close.
