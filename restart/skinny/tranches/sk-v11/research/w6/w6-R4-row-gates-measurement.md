# SK-V11 W6-R4: Row Gates, Floors, Profiles, And Measurement Commands

Date: 2026-05-20.
Scope: W6 Phase 1 research for falsifiability gates, row floors,
probe/Criterion commands, comparator requirements, guard rows, and probe
plausibility. This artifact is research-only. It does not edit source code,
generated code, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.

## Inputs Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0.4, 0.5, and 10.
- `restart/skinny/tranches/sk-v11/HANDOFF.md` residual table and W6 handoff
  note.
- `skinny/RESULTS.md` SK-V11-open rows for `unicode_escapes`,
  `unicode_mixed`, and `y_string_unicode`.
- `skinny/REDRESS.md` REDRESS 116.
- `restart/skinny/tranches/sk-v11/research/w5/redress/w5-redress-entry-blocked.md`.
- P1 profile and hot-leaf artifacts:
  `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`,
  `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`, and
  `p1f-results-delta.md`.

## Entry State

W6 may research because W5 has a disposition, but W5 did not admit a span API
or reusable scalar proof. REDRESS 116 is binding: W6 may dispatch only through
SPEC Section 10's independent segment-plan route, and CHALLENGE must name a new
source delta beyond the already-consuming `unescape_string` path.

W6 cannot close by proving that the existing `unescape_string` path already
calls `unescape_uxxxx_x4_neon`. Reuse of the existing path as the same-wave
production consumer is pre-blocked in SPEC Section 10. The selected plan must
name a fresh escaped-segment visitor, hex-run oracle, generated caller, typed
caller, or non-JSON escaped-string/hex-color consumer that changes production
behavior inside W6 owner paths.

## W6 Exit Gate Summary

Exit gate `G-W6-ESCAPE-SEGMENT-DIRECT` admits only selected direct rows from:

- `unicode_escapes/direct_to_struct`
- `unicode_mixed/direct_to_struct`
- `y_string_unicode/direct_to_struct`

Each admitted row must meet its SPEC Section 0.4 floor on generated Track 1 and
independent Track 2 in the same native measurement packet. Admission also
requires same-run strict direct comparator rows, exact generated Track 1 versus
independent Track 2 output equality on the digest plane, no Track 2 coupling,
and gate consumption through the JSON direct contract.

If no JSON direct row can close, W6 may admit a non-JSON escaped-string or
hex-color consumer only if W2 has not already satisfied the non-JSON close axis
and all JSON rows record honest measurements. In the current SK-V11 state W2
is blocked by REDRESS 113, so a non-JSON W6 route would still need a generated
Track 1 and independent oracle in the same wave; it cannot be a prose Lock 14
claim.

## Candidate Floors And Selection

Floor is `ceil(sonic-rs strict direct Mbps / 1.10)`. Miss values are
`floor - W0 Mbps`, so negative values mean the W0 number is above the floor but
still cannot admit without behavior-wave provenance.

| Row | W0 Track 1 | W0 Track 2 | sonic direct | Floor | Miss T1 | Miss T2 | Floor ratio T1 | Floor ratio T2 | R4 disposition |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | -1165 | 161 | 0.69x | 1.07x | selected primary W6 candidate |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | 2096 | 2100 | 2.56x | 2.57x | scout/uncloseable-proof candidate only unless probes clear both floors |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | 1967 | 2921 | 1.99x | 3.84x | not selected for first W6 admission packet |

R4 selects exactly one primary row for the first W6 plan:
`unicode_mixed/direct_to_struct`. It is W0-clamped and still `N-direct /
NO-GO`, but its Track 1 value is already above the floor and Track 2 needs only
161 Mbps, a 6.6% lift. Its P1 direct hot leaves align with W6's escape/string
surface: Track 1 is 28.0% `full_string`, 20.3% `unescape`, and 12.9%
`validate_escape`; Track 2 is 26.4% `full_string`, 18.4% `unescape`, and
13.8% `validate_escape`.

`unicode_escapes` is SPEC-eligible but not a good first selected row. Both
tracks need roughly 2.56x current throughput to reach the floor. The target
leaves are real - Track 1 is 25.1% `unescape`, 22.1% `full_string`, and 10.3%
`hex_unit`; Track 2 is 23.4% `unescape`, 22.0% `full_string`, and 9.0%
`hex_unit` - but the row requires a near-total elimination of the selected cost
surface plus no new overhead. It should be measured as a W6 scout and recorded
as an uncloseable proof if it remains below floor.

`y_string_unicode` is also SPEC-eligible but should not be selected for the
first W6 admission packet. Track 1 needs about 1.99x current throughput and
Track 2 needs about 3.84x. Its direct hot leaves are `hex_nibble`, `hex_unit`,
`unescape`, and `validate_unicode`, but the Track 2 miss is too large for a
single <=360 LOC escaped-segment packet unless a pre-redress probe already
shows floor-level movement.

## Probe Plausibility Before Criterion

Probe movement is not row admission. It only decides whether Criterion is worth
running.

Required W6 probe sub-gate:

- Build the native `profile_direct` binary from the post-plan source tree.
- Run old-vs-new probes for all three W6 candidate rows on Track 1 and Track 2.
- Continue to Criterion only if every selected row is at or above its Section
  0.4 floor on both tracks in repeated same-host probes.
- For `unicode_mixed`, require Track 2 to clear 2588 Mbps with margin because
  the miss is only 161 Mbps and Criterion noise can erase a barely passing
  probe. A practical target is repeated Track 2 probes at or above 2620 Mbps.
- `unicode_escapes` and `y_string_unicode` cannot be promoted from scout to
  selected admission rows unless their post-patch probes already clear both
  floors. A 1% primitive movement is insufficient.
- Direct guards, typed guards, and unselected W6 candidate monitors must not
  show guard-threatening regression.

R4 plausibility verdict:

| Row | Probe plausibility | Criterion permission |
|---|---|---|
| `unicode_mixed/direct_to_struct` | Plausible if the plan names a concrete new Track 2 cost mechanism and source delta beyond existing `unescape_string`. | Run Criterion only after repeated probes put both tracks above 2588 Mbps, with Track 2 margin. |
| `unicode_escapes/direct_to_struct` | Low. The cost surface is real, but both tracks need more than 2.5x current throughput. | Do not run Criterion for admission unless probes first clear 3441 Mbps on both tracks. |
| `y_string_unicode/direct_to_struct` | Very low for first W6 redress. Track 2 needs almost 4x current throughput. | Do not run Criterion for admission unless probes first clear 3950 Mbps on both tracks. |

## Comparator And Correctness Requirements

W6 admission requires all of the following in the same measurement packet:

- generated Track 1 direct digest for the selected row;
- independent hand Track 2 direct digest for the selected row;
- exact Track 1 versus Track 2 output equality on the direct digest plane;
- same-run `sonic_rs_direct_to_struct` strict direct comparator row;
- same-run `serde_json_direct_to_struct` row for output-plane parity and
  malformed-input backstop;
- no Track 2 calls into generated Track 1, generated SinkOnly helpers,
  generated typed helpers, or a hidden shared parser;
- malformed escape/surrogate fixtures rejected by generated Track 1,
  independent Track 2, `serde_json`, and `sonic-rs`;
- `wave_id=SK-V11-W6`, expected next redress id `REDRESS-117` if the ledger has
  not advanced, native host triple, feature mask, `RUSTFLAGS="-C
  target-cpu=native"`, sample count, run id, and comparator freshness;
- no parse-only SOTA claim, decoded scratch, output hash side channel,
  JSON surrogate policy in generic crates, x4 proof-only production, or
  producer-only telemetry.

## Guard Floors

Direct guards from SPEC Section 0.5:

| Row | W0 Track 1 | W0 Track 2 | sonic direct | Track 1 maintain | Track 2 maintain |
|---|---:|---:|---:|---:|---:|
| `citm_catalog/direct_to_struct` | 18563 | 17787 | 15530 | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11254 | 10189 | 10995 | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8938 | 9437 | 8473 | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2299 | 2227 | 2353 | 2253 | 2182 |

Typed guards from SPEC Section 0.5:

| Row | W0 Track 1 | W0 Track 2 | sonic typed | Track 1 maintain | Track 2 oracle guard |
|---|---:|---:|---:|---:|---:|
| `twitter/real_typed_struct` | 17740 | 15912 | 15010 | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 30539 | 17675 | 20726 | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8478 | 6892 | 8106 | 8308 | 6754 |
| `github_events/real_typed_struct` | 11871 | 12275 | 12224 | 11633 | 12029 |
| `update_center/real_typed_struct` | 11851 | 10358 | 12467 | 11613 | 10150 |
| `mesh/real_typed_struct` | 9403 | 7897 | 8923 | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11788 | 10096 | 9010 | 11552 | 9894 |

W6 candidate monitors:

| Row | Floor | W6 treatment |
|---|---:|---|
| `unicode_mixed/direct_to_struct` | 2588 | selected primary candidate |
| `unicode_escapes/direct_to_struct` | 3441 | scout/uncloseable monitor unless floor-level probes clear both tracks |
| `y_string_unicode/direct_to_struct` | 3950 | scout/uncloseable monitor unless floor-level probes clear both tracks |

## Exact Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Build the probe binary:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Selected row, W6 scout rows, and direct guards. Run before and after the W6
source patch; use repeated post-patch probes before allowing Criterion:

```sh
for row in unicode_mixed unicode_escapes y_string_unicode citm_catalog apache_builds marine_ik unicode_basic; do
  ./target/release/profile_direct 20000 "$row" track1
  ./target/release/profile_direct 20000 "$row" track2
done
```

Typed guard probes:

```sh
for row in twitter citm_catalog apache_builds github_events update_center mesh marine_ik; do
  ./target/release/profile_direct 5000 "$row" real_typed_track1
  ./target/release/profile_direct 5000 "$row" real_typed_track2
done
```

Expected W6 correctness and policy tests for the implementation packet:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p parse-that-regex unescape -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p parse-that-regex unicode_escape -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w6_escape_segment -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w6 -- --nocapture
```

If W6 routes any AArch64 x4 body, strict SIMD parity is mandatory before
product rows count:

```sh
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --tests
```

Criterion for the selected row, W6 scout monitors, direct guards, and same-run
strict direct comparators:

```sh
CRITERION_HOME=/tmp/skv11-w6-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(unicode_mixed|unicode_escapes|y_string_unicode|citm_catalog|apache_builds|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

Criterion for typed guards:

```sh
CRITERION_HOME=/tmp/skv11-w6-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Final gate/report check:

```sh
CRITERION_HOME=/tmp/skv11-w6-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Rejection Conditions

Reject W6 and record measured REDRESS if any of these occur:

- CHALLENGE cannot name a new source delta beyond the already-consuming
  `unescape_string` path.
- The selected row misses its Section 0.4 floor on either Track 1 or Track 2.
- `unicode_mixed` Track 2 probes do not clear the 2588 Mbps floor with margin
  before Criterion.
- `unicode_escapes` or `y_string_unicode` are promoted to admission rows
  without floor-level probes on both tracks.
- Generated Track 1 and independent Track 2 digest output diverge.
- `serde_json` or `sonic-rs` disagrees on malformed escape/surrogate rejection
  for the W6 fixture set.
- Direct or typed guard floors fail.
- The patch replays decoded scratch, output hash side channel, JSON surrogate
  policy in a generic crate, x4 proof-only production, existing `unescape_string`
  as same-wave production, Track 2 coupling, or parse-only SOTA movement.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0.4, 0.5, and 10.
- `restart/skinny/tranches/sk-v11/HANDOFF.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` REDRESS 116.
- `restart/skinny/tranches/sk-v11/research/w5/redress/w5-redress-entry-blocked.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md`.
