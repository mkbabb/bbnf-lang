# SK-V11 W5-R4: Row Gates, Floors, Profiles, And Measurement Commands

Date: 2026-05-20.
Scope: W5 Phase 1 research for row gates, floors, profile-backed candidate
ranking, and exact measurement commands. This artifact is research-only. It
does not edit source code, generated code, `skinny/RESULTS.md`, or
`skinny/REDRESS.md`.

## Inputs Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0 and 9.
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`.
- `skinny/RESULTS.md`.
- P1 profile and hot-leaf artifacts:
  `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`,
  `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`,
  `p1f-results-delta.md`, and `HARDENING-S-P1-CONVERGED.md`.
- REDRESS 111-115 in `skinny/REDRESS.md`.
- Prior W3/W4 plan command shapes for local measurement convention.

## Entry State

W5 may dispatch because W4 rejected with REDRESS 115. The carried-forward
state is:

- REDRESS 111 admitted W1a's non-JSON gate/report lane.
- REDRESS 112 rejected W1b's generated non-JSON baseline.
- REDRESS 113 blocks W2 from creating the first measurable non-JSON row.
- REDRESS 114 rejected W3 numeric direct closure on `mesh`.
- REDRESS 115 rejected W4 container-tail direct dispatch on `random`.

W5 therefore remains a JSON direct-plane closure attempt under SPEC Section 9.
It does not close the non-JSON axis and must carry REDRESS 113 forward.

## Selected Row Gate

R4 selects exactly one W5 row for the first redress packet:

| Row | W0 Track 1 | W0 Track 2 | sonic direct | Floor | Miss T1 | Miss T2 | R4 disposition |
|---|---:|---:|---:|---:|---:|---:|---|
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | 185 | 929 | selected primary W5 row |

Rationale: `random` is W5-eligible, near-floor on Track 1, string/whitespace
profiled on both tracks, and still has enough Track 2 miss to falsify weak
string-span claims. REDRESS 115 already showed that unrelated container-tail
work can crater this row, so W5 must stay probe-first and must not select a
second row without fresh CHALLENGE approval.

No second row is selected in R4. The W5 SPEC allows at most two target rows,
but every other string-profiled eligible row either has a large floor miss or
belongs more naturally to W6's escaped-string route. A later plan may promote a
second row only after same-host probes show useful movement and all guard rows
stay inside budget.

## Eligible W5 Rows And Floors

Floor is `ceil(sonic-rs strict direct Mbps / 1.10)`. Miss values are
`floor - W0 Mbps`, so positive values are below floor.

| Row | W0 Track 1 | W0 Track 2 | sonic direct | Floor | Miss T1 | Miss T2 | Profile read |
|---|---:|---:|---:|---:|---:|---:|---|
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | 185 | 929 | selected primary; tiny/string/whitespace/digest |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | 1872 | 2585 | scout only; strongest tiny-string leaf but large Track 2 miss |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | 1485 | 2807 | scout only; tiny/movemask but large Track 2 miss |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | 908 | 1033 | micro-proof sentinel only; huge relative row lift needed |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | 1072 | 1159 | support scout only; movemask/special-byte dominated |
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | 2127 | 2924 | scout only; large absolute Track 2 miss |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | 1967 | 2921 | defer to W6 unless explicitly selected as Unicode route |

Likely profile order for scouts, not row admission order:

1. `update_center`: direct T1 hot leaves are 26.3% `tiny_string`, 10.0%
   `movemask`, 7.9% `u64_add`; T2 is 22.3% `hand_tiny`, 12.3%
   `plain_string`, 10.9% `movemask`. It is the best profile fit but not a
   good first row gate because it needs a large Track 2 lift.
2. `github_events`: direct T1 is 24.4% `tiny_string`, 15.2% `movemask`,
   13.6% `ws`; T2 is 19.9% `hand_tiny`, 14.3% `movemask`, 9.7% `ws`.
3. `distinct_values`: direct T1 is 22.1% `tiny_string`, 15.8% `ws`,
   11.6% `fold_string`; T2 is 19.4% `hand_tiny`, 16.5% `ws`. Use it to
   falsify primitive-only enthusiasm, not as an initial row gate.
4. `gsoc-2018`: direct T1/T2 are movemask-led. It is a special-byte support
   scout, not a scalar span first pick.
5. `twitter`: string/whitespace/movemask profile is real, but the absolute
   floor miss is too large for the first W5 redress.
6. `y_string_unicode`: high c/B and escape/hex profile belong to W6 unless W5
   deliberately selects a Unicode row and accepts the larger guard burden.

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

Unicode residual monitors for a plain-string W5 packet:

| Row | W0 Track 1 | W0 Track 2 | sonic direct | Floor | W5 treatment |
|---|---:|---:|---:|---:|---|
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | residual monitor, not admitted |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | W0-clamped residual monitor |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | residual monitor unless explicitly selected |

## Measurement Gates

Probe sub-gate:

- Run the selected `random` probes before and after the W5 patch.
- `random` passes the probe sub-gate only if at least one track improves by
  `>= 1.0%` Mbps and the other track does not regress by more than `0.5%`.
- Direct guard, typed guard, and Unicode monitor probes must not show a
  guard-threatening regression.
- Probe movement is not row admission. It only permits Criterion.

Criterion row gate:

- `random/direct_to_struct` must meet `>= 7878` Mbps on both Track 1 and
  independent Track 2 in the same native Criterion root.
- The same Criterion root must include same-run `sonic_rs_direct_to_struct`
  and `serde_json_direct_to_struct` comparator rows.
- Direct and typed guard floors above must hold.
- Unicode residual rows remain `N-direct / NO-GO` unless selected by a later
  CHALLENGE; W5 must record their measurements honestly.

Gate/report consumption:

- W5 should reuse the JSON direct contract instead of adding telemetry fields.
- Any admitted row must be strict measured-row evidence on digest output with
  `same_wave_consumer_class=gate_json_direct_contract`,
  `wave_id=SK-V11-W5`, and the next W5 REDRESS id, expected to be
  `REDRESS-116` if the ledger has not advanced.
- `gate-json --with-cost-facts --check-results` must reject stale W2/W10/W4
  provenance, `gate_only`, deferred validation, coupled Track 2, wrong output
  plane, missing REDRESS, or a false accept below the W5 floor.

## Exact Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Build the probe binary:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Selected row, direct guards, and Unicode monitors. Run this block before and
after the W5 source patch:

```sh
for row in random citm_catalog apache_builds marine_ik unicode_basic unicode_escapes unicode_mixed y_string_unicode; do
  ./target/release/profile_direct 20000 "$row" track1
  ./target/release/profile_direct 20000 "$row" track2
done
```

Scout-only W5 eligible rows. These do not become selected rows unless a later
CHALLENGE widens the target set:

```sh
for row in update_center github_events distinct_values gsoc-2018 twitter; do
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

Expected W5 correctness and gate tests for the implementation packet:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p parse-that-regex bounded_plain_string -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w5_string_span -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w5 -- --nocapture
```

If W5 routes any AArch64 SIMD body, strict SIMD parity is mandatory before
product rows count:

```sh
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --tests
```

Criterion for the selected row, direct guards, and Unicode residual monitors:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(random|citm_catalog|apache_builds|marine_ik|unicode_basic|unicode_escapes|unicode_mixed|y_string_unicode)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

Criterion for typed guards:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

Final gate/report check:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Rejection Conditions

Reject W5 and record measured REDRESS if any of these occur:

- `random` misses 7878 Mbps on either Track 1 or Track 2.
- Probe movement is below the useful threshold or looks like unrelated
  container-tail, digest, cold, or noise movement.
- Direct or typed guard floors fail.
- Unicode residual monitors regress materially or are admitted without being
  selected.
- The patch replays REDRESS 106/108 string-proof-to-production, retained
  `StringBlock16`, decoded scratch/stats, quote-source streaming hash,
  byte-output materialization, parser-owned sidecar, or primitive-parity-only
  production.
- The patch treats W2's blocked non-JSON axis, W3's numeric rejection, or W4's
  container-tail rejection as closed by W5.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0 and 9.
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` REDRESS 111-115.
- `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md`.
- `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
