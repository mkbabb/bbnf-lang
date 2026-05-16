# SK-V7 W2 Phase 1 Research: Direct Gate Mechanics

Date: 2026-05-16

Scope: benchmark/gate mechanics for `canada`, `numbers`, `mesh`, and `marine_ik` direct rows. This report does not change source; it identifies the commands, row extraction points, current gate semantics, and routes that are blocked before redress.

## Findings

1. W2's authoritative target is `canada` direct-to-struct, with `numbers` as the no-regression guard and `mesh`/`marine_ik` direct rows as companion checks. `SPEC.md` names the owner paths and asks W2 to profile `canada`, investigate Eisel-Lemire mantissa widening, land parity-preserving changes, and bench `canada direct + numbers direct + mesh direct + marine_ik direct` (`restart/skinny/tranches/sk-v7/SPEC.md:149`, `restart/skinny/tranches/sk-v7/SPEC.md:151`, `restart/skinny/tranches/sk-v7/SPEC.md:156`, `restart/skinny/tranches/sk-v7/SPEC.md:160`). The falsifiability gate is explicit: `canada` direct must reach at least sonic-strict, `numbers` direct must remain PASS, and no row may regress (`restart/skinny/tranches/sk-v7/SPEC.md:162`, `restart/skinny/tranches/sk-v7/SPEC.md:163`, `restart/skinny/tranches/sk-v7/SPEC.md:164`, `restart/skinny/tranches/sk-v7/SPEC.md:165`).

2. The global close protocol is stricter than the W2 bench subset. `SPEC.md` requires `check-conformance`, `bench-json`, and `gate-json` before a wave exit gate can close (`restart/skinny/tranches/sk-v7/SPEC.md:72`, `restart/skinny/tranches/sk-v7/SPEC.md:74`, `restart/skinny/tranches/sk-v7/SPEC.md:81`). The schema also matters: `RESULTS.md` must carry the 24-column schema, and `gate-json` rejects rows missing required columns (`restart/skinny/tranches/sk-v7/SPEC.md:56`, `restart/skinny/tranches/sk-v7/SPEC.md:69`).

3. The current direct gate is time-slack based, not exactly `Track 1 Mbps >= sonic Mbps`. `DIRECT_PROJECTION_SONIC_SLACK` is `1.10` (`skinny/crates/bbnf-bench/src/gate.rs:55`), and `classify_direct_projection` fails if either Track 1 or Track 2 time is more than `1.10x` sonic time (`skinny/crates/bbnf-bench/src/gate.rs:169`, `skinny/crates/bbnf-bench/src/gate.rs:178`). This explains why `numbers` currently shows PASS despite `Track 1` being slightly below sonic in the current table.

4. Current `RESULTS.md` direct rows:

| Corpus | Track 1 Mbps | Track 2 Mbps | Sonic strict Mbps | Current verdict signal |
|---|---:|---:|---:|---|
| `canada` | 10464 | 10119 | 12509 | NO-GO direct (`skinny/RESULTS.md:11`) |
| `numbers` | 12566 | 12073 | 12919 | PASS within gate (`skinny/RESULTS.md:30`) |
| `mesh` | 8252 | 8298 | 9612 | NO-GO direct (`skinny/RESULTS.md:20`) |
| `marine_ik` | 9193 | 9341 | 8592 | PASS within gate (`skinny/RESULTS.md:26`) |

5. The current `canada` direct blocker is not structural scan throughput. The `canada` direct note says Track 1/Track 2 must be within `1.10x` sonic time (`skinny/RESULTS.md:158`), while the same report says the `canada` structural scan is 69075 Mbps against a 40000 Mbps floor (`skinny/RESULTS.md:162`).

6. `json_parity` emits all direct-to-struct rows needed for W2. For each fixture, it runs parity first (`skinny/crates/bbnf-bench/benches/json_parity.rs:15`, `skinny/crates/bbnf-bench/benches/json_parity.rs:18`), then benches `track1_direct_to_struct`, `track2_direct_to_struct`, `sonic_rs_direct_to_struct`, and `serde_json_direct_to_struct` (`skinny/crates/bbnf-bench/benches/json_parity.rs:181`, `skinny/crates/bbnf-bench/benches/json_parity.rs:203`, `skinny/crates/bbnf-bench/benches/json_parity.rs:225`, `skinny/crates/bbnf-bench/benches/json_parity.rs:243`). Criterion metadata is written under `target/criterion/json_<corpus>/<bench>/metadata.toml` (`skinny/crates/bbnf-bench/benches/json_parity.rs:490`, `skinny/crates/bbnf-bench/benches/json_parity.rs:497`, `skinny/crates/bbnf-bench/benches/json_parity.rs:505`).

7. The gate reads row values from Criterion slope estimates. For direct rows, `gate.rs` reads `track1_direct_to_struct`, `track2_direct_to_struct`, `sonic_rs_direct_to_struct`, and `serde_json_direct_to_struct` from `target/criterion/json_<fixture>/<bench>/new/estimates.json` (`skinny/crates/bbnf-bench/src/bin/gate.rs:35`, `skinny/crates/bbnf-bench/src/bin/gate.rs:43`, `skinny/crates/bbnf-bench/src/bin/gate.rs:46`, `skinny/crates/bbnf-bench/src/bin/gate.rs:553`). It converts nanoseconds to Mbps as `bytes * 8000 / ns` (`skinny/crates/bbnf-bench/src/bin/gate.rs:411`, `skinny/crates/bbnf-bench/src/bin/gate.rs:416`) and pushes the direct workload row to `RESULTS.md` (`skinny/crates/bbnf-bench/src/bin/gate.rs:101`, `skinny/crates/bbnf-bench/src/bin/gate.rs:108`).

8. The direct Track 1/Track 2 mechanics are separate but comparable. Track 1 calls generated `runtime::generated_json::parse_direct` through `track1_digest` (`skinny/crates/bbnf-bench/src/direct_struct.rs:401`, `skinny/crates/bbnf-bench/src/direct_struct.rs:403`). Track 2 is the hand parser (`skinny/crates/bbnf-bench/src/direct_struct.rs:408`, `skinny/crates/bbnf-bench/src/direct_struct.rs:440`). Both are parity-checked against serde and sonic shape before the gate treats the row as valid (`skinny/crates/bbnf-bench/src/direct_struct.rs:420`, `skinny/crates/bbnf-bench/src/direct_struct.rs:421`, `skinny/crates/bbnf-bench/src/direct_struct.rs:425`).

9. W2's suspected number fallback is in `parse-that-regex`, not in `direct_struct.rs` itself. `direct_struct.rs` calls `materialize_f64` for non-integer or non-fitting numeric spans (`skinny/crates/bbnf-bench/src/direct_struct.rs:89`, `skinny/crates/bbnf-bench/src/direct_struct.rs:102`). `materialize_f64` uses Eisel-Lemire only when the mantissa has not overflowed, then falls back to `text.parse::<f64>()` (`skinny/crates/parse-that-regex/src/number/mod.rs:260`, `skinny/crates/parse-that-regex/src/number/mod.rs:262`, `skinny/crates/parse-that-regex/src/number/mod.rs:270`). Mantissa overflow is currently set once digit count exceeds 19 and a later digit is non-zero (`skinny/crates/parse-that-regex/src/number/mod.rs:304`, `skinny/crates/parse-that-regex/src/number/mod.rs:317`).

10. `mesh` and `marine_ik` real typed rows are pre-blocked for W2 research. The current real-typed fixture enum supports only `Twitter` and `UpdateCenter` (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:9`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:13`), and `fixture_for_name` returns rows only for `twitter` and `update_center` (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:74`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:78`). `SPEC.md` correctly assigns mesh/marine real-typed schema work to W3, not W2 (`restart/skinny/tranches/sk-v7/SPEC.md:179`, `restart/skinny/tranches/sk-v7/SPEC.md:186`, `restart/skinny/tranches/sk-v7/SPEC.md:194`).

## Exact Commands

Use the full gate protocol before declaring W2 closed:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

For a W2 Phase 1 direct-only measurement loop, run the scoped Criterion subset first, then use the extraction commands below. This does not replace the full gate:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo bench -p bbnf-bench --bench json_parity -- 'json/(canada|numbers|mesh|marine_ik)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
```

If fresh gate output is desired after a full or sufficiently complete bench set:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

Use `--advisory` during partial/direct-only loops because the gate otherwise exits on the worst measured outcome after writing `RESULTS.md` (`skinny/crates/bbnf-bench/src/bin/gate.rs:231`, `skinny/crates/bbnf-bench/src/bin/gate.rs:236`, `skinny/crates/bbnf-bench/src/bin/gate.rs:237`). For the final close, use the xtask protocol without `--advisory`.

For profiling the direct row hot path:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --release -p bbnf-bench --bin profile_direct
samply record --save-only -o canada-direct-track1.profile.json.gz ./target/release/profile_direct 10000 canada track1
samply record --save-only -o canada-direct-track2.profile.json.gz ./target/release/profile_direct 10000 canada track2
```

The profiling binary documents the build and `samply` shape (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:1`, `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:4`, `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:5`) and accepts modes `track1`, `track2`, `sonic`, `serde`, and real-typed modes (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:57`, `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:59`, `skinny/crates/bbnf-bench/src/bin/profile_direct.rs:63`).

## Row Extraction

Criterion estimate extraction for the four W2 direct rows:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
for corpus in canada numbers mesh marine_ik; do
  bytes=$(awk -v c="$corpus" '
    $0 == "[fixtures." c "]" { in_row=1; next }
    in_row && /^size_bytes = / { print $3; exit }
  ' crates/test-fixtures/corpus/json/manifest.toml)
  printf "%s\n" "$corpus"
  for bench in track1_direct_to_struct track2_direct_to_struct sonic_rs_direct_to_struct serde_json_direct_to_struct; do
    ns=$(jq -r '.slope.point_estimate // .mean.point_estimate' "target/criterion/json_${corpus}/${bench}/new/estimates.json")
    awk -v bench="$bench" -v bytes="$bytes" -v ns="$ns" 'BEGIN { printf "  %-28s ns=%10.2f Mbps=%8.0f\n", bench, ns, bytes * 8000 / ns }'
  done
done
```

`RESULTS.md` row extraction after `gate-json`:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
awk -F'|' '
  $2 ~ / (canada|numbers|mesh|marine_ik) / && $3 ~ / direct_to_struct / {
    gsub(/^ +| +$/, "", $2); gsub(/^ +| +$/, "", $3);
    gsub(/^ +| +$/, "", $11); gsub(/^ +| +$/, "", $12); gsub(/^ +| +$/, "", $13);
    gsub(/^ +| +$/, "", $27);
    printf "%s %-17s Track1=%s Track2=%s sonic=%s Signal=%s\n", $2, $3, $11, $12, $13, $27
  }
' RESULTS.md
```

The fixture sizes for these rows are in the manifest: `canada` 2,251,051 bytes, `mesh` 723,597 bytes, `marine_ik` 2,983,466 bytes, and `numbers` 150,124 bytes (`skinny/crates/test-fixtures/corpus/json/manifest.toml:13`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:15`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:37`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:39`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:55`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:57`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:67`, `skinny/crates/test-fixtures/corpus/json/manifest.toml:69`).

## Recommendations

1. Treat W2 Phase 1 as a measurement and attribution pass before changing the EL path. Run the scoped direct Criterion subset, extract all four rows, then profile `canada track1` and `track2` with `profile_direct`.

2. For the redress implementation, instrument or locally count how often `materialize_f64` reaches the `text.parse::<f64>()` fallback, but keep that instrumentation out of the committed source unless the wave plan explicitly admits it. The source path is `materialize_f64` fallback in `parse-that-regex`, not `direct_struct.rs`.

3. Preserve `numbers` as a guard against mantissa widening regressions. It is currently a PASS under the `1.10x` time slack gate even though `Track 1 Mbps` is slightly below sonic in `RESULTS.md`; the final W2 report should state both Mbps ratio and gate outcome to avoid confusing "PASS" with ">=100% Mbps".

4. Do not attempt to close mesh/marine real-typed goals in W2. For this phase, bench their direct-to-struct rows only. Mesh/marine real-typed rows require the W3 schema and harness work before Criterion/gate can emit them.

5. For final close, run the canonical xtask sequence and include the generated `RESULTS.md` direct rows plus `primitive-checkasm`, because W2 exit requires EL parity tests green (`restart/skinny/tranches/sk-v7/SPEC.md:167`, `restart/skinny/tranches/sk-v7/SPEC.md:170`).

## Risks And Pre-Blocked Routes

- Partial Criterion runs can leave stale estimates for unmeasured rows. Use them only for local W2 diagnosis; final `gate-json` must follow a full `bench-json`.
- `gate-json --advisory` is useful for partial loops, but a wave cannot close under advisory mode because `SPEC.md` requires all protocol commands to pass.
- `mesh` direct is currently NO-GO and may not be helped by EL fallback elimination if digit scanning, not EL, dominates. SK-V7 synthesis already warns that mesh's actual close is the W3 Vec specialization route, while EL is only a small slice of mesh cost (`restart/skinny/tranches/sk-v7/SYNTHESIS.md:130`, `restart/skinny/tranches/sk-v7/SYNTHESIS.md:135`).
- `marine_ik` direct is already PASS in the current gate, so W2 should protect it as a regression sentinel rather than using it as proof that the `canada` intervention worked.
- Current `parse-that-regex` has no `parse-attribution` feature in its `Cargo.toml`; attribution symbols exist mainly through generated runtime/codegen `cfg_attr(feature = "parse-attribution", inline(never))` sites and the runtime feature. If W2 needs numeric fallback counters, prefer local profiling/instrumentation over assuming a ready-made crate feature surface.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md`
- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/test-fixtures/corpus/json/manifest.toml`
