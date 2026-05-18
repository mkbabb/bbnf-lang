# SK-V8 W2 Hardening V2 - CH4

Date: 2026-05-18.
Reviewer: CH4.
Target reviewed: `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

## Verdict

ACCEPT.

Confidence: 93%.

## Findings

1. Apache root-field coverage is folded. The W2 plan now names root `mode` and
   root `nodeName` beside `jobs[].name`, `jobs[].url`, and `jobs[].color`
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:11-13`). The
   schema source includes `mode`, `nodeName`, and `jobs` on `ApacheBuilds`
   (`skinny/xtask/src/real_typed_schema.rs:57-64`), the generated parser
   consumes the same keys (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:169-212`),
   and the checksum folds both root strings before the job vector
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:351-359`).

2. CITM map-entry semantics are product-visible rather than value-only. The
   schema emits `events` as `map_entries(..., "key", "value", 184, ty("CitmEvent"))`
   (`skinny/xtask/src/real_typed_schema.rs:75-88`), the generated parser builds
   `CitmEventEntry { key, value }` from object keys
   (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:797-811`), and the
   serde_json/sonic sidecar visitor preserves the same key/value entry shape
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:547-574`).

3. Checksum coverage matches the admitted W2 product plane. Apache folds root
   strings, job count, and each job's selected string fields. CITM folds event
   count, every event key, `id`, `name`, `subTopicIds`, and `topicIds`
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:351-384`). The minimal
   Apache/CITM tests exercise the added root fields and keyed CITM event shape,
   and the full-fixture W2 test covers both fixtures
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:595-618`).

4. The Track 2/oracle wording is no longer overclaimed in W2 admission text.
   Research says the product proof is generated Track 1 plus serde_json as the
   Track 2/oracle path and a separate sonic-rs typed parity lane, and explicitly
   says `track2_typed` delegates to serde_json
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:11-15`).
   The code agrees: `track2_typed` calls `serde_typed`, while `serde_typed`
   calls `serde_json::from_slice::<T>`
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251-281`).

5. The sonic lane is strict and separate for the typed source/product proof.
   W2 admission requires checksum equality against a separate sonic-rs strict
   lane (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:41-43`).
   The code calls `sonic_rs::from_slice::<T>` for each real typed fixture and
   compares the sonic checksum against Track 1, Track 2, and serde_json
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:284-323`). The bench
   crate keeps `sonic-rs` on `default-features = false` with only `sort_keys`,
   so this typed lane does not use the lossy comparator path
   (`skinny/crates/bbnf-bench/Cargo.toml:21`).

6. Canada remains excluded from W2 admission. The W2 plan records that Canada
   was falsified and routed out (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:4-5`);
   REDRESS rejects `canada/real_typed_struct` rather than weakening equality
   (`skinny/REDRESS.md:2637-2640`). `RealTypedFixture` and the W2 full-fixture
   test include Apache and CITM, not Canada
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:8-15`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:609-618`).

7. The W2 source/product posture is bounded correctly. HANDOFF records that
   `skinny/RESULTS.md` stays at the W0 four measured `real_typed_struct` rows
   and that W2 does not claim six measured `A / GO` rows
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:175-185`). REDRESS records source
   parity admission plus benchmark row-table rejection for this wave
   (`skinny/REDRESS.md:2641-2650`).

8. Focused verification passed in this review:
   `cargo test -p bbnf-bench real_typed -- --nocapture`,
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture`, and
   `cargo xtask check-real-typed`.

## Required Folds

None for CH4. The V1 blockers under this lane are folded: Apache root fields,
CITM keyed-entry semantics, checksum coverage, serde_json-as-Track2/oracle
wording, the sonic strict lane, and Canada exclusion all line up between the
W2 prose and typed source/product code.
