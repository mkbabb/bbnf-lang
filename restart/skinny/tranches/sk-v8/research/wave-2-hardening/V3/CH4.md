# SK-V8 W2 Hardening V3 - CH4

Date: 2026-05-18.
Reviewer: CH4.
Target reviewed: `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

## Verdict

ACCEPT.

Confidence: 94%.

## Findings

1. Apache source/product fields line up. The W2 plan admits root `mode`, root
   `nodeName`, and `jobs[].name` / `jobs[].url` / `jobs[].color`
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:11-13`). The schema
   source declares exactly those fields on `ApacheBuilds` and `ApacheJob`
   (`skinny/xtask/src/real_typed_schema.rs:57-73`), the generated parser reads
   the same keys (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:169-256`),
   the serde carrier uses matching field names and `nodeName` rename
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:33-50`), and the checksum
   folds all admitted Apache fields (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:351-367`).

2. CITM remains a keyed-entry product row, not a value-only map. The W2 research
   and plan admit `events` as keyed entries with `id`, `name`, `subTopicIds`,
   and `topicIds` (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:27-30`;
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:14-16`). The schema
   uses `map_entries(..., "key", "value", 184, ty("CitmEvent"))`
   (`skinny/xtask/src/real_typed_schema.rs:75-98`), generated DirectBuild stores
   each object key with its parsed event value
   (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:797-814`), and the
   serde visitor preserves the same `CitmEventEntry { key, value }` shape
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:547-574`).

3. The checksum lane covers the admitted product surface. Apache folds root
   strings, job count, and selected job strings; CITM folds event count, event
   key, `id`, `name`, `subTopicIds`, and `topicIds`
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:351-384`). The parity
   assertion compares Track 1, Track 2, serde_json, and sonic-rs checksums
   directly (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:310-323`), so
   the keyed CITM shape and Apache root fields are product-visible.

4. The serde_json-as-oracle wording is now correct. W2 prose says the proof is
   generated Track 1 plus serde_json as the Track 2/oracle path, with a separate
   sonic-rs typed parity lane, and explicitly avoids claiming a third independent
   typed parser (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:11-15`;
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:41-43`). The code
   agrees: `track2_typed` delegates to `serde_typed`, and `serde_typed` calls
   `serde_json::from_slice` for the real typed carriers
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251-281`).

5. The sonic lane is strict and separate. The W2 gate names a separate sonic-rs
   strict lane (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:41-43`).
   The implementation calls `sonic_rs::from_slice` independently for each typed
   fixture and compares its checksum against Track 1, Track 2, and serde_json
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:284-323`). The bench crate
   depends on `sonic-rs` with `default-features = false` and only `sort_keys`,
   so the typed proof is not using the lossy sonic comparator path
   (`skinny/crates/bbnf-bench/Cargo.toml:20-21`).

6. Canada remains excluded from the W2 typed source/product admission. The
   fixture enum and W2 full-fixture test cover Apache and CITM, not Canada
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:9-17`;
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:609-618`). W2 research,
   REDRESS, and HANDOFF all route Canada out after full-fixture DirectBuild-vs-
   serde checksum mismatch on long decimal coordinates, without weakening typed
   equality (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:23-26`;
   `skinny/REDRESS.md:2637-2640`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-187`).

7. Focused V3 recheck passed on the unchanged V2-folded packet:
   `cargo xtask check-real-typed` and
   `cargo test -p bbnf-bench real_typed -- --nocapture`. The test suite passed
   all seven real typed tests, including minimal Apache/CITM sidecar parity and
   `w2_full_real_typed_fixtures_match_sidecars`.

## Required Folds

None. The V2 folds remain intact for CH4: Apache fields, CITM keyed entries,
checksum coverage, serde_json-as-oracle wording, the strict sonic lane, and
Canada exclusion are consistent between W2 prose and typed source/product code.
