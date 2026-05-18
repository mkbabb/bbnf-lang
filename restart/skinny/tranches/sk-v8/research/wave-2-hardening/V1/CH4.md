# CH4 - SK-V8 W2 Product-Plane Hardening Review

Reviewed commit: `12aff1e4` (`feat(sk-v8-wave2-typed): add Apache and CITM typed product rows`).

Verdict: REVISE

Confidence: 90%

## Findings

1. The W2 data-plane implementation is directionally correct. The source slice
   adds `apache_builds` and `citm_catalog` only through the existing real-typed
   schema/generator/carrier path, with generated roots
   `parse_apache_builds` and `parse_citm_catalog`. The commit does not add a
   Canada typed fixture, directive, BIR variant, runtime JSON behavior,
   substrate surface, sidecar, direct-digest parser, or parser-owned cursor.
   Evidence: `skinny/xtask/src/real_typed_schema.rs:22`,
   `skinny/xtask/src/real_typed_schema.rs:57`,
   `skinny/xtask/src/real_typed_schema.rs:75`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:10`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:53`.

2. Full-fixture parity exists for the two new W2 rows and is green. The unit
   test loads the real `apache_builds` and `citm_catalog` fixture payloads,
   runs generated Track 1, Track 2, serde, and sonic, and compares the typed
   checksum. I reran `cargo test -p bbnf-bench real_typed -- --nocapture`; all
   seven real-typed tests passed, including
   `w2_full_real_typed_fixtures_match_sidecars`. Evidence:
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:610`.

3. The W2 checksums cover the declared product fields. Apache folds root
   `mode`, `nodeName`, job count, and each job's `name`, `url`, and `color`.
   CITM folds event count, event key, `id`, `name`, `subTopicIds`, and
   `topicIds`. Unknown fields are deliberately skipped by the schema, so they
   are not part of the product-plane equality proof. Evidence:
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:351`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:362`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:369`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:378`.

4. Canada is correctly rejected from this W2 slice. The new CITM schema stays on
   strings, `u64`, and `Vec<u64>` product data; Apache stays on strings. That
   avoids the rejected Canada long-decimal `f64` materialization path. Existing
   `mesh` and `marine_ik` still have `f64` vectors, but they predate W2 and are
   not evidence that Canada was admitted. Evidence:
   `skinny/xtask/src/real_typed_schema.rs:57`,
   `skinny/xtask/src/real_typed_schema.rs:75`,
   `skinny/xtask/src/real_typed_schema.rs:93`,
   `skinny/xtask/src/real_typed_schema.rs:96`,
   `skinny/xtask/src/real_typed_schema.rs:159`.

5. The parity wording overcounts the independent oracles. `track2_typed` is
   implemented as `serde_typed(fixture, input.as_bytes())`, then
   `assert_real_typed_parity` separately calls `serde_typed` again. That means
   the strict parser engines are generated Track 1, serde_json, and sonic-rs;
   Track 2 is not an additional independent implementation beyond serde.
   Evidence: `skinny/crates/bbnf-bench/src/real_typed_struct.rs:251`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:258`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:310`.

6. No strict-vs-strict row-table admission is present at HEAD. `skinny/RESULTS.md`
   still has four `real_typed_struct` rows and no
   `apache_builds/real_typed_struct` or `citm_catalog/real_typed_struct`
   results. Existing generated rows carry `Strictness=deferred`, while sonic and
   serde comparators are strict and same output plane (`typed direct`) only for
   existing measured rows. W2 can claim source/product checksum parity, not new
   measured `A / GO` strict-vs-strict rows, until a clean RESULTS refresh lands.
   Evidence: `skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
   `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`,
   `skinny/crates/bbnf-bench/src/metadata.rs:196`,
   `skinny/crates/bbnf-bench/src/metadata.rs:460`.

7. The broader gate is not green because Lock 14 still treats the W2 typed-owner
   files as frozen. I reran
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture`; it failed
   `accepts_current_allowlist` with `Lock 14 frozen diff failed` over
   `real_typed_struct.rs`, `generated_real_typed.rs`, and
   `xtask/src/real_typed_schema.rs`. That is wave-authorized product movement,
   not parser/substrate drift, but the gate has not been taught that distinction.

## Verification

- PASS: `cargo xtask check-real-typed`
- PASS: `cargo test -p bbnf-bench real_typed -- --nocapture`
- PASS: `cargo xtask check-json`
- PASS: `cargo xtask check-conformance`
- PASS: `cargo test -p codegen typed_direct -- --nocapture`
- PASS: `git diff --check 9923b804..HEAD -- skinny/xtask/src/real_typed_schema.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- FAIL: `cargo test -p bbnf-bench lock14_baseline -- --nocapture`

## Required Folds

1. Fold Lock 14 accounting so W2-authorized real-typed product files can move
   while parser/runtime/tape/substrate/direct-digest/materialization surfaces
   remain frozen. The focused Lock 14 test, or a named W2 replacement gate, must
   pass before W2 is called closed.

2. Fold the strict/output-plane wording. Until a measured `RESULTS.md` refresh
   adds Apache and CITM real-typed rows, closure text must say W2 admitted
   source/product checksum parity only. Do not count the repo as having six
   measured `real_typed_struct A / GO` rows, and do not call the new rows
   strict-vs-strict performance wins.

3. Fold the Track 2/oracle wording. Either implement a genuinely separate typed
   Track 2 oracle, or state plainly that serde_json is the Track 2/oracle path
   and that the distinct strict engines are generated Track 1, serde_json, and
   sonic-rs.

4. Preserve the Canada route-out. Any follow-up must keep Canada out of W2
   unless a separate f64 materialization fix passes full-fixture parity without
   weakening typed equality to length-only or digest-only evidence.
