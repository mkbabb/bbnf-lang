# SK-V8 W2 Hardening V2 CH2

Verdict: ACCEPT

Confidence: 94%

Scope: reviewed current HEAD `8ce03af4` after the W2 V1 hardening fold, with
focus on the V1 CH2 blockers: measured `RESULTS.md` row posture, W2
source/product disposition, benchmark row-table rejection, oracle wording, and
REDRESS/HANDOFF/SPEC-plan consistency.

Checks run:

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- `cargo xtask gate-json --with-cost-facts --advisory --check-results`
- `git diff --check`
- `awk` row audits over `skinny/RESULTS.md`

## Findings

1. No measured `RESULTS.md` overclaim remains. `skinny/RESULTS.md` is unchanged
   in the reviewed fold, and the row audit finds exactly four measured
   `real_typed_struct` rows: `twitter`, `update_center`, `mesh`, and
   `marine_ik`. There are no measured `apache_builds/real_typed_struct` or
   `citm_catalog/real_typed_struct` rows. HANDOFF repeats the same measured
   state as four `A / GO` rows and explicitly says W2 does not claim six
   measured `real_typed_struct A / GO` rows
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:31-40`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-187`;
   `skinny/RESULTS.md:50`, `skinny/RESULTS.md:61`,
   `skinny/RESULTS.md:64`, `skinny/RESULTS.md:71`).

2. W2 disposition is now the intended split: source/product parity admitted,
   benchmark row-table admission rejected for this wave. REDRESS 91 says the
   admitted Apache/CITM rows are source/product rows absent from the current W0
   manifest, rejects `canada/real_typed_struct`, keeps `skinny/RESULTS.md`
   unchanged, and states that W2 admits source/product parity only
   (`skinny/REDRESS.md:2622-2652`). HANDOFF and the W2 plan/research mirror
   that posture (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-7`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:131-139`;
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:46-50`;
   `restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:55-58`).

3. The Track 2/oracle wording is no longer overclaimed. Code still has
   `track2_typed` delegate to `serde_typed`, while sonic-rs remains the separate
   strict lane (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251-323`).
   The W2 research, plan, HANDOFF, and REDRESS now describe this as
   serde_json-backed Track 2/oracle plus a separate sonic-rs parity lane, not
   Track 2 plus serde plus sonic as three independent engines.

4. The source/product fold matches the declared product schema. The schema and
   carriers consume Apache root `mode`, root `nodeName`, `jobs[].name`,
   `jobs[].url`, and `jobs[].color`; CITM consumes keyed `events` entries with
   `id`, `name`, `subTopicIds`, and `topicIds`
   (`skinny/xtask/src/real_typed_schema.rs:57-98`;
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:33-74`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:351-383`). The focused
   real-typed tests passed, including full Apache and CITM fixture parity.

5. The V1 gate/provenance folds landed. The real typed schema identity is now
   `sk-v8-real-typed-w2` in both schema source and generated output
   (`skinny/xtask/src/real_typed_schema.rs:7-10`;
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1-4`). Lock 14 now
   allows only `sk-v8-wave2` parent diffs confined to the three real typed owner
   paths and rejects W2-scoped parent diffs outside those paths
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399-470`). The focused
   `lock14_baseline` test suite passed.

6. SPEC/plan consistency is acceptable for this disposition. SPEC Section 5
   still defines the full W2 benchmark row-table admission gate, but the W2 plan
   explicitly provides the fail-closed route used here: keep `RESULTS.md`
   unchanged and reject benchmark row-table admission when the W0 validator
   rejects unrelated Criterion drift. REDRESS and HANDOFF choose that route
   rather than pretending the SPEC row-table exit gate is green.

## Required Folds

None for CH2. Preserve the current wording: W2 source/product parity admitted;
benchmark row-table admission rejected for this wave; current `RESULTS.md`
contains only the W0 four measured `real_typed_struct` rows.
