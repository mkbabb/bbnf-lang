# SK-V8 W2 Hardening V1 CH5: Plan/Research/Redress Discipline

Date: 2026-05-18.
Lens: CH5 W2 closure discipline.
Scope: commit `12aff1e4`, W2 research/plan docs, current W2 REDRESS entry,
and `skinny/RESULTS.md` disposition.

## Verdict

ACCEPT.

Confidence: 91%.

## Findings

1. Plan and research discipline is satisfied. W2 is scoped as a typed
   product-plane wave only: generated Track 1 typed DirectBuild plus
   independent Track 2, serde, and sonic typed parity. The research keeps
   parser/runtime substrate, direct digest guard rows, generic CostFacts, and
   Lock 14 roots out of scope unless separately named and challenged
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:8`).

2. Commit `12aff1e4` matches the W2 source scope. The diff is limited to
   `skinny/xtask/src/real_typed_schema.rs`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, and generated
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, which are the source
   paths allowed by the plan (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:18`).
   The commit adds `apache_builds` and `citm_catalog` schemas, generated typed
   parsers, typed sidecars, checksums, minimal parity tests, and full-fixture
   parity tests without touching parser/runtime/tape/direct-digest substrate.

3. Canada routing does need a REDRESS entry, and the current redress ledger has
   it. The research says `canada` was falsified during W2 pre-redress and is
   routed out instead of weakening typed equality to length-only proof
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:20`).
   The same research says `skinny/REDRESS.md` is in scope when a W2 row rejects
   or routes (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:39`).
   Current `skinny/REDRESS.md` Item 91 records the admitted source slice, rejects
   `canada/real_typed_struct`, names the DirectBuild-versus-serde checksum
   mismatch on long decimal coordinate payloads, and blocks length-only or
   digest-only evidence (`skinny/REDRESS.md:2620`).

4. `skinny/RESULTS.md` can remain unchanged for W2 close. The plan permits
   leaving RESULTS unchanged when a benchmark refresh is not admitted or is
   blocked by W0 validator/run-id drift unrelated to W2 source
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:45`). The current
   REDRESS entry states W2 admits source/product parity only, did not admit a
   benchmark/report refresh, and leaves row-table performance status to a later
   clean measured refresh (`skinny/REDRESS.md:2646`). Current RESULTS still has
   no `apache_builds/real_typed_struct` or `citm_catalog/real_typed_struct`
   row, which is acceptable because W2 did not claim a refreshed row-table
   performance admission.

5. No hidden coupling defect found. The admitted rows are product-plane typed
   consumers, not parse-only or direct-digest close claims. `apache_builds`
   stays on root/job strings; `citm_catalog` stays on keyed event entries with
   `id`, `name`, `subTopicIds`, and `topicIds`; and the source slice adds no
   directive, BIR variant, `BackendShape`, substrate surface, sidecar, or
   parser-owned cursor.

## Required Folds

None.

Closure is acceptable only with current REDRESS Item 91 retained. If that
redress entry is dropped or W2 closure is represented only by commit `12aff1e4`,
CH5 would flip to REVISE until the Canada route and no-RESULTS-refresh status
are restored in `skinny/REDRESS.md`.
