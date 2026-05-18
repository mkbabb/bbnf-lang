# SK-V8 W2 Hardening V1 CH6 - Anti-Paper-Close

Verdict: REVISE.

Confidence: 92%.

Target reviewed: `12aff1e4` (`feat(sk-v8-wave2-typed): add Apache and CITM typed product rows`), plus the current uncommitted W2 status text in `HANDOFF.md` and `REDRESS.md`.

## Findings

1. **Source/product parity admission is credible, but full W2 closure is overclaimed.**

   The admitted source slice is narrow: commit `12aff1e4` touches only `skinny/xtask/src/real_typed_schema.rs`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, and `skinny/crates/bbnf-bench/src/generated_real_typed.rs`. The W2 plan also limits source to those files and says no parser/runtime/tape/direct-digest/substrate source is in scope (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:18-25`). Focused checks passed in this review:

   - `env CARGO_TARGET_DIR=/tmp/skv8-w2-ch6-target cargo test -p bbnf-bench real_typed -- --nocapture`: 7 passed, including `w2_full_real_typed_fixtures_match_sidecars`.
   - `env CARGO_TARGET_DIR=/tmp/skv8-w2-ch6-target cargo xtask check-real-typed`: passed.
   - `env CARGO_TARGET_DIR=/tmp/skv8-w2-ch6-target cargo test -p codegen typed_direct -- --nocapture`: 1 passed.
   - `env CARGO_TARGET_DIR=/tmp/skv8-w2-ch6-target cargo xtask check-json`: passed.
   - `env CARGO_TARGET_DIR=/tmp/skv8-w2-ch6-target cargo xtask check-conformance`: passed, 21 valid accepted and 7 invalid rejected.
   - `git diff --exit-code HEAD^..HEAD -- <runtime/parser/SIMD/codegen/direct/track2/parity/scan/materialization/RESULTS frozen surfaces>`: passed.

   That supports admitting the source/product parity slice. It does not support saying W2 is closed under SPEC Section 5. The SPEC W2 exit gate requires at least two new generated typed rows to pass their declared same-plane gate, existing typed GO rows to maintain floors, existing direct GO rows to maintain GO, and every non-target row to stay within the `SK-V8-open` maintain budget (`restart/skinny/tranches/sk-v8/SPEC.md:467-479`). Current `skinny/RESULTS.md` still has only four manifest `real_typed_struct` rows, all pre-W2: twitter, update_center, mesh, and marine_ik (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`). A row audit in this review reported `manifest_rows=38 real_typed_rows=4`.

2. **The W0 run-id validator is correctly hostile, so W2 cannot smuggle benchmark closure through stale RESULTS.**

   W0 V12 deliberately binds row telemetry to the captured Criterion fingerprint and fails admitted-row Criterion mutation on run-id drift (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/CH6.md:110-121`). W2's own plan accounts for that: if a W2 benchmark refresh is attempted and the standard W0 validator rejects run-id or unrelated throughput drift, W2 must keep `RESULTS.md` unchanged and route the benchmark surface explicitly (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:45-48`). Current REDRESS says exactly that `skinny/RESULTS.md` is unchanged and no benchmark/report refresh was admitted because local Criterion metadata was already known to trip the W0 run-id validator (`skinny/REDRESS.md:2646-2650`).

   This is the right fail-closed posture for stale Criterion data, but it has a downstream consequence: the W2 source commit can be admitted only as source/product parity until a clean measured refresh or an explicit gate revision adds the new rows. It cannot simultaneously avoid RESULTS refresh because the validator rejects it and claim benchmark-row closure.

3. **Current status/doc text has a hard mismatch.**

   `HANDOFF.md` says SK-V8 W0, W1, and W2 are closed and W3 is next (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-8`), and later says W2 is closed by `12aff1e4` with W2 hardening V1 recording challenge disposition (`restart/skinny/tranches/sk-v8/HANDOFF.md:130-135`). A lane artifact is not a consolidated V1 disposition; until all lanes and a consolidation exist, the handoff cites a challenge record before it exists.

   The status mismatch is reinforced by REDRESS item 91 saying the admitted rows are `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` (`skinny/REDRESS.md:2622-2624`) while `skinny/RESULTS.md` has no `apache_builds/real_typed_struct` or `citm_catalog/real_typed_struct` row. The existing REDRESS caveat that RESULTS is unchanged is good, but the word "rows" is ambiguous enough to be read as benchmark-row admission.

4. **The generated schema identity still says SK-V7 after the W2 row-set expansion.**

   The schema source and generated output both retain `sk-v7-real-typed-v2` after adding Apache and CITM roots (`skinny/xtask/src/real_typed_schema.rs:10`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:3`). If that string is a generator ABI/version marker, it needs an explicit comment saying it is not the row-set identity. If it is intended to identify the generated schema set, it should be bumped for W2. Leaving it unchanged while calling W2 closed creates a traceability footgun.

## Required Folds

1. Replace W2 close language with source-slice language until the row-table gap is resolved. Acceptable wording: "W2 source/product parity slice admitted; benchmark row-table admission routed." Do not say "W2 closed" or "W3 active" from W2 unless the remaining folds below land.

2. Add a W2 hardening V1 consolidation only after all CH lanes exist. Until then, remove or soften the `HANDOFF.md` claim that "W2 hardening V1 records challenge disposition."

3. Clarify REDRESS item 91 so "admitted rows" means generated typed source/product rows, not current `skinny/RESULTS.md` benchmark rows. State explicitly that `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` are absent from the current W0 manifest.

4. Choose one benchmark posture:

   - Run a clean W2 measurement path that the W0 run-id validator accepts, update `RESULTS.md` with the new typed rows, and prove SPEC Section 5 maintain/guard floors; or
   - Revise the W2 plan/SPEC fold to make this W2 intentionally source-only, with the row-table refresh routed to a later wave and no wave-close claim.

5. Bump the generated typed schema hash for W2, or document that `schema_hash` is generator ABI identity rather than row-set identity.

## Residual

No CH6 blocker was found in the actual typed parser/source parity slice. The blocker is closure posture: current docs blur source admission, benchmark admission, and challenge closure in a way the W0 run-id validator was designed to prevent.
