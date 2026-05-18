# SK-V8 W2 Hardening V3 CH3

Reviewed target: `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

Verdict: REVISE

Confidence: 90%

## Findings

1. The core W2 source/product gates are green on the unchanged V2-folded
   packet. I reran `cargo test -p bbnf-bench lock14_baseline`,
   `cargo xtask check-real-typed`,
   `cargo test -p bbnf-bench real_typed -- --nocapture`,
   `cargo xtask check-json`, and `cargo xtask check-conformance`. Lock 14
   passed 10 tests, including W2 owner admission/rejection and root-path
   normalization. `check-real-typed` passed. The filtered real typed suite
   passed all seven tests, including Apache/CITM sidecar parity and
   `w2_full_real_typed_fixtures_match_sidecars`. Conformance accepted 21 valid
   fixtures and rejected 7 invalid fixtures. The supporting
   `cargo test -p codegen typed_direct -- --nocapture` test also passed.

2. The CostFacts `gate-json` path is green with result checking. I ran
   `cargo xtask gate-json --with-cost-facts --advisory --check-results`; it
   emitted schema `sk-v8-costfacts-v1`, `wave_id` `SK-V8-W1`, grammar `json`,
   15 manifest rows, and zero diagnostics. The W0 `RESULTS.md` snapshot markers
   required by that path were accepted.

3. The standard checked report gate is not green. I ran
   `cargo xtask gate-json --advisory --check-results` from `skinny/`; it failed
   before rendering with:
   `citm_catalog metadata invalid: missing coherent metadata for track1_real_typed_struct`.
   This is not the REDRESS-described W0 run-id validator failure. It is a
   deterministic mismatch between the V2 source-only typed fixture expansion
   and the checked report gate: `gate.rs` now treats every
   `fixture_for_name(..).is_some()` fixture as requiring
   `track1_real_typed_struct`, `track2_real_typed_struct`,
   `sonic_rs_real_typed_struct`, and `serde_json_real_typed_struct` Criterion
   metadata, but the current Criterion tree has no Apache/CITM real typed
   directories and `skinny/RESULTS.md` intentionally has no Apache/CITM measured
   `real_typed_struct` rows.

4. The row-table non-admission posture itself is visible and unchanged.
   `git diff --exit-code HEAD^ HEAD -- skinny/RESULTS.md` is empty, and
   `RESULTS.md` still contains exactly four measured `real_typed_struct` rows:
   `twitter`, `update_center`, `mesh`, and `marine_ik`. It contains no measured
   `apache_builds/real_typed_struct`, `citm_catalog/real_typed_struct`, or
   `canada/real_typed_struct` rows.

5. Off-scope diff is clean. A targeted `6b4f46ae..HEAD` diff over grammar,
   fixtures, runtime, IR, passes, codegen, grammar crate, bbnf crate, SIMD,
   parse-that-regex, direct struct, Track 2, parity, scan, materialization, and
   `skinny/RESULTS.md` is empty. The W2 source movement is confined to the typed
   owner paths plus Lock 14 gate hardening and W2 disposition docs. `git diff
   --check 6b4f46ae..HEAD` and `git diff --check HEAD^ HEAD` both passed.

## Required Folds

1. Reconcile standard `gate-json --advisory --check-results` with the W2
   source-only typed fixture expansion. Either make the checked report gate
   derive real-typed benchmark requirements from admitted/measured row-table
   state rather than `fixture_for_name(..)`, or add an explicit W2 source-only
   exclusion so Apache/CITM source/product parity does not require unadmitted
   Criterion `real_typed_struct` rows. Add a regression test for unchanged W0
   `RESULTS.md` plus Apache/CITM source-only typed fixtures.

2. Correct the W2 disposition text to match the executable failure mode. The
   current failure is missing coherent Apache/CITM `real_typed_struct` metadata,
   not W0 run-id drift. If standard `gate-json --check-results` is intentionally
   out of scope for source-only W2, say that explicitly; otherwise make it pass
   before the packet is accepted.
