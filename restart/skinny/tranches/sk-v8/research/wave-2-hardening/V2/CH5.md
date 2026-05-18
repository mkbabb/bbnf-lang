# SK-V8 W2 Hardening V2 CH5 Review

Date: 2026-05-18.
Target reviewed: `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).
Lens: CH5 hidden coupling and governance discipline.

## Verdict

ACCEPT.

Confidence: 93%.

## Findings

1. Triumvirate separation is sufficient for W2 disposition. The live history has
   separate W2 research, plan, Canada-routing plan fold, source implementation,
   and post-hardening disposition commits (`eacba76a`, `9923b804`, `6b4f46ae`,
   `12aff1e4`, `8ce03af4`). Current HANDOFF also restates the separation rule:
   research, plan, and redress are distinct, with no role merger and no wave
   close without REDRESS or explicit no-source telemetry close
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:256`). The V1 hardening archive
   landing in the V2 fold commit is untidy, but it does not merge W2 research,
   plan, and redress authority or hide a source change outside the typed slice.

2. The V1 consolidation was folded. V1 required Lock 14 parent-diff accounting,
   schema identity, Apache root fields, Track 2/oracle wording, REDRESS 91, and
   no RESULTS row-table claim
   (`restart/skinny/tranches/sk-v8/research/wave-2-hardening/V1/HARDENING-W2-V1-CONSOLIDATED.md:49`).
   Current HEAD binds the W2 owner-path exception to the three real typed paths
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`), normalizes git paths
   and rejects non-owner parent diffs (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:425`),
   bumps the generated schema identity to `sk-v8-real-typed-w2`
   (`skinny/xtask/src/real_typed_schema.rs:10`;
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:3`), and names Apache
   root `mode` / `nodeName` in the plan and research
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:11`;
   `restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:19`).

3. REDRESS 91 is an honest W2 disposition rather than a deferred benchmark
   admission. It admits only the source/product rows, explicitly says those rows
   are not present as measured rows in the W0 RESULTS manifest
   (`skinny/REDRESS.md:2622`), rejects `canada/real_typed_struct` on full-fixture
   DirectBuild-vs-serde checksum mismatch (`skinny/REDRESS.md:2637`), records the
   completed source verification (`skinny/REDRESS.md:2641`), and rejects W2
   benchmark row-table admission with `skinny/RESULTS.md` unchanged
   (`skinny/REDRESS.md:2648`). Current RESULTS still has only the W0 measured
   `real_typed_struct` rows for `twitter`, `update_center`, `mesh`, and
   `marine_ik` (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
   `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).

4. No CH5 hidden-coupling defect was found. W2 stays on existing typed
   DirectBuild product-plane consumers and does not introduce a directive, BIR
   variant, `BackendShape`, substrate surface, sidecar, parser-owned cursor,
   runtime JSON behavior, or direct-digest product claim
   (`skinny/REDRESS.md:2626`). `track2_typed` delegates to serde_json, while
   sonic-rs remains a separate parity lane
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:284`); the V2 docs no
   longer overclaim a third independent typed parser
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:11`).
   An off-scope diff audit from `12aff1e4^..HEAD` found no changes to
   `RESULTS.md`, runtime, IR, passes, codegen, grammar, bbnf, SIMD,
   parse-that-regex, Track 2, direct-struct, parity, scan, or materialization
   surfaces.

5. W2 can hand off to W3 without carrying hidden W2 work. HANDOFF records W2 as
   source/product parity admitted, benchmark row-table admission rejected, and W3
   as the next dispatchable wave only after its own research, plan, challenge,
   and redress gate (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`;
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175`). SPEC keeps W3 separately
   gated on a fresh plan naming exact files, rows, same-wave consumer, revert
   protocol, measured-path proof, Lock 1 fork, and pre-block differences
   (`restart/skinny/tranches/sk-v8/SPEC.md:525`), and forbids automatic W3
   implementation dispatch from S-P2 or S-P3 alone
   (`restart/skinny/tranches/sk-v8/SPEC.md:803`).

6. Focused verification passed for the highest-risk governance fold:
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 10
   `lock14_baseline` tests on current HEAD.

## Required Folds

None.

V2 does not need another CH5 fold. Preserve the current REDRESS 91 split:
source/product parity is admitted, `canada/real_typed_struct` is rejected for
W2, benchmark row-table admission is rejected for this wave, and W3 must start
from its own plan/challenge gate rather than inheriting any W2 row-table work.
