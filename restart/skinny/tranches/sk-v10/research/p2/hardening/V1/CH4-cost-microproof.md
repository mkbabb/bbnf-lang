# SK-V10 S-P2 V1 CH4: Cost / Micro-Proof

Date: 2026-05-19.
Lens: CH4 cost, scalar-reference burden, checkasm burden, same-wave consumer,
and micro-prove-first readiness.
Scope: S-P2 cohort `p2a` through `p2f`, S-P1 `p1c` / `p1e` / `p1f`,
`bbnf-simd` checkasm tests, and the SK-V10 micro-prove-first contract.
Output: this file.

## Disposition

REVISE.

The S-P2 V1 cohort has the right proof posture, but not yet the cost ledger
S-P3 needs. P2-B states the scalar-oracle-first admission process and P2-C/P2-E
name plausible scalar references and consumers. The missing piece is a single
normalized ledger that turns those prose/table entries into per-candidate
micro-proof obligations: scalar oracle, checkasm file, target host/feature,
representative slices, same-wave consumer, benchmark harness, failure threshold,
and <=90-minute wave split.

This is not a REJECT because the core gates are present and most risky candidates
are already downgraded, blocked, or conditioned. It is not ACCEPT because S-P3
cannot safely scope kernel implementation waves from V1 alone.

## Contract Checked

- PASS-2 CH4 requires every candidate to carry scalar-reference status,
  checkasm-parity expectation, and same-wave-consumer note; a candidate missing
  any of the three fails CH4
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:119-124`).
- SK-V10 synthesis blocks any substrate/kernel wave from S-P3 wave-scoping until
  an isolated same-host micro-benchmark proves the primitive or call-site change
  and names scalar reference, host flags, representative slices, consumer, and
  failure threshold (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:143-153`).
- P2-B repeats the correct admission order: P1 antecedent, scalar oracle,
  checkasm differential, feature gate, same-wave consumer, and primitive plus
  caller micro-benchmark before integration
  (`restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md:143-160`).

## Findings

1. Proof discipline is present but scattered.

   P2-A gives scalar/checkasm/consumer notes for SOTA-shaped primitives, including
   string, unicode, number, transient structural classify, and direct sink
   contract candidates (`p2a-sota-teardown.md:31-35`). P2-C has the strongest
   candidate table because it includes scalar-ref status, checkasm status, and
   same-wave consumer per architecture candidate (`p2c-arch-esoterica.md:25-47`).
   P2-E gives scalar sketches and same-wave consumers for parse-that gaps, but
   does not include an explicit checkasm expectation column for each row
   (`p2e-parse-that-gaps.md:28-36`). P2-F repeats scalar/reference posture and
   grammar-neutrality, but likewise does not normalize checkasm/microbench
   burden for S-P3 (`p2f-grammar-neutral.md:24-33`).

2. Primitive-only speedups are correctly treated as insufficient.

   P1-C shows SIMD structural scan beats scalar on every corpus, but explicitly
   says this does not reopen W3 and that future kernels must micro-prove both
   the primitive and the product-plane caller (`p1c-samply-mode-3.md:111-116`).
   P1-E likewise says `simd_movemask` on `gsoc-2018` is a measured hot leaf, not
   a kernel authorization (`p1e-hot-leaf-attribution.md:89-92`). This supports
   V1's rejection/demotion of structural cursor, PMULL/CTZ default rewires, and
   sidecar routes.

3. The checkasm harness is cheap enough, but every new primitive still pays real
   proof LOC.

   Existing checkasm has deterministic inputs, scalar-vs-candidate comparison,
   mutation/clobber checks, alignment sweep, stack canary, signal trapping, and
   robust benchmark support (`CHECKASM-REPORT.md:41-63`). Reported warm cost is
   about 6.3s including the robust benchmark, with the core parity cells in the
   millisecond range (`CHECKASM-REPORT.md:128-140`). The current
   `byte_class_from_eq_set_64` harness demonstrates the right shape: scalar
   reference as executable specification, alignment sweep, set-size sweep,
   corpus parity, and edge cases
   (`tests/checkasm_byte_class_from_eq_set_64.rs:14-17`,
   `:189-205`, `:224-242`, `:294-306`, `:376-386`).

   Cost risk remains nontrivial because each new admitted primitive needs its
   own scalar oracle, dedicated checkasm cell, target-host feature gate, and
   product/caller microbench. V1 does not estimate that LOC by candidate.

4. S-P3 cannot yet scope all kernel waves to <=90 minutes.

   Contract and telemetry work can fit <=90-minute slices. A proof-only slice for
   one small primitive can also fit if it is limited to scalar oracle, checkasm,
   and Criterion microbench. A combined "unicode/string kernel pair" or
   "number primitive plus production caller" is too broad for a single
   <=90-minute wave unless split after a passing micro-proof.

5. Several rows need explicit non-admission labels.

   The cohort mostly says this already, but CH4 needs it in one ledger:
   `number_digit_run_classify_64` is research-only until a concrete direct/typed
   numeric consumer is proven (`p2b-dav1d-process.md:167`);
   `whitespace_skip_mask_64` is maintain-only unless paired with a current
   caller (`p2b-dav1d-process.md:168`);
   structural movemask is not a product wave unless the consumer is typed-root or
   direct-output work (`p2b-dav1d-process.md:169`);
   mask-next/bulk-emission and CTZ/PMULL defaults remain blocked by REDRESS
   (`p2c-arch-esoterica.md:33-34`);
   `structural_cursor_from_movemask` is correctly a non-candidate
   (`p2e-parse-that-gaps.md:38`).

## Required Fixes For V2

1. Add a normalized micro-proof ledger covering every unique P2 candidate family:
   direct contract, typed/root contract, tiny string, full string, unicode escape
   x4, string segment fold, digit/number, whitespace, byte-class/movemask,
   tape/flag economy, telemetry, and all rejected/research-only primitives.

2. Each ledger row must include:
   scalar oracle path or exact scalar sketch; checkasm status and target test file
   to add or reuse; target host/feature flags; representative input slices;
   benchmark harness; same-wave consumer; failure threshold; and admission
   disposition.

3. Add preliminary LOC/minute bands per ledger row. The bands may be marked
   "S-P3 tightens", but V2 must distinguish:
   proof-only <=90-minute slices, integration slices that require a prior
   passing proof, and candidates that are too broad or blocked.

4. Split bundled kernel candidates before S-P3:
   tiny-string proof, full-string proof, unicode escape proof, digit-run proof,
   whitespace proof, and production caller wiring are separate slices. No
   single S-P3 wave may combine scalar oracle, checkasm, microbench, and multiple
   production consumers for more than one primitive family.

5. Mark x86 scaffold-only routes as secondary/non-row-moving for SK-V10 on the
   Apple aarch64 host unless a future same-host x86 run is explicitly in scope.

6. Keep product-level contracts out of bbnf-simd checkasm. Direct output,
   typed-root generalization, `instruments` typed admission, tape economy, and
   telemetry need scalar/product oracles and gate tests, not checkasm, unless
   they invoke a SIMD leaf.

## S-P3 Boundary After Fix

With the ledger added, S-P3 may scope <=90-minute waves for contract/gate work
and for one proof-only primitive at a time. S-P3 may not scope a substrate or
kernel implementation wave from V1 P2 alone. The implementation boundary remains:
micro-proof first, same-wave consumer second, product-row movement last.

## Sources

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
