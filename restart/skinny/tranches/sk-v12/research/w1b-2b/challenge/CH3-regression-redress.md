# SK-V12 W1b-2b CH3 - Regression / REDRESS

Verdict: REVISE.

CH3 does not accept the packet as-is because the corrected REDRESS 125 route is
not yet consistently bound across the W1b-2b research set.

Blocking findings:

- REDRESS slot drift remains in `A1-report-schema.md`: it still requires
  `redress_entry == REDRESS-124` and describes `PASS-MEASURED-BASELINE` as
  REDRESS 124 evidence. Section 7.2 plus the current `skinny/REDRESS.md` tail
  make W1b-2a item 124 and leave W1b-2b as item 125.
- `A6-test-plan.md` still includes stale W1b-2 labels and says REDRESS 124 is
  the implementation-run outcome evidence. That conflicts with PLAN.md and A5,
  and it can seed tests that accept the wrong ledger entry or gate label.
- Because the stale references are in schema/test planning, this is a regression
  risk, not just wording. A W1b-2b implementation could pass its local tests
  while emitting or accepting the wrong REDRESS entry.

Accepted CH3 surfaces:

- `PLAN.md` correctly names REDRESS 125, requires
  `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`, and routes outcomes as
  `PASS-ADMIT-CANDIDATE`, `PASS-MEASURED-BASELINE`, or `BLOCKED/FAIL`.
- A5 correctly says `skinny/RESULTS.md` moves only for a real CSS admit
  candidate or for an accepted measured JSON guard demotion.
- The JSON guard contract is directionally correct: no write/probe flags,
  populated JSON Criterion root for guard proof, and CSS-only Criterion roots
  rejected.
- Stale-results guidance is correctly scoped in A2/A4: companion-only CSS
  measured-baseline evidence must not tell operators to rewrite
  `skinny/RESULTS.md`; JSON stale guidance belongs only to the JSON guard path.
- The revert protocol is sufficient if the implementation slice is rejected:
  revert only W1b-2b gate/report/result edits, preserve unrelated work, and save
  `/tmp/skv12-waveW1b-2b-rejected.patch`.

Required revision before ACCEPT:

1. Replace all W1b-2b REDRESS 124 references with REDRESS 125 in schema,
   tests, and outcome text.
2. Normalize gate labels to the Section 7.2 name:
   `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`.
3. Keep the `RESULTS.md` movement rule explicit: no movement for
   `PASS-MEASURED-BASELINE`; movement only for CSS `PASS-ADMIT-CANDIDATE` or
   accepted measured JSON guard demotion.
4. Keep the no-write JSON guard and stale-results guidance as currently stated
   in PLAN.md/A2/A4.
