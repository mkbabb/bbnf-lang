# SK-V12 W4 CHALLENGE V1 - CH4 Cost

Verdict: REVISE.

The selected direction is plausible, but PLAN-V1 does not fit the 30-minute
redress cap as written.

## Findings

1. PLAN-V1 mixes two caller shapes: delimiter member-find for `scan_block` and
   layout member-skip for `skip_ws_and_comments`. Redress needs one scalar
   reference and one generated caller contract.

2. Gate/report source edits are not credible inside the same small slice unless
   tightly bounded. Current CSS SOTA validation is hard-coded to W1b-2b /
   REDRESS-125 and `lock16_status=n/a:no_simd_or_asm_claim`; making that
   W4-aware is not a minor side edit.

3. Command cost is undercounted. PLAN-V1 lists checkasm, bbnf-bench tests,
   Lock 14, full Criterion, CSS gate, JSON gate, and AWK. A5 adds dedicated
   caller checkasm/runtime/corpus commands. PLAN-V2 needs a minimal command
   set that still satisfies Lock 16 and JSON guard discipline.

4. LOC budget only works for a minimal generated scanner change. The current
   generated CSS module is already close to the generated-size guard; optional
   scanner hooks, broad report/gate schemas, or multiple caller routes risk
   exceeding the budget.

5. The microbench win is high risk. The CSS fixture is 187 bytes, and the full
   benchmark measures fact-stream construction, not only delimiter search.
   PLAN-V2 should treat `MEASURED-REJECT` as likely if the microbench misses.

Recommended revision: pick one caller, keep report/gate edits minimal but
W4-current, bind redress to micro-prove-first plus strict equality/full bench,
and stop at measured reject if the isolated microbench misses.
