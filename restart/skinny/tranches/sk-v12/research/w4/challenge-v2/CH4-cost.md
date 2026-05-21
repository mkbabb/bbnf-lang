# SK-V12 W4 CHALLENGE V2 - CH4 Cost

Verdict: REVISE.

PLAN-V2 is still too expensive for the 30-minute W4 redress cap as written.

Findings:

1. The reject branch still requires release checkasm, bench tests, Lock 14,
   JSON gate, and AWK; the PASS branch adds codegen/runtime tests, full
   Criterion, and a new W4 gate path. That is not credible inside one
   30-minute redress unless the plan splits the work.

2. Report/gate work is underbudgeted. The existing CSS SOTA validator is
   W1b-2b/REDRESS-125-only; W4 needs a real schema/CLI/test branch if
   production wiring passes microbench.

3. LOC/generation budget needs a split. The production CSS hook is narrow, but
   caller parity, microbench JSON emission, report/gate schema, CLI tests,
   orphan disposition, and REDRESS evidence likely exceed the 430 hand/test/gate
   budget unless PLAN-V3 caps each class or routes production PASS to a
   follow-up after microbench PASS.

4. Measured reject is highly likely. The CSS fixture is 187 bytes and
   fact-stream construction can hide or reverse a 64-byte classifier win.

PLAN-V3 must define a cheaper microbench-reject path and explicit split rules
for the rare production PASS branch.
