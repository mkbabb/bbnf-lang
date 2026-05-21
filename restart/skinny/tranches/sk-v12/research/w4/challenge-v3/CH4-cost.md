# SK-V12 W4 CHALLENGE V3 - CH4 Cost

Verdict: REVISE.

PLAN-V3 splits production admission from the default reject path, but the
default verification bundle is still larger than the touched surface and not
fully executable from the repository root.

## Findings

1. The default branch lists three release checkasm commands, a Lock 14 run, JSON
   gate work, and AWK proof. For a branch that edits only a new `bbnf-simd`
   caller test, W4 research evidence, and REDRESS, that command set is too broad
   for the 30-minute redress cap. PLAN-V4 must trim default verification to the
   touched surface and use a no-touch proof for JSON/report/gate roots.

2. Commands must be root-executable. Use `cargo --manifest-path
   skinny/Cargo.toml ...` or explicitly `cd skinny` before cargo commands.

3. The microbench JSON producer is not named. PLAN-V4 must name the command
   that emits `w4-delimiter-find-microbench.json`, including the output
   environment variable.

4. The default source/test slice lacks a LOC cap. PLAN-V4 must cap the new
   caller checkasm and microbench writer so redress cannot accidentally grow
   into a hidden production implementation.

The rare microbench-pass production branch may keep the larger verification
surface, but only as a routed follow-up or a separately accepted production
split. It cannot be part of the default measured-reject branch.
