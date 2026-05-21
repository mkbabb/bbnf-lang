# SK-V12 W4 CHALLENGE V4 - CH4 Cost

Verdict: ACCEPT.

No cost blocker remains.

PLAN-V4 makes the default path realistic for the 30-minute W4 redress cap by
limiting implementation to one new caller checkasm/microbench test, one JSON
artifact, orphan disposition, and `REDRESS.md`. Production, gate, report,
Lock 14, generated code, and `RESULTS.md` edits are all capped at zero for the
default branch.

The required command is root-executable with
`cargo --manifest-path skinny/Cargo.toml ...`, the microbench artifact producer
is named with `SKV12_W4_MICROBENCH_OUT`, and touched-path verification is
scoped to the new test plus a JSON/report/gate/Lock 14/RESULTS no-touch proof.
The new caller test and microbench writer carry an explicit `<= 220` physical
line cap, inside the W4 source/test budget.
