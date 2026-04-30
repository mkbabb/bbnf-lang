# BC Challenge Ledger

Challenge wave opened 2026-04-29.

| Claim | Disposition | Evidence | Plan Consequence |
|---|---|---|---|
| Research must be challenged before synthesis. | accepted | `docs/precepts/instructions/tranche/CHALLENGE.md` and BC challenge agents | Save this ledger before W1 close. |
| `precepts` cap must move from 6 to 10. | narrowed | bbnf/config already carry 10-agent language; config lessons warn about agent budget | W1 makes 10 a hard ceiling, not the default target. |
| Challenge wave max moves from 3 to 5. | narrowed | challenge agent noted repo-local cases may need broader challenge | W1 makes five the default max, with documented override inside the 10-agent ceiling. |
| Triumvirate protocol should be first-class. | accepted | bbnf has auto-trigger language; shared precepts only implied a trio | W1 names research + plan augment/synthesis + redress/redeployment. |
| Scope dilation always triggers triumvirate. | narrowed | challenge agent distinguishes absorbable reveals from unclear root-cause/mode-choice cases | W1 triggers triumvirate for stalls and unclear dilation, not every absorbable reveal. |
| Consumers should use submodules, not copies or symlinks. | narrowed | all top-level consumers lacked `docs/precepts`; `precepts` now has a private remote | W2 uses submodule paths and FINAL records the remote URL. |
| Local tails replace only instruction surfaces. | accepted | product docs under `docs/` remain real local documentation | W2 does not delete or relocate product docs. |
| csp-solver should not get duplicate submodule. | accepted | `crates/csp-solver` is inside bbnf-lang workspace | W2 adds crate-local note only. |
| Dirty worktrees require path-specific staging. | accepted | keyframes/value/words/fourier/ffuzzy/speedtest have pre-existing dirty work | W2 edits exact rollout paths only. |
