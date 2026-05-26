# SK-V14 W4 Redress: PRUNE-2 Provider-Deletion Cycle

Date: 2026-05-26.
Wave: W4.
Phase: redress.
Status: REJECTED.

## Finding

W4 cannot honestly close under the current SPEC wording. The wave requires
deleting the seven CSS L4 provider modules and then immediately consuming
`cargo xtask regen-css` to re-emit the deleted runtime twins. The current
`regen-css` implementation is not independent of those provider modules.

The failure is intrinsic to wave ordering:

- W4 owns provider deletion.
- W5 owns provider replacement / generic generator collapse.
- W5 requires W4 admission.

## Executable Evidence

The throwaway-worktree probe in `skv14-W4-B-provider-cycle.md` deleted the W4
provider/template/runtime surface and ran:

```sh
cd /Users/mkbabb/Programming/bbnf-lang-w4-prune2-probe/skinny
cargo xtask regen-css
```

The command failed compiling `codegen` because `src/lib.rs` still declares the
seven deleted CSS provider modules. That proves W4's same-wave consumer is
sequenced after the deletion it depends on.

## REDRESS

Record REDRESS-184 in `skinny/REDRESS.md`:

- Gate: `G-SK-V14-W4-PRUNE-2`.
- Decision: `REJECTED`.
- Root cause: W4 deletes the provider/template modules still required by W2's
  `regen-css`; W5 owns their generic replacement but is gated by W4.
- Corrective route: Pass Omega V4 W4R.

## Blocked State

W5/W6/W7 remain blocked by the PRUNE chain. W8/W9/W10 remain globally blocked
until PRUNE-1 through PRUNE-5 close. No CSS L4 row is admitted or newly
measured by W4.
