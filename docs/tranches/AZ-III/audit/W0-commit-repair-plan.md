# AZ-III W0 Commit Repair Record

## Action

Rewrote commit messages only for the history range:

```text
53d3e6b203ca4d5e1b5e34c06e05d867518ae0a5..HEAD
```

The repair targeted the AZ-II cutover span whose commits were bodyless,
overly terse, or scoped only as `az-ii` despite touching generated output,
deletion sweeps, benchmark surfaces, gate/status docs, or implementation
surfaces.

## Guardrails

- Created backup branch:
  `codex/az-history-before-reword-20260430-114057`.
- Stashed dirty and untracked root work before rewrite:
  `stash@{0}: pre-az-history-reword 2026-04-30T11:40:57-04:00`.
- Used a message filter only; no tree edits were made by the rewrite.
- Verified `git diff <backup>..HEAD --exit-code` was clean before
  restoring the stash.

## Result

Recent implementation commits now use concrete scopes such as:

- `refactor(lower/view-walk): rename tape traversal helpers`
- `chore(bench/cutover): add O5 close target`
- `fix(dispatch/alt): wire pure AltDispatch chains`
- `fix(grammar/generated): refresh generated grammar outputs`
- `fix(runtime/tape): delete tape crate`

Bodies now state why the slice exists, what class of work it belongs to,
what evidence is implied by the message-only repair, and where remaining
proof is routed. The repair deliberately does not claim that historical
commits have newly passed tests; AZ-III W1-W5 own the current evidence.

## Remaining Discipline

Future source commits must be sliced by mechanism or owned surface, with
bodies for generated output, deletion sweeps, benchmark/profiling artefacts,
gate/status changes, or broad refactors. Large "fix(az-ii)"-style scopes are
not acceptable for continuation implementation.
