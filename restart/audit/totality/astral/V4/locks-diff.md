# Pass Omega V4 Locks Diff

Status: zero delta.

No `restart/locks/LOCKS.md` amendment is proposed for REDRESS-184 / W4R.

Verification:

```sh
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
```

Expected and observed count: `16`.

The five-shape `BackendShape` canon remains:

```text
{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
```
