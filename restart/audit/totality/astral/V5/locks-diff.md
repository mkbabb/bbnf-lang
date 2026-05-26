# Pass Omega V5 Locks Diff

Status: zero delta.

No `restart/locks/LOCKS.md` amendment is proposed for REDRESS-209 / W5R.

Verification:

```sh
grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

Expected and observed:

- lock count: `16`;
- Pattern H runtime-file count: `67`.

The five-shape `BackendShape` canon remains:

```text
{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
```

`FactStream` remains the Lock 1 substrate-manifest category, not a sixth
`BackendShape` variant.
