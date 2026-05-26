# Pass Omega V8 Locks Diff

Disposition: zero delta.

No changes are proposed for `restart/locks/LOCKS.md`.

REDRESS-212 is a wave-graph and cap-accounting correction inside the existing
Lock 14 W5B-FRONTEND scope. It does not add public syntax, a BIR variant, a
BackendShape variant, a substrate surface, or a lock.

Verification required at CRUD-3:

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
```

Expected: `16`.

The five-shape BackendShape canon remains:

```text
EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage
```

`FactStream` remains a Lock 1 substrate-manifest category, not a sixth
BackendShape.
