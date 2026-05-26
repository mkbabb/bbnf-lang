# Omega-C Locks Amendments - Pass Omega V6 W5BR

Pass: Pass Omega V6.
Date: 2026-05-26.
Scope: Lock amendments implied by REDRESS-210.

## Verdict

NO LOCK AMENDMENT.

Lock 14 already forbids the route REDRESS-210 caught. The issue is not an
under-specified lock; it is a wave graph that tried to close deletion before the
provider-free generator body existed.

## Invariants Preserved

- Lock count remains 16.
- Lock 10 BackendShape canon remains five shapes:
  `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`.
- Lock 14 continues to require zero grammar-specific provider/template code in
  generic crates after the PRUNE-3 deletion close.
- LAC-1E-12 executable verification remains binding.

## Proposed Locks Diff

Zero delta. CRUD-3 is read/no-op.

## Verification Commands

```sh
grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
rg -n "EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage" restart/locks/LOCKS.md
```

Observed at V6 dispatch HEAD:

- Lock count: 16.
- Pattern H file count: 67.
- Lock 10 five-shape canon is preserved.
- `FactStream` remains the Lock 1 substrate-manifest category, not a sixth
  BackendShape.
