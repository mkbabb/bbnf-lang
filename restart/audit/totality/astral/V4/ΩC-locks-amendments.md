# Omega-C Locks Amendments - Pass Omega V4 W4R

Pass: Pass Omega V4.
Date: 2026-05-26.
Scope: decide whether REDRESS-184 / W4R requires `restart/locks/LOCKS.md`
amendment.

## Verdict

No `LOCKS.md` amendment is required.

W4R is a SK-V14 wave-graph and exit-gate correction. It moves CSS
provider/template deletion from W4 into W5 so deletion happens in the same wave
as the grammar-agnostic provider replacement. Existing locks already require
the corrected shape.

CRUD-3 is read/no-op for Pass Omega V4.

## Verification

`grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md` returns `16`.

`BackendShape` remains the five-shape canon:

```text
{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
```

| Lock | Disposition |
|---|---|
| Lock 1 | No retained sidecar, second tape, public substrate API, or cross-call classifier state is introduced. |
| Lock 10 | No BackendShape amendment; `FactStream` remains substrate-manifest only. |
| Lock 14 | W4R reinforces Lock 14 by moving provider deletion to the wave that replaces per-grammar providers with the grammar-agnostic path. |
| Lock 16 | No SIMD/ASM/primitive allowlist or same-wave primitive-consumer change is involved. |

## Final Omega-C Determination

REDRESS-184 exposes a sequencing contradiction between W4's deletion gate and
W5's replacement ownership. Resolving the contradiction is dispatch-surface
work only. The companion `locks-diff.md` is zero delta.
