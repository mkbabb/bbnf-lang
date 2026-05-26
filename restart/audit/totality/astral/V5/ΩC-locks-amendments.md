# Omega-C Locks Amendments - Pass Omega V5 W5R

Pass: Pass Omega V5.
Date: 2026-05-26.
Scope: decide whether REDRESS-209 / W5R requires `restart/locks/LOCKS.md`
amendment.

## Verdict

No `LOCKS.md` amendment is required.

W5R is a SK-V14 wave-graph and exit-gate correction. Existing Lock 14 already
requires the corrected shape: per-grammar providers/templates cannot be
preserved, but deletion cannot be claimed until the grammar-source-consuming
replacement path is real and consumed.

CRUD-3 is read/no-op for Pass Omega V5.

## Verification

`grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md` returns `16`.

`find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` returns
`67`.

The five-shape `BackendShape` canon remains:

```text
{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}
```

| Lock | Disposition |
|---|---|
| Lock 1 | No retained sidecar, public substrate API, or cross-call classifier state is introduced. |
| Lock 10 | No BackendShape amendment; `FactStream` remains substrate-manifest only. |
| Lock 14 | W5R reinforces the existing grammar-neutral generator requirement by preventing static centralization from counting as provider collapse. |
| Lock 16 | No SIMD/ASM/primitive allowlist or same-wave primitive-consumer change is involved. |

## Final Omega-C Determination

REDRESS-209 exposes a sequencing contradiction inside W5. Resolving it is
dispatch-surface work only. The companion `locks-diff.md` is zero delta.
