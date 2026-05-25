# Omega-C Locks Amendments - Pass Omega V3 W2R

Pass: Pass Omega V3.
Date: 2026-05-25.
Scope: decide whether W2R / REDRESS-183 requires `restart/locks/LOCKS.md`
amendment.

## Verdict

No `LOCKS.md` amendment is required.

W2R is a SK-V14 wave-graph and exit-gate correction: W2 keeps skinny-side
`regen-css` ownership, while root CSS L4 runtime collapse moves to W6.0 after
the W5 generic-generator prerequisite. The active locks already cover the
corrected shape.

CRUD-3 is read/no-op for Pass Omega V3.

## Verification

`grep -cE "^[0-9]+\\. \\*\\*" restart/locks/LOCKS.md` returns `16`.

| Lock | Disposition |
|---|---|
| Lock 1 | No retained sidecar, second tape, public substrate API, or cross-call classifier state is introduced. |
| Lock 10 | The five-shape `BackendShape` canon remains `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. `FactStream` remains substrate-manifest only. |
| Lock 14 | W2R reinforces generated-output truth: W2 cannot claim root runtime generation before W6 owns it. |
| Lock 16 | No SIMD/ASM/primitive allowlist or same-wave primitive-consumer change is involved. |

## Final Omega-C Determination

REDRESS-183 exposes a contradiction between the SK-V14 W2 dual-tree round-trip
gate and the already-authoritative W5/W6 ownership of generic-provider and
Pattern H runtime collapse. W2R resolves that contradiction by changing wave
ownership and dispatch text, not lock semantics.

The companion `locks-diff.md` is therefore zero delta.
