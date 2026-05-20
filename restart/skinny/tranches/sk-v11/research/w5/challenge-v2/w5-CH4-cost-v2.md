# SK-V11 W5 CHALLENGE V2 CH4 Cost Re-check

Date: 2026-05-20.
Lens: CH4 cost / probe trigger re-check.
Scope: Whether Plan V2 resolves CH4's cost/probe trigger concerns for
`random/direct_to_struct`.
Disposition: REVISE.

## Authorities Read

- `restart/skinny/tranches/sk-v11/research/w5/challenge/w5-CH4-cost.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-string-span-v2.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-v2-challenge-disposition.md`.

## Verdict

REVISE. Plan V2 resolves the probe-trigger half of CH4, but not the cost
mechanism half.

The V1 CH4 blocker had two connected parts:

1. The plan could not justify the required Track 2 lift from 6949 to 7878 Mbps
   with a same-cap-8 raw-end-to-span factoring, because the independent hand
   Track 2 path already has a local cap-8 tiny plain-string loop.
2. The old probe trigger was too weak because a one-track 1% movement could
   fall through to Criterion even when Track 2 remained far below the 7878 Mbps
   floor.

Plan V2 fixes the second point. It states that Criterion is allowed only when
the post-patch `random` probe shows both Track 1 and Track 2 at or near the
7878 Mbps floor with plausible noise margin, and it explicitly rejects a mere
1% improvement trigger. Its reject protocol also stops W5 if probes do not put
both `random` tracks near the floor. That is the floor-level trigger CH4
required.

Plan V2 does not fix the first point. It still selects a JSON-local cap-8
bounded string span and says the independent hand Track 2 path may keep its
local cap-8 loop. It does not name a material Track 2 source delta that would
plausibly fund the known 929 Mbps, 13.4% lift. It also does not carry forward
the V1 CH4 cost budget in time terms or explain why the span-shaped result
removes work that the current raw-end cap-8 fast path cannot already erase.

The stricter probe gate prevents a non-admitting Criterion run from updating
`RESULTS.md`, which is necessary. It does not by itself make the planned source
attempt cost-plausible under CH4. Measurement discipline can reject a weak
patch; it does not supply the missing Track 2 mechanism.

## Required V2 Revision

Before CH4 can accept Plan V2, the plan must add one of these:

- a concrete independent hand Track 2 cost mechanism, local to the Track 2
  owner path, that plausibly removes enough work from `random/direct_to_struct`
  to close the 6949-to-7878 Mbps gap; or
- an explicit diagnostic-only source/probe status that cannot admit W5 or
  update `RESULTS.md` unless later challenge text adds the missing cost
  mechanism.

Keep the V2 floor-level probe trigger. Do not return to the old one-track 1%
trigger. Do not smuggle in a cap change without fresh cap evidence and the
REDRESS 72 differential CH4 required.

DISPOSITION: REVISE.
