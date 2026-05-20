# SK-V12 S-P3 V4 CH4 Cost

Pass: S-P3 Synthesis-Plan.
Cycle: V4.
Lens: CH4 cost / cap.
Disposition: ACCEPT.

## Findings

No cost or cap defects found.

- SK-V12 is W0-W4 only, five waves total, below the 12-wave ceiling.
- LOC, wall, redress, and rerun caps align in SPEC and DISPATCH:
  W0 `<=180`, W1 `<=520`/`<=480`/`<=460`, W2 `<=430`, W3 `<=300`,
  W4 `<=120`, with redress `<=75 min`.
- W1 remains one-target/no-fallthrough: fallback is plan-time only, redress
  attempts one selected grammar, and any split requires future S-P3 revision.
- W2 carries the required five-part cost table and fail-before-source-work rule
  if the slice cannot fit the cap.
- W3 is one selected residual-row intervention or a routed block; no hidden
  split is authorized.

## Required Folds

None.

## Residual Risk

Non-blocking wording drift: P3-C says "V3 packet" inside a V4 artifact. The
surrounding rule still blocks W1 split authority.
