# CH3 Regression - Pass Omega V3 W2R

Verdict: ACCEPT.

The proposed W2R amendment does not reopen the REDRESS recurrence routes in the
CH3 watch list. It closes the REDRESS-183 failure by narrowing W2 to the
skinny-side `regen-css` mirror and moving `crates/core/src/runtime/css_l4/` to
W6.0, where the generic generator prerequisite exists.

## Regression Checks

| Route | CH3 result |
|---|---|
| Fake generated headers | Closed. W2R keeps the bypass-header detector and forbids treating root Pattern H runtime files as generated. |
| Hand-patched generated output | Closed. W2 remains clean-regen only for `skinny/crates/runtime/src/grammars/css_l4_*`; W6.0 owns the root-runtime round-trip. |
| W2 partial admit | Closed. W2 redress explicitly rejected partial admit; W2R requires an amended rerun before dispatch can continue. |
| W3+ dispatch after rejected W2 | Closed. V3 artifacts block W2 rerun and W3+ dispatch until G-Omega accepts W2R and amended W2 admits. |
| RESULTS / REDRESS row movement | Closed. V3 limits CRUD to document/spec/handoff/corpus alignment and forbids source, generated output, benchmark, gate, `RESULTS.md`, and new `REDRESS.md` movement beyond REDRESS-183. |
| PRUNE ordering breach | Closed. W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close; W6.0 preserves the PRUNE chain rather than resequencing W5/W6 before W2. |

## Evidence

- REDRESS-183 rejects `G-W2-FULL-ROUNDTRIP` and records that W2 rejection
  blocks W3/W4/W5/W6/W7 and all new-admit waves.
- W2 redress rejects partial admission and names fake generated headers /
  copied root runtime files as recurrence vectors.
- W2R makes W2 skinny-side only, preserves bypass-header detection, forbids CSS
  row movement and root-runtime touch, and assigns root CSS L4 runtime collapse
  to W6.0.
- Omega-F blocks W3+, preserves W2 skinny-only rerun requirements, assigns root
  runtime to W6.0, and permits W8/W9/W10 only after PRUNE-1..PRUNE-5 close.

## Disposition

CH3 accepts the V3 amendment packet as regression-safe. Until G-Omega
authorizes W2R and amended W2 re-admits, no W2 rerun, W3+ dispatch, or
new-admit dispatch is authorized.
