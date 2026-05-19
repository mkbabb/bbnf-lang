# SK-V10 Alpha CH3 Regression And REDRESS

Date: 2026-05-19.

Scope: REDRESS pre-blocks, regression risk, and stale route reopening.

## Disposition

REVISE -> ACCEPT after fold.

## Findings

1. The only CH3 blocker was shared with CH5: typed rows were framed as
   strict-vs-strict despite the current deferred/view-boundary gate.
   Fold: wording now uses same-run typed comparator evidence and explicitly
   blocks strict admission until `gate-json` consumes the validation-path change.

## Pass Checks

- W3 union/event substrate is retired and pre-blocked by REDRESS 98.
- W4 cascade through W3 is not a valid entry gate or consumer.
- Parse-only remains `S / NO-GO` and out of the SOTA scoreboard.
- Direct digest rows stay guard-plane evidence until a direct product/control
  contract exists.
- Existing-substrate unicode/string work names current string/unescape callers,
  not W3.

## Result

No SK-V10 Alpha candidate reopens REDRESS 96, 97, or 98 after the wording fold.
