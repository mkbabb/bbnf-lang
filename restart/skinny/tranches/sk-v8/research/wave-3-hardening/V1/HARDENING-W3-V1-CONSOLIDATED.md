# SK-V8 W3 Hardening V1 Consolidated

Date: 2026-05-18.
Target: `fc91c217` (`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`).

## Verdict

ACCEPT, 6/6.

Minimum confidence: 94%.

## Disposition

V1 accepts the W3 plan's reject/routing decision. This is not authorization to
patch SIMD, runtime tape, generated JSON parser, retained view/value, codegen
templates, bench gates, or `skinny/RESULTS.md`.

The accepted blocker is the scanner/tape event-model mismatch:

- scanner positions are structural punctuation plus real quotes;
- current retained tape offsets are generated parser events: container
  opens/closes, opening quotes, number starts, and literal starts;
- retained view/`ValueRef` traversal depends on that event stream.

Therefore Tier A cannot be implemented in SK-V8 as a bounded storage swap. It
requires a split retained class/event grammar and cursor-contract proof before
structural-heavy parse rows can be measured.

## Required Redress Fold

- Add a `skinny/REDRESS.md` entry rejecting/routing W3 for this wave.
- Name target rows `twitter/parse_only` and `apache_builds/parse_only`.
- Name guard rows `canada/parse_only`, `mesh/parse_only`,
  `numbers/parse_only`, and `marine_ik/parse_only`.
- Preserve `skinny/RESULTS.md` unchanged with no W3 row-table admission.
- Update HANDOFF so W3 is rejected/routed and W4 is the next active wave under
  existing entry rules.
- Route the split precursor to SK-V9/Pass Omega: define the retained
  class/event grammar including numbers/literals and string quote ownership,
  prove the retained `ValueRef` cursor contract over that grammar, then measure
  a later structural-heavy parse row wave.

## Verification Cited By Challenge

- `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
- `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`
- `git diff --exit-code HEAD -- skinny/RESULTS.md`
- `git diff --check`

## Required Folds

The redress fold listed above is required before W3 can close.
