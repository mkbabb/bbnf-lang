# SK-V13 S-P2 V4 CH2: Generality / Lock 14 Confirmation

## Verdict

ACCEPT.

V4 confirms the V3 CH2 acceptance. The unchanged V3 S-P2 packet preserves
Lock 14 by keeping CSS feature work as generated row/fact-stream metadata,
preserving the P2-A C1-C8 verdict mapping, and rejecting JSON/CSS control flow
inside generic crates.

## Evidence

- V3's consolidated disposition says the only V2 blocker was resolved:
  `CSS-ROW-SCOPE-CONDITIONAL` marks CSS rows 1-6 as generated row/fact-stream
  work, not primitive eligibility, and all six lenses accepted V3
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:10`-`25`).

- P2-F defines the Lock 14 line directly: generic crates carry no grammar arms,
  grammar-named modules, grammar-specific public types, or grammar-specific
  feature flags; per-grammar behavior must be generated from grammar-agnostic
  templates (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:29`-`47`).

- The six CSS expansion scopes are all fenced as `CSS-ROW-SCOPE-CONDITIONAL`.
  Each row requires a strict row consumer plus fresh narrow CSS parser profiling
  or same-wave lightningcss/cssparser row movement, and none is presented as a
  generic primitive admission (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:69`-`74`).

- P2-F explicitly states the grammar-neutral property for CSS rows is the shared
  template and metadata mechanism, not hardcoded CSS runtime behavior; it also
  rejects generic at-rule branches and requires vendor/custom taxonomy to come
  from grammar metadata (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:123`-`132`,
  `:72`-`:74`).

- The P2-A C1-C8 lineage remains intact. P2-A defines C1-C8 as comparator-led
  primitives (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:74`-`81`),
  and P2-F carries the literal mapping to admissible, conditional,
  route-production, or fact-stream-only verdicts with explicit rejection
  boundaries for retained sidecars, generic JSON object/array/key-colon
  branches, parser-speed digest claims, and `JsonDigestSink` internals
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:108`-`119`).

- P2-F keeps JSON/CSS control flow out of generic crates. It requires generated
  FIRST/action tables, generated sink/view surfaces, generated flag policy, and
  generated grammar metadata for union and CSS routes; current `JsonSink`,
  `OffsetFlags`, and JSON quote/backslash/string behavior remain overfit until
  moved behind generated policy surfaces
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:76`-`81`,
  `:167`-`:194`).

- The V3 cross-read disposition confirms the V2 accepted folds remain intact:
  CSS stylesheet/selectors, declaration-value extension, visual functions,
  at-rules/media, nesting, and vendor/custom at-rule taxonomy are row-production
  scopes requiring measurement, while grammar-neutrality is admitted only at
  byte-set, policy, fact-stream, regex-analysis, resolver, or codegen-private
  same-substrate boundaries (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:239`-`253`).

## Blockers / Fold Requirements

No CH2 blocker remains.

Carry-forward requirements:

1. S-P3 must preserve CSS rows 1-6 as conditional row-production scopes, not
   primitive eligibility.
2. Any CSS row plan must name a strict row consumer plus fresh narrow CSS parser
   profiling or same-wave lightningcss/cssparser row movement.
3. Candidate renames or splits must retain auditable P2-A C1-C8 lineage and the
   P2-F verdict vocabulary.
4. JSON/CSS/Sheets/BBNF behavior must not enter generic-crate control flow.
   Grammar differences remain generated metadata, generated per-grammar modules,
   or codegen-private policy surfaces.
5. `JsonSink`, `OffsetFlags`, JSON quote/backslash policy, object/array/key-colon
   dispatch, and inventory-only SIMD cannot be promoted without a later accepted
   research fold plus scalar/parity/checkasm and same-wave row consumer gates.

## Disposition

CH2 generality / Lock 14 confirms V3. V4 should count as the second consecutive
accepted cycle for this lens if the remaining V4 challenge reports also accept.
