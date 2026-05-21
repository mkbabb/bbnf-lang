# SK-V13 S-P2 V4 CH6: Anti-Paper-Close Confirmation

## Verdict

ACCEPT.

## Evidence

- V4 confirms the unchanged V3 packet. The V3 consolidated hardening records
  `CSS-ROW-SCOPE-CONDITIONAL` as generated row/fact-stream work, not primitive
  eligibility, and requires fresh narrow CSS parser profiling or same-wave
  strict lightningcss/cssparser row movement before S-P3 may plan CSS rows 1-6
  (`HARDENING-S-P2-V3-CONSOLIDATED.md:13`-`:16`, `:34`-`:38`).
- P2-F carries that rule into the candidate table itself. Stylesheet/selectors,
  declaration-value extension, visual functions, at-rules/media, nesting, and
  vendor/custom at-rule taxonomy each name a strict CSS row consumer and require
  fresh narrow CSS parser evidence or same-wave strict row movement; none is
  allowed to close as a support-only primitive (`p2f-grammar-neutral.md:44`-`:47`,
  `:69`-`:74`).
- The same anti-paper-close rule is restated at the carry-forward boundary:
  CSS rows 1-6 are conditional row-production scopes, not primitive eligibility,
  and any SIMD inside a CSS row inherits scalar/checkasm/consumer gates
  (`p2f-grammar-neutral.md:151`-`:166`). This prevents S-P3 from converting a
  generated row template, comparator lane, or future-wave promise into closure.
- Inventory-only SIMD remains fenced. P2-F marks standalone prefix/next/bulk
  bitmap primitives, `byte_context`, `cache_hints`, EOR3/BCAX, LD4/TBX/SMIN/SMAX,
  and standalone D1 lazy tape capacity as `INVENTORY-ONLY` or
  `NOT-S-P3-ELIGIBLE` unless later evidence adds a named hot expression, scalar
  reference, checkasm/parity, and same-wave row consumer
  (`p2f-grammar-neutral.md:89`-`:96`, `:162`-`:166`, `:199`-`:201`).
- P2-A and P2-B still require measurable gates for row-moving primitives:
  scalar-reference-first shape, checkasm/parity expectations, named P1
  antecedent, same-wave consumer, and strict row gate. B2/C7 specifically cannot
  count the SK-V12 microbench as admission; it must wire a CSS generated scanner
  and preserve strict lightningcss equality in the same wave
  (`p2a-sota-teardown.md:65`-`:87`; `p2b-dav1d-process.md:37`-`:48`,
  `:68`-`:80`).
- P2-C and P2-D preserve the historical-failure boundaries rather than promising
  future repair. PMULL/CSSC, UDOT, TBL/TBX, string-context, and EOR3 are tied to
  named aarch64 features plus scalar/checkasm/consumer requirements; union work
  must cite REDRESS 96/97/98 and land only as a same-tape/sink projection with a
  same-wave row-moving gate (`p2c-arch-esoterica.md:16`-`:24`,
  `:54`-`:102`; `p2d-substrate-tape.md:125`-`:137`).
- P2-E closes the last support-only escape hatch for parser-vocabulary work:
  every parse-that primitive P2E-1 through P2E-8 includes a scalar-reference
  sketch, parity/checkasm or unit-test requirement, and same-wave consumer note;
  a crate extraction with no resolver/codegen consumer is explicitly called a
  CH6 failure (`p2e-parse-that-gaps.md:35`-`:121`).

## Blockers / Fold Requirements

None for CH6.

## Disposition

V4 confirms the V3 CH6 acceptance. The S-P2 research packet no longer contains a
paper-close path where CSS rows, SIMD inventory, parse-that scaffolding, or union
substrate candidates can close without strict row movement or measured
architectural-block evidence. S-P3 may proceed if the other V4 lenses also
confirm acceptance, carrying forward the same CSS row-production, scalar
reference, checkasm/parity, same-wave consumer, and strict comparator gates.
