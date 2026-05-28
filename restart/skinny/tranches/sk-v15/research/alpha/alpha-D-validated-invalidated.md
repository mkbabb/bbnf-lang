# Alpha-D — Validated / Invalidated Ledger — SK-V15 V1

Pass: Pass Alpha. Cycle: SK-V14 -> SK-V15.
Date: 2026-05-27.
Scope: durable wins, invalidated claims, open implementation gaps.
Output: this file.

## Validated

SK-V14 JSON carries forward as the validated baseline. JSON closes 51/51
admitted rows across `parse_only`, `direct_to_struct`, and
`real_typed_struct` (`skinny/RESULTS.md:139`). The load-bearing wins are:

- W11W parse_only `memchr2` plus SWAR control-byte preservation
  (`skinny/REDRESS.md:6256`);
- W11A direct strict-product rows (`skinny/REDRESS.md:5855`);
- W9/W9AA/W9AB typed product rows;
- W11U/L/N/O residual typed and direct product closures.

These remain SK-V15 guard rows. They must not be reopened without a
stricter same-plane falsification.

## Invalidated

SK-V14 CSS L4 does not carry forward as a validated >SOTA admit. The
24 CSS L4 rows are demoted to INVALIDATED/AUDIT-FALSIFIED because W8R:

- broadcast one aggregate measurement across 24 conceptual rows;
- compared a brace-counter summary against lightningcss CSSOM;
- used a hand-written 646-line CSS tokeniser embedded as a generator
  string literal rather than grammar-derived emission.

Evidence: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`,
`:29`, and `:31`.

## Still Open

| Gap | Evidence | Receiver |
|---|---|---|
| CSS admission honesty | broadcast rows + mismatched comparator | PRUNE-WAVE-A |
| CSS Value API | no typed CSS value/view/visitor; parse returns fact-stream string | REBUILD-WAVE-E |
| Lock 14 / Lock 16 gates | scan-root, checkasm, or report exclusions hide known leaks | PRUNE-WAVE-B |
| Codegen grammar creep | runtime modes, CSS config table, JSON/CSS templates in generic path | PRUNE-WAVE-C |
| Pattern H | 67 files, 0 generated headers, 4 bespoke grammars | PRUNE-WAVE-D |
| Decision Engine | zero rewrite rules, tautological CSP, four lowerer stubs | REBUILD-WAVE-F |
| FNV closed-enum products | bench-only strict-product contrivance | REBUILD-WAVE-G |

## Ledger Text

SK-V15 starts from JSON validated and CSS invalidated/open. It is not a
grammar-driven generalisation tranche yet. It is the prune/rebuild cycle
that must make CSS a real second worked example before SK-V16, and under
the latest user extension SK-V17, can claim the generalisation inflection
point.
