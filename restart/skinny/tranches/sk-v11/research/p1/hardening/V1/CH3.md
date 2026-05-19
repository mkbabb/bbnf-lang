# SK-V11 S-P1 V1 CH3: Regression And Pre-Block

Disposition: REVISE.
Date: 2026-05-19.
Scope: CH3 only. This assesses whether S-P1 V1 silently reopens retired
W3/substrate/sidecar/PMULL/CTZ/generic-policy/string-scanner routes or fails
to pre-block suggestive anomalies.
Output: this file.

## Standard

`restart/prompts/skinny/PASS-1-PROFILE.md:137` says CH3 asks whether any
Section 4 anomaly silently re-proposes a route already in `skinny/REDRESS.md`;
lines 138-141 require a suggestive "hot leaf suggests X" anomaly to cite the
REDRESS entry and mark the route pre-blocked. `restart/prompts/ORCHESTRATOR.md:85`
adds the general CH3 rule: no proposal may reopen a REDRESS route, the
pre-block list must be correctly identified, and no admitted row may be
silently regressed.

## Findings

1. No behavior route is actually proposed by the P1 cohort. W0 says
   parse-only is diagnostic, `direct_to_struct` has 13 `N-direct / NO-GO`
   rows, and `real_typed_struct` is a guard surface
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:24`).
   W0 also keeps `instruments`, `numbers`, and `unicode_mixed` as
   W0-clamped non-admissions even when one track clears the computed floor
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:47`,
   `:54`). P1-F preserves this by saying diagnostic nonproducers are not
   behavior evidence (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:198`).

2. The W3/substrate family is not silently reopened, but citation coverage is
   uneven. P1-E is the strongest artifact: it explicitly maps structural
   rediscovery to REDRESS 96, 97, and 98, and maps parser-owned cursor/event
   vector/whitespace bitmap/aux projection to REDRESS 51, 53, and 98
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:220`,
   `:228`, `:229`). That correctly pre-blocks the retired W3 union substrate,
   whose REDRESS history rejected class-column structural index, rejected the
   streaming cursor, and retired `G-W3-UNION-SUBSTRATE`
   (`skinny/REDRESS.md:2797`, `:2852`, `:2910`, `:2934`). However, P1-A,
   P1-B, P1-C, and P1-D use the same caution vocabulary without local REDRESS
   citations: sidecar scanner (`p1a-samply-mode-1.md:141`), W3/sidecar
   substrate (`p1b-samply-mode-2.md:266`), W3 sidecar family
   (`p1c-samply-mode-3.md:170`), and retained sidecar cursor
   (`p1d-pmu-cycles.md:212`). This is not a route reopen, but it misses the
   PASS-1 CH3 citation requirement.

3. String/unicode scanner and materialization anomalies are fenced in meaning
   but not always in citation. P1-A labels `y_string_unicode` scalar unicode
   escape cost as "not permission to add a sidecar scanner"
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:138`);
   P1-B keeps `unicode_mixed` separate and W0-clamped
   (`p1b-samply-mode-2.md:256`); P1-C says eager decode/materialization is a
   cost probe, not a production plan (`p1c-samply-mode-3.md:151`); and P1-E
   says structural scan remains a nonproducer and W3 cannot be reintroduced
   from hot-leaf attribution alone (`p1e-hot-leaf-attribution.md:291`). Those
   cautions need explicit pre-block anchors to the retired string route
   families: REDRESS 54-55 decoded stats and quote-source streaming hash
   (`skinny/REDRESS.md:815`, `:846`), REDRESS 60-62 retained string boundary
   collapse and wide/delayed-wide scanners (`skinny/REDRESS.md:1346`,
   `:1382`, `:1441`), REDRESS 64 and 66-69 escape/direct materialization
   families (`skinny/REDRESS.md:1584`, `:1688`, `:1736`, `:1789`, `:1839`),
   REDRESS 72's admitted cap-16 split that rejects global policy
   (`skinny/REDRESS.md:1996`, `:2045`), and REDRESS 82-83 single-quartet and
   StringBlock16 rejects (`skinny/REDRESS.md:2287`, `:2320`).

4. PMULL/CTZ/generic primitive routes are not reopened in V1. P1 mentions
   `movemask`, `trailing_zeros`, structural scan, and PMU/cycles as hot-leaf
   or cost evidence, but no artifact proposes an asm body fill, PMULL prefix
   XOR, CTZ bulk consumer, or generic policy change. This is acceptable only if
   downstream S-P2/S-P3 continues to carry REDRESS 88 and REDRESS 89 as hard
   pre-blocks: PMULL prefix-XOR regressed hard JSON rows
   (`skinny/REDRESS.md:2510`, `:2527`, `:2535`), and CTZ/bulk rewiring was
   rejected under the W10b maintain invariant (`skinny/REDRESS.md:2544`,
   `:2573`, `:2580`). REDRESS 90 then confirms both bitmap asm body fills
   remain rejected (`skinny/REDRESS.md:2594`).

5. Direct row regression is handled fail-closed. P1-F catches and folds the
   `canada/parse_only` surface mismatch, marks absent `instruments` W10 close
   telemetry, and marks stale/absent `numbers` direct telemetry
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:161`,
   `:165`, `:169`). These are row-surface hygiene findings, not attempts to
   re-admit rows from W0 evidence.

## Required Fold

Before CH3 can become ACCEPT, fold a compact REDRESS pre-block matrix into the
S-P1 V2 cohort or the V1 consolidation. It must attach the following anchors to
the existing anomaly cautions:

- W3/substrate/sidecar/aux projection: REDRESS 50, 51, 53, 96, 97, 98, and
  the REDRESS 102 parse-only firewall.
- String/unicode scanner/materialization: REDRESS 54, 55, 60, 61, 62, 64,
  66, 67, 68, 69, 72, 82, and 83.
- Parser-control/value-byte and object/key carry: REDRESS 63 as admitted but
  non-closing, REDRESS 65 as rejected object next-key carry, and REDRESS 84 as
  rejected object-pair value-byte compaction.
- Numeric fallback/global policy: REDRESS 80.
- PMULL/CTZ/bitmap body fills: REDRESS 88, 89, and the REDRESS 90 rejection
  carry-forward.

The fold must preserve current semantics: all these P1 signals are diagnostic
or planning evidence only, not behavior prescriptions, row admissions, source
work, or permission to dispatch a retired route under a new name.

## Final Judgment

REVISE, not REJECT. S-P1 V1 does not propose a retired route and does not move
rows from diagnostic evidence, but multiple suggestive anomaly notes lack the
explicit REDRESS citations required by PASS-1 CH3. The required fold is
citation/pre-block hygiene, not an implementation change.
