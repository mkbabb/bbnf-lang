ACCEPT

# SK-V11 S-P1 Hardening V2 CH3: Regression / Pre-Block

Date: 2026-05-19.
Scope: assess only whether the V2-folded S-P1 packet attaches REDRESS anchors
for the V1 CH3 pre-block families without reopening or prescribing behavior.
Output: this file.

## Basis

- PASS-1 Section 3 CH3 requires any S-P1 anomaly that suggests a route already
  in `skinny/REDRESS.md` to cite the entry and mark the route pre-blocked
  (`restart/prompts/skinny/PASS-1-PROFILE.md:137`).
- ORCHESTRATOR Section 3Z requires V1 challenge dispositions to fold before the
  pass advances (`restart/prompts/ORCHESTRATOR.md:104` through `:121`).
- The V1 consolidation records the requested fold: a compact pre-block matrix
  covering REDRESS 50, 51, 53, 54, 55, 60-69, 72, 80, 82-84, 88-90, 96-98,
  and 102 (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:46`).

## Findings

1. The requested anchor fold is present in the V2 packet. P1-E carries the
   load-bearing matrix: W3/substrate routes are tied to REDRESS 50, 51, 53, 96,
   97, 98, and 102; string/unicode scanner and materialization routes are tied
   to REDRESS 54, 55, 60-62, 64, 66-69, 72, 82, and 83; parser-control and
   value-byte routes are tied to REDRESS 63, 65, and 84; numeric fallback is
   tied to REDRESS 80; and PMULL/CTZ/body-fill routes are tied to REDRESS 88,
   89, and 90
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:252`
   through `:259`).
2. The surrounding V2 artifacts also fence the same families where their local
   anomalies appear. P1-A marks parse-only as diagnostic, names the W3/substrate
   pre-blocks, and attaches string/unicode anchors
   (`restart/skinny/tranches/sk-v11/research/p1/p1a-samply-mode-1.md:154`
   through `:173`). P1-B repeats the W3/parse-only-firewall closure on product
   leaves (`p1b-samply-mode-2.md:308` through `:313`). P1-C marks eager
   decode/materialization and structural/lazy-tape evidence as diagnostic, then
   cites W3, parse-only firewall, and string/materialization anchors
   (`p1c-samply-mode-3.md:177` through `:203`). P1-D keeps PMU/cycles as
   diagnostic non-producers and cites W3 plus parse-only firewall pre-blocks
   (`p1d-pmu-cycles.md:211` through `:239`). P1-F keeps structural scan,
   masking probes, PMU, and cycles out of behavior evidence
   (`p1f-results-delta.md:223` through `:225`).
3. The anchors resolve in `skinny/REDRESS.md` through REDRESS 110. The cited
   entries cover aux side tables, event/structural cursors, string/unicode
   scanner and materialization variants, parser-control carry, object value-byte
   compaction, numeric fallback, PMULL/CTZ/body fills, W3 union-substrate
   retirement, and the parse-only firewall
   (`skinny/REDRESS.md:715`, `:742`, `:784`, `:815`, `:846`, `:1346`,
   `:1382`, `:1441`, `:1492`, `:1584`, `:1639`, `:1688`, `:1736`, `:1789`,
   `:1839`, `:1996`, `:2217`, `:2287`, `:2320`, `:2360`, `:2510`, `:2544`,
   `:2589`, `:2797`, `:2852`, `:2910`, `:3042`, and `:3259`).
4. No V2 CH3 regression is present. The packet remains profile evidence only:
   parse-only rows stay diagnostic, PMU/cycles and masking probes stay
   non-producers, and no retired REDRESS route is reintroduced as a behavior
   plan, row admission, source edit, generated-output change, or benchmark-body
   change.

## Required Fold

None. The V1 CH3 citation/pre-block hygiene fold is satisfied for V2.
