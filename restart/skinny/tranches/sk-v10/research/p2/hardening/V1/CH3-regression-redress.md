# SK-V10 S-P2 V1 CH3: Regression And REDRESS

Disposition: ACCEPT.
Date: 2026-05-19.
Scope: S-P2 candidate regression risk against `skinny/REDRESS.md`, with special
checks for W3/union, parse-only SOTA, eager scratch, PMULL/CTZ defaults, Canada
shortcuts, and prior failed routes without material differential.
Output: this file.

## Reviewed

- `restart/skinny/tranches/sk-v10/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`

## Findings

CH3 returns ACCEPT. No S-P2 candidate reopens a REDRESS-rejected route without a
material differential.

1. W3 / union substrate remains retired.
   The governing contract records W3 as retired and pre-blocks renamed union or
   W4-through-W3 routes (`HANDOFF.md:5-9`, `HANDOFF.md:75-82`,
   `SYNTHESIS.md:69-72`, `SYNTHESIS.md:120-133`). REDRESS 96 and 97 made both
   class-column and streaming-cursor W3 shapes measurable and failed every
   must-improve and maintain row; REDRESS 98 retires the thesis rather than
   merely deferring it (`skinny/REDRESS.md:2795-2848`,
   `skinny/REDRESS.md:2850-2906`, `skinny/REDRESS.md:2908-2950`).
   The P2 cohort preserves that boundary: P2-D permits only the existing tape
   contract and forbids retained class columns, sidecars, second source passes,
   or parser-owned structural cursors (`p2d-substrate-tape.md:28-54`,
   `p2d-substrate-tape.md:63-69`); P2-E explicitly declines
   `structural_cursor_from_movemask` (`p2e-parse-that-gaps.md:24`,
   `p2e-parse-that-gaps.md:38`); P2-F rejects structural sidecars or tape splits
   (`p2f-grammar-neutral.md:49-58`).

2. Parse-only is not used as SOTA close evidence.
   P1-F records all 17 parse rows as `S / NO-GO` and diagnostic only
   (`p1f-results-delta.md:35-39`, `p1f-results-delta.md:74-88`). P2-A, P2-B,
   P2-D, P2-E, and P2-F keep parse-only out of row admission and require
   direct/typed or product-plane consumers for row movement
   (`p2a-sota-teardown.md:12`, `p2a-sota-teardown.md:53-59`,
   `p2b-dav1d-process.md:157-160`, `p2d-substrate-tape.md:35-54`,
   `p2e-parse-that-gaps.md:30-38`, `p2f-grammar-neutral.md:16-18`).

3. Eager scratch, decoded materialization, and prior direct string shortcuts stay
   fenced.
   P1-C measured eager decoded-value materialization slower on every row and
   requires a material differential before any decoded scratch, semantic fact, or
   receiver-materialization route is reopened (`p1c-samply-mode-3.md:75-87`).
   REDRESS 66-69 reject parser-owned decoded scratch, byte-output unescape, and
   semantic string facts under the current direct digest workload
   (`skinny/REDRESS.md:1736-1785`, `skinny/REDRESS.md:1789-1835`,
   `skinny/REDRESS.md:1839-1886`). P2-B rejects the allocation materializer as a
   SIMD primitive (`p2b-dav1d-process.md:171`); P2-D keeps string/unicode work
   behind source spans and flags (`p2d-substrate-tape.md:49-54`); P2-E's
   `string_segments_fold` is admissible only as a consumer-owned output-plane
   bridge with no retained scratch (`p2e-parse-that-gaps.md:32-33`,
   `p2e-parse-that-gaps.md:56-63`).

4. PMULL and CTZ are not reintroduced as default hot paths.
   REDRESS 88 rejects PMULL as the default `bitmap_prefix_xor_64` body after
   JSON parse regressions, and REDRESS 89 rejects the CSSC CTZ bulk consumer
   after maintain-row regressions (`skinny/REDRESS.md:2510-2540`,
   `skinny/REDRESS.md:2542-2585`). P2-C carries PMULL only as a narrow,
   caller-proven, non-default string-region candidate and marks CTZ default bulk
   emit as REDRESS-blocked (`p2c-arch-esoterica.md:33-34`,
   `p2c-arch-esoterica.md:63-73`). P2-B and P2-F repeat that default production
   rewires are rejected (`p2b-dav1d-process.md:169-170`,
   `p2f-grammar-neutral.md:57`).

5. Canada shortcut remains blocked.
   REDRESS 94 admits Apache/CITM typed rows only and keeps
   `canada/real_typed_struct` rejected pending full-fixture
   DirectBuild-vs-serde checksum proof (`skinny/REDRESS.md:2731-2757`).
   P1-E marks Canada as numeric-only and explicitly pre-blocked for typed
   admission without full-fixture proof (`p1e-hot-leaf-attribution.md:53-56`).
   P2 numeric candidates preserve that boundary: P2-A says Canada typed remains
   pre-blocked, P2-C blocks Canada typed shortcuts, P2-E requires full fixture
   parity before Canada typed movement, and P2-F repeats the full-fixture
   generated/serde/sonic checksum requirement (`p2a-sota-teardown.md:33`,
   `p2c-arch-esoterica.md:30`, `p2e-parse-that-gaps.md:34`,
   `p2f-grammar-neutral.md:56`).

6. Previously failed REDRESS families are differentiated materially or rejected.
   The cohort does not simply rename failed routes. Tiny-string work is scoped to
   current direct/typed callers after scalar oracle and micro-proof, not the
   REDRESS 28/33 retained active dispatch (`p2b-dav1d-process.md:164`,
   `p2e-parse-that-gaps.md:56`, `p2f-grammar-neutral.md:52`). Unicode work is
   batched or caller-owned and barred from per-quartet materializer replay after
   REDRESS 82 (`p2c-arch-esoterica.md:29`, `p2e-parse-that-gaps.md:32`,
   `p2e-parse-that-gaps.md:60`). Number work targets digit-run/direct-array hot
   leaves, not the REDRESS 80 mantissa-widen shortcut
   (`p2e-parse-that-gaps.md:34`, `p2e-parse-that-gaps.md:59`).

## Required Fixes

None for CH3.

## Non-Blocking Notes

- P2-A's W3 risk paragraph is substantively correct because it cites SK-V10
  synthesis, but its `skinny/REDRESS.md` line anchors point at early redress
  implementation notes rather than REDRESS 96-98. This is a citation-hygiene
  issue for CH1/CH6, not a CH3 regression blocker.
- Any S-P3 wave that promotes P2-C PMULL/CTZ, P2-E string segment folding, or
  P2-D tape economy from research to source scope must restate the REDRESS
  material differential in the wave gate before implementation.

## Verdict

ACCEPT. The S-P2 V1 packet is regression-safe under the CH3 lens. No candidate
reopens W3/union, parse-only SOTA, eager scratch, PMULL/CTZ defaults, Canada
typed shortcuts, or prior REDRESS-failed routes without either rejecting the
route or requiring a concrete material differential.
