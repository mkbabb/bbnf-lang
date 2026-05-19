# SK-V11 S-P1 Hardening V1 Consolidation

Pass: S-P1 Profile. Cycle: V1 CHALLENGE -> V2 fold.
Date: 2026-05-19.
Scope: consolidate the six-lens S-P1 V1 challenge and record the documentation
fold applied before the next S-P1 hardening cycle.

## Lens Dispositions

| Lens | Disposition | Required fold |
|---|---|---|
| CH1 correctness | REVISE | P1-E hot-leaf shorthand needed exact symbol and source-map resolution. |
| CH2 generality / Lock 14 | REVISE | Tiny-string leaves needed one canonical grammar-neutral primitive, and JSON object/array labels needed to become evidence members under container/sequence dispatch. |
| CH3 regression / pre-block | REVISE | Suggestive anomaly cautions needed explicit REDRESS anchors. |
| CH4 cost / reproducibility | REVISE | P1-A, P1-C, and P1-E needed provenance tightening; all P1 artifacts needed a shared capture block. |
| CH5 hidden coupling | ACCEPT | No fold required. |
| CH6 anti-paper-close | ACCEPT | No fold required. |

V1 is therefore REVISE, not REJECT. The raw profile evidence is intact: no
challenge lens requested a new capture, a behavior source change, or a row
admission change.

## Fold Applied

- Added shared capture provenance to P1-A through P1-F: run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, capture root
  `/tmp/skv11-p1`, W0 Criterion root `/tmp/skv11-open-criterion-3ce75df`,
  host/toolchain, `3ce75df4` binary source SHA, `9c8da194` documentation freeze
  SHA, target directory, binary paths, and exact build command.
- Reframed P1-A samply evidence as artifact-only where the saved logs do not
  embed an exact per-row shell transcript; xctrace remains the self-time
  authority. The hardening host reports `samply 0.13.1`.
- Retitled P1-C as W0 Criterion masking-probe extraction rather than a new
  samply Mode III call-stack capture and moved the run id into its frontmatter.
- Added the P1-E run id in frontmatter and sources.
- Reworked P1-B and P1-E vocabulary so `bounded_plain_string_scan`,
  `string_escape_decode`, `unicode_escape_hex_decode`, `number_digit_span`,
  `ascii_whitespace_skip`, `simd_movemask`, `container_dispatch`, and
  `output_digest_hash` are the canonical primitive names; JSON-specific
  generated, Track 2, typed, serde, and core helper symbols are evidence
  members.
- Expanded P1-E's hot-leaf source map to resolve the CH1 shorthand set:
  `memcpy`, `container`, `key colon`, `trailing-zeros`, `array next`,
  `wrapping-add`, `split-at`, `option copied`, `object direct`,
  `NonNull eq`, and `UTF-8 validation`.
- Added a compact pre-block matrix covering REDRESS 50, 51, 53, 54, 55,
  60-69, 72, 80, 82-84, 88-90, 96-98, and 102.

## V2 Entry

The V2 packet remains read-only profile evidence. It does not change
`skinny/RESULTS.md`, row outcomes, gate floors, code, or capture artifacts.
V2 is ready for a fresh six-lens challenge cycle.
