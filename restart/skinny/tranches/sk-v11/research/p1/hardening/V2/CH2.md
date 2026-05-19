REVISE

# SK-V11 S-P1 V2 CH2: Generality / Lock 14

Date: 2026-05-19.
Lens: CH2 GENERALITY / Lock 14.
Scope: review the folded S-P1 P1-A through P1-F packet, W0 baseline,
`skinny/RESULTS.md`, and V1 consolidation for grammar-neutral hot-leaf
attribution.

## Findings

1. The V1 fold mostly canonicalized the hot leaves correctly. P1-B now states
   that canonical primitive names are grammar-neutral and that JSON, hand,
   generated, typed, and serde symbols are evidence members, not generic claims
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:110`).
   P1-E makes the same load-bearing rule explicit and maps the hot set to
   `bounded_plain_string_scan`, `string_escape_decode`,
   `unicode_escape_hex_decode`, `number_digit_span`,
   `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, and
   `output_digest_hash`
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:98`).

2. JSON/generated/serde names are mostly quarantined as source evidence. P1-E
   keeps `runtime::generated_json`, Track 2 JSON, typed generated, serde/oracle,
   and Rust core helper names inside the evidence/source-locus column under the
   canonical primitive table
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:100`),
   and P1-E explicitly says typed Track 2 `serde_json` leaves are
   comparator/oracle evidence, not generated-product hot leaves
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:208`).

3. The fold is incomplete because load-bearing prose still leaks JSON role
   vocabulary. P1-B summarizes numeric direct rows as "digit-scan plus
   array-walk rows" and later calls `instruments` ordinary
   "string/whitespace/object leaves"
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:193`;
   `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:203`).
   The same object wording reappears in the anomaly section
   (`restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md:305`).
   P1-E also groups residuals as "Number/array rows"
   (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:170`).
   Those are not merely row-local generated symbol citations; they are summary
   classifications. Under the S-P1 CH2 rule, the load-bearing terms must be
   `number_digit_span`, `container_dispatch`, `sequence_element_dispatch`, or
   equivalent grammar-neutral primitive names
   (`restart/prompts/skinny/PASS-1-PROFILE.md:129`).

4. V2 does not claim non-JSON proof from JSON-only telemetry. W0 is explicitly a
   JSON baseline
   (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:5`), and
   P1-F states that all 41 manifest rows are JSON domain rows with no CSS L4,
   Sheets, or BBNF-self telemetry in W0
   (`restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md:221`).
   SK-V11 still requires an admitted, benchmarked non-JSON generated-parser
   intervention rather than prose proof
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:56`;
   `restart/skinny/tranches/sk-v11/HANDOFF.md:72`).

## Required Fold

1. In P1-B, replace the remaining summary phrases `array-walk rows`,
   `object leaves`, and equivalent object/array summary wording with
   `container_dispatch`, `sequence_element_dispatch`, or
   `number_digit_span plus sequence/container dispatch`.
2. In P1-E, replace the residual group label `Number/array rows` with
   `number/sequence rows` or `number/container-dispatch rows`.
3. Preserve row-local generated JSON, Track 2 JSON, typed generated, serde, and
   core helper names as evidence members under the canonical primitive bridge.
4. Keep the current non-JSON boundary unchanged: JSON profile telemetry may
   nominate primitive families for S-P2, but it must not stand as proof for CSS
   L4, Sheets, or BBNF-self until a real non-JSON generated parser row exists.

## Verdict

REVISE. The V2 packet is close: the canonical bridge exists, JSON/generated/serde
symbols are mostly evidence-only, and JSON-only telemetry is not promoted to
non-JSON proof. The remaining Lock 14 issue is narrow but load-bearing: remove
the residual object/array summary vocabulary before CH2 can accept the fold.
