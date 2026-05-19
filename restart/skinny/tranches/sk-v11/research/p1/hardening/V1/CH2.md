# SK-V11 S-P1 V1 CH2: Generality / Lock 14

Disposition: REVISE.
Date: 2026-05-19.
Lens: CH2 GENERALITY / Lock 14.
Scope: assess the S-P1 P1-A through P1-F profile packet, W0 baseline,
`skinny/RESULTS.md`, and the cited source for grammar-neutral hot-leaf
attribution. This file does not assess correctness, cost, hidden coupling, or
paper-close except where they affect Lock 14.

## Findings

### CH2-1 - REVISE: tiny-string leaves need one canonical primitive

P1-E mostly does the right thing by naming `string_tiny_scan` instead of treating
`runtime::generated_json::generated::match_tiny_plain_string_with_cap` as the
primitive (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:76`).
However, the same byte primitive is then split into implementation-surface
clusters: `track2_tiny_scan`, `direct_hand_tiny_scan`, and `typed_string_scan`
(`p1e-hot-leaf-attribution.md:77` through `:79`). P1-B uses the same split as
`tiny_string`, `hand_tiny`, and `typed_tiny` in its source map
(`p1b-samply-mode-2.md:88`, `:91`, `:114`).

The cited source shows these are variants of the same bounded plain/delimited
string scanner, not separate grammar primitives: the generated parser scans
until quote, slash, or control (`skinny/crates/runtime/src/grammars/json/generated.rs:171`);
Track 2 performs the same check (`skinny/crates/bbnf-bench/src/track2/json.rs:314`);
the hand direct parser does the same cap-limited scan
(`skinny/crates/bbnf-bench/src/direct_struct.rs:565`); and the typed parser's
`tiny_plain_string_end` / `skip_plain_string_end` are the same class with
different caps (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811`,
`:1825`).

Required fold: make the canonical primitive something like
`bounded_plain_string_scan` or `delimited_string_scan`. Keep generated,
Track 2, hand, and typed symbols as evidence rows or source-map members under
that primitive, not as peer primitive classes.

### CH2-2 - REVISE: JSON object/array labels leak into load-bearing summaries

P1-B globally maps hot leaves to `object_walk` and `array_walk`
(`p1b-samply-mode-2.md:89` through `:90`) and then summarizes numeric direct
rows as "digit-scan plus array-walk rows" (`p1b-samply-mode-2.md:152` through
`:154`). That is a Lock 14 vocabulary leak: object/array are JSON roles, while
the portable question for S-P2 is container/sequence dispatch or structural
rediscovery.

P1-E has the better vocabulary: `dispatch_walk` and `structural_rediscovery`
(`p1e-hot-leaf-attribution.md:86` through `:87`), and it warns that these leaves
may not reopen sidecar/class-column/streaming-cursor routes
(`p1e-hot-leaf-attribution.md:220` through `:231`). The source supports that
split. `parse_object_value_at_direct` and `parse_array_element_at_direct` are
generated JSON direct functions (`skinny/crates/runtime/src/grammars/json/generated.rs:468`,
`:508`), while the reusable pressure is the container/structural movement:
`consume_structural`, `consume_container_next`, and whitespace-backed dispatch
(`skinny/crates/runtime/src/grammars/json/generated.rs:292`, `:310`;
`skinny/crates/bbnf-bench/src/track2/json.rs:253`, `:271`).

Required fold: replace load-bearing `object_walk` / `array_walk` summaries with
`container_dispatch`, `sequence_element_dispatch`, or
`structural_rediscovery`. Preserve the JSON object/array function names only as
row-local source evidence.

### CH2-3 - ACCEPT: generic byte primitives are mostly named correctly

The packet correctly treats several hot leaves as grammar-neutral byte
primitives where the source justifies it:

- whitespace skip: `skip_ascii_whitespace` / `skip_ascii_spaces`
  (`p1e-hot-leaf-attribution.md:85`; `skinny/crates/parse-that-regex/src/lib.rs:113`,
  `:128`);
- full string scan and escape decode:
  `match_string_at_quote_trusted_utf8`, `skip_string_plain_trusted`,
  `validate_string_escape`, `validate_unicode_escape_run`, `unescape_string`,
  `read_hex_unit_scalar`, and `hex_nibble`
  (`p1e-hot-leaf-attribution.md:80` through `:82`;
  `skinny/crates/parse-that-regex/src/lib.rs:162`, `:284`, `:347`, `:547`,
  `:718`, `:945`, `:959`);
- number digit/span/materialization:
  `match_number_span_from_first`, `scan_digit_run`, `parse_eight_digits`,
  `materialize_u64`, and `materialize_f64`
  (`p1e-hot-leaf-attribution.md:83` through `:84`;
  `skinny/crates/parse-that-regex/src/number/mod.rs:38`, `:106`, `:214`,
  `:247`, `:261`);
- SIMD movemask: `bbnf_simd::aarch64::movemask::movemask_u8x16`
  (`p1e-hot-leaf-attribution.md:88`;
  `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`).

These are acceptable as S-P2 antecedents, subject to the required fold above and
subject to non-JSON proof before any generic implementation claim.

### CH2-4 - ACCEPT: JSON-only evidence is not promoted to non-JSON proof

The profile packet does not claim CSS L4, Sheets, or BBNF-self performance from
the JSON-only profile. P1-F explicitly says all 41 manifest rows are JSON domain
rows and that no CSS L4, Sheets, or BBNF-self telemetry exists in W0
(`p1f-results-delta.md:196` through `:197`). W0 freezes a JSON baseline
(`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:5`) and limits
the result surface to parse, direct, and typed JSON families
(`W0-open-baseline.md:26` through `:30`). `skinny/RESULTS.md` identifies Track 1
as `runtime::generated_json::parse` and Track 2 as a hand-coded parser over the
tape (`skinny/RESULTS.md:144` through `:145`).

The non-JSON requirement remains external and executable, not inferred from P1:
SYNTHESIS requires one admitted, benchmarked non-JSON generated direct or typed
parser intervention, and rejects a Lock 14 prose proof as close
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:56` through `:60`,
`:151` through `:166`). HANDOFF carries the same expectation for CSS L4,
Sheets, or BBNF-self (`restart/skinny/tranches/sk-v11/HANDOFF.md:72` through
`:83`, `:105` through `:112`).

## Required Fold

1. Add a canonical CH2 vocabulary bridge to the P1 packet, preferably in P1-E:
   canonical primitive first, then implementation-specific symbols as evidence.
   At minimum it must cover `bounded_plain_string_scan`,
   `string_escape_decode`, `unicode_escape_hex_decode`, `number_digit_span`,
   `ascii_whitespace_skip`, `simd_movemask`, `container_dispatch` /
   `structural_rediscovery`, and `output_digest_hash`.
2. Rewrite P1-B and P1-E load-bearing summaries so JSON role labels
   (`object_walk`, `array_walk`, generated JSON paths, Track 2 JSON paths,
   serde_json oracle paths) are evidence members under the canonical primitive,
   not primitive names.
3. Preserve the existing JSON row names, generated paths, source file:line
   citations, and per-row percentages. The fold is an attribution/vocabulary
   repair, not a request to discard evidence.
4. Keep the P1 packet's non-JSON boundary explicit: JSON profile evidence may
   nominate candidate primitive families for S-P2, but it must not assert that
   the family works for CSS L4, Sheets, or BBNF-self until a real non-JSON
   generated parser row exists.

## Verdict

REVISE. The packet is not rejected: it has usable hot-leaf data, correctly marks
diagnostic/nonproducer surfaces, and does not claim non-JSON proof from JSON-only
telemetry. The required V2 fold is to canonicalize hot leaves to
grammar-neutral primitives and quarantine JSON/generated/serde path labels as
row-local evidence.
