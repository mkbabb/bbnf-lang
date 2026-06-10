# Oracle Census — Parity and Correctness Gates Across the 9-Grammar Fleet

## Scope

Every parity and correctness oracle visible in the skinny proving tree
(`skinny/crates/`) and the totality tree (`crates/core/tests/`) is enumerated
here. For each grammar in the 9-fleet the census records the depth of
verification, the independence of the reference comparator, and the gaps. The
lock-bearing precept is precept 3: performance and correctness claims must be
independently validated, no overfitting to JSON or CSS.

The 9-grammar fleet (from the Lock-14 roster in
`crates/ir/tests/lock14_grammar_generalization.rs:26`):

1. JSON — `JsonParser`
2. CSS L4 — `CssL4Parser`
3. Google Sheets — `GoogleSheetsParser`
4. BBNF — `BbnfBootstrap`
5. CSV — `CsvParser`
6. Math — `MathParser`
7. BNF — `BnfParser`
8. EBNF — `EbnfParser`
9. CSS Pretty — `CssPrettyParser`

---

## Grammar-by-Grammar Oracle Table

| Grammar | Oracle | Depth | Independence | Key Gaps |
|---------|--------|-------|--------------|----------|
| JSON | sonic-rs full-tree walk (`json_parity.rs`); byte-canonical vs sonic-rs (`json_canonical_parity.rs`); skinny `direct_struct.rs` (12-field digest vs serde_json + sonic-rs) | **Full-tree** — field-for-field on data, twitter, citm, canada, data_xl | **Independent**: sonic-rs and serde_json are external crates | Number precision collapses to f64; skinny `parity_oracle` binary is count-only (6 buckets vs serde_json); `canonical_parity_data_xl` skipped in debug builds |
| CSS L4 | 9-field cssparser oracle (`nonjson_css_l4.rs`); lightningcss corpus admission (`lightningcss_parity.rs`); byte-canonical vs lightningcss on normalize.css (`css_l4_canonical_parity.rs`); named-color self-check (`css_l4_named_color_parity.rs`); dimension parse-reach (`css_l4_dimensions.rs`); comment probe (`css_l4_comment_probe.rs`); typed materialisation (`css_l4_parity.rs`); CSS scanner differential (`css_scan_differential.rs`) | **Stratified** — (a) 9-field population parity on 4 corpora; (b) byte-identical canonical on normalize.css only; (c) bootstrap/tailwind: admission + bbnf self-round-trip only | (a) cssparser is **independent**; (b) lightningcss is **independent**; (c) named-color oracle is **self-referential** (reads grammar source file, not a W3C table); scanner differential is **independent** (NEON vs scalar reference) | `color_channel_parity_rgb_family` has unconditional early-return when `bbnf_colors.is_empty()` (`lightningcss_parity.rs:313`) — the field-for-field RGB gate is dead code; bootstrap/tailwind have no byte-parity gate against lightningcss; lab/lch/oklch excluded from color comparator |
| Google Sheets | Typed-payload materialisation walk (`sheets_parity.rs`); prettify idempotency + re-parse (`sheets_self_parity.rs`); expression skeleton vs hand-written reference (`sheets_expr_parity.rs`); skinny conformance gate (`sheets_conformance.rs`) | **Head-level** — typed byte values per formula node; no external evaluator comparison | All four oracles are **self-referential**: `sheets_expr_parity.rs`'s `Oracle` struct is hand-written inside the test file; `sheets_conformance.rs` asserts `recognize == parse` but no external Sheets reference | Cell references and ranges are deferred to raw Span scan (`sheets_conformance.rs:34` "DEFERRED_LEAVES" characterized, not pinned); no external formula evaluator; the hand-written `RefExpr` can only catch regressions relative to itself |
| BBNF | Self-parse reach gate (`bbnf_parity.rs`); AST round-trip + prettify idempotency over all `.bbnf` fixtures (`bbnf_self_parity.rs`) | **Count-only** — rule name list non-empty; re-parse fingerprint stable | **Self-referential** in both cases — BBNF parses its own grammar source; no external BBNF reference parser exists | No external comparator is architecturally possible for a self-hosting grammar; catches serialization instability but not semantic divergences from spec |
| CSV | Admission smoke + `doc.input() == source` only (`serialize_roundtrip.rs`, `runtime_root.rs`) | **Nothing** | N/A | Zero parity gate against any external CSV library; typed cell values never asserted; no corpus-scale fixture |
| Math | Admission smoke + `doc.input() == source` only (`serialize_roundtrip.rs`, `runtime_root.rs`) | **Nothing** | N/A | Zero parity gate against any expression evaluator or alternative math parser; typed f64 output never asserted against expected value |
| BNF | Admission smoke + `doc.input() == source` only (`serialize_roundtrip.rs`, `runtime_root.rs`) | **Nothing** | N/A | Zero parity gate; no comparison against any canonical BNF parser library; structural tree shape never validated |
| EBNF | Admission smoke + `doc.input() == source` only; prettify idempotency on a single rule (`ebnf_prettify.rs`) | **Nothing** beyond single-rule prettify idempotency | N/A | `prettify_multi_rule` is `#[ignore]` (`ebnf_prettify.rs:39`) with the comment "prettify codegen stops after first rule"; no typed value checks; no external comparator |
| CSS Pretty | Admission smoke only on normalize/bootstrap/tailwind (`css_pretty.rs`, `runtime_root.rs`) | **Nothing** | N/A | Zero parity gate against `CssL4Parser` or lightningcss on the same corpora; declaration or rule count cross-check absent |

---

## Skinny-Tree Oracle Detail

### 9-Field cssparser Oracle (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`)

The load-bearing CSS correctness oracle in the skinny tree is
`assert_rich_strict_equality` (`nonjson_css_l4.rs:128`). It compares the
tape-projected Track-1 CSS L4 summary against an independent cssparser pass over
the same source. Both sides reduce to a canonical 9-field string
(`nonjson_css_l4.rs:65`):

```
rules, at_rules, qualified_rules, declarations,
selectors, dimensions, numbers, colors, functions
```

The test `rich_cssom_matches_cssparser_on_real_corpora` (`nonjson_css_l4.rs:761`)
drives this oracle against `css_l4_corpus::load_all()` — the four CDN-pinned
minified corpora (bootstrap, tailwindcss, material-components-web, animate). This
is the strongest CSS oracle in either tree: 9 structural fields, externally
sourced (cssparser), corpus-scale.

Depth qualification: the oracle is population-count equality, not per-declaration
byte comparison. The selector count uses top-level comma scanning over the raw
prelude span (`nonjson_css_l4.rs:459`), not a full CSS selector parser. The
typed-value category uses leading-byte classification (`nonjson_css_l4.rs:204`).
Both are approximations of a full parse-tree comparison. A regression that changes
node types while preserving counts would pass.

### JSON Skinny Parity (`skinny/crates/bbnf-bench/src/parity.rs`)

`assert_parity` (`parity.rs:23`) compares track1 against track2 at three
layers: tape offset arrays, flag cursors/values, and canonical serialization
string. Both sides are bbnf implementations — this is **self-referential** for
correctness against the JSON specification.

The `direct_struct.rs` harness (12-field `JsonDirectDigest`: objects, arrays,
members, elements, strings, string_bytes, numbers, bools, trues, nulls, max_depth,
fingerprint) is the most comprehensive JSON gate because it compares four
independent projections: track1, track2, serde_json, and sonic-rs. The
serde_json and sonic-rs axes are genuinely **independent**.

The `parity_oracle` binary (`xtask/src/bin/parity_oracle.rs`) compares skinny
JSON against serde_json by counting 6 node-type buckets (obj, arr, str, num,
bool, null). This is count-only — it would not catch key-value swap, string
content error, or number precision drift beyond type boundary.

### CSS Scanner Differential (`skinny/crates/runtime/tests/css_scan_differential.rs`)

`css_balanced_component_scan_matches_scalar_reference` sweeps every start
position over 4 real CSS corpora against all 4 grammar-derived STOP sets and
asserts the NEON-backed scan equals an independent scalar reference walker. This
is an **independent** gate for the SIMD scanner layer specifically. It guards
that structural decisions are bit-identical, not just that parse succeeds.

---

## Critical Deficiencies

### 1. Color Channel Gate Is Dead Code

`lightningcss_parity.rs:313` unconditionally returns early when
`bbnf_colors.is_empty()`. The comment names this as W2 pending. Until colorFn
aggregate wiring lands, `color_channel_parity_rgb_family` asserts only that
lightningcss sees 3 RGBA colours — it never touches bbnf output. The test name is
misleading: it is currently an admission-only gate for lightningcss with a
dead code path on the bbnf side.

### 2. Named-Color Oracle Is Self-Referential

`css_l4_named_color_parity.rs` loads expected `(name, hex)` pairs by parsing
the grammar source file `grammar/css/l4/color.bbnf` (`css_l4_named_color_parity.rs:21`).
The reference IS the grammar. A systematic error in the grammar's RGBA constants
(wrong value for any named color) would not be caught because both sides would
agree on the wrong value.

The test also carries a soft failure path at lines 126–131: when
`find_named_color_payload` returns a non-null result but the specific expected
u32 does not match, the test emits `eprintln!` rather than failing, silently
masking per-branch alt-payload regressions.

### 3. Bootstrap and Tailwind Have No Byte-Level Parity Gate

`css_l4_canonical_parity.rs::assert_scale_interop` (`css_l4_canonical_parity.rs:137`)
asserts four things: bbnf prettify produces non-empty output, lightningcss admits
the source, bbnf self-round-trips, lightningcss re-parses bbnf output. It does not
assert byte-identical or token-normalized canonical equality. The two largest
real-world CSS corpora (bootstrap at ~230 KB, tailwind at ~180 KB) have no
structural depth gate against an external reference — only the 2.7 KB
normalize.css receives byte-identical parity.

### 4. Five Grammars Have No Parity Oracle

CSV, Math, BNF, EBNF, and CSS Pretty have zero structural parity gate. The only
tests are admission smokes: `parse().is_ok()` and `doc.input() == source`. For
CSV especially — a format with broad external ecosystem and a clear external
reference library — the absence of a structural comparison means regressions in
typed field emission are undetectable.

### 5. Google Sheets Oracle Is Entirely Self-Referential

`sheets_expr_parity.rs` uses a `RefExpr` enum (`sheets_expr_parity.rs:32`)
defined inside the same test file. The "oracle" function (`sheets_expr_parity.rs:419`)
is a hand-written recursive descent parser. Any systematic misunderstanding shared
by both the production parser and the oracle would produce agreement on the wrong
result. No external Google Sheets formula evaluator comparison exists anywhere in
either tree.

### 6. Skinny JSON `parity.rs` Is Self-Referential on Correctness

`skinny/crates/bbnf-bench/src/parity.rs::assert_parity` compares two bbnf
implementations. Divergences from the JSON specification that both share are
invisible. The external serde_json gate in the `parity_oracle` binary is
count-only (6 type buckets), not field-for-field.

---

## Summary: Grammars With No Oracle

The following grammars have **no correctness oracle** against any external
independent reference:

- **CSV** — admission smoke only; no comparison against the `csv` crate or RFC 4180
- **Math** — admission smoke only; typed f64 output never compared against any evaluator
- **BNF** — admission smoke only; structural shape never validated externally
- **EBNF** — admission smoke; multi-rule prettify is `#[ignore]`; no external comparator
- **CSS Pretty** — admission smoke; no cross-check with CssL4Parser on shared corpora

These five grammars account for more than half the Lock-14 fleet. Each can
produce structurally wrong output (dropped rules, wrong typed values, inverted
branches) and pass all existing tests.

---

## Recommendations

1. Rekey the named-color test against the W3C CSS Named Color specification
   table rather than the grammar source file. A one-time generated fixture from
   the W3C table gives independence. The current `every_named_color_materialises_its_u32_payload`
   test also must hard-fail rather than `eprintln!` on value mismatch.

2. Add a CSV parity gate against the `csv` crate (`csv::ReaderBuilder`) for at
   least one multi-row, quoted, escape-bearing corpus. Assert cell text
   field-for-field, not just row count.

3. Activate or delete `color_channel_parity_rgb_family`. As written it provides
   zero correctness coverage for bbnf. Wire colorFn aggregate emission so
   `bbnf_colors` is non-empty, or delete the test until that wiring lands.

4. The bootstrap/tailwind CSS canonical parity gap is self-documented as blocked
   by `calc()` evaluation and property-order differences. The gestalt transposition
   is to introduce a grammar-derived semantic normalizer that factors out
   the differences, rather than permanently capping depth at normalize.css.
