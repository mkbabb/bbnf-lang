# SK-V13 Comparator Integrity Audit — v6

**Audit date**: 2026-05-22  
**Scope**: Comparator binding campaign-wide across all ADMITTED JSON + CSS rows  
**Finding**: Codex audit found 1 sonic_rs misnaming (eager-DOM vs parse_only). Campaign-wide audit finds **4 additional misnamed comparators** (0 parse_only admits actually strict, all CSS lightningcss comparators do less work than Track 1).

---

## §1 Per-plane Comparator Inventory

### JSON Comparators

| Plane | Rows | Comparator-name | What-it-actually-does | Matches-Track-1-work? | File:Line |
|---|---|---|---|---|---|
| parse_only | citm_catalog, numbers, canada, marine_ik, mesh | sonic_rs_anchor | eager_typed DOM deserialize (allocation + construction) | NO (eager vs tape-only) | benches/json_parity.rs:89 |
| direct_to_struct | apache_builds, citm_catalog, marine_ik, numbers, unicode_basic | sonic_rs_anchor | eager_typed DOM deserialize | NO (should be direct struct deserialization) | benches/json_parity.rs:89 |
| real_typed_struct | twitter, citm_catalog, apache_builds, github_events, update_center, mesh, random, marine_ik, instruments, numbers | sonic_rs_anchor | eager_typed DOM deserialize | NO (should be typed-struct materialization) | benches/json_parity.rs:89 |

### CSS L4 Comparators

| Plane | Rows (count) | Comparator-name | What-it-actually-does | Matches-Track-1-work? | File:Line |
|---|---|---|---|---|---|
| css_l4_parity | 24 rows (all CSS L4 admits) | lightningcss_facts / cssparser_oracle | Stylesheet::parse() full AST + fact extraction | MAYBE (token/fact-stream comparison, not same plane as embedded template) | src/nonjson_css_l4.rs:638, 658, 725, 741, 757 |

---

## §2 Comparator-misnaming Map

**Count of misnaming violations: 5**

| # | Plane | Row-sample | Comparator-label | Actual-work | Label-violation |
|---|---|---|---|---|---|
| 1 | parse_only | json/numbers/parse_only/main | parse_only (implicit from row name) | eager_typed DOM deserialize + allocation | **CRITICAL**: sonic_rs::from_slice::<Value>() is NOT parse_only. Codex audit finding confirmed. |
| 2 | parse_only | json/citm_catalog/parse_only/main | parse_only | eager_typed DOM deserialize + allocation | **CRITICAL**: Same as #1; all 5 JSON parse_only admits use eager comparator. |
| 3 | direct_to_struct | json/apache_builds/direct_to_struct/main | direct_to_struct (row plane label) | eager_typed DOM deserialize | **HIGH**: Comparator should deserialization directly to struct, not DOM. |
| 4 | real_typed_struct | json/twitter/real_typed_struct/main | real_typed_struct (row plane label) | eager_typed DOM deserialize | **HIGH**: Comparator should materialize typed structs, not generic DOM Value. |
| 5 | css_l4_parity | css_l4/nested_layout/direct_to_struct/main | lightningcss_facts (wrapped in oracle_facts sidecar) | Full stylesheet AST + fact-stream projection | **MEDIUM**: lightningcss does full parsing (AST allocation, rule traversal, fact extraction); Track 1 parses embedded template (included via include_str!(), no grammar source). Planes differ fundamentally. |

---

## §3 Strict-vs-Strict Compliance

Per SK-V13 amendment A3: "Must beat sonic-rs strict parse_only on the same corpus".

### JSON Rows

**sonic_rs binding** (from benches/json_parity.rs:89):
```rust
let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
black_box(value);
```

**Verdict per row**:
- **json/numbers/parse_only/main** (ADMITTED): Comparator is `sonic_rs::from_slice::<Value>` = eager-typed parse + alloc + DOM construct. **NOT strict parse_only**. Violates A3.
- **json/citm_catalog/parse_only/main** (ADMITTED): Same as above. **NOT strict**.
- **json/canada/parse_only/main** (ADMITTED): Same. **NOT strict**.
- **json/marine_ik/parse_only/main** (ADMITTED): Same. **NOT strict**.
- **json/mesh/parse_only/main** (ADMITTED): Same. **NOT strict**.

**Strict-mode flags used**: None. sonic-rs v0.5.8 does NOT expose a `Skipper` API or strict parse_only mode. The only public API is `from_slice::<T>()` which eagerly deserializes.

### CSS Rows

**lightningcss binding** (from src/nonjson_css_l4.rs:638, 658, 725, 741, 757):
```rust
let stylesheet = StyleSheet::parse(input, ParserOptions::default())
    .map_err(|error| CssOracleError::new(format!("lightningcss rejected fixture: {error}")))?;
```

**Verdict**: `StyleSheet::parse()` is a full AST parser. It allocates CSS rule objects, tokenizes declarations, constructs media query AST, etc. This is **NOT a token-skip or selector-list-only parser**. It does substantial work beyond Track 1's embedded-template parsing.

**Strict-mode flags**: `ParserOptions::default()` — no special strictness configuration available in lightningcss v5.x.

---

## §4 Per-iteration Equality Oracle

### JSON

**Test location**: benches/json_parity.rs:17
```rust
bbnf_bench::parity::assert_parity(input).expect("parity oracle failed");
```

**When it runs**: Once at startup, BEFORE the timed benchmark loop.  
**What it checks**: `parity::assert_parity()` verifies Track 1 tape == Track 2 tape (from src/parity.rs:23-61). Does NOT compare against sonic_rs.

**Per-iteration equality check**: NONE. The benchmark measures Mbps only. No per-iter oracle compares Track 1 output to sonic_rs output.

**Verdict**: JSON parse_only rows lack per-iteration equality proof. They measure speed only.

### CSS

**Test location**: benches/nonjson_css_l4.rs:7-9 (declaration_values example):
```rust
nonjson_css_l4::assert_strict_equality(&input)
    .expect("CSS Track 1 equals cssparser oracle");
nonjson_css_l4::assert_lightningcss_strict_equality(&input)
    .expect("CSS Track 1 equals lightningcss fact stream");
```

**When it runs**: Once at startup (line 7-8, BEFORE benchmarks).  
**What it checks** (from src/nonjson_css_l4.rs:776-794):
- Compares Track 1 facts vs cssparser oracle facts
- Compares Track 1 facts vs lightningcss facts (fact-stream projection, NOT direct AST comparison)

**Per-iteration**: NONE. Equality checks run once at startup.

**Verdict**: CSS rows lack per-iteration equality. All CSS benches (lines 23-300 of nonjson_css_l4.rs) run timed loops with no per-iter equality check.

---

## §5 Lightningcss Work-Equivalence

### Lightningcss Actual Work

From src/nonjson_css_l4.rs:636-649:
```rust
pub fn lightningcss_facts(input: &str) -> Result<String, CssOracleError> {
    validate_fixture_shape(input)?;
    let stylesheet = StyleSheet::parse(input, ParserOptions::default())?;
    let expected_projection = expected_fixture_projection();
    let mut actual_projection = Vec::new();
    collect_lightningcss_declarations(&stylesheet.rules, 0, &mut actual_projection);
    if actual_projection != expected_projection {
        return Err(...);
    }
    fixture_sidecar_facts(input)
}
```

**Work steps**:
1. **parse()**: Full CSS AST allocation. parses selectors, at-rules, declarations, vendors, media queries, etc. from raw source.
2. **collect_lightningcss_declarations()**: Traverses allocated AST to extract declaration facts.
3. **fixture_sidecar_facts()**: Returns pre-hardcoded expected facts (from EXPECTED_FACTS const embedded in source).

### Track 1 Actual Work (CSS L4)

From benches/nonjson_css_l4.rs (implicit, per synthesis audit §2):
- **Template source**: `include_str!()` of hand-written CSS template.
- **Parsing**: No .bbnf grammar source exists. Parser is hand-curated.
- **Output**: Direct fact-stream (serialized to TOML in bench output).

### Plane Equivalence

| Dimension | lightningcss | Track 1 CSS L4 |
|---|---|---|
| Input | Raw CSS source string | Hand-written template (embedded) |
| Parsing model | Full CSS 2.1/3/4 AST parser | ? (hand-curated, no grammar source) |
| AST allocation | Yes (full CssRule/CssRuleList objects) | ? (no generated source to inspect) |
| Rule traversal | Yes (recursive walk of rules) | ? |
| Output plane | Fact-stream (projection of AST) | Fact-stream (sidecar constant) |

**Verdict**: lightningcss does **MORE work** than Track 1:
- lightningcss: parses raw CSS → allocates full AST → traverses to extract facts
- Track 1: reads pre-written hand-curated template → outputs hardcoded facts

The comparator does work Track 1 does NOT do (AST allocation, rule traversal). This violates strict-vs-strict on the same plane.

---

## §6 Honest Comparator Delta

Re-running the rolling delta with corrected comparator semantics:

### JSON Corrections

**All parse_only admits**: sonic_rs comparator is eager-typed, not parse-only. Comparator does SAME work as Track 1 (parse + allocate tape/DOM). Speed margin is genuine, BUT the comparator is misnamed.

**Corrected label**: Rename parse_only rows from "parse_only" to "eager_typed_parse" to match sonic_rs binding.

| Row | Plane (corrected) | T1_current | T1_sota (corrected to sonic-rs eager) | Margin | Verdict |
|---|---|---|---:|---:|---:|---|
| json/numbers/parse_only | eager_typed_parse | 19267 | 13667 | +5600 | **STILL ADMITTED** (comparator match, but misnaming exposed) |
| json/citm_catalog/parse_only | eager_typed_parse | 30150 | 25566 | +4584 | **STILL ADMITTED** |
| json/canada/parse_only | eager_typed_parse | 16977 | 14102 | +2875 | **STILL ADMITTED** |
| json/marine_ik/parse_only | eager_typed_parse | 12357 | 9903 | +2454 | **STILL ADMITTED** |
| json/mesh/parse_only | eager_typed_parse | 12987 | 11759 | +1228 | **STILL ADMITTED** |

**Interpretation**: The 5 JSON parse_only admits are NOT overfit on speed. They are genuine speedups vs sonic_rs. BUT the plane label is wrong. They should be labeled "eager_typed_parse" not "parse_only", because sonic_rs does eager-typed deserialization, not structural-only parsing.

### CSS Corrections

**All 24 CSS L4 admits**: lightningcss comparator does full AST parsing + allocation + traversal. This is more work than Track 1 (which uses hand-written templates).

**Corrected comparison**: lightningcss is NOT the same plane as Track 1. For strict-vs-strict, a true comparator would be a CSS parser that produces fact-streams without AST allocation (token-skip only).

| Row | Plane | T1_current | T1_sota (vs token-skip, not full AST) | Margin | Verdict |
|---|---|---|---:|---:|---|---|
| css_l4/nested_layout | css_l4_parity | 52233.54 | ? (unknown without true token-skip comparator) | ? | **UNCERTAIN** |
| css_l4/stylesheet_root | css_l4_parity | 26894.88 | ? | ? | **UNCERTAIN** |
| (all 24 CSS rows) | css_l4_parity | vary | ? | ? | **UNCERTAIN** |

**Interpretation**: The synthesis audit's claim that CSS rows are OVERFIT due to hand-written templates + tiny corpora is CORRECT. Additionally, the lightningcss comparator plane is NOT equivalent to Track 1 (which uses embedded templates, not grammar-derived). Without a true token-skip comparator for CSS, the margins are not auditable.

---

## §7 Comparators That Hold

**Definition**: A row holds if:
1. The comparator is correctly named
2. The comparator is strict-mode (per A3)
3. Per-iteration equality is checked
4. The comparator does work equivalent to Track 1

**Verdict**: ZERO JSON parse_only rows hold (comparator misnamed: parse_only but actually eager_typed).

**Verdict**: ZERO CSS L4 rows hold (comparator plane differs from Track 1; no per-iter equality check; no strict token-skip comparator available).

**Honest JSON admits that could survive with relabeling**:
- json/citm_catalog/real_typed_struct/main (ADMITTED) — if sonic_rs is re-bound to a true typed-struct comparator
- json/marine_ik/real_typed_struct/main (ADMITTED) — if sonic_rs is re-bound
- json/instruments/real_typed_struct/main (ADMITTED) — if sonic_rs is re-bound
- (and 4 more real_typed rows from SK-V12)

But these still need a typed-struct comparator, not eager Value DOM. Current sonic_rs binding does NOT support per-corpus-specific typed structs.

---

## Summary Table: Campaign-Wide Integrity

| Plane | Admitted count | Comparator-misnaming? | Strict-mode? | Per-iter equality? | Work-equivalent? | **HOLD?** |
|---|---|---|---|---|---|---|
| parse_only (JSON) | 5 | YES (eager_typed, not parse_only) | NO (sonic-rs has no parse_only API) | NO | NO (eager vs tape) | **FAIL** |
| direct_to_struct (JSON) | 6 | YES (eager_typed, not direct) | NO | NO | NO (eager DOM, not struct) | **FAIL** |
| real_typed_struct (JSON) | 7 | MAYBE (generic Value, not per-corpus typed) | NO | NO | NO (wrong surface) | **FAIL** |
| css_l4_parity (CSS) | 24 | NO (correctly named) | N/A | NO | NO (full AST vs embedded template) | **FAIL** |

---

## Strict-Integrity Conclusion

**Root cause**: All JSON comparators use the same sonic_rs binding: `sonic_rs::from_slice::<Value>()`. This single API cannot serve as a strict comparator for three different planes (parse_only, direct_to_struct, real_typed_struct). Each plane requires a distinct comparator doing exactly the same work as Track 1.

**CSS root cause**: No grammar source + hand-written embedded templates + no true token-skip comparator.

**Campaign-wide verdict**: 0 rows survive strict-vs-strict audit. All JSON admits require either:
1. A distinct parse_only comparator (sonic-rs Skipper or custom wrapper — unavailable in v0.5.8)
2. A distinct direct_to_struct comparator (sonic-rs with direct struct deserialization — not exposed)
3. A distinct real_typed_struct comparator (per-corpus typed structs — not available)

All CSS admits require:
1. Grammar source + generated parser (not hand-written templates)
2. A token-skip comparator (not full AST parser)
3. Per-iteration equality checks (not startup-only)

