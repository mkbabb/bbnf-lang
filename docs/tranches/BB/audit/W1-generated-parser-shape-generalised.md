# W1 Generated Parser Shape — Generalised Across All Grammars

Date: 2026-05-03
Scope: Per-grammar generalisation of BA.W5's JSON generated-parser-shape specification per gap G of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:175-198`. BA writes the JSON-side; this document extends to all 9 grammars: construct-by-construct emission tables, per-grammar deviations, cost-model decision boundaries.

## §1 Construct-by-construct emission table

Each grammar's parser shape per construct, cross-referencing BA.W5's JSON spec at `docs/tranches/BA/audit/W5-generated-parser-shape.md` (the JSON-side of this surface).

### CSS L4

| Construct | Emission shape | Cost-model decision boundary | Anchor |
|---|---|---|---|
| Alt | char-class SIMD scan for declaration vs at-rule vs comment dispatch (cardinality ~25); speculative ordered try for selector-specific alts | structural alphabet cardinality > 6 → SIMD; cardinality ≤ 6 → byte-disjoint match | `audit/CENSUS-2026-05-03.md:464`; lightningcss precedent `audit/SOTA-2026-05-03.md:124` |
| Seq | linear push with field-binding; CSS L4 declarations bind `name`, `value`, `important?` to typed-enum variant | none (always linear) | BA.W5 baseline shape |
| Repeat | byte-skip loop for whitespace; SIMD-driven declaration block iteration; cursor consultation absent on eager path | repeat density × structural alphabet → SIMD threshold | BB.W3c cost model |
| Optional | peek-byte for `!important`, `;` terminator, vendor-prefix; commit-or-skip per BA.W5 pattern | none | same |
| CharClass | lookup table for ident chars, dimension digits, color hex; SIMD shuffle on long property names; scalar fallback for short tokens | input length ≥ 32 bytes + alphabet cardinality ≥ 8 → SIMD shuffle | `audit/SOTA-2026-05-03.md:79-82` |
| Keyword | PHF for at-rule names (@media, @supports, @keyframes, ...); small-string compare for property keywords | keyword count > 8 → PHF; ≤ 8 → linear cmp | `audit/CENSUS-2026-05-03.md:466` |
| Scanner | regex DFA for color hex (`#[0-9a-fA-F]{3,8}`); inline byte-test for `(`, `)`; bbnf-regex HIR for unicode-range | regex complexity score → DFA vs inline | BB.W3a + bbnf-regex |
| Pratt | NOT applicable to CSS L4 (no operator chains) | n/a | Lock 10 |
| HostCall | `parse_hex_color` host fn at `crates/core/src/grammar/host/css_l4.rs` (per surgery 15); `parse_url`, `parse_calc` similar | resolved at codegen via workspace metadata | G05-1 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:24` |
| MapExpr | typed-enum constructor with bound field positions: `CssTypedValue::Declaration { name, value, important }` | none (mechanical) | BA.W5 baseline |

### BBNF (self-host)

| Construct | Emission shape | Cost-model decision boundary | Anchor |
|---|---|---|---|
| Alt | byte-disjoint match for top-level rule kind dispatch (cardinality 5-8 per rule) | cardinality ≤ 6 → match | `audit/CENSUS-2026-05-03.md:441` |
| Seq | linear push with bounds-recording extension: each bound parse fn carries `record_bounds: bool` parameter | none (always linear with bounds) | BB.W1b deliverable |
| Repeat | char-class for whitespace; BBNF-specific repeat over `( | )` alts | repeat density → no SIMD (BBNF inputs typically small) | BB.W3c cost model |
| Optional | peek-byte for `?`, `*`, `+` quantifiers | none | BA.W5 pattern |
| CharClass | lookup table for ident chars, regex special; scalar | input length typically < 64 bytes → scalar | same |
| Keyword | small-string compare for `:=`, `\|\|=`, `?`, `*`, `+` | keyword count ≤ 6 → linear | n/a |
| Scanner | regex DFA for embedded regex literals; bbnf-regex HIR delegate | always DFA (regex inputs are validated) | bbnf-regex |
| Pratt | left-recursive operator chains (e.g., `binary_factor := factor (op factor)*`) → Pratt emission | left-recursion + closed op-set → Pratt | BB.W3c, Lock 10 |
| HostCall | `bbnf_pre_canon`, `bbnf_post_canon` host fns at per-grammar host namespace | resolved at codegen | same |
| MapExpr | typed-enum constructor; bounds-binding: `BbnfTypedValue::Rule { name, body, bounds }` | none | BB.W1b |

### Sheets

| Construct | Emission shape | Cost-model decision boundary | Anchor |
|---|---|---|---|
| Alt | byte-disjoint match for cell_ref vs identifier vs sheet_prefix vs error dispatch | cardinality 4 → match | `audit/CENSUS-2026-05-03.md:485-490` |
| Seq | linear push; specialised leaf-deposit at host-fn site | none | BB.W1c |
| Repeat | char-class for whitespace; specialised repeat over `:` range, `,` arg-list | repeat density × spreadsheet input length → SIMD threshold | BB.W3c |
| Optional | peek-byte for `$` (absolute-ref marker), `:` (range marker) | none | same |
| CharClass | lookup table for cell_ref chars (A-Z + 0-9 + $); scalar for short refs (typical) | input length ≥ 16 bytes → SIMD shuffle | same |
| Keyword | PHF for built-in functions (SUM, AVERAGE, INDEX, ...); ≥ 200 names | keyword count > 50 → PHF | `audit/CENSUS-2026-05-03.md:489` |
| Scanner | bbnf-regex HIR for cell_ref regex (`\$?[A-Z]+\$?[0-9]+`) | DFA always | bbnf-regex |
| Pratt | NOT applicable (Sheets has no expression chains; arithmetic is per-cell) | n/a | Lock 10 |
| HostCall | `normalise_cell_ref`, `resolve_identifier`, `lookup_sheet_prefix` at `crates/core/src/grammar/host/google_sheets.rs` (per surgery 15 + G05-9) | resolved at codegen | G05-9 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:32` |
| MapExpr | typed-enum constructor with normalised cell_ref: `SheetsTypedValue::CellRef { col, row, absolute_col, absolute_row, sheet_prefix }` | none | BB.W1c |

### BNF, CSV, EBNF, CSS Pretty, Math (cohort)

Cohort grammars share the templated emission shape per BB.W2a; per-grammar deviations are in the leaf-kinds set, not in the emission mechanism.

| Construct | Cohort shape | Notes |
|---|---|---|
| Alt | byte-disjoint match for cohort top-level dispatch (cardinality ≤ 8 per grammar) | All cohort grammars have small dispatch alphabets |
| Seq | linear push (templated `SimpleStructBuilder` substrate per `audit/MODULES-2026-05-03.md:923`) | Templated; no per-grammar specialisation |
| Repeat | byte-skip loop for whitespace; cohort-specific repeat (e.g., CSV `,`-separated fields) | Templated `parse_repeat` fn |
| Optional | peek-byte; commit-or-skip | Templated |
| CharClass | scalar lookup table; cohort inputs typically small | No SIMD (per `simd_threshold_bytes` cost-model) |
| Keyword | small-string compare; cohort grammars have ≤ 16 keywords each | Linear cmp |
| Scanner | inline byte-tests for cohort delimiters (CSV `","`, BNF `:=`, EBNF `;`) | No regex DFA |
| Pratt | Math grammar's expression rule MAY classify as Pratt at BB.W3c (BinOp shape: `expr := factor (op factor)*`); other cohort grammars do not | Cost-model decides per grammar |
| HostCall | empty (cohort grammars have no host fns by definition per BB.W2a §6) | n/a |
| MapExpr | templated typed-enum constructor: `<G>Value::Field { ... }` | Mechanical; per `crates/core/src/codegen/runtime_template.rs` |

### JSON

JSON's per-construct table is the BA.W5 baseline at `docs/tranches/BA/audit/W5-generated-parser-shape.md` (BA-side anchor). BB does not re-specify; BB.W2b's cursor unification and BB.W4a's three-surface API extend the JSON shape uniformly across grammars without modifying the JSON construct emission.

## §2 Per-grammar deviations from JSON pattern

The deviation table records where each grammar diverges from the JSON shape:

| Grammar | Deviation | Mechanism |
|---|---|---|
| CSS L4 | Color-function emission: `rgb(r, g, b)`, `rgba(r, g, b, a)`, `hsl(h, s%, l%)`, `oklch(L C H)`, `color-mix(in <space>, c1, c2)` each route through host-fn dispatch; the host fn at `crates/core/src/grammar/host/css_l4.rs::parse_color_function` is called from the per-rule parse fn | Per surgery 15 + G05-1; the host fn lives at per-grammar namespace not generic root |
| BBNF | Bounds-recording extension: each bound parse fn carries `record_bounds: bool` parameter; the BbnfStructBuilder consumer reads bounds from the parse-fn return value, not from a per-frame slot | Per BB.W1b §1; LSP consumer at `crates/analysis/` reads bounds for source-position diagnostics |
| BBNF | Left-recursive operator chain (`binary_factor`) routes through Pratt emission per BB.W3c | Pratt detection at `recognizers/operator_chain.rs` mines left-recursive Alt with `factor (op factor)*` shape |
| Sheets | Cell_ref normalisation: the per-rule parse fn calls `normalise_cell_ref(input_slice) -> NormalisedRef`; the SheetsStructBuilder consumer reads the normalised value from the parse-fn return | Per BB.W1c §1; `normalise_cell_ref` host fn at per-grammar namespace |
| Sheets | Function-name PHF: the function-call dispatch reads from a 200+ entry PHF (SUM, AVERAGE, INDEX, ...); the PHF lookup is the dominant Sheets-specific emit cost | Generated at xtask regen; the PHF table is committed source per Lock 6 |
| BNF/CSV/EBNF/CSS Pretty/Math | Templated emission per BB.W2a; no grammar-specific emit shape; the per-grammar variation is in the leaf-kinds enumeration only | Cohort discipline; per-grammar host fns empty |
| Math | If `binary_op` rule classifies as Pratt at BB.W3c, the templated emission swaps to `parse_pratt_math_expression` for that rule only; the rest of the cohort emission shape unchanged | Pratt detection per cost model; cohort exception is per-rule, not per-grammar |
| JSON | (the baseline; no deviation) | n/a |

## §3 Cost-model decision boundaries per grammar

The cost model at `crates/egraph/src/extract.rs` reads the per-grammar facts (FIRST set, structural alphabet, rule depth, recursion shape) and decides per-rule emission strategy. The decision boundaries:

| Decision | Input fact | Threshold | Output |
|---|---|---|---|
| Alt match vs SIMD scan | structural alphabet cardinality | > 6 → SIMD; ≤ 6 → match | per-rule `AltStrategy` |
| CharClass scalar vs SIMD shuffle | input length × alphabet cardinality | length ≥ 32 + cardinality ≥ 8 → SIMD | per-rule `CharClassStrategy` |
| Keyword PHF vs linear cmp | keyword count | > 8 → PHF; ≤ 8 → linear | per-rule `KeywordStrategy` |
| Scanner regex DFA vs inline byte-test | regex complexity score | > 5 nodes → DFA; ≤ 5 → inline | per-rule `ScannerStrategy` |
| Pratt vs recursive-descent | left-recursive shape + op-set closure + chain depth | left-recursion + closed op-set + depth ≥ 2 → Pratt | per-rule `PrattStrategy` |
| HostCall direct vs via dispatch table | host fn count per grammar | > 5 → dispatch table; ≤ 5 → direct call | per-grammar `HostFnStrategy` |
| MapExpr typed-enum vs builder | grammar shape (cohort vs specialised) | cohort → templated `SimpleValue`; specialised → typed-enum | per-grammar `MapStrategy` |

The cost model does not branch per grammar identifier; it branches on grammar facts that the layout-lowering pass + miner outputs produce. This honours grammar-authoritative discipline per Lock 5 + carry BA→BB.C5.

## §4 Cross-reference to BA.W5 JSON spec

Each construct row above cross-references the BA.W5 baseline at `docs/tranches/BA/audit/W5-generated-parser-shape.md`:

- BA.W5 specifies: Alt (byte-disjoint match for JSON top-level: `{`, `[`, `"`, digit, `t`, `f`, `n`); Seq (linear push to JsonObject / JsonArray); Repeat (byte-skip whitespace + comma-separated elements); Optional (peek for trailing comma per RFC); CharClass (digit / non-zero-digit / hex-digit lookup tables); Keyword (`true`, `false`, `null` small-string compare); Scanner (no regex; all inline byte-tests); HostCall (none for JSON); MapExpr (`JsonValue::Object { entries }`, `JsonValue::Array { elements }`).
- BB.W1{a,b,c} + BB.W2a inherit this shape; the per-grammar deviations in §2 record where each diverges. For grammars with no deviation row (e.g., cohort grammars at the construct level), the JSON pattern applies verbatim through the templated emission.

## §5 SOTA validation

The shape decisions cite primary-source SOTA per `audit/SOTA-2026-05-03.md`:

| Decision | SOTA anchor |
|---|---|
| Direct-to-struct (no tape) | sonic-rs `audit/SOTA-2026-05-03.md:30-44`, `audit/SOTA-2026-05-03.md:198-214` |
| SIMD structural alphabet scan | simdjson `audit/SOTA-2026-05-03.md:79-89`; sonic-rs `audit/SOTA-2026-05-03.md:42` |
| Visitor + bitflag pruning | lightningcss `audit/SOTA-2026-05-03.md:104-118` |
| Slice-borrow primary | lightningcss `audit/SOTA-2026-05-03.md:122-123` |
| Pratt operator-chain | pest + chumsky `audit/SOTA-2026-05-03.md:161, 180` |
| Lazy-value materialisation | sonic-rs `LazyValue<'a>` `audit/SOTA-2026-05-03.md:33-42` |
| Per-record macro emission | lightningcss `define_properties!` `audit/SOTA-2026-05-03.md:99-101, 280-285` |

The per-grammar generalisation does not extend the SOTA surface; it instantiates the SOTA-anchored decisions for each grammar's construct emission.
