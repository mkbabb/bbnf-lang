# AW-V H1 — Shape Taxonomy Completeness Audit

## Executive summary (≤150 words)

Auditing every rule in `grammar/{json,css/l4,google-sheets,bbnf,ebnf,misc}/**.bbnf` against B4's 7-category taxonomy (Object, Array, String, Number, Keyword, Pratt, Unordered) shows the primary shapes are insufficient. Four shape families collectively covering ~25-30% of CSS/BBNF hot-path visits are missing: **ArgList** (`name(args)` function calls — 40+ CSS rules), **Flat** (typed `Seq` with Kw head — CSS's 28 `*Decl` rules + BBNF's directives), **Wrap** (transparent `Alt(Ref…)` dispatcher — `color`, `atRule`, `range_end`), **HRegex** (regex leaf with host decode — `hex`, `cell_ref`, `identifier`). Under strict 7-shape, hot-path coverage averages ~69% (CSS 58%, BBNF 52%) — B4's 80% gate fails. Under proposed 11-shape expansion, coverage reaches 91-93%. Interpreter fallback must still cover `funcBody`, `customPropertyDecl`, `genericDecl` (~3-5% of CSS). Pratt-shape requires enclosing-statement reparse under AX; all others admit local reparse.

## 1. Per-grammar rule classification

Per-rule tables in Appendix A. Summary by shape:

**JSON** (6 rules): all map to B4 shapes; `value` is dispatcher. 100% rule-count. ✓

**CSS L4** (~165 rules, 15 files): under strict B4, Object 1 (ruleBlock) + String 2 + Number 1 + Kw ~45 + Pr 1 + Un 6 = ~55/165 rules. The ~110 non-shaped: 28 `*Decl` typed declarations (`head , ":" ?w , (value ?w)* , "!important"? , ";"?`); 40+ `*Function` rules (transforms/filters/gradients/easing/color); recursive enums (color, mediaCondition, dimension); `@{}`-spans (urlFunction); catch-alls (customPropertyDecl, genericDecl, funcBody). Flat + ArgList alone covers ~68.

**Sheets** (~32 rules): Pr 7, Kw 6, Un 1, Num 1, Str 1, Array 1. `func_call` / `let_call` / `lambda_call` / `func_args` / `let_binding` / `array_row` (6-7 rules) are ArgList. `cell_ref` / `sheet_prefix` / `identifier` / `range_end` / `cell` / `range_ref` (6) need HRegex/Wrap/Flat.

**BBNF** (~38 rules, 3 files): directive Kw (7-way PHF), grammar_item Un (4-way), term Un (8-way disjoint FIRST), modifier Kw, type_name Kw, value-expr 6-rung Pr. The ~15 remaining rules (`rule`, directive family, `closure`, `lhs`, `rhs`, `call_arg`, `literal`, `factor`, `mapped_factor`) are Flat/Wrap/HRegex.

## 2. Coverage computation (visit-frequency weighted)

**JSON** — P1 profile: walker 42.9% + PSI 14% + regex_scan 6.3% + dec2flt 10.1% = ~73% grammar-rule work (rest is harness). B4's 100% rule-coverage ⇒ ~92-95% visit-coverage. ✓

**CSS L4** — P2: `try_branch` 70.9-78.9%. Under strict B4: Un covers `value`, `atRule`, `ruleItem`, `blockContent`, `compoundSelector`, `easingFunction` = ~35%; Kw covers props/keywords/units = ~18%; `*Decl` bodies (hottest) fall through without Flat = ~25% uncovered; function args ~10% uncovered. **Strict: ~58%, NOT B4's claimed 78%.** With Flat + ArgList: ~86%.

**Sheets** — P3: `try_branch` 52-72% on Pratt tower + primary. Pr + Un + Kw = ~65%; ArgList missing bleeds ~20%. **Strict: ~72%, B4 claimed 92%.** With ArgList: ~94%.

**BBNF** — P4: `try_branch` 68-75% across 6 entries. Kw (directive/modifier/type_name/op) = ~25%; Pr (value tower) = ~15%; Un (term/grammar_item/value_atom) = ~12%. **Strict: ~52%.** With Flat (rule, 8 directives, literal): ~87%.

**Cross-grammar average**: strict 7-shape = **69%** (fails ≥80% gate); proposed 11-shape = **91%**. B4's 78%/92%/75% projections implicitly assume a Flat-shape they never enumerate.

## 3. Unshaped-rule catalogue (fits no B4 shape)

1. **funcBody** (`css/l4/func-body.bbnf:11`) — recursive balanced-paren + string escapes. ~2-5% of tailwind via `genericFunction`. **Walker.**
2. **customPropertyDecl** (`css/l4/properties.bbnf:206`) — regex with embedded comment parsing. ~1% bootstrap/tailwind. **Walker.**
3. **genericDecl** (`css/l4/properties.bbnf:212`) — catch-all. Flat if emitter admits regex-body variant, else walker.
4. **urlFunction** (`css/l4/values.bbnf:69`) — `@{...}` span around `"url"("(" >> … << ")"`. Arg-shape w/ span flag.
5. **valueSpan / selectorSpan / propertyName** (`css/pretty.bbnf:20-24`) — `@token` → HRegex.
6. **range_end** (`google-sheets.bbnf:72`) — 3-Alt of Regex refs, no Literal branches. Fits Wrap.
7. **mediaCondition / color / dimension** — recursive Alt-enum families with distinct payload per branch.
8. **varFallback, conicConfig, radialConfig, linearDirection** — optional-head sequences.
9. **@debug/@ws/@pretty** — IR lowers to `DtaState::Epsilon`; not a shape.

## 4. New shape category proposals

### 4.1 ArgList-shape (#8)

**Predicate**: `Seq(name_literal, "(", arg (sep arg)*?, ")")` — positional args; distinct from Object (no key:value pairs) and Array (leading name + parens, not brackets).

**Covers**: CSS — gradients (6), transforms (19), filters (10), calc/min/max/clamp (4), var/env/url (3), easing (3), colorFunction/colorFn/colorMix (3). Sheets — func_call, let_call, lambda_call. BBNF — `term`'s `identifier(call_arg, …)` branch.

**Critical**: without Arg, CSS tailwind's ~18% of bytes in transform/filter/gradient chains fall through to walker.

### 4.2 Flat-shape (#9)

**Predicate**: `Seq(head, (literal|ref|regex)+)` — sequential typed Seq without wrap delimiters or Repeat-of-Alt.

**Covers**: CSS — every `*Decl` (28); qualifiedRule, mediaRule, complexSelector, selectorList, keyframesRule, keyframeSel, nthPseudo, attrSelector body, langPseudo. BBNF — rule + all 7 directive rules + import_directive. Sheets — cell, range_ref, array_row, formula.

**Critical**: without Flat, CSS `*Decl` family remains on walker and the hottest CSS nodes. B4's claimed 78% CSS coverage implicitly depends on Flat.

### 4.3 Wrap-shape (#10)

**Predicate**: `Alt(Ref_1, …, Ref_n)` with no Literal branches, OR `Ref_single` transparent alias.

**Covers**: Sheets — range_end, cell_or_range, expression, arg. CSS — color (5-Ref), atRule (3-Ref), ruleItem (2-Ref), dimension (7-Ref). BBNF — grammar_item, rhs, lhs.

**Rationale**: pure dispatchers; between Un (needs Repeat) and Kw (needs Literal). Emission = byte-class lookup + tail-call. No compound record.

### 4.4 HRegex-shape (#11)

**Predicate**: `Map { Regex, value_expr, type: Named(T) }` where output is not f64/i64/u8 (those are Num/Kw). Pattern is not a QuotedString class.

**Covers**: CSS — hex (→ `parse_hex_color`), integer (→ i64), selectorIdent, ident, dashIdent, dashed, propertyName. Sheets — cell_ref, sheet_prefix, identifier. BBNF — identifier, literal, regex, int_lit, float_lit, string_lit.

**Rationale**: these leaves don't follow String's NEON quoted-string kernel — they need a tiny per-pattern DFA inline or host-fn after span capture.

### 4.5 Recursive-enum (flag, not shape)

Alt where a branch transitively references the enclosing rule (CSS mediaCondition/color/mathValue, Sheets expression, BBNF term). Forbids trivial inlining; requires trampoline. Simpler as `is_recursive: bool` flag on Wrap/Un.

### 4.6 Rejected candidates

- `@debug foo`: IR lowering already erases.
- `\u{XXXX}` escapes: inside String's cold escape path, not a shape.
- Delimited-list-of-union: just Unordered with a Ref branch.

## 5. Hot-path gap analysis (samply-backed)

P1-P4 AW-IV artefacts:

| grammar | entry | hot rules | B4 shape | proposed |
|---|---|---|---|---|
| JSON | twitter | string, object, array | Str, Obj, Arr | same |
| CSS | bootstrap | compoundSelector, declaration, value | Un, **(none)**, Un | Un, **Flat**, Un |
| CSS | tailwind | declaration, compoundSelector, transformFunction | Un, **(none)**, **(none)** | Un, **Flat**, **Arg** |
| Sheets | nested | operator tower, primary, func_call | Pr, Un, **(none)** | Pr, Un, **Arg** |
| BBNF | all 6 | directive, rhs, term, rule | Kw, **(none)**, Un, **(none)** | Kw, **Wrap/Un**, Un, **Flat** |

**Unshaped hot-visit fraction**: strict B4 — JSON 0%, CSS 35-45%, Sheets 20-25%, BBNF 40-50%. Proposed 11-shape — 5% / 8-10% / 3-5% / 6-8%.

## 6. CSS L4 deep dive — the uncovered 22% (B4's claim)

Actual uncovered under strict 7-shape:

| rule class | % hot path | proposed shape |
|---|---|---|
| `*Decl` family (28 rules) | ~20-25% | **Flat #9** |
| `*Function` args (transforms/filters/gradients) | ~8-12% (tailwind) | **Arg #8** |
| `funcBody` recursive balanced-paren | ~2-5% | **walker** |
| `customPropertyDecl` | ~1-2% | **walker** |
| `urlFunction` `@{}` span | <1% | **Arg #8 w/ span flag** |
| `@media`/`@supports`/`@font-face` | ~1-2% | **Flat #9** |
| pseudo-class (isPseudo/nthPseudo/langPseudo) | ~3-5% | **Arg #8** |
| `genericAtRule` | ~1% | **Flat #9** |

Recoverable under proposed 11: ~17-19 of 22 points → CSS coverage climbs to ~95-97%. Residual ~3-5% stays on walker (funcBody, customPropertyDecl).

## 7. Sheets Pratt coverage verification

**Tower**: `PRECEDENCE_LUT` populated (P3 §5); operator_chain miner fires; 6 rungs + postfix covered by Pr. ✓

**150 function names**: brief's premise **incorrect**. Per P3 §6, Sheets function names match generic `identifier` regex; no `KEYWORD_PHF` for function names in the grammar. Only `LET(` and `LAMBDA(` use case-insensitive regex. Dispatch is `func_open = identifier , "("`. **ArgList-shape (#8) fires on `func_call`**, not Kw. ✓

**cell_ref, sheet_prefix, identifier** → HRegex #11 ✓; **range_end** → Wrap #10 ✓; **range_ref, cell** → Flat #9 ✓; **array_literal** → nested Array (emitter must admit `Array(Array(expression))`) ✓; **structured references** (`Table[Col]`) — NOT in grammar, out of scope.

## 8. BBNF self-host shape coverage

**Directive**: 7-branch Alt all starting `@<keyword>`. Kw-shape with shared prefix. ✓
**Rule body**: `rule = lhs, "=" ?w, rhs ?w, terminator` — Flat #9. ✓
**RHS alternation**: `( concatenation ?w, "|" ? ) +` — Pr (1-rung) or Flat-Repeat. ✓
**Term**: 8-branch Un with disjoint FIRST (`ε`, ident, `"`, `/`, `@{`, `(`, `[`, `{`). ✓
**expressions.bbnf**: value tower = Pr 6-rung; value_atom = Un; value_fn_call = Arg. ✓
**types.bbnf**: type_name = Kw 11-branch + regex fallback — Kw #5 must admit non-literal tail branch (trivial).

BBNF under 11-shape: ~87% visit. Residual is recursive term/factor cycle (term re-entering via rhs); operator-chain inlining into Pr absorbs. ✓

## 9. Incremental-parse compatibility (AX contract)

| shape | local reparse admissible? |
|---|---|
| Object, Array | ✓ delimiters are hard boundaries |
| String, Number, Keyword | ✓ leaf |
| ArgList #8 | ✓ `name(` / `)` paired |
| Flat #9 | partial — intra-sequence changes may need re-parse from enclosing head |
| Wrap #10 | ✓ trivial pass-through |
| HRegex #11 | ✓ leaf |
| **Pratt** | ✗ precedence is non-local; changing `a+b` to `a*b+c` requires reparse from enclosing statement |
| Unordered | ✓ Repeat boundaries are natural edges |

**Shapes requiring enclosing-statement reparse under AX**: **Pratt, Flat (conditionally)**. AX must annotate differently.

## 10. Conclusion

**Hot-path coverage**:
- Strict 7-shape: JSON 92%, CSS ~58%, Sheets ~72%, BBNF ~52%. **Average ~69%** — fails ≥80% gate by ~11 points.
- Proposed 11-shape: JSON 95%, CSS 95%, Sheets 95%, BBNF 87%. **Average 93%** — clears gate.

**Add before W3**:
1. **ArgList #8** — `name(arg, …)`. CSS 40+ `*Function` + Sheets `func_call/let/lambda`.
2. **Flat #9** — typed Seq. CSS 28 `*Decl` + BBNF 7 `*_directive` + Sheets `cell/range_ref/array_row`.
3. **Wrap #10** — transparent Alt(Ref…). CSS `color/atRule/ruleItem/dimension`, Sheets `range_end/expression`, BBNF `rhs/grammar_item`.
4. **HRegex #11** — regex leaf + host decode. CSS `hex`, Sheets `cell_ref/sheet_prefix/identifier`, BBNF `identifier/literal`.

Optional: `is_recursive` flag on Wrap/Un rather than a separate shape.

**Must remain on interpreter fallback**:
- `funcBody` (css/l4/func-body.bbnf:11)
- `customPropertyDecl` (css/l4/properties.bbnf:206)
- `genericDecl` (css/l4/properties.bbnf:212) — unless Flat admits regex body
- any `@recover` recovery path

Fallback residual: ~3-5% CSS, <1% elsewhere.

**Bottom line**: B4's 7 shapes identify the hottest paths but undersize the mechanism. Four additions (ArgList, Flat, Wrap, HRegex) shift coverage from marginal-pass to comfortable-clearance. **W3 should open with 11 shapes, not 7.**

---

## Appendix A — Per-grammar rule catalogs

### A.1 JSON (6 rules, grammar/json/json.bbnf)

| rule | shape | justification |
|---|---|---|
| null | Scalar/Kw | `"null" -> 0u8` |
| bool | Kw | 2-branch literal Alt |
| number | Number | `/-?(0\|[1-9]\d*)…/ -> f64` |
| comma | Scalar | `"," ?w` |
| colon | Scalar | `":" ?w` |
| string | String | QuotedString regex |
| pair | Object-body | `string, colon >> value` |
| array | Array | `"[" >> (value<<comma?)*?w << "]"` |
| object | Object | `"{" >> (pair<<comma?)*?w << "}"` |
| value | Wrap/Un dispatcher | 6-way disjoint-FIRST Alt |

### A.2 CSS L4 (15 files, ~165 rules)

**tokens.bbnf**: ident (HRegex), selectorIdent (HRegex), string (Str).

**value-unit.bbnf**: number (Num), integer (HRegex); absoluteLengthUnit/viewportLengthUnit/containerLengthUnit/fontLengthUnit/angleUnit/timeUnit/frequencyUnit/resolutionUnit/flexUnit/percentageUnit (all Kw); relativeLengthUnit/lengthUnit (Wrap); length/angle/time/frequency/resolution/flex/percentage (all Flat); unitless (Wrap → number); valueUnit (Un).

**color.bbnf**: digit/comma/whitespace/div (Scalar); sep/alphaSep (Wrap/Kw); colorPercentage (Wrap); colorValue (Wrap); namedColor (Kw, 148 branches); hex (HRegex); colorType/colorSpace/mixSpace/hueMethodKeyword (Kw); hueMethod (Flat); colorFunction (Arg); colorFn (Arg); colorMix (Arg); color (Wrap/Recursive).

**selectors.bbnf**: hash (HRegex); nsPrefix/wqName (Flat); typeSelector (Wrap); classSelector/idSelector (Flat); attrName (Un); attrMatcher (Kw); attrModifier (HRegex); attrSelector (Flat/Arg); anPlusB (Kw + HRegex fallback); isPseudo/wherePseudo/notPseudo/hasPseudo/langPseudo/dirPseudo (Arg); nthFunctionName (Kw); nthPseudo (Flat); dirKeyword (Kw); simplePseudoClass/simplePseudoElement (Flat); partPseudo/slottedPseudo/highlightPseudo (Arg); pseudoClass/pseudoElement/colonSelector (Un); compoundSelector (Un); combinator (Kw + HRegex); complexSelector (Flat); relativeSelector (Flat); relativeSelectorList/selectorList (Flat sep-list).

**properties.bbnf**: ident/propertyName (HRegex); importantSuffix (Flat); dimension (Wrap); cssString (Str); dashIdent (HRegex); value (Un); colorProps/sizeProps/spacingProps/fontProps/bgProps/transformProps/transitionProps/listTableProps (Kw); colorDecl/sizeDecl/spacingDecl/fontDecl/bgDecl/transformDecl/transitionDecl/listTableDecl/displayDecl/positionDecl/overflowDecl/visibilityDecl/flexDirDecl/flexWrapDecl/alignDecl/flexNumDecl/fontSizeDecl/fontWeightDecl/lineHeightDecl/borderWidthDecl/borderStyleDecl/borderRadiusDecl/opacityDecl/textAlignDecl/boxSizingDecl/cursorDecl (all Flat); customPropertyDecl (**walker**); genericDecl (**walker** or Flat-with-regex-body); declaration (Un).

**values.bbnf**: dashed (HRegex); dimension (Wrap); mathOperator (Kw); mathValue (Un); mathProduct/mathExpr (Pr); calcFunction/minFunction/maxFunction/clampFunction (Arg); varFallback (Flat); varFunction/envFunction (Arg); urlFunction (Arg w/ span); globalKeyword (Kw); value (Un).

**func-body.bbnf**: string (Str); funcBody (**walker**); mathFunctionName (Kw); mathProductOp/mathSumOp (Kw); mathValue (Un); mathProduct/mathExpr (Pr); calcFunction/minFunction/maxFunction/clampFunction/varFunction/envFunction/urlFunction/genericFunction (Arg); dashed (HRegex); varFallback (Wrap).

**gradients.bbnf**: gradientKind (Kw); colorStopPosition/colorStop (Flat); colorStopList (Flat sep-list); linearSide/radialShape/radialExtent (Kw); linearDirection/radialSize/radialPosition/radialConfig/conicConfig (Flat); linearGradient/repeatingLinearGradient/radialGradient/repeatingRadialGradient/conicGradient/repeatingConicGradient (Arg); gradient (Un).

**transforms.bbnf**: transformName (Kw); transformValue (Wrap); transformArgs (Flat sep-list); translate/translateX/translateY/translate3d/rotate/rotateX/rotateY/rotateZ/rotate3d/scale/scaleX/scaleY/scale3d/skew/skewX/skewY/matrix/matrix3d/perspectiveFn (Arg); transformFunction (Un).

**filters.bbnf**: filterName (Kw); blur/brightness/contrast/dropShadow/grayscale/hueRotate/invert/filterOpacity/saturate/sepia (Arg); filterFunction (Un).

**easing.bbnf**: cubicBezier (Arg); stepsKeyword (Kw); steps (Arg); linearEasing (Arg); easingKeyword (Kw); easingFunction (Un).

**media.bbnf**: mediaType/mediaQualifier (Kw); mediaFeatureName (HRegex); mediaFeatureValue (HRegex); mediaFeature (Arg); mediaNot/mediaAnd/mediaOr (Flat recursive); mediaInParens (Wrap recursive); mediaCondition (Wrap recursive); mediaQuery/mediaQueryList (Flat).

**stylesheet.bbnf**: blockContent (Un); ruleBlock (Object); qualifiedRule (Flat); mediaRule (Flat); keyframeStop (Kw); keyframeSel (Flat); keyframeBlock (Flat); keyframesRule (Flat); atRuleBody (Wrap); genericAtRule (Flat); atRule (Wrap/Un); ruleItem (Wrap); ruleList (Flat); stylesheet (Flat).

**keywords.bbnf**: 14 Kw tables (displayKeyword, positionKeyword, overflowKeyword, visibilityKeyword, flexDirKeyword, flexWrapKeyword, alignKeyword, borderStyleKeyword, borderWidthKeyword, fontWeightKeyword, textAlignKeyword, boxSizingKeyword, cursorKeyword, globalKeyword).

**keyframes.bbnf**: mirror of stylesheet's keyframe rules (redundant/legacy; not counted separately).

### A.3 Google Sheets (32 rules, grammar/google-sheets/google-sheets.bbnf)

| rule | shape |
|---|---|
| number | Num |
| string | Str |
| boolean | Kw |
| error_literal | Kw |
| sheet_prefix | HRegex |
| cell_ref | HRegex |
| range_end | Wrap |
| cell | Flat |
| range_ref | Flat |
| cell_or_range | Wrap |
| identifier | HRegex |
| compare_op | Kw |
| comparison_expr | Pr |
| concat_expr | Pr |
| add_op | Kw |
| add_expr | Pr |
| mul_op | Kw |
| mul_expr | Pr |
| exp_expr | Pr |
| unary_prefix | Kw |
| unary_expr | Pr (unary head) |
| postfix_expr | Pr (postfix) |
| primary | Un |
| paren_expr | Wrap/Flat |
| func_open | Flat |
| arg | Wrap |
| comma | Scalar |
| func_args | Flat sep-list |
| func_call | Arg |
| let_binding | Flat |
| let_args | Flat sep-list |
| let_call | Arg |
| lambda_params | Flat sep-list |
| lambda_call | Arg |
| array_row | Flat sep-list |
| array_rows | Flat sep-list |
| array_literal | Array |
| expression | Wrap |
| formula | Flat |

### A.4 BBNF (bbnf.bbnf + expressions.bbnf + types.bbnf, 38 rules)

**bbnf.bbnf**: identifier (HRegex); literal (Flat); regex (Flat); big_comment (Flat); comment (Flat); lhs (Wrap); call_arg (Flat); term (Un); modifier (Kw); factor (Flat); mapped_factor (Flat); binary_operators (Kw); binary_factor (Flat); concatenation (Pr-deg); alternation (Pr-deg); closure (Flat); rhs (Wrap); rule (Flat); import_path (Flat); import_items (Flat); import_directive (Flat); recover_directive (Flat); pretty_hint (Flat); pretty_directive (Flat); ws_directive (Flat); token_directive (Flat); debug_directive (Flat); host_directive (Flat); directive (Kw 7-way PHF); grammar_item (Un); grammar (Flat-Repeat).

**expressions.bbnf**: int_lit/float_lit (HRegex); bool_lit (Kw); string_lit (Str); value_ident (HRegex); value_path (Flat); value_input (Flat); value_fn_call (Arg); value_atom (Un); mul_op/add_op/cmp_op (Kw); value_unary/value_mul/value_add/value_cmp/value_and/value_or (Pr); value_closure (Flat); value_expr (Wrap); type_annotation (Flat).

**types.bbnf**: type_name (Kw 11-branch + regex fallback).

### A.5 EBNF, CSV, Math, misc

**EBNF** (14 rules): classical Kw + HRegex + Flat + Pr; no new shapes needed.

**CSV** (6 rules, misc/csv.bbnf): DQUOTE (Scalar); escaped (Flat); textdata (HRegex); field (Wrap); record (Flat sep-list); csv (Flat sep-list). All standard.

**Math** (4 rules, misc/math.bbnf): expr/term = Pr 2-rung; factor (Wrap); number (Num); plus 6-level `p`..`pppppp` alias chain (all Wrap).
