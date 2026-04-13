# Scanner + Regex Generalization Audit (Tranche AR)

This audit catalogs every scanner / regex-pattern detection site across
the two concurrent regex tiers — the `bbnf-regex` library
(`parse-that/rust/regex/`) and the `bbnf-lang` IR pattern sector
(`crates/ir/src/passes/recognizers/`, `crates/core/src/backend/kernels/`,
`crates/core/src/generate/regex/`) — and classifies each site by its
proper home: structural / domain-agnostic vs grammar-pattern-specific.
The mapping informs the AR refactor plan that follows.

The substrate is split:

- **Tier 1 — `bbnf-regex`**: HIR, classifier, `RegexInfo`, e-graph
  rules, byte/charset substrate. No grammar IR knowledge.
- **Tier 2 — Runtime scanners**: `parse-that/parsers/scan/`. Pure
  byte-level scanners with `IdentConfig` / `QuotedStringConfig` /
  `BalancedScanConfig` / `NumberConfig` knobs. Configurable, no
  language IR.
- **Tier 3 — `bbnf-lang` kernel emitters**: `backend/kernels/`. Each
  module is a thin `quote!`-wrapper that splices a tier-2 call into
  emitted Rust.
- **Tier 4 — `bbnf-lang` recognizers**: `passes/recognizers/`.
  Per-node IR shape mining → `Recognizer` records / sidecar configs.
- **Tier 5 — `bbnf-lang` codegen regex**: `generate/regex/emit/`.
  Tiered fallback (fast-path / HIR / DFA) for arbitrary regex
  patterns; routes through tier-3 kernels when classification matches.

## Tier 1: bbnf-regex (parse-that/rust/regex)

### `classify/mod.rs` — `RegexClass` taxonomy

`RegexClass` (lines 51–118) is the structural classification enum. Every
variant is **truly structural** — defined by HIR shape, not by a named
grammar dialect. Variant inventory:

| Variant | Status | Reasoning |
|---|---|---|
| `Numeric { allows_sign, allows_fraction, allows_exponent, reject_leading_zero, allow_leading_dot }` | structural | flags collapse JSON/CSS/generic dialects into one parameterized variant — exactly the AQ.3 deoverfit goal |
| `QuotedString { quote_char, allows_escapes, allows_u_escapes }` | structural | `quote_char + escape vocabulary` cleanly distinguishes JSON `"..."` (with `\uXXXX`) from CSS `'...'` |
| `HexDigits` | structural | nullary; the `[0-9a-fA-F]+` shape carries no extra dimension |
| `Identifier { allows_leading_dash, allows_double_dash_prefix }` | structural | the two flags fold the CSS dialects (`-foo` vendor / `--foo` custom-property) into one variant |
| `WhitespaceWithBlockComment` | structural | nullary; the `(?s)(?:\s|/\*.*?\*/)*` shape is fully specified by the alternation pair |
| `CharClassQuantified(ClassRangeInfo)` | structural | introduced Tranche V; carries the same `chars/negated/min/max` data as `RegexInfo.quantified_class` |
| `PrefixThenClass { prefix, tail }` | structural | introduced Tranche V; closes the `--[\w-]+` / `@[a-z][\w-]*` / `#[a-f0-9]+` coverage gap |
| `AccelDriven(u8)` | structural | gives `RegexInfo.accel_candidate` a first-class taxonomy home |
| `Unknown` | structural | universal fallback |

`canonical_pattern()` (lines 129–148) returns canonical pattern strings
for the dialects that have them; this is **structural** but the
hard-coded match arms only cover the Identifier / QuotedString families.
Acceptable since the variants without canonical strings depend on
caller-supplied data.

### `classify/structural.rs` — HIR shape walkers

Pure HIR analyzers (`try_classify_numeric`, `try_classify_quoted_string`,
`try_classify_hex`, `try_classify_identifier`,
`try_classify_charclass_quantified`, `try_classify_prefix_then_class`).
Every helper operates on `Hir::Class { ranges, negated }` /
`Hir::Concat([..])` / `Hir::Alternation(..)` shape — **zero** grammar
heuristics. The helpers correctly handle HIR-parser quirks (single-byte
vs fused literal nodes, leading `Empty` flag markers,
unwrap_group/unwrap_repetition normalization).

**Verdict: 100% structural. Belongs in bbnf-regex.**

### `info/mod.rs` — `RegexInfo` + `EngineSet`

`RegexInfo` (lines 137–188) carries the full structural fact bundle:
classification + literal_prefix/suffix + negated_class +
quantified_class + first_chars + nullable + must_consume +
one_pass_eligible + match-width + dfa_size + accel_candidate +
hir_walkable + feasible_engines. Every field is **structural**.

`EngineSet` (lines 60–105) is a hand-rolled bitset newtype over the 8
engine bits (Memchr1/2/3, NibbleLut, OnePass, SmallDfa, Dfa,
FamilyHelper). The `FamilyHelper` bit is the **single piece of
cross-tier coupling**: it's set whenever the classifier landed on a
named family that has a kernel module in
`crates/core/src/backend/kernels/`. The bit is morally structural (it
mirrors the `matches!(... Numeric | QuotedString | ...)` arm in
`derive_feasible_engines` lines 393–408), but its semantics are
"bbnf-lang has a kernel for this", which is **bbnf-lang-side
information**. The current arrangement puts the truth in the wrong
crate; if a downstream consumer adds a new family kernel, this list
silently drifts.

**Verdict: structural except for the `FamilyHelper` bit, which is the
one piece of bbnf-lang policy leaking into bbnf-regex.**

`derive_feasible_engines` (lines 342–410) is otherwise pure structural
derivation from existing fields.

### `info/{classify,dfa_size,literal_prefix,one_pass,width}.rs`

- `classify.rs` — `detect_accel_candidate` (literal prefix/suffix-driven
  memchr byte) + `detect_negated_class` + `detect_quantified_class` +
  `detect_anchored`. All structural.
- `dfa_size.rs` — NFA state estimator `estimate_dfa_size`. Structural.
- `literal_prefix.rs` — `extract_literal_prefix` / `extract_literal_suffix`. Structural.
- `one_pass.rs` — `is_hir_walkable` + `check_one_pass_eligible` (pairwise
  FIRST disjointness). Structural.
- `width.rs` — `is_nullable`, `compute_match_width`, `count_hir_nodes`.
  Structural.

**All five files: pure HIR shape analysis. Belong in bbnf-regex.**

### `egraph/`

`mod.rs` — `HirEGraph` substrate, `build_hir_egraph`,
`saturate_hir_egraph`, `extract_canonical`, `simplify_hir`,
`simplify_hir_cached`, `needs_saturation` fast-path predicate. All
structural; isomorphic to the grammar-tier substrate in `bbnf-ir`.

`rules/{flatten,redundant,repetition,superset,union}.rs` — the five
retained HIR rewrite rules. Each delegates to a pure helper in
`crate::algebra::*` for the actual algebraic check (superset, union,
absorption). All structural.

**Verdict: pure equality-saturation substrate. Belongs in bbnf-regex.**

### `algebra/{mod,superset,union}.rs`

`extract_char_class_bytes`, `byteset_to_pattern`, `pattern_is_superset`,
`try_union_patterns` plus the per-byteset `is_superset` / `try_union`
helpers. All operate on `ByteSet` and pattern strings — **purely
structural regex algebra** consumed by both tiers' e-graph rules.

### `sets/`, `automata/`, `hir/`, `unicode.rs`, `utf8.rs`, `first.rs`

Pure substrate: `ByteSet`, `CharSet128`, `equiv` partitioning, NFA/DFA
compilation, accel detection, HIR types + parser, Unicode tables, UTF-8
codepoint expansion, `regex_first_chars`. All structural and properly
located.

## Tier 2: Runtime scanners (parse-that/parsers/scan/)

All tier-2 scanners are configurable via dedicated config structs,
allowing every dialect to be expressed as a `Config` rather than a
hard-coded helper. Inventory:

### `number.rs` + `number_f64.rs`

`scan_number_mantissa` is the canonical core (8-digit SWAR chunking +
SIMD digit validation + Eisel-Lemire-ready accumulation). Driven by
`NumberConfig { allow_plus_sign, allow_leading_dot,
reject_leading_zero }` with `GENERIC_NUMBER_CONFIG` (CSS-style) and
`STRICT_NUMBER_CONFIG` (RFC 8259 / JSON) presets. The `*_span` /
`*_fused` / `*_f64` wrappers are thin façades.

**Pure runtime: zero grammar-specific logic.** The 8-digit SWAR
mantissa fold, SWAR digit validation, and Eisel-Lemire f64 conversion
are all generic numeric algorithms.

### `ident.rs`

`scan_ident(state, &IdentConfig)` driven by `IdentConfig {
allow_leading_dash, allow_double_dash_prefix }`. `DEFAULT_IDENT_CONFIG`
= `[a-zA-Z_][\w-]*`; `CSS_IDENT_CONFIG` = `-?[a-zA-Z_][\w-]*` plus
`--[\w-]+`. The two flags exhaustively parameterize the
`RegexClass::Identifier` dialect space.

**Pure runtime: zero grammar logic.** The CSS and DEFAULT presets are
specifications, not heuristics.

### `ws_comment.rs`

`scan_ws_block_comments(state) -> Span` and `scan_block_comment(state)`.
The whitespace LUT (`WS_LUT`) covers the canonical ASCII whitespace
set `{' ', \t, \n, \f, \r}`. Block-comment recognition is hard-coded
to `/* ... */`.

**Slightly CSS-flavored**: the `/* ... */` block-comment delimiters
are CSS / C-family. There is currently no `WsCommentConfig` exposing
the delimiter pair. That said, **every actual consumer uses C-style
block comments** (CSS, JSON-with-comments, BBNF, JS, Rust, etc.), and
no production grammar requests an alternative pair. The arrangement is
a "structural with rare exception" — if a `#...\n`-comment grammar
appears (Python, Bash, BBNF prelude), the scanner needs splitting into
"line comment" + "block comment" config.

**Verdict: pure runtime, but should be promoted to a
`WsCommentConfig { ws_lut, line_comment, block_comment_open, block_comment_close }`
in the AR refactor for full generality.**

### `balanced.rs`

`scan_balanced(bytes, &BalancedScanConfig)` with
`BalancedScanConfig { open, close, quotes, escape, terminators }`. SIMD
nibble-LUT-driven structural-byte classification (≤ 8 unique bytes
per call). Used by AR's planned `BalancedScan` kernel emitters.

**Pure runtime, fully general.** The config covers every dialect
(JSON `[…]/{…}`, CSS `(…)`, Lisp `(…)`, etc.).

### `quoted.rs` + `quoted_simd.rs`

`scan_string_quoted` (generic, accepts `"` or `'`, no escape
validation) and `scan_quoted_string_strict` (RFC 8259, validates `\u`
+ surrogates). The strict scanner uses
`STRICT_QUOTED_STRING_CONFIG = QuotedStringConfig { quote_char: b'"',
allows_escapes: true, allows_u_escapes: true }`. SIMD escape-parity
inner loop in `quoted_simd::scan_quoted_string_simd` (16-byte
carry-based parity).

**Pure runtime.** The two configs (`STRICT_QUOTED_STRING_CONFIG`,
`GENERIC_QUOTED_STRING_CONFIG`) are exhaustive for the
`RegexClass::QuotedString` dimensions.

### `digits.rs`

`scan_digits_mut`, `scan_digits_star_mut`, `scan_alnum_mut`,
`scan_hex_mut`. Three byte-class LUTs in `.rodata` (`DIGIT_LUT`,
`ALNUM_LUT`, `HEX_LUT`). Tight LUT-driven loops.

**Pure runtime**, but the `DIGIT/ALNUM/HEX` choice is **a fixed enum
of three shapes** rather than a generic `scan_class_lut(state,
&LUT)`. This is over-specialized — see Refactor #2.

## Tier 3: bbnf-lang kernels (crates/core/src/backend/kernels/)

Each kernel is a thin `quote!`-wrapper that selects + splices a tier-2
scanner. Inventory and call shape:

| Kernel module | Exports | Targets `parse_that::*` | Status |
|---|---|---|---|
| `quoted_string.rs` | `emit_call`, `emit_call_strict` | `scan_string_quoted`, `scan_quoted_string_strict` | thin wrapper, structural |
| `number.rs` | `emit_call_span`, `emit_call_fused` | `scan_number_strict_span`, `scan_number_strict_fused` | thin wrapper; misses non-strict variant for CSS |
| `identifier.rs` | `emit_call`, `emit_call_css` | `scan_ident(state, &DEFAULT_IDENT_CONFIG)`, `scan_ident(state, &CSS_IDENT_CONFIG)` | thin wrapper, structural |
| `comment_ws.rs` | `emit_call` | `scan_ws_block_comments` | thin wrapper |
| `charclass.rs` | `emit_call_opt`, `emit_stmt_opt`, `charset_from_class_body`, `charset_from_shorthand`, `charset_from_byte_ranges` | `scan_digits_mut`, `scan_digits_star_mut`, `scan_alnum_mut`, `scan_hex_mut` | hand-rolled trio dispatch (DIGITS/ALNUM/HEX) |
| `prefix_class.rs` | `emit_call_opt` | `scan_alnum_mut` / `scan_digits_mut` / `scan_hex_mut` (tail dispatch) | hand-rolled trio dispatch |
| `charset_shapes.rs` | `DIGITS`, `ALNUM`, `HEX`, `matches_set` | (none) | shared classifier consumed by `charclass` + `prefix_class` |
| `punct_ws_region.rs` | `emit_call(puncts, ws_pattern)` | inline byte loops | grammar-derived `puncts` + `ws_pattern` |
| `balanced_wrap.rs` | `emit_call(open, close, pivot, ...)` | `find_first_of_3` | wraps a delim-scan body, structural |

Observations:

1. **`charclass` + `prefix_class` are over-specialized to three
   shapes** (`DIGITS`, `ALNUM`, `HEX`) via the shared
   `charset_shapes::matches_set` classifier. Any other shape — `\s+`
   for instance — falls to the generalized inline emitter. The
   overspecialization is justified by the cargo-expand audit (86
   duplications in CSS L4) but the *kernel ↔ helper* split is too
   tight: there's no way to add `WHITESPACE_LUT` without touching
   four places. See Refactor #2.

2. **`punct_ws_region` is grammar-pattern-specific, NOT structural.**
   It detects single-byte punctuation that the grammar's own
   `AltDispatch` table treats as a branch key. The byte set is
   derived per-grammar by walking the dispatch tables — proper
   AQ.4 deoverfit. Belongs in bbnf-lang IR. ✓

3. **`balanced_wrap` is grammar-shape-specific** (`Wrap(open,
   Repeat(Alt), close)`), but the kernel body itself is structural —
   it's a `find_first_of_3`-driven balanced-byte loop with
   per-grammar block/pivot calls spliced in. Right home; right shape.
   ✓

4. **`number.rs` exposes only the strict (JSON-style) scanner**;
   CSS-style `scan_number_span` / `scan_number_f64` are reachable
   only via the FnDescriptor::NumberConvert path
   (`generate/regex/emit_regex` ad-hoc routing). The kernel module
   should expose `emit_call_generic` for symmetry. See Refactor #4.

5. **`comment_ws.rs` is 13 lines** for a single quote!-wrapper. The
   value is *zero* — the AR plan should fold it into a generic
   `kernels::scan_call(scanner_name)` helper.

6. **All kernel modules are pure emitters** — they read no IR, take
   pre-computed inputs (CharSet128, byte slices, configs from sidecars,
   per-grammar TokenStreams). The AF.1 invariant ("kernels never
   reach into &GrammarIR") is preserved. ✓

## Tier 4: bbnf-lang recognizers (crates/ir/src/passes/recognizers/)

Single-walk Z.0 + AF.1 unified miner orchestrator that walks every
rule body once via `RecognizerMiner` trait. Ten miners run in load-
bearing order. Inventory:

| Miner | Detects | Emits | Status |
|---|---|---|---|
| `ContextFactsMiner` | discrimination strength on Alt; scan safety on Wrap; in-token-dispatch flag | `outputs.context_facts: ContextFactsMap` | structural (IR shape) |
| `QuotedStringMiner` | `IrNode::Regex(sid)` where `RegexClass::QuotedString { .. }` | `Recognizer { shape: Regex { sid } }` | thin classifier passthrough |
| `BalancedWrapMiner` | `unwrap_wrap(node)` with single-byte open/close literals | `Recognizer { shape: DelimiterBalanced { open, close, inner } }` | structural (IR shape) |
| `CommentWsMiner` | `IrNode::Regex(sid)` where `RegexClass::WhitespaceWithBlockComment` | `Recognizer { shape: Regex { sid } }` | thin classifier passthrough |
| `IdentifierMiner` | `IrNode::Regex(sid)` where `RegexClass::Identifier`, `PrefixThenClass`, `CharClassQuantified`, `HexDigits` | `Recognizer { shape: Regex { sid } }` | thin classifier passthrough |
| `SeparatorListMiner` | `IrNode::Skip(element, opt_sep)` with single-byte sep | `Recognizer { shape: SeparatorList { .. } }` | structural (IR shape) |
| `TokenLedBranchesMiner` | `IrNode::Alt` with `discrimination = Strong` (read from context_facts in same walk) | `Recognizer { shape: TokenLedBranches { .. } }` | structural (IR shape) |
| `PunctWsRegionMiner` | `OptionalWhitespace(Literal(p))` etc. where `p` ∈ grammar's dispatch byte vocabulary | `Recognizer { shape: PunctWsRegion { puncts } }` | grammar-shape (correct, AQ.4 deoverfit) |
| `DelimScanMiner` | `Wrap(open, Repeat(Alt(..)), close)` with pivot byte detection | `outputs.delim_scan_configs: HashMap<NodeId, DelimScanConfig>` | grammar-shape (correct) |
| `KeyDispatchMiner` | `IrNode::Alt` with literal-led keys + common separator + regex fallback | `outputs.key_dispatch_configs: HashMap<NodeId, KeyDispatchMatch>` | grammar-shape (correct) |

Observations:

1. **Four miners (`QuotedString`, `Identifier`, `CommentWs`, plus the
   `Numeric` family which has no dedicated miner) are pure passthroughs**
   to `RegexClass::*` matching. They produce identical
   `Recognizer { shape: Regex { sid }, signature: <hash> }` records.
   This is **30+ lines per miner** for what should be one
   `RegexClassMiner { class_filter: Fn(&RegexClass) -> bool }`
   passthrough. See Refactor #1.

2. **`Numeric` has NO miner.** The `Numeric` regex class is consumed
   only by `try_specialize_map_fn` in `lower/expression.rs` (FnDescriptor
   specialization) and by `scanner_plan_for` (kernel routing). There's
   no `NumberMiner` producing `Recognizer { shape: Regex { sid } }`
   for it — meaning the recognizer / kernel-routing path is asymmetric
   between the four numeric / string / identifier / hex families.

3. **All grammar-shape miners (`Balanced*`, `Separator*`, `TokenLed*`,
   `PunctWs*`, `DelimScan*`, `KeyDispatch*`) are correctly placed** —
   they detect IR-tree shapes that have no meaning at the bbnf-regex
   tier.

4. **`signature.rs::compute_shape_hash`** computes a stable 64-bit
   hash over byte/class/shape data only. The hash collapses two
   distinct `StringId`s for the same canonical pattern. The
   discriminant-only `hash_regex_class_into` (lines 76–86) is correct
   for the parameterized variants — two `Numeric` classes with
   different flags will hash equal. This is fine because the
   `signature` is consumed only for kernel-helper dedup (which IS
   discriminant-keyed), but it's worth noting that **the signature
   hash is structurally lossier than the classification it derives
   from**. Future tranches that depend on per-flag dedup will need
   to extend this.

## Tier 5: bbnf-lang codegen (crates/core/src/generate/regex/)

Tiered fallback emission:

```
emit_regex(pattern, opts)
  └─ Tier 1: emit_regex_fast_path
      ├─ scanner_plan::plan_regex_scanner — kernel routing via RegexClass
      ├─ ad-hoc ",|\s+" comma-or-ws (lines 117–144 of emit/mod.rs)
      ├─ try_emit_simd_positive_class (\d+, \s+, [a-z]+ → memchr/nibble-LUT)
      ├─ generalized::emit_generalized_regex_direct (single ranges, sets, prefix+class, ws-padded literal)
      └─ negated_class detection → simd::emit_negated_scan_*
  └─ Tier 2: hir::try_emit_regex_inline
      └─ recursive HIR walker; at Repetition(Class(_)), routes through
         kernels::charclass::emit_stmt_opt
  └─ Tier 3: dfa::try_emit_dfa_inline (Tier A decision tree / Tier B
             transition table by state count)
  └─ Tier 4: emit_regex_unsupported (compile_error!)
```

### `emit/mod.rs` — entry point

`emit_regex_fast_path` (lines 111–225) consults `scanner_plan` first,
then ad-hoc patterns. **Two structurally-suspicious branches**:

1. **Lines 117–144: hard-coded `",|\s+"` and `r"\s+|,"` patterns.** These
   are CSS-specific (comma-or-whitespace separator). The classify path
   doesn't recognize them because they're an `Alternation` that doesn't
   fit any current `RegexClass` variant. See Refactor #5.

2. **`try_emit_simd_positive_class` + `parse_positive_class_ranges`
   (lines 327–461)**: re-parses regex strings to detect `[a-z]+`,
   `\s+`, etc. **This duplicates** what
   `RegexClass::CharClassQuantified` already extracts via the HIR — the
   re-parse should be replaced by a `RegexInfo`-keyed lookup.

### `emit/scanner_plan.rs`

`plan_regex_scanner(pattern, opts)` (lines 105–182) is the **canonical
RegexClass → kernel router**. Uses `opts.classify_regex(pattern)` (which
hits the `ir.regex_info` cache when available). Routes:

- `QuotedString { allows_u_escapes: true }` → `kernels::quoted_string::emit_call_strict`
- `Numeric { reject_leading_zero: true }` → `kernels::number::emit_call_fused/span`
- `WhitespaceWithBlockComment` → `kernels::comment_ws::emit_call`
- `Identifier { .. }` → `kernels::identifier::emit_call` (NB: hard-coded to default config, not CSS)
- `QuotedString { .. }` (non-JSON) → `kernels::quoted_string::emit_call`
- `CharClassQuantified(ClassRangeInfo)` → `kernels::charclass::emit_call_opt`
- `PrefixThenClass { prefix, tail }` → `kernels::prefix_class::emit_call_opt`
- `Numeric { allows_sign: false, .. }` → `kernels::number::emit_call_span(false)` (CSS)

This is the **single canonical routing site**. ✓ But the routing is
**incomplete**:

- `Identifier { allows_leading_dash, allows_double_dash_prefix }` is
  collapsed to `kernels::identifier::emit_call` (default config) — the
  `emit_call_css` variant is **never selected by scanner_plan**. CSS
  identifier emission goes through a different code path
  (`backend/rust/emitter/leaves.rs`?). See Refactor #4.
- `RegexClass::AccelDriven(_)` falls through to `None`.

### `emit/generalized/{mod,class_segments}.rs`

`emit_generalized_regex_direct` re-parses regex pattern strings to
detect `[a-z]+`, `[abc]`, `\s+`, `--[\w-]+`. **All these shapes are
already classified by the bbnf-regex HIR**, so the re-parsing is
**redundant work**. The right way: consume `RegexInfo.classification`
and dispatch on `CharClassQuantified` / `PrefixThenClass`. See
Refactor #5.

`class_segments::char_class_to_predicate` (lines 198–251) is a
**third** independent regex-parser-style routine that converts a
char-class body string to a Rust byte predicate. Duplicates what
`patterns/shorthand.rs::detect_from_ranges` does for the canonical
shorthand classes plus what
`charclass.rs::charset_from_class_body` does for parsing.

### `emit/hir/{mod,leaf,alternation,repetition}.rs`

The HIR walker that emits inline byte ops. Properly delegates to
`kernels::charclass` for `Hir::Class` repetitions and uses the
`bbnf-regex` HIR types throughout. **Structurally clean**, but the
walker has its own `is_nullable` / `is_broad_byte_class` /
`contains_lazy_quantifier` helpers that **duplicate**
`bbnf_regex::is_nullable` / etc. — there's no reason these should be
re-implemented in bbnf-lang.

### `emit/dfa/{mod,helpers,table}.rs`

DFA-tier emission. Uses `bbnf_regex::Dfa::compile` directly. Pure
codegen; no duplicated regex-parser logic.

### `emit/simd.rs`

SIMD memchr / nibble-LUT emission helpers. Pure codegen; takes byte
slices and emits TokenStream. **Structural; correctly placed.**

### `emit/negated_class.rs`

`is_negated_char_class_regex(pattern)` re-parses pattern strings to
detect `[^XYZ]+/*`. **Yet another regex-parser-lite** — duplicates
what `RegexInfo.negated_class` already extracts.

`try_strip_ws_padded_literal(pattern)` re-parses for `\s*LIT\s*`. Same
problem.

### `patterns/{char_class,shorthand}.rs`

`char_class.rs::CharClassAnalysis` — analyzes positive/negative char
classes for memchr / nibble-LUT / scalar strategy selection. Pure
analysis given byte ranges. **Structural and properly located** (it
operates on byte ranges, not patterns).

`shorthand.rs::detect_from_ranges` / `detect_from_bytes` /
`emit_predicate` — detects `\d`, `\w`, `\s`, `[a-zA-Z]`,
`[a-zA-Z0-9]`, `[0-9a-fA-F]` from `ByteRange` slices. **Structural**,
but it overlaps with `kernels::charset_shapes::DIGITS/ALNUM/HEX` and
with `RegexClass::HexDigits` — three modules know about the same
canonical shapes. See Refactor #2.

## Misplaced code (cross-tier)

Concrete cross-tier mismatches identified by the audit:

### M1. `EngineSet::FAMILY_HELPER` bit lives in bbnf-regex but is determined by bbnf-lang kernel coverage

`bbnf_regex::info::derive_feasible_engines` (info/mod.rs:393–408) sets
`FAMILY_HELPER` whenever the `RegexClass` matches a hand-listed set of
variants that have kernel modules in `crates/core/src/backend/kernels/`.
The hand-list is in **bbnf-regex source**, but the kernel coverage is
**bbnf-lang policy**. Adding a new family kernel requires updating both.

**Fix**: lift the predicate into a `KernelCoverage` trait or a static
`EngineSet` mask the bbnf-lang side passes in via `EmitOpts`.

### M2. Three independent regex-pattern-parser-lites in bbnf-lang codegen

- `emit/negated_class.rs::is_negated_char_class_regex`
- `emit/generalized/mod.rs::is_single_char_range_regex`,
  `is_small_char_set_regex`, `is_char_range_plus_regex`,
  `is_char_range_star_regex`, `parse_positive_class_ranges`
- `emit/generalized/class_segments.rs::parse_class_segments`,
  `char_class_to_predicate`, `unescape_regex_prefix`
- `emit/mod.rs::parse_positive_class_ranges`
- `kernels/charclass.rs::charset_from_class_body`,
  `charset_from_shorthand`

That's **five** separate implementations of "regex string → some byte
representation" inside bbnf-lang, each with subtly different escape
handling. The bbnf-regex HIR parser already does this work — every
caller should consume `RegexInfo.classification` /
`RegexInfo.quantified_class` / `RegexInfo.negated_class` instead of
re-parsing strings.

### M3. `FnDescriptor` specialization re-classifies in lower/expression.rs

`lower/expression.rs::try_specialize_map_fn` (lines 1318–1365) calls
`classify_regex(pattern)` directly — **without** going through the
`ir.regex_info` cache (because at lowering time `ir.regex_info` doesn't
exist yet). The classifier therefore runs twice per pattern: once at
lower-time (cache miss) and once at `compute_regex_info` time (cache
populate). This is a sequencing bug in disguise. The lowering pass
should defer FnDescriptor specialization until after
`compute_regex_info`, or share a memoization arena.

### M4. `kernels/charset_shapes.rs::DIGITS/ALNUM/HEX` is a third copy of the canonical-shape data

The same canonical byte sets are also encoded in:
- `bbnf-regex::classify::structural::is_hex_class`,
  `is_letter_class`, `is_word_class`, etc. (HIR-shape predicates)
- `generate/regex/patterns/shorthand.rs::detect_from_ranges` (range
  predicates)

Three independent encodings of the same `[0-9]` / `[a-zA-Z0-9]` /
`[0-9a-fA-F]` byte sets.

### M5. `scan_ws_block_comments` hardcodes `/* ... */` delimiters

`parse_that/parsers/scan/ws_comment.rs` is otherwise structural but
the block-comment delimiter pair is hard-coded. The matching
`RegexClass::WhitespaceWithBlockComment` variant is also nullary
(carries no delimiter info). For non-C-family grammars, both sides
need a `WsCommentConfig { line_comment, block_comment_open,
block_comment_close }`.

### M6. `emit/mod.rs::emit_regex_fast_path` ad-hoc `",|\s+"` branch

A 28-line CSS-specific match arm in the generic regex emitter. The
right home: a `RegexClass::Separator { byte, with_whitespace }`
variant in bbnf-regex, plus a `kernels::separator::emit_call`.

### M7. `RecognizerShape::Regex { sid }` recognizers are a 4× duplicated thin pattern

`QuotedStringMiner`, `IdentifierMiner`, `CommentWsMiner` (and the
absent `NumberMiner`) all emit the same record shape with the same
boilerplate. The miner trait substrate should host a single
`RegexClassMiner { name, accept: Fn(&RegexClass) -> bool }` driver.

## Proposed refactors for tranche AR

| # | Change | From | To | Rationale | Deletes |
|---|---|---|---|---|---|
| AR.1 | Collapse the four `RegexClass`-passthrough miners (`QuotedStringMiner`, `IdentifierMiner`, `CommentWsMiner`, plus the missing `NumericMiner`) into one parameterized `RegexClassMiner` driven by a class-acceptance predicate | `passes/recognizers/{quoted_string,identifier,comment_ws}.rs` (190 LOC across 3 files) | `passes/recognizers/regex_class.rs` (~80 LOC) | M7 | 110 LOC of structurally-identical boilerplate |
| AR.2 | Generalize the `kernels::charclass` → `parse_that::scan_*_mut` family from a hand-coded trio (DIGITS/ALNUM/HEX) into a `ScanLut` registry consulted by both kernels and `bbnf-regex::classify::structural`. The runtime side gets a generic `scan_class_lut(state, &LUT) -> Option<Span>`; the kernel side becomes `emit_class_scan(charset)` that picks the right LUT or emits inline | `parse_that/parsers/scan/digits.rs`, `kernels/charclass.rs`, `kernels/charset_shapes.rs`, `generate/regex/patterns/shorthand.rs` | `parse_that/parsers/scan/class_lut.rs` + `kernels/scan_class.rs` (single source of truth for canonical byte sets) | M4 + over-specialization | the `DIGIT_LUT/ALNUM_LUT/HEX_LUT` triplet is replaced by a registry; `charset_shapes` deleted |
| AR.3 | Promote `EngineSet::FAMILY_HELPER` from a bbnf-regex-side hardcoded list to a `KernelCoverage` mask passed in from bbnf-lang via `EmitOpts.kernel_coverage: EngineSet`. The bbnf-regex side computes everything else; the family bit is OR'd in by the caller | `bbnf_regex::info::derive_feasible_engines` | a new `EmitOpts` field + a one-line `set |= opts.kernel_coverage` in the derivation | M1 | the hardcoded family-variant `matches!` arm in info/mod.rs |
| AR.4 | Fold the five regex-pattern-parser-lites in bbnf-lang into a single `RegexClassEmitter::route(class, opts) -> Option<TokenStream>` that consumes `RegexInfo` and dispatches to `kernels::*` | `emit/{mod,negated_class,generalized/mod,generalized/class_segments}.rs` (5 ad-hoc detectors) | `emit/route.rs` (single dispatch table over `RegexClass` + `RegexInfo` fields) | M2 + M6 (the `",|\s+"` becomes a routed branch) | ~400 LOC of duplicated regex-parser-lite code |
| AR.5 | Promote `WhitespaceWithBlockComment` to `WhitespaceWithComments { line_comment, block_comment }` (parameterized `Option<(open, close)>` for each) and add `WsCommentConfig` to the runtime scanner | `bbnf_regex::RegexClass::WhitespaceWithBlockComment`, `parse_that::scan_ws_block_comments` | parameterized variant + `WsCommentConfig`-driven scanner | M5 | nothing; this is an extension |
| AR.6 | Move `FnDescriptor` specialization out of `lower/expression.rs` into a post-lowering pass that runs after `compute_regex_info`, sharing the cache | `lower/expression.rs::try_specialize_map_fn` (calls `classify_regex` directly) | `passes/specialize_fns.rs` (reads `ir.regex_info[sid].classification`) | M3 + correctness (the lowering-time classification can disagree with the canonicalized HIR-tier classification) | ~50 LOC of `try_specialize_map_fn`; replaces with a clean post-pass |
| AR.7 | Promote `kernels::number::emit_call_span` to expose the generic CSS-style scanner directly (currently the only path is via FnDescriptor::NumberConvert hardcoded in `lower/expression.rs`); add `emit_call_generic` | `kernels/number.rs` (only strict variants exposed) | `kernels/number.rs` exposes both strict + generic | symmetry | nothing; closes the asymmetry |
| AR.8 | Replace bbnf-lang's HIR walker helpers (`is_nullable`, `is_broad_byte_class`, `contains_lazy_quantifier`) with `bbnf_regex::is_nullable` etc. and a published `bbnf_regex::is_broad_byte_class` | `generate/regex/emit/hir/mod.rs` (~50 LOC) | re-export from bbnf-regex | DRY | 50 LOC of duplicated HIR predicates |

## Success metrics

The AR refactor lands successfully when:

1. **Zero language-specific names in bbnf-regex source** (e.g. no
   `JSON`, `CSS`, `BBNF`, `EBNF`, `IDENTIFIER_CSS` in any file under
   `parse-that/rust/regex/`). Every dialect distinction is a structural
   parameter of an existing `RegexClass` variant. **Currently violated**:
   `info/mod.rs::derive_feasible_engines`'s `FAMILY_HELPER` arm
   enumerates a closed list that is bbnf-lang policy.

2. **Every `IrNode::Regex(sid)`-consuming recognizer routes through the
   `RegexClassMiner` parameterized driver.** Per-class miners exist
   only when they need additional grammar-shape data
   (`SeparatorListMiner`, `BalancedWrapMiner`, etc.); pure pattern-class
   passthroughs do not.

3. **Single source of truth for canonical byte sets `[0-9]`,
   `[a-zA-Z0-9]`, `[0-9a-fA-F]`.** Currently three (M4); after AR.2,
   one — published from bbnf-regex.

4. **Zero regex-pattern-string parsing in bbnf-lang.** Every byte-shape
   decision consults `RegexInfo.classification` /
   `RegexInfo.quantified_class` / `RegexInfo.negated_class` /
   `RegexInfo.literal_prefix`. The HIR parse runs once, in
   `bbnf_regex::RegexInfo::analyze_from_hir_with_cost`. **Currently
   five** independent re-parsers in bbnf-lang (M2).

5. **`kernels::*::emit_call*` is the only way `parse_that::scan_*` /
   `parse_that::find_first_of_*` enters generated code.** The
   tranche W phase 5 hard gate already enforces this for the
   six kernel families; AR extends it to the seven not-yet-kernel-routed
   sites in `emit/mod.rs` and `emit/generalized/`.

6. **`emit/mod.rs::emit_regex_fast_path` is a single dispatch table
   over `RegexClass`** plus a fallthrough to the generalized /
   HIR / DFA tiers — no ad-hoc `if pattern == "..."` branches.

7. **`scanner_plan::plan_regex_scanner` routes every parameterized
   `RegexClass` variant to the right kernel config.** Currently
   `Identifier { allows_leading_dash, allows_double_dash_prefix }`
   collapses to the default-config kernel; AR fixes the routing to
   pick `emit_call_css` when the flags are set.

8. **Production line-of-code delta**: AR deletes ~600 LOC of
   duplicated parser-lite + per-class miner boilerplate, adds ~250 LOC
   of unified registry + config + dispatch table, for a **net ~350
   LOC reduction**.

9. **No new orthogonal subsystems**: the four-layer architecture
   (e-graph / facts / CSP / backend) is preserved. The shared canonical
   byte-set registry lives in bbnf-regex (where it is structural fact);
   the kernel-coverage mask lives in bbnf-lang (where it is policy);
   the route table lives in `emit/route.rs` (where it is emission
   policy).

## Mining cross-references

Per-tier consumer counts (production callers excluding tests/docs/benchmarks):

| Symbol | Production callers | Crate |
|---|---|---|
| `kernels::charclass::emit_call_opt` | 2 | `generate/regex/emit/{generalized/mod,generalized/class_segments}.rs` |
| `kernels::charclass::emit_stmt_opt` | 1 | `generate/regex/emit/hir/repetition.rs` |
| `kernels::charclass::charset_from_class_body` | 2 | `generate/regex/emit/generalized/{mod,class_segments}.rs` |
| `kernels::charclass::charset_from_shorthand` | 1 | `generate/regex/emit/generalized/mod.rs` |
| `kernels::charclass::charset_from_byte_ranges` | 1 | `generate/regex/emit/hir/repetition.rs` |
| `kernels::charset_shapes::matches_set` | 2 | `kernels/charclass.rs`, `kernels/prefix_class.rs` |
| `kernels::prefix_class::emit_call_opt` | 1 | `generate/regex/emit/generalized/class_segments.rs` |
| `kernels::number::emit_call_span/fused` | 1 each | `generate/regex/emit/scanner_plan.rs` |
| `kernels::quoted_string::emit_call*` | 2 | `generate/regex/emit/scanner_plan.rs` |
| `kernels::identifier::emit_call` | 1 | `generate/regex/emit/scanner_plan.rs` |
| `kernels::identifier::emit_call_css` | 0 | (orphaned — never called) |
| `kernels::comment_ws::emit_call` | 2 | `generate/regex/emit/scanner_plan.rs`, `backend/rust/emitter/ws.rs` |
| `kernels::balanced_wrap::emit_call` | 1 | `backend/rust/emitter/dispatch.rs` |
| `kernels::punct_ws_region::emit_call` | 1 | `backend/rust/emitter/mod.rs` |

Two findings of note:

- **`kernels::identifier::emit_call_css` has zero production callers.**
  CSS identifier emission must be going through a different code path
  (or the CSS L4 grammar isn't actually using the kernel route for
  identifiers). Worth investigating during AR.4.
- **`scanner_plan::plan_regex_scanner` is the central kernel router**
  (5 of 11 unique kernel call sites flow through it). Tightening this
  function is the highest-leverage refactor.
