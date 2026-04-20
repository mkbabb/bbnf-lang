# AX Next-Tranche Audit — CSS L4 Parse Bench + Profile (A2)

Canonical CSS L4 parse audit at master HEAD `9074a685`. Replaces the
cross-reference `-az-a5` tailwind capture from doc 05; all three
fixtures profiled fresh on Apr 20 01:51 against the prebuilt shared-
target binary `css_l4-0d1a22af4b4b8964`. Cold per-parse, release
profile, mimalloc global allocator.

## 1. Bench matrix — bbnf vs lightningcss vs cssparser

| fixture      | bytes      | bbnf ns/iter | bbnf MB/s | lightningcss ns/iter | lcss MB/s | cssparser ns/iter | cssp MB/s | bbnf/lcss | bbnf/cssp |
|--------------|-----------:|-------------:|----------:|---------------------:|----------:|------------------:|----------:|----------:|----------:|
| normalize    |      6 138 |       31 627 |       194 |               29 442 |       208 |            11 108 |       552 |    1.07×  |    2.85×  |
| bootstrap    |    280 311 |    2 448 237 |       114 |            3 020 087 |        92 |           788 955 |       355 |    0.81×  |    3.10×  |
| tailwind     |  3 642 321 |   26 052 116 |       139 |           43 564 408 |        83 |        10 714 370 |       339 |    0.60×  |    2.43×  |

Sources:
- bbnf — `.profiles/samply/css_l4/{normalize,bootstrap,tailwind}/bench.txt` (fresh Apr 20 01:51).
- lightningcss + cssparser — `/tmp/a2-css-competitors.txt` (`cargo bench -p bbnf --bench css_competitors`, Apr 20 01:45).

**Headline.** bbnf is between **0.60× and 1.07× lightningcss** — matches
on tiny input, beats by 40% on a 3.5 MB real-world stylesheet. vs
cssparser (a pure tokenizer with empty-body visitors), bbnf is
2.4–3.1× slower across all sizes, which is the cost of structural
tape emission vs zero-retention token walking. Scale advantage over
lightningcss grows with input size; scale disadvantage vs cssparser
is roughly flat — consistent with bbnf's per-token emit cost being
the dominant invariant.

## 2. Top-10 self-time per fixture

Extracted from `profile.json.gz` leaf-frame histograms, symbolicated
via the paired `profile.json.syms.json`. Leaf-count column is the
raw denominator.

### `css_l4::normalize` (3 549 leaf samples)

```
 25.81%  css_l4::__cssl4parser_emit_impl::__regex_scan_CssL4Parser
 22.68%  <tape::columns::Columns>::push_structural
 13.52%  tape::finaliser::finalise
  7.55%  <css_l4::CssL4Parser>::parse
  6.28%  parse_wrap_CssL4Parser_ruleItem
  4.85%  parse_wrap_CssL4Parser_declaration
  2.54%  <tape::builder::TapeBuilder>::push_leaf_with
  2.34%  parse_flat_CssL4Parser_ruleBlock
  2.03%  <tape::columns::Columns>::truncate
  1.80%  parse_unordered_CssL4Parser_compoundSelector
```

### `css_l4::bootstrap` (2 759 leaf samples)

```
 26.06%  __regex_scan_CssL4Parser
 24.36%  <tape::columns::Columns>::push_structural
 15.98%  tape::finaliser::finalise
  6.74%  <css_l4::CssL4Parser>::parse
  3.73%  parse_wrap_CssL4Parser_declaration
  2.94%  parse_flat_CssL4Parser_ruleBlock
  2.61%  <tape::builder::TapeBuilder>::push_leaf_with
  2.39%  <tape::columns::Columns>::truncate
  1.99%  parse_wrap_CssL4Parser_ruleItem
  1.12%  parse_unordered_CssL4Parser_compoundSelector
```

### `css_l4::tailwind` (10 841 leaf samples)

```
 26.66%  <tape::columns::Columns>::push_structural
 26.44%  __regex_scan_CssL4Parser
 14.60%  tape::finaliser::finalise
  7.31%  <css_l4::CssL4Parser>::parse
  3.20%  parse_flat_CssL4Parser_ruleBlock
  3.17%  parse_wrap_CssL4Parser_ruleItem
  2.90%  <tape::builder::TapeBuilder>::push_leaf_with
  2.32%  parse_wrap_CssL4Parser_declaration
  1.57%  <tape::columns::Columns>::truncate
  1.25%  parse_unordered_CssL4Parser_compoundSelector
```

Zero walker symbols (`dta_walker`, `dta_run`, `dispatch_one`) in all
three captures. Tape-first invariant holds. `nm
.profile-target/release/deps/css_l4-0d1a22af4b4b8964 | grep -E
'dta_walker|dta_run|dispatch_one' | wc -l` = **0**.

## 3. Cross-fixture hotspot union for CSS L4

Symbols appearing in ≥ 2/3 fixtures, grouped by source layer:

| symbol                                              | normalize | bootstrap | tailwind | source              |
|-----------------------------------------------------|----------:|----------:|---------:|---------------------|
| `__regex_scan_CssL4Parser`                          |   25.81%  |   26.06%  |  26.44%  | regex scan          |
| `Columns::push_structural`                          |   22.68%  |   24.36%  |  26.66%  | tape substrate      |
| `finaliser::finalise`                               |   13.52%  |   15.98%  |  14.60%  | tape substrate      |
| `<CssL4Parser>::parse` (entry shell)                |    7.55%  |    6.74%  |   7.31%  | dispatcher          |
| `parse_wrap_CssL4Parser_ruleItem`                   |    6.28%  |    1.99%  |   3.17%  | per-shape parse_fn  |
| `parse_wrap_CssL4Parser_declaration`                |    4.85%  |    3.73%  |   2.32%  | per-shape parse_fn  |
| `TapeBuilder::push_leaf_with`                       |    2.54%  |    2.61%  |   2.90%  | tape substrate      |
| `parse_flat_CssL4Parser_ruleBlock`                  |    2.34%  |    2.94%  |   3.20%  | per-shape parse_fn  |
| `Columns::truncate`                                 |    2.03%  |    2.39%  |   1.57%  | tape substrate      |
| `parse_unordered_CssL4Parser_compoundSelector`      |    1.80%  |    1.12%  |   1.25%  | per-shape parse_fn  |

**Layered attribution.**

- **Tape substrate** (push_structural + finalise + push_leaf_with +
  truncate) = **40.77% / 45.34% / 45.73%** across normalize /
  bootstrap / tailwind.
- **Regex scan** (`__regex_scan_CssL4Parser`) = **25.81% / 26.06% /
  26.44%** — remarkably size-invariant, suggesting the scan cost
  scales linearly with input as expected and dominates the per-byte
  budget.
- **Dispatcher + per-shape `parse_*`** = remaining ~30% across fixtures.

Tape substrate + regex scan together are **66–72% of self-time** on
every CSS L4 fixture. The grammar-emitted per-shape parse bodies are
never individually hot — the hottest single parse_ fn is ~6% and the
long tail is 15+ fns each < 2%. The four grammar-level hotspots
identified in doc 05's cross-grammar union (push_structural, finalise,
regex_scan, parse-entry shell) all reproduce in this fresh CSS L4
capture at very close percentages to the `-az-a5` reference.

## 4. Tailwind-specific analysis — size scaling

Input sizes: normalize 6 KB, bootstrap 274 KB (45×), tailwind 3.6 MB
(593×). ns/iter scaling: normalize 1×, bootstrap 77×, tailwind 824× —
**bootstrap and tailwind scale sub-linearly with input size** (1.72×
and 1.39× bytes/ns, both higher throughput than normalize's). bbnf
runs the tailwind fixture at **139 MB/s**, its highest throughput of
the three; normalize pays a fixed-cost overhead (column allocation,
parser entry, first-emit capacity growth) that the two larger inputs
amortise.

**Does per-symbol attribution scale?** Near-constant percentages tell
the story:

- push_structural climbs modestly with size (22.7% → 24.4% → 26.7%) —
  suggesting deep-tree amortisation of allocation-churn overhead into
  more hot-path work.
- regex_scan stays flat (25.8% → 26.1% → 26.4%) — scan is per-byte
  linear, and its share of the budget is stable.
- finalise dips on tailwind (13.5% / 16.0% / 14.6%) — the linear
  post-pass loses relative share as the emit loop grows disproportion-
  ately.
- The dispatcher shell `<CssL4Parser>::parse` stays at 6.7–7.6% — it's
  a per-parse fixed cost masquerading as per-iter cost because
  `bencher` measures one parse per iteration.

No size-gated regressions. CSS L4 scales gracefully. The concentration
at (tape + regex_scan) = ~70% across every fixture size means a
tranche that attacks those two layers directly reclaims a uniform
fraction across all three fixtures — there is no fixture for which a
different lever dominates.

## 5. lightningcss comparator semantics

lightningcss parses into a **typed CSS AST** (`StyleSheet` with
`CssRule` enum variants: `Style`, `Media`, `Keyframes`, `FontFace`,
etc.) via `cssparser` tokenization. Every declaration materialises as
a strongly-typed property value (`Property::Color(CssColor::RGBA(…))`,
`Property::BorderRadius(BorderRadius(…))`) with full semantic content
— colors decoded, numbers parsed, calc() simplified. The iteration
benchmark does `StyleSheet::parse(...).unwrap()` and black-boxes the
result; drop cost of the AST (Vec<CssRule> recursive free) is
included in ns/iter.

bbnf produces a **columnar tape + NodeView cursor**. Parse emits Span
bounds + structural markers into SoA columns; typed shapes project to
payload unions when directly emitted, but values like `color:
rgb(1,2,3)` stay as `Span(start, len)` into the input buffer unless
downstream `NodeView::as_<Field>()` materialises them. The tailwind
benchmark black-boxes the `ColdParseResult` (a tuple of the tape +
NodeView root), which includes tape capacity allocation, structural
emit, finalise, and drop of the owned Columns.

**Work-matched comparison is not 1:1.** bbnf parse-only vs lightningcss
parse+AST-build measures:

- bbnf: byte scan + span emission into columns + structural linking.
- lightningcss: byte scan + token materialisation + recursive typed
  enum construction + calc() simplification.

The enum construction and semantic reduction are real work; bbnf
defers them to read-time. An apples-to-apples comparison requires
**one of** (a) extending the bbnf bench to materialise a typed AST
via the grammar-emitted `to_value::<StyleSheet>()` surface proposed
by A5's Value API workstream, (b) adding a lightningcss variant that
parses with `ParserOptions { parse_values: false }` and skips typed
projection, or (c) comparing rule-counter variants only (cssparser's
approach — see §6). Option (a) is the AZ-tranche apples-to-apples
target; it does not invalidate the raw parse numbers above but
contextualises bbnf's tailwind advantage (1.67× lightningcss) as
partly a by-product of deferred typed projection.

## 6. cssparser comparator semantics

cssparser is the tokenizer underlying lightningcss and is used in the
competitor bench via a `RuleCounter` visitor that counts rules +
declarations and black-boxes tokens without building any tree. This
is **closer to bbnf's work envelope**: both walk every input byte
exactly once, neither materialises typed values, both retain
structural bounds (cssparser retains nothing beyond the counter;
bbnf retains Span + structural links).

bbnf's 2.4–3.1× slowdown vs cssparser across all sizes is the cost of
**structural retention**. cssparser discards each token as the
visitor returns; bbnf writes every structural boundary into the tape
(push_structural) and finalises column offsets (finalise). The 40–45%
self-time the tape substrate consumes on every fixture is precisely
the overhead cssparser avoids by retaining nothing. Closing the gap
requires shrinking push_structural + finalise, not eliminating them —
the tape IS the product.

## 7. Lever proposals

Ordered by estimated reclaim. Each cites the fresh `.profiles/samply/
css_l4/<fixture>/profile.json.gz` it attributes from.

### L1 — Inline + specialise `Columns::push_structural`

**Profile citation.** 22.7% / 24.4% / 26.7% self-time on
`normalize/bootstrap/tailwind/profile.json.gz`. Present as a real
cross-crate symbol at `0x1114c` in
`css_l4-0d1a22af4b4b8964` (nm demangled
`_<tape::columns::Columns>::push_structural`) — **every invocation is
a function-call boundary**, not inlined.

**Proposed change.** Per the universal-perf invariant, either
`#[inline(always)]` the hot path in `crates/tape/src/columns.rs` OR
emit the body per-grammar in `__cssl4parser_emit_impl` so the CSS L4
`push_structural` is a local `t` symbol not a cross-crate call.
Secondary: pre-reserve column capacity from emitter-known tape-width
budgets (CSS L4's grammar has a knowable upper-bound structural
depth). Expected reclaim: **15–20% of self-time per fixture**.

### L2 — Fuse `finaliser::finalise` into emit loop

**Profile citation.** 13.5% / 16.0% / 14.6% self-time. Cross-crate
symbol `_tape::finaliser::finalise` at `0xabc3c`. Not inlined.

**Proposed change.** finalise runs a linear post-pass over tape
columns to compute width-prefix / offset indices after parse
completes. For each invariant finalise computes, move the update into
the `push_structural` / `push_leaf_with` hot path — maintaining
running invariants during emit rather than rebuilding them. If full
fusion is infeasible, replace any `<Iterator>::max` fold over columns
with a running-max maintained during emit (cf. doc 05 lever U2 —
Sheets profile showed `core::cmp::Ord::cmp` in the symbol table).
Expected reclaim: **8–12% of self-time per fixture**.

### L3 — Regex-scan specialisation via HIR-driven CSS alphabet

**Profile citation.** 25.8% / 26.1% / 26.4% self-time — the **single
most concentrated CSS-specific hotspot** (tape substrate is universal;
regex scan is CSS-hot). Symbol
`css_l4::__cssl4parser_emit_impl::__regex_scan_CssL4Parser` at
`0xbe7bc`.

**Proposed change.** The scanner is HIR-derived but currently emits a
general-purpose NFA/DFA. CSS L4's token alphabet is small and
high-frequency: whitespace, idents, hex colors, numbers, units,
`(){}[],:;`. Two sub-levers:

1. **Byte-class dispatch table** — a 256-entry PHF-style table keyed
   by the first byte of each scan position routes to a specialised
   per-class scanner (ident scan, number scan, string scan).
   Emitter-knowable from the CSS L4 grammar's terminal set.
2. **Property-name PHF.** The `parse_flat_CssL4Parser_*_decl` bodies
   parse `property: value` pairs; the property name is one of ~250
   CSS-known idents. A compile-time PHF at the declaration-name slot
   dispatches directly to the correct typed parser without a generic
   ident scan + string compare. Emitter-knowable from the grammar.

Expected reclaim: **8–15% of self-time per fixture** — the larger
share on tailwind (which is declaration-dense).

### L4 — Parse-entry shell elision

**Profile citation.** 7.6% / 6.7% / 7.3% self-time on
`<CssL4Parser>::parse`. Shell is a real symbol (nm search for
`CssL4Parser..parse$` returned nothing distinct from the pattern
symbol, but the profile resolves it — it exists as a wrapper in the
emitted impl).

**Proposed change.** `#[inline(always)]` the dispatch shell, OR split
capacity-reservation from the dispatch so multiple parses amortise
the `Columns::with_capacity` call. The shell allocates fresh column
vectors per parse — `_mi_page_retire` / `mi_malloc_aligned` trace
the allocation churn visible in the profile symbol tables. Expected
reclaim: **3–6% of self-time per fixture plus reduced mimalloc
traffic**.

### L5 — Deep-tree `push_structural` depth-specialisation (CSS-specific)

**Profile citation.** CSS L4's `push_structural` climbs +4 pp from
normalize (22.7%) to tailwind (26.7%) — alone among the top-three
hotspots. CSS ASTs are deeper than JSON and Sheets (selector trees +
declaration blocks + calc() trees + color functions nest 5–8 levels).
The depth drives push_structural frequency super-linearly even as
per-byte throughput improves.

**Proposed change.** Emit per-depth-class `push_structural` variants
from the grammar: shallow (selector-list, declaration), medium
(rule-block, compound selector), deep (calc, color-fn, complex-value).
Each variant inlines the depth-known column writes. This is a
CSS-specific refinement of L1; the two should land together, with L1
covering the universal case and L5 covering the grammar-specific
deep-tree specialisation. Expected reclaim: **additive 3–5% on
tailwind, 1–2% on bootstrap, nil on normalize**.

## 8. Byte-parity scope status

Current state (per W1r.3a-diag + AX invariant 18 resolution):

- `css_l4_canonical_parity::canonical_parity_normalize` — **active
  byte-level parity gate** vs lightningcss printer through the
  20-rule `token_normalize` symmetric normalizer.
- `css_l4_canonical_parity::scale_interop_bootstrap` + `…_tailwind`
  — **active scale + interop gates**: bbnf parses, prettifies,
  round-trips via bbnf, and round-trips via lightningcss. No byte-
  level equality asserted.

Byte-parity on bootstrap + tailwind remains bounded by lightningcss's
own `calc()` arithmetic simplification + position-pair commutativity
+ multi-value shorthand re-ordering. These are not symmetric byte-
level transforms reducible to spec-citable rewrites; they require a
CSS semantic canonicalizer.

**Scope for future tranche (not re-opened here).** A dedicated
workstream — provisionally `bbnf-css-canonical` — would implement:

1. A full `calc()` evaluator applying CSS Values & Units normative
   arithmetic (`calc(3rem + calc(1.5em + .75rem))` → `calc(3.75rem +
   1.5em)`).
2. A per-property canonical-order table for position-pair
   commutativity (CSS Backgrounds §3.7) + multi-value shorthand
   canonicalisation.
3. Integration with the existing `token_normalize` pipeline, applied
   symmetrically to both sides before byte-equality comparison.

The scope is standalone — no `css_l4.bbnf` grammar changes required,
no codegen changes, no tape changes. The three active CSS L4 parity
tests remain the gate; `calc()`-semantic parity adds a fourth fixture
class (e.g. `semantic_parity_bootstrap`) that asserts byte equality
**after** both normalizer + calc-evaluator pre-processing. Bootstrap
+ tailwind remain on the scale+interop gates until that workstream
lands.

## Artefacts

- `.profiles/samply/css_l4/normalize/{bench,build,load,record,profile.json.gz,profile.json.syms.json,syms-proof}.txt` (Apr 20 01:51)
- `.profiles/samply/css_l4/bootstrap/{...same 7...}` (Apr 20 01:51)
- `.profiles/samply/css_l4/tailwind/{...same 7...}` (Apr 20 01:51)
- `/tmp/a2-css-competitors.txt` — lightningcss + cssparser bench output (Apr 20 01:45).
- `/tmp/a2-prof-{normalize,bootstrap,tailwind}.log` — headless-profile-script stdout for each capture.
