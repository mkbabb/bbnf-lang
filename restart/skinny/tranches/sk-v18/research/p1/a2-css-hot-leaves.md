# SK-V18 S-P1 / A2 — CSS Hot-Leaf Attribution + G6 Wire-or-Retire Gate

## Honesty preamble (load context)

Capture `capture.log` ran under **host_loadavg 4.35 6.03 5.70** (concurrent-session
machine load). Per the S-P1 contract, today's **absolute Mbps is DIRECTIONAL/depressed**
versus the W0-locked baseline and must NOT be re-locked as a baseline. The load-bearing,
load-robust outputs are (1) the same-run >SOTA ratios `track1_rich / lightningcss` and
`track1_full_parse / lightningcss`, and (2) the relative hot-leaf RANK. Both are reported
below; absolute Mbps appears only as directional color.

## What the sample actually drove

`skinny/crates/bbnf-bench/src/bin/css_track1_profile.rs` (line 31) loops **only**
`runtime::generated_css_l4_declaration_values::parser::parse_full` and accumulates
`out.len()` — it does **NOT** call `rich_summary`. So this sample directly attributes the
**recognizer** (`parse_full` = `CssFullParser` `parse_block` → `parse_declaration` →
`find_component_delim`/`consume_balanced_at`) over the SK-V14 corpora, i.e.
`track1_full_parse`. The leaf ranking below is **measured for the recognizer**. It bounds
the rich product's scan cost only by construction, NOT by direct sampling: `rich_summary()`
(generated.rs:305) re-walks the SAME tape via `self.nodes()` (the same `parse_full` produced)
and projects 9 typed fields atop it, so the shared scalar scan is a floor on `track1_rich`'s
cost too — but the rich-plane projection symbols are NOT in this capture (the driver never
ran them). §"Rich-projection cost leaves" below records that S-P2 must sample `rich_summary`
directly to attribute the projection plane; this capture is recognizer-only.

## Load-robust >SOTA ratios (same-run, the real bar)

| corpus | track1_rich / lcss | track1_full / lcss |
|---|---|---|
| bootstrap | **2.19x** | 2.34x |
| tailwindcss | **3.38x** | 3.39x |
| material-components-web | **1.66x** | 1.89x |
| animate | **2.10x** | 1.99x |

The rich typed-CSSOM projection (the >SOTA product) still beats the lightningcss full-CSSOM
bar by 1.66x–3.38x even under depressed load. SOTA holds.

## Per-leaf self-sample ranking (css_sample.txt "Sort by top of stack, same collapsed")

Raw self counts (1ms sampler, 6160 total in-thread samples):

| rank | leaf (demangled) | self count | parser-share* | total-share** |
|---|---|---|---|---|
| 1 | `CssFullParser::find_component_delim` | 3483 | **79.5%** | 56.6% |
| — | `css_track1_profile::main` (corpus loop) | 1770 | excluded | 28.8% |
| 2 | `CssFullParser::consume_balanced_at` | 638 | 14.6% | 10.4% |
| 3 | `CssFullParser::parse_block` | 185 | 4.2% | 3.0% |
| 4 | `CssFullParser::parse_declaration` | 60 | 1.4% | 1.0% |
| 5 | `CssFullParser::parse_at_rule` | 13 | 0.3% | 0.2% |

\* parser-share = count / 4379 (sum of all PARSER leaves, `main` corpus-loop frame excluded
from the denominator). \*\* total-share = count / 6149 (parser leaves + `main`).

Note: the sampler's `>= 5` collapse threshold cuts the tail; only 5 parser leaves clear it.
`main` (1770, total-share 28.8%) is the corpus String-iteration / black_box loop overhead of
the driver harness, not parse work — reported but excluded from the parser denominator. There
is no String-emit leaf above threshold: the recognizer path allocates nothing hot.

### The top-8 leaves (all that exist + the inner classifier lines they roll up)

Only 5 parser leaves + `main` clear the collapse threshold; there is no 8th distinct
parser symbol. For completeness the inner call-tree (lines 27–221) shows `find_component_delim`
self-time concentrates in `generated.rs:664` (`delimiters.contains(&byte)` membership test),
`:667` (the `match byte` skip dispatch), and `:663` (the byte load); `consume_balanced_at`
self-time concentrates in `:700` (its inner skip `match`) and `:696`/`:709` (load + loop).
These are line attributions within the two scalar-scan leaves, not separate functions.

## The scalar-scan share

`find_component_delim` and `consume_balanced_at` are the **same scalar scanning machine**:
both walk bytes one at a time, testing a delimiter set and dispatching on the structural
bytes `' " / ( [ { ) ] }` to skip strings/comments/balanced groups (generated.rs:662–680 and
693–713). `consume_balanced_at` is the mutually-recursive inner half of the identical scan.

> **Combined scalar-scan self-share = 4121 / 4379 = 94.1% of parser self-time**
> (67.0% of total including the harness loop). `find_component_delim` alone = **79.5%**.

The hot path is overwhelmingly the scalar byte-at-a-time delimiter scan.

## G6 WIRE-OR-RETIRE verdict

### The sharper finding: the dead NEON does not even cover the hot leaf

S-P0 R7 is **confirmed by direct inspection**. The two NEON kernels written —
`runtime_simd::find_css_significant` (runtime_simd.rs:169) and `find_comment_close`
(runtime_simd.rs:112) — have **zero live callers**. Their only callers are the
`#[cfg(test)] mod tests` parity guards in `runtime/src/lib.rs` (`#[cfg(test)]` at line 51,
`mod tests {` at line 52; the calls at lines 574/598/608 are all inside it: `neon_significant_skip_matches_scalar`,
`neon_comment_close_matches_scalar`). No grammar `generated.rs` and no bench bin references them.
The live hot scan is 100% scalar.

Crucially, **the dead NEON was written for a DIFFERENT function than the hot leaf.**
`find_css_significant` is a flat "stop at first delimiter-or-structural-byte" skip — it
does NOT do the balanced bracket/string/comment *consumption* that the hot
`find_component_delim` + `consume_balanced_at` machine performs (which recurses through
nested `()[]{}` and skips quoted strings). So even if `find_css_significant` were wired in,
it would only accelerate the *flat-skip portion between structural bytes*, not the
balanced-consume recursion (`consume_balanced_at`, 14.6% parser-share) that the hot leaf
fans into. The existing dead NEON does not cover the dominant hot path as written.

### Verdict: WIRE (under dav1d discipline) — numeric basis

The scalar scan is the **#1 and #2 self-time leaves at a combined 94.1% parser-share**
(find_component_delim alone 79.5%), far above the ~8% wire threshold. There is real,
large headroom. Recommend **WIRE**, but with two hard conditions grounded in the finding above:

1. **The NEON must be (re)targeted to the hot leaf**, not merely wired as-is. The kernel that
   actually needs acceleration is the `find_component_delim` flat-skip inner loop
   (generated.rs:662–680): vectorize the "advance to next byte in
   `{delimiters} ∪ {' " / ( [ { ) ] }`" search (16/32-byte NEON `vceqq` + reduce, the same
   shape as `find_css_significant`). The balanced-consume recursion (`consume_balanced_at`)
   can reuse the same inner skip for its `match byte` advance.
   **Grammar-neutrality constraint (Lock-14 / G3 / G6):** `find_component_delim` is NOT a
   single function — it is **replicated byte-identically across 7 `css_l4_*/generated.rs`
   files** (the same 7 replicas P3 collapses). The retargeted NEON must therefore land as a
   **shared, grammar-neutral runtime primitive that the generated scan CALLS** (e.g. the
   existing `runtime_simd` surface), NOT as bespoke vector code re-emitted per-grammar into
   each `generated.rs` — re-emitting it 7 ways pre-collapse would re-fork the very thing G3
   un-forks and re-introduce the per-grammar bespoke shape G6 polices. Sequencing: the WIRE
   consumes the **P3-collapsed single CSS scan**; the emitter calls the shared primitive,
   the generator does not author the kernel. (Pre-P3 the call site exists in 7 copies; the
   primitive itself stays singular and grammar-agnostic regardless.)
2. **dav1d discipline**: scalar reference FIRST (the `significant_ref` already in lib.rs:506
   is exactly this), checkasm-style differential parity over the corpora (the existing
   `neon_*_matches_scalar` tests are the seed), aarch64 NEON only (x86 is a prune target).

**RETIRE alternative is rejected**: retiring would delete a kernel that targets a 94%-share
hot path. The honest action is not to delete the dead NEON but to **finish wiring it (after
retargeting to the consuming scan)** so it stops being dead and starts paying the 94% it can
address. (If a future iteration finds the NEON cannot be safely retargeted to cover the
balanced-consume recursion, only then retire `find_comment_close` specifically — it maps to
`consume_comment_at` which never surfaced as its own leaf above threshold, i.e. comments are
cold in these corpora.)

## Rich-projection cost leaves for S-P2 to ground the grammar-driven projection

The rich path (`track1_rich`) is the same scan plus the typed projection. None of the
projection symbols surfaced as their own self-time leaves above the `>=5` threshold in this
recognizer-only sample (the driver calls `parse_full`, not `rich_summary`), so S-P2 must
sample `rich_summary` directly. The projection surface to ground, from generated.rs:

- `CssRichSummary CssFullParser::rich_summary` (generated.rs:305) — the 9-field driver loop
  over `self.nodes()`; the `match node.value()` dispatch per node.
- `nodes()` iteration (generated.rs:307) — the lazy span re-walk; every field re-derived from
  `(source, offset)`, writing nothing to the payload arena (preserve-rich-ast: lazy not eager).
- `CssDeclaration::typed_value` (generated.rs:201) → `CssTypedValue::classify` (generated.rs:229)
  — the per-declaration-value-head classification into
  dimension/number/color/function/keyword. This is the rich-plane hot candidate: it runs once
  per declaration and itself does a small byte scan of the value head.
- `rule.selector_count()` / `rule.is_at_rule()` (generated.rs:316/309) — selector-list counting
  per qualified rule.

S-P2's grammar-driven projection generator must reproduce these 9 fields (rules, at_rules,
qualified_rules, declarations, selectors, dimensions, numbers, colors, functions) from the
grammar's `->` typed materialization, and should expect `classify` + `nodes()` lazy re-walk to
be its dominant cost once the shared scalar scan is NEON-accelerated.

---

### TOP-8 LEAVES (return summary)

1. `find_component_delim` — 3483, **79.5%** parser-share (scalar scan #1)
2. `main` (corpus loop, harness, EXCLUDED from parser denom) — 1770, 28.8% total
3. `consume_balanced_at` — 638, 14.6% parser-share (scalar scan #2, inner-recursive half)
4. `parse_block` — 185, 4.2%
5. `parse_declaration` — 60, 1.4%
6. `parse_at_rule` — 13, 0.3%
   (no 7th/8th parser leaf clears the sampler's `>=5` collapse threshold)

**Scalar-scan share = 94.1% of parser self-time** (find_component_delim + consume_balanced_at).

**G6 VERDICT = WIRE** (numeric basis: 94.1% >> ~8% threshold), under dav1d discipline,
with the explicit caveat that the dead NEON (`find_css_significant`/`find_comment_close`)
was written for a DIFFERENT/flatter function than the hot leaf and must be **retargeted to
the live `find_component_delim` scan**, not wired as-is. The dead kernels are confirmed
`#[cfg(test)]`-only (R7 verified).
