# H1 — Materialization Framing Disclosure (the honest close, §10 task 1)

`materialization_framing == lazy-rich-vs-eager-cssom`. Disclosed EXPLICITLY, per R-A0-1/R14: the
`track1_rich` workload the SK-V18 CSS >SOTA claim rests on is **LAZY-rich**, NOT eager.

## What `track1_rich` actually does
`track1_rich` parses the stylesheet into the single sealed offset tape, then projects the rich CSSOM
summary LAZILY — every typed field (selectors, declarations, dimensions, numbers, colors, functions) is
RE-DERIVED on demand from the `(source, offset)` spans
(`crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`, the `rich_summary` projection),
writing NOTHING back to an arena. It performs equal-DEPTH typed value work — it walks and types the same
value nodes lightningcss does — but it materializes them transiently rather than retaining an owned tree.

## The asymmetry, stated plainly
lightningcss builds an EAGER, owned CSSOM (allocated nodes retained in memory). `track1_rich` builds a
LAZY projection (typed on demand from spans, nothing retained). Both do the typed value work; they differ
in RETENTION. So the comparison is **lazy-rich vs eager-CSSOM**, not "equal-work" in the allocation sense.

## What is and is NOT claimed
- CLAIMED: on the symmetric corpus-in-timer `css_canon_bench` plane (cold, real corpora, both sides parse
  the same bytes and produce a typed CSSOM-equivalent product), `track1_rich` throughput exceeds
  lightningcss — the lazy-rich projection is faster than the eager CSSOM build at equal typed-value depth.
- NOT CLAIMED: that `track1_rich` does the same ALLOCATION work as lightningcss. It does not retain an
  owned tree; the speed advantage is partly the lazy/no-retain strategy, disclosed here. An unqualified
  "beats CSSOM" / "equal-work" claim behind the lazy-rich relabel would be a REJECT (R-A0-1); this
  document is the disclosure that prevents that.

## Why this is fair, not a dodge
The 9-field cssparser/lightningcss parity oracle (`nonjson_css_l4.rs`) proves `track1_rich` produces the
SAME typed product (rules / at_rules / qualified_rules / declarations / selectors / dimensions / numbers /
colors / functions all equal). So the value DEPTH is genuinely equal — this is not a count-only structural
probe. The honest qualifier is solely about RETENTION (lazy vs eager), disclosed above.
