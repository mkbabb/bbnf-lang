# H1 — Materialization Framing Disclosure (the honest close, §10 task 1)

`materialization_framing == lazy-rich-vs-eager-cssom`. Disclosed EXPLICITLY, per R-A0-1/R14: the
`track1_rich` workload the SK-V18 CSS >SOTA claim rests on is **LAZY-rich**, NOT eager.

## What `track1_rich` actually does
`track1_rich` parses the stylesheet into the single sealed offset tape, then projects the rich CSSOM
summary LAZILY — every typed field (selectors, declarations, dimensions, numbers, colors, functions) is
RE-DERIVED on demand from the `(source, offset)` spans
(`crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`, the `rich_summary` projection),
writing NOTHING back to an arena. It classifies the LEADING significant token of each declaration value
(`CssTypedValue::classify` on the value head) and counts selectors by top-level commas — it does NOT walk
or own the full value subtree (nested functions, every component value, full selector component list) that
lightningcss materializes.

## The TWO asymmetries, stated plainly
lightningcss builds an EAGER, owned CSSOM (every value/selector node allocated + retained). `track1_rich`
builds a LAZY, HEAD-LEVEL projection. The comparison therefore carries **two** asymmetries, BOTH disclosed:
1. **Retention** — lazy (typed on demand from spans, nothing retained) vs eager (owned tree).
2. **Depth** — value-HEAD classification + comma-counted selectors vs the full owned value+selector tree.
So the comparison is **lazy-rich-head vs eager-full-CSSOM** — NOT "equal-work" and NOT "equal-depth".

## What is and is NOT claimed
- CLAIMED: on the symmetric corpus-in-timer `css_canon_bench` plane (cold, real corpora, both sides parse
  the same bytes), `track1_rich`'s lazy head-level typed projection has higher throughput than
  lightningcss's eager full-CSSOM build.
- NOT CLAIMED: that `track1_rich` does the same ALLOCATION work (it retains nothing) OR the same DEPTH of
  typed work (it classifies value heads, not full subtrees). The speed advantage is partly the
  lazy/no-retain strategy AND partly a shallower typed product — both disclosed here. An unqualified
  "beats CSSOM" / "equal-work" / "equal-depth" claim behind the lazy-rich relabel would be a REJECT
  (R-A0-1); this document is the disclosure that prevents that.

## Why this is fair, not a dodge
The 9-field cssparser/lightningcss parity oracle (`nonjson_css_l4.rs`) proves `track1_rich` produces a
genuine, independently-verified typed product (rules / at_rules / qualified_rules / declarations /
selectors / dimensions / numbers / colors / functions all equal vs an independent cssparser-based
classifier) — NOT a count-only structural probe. But the oracle is itself HEAD-LEVEL: `record_leading_typed_value`
classifies the LEADING significant token of each declaration value, by the same definition the tape
projection uses. So the oracle certifies that two HEAD-level classifications AGREE — it does NOT certify
full-tree depth parity with lightningcss's owned CSSOM. The honest qualifier therefore covers BOTH
asymmetries above (retention AND depth); the >SOTA ratio is real and the typed product is genuinely
parity-checked at the head level, but the claim is explicitly NOT "equal full-tree depth".
