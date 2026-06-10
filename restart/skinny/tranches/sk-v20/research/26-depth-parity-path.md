# 26 — The Depth-Parity Path: Genuine Equal-Depth Typed CSS Under the Lazy Model

## The chronic asymmetry, stated mechanically

The CSS >SOTA claim rests on `track1_rich`, which classifies the **leading significant token** of
each declaration value and counts selectors by top-level commas — while lightningcss builds the full,
owned, eager CSSOM. This is not a framing nicety; it is a structural property of the recognizer and
the tape.

The recognizer (`CssFullParser`) emits exactly **three** structural tape kinds —
`QualifiedRule`, `AtRule`, `Declaration` — one offset per node, no payload
(`css_l4_declaration_values/generated.rs:494-511`: `push_rule` pushes a rule-open offset with one
sparse at-rule flag bit; `push_declaration` pushes the colon offset with no flag and no payload).
The declaration **value subtree is never placed on the tape**. The typed value is recovered, on
demand, by re-scanning source bytes: `value_span()` forward-scans the balanced run to the value
terminator (`generated.rs:237-243` → `scan_value_end` at `:811-830`), then `CssTypedValue::classify`
switches on the **first byte** of the trimmed span (`generated.rs:274-298`) into six head categories
(Dimension/Number/Color/Function/Keyword/Other). `rich_summary` (`generated.rs:344-370`) tallies one
category per declaration and `1 + count_top_level_commas(prelude)` selectors per qualified rule.

So the rich product is, per declaration, **one head classification** and, per rule, **one comma
count**. It walks nothing below the head: a `border: 1px solid rgb(0 0 0 / .5)` declaration is typed
`Dimension` (the leading `1px`) and the `solid` keyword, the `rgb(...)` function, the three channel
numbers, the alpha, and the nested component list are **never touched**.

## Why JSON does not have this problem, and CSS does

The asymmetry is not intrinsic to laziness — it is intrinsic to **what the CSS recognizer puts on the
tape**. The JSON grammar's recognizer emits a tape event **per container boundary**:
`JsonNodeKind { ObjectOpen, ObjectClose, ArrayOpen, ArrayClose, ... }`
(`grammars/json/value.rs:12-17`), and `value_from_ref` (`value.rs:143-150`) projects an
`ObjectOpen`/`ArrayOpen` cursor into a `JsonObject`/`JsonArray` whose children are themselves lazy
cursors. JSON's lazy model is therefore **full-depth-capable**: descending into a nested array is a
cursor advance over tape cells that already exist, with zero re-scan. JSON laziness defers
*materialization* but not *structure* — every nesting level is a recorded event.

CSS records **no value-internal structure at all**. There is no `FunctionOpen`, no `ComponentValue`,
no nesting event below `Declaration`. The lazy CSS projection cannot descend because there is nothing
on the tape to descend into; it can only re-scan bytes and read the head. The depth asymmetry is
exactly this gap: **JSON's tape is depth-isomorphic to its values; CSS's tape is flat at the
declaration boundary.** Any honest equal-depth path must close *this* gap, not adjust prose.

## The grammar already specifies the depth — the codegen discards it

The most damning finding: the CSS L4 grammar **already describes the full typed value tree**, and the
codegen throws it away. `grammar/css/l4/values.bbnf:39-52` defines `mathValue`/`mathProduct`/`mathExpr`
as a genuinely recursive structure (nested `calc()`/`min()`/`max()`/`clamp()`, parenthesised
sub-expressions). `grammar/css/l4/color.bbnf:36+` projects 148 named colors to packed RGBA u32
(`"aliceblue" -> 0xF0F8FFFFu32`), `gradients.bbnf:13-35` projects gradient kinds / sides / shapes to
`Nu8` discriminants, `values.bbnf:74-78` projects global keywords. These `->` projections are the
typed, full-depth product — and they are **identical in spirit to the Sheets typed value layer**
that `derive(value-api)` (commit `b7ab7c558`) already wired end-to-end: `classify_projection` over a
rule's body yields `as_f64`/`as_bool`/`as_tag` from a derived `TAG_TABLE`, with zero hardcoded rule
names.

The CSS recognizer does not consume this grammar's value rules. The "Scan shape" codegen
(`crates/codegen/src/css_scan_direct/mod.rs:3-9`) renders a **flat balanced-delimiter driver** that
harvests only the *structural alphabet* (per-call-site STOP arrays + the fixed-9 SKIP array,
`mod.rs:109-152`) and emits the 3-kind tape. The recursive `mathExpr`, the projected `namedColor`,
the gradient discriminants — none reach the recognizer or the tape. **The depth was lowered away.**
This is the architectural locus of the asymmetry: one codegen path (Scan) intentionally flattens the
value subtree the grammar took pains to type.

## The oracle does the full-depth work, then discards it

The 9-field parity oracle is **itself head-level** by construction, and worse: its cssparser reference
already **performs** the full-depth tokenization, then collapses it to head counts. `OracleParser`
recurses into every nested block (`nonjson_css_l4.rs:298-351`: `Token::Function` /
`ParenthesisBlock` / `SquareBracketBlock` each `parse_nested_block` → `emit_component_values` →
`push_token` for ident/hash/function/number/percentage/dimension/string/url at full depth). But the
rich-summary equality (`assert_rich_strict_equality`, `:128-135`) calls `cssparser_rich_summary_facts`
(`:110-120`), whose only typed signal is `record_leading_typed_value` over `input[value_start..last_end]`
— the **leading** token (`:204-226`). The per-token `sink.token(...)` lines exist (`:549-566`) but
are NOT part of the equality. So the oracle certifies that **two head classifications agree**, while
the reference engine already proves it *could* certify full-tree parity — the depth signal is computed
and thrown on the floor. The oracle is a proof of head-agreement masquerading near a proof of depth.

(Note an internal contradiction the path must retire: `sk-v18/waves/h1/H1-LEDGER.md:11-13` still
asserts "the materialization-DEPTH is equal … the asymmetry is RETENTION", directly contradicted by
the corrective `FRAMING.md:16-21` and commit `a97f5f365`. The ledger is stale and overstated.)

## The depth-closure design

The closure is **one move**: give CSS the same depth-isomorphic tape JSON has, derived from the
value rules the grammar already carries, consumed by the same lazy cursor seam. No new substrate, no
second codepath.

### D1 — Value-component tape events (the structural fix)

Extend the CSS node kinds from 3 to a value-internal set the grammar's value rules induce:
`FunctionOpen` / `FunctionClose` (every `name(`), `ComponentBoundary` (top-level whitespace/comma
separators inside a value), and `ValueLeaf` (a typed terminal — dimension, number, hash-color,
keyword, named-color). Each is one tape offset, exactly as JSON's `ObjectOpen`/`ArrayOpen`. The
recognizer already walks the balanced run in `scan_value_end`/`css_balanced_component_scan`; the
change is to **push an offset at each boundary it already crosses** instead of skipping silently. The
NEON balanced-scan kernel is unchanged — it already finds `(`/`)`/`,`/depth; it currently discards
the positions. This is strictly additive emission on the existing sweep. Cost: one `push_offset` per
component value (an amortized append into the already-sized tape).

### D2 — Grammar-derived leaf typing (the typing fix, already built for Sheets)

The `ValueLeaf` payload is the **projected typed value** from the grammar's `->`, decoded by the
*existing* `classify_projection` machinery (`b7ab7c558`). `namedColor -> 0xF0F8FFFFu32` yields the
packed RGBA u32; `mathOperator -> Nu8` and `gradientKind -> Nu8` yield discriminants; dimensions
carry the unit. No new projection logic — CSS reuses the Sheets-proven derivation. This is what makes
the depth *typed*, not merely structural: the leaf tape cell carries the grammar's projected value,
not a re-scanned byte head.

### D3 — Lazy full-depth cursor (the navigation fix, already the JSON shape)

`CssDeclaration::value()` returns a `CssComponentValue` cursor over the value's tape sub-range, with
`.children()` descending `FunctionOpen`→`FunctionClose` (the CSS analogue of `JsonArray`'s child
iterator, `value.rs:143-150`). `rich_summary` walks the full component tree lazily, **without
retaining** it — the same defer-materialization-not-structure model JSON already proves. The 9 head
counters become full population counts (every nested color, every component dimension), and the
oracle's already-computed `push_token` stream becomes the parity reference (D4).

### D4 — Full-depth parity oracle (delete the head-level collapse)

`assert_rich_strict_equality` switches from `record_leading_typed_value` to the **full token sink** the
oracle already builds (`:549-566`). The equality becomes per-component-value population parity vs
cssparser's complete tokenization — which the reference engine *already performs* today. This costs
almost nothing on the reference side (the tokens are already emitted); it costs the tape projection a
full walk (D3).

## Can lazy-full-depth honestly beat eager CSSOM — and where it stops winning

**Yes, on summary/transform workloads; no, on full-tree-touch workloads.** The honest analysis by
workload class:

- **Summary / count / measured-metric workloads** (the `css_canon_bench` plane): lazy-full-depth
  still wins, because lightningcss *allocates and retains* every value node (`StyleSheet::parse`
  builds an owned `rules` tree, `css_canon_bench.rs:126-128`) while the lazy walk touches each
  component once and writes nothing to an arena. The win shrinks from the current ratios (2.10×–3.27×,
  `H1-LEDGER.md:21-26`) because the lazy side now does the **same number of classifications**
  lightningcss does — the speed advantage collapses from "shallower work + no-retain" to "no-retain
  alone." Expect the directional ratio to fall toward the retention-only differential (plausibly
  1.2×–1.8× on dense-value corpora like tailwind/material), and that is the **honest** number.

- **Full-tree-touch workloads** (serialize-back-out, minify, every-node-rewrite, query-every-value):
  laziness **stops being an advantage and may invert**. If the consumer touches every component value
  exactly once, the lazy re-projection from spans is pure overhead vs an eager tree already in cache,
  and the tape's offset indirection adds a layer the owned tree does not have. On these workloads the
  honest claim is **parity-or-slower**, and the path must disclose it. The lazy model wins precisely
  when the consumer touches **less than the whole tree**; its advantage is a function of touch-ratio,
  not of parser speed.

**The decisive honesty point:** the current head-level model wins partly because it does *less typed
work*. Closing depth removes that lever. The remaining lever — no retention — is real and defensible,
but the ratio must be re-measured and re-stated downward, on both summary and full-touch planes,
under loadavg < 1.0 (the `H1-LEDGER.md:27-32` directional figures are load-polluted at 6.79 and
cannot anchor an absolute).

## Honest cost estimate

- **D1 (tape events):** moderate. The recognizer sweep is unchanged; the cost is emission +
  larger tape (value components ≫ structural nodes — tape grows from O(rules+decls) to
  O(component-values), order-of-magnitude on value-dense corpora). Memory bandwidth on the tape is
  the new hot cost; capacity reserve (`structural_reserve`, `generated.rs:430`) must rescale from
  `byte_len/8` to value-density-aware.
- **D2 (leaf typing):** small. `classify_projection` exists; CSS adds projection tables (named-color
  u32, gradient/keyword u8) the grammar already declares. The 148-color match must lower to a
  perfect-hash/sorted-table lookup, not a linear `|`-chain, or it becomes the new hot loop.
- **D3 (lazy cursor):** small-to-moderate code; this is the JSON cursor shape transposed.
- **D4 (oracle):** near-zero — flip to the existing full token sink.
- **Net throughput:** the lazy side does ~N× more classifications (N = mean components/declaration,
  ~3–8 on real CSS). Throughput on the summary plane drops proportionally to the added classification
  work *minus* the retention savings lightningcss pays. **The >SOTA ratio will fall; it should still
  exceed 1.0 on summary workloads and approach/cross 1.0 on full-touch workloads.**

## The honest claim ladder

| Rung | Claim | Gate | Status |
|---|---|---|---|
| **R0 — NOW** | "`track1_rich` is a **lazy, head-level** typed projection (leading-token classification + comma-counted selectors); it beats lightningcss's eager full CSSOM on cold summary throughput **partly because it is shallower and retains nothing** — both asymmetries disclosed." | `FRAMING.md` disclosure + head-level 9-field oracle. | **Held**, but `H1-LEDGER.md:11-13` must be corrected (it still claims equal depth). Absolute Mbps forbidden under loadavg 6.79; ratios directional only. |
| **R1 — Depth-structural** | "CSS records full value-component structure on the tape (parity-isomorphic to JSON); the lazy cursor descends the full tree without retention." | D1+D3 land; `CssComponentValue.children()` navigates the full subtree; round-trip span reconstruction matches source. | Target. |
| **R2 — Depth-typed** | "Every component value carries its **grammar-projected typed value** (named-color RGBA u32, dimension+unit, gradient/keyword discriminants), derived from `->`, zero hardcoded rule names." | D2 lands; the named-color/gradient projection tables are grammar-derived (Sheets `classify_projection` reused); typed-materialization-invariant: every `->` reaches the leaf cell. | Target. |
| **R3 — Equal-depth parity** | "The lazy full-depth typed product equals cssparser's **complete** tokenization — full-tree population parity, not head agreement." | D4 lands; `assert_rich_strict_equality` uses the full token sink (`:549-566`), not `record_leading_typed_value`. | Gate for any "equal-depth" word. |
| **R4 — Equal-depth >SOTA, honest** | "On the symmetric cold corpus-in-timer plane, the lazy **equal-depth** typed product out-throughputs lightningcss's eager full CSSOM **by the retention differential alone** — ratio re-measured (lower than R0), with the full-touch-workload inversion disclosed." | R3 ∧ loadavg<1.0 capture ∧ both summary AND full-tree-touch planes reported ∧ ratio re-stated downward. | The genuine equal-depth >SOTA claim. **Only here may "beats CSSOM at equal depth" be uttered.** |

**Binding rule for the ladder:** the word "equal-depth" is forbidden until R3 (the parity gate); the
phrase "beats lightningcss at equal depth" is forbidden until R4 (re-measured, both planes, quiet
host). Until then the only honest claim is R0's lazy-head-level, with both asymmetries named. The
fold-in target is R4; the current campaign sits at R0 with a stale R0-overstatement in the H1 ledger
that must be retired in the same pass.

## Adversarial residue

1. **The grammar's depth is real but unconsumed.** `values.bbnf`/`color.bbnf`/`gradients.bbnf` carry
   the full typed tree; the Scan codegen flattens it. The fix is not new grammar — it is making the
   recognizer emit the boundaries it already crosses. Any "we lack the grammar features" excuse is
   false; the features (`->` projection, recursive value rules) exist and are Sheets-proven.
2. **The named-color projection is an overfit risk.** A 148-arm `|`-chain projecting to u32
   (`color.bbnf:36+`) is exactly the hardcoded-pattern-list the precepts forbid in spirit; it must
   lower to a derived perfect-hash, and it must not bleed grammar-specific literals into any generic
   crate (Lock 14).
3. **The ratio will fall and that is the point.** Resist the temptation to keep R0's 2–3× number after
   closing depth. The honest equal-depth ratio is smaller, and presenting the old ratio under the new
   claim would be the precise R-A0-1 violation `FRAMING.md` was written to prevent.
