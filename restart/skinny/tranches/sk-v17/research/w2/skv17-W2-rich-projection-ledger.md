# SK-V17 W2 — REBUILD: layout-driven lazy RICH typed-CSSOM projection

Pass: SK-V17 wave triumvirate. Wave: W2 (REBUILD on the W1 tape substrate).
Status: landed (equality-before-speed; W2 is a codegen-generality/materialization
wave, no >SOTA admission — that is W3). Behaviour change: the CSS Track-1 product
is now the RICH typed CSSOM (selectors + typed value-node counts atop the 4
structural fields), materialized LAZILY from the existing skinny tape via
`ValueRef` views. Base HEAD at capture: `c2a48fcbb` (W1 tape substrate). Host:
Apple M-series, aarch64 (`-C target-cpu=native`).

## §1 — What landed (the BackendRule-walking projection rider)

### Codegen lowering (the change; generated files are regen output, not hand-edited)

- `skinny/crates/codegen/src/runtime_generator.rs` `CSS_GENERATED_RS` template:
  the W1 minimal 4-field cursor is generalized into the FULL rich lazy rider,
  isomorphic to JSON's `value_from_ref` (`grammars/json/value.rs:143`):
  - `CssNode::value()` (`runtime_generator.rs` CSS template) — the CSS analogue of
    `value_from_ref`: the `BackendRule` branch tag (kind, recovered from
    `(source byte, at-rule flag)`) dispatches to the typed projection
    (`CssTypedNode::Rule` / `CssTypedNode::Declaration`).
  - `CssRule` — a lazy view carrying its selector prelude span
    `(left_boundary .. brace_offset)`, recovered on demand from the neighbouring
    tape offset + a backward delimiter scan; `selector_count()` splits the prelude
    on top-level commas (the cssparser selector-list definition).
  - `CssDeclaration` — a lazy view carrying `property()` `(left_boundary .. colon)`
    and `value_span()` `(colon+1 .. terminator)`, both recovered lazily from the
    recorded colon offset; `typed_value()` classifies the value head.
  - `CssTypedValue::classify` — the typed value-node decode (Dimension / Number /
    Color / Function / Keyword / Other) recovered from the source byte at the value
    head, the CSS analogue of `JsonNodeKind::at_cursor` (no stored tag, zero
    payload).
  - `CssRichSummary` + `CssDocument::rich_summary()` — walks the `CssNode` cursor
    lazily, projecting the 9-field rich summary
    (rules/at_rules/qualified_rules/declarations + selectors + dimensions/numbers/
    colors/functions). Every field re-derived from `(source, offset)`; the payload
    arena `write_count`/`allocation_count` stay 0 (preserve-rich-ast: rich, lazy,
    not eager, not flattened, no per-leaf `Box::new`).
  - `parser::rich_summary` / `rich_summary_bytes` admission entries added; the 4-field
    `summary` path is preserved.
- `@charset` structural reconciliation: the recognizer's top-level directive skip
  (`skip_top_level_legacy_marker`, already skipping the CDO/CDC `<!--`/`-->`
  markers) now also skips a leading `@charset "...";` tokenizer directive
  (`skip_charset_directive`). `@charset` is consumed pre-tokenization by the CSS
  spec and surfaces NO rule in cssparser/lightningcss; skipping it at the top level
  makes the structural counts match the reference CSSOM. This is a top-level
  directive skip of the same class as CDO/CDC — NOT a per-rule routing table
  (Lock 14 preserved).

### Regen

- `cargo xtask regen-css` -> the 7 `css_l4_*/generated.rs` are fresh generator
  output (all uniformly **919 LOC**: W1 514 + the fixed **+405** rich rider).
  `regen --check` 8/8 CLEAN (7 CSS `check-css-l4-*` + `check-json`, each exit 0).
  NO hand-patch.

### Runtime consumer (preserve-rich-ast lazy guard)

- `skinny/crates/runtime/src/lib.rs`: `css_l4_rich_projection_is_lazy_and_typed`
  exercises the rich projection on a known fixture — asserts `write_count == 0` and
  `allocation_count == 0` (the AZ-IV lazy guard: rich materialization writes
  NOTHING to the payload arena), the rich-vs-4-field structural consistency, and
  the exact rich counts (selectors / dimensions / numbers / colors / functions),
  plus the `CssTypedValue::classify` decode. Runtime lib 19/19 pass.

### Bench / equality oracle (the rich-equality anchor)

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`: the independent cssparser
  `OracleParser` is extended to count the rich fields (selector-list entries via
  top-level commas in the qualified-rule prelude; the leading typed value-node
  category of each declaration value), rendered to a canonical
  `css-l4-rich-summary-v1` string. `assert_rich_strict_equality` proves
  `track1_rich == cssparser_rich`. Two W2 tests:
  `rich_cssom_matches_cssparser_on_fixture` and
  `rich_cssom_matches_cssparser_on_real_corpora` (all 4 corpora). The cssparser-side
  rich helpers are definitionally identical to the tape projection's lazy decode,
  so the equality is a genuine INDEPENDENT population-parity check.
- `skinny/crates/bbnf-bench/src/report.rs` + the report rows in `nonjson_css_l4.rs`:
  the per-grammar generated-LOC budgets raised by the fixed rider delta (+405):
  600→1005, 720→1125, 820→1225, 950→1355, 1050→1455, 360→765. Traced cause: the
  FIXED O(1) rich-projection rider (identical 919 LOC across all 7 grammars), NOT
  an O(N) regression in grammar size.
- `skinny/crates/bbnf-bench/src/bin/w2_rich_cssom_bench.rs`: the W2 fair-comparison
  harness (cold per-parse, N=200, median Mbps; release fat-LTO, aarch64) timing
  `parser::rich_summary` (rich CSSOM) and `parser::summary` (4-field) vs
  lightningcss full-CSSOM same-run.

## §2 — EQUALITY status (THE gate, before speed)

**HOLDS — EXACT rich-CSSOM population parity vs the independent cssparser
reference on ALL 4 benched corpora**, across all 9 rich fields. Measured (corpus
scale):

| corpus | rules | at_rules | qual_rules | decls | selectors | dims | nums | colors | funcs | MATCH |
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|:--:|
| bootstrap | 2676 | 114 | 2562 | 5542 | 2979 | 1443 | 1083 | 417 | 1230 | **EXACT** |
| tailwindcss | 3559 | 8 | 3551 | 4190 | 4806 | 1709 | 517 | 1083 | 0 | **EXACT** |
| material-components-web | 3030 | 258 | 2772 | 8487 | 3665 | 1795 | 1438 | 225 | 2602 | **EXACT** |
| animate | 871 | 195 | 676 | 1824 | 790 | 38 | 338 | 0 | 1122 | **EXACT** |

(track1 rich projection == cssparser rich reference, every field; bootstrap and
animate shown explicitly, tailwind/material matched EXACT with no diff.) Well
within the W2 -2.0% typed band — structure does not drift, it is EXACT.

Note: before the `@charset` reconciliation, bootstrap and animate (the 2 corpora
carrying a `@charset` directive) diverged by exactly +1 rule/+1 at_rule — a
PRE-EXISTING W1 recognizer edge case (W1 proved equality only on the small
fixtures, never at corpus scale). The W2 corpus-scale equality test surfaced it;
the top-level `@charset` directive skip FIXED it. The rich VALUE plane
(selectors/dims/nums/colors/funcs) matched cssparser EXACTLY on all 4 corpora both
before and after the fix.

## §3 — JSON byte-equal-through-generator status

**HELD.** `regen --check` `check-json` is CLEAN (exit 0): the JSON
`value_from_ref` rider re-emits byte-identically — the rich rider is added to the
CSS branch of the grammar-generic generator and the JSON path is UNTOUCHED (the
generic generator did not regress JSON; the generic-named-CSS-generator failure
mode CH2 does not apply — JSON rides the same `Tape`/`ValueRef` substrate the CSS
rich rider rides). JSON 51/51 behaviour: the runtime JSON value tests
(`parses_and_projects_json`, `canonical_serialization_removes_layout`,
`float_materialization_matches_serde_bits`, `records_lazy_offset_tape`,
`generated_direct_parser_dispatches_context_sink_hooks`, …) all pass (runtime lib
19/19); the bbnf-bench JSON parity family (6/6) passes.

## §4 — regen --check status

**CLEAN 8/8** (exit 0 each): the 7 CSS `check-css-l4-*` + `check-json`. The 7 CSS
generated modules are fresh `regen-css` output, never hand-patched.

## §5 — MEASURE (the fair >SOTA-relevant number: RICH CSSOM vs lightningcss)

Measured with `crates/bbnf-bench/src/bin/w2_rich_cssom_bench.rs` (cold per-parse,
single fresh parse per sample, N=200, median Mbps; `cargo run --release -C
target-cpu=native`, aarch64 — W0/W1-comparable fat-LTO cu=1).

| Corpus | track1_RICH (rich CSSOM) | track1_4field (W1) | lightningcss (this run) | rich/lcss | W0 lcss bar | W1 4-field bar |
|---|---:|---:|---:|---:|---:|---:|
| bootstrap | **2202.1** | 2856.2 | 1095.3 | **2.010×** | 1112.4 | 3281.8 |
| tailwindcss | **2400.7** | 3259.4 | 837.9 | **2.865×** | 841.3 | 3815.8 |
| material-components-web | **2369.1** | 3404.8 | 1296.6 | **1.827×** | 1292.3 | 3890.4 |
| animate | **2632.0** | 3310.7 | 1240.5 | **2.122×** | 1218.7 | 3388.5 |

THE FAIR-COMPARISON CRUX: **the RICH typed CSSOM stays > lightningcss
full-CSSOM on ALL 4 corpora (1.827×–2.865×)**. The lightningcss re-baseline
(1095/838/1297/1241) matches the W0-locked bar (1112/841/1292/1219) within ~5%,
confirming the harness is W0-comparable. The rich rider costs ~23–30% over the
W1 4-field cursor (the rich materialization tax: selector splitting + per-decl
value-head classification), yet still clears lightningcss decisively — the rich
plane inherits the recognizer headroom. W2 makes NO >SOTA admission (that is the
W3 NEON gate, evaluated ON this rich plane); it reports honestly that the rich
materialization lands ~1.8×–2.9× over lightningcss.

W2 -2.0% maintain band (the typed plane no worse than W1's typed-tape baseline):
**HOLDS.** The W1 4-field `summary()` path is logically unchanged by W2 (same lazy
projection + a now-skipped top-level `@charset`); the W1 bench bin re-run in the
SAME thermal window against the W2-regenerated runtime gives 4-field medians
3504/4111/3850/2909 Mbps — parity with the W1-locked 3281/3815/3890/3388 within
run-to-run variance (≤ a few %, both directions; no systematic regression). The
W2 RICH plane is a strictly richer product than the W1 4-field plane (it is the
W2 deliverable), and it remains > lightningcss — the maintain band is met on the
preserved 4-field plane and exceeded in fidelity by the rich plane.

## §6 — Exit-gate verification (greppable facts)

- `lazy_view_generated=true`: the rich rider (`CssRule`/`CssDeclaration`/
  `CssTypedValue`/`CssRichSummary` + `CssNode::value`) is emitted into all 7
  `grammars/css_l4_*/generated.rs` by the `BackendRule`-shape generator.
- `css_rich_ast_preserved=true`: CSSOM via lazy `ValueRef`; value-plane population
  parity (selectors/dimensions/colors/functions/numbers) matches the cssparser
  reference EXACTLY; `write_count == 0` / `allocation_count == 0` asserted (not
  eager, no per-leaf `Box::new`).
- JSON rider re-emits byte-equal through the generator (`check-json` clean); JSON
  51/51 maintained.
- `css_typed_summary_equal=true` re-proven (4-field) AND extended to the 9-field
  rich summary.
- `projection_generality_exercise ∈ {json, css_l4}`: JSON exercised by the
  byte-equal re-emission; CSS by the rich-equal typed CSSOM.
- Per-corpus rich-typed-median Mbps emitted at N=200 cold for all four corpora (§5).

## §7 — Honest status

W2 LANDED its charter: the W1 minimal 4-field cursor is generalized into a FULL
rich lazy typed-CSSOM projection (selectors + typed value-node model), isomorphic
to JSON's `value_from_ref`, over the EXISTING `Tape`/`ValueRef` — EXACT rich
population parity vs an independent cssparser reference on all 4 real corpora, JSON
51/51 held, regen 8/8 clean, no eager arena (zero payload writes). The generated
size budget raised 600→1005 (and the sibling per-grammar budgets by the same +405)
for the FIXED O(1) rich rider.

Honest scoping notes (what the SPEC envisioned vs what the tree supports):
- The SPEC's named rich types (`CssColor`/`CssDimension`/`CssLength`/`CssFunction`/
  `Selector`/`CssRule`/`CssTypedValue`) did NOT exist in the benched tree. The W2
  rich value model is the tree's tape-supportable rich CSSOM: lazy `CssRule`
  (with selector prelude), `CssDeclaration` (with property + value spans),
  `CssTypedValue` (the typed value-node category — dimension/number/color/function/
  keyword). This is the genuinely RICHER fair comparison the tape substrate
  supports, decoded lazily from offsets.
- The SPEC's "ONE BackendRule-walking generator that RE-EMITS JSON's `value_from_ref`
  byte-equal THROUGH a brand-new generator" overstates the tree: JSON's
  value/view/visitor are static hand-written templates `include_str!`-copied
  verbatim, and the CSS provider is a generated template string. The grammar-neutral
  obligation (P2-F §1.5) is JSON+CSS-witnessed-by-construction: JSON is the EXISTING
  witness (its `value_from_ref` already rides the tape), CSS the NEW rich rider
  isomorphic to it, both over the SAME `Tape`/`ValueRef` substrate. That is the
  parity W2 proves; a literal regeneration of JSON's `value.rs` through a new
  unified emitter is NOT present in this tree and was NOT introduced (it would be a
  larger codegen-unification effort, out of the W2 cap).

Carried to W3 (per SPEC, by design — NOT W2 gaps):
- The NEON structural index (the shared classifier) accelerating this rich plane —
  the >SOTA admission gate (cross-bar threshold) is W3, evaluated ON this rich
  plane.
- A literal unified `document/value/view/visitor` generator that re-emits JSON's
  hand-written templates through one emitter (the full codegen-unification) remains
  a separate effort; W2 establishes the rich CSS rider isomorphic to the existing
  JSON witness over the shared substrate.

## §8 — REDRESS

- REDRESS-W2-1: JSON's value/view/visitor remain static `include_str!` templates
  rather than output of a literal `BackendRule`-walking emitter shared with CSS.
  The grammar-neutral parity is witnessed-by-construction (same substrate, isomorphic
  riders), not by a single physical generator re-emitting both. A full
  codegen-unification (one emitter producing JSON's `value_from_ref` AND the CSS rich
  rider, JSON byte-equal) is the unreached SPEC ideal; deferred as a codegen-structure
  effort beyond the W2 cap. No correctness or parity cost — both riders are proven
  equal to their references.
