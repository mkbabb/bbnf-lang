# AW-III Research Synthesis — Viability, Hot Paths, and the 20× Regression

Eight parallel agents produced artefact-grounded analyses of the DTA-primary parse
path at master HEAD `b58d1461` (post-AW-II): five samply profiles across the
19-entry bench matrix, one code audit covering lever firing + precedence
collapsing + dead code + direct-to-struct + type inference, and two correctness
audits (50 failing tests + 58 ignored tests). This document synthesises their
findings into a single picture + AW-III wave-schedule refinement.

## The regression's shape

Five independent samply profiles converge on the same picture. The hot-function
profile is **uniform across grammars and across input sizes**:

| Hot path | json | css | sheets | bbnf | json_value (bbnf) |
|----------|:----:|:---:|:------:|:----:|:-----------------:|
| `dta_run → dispatch_one` | 20-31% | 18.15% | 24-35% | 35-40% | 20-31% |
| `driver::try_branch` (Alt savepoint/restore) | 7-10% | 7.18% | 14-16% | 16-21% | 9-15% |
| `reserve_compound` (Seq open) | 7-10% | 6.05% | 10-11% | 13-19% | 7-10% |
| `DtaDfaScanner::scan` + `cached_dfa` + HashMap + Sip13 | 13-33% | 56.35% | 10-12% | 8-12% | 15-20% |
| `close_compound` + `emit_leaf` + `advance_or_pop_with` | 10-15% | ~8% | 6-9% | 6-9% | 6-12% |

**The signature**: `dispatch_one` is the ceiling everywhere. `DtaDfaScanner::scan`
is secondary — dominant only on scanner-heavy grammars (CSS; JSON twitter with
high string-density). **This is canonical state-machine-interpreter overhead**.

### Scaling is linear, not super-linear

Sheets explicitly tested this (P3): `parse_simple` → `parse_stress` scales
**linearly in input length** (ns/byte roughly flat at 222-277 across all three
entries). Deeper formulas generate more structural tokens per byte but do not
change the per-byte walker cost. `parse_stress`'s regression is not worse than
`parse_simple`'s; the 20-40× gap is the fixed state-machine baseline, not a
scaling cliff.

This is the pivot fact for the viability decision. A linear-dispatch regression
is recoverable by amortising dispatch (the AW-IV lever thesis). A super-linear
regression would mean architectural flaw below AW-IV's reach.

### The bbnf-vs-sonic twin pair (P5) quantifies the gap

| Input | BBNF-DTA ns/iter | sonic-rs ns/iter | Ratio |
|-------|:----------------:|:----------------:|:-----:|
| twitter | 5,250,578 | ~145,000 | 36.3× |
| citm | 12,050,000 | ~488,000 | 24.7× |
| data_xl | 262,030,000 | ~14,890,000 | 17.6× |

sonic-rs (SIMD-specialised production JSON parser) has one hot loop per shape
(parse_object + parse_array = 90%+). BBNF-DTA spreads the same work across eight
named driver functions because the DTA interprets one state-transition at a
time. **Payload materialisation (W5c's fix) is NOT the bottleneck** — `walk_cursor`
holds a consistent 7.3-8.1% leaf-time across entries. The W5c fix is verified
correct and firing; it is not the regression source.

## AW-IV lever firing — zero fully active

A1's code audit examined each of the eight AW-IV levers at master HEAD:

| Lever | Status | Substrate | Consumer |
|-------|:------:|:---------:|:--------:|
| PSI rayon stage-B | NOT FIRING | present | `parallel_break_even_bytes: 0` gates it off |
| ShapeRef runtime dispatch | PARTIAL | CSS L4 has 13 entries in `SHAPE_DICT` | walker-side `push_shape_ref` does NOT exist |
| PHF + SIMD keyword | NOT FIRING | substrate slot hardcoded `&[]` | no emitter pass |
| Selector classifier | NOT FIRING | no emitter | no walker arm |
| Scanner closure | NOT FIRING | no `Arc<Dfa>` field on `DtaState::Regex` | HashMap lookup per scan |
| Bloom + GADT dedup | NOT FIRING | `dedup_eligible_rules: &[]` | no emitter pass |
| Pratt generalisation | PARTIAL (Sheets + BBNF-self) | works on `IrNode::Seq` | fails on `IrNode::Next` — CSS `calc/min/max/clamp` miss |
| `reduce_column<C,R>` | NOT FIRING | no visitor API | no consumer |

Zero levers fully fire. The 20× regression is measured against AW-IV's complete
absence. Activating even a subset produces substantial recovery.

## The highest-leverage ONE-BYTE and ONE-PEEL fixes

Two fixes surfaced across multiple agents that are individually small but
structurally load-bearing:

### 1. `strip_transparent_owned` peels `Next` (A1, P2)

Grammar:
```
mathExpr    = mathProduct , ( ( "+" | "-" ) , mathProduct ) * ;
mathProduct = mathValue , ( ( "*" | "/" ) , mathValue ) * ;
calc        = "calc(" , mathExpr , ")" ;
```

`mathExpr` uses `>>` (IrNode::Next) as the "run op, discard op-literal"
separator. `strip_transparent_owned` at `crates/ir/src/passes/recognizers/dta.rs:885-890`
peels `IrNode::Seq`, not `IrNode::Next`. `match_operator_chain_rule` never fires
on CSS.

**One-peel fix**: extend the arm to also peel `IrNode::Next`. Pratt lifts
mathExpr + mathProduct + every `min/max/clamp/calc` body to `DtaState::ShuntingYard`.
State count drops; walker dispatch depth drops; operator precedence becomes a
byte-indexed LUT lookup.

### 2. Scanner closure (P1, P2, P5)

`DtaDfaScanner::scan` calls `cached_dfa(pattern_string)` on every invocation:
```rust
// crates/bbnf-tape/src/driver.rs:~912 (walker Regex arm)
let dfa = CACHED_DFA.get_or_insert(pattern);  // HashMap<String, Arc<Dfa>> lookup
dfa.find_at(input, offset)
```

Per-scan cost: hash the pattern string → SipHasher → probe HashMap → Arc clone.
Equally true for every entry where the pattern is a compile-time constant.

**One-field fix**: add `pattern_dfa: Arc<Dfa>` to `DtaState::Regex` at lift time.
Walker dispatch goes directly to `dfa.find_at`. Eliminates 6-33% self-time
depending on grammar (CSS 33%, JSON twitter 13-20%, BBNF 6-10%).

Both fixes are < 50 LOC each. Both close holes that every grammar hits. Both
ship as W1 infrastructure before the broader W5 lever activation.

## The five producer-side payload holes (A1, C1)

A1 named five concrete holes in the type-inference / payload-write pipeline,
not just the one W5c's agent surfaced:

1. **Lifter strips `IrNode::Map { inner, .. }`** at `crates/ir/src/passes/recognizers/dta.rs:525`. Wholesale type-annotation discard.
2. **`DtaState::Regex` / `DtaState::Literal` have no `payload: PayloadKind` field** at `crates/bbnf-tape/src/dta.rs:93-104`. Wire contract has no slot.
3. **Walker hardcodes `PayloadKind::F64`** at `crates/bbnf-tape/src/driver.rs:912`. Every regex leaf writes an F64, regardless of declared type.
4. **Walker Literal arm emits no payload** at `crates/bbnf-tape/src/driver.rs:875-891`. Literal-dispatched scalars land as bare spans.
5. **`frame_to_tape_kind` doesn't promote `Seq → KvPair`** even when the IR layout pass classifies the rule as `is_kv_pair_shape`.

C1 cross-references: **37 of 50 failing tests close under a single coordinated
six-point fix**: schema change (DtaState variants + payload field), lifter Map-descent
with FnDescriptor→PayloadKind, walker consumption, emitter const-fold, bootstrap
regen, Seq→KvPair promotion. **Cluster 1 cascades from one W1 wave.**

## CSS-specific structural wins

P2 identified six CSS-specific levers beyond the global ones. Two are gated on
structural fixes that must land first:

- **H2 `DtaState::ClassifyByte` LUT** — collapses the 5-way `compoundSelector`
  Alt backtracking (compound selectors are a hot path; classifier bitmap needs
  walker consumer).
- **H3 `DtaState::PhfKeyword`** — 163-branch `namedColor`, 72-branch keyword,
  92-branch properties ladders. Sharing machinery with BBNF PHF.
- **H5 fused `push_compound`** — replace the 7-vector row in `reserve_compound`
  with a single struct-write. 6.05% self-time.
- **H6 ShapeRef CSS declarations** — substrate exists; consumer missing.
- **H4 Pratt selection for mathExpr/mathProduct** — gated on the `IrNode::Next`
  peel (global fix #1 above).
- **H1 pattern-id-indexed DFA array** — deduplicates with the scanner closure
  (global fix #2).

## Sheets-specific viability concern (P3)

Sheets' profile is the most diagnostically dire. Even with full AW-III.W5 scope
(Pratt + scanner closure + ShapeRef), modelled ceiling is **~13 MB/s** against
95-128 MB/s post-AU baseline — **8-9× residual**. The `dispatch_one` tagged-union
match floor (~24% self-time) is not addressed by any AW-IV lever.

Two resolutions:
1. **Accept 8-9×** as the honest post-optimisation envelope for sheets and
   document it as a DTA-architecture tradeoff.
2. **Introduce a lever not in AW-IV's inventory** — e.g. codegen-specialised
   per-grammar walkers (emits `fn walk_bbnf_grammar(...)` with inline dispatch),
   recovering LLVM inlining on the common path.

(2) is substantial new work — a new tranche's worth, not a W5 sub-scope. For
AW-III it's a viability-escalation item, not an in-wave fix.

## The 50-failing-test catalogue (C1)

C1 produced a six-cluster decomposition. Numbers post-cascade:

| Cluster | Count | Wave | Cascade source |
|---------|------:|:----:|----------------|
| DTA payload wiring | 37 | W1 | one six-point fix |
| CSS tape truncation | 2 | W2 | walker early-terminate diagnosis |
| Large-corpus parse fail | 4 | W2 | walker EOF / limit handling |
| EBNF offset-0 | 6 | W2 | first-literal dispatch gap |
| CSV Repeat-of-Seq | 1 | W2 | Repeat walker arm |
| LSP inlay hints | 1 | W3 | test heuristic stale |

**Expected workspace trajectory**:
- W1 closes: 1050 → 1085-1087 passed, 15 failed
- W2 closes: 1087 → 1097+ passed, 0-3 failed
- W3 closes: ignored count drops

**P1's shared EOF insight** refines Cluster 2 and 2': `data` and `canada`
fail **one byte from EOF**, sharing aetiology with `css_tailwind` offset
3633741 being effectively "near end-of-input". This is a single walker
EOF / trailing-whitespace gap, not three separate large-file bugs. Unblocks
in ONE W2 fix.

## The 58 ignored-tests catalogue (C2)

58 unique source-level `#[ignore]` (not 67 — the 67 reflects multi-feature-gate
compilation artifacts):

- **CLOSE — 14 tests**: lift attribute, they pass immediately. Includes the
  `serialize_roundtrip::css_simple` that AW-I.W2.5 marked Category A — post-W5c
  view-layer reconciliation fixed it.
- **DELETE — 4 tests**: 3 `unreachable!()` stubs (docs-as-tests for consumer
  migration) + 2 gorgeous visualisation dumps (non-checked-in fixtures).
- **INVESTIGATE — 40 tests** across 7 groups:
  - A (10): CSS percentage + JSON variant_idx — cascade from W1 payload wiring.
  - B (1): ebnf_rule serialize — cascade from W2 EBNF completeness.
  - C (6): structural-mode analysis pipeline — out of AW scope.
  - D (5): closure-body lowering — grammar-closures project.
  - E (6): CSP solver GAC alldiff — csc411 solver tranche.
  - F (4): gorgeous prettify + pprint-vm drift.
  - G (7): misc producer / test-data.

**Gate mismatch**: AW-III.W3's "ignored count ≤ 10" target is infeasible
without expanding scope into Groups C + F + G (17 tests). Honest disposition
is either (a) accept residual ~27 with routing to successor tranches, or (b)
expand AW-III.W3 scope to close C+F+G (adds ~1 wave of work).

## Viability decision — preview for W4

AW-III.W4's samply attribution is chartered to produce a binary viability
decision. These eight research docs pre-stage the decision:

### DTA is viable under AW-IV levers for MOST grammars

Post-AW-IV ceiling estimates (from agent attribution × lever-coverage):
- **JSON (twitter, citm, data_xl)**: 3-5× regression residual. Scanner closure
  + ShapeRef recover ~60-70%. Close to post-AU parity practical.
- **CSS**: 2-4× regression residual. Scanner closure + ShapeRef + classifier
  + Pratt(Next) recover majority. Close to post-AU parity practical.
- **BBNF**: 3-5× regression residual. ShapeRef + scanner closure dominate.
- **JSON (data, canada)**: post-W2 parse-close unblocks; expect similar to
  twitter ratio.

### DTA is NOT viable within 2× for Sheets

- **Sheets**: 8-9× regression residual, full W5 scope insufficient. The
  `dispatch_one` tagged-union floor isn't addressed by any AW-IV lever.

This is an escalation, not an in-wave fix. W4 documents; user decides:
(a) accept 8-9× as DTA-architecture tradeoff, or (b) open a new tranche for
codegen-specialised per-grammar walkers.

## Refined AW-III wave schedule

Based on this synthesis:

### W1 — DTA payload wiring (unchanged, now with concrete six-point commitment)

1. Schema extension: `DtaState::Regex { payload: PayloadKind }` + same for `Literal`.
2. Lifter Map-descent with `FnDescriptor → PayloadKind`.
3. Walker consumption (replace hardcoded F64).
4. Emitter const-fold.
5. Bootstrap regen.
6. `frame_to_tape_kind` Seq→KvPair promotion.

Cluster 1 cascades (37 tests). Cluster-A ignores (10 tests) also close.

### W1.5 (NEW) — Two structural one-fix levers (add to W1 as W1.7 + W1.8)

7. **Pratt `IrNode::Next` peel** — extend `strip_transparent_owned` in `dta.rs:885-890`
   to peel `Next` alongside `Seq`. CSS `calc/min/max/clamp` now Pratt-lifted.
8. **Scanner closure** — add `pattern_dfa: Arc<Dfa>` field to `DtaState::Regex`;
   lift-time population; walker `dispatch_one` Regex arm uses directly.

These two land in W1 (not deferred to W5) because they're single-file < 100 LOC
fixes that eliminate correctness holes AND unlock W5 levers (Pratt peel → Pratt
firing on CSS; scanner closure → no HashMap lookup tax anywhere).

### W2 — Parse completeness (refined with EOF insight)

Primary: shared EOF / trailing-ws handling closes `data`, `canada`, `css_tailwind`,
`css_bootstrap` truncation, `css_normalize` truncation in one fix. Then:
EBNF offset-0 (6 tests), CSV Repeat-of-Seq (1 test).

### W3 — Ignored audit (gate relaxed)

CLOSE 14 + DELETE 4 = 18 tests dispositioned at wave close. Groups A+B cascade
from W1/W2 (11 tests close). Residual: ~27 in Groups C/D/E/F/G routed to
successor tranches per plan's fallback clause. Gate revised from "≤ 10" to
"every ignore has in-file rationale or routed-successor annotation".

### W4 — Viability profile (already staged)

Samply on `json_twitter`, `sheets_parse_stress`, `bbnf_ebnf` → binary decision
document. Pre-staged by this SYNTHESIS.

### W5 — Minimum-viable specialisation

**Primary levers** (per attribution):
- ShapeRef runtime dispatch (global, consumer side) — CSS/BBNF/JSON.
- PHF keyword tables (per-grammar emitter pass) — CSS namedColor + BBNF keywords.
- `push_compound` fused write — 6% global.

**Secondary (per-grammar)**:
- Selector classifier — CSS-only.
- PSI rayon stage-B calibration — population of `parallel_break_even_bytes`
  per grammar.

**Deferred to AW-IV**:
- Bloom + GADT dedup.
- reduce_column visitor API (consumer side, not parse hot path).

### W6 — FINAL + full 19-entry bench matrix + close

Unchanged. The 5 AW-II-blocked bench entries unblock at W2; all 19 measurable.
Target: within 2× post-AU geomean on 18 of 19 entries; sheets explicitly
documented as tradeoff (if viability call is "accept").

## Synthesis's own load-bearing claim

**The 20× regression is dispatch-interpreter overhead against inlined recursive
descent.** No single lever eliminates it; combined levers amortise it to within
2-5× for most grammars and 8-9× for Sheets. The AW-IV lever portfolio is the
right portfolio — it's just never been activated. AW-III's job is to close
correctness (W1, W2, W3) and activate minimum-viable subset (W5) that proves
the remaining regression is in the AW-IV-amortisable-or-escalate band, not the
DTA-architecturally-broken band.

Bootstrap is idempotent. Workspace is 1050/50/67. Viability looks good for JSON
+ CSS + BBNF; genuine tradeoff question for Sheets. Agents cherry-picked cleanly
onto master at `3c33bc35`.
