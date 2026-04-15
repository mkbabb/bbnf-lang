# Tranche AU — FINAL

Tranche: projection activation, regression redress, scanner truth.
Landed across seven waves between 2026-04-14 and 2026-04-15. W7 is
this document plus `post-AU.json`; no code changes in this wave.

Ten hard gates fully met; two met with a documented qualifier; five
partial; five missed; one deferred; one not applicable. The full tally
with artefact citations and forward routing is in §2 below. Two
systemic codegen bugs (typed-parity audit Bug 1 and Bug 2) block the
strict reading of "every `->` reaches the tape"; both are routed to
AV with a surgical fix sketch. The larger performance gates
(canada 1800 MB/s, bootstrap 600 MB/s, parse_simple 200 MB/s) are
also AV-scale: Eisel-Lemire + simdjson SIMD decode, PHF + SIMD
selector classifier, Pratt precedence-tower lowering respectively.

Default `cargo test --workspace` exits 101 on the single pre-existing
imports failure (matching the tranche-plan expectation). Under
`--no-fail-fast` the real picture is 967 pass / 33 fail / 30 ignored;
the additional 32 failures were hidden by cargo's stop-at-first
behaviour. Of those: 4 are CSS tape_parity goldens that W6.D's regen
commit missed, 5 are sheets_parity Bug-1 pinned assertions that
flipped from PASS to FAIL once W6.D's scalar-bypass landed, and 23
are the pre-existing Session-1 '18 tests fail' family (closures,
debug, analysis, graph, gorgeous, lsp, lower, recover). Details in
§2 gate 17 and §4.

## 1. Phase-by-phase recapitulation

### Phase 1 — Projection activation

Status: **COMPLETE**.

`branch_pushes_children` returned `true` for `Ref` nodes pointing to
rules the driver inlines via `CallStrategy::InlineBody`, forcing
`mark_children + push_compound` on every Alt branch. All JSON
`value` typed payload captures were dead stores.

- **AU.1.1** — `branch_pushes_children` takes `DriverState`, checks
  `CallStrategy::InlineBody`, recurses into `Alt`/`Skip`/`Next`/
  `Minus`. Commit `83357e4`.
- **AU.1.1 addendum** — Seq-of-leaves classification. Commit
  `f7be09e`.
- **AU.1.1 canada fix** — `payload_idx: u16` overflowed at 65535;
  canada.json has 111K f64 payloads. Byte offset stored in
  `child_off` (u32 range); sentinel for "payload present" is
  `payload_idx: 1`. Commit `83357e4` (subsumed later by W5 unified
  arena).
- **AU.1.2** — Payload correctness test landed; `grammar_roundtrip`
  6/6, `payload_layouts` 13/13.
- **AU.1.3** — Payload `Vec::with_capacity` tuned via per-grammar
  fingerprint divisor (W3.B); subsumed by unified arena in W5.
- **AU.1.4** — Bench miss. Canada dropped from AQ 1796 to post-AU
  1231 MB/s. The regression is the cost of storing 111K × 8 bytes
  of f64 payload writes on a previously discarded-then-recomputed
  path; PROGRESS Session 1 flagged this as uninvestigated and routed
  profiling to AU.5 (W2) where samply data confirmed the parse-stage
  bottleneck lives in Eisel-Lemire + scanner dispatch (AV-scale
  remediation).

### Phase 2 — CSS typed-AST parity with lightningcss

Status: **PARTIAL** (AU.2.0, AU.2.3, AU.2.4, AU.2.5 landed; AU.2.6
colour-function aggregates did not ship; AU.2.7 structural bitmap
landed architecturally but did not hit its perf gate).

- **AU.2.0** — Grammar-wide annotation audit across the 15
  `grammar/css/l4/*.bbnf` files. W1 CSS agent cherry-picked 13
  commits. `CSS_L4_RULE_COUNT` advanced 185 → 190 → 195.
- **AU.2.1** — Tuned; the `scan_ws_block_comments_slow` byte-wise
  tail deleted via AU.2.7's structural bitmap subsumption. Commit
  `143d19e`.
- **AU.2.2** — PROGRESS Session 1 established the original audit
  claim was incorrect. 7 of 8 `scan_ident` call sites correctly
  use `DEFAULT_IDENT_CONFIG` because the CSS `ident` regex does
  not admit a leading dash; only `selectorIdent` needs
  `CSS_IDENT_CONFIG` and it receives it. No code change.
- **AU.2.3** — `number -> f64` held back post-W3.E because
  `scan_number_strict_f64` rejected leading-dot literals and
  tailwind panicked. W6.B restored `number -> f64` after the
  emitter dispatch fix (commit `240535b`) selects
  `scan_number_f64` (generic) vs `scan_number_strict_f64` (JSON)
  by `RegexClass::Numeric { allow_leading_dot }`. Commit
  `63f86a5`.
- **AU.2.4** — `parse_hex_color` routed through the tape-first
  emitter. `emit_hex_convert` now fires `push_leaf_with_aggregate`
  at 20+ sites (post-W2). Commit `2bd75a3`.
- **AU.2.5** — Typed dimension aggregates. AU.2.5 activation
  required `IrNode::Ref(_)` to project the target rule's scalar
  via `RefConstraint`; W3.F landed that (`558a457`) plus
  `join_types` reconciliation of `Tuple([Span, T])` with bare-T
  under factor-pass (`05e38dc`). `push_leaf_with_aggregate` sites
  grew 2 → 20 → 27. `valueUnit` wired through `dimension` via
  `eded24f`.
- **AU.2.6** — `colorFunction`, `colorFn`, `colorMix` aggregate
  payloads (u8 space + f64×3 + f64 alpha, with arena-backed
  `ColorRef` for recursion) did not ship. The grammar annotations
  exist; the emitter's aggregate-size widening to ≥ 33 bytes on
  top of the arena was not extended, so these rules continue to
  emit `push_compound` with no payload. Routes to AV.
- **AU.2.7** — Structural bitmap v2. `emit_structural_bitmap_kernel`
  subsumes `memchr1`/`memchr2`/`memchr3` + `nibble_lut`; SIMD
  `filter_quote_parity` via clmul/pmull (parse-that `b8d2…` and
  master commit `143d19e`). Hard architectural gate met
  (`grep -c 'scan_ws_block_comments_slow\|memchr1\|memchr2\|
  memchr3\|nibble_lut'` returns 0). Perf gate **not** met: CSS
  bootstrap landed at 411 MB/s post-W3.C, 438 MB/s post-W6.D,
  454 MB/s at W7 close-out — below the 650 MB/s gate. The
  structural bitmap kernel's byte-distribution on bootstrap
  appears slower than the deleted nibble_lut for some call sites;
  residual hotspots are `__declaration` + `__compoundSelector`,
  AV-scale (PHF + SIMD selector classifier).

### Phase 3 — String decode + honest JSON bench

Status: **PARTIAL** (AU.3.1 landed; AU.3.2 hard gate ratio missed).

- **AU.3.1** — `decode_json_string_to_arena` routed through
  `scanner_plan.rs` as `SharedScanner::JsonStringDecode`. New
  `push_leaf_with_string` on `TapeBuilder` (W2.D, later collapsed
  into `push_leaf_with(PayloadData::Bytes)` under W5's unified
  arena). Commits `4c273b0`, `ceab7e8`. `decode_json_string_to_arena`
  fires 3× in expand.
- **AU.3.2** — `json_monolithic_value` extended; bbnf side walks
  the tape (commit `de51346`). W3.D zero-alloc `ChildIter` landed
  (`39aa0d0`). W6.D borrow-safe JSON strings skip arena copy
  (`bf8f16a`) + single-scalar rules bypass aggregate planner
  (`34f049a`). Post-W6.D ratios: canada 0.61 (target 0.80),
  twitter 0.52 (target 0.60). **Gate missed**. Residual is
  parse-stage (Eisel-Lemire bridge, simdjson-scale SIMD decode),
  AV substrate work.

### Phase 4 — Accumulated debt elimination

Status: **COMPLETE on major items**, one pre-existing failure
carried over.

- **AU.4.1** — `ParsedGrammar` deleted. 11-tranche deferral
  resolved. `GrammarSink` trait + two sinks (`PipelineSink`,
  `ExtractSink`) replace the flat-Vec intermediate. Net +201 LOC
  because external read-only crates (bbnf-analysis, gorgeous,
  bbnf-lsp, bbnf-bootstrap) require a flat return shape — factored
  as `GrammarExtract` via `ExtractSink`. Commit `688d6ea`.
- **AU.4.2** — `StructRegistry` deleted (dead scaffold). Commit
  `ab8588a`.
- **AU.4.3** — Module-level `#![allow]` on `generated.rs`;
  `generated.rs` 25513 → 25228 lines. Commit `28f5023`.
- **AU.4.4** — Schema stub emitters deleted early in W1. Commit
  `ef090a5`.
- **AU.4.5** — Bootstrap regen pass; idempotent across three
  successive clean-cache regens. Commits `6e33a40` (W1),
  `c9a4fc7` (W2), `5b2a48e` (W3). `generated.rs` matches fresh
  regen at every wave boundary.
- **AU.4.6** — Pre-existing debug-wildcard failures resolved via
  `@debug *` span-text fallback (commit `2b4b1d0`). Five
  `pipeline_vm_full_*` and `adapter::*` failures additionally
  resolved via W6.A's `GrammarSink` directive-decoder safety.
  One failure remains: `test_selective_transitive_unfurling`
  (imports.rs) — pre-existing, orthogonal to AU scope, flagged
  for follow-up. Not an AU regression.

### Phase 5 — Profile-driven optimisation + bench parity

Status: **MET** (profiles), **PARTIAL** (parity).

- **AU.5.1** — 27 (bench, entry) pairs captured with seven
  required artefacts each; 19 profile sub-directories under
  `.profiles/samply/prebuild/`. Per-bench analysis in
  `profiling-2.md`. Headline hotspots recorded per grammar
  (JSON `__value`, CSS `__compoundSelector`/`__declaration`,
  Sheets precedence tower, BBNF `__mapped_factor`/`__big_comment`).
- **AU.5.2** — W7 full bench re-run completed (this document's
  `post-AU.json`). Regression vs AQ: canada 1796 → 1231 (-32%);
  this is the honest cost of actually storing the f64 payload
  that AQ computed then discarded (correctness fix).

### Phase 6 — Cross-bench generality levers

Status: mixed (6.1, 6.2, 6.5, 6.6, 6.7 landed; 6.3, 6.4, 6.8,
6.9 partial or missed).

- **AU.6.1** — 64-byte-aligned `padded_bytes()` on `ParserState`;
  four scanner kernels consume it. parse-that commit `64fe9f2`.
  The kernel opt-in cascade (dropping internal bounds guards
  once all callers pass padded views) is incomplete — AV scope.
- **AU.6.2** — `compute_push_fingerprint` IR pass +
  `GrammarIR.push_fingerprint` field; per-grammar (numer, denom)
  divisor emitted at codegen time. Commits `ff32c0b`, `c2664f3`.
  JSON canada +18% (989 → 1169), BBNF `json.bbnf` +49%.
- **AU.6.3** — Pratt precedence-tower flattening **did not ship**.
  Sheets precedence tower remains six-level left-recursive;
  `parse_simple` sits at 95 MB/s (gate was 200). Routes to AV.
- **AU.6.4** — Leaf payload activation for token-shaped rules.
  Partial: BBNF token rules grew `-> Span`/`-> i64`/`-> f64`
  annotations (commit `26d7620`), but the emitter's
  `TypeDesc::Named("Span")` path does not lower to PayloadData;
  4 sites in source, 0 firing at runtime. Routes to AV (Bug 2).
- **AU.6.5** — `.map(|_| ())` elimination at codegen. 309 sites
  → 0. Commit `4e4a75e`. Hard gate met; `no-value-discard`
  invariant enforced uniformly.
- **AU.6.6** — Bench-name disambiguation. `data` → `data_s`
  across JSON monolithic + value benches. Commits `de51346`
  (value side) + earlier monolithic rename. Per-entry profile
  attribution restored.
- **AU.6.7** — Unified arena on AoS. Every `payload_*: Vec<_>`
  side-car on `Tape` collapsed into one `arena: Vec<u8>`; ten+
  `push_leaf_with_*` methods collapsed into one
  `push_leaf_with(kind, PayloadData)`. `TapeRec::payload_idx`
  removed. `PayloadData` enum covers `None | InlineScalar(u32) |
  WideScalar(u64) | Aggregate(&[u8]) | Bytes(&[u8])`. Commits
  `3b75463`, `9a1186e`, `7fc0adf`.
- **AU.6.8** — Typed materialisation parity audit across
  grammars. 58 parity tests landed (JSON 9, CSS 14, BBNF 18,
  Sheets 17). 10 ignored post-integration (7 JSON variant-
  dispatch + 3 CSS percentageUnit reader migration).
  `typed-parity-audit.md` documents the two systemic codegen
  bugs blocking the "every `->` reaches the tape" invariant.
  Routes to AV.
- **AU.6.9** — BBNF comment fast path. Did not ship; Bug 2
  (`-> Span` lowers to push_compound) blocks the leaf-Span
  optimisation until the emitter admits `TypeDesc::Named("Span")`
  to the leaf-payload route. Routes to AV.

### Phase 7 — Substrate pivot: columnar tape (SoA)

Status: **GATE NOT MET, ROUTED TO AV**.

- **AU.7.1** — Scratch `columnar_tape` sibling crate measured
  sum-all-f64 on canada.json against AoS. AoS median 115,618 ns;
  SoA median 59,567 ns; ratio **1.94×**, seven cold runs, very
  low variance. Gate was ≥ 5×. **Gate not met.**
- **AU.7.2 / AU.7.3** — Substrate migration did not fire. W5
  routed to AU.6.7 (unified arena on AoS) per the plan's escape
  clause. SoA deferred to AV.

Key W4 finding for AV planning: `cols.pay_f64.iter().sum()` does
NOT auto-vectorise because strict-IEEE f64 left-fold blocks LLVM
reordering. A 4-lane reordered unrolling clears the gate at 6.64×;
SoA is necessary-but-not-sufficient. The emitter-side reordering
pattern is the additional AV lever.

## 2. Hard-gate status table

From AU.md §"Hard gates summary" (24 gates total). Status codes:
**MET**, **MET*** (met with documented qualifier), **PARTIAL**,
**MISSED**, **N/A**, **DEFERRED**.

### Structural activation (1–9)

| # | Gate | Status | Evidence / Routing |
|---|------|--------|--------------------|
| 1 | JSON `__value` number → `push_leaf_with_f64` | **MET** | Commit `83357e4`; W5 folds into `push_leaf_with(PayloadData::WideScalar)` |
| 2 | JSON `__value` bool → `push_leaf_with_bool` | **PARTIAL** | Only the `false` branch fires (typed-parity-audit Bug 1); `true` materialises `has_payload=false`. → AV |
| 3 | JSON `__value` null → `push_leaf_with_u8` | **MET** | Single-branch rule; InlineScalar |
| 4 | Payload correctness test passes | **MET** | `grammar_roundtrip` 6/6, `payload_layouts` 13/13, `tape_walker_allocs` 8/8 |
| 5 | CSS `number -> f64` activation (20 sites) | **MET** | Commits `63f86a5` + `240535b` (dispatch fix) |
| 6 | CSS `parse_hex_color` → `push_leaf_with_u32` | **MET** | Commit `2bd75a3`; 20+ aggregate sites fire |
| 7 | CSS 7/8 `scan_ident` → `CSS_IDENT_CONFIG` | **MET*** | Original audit claim was incorrect; correct routing is 7/8 using `DEFAULT_IDENT_CONFIG` per PROGRESS Session 1 |
| 8 | Every CSS dimension materialises `(f64, u8)` | **PARTIAL** | Activated via `558a457` + `05e38dc`; multi-branch unit rules hit Bug 1, single-branch (percentageUnit / flexUnit) fire reliably. → AV |
| 9 | Every CSS colour family round-trips | **PARTIAL** | `hex` + `namedColor` (113/148 branches) fire; `colorFunction`/`colorFn`/`colorMix` did not ship. → AV |

### Typed-materialisation parity (10–14)

| # | Gate | Status | Evidence / Routing |
|---|------|--------|--------------------|
| 10 | `json_monolithic_value` walks tape | **MET*** | Commits `de51346` + `39aa0d0` + `aa778c8`. 7 variant-dispatch parity tests ignored post-W5/W6.D (walker coherence) → AV |
| 11 | bbnf/sonic ratio ≥ 0.60 twitter / ≥ 0.80 canada | **MISSED** | canada 0.61 / twitter 0.52 post-W6.D. → AV (Eisel-Lemire + simdjson) |
| 12 | Every `->` reaches tape | **MISSED** | Bug 1 + Bug 2 systemic; typed-parity-audit.md details per-grammar firing tables. BBNF 4 sites in source, 0 firing at runtime. → AV |
| 13 | No `__*_comment` rule emits `push_compound` | **DEFERRED** | AU.6.9 leaf-Span optimisation blocked by Bug 2. → AV |
| 14 | `grep -cF '.map(\|_\| ())'` == 0 on every expand | **MET** | Commit `4e4a75e`; W3.C verified |

### Architectural cleanup (15–18)

| # | Gate | Status | Evidence / Routing |
|---|------|--------|--------------------|
| 15 | `ParsedGrammar` does not exist | **MET** | Commit `688d6ea`; GrammarSink trait + sinks replace the flat-Vec intermediate |
| 16 | `payload_idx` + side-car Vecs eliminated; single `push_leaf_with` | **MET** | Commits `3b75463` + `9a1186e`; arena + `PayloadData` enum; `_reserved: u16` in the vacated slot |
| 17 | `cargo test --workspace` compiles + zero new failures | **PARTIAL** | Compiles zero errors. Default `cargo test --workspace` exits 101 on `test_selective_transitive_unfurling` (pre-existing imports). Under `--no-fail-fast` the full picture is 967 pass / 33 fail / 30 ignored — 33 real failures hidden by cargo's stop-at-first behaviour. 4 css `tape_parity` goldens are stale post-W6.D (`4df6b8c` regenerated only json/sheets); 5 sheets_parity Bug-1 pinned tests flipped from PASS to FAIL once W6.D's scalar-bypass landed (audit expected `#[ignore]` marking that was missed); 23 remaining failures are pre-existing Session-1 '18 tests fail' family. → W7 follow-up or AV |
| 18 | `generated.rs` matches fresh regen | **MET** | Bootstrap idempotent across wave boundaries; commits `6e33a40` / `c9a4fc7` / `5b2a48e` / `28f5023` |

### Performance / cross-bench (19–24)

| # | Gate | Status | Evidence / Routing |
|---|------|--------|--------------------|
| 19 | Fresh samply profiles for 27 pairs | **MET** | `profiling-2.md` + 19 profile sub-directories under `.profiles/samply/prebuild/` |
| 20 | Every SIMD-aware kernel sees padded input | **PARTIAL** | parse-that `64fe9f2`; four kernels opt-in. Full cascade (all kernels drop internal bounds guards) → AV |
| 21 | JSON canada ≥ 1800 MB/s on decoded path | **MISSED** | canada 1231 MB/s tape-only; 0.61 × 3155 ≈ 1925 MB/s sonic-equivalent; does not clear 1800 on the honest decoded path. → AV |
| 22 | CSS bootstrap ≥ 600 MB/s (post AU.2.3 + AU.2.4) | **MISSED** | bootstrap 454 MB/s; residual is `__declaration` + `__compoundSelector`. → AV (PHF + SIMD selector classifier) |
| 23 | CSS bootstrap ≥ 550 MB/s (post AU.2.5 + AU.2.6) | **N/A** | Gate 22 not met; AU.2.6 colour aggregates did not ship, so the regression-budget gate is inapplicable |
| 24 | Sheets `parse_simple` ≥ 200 MB/s | **MISSED** | parse_simple 95 MB/s; AU.6.3 Pratt lowering did not ship. → AV |

**Tally**: 10 **MET**, 2 **MET*** (qualified), 5 **PARTIAL**,
5 **MISSED**, 1 **DEFERRED**, 1 **N/A**. Total 24.

## 3. Invariant verification

Restating AU.md §"Architectural invariants" 1–7 and verifying each
against the final state.

1. **No legacy code, no fallbacks, no workarounds.** Invariant
   holds. `ParsedGrammar` deleted; `StructRegistry` deleted; schema
   stub emitters deleted; `memchr1/2/3` + `nibble_lut` deleted in
   favour of structural bitmap; side-car payload Vecs deleted in
   favour of unified arena; ten+ `push_leaf_with_*` methods
   collapsed to one. Zero backward-compatibility shims.

2. **Every `->` annotation reaches the tape.** Invariant does
   **not** hold in strict reading. Bug 1 (alt-payload first-branch
   loss) and Bug 2 (`-> Span` lowers to push_compound) block
   substantial fractions of declared annotations. The parity test
   suite (`json_parity`, `css_l4_parity`, `bbnf_parity`,
   `sheets_parity`) pins the current firing surface so future
   codegen changes are visible-delta; typed-parity-audit.md
   documents the specific gaps and routes the fixes to AV.

3. **Inference composes types; never loses them.** Invariant
   holds at the IR level. `RefConstraint` (W3.F commit `558a457`)
   projects `Ref` targets' scalar type; `join_types` reconciles
   `Tuple([Span, T])` with bare-T under factor-pass (`05e38dc`).
   Codegen is where composition is lost, not inference.

4. **Parity targets are full typed AST equivalence, not isolated
   parse-speed.** Invariant acknowledged. AU did not clear the
   sonic/lightningcss equivalence bar — bbnf/sonic ratio plateaus
   at 0.52–0.61, and lightningcss equivalence requires all colour
   families + all dimension branches to reach the tape (blocked by
   Bug 1 + AU.2.6). Routes to AV.

5. **One tape layout, one access API.** Invariant holds post-W5.
   `Tape` carries only `records: Vec<TapeRec>` + `arena: Vec<u8>`.
   Single `push_leaf_with(kind, PayloadData)` entry point. View
   layer is uniform across grammars.

6. **Grammar-specialised codegen emitted from the grammar.**
   Invariant holds. Push-fingerprint pass per grammar (AU.6.2);
   structural-alphabet IR pass (AU.2.7); scanner dispatch driven
   by `RegexClass` metadata (commit `240535b`). No hand-written
   grammar-specialised constants remain.

7. **Type-descriptor coverage is total.** Invariant does **not**
   hold. `TypeDesc::Named("Span")` lacks a codegen route (Bug 2);
   aggregate payloads > 16 bytes (colour-function `(u8 space,
   f64×3, f64 alpha) = 33 B`) did not get the arena-backed
   widening. Routes to AV.

The `@scan`-directive escape remains a latent possibility; no
Phase 1–7 deliverable targeted it.

## 4. Cross-tranche debt addressed / deferred

### Addressed

| Item | Origin tranche | AU delta |
|------|---------------|----------|
| `ParsedGrammar` elimination | AR.7.2 | **DELETED** (11-tranche deferral resolved); commit `688d6ea` |
| `StructRegistry` population-or-deletion | AS.2.3 / AT.6.1 | **DELETED** (unused scaffold); commit `ab8588a` |
| 64-byte input padding | AR.5.2 | **LANDED** at `ParserState::new`; parse-that `64fe9f2` (kernel cascade deferred) |
| Fresh samply profiles | AT.4.1 | **LANDED** across 27 (bench, entry) pairs; `profiling-2.md` |
| Debug-wildcard failures | AT | **RESOLVED** via `@debug *` span-text fallback (commit `2b4b1d0`) |
| Number-dispatch strict-vs-permissive | W3 carry-over | **RESOLVED** via `RegexClass::Numeric { allow_leading_dot }` routing (commit `240535b`) |
| `.map(|_| ())` discards | AT codegen debt | **ELIMINATED** at every site (commit `4e4a75e`) |
| `generated.rs` stale vs fresh regen | AT.4.5 | **RECONCILED** at every wave boundary; idempotent |

### Deferred (explicit forward references)

| Item | Origin | Forward routing |
|------|--------|-----------------|
| Named struct view codegen | AT.6.2 | **AV** (colour aggregates + BBNF struct-shaped rules) |
| NEON fractional scan | AR.8.1 | **AV** (sub-item of Eisel-Lemire / simdjson work) |
| Bug 1 — Alt-payload first-branch loss | AU.6.8 (this tranche) | **AV** — per-alt-branch payload emission in `emitter/grammar.rs` |
| Bug 2 — `-> Span` lowers to push_compound | AU.6.8 (this tranche) | **AV** — admit `TypeDesc::Named("Span")` to the leaf-payload route |
| Bug 2b — scanner-to-payload i64/f64 threading | AU.6.8 (this tranche) | **AV** — `parse_that::scan_*_mut` returns `Option<T>`; emitter threads through alt prelude |
| Named-color factor-pass payload loss (35/148) | AU.6.8 (this tranche) | **AV** — IR-pass preserves payload writes through byte-dispatch factorisation |
| Colour-function aggregates ≥ 33 B | AU.2.6 (this tranche) | **AV** — extend arena-backed aggregate widening |
| CSS bootstrap 454 → 600 MB/s | AU.2.7 perf gate | **AV** — PHF + SIMD selector classifier over the structural bitmap |
| JSON canada 1231 → 1800 MB/s decoded | AU.1.4 / AU.3.2 | **AV** — Eisel-Lemire + simdjson-scale SIMD decode |
| Sheets parse_simple 95 → 200 MB/s | AU.6.3 | **AV** — Pratt precedence-tower lowering |
| SoA columnar substrate | AU.7 | **AV** — gate not met at 1.94×; 4-lane reordered unrolling pattern is the missing lever |
| `test_selective_transitive_unfurling` | Pre-existing | Dedicated imports-subsystem pass (orthogonal) |
| Padded-input kernel opt-in cascade | AU.6.1 | **AV** — drop internal bounds guards once all callers pass padded views |
| `variant_idx` dispatch walker coherence | AU.6.8 (7 JSON tests ignored) | **AV** — trivial once Bug 1 lands |
| CSS percentage InlineScalar reader migration (3 tests) | AU.6.7 (arena landing) | **AV** (or W-next) — trivial reader migration |
| Empty-compound `has_payload=true` API quirk | AU.6.8 parity-audit §Recommendations | **AV** — `push_compound` writes `TapeOffset::NONE` when children-run is empty |
| CSS tape_parity goldens stale (4 tests) | W6.D `4df6b8c` regen only covered json/sheets | **Early AV** — fresh regen of `crates/core/tests/fixtures/tape_golden/css_l4/*.json`; one-shot once the codebase settles |
| Sheets_parity Bug-1 pinned assertions (5 tests) | W6.C audit pinned as `assert_eq!(count, 0)`, should flip on fix | **AV** — assertions flip from `== 0` to `>= N` when Bug 1 lands; marking as `#[ignore]` was missed during W6.C integration |
| Session-1 '18 tests fail' family (23 tests) | Pre-existing across closures/debug/lower/analysis/graph/gorgeous/lsp/recover | **AV** — each pre-existing failure needs individual triage; hidden from W1–W6 orchestrator by cargo's stop-at-first behaviour |

## 5. Future work — AV planning seeds

Four themes, derived from the gap analysis above.

### AV-1: Typed-materialisation completion

Single theme spanning the two systemic bugs and the named-colour
factor-pass loss. Surgical fix for Bug 1 (per-branch payload-write
hoisting in `crates/core/src/backend/rust/emitter/grammar.rs`'s
alt-lit emission path) unblocks the majority of the PARTIAL gates
(2, 8, 9). Bug 2 fix (admit `TypeDesc::Named("Span")` to the
leaf-payload route; already whitelisted for KvPair in W2.B, extend
to bare-Span) unblocks BBNF's zero-firing token rules and Sheets's
`-> input : Span` rules. Bug 2b (scanner-to-payload i64/f64
threading) is heavier — cross-crate wiring through parse-that's
`scan_*_mut` API. AV design should treat 2b as the substrate-level
change that its emitter twin depends on.

### AV-2: Parse-stage performance substrate

The three perf misses (gates 21, 22, 24) share a common subtext:
the parse-stage hotspots live below the level that AU's levers
can reach. Eisel-Lemire is the JSON / CSS number bridge; simdjson
SIMD decode is the JSON string path; PHF + SIMD selector
classifier is the CSS declaration path; Pratt precedence lowering
is the Sheets expression path. Each is its own sub-pass. The
structural bitmap (AU.2.7) is the substrate that the selector
classifier and the number-scanner can both consume; reusing that
substrate is the integration discipline.

### AV-3: SoA columnar substrate (take 2)

W4's 1.94× prototype established that AoS+arena is not the
terminal state but that a naive SoA pivot does not pay for
itself. The identified lever — 4-lane reordered unrolling over
`pay_f64` — clears 6.64× on the sum-all-f64 benchmark. AV should
ship SoA with the emitter-side reordering pattern as a first-class
codegen pass, not as a post-facto optimisation. The column set
remains bounded (6 structural + 6 typed-payload + up to 2
grammar-specific overlays per AU.7.3's design).

### AV-4: Bench and test hygiene

Three small items that touch walker / reader paths uniformly:
`variant_idx` dispatch coherence after the unified-arena shape
shift; CSS percentage InlineScalar reader migration; the
empty-compound `has_payload=true` API quirk (`push_compound`
should write `TapeOffset::NONE` on empty children-runs). Tangential
to the three large themes but prevents the parity tests from
drifting under further tape-shape changes.

## Appendix — Bench snapshot (post-AU, commit `3b8b757`)

Full detail in `docs/benchmarks/post-AU.json`.

| Bench | Entry | ns/iter | MB/s |
|-------|-------|---------|-----:|
| json_monolithic | canada | 1,827,428 | 1231 |
| json_monolithic | citm | 708,191 | 2438 |
| json_monolithic | data_s | 20,326 | 1746 |
| json_monolithic | data_xl | 18,037,587 | 1179 |
| json_monolithic | twitter | 320,995 | 1967 |
| css_l4 | bootstrap | 616,486 | 454 |
| css_l4 | normalize | 8,351 | 735 |
| css_l4 | tailwind | 7,331,274 | 496 |
| google_sheets_monolithic | parse_simple | 5,271 | 95 |
| google_sheets_monolithic | parse_nested | 11,333 | 128 |
| google_sheets_monolithic | parse_stress | 15,121 | 121 |
| google_sheets_monolithic | format_simple | 140 | 42 |
| google_sheets_monolithic | format_stress | 3,813 | 52 |
| bbnf_monolithic | bbnf_self | 13,003 | 394 |
| bbnf_monolithic | css_l4_grammar | 102,451 | 496 |
| bbnf_monolithic | css_pretty | 3,950 | 647 |
| bbnf_monolithic | ebnf | 6,490 | 223 |
| bbnf_monolithic | google_sheets | 8,731 | 858 |
| bbnf_monolithic | json | 1,892 | 283 |
