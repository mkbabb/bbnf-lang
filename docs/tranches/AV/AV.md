# Tranche AV — The Flattening

AV is the semantic-parity tranche. It closes AU's typed-
materialisation debt, ships the dispatch automaton + PSI pipeline +
columnar substrate as one coherent architecture, and drives the
sonic-rs and lightningcss parity gates that AU planned but did not
reach. This is the tranche that earns the creed: *every `->`
annotation in every grammar reaches the tape; every typed AST bbnf
emits matches its lightningcss or sonic-rs counterpart node-for-
node; no fallbacks, no legacy paths, no workarounds*.

The scope is large because AU's bench reality forced it to be.
AU shipped the architectural refactor — arena unification,
ParsedGrammar elimination, `.map(|_|())` elimination, structural-
bitmap subsumption, fingerprint-driven capacity — and surfaced two
systemic codegen bugs that retroactively undermine the "every `->`
reaches the tape" invariant (`typed-parity-audit.md` Bug 1 and Bug
2). AU also missed three headline perf gates (JSON canada 1231 vs.
1800; CSS bootstrap 454 vs. 600; Sheets parse_simple 95 vs. 200)
because the residual hotspots live below the level that AU's
refactor could reach — Eisel-Lemire bridge, simdjson-scale string
decode, PHF + SIMD selector classifier, Pratt precedence lowering.
AV takes all of that plus the DTA/PSI/columnar pipeline as one
coordinated lift.

Large structural shifts carry real risk of between-wave churn.
The wave schedule below accepts between-wave test-suite failures
as a first-class allowance — V3 (DTA) and V4 (PSI) may leave the
tree in a state where some grammars don't parse between wave
boundaries, and the closure wave (V10) is where the tree becomes
green end-to-end. Each wave must still leave its own file bounds
committable; the *workspace* may be intentionally unworkable
between waves 3 and 10.

## Post-AU reality check

Per `docs/tranches/AU/FINAL.md` at tranche close, 10 of 24 hard
gates MET, 2 MET-qualified, 5 PARTIAL, 5 MISSED, 1 DEFERRED, 1 N/A.
The MISSED and PARTIAL gates, and the FINAL.md §4 deferred items,
are AV's opening scope. `post-AU.json` (commit `3b8b757`) shows
the perf floor AV builds from:

| Bench | Entry | post-AU MB/s | AV gate | Delta required |
|-------|-------|-------------:|--------:|---------------:|
| json_monolithic | canada | 1231 | 1800 | +46% |
| json_monolithic | twitter | 1967 | 2400 | +22% |
| json_monolithic | data_s | 1746 | 2100 | +20% |
| json_monolithic | data_xl | 1179 | 1700 | +44% |
| json_monolithic | citm | 2438 | 3000 | +23% |
| css_l4 | bootstrap | 454 | 600 | +32% |
| css_l4 | normalize | 735 | 1100 | +50% |
| css_l4 | tailwind | 496 | 1200 | +142% |
| google_sheets_monolithic | parse_simple | 95 | 250 | +163% |
| google_sheets_monolithic | parse_nested | 128 | 300 | +134% |
| google_sheets_monolithic | parse_stress | 121 | 300 | +148% |
| bbnf_monolithic | bbnf_self | 394 | 500 | +27% |
| bbnf_monolithic | json | 283 | 400 | +41% |
| bbnf_monolithic | ebnf | 223 | 350 | +57% |
| bbnf_monolithic | google_sheets | 858 | 1000 | +17% |
| bbnf_monolithic | css_pretty | 647 | 800 | +24% |
| bbnf_monolithic | css_l4_grammar | 496 | 650 | +31% |

The tailwind and sheets gates are the largest (2.4× and 2.6×
respectively) because the DTA + shunting-yard + document-level
parallel parse all land together.

## Architectural thesis

The grammar's static fingerprint is a first-class **codegen output
channel**, not merely an optimiser input. Every per-grammar bit of
knowledge — push counts, payload classifications, fixed-shape
e-classes, structural alphabet, document-list rules, branch priors,
keyword sets, column-set selection, dedup-eligible rules — reaches
the emitted binary as a specialised constant, layout, or kernel.
This is what an owned-stack grammar/compiler/tape pipeline can do
that a library (simdjson, sonic-rs, serde_json, lightningcss)
cannot.

The parsing pipeline becomes three cache-friendly linear passes:

1. **Stage A — skeleton mining.** A grammar-derived counter-DFA
   reads bytes once and emits the tape's full structural skeleton
   (kinds, variant_idx, meta_idx, span_lo; `span_hi = 0`,
   `payload_off = 0`) plus a `PayloadJob` stream naming every
   scalar leaf whose payload needs decoding.
2. **Stage B — payload fill.** Workers run the appropriate
   scanner kernel (Eisel-Lemire-widened `scan_number_f64`,
   simdjson-style `decode_json_string`, `parse_hex_color`, etc.)
   over disjoint `PayloadJob` slices and write decoded values
   into dense typed columns (SoA).
3. **Stage C — tree finalisation.** A segmented prefix scan over
   frame depths reconstructs `child_off` and `span_hi` per
   compound record.

The tape stays flat. The walker stays uniform. The `.view()` API
is unchanged surface-wise. What changes is the construction path
and the internal layout — both grammar-specialised at codegen
time.

## Architectural invariants

Inherited from AU and strengthened:

1. **No legacy code, no fallbacks, no workarounds.** Deletions
   where reductions serve. No backward-compat shims, no `#[allow]`
   to mask issues, no half-landing hybrids.
2. **Every `->` annotation reaches the tape emitter.** Inference
   composes types and never loses them. AU's Bug 1 and Bug 2 are
   Phase 0 scope — this invariant is a V0-exit gate, not a V10
   closer.
3. **Parity targets are full typed-AST equivalence.** bbnf's
   `.view().as_color()` on `rgb(255 128 0 / 0.5)` returns the
   same tuple shape that `lightningcss::values::color::Color::RGBA
   { r: 255.0, g: 128.0, b: 0.0, a: 0.5 }` builds. bbnf's JSON
   `.view().as_value()` round-trips into `sonic_rs::Value`
   equivalence. Speed gates are downstream of parity; gates that
   would regress parity are rejected.
4. **One tape layout, one access API, one substrate.** The
   columnar substrate (AoS → SoA) is the terminal layout. No
   conditional row/column choice per grammar; the column set is
   grammar-parameterised, but the type `Tape` and the walker API
   are uniform.
5. **Grammar-specialised codegen is emitted from the grammar.**
   Schemas, dispatch tables, payload layouts, scanner alphabets,
   capacity closures, column selectors, keyword tables,
   column-set selections — all generated. Every codegen decision
   is derived from rule structure and fingerprint data. The
   grammar-author surface remains the BBNF syntax the existing
   grammars already use; AV adds no annotations grammar authors
   have to learn.
6. **Type-descriptor coverage is total.** Primitives (`i8`..`u64`,
   `f32`/`f64`, `bool`), `Span<'src>`, owned UTF-8 via the arena,
   tuples, tagged-union enums with recursive payloads (CSS
   `color-mix` with nested `Color` references is the canonical
   case), optional types, variable-length lists — every
   `TypeDesc` variant has a codegen route. The V0 closure proves
   it against every grammar in the test corpus.
7. **Runtime structural dedup is part of the architecture, not a
   gated optimisation.** Every grammar using the DTA gets runtime
   bloom + GADT dedup on dedup-eligible rules. Activation is
   fingerprint-driven per-grammar at codegen time. When a
   grammar has zero dedup-eligible rules (inferred payload type
   `Unit`/`Span` with no child Span-returning leaves), the
   emitter elides the bloom+GADT scaffolding for that grammar —
   same decision process, different cost.

## Precedent and archaeology

Every concept in AV has a commit trail. Resurrection without
archaeology is rejected per `docs/instructions/RESEARCH.md`.

- **Dispatch Tape Automaton / flat parser.** Never attempted in
  production. `AA-prototype-3.md` sketched an alternative
  substrate in the pre-AM era; AM/AN/AO/AP pursued incremental
  scanner and dispatch refinement but kept the fn-per-rule
  recursion all the way through AU. AV is the first production
  DTA lift. The AU.2.7 structural bitmap (commits `143d19e` +
  parse-that's SIMD `filter_quote_parity`) is the stage-1 byte-
  class primitive the DTA dispatches on.
- **Columnar substrate (SoA).** AU's W4 prototype (FINAL §1 Phase
  7) measured AoS+arena → SoA at 1.94× on `sum-all-f64(canada)`;
  gate was ≥ 5×. Gate missed because `cols.pay_f64.iter().sum()`
  does NOT auto-vectorise — strict-IEEE f64 left-fold blocks LLVM
  reordering. **A 4-lane reordered unrolling clears 6.64×** on
  the same benchmark. AV ships SoA together with the emitter-
  side reordering-codegen pass — SoA is necessary-but-not-
  sufficient, and AU routed this explicit finding to AV.
- **Subtree deduplication.** `AE/tape-shapes.md` catalogued
  shape taxonomy but never reached the tape. E-graph
  `EClassFacts.is_fixed_shape` + `RecognizerSignature.shape_hash`
  were built across AM/AN for inline/fuse optimisations and sit
  dormant relative to the tape. AV admits them as the
  compile-time half of dedup; the runtime bloom+GADT half stacks
  on top, unconditionally active where the fingerprint says the
  grammar has dedup-eligible rules.
- **Eisel-Lemire widening + simdjson decode.** `parse-that/
  rust/parse_that/src/parsers/scan/number_*.rs` carries a
  scalar-SWAR integer path + fractional-SIMD path today. The
  canada dataset's 11.5% `compute_f64` self-time is the bridge
  between those paths and the Eisel-Lemire finaliser. No prior
  tranche attempted an integer-fastpath widening or an AVX-512 /
  NEON lanewise decode. `decode_json_string_to_arena` exists
  (AU.3.1) but its kernel is scalar UTF-8-aware byte copy with
  escape handling; simdjson's `_mm256_cmpestri` + 8-byte
  fast-path lane for escape-free runs is unimplemented.
- **PHF + SIMD selector classifier.** CSS `__compoundSelector`
  was 33–43% of self-time pre-AU, dropped to a still-dominant
  share post-AU. No prior attempt at a structural-bitmap-driven
  SIMD selector classifier; this is greenfield for CSS and the
  largest single lever toward the bootstrap + tailwind gates.
- **Pratt precedence lowering.** Sheets' six-level left-
  recursive tower (`__comparison_expr → … → __unary_expr`) is
  the 56–86% self-time band. AU.6.3 planned the lowering; it did
  not ship. AV's AV.3.3 shunting-yard DTA is the natural home —
  the DTA discovers the collapse, not a special case.

## Wave schedule

Ten waves, each with explicit fan-out and file bounds. Between-
wave workspace failures are permitted on waves V3–V9; V10 is the
final-workable wave that produces the completion artefacts.

| Wave | Parallel sub-agents | Workspace state | Blocks |
|------|---------------------|-----------------|--------|
| **V0 — Typed-materialisation completion** (5 parallel) | (a) Bug 1 emitter fix (AV.0.1), (b) Bug 2 + 2b emitter + parse-that wiring (AV.0.2 + AV.0.3), (c) Named-color factor-pass preservation (AV.0.4), (d) Colour-function aggregate widening ≥ 33 B (AV.0.5), (e) Empty-compound `has_payload` API quirk (AV.0.6). Each agent owns disjoint emitter + IR + bbnf-tape files. | Green after V0; all 58 parity tests + pinned assertions flip | V1 |
| **V1 — GrammarProfile codegen channel** (serial) | Single agent: AV.1.x promotes push fingerprint, fixed-shape e-classes, structural alphabet, keyword tables, list rules, branch priors into `const GRAMMAR_PROFILE` per grammar. | Green | V2 |
| **V2 — Columnar substrate + reordering codegen** (3 parallel) | (a) `Columns` struct + sibling-skip pointers (AV.2.1 – AV.2.2), (b) column-local rank + payload-idx elimination (AV.2.3), (c) 4-lane reordered-unrolling codegen pass (AV.2.5). Walker migration (AV.2.6) sequenced at wave close. | Green after V2; fixture regen locks row-vs-column parity | V3 |
| **V3 — DTA synthesis** (serial) | Single agent: AV.3.x lifts GrammarIR to counter-DFA + shunting-yard + diagnostic replay + Eisel-Lemire widening. The DTA is one file; parallelism inside this wave produces merge conflicts. | **May leave workspace unworkable**; DTA-only grammars parse, legacy fn-per-rule path is being deleted | V4 |
| **V4 — PSI stage-B + stage-C + simdjson decode** (3 parallel) | (a) PSI stream + rayon stage-B (AV.4.1 – AV.4.2), (b) simdjson-scale JSON string decode (AV.4.3), (c) segmented prefix-scan stage-C (AV.4.4). Disjoint kernel files. | **May leave workspace unworkable** between V4 and V10 | V5 |
| **V5 — ShapeDictionary** (2 parallel) | (a) CSS declaration shape (AV.5.1 – AV.5.5), (b) BBNF big_comment + mapped_factor (AV.6.1 – AV.6.3). `TapeKind::ShapeRef` variant lands in V4's finaliser worktree so V5 agents fan out without editing the kind enum concurrently. | Workspace unworkable until V10 | V6 |
| **V6 — Document-level parallel parse** (serial) | Single agent: AV.7.x list-rule identification + chunk boundary detection + offset remap + fingerprint-gated activation. | Workspace unworkable until V10 | V7 |
| **V7 — SIMD keyword dispatch + PHF + selector classifier** (4 parallel) | (a) PHF for CSS namedColor + Sheets function names (AV.8.1, AV.8.4), (b) SIMD-wide keyword compare for CSS colorType + BBNF directive (AV.8.2), (c) CSS selector classifier over structural bitmap (AV.8.3), (d) BBNF directive + CSS keyword enums via SIMD. | Workspace unworkable until V10 | V8 |
| **V8 — Runtime bloom+GADT dedup** (serial) | Single agent: AV.9.x adds content-addressed runtime dedup, mandatory where fingerprint shows dedup-eligible rules. Activation is fingerprint-driven at codegen time. | Workspace unworkable until V10 | V9 |
| **V9 — Walker + reader migration closure** (2 parallel) | (a) variant_idx walker coherence across all 58 parity tests (AV.10.1); (b) percentage InlineScalar reader migration + pre-existing test triage (AV.10.2 – AV.10.3). Closes the `#[ignore]`-marked tests AU deferred. | Workspace green by V9 close | V10 |
| **V10 — Tranche completion** (serial) | Single agent: bench re-run across the four parse-benches, `post-AV.json` write, `FINAL.md` composition, workspace test confirmation. No code changes. | Green | — |

**Wave-failure policy.** On waves V3–V8 the workspace is allowed
to fail `cargo test --workspace`. The orchestrator documents
which tests fail at each wave boundary in `PROGRESS.md` and
tracks them forward to V9 where the closure happens. A wave that
breaks a test the *previous* wave left green without documenting
the break is a violation — regression sneaking through is never
acceptable. Wave V10 does not run if V9 leaves any test failing;
the orchestrator extends V9 until green.

**Cross-wave invariants.**

- Every wave commits onto master before the next wave dispatches.
- No file is written by two agents in the same wave. When a file
  spans sub-phases, promote the split-owner piece to an earlier
  or later wave.
- `TapeKind::ShapeRef` lands exactly once (in V4's finaliser
  worktree); V5 agents consume it read-only.
- The DTA file (`crates/core/src/backend/rust/emitter/dta.rs`) is
  serialised across V3 and is the single write target for that
  wave. V4's stage-B, stage-C, and simdjson decoder all land in
  sibling files, no collision.

## Phases

### Phase 0 — Typed-materialisation completion (AU closure)

AU's `typed-parity-audit.md` documents two systemic codegen bugs
plus associated secondary gaps. Phase 0 closes every one before
V1 dispatches. This is the V0-exit gate for AV invariant #2.

#### AV.0.1 Bug 1 — Alt-payload first-branch loss

For Alt-bodied rules with literal alternatives carrying typed
payloads (e.g. `bool = "true" -> true | "false" -> false`), the
emitter writes the payload-write block only on the first match-arm
of the alt. `crates/core/src/backend/rust/emitter/grammar.rs` (the
alt-lit emission path) constructs the per-branch match arms via a
closure that gates `__has_payload = true` on a single admissibility
flag set before the closure fires; the conditional needs hoisting
so every branch receives the write.

Fix: per-branch payload-write emission. The `__aggregate_buf`
fill and `__has_payload = true` lines appear in every arm of
`'__alt_lit_blk0`, keyed by the branch's declared return value.
Targets: JSON `bool` (2 branches), every CSS Nu8 unit rule (7+24+
6+12+4+2+2+4 = 61 branches), every CSS keyword enum
(`colorType`, `colorSpace`, `mixSpace`, `hueMethodKeyword`,
`radialShape`, `radialExtent`, `linearSide`, `mediaType`,
`filterName`, `mathFunctionName`, `mathProductOp`, `mathOperator`,
`anPlusB`, all `*Keyword` rules), Sheets `boolean`, `error_literal`
(9 branches), `sheet_prefix`, `compare_op` (6), `add_op`, `mul_op`,
`unary_prefix`.

Hard gate: every `pinned_*_drops_payload` test in
`crates/core/tests/{json,css_l4,bbnf,sheets}_parity.rs` flips
from `assert_eq!(count, 0)` to `assert!(count >= N)` with N equal
to the grammar-declared branch count. 5 sheets_parity assertions
flip first. Walker coherence for the 7 ignored JSON variant-
dispatch tests restored (AV.10.1 confirms).

#### AV.0.2 Bug 2 — `-> Span` leaf-payload route

`TypeDesc::Named("Span")` (from BBNF `-> Span` shorthand and
Sheets `-> input : Span`) admits into `TypeDesc::from_scalar_name`
and `is_type_name` for the Alt-bodied dispatch path but the rule-
body emitter does not route the resulting `__has_payload` setup
through the bare-Span case of `PayloadData::Aggregate(8 bytes)` or
`PayloadData::WideScalar(...)`.

Fix: extend the KvPair-aggregate whitelist landed in AU W2.B to
admit bare-Span to the leaf-payload route. Spans are 8 bytes
(`(u32 lo, u32 hi)`), so they pack into the existing 8-byte
aggregate slot without widening.

Targets: BBNF `identifier`, `literal`, `regex`, `big_comment`,
`comment`, `string_lit` (6 rules, currently 0 firing at runtime).
Sheets `string`, `cell_ref`, `identifier`, `sheet_prefix` (where
the rule declares `-> input : Span`).

Hard gate: BBNF `push_leaf_with` firing count moves from 0 to ≥ 6
in generated.rs; every `pinned_*_drops_payload` BBNF test flips.

#### AV.0.3 Bug 2b — Scanner-to-payload `i64`/`f64` threading

BBNF `int_lit -> i64` and `float_lit -> f64` route through
`__value_atom`'s `__payload_tag` dispatch, but the inner
`__int_lit` / `__float_lit` rules declare `__has_payload = false`
and never overwrite it — the scanners `scan_digits_mut` and
`scan_hex_mut` advance `state.offset` without returning the
parsed scalar.

Fix: this is cross-crate work. `parse_that::scan_digits_mut`,
`scan_hex_mut`, and the corresponding `scan_number_f64` variants
gain `_parse_mut` variants returning `Option<(end_offset, T)>`
instead of `Option<()>`. The emitter threads the parsed scalar
through the alt prelude into `__payload_i64` / `__payload_f64` /
`__aggregate_buf`. Same pattern as the existing
`scan_quoted_string_strict_mut` → decode kernel threading landed
in AU.3.1.

Targets: BBNF `int_lit`, `float_lit`, `bool_lit` (currently no
runtime firing despite the prelude scaffolding).

Hard gate: `grammar_roundtrip` + typed-parity tests prove `i64`
and `f64` payloads reach the tape for BBNF numeric literal
rules. `pinned_int_lit_drops_payload` and
`pinned_float_lit_drops_payload` flip.

#### AV.0.4 Named-color factor-pass payload preservation

The byte-dispatch factorisation in `crates/ir/src/passes/
sets/dispatch/annotate.rs` factors the 148-branch `namedColor`
alt by first-byte prefix; branches whose first byte is shared
with other keywords (e.g. `violet` / `yellow` both starting with
inside factored sub-alts) lose their payload writes during the
factor-pass rewrite. ~35 of 148 branches affected.

Fix: the factor-pass rewrite walks the Alt node carrying each
branch's `MapExpr` payload; the rewrite to a byte-dispatch
table must propagate the MapExpr into the sub-alt's payload
write rather than discarding it. IR-level fix in
`crates/ir/src/passes/transform/` (or wherever factor-pass
lives). Parity test `css_l4_parity::named_color_all_148_fire`
will assert full coverage after the fix.

Hard gate: all 148 namedColor branches emit
`push_leaf_with(u32)` at runtime; CSS L4 `expand.rs` shows 148+
`push_leaf_with` sites reachable from the namedColor arm.

#### AV.0.5 Colour-function aggregates (≥ 33 B arena-backed)

AU.2.6's aggregate-size widening to ≥ 33 bytes did not ship:
`push_leaf_with_aggregate` caps at 16 bytes inline, and the
arena-backed wider-aggregate variant was only scaffolded for
strings (`PayloadData::Bytes`). CSS `colorFunction` (rgb/rgba/
hsl/hsla/hwb/lab/lch/oklab/oklch), `colorFn` (`color(<space>
c1 c2 c3 / α)`), and `colorMix` (`color-mix(in <space>
<hue-method>? , <color> α? , <color> α?)`) need payloads of
33+ bytes (`u8 space + f64×3 + f64 alpha`, with nested
`ColorRef` indirection for color-mix recursion).

Fix: `PayloadData` gains a `LargeAggregate(&[u8])` variant (>
16 bytes, arena-backed) stored identically to `Bytes` but
carrying no length prefix — the size is known from the kind +
variant_idx at read time via the payload-layout table. The
emitter routes `push_leaf_with_aggregate` with an oversize
`&[u8]` through the arena path transparently. Colour-function
grammar annotations land in `grammar/css/l4/color.bbnf` as
`colorFunction -> (u8 space, f64 c1, f64 c2, f64 c3, f64 alpha)`
and equivalents for `colorFn` and `colorMix`.

`colorMix` recursion: the nested `<color>` arguments are not
duplicated in the aggregate; instead, each nested position
holds an 8-byte `ColorRef(arena_offset: u32, kind_tag: u8,
_pad: [u8; 3])`. Readers follow the arena offset to the nested
colour aggregate.

Hard gate: `CssL4Parser::parse("rgb(255 128 0 / 0.5)").view()
.as_color()` returns `Color::Rgb { r: 255.0, g: 128.0, b: 0.0,
a: 0.5 }`, byte-equivalent to `lightningcss::values::color::
Color::RGBA { r: 255.0, g: 128.0, b: 0.0, a: 0.5 }`. The full
bootstrap.css + tailwind.css corpora round-trip colour-by-colour
against lightningcss's typed output.

#### AV.0.6 Empty-compound `has_payload=true` API quirk

`bbnf-tape`'s `push_compound(kind, children_start, ...)` writes
`child_off = marked_offset` even when the children run is empty;
the reader's `has_payload()` returns true on any non-NONE
`child_off`, so empty compounds spuriously report payload-present.

Fix: `push_compound` writes `TapeOffset::NONE` to `child_off`
when `children_start == self.records.len()` (empty children run).
Readers: unchanged; `TapeOffset::NONE` remains the single
has-payload-false sentinel. This shifts tape semantics across
every reader and requires a coordinated golden regen.

Hard gate: `tape_parity` goldens regenerated under the new
semantics; `crates/bbnf-tape/tests/tape_basic.rs` validates the
NONE-on-empty-children invariant; all 58 parity tests green.

#### AV.0.7 Padded-input kernel opt-in cascade

AU.6.1 landed `padded_bytes()` at `ParserState::new` (parse-that
`64fe9f2`); four scanner kernels consume it. The full cascade —
dropping internal per-chunk bounds guards once all callers pass
padded views — was deferred.

Fix: every SIMD-aware scanner kernel (`scan_quoted_string_simd`,
`scan_digits_simd`, `scan_ws_block_comments` SIMD inner loop,
`scan_ident_fast`, hex-digit scanner, `scan_number_f64` and
strict variant, UTF-8 validation path, `decode_json_string`
escape-handling path) takes a `PaddedView<'_>` instead of
`&[u8]`, eliminates its per-chunk bounds check, asserts-at-
construction that the source is padded. Caller refactoring via
`ParserState::padded()`.

Hard gate: `grep -r "if i + 16 <= bytes.len()" parse-that/` and
similar per-chunk bounds checks return 0 in the consolidated
SIMD inner loops. Measured +2–4% uniform across JSON / CSS /
Sheets / BBNF on the matrix post-V0.

#### AV.0.8 – AV.0.12 Test hygiene and deferred closures

Folded as one sub-phase because each is mechanical and scoped to
its fixture or parity surface.

- **AV.0.8** — 4 stale CSS `tape_parity` goldens regenerated
  (W6.D's `4df6b8c` covered only json/sheets).
- **AV.0.9** — 7 JSON variant-dispatch parity tests un-ignored
  after Bug 1 lands.
- **AV.0.10** — 3 CSS percentage InlineScalar reader migration
  tests: switch reader call sites from `payload_aggregate(kind)`
  to `payload_inline_scalar(kind)` per the post-W5 arena shape.
- **AV.0.11** — 23 Session-1 pre-existing failures (closures,
  debug, analysis, graph, gorgeous, lsp, lower, recover): each
  triaged individually. Category A (orthogonal, pre-existing
  scope) stays `#[ignore]` with a documented forward ticket;
  Category B (cascaded from AU's substrate change) fixes
  alongside V0. Category C (fixed by Bug 1 / Bug 2 landing)
  auto-resolves when V0 completes.
- **AV.0.12** — `test_selective_transitive_unfurling`
  (imports.rs): orthogonal to AV scope; stays deferred with a
  ticket reference. Documented in PROGRESS as the one
  acceptable residual ignore.

Hard gate: `cargo test --workspace` with `--no-fail-fast` shows
30 ignored / 0 failed after V0. The ignored count is pre-existing
items explicitly documented per the `AV.0.11` Category A triage.

### Phase 1 — GrammarProfile as codegen output channel

`GrammarProfile` is the runtime face of the IR fingerprint data
that AU computed but left scattered across emitter-local constants
and runtime ad-hoc queries. Every AV phase consumes it.

```rust
// crates/bbnf-tape/src/profile.rs (new)
pub struct GrammarProfile {
    pub push_compound_count: u16,
    pub push_leaf_count: u16,
    pub push_leaf_with_count: u16,

    pub compounds_per_input_byte: f32,
    pub leaves_per_input_byte: f32,
    pub payload_bytes_per_input_byte: f32,
    pub expected_ns_per_byte: f32,
    pub parallel_break_even_bytes: u32,

    pub structural_alphabet: &'static [u8],
    pub structural_digraphs: &'static [[u8; 2]],
    pub active_columns: &'static [ColumnId],
    pub list_rules: &'static [RuleId],
    pub keyword_tables: &'static [KeywordTable],
    pub shape_dict: &'static [ShapeEntry],
    pub branch_priors: &'static [BranchPrior],
    pub dedup_eligible_rules: &'static [RuleId],
    pub reorder_unroll_visitors: &'static [VisitorId],
}
```

#### AV.1.1 IR plumbing

Every field is already computable in `bbnf-ir`: push counts from
`PushFingerprint` (AU.6.2), shape_hash from `RecognizerSignature`,
`is_fixed_shape` from `EClassFacts`, `structural_alphabet` from
the AU.2.7 pass, column-set from the post-W5 `TypeDesc` universe,
list-rule detection from structural mining, keyword tables from
Alt-of-literal mining. Consolidate the reads into a single
`GrammarIR::profile()` accessor.

#### AV.1.2 Emitter integration

`crates/core/src/backend/rust/emitter/profile.rs` (new) emits
`const GRAMMAR_PROFILE: GrammarProfile = GrammarProfile { ... };`
into each grammar's `generated.rs`. The struct literal is fully
const-evaluable; no runtime initialisation.

#### AV.1.3 Downstream consumers

Replace AU's per-emitter constants with `GRAMMAR_PROFILE` field
reads: `TAPE_CAPACITY_NUM`/`TAPE_CAPACITY_DEN` (AU.6.2 W3.B),
`STRUCTURAL_ALPHABET` (AU.2.7), capacity coefficients, scanner
dispatch routing — all resolve via the profile. The wins are
consolidation and the new fields that V2–V9 need (active_columns,
dedup_eligible_rules, reorder_unroll_visitors).

Hard gate: `grep -rn 'const [A-Z_]*: &\[u8\]' crates/core/src/
backend/rust/emitter/` returns 0. Every per-grammar emitter
constant that grammars differ on has moved into `GRAMMAR_PROFILE`.

### Phase 2 — Columnar substrate (SoA) + reordered-unrolling codegen

AU's W4 columnar prototype measured 1.94× on `sum-all-f64(canada)`
vs. 6.64× with 4-lane reordered unrolling. The pivot ships both:
the substrate swap and the codegen pass that makes typed-payload
bulk visitors auto-vectorise.

Column set (fixed, 6 structural + 6 typed-payload + up to 2
grammar-specific overlays):

| Column | Type | Width | Role |
|--------|------|------:|------|
| `kinds` | `Vec<u8>` | 1 B | `TapeKind` discriminant |
| `span_lo` | `Vec<u32>` | 4 B | Source start offset |
| `span_hi` | `Vec<u32>` | 4 B | Source end offset |
| `sib_skip` | `Vec<u32>` | 4 B | Distance to next sibling, 0 if last |
| `flags` | `Vec<u8>` | 1 B | variant_idx (5b) + has_children (1b) + meta_bit (1b) + reserved (1b) |
| `meta_lo4` | Packed nibbles | 0.5 B | Low 4 bits of meta_idx |
| `pay_f64` | `Vec<f64>` | 8 B | Numeric leaves |
| `pay_u32` | `Vec<u32>` | 4 B | Hex colors, unit enums widened, compact arena refs |
| `pay_u64` | `Vec<u64>` | 8 B | Integer literals, timestamps |
| `pay_u8` | `Vec<u8>` | 1 B | Bool, small enums, unit discriminants |
| `pay_agg` | arena `Vec<u8>` + `Vec<u32>` offsets | var | Aggregates, decoded strings, colour functions, color-mix refs |
| `str_off` | `Vec<u32>` | 4 B | Arena offset for bare-Span / decoded-string leaves |

Sibling-skip pointer replaces first-child pointer: first child is
always `idx + 1` in pre-order; `sib_skip` is the distance to the
next sibling (0 = last among siblings). Walker for child descent:
no pointer load at all. Siblings traverse via one indexed column
read.

Column-local positional rank replaces per-record payload_idx: the
k-th record of kind `F64` lives at `pay_f64[k]`; walkers maintain
a monotonic running rank counter during `.view()` descent. For
grammars with sparse typed access, a per-kind optional
`f64_rank: Vec<u32>` overlay is emitted conditionally — written
only for records that actually carry f64.

#### AV.2.1 – AV.2.2 Columns struct + sibling-skip

`crates/bbnf-tape/src/columns.rs` (new) holds the six structural
columns. `Tape` gains a typed wrapper over `(Columns, Payloads)`
maintaining the pre-AV semantics at the `.view()` surface.

`crates/bbnf-tape/src/cursor.rs` switches from `TapeRec`-based
accessors to column-indexed reads. The `TapeOffset` → `u32 idx`
wire representation is preserved.

#### AV.2.3 Column-local rank + payload_idx elimination

`TapeRec::payload_idx` deletion is permanent; the reader
monotonic rank counter lives on `TapeCursor`. The `PayloadData`
enum from AU.6.7 retains its variants but `InlineScalar(u32)`
now always inlines into the appropriate typed column (never into
`child_off`); the AU-era `child_off` overload is unwound.

#### AV.2.4 Per-grammar column-set selection

`GrammarProfile::active_columns` drives which payload columns are
allocated for a grammar. JSON activates `pay_f64 + pay_u8 +
pay_u32 + str_off + pay_agg`; CSS adds `pay_agg`-heavy usage and
the `pay_selref: Vec<u32>` overlay; Sheets adds `pay_cellref:
Vec<u32>` for (row, col, abs_row, abs_col) packed refs; BBNF
uses `str_off` almost exclusively (post-V0 Bug 2 fix). The
emitter refuses to activate a 13th column without a registered
overlay registration.

#### AV.2.5 Reordered-unrolling codegen for typed-payload visitors

The W4 finding:
`cols.pay_f64.iter().sum::<f64>()` compiles to a scalar left-fold
because strict-IEEE f64 addition is non-associative and LLVM
cannot reorder. A 4-lane reordered accumulator (`lane0 + lane4 +
lane8 + …`, `lane1 + lane5 + lane9 + …`, etc., then reduce at
end) vectorises cleanly on NEON / AVX2.

`GrammarProfile::reorder_unroll_visitors` names the visitors for
which the codegen should emit the reordered pattern:
sum/reduce/max/min/count over a typed payload column. The
codegen pass lives in `crates/core/src/backend/rust/emitter/
visitor.rs` (new). Grammar authors don't write the pattern —
visitor-like accessors declared in the grammar (`$sum_of_f64()`,
`$count()`) are the lowering targets; the emitter produces the
unrolled loop.

#### AV.2.6 Walker migration

Every `.view()` accessor across `crates/core/src/backend/rust/
view/` migrates from `TapeRec`-indexed reads to column-indexed.
The `ViewRef<'_, T>` structural shape doesn't change at the
public API surface.

#### AV.2.7 Fixture regen + parity

All `crates/core/tests/fixtures/tape_golden/*/*.json` regenerate
under the new layout (record count, child_off semantics, sib_skip
annotations). `cargo test --test tape_parity` green before V3
dispatches.

Hard gates:

- `Vec<TapeRec>` does not exist anywhere in the codebase (`grep
  -rn 'Vec<TapeRec>' crates/` returns 0); `Columns` is the
  substrate.
- First-child descent compiles to a single indexed load from
  `sib_skip` + a bounded add, zero pointer dereferences.
- `sum-all-f64(canada)` on the columnar path shows ≥ 6× speedup
  over the pre-AV AoS path (the W4 projection).
- Every tape-parity fixture passes after regeneration.
- sonic-rs JSON-value parity tests pass on the columnar walker.
- lightningcss CSS AST equivalence tests pass on the columnar
  walker (covers AV.0.5 colour-function round-trip).

### Phase 3 — Dispatch Tape Automaton (DTA)

The DTA replaces the recursive-descent-per-rule codegen with a
grammar-derived DFA + counter that mines the full tape skeleton
in one linear byte pass. Each stage-A record lands with correct
`kind_meta` / `variant_idx` / `meta_idx` / `span_lo` and empty
`span_hi` / `sib_skip` / payload column positions.

The DTA has three layers:

1. **Byte-class dispatch.** The AU.2.7 structural bitmap produces
   a 64-bit-per-stripe mask of the grammar's structural alphabet.
   The DTA walker consumes `trailing_zeros(mask) → offset` and
   selects an Alt branch via the grammar's dispatch LUT keyed on
   `src[offset]` and (optionally) `src[offset + 1]` for digraphs.
2. **Frame counter stack.** Seq frames hold a linear advance
   counter; Alt frames hold the selected branch index; Repeat
   frames hold a count + body-DFA pointer. The stack is a fixed-
   size `[Frame; 64]` with the depth tracked in a single `u8`.
   For grammars with nesting depth > 64 (not observed in the
   target corpus), the stack spills to a heap-allocated overflow
   region — one allocation, amortised zero on realistic inputs.
3. **Counter-DFA extensions.** Pure DFA cannot represent nested
   optional-with-empty-body (BBNF `__mapped_factor`'s optional
   `( "->" __value_expr __type_annotation? )?`) without state
   explosion. Counter-DFA handles it with one extra counter per
   nested optional, keyed by grammar annotation (optional-
   counter marked on the IR during lift).

#### AV.3.1 DTA lifter (GrammarIR → counter-DFA)

`crates/core/src/backend/rust/emitter/dta.rs` (new, single owner
for V3). Lifts every rule's Alt/Seq/Repeat nodes to a state in
the counter-DFA. State count bounded by O(rules × alt arity); for
CSS L4 ~1200 states, BBNF ~400, JSON ~25, Sheets ~80 (after
precedence collapse).

The DFA emission uses the existing `ir::dag::extract` traversal
with a new `DtaBuilder` sink replacing `GrammarSink` for the
stage-A path. `GrammarSink` remains for downstream consumers
(bbnf-analysis, gorgeous, bbnf-lsp, bbnf-bootstrap) — that
decoupling landed in AU.4.1.

#### AV.3.2 Counter states for optional-with-lookahead

BBNF `__mapped_factor`'s nested optional, CSS `alphaSep?` in
colour-function arguments, CSS `colorFn` optional `/ alpha`,
Sheets optional `$` cell absoluteness markers. Each is marked in
the IR and compiled to a counter-DFA state that branches on
presence.

#### AV.3.3 Shunting-yard DTA for Sheets precedence

The six-level left-recursive chain (`__formula → __comparison_expr
→ __concat_expr → __add_expr → __mul_expr → __exp_expr →
__unary_expr`) collapses into a single shunting-yard loop in the
DTA. Operator precedence table keyed by byte:

```
byte | op           | precedence | associativity
 '<' | LT           | 3          | L
 '>' | GT           | 3          | L
 '=' | EQ           | 3          | L
 '&' | CONCAT       | 4          | L
 '+' | ADD          | 5          | L
 '-' | SUB          | 5          | L
 '*' | MUL          | 6          | L
 '/' | DIV          | 6          | L
 '^' | EXP          | 7          | R
 ...
```

The DTA emits one `push_compound` per operator that fires — zero
redundant tower-wrappers. The typed materialisation of operator
literals (Bug 1 territory, resolved in V0) puts the operator's u8
discriminant into the compound's payload column.

#### AV.3.4 Diagnostic replay

Recoverable errors require `furthest_offset` tracking. The DTA's
happy path does not backtrack. Diagnostic mode re-enters the same
state machine with an instrumentation hook that tracks the
deepest successful advance and the failing state. One automaton,
two driver modes — no second codegen path.

#### AV.3.5 Eisel-Lemire widening + integer fastpath

`parse_that::scan_number_f64` currently pays ~12% self-time on
canada.json. Two changes:

- **Integer-digit SWAR → SIMD.** The current 8-digit SWAR block
  in `number.rs:134-149` uses scalar chunking. Replace with a
  NEON `vqtbl1q_u8`-based digit classifier + `parse_sixteen_digits`
  path for runs ≥ 16 digits; fallback to SWAR for shorter runs.
- **Eisel-Lemire short-circuit.** For mantissa ≤ 18 digits and
  exponent in-range, the Eisel-Lemire final rounding reduces to
  a single 64-bit multiplication. The current
  `parse_that::parsers::eisel_lemire::compute_f64` does the full
  slow path for every number. Short-circuit the easy case.

Both changes land in `parse_that/rust/parse_that/src/parsers/
scan/`. No grammar changes.

#### AV.3.6 Legacy fn-per-rule deletion

`crates/core/src/grammar/generated.rs` regenerates with the DTA
output. `fn __<rule>` functions no longer exist in the hot path
— a single `parse()` entrypoint drives the DTA over the padded
input. The `__rule_kind()` dispatch used by the IR pipeline is
preserved (separate concern from the parser hot path).

Hard gates:

- `grep -cE 'fn __[a-zA-Z_]+<' crates/core/src/grammar/
  generated.rs` returns 0 outside the prettify path.
- Counter-DFA states for every BBNF/CSS optional-with-lookahead
  rule round-trip through the existing tape-parity fixtures.
- Sheets `parse_simple` ≥ 250 MB/s after shunting-yard collapse.
- canada.json `compute_f64` self-time drops below 4% (from
  11.5% wave-2 baseline).

### Phase 4 — PSI stage-B + stage-C + simdjson decode

Stage A (the DTA) emits records with empty `span_hi` and empty
typed-column positions, plus a `Vec<PayloadJob>` naming every
scalar leaf whose payload needs decoding:

```rust
// crates/bbnf-tape/src/psi.rs (new)
#[repr(C)]
pub struct PayloadJob {
    rec_idx: u32,
    input_lo: u32,
    input_hi: u32,
    kind: PayloadKind,   // 1 byte — f64, u8, bool, hex_u32, string, aggregate_large, ...
    column_idx: u8,       // target column position in the active column set
    _pad: [u8; 2],
}
```

Stage B workers own disjoint `PayloadJob` slices. Stage C closes
`span_hi` and `sib_skip` via a segmented prefix scan over
`frame_depth`.

#### AV.4.1 PSI stream construction

The DTA emits `PayloadJob` inline during stage A at the cost of
one store per scalar leaf. Capacity derived from
`GrammarProfile::leaves_per_input_byte × input.len()`; typically
1–2% of input bytes for scalar-sparse grammars (BBNF, Sheets), up
to 20% for scalar-dense (canada).

#### AV.4.2 Stage-B rayon payload fill

`rayon::par_iter_mut` over PSI chunks. Each worker runs the
terminal scanner for its job's `PayloadKind` and writes the
decoded payload into its assigned column slot. Fingerprint gate:
`GrammarProfile::parallel_break_even_bytes`; below that, stage B
runs single-threaded on the same API (no code-path fork).

Cache-line-aligned chunk stride (4 records per 64 B line) avoids
false sharing when adjacent `rec_idx` values land on the same
line across workers. Per-worker column slot ranges pre-computed
from PSI.

#### AV.4.3 simdjson-scale JSON string decode

`parse-that/rust/parse_that/src/parsers/scan/decode.rs` carries
a scalar UTF-8-aware byte-copy decoder today. simdjson's pattern:

- `_mm256_cmpestri` (x86) / NEON equivalent classifies escape
  bytes (`\`) and quote terminators across a 16/32-byte stripe.
- Escape-free runs copy 16/32 bytes at a time via
  `vst1q_u8` / `_mm256_storeu_si256`.
- Escape-bearing runs fall through to scalar handling.

Target: `decode_json_string_to_arena` runs at arena-memcpy
bandwidth on escape-free inputs (common case for twitter.json,
citm.json, data_*.json).

#### AV.4.4 Stage-C segmented prefix scan

`crates/bbnf-tape/src/finaliser.rs` (new) runs a segmented prefix
scan over `frame_depth` (emitted per record during stage A) to
reconstruct the parent-child structure. `sib_skip` and `span_hi`
close in one linear pass. Parallelisable as a tree-based segmented
scan when measurements justify.

#### AV.4.5 Per-grammar activation

JSON canada, CSS bootstrap/tailwind, BBNF css_l4_grammar: stage B
parallel (corpus size × parse time > 50 µs). Small inputs
(BBNF json.bbnf at 1.8 µs, Sheets parse_simple at 1 ns/byte)
single-threaded — the stage-B API is uniform.

Hard gates:

- Every typed scalar leaf in the target corpus has its decoded
  value in the appropriate column after stage B.
- canada.json parallel stage B shows per-core scaling on a 4-core
  machine (not super-linear — memory bandwidth bound).
- twitter.json `decode_json_string` self-time drops below 5%
  (from 7-19% memchr closure wave-2 baseline for the
  scan_quoted_string-driven path).
- Tape bit-identical to post-V2 column layout for every tape-
  parity fixture.

### Phase 5 — ShapeDictionary (CSS + BBNF)

Compile-time structural dedup via CSP-selected shape templates.
Backed by `EClassFacts.is_fixed_shape` + `RecognizerSignature.
shape_hash` (built in AM/AN, dormant until AV).

#### AV.5.1 TapeKind::ShapeRef variant + cursor expansion

`TapeKind::ShapeRef` joins the enum at slot 13 (AU reserved slot
15 for grammar-specific shapes; slot 13 is available). A record
of this kind carries:

```
kinds[i]        = ShapeRef
flags[i]        = shape_dict_idx (low 5 bits) + has_payload (1b)
span_lo[i]      = covered region start
span_hi[i]      = covered region end
sib_skip[i]     = distance to next sibling
pay_agg[rank]   = packed per-instance payload blob
                   (non-constant leaf spans + typed payloads
                    following the shape template's layout)
```

`TapeCursor::children(ShapeRef)` lazily expands via template
lookup. The template declares which cursor child positions are
structural (emit synthetic sub-cursors pointing into the packed
blob) and which resolve to actual tape records (not dedup'd).

#### AV.5.2 ShapeDictMiner IR pass

`crates/ir/src/passes/recognizers/shape_dict.rs` (new) folds
into the single-walk miner substrate. Emits `(NodeId,
ShapeTemplate { skeleton: ENode, leaf_holes: Vec<TypeDesc>,
shape_hash: u64 })` into `MineOutputs`.

#### AV.5.3 Shape-dict CSP constraint

`crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs`
(new) adds variables `x_c ∈ {include, exclude}` per candidate
template with cost `-freq(c) × savings(c) + static_entry_cost`.
Constraint: `Σ include ≤ 256` (u8 dict index, plenty of
headroom).

Frequency estimate: `freq(c) ≈ Π over ancestors of
(1/alt_arity) × (repeat_unbounded ? 1.0 : avg_count)`. Refine-
able from saved `.profiles/samply/<bench>/<entry>/profile.json.
syms.json` as post-landing calibration.

#### AV.5.4 DTA emits ShapeRef on match

The DTA at stage A checks each rule body against the grammar's
dictionary via `shape_hash` comparison during counter-DFA
transition. On match, emit a single `ShapeRef` record plus pack
the non-constant leaf payloads into `pay_agg`. On no match, emit
the normal per-rule skeleton.

#### AV.5.5 CSS bootstrap validation

Bootstrap.css target shape: `declaration = propertyName :
value ;`. On the fixture, 5000+ declaration subtrees collapse
into ShapeRef leaves. Per-declaration tape records drop from
~5–7 to 1 (ShapeRef) + the packed payload blob (24–40 B).

#### AV.5.6 BBNF big_comment + mapped_factor

BBNF `__big_comment` (9–15% self-time wave-2): single-hole
template, one-record collapse from the current three-record
`Rule → Repeat → Span` wrap. Subsumes AU.6.9.

BBNF `__mapped_factor` with empty `->` branch: the common case
where no mapping appears (observed ~40% of factor calls per
profiling-2.md). Template captures the `__factor` tape offset;
payload blob is empty.

Hard gates:

- Bootstrap.css parses with `declaration` subtrees as ShapeRef
  leaves; view-layer iteration returns semantically identical
  typed declarations (equivalence test against lightningcss's
  `DeclarationBlock` iteration).
- Bootstrap.css bench ≥ 700 MB/s.
- BBNF self-hosting benches ≥ +20% from post-V4 baseline.
- BBNF `__big_comment` self-time < 3% across all six self-
  hosting entries.

### Phase 6 — Runtime content-addressed dedup (bloom + GADT)

Layered over the DTA's stage-A emit. **Not optional.** Every
grammar using the DTA gets runtime dedup on dedup-eligible
rules. Activation is fingerprint-driven at codegen time.

`GrammarProfile::dedup_eligible_rules` names rules where:

- the rule's declared payload type is `()` (`Unit`) or `Span`,
- no child is a `Span`-returning scalar leaf with instance
  variance (i.e., string contents differ between occurrences),
- the rule is Alt- or Seq-bodied with ≥ 1 nested compound.

For CSS: `compoundSelector`, `identifier`, `namedColor`-wrap,
`dimension`-wrap, fixed unit suffixes. For JSON: `null`,
`true`-branch (post-V0 Bug 1 fix), `emptyObject`, `emptyArray`.
For BBNF: every literal-only Alt branch, every `@`-directive
marker. For Sheets: the six pass-through precedence compounds
(collapsed post-AV.3.3 to zero emissions; the dedup-eligible
list contains them for grammars that retain the tower).

#### AV.6.1 GADT + bloom construction

```
Tape (unchanged SoA):   Columns + Payloads
— new —
Gadt:      FxHashMap<(rule_id: u16, body_hash: u64), TapeOffset>
Bloom:     [u64; 8192]           // 64 KiB, sized to input.len()/256
                                  // at parse init; doubled on saturation
SharedBits: BitVec                // 1 bit per record; flipped when
                                  // referenced from two places
```

The bloom filter is the admission gate. On each compound-emit for
a dedup-eligible rule:

```rust
let h = hash_children_tail(&records[start..end], rule_id);
let (bloom_word, bloom_bit) = bloom_index(h);
if (bloom[bloom_word] & bloom_bit) != 0 {
    if let Some(&existing) = gadt.get(&(rule_id, h)) {
        // memcmp on hit to rule out hash collisions
        if columns_range_eq(existing, start, end) {
            columns.truncate(start);
            push_compound_referring(rule_id, existing, span);
            shared_bits.set(existing);
            return;
        }
    }
}
bloom[bloom_word] |= bloom_bit;
let off = push_compound_normal(...);
gadt.insert((rule_id, h), off);
```

`hash_children_tail` is a 64-bit rolling FNV over the raw column
bytes of the child records (`unsafe { slice::from_raw_parts }`
over the kinds + spans + sib_skip ranges). Ignores span_lo/span_hi
for structural rules (two `border: 0` declarations at different
file positions are structurally identical); the rule's dedup
eligibility classification decides.

#### AV.6.2 IR pass for dedup eligibility

`crates/ir/src/passes/recognizers/dedup_eligibility.rs` (new)
classifies each rule. The classifier uses existing IR facts:
`TypeDesc`, `EClassFacts.closure_free`, `EClassFacts.all_
descendants_elidable`. No new IR data.

#### AV.6.3 Fingerprint-driven activation

`GrammarProfile::dedup_eligible_rules` empty ⇒ emitter elides the
GADT/Bloom scaffolding entirely for that grammar. Non-empty ⇒
scaffolding generates. JSON canada has 3 eligible rules
(`null`, `true`-branch, structural `[number]` wrapper); steady-
state overhead ≈ 1 ns per compound emit × ~4 compounds per
record-run ≈ 0.05% net cost.

#### AV.6.4 Cursor compatibility

`child_off < self.offset` invariant preserved: dedup points only
at records written earlier, never forward. `count_backward` and
`nth_backward` still terminate. `walk_shared_subtree` (new
cursor method) descends into a deduped subtree and returns a
self-consistent pre-order view identical to if the DTA had
written the subtree fresh.

Stacks with Phase 5: ShapeDictionary handles the compile-time
high-frequency templates; bloom+GADT handles the runtime long
tail of instance dedup that CSP's 256-entry budget rejected.
Bootstrap.css net: +ShapeDict reduces 280k records to ~180k;
+bloom+GADT reduces further to ~150k (5–15% additional
reduction, dataset-dependent).

Hard gates:

- Runtime dedup activation is driven entirely by
  `GrammarProfile::dedup_eligible_rules`; grammar source files
  carry no dedup-related annotations.
- canada.json (zero sharing opportunity): steady-state bloom-
  AND overhead < 2% of parse time.
- bootstrap.css: combined ShapeDict + bloom+GADT reduction ≥
  30% tape records vs. post-V4 column baseline; bench ≥ 800 MB/s.
- tailwind.css: combined reduction ≥ 25% tape records; bench ≥
  900 MB/s cold single-threaded (before Phase 7 parallelism).

### Phase 7 — Document-level parallel parse

Structural mining identifies list-rules at document scope; the
DTA emits fork points at these rules; parallel workers parse
chunks independently; tape chunks concatenate at join with
offset remap.

#### AV.7.1 List-rule identification

`crates/ir/src/passes/recognizers/list_rules.rs` (new). A rule
is a fork candidate iff:

- body is a Repeat over an Alt or a single compound rule,
- children carry no cross-item state (first-set check over
  all alternatives),
- each item's byte extent is bounded by a structural-bitmap
  position (every item starts at a known byte class).

Candidates emitted to `GrammarProfile::list_rules`. Targets:
CSS `stylesheet = (ruleset | at_rule)*`, JSON root `value`
when it's an array or object, BBNF `grammar = rule+`, Sheets
`file = formula_line*`.

#### AV.7.2 Chunk boundary detection

The stage-1 structural bitmap marks every ruleset /
array-element / rule / formula-line boundary; workers take
contiguous bitmap regions. Boundaries align to the structural
alphabet, not byte count — preserves parser correctness across
boundaries.

#### AV.7.3 Tape offset remap at join

Each worker writes into a local `Columns` instance; the join
phase concatenates (memcpy each column in order) and rewrites
all `sib_skip` cross-worker references by the worker's
contribution offset. One linear pass per column.

#### AV.7.4 Fingerprint-gated activation

`parallel_break_even_bytes` per grammar: Tailwind.css (~3 MB)
forks; bootstrap.css (~200 KB) may or may not (empirical break-
even around 4 cores × 50 KB per chunk). Small inputs stay
single-threaded on the same DTA — no code-path fork.

Hard gates:

- Tailwind.css cold parse ≥ 1.2 GB/s on a 4-core machine.
- JSON canada ≥ 1800 MB/s on a 4-core machine (stacks with
  Phase 3 number scanner + Phase 4 stage-B parallelism).
- No regression on small inputs; fingerprint gate guarantees it
  (documented in V7 per-entry PROGRESS).

### Phase 8 — SIMD keyword dispatch + PHF + selector classifier

Grammar keyword sets reach the emitter through
`GrammarProfile::keyword_tables`.

#### AV.8.1 Perfect-hash tables

- CSS `namedColor` (148 entries) → `phf::OrderedMap<&'static
  [u8], u32>`. Emitted as `const NAMED_COLOR_PHF: phf::Map = …`
  in the grammar's `generated.rs`. ~2 KiB; fits in L1. One
  lookup replaces the 148-branch linear Alt chain.
- CSS `*Keyword` rules (positionKeyword, overflowKeyword, etc.)
  → PHF each.
- Sheets function names → PHF.

#### AV.8.2 SIMD-wide keyword compare for ≤16 keyword alts

CSS `colorType` (9 entries: "oklch" / "oklab" / "rgba" / "rgb" /
"hsla" / "hsl" / "hwb" / "lab" / "lch"). Pack all 9 keywords
into one 128-bit NEON register (9 × 8-byte slots, padded). One
parallel 8-byte-lane compare emits a match bitmask;
`trailing_zeros` picks the branch index. Hits the typed u8
discriminant via the post-V0 Bug-1 fixed per-branch payload
emission.

Same pattern for BBNF `__directive` (8 entries:
"@import" / "@recover" / "@pretty" / "@ws" / "@token" /
"@debug" / "@host" / "@extern").

#### AV.8.3 CSS selector classifier over structural bitmap

The AU.2.7 structural bitmap names every structural character
in the input. A selector classifier on top of it reads the
bitmap's positions and dispatches the selector's classified
type (`.class`, `#id`, `tag`, `[attr]`, `:pseudo`, `::elem`,
`>combinator`) in one pass. Replaces the compound-heavy
byte-level alt dispatch in `__compoundSelector` (33–43% self-
time wave-2).

Classifier lookup table: 256-entry byte-to-selector-kind LUT,
consumed by the DTA at compound-selector positions.
`__compoundSelector` self-time drops below 15%.

#### AV.8.4 Integration with AV.3 DTA

The DTA's Alt dispatch consumes PHF / SIMD-compare tables
directly as its branch-selection primitive. The emitter's
keyword-dispatch code path replaces the branchless-match
cascade with a single PHF lookup or SIMD compare. Hand-written
grammar-specific constants deleted per invariant 5.

Hard gates:

- CSS `__compoundSelector` self-time < 15% on bootstrap.css and
  tailwind.css.
- `grep -rn 'const [A-Z_]*: \[&\[u8\]' crates/core/src/
  backend/rust/emitter/` returns 0; all keyword tables route
  through PHF or SIMD compare emitted from GrammarProfile.
- CSS bootstrap ≥ 900 MB/s; tailwind ≥ 1.4 GB/s.

### Phase 9 — Walker + reader migration closure

Closes the `#[ignore]`-marked tests AU deferred, migrates the
last reader call sites to the columnar substrate, and ensures
the workspace is green before V10 dispatches.

#### AV.9.1 variant_idx walker coherence

AU's 7 ignored JSON variant-dispatch tests (`json_parity`)
assume AoS + payload_idx semantics that the post-V2 columnar
substrate replaces. Walker migration updates the dispatch surface;
tests un-ignore and pass.

#### AV.9.2 Percentage InlineScalar reader migration

AU's 3 ignored CSS percentage tests rely on a
`payload_aggregate` read path that the columnar substrate
replaces with `payload_inline_scalar`. Mechanical reader
migration.

#### AV.9.3 Pre-existing test triage close

The 23 Session-1 pre-existing failures (per V0's Category B
auto-resolved and Category A documented): any residual Category B
fixes land here. Category C (auto-resolved by Bug 1 / Bug 2)
confirmed green. Category A stays `#[ignore]` with tickets.

Hard gate: `cargo test --workspace` passes with 0 failures and
`#[ignore]` count matches the documented Category A list in
PROGRESS.

### Phase 10 — Tranche completion

No code changes. Full bench re-run across the four parse-benches
(`json_monolithic`, `css_l4`, `google_sheets_monolithic`,
`bbnf_monolithic`). `post-AV.json` written; `FINAL.md` composed
per the `docs/instructions/README.md` completion requirements;
workspace test confirmation.

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/backend/rust/emitter/grammar.rs` (Bug 1 + Bug 2 emitter fixes) | 0 |
| `crates/core/src/backend/rust/emitter/alt.rs` (alt-lit per-branch payload) | 0 |
| `crates/ir/src/passes/transform/` (named-color factor-pass) | 0 |
| `parse-that/rust/parse_that/src/parsers/scan/number*.rs` (Bug 2b + AV.3.5) | 0, 3 |
| `parse-that/rust/parse_that/src/parsers/scan/decode.rs` (simdjson decode) | 4 |
| `parse-that/rust/parse_that/src/state.rs` (`PaddedView` construction) | 0 |
| `crates/bbnf-tape/src/builder.rs` (empty-compound NONE fix; stage A/B/C split) | 0, 3, 4 |
| `crates/bbnf-tape/src/columns.rs` (new — SoA substrate) | 2 |
| `crates/bbnf-tape/src/cursor.rs` (column-indexed walker) | 2 |
| `crates/bbnf-tape/src/profile.rs` (new — GrammarProfile runtime face) | 1 |
| `crates/bbnf-tape/src/psi.rs` (new — PayloadJob stream) | 4 |
| `crates/bbnf-tape/src/finaliser.rs` (new — stage-C prefix scan) | 4 |
| `crates/bbnf-tape/src/kind.rs` (TapeKind::ShapeRef slot 13) | 5 |
| `crates/bbnf-tape/src/dedup.rs` (new — bloom + GADT) | 6 |
| `crates/core/src/backend/rust/emitter/profile.rs` (new — emit GRAMMAR_PROFILE) | 1 |
| `crates/core/src/backend/rust/emitter/dta.rs` (new — DTA synthesis, V3 single owner) | 3 |
| `crates/core/src/backend/rust/emitter/visitor.rs` (new — reordered-unrolling codegen) | 2 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (new — PHF + SIMD compare) | 8 |
| `crates/core/src/backend/rust/emitter/selector_classifier.rs` (new — CSS) | 8 |
| `crates/core/src/backend/rust/view/` (column-indexed accessors) | 2, 9 |
| `crates/core/src/grammar/generated.rs` (DTA replaces fn-per-rule) | 3 |
| `crates/ir/src/passes/recognizers/shape_dict.rs` (new) | 5 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (new) | 7 |
| `crates/ir/src/passes/recognizers/dedup_eligibility.rs` (new) | 6 |
| `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs` (new) | 5 |
| `grammar/css/l4/color.bbnf` (colour-function aggregates) | 0 |
| `crates/core/tests/{json,css_l4,bbnf,sheets}_parity.rs` (assertion flips) | 0, 9 |
| `crates/core/tests/fixtures/tape_golden/` (regen for columnar + ShapeRef) | 2, 4, 5 |
| `docs/tranches/AV/{PROGRESS,FINAL}.md` + `docs/benchmarks/post-AV.json` | 10 |

## Hard gates summary

### V0 — Typed-materialisation completion

1. Bug 1 fixed; every alt-lit branch emits its payload write.
2. Bug 2 fixed; `-> Span` routes to `push_leaf_with(PayloadData::Aggregate)` with an 8-byte `(u32 lo, u32 hi)` layout.
3. Bug 2b fixed; `int_lit -> i64`, `float_lit -> f64` reach the tape at runtime.
4. All 148 namedColor branches fire at runtime.
5. Colour-function / colorFn / colorMix round-trip to lightningcss-equivalent typed values.
6. Empty-compound `push_compound` writes `TapeOffset::NONE` on empty children run.
7. `cargo test --workspace --no-fail-fast` reports 0 failures (ignored count matches Category A triage).
8. Padded-input kernel opt-in cascade complete; no per-chunk bounds guards in consolidated SIMD inner loops.

### V1 — GrammarProfile

9. `const GRAMMAR_PROFILE: GrammarProfile` emitted into `generated.rs` for every grammar.
10. Every per-grammar emitter constant has moved into `GRAMMAR_PROFILE`.

### V2 — Columnar substrate

11. `Vec<TapeRec>` does not exist in the codebase.
12. `sum-all-f64(canada)` on the columnar path ≥ 6× speedup over pre-AV AoS path.
13. Every tape-parity fixture passes after regeneration.
14. sonic-rs JSON-value parity + lightningcss CSS AST equivalence tests pass on the columnar walker.

### V3 — DTA

15. `fn __<rule>` monolithic recursion deleted from hot path.
16. Counter-DFA states for every optional-with-lookahead rule round-trip.
17. Sheets `parse_simple` ≥ 250 MB/s.
18. canada.json `compute_f64` self-time < 4%.

### V4 — PSI

19. Stage-B rayon activates per fingerprint gate.
20. twitter.json `decode_json_string` self-time < 5%.
21. Tape bit-identical to post-V2 column layout for every parity fixture.

### V5 — ShapeDictionary

22. Bootstrap.css declaration subtrees emit as ShapeRef leaves; view-layer iteration parity against lightningcss's DeclarationBlock iteration.
23. Bootstrap.css ≥ 700 MB/s single-threaded.
24. BBNF `__big_comment` self-time < 3%; BBNF self-hosting benches +20%.

### V6 — Runtime dedup

25. Runtime dedup activation driven entirely by `GrammarProfile::dedup_eligible_rules`.
26. canada.json bloom-AND steady-state overhead < 2%.
27. Bootstrap.css combined (ShapeDict + bloom+GADT) tape record reduction ≥ 30% vs. post-V4.
28. Bootstrap.css ≥ 800 MB/s; tailwind.css ≥ 900 MB/s single-threaded.

### V7 — Parallel parse

29. Tailwind.css ≥ 1.2 GB/s on 4 cores.
30. JSON canada ≥ 1800 MB/s on 4 cores.
31. No small-input regression (BBNF json.bbnf, Sheets parse_simple, JSON data_s).

### V8 — SIMD keyword dispatch

32. CSS `__compoundSelector` self-time < 15% on bootstrap + tailwind.
33. `grep -rn 'const [A-Z_]*: \[&\[u8\]' crates/core/src/backend/rust/emitter/` returns 0.
34. CSS bootstrap ≥ 900 MB/s single-threaded; tailwind ≥ 1.4 GB/s on 4 cores.

### V9 — Closure

35. `cargo test --workspace` passes with 0 failures.
36. The `#[ignore]` count matches the Category A triage documented in V0.

### V10 — Completion

37. `docs/tranches/AV/FINAL.md` exists per completion requirements.
38. `docs/benchmarks/post-AV.json` exists covering the four parse-benches.
39. Every AV-scope invariant (inherited + new) holds in the final state with artefact citation.

### Cross-tranche parity

40. **sonic-rs parity.** The `json_monolithic_value` bench (AU.3.2) shows bbnf/sonic ratio ≥ 0.95 on canada, ≥ 0.85 on twitter / data_s / citm / data_xl. Honest materialised path (strings decoded, full Value tree).
41. **lightningcss parity.** A per-declaration equivalence test harness (new, lands in V5) parses every declaration from bootstrap.css + tailwind.css with both lightningcss and bbnf; asserts typed-AST equivalence for every declaration. Zero divergences permitted.

## Deferred-item fold-in from AU FINAL.md §4

Every AU deferred item is folded in above. Mapping:

| AU deferred item | AV phase |
|------------------|----------|
| Named struct view codegen (AT.6.2) | AV.0.5 colour-function aggregates + AV.6.2 BBNF struct-shaped rules |
| NEON fractional scan (AR.8.1) | AV.3.5 integer-fastpath + Eisel-Lemire short-circuit |
| Bug 1 — Alt-payload first-branch loss | AV.0.1 |
| Bug 2 — `-> Span` lowers to push_compound | AV.0.2 |
| Bug 2b — scanner-to-payload i64/f64 threading | AV.0.3 |
| Named-color factor-pass payload loss (35/148) | AV.0.4 |
| Colour-function aggregates ≥ 33 B | AV.0.5 |
| CSS bootstrap 454 → 600 MB/s | AV.8.3 selector classifier, AV.5 ShapeDict, AV.6 runtime dedup, AV.7 parallelism |
| JSON canada 1231 → 1800 MB/s decoded | AV.3.5 Eisel-Lemire + AV.4.3 simdjson decode + AV.7 parallel |
| Sheets parse_simple 95 → 200 MB/s | AV.3.3 shunting-yard DTA |
| SoA columnar substrate | AV.2 with AV.2.5 reordered-unrolling codegen |
| Padded-input kernel opt-in cascade | AV.0.7 |
| `variant_idx` dispatch walker coherence | AV.9.1 |
| CSS percentage InlineScalar reader migration | AV.9.2 |
| Empty-compound `has_payload` API quirk | AV.0.6 |
| CSS tape_parity goldens stale (4 tests) | AV.0.8 |
| Sheets_parity Bug-1 pinned assertions (5 tests) | AV.0.1 hard gate |
| Session-1 '18 tests fail' family (23 tests) | AV.0.11 triage |
| `test_selective_transitive_unfurling` | AV.0.12 deferred with ticket |

## Research artefacts

`docs/tranches/AV/research/` carries the six verbatim
architecture deliverables that seeded AV:

- `01-simd-structural-bitmap.md` — AU.2.7 v2 (scanner-only) + AV.3 stage-1 primitive.
- `02-fdmp-cache-locality.md` — GrammarProfile + per-grammar capacity closures + mimalloc segment rounding.
- `03-shape-dictionary-csp-egraph.md` — AV.5 compile-time shape templates.
- `04-columnar-soa.md` — AV.2 columnar substrate + AV.2.5 reordered-unrolling codegen.
- `05-parse-dag-bloom.md` — AV.6 runtime bloom+GADT dedup (now mandatory, not gated).
- `06-psi-dta-parallelism.md` — AV.3 DTA + AV.4 PSI stage-B/C + AV.7 document-level parallel parse.

Each contains ISA-level, bit-layout-level, or algorithm-level
detail that the phases above reference but do not reproduce.
