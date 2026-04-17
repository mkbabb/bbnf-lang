# Tranche AW-IV — Optimisation and Parity

AW-IV activates every substrate channel AV emitted that AW-I
did not consume: PSI rayon stage-B, ShapeRef runtime
dispatch, PHF + SIMD keyword tables, CSS selector
classifier, scanner PaddedView migration, document-level
parallel parse over chunkable list rules, bloom + GADT
runtime dedup, Pratt generalisation from Sheets to CSS value
expressions and BBNF binary tower, sonic-rs + lightningcss
parity harnesses, the `Tape::reduce_column<C, R>` visitor
API, and bench parity confirmation against the post-AU
baseline.

AW-I delivered the DTA-primary parse path; AW-IV extracts
the performance AV's substrate promised. Every lever here
integrates with the walker's compound-emit / Alt / list
branches; no parallel subsystems.

## Architectural thesis

The DTA walker is the integration point for every AW-IV
lever. ShapeRef dispatch consults `SHAPE_DICT` inside the
walker's compound-emit branch. Keyword dispatch (PHF + SIMD)
replaces the walker's `AltLinear` / `ByteDispatch` arms when
the Alt is a dense keyword set. The selector classifier
over the structural bitmap feeds the walker's dispatch for
CSS compound selectors. List-rule parallel parse forks the
DTA walker across worker threads on structural-bitmap chunk
boundaries. Bloom + GADT dedup layers over the walker's
`push_compound`. Pratt generalisation promotes AW-I.W2.1's
`ShuntingYard` arm to CSS `calc`/`min`/`max`/`clamp` and
BBNF `value_or`…`value_unary` tower.

Parity harnesses (sonic-rs, lightningcss) run the DTA-primary
`parse()` end-to-end and compare typed-AST projections
node-for-node. Visitor API lands as a Rust-side method on
the `Tape`, codegen-specialised per active payload column
per grammar.

## Invariants

1. **One path** — inherited from AW-I. AW-IV does not
   introduce parallel parse surfaces, feature flags, or
   dual-path builds. Document-level parallel parse (W3
   chunk fork) is orthogonal — multiple DTA walkers run
   the same entry, not different entries.
2. **Every substrate AV emitted has an active consumer
   at AW-IV close.** `SHAPE_DICT` drives `push_shape_ref`
   (W1). Keyword tables resolve via PHF / SIMD compare
   (W2). `dedup_eligible_rules` drives bloom + GADT (W3).
   `parallel_break_even_bytes` per-grammar-calibrated (W3).
   The visitor reordered-unrolling kernels reach end
   users via `Tape::reduce_column<C, R>` (W5).
3. **Typed-AST parity is total.** sonic-rs harness on
   canada / twitter / citm / data / data_xl: zero
   divergences. lightningcss harness on bootstrap /
   tailwind / normalize: zero divergences. Color view
   field-for-field equivalence under W4 parity harness.
4. **Bench-between-waves structural.** Each wave closes
   with a bench checkpoint (`post-AW-IV-W{N}.json`); W6
   composes the multi-wave aggregator `post-AW-IV.json`.
   A wave whose checkpoint regresses against the prior
   without a written rationale reopens.
5. **Workspace green at every wave boundary.** Snapshot
   migration lands in AW-I.W4.5; AW-IV inherits workspace-
   green and preserves it across every wave.

## Wave schedule (refined 2026-04-17 to exceed recursive descent)

**Re-scoped after AW-III SYNTHESIS.md + structural fold-in directive.**
AW-III absorbs every STRUCTURAL item from the original AW-IV plan:
Pratt `IrNode::Next` peel, scanner closure, ShapeRef consumer, PHF
keyword tables, fused push_compound, selector classifier, PSI rayon
CALIBRATION (constants only), **codegen-specialised per-grammar walkers**,
**direct-to-struct expansion** (JSON + BBNF), and per-grammar Pratt
const-fold. AW-III definitively answers the DTA viability question by
reaching **geomean within 2× of post-AU on ALL 19 entries** (including
sheets, which the codegen-specialisation closes).

**AW-IV is now the granular-optimisation tranche.** Arch-gated SIMD
widening, scanner PaddedView migration, bloom + GADT dedup,
document-parallel fork, reduce_column visitor API, SIMD 4-lane column
pack, and sonic-rs / lightningcss parity harnesses. These are
refinements that layer over AW-III's 2× envelope to achieve the
**EXCEED post-AU** goal — implementation refinements, not new
architectural levers.

| Wave | Scope | Agents | Bench gate |
|------|-------|--------|------------|
| W1 | PSI rayon stage-B walker integration (fills, lock-free workers) — layered over AW-III's calibration + ShapeRef tail (patterns AW-III deferred) + Bug 2b residuals | 3 parallel | canada 4c per-core scales; bootstrap ≥ 700 MB/s |
| W2 | PHF frequency-ordering + length-bucket tail (refinement over AW-III's PHF) + SIMD u8x32 AVX2 widening + scanner PaddedView migration + scanner-cluster closure | 4 parallel | bootstrap ≥ 900 MB/s; tailwind 4c ≥ 1.4 GB/s |
| W3 | Document-level parallel parse fork + bloom + GADT dedup + cost-model grid | 4 parallel | tailwind 4c ≥ 1.2 GB/s; canada 4c ≥ 1800 MB/s |
| W4 | Walker + reader migration + sonic-rs + lightningcss parity harnesses | 3 parallel | parity harnesses green |
| W5 | reduce_column<C,R> visitor API + SIMD 4-lane column pack + bench parity confirmation | 3 parallel | **every entry EXCEEDS post-AU**; parity harness green |
| W6 | FINAL + close | 1 serial | post-AW-IV.json composed; geomean > 1.0× post-AU |

## Phases

### W1 — PSI rayon + ShapeRef + Bug 2b residuals [3 parallel]

#### W1.1 PSI rayon stage-B

Owner: `crates/bbnf-tape/src/psi.rs`,
`crates/core/src/backend/rust/emitter/profile.rs`.

Calibrate `parallel_break_even_bytes` per grammar from
samply profiles on representative inputs. JSON canada
~50 KiB break-even; CSS bootstrap ~100 KiB; tailwind
~50 KiB; Sheets stays sequential; BBNF self stays
sequential. PSI `fill_columns` forks rayon workers when
input bytes ≥ per-grammar threshold. Workers write into
pre-resized columns at distinct indices — lock-free per
the psi.rs:54-65 design note; verify under stress with a
4-core parallel-fill tortured input.

Hard gate: canada shows per-core scaling on a 4-core
machine for inputs ≥ break-even; zero torn writes under
stress.

#### W1.2 ShapeRef runtime dispatch

Owner: `crates/bbnf-tape/src/driver.rs` compound-emit
branch; `crates/bbnf-tape/src/shape_dict.rs`;
`crates/core/src/backend/rust/emitter/{grammar,profile}.rs`
for `active_columns` + `shape_dict` population.

Per research/02: strict-injective compile-time collision
assertion over `SHAPE_DICT.shape_hash`. The emitter
verifies at `emit_shape_dict_arrays` time that every
emitted hash is unique per grammar; fails compilation
otherwise. Runtime walker consults `SHAPE_DICT` in the
compound-emit branch via a single indexed load + equality
compare, no `columns_range_eq` confirm. On hit:
`push_shape_ref(span, dict_idx, packed_payload)` replaces
the compound-run. On miss: normal compound emit.

View-layer `ShapeRefSyntheticChild` cursor expansion
already landed (AV.5.1). W1.2 verifies parity via a new
`crates/core/tests/shape_ref_view_parity.rs` — walks every
CSS L4 declaration in `bootstrap.css`, emits once with
dispatch enabled and once with a per-grammar flag
disabling it, asserts byte-identical typed-AST
projections.

Populate `GRAMMAR_PROFILE.active_columns` +
`GRAMMAR_PROFILE.shape_dict` from the emit-time mined
dict. AW.0.9 ledger closes the `active_columns` +
`shape_dict` slots.

Hard gate: bootstrap.css declaration record count drops
≥ 30% vs W1.1 baseline; `shape_ref_view_parity` test
passes; `GRAMMAR_PROFILE.shape_dict` non-empty for CSS L4.

#### W1.3 Bug 2b residuals

Owner: `crates/ir/src/passes/payload/layout.rs`,
`crates/core/src/backend/rust/emitter/{dispatch,grammar}.rs`
(via the regenerated emitter surface post-AW-I.W4).

Three items deferred from AV V0:

- **`pinned_number_drops_f64_payload`** (Sheets
  `number -> f64`). Map-bodied regex rule needs admission
  to the layout pass. Extend `scalar_layout_eligible` to
  admit Map-bodied rules whose body is a regex match
  producing a typed scalar payload — F64 / I64 / U64 /
  Bool / U8.
- **Sheets `boolean` FALSE branch drops `0u8`.** Dispatch
  composer today requires literal-branch Alts.
  `boolean` uses regex-branch (`/TRUE/i`, `/FALSE/i`).
  Extend dispatch composer to admit
  `Map { Regex, BoolLit }` branches.
- **3 CSS percentage InlineScalar reader tests** un-
  ignore. `payload_u8` reader call sites in
  `crates/core/tests/css_l4_parity.rs` flip from
  `#[ignore]` to active.

Hard gate: every pinned_*_drops_payload test flips;
3 percentage tests un-ignore; parity suites green.

### W2 — PHF + SIMD keyword + selector classifier + scanner closure [4 parallel]

#### W2.1 PHF + frequency ordering + length-bucket tail

Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs`
(new); `crates/core/src/backend/rust/emitter/grammar.rs`;
`crates/ir/src/passes/recognizers/keyword_stats.rs` (new —
frequency mining).

Emit `pub const NAMED_COLOR_PHF: phf::OrderedMap<&'static
[u8], u32> = phf_map! { ... };` for CSS `namedColor` (148
entries). Same for CSS `*Keyword` rules and Sheets function
names (~150 entries). Walker's `AltLinear` arm consults the
PHF directly — one PHF lookup instead of 148-branch linear
scan or byte-dispatch.

**AO.5.3 frequency-ordered dispatch** (chronic since AO):
emit `FREQUENCY_ORDER: &[u32]` alongside PHF where profiling
evidence identifies hot keywords. The walker's
byte-dispatched miss path consults frequency order before
PHF lookup for ≤ 8 hot keywords. Frequency comes from
samply self-time attribution over a representative corpus
per grammar.

**AQ.7.3 generalised length-bucket PHF tail** (chronic since
AQ): the PHF for variable-length dense keywords subdivides
by byte length before hashing. `NAMED_COLOR_PHF` stays one
table (single byte length ranges fit cleanly); Sheets
function names add a length-bucket prefix — `[bucket_6,
bucket_7, bucket_8, …]: &[Phf]` indexed by
`[input_length.saturating_sub(MIN).min(MAX - MIN)]`. Dense
tail for short names; sparse tail for long.

White-colour `0xFFFFFFFFu32` collision routed to WideScalar
in AW-I.W0.8; PHF stays homogeneous.

Hard gate: `grep -rn 'const [A-Z_]*: \[&\[u8\]'
crates/core/src/backend/rust/emitter/` returns 0 (every
keyword table PHF-routed). Samply delta on
`Sheets::__function_name` self-time ≥ 30% from frequency +
length-bucket combined vs the AW-I post-W1 baseline.

#### W2.2 SIMD keyword compare + AVX2 u8x32 widening

Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs`;
`crates/bbnf-tape/src/driver.rs` (walker arm integration);
`crates/core/src/generate/regex/emit/simd.rs` (x86_64
widening).

CSS `colorType` (9 entries): pack into one 128-bit NEON
register (9 × 8-byte lanes, padded). One parallel 8-byte-
lane compare emits a match bitmask; `trailing_zeros` picks
branch index. Typed u8 discriminant flows through per-
branch Bug-1 payload emission (AW-I.W2.1 carry-forward).

Same pattern for BBNF `__directive` (8 entries). Walker
dispatches to the SIMD compare when Alt is ≤ 16 keywords,
length-bounded, FIRST-set mutually-disjoint.

**AN.5 u8x32 AVX2 widening** (chronic since AN): every
SIMD call site today uses `u8x16`. On x86_64 AVX2, widen
to `u8x32` for scanner structural-byte passes. The walker's
structural-bitmap producer (inherited from AU.2.7) and the
DFA scanner dispatch both consume. Arch-gate via
`#[cfg(target_feature = "avx2")]`; NEON path unchanged.

Hard gate: BBNF `__directive` + CSS `colorType` dispatched
via SIMD compare; `cargo expand` shows the NEON intrinsic
on AArch64 and the AVX2 intrinsic on x86_64. Samply on
canada.json (x86_64 AVX2) shows ≥ 15% reduction in
structural-scan self-time vs u8x16 baseline.

#### W2.3 CSS selector classifier

Owner:
`crates/core/src/backend/rust/emitter/selector_classifier.rs`
(new); `crates/bbnf-tape/src/driver.rs` (compound-selector
dispatch integration).

256-entry byte-to-selector-kind LUT, consumed by the
walker at compound-selector positions. The AU.2.7
structural bitmap names every structural character; the
classifier reads the bitmap's positions and dispatches the
selector's classified type (`.class`, `#id`, `tag`,
`[attr]`, `:pseudo`, `::elem`, `>combinator`) in one pass.
Replaces byte-level alt dispatch in `__compoundSelector`
(33-43% self-time pre-AU).

Hard gate: samply on bootstrap + tailwind shows
`__compoundSelector` self-time < 15%.

#### W2.4 `find_next_structural_from` paired migration

Owner: `crates/core/src/backend/rust/emitter/string_decode.rs`
(in the post-AW-I deletion set; the migration work reifies
the surviving SIMD scanner call sites into the DTA's
scanner interface); `crates/core/src/generate/regex/emit/simd.rs`.

CO-E2's deferred migration: 7 emitter call sites pass
`&state.src_bytes` (unpadded). Migrate to `PaddedView`.
Pair the `scan_quoted_string_simd` /
`decode_json_string_to_arena` migration — both kernels
move to `PaddedView`.

Hard gate: per-chunk SIMD bounds guards in consolidated
loops return 0; `grep -rn 'src_bytes' crates/core/src/`
in scanner paths returns 0.

#### W2.5 Scanner-architecture cluster consolidation + NEON 17-digit

Owner: `parse-that/rust/parse_that/src/{scanners,regex}/`
(orchestrator-landed from main; sibling-repo);
`crates/ir/src/regex_info.rs`.

**Scanner cluster** (chronic since AR.6.x / AS.5.x, six-
tranche deferral): `RegexClassMiner` consolidation into one
canonical miner shared across scanner emission paths;
`ScanLut` registry as a per-grammar resource (replaces the
scattered per-rule LUT emit sites);
`WsCommentConfig` parameterisation (one config struct per
grammar carries whitespace + comment alphabet);
`FnDescriptor` post-pass (unifies function-call descriptor
emission); HIR predicate re-exports collapsed to one
module. Net: ~600 LOC delete + ~350 LOC net reduction per
AR audit.

**AT.4.3 NEON 17-digit fractional scan** (chronic since
AT): AV.3.5 landed Eisel-Lemire + 16-digit integer SIMD
fastpath; the 17-digit fractional kernel specifically
never landed. Hand-written NEON kernel for 17-digit
fractional part — ±1 ULP vs scalar `f64::from_str` on the
canonical corpus.

Hard gate: `parse-that/rust/parse_that/src/scanners/` LOC
drops by ≥ 600 (baseline at W2 open); HIR predicate module
count drops to 1; every scanner call site resolves through
the consolidated miners. `parse-that` f64-parse tests
pass bit-identically on fractional inputs up to 17 digits.

> **§W3 moved to AW-III.W5.6–W5.8** per 2026-04-17 fold-in directive.
> Codegen-specialised per-grammar walkers + direct-to-struct expansion
> (JSON + BBNF) + per-grammar Pratt const-fold are structural items
> that prove viability and belong in the correctness tranche. AW-III.W5
> now delivers the 2× envelope on ALL 19 entries; AW-IV is the
> granular-optimisation tranche atop. See `docs/tranches/AW/AW-III.md`
> §W5 for the specification.

### W3 — Document-level parallel parse + bloom dedup + cost-model grid [4 parallel]

#### W3.1 List-rule identification + chunk fork

Owner: `crates/ir/src/passes/recognizers/list_rules.rs`
(new); `crates/bbnf-tape/src/driver.rs` (fork orchestration).

A rule is a fork candidate iff:
- body is `Repeat` over an `Alt` or single compound rule,
- children carry no cross-item state (first-set check
  over all alternatives),
- each item's byte extent is bounded by a structural-
  bitmap position.

Candidates emitted to `GRAMMAR_PROFILE.list_rules`.
Targets: CSS `stylesheet = (ruleset | at_rule)*`, JSON
root `value` when array/object, BBNF `grammar = rule+`,
Sheets `file = formula_line*`.

Stage-1 structural bitmap marks every item boundary;
workers take contiguous regions. Each worker writes into
a local `Columns`. Join phase memcpy-concatenates columns
in order and rewrites `sib_skip` cross-worker references
by the worker's contribution offset. One linear pass per
column.

Hard gate: tailwind.css on 4 cores shows sub-linear-to-
linear scaling; `GRAMMAR_PROFILE.list_rules` non-empty
for CSS L4.

#### W3.2 Bloom + GADT runtime dedup + grammar-level pattern hoisting

Owner: `crates/bbnf-tape/src/dedup.rs` (new);
`crates/bbnf-tape/src/driver.rs`;
`crates/ir/src/passes/recognizers/dedup_eligibility.rs`
(new); `crates/ir/src/passes/transform/pattern_dedup.rs`
(new — compile-time sibling).

**Runtime bloom + GADT.** Layered over the DTA stage-A
emit per AW.md §Phase 6. Mandatory where
`GRAMMAR_PROFILE.dedup_eligible_rules` is non-empty (CSS
`compoundSelector`, `identifier`, `namedColor`-wrap, fixed
unit suffixes; JSON `null`, `true`-branch, `emptyObject`,
`emptyArray`; BBNF literal-only Alt branches).

64-bit rolling FNV over raw column bytes of child records
(`hash_children_tail`). Span_lo/span_hi ignored for
structural rules. Bloom admission gate; on hit, GADT
lookup → `columns_range_eq` confirms; on confirm,
`push_compound_referring(rule_id, existing, span)` shares
the existing subtree.

`dedup_eligibility` IR pass classifies each rule using
existing IR facts: `TypeDesc`, `EClassFacts.closure_free`,
`EClassFacts.all_descendants_elidable`. Populates
`GRAMMAR_PROFILE.dedup_eligible_rules`.

**AP.4.2 grammar-level pattern dedup** (chronic since AP,
compile-time sibling to runtime bloom): `ws + ':' + ws`
appears 43 times in CSS L4; `!important` appears 42 times
across grammars; similar multi-rule repeats across the
corpus. Compile-time pass identifies recurring sub-patterns
and hoists into synthetic non-terminals. Pre-egraph pass
(runs after `canonicalize_aliases`, before
`factor_common_prefixes`). Bloom handles runtime structural
sharing; pattern hoisting handles compile-time grammar-
level sharing — orthogonal axes.

Hard gate: canada.json (zero-sharing input) bloom-AND
steady-state overhead < 2% of parse time;
bootstrap.css record count drops ≥ 30% vs W2 baseline;
`GRAMMAR_PROFILE.dedup_eligible_rules` non-empty;
grammar-level pattern hoisting synthesizes ≥ 5 non-terminals
on CSS L4 (`ws_colon_ws`, `important_kw`, etc.) with a
DTA state-count reduction ≥ 100.

#### W3.3 Pratt generalisation

Owner: `crates/bbnf-tape/src/driver.rs::ShuntingYard`;
`crates/ir/src/passes/recognizers/operator_chain.rs`
(extension).

AW-I.W2.1 landed the Sheets `ShuntingYard` reducer. W3.3
generalises:

- **CSS value expressions** — `calc(2 * (3 + 4))`,
  `min`, `max`, `clamp`. Operator set: `+`, `-`, `*`, `/`,
  `,` (list separator at precedence 0). Comma-list routes
  elsewhere (W3.1 list-rule recogniser) per research/03;
  `calc` / `min` / `max` fit the Pratt frame directly (two
  rungs for `calc`'s + - vs * /).
- **BBNF binary tower** — `value_or` → `value_and` →
  `value_concat` → `value_unary`. Six rungs per
  research/03. Fits Pratt.

Grammar mining emits per-grammar `PRECEDENCE_LUT: [u8; 256]`
packed as `prec(4b) | assoc(1b) | arity(2b) | two_byte(1b)`,
plus sparse `&'static [DtaPrecedenceEntry]` for second-
byte + op_rule + discriminant. Integration point:
`ShuntingYard`'s peek-next-byte consults the emitted LUT,
not hardcoded per-grammar dispatch.

Healing `test_let_parses_as_let_call` (Sheets dispatch
surface) — the Pratt reducer subsumes the LET/IF/LAMBDA
dispatch. Un-ignore the test.

Hard gate: CSS `calc(2 * (3 + 4))` produces correct AST
shape; BBNF `value_or` tower produces correct
associativity; `test_let_parses_as_let_call` un-ignored
+ passing.

#### W3.4 GrammarProfile calibration + small-input amortisation + ws polish

Owner: `crates/core/src/backend/rust/emitter/profile.rs`;
`crates/bbnf-tape/src/profile.rs`;
`crates/bbnf-tape/src/driver.rs::skip_ws`;
`parse-that/rust/parse_that/src/` (skip_ws bitmap
caching).

**Calibration.** Per-grammar `expected_ns_per_byte`,
`parallel_break_even_bytes`,
`payload_bytes_per_input_byte`, `dta_setup_floor_ns`
measured against the W3 single-threaded matrix. Commit
values as const literals in each emitter's
`GRAMMAR_PROFILE`.

Small-input amortisation (AW.4.7): bench reports compute
expected MB/s for sub-100 µs parses from
`(input.len() × 1e9) / (setup_floor + input.len()
× expected_ns_per_byte)`, report achieved/expected
ratio instead of fixed gate.

**AP.3.2 redundant trim-call elision** (chronic since AP):
fused-scan with `last_trim_offset` memoises the most
recent trim-result so adjacent trim calls short-circuit
when the input position hasn't advanced. Walker's
`skip_ws` consults before scanning.

**AQ.8.1 skip_space bitmap caching** (chronic since AQ):
`nospace_bits: [u8; N]` + `nospace_start: u32` cache
populated on first `skip_ws` call per parse. Subsequent
calls hit the cache for their byte range.

Hard gate: every grammar's `GRAMMAR_PROFILE` const
populated; stub `&[]` slots remain only where
populated-by-design. Samply on bootstrap shows
`skip_ws`/`__ws` self-time drops ≥ 30% vs the AW-I post-
W1 baseline.

#### W3.5 Cost-model grid sweep (AM.6 → five-tranche deferral)

Owner: `crates/egraph/src/cost.rs`;
`crates/bbnf-ir/src/egraph/`;
`scripts/cost-grid-sweep.sh` (new);
`docs/benchmarks/cost-weights-sweep.json` (new).

**Six-tranche chronic** (AM.6 → AO.4.1 → AP.6.4 → AQ.9.4
→ AW ledger): egraph `CostWeights` have been hand-
calibrated since AL. The grid-sweep harness runs
orthogonal to the runtime calibration W3.4 handles —
compile-time IR ranking weights driving cost-aware
extraction.

Harness: for each weight in
`{seq_cost, alt_cost, repeat_cost, literal_bonus,
regex_cost, payload_bonus, ...}` sweep a logarithmic
grid (0.5×, 1×, 2×, 4×) across the 4-grammar corpus.
Measure DTA state count post-extraction + extraction
pass wall-clock. Pick the Pareto frontier per grammar;
pick the dominant weights that minimise state count
across the corpus.

Commit calibrated `CostWeights` as a `pub const` in the
egraph crate. Measure post-calibration delta vs the
baseline CostWeights.

Hard gate: ≥ 5% reduction in DTA state count OR
extraction-pass wall-clock vs the post-AW-I master
baseline on the 4-grammar corpus. If neither moves,
close the item as a null result — the hand-calibrated
CostWeights are the permanent decision with measurement
evidence in `cost-weights-sweep.json`. Either outcome
closes the chronic.

### W4 — Walker + reader migration + parity harnesses [3 parallel]

#### W4.1 variant_idx walker coherence

Owner: `crates/core/src/backend/rust/view/alt.rs`;
`crates/core/tests/json_parity.rs` (un-ignore).

AU's 7 ignored JSON variant-dispatch tests assume AoS +
payload_idx semantics that the V2 columnar substrate
replaced. Cursor's variant_idx accessor reads from
`flags` column, not from the deleted `payload_idx`
field. Mechanical reader migration.

Hard gate: 7 JSON variant-dispatch tests un-ignored +
passing.

#### W4.2 Serialize/structural roundtrip + transitive unfurling

Owner: `crates/core/tests/{json_parity,structural_parity}.rs`;
`crates/core/src/backend/rust/serialize/` (if the emit
path exists); `crates/core/tests/imports.rs` (for
`test_selective_transitive_unfurling`).

13 serialize/structural roundtrip tests un-ignore + fix.
`test_selective_transitive_unfurling` triage: fix the
import-system bug (module loader does not unfurl
transitive deps of selectively-imported rules) or
document as AX-scope if fix is genuinely orthogonal.

Hard gate: 13 tests un-ignored + passing; transitive
unfurling fixed OR rationale in FINAL.

#### W4.3 sonic-rs + lightningcss parity harnesses

Owner: `crates/core/tests/sonic_rs_parity.rs` (new);
`crates/core/tests/lightningcss_parity.rs` (new).

sonic-rs harness: for every JSON file in `data/json/`,
parse with bbnf + sonic-rs, compare `view().as_value()`
vs `sonic_rs::Value` node-for-node. Numbers bit-for-bit
(f64 ULP tolerance). Strings byte-for-byte (escape
decoding). Objects key-set + per-key value equality.
Arrays length + per-index value equality.

lightningcss harness: per-declaration equivalence over
bootstrap.css + tailwind.css + normalize.css. Colors via
W0.5 `Color` projection (field-for-field with
`lightningcss::values::color::Color::RGBA`). Selectors
via tokenised form. f32 ↔ f64 compared via `(f32 as
f64)`. Alpha-less inputs handled per AW-I.W0.5 NaN
discipline.

Both harnesses CI-gate alongside `grammar_roundtrip` +
`tape_parity`.

Hard gate: zero divergences on canada / twitter / citm /
data / data_xl (sonic-rs) and bootstrap / tailwind /
normalize (lightningcss); CI step wired in
`.github/workflows/ci.yml`.

### W5 — Visitor reduce_column + SIMD pack + bench parity [2 parallel]

#### W5.1 `Tape::reduce_column<C, R>` + per-column codegen + SoA 4-lane SIMD pack

Owner: `crates/bbnf-tape/src/columns.rs` (API surface);
`crates/core/src/backend/rust/emitter/visitor.rs`
(codegen specialisations); `crates/core/tests/visitor_reduce.rs`
(new).

**Substrate lineage.** AV.2.5 landed the SoA-substrate's
reordered-unrolling kernel as codegen: `emit_visitor_kernels`
emits, per active f64 payload column, a 4-lane scalar left-
fold-free reducer. The 4 lanes break the strict-IEEE left-
fold dependency chain; LLVM's auto-vectoriser produces four
independent scalar `fadd d*` chains on AArch64. AV measured
3.3× on synthetic `Vec<f64>`. AV's hard gate 12 ("6×
packed SIMD") was partial at AV close — the consumer API
and SIMD promotion never shipped. W5.1 ships both.

```rust
let total: f64 = parsed.tape().reduce_column::<F64Column, _>(
    0.0,
    |acc, x| acc + x,
);
```

Emitter extends `visitor.rs::emit_visitor_kernels` to produce
one `reduce_column<C, R>` impl per active payload column per
grammar, driven by `GRAMMAR_PROFILE.active_columns`. LLVM
monomorphises the reducer at the call site, preserving the
AV.2.5 4-lane scalar reordered-unrolled loop as the inner
body when the reducer matches `|acc, x| acc + x` associatively.

Test surface: one reducer per grammar in `visitor_reduce.rs`.
JSON sum-all-f64 on canada.json. CSS count-all-declarations
on bootstrap.css. BBNF count-all-rules on bbnf_self.bbnf.
Sheets sum-all-cell-refs on stress.txt. Each test matches
the AV.2.5 microbench 3.3× scalar baseline first, then
clears the 6× SIMD-packed gate.

Promote the emitted inner loop to packed `std::simd::f64x4`
(or arch-intrinsic `vfaddq_f64` pairs on NEON,
`_mm256_add_pd` on AVX2). Portable-simd is stable; no
nightly dependence. The scalar 4-lane reordering AV.2.5
delivered is the substrate; SIMD packing completes the 6×
gate on AVX2 and documents the AArch64 per-arch ceiling
(NEON is 2-lane f64-wide; portable f64x4 lowers to 2×
pairs).

Verify on canada.json's f64 column (~6M entries):
```
cargo bench -p bbnf --bench visitor_reduce_simd --
    --measurement-time 10
```

Hard gate: ≥ 6× speedup over AV.2.5-baseline scalar
left-fold on canada.json f64 column, OR per-arch
rationale documenting AArch64 ceiling (NEON is 2-lane
f64-wide; portable_simd f64x4 lowers to 2× pairs; AVX2
reaches 4-lane natively). No grammar surface introduced —
verifying invariant 5.

#### W5.2 Bench parity confirmation

Owner: dedicated bench agent per research/05 protocol.
Read-only on source; writes `docs/benchmarks/post-AW-IV-W5.json`
+ `docs/tranches/AW/PROGRESS.md` paragraph.

Run the four parse-bench matrix cold, sequential,
mimalloc, cwd `crates/core`. Every entry from the
post-AV reality-check table checked against the W6 gate:

| Bench | Entry | W6 gate MB/s |
|-------|-------|--------------|
| json_monolithic | canada | 2000 |
| json_monolithic | twitter | 2400 |
| json_monolithic | data | 2000 |
| json_monolithic | data_xl | 1700 |
| json_monolithic | citm | 3000 |
| css_l4 | bootstrap | 800 |
| css_l4 | normalize | 1100 |
| css_l4 | tailwind | 1200 |
| google_sheets_monolithic | parse_simple | 250 |
| google_sheets_monolithic | parse_nested | 300 |
| google_sheets_monolithic | parse_stress | 300 |
| bbnf_monolithic | bbnf_self | 500 |
| bbnf_monolithic | json | 400 |
| bbnf_monolithic | ebnf | 350 |
| bbnf_monolithic | google_sheets | 1000 |
| bbnf_monolithic | css_pretty | 800 |
| bbnf_monolithic | css_l4_grammar | 650 |

Each entry's delta annotated with the dominant AW-IV
lever (attribution enum per research/05: `psi_rayon`,
`shape_ref`, `phf_keyword`, `simd_compare`,
`selector_classifier`, `scanner_padded`, `parallel_fork`,
`bloom_dedup`, `pratt_lower`, `profile_calibration`,
`visitor_simd_pack`). Samply sidecar mandated on
`__compoundSelector` (W2.3) and `decode_json_string`
(W2.4) self-time gates.

Hard gate: every entry meets its W6 gate OR carries a
written rationale; samply sidecars present for the two
self-time gates.

### W6 — FINAL + close [1 serial]

Compose `docs/tranches/AW/FINAL.md` (final AW FINAL
across AW-I + AW-IV): per-phase recap with commit
hashes; hard-gate status table with artefact citations;
cross-tranche debt reconciled; AX seeds enumerated;
attribution ledger for every bench entry delta.

Compose `docs/benchmarks/post-AW.json` as enriched
`multi_wave_history` map per research/05 — folds
post-AW-I.json + every post-AW-IV-W{N}.json into a
single artefact consumed by downstream tooling.

Confirm `cargo test --workspace --no-fail-fast` 0
failures; ignored count ≤ 14 Category A per the
enumerated set (closure tests, analysis structural-
mode gates, gorgeous fixture tests, pprint-vm hint
tests).

Hard gate: `FINAL.md` exists + enumerates every AW-I +
AW-IV hard gate with artefact citation;
`docs/benchmarks/post-AW.json` covers the 19-entry
matrix with multi-wave history; workspace tests 0
failures.

## Critical files

| File | Wave |
|------|------|
| `crates/bbnf-tape/src/psi.rs` (rayon activation) | W1.1 |
| `crates/bbnf-tape/src/driver.rs` (ShapeRef + keyword + classifier + fork + bloom + Pratt integration) | W1.2, W2.1-3, W3.1-3 |
| `crates/bbnf-tape/src/shape_dict.rs` (strict-injective check) | W1.2 |
| `crates/ir/src/passes/payload/layout.rs` (Map-bodied admission) | W1.3 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (new) | W2.1-2 |
| `crates/ir/src/passes/recognizers/keyword_stats.rs` (new — frequency mining) | W2.1 |
| `crates/core/src/backend/rust/emitter/selector_classifier.rs` (new) | W2.3 |
| `crates/core/src/generate/regex/emit/simd.rs` (PaddedView migration + AVX2 u8x32) | W2.2, W2.4 |
| `parse-that/rust/parse_that/src/{scanners,regex}/` (cluster consolidation, sibling-repo) | W2.5 |
| `crates/ir/src/regex_info.rs` (HIR predicate re-export collapse) | W2.5 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (new) | W3.1 |
| `crates/bbnf-tape/src/dedup.rs` (new) | W3.2 |
| `crates/ir/src/passes/recognizers/dedup_eligibility.rs` (new) | W3.2 |
| `crates/ir/src/passes/transform/pattern_dedup.rs` (new — compile-time hoisting) | W3.2 |
| `crates/ir/src/passes/recognizers/operator_chain.rs` (extension) | W3.3 |
| `crates/core/src/backend/rust/emitter/profile.rs` (calibration) | W3.4 |
| `crates/bbnf-tape/src/driver.rs::skip_ws` (trim elision + bitmap cache) | W3.4 |
| `crates/egraph/src/cost.rs` (CostWeights grid sweep) | W3.5 |
| `scripts/cost-grid-sweep.sh` (new) | W3.5 |
| `docs/benchmarks/cost-weights-sweep.json` (new) | W3.5 |
| `crates/core/src/backend/rust/view/alt.rs` (variant_idx migration) | W4.1 |
| `crates/core/tests/{json_parity,structural_parity,imports}.rs` (un-ignore) | W4.2 |
| `crates/core/tests/{sonic_rs_parity,lightningcss_parity}.rs` (new) | W4.3 |
| `crates/bbnf-tape/src/columns.rs::reduce_column` API | W5.1 |
| `crates/core/src/backend/rust/emitter/visitor.rs` (codegen + SIMD pack) | W5.1 |
| `crates/core/tests/visitor_reduce.rs` (new) | W5.1 |
| `docs/tranches/AW/FINAL.md` (new) | W6 |
| `docs/benchmarks/post-AW.json` (new, aggregator) | W6 |
| `docs/benchmarks/post-AW-IV-W{1..5}.json` (new per wave) | W1-W5 |

## Hard gates summary

### W1
1. canada per-core scaling on 4-core for inputs ≥ break-
   even; zero torn PSI writes under stress.
2. `bootstrap.css` record count drops ≥ 30% from W1.1
   baseline; `shape_ref_view_parity` passes.
3. Sheets pinned_number + boolean FALSE flips; 3
   percentage tests un-ignore.
4. `post-AW-IV-W1.json`: bootstrap ≥ 700 MB/s; twitter
   `decode_json_string` self-time < 5%.

### W2
5. `grep -rn 'const [A-Z_]*: \[&\[u8\]'
   crates/core/src/backend/rust/emitter/` = 0.
6. BBNF `__directive` + CSS `colorType` SIMD-compared;
   NEON intrinsic visible under `cargo expand`.
7. `__compoundSelector` self-time < 15% via samply.
8. `grep -rn 'src_bytes' crates/core/src/` in scanner
   paths = 0.
9. `post-AW-IV-W2.json`: bootstrap ≥ 900 MB/s; tailwind
   4c ≥ 1.4 GB/s.

### W3
10. tailwind.css 4c sub-linear-to-linear scaling;
    `GRAMMAR_PROFILE.list_rules` non-empty for CSS L4.
11. canada bloom-AND steady-state overhead < 2%;
    bootstrap record count drops ≥ 30% vs W2.
12. CSS `calc(2*(3+4))` shape correct; BBNF
    `value_or` associativity correct;
    `test_let_parses_as_let_call` un-ignored + passing.
13. Every grammar's `GRAMMAR_PROFILE` const populated;
    populated-by-design `&[]` documented.
14. `post-AW-IV-W3.json`: tailwind 4c ≥ 1.2 GB/s;
    canada 4c ≥ 1800 MB/s; parse_simple ≥ 250 MB/s.

### W4
15. 7 JSON variant-dispatch tests un-ignored + passing.
16. 13 serialize/structural roundtrip tests un-ignored +
    passing.
17. sonic-rs harness zero divergences on canada /
    twitter / citm / data / data_xl.
18. lightningcss harness zero divergences on bootstrap /
    tailwind / normalize.
19. CI step wired for both parity harnesses.
20. Ignored count ≤ 14 Category A.

### W5
21. `Tape::reduce_column<C, R>` + per-column codegen
    lands; per-grammar reducer test matches V2.5
    microbench ceiling.
22. Visitor SIMD-pack: ≥ 6× scalar left-fold on
    canada.json f64 column OR per-arch rationale.
23. `post-AW-IV.json`: every post-AV reality-check
    entry meets its W6 gate OR rationale;
    samply sidecars present for self-time gates.

### W6
24. `docs/tranches/AW/FINAL.md` exists; AW-I + AW-IV
    hard gates enumerated with artefact citations.
25. `docs/benchmarks/post-AW.json` composed as multi-
    wave history.
26. Workspace `cargo test` 0 failures; ignored ≤ 14
    Category A.

## Cross-tranche parity

27. sonic-rs parity: `json_monolithic_value` bench
    bbnf/sonic ratio ≥ 0.95 on canada, ≥ 0.85 on
    twitter / data / citm / data_xl.
28. lightningcss parity: per-declaration equivalence on
    bootstrap + tailwind + normalize.
29. Named struct ABI: `pub struct Color` matches
    `lightningcss::values::color::Color::RGBA` field
    layout under the W4 harness.

## AX seeds

- **Cost-model grid sweep** (AM.6 → AQ.9.4 → AW ledger)
  — egraph `CostWeights` calibration via grid search.
  Acceptance: ≥ 5% reduction in DTA state count or
  extraction wall-clock vs post-AW master, OR
  null-result close with measurement evidence.
- **Global CSP solve** (AL → AQ.9.5 → AW ledger) —
  single-solver path behind a feature flag; byte-for-
  byte comparison of emitted constants. Acceptance:
  strictly-better-or-equal on every grammar, OR
  documented null-result.
- **Scanner-architecture cluster** (AR.6.x / AS.5.x)
  — dedicated scanner-hygiene tranche.
- **Hyperopt cluster**: AN.5 u8x32 AVX2 widening;
  AO.5.3 frequency-ordered dispatch; AP.3.2 trim
  elision; AP.4.2 grammar-level pattern dedup; AP.5.4
  deferred UTF-8 validation; AQ.7.3 generalised
  length-bucket PHF tail; AQ.8.1 skip_space bitmap
  caching; AQ.8.3 TLS-recycled scratch; AT.4.3 NEON
  17-digit fractional scan.
- **Substrate cluster**: FDMP mimalloc segment-class
  rounding; per-grammar column overlays; AV.3.6 CSS L4
  DTA state-count narrowing (conditional on post-AW
  I-cache pressure).
- **AltLinear backtracking cost model** — if AW-IV W2
  profiling shows backtracking dominates some grammar's
  parse, AX speculative-execution substrate or
  savepoint-compression.
