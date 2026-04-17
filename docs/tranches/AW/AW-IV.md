# Tranche AW-IV — Granular Exceed & Parity Harnesses

AW-IV refines the architectural transposition AW-III shipped: arch-gated SIMD
widening, scanner PaddedView migration + cluster consolidation, NEON 17-digit
float scan, bloom + GADT runtime dedup + grammar-level pattern hoisting,
document-parallel fork over the stage-1 structural index, `Tape::reduce_column<C, R>`
visitor + 4-lane SIMD pack, cost-model grid sweep, sonic-rs + lightningcss
parity harnesses, and walker/reader migration carry-overs from AU.

AW-III delivered the DTA-as-flattened-tape-automaton via three general
emitter passes (walker specialisation + stage-1 SIMD bitmap + fused SoA
write) plus five emitter-mined consumer activations (ShapeRef, PHF,
ClassifyByte, direct-to-struct, Pratt const-fold). AW-III's hard gate was
**strict-better-than post-AU on ≥ 15/19 entries**. AW-IV's mandate is
**every entry exceeds post-AU; parity harnesses CI-gated**.

## Architectural thesis

Every AW-IV item layers over the AW-III substrate. The walker-specialisation
pass (AW-III W4) is the integration point for parity harnesses. The stage-1
SIMD index (AW-III W5) is the substrate for document-parallel fork.
ShapeRef + PHF + ClassifyByte (AW-III W6) are the substrate for bloom
runtime dedup + grammar-level pattern hoisting. `reduce_column<C, R>` lives
on the SoA Columns substrate AW-III preserved.

Parity harnesses run the AW-III-emitted parsers end-to-end against sonic-rs
and lightningcss; per-grammar-emitted typed-AST projections compare node-for-
node against the upstream typed-AST. The harnesses are CI-gated.

## Invariants

1. **One path** — inherited from AW-III. AW-IV does not introduce parallel
   parse surfaces, feature flags, or dual-path builds. Document-level parallel
   parse (W4) is orthogonal — multiple specialised walkers run the same
   entry, not different entries.
2. **Every substrate AW-III emitted has an active consumer at AW-IV close.**
   The cost model (AW-IV W3) consumes egraph weights; bloom dedup (W3) consumes
   `dedup_eligible_rules`; document-parallel fork (W4) consumes
   `parallel_break_even_bytes` + the stage-1 index; reduce_column (W5)
   consumes `active_columns`.
3. **Typed-AST parity is total.** sonic-rs harness on canada / twitter / citm /
   data / data_xl: zero divergences. lightningcss harness on bootstrap /
   tailwind / normalize: zero divergences. Color view field-for-field
   equivalence under W5 parity harness.
4. **Bench-between-waves structural.** Each wave closes with a bench
   checkpoint (`post-AW-IV-W{N}.json`); W6 composes the multi-wave aggregator
   `post-AW-IV.json`. A wave whose checkpoint regresses against the prior
   without a written rationale reopens.
5. **Workspace green at every wave boundary.**
6. **Full generalization** — inherited from AW-III. Every emitter pass and
   every IR-mining pass triggered by IR-structural properties (architecture
   capability, alphabet cardinality, state count, payload column count). No
   per-grammar hand-written branches.
7. **No deferrals, regardless of newfound scope.** Inherited from
   `docs/instructions/README.md` and AW-III. Scope-reveal under contact
   triggers re-plan-with-more-agents, never silent forward-routing.

## Wave schedule

| Wave | Scope | Agents | Bench gate |
|------|-------|--------|------------|
| W1 | AVX2 u8x32 widening + AArch64 NEON + WASM simd128 + PHF frequency ordering + length-bucket tail + small-Alt SIMD compare | 4 parallel | AVX2 scan ≥ 15% drop vs AW-III; Sheets `__function_name` ≥ 30% drop from frequency + bucket combined; small-Alt SIMD intrinsics visible under `cargo expand` per primary grammar |
| W2 | Scanner PaddedView migration + scanner cluster consolidation + NEON 17-digit fractional scan + skip_ws bitmap caching + trim-call elision | 3 parallel | parse-that scanners LOC drops ≥ 600; canada f64-fraction parses bit-identical to scalar; `skip_ws` self-time drops ≥ 30% |
| W3 | Bloom + GADT runtime dedup + grammar-level pattern hoisting + cost-model grid sweep | 3 parallel | bootstrap record count drops ≥ 30% vs AW-III; cost-model sweep produces calibrated weights or null-result close |
| W4 | Document-parallel fork over stage-1 index + GrammarProfile calibration + AU walker/reader migration | 3 parallel | tailwind 4c sub-linear-to-linear scaling; canada 4c ≥ 1800 MB/s; AU's 7 variant-dispatch tests un-ignored |
| W5 | `reduce_column<C, R>` + 4-lane SIMD pack + sonic-rs parity + lightningcss parity | 3 parallel | reducer ≥ 6× scalar baseline OR per-arch rationale; both parity harnesses zero-divergence + CI-gated |
| W6 | FINAL + close + multi-wave bench aggregator | 1 serial | every entry exceeds post-AU; `post-AW-IV.json` exists; `FINAL-IV.md` exists |

## Phases

### W1 — Arch-gated SIMD widening + PHF refinements

#### W1.1 AVX2 u8x32 widening (AN.5 chronic)

Owner: `crates/core/src/generate/regex/emit/simd.rs`; integration into
`bbnf-simd-scan` kernels emitted by AW-III W5.

Every SIMD call site today uses `u8x16`. On x86_64 AVX2, widen to `u8x32` for
scanner structural-byte passes. The walker's structural-bitmap producer
(inherited from AU.2.7 + AW-III W5) and the DFA scanner dispatch both
consume. Arch-gate via `#[cfg(target_feature = "avx2")]`; NEON path
unchanged.

**Hard gate**: `cargo expand` shows the AVX2 intrinsic on x86_64. Samply on
canada.json (x86_64 AVX2) shows ≥ 15% reduction in structural-scan self-time
vs the AW-III u8x16 baseline.

#### W1.2 WASM simd128 + AArch64 NEON polish

Owner: `crates/core/src/generate/regex/emit/simd.rs`; `bbnf-simd-scan/src/`.

Verify `bbnf-simd-scan`'s NEON path lowers cleanly on AArch64 (no scalar
fallback creeping in via portable-simd pessimisation). Add WASM simd128
variant for browser-side parsing parity.

**Hard gate**: `cargo asm` confirms NEON intrinsics on AArch64; WASM simd128
intrinsics on `wasm32-unknown-unknown` with `-C target-feature=+simd128`.

#### W1.3 PHF frequency ordering (AO.5.3 chronic)

Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs`
(extends AW-III W6.2); `crates/ir/src/passes/recognizers/keyword_stats.rs`
(extends mining pass with frequency facts).

Five-tranche chronic. Emitter mines per-keyword visit frequency from a
representative-corpus profile (samply attribution over canonical inputs per
grammar), emits `FREQUENCY_ORDER: &[u32]` alongside the AW-III PHF table.
Walker's byte-dispatched miss path consults frequency order before PHF
lookup for the top-N hot keywords (N tunable; default 8).

The mining is general — any keyword appearing more than `frequency_threshold`
times in the representative corpus enters the fast-path. `frequency_threshold`
is a `pub const` in the egraph cost model (pluggable, not hardcoded per
grammar).

**Hard gate**: samply delta on `Sheets::__function_name` self-time ≥ 30% from
frequency ordering vs AW-III W6.2 baseline. At least one grammar (Sheets is
the canonical case) shows the frequency-ordered top-8 covering ≥ 50% of
function-name dispatches.

#### W1.4 PHF length-bucket tail (AQ.7.3 chronic)

Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs`
(extends W1.3).

Four-tranche chronic. The PHF for variable-length dense keywords subdivides
by byte length before hashing. CSS `NAMED_COLOR_PHF` stays one table (single
byte length ranges fit cleanly); Sheets function names add a length-bucket
prefix — `[bucket_6, bucket_7, bucket_8, …]: &[Phf]` indexed by
`[input_length.saturating_sub(MIN).min(MAX - MIN)]`. Dense tail for short
names; sparse tail for long.

White-colour `0xFFFFFFFFu32` collision routed to WideScalar in AW-I.W0.8 —
PHF stays homogeneous. Mechanism is general — grammar's keyword set + length
distribution drives the bucket structure; no per-grammar branch.

**Hard gate**: bucket structure emitted for every grammar where keyword
length variance exceeds threshold; samply on `Sheets::__function_name` shows
incremental improvement over W1.3 frequency ordering.

#### W1.5 SIMD keyword compare (small-Alt specialisation)

Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (extends);
`crates/bbnf-tape/src/driver.rs` (walker arm integration);
`crates/core/src/generate/regex/emit/simd.rs` (small-Alt SIMD emit).

When an Alt is `≤ 16 keywords ∧ length-bounded ∧ FIRST-set mutually-disjoint`
(all general IR facts), emit a SIMD compare instead of a PHF lookup. CSS
`colorType` (9 entries) packs into one 128-bit NEON register (9 × 8-byte
lanes, padded). One parallel 8-byte-lane compare emits a match bitmask;
`trailing_zeros` picks branch index. Same pattern for BBNF `__directive`
(8 entries). Typed u8 discriminant flows through per-branch payload emission.

The trigger is a general IR-fact predicate, not a per-grammar branch. Same
mechanism, different workload density per invariant 6.

**Hard gate**: `cargo expand` shows the NEON intrinsic on AArch64 and the
AVX2 intrinsic on x86_64 for every Alt meeting the predicate. At least one
small-Alt SIMD compare per primary grammar where the IR predicate fires.

### W2 — Scanner consolidation + float-parse polish

#### W2.1 Scanner PaddedView paired migration (CO-E2 chronic)

Owner: `crates/core/src/backend/rust/emitter/string_decode.rs`;
`crates/core/src/generate/regex/emit/simd.rs`.

CO-E2's deferred migration: 7 emitter call sites pass `&state.src_bytes`
(unpadded). Migrate to `PaddedView`. Pair the `scan_quoted_string_simd` /
`decode_json_string_to_arena` migration — both kernels move to `PaddedView`.

**Hard gate**: per-chunk SIMD bounds guards in consolidated loops return 0;
`grep -rn 'src_bytes' crates/core/src/` in scanner paths returns 0.

#### W2.2 Scanner-architecture cluster consolidation + NEON 17-digit (AR.6.x / AT.4.3 chronics)

Owner: `parse-that/rust/parse_that/src/{scanners,regex}/`;
`crates/ir/src/regex_info.rs`.

**Scanner cluster** (six-tranche deferral): `RegexClassMiner` consolidation
into one canonical miner shared across scanner emission paths; `ScanLut`
registry as a per-grammar resource (replaces scattered per-rule LUT emit
sites); `WsCommentConfig` parameterisation; `FnDescriptor` post-pass; HIR
predicate re-exports collapsed to one module. Net: ~600 LOC delete + ~350 LOC
net reduction per AR audit.

**NEON 17-digit fractional scan** (AT.4.3 chronic): AV.3.5 landed
Eisel-Lemire + 16-digit integer SIMD fastpath; the 17-digit fractional kernel
specifically never landed. Hand-written NEON kernel for 17-digit fractional
part — ±1 ULP vs scalar `f64::from_str` on the canonical corpus.

**Hard gate**: `parse-that/rust/parse_that/src/scanners/` LOC drops by ≥ 600;
HIR predicate module count drops to 1; `parse-that` f64-parse tests pass
bit-identically on fractional inputs up to 17 digits.

#### W2.3 skip_ws bitmap caching + trim-call elision (AQ.8.1 / AP.3.2 chronics)

Owner: `crates/bbnf-tape/src/driver.rs::skip_ws`;
`parse-that/rust/parse_that/src/`.

**AP.3.2 redundant trim-call elision**: fused-scan with `last_trim_offset`
memoises the most recent trim-result so adjacent trim calls short-circuit when
the input position hasn't advanced. Walker's `skip_ws` consults before
scanning.

**AQ.8.1 skip_space bitmap caching**: `nospace_bits: [u8; N]` + `nospace_start: u32`
cache populated on first `skip_ws` call per parse. Subsequent calls hit
the cache for their byte range. (Note: largely subsumed by AW-III W5's
WS-collapse-into-stage-1 fix; remaining benefit is on per-call skip_ws sites
the stage-1 cursor doesn't cover.)

**Hard gate**: samply on bootstrap shows `skip_ws`/`__ws` self-time drops
≥ 30% vs the AW-III baseline (incremental over AW-III's stage-1 absorption).

### W3 — Runtime dedup + cost-model grid

#### W3.1 Bloom + GADT runtime dedup + grammar-level pattern hoisting (AP.4.2 chronic)

Owner: `crates/bbnf-tape/src/dedup.rs` (new);
`crates/bbnf-tape/src/driver.rs`;
`crates/ir/src/passes/recognizers/dedup_eligibility.rs` (new);
`crates/ir/src/passes/transform/pattern_dedup.rs` (new — compile-time sibling).

**Runtime bloom + GADT.** Layered over the AW-III specialised walker. Mandatory
where `GRAMMAR_PROFILE.dedup_eligible_rules` is non-empty (CSS
`compoundSelector`, `identifier`, `namedColor`-wrap, fixed unit suffixes;
JSON `null`, `true`-branch, `emptyObject`, `emptyArray`; BBNF literal-only Alt
branches). Mining pass general; consumer general.

64-bit rolling FNV over raw column bytes of child records
(`hash_children_tail`). Span_lo/span_hi ignored for structural rules. Bloom
admission gate; on hit, GADT lookup → `columns_range_eq` confirms; on
confirm, `push_compound_referring(rule_id, existing, span)` shares the
existing subtree.

`dedup_eligibility` IR pass classifies each rule using existing IR facts:
`TypeDesc`, `EClassFacts.closure_free`, `EClassFacts.all_descendants_elidable`.
Populates `GRAMMAR_PROFILE.dedup_eligible_rules`.

**AP.4.2 grammar-level pattern dedup** (compile-time sibling): `ws + ':' + ws`
appears 43 times in CSS L4; `!important` appears 42 times across grammars.
Compile-time pass identifies recurring sub-patterns and hoists into synthetic
non-terminals. Pre-egraph pass (runs after `canonicalize_aliases`, before
`factor_common_prefixes`).

**Hard gate**: canada.json (zero-sharing input) bloom-AND steady-state
overhead < 2% of parse time; bootstrap.css record count drops ≥ 30% vs
AW-III baseline; `GRAMMAR_PROFILE.dedup_eligible_rules` non-empty;
grammar-level pattern hoisting synthesizes ≥ 5 non-terminals on CSS L4 with
DTA state-count reduction ≥ 100.

#### W3.2 Cost-model grid sweep (AM.6 → AQ.9.4 chronic)

Owner: `crates/egraph/src/cost.rs`; `crates/bbnf-ir/src/egraph/`;
`scripts/cost-grid-sweep.sh` (new); `docs/benchmarks/cost-weights-sweep.json` (new).

**Six-tranche chronic.** egraph `CostWeights` have been hand-calibrated since
AL. Grid-sweep harness: for each weight in `{seq_cost, alt_cost, repeat_cost,
literal_bonus, regex_cost, payload_bonus, ...}` sweep a logarithmic grid
(0.5×, 1×, 2×, 4×) across the 4-grammar corpus. Measure DTA state count
post-extraction + extraction pass wall-clock. Pick the Pareto frontier per
grammar; pick dominant weights that minimise state count across the corpus.

Commit calibrated `CostWeights` as a `pub const` in the egraph crate.

**Hard gate**: ≥ 5% reduction in DTA state count OR extraction-pass wall-
clock vs AW-III baseline on the 4-grammar corpus. If neither moves, close as
null result — hand-calibrated CostWeights are the permanent decision with
measurement evidence in `cost-weights-sweep.json`.

### W4 — Document-parallel fork + GrammarProfile calibration + AU migrations

#### W4.1 Document-level parallel parse fork

Owner: `crates/ir/src/passes/recognizers/list_rules.rs` (new);
`crates/bbnf-tape/src/driver.rs` (fork orchestration).

A rule is a fork candidate iff:
- body is `Repeat` over an `Alt` or single compound rule,
- children carry no cross-item state (first-set check over all alternatives),
- each item's byte extent is bounded by a stage-1 structural-bitmap position.

Candidates emitted to `GRAMMAR_PROFILE.list_rules`. Targets: CSS
`stylesheet = (ruleset | at_rule)*`, JSON root `value` when array/object,
BBNF `grammar = rule+`, Sheets `file = formula_line*`.

The stage-1 structural bitmap (AW-III W5) marks every item boundary; workers
take contiguous regions. Each worker writes into a local `Columns`. Join
phase memcpy-concatenates columns in order and rewrites `sib_skip` cross-
worker references by the worker's contribution offset. One linear pass per
column.

**Hard gate**: tailwind.css on 4 cores shows sub-linear-to-linear scaling;
`GRAMMAR_PROFILE.list_rules` non-empty for CSS L4.

#### W4.2 PSI rayon stage-B walker integration + GrammarProfile calibration + small-input amortisation

Owner: `crates/bbnf-tape/src/psi.rs` (walker integration + lock-free workers);
`crates/core/src/backend/rust/emitter/profile.rs` (calibration emit);
`crates/bbnf-tape/src/profile.rs`.

**PSI rayon stage-B walker integration** (substrate present in `psi.rs:54-65`
but `parallel_break_even_bytes: 0` gates it off everywhere). PSI
`fill_columns` forks rayon workers when input bytes ≥ per-grammar threshold.
Workers write into pre-resized columns at distinct indices — lock-free per
the psi.rs:54-65 design note.

Stress verification: a 4-core parallel-fill tortured input (synthetic JSON
with ~10K array elements) drives concurrent fills under `cargo test
--release psi_lock_free_stress` for ≥ 60 s; assert zero torn writes via
post-test column-equality against the single-threaded reference. Lock-
freedom is verified by ASAN-clean run + `cargo +nightly miri test
psi_lock_free` (Miri's data-race detection catches any UB in the unsafe
`get_unchecked_mut` paths).

**GrammarProfile calibration.** Per-grammar `expected_ns_per_byte`,
`parallel_break_even_bytes`, `payload_bytes_per_input_byte`,
`dta_setup_floor_ns` measured against the W4 single-threaded matrix. Commit
values as const literals in each emitter's `GRAMMAR_PROFILE`. The
calibration is mechanical — samply attribution over the canonical corpus
produces the constants; emitter writes them as `pub const`.

**Small-input amortisation** (AW.4.7). Bench reports compute expected MB/s
for sub-100 µs parses from `(input.len() × 1e9) / (setup_floor + input.len()
× expected_ns_per_byte)`, report achieved/expected ratio instead of fixed
gate. The three sheets-small entries documented as small-input cardinality
fact (not a per-grammar specialisation).

**Hard gate**: canada per-core scaling on a 4-core machine for inputs ≥
break-even threshold; zero torn writes under stress (60 s); Miri-clean PSI
fill paths; every grammar's `GRAMMAR_PROFILE` const populated; stub `&[]`
slots remain only where populated-by-design (with in-source rationale).

#### W4.3 variant_idx walker coherence + serialize/structural roundtrip

Owner: `crates/core/src/backend/rust/view/alt.rs`;
`crates/core/tests/{json_parity,structural_parity,imports}.rs` (un-ignore).

AU's 7 ignored JSON variant-dispatch tests assume AoS + payload_idx semantics
that the V2 columnar substrate replaced. Cursor's variant_idx accessor reads
from `flags` column, not from the deleted `payload_idx` field. Mechanical
reader migration.

13 serialize/structural roundtrip tests un-ignore + fix.
`test_selective_transitive_unfurling` triage: fix the import-system bug or
document as AX-scope.

**Hard gate**: 7 JSON variant-dispatch tests un-ignored + passing; 13
serialize/structural roundtrip tests un-ignored + passing; transitive
unfurling fixed OR rationale in `FINAL-IV.md`.

### W5 — `reduce_column<C, R>` + 4-lane SIMD pack + parity harnesses

#### W5.1 `Tape::reduce_column<C, R>` + per-column codegen + 4-lane SIMD pack

Owner: `crates/bbnf-tape/src/columns.rs` (API surface);
`crates/core/src/backend/rust/emitter/visitor.rs` (codegen specialisations);
`crates/core/tests/visitor_reduce.rs` (new).

**Substrate lineage.** AV.2.5 landed the SoA-substrate's reordered-unrolling
kernel as codegen. AW-IV W5 ships the consumer API + SIMD promotion AV could
not finish.

```rust
let total: f64 = parsed.tape().reduce_column::<F64Column, _>(
    0.0,
    |acc, x| acc + x,
);
```

Emitter extends `visitor.rs::emit_visitor_kernels` to produce one
`reduce_column<C, R>` impl per active payload column per grammar, driven by
`GRAMMAR_PROFILE.active_columns`. LLVM monomorphises the reducer at the call
site, preserving the AV.2.5 4-lane scalar reordered-unrolled loop as the
inner body.

Promote the emitted inner loop to packed `std::simd::f64x4` (or arch-
intrinsic `vfaddq_f64` pairs on NEON, `_mm256_add_pd` on AVX2). Portable-simd
is stable; no nightly dependence.

**Hard gate**: ≥ 6× speedup over AV.2.5-baseline scalar left-fold on
canada.json f64 column, OR per-arch rationale documenting AArch64 ceiling
(NEON is 2-lane f64-wide; portable_simd f64x4 lowers to 2× pairs; AVX2
reaches 4-lane natively). No grammar surface introduced — verifying invariant
6.

#### W5.2 sonic-rs + lightningcss parity harnesses

Owner: `crates/core/tests/sonic_rs_parity.rs` (new);
`crates/core/tests/lightningcss_parity.rs` (new).

sonic-rs harness: for every JSON file in `data/json/`, parse with bbnf +
sonic-rs, compare `view().as_value()` vs `sonic_rs::Value` node-for-node.
Numbers bit-for-bit (f64 ULP tolerance). Strings byte-for-byte. Objects
key-set + per-key value equality. Arrays length + per-index value equality.

lightningcss harness: per-declaration equivalence over bootstrap.css +
tailwind.css + normalize.css. Colors via `Color` projection (field-for-field
with `lightningcss::values::color::Color::RGBA`). Selectors via tokenised
form. f32 ↔ f64 compared via `(f32 as f64)`. Alpha-less inputs handled per
AW-I.W0.5 NaN discipline.

Both harnesses CI-gate alongside `grammar_roundtrip` + `tape_parity`.

**Hard gate**: zero divergences on canada / twitter / citm / data / data_xl
(sonic-rs) and bootstrap / tailwind / normalize (lightningcss); CI step wired
in `.github/workflows/ci.yml`.

### W6 — FINAL + close

Orchestrator serial.

1. Full workspace test: 0 failed; ignored count = AX-routed-residual only.
2. Full 19-entry bench matrix.
3. `docs/benchmarks/post-AW-IV.json` — bench-checkpoint + multi-wave
   aggregator (`post-AW-III.json` + `post-AW-IV-W{1..5}.json` folded in).
4. `docs/tranches/AW/FINAL-IV.md` — close document with hard-gate attribution.
5. Update `docs/tranches/AW/FINAL.md` (composite AW close) referencing
   AW-I → AW-II → AW-III → AW-IV chain.

**Hard gate**: every entry exceeds post-AU; both parity harnesses CI-gated;
`FINAL-IV.md` enumerates every hard gate with artefact citation.

## Critical files

| File | Wave |
|------|------|
| `crates/core/src/generate/regex/emit/simd.rs` (AVX2 widening + WASM + small-Alt SIMD compare) | W1 |
| `bbnf-simd-scan/src/{x86,wasm}.rs` (arch siblings to AW-III's NEON) | W1 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (frequency ordering + length-bucket tail extends AW-III W6.2) | W1 |
| `crates/ir/src/passes/recognizers/keyword_stats.rs` (frequency mining extension) | W1 |
| `crates/bbnf-tape/src/psi.rs` (rayon walker integration + lock-free worker stress) | W4.2 |
| `crates/core/src/backend/rust/emitter/string_decode.rs` (PaddedView migration) | W2.1 |
| `parse-that/rust/parse_that/src/{scanners,regex}/` (cluster consolidation, NEON 17-digit) | W2.2 |
| `crates/ir/src/regex_info.rs` (HIR predicate re-export collapse) | W2.2 |
| `crates/bbnf-tape/src/driver.rs::skip_ws` (trim elision + bitmap cache) | W2.3 |
| `crates/bbnf-tape/src/dedup.rs` (new) | W3.1 |
| `crates/ir/src/passes/recognizers/dedup_eligibility.rs` (new) | W3.1 |
| `crates/ir/src/passes/transform/pattern_dedup.rs` (new — compile-time hoisting) | W3.1 |
| `crates/egraph/src/cost.rs` (CostWeights grid sweep) | W3.2 |
| `scripts/cost-grid-sweep.sh` (new) | W3.2 |
| `docs/benchmarks/cost-weights-sweep.json` (new) | W3.2 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (new) | W4.1 |
| `crates/bbnf-tape/src/driver.rs` (fork orchestration, document-parallel) | W4.1 |
| `crates/core/src/backend/rust/emitter/profile.rs` (calibration) | W4.2 |
| `crates/core/src/backend/rust/view/alt.rs` (variant_idx migration) | W4.3 |
| `crates/core/tests/{json_parity,structural_parity,imports}.rs` (un-ignore) | W4.3 |
| `crates/bbnf-tape/src/columns.rs::reduce_column` API | W5.1 |
| `crates/core/src/backend/rust/emitter/visitor.rs` (codegen + SIMD pack) | W5.1 |
| `crates/core/tests/visitor_reduce.rs` (new) | W5.1 |
| `crates/core/tests/{sonic_rs_parity,lightningcss_parity}.rs` (new) | W5.2 |
| `docs/tranches/AW/FINAL-IV.md` (new) | W6 |
| `docs/benchmarks/post-AW-IV.json` (new, aggregator) | W6 |

## Cross-tranche parity

- sonic-rs parity: `json_monolithic_value` bench bbnf/sonic ratio ≥ 0.95 on
  canada (already > 1.0 per AW-III projections); ≥ 0.85 on twitter / data /
  citm / data_xl.
- lightningcss parity: per-declaration equivalence on bootstrap + tailwind +
  normalize.
- Named struct ABI: `pub struct Color` matches `lightningcss::values::color::
  Color::RGBA` field layout under the W5 harness.

## Operational posture

Inherits `docs/instructions/README.md` + `docs/instructions/TRANCHE_SPEC.md`
in full. Inherits AW-III's no-deferrals invariant.

- **No deferrals, regardless of newfound scope.** Reiterated. Scope-reveal
  triggers re-plan-with-more-agents per the operational protocol.
- **Bench between every wave.** Each wave closes with a sidecar; W6 composes
  the multi-wave aggregator.
- **`cargo asm` + samply discipline** — every codegen and perf claim cites an
  artefact.

## AX seeds (carried forward — not in AW-IV scope, route to AX or successor)

- **AltLinear backtracking cost model** — if AW-IV W2/W3 profiling shows
  backtracking dominates some grammar's parse, AX speculative-execution
  substrate or savepoint-compression. Mining pass general (Alt
  branch-attempt-frequency from samply); consumer is a new `DtaState` arm
  for compressed-savepoint Alts.
- **Global CSP solve** (AL → AQ.9.5 → AW ledger) — single-solver path behind
  a feature flag; byte-for-byte comparison of emitted constants. Acceptance:
  strictly-better-or-equal on every grammar, OR documented null-result.
- **AP.5.4 deferred UTF-8 validation** — five-tranche chronic. Skip
  per-byte UTF-8 validation in scanner hot loop when grammar's structural
  alphabet is ASCII-only; defer validation to view-time accessor. Mining
  pass general (alphabet ⊆ ASCII7 predicate); consumer is a new
  `DtaState::Regex` flag.
- **AQ.8.3 TLS-recycled scratch** — three-tranche chronic. Per-thread scratch
  arena for transient allocations (savepoint snapshots, regex match
  buffers); recycled across parses on the same thread. Substrate-level;
  consumer is the savepoint/restore path.
- **FDMP mimalloc segment-class rounding** — substrate cluster; column
  capacity rounding to mimalloc's segment size class to eliminate slop on
  large parses.
- **Per-grammar column overlays** — substrate cluster; remap unused columns
  per grammar (CSS doesn't use Sheets's operator column; reclaim the space).
- **AV.3.6 CSS L4 DTA state-count narrowing** — substrate cluster;
  conditional on post-AW I-cache pressure observation. Should fall out of
  AW-III W4 walker-pass hot/cold partitioning automatically; left as a seed
  in case dedicated narrowing is needed.
- **Hyperopt cluster residue** — items not absorbed: any remaining
  refinements over the AW-IV W1.3/W1.4 PHF work that profile evidence
  surfaces.

These seeds are documented forward-references, not scope deferrals from
AW-IV. AW-IV's no-deferrals invariant applies to declared scope; AX seeds
are work that has not been planned, not work that has been planned and
deferred.

## Successor chain

AW-IV closes green → AX opens (replay tooling, snapshot persistence,
incremental re-parse, structural-default recovery, subsystem closures). AX's
substrate (DTA_TABLE const, DtaSnapshot, decision log, per-record snapshot
metadata) preserved verbatim under AW-III + AW-IV; stage-1 bitmap is
deterministic, replay re-derives it.

Indefatigable. No deferrals. No stubs. No shims. No new `#[ignore]`. No
grammar-specific code paths. Every entry exceeds post-AU at AW-IV close.
