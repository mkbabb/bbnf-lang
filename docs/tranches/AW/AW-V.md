# Tranche AW-V — JSON Prototype + Shape-Mining Generalisation

AW-V ships the sonic-rs-class per-shape inline emitter that the AW-IV
profile wave proved viable. AW-IV closed at a post-W2 floor of ~240 MB/s
twitter / 24 MB/s CSS normalize because the walker was still indirected
at the `try_branch` boundary; the `bbnf-tape` residual-helper boundary;
the `walk_cursor` second-pass boundary; and the cold-helper-BL boundary
on CSS L4's i-cache-overflowing walker. **AW-V inverts the consumer**:
a JSON-only hand-tuned prototype in an isolated worktree validates that
the existing `bbnf-tape` + `bbnf-simd-scan` substrate supports
sonic-rs-class throughput; the same per-shape structure then generalises
back through the emitter via a new IR shape-mining pass and per-shape
emitter modules.

Five phases; strict sequencing. Prototype isolated until its 10%-of-sonic
gate passes, then cherry-picked onto master alongside the AW-V
substrate-enabler changes and the emitter-lifted per-shape pipeline.

## Scope

1. **Substrate enablers** (B2 + B3): four surgical `bbnf-tape` changes
   + one `bbnf-simd-scan::emit` addition. All preserve AX's cold-path
   replay surface verbatim.
2. **JSON-only prototype** (B1): new crate `crates/bbnf-json-prototype/`
   with hand-tuned per-shape inline loops + monomorphic visitor.
   Bench-gated against sonic-rs ± 10%.
3. **Shape-mining IR pass + JSON emitter-lift** (B4 Phase 2): generalise
   the prototype back through codegen. Emitter-produced JSON parser
   matches hand-prototype bench ± 5%.
4. **CSS + Sheets + BBNF shape coverage** (B4 Phases 3–5): extend
   shape-mining to the remaining primary grammars; wire-contract pipeline
   fix for BBNF's `GRAMMAR_PROFILE` silent data-drop.
5. **FINAL + parity harnesses + 19-entry bench matrix**: every parse
   entry exceeds post-AU; sonic-rs + lightningcss CI-gated.

## Architectural thesis

Per the AW-IV profile wave (six samply + static-audit agents at HEAD
`f457b4df`):

1. **The DTA substrate is architecturally complete.** `bbnf-simd-scan`
   is algorithmically sufficient (B3); `bbnf-tape` needs four surgical
   changes that preserve the cold-path replay surface (B2). No crate
   rewrite; no architectural transposition.

2. **The interpreter persists in the consumer, not the substrate.**
   `try_branch` (AltLinear dispatch helper) contains inlined
   `dispatch_one` + runtime `match table.states[N]`; every AltLinear
   branch attempt re-interprets the state table at the cross-crate
   boundary. CSS L4 70.9–78.9% self-time; Sheets 52–72%; BBNF 70%.
   CSS L4's 153.9 KB walker sits at 0.1–0.2% self-time because the hot
   path never reaches the walker's outer `match cur` — it is trapped
   inside `try_branch` (P2).

3. **sonic-rs's winning shape is replicable.** `parse_object::<DocumentVisitor>`
   + `parse_array::<DocumentVisitor>` cover 81–88% self-time at 2 symbols
   — tight monomorphic inner loops, cached SIMD whitespace bitmap,
   compile-time-monomorphised visitor, zero function-call boundaries
   inside the per-token loop (P5). The same shape lands via
   per-shape inline emission over our existing SIMD + tape substrate.

4. **The generalisation is mechanical.** One new IR pass
   (`shape_mining.rs`) classifies each rule into 7 shape categories
   (Object/Array/String/Number/Keyword/Pratt/Unordered). Per-shape
   emitter modules at `crates/core/src/backend/rust/emitter/shapes/`.
   Detectors ground in existing miner outputs; no grammar-name
   branches. Rules without shape match fall back to
   `__dta_walker_inline::run` — AW-III/IV substrate + AX replay
   preserved. Coverage: JSON 100%, Sheets 92%, CSS L4 78%, BBNF 75%
   (≥ 80% average).

## Invariants

1. **No deferrals, regardless of newfound scope.** Per
   `docs/instructions/README.md`.
2. **Substrate-with-consumer is one unit of work.** Per
   `docs/instructions/README.md` §code-discipline. Each shape-mining
   detector that lands without a consuming emitter is rejected.
3. **AX replay-surface preserved.** `bbnf_tape::driver::dispatch_one` +
   helpers + `DtaState` variants + `DTA_TABLE` + the cold-path
   table-interpretive path continue to exist. Per
   `docs/tranches/AX/AX.md` §3 (cold-path replay-surface invariant).
4. **§6 generalisation invariant.** Every shape detector is an IR pass
   triggered by IR-structural properties; per-grammar OUTPUT varies
   because per-grammar IR varies; per-grammar MECHANISM does not. The
   grammar identity appears only in symbol prefixes.
5. **Prototype isolation.** Phase 2's hand-written prototype lives in
   `bbnf-wt-aw5-prototype` sibling worktree; does NOT modify any
   master-tracked file; cherry-picks to master only after the
   10%-of-sonic gate passes.
6. **Wire-contract end-to-end tests.** Per `docs/instructions/README.md`
   §architecture-invariants. The shape-mining output has one
   wire-contract test asserting IR → emitter → `pub const` → runtime
   consumer end-to-end for each of the seven shape categories.
7. **Bench-between-waves.** Per-wave sidecar
   `docs/benchmarks/post-AW-V-W{N}.json`; W6 composes the multi-wave
   aggregator.
8. **Per-wave verification ledger.** `nm` symbol-presence assertions,
   `cargo expand` arm-body inspection, `cargo asm` instruction-count,
   samply attribution per consumer.

## Wave schedule

| Wave | Scope | Agents | Opens after | Hard gate |
|------|-------|--------|-------------|-----------|
| W1 | Substrate enablers: `bbnf-tape-codegen` subcrate (TokenStream body fragments for 4 residual helpers) + `bbnf-simd-scan::emit` submodule (~300 LOC) + `Columns::push_scalar_payload_*` + monomorphic `Visitor` trait in bbnf-tape | 3 parallel | AW-IV closed | `bbnf-tape-codegen` exposes the 4 helper-body fragments; `bbnf-simd-scan::emit` round-trips a test fragment; `Visitor` trait has `TapeVisitor` + placeholder `ValueVisitor` implementations; all W1 work preserves `bbnf_tape::driver::dispatch_one` verbatim |
| W2 | JSON-only prototype in isolated worktree: `crates/bbnf-json-prototype/` per B1 | 2 parallel (prototype-build + bench-scaffolding) in `bbnf-wt-aw5-prototype` | W1 closed | each of {data_s, twitter, citm, canada, data_xl} within 10% of sonic-rs's ns/iter on the twin-pair bench; samply confirms 2 monomorphised hot symbols at ≥ 70% self-time (shape parity with sonic) |
| W3 | Shape-mining IR pass + JSON emitter-lift: `crates/ir/src/passes/recognizers/shape_mining.rs` + `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar}.rs` | 4 parallel | W2 closed + cherry-picked | emitter-produced JSON parser matches hand-prototype bench ± 5%; rules without shape match continue to route through `__dta_walker_inline::run`; wire-contract test per shape category |
| W4 | CSS L4 + Sheets shape coverage: `shapes/{pratt,unordered}.rs` + extend `shape_mining.rs` for CSS compound-selectors + Sheets 6-rung Pratt + function-name PHF via shape-mining | 3 parallel | W3 closed | CSS bootstrap ≥ 1500 MB/s; tailwind / normalize sonic-parity-equivalent; Sheets parse entries ≥ parity post-AU |
| W5 | BBNF shape coverage + wire-contract pipeline fix for BBNF's `GRAMMAR_PROFILE` silent drop | 2 parallel | W4 closed | BBNF self-host ≥ 500 MB/s; `GRAMMAR_PROFILE` literal non-empty for every slot where IR mining produces data |
| W6 | FINAL + 19-entry bench matrix + sonic-rs + lightningcss parity harnesses CI-gated | 1 serial + 1 parity-harness agent | W5 closed | every parse entry exceeds post-AU; both parity harnesses zero-divergence + CI-gated; verification ledger complete |

## Phases

### W1 — Substrate enablers

Three parallel agents. None modifies the AW-IV hot-path walker; all
additions are additive (new crate, new submodule, new method, new trait)
preserving the cold-path replay surface verbatim.

#### W1.1 — `bbnf-tape-codegen` subcrate

Owner: `crates/bbnf-tape-codegen/` (new workspace member);
`crates/bbnf-tape/src/driver.rs` (annotate source helpers with
`#[body_fragment]`-ish attribute or keep separate; see below).

Per B2 §1, the four residual helpers — `advance_or_pop_with`,
`nearest_variant_frame`, `write_decoded`, `finalise` — expose their
bodies as TokenStream fragments the walker emitter splices inline.
Two approaches:

- **Body-source fragments**: `bbnf-tape-codegen` crate holds the helper
  bodies as stringified Rust source + `syn::parse_str` at emitter-build
  time. Each fragment is a compile-time constant.
- **Generated from annotations**: a `#[export_body_fragment]` attribute
  on the helper fn in `bbnf-tape` produces a paired `pub const
  __<helper>_BODY: &str = "..."` at build time.

W1.1 picks the approach with fewer moving parts (likely the first) and
ships the fragment library. The runtime helpers in `bbnf-tape::driver`
survive unchanged — they remain callable for the cold-path
`dispatch_one` replay surface.

**Hard gate**: `bbnf-tape-codegen` exports 4 body fragments; a unit test
parses each fragment with `syn::parse_str::<syn::Block>` and confirms it
is valid Rust. No change to `bbnf-tape::driver`'s public API.

#### W1.2 — `bbnf-simd-scan::emit` submodule

Owner: `crates/bbnf-simd-scan/src/emit.rs` (new, ~300 LOC).

Per B3, expose TokenStream body-fragments for the SIMD kernels that
the per-shape emitter splices inline: `nibble_lut_scan`,
`multi_cmp_scan`, `clmul_parity`, `shift_xor_parity`, `tzcnt_compact`,
`nospace64_scan`, `first_quote_or_backslash`, `quoted_string_simd_body`,
`eisel_lemire_body`.

The library fns stay. `emit` is an additional surface.

**Hard gate**: every kernel in the list has a paired body-fragment
exporter; unit tests parse each and confirm valid Rust; kernel crate's
public API unchanged.

#### W1.3 — `Columns::push_scalar_payload_*` + monomorphic `Visitor` trait

Owner: `crates/bbnf-tape/src/columns.rs` (new method);
`crates/bbnf-tape/src/visitor.rs` (new module).

Per B2 §3: add `push_scalar_payload_{f64,u8,bool,hex_u32,i64}` methods
on `Columns` that write the scalar directly into the appropriate column
without going through PSI scheduling. Used by the per-shape emitter's
leaf-emission arms.

Per B2 §4: add a `Visitor` trait in `crates/bbnf-tape/src/visitor.rs`
with the hierarchy B4 §5 designed (`GrammarVisitor` top-level +
per-shape sub-traits). Ship default impls `TapeVisitor` (emits into
`Columns + PayloadStream + FrameStack`) and placeholder `ValueVisitor`
(to be filled in by the per-grammar type resolver). Both are
monomorphised at call sites; no dyn dispatch.

**Hard gate**: `push_scalar_payload_*` write bytes to the right column
offsets; unit tests verify. `TapeVisitor` emits tape-shape-identical
output to `dispatch_one`'s path on a fixture; `ValueVisitor` compiles
against a minimal JSON enum.

### W2 — JSON-only prototype

Two parallel agents in `bbnf-wt-aw5-prototype` worktree (**not** master).

#### W2.1 — Prototype crate

Owner: `crates/bbnf-json-prototype/` in the worktree.

Per B1:
- Workspace member depending only on `bbnf-tape` + `bbnf-simd-scan`.
- Single `pub fn parse_json<V: JsonVisitor>(input: &[u8], visitor: &mut V) -> Result<(), ParseError>`.
- Five `#[inline(always)]` per-shape functions: `parse_value`,
  `parse_object`, `parse_array`, `parse_string`, `parse_number`.
- Zero `dispatch_one` / `try_branch` / `advance_or_pop_with` /
  `DtaState` / `FrameStack`. Recursive descent via the CPU stack.
- Inline SIMD kernels via W1.2's `bbnf-simd-scan::emit` fragments OR
  via direct `#[inline(always)]` fn calls (the prototype picks
  whichever matches sonic's shape).
- Inline Eisel-Lemire for f64 decode.
- Two visitors: `ValueVisitor` (materialises into `sonic_rs::Value`-shaped
  enum; this is the sonic-parity validator) and `TapeVisitor` (emits
  into `bbnf_tape::Columns`; this is the AW-IV-substrate validator).

#### W2.2 — Bench scaffolding

Owner: `crates/bbnf-json-prototype/benches/json_value.rs` (in worktree).

Isomorphic to `crates/core/benches/json/value.rs` — same input corpus
(data_s, twitter, citm, canada, data_xl); one `cargo bench` target per
entry; divvy-mode SIMD-off tests for fallback coverage.

**Hard gate**: each of {data_s, twitter, citm, canada, data_xl} within
**10% of sonic-rs's ns/iter** on the twin-pair bench (per B1 §7).
Samply on JSON twitter confirms the walker's top-2 hot symbols
(`parse_object` + `parse_array`) cover ≥ 70% self-time (sonic parity at
81–88%; 70% is the gate with margin). No out-of-line `#[cold]` helpers
inside the walker module. No symbol named `dispatch_one` /
`advance_or_pop_with` / `try_branch` / `walk_cursor` reachable from the
walker.

If gate passes: cherry-pick the prototype crate onto master at W3 open.
If gate misses: re-open with additional agents per the no-deferrals
invariant — close the specific cycle-budget item the miss reveals.

### W3 — Shape-mining IR pass + JSON emitter-lift

Four parallel agents.

#### W3.1 — `shape_mining.rs` IR pass

Owner: `crates/ir/src/passes/recognizers/shape_mining.rs` (new).

Per B4 §1–2: classify each rule into one of seven shape categories.
Detector per category grounds in existing IR-miner outputs. Output: a
`pub struct ShapeAssignments { per_rule: HashMap<RuleId, ShapeTag> }`
carried in the IR for downstream consumption. `ShapeTag` enum with 7
variants (Object, Array, String, Number, Keyword, Pratt, Unordered) +
a `None` fallback.

**Hard gate**: JSON's 6 rules get shape tags (object/array/string/number
/bool-keyword/null-keyword); Sheets's operator tower gets Pratt;
CSS's compoundSelector gets Unordered; BBNF's directive gets Keyword.

#### W3.2 — Per-shape emitter modules (JSON subset)

Owner: `crates/core/src/backend/rust/emitter/shapes/` (new directory);
emit modules `object.rs`, `array.rs`, `string.rs`, `number.rs`,
`keyword.rs`, `scalar.rs` land here (Pratt + Unordered deferred to W4).

Per B4 §2: each emitter produces `pub fn parse_<shape>_<grammar>_<rule>
<V: ShapeVisitor>(...) -> Result<(), ParseError>` with the
sonic-rs-inline-loop shape. Dispatch at call sites via compile-time
byte-match over existing `DisjointFirstMiner` tables (no monolithic
`match cur`). SIMD kernel bodies spliced inline via W1.2
`bbnf-simd-scan::emit` fragments.

**Hard gate**: emitted JSON parser bench matches the hand-prototype
within ± 5% on every entry. Wire-contract test per shape: a fixture
grammar with known rule assignments → `ShapeAssignments` contains
expected tags → emitter produces parse function with expected shape
(via `cargo expand` inspection + per-shape integration test).

#### W3.3 — Shape-mining `cargo expand` regression tests

Owner: `crates/core/tests/shape_mining_emission.rs` (new).

For each of the 7 shape categories, a fixture grammar rule + a test
asserting the expanded emit matches a canonical shape (per-shape
TokenStream golden file).

#### W3.4 — Integration + regression suite

Owner: `crates/core/tests/json_parity_shape_emit.rs` (new).

Re-run the full JSON parity test suite with the shape-emitter-produced
parser; assert zero divergence vs the `__dta_walker_inline::run` path
on every existing fixture.

### W4 — CSS L4 + Sheets shape coverage

Three parallel agents.

#### W4.1 — Pratt-shape emitter + Unordered-shape emitter

Owner: `crates/core/src/backend/rust/emitter/shapes/{pratt,unordered}.rs`.

Pratt-shape: CSS's `calc` / `min` / `max` / `clamp` bodies; Sheets's
6-rung operator tower. Precedence LUT byte-indexed per B4 §1;
`lookup_precedence` linear scan deleted on the shape-emit path.

Unordered-shape: CSS's `compoundSelector` (5-way Alt; each branch
independent; emit as byte-dispatch + sub-loop per B4 §1).

#### W4.2 — CSS L4 shape coverage + Pratt/Unordered consumer

Owner: extend `shape_mining.rs` + wire CSS L4 rules to the new emitters.

Target 78% coverage per B4. Rules without shape match continue through
`__dta_walker_inline::run` per the fallback contract.

**Hard gate**: CSS bootstrap ≥ 1500 MB/s; samply confirms CSS compound-
selector arms are monomorphic hot symbols (sonic-parity-equivalent);
`__dta_walker_inline::run` symbol size reduced by ≥ 50% (most hot arms
moved to shape emitters, eliminating the 154 KB overflow).

#### W4.3 — Sheets shape coverage + function-name PHF

Owner: extend `shape_mining.rs` for Sheets; wire Keyword-shape emitter
to populate `KEYWORD_PHF` for the 150 Sheets functions.

**Hard gate**: Sheets parse entries ≥ parity post-AU; `KEYWORD_PHF`
literal non-empty for Sheets.

### W5 — BBNF shape coverage + wire-contract pipeline fix

Two parallel agents.

#### W5.1 — BBNF `GRAMMAR_PROFILE` wire-contract fix

Owner: `crates/ir/src/passes/profile.rs`;
`crates/core/src/backend/rust/emitter/profile.rs`;
BBNF-specific emit path.

Per P4: every `GRAMMAR_PROFILE` slot for BBNF emits `&[]` despite the
IR mining succeeding. Trace the BBNF-specific projection path and fix
the drop. Add a wire-contract end-to-end test per the §invariants.

**Hard gate**: BBNF's `GRAMMAR_PROFILE` literal in `generated.rs` is
non-empty for every slot where IR mining produces data (singletons,
digraphs, quote_classes, keyword_tables, shape_dict, etc.).

#### W5.2 — BBNF shape coverage

Owner: extend `shape_mining.rs` for BBNF; wire directive dispatch via
Keyword-shape + PHF.

Target 75% coverage per B4. BBNF has no upstream comparator, so the
gate is relative to post-AU.

**Hard gate**: BBNF self-host bench ≥ 500 MB/s.

### W6 — FINAL + parity harnesses + bench matrix

Orchestrator serial + one parity-harness agent.

1. `crates/core/tests/sonic_rs_parity.rs` (new; shape-emitted parser vs
   sonic-rs on every JSON fixture) + `lightningcss_parity.rs` (new; CSS
   shape-emitted vs lightningcss). Both CI-gated.
2. Full 19-entry bench matrix; **every parse entry exceeds post-AU**.
3. `docs/benchmarks/post-AW-V.json` + `docs/tranches/AW/FINAL-V.md`.
4. Verification ledger complete: `nm` symbol-presence, samply
   attribution, wire-contract tests, `cargo asm` arm-body inspection
   per wave.

## Per-grammar projections at AW-V close

Computed from B1's ~1.2 cyc/byte cost model + B3's 0.84 cyc/byte SIMD
contribution + B4's coverage percentages:

| Entry | post-AU | post-AW-IV-W2 | post-AW-V projected | vs post-AU |
|---|---:|---:|---:|:---:|
| json twitter | 1967 | 241 | 2500–2900 | **1.3–1.5×** |
| json citm | 2438 | 284 | 3000–4000 | **1.2–1.6×** |
| json canada | 1231 | 142 | 3500–4500 | **2.8–3.7×** |
| json data_xl | 1179 | 185 | 2800–3500 | **2.4–3.0×** |
| json data_s | 1746 | 243 | 2200–2800 | **1.3–1.6×** |
| css normalize | 735 | 25 | 1500–2200 | **2.0–3.0×** |
| css bootstrap | 454 | 15 | 1800–2500 | **4.0–5.5×** |
| css tailwind | 496 | 16 | 2000–3000 | **4.0–6.0×** |
| sheets parse_simple | 95 | 4 | 80–120 | **0.84–1.26×** |
| sheets parse_nested | 128 | 5 | 120–160 | parity–1.25× |
| sheets parse_stress | 121 | 4 | 120–160 | parity–1.32× |
| bbnf json | 283 | 10 | 400–600 | **1.4–2.1×** |
| bbnf ebnf | 223 | 6 | 350–500 | **1.6–2.2×** |
| bbnf css_pretty | 647 | 22 | 800–1100 | **1.2–1.7×** |
| bbnf google_sheets | 858 | 33 | 1100–1500 | **1.3–1.7×** |
| bbnf bbnf_self | 394 | 13 | 600–900 | **1.5–2.3×** |
| bbnf css_l4_grammar | 496 | 20 | 700–1000 | **1.4–2.0×** |

17/17 parse entries exceed post-AU. Small-input Sheets entries still
hit the documented setup-floor tradeoff but land at parity post the
shape-mining path (no cross-crate dispatch per byte).

## Critical files

| File | Wave |
|------|------|
| `crates/bbnf-tape-codegen/` (new subcrate) | W1.1 |
| `crates/bbnf-simd-scan/src/emit.rs` (new) | W1.2 |
| `crates/bbnf-tape/src/columns.rs` (push_scalar_payload_*) | W1.3 |
| `crates/bbnf-tape/src/visitor.rs` (new, trait + TapeVisitor + placeholder ValueVisitor) | W1.3 |
| `crates/bbnf-json-prototype/` (new crate in `bbnf-wt-aw5-prototype` worktree) | W2 |
| `crates/bbnf-json-prototype/benches/json_value.rs` (new, in worktree) | W2 |
| `crates/ir/src/passes/recognizers/shape_mining.rs` (new) | W3.1 |
| `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar}.rs` (new) | W3.2 |
| `crates/core/tests/{shape_mining_emission,json_parity_shape_emit}.rs` (new) | W3.3–3.4 |
| `crates/core/src/backend/rust/emitter/shapes/{pratt,unordered}.rs` (new) | W4.1 |
| `crates/ir/src/passes/recognizers/shape_mining.rs` (extend for CSS + Sheets + BBNF) | W4.2, W4.3, W5.2 |
| `crates/ir/src/passes/profile.rs`, `crates/core/src/backend/rust/emitter/profile.rs` (BBNF wire-contract fix) | W5.1 |
| `crates/core/tests/{sonic_rs_parity,lightningcss_parity}.rs` (new) | W6 |
| `docs/tranches/AW/FINAL-V.md`, `docs/benchmarks/post-AW-V.json` | W6 |

## Prototype isolation contract

W2's prototype lives in `bbnf-wt-aw5-prototype` sibling worktree and
does NOT modify any master-tracked file until the 10%-of-sonic gate
passes. The worktree:

- Checkout at master HEAD when W2 opens (at minimum post-AW-IV close).
- Seeds via `scripts/seed-worktree.sh` for corpus access.
- Contains `crates/bbnf-json-prototype/` as the only new workspace
  member.
- Builds and benches independently of master.
- On gate pass: orchestrator cherry-picks the prototype crate's
  commits onto master at W3 open.
- On gate miss: orchestrator re-opens W2 with additional agents per
  the no-deferrals invariant; never silently lowers the gate.

The worktree is not `/tmp` / `/private/tmp` / ephemeral per the
operational protocol.

## Research artefacts

AW-V opens backed by a 10-agent research wave (six samply +
static-audit profilers; four design brainstormers):

- `docs/tranches/AW/research/aw4-profile-p{1..6}-*.md`
- `docs/tranches/AW/research/aw4-b{1..4}-*.md`
- `docs/tranches/AW/research/SYNTHESIS-3-PROTOTYPE-PATH.md`

These nine + synthesis documents pre-stage the wave schedule above.
The design constraint is traceable to specific profile findings:

- W1 enablers → B2 §1–4 + B3 §3
- W2 prototype gate → B1 §7 + P5's sonic twin-pair measurements
- W3 shape-mining → B4 §1–4
- W4 Pratt / Unordered / Sheets → B4 §7 + P2 + P3
- W5 BBNF wire-contract fix → P4 + P6
- W6 parity harnesses → P5's sonic-rs twin pair methodology

## Operational posture

Inherits `docs/instructions/README.md` + `docs/instructions/PROFILING.md`
+ `docs/instructions/TRANCHE_SPEC.md` in full.

- **No deferrals.** Every item declared in this tranche ships.
- **Bench per wave.** `docs/benchmarks/post-AW-V-W{N}.json` per close;
  W6 composes the aggregator.
- **Verification ledger per wave** per `docs/instructions/README.md`
  §wave-verification-ledger: `nm`, `cargo asm`, samply attribution,
  wire-contract tests.
- **Samply per wave** per `docs/instructions/PROFILING.md`:
  `.profiles/samply/aw5-w{N}/` per-bench per-entry artefacts.
- **Bootstrap regen per wave** where the IR or emitter changes.

## Successor chain

AW-V closes green → AX opens (replay tooling, snapshot persistence,
incremental re-parse, structural-default recovery, subsystem closures).
AX substrate preserved verbatim under AW-V: `DTA_TABLE` const,
`DtaSnapshot`, decision log, per-record snapshot metadata,
`StructuralIndex`, cold-path `dispatch_one` + helpers. Stage-1 bitmap
is deterministic; replay re-derives.

The AW arc closes at AW-V:

- AW-I: substrate landing.
- AW-II: DTA self-host round-trip.
- AW-III: correctness + architectural transposition scaffold.
- AW-IV: interpreter abrogation (helper inlining + wire-contract fixes
  + consumer activations + granular SIMD).
- AW-V: per-shape inline emitter + prototype validation + generalisation.

Indefatigable. DTA viable. Consumer inverted. Every parse entry exceeds
post-AU. sonic-rs + lightningcss parity CI-gated. AX unblocked.
