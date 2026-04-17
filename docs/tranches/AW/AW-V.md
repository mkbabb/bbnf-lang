# Tranche AW-V — Compile DTA/PSI into Hot-Path Code + Novel-Exceed

AW-V **compiles the DTA IR into hot-path code** via shape-mining
+ per-shape inline emission + novel algorithmic levers that exceed
sonic-rs on single-thread — not via fork, not via parallelism, not
by abandoning the DTA. The DTA IR representation (state graph,
`GrammarProfile`, `SHAPE_DICT`, structural alphabet, dispatch-table
mining) survives verbatim as the substrate feeding the shape miner;
the DTA interpreter survives as AX's cold-path replay surface. What
is replaced is the interpreter-as-consumer-on-the-hot-path shape.
What is added is six-plus algorithmic levers sonic-rs lacks because
sonic is JSON-hand-tuned rather than grammar-derived.

AW-IV closed at a post-W2 floor of ~240 MB/s twitter / 24 MB/s CSS
normalize because the walker was still indirected at the `try_branch`
boundary; the `bbnf-tape` residual-helper boundary; the `walk_cursor`
second-pass boundary; and the cold-helper-BL boundary on CSS L4's
i-cache-overflowing walker. A JSON-only hand-tuned prototype in an
isolated worktree validates that the existing `bbnf-tape`
+ `bbnf-simd-scan` substrate supports sonic-rs-class single-thread
throughput (W2.1); novel-exceed levers then push the prototype past
sonic on single-thread without recourse to document-parallel fork
(W2.3); the same per-shape structure then generalises back through
the emitter via a new IR shape-mining pass and per-shape emitter
modules (W3).

Six waves; strict sequencing. Prototype isolated until its exceed-
sonic gate passes, then cherry-picked onto master alongside the
AW-V substrate-enabler changes and the emitter-lifted per-shape
pipeline.

## Compile DTA into hot-path code — not abrogate

The DTA is not abandoned. The DTA IR is compiled into hot-path code
at a different level of granularity. Five compilation strategies
considered:

| Strategy | Per-state cost | i-cache | Dispatch boundary | Status |
|---|---|---|---|---|
| DTA interpreter (AW-II baseline) | runtime `match table.states[N]` | fine | every byte — interpreted | cold-path survives as AX replay surface |
| State-table inlined (AW-IV W1.4-aggro) | inlined state body | CSS L4 154 KB overflows L1 | 2,283 cold-helper BLs spilled | attempted; perf pathology |
| Fn-per-rule RD (post-AU) | inlined per-rule fn | per-rule size | LLVM inlines across rules | the pre-DTA baseline |
| **Shape-mined per-shape emission (B4)** | **inlined per-shape fn** | **naturally small (7–8 shapes)** | **compile-time resolved at call site** | **AW-V.W3 target** |
| Hand-prototype per-shape | same as shape-mined | same | same | AW-V.W2 validation |

Shape-mining is a **higher-level compilation** than state-table
inlining: 7 shapes vs ~500 states, per-shape loop body stays small,
dispatch is at compile-time-resolved call sites (no runtime state
lookup, no cold-helper BLs). The DTA state table feeds the shape
miner; the shape miner feeds the per-shape emitters; the DTA
interpreter survives as cold-path replay of the same state table.
PSI is selectively bypassed: scalar payloads direct-write into
Columns; non-trivial typed fills stay on PSI rayon. Nothing is
abrogated; everything is *re-consumed at the right granularity*.

## Scope

1. **Substrate enablers** (B2 + B3): four surgical `bbnf-tape` changes
   + one `bbnf-simd-scan::emit` addition. All preserve AX's cold-path
   replay surface verbatim.
2. **JSON-only hand-prototype — sonic parity** (B1, W2.1): new crate
   `crates/bbnf-json-prototype/` with hand-tuned per-shape inline
   loops + monomorphic visitor. Bench-gated within 10% of sonic-rs
   single-thread.
3. **Novel-exceed levers** (W2.3): six algorithmic differentiators
   sonic-rs lacks. Single-thread **≥ 1.10× sonic-rs on JSON twitter**
   after W2.3. No fork; no core-count cheating.
4. **Shape-mining IR pass + JSON emitter-lift** (B4 Phase 2): generalise
   the prototype back through codegen. Emitter-produced JSON parser
   matches hand-prototype bench ± 5%.
5. **CSS + Sheets + BBNF shape coverage** (B4 Phases 3–5): extend
   shape-mining to the remaining primary grammars; wire-contract
   pipeline fix for BBNF's `GRAMMAR_PROFILE` silent data-drop.
6. **FINAL + parity harnesses + 19-entry bench matrix**: every parse
   entry exceeds post-AU on single-thread; sonic-rs + lightningcss
   CI-gated. Document-parallel fork remains in AW-V's later-wave
   granular-exceed scope as a *workload-size multiplier only*, not
   as an exceed lever.

## Novel algorithmic levers vs sonic-rs

Six levers that sonic-rs lacks because sonic is JSON-hand-tuned rather
than grammar-derived. Each is grammar-agnostic by construction — they
fall out of shape-mining. All land in the prototype (W2.3), verified
single-thread, then generalise through the emitter (W3+).

### Lever 1 — Shape-mined codegen for arbitrary grammars (the core thesis)

sonic is hand-written for JSON; lightningcss hand-written for CSS;
each new grammar = new hand-written parser. AW-V auto-derives the
sonic-rs-shape from any BBNF grammar. This is the architecture's
irreducible novelty; every other lever composes with it.

### Lever 2 — Grammar-specialised SIMD kernel selection via IR cardinality

B3's pluggable `prefer_inline_in_loop` gate at
`recognizers/kernel_shape.rs` picks per-compound between stage-1-
index and sonic-style inline-SIMD-in-loop based on IR-mined
structural density. Grammars with high structural density
(CSS L4: 17 singletons in ~5-20 KB declarations) use stage-1 index;
grammars with low structural density (JSON: 6 singletons in
long-string-dominated workloads) use inline-SIMD-in-loop. sonic uses
a single hardcoded strategy.

### Lever 3 — SIMD-parallel multi-key compare in `parse_object`

When ValueVisitor's target struct declares known keys (serde-style
`#[derive(Visitor)]`), the emitter produces a SIMD-parallel key-
compare: AVX2 `vpcmpeqb` over multiple packed key prefixes in one
instruction; NEON `vceqq_u8` equivalent. `vpmovmskb` + `tzcnt`
selects the matched key → direct visitor method dispatch. sonic does
linear key-compare at serde-deserialize; we compact N-key dispatch
to one compare. Common case (N ≤ 16 keys): 16× fewer comparison
instructions.

### Lever 4 — Column-parallel SIMD emission

TapeVisitor writes 7 SoA columns per compound record (rule_kind + tape_kind
+ span_lo + span_hi + child_off + variant_idx + sib_skip). Pack into
a 20-byte record, emit as one AVX-256 or NEON Q-register 32-byte
vector store. sonic writes to an AoS u64 tape (8 bytes per record,
compact but algorithmically serial); we emit SoA columns in parallel.
`Columns::push_compound_fused_v32` is the new store API;
`bbnf-tape` §columns gets the vectorised path.

### Lever 5 — Bounded Regex via inverse-alphabet invariant

A regex whose `last_byte_set` (computed from NFA accept-state
incoming transitions) is disjoint from the structural alphabet
bounds its scan at the next structural byte — cursor jumps to
`idx.positions[slot]` in O(1) when the DFA would have otherwise run
to end-of-input. sonic has no concept of structural-bounded regex;
it scans strings until quote, numbers until non-digit. We scan
strings OR numbers until *either* the natural terminator OR the
next structural byte, whichever first. Reduces open-ended scan
tails on malformed inputs; enables speculative short-scan on
well-formed.

### Lever 6 — ShapeRef dedup at parse time

AW substrate: `SHAPE_DICT.lookup(shape_hash)` in the compound-emit
branch. For repetitive compound shapes — CSS `ws : ws` (43× in
bootstrap), `!important` (42× in tailwind), identical selectors,
JSON `null` / `true` / `emptyObject` / `emptyArray` — the hot path
replaces `push_compound_fused` with `push_shape_ref(existing_idx)`.
sonic doesn't dedup. Measurable on repetition workloads (CSS
bootstrap record count drops ≥ 30%); neutral on non-repetitive
(JSON numeric arrays).

### Lever 7 — Multi-visitor parallel monomorphisation

Parse once, emit to `TapeVisitor` AND to a user-struct `ValueVisitor`
simultaneously via two visitor arguments. Each visitor method inlines
into its call site at compile time; the walker body duplicates (code
size) but emission is branchless at the source level. sonic's visitor
is single-target per parse call. Our trait-based approach composes
any finite visitor set.

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
| W1 | Substrate enablers: `bbnf-tape-codegen` subcrate (TokenStream body fragments for 4 residual helpers) + `bbnf-simd-scan::emit` submodule (~300 LOC) + `Columns::push_scalar_payload_*` + `Columns::push_compound_fused_v32` (32-byte vector store, Lever 4) + monomorphic `Visitor` trait in bbnf-tape | 3 parallel | AW-IV closed | `bbnf-tape-codegen` exposes the 4 helper-body fragments; `bbnf-simd-scan::emit` round-trips a test fragment; `Visitor` trait has `TapeVisitor` + placeholder `ValueVisitor`; `push_compound_fused_v32` emits a single AVX-256/NEON-Q store on a fixture; all W1 work preserves `bbnf_tape::driver::dispatch_one` verbatim |
| W2.1 | JSON-only hand-prototype in isolated worktree, sonic-parity form: `crates/bbnf-json-prototype/` per B1 | 2 parallel (prototype-build + bench-scaffolding) in `bbnf-wt-aw5-prototype` | W1 closed | each of {data_s, twitter, citm, canada, data_xl} within 10% of sonic-rs's ns/iter on the twin-pair bench; samply confirms 2 monomorphised hot symbols at ≥ 70% self-time (shape parity with sonic). This is the *parity baseline* before novel levers are applied |
| W2.3 | **Novel-exceed levers** (Levers 3–7 of §Novel algorithmic levers) folded into the prototype: SIMD-parallel multi-key compare, column-parallel SoA emission, bounded Regex via inverse-alphabet, ShapeRef dedup consumer, multi-visitor monomorphisation. Grammar-specialised SIMD kernel selection (Lever 2) wired via `recognizers/kernel_shape.rs` extension | 3 parallel in `bbnf-wt-aw5-prototype` | W2.1 closed at parity gate | **JSON twitter ≥ 1.10× sonic-rs on NEON single-thread** (~2900 MB/s); citm ≥ 1.10× sonic-rs on single-thread; canada ≥ 1.20× sonic-rs on single-thread (NEON 17-digit fraction advantage); no fork; samply confirms novel-lever symbols active on hot path |
| W3 | Shape-mining IR pass + JSON emitter-lift: `crates/ir/src/passes/recognizers/shape_mining.rs` + `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar}.rs`; the novel levers from W2.3 generalise through the shape emitter | 4 parallel | W2.3 closed + cherry-picked | emitter-produced JSON parser matches hand-prototype exceed-sonic bench ± 5%; rules without shape match continue to route through `__dta_walker_inline::run`; wire-contract test per shape category |
| W4 | CSS L4 + Sheets shape coverage: `shapes/{pratt,unordered}.rs` + extend `shape_mining.rs` for CSS compound-selectors + Sheets 6-rung Pratt + function-name PHF via shape-mining. ShapeRef dedup consumer activates on CSS (high-repetition workload) | 3 parallel | W3 closed | CSS bootstrap ≥ 1500 MB/s single-thread; tailwind / normalize sonic-parity-equivalent; Sheets parse entries ≥ parity post-AU |
| W5 | BBNF shape coverage + wire-contract pipeline fix for BBNF's `GRAMMAR_PROFILE` silent drop | 2 parallel | W4 closed | BBNF self-host ≥ 500 MB/s single-thread; `GRAMMAR_PROFILE` literal non-empty for every slot where IR mining produces data |
| W6 | FINAL + 19-entry bench matrix + sonic-rs + lightningcss parity harnesses CI-gated. Document-parallel fork lands as an *amortisation multiplier on top of single-thread exceed*, documented separately — not as an exceed lever | 1 serial + 1 parity-harness agent | W5 closed | every parse entry exceeds post-AU on single-thread; both parity harnesses zero-divergence + CI-gated; verification ledger complete |

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

### W2.1 — JSON hand-prototype, sonic-parity baseline

Two parallel agents in `bbnf-wt-aw5-prototype` worktree (**not** master).

#### W2.1.a — Prototype crate

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

#### W2.1.b — Bench scaffolding

Owner: `crates/bbnf-json-prototype/benches/json_value.rs` (in worktree).

Isomorphic to `crates/core/benches/json/value.rs` — same input corpus
(data_s, twitter, citm, canada, data_xl); one `cargo bench` target per
entry; divvy-mode SIMD-off tests for fallback coverage.

**W2.1 hard gate**: each of {data_s, twitter, citm, canada, data_xl}
within **10% of sonic-rs's ns/iter** on the twin-pair bench (per B1 §7).
Samply on JSON twitter confirms the walker's top-2 hot symbols
(`parse_object` + `parse_array`) cover ≥ 70% self-time. No out-of-line
`#[cold]` helpers inside the walker module. No symbol named
`dispatch_one` / `advance_or_pop_with` / `try_branch` / `walk_cursor`
reachable from the walker. **This is the parity baseline** — the
shape is correct; exceed-sonic work is W2.3.

If gate passes: W2.3 opens on the same prototype crate; cherry-pick
to master happens at W3 open after W2.3 lands the exceed gate.

### W2.3 — Novel-exceed levers folded into the prototype

Three parallel agents in `bbnf-wt-aw5-prototype`. **Opens after W2.1
closes at sonic-parity.** File-disjoint per agent:

#### W2.3.a — SIMD-parallel multi-key compare + multi-visitor monomorphisation

Owner: `crates/bbnf-json-prototype/src/object.rs` (extend `parse_object`);
`crates/bbnf-json-prototype/src/visitor.rs` (extend visitor trait with
key-set declaration).

Implements Levers 3 + 7 from §Novel algorithmic levers. For
ValueVisitor-like targets with known key sets, emit NEON `vceqq_u8` /
AVX2 `vpcmpeqb` over up to 16 packed key prefixes in one instruction;
`vpmovmskb` + `tzcnt` selects match → direct visitor method dispatch.
Multi-visitor: generalise `parse_json::<V>` to `parse_json::<V1, V2>`
optionally; two visitors land in parallel (TapeVisitor + user
ValueVisitor) via compile-time monomorphisation. No dyn dispatch.

**Hard gate**: `cargo asm` shows the SIMD multi-key compare
instruction sequence on object-heavy twitter (13K pairs × ~3 keys
avg). JSON twitter +10% vs W2.1 parity baseline.

#### W2.3.b — Column-parallel SoA emission + bounded Regex via inverse-alphabet

Owner: `crates/bbnf-tape/src/columns.rs` in the worktree (the W1-landed
`push_compound_fused_v32` is the 32-byte vector-store API);
`crates/bbnf-json-prototype/src/string.rs` + `src/number.rs` (bounded
Regex in scan paths).

Implements Levers 4 + 5. TapeVisitor's `begin_compound` + `end_compound`
route through `push_compound_fused_v32` emitting a single AVX-256 /
NEON-Q 32-byte store per record. String / number scan bodies check
`cursor.slot` against the stage-1 structural index — scan bounded to
`[cursor.pos, idx.positions[cursor.slot])`; terminates on structural
byte even if the pattern-natural terminator hasn't been seen.
`last_byte_set` mining runs at W1 codegen for the JSON number /
string patterns; pattern is disjoint from `{ "{", "}", "[", "]", ",",
":" }` ⇒ bound is admissible.

**Hard gate**: per-record store count on TapeVisitor path halves
(one v32 store vs seven scalar stores); JSON canada +15% vs W2.1
(number-heavy; bounded number scan amortises).

#### W2.3.c — ShapeRef dedup consumer + NEON 17-digit fraction inline

Owner: `crates/bbnf-json-prototype/src/object.rs` (ShapeRef dedup
consumer in compound-emit branch); `crates/bbnf-json-prototype/src/
number.rs` (NEON 17-digit fraction body inline from AT.4.3 chronic).

Implements Lever 6. The W1-populated `SHAPE_DICT` (per-grammar) lists
known-repetitive shapes; `parse_object` + `parse_array`'s compound-
emit branches check `SHAPE_DICT.lookup(shape_hash)` before
`push_compound_fused_v32`. On hit, emit `push_shape_ref(existing_idx)`
(one u32 write); on miss, full compound. Typical JSON hits: `null`,
`true`, `false`, `emptyObject`, `emptyArray`.

NEON 17-digit fraction: AT.4.3 chronic finally lands. sonic's NEON
path falls back to scalar after 16 digits; ours handles up to 17
digits inline via `vqdmulh_s16` + multi-lane accumulate. Canada's
long-fraction workload is the direct target.

**Hard gate**: JSON canada (~10K f64s with avg ~12 fraction digits,
some up to 17) sees NEON 17-digit inline fire; canada +20% vs W2.1.
JSON twitter sees ShapeRef fire on null / empty compounds; twitter
+5% vs W2.1 (low-repetition workload; measurable floor).

#### W2.3 composite hard gate

**JSON twitter ≥ 1.10× sonic-rs on single-thread NEON (≥ 2900 MB/s)**,
**JSON citm ≥ 1.10× sonic-rs on single-thread (≥ 3400 MB/s)**,
**JSON canada ≥ 1.20× sonic-rs on single-thread (≥ 1850 MB/s)**.

Samply attribution confirms each novel lever's symbol present on the
hot path. `cargo asm` confirms SIMD instruction emission per lever.
No fork; no core-count assist.

If any entry misses: diagnose the specific lever that failed
(per-agent, file-disjoint) and iterate. No silent forward-routing
per the no-deferrals invariant.

If all three entries pass: cherry-pick the prototype + `bbnf-tape`
W1 changes to master; W3 opens on exceeding-sonic substrate, with
shape-mining inheriting every lever.

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

## Per-grammar projections at AW-V close — single-thread

All numbers are **single-thread NEON on Apple M-class**; no document-
parallel fork. Exceed-sonic via algorithmic novelty, not core count.

Computed from B1's ~1.2 cyc/byte cost model, W2.3's novel-lever
compounding (multi-key SIMD ~1.1×, column-parallel SoA ~1.1×, bounded
Regex ~1.05×, ShapeRef dedup workload-dependent, NEON 17-digit ~1.15×
on fraction-heavy), and B4's coverage percentages for grammars where
sonic doesn't exist as a comparator.

| Entry | post-AU | sonic (single-thread) | post-AW-V projected | vs post-AU | vs sonic |
|---|---:|---:|---:|:---:|:---:|
| json twitter | 1967 | 2652 | 2900–3100 | **1.5–1.6×** | **1.10–1.17×** |
| json citm | 2438 | 3062 | 3400–3800 | **1.4–1.6×** | **1.11–1.24×** |
| json canada | 1231 | 1545 | 1850–2200 | **1.5–1.8×** | **1.20–1.42×** (NEON 17-digit lever) |
| json data_xl | 1179 | 1460 | 1650–1900 | **1.4–1.6×** | **1.13–1.30×** |
| json data_s | 1746 | 2346 | 2500–2700 | **1.4–1.5×** | **1.07–1.15×** |
| css normalize | 735 | — (lightningcss) | 1500–2200 | **2.0–3.0×** | n/a |
| css bootstrap | 454 | — | 1800–2500 | **4.0–5.5×** | n/a (ShapeRef dedup on repetition) |
| css tailwind | 496 | — | 2000–3000 | **4.0–6.0×** | n/a |
| sheets parse_simple | 95 | n/a | 100–140 | **1.05–1.47×** | n/a |
| sheets parse_nested | 128 | n/a | 150–200 | **1.17–1.56×** | n/a |
| sheets parse_stress | 121 | n/a | 150–200 | **1.24–1.65×** | n/a |
| bbnf json | 283 | n/a | 400–600 | **1.4–2.1×** | n/a |
| bbnf ebnf | 223 | n/a | 350–500 | **1.6–2.2×** | n/a |
| bbnf css_pretty | 647 | n/a | 800–1100 | **1.2–1.7×** | n/a |
| bbnf google_sheets | 858 | n/a | 1100–1500 | **1.3–1.7×** | n/a |
| bbnf bbnf_self | 394 | n/a | 600–900 | **1.5–2.3×** | n/a |
| bbnf css_l4_grammar | 496 | n/a | 700–1000 | **1.4–2.0×** | n/a |

**17/17 parse entries exceed post-AU on single-thread.** All five JSON
entries exceed sonic-rs on single-thread NEON by ≥ 1.07×.
Small-input Sheets entries now clear post-AU (novel levers collapse
the per-byte cross-crate dispatch tax that amplified small-input
pathology in AW-IV). CSS / BBNF have no sonic-equivalent comparator;
the vs-post-AU multiplier is the primary validation.

**Document-parallel fork** stays out of this projection. When folded
in for workload-size ≥ 1 MB (AW-VI scope, future tranche), fork
amortises 2–4× on canada / citm / tailwind / data_xl — *on top of*
these single-thread numbers. Fork is an amortisation multiplier;
AW-V is the algorithmic exceed.

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

AW-V closes green → AW-VI opens (document-parallel fork over stage-1
index as a workload-size amortisation multiplier *on top of* AW-V's
single-thread exceed; canada / citm / tailwind / data_xl gain another
2–4× on 4 cores) → AX opens (replay tooling, snapshot persistence,
incremental re-parse, structural-default recovery, subsystem closures).
AX substrate preserved verbatim under AW-V: `DTA_TABLE` const,
`DtaSnapshot`, decision log, per-record snapshot metadata,
`StructuralIndex`, cold-path `dispatch_one` + helpers. Stage-1 bitmap
is deterministic; replay re-derives.

The AW arc:

- AW-I: substrate landing.
- AW-II: DTA self-host round-trip.
- AW-III: correctness + architectural transposition scaffold.
- AW-IV: interpreter abrogation (helper inlining + wire-contract fixes
  + consumer activations + granular SIMD).
- **AW-V**: compile DTA/PSI into hot-path code via shape-mining +
  per-shape inline emitter + **six novel algorithmic levers that
  exceed sonic-rs on single-thread NEON**. DTA IR preserved; DTA
  interpreter demoted to cold-path AX replay; PSI selectively bypassed
  on scalars.
- AW-VI (future): document-parallel fork as workload-size multiplier.
- AX: replay / recovery / incremental consumers over AW-V substrate.

Indefatigable. DTA compiled, not abrogated. Consumer inverted at
shape-granularity. Every parse entry exceeds post-AU on single-thread.
Every JSON entry exceeds sonic-rs on single-thread NEON by ≥ 1.07×.
sonic-rs + lightningcss parity CI-gated. Fork reserved as
amortisation multiplier, not conflated with algorithmic advantage.
AX unblocked.
