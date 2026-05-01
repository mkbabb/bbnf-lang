# Tranche AW-V — Fn-per-Rule over Shape Templates

> **W2.1 CLOSE — hard gate MET, sonic-rs beaten on every entry (2026-04-17+).**
> Prototype `crates/bbnf-json-prototype/` (cherry-picked 2026-04-17
> HEAD `f8e56d50`) beats sonic-rs twin-pair on all 5 JSON entries
> single-thread NEON:
> data_s 0.94×, twitter 0.89×, citm 0.89×, canada 0.90×, data_xl 0.90×
> (ratios of prototype/sonic ns/iter; gate was ≤1.10). Samply: one
> monomorphised symbol at 91.15% self-time (sonic's twin is ≤88%
> over two symbols). `nm` verification clean on the six historical
> scaffold symbols reachable from the bench binary. Close ledger at
> `docs/tranches/AW/AW-V-W2-close.md`; bench artefact at
> `docs/benchmarks/post-AW-V-W2-prototype.json`.
>
> **The substrate is viable.** `bbnf-tape` + `bbnf-simd-scan` + the
> existing miner IR produce a parser that exceeds sonic-rs on
> single-thread NEON with just the W2.1 hand-tuned shape — no novel
> exceed lever required to meet the gate. W2.3 (the novel levers)
> rescopes from "needed to meet exceed-sonic gate" to "optional
> refinements composed with shape-emitter generalisation"; they
> land in W3/W4 alongside shape-mining where they apply, or defer to
> AX where they don't. See §W2.3 rescope.

> **Post-W3-flat rethink anchor.** AW-IV.W3 closed with consumer levers
> ledger-marked active but bench flat (see
> `docs/tranches/AW/research/SYNTHESIS-4-RETHINK.md`). AW-V is now the
> **primary performance path**, no longer just a future tranche. AW-IV
> freezes as a cleanup tranche post-W4 close. AW-V.W1 opens
> immediately after AW-IV.W6 close. Narrowing per the rethink:
>
> - **JSON prototype first** — hand-tuned `crates/bbnf-json-prototype/`
>   validates substrate viability before the emitter generalises.
> - **One-pass parse + monomorphic visitor.** `ValueVisitor` path skips
>   the tape entirely; no second materialisation pass.
> - **`TapeVisitor` path retained.** Tape is the shape-emitter's
>   downstream surface; `TapeCursor` + `Root::View` consumers project
>   from it.
> - **N1 / N2 novel-exceed layer stays OUT of critical path.**
>   Speculative parsing, JIT, PMC feedback, e-graph rewrite codegen
>   defer to AX.
> - **11-shape taxonomy** (H1 audit), not 7. CSS L4 hot/cold
>   partitioning mandatory from W4.1 opening per H2's L1-fit
>   projection.
> - **Governance rule applied across tranches**: IR decisions must be
>   authoritative consumers (compile-time code), not advisory
>   substrate the backend can bypass (runtime advice). Substrate-
>   without-authoritative-consumer rejected at wave close per
>   `docs/instructions/README.md` §Wave-verification-ledger.

AW-V **lifts fn-per-rule recursive descent over shape templates** via
per-shape emitter dispatch + novel algorithmic levers that exceed
sonic-rs on single-thread — not via fork, not via parallelism, and not
by duplicating the existing mining / CSP / e-graph infrastructure. The
DTA-era IR facts (state graph, `GrammarProfile`, `SHAPE_DICT`,
structural alphabet, dispatch-table mining, `disjoint_first`,
`operator_chain`, `pattern_alphabet`, `keyword_stats`, etc.) survive
verbatim as the substrate feeding the **shape classifier** (a thin
pattern-match over existing miner outputs, ~150 LOC — not a new
mining system). Per-shape emitter modules consume the classifier's
output + the existing mining facts to produce sonic-rs-shaped inline
loops per rule. CSP + e-graph substrates unchanged. What is added is
the **per-shape emitter modules** (new TokenStream producers that
lower grammar rules to inline loops) plus algorithmic levers sonic-rs
lacks because sonic is JSON-hand-tuned rather than grammar-derived.

AW-IV closed at a post-W2 floor of ~240 MB/s twitter / 24 MB/s CSS
normalize because the hot path crossed four indirection boundaries
per byte (dispatch helper, residual-helper, second-pass materialisation,
cold-helper BLs in the CSS L4 i-cache-overflowing scaffold). A JSON-only
hand-tuned prototype in an isolated worktree validates that the
existing `bbnf-tape` + `bbnf-simd-scan` substrate supports sonic-rs-
class single-thread throughput (W2.1); novel-exceed levers then push
the prototype past sonic on single-thread without recourse to
document-parallel fork (W2.3); the same per-shape structure then
generalises back through the emitter via a thin shape-dispatch
classifier and per-shape emitter modules (W3).

Six waves; strict sequencing. Prototype isolated until its exceed-
sonic gate passes, then cherry-picked onto master alongside the
AW-V substrate-enabler changes and the emitter-lifted per-shape
pipeline.

## Fn-per-rule over shape templates

The DTA-era IR facts feed the shape emitter at a different level of
granularity than the historical scaffold they were minted for. Five
compilation strategies considered:

| Strategy | Per-state cost | i-cache | Dispatch boundary | Status |
|---|---|---|---|---|
| Table-driven dispatch (AW-II baseline) | runtime `match table.states[N]` | fine | every byte | historical scaffold; retired W0a/W0b |
| State-table inlined (AW-IV W1.4-aggro) | inlined state body | CSS L4 154 KB overflows L1 | 2,283 cold-helper BLs spilled | attempted; perf pathology |
| Fn-per-rule RD (post-AU) | inlined per-rule fn | per-rule size | LLVM inlines across rules | the pre-DTA baseline |
| **Shape-mined per-shape emission (B4)** | **inlined per-shape fn** | **naturally small (7–8 shapes)** | **compile-time resolved at call site** | **AW-V.W3 target** |
| Hand-prototype per-shape | same as shape-mined | same | same | AW-V.W2 validation |

Shape-mining is a **higher-level compilation** than state-table
inlining: 7 shapes vs ~500 states, per-shape loop body stays small,
dispatch is at compile-time-resolved call sites (no runtime state
lookup, no cold-helper BLs). The DTA-era state table feeds the shape
miner; the shape miner feeds the per-shape emitters; the scaffold came
down as the consumer emerged. PSI is selectively bypassed: scalar
payloads direct-write into Columns; non-trivial typed fills stay on
PSI rayon. IR facts are *re-consumed at the right granularity*.

## Scope

1. **Substrate enablers** (B2 + B3): surgical `bbnf-tape` changes
   + one `bbnf-simd-scan::emit` addition that feed the shape emitter.
2. **JSON-only hand-prototype — sonic parity** (B1, W2.1): new crate
   `crates/bbnf-json-prototype/` with hand-tuned per-shape inline
   loops + monomorphic visitor. Bench-gated within 10% of sonic-rs
   single-thread.
3. **Novel-exceed levers** (W2.3): algorithmic differentiators sonic-rs
   lacks. Single-thread **≥ 1.10× sonic-rs on JSON twitter** after W2.3.
   No fork; no core-count cheating.
4. **Shape-dispatch classifier + JSON emitter-lift** (B4 Phase 2): generalise
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

Levers that sonic-rs lacks because sonic is JSON-hand-tuned rather
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

### Lever 7 — Multi-visitor parallel monomorphisation (opt-in; bounded)

Parse once, emit to `TapeVisitor` AND to a user-struct `ValueVisitor`
simultaneously via two visitor arguments. Each visitor method inlines
into its call site at compile time; the emitted body duplicates (code
size) but emission is branchless at the source level. sonic's visitor
is single-target per parse call. Our trait-based approach composes
any finite visitor set.

**Bounded in AW-V per H2's L1-fit analysis** (`aw5-h2-visitor-monomorphisation.md`):
unconditional multi-visitor monomorphisation explodes past L1 even on
JSON (96 pairs × 200 LOC × 25 B ≈ 480 KB). AW-V ships the
`(TapeVisitor, ValueVisitor)` pair **only**, gated via
`#[derive(Visitor)] #[emit_paired_with(V2)]`. User-declared custom
multi-visitor combinations are opt-in at user authorship and land as
AX consumers (see §Deferred — AX below).

## Shape taxonomy — 11 categories (H1-corrected)

Per `aw5-h1-shape-taxonomy-audit.md`, B4's original 7 shapes covered
only ~58–69% of CSS / BBNF hot-path visits under strict accounting.
Four additional shapes emerge from the grammar rule graphs and lift
aggregate coverage to ~93% across the primary grammars:

| Shape | Example rules | Emitter module |
|---|---|---|
| Object | JSON `object`, CSS declaration block | `shapes/object.rs` |
| Array | JSON `array`, CSS selector list, comma-separated values | `shapes/array.rs` |
| String | JSON `string`, CSS `<string>`, BBNF string literal | `shapes/string.rs` |
| Number | JSON `number`, CSS number, Sheets number | `shapes/number.rs` |
| Keyword | JSON `true`/`false`/`null`, BBNF directive prefix, CSS `@`-rule head | `shapes/keyword.rs` |
| Pratt | Sheets operator tower, CSS `calc` / `min` / `max` body | `shapes/pratt.rs` |
| Unordered | CSS `compoundSelector` (5-way independent branches) | `shapes/unordered.rs` |
| **ArgList** *(new)* | **CSS `calc(...)` / `min(...)` / `rgb(...)` / `url(...)`; Sheets `func_call`** | `shapes/arglist.rs` |
| **Flat** *(new)* | **CSS 28 `*Decl` rules + BBNF directive bodies — typed `Seq` with Kw head** | `shapes/flat.rs` |
| **Wrap** *(new)* | **`color`, `atRule`, `range_end` — transparent `Alt(Ref…)` dispatcher** | `shapes/wrap.rs` |
| **HRegex** *(new)* | **`hex`, `cell_ref`, `identifier` — regex leaf with host decode** | `shapes/hregex.rs` |

**Sheets function dispatch correction**: H1 (and P3 samply §6)
confirmed Sheets function names match the generic `identifier` regex;
there is NO keyword set. Function dispatch is ArgList-shape, not
Kw-shape + PHF. Projection updated accordingly.

**Heterogeneous fallback — permanent** (~3–5% of CSS): `funcBody`
(`grammar/css/l4/func-body.bbnf:11`), `customPropertyDecl`
(`grammar/css/l4/properties.bbnf:206`), `genericDecl`
(`grammar/css/l4/properties.bbnf:212`). These three are grammatically
heterogeneous (free-form content, user-extension hooks); no general
shape admits them. They stay on a per-rule generic emitter by design,
not by deferral.

**Pratt AX-incremental caveat**: Pratt-shape admits local reparse only
at the operator-chain level. Editing `a+b` → `a*b+c` changes operator
precedence; re-parse must span the enclosing statement. Documented in
AX.md §incremental contract; no AW-V action required beyond the
annotation.

## Deferred — AX + successor tranches

Not every novel idea folds into AW-V. The following items are
explicitly scoped out to preserve AW-V's viability-proof gate and
prevent scope creep:

**Deferred to AX** (consumer-facing; AX already owns replay / recovery
/ incremental / subsystem closures — these fit the AX substrate):

- **Gradient parsing / `LazyValue` / on-demand materialisation**
  (`aw5-n2-novel-parsing-approaches.md` N2.2). Architecturally
  equivalent to AX's incremental-reparse contract: on-demand
  re-parse of a specified subtree with shape-cached re-entry. Ships
  as `AX.X8 — Gradient parsing consumer`.
- **User-declared custom multi-visitor pairs** (extends Lever 7
  beyond `(TapeVisitor, ValueVisitor)`). Visitors declared by user
  code via `#[derive(Visitor)]`; landed as the AX consumer phase.
- **Speculative parsing with shape-transition Markov predictor**
  (N2.1). The rollback-scratchpad infrastructure fits AX's snapshot
  mechanism; ships alongside the snapshot serdes + resume entrypoint.

**Deferred to AX** (meta-optimisation layers that sit atop AW-V's
viability proof; implementing them before the substrate is proven is
scope reversal):

- **E-graph rewrite codegen** (`aw5-n1-egraph-rewrite-codegen.md`).
  12 rewrites over Shape-Emit IR; +30% on CSS bootstrap, +20% on
  Sheets, etc. Lands as AX's e-graph grammar-rewrite wave.
- **Runtime CPU-capability auto-tuning** (N2.5). Five-variant kernel
  dispatch at runtime; 10–25% on Ice Lake / Graviton. AX.W13 cost-
  grid sweep.
- **PMC-feedback adaptive kernels** (N2.6). Self-tuning via Apple M
  performance counters; experimental; ≥ 10 KB input break-even.
  AX.W13 alongside the autotune cost grid.
- **Cranelift JIT per-schema** (N2.7). Conflicts with AW-V's static-
  codegen invariant; useful as alternate deployment mode. Routed to
  AY's replay + JIT surface, not AX.

**Folded into AW-V substrate (not as novel levers; as refinements)**:

- **N2.3 — kind-separated stage-1 position streams**. Refines Mison's
  query-bitmaps into shape-parser streams; 4 c saved per per-shape
  dispatch. Low-risk; folds into AW-V.W1.2 (`bbnf-simd-scan::emit`
  extension — one additional body fragment per structural-byte
  kind).
- **N2.4 — SIMD-speculative Alt-branch prefix-match**. The natural
  implementation of the **Unordered-shape emitter** at AW-V.W4.1.
  CSS `compoundSelector` (5-way) is the canonical use case; already
  in W4's scope per the plan.

## Architectural thesis

Per the AW-IV profile wave (six samply + static-audit agents at HEAD
`f457b4df`):

1. **The IR substrate is architecturally complete.** `bbnf-simd-scan`
   is algorithmically sufficient (B3); `bbnf-tape` needs surgical
   additions that feed the shape emitter (B2). No crate rewrite; no
   architectural transposition.

2. **The indirection persists at the consumer boundary.** The
   historical scaffold's AltLinear dispatch helper sits across a
   cross-crate boundary; every branch attempt re-dispatches over the
   state table. CSS L4 70.9–78.9% self-time; Sheets 52–72%; BBNF 70%.
   CSS L4's 153.9 KB emitted body sits at 0.1–0.2% self-time because
   the hot path never reaches the outer per-rule `match` — it is
   trapped inside the helper (P2).

3. **sonic-rs's winning shape is replicable.** `parse_object::<DocumentVisitor>`
   + `parse_array::<DocumentVisitor>` cover 81–88% self-time at 2 symbols
   — tight monomorphic inner loops, cached SIMD whitespace bitmap,
   compile-time-monomorphised visitor, zero function-call boundaries
   inside the per-token loop (P5). The same shape lands via
   per-shape inline emission over our existing SIMD + tape substrate.

4. **The generalisation is mechanical.** One new IR pass
   (`shape_dispatch.rs`) classifies each rule into 7 shape categories
   (Object/Array/String/Number/Keyword/Pratt/Unordered). Per-shape
   emitter modules at `crates/core/src/backend/rust/emitter/shapes/`.
   Detectors ground in existing miner outputs; no grammar-name
   branches. Rules without shape match route to a generic per-rule
   emitter body. Coverage: JSON 100%, Sheets 92%, CSS L4 78%, BBNF 75%
   (≥ 80% average).

## Invariants

1. **No deferrals, regardless of newfound scope.** Per
   `docs/instructions/README.md`.
2. **Substrate-with-consumer is one unit of work.** Per
   `docs/instructions/README.md` §code-discipline. Each shape-mining
   detector that lands without a consuming emitter is rejected.
3. **Shape emission is the one hot path.** No hedged replay surface
   inside the tranche. Replay tooling (snapshot, resume, incremental
   re-parse) is routed to AY per AX.md §Architectural-thesis bullet —
   AX handles the broader replay contract without reinstating a
   parallel interpretive path in AW-V.
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
| W1 | Substrate enablers: `bbnf-tape-codegen` subcrate (TokenStream body fragments for 4 residual helpers) + `bbnf-simd-scan::emit` submodule (~300 LOC) + `Columns::push_scalar_payload_*` + monomorphic `Visitor` trait in bbnf-tape | 3 parallel | AW-IV closed | `bbnf-tape-codegen` exposes the 4 helper-body fragments; `bbnf-simd-scan::emit` round-trips a test fragment; `Visitor` trait has `TapeVisitor` + placeholder `ValueVisitor`; every W1 addition feeds the shape emitter |
| W2.1 *(CLOSED 2026-04-17)* | JSON hand-prototype in `crates/bbnf-json-prototype/`, sonic-exceed form. Cherry-picked at `f8e56d50`. | 1 serial agent in `bbnf-wt-aw5-prototype` | W1 closed | **MET BY EXCEED, NOT PARITY**: data_s 0.94× / twitter 0.89× / citm 0.89× / canada 0.90× / data_xl 0.90× sonic-rs ns/iter (5/5 entries beat sonic). Samply: one symbol at 91.15% self-time (sonic's twin ≤88% over 2 symbols). `nm` clean on the six historical scaffold symbols. See `docs/tranches/AW/AW-V-W2-close.md` + `docs/benchmarks/post-AW-V-W2-prototype.json` |
| ~~W2.3~~ *(RETIRED)* | ~~Novel-exceed levers (3 parallel agents)~~ | — | — | **Retired.** W2.1 met exceed-sonic without the novel levers. Levers redistribute: Lever 3 (multi-key SIMD) + Lever 5 (bounded Regex) + Lever 6 (ShapeRef) fold into W3 shape-emitter options; Lever 7 (multi-visitor) bounded to `(TapeVisitor, ValueVisitor)` in W3 codegen; user-declared custom multi-visitor defers to AX |
| W3 | Shape-dispatch classifier + JSON emitter-lift: `crates/ir/src/passes/recognizers/shape_dispatch.rs` + `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar}.rs`; shape-emitter consumes the existing miner IR facts + emits sonic-exceeding per-shape inline loops. Redistributed novel levers 3/5/6/7 fold in per-shape where applicable | 4 parallel | W2.1 closed + cherry-picked *(done)* | emitter-produced JSON parser matches hand-prototype exceed-sonic bench ± 5%; every grammar's `parse()` tail-calls a per-shape emitter fn; wire-contract test per shape category |
| W4 | CSS L4 + Sheets shape coverage: `shapes/{pratt,unordered}.rs` + extend `shape_dispatch.rs` for CSS compound-selectors + Sheets 6-rung Pratt + function-name PHF via shape-mining. ShapeRef dedup consumer activates on CSS (high-repetition workload) | 3 parallel | W3 closed | CSS bootstrap ≥ 1500 MB/s single-thread; tailwind / normalize sonic-parity-equivalent; Sheets parse entries ≥ parity post-AU |
| W5 | BBNF shape coverage + wire-contract pipeline fix for BBNF's `GRAMMAR_PROFILE` silent drop. Per-shape emitter covers BBNF's entry-reachable rule set; any shape miss routes through the generic per-rule fn, never the historical scaffold | 2 parallel | W4 closed | BBNF self-host ≥ 500 MB/s single-thread; `GRAMMAR_PROFILE` literal non-empty for every slot where IR mining produces data |
| W6 | FINAL + 19-entry bench matrix + sonic-rs + lightningcss AST-level parity harnesses CI-gated. Per AX invariant 20 (added post-tranche), semantic `*_parity.rs` harnesses are the forward correctness oracle; tape-record parity against any historical scaffold is not a gate. Document-parallel fork lands as an *amortisation multiplier on top of single-thread exceed*, documented separately — not as an exceed lever | 1 serial + 1 parity-harness agent | W5 closed | every parse entry exceeds post-AU on single-thread; both parity harnesses zero-divergence + CI-gated; verification ledger complete |

## Phases

### W1 — Substrate enablers

Three parallel agents. All additions are additive (new crate, new
submodule, new method, new trait); every landing feeds the shape
emitter.

#### W1.1 — `bbnf-tape-codegen` subcrate

Owner: `crates/bbnf-tape-codegen/` (new workspace member);
`crates/bbnf-tape/src/driver.rs` (annotate source helpers with
`#[body_fragment]`-ish attribute or keep separate; see below).

Per B2 §1, the four residual helpers — `advance_or_pop_with`,
`nearest_variant_frame`, `write_decoded`, `finalise` — expose their
bodies as TokenStream fragments the shape emitter splices inline.
Two approaches:

- **Body-source fragments**: `bbnf-tape-codegen` crate holds the helper
  bodies as stringified Rust source + `syn::parse_str` at emitter-build
  time. Each fragment is a compile-time constant.
- **Generated from annotations**: a `#[export_body_fragment]` attribute
  on the helper fn in `bbnf-tape` produces a paired `pub const
  __<helper>_BODY: &str = "..."` at build time.

W1.1 picks the approach with fewer moving parts (likely the first) and
ships the fragment library.

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
offsets; unit tests verify. `TapeVisitor` emits a shape-emitter-defined
tape on a fixture; `ValueVisitor` compiles against a minimal JSON enum.

### W2.1 — JSON hand-prototype, sonic-parity baseline

Two parallel agents in `bbnf-wt-aw5-prototype` worktree (**not** master).

#### W2.1.a — Prototype crate

Owner: `crates/bbnf-json-prototype/` in the worktree.

Per B1:
- Workspace member depending only on `bbnf-tape` + `bbnf-simd-scan`.
- Single `pub fn parse_json<V: JsonVisitor>(input: &[u8], visitor: &mut V) -> Result<(), ParseError>`.
- Five `#[inline(always)]` per-shape functions: `parse_value`,
  `parse_object`, `parse_array`, `parse_string`, `parse_number`.
- Pure recursive descent via the CPU stack; no dispatch-table indirection.
- Inline SIMD kernels via W1.2's `bbnf-simd-scan::emit` fragments OR
  via direct `#[inline(always)]` fn calls (the prototype picks
  whichever matches sonic's shape).
- Inline Eisel-Lemire for f64 decode.
- Two visitors: `ValueVisitor` (materialises into `sonic_rs::Value`-shaped
  enum; this is the sonic-parity validator) and `TapeVisitor` (emits
  into `bbnf_tape::Columns`; this is the shape-emitter substrate
  validator).

#### W2.1.b — Bench scaffolding

Owner: `crates/bbnf-json-prototype/benches/json_value.rs` (in worktree).

Isomorphic to `crates/core/benches/json/value.rs` — same input corpus
(data_s, twitter, citm, canada, data_xl); one `cargo bench` target per
entry; divvy-mode SIMD-off tests for fallback coverage.

**W2.1 hard gate**: each of {data_s, twitter, citm, canada, data_xl}
within **10% of sonic-rs's ns/iter** on the twin-pair bench (per B1 §7).
Samply on JSON twitter confirms the prototype's top-2 hot symbols
(`parse_object` + `parse_array`) cover ≥ 70% self-time. No out-of-line
`#[cold]` helpers inside the prototype module; `nm` confirms the six
historical scaffold symbols are unreachable from the prototype binary.
**This is the parity baseline** — the shape is correct; exceed-sonic
work is W2.3.

If gate passes: W2.3 opens on the same prototype crate; cherry-pick
to master happens at W3 open after W2.3 lands the exceed gate.

### W2.3 — Novel-exceed levers (WAVE RETIRED; CONTENT PRESERVED)

**Rescope rationale**: W2.1 closed with prototype beating sonic-rs on
every twin-pair entry 0.89–0.94×. The novel levers are no longer
required to meet the exceed-sonic gate — the gate is already met by
shape + inlining discipline alone. The W2.3 **wave structure** (3
parallel agents with an independent gate) is unnecessary. The W2.3
**lever content is preserved** — every surviving lever has a concrete
home in W1 / W3 / AX per the table below.

Full lever accounting:

| Lever | Home | Rationale |
|---|---|---|
| 1 Shape-mined codegen | AW-V.W3 (core thesis; shape-dispatch classifier + per-shape emitters) | The thesis of AW-V; every other lever composes with it. |
| 2 Grammar-specialised SIMD kernel selection | AW-V.W3 via `recognizers/kernel_shape.rs` | IR-cardinality gate `prefer_inline_in_loop`; per-compound dispatch at codegen time. |
| 3 SIMD-parallel multi-key compare | AW-V.W3 Object-shape emitter codegen option | Active when `ObjectVisitor` declares known keys (serde-compat use case); NEON `vceqq_u8` / AVX2 `vpcmpeqb` over up to 16 packed key prefixes. |
| 5 Bounded Regex via inverse-alphabet | AW-V.W3 String / HRegex shape emitters | `pattern.last_byte_set ⊆ structural_alphabet` invariant check per rule; scan bounded at next structural byte when admissible. |
| 6 ShapeRef dedup | AW-V.W3 Object / Array / Flat compound-emit | `SHAPE_DICT.lookup(shape_hash)` short-circuit before the shape emitter's compound-push; substrate already mined. |
| 7 Multi-visitor `(TapeVisitor, ValueVisitor)` pair | AW-V.W3 codegen option (bounded) | Per H2's L1-fit analysis — only the named pair lands in AW-V; user-declared custom pairs → AX. |
| 7-ext user-declared custom pairs | **AX** | `#[derive(Visitor)] #[emit_paired_with]` macro + emitter budget guard; user-authored multi-visitor combinations. |

**W3 is a multi-file wave** consuming these levers as codegen options
in per-shape emitter modules. The levers do not require their own
standalone wave because they're *per-shape codegen features*, not
standalone architectural layers. Each shape module decides which
lever(s) apply to its output.

**Preservation guarantee**: if a planned AW-V work item discovers a
reason any Lever cannot land in its assigned wave, the orchestrator
re-plans-with-more-agents per `docs/instructions/README.md`
§code-discipline. Silently dropping a lever is a deferral violation.

### W3 — Shape-dispatch classifier + JSON emitter-lift

Four parallel agents.

#### W3.1 — `shape_dispatch.rs` IR pass

Owner: `crates/ir/src/passes/recognizers/shape_dispatch.rs` (new).

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

Owner: `crates/core/tests/shape_dispatch_emission.rs` (new).

For each of the 7 shape categories, a fixture grammar rule + a test
asserting the expanded emit matches a canonical shape (per-shape
TokenStream golden file).

#### W3.4 — Integration + regression suite

Owner: `crates/core/tests/json_parity_shape_emit.rs` (new).

Run the full JSON semantic parity suite against the shape-emitted
parser + sonic-rs / serde_json / simdjson-OnDemand; assert zero
divergence at the AST-level comparator. Record-count equivalence
against any historical scaffold is not a gate.

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

Owner: extend `shape_dispatch.rs` + wire CSS L4 rules to the new emitters.

Target 78% coverage per B4. Rules without shape match route to the
generic per-rule emitter fn, which ships field-complete under the
shape-emission-authoritative invariant (no deferral to a secondary
scaffold).

**Hard gate**: CSS bootstrap ≥ 1500 MB/s; samply confirms CSS compound-
selector arms are monomorphic hot symbols (sonic-parity-equivalent);
shape-emitted hot arms dominate self-time (the 154 KB i-cache overflow
from the AW-IV consumer boundary is eliminated).

#### W4.3 — Sheets shape coverage + function-name PHF

Owner: extend `shape_dispatch.rs` for Sheets; wire Keyword-shape emitter
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

Owner: extend `shape_dispatch.rs` for BBNF; wire directive dispatch via
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
compounding (multi-key SIMD ~1.1×, bounded Regex ~1.05×, ShapeRef
dedup workload-dependent), and B4's coverage percentages for grammars
where sonic doesn't exist as a comparator.

| Entry | post-AU | sonic (single-thread) | post-AW-V projected | vs post-AU | vs sonic |
|---|---:|---:|---:|:---:|:---:|
| json twitter | 1967 | 2652 | 2900–3100 | **1.5–1.6×** | **1.10–1.17×** |
| json citm | 2438 | 3062 | 3400–3800 | **1.4–1.6×** | **1.11–1.24×** |
| json canada | 1231 | 1545 | 1850–2200 | **1.5–1.8×** | **1.20–1.42×** |
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
in for workload-size ≥ 1 MB under AX's later wave, fork amortises
2–4× on canada / citm / tailwind / data_xl — *on top of* these
single-thread numbers. Fork is an amortisation multiplier; AW-V is
the algorithmic exceed.

## Critical files

| File | Wave |
|------|------|
| `crates/bbnf-tape-codegen/` (new subcrate) | W1.1 |
| `crates/bbnf-simd-scan/src/emit.rs` (new) | W1.2 |
| `crates/bbnf-tape/src/columns.rs` (push_scalar_payload_*) | W1.3 |
| `crates/bbnf-tape/src/visitor.rs` (new, trait + TapeVisitor + placeholder ValueVisitor) | W1.3 |
| `crates/bbnf-json-prototype/` (new crate in `bbnf-wt-aw5-prototype` worktree) | W2 |
| `crates/bbnf-json-prototype/benches/json_value.rs` (new, in worktree) | W2 |
| `crates/ir/src/passes/recognizers/shape_dispatch.rs` (new) | W3.1 |
| `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar}.rs` (new) | W3.2 |
| `crates/core/tests/{shape_dispatch_emission,json_parity_shape_emit}.rs` (new) | W3.3–3.4 |
| `crates/core/src/backend/rust/emitter/shapes/{pratt,unordered}.rs` (new) | W4.1 |
| `crates/ir/src/passes/recognizers/shape_dispatch.rs` (extend for CSS + Sheets + BBNF) | W4.2, W4.3, W5.2 |
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

## Delete-manifest

The historical scaffold retires inside AX, not AW-V. AW-V ships the
consumer; AX deletes the substrate the consumer displaced. The three
AW audit docs enumerate what comes down:

- [`docs/tranches/AW/audit/dead-code-manifest.md`](audit/dead-code-manifest.md)
  — per-file enumeration of reachable-but-consumer-less substrate
  retired under AX.W0b.
- [`docs/tranches/AW/audit/psi-and-dead-substrate.md`](audit/psi-and-dead-substrate.md)
  — PSI scheduling + dead-tape surfaces that no shape emitter consumes.
- [`docs/tranches/AW/audit/full-codebase-prune.md`](audit/full-codebase-prune.md)
  — cross-crate prune pass: `bbnf-tape-codegen`, deprecated
  `csp_strategy` aliases, `simd-scan::emit/*` surfaces, 7 dead
  `GrammarProfile` slots, ~57k LOC of generated-output residue.

Total reclaim per AX.md §Architectural-thesis item 2: ~78,500 LOC.

## Successor chain

AW-V closes green → AX opens (**the RD reckoning**: gate repair,
routing, structural parity against sonic-rs / lightningcss /
simdjson-OnDemand / serde_json / cssparser, shape-emission authority
per AX invariant 20, e-graph grammar-rewrite subsumption, detector
retirement, document-parallel fork as amortisation multiplier) → AY
opens (**replay + JIT**: decision-log replay tooling, incremental
re-parse, structural-default recovery, parse-step debugger, Cranelift
per-schema). AW-VI is not in the chain.

The AW arc:

- AW-I: substrate landing.
- AW-II: DTA self-host round-trip.
- AW-III: correctness + architectural transposition scaffold.
- AW-IV: helper inlining + wire-contract fixes + consumer activations
  + granular SIMD.
- **AW-V**: fn-per-rule over shape templates — DTA-era IR facts feed
  the shape emitter; per-shape inline emission + algorithmic levers
  that exceed sonic-rs on single-thread NEON. The scaffold came down
  as the consumer emerged; AX completes the retirement.

## Pivot

The W0a.2.h pivot (landed 2026-04-19 in tranche AX) reframes the AW-V
arc retroactively. AW-V.W3–W6 tracked correctness via tape-record
parity against the then-active walker scaffold. That framing was a
category error the plan inherited from the AW-II / AW-III
retrospectives — neither priced in the scaffold distinction between
substrate-as-oracle and substrate-as-consumer-target. Per
`docs/benchmarks/archive/post-AX-W0a2g-progress.md` §Remaining-blockers, four
deferred walker-parity deltas (Flat Next/Skip flatten, Ref→HRegex
Rule wrap, Repeat `iter_count < lo` leak, zero-length Seq elision)
surfaced as irreducible shape-emitter vs walker divergences that no
downstream consumer demands.

Per `docs/tranches/AX/audit/R4-plan-redress.md`, AX closed the loop
with **invariant 20 — Shape-emission authority**: tape shape is
shape-emission-authoritative; correctness is asserted by AST-level
`*_parity.rs` harnesses (JSON/CSS/Sheets/BBNF against sonic-rs /
lightningcss / simdjson-OnDemand + self-parity), not by record-count
or column-layout equivalence against the walker. Walker tape was a
historical scaffold; consumers read through `Root::View` +
`TapeCursor`, not raw records. The six AST-level semantic harnesses
went green on master under the pivot; the walker came down in AX.W0b.

The pivot is not a failure of the shape emitter. The emitter produces
correct parse output at the AST layer. The failure mode was
positioning the walker's emission as the correctness oracle when the
walker was itself the scaffold being retired. Walker-parity chasing
was a category-error the AW-V arc inherited, not an emitter defect.

## Closing

Fn-per-rule over shape templates. Every parse entry exceeds post-AU
on single-thread. Every JSON entry exceeds sonic-rs on single-thread
NEON by ≥ 1.07×. AST-level parity CI-gated against five external
comparators. Fork reserved as amortisation multiplier, not conflated
with algorithmic advantage. AX carries the reckoning; AY carries
replay + JIT.
