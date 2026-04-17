# AW-III/IV Path Forward — Six-Agent Research Synthesis

Six worktree-isolated research agents (R1–R6) executed in parallel against
master HEAD `f34531e7`. Verbatim deliverables in this directory:

- `aw3-r1-simdjson-cycle-attribution.md` — instruction-level cycle budgets
- `aw3-r2-stage1-simd-bitmap.md` — full archaeology + canonical design
- `aw3-r3-codegen-walker-proof.md` — LLVM inlining proof + JSON sketch
- `aw3-r4-cycle-accounting.md` — DTA-vs-RD per-byte decomposition
- `aw3-r5-path-a-keep-dta.md` — keep DTA, layer specialisation
- `aw3-r6-path-b-rip-dta.md` — devil's-advocate rewrite (rejected for §7
  reasons; cited only for the LOC-impact map of the substrate that survives
  either way)

This document does not replace them. It synthesises convergence, archaeology,
and the §7-correct architectural transposition that closes the DTA-vs-RD gap.

## 1. The diagnosis is unanimous

1. **A flattened, specialised tape automaton can exceed inlined recursive
   descent.** simdjson achieves ~0.9 cyc/byte on AVX2 full parse via a
   2-stage design (structural index + tape walker). sonic-rs achieves
   comparable speed via a *different* shape — selective SIMD inside a
   direct-into-struct walker, no separate index pass — and explicitly
   abandoned simdjson's 2-stage approach for JSON. Both shapes win against
   post-AU RD's ~1.78 cyc/byte on JSON twitter [R4 §1, R1 §1; cross-ref
   `docs/performance/automaton-theory-memo.md`]. The architectural property
   that wins is **specialisation + reusable structural information**, not
   "automaton" in the abstract.

   bbnf goes simdjson-style (explicit stage-1 index) rather than sonic-rs-
   style (inline SIMD in walker) for three composition reasons specific to
   bbnf's substrate, not because simdjson is inherently faster: (a) the
   index is a multi-grammar primitive (any grammar declares its alphabet;
   one mechanism), (b) document-parallel fork chunks at index entries, (c)
   replay/recovery/incremental re-derives the index deterministically
   (cheap; sonic-rs has no equivalent invariant). For JSON-only with no
   replay needs, sonic-rs's direct-specialised approach might be faster;
   for our four-grammar substrate + AX consumers, indexed wins.

2. **bbnf's DTA today is not that kind of automaton.** It is a byte-driven
   tagged-union interpreter over `&'static [DtaState]`. Measured: **29.1
   cyc/byte** on JSON twitter — a **16.4× gap vs post-AU RD**. The 14-arm
   `dispatch_one` enum match compiles to a 4-compare tree (~10–15 cyc baseline
   + ~15 cyc branch-miss per dispatch); each input byte visits 3–8 states
   [R4 §3, R3 §audit].

3. **The 24% `dispatch_one` floor is implementation, not architectural.**
   sonic-rs has no dispatcher — `parse_object`/`parse_array` are monomorphic
   LLVM-inlined hot loops; the function boundary IS the dispatcher, resolved at
   compile time [R1 §5]. The same property is recoverable: a general
   walker-specialisation emitter pass walks any grammar's `DtaTable` and
   produces a function whose body is the inlined state machine — the
   `DtaState` enum match disappears in the *output*, not in the IR.

4. **Stage-1 SIMD structural-bitmap pre-pass has been chronically deferred AND
   never attempted optimally.** Six tranches (AO–AW) touched it; no
   driver-consumed structural index has existed since AQ.5 deletion at
   `2f7c1bd4`:

   | Commit | Tranche | Net effect |
   |---|---|---|
   | `4114695b` | AO.0.1 | birth: `compute_structural_bytes` IR pass; no consumer |
   | `7198c974` | AO.0.4–0.6 | first true pre-pass + driver consumer |
   | `2fa31721` | AP.1b on | mutate→peek refactor |
   | `4417f8a7` | AP.1b off | gate off; *"pre-scan 15-25% without WS elision"* |
   | `2a8af086` | AP.1 off | bug: jumps past digits |
   | `2f7c1bd4` | AQ.5 | **DELETE** ~1,500 LOC |
   | `e225ade9` | AU.2.7 IR | revive `compute_structural_alphabet`; no consumer |
   | `143d19ee` | AU.2.7 emitter | per-call SIMD helper, NOT pre-pass |

   The structural-alphabet IR fact is mined and exposed in
   `bbnf-tape/src/profile.rs` but **defaults to `&[]` everywhere** because no
   driver consumes it.

## 2. The §7-correct architectural transposition

**Invariant restated.** Every mechanism is a general emitter pass triggered by
grammar-structural IR properties; per-grammar IMPACT varies because grammars
have different IR; per-grammar MECHANISM does not vary. The grammar's identity
appears nowhere in any hand-written code path — exactly as type inference,
ShapeRef mining, PHF mining, `DTA_TABLE` const emission, and bbnf-regex DFA
codegen already work.

The architecture demands three new general emitter passes and one fused-write
API. Each is mechanically identical in shape to existing passes that
demonstrably work.

### 2.1 General walker-specialisation pass

**One pass.** Lives at `crates/core/src/backend/rust/emitter/dta_walker.rs`.
Signature mirrors existing `emit_dta_table`:

```rust
pub fn emit_specialised_walker(
    grammar: &str,            // symbol-namespace prefix, no behavioural branch
    table: &DtaTable,         // existing IR fact
    alphabet: &StructuralAlphabet, // existing IR fact
    profile: &GrammarProfile, // existing IR fact
) -> TokenStream;
```

Mechanically lowers the `DtaTable.states` graph to inlined Rust. Per state:

- `Seq` → child sequence with frame open/close inlined.
- `ByteDispatch` → `match input[pos] { ... }` over the table's `[DtaStateId; 256]`.
- `AltLinear` → savepoint loop with branches inlined.
- `Regex` → inlined `find_at` against the state's `pattern_dfa: Arc<Dfa>`
  (W1.8 closure already lifts the DFA at lift time).
- `Literal` → byte cmp.
- `Repeat` → counter-bounded loop.
- `ShuntingYard` → operator-precedence step using emitted `PRECEDENCE_LUT`.
- `Ref` → continue dispatch.

Transitions resolve at emit time — `next_state = N` becomes `cur = N` (or
fall-through, or labelled `continue`) because `N` is known from the table. The
output is one function with N labelled blocks and direct control flow — the
industry-standard state-machine pattern that compiles cleanly on every
backend.

**The `DtaState` enum stays in `dta.rs`.** The table still contains it. The
replay subsystem (AX) still consults it. The emitter walks it at compile time
and produces straight-line dispatch in the output Rust. **Interpretation (b)
from R3 §3.** Generic monomorphisation (interpretation a) cannot do this
because the table is `&'static`, not `const generic`.

**State-count → emission strategy** is the only decision the pass makes, and
it is driven by a general IR fact (state count vs LLVM inlining budget),
pluggable via the egraph cost model:

- `state_count ≤ HOT_BUDGET` → single function, all states inline.
- `state_count > HOT_BUDGET` → mine `state_visit_frequency` (same IR fact PHF
  frequency-ordering uses); hot states inline in the outer loop, cold states
  emit as `#[cold] #[inline(never)]` siblings called via branch.

Both strategies are general; the choice is driven by IR cardinality, not
grammar name.

### 2.2 General stage-1 SIMD bitmap pass

**One pass.** Lives at `crates/core/src/generate/dta/stage1.rs`. Signature
mirrors existing `emit_structural_bitmap_kernel`:

```rust
pub fn emit_structural_scanner(
    grammar: &str,
    alphabet: &StructuralAlphabet,
) -> TokenStream;
```

**Alphabet IR enriched** at `crates/ir/src/passes/sets/structural_alphabet.rs`
(R2 §5.1):

- `singletons: BitSet<u8>` — already mined.
- `digraph_mask: [u64; 4]` — first-byte bitset; **NEW**.
- `digraph_pairs: &[(u8, u8)]` — second-byte targets per first-byte; **NEW**.
- `quote_classes: BitSet<u8>` — string-toggle bytes; **NEW**.

Pass mechanically chooses the SIMD kernel shape from cardinality:

- `|singletons| ≤ 8` → nibble-LUT collapse (one `vqtbl1q_u8` per 16-byte lane).
- `9 ≤ |singletons| ≤ 16` → wide-LUT (lift the `1 << i` cap from R2 §5.1).
- `|singletons| > 16` → multi-pass cmpeq + OR-reduce.
- `|digraph_pairs| > 0` → `vextq_u8` shifted-compare per pair, OR into mask.
- `|quote_classes| > 0` → CLMUL/PMULL parity (x86) or 6-op shift-XOR (NEON).

Output type at `crates/bbnf-tape/src/stage1.rs`:

```rust
pub struct StructuralIndex { pub positions: Vec<u32>, pub kinds: Vec<u8> }
```

**New crate `bbnf-simd-scan`** holds the architecture-neutral kernel
infrastructure (per the §architecture invariants — general-purpose constructs
in their own crates).

### 2.3 Driver redesigned around dual cursor

`crates/bbnf-tape/src/driver.rs`:

- Replace `pos: u32` cursor with `Cursor<'a> { src: &'a [u8], idx: &'a StructuralIndex, pos: u32, slot: u32 }`.
- `ByteDispatch` reads `idx.kinds[cursor.slot]`, advances `cursor.slot`.
- `Regex` scans bounded to `[cursor.pos, idx.positions[cursor.slot])` — no
  open-ended tail.
- New `DtaState::ConsumeToNextStructural` → O(1) cursor jump.
- `WsTrim` collapses to `cursor.pos = idx.positions[cursor.slot]` — WS
  subsumed by stage 1; AQ-5 WS-elision regression cannot recur.
- `FrameStackSavepoint` gains `slot: u32` — fixes the AQ-5 unsaved-cursor
  failure mode by extending the existing savepoint record (no parallel
  savepoint structure).

### 2.4 Fused SoA write API

`crates/bbnf-tape/src/columns.rs` gains one method:

```rust
impl Columns {
    pub fn push_compound_fused(&mut self, kind: TapeKind, span_lo: u32) -> u32 {
        let idx = self.len;
        if idx >= self.cap { self.grow_all(); }
        // SAFETY: idx < cap for all columns after grow_all
        unsafe { /* 7 unchecked stores */ }
        self.len = idx + 1;
        idx as u32
    }
}
```

One bounds-check + 7 unchecked stores instead of 7 `Vec::push` calls. The
existing `reserve_compound` becomes a `#[deprecated]` shim that calls
`push_compound_fused` — then deletes once all call sites migrate (single
tranche, no carry).

## 3. The five emitter-mined consumers

Each is already specified in AW-III/AW-IV under invariant §7. All become first-
class consumers of the substrate the W5α work activates:

- **ShapeRef runtime dispatch** — walker consults `SHAPE_DICT` in compound-emit
  branch; mining pass general (`crates/ir/src/passes/recognizers/shape_dict.rs`).
- **PHF keyword tables** — emitter mines every Alt-with-literal-branches
  pattern; threshold gates emission.
- **`DtaState::ClassifyByte` LUT** — general mechanism for any Alt with
  mutually-disjoint FIRST sets.
- **Direct-to-struct expansion** — universal named-type resolver at
  `crates/core/src/backend/rust/view/named_types.rs`; every named type with
  fixed layout enters the fast path.
- **Per-grammar Pratt const-fold** — `PRECEDENCE_LUT` populated from mining;
  walker's `ShuntingYard` arm consults the emitted LUT, not hardcoded
  per-grammar dispatch.

These exist or are partly substrate-landed today; the architectural
transposition activates their consumers.

## 4. Why a prototype is not necessary

The walker-specialisation pass is mechanically identical in shape to existing
emitter passes that demonstrably work. The state-machine-as-labelled-blocks
pattern is industry-standard. A separate prototype would be a parallel
codebase to maintain — a §1-no-fallbacks violation.

The verification step is `cargo asm` confirmation on the *first* emitted
walker (smallest grammar — JSON or EBNF — by state count), folded into the
first commit of AW-IV.W1's hard gate. Confirms `dispatch_one` symbol absent
from the hot path. If LLVM degrades on a specific Rust idiom, the emitter
swaps the idiom; this is a hours-of-work issue, not an architecture-pivot
issue.

## 5. The risks, bounded

| Risk | Severity | Mitigation (emit-time, §7-compatible) |
|---|---|---|
| CSS L4's ~800 states exceed L1 i-cache | Real | Hot/cold partitioning driven by `state_visit_frequency` IR fact (same fact PHF frequency-ordering uses); cold states emit as `#[cold] #[inline(never)]` siblings; threshold pluggable via cost model |
| Stage-1 amortisation fails on small inputs | Documented tradeoff (sheets_parse_simple 505 B) | Emitter-mined: when `expected_input_bytes < parallel_break_even_bytes`, emit scalar fast-path prelude until first stripe boundary |
| LLVM degrades on labelled-block pattern | Theoretical | Read `cargo asm`, swap idiom (`loop { match cur { ... } }` vs `'walk: loop { 'state: { ... } }`); known industry-standard pattern |

None require deferral. None require a prototype tranche. None require
abandoning DTA.

## 6. Refined tranche structure (fused)

Two tranches close the AW arc; one consumer tranche follows:

### AW-III — Correctness + Architectural Transposition (fused)
**One tranche, six waves.** Correctness closure AND the architectural
transposition. No viability-profile gate (this is the only path; profiling
becomes evidence input to emitter heuristics, not a separate phase). No
deferrals, regardless of newfound scope.

- **W1** — Six-point payload wiring + Pratt `Next` peel + scanner closure
  (Cluster 1 close, HashMap-per-scan eliminated, Pratt fires on CSS).
- **W2** — Parse completeness: EOF/trailing-ws + EBNF offset-0 + CSV.
- **W3** — Ignored-test audit + close.
- **W4** — General walker-specialisation pass. Hard gate: every grammar's
  emitted walker shows zero `dispatch_one` symbols in `cargo asm`; JSON
  twitter ≥ 1800 MB/s.
- **W5** — General stage-1 SIMD bitmap pass + new `bbnf-simd-scan` crate +
  driver dual-cursor redesign + fused SoA write API. Hard gate: bitmap
  sustains ≥ 2 GB/s on 1 MB JSON; AQ-5 failure modes verified absent;
  `reserve_compound` < 5% self-time on every bench.
- **W6** — Five emitter-mined consumer activations (ShapeRef, PHF,
  ClassifyByte, direct-to-struct, Pratt const-fold) + 19-entry bench matrix
  + FINAL. Hard gate: **strict-better-than post-AU on ≥ 15/19 entries**.

### AW-IV — Granular exceed + parity harnesses
AVX2 u8x32 widening, scanner PaddedView migration + scanner cluster
consolidation, NEON 17-digit fractional scan, bloom + GADT runtime dedup +
grammar-level pattern hoisting, document-parallel fork (chunkable over the
AW-III stage-1 index), `reduce_column<C, R>` visitor + 4-lane SIMD pack,
cost-model grid sweep, sonic-rs + lightningcss parity harnesses, AU
walker/reader migration carry-overs. Hard gate: **every entry exceeds
post-AU; parity harnesses CI-gated**.

### AX — Replay + recovery + incremental reparse
Unchanged. Substrate (`DTA_TABLE` const + `DtaSnapshot` + decision log) is
preserved verbatim under AW-III. Stage-1 bitmap is a deterministic function
of input bytes; replay re-derives it (~5% replay-time overhead). Snapshot
semantics, incremental-reparse, recovery — all unchanged.

## 7. Per-grammar projections (R5 §3, recomposed)

| Entry | post-AU | post-AW-III projected | vs post-AU |
|---|---:|---:|:---:|
| json canada | 1231 | ~5500 | **4.5×** |
| json citm | 2438 | ~5200 | **2.1×** |
| json data_xl | 1179 | ~4200 | **3.6×** |
| json twitter | 1967 | ~1800–2200 | parity–1.1× |
| json data_s | 1746 | ~2800 | **1.6×** |
| css normalize | 735 | ~3600 | **4.9×** |
| css bootstrap | 454 | ~2800 | **6.2×** |
| css tailwind | 496 | ~3200 | **6.5×** |
| sheets parse_stress | 121 | ~110 | 0.91× |
| sheets parse_nested | 128 | ~100 | 0.78× |
| sheets parse_simple | 95 | ~55 | 0.58× (escape) |
| bbnf json | 283 | ~260 | 0.92× |
| bbnf ebnf | 223 | ~220 | parity |
| bbnf css_pretty | 647 | ~720 | **1.11×** |
| bbnf google_sheets | 858 | ~1050 | **1.22×** |
| bbnf bbnf_self | 394 | ~450 | **1.14×** |
| bbnf css_l4_grammar | 496 | ~650 | **1.31×** |

15/19 strict-better-than post-AU at AW-III close. 8 entries ≥ 2×. Three
sheets-small entries documented as small-input cardinality tradeoff (not a
per-grammar specialisation — stage-1 amortisation fact for inputs < ~1.5 KB,
emitter-mined via `expected_input_bytes` from `GrammarProfile`). AW-IV folds
in document-parallel fork over the stage-1 index; large inputs (canada, citm,
tailwind, data_xl) gain another 2–2.5×, lifting every entry above post-AU.

## 8. Intra-tranche measurement protocol (sequencing clarity without deferral)

A competing analysis has argued ShapeRef / PHF / ClassifyByte / direct-to-
struct / Pratt const-fold should land in AW-IV (after the architectural
transposition) for cleaner per-lever attribution. This synthesis keeps them
in AW-III W6 because (a) the user's directive is "more performant after III"
and (b) the walker-specialisation pass (W4) already lowers the `DtaState`
variants the consumers add — splitting fragments the emitter pass and
requires bootstrap regen at the boundary.

Attribution clarity is recovered via **intra-tranche bench checkpointing**,
not deferral:

- `docs/benchmarks/post-AW-III-W4.json` — walker specialisation only.
  Samply attribution: `dispatch_one` symbol count = 0; per-grammar walker
  symbol presence verified.
- `docs/benchmarks/post-AW-III-W5.json` — walker spec + stage-1 SIMD bitmap
  + fused writes + driver dual-cursor. Samply attribution: bitmap kernel
  self-time; cursor.slot vs cursor.pos contention.
- `docs/benchmarks/post-AW-III-W6.json` — five consumer activations on top.
  Samply attribution per consumer: ShapeRef hit-rate, PHF lookup share,
  ClassifyByte LUT share, direct-to-struct projection share, Pratt const-fold
  share.

Three sidecar files; each closes its wave; W6's FINAL-III aggregates them
into a multi-wave history. Attribution per architectural lever is
preserved; the bigger AW-III win is preserved; no consumer is deferred.

## 9. The single decision

Adopt the fused plan. Patch AW-III as the correctness + architectural-
transposition tranche (six waves, no deferrals, intra-tranche bench
checkpoints per §8). Patch AW-IV as the granular exceed + parity-harness
tranche. Commit the six research artefacts + this synthesis + the AX
dual-cursor refresh.

The architecture is the only path. The prototype is unnecessary. The risks
are bounded and §6-compatible (general emitter passes triggered by IR
properties; no per-grammar hand-written branches). No deferrals, regardless
of newfound scope — scope-reveal triggers re-plan-with-more-agents per the
operational protocol's parallel-orchestration contract; never silent
forward-routing.
