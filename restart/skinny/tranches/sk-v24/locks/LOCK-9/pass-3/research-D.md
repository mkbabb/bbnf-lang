# LOCK-9 · Family 9D — LAZY-SURFACE (streaming-first scanners, cost-model kernel selection)

*parse-that Rust scanner & kernel plane · pass-3 research · standalone.*

## 0. The family, stated honestly, and the law it collides with

Family 9D proposes scanners as **incremental lazy planes over input windows**, with
**kernels selected by a cost model at combinator-compile time**, and **streaming parse as the
design center**. Two claims are bundled here, and they must be split because one is dead-on-arrival
under the constitution and one is the durable center.

The binding refutation is **SPEC §0 (line 51): "fold-over-lazy-cursor dead (2.3–4.9× behind)."**
A lazy/streaming cursor that folds byte production into the parse hot loop was *measured* 2.3–4.9×
slower and is carried as law across campaigns. Any 9D formulation that reintroduces a per-byte
window-boundary branch into the scanner inner loop reproduces exactly that counterexample. So 9D's
strongest form is a **disaggregation**:

- **(A) Streaming/lazy input surface** — must be developed, then bounded by three facts below; it
  survives only as a *bounded-window boundary at the input edge*, never a lazy cursor threaded
  through the kernels.
- **(B) Cost-model kernel selection at compile time** — orthogonal to streaming, does not touch the
  fold-over-cursor law, and is the defensible durable center. It has an extant engine home.

## 1. Substrate census — F4 answered as data

### 1.1 There is NO streaming input consumer (grep, exhaustive)

`rg 'feed|push_bytes|Decoder|BufRead|document_stream|from_reader|io::Read|Read::'` over
`rust/parse_that/src` + `rust/regex/src` returns **zero** streaming call-sites. Every match on
`chunk`/`stream` is the SIMD-stripe sense (16-byte `u8x16` / 64-byte `PaddedChunk`), not input
feeding: `span_parser/scan.rs:139`, `scanners.rs:158`, `state.rs` `PaddedChunk`. Every shipped
grammar builds a **whole owned tree** over a **whole owned `PaddedView`**.

The parse entry `parse(&self, src: &'a str)` (`parse.rs:164`) calls `ParserState::new(src)`
(`parse.rs:147`), which at `state.rs:285-287` runs `allocate_padded_buf(src.as_bytes())` — a
`Vec<PaddedChunk>` of `n_chunks = (len+64).div_ceil(64)` cache-line-aligned chunks, an owned copy
of the *entire* input with a 64-NUL tail (`INPUT_PAD_BYTES = 64`, `state.rs:57`). This owned padded
copy is the physical thing that pins the whole document; `&'a str` borrows in `StringPayload::Borrowed
{start,end}` (`decode.rs:36`) and span leaves add a second pin.

**Verdict for F4:** whole-document-granularity streaming **does not reduce peak tree RSS** — the
JSON/CSS AST and the decode arena are whole-document, and the `Borrowed` payload retains slices into
the source. Streaming can bound only the *input buffer*, never the *output tree*, unless paired with
an incremental tape-emit boundary (the byte-exact tape crossing, whose full-walk variant is a
measured non-goal). Say so plainly: **on the shipped JSON+CSS grammars, streaming is an input-edge
convenience, not a memory-footprint win.**

### 1.2 `lazy()` is lazy *construction*, not lazy *input*

`lazy.rs:49-65`: `lazy(f)` wraps `LazyParser` (`lazy.rs:20-47`) in an `UnsafeCell`, memoizing the
**constructed `Parser`** in `cached_parser` on first call. It exists to break recursive-grammar
construction cycles — it has nothing to do with input windows. 9D cannot borrow the name; the
lazy-input plane would be net-new.

### 1.3 Backtrack is pure offset-rewind, NO packrat memo on the Rust side

Backtracking is checkpoint-of-`state.offset` (a `usize` index into whole `src_bytes`), restored on
failure — **no memo table, no retained input dependency**. `state.rs:202-254` `ParserState` carries
`offset` + `furthest_offset` and no memo field (`rg 'packrat|memo'` in `state.rs` = 0). Packrat is a
TypeScript-side facility, not present in the Rust core. Restore idioms enumerated:

| Site | File:line | Rewind mechanism |
|---|---|---|
| `or` (alternation) | `combinators/methods/alternation.rs:12,20-24` | `let checkpoint = state.offset` → restore both arms; `furthest_offset = max(furthest, offset)` |
| `many` | `combinators/methods/repetition.rs:24,38` | `prev_offset` per-iter; restore on inner fail |
| `minus` | `combinators/methods/minus.rs` | 4 checkpoint idioms |
| `recover` (CSS) | `combinators/methods/recover.rs:25,34,41` | checkpoint + `snapshot_diagnostic` + sync-scan |
| span-parser | `span_parser/{combinators.rs:13-32, sep_by.rs:32-88, assertions.rs:16-24, mod.rs:159-170}` | `FLAG_SAVE_STATE`-gated checkpoint |

**Consequence for 9D-A (decisive):** because there is no memo retention pinning arbitrary past
offsets, the *only* thing forcing whole-input residence is the owned `PaddedView` + `Borrowed`
slices. If backtrack rewind distance is **bounded**, a bounded ring buffer over a chunked reader
is sound — the parser never needs an offset older than `offset − max_rewind`. This is the single
hinge on which 9D-A's streaming feasibility turns, and it is directly measurable.

## 2. Mechanism made concrete

### 2.1 9D-A — bounded-window input surface (the streaming half, honestly scoped)

Not a lazy cursor. A **ring-buffered reader boundary at the input edge** that preserves the two
kernel invariants exactly:

1. **Padded-tail invariant per window.** Every window handed to a scanner must carry the
   `INPUT_PAD_BYTES = 64` NUL tail so `PaddedView::get_unchecked(i..i+STRIDE)` needs no per-chunk
   tail guard (`state.rs:114-151`, `ws_comment.rs:117`). A ring buffer must over-allocate by 64 and
   re-pad at each refill boundary — a cold, per-refill cost, **never a per-byte hot-loop branch**
   (that is the fold-over-cursor trap). Refill cadence = window size (e.g. 64 KiB), so the
   amortized boundary cost is O(input/window), negligible.
2. **Bounded rewind window.** The ring holds `[offset − R, frontier + 64]` where `R` = the measured
   P99/max rewind (§4). A restore that seeks before `offset − R` is a hard error (unsupported by the
   streaming surface); the whole-`PaddedView` path stays the default and the fallback for
   unbounded-rewind grammars.
3. **`Borrowed` payloads become `Owned` under streaming.** Because a streamed window is transient,
   `StringPayload::Borrowed {start,end}` (`decode.rs:36`) cannot outlive its window; the streaming
   surface forces the `Owned {arena_offset,len}` path (`decode.rs:37`) — decode-to-arena is already
   the escape path (`decode_json_string_to_arena`, `decode.rs:52`), so this is a policy flip, not
   new machinery. This is the concrete RSS story: streaming trades source-slice borrows for arena
   copies; a footprint *win only when input ≫ retained tree*, a *loss* on borrow-heavy small inputs.

API shape (net-new, additive — the whole-doc `parse(&str)` is untouched and stays the fast default):
`fn parse_reader<R: BufRead>(&self, r: R) -> Option<Output>` driving a `RingView` that implements the
same padded-load contract per window. Type-level surface follows nom `Streaming`/winnow `Partial`:
an `Incomplete{needed}` result keeps the boundary out of the hot path.

### 2.2 9D-B — cost-model kernel selection at combinator-compile time (the durable center)

The scanners already carry **hand-tiered kernel ladders** chosen by static branch, not by a model:
`ws_comment.rs:9-18` — tier 1 scalar single-byte short-circuit (bails ~300 no-op CSS WS sites),
tier 2 one-`u8x16` classify (covers ~100% CSS / essentially all JSON WS), tier 3 cold 64-byte stripe
walker; `number_simd.rs` per-arch NEON/scalar; `structural_bitmap.rs`/`quoted_simd.rs` full-loop
SIMD. These tiers are exactly a **candidate set awaiting a ranker**.

The ranker already exists on the bbnf side and is the physical home 9D-B plugs into:
`skinny/crates/passes/src/backend_egraph.rs:36` `select(rule_id, candidates: Vec<BackendCandidate>)`
→ `DecisionCostModel` (`:83`) → `Extractor::new(&graph,&cost_model)` (`:84`) picking min-cost over
`perf_cost` / `capacity_cost` / `static_size_cost` (`:21-23`), schema
`sk-v13-decision-active-cost-v1`. 9D-B's mechanism: **emit each scanner tier as a `BackendCandidate`
for its grammar rule** (`?w` skip-WS, number leaf, quoted-string leaf, structural scan) tagged with
grammar-derived facts — expected run length, class density, whether the leaf sits on a hot
repetition spine — and let the extant CSP/e-graph extractor pick the tier at generation time. The
hand-tiered `if`-ladder in each scanner is then *generated* per-site from the winning candidate
rather than hard-coded. This satisfies the constitution's pluggable-decision-point edict with zero
new engine.

**Why 9D-B does not touch the fold-over law:** selection happens at *compile/generation* time and
lowers to the *same* branch-free kernel the hand ladder emits today; there is no runtime cursor, no
per-byte dispatch. It is a strict generalization of an existing static choice.

## 3. Integration seams (path:line)

- Whole-input pin to relax for 9D-A: `state.rs:285-287` (`allocate_padded_buf`), `state.rs:57`
  (`INPUT_PAD_BYTES`), `state.rs:114-151` (padded-load contract a `RingView` must re-implement).
- Borrow→own policy flip: `decode.rs:35-37` (`StringPayload` Borrowed/Owned), `decode.rs:52`
  (`decode_json_string_to_arena` — already the Owned producer).
- Rewind-instrumentation targets (§4 histogram): `alternation.rs:12`, `repetition.rs:24`,
  `minus.rs`, `recover.rs:25`, `span_parser/mod.rs:159`, `state.rs:211` (`furthest_offset`).
- Cost-model plug (9D-B): `skinny/crates/passes/src/backend_egraph.rs:21-23,36,83-87`
  (`DecisionCostModel`, `select`, `Extractor`); candidate tiers to lift:
  `ws_comment.rs:9-18,113-122`, `number_simd.rs` per-arch entries, `structural_bitmap.rs`,
  `quoted_simd.rs`. bench/report cost lineage: `skinny/crates/bbnf-bench/src/report.rs`.
- Kernel home for the lowered candidates: `skinny/crates/bbnf-simd/src/aarch64/`
  (`structural_classify_64.rs`, `movemask.rs`, `byte_class_from_table_64.rs`,
  `bitmap_next_set_bit.rs`) — whichever home (9A shared vs 9B own) wins; 9D-B stays home-agnostic.
- Stop-early adjacency: the byte-exact tape crossing's `4.4× stop-early` is the one streaming-shaped
  win already banked — parse can *cease reading* once committed; a superset of what 9D-A offers on
  truncatable prefixes, and it needs no ring buffer.

## 4. Exactly what a prototype must test (to move this family)

1. **Rewind histogram (the hinge).** Instrument the six restore sites to record
   `furthest_offset − offset` at each rewind; run the JSON corpora (`apache-builds`, `canada`,
   `citm_catalog`, `data`, `data_xl`, `twitter`) + CSS (`bootstrap`, `normalize`, `tailwind-output`),
   diagnostics on and off, base-pinned. Report **P50 / P99 / max** per corpus. Hypothesis: JSON is
   structural/near-LL(1) → P99 rewind ≈ single-token; CSS `recover`/`css_recovery` paths
   (`recover.rs:25`) are the long tail. **If max is bounded and small, a ring buffer suffices and
   9D-A is buildable; if any corpus shows unbounded rewind, 9D-A is refuted for that grammar and the
   whole-`PaddedView` path is the only lawful surface** — a valid negative-result close.
2. **Streaming byte-parity + no-regression.** A `parse_reader<BufRead>` prototype must produce
   **byte-identical** ASTs to whole-doc `parse` on all corpora, AND the whole-doc cold per-parse
   bench (divan, paired two-window, opposite arm order, N≥200) must show **≥0.98×** — proving the
   ring boundary added no hot-loop branch. Reproducing the fold-over-cursor 2.3–4.9× regression on
   any corpus kills 9D-A.
3. **Cost-model tier selection (9D-B).** Emit `ws`/number/quoted tiers as `BackendCandidate`s;
   confirm the `DecisionCostModel` extractor reproduces the current hand-tier choice on the CSS L4
   parser (no regression vs the ~300 no-op-WS short-circuit sites) and can *beat* it where grammar
   facts differ (e.g. a leaf known to sit on a hot `many` spine promoted to the SIMD-loop tier).
   Gate: generated scanner-tier selection ≥0.98× the hand ladder, per-corpus, base-pinned.

## 5. Prior art with citations

- **simdjson two-stage** (Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDBJ 2019):
  whole-document structural bitmap, explicitly *not* streaming — the padded-tail + stripe-classify
  design parse-that mirrors (`structural_bitmap.rs`) is inherited from it; the block-boundary state
  (in-string, escape parity) is exactly what makes true streaming costly, matching the
  fold-over-cursor refutation.
- **serde_json `from_reader` / `StreamDeserializer`**: the canonical bounded-window Rust streaming
  surface; it streams *top-level values*, not sub-document bytes, and cannot borrow escaped strings —
  precisely the Borrowed→Owned flip 9D-A predicts (`decode.rs:37`).
- **tree-sitter incremental parsing** (Brunsfeld): rewind/reuse bounded by edit locality with a
  retained tree; confirms bounded-rewind is the enabling invariant, but its win is *reparse* not
  *streaming* — orthogonal to 9D-A, and it keeps the whole tree resident (agrees with §1.1).
- **rustc/LLVM lexers, GHC alex**: table-driven scanners select transition kernels at generation
  time from the DFA — the exact shape of 9D-B (compile-time kernel choice), no runtime cursor.
- **nom `Streaming` vs `Complete`** and **winnow `Partial`**: model the two input surfaces as a
  *type-level* distinction (an `Incomplete`/needed-bytes error), the cleanest published way to keep
  the streaming boundary out of the hot path — the template for the additive `parse_reader` surface
  that leaves `parse(&str)` untouched.

## 6. Named risks / counterexamples

- **R1 — fold-over-cursor resurrection (fatal).** Any window-boundary check landing inside the
  `u8x16` classify loop reproduces SPEC §0's 2.3–4.9× death. Guard: boundary cost is per-refill
  only; kernels see a re-padded contiguous window identical to `PaddedView`.
- **R2 — streaming claims a false RSS win.** On JSON/CSS the tree + arena are whole-document
  (§1.1); asserting a memory reduction is dishonest. The only true reduction is input-buffer bound,
  and only when input ≫ retained tree; borrow-heavy small inputs *regress* (Borrowed→Owned copies).
- **R3 — unbounded rewind.** If the histogram (§4.1) shows a grammar with rewind ≥ window, the ring
  buffer cannot serve it; 9D-A falls back to whole-`PaddedView`. Close as measured negative.
- **R4 — consumer-less substrate (Y.13).** A `parse_reader` with no in-wave consumer is forbidden;
  it must ride a real reader-fed workload (stdin JSON / large-file CSS) in its own gate.
- **R5 — 9D-B double-homing the cost model.** The ranker lives in the graph-engine plane
  (`backend_egraph.rs`); 9D-B must *consume* it, never fork a scanner-local cost model — that
  violates the one-engine edict. 9D-B contributes candidates + grammar facts only.
