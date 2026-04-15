# Research 06 — PSI / DTA / Parallelism (Skeleton + Payload Fission)

*Verbatim deliverable from architecture research agent, April 2026.
The architectural spine of AV — backs Phases 2 (DTA), 3 (stage-B
parallel payload), 4 (stage-C prefix scan), and 8 (document-level
parallel parse).*

---

# Tranche AU — Parallelism & Novel Algorithms for Tape-Compatible Parsing

## Angle headline

**Grammar-fingerprinted skeleton/payload fission: treat the tape as a two-pass write target where phase 1 is a grammar-directed SIMD structural miner emitting unfinalized tape skeletons, and phase 2 is an embarrassingly-parallel payload refiner pass.** The key unlock is that bbnf has something simdjson/sonic-rs/YYJSON do not: *static grammar fingerprints* telling us which records are compound frames, which are scalar leaves, which rules mark document-level independence boundaries, and exactly how wide the payload slot for each variant must be. Where simdjson's stage-1 can only emit structural character indices (it doesn't know what's a rule vs. a pair), bbnf's stage-1 can emit the full `TapeRec` skeleton with correct `kind_meta`, `variant_idx`, `child_off` targets and empty `payload_idx`.

## Motivation from wave-2 hotspots

Profiling-2 makes the picture plain. Every grammar's top self-time sits inside generated rule bodies, not at tape or scanner boundaries:

- JSON canada: `__value` 83.3%, `compute_f64` 11.5%, `push_compound` 4.1%.
- CSS tailwind: `__compoundSelector` 40.0%, `__declaration` 25.7%, `scan_ws_block_comments_slow` 11.6%, `__value` 4.3%.
- Sheets stress: precedence tower 86.3% of self-time.
- BBNF: `__mapped_factor` 35-41%, `__rhs` 10-15%, `__directive` 7-19%.

The dispatch overhead (branch prediction, per-branch `mark_children` + offset snapshot + rollback, `__has_children` bookkeeping) is the cross-bench constant tax. Three orthogonal facts bear on parallelism design:

1. The tape is a flat `Vec<TapeRec>` with append-only semantics, 16-byte records, written in pre-order. This is trivially partitionable if we can compute sub-tape sizes ahead of write.
2. `push_compound` with `child_off` arithmetic is relative to the *global* tape index; parallel workers would need offset remapping at join.
3. The `__value` body at expand.rs:2478 already stages `__payload_tag` + `__payload_f64/_bool/_u8` locals before the terminal `push_leaf_with_*`. The payload fill and the structural push are already logically separate — they're just fused in the same function.

## Novel proposal: two-stage tape emission with a precomputed structural index (PSI)

### The idea

Split every generated rule into two generated halves, with a grammar-level trigger for parallel deployment at document-level list rules.

**Stage A (structural mine).** A single-threaded linear pass over the input using a grammar-derived *dispatch tape automaton* (DTA) — not a parser. The DTA is a DFA-plus-counter that tracks Alt branch selection, Repeat frame counts, and Seq frame advancement *without* executing rule bodies. It reads bytes, recognises opening delimiters (`{`, `[`, `"`, `@media`, CSS-selector start, Sheets operator), and emits the tape skeleton:

- A `TapeRec` for every compound and every scalar leaf — correct `kind`, `variant_idx`, `meta_idx` — but with `span_hi = 0`, `child_off = 0`, `payload_idx = 0`.
- A parallel **PSI** (Vec<PayloadJob>) — one entry per scalar leaf with a non-empty payload kind. Each PayloadJob is (tape_record_index, input_span_lo, input_span_hi, payload_kind).

The DTA state is 4 usize per active Seq/Repeat/Alt frame (call stack depth, which for JSON is ~50 and for CSS is ~8). No generated function calls; one big `match` on current byte + DTA state.

**Stage B (payload fill).** A `rayon::par_iter_mut` over PSI chunks. Each worker owns a chunk range, reads `src[span_lo..span_hi]`, runs the terminal scanner (`scan_number_strict_f64`, `decode_json_string_to_arena`, `parse_hex_color`), and writes the result into `tape.payloads` at a pre-reserved offset. The output payload index is then written back into `tape.records[rec_idx].payload_idx`.

**Stage C (span close).** A prefix-scan resolves `span_hi` and `child_off` for compounds. Because the DTA emitted records in pre-order with Seq/Repeat counters, we know exactly which record is the parent of which — it's a tree-from-depth-tag reconstruction identical to Prüfer-style. This runs in O(n) single-threaded but can be parallelised as a segmented prefix scan.

### Concrete API

```rust
pub struct SkeletonTape {
    records: Vec<TapeRec>,      // stage A output (partial: span_hi=0, payload_idx=0, child_off=0)
    psi: Vec<PayloadJob>,       // stage A output
    frame_depth: Vec<u8>,       // stage A output — depth at each record, for stage C
}

#[repr(C)]
pub struct PayloadJob {
    rec_idx: u32,       // which TapeRec to patch
    input_lo: u32,
    input_hi: u32,
    kind: PayloadKind,  // 1 byte — f64 / u8 / bool / hex_u32 / string_decode / span_only
    _pad: [u8; 3],
}

// Stage A — one linear byte pass, grammar-fingerprinted DFA
fn mine_skeleton<'a>(src: &'a [u8]) -> SkeletonTape;

// Stage B — embarrassingly parallel over psi chunks
fn fill_payloads(src: &[u8], sk: &mut SkeletonTape);

// Stage C — parallel segmented prefix scan
fn close_spans_and_children(sk: &mut SkeletonTape) -> Tape;
```

The generated parser's existing fn-per-rule ABI is retained for diagnostics / recovery paths (which never reached production hot paths) — but the monolithic codegen fast path dispatches to `mine_skeleton` / `fill_payloads` / `close_spans_and_children` instead of the recursive functions currently emitted at expand.rs:2470-2690.

### Data flow

```
   src: &[u8]
       │
       ▼   [stage A: single thread, linear scan, DTA]
   SkeletonTape {records, psi, frame_depth}
       │
       ├────────────────┐  [stage B: par_iter over psi chunks]
       ▼                ▼
   worker 0...N   →  payloads patched in shared records buffer
       │
       ▼   [stage C: segmented prefix scan over frame_depth]
   Tape (finalized child_off + span_hi)
```

## Which grammar benefits most

**CSS L4 tailwind.css (3 MB, 608 MB/s cold).** The profile is 40.0% `__compoundSelector` + 25.7% `__declaration` + 11.6% whitespace — totalling 77% in three scanner-and-dispatch-bound frames. Tailwind is the canonical "list of independent rulesets" shape: the `stylesheet` rule matches `(ruleset | at_rule)*`, and each ruleset's internal tape is size-bounded by its selectors + declarations. These are *safe document-level fork points* — no cross-ruleset state.

Stage B parallelises linearly over payload jobs. On an 8-core M-series chip, the 20 `scan_number_f64` discard sites activated as `-> f64` would saturate the payload workers rather than serialise them. Expected impact: stage A runs at memory-bandwidth speed (~5 GB/s on M-series) for the structural scan; stage B adds ~1-2 ns per payload job across 8 workers; stage C is a ~0.5 ns/record fixup. Composite ≈ 1.2-1.5 GB/s cold on tailwind (2-2.5× current), closing the lightningcss gap.

**Secondary: JSON canada (1293 MB/s, numeric-heavy).** 11.5% Eisel-Lemire decode lifts cleanly into stage B; the 83.3% `__value` self-time shrinks because the DTA's `match` on byte class has one branch per JSON kind (5 branches) and no per-branch `mark_children` / checkpoint / rollback.

**Tertiary: Sheets stress (116 MB/s).** The precedence tower is 86.3% of self-time — but the DTA collapses it entirely. Operator-precedence can be parsed as a *shunting-yard* DTA that emits compound records in tree-unfolding order; the current 6-level function tower is replaced by a single pass with a rank-encoded operator stack.

## Tape invariants preserved

1. **Pre-order record layout.** Stage A emits records strictly in source-order pre-order (DTA tracks open/close of every frame; emits compound header before children). Identical to current codegen.
2. **Record offset stability.** Stage A determines the final `TapeRec` index for every record; stage B/C only mutate fields within an already-indexed record. No compaction.
3. **`child_off` semantics.** The chunks-written-during-construction invariant (`child_off < self_idx` ⇒ has_children) is maintained because stage C writes `child_off` from a depth-tag scan that preserves the existing pre-order rule.
4. **Payload slot ordering.** Payload slots are reserved in tape-record-order (the PSI is built in emission order), so `payload_idx` values are monotonic per-worker. A prefix-sum before stage B hands each worker a disjoint slot range.
5. **View API untouched.** `TapeCursor::children`, `subtree_size`, `variant_idx`, `span_lo`, `span_hi` all read identical bytes from finalized `TapeRec`s. Consumers see no change.
6. **Single-path invariant.** This *replaces* the recursive codegen for the hot monolithic benches; the generated `fn __value` recursive path is deleted (per no-workarounds / one-codegen-path). Diagnostics-enabled builds use the same DTA plus an instrumentation hook.

## Honest risks

- **Overhead floor on small inputs.** For Sheets simple (505 bytes, 158 ns/formula), stage A + C fixed overhead dominates. Mitigation: the codegen emits a byte-threshold gate (`if src.len() < 2 * KB { fallthrough_single_pass() }`) *only as a compile-time inlining decision*, not a runtime branch — fingerprint-driven. Without it, sub-200 ns formulas regress 20-30%.
- **DTA generation complexity.** Building a DTA from BBNF requires projecting Alt/Seq/Repeat onto a finite counter-DFA. Grammars with nested optional-with-lookahead (BBNF's `__mapped_factor` with its `(->  __value_expr __type_annotation?)?`) require counter states, not pure DFA. This is the grammar-analysis lift — feasible but non-trivial; roughly 3-4 tranches of work to generalise.
- **Memory bandwidth ceiling.** Stage B writes to the shared records buffer; cache-line contention if workers' `rec_idx` values are close. Mitigation: chunk PSI into cache-line-aligned strides (4 records / 64 B) so false sharing is bounded.
- **Thread-startup cost.** Rayon's first `par_iter` pays ~1-2 µs. For per-parse workloads below 100 µs (JSON data: 2 MB at 2 GB/s = 1 ms, so fine; bbnf json.bbnf: 483 B / 266 MB/s = 1.8 µs, *not* fine), the fork overhead dwarfs the gain. Fingerprint gate must exclude grammars where expected parse time < 50 µs. A pre-warmed persistent worker pool (lazy_static rayon pool) removes the first-call tax but introduces test-isolation concerns.
- **Recoverable errors.** The current parser's diagnostic path relies on `furthest_offset` tracking inside ParserState. The DTA has no backtrack in the hot loop, so diagnostics require a fallback pass — which *is* a second codegen path. This tension needs a principled resolution: either the DTA itself becomes diagnosis-capable via replay, or diagnostics-mode is explicitly a different build (acceptable under no-workarounds if both paths share the underlying grammar fingerprint).

**What is fundamentally uncopyable by simdjson/YYJSON/sonic-rs:** they have no grammar fingerprint. They cannot know that `colorType` has 9 keywords, that `stylesheet` is a list-of-ruleset at document level, or that `number` returns f64 whereas `ident` returns Span. Every optimisation they bolt on is grammar-specific code hand-written by a human. bbnf's DTA is synthesised from the grammar automatically — the compile-time work the user pays once becomes runtime vectorisation the grammar's consumers get free. That is the moat.

**Key file paths referenced:**
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AU/profiling-2.md`
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated.rs` (lines 14143, 15181, 15407, 15896, 17569)
- `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/json_monolithic/expand.rs` (lines 2470-2690 = `__value`)
- `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/state.rs` (ParserState at 165-209)
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/builder.rs` (TapeBuilder, push_leaf_with_*, with_capacity)
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/tape.rs` (TapeRec 16B layout, TapeOffset semantics)
