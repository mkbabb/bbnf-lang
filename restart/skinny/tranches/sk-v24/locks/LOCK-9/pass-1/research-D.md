# LOCK-9 family 9D — LAZY-SURFACE (streaming-first scanners; cost-model kernel selection)

Charter: the scan/kernel layer reborn streaming-first — scanners as incremental lazy planes
over input windows; kernels selected by a cost model at combinator-compile time; streaming
parse becomes the design center. This report develops 9D alone to its strongest lawful form,
names its refutations, and states exactly what a prototype must measure to move it.

## 0. The one fact that governs everything: the whole-input padded model is the antithesis

parse-that's entire scan plane is built on an EAGER whole-input invariant. `ParserState::new`
(`parse-that/rust/parse_that/src/state.rs:285`) allocates a 64-byte-aligned copy of the
**whole** input plus `INPUT_PAD_BYTES = 64` trailing NUL (`state.rs:57`, `allocate_padded_buf`
`:82`). The `PaddedView<'a>` witness (`state.rs:122`) certifies at the type level that any
fixed stripe load at offset `i` with `i + STRIDE <= len + 64` needs **no per-block tail
guard**. Every scanner consumes this witness: `structural_bitmap.rs` classifies 64-byte
stripes and CTZ-iterates the mask (`classify_stripe` `:49`, `STRIPE=64` `:31`, doc `:19`),
`quote_parity.rs` (`compute_in_string_bitmap :121`, `STRIPE=64 :34`), `decode.rs`,
`ws_comment.rs`. This is precisely the simdjson design (`SIMDJSON_PADDING = 64`, whole-buffer
stage-1 structural index — Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDB J.
28 (2019)). Streaming-first **contradicts this witness**: a window's tail is not the input's
tail, so offsets cannot be clamped to `len`, and the 64-NUL guarantee that elides the tail
guard does not hold at a window boundary. 9D's mechanism must dissolve or generalize the
PaddedView invariant, and its honest cost is exactly the tail guard PaddedView was built to
remove. This is the family's central tension and the thing a prototype must price.

## 1. The mechanism, made concrete

**Partiality-as-witness-type, mirroring PaddedView — the single-path formulation.** Do NOT
fork the combinator set into streaming/complete variants (nom's model — `nom::bytes::streaming`
returns `Err::Incomplete(Needed)` vs `::complete` treats EOF as final; a doubled API that
violates the one-path edict P5). Instead follow winnow's `Partial<I>` model: partiality is a
property of the *input witness*, not of the combinators. Introduce a `Window` witness carrying
(a) the current ≤64-byte block view, (b) the threaded inter-block scanner carries, and (c) a
`final: bool` partiality flag. The eager whole-input parse becomes the **degenerate case**: one
window equal to the whole padded buffer, all carries zero, `final = true`. Streaming is the
general case: windows fed incrementally, carries threaded across boundaries, `final = true` only
on the last window. One code path; the eager path is a special case of the streaming path — no
orthogonal subsystem (satisfies no-orthogonal-codepaths, KISS/DRY).

**The carries ARE the streaming vocabulary, and bbnf-simd already speaks it.** The reason
streaming SIMD JSON is hard is cross-block state (string-parity, escape parity, comment
interiors, bracket depth). `crates/bbnf-simd` already threads every one of these as an explicit
inter-block carry: `bitmap_prefix_xor_64(mask, carry_in) -> u64` (`lib.rs:169`),
`escape_mask_64(bs_mask, bs_carry_in) -> (u64, bool)` (`:174`), `comment_body_mask_64(...,
CommentCarry) -> (u64, CommentCarry)` (`:316`), `bracket_depth_mask_64(..., depth: i32) ->
(u64, i32)` (`:336`); `scan_dispatch` (`lib.rs:105`) is already a block-at-a-time 64-byte
window loop with `cursor += 64` and a scalar tail. bbnf-simd's kernels are the *resumable*
form; parse-that's PaddedView scanners are the *whole-buffer* form of the same computation.
9D = refactor parse-that's scanners onto the carry-threaded window form bbnf-simd already
exports, so consumed windows need not be retained by the SIMD layer. This makes the
kernel-sharing question (9A's territory) fall out physically: the streaming primitive set is
bbnf-simd's carry kernels, and the `Window` witness is the shared calling convention.

**The extant incremental plane is already in the tree — generalize it, do not invent it.**
`ParserState` carries `ws_bitmap: u64` + `ws_bitmap_start: usize` (`state.rs:228-243`): a
64-byte window bitmap over whitespace, refreshed on demand so consecutive `?w` calls do not
re-scan. This is a single ad-hoc lazy window cache. 9D's `Window` plane is that pattern lifted
to first class for every scanner (structural, quote-parity, ws, comment, bracket), each with its
own threaded carry instead of one bespoke `ws_bitmap`.

**Cost-model kernel selection at combinator-compile time.** At parser construction — the
`Parser::new` / scanner-compile seam (`parse.rs:74`; for the enum-dispatched second tier,
`span_parser/`) — derive a `KernelPlan` from static grammar properties and store it in the
scanner node; the window-scan loop reads its plan (live in-wave consumer, Y.13). The plan is
composed from precedents already in-tree, not a new subsystem: alphabet admissibility from
bbnf-simd's `lo6_table_admissible` (`dispatch.rs:101`, picks NeonTbl4 vs Scalar); the
prefilter entry-byte tier from PROVE-10 `Prefilter::from_dfa` (Byte1/2/3 → memchr/2/3, else
None); the window stride. This folds PROVE-10's *runtime* prefilter branch and bbnf-simd's
`OnceLock` runtime dispatch (`dispatch.rs:58`) into a *compile-time-selected* plan chosen once
per parser node — eliminating per-call dispatch, the pluggable-decision-point shape.

## 2. Substrate and integration seams (path:line)

- `state.rs:57` `INPUT_PAD_BYTES=64`; `:82` `allocate_padded_buf`; `:122` `PaddedView`; `:226`
  `padded_buf`; `:285` `ParserState::new` — the whole-input allocation site 9D must generalize.
- `state.rs:228-243` `ws_bitmap`/`ws_bitmap_start` — the ONE extant incremental window cache;
  the seam to lift to a first-class `Window` plane.
- `lazy.rs:20` `LazyParser` / `:49` `lazy()` — **name collision to disambiguate**: this is
  *combinator-graph* laziness (deferred parser construction for recursion), a different axis
  from 9D's *input-window* laziness. The `KernelPlan` derivation rides the same construction
  seam, so the two "lazy" notions meet at `Parser::new` but must not be conflated in prose.
- bbnf-simd `dispatch.rs:58` `primitive_kernels` OnceLock, `:89` `select_backend`, `:101`
  `lo6_table_admissible` (compile-time-admissibility precedent); `lib.rs:105` `scan_dispatch`
  (block-window loop), `:169/:174/:316/:336` the carry kernels (streaming vocabulary).
- parse-that scanners: `structural_bitmap.rs:49` `classify_stripe` (best refactor candidate —
  already stripe-at-a-time with an implicit whole-buffer carry), `quote_parity.rs:121`
  `compute_in_string_bitmap`, `decode.rs`, `ws_comment.rs`, `span_parser/scan.rs`.
- PROVE-10 `automata/prefilter.rs` `Prefilter::from_dfa` + `Dfa::find` — the cost-model tier
  the `KernelPlan` consumes; note its `vshrn #4` movemask asm (REPORT §2c) is the idiom 9D
  must preserve.

## 3. Prior art with citations

- **simdjson** (Langdale & Lemire, VLDB J. 2019): whole-buffer padded stage-1 is SOTA;
  `SIMDJSON_PADDING=64` is the exact mirror of `INPUT_PAD_BYTES`. Grounds *why* the eager model
  exists and *why* streaming is hard. Its `document_stream` streams **ND-JSON at whole-document
  granularity**, never a partial document — the realistic streaming unit for 9D is a complete
  small document, not a byte-partial one.
- **winnow `Partial<I>`**: partiality as an input-type wrapper, single combinator set — the
  SOTA design 9D adopts (partiality as witness, like PaddedView).
- **nom `streaming` vs `complete`**: the forked-combinator anti-pattern (`Err::Incomplete(Needed)`)
  — the design 9D must *avoid* to honor P5 one-path; cited as the negative precedent.
- **tree-sitter `TSInput.read`**: the canonical "lazy plane over input windows" API — a `read`
  callback returning a chunk at a byte offset; incremental reparse reuses unchanged subtrees.
  The API shape for 9D's chunked-feed harness.
- **Ragel / re2c**: DFA scanners emit *resumable* state machines (saved current-state `cs`
  across `write exec` invocations) — the model for "scanner = DFA whose state survives a window
  boundary." Direct overlap with LOCK-5 SCANNER-AS-DFA; a streaming scanner is a resumable DFA.
- **Lemire, *Validating UTF-8 in less than one instruction per byte*** (2020): the continuation-
  byte lookback is the cross-block carry problem in miniature; bbnf-simd `aarch64/utf8/
  validate_block.rs` already solves it block-locally — the template for boundary-correct carries.

## 4. Named risks and counterexamples (the refutation surface)

1. **Backtracking defeats the memory claim (the killer).** parse-that is a backtracking PEG
   library; `alt`/`recover` can rewind past a window. A streaming input that *discards* consumed
   windows cannot serve an arbitrary rewind. If JSON/CSS rewind distance is unbounded (recovery
   to document start), the ring buffer must retain all consumed input — which is the whole-input
   model with a lazy tail, **zero peak-RSS win**. 9D's memory center survives ONLY if a bounded
   backtrack window is *measured* for the target grammars. Unmeasured, the claim is refuted to
   latency/time-to-first-token only.
2. **The straddle cost resurrects the tail guard PaddedView deleted.** A window edge at a
   non-64-aligned point forces either a per-window pad-copy or a straddle branch per boundary —
   the exact per-block guard the eager witness removed. On whole-input throughput benches
   streaming is expected **neutral-to-slower**. Under the SPEC §0/09 refutation law and PT-2's
   posture, 9D must make **zero throughput claim**; its only lawful value is peak-memory +
   latency, itself gated by risk 1.
3. **The 5390 per-parse allocs are untouched** (PROVE-09 §Alloc; PT-2 W4): streaming moves the
   whole-buffer allocation (one padded copy) but not the Vec-growth mass in `many`/`sep_by` +
   string arena. The reward ceiling is the padded-buffer footprint (input + 64 B), known and
   bounded — low-reward, exactly why the close may be a refutation (P4/P39).
4. **Consumer-less substrate (Y.13).** A `Window` plane with no streaming consumer in the same
   wave is forbidden. The prototype must ship an actual chunked-feed harness measuring peak RSS,
   not the abstraction alone.
5. **PROVE-09 movemask law.** Any scanner refactor must keep the `vshrn` narrow movemask idiom
   (single `shrn.8b v,v,#4`), never a double-`vaddv`/double-`addv.8b` reduction (VERIFY attack
   #2: 2271 vs 509 `addv.8b`). Stable NEON must be measured no-regression, paired cold, N≥200.

## 5. What a prototype must test to move this family

- **P1 — one scanner, windowed, no-regression.** Refactor `structural_bitmap.rs` (`classify_
  stripe :49`) to consume a `Window` witness with a threaded carry instead of `PaddedView`.
  Gate: divan paired cold, two windows opposite arm order, N≥200, six JSON corpora, patched
  median ≥ **0.98×** eager baseline (PROVE-09 law); `otool -tv` reduction-insn count ≤ baseline
  (no double-`addv`). Honest expected outcome: throughput-neutral (the block loop is what the
  scanner already does internally); a >2% regression means the straddle guard (risk 2) is real
  and priced.
- **P2 — the streaming consumer, peak-RSS.** A harness feeding JSON in N-KB chunks through the
  `Window` plane, measuring **peak RSS** and **time-to-first-structural** vs the eager
  whole-input parse. Gate: peak RSS strictly < eager (the raison d'être); throughput within
  noise. This is the Y.13 live consumer.
- **P3 — the backtrack bound (decides risk 1).** Instrument `furthest_offset − offset` rewind
  distance across the JSON+CSS corpora (both with and without the `diagnostics` recovery
  feature). If bounded by a small window (e.g. < 4 KB), a bounded ring buffer is sound and P2's
  memory win holds. If unbounded, file the measured refutation: 9D's memory center is defeated;
  only incremental-feed/latency survives. Either outcome is a valid close with an artifact.
- **P4 — compile-time KernelPlan, zero per-call dispatch.** Derive `KernelPlan` at `Parser::new`
  from `lo6_table_admissible` + `Prefilter::from_dfa` + stride; assert via `cargo asm` that the
  scan loop reads a resolved plan with no `OnceLock`/branch per call; a live scanner consumer in
  the same wave. Gate: no dispatch-per-byte in the hot loop; parity with the runtime-dispatched
  baseline.

**Strongest lawful form of 9D:** partiality-as-witness (winnow model, not nom's fork), the eager
parse as the degenerate one-window case, bbnf-simd's carry kernels as the streaming primitive
set, and a compile-time `KernelPlan` folding PROVE-10 + bbnf-simd dispatch — with the family's
value scoped honestly to peak-memory + latency (never throughput) and gated on the P3 backtrack
measurement, which alone can refute the memory center.
