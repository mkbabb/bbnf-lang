# LOCK-9 · Family 9D — THE LAZY / STREAMING SURFACE (parse-that rust)

*Charter (9D): "streaming-first — scanners as incremental lazy planes over input windows;
kernels selected by a cost model at combinator-compile time; streaming parse support becomes the
design center." Directive tag: F4 (windowed vs streaming input surface). Family developed alone to
its strongest form; the strongest form is the honest one, and the honest form splits the charter
into a refuted half and a lawful half.*

---

## 0. The governing census (F4, answered as data)

### 0.1 Is there ANY in-wave streaming consumer? — NO. Whole-document owned trees, everywhere.

A repo-wide grep (`feed(|push_bytes|Decoder|BufRead|document_stream|from_reader|StreamDeserializer|
Incomplete|Partial<|StreamIsPartial`, over `rust/` and `typescript/`) returns **zero streaming
producers in parse-that source**. The only hits are competitor benches (`benches/competitors/
winnow.rs` `ModalResult`/`Partial`, `typescript/test/benchmarks/nearley.ts:206` `parser.feed`).
Parse-that itself has no `feed`, no reader, no push/pull refill, no event sink.

The input surface is whole-document and *owned*: `ParserState::new` (`parse_that/src/state.rs:285`)
runs `allocate_padded_buf(src.as_bytes())` (`state.rs:82`), producing an owned, 64-byte-aligned
`padded_buf: Vec<PaddedChunk>` (`state.rs:226`) — a full copy of the input plus `INPUT_PAD_BYTES=64`
trailing NUL (`state.rs:57`). Every SIMD kernel loads from the whole-doc `PaddedView`
(`state.rs:122`, `padded()` at `:353`). The output is a whole owned tree: `JsonValue::Object(Box<Vec<
(Cow<'a,str>, JsonValue<'a>)>>)` (`parsers/json.rs:21`), with per-container `Vec::with_capacity(4)`
grown in the hand-rolled loops (`json.rs:64,97`).

**Consequence (per F4's own instruction):** the only streaming granularity parse-that could offer is
*whole-document* — feed one complete JSON/CSS document at a time out of a stream of documents (the
simdjson `document_stream` / ndjson model). This does **not** reduce peak tree RSS, because each
document still builds its whole owned tree before the next begins. Peak RSS is the *tree*, not the
input buffer; whole-document streaming touches only the buffer. So "streaming parse support becomes
the design center" has **no RSS lever and no consumer** — a consumer-less substrate (Constitution
viii / Y.13), forbidden as a design center.

### 0.2 Rewind distance at restore sites over JSON+CSS — BOUNDED, and near-zero for JSON.

Backtracking is uniformly *checkpoint-local*: capture `cp = state.offset`, restore
`state.offset = cp` on failure. Every restore site follows this idiom:
- `combinators/methods/alternation.rs:13,18,24` — `.or`: `checkpoint = state.offset`, restore twice.
- `combinators/macros.rs:91–242` — `alt!` unrolled 2..=N arms, each a `cp`/`state.offset = cp` pair.
- `span_trait.rs:141,143,152` — `sep_by`: `cp` before each separator; restore on sep-miss or
  element-after-sep failure (rejects trailing separators). Rewind ≤ one separator + one element try.
- `leaf.rs:301,306` — flat N-way alternation checkpoint.

Rewind distance at a restore is `state.offset(at failure) − cp` — how far the *failed* branch
advanced. This bounds the ring buffer a streaming reader would need. By corpus:

- **JSON: P50 = 0, P99 = 0, max ≈ one token (~20 B).** The value grammar does **not** use `.or()`
  chains — it uses **first-byte dispatch**: `dispatch_byte_multi(vec![(b"{",object),(b"[",array),
  (b"\"",string),(b"t",true),(b"f",false),(b"n",null),(b"-0123456789",number)])` (`json.rs:117–125`,
  impl `leaf.rs:340`). Dispatch reads one byte and jumps — **no checkpoint, no restore**. The only
  JSON backtrack is inside a leaf that partially matches then fails (a malformed number/keyword),
  bounded by that token's length. Structural JSON parses with essentially zero rewind.
- **CSS: bounded by a rule/declaration, never the document.** CSS uses `.or()` and `sep_by`
  (`css/declaration.rs:103` `from.or(to).or(pct)`; `css/selector.rs:20` `sep_by_small`). Where a
  selector and a declaration share an ident prefix the failed branch can advance a whole
  selector/declaration before restore. `css/mod.rs:240` already replaced the top-level `.or()` chain
  with byte dispatch ("Byte dispatch instead of .or() chain"), capping the common case. Worst-case
  rewind is one rule's span (tens–low-hundreds of bytes), **not unbounded**.

**Verdict on the ring-buffer question:** backtrack is *bounded* — a bounded ring buffer provably
suffices (JSON trivial; CSS one-rule window). But see §3.1: bounded-but-rewardless, and it
*collides with zero-copy*.

---

## 1. The mechanism, made concrete — reframed to its lawful core

The charter has two readings of "lazy," and parse-that already realizes one:

- **Input-lazy** (a reader pulling bytes on demand into a window, SAX/event emission): **refuted**
  above — no consumer, no RSS lever, and it breaks zero-copy (§3.1).
- **Graph-lazy** (the parser *graph* built on demand to break recursion/cycles): **already shipped**
  as `lazy()` (`lazy.rs:49`), a memoized `UnsafeCell<LazyParser>` whose inner `Parser` is constructed
  once and cached (`lazy.rs:42–46`), used for `json_array`/`json_object` recursion (`json.rs:50,81`).
  "Lazy plane over input windows" in parse-that *means* this — deferred parser construction, not
  deferred input.

The single lawful, consumer-backed residue of 9D is the charter's second clause: **"kernels selected
by a cost model at combinator-compile time."** It exists in embryo and is the family's real
contribution to the gestalt:

- `leaf.rs::take_until_any_span` (`leaf.rs:177`) selects a scan kernel **at parser-construction time**
  from the excluded-byte set cardinality: `One(u8)` / `Two` / `Three` / `Lut(Box<[bool;256]>)`
  (`leaf.rs:182`) — a hand-coded cost model (≤3 distinct bytes → memchr-class compares; >3 → 256-LUT).
- `span_parser/scan.rs` provides the vtable-free enum-dispatched arms the selection lowers to:
  `dispatch_take_until_any1` (`scan.rs:9`, `memchr::memchr`), `_any2`, `_any3`, LUT/SIMD-nibble.
- `bbnf-simd/src/dispatch.rs:42 select_classifier(alphabet:&[u8;64])` is the *same pattern* one level
  up: pick a classifier for a 64-byte structural alphabet at build time.

**The concrete 9D mechanism:** promote the ad-hoc `One/Two/Three/Lut` ladder into a *pluggable cost
model* over scan-kernel candidates, selected at combinator-construction time — the "compile-time" of a
combinator library. Not new machinery for a streaming fantasy; the generalization of code that already
runs, with a live consumer in every `take_until`/scanner site (Y.13 satisfied). It is the natural seam
to **LOCK-2's cost model** (CSP over per-site lowering: kernel, capacity, prefilter tier) and
**LOCK-5's prefilter selection** — 9D contributes the *scanner site's* candidate set and cost inputs
(excluded-set cardinality, expected run length, alphabet separability), never a bespoke solver.

---

## 2. Substrate & integration seams (path:line where extant code serves)

| Seam | Location | Role for 9D |
|---|---|---|
| Whole-doc owned window | `state.rs:82` `allocate_padded_buf`, `:226` `padded_buf`, `:353` `padded()` | The "input window" already exists — whole-doc, owned, 64B-padded. A streaming window would *replace* this; the pad contract (`INPUT_PAD_BYTES=64`, `:57`) is what a ring buffer must reproduce at every refill boundary. |
| Cursor + furthest | `state.rs:210` `offset`, `:211` `furthest_offset` | The only mutable position state. `furthest_offset` (`alternation.rs:17`) is the max-reach watermark — the exact quantity a ring buffer must not have discarded. |
| Checkpoint/restore idiom | `alternation.rs:13–24`, `macros.rs:91–242`, `span_trait.rs:141–152`, `leaf.rs:301–306` | Every rewind site. A ring-buffer window must retain `[min(live checkpoints) .. furthest_offset]`. Bounded (§0.2). |
| First-byte dispatch (rewind-free) | `json.rs:117`, `leaf.rs:315 dispatch_byte`/`:340 dispatch_byte_multi` | Proves the JSON hot path needs *no* backtrack window at all — the template for eliminating rewind by construction. |
| Graph-lazy plane | `lazy.rs:49` `lazy()`, `:20 LazyParser`, `:42 get()` | The real "lazy plane" — parser-graph memoization, already shipped. |
| Compile-time kernel selection | `leaf.rs:177–214` `take_until_any_span` (`One/Two/Three/Lut`), `span_parser/scan.rs:9+` dispatch arms | The lawful 9D core: generalize into a pluggable cost model. Live consumers: every JSON string/number scan, every CSS `take_until`. |
| Kernel producer home | `bbnf-simd/src/lib.rs` (`compact_mask:228`, `byte_class_from_table_64:256`, `find_ascii_set_member64:208`, `structural_classify_64:301`), `dispatch.rs:42 select_classifier` | Where a shared cost-model-selected kernel would resolve. The contract-split movemask producers (LEAN nibble-`vshrn` / DENSE `vpaddq`) are absent here — F1/9A territory, not 9D. |
| Zero-copy output contract | `json.rs:19,29` `Cow::Borrowed(s.as_str())`, `state.rs:172 Span<'a>{src:&'a str}` | The counterexample generator (§3.1): outputs borrow `&'a str` from the *whole* `src`. A sliding window invalidates these borrows. |

---

## 3. Named risks & counterexamples

### 3.1 The killer counterexample: streaming destroys zero-copy.
Parse-that's headline is zero-copy: `sp_quoted_string(...).map(|s| Cow::Borrowed(s.as_str()))`
(`json.rs:29`) and `Span<'a>{ src: &'a str }` (`state.rs:172`) borrow directly from the whole `src`
for the *lifetime of the tree*. A sliding/ring-buffer input window frees bytes behind the cursor —
but the tree still points into them. Streaming therefore **forces owning copies** of every string leaf
(`Cow::Owned`), converting the zero-copy fast path into an allocating one. The refactor that enables
streaming *regresses* the exact metric parse-that optimizes. Decisive refutation of "streaming as
design center."

### 3.2 Packrat incompatibility. R3 §1 records a "packrat epoch" (`934b2fa` S.H1). Packrat memo tables
key on `(rule, absolute offset)` and must survive the whole parse — they pin the entire input.
Bounded-window streaming and packrat memoization are mutually exclusive (cf. ANTLR
`UnbufferedCharStream` requiring explicit markers; PEG packrat's O(n)-memory law). Any 9D streaming
claim must first prove packrat is off on the streamed grammar, or it is unsound.

### 3.3 "Bounded but rewardless." §0.2 proves the ring buffer is small. But small ≠ worthwhile: the
reward for a bounded input window is bounded *input* RSS, while peak RSS is the *tree* (§0.1). The
ceiling is known-nil before any prototype — mirroring PT-2 W4's "high-risk, low-reward by measurement"
arena close (PT-2.md §4 W4): the honest disposition is a *measured refutation*, not a build.

### 3.4 Cost-model over-reach. The lawful residue (§1) must not metastasize into a bespoke solver in
the scanner crate (Constitution vi; `no-god-modules`, `general-infra-crates`). The cost model is
LOCK-2's; 9D supplies candidate kernels + cost inputs only. A scanner-local CSP would double-home the
optimizer.

---

## 4. Prior art (with citations)

- **nom `streaming` vs `complete`** — the canonical combinator streaming model: a `streaming::*` module
  returns `Err::Incomplete(Needed)` when input may continue; the consumer re-feeds. Documented cost:
  every combinator doubles, and `Incomplete` propagation is viral (`nom::Err::Incomplete`).
- **winnow `Partial<&[u8]>` + `StreamIsPartial`** — nom's successor isolates streaming into a *stream
  wrapper type* (`Partial<I>`) and a trait, so the combinator body is written once; `ErrMode::Incomplete`
  signals refill. Present only in parse-that's competitor bench (`benches/competitors/winnow.rs:35`).
  Lesson: streaming is a *stream-type* concern, not a combinator-rewrite — but it still forfeits
  zero-copy across refills.
- **simdjson** — `SIMDJSON_PADDING` (64 B) is the exact analog of `INPUT_PAD_BYTES=64` (`state.rs:57`);
  simdjson requires the *whole padded document* in memory and its `document_stream` handles only
  *concatenated* (ndjson) documents, each parsed whole. Direct authority for §0.1. On-Demand is lazy
  *tree materialization* over a whole buffer, not input streaming.
- **serde_json `from_reader` / `StreamDeserializer`** — serde's own guidance: `from_reader` buffers and
  is typically *slower* than `from_slice`; `StreamDeserializer` streams *top-level sequences* (ndjson),
  not sub-document windows. Confirms the whole-document ceiling.
- **tree-sitter** — incremental *reparse over edits* (retains prior tree, re-lexes changed ranges) — a
  different axis (edit-incrementality) from byte-streaming; not applicable to a one-shot parse.
- **re2c `--storable-state` / Ragel push model** — machine-generated refill/ring-buffer state-save cost
  for genuine push lexers; the boilerplate a streaming scanner inherits.
- **regex-automata `meta`** — the cost-model-selects-kernel pattern done right: the meta engine picks
  DFA / lazy-DFA / one-pass / backtracker / PikeVM by static properties at build time. The model for
  §1's pluggable scanner-kernel selection (and the `pluggable-components` law).
- **Hyperscan / Teddy** — literal-set prefilter tier selection by set size; the analog of the
  `One/Two/Three/Lut` ladder (`leaf.rs:182`) generalized.

---

## 5. Exactly what a prototype must test to move this family

1. **Rewind histogram (the F4 evidence).** Instrument `state.rs` restore sites to log
   `furthest_offset − offset` at every `state.offset = cp`/`checkpoint`. Run the divan combinator +
   CSS benches (R3 §6) over all six JSON corpora + bootstrap/normalize/tailwind CSS, diagnostics on and
   off. Emit P50/P99/max per corpus, base-pinned. **Predicted:** JSON P50=P99=0, max ≈ token; CSS
   max ≈ one rule. Confirms/falsifies "bounded ring buffer suffices." (Cheap: a counter + `Vec<u32>`.)
2. **Zero-copy-forfeit measurement (the refutation, §3.1).** Fork the JSON string leaf to `Cow::Owned`
   (simulating a window that cannot borrow) and re-run the paired cold bench + `alloc_probe`
   (PT-2.md W1). Quantify throughput + alloc regression. **Predicted:** measurable slowdown +
   per-string alloc explosion — the artifact that closes "streaming as design center" as a *refutation*
   (PT-2 W4-style dual-outcome close, P39).
3. **Pluggable kernel-selection prototype (the lawful build).** Extract `take_until_any_span`'s
   `One/Two/Three/Lut` decision (`leaf.rs:177`) behind a `ScanKernelCost` trait fed by
   {excluded-set cardinality, alphabet separability from `bbnf-simd::select_classifier`, expected run
   length}. Gate: byte-identical scan results (parity oracle vs the current ladder), and `otool -tv`
   insn-count ≤ the hand ladder on the JSON/CSS hot scanners, base-pinned, paired ≥0.98× (the PT-2 W1
   gate shape). The only 9D artifact that *builds* rather than *refutes*.
4. **Packrat-off proof (§3.2).** Confirm the streamed grammar carries no live packrat memo table across
   the window boundary, or record the pin as a hard blocker.

---

## 6. Family disposition (the strongest honest form)

9D's charter as literally stated — *streaming input as the design center* — is **refuted for
parse-that**: no consumer (§0.1), no peak-RSS lever (peak is the tree), and a decisive collision with
zero-copy (§3.1). The bounded-rewind ring-buffer question (§0.2) resolves *bounded but rewardless*.
The lawful, consumer-backed residue is the second charter clause — **compile-time (combinator-
construction-time) kernel selection by a pluggable cost model** (§1), already running in embryo at
`leaf.rs:177` + `bbnf-simd/dispatch.rs:42`, generalized into LOCK-2's cost model with 9D supplying the
scanner-site candidate set. The family advances as: (a) a *measured refutation* of streaming
(prototype §5.2, an artifact per P39), plus (b) a *landed* pluggable scan-kernel-selection plane
(prototype §5.3) feeding the gestalt's one cost model. "Lazy" in parse-that already means graph-lazy
(`lazy()`), not input-lazy.
