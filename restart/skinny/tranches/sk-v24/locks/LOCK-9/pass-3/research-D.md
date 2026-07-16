# LOCK-9 · Family 9D — LAZY-SURFACE · pass-3 research

*Charter (9D): streaming-first — scanners as incremental lazy planes over input windows;
kernels selected by a cost model at combinator-compile time; streaming parse support becomes
the design center. Paths absolute-relative to `/Users/mkbabb/Programming/parse-that/` (READ-ONLY,
HEAD `ef10d5b`) and `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/`.*

This family has two separable halves. The **streaming half** ("streaming parse becomes the
design center") is refuted at the substrate by measured architecture and by parse-that's own
declared policy; the honest strongest form is to name that refutation with its counterexamples
and to salvage what "lazy plane over windows" can mean once streaming is excised. The
**cost-model half** ("kernels selected by a cost model at combinator-compile time") is the
family's durable, consumer-backed contribution and survives intact — it is where 9D's design
center actually lands. Both are developed below to their strongest form.

---

## 1. The streaming surface census (F4-Q1) — there is none, and it is a stated non-goal

A grep census of the streaming vocabulary over `rust/parse_that/src` and `rust/regex/src`
(`feed | push_bytes | Decoder | BufRead | from_reader | document_stream | incremental | chunk`)
returns **zero streaming consumers**. Every `chunk` hit is a fixed-width SIMD stripe load
(`u8x16::from_slice`, `scanners.rs:158/199/278`, `ws_comment.rs:117`, `quote_parity.rs:78`) or
the 64-byte cache-line backing tile `PaddedChunk([u8;64])` (`state.rs:66`) — none is an
input-streaming window. There is no `feed`, no reader adaptor, no push interface, no chunked
decoder anywhere in the tree.

This is not an accident of the current cut — it is **declared policy**.
`docs/future-research.md:201` lists, verbatim, the *deliberate non-goals of the 1.0.0 cut*:
"**token streams, incremental parsing**, Squirrel …". Streaming is a named, intentional
exclusion, not an unbuilt feature awaiting a home. A design that makes streaming "the design
center" therefore directly contradicts the extant-tranche baseline and would have to be a
*named supersession* of an explicit product decision — not a gap-fill.

**Every grammar builds a whole owned tree over a whole owned input.** `ParserState::new(src:
&'a str)` (`state.rs:285`) calls `allocate_padded_buf(src.as_bytes())` (`state.rs:82`), which
performs a **single `copy_nonoverlapping` of the entire input** into an owned, 64-byte-aligned
`Vec<PaddedChunk>` at construction (`state.rs:99-103`). The parse then walks a monotone integer
cursor `offset` over that resident buffer; the JSON/CSS parsers accrete `Vec`-grown collections
(`many` → `values.push`, `sep_by` → `values.push`) and a decode arena. There is no
whole-document-granularity streaming surface either (no `document_stream`, no NDJSON reader).

**Per the constitution (F4 parenthetical, binding):** whole-document-granularity streaming does
not reduce peak tree RSS — so even the one streaming shape that *could* be bolted on (a reader
that yields one owned JSON/CSS tree per document) buys nothing on the metric streaming exists to
improve. Peak RSS is set by the single largest resident tree, which document-granularity
streaming does not shrink. **We say so plainly: the only streaming surface available to
parse-that's shipped grammars is document-granularity, and it does not reduce peak tree RSS.**

---

## 2. Why byte-level streaming is refuted at the substrate — three counterexamples

The streaming half fails not for want of effort but because three load-bearing invariants of
the scan/kernel plane are *predicated on the whole input being resident*. Each is a concrete
counterexample a streaming redesign must defeat and cannot.

**C1 — The PaddedView over-read invariant.** `INPUT_PAD_BYTES = 64` (`state.rs:57`); every SIMD
kernel is authored to load a full fixed-width stripe at any cursor `i` where `i + STRIDE ≤ len +
64` *without a per-chunk bounds guard* (`state.rs:113-128`, `PaddedView::bytes`), relying on the
trailing 64 NUL bytes to make the tail stripe well-defined and to classify as non-member so
scans terminate naturally (`ws_comment.rs:113-140`, "NUL classifies as non-WS so stripes that
straddle the public end terminate"). A streaming reader that feeds bounded windows **has no
valid tail padding at a chunk boundary that is not the true end of input** — the kernel would
either over-read into the next chunk's not-yet-arrived bytes (unsound) or reintroduce the
per-stripe bounds guard the whole padded-view design exists to delete (a measured regression to
the pre-padding scan cost). Streaming and the branch-free padded stripe walk are mutually
exclusive.

**C2 — The zero-copy borrow model.** JSON string decode returns `StringPayload::Borrowed {
start, end }` (`parsers/scan/decode.rs:6-8`) — an escape-free string is a *borrow into the
resident input*, never copied ("an escape-free input takes the Borrowed path without ever
touching the arena", `decode.rs:19-21`); the produced `JsonValue<'a>` borrows `&'a str` spans
directly. A streaming window that is *evicted* after parsing invalidates every outstanding
borrow, forcing every string to the `Owned` arena copy path — this converts the hot,
allocation-free borrow into a per-string copy, precisely the mass (`5390` per-parse allocs on
twitter is already the Vec/arena tier, and streaming would *add* to it). Streaming destroys the
zero-copy tape.

**C3 — Backtrack reaches arbitrarily far into resident bytes.** Restore is a plain
cursor rewind into the resident buffer (`state.offset = checkpoint`, §3). A streaming ring buffer
would have to retain every byte back to the furthest live checkpoint; because checkpoints nest
through the combinator tree (each `many`/`sep_by`/alternation frame saves one), the retention
window is the whole in-flight subtree, not a bounded suffix. Combined with C1/C2, the "window"
is forced to the whole document — streaming degenerates to the current whole-input model with
extra bookkeeping.

**Verdict for the streaming half:** SPEC-ONLY-negative. It is not buildable without abrogating
the padded-view scan invariant, the borrow tape, and the nested-checkpoint model — the three
features that make the plane fast. A prototype is not needed to *disprove* streaming (the
counterexamples are structural); a prototype IS specified in §5 to *quantify* the one empirical
question that gates any future reconsideration: is backtrack bounded (§3)?

---

## 3. The rewind/backtrack distribution (F4-Q2) — bounded by construction, quantify by probe

Restore sites are uniform and simple: a checkpoint is `let checkpoint = state.offset` and a
restore is `state.offset = checkpoint`. Enumerated sites:
`leaf.rs:301-306` (`one_of`, flat N-way alternation), `combinators/macros.rs:91-297` (the
`seq!`/`alt!` collapse — ~40 `furthest_offset.max(offset)` guards), `span_parser/assertions.rs:16-90`
(lookahead/negative-lookahead save-and-restore of BOTH `offset` and `furthest_offset`),
`span_parser/mod.rs:159-169` (`FLAG_SAVE_STATE` checkpoint), `combinators/methods/recover.rs:37`,
`span_parser/sep_by.rs:29-41` and `span_trait.rs:138-150` (checkpoint before separator, backtrack
past it on element-after-separator failure).

**`furthest_offset` is monotone-forward and diagnostics-only.** It is written *only* via
`self.furthest_offset = self.furthest_offset.max(self.offset)` (never decremented on restore —
lookahead assertions are the sole sites that *save/restore* it, `assertions.rs:17-24`, to prevent
speculative lookahead from polluting the error frontier). It feeds `add_expected`
(`state.rs:386`, three-way `cmp` against furthest) and `snapshot_diagnostic` (`state.rs:436`).
It is **never an input-rewind source** — the rewind target is always the saved `checkpoint`,
i.e. `offset` at frame entry.

**The distribution, reasoned statically (both grammars are near-LL(1)):**
- **JSON is LL(1).** Value dispatch is a first-byte lookup table `dispatch_byte_multi`
  (`leaf.rs:46`, `json.rs:94`) — O(1) branch by peeking one byte, no speculative descent, so a
  wrong-branch rewind is **≤ 1 byte**. The `many`/`sep_by` array/object loops checkpoint
  immediately before the separator and, on element-after-separator failure, rewind *past the
  separator* — a distance of one separator plus surrounding whitespace, i.e. **O(1) bytes**
  (P99 ≈ a few bytes). JSON has no unbounded speculative alternation.
- **CSS backtrack is dominated by forward resync, not backward rewind.** `recover`
  (`recover.rs:23-45`) on element failure snapshots a diagnostic, restores to `checkpoint`, then
  runs `sync` **forward** to skip bad content and returns a sentinel — the net cursor motion is
  *forward*, the rewind is only back to the frame's own entry (bounded). Alternation frames
  (`macros.rs`) rewind to frame entry; the deepest CSS speculation (selector-vs-declaration) is a
  handful of tokens.

**Conclusion:** backtrack is **bounded** for parse-that's shipped grammars — a bounded ring
buffer *would* suffice were streaming otherwise viable. This is the one datum that keeps the door
ajar; it is refuted from becoming useful by C1/C2 (§2), not by unbounded rewind. The exact
P50/P99/max histogram is a `diagnostics`-instrumented probe (§5), because static reasoning gives
the *shape* (bounded, O(1)) but not certified percentiles over the real corpora.

---

## 4. The cost-model half — 9D's surviving design center (kernel selection at compile time)

Strip streaming away and the charter's second clause is the live one: **kernels selected by a
cost model at combinator-compile time.** The substrate for this already exists in two places and
is currently exercised as *runtime data-tiering*; 9D's contribution is to lift the selection from
run time to **combinator-compile time**, driven by grammar-derived facts and a fitted cost model.

**Seed A — bbnf-simd's alphabet-driven classifier selection (already a compile-time-choosable
kernel).** `bbnf-simd/src/lib.rs:8` `select_classifier(alphabet: &'static [u8;64]) ->
SelectedClassifier`; the selection resolves to `SelectedBackend::{Scalar, NeonTbl4}`
(`dispatch.rs:6-28`) — a scalar `byte_class_from_table_64` path vs a NEON `classify_tbl4`
`vqtbl1q` path chosen from the **known alphabet**. Because a grammar's scan-leaf alphabet is
known when the parser graph is built, this `&'static [u8;64]` is a *compile-time constant* — the
runtime `select_classifier` is a residualizable choice. bbnf-simd additionally exposes
`ScanBackend`/`active_backend()` (runtime NEON-vs-scalar) and a `PrimitiveKernels` fn-pointer
table (`lib.rs:50-58`) — the vocabulary a cost model would populate.

**Seed B — the regex tier-planning cost inputs.** `bbnf-regex`'s `RegexInfo` (`info/mod.rs:135`,
"Cost estimation … feeds into tier planning CSP") already computes exactly the facts a
kernel-selection cost model needs, per scan leaf: `first_chars: CharSet128`, `literal_prefix`,
`negated_class`, `quantified_class`, `min_match_len`/`max_match_len` (expected run length),
`must_consume`, `one_pass_eligible`, and `dfa_size_estimate` (`info/mod.rs:150-175`). These are
the cost model's *feature vector*.

**Seed C — the runtime tiering the design replaces.** `ws_comment.rs`'s
`scan_ws_block_comments` (`ws_comment.rs:98-150`) branches at run time across **Tier 1** (scalar
first-byte reject), **Tier 2** (one 16-byte `u8x16` classify), **Tier 3** (`#[cold]` 64-byte
stripe walker `scan_ws_bitmap_cold`). This is a *data-dependent* branch decided per call. When
the run-length distribution of a leaf is known at grammar-compile time (short idents vs long
whitespace runs vs block comments), the tier is a *static* property of the leaf, and the runtime
branch is dead weight.

**The 9D mechanism, made concrete.** At combinator-compile time (the point where the parser
graph is materialized — the natural home is the sibling closure-to-graph node model, LOCK-4A),
each **scan leaf** carries its grammar-derived `RegexInfo` feature vector. A **kernel cost model**
— a pluggable table of per-kernel cost coefficients (scalar SWAR, 16-byte classify, 64-byte
stripe, nibble-LUT `tbl4`, `memchr` literal-prefilter), fitted from paired PMU captures (the
closed-loop discipline) — scores each legal kernel for that leaf's `(alphabet cardinality,
separability, expected width, prefilter selectivity)` and **selects one, baking a specialized
scanner into the emitted parser**. This:
- replaces `ws_comment`'s runtime Tier1/2/3 branch with a compile-time-selected specialization
  (the branch is resolved, not taken);
- makes `select_classifier`'s alphabet argument a const-folded choice rather than a runtime call;
- is the **direct consumer** of LOCK-5's `RegexInfo`/tier-planning CSP and of LOCK-2's
  KernelCostTable/CSP driver — i.e. it satisfies the no-consumerless-substrate law by *being* the
  seam where those two upstream products cash out on the scan plane.

Cost inputs, precisely: **alphabet separability** (single-`vqtbl1q` when the class is 4-bit-lo
separable, else multi-table or scalar — the `classify_tbl4::build_lo6_table` basis,
`dispatch.rs:23-28`); **expected run length** (`min/max_match_len` — short runs favor scalar/16B
and *disfavor* a multistate `vqtbl1q` chain, which is a measured negative on short runs);
**prefilter selectivity** (`literal_prefix` + `first_chars` rarity → `memchr`/rare-byte ladder,
the prefilter tier); **DFA size** (`dfa_size_estimate` gates whether a compiled-DFA scan is even a
candidate under the 255-class/4096-state ceilings). The objective is cyc/byte from fitted
coefficients; legality constraints are the kernel's applicability predicate; the selector is the
LOCK-2 CSP over extracted candidates. This is `pluggable-components` by construction — the cost
table and kernel registry are data, not hardcoded branches.

**The "lazy plane over windows", honestly reframed.** The simdjson two-stage architecture is
*already* the lawful reading of "lazy plane": stage-1 produces a structural-position index
(`bbnf-simd`'s `StructuralIndex`/`scan_dispatch`, `lib.rs:71-123`; parse-that's
`structural_bitmap.rs`), stage-2 walks positions on demand. The "window" is the 64-byte SIMD
stripe; the "lazy plane" is the on-demand position index. Both operate over a **fully-resident**
buffer — which is exactly right and needs no streaming. 9D's contribution here is to select the
stage-1 kernel by the cost model above, not to feed the stripes incrementally.

---

## 5. What a prototype must test to move this family

Two probes, both cheap, both born-falsifiable; the scratch-copy recipe (rsync + external
patch-repoint + isolated `CARGO_TARGET_DIR`) is the substrate.

**P1 — the rewind histogram (settles F4-Q2, gates any streaming reconsideration).** Add a
`diagnostics`-gated counter at every `state.offset = checkpoint` restore site
(`leaf.rs:306`, `macros.rs`, `span_parser/assertions.rs`, `recover.rs:37`) recording the rewind
distance `checkpoint − offset_at_fail` (the bytes un-consumed by the restore), and separately
`furthest_offset − offset` at each restore. Run over all six JSON corpora
(`apache-builds/canada/citm_catalog/data/data_xl/twitter`) + the CSS corpora (bootstrap,
normalize, tailwind-output), emit P50/P99/max, base-pinned, diagnostics-on-vs-off to bound the
probe's own cost. **Expected:** P99 ≤ tens of bytes (bounded); a P99 in the kilobytes on any
corpus would *falsify* the "bounded backtrack" claim and is the named counterexample to hunt
(unbounded speculative alternation would surface here).

**P2 — the kernel cost-model selection oracle (moves the surviving design center).** For a small
set of scan leaves with *known* alphabets and width distributions (JSON string body, JSON
number, CSS ident, CSS whitespace/comment), (a) enumerate the legal kernels, (b) measure each
kernel's cyc/byte cold-paired (divan, N≥200, two windows, opposite arm order — ratios certified,
absolutes directional) on that leaf's real run-length distribution, (c) confirm the cost model's
*argmax matches the measured winner* per leaf. **Falsifier:** if the model selects a kernel that
the paired measurement shows is not the fastest for that leaf (e.g. a multistate `vqtbl1q` chain
chosen for a short-run leaf where 16-byte classify wins — the measured-negative short-run case),
the cost coefficients are miscalibrated and the selector is re-fit before it can be trusted to
bake static choices. Instruction-count guard: the selected kernel's `otool -tv` reduction-insn
count must not regress against the hand-tuned baseline for that site.

---

## 6. Integration seams (path:line where extant code serves)

- **Whole-input materialization** (the streaming refutation's root): `state.rs:82`
  `allocate_padded_buf` (single `copy_nonoverlapping` of entire input), `state.rs:285`
  `ParserState::new`, `state.rs:57` `INPUT_PAD_BYTES`, `state.rs:113-128` `PaddedView` over-read
  contract.
- **Rewind/checkpoint sites** (F4-Q2 probe targets): `leaf.rs:301-306` `one_of`;
  `combinators/macros.rs:91-297`; `span_parser/assertions.rs:16-90`; `span_parser/mod.rs:159-169`
  `FLAG_SAVE_STATE`; `combinators/methods/recover.rs:37`; `span_parser/sep_by.rs:29-41`.
- **Zero-copy borrow tape** (streaming counterexample C2): `parsers/scan/decode.rs:6-8`
  `StringPayload::Borrowed`, `decode.rs:19-23` borrow-vs-arena split.
- **Runtime tiering to be lifted to compile time**: `parsers/scan/ws_comment.rs:98-150`
  (Tier1/2/3), `ws_comment.rs:190+` `scan_ws_bitmap_cold` (`#[cold]` stripe walker).
- **Cost-model seeds**: `bbnf-simd/src/lib.rs:8` `select_classifier`, `dispatch.rs:6-28`
  `SelectedClassifier`/`{Scalar,NeonTbl4}`, `lib.rs:50-58` `PrimitiveKernels`,
  `lib.rs:71-123` `StructuralIndex`/`scan_dispatch` (the lazy two-stage plane);
  `bbnf-regex/src/info/mod.rs:135-175` `RegexInfo` feature vector + tier-planning CSP;
  `bbnf-regex/src/info/dfa_size.rs` `dfa_size_estimate`.
- **First-byte dispatch** (bounded-lookahead evidence): `leaf.rs:46` `dispatch_byte_multi`,
  `json.rs:94`.
- **Declared non-goal** (authoritative policy): `docs/future-research.md:201`.

---

## 7. Prior art

- **simdjson (Langdale & Lemire, 2019)** — the two-stage structural-index-then-tape model is the
  lawful "lazy plane over a resident buffer"; on-demand parsing (`ondemand`) is lazy *materialization*
  of an already-fully-scanned buffer, not streaming input. It confirms the design center: laziness
  is in the *consumer walk*, not the *input feed*. It has no chunked-input streaming mode for the same
  padded-load reason (C1) — it pads and requires the whole document resident.
- **serde_json `StreamDeserializer` / `Deserializer::from_reader`** — the *only* mainstream
  streaming JSON shape is document-granularity (NDJSON-style), which per the constitution does not
  reduce peak per-document RSS; it is a throughput convenience, not a memory lever. This is the
  ceiling of what parse-that could offer and it buys nothing on the metric.
- **regex-automata prefilter ladder (memchr/rare-byte, Teddy)** — the model for the cost-model
  half's prefilter tier: kernel selected from literal/first-byte facts at *compile* time
  (`Prefilter` is chosen when the regex is built), exactly the residualized selection 9D lifts.
- **tree-sitter incremental parsing** — the canonical incremental/streaming parser; it pays for it
  with a persistent editable tree and node-reuse bookkeeping — a different product (editor
  re-parse), and precisely the cost model parse-that's 1.0.0 non-goal declines.
- **rustc/LLVM instruction selection & tablegen** — the pattern for compile-time kernel selection
  by a cost model over pattern facts: the selection is a solved problem over a fitted cost table, not
  a runtime branch, which is the shape 9D's kernel selector adopts.

---

## 8. Named risks / where 9D can still lose

- **R1 — the streaming half is resurrected as a "feature".** Any attempt to make streaming the
  design center contradicts `future-research.md:201` and abrogates C1/C2/C3; it must be rejected as
  a named non-goal, not entertained. Counterexample: a chunk-boundary stripe read has no valid tail
  padding.
- **R2 — the cost model bakes a wrong static choice.** A miscalibrated coefficient picks a
  multistate `vqtbl1q` for a short-run leaf (measured-negative). Guard: P2's per-leaf paired
  argmax-vs-measured oracle + insn-count no-regression before any choice is baked.
- **R3 — consumerless substrate.** A kernel registry / cost table with no wiring into the parser
  graph would violate Y.13. Guard: the selector's *only* reason to exist is to consume LOCK-5's
  `RegexInfo` and LOCK-2's KernelCostTable and emit a specialized scan leaf — it must land in the
  same wave as a grammar leaf whose kernel it selects, or not at all.
- **R4 — over-claiming laziness.** Framing the existing simdjson two-stage index as "new streaming"
  is disingenuous; it is a resident-buffer lazy walk. Honest framing only.
