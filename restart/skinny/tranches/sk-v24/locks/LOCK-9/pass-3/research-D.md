# LOCK-9 · Family 9D — LAZY-SURFACE (streaming-first scanners; kernels selected by a
# cost model at combinator-compile time)

Charter: streaming-first — scanners as incremental lazy planes over input windows; kernels
selected by a cost model at combinator-compile time; streaming parse support the design
center. Steering: F4 (windowed vs streaming input surface) primarily; the cost-model
kernel-selection half touches F1/F3. All paths absolute-relative to
`/Users/mkbabb/Programming/parse-that/` unless noted. HEAD `ef10d5b`.

---

## 1. The substrate as it stands — a whole-document, twice-resident input

The mechanism 9D would rebuild does not exist in any partial form; the extant surface is its
antithesis, and that must be stated as the design's starting fact.

**Entry is whole-string, eager.** The only parse entry is `parse_that/src/parse.rs:146`
`parse_return_state(&self, src: &'a str)` → `parse_that/src/parse.rs:285`
`ParserState::new(src: &'a str)`, which fixes `end = src.len()` (`:286`) once and never
grows it. There is no `end`-mutation path, no reader, no feed. `is_at_end` (`state.rs:360`)
is `offset >= end` against that frozen length.

**The input is materialized TWICE.** `ParserState::new` calls
`allocate_padded_buf(src.as_bytes())` (`state.rs:287`), producing an OWNED, 64-byte-aligned
copy of the ENTIRE input plus `INPUT_PAD_BYTES = 64` trailing NUL (`state.rs:57,82-106`),
held as `padded_buf: Vec<PaddedChunk>` (`state.rs:226`). So during a parse the document is
resident as the borrowed `src`/`src_bytes` (`&'a str`, `state.rs:204-206`) AND the owned
padded copy — one `memcpy(N)` at construction (`state.rs:100`). Every SIMD scanner reads the
owned copy through `PaddedView` (`state.rs:122-170`, obtained via `state.padded()` `:353`).

**The 64-byte tail-NUL is a whole-buffer invariant, not a window property.** `PaddedView`'s
contract (`state.rs:130-147`, `debug_assert bytes[len..].all(== 0)`) is what lets every
kernel drop its per-stripe `i + STRIDE <= len` guard — the load-bearing fast-path premise of
all eight scanners (R3 §4; `state.rs:313-344`). This invariant holds *because the whole
document is contiguous with a single trailing pad*. A windowed/ring-buffer input breaks it at
every chunk seam.

**Census confirms zero streaming surface.** `rg 'feed|push_bytes|Decoder|BufRead|
document_stream|from_reader|from_read'` over `parse_that/src` → **0 live hits** (the `chunk`
tokens that surface are SIMD STRIPE loads, e.g. `span_parser/scan.rs:139`
`u8x16::from_slice(&bytes[i..i+16])`, not document chunks). parse-that is **complete-only**:
every grammar builds one whole owned tree over a fully-resident, pre-padded input.

**F4's own caveat is dispositive for the naive reading.** Whole-document-granularity
streaming (accept a `BufRead`, buffer it all, then parse) reduces nothing: the tree is still
built whole; peak tree RSS is unchanged; and the owned `padded_buf` copy is *additional* RSS,
not saved. If a chunked-feed API over a whole-document build is the only surface 9D can reach,
it is a pure loss and must be reported as such.

---

## 2. The `lazy` name is already taken — and means the opposite

`parse_that/src/lazy.rs` is a construction-deferral primitive, **not** input laziness.
`LazyParser` (`lazy.rs:20`) memoizes the *built* `Parser` in `cached_parser` (`lazy.rs:22,42`)
to break recursive-grammar construction cycles; `lazy()` (`lazy.rs:49`) wraps a thunk that
builds a sub-parser on first call and caches it. Its consumers are exactly the self-referential
JSON/CSS rules: `parsers/json.rs:50` (`json_array`), `:81` (`json_object`),
`parsers/css/mod.rs:28,225`. This is deferred parser *building*, invoked over a fully-resident
input; it moves no byte lazily. 9D must not reuse this word or this type — a "LazyScanner" that
sounds sibling to `LazyParser` would collide semantically. Any streaming plane needs a new,
distinct home (candidate: `parse_that/src/stream/`).

---

## 3. Backtrack IS bounded and PEG-predictive — the ring-buffer feasibility axis

The question that decides whether streaming is even *architecturally* possible is F4's second:
is backtrack bounded (a ring buffer of fixed size can serve rewinds) or unbounded (streaming
cannot bound its retained window)?

**Mechanism.** Backtrack is checkpoint-based, not log-based. `Parser::save_state`
(`parse.rs:196`, sets `FLAG_SAVE_STATE`) records `checkpoint = state.offset` before the child
(`parse.rs:105`); on child failure it resets `state.offset = cp` and folds the furthest reach
into diagnostics: `state.furthest_offset = state.furthest_offset.max(state.offset)` then
`state.offset = cp` (`parse.rs:114-119`). `furthest_offset` (`state.rs:211`) is a
diagnostics-only high-water mark (consumed by `snapshot_diagnostic` `state.rs:436-449` and the
error surface `parse.rs:183-189`), never a re-read trigger. Rewind is O(1) `usize`
assignment *because the whole document is in memory* — no byte is re-fetched.

**Restore-site inventory (the ring-buffer sizing surface).**
- Ordered-choice arms: `combinators/macros.rs:91-159` — ~11 `furthest_offset.max(offset)`
  folds, one per `alt!` branch boundary.
- Zero-width assertions (the classic unbounded-rewind risk): `span_parser/assertions.rs`
  Not (`:17-24`), Minus (`:36-43`), LookAhead (`:69-76`), Peek (`:87-90`) — each saves BOTH
  `offset` and `furthest_offset` and resets both; all are structurally zero-width (they probe
  then rewind fully).
- `sep_by` element-after-separator backtrack: `span_parser/sep_by.rs:41`,
  `combinators/methods/sep_by.rs`.
- Core save/restore: `parse.rs:105,118`; span mirror `span_parser/combinators.rs:31`,
  `span_parser/mod.rs:169`.

**Structural claim (grounded, but the histogram is OWED).** JSON value dispatch is
first-byte-deterministic — `alt!(object,array,string,number,bool,null)` discriminates on the
opening byte, so a failed branch rewinds ≈ 0–1 byte (effectively LL(1)/predictive, not
speculative). Array/object elements are `sep_by(value, sep)`; a failed separator rewinds past
whitespace + one delimiter — bounded by a token, not the document. CSS adds recovery
combinators (`parsers/css/` + the `diagnostics`-gated `css_recovery`) whose synchronization
scan is the widest rewind candidate. **No restore site's rewind is proportional to document
size**; the bound is per-rule/per-token. Therefore a bounded ring buffer *would* suffice on
the backtrack axis — CONDITIONAL on the measured P99/max. The prototype obligation (§7.1) is
to instrument these exact sites and emit the `furthest_offset − offset` and
`offset − checkpoint` histograms (P50/P99/max, diagnostics on/off) over the JSON+CSS corpora;
until then "bounded" is a structural argument, not a certified number.

---

## 4. The cost-model kernel-selection half — this one is REAL and has a working seed

9D's second clause ("kernels selected by a cost model at combinator-compile time") is not
speculative; it is already partly built and SPEC-ratified, and it is where 9D's strongest,
defensible contribution lives — decoupled from the streaming risk.

**SPEC ratifies the cost model as the differentiator.** SK-V24 SPEC §1 (the union): "no other
parser generator emits grammar-specialized SIMD kernels behind a cost model." The generator's
decision driver is LOCK-2's CSP/e-graph engine; 9D is a *client* of that driver, contributing
the kernel candidate set and the per-site selection criterion.

**PROVE-10 is the working template of compile-time kernel selection.** `Prefilter::from_dfa`
(prototype `automata/prefilter.rs`, +97 LOC) reads the compiled DFA start-state transition row
and derives the entry-byte set → selects `Byte1/Byte2/Byte3` (memchr/memchr2/memchr3 tiers) or
`None` (parity fall-through), a one-time O(256) derivation folded into `Dfa::find`. Measured
9.6×–29× on prefilterable patterns, **0.99× on non-prefilterable controls** (zero regression),
compile-neutral, 44/44 tests (PROVE-10 REPORT §2, GO). This is *exactly* "a kernel chosen at
compile time by a grammar-/DFA-derived property," and it is the seed a 9D cost model
generalizes: the candidate tiers become {memchr prefilter, membership shufti (SPEC §0/01,
CONFIRMED 7.5×/4.7× skip-heavy, dense-neutral 1.03–1.15×), structural bitmap, fused string
scan (SPEC §0/03 `vshrn`, 0.898–0.931× cyc/B), scalar}, and the selector is a cost lattice fed
by the grammar's derived scan character (self-loop membership, rare-entry prefilter, dense
structural).

**The refutation laws bound the candidate set — the cost model may NOT select these.**
- 04 (block scheduling dead, 1.05–1.06× only): the M-class OoO core already pipelines the
  single-block loop; the model selects *tier*, never *pipeline depth*.
- 02 (digit-scan SIMD NO_GO): latency-bound CPI inversion 0.13→0.30; no digit-vector kernel
  enters the candidate set — the redress target is float conversion, not the scan.
- 01 dense-neutrality: shufti wins are skip-heavy only; on dense input it is ~parity, so the
  cost model must gate prefilter/shufti on a *measured* skip-density estimate, not always-on.

The kernel-selection half therefore stands on its own as a buildable, refutation-respecting
9D contribution even if the streaming half is refuted.

---

## 5. Prior art — where streaming combinator/DFA parsers actually draw their lines

- **nom `streaming` vs `complete` modules** (and winnow's `Partial<&[u8]>` +
  `ErrMode::Incomplete`): the canonical Rust-combinator streaming contract returns
  `Incomplete(Needed)` when the window is short, letting the caller feed more. parse-that has
  NEITHER module — it is complete-only. This is the closest idiomatic template for a 9D
  streaming lane, and it shows the cost: every leaf combinator must learn a "need more input"
  return, a pervasive type change to `ParserResult` (`parse.rs:39`).
- **serde_json `Deserializer::from_reader` + `StreamDeserializer`**: whole-*value* streaming —
  one JSON value at a time from a `BufRead`, tree dropped between values. This is the ONLY
  streaming variant with a real peak-RSS win, and the direct analog of a parse-that
  `document_stream`. It is value-granular, not byte-granular.
- **simdjson `document_stream` / On-Demand**: batched ndjson; requires `SIMDJSON_PADDING`
  (the exact analog of parse-that's 64B pad) — and simdjson re-pads per batch, confirming the
  chunk-seam re-padding cost (§6.3) is real and paid by the SOTA engine, not avoidable.
- **tree-sitter incremental parse**: edit-incremental (persistent tree, reused subtrees on
  reparse) — a *different* axis (editor latency, not RSS); not what 9D's "streaming input"
  means, but the reference for "incremental" if the charter is re-read as edit-incremental.
- **Ragel / re2c streaming**: DFA scanners with an explicit `p`/`pe`/`eof` window and a resume
  state — the DFA-scanner-over-a-window model that 9C (SCANNER-AS-DFA) would need and that a
  windowed 9D would consume. The resumable-DFA state is small (one state id) — the encouraging
  half of streaming-DFA feasibility.
- **PEG "cut" (ordered-choice commit) and packrat bounded lookahead**: the theory that a PEG's
  backtrack is bounded once cuts are placed; corroborates §3's structural bound.

---

## 6. Named risks & counterexamples (why 9D's streaming half is fragile)

**6.1 Consumer-less-substrate law (Constitution viii / Y.13) — 9D's existential risk.** There
is NO in-wave streaming consumer anywhere. The parse-that consumers are whole-string
(value.js, latex-paper, keyframes.js — SPEC §2b; all pass a full `&str`). Building a streaming
surface with no consumer is forbidden by design. 9D must NAME a real streaming consumer
(ndjson ingest? a large-log CSV lane? a wasm-side reader?) or be reported as a substrate
without a consumer — a refutation, per the constitution.

**6.2 The owned `padded_buf` is fundamentally anti-streaming.** `allocate_padded_buf`
(`state.rs:82`) demands the whole input contiguous before parse. A streaming input has no such
contiguity; the copy would have to become per-window, re-padding at each seam — which either
re-introduces the per-stripe bounds guard (defeating the fast-path premise of all eight
scanners, R3 §4) or pays an overlap-copy at every window boundary.

**6.3 Chunk-seam re-padding defeats the PaddedView invariant.** The 64B tail-NUL
(`state.rs:130-147`) is why kernels are guard-free. At a ring-buffer window boundary the "tail"
is live data, not NUL, so either the window carries a 64B overlap (simdjson pays this) or the
boundary stripe reverts to a scalar/guarded tail (`parsers/scan/decode.rs:241`,
`parsers/scan/number_simd.rs:239` scalar-tail pattern) — a per-window tax that a whole-document
parse never pays. The prototype must measure whether the streaming RSS win survives this tax.

**6.4 Whole-document streaming ≠ RSS win (F4's caveat, restated as a risk).** Only
sub-document / value-granular streaming (§5 serde/simdjson model) reduces peak tree RSS. A
chunked-feed API that still builds the whole tree is a net loss (adds the reader buffer to the
already-double-resident input). 9D must commit to *value-granular* streaming or nothing.

**6.5 Cost-model over-reach.** A selector that ranks kernels by static instruction count would
pick the digit-SIMD and block-pipelined kernels the refutation laws (02, 04) kill; the model
must be fed *measured* cyc/B facts (LOCK-2's PMU fact tables), not static heuristics, or it
reproduces the refuted wins.

---

## 7. What a prototype MUST test to move family 9D

1. **The rewind histogram (the go/no-go for streaming feasibility).** Instrument the restore
   sites (`parse.rs:118`, `span_parser/assertions.rs:17-90`, `combinators/macros.rs:91-159`,
   `sep_by.rs`) to record `furthest_offset − offset` AND `offset − checkpoint` per rewind,
   over the six JSON corpora + the CSS corpora, diagnostics ON and OFF, base-pinned. Emit
   P50/P99/max. **Decision rule:** if max is a small constant (independent of document size),
   a bounded ring buffer suffices and streaming is architecturally possible; if any site's
   rewind scales with input, streaming is refuted on the backtrack axis. (Diagnostics ON is
   expected to widen the distribution via `furthest_offset` tracking; report both.)

2. **The ndjson / `document_stream` RSS probe (the only RSS-positive variant).** Build a
   value-granular lane: parse one JSON value per record from a `BufRead`, drop the tree after
   each; measure peak RSS (`/usr/bin/time -l` max RSS on this Darwin host) vs the whole-file
   `parse` on a multi-value corpus. This requires `padded_buf` to become per-record.
   **Decision rule:** RSS strictly and materially below whole-file, net of the per-record
   padding cost, or the streaming charter is a loss.

3. **The PaddedView-under-window survival test.** Prototype a ring-buffer window feeding ONE
   scanner (e.g. `parsers/scan/ws_comment.rs` or `parsers/scan/structural_bitmap.rs`); measure
   the chunk-seam re-padding cost (overlap-copy vs scalar-tail revert) and the resulting cyc/B
   vs the whole-buffer guard-free path. **Decision rule:** the guard-free fast-path premise
   (R3 §4) either survives windowing or the streaming path is measurably slower per byte —
   quantify the tax.

4. **Name-and-wire a streaming consumer (Y.13).** Identify one real consumer (ndjson ingest,
   large-log lane, or a wasm reader on the GN-4 seam) and wire it in the same wave, or file the
   consumer-less refutation. No dormant streaming substrate.

5. **Generalize PROVE-10's compile-time selection into a cost-model kernel picker.** Extend
   `Prefilter::from_dfa`'s derivation into a selector over {prefilter, membership shufti,
   structural bitmap, fused `vshrn` string scan, scalar}, driven by a grammar-derived scan
   character and LOCK-2's PMU fact table; gate shufti/prefilter on a measured skip-density
   estimate (respecting 01's dense-neutrality). **Decision rule:** on a real grammar scan site,
   the selected kernel matches or beats always-scan, and the selector never picks a
   refutation-killed kernel (02/04). This half is buildable independent of streaming and is
   9D's defensible core.

---

## 8. Disposition — 9D's honest strongest form

9D splits cleanly into two claims of very different strength.

**The kernel-selection half is REAL, buildable, and refutation-respecting** (§4): PROVE-10 is
its working seed, LOCK-2's driver is its engine, the candidate tiers are all SPEC §0-measured,
and the refutation laws bound its choices. This is the half to advance.

**The streaming/lazy-input half is a substrate at high risk of being consumer-less** (§6.1)
and blocked by the owned-padded-buffer contiguity requirement (§6.2) + the PaddedView tail-NUL
invariant (§6.3). Its only RSS-positive form is value-granular ndjson streaming (§5, §7.2), and
it needs a named consumer to exist at all. Its go/no-go is the rewind histogram (§7.1) — a
measurement that does not yet exist and that this family cannot honestly assert without. The
strongest honest 9D therefore advances the cost-model kernel picker as its core, and carries
the streaming lane as a *conditional* plane gated on (a) a bounded measured rewind, (b) a
named value-granular consumer, and (c) a windowed-PaddedView survival proof — any of which
failing files streaming as a measured refutation rather than shipping a consumer-less lazy
substrate.
