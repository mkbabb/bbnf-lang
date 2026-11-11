# LOCK-9 Family 9D — LAZY-SURFACE (streaming-first scanners over input windows)

Charter (PORTFOLIO §9D): scanners as incremental lazy planes over input windows; kernels
selected by a cost model at combinator-compile time; streaming parse support as the design
center. This report develops 9D to its strongest concrete form and reports the two facts that
govern its viability — the backtrack-bound and the consumer — as measured/structural evidence,
per the Y.13 consumer-or-defect law and the SPEC §0 no-throughput-claim constraint.

All parse-that paths are absolute-relative to `/Users/mkbabb/Programming/parse-that/rust/`
and READ-ONLY (P23 non-clobber); any mutation is a named cross-lock supersession, never presumed.

---

## 1. The mechanism made concrete

A lazy/windowed input surface means: the parser consumes input through a *window abstraction*
(a bounded byte range materialized on demand) rather than a single fully-resident `&'a str`.
Two independent capabilities are conflated in the charter and must be separated:

- **(M1) Windowed scanning over a resident buffer** — the scanner kernels operate on fixed
  cursors + 64-byte stripes, caching per-window bitmaps, so a scan reads only the stripe it
  needs. This *already exists* (§2) and is orthogonal to streaming.
- **(M2) Streaming input** — the parser accepts input incrementally (`feed(chunk)`), never
  holding the whole document, emitting structural output before the last byte arrives; peak-RSS
  is bounded by a window, not by input length. This *does not exist* in any form (§3) and is
  the actual "streaming-first" design center.

The two are joined by one hard question: **can the window be smaller than the maximum rewind
distance?** If backtracking can rewind to an arbitrary earlier byte, no already-consumed byte
may be discarded, and M2 is unsound without full retention — which defeats its own premise.
This is the pivot the whole family turns on (§4).

---

## 2. Substrate that already exists (M1 — windowed scan over resident input)

parse-that already scans through 64-byte windows with cursor state; a 9D "lazy plane over input
windows" would *generalize the primitives already present*, not invent them:

- **The 64-byte whitespace window.** `ParserState` carries `ws_bitmap: u64` +
  `ws_bitmap_start: usize` (`parse_that/src/state.rs:228-243`), a one-shot 64-byte whitespace
  bitmap computed once (`parse_that/src/scanners.rs:150-177`) and consumed across consecutive
  `?w` calls via `(ws_bitmap >> bit_offset).trailing_ones()` (`scanners.rs:123-126`). This is a
  live, consumed, incremental windowed-scan plane — the exact shape 9D would lift to a general
  window abstraction.
- **The structural stripe iterator.** `NibbleBitmapIter` (`parsers/scan/structural_bitmap.rs:158-262`)
  is a stateful 64-byte-stripe cursor: it classifies a stripe into a `u64` mask, CTZ-iterates
  hits, retains `stripe_base`/`stripe_mask`, and supports `set_offset` fast-forward
  (`:249-261`). The one-shot form `find_next_structural_from` (`:104-154`) is the stripe-window
  scan. Granularity is already the natural streaming quantum (64 B = four `u8x16` loads / one
  cache line, `state.rs:57` `INPUT_PAD_BYTES=64`).
- **Compile-time kernel selection already exists.** The charter's "kernels selected by a cost
  model at combinator-compile time" is partially extant: `classify_regex`
  (`regex/src/classify.rs:157`) → `RegexClass` (`:55`) drives `FnDescriptor` specialization on
  the bbnf side (R3 §Classification), selecting a scanner kernel per leaf at grammar-compile
  time. 9D would extend this selector with a window/streaming dimension, not build a new one.

**M1 is real, consumed, and cheap to generalize.** But it windows over a *fully materialized*
buffer, so it delivers no streaming/RSS value on its own.

---

## 3. The consumer question (directive Y.13) — measured NEGATIVE

**There is no chunked-feed streaming consumer for parse-that's grammars, at any granularity.**
Evidence (exhaustive grep over `parse_that/src`, empty result):
`grep -rniE 'fn feed|fn push_bytes|Decoder|BufRead|document_stream|from_reader|next_document|iter_documents'`
returns zero streaming entry points. The complete public entry surface is whole-buffer:

- `Parser::parse(&self, src: &'a str)` (`parse.rs:164`), `parse_or_error` (`:173`),
  `parse_return_state` (`:146`) — every one takes the entire `&'a str`.
- `ParserState::new(src)` (`state.rs:285-305`) requires the whole input and *immediately
  materializes a full second copy*: `allocate_padded_buf` (`state.rs:82-106`) does one
  `copy_nonoverlapping(input.as_ptr(), dst, input.len())` of the whole input (`:100`) into a
  64-byte-aligned padded `Vec<PaddedChunk>`. Peak-RSS therefore already carries input + a full
  padded duplicate before the first byte is parsed.
- `lazy()` (`lazy.rs:49-65`) is a **false friend**: it defers *parser construction* (for
  recursion — `LazyParserFn: Fn() -> Parser`, `:6-18`), not input. It has nothing to do with
  lazy input windows.

The three shipped grammars (`parsers/json.rs`, `parsers/css/`, `parsers/csv.rs`) each build a
whole **owned** tree: `JsonValue` with `Box<Vec<..>>` arrays/objects and `Cow<str>`
(`json.rs:12-19`). PROVE-09 REPORT measures parse of twitter.json (631 514 B) at **5 390
allocs** — the output tree is the alloc/RSS mass, "unaffected by" scanner changes, and it is
built eagerly to completion. There is no visitor / event / SAX consumer that could accept
structural events before end-of-input.

**Verdict for the family:** per Constitution clause viii (Y.13, consumer-less substrate
forbidden), a streaming input surface (M2) has **no in-wave consumer today**. To be lawful, 9D
MUST co-deliver a streaming consumer — the minimal honest one is an **ndjson / concatenated-value
document iterator** (simdjson's `document_stream` granularity: whole documents in a stream, never
a byte-partial document). A single-document byte-partial streaming parser has no consumer and no
demand and would be a design defect.

---

## 4. The backtrack-bound (the cheap decider) — structural analysis + instrumentation spec

parse-that is a **backtracking PEG**. Rewind mechanics: a checkpoint saves `state.offset`
before a branch and restores it on failure. Three restore sites, all identical in shape:
- `Parser::or` (`combinators/methods/alternation.rs:12-26`): `checkpoint = state.offset`; on
  each failed alternative `furthest_offset = furthest_offset.max(offset); offset = checkpoint`.
- `save_state` flag path (`parse.rs:105-119`): same save/restore, gated by `FLAG_SAVE_STATE`.
- `seq!`/`alt!` macros (`combinators/macros.rs:91-117`) and the flat N-way alt
  (`leaf.rs:305`) update `furthest_offset` identically.

`furthest_offset` (`state.rs:211`) is **monotone non-decreasing** (always a `max`), never reset
on backtrack, and feeds diagnostics only (`snapshot_diagnostic`, `state.rs:436-463`;
`ParseError.furthest_offset`, `parse.rs:13`). The **rewind distance** the directive names is
exactly `furthest_offset − offset` observed at a restore site, i.e. how far the deepest failed
branch consumed before the cursor snapped back.

### Structural bound (no measurement can grant a guarantee PEG semantics withhold)

**PEG gives NO bounded-backtrack guarantee.** A rule `("a"* "b") | ("a"* "c")` scans the entire
`a`-run, fails at `b`, and rewinds the whole run — rewind distance is O(input). This is the
canonical unbounded-lookahead property (Ford 2004, *Parsing Expression Grammars*). Nothing in
parse-that's engine bounds it; `or`'s checkpoint is an arbitrary earlier offset.

**But the shipped JSON/CSS grammars are near-LL(1) and prefix-disambiguated,** so their *measured*
distribution is expected small-and-bounded:
- JSON `json_value` alternates null/true/false/number/string/array/object (`json.rs`). Each
  alternative is disambiguated by first byte — `sp_string("[")`/`("{")`/`"null"` fail on the
  first byte mismatch consuming ~0 bytes; `number` backtracks at most a lone `.`
  (`parsers/scan/number.rs:282`) or an exponent marker (`:330`) — single bytes. Failed
  alternatives rewind ≈ one token.
- CSS carries a few genuine multi-byte lookaheads: the `not` media-condition prefix backtrack
  (`parsers/css/media.rs:198`) and selector alternation. Still token-bounded in practice, never
  document-scale.

So the honest statement: **rewind is structurally unbounded (PEG), empirically small on the
shipped corpora.** A window sized to the *measured* max would be sound only if the grammar class
is frozen — a fragile guarantee. The cheap decider must therefore *measure* the distribution to
size a candidate window AND record that the size is corpus-conditional, not semantic.

### Instrumentation spec (what the prototype measures — no build run performed here)

A non-invasive probe: at every restore site (`alternation.rs:23`, `parse.rs:117`,
`macros.rs:91-117`) sample `d = state.furthest_offset − state.offset` immediately before
`offset = checkpoint`, accumulate a histogram in a thread-local (mirror the `diagnostics`
COLLECTED_DIAGNOSTICS pattern, `state.rs:468-492`). Run over the six JSON corpora
(`apache-builds, canada, citm_catalog, data, data_xl, twitter`) + the CSS corpora
(`bootstrap, normalize, tailwind-output`), **with and without `--features diagnostics`** (the
directive's fork — diagnostics recovery via `recover()`, `combinators/methods/recover.rs`,
adds `sync`-forward re-scans that lengthen tails). Report P50/P99/max of `d`. The decision datum:
is P99 `< W` for a small `W` (e.g. one stripe, 64 B) or is the tail unbounded? Only a measured
P99 within a small window makes M2's window-discard sound on these corpora.

---

## 5. The candidate value — latency / TTFS / peak-RSS (never throughput)

Per directive constraint + Constitution clause iv: **make no throughput claim.** The straddle
guard (re-checking a match that spans a window boundary) is expected neutral-to-slower on
bytes/s. The only defensible value is latency and memory:

- **Peak-RSS.** Today = input + full padded copy (`allocate_padded_buf`, `state.rs:100`) +
  owned output tree (5 390 allocs for twitter). A windowed input surface that scans stripes in
  place removes the *padded-copy duplicate* (one input-sized allocation) and, with a streaming
  consumer, caps resident input at one window. But the output tree dominates RSS and is
  untouched unless the consumer is also streaming — reinforcing §3: RSS value requires the
  consumer, not just the scanner.
- **TTFS (time-to-first-structural).** Today the padded memcpy of the whole input precedes any
  parse work; first structural is emitted only after full materialization + descent to the
  first leaf. A stripe-fed surface could emit the first structural after one 64-byte stripe.
  This is a *latency* win, MEASURED as wall-time-to-first-`JsonValue`, only meaningful to a
  consumer that acts on early structurals (again: no such consumer exists).

**Both candidate values are gated on the streaming consumer.** Absent it, 9D's measurable
surface collapses to "remove the padded-copy duplicate" — a peak-RSS delta with no streaming,
which belongs to the resident-buffer scanner reform, not to a lazy-surface family.

---

## 6. Prior art (citations)

- **simdjson `document_stream`** (Langdale & Lemire, *Parsing Gigabytes of JSON per Second*,
  VLDBJ 2019; simdjson On-Demand docs): streams at **whole-document** granularity (ndjson /
  concatenated JSON), batches ~1 MB, **never a byte-partial document**. This is the *ceiling* of
  what a lawful streaming consumer for parse-that should target — the directive names it
  explicitly as the honest granularity.
- **nom `streaming` vs `complete`** (`nom::Err::Incomplete(Needed)`): the canonical
  combinator-streaming model — a combinator returns `Incomplete` when it needs more bytes,
  letting a driver `feed` more. The cost: every leaf must be dual-mode (partial-aware), doubling
  the combinator surface. Direct precedent for what M2 would demand of parse-that's ~40 leaves.
- **winnow `Partial<&[u8]>`** (nom successor): wraps input to signal "more may come";
  `ErrMode::Incomplete`. The modern, cleaner encoding of the same dual-mode requirement — a
  window-type wrapper rather than a whole rewrite. The most transplantable model for a
  `PaddedView`-analogue windowed input type.
- **Rust `regex-automata`**: deliberately provides **no streaming search API** because
  unbounded look-around cannot be resolved without the full haystack — an authoritative negative
  result exactly parallel to §4's PEG unbounded-rewind. A DFA-scanner (9C's territory) is the
  only sublayer that *could* stream soundly (bounded lookbehind), reinforcing that streaming and
  backtracking are antithetical.
- **Packrat / PEG memoization** (Ford 2004): packrat's O(n) memo table pins the *entire* input
  in memory — the anti-streaming property. Any window-discard scheme is incompatible with
  packrat memoization; the two cannot co-exist.
- **tree-sitter incremental parsing**: reuses subtrees across edits but requires the whole buffer
  resident + an edit delta — incremental *re-parse*, not streaming *input*. Not a model for M2.

---

## 7. Named risks / counterexamples

- **R-D1 — Unbounded rewind defeats window discard.** The `("a"* "b")|("a"* "c")` PEG pattern
  rewinds O(input); any window smaller than max-rewind is unsound. *Guard:* the §4 histogram
  must show P99 within the window on the target corpora AND the design must freeze the admissible
  grammar class (or fall back to full retention, forfeiting the RSS value). A window sized to a
  measured corpus max is corpus-conditional, not a guarantee.
- **R-D2 — Consumer-less substrate (Y.13 violation).** A windowed input plane with no streaming
  consumer is a defect by Constitution clause viii. *Guard:* co-deliver the ndjson document
  iterator (simdjson granularity) in-wave; do not ship the surface alone.
- **R-D3 — Straddle guard regresses the common (non-streaming) path.** Adding boundary
  re-checks to every stripe scan taxes the 99% whole-buffer case. *Guard:* the window
  abstraction must ICF-fold to today's resident-buffer scan when the input is fully present
  (a `Partial`-vs-`Complete` type distinction à la winnow, resolved at compile time), so the
  non-streaming path pays zero — mirrors the OnceLock-facade lesson (LOCK-9 shared fact: an
  indirect facade blocks inlining; a `cfg`/type pivot ICF-folds).
- **R-D4 — Output tree dominates RSS; scanner streaming buys little.** 5 390 per-parse allocs
  (owned tree) swamp the one padded-copy allocation a windowed input removes. *Guard:* honest
  accounting — the peak-RSS claim must isolate the padded-copy delta from the tree, and any
  larger RSS win requires a streaming *output* consumer, not a streaming input.
- **R-D5 — `diagnostics` recovery lengthens tails.** `recover()` (`recover.rs`) re-scans via a
  `sync` parser after a failure, and `add_expected` accumulates at `furthest_offset`
  (`state.rs:386-403`); the with-diagnostics histogram may have a heavier tail than without.
  *Guard:* the §4 measurement runs both feature configs; the window is sized to the worse.

---

## 8. What a prototype MUST test to move this family

1. **Rewind histogram (the cheap decider).** Instrument the three restore sites
   (`alternation.rs:23`, `parse.rs:117`, `macros.rs:91-117`) with a thread-local histogram of
   `furthest_offset − offset`; run over all six JSON + three CSS corpora, `--features
   diagnostics` on and off; report P50/P99/max. **Decision:** P99 < 64 B (one stripe) ⇒ a
   single-stripe window is corpus-sound; unbounded tail ⇒ M2 is unsound without full retention.
2. **The consumer.** Build the minimal lawful streaming consumer — an ndjson/concatenated-value
   document iterator at simdjson `document_stream` granularity — and prove it consumes
   parse-that grammars incrementally. Without this, the family is a Y.13 defect and closes as a
   negative result (a valid close per P4/P39).
3. **Peak-RSS + TTFS, measured (no throughput).** Paired cold measurement (divan wall-time +
   `/usr/bin/time -l` peak-RSS on macos-14) of: (a) removing the `allocate_padded_buf`
   duplicate under a windowed scan; (b) wall-time-to-first-structural stripe-fed vs
   whole-buffer. Report peak-RSS delta and TTFS delta only; assert no bytes/s claim (the
   straddle guard is expected neutral-to-slower).
4. **Zero-cost non-streaming path.** Prove a `Partial`/`Complete` input-type distinction
   (winnow model) ICF-folds the whole-buffer case back to today's resident scan via `otool -tv`
   (no residual straddle branch in the complete-input path) — the R-D3 guard.

**Family disposition (no verdict rendered):** 9D's windowed-scan substrate (M1) is real and
consumed; its streaming-input center (M2) is structurally at odds with PEG unbounded rewind and
has zero extant consumer. The family advances only if item 1 shows a small measured P99 AND item
2 delivers the document-stream consumer; otherwise it closes on a measured refutation with the
histogram + the padded-copy peak-RSS delta as the artifact.
