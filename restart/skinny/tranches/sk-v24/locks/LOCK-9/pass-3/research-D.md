# LOCK-9 · PASS-3 · FAMILY 9D — THE LAZY-SURFACE (streaming-first / cost-model kernel selection)

Charter: scanners as incremental lazy planes over input windows; kernels selected by a cost
model at combinator-compile time; streaming parse support as the design center. This report
develops 9D to its strongest HONEST form: it distinguishes the family's two live clauses
(deferred kernel selection — consumer-bearing) from its dead clause (lazy INPUT streaming —
consumer-less and soundness-blocked), and states the measurements that settle each. HEAD:
parse-that `ef10d5b` (R3 §1); substrate skinny bbnf-simd @ current tree; base-pin for any
number = the PT-1/PT-2 W0 banked baseline (PT-2.md §4 W0).

---

## 1. The streaming surface census — there is none (F4)

`grep -rE "feed|push_bytes|Decoder|BufRead|document_stream|from_reader|io::Read|Iterator<Item"`
over `parse_that/src` returns **zero real consumers** — the only hits are `regex/info/mod.rs:135`
("feeds into tier planning CSP", prose) and `scanners.rs:85` ("form feed", a comment). Every
grammar builds a whole owned tree over `&'a str`. `ParserState<'a>` (`state.rs:202`) holds
`src: &'a str`, `src_bytes: &'a [u8]`, AND an owned `padded_buf: Vec<PaddedChunk>` allocated
whole at `ParserState::new` (`state.rs:285-305`). Input is whole-document by construction; no
chunked feed, no reader, no `Incomplete` contract exists anywhere in the crate.

**Whole-document-granularity streaming does not reduce peak tree RSS** — the family charter's
own qualifier, and it holds. The output tree is owned regardless of how input arrives; the
only RSS a document-stream could shave is the INPUT buffer, and §3 shows even that is blocked.

## 2. The existing "lazy" is deferred CONSTRUCTION, not deferred INPUT

`lazy.rs:20-65` is the sole `lazy` in the tree: `LazyParser` boxes a `Fn() -> Parser` thunk,
builds the inner `Parser` once, caches it (`cached_parser: Option<Parser>`), and thereafter
bypasses flag dispatch (`lazy.rs:56-62`). This exists to break the construction cycle for
recursive grammar rules — it is lazy PARSER CONSTRUCTION, never lazy input consumption. It is
the mechanism template for clause-2 (deferred kernel selection, §5) and is NOT a streaming seam.

## 3. Why lazy INPUT streaming is soundness-blocked — the decisive counterexample (F4)

parse-that returns `Span<'a>` (`state.rs:172`) whose fields borrow `src: &'a str` — a
zero-copy view into ONE buffer. A chunked `feed(&[u8])` API that frees consumed chunks
**dangles every earlier span**; keeping all chunks alive gives no RSS win. The only escape is
copying spans out to owned `String` — which reintroduces exactly the per-parse alloc mass
streaming was meant to avoid (PROVE-09 REPORT §72: twitter parse = **5390 allocs**, "Vec
growth in many/sep_by + string arena — the real alloc mass"). Streaming is thus incompatible
with the borrowed-span output model unless the whole value surface converts to owned copies,
whereupon the alloc mass returns. This is a hard wall, not a tuning knob.

Compounding it: the PaddedView SIMD contract is anti-chunking. `INPUT_PAD_BYTES = 64`
(`state.rs:57`); `allocate_padded_buf` (`state.rs:82-106`) does ONE `copy_nonoverlapping` of
the whole input plus a trailing 64-NUL guard, and every kernel loads a full 64-byte stripe
past the tail WITHOUT a bounds check (`padded_bytes`, `state.rs:313-344`). A per-chunk feed
breaks the trailing-NUL invariant at every boundary → either re-pad each chunk (a copy tax
that defeats streaming) or reinstate the per-stripe tail guard (surrendering the exact win
PaddedView was built for). simdjson is the SOTA precedent: `SIMDJSON_PADDING = 64` (parse-that
mirrors it byte-for-byte), and simdjson does NOT stream a single document — its
`document_stream` is NDJSON (many small docs, each parsed whole). The fastest engines pad and
materialize; they do not chunk single-doc input.

**Standing law binds hardest here:** SPEC §0/51 — *fold-over-lazy-cursor dead (2.3-4.9×
behind)*. A lazy-input cursor resurrects the refuted architecture. 9D's "streaming as design
center" cannot be pursued as literal input streaming without violating this law AND
Constitution viii (consumer-less substrate forbidden — a streaming parser would be born
dormant).

## 4. The rewind distribution — bounded, but unmeasured (F4)

The parser is a backtracking PEG whose restore sites are LOCAL checkpoints held on the Rust
call stack as `usize` offsets — never an unbounded history buffer:
- `one_of` (`leaf.rs:298-311`): `checkpoint = state.offset`; on arm failure
  `furthest_offset = max(furthest, offset); offset = checkpoint` (`leaf.rs:305-306`).
- `many` (`span_trait.rs:84-99`): restores `prev_offset` on element failure; breaks on
  zero-advance (left-recursion guard, `span_trait.rs:94`).
- `sep_by` (`span_trait.rs:141-153`): `cp = state.offset` before the separator; restores `cp`
  to reject a trailing separator.

Rewind distance = how far a failed alternative advanced before failing. JSON/CSS are largely
first-byte-dispatchable — `dispatch_byte` (`leaf.rs:317`) is an O(1) 256-entry LUT that peeks
the next byte with ZERO speculative advance — so committed alternatives rarely deep-advance
then fail. Backtrack is therefore expected P50≈0, P99 bounded by the longest keyword/token,
max bounded by the longest backtracked production — i.e. **a bounded ring buffer WOULD suffice
if streaming existed**. But no histogram harness exists; the actual P50/P99/max (diagnostics
on/off) must be MEASURED, not asserted (§6.1). There is no packrat memo in the Rust core
(packrat is the TS S.H1 epoch, R3 §1) — so no memo-history fights streaming, but also no
bounded-backtrack accounting is banked.

## 5. The live clause — cost-model kernel selection at combinator-compile time (F1 seam)

This is 9D's ONLY consumer-bearing surface, and the strongest form of the family leans on it.
The charter's "kernels selected by a cost model at combinator-compile time" already has a
proven analogue in the substrate:

- **bbnf-simd `dispatch.rs`** — `select_backend(alphabet)` (`dispatch.rs:89-98`) picks
  `NeonTbl4` vs `Scalar` by `lo6_table_admissible` (`dispatch.rs:100-113`): a static
  separability predicate on the 64-byte alphabet (each byte's low-6 bits distinct → a single
  `vqtbl` classify is admissible). `select_primitive_kernels()` (`dispatch.rs:63-87`) picks
  per-arch kernel fn-pointers via `OnceLock`. This is a cheap-predicate dispatch — the exact
  "cost model chooses a kernel" shape, at INIT time not comptime.
- **parse-that scan leaves** carry the un-modelled selection points: `TakeUntilScan::Lut(Box<[bool;256]>)`
  (`leaf.rs:182,203,209`) is a hand-built delimiter LUT with NO cost model — always the LUT,
  never a SIMD shufti even when the delimiter set is separable. This is the seam where a
  comptime cost model (mirroring `lo6_table_admissible`) selects LUT-vs-shufti at `Parser::new`
  by a static property of the delimiter set; `lazy.rs`'s deferred-construction thunk is the
  select-once-cache mechanism.
- The GENERATOR-side precedent already exists and is measured: SPEC §1 — "grammar-specialized
  SIMD kernels behind a cost model" — features 05 (CSS shufti token kernel, tailwind −23.6%,
  VERIFY CONFIRMED_DELTA) and 12 (emit-spine residualization, const-fold delimiter mask, CSS
  0.80-0.91× cyc/B, CONFIRMED). Grammar-specialized kernel selection is a bbnf-lang emit-spine
  facility today; parse-that's hand-written combinators lack the analogous comptime seam.
  9D's live contribution = port that selection discipline to the parse-that scan leaf, using
  bbnf-simd's `lo6_table_admissible` predicate as the reusable cost model.

Constraints on the model (SPEC §0): it must select among membership/structural kernel TIERS,
never a block schedule (§0/04 refuted, 1.05-1.06×) nor a digit-scan SIMD (§0/02 NO_GO); M-series
is NEON-only (§0/54); the lever is movemask-narrow + vectorized compaction (§0/45).

## 6. What a prototype must test to move 9D

### 6.1 Rewind histogram harness (settles the streaming-feasibility question)
Instrument the three restore sites (`one_of` `leaf.rs:305`, `many` `span_trait.rs:96`,
`sep_by` `span_trait.rs:143,152`) to record `checkpoint − final_offset` per restore; run the
6 JSON corpora + 3 CSS corpora (R3 §6). Emit P50/P99/max, diagnostics on/off, base-pinned.
Deliverable: the objective bounded-vs-unbounded proof. If max is bounded (near-certain for
first-byte-dispatch grammars) → a bounded ring buffer WOULD suffice, banked as a positive
feasibility result WITHOUT building an unconsumed streamer.

### 6.2 Span-dangle soundness probe (the decisive refutation, measured)
A compile-fail test demonstrating a chunked `feed(&[u8])` cannot return `Span<'a>` without
(a) keeping all chunks alive — measure input-RSS = whole-doc, no win; or (b) copying spans to
owned `String` — measure alloc delta vs the 5390-alloc baseline (PROVE-09 §72). Deliverable:
the RSS-vs-alloc tradeoff table that files streaming-as-design-center as a MEASURED negative
(SPEC-style refutation-as-law), the honest close for the family's dead clause.

### 6.3 Comptime cost-model kernel-selection micro (the live clause)
At `Parser::new`, select `TakeUntilScan::Lut` vs a SIMD shufti kernel by a static
delimiter-set predicate ported from `bbnf-simd dispatch.rs:100 lo6_table_admissible`; measure
the `take_until` micro (`benches/parse_that/micro.rs`, R3 §6) against always-LUT and
always-SIMD arms, cold paired, N≥200, two windows. Deliverable: the win/neutral/loss table
for comptime kernel selection — the consumer-bearing part of 9D, scan leaves as in-wave
consumer.

## 7. Prior art (citations)

- **tree-sitter** — `TSInput.read` callback streams chunks; but its streaming serves
  incremental EDIT reparse over a PERSISTENT tree, not single-parse RSS reduction. Confirms
  the charter qualifier: streaming ≠ RSS win.
- **nom** `Streaming`/`Complete` modes + `Err::Incomplete(Needed)` — the only mature Rust
  combinator streaming contract; measured slower than Complete (an Incomplete branch per
  primitive). The reference design IF 9D pursued a streamer, and the cautionary cost.
- **attoparsec (GHC)** — `Partial (ByteString -> Result)` continuation model; the
  continuation-passing alternative to nom's Incomplete flag. Both copy the value out (no
  borrowed spans) — corroborating §3's span-dangle wall.
- **regex-automata** `hybrid::dfa` — "lazy DFA" = on-demand memoized transitions, NOT lazy
  input (a naming caution); `Input` is a whole `&[u8]`; streaming search needs a bounded
  look-behind window = the ring-buffer bound of §6.1.
- **simdjson / sonic-rs** — `SIMDJSON_PADDING = 64` (parse-that's `INPUT_PAD_BYTES = 64`
  mirrors it); `document_stream` is NDJSON only, each doc parsed whole. SOTA precedent: pad +
  materialize, never chunk single-doc.

## 8. Named risks & counterexamples

- **R-DEAD** — fold-over-lazy-cursor 2.3-4.9× regression (SPEC §0/51): any lazy-input cursor
  resurrects the refuted architecture. Hard block on literal streaming.
- **R-SPAN** — `Span<'a>` borrows one buffer (`state.rs:172`); freeing consumed chunks
  dangles earlier spans; retaining them = no RSS win; copying out = the 5390-alloc mass
  returns. The decisive soundness counterexample (§3).
- **R-PAD** — PaddedView's 64-NUL contract (`state.rs:57,82`) breaks at chunk boundaries →
  re-pad (copy tax) or tail-guard (lost SIMD win). Anti-streaming by construction.
- **R-CONSUMER** — Constitution viii: a streaming parser has no in-wave consumer; born
  dormant = forbidden. The live clause (§5) is the only surface that satisfies Y.13.
- **R-COST** — a comptime cost model must select among membership/structural kernel TIERS,
  never a block schedule (§0/04) nor a digit-scan SIMD (§0/02); the predicate is separability
  (`lo6_table_admissible`), not throughput speculation.

## 9. The strongest honest form of 9D

Not "streaming parse as the design center" — that clause is consumer-less, soundness-blocked
by borrowed spans (§3), anti-SIMD by the pad contract (§3), and dead-by-law (§0/51). The
family's defensible core is a **cost-model kernel-selection plane at combinator-compile time**
(§5), consuming bbnf-simd's `lo6_table_admissible` predicate at the parse-that scan leaves —
with the streaming ambition discharged as a MEASURED refutation (§6.2, the RSS-vs-alloc table)
plus a banked bounded-backtrack histogram (§6.1) certifying a ring buffer WOULD suffice if a
streaming consumer ever appeared. Live clause lands; dead clause files as law; the family
closes on artifacts either way.
