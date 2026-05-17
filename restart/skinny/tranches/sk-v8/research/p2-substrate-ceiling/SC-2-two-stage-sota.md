---
tranche: SK-V8
phase: P2 substrate-ceiling
artefact: SC-2
title: The two-stage SOTA architecture vs the bbnf single-pass offset-tape
date: 2026-05-17
scope: research-only; no code edits; no commits
lens: substrate-ceiling — does the offset-tape substrate impose an irreducible per-structural-element cost?
authority_inputs:
  - skinny/RESULTS.md (post SK-V7 W10c)
  - restart/skinny/tranches/sk-v7/research/skv7-A2-sota-strict-beat.md
  - restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md
  - restart/skinny/tranches/sk-v7/SYNTHESIS.md
  - skinny/crates/runtime/src/tape/{mod,assembler}.rs
  - skinny/crates/runtime/src/grammars/json/{scan,generated,parser}.rs
external_anchors:
  - "Langdale & Lemire, 'Parsing Gigabytes of JSON per Second', VLDB Journal 2019 (arXiv:1902.08318)"
  - "simdjson source: src/generic/stage1, src/generic/stage2, On Demand iterator"
  - "sonic-rs upstream README + skip-scan architecture notes"
  - "yyjson upstream README + yyjson.c read path"
  - "asmjson docs.rs + bbnf.asm:316-368 DPDA model"
---

# SC-2 — The Two-Stage SOTA Architecture

## §1. Findings

### 1.1 The simdjson two-stage architecture, precisely

simdjson (Langdale & Lemire, "Parsing Gigabytes of JSON per Second",
arXiv:1902.08318, §3–§4) is explicitly and irreducibly **two stages**, and the
separation is the whole performance thesis.

**Stage 1 — the structural indexer.** Stage 1 reads the input exactly once,
64 bytes at a time (two 32-byte AVX2 loads, or one 64-byte AVX-512 load), and
produces *one output*: a flat array of `u32` indices, one per *structural or
pseudo-structural character*. The structural characters are `{ } [ ] : ,` and
the first character of every atom (string-opening `"`, and the leading byte of
each number/`true`/`false`/`null`). Stage 1 is, in order:

1. **Quote/escape state resolution** — a backslash mask is computed, a prefix
   problem (`escape_mask`) cancels escaped quotes, and a *carry-less prefix-XOR*
   (`_mm_clmulepi64_si128` against `0xFF...FF`) turns the surviving quote bitmap
   into an "inside-string" bitmap in one instruction. This is the paper's §4.2
   — the single most-cited trick.
2. **Character classification** — `vpshufb`/TBL lookups assign each byte to a
   class (whitespace, structural, other) using a 16-entry nibble table.
3. **Structural-bit identification** — structural bits = (classified-structural
   OR atom-leading) AND NOT inside-string.
4. **Bit-to-index extraction** — the per-block 64-bit structural bitmap is
   converted to indices by an *unrolled, branch-free* `tzcnt`/`blsr` loop that
   `compressstore`s 8 indices at a time regardless of how many bits are set
   (the paper §4.4: writing 8 and overwriting is faster than branching on
   popcount).

Stage 1 is **completely branch-free over data** — there is no `if` whose
direction depends on the document's content in the hot loop. Branch
mispredictions are the dominant cost of scalar JSON parsers (paper §2), and
stage 1's design goal is to spend zero of them. Its output, the structural
index, is *grammar-agnostic at the byte level*: it knows nothing about JSON
nesting; it only knows "these byte offsets are where something interesting
begins". UTF-8 validation runs as a fused side-channel of the same pass.

**Stage 2 — the tape builder.** Stage 2 walks the *structural index*, not the
input. It is a goto-threaded state machine (`object_begin`, `array_begin`,
`object_field`, etc.). At each step it reads the *next index* from stage 1,
dereferences the input only at that offset, dispatches on the single byte
there, and appends a 64-bit *tape word* (the simdjson tape: a tagged
`(type, value/payload)` 64-bit cell, with strings/numbers spilled into a side
buffer). Crucially, stage 2 **never scans** — it never executes an inner
character loop searching for a closing quote or a number's end, because stage 1
already told it where the *next* structural character is. The closing quote of
a string is simply "the next index". A number's extent is "this index to the
next index". Stage 2's only loop is *one iteration per structural element*.

**Why the separation buys throughput.** The paper's §6 measurement isolates it:
the two stages run at very different IPC and very different
branch-misprediction rates. Stage 1 is a wide, predictable, memory-streaming
kernel at ~3 GB/s+ with near-zero data-dependent branches. Stage 2 is a
narrow, pointer-chasing state machine — but because it is *fed indices*, its
inner work per element is O(1) and its branches dispatch on a tiny finite
alphabet of structural bytes. The conflated single-pass alternative would force
the predictable wide kernel and the unpredictable narrow machine to *share one
instruction stream and one branch-predictor budget* — every string body would
be re-walked by the same code that does structural dispatch, and the scan for
"where does this string end" would run in the slow, branchy plane. The
two-stage split is a **work-partitioning** that lets each plane run at its own
natural IPC. Stage 2's per-element cost is bounded and *independent of token
length*: a 2-byte string and a 2000-byte string both cost stage 2 exactly one
index-read + one tape-append. Token length is paid *once*, in the wide
branch-free stage-1 stream.

**On Demand.** simdjson On Demand (the lazy front-end) keeps stage 1 verbatim
and *replaces stage 2 with a forward-only iterator* over the same structural
index — values materialise only on access (`obj["field"]`), "values can only be
parsed once". This is the proof that stage 1's output is a *reusable
substrate*: the same index drives an eager tape build or a lazy iterator.

### 1.2 asmjson — SWAR + AVX-512, single-pass DPDA

asmjson (per `bbnf.asm:316-368` model and skv7-A2 §6, skv6-A1) is *not*
simdjson's index-then-walk shape. It is a **9-state DPDA** (deterministic
pushdown automaton) that fuses classification and structural emission into one
streaming pass. Its loop per 64-byte AVX-512BW chunk:

1. `vpcmpeqb` × N against the structural byte set, `korq`-reduced to a 64-bit
   class mask (`BYTE_CLASS_FROM_EQ_SET_64`).
2. `tzcnt`-driven seek to the next set bit (`BITMAP_NEXT_SET_BIT`), 18× per
   the AVX-512 instruction histogram.
3. PC-as-state direct threading: register `r10` holds the next-state target
   across chunk boundaries — *no state-variable memory traffic*
   (`FSM_DISPATCH_THREADED`).
4. An explicit bounded stack (`frames_buf[64]`, `open_buf[64]`) for
   bracket-pair tracking (`FRAME_PUSH_BOUNDED` / `FRAME_POP_BOUNDED`).

asmjson differs from simdjson in two ways that matter to this report. First,
**it does not produce a separate persistent structural index** — the mask is
consumed within the same chunk that produced it, and the DOM tape word is
emitted directly from the DPDA transition. It is "two-stage" only in the sense
that classification *precedes* emission *within a chunk*; there is no
materialised intermediate array spanning the document. Second, asmjson hits
10.93 GiB/s on *plain AVX-512BW + BMI1* — `vpcmpeqb`, `korq`, `tzcnt`, no
esoterica — and pays for it with **permissive strictness**: it treats every
byte `<0x20` as whitespace and never scans string bodies for unescaped control
characters (skv7-A2 §3). asmjson's design lesson is that the *DPDA-in-registers*
model collapses the stage-1/stage-2 boundary *without re-materialisation* — but
only because the per-element work is a register-resident state transition, not
a tape append through memory. asmjson does not lower to typed visitors;
generality is JSON-specific in the ASM source.

### 1.3 sonic-rs — single-pass with a skip-scan, no persistent index

sonic-rs (skv7-A2 §2–§3, upstream README) **explicitly rejects simdjson's
two-stage tape**. There is no materialised structural index that survives
between passes. sonic-rs's SIMD work is a set of *bounded skip-scan kernels*
invoked *on demand inside* a single recursive-descent / `serde::Deserialize`
driver: `skip_string`, `skip_number`, `get_from` use SIMD (`vpcmpeqb` + bitmask
+ `tzcnt`) to *jump* over a token in 16/32/64-byte strides, but the result is
consumed immediately by the value constructor — it is never written to an index
array. sonic-rs is therefore **single-pass with vectorised inner scans**: the
structural projection is *ephemeral, per-token, register-resident*. Its lazy
modes (`LazyValue`, `get_from` pointer access) skip-scan past unwanted subtrees
without ever building their tape. The architectural bet: a persistent
structural index is *memory traffic you can avoid* if your inner scans are fast
enough to be re-derived locally — and for product-shaped JSON (twitter, gsoc)
sonic-rs's bet wins, which is why sonic-rs is the strict comparator bbnf loses
to on exactly those rows.

### 1.4 yyjson — single-pass scalar, no SIMD, no index

yyjson (skv7-A2 §2–§3, upstream README) is single-pass C89 scalar with **no
explicit SIMD and no structural index whatsoever**. It walks the input one
byte at a time inside one force-inlined function that fits in ≤20 KiB i-cache,
appending typed-cell DOM nodes as it goes. `\uXXXX` decode is fused *into* the
string-walk loop — no function-call boundary. yyjson is the proof that a
*perfectly fused* single pass with disciplined i-cache footprint beats SIMD
when the SIMD implementation pays dispatch overhead or stage-boundary memory
traffic — on M5 Max twitter, yyjson at 3687 MiB/s beats simdjson DOM at
2923 MiB/s. yyjson does NOT two-stage; its lesson is the *opposite* of
simdjson's: total fusion of a narrow scalar machine. The two SOTA poles are
thus (a) simdjson — wide branch-free stage 1, fed-index stage 2; and
(b) yyjson — total scalar fusion. Both beat bbnf on string-heavy corpora.

### 1.5 What bbnf's offset-tape actually is, and what it conflates

bbnf's substrate is the `OffsetTape` (`runtime/src/tape/mod.rs`,
`tape/assembler.rs`): a `Vec<u32>` of offsets, a parallel sparse
`(flag_cursor, flag_value)` sidecar for `HAS_ESC`/`HAS_CONTROL`, and an
(empty in every measured row) `PayloadArena`. RESULTS.md confirms the tape is
genuinely offset-only: every row reports `0 payload bytes`,
`0/0 writes/allocations`. So far this *looks* like a simdjson-style structural
index — it is, structurally, "an array of `u32`, one per structural element".

There is even a real stage-1 in the tree: `scan.rs::scan_structurals` is a
genuine branch-free-ish NEON structural indexer — it does the quote/escape
prefix-XOR (`prefix_xor_64`), the TBL classification
(`classify_block_from_table`), and `compact_mask` bit-to-index extraction.
**That kernel is a simdjson stage 1.**

The conflation — and this is the load-bearing finding — is that **the generated
recursive-descent parser does not consume the structural index that
`scan_structurals` produces.** `parser.rs:18` calls `structural_capacity_for`,
which calls `scan_structurals` *only to size the `Vec`* (`CapacityPlan::Exact`
and `OneShotSimd` literally throw the index away after counting it; the
production default `GrowOnly` doesn't even scan). The generated `parse_value_at`
(`generated.rs:30+`) then **re-walks the input byte-by-byte from scratch**:
`match_tiny_plain_string` and `match_string_at_quote` (`generated.rs:95,98,
147,151`) each run their *own* inner character loop searching for a string's
closing quote. This is precisely the inner scan that a fed-index stage 2 never
performs. bbnf is therefore *single-pass at the substrate level the way sonic-rs
and yyjson are* — but **without sonic-rs's vectorised skip-scans and without
yyjson's total i-cache fusion**. It pays the single-pass branchy inner-scan cost
of yyjson with the function-call-boundary overhead yyjson explicitly avoids
(`match_string_at_quote` is a cross-crate call into `parse-that-regex`,
`generated.rs:6,193,625`).

The offset-tape is then *built a third time*, conceptually: `TapeBuilder::
push_offset` appends an offset per structural element as the recursive-descent
parser discovers it. So bbnf's actual per-structural-element pipeline is:
**(a) scan_structurals produces an index that is discarded; (b) the recursive
descent parser re-discovers every structural position by branchy inner scan;
(c) each discovered position is appended to the offset-tape `Vec<u32>`.** The
index built in (a) is byte-identical in *content* to the offsets written in
(c). bbnf builds the structural projection twice and consumes it once, and the
consume happens in the slow plane.

## §2. Substrate-ceiling verdict

**Does the two-stage separation explain bbnf's string-plane losses? Yes —
decisively, and the mechanism is specific.**

The substrate-ceiling hypothesis (P2's framing) is that the offset-tape imposes
an irreducible per-structural-element cost. The two-stage lens *refines* that
hypothesis into something falsifiable and correct: the cost is not the
`Vec<u32>` of offsets itself (that is cheap — RESULTS.md shows tape sizes of
0.05x–0.50x input, and `push_plain_offset` is a branch-predicted unsafe write).
The irreducible cost is that **bbnf's structural projection is consumed in the
branchy recursive-descent plane instead of being produced once branch-free and
walked as indices.**

The evidence is exact and corpus-correlated:

- The SK-V7 cohort measured `match_string_at_quote` ≈ 47% + `match_tiny_plain_
  string` ≈ 28% ≈ **75% of total self-time** on the string-heavy rows
  (SYNTHESIS §3.4). Those two functions *are* the per-string inner scan that a
  fed-index stage 2 does not run. In simdjson, finding a string's end is "read
  the next index" — O(1), branch-free, already done in stage 1. In bbnf it is a
  byte loop with a data-dependent branch per byte.
- The losses cluster *exactly* on string/escape density. RESULTS.md: number-
  heavy corpora where strings are rare *win* — canada (+54.6% vs sonic strict,
  12 string quotes, 111126 numbers), mesh (+51.5%, 11 quotes), numbers, marine_ik.
  String-heavy corpora *lose* — twitter (-35.8%, 18099 quotes), update_center
  (-63.4%, 27229 quotes), distinct_values (-70.8%, 9796 quotes), apache_builds
  (-65.3%, 5289 quotes), y_string_unicode (-54.4%). The discriminant is the
  string-quote count. That is the *signature of a per-string inner-scan cost*,
  i.e. the signature of a missing stage-2-fed-index.
- SK-V7 rejected *every micro-kernel* (Eisel-Lemire, per-`\uXXXX` TBL, NEON
  plain-string scan, control/key compaction, PMULL/CSSC bodies) precisely
  because each one optimised *the inner scan* without removing it. A faster
  branchy inner scan is still a branchy inner scan in the same instruction
  stream as structural dispatch. The substrate ceiling is real, and it is the
  *absence of the stage-1/stage-2 cleavage*, not the offset-tape's data layout.

The verdict, sharply: bbnf already *has* a SOTA-grade stage 1
(`scan_structurals`). It throws the result away and re-derives the same
information in the slow plane. The "irreducible cost" is self-inflicted by
*not consuming the index that has already been built*. The substrate is not
the ceiling — the **missing index-driven stage 2** is the ceiling.

## §3. Recommendation — the union substrate

The user's framing — "a union of tape and structural projection" — is exactly
the right move, and the two-stage lens makes it concrete. The recommendation is
**not** to add a second materialisation pass (that would re-introduce the
stage-boundary memory traffic that yyjson and sonic-rs avoid, and the SK-V7
verdict already shows micro-kernels don't pay). The recommendation is:

> **The structural index IS the offset-tape. Produce it once, branch-free, in
> the stage-1 kernel; consume it lazily as the parser's token cursor.**

Concretely — call it the **fused index-tape**:

1. **One production site.** `scan_structurals` is promoted from a capacity-
   sizing throwaway to *the* structural pass. Its `Vec<u32>` of positions *is*
   the offset-tape's `offsets` vector — no copy, no second build. The
   `(flag_cursor, flag_value)` escape/control sidecar is emitted by the *same*
   stage-1 kernel as a fused side-channel (it already computes the backslash
   mask and the in-string mask — `HAS_ESC` is a popcount-test on a mask it
   already holds). This is the "union": the tape and the structural projection
   become the *same array*, produced in the branch-free plane.

2. **The recursive-descent parser becomes a fed-index walker.** `parse_value_
   at` no longer calls `match_tiny_plain_string` / `match_string_at_quote` to
   *find* a string's end. It reads `tape.offsets[cursor]` and
   `tape.offsets[cursor+1]`: the string body is exactly the half-open interval
   between two consecutive indices; the closing quote *is* the next index.
   String *validation* (UTF-8, unescaped-control rejection) was already done
   branch-free in stage 1 via the masks — the `HAS_CONTROL` flag is the
   rejection signal. The parser's inner loop collapses from O(token-length)
   branchy bytes to O(1) index reads. This is literally simdjson's stage 2,
   minus the separate tape word — because the index *is* the tape.

3. **No second materialisation.** The fused index-tape is consumed *lazily* and
   *forward-only*, exactly like simdjson On Demand: typed `direct_to_struct` /
   `real_typed_struct` rows materialise a field only when the schema asks for
   it, by reading the interval from the index and decoding in place. There is
   no eager tape *word* array distinct from the index array. The offset-tape's
   current `PayloadArena` (empty in every row already) stays empty — payloads
   are decoded on access from the source slice the index points at. This keeps
   the two-stage throughput win (branch-free stage 1; O(1)-per-element stage 2)
   *without* paying simdjson's separate-tape memory traffic — it is the
   simdjson stage split fused onto a sonic-rs-style lazy consume.

4. **Strict-plane preservation.** Strictness is *not* lost by this move — it is
   *strengthened*. simdjson and bbnf both validate UTF-8 and reject unescaped
   controls at the stage-1 scan boundary; the `OffsetFlags::HAS_CONTROL` bit
   already exists for exactly this. asmjson's permissive 10.93 GiB/s is *not*
   the target — the target is strict-vs-strict, and stage-1 branch-free
   validation is the only way to do strict cheaply.

The size budget: this is a *deletion-positive* change. It removes the
`match_tiny_plain_string`/`match_string_at_quote` inner-scan call sites from
the generated parser and removes the throwaway capacity scan. The cost-model
hook is `BackendShape` — the fused index-tape is the lowering for the
`OffsetTape` shape; the per-rule decision "fed-index walk vs inner-scan" becomes
a `CostFacts` fact (the SK-V7 W9 CostFacts substrate is the natural home).

The predicted impact: the string-heavy loss cluster (twitter, update_center,
distinct_values, apache_builds, github_events, the unicode rows — ~9 of 13
parse-G rows) is exactly the set whose hot leaf is the inner string scan. A
fed-index stage 2 removes that leaf. This is the *one structural move* SK-V7's
14 wasted micro-kernel waves could not be, because it changes the substrate
shape rather than speeding the wrong plane.

## §4. Generalisation — grammar-neutral

The fused index-tape is **not JSON-specific**, and the two-stage decomposition
is the most grammar-neutral thing in the SOTA literature, because stage 1 knows
*nothing about grammar* — it only classifies bytes into a structural alphabet.

- **The grammar-neutral interface** is: a grammar declares (a) its *structural
  byte alphabet* (JSON: `{}[],:"`; CSS L4: `{}();:,` plus `/` `*` `@`; Sheets:
  `(),;` plus operators; BBNF-self: `|()[]{}` plus `::=`); and (b) a *string-
  like delimited-region set* (JSON strings; CSS strings + comments + url();
  Sheets quoted text). Stage 1 is then the *same kernel parameterised by a
  per-grammar 256-byte classifier LUT* — exactly the dav1d data-vs-code split
  (skv7-A3 §1, §6): one shared `BYTE_CLASS_FROM_TABLE_64` macro body, one
  per-grammar `.data` table. The fused index-tape `Vec<u32>` is grammar-neutral
  by construction — it is just offsets.

- **The fed-index stage-2 walker** is the existing recursive-descent codegen
  with its inner *find-the-token-end* loops replaced by *read-the-next-index*.
  That transformation is mechanical and grammar-independent: any rule that
  currently scans for a delimiter instead reads the interval `[idx[c], idx[c+1])`.
  Grammars whose tokens are *not* delimited by structural bytes (significant
  whitespace, e.g. YAML) cannot use a fed-index stage 2 — but that is already
  the `CollapsedStage` admissibility boundary (skv7-A3 §5): YAML is rejected,
  CSS L4 declaration-body admits, BBNF-self admits, Sheets admits.

- **CSS L4 specifically.** CSS declarations are structural-byte-delimited
  within a block; `scan_structurals` parameterised with the CSS alphabet
  produces an index whose intervals are selectors, property names, values.
  The escape sidecar generalises directly to CSS hex escapes (`\E9 `). The
  `@`-rule envelope and `@error(recover)` recovery branches are the part that
  falls back — but the *declaration body*, which is the bulk of any
  stylesheet, gets the fed-index stage 2. This matches skv7-A3 §5's
  "CSS L4 declaration-body only" admissibility row.

- **The union is the right shape for the whole framework**, not a JSON hack:
  it says *every grammar with a structural-byte alphabet gets one branch-free
  stage-1 index, and that index is the substrate the typed/event/sink lowerings
  all consume*. That is precisely the "skinny ⊂ greater arch" feedback loop
  (skv7-A3 §7): the JSON close is the proof; the index-tape substrate is the
  generalisation.

## §5. Risks

1. **Stage-1 carry correctness across 64-byte blocks.** The fed-index contract
   requires stage 1's quote/escape state to be *exactly* right at every block
   boundary — a single misplaced index desynchronises the entire stage-2 walk.
   `scan.rs` already has a `fast_path_is_strict` guard that falls back to
   `resolve_string_masks_64` scalar when the SIMD prefix-XOR path cannot prove
   strictness; that fallback must remain, and the fed-index walk must treat the
   index as authoritative *only* on the strict path. Mitigation: differential
   parity (`scalar_parity_report` already exists) gating every corpus row.

2. **Index density vs cache.** A `Vec<u32>` index over a string-dense corpus is
   larger than over a number-dense one (RESULTS.md: gsoc-2018 tape 0.05x input,
   marine_ik 0.48x). For pathological all-structural input the index approaches
   0.5x input size. simdjson accepts this; it is bounded and sequential, so it
   streams from cache well. Risk is low but must be measured, not assumed.

3. **The lazy-consume vs eager-tape decision is a real cost-model fork.** For
   `real_typed_struct` rows that access *all* fields (mesh, marine_ik — every
   numeric required), lazy access has no skip-work win and the fed-index walk
   must be as fast eager as a tuned inner scan. The SK-V7 twitter 151.5% "win"
   was shown to be skip-work (SYNTHESIS §3.6) — the fused index-tape must be
   honest about which rows are skip-work wins and which are genuine. Mitigation:
   the CostFacts substrate records access density per rule.

4. **Refactor surface.** Replacing the inner-scan call sites in generated code
   touches the codegen string-rule lowering and the `parse-that-regex` string
   matchers. This is a Lock-14-class change (codegen rebrand was the riskiest
   SK-V7 phase). Mitigation: it is *deletion-positive* — fewer call sites, not
   more — and byte-identical generated-output diffing gates each step.

5. **Not a micro-kernel — must not be waved as one.** The SK-V7 failure mode
   was 14 waves of micro-kernels that moved no production number. The fused
   index-tape is a *substrate change*; it must be planned as one structural
   wave with a falsifiable gate (string-heavy parse rows cross toward PASS),
   not decomposed into kernel admissions. If decomposed, it will fail the same
   way SK-V7 did.

## §6. Sources

External (primary):

- Langdale, G. & Lemire, D. "Parsing Gigabytes of JSON per Second." VLDB
  Journal 28(6), 2019. arXiv:1902.08318. §2 (branch-misprediction cost), §3
  (two-stage decomposition), §4.2 (carry-less prefix-XOR string-mask), §4.4
  (branch-free bit-to-index `compressstore`), §6 (per-stage IPC measurement).
- simdjson source architecture: `src/generic/stage1/` (structural indexer),
  `src/generic/stage2/` (tape builder), On Demand iterator
  (`include/simdjson/generic/ondemand/`) — forward-only, parse-once, stage-1
  index reused.
- simdjson On Demand 0.8.0 docs: `https://simdjson.org/api/0.8.0/md_doc_ondemand.html`
  — "values can only be parsed once"; lazy materialisation over stage-1 index.
- sonic-rs upstream README + skip-scan source — rejects two-stage tape;
  `skip_string`/`skip_number`/`get_from` ephemeral per-token SIMD scans;
  `LazyValue` lazy access.
- yyjson upstream README + `yyjson.c` read path — single-pass C89 scalar, no
  SIMD, no structural index, `\uXXXX` decode fused into string walk,
  ≤20 KiB i-cache discipline.
- asmjson docs.rs `https://docs.rs/asmjson/` — AVX-512BW + SWAR routing,
  10.93 GiB/s permissive single-thread DOM (Zen 4); 9-state DPDA model.
- RFC 8259 — strict JSON: four whitespace bytes; strings escape `"`, `\`,
  C0 controls U+0000–U+001F.

In-tree (authoritative):

- `skinny/RESULTS.md` (post SK-V7 W10c) — 17 parse-G rows; per-row string-quote
  / number counts; tape materialisation sizes; hot-leaf provenance.
- `restart/skinny/tranches/sk-v7/research/skv7-A2-sota-strict-beat.md` §2–§3,
  §5–§6 — per-parser architecture taxonomy; what each parser avoids.
- `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md` §1, §5–§7
  — dav1d data-vs-code split; DPDA/CollapsedStage admissibility matrix;
  grammar-neutral primitive vocabulary.
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md` §3.3–§3.6 — string-scanner pair
  ≈75% self-time; twitter "win" is skip-work.
- `skinny/crates/runtime/src/tape/mod.rs`, `tape/assembler.rs` — `OffsetTape`
  substrate: `Vec<u32>` offsets + sparse flag sidecar + empty `PayloadArena`;
  `TapeBuilder::push_offset`.
- `skinny/crates/runtime/src/grammars/json/scan.rs` — `scan_structurals`: the
  existing branch-free NEON stage-1 structural indexer (prefix-XOR, TBL
  classify, `compact_mask`).
- `skinny/crates/runtime/src/grammars/json/generated.rs` (lines 6, 30, 95–98,
  147–151, 161–193, 409–625) — generated recursive-descent parser; inner-scan
  call sites `match_tiny_plain_string` / `match_string_at_quote`.
- `skinny/crates/runtime/src/grammars/json/parser.rs` (lines 5, 11, 18, 23) —
  `structural_capacity_for` uses `scan_structurals` for sizing only; the index
  is discarded.

End of SC-2.
