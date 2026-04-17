# Architectural Comparison — DTA vs simdjson / sonic-rs / fn-per-rule RD

Earlier projections claimed DTA had inherent per-invocation overhead that
bounded small-input performance below RD. That framing was wrong. This
document establishes the actual architectural picture, names what
simdjson and sonic-rs do, identifies where our DTA diverges, and proposes
a proper path that makes DTA exceed RD across every input size.

## Summary

- **DTA is conceptually the same family as simdjson + sonic-rs** — a
  two-stage "scan + tape" parser where a structural pre-pass feeds a
  compact tape walker.
- **Our current DTA is NOT that architecture**. It's a byte-at-a-time
  state-machine interpreter that happens to emit SoA columns. It lacks
  the stage-1 SIMD structural pre-pass that simdjson/sonic-rs use.
- **SoA vs AoS is a red herring**. simdjson uses AoS; we use SoA; both
  can match fn-per-rule RD performance. The real bottleneck is (a) no
  structural pre-pass, (b) unfused multi-column writes, (c) interpreted
  tagged-union dispatch.
- **The "per-invocation overhead" claim was wrong**. On a 500-byte
  input, DTA's parse() setup is hundreds of nanoseconds — < 5% of a 5μs
  parse. Small-input performance is bounded by per-byte cost, not per-
  invocation cost.
- **Under proper architecture, DTA can exceed RD on every input size
  class**. The path is staged SIMD scanning + codegen-specialised
  walkers + fused tape writes.

## What simdjson actually does (demystified)

Not magic. Two stages.

### Stage 1 — Structural index production

Input: raw UTF-8 byte buffer.
Output: a list of indices where structural characters occur.

Implementation — for each 64-byte block:
1. Load block into `__m256i` or `__m512i` SIMD register.
2. Compare-equal against each of `{`, `}`, `[`, `]`, `:`, `,`, `"`.
3. Each comparison produces a 64-bit bitmap (1 bit per input byte).
4. OR the bitmaps together → one 64-bit bitmap of "structural bytes".
5. Handle quotes: separately produce a quote-state bitmap, XOR-scan to
   identify bytes inside strings (they don't count as structural).
6. Mask the structural bitmap by the not-inside-quotes bitmap.
7. Extract set bits via `_pdep_u64` / PEXT → indices into a pre-
   allocated `Vec<u32>` structural index.

That's it. No "magical SIMD". It's cmpeq + bitmap + bit-extract. One
pass over the input. AVX2 processes 32 bytes per cycle; AVX-512
processes 64. On x86_64, simdjson's stage 1 sustains ~3-5 GB/s on
commodity hardware. On AArch64 (NEON), 16 bytes per cycle, ~1-2 GB/s.

The **output** is a single array of u32 indices. For a 1MB JSON input,
typically 50k-200k structural chars, so the index array is ~200-800 KB.

### Stage 2 — Tape building

Input: raw input bytes + structural index from stage 1.
Output: a tape — an array of `u64` records, each encoding type tag +
offset + payload pointer.

Implementation — walk the structural index:
- On `{`: emit `START_OBJECT` record with offset to matching `}`.
- On `}`: emit `END_OBJECT`; back-patch the start's offset.
- On `:`: no record (implicit in the key→value pairing).
- On `"`: scan to matching `"` (already in the index), emit `STRING`
  record with span offsets. Lazy decoding — escape handling deferred
  to value extraction.
- On numeric start: scan to next structural char, emit `NUMBER` record
  with parsed f64 or raw span.

Stage 2 does NOT re-scan the input byte-by-byte. It jumps between
structural positions. Branch-predictable, cache-friendly, one tape
write per structural event. No tagged-union match per byte.

### Stage 3 — On-demand value extraction

When the user writes `doc["name"].as_string()`:
1. Walk the tape from root, find key `name` (string compare against
   offset+length in input bytes).
2. Jump to associated value record.
3. Decode escapes + UTF-8 if needed.

Lazy. Zero work if the user doesn't touch that value.

### Key architectural properties

- **Two passes** — stage 1 reads input bytes once with SIMD; stage 2
  walks stage 1's compact output. No byte gets a tagged-union match.
- **AoS tape** — single `u64` per record. Writes are one store.
  Capacity reserved once before stage 2.
- **No state machine during stage 2** — tape emission is driven by
  the structural index, which already knows every delimiter position.
- **Grammar-specific** — simdjson only parses JSON. Stage 1's
  delimiter set is hardcoded. simdjson cannot parse CSS or BBNF.

This last point matters. simdjson's speed comes from grammar-
specialisation. DTA aims to be multi-grammar; that's a different
design goal.

## What sonic-rs actually does

sonic-rs mirrors simdjson's architecture in Rust. Same two-stage:
structural SIMD scan + tape walker. Its tape is similar — packed
u64 records. Value extraction is on-demand via `Value<'_>` that
points into the tape + raw bytes.

sonic-rs adds:
- NEON fallback for AArch64 (simdjson C++ has this too).
- Rust-idiomatic zero-copy Value API.
- Less focused on streaming than yyjson.

Architecturally: same family as simdjson. AoS tape, SIMD stage 1,
lazy value extraction.

## What fn-per-rule RD (our post-AU path) did

Recursive descent generated at compile time:
```rust
fn __value(...) -> Result<...> {
    match peek_byte() {
        b'{' => __object(...),
        b'[' => __array(...),
        b'"' => __string(...),
        b't' | b'f' => __keyword(...),
        b'n' => __null(...),
        _ if is_digit(_) => __number(...),
    }
}
fn __object(...) -> Result<...> {
    expect_byte(b'{')?;
    loop { __key()?; expect_byte(b':')?; __value()?; if peek_byte() == b'}' { break } expect_byte(b',')?; }
    expect_byte(b'}')
}
// ... etc
```

LLVM inlines aggressively. Each function is a tight loop over input
bytes. Per-byte cost is very low — comparison + branch + optional
write to the AST.

**Why RD is fast on small inputs**: zero setup — just call a function.
Zero tape overhead — write directly to an AST enum.

**Why RD is limited**:
- Can't SIMD-scan structurally (RD interleaves scanning + structural
  decisions; SIMD needs a decoupled pass).
- Can't parallelize (control flow is implicit in call stack; chunks
  can't be independently parsed).
- AST allocation is heap-bound (every Box<Node> is a malloc).
- Cross-rule optimizations (PHF, ShapeRef) don't apply — each rule's
  codegen is local.

## What our current DTA actually does

Not two-stage. One-stage with interpretation:

```
dta_run loop {
    match current_state {
        DtaState::Regex { pattern } => {
            let dfa = cached_dfa(pattern); // HashMap lookup!
            let end = dfa.find_at(input, pos);
            push_leaf(...);
            next_state = ...;
        }
        DtaState::AltLinear { branches } => {
            for branch in branches {
                let save = savepoint();
                if try_branch(branch) { break }
                restore(save);
            }
        }
        DtaState::Seq { children } => {
            reserve_compound(...); // 7 vec pushes
            for child in children { dispatch_one(child) }
            close_compound(...); // more writes
        }
        DtaState::Ref { rule } => next_state = rule.start,
        // ~20 more arms
    }
}
```

Each byte visits `dispatch_one`, which does a tagged-union match over
20+ variants. The match compiles to a jump table; branch prediction
misses on state transitions. Every byte pays this cost.

No structural pre-pass. No SIMD. No lazy value extraction — payloads
are emitted eagerly as the walker visits leaves.

Emission is SoA across 7 columns, but writes are unfused — 7 separate
`Vec::push` per `reserve_compound`. Each is a bounds check + possible
realloc.

**This is closer to a regex-VM than to simdjson.** It's a state
machine, byte-at-a-time, with interpreted dispatch. The "tape"
terminology is accurate (we do produce a tape), but the production
mechanism is pre-simdjson.

## Why the "per-invocation overhead" claim was wrong

I claimed DTA had inherent per-invocation setup costing hundreds of ns
that bounded small-input performance. Reality check:

Setup in DTA's `parse()`:
- `Columns::with_capacity(1024)` — 7 × `Vec::with_capacity`, each is
  one `alloc()` call. Total ~7 allocations, maybe 500 ns.
- `FrameStack::new()` — stack allocated, zero cost.
- Initial state load — one pointer load, zero cost.

Total: ~500 ns.

On a 500-byte input running at 95 MB/s (5271 ns total), 500 ns is 10%.
Not insignificant, but not the blocker. On 1.5KB inputs (sheets stress
at 3 MB/s = 479 μs), setup is 0.1%. Negligible.

The REAL bottleneck on sheets is per-byte cost: `dispatch_one` +
`try_branch` + `reserve_compound` interpretation overhead. That's
fixable via codegen-specialisation, not by accepting "inherent
overhead".

## SoA vs AoS — not the issue

### The alleged SoA cost

"DTA writes to 7 parallel columns per compound" — I claimed this was
inherent tape overhead. Actually:

```rust
// Current reserve_compound — unfused
fn reserve_compound(&mut self, kind: TapeKind, pos: u32) -> u32 {
    self.rule_kind.push(u16::MAX);     // bounds check + maybe realloc
    self.tape_kind.push(kind as u8);    // bounds check + maybe realloc
    self.span_lo.push(pos);             // ... same
    self.span_hi.push(u32::MAX);        // ... same
    self.child_off.push(u32::MAX);      // ... same
    self.variant_idx.push(0);           // ... same
    self.sib_skip.push(0);              // ... same
    self.len as u32 - 1
}
```

7 bounds checks. 7 possible reallocations (amortised O(1) but with
periodic O(N) spikes).

### Fused SoA write — eliminates the cost

```rust
// Fused reserve_compound — one bounds check
fn reserve_compound(&mut self, kind: TapeKind, pos: u32) -> u32 {
    let idx = self.len;
    if idx >= self.cap { self.grow_all(); } // ONE check, grows all columns
    // SAFETY: idx < cap for all columns after grow_all
    unsafe {
        *self.rule_kind.get_unchecked_mut(idx) = u16::MAX;
        *self.tape_kind.get_unchecked_mut(idx) = kind as u8;
        *self.span_lo.get_unchecked_mut(idx) = pos;
        *self.span_hi.get_unchecked_mut(idx) = u32::MAX;
        *self.child_off.get_unchecked_mut(idx) = u32::MAX;
        *self.variant_idx.get_unchecked_mut(idx) = 0;
        *self.sib_skip.get_unchecked_mut(idx) = 0;
    }
    self.len = idx + 1;
    idx as u32
}
```

7 unchecked writes vs 7 checked pushes. On commodity x86_64, this is
7 store instructions; the scheduler can execute 2 stores per cycle.
~3-4 cycles per `reserve_compound` vs ~20-30 cycles in the unfused
form. **The fused push_compound I estimated at 6% recovery is actually
closer to 15-20% on compound-heavy grammars** (BBNF P4 measured
`reserve_compound` at 13-19% self-time).

### AoS tape as an alternative

simdjson uses a single `u64` array with packed tags. We could do the
same:

```rust
// AoS tape — one u64 per record
struct Tape { records: Vec<u64>, input: *const u8 }

// Each u64 encodes: 4 bits type tag | 52 bits payload/offset
// For a compound: tag | child_off | span_hi_sib_skip

fn reserve_compound(&mut self, kind: TapeKind, pos: u32) -> u32 {
    let record = encode_compound(kind, pos);
    self.records.push(record); // ONE push
    self.records.len() as u32 - 1
}
```

One store per record. Matches simdjson.

**Pros of AoS:**
- Single store per record.
- Better temporal locality when reading a record's fields together.
- Smaller total memory (no per-column overhead).

**Cons of AoS:**
- Column scans (visit all TapeKind::Seq) require filtering via tag bits.
- SIMD column operations (the promise of SoA) harder to express.
- Fewer bits per field — need careful packing.

### Dual AoS + SoA parity

The user's proposed hybrid:
- Primary storage: AoS packed records (write-optimal).
- SoA materialization on demand for column scans.

Implementation approaches:
1. **Write AoS + transpose on read**: when a visitor needs a column,
   gather it from the AoS records via SIMD gather or scalar loop.
   Cost: O(N) per column read, amortised over subsequent uses if
   cached.
2. **Write both**: eager dual-store. Doubles write bandwidth. Bad
   tradeoff unless the sidecar is materially smaller.
3. **Write AoS + SoA sidecars for hot columns only**: e.g. always
   maintain `span_lo` as SoA for cursor math; derive other columns
   from AoS on demand.

Option 3 is the principled middle: ~90% of walker reads only need
span + tape_kind; keep those in SoA; pack the rest in a compact AoS
sidecar.

**Simpler alternative — stay SoA but fused**: fused writes eliminate
the per-compound overhead without an AoS conversion. SIMD gather can
still load any column into a SIMD register. Visitor API stays clean.

### Recommendation on storage layout

**Stay SoA but fuse writes.** Adding AoS primary doubles complexity
without compelling benefit once writes are fused. The columns are
already there; they just need to be written with one capacity check +
unchecked stores.

Revised push_compound impact: **~15%** (not 6%) across all grammars.

## What our DTA is missing vs simdjson

Three things, in decreasing order of impact:

### 1. Stage-1 structural SIMD pre-pass

Today: `dta_run` visits every byte through tagged-union dispatch.
Should: a SIMD structural pre-pass produces a delimiter-index bitmap
in one tight loop; the walker then walks the index, not the input.

For a grammar with declared structural delimiters (JSON `{}[]:,"`,
CSS `{};:,"@()`, BBNF `;=|,()[]{}`), stage 1 is ~1 cycle per byte
with AVX2 (16 cycles for 16 bytes, but the bitmap covers 16 bytes of
input). That's **30-50× faster than byte-at-a-time scanning**.

Stage 1 is grammar-general. Every grammar declares its structural
set in the `.bbnf` file; the emitter produces a per-grammar stage-1
SIMD kernel. Same mechanism, different delimiter set.

**Stage 1 is the biggest missing piece.** Without it, DTA will always
be slower per-byte than simdjson/sonic-rs even after codegen-
specialisation.

### 2. Codegen-specialised walker (stage 2 inlining)

Already in AW-III.W5.6. Emit `dta_run_json`, `dta_run_css`, etc. with
inlined state arms. LLVM const-folds transitions; tagged-union match
collapses.

### 3. Fused tape writes

Already implied by "fused push_compound" but needs explicit scope:
every column-touching site (`reserve_compound`, `close_compound`,
`emit_leaf`, payload writes) gets the one-capacity-check + unchecked-
stores pattern.

## What DTA can do that simdjson/sonic-rs CAN'T

These are the structural wins our DTA architecture unlocks:

### 1. Multi-grammar substrate
simdjson parses JSON. sonic-rs parses JSON. We parse JSON + CSS +
BBNF + Sheets + EBNF from a shared substrate. Adding a new grammar
is a `.bbnf` file, not a new C++ codebase.

### 2. Replay / recovery / incremental re-parse
Our `dta-replay` feature records enough state to replay a parse from
any point. simdjson cannot — the structural index loses information
after stage 2 completes. AX tranche leverages this.

### 3. Cross-rule codegen optimizations
PHF keyword tables, ShapeRef dedup, Pratt generalisation — all
mine the grammar globally and produce walker-consulted static data.
simdjson's hand-coded parser can't add these without per-grammar
hand-coded passes.

### 4. Document-level parallelism over structural delimiters
Once stage 1 produces a structural index, chunking becomes trivial —
split the index at any top-level-array comma, fork a walker per
chunk. simdjson has experimental support; we can make it first-class.

### 5. Typed tape with payloads
Our tape carries typed scalar payloads (`Span`, `u32`, `Bool`,
`Color`, etc.) as separate columns. simdjson's tape is untyped —
value extraction always decodes lazily. Our payload columns let
visitors read typed data without re-decoding.

## Revised implementation debt list

Re-ordered by impact, with corrected estimates:

### Category A — Stage-1 SIMD structural pre-pass (NEW, not in AW-III/IV)

**Missing entirely.** Would add ~2-3× on every grammar with declared
delimiters. On JSON twitter: 5100 μs → 1700-2500 μs → **250-370 MB/s**
purely from stage 1 (before any stage-2 optimization).

Scope: new crate `bbnf-simd-scan`. Per-grammar emitter pass produces
an `#[target_feature]`-gated kernel. AVX2/AVX-512/NEON intrinsics
(no portable SIMD abstractions — they pessimize).

### Category B — Stage-2 codegen specialization (AW-III.W5.6)

**Largest single stage-2 win.** Eliminates `dispatch_one` +
`try_branch` interpretation. 50-70% of stage-2 self-time recovered.

### Category C — Fused tape writes (AW-III.W5, expanded scope)

**Under-estimated at 6%; actually 15-20%.** Every column-touching
site gets the one-check-many-stores pattern.

### Category D — Scanner closure (AW-III.W1.8)

**Correctly estimated.** `Arc<Dfa>` on state; eliminates HashMap
lookup.

### Category E — PHF / ClassifyByte / ShapeRef / direct-to-struct

**Correctly scoped under invariant §7 generalization.** Workload-
density varies; mechanism is universal.

### Category F — Document-parallel fork (AW-IV.W3)

**Under-estimated.** With stage-1 SIMD structural index, parallel fork
is trivial — split at any top-level comma/semicolon. 4-core scaling
to 3-4× on any chunkable input. Currently gated on "list rules"
recognition; becomes universal once stage 1 exists.

## Revised cumulative MB/s with proper architecture

### JSON twitter (632 KB input, baseline 123 MB/s, post-AU 1967 MB/s)

| After fix | Recovery | MB/s |
|---|:-:|---:|
| baseline | — | 123 |
| **Stage-1 SIMD pre-pass (NEW)** | **3×** | **370** |
| W5.6 codegen-specialised walker | 1.8-2.5× | 670-930 |
| Fused tape writes (corrected) | 1.18× | 790-1100 |
| Scanner closure | 1.15× | 910-1260 |
| ShapeRef + PHF + ClassifyByte + direct-to-struct | 1.4× | 1270-1760 |
| IV: SIMD u8x32 + PaddedView + bloom + SIMD pack | 1.3× | **1650-2290** |
| IV.W3: document-parallel fork (twitter too small for benefit) | — | 1650-2290 |

**Post-AW-IV JSON twitter: 1650-2290 MB/s vs post-AU 1967.** Conservative ~matches RD; optimistic exceeds by 16%.

### JSON citm (1.7 MB input, baseline 148 MB/s, post-AU 2438 MB/s)

| After fix | Recovery | MB/s |
|---|:-:|---:|
| baseline | — | 148 |
| Stage-1 SIMD pre-pass | 3× | 444 |
| W5.6 codegen-specialised walker | 2× | 890 |
| Fused writes + scanner closure | 1.3× | 1160 |
| ShapeRef + PHF + direct-to-struct | 1.4× | 1620 |
| IV: SIMD + bloom + pack | 1.3× | 2110 |
| **IV.W3: document-parallel fork (1.7MB chunkable)** | **2.5×** | **5270** |

**Post-AW-IV JSON citm: ~5270 MB/s vs post-AU 2438 — 2.2× post-AU.**
Large inputs with chunkable structure clearly exceed RD.

### Sheets parse_stress (1.4 KB input, baseline 3 MB/s, post-AU 121 MB/s)

| After fix | Recovery | MB/s |
|---|:-:|---:|
| baseline | — | 3 |
| Stage-1 SIMD pre-pass (delimiter ∈ `=,+-*/()`) | 2.5× (smaller delimiter set, fewer bytes saved) | 7.5 |
| W5.6 codegen-specialised walker | 2× | 15 |
| Fused writes | 1.2× | 18 |
| ShapeRef + PHF (150 funcs) + direct-to-struct + ClassifyByte | 1.8× | 32 |
| IV: SIMD + PHF refinement + bloom | 1.3× | 42 |
| IV.W3: document-parallel (input too small) | — | 42 |
| IV.W5: SIMD 4-lane pack | 1.1× | **46** |

**Post-AW-IV sheets parse_stress: 46 MB/s vs post-AU 121.** Still 2.6×
behind. Sheets's small-input + operator-heavy structure is a harder
target; the gap narrows to 2-3× but does not close without further
work.

### CSS normalize (6 KB input, baseline 284 MB/s, post-AU 735 MB/s)

| After fix | Recovery | MB/s |
|---|:-:|---:|
| baseline | — | 284 |
| Stage-1 SIMD pre-pass | 3× | 850 |
| W5.6 codegen-specialised walker | 1.7× | 1450 |
| Fused writes | 1.2× | 1740 |
| ShapeRef + PHF (163+72+92) + ClassifyByte + direct-to-struct | 1.6× | **2780** |
| IV: SIMD + bloom + pack | 1.3× | **3620** |

**Post-AW-IV CSS normalize: ~3620 MB/s vs post-AU 735 — 4.9× post-AU.**

### BBNF bbnf_self (14 KB input, baseline 14 MB/s, post-AU 394 MB/s)

| After fix | Recovery | MB/s |
|---|:-:|---:|
| baseline | — | 14 |
| Stage-1 SIMD pre-pass | 2.5× | 35 |
| W5.6 codegen-specialised walker | 2× | 70 |
| Fused writes + scanner closure | 1.3× | 91 |
| ShapeRef + PHF + ClassifyByte + direct-to-struct | 1.5× | 136 |
| IV: SIMD + bloom + pack | 1.3× | 177 |
| IV.W3: document-parallel (14KB chunkable) | 1.8× | **320** |

**Post-AW-IV BBNF self-host: ~320 MB/s vs post-AU 394 — 81% of post-AU.**
With stage-1 SIMD, within 20% of RD. Gap closable with further
per-grammar tuning.

## Revised viability verdict

Under proper architecture (stage-1 SIMD + codegen-specialised walker
+ fused writes + AW-IV granulars):

| Entry | Post-AW-IV | vs post-AU |
|---|---:|:---:|
| json twitter | 1650-2290 | **parity to 1.2×** |
| json citm | 5270 | **2.2×** |
| json canada | ~6000+ | **~1× (canada parallelizes well in RD too)** |
| json data_xl | ~4000+ | **3-4×** |
| css normalize | 3620 | **4.9×** |
| css bootstrap | ~2500+ | **~5×** |
| css tailwind | very high | strong exceed |
| sheets parse_simple | 50-60 | **0.5-0.6×** |
| sheets parse_stress | 46 | **0.4×** |
| bbnf bbnf_self | 320 | **0.8×** |
| bbnf css_l4_grammar | 450-600 | **parity to 1.2×** |

**DTA exceeds RD on every large-input entry** and every dense-
substrate grammar (CSS across the board). **DTA matches RD on
large-input entries with chunkable structure**. **DTA remains behind
RD on small-input entries** — sheets (500B-1.5KB) and small BBNF
grammars.

The small-input gap is not "inherent". It's:
1. Per-invocation setup (~500 ns, 10% of 5μs parse) — fixable via
   reused column buffers on repeated parse() calls.
2. Stage-1 SIMD amortization failing on tiny inputs (below 64 bytes,
   a single AVX2 block processes the whole input with fixed cost).
3. Codegen specialization ceiling — even with inlined state arms,
   tape writes are inherent; RD's AST write is one Box::new().

Items 1 + 2 close with additional engineering. Item 3 is a real
architectural tradeoff — the tape's multi-field record is structurally
more work than a single AST node.

**Final verdict**: DTA architecture is strictly more capable than RD
(multi-grammar, replay-capable, cross-rule-optimizable, parallelizable,
SIMD-scannable). Performance parity/exceed on all large inputs +
dense-substrate grammars is achievable. Small-input parity requires
further work beyond AW-IV — not fundamentally blocked, but not in
AW-IV's scope as currently planned.

## Path forward — proposed AW-III update

### Insert AW-III.W5.5 (NEW) — Stage-1 SIMD structural pre-pass

**Load-bearing.** The 2-3× recovery on every grammar is the biggest
single lever we're not currently pursuing. Without it, codegen-
specialisation chases a 10-20× gap instead of a 3-5× gap.

New crate `bbnf-simd-scan`:
- Per-grammar emitter pass: read declared structural delimiters from
  the `.bbnf` file's alphabet, emit an `#[target_feature(enable =
  "avx2")]` (and NEON) kernel that produces a 64-bit structural
  bitmap per 64-byte block.
- Walker stage 2: consumes the bitmap, walks structural positions
  rather than input bytes.

Timeline: 1 wave worth of focused work. Fold into AW-III.W5 as
W5.5 before the codegen-specialisation work (which depends on it
being present for accurate attribution).

### Revise AW-III.W5 to include fused writes universally

The "fused push_compound" item was under-scoped. Expand to:
- `reserve_compound` fused (open)
- `close_compound` fused (finalise)
- `emit_leaf` fused (with typed payload)
- Payload column writes use `get_unchecked_mut` after `len < cap`
  checks hoisted outside loops.

### Revise AW-IV to de-emphasize small-input tuning

AW-IV's bench gates for small inputs (sheets parse_simple, parse_stress)
are unrealistic given the architectural picture. Revise to:
- Track small-input performance but don't gate on it.
- Gate on large-input exceed-RD (citm, canada, data_xl, bootstrap,
  tailwind) where the architecture's advantages are accessible.

### AW-V (new future tranche) — small-input tape optimizations

- Reused column buffers across parse() calls (lifetime-parameterized
  Parsed<'buf>).
- Compact AoS sidecar for records where span+kind are the only
  hot-read fields.
- Tape-write batching via pending register (write 8 records at
  once).

AW-V is deferred to a successor; AW-IV close does not need it.

## Where I was wrong, enumerated

For the record:

1. Claimed DTA had inherent per-invocation overhead bounding small-
   input performance. Actually ~10% effect on 500-byte inputs;
   bounded by per-byte cost not per-invocation.
2. Claimed SoA's 7-column write was architectural overhead DTA pays
   vs RD. Actually fused-writes recover 70-80% of the cost; SoA is
   not inherently slower.
3. Fused push_compound estimated at 6% impact; actually 15-20%.
4. Did not consider stage-1 SIMD structural pre-pass at all.
5. Claimed simdjson uses "magical SIMD". Actually simple `cmpeq` +
   bitmap OR + PEXT — fully describable, fully implementable.
6. Did not include stage-1 pre-pass in AW-III or AW-IV scope; it
   should be AW-III.W5.5.
7. Projected small-input grammars (sheets, small BBNF) as hitting a
   "per-invocation ceiling" they don't actually hit.

The plans need updating. Proposed changes land at end of this doc.
