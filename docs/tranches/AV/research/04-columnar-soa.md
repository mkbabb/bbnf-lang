# Research 04 — Kind-Partitioned Columnar Struct-of-Arrays

*Verbatim deliverable from architecture research agent, April 2026.
Informs AV Phase 7 (columnar payload stage) and the hybrid tape +
columns framing.*

---

# Columnar Struct-of-Arrays Materialisation

## 1. Thesis (one sentence)

Replace the row-oriented `Vec<TapeRec>` tape with a **kind-partitioned columnar store** in which the record kind *is itself* the column selector and the record index is the *column index* — so a walker that touches only structure pays only structure cache lines, SIMD traversals over numeric leaves become packed-slice kernels, and the per-record payload pointer (`payload_idx`, `child_off-as-offset`) disappears entirely.

## 2. Column schema

Shared backbone (every grammar, every record):

| Column | Type | Width/rec | Role |
|---|---|---|---|
| `kinds` | `Vec<u8>` | 1 B | `TapeKind` discriminant; kind also selects payload column |
| `span_lo` | `Vec<u32>` | 4 B | Source start offset |
| `span_hi` | `Vec<u32>` | 4 B | Source end offset (= span_lo for leaves with implicit width) |
| `sib_skip` | `Vec<u32>` | 4 B | Distance to the next sibling (replaces `child_off` — see §3) |
| `flags` | `Vec<u8>` | 1 B | `variant_idx` (6 bits) + `has_children` (1) + meta_bit (1) |
| `meta_lo4` | Packed nibble array `Vec<u8>` | 0.5 B | Low 4 bits of `meta_idx`; upper bit in `flags[7]` |

Total structural footprint: **14.5 B/record** — *less* than the 16 B TapeRec, and in six independently-streamed buffers.

Typed payload columns (allocated lazily, zero cost when empty):

| Column | Width | Filled by kinds |
|---|---|---|
| `pay_f64` | 8 B | F64 scalar leaves (JSON numbers, CSS dimensions, Sheets numerics) |
| `pay_u32` | 4 B | Kind discriminators, unit enums, hex colors |
| `pay_u64` | 8 B | Packed `Span` payloads, timestamps |
| `pay_u8` | 1 B | Bool / small enums (CSS color space, Sheets error codes) |
| `pay_agg16` | 16 B | Fixed-width aggregates (CSS `color-function`, `length-percentage`) |
| `str_off` | 4 B | Offset into a dedup'd `string_arena: Vec<u8>` for cooked strings |

A record `i` with `kinds[i] == K` consults column `col_of[K]`. Records of the same kind form a **dense contiguous sequence within their payload column** (indexed via a small per-record `pay_idx: u32` stored only in the records of kinds that need one; see §3).

Per-grammar specialisations — fixed-size overlays, never a column-count explosion:

- **JSON**: `pay_f64` is the hot column (~50% of records in canada.json). `str_off` catches strings. `pay_agg16` is unused — schema compresses to 5 payload columns.
- **CSS**: `pay_agg16` carries color/dimension tuples planned by the existing `PayloadLayout` pass. A dedicated `sel_col: Vec<SelectorRef>` specialisation when selectors form > 15 % of records (decided at grammar-mining time, not runtime).
- **Sheets**: `pay_f64` + a dedicated `pay_cellref: Vec<u32>` for `A1` / `R1C1` encoded cell refs. Formula leaves reuse `pay_agg16`.
- **BBNF (self-host)**: mostly `Span` leaves — no payload column active; it runs cheapest.

The column set is closed at **6 structural + 6 payload = 12 columns**. Grammar-specific overlays extend by declaration from the IR's `TypeDesc` universe; the emitter refuses to add a 13th column without a registered overlay, preventing schema sprawl.

## 3. Construction algorithm

Two innovations over the tape builder's `mark_children` / `push_compound` dance:

**(a) Sibling-skip pointer replaces first-child pointer.** The tape stores pre-order; siblings are contiguous. Instead of recording `child_off` on the parent and working out subtree bounds from the *next* compound's `child_off`, each record stores `sib_skip`: the distance to its own next sibling (or `0` if last among siblings). This is writable at the moment the record closes — no back-patching — because we always know how many records we just emitted since `mark_children`. Critically, `sib_skip` is uniform for leaves and compounds, killing the leaf/compound asymmetry in `TapeCursor::children`.

**(b) Column-local pay_idx is elided for kinds with a fixed mapping.** For any kind whose payload column has a one-to-one ratio with occurrences of that kind (`F64` leaves, `U8` leaves, etc.), the *n-th occurrence of that kind* writes to the *n-th slot of its column*. The walker maintains a per-kind running counter during traversal — but since walkers iterate in record order, the counter is a single u32 bumped per matching kind. No `pay_idx` stored; the relationship is positional.

For kinds whose column has variable per-occurrence width (aggregates with different widths, strings with variable lengths), a separate `pay_idx: Vec<u32>` overlay column is materialised *only for records of those kinds*, held in a parallel sparse `HashMap<TapeKind, Vec<u32>>` or (better) interleaved into `pay_agg16`'s header.

Builder API:

```rust
pub struct ColumnBuilder { /* 6 structural Vecs + payload Vecs + string_arena */ }

impl ColumnBuilder {
    pub fn mark_run(&self) -> u32 { self.kinds.len() as u32 }

    pub fn push_leaf_span(&mut self, k: TapeKind, lo: u32, hi: u32, vi: u8, mi: u8);
    pub fn push_leaf_f64(&mut self, lo: u32, hi: u32, vi: u8, mi: u8, v: f64);
    pub fn push_leaf_agg16(&mut self, k: TapeKind, lo: u32, hi: u32, vi: u8, mi: u8, bytes: &[u8; 16]);
    pub fn close_compound(&mut self, run_start: u32, k: TapeKind, span_lo: u32, span_hi: u32, vi: u8, mi: u8) {
        // sib_skip patched for the compound itself when its parent closes.
        // Children already have sib_skip set because sibling count is known
        // at each child's close.
    }
}
```

Write pattern: each push touches 6 hot cache lines (the tail of each structural column) plus exactly one payload column. Because columns are `Vec<T>` with amortised-`O(1)` tails, every push is 6 aligned stores + 1 typed store, fully pipelined — no `repr(C)` 16-byte struct write that straddles scalar types.

## 4. Walker API — `.view()`

Every `ViewRef` becomes:

```rust
pub struct ViewRef<'t> {
    cols: &'t Columns,
    idx: u32,
}
```

Accessors compile to one indexed load each:

```rust
impl<'t> ViewRef<'t> {
    #[inline] fn kind(&self)   -> TapeKind { unsafe { *self.cols.kinds.get_unchecked(self.idx as usize) } }
    #[inline] fn span(&self)   -> (u32,u32) { (cols.span_lo[i], cols.span_hi[i]) }
    #[inline] fn sibling(&self) -> Option<Self> { let s = cols.sib_skip[i]; (s>0).then(|| self.at(self.idx+s)) }
    #[inline] fn first_child(&self) -> Option<Self> {
        (cols.flags[i] & 0x40 != 0).then(|| self.at(self.idx + 1))  // first child is ALWAYS idx+1
    }
}
```

First-child at `idx+1` is a mechanical consequence of pre-order layout — *no pointer read at all* for child descent. That is the single biggest latency win over the tape.

Typed accessors (generated per rule):

```rust
impl<'t> JsonNumber<'t> {
    #[inline] pub fn value(&self) -> f64 {
        // f64 leaves use positional mapping: the k-th F64 record is pay_f64[k].
        // But that requires a running counter — instead, at parse time we
        // write a small monotonic cursor for the specific rule's kind:
        //     kind_cursor[F64] bumped per push, stored in a side table only
        //     when the grammar has any F64→value view methods.
        // For canada.json we cache this once: 111.1M f64 leaves → pay_f64[k].
        unsafe { *self.cols.pay_f64.get_unchecked(self.f64_rank as usize) }
    }
}
```

When rank-tracking cost is unacceptable, the emitter falls back to a compact `f64_rank: Vec<u32>` that is *only written for records carrying an f64 payload*. With JSON this column has ~N_f64 entries — still half the storage of a naive `payload_idx: Vec<u32>`.

## 5. Worked example — canada.json, sum all numeric values

canada.json: ~11 M records, ~6 M of them F64 leaves (lat/lon pairs).

**Tape version** (current):

```rust
let mut sum = 0.0;
for rec in tape.iter() {
    if rec.kind() == TapeKind::Span && rec.payload_idx != 0 {
        sum += tape.payload_f64(rec).unwrap();
    }
}
```

Per iteration: load 16 B TapeRec, branch on kind, load 8 B from disjoint `payloads: Vec<u8>` indexed by `child_off`. Two cache lines *per record*; the payload fetch is a gather.

**Column version**:

```rust
let sum: f64 = cols.pay_f64.iter().sum();   // done.
```

`pay_f64` is a dense packed `Vec<f64>`; LLVM auto-vectorises this to AVX2 (`vaddpd`) or NEON. 6 M f64 at 8 B each = 48 MB of contiguous memory; streaming read maxes out memory bandwidth (~30 GB/s on M3 = 1.6 ms). The tape version must read 11 M × 16 B = 176 MB of records *and* 48 MB of payloads while branching on every record — easily 10× slower.

This is the dominating workload the thesis is built to win on: *any traversal whose filter collapses to "records of kind K" is O(pay_K.len()), not O(tape.len())*. That is mechanically impossible with row-oriented records.

## 6. Invariant satisfaction

**Typed AST preservation.** Every `->` still produces a `TypeDesc`; emitter selects a payload column from a fixed dispatch table (`TypeDesc::F64 → pay_f64`, `TypeDesc::Named(s) → pay_agg16` when layout fits, etc.). Views reconstruct typed tuples by reading the correct column. No type is silently dropped — the emitter fails compilation if a `TypeDesc` has no column route.

**sonic-rs parity.** Sonic's win is SIMD scan + lazy materialisation. Columnar *structurally* lazy — payload columns are independent of structural scan — and SIMD-native over `pay_f64`. `.view().as_f64()` compiles to a single packed load.

**lightningcss parity.** CSS rich types fit `pay_agg16` (color functions, dimensions) exactly as today's 16-byte aggregate slot. Selector objects go to the `sel_col` overlay for the CSS-specialised column set. Rich AST walking is one-indexed-load-per-field, matching lightningcss's `Box<Dyn>` arena access but with better cache density.

**Single architecture across grammars.** Column set is grammar-parameterised but shape-identical; the emitter picks which payload columns to allocate from the fixed 6-slot vocabulary based on the IR's type set — same code path, different subset active.

## 7. Hybrid with tape

A genuine hybrid: emit the **structural columns always**, and optionally materialise `pay_agg16` as a row-record carrying `(kind_meta, flags, span_lo, span_hi, child_off, payload_idx)` — i.e., the legacy 16 B TapeRec. Then "tape" is just one specific payload column layout, and the walker chooses per kind. Grammars dominated by diverse aggregates (BBNF itself) keep row-density; grammars dominated by scalars (JSON, Sheets) get columnar density. One unified emitter, one unified walker, the choice is a per-column switch not a global architecture split.

## 8. Risks and prototype needs

1. **Random walkers (pretty-printer) pay gather cost.** A full pretty walk touches every column. Prototype: measure `fused_prettify` on canada.json; target parity, not regression. Mitigation: pack `span_lo`/`span_hi`/`sib_skip` into one `Vec<(u32,u32,u32)>` when the access pattern is consistently bundled.

2. **Rank-tracking during typed accessors.** `JsonNumber.value()` needs to know its index in `pay_f64`. Either (a) eager `f64_rank` column (cheap for f64-heavy grammars), or (b) recompute on demand via `pay_f64` cursor. Prototype: bench both on canada.json + CSS bootstrap; pick per-grammar at IR time.

3. **`sib_skip` patching cost on compounds with deep right spines.** If every Seq closes with a sib_skip patch to its last child, that's extra writes. Validate: microbench against `mark_children`/`push_compound` on CSS stylesheet (deep Seq spines).

4. **Column-count discipline.** The 12-column ceiling *must* be enforced by the emitter (compile error on overflow). Prototype: add a static assert over `TypeDesc → column_id` in the IR pass so grammar authors see the limit at grammar compile time, not at runtime.

What I'd build first: a spike that replays the existing `tape_basic.rs` test fixture against a columnar builder and validates record-by-record parity on the structural columns. Then a single-column `pay_f64` specialisation, bench sum-all-numerics on canada.json — this one benchmark will either confirm the 10× thesis or reveal that payload_idx rank-tracking is secretly expensive. Only after that prototype clears do we propagate to CSS aggregates.
