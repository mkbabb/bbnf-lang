# Skinny Spec — Substrate Slice

## 0. Scope and stance

The skinny exists to validate the V1 SOTA-viability claim against ONE grammar end-to-end before tranches A-J commit. This document specifies the substrate slice — the runtime layer that codegen lowers to and the bench measures against.

The substrate is **grammar-neutral**: every type, layout decision, and arena policy here is the same shape the full V1 will ship for CSS, BBNF, Sheets, and the rest. The skinny exercises only the JSON cell of that shape. If JSON cannot reach SOTA-parity through this substrate, that is strong negative evidence for JSON-class tape/SIMD throughput; if JSON does reach parity, the substrate-side risk for JSON-class grammars collapses. CSS layout, Sheets Pratt/host chains, and other non-JSON mechanisms still close under their V1 gates.

Source authority (verbatim citations, not paraphrase):

- `restart/ARCHITECTURE.md` §9 (lines 1373-1426) — tape and direct-to-struct union; tape invariants; per-grammar runtime template path.
- `restart/ARCHITECTURE.md` §3.1 (lines 191-244) — `Grammar` trait, `parse` / `parse_in` / `parse_owned` shape, the `'a` lifetime carry.
- `restart/audit/pass-3-runtime/PASS-3.md` §4 (lines 150-191) — illustrative `Tape<'input>` / `TapeToken` / `ValueRef<'doc, 'input, K>` shape; closure environment by `&'i Tape<'i>`.
- `restart/ARCHITECTURE.md` §7.2 BIR rows for `TapeEmit`, `DirectBuild`, `ValueProject`, `SimdScan`, `SpanMark` (lines 920-963) — what the substrate is the receiver of.
- `restart/audit/pass-2-codegen/PASS-2.md` §2 commitment 3 — TapeShape + ValueShape are one materialisation plan.
- `restart/locks/14-LOCKS.md` Lock 1 (line 34) — tape is THE substrate; no parallel substrate; no OpenFrame clone.
- `restart/locks/14-LOCKS.md` Lock 8 (line 48) — sonic-rs / simdjson / lightning-css anchors.
- `restart/ARCHITECTURE.md` §11 row `simd/structural_scan` (line 1519) — ≥7 GB/s AVX2, ≥5 GB/s NEON, scalar parity hash mandatory.

The full-V1 spec PASS-3 §4 admits its layout is **illustrative, not mandatory** ("This layout is not a PASS-1 mandate; it is a user-surface contract. PASS-1 may pack differently if these semantics remain true." — `PASS-3.md:187`). The skinny pins one packing and measures it. If the bench fails parity, the packing is the variable to perturb, not the contract.

---

## 1. `Tape`, `TapeToken`, `ValueRef`, `DocumentView` — concrete layouts

### 1.1 `TapeToken` — 16 bytes, hot-path packed

```rust
#[repr(C, align(16))]
pub struct TapeToken {
    /// Node kind id (grammar-agnostic; emitted into a per-grammar
    /// `NodeKind` enum at codegen time).
    pub kind: NodeKindId,        // u16 — 65 535 kinds is far above
                                 //       any realistic grammar
    pub flags: TokenFlags,       // u16 bitfield: PAYLOAD_CLASS (4 bits),
                                 //               HAS_SCALAR_CACHE (1),
                                 //               STRING_NEEDS_UNESCAPE (1),
                                 //               STRING_BORROWS_SOURCE (1),
                                 //               IS_STRUCTURAL_OPEN (1),
                                 //               IS_STRUCTURAL_CLOSE (1),
                                 //               RECOVERY_KIND (2),
                                 //               reserved (5)
    pub start: u32,              // source byte offset (input length
                                 //                     ≤ 2³² for V1 hot path)
    pub end: u32,                // source byte offset (exclusive)
    pub payload_or_skip: u32,    // discriminated by flags.PAYLOAD_CLASS:
                                 //   - INLINE_BOOL_NULL: ignored (zero)
                                 //   - INLINE_NUMBER_FAST: nothing
                                 //     (number recovered from start..end)
                                 //   - ARENA_OFFSET: byte offset into
                                 //     PayloadArena
                                 //   - SIBLING_SKIP for container nodes:
                                 //     count of token slots to skip to
                                 //     reach next sibling
}
```

Fields total: 2 + 2 + 4 + 4 + 4 = 16 bytes. `#[repr(C, align(16))]` so a 64-byte cache line holds exactly four tokens with no straddle on x86 / ARM. AVX2 may load eight tokens (128 bytes) with two aligned 32-byte loads.

`payload_or_skip` is the load-bearing union slot. PASS-3 §4 carries `payload: u32` and `sibling_skip: u32` as separate fields — but for the JSON skinny we collapse them, because:

- Scalar tokens (`JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`) need the payload pointer; they do not need a sibling-skip (they have no children).
- Container open tokens (`JsonObjectOpen`, `JsonArrayOpen`) need the sibling-skip to allow O(1) traversal past the subtree; they do not need a payload pointer (the close-token carries the span).
- `flags.PAYLOAD_CLASS` discriminates.

If the bench shows the union encoding is the bottleneck, splitting back to 24 bytes is a one-commit perturbation; carrying both eagerly costs a cache line per token-pair. The full V1 may refine this; the skinny commits to 16 bytes because the SOTA target lives or dies by token-cache density.

`NodeKindId` is `u16` (a transparent newtype) and is grammar-derived but the type itself lives in `runtime/src/tape/`. The mapping `JsonObjectOpen → 0u16`, `JsonObjectClose → 1u16`, etc., is generated per grammar; the substrate never sees the names.

### 1.2 `Tape<'input>` — owning token stream + payload arena

```rust
pub struct Tape<'input> {
    /// The input bytes the tape borrows from. JSON is byte-clean so
    /// `&[u8]` here even though the public Grammar API may take `&str`.
    source: &'input [u8],

    /// Tape token stream, append-only after commit. Box<[T]>, not Vec<T>,
    /// because the parser writes once and then the tape is read-only.
    /// Sealing as `Box<[_]>` removes the `len/cap` divergence and
    /// improves codegen for traversal.
    tokens: Box<[TapeToken]>,

    /// Payload arena; see §2.
    payloads: PayloadArena,

    /// Snapshot identity. Each parse produces one `TapeId`, monotonic
    /// per process. The skinny does not exercise reuse; identity is here
    /// to anchor the §5 invariant.
    id: TapeId,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct TapeId(pub u64);
```

JSON skinny does **not** carry `Box<[Diagnostic]>` on `Tape`; recovery is omitted (§7), so diagnostics for the SOTA hot path are zero-length. If the bench harness needs to surface a parse error, that is a `Result<Self::View<'a>, ParseError>` at the `Grammar::parse` boundary, not a tape field.

**Sealing trade-off (incremental deviation).** `tokens: Box<[TapeToken]>` produces tighter codegen for traversal and removes the `len/cap` divergence — measurable on the SOTA gate. It also precludes append-after-parse, which the V1 I tranche's incremental reuse map (`ReparsePlan` per ARCH §3.3) requires. The graduation path is not "rewrite the substrate" — it is a typed handoff: the skinny's `Tape<'input>` becomes the *committed-snapshot* projection of a V1 `TapeBuilder<'input>` whose internal storage is `Vec<TapeToken>` (or chunked); the snapshot view re-seals as `Box<[T]>` for the read path. Lens N classification: **MECHANICAL with named inversion** — the inversion is "skinny seals at parse boundary; V1 seals at snapshot boundary," and the read-side type-shape (`&Tape<'input>`, `ValueRef<_, _, K>`) does not change. Tracked in `INDEX.md` deviation ledger.

### 1.3 `ValueRef<'doc, 'input, K>` — typed cursor over tape

```rust
#[derive(Copy, Clone)]
pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind> {
    tape: &'doc Tape<'input>,
    /// Token-stream index. `u32` because tokens.len() ≤ 2³² in V1
    /// (matches `start`/`end` width).
    index: u32,
    _kind: PhantomData<fn() -> K>,
    _input: PhantomData<&'input [u8]>,
}
```

The lifetime parameters `'doc` and `'input` are **the discriminant** of the slice-borrow / arena / owned forms (Lock 9, `restart/locks/14-LOCKS.md:50`). For the skinny:

- `parse(&'a [u8]) -> Result<Self::View<'a>, _>` collapses to `'doc = 'input = 'a`.
- `parse_in(&'a [u8], &'a Arena) -> Result<Self::View<'a>, _>` keeps them collapsed; the `Arena` only widens the payload arena's backing storage.
- `parse_owned` lives behind an `OwnedDocument` wrapper that self-references. The skinny facade may expose `parse_owned` only as a cold wrapper over `parse`; the SOTA rows measure `parse(&str)` with UTF-8 prevalidation outside the timed region and do not treat owned form as implemented hot-path substrate.

`PhantomData<fn() -> K>` (rather than `PhantomData<K>`) keeps `ValueRef` `Copy`/`Send`/`Sync` regardless of `K`'s auto-trait posture. The kind tag never holds data.

Kind families for JSON:

```rust
pub enum AnyKind {}      // erased
pub enum JsonRoot {}
pub enum JsonValue {}
pub enum JsonObject {}
pub enum JsonArray {}
pub enum JsonString {}
pub enum JsonNumber {}
pub enum JsonBool {}
pub enum JsonNull {}
pub enum JsonMember {}   // a (key, value) pair token
```

These are all uninhabited; they are markers that constrain the methods exposed on `ValueRef<_, _, K>` and direct the codegen-emitted `JsonObject::get`, `JsonArray::iter`, `JsonString::as_str`, `JsonNumber::as_f64` projections.

### 1.4 `DocumentView<'a>` — the public root

```rust
pub trait DocumentView<'a> {
    type Root: 'a;
    fn root_value(&'a self) -> Self::Root;
    fn tape_id(&self) -> TapeId;
    fn source(&'a self) -> &'a [u8];
}

/// Generated per-grammar; for JSON the skinny emits this exact struct.
pub struct JsonDocument<'input> {
    tape: Tape<'input>,
}

impl<'input> DocumentView<'input> for JsonDocument<'input> {
    type Root = ValueRef<'input, 'input, JsonRoot>;
    fn root_value(&'input self) -> Self::Root {
        ValueRef { tape: &self.tape, index: 0, _kind: PhantomData, _input: PhantomData }
    }
    fn tape_id(&self) -> TapeId { self.tape.id }
    fn source(&'input self) -> &'input [u8] { self.tape.source }
}
```

`'doc` and `'input` are deliberately unified at the `JsonDocument` level. Callers that must outlive the input bytes use the cold `OwnedDocument` wrapper; it is outside the measured SOTA path.

---

## 2. Payload arena policy

The payload arena's job is to hold scalar payloads that **cannot** be recovered cheaply from a `(start, end)` source slice. JSON's specific cases:

| Scalar kind | Storage decision | Rationale |
|---|---|---|
| `null` | Inline. Payload class = `INLINE_BOOL_NULL`. `payload_or_skip` is unused. | Zero data; recovering it costs a kind check. |
| `true` / `false` | Inline. Payload class = `INLINE_BOOL_NULL`. | One bit of state encoded in `kind` itself (`JsonTrue` vs `JsonFalse` are distinct kinds). |
| Numbers (no escapes by definition; no normalisation needed) | **Lazy.** Payload class = `INLINE_NUMBER_FAST`. The tape token records `(start, end)`; the f64 / i64 is parsed only on `JsonNumber::as_f64()` / `as_i64()`. | Eager parse forces a `strtod`-class call per number on the hot path; sonic-rs and simdjson both lazy-parse. The bench measures structure-parse throughput; number materialisation is amortised across consumers. |
| Strings, escape-free (the common case) | **Borrow source slice.** Payload class = `INLINE_STRING_BORROW`. `flags.STRING_BORROWS_SOURCE = 1`. The tape token's `start`/`end` cover the bytes between (but not including) the surrounding `"`. | Zero copy. `JsonString::as_str()` returns `&'input str` reconstituted from `source[start..end]`. |
| Strings with escapes (`\n`, `ÿ`, etc.) | **Lazy unescape.** Payload class = `INLINE_STRING_BORROW`, `flags.STRING_NEEDS_UNESCAPE = 1`. The borrowed slice still spans the raw escape sequence. `JsonString::as_str()` returns `Cow<'input, str>`; the unescape lazily allocates only on the unescape path. | Eager unescape allocates for every string, even ones never read. The lightning-css model (`Cow`) is the V1 default per Lock 9. |
| Strings the user takes an owned copy of via `JsonString::to_string()` | Heap allocate at the call site. | Not the hot path; user opt-in. |

**Arena structure** for the skinny:

```rust
pub struct PayloadArena {
    /// One contiguous bump region. JSON skinny needs the arena only for
    /// (a) the path-schema stash if the BENCH agent decides to test
    /// `path!`, and (b) future closure capture frames. Hot-path JSON
    /// parsing has zero arena writes.
    bytes: Vec<u8>,
    /// Bench-only accounting hook. This stays zero for the JSON hot path.
    #[cfg(any(test, feature = "bench-counters"))]
    writes: u32,
    #[cfg(any(test, feature = "bench-counters"))]
    allocations: u32,
}

impl PayloadArena {
    pub fn empty() -> Self {
        Self {
            bytes: Vec::new(),
            #[cfg(any(test, feature = "bench-counters"))]
            writes: 0,
            #[cfg(any(test, feature = "bench-counters"))]
            allocations: 0,
        }
    }

    #[cfg(any(test, feature = "bench-counters"))]
    pub fn write_count(&self) -> u32 { self.writes }

    #[cfg(any(test, feature = "bench-counters"))]
    pub fn allocation_count(&self) -> u32 { self.allocations }
}
```

The skinny commits to **zero arena allocations and zero arena writes on the JSON hot path.** Every scalar lives inline (booleans/null) or as a borrowed source slice (numbers, strings). The arena is present because the substrate is grammar-neutral and other grammars (e.g. CSS L4 colour-function intermediates) require it; for JSON it stays empty. BENCH must assert `PayloadArena::write_count() == 0` and `PayloadArena::allocation_count() == 0` for Track 1 and Track 2 on twitter/citm/canada under the bench-counters feature. If the bench shows arena cache pressure on the empty path, the field becomes `Option<Box<PayloadArena>>` behind a feature gate.

Lifetime relationship: `PayloadArena` is owned by `Tape<'input>`. Anything stashed in it lives at least `'doc`. The skinny never stashes anything that depends on `'input`, so it does not need an arena-lifetime parameter.

---

## 3. SIMD scan integration contract

The structural alphabet for JSON is the eight bytes `{ } [ ] : , "` plus whitespace `space \t \n \r`. The simd-scan pass produces an offset stream identifying every structural byte in one pass over the input.

### 3.1 The dispatch table

```rust
// Lives in runtime/src/tape/scan.rs (substrate-level glue;
// the simd-scan crate owns the kernels themselves).
pub fn structural_scan_into(
    input: &[u8],
    out: &mut StructuralOffsets,
) -> ScanReport {
    #[cfg(target_arch = "x86_64")]
    {
        if is_x86_feature_detected!("avx2") {
            return simd_scan::avx2::structural_scan(input, out);
        }
    }
    #[cfg(target_arch = "aarch64")]
    {
        if std::arch::is_aarch64_feature_detected!("neon") {
            return simd_scan::neon::structural_scan(input, out);
        }
    }
    simd_scan::scalar::structural_scan(input, out)
}
```

`is_x86_feature_detected!` is checked once per `Tape` build via `std::sync::Once`; the bench protocol records which kernel ran (`ScanReport.kernel_id`).

### 3.2 The structural alphabet feed

```rust
pub struct StructuralOffsets {
    /// Byte offsets of each structural character in the input, in order.
    /// Pre-allocated to `input.len() / 8` (empirical density bound for
    /// twitter/citm/canada).
    offsets: Vec<u32>,
    /// Parallel byte stream — the structural byte at each offset, used
    /// for fast kind dispatch in the tape builder without re-reading the
    /// input.
    bytes: Vec<u8>,
}
```

Two parallel arrays so the tape builder can do a single dispatch on `bytes[i]` to choose between `JsonObjectOpen`, `JsonArrayClose`, etc., and only then index into `input` for the offset.

### 3.3 Prefilter vs verifier route

JSON's structural alphabet is **exact**, not a prefilter, by Lock 8 / `restart/ARCHITECTURE.md:951`:

> `SimdScan` Exact mode must match scalar offsets; prefilter mode emits candidates only.

The substrate accepts `SimdScanMode::Exact` for JSON structural-alphabet scans. Under `Exact`, no verifier route is needed — the SIMD output IS the answer. The substrate still demands the **scalar parity hash** (§3.4) before any tape token is emitted from the SIMD output.

For string-content scans (looking for the closing `"` past escapes), the substrate uses `SimdScanMode::Prefilter`: SIMD finds `"` and `\` candidates, the scalar verifier walks each candidate to check if `"` is preceded by an even number of `\`s. Tape emission happens only after the verifier accepts. The skinny implements both Exact (structural) and Prefilter (string-content) because both are on the JSON hot path.

### 3.4 Scalar parity hash

The bench harness owns the parity-check protocol; the substrate exposes the hook:

```rust
pub struct ScanReport {
    pub kernel_id: KernelId,        // Avx2 | Neon | Scalar
    pub offsets_count: u32,
    /// xxhash64 of (offsets || bytes). Computed by the kernel; the
    /// scalar kernel computes the same hash. Bench compares.
    pub parity_hash: u64,
}
```

If `parity_hash(SIMD) != parity_hash(scalar)` over the same input, the SIMD path is treated as broken and the kernel demoted to scalar for that bench row. The substrate does NOT silently fall back at runtime — the demotion is bench-time only; runtime trusts SIMD once parity has been validated against the corpus.

### 3.5 Throughput targets

Per `restart/ARCHITECTURE.md:1519`:

| Kernel | Target |
|---|---|
| AVX2 (x86_64) | ≥ 7 GB/s structural scan throughput. |
| NEON (M-series ARM) | ≥ 5 GB/s structural scan throughput. |
| Scalar fallback | No throughput target; correctness only. |

The bench owns the actual measurement methodology; the substrate guarantees the SIMD output is byte-faithful to scalar.

---

## 4. Direct-to-struct overlay

Direct-to-struct is the typed projection layer. Per Lock 1 and PASS-2 §2 commitment 3, direct values are NOT a second authoritative tree — they are **typed views over the same tape token stream**.

### 4.1 The typed-view shape for JSON

```rust
// All generated by codegen; substrate owns the trait shape only.

#[derive(Copy, Clone)]
pub struct JsonRoot<'doc, 'input: 'doc> {
    cursor: ValueRef<'doc, 'input, super::JsonRoot>,
}

impl<'doc, 'input> JsonRoot<'doc, 'input> {
    pub fn value(self) -> JsonValueRef<'doc, 'input> {
        // The root is a single value; index 0 is JsonRoot, index 1 is
        // its payload value.
        JsonValueRef { cursor: cursor_at(self.cursor.tape, 1) }
    }
}

#[derive(Copy, Clone)]
pub enum JsonValueRef<'doc, 'input: 'doc> {
    Object(JsonObject<'doc, 'input>),
    Array(JsonArray<'doc, 'input>),
    String(JsonString<'doc, 'input>),
    Number(JsonNumber<'doc, 'input>),
    Bool(bool),
    Null,
}

#[derive(Copy, Clone)]
pub struct JsonObject<'doc, 'input: 'doc> {
    cursor: ValueRef<'doc, 'input, super::JsonObject>,
}

impl<'doc, 'input> JsonObject<'doc, 'input> {
    pub fn len(self) -> usize { /* read open-token's payload_or_skip */ }
    pub fn iter(self) -> JsonMemberIter<'doc, 'input> { /* ... */ }
    pub fn get(self, key: &str) -> Option<JsonValueRef<'doc, 'input>> {
        // Linear scan in the skinny. A JSON-specific PHF cache lives in
        // ValueShape post-skinny; the skinny does not implement it
        // because it is not on the structure-parse hot path.
    }
}

#[derive(Copy, Clone)]
pub struct JsonString<'doc, 'input: 'doc> {
    cursor: ValueRef<'doc, 'input, super::JsonString>,
}

impl<'doc, 'input> JsonString<'doc, 'input> {
    pub fn as_str(self) -> Cow<'input, str> {
        let tok = self.cursor.tape.tokens[self.cursor.index as usize];
        let raw = &self.cursor.tape.source[tok.start as usize..tok.end as usize];
        if tok.flags.contains(TokenFlags::STRING_NEEDS_UNESCAPE) {
            Cow::Owned(unescape_json(raw))
        } else {
            // SAFETY: JSON parser validated UTF-8 boundaries during the
            // structural scan's verifier route.
            Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(raw) })
        }
    }
}

#[derive(Copy, Clone)]
pub struct JsonNumber<'doc, 'input: 'doc> {
    cursor: ValueRef<'doc, 'input, super::JsonNumber>,
}

impl<'doc, 'input> JsonNumber<'doc, 'input> {
    pub fn as_f64(self) -> f64 {
        // Lazy parse from source slice. parse-that's number kernel here
        // post-skinny; for the skinny, std::str::FromStr.
    }
    pub fn as_i64(self) -> Option<i64> { /* ... */ }
}
```

### 4.2 Field-cache decisions

PASS-3 §4 distinguishes node kinds with **scalar caches** (the cache lives in tape token's payload slot, paid for once at parse time) vs **tape-only** (read on demand from source). Skinny JSON policy:

| Node kind | Scalar cache | Why |
|---|---|---|
| `JsonObjectOpen` / `JsonArrayOpen` | sibling-skip count | O(1) traversal past the subtree; this IS the cache. |
| `JsonString` | none | The escape-free vs escape bit lives in `flags`; the bytes are in source. |
| `JsonNumber` | none | Lazy parse. |
| `JsonBool` | inline (kind itself) | One bit. |
| `JsonNull` | inline | Zero bits. |
| `JsonMember` (key-value pair) | none | The key is a `JsonString` token at index `i+1`; the value follows. |

This matrix is what the codegen agent will emit `materialisation_cost.toml` from per `restart/audit/pass-3-runtime/PASS-3.md:144`.

### 4.3 Lifetime relationships

Every typed view carries `<'doc, 'input: 'doc>`. `'input` outlives `'doc`; the `Tape` borrows source for `'input` and exposes views for `'doc`. This is the discriminant of Lock 9.

For the skinny, `parse(&'a str) -> JsonDocument<'a>` collapses both lifetimes to `'a`. `parse_in` keeps them collapsed; only `parse_owned` (cold wrapper) splits them.

---

## 5. Snapshot / identity invariant

Per `restart/ARCHITECTURE.md:1401`:

> Every public node has one `(TapeId, node id, payload class)` identity.

The skinny has four named consumers of identity (PASS-3 §4 lists five for full V1; the skinny drops "debug trace"):

1. **`DocumentView::root_value`** — returns `ValueRef { tape, index: 0, .. }`. Identity: `(tape.id, 0, payload_class_of(tokens[0]))`.
2. **`ValueRef`-direct cursor** — every projection (`JsonObject::get`, `JsonArray::iter`) yields `ValueRef { tape, index: i, .. }` where `i` is a token index into the same `tape`. Identity: `(tape.id, i, payload_class_of(tokens[i]))`.
3. **Visitor walker** — see §6. The walker yields `ValueRef`s with the same `(tape.id, i, payload_class)`.
4. **Bench harness** — the BENCH agent's parity harness compares the generated parser's output to the hand-coded JSON parser on identity. Both must produce `(TapeId, index)` pairs with the same `payload_class` for matching source positions.

**Identity proof on paper.** Every `ValueRef` is constructed by code that has a `&Tape<'input>` in scope; `tape.id` is fixed for the parse. `index` comes from one of three sources: zero (root), an arithmetic step (`i + 1` for "next sibling at scalar", or `i + 1 + skip` for "next sibling past container"), or a sub-cursor returned by a typed projection method. Each arithmetic step lands on a token; `payload_class` is read from `tokens[index].flags & PAYLOAD_CLASS_MASK`. Hence `(tape.id, index, payload_class)` is determined by `(tape, index)` alone, and `tape` cannot mutate after parse (Box<[T]>, sealed). Identity is stable.

**No second tree.** There is no separate AST; `JsonValueRef` is `Copy` and contains only `(tape, index, kind-marker)`. Visitors and the bench harness share identity by construction.

---

## 6. Visitor entry

The full V1 carries `Visitor`, `VisitMut`, `VisitTypes` bitflag pruning, and a generated walker. The skinny carries the read-only walker only.

```rust
// runtime/src/visitor/mod.rs (substrate-level trait shape).
pub trait Visit<'doc, 'input: 'doc, K> {
    fn visit(&mut self, node: ValueRef<'doc, 'input, K>);
}

// Generated for JSON.
pub trait JsonVisitor<'doc, 'input: 'doc> {
    fn visit_root(&mut self, n: JsonRoot<'doc, 'input>) {
        self.walk_root(n);
    }
    fn visit_value(&mut self, n: JsonValueRef<'doc, 'input>) {
        self.walk_value(n);
    }
    fn visit_object(&mut self, n: JsonObject<'doc, 'input>) {
        self.walk_object(n);
    }
    fn visit_array(&mut self, n: JsonArray<'doc, 'input>) {
        self.walk_array(n);
    }
    fn visit_string(&mut self, _n: JsonString<'doc, 'input>) {}
    fn visit_number(&mut self, _n: JsonNumber<'doc, 'input>) {}

    fn walk_root(&mut self, n: JsonRoot<'doc, 'input>) {
        self.visit_value(n.value());
    }
    fn walk_value(&mut self, n: JsonValueRef<'doc, 'input>) {
        match n {
            JsonValueRef::Object(o) => self.visit_object(o),
            JsonValueRef::Array(a) => self.visit_array(a),
            JsonValueRef::String(s) => self.visit_string(s),
            JsonValueRef::Number(x) => self.visit_number(x),
            JsonValueRef::Bool(_) | JsonValueRef::Null => {}
        }
    }
    fn walk_object(&mut self, n: JsonObject<'doc, 'input>) {
        for member in n.iter() {
            self.visit_value(member.value());
        }
    }
    fn walk_array(&mut self, n: JsonArray<'doc, 'input>) {
        for v in n.iter() { self.visit_value(v); }
    }
}
```

**Stubbed in the skinny.**

- `VisitMut` — full V1 mutation visitor; the JSON SOTA bench is read-only.
- `VisitTypes` bitflag pruning — the W5 walker pruning trick. The skinny does not measure pruning benefit; it visits the whole tape.
- `LayoutVisitor` / `@pretty` re-emission — irrelevant to parse-throughput.

The visitor trait is here because the bench harness's "read-twice" path (parse → traverse) needs a second-pass touchpoint; without it, parse-time SIMD wins are masked by traversal cost.

---

## 7. What this skinny substrate omits, and why each omission is safe

| Omitted feature | Where it lands in full V1 | Why omitting it does not compromise the SOTA test |
|---|---|---|
| `@layout` / layout policy | `runtime/src/layout/`; consumed by `@pretty` re-emission | JSON has no layout-significant whitespace; the `simd/structural_scan` row treats whitespace as a skip class. Layout would be measured against CSS, not JSON. |
| Error recovery (`@error(recover = ...)`) | `runtime/src/error/`, `RecoveryKind` flags on TapeToken | Hot-path SOTA benches use valid JSON corpora (twitter, citm, canada). The recovery codepath is cold-path; benching well-formed input does not exercise it. The bench gate explicitly targets `<= 380us` on twitter assuming valid input. |
| Closure environment frames | PASS-3 §4.1; `&'i Tape<'i>` capture only | JSON grammar has no closures (no `|x| body` in the grammar source). Lock 4 amendment names four sites — host-chain, map, predicate, recovery — none of which fire on JSON's hot path. |
| DAP trace events (`debug-trace`) | `runtime/src/tape/trace.rs` | Pure debug instrumentation; off in release. The bench runs in release. |
| Incremental reuse map (`ReparsePlan`) | `crates/lsp/`, `DocumentSnapshot::id` | The bench measures cold-parse throughput, not edit-reuse. |
| `OwnedDocument` / `parse_owned` | `runtime/src/owned/` | The skinny measures `parse(&str)`; owned form is a wrapper that does not change parse cost. |
| `path!` / `select!` runtime | `crates/path/`, `crates/path-core/` | Path eval is post-parse cursor traversal; skinny's visitor §6 is sufficient to prove typed views project. |
| `Visit::Mut` mutation | `runtime/src/visitor/mut.rs` | Read-only is sufficient for parse-throughput SOTA. |
| `JsonObject::get` PHF cache | `ValueShape` policy, post-skinny | Object-key lookup is not on the parse hot path; sonic-rs benches do not measure it either. Skinny linear-scans keys. |
| Eager number parse | Number scalar cache in tape token | Sonic-rs and simdjson both lazy-parse numbers; lazy is the SOTA convention. |
| Multi-grammar tape kind sharing | `NodeKindId` mapping table per grammar | Skinny is one grammar; the kind table is hard-coded. |

The skinny substrate keeps **only** what the parse-throughput SOTA test requires: token packing, structural SIMD scan, payload arena (empty for JSON), typed projection, snapshot identity, read visitor. If JSON SOTA-parity holds with this substrate, the SOTA premise is validated; the omitted features are orthogonal axes (correctness on bad input, mutation, incremental reuse, etc.) that do not contribute to the throughput row.

---

## 8. Hand-coded JSON parity contract (delivered to BENCH)

The BENCH agent will hand-code a JSON parser against this substrate to establish the parity-floor side of the dual-track measurement. The substrate exposes for that purpose:

```rust
// Public to the workspace; gated by `#[doc(hidden)]` for the published
// crate.

pub fn build_tape_for_json<'input>(
    source: &'input [u8],
    payloads: PayloadArena,
) -> Result<Tape<'input>, ParseError>;

/// Lower-level: append a token. Used by the hand-coded parallel.
pub fn push_token(builder: &mut TapeBuilder<'_>, tok: TapeToken);

pub struct TapeBuilder<'a> {
    source: &'a [u8],
    tokens: Vec<TapeToken>,
    payloads: PayloadArena,
    next_id: TapeId,
}

impl<'a> TapeBuilder<'a> {
    pub fn finish(self) -> Tape<'a>;
}
```

The hand-coded parallel will use `TapeBuilder` directly and produce a `Tape<'input>` byte-identical (modulo `TapeId`) to the codegen-emitted parser's output on the same input. `TapeBuilder` owns the `PayloadArena` it seals into `Tape`; `build_tape_for_json` may accept a caller arena for `parse_in`, but the finished `Tape` has one ownership story. The BENCH agent's identity gate compares token streams; the substrate guarantees the comparison is well-defined because token layout (§1.1) is fixed.

---

## 9. Module layout for the skinny

Per Lock 13 (no god directories) and PASS-2 §3 (`runtime/src/tape/`), the skinny substrate lives at:

```text
crates/runtime/src/
  lib.rs
  tape/
    mod.rs        // Tape, TapeId, public re-exports
    token.rs      // TapeToken, NodeKindId, TokenFlags
    builder.rs    // TapeBuilder, push_token
    payload.rs    // PayloadArena
    scan.rs       // structural_scan_into dispatch (calls simd-scan)
    view.rs       // ValueRef<'doc, 'input, K>, AnyKind
  visitor/
    mod.rs        // Visit trait
  grammars/
    json/
      mod.rs           // JsonDocument, JsonRoot, public surface
      generated.rs     // emitted by codegen; the skinny's hand-fixture
                       // version of this file lives in fixtures, not here
      view.rs          // typed views (JsonObject, JsonArray, ...)
      visitor.rs       // JsonVisitor trait (generated, hand-stubbed for skinny)
```

The WORKSPACE agent owns Cargo.toml shape and crate boundaries; this layout is what SUBSTRATE asks WORKSPACE to land.

---

## 10. Open questions surfaced for the orchestrator

These are not blockers for the skinny, but the bench result will turn them into commit-able decisions.

- **`payload_or_skip` union vs split fields.** The 16-byte token assumes a discriminated union. If bench shows the payload-class branch on every token-walk dominates, the full V1 may pay 24 bytes for a split. Decision deferred to bench.
- **NodeKindId width.** `u16` is comfortable for JSON (≤16 kinds) and for the V1 grammar set (CSS L4 has the largest kind table; ~512 kinds estimated). If a future grammar exceeds 65 535 kinds, the field widens to `u32`, blowing the 16-byte token. Acknowledged; not a JSON skinny risk.
- **`Tape` vs `Arc<Tape>`.** The skinny uses `Tape<'input>` borrowed by `&'doc`. Multi-thread sharing of a parsed tape (a feature sonic-rs supports) requires `Arc`. The skinny does not exercise this; full V1 may decide on `Arc<Tape>` in `OwnedDocument` form.
- **Whitespace-skip token policy.** The skinny does not emit whitespace tokens (whitespace is consumed by the structural scan and dropped). This matches sonic-rs / simdjson. CSS will need a different policy under `@layout`. Substrate-level: whitespace handling is per-grammar at codegen, not substrate-level.

The bench result drives any of these to a commit.
