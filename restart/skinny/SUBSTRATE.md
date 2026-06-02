# Skinny Spec — Substrate Slice

## 0. Scope and stance

The skinny exists to validate the V1 SOTA-viability claim against ONE grammar end-to-end before tranches A-J commit. This document specifies the substrate slice — the runtime layer that codegen lowers to and the bench measures against.

The substrate is **grammar-neutral**: every type, layout decision, and arena policy here is the same shape the full V1 will ship for CSS, BBNF, Sheets, and the rest. The skinny exercises only the JSON cell of that shape. CSS layout, Sheets Pratt/host chains, and other non-JSON mechanisms still close under their V1 gates.

**Iteration evidence — split verdict.** The substrate has been measured against
two gates. The original twitter / citm_catalog / canada triad passes and
validates `OffsetTape` (the lazy-offset implementation of one of the five
`BackendShape` values per ARCH §7.3) plus direct projection as the canonical
substrate; the empirical anchor is the A/A/A triad at `skinny/RESULTS.md` and
the V3 authority at `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`
§4 "validated original research items". The expanded corpus in
`skinny/RESULTS.md` is now the binding SOTA-BEAT authority and records overall
outcome G / NoGo: `github_events`, `update_center`, `random`,
`unicode_escapes`, and `y_string_unicode` miss the S anchor. The
structural-only canada scan stays above floor, so the remaining work is not a
return to eager tokens; it is string/Unicode primitives, object/key-dispatch
shape, typed projection discipline, and streaming mask-consume kernels.

**SK-V6 substrate fold-back (2026-05-15).** The asmjson/DAV1D pass reaffirms
that Lock 1 is a union contract, not an eager-token contract: retained
structural projection is tape storage, direct output is a sibling projection,
and transient masks are not retained substrate. The generalized substrate facts
are `StructuralClassTable`, `EventProjectionPlan`, `PayloadFlagPlan`,
`TapeBuilderPlan`, `DirectFieldFacts`, `CostFacts`, and `PrimitiveFacts`. JSON
supplies one instance; CSS, Sheets, and BBNF-self supply others. SIMD/ASM
primitives are admitted only through Lock 16 scalar-oracle plus checkasm gates
and a same-wave runtime or generated consumer.

**Pass Omega V10 / SK-V18 substrate receiver (2026-06-01).** No substrate
amendment follows from SK-V18 totality closure. Lock 1 substrate union, no
parallel substrate, no cross-call retained classifier state, and the exact
5-shape `BackendShape` canon {EagerTape, OffsetTape, EventTape, SinkOnly,
CollapsedStage} remain binding (the 16-lock count is preserved by addition). The
active implementation route is SK-V18 W-PRUNE→G1..G6→PROVE→H1, not SK-V15 W0-W11.

1. **Lock 1 substrate-union boundary remains elevated.** Quote/escape/structural
   masks, class-stream, prev-state byte, and prefix-XOR carry may be transient
   producers; cross-call retained classifier state remains inadmissible. They do
   not create a retained sidecar, second tape, public `UnionTape`, alternate
   document projection, or public substrate API.
2. **The fact-stream `String` is DELETED (W-PRUNE).** FactStream is not a sixth
   `BackendShape`; EventTape is not a retained sidecar stream. `CSS_GENERATED_RS`,
   `CssFullParseSummary`, and brace-counter proof are RETIRED; CSS live admission
   is the `track1_rich` typed CSSOM projection over the tape.
3. **SK-V18 G2 lowers CSS via the `css_balanced_component_scan` named primitive;
   G3 un-forks the emitter on the `BackendShape` discriminator; G5/G6 wire the
   NEON classifier** (`acceleration_at_admission == admission`; the only grammar
   datum is `alphabet:&[u8;64]`). Every SIMD primitive carries a scalar
   oracle/reference, strict parity/checkasm where relevant, a same-wave generated
   consumer, and native Apple M5 Max / aarch64 admission evidence. x86 is DELETED.
4. **The 5-shape search domain at Lock 10 holds verbatim.** A sixth shape,
   retained EventTape sidecar, public `UnionTape`, or production FNV route remains
   blocked.

**Pass Omega V10 / SK-V18 substrate receiver (tape-as-unified-substrate +
ValueRef<G>, CERTIFIED).** The SoA `Tape<'input>` is the single post-fold
substrate; lazy `ValueRef<G>` is the one materialization plane, now
SK-V18-CERTIFIED to project JSON byte-equal AND CSS + Sheets lazy from ONE
`BackendRule`-walk. These are the proven-and-benched skinny WINs (JSON 51/51
strict A/GO Track 1 > sonic same-plane; CSS track1_rich > lightningcss
1.66-3.38x). No substrate amendment is created here. The receiver binds the LOCKS
SK-V17 T-P3 Crystallisation Addendum (Lock 1 tape-substrate-union clause + Lock 14
ValueRef/classifier-generalisation clause), preserved by addition in the 3C
locks-v+1 diff.

1. **SoA `Tape<'input>` is the single post-fold substrate (`3D-SK17-D01`).** The
   SoA encoding (`offsets:Vec<u32>` + sparse `flag_cursors`/`flag_values` +
   `PayloadArena`) is the V1-authoritative substrate the five `BackendShape`
   shapes project from. The AoS `TapeRec` (16-byte/align-4) converges ONTO it
   under SK-V18; a dual AoS/SoA end-state is a transient fold-state only, NEVER a
   permissible Lock-1 closure (exactly one encoding survives). The substrate
   stays grammar-column-free (`3E17-D02`): flags are sparse position-keyed
   side-vectors, never a per-grammar dense class column (the barred AV.04 shape).
2. **Lazy `ValueRef<'doc,'input,K,G:EventGrammar>` is the one materialization
   plane (`3D-SK17-D02` / `3E17-D01`).** The grammar enters as a TYPE parameter
   monomorphised at codegen, with ZERO runtime `match grammar {…}` arm. One
   `BackendRule`-walking generator emits document/value/view/visitor; the
   existing `@generated` per-grammar value path is RETARGETED to emit lazy
   `ValueRef<G>`. preserve-rich-ast holds: the lazy view IS the rich-AST
   materialization plane, never a typed-AST flattening.
3. **The `FieldSource` projection walk is compile-time emission (`3E17-D09`).**
   The `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}`
   walk that produces the `ValueRef<G>` projection is grammar-neutral ONLY as
   compile-time projection-emission resolved once at codegen. Any per-leaf
   runtime `StructRegistry::layout` walk in the projection hot path re-opens the
   measured 28-65×/983×/10583× regression AND re-introduces grammar-shaped runtime
   dispatch; `begin_compound` reads `layout.rule_id & 0x1F` only (grep-zero
   `StructRegistry`). REJECT.
4. **The eager value tree is the fold-DELETION target, never carried forward.**
   The AZ-IV eager value tree REFUTED 118× — the crates/core `CssTypedValue` enum
   + six `pending_*` Vecs (the eager `OpenFrame` builder) — is replaced by the
   lazy projection, retired no-delete-before-same-wave-replacement (the tape
   consumer proven first). Materialization stays lazy-by-default.

Source authority (verbatim citations, not paraphrase):

- `restart/ARCHITECTURE.md` §9 (lines 1373-1426) — tape and direct-to-struct union; tape invariants; per-grammar runtime template path.
- `restart/ARCHITECTURE.md` §3.1 (lines 191-244) — `Grammar` trait, `parse` / `parse_in` / `parse_owned` shape, the `'a` lifetime carry.
- `restart/audit/pass-3-runtime/PASS-3.md` §4 (lines 150-191) — illustrative `Tape<'input>` / `TapeToken` / `ValueRef<'doc, 'input, K>` shape; closure environment by `&'i Tape<'i>`.
- `restart/ARCHITECTURE.md` §7.2 BIR rows for `TapeEmit`, `DirectBuild`, `ValueProject`, `SimdScan`, `SpanMark` (lines 920-963) — what the substrate is the receiver of.
- `restart/audit/pass-2-codegen/PASS-2.md` §2 commitment 3 — TapeShape + ValueShape are one materialisation plan.
- `restart/locks/LOCKS.md` Lock 1 (line 75; v+1 substrate-union ELEVATION at `:137` LAC-2F-V5-02; FactStream 5th substrate category at `:100`-`116` LAC-1E-14) — tape is THE substrate; no parallel substrate; no OpenFrame clone; no cross-call retained classifier state.
- `restart/locks/LOCKS.md` Lock 8 (line 202; v+1 row-plane + audit-overlay LAC-1E-16 at `:213` 4-column gate) — sonic-rs / simdjson / lightning-css anchors with comparator-plane provenance and audit-overlay verdict per row.
- `restart/ARCHITECTURE.md` §11 row `simd/structural_scan` (line 1519) — ≥56000 Mbps AVX2, ≥40000 Mbps NEON, scalar parity hash mandatory.

The full-V1 spec PASS-3 §4 admits its layout is **illustrative, not mandatory** ("This layout is not a PASS-1 mandate; it is a user-surface contract. PASS-1 may pack differently if these semantics remain true." — `PASS-3.md:187`). The skinny pins one packing and measures it. If the bench fails parity, the packing is the variable to perturb, not the contract.

---

## 1. `Tape`, `TapeToken`, `ValueRef`, `DocumentView` — concrete layouts

**Canonical skinny storage, 2026-05-12.** The implemented skinny JSON path uses
the `OffsetTape` `BackendShape` value (one of five per ARCH §7.3): lazy offset
tape with sparse flags. The structural projection is tape storage, not a
sidecar. The 16-byte `TapeToken` model below is retained as the `EagerTape`
shape (V1 fallback for grammars whose recovery, layout, or parse-time
materialization requirements force `EagerTape` per the ARCH §7.3 derivation
algorithm). Generated JSON views and direct-to-struct projections must treat
offsets plus flags as the committed tape identity.

### 1.1 `TapeToken` — 16 bytes, hot-path packed

```rust
#[repr(C, align(16))]
pub struct TapeToken {
    /// Node kind id (grammar-agnostic; emitted into a per-grammar
    /// `NodeKind` enum at codegen time).
    pub kind: NodeKindId,        // u16 — 65 535 kinds is far above
                                 //       any realistic grammar
    pub flags: TokenFlags,       // u16 bitfield (bit layout):
                                 //   bits 0..=3  PAYLOAD_CLASS         (4)
                                 //   bit  4      HAS_SCALAR_CACHE      (1)
                                 //   bit  5      PAYLOAD_NEEDS_NORMALIZE (1)
                                 //   bit  6      PAYLOAD_BORROWS_SOURCE  (1)
                                 //   bit  7      IS_STRUCTURAL_OPEN    (1)
                                 //   bit  8      IS_STRUCTURAL_CLOSE   (1; reserved
                                 //                 for grammars/recovery that emit
                                 //                 explicit close tokens; JSON
                                 //                 skinny emits zero close tokens)
                                 //   bits 9..=15 reserved              (7)
    pub start: u32,              // source byte offset (input length
                                 //                     ≤ 2³² for V1 hot path)
    pub end: u32,                // source byte offset (exclusive)
    pub payload_or_skip: u32,    // discriminated by flags.PAYLOAD_CLASS:
                                 //   - INLINE_BOOL_NULL:    ignored (zero)
                                 //   - INLINE_NUMBER_FAST:  nothing
                                 //     (number recovered from start..end)
                                 //   - INLINE_STRING_BORROW: nothing
                                 //     (string slice = source[start..end];
                                 //      PAYLOAD_BORROWS_SOURCE / 
                                 //      PAYLOAD_NEEDS_NORMALIZE annotate)
                                 //   - ARENA_OFFSET:        byte offset into
                                 //                          PayloadArena
                                 //   - SIBLING_SKIP for container nodes:
                                 //                          count of token slots
                                 //                          to skip to reach
                                 //                          next sibling
}
```

The `PAYLOAD_CLASS` 4-bit field enumerates: `INLINE_BOOL_NULL` (0), `INLINE_NUMBER_FAST` (1), `INLINE_STRING_BORROW` (2), `ARENA_OFFSET` (3), `SIBLING_SKIP` (4); values 5..=15 are reserved for V1 grammars. `RECOVERY_KIND` is **not** present in the skinny TokenFlags (recovery is omitted per §7); the 2 bits the SK-V1 draft reserved for it are folded into the `reserved (7)` tail and remain available for V1 grammars that need explicit recovery state. Flag names are grammar-neutral (`PAYLOAD_*`, not `STRING_*`): the same flags annotate CSS function-call arguments, BBNF terminal payloads, and Sheets formula tokens.

Fields total: 2 + 2 + 4 + 4 + 4 = 16 bytes. `#[repr(C, align(16))]` so a 64-byte cache line holds exactly four tokens with no straddle on x86 / ARM. AVX2 may load eight tokens (128 bytes) with two aligned 32-byte loads.

`payload_or_skip` is the load-bearing union slot. PASS-3 §4 carries `payload: u32` and `sibling_skip: u32` as separate fields — but for the JSON skinny we collapse them, because:

- Scalar tokens (`JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`) need the payload pointer; they do not need a sibling-skip (they have no children).
- Container open tokens (`JsonObjectOpen`, `JsonArrayOpen`) need the sibling-skip to allow O(1) traversal past the subtree; they do not need a payload pointer. JSON skinny uses close-token elision: the open token's `end` is patched to the close delimiter's exclusive offset and `payload_or_skip` carries the subtree skip.
- `flags.PAYLOAD_CLASS` discriminates.

If the bench shows the union encoding is the bottleneck, splitting back to 24 bytes is a one-commit perturbation; carrying both eagerly costs a cache line per token-pair. The full V1 may refine this; the skinny commits to 16 bytes because the SOTA target lives or dies by token-cache density.

`NodeKindId` is `u16` (a transparent newtype) and is grammar-derived but the type itself lives in `runtime/src/tape/`. The mapping `JsonObjectOpen → 0u16`, `JsonObjectClose → 1u16`, etc., is generated per grammar; the substrate never sees the names. JSON close kinds remain reserved in the generated kind table for diagnostics/recovery and V1 grammars that need explicit close events, but the SOTA JSON tape emits zero close tokens.

### 1.2 `Tape<'input>` — owning token stream + payload arena

```rust
pub struct Tape<'input> {
    /// The input bytes the tape borrows from. JSON is byte-clean so
    /// `&[u8]` here even though the public Grammar API may take `&str`.
    source: &'input [u8],

    /// Tape token stream, append-only inside TapeBuilder and private
    /// after commit. Vec<T> avoids the parse-boundary shrink/copy cost;
    /// the public read API exposes only &[TapeToken].
    tokens: Vec<TapeToken>,

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

**Sealing trade-off (incremental deviation).** The initial skinny pinned `Box<[TapeToken]>` to remove `len/cap` divergence. Measurement (`skinny/REDRESS.md` item 15) showed that `Vec::into_boxed_slice()` adds a parse-boundary shrink/copy after over-reserving from the structural index. The canonical skinny therefore uses a private `Vec<TapeToken>` inside the finished `Tape`: semantic sealing is enforced by type privacy, and the public read API remains `&[TapeToken]`. The bench reports both logical tape bytes and allocated tape bytes so this throughput win does not hide memory residency (`skinny/RESULTS.md` "Notes": twitter 649680 logical / 1064272 allocated = 1.03× / 1.69× input; citm_catalog 1432272 / 2351040 = 0.83× / 1.36×; canada 2675136 / 3572160 = 1.19× / 1.59×). V1 graduation remains mechanical: the committed snapshot can stay private-Vec, become chunked, or re-seal to `Box<[T]>` at a non-hot snapshot boundary without changing `ValueRef` or typed projections.

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

The lifetime parameters `'doc` and `'input` are **the discriminant** of the slice-borrow / arena / owned forms (Lock 9, `restart/locks/LOCKS.md:50`). For the skinny:

- `parse<'i>(input: &'i str) -> Result<JsonDocument<'i>, ParseError>` collapses to `'doc = 'input = 'i`. The public Grammar API takes `&str` (UTF-8 prevalidation outside the timed region per Lock 9); the substrate operates on the underlying byte slice internally.
- `parse_in<'i>(input: &'i str, arena: &'i Arena) -> Result<JsonDocument<'i>, ParseError>` keeps them collapsed; the `Arena` only widens the payload arena's backing storage.
- `parse_owned` lives behind an `OwnedDocument` wrapper that self-references (V1 receiver: `runtime/src/owned/`). The skinny facade may expose `parse_owned` only as a cold wrapper over `parse`; the SOTA rows measure `parse(&str)` and do not treat owned form as implemented hot-path substrate.

`PhantomData<fn() -> K>` (rather than `PhantomData<K>`) keeps `ValueRef` `Copy`/`Send`/`Sync` regardless of `K`'s auto-trait posture. The kind tag never holds data.

Kind families for JSON:

```rust
pub enum AnyKind {}            // erased
pub enum JsonRootKind {}
pub enum JsonValueKind {}
pub enum JsonObjectKind {}
pub enum JsonArrayKind {}
pub enum JsonStringKind {}
pub enum JsonNumberKind {}
pub enum JsonBoolKind {}
pub enum JsonNullKind {}
pub enum JsonMemberKind {}     // a (key, value) pair token
```

The `*Kind` suffix disambiguates these uninhabited markers from the typed-view structs at §4.1 (`JsonRoot<'doc, 'input>`, `JsonObject<'doc, 'input>`, …). The marker enums constrain the `K` type parameter on `ValueRef<_, _, K>`; the typed-view structs are thin wrappers around `ValueRef<_, _, *Kind>`.

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
    // The public `root_value()` returns the typed-view `JsonRoot<'input, 'input>`
    // from §4.1 by wrapping the raw cursor. Internal projection uses the
    // `JsonRootKind` marker so the type parameter on `ValueRef` is constrained.
    type Root = JsonRoot<'input, 'input>;
    fn root_value(&'input self) -> Self::Root {
        JsonRoot {
            cursor: ValueRef {
                tape: &self.tape,
                index: 0,
                _kind: PhantomData,
                _input: PhantomData,
            },
        }
    }
    fn tape_id(&self) -> TapeId { self.tape.id }
    fn source(&'input self) -> &'input [u8] { self.tape.source }
}
```

`'doc` and `'input` are deliberately unified at the `JsonDocument` level. Callers that must outlive the input bytes use the cold `OwnedDocument` wrapper (V1 receiver: `crates/runtime/src/owned/`); it is outside the measured SOTA path.

**Return-type settlement.** `Json::parse(&'i str) -> Result<JsonDocument<'i>, ParseError>` is the public return type; `JsonDocument` owns the sealed `Tape<'input>`. Callers obtain the root projection through `document.root_value() -> JsonRoot<'i, 'i>` (see §4.1). The `JsonRoot<'doc, 'input>` typed view at §4.1 is the second-tier surface, never the parse-API return type. The kind-marker enum at §1.3 is named `JsonRootKind` (uninhabited); the typed cursor at §4.1 retains the name `JsonRoot<'doc, 'input>`; the two identifiers no longer overload.

### 1.5 Tape ≡ structural projection (canonical; OffsetTape implementation)

`Tape<'input>` is the canonical retained substrate; the lazy-offset implementation is the `BackendShape::OffsetTape` value of the five-shape taxonomy carried at `restart/ARCHITECTURE.md` §7.3 (`LayoutFacts.backend_shape`). The substrate is grammar-neutral: the five shapes are `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`. JSON in the skinny lowers to `OffsetTape` per the cost-model derivation in ARCH §7.3; that derivation is auto (no directive, no workspace metadata) and is keyed off existing Grammar IR facts (first-set disjointness, `@error(recover)` presence, `@host fn` parse-time-decoded presence, `@layout` scope presence). The lazy-offset implementation remains the measured storage choice, but the expanded throughput corpus is the binding SOTA-BEAT gate and currently records overall `N-direct / NoGo`: 5 retained hard-G rows, D/E retained codegen-gap rows, five `semantic_full_digest_stressor` pass rows, 12 direct digest misses, and representative `real_typed_struct` passes for `twitter` and `update_center`. SK-V5 redress item 56 restores the focused Canada structural-only scan floor above the 40000 Mbps NEON floor, so the storage shape is not reopened as eager tape; the remaining work is the typed event cursor over the same projection, decoded string/direct materialization, grammar-neutral cost facts, and primitive closure. Lock 1 verbatim plus the 2026-05-12 clarification at `restart/locks/LOCKS.md:34` establish that the structural projection IS the tape's storage; there has never been a separate "tape" buffer the parser writes to after consuming the scan output.

`Tape<'input>` owns the offsets array, the payload arena, and the per-offset packed flags as one structure. `ValueRef<'doc, 'input, K>` carries `cursor: u32` indexing into `Tape::offsets`; the typed cursor consumes the same projection that the scan emitted. There is no parallel sidecar structural-index `Vec`; if structural offsets are retained, they ARE the tape. Among the five comparator parsers (simdjson, asmjson, sonic-rs LazyValue, yyjson, RapidJSON), only simdjson keeps two buffers post-parse (`structural_indexes` on `dom_parser_implementation` plus `tape` on `document`). asmjson, yyjson, RapidJSON, and sonic-rs's LazyValue path all emit directly into the parse-time-output buffer; the LazyValue path emits nothing because the slice IS the projection. The skinny adopts the one-buffer posture.

The pre-lazy skinny implementation carried **three** parallel offset buffers at runtime: `ParserState.structural_offsets` (scan-emitted; lifetime: parse), `TapeAssembler.offsets` (codegen-copied; lifetime: parse), `Tape.offsets` (sealed; lifetime: post-parse). That was implementation drift, not a Lock 1 defect. Lock 1's verbatim text never required two buffers. The current runnable skinny has moved to `TapeBuilder` plus `Tape::offsets`, but `generated::attach_structural_index` is still a no-op and generated parse functions still walk source bytes through `cursor`, `skip_ws`, and `parse_value_at`. The next substrate close is therefore not another stored sidecar; it is consuming one tape projection through a typed event cursor (§1.6 below; lowering contract at `COMPILER.md` §3.3).

SK-V5 redress item 50 makes this negative requirement binding. A dense parse-time aux column (string/scalar ends plus container next cursors) improved retained traversal probes but regressed `track1_generated` by 25-29% on twitter/citm_catalog; a sparse aux side table with O(1) slot patching still regressed the original triad by 28-33%. Both variants were reverted. Retained projection facts may be derived or consumed by the typed event cursor, but they must not be materialized as another parse-time retained column in the SOTA path.

Lock 1 stands. The clarification appends to the existing 2026-05-04 reframe: *"the structural projection IS the tape, not a sidecar to it; no parallel offset stream."* The substrate-side migration is mechanical:

- `Tape<'input>` already holds `offsets: Box<[u32]>` + `flags: Box<[u8]>` (currently `string_escape_offsets` + `string_control_offsets` — fold the three Box<[u32]> sidecars into one + one packed `flags: Box<[u8]>` byte-per-offset).
- `ParserState.structural_offsets` — DELETE; scan emits through `TapeBuilder` (renamed from `TapeAssembler`) which is a thin facade over `Tape::offsets` during construction.
- `TapeBuilder` (the OLD eager `Vec<TapeToken>` carrier at `tape/mod.rs:223-292`) — DELETE; dead code from the eager era.
- `TapeToken`, `NodeKindId`, `TokenFlags` PAYLOAD_CLASS constants — DELETE; unused after eager-path retirement.

Net LOC delta for the remaining substrate consolidation: ~−80 LOC capacity/sidecar cleanup + ~60 LOC typed event cursor adapter + ~60 LOC scanner-to-builder write-through. The fresh `update-center` profile still shows sparse-flag capacity and allocation-growth samples, so the capacity policy is load-bearing and must be profiled before/after.

The architectural lesson is narrower than the earlier Wave 3 forecast: the implemented triad win did not require a sidecar structural-index typed-parser prepass, a NEON no-escape matcher, separator elision, generic SWAR whitespace, 12-byte/width churn, or dispatch-table/function-pointer alternates. The expanded gate then revealed the next honest lever: a typed event cursor over the same tape projection, plus grammar-neutral byte/string/number primitives. `parse_value_at` being the dominant fresh profile leaf on `random` and `unicode_escapes` makes this a codegen/substrate-consumption issue, not a return to eager tokens.

SK-V5 redress items 51 and 53 narrow "typed event cursor" further. A transient
`JsonEventCursor` that only centralized whitespace skipping behind
`BYTE_CLASS_FROM_EQ_SET_64` was measured and rejected: focused
`track1_generated` rows fell to roughly twitter 7130 Mbps, citm_catalog
10291 Mbps, and canada 14110 Mbps. A stricter parser-local
`JsonStructuralCursor` then consumed the scanner's live per-stripe JSON emit
mask (`punctuation & !string_body | real_quotes`) with O(1) pending state and
still regressed to twitter 6156 Mbps, citm_catalog 8344 Mbps, and canada 7139
Mbps because it scanned source bytes beside the recursive-descent parser. The
admissible H.W1 cursor therefore is not a renamed whitespace skipper and not a
second parser-local scanner. It must consume the scanner/tape event stream as
the single parse substrate, with no retained `StructuralIndex`, no
`Vec<JsonEvent>`, no whitespace bitmap sidecar, and no aux projection column.

### 1.6 Typed event cursor over tape projection (canonical lowering pattern)

The surviving architectural lever is a **codegen template inversion** over the tape projection: a lowering pattern, not a new BIR variant or directive, and not a new substrate variant. The substrate variants are the five values of `BackendShape` carried at `LayoutFacts.backend_shape` (ARCH §7.3); per-rule selection across `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage` is cost-model-derived from existing Grammar IR facts. Generated parsers consume typed events from the offset/flag stream, never re-scanning source bytes for whitespace or value boundaries outside grammar-neutral primitives. The substrate is Lock 1 verbatim: tape and direct-to-struct are one retained projection. The change is what the emitted `parse_*` functions do with that projection, governed by the per-rule `backend_shape` decision.

Empirical anchor (verified at `skinny/profile/simdjson-v2/PROFILE-REPORT.md`): simdjson's `json_iterator::advance()` is `&buf[*(next_structural++)]`, a single u32-indexed pointer add per dispatch; whitespace and structural delimiters are never re-scanned in stage2; only `parse_string` and `parse_number` re-touch source bytes for the primitive's own content. Sonic-rs achieves the equivalent fusion through LTO and hot-path inlining (`sonic-rs-v2/PROFILE-REPORT.md` hot-leaf count = 1 across every corpus × path). yyjson beats simdjson on twitter (3687 vs 2923 MiB/s, 0.91 vs 1.142 c/B) without SIMD by force-inlining the parse driver into one ~18 KiB hot function (`skinny/profile/yyjson/PROFILE-REPORT.md`). The fresh reprofile adds the current local evidence: `random` and `unicode_escapes` are dominated by `runtime::generated_json::generated::parse_value_at`; `update-center` also shows sparse-flag capacity and allocation-growth costs. The parser computes a tape projection, then still walks raw source. That is the defect this section closes.

The codegen contract is normative; see `COMPILER.md` §3.3 for the lowering pattern. The substrate-side requirement is one additive field on `ValueRef`:

```rust
pub struct ValueRef<'doc, 'input, K> {
    pub tape: &'doc Tape<'input>,
    pub cursor: u32,                   // index into Tape::offsets, not byte position
    _kind: PhantomData<fn() -> K>,
}
```

`(TapeId, cursor, event_kind_or_payload_class)` is the canonical identity tuple. For one-byte-decidable grammars, `event_kind` derives from `source[offsets[cursor]]`; for grammars with stored payload classes, the same cursor indexes the event tape's side facts. Sink-only direct-to-struct paths have no document identity because they do not retain a queryable document. **The per-rule shape selection is cost-model-derived**: no user-visible directive, no workspace metadata declaration, no `@runtime` directive. Per Lock 10 auto-detect mandate:

```rust
LayoutFacts.backend_shape[rule_id]: BackendShape  // see ARCH §7.3
// derivation: passes::recognizers mines first-set disjointness, @error(recover)
// presence, @host fn parse-time-decoded presence, @layout scope presence,
// target-feature admissibility, retained-document need; emits one of
// EagerTape / OffsetTape / EventTape / SinkOnly / CollapsedStage automatically
// per the 8-step algorithm at ARCH §7.3.
```

Lens N classification: **ADDITIVE-MECHANICAL**. The substrate gains a typed cursor adapter and a materialization-plan enum; `DocumentView` is unchanged; the payload arena is unchanged. The BIR alphabet is unchanged (20 variants); the lowerer at `crates/codegen/src/lower/rust.rs` reads `LayoutFacts.backend_shape[rule_id]` and emits one access pattern for the existing `Alt { mode: Dispatch }` variant. V1 closure cost: ~650-900 LOC across `runtime::tape`, `bbnf-codegen`, `bbnf-simd`, and `parse-that/{string,unicode,number}`. The exact wave contract lives in `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`.

The SOTA-BEAT route stacks four levers, each falsifiable independently:
1. **Lock 15 enforcement** (`lto=fat` + force-inline hot leaves + ~20 KiB i-cache budget): catches the `lto=thin` regression and applies the yyjson lever; closes ~30-40% of the gap.
2. **Phase 1 NEON intrinsic upgrade** (`vqtbl4q_u8` + Validark `vshrn_n_u16` movemask + `vld1q_u8_x4` quad-load + `vextq_u8` cross-chunk byte-shift + `udot` for digit-block MAC): ~70-150 LOC in `bbnf-simd/aarch64/`; closes scan stage from ~2.08 c/B to ~0.9 c/B.
3. **Phase 2 typed-event lowering** (`LayoutFacts.backend_shape` cost model + `Alt { Dispatch }` event-cursor lowerer + HasEsc flag + lazy borrow + capacity policy): closes `parse_value_at` domination on `random` and `unicode_escapes`.
4. **Phase 3 x86_64 AVX-512BW/VBMI2 backend + Phase 4 collapsed-stage backend** (cost-model auto-selects `CollapsedStage` when target features and grammar facts admit): closes toward asmjson-class on x86_64 hardware, with strict/permissive result planes separated.

All four extend the same substrate; no Lock 1 amendment is needed. The four-perturbation substrate-rejection cluster (REDRESS 16/17/18/20) empirically bounds the substrate-amendment search space.

---

## 2. Payload arena policy

The payload arena's job is to hold scalar payloads that **cannot** be recovered cheaply from a `(start, end)` source slice. JSON's specific cases:

| Scalar kind | Storage decision | Rationale |
|---|---|---|
| `null` | Inline. Payload class = `INLINE_BOOL_NULL`. `payload_or_skip` is unused. | Zero data; recovering it costs a kind check. |
| `true` / `false` | Inline. Payload class = `INLINE_BOOL_NULL`. | One bit of state encoded in `kind` itself (`JsonTrue` vs `JsonFalse` are distinct kinds). |
| Numbers (no escapes by definition; no normalisation needed) | **Lazy.** Payload class = `INLINE_NUMBER_FAST`. The tape token records `(start, end)`; the f64 / i64 is parsed only on `JsonNumber::as_f64()` / `as_i64()`. | Eager parse forces a `strtod`-class call per number on the hot path; sonic-rs and simdjson both lazy-parse. The bench measures structure-parse throughput; number materialisation is amortised across consumers. |
| Strings, escape-free (the common case) | **Borrow source slice.** Payload class = `INLINE_STRING_BORROW`. `flags.PAYLOAD_BORROWS_SOURCE = 1`. The tape token's `start`/`end` cover the bytes between (but not including) the surrounding `"`. | Zero copy. `JsonString::as_str()` returns `&'input str` reconstituted from `source[start..end]`. |
| Strings with escapes (`\n`, `ÿ`, etc.) | **Lazy unescape.** Payload class = `INLINE_STRING_BORROW`, `flags.PAYLOAD_NEEDS_NORMALIZE = 1`. The borrowed slice still spans the raw escape sequence. `JsonString::as_str()` returns `Cow<'input, str>`; the unescape lazily allocates only on the unescape path. | Eager unescape allocates for every string, even ones never read. The lightning-css model (`Cow`) is the V1 default per Lock 9. |
| Direct `SinkOnly` string outputs | **Source hook.** The generated parser passes `(raw, needs_unescape)` to the sink. Default hooks share the lazy `Cow` policy above; SOTA sinks may replace them only with a measured field-layout materializer or same-loop SinkOnly/CollapsedStage primitive that beats the default allocate-then-contiguous-hash baseline. | Keeps direct sinks on the same source-span substrate as retained views. The attempted generic no-allocation decoded visitor, later exact decoded-stats sink, and quote-source one-pass streaming hasher all regressed Unicode direct rows and are rejected; no parser-side eager decode, no sink-local decoded hash helper, and no arena write in the timed direct path. |
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
    pub fn write_count(&self) -> u64 { self.writes as u64 }

    #[cfg(any(test, feature = "bench-counters"))]
    pub fn allocation_count(&self) -> u64 { self.allocations as u64 }
}

impl<'input> Tape<'input> {
    /// Bench-only: total payload-arena writes recorded during parse.
    /// Zero for the JSON hot path; falsifies the zero-arena pillar if non-zero.
    #[cfg(any(test, feature = "bench-counters"))]
    pub fn payload_arena_writes(&self) -> u64 { self.payloads.write_count() }

    /// Bench-only: total payload-arena allocations recorded during parse.
    /// Zero for the JSON hot path.
    #[cfg(any(test, feature = "bench-counters"))]
    pub fn payload_arena_allocations(&self) -> u64 { self.payloads.allocation_count() }
}
```

These two `Tape<'input>` accessors are the surface BENCH §3.4 calls. The `PayloadArena` methods remain crate-private hooks; the `Tape` methods are the public falsifiability gate per Lock 1.

The skinny commits to **zero arena allocations and zero arena writes on the JSON hot path.** Every scalar lives inline (booleans/null) or as a borrowed source slice (numbers, strings). The arena is present because the substrate is grammar-neutral and other grammars (e.g. CSS L4 colour-function intermediates) require it; for JSON it stays empty. BENCH must assert `Tape::payload_arena_writes() == 0` and `Tape::payload_arena_allocations() == 0` for Track 1 and Track 2 on twitter/citm_catalog/canada under the bench-counters feature.

Iteration evidence confirms zero pressure: `skinny/RESULTS.md` "Notes" reports `Track 1 0/0 writes/allocations; Track 2 0/0 writes/allocations` across all three corpora; the `Option<Box<PayloadArena>>` conditional is therefore dormant and remains a V1-deferred gate that only reactivates if a future grammar perturbs the empty-path assumption. The host-fn-free posture is FAITHFUL-conditional on V1 keeping `JsonString::as_str()` lazy per `skinny/REDRESS.md` item 19: if V1 emits a parse-time `decode_json_string_to_arena` host call, the zero-arena claim breaks and the cut becomes MASKING (eager-decode probe lands at 57.6% / 77.2% / 81.9% of Track 1 across twitter/citm_catalog/canada per RESULTS.md).

Lifetime relationship: `PayloadArena` is owned by `Tape<'input>`. Anything stashed in it lives at least `'doc`. The skinny never stashes anything that depends on `'input`, so it does not need an arena-lifetime parameter.

---

## 3. SIMD scan integration contract

The exact structural alphabet for JSON is the seven punctuation bytes `{ } [ ] : ,` plus quote `"`. Layout whitespace (`space \t \n \r`) is a skip class consumed by parser boundary loops, not a structural token emitted into the parser index. The implementation exposes two SIMD products:

- `StructuralIndex`: the bench/floor product, carrying exact structural offsets for the punctuation/quote alphabet only.
- `JsonParseIndex`: the parser product, carrying structural offsets plus string escape/control candidate columns. It deliberately does not carry whitespace bytes or a duplicate structural-byte column; both variants were measured as extra parser-index work that the hot path did not recover.

The structural-only scanner may take a no-quotes fast path: if a SIMD stripe is outside a string and contains no quote bytes, it classifies punctuation structurals without paying escape/parity bookkeeping. SK-V5 redress item 56 implements this as a grammar-neutral structural+terminator classifier: the runtime supplies the structural table and quote terminator, then emits set-bit positions through `bulk_emit_positions_64` into reserved offset capacity. This preserves exactness because string state cannot change inside such a stripe.

### 3.1 The dispatch table

```rust
// Lives in runtime/src/tape/scan.rs (substrate-level glue;
// the bbnf-simd crate owns the kernels themselves).
pub fn structural_scan_into(
    input: &[u8],
    out: &mut StructuralOffsets,
) -> ScanReport {
    #[cfg(target_arch = "x86_64")]
    {
        if is_x86_feature_detected!("avx2") {
            return bbnf_simd::x86_64::avx2::scan_json_structurals(input, out);
        }
    }
    #[cfg(target_arch = "aarch64")]
    {
        if std::arch::is_aarch64_feature_detected!("neon") {
            return bbnf_simd::aarch64::scan_json_structurals(input, out);
        }
    }
    bbnf_simd::scalar::scan_json_structurals(input, out)
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
}
```

One offsets array is the canonical feed. The parser reads `input[offset]` when it needs the structural byte; carrying a parallel byte vector measured as throughput-negative because it adds one write and one extra read stream per structural. `JsonParseIndex` extends the offsets array with string escape and string control candidate arrays for string validation. `StructuralIndex` remains the lean scan floor; `JsonParseIndex` is allowed to be heavier only where the full parse row recovers that cost.

### 3.3 Prefilter vs verifier route

JSON's structural alphabet is **exact**, not a prefilter, by Lock 8 / `restart/ARCHITECTURE.md:951`:

> `SimdScan` Exact mode must match scalar offsets; prefilter mode emits candidates only.

The substrate accepts `SimdScanMode::Exact` for JSON structural-alphabet scans. Under `Exact`, no verifier route is needed — the SIMD output IS the answer. The substrate still demands the **scalar parity hash** (§3.4) before any tape token is emitted from the SIMD output.

For grammar-specific content scans (e.g. delimited-string close detection with backslash-escape parity, regex-class scans, CSS unquoted-token closes), the substrate exposes `SimdScanMode::Prefilter`: SIMD finds candidate bytes, a scalar verifier walks each candidate to decide whether to emit. Tape emission happens only after the verifier accepts. The per-grammar verifier predicate is owned by COMPILER (the JSON skinny installs a closing-quote verifier that checks for an even number of `\` runs preceding `"`); the substrate provides the dispatch shape only. The skinny implements both Exact (structural) and Prefilter (grammar content) modes because both are on the JSON hot path.

### 3.4 Scalar parity hash

The bench harness owns the parity-check protocol; the substrate exposes the hook:

```rust
pub struct ScanReport {
    pub kernel_id: KernelId,        // Avx2 | Neon | Scalar
    pub offsets_count: u32,
    /// xxhash64 of structural offsets and the input bytes at those
    /// offsets. Computed by the kernel; the
    /// scalar kernel computes the same hash. Bench compares.
    pub parity_hash: u64,
}
```

If `parity_hash(SIMD) != parity_hash(scalar)` over the same input, the SIMD path is treated as broken and the kernel demoted to scalar for that bench row. The substrate does NOT silently fall back at runtime — the demotion is bench-time only; runtime trusts SIMD once parity has been validated against the corpus.

### 3.5 Throughput targets

Per `restart/ARCHITECTURE.md:1519`:

| Kernel | Target |
|---|---|
| AVX2 (x86_64) | ≥ 56000 Mbps structural scan throughput. |
| NEON (M-series ARM) | ≥ 40000 Mbps structural scan throughput. |
| Scalar fallback | No throughput target; correctness only. |

The bench owns the actual measurement methodology; the substrate guarantees the SIMD output is byte-faithful to scalar.

### 3.6 Token-economy materialization gate

The bench report must publish, per corpus, token count, logical tape bytes, allocated tape bytes, both tape/input ratios, payload bytes, pair-token count, open-container count, close-token count, scalar-token count, and sibling-skip count for Track 1 and Track 2. These numbers are part of the Lock 1 premise: if throughput is below sonic while structural scan and arena counters pass, the materialization statistics identify whether close tokens, pair tokens, skip patching, allocation capacity, or sealing are the remaining substrate cost.

Close-token emission and pair-token emission are therefore load-bearing skinny choices, not implementation trivia. JSON skinny adopted close-token elision after before/after measurement (`skinny/REDRESS.md` item 13): open container tokens carry end spans and subtree skips, while the materialization report still counts observed closing delimiters as `closes` so the row can audit source shape. Pair-token fusion was measured and rejected for the canonical path because it reduced token count but regressed or failed to improve Track 1 Mbps (`skinny/REDRESS.md` item 16). A 12-byte skipless-token perturbation (`kind + flags + start + end`, deriving subtree skips from spans) was also measured and rejected as canonical because it was mixed: twitter regressed, citm improved, and canada stayed within noise (`skinny/REDRESS.md` item 18). A dispatch-table alternate was measured and rejected (`skinny/REDRESS.md` item 17 — the real function-pointer table regressed key corpora; canonical Rust `match` dispatch wins).

After these measured perturbation rejections plus close-token adoption, the lazy-offset tape became the canonical measured JSON substrate for the historical triad. The accepted path keeps explicit close offsets and separators in the structural projection, uses sparse flags and direct spare-capacity offset writes, and passes twitter/citm_catalog/canada. The expanded gate remains overall G / NoGo, so removing, fusing, or replacing pair tokens, separators, capacity policy, or cursor representation still requires a before/after bench row against the expanded corpus.

---

## 4. Direct-to-struct overlay

Direct-to-struct is the typed projection layer. Per Lock 1 and PASS-2 §2 commitment 3, direct values are NOT a second authoritative tree — they are **typed views over the same tape token stream**.

### 4.1 The typed-view shape for JSON

```rust
// All generated by codegen; substrate owns the trait shape only.

#[derive(Copy, Clone)]
pub struct JsonRoot<'doc, 'input: 'doc> {
    cursor: ValueRef<'doc, 'input, super::JsonRootKind>,
}

impl<'doc, 'input> JsonRoot<'doc, 'input> {
    pub fn value(self) -> JsonValueRef<'doc, 'input> {
        // The root is a single value; token index 0 carries the JsonRootKind
        // marker, index 1 carries its payload value.
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
    cursor: ValueRef<'doc, 'input, super::JsonObjectKind>,
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
    cursor: ValueRef<'doc, 'input, super::JsonStringKind>,
}

impl<'doc, 'input> JsonString<'doc, 'input> {
    pub fn as_str(self) -> Cow<'input, str> {
        let tok = self.cursor.tape.tokens[self.cursor.index as usize];
        let raw = &self.cursor.tape.source[tok.start as usize..tok.end as usize];
        if tok.flags.contains(TokenFlags::PAYLOAD_NEEDS_NORMALIZE) {
            Cow::Owned(unescape_json(raw))
        } else {
            // The SIMD structural scan classifies punctuation/quote bytes only;
            // it does not establish UTF-8 well-formedness. UTF-8 prevalidation
            // is run outside the timed region (see §1.3 `parse(&'a str)`
            // contract — the public Grammar API takes `&str`, so the caller
            // already discharged validity). For the byte-slice entry point,
            // the substrate uses checked decoding.
            Cow::Borrowed(std::str::from_utf8(raw).expect("UTF-8 prevalidated"))
        }
    }
}

#[derive(Copy, Clone)]
pub struct JsonNumber<'doc, 'input: 'doc> {
    cursor: ValueRef<'doc, 'input, super::JsonNumberKind>,
}

impl<'doc, 'input> JsonNumber<'doc, 'input> {
    pub fn as_f64(self) -> f64 {
        // Lazy parse from source slice. V1 receiver: `crates/parse-that/src/num.rs`
        // (the parse-that number kernel installed post-skinny); for the skinny,
        // `std::str::FromStr` per `skinny/REDRESS.md` item 12 (parse-that-regex
        // already tightened the number scanner).
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

**Identity proof on paper.** Every `ValueRef` is constructed by code that has a `&Tape<'input>` in scope; `tape.id` is fixed for the parse. `index` comes from one of three sources: zero (root), an arithmetic step (`i + 1` for "next sibling at scalar", or `i + 1 + skip` for "next sibling past container"), or a sub-cursor returned by a typed projection method. Each arithmetic step lands on a token; `payload_class` is read from `tokens[index].flags & PAYLOAD_CLASS_MASK`. Hence `(tape.id, index, payload_class)` is determined by `(tape, index)` alone, and `tape` cannot mutate after parse because the committed `Vec<TapeToken>` is private and exposed only as `&[TapeToken]`. Identity is stable.

**No second tree.** There is no separate AST; `JsonValueRef` is `Copy` and contains only `(tape, index, kind-marker)`. Visitors and the bench harness share identity by construction.

---

## 6. Visitor entry

The full V1 carries `Visitor`, `VisitMut`, `VisitTypes` bitflag pruning, and a generated walker. The skinny carries the read-only walker only.

```rust
// runtime/src/visitor/mod.rs (substrate-level trait shape).
pub trait Visit<'doc, 'input: 'doc, K> {
    fn visit(&mut self, node: ValueRef<'doc, 'input, K>);
}

// Generated for JSON: the skinny ships a single-method visitor surface that
// drives the traversal cost the BENCH "read-twice" path needs to expose.
// Multi-method dispatch (visit_object / visit_array / visit_string / ...) is
// deferred to V1 PASS-3 §3; the multi-method walk_* defaults are a codegen
// artefact, not substrate trait obligations.
pub trait JsonVisitor<'doc, 'input: 'doc> {
    fn for_each_value(&mut self, value: JsonValueRef<'doc, 'input>);
}

// The skinny provides a free function that drives traversal against any
// `JsonVisitor`, so the visitor surface does not embed the walk control flow
// as a default-method tree. V1 may inline this as default methods if the
// per-kind hook surface is needed.
pub fn walk_json<'doc, 'input, V: JsonVisitor<'doc, 'input>>(
    root: JsonRoot<'doc, 'input>,
    visitor: &mut V,
) {
    fn rec<'doc, 'input, V: JsonVisitor<'doc, 'input>>(
        v: JsonValueRef<'doc, 'input>,
        visitor: &mut V,
    ) {
        visitor.for_each_value(v);
        match v {
            JsonValueRef::Object(o) => {
                for member in o.iter() { rec(member.value(), visitor); }
            }
            JsonValueRef::Array(a) => {
                for child in a.iter() { rec(child, visitor); }
            }
            _ => {}
        }
    }
    rec(root.value(), visitor);
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
| `@host fn` decode-string call | Per-grammar `host_decode` registry; V1 emits a lazy `decode_json_string` call from `JsonString::as_str()` | **FAITHFUL-conditional on V1 keeping decode lazy.** `skinny/REDRESS.md` item 19 measured the host-call split: dispatch overhead is fine (`host_call_dispatch_overhead` 0.7-0.7 ns / call, PASS ≤50 ns); but parse-time eager decode is MASKING (`host_call_eager_decode` 57.6% / 77.2% / 81.9% of Track 1 across twitter/citm_catalog/canada). The host-fn-free skinny remains FAITHFUL only if V1 emits `JsonString::as_str` as a lazy host call (matching the `Cow<'input, str>` model in §4.1); if V1 emits a parse-time `decode_json_string_to_arena` host call, the zero-arena claim at §2 breaks and the cut becomes MASKING. |

The skinny substrate keeps **only** what the parse-throughput SOTA test requires: offset tape, structural SIMD scan, payload arena (empty for JSON), typed projection, snapshot identity, read visitor. The omitted features are orthogonal axes (mutation, incremental reuse, recovery, multi-grammar visitors) that do not contribute to the throughput row. The lazy-offset substrate has been measured against the historical triad and passes; the expanded gate remains overall G / NoGo, while the focused Canada structural floor is restored by SK-V5 redress item 56. The remaining work is single-substrate event/tape consumption, string/Unicode/number primitive closure, and capacity policy, all without reintroducing the rejected alternates.

---

## 8. Hand-coded JSON parity contract (delivered to BENCH)

The BENCH agent will hand-code a JSON parser against this substrate to establish the parity-floor side of the dual-track measurement. The substrate exposes for that purpose:

```rust
// Lives in `crates/bbnf-bench/src/track2/json.rs` (BENCH-side, not
// substrate-side). Lock 14 forbids per-grammar entry points in the generic
// `runtime/src/tape/` crate. The BENCH agent imports `TapeBuilder` from the
// runtime crate and composes `build_tape_for_json` against the substrate
// surface below.

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
  lib.rs                                 // ≤ 100 LOC (re-export surface only)
  tape/
    mod.rs        // Tape, TapeId, public re-exports    // ≤ 150 LOC
    token.rs      // TapeToken, NodeKindId, TokenFlags  // ≤ 200 LOC
    builder.rs    // TapeBuilder, push_token            // ≤ 250 LOC
    payload.rs    // PayloadArena                       // ≤ 150 LOC
    scan.rs       // structural_scan_into dispatch      // ≤ 150 LOC
    view.rs       // ValueRef<'doc, 'input, K>, AnyKind // ≤ 200 LOC
  visitor/
    mod.rs        // Visit trait                        // ≤ 100 LOC
  grammars/
    json/
      mod.rs           // JsonDocument, JsonRoot, surface  // ≤ 200 LOC
      generated.rs     // emitted by codegen; skinny fixture lives in
                       // fixtures, not here              // ≤ 600 LOC
      view.rs          // typed views (JsonObject, ...)  // ≤ 400 LOC
      visitor.rs       // JsonVisitor (hand-stubbed)     // ≤ 150 LOC
```

LOC ceilings echo `restart/skinny/WORKSPACE.md` §2's 2 000-line `runtime` crate cap. Generated `generated.rs` carries the highest budget because codegen output is denser; hand-written files target the lower limits to keep review-friction tractable.

The WORKSPACE agent owns Cargo.toml shape and crate boundaries; this layout is what SUBSTRATE asks WORKSPACE to land.

---

## 10. Open questions surfaced for the orchestrator

These are orthogonal to the historical triad pass, but not to the expanded SOTA-BEAT gate: the iteration has measured the eager-tape ceiling, implemented lazy-offset (§1.5), and rejected the non-canonical alternates in REDRESS-25. The items below are either V1 axes the bench has not yet exercised or skinny SOTA-BEAT items that now have a concrete receiver in the SK-V3 implementation packet.

- **`payload_or_skip` union vs split fields.** The 16-byte token assumes a discriminated union. If bench shows the payload-class branch on every token-walk dominates (orthogonal to the lazy-offset axis), the full V1 may pay 24 bytes for a split. Decision deferred to bench.
- **NodeKindId width.** `u16` is comfortable for JSON (≤16 kinds) and for the V1 grammar set (CSS L4 has the largest kind table; ~512 kinds estimated). If a future grammar exceeds 65 535 kinds, the field widens to `u32`, blowing the 16-byte token. Acknowledged; not a JSON skinny risk.
- **`Tape` vs `Arc<Tape>`.** The skinny uses `Tape<'input>` borrowed by `&'doc`. Multi-thread sharing of a parsed tape (a feature sonic-rs supports) requires `Arc`. The skinny does not exercise this; full V1 may decide on `Arc<Tape>` in `OwnedDocument` form.
- **Whitespace-skip token policy.** The skinny does not emit whitespace tokens, and the parser parse-index does not carry whitespace bytes. Whitespace is consumed by caller-owned boundary loops and dropped. This matches the JSON SOTA target shape; CSS will need a different policy under `@layout`. Substrate-level: whitespace handling is per-grammar at codegen, not substrate-level.

The 12-byte skipless-token open question of the prior draft has been measured and rejected (`skinny/REDRESS.md` item 18); see §1.5 for the lazy-offset substrate amendment surface that the iteration evidence escalated from this list to a primary §1 surface.

The bench result drives any of these to a commit.
