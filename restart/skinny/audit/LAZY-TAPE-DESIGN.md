# Lazy-Tape Substrate Redesign — Amendment Proposal

> **[SUPERSEDED BY IMPLEMENTATION — 2026-05-12 FINAL GATE]** This proposal was implemented per §10 (~860 LOC across `skinny/crates/runtime/src/tape/{offsets,assembler}.rs` + `codegen/src/lower/rust.rs` + `grammars/json/view.rs`) and initially re-benched through Wave 1/2 at outcome G. That was an intermediate result, not the final disposition. The current measured gate in `skinny/RESULTS.md` is overall outcome C / Go: twitter C / GO at Track 1 21552 Mbps, Track 2 18833 Mbps, sonic-rs 19062 Mbps; citm_catalog and canada classify A / GO. Lazy-offset tape is therefore part of the measured winning substrate, after sparse flags, direct spare-capacity offset writes, cold errors, SWAR digit/plain-string paths, delimiter fusion, `parse_value_at`, short plain-string fast path, and Track 2 inline parity. This document is preserved as the historical proposal and falsifiability record; do not read its outcome-G/refutation language below as current status.

**Audience.** Skinny v2 + V1 Lock 1 amendment surface. Citable, surgical, with verbatim before/after spec edits where appropriate. Produced after three iterations of empirical evidence on the eager-tape substrate; this document does NOT relitigate microarchitectural perturbations already measured and rejected.

**Status.** SUPERSEDED. Original design-proposal status preserved below for the audit trail.

---

## §1 Empirical premise

Three iterations of the skinny JSON bench against three corpora (twitter, citm_catalog, canada) consistently land outcome G (NO-GO) at the substrate-ceiling row. Verbatim per `skinny/RESULTS.md:5-7`:

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | Track 1 / sonic | Track 2 / sonic |
|---|---:|---:|---:|---:|---:|
| twitter | 12470 | 10063 | 18440 | 67.6% | 54.6% |
| citm_catalog | 12246 | 11547 | 23075 | 53.1% | 50.0% |
| canada | 8895 | 8177 | 12021 | 74.0% | 68.0% |

Aggregating: Track 1 lands ~12.5K Mbps geometric mean (range 8.9K-12.5K), Track 2 lands ~12.0K Mbps geometric mean (range 8.2K-11.5K), sonic-rs lands ~21K Mbps geometric mean (range 12.0K-23.1K). The substrate ceiling sits at roughly **60-65% of sonic-rs** on small/medium corpora and **~70% on canada**. The structural-scan microbench passes (canada at 65689 Mbps; floor 40000 Mbps per `RESULTS.md:40`), the arena counters are zero (`RESULTS.md:34,36,38`), and the host-call dispatch microprobe passes (~0.7ns/call per `RESULTS.md:13,19,25`). The eager-decode probe is MASKING on all three corpora (`RESULTS.md:14,20,26`), which forecloses the eager string-decode V1 design without separate ratification.

Per `skinny/REDRESS.md:158-188`, two architectural amendments have been MEASURED and REJECTED inside the eager-tape envelope:

- **Dispatch-table-as-canonical** (REDRESS §17): a real 256-entry function-pointer table was implemented in both Track 1 generation and Track 2, measured, and reverted because it regressed key corpora. The canonical lowering stays Rust `match`.
- **Narrower 12-byte token** (REDRESS §18): a `kind + flags + start + end` shape removing `payload_or_skip` and deriving subtree skips from spans saved memory but produced mixed throughput (twitter regressed, citm improved, canada within noise). Canonical stays 16 bytes.

Pair-token fusion was also measured and rejected as token-count-positive but throughput-negative (`REDRESS.md:273-276`). A duplicate structural-byte column on the parse index was measured and removed (`REDRESS.md:201-205`).

**Architectural diagnosis.** The eager-tape substrate writes ~16-byte tokens during the same typed walk that sonic-rs performs as a lazy read against the structural index. Tape materialization stats per `RESULTS.md:34-39`:

- twitter: 40605 tokens; 649680 logical tape bytes (1.03× input); 1064272 allocated bytes (1.69× input)
- citm_catalog: 89517 tokens; 1432272 logical bytes (0.83× input); 2351040 allocated (1.36× input)
- canada: 167196 tokens; 2675136 logical bytes (1.19× input); 3572160 allocated (1.59× input)

Each token is a write-stream that sonic-rs avoids because sonic-rs's parser-AST shape IS its structural index plus a typed walker that reads bytes from source on demand. The bench gap therefore is not codegen overhead (Track 1 / Track 2 is 0.81-1.24×; codegen is within the 1.10-1.15× expected band) and not SIMD throughput (structural scan beats its floor by 1.6×). It is **substrate materialization cost**: ~16 byte-writes per structural offset across 40K-167K offsets per corpus.

The remaining honest architectural route — identified by the user — is **lazy materialization**: the structural index IS the tape, materialization happens on typed-view access, no separate token stream is emitted.

---

## §2 Proposed substrate redesign

### 2.1 New `Tape<'input>` layout

The structural-index buffer becomes the tape. The eager `tokens: Vec<TapeToken>` field is deleted. The payload arena is retained because V1 substrate is grammar-neutral (CSS L4 colour-function intermediates require it); for JSON it stays empty as today.

```rust
// runtime/src/tape/mod.rs — lazy mode
pub struct Tape<'input> {
    /// Source bytes the tape borrows from. JSON is byte-clean.
    source: &'input [u8],

    /// Structural offsets: one u32 per structural byte. This IS the tape.
    /// Replaces the eager `tokens: Vec<TapeToken>`.
    /// For JSON, density bound ~1 in 6-8 input bytes; pre-allocated to
    /// `input.len() / 8`. canada is offset-dense (numeric arrays) at
    /// ~167K offsets / 2.25MB input ≈ 1 in 13.
    offsets: Box<[u32]>,

    /// Parser-grade string escape/control candidates from JsonParseIndex.
    /// Retained as a parser auxiliary; sonic-rs has the analog as part of
    /// its string-validation route.
    string_candidates: Box<[u32]>,

    /// Payload arena. Empty on JSON hot path; preserved for grammar
    /// neutrality (CSS L4 etc).
    payloads: PayloadArena,

    /// Snapshot identity. One TapeId per parse, monotonic per process.
    id: TapeId,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct TapeId(pub u64);
```

`offsets: Box<[u32]>` (sealed at parse end) replaces `tokens: Vec<TapeToken>`. The seal cost is one shrink-to-fit `Vec::into_boxed_slice()` at parse close; the over-reservation that motivated the private-Vec move in the eager design (`REDRESS.md:295-301`) is a smaller absolute number here (one u32 per offset vs 16 bytes per token), so the boxed-slice form is acceptable. If empirical measurement shows the shrink-copy is non-trivial, the same private-Vec semantic sealing is available as a one-line revert.

### 2.2 New `ValueRef<'doc, 'input, K>` layout

The token-stream index is replaced by an offset-array cursor. The kind tag is computed lazily, not stored.

```rust
// runtime/src/tape/view.rs — lazy mode
#[derive(Copy, Clone)]
pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind> {
    tape: &'doc Tape<'input>,
    /// Index into `tape.offsets`. `u32` because input.len() <= 2^32 (V1
    /// hot path) and `offsets.len() <= input.len()`.
    cursor: u32,
    _kind: PhantomData<fn() -> K>,
    _input: PhantomData<&'input [u8]>,
}
```

`cursor` indexes `tape.offsets`. The node-kind is recovered by `source[offsets[cursor]]` plus the §5 discriminator. `PhantomData<fn() -> K>` keeps `ValueRef` `Copy`/`Send`/`Sync` regardless of `K`'s auto traits, preserving the existing skinny semantics (`SUBSTRATE.md:123`).

The 16-byte `TapeToken` is **deleted entirely** from JSON-mode substrate. For eager-mode grammars (CSS L4, BBNF-self, Sheets) the token row remains canonical; see §4 (Lock 1 amendment) and §8 (backwards-compat).

### 2.3 Identity invariant restatement

Currently `(TapeId, node id, payload class)` per `ARCHITECTURE.md:1409`. In lazy mode:

- `node id` becomes `cursor` (u32 index into `offsets`).
- `payload class` becomes `kind_at_cursor` — the computed JSON node kind, derived from `source[offsets[cursor]]` plus a small lookup table.

Steelman check that the §5 SUBSTRATE invariant survives:

> Every public node has one `(TapeId, node id, payload class)` identity.

Under lazy mode, `(TapeId, cursor, computed_kind)`:

- `TapeId` is fixed per parse (unchanged).
- `cursor` is a stable u32 across all public construction sites: root (cursor = 0), `JsonObject::iter` (cursor advances through member-key offset and member-value offset), `JsonArray::iter` (cursor advances through element offsets), every typed projection method.
- `kind_at_cursor` is a pure function of `(tape.source, tape.offsets[cursor])`. `tape.source` and `tape.offsets` are immutable after seal. Therefore `kind_at_cursor` is determined by `(tape, cursor)` alone, mirroring the eager argument that `payload_class` is determined by `(tape, index)` via `tokens[index].flags`.

**Identity proof, restated.** Every `ValueRef` is constructed with `&Tape<'input>` in scope; `tape.id` is fixed for the parse. `cursor` comes from one of three sources: zero (root), arithmetic step (`cursor + 1` to advance over a member-separator or scalar, or a precomputed sibling-skip target lookup), or a sub-cursor from a typed projection. Each arithmetic step lands on an offset; `kind_at_cursor` is computed from `source[offsets[cursor]]`. Hence identity is stable.

**Sibling skip in lazy mode.** The eager-mode subtree skip stored in the open token's `payload_or_skip` is replaced by depth-tracked traversal: `JsonObject::iter` and `JsonArray::iter` advance cursor-by-cursor with a bracket-depth counter, descending on `{`/`[` and ascending on `}`/`]`. This is O(n) for the subtree, not O(1) skip; see §9 risk register.

### 2.4 `TapeBuilder` becomes `TapeAssembler`

The current `TapeBuilder` (`SUBSTRATE.md:538-548`) is an append-only Vec<TapeToken> writer. In lazy mode, the parse-index Vec<u32> output of Stage 1 IS the canonical content; the assembler's job is to validate it (enforce balanced brackets, well-formed strings, well-formed numbers; this is the verifier-route work that today drives `TapeEmit`) and seal it.

```rust
// runtime/src/tape/builder.rs — lazy mode (replaces TapeBuilder)
pub struct TapeAssembler<'a> {
    source: &'a [u8],
    parse_index: simd_scan::JsonParseIndex,
    payloads: PayloadArena,
    next_id: TapeId,
}

impl<'a> TapeAssembler<'a> {
    pub fn new(source: &'a [u8], payloads: PayloadArena) -> Self;

    /// Verify the parse index against JSON grammar. Returns Err on
    /// unbalanced brackets, invalid escape, malformed number boundary,
    /// etc. The verifier walks the offsets and source, but emits NO tokens.
    pub fn verify(&mut self) -> Result<(), ParseError>;

    /// Seal into a Tape. The parse_index becomes `offsets` +
    /// `string_candidates`; the payload arena is sealed into Tape.
    pub fn finish(self) -> Tape<'a>;
}

pub fn build_tape_for_json<'input>(
    source: &'input [u8],
    payloads: PayloadArena,
) -> Result<Tape<'input>, ParseError> {
    let parse_index = simd_scan::scan_json_parse_index(source);
    let mut asm = TapeAssembler::new(source, payloads);
    // Drive verification using the parse_index; this is the verifier-route
    // work the eager parser folded into TapeEmit. The cost moves from "emit
    // 16 bytes per node" to "walk offsets, verify; emit nothing".
    asm.verify()?;
    Ok(asm.finish())
}
```

The hand-coded JSON parallel parser (Track 2) is rewritten to drive the assembler exactly as codegen will. Both tracks produce a `Tape<'input>` whose content is the parse_index plus the sealed PayloadArena. The parity oracle's token-stream comparison becomes an **offset-stream comparison** (see §3 BENCH amendments).

---

## §3 Concrete spec amendments — skinny side

Per-file, verbatim before/after edits. Each edit is a literal text replacement; the orchestrator can apply them to the spec files mechanically.

### 3.1 `restart/skinny/SUBSTRATE.md`

**§1.1 `TapeToken` block → DELETED for JSON; PRESERVED for eager-mode grammars under §4 amendment.**

BEFORE (`SUBSTRATE.md:26-71`): the 16-byte `TapeToken` struct, `NodeKindId`, `TokenFlags`, payload-class discussion.

AFTER: the section becomes "§1.1 Eager-mode `TapeToken` — preserved for grammars opting into `tape_mode = "eager"`". The struct definition is preserved verbatim; one paragraph added at the top of §1.1:

> **Lazy mode (JSON, CSS scan-class).** Grammars marked `tape_mode = "lazy"` in workspace metadata do not emit `TapeToken`; the substrate skips this section's machinery. Lazy mode uses the `Tape<'input>` layout in §1.2 lazy variant. See §4 typed-views-as-cursor-walks. Eager mode (this section) remains canonical for `tape_mode = "eager"` grammars (CSS L4, BBNF-self, Sheets) where layout, recovery, and recovery-flag-bearing tokens require stored payload classes.

**§1.2 `Tape<'input>` → new layout (lazy variant added; eager variant preserved).**

BEFORE (`SUBSTRATE.md:75-101`): the eager `Tape<'input>` with `tokens: Vec<TapeToken>`.

AFTER: section renames to "§1.2 `Tape<'input>` — owning structural-index stream OR token stream". Two variants per `tape_mode`. Eager variant preserved verbatim. Lazy variant added:

```rust
// Lazy mode (tape_mode = "lazy"; JSON canonical):
pub struct Tape<'input> {
    source: &'input [u8],
    offsets: Box<[u32]>,
    string_candidates: Box<[u32]>,
    payloads: PayloadArena,
    id: TapeId,
}
```

The sealing trade-off paragraph (`SUBSTRATE.md:101`) is extended: in lazy mode, the absolute over-reservation cost is ~4× smaller (4 bytes per offset vs 16 bytes per token), so `Box<[u32]>` is the canonical seal; private-Vec semantic sealing is a one-line revert if the shrink-copy is empirically non-trivial. The bench reports both logical and allocated offset bytes (`offsets.len() * 4` and `offsets.capacity_at_seal() * 4`).

**§1.3 `ValueRef<'doc, 'input, K>` → new cursor shape (lazy variant added).**

BEFORE (`SUBSTRATE.md:106-115`): `ValueRef` with `index: u32` indexing tokens.

AFTER: the `index: u32` field is renamed to `cursor: u32` and documented as "Index into `tape.offsets` (lazy mode) or `tape.tokens` (eager mode); both share the u32 width." The remaining paragraphs (lifetime discriminant, kind families) are preserved.

**§4 Direct-to-struct overlay → typed-views-as-cursor-walks (rewrite under lazy mode).**

BEFORE (`SUBSTRATE.md:325-399`): typed views (`JsonObject`, `JsonArray`, etc.) read `tape.tokens[index]` for kind + flags + spans.

AFTER: in lazy mode, typed views compute kind from `source[offsets[cursor]]`. The specific replacements:

```rust
// JsonString::as_str() — BEFORE (eager):
pub fn as_str(self) -> Cow<'input, str> {
    let tok = self.cursor.tape.tokens[self.cursor.index as usize];
    let raw = &self.cursor.tape.source[tok.start as usize..tok.end as usize];
    // ...
}

// JsonString::as_str() — AFTER (lazy):
pub fn as_str(self) -> Cow<'input, str> {
    // Cursor sits at the opening quote offset. Walk forward through
    // string_candidates to find the matching close quote.
    let cursor = self.cursor.cursor as usize;
    let start = self.cursor.tape.offsets[cursor] as usize + 1;  // skip "
    let end_offset_idx = find_matching_close_quote(
        &self.cursor.tape.offsets,
        &self.cursor.tape.string_candidates,
        cursor,
    );
    let end = self.cursor.tape.offsets[end_offset_idx] as usize;
    let raw = &self.cursor.tape.source[start..end];
    let has_escapes = string_candidates_in_range(
        &self.cursor.tape.string_candidates, start, end,
    );
    if has_escapes {
        Cow::Owned(unescape_json(raw))
    } else {
        Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(raw) })
    }
}
```

`JsonObject::len`, `JsonObject::get`, `JsonObject::iter`, `JsonArray::len`, `JsonArray::iter` all migrate from `tape.tokens[index].payload_or_skip` reads to bracket-depth walks over `offsets[cursor..]`. The "field-cache decisions" table (`SUBSTRATE.md:407-413`) loses the "sibling-skip count" row for JsonObjectOpen/JsonArrayOpen (no longer stored); the table gains a column "lazy-mode walk cost" with O(subtree size) entries for object/array; scalar rows remain O(1).

**§5 Identity invariant → restated (verbatim addition).**

BEFORE (`SUBSTRATE.md:432-437`): identity via `(tape.id, index, payload_class_of(tokens[index]))`.

AFTER: a new bullet inserted at `SUBSTRATE.md:432`:

> **Lazy mode identity.** In `tape_mode = "lazy"` grammars, `(tape.id, cursor, kind_at_cursor)` replaces `(tape.id, index, payload_class)`. The kind is computed via §5 lazy discriminator (`source[offsets[cursor]]` plus the per-grammar lookup). `kind_at_cursor` is a pure function of immutable `(tape.source, tape.offsets[cursor])`; identity is stable by the same construction argument as the eager case.

**§8 Hand-coded parity contract → renamed and reshaped.**

BEFORE (`SUBSTRATE.md:537-548`): `TapeBuilder` API, `push_token`, `build_tape_for_json` returning a token-stream tape.

AFTER: the `TapeBuilder` block is replaced by `TapeAssembler` per §2.4. `push_token` is removed in lazy mode (no tokens to push). `build_tape_for_json` retains its signature; the body now drives `simd_scan::scan_json_parse_index` and the assembler verify/finish steps. The parity contract changes: BENCH parity oracle compares `tape.offsets` byte-equal across Track 1 and Track 2 (since both consume the same `JsonParseIndex` output, parity is essentially automatic; the assembler verification is the load-bearing step).

### 3.2 `restart/skinny/COMPILER.md`

**§3 BIR subset — `TapeEmit` and `DirectBuild` semantics under lazy mode.**

BEFORE (`COMPILER.md:208`): `TapeEmit` appends `(NodeKind, span, payload_slot?)` to the tape.

AFTER: the table row at `COMPILER.md:208` for `TapeEmit` adds a "lazy-mode behavior" column:

> | `TapeEmit` | every node + token event (compiler-generated) | Eager mode: `state.tape.emit(NodeKind, span, payload)`. **Lazy mode: NO-OP.** The parse_index already carries the offset; nothing is appended. |

Similarly `DirectBuild` (`COMPILER.md:209`):

> | `DirectBuild` | every typed-view rule | Eager mode: builds typed view fields pointing at sealed tape token indices. **Lazy mode: builds typed view fields pointing at sealed `offsets` cursor positions.** Field shape unchanged; cursor semantics changed. |

`ValueProject` (`COMPILER.md:210`) is unaffected; it remains a `from -> projection` call.

**§3.3 BIR construction discipline (`COMPILER.md:227-235`).**

A new row appended:

> | Tape and direct-to-struct lower under `tape_mode` from grammar metadata. | `passes::extract` reads `[workspace.metadata.bbnf.grammars.<g>.runtime] tape_mode` and selects the lowering shape (eager-emit vs lazy-noop). |

**§6 codegen::rust path (`COMPILER.md:480-501`).**

The per-BIR-variant lowering table at §6.1 adds lazy-mode rows. For `TapeEmit`:

```rust
// Eager mode (today; preserved verbatim):
state.tape.emit(NodeKind::<kind>, __span, <payload_slot>);

// Lazy mode (new):
// (no emit — the parse_index already has the offset; verifier-only work
// drives state.cursor_advance(__span) for bracket-depth tracking).
state.advance_through_span(__span);
```

For `DirectBuild`:

```rust
// Eager mode (preserved verbatim):
Json<Shape> { <field_1>: <slot_1>, ... }  // slot_1 is u32 token index

// Lazy mode (new):
Json<Shape> { <field_1>: ValueRef { tape, cursor: <slot_1_offset_idx>, ... }, ... }
```

§6.2 emitted file shape (`COMPILER.md:506-515`): `generated.rs` LOC budget drops from ~600 to ~400 because the per-rule `state.tape.emit(...)` calls vanish. `view.rs` LOC grows from ~250 to ~350 because the lazy walker functions (bracket-depth `next_sibling`, `find_matching_close`, `kind_at_cursor`) live there. Net `generated.rs + view.rs` budget drops by ~100 LOC.

§6.3 emitted parser entry sketch (`COMPILER.md:524-561`): the parse function signature stays:

```rust
pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>;
```

Only the body shape changes. Today's body: `simd_scan` → attach index → recursive-descent emit. Lazy body: `simd_scan` → attach index → recursive-descent verify (NO emit) → seal into `JsonDocument`. The verifier is the same control flow as today's emitter minus the `state.tape.emit` lines; codegen emits the same `match` dispatch on the alt-first-byte table, same regex-program calls, same `match state.peek_byte() { b'{' => ..., ... }` shape (per the §3.1 BIR Alt-Dispatch lowering at `COMPILER.md:489`).

### 3.3 `restart/skinny/BENCH.md`

**§1.1 Track 1 contract (`BENCH.md:32-60`).**

BEFORE: "The generated parser consumes `runtime::tape` and `simd-scan` exactly as the hand-coded substrate parser does."

AFTER: same, with appended clarification:

> Under `tape_mode = "lazy"` (JSON canonical), `runtime::tape::scan_json_parse_index` is the single Stage 1 product; both Track 1 and Track 2 consume the resulting `JsonParseIndex` and drive verification via `TapeAssembler`. No `TapeEmit` calls fire in lazy mode.

**§1.2 Track 2 contract (`BENCH.md:62-95`).**

The hand-coded parser layout migrates to the assembler-driven shape. Track 2 source under `crates/bbnf-bench/src/track2/json.rs` is rewritten to:

1. Call `simd_scan::scan_json_parse_index(bytes)`.
2. Drive `TapeAssembler::verify()` with a hand-coded recursive-descent verifier.
3. Return `JsonDocument` wrapping the sealed `Tape`.

The §10.6 substrate-API correspondence checklist (`BENCH.md:1521-1543`) gains rows:

> - [ ] Track 2 drives `TapeAssembler::verify()` (lazy mode); no `state.tape.emit(...)` calls.
> - [ ] Track 2's `parse` returns `JsonDocument<'i>` whose tape is offset-stream-canonical (no parallel tokens vector).

**§1.3 invariants table (`BENCH.md:99-107`).**

BEFORE row:

> | `runtime::tape` substrate | identical | identical |

AFTER (replaces row above):

> | `runtime::tape` substrate (mode-dispatched) | identical | identical |
> | Tape token stream identical (eager-mode grammars) | required if `tape_mode = "eager"` | required if `tape_mode = "eager"` |
> | Tape offsets identical (lazy-mode grammars) | required if `tape_mode = "lazy"` | required if `tape_mode = "lazy"` |

**§10.6 checklist (`BENCH.md:1521`).**

The checklist row "Track 2 records raw scalar spans and leaves the payload arena empty" stays (still true under lazy mode). The row "Track 2 returns JsonRoot<'i>" is augmented: "Track 2 returns JsonRoot<'i> backed by a `Tape<'i>` whose `offsets` array is identical (byte-equal) to Track 1's `offsets` array on the same input."

**§3.4 parity oracle (`BENCH.md:336-362`).**

BEFORE: parity compares `token_stream(&t1) == token_stream(&t2)`.

AFTER: lazy mode replaces `token_stream(&t)` with `tape_offsets(&t)`:

```rust
pub fn assert_parity<'i>(input: &'i str) -> Result<(), ParityError> {
    let t1 = runtime::grammars::json::parse(input)?;
    let t2 = bbnf_bench::track2::json::parse(input)?;
    // Lazy mode: parity over offsets.
    assert_eq!(t1.tape().offsets(), t2.tape().offsets());
    assert_eq!(t1.tape().string_candidates(), t2.tape().string_candidates());
    assert_eq!(t1.payload_arena_writes(), 0);
    assert_eq!(t2.payload_arena_writes(), 0);
    let s1 = serialize_canonical(&t1);
    let s2 = serialize_canonical(&t2);
    if s1 != s2 { return Err(ParityError::Divergence { /* span */ }); }
    Ok(())
}
```

### 3.4 `restart/skinny/INDEX.md`

**Cross-quadrant invariants bullet 3 (`INDEX.md:50`).**

BEFORE: "Tape + direct-to-struct as one substrate. Per Lock 1. No parallel substrate. No OpenFrame clone. SUBSTRATE.md §1; BENCH.md §1.1."

AFTER:

> Tape + direct-to-struct as one substrate, with per-grammar `tape_mode` ∈ {eager, lazy}. Per Lock 1 (as amended). No parallel substrate. No OpenFrame clone. Eager mode emits `TapeToken` stream; lazy mode treats the structural offset array as the tape and computes kind from `source[offsets[cursor]]`. SUBSTRATE.md §1; BENCH.md §1.1.

**Deviation ledger (`INDEX.md:59-67`).**

A new row appended:

> | Lazy-mode tape introduces `tape_mode = "lazy"` for JSON; the eager `TapeToken` stream is omitted in lazy mode and the structural-offset array IS the tape. | SUBSTRATE.md §1.2 lazy variant; COMPILER.md §3 lazy-mode BIR rows; BENCH.md §1.3 invariants table | Lock 1 amendment under §4 of this design; Lock 5 (BIR-only lowerer) survives because the lowerer reads BIR + `tape_mode` metadata. | V1 graduation may flip CSS scan-class grammars to lazy mode after measurement; BBNF-self / Sheets retain eager mode for layout/recovery reasons. |

---

## §4 Lock 1 amendment surface

The current Lock 1 text (`14-LOCKS.md:34`) reads in part:

> Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead. Tape is the greenfield's contiguous parsed-token-stream-with-payload-arena, unioned with direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + index). [...] Plans that resurrect parallel substrates (OpenFrame ladders; columnar SoA; type-ambivalent dual representations) or implement tape with consumer-later sequencing are faults; plans that implement tape properly with same-wave consumer wiring + direct-to-struct union are honoured.

### 4.1 Proposed amendment text (verbatim)

> Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead. Tape's contiguous parsed-stream may be materialised in one of two modes selected per grammar at codegen time:
>
> - **Eager mode** (`tape_mode = "eager"`; canonical for grammars with layout, recovery, or recovery-flag-bearing tokens — CSS L4, BBNF-self, Sheets). Tape is a token stream (`Vec<TapeToken>` private; `&[TapeToken]` public) with stored kind, flags, spans, and payload-or-skip slots per §1.1 SUBSTRATE.
> - **Lazy mode** (`tape_mode = "lazy"`; canonical for SOTA-class structural grammars — JSON; CSS scan probe; future Sheets array fast-path probe). Tape is the structural-offset array (`Box<[u32]>` private; `&[u32]` public) plus auxiliary candidate arrays (string-escape, string-control). Node kind is computed lazily from `source[offsets[cursor]]` plus a per-grammar discriminator function. No `TapeToken` is emitted at parse time; the typed walker (direct-to-struct projections) IS the materialisation.
>
> Both modes share: the same `Tape<'input>` type wrapping the underlying buffer (private to the module; the type itself is mode-monomorphic via a feature/`#[cfg]` switch at the grammar level), the same `ValueRef<'doc, 'input, K>` cursor shape (with `cursor: u32`), the same `DocumentView` trait, the same payload arena, the same `(TapeId, cursor, kind)` identity, the same Visitor trait. There is no parallel substrate; there is no OpenFrame ladder; there is no second tree. Lazy-mode kind computation is a pure function of immutable `(source, offsets)`.
>
> Forbidden surfaces (preserved):
> - No `Vec<OpenFrame>::clone` recovery substrate (the 86.07% samply pathology).
> - No columnar SoA per-field token stripes.
> - No "AST" type that is not a typed view over the tape.
> - No grammar-specific tape variants beyond the two `tape_mode` values; CSS L4 cannot have its own bespoke tape shape.
>
> Per-grammar `tape_mode` is declared in workspace metadata: `[workspace.metadata.bbnf.grammars.<g>.runtime] tape_mode = "lazy"` or `"eager"`. The default is `"eager"` for any grammar that opts in to layout, recovery, or payload-bearing tokens; `"lazy"` is permitted only for grammars whose parse-time work is structurally bounded (verifier-route only; no payload-class storage).

### 4.2 Why both modes coexist under Lock 1

The original Lock 1 forbids "parallel substrates" and "OpenFrame clone" — both of which are about resurrection of pre-restart pathologies (Era V failure mode). Two materialisation modes under one substrate API are NOT a parallel substrate; they are a single substrate with two storage representations gated by per-grammar metadata. The `ValueRef`, `DocumentView`, `Visitor`, and `Tape` types are shared. The lowerer (Lock 5) consumes BIR plus mode metadata; this preserves "BIR-only lowering" because mode is a BIR-input, not a BIR-output. The codegen template branches at emit time on mode; this is the same pattern as branching on `target = "wasm32"` for the Backend trait per Lock 5 amendment.

### 4.3 What this does NOT amend

- **Lock 5 (IR + per-backend lower).** BIR remains the contract. Lazy mode adds zero BIR variants; it changes the lowering of `TapeEmit` and `DirectBuild` to no-ops or cursor-cell-writes under `tape_mode = "lazy"`. The Rust lowerer reads `tape_mode` from extracted grammar metadata; this is a lowering parameter, not a BIR addition.
- **Lock 9 (Slice-borrow primary).** ValueRef still borrows source for `'input` and tape for `'doc`. `parse(&str)`, `parse_in(&str, &Arena)`, `parse_owned` still exist. The owned wrapper holds a `Tape` whose `offsets: Box<[u32]>` is owned but borrows `source` from the wrapper's owned bytes. No lifetime-discriminant change.
- **Lock 14 (Full grammar generalisation).** The substrate has zero grammar-specific code; the kind discriminator function is generated per-grammar from the grammar source, not hardcoded in the runtime crate. The same template generates a different discriminator for CSS-scan if/when CSS-scan opts into lazy mode.

---

## §5 ValueRef semantics — the lazy discriminator function

### 5.1 JSON discriminator (verbatim spec)

For JSON, the kind discriminator function reads one byte from `source[offsets[cursor]]` and matches:

```rust
// runtime/src/grammars/json/view.rs — generated
#[inline(always)]
fn kind_at_cursor<'i>(tape: &Tape<'i>, cursor: u32) -> JsonKind {
    let off = tape.offsets[cursor as usize] as usize;
    match tape.source[off] {
        b'{' => JsonKind::ObjectOpen,
        b'}' => JsonKind::ObjectClose,
        b'[' => JsonKind::ArrayOpen,
        b']' => JsonKind::ArrayClose,
        b',' => JsonKind::ElementSeparator,
        b':' => JsonKind::MemberSeparator,
        b'"' => JsonKind::String,
        b'-' | b'0'..=b'9' => JsonKind::Number,
        b't' => JsonKind::BoolTrue,
        b'f' => JsonKind::BoolFalse,
        b'n' => JsonKind::Null,
        _ => unreachable!(/* verifier guarantees this is never reached */),
    }
}
```

The `unreachable!()` is sound because `TapeAssembler::verify()` rejects any input whose `offsets[i]` does not point at one of the structural alphabet bytes. The parse index by construction (`simd-scan::scan_json_parse_index`) emits offsets only for structural alphabet bytes plus literal-keyword starts (`t`, `f`, `n`) plus number-starts (`-`, `0..=9`) plus the opening quote of strings.

The discriminator function compiles to a 256-entry jump table (LLVM does this for byte-disjoint match arms automatically; see `COMPILER.md:489` where the same pattern is used for alt-dispatch). On M1 Pro this is one cache-line load from `tape.source[off]`, one indirect branch, ~1-2 ns total.

### 5.2 Per-byte cost compared to eager mode

| Operation | Eager mode | Lazy mode |
|---|---|---|
| Read kind | `tape.tokens[index].kind` — one u16 read from token stream, no source touch | `tape.source[offsets[cursor]]` — one u32 read from offsets, one u8 read from source |
| Cache-line touches per node | 1 (the token, 16 bytes within a cache line) | 2 (offsets[cursor] in offset cache line + source byte in source cache line) |
| Computed-vs-stored | stored | computed |
| Per-node cost estimate (M1 Pro warm cache) | ~1 ns | ~2-3 ns |

The eager cost is comparable to one match arm; the lazy cost is one extra cache-line touch into source. **However**, the lazy mode has a 4× bandwidth advantage on the offset array (4 bytes vs 16 bytes), so the offsets walk dominates fewer cache lines per node. Net prediction: lazy mode wins on inputs > L2 (twitter at 616 KB fits, citm at 1.7MB does not, canada at 2.2MB does not).

### 5.3 The kind type

`JsonKind` is a small enum, not stored in any struct field:

```rust
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(u8)]
pub enum JsonKind {
    ObjectOpen, ObjectClose, ArrayOpen, ArrayClose,
    ElementSeparator, MemberSeparator,
    String, Number, BoolTrue, BoolFalse, Null,
}
```

It is a transient computation result. It is NEVER materialised into a Vec or stored in the Tape. The whole point of lazy mode is that kind is computed on demand at view-projection sites; if a consumer never asks for an array's third element, no kind computation fires for that element's cursor.

---

## §6 Identity invariant (steelman)

### 6.1 Preserved

The §5 SUBSTRATE invariant is `(TapeId, node id, payload class)`. Under lazy mode, `(TapeId, cursor, kind_at_cursor)` is the substitute. All four consumers of identity remain coherent:

1. **`DocumentView::root_value`** returns `ValueRef { tape, cursor: 0, .. }`. Identity: `(tape.id, 0, kind_at_cursor(tape, 0))`. For JSON the root is whatever cursor 0 points at (the first structural byte after leading whitespace; in lazy mode the index always starts at the first structural offset).
2. **Typed projections** (`JsonObject::get`, `JsonArray::iter`, `JsonObject::iter`) yield `ValueRef { tape, cursor: c, .. }` where `c` is a u32 cursor into `tape.offsets`. Identity propagates by construction.
3. **Visitor walker** yields cursors with the same identity. The depth-tracked walker (replacing the sibling-skip walker) computes the next cursor by scanning offsets and decrementing a bracket-depth counter; the cursor it yields is still a stable u32 indexing into immutable `tape.offsets`.
4. **Bench harness** (parity oracle) compares `tape.offsets[..]` between Track 1 and Track 2 — automatically equal because both consume the same `simd_scan::scan_json_parse_index` output.

### 6.2 Adversarial: who requires payload class to be STORED, not computed?

Walking the existing skinny: every consumer of `flags & PAYLOAD_CLASS_MASK` is in `view.rs` (typed accessors) and the visitor walker. All of these can route their work through the lazy discriminator instead of a token flag. The `STRING_NEEDS_UNESCAPE` flag is currently stored on the token; in lazy mode, it is computed by walking `string_candidates` in the [start, end) range of the string body. The cost is O(escape-density × string-length); for non-escape strings (the common case) it is O(1) (no candidates in range).

The recovery-flag path (`RECOVERY_KIND` bits in `TokenFlags`) is the one consumer that genuinely requires stored flags. JSON skinny omits recovery (`SUBSTRATE.md:508`), so no consumer fires. For grammars that DO require recovery (CSS L4 layout repair, BBNF-self), `tape_mode = "eager"` is required and stored flags remain canonical. This is the V1 multiplexing point — lazy mode is for grammars whose parse-time work is verifier-only.

### 6.3 Lifetime discriminant (Lock 9)

`ValueRef<'doc, 'input, K>` retains both lifetime parameters. `'doc` borrows `tape` (which owns `offsets` and references `source`); `'input` is the bytes the tape references. In lazy mode the offsets array is owned by the tape, exactly as the tokens array was; the `parse(&str)` collapse to `'doc = 'input = 'a` works identically.

---

## §7 Codegen template changes

### 7.1 Today's eager-mode emit template (preserved verbatim under `tape_mode = "eager"`)

```rust
// COMPILER.md §6.1, TapeEmit row, today's behavior:
state.tape.emit(NodeKind::<kind>, __span, <payload_slot>);
```

This stays canonical for eager-mode grammars (CSS L4, BBNF-self, Sheets).

### 7.2 New lazy-mode emit template (`tape_mode = "lazy"`)

In lazy mode, the equivalent emits **nothing**. The parse-time work is verifier-route only:

```rust
// COMPILER.md §6.1, TapeEmit row, lazy-mode behavior:
// (no emit; the parse_index already has the offset for this node).
// Verifier work fires through state.advance / state.expect_byte calls
// that consume the offset cursor.
```

The codegen template for the JSON parser body under lazy mode collapses by ~30%. Each `parse_<rule>` function does:

1. Read the next offset from `state.parse_index.offsets[state.cursor]`.
2. Match on `state.source[that_offset]` to dispatch alts (the same byte-match pattern as the eager Alt-Dispatch lowering at `COMPILER.md:489`).
3. Recurse into the dispatched alt; the recursive call advances `state.cursor` through the subtree's offsets.
4. Return; the caller's `state.cursor` is at the first offset after the subtree.

No `state.tape.emit(...)` lines fire. No `__span` is captured into a tape token; spans live in the offsets+source addressing (start = offsets[cursor]; end = offsets[next_sibling_cursor]).

### 7.3 New `BBNF.Generate.JsonParser` template shape

The emitted parse entry signature is unchanged:

```rust
// runtime/src/grammars/json/generated.rs — lazy mode
pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError> {
    let bytes = input.as_bytes();
    let parse_index = simd_scan::scan_json_parse_index(bytes);
    let mut state = ParserState::new(bytes, parse_index);
    parse_value(&mut state)?;
    state.finish_lazy::<JsonRoot<'i>>()
}

fn parse_value(state: &mut ParserState<'_>) -> Result<(), ParseError> {
    let off = state.peek_offset()?;
    match state.source[off] {
        b'{' => parse_object(state),
        b'[' => parse_array(state),
        b'"' => parse_string(state),
        b'-' | b'0'..=b'9' => parse_number(state),
        b't' => state.expect_keyword(b"true"),
        b'f' => state.expect_keyword(b"false"),
        b'n' => state.expect_keyword(b"null"),
        _ => Err(ParseError::ExpectedValue),
    }
}

fn parse_object(state: &mut ParserState<'_>) -> Result<(), ParseError> {
    state.expect_byte(b'{')?;  // advances state.cursor past `{` offset
    if state.peek_offset_byte()? == b'}' {
        state.expect_byte(b'}')?;
        return Ok(());
    }
    loop {
        parse_string(state)?;
        state.expect_byte(b':')?;
        parse_value(state)?;
        match state.peek_offset_byte()? {
            b',' => state.expect_byte(b',')?,
            b'}' => { state.expect_byte(b'}')?; return Ok(()); }
            _ => return Err(ParseError::ExpectedCommaOrClose),
        }
    }
}
```

`ParserState::expect_byte(b: u8)` reads `state.source[state.parse_index.offsets[state.cursor]]`, asserts equality, increments `state.cursor`. Returns Err on mismatch.

The view side (`view.rs`) gains the depth-tracked walkers. `JsonObject::iter` becomes:

```rust
impl<'doc, 'input> JsonObject<'doc, 'input> {
    pub fn iter(self) -> JsonMemberIter<'doc, 'input> {
        let cursor = self.cursor.cursor;
        // cursor points at '{'. Member cursors start one past.
        JsonMemberIter { tape: self.cursor.tape, cursor: cursor + 1, done: false }
    }
}

impl<'doc, 'input> Iterator for JsonMemberIter<'doc, 'input> {
    type Item = (JsonString<'doc, 'input>, JsonValueRef<'doc, 'input>);
    fn next(&mut self) -> Option<Self::Item> {
        if self.done { return None; }
        let key_cursor = self.cursor;
        let key = JsonString { cursor: ValueRef { tape: self.tape, cursor: key_cursor, .. } };
        // Advance past `"..."` (one offset) and `:` (one offset) to the value cursor.
        let value_cursor = self.cursor + 2;
        let value = JsonValueRef::project(self.tape, value_cursor);
        // Advance past value subtree (depth-tracked walk).
        self.cursor = next_sibling_cursor(self.tape, value_cursor);
        // Check for `,` or `}` at self.cursor.
        let kind = kind_at_cursor(self.tape, self.cursor);
        match kind {
            JsonKind::ElementSeparator => self.cursor += 1,
            JsonKind::ObjectClose => self.done = true,
            _ => unreachable!(),  // verifier guarantees
        }
        Some((key, value))
    }
}
```

`next_sibling_cursor` is the depth-tracked walker. It is the cost lazy mode pays in exchange for not emitting tokens; see §9 risk register.

### 7.4 LOC delta on emitted JSON

| File | Eager LOC | Lazy LOC | Delta |
|---|---:|---:|---:|
| `generated.rs` | ~600 | ~400 | -200 |
| `view.rs` | ~250 | ~350 | +100 |
| `parser.rs` | ~120 | ~120 | 0 |
| `host.rs` | ~5 | ~5 | 0 |
| `value.rs` | ~80 | ~80 | 0 |
| `visitor.rs` | ~100 | ~100 | 0 |
| Total | ~1,155 | ~1,055 | -100 |

Net JSON generated LOC drops by ~100. The drop is concentrated in fewer `state.tape.emit` lines.

---

## §8 Backwards-compat path

### 8.1 Per-grammar mode metadata

```toml
# Cargo.toml workspace metadata
[workspace.metadata.bbnf.grammars.json.runtime]
tape_mode = "lazy"

[workspace.metadata.bbnf.grammars.css_l4.runtime]
tape_mode = "eager"  # layout, recovery, payload-bearing tokens

[workspace.metadata.bbnf.grammars.bbnf_self.runtime]
tape_mode = "eager"  # source-of-truth grammar; payload-class storage required

[workspace.metadata.bbnf.grammars.sheets.runtime]
tape_mode = "eager"  # Pratt host chains; needs token-flag carry

[workspace.metadata.bbnf.grammars.css_scan.runtime]
tape_mode = "lazy"  # scan-class CSS; verifier-only; experimental
```

The default is `"eager"`. `"lazy"` is the opt-in for SOTA-class structural grammars. The `passes::extract` pipeline reads this value; `codegen::lower::rust` branches at emit time.

### 8.2 BIR alphabet (Lock 5 survival)

ARCH §7.2's 20 BIR variants are unchanged. The same `TapeEmit` and `DirectBuild` rows exist; their **lowering** differs by mode. This is identical in spirit to the way `Alt { mode: Dispatch | Speculative }` (`ARCH:915`) carries a mode discriminator in payload — except here the mode is at the grammar level, not the per-node level, and lives in workspace metadata instead of BIR payload.

The lowerer reads BIR + grammar metadata; it does NOT inspect Grammar IR. Lock 5's "lowerer never inspects Grammar IR" invariant (`ARCH:996`) survives because metadata is BIR-adjacent, not Grammar-IR.

### 8.3 Eager-mode grammars survive verbatim

CSS L4, BBNF-self, Sheets continue to use the 16-byte `TapeToken` substrate as specified today. Their `runtime/src/grammars/<g>/generated.rs` emits `state.tape.emit(...)` calls. Their `view.rs` reads `tape.tokens[index]` for kind. Their `Tape<'input>` holds `tokens: Vec<TapeToken>` (or private-Vec-sealed equivalent).

The substrate crate (`crates/runtime/src/tape/`) exposes BOTH layouts as `#[cfg]`-selected or feature-selected modules. The public API is mode-monomorphic at the per-grammar-crate level: `runtime::grammars::json::Tape` is the lazy variant; `runtime::grammars::css_l4::Tape` is the eager variant. They are distinct types with the same trait surface (`DocumentView` impls), not a runtime polymorphic dispatch. There is no `enum TapeShape { Lazy(...), Eager(...) }` in the runtime — that would BE a parallel substrate.

### 8.4 V1 graduation path

The V1 spec amendment (per the §11 decision protocol below) adds the `tape_mode` metadata key and the corresponding ARCH §9.1 Tape invariants section. Existing grammars keep `tape_mode = "eager"` by default; JSON flips to `"lazy"` based on this design's skinny re-bench. CSS scan and other future SOTA-class structural grammars can flip per measurement.

---

## §9 Risk register + falsifiability hooks

### 9.1 Risk: per-byte kind classification cost

The lazy discriminator reads `source[offsets[cursor]]` per kind query. Each query is ~2-3 ns (one cache line into offsets, one byte from source, one match arm). Total parse-time impact:

- Verifier walk: one kind query per offset; ~167K queries on canada → ~330-500 us. canada total parse budget (sonic-rs anchor): 3144 us. The verifier cost is ~10-15% of total parse time.
- Typed walk (the consumer side; not in the timed `parse(&str)` region): O(consumed-nodes) queries; lazy by definition.

Net prediction for the timed parse region: lazy mode should land **14-16K Mbps on twitter** (vs sonic-rs 18.4K Mbps; ratio 76-87%). This is the falsifiability target.

**Falsification:** If post-implementation Track 1 lands < 13K Mbps on twitter, lazy mode also has a ceiling and the SOTA-beat path requires further architectural moves (e.g., fused parse-and-verify; pext-mask alternates that V1 H tranche owns). RESULTS.md classifies outcome G again.

**Validation:** If Track 1 lands > 14K Mbps on twitter (a 12-15% jump from today's 12.5K), outcome C or B is reached, and lazy mode is ratified.

**Strong validation:** If Track 1 lands > 17K Mbps on twitter (1.36× today's 12.5K), outcome A or B is reached and the README beat target (380 us = ~17K Mbps for twitter at 616KB) is met.

### 9.2 Risk: cache locality on offsets vs tokens

Eager mode walks `tokens: Vec<TapeToken>` at 16 bytes per token. canada has 167K tokens → 2.67 MB of token stream. canada source is 2.25 MB. **The eager token stream exceeds the source size on canada.** L2 cache (M1 Pro: 12 MB shared) holds both, but L1d (M1 Pro: 128 KB per core) misses dominate.

Lazy mode walks `offsets: Box<[u32]>` at 4 bytes per offset; canada has 167K offsets → 668 KB. **Fits in L2.** Each kind lookup is one byte from source (which itself is 2.25 MB → does NOT fit in L2). So the trade-off is:

- Eager: cold reads dominated by token-stream cache misses; source bytes read only for string/number bodies (lazy in both modes).
- Lazy: cold reads dominated by source byte misses, but the source bytes ARE the data sonic-rs reads anyway. The offset array is 4× smaller and walks more linearly than a 16-byte stride.

**Prediction:** Lazy mode has a 4× bandwidth advantage on the structural walk. Each kind query has an extra cache line miss into source, but source bytes are already paid for by every JSON parser. Net should favor lazy on inputs > L2 size (citm and canada), and approximately tie on twitter.

**Falsification:** If lazy mode regresses on twitter while improving on canada, the substrate is L1-cache-bound on small inputs (token-stream resident in L1d) and the lazy win is only at scale. This is acceptable — sonic-rs's gap is largest on small corpora (twitter at 67.6%) and smallest on canada (74.0%), so closing the gap most on twitter is the prize. A lazy-mode that wins canada and ties twitter is a partial validation; RESULTS would classify by the worst-case across corpora per BENCH §6.

### 9.3 Risk: `ValueRef::iter` becomes O(n) for sibling-skip

Eager mode stores `payload_or_skip` on container open tokens. `JsonObject::iter` finishing its iteration is O(member-count); but `JsonObject::iter().nth(k)` for large k can short-circuit by skipping subtrees in O(1) per sibling. Lazy mode loses this: skipping a subtree requires a bracket-depth walk through the subtree's offsets, which is O(subtree-size).

For deep, wide JSON (e.g., a 10K-member object where the consumer queries only `.get("the_one_key_at_position_9999")`), eager mode skips 9998 subtrees in O(9998) cursor-position increments; lazy mode walks every offset of every prior subtree. On JSON consumed via `JsonObject::iter` (the common case for visitors and the parity oracle's serialize_canonical), both are O(n) and identical.

**Mitigation:** the verifier pass (TapeAssembler::verify) can OPTIONALLY emit a sidecar `subtree_skip_index: Option<Box<[u32]>>` — a small array indexed by container-open-cursor giving the skip distance. This adds back ~4 bytes per container, ~25K entries on twitter → 100 KB; ~46K on citm → 184 KB; ~56K on canada → 224 KB. Whether this sidecar is worth its cost is a measurement decision, not a design decision. Skinny v2 lands WITHOUT the sidecar to test the pure lazy hypothesis; if `JsonObject::get` and visitor-traversal benches show O(n²) pathology, the sidecar lands at v3.

**Falsification:** the parity oracle's `serialize_canonical` walks every node by definition; its cost should be comparable to today's. If post-implementation `serialize_canonical` is > 1.5× slower, the sidecar lands.

### 9.4 Falsifiability hooks (BENCH §6 outcome classification)

The BENCH harness is unchanged. Outcomes A/B/C/D/E/F/G classify against the same threshold matrix (`BENCH.md:613-628`). If lazy-mode implementation lands:

- **Outcome A** (Track 2 ≤ BEAT_BOUND, Track 1 ≤ Track 2 × 1.10) on all three corpora: lazy mode is fully validated; V1 SOTA-beat is confirmed.
- **Outcome B/C** on all three corpora: lazy mode validates V1 parity; SOTA-beat probability 30-70%.
- **Outcome G** repeating: lazy mode is refuted; the substrate hypothesis (that materialization cost was the gap) is wrong. RESULTS.md classifies outcome G with an explicit "lazy-mode tested; refuted" note. The next architectural move investigates parse-time inlining and instruction-cache pressure.

The gate runs unchanged; no new probe is needed for the design itself. The masking probes from BENCH §7.8 still fire (host-call dispatch, eager-decode, alternate-plan, cold-cache); their semantics are unchanged.

---

## §10 Implementation cost estimate

### 10.1 LOC delta

| Module | LOC added | LOC deleted | Net |
|---|---:|---:|---:|
| `runtime/src/tape/mod.rs` (lazy Tape variant) | +120 | -80 | +40 |
| `runtime/src/tape/view.rs` (cursor walker, depth-track helpers) | +250 | -50 | +200 |
| `runtime/src/tape/builder.rs` → `assembler.rs` (rename + rewrite) | +200 | -180 | +20 |
| `runtime/src/tape/token.rs` (preserved for eager mode; no change) | 0 | 0 | 0 |
| `runtime/src/grammars/json/generated.rs` (less emit, more verify) | +50 | -200 | -150 |
| `runtime/src/grammars/json/view.rs` (lazy walkers) | +200 | -100 | +100 |
| `runtime/src/grammars/json/visitor.rs` (depth-tracked walker) | +80 | -50 | +30 |
| `codegen/src/lower/rust.rs` (mode-branching) | +200 | -100 | +100 |
| `passes/src/extract.rs` (read `tape_mode` metadata) | +50 | 0 | +50 |
| `ir/src/backend_ir.rs` (no BIR variant adds; comments only) | +20 | 0 | +20 |
| Tests (parity, identity, walkers, kind discriminator) | +300 | 0 | +300 |
| Bench Track 2 hand-coded (rewrite to assembler) | +200 | -300 | -100 |
| Documentation (SUBSTRATE.md, COMPILER.md, BENCH.md, INDEX.md) | +400 | -150 | +250 |
| **Total** | **+2070** | **-1210** | **+860** |

The substrate side: ~+800/-400 LOC (~+400 net). The codegen template: ~+200/-100 LOC (~+100 net). BIR additions: ~+20 LOC (comments only; no variant change). Tests: ~+300 LOC. Track 2 rewrite: ~-100 LOC. Documentation: ~+250 LOC. Total LOC delta ~+860 LOC.

`bbnf-bench` LOC sits inside its current budget (`BENCH.md:1583`) but pushes from ~2,200 toward ~2,300. The runtime crate grows from ~4,000 LOC (skinny `runtime`) to ~4,400 LOC (~+10%). Generated JSON LOC drops slightly (-100).

### 10.2 Wall-clock estimate

- Substrate scaffolding (Tape lazy variant; ValueRef cursor migration; PayloadArena unchanged): 2-3 days.
- Assembler verify implementation: 2 days.
- View-layer cursor walkers + depth-tracked iter: 2-3 days.
- Codegen template mode-branching: 2 days.
- Track 2 hand-coded rewrite: 1 day.
- Tests, parity oracle migration, identity smoke: 2 days.
- Bench harness updates, masking-probe verification: 1 day.
- Documentation amendments (SUBSTRATE/COMPILER/BENCH/INDEX): 1-2 days.

Total: **8-12 working days** assuming one engineer. The skinny is buildable in this window; the cohort cycle's typical 1-2 week budget per re-iteration applies.

### 10.3 Tranches affected (V1 graduation)

- **Skinny implementation** (this design): re-bench gate.
- **V1 Tranche B (runtime substrate)**: adopts lazy mode for JSON; the `Tape` variant becomes load-bearing for the JSON row.
- **V1 Tranche F (Rust lowerer template)**: gains mode-branching emit path.
- **V1 Tranche I (LSP / incremental parse)**: lazy mode interacts with `ReparsePlan` carefully. Today's `ReparsePlan::Reuse { unchanged: Vec<TapeRange> }` (per `PASS-3.md:209-220`) names tape ranges. In lazy mode, a "tape range" is a range of offsets; reuse is a range of `offsets[..]` indices. The dirty-range / anchor-set algorithm is unchanged in shape but works at offset granularity instead of token granularity. The fallback-rate gates (`PASS-3.md:277-282`) apply with the same thresholds. **No I tranche redesign required**; only the data type for "reusable range" changes.
- **V1 Tranche J (memory residency / SOTA close)**: lazy mode is expected to dramatically lower peak RSS on canada (today's 3.572 MB allocated tape on canada drops to ~668 KB offsets; ~5× memory win). The outcome M gate (peak RSS ≤ 3× competitor) is more easily passed.

The user-facing API (`Json::parse(&str) -> Result<JsonRoot<'i>, ParseError>`) is unchanged. Downstream consumers (path-core, visitor, LSP cursor positioning) see the same `ValueRef` shape and the same `(TapeId, cursor, kind)` identity.

---

## §11 Decision protocol

### 11.1 Sequence

1. **SK-V2 cohort returns** (this audit cycle). Cohort verdicts on design soundness, Lock 1 amendment legality, mode-dispatch architecture, and cost-estimate plausibility.
2. **User reviews** this design plus the cohort verdict. Final disposition is the user's; this design proposes, does not commit.
3. **If approved**: implement lazy-mode for JSON skinny grammar over 1-2 weeks per §10.2. The eager-mode substrate stays in tree (CSS / BBNF-self / Sheets need it). The implementation is additive to the substrate crate; existing tests on eager-mode grammars remain green.
4. **Re-bench** against the same three fixtures (twitter, citm_catalog, canada) with the same harness (`cargo bench -p bbnf-bench`). The gate (`crates/bbnf-bench/src/bin/gate.rs`) runs unchanged.
5. **Classify** by the same threshold matrix (`BENCH.md:613-628`).

### 11.2 Branch decisions on outcome

- **If T1 > 17K Mbps on twitter** (outcome A or B): the design is fully validated. README beat target met. Dispatch tranches A-J. Update Lock 1 with the §4 amendment text verbatim. V1 SUBSTRATE.md adopts the `tape_mode` metadata key.
- **If T1 between 14K-17K Mbps on twitter** (outcome C or D): the design is validated for parity. Dispatch tranches A-J with H tranche pre-allocated for further SOTA tuning. Lock 1 amendment lands.
- **If T1 between 13K-14K Mbps on twitter** (borderline outcome F): the design produces a marginal substrate win. Run the F-positive vs F-noise sub-classification (`BENCH.md:621-622`). Lock 1 amendment lands conditionally — only the JSON grammar uses lazy mode; CSS-scan deferred. Re-bench on bare metal.
- **If T1 < 13K Mbps on twitter** (outcome G): lazy mode is REFUTED. Architectural claim is wrong; materialization cost is not the dominant gap. Update RESULTS.md with explicit "lazy-mode tested; refuted on twitter at <ratio>×" note. Investigate via samply profile of the lazy-mode hot path; possibly write the SOTA-beat work off to V1 H tranche body with no architectural prior. Do NOT amend Lock 1 — eager mode stays canonical until the next architectural candidate measures positive.
- **If outcome I (parity fail)**: a bug in the lazy implementation; fix and re-bench. Does not refute the design.
- **If outcome M (peak RSS fail)**: surprising; lazy mode should improve RSS. Investigate allocator / over-reservation.

### 11.3 V1 graduation gate

The lazy-mode skinny does NOT alone authorize V1 dispatch. The same SK-V<N> gate process applies: cohort review + user disposition + re-bench. After lazy-mode JSON outcome A/B/C lands, the design moves to V1 by:

1. Lock 1 amendment landing in `restart/locks/14-LOCKS.md`.
2. ARCH §9.1 Tape invariants amendment per the §4 text.
3. SUBSTRATE.md §1.1-§1.3 per §3.1.
4. COMPILER.md §3 + §6 per §3.2.
5. BENCH.md §1.1-§1.3 + §3.4 per §3.3.
6. INDEX.md cross-quadrant invariants + deviation ledger per §3.4.
7. Workspace metadata key `tape_mode` added to `WORKSPACE.md`.

The V1 Tranche B (runtime substrate) wave that consumes this amendment is dispatched only after the amendments land. Tranche I (LSP) absorbs the offset-range `ReparsePlan` data-type update during its W1 substrate-consumer wave.

### 11.4 Boundary with the rejected amendments

The two previously rejected architectural moves (dispatch-table-as-canonical; 12-byte token) remain rejected. This design does NOT propose reopening them. The lazy-mode design is orthogonal:

- Dispatch table: lazy mode's alt-dispatch still lowers as Rust `match` (the BIR `Alt { mode: Dispatch }` row is unchanged); LLVM's match-arm codegen on byte-disjoint alts remains canonical. Function-pointer tables remain rejected.
- 12-byte token: lazy mode REMOVES the token entirely; the 12-vs-16-byte question is moot for JSON. Eager-mode grammars keep the 16-byte token.

### 11.5 Boundary with V1 H tranche body

The V1 H tranche owns cost-model-driven plan selection and recogniser tuning. The lazy-mode design does NOT preempt H. If lazy mode lands outcome C (parity but no beat), the SOTA-beat work for JSON is still H's. The cost model can later select between eager-mode lowering and lazy-mode lowering per grammar based on measured throughput; today's design hard-codes the choice per grammar in metadata, which is the V1 path. The H tranche can later add a cost-model-driven `tape_mode` selector that overrides the metadata default.

---

## §12 Summary (one paragraph)

The eager-tape substrate hits ~12.5K Mbps on twitter while sonic-rs hits 18.4K Mbps. Three iterations of microarchitectural perturbations (dispatch-table; narrower token; pair-fusion; duplicate-byte column; whitespace-bearing index) failed to close the gap. The remaining honest architectural move is lazy materialization: the structural-offset array IS the tape, node kind is computed from `source[offsets[cursor]]`, and `TapeEmit` becomes a no-op for `tape_mode = "lazy"` grammars. The design preserves the `(TapeId, cursor, kind)` identity invariant, the `ValueRef<'doc, 'input, K>` cursor shape, the `DocumentView` trait, the payload arena, the visitor surface, the BIR alphabet, the parse function signature, and the bench harness. It amends Lock 1 to admit two `tape_mode` values under one substrate API (lazy for JSON / CSS-scan; eager for CSS L4 / BBNF-self / Sheets). Implementation cost is ~860 LOC and 8-12 working days. Falsifiability is sharp: if post-implementation T1 on twitter lands < 13K Mbps, the lazy-tape architectural claim is refuted; if T1 > 14K Mbps, the claim is validated. The gate runs unchanged.

---

### Critical Files for Implementation

- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/SUBSTRATE.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/COMPILER.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/BENCH.md
- /Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md
- /Users/mkbabb/Programming/bbnf-lang/restart/ARCHITECTURE.md
