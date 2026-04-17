# AW-IV B2 — bbnf-tape architectural changes for the per-shape inline emitter

## Executive summary

Four surgical bbnf-tape changes enable the per-shape inline emitter without
forking the substrate. (1) Expose the un-inlined helpers (`advance_or_pop_with`,
`nearest_variant_frame`, `write_decoded`, `finalise`) as TokenStream body
fragments in a `bbnf-tape-codegen` subcrate so the walker emitter splices
them into arm tails; cold-path `dispatch_one` keeps the named fn surface
verbatim. (2) Fold `finaliser::finalise` into the per-shape loops — emitters
know `span_hi`/`child_off`/`sib_skip` at close time; retain the fn for
`dta_run_cold` + legacy fn-per-rule reconstruction. (3) Split PSI into
scalar-direct (f64/u8/bool/HexU32 via `Columns::push_scalar_payload`) and
job-queued (String/AggregateLarge via rayon) populations. (4) Add a
monomorphic `Visitor` trait so the one emitter drives `TapeVisitor`
(column emission) and `ValueVisitor` (direct struct emission) — the bench's
second `walk_cursor` pass collapses on the Value path. `dispatch_one` +
every cold-path helper + every `DtaState` variant survive as the AX replay
surface per AX.md §93-103.

## 1. Inline-emitter helper API

The four helpers are called from every hot walker arm (`advance_or_pop_with`
tail, `nearest_variant_frame` per `emit_leaf_with_payload`) and from the
cold-path `dispatch_one`. Splicing bodies inline without duplicating source
between crates requires one source authority.

**Recommendation.** A `bbnf-tape-codegen` subcrate exposes `fn
advance_or_pop_with_body() -> TokenStream`, `nearest_variant_frame_body()`,
`write_decoded_body(kind)`, etc. The walker emitter calls these from arm
codegen; cold-path `dispatch_one` + `fn advance_or_pop_with` also consume
the TokenStream at build time via a `quote!`-expanded fn body. One source,
two consumers; no divergence between hot/cold semantics — the class of
bug AX is specifically consuming these surfaces to detect. Alternative
(emitter reimplements bodies; `bbnf-tape` keeps fns only for cold-path)
duplicates 150 lines of Repeat/Alt/ShuntingYard logic (driver.rs:2518-2790)
— rejected. P1 §8 puts the four boundaries at 26–34 % of every parse;
per-arm splice collapses each.

## 2. FrameStack refactor

P1 §3 puts `FrameStack::nearest_variant_frame` at 1.7–2.6 % self-time
(driver.rs:546-561). It walks overflow-then-inline for a non-`u8::MAX`
`variant_idx` on every payload-bearing leaf (driver.rs:2222-2228). The
stack walk exists because `dispatch_one` cannot tell at runtime whether a
leaf was emitted under a transparent-rule body-Seq or a discriminated Alt
branch.

**Per-shape emission obviates the ambiguity.** Each specialised
`parse_string_<grammar>` is called from a known rule context; the emitter
inlines a **constant** `variant_idx` at every `push_leaf_fused` call. Add
a literal `variant_idx: u8` parameter to `push_leaf_fused` (columns.rs:515);
the emitter passes a constant; `nearest_variant_frame` stays only for
`dispatch_one` fallback.

**Minimum hot-path state.** Per-shape loops (B1's recommendation) reduce
FrameStack to zero for JSON / CSS-leaf-path / Sheets — each specialised
function owns its frame on the CPU stack. Generic compound paths keep
`ShuntingYard` frames only. Per-arm direct writes replace variant_idx
bookkeeping: `advance_or_pop_with` call sites in walker arms become `continue`
(Seq-next), `return` (Alt-close), or emitter-staged fixup (Repeat re-enter).

## 3. Finaliser redesign

P1 §3 puts `finalise` at **11–14 % flat** across every JSON entry
(data_xl 13.14 %, twitter 11.28 %, canada 13.56 %). The pass patches
`sib_skip`/`span_hi`/`child_off` (finaliser.rs:154-252) because the generic
walker emits compounds without knowing child count at close time.

**Per-shape emitters know all four values at emit time:** `span_hi` = pos
after last child; `child_off` = columns.len() at reservation; `sib_skip` =
idx delta to prev-sibling threaded through the emitter-local loop;
`frame_depth` = constant per arm or depth counter in the loop header.

**Changes.** (a) Delete the `finalise` call in `TapeBuilder::finish` on the
`has_inline_frame_depth == true` hot path (builder.rs:685-693). (b) Keep
`finalise` + `derive_frame_depth` callable for the cold-path `dta_run_cold`
+ legacy fn-per-rule reconstruction (builder.rs:694-697) — AX substrate.
(c) Extend `close_compound` (driver.rs:2358) to accept a `prev_sibling_idx`
threaded by the emitter; one new param, matches per-shape emission.

Cost/benefit: dropping the 13 % flat pass recovers ~2.6 cyc/B on data_xl
(P5 §5 finalise fold = 2.01 cyc/B on twitter). Zero AX impact.

## 4. PSI hot-path elision for scalars

P1 §3 puts `psi::write_decoded` at 0.78–5.67 %. P6 §10 shows the symbol
present in every binary; the W2.3.a promise of scalar-inline direct-write
(psi.rs:14-24) is not yet firing because the emitter still routes scalars
through `psi.push`.

**Qualifying kinds.** F64, I64, U8, Bool, HexU32 — `arena_byte_width() ∈
{1,4,8}` and trivially inlinable decoders. Disqualified: String
(escape-decode non-trivial; rayon pays for itself) and AggregateLarge
(variable width, CSS color() fills).

**API.** `Columns::push_scalar_payload_f64(v: f64) -> u32`, parallels for
u8/bool/i64/u32 — each a one-line `pay_agg.extend_from_slice`. Capacity
pre-sized via `GrammarProfile::leaves_per_input_byte × input.len()`
(psi.rs:82); zero steady-state growth.

**Routing.** Codegen-time, not runtime. The emitter reads
`DtaState::Regex { payload }` (dta.rs:229-237) and emits
`columns.push_scalar_payload_f64(parsed)` for scalar payloads,
`psi.push(PayloadJob::new(...))` for String/AggregateLarge. Canada
(90 % numeric, P1 §5) recovers the most.

## 5. Capacity pre-allocation

P6 §8 confirms pre-allocation is already present (JSON `N/2 + 2`, others
`N + 2`). Only clean-up needed: a `TapeBuilder::with_capacity_for(
profile: &GrammarProfile, input_len: usize)` convenience that computes
from `GrammarProfile::capacity_for` (profile.rs:281). One line, eliminates
duplication at every `parse()` entry point.

## 6. Fused visitor trait

Add `crates/bbnf-tape/src/visitor.rs` exporting `Visitor` with
`begin_compound` / `end_compound` / `leaf_span` / `leaf_f64` / `leaf_u8` /
`leaf_bool` / `leaf_string_borrowed` / `leaf_string_decoded` / `finalize`.
Two implementations: `TapeVisitor<'b>` wraps `&mut TapeBuilder` (column
emission); `ValueVisitor<'b, T>` wraps `&mut T` (direct struct emission).
Per-shape emitter generates `fn parse_object_<grammar><V: Visitor>(input,
pos, visitor: &mut V) -> usize` — monomorphises at link time; zero dyn
dispatch. One emitter, two consumers — the generality lever that makes
the tape path and the fused direct-materialisation path share codegen.

## 7. Cursor path unification

P5 §2+§5 puts `walk_cursor` at 12.1 % self-time, 2.01 cyc/B on twitter —
sonic's one pass. Two paths per grammar:

**ValueVisitor emit (JSON bench, value materialisation).** Emit the walker
with `ValueVisitor` directly; `walk_cursor` never runs; sonic parity.

**TapeVisitor emit (CSS L4, BBNF, Sheets, incremental-parse consumers).**
Tape persists; cursor walk is the consumer API. Make the walk free via
AW-IV W5's `reduce_column` consumer API — a forward column scan
`for i in 0..cols.len() { match cols.kinds[i] { ... } }` reads
`cols.pay_agg[co..co+8]` direct slices, skipping `TapeCursor::children_
zero_alloc` indirection. The 12 % self-time drops to ~2 %. Grammar author
opts in via an emitter knob.

## 8. AX replay-surface guarantee

Every surface AX.md §93-103 declares load-bearing survives verbatim:
`driver::dispatch_one` (driver.rs:1350), `dta_run_cold` (driver.rs:788),
`try_branch` (driver.rs:1277), `advance_or_pop_with` (driver.rs:2518,
body extracted to `helpers::emit` but fn signature + callers unchanged),
`handle_repeat_failure`/`handle_repeat_failure_bounded` (driver.rs:1095,
1158), every `emit_leaf`/`push_leaf_fused`/`push_compound_fused`/
`close_compound` (hot-path splices body, cold-path calls by name), every
`DtaState` variant in `dta.rs` (including `ClassifyByte`/`Minus`/
`ConsumeToNextStructural`/`ShuntingYard`), `finaliser::finalise` +
`derive_frame_depth` (cold-path + legacy fn-per-rule callers only),
`PayloadStream` + `PayloadJob` + `fill_columns` + `write_decoded` (String
/ AggregateLarge only), `FrameStack` + savepoint + probe-snapshot types
(cold-path). Hot-path per-shape loops bypass most of the surface; nothing
deletes.

## 9. Migration plan

Ordered enables → breaks → risk:

1. **Capacity helper (§5).** `TapeBuilder::with_capacity_for`. Trivial.
2. **Scalar PSI elision (§4).** `Columns::push_scalar_payload_*`. Emitter
   arms switch one-by-one; psi.push remains for unconverted arms; A/B
   per-grammar.
3. **Visitor trait (§6).** Additive; no existing caller breaks.
4. **Helper body extraction (§1).** `bbnf-tape-codegen` subcrate created;
   `dispatch_one`'s callees compile unchanged.
5. **Per-shape emitter activation (B1 scope).** Walker emitter splices
   bodies from §1; JSON first, CSS/BBNF/Sheets follow.
6. **Finaliser inline-fixup (§3).** Once per-shape emitters own
   `span_hi`/`child_off`/`sib_skip`, delete the `finalise` call on the
   hot path; cold-path unchanged.
7. **FrameStack thinning (§2).** `push_leaf_fused` gains literal
   `variant_idx` param; emitter passes constants.
8. **reduce_column consumer API (§7, AW-IV.W5).** Tape consumers switch
   off per-step `children_zero_alloc` indirection.

Breakage: none on cold-path replay. Only emitter-side refactor in W4.b —
AW-III.W4 already owns these splice points.

## 10. Generalisation note

The per-shape inline emitter is grammar-agnostic: it projects each compound
rule as a specialised loop, splices `helpers::emit` bodies, consumes a
monomorphic `Visitor`, pre-sizes columns via `GrammarProfile::capacity_for`.
JSON / CSS / BBNF / Sheets differ only in which `DtaState` variants
populate the lifted table and which `PayloadKind`s claim the scalar-elision
path — both mined per-grammar by the IR pass. Every change above
(helpers-as-fragments, visitor trait, scalar push, inline fixup,
FrameStack thinning) fires whenever the emitter emits a per-shape loop;
AX's `dispatch_one` walks any `DtaTable` verbatim. One tape, two consumers
— per-shape hot path + cold-path dispatch_one — is the generalisation
invariant; bbnf-tape's job is to make both cheap without forking.

## Appendix — API sketches

### A. `bbnf-tape-codegen` subcrate

```rust
// crates/bbnf-tape-codegen/src/lib.rs
use proc_macro2::TokenStream;
use quote::quote;

pub fn advance_or_pop_with_body() -> TokenStream {
    quote! {
        loop {
            let Some(top) = stack.top_mut() else {
                return Ok(StepResult::Done);
            };
            match top.kind {
                DtaFrameKind::Seq => { /* ... */ }
                DtaFrameKind::Alt => { /* ... */ }
                DtaFrameKind::Repeat => { /* ... */ }
                DtaFrameKind::ShuntingYard => { /* ... */ }
            }
        }
    }
}

pub fn nearest_variant_frame_body() -> TokenStream { /* ... */ }
pub fn write_decoded_body(kind: PayloadKind) -> TokenStream { /* ... */ }
pub fn finalise_close_fixup_body() -> TokenStream { /* ... */ }
```

Cold-path `fn advance_or_pop_with` in `driver.rs` uses the same
TokenStream via a `quote!`-expanded fn body at build time — one source,
hot + cold consumers.

### B. Columns scalar direct-write

```rust
// crates/bbnf-tape/src/columns.rs — new methods
impl Columns {
    #[inline(always)]
    pub fn push_scalar_payload_f64(&mut self, v: f64) -> u32 {
        let off = self.pay_agg.len() as u32;
        self.pay_agg.extend_from_slice(&v.to_bits().to_le_bytes());
        off
    }
    #[inline(always)]
    pub fn push_scalar_payload_u8(&mut self, v: u8) -> u32 {
        let off = self.pay_agg.len() as u32;
        self.pay_agg.push(v);
        off
    }
    // parallel: _bool, _i64, _u32, _hex_u32
}
```

### C. Visitor trait

```rust
// crates/bbnf-tape/src/visitor.rs
pub trait Visitor {
    type Output;
    fn begin_compound(&mut self, kind: TapeKind, variant: u8, span_lo: u32);
    fn end_compound(&mut self, span_hi: u32);
    fn leaf_span(&mut self, span_lo: u32, span_hi: u32);
    fn leaf_f64(&mut self, v: f64, span_lo: u32, span_hi: u32);
    fn leaf_u8(&mut self, v: u8, span_lo: u32, span_hi: u32);
    fn leaf_bool(&mut self, v: bool, span_lo: u32, span_hi: u32);
    fn leaf_string_borrowed(&mut self, src: &[u8], span_lo: u32, span_hi: u32);
    fn leaf_string_decoded(&mut self, decoded: &[u8], span_lo: u32, span_hi: u32);
    fn finalize(self) -> Self::Output;
}

pub struct TapeVisitor<'b> {
    columns: &'b mut Columns,
    frame_depth: &'b mut Vec<u8>,
    psi: &'b mut PayloadStream,
}

pub struct ValueVisitor<'b, T> {
    target: &'b mut T,
}
```

### D. TapeBuilder capacity helper

```rust
// crates/bbnf-tape/src/builder.rs
impl TapeBuilder {
    #[inline]
    pub fn with_capacity_for(profile: &GrammarProfile, input_len: usize) -> Self {
        Self::with_capacity(profile.capacity_for(input_len))
    }
}
```

### E. Per-shape emitter arm shape (B1 scope; bbnf-tape contract only)

```rust
// Emitted per-grammar code calling into bbnf-tape surfaces.
fn parse_object_JsonParser<V: Visitor>(
    input: &[u8],
    pos: usize,
    visitor: &mut V,
) -> usize {
    visitor.begin_compound(TapeKind::Seq, VARIANT_OBJECT, pos as u32);
    let mut p = pos + 1; // past '{'
    loop {
        // skip_ws (inline SIMD whitespace-bitmap)
        // parse_string_JsonParser → visitor.leaf_string_*
        // expect ':'
        // parse_value_JsonParser → recurses
        // ',' continues; '}' breaks
    }
    visitor.end_compound(p as u32);
    p
}
```

Every call into `bbnf-tape` from emitted code goes through the `Visitor`
trait + `Columns` scalar-direct API — zero un-inlined cross-crate boundary
on the hot path.

## Profile citations

- P1 §3.2–3.5: `advance_or_pop_with` 10–17 %, `finalise` 11–14 %,
  `write_decoded` 0.78–5.67 %, `nearest_variant_frame` 1.7–2.6 %.
  (`docs/tranches/AW/research/aw4-profile-p1-json-monolithic.md`)
- P3 §1+§8: Sheets `try_branch` 52–72 %; projected ~10 MB/s post-inlining.
  (`docs/tranches/AW/research/aw4-profile-p3-sheets.md`)
- P4 §4: BBNF dispatch + advance_or_pop + finaliser 88 % uniformly.
  (`docs/tranches/AW/research/aw4-profile-p4-bbnf.md`)
- P5 §5: bbnf_twitter 19.36 cyc/B, sonic 1.51 cyc/B; 22 % of bbnf is
  second-pass walk + finaliser. (`docs/tranches/AW/research/aw4-profile-p5-bbnf-vs-sonic.md`)
- P6 §3: JSON 153 cross-crate BL calls per walker; CSS 2 283 cold_state_N.
  (`docs/tranches/AW/research/aw4-profile-p6-begotten-code-audit.md`)
- AX.md §93-103: cold-path replay surface preservation contract.

## bbnf-tape source-file anchors

- `crates/bbnf-tape/src/driver.rs:546` — `FrameStack::nearest_variant_frame`
- `crates/bbnf-tape/src/driver.rs:1277` — `try_branch`
- `crates/bbnf-tape/src/driver.rs:1350` — `dispatch_one` (AX surface)
- `crates/bbnf-tape/src/driver.rs:2196` — `emit_leaf_with_payload`
- `crates/bbnf-tape/src/driver.rs:2358` — `close_compound`
- `crates/bbnf-tape/src/driver.rs:2518` — `advance_or_pop_with`
- `crates/bbnf-tape/src/finaliser.rs:154` — `finalise`
- `crates/bbnf-tape/src/finaliser.rs:276` — `derive_frame_depth`
- `crates/bbnf-tape/src/psi.rs:378` — `PayloadStream::push`
- `crates/bbnf-tape/src/psi.rs:448` — `fill_columns`
- `crates/bbnf-tape/src/psi.rs:662` — `write_decoded`
- `crates/bbnf-tape/src/columns.rs:446` — `push_compound_fused`
- `crates/bbnf-tape/src/columns.rs:515` — `push_leaf_fused`
- `crates/bbnf-tape/src/builder.rs:674` — `TapeBuilder::finish`
- `crates/bbnf-tape/src/dta.rs:203` — `DtaState` enum (AX surface)
