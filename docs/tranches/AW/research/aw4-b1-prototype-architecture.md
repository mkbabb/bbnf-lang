# AW-IV B1 — JSON-only prototype architecture

Design for a hand-tuned JSON parser that replaces bbnf's `__dta_walker_inline::run` on the JSON grammar only, reuses the existing `bbnf-simd-scan` + `bbnf-tape` substrate, and is shaped so codegen can regenerate it per-grammar once validated. All design decisions grounded in the P1–P6 profiles (`docs/tranches/AW/research/aw4-profile-p{1,5,6}-*.md`) and the sonic-rs 0.3.17 source shape (`parser.rs:400–555`, `parser.rs:1205–1254`, `value/visitor.rs:5–88`).

## 1. Crate location and dependency graph

New workspace member `crates/bbnf-json-prototype/`:

```toml
[package] name = "bbnf-json-prototype"
[dependencies]
bbnf-tape = { path = "../bbnf-tape" }       # Columns, TapeBuilder, TapeKind
bbnf-simd-scan = { path = "../bbnf-simd-scan" }  # parity, nospace bitmap
[dev-dependencies]
sonic-rs = "0.3"                            # parity target
bencher = "0.1"                             # isomorphic to crates/core/benches/json/value.rs
```

`benches/json_value.rs` mirrors `crates/core/benches/json/value.rs:119–189` verbatim (same `load`, same macros, same five datasets). Naming keeps `--bench` filters apples-to-apples; no new timing harness.

**Per `feedback-general-infra-crates`:** the prototype is a self-contained crate, not shoved into `bbnf-tape`. It is consumer-code; `bbnf-tape` stays substrate.

## 2. Visitor trait (§P5 §8)

Monomorphised at compile time — no `dyn`. Trait shape mirrors sonic-rs `visitor.rs:5–88` but returns `Result<(), ParseError>` so the parse short-circuits on visitor reject without a second `Err` projection at every return site:

```rust
pub trait JsonVisitor<'de> {
    fn begin_object(&mut self, hint: usize) -> Result<(), ParseError>;
    fn end_object(&mut self, len: usize)    -> Result<(), ParseError>;
    fn begin_array (&mut self, hint: usize) -> Result<(), ParseError>;
    fn end_array   (&mut self, len: usize)  -> Result<(), ParseError>;
    fn key         (&mut self, k: &'de str, borrowed: bool) -> Result<(), ParseError>;
    fn string      (&mut self, s: &'de str, borrowed: bool) -> Result<(), ParseError>;
    fn number_f64  (&mut self, v: f64) -> Result<(), ParseError>;
    fn bool        (&mut self, v: bool) -> Result<(), ParseError>;
    fn null        (&mut self)           -> Result<(), ParseError>;
}
```

`borrowed: bool` mirrors `bbnf-tape/src/tape.rs:646–671`'s `payload_string_with_source` two-mode contract. Hint arguments enable the `TapeVisitor` to reserve compounds without a second pass (§P5 identifies that `finaliser::finalise` fold — 10–14% — is a post-pass; hint kills it).

## 3. Shape-specialised loops (§P5 §8)

Five `#[inline(always)]` free functions in `src/parse.rs`. Each monomorphised at the single call site `parse_json::<V>`:

```rust
pub fn parse_json<'de, V: JsonVisitor<'de>>(input: &'de [u8], v: &mut V)
    -> Result<(), ParseError>
{
    let mut p = 0usize;
    let mut state = ScanState::new(input);   // holds nospace_bits cache
    skip_space(input, &mut p, &mut state);
    parse_value(input, &mut p, &mut state, v)?;
    skip_space(input, &mut p, &mut state);
    if p != input.len() { return Err(ParseError::Trailing(p)); }
    Ok(())
}
```

Per-shape bodies in Appendix A. `ScanState` holds `nospace_bits: u64` + `nospace_start: isize` (isomorphic to `sonic-rs-0.3.17/parser.rs:1220–1237`); it is *per-parse local state on the CPU stack* — not `FrameStack::nearest_variant_frame` (§P5: 1.6–2.6% self-time in bbnf; sonic has no analogue).

**Forbidden:** no `dispatch_one`, no `try_branch`, no `advance_or_pop_with`, no `DtaState` match arms, no `FrameStack`. The call stack *is* the frame stack — this is direct recursive descent.

## 4. SIMD-kernel inline consumption (§P6 audit)

`bbnf-simd-scan` publicly exports `scan_structural` (`lib.rs:80`) — a *pre-scan* that emits a `StructuralIndex`. The prototype does **not** use it. §P6 shows `scan_structural` compiles in but is absent from hot symbols; §P5 §3 confirms sonic doesn't pre-scan. Instead the prototype calls the sub-primitives directly:

| kernel | source | strategy |
|---|---|---|
| nospace bitmap | new `nospace64.rs`, ported from `sonic-rs/src/util/simd/*::get_nonspace_bits` | `#[inline(always)]` free fn; single NEON stripe, 4 × `vqtbl1q_u8` + OR + `vshrn_n_u16`; AVX2 via `_mm256_cmpeq_epi8` against `{space, \t, \n, \r}`. Collapse into `parse.rs` on every `skip_space`. Source authority: `sonic-rs-0.3.17/src/util/arch/*`. |
| quote-run / escape-run | `bbnf-simd-scan::parity::{prefix_xor_64, escape_mask_64}` (lib.rs:52, parity.rs:40–47, 74–82) | `#[inline(always)]` library call; LLVM inlines (trait-object-free, `Copy` args). Consumed from `parse_string`. Rationale: parity.rs is arch-clean (single CLMUL/shift-XOR), already `#[inline]` — cross-crate boundary collapses under workspace LTO. |
| memchr(b'"',b'\\') | `bbnf-simd-scan::neon::classify_chunk_nibble` primitive (neon.rs:200–209) | **Extract + republish** as `bbnf_simd_scan::neon::first_quote_or_backslash(ptr, len) -> Option<(usize, u8)>`. Called per-stripe from `parse_string`. Alternative — keep `scan_structural` with a singleton-alphabet `{b'"', b'\\'}` and walk its positions — is rejected: P6 shows `scan_structural` materialises a `Vec<u32>` + `Vec<u8>` (neon.rs:68), payload the tight string loop must not touch. |
| digit run | hand-written NEON `vcleq_u8(b'9') & vcgeq_u8(b'0')` + `vshrn_n_u16` | ≤32 lines in `parse_number`; no general helper needed. |

**PSI elision.** §P1 shows `psi::write_decoded` at 5.67%/5.59% on string-heavy inputs (`twitter`, `data_xl`); for the prototype's `ValueVisitor` the string lands directly in a `&'de str` (no arena copy — borrowed when `\` absent), and for `TapeVisitor` the decoded bytes go straight to `columns.pay_agg` via `TapeBuilder::push_leaf_with_arena_frame` (builder.rs path). Zero `PayloadStream::push`. psi.rs:50–58 notes PSI persists only for `String` + `AggregateLarge` residuals under the cold replay driver; the prototype never touches that path.

## 5. Number kernel — inline Eisel-Lemire

§P1 §3.4 names `core::num::dec2flt::lemire::compute_float::<f64>` as 1.28% + 4.81% (`<f64>::from_str`) + 4.04% (`from_utf8_unchecked` upstream) on `canada` — a 10.1% recoverable strip. The prototype inlines the 80-line fast-path from `core::num::dec2flt::lemire::compute_float` (integer-run scan + binary64 encode via the Clinger-Rybicki-Lemire power-of-five tables) directly in `parse_number`, falling back to `f64::from_str` only for values >19 digits or non-representable. This is the kernel the `parse-that/eisel_lemire` re-home was staged for; the prototype absorbs it.

## 6. Tape pre-allocation (§P6 §8)

`TapeVisitor::new(input_len)` calls `Columns::with_capacity(input_len / 2 + 2)` — mirrors the JSON `GRAMMAR_PROFILE.capacity_for` decision at `profile.rs:281` (§P6 table confirms JSON is 0.5 cpb, not the pessimistic 1.0). Strings with escapes still route through `push_leaf_with_arena_frame`; strings without escapes push `TapeBuilder::push_leaf_borrowed_string` (§P5 §6 confirms borrowed path exists post-W6).

## 7. Projected cycle budget — twitter (631 514 B, 64% strings, 26% ws)

Per-byte decomposition at 3.5 GHz (Apple M4 P-core, the §P5 profile host), framed against sonic's measured 1.51 cyc/B (§P5 §1):

| component | cost | basis |
|---|---:|---|
| nospace skip (SIMD stripe amortised 1/64B) | 0.20 | 4 `vqtbl1q_u8` + OR + `vshrn_n_u16` + `tzcnt` per 64 B, 6 µops/stripe × M4 8-wide dispatch ≈ 0.8 cyc/stripe ÷ 64 = 0.013; per-byte skip scales to 10× at 10% whitespace-bytes → 0.20 |
| structural byte-dispatch (1/rec) | 0.15 | 4-way jump table (`{ `, `[`, `"`, digit) — 1 LDRB + 1 CBZ/BR indirect ≈ 1 cyc, amortised per-byte by avg record length ~7 B on twitter → 0.15 |
| quote scan (string-byte, SIMD stripe) | 0.45 | 2 `vceqq_u8` + parity::prefix_xor_64 (6 op shift-XOR or 1 PMULL) ≈ 2.5 cyc/16 B string run = 0.16; escape-run rare (<1% of string bytes) → 0.45 including the `vextq_u8` first-quote locate |
| visitor call (string/number emission) | 0.20 | monomorphised `V::string` on `ValueVisitor` collapses to a `Value::String` write (1 branch + 1 store); 1 emit per ~7 B |
| Eisel-Lemire (number-byte) | 0.05 | twitter is 2% numbers; per number ~15 cyc (power5 lookup + FMA) / avg 6 digits = 2.5 cyc/byte × 0.02 = 0.05 |
| allocation (mimalloc small-bump) | 0.10 | sonic shows <2% at scale (§P5); borrowed strings skip alloc entirely |
| misc (UTF-8 validation: **none**; `from_utf8` was a bbnf artefact via `f64::from_str`, now inline) | 0.05 | |
| **total** | **≈ 1.20 cyc/B** | ⇒ **≈ 2 900 MB/s** on twitter @ 3.5 GHz |

Versus bbnf's measured 17–19 cyc/B (§P5 §1, §P1 §1), this lands within 10% of sonic's 1.51 cyc/B, meeting the brief's target. The delta vs sonic is ±0.2 cyc/B — absorbed by either winning on borrowed strings (bbnf's `payload_string_with_source` has no sonic equivalent) or losing on the 64-byte nospace-bitmap cache hit-rate (sonic's cache amortisation is empirically tuned over 2+ years — reviewable after validation).

**Canada projection:** 90% numbers × 2.5 cyc/digit-byte ≈ 2.25 cyc/B number work + 0.15 structural = **2.40 cyc/B** ⇒ **1 460 MB/s** (sonic measures 1 592 MB/s on canada; within 10%).

## 8. Validation benchmarks and gate

`benches/json_value.rs` runs the five isomorphic pairs: `data_s`, `twitter`, `citm`, `canada`, `data_xl`. Gate condition per dataset:

```
bbnf_prototype.ns_per_iter / sonic_rs.ns_per_iter ≤ 1.10
```

If `data_s` / `twitter` miss (mostly-string workloads), the residual lives in string emission — diagnose via extracted `parse_string` microbench. If `citm` misses, it's whitespace elision (71% ws per §P1 §5) — 64-byte bitmap cache may be under-amortised; extend cache window. If `canada` misses, it's the Eisel-Lemire kernel — compare against `core::num::dec2flt::lemire` directly, not via `<f64>::from_str`.

The **AW-IV-compatible shape** ships via `TapeVisitor`. A JSON bench binary at `crates/core/benches/json/` can additionally wire `JsonParser::parse` to the prototype under a cargo feature, bypassing `__dta_walker_inline::run` entirely on this one grammar. Both visitors share the *parse body* — that's the codegen-regenerable artefact. Validation of the prototype is validation of the emitter shape the generalised AW-IV.W5/W6 then retargets per grammar.

## 9. Generalisation path

The five `parse_*<V>` functions are derivable from grammar shape. `parse_object` corresponds to the `object` rule (`{`-delimited, `,`-separated, key-value); `parse_array` to `array` (`[`-delimited, `,`-separated); `parse_string` to the JSON-string Regex leaf; `parse_number` to the JSON-number Regex leaf. The emitter pass `crates/core/src/backend/rust/emitter/shape_walker/` (new, non-existent today — §P6 audit confirms no such symbol) would consume the grammar's recursive-compound projection and emit analogous `parse_<rule><V>` functions per grammar. Non-JSON grammars (CSS L4, BBNF) retain the general walker; the emitter chooses shape-specialised emission only when the grammar's compound graph is well-formed recursive-descent (test: no Alt cycles, no unbounded repeat-of-compound-that-contains-self). Any grammar that fails the test keeps `__dta_walker_inline::run` unchanged.

---

## Appendix A — per-shape bodies (pseudo-Rust, ≤40 lines each)

### `parse_value`

```rust
#[inline(always)]
fn parse_value<'de, V: JsonVisitor<'de>>(
    input: &'de [u8], p: &mut usize, s: &mut ScanState, v: &mut V,
) -> Result<(), ParseError> {
    skip_space(input, p, s);
    let b = *input.get(*p).ok_or(ParseError::Eof)?;
    match b {
        b'{' => { *p += 1; parse_object(input, p, s, v) }
        b'[' => { *p += 1; parse_array (input, p, s, v) }
        b'"' => { *p += 1; parse_string(input, p, s, v, /*is_key=*/false) }
        b'-' | b'0'..=b'9' => parse_number(input, p, v),
        b't' => { expect(input, p, b"true")?;  v.bool(true) }
        b'f' => { expect(input, p, b"false")?; v.bool(false) }
        b'n' => { expect(input, p, b"null")?;  v.null() }
        c    => Err(ParseError::UnexpectedByte(*p, c)),
    }
}
```

### `parse_object`  (sonic-rs/parser.rs:417–446 shape)

```rust
#[inline(always)]
fn parse_object<'de, V: JsonVisitor<'de>>(
    input: &'de [u8], p: &mut usize, s: &mut ScanState, v: &mut V,
) -> Result<(), ParseError> {
    v.begin_object(0)?;
    skip_space(input, p, s);
    if input.get(*p) == Some(&b'}') { *p += 1; return v.end_object(0); }
    let mut n = 0usize;
    loop {
        if input.get(*p) != Some(&b'"') { return Err(ParseError::ExpectKey(*p)); }
        *p += 1;
        parse_string(input, p, s, v, /*is_key=*/true)?;
        skip_space(input, p, s);
        if input.get(*p) != Some(&b':') { return Err(ParseError::ExpectColon(*p)); }
        *p += 1;
        parse_value(input, p, s, v)?;
        n += 1;
        skip_space(input, p, s);
        match input.get(*p).copied() {
            Some(b'}') => { *p += 1; return v.end_object(n); }
            Some(b',') => { *p += 1; skip_space(input, p, s); }
            _ => return Err(ParseError::ExpectCommaOrEnd(*p)),
        }
    }
}
```

### `parse_array`  (sonic-rs/parser.rs:390–415 shape)

```rust
#[inline(always)]
fn parse_array<'de, V: JsonVisitor<'de>>(
    input: &'de [u8], p: &mut usize, s: &mut ScanState, v: &mut V,
) -> Result<(), ParseError> {
    v.begin_array(0)?;
    skip_space(input, p, s);
    if input.get(*p) == Some(&b']') { *p += 1; return v.end_array(0); }
    let mut n = 0usize;
    loop {
        parse_value(input, p, s, v)?;
        n += 1;
        skip_space(input, p, s);
        match input.get(*p).copied() {
            Some(b']') => { *p += 1; return v.end_array(n); }
            Some(b',') => { *p += 1; skip_space(input, p, s); }
            _ => return Err(ParseError::ExpectCommaOrEnd(*p)),
        }
    }
}
```

### `parse_string`

```rust
#[inline(always)]
fn parse_string<'de, V: JsonVisitor<'de>>(
    input: &'de [u8], p: &mut usize, _s: &mut ScanState, v: &mut V, is_key: bool,
) -> Result<(), ParseError> {
    let start = *p;
    // SIMD fast-scan for first b'"' or b'\\' using parity kernels from bbnf-simd-scan.
    // Borrow-safe hot-path: escape-free string slice.
    loop {
        let stripe = first_quote_or_backslash(&input[*p..]);  // Option<(usize, u8)>
        match stripe {
            Some((off, b'"')) => {
                let end = *p + off;
                // SAFETY: JSON string body is UTF-8-clean (decoder contract per bbnf-tape/src/tape.rs:664).
                let s = unsafe { std::str::from_utf8_unchecked(&input[start..end]) };
                *p = end + 1;
                return if is_key { v.key(s, true) } else { v.string(s, true) };
            }
            Some((off, b'\\')) => {
                *p += off;
                return parse_string_escaped(input, p, start, v, is_key); // cold
            }
            None => return Err(ParseError::UnterminatedString(start)),
            _ => unreachable!(),
        }
    }
}
```

### `parse_number`

```rust
#[inline(always)]
fn parse_number<'de, V: JsonVisitor<'de>>(
    input: &[u8], p: &mut usize, v: &mut V,
) -> Result<(), ParseError> {
    // Inline integer run + optional fraction + optional exponent.
    let start = *p;
    if input[*p] == b'-' { *p += 1; }
    while let Some(b) = input.get(*p) { if (*b).is_ascii_digit() { *p += 1; } else { break; } }
    let mut has_frac = false;
    if input.get(*p) == Some(&b'.') {
        has_frac = true; *p += 1;
        while let Some(b) = input.get(*p) { if (*b).is_ascii_digit() { *p += 1; } else { break; } }
    }
    if matches!(input.get(*p), Some(b'e') | Some(b'E')) {
        *p += 1;
        if matches!(input.get(*p), Some(b'+') | Some(b'-')) { *p += 1; }
        while let Some(b) = input.get(*p) { if (*b).is_ascii_digit() { *p += 1; } else { break; } }
    }
    // Inline Eisel-Lemire for the common (≤19-digit mantissa) path.
    // Fallback to f64::from_str for long mantissas / subnormals.
    let bytes = unsafe { input.get_unchecked(start..*p) };
    let f: f64 = eisel_lemire_fast(bytes).unwrap_or_else(|| {
        let s = unsafe { std::str::from_utf8_unchecked(bytes) };
        s.parse().unwrap_or(f64::NAN)
    });
    let _ = has_frac;
    v.number_f64(f)
}
```

## Appendix B — two visitors

```rust
/// Materialises into a Rust enum mirroring `sonic_rs::Value`.
pub struct ValueVisitor<'de> {
    stack: Vec<Build<'de>>,                    // compound building stack
    root:  Option<Value<'de>>,
}
impl<'de> JsonVisitor<'de> for ValueVisitor<'de> { /* push on begin, build+commit on end */ }

/// Materialises into the tape Columns substrate; AW-IV-compatible.
pub struct TapeVisitor<'de, 'src> {
    tb: TapeBuilder<'de>,                      // bbnf-tape
    source: &'src [u8],
    // hint-driven push_compound so no post-pass derive_frame_depth needed.
}
impl<'de, 'src> JsonVisitor<'src> for TapeVisitor<'de, 'src> {
    fn begin_object(&mut self, _hint: usize) -> Result<(), ParseError> {
        self.tb.push_compound_open(TapeKind::Struct); Ok(())     // bbnf-tape::builder
    }
    fn end_object(&mut self, _n: usize) -> Result<(), ParseError> {
        self.tb.push_compound_close(TapeKind::Struct); Ok(())
    }
    fn string(&mut self, s: &'src str, borrowed: bool) -> Result<(), ParseError> {
        if borrowed {
            self.tb.push_leaf_borrowed_string(s);                // tape.rs:646 path
        } else {
            self.tb.push_leaf_with_arena_frame(s.as_bytes());    // decoded path
        }
        Ok(())
    }
    fn number_f64(&mut self, v: f64) -> Result<(), ParseError> {
        self.tb.push_leaf_with(TapeKind::Regex, PayloadData::WideScalar(v.to_bits())); Ok(())
    }
    fn bool(&mut self, v: bool) -> Result<(), ParseError> {
        self.tb.push_leaf_with(TapeKind::Literal, PayloadData::InlineScalar(v as u32)); Ok(())
    }
    fn null(&mut self) -> Result<(), ParseError> {
        self.tb.push_leaf_with(TapeKind::Literal, PayloadData::InlineScalar(0)); Ok(())
    }
    fn key(&mut self, k: &'src str, borrowed: bool) -> Result<(), ParseError> { self.string(k, borrowed) }
    /* begin/end_array, end_object symmetric */
}
```

Both compile under the same `parse_json::<V>` body. The two resulting binaries differ only in the monomorphised visitor methods — the 13× gap closure is validated through `ValueVisitor` (sonic's shape), and the architectural wiring back to AW-IV lands through `TapeVisitor` (bbnf's shape).
