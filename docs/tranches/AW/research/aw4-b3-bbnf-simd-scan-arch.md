# AW-IV B3 — `bbnf-simd-scan` integration with the per-shape inline emitter

**Worktree.** `bbnf-wt-aw4-brainstorm-b3`. Read-only against HEAD
`2ca0f7af` (post-W1.4-aggressive). All profile numbers cited from the
six `aw4-profile-p{1..6}-*.md` deliverables; all kernel references
cite `crates/bbnf-simd-scan/src/*.rs` file:line.

## Executive summary

Substrate complete; consumer inverted. `bbnf-simd-scan` exposes every
kernel a sonic-style per-shape walker wants — nibble/wide/multi-cmp
classifiers (`neon.rs:59..407`), CLMUL-or-6-op-shift-XOR parity
(`parity.rs:40..204`), tzcnt compaction (`compaction.rs:27..67`),
`StructuralIndex` wire type (`stage1.rs:50..107`). JSON twitter pays
3-5% on `neon::scan_nibble` today (P5 ranks 5-10) yet the 12.87× sonic
gap persists because `advance_or_pop_with` (9-13%) + `walk_cursor`
(12-14%) + `finalise` fold (10-13%) consume 33% of cycles downstream
(P5:76-88). Transposition is NOT inside `bbnf-simd-scan` — it is at
the emitter: stage-1 stays unchanged for general grammars; a JSON-
family per-shape emitter splices kernel **bodies** (not calls) into
`parse_object_<g>` / `parse_array_<g>` / `parse_string_<g>` /
`parse_number_<g>`, using the same pattern
`dta_walker::decoders::emit_neon_string_scan_inline_body`
(`decoders.rs:451..611`) pioneers. Pluggable IR-cardinality gate
selects stage-1 vs inline-in-loop at emit time.

## 1. Stage-1 firing diagnosis on CSS

Index is built, walker doesn't consume it. Prelude
(`emitter/grammar.rs:436..444`) builds `STRUCTURAL_ALPHABET::from_
profile(&GRAMMAR_PROFILE)` and runs `scan_structural`; `Cursor`
threads in at `dta_walker/mod.rs:278..283`. CSS normalize has
`try_branch` + `dispatch_one` = 25.33% self-time (perf-02:88-92);
CSS sheets P3 shows `try_branch` 52-72% invariant across input size
(P3:20,42,61). The slot-indexed shortcut at
`Cursor::jump_to_next_structural` (`driver.rs:182..190`) is reachable
only from `ConsumeToNextStructural` — which the emitter rarely
selects. Stage-1 cost on bootstrap (280 KB, ~7% density): NEON
nibble-LUT is ~14 µops per 64-byte stripe (AW-III.R1 §3) = **~0.22
cyc/B** at 3.4 GHz, below what byte-stepping elimination would save
across the 93% non-structural tail. Stage-1 IS amortising; the
savings aren't collected because `try_branch` dominates a different
axis (savepoint/restore loops, not byte-stepping). Fixing this is a
walker-emitter task (B2 scope), not `bbnf-simd-scan`.

## 2. Per-shape inline kernel emission — API shape

**Both: `#[inline(always)] pub fn` for kernels PLUS body-fragment
APIs for the per-shape emitter.**

1. `scan_structural`, `scan_nibble`, `scan_wide`, `scan_multi`
   (`neon.rs:47..407`) stay `pub fn` — once-per-parse from the
   prelude; inlining them into the prelude bloats pointlessly.
2. Inner kernels (`classify_chunk_nibble`, `quote_stripe_masked`,
   `digraph_stripe`, `compact_stripe_tzcnt`) stay
   `#[inline(always)]` (already: `neon.rs:185,200,430,455`;
   `compaction.rs:27..67`) — fold into `scan_*` at LLVM time.
3. **NEW `bbnf-simd-scan::emit` sub-module** returns `TokenStream`
   fragments mirroring inner-kernel bodies. The per-shape emitter
   splices these. Precedent: `dta_walker/decoders.rs:451..611` does
   this ad-hoc for the NEON string scanner with compile-time
   `quote_byte` binding; the sub-module formalises it.

Why body-fragment rather than just `#[inline(always)] pub fn`:
per-shape emitter must bind `quote_byte`/`lo`/`hi`/`arena_off` at
the source level. LLVM cross-crate inliner under LTO regresses on
large bodies (P6:55-58 CSS walker 153.9 KB over L1i). Body-fragment
splicing keeps each per-shape function self-contained.

## 3. sonic-style inline-in-loop vs simdjson-style 2-stage

**Both, with IR-cardinality pluggable switch.** P5 §5 justifies:

| grammar | struct density | string share | lens |
|---|---:|---:|---|
| JSON twitter | 4.8% | 64% | sonic inline-in-loop |
| JSON canada | 9.9% | 0% (numeric) | sonic inline (f64) |
| CSS bootstrap | 5-7% | 16% | simdjson stage-1 |
| CSS tailwind | 7%+ | low | simdjson stage-1 |
| BBNF | 5-15% | low | simdjson stage-1 |
| Sheets | 10%+ | — | sonic (small inputs) |

`recognizers/kernel_shape.rs:119..126` produces `KernelStrategy`
today; extend with `prefer_inline_in_loop: bool` driven by
`estimated_string_byte_share > 0.30 && alt_density < 3.0`. For
JSON twitter the gate fires → emit per-shape with inline-in-loop
SIMD. For CSS bootstrap (16% strings, 25+ Alt branches per compound
selector), stage-1 stays. For both-on grammars the prelude still
builds the index cheaply; per-shape loops ignore it. **Not a fork**
— one emitter pass, two code-gen lenses, same `KernelStrategy`
mechanism already picking `NibbleLut` vs `WideLut` vs `MultiCmp`
(`alphabet.rs:137..147`). Grammar-name-blind; cardinality-driven.

## 4. Quote-parity + string scan for JSON

Every `parse_string_<g>` inlines a 3-tier body. Half-built already
in `dta_walker/decoders.rs:451..611`; B3 formalises it.

- **Tier 1 fast path — `memchr(b'"')`.** When no backslashes present
  (twitter realistic corpora ~0%), byte-position scan auto-vectorises
  to NEON `vceqq_u8` + `vshrn_n_u16 #4`. ~0.15 cyc/byte inside
  string body; covers ≥ 99% of twitter string invocations.
- **Tier 2 slow path — parity + backslash escape.** Splice
  `escape_mask_64` (`parity.rs:74..142`) + `prefix_xor_64`
  (`parity.rs:40..47`); 6-op shift-XOR or PMULL64. Already inline-
  body-ready at `decoders.rs:519..567`; merge into
  `emit::scan_quoted_string_neon_body(quote_byte: u8)` so JSON/CSS
  emitters share one source.
- **Tier 3 scalar tail — ≤ 16 trailing bytes.** `decoders.rs:590..608`
  unchanged.

Per-byte contribution: inline memchr on 64% of twitter bytes at ~0.15
cyc/B = ~0.10 cyc/B amortised. Current `psi::write_decoded` is 5.7%
of 19.36 cyc/B ≈ 1.10 cyc/B (P5:76-88); inline bypasses PSI schedule
entirely → ~1.0 cyc/B recovered on twitter.

## 5. Number scan + Eisel-Lemire inline

Already drafted at `decoders.rs:80..411` — full Eisel-Lemire body
(mantissa + exponent + sign + Clinger fast-path + 128-bit
`POWER_OF_FIVE_128`) inline into `Map { Regex, F64 }` arms (JSON
`number`, CSS `<number>`, Sheets numerics). Today writes 8 LE bytes
into `pay_agg`. For per-shape `parse_number_<g>`:

```rust
let n = { /* eisel_lemire body yielding Option<f64> */ }
    .ok_or(DtaError::Syntax { .. })?;
visitor.visit_number(n);   // monomorphic, no vtable
```

f64 never touches tape/arena; register → visitor in one step,
matching sonic's `parse_number` (P5:43). NEON 17-digit fraction
SWAR (AT.4.3 chronic): optional refinement, gated on IR
`mantissa_len ≥ 9`; +0.05-0.10 cyc/B on canada (90% numeric).
Not load-bearing for the sonic-gap prototype — can land AW-V.

## 6. Nospace bitmap caching — reachable from per-shape loops

Yes. `parse-that::state.ws_bitmap` ≡ sonic's cached `nospace_bits`
(P5 §3). Emitter routes `DtaState::WsTrim` through
`Cursor::jump_to_next_structural` (`driver.rs:182..190`); for
per-shape, body-fragment API exposes
`emit::skip_whitespace_neon_body() -> TokenStream` which splices
64-bit nospace-bitmap into every per-shape loop. citm whitespace
(71%, P1 §5) currently pays 14.7% at `advance_or_pop_with`; inline
collapses to `trailing_zeros()` per non-WS byte, ~0.05 cyc/B.

## 7. Per-kernel inline-body vs inline-always-fn

| kernel | strategy | rationale |
|---|---|---|
| `scan_structural` | `pub fn` | once-per-parse from prelude |
| `scan_nibble`/`wide`/`multi` | `pub fn` | dispatched from `scan_structural` |
| `classify_chunk_nibble` | `#[inline(always)]` | hot inner; folds into `scan_*` |
| `quote_stripe_masked` | `#[inline(always)]` + body-fragment | stage-1 calls fn; per-shape splices body |
| `prefix_xor_64` | `#[inline(always)]` | 1-op CLMUL or 6-op shift-XOR |
| `compact_stripe_tzcnt` | `#[inline(always)]` | per-stripe; inlines cleanly |
| `memchr(b'"')` | body-fragment | per-shape splices 16-byte loop |
| Eisel-Lemire | body-fragment (already) | `decoders.rs:80..411`; literal bindings |
| `digit_chunk_swar` | body-fragment | per-arm `n_digits`; LLVM const-folds |

**Rule**: once-per-parse → `pub fn`. Many-times-in-hot-loops →
both `#[inline(always)]` reference impl AND body-fragment emit API.
Per-shape emitter prefers body-fragment; stage-1 prefers fn. One
source of truth per kernel (the `fn` body); emit API is a
`quote!{}` mirror verified by `cargo expand`-diff in
`crates/bbnf-simd-scan/tests/emit_parity.rs`.

## 8. Architecture transposition

No new crate. `bbnf-simd-scan` stays unchanged algorithmically.
Transposition is:

1. **Emitter** (`crates/core/src/backend/rust/emitter/`): new
   `per_shape/` module alongside `dta_walker/` emits
   `parse_{object,array,string,number}_<g>` when
   `prefer_inline_in_loop` fires; splices from `bbnf-simd-scan::emit`.
2. **`bbnf-simd-scan` addition**: new `pub mod emit` with
   body-fragment `TokenStream` APIs, one file per kernel (same
   shape as `crates/core/src/backend/kernels/`). Each diffed
   against the reference fn body via `tests/emit_parity.rs`. ~300
   LOC across `emit/{structural,quoted_string,quote_parity,
   skip_space,eisel_lemire,digit_swar}.rs`.
3. **Driver**: no change. Per-shape fns own control state on the
   CPU stack (mutual recursion); never touch `FrameStack` or
   `try_branch`. `Cursor` + `jump_to_next_structural` already exist
   (`driver.rs:143..190`) for the stage-1 path.

No fork: general grammars go through the walker; JSON-family
grammars additionally get per-shape functions called from the
walker's `Seq` arm. Stage-1 index is available in both paths (same
`Cursor`); per-shape chooses to not consult when inline SIMD is
cheaper.

## 9. Projected per-byte contribution on JSON twitter

| component | cyc/B |
|---|---:|
| stage-1 SIMD (retained) | 0.22 |
| inline `parse_object_<g>` byte-cmp | 0.30 |
| inline `parse_string_<g>` Tier-1 memchr | 0.15 |
| inline Eisel-Lemire (~2% bytes) | 0.02 |
| inline skip-WS (~26% bytes) | 0.05 |
| visitor `visit_*` monomorphic | 0.10 |
| **bbnf per-shape projected** | **~0.84** |
| sonic twitter measured (P5:18) | 1.51 |

Per-shape eliminates `walk_cursor` (2.34) + `advance_or_pop_with`
(1.83) + `finalise` fold (2.01) = **6.18 cyc/B** entirely; visitor
materialises value with no tape round-trip.

## 10. Generalisation handle

`recognizers/kernel_shape.rs` selects `KernelStrategy` today
(singleton cardinality + digraph + quote presence). B3 extends it
with one field `prefer_inline_in_loop: bool`, driven by existing /
new IR facts: `GrammarProfile::estimated_string_byte_share` (default
`None` → infer from `quote_classes.len()` > 0 + Alt density). The
`KernelStrategy` reaches both `scan_structural` (kernel shape at
scan time) AND the per-shape emitter (inline vs stage-1 at emit
time). Same IR fact, two consumers. Grammar-name-blind. Mechanism
identical to `NibbleLut` vs `WideLut` selection today
(`alphabet.rs:137..147`).

---

## Appendix A — Body-fragment API sketch

```rust
// crates/bbnf-simd-scan/src/emit/mod.rs
pub mod quoted_string;
pub mod quote_parity;
pub mod skip_space;
pub mod digit_swar;

// crates/bbnf-simd-scan/src/emit/quoted_string.rs
use proc_macro2::TokenStream;
use quote::quote;

/// Emit the 16-byte-chunked NEON quoted-string scanner body.
/// Mirrors the reference `fn` in `crate::neon::quote_stripe_masked`.
/// Expects `input: &[u8]` + `start: usize` in scope; binds
/// `quote_byte` as a compile-time literal so LLVM const-folds the
/// splat register setup. Returns `Option<usize>` — byte offset of
/// closing quote, or `None` if unterminated.
pub fn scan_quoted_string_neon_body(quote_byte: u8) -> TokenStream {
    let q = proc_macro2::Literal::u8_unsuffixed(quote_byte);
    quote! {
        '__sstring: {
            const __QUOTE: u8 = #q;
            // body here — verbatim copy of decoders.rs:465..608
            // (already drafted; migrated from dta_walker/decoders.rs
            // to here with grammar-agnostic contract).
        }
    }
}
```

## Appendix B — Per-shape emitter sketch (JSON object)

```rust
// crates/core/src/backend/rust/emitter/per_shape/object.rs
pub fn emit_parse_object(grammar: &str, shape: &ObjectShape) -> TokenStream {
    let fn_ident = format_ident!("parse_object_{}", grammar);
    let skip_ws = bbnf_simd_scan::emit::skip_whitespace_neon_body();
    let scan_str = bbnf_simd_scan::emit::scan_quoted_string_neon_body(b'"');
    quote! {
        #[inline]
        fn #fn_ident<V: JsonVisitor>(
            input: &[u8], pos: &mut usize, visitor: &mut V,
        ) -> Result<(), DtaError> {
            visitor.visit_object_start();
            debug_assert_eq!(input[*pos], b'{');
            *pos += 1;
            #skip_ws                       // inline SIMD WS skip
            if input.get(*pos) == Some(&b'}') { *pos += 1; visitor.visit_object_end(); return Ok(()); }
            loop {
                #skip_ws
                debug_assert_eq!(input[*pos], b'"');
                let start = *pos + 1;
                let end = { #scan_str };   // inline SIMD string scan
                visitor.visit_key(&input[start..end.unwrap()]);
                *pos = end.unwrap() + 1;
                #skip_ws
                debug_assert_eq!(input[*pos], b':'); *pos += 1;
                #skip_ws
                parse_value_<grammar>(input, pos, visitor)?;
                #skip_ws
                match input.get(*pos) {
                    Some(&b',') => { *pos += 1; continue; }
                    Some(&b'}') => { *pos += 1; visitor.visit_object_end(); return Ok(()); }
                    _ => return Err(DtaError::Syntax { .. }),
                }
            }
        }
    }
}
```

One function per compound shape. No `try_branch`, no `advance_or_pop_
with`, no `FrameStack`. Mutual recursion holds the parse stack; SIMD
bodies splice inline from `bbnf-simd-scan::emit`. The stage-1 index
is still built in the prelude (cheap) and remains available for any
grammar that prefers indexed dispatch — orthogonal, not rival.
