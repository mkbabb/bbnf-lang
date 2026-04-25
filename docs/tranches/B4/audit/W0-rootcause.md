# B4.W0 — Root Cause: SIMD Bitmap Kernel Emits Bare-Path After Labelled `break`

## Panic location

`crates/core/src/generate/regex/emit/simd.rs:179` and
`crates/core/src/generate/regex/emit/simd.rs:200`. The two
`break 'avx2_scan ::core::option::Option::Some(...)` forms inside
`emit_structural_bitmap_kernel` produce TokenStream fragments that
`syn::parse2` rejects with "expected loop or block expression".

## Root cause

After parsing `break LIFETIME`, syn's expression parser admits only a
loop expression, a brace block, or a labelled-block expression as the
break-value. A bare `::path::Item(...)` does not match any of those
forms, so the parser fails with the diagnostic the surface text
quotes verbatim. Rust's reference allows `break LIFETIME EXPR` for
arbitrary EXPR at compile time, but that wider grammar requires
disambiguation that syn (and earlier rustc parser passes) does not
perform — the precedent is that `break 'label EXPR` either takes a
block-shaped EXPR directly or a parenthesised expression. The
emitter shipped a bare path expression, which is neither.

The bbnf grammar exercises the prettify codegen path that fans out
through `emit_structural_bitmap_kernel` for negated-character-class
regexes (`/[^*]*/`, `/.*/`, `/[^)]*/`, etc.) appearing in rules
selected by `@pretty` directives. The json grammar's prettify scope
(if any) does not invoke this kernel; the bbnf grammar's
self-host scope does, on multiple `*_prettify` functions
(`__big_comment_prettify`, `__comment_prettify`,
`__pretty_hint_prettify`, `__grammar_item_prettify`). The defect is
not bbnf-specific — every grammar that triggers the SIMD kernel under
prettify codegen would surface the same panic; bbnf is just the
first grammar to hit that combination after the parser-baseline-
restoration substrate landed.

## Fix

`crates/core/src/generate/regex/emit/simd.rs:177-185, 196-205` wrap
each break-value in a brace block so the emitted form is
`break 'avx2_scan { ::core::option::Option::Some(...) }`. The block
satisfies syn's "loop or block expression" expectation while
preserving identical runtime semantics — a labelled break with a
single-expression block body is byte-for-byte equivalent to the
unwrapped form rustc accepts.

Excerpt of the changed emitter (one of two sites; the second site is
identical except for the body expression):

```rust
if __mask != 0 {
    let __rel = __mask.trailing_zeros() as usize;
    break 'avx2_scan {
        ::core::option::Option::Some(
            (__i + __rel) - __start,
        )
    };
}
```

The fix is single-source at the emitter (`simd.rs`, two adjacent
quote-block sites), no shadow surface at the consumer, no
`try`-fallback at the `syn::parse2` callsite, no compositional
change elsewhere in the codegen pipeline.

## Verification

`cargo xtask regen --grammar bbnf` exits 0 post-fix; the bbnf grammar
emits a populated `crates/core/src/grammar/generated/bbnf.rs` file
whose line count matches the same order of magnitude as the existing
`crates/core/src/grammar/generated.rs`. `cargo xtask regen --grammar
json` continues to exit 0 (regression sanity). `cargo check -p bbnf
-p xtask -p bbnf_derive --profile ax-iter` exits 0. `cargo nextest
run -p tape --profile ax-iter` exits 0 with the tape suite's full
parser-baseline coverage intact.

Numeric evidence lives in the wave-side audit files
(`W0-verify-bbnf.txt`, `W0-verify-json.txt`,
`W0-workspace-check.txt`, `W0-tape-tests.txt`).
