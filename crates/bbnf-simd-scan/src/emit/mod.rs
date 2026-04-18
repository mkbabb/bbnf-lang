//! Body-fragment exporters for the SIMD kernels.
//!
//! # Role (AW-V.W1.2)
//!
//! The AW-V per-shape emitter (`crates/core/src/backend/rust/emitter/
//! shapes/*.rs`, W3.2) splices SIMD kernel bodies **inline at the
//! per-rule scan call site** to eliminate the cross-crate boundary the
//! AW-IV `nm` audit identified. This module exposes each kernel's body
//! source as a [`proc_macro2::TokenStream`] the per-shape emitter can
//! paste into its output.
//!
//! Every fragment is paired with the runtime kernel of the same name
//! in the sibling arch modules ([`crate::neon`], [`crate::avx2`],
//! [`crate::scalar`], [`crate::parity`], [`crate::compaction`]). The
//! runtime kernels survive verbatim — they remain the cold-path
//! reference and carry the `#[inline(always)]` + workspace-LTO discipline
//! the AW-V.W2 prototype relied on to beat sonic-rs.
//!
//! # Contract
//!
//! Each fragment module exposes:
//!
//! - A `SOURCE` `&'static str` (or per-arch `SOURCE_NEON` / `SOURCE_AVX2`
//!   / `SOURCE_SCALAR`) carrying the verbatim inner body of the kernel.
//!   The string is self-sufficient Rust — it parses under
//!   [`syn::parse_str`] without surrounding fn / block scaffolding added
//!   by the caller.
//! - A `fragment()` fn (or per-arch `fragment_neon()` / `fragment_avx2()`
//!   / `fragment_scalar()`) returning the parsed
//!   [`proc_macro2::TokenStream`]. Panics at build time if the source
//!   fails to parse — this is intentional: a stale fragment should not
//!   silently corrupt the emitter output, it should halt codegen.
//!
//! The module-level doc on each fragment declares the splice-target
//! [`syn`] type (`syn::Block` / `syn::Expr` / `syn::Stmt`) the per-shape
//! emitter is expected to use. The paired test in
//! `tests/emit_fragments.rs` asserts this by calling `fragment()` and
//! confirming the output parses under the declared type.
//!
//! # Per-arch splice strategy
//!
//! Kernels whose body varies across architecture (`nibble_lut_scan`,
//! `nospace64_scan`, `first_quote_or_backslash`) expose separate
//! `SOURCE_NEON` / `SOURCE_AVX2` / `SOURCE_SCALAR` constants rather than
//! a single body wrapped in `#[cfg]` attributes. Per AW-V.W3.2 each
//! per-shape emitter knows its target architecture at codegen time
//! (via the emitter's `EmitContext`), so it splices the right fragment
//! into a `cfg(target_arch = …)` arm in the generated code. Keeping
//! each arch body un-wrapped in the fragment keeps the splice contract
//! simple — the emitter owns the `cfg` gate, the fragment owns the body.
//!
//! Kernels whose body is architecture-neutral (`shift_xor_parity`,
//! `tzcnt_compact`, `quoted_string_simd_body`, `eisel_lemire_body`,
//! `multi_cmp_scan`, `clmul_parity`) expose a single `SOURCE` constant.
//! The CLMUL variant is gated at fragment-splice time by the consuming
//! emitter's `cfg(target_feature = "aes")` / `cfg(target_feature =
//! "pclmulqdq")` check, not inside the fragment.

pub mod clmul_parity;
pub mod eisel_lemire_body;
pub mod first_quote_or_backslash;
pub mod multi_cmp_scan;
pub mod nibble_lut_scan;
pub mod nospace64_scan;
pub mod quoted_string_simd_body;
pub mod shift_xor_parity;
pub mod tzcnt_compact;
