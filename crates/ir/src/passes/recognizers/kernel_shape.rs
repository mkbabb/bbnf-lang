//! Kernel-shape selector pass — mechanically chooses the SIMD kernel
//! shape that the `bbnf-simd-scan` crate dispatches at scan time and
//! that the W6 emitter consults when const-folding the per-grammar
//! `scan_structural_<grammar>` entry function.
//!
//! ## Design (AW-III.W5.a)
//!
//! Per `docs/tranches/AW/AW-III.md` §W5, kernel-shape selection is
//! mechanical and grammar-name-blind:
//!
//! - `|singletons| ≤ 8` → nibble-LUT collapse (one `vqtbl1q_u8` per
//!   16-byte lane on NEON; `vpshufb` per 16-byte lane on AVX2).
//! - `9 ≤ |singletons| ≤ 16` → wide-LUT (lifts the AU.2.7 v2 `1 << i`
//!   membership cap; the `bbnf-simd-scan::neon` wide variant matches).
//! - `|singletons| > 16` → multi-pass cmpeq + OR-reduce.
//! - `|digraph_pairs| > 0` → kernel folds `vextq_u8` shifted-compare
//!   per pair, OR'd into the structural mask before compaction.
//! - `|quote_classes| > 0` → kernel folds CLMUL/PMULL parity (x86) or
//!   6-op shift-XOR (NEON) before compaction so inside-string bytes
//!   are masked off.
//!
//! The pass returns a [`KernelStrategy`] describing all four lever
//! decisions; `bbnf-simd-scan::scan_structural` reads the strategy at
//! scan time. At W6 the emitter will read the same struct to produce
//! one tightly-specialised entry function per grammar.
//!
//! ## §6 generalisation
//!
//! No grammar-name conditionals; selection consumes only IR-mined
//! cardinality. A grammar with a 4-byte alphabet picks `NibbleLut`
//! whether it is JSON, CSS, or a hand-written test fixture; the
//! mechanism does not vary, only the data does.

use crate::passes::sets::StructuralAlphabet;

/// Selected SIMD kernel shape for the singleton-byte scan path.
///
/// One of these is chosen per grammar by [`select_kernel_strategy`]
/// from the cardinality of [`StructuralAlphabet::single_bytes`]. The
/// thresholds are pluggable via the cost model — today they match the
/// architectural-lever boundaries from AW-III.W5.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernelShape {
    /// `|singletons| ≤ 8` — nibble-LUT collapse. One `vqtbl1q_u8` per
    /// 16-byte lane on NEON; `vpshufb` per 16-byte lane on AVX2.
    NibbleLut,

    /// `9 ≤ |singletons| ≤ 16` — wide nibble-LUT with full membership
    /// (no `1 << i` cap). Same intrinsic count, wider mask payload.
    WideLut,

    /// `|singletons| > 16` — multi-pass `cmpeq` + OR-reduce. One pass
    /// per byte; results OR'd into a single structural mask. Worst-
    /// case ladder; chosen only when the alphabet is too dense for
    /// either LUT shape.
    MultipassCmpEq,
}

impl KernelShape {
    /// Cardinality boundary between [`KernelShape::NibbleLut`] and
    /// [`KernelShape::WideLut`]. Inclusive — a singleton count of 8
    /// uses [`KernelShape::NibbleLut`].
    pub const NIBBLE_LUT_MAX: usize = 8;

    /// Cardinality boundary between [`KernelShape::WideLut`] and
    /// [`KernelShape::MultipassCmpEq`]. Inclusive — a singleton count
    /// of 16 uses [`KernelShape::WideLut`].
    pub const WIDE_LUT_MAX: usize = 16;

    /// Choose the kernel shape from a singleton count.
    pub const fn for_singleton_count(count: usize) -> Self {
        if count <= Self::NIBBLE_LUT_MAX {
            Self::NibbleLut
        } else if count <= Self::WIDE_LUT_MAX {
            Self::WideLut
        } else {
            Self::MultipassCmpEq
        }
    }
}

/// Per-grammar kernel strategy. Carries the singleton-shape decision
/// plus the two boolean levers controlling whether the kernel folds in
/// digraph detection and quote-parity correction.
///
/// Consumed at scan time by `bbnf-simd-scan::scan_structural` and at
/// emit time by the W6 const-fold pass.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KernelStrategy {
    /// Selected singleton-byte kernel shape — see [`KernelShape`].
    pub singleton_kernel: KernelShape,

    /// Whether the kernel must fold a digraph-detection pass into the
    /// scan. `true` iff `alphabet.digraphs` is non-empty.
    pub has_digraphs: bool,

    /// Whether the kernel must fold a quote-parity pass into the
    /// scan. `true` iff `alphabet.quote_classes` is non-empty.
    pub has_quote_parity: bool,
}

impl KernelStrategy {
    /// Empty-alphabet identity strategy. The kernel falls through to
    /// scalar memchr semantics; the SIMD pre-pass amortises nothing.
    pub const EMPTY: KernelStrategy = KernelStrategy {
        singleton_kernel: KernelShape::NibbleLut,
        has_digraphs: false,
        has_quote_parity: false,
    };
}

/// Mechanically select the SIMD kernel strategy for an alphabet.
///
/// The selection is a pure function of singleton cardinality, digraph
/// presence, and quote-class presence — no grammar identity, no
/// pluggable per-grammar branch. The thresholds derive from the
/// architectural-lever boundaries declared in `docs/tranches/AW/
/// AW-III.md` §W5.
pub fn select_kernel_strategy(alphabet: &StructuralAlphabet) -> KernelStrategy {
    KernelStrategy {
        singleton_kernel: KernelShape::for_singleton_count(alphabet.single_bytes.len()),
        has_digraphs: !alphabet.digraphs.is_empty(),
        has_quote_parity: !alphabet.quote_classes.is_empty(),
    }
}

// Tests live in tests/kernel_shape.rs (crate-level).
