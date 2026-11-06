//! Scalar reference for `STRUCTURAL_CLASSIFY_64` — the executable specification.
//!
//! Contract (simdjson-stage-1 structural classification, grammar-NEUTRAL):
//!   Input  — 64 contiguous source bytes + a `KindPartition`: the caller's
//!            structural byte-set partitioned into kind buckets
//!            (`open`/`close`/`quote`/`slash`/`stop`). Every member byte is
//!            CALLER DATA (grammar-derived; the kernel names no grammar byte).
//!   Output — a `StructuralClassify64`: a `significant` u64 (bit `i` set iff
//!            `src[i]` is in ANY bucket) plus one u64 per kind bucket (bit `i`
//!            set iff `src[i]` is a member of that bucket). The shell
//!            CTZ-iterates `significant` and reads each structural's KIND from
//!            the kind masks by bit-test — NO re-load, NO re-match.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift): the scalar reference is
//!     the parity anchor for the checkasm admission gate; the NEON body must
//!     agree with this implementation bit-for-bit on every (src, partition) pair
//!     the harness sweeps over the REAL css-l4 corpora.
//!   * simdjson stage-1 (Langdale & Lemire): one vectorized pass classifies a
//!     block into a significant bitmask + per-class masks; the scalar consumer
//!     iterates the bitmask instead of testing every byte. This is the strict
//!     scalar twin of that classification.
//!
//! Body status: source-of-truth. The NEON body (`aarch64/structural_classify_64`,
//! the `vceqq` fan) is a strictly correctness-equivalent fan-out.

/// The caller-supplied partition of the structural byte-set into kind buckets.
///
/// Grammar-NEUTRAL: the kernel takes the byte membership of every kind as CALLER
/// DATA, exactly like `byte_class_from_eq_set_64` takes its `set`. A `.bbnf`
/// mutation that widens the bracket family or the stop set changes THESE bytes,
/// never the kernel (the arg-mutation falsifier).
///
/// Each bucket is a `&[u8]` of the bytes belonging to that kind. The buckets are
/// expected to be pairwise disjoint (a byte has exactly one structural kind), but
/// the contract does not REQUIRE disjointness: a byte appearing in two buckets
/// simply sets both kind bits (and the `significant` bit once). The scalar
/// reference and the NEON body agree under either condition, so the checkasm
/// differential holds regardless.
#[derive(Copy, Clone, Debug)]
pub struct KindPartition<'a> {
    /// Open-bracket family (CSS `( [ {`).
    pub open: &'a [u8],
    /// Close-bracket family (CSS `) ] }`).
    pub close: &'a [u8],
    /// String-quote family (CSS `' "`).
    pub quote: &'a [u8],
    /// Comment-opening slash (CSS `/`).
    pub slash: &'a [u8],
    /// The grammar-derived STOP delimiters (the membership test, ≤4).
    pub stop: &'a [u8],
}

/// The classification of a 64-byte block: a significant bitmask plus one bitmask
/// per kind. Bit `i` corresponds to `src[i]`.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct StructuralClassify64 {
    /// Bit `i` set iff `src[i]` is in ANY kind bucket (the union). The shell
    /// CTZ-iterates this to visit every structural position once.
    pub significant: u64,
    /// Bit `i` set iff `src[i]` ∈ `partition.open`.
    pub open: u64,
    /// Bit `i` set iff `src[i]` ∈ `partition.close`.
    pub close: u64,
    /// Bit `i` set iff `src[i]` ∈ `partition.quote`.
    pub quote: u64,
    /// Bit `i` set iff `src[i]` ∈ `partition.slash`.
    pub slash: u64,
    /// Bit `i` set iff `src[i]` ∈ `partition.stop` (the membership test,
    /// vectorized).
    pub stop: u64,
}

/// Scalar reference for `STRUCTURAL_CLASSIFY_64`.
///
/// Classifies each of the 64 source bytes into its kind bucket(s) and sets the
/// corresponding kind-mask bit, with `significant` = the OR of all kind masks.
/// This is the byte-exact specification the NEON `vceqq` fan must reproduce.
#[inline]
pub fn structural_classify_64_scalar(
    src: &[u8; 64],
    partition: &KindPartition<'_>,
) -> StructuralClassify64 {
    let mut out = StructuralClassify64::default();
    for i in 0..64 {
        let byte = src[i];
        let bit = 1u64 << i;
        if partition.open.contains(&byte) {
            out.open |= bit;
        }
        if partition.close.contains(&byte) {
            out.close |= bit;
        }
        if partition.quote.contains(&byte) {
            out.quote |= bit;
        }
        if partition.slash.contains(&byte) {
            out.slash |= bit;
        }
        if partition.stop.contains(&byte) {
            out.stop |= bit;
        }
    }
    // `significant` is exactly the union of the kind buckets — the OR of the
    // per-kind masks (the NEON body produces it the same way: one extra `vorrq`).
    out.significant = out.open | out.close | out.quote | out.slash | out.stop;
    out
}
