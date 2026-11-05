//! Runtime kernel dispatch.
//!
//! Every ISA tier whose kernel can exist on the build's target
//! architecture is compiled into a default build; which one runs is
//! decided once, at runtime, by CPU-feature detection and cached in a
//! [`OnceLock`]. A kernel's body is compiled under
//! `#[target_feature(enable = ...)]`, so the build's baseline
//! `target_feature` set never hides a tier.
//!
//! [`scan_with`] runs a named tier (or reports that the host cannot run
//! it); it is how the differential tests hold every supported tier to
//! the [`scalar`](crate::scalar) oracle.

use std::sync::OnceLock;

use crate::StructuralIndex;
use crate::alphabet::StructuralAlphabet;

/// One structural-scan kernel tier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Tier {
    /// Portable byte-at-a-time reference ([`crate::scalar`]).
    Scalar,
    /// aarch64 Advanced SIMD (baseline on every aarch64 CPU).
    Neon,
    /// x86_64 AVX2.
    Avx2,
    /// x86_64 AVX-512 F + BW + VBMI2.
    Avx512,
    /// wasm32 SIMD128.
    Simd128,
}

impl Tier {
    /// Every tier, best first: the order [`selected_tier`] searches.
    pub const PREFERENCE: [Tier; 5] = [
        Tier::Avx512,
        Tier::Avx2,
        Tier::Neon,
        Tier::Simd128,
        Tier::Scalar,
    ];

    /// Stable lowercase name (used in records and test messages).
    pub const fn name(self) -> &'static str {
        match self {
            Tier::Scalar => "scalar",
            Tier::Neon => "neon",
            Tier::Avx2 => "avx2",
            Tier::Avx512 => "avx512",
            Tier::Simd128 => "simd128",
        }
    }

    /// Whether this build contains the tier's kernel.
    pub const fn is_compiled(self) -> bool {
        match self {
            Tier::Scalar => true,
            Tier::Neon => cfg!(target_arch = "aarch64"),
            Tier::Avx2 | Tier::Avx512 => cfg!(target_arch = "x86_64"),
            Tier::Simd128 => cfg!(target_arch = "wasm32"),
        }
    }

    /// Whether this build contains the tier **and** the running CPU
    /// supports it.
    pub fn is_supported(self) -> bool {
        if !self.is_compiled() {
            return false;
        }
        match self {
            Tier::Scalar | Tier::Neon | Tier::Simd128 => true,
            #[cfg(target_arch = "x86_64")]
            Tier::Avx2 => std::is_x86_feature_detected!("avx2"),
            #[cfg(target_arch = "x86_64")]
            Tier::Avx512 => {
                std::is_x86_feature_detected!("avx512f")
                    && std::is_x86_feature_detected!("avx512bw")
                    && std::is_x86_feature_detected!("avx512vbmi2")
            }
            #[allow(unreachable_patterns)]
            _ => false,
        }
    }
}

/// The tiers this build contains, best first.
pub fn compiled_tiers() -> Vec<Tier> {
    Tier::PREFERENCE
        .into_iter()
        .filter(|t| t.is_compiled())
        .collect()
}

/// The best tier the running CPU supports. Detected once per process.
pub fn selected_tier() -> Tier {
    static SELECTED: OnceLock<Tier> = OnceLock::new();
    *SELECTED.get_or_init(|| {
        Tier::PREFERENCE
            .into_iter()
            .find(|t| t.is_supported())
            .unwrap_or(Tier::Scalar)
    })
}

/// Run `tier`'s kernel, or `None` when the build or the host cannot
/// run it. Output is byte-identical to [`crate::scalar::scan`] for
/// every tier that returns `Some`.
pub fn scan_with(
    tier: Tier,
    input: &[u8],
    alphabet: &StructuralAlphabet,
) -> Option<StructuralIndex> {
    // SAFETY: `run`'s precondition is exactly `is_supported`.
    tier.is_supported()
        .then(|| unsafe { run(tier, input, alphabet) })
}

/// [`scan_with`] on [`selected_tier`], which is supported by
/// construction.
pub(crate) fn scan_selected(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    // SAFETY: `selected_tier` only returns a supported tier.
    unsafe { run(selected_tier(), input, alphabet) }
}

/// # Safety
/// `tier.is_supported()`: the kernel's target features are present.
unsafe fn run(tier: Tier, input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    if alphabet.singletons.is_empty() && alphabet.digraph_pairs.is_empty() {
        return StructuralIndex::new();
    }
    match tier {
        #[cfg(target_arch = "aarch64")]
        Tier::Neon => crate::neon::scan(input, alphabet),
        // SAFETY (both x86 arms): the caller guarantees the features.
        #[cfg(target_arch = "x86_64")]
        Tier::Avx2 => unsafe { crate::avx2::scan(input, alphabet) },
        #[cfg(target_arch = "x86_64")]
        Tier::Avx512 => unsafe { crate::avx512::scan(input, alphabet) },
        #[cfg(target_arch = "wasm32")]
        Tier::Simd128 => crate::wasm::scan(input, alphabet),
        _ => crate::scalar::scan(input, alphabet),
    }
}
