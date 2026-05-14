pub mod aarch64;
pub mod classifier;
pub mod dispatch;
pub mod scalar;
pub mod x86_64;

pub use classifier::{ClassifyResult, SimdClassifier};

pub fn select_classifier(alphabet: &'static [u8; 64]) -> dispatch::SelectedClassifier {
    dispatch::select_classifier(alphabet)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ScanBackend {
    Scalar,
    Neon,
}

#[derive(Clone)]
pub struct StructuralAlphabet {
    table: [bool; 256],
}

impl StructuralAlphabet {
    pub const fn from_bytes(bytes: &[u8]) -> Self {
        let mut table = [false; 256];
        let mut cursor = 0;
        while cursor < bytes.len() {
            table[bytes[cursor] as usize] = true;
            cursor += 1;
        }
        Self { table }
    }

    #[inline]
    pub fn contains(&self, byte: u8) -> bool {
        self.table[byte as usize]
    }
}

#[inline]
pub(crate) fn alphabet_contains(alphabet: &[u8; 64], byte: u8) -> bool {
    alphabet
        .iter()
        .copied()
        .take_while(|member| *member != 0)
        .any(|member| member == byte)
}

impl std::fmt::Debug for StructuralAlphabet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let members: Vec<u8> = (0..=255)
            .filter(|byte| self.table[*byte])
            .map(|byte| byte as u8)
            .collect();
        f.debug_tuple("StructuralAlphabet").field(&members).finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructuralIndex {
    positions: Vec<u32>,
    backend: ScanBackend,
}

impl StructuralIndex {
    pub fn from_positions(positions: Vec<u32>, backend: ScanBackend) -> Self {
        Self { positions, backend }
    }

    pub fn positions(&self) -> &[u32] {
        &self.positions
    }

    pub fn into_positions(self) -> Vec<u32> {
        self.positions
    }

    pub fn backend(&self) -> ScanBackend {
        self.backend
    }

    pub fn parity_hash(&self, input: &[u8]) -> [u8; 32] {
        hash_positions(input, &self.positions)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScalarParityReport {
    pub input_len: usize,
    pub structural_count: usize,
    pub hash: [u8; 32],
}

pub fn scan_dispatch(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    scan_scalar(input, alphabet)
}

pub fn scan_scalar(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    StructuralIndex::from_positions(scalar_positions(input, alphabet), ScanBackend::Scalar)
}

pub fn scalar_parity_report(input: &[u8], alphabet: &StructuralAlphabet) -> ScalarParityReport {
    let positions = scalar_positions(input, alphabet);
    ScalarParityReport {
        input_len: input.len(),
        structural_count: positions.len(),
        hash: hash_positions(input, &positions),
    }
}

#[inline]
pub fn active_backend() -> ScanBackend {
    ScanBackend::Scalar
}

fn scalar_positions(input: &[u8], alphabet: &StructuralAlphabet) -> Vec<u32> {
    let mut positions = Vec::new();
    for (offset, byte) in input.iter().copied().enumerate() {
        if alphabet.contains(byte) {
            positions.push(checked_position(offset));
        }
    }
    positions
}

#[inline]
pub fn checked_position(offset: usize) -> u32 {
    u32::try_from(offset).expect("skinny hot path supports inputs up to u32::MAX bytes")
}

fn hash_positions(input: &[u8], positions: &[u32]) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    hasher.update(&(input.len() as u64).to_le_bytes());
    for position in positions {
        hasher.update(&position.to_le_bytes());
        hasher.update(&[input[*position as usize]]);
    }
    *hasher.finalize().as_bytes()
}

#[inline]
pub fn prefix_xor_64(mut mask: u64, carry_in: bool) -> u64 {
    mask ^= mask << 1;
    mask ^= mask << 2;
    mask ^= mask << 4;
    mask ^= mask << 8;
    mask ^= mask << 16;
    mask ^= mask << 32;
    if carry_in {
        !mask
    } else {
        mask
    }
}

#[inline]
pub fn escape_mask_64(bs_mask: u64, bs_carry_in: bool) -> (u64, bool) {
    const EVEN_BITS: u64 = 0x5555_5555_5555_5555;
    const ODD_BITS: u64 = 0xAAAA_AAAA_AAAA_AAAA;

    let starts = bs_mask & !(bs_mask << 1);
    let carry_continues = bs_carry_in && (bs_mask & 1) == 1;
    let starts_eff = if carry_continues { starts & !1 } else { starts };
    let (even_starts, odd_starts) = if carry_continues {
        (starts_eff & EVEN_BITS, (starts_eff & ODD_BITS) | 1)
    } else {
        (starts_eff & EVEN_BITS, starts_eff & ODD_BITS)
    };

    let (even_carries, _) = bs_mask.overflowing_add(even_starts);
    let even_escape = (even_carries & !bs_mask) & ODD_BITS;
    let (odd_carries, _) = bs_mask.overflowing_add(odd_starts);
    let odd_escape = (odd_carries & !bs_mask) & EVEN_BITS;
    let mut escape = even_escape | odd_escape;

    if bs_carry_in && (bs_mask & 1) == 0 {
        escape |= 1;
    }

    let new_carry = if (bs_mask & (1u64 << 63)) == 0 {
        false
    } else if bs_mask == u64::MAX {
        bs_carry_in
    } else {
        bs_mask.leading_ones() % 2 == 1
    };
    (escape, new_carry)
}

pub fn compact_mask(input: &[u8], base: usize, mask: u64, positions: &mut Vec<u32>) {
    let mut bits = mask;
    while bits != 0 {
        let offset = bits.trailing_zeros() as usize;
        let position = base + offset;
        debug_assert!(input.get(position).is_some());
        positions.push(checked_position(position));
        bits &= bits - 1;
    }
}

// ============================================================================
// BYTE_CLASS_FROM_EQ_SET_64 — first reference primitive
// Contract documented in ext/x86/bbnf.asm; scalar reference is the executable
// specification per the checkasm admission gate (Lock 16).
// ============================================================================

pub mod prim {
    /// Returns a 64-bit mask where bit i is set iff src[i] ∈ set.
    /// set must have len() ≤ 8.
    #[inline]
    pub fn byte_class_from_eq_set_64(src: &[u8; 64], set: &[u8]) -> u64 {
        debug_assert!(set.len() <= 8);
        #[cfg(all(target_arch = "x86_64", target_feature = "avx512bw"))]
        unsafe {
            return crate::x86_64::byte_class_from_eq_set_64::byte_class_from_eq_set_64(src, set);
        }
        #[cfg(target_arch = "aarch64")]
        return crate::aarch64::byte_class_from_eq_set_64::byte_class_from_eq_set_64_neon(src, set);
        #[allow(unreachable_code)]
        crate::scalar::byte_class_from_eq_set_64::byte_class_from_eq_set_64_scalar(src, set)
    }
}
