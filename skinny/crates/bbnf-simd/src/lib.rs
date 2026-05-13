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

    fn is_json_structural(&self) -> bool {
        (0..=255).all(|byte| self.contains(byte as u8) == JSON_STRUCTURAL.contains(byte as u8))
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

pub(crate) fn is_json_structural_alphabet(alphabet: &[u8; 64]) -> bool {
    const JSON: &[u8] = b"{}[],:\"";
    JSON.iter()
        .copied()
        .all(|byte| alphabet_contains(alphabet, byte))
        && alphabet
            .iter()
            .copied()
            .take_while(|member| *member != 0)
            .all(|byte| JSON.contains(&byte))
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

pub const JSON_STRUCTURAL: StructuralAlphabet = StructuralAlphabet::from_bytes(b"{}[],:\"");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructuralIndex {
    positions: Vec<u32>,
    backend: ScanBackend,
}

impl StructuralIndex {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonParseIndex {
    structural_offsets: Vec<u32>,
    string_escape_offsets: Vec<u32>,
    string_control_offsets: Vec<u32>,
}

impl JsonParseIndex {
    pub fn structural_offsets(&self) -> &[u32] {
        &self.structural_offsets
    }

    pub fn string_escape_offsets(&self) -> &[u32] {
        &self.string_escape_offsets
    }

    pub fn string_control_offsets(&self) -> &[u32] {
        &self.string_control_offsets
    }

    pub fn into_parts(self) -> (Vec<u32>, Vec<u32>, Vec<u32>) {
        (
            self.structural_offsets,
            self.string_escape_offsets,
            self.string_control_offsets,
        )
    }
}

pub fn scan_dispatch(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    if alphabet.is_json_structural() {
        return scan_json_structurals(input);
    }
    scan_scalar(input, alphabet)
}

pub fn scan_json_structurals(input: &[u8]) -> StructuralIndex {
    #[cfg(target_arch = "aarch64")]
    {
        return StructuralIndex {
            positions: neon::scan_json(input),
            backend: ScanBackend::Neon,
        };
    }

    #[allow(unreachable_code)]
    scan_scalar(input, &JSON_STRUCTURAL)
}

pub fn scan_json_parse_index(input: &[u8]) -> JsonParseIndex {
    #[cfg(target_arch = "aarch64")]
    {
        let (structural_offsets, string_escape_offsets, string_control_offsets) =
            neon::scan_json_parse(input);
        return JsonParseIndex {
            structural_offsets,
            string_escape_offsets,
            string_control_offsets,
        };
    }

    #[allow(unreachable_code)]
    {
        let (structural_offsets, string_escape_offsets, string_control_offsets) =
            scalar_json_parse_index(input);
        JsonParseIndex {
            structural_offsets,
            string_escape_offsets,
            string_control_offsets,
        }
    }
}

pub fn scan_scalar(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    StructuralIndex {
        positions: scalar_positions(input, alphabet),
        backend: ScanBackend::Scalar,
    }
}

#[inline(always)]
pub fn match_json_tiny_plain_string(input: &[u8], offset: usize) -> Option<usize> {
    match_json_tiny_plain_string_scalar(input, offset)
}

#[inline(always)]
pub fn match_json_tiny_plain_string_scalar(input: &[u8], offset: usize) -> Option<usize> {
    let mut cursor = offset + 1;
    let limit = (cursor + 8).min(input.len());
    while cursor < limit {
        match input[cursor] {
            b'"' => return Some(cursor + 1),
            b'\\' | 0x00..=0x1f => return None,
            _ => cursor += 1,
        }
    }
    None
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
    #[cfg(target_arch = "aarch64")]
    {
        return ScanBackend::Neon;
    }

    #[allow(unreachable_code)]
    ScanBackend::Scalar
}

fn scalar_positions(input: &[u8], alphabet: &StructuralAlphabet) -> Vec<u32> {
    if alphabet.is_json_structural() {
        let mut positions = Vec::with_capacity(input.len() / 8 + 8);
        scan_json_tail(input, 0, &mut positions, false, false);
        return positions;
    }

    let mut positions = Vec::new();
    for (offset, byte) in input.iter().copied().enumerate() {
        if alphabet.contains(byte) {
            positions.push(checked_position(offset));
        }
    }
    positions
}

fn scan_json_tail(
    input: &[u8],
    start: usize,
    positions: &mut Vec<u32>,
    mut in_string: bool,
    mut escaped: bool,
) {
    let mut cursor = start;
    while cursor < input.len() {
        let byte = input[cursor];
        if in_string {
            if escaped {
                escaped = false;
            } else if byte == b'\\' {
                escaped = true;
            } else if byte == b'"' {
                positions.push(checked_position(cursor));
                in_string = false;
            }
            cursor += 1;
            continue;
        }

        if byte == b'"' {
            positions.push(checked_position(cursor));
            in_string = true;
        } else if is_json_punctuation(byte) {
            positions.push(checked_position(cursor));
        }
        cursor += 1;
    }
}

fn scan_json_tail_parse(
    input: &[u8],
    start: usize,
    positions: &mut Vec<u32>,
    string_escapes: &mut Vec<u32>,
    string_controls: &mut Vec<u32>,
    mut in_string: bool,
    mut escaped: bool,
) {
    let mut cursor = start;
    while cursor < input.len() {
        let byte = input[cursor];
        if in_string {
            if escaped {
                escaped = false;
            } else if byte == b'\\' {
                string_escapes.push(checked_position(cursor));
                escaped = true;
            } else if byte == b'"' {
                positions.push(checked_position(cursor));
                in_string = false;
            } else if byte < 0x20 {
                string_controls.push(checked_position(cursor));
            }
            cursor += 1;
            continue;
        }

        if byte == b'"' {
            positions.push(checked_position(cursor));
            in_string = true;
        } else if is_json_punctuation(byte) {
            positions.push(checked_position(cursor));
        }
        cursor += 1;
    }
}

fn scalar_json_parse_index(input: &[u8]) -> (Vec<u32>, Vec<u32>, Vec<u32>) {
    let mut positions = Vec::with_capacity(input.len() / 8 + 8);
    let mut string_escapes = Vec::with_capacity(input.len() / 512 + 1);
    let mut string_controls = Vec::new();
    scan_json_tail_parse(
        input,
        0,
        &mut positions,
        &mut string_escapes,
        &mut string_controls,
        false,
        false,
    );
    (positions, string_escapes, string_controls)
}

#[inline]
fn is_json_punctuation(byte: u8) -> bool {
    matches!(byte, b'{' | b'}' | b'[' | b']' | b':' | b',')
}

#[inline]
fn checked_position(offset: usize) -> u32 {
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
fn prefix_xor_64(mut mask: u64, carry_in: bool) -> u64 {
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
fn escape_mask_64(bs_mask: u64, bs_carry_in: bool) -> (u64, bool) {
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

fn compact_mask(input: &[u8], base: usize, mask: u64, positions: &mut Vec<u32>) {
    let mut bits = mask;
    while bits != 0 {
        let offset = bits.trailing_zeros() as usize;
        let position = base + offset;
        debug_assert!(input.get(position).is_some());
        positions.push(checked_position(position));
        bits &= bits - 1;
    }
}

#[inline]
fn resolve_json_string_masks_64(
    quotes: u64,
    backslashes: u64,
    controls: u64,
    mut in_string: bool,
    escaped_carry: bool,
) -> (u64, u64, u64, u64, bool, bool) {
    let mut escaped = in_string && escaped_carry;
    let mut real_quotes = 0u64;
    let mut string_body = 0u64;
    let mut escape_starts = 0u64;
    let mut control_hits = 0u64;

    for index in 0..64 {
        let bit = 1u64 << index;
        if in_string {
            string_body |= bit;
            if escaped {
                escaped = false;
                continue;
            }
            if backslashes & bit != 0 {
                escape_starts |= bit;
                escaped = true;
                continue;
            }
            if quotes & bit != 0 {
                real_quotes |= bit;
                string_body &= !bit;
                in_string = false;
                continue;
            }
            if controls & bit != 0 {
                control_hits |= bit;
            }
        } else if quotes & bit != 0 {
            real_quotes |= bit;
            in_string = true;
        }
    }

    (real_quotes, string_body, escape_starts, control_hits, in_string, escaped)
}

#[cfg(target_arch = "aarch64")]
mod neon {
    use super::{
        compact_mask, escape_mask_64, prefix_xor_64, resolve_json_string_masks_64, scan_json_tail,
        scan_json_tail_parse,
    };
    use core::arch::aarch64::*;

    const STRIPE: usize = 64;

    pub fn scan_json(input: &[u8]) -> Vec<u32> {
        let mut positions = Vec::with_capacity(input.len() / 8 + 8);
        let mut cursor = 0usize;
        let mut in_string = false;
        let mut bs_carry = false;

        unsafe {
            while cursor + STRIPE <= input.len() {
                let (punctuation, quotes) = classify_stripe(input.as_ptr().add(cursor));
                let emit = if quotes == 0 && !in_string {
                    punctuation
                } else {
                    let backslashes = backslash_stripe(input.as_ptr().add(cursor));
                    let (escaped, new_bs_carry) = escape_mask_64(backslashes, bs_carry);
                    let real_quotes_fast = quotes & !escaped;
                    let prefix = prefix_xor_64(real_quotes_fast, in_string);
                    let string_body_fast = prefix ^ real_quotes_fast;
                    let escaped_quotes = quotes & escaped;
                    let carry_is_in_string =
                        !new_bs_carry || (string_body_fast & (1u64 << 63)) != 0;
                    let fast_path_is_strict =
                        (escaped_quotes & !string_body_fast) == 0 && carry_is_in_string;
                    let (real_quotes, string_body, next_in_string, next_bs_carry) =
                        if fast_path_is_strict {
                            (
                                real_quotes_fast,
                                string_body_fast,
                                ((prefix >> 63) & 1) == 1,
                                new_bs_carry,
                            )
                        } else {
                            let (real_quotes, string_body, _, _, next_in_string, next_escaped) =
                                resolve_json_string_masks_64(
                                    quotes,
                                    backslashes,
                                    0,
                                    in_string,
                                    bs_carry,
                                );
                            (real_quotes, string_body, next_in_string, next_escaped)
                        };
                    in_string = next_in_string;
                    bs_carry = next_bs_carry && in_string;
                    (punctuation & !string_body) | real_quotes
                };
                if emit != 0 {
                    compact_mask(input, cursor, emit, &mut positions);
                }
                cursor += STRIPE;
            }
        }

        scan_json_tail(input, cursor, &mut positions, in_string, bs_carry);
        positions
    }

    pub fn scan_json_parse(input: &[u8]) -> (Vec<u32>, Vec<u32>, Vec<u32>) {
        let mut positions = Vec::with_capacity(input.len() / 8 + 8);
        let mut string_escapes = Vec::with_capacity(input.len() / 512 + 1);
        let mut string_controls = Vec::new();
        let mut cursor = 0usize;
        let mut in_string = false;
        let mut bs_carry = false;

        unsafe {
            while cursor + STRIPE <= input.len() {
                let (punctuation, quotes, backslashes, controls) =
                    classify_parse_stripe(input.as_ptr().add(cursor));
                let (escaped, new_bs_carry) = escape_mask_64(backslashes, bs_carry);
                let real_quotes_fast = quotes & !escaped;
                let prefix = prefix_xor_64(real_quotes_fast, in_string);
                let string_body_fast = prefix ^ real_quotes_fast;
                let escaped_quotes = quotes & escaped;
                let carry_is_in_string = !new_bs_carry || (string_body_fast & (1u64 << 63)) != 0;
                let carry_continues = bs_carry && (backslashes & 1) != 0;
                let fast_path_is_strict = (escaped_quotes & !string_body_fast) == 0
                    && carry_is_in_string
                    && !carry_continues;
                let (
                    real_quotes,
                    string_body,
                    escape_starts,
                    control_bytes,
                    next_in_string,
                    next_bs_carry,
                ) = if fast_path_is_strict {
                    (
                        real_quotes_fast,
                        string_body_fast,
                        backslashes & string_body_fast & !escaped,
                        controls & string_body_fast & !escaped,
                        ((prefix >> 63) & 1) == 1,
                        new_bs_carry,
                    )
                } else {
                    resolve_json_string_masks_64(quotes, backslashes, controls, in_string, bs_carry)
                };
                in_string = next_in_string;
                bs_carry = next_bs_carry && in_string;
                let emit = (punctuation & !string_body) | real_quotes;
                if emit != 0 {
                    compact_mask(input, cursor, emit, &mut positions);
                }
                if escape_starts != 0 {
                    compact_mask(input, cursor, escape_starts, &mut string_escapes);
                }
                if control_bytes != 0 {
                    compact_mask(input, cursor, control_bytes, &mut string_controls);
                }
                cursor += STRIPE;
            }
        }

        scan_json_tail_parse(
            input,
            cursor,
            &mut positions,
            &mut string_escapes,
            &mut string_controls,
            in_string,
            bs_carry,
        );
        (positions, string_escapes, string_controls)
    }

    #[inline(always)]
    unsafe fn classify_stripe(ptr: *const u8) -> (u64, u64) {
        let mut punctuation = 0u64;
        let mut quotes = 0u64;
        for lane in 0..4 {
            let (p, q) = unsafe { classify_chunk(ptr.add(lane * 16)) };
            let shift = lane * 16;
            punctuation |= (p as u64) << shift;
            quotes |= (q as u64) << shift;
        }
        (punctuation, quotes)
    }

    #[inline(always)]
    unsafe fn backslash_stripe(ptr: *const u8) -> u64 {
        let mut backslashes = 0u64;
        for lane in 0..4 {
            let mask = unsafe { backslash_chunk(ptr.add(lane * 16)) };
            backslashes |= (mask as u64) << (lane * 16);
        }
        backslashes
    }

    #[inline(always)]
    unsafe fn classify_parse_stripe(ptr: *const u8) -> (u64, u64, u64, u64) {
        let mut punctuation = 0u64;
        let mut quotes = 0u64;
        let mut backslashes = 0u64;
        let mut controls = 0u64;
        for lane in 0..4 {
            let (p, q, b, c) = unsafe { classify_parse_chunk(ptr.add(lane * 16)) };
            let shift = lane * 16;
            punctuation |= (p as u64) << shift;
            quotes |= (q as u64) << shift;
            backslashes |= (b as u64) << shift;
            controls |= (c as u64) << shift;
        }
        (punctuation, quotes, backslashes, controls)
    }

    #[inline(always)]
    unsafe fn classify_chunk(ptr: *const u8) -> (u16, u16) {
        unsafe {
            let chunk = vld1q_u8(ptr);
            let mut punctuation = vceqq_u8(chunk, vdupq_n_u8(b'{'));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b'}')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b'[')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b']')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b':')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b',')));
            let quotes = vceqq_u8(chunk, vdupq_n_u8(b'"'));
            (movemask_u8x16(punctuation), movemask_u8x16(quotes))
        }
    }

    #[inline(always)]
    unsafe fn backslash_chunk(ptr: *const u8) -> u16 {
        unsafe {
            let chunk = vld1q_u8(ptr);
            movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(b'\\')))
        }
    }

    #[inline(always)]
    unsafe fn classify_parse_chunk(ptr: *const u8) -> (u16, u16, u16, u16) {
        unsafe {
            let chunk = vld1q_u8(ptr);
            let mut punctuation = vceqq_u8(chunk, vdupq_n_u8(b'{'));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b'}')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b'[')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b']')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b':')));
            punctuation = vorrq_u8(punctuation, vceqq_u8(chunk, vdupq_n_u8(b',')));
            let quotes = vceqq_u8(chunk, vdupq_n_u8(b'"'));
            let backslashes = vceqq_u8(chunk, vdupq_n_u8(b'\\'));
            let controls = vcltq_u8(chunk, vdupq_n_u8(0x20));
            (
                movemask_u8x16(punctuation),
                movemask_u8x16(quotes),
                movemask_u8x16(backslashes),
                movemask_u8x16(controls),
            )
        }
    }

    #[inline(always)]
    unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {
        unsafe {
            let pattern: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
            let bits = vandq_u8(value, vld1q_u8(pattern.as_ptr()));
            let lo = vaddv_u8(vget_low_u8(bits)) as u16;
            let hi = vaddv_u8(vget_high_u8(bits)) as u16;
            lo | (hi << 8)
        }
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
