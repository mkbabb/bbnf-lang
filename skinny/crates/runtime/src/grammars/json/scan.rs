// JSON-owned structural scan source; not part of the generated JSON roster.
use bbnf_simd::{ScanBackend, ScalarParityReport, StructuralAlphabet, StructuralIndex};

use crate::tape::CapacityPlan;

pub const STRUCTURAL_BYTES: &[u8] = b"{}[],:\"";
pub const STRUCTURAL_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_bytes(STRUCTURAL_BYTES);

#[cfg(target_arch = "aarch64")]
const STRUCTURAL_CLASS_TABLE_LO6: [u8; 64] = {
    let mut table = [0u8; 64];
    table[(b'"' & 0x3f) as usize] = b'"';
    table[(b',' & 0x3f) as usize] = b',';
    table[(b':' & 0x3f) as usize] = b':';
    table[(b'[' & 0x3f) as usize] = b'[';
    table[(b']' & 0x3f) as usize] = b']';
    table[(b'{' & 0x3f) as usize] = b'{';
    table[(b'}' & 0x3f) as usize] = b'}';
    table
};

pub fn scan_structurals(input: &[u8]) -> StructuralIndex {
    #[cfg(target_arch = "aarch64")]
    {
        return StructuralIndex::from_positions(neon::scan(input), ScanBackend::Neon);
    }

    #[allow(unreachable_code)]
    scan_structurals_scalar(input)
}

pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex {
    let mut positions = Vec::with_capacity(input.len() / 8 + 8);
    scan_tail(input, 0, &mut positions, false, false);
    StructuralIndex::from_positions(positions, ScanBackend::Scalar)
}

pub fn scalar_parity_report(input: &[u8]) -> ScalarParityReport {
    let index = scan_structurals_scalar(input);
    ScalarParityReport {
        input_len: input.len(),
        structural_count: index.positions().len(),
        hash: index.parity_hash(input),
    }
}

pub fn structural_capacity_for(plan: CapacityPlan, source: &[u8]) -> usize {
    match plan {
        CapacityPlan::Sampled => sampled_structural_capacity(source),
        CapacityPlan::Exact => exact_structural_count(source) + 8,
        CapacityPlan::OneShotSimd => scan_structurals(source).positions().len() + 8,
        CapacityPlan::GrowOnly => 256,
    }
}

fn sampled_structural_capacity(source: &[u8]) -> usize {
    let len = source.len();
    if len == 0 {
        return 8;
    }

    let sample_len = len.min(4096);
    let sample = &source[..sample_len];
    let mut quote_count = 0usize;
    let mut container_count = 0usize;
    let mut delimiter_count = 0usize;
    for byte in sample {
        match *byte {
            b'"' => quote_count += 1,
            b'{' | b'}' | b'[' | b']' => container_count += 1,
            b',' | b':' => delimiter_count += 1,
            _ => {}
        }
    }

    let structural_count = quote_count + container_count + delimiter_count;
    let string_count = quote_count / 2;
    let scalar_budget = if quote_count >= delimiter_count {
        delimiter_count / 4
    } else {
        delimiter_count
    };
    let emitted_sample = container_count + string_count + scalar_budget + 1;
    let sampled = (emitted_sample * len * 5) / (sample_len * 4) + 8;
    let numeric_floor = if quote_count == 0 && delimiter_count * 12 >= sample_len {
        len / 2 + 8
    } else if structural_count * 20 >= sample_len {
        len / 6 + 8
    } else {
        8
    };

    sampled.max(numeric_floor).max(8)
}

fn exact_structural_count(source: &[u8]) -> usize {
    let mut count = 0usize;
    for &byte in source {
        match byte {
            b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"' => count += 1,
            _ => {}
        }
    }
    count
}

fn scan_tail(
    input: &[u8],
    start: usize,
    positions: &mut Vec<u32>,
    mut in_string: bool,
    mut escaped: bool,
) {
    let mut cursor = start;
    while cursor < input.len() {
        let remaining = input.len() - cursor;
        let take = remaining.min(64);
        let block = bbnf_simd::prim::eob_pad_clamp(&input[cursor..cursor + take]);
        let mut lane = 0usize;
        while lane < take {
            let byte = block.bytes[lane];
            let absolute = cursor + lane;
            scan_tail_byte(byte, absolute, positions, &mut in_string, &mut escaped);
            lane += 1;
        }
        cursor += take;
    }
}

#[inline]
fn scan_tail_byte(
    byte: u8,
    cursor: usize,
    positions: &mut Vec<u32>,
    in_string: &mut bool,
    escaped: &mut bool,
) {
    if *in_string {
        if *escaped {
            *escaped = false;
        } else if byte == b'\\' {
            *escaped = true;
        } else if byte == b'"' {
            positions.push(bbnf_simd::checked_position(cursor));
            *in_string = false;
        }
        return;
    }

    if byte == b'"' {
        positions.push(bbnf_simd::checked_position(cursor));
        *in_string = true;
    } else if is_punctuation(byte) {
        positions.push(bbnf_simd::checked_position(cursor));
    }
}

#[inline]
fn is_punctuation(byte: u8) -> bool {
    matches!(byte, b'{' | b'}' | b'[' | b']' | b':' | b',')
}

#[inline]
fn resolve_string_masks_64(
    quotes: u64,
    backslashes: u64,
    mut in_string: bool,
    escaped_carry: bool,
) -> (u64, u64, bool, bool) {
    let mut escaped = in_string && escaped_carry;
    let mut real_quotes = 0u64;
    let mut string_body = 0u64;

    for index in 0..64 {
        let bit = 1u64 << index;
        if in_string {
            string_body |= bit;
            if escaped {
                escaped = false;
                continue;
            }
            if backslashes & bit != 0 {
                escaped = true;
                continue;
            }
            if quotes & bit != 0 {
                real_quotes |= bit;
                string_body &= !bit;
                in_string = false;
            }
        } else if quotes & bit != 0 {
            real_quotes |= bit;
            in_string = true;
        }
    }

    (real_quotes, string_body, in_string, escaped)
}

#[cfg(target_arch = "aarch64")]
mod neon {
    use super::{resolve_string_masks_64, scan_tail, STRUCTURAL_CLASS_TABLE_LO6};
    use bbnf_simd::{compact_mask, escape_mask_64, prefix_xor_64};

    const STRIPE: usize = 64;

    pub fn scan(input: &[u8]) -> Vec<u32> {
        let mut positions = Vec::with_capacity(input.len() / 8 + 8);
        let mut cursor = 0usize;
        let mut in_string = false;
        let mut bs_carry = false;

        unsafe {
            let table = bbnf_simd::aarch64::classify_tbl4::load_lo6_table(
                &STRUCTURAL_CLASS_TABLE_LO6,
            );
            while cursor + STRIPE <= input.len() {
                let structural =
                    bbnf_simd::aarch64::classify_tbl4::classify_structural_terminator_block_from_table(
                    input.as_ptr().add(cursor),
                    table,
                    b'"',
                );
                let quotes = structural.terminator_mask;
                let emit = if quotes == 0 && !in_string {
                    structural.structural_mask
                } else {
                    let result = bbnf_simd::aarch64::classify_tbl4::classify_block_from_table(
                        input.as_ptr().add(cursor),
                        table,
                        b'"',
                        b'\\',
                        0x20,
                    );
                    let punctuation = result.structural_mask & !result.quote_mask;
                    let (escaped, new_bs_carry) =
                        escape_mask_64(result.backslash_mask, bs_carry);
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
                            resolve_string_masks_64(
                                quotes,
                                result.backslash_mask,
                                in_string,
                                bs_carry,
                            )
                        };
                    in_string = next_in_string;
                    bs_carry = next_bs_carry && in_string;
                    (punctuation & !string_body) | real_quotes
                };
                if emit != 0 {
                    compact_mask(cursor, emit, &mut positions);
                }
                cursor += STRIPE;
            }
        }

        scan_tail(input, cursor, &mut positions, in_string, bs_carry);
        positions
    }
}

#[cfg(test)]
mod tests {
    use super::{scan_structurals, scan_structurals_scalar, ScanBackend};

    fn fill_jsonish(seed: u64, len: usize) -> Vec<u8> {
        const POOL: &[u8] = b"{}[],:\"\\\n\t \"abcdefghijklmnopqrstuvwxyz0123456789{}[],:\"\\";
        let mut state = seed | 1;
        let mut bytes = vec![0u8; len];
        for byte in &mut bytes {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            *byte = POOL[(state as usize) % POOL.len()];
        }
        bytes
    }

    fn assert_scan_parity(input: &[u8]) {
        let simd = scan_structurals(input);
        let scalar = scan_structurals_scalar(input);
        #[cfg(target_arch = "aarch64")]
        assert_eq!(simd.backend(), ScanBackend::Neon);
        assert_eq!(simd.positions(), scalar.positions(), "input={input:?}");
        assert_eq!(simd.parity_hash(input), scalar.parity_hash(input));
    }

    #[test]
    fn adversarial_escape_windows_match_scalar() {
        assert_scan_parity(&fill_jsonish(0xCAFE_F00D_BAAD_F00D, 128));

        for tail in 0..64 {
            let mut input = b"[\"".to_vec();
            input.resize(63, b'a');
            input.push(b'\\');
            input.extend(std::iter::repeat_n(b'a', tail));
            input.extend_from_slice(b"\"]");
            assert_scan_parity(&input);
        }

        for align in 0..16 {
            let mut input = vec![b' '; align];
            input.extend_from_slice(b"{\"a\":\"ascii\\\\\\\"quote\",\"b\":[1,2,3]}");
            assert_scan_parity(&input);
        }
    }

    #[test]
    fn slash_runs_before_stripe_quotes_match_scalar() {
        for boundary in [63usize, 64, 65] {
            for run_len in 1..=8 {
                let mut input = b"[\"".to_vec();
                let target = boundary.saturating_sub(run_len);
                input.resize(target, b'a');
                input.extend(std::iter::repeat_n(b'\\', run_len));
                input.extend_from_slice(b"\"]");
                assert_scan_parity(&input);
            }
        }
    }
}
