mod checkasm_common;

use bbnf_simd::escape_mask_64;
use checkasm_common::{guarded_call, Xorshift64};

fn scalar_escape_mask_64(bs_mask: u64, carry_in: bool) -> (u64, bool) {
    let mut pending_escape = carry_in;
    let mut escape = 0u64;

    for bit_index in 0..64 {
        let bit = 1u64 << bit_index;
        let is_backslash = (bs_mask & bit) != 0;
        if pending_escape {
            if !is_backslash {
                escape |= bit;
            }
            pending_escape = false;
        } else if is_backslash {
            pending_escape = true;
        }
    }

    (escape, pending_escape)
}

fn assert_mask(label: &str, mask: u64, carry_in: bool) {
    let expected = scalar_escape_mask_64(mask, carry_in);
    let observed = guarded_call(|| escape_mask_64(mask, carry_in));
    assert_eq!(
        observed, expected,
        "{label} mask={mask:#018x} carry={carry_in}"
    );
}

fn backslash_mask(bytes: &[u8]) -> u64 {
    bytes
        .iter()
        .copied()
        .enumerate()
        .fold(0u64, |mask, (index, byte)| {
            mask | (((byte == b'\\') as u64) << index)
        })
}

fn fill_jsonish(seed: u64, len: usize) -> Vec<u8> {
    const POOL: &[u8] = b"{}[],:\"\\\n\t \"abcdefghijklmnopqrstuvwxyz0123456789{}[],:\"\\";
    let mut rng = Xorshift64::new(seed);
    let mut bytes = vec![0u8; len];
    for slot in &mut bytes {
        *slot = POOL[(rng.next_u64() as usize) % POOL.len()];
    }
    bytes
}

#[test]
fn direct_masks_match_byte_walk_reference() {
    let cases = [
        ("empty", 0),
        ("bit0", 1),
        ("bits0_1", 0b11),
        ("bits0_1_2", 0b111),
        ("bit63", 1u64 << 63),
        ("bits62_63", 3u64 << 62),
        ("bits61_63", 7u64 << 61),
        ("all", u64::MAX),
        ("sparse", 0x8000_0000_0000_0001),
        ("alternating", 0xAAAA_AAAA_AAAA_AAAA),
        ("quartets", 0x0F0F_0000_F0F0_F00F),
    ];

    for (label, mask) in cases {
        assert_mask(label, mask, false);
        assert_mask(label, mask, true);
    }
}

#[test]
fn deterministic_random_masks_match_reference() {
    let mut rng = Xorshift64::new(0xCAFE_F00D_BAAD_F00D);
    for index in 0..4096 {
        let mask = rng.next_u64();
        assert_mask("random false", mask, false);
        assert_mask("random true", mask, true);
        if index % 257 == 0 {
            assert_mask(
                "random sparse",
                mask & 0x0101_0101_0101_0101,
                index % 2 == 0,
            );
        }
    }
}

#[test]
fn historical_json_pool_falsifier_chunks_match_reference() {
    let input = fill_jsonish(0xCAFE_F00D_BAAD_F00D, 128);
    let mut carry = false;
    for (chunk_index, chunk) in input.chunks_exact(64).enumerate() {
        let mask = backslash_mask(chunk);
        let expected = scalar_escape_mask_64(mask, carry);
        let observed = guarded_call(|| escape_mask_64(mask, carry));
        assert_eq!(observed, expected, "chunk {chunk_index}");
        carry = observed.1;
    }
}

#[test]
fn long_backslash_runs_split_across_stripes_match_reference() {
    for run_len in 1..=128 {
        let mut input = vec![b'a'; 192];
        input[..run_len].fill(b'\\');
        input[run_len] = b'"';

        let mut carry = false;
        for (chunk_index, chunk) in input.chunks_exact(64).enumerate() {
            let mask = backslash_mask(chunk);
            let expected = scalar_escape_mask_64(mask, carry);
            let observed = guarded_call(|| escape_mask_64(mask, carry));
            assert_eq!(observed, expected, "run_len={run_len} chunk={chunk_index}");
            carry = observed.1;
        }
    }
}
