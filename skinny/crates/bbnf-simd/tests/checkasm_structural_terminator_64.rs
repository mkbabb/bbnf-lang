#![cfg(target_arch = "aarch64")]

mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};

const MEMBERS: &[u8] = b"{}[],:\"";

fn reference(src: &[u8; 64], terminator: u8) -> (u64, u64) {
    let mut structural = 0u64;
    let mut terminators = 0u64;
    for (index, byte) in src.iter().copied().enumerate() {
        if MEMBERS.contains(&byte) {
            structural |= 1u64 << index;
        }
        if byte == terminator {
            terminators |= 1u64 << index;
        }
    }
    (structural, terminators)
}

fn candidate(src: &[u8; 64], terminator: u8) -> (u64, u64) {
    let table = bbnf_simd::aarch64::classify_tbl4::build_lo6_table(MEMBERS);
    let table = unsafe { bbnf_simd::aarch64::classify_tbl4::load_lo6_table(&table) };
    let block = unsafe {
        bbnf_simd::aarch64::classify_tbl4::classify_structural_terminator_block_from_table(
            src.as_ptr(),
            table,
            terminator,
        )
    };
    (block.structural_mask, block.terminator_mask)
}

#[test]
fn structural_terminator_64_alignment_windows() {
    let mut backing = [0u8; 128];
    for (index, byte) in backing.iter_mut().enumerate() {
        *byte = index as u8;
    }
    for align in 0..64 {
        let src: &[u8; 64] = backing[align..align + 64].try_into().unwrap();
        assert_eq!(guarded_call(|| candidate(src, b'"')), reference(src, b'"'));
    }
}

#[test]
fn structural_terminator_64_random_jsonish() {
    let mut rng = Xorshift64::new(0x5354_5255_4354_544D);
    const POOL: &[u8] = b"{}[],:\"\\\n\t abcdefghijklmnopqrstuvwxyz0123456789";
    for _ in 0..4096 {
        let mut src = [0u8; 64];
        for byte in &mut src {
            *byte = POOL[(rng.next_u64() as usize) % POOL.len()];
        }
        assert_eq!(
            guarded_call(|| candidate(&src, b'"')),
            reference(&src, b'"')
        );
    }
}
