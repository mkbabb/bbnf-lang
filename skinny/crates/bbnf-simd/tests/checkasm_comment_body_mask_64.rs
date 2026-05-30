//! Differential parity harness for the `comment_body_mask_64` primitive (L5).
//!
//! Mirrors the structure of `tests/checkasm_byte_class_from_eq_set_64.rs`:
//! deterministic xorshift input, alignment sweeps over a 128-byte backing
//! buffer, a stack-canary clobber guard, adversarial comment-digraph windows
//! (open/close digraphs straddling the 64-byte block boundary), and a corpus
//! parity slide over a real CSS fixture.
//!
//! The primitive under test is the auto-dispatched
//! `bbnf_simd::prim::comment_body_mask_64(src, open, close, carry)`; the scalar
//! reference `comment_body_mask_64_scalar` is the executable specification.
//! Every active backend must agree with it bit-for-bit AND carry-for-carry on
//! every (src, carry) pair.

#![allow(clippy::needless_range_loop)]

mod checkasm_common;

use bbnf_simd::prim::{comment_body_mask_64, CommentCarry};
use bbnf_simd::scalar::comment_body_mask_64::comment_body_mask_64_scalar;
use checkasm_common::{with_stack_canary_xor_fold, Xorshift64};

const OPEN: [u8; 2] = [b'/', b'*'];
const CLOSE: [u8; 2] = [b'*', b'/'];

fn check(label: &str, src: &[u8; 64], carry: CommentCarry) {
    let (ref_mask, ref_carry) = comment_body_mask_64_scalar(src, OPEN, CLOSE, carry);
    let (cand_mask, cand_carry) =
        with_stack_canary_xor_fold("comment_body_mask_64", || {
            comment_body_mask_64(src, OPEN, CLOSE, carry)
        });
    assert_eq!(
        cand_mask, ref_mask,
        "{label}: mask diverged carry={carry:?} xor={:#018x}",
        cand_mask ^ ref_mask
    );
    assert_eq!(
        cand_carry, ref_carry,
        "{label}: next-carry diverged carry={carry:?}"
    );
}

fn carries() -> [CommentCarry; 4] {
    [
        CommentCarry { in_comment: false, pending_half: false },
        CommentCarry { in_comment: true, pending_half: false },
        CommentCarry { in_comment: false, pending_half: true },
        CommentCarry { in_comment: true, pending_half: true },
    ]
}

/// Window filled with random bytes, occasionally sprinkled with comment
/// digraphs so the comment machinery is genuinely exercised.
fn comment_flavoured_window(rng: &mut Xorshift64, density: u32) -> [u8; 64] {
    let mut src = [0u8; 64];
    rng.fill(&mut src);
    for i in 0..64 {
        let roll = (rng.next_u64() % 100) as u32;
        if roll < density {
            // Plant a digraph; choose open or close.
            if roll & 1 == 0 {
                src[i] = OPEN[0];
                if i + 1 < 64 {
                    src[i + 1] = OPEN[1];
                }
            } else {
                src[i] = CLOSE[0];
                if i + 1 < 64 {
                    src[i + 1] = CLOSE[1];
                }
            }
        }
    }
    src
}

#[test]
fn comment_alignment_sweep() {
    let mut rng = Xorshift64::new(0xC0FFEE_F00D_BAAD_01);
    let mut backing = [0u8; 128];
    rng.fill(&mut backing);
    // Embed several comment regions in the backing buffer.
    for region in &[(3usize, 18usize), (40, 55), (70, 71), (100, 124)] {
        backing[region.0] = OPEN[0];
        backing[region.0 + 1] = OPEN[1];
        backing[region.1] = CLOSE[0];
        backing[region.1 + 1] = CLOSE[1];
    }
    for align in 0..64 {
        let mut window = [0u8; 64];
        window.copy_from_slice(&backing[align..align + 64]);
        for carry in carries() {
            check("alignment_sweep", &window, carry);
        }
    }
}

#[test]
fn comment_digraph_density_sweep() {
    for &density in &[0u32, 2, 8, 25, 60] {
        let mut rng = Xorshift64::new(0xD1A6_0000 ^ density as u64);
        for _ in 0..64 {
            let src = comment_flavoured_window(&mut rng, density);
            for carry in carries() {
                check("density_sweep", &src, carry);
            }
        }
    }
}

#[test]
fn comment_boundary_digraphs() {
    // Exercise digraphs split exactly across the 64-byte boundary: the first
    // half lands at byte 63, the second at byte 0 of the "next" block — modeled
    // by the pending_half carry.
    for &(first, second) in &[(OPEN, CLOSE), (CLOSE, OPEN)] {
        let mut src = [b'x'; 64];
        src[63] = first[0]; // pending first half at the boundary
        // The continuation: a block beginning with the matching second half.
        let mut next = [b'y'; 64];
        next[0] = first[1];

        // Carry from a clean state: scan the first block, then feed its carry.
        let (_m0, carry0) = comment_body_mask_64_scalar(&src, OPEN, CLOSE, CommentCarry::default());
        check("boundary_first", &src, CommentCarry::default());
        check("boundary_second", &next, carry0);
        let _ = second;
    }
}

#[test]
fn comment_all_constant_fills() {
    for &fill in &[0x00u8, b'/', b'*', 0xFF] {
        let src = [fill; 64];
        for carry in carries() {
            check("constant_fill", &src, carry);
        }
    }
}

#[test]
fn comment_corpus_parity() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../data/css/bootstrap.css");
    let bytes = match std::fs::read(path) {
        Ok(b) => b,
        Err(error) => {
            eprintln!("comment corpus parity: fixture unavailable ({error}); skipping");
            return;
        }
    };
    if bytes.len() < 64 {
        return;
    }
    // Slide block-by-block (NOT byte-by-byte) threading the carry, exactly as
    // the production consumer would.
    let mut carry = CommentCarry::default();
    let mut cursor = 0usize;
    let mut window = [0u8; 64];
    while cursor + 64 <= bytes.len() {
        window.copy_from_slice(&bytes[cursor..cursor + 64]);
        let (ref_mask, ref_carry) = comment_body_mask_64_scalar(&window, OPEN, CLOSE, carry);
        let (cand_mask, cand_carry) = comment_body_mask_64(&window, OPEN, CLOSE, carry);
        assert_eq!(
            cand_mask, ref_mask,
            "corpus parity diverged at offset {cursor}: xor={:#018x}",
            cand_mask ^ ref_mask
        );
        assert_eq!(cand_carry, ref_carry, "corpus carry diverged at offset {cursor}");
        carry = ref_carry;
        cursor += 64;
    }
}
