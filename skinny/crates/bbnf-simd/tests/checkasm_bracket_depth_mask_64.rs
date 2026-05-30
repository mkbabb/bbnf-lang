//! Differential parity harness for the `bracket_depth_mask_64` primitive (L6).
//!
//! Mirrors `tests/checkasm_byte_class_from_eq_set_64.rs`: deterministic
//! xorshift input, alignment sweeps, a stack-canary clobber guard, adversarial
//! deeply-nested-bracket windows, and a real CSS corpus slide threading the
//! i32 depth carry.
//!
//! The primitive under test is the auto-dispatched
//! `bbnf_simd::prim::bracket_depth_mask_64(...)`; the scalar running-balance
//! reference `bracket_depth_mask_64_scalar` is the executable specification AND
//! the default body (REDRESS-89: NOT CTZ). Every active backend must agree
//! with it bit-for-bit AND depth-for-depth.

#![allow(clippy::needless_range_loop)]

mod checkasm_common;

use bbnf_simd::prim::bracket_depth_mask_64;
use bbnf_simd::scalar::bracket_depth_mask_64::bracket_depth_mask_64_scalar;
use checkasm_common::{with_stack_canary_xor_fold, Xorshift64};

const OPENS: [u8; 4] = [b'(', b'[', b'{', 0];
const CLOSES: [u8; 4] = [b')', b']', b'}', 0];
const OPEN_LEN: usize = 3;
const CLOSE_LEN: usize = 3;

fn check(label: &str, src: &[u8; 64], depth: i32) {
    let (ref_mask, ref_depth) =
        bracket_depth_mask_64_scalar(src, &OPENS, OPEN_LEN, &CLOSES, CLOSE_LEN, depth);
    let (cand_mask, cand_depth) = with_stack_canary_xor_fold("bracket_depth_mask_64", || {
        bracket_depth_mask_64(src, &OPENS, OPEN_LEN, &CLOSES, CLOSE_LEN, depth)
    });
    assert_eq!(
        cand_mask, ref_mask,
        "{label}: mask diverged depth={depth} xor={:#018x}",
        cand_mask ^ ref_mask
    );
    assert_eq!(cand_depth, ref_depth, "{label}: depth-out diverged depth_in={depth}");
}

/// Window with random bytes plus a controllable density of bracket bytes.
fn bracket_flavoured_window(rng: &mut Xorshift64, density: u32) -> [u8; 64] {
    let mut src = [0u8; 64];
    rng.fill(&mut src);
    for i in 0..64 {
        if (rng.next_u64() % 100) as u32 >= density {
            continue;
        }
        let pick = (rng.next_u64() % 6) as usize;
        src[i] = match pick {
            0 => b'(',
            1 => b'[',
            2 => b'{',
            3 => b')',
            4 => b']',
            _ => b'}',
        };
    }
    src
}

#[test]
fn bracket_alignment_sweep() {
    let mut rng = Xorshift64::new(0xBBAA_CCDD_1234_5678);
    let mut backing = [0u8; 128];
    rng.fill(&mut backing);
    // Plant a nested bracket structure.
    let seq = b"a(b[c{d}e]f)g(h)i";
    backing[10..10 + seq.len()].copy_from_slice(seq);
    backing[70..70 + seq.len()].copy_from_slice(seq);
    for align in 0..64 {
        let mut window = [0u8; 64];
        window.copy_from_slice(&backing[align..align + 64]);
        for depth in [0i32, 1, 2, 5] {
            check("alignment_sweep", &window, depth);
        }
    }
}

#[test]
fn bracket_density_sweep() {
    for &density in &[0u32, 3, 10, 30, 70] {
        let mut rng = Xorshift64::new(0xDADA_0000 ^ density as u64);
        for _ in 0..64 {
            let src = bracket_flavoured_window(&mut rng, density);
            for depth in [0i32, 1, 4] {
                check("density_sweep", &src, depth);
            }
        }
    }
}

#[test]
fn bracket_deep_nesting() {
    // 32 opens then 32 closes: depth ramps to 32 within the block.
    let mut src = [b'x'; 64];
    for i in 0..32 {
        src[i] = b'(';
    }
    for i in 32..64 {
        src[i] = b')';
    }
    for depth in [0i32, 1, 3] {
        check("deep_nesting", &src, depth);
    }

    // All opens: depth carries out at +64.
    let opens = [b'['; 64];
    check("all_opens", &opens, 0);
    // All closes from a high carry: depth winds down.
    let closes = [b']'; 64];
    check("all_closes", &closes, 64);
    // Unbalanced closes at depth 0 must clamp (no negative depth).
    check("unbalanced_closes", &closes, 0);
}

#[test]
fn bracket_constant_fills() {
    for &fill in &[0x00u8, b'(', b')', b'{', b'}', 0xFF] {
        let src = [fill; 64];
        for depth in [0i32, 1, 2] {
            check("constant_fill", &src, depth);
        }
    }
}

#[test]
fn bracket_corpus_parity() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../data/css/bootstrap.css");
    let bytes = match std::fs::read(path) {
        Ok(b) => b,
        Err(error) => {
            eprintln!("bracket corpus parity: fixture unavailable ({error}); skipping");
            return;
        }
    };
    if bytes.len() < 64 {
        return;
    }
    let mut depth = 0i32;
    let mut cursor = 0usize;
    let mut window = [0u8; 64];
    while cursor + 64 <= bytes.len() {
        window.copy_from_slice(&bytes[cursor..cursor + 64]);
        let (ref_mask, ref_depth) =
            bracket_depth_mask_64_scalar(&window, &OPENS, OPEN_LEN, &CLOSES, CLOSE_LEN, depth);
        let (cand_mask, cand_depth) =
            bracket_depth_mask_64(&window, &OPENS, OPEN_LEN, &CLOSES, CLOSE_LEN, depth);
        assert_eq!(
            cand_mask, ref_mask,
            "corpus parity diverged at offset {cursor}: xor={:#018x}",
            cand_mask ^ ref_mask
        );
        assert_eq!(cand_depth, ref_depth, "corpus depth diverged at offset {cursor}");
        depth = ref_depth;
        cursor += 64;
    }
}
