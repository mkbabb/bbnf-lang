mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};

fn reference(base: u32, mut mask: u64) -> Vec<u32> {
    let mut out = Vec::new();
    while mask != 0 {
        let bit = mask.trailing_zeros();
        out.push(base + bit);
        mask &= mask - 1;
    }
    out
}

fn candidate(base: u32, mask: u64) -> Vec<u32> {
    let mut out = vec![0xFFFF_FFFF; 3];
    let prefix_len = out.len();
    out.reserve(64);
    let written = unsafe {
        bbnf_simd::prim::bulk_emit_positions_64(base, mask, out.as_mut_ptr().add(prefix_len))
    };
    unsafe {
        out.set_len(prefix_len + written);
    }
    assert_eq!(&out[..prefix_len], &[0xFFFF_FFFF; 3]);
    out[prefix_len..].to_vec()
}

#[test]
fn bulk_emit_positions_64_boundary_cases() {
    for mask in [
        0,
        1,
        1 << 63,
        u64::MAX,
        0x8000_0000_0000_0001,
        0x1010_0000_0000_0101,
    ] {
        for base in [0, 1, 64, 4096, u32::MAX - 64] {
            assert_eq!(
                guarded_call(|| candidate(base, mask)),
                reference(base, mask),
                "base={base} mask={mask:#018x}"
            );
        }
    }
}

#[test]
fn bulk_emit_positions_64_random_sweep() {
    let mut rng = Xorshift64::new(0x4255_4C4B_454D_4954);
    for _ in 0..4096 {
        let base = (rng.next_u64() as u32) & !63;
        let mask = rng.next_u64();
        assert_eq!(
            guarded_call(|| candidate(base, mask)),
            reference(base, mask)
        );
    }
}
