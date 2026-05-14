mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};

#[test]
fn bitmap_next_set_bit_boundary_cases() {
    for mask in [0, u64::MAX, 1, 1 << 63, 0x1010_0000_0000_0101] {
        for cursor in 0..=64u8 {
            assert_eq!(
                guarded_call(|| bbnf_simd::prim::bitmap_next_set_bit(mask, cursor)),
                bbnf_simd::scalar::bitmap_next_set_bit::bitmap_next_set_bit_scalar(mask, cursor)
            );
        }
    }
}

#[test]
fn bitmap_next_set_bit_random_sweep() {
    let mut rng = Xorshift64::new(0x4E45_5854_5F53_4554);
    for _ in 0..4096 {
        let mask = rng.next_u64();
        for cursor in 0..=64u8 {
            assert_eq!(
                guarded_call(|| bbnf_simd::prim::bitmap_next_set_bit(mask, cursor)),
                bbnf_simd::scalar::bitmap_next_set_bit::bitmap_next_set_bit_scalar(mask, cursor)
            );
        }
    }
}
