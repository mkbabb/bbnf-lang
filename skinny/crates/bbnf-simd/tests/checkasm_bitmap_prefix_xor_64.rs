mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};

#[test]
fn bitmap_prefix_xor_64_matches_scalar() {
    let cases = [
        0,
        u64::MAX,
        0x5555_5555_5555_5555,
        0xAAAA_AAAA_AAAA_AAAA,
        1,
        1 << 63,
    ];
    for carry in [false, true] {
        for mask in cases {
            assert_eq!(
                guarded_call(|| bbnf_simd::prim::bitmap_prefix_xor_64(mask, carry)),
                bbnf_simd::scalar::bitmap_prefix_xor_64::bitmap_prefix_xor_64_scalar(mask, carry)
            );
        }
    }
}

#[test]
fn bitmap_prefix_xor_64_random_sweep() {
    let mut rng = Xorshift64::new(0x5052_4546_4958_5852);
    for _ in 0..4096 {
        let mask = rng.next_u64();
        for carry in [false, true] {
            assert_eq!(
                guarded_call(|| bbnf_simd::prim::bitmap_prefix_xor_64(mask, carry)),
                bbnf_simd::scalar::bitmap_prefix_xor_64::bitmap_prefix_xor_64_scalar(mask, carry)
            );
        }
    }
}
