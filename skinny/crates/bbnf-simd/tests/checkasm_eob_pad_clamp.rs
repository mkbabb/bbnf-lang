mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};

#[test]
fn eob_pad_clamp_length_sweep() {
    let mut rng = Xorshift64::new(0x454F_425F_5041_4401);
    let mut backing = [0u8; 128];
    rng.fill(&mut backing);
    for len in 0..=64 {
        let input = &backing[..len];
        assert_eq!(
            guarded_call(|| bbnf_simd::prim::eob_pad_clamp(input)),
            bbnf_simd::scalar::eob_pad_clamp::eob_pad_clamp_scalar(input)
        );
    }
}

#[test]
fn eob_pad_clamp_zeroes_dead_lanes() {
    let input = b"tail";
    let block = guarded_call(|| bbnf_simd::prim::eob_pad_clamp(input));
    assert_eq!(&block.bytes[..4], input);
    assert!(block.bytes[4..].iter().all(|byte| *byte == 0));
    assert_eq!(block.live_mask, 0b1111);
}
