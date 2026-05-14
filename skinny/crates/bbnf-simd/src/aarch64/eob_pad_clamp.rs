pub use crate::scalar::eob_pad_clamp::EobBlock;

#[inline]
pub fn eob_pad_clamp_neon(input: &[u8]) -> EobBlock {
    crate::scalar::eob_pad_clamp::eob_pad_clamp_scalar(input)
}
