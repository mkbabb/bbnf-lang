#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EobBlock {
    pub bytes: [u8; 64],
    pub live_mask: u64,
}

#[inline]
pub fn eob_pad_clamp_scalar(input: &[u8]) -> EobBlock {
    debug_assert!(input.len() <= 64);
    let len = input.len().min(64);
    let mut bytes = [0u8; 64];
    bytes[..len].copy_from_slice(&input[..len]);
    let live_mask = if len == 64 {
        u64::MAX
    } else {
        (1u64 << len) - 1
    };
    EobBlock { bytes, live_mask }
}
