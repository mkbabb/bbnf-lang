#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ClassifyResult {
    pub structural_mask: u64,
    pub quote_mask: u64,
    pub backslash_mask: u64,
    pub control_mask: u64,
}

pub trait SimdClassifier: Send + Sync {
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult;
    fn alphabet(&self) -> &'static [u8; 64];
}
