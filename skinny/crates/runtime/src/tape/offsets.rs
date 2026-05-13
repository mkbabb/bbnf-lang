#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OffsetTapeStats {
    pub offset_count: usize,
    pub offset_bytes: usize,
    pub offset_capacity_bytes: usize,
}
