#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OffsetTapeStats {
    pub offset_count: usize,
    pub offset_bytes: usize,
    pub offset_capacity_bytes: usize,
}

#[inline]
pub fn contains_offset_in_range(offsets: &[u32], start: usize, end: usize) -> bool {
    match offsets.binary_search(&(start as u32)) {
        Ok(index) => offsets
            .get(index)
            .is_some_and(|offset| (*offset as usize) < end),
        Err(index) => offsets
            .get(index)
            .is_some_and(|offset| (*offset as usize) < end),
    }
}
