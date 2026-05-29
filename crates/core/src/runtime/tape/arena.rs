//! `PayloadArena` — an 8-aligned bump arena for the wide payloads a
//! [`super::record::TapeRec`] cannot inline into its 4-byte `child_off`.
//!
//! Mirrors the skinny `tape/mod.rs` `PayloadArena`: a single `Vec<u8>`,
//! `write_*` returning an 8-aligned offset, `len`/`truncate` for the
//! O(1) tape checkpoint. Holds f64/i64/u64 magnitudes (CssLength /
//! CssColor components) and decoded strings that are not zero-copy
//! source borrows. Most CSS scalars are inline-packed in `child_off` or
//! are zero-copy spans, so writes here are near-zero in practice.

/// 8-aligned bump arena for wide tape payloads.
#[derive(Debug, Default)]
pub struct PayloadArena {
    bytes: Vec<u8>,
}

impl PayloadArena {
    #[inline]
    pub fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            bytes: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn len(&self) -> u32 {
        self.bytes.len() as u32
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Rollback to a previously-captured length (O(1) tape checkpoint).
    #[inline]
    pub fn truncate(&mut self, len: u32) {
        self.bytes.truncate(len as usize);
    }

    #[inline]
    fn align_to_8(&mut self) -> u32 {
        let rem = self.bytes.len() % 8;
        if rem != 0 {
            self.bytes.resize(self.bytes.len() + (8 - rem), 0);
        }
        self.bytes.len() as u32
    }

    /// Append raw bytes, returning their 8-aligned start offset.
    #[inline]
    pub fn write_bytes(&mut self, src: &[u8]) -> u32 {
        let off = self.align_to_8();
        self.bytes.extend_from_slice(src);
        off
    }

    #[inline]
    pub fn write_f64(&mut self, value: f64) -> u32 {
        self.write_bytes(&value.to_ne_bytes())
    }

    #[inline]
    pub fn write_i64(&mut self, value: i64) -> u32 {
        self.write_bytes(&value.to_ne_bytes())
    }

    #[inline]
    pub fn write_u64(&mut self, value: u64) -> u32 {
        self.write_bytes(&value.to_ne_bytes())
    }

    /// Append a UTF-8 string, returning `(offset, len)` for later
    /// reconstruction via [`Self::read_str`].
    #[inline]
    pub fn write_str(&mut self, value: &str) -> (u32, u32) {
        let off = self.write_bytes(value.as_bytes());
        (off, value.len() as u32)
    }

    #[inline]
    pub fn read_f64(&self, off: u32) -> f64 {
        let o = off as usize;
        f64::from_ne_bytes(self.bytes[o..o + 8].try_into().expect("8 arena bytes"))
    }

    #[inline]
    pub fn read_i64(&self, off: u32) -> i64 {
        let o = off as usize;
        i64::from_ne_bytes(self.bytes[o..o + 8].try_into().expect("8 arena bytes"))
    }

    #[inline]
    pub fn read_u64(&self, off: u32) -> u64 {
        let o = off as usize;
        u64::from_ne_bytes(self.bytes[o..o + 8].try_into().expect("8 arena bytes"))
    }

    #[inline]
    pub fn read_str(&self, off: u32, len: u32) -> &str {
        let o = off as usize;
        core::str::from_utf8(&self.bytes[o..o + len as usize]).unwrap_or("")
    }
}
