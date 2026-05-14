use super::{checked_u32, OffsetFlags, PayloadArena, Tape};

/// SK-V3 Wave 2 capacity-plan probe selector.
///
/// `A` — sampled legacy plan;
/// `B` — exact structural count via a NEON `vceqq_u8` reduction pre-scan;
/// `C` — one-shot reserve from the SIMD scan output;
/// `D` — reserve-at-growth: small initial capacity, geometric grow on demand
///       (production default).
///
/// Selection is driven by the `BBNF_CAPACITY_PLAN` env var. Reads land in the
/// cold path because the plan only matters at builder construction time.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CapacityPlan {
    Sampled,
    Exact,
    OneShotSimd,
    GrowOnly,
}

impl CapacityPlan {
    pub fn from_env() -> Self {
        match std::env::var("BBNF_CAPACITY_PLAN").ok().as_deref() {
            Some("A") | Some("sampled") => CapacityPlan::Sampled,
            Some("B") | Some("exact") => CapacityPlan::Exact,
            Some("C") | Some("simd") | Some("oneshot") => CapacityPlan::OneShotSimd,
            Some("D") | Some("grow") | Some("growth") | None => CapacityPlan::GrowOnly,
            Some(other) => panic!("unknown BBNF_CAPACITY_PLAN: {other}"),
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            CapacityPlan::Sampled => "A:sampled",
            CapacityPlan::Exact => "B:exact",
            CapacityPlan::OneShotSimd => "C:oneshot-simd",
            CapacityPlan::GrowOnly => "D:grow-only",
        }
    }
}

pub struct TapeBuilder<'input> {
    source: &'input [u8],
    offsets: Vec<u32>,
    flag_cursors: Vec<u32>,
    flag_values: Vec<u8>,
    payloads: PayloadArena,
}

impl<'input> TapeBuilder<'input> {
    pub fn new(source: &'input [u8], structural_capacity: usize) -> Self {
        Self {
            source,
            offsets: Vec::with_capacity(structural_capacity),
            flag_cursors: Vec::new(),
            flag_values: Vec::new(),
            payloads: PayloadArena::empty(),
        }
    }

    #[inline(always)]
    pub fn push_offset(&mut self, offset: usize, flags: OffsetFlags) -> u32 {
        let cursor = self.push_plain_offset(offset);
        if flags.bits() != 0 {
            self.patch_flags(cursor, flags);
        }
        cursor
    }

    #[inline(always)]
    pub fn push_plain_offset(&mut self, offset: usize) -> u32 {
        let len = self.offsets.len();
        if len == self.offsets.capacity() {
            self.reserve_offsets_cold(len);
        }
        unsafe {
            self.offsets
                .as_mut_ptr()
                .add(len)
                .write(checked_u32(offset));
            self.offsets.set_len(len + 1);
        }
        let cursor = checked_u32(len);
        cursor
    }

    #[cold]
    #[inline(never)]
    fn reserve_offsets_cold(&mut self, len: usize) {
        self.offsets.reserve((len / 2).max(8));
    }

    #[inline(always)]
    pub fn patch_flags(&mut self, cursor: u32, flags: OffsetFlags) {
        if flags.bits() == 0 {
            return;
        }
        if self.flag_cursors.last().copied() == Some(cursor) {
            *self
                .flag_values
                .last_mut()
                .expect("flag cursor and value vectors stay paired") = flags.bits();
            return;
        }
        debug_assert!(
            self.flag_cursors
                .last()
                .is_none_or(|last_cursor| *last_cursor < cursor),
            "flag patches must be emitted in cursor order"
        );
        self.flag_cursors.push(cursor);
        self.flag_values.push(flags.bits());
    }

    pub fn finish(self) -> Tape<'input> {
        Tape::from_offsets(
            self.source,
            self.offsets,
            self.flag_cursors,
            self.flag_values,
            self.payloads,
        )
    }
}
