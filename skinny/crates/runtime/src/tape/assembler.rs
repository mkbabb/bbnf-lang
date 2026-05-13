use super::{checked_u32, OffsetFlags, PayloadArena, Tape};

/// SK-V3 Wave 2 capacity-plan probe selector.
///
/// `A` — sampled current plan (default heuristic);
/// `B` — exact structural count via a NEON `vceqq_u8` reduction pre-scan;
/// `C` — one-shot reserve from the SIMD scan output;
/// `D` — reserve-at-growth: small initial capacity, geometric grow on demand.
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
            Some("A") | Some("sampled") | None => CapacityPlan::Sampled,
            Some("B") | Some("exact") => CapacityPlan::Exact,
            Some("C") | Some("simd") | Some("oneshot") => CapacityPlan::OneShotSimd,
            Some("D") | Some("grow") | Some("growth") => CapacityPlan::GrowOnly,
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
    /// Compute the structural capacity according to the active probe plan.
    ///
    /// Plan A: sampled heuristic on a 4 KiB prefix (legacy path).
    /// Plan B: exact structural count via SIMD pre-scan on the full source.
    /// Plan C: identical to plan B in measurement; the probe binary stores the
    ///         positions vector for re-use as an event stream so the cost is
    ///         amortised, not doubled. The split is kept here for parity.
    /// Plan D: returns a small constant so growth dominates.
    pub fn json_structural_capacity_for(plan: CapacityPlan, source: &[u8]) -> usize {
        match plan {
            CapacityPlan::Sampled => Self::json_structural_capacity(source),
            CapacityPlan::Exact => exact_structural_count(source) + 8,
            CapacityPlan::OneShotSimd => oneshot_simd_count(source) + 8,
            CapacityPlan::GrowOnly => 256,
        }
    }

    pub fn json_structural_capacity(source: &[u8]) -> usize {
        let len = source.len();
        if len == 0 {
            return 8;
        }

        let sample_len = len.min(4096);
        let sample = &source[..sample_len];
        let mut quote_count = 0usize;
        let mut container_count = 0usize;
        let mut delimiter_count = 0usize;
        for byte in sample {
            match *byte {
                b'"' => quote_count += 1,
                b'{' | b'}' | b'[' | b']' => container_count += 1,
                b',' | b':' => delimiter_count += 1,
                _ => {}
            }
        }

        let structural_count = quote_count + container_count + delimiter_count;
        let string_count = quote_count / 2;
        let scalar_budget = if quote_count >= delimiter_count {
            delimiter_count / 4
        } else {
            delimiter_count
        };
        let emitted_sample = container_count + string_count + scalar_budget + 1;
        let sampled = (emitted_sample * len * 5) / (sample_len * 4) + 8;
        let numeric_floor = if quote_count == 0 && delimiter_count * 12 >= sample_len {
            len / 2 + 8
        } else if structural_count * 20 >= sample_len {
            len / 6 + 8
        } else {
            8
        };

        sampled.max(numeric_floor).max(8)
    }

    pub fn new(source: &'input [u8], structural_capacity: usize) -> Self {
        let flag_capacity = sparse_flag_capacity(source, structural_capacity);
        Self {
            source,
            offsets: Vec::with_capacity(structural_capacity),
            flag_cursors: Vec::with_capacity(flag_capacity),
            flag_values: Vec::with_capacity(flag_capacity),
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

/// Plan B: a tight scalar pass that counts the eight structural bytes
/// `{}[],:" \\` plus quotes. On aarch64 the loop autovectorises to NEON
/// `vceqq_u8` reductions; the count is upper-bounded by the source length so
/// it stays a single linear pass.
fn exact_structural_count(source: &[u8]) -> usize {
    let mut count = 0usize;
    for &b in source {
        // Match the legacy `json_structural_capacity` accounting envelope:
        // count container/delimiter/quote bytes which become tape offsets.
        match b {
            b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"' => count += 1,
            _ => {}
        }
    }
    count
}

/// Plan C: re-use the bbnf-simd `scan_json_structurals` output. The probe
/// reports the exact post-classifier count (escaped quotes excluded, string
/// bodies excluded) which is the tightest possible structural reserve.
fn oneshot_simd_count(source: &[u8]) -> usize {
    bbnf_simd::scan_json_structurals(source).positions().len()
}

fn sparse_flag_capacity(source: &[u8], structural_capacity: usize) -> usize {
    if source.is_empty() {
        return 0;
    }

    let sample_len = source.len().min(4096);
    let backslash_count = source[..sample_len]
        .iter()
        .filter(|byte| **byte == b'\\')
        .count();
    if backslash_count == 0 {
        return 0;
    }
    if source.len() > 128 * 1024 && backslash_count * 64 >= sample_len {
        return structural_capacity;
    }

    let sampled = (backslash_count * source.len()) / (sample_len * 8) + 8;
    sampled.min(structural_capacity)
}
