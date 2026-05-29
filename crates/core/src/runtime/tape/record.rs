//! Flat-tape record — the 16-byte post-order node shared by every
//! grammar's tape-backed [`super::TapeStructBuilder`].
//!
//! Recovered from the A-series `crates/bbnf-tape/src/tape.rs` `TapeRec`
//! (AoS, `#[repr(C)]`, align 4 — one quarter of a 64-byte cache line)
//! and refined toward the skinny SoA tape's payload encoding. The shape
//! is deliberately grammar-agnostic: it stores *which* push produced it
//! (a [`TapeKind`] + a leaf-type tag) plus the source byte span and a
//! single `child_off` word that is overloaded for compounds (first-child
//! offset, post-order) and for payload leaves (inline scalar or
//! [`super::arena::PayloadArena`] offset). No `&str`, no per-grammar
//! enum, no `OpenFrame`.

/// Byte offset into the flat record vector / payload arena. `u32::MAX`
/// is the sentinel `NONE` (pure-span leaf, or empty compound).
pub type TapeOffset = u32;

/// Sentinel `child_off` meaning "no inline payload / no children".
pub const TAPE_NONE: TapeOffset = u32::MAX;

/// Low-nibble node discriminant stored in [`TapeRec::kind_meta`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TapeKind {
    /// An open compound (object / pair / Alt branch). `child_off` holds
    /// the post-order offset of the compound's first child record; the
    /// children occupy `[child_off .. self)` and are walked backward by
    /// [`super::cursor::TapeCursor`].
    Open = 0,
    /// A typed scalar leaf. `extra`'s low bits hold the [`LeafType`] tag;
    /// `child_off` holds the inline-packed scalar (`<= 4` bytes) or an
    /// 8-aligned [`super::arena::PayloadArena`] offset for wide payloads.
    Leaf = 1,
}

impl TapeKind {
    #[inline]
    pub(crate) fn from_bits(bits: u8) -> Self {
        match bits & 0x0F {
            0 => TapeKind::Open,
            _ => TapeKind::Leaf,
        }
    }
}

/// Which `push_leaf_with_*` produced a [`TapeKind::Leaf`] record. Stored
/// in [`TapeRec::extra`] low bits; the high bit of `extra` is reserved as
/// [`STRING_BORROW_BIT`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum LeafType {
    /// `push_leaf_with_f64` — wide payload, lives in the arena.
    F64 = 0,
    /// `push_leaf_with_i64` — wide payload, lives in the arena.
    I64 = 1,
    /// `push_leaf_with_u64` — wide payload, lives in the arena.
    U64 = 2,
    /// `push_leaf_with_bool` — inline in `child_off` (0 / 1).
    Bool = 3,
    /// `push_leaf_with_str` — span leaf; the borrow lives in `span_lo/hi`
    /// when [`STRING_BORROW_BIT`] is set, else arena bytes at `child_off`.
    Str = 4,
    /// `push_leaf_with_unit` — no payload.
    Unit = 5,
}

impl LeafType {
    #[inline]
    pub(crate) fn from_bits(bits: u16) -> Self {
        match bits & 0x7 {
            0 => LeafType::F64,
            1 => LeafType::I64,
            2 => LeafType::U64,
            3 => LeafType::Bool,
            4 => LeafType::Str,
            _ => LeafType::Unit,
        }
    }
}

/// `extra` bit marking a [`LeafType::Str`] leaf as a zero-copy source
/// borrow (`span_lo/hi` are valid) rather than arena-decoded bytes.
pub const STRING_BORROW_BIT: u16 = 0x8000;

/// `flags` bit: the compound has at least one child.
pub const FLAG_HAS_CHILDREN: u8 = 1 << 6;
/// `flags` bit: high bit of the 5-bit `meta_idx` (bits 0..3 live in
/// `kind_meta`'s high nibble, bit 4 lives here).
pub const FLAG_META_IDX_HI: u8 = 1 << 7;
/// Mask for the low-6 branch/variant index packed into `flags`.
pub const FLAG_BRANCH_MASK: u8 = 0x3F;

/// One flat-tape node — 16 bytes, `#[repr(C)]`, align 4.
///
/// `kind_meta` packs the [`TapeKind`] in the low nibble and bits `0..3`
/// of the 5-bit `meta_idx` in the high nibble; `flags` carries the
/// 6-bit branch/variant index plus the `has_children` and `meta_idx`
/// high bits. `extra` packs the leaf-type tag and string-borrow bit.
/// `span_lo/span_hi` are byte offsets into the source. `child_off` is the
/// overloaded first-child / inline-payload / arena-offset word.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C, align(4))]
pub struct TapeRec {
    /// low nibble: [`TapeKind`]; high nibble: `meta_idx` bits `0..3`.
    pub kind_meta: u8,
    /// low 6 bits: branch/variant index; bit6 `has_children`; bit7
    /// `meta_idx` bit 4.
    pub flags: u8,
    /// leaf-type tag ([`LeafType`]) + [`STRING_BORROW_BIT`].
    pub extra: u16,
    /// source byte span start.
    pub span_lo: u32,
    /// source byte span end.
    pub span_hi: u32,
    /// compound: post-order first-child offset; leaf: inline scalar or
    /// arena offset; [`TAPE_NONE`] for pure-span leaf / empty compound.
    pub child_off: TapeOffset,
}

const _: () = assert!(core::mem::size_of::<TapeRec>() == 16);
const _: () = assert!(core::mem::align_of::<TapeRec>() == 4);

impl TapeRec {
    /// An open compound with the given source span and `meta_idx`
    /// (5-bit variant/discriminant index), no children yet.
    #[inline]
    pub fn open(span_lo: u32, span_hi: u32, meta_idx: u8) -> Self {
        let mut rec = TapeRec {
            kind_meta: TapeKind::Open as u8,
            flags: 0,
            extra: 0,
            span_lo,
            span_hi,
            child_off: TAPE_NONE,
        };
        rec.set_meta_idx(meta_idx);
        rec
    }

    /// A typed leaf with the given leaf type, span, and payload word.
    #[inline]
    pub fn leaf(leaf_ty: LeafType, span_lo: u32, span_hi: u32, child_off: TapeOffset) -> Self {
        TapeRec {
            kind_meta: TapeKind::Leaf as u8,
            flags: 0,
            extra: leaf_ty as u16,
            span_lo,
            span_hi,
            child_off,
        }
    }

    #[inline]
    pub fn kind(&self) -> TapeKind {
        TapeKind::from_bits(self.kind_meta)
    }

    #[inline]
    pub fn leaf_type(&self) -> LeafType {
        LeafType::from_bits(self.extra)
    }

    #[inline]
    pub fn is_string_borrow(&self) -> bool {
        self.extra & STRING_BORROW_BIT != 0
    }

    /// The 5-bit variant/discriminant index (Alt branch on the compound's
    /// own rule, or a leaf's numeric-unit discriminant).
    #[inline]
    pub fn meta_idx(&self) -> u8 {
        let lo = self.kind_meta >> 4; // bits 0..3
        let hi = (self.flags & FLAG_META_IDX_HI != 0) as u8; // bit 4
        lo | (hi << 4)
    }

    #[inline]
    pub fn set_meta_idx(&mut self, idx: u8) {
        self.kind_meta = (self.kind_meta & 0x0F) | ((idx & 0x0F) << 4);
        if idx & 0x10 != 0 {
            self.flags |= FLAG_META_IDX_HI;
        } else {
            self.flags &= !FLAG_META_IDX_HI;
        }
    }

    /// The 6-bit branch/variant index stamped by `push_branch_tag`.
    #[inline]
    pub fn branch(&self) -> u8 {
        self.flags & FLAG_BRANCH_MASK
    }

    #[inline]
    pub fn set_branch(&mut self, branch: u8) {
        self.flags = (self.flags & !FLAG_BRANCH_MASK) | (branch & FLAG_BRANCH_MASK);
    }

    #[inline]
    pub fn has_children(&self) -> bool {
        self.flags & FLAG_HAS_CHILDREN != 0
    }

    #[inline]
    pub fn set_has_children(&mut self, yes: bool) {
        if yes {
            self.flags |= FLAG_HAS_CHILDREN;
        } else {
            self.flags &= !FLAG_HAS_CHILDREN;
        }
    }

    #[inline]
    pub fn span(&self) -> (u32, u32) {
        (self.span_lo, self.span_hi)
    }
}
