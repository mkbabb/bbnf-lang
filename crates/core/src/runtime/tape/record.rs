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
/// in [`TapeRec::extra`] low bits.
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
    /// `push_leaf_with_str` — owned UTF-8 bytes in the payload arena.
    Str = 4,
    /// `push_leaf_with_unit` — no payload.
    Unit = 5,
    /// `push_leaf_with_u8` — exact inline semantic byte.
    U8 = 6,
}

impl LeafType {
    #[inline]
    pub(crate) fn from_bits(bits: u16) -> Option<Self> {
        match bits & 0x7 {
            0 => Some(LeafType::F64),
            1 => Some(LeafType::I64),
            2 => Some(LeafType::U64),
            3 => Some(LeafType::Bool),
            4 => Some(LeafType::Str),
            5 => Some(LeafType::Unit),
            6 => Some(LeafType::U8),
            _ => None,
        }
    }
}

/// `flags` bit: the open compound has a structural Alt selection.
pub(crate) const FLAG_HAS_BRANCH: u8 = 1;
/// `flags` bit: a `u64` leaf's `child_off` indexes the payload arena
/// rather than carrying an inline scalar.
pub(crate) const FLAG_U64_ARENA_PAYLOAD: u8 = 1 << 1;
/// `flags` bit: the compound has at least one child.
pub(crate) const FLAG_HAS_CHILDREN: u8 = 1 << 6;
/// One flat-tape node — 16 bytes, `#[repr(C)]`, align 4.
///
/// `kind_meta` carries the [`TapeKind`] in the low nibble. Exact rule
/// identity lives in the builder's parallel `u32` lane rather than being
/// truncated into this record. For compounds, `extra` carries the full
/// structural [`bbnf_ir::AltBranchId`]. For leaves, it carries the
/// [`LeafType`] tag.
/// `span_lo/span_hi` are byte offsets into the source. `child_off` is the
/// overloaded first-child / inline-payload / arena-offset word.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C, align(4))]
pub struct TapeRec {
    /// Low nibble: [`TapeKind`]; remaining bits are reserved.
    pub(crate) kind_meta: u8,
    /// Bit 0: `has_branch`; bit 1: wide `u64`; bit 6: `has_children`.
    pub(crate) flags: u8,
    /// Compound: structural Alt branch; leaf: [`LeafType`] tag.
    pub(crate) extra: u16,
    /// source byte span start.
    pub(crate) span_lo: u32,
    /// source byte span end.
    pub(crate) span_hi: u32,
    /// compound: post-order first-child offset; leaf: inline scalar or
    /// arena offset; [`TAPE_NONE`] for pure-span leaf / empty compound.
    pub(crate) child_off: TapeOffset,
}

const _: () = assert!(core::mem::size_of::<TapeRec>() == 16);
const _: () = assert!(core::mem::align_of::<TapeRec>() == 4);

impl TapeRec {
    /// An open compound with the given source span, no children yet.
    #[inline]
    pub(crate) fn open(span_lo: u32, span_hi: u32, branch: Option<bbnf_ir::AltBranchId>) -> Self {
        let mut rec = TapeRec {
            kind_meta: TapeKind::Open as u8,
            flags: 0,
            extra: branch.map_or(0, bbnf_ir::AltBranchId::get),
            span_lo,
            span_hi,
            child_off: TAPE_NONE,
        };
        if branch.is_some() {
            rec.flags |= FLAG_HAS_BRANCH;
        }
        rec
    }

    /// A typed leaf with the given leaf type, span, and payload word.
    #[inline]
    pub(crate) fn leaf(
        leaf_ty: LeafType,
        span_lo: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) -> Self {
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
    pub fn leaf_type(&self) -> Option<LeafType> {
        (self.kind() == TapeKind::Leaf)
            .then(|| LeafType::from_bits(self.extra))
            .flatten()
    }

    #[inline]
    pub(crate) fn has_u64_arena_payload(&self) -> bool {
        self.kind() == TapeKind::Leaf && self.flags & FLAG_U64_ARENA_PAYLOAD != 0
    }

    #[inline]
    pub(crate) fn set_u64_arena_payload(&mut self) {
        self.flags |= FLAG_U64_ARENA_PAYLOAD;
    }

    /// The structural branch selected on an open compound.
    #[inline]
    pub fn branch(&self) -> Option<bbnf_ir::AltBranchId> {
        (self.kind() == TapeKind::Open && self.flags & FLAG_HAS_BRANCH != 0)
            .then(|| bbnf_ir::AltBranchId::new(self.extra))
    }

    #[inline]
    pub fn has_children(&self) -> bool {
        self.flags & FLAG_HAS_CHILDREN != 0
    }

    #[inline]
    pub(crate) fn set_has_children(&mut self, yes: bool) {
        if yes {
            self.flags |= FLAG_HAS_CHILDREN;
        } else {
            self.flags &= !FLAG_HAS_CHILDREN;
        }
    }

    #[inline]
    pub(crate) fn span(&self) -> (u32, u32) {
        (self.span_lo, self.span_hi)
    }
}
