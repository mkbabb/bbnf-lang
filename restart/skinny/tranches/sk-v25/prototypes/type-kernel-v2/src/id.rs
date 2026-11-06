use std::fmt;

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeId {
    pub(crate) scope: u64,
    pub(crate) slot: u32,
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct MetaId {
    pub(crate) scope: u64,
    pub(crate) slot: u32,
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RowId {
    pub(crate) scope: u64,
    pub(crate) slot: u32,
}

macro_rules! debug_id {
    ($ty:ident, $prefix:literal) => {
        impl fmt::Debug for $ty {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!($prefix, "@{:x}:{}"), self.scope, self.slot)
            }
        }
    };
}

debug_id!(TypeId, "t");
debug_id!(MetaId, "m");
debug_id!(RowId, "r");
