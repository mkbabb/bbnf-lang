//! Host function table used during lowering.

use bbnf_ir::{FnDescriptor, FnId};

/// Host function table used during lowering.
pub(crate) struct FnTable {
    pub(crate) fns: Vec<FnDescriptor>,
}

impl FnTable {
    pub(crate) fn new() -> Self {
        Self { fns: Vec::new() }
    }

    pub(crate) fn push(&mut self, desc: FnDescriptor) -> FnId {
        let id = self.fns.len() as FnId;
        self.fns.push(desc);
        id
    }
}
