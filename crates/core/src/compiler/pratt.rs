use std::iter::FusedIterator;
use std::ops::Range;

use serde::Serialize;

use super::source::Revision;
use super::syntax::{RuleId, RuleIx};

// Pratt ordinals remain compiler-private and four bytes wide. Public callers
// receive only the corresponding full-revision capabilities below.
macro_rules! local_index_type {
    ($name:ident) => {
        #[repr(transparent)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
        pub(crate) struct $name(u32);

        impl $name {
            pub(crate) fn checked(index: usize) -> Option<Self> {
                Some(Self(u32::try_from(index).ok()?))
            }

            pub(crate) const fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

local_index_type!(PrattTableIx);
local_index_type!(PrattOperatorIx);

macro_rules! capability_type {
    ($name:ident, $local:ident) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name {
            local: $local,
            revision: Revision,
        }

        impl $name {
            pub(crate) const fn bind(revision: Revision, local: $local) -> Self {
                Self { local, revision }
            }

            pub(crate) const fn local(self) -> $local {
                self.local
            }

            pub const fn revision(self) -> Revision {
                self.revision
            }

            pub const fn index(self) -> usize {
                self.local.index()
            }
        }
    };
}

capability_type!(PrattTableId, PrattTableIx);
capability_type!(PrattOperatorId, PrattOperatorIx);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum PrattFixity {
    Prefix,
    Postfix,
    Infix,
}

/// Immutable, revision-branded view of one resolved Pratt table.
///
/// The row retains compact local ordinals. Its public accessors bind those
/// ordinals to the owning compilation revision on demand.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PrattTable {
    revision: Revision,
    id: PrattTableIx,
    site_rule: RuleIx,
    atom_rule: RuleIx,
    operator_start: PrattOperatorIx,
    operator_len: u32,
}

impl PrattTable {
    pub(crate) fn checked(
        revision: Revision,
        id: PrattTableIx,
        site_rule: RuleIx,
        atom_rule: RuleIx,
        operator_start: PrattOperatorIx,
        operator_len: usize,
    ) -> Option<Self> {
        let operator_len = u32::try_from(operator_len).ok()?;
        let start = operator_start.index();
        let end = start.checked_add(operator_len as usize)?;
        if operator_len != 0 {
            PrattOperatorIx::checked(end.checked_sub(1)?)?;
        }
        Some(Self {
            revision,
            id,
            site_rule,
            atom_rule,
            operator_start,
            operator_len,
        })
    }

    pub const fn id(&self) -> PrattTableId {
        PrattTableId::bind(self.revision, self.id)
    }

    pub const fn site_rule(&self) -> RuleId {
        RuleId::bind(self.revision, self.site_rule)
    }

    pub const fn atom_rule(&self) -> RuleId {
        RuleId::bind(self.revision, self.atom_rule)
    }

    pub fn operators(
        &self,
    ) -> impl Clone + DoubleEndedIterator<Item = PrattOperatorId> + ExactSizeIterator + FusedIterator + '_
    {
        PrattOperatorIds {
            revision: self.revision,
            remaining: self.operator_range(),
        }
    }

    pub(crate) const fn id_local(&self) -> PrattTableIx {
        self.id
    }

    pub(crate) const fn site_rule_local(&self) -> RuleIx {
        self.site_rule
    }

    pub(crate) const fn atom_rule_local(&self) -> RuleIx {
        self.atom_rule
    }

    pub(crate) const fn operator_start_local(&self) -> PrattOperatorIx {
        self.operator_start
    }

    pub(crate) const fn operator_len(&self) -> u32 {
        self.operator_len
    }

    fn operator_range(&self) -> Range<usize> {
        let start = self.operator_start.index();
        let end = start
            .checked_add(self.operator_len as usize)
            .expect("checked Pratt operator range remains representable");
        start..end
    }
}

/// Immutable, revision-branded view of one resolved Pratt operator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PrattOperator {
    revision: Revision,
    id: PrattOperatorIx,
    table: PrattTableIx,
    matcher_rule: RuleIx,
    fixity: PrattFixity,
    left_binding_power: Option<u16>,
    right_binding_power: Option<u16>,
}

impl PrattOperator {
    pub(crate) const fn new(
        revision: Revision,
        id: PrattOperatorIx,
        table: PrattTableIx,
        matcher_rule: RuleIx,
        fixity: PrattFixity,
        left_binding_power: Option<u16>,
        right_binding_power: Option<u16>,
    ) -> Self {
        Self {
            revision,
            id,
            table,
            matcher_rule,
            fixity,
            left_binding_power,
            right_binding_power,
        }
    }

    pub const fn id(&self) -> PrattOperatorId {
        PrattOperatorId::bind(self.revision, self.id)
    }

    pub const fn table(&self) -> PrattTableId {
        PrattTableId::bind(self.revision, self.table)
    }

    pub const fn matcher_rule(&self) -> RuleId {
        RuleId::bind(self.revision, self.matcher_rule)
    }

    pub const fn fixity(&self) -> PrattFixity {
        self.fixity
    }

    pub const fn left_binding_power(&self) -> Option<u16> {
        self.left_binding_power
    }

    pub const fn right_binding_power(&self) -> Option<u16> {
        self.right_binding_power
    }

    pub(crate) const fn id_local(&self) -> PrattOperatorIx {
        self.id
    }

    pub(crate) const fn table_local(&self) -> PrattTableIx {
        self.table
    }

    pub(crate) const fn matcher_rule_local(&self) -> RuleIx {
        self.matcher_rule
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PrattOperatorIds {
    revision: Revision,
    remaining: Range<usize>,
}

impl Iterator for PrattOperatorIds {
    type Item = PrattOperatorId;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.remaining.next()?;
        let local = PrattOperatorIx::checked(index)
            .expect("checked Pratt operator range contains representable ordinals");
        Some(PrattOperatorId::bind(self.revision, local))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.remaining.size_hint()
    }
}

impl DoubleEndedIterator for PrattOperatorIds {
    fn next_back(&mut self) -> Option<Self::Item> {
        let index = self.remaining.next_back()?;
        let local = PrattOperatorIx::checked(index)
            .expect("checked Pratt operator range contains representable ordinals");
        Some(PrattOperatorId::bind(self.revision, local))
    }
}

impl ExactSizeIterator for PrattOperatorIds {}
impl FusedIterator for PrattOperatorIds {}

pub(crate) fn checked_table(
    revision: Revision,
    rows: &[PrattTable],
    id: PrattTableId,
) -> Option<&PrattTable> {
    (id.revision() == revision)
        .then(|| rows.get(id.local().index()))
        .flatten()
        .filter(|row| row.id() == id)
}

pub(crate) fn checked_operator(
    revision: Revision,
    rows: &[PrattOperator],
    id: PrattOperatorId,
) -> Option<&PrattOperator> {
    (id.revision() == revision)
        .then(|| rows.get(id.local().index()))
        .flatten()
        .filter(|row| row.id() == id)
}

#[cfg(test)]
mod representation_laws {
    use std::mem::size_of;

    use super::{PrattOperatorIx, PrattTableIx};

    #[test]
    fn pratt_ordinals_remain_four_bytes() {
        assert_eq!(size_of::<PrattTableIx>(), 4);
        assert_eq!(size_of::<PrattOperatorIx>(), 4);
    }
}
