//! AZ-II.cutover.A — DTA live-substrate types, re-exported from
//! [`bbnf_ir::dta`].
//!
//! Per `audit/AUDIT-6-ARCHITECTURE.md` §8.2 the four types
//! ([`DtaStateId`], [`DtaRuleId`], [`DtaAssociativity`],
//! [`DtaPrecedenceEntry`]) hoisted to the IR layer — the IR is the
//! natural owner of rule identity / associativity / precedence-row
//! data the Pratt reducer consumes at compile time. Tape now
//! consumes them via `bbnf_ir::dta::*`; the tape crate's delete at
//! AZ-II.cutover.C retires this re-export module entirely.
//!
//! The re-export keeps in-tree consumers (`tape::driver`,
//! `tape::finaliser`, the `tape::DtaPrecedenceEntry` re-export at
//! the crate boundary) buildable without a touch-and-rewire pass —
//! the IR-side hoist landed unblocked the dep promotion below
//! without forcing every interior consumer to switch import paths
//! in the same commit.

pub use bbnf_ir::dta::{DtaAssociativity, DtaPrecedenceEntry, DtaRuleId, DtaStateId};
