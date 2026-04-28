//! AZ-II.cutover.A — DTA live-substrate types, hoisted to the IR.
//!
//! Per `audit/AUDIT-6-ARCHITECTURE.md` §8.2 the four types
//! ([`DtaStateId`], [`DtaRuleId`], [`DtaAssociativity`],
//! [`DtaPrecedenceEntry`]) describe codegen-time facts the IR layer
//! is the natural owner of — the rule identity / associativity /
//! precedence row that the Pratt reducer consumes at compile time.
//! Hoisting them out of `tape::dta` retires the workaround crate
//! dependency edge documented at the pre-cutover `tape/src/dta.rs`
//! header.
//!
//! Tape consumes them as `bbnf_ir::dta::*` (via the `bbnf-ir`
//! lib-dep edge promoted at the cutover) until the tape crate's
//! deletion at AZ-II.cutover.C.

pub mod types;
pub use types::*;
