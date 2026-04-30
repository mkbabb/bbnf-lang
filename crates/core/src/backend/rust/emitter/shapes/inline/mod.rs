//! Inline StructDirect branch emitters.
//!
//! # Role
//!
//! Keyword branch dispatch can choose a structural `Seq` branch whose
//! positions are not plain literals. The helper exported here emits
//! that branch directly through the StructBuilder substrate: literals
//! and regexes advance input, refs delegate to shape fns, and
//! speculative operators use builder checkpoints for rollback.
//!
//! # Wire contract
//!
//! This module intentionally has one live entry point:
//! [`emit_seq_branch_structural_struct_direct`]. The former inline
//! tape emitters were unreachable after `EmitStrategy` became
//! StructDirect-only and were deleted in AZ-II/O5.

mod structural_branch;

pub(super) use structural_branch::emit_seq_branch_structural_struct_direct;
