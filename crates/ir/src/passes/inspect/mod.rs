//! IR-walking primitives shared across passes (Tranche AQ.2).
//!
//! Five categories of small structural helpers historically lived as
//! near-identical copies inside individual recognizer miners and CSP
//! decoders. Tranche AQ.2 consolidates them here so every consumer
//! shares one definition.
//!
//! - [`walk`] — generic one-level child recursion ([`visit_children_alt`]).
//! - [`unwrap`] — wrap-shape decomposer ([`unwrap_wrap`]) and
//!   trivial-wrapper stripper ([`unwrap_map_ow`]).
//! - [`resolve`] — cycle-safe Ref-chain resolvers
//!   ([`resolve_to_seq`] / [`unwrap_to_alt`] / [`unwrap_to_repeat`]).
//! - [`literal`] — single-byte literal extractor ([`single_byte_literal`]).
//! - [`leading`] — leading-position literal/regex extraction
//!   ([`extract_leading_literals`] / [`extract_leading_regex_pattern`]).
//!
//! Every helper is `pub` so consumers may import directly:
//!
//! ```ignore
//! use crate::passes::inspect::{visit_children_alt, single_byte_literal};
//! ```

pub mod leading;
pub mod literal;
pub mod resolve;
pub mod unwrap;
pub mod walk;

pub use leading::{extract_leading_literals, extract_leading_regex_pattern};
pub use literal::single_byte_literal;
pub use resolve::{resolve_to_seq, unwrap_to_alt, unwrap_to_repeat};
pub use unwrap::{unwrap_map_ow, unwrap_wrap};
pub use walk::visit_children_alt;
