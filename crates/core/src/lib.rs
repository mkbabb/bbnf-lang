// B5.W1 — self-alias retired alongside the welded substrate
// boundary. Generated code uses `crate::runtime::*` paths uniformly
// inside the `bbnf` crate; downstream consumers (`bbnf-bootstrap`,
// integration tests) reach the same types via `bbnf::runtime::*`
// where `bbnf` is the cargo dependency, not the self-alias.

pub mod types;
pub use types::*;

/// Re-export parse_that types used by the grammar-generated enum.
pub use parse_that::Span;

// Host shims for grammar-side `-> crate::<module>::<fn>(...)` map
// annotations. Pre-B2 these lived in test-common modules; post-B2.W1
// the xtask-emitted source resides under `bbnf` lib so `crate::`
// resolves here. Tests now reach the symbol via
// `bbnf::css_types::parse_hex_color`.
pub mod css_types;

pub mod grammar;

pub mod generate;
pub use generate::*;

pub mod backend;

pub mod graph;
pub use graph::*;

pub mod imports;

pub mod lower;
pub mod pipeline;
pub mod runtime;
