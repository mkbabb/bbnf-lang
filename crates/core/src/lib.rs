// Self-alias so generated code inside `grammar/generated.rs` can
// reference `::bbnf::runtime::*` paths uniformly with downstream
// consumers. Without this, the same code path resolves in
// `bbnf-bootstrap` (external) but fails inside the `bbnf` crate
// where the auto-generated bootstrap module lives.
extern crate self as bbnf;

pub mod types;
pub use types::*;

/// Re-export parse_that types used by the grammar-generated enum.
pub use parse_that::Span;

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
