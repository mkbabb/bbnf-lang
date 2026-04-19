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

/// AX.W1.A — first-class per-grammar `Value` projection API.
///
/// Consumer-facing re-export surface for the typed value trees the
/// BBNF tape walker produces. Each grammar contributes one submodule
/// mirroring the shape of its external comparator:
///
/// - [`json`] — `bbnf::json::Value` ↔ `sonic_rs::Value` (AX.W1.A).
///
/// Additional grammars (`css`, `sheets`, `bbnf`) join this module as
/// W1.B / W1.C lands their Value types. Each projection lives under
/// `backend::rust::view::<grammar>::` and is re-exported here so
/// downstream consumers import from a stable, grammar-scoped path.
pub mod json {
    pub use crate::backend::rust::view::json::{JsonRuleIds, N, Number, Value};
}
