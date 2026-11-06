//! Falsifiable v2 carrier for bbnf-lang semantic type inference.
//!
//! Construction, solving, generalization, layout, and schema projection are
//! separate authority boundaries. Public handles are opaque and scoped to one
//! immutable store revision.

mod build;
mod error;
mod id;
mod layout;
mod project;
mod scheme;
mod shape;
mod solve;
mod term;

pub use build::{Binder, StoreBuilder};
pub use error::{BuildError, Exhaustion, LayoutError, ProjectError, SolveError};
pub use id::{MetaId, RowId, TypeId};
pub use layout::{lower_layout, Abi, Layout, LayoutPlan};
pub use project::{GeneratedArtifacts, Projection};
pub use scheme::{Boundary, Scheme};
pub use shape::{GrammarShape, ShapeMismatch};
pub use solve::{solve, Budget, Constraint, Origin, Solution, SolvedRow, SolvedTail, SolvedType};
pub use term::{Primitive, RowSpec, TypeStore};
