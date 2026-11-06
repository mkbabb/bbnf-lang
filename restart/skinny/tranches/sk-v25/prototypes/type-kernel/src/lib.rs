//! A falsifiable semantic type-kernel prototype for BB-1 adjudication.

mod error;
mod layout;
mod scheme;
mod solve;
mod term;

pub use error::{ErrorKind, Origin, RowSide, TypeError};
pub use layout::{Layout, LayoutPlan};
pub use scheme::{FreeVariables, ReuseBoundary, Scheme};
pub use solve::{Constraint, Solution};
pub use term::{Primitive, Row, RowTail, RowVarId, TypeId, TypeStore, TypeTerm, TypeVarId};
