//! Constraint trait, built-in implementations, and devirtualized dispatch.

pub mod all_different;
pub mod all_different_except;
pub mod bounded_sum;
pub mod dispatch;
pub mod implication;
pub mod lambda;
pub mod not_equal;
pub(crate) mod scratch;
pub mod traits;

pub use all_different::AllDifferent;
pub use all_different_except::AllDifferentExcept;
pub use bounded_sum::BoundedSumConstraint;
pub use dispatch::ConstraintEnum;
pub use implication::ImplicationConstraint;
pub use lambda::LambdaConstraint;
pub use not_equal::NotEqual;
pub use traits::{Constraint, Revision, VarId};
