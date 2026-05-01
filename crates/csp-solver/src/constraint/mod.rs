//! Constraint trait, built-in implementations, and devirtualized dispatch.

pub mod all_different;
pub mod all_different_except;
pub mod cardinality;
pub mod dispatch;
pub mod implication;
pub mod lambda;
pub mod not_equal;
pub mod soft;
pub mod traits;

pub use all_different::AllDifferent;
pub use all_different_except::AllDifferentExcept;
pub use cardinality::CardinalityConstraint;
pub use dispatch::ConstraintEnum;
pub use implication::ImplicationConstraint;
pub use lambda::LambdaConstraint;
pub use not_equal::NotEqual;
pub use soft::SoftLambdaConstraint;
pub use traits::{Constraint, Revision, SoftConstraint, VarId};
