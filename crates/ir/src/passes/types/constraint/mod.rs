//! CSP type constraint definitions using the generalized `csp-solver` crate.
//!
//! Models type inference as a Constraint Satisfaction Problem:
//! - **Variables**: One per IR node, domain is `TypeDomain` (wrapping
//!   `Option<TypeDesc>`).
//! - **Constraints**: Type equations from grammar structure (Seq projection,
//!   Alt join, Optional/Repeat wrapping, Skip/Next/Minus projection,
//!   Map override).
//! - **Solver**: AC-3 propagation via `csp_solver::Csp::propagate()`.
//!
//! The domain of each variable is a single `TypeDesc` value (deterministic —
//! no search/backtracking needed). AC-3 propagation resolves all constraints
//! in a single pass for acyclic grammars; cyclic grammars converge via
//! fixed-point iteration.
//!
//! Sub-modules:
//!
//! - [`domain`] — `TypeDomain` + `Domain`/`LatticeDomain` impls.
//! - [`grounds`] — `GroundConstraint`, `EqualConstraint`.
//! - [`seq`] — `SeqConstraint`, `SeqChildKind`.
//! - [`alt`] — `AltConstraint`, `AltInVecConstraint`.
//! - [`reference`] — `RefConstraint` (scalar-propagating Ref).
//! - [`operators`] — `OptionalConstraint`, `RepeatConstraint`,
//!   `ProjectConstraint`, `MapConstraint`.
//! - [`helpers`] — internal `assign`, `project_seq_type`, `join_types`.

pub mod alt;
pub mod domain;
pub mod grounds;
pub mod helpers;
pub mod operators;
pub mod reference;
pub mod seq;

pub use csp_solver::constraint::VarId as TypeVarId;

pub use alt::{AltConstraint, AltInVecConstraint};
pub use domain::TypeDomain;
pub use grounds::{EqualConstraint, GroundConstraint};
pub use operators::{MapConstraint, OptionalConstraint, ProjectConstraint, RepeatConstraint};
pub use reference::RefConstraint;
pub use seq::{SeqChildKind, SeqConstraint};
