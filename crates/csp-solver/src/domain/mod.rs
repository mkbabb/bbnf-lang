//! Domain trait and built-in implementations.

pub mod bitset;
pub mod finite;
pub mod lattice;
pub mod traits;

pub use bitset::BitsetDomain;
pub use finite::FiniteDomain;
pub use lattice::BitsetLatticeDomain;
pub use traits::{CostDomain, Domain, LatticeDomain};
