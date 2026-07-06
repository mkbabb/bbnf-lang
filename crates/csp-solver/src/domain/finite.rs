//! FiniteDomain<T> — a generic finite domain backed by Vec<T>.

use super::traits::Domain;

/// A generic finite domain holding arbitrary values.
///
/// Uses `Vec<T>` internally. Operations are O(n) where n is the domain size.
/// Suitable for domains with non-integer values or when bitset representation
/// is not appropriate.
#[derive(Debug, Clone, PartialEq)]
pub struct FiniteDomain<T: Clone + PartialEq + std::fmt::Debug> {
    values: Vec<T>,
}

impl<T: Clone + PartialEq + std::fmt::Debug> FiniteDomain<T> {
    /// Create a new finite domain from a list of values.
    pub fn new(values: Vec<T>) -> Self {
        Self { values }
    }
}

impl<T: Clone + PartialEq + std::fmt::Debug> Domain for FiniteDomain<T> {
    type Value = T;

    fn size(&self) -> usize {
        self.values.len()
    }

    fn contains(&self, val: &T) -> bool {
        self.values.contains(val)
    }

    fn remove(&mut self, val: &T) -> bool {
        if let Some(pos) = self.values.iter().position(|v| v == val) {
            self.values.swap_remove(pos);
            true
        } else {
            false
        }
    }

    fn add(&mut self, val: &T) {
        if !self.values.contains(val) {
            self.values.push(val.clone());
        }
    }

    fn values(&self) -> Vec<T> {
        self.values.clone()
    }

    fn iter(&self) -> impl Iterator<Item = T> + use<T> {
        self.values.clone().into_iter()
    }
}
