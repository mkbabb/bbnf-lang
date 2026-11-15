//! Equality up to renaming, and the instance order.
//!
//! Both are what the reference checker of LAYOUT-HM-V2 §9 needs: L-REF
//! compares an inferred type with the reference's up to renaming, and
//! L-PRINCIPAL asks whether a generated type is an instance of the inferred
//! one.

use std::collections::{BTreeMap, BTreeSet};

use super::scheme::Scheme;
use super::ty::{Head, Kind, Ty, Var, Vars};

/// A type with its renamable variables replaced by their order of first
/// occurrence and kind; kept variables stay as they are.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Canon {
    Kept(Var),
    Bound(u32, Kind),
    Con(Head, Vec<Canon>),
}

/// The canonical form of `t`, renaming every variable not in `keep`. Two types
/// are equal up to a kind-preserving renaming exactly when their canonical
/// forms are equal.
pub(crate) fn canonical(vars: &Vars, keep: &BTreeSet<Var>, t: &Ty) -> Canon {
    fn walk(vars: &Vars, keep: &BTreeSet<Var>, t: &Ty, seen: &mut BTreeMap<Var, u32>) -> Canon {
        match t {
            Ty::Var(v) if keep.contains(v) => Canon::Kept(*v),
            Ty::Var(v) => {
                let next = seen.len() as u32;
                Canon::Bound(*seen.entry(*v).or_insert(next), vars.kind(*v))
            }
            Ty::Con(h, args) => Canon::Con(h.clone(), args.iter().map(|a| walk(vars, keep, a, seen)).collect()),
        }
    }
    walk(vars, keep, t, &mut BTreeMap::new())
}

/// Equal up to a kind-preserving renaming of every variable.
pub(crate) fn same_up_to_renaming(vars: &Vars, a: &Ty, b: &Ty) -> bool {
    let none = BTreeSet::new();
    canonical(vars, &none, a) == canonical(vars, &none, b)
}

/// Alpha-equivalence of schemes: quantified variables renamed, free ones kept.
pub(crate) fn alpha_equivalent(vars: &Vars, a: &Scheme, b: &Scheme) -> bool {
    let free_a: BTreeSet<Var> = a.free_vars().into_iter().collect();
    let free_b: BTreeSet<Var> = b.free_vars().into_iter().collect();
    free_a == free_b
        && a.quantified.len() == b.quantified.len()
        && canonical(vars, &free_a, &a.body) == canonical(vars, &free_b, &b.body)
}

/// Is `target` an instance of `general`: is there a kind-respecting θ with
/// `θ(general) = target`? Variables of `target` are treated as constants of
/// their kind; a variable of `general` may be mapped to a type whose least
/// kind is within its own.
pub(crate) fn instance_of(vars: &Vars, general: &Ty, target: &Ty) -> bool {
    fn go(vars: &Vars, g: &Ty, t: &Ty, theta: &mut BTreeMap<Var, Ty>) -> bool {
        match (g, t) {
            (Ty::Var(v), _) => match theta.get(v) {
                Some(bound) => bound == t,
                None if vars.kind_of(t) <= vars.kind(*v) => {
                    theta.insert(*v, t.clone());
                    true
                }
                None => false,
            },
            (Ty::Con(h1, a1), Ty::Con(h2, a2)) => {
                h1 == h2 && a1.len() == a2.len() && a1.iter().zip(a2).all(|(x, y)| go(vars, x, y, theta))
            }
            (Ty::Con(..), Ty::Var(_)) => false,
        }
    }
    go(vars, general, target, &mut BTreeMap::new())
}
