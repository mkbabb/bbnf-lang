//! The unifier's laws (LAYOUT-HM-V2 §11 item 2, §9 L-REF), over generated
//! terms. Each law runs a fixed number of seeded cases; a failure names its
//! seed.
//!
//! - soundness: an mgu unifies;
//! - idempotence: the mgu is idempotent (`S∘S = S`; no domain variable in its
//!   range), and the meet of a term with itself is the empty substitution;
//! - most general: every kind-respecting unifier factors through the mgu;
//! - commutativity and associativity up to renaming;
//! - antisymmetry of the instance order up to renaming;
//! - occurs refusal;
//! - the kind chain: the meet of two variables has the smaller kind, and no
//!   binding leaves its variable's kind;
//! - generalise and instantiate round trip.

use std::collections::BTreeSet;

use crate::sample::Rng;

use super::arb::{self, KINDS};
use super::order::{alpha_equivalent, canonical, instance_of, same_up_to_renaming};
use super::scheme::{generalise, instantiate, Scheme};
use super::subst::{mgu, Subst, UnifyError};
use super::ty::{Base, Head, Kind, Ty, Var, Vars};

const CASES: u64 = 3_000;

/// A case: a fresh supply, a pool of shared variables, and the rng.
fn case(seed: u64) -> (Rng, Vars, Vec<Var>) {
    let mut rng = Rng::new(seed.wrapping_mul(0x2545_f491_4f6c_dd1d) ^ 0x6a09_e667);
    let mut vars = Vars::default();
    let n = 1 + rng.below(4) as usize;
    let pool = arb::pool(&mut rng, &mut vars, n);
    (rng, vars, pool)
}

/// A pair that unifies by construction (two abstractions of one term), with
/// the kind-respecting unifier the construction witnesses.
fn unifiable_pair(rng: &mut Rng, vars: &mut Vars, pool: &[Var]) -> (Ty, Ty, Subst) {
    let t = arb::ty(rng, pool, 4);
    let mut theta = Vec::new();
    let a = arb::abstracted(rng, vars, &t, &mut theta);
    let b = arb::abstracted(rng, vars, &t, &mut theta);
    (a, b, Subst::from_map(theta.into_iter().collect()))
}

/// A pair: half by construction, half independent (mostly not unifiable).
fn any_pair(rng: &mut Rng, vars: &mut Vars, pool: &[Var]) -> (Ty, Ty) {
    if rng.chance(1, 2) {
        let (a, b, _) = unifiable_pair(rng, vars, pool);
        (a, b)
    } else {
        (arb::ty(rng, pool, 4), arb::ty(rng, pool, 4))
    }
}

/// Every variable of the given terms, as one tuple, so a substitution can be
/// compared as a whole: `S(⟨x̄⟩) = ⟨S(x̄)⟩`.
fn all_vars(ts: &[&Ty]) -> Ty {
    let mut seen = Vec::new();
    for t in ts {
        for v in t.free_vars() {
            if !seen.contains(&v) {
                seen.push(v);
            }
        }
    }
    Ty::Con(Head::Prod, seen.into_iter().map(Ty::Var).collect())
}

#[test]
fn soundness_an_mgu_unifies() {
    let mut ok = 0;
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let (a, b) = any_pair(&mut rng, &mut vars, &pool);
        if let Ok(s) = mgu(&vars, &a, &b) {
            ok += 1;
            assert_eq!(s.apply(&a), s.apply(&b), "seed {seed}: {a:?} ~ {b:?} under {s:?}");
        }
    }
    assert!(ok > CASES / 3, "too few unifiable cases ({ok}) for the law to bite");
}

#[test]
fn idempotence_the_mgu_is_idempotent_and_the_self_meet_is_empty() {
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let (a, b) = any_pair(&mut rng, &mut vars, &pool);
        if let Ok(s) = mgu(&vars, &a, &b) {
            for (v, r) in s.bindings() {
                assert!(s.bindings().all(|(w, _)| !r.mentions(w)), "seed {seed}: {v} := {r:?} mentions the domain of {s:?}");
            }
            let x = all_vars(&[&a, &b]);
            assert_eq!(s.apply(&s.apply(&x)), s.apply(&x), "seed {seed}: S∘S ≠ S");
        }
        assert!(mgu(&vars, &a, &a).expect("a term unifies with itself").is_empty(), "seed {seed}: mgu(a, a) ≠ ∅");
    }
}

#[test]
fn most_general_every_unifier_factors_through_the_mgu() {
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let (a, b, theta) = unifiable_pair(&mut rng, &mut vars, &pool);
        assert_eq!(theta.apply(&a), theta.apply(&b), "seed {seed}: the witness does not unify");
        let s = mgu(&vars, &a, &b).unwrap_or_else(|e| panic!("seed {seed}: unifiable pair refused: {e:?}\n{a:?}\n{b:?}"));
        // θ = ρ∘S for some kind-respecting ρ: S(x̄) generalises θ(x̄) as a whole.
        let x = all_vars(&[&a, &b]);
        assert!(instance_of(&vars, &s.apply(&x), &theta.apply(&x)), "seed {seed}: θ does not factor through S = {s:?}");
    }
}

#[test]
fn commutativity_up_to_renaming() {
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let (a, b) = any_pair(&mut rng, &mut vars, &pool);
        let x = all_vars(&[&a, &b]);
        match (mgu(&vars, &a, &b), mgu(&vars, &b, &a)) {
            (Ok(s1), Ok(s2)) => {
                assert!(same_up_to_renaming(&vars, &s1.apply(&x), &s2.apply(&x)), "seed {seed}: {s1:?} vs {s2:?}");
            }
            (Err(_), Err(_)) => {}
            (l, r) => panic!("seed {seed}: one order unifies, the other not: {l:?} / {r:?}"),
        }
    }
}

#[test]
fn associativity_up_to_renaming() {
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let t = arb::ty(&mut rng, &pool, 4);
        let mut theta = Vec::new();
        let mut three = || arb::abstracted(&mut rng, &mut vars, &t, &mut theta);
        let (a, b, c) = (three(), three(), three());
        let x = all_vars(&[&a, &b, &c]);
        // (a ⊓ b) ⊓ c against a ⊓ (b ⊓ c).
        let left = mgu(&vars, &a, &b).and_then(|mut s| s.unify(&vars, &a, &c).map(|()| s));
        let right = mgu(&vars, &b, &c).and_then(|mut s| s.unify(&vars, &a, &b).map(|()| s));
        let (l, r) = (left.unwrap_or_else(|e| panic!("seed {seed}: {e:?}")), right.unwrap_or_else(|e| panic!("seed {seed}: {e:?}")));
        assert!(same_up_to_renaming(&vars, &l.apply(&x), &r.apply(&x)), "seed {seed}: {l:?} vs {r:?}");
    }
}

#[test]
fn antisymmetry_of_the_instance_order_up_to_renaming() {
    let (mut both, mut strict) = (0, 0);
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let a = arb::ty(&mut rng, &pool, 4);
        let b = match rng.below(3) {
            0 => arb::renamed(&mut vars, &a),
            1 => {
                let mut theta = Vec::new();
                arb::abstracted(&mut rng, &mut vars, &a, &mut theta)
            }
            _ => arb::ty(&mut rng, &pool, 4),
        };
        let (ab, ba) = (instance_of(&vars, &a, &b), instance_of(&vars, &b, &a));
        if ab && ba {
            both += 1;
            assert!(same_up_to_renaming(&vars, &a, &b), "seed {seed}: mutual instances not renamings: {a:?} / {b:?}");
        }
        if ab != ba {
            strict += 1;
        }
    }
    assert!(both > CASES / 10 && strict > CASES / 10, "law did not bite: both {both}, strict {strict}");
}

#[test]
fn occurs_a_variable_never_binds_to_a_term_containing_it() {
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let v = vars.fresh(Kind::Any);
        let t = arb::containing(&mut rng, &pool, v, 4);
        for (l, r) in [(Ty::Var(v), t.clone()), (t.clone(), Ty::Var(v))] {
            match mgu(&vars, &l, &r) {
                Err(UnifyError::Occurs(w, _)) if w == v => {}
                other => panic!("seed {seed}: {v} ~ {t:?} gave {other:?}, not an occurs refusal"),
            }
        }
    }
    // N01's shape on the same unifier: `x(x)` needs `α = fn(α) → β`.
    let mut vars = Vars::default();
    let (a, b) = (vars.fresh(Kind::Any), vars.fresh(Kind::Any));
    let omega = Ty::func(vec![Ty::Var(a)], Ty::Var(b));
    assert!(matches!(mgu(&vars, &Ty::Var(a), &omega), Err(UnifyError::Occurs(..))));
}

#[test]
fn the_kind_chain_meets_downward_and_no_binding_leaves_its_kind() {
    for k1 in KINDS {
        for k2 in KINDS {
            let mut vars = Vars::default();
            let (x, y) = (vars.fresh(k1), vars.fresh(k2));
            let s = mgu(&vars, &Ty::Var(x), &Ty::Var(y)).expect("two variables unify");
            let r = s.apply(&Ty::Var(x));
            assert_eq!(r, s.apply(&Ty::Var(y)));
            assert_eq!(vars.kind_of(&r), k1.min(k2), "meet of {k1:?} and {k2:?}");
            for b in Base::ALL {
                assert_eq!(mgu(&vars, &Ty::Var(x), &Ty::base(b)).is_ok(), b.kind() <= k1, "{k1:?} ~ {b:?}");
            }
            let list = Ty::list(Ty::base(Base::U8));
            assert_eq!(mgu(&vars, &Ty::Var(x), &list).is_ok(), k1 == Kind::Any, "{k1:?} ~ List u8");
        }
    }
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let (a, b) = any_pair(&mut rng, &mut vars, &pool);
        if let Ok(s) = mgu(&vars, &a, &b) {
            for (v, r) in s.bindings() {
                assert!(vars.kind_of(r) <= vars.kind(v), "seed {seed}: {v}:{:?} := {r:?}", vars.kind(v));
            }
        }
    }
}

#[test]
fn generalise_and_instantiate_round_trip() {
    for seed in 0..CASES {
        let (mut rng, mut vars, pool) = case(seed);
        let t = arb::ty(&mut rng, &pool, 4);
        let env: BTreeSet<Var> = pool.iter().copied().filter(|_| rng.chance(1, 2)).collect();
        let sigma = generalise(&env, &t);
        let fv: BTreeSet<Var> = t.free_vars().into_iter().collect();
        let q: BTreeSet<Var> = sigma.quantified.iter().copied().collect();
        assert_eq!(q, fv.difference(&env).copied().collect(), "seed {seed}: quantified set");

        let (i1, i2) = (instantiate(&mut vars, &sigma), instantiate(&mut vars, &sigma));
        // Same shape and kinds; environment variables verbatim.
        assert_eq!(canonical(&vars, &env, &i1), canonical(&vars, &env, &t), "seed {seed}: instance ≠ body up to renaming");
        let fresh = |i: &Ty| -> BTreeSet<Var> { i.free_vars().into_iter().filter(|v| !env.contains(v)).collect() };
        let (f1, f2) = (fresh(&i1), fresh(&i2));
        assert!(f1.is_disjoint(&fv), "seed {seed}: instance reuses a variable of the body");
        assert!(f1.is_disjoint(&f2), "seed {seed}: two instances share a variable");
        assert!(alpha_equivalent(&vars, &generalise(&env, &i1), &sigma), "seed {seed}: generalise ∘ instantiate ≠ id");
        // Instantiating a monotype changes nothing.
        assert_eq!(instantiate(&mut vars, &Scheme::mono(t.clone())), t);
    }
}
