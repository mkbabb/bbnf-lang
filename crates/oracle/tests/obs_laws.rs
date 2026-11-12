//! sk-v27 LW3.COMP-4a: the laws of `obs()` and of the failure contract, at
//! 10⁴ generated cases each (seeded; a failing case prints its seed).

use bbnf_oracle::obs::{ContractError, FailureContract, Obs, ObsClass, equivalent, obs};
use bbnf_oracle::sample::{self, Rng};

const CASES: u64 = 10_000;
const SEED: u64 = 0x5eed_c0de_4a00_0001;

fn each_tree(mut f: impl FnMut(u64, &mut Rng, Obs)) {
    for i in 0..CASES {
        let seed = SEED ^ i.wrapping_mul(0x9e37_79b9_7f4a_7c15);
        let mut rng = Rng::new(seed);
        let t = sample::tree(&mut rng, 5);
        f(seed, &mut rng, t);
    }
}

#[test]
fn obs_is_reflexive_for_every_class() {
    each_tree(|seed, _, t| {
        let u = t.clone();
        for c in ObsClass::ALL {
            assert!(
                equivalent(c, &t, &u),
                "seed {seed:#x}: {c} not reflexive on {t:?}"
            );
        }
    });
}

#[test]
fn every_class_is_an_idempotent_projection() {
    each_tree(|seed, _, t| {
        for c in ObsClass::ALL {
            let once = obs(c, &t);
            assert_eq!(
                obs(c, &once),
                once,
                "seed {seed:#x}: obs({c}) is not idempotent on {t:?}"
            );
        }
    });
}

#[test]
fn exact_is_contained_in_semantic_and_semantic_in_the_coarser_classes() {
    let mut nontrivial = 0u64;
    each_tree(|seed, rng, t| {
        // A pair that is exact-equal is semantic-equal (the clone), and a pair
        // that is semantic-equal but not exact-equal (the noise) is equal under
        // every class at least as coarse as `semantic`.
        let pairs = [t.clone(), sample::semantic_noise(rng, &t)];
        for u in &pairs {
            if equivalent(ObsClass::Exact, &t, u) {
                assert!(
                    equivalent(ObsClass::Semantic, &t, u),
                    "seed {seed:#x}: exact ⊄ semantic"
                );
            }
            assert!(
                equivalent(ObsClass::Semantic, &t, u),
                "seed {seed:#x}: the declared normalisation saw noise"
            );
            assert!(
                equivalent(ObsClass::ScalarEvents, &t, u),
                "seed {seed:#x}: semantic ⊄ scalar-events"
            );
            assert!(
                equivalent(ObsClass::LeftFold, &t, u),
                "seed {seed:#x}: semantic ⊄ left-fold"
            );
        }
        if !equivalent(ObsClass::Exact, &t, &pairs[1]) {
            nontrivial += 1;
        }
    });
    // The containment is strict on the generated pairs, so the law is not vacuous.
    assert!(
        nontrivial > CASES / 2,
        "only {nontrivial} noisy pairs differed exactly"
    );
}

#[test]
fn semantic_sees_a_label_or_text_change() {
    let mut changed = 0u64;
    each_tree(|seed, rng, t| {
        if let Some(u) = sample::semantic_change(rng, &t) {
            changed += 1;
            assert!(
                !equivalent(ObsClass::Exact, &t, &u),
                "seed {seed:#x}: exact missed a change"
            );
            assert!(
                !equivalent(ObsClass::Semantic, &t, &u),
                "seed {seed:#x}: semantic missed a change"
            );
        }
    });
    assert!(
        changed > CASES / 2,
        "only {changed} trees had a site to change"
    );
}

#[test]
fn left_fold_tells_a_left_chain_from_a_right_nest() {
    let leaf = |s: &str| Obs::leaf(None, None, s);
    let flat = Obs::node(
        Some("expr"),
        None,
        vec![leaf("1"), leaf("-"), leaf("2"), leaf("-"), leaf("3")],
    );
    let left = Obs::node(
        Some("expr"),
        None,
        vec![
            Obs::node(Some("expr"), None, vec![leaf("1"), leaf("-"), leaf("2")]),
            leaf("-"),
            leaf("3"),
        ],
    );
    let right = Obs::node(
        Some("expr"),
        None,
        vec![
            leaf("1"),
            leaf("-"),
            Obs::node(Some("expr"), None, vec![leaf("2"), leaf("-"), leaf("3")]),
        ],
    );
    assert!(equivalent(ObsClass::LeftFold, &flat, &left));
    assert!(!equivalent(ObsClass::LeftFold, &flat, &right));
    assert!(!equivalent(ObsClass::LeftFold, &left, &right));
    assert!(equivalent(ObsClass::ScalarEvents, &left, &right));
}

#[test]
fn an_unsorted_expected_set_fails_to_construct() {
    let s = |v: &[&str]| v.iter().map(|x| x.to_string()).collect::<Vec<_>>();
    assert!(FailureContract::new(3, s(&["\",\"", "\"]\""])).is_ok());
    assert!(matches!(
        FailureContract::new(3, s(&["\"]\"", "\",\""])),
        Err(ContractError::NotStrictlySorted { at: 0, .. })
    ));
    assert!(matches!(
        FailureContract::new(3, s(&["a", "b", "b"])),
        Err(ContractError::NotStrictlySorted { at: 1, .. })
    ));
    // The serialized form is refused the same way.
    assert!(
        serde_json::from_str::<FailureContract>(r#"{"furthest":1,"expected":["b","a"]}"#).is_err()
    );
    assert!(
        serde_json::from_str::<FailureContract>(r#"{"furthest":1,"expected":["a","b"]}"#).is_ok()
    );

    let mut rng = Rng::new(SEED);
    let (mut refused, mut built) = (0u64, 0u64);
    for _ in 0..CASES {
        let l = sample::labels(&mut rng);
        let strictly_sorted = l.windows(2).all(|w| w[0] < w[1]);
        match FailureContract::new(rng.below(100) as u32, l.clone()) {
            Ok(c) => {
                built += 1;
                assert!(strictly_sorted, "{l:?} constructed");
                assert_eq!(c.expected(), &l[..]);
            }
            Err(_) => {
                refused += 1;
                assert!(!strictly_sorted, "{l:?} refused");
            }
        }
    }
    assert!(refused > 0 && built > 0);
}

#[test]
fn merge_keeps_the_furthest_and_unions_at_equal_offset() {
    let mut rng = Rng::new(SEED ^ 0xfa11);
    let sorted = |rng: &mut Rng| {
        let mut l = sample::labels(rng);
        l.sort();
        l.dedup();
        FailureContract::new(rng.below(4) as u32, l).unwrap()
    };
    for _ in 0..CASES {
        let (a, b, c) = (sorted(&mut rng), sorted(&mut rng), sorted(&mut rng));
        // commutative and associative: the order alternatives fail in does not matter
        assert_eq!(a.clone().merge(b.clone()), b.clone().merge(a.clone()));
        assert_eq!(
            a.clone().merge(b.clone()).merge(c.clone()),
            a.clone().merge(b.clone().merge(c.clone()))
        );
        let m = a.clone().merge(b.clone());
        assert_eq!(m.furthest(), a.furthest().max(b.furthest()));
        // a later failure at a smaller offset does not replace the furthest one
        if a.furthest() > b.furthest() {
            assert_eq!(m, a);
        }
        if a.furthest() == b.furthest() {
            let mut u: Vec<String> = a.expected().iter().chain(b.expected()).cloned().collect();
            u.sort();
            u.dedup();
            assert_eq!(m.expected(), &u[..]);
        }
        // the merged set is itself a valid contract
        assert!(FailureContract::new(m.furthest(), m.expected().to_vec()).is_ok());
    }
}
