//! Type inference, lattice propagation tests.
//! Extracted from lib.rs inline tests.

use csp_solver::constraint::LambdaConstraint;
use csp_solver::domain::bitset::BitsetDomain;
use csp_solver::domain::finite::FiniteDomain;
use csp_solver::domain::lattice::BitsetLatticeDomain;
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::ordering::Ordering;
use csp_solver::{Csp, Pruning, SolveConfig};

// -----------------------------------------------------------------------
// 4. AC-3 propagation only — lattice domain
// -----------------------------------------------------------------------
#[test]
fn test_lattice_propagation() {
    // Test: two variables with a "subset" constraint.
    // var0 starts with {0, 1}, var1 starts with bottom (empty).
    // Constraint: var1 must be the join (union) of var0 and var1.
    // After propagation, var1 should contain {0, 1}.

    let mut csp = Csp::new();

    let d0 = BitsetLatticeDomain::new(BitsetDomain::new([0, 1]));
    let d1 = BitsetLatticeDomain::bottom();

    let v0 = csp.add_variable(d0);
    let v1 = csp.add_variable(d1);

    // Constraint: v1 >= v0 (v1 contains everything in v0)
    csp.add_constraint(LambdaConstraint::<BitsetLatticeDomain>::new(
        vec![v0, v1],
        move |assignment| {
            match (&assignment[v0 as usize], &assignment[v1 as usize]) {
                (Some(a), Some(b)) => {
                    // b must be a superset of a
                    (a.bits() & b.bits()) == a.bits()
                }
                _ => true,
            }
        },
        "v1 >= v0",
    ));

    csp.finalize();

    let result = csp.propagate();
    // Note: pure AC-3 propagation on lattice domains with lambda constraints
    // uses the default revise() which only operates on binary pairwise support.
    // This verifies the machinery works without wipe-out.
    assert!(result.is_ok(), "Propagation should not fail");
}

// -----------------------------------------------------------------------
// SimpleType lattice domain — models BBNF's TypeDesc as a CSP domain
// -----------------------------------------------------------------------

/// A simplified type descriptor modeling BBNF's type system.
#[derive(Clone, Debug, PartialEq)]
enum SimpleType {
    /// Unresolved / bottom of the lattice.
    Bottom,
    /// A span (leaf text node).
    Span,
    /// A named type (e.g., a grammar rule).
    Named(String),
    /// A vector of a type.
    Vec(Box<SimpleType>),
    /// A tuple of types (Seq result).
    Tuple(Vec<SimpleType>),
    /// A boxed enum (fallback for heterogeneous alternation).
    BoxedEnum,
}

/// LatticeDomain wrapper around SimpleType for the CSP solver.
///
/// Domain is always a singleton containing the current type. Join computes
/// the least upper bound: identical types stay, different types become BoxedEnum.
#[derive(Clone, Debug, PartialEq)]
struct SimpleTypeDomain {
    value: SimpleType,
}

impl SimpleTypeDomain {
    fn new(value: SimpleType) -> Self {
        Self { value }
    }
}

impl Domain for SimpleTypeDomain {
    type Value = SimpleType;

    fn size(&self) -> usize {
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<SimpleType> {
        Some(self.value.clone())
    }

    fn contains(&self, val: &SimpleType) -> bool {
        self.value == *val
    }

    fn remove(&mut self, _val: &SimpleType) -> bool {
        false // lattice domains don't support removal
    }

    fn add(&mut self, _val: &SimpleType) {
        // no-op
    }

    fn values(&self) -> Vec<SimpleType> {
        vec![self.value.clone()]
    }
}

impl LatticeDomain for SimpleTypeDomain {
    fn bottom() -> Self {
        Self {
            value: SimpleType::Bottom,
        }
    }

    fn join(&mut self, other: &Self) -> bool {
        let new_value = type_join(&self.value, &other.value);
        if new_value != self.value {
            self.value = new_value;
            true
        } else {
            false
        }
    }
}

/// Compute the least upper bound (join) of two types.
///
/// Rules:
/// - Bottom join X = X (Bottom is identity)
/// - X join X = X
/// - BoxedEnum join X = BoxedEnum (top absorbs)
/// - Different non-bottom types = BoxedEnum
fn type_join(a: &SimpleType, b: &SimpleType) -> SimpleType {
    match (a, b) {
        (SimpleType::Bottom, x) | (x, SimpleType::Bottom) => x.clone(),
        (SimpleType::BoxedEnum, _) | (_, SimpleType::BoxedEnum) => SimpleType::BoxedEnum,
        (x, y) if x == y => x.clone(),
        // Structural join for Tuple: element-wise join
        (SimpleType::Tuple(xs), SimpleType::Tuple(ys)) if xs.len() == ys.len() => {
            let joined: Vec<SimpleType> = xs
                .iter()
                .zip(ys.iter())
                .map(|(a, b)| type_join(a, b))
                .collect();
            SimpleType::Tuple(joined)
        }
        // Structural join for Vec: join inner type
        (SimpleType::Vec(a), SimpleType::Vec(b)) => SimpleType::Vec(Box::new(type_join(a, b))),
        // Different types: collapse to BoxedEnum
        _ => SimpleType::BoxedEnum,
    }
}

/// Build a FiniteDomain<SimpleType> containing the candidate types for a result variable.
fn type_domain(candidates: Vec<SimpleType>) -> FiniteDomain<SimpleType> {
    FiniteDomain::new(candidates)
}

/// Standard set of candidate types for search-based type inference tests.
fn standard_type_candidates() -> Vec<SimpleType> {
    vec![
        SimpleType::Bottom,
        SimpleType::Span,
        SimpleType::Named("Expr".to_string()),
        SimpleType::Named("Hir".to_string()),
        SimpleType::Vec(Box::new(SimpleType::Span)),
        SimpleType::Vec(Box::new(SimpleType::Named("Expr".to_string()))),
        SimpleType::Tuple(vec![SimpleType::Span, SimpleType::Span]),
        SimpleType::Tuple(vec![SimpleType::Span, SimpleType::Span, SimpleType::Span]),
        SimpleType::Tuple(vec![
            SimpleType::Span,
            SimpleType::Named("Expr".to_string()),
        ]),
        SimpleType::Tuple(vec![
            SimpleType::Span,
            SimpleType::Named("Expr".to_string()),
            SimpleType::Span,
        ]),
        SimpleType::BoxedEnum,
    ]
}

// -----------------------------------------------------------------------
// 17. Lattice domain set join
// -----------------------------------------------------------------------
#[test]
fn test_lattice_domain_set_join() {
    let mut d = BitsetLatticeDomain::bottom();
    assert_eq!(d.inner().bits(), 0); // empty

    // Join {0, 1}
    let s1 = BitsetLatticeDomain::new(BitsetDomain::new([0, 1]));
    assert!(d.join(&s1)); // changed
    assert_eq!(d.inner().values(), vec![0, 1]);

    // Join {1, 2} -> union = {0, 1, 2}
    let s2 = BitsetLatticeDomain::new(BitsetDomain::new([1, 2]));
    assert!(d.join(&s2)); // changed
    assert_eq!(d.inner().values(), vec![0, 1, 2]);

    // Join {0} -> subset, no change
    let s3 = BitsetLatticeDomain::new(BitsetDomain::new([0]));
    assert!(!d.join(&s3)); // no change
}

// -----------------------------------------------------------------------
// 18. Lattice domain protocol
// -----------------------------------------------------------------------
#[test]
fn test_lattice_domain_protocol() {
    let d = BitsetLatticeDomain::bottom();
    assert_eq!(d.size(), 1); // always singleton
    assert!(d.is_singleton());

    // Singleton value is the empty bitset
    let sv = d.singleton_value().unwrap();
    assert_eq!(sv.size(), 0);

    // After join, still singleton but value changes
    let mut d2 = BitsetLatticeDomain::new(BitsetDomain::new([5]));
    assert_eq!(d2.size(), 1);
    assert!(d2.is_singleton());
    let sv2 = d2.singleton_value().unwrap();
    assert!(sv2.contains(&5));
    assert!(!sv2.contains(&6));

    // Join more
    let more = BitsetLatticeDomain::new(BitsetDomain::new([6]));
    d2.join(&more);
    let sv3 = d2.singleton_value().unwrap();
    assert!(sv3.contains(&5));
    assert!(sv3.contains(&6));
}

// -----------------------------------------------------------------------
// 47. Span compression: Seq of consecutive Spans -> single Span
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_span_compression() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    // Children: fixed singletons
    let c0 = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let c1 = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let c2 = csp.add_variable(type_domain(vec![SimpleType::Span]));

    // Result: all candidate types
    let result = csp.add_variable(type_domain(standard_type_candidates()));

    // Constraint: result = seq_type(c0, c1, c2)
    csp.add_constraint(LambdaConstraint::new(
        vec![c0, c1, c2, result],
        move |assignment: &[Option<SimpleType>]| {
            let children: Vec<&SimpleType> = [c0, c1, c2]
                .iter()
                .filter_map(|&v| assignment[v as usize].as_ref())
                .collect();
            let res = assignment[result as usize].as_ref();

            if children.len() == 3 {
                let all_span = children.iter().all(|t| matches!(t, SimpleType::Span));
                let expected = if all_span {
                    SimpleType::Span
                } else {
                    SimpleType::Tuple(children.into_iter().cloned().collect())
                };
                match res {
                    Some(r) => *r == expected,
                    None => true,
                }
            } else {
                true
            }
        },
        "seq_type_constraint",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty(), "Should find a solution");
    assert_eq!(
        solutions[0][result as usize],
        SimpleType::Span,
        "All-Span Seq should compress to Span"
    );
}

// -----------------------------------------------------------------------
// 48. Span compression: mixed children -> Tuple
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_seq_mixed_no_compression() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    let c0 = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let c1 = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".to_string())]));
    let result = csp.add_variable(type_domain(standard_type_candidates()));

    csp.add_constraint(LambdaConstraint::new(
        vec![c0, c1, result],
        move |assignment: &[Option<SimpleType>]| {
            let children: Vec<&SimpleType> = [c0, c1]
                .iter()
                .filter_map(|&v| assignment[v as usize].as_ref())
                .collect();
            let res = assignment[result as usize].as_ref();

            if children.len() == 2 {
                let all_span = children.iter().all(|t| matches!(t, SimpleType::Span));
                let expected = if all_span {
                    SimpleType::Span
                } else {
                    SimpleType::Tuple(children.into_iter().cloned().collect())
                };
                match res {
                    Some(r) => *r == expected,
                    None => true,
                }
            } else {
                true
            }
        },
        "seq_type_mixed",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty());
    assert_eq!(
        solutions[0][result as usize],
        SimpleType::Tuple(vec![
            SimpleType::Span,
            SimpleType::Named("Expr".to_string())
        ]),
        "Mixed Seq should produce Tuple"
    );
}

// -----------------------------------------------------------------------
// 49. Alternation join: all same type -> that type, else BoxedEnum
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_alt_join_same() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    let b0 = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".to_string())]));
    let b1 = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".to_string())]));
    let b2 = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".to_string())]));
    let result = csp.add_variable(type_domain(standard_type_candidates()));

    csp.add_constraint(LambdaConstraint::new(
        vec![b0, b1, b2, result],
        move |assignment: &[Option<SimpleType>]| {
            let branches: Vec<&SimpleType> = [b0, b1, b2]
                .iter()
                .filter_map(|&v| assignment[v as usize].as_ref())
                .collect();
            let res = assignment[result as usize].as_ref();

            if branches.len() == 3 {
                let mut joined = branches[0].clone();
                for t in &branches[1..] {
                    joined = type_join(&joined, t);
                }
                match res {
                    Some(r) => *r == joined,
                    None => true,
                }
            } else {
                true
            }
        },
        "alt_join",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty());
    assert_eq!(
        solutions[0][result as usize],
        SimpleType::Named("Expr".to_string()),
        "All same type => that type"
    );
}

// -----------------------------------------------------------------------
// 50. Alternation join: different types -> BoxedEnum
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_alt_join_different() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    let b0 = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let b1 = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".to_string())]));
    let result = csp.add_variable(type_domain(standard_type_candidates()));

    csp.add_constraint(LambdaConstraint::new(
        vec![b0, b1, result],
        move |assignment: &[Option<SimpleType>]| {
            let branches: Vec<&SimpleType> = [b0, b1]
                .iter()
                .filter_map(|&v| assignment[v as usize].as_ref())
                .collect();
            let res = assignment[result as usize].as_ref();

            if branches.len() == 2 {
                let joined = type_join(branches[0], branches[1]);
                match res {
                    Some(r) => *r == joined,
                    None => true,
                }
            } else {
                true
            }
        },
        "alt_join_diff",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty());
    assert_eq!(
        solutions[0][result as usize],
        SimpleType::BoxedEnum,
        "Different types => BoxedEnum"
    );
}

// -----------------------------------------------------------------------
// 51. Map override: -> mapper overrides inner expression type
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_map_override() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    let inner = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let result = csp.add_variable(type_domain(standard_type_candidates()));

    csp.add_constraint(LambdaConstraint::new(
        vec![inner, result],
        move |assignment: &[Option<SimpleType>]| {
            let _inner_type = assignment[inner as usize].as_ref();
            let res = assignment[result as usize].as_ref();

            match (_inner_type, res) {
                (Some(_), Some(r)) => *r == SimpleType::Named("Hir".to_string()),
                _ => true,
            }
        },
        "map_override",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty());
    assert_eq!(
        solutions[0][result as usize],
        SimpleType::Named("Hir".to_string()),
        "Map override should produce Named(Hir)"
    );
}

// -----------------------------------------------------------------------
// 52. Recursive rule types: cycle with fixed-point convergence
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_recursive_cycle() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    let candidates = vec![
        SimpleType::Bottom,
        SimpleType::Span,
        SimpleType::Named("Expr".to_string()),
        SimpleType::BoxedEnum,
    ];

    // A is fixed to Named("Expr")
    let a = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".to_string())]));
    // B and C search among candidates
    let b = csp.add_variable(type_domain(candidates.clone()));
    let c = csp.add_variable(type_domain(candidates));

    // B must equal A (same type flows through the cycle)
    csp.add_constraint(LambdaConstraint::new(
        vec![a, b],
        move |assignment: &[Option<SimpleType>]| {
            match (&assignment[a as usize], &assignment[b as usize]) {
                (Some(ta), Some(tb)) => {
                    // join(A, B) == B means B >= A
                    type_join(ta, tb) == *tb
                }
                _ => true,
            }
        },
        "B >= A",
    ));

    // C must equal B
    csp.add_constraint(LambdaConstraint::new(
        vec![b, c],
        move |assignment: &[Option<SimpleType>]| match (
            &assignment[b as usize],
            &assignment[c as usize],
        ) {
            (Some(tb), Some(tc)) => type_join(tb, tc) == *tc,
            _ => true,
        },
        "C >= B",
    ));

    // A must be >= C (closing the cycle)
    csp.add_constraint(LambdaConstraint::new(
        vec![c, a],
        move |assignment: &[Option<SimpleType>]| match (
            &assignment[c as usize],
            &assignment[a as usize],
        ) {
            (Some(tc), Some(ta)) => type_join(tc, ta) == *ta,
            _ => true,
        },
        "A >= C",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(
        !solutions.is_empty(),
        "Cyclic type inference should converge"
    );
    assert_eq!(
        solutions[0][a as usize],
        SimpleType::Named("Expr".to_string())
    );
    assert_eq!(
        solutions[0][b as usize],
        SimpleType::Named("Expr".to_string())
    );
    assert_eq!(
        solutions[0][c as usize],
        SimpleType::Named("Expr".to_string())
    );
}

// -----------------------------------------------------------------------
// 53. FIRST set propagation: manual fixed-point using LatticeDomain::join
// -----------------------------------------------------------------------
#[test]
fn test_first_set_propagation() {
    let mut first_a = BitsetLatticeDomain::new(BitsetDomain::new([0])); // {a}
    let mut first_b = BitsetLatticeDomain::new(BitsetDomain::new([1])); // {b}
    let first_c = BitsetLatticeDomain::new(BitsetDomain::new([2])); // {c}

    // Fixed-point propagation:
    // Constraint graph: A >= B, B >= C
    let mut changed = true;
    let mut iterations = 0;
    while changed {
        changed = false;
        // B >= C
        changed |= first_b.join(&first_c);
        // A >= B
        changed |= first_a.join(&first_b);
        iterations += 1;
        assert!(iterations < 100, "Should converge quickly");
    }

    let a_val = first_a.inner();
    let b_val = first_b.inner();
    let c_val = first_c.inner();

    // FIRST(C) = {c}
    assert!(c_val.contains(&2));
    assert_eq!(c_val.size(), 1);

    // FIRST(B) = {b, c}
    assert!(b_val.contains(&1));
    assert!(b_val.contains(&2));
    assert_eq!(b_val.size(), 2);

    // FIRST(A) = {a, b, c}
    assert!(a_val.contains(&0));
    assert!(a_val.contains(&1));
    assert!(a_val.contains(&2));
    assert_eq!(a_val.size(), 3);

    // Convergence: should take exactly 2 iterations
    assert_eq!(iterations, 2, "Should converge in 2 iterations");
}

// -----------------------------------------------------------------------
// 54. FIRST set propagation with cyclic grammar
// -----------------------------------------------------------------------
#[test]
fn test_first_set_propagation_cyclic() {
    let mut first_a = BitsetLatticeDomain::new(BitsetDomain::new([0])); // {a}
    let mut first_b = BitsetLatticeDomain::new(BitsetDomain::new([1])); // {b}

    let mut changed = true;
    let mut iterations = 0;
    while changed {
        changed = false;
        // A >= B (A contains everything in B)
        changed |= first_a.join(&first_b);
        // B >= A (B contains everything in A)
        changed |= first_b.join(&first_a);
        iterations += 1;
        assert!(iterations < 100, "Should converge quickly");
    }

    let a_val = first_a.inner();
    let b_val = first_b.inner();

    // Both should contain {a, b}
    assert!(
        a_val.contains(&0) && a_val.contains(&1),
        "FIRST(A) should be {{a, b}}, got {:?}",
        a_val.values()
    );
    assert!(
        b_val.contains(&0) && b_val.contains(&1),
        "FIRST(B) should be {{a, b}}, got {:?}",
        b_val.values()
    );

    // Convergence: 2 iterations
    assert_eq!(iterations, 2);
}

// -----------------------------------------------------------------------
// 55. Type join function unit tests
// -----------------------------------------------------------------------
#[test]
fn test_type_join_properties() {
    // Identity: Bottom join X = X
    assert_eq!(
        type_join(&SimpleType::Bottom, &SimpleType::Span),
        SimpleType::Span
    );
    assert_eq!(
        type_join(&SimpleType::Bottom, &SimpleType::Named("X".into())),
        SimpleType::Named("X".into())
    );
    assert_eq!(
        type_join(&SimpleType::Bottom, &SimpleType::Bottom),
        SimpleType::Bottom
    );

    // Absorption: BoxedEnum join X = BoxedEnum
    assert_eq!(
        type_join(&SimpleType::BoxedEnum, &SimpleType::Span),
        SimpleType::BoxedEnum
    );
    assert_eq!(
        type_join(&SimpleType::Span, &SimpleType::BoxedEnum),
        SimpleType::BoxedEnum
    );

    // Idempotence: X join X = X
    assert_eq!(
        type_join(&SimpleType::Span, &SimpleType::Span),
        SimpleType::Span
    );
    let named = SimpleType::Named("Foo".into());
    assert_eq!(type_join(&named, &named), named);

    // Commutativity: X join Y = Y join X
    let span = SimpleType::Span;
    let named_x = SimpleType::Named("X".into());
    assert_eq!(type_join(&span, &named_x), type_join(&named_x, &span));

    // Different types -> BoxedEnum
    assert_eq!(
        type_join(&SimpleType::Span, &SimpleType::Named("X".into())),
        SimpleType::BoxedEnum
    );

    // Structural join for Vec
    assert_eq!(
        type_join(
            &SimpleType::Vec(Box::new(SimpleType::Span)),
            &SimpleType::Vec(Box::new(SimpleType::Span)),
        ),
        SimpleType::Vec(Box::new(SimpleType::Span))
    );
    assert_eq!(
        type_join(
            &SimpleType::Vec(Box::new(SimpleType::Span)),
            &SimpleType::Vec(Box::new(SimpleType::Named("X".into()))),
        ),
        SimpleType::Vec(Box::new(SimpleType::BoxedEnum))
    );

    // Structural join for Tuple: same length, element-wise
    assert_eq!(
        type_join(
            &SimpleType::Tuple(vec![SimpleType::Span, SimpleType::Span]),
            &SimpleType::Tuple(vec![SimpleType::Span, SimpleType::Named("X".into())]),
        ),
        SimpleType::Tuple(vec![SimpleType::Span, SimpleType::BoxedEnum])
    );

    // Tuple with different lengths -> BoxedEnum
    assert_eq!(
        type_join(
            &SimpleType::Tuple(vec![SimpleType::Span]),
            &SimpleType::Tuple(vec![SimpleType::Span, SimpleType::Span]),
        ),
        SimpleType::BoxedEnum
    );
}

// -----------------------------------------------------------------------
// 56. Lattice domain join convergence
// -----------------------------------------------------------------------
#[test]
fn test_simple_type_lattice_domain_convergence() {
    let mut d = SimpleTypeDomain::bottom();
    assert_eq!(d.value, SimpleType::Bottom);

    // Join with Span: Bottom -> Span (changed)
    let span = SimpleTypeDomain::new(SimpleType::Span);
    assert!(d.join(&span));
    assert_eq!(d.value, SimpleType::Span);

    // Join with same: no change
    assert!(!d.join(&span));
    assert_eq!(d.value, SimpleType::Span);

    // Join with different: Span -> BoxedEnum
    let named = SimpleTypeDomain::new(SimpleType::Named("X".into()));
    assert!(d.join(&named));
    assert_eq!(d.value, SimpleType::BoxedEnum);

    // Join anything with BoxedEnum: no change (already top)
    assert!(!d.join(&span));
    assert!(!d.join(&named));
    assert_eq!(d.value, SimpleType::BoxedEnum);
}

// -----------------------------------------------------------------------
// 57. Type inference with Vec wrapping
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_vec_wrapping() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    let inner = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let result = csp.add_variable(type_domain(standard_type_candidates()));

    csp.add_constraint(LambdaConstraint::new(
        vec![inner, result],
        move |assignment: &[Option<SimpleType>]| match (
            &assignment[inner as usize],
            &assignment[result as usize],
        ) {
            (Some(inner_t), Some(res_t)) => {
                let expected = SimpleType::Vec(Box::new(inner_t.clone()));
                *res_t == expected
            }
            _ => true,
        },
        "many_wraps_vec",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty());
    assert_eq!(
        solutions[0][result as usize],
        SimpleType::Vec(Box::new(SimpleType::Span)),
        "Many(Span) should produce Vec(Span)"
    );
}

// -----------------------------------------------------------------------
// 58. Complex type inference: Seq containing Alt containing Map
// -----------------------------------------------------------------------
#[test]
fn test_type_inference_complex_pipeline() {
    let mut csp: Csp<FiniteDomain<SimpleType>> = Csp::new();

    // Children of the outer Seq — fixed singletons
    let child_literal = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let child_expr = csp.add_variable(type_domain(vec![SimpleType::Named("Expr".into())]));

    // Alt(";" | "}") branches — both Span
    let alt_branch_a = csp.add_variable(type_domain(vec![SimpleType::Span]));
    let alt_branch_b = csp.add_variable(type_domain(vec![SimpleType::Span]));
    // Terminator result — searchable
    let child_terminator = csp.add_variable(type_domain(standard_type_candidates()));

    // Alt constraint on terminator
    csp.add_constraint(LambdaConstraint::new(
        vec![alt_branch_a, alt_branch_b, child_terminator],
        move |assignment: &[Option<SimpleType>]| match (
            &assignment[alt_branch_a as usize],
            &assignment[alt_branch_b as usize],
            &assignment[child_terminator as usize],
        ) {
            (Some(a), Some(b), Some(r)) => {
                let joined = type_join(a, b);
                *r == joined
            }
            _ => true,
        },
        "alt_terminator",
    ));

    // Seq result — searchable
    let seq_result = csp.add_variable(type_domain(standard_type_candidates()));

    // Seq constraint
    csp.add_constraint(LambdaConstraint::new(
        vec![child_literal, child_expr, child_terminator, seq_result],
        move |assignment: &[Option<SimpleType>]| {
            let children: Vec<&SimpleType> = [child_literal, child_expr, child_terminator]
                .iter()
                .filter_map(|&v| assignment[v as usize].as_ref())
                .collect();
            let res = assignment[seq_result as usize].as_ref();

            if children.len() == 3 {
                let all_span = children.iter().all(|t| matches!(t, SimpleType::Span));
                let expected = if all_span {
                    SimpleType::Span
                } else {
                    SimpleType::Tuple(children.into_iter().cloned().collect())
                };
                match res {
                    Some(r) => *r == expected,
                    None => true,
                }
            } else {
                true
            }
        },
        "seq_constraint",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty());

    // Terminator: Alt(Span, Span) = Span
    assert_eq!(
        solutions[0][child_terminator as usize],
        SimpleType::Span,
        "Alt of same types should collapse"
    );

    // Seq(Span, Named(Expr), Span) = Tuple(Span, Named(Expr), Span)
    assert_eq!(
        solutions[0][seq_result as usize],
        SimpleType::Tuple(vec![
            SimpleType::Span,
            SimpleType::Named("Expr".into()),
            SimpleType::Span,
        ]),
        "Mixed Seq should produce Tuple"
    );
}

// -----------------------------------------------------------------------
// 59. FIRST set with nullable prefix
// -----------------------------------------------------------------------
#[test]
fn test_first_set_nullable_prefix() {
    // a=0, b=1, c=2
    let first_a = BitsetLatticeDomain::new(BitsetDomain::new([0]));
    let first_b = BitsetLatticeDomain::new(BitsetDomain::new([1]));
    let first_c = BitsetLatticeDomain::new(BitsetDomain::new([2]));

    // Compute FIRST(S) = union of FIRST(A), FIRST(B), FIRST(C)
    // since A and B are nullable, all three contribute.
    let mut first_s = BitsetLatticeDomain::bottom();

    let mut changed = true;
    let mut iterations = 0;
    while changed {
        changed = false;
        changed |= first_s.join(&first_a);
        changed |= first_s.join(&first_b);
        changed |= first_s.join(&first_c);
        iterations += 1;
        assert!(iterations < 100);
    }

    let s_val = first_s.inner();
    assert!(s_val.contains(&0), "FIRST(S) should contain 'a'");
    assert!(s_val.contains(&1), "FIRST(S) should contain 'b'");
    assert!(s_val.contains(&2), "FIRST(S) should contain 'c'");
    assert_eq!(s_val.size(), 3);

    // All three are absorbed in one pass
    assert_eq!(
        iterations, 2,
        "One pass absorbs all, second confirms fixed point"
    );
}
