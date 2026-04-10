//! Nogood store tests.
//! Extracted from solver/nogoods.rs inline tests.

use csp_solver::solver::nogoods::NogoodStore;

#[test]
fn record_and_detect() {
    let mut store = NogoodStore::<i32>::new(5, 100, 4);

    // Nogood: x0=1, x1=2
    store.record(&[(0, 1), (1, 2)]);
    assert_eq!(store.len(), 1);

    // Assignment has x1=2 already assigned.
    let assignment: Vec<Option<i32>> = vec![None, Some(2), None, None];

    // Assigning x0=1 would complete the nogood.
    assert!(store.is_nogood(0, &1, &assignment));

    // Assigning x0=2 would NOT complete the nogood.
    assert!(!store.is_nogood(0, &2, &assignment));
}

#[test]
fn lru_eviction() {
    let mut store = NogoodStore::<i32>::new(5, 2, 3);

    store.record(&[(0, 1), (1, 2)]); // nogood 0
    store.record(&[(0, 3), (2, 4)]); // nogood 1
    assert_eq!(store.len(), 2);

    // Adding a third evicts the first (LRU).
    store.record(&[(1, 5), (2, 6)]); // nogood 2
    assert_eq!(store.len(), 2);

    // The first nogood (x0=1, x1=2) should be gone.
    let assignment: Vec<Option<i32>> = vec![None, Some(2), None];
    assert!(!store.is_nogood(0, &1, &assignment));

    // The second nogood (x0=3, x2=4) should still be present.
    let assignment2: Vec<Option<i32>> = vec![None, None, Some(4)];
    assert!(store.is_nogood(0, &3, &assignment2));
}

#[test]
fn skip_too_long() {
    let mut store = NogoodStore::<i32>::new(2, 100, 3);

    // This nogood has 3 entries, exceeding max_length=2.
    store.record(&[(0, 1), (1, 2), (2, 3)]);
    assert_eq!(store.len(), 0);
}

#[test]
fn clear_resets() {
    let mut store = NogoodStore::<i32>::new(5, 100, 3);
    store.record(&[(0, 1), (1, 2)]);
    store.record(&[(1, 3), (2, 4)]);
    assert_eq!(store.len(), 2);

    store.clear();
    assert_eq!(store.len(), 0);
    assert!(store.is_empty());

    // Nothing should match after clear.
    let assignment: Vec<Option<i32>> = vec![None, Some(2), None];
    assert!(!store.is_nogood(0, &1, &assignment));
}

#[test]
fn partial_assignment_no_false_positive() {
    let mut store = NogoodStore::<i32>::new(5, 100, 3);

    // Nogood: x0=1, x1=2, x2=3
    store.record(&[(0, 1), (1, 2), (2, 3)]);

    // Only x1 is assigned — x2 is still None, so the nogood is incomplete.
    let assignment: Vec<Option<i32>> = vec![None, Some(2), None];
    assert!(!store.is_nogood(0, &1, &assignment));

    // Now x1=2 and x2=3 are both assigned.
    let assignment2: Vec<Option<i32>> = vec![None, Some(2), Some(3)];
    assert!(store.is_nogood(0, &1, &assignment2));
}
