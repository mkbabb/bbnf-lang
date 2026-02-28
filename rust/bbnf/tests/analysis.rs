mod common;

use std::collections::{HashMap, HashSet};

use bbnf::analysis::{
    build_dispatch_table, compute_ref_counts, regex_first_chars, tarjan_scc, CharSet,
    Dependencies, FirstSets,
};
use bbnf::types::{Expression, AST};
use indexmap::IndexMap;

use common::{lit, nt};

// -- CharSet tests --

#[test]
fn charset_basic() {
    let mut cs = CharSet::new();
    assert!(cs.is_empty());

    cs.add(b'a');
    assert!(cs.has(b'a'));
    assert!(!cs.has(b'b'));
    assert!(!cs.is_empty());
    assert_eq!(cs.len(), 1);
}

#[test]
fn charset_range() {
    let mut cs = CharSet::new();
    cs.add_range(b'A', b'Z');
    assert_eq!(cs.len(), 26);
    assert!(cs.has(b'A'));
    assert!(cs.has(b'Z'));
    assert!(!cs.has(b'a'));
}

#[test]
fn charset_union_and_disjoint() {
    let mut a = CharSet::new();
    a.add_range(b'a', b'z');

    let mut b = CharSet::new();
    b.add_range(b'0', b'9');

    assert!(a.is_disjoint(&b));

    a.union(&b);
    assert!(!a.is_disjoint(&b));
    assert!(a.has(b'5'));
}

#[test]
fn charset_iter() {
    let mut cs = CharSet::new();
    cs.add(b'x');
    cs.add(b'y');
    cs.add(b'z');
    let collected: Vec<u8> = cs.iter().collect();
    assert_eq!(collected, vec![b'x', b'y', b'z']);
}

#[test]
fn charset_high_codes() {
    let mut cs = CharSet::new();
    cs.add(127);
    assert!(cs.has(127));
    // Code 128 should be silently ignored.
    cs.add(128);
    assert!(!cs.has(128));
}

// -- Regex first chars tests --

#[test]
fn regex_first_literal() {
    let cs = regex_first_chars("abc").unwrap();
    assert!(cs.has(b'a'));
    assert!(!cs.has(b'b')); // only first char
}

#[test]
fn regex_first_digit_escape() {
    let cs = regex_first_chars(r"\d+").unwrap();
    assert!(cs.has(b'0'));
    assert!(cs.has(b'9'));
    assert!(!cs.has(b'a'));
}

#[test]
fn regex_first_char_class() {
    let cs = regex_first_chars("[a-fA-F0-9]").unwrap();
    assert!(cs.has(b'a'));
    assert!(cs.has(b'F'));
    assert!(cs.has(b'0'));
    assert!(!cs.has(b'g'));
}

#[test]
fn regex_first_alternation() {
    let cs = regex_first_chars("abc|def|xyz").unwrap();
    assert!(cs.has(b'a'));
    assert!(cs.has(b'd'));
    assert!(cs.has(b'x'));
    assert!(!cs.has(b'b'));
}

#[test]
fn regex_first_anchor() {
    let cs = regex_first_chars("^abc").unwrap();
    assert!(cs.has(b'a'));
}

#[test]
fn regex_first_dot_returns_none() {
    assert!(regex_first_chars(".+").is_none());
}

#[test]
fn regex_first_group() {
    let cs = regex_first_chars("(?:abc|def)").unwrap();
    assert!(cs.has(b'a'));
    assert!(cs.has(b'd'));
}

#[test]
fn regex_first_word_escape() {
    let cs = regex_first_chars(r"\w+").unwrap();
    assert!(cs.has(b'a'));
    assert!(cs.has(b'Z'));
    assert!(cs.has(b'_'));
    assert!(cs.has(b'5'));
}

#[test]
fn regex_first_space_escape() {
    let cs = regex_first_chars(r"\s").unwrap();
    assert!(cs.has(b' '));
    assert!(cs.has(b'\t'));
    assert!(cs.has(b'\n'));
}

// -- Tarjan SCC tests --

#[test]
fn tarjan_no_cycles() {
    // A -> B -> C (linear chain, no cycles)
    let a = nt("A");
    let b = nt("B");
    let c = nt("C");

    let mut deps: Dependencies = HashMap::new();
    let mut a_deps = HashSet::new();
    a_deps.insert(b.clone());
    deps.insert(a.clone(), a_deps);

    let mut b_deps = HashSet::new();
    b_deps.insert(c.clone());
    deps.insert(b.clone(), b_deps);

    deps.insert(c.clone(), HashSet::new());

    let result = tarjan_scc(&deps);
    assert_eq!(result.sccs.len(), 3);
    assert!(result.cyclic_rules.is_empty());

    // Verify reverse-topo order: C should come before B, B before A.
    let scc_idx_a = result
        .scc_index
        .iter()
        .find(|(k, _)| ***k == a)
        .unwrap()
        .1;
    let scc_idx_b = result
        .scc_index
        .iter()
        .find(|(k, _)| ***k == b)
        .unwrap()
        .1;
    let scc_idx_c = result
        .scc_index
        .iter()
        .find(|(k, _)| ***k == c)
        .unwrap()
        .1;
    assert!(scc_idx_c < scc_idx_b || scc_idx_b < scc_idx_a);
}

#[test]
fn tarjan_self_cycle() {
    // A -> A (self-referencing)
    let a = nt("A");

    let mut deps: Dependencies = HashMap::new();
    let mut a_deps = HashSet::new();
    a_deps.insert(a.clone());
    deps.insert(a.clone(), a_deps);

    let result = tarjan_scc(&deps);
    assert_eq!(result.sccs.len(), 1);
    assert!(result.cyclic_rules.contains(&a));
}

#[test]
fn tarjan_mutual_cycle() {
    // A -> B, B -> A
    let a = nt("A");
    let b = nt("B");

    let mut deps: Dependencies = HashMap::new();
    let mut a_deps = HashSet::new();
    a_deps.insert(b.clone());
    deps.insert(a.clone(), a_deps);

    let mut b_deps = HashSet::new();
    b_deps.insert(a.clone());
    deps.insert(b.clone(), b_deps);

    let result = tarjan_scc(&deps);
    assert_eq!(result.sccs.len(), 1);
    assert!(result.cyclic_rules.contains(&a));
    assert!(result.cyclic_rules.contains(&b));
}

// -- Reference counting tests --

#[test]
fn ref_counts_basic() {
    let a = nt("A");
    let b = nt("B");
    let c = nt("C");

    let mut deps: Dependencies = HashMap::new();
    // A depends on B and C
    let mut a_deps = HashSet::new();
    a_deps.insert(b.clone());
    a_deps.insert(c.clone());
    deps.insert(a.clone(), a_deps);
    // B depends on C
    let mut b_deps = HashSet::new();
    b_deps.insert(c.clone());
    deps.insert(b.clone(), b_deps);
    // C has no dependencies
    deps.insert(c.clone(), HashSet::new());

    let counts = compute_ref_counts(&deps);

    // A is referenced by nobody.
    let a_count = counts.iter().find(|(k, _)| ***k == a).unwrap().1;
    assert_eq!(*a_count, 0);

    // B is referenced by A.
    let b_count = counts.iter().find(|(k, _)| ***k == b).unwrap().1;
    assert_eq!(*b_count, 1);

    // C is referenced by A and B.
    let c_count = counts.iter().find(|(k, _)| ***k == c).unwrap().1;
    assert_eq!(*c_count, 2);
}

// -- Dispatch table tests --

#[test]
fn dispatch_table_basic() {
    // Simulate two literal alternatives: "abc" and "xyz".
    let alt_a = lit("abc");
    let alt_x = lit("xyz");

    let ast: AST = IndexMap::new();
    let first_sets = FirstSets {
        first: HashMap::new(),
        nullable: HashSet::new(),
        branch_firsts: HashMap::new(),
    };

    let alternatives: Vec<&Expression> = vec![&alt_a, &alt_x];
    let table = build_dispatch_table(&alternatives, &first_sets, &ast).unwrap();

    assert_eq!(table.lookup(b'a'), Some(0));
    assert_eq!(table.lookup(b'x'), Some(1));
    assert_eq!(table.lookup(b'z'), None);
}

#[test]
fn dispatch_table_overlapping_returns_none() {
    // Two literals that start with the same character.
    let alt1 = lit("abc");
    let alt2 = lit("axyz");

    let ast: AST = IndexMap::new();
    let first_sets = FirstSets {
        first: HashMap::new(),
        nullable: HashSet::new(),
        branch_firsts: HashMap::new(),
    };

    let alternatives: Vec<&Expression> = vec![&alt1, &alt2];
    assert!(build_dispatch_table(&alternatives, &first_sets, &ast).is_none());
}
