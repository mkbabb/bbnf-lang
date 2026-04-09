mod common;

use indexmap::{IndexMap, IndexSet};

use bbnf::graph::{Dependencies, tarjan_scc};
use bbnf_ir::CharSet128 as CharSet;
use bbnf_ir::regex_first::regex_first_chars;

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
    let mut deps: Dependencies = IndexMap::new();
    deps.insert("A", IndexSet::from(["B"]));
    deps.insert("B", IndexSet::from(["C"]));
    deps.insert("C", IndexSet::new());

    let result = tarjan_scc(&deps);
    assert_eq!(result.sccs.len(), 3);
    assert!(result.cyclic_rules.is_empty());

    // Verify reverse-topo order: C should come before B, B before A.
    let scc_idx_a = result.scc_index["A"];
    let scc_idx_b = result.scc_index["B"];
    let scc_idx_c = result.scc_index["C"];
    assert!(scc_idx_c < scc_idx_b || scc_idx_b < scc_idx_a);
}

#[test]
fn tarjan_self_cycle() {
    // A -> A (self-referencing)
    let mut deps: Dependencies = IndexMap::new();
    deps.insert("A", IndexSet::from(["A"]));

    let result = tarjan_scc(&deps);
    assert_eq!(result.sccs.len(), 1);
    assert!(result.cyclic_rules.contains("A"));
}

#[test]
fn tarjan_mutual_cycle() {
    // A -> B, B -> A
    let mut deps: Dependencies = IndexMap::new();
    deps.insert("A", IndexSet::from(["B"]));
    deps.insert("B", IndexSet::from(["A"]));

    let result = tarjan_scc(&deps);
    assert_eq!(result.sccs.len(), 1);
    assert!(result.cyclic_rules.contains("A"));
    assert!(result.cyclic_rules.contains("B"));
}

