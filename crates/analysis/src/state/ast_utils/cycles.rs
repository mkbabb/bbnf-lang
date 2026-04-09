use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use super::super::types::RuleInfo;

/// Build a representative cycle path string for a rule within its SCC.
///
/// Walks the dependency graph from `start` following only edges within the SCC members,
/// until it returns to `start`, producing a string like "expr -> term -> factor -> expr".
pub fn build_cycle_path(
    start: &str,
    scc_members: &[&str],
    deps: &IndexMap<&str, IndexSet<&str>>,
) -> String {
    let member_set: HashSet<&str> = scc_members.iter().copied().collect();

    let mut path = vec![start];
    let mut visited = HashSet::new();
    visited.insert(start);
    let mut current = start;

    loop {
        let mut found_next = false;
        if let Some(dep_set) = deps.get(current) {
            for dep_name in dep_set {
                if *dep_name == start && path.len() > 1 {
                    // Completed the cycle.
                    path.push(start);
                    return path.join(" \u{2192} ");
                }
                if member_set.contains(dep_name) && !visited.contains(dep_name) {
                    visited.insert(dep_name);
                    path.push(dep_name);
                    current = dep_name;
                    found_next = true;
                    break;
                }
            }
        }
        if !found_next {
            // Couldn't extend the path -- close the cycle back to start.
            path.push(start);
            return path.join(" \u{2192} ");
        }
    }
}

/// Compute the set of rule names reachable from root rules via BFS.
///
/// Root rules are: the first and last rules in the grammar, plus any rule referenced by an import.
pub fn compute_reachable_rules(
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
) -> HashSet<String> {
    let mut reachable = HashSet::new();

    if rules.is_empty() {
        return reachable;
    }

    // The first and last rules are both plausible entry points.
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(rules[0].name.clone());
    reachable.insert(rules[0].name.clone());
    let last = rules.len() - 1;
    if last != 0 {
        queue.push_back(rules[last].name.clone());
        reachable.insert(rules[last].name.clone());
    }

    // BFS from root rules.
    while let Some(current) = queue.pop_front() {
        // Find all rules referenced by `current`.
        if let Some(&idx) = rule_index.get(&current) {
            let rule = &rules[idx];
            for refinfo in &rule.references {
                if !reachable.contains(&refinfo.name) {
                    reachable.insert(refinfo.name.clone());
                    queue.push_back(refinfo.name.clone());
                }
            }
        }
    }

    reachable
}
