//! Cross-rule signature dedup: hash every recognizer's
//! `RecognizerSignature.shape_hash` and assign a `peer_group` to nodes
//! whose group has at least `HOIST_THRESHOLD` members.
//!
//! V.4: O(N) average-case via a hash-map probe. The threshold is a
//! cost-arithmetic derived value (size_savings × (group − 1) >
//! call_overhead); for V.4 we use a static default of 3.

use std::collections::HashMap;

use crate::dag::NodeId;
use crate::GrammarIR;

const HOIST_THRESHOLD: usize = 3;

pub(super) fn mine(ir: &mut GrammarIR) {
    // Collect (NodeId, shape_hash) for every recognizer.
    let entries: Vec<(NodeId, u64)> = ir
        .node_facts
        .iter()
        .filter_map(|(nid, facts)| {
            facts
                .recognizer
                .as_ref()
                .map(|r| (*nid, r.signature.shape_hash))
        })
        .collect();

    let mut groups: HashMap<u64, Vec<NodeId>> = HashMap::new();
    for (nid, hash) in &entries {
        groups.entry(*hash).or_default().push(*nid);
    }

    // Assign group ids to peer groups that meet the threshold.
    let mut next_group: u32 = 0;
    let mut group_ids: HashMap<u64, u32> = HashMap::new();
    for (hash, members) in &groups {
        if members.len() >= HOIST_THRESHOLD {
            group_ids.insert(*hash, next_group);
            next_group += 1;
        }
    }

    // Write peer_group back into NodeFacts.recognizer.
    for (nid, hash) in entries {
        if let Some(&gid) = group_ids.get(&hash) {
            if let Some(facts) = ir.node_facts.get_mut(&nid) {
                if let Some(rec) = facts.recognizer.as_mut() {
                    rec.peer_group = Some(gid);
                }
            }
        }
    }
}
