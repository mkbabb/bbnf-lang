use std::collections::HashMap;

use bbnf::graph::{Dependencies, SccResult, tarjan_scc};
use ls_types::*;

use crate::analysis::LineIndex;

use super::super::ast_utils::build_cycle_path;
use super::super::types::RuleInfo;

/// Detect left recursion and cycles via Tarjan SCC over the supplied
/// dependency graph. Emits INFORMATION diagnostics and returns the
/// `cyclic_rule_paths` map together with the full `SccResult` (the caller
/// must keep `deps` alive for the SCC result's lifetime).
pub(super) fn detect_cycles<'a>(
    deps: &'a Dependencies<'a>,
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) -> (HashMap<String, String>, SccResult<'a>) {
    let scc = tarjan_scc(deps);

    let mut cyclic_rule_paths = HashMap::new();
    for scc_group in &scc.sccs {
        // An SCC with >1 member means all members are cyclic.
        // An SCC with 1 member is cyclic only if it self-references (checked via cyclic_rules).
        let is_multi = scc_group.len() > 1;
        let cyclic_members: Vec<&str> = scc_group
            .iter()
            .filter(|&&name| {
                if is_multi {
                    true
                } else {
                    scc.cyclic_rules.contains(name)
                }
            })
            .copied()
            .collect();

        if cyclic_members.is_empty() {
            continue;
        }

        for &member in &cyclic_members {
            let path = if cyclic_members.len() == 1 {
                // Self-recursive rule.
                format!("{} \u{2192} {}", member, member)
            } else {
                // Multi-member SCC -- reconstruct a representative cycle path.
                build_cycle_path(member, &cyclic_members, deps)
            };

            cyclic_rule_paths.insert(member.to_string(), path.clone());

            if let Some(&idx) = rule_index.get(member) {
                let rule = &rules[idx];
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    source: Some(crate::DIAGNOSTIC_SOURCE.into()),
                    message: format!("Rule `{}` participates in a cycle: {}", member, path),
                    ..Default::default()
                });
            }
        }
    }

    (cyclic_rule_paths, scc)
}
