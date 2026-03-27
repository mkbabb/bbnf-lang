//! Pass: @token-guided prefix factoring.
//!
//! For Alts where multiple branches start with expressions whose FIRST sets
//! overlap with a `@token` rule's character class, factors the shared token
//! prefix into a Seq: `Seq(token_regex, Alt(continuations))`.
//!
//! The continuations are synthetic rules created by stripping the leading
//! keyword from each branch's rule body. The continuation Alt typically has
//! disjoint FIRST sets (e.g., `(` for functions vs Epsilon for bare idents),
//! enabling dispatch by the subsequent `generate_dispatch_tables` pass.
//!
//! This is grammar-agnostic: any grammar with overlapping-FIRST Alts and
//! `@token` declarations benefits.

use crate::{AltBranch, CharSet128, GrammarIR, IrNode, IrRule, RuleMeta, StringId};

/// Factor overlapping-FIRST Alts using @token hints.
pub fn fuse_token_dispatch(ir: &mut GrammarIR) {
    // Find @token rules suitable for prefix factoring.
    // Criteria: FIRST set has moderate cardinality (2-80 bytes) — narrow enough
    // to be selective, broad enough to cover multiple branches.
    // Too narrow (1 byte) = dispatch already handles it.
    // Too broad (>80 bytes) = matches everything, factoring adds overhead.
    let token_rules: Vec<(u32, CharSet128, StringId)> = ir
        .rules
        .iter()
        .filter(|r| {
            if !r.meta.is_token {
                return false;
            }
            let fs_len = r.meta.first_set.len();
            // Must be a scannable token (Regex or Alt-of-Literals after merge)
            // with moderate FIRST set.
            let is_scannable = matches!(&r.body, IrNode::Regex(_))
                || matches!(&r.body, IrNode::Alt(branches, _) if branches.iter().all(|b| matches!(&b.node, IrNode::Literal(_))));
            is_scannable && fs_len >= 2 && fs_len <= 80
        })
        .map(|r| (r.id, r.meta.first_set.clone(), r.name))
        .collect();

    if token_rules.is_empty() {
        return;
    }

    // Snapshot rule bodies for analysis.
    let rule_bodies: Vec<(u32, IrNode)> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.body.clone()))
        .collect();
    let rule_first_sets: Vec<CharSet128> = ir
        .rules
        .iter()
        .map(|r| r.meta.first_set.clone())
        .collect();

    // Collect synthetic continuation rules to add.
    let mut new_rules: Vec<IrRule> = Vec::new();
    let mut next_id = ir.rules.iter().map(|r| r.id).max().unwrap_or(0) + 1;

    // Snapshot rule name→id mapping for lookups.
    let rule_names: Vec<(u32, String)> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    // Process each rule body. Take rules out temporarily to split the borrow
    // on `ir` (we need `&mut ir.strings` alongside `&mut rules`).
    let mut rules = std::mem::take(&mut ir.rules);
    for rule in &mut rules {
        let new_body = try_factor_alt(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &token_rules,
            &rule_bodies,
            &mut ir.strings,
            &rule_first_sets,
            &mut new_rules,
            &mut next_id,
            &rule_names,
        );
        rule.body = new_body;
    }
    ir.rules = rules;

    // Add synthetic continuation rules.
    for new_rule in new_rules {
        ir.rules.push(new_rule);
    }
}

fn try_factor_alt(
    node: IrNode,
    token_rules: &[(u32, CharSet128, StringId)],
    rule_bodies: &[(u32, IrNode)],
    strings: &mut Vec<String>,
    rule_first_sets: &[CharSet128],
    new_rules: &mut Vec<IrRule>,
    next_id: &mut u32,
    rule_names: &[(u32, String)],
) -> IrNode {
    match node {
        IrNode::Alt(branches, dispatch) if dispatch.is_none() && branches.len() >= 4 => {
            // Recurse into branch bodies first.
            let branches: Vec<AltBranch> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = try_factor_alt(
                        b.node,
                        token_rules,
                        rule_bodies,
                        strings,
                        rule_first_sets,
                        new_rules,
                        next_id,
                        rule_names,
                    );
                    b
                })
                .collect();

            // Try to factor using each @token rule.
            for (token_rule_id, token_first, token_name_sid) in token_rules {
                if let Some(factored) = factor_with_token(
                    &branches,
                    *token_rule_id,
                    token_first,
                    *token_name_sid,
                    rule_bodies,
                    strings,
                    rule_first_sets,
                    new_rules,
                    next_id,
                    rule_names,
                ) {
                    return factored;
                }
            }

            IrNode::Alt(branches, None)
        }
        // Recurse into other node types.
        IrNode::Seq(children) => IrNode::Seq(
            children
                .into_iter()
                .map(|c| {
                    try_factor_alt(
                        c,
                        token_rules,
                        rule_bodies,
                        strings,
                        rule_first_sets,
                        new_rules,
                        next_id,
                        rule_names,
                    )
                })
                .collect(),
        ),
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(try_factor_alt(
                *inner,
                token_rules,
                rule_bodies,
                strings,
                rule_first_sets,
                new_rules,
                next_id,
                rule_names,
            )),
            lo,
            hi,
        },
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(try_factor_alt(
                *inner,
                token_rules,
                rule_bodies,
                strings,
                rule_first_sets,
                new_rules,
                next_id,
                rule_names,
            )),
            fn_id,
        },
        IrNode::OptionalWhitespace(inner) => IrNode::OptionalWhitespace(Box::new(
            try_factor_alt(
                *inner,
                token_rules,
                rule_bodies,
                strings,
                rule_first_sets,
                new_rules,
                next_id,
                rule_names,
            ),
        )),
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(try_factor_alt(*a, token_rules, rule_bodies, strings, rule_first_sets, new_rules, next_id, rule_names)),
            Box::new(try_factor_alt(*b, token_rules, rule_bodies, strings, rule_first_sets, new_rules, next_id, rule_names)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(try_factor_alt(*a, token_rules, rule_bodies, strings, rule_first_sets, new_rules, next_id, rule_names)),
            Box::new(try_factor_alt(*b, token_rules, rule_bodies, strings, rule_first_sets, new_rules, next_id, rule_names)),
        ),
        other => other,
    }
}

/// Try to factor an Alt using a specific @token rule.
fn factor_with_token(
    branches: &[AltBranch],
    token_rule_id: u32,
    token_first: &CharSet128,
    _token_name_sid: StringId,
    rule_bodies: &[(u32, IrNode)],
    strings: &mut Vec<String>,
    rule_first_sets: &[CharSet128],
    new_rules: &mut Vec<IrRule>,
    next_id: &mut u32,
    rule_names: &[(u32, String)],
) -> Option<IrNode> {
    // Find branches whose FIRST set overlaps with the token's FIRST set.
    let mut overlap_indices: Vec<usize> = Vec::new();
    let mut non_overlap_indices: Vec<usize> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        let branch_first = branch
            .first_set
            .as_ref()
            .or_else(|| {
                // Compute from Ref if missing.
                if let IrNode::Ref(id) = &branch.node {
                    rule_first_sets.get(*id as usize)
                } else {
                    None
                }
            });

        if let Some(fs) = branch_first {
            let intersection_count = fs.intersection(token_first).len();
            let branch_count = fs.len();
            // Require at least 50% of the branch's FIRST set to overlap with the
            // token. Branches that share only 1-2 marginal characters (like `-`)
            // stay in the outer Alt — they start with distinct character classes.
            if intersection_count > 0
                && branch_count > 0
                && intersection_count * 2 >= branch_count
            {
                overlap_indices.push(i);
            } else {
                non_overlap_indices.push(i);
            }
        } else {
            non_overlap_indices.push(i);
        }
    }

    // Need at least 3 overlapping branches to justify factoring
    // (checked after continuation building filters out keyword-only branches).
    if overlap_indices.len() < 3 {
        return None;
    }

    // Find the token rule's regex (the shared prefix scanner).
    let token_body = rule_bodies
        .iter()
        .find(|(id, _)| *id == token_rule_id)?
        .1
        .clone();
    let token_regex_sid = match &token_body {
        IrNode::Regex(sid) => *sid,
        _ => return None, // Token rule must be a regex for prefix factoring.
    };

    // Build continuations for each overlapping branch.
    // Branches with keyword-only bodies (Epsilon continuation) get moved back
    // to non_overlap (they need the pre-scanned token as their result).
    let mut continuation_branches: Vec<AltBranch> = Vec::new();
    let mut actual_overlap: Vec<usize> = Vec::new();

    for &i in &overlap_indices {
        let branch = &branches[i];

        // Get the rule body (follow Ref).
        let (ref_rule_id, body) = match &branch.node {
            IrNode::Ref(id) => {
                let b = rule_bodies.iter().find(|(rid, _)| *rid == *id)?.1.clone();
                (*id, b)
            }
            _ => continue, // Can't factor non-Ref branches (bare expressions).
        };

        // Strip the leading keyword from the body.
        let continuation_body = strip_leading_keyword(&body, strings);

        // Skip keyword-only branches (continuation is Epsilon) — they need
        // the pre-scanned token as their result, which the Seq can't provide.
        // Leave them in the outer Alt for the flat chain to handle.
        if matches!(continuation_body, IrNode::Epsilon) {
            non_overlap_indices.push(i);
            continue;
        }

        let cont_first = leading_first_set(&continuation_body, rule_first_sets, strings);

        // Create a synthetic continuation rule instead of inlining the body.
        // This ensures the continuation always infers to BoxedEnum, avoiding
        // tuple-type coercion failures for complex Seq bodies from imports.
        let cont_rule_id = *next_id;
        *next_id += 1;

        let orig_name = rule_names
            .iter()
            .find(|(id, _)| *id == ref_rule_id)
            .map(|(_, n)| n.as_str())
            .unwrap_or("unknown");
        let cont_name = format!("__{}_cont_{}", orig_name, cont_rule_id);
        let cont_name_sid = strings.len() as u32;
        strings.push(cont_name);

        let mut cont_meta = RuleMeta::default();
        cont_meta.first_set = cont_first.clone().unwrap_or_default();

        new_rules.push(IrRule {
            id: cont_rule_id,
            name: cont_name_sid,
            body: continuation_body,
            meta: cont_meta,
            source_span: None,
        });

        // Use Ref to the synthetic rule in the Alt, not the inline body.
        continuation_branches.push(AltBranch {
            node: IrNode::Ref(cont_rule_id),
            first_set: cont_first,
        });
        actual_overlap.push(i);
    }

    // Need at least 3 actual continuation branches.
    if actual_overlap.len() < 3 {
        return None;
    }

    // Build the fused group: Seq(token_regex, Alt(continuations))
    let fused = IrNode::Seq(vec![
        IrNode::Regex(token_regex_sid),
        IrNode::Alt(continuation_branches, None),
    ]);

    let fused_first = crate::regex_first::regex_first_chars(&strings[token_regex_sid as usize]);

    // Rebuild the outer Alt: non-overlapping branches + fused group.
    let last_overlap_pos = *actual_overlap.last().unwrap();

    let mut new_branches: Vec<AltBranch> = Vec::new();
    let mut fused_inserted = false;

    for i in 0..branches.len() {
        if actual_overlap.contains(&i) {
            if !fused_inserted && i == last_overlap_pos {
                new_branches.push(AltBranch {
                    node: fused,
                    first_set: fused_first.clone(),
                });
                fused_inserted = true;
                return Some(IrNode::Alt(new_branches_with_rest(
                    new_branches,
                    branches,
                    &actual_overlap,
                    i + 1,
                    fused_inserted,
                ), None));
            }
            // Skip individual overlapping branches.
        } else {
            new_branches.push(branches[i].clone());
        }
    }

    if !fused_inserted {
        new_branches.push(AltBranch {
            node: fused,
            first_set: fused_first,
        });
    }

    Some(IrNode::Alt(new_branches, None))
}

fn new_branches_with_rest(
    mut new_branches: Vec<AltBranch>,
    branches: &[AltBranch],
    overlap_indices: &[usize],
    from: usize,
    _fused_inserted: bool,
) -> Vec<AltBranch> {
    for i in from..branches.len() {
        if !overlap_indices.contains(&i) {
            new_branches.push(branches[i].clone());
        }
    }
    new_branches
}

/// Strip the leading keyword (Literal or Alt-of-Literals) from a rule body.
fn strip_leading_keyword(body: &IrNode, strings: &[String]) -> IrNode {
    // Unwrap OW.
    let inner = match body {
        IrNode::OptionalWhitespace(i) => i.as_ref(),
        other => other,
    };

    match inner {
        IrNode::Seq(children) if children.len() >= 2 => {
            let first = &children[0];
            let is_keyword = match first {
                IrNode::Literal(sid) => {
                    let s = &strings[*sid as usize];
                    s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
                }
                IrNode::Alt(alts, _) => alts.iter().all(|b| {
                    matches!(&b.node, IrNode::Literal(sid) if {
                        let s = &strings[*sid as usize];
                        s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
                    })
                }),
                IrNode::Regex(_) => true, // Regex prefix (genericFunction)
                _ => false,
            };

            if is_keyword {
                if children.len() == 2 {
                    children[1].clone()
                } else {
                    IrNode::Seq(children[1..].to_vec())
                }
            } else {
                body.clone()
            }
        }
        // Body is a single Regex or Literal → continuation is Epsilon.
        IrNode::Regex(_) | IrNode::Literal(_) => IrNode::Epsilon,
        // Body is an Alt of Literals → continuation is Epsilon.
        IrNode::Alt(alts, _)
            if alts
                .iter()
                .all(|b| matches!(&b.node, IrNode::Literal(_))) =>
        {
            IrNode::Epsilon
        }
        _ => body.clone(),
    }
}

/// Compute FIRST set for a continuation node.
fn leading_first_set(
    node: &IrNode,
    rule_first_sets: &[CharSet128],
    strings: &[String],
) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            s.bytes().next().map(|b| {
                let mut cs = CharSet128::new();
                cs.add(b);
                cs
            })
        }
        IrNode::Regex(sid) => {
            crate::regex_first::regex_first_chars(&strings[*sid as usize])
        }
        IrNode::Ref(id) => {
            rule_first_sets.get(*id as usize).cloned()
        }
        IrNode::Seq(children) if !children.is_empty() => {
            leading_first_set(&children[0], rule_first_sets, strings)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            leading_first_set(inner, rule_first_sets, strings)
        }
        IrNode::Epsilon => None, // Nullable.
        _ => None,
    }
}
