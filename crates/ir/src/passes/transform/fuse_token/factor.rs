//! Recursive factoring logic: walk a rule body, find Alt nodes whose
//! branches share an overlapping FIRST set with one of the configured
//! `@token` rules, and rewrite them into `Seq(token_regex, Alt(continuations))`.

use std::collections::HashSet;

use crate::regex_first;
use crate::{AltBranch, CharSet128, IrNode, IrRule, RuleMeta, StringId};

use super::detect::{leading_first_set, starts_with_different_token, strip_leading_keyword};

/// Shared mutable context threaded through the recursive factoring pass.
pub(super) struct FactorCtx<'a> {
    pub(super) token_rules: &'a [(u32, CharSet128, StringId)],
    pub(super) rule_bodies: &'a [(u32, IrNode)],
    pub(super) strings: &'a mut Vec<String>,
    pub(super) rule_first_sets: &'a [CharSet128],
    pub(super) new_rules: &'a mut Vec<IrRule>,
    pub(super) next_id: &'a mut u32,
    pub(super) rule_names: &'a [(u32, String)],
}

pub(super) fn try_factor_alt(node: IrNode, ctx: &mut FactorCtx<'_>) -> IrNode {
    match node {
        IrNode::Alt(branches, dispatch) if dispatch.is_none() && branches.len() >= 4 => {
            // Recurse into branch bodies first.
            let branches: Vec<AltBranch> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = try_factor_alt(b.node, ctx);
                    b
                })
                .collect();

            // Try to factor using each @token rule.
            for (token_rule_id, token_first, token_name_sid) in ctx.token_rules {
                if let Some(factored) = factor_with_token(
                    &branches,
                    *token_rule_id,
                    token_first,
                    *token_name_sid,
                    ctx,
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
                .map(|c| try_factor_alt(c, ctx))
                .collect(),
        ),
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(try_factor_alt(*inner, ctx)),
            lo,
            hi,
        },
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(try_factor_alt(*inner, ctx)),
            fn_id,
        },
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(try_factor_alt(*inner, ctx)))
        }
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(try_factor_alt(*a, ctx)),
            Box::new(try_factor_alt(*b, ctx)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(try_factor_alt(*a, ctx)),
            Box::new(try_factor_alt(*b, ctx)),
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
    ctx: &mut FactorCtx<'_>,
) -> Option<IrNode> {
    // Find branches whose FIRST set overlaps with the token's FIRST set.
    let mut overlap_indices: Vec<usize> = Vec::new();
    let mut non_overlap_indices: Vec<usize> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        let branch_first = branch.first_set.as_ref().or_else(|| {
            // Compute from Ref if missing.
            if let IrNode::Ref(id) = &branch.node {
                ctx.rule_first_sets.get(*id as usize)
            } else {
                None
            }
        });

        if let Some(fs) = branch_first {
            // The branch's FIRST set must be a subset of the token's FIRST set.
            // If the branch accepts characters the token can't scan (e.g.,
            // genericDecl accepts `-` but ident doesn't), factoring under
            // the token would silently drop those inputs.
            if fs.is_subset(token_first) && !fs.is_empty() {
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
    let token_body = ctx
        .rule_bodies
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
    struct ContinuationInfo {
        branch_idx: usize,
        cont_ref: IrNode,
        cont_first: Option<CharSet128>,
    }

    let mut continuations: Vec<ContinuationInfo> = Vec::new();

    // Collect all @token rule IDs so we can detect branches that start with a
    // different token rule than the one being used for factoring.
    let token_ids: HashSet<u32> = ctx.token_rules.iter().map(|(id, _, _)| *id).collect();

    for &i in &overlap_indices {
        let branch = &branches[i];

        // Get the rule body: follow Ref, or use inline body directly.
        let (ref_rule_id, body) = match &branch.node {
            IrNode::Ref(id) => {
                let b = ctx
                    .rule_bodies
                    .iter()
                    .find(|(rid, _)| *rid == *id)?
                    .1
                    .clone();
                (Some(*id), b)
            }
            // Handle inline Seq/OW branches (e.g., after fuse_single_use inlines declarations).
            IrNode::Seq(_) | IrNode::OptionalWhitespace(_) => (None, branch.node.clone()),
            _ => continue,
        };

        // Reject branches that start with a different @token rule — their scanning
        // semantics differ from the factoring token. E.g., `selectorSpan` uses
        // `[^{};]+` while `propertyName` uses `[a-zA-Z-]+`; folding `selectorSpan`
        // branches under `propertyName`'s regex replaces the scanner incorrectly.
        if starts_with_different_token(&body, token_rule_id, &token_ids, ctx.rule_bodies) {
            non_overlap_indices.push(i);
            continue;
        }

        // Strip the leading keyword from the body (follows Refs to detect
        // property group rules like colorProps = "color" | "background-color" | ...).
        let continuation_body = strip_leading_keyword(&body, ctx.strings, ctx.rule_bodies);

        // Skip keyword-only branches (continuation is Epsilon) — they need
        // the pre-scanned token as their result, which the Seq can't provide.
        // Leave them in the outer Alt for the flat chain to handle.
        if matches!(continuation_body, IrNode::Epsilon) {
            non_overlap_indices.push(i);
            continue;
        }

        // Skip branches where strip_leading_keyword couldn't strip anything —
        // the continuation still expects the leading token (e.g., Ref to a property
        // group rule that isn't a simple keyword). Including these would cause the
        // fused Seq to double-consume the prefix.
        if continuation_body == body {
            non_overlap_indices.push(i);
            continue;
        }

        let cont_first = leading_first_set(&continuation_body, ctx.rule_first_sets, ctx.strings);

        // Create a synthetic continuation rule.
        let cont_rule_id = *ctx.next_id;
        *ctx.next_id += 1;

        let orig_name = ref_rule_id
            .and_then(|rid| ctx.rule_names.iter().find(|(id, _)| *id == rid))
            .map(|(_, n)| n.as_str())
            .unwrap_or("branch");
        let cont_name = format!("__{}_cont_{}", orig_name, cont_rule_id);
        let cont_name_sid = ctx.strings.len() as u32;
        ctx.strings.push(cont_name);

        let cont_meta = RuleMeta {
            first_set: cont_first.clone().unwrap_or_default(),
            ..Default::default()
        };

        ctx.new_rules.push(IrRule {
            id: cont_rule_id,
            name: cont_name_sid,
            body: continuation_body,
            meta: cont_meta,
            source_span: None,
        });

        continuations.push(ContinuationInfo {
            branch_idx: i,
            cont_ref: IrNode::Ref(cont_rule_id),
            cont_first,
        });
    }

    // Exclude continuations with overlapping FIRST sets. When two continuations
    // both start with the same byte (e.g., `(` for both urlFunction and
    // genericFunction), the factored Alt can't dispatch correctly since the
    // keyword has been stripped. Remove ALL overlapping continuations from the
    // factored group — they stay in the outer Alt with their original keyword check.
    // `None` FIRST means unknown/nullable — conservatively treat as overlapping.
    {
        let mut to_remove = vec![false; continuations.len()];
        for i in 0..continuations.len() {
            for j in i + 1..continuations.len() {
                let dominated = match (&continuations[i].cont_first, &continuations[j].cont_first) {
                    (Some(fi), Some(fj)) => !fi.is_disjoint(fj),
                    (None, _) | (_, None) => true,
                };
                if dominated {
                    to_remove[i] = true;
                    to_remove[j] = true;
                }
            }
        }
        let mut idx = 0;
        continuations.retain(|c| {
            let keep = !to_remove[idx];
            if !keep {
                non_overlap_indices.push(c.branch_idx);
            }
            idx += 1;
            keep
        });
    }

    let actual_overlap: Vec<usize> = continuations.iter().map(|c| c.branch_idx).collect();

    // Need at least 3 actual continuation branches.
    if actual_overlap.len() < 3 {
        return None;
    }

    // Always produce Seq+Alt. The dispatch table pass handles disjoint FIRST
    // sets. For shared FIRST sets (key-value patterns like CSS declarations),
    // the codegen-level ident_dispatch is more efficient than IR-level
    // TokenDispatch (avoids function call overhead from synthetic continuation rules).
    let fused = {
        let continuation_branches: Vec<AltBranch> = continuations
            .iter()
            .map(|c| AltBranch {
                node: c.cont_ref.clone(),
                first_set: c.cont_first.clone(),
            })
            .collect();
        IrNode::Seq(vec![
            IrNode::Regex(token_regex_sid),
            IrNode::Alt(continuation_branches, None),
        ])
    };

    let fused_first = regex_first::regex_first_chars(&ctx.strings[token_regex_sid as usize]);

    // For TokenDispatch, the fallback already handles non-matching keys.
    // The outer Alt only needs: non-overlap branches that weren't folded into
    // the fallback (e.g., branches with non-ident FIRST sets like hex, number)
    // plus the TokenDispatch node itself.
    //
    // For Seq+Alt, same as before: non-overlap branches + fused group.
    let last_overlap_pos = *actual_overlap
        .last()
        .expect("overlap set must not be empty when building fused token");

    let mut new_branches: Vec<AltBranch> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        if actual_overlap.contains(&i) {
            if i == last_overlap_pos {
                new_branches.push(AltBranch {
                    node: fused,
                    first_set: fused_first.clone(),
                });
                return Some(IrNode::Alt(
                    new_branches_with_rest(new_branches, branches, &actual_overlap, i + 1),
                    None,
                ));
            }
            // Skip individual overlapping branches.
        } else {
            new_branches.push(branch.clone());
        }
    }

    // Fallback: fused node not yet inserted (last_overlap_pos was not reached).
    new_branches.push(AltBranch {
        node: fused,
        first_set: fused_first,
    });

    Some(IrNode::Alt(new_branches, None))
}

fn new_branches_with_rest(
    mut new_branches: Vec<AltBranch>,
    branches: &[AltBranch],
    overlap_indices: &[usize],
    from: usize,
) -> Vec<AltBranch> {
    for (i, branch) in branches.iter().enumerate().skip(from) {
        if !overlap_indices.contains(&i) {
            new_branches.push(branch.clone());
        }
    }
    new_branches
}
