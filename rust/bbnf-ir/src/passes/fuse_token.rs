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

/// Shared mutable context threaded through the recursive factoring pass.
struct FactorCtx<'a> {
    token_rules: &'a [(u32, CharSet128, StringId)],
    rule_bodies: &'a [(u32, IrNode)],
    strings: &'a mut Vec<String>,
    rule_first_sets: &'a [CharSet128],
    new_rules: &'a mut Vec<IrRule>,
    next_id: &'a mut u32,
    rule_names: &'a [(u32, String)],
}

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
            is_scannable && (2..=80).contains(&fs_len)
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
    // Build ID-indexed FIRST set map (rule IDs may be sparse after pruning).
    let max_rule_id = ir.rules.iter().map(|r| r.id).max().unwrap_or(0) as usize;
    let mut rule_first_sets: Vec<CharSet128> = vec![CharSet128::new(); max_rule_id + 1];
    for r in &ir.rules {
        if (r.id as usize) < rule_first_sets.len() {
            rule_first_sets[r.id as usize] = r.meta.first_set.clone();
        }
    }

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
    let mut ctx = FactorCtx {
        token_rules: &token_rules,
        rule_bodies: &rule_bodies,
        strings: &mut ir.strings,
        rule_first_sets: &rule_first_sets,
        new_rules: &mut new_rules,
        next_id: &mut next_id,
        rule_names: &rule_names,
    };
    for rule in &mut rules {
        let new_body = try_factor_alt(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &mut ctx,
        );
        rule.body = new_body;
    }
    ir.rules = rules;

    // Add synthetic continuation rules.
    for new_rule in new_rules {
        ir.rules.push(new_rule);
    }
}

fn try_factor_alt(node: IrNode, ctx: &mut FactorCtx<'_>) -> IrNode {
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
        let branch_first = branch
            .first_set
            .as_ref()
            .or_else(|| {
                // Compute from Ref if missing.
                if let IrNode::Ref(id) = &branch.node {
                    ctx.rule_first_sets.get(*id as usize)
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
    let token_body = ctx.rule_bodies
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

    for &i in &overlap_indices {
        let branch = &branches[i];

        // Get the rule body: follow Ref, or use inline body directly.
        let (ref_rule_id, body) = match &branch.node {
            IrNode::Ref(id) => {
                let b = ctx.rule_bodies.iter().find(|(rid, _)| *rid == *id)?.1.clone();
                (Some(*id), b)
            }
            // Handle inline Seq/OW branches (e.g., after fuse_single_use inlines declarations).
            IrNode::Seq(_) | IrNode::OptionalWhitespace(_) => {
                (None, branch.node.clone())
            }
            _ => continue,
        };

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

    let fused_first = crate::regex_first::regex_first_chars(&ctx.strings[token_regex_sid as usize]);

    // For TokenDispatch, the fallback already handles non-matching keys.
    // The outer Alt only needs: non-overlap branches that weren't folded into
    // the fallback (e.g., branches with non-ident FIRST sets like hex, number)
    // plus the TokenDispatch node itself.
    //
    // For Seq+Alt, same as before: non-overlap branches + fused group.
    let last_overlap_pos = *actual_overlap.last()
        .expect("overlap set must not be empty when building fused token");

    let mut new_branches: Vec<AltBranch> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        if actual_overlap.contains(&i) {
            if i == last_overlap_pos {
                new_branches.push(AltBranch {
                    node: fused,
                    first_set: fused_first.clone(),
                });
                return Some(IrNode::Alt(new_branches_with_rest(
                    new_branches,
                    branches,
                    &actual_overlap,
                    i + 1,
                ), None));
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

/// Check whether an IR node is a keyword pattern (ident-like literals, regex,
/// or a Ref to a rule whose body is a keyword pattern).
fn is_keyword_node(node: &IrNode, strings: &[String], rule_bodies: &[(u32, IrNode)]) -> bool {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            s.bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
        }
        IrNode::Alt(alts, _) => alts.iter().all(|b| is_keyword_node(&b.node, strings, rule_bodies)),
        // Seq of keywords is a keyword (e.g., after prefix factoring:
        // Seq(Literal("b"), Alt(Literal("order-color"), ...)) → "border-color").
        IrNode::Seq(children) => children.iter().all(|c| is_keyword_node(c, strings, rule_bodies)),
        IrNode::Regex(_) => true,
        IrNode::Epsilon => true,
        // Follow Refs: check if the referenced rule body is a keyword pattern.
        IrNode::Ref(id) => {
            if let Some((_, ref_body)) = rule_bodies.iter().find(|(rid, _)| *rid == *id) {
                is_keyword_node(ref_body, strings, rule_bodies)
            } else {
                false
            }
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            is_keyword_node(inner, strings, rule_bodies)
        }
        _ => false,
    }
}

/// Strip the leading keyword (Literal, Alt-of-Literals, Regex, or Ref to keyword
/// rule) from a rule body. Follows Refs to detect keyword patterns in property
/// group rules like `colorProps = "color" | "background-color" | ...`.
fn strip_leading_keyword(
    body: &IrNode,
    strings: &[String],
    rule_bodies: &[(u32, IrNode)],
) -> IrNode {
    // Unwrap OW.
    let inner = match body {
        IrNode::OptionalWhitespace(i) => i.as_ref(),
        other => other,
    };

    match inner {
        IrNode::Seq(children) if children.len() >= 2 => {
            if is_keyword_node(&children[0], strings, rule_bodies) {
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
