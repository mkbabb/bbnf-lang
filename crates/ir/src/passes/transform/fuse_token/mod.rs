//! Pass: `@token`-guided prefix factoring.
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
//!
//! Sub-modules:
//!
//! - [`detect`] — `is_keyword_node`, `starts_with_different_token`,
//!   `leading_first_set`, `strip_leading_keyword` — pure structural
//!   recognizers.
//! - [`factor`] — `factor_with_token`, `try_factor_alt`, `FactorCtx`,
//!   `new_branches_with_rest` — the recursive factoring logic.
//!
//! `mod.rs` owns the public entry point `fuse_token_dispatch`, which
//! collects suitable `@token` rules, builds the shared `FactorCtx`, and
//! drives the rewrite over every rule body.

mod detect;
mod factor;

use crate::{CharSet128, GrammarIR, IrNode, IrRule, StringId};

use self::factor::{FactorCtx, try_factor_alt};

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
            if !r.meta.directives.token {
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
    let rule_bodies: Vec<(u32, IrNode)> = ir.rules.iter().map(|r| (r.id, r.body.clone())).collect();
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
        let old_body = std::mem::replace(&mut rule.body, IrNode::Epsilon);
        let new_body = try_factor_alt(old_body, &mut ctx);
        rule.body = new_body;
    }
    ir.rules = rules;

    // Add synthetic continuation rules.
    for new_rule in new_rules {
        ir.rules.push(new_rule);
    }
}
