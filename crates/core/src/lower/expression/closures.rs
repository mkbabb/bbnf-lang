//! Grammar-call beta-reduction + identifier-name resolution.
//!
//! When a grammar closure is applied at a call site, we push a fresh
//! frame on `LowerCtx.env` mapping each parameter name to its argument
//! CST view, lower the closure body in the augmented context, then
//! pop the frame. Identifier resolution checks the env stack first
//! before the rule table, so identifier sites inside the body see the
//! bindings via [`resolve_name`].

use std::collections::HashMap;

use bbnf_ir::IrNode;

use crate::runtime::bbnf::BbnfView;

use super::super::LowerCtx;
use super::lower_rhs;

/// Resolve a bare nonterminal name to an `IrNode`.
///
/// Lookup order:
/// 1. **Beta-reduction environment** — if the name is bound by an enclosing
///    grammar closure application, lower the bound CST view in the current
///    context (which itself sees the same env, supporting nested closures).
/// 2. **Rule table** — emit `IrNode::Ref(rule_id)`.
/// 3. **Recovery fallback** — emit `Epsilon` if `recovery_mode`, else panic.
pub(crate) fn resolve_name<'a>(name: &'a str, ctx: &mut LowerCtx<'a>) -> IrNode {
    if let Some(bound) = lookup_env(name, &ctx.env) {
        return lower_rhs(bound, ctx);
    }
    match ctx.name_to_rule_id.get(name) {
        Some(&rule_id) => IrNode::Ref(rule_id),
        None if ctx.recovery_mode => IrNode::Epsilon,
        None => panic!(
            "unknown nonterminal `{}` — should have been caught by validate_ast()",
            name,
        ),
    }
}

/// Beta-reduction: apply a grammar closure call.
///
/// Pushes a fresh env frame mapping each parameter to its argument CST view,
/// lowers the closure body in the augmented context (so identifier sites
/// inside the body see the bindings via `resolve_name`), then pops the
/// frame. If `name` doesn't refer to a closure, falls back to a normal
/// nonterminal reference.
pub(super) fn lower_grammar_call<'a>(
    name: &'a str,
    args: &[BbnfView<'a, 'a>],
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let Some(closure) = ctx.closures.get(name) else {
        return resolve_name(name, ctx);
    };
    // Snapshot params + body so we can take `&mut ctx` for env push/pop.
    let params: Vec<&'a str> = closure.params.clone();
    let body: BbnfView<'a, 'a> = closure.body;

    assert_eq!(
        args.len(),
        params.len(),
        "arity mismatch: `{}` expects {} args, got {}",
        name,
        params.len(),
        args.len(),
    );

    let mut frame: HashMap<&'a str, BbnfView<'a, 'a>> = HashMap::with_capacity(args.len());
    for (param, arg) in params.iter().zip(args.iter()) {
        frame.insert(*param, *arg);
    }
    ctx.env.push(frame);
    let result = lower_rhs(body, ctx);
    ctx.env.pop();
    result
}

/// Walk the env stack from innermost to outermost, returning the first
/// binding for `name` (if any).
fn lookup_env<'a>(
    name: &str,
    env: &[HashMap<&'a str, BbnfView<'a, 'a>>],
) -> Option<BbnfView<'a, 'a>> {
    for frame in env.iter().rev() {
        if let Some(&bound) = frame.get(name) {
            return Some(bound);
        }
    }
    None
}
