use ir::{BackendIr, BackendRule, BackendShape, CostFacts, RuleId};
use passes::diagnostics::PassDiagnostic;
use std::collections::HashMap;

use super::{select_lowering, sink_only};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoweredRust {
    pub rule_plans: Vec<RuleLoweringPlan>,
    pub sink_only_program: Option<sink_only::SinkOnlyProgram>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuleLoweringPlan {
    pub rule: String,
    pub shape: BackendShape,
    pub body: String,
}

#[derive(Clone, Debug)]
pub struct LowerCtx<'a> {
    pub backend_shape: &'a HashMap<RuleId, BackendShape>,
    pub cost_facts: &'a HashMap<RuleId, CostFacts>,
    pub diagnostics: &'a [PassDiagnostic],
}

pub fn lower_to_rust(backend: &BackendIr, ctx: &LowerCtx<'_>) -> Result<LoweredRust, String> {
    let _diagnostics_seen = ctx.diagnostics.len();
    let mut rule_plans = Vec::with_capacity(backend.rules.len());
    for (index, rule) in backend.rules.iter().enumerate() {
        let rule_id = RuleId(index);
        let shape = shape_for(ctx, rule_id)?;
        match ctx.cost_facts.get(&rule_id) {
            Some(cost) => {
                if shape != cost.chosen {
                    return Err(format!(
                            "W7 fail-closed: backend shape {:?} disagrees with cost facts {:?} for rule {}",
                            shape, cost.chosen, rule.name
                        ));
                }
                if cost.active_cost.is_none() {
                    return Err(format!(
                        "W7 fail-closed: missing active-cost facts for rule {}",
                        rule.name
                    ));
                }
                match cost.decision_csp.as_ref() {
                    Some(csp)
                        if csp.csp_status == "sat"
                            && csp.csp_budget_status == "pass"
                            && csp.selected_rule_count > 0
                            && csp.csp_solve_us <= csp.csp_timeout_ms.saturating_mul(1_000) => {}
                    Some(csp) => {
                        return Err(format!(
                            "W7 fail-closed: decision-CSP status {} budget {} selected {} for rule {}",
                            csp.csp_status,
                            csp.csp_budget_status,
                            csp.selected_rule_count,
                            rule.name
                        ));
                    }
                    None => {
                        return Err(format!(
                            "W7 fail-closed: missing decision-CSP facts for rule {}",
                            rule.name
                        ));
                    }
                }
                rule_plans.push(lower_rule(ctx, rule, cost));
            }
            None => {
                return Err(format!(
                    "W7 fail-closed: missing cost facts for rule {}",
                    rule.name
                ));
            }
        }
    }

    Ok(LoweredRust {
        rule_plans,
        sink_only_program: sink_only::lower_program(backend),
    })
}

fn shape_for(ctx: &LowerCtx<'_>, rule_id: RuleId) -> Result<BackendShape, String> {
    ctx.backend_shape.get(&rule_id).copied().ok_or_else(|| {
        format!(
            "W7 fail-closed: missing backend shape for rule {}",
            rule_id.0
        )
    })
}

fn lower_rule(ctx: &LowerCtx<'_>, rule: &BackendRule, cost: &CostFacts) -> RuleLoweringPlan {
    let body = select_lowering(cost).lower_rule(ctx, rule, cost);
    RuleLoweringPlan {
        rule: rule.name.clone(),
        shape: cost.chosen,
        body,
    }
}
