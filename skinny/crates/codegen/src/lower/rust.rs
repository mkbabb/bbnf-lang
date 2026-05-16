use ir::{BackendIr, BackendRule, BackendShape, CostFacts, PriorityStep, RuleId, ShapeRationale};
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

pub fn lower_to_rust(backend: &BackendIr, ctx: &LowerCtx<'_>) -> LoweredRust {
    let _diagnostics_seen = ctx.diagnostics.len();
    let rule_plans = backend
        .rules
        .iter()
        .enumerate()
        .map(|(index, rule)| {
            let rule_id = RuleId(index);
            let shape = shape_for(ctx, rule_id);
            match ctx.cost_facts.get(&rule_id) {
                Some(cost) => {
                    debug_assert_eq!(shape, cost.chosen);
                    lower_rule(ctx, rule, cost)
                }
                None => {
                    let fallback = CostFacts::projection(
                        rule_id,
                        shape,
                        ShapeRationale::DefaultOffsetTape,
                        PriorityStep::P7OffsetTapeDefault,
                    );
                    lower_rule(ctx, rule, &fallback)
                }
            }
        })
        .collect();

    LoweredRust {
        rule_plans,
        sink_only_program: sink_only::lower_program(backend),
    }
}

fn shape_for(ctx: &LowerCtx<'_>, rule_id: RuleId) -> BackendShape {
    ctx.backend_shape
        .get(&rule_id)
        .copied()
        .unwrap_or(BackendShape::EagerTape)
}

fn lower_rule(ctx: &LowerCtx<'_>, rule: &BackendRule, cost: &CostFacts) -> RuleLoweringPlan {
    let body = select_lowering(cost).lower_rule(ctx, rule, cost);
    RuleLoweringPlan {
        rule: rule.name.clone(),
        shape: cost.chosen,
        body,
    }
}
