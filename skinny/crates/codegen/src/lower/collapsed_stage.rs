use super::{LowerCtx, ShapeLowering};
use ir::{BackendRule, BackendShape, CostFacts};

pub static LOWERING: Lowering = Lowering;

pub struct Lowering;

impl ShapeLowering for Lowering {
    fn lower_rule(&self, _ctx: &LowerCtx<'_>, rule: &BackendRule, cost: &CostFacts) -> String {
        debug_assert_eq!(cost.chosen, BackendShape::CollapsedStage);
        lower_rule(rule)
    }
}

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> collapsed_stage", rule.name)
}
