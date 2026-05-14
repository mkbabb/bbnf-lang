use ir::{BackendIr, BackendRule, BackendShape, RuleId};
use passes::diagnostics::PassDiagnostic;
use std::collections::HashMap;

use super::{collapsed_stage, eager_tape, event_tape, offset_tape, sink_only};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoweredRust {
    pub parser: String,
    pub generated: String,
    pub rule_plans: Vec<RuleLoweringPlan>,
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
    pub diagnostics: &'a [PassDiagnostic],
}

pub fn lower_to_rust(backend: &BackendIr, ctx: &LowerCtx<'_>) -> LoweredRust {
    let _diagnostics_seen = ctx.diagnostics.len();
    let rule_plans = backend
        .rules
        .iter()
        .enumerate()
        .map(|(index, rule)| lower_rule(rule, shape_for(ctx, index)))
        .collect();

    // Wave 1 makes BIR shape selection real while preserving the current JSON
    // surface byte-for-byte; Wave 2 consumes these plans for SinkOnly emission.
    LoweredRust {
        parser: include_str!("../json_templates/parser.rs").to_string(),
        generated: include_str!("../json_templates/generated.rs").to_string(),
        rule_plans,
    }
}

fn shape_for(ctx: &LowerCtx<'_>, index: usize) -> BackendShape {
    ctx.backend_shape
        .get(&RuleId(index))
        .copied()
        .unwrap_or(BackendShape::EagerTape)
}

fn lower_rule(rule: &BackendRule, shape: BackendShape) -> RuleLoweringPlan {
    let body = match shape {
        BackendShape::EagerTape => eager_tape::lower_rule(rule),
        BackendShape::OffsetTape => offset_tape::lower_rule(rule),
        BackendShape::EventTape => event_tape::lower_rule(rule),
        BackendShape::SinkOnly => sink_only::lower_rule(rule),
        BackendShape::CollapsedStage => collapsed_stage::lower_rule(rule),
    };
    RuleLoweringPlan {
        rule: rule.name.clone(),
        shape,
        body,
    }
}
