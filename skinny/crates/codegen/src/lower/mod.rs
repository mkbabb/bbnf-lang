pub mod collapsed_stage;
pub mod eager_tape;
pub mod event_tape;
pub mod offset_tape;
pub mod rust;
pub mod schema_direct;
pub mod sink_only;

use ir::{BackendRule, BackendShape, CostFacts};

pub use rust::{lower_to_rust, LowerCtx};

pub trait ShapeLowering {
    fn lower_rule(&self, ctx: &LowerCtx<'_>, rule: &BackendRule, cost: &CostFacts) -> String;
}

pub fn select_lowering(cost: &CostFacts) -> &'static dyn ShapeLowering {
    match cost.chosen {
        BackendShape::EagerTape => &eager_tape::LOWERING,
        BackendShape::OffsetTape => &offset_tape::LOWERING,
        BackendShape::EventTape => &event_tape::LOWERING,
        BackendShape::SinkOnly => &sink_only::LOWERING,
        BackendShape::CollapsedStage => &collapsed_stage::LOWERING,
    }
}
