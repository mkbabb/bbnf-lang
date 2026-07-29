mod analysis;
mod budget;
mod dependency;
mod diagnostic;
mod edit;
mod emit;
mod event;
mod frontend;
mod lower;
mod replay;
mod source;
mod syntax;
mod typecheck;

const _: () = assert!(parse_that::KERNEL_ABI == 2);

pub use analysis::{AnalysisFacts, NodeFacts, NodeFactsRows, RuleFacts, RuleFactsRows};
pub use budget::{CompileOptions, RegexBudget, RuntimeBudget};
pub use dependency::{DependencyGraph, DependencySolution};
pub use diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
pub use edit::{CompilationSession, EditError};
pub use emit::GeneratedModule;
pub use event::{EventKind, ScalarEventPlan, TransformSite, TransformSites};
pub use lower::{
    LoweredField, LoweredFields, LoweredRegex, LoweredRule, LoweredRules, LoweringPlan, PlanNode,
    PlanNodes, PlanOp, PlanRecordSlot, PlanRecordSlots,
};
pub use replay::{ReplayComponent, ReplayError, replay_compilation};
pub use source::{ModuleInput, Revision, SourceMap, SourceModule, SourceSpan};
pub use syntax::{
    FieldDecl, FieldId, Fields, Node, NodeId, NodeIds, NodeKind, NodeRemap, Nodes, PlanNodeId,
    PlanNodeIds, Program, RecordSlot, RecordSlots, Rule, RuleId, RuleIds, Rules, SccId, SccIds,
    TransformId, TypeId, TypeIds,
};
pub use typecheck::{
    InferenceRule, NodeTypeDerivation, NodeTypeDerivations, RecordFieldType, RecordFieldTypes,
    RecordWitnessSlot, RecordWitnessSlots, RuleTypeDerivation, RuleTypeDerivations, SemanticType,
    SemanticTypes, TypeSolution,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompiledModule {
    options: CompileOptions,
    source_map: SourceMap,
    program: Program,
    remap: NodeRemap,
    dependency: DependencySolution,
    types: TypeSolution,
    facts: AnalysisFacts,
    lowering: LoweringPlan,
    events: ScalarEventPlan,
    generated: GeneratedModule,
}

impl CompiledModule {
    pub const fn options(&self) -> CompileOptions {
        self.options
    }

    pub const fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub const fn program(&self) -> &Program {
        &self.program
    }

    pub const fn remap(&self) -> &NodeRemap {
        &self.remap
    }

    pub const fn dependency(&self) -> &DependencySolution {
        &self.dependency
    }

    pub const fn types(&self) -> &TypeSolution {
        &self.types
    }

    pub const fn facts(&self) -> &AnalysisFacts {
        &self.facts
    }

    pub const fn lowering(&self) -> &LoweringPlan {
        &self.lowering
    }

    pub const fn events(&self) -> &ScalarEventPlan {
        &self.events
    }

    pub const fn generated(&self) -> &GeneratedModule {
        &self.generated
    }
}

pub fn compile_module(
    input: ModuleInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    let source = match frontend::parse_source(input, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    let source_map = SourceMap::one(source);
    let module = &source_map.modules()[0];
    let program = match Program::resolve(module) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    let remap = NodeRemap::identity(&program);
    let dependency = match DependencySolution::build(&program) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    let types = match TypeSolution::solve(&program, &dependency) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    if let StageResult::Rejected { diagnostics } = types.verify(&program) {
        return StageResult::Rejected { diagnostics };
    }
    let facts = match AnalysisFacts::derive(&program, &dependency, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    let lowering = match LoweringPlan::build(&program, &types, &facts, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    let events = match ScalarEventPlan::seal(&program, &lowering) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    let generated = match GeneratedModule::emit(&events, &lowering, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };

    StageResult::accepted(CompiledModule {
        options,
        source_map,
        program,
        remap,
        dependency,
        types,
        facts,
        lowering,
        events,
        generated,
    })
}
