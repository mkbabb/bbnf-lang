mod analysis;
mod budget;
mod dependency;
mod diagnostic;
mod emit;
mod event;
mod frontend;
mod lower;
mod replay;
mod source;
mod syntax;
mod typecheck;

pub use analysis::{AnalysisFacts, NodeFacts, RuleFacts};
pub use budget::{CompileOptions, RegexBudget};
pub use dependency::{DependencyGraph, DependencySolution};
pub use diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
pub use emit::GeneratedModule;
pub use event::{EventKind, ScalarEventPlan};
pub use lower::{LoweringPlan, PlanOp};
pub use source::{ModuleInput, Revision, SourceMap, SourceModule, SourceSpan};
pub use syntax::{
    Node, NodeId, NodeKind, NodeRemap, PlanNodeId, Program, Rule, RuleId, SccId, TypeId,
};
pub use typecheck::{NodeTypeDerivation, RuleTypeDerivation, SemanticType, TypeSolution};

#[derive(Clone, Debug)]
pub struct CompiledModule {
    pub source_map: SourceMap,
    pub program: Program,
    pub dependency: DependencySolution,
    pub types: TypeSolution,
    pub facts: AnalysisFacts,
    pub lowering: LoweringPlan,
    pub events: ScalarEventPlan,
    pub generated: GeneratedModule,
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
    let generated = match GeneratedModule::emit(&events, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };

    StageResult::accepted(CompiledModule {
        source_map,
        program,
        dependency,
        types,
        facts,
        lowering,
        events,
        generated,
    })
}
