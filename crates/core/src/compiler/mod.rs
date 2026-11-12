mod analysis;
mod budget;
mod dependency;
mod diagnostic;
mod emit;
mod event;
mod frontend;
mod instance;
mod lower;
mod pratt;
mod regex_table;
mod replay;
mod source;
mod syntax;
mod typecheck;

use instance::ValidatedGrammarInstance;

const _: () = assert!(parse_that::kernel::KERNEL_ABI == 4);

pub use analysis::{AnalysisFacts, NodeFacts, NodeFactsRows, RuleFacts, RuleFactsRows};
pub use budget::{CompileOptions, RegexBudget, RuntimeBudget};
pub use dependency::{DependencyGraph, DependencySolution};
pub use diagnostic::{Diagnostic, DiagnosticCode, DiagnosticSet, StageResult};
pub use emit::GeneratedModule;
pub use event::{EventKind, ScalarEventPlan, TransformSite, TransformSites};
pub use instance::GrammarInstanceInput;
pub use lower::{
    LoweredField, LoweredFields, LoweredRegex, LoweredRule, LoweredRules, LoweringPlan, PlanNode,
    PlanNodes, PlanOp, PlanRecordSlot, PlanRecordSlots,
};
pub use pratt::{PrattFixity, PrattOperator, PrattOperatorId, PrattTable, PrattTableId};
pub use replay::{CompiledModule, ReplayComponent, ReplayError, replay_compilation};
pub use source::{Revision, SourceMap, SourceModule, SourceSpan};
pub use syntax::{
    FieldDecl, FieldId, Fields, Node, NodeId, NodeIds, NodeKind, NodeRemap, Nodes, PlanNodeId,
    PlanNodeIds, Program, ProjectionKeep, RecordSlot, RecordSlots, Rule, RuleId, RuleIds, Rules,
    SccId, SccIds, TransformId, TypeId, TypeIds,
};
pub use typecheck::{
    InferenceRule, NodeTypeDerivation, NodeTypeDerivations, RecordFieldType, RecordFieldTypes,
    RecordWitnessSlot, RecordWitnessSlots, RuleTypeDerivation, RuleTypeDerivations, SemanticType,
    SemanticTypes, TypeSolution,
};

pub fn compile_instance(
    input: GrammarInstanceInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    let validated = match instance::decode_instance(input, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    compile_validated_instance(validated, options)
}

fn compile_validated_instance(
    validated: ValidatedGrammarInstance<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    let (instance, source_bytes) = validated.into_parts();
    if source_bytes.len() > options.source_bytes || u32::try_from(source_bytes.len()).is_err() {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::SourceTooLarge,
            0,
            source_bytes.len(),
            format!(
                "source has {} bytes; configured limit is {}",
                source_bytes.len(),
                options.source_bytes
            ),
        ));
    }
    let revision = instance.revision(source_bytes);
    let (source, presentation) = match frontend::parse_source(source_bytes, revision, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    match (instance.trivia_regex().is_some(), source.uses_trivia()) {
        (true, false) => {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::InvalidGrammarInstance,
                0,
                0,
                "trivia_regex is present but the grammar contains no `?w` use",
            ));
        }
        (false, true) => {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::InvalidGrammarInstance,
                0,
                0,
                "the grammar uses `?w` but trivia_regex is null",
            ));
        }
        _ => {}
    }
    let source_map = SourceMap::one(source, presentation);
    let module = &source_map.modules()[0];
    let program = match Program::resolve(module, &instance) {
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
    let lowering =
        match LoweringPlan::build(&program, &types, &facts, instance.trivia_regex(), options) {
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
        instance,
        source: source_bytes.to_vec(),
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
