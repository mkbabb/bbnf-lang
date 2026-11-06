use bbnf_json_stages_prototype::vertical::{
    assay,
    budget::Budget,
    graph::DependencyGraph,
    normalize::NormalizedProgram,
    program::Program,
    source::{SourceLock, SourceUnit},
};

const SOURCE: &str = include_str!("fixtures/source/json.bbnf");
const SOURCE_SHA256: &str = "ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604";

fn stages() -> (SourceUnit, Program, NormalizedProgram, DependencyGraph) {
    let budget = Budget::default();
    let source = SourceUnit::acquire_locked(
        SOURCE,
        SourceLock {
            bytes: 537,
            sha256: SOURCE_SHA256,
        },
        budget,
    )
    .expect("locked fixture");
    let stages = assay(&source, budget).expect("assay stages");
    (source, stages.program, stages.normalized, stages.graph)
}

#[path = "vertical/budget.rs"]
mod budget;
#[path = "vertical/graph.rs"]
mod graph;
#[path = "vertical/normalize.rs"]
mod normalize;
#[path = "vertical/program.rs"]
mod program;
#[path = "vertical/source.rs"]
mod source;
#[path = "vertical/types.rs"]
mod types;
