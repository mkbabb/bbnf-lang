mod support;

use bbnf::StageResult;

fn main() {
    let module = match support::compile() {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { .. } => return,
    };
    let id = module.program().nodes().get(0).unwrap().id();
    let bbnf::compiler::NodeId { local, revision } = id;
    let _forged = bbnf::compiler::NodeId { local, revision };
}
