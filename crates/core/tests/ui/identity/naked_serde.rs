mod support;

use bbnf::StageResult;

fn main() {
    let module = match support::compile() {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { .. } => return,
    };
    let id = module.program().nodes().get(0).unwrap().id();
    let _json = serde_json::to_string(&id).unwrap();
}
