mod support;

use bbnf::StageResult;

fn main() {
    let module = match support::compile() {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { .. } => return,
    };
    let _json = serde_json::to_string(module.program()).unwrap();
}
