use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

fn main() {
    let module = match compile_module(
        ModuleInput::new("opacity.bbnf", b"root = \"x\";"),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { .. } => return,
    };
    let _json = serde_json::to_string(module.program()).unwrap();
}
