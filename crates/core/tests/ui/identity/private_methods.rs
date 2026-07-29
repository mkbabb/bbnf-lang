use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

fn main() {
    let module = match compile_module(
        ModuleInput::new("opacity.bbnf", b"root = \"x\";"),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { .. } => return,
    };
    let id = module.program().nodes().get(0).unwrap().id();
    let _ = id.local();
}
