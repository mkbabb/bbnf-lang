use bbnf::{CompileOptions, CompiledModule, GrammarInstanceInput, StageResult, compile_instance};

pub fn compile(
    logical_path: &str,
    source_bytes: &[u8],
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    let manifest = serde_json::to_vec(&serde_json::json!({
        "schema": "bbnf.grammar-instance.v1",
        "logical_path": logical_path,
        "entry": "root",
        "trivia_regex": null,
        "pratt_tables": [],
    }))
    .expect("test grammar-instance manifest is serializable");
    compile_instance(GrammarInstanceInput::new(&manifest, source_bytes), options)
}
