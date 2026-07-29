use bbnf::{CompileOptions, CompiledModule, GrammarInstanceInput, StageResult, compile_instance};

const MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "opacity.bbnf",
  "entry": "root",
  "trivia_regex": null,
  "pratt_tables": []
}"#;

pub fn compile() -> StageResult<CompiledModule> {
    compile_instance(
        GrammarInstanceInput::new(MANIFEST, b"root = \"x\";"),
        CompileOptions::default(),
    )
}
