use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::PathBuf;
use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request};
#[test]
fn emoji_err() {
    for rel in ["grammar/misc/emoji.bbnf", "typescript/test/fixtures/grammar/lang/emoji.bbnf"] {
        let request = CompileRequest { options: PipelineOptions::default(), target: CompileTarget::Ts };
        let path = std::path::PathBuf::from("../..").join(rel);
        if let Err(e) = compile_paths_request(&[path], &request) { println!("EMOJI-ERR {rel} {e:?}"); }
    }
}
