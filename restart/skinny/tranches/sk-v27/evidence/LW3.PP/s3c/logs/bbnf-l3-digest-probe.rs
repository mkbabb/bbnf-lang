//! Temporary probe (sk-v27 LW3.S3c-PP L3 digest explanation); never committed.
use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request};
use std::path::PathBuf;
#[test]
fn jc_dump() {
    let Some(out) = std::env::var_os("BBNF_JC_OUT") else { return };
    std::thread::Builder::new().stack_size(1 << 30).spawn(move || {
        let out = PathBuf::from(out);
        let path = PathBuf::from("../..").join("grammar/misc/emoji.bbnf");
        let ts = compile_paths_request(&[path], &CompileRequest { options: PipelineOptions::default(), target: CompileTarget::Ts });
        let text = match ts { Ok(CompileOutput::Ts(s)) => format!("OK\n{s}"), Ok(_) => "OK other".into(), Err(e) => format!("ERR\n{e:#?}") };
        std::fs::write(out.join("emoji.txt"), text).unwrap();
    }).unwrap().join().unwrap();
}
