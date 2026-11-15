//! GEN-RECURSE site probe (evidence only, never committed into the suite): with `probe.diff` applied (a
//! depth guard in `alt.rs::branch_pushes_children` that panics past depth 4096 naming the site), compile the
//! repro grammar and every MANIFEST entry through root on a 1 GiB stack, and print which ones reach the guard.
use bbnf::pipeline::{CompileRequest, CompileTarget, PipelineOptions, compile_paths_request};
use std::path::PathBuf;

#[test]
fn site_probe() {
    std::thread::Builder::new().stack_size(1 << 30).spawn(run).unwrap().join().unwrap();
}

fn run() {
    let root = PathBuf::from("../..");
    let manifest = std::fs::read_to_string(root.join("grammar/MANIFEST")).unwrap();
    let mut hit = Vec::new();
    for line in manifest.lines().filter(|l| !l.trim().is_empty()) {
        let rel = line.split_whitespace().last().unwrap().to_string();
        let path = root.join(&rel);
        let request = CompileRequest {
            options: PipelineOptions::default(),
            target: CompileTarget::Rust { requested_prettify: false },
        };
        let r = std::panic::catch_unwind(|| {
            if let Ok(bbnf::pipeline::CompileOutput::Rust(p)) = compile_paths_request(&[path], &request) {
                let attrs = bbnf::ParserAttributes {
                    builder_path: Some("crate::runtime::bbnf::BbnfStructBuilder".to_string()),
                    document_path: Some("crate::runtime::bbnf::BbnfDocument".to_string()),
                    ..bbnf::ParserAttributes::default()
                };
                let _ = bbnf::generate::generate_all(&p, &attrs, &quote::format_ident!("ProbeParser"));
            }
        });
        let v = match r {
            Ok(()) => "ok".to_string(),
            Err(e) => e.downcast_ref::<String>().cloned()
                .or_else(|| e.downcast_ref::<&str>().map(|s| s.to_string())).unwrap_or_default(),
        };
        if v.contains("GEN-RECURSE") { hit.push(rel.clone()); }
        println!("PROBE\t{rel}\t{}", v.lines().next().unwrap_or(""));
    }
    println!("PROBE-HITS\t{}\t{}", hit.len(), hit.join(","));
}
