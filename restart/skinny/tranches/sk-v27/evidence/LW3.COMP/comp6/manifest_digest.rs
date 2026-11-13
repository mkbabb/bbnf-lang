//! sk-v27 LW3.COMP-6 evidence tool (not a test of record): compiles every
//! `grammar/MANIFEST` entry through the root pipeline to the TS, WASM and
//! Rust-prepared targets and writes one line per entry and target, the
//! output's digest or the error, to `$BBNF_MANIFEST_DIGEST_OUT`. Read before
//! and after the COMP6-Q cure; any differing line is a consumer-visible change.
//! Placed temporarily at crates/core/tests/manifest_digest.rs to run; never committed there.
//! One entry per process (`$BBNF_MANIFEST_ENTRY`, 0-based line of the MANIFEST), appended to
//! the output, so an entry that aborts the process (a stack overflow) is recorded by the
//! driver `manifest_digest.sh` as ABORT and the rest still run.
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::PathBuf;

use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request};

fn digest<T: Hash + ?Sized>(t: &T) -> u64 {
    let mut h = DefaultHasher::new();
    t.hash(&mut h);
    h.finish()
}

#[test]
fn manifest_digest() {
    let Some(out) = std::env::var_os("BBNF_MANIFEST_DIGEST_OUT") else { return };
    // The root pipeline recurses deeply on some grammars (xtask regen runs it
    // on a large stack too).
    std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(move || run(out))
        .unwrap()
        .join()
        .unwrap();
}

fn run(out: std::ffi::OsString) {
    let root = PathBuf::from("../..");
    let manifest = std::fs::read_to_string(root.join("grammar/MANIFEST")).unwrap();
    let mut lines = Vec::new();
    let n: usize = std::env::var("BBNF_MANIFEST_ENTRY").unwrap().parse().unwrap();
    for line in manifest.lines().filter(|l| !l.trim().is_empty()).skip(n).take(1) {
        let rel = line.split_whitespace().last().unwrap();
        for (name, target) in [
            ("ts", CompileTarget::Ts),
            ("wasm", CompileTarget::Wasm),
            ("rust", CompileTarget::Rust { requested_prettify: false }),
        ] {
            let request = CompileRequest { options: PipelineOptions::default(), target };
            let path = root.join(rel);
            let r = std::panic::catch_unwind(|| compile_paths_request(&[path], &request));
            let v = match r {
                Ok(Ok(CompileOutput::Ts(s))) => format!("ok {:016x}", digest(&s)),
                Ok(Ok(CompileOutput::Wasm(b))) => format!("ok {:016x}", digest(&b)),
                Ok(Ok(CompileOutput::Rust(p))) => format!("ok types={:016x}", digest(&format!("{:?}", p.ir.types))),
                Ok(Ok(_)) => "ok other".to_string(),
                Ok(Err(e)) => format!("err {:016x}", digest(&format!("{e:?}"))),
                Err(_) => "PANIC".to_string(),
            };
            lines.push(format!("{rel}\t{name}\t{v}"));
        }
    }
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(out).unwrap();
    f.write_all((lines.join("\n") + "\n").as_bytes()).unwrap();
}
