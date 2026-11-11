//! sk-v27 LW2.RX-4: `FuseAltRegexBranches` leaves the pass list of every
//! backend whose regex leaf is parse-that-regex's DFA.
//!
//! The DFA is leftmost-longest. JS `RegExp` (the TS backend's leaf, and the
//! oracle of record, regex C1-3) is leftmost-first, and so is the grammar's
//! ordered choice. Fusing `Alt(/a/, /ab/)` into `/a|ab/` is sound under the
//! JS leaf and unsound under the DFA leaf: on `"ab"` the ordered choice
//! consumes 1 byte (oracle `rx2-alt-a-ab`) and the fused DFA leaf consumes 2.
//! The rule comes back for the DFA backends only through LW3.RX-1, with its
//! `L_cut` certificate.
//!
//! The pass list is printed by the e-graph report (`BBNF_EGRAPH_REPORT=1`,
//! one `rule=<name>` line per rule); `evidence/LW2.RX/rx4-falsifier.sh` reads
//! it through `rx4_print_pass_list_{rust,ts}`. The TS emissions of the
//! workspace's 9 entry grammars are written by `rx4_ts_emit_dump` for the
//! byte-identity read.

use std::path::{Path, PathBuf};

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar,
    compile_grammar_request, compile_paths_request,
};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;

/// Two regex branches, the shorter first: the LW1.RX-2 `Alt(/a/, /ab/)` case.
const ALT_A_AB: &str = r#"
x = /a/ | /ab/ ;
"#;

fn request(target: CompileTarget) -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target,
    }
}

/// Oracle `rx2-alt-a-ab`: `/a|ab/y` on `"ab"` is `"a"`, and the ordered
/// choice `/a/ | /ab/` takes its first branch. The VM runs the DFA leaf.
#[test]
fn vm_alt_a_ab_consumes_one_byte() {
    let ir = compile_grammar(ALT_A_AB, &PipelineOptions::default()).expect("compiles");
    let program = compile_bytecode(&ir);
    let mut interp = Interpreter::new(&program, "ab");
    let result = interp.run();
    assert!(result.success, "the first branch matches");
    assert_eq!(
        result.offset, 1,
        "ordered choice takes /a/ (1 byte), as the oracle does; 2 means the branches were fused into one leftmost-longest DFA leaf"
    );
}

/// The IR the Rust emitter receives carries no fused `a|ab` pattern: the
/// string is never interned, so no regex leaf can name it.
#[test]
fn rust_ir_keeps_the_branches_apart() {
    let prepared = match compile_grammar_request(
        ALT_A_AB,
        &request(CompileTarget::Rust {
            requested_prettify: false,
        }),
    )
    .expect("compiles")
    {
        CompileOutput::Rust(prepared) => *prepared,
        _ => unreachable!("Rust target"),
    };
    assert_eq!(
        prepared.ir.find_string_id("a|ab"),
        None,
        "the Rust target's IR interns the fused pattern `a|ab`"
    );
}

/// The TS backend keeps the rule: its leaf is JS `RegExp`, leftmost-first.
#[test]
fn ts_emit_keeps_fusion() {
    let source =
        match compile_grammar_request(ALT_A_AB, &request(CompileTarget::Ts)).expect("compiles") {
            CompileOutput::Ts(source) => source,
            _ => unreachable!("TS target"),
        };
    assert!(
        source.contains("a|ab"),
        "the TS product fuses the branches into `a|ab`"
    );
}

/// Prints the e-graph pass list for the Rust target (read by the falsifier
/// script with `BBNF_EGRAPH_REPORT=1 --nocapture`).
#[test]
fn rx4_print_pass_list_rust() {
    compile_grammar_request(
        ALT_A_AB,
        &request(CompileTarget::Rust {
            requested_prettify: false,
        }),
    )
    .expect("compiles");
}

/// Prints the e-graph pass list for the TS target.
#[test]
fn rx4_print_pass_list_ts() {
    compile_grammar_request(ALT_A_AB, &request(CompileTarget::Ts)).expect("compiles");
}

/// Writes the TS emission of every entry grammar of the workspace manifest
/// (`[workspace.metadata.bbnf.grammars]`, the grammars `cargo xtask regen`
/// emits for Rust) to `$RX4_TS_DUMP/<ident>.ts` (or `.err` with the error).
/// Runs only when `RX4_TS_DUMP` is set.
#[test]
fn rx4_ts_emit_dump() {
    let Ok(out) = std::env::var("RX4_TS_DUMP") else {
        return;
    };
    let out = PathBuf::from(out);
    std::fs::create_dir_all(&out).expect("dump dir");
    // The CSS grammars recurse deeply in a debug build; css_l4.rs runs its
    // compiles on the same 64 MiB stack.
    std::thread::Builder::new()
        .stack_size(67_108_864)
        .spawn(move || dump_ts_emissions(&out))
        .expect("spawn")
        .join()
        .expect("dump thread");
}

fn dump_ts_emissions(out: &Path) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let cargo = std::fs::read_to_string(root.join("Cargo.toml")).expect("Cargo.toml");
    let field = |line: &str, key: &str| -> Option<String> {
        let rest = &line[line.find(key)? + key.len()..];
        let rest = &rest[rest.find('"')? + 1..];
        Some(rest[..rest.find('"')?].to_string())
    };
    let mut seen = 0;
    for line in cargo
        .lines()
        .filter(|l| l.trim_start().starts_with("{ ident = "))
    {
        let (Some(ident), Some(rel)) = (field(line, "ident"), field(line, "path")) else {
            continue;
        };
        seen += 1;
        match compile_paths_request(&[root.join(&rel)], &request(CompileTarget::Ts)) {
            Ok(CompileOutput::Ts(source)) => {
                std::fs::write(out.join(format!("{ident}.ts")), source).expect("write");
            }
            Ok(_) => unreachable!("TS target"),
            Err(err) => {
                std::fs::write(out.join(format!("{ident}.err")), err.to_string()).expect("write");
            }
        }
    }
    assert_eq!(seen, 9, "the workspace grammar manifest lists 9 grammars");
}
