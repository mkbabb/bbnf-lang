use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use gorgeous::PrinterConfig;

/// Extract rule names from a BBNF grammar source string.
fn extract_rule_names(grammar_src: &str) -> Vec<String> {
    let parser = bbnf::grammar::parse();
    let Some(parsed) = parser.parse(grammar_src) else {
        return Vec::new();
    };
    parsed
        .rules
        .keys()
        .filter_map(|expr| bbnf::get_nonterminal_name(expr).map(String::from))
        .collect()
}

/// Stable hash of grammar content → hex string (for cache key).
fn grammar_hash(content: &str) -> String {
    let mut h = DefaultHasher::new();
    content.hash(&mut h);
    format!("{:016x}", h.finish())
}

/// Where cached binaries live: `~/.cache/gorgeous/<hash>/formatter`
pub fn cache_dir(hash: &str) -> PathBuf {
    let base = std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            let home = std::env::var("HOME").unwrap_or_else(|_| ".".into());
            PathBuf::from(home).join(".cache")
        });
    base.join("gorgeous").join(hash)
}

/// Crate versions baked in at compile time (match gorgeous's own deps).
const DEP_PARSE_THAT: &str = "0.3.0";
const DEP_BBNF_DERIVE: &str = "0.2.3";
const DEP_BBNF: &str = "0.2.4";
const DEP_PPRINT: &str = "0.3.4";

/// Generate a temporary Cargo project that `#[derive(Parser)]` from the grammar.
fn generate_project(
    dir: &Path,
    grammar_content: &str,
    rule_names: &[String],
) -> Result<(), String> {
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).map_err(|e| format!("mkdir: {e}"))?;

    // Copy grammar into the project root
    std::fs::write(dir.join("grammar.bbnf"), grammar_content)
        .map_err(|e| format!("write grammar: {e}"))?;

    // Cargo.toml — deps from crates.io, no local paths
    let cargo_toml = format!(
        r#"[package]
name = "gorgeous-jit"
version = "0.0.0"
edition = "2024"
publish = false

[dependencies]
parse_that = "{DEP_PARSE_THAT}"
bbnf_derive = "{DEP_BBNF_DERIVE}"
bbnf = "{DEP_BBNF}"
pprint = "{DEP_PPRINT}"

[profile.release]
opt-level = 2
lto = "thin"
"#
    );
    std::fs::write(dir.join("Cargo.toml"), cargo_toml)
        .map_err(|e| format!("write Cargo.toml: {e}"))?;

    // rust-toolchain.toml (ensure nightly)
    std::fs::write(
        dir.join("rust-toolchain.toml"),
        "[toolchain]\nchannel = \"nightly\"\n",
    )
    .map_err(|e| format!("write toolchain: {e}"))?;

    // Generate main.rs with match arms for every rule
    // Last rule is typically the entry point (leaf rules first, root rule last).
    let first_rule = rule_names.last().map(|s| s.as_str()).unwrap_or("start");

    let match_arms: String = rule_names
        .iter()
        .map(|name| {
            format!(
                "            \"{name}\" => GrammarParser::{name}_prettify().parse(&input)?"
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let rule_list = rule_names.join(", ");

    let main_rs = format!(
        r##"#![feature(cold_path)]

use std::io::{{self, Read, Write}};
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "grammar.bbnf", prettify)]
pub struct GrammarParser;

fn main() {{
    let args: Vec<String> = std::env::args().skip(1).collect();

    let mut rule: Option<String> = None;
    let mut width: usize = 80;
    let mut indent: usize = 4;
    let mut input_path: Option<String> = None;

    let mut i = 0;
    while i < args.len() {{
        match args[i].as_str() {{
            "--rule" | "-r" => {{
                i += 1;
                rule = args.get(i).cloned();
            }}
            "--width" | "-w" => {{
                i += 1;
                width = args.get(i).and_then(|s| s.parse().ok()).unwrap_or(80);
            }}
            "--indent" | "-i" => {{
                i += 1;
                indent = args.get(i).and_then(|s| s.parse().ok()).unwrap_or(4);
            }}
            _ => input_path = Some(args[i].clone()),
        }}
        i += 1;
    }}

    let rule = rule.as_deref().unwrap_or("{first_rule}");

    let input = match &input_path {{
        Some(path) => std::fs::read_to_string(path).unwrap_or_else(|e| {{
            eprintln!("error reading {{path}}: {{e}}");
            std::process::exit(1);
        }}),
        None => {{
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf).unwrap();
            buf
        }}
    }};

    let ops: Option<Vec<pprint::FmtOp<'_>>> = match rule {{
{match_arms}
        other => {{
            eprintln!("unknown rule: {{other}}");
            eprintln!("available: [{rule_list}]");
            std::process::exit(1);
        }}
    }};

    match ops {{
        Some(ops) => {{
            let output = pprint::render(&ops, pprint::Printer::new(width, indent, false));
            io::stdout().write_all(output.as_bytes()).unwrap();
        }}
        None => {{
            eprintln!("parse error");
            std::process::exit(1);
        }}
    }}
}}
"##
    );

    std::fs::write(src_dir.join("main.rs"), main_rs).map_err(|e| format!("write main.rs: {e}"))?;
    Ok(())
}

/// Compile the generated project.
fn compile_project(dir: &Path) -> Result<PathBuf, String> {
    let output = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .arg("--quiet")
        .current_dir(dir)
        .output()
        .map_err(|e| format!("cargo build: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("compilation failed:\n{stderr}"));
    }

    let binary = dir.join("target/release/gorgeous-jit");
    if !binary.exists() {
        return Err("binary not found after compilation".into());
    }
    Ok(binary)
}

/// Run the cached JIT binary, forwarding config + input.
fn run_jit_binary(
    binary: &Path,
    rule: Option<&str>,
    input_path: Option<&str>,
    input_stdin: Option<&str>,
    config: &PrinterConfig,
) -> Result<String, String> {
    let mut cmd = Command::new(binary);
    cmd.arg("--width").arg(config.max_width.to_string());
    cmd.arg("--indent").arg(config.indent.to_string());
    if config.use_tabs {
        cmd.arg("--tabs");
    }
    if let Some(r) = rule {
        cmd.arg("--rule").arg(r);
    }

    if let Some(path) = input_path {
        cmd.arg(path);
    }

    let output = if input_stdin.is_some() {
        use std::process::Stdio;
        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("spawn: {e}"))?;
        if let Some(stdin_data) = input_stdin {
            child
                .stdin
                .take()
                .unwrap()
                .write_all(stdin_data.as_bytes())
                .map_err(|e| format!("stdin write: {e}"))?;
        }
        child.wait_with_output().map_err(|e| format!("wait: {e}"))?
    } else {
        cmd.output().map_err(|e| format!("exec: {e}"))?
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(stderr.to_string());
    }

    String::from_utf8(output.stdout).map_err(|e| format!("utf8: {e}"))
}

/// Full JIT pipeline: parse grammar → generate → compile → cache → run.
pub fn format_grammar(
    grammar_path: &str,
    rule: Option<&str>,
    input_path: Option<&str>,
    input_stdin: Option<&str>,
    config: &PrinterConfig,
) -> Result<String, String> {
    let grammar_content = std::fs::read_to_string(grammar_path)
        .map_err(|e| format!("error reading grammar {grammar_path}: {e}"))?;

    let rule_names = extract_rule_names(&grammar_content);
    if rule_names.is_empty() {
        return Err("no rules found in grammar (parse failed?)".into());
    }

    eprintln!(
        "gorg: grammar has {} rules: {}",
        rule_names.len(),
        rule_names.join(", ")
    );

    let hash = grammar_hash(&grammar_content);
    let dir = cache_dir(&hash);
    let binary = dir.join("target/release/gorgeous-jit");

    if !binary.exists() {
        eprintln!("gorg: compiling grammar (first run)…");
        generate_project(&dir, &grammar_content, &rule_names)?;
        compile_project(&dir)?;
        eprintln!("gorg: cached at {}", dir.display());
    }

    run_jit_binary(&binary, rule, input_path, input_stdin, config)
}
