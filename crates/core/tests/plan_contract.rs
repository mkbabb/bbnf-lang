//! The compile plan's grammar-level contract (sk-v27 LW1.COMP-3).
//!
//! - **Contradiction is refused.** A rule that two directives force
//!   to opposite call modes is a contradictory grammar. `@token`
//!   forces `InlineBody` and `@pretty` forces `DirectCall`. Born RED in
//!   LW1.COMP-3 (the priority chain picked `DirectCall` and the grammar
//!   compiled); LW2.COMP-2 makes the conflict ⊤ and the compile an
//!   error that names both directives.
//!
//! - **I7 generality.** No plan decision may read a rule's name. The
//!   same grammar under two spellings, one with names a heuristic might
//!   key on (`value`, `string`, `number`, `object`, `array`) and one
//!   opaque, must produce the same call strategies and the same rule
//!   metadata, rule for rule.

use bbnf::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar,
    compile_grammar_request, compute_call_strategies,
};

#[test]
fn contradictory_call_mode_directives_are_refused() {
    let source = r#"
        word = /[a-z]+/ ;
        pair = word, ":" >> word ;
        @token word ;
        @pretty word group ;
    "#;

    let ir = compile_grammar(source, &PipelineOptions::default())
        .expect("the grammar is well-formed apart from the contradiction");
    let decided = compute_call_strategies(&ir);

    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust {
            requested_prettify: true,
        },
    };
    let output = compile_grammar_request(source, &request);
    assert!(
        output.is_err(),
        "`word` is forced InlineBody by @token and DirectCall by @pretty; the plan silently \
         decided {decided:?} and the compile returned {}",
        match &output {
            Ok(CompileOutput::Rust(_)) => "Ok(Rust)",
            Ok(_) => "Ok(other)",
            Err(_) => unreachable!(),
        },
    );
    let Err(err) = output else { unreachable!() };
    assert!(
        matches!(&err, CompileError::CallModeConflict { rule, .. } if rule == "word"),
        "expected a call-mode conflict on `word`, got {err:?}"
    );
    let message = err.to_string();
    assert!(
        message.contains("@token") && message.contains("@pretty"),
        "the diagnostic must name both directives: {message}"
    );
}

/// One grammar, spelled with the eight names in `names`.
fn grammar(names: [&str; 8]) -> String {
    let [word, digits, sep, atom, items, list, pair, top] = names;
    format!(
        r#"
        {word} = /[a-z]+/ ;
        {digits} = /[0-9]+/ ;
        {sep} = "," ?w ;
        {atom} = {word} | {digits} | {list} ;
        {items} = ( {atom} << {sep} ? ) * ;
        {list} = "[" >> {items} ?w << "]" ;
        {pair} = {word}, ":" >> {atom} ;
        {top} = {pair} | {atom} ;
        @pretty {list} group ;
        "#
    )
}

fn plan_of(source: &str) -> Vec<String> {
    let ir = compile_grammar(source, &PipelineOptions::default()).expect("grammar compiles");
    let strategies = compute_call_strategies(&ir).expect("the plan solves");
    ir.rules
        .iter()
        .zip(strategies)
        .map(|(rule, strategy)| format!("{strategy:?} {:?}", rule.meta))
        .collect()
}

#[test]
fn plan_decisions_do_not_read_rule_names() {
    let suggestive = plan_of(&grammar([
        "string", "number", "comma", "value", "members", "array", "pair", "object",
    ]));
    let opaque = plan_of(&grammar(["r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"]));
    assert_eq!(suggestive.len(), opaque.len());
    for (idx, (a, b)) in suggestive.iter().zip(&opaque).enumerate() {
        assert_eq!(a, b, "rule {idx} is planned differently under a renaming");
    }
}

// ── sk-v27 LW3.COMP-5a: the rename test on root over 9 + 3 grammars ──────
//
// Every rule of each grammar of record (`[workspace.metadata.bbnf]
// grammars`) and of each held-out grammar (`grammar/heldout`) is renamed
// to an opaque name in a temporary copy of its directory (imports
// included). The plans (call strategy and rule metadata, rule for rule)
// must be byte-identical. For the nine, the root emission under an
// opaque grammar ident must equal the emission under the grammar's own
// ident once the ident is spelled back: no emission decision reads the
// grammar's name. The spine half (the same over `compiler/`) is read
// after LW3.COMP-1c (RULINGS 2026-11-13c COMP5A-S).

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use bbnf::ParserAttributes;
use bbnf::backend::PreparedGrammar;
use bbnf::pipeline::compile_paths_request;

struct Subject {
    /// The parser ident regen gives a grammar of record; `None` for a
    /// held-out grammar.
    ident: Option<String>,
    path: PathBuf,
    features: Vec<String>,
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("workspace root")
}

/// The regen marker ident (`xtask/src/regen.rs` `marker_ident`).
fn marker_ident(ident: &str) -> String {
    if ident == "bbnf" {
        return "BbnfBootstrap".to_string();
    }
    let camel: String = ident
        .split('_')
        .map(|part| {
            let mut chars = part.chars();
            chars
                .next()
                .map(|c| c.to_ascii_uppercase().to_string() + chars.as_str())
                .unwrap_or_default()
        })
        .collect();
    format!("{camel}Parser")
}

fn subjects() -> Vec<Subject> {
    let root = workspace_root();
    let manifest: toml::Value =
        toml::from_str(&std::fs::read_to_string(root.join("Cargo.toml")).unwrap()).unwrap();
    let mut out: Vec<Subject> = manifest["workspace"]["metadata"]["bbnf"]["grammars"]
        .as_array()
        .expect("[workspace.metadata.bbnf] grammars")
        .iter()
        .map(|entry| Subject {
            ident: Some(marker_ident(entry["ident"].as_str().unwrap())),
            path: root.join(entry["path"].as_str().unwrap()),
            features: entry["features"]
                .as_array()
                .unwrap()
                .iter()
                .map(|f| f.as_str().unwrap().to_string())
                .collect(),
        })
        .collect();
    let mut heldout: Vec<PathBuf> = std::fs::read_dir(root.join("grammar/heldout"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|x| x == "bbnf"))
        .collect();
    heldout.sort();
    out.extend(heldout.into_iter().map(|path| Subject {
        ident: None,
        path,
        features: Vec::new(),
    }));
    out
}

fn attrs_for(subject: &Subject, path: &Path) -> ParserAttributes {
    let mut attrs = ParserAttributes::with_paths(vec![path.to_path_buf()]);
    for feature in &subject.features {
        match feature.as_str() {
            "structural" => attrs.structural = true,
            "prettify" => attrs.prettify = true,
            "skip_recover" => attrs.skip_recover = true,
            "serialize" => attrs.serialize = true,
            other => panic!("unknown grammar feature `{other}`"),
        }
    }
    attrs
}

fn prepare(attrs: &ParserAttributes) -> PreparedGrammar {
    let request = CompileRequest {
        options: PipelineOptions {
            structural: attrs.structural,
            ..PipelineOptions::default()
        },
        target: CompileTarget::Rust {
            requested_prettify: attrs.prettify,
        },
    };
    match compile_paths_request(&attrs.paths, &request) {
        Ok(CompileOutput::Rust(prepared)) => *prepared,
        Ok(_) => unreachable!("a Rust request returns Rust output"),
        Err(err) => panic!("{}: {err}", attrs.paths[0].display()),
    }
}

fn rule_names(prepared: &PreparedGrammar) -> Vec<String> {
    prepared
        .ir
        .rules
        .iter()
        .map(|rule| prepared.ir.get_string(rule.name).to_string())
        .collect()
}

fn prepared_plan(prepared: &PreparedGrammar) -> Vec<String> {
    let strategies = compute_call_strategies(&prepared.ir).expect("the plan solves");
    prepared
        .ir
        .rules
        .iter()
        .zip(strategies)
        .map(|(rule, strategy)| format!("{strategy:?} {:?}", rule.meta))
        .collect()
}

/// Rewrites every identifier token of a BBNF source that `names` maps,
/// leaving string literals, regexes, comments and `@directive` keywords
/// untouched (the lexical forms of `grammar/bbnf/bbnf.bbnf`).
fn rename_source(source: &str, names: &BTreeMap<String, String>) -> String {
    let bytes = source.as_bytes();
    let mut out = String::with_capacity(source.len());
    let mut i = 0;
    let skip_quoted = |mut j: usize, close: u8| {
        while j < bytes.len() && bytes[j] != close {
            j += if bytes[j] == b'\\' { 2 } else { 1 };
        }
        (j + 1).min(bytes.len())
    };
    while i < bytes.len() {
        let c = bytes[i];
        let start = i;
        i = match c {
            b'"' | b'\'' | b'`' => skip_quoted(i + 1, c),
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                source[i..].find('\n').map_or(bytes.len(), |n| i + n)
            }
            b'/' if bytes.get(i + 1) == Some(&b'*') => source[i + 2..]
                .find("*/")
                .map_or(bytes.len(), |n| i + 2 + n + 2),
            b'/' => skip_quoted(i + 1, b'/'),
            b'@' | b'_' | b'a'..=b'z' | b'A'..=b'Z' => {
                let mut j = i + 1;
                while j < bytes.len()
                    && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_' || bytes[j] == b'-')
                {
                    j += 1;
                }
                j
            }
            _ => {
                let len = source[i..].chars().next().unwrap().len_utf8();
                i + len
            }
        };
        let token = &source[start..i];
        out.push_str(names.get(token).map_or(token, String::as_str));
    }
    out
}

/// A copy of the grammar's directory with every rule renamed; returns
/// the renamed entry path (inside `dir`).
fn renamed_copy(entry: &Path, names: &BTreeMap<String, String>, dir: &Path) -> PathBuf {
    let source_dir = entry.parent().unwrap();
    for file in std::fs::read_dir(source_dir).unwrap() {
        let path = file.unwrap().path();
        if path.extension().is_some_and(|x| x == "bbnf") {
            let source = std::fs::read_to_string(&path).unwrap();
            std::fs::write(
                dir.join(path.file_name().unwrap()),
                rename_source(&source, names),
            )
            .unwrap();
        }
    }
    dir.join(entry.file_name().unwrap())
}

fn emit(
    prepared: &PreparedGrammar,
    attrs: &ParserAttributes,
    ident: &str,
) -> Result<String, String> {
    let ident = quote::format_ident!("{ident}");
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        bbnf::generate::generate_all(prepared, attrs, &ident).to_string()
    }))
    .map_err(|panic| {
        panic
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| panic.downcast_ref::<&str>().map(|s| s.to_string()))
            .unwrap_or_default()
    })
}

#[test]
fn renaming_every_rule_and_the_grammar_ident_changes_no_root_plan_or_emission() {
    const OPAQUE_IDENT: &str = "I7RenamedParser";
    let subjects = subjects();
    assert_eq!(subjects.len(), 12, "9 grammars of record + 3 held-out");
    let mut failures = Vec::new();
    for subject in &subjects {
        let label = subject.path.display().to_string();
        let attrs = attrs_for(subject, &subject.path);
        let prepared = prepare(&attrs);
        let names = rule_names(&prepared);
        let mut sorted = names.clone();
        sorted.sort();
        sorted.dedup();
        let map: BTreeMap<String, String> = sorted
            .iter()
            .enumerate()
            .map(|(i, name)| (name.clone(), format!("i7r{i}")))
            .collect();
        let dir = tempfile::tempdir().unwrap();
        let renamed_path = renamed_copy(&subject.path, &map, dir.path());
        let renamed = prepare(&attrs_for(subject, &renamed_path));
        let expected: Vec<String> = names.iter().map(|n| map[n].clone()).collect();
        if rule_names(&renamed) != expected {
            failures.push(format!(
                "{label}: the renaming did not rename rule for rule"
            ));
            continue;
        }
        if prepared_plan(&prepared) != prepared_plan(&renamed) {
            failures.push(format!(
                "{label}: the plan differs under a renaming of every rule"
            ));
        }
        let Some(ident) = &subject.ident else {
            continue;
        };
        let own = emit(&prepared, &attrs, ident);
        let opaque = emit(&prepared, &attrs, OPAQUE_IDENT).map(|text| {
            text.replace(OPAQUE_IDENT, ident)
                .replace(&OPAQUE_IDENT.to_lowercase(), &ident.to_lowercase())
        });
        match (own, opaque) {
            (Ok(own), Ok(opaque)) if own == opaque => {}
            (Ok(_), Ok(_)) => failures.push(format!(
                "{label}: the root emission under `{OPAQUE_IDENT}` differs from `{ident}`'s"
            )),
            (own, opaque) => failures.push(format!(
                "{label}: emission under `{ident}`: {:?}; under `{OPAQUE_IDENT}`: {:?}",
                own.err(),
                opaque.err()
            )),
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
