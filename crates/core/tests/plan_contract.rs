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

// ── sk-v27 LW3.COMP-5a: the rename test on root ─────────────────────────
//
// Every rule of each grammar of record (`[workspace.metadata.bbnf]
// grammars`) is renamed to an opaque name in a temporary copy of its
// directory (imports included). The plans (call strategy and rule
// metadata, rule for rule) must be byte-identical modulo the names. The
// root emission under an opaque grammar ident must equal the emission
// under the grammar's own ident once the ident is spelled back: no
// emission decision reads the grammar's name. The spine half (the same
// over `compiler/`) is read after LW3.COMP-1c (RULINGS 2026-11-13c
// COMP5A-S). The three held-out grammars (`grammar/heldout`) do not
// compile on root at the base (kebab-case rule references are reported
// unknown), so their reading waits on ask COMP5A-H (LEDGER).

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use bbnf::ParserAttributes;
use bbnf::backend::PreparedGrammar;
use bbnf::pipeline::compile_paths_request;

struct Subject {
    /// The parser ident regen gives the grammar.
    ident: String,
    path: PathBuf,
    features: Vec<String>,
    /// The runtime binding the regen entry supplies with the grammar.
    builder: String,
    document: String,
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
    let out: Vec<Subject> = manifest["workspace"]["metadata"]["bbnf"]["grammars"]
        .as_array()
        .expect("[workspace.metadata.bbnf] grammars")
        .iter()
        .map(|entry| Subject {
            ident: marker_ident(entry["ident"].as_str().unwrap()),
            path: root.join(entry["path"].as_str().unwrap()),
            builder: entry["builder"].as_str().unwrap().to_string(),
            document: entry["document"].as_str().unwrap().to_string(),
            features: entry["features"]
                .as_array()
                .unwrap()
                .iter()
                .map(|f| f.as_str().unwrap().to_string())
                .collect(),
        })
        .collect();
    out
}

fn attrs_for(subject: &Subject, path: &Path) -> ParserAttributes {
    let mut attrs = ParserAttributes::with_paths(vec![path.to_path_buf()]);
    attrs.builder_path = Some(subject.builder.clone());
    attrs.document_path = Some(subject.document.clone());
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

fn prepare(attrs: &ParserAttributes) -> Result<PreparedGrammar, String> {
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
        Ok(CompileOutput::Rust(prepared)) => Ok(*prepared),
        Ok(_) => unreachable!("a Rust request returns Rust output"),
        Err(err) => Err(err.to_string()),
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

/// The plan of every rule, keyed by rule name, with interned string
/// ids (a recover regex, a sub-variant name) spelled as their text: ids
/// follow the rule order, which follows the names.
fn prepared_plan(prepared: &PreparedGrammar) -> BTreeMap<String, String> {
    let strategies = compute_call_strategies(&prepared.ir).expect("the plan solves");
    prepared
        .ir
        .rules
        .iter()
        .zip(strategies)
        .map(|(rule, strategy)| {
            let mut text = format!("{strategy:?} {:?}", rule.meta);
            for marker in ["Regex(", "variant_name: "] {
                let mut at = 0;
                while let Some(found) = text[at..].find(marker) {
                    let open = at + found + marker.len();
                    let close = open
                        + text[open..]
                            .find(|c: char| !c.is_ascii_digit())
                            .unwrap_or(text.len() - open);
                    match text[open..close].parse::<u32>() {
                        Ok(id) => {
                            let spelled = format!("{:?}", prepared.ir.get_string(id as _));
                            text.replace_range(open..close, &spelled);
                            at = open + spelled.len();
                        }
                        Err(_) => at = close,
                    }
                }
            }
            (prepared.ir.get_string(rule.name).to_string(), text)
        })
        .collect()
}

/// `text` with every opaque rule name (`i7r<k>`, alone or as the stem of
/// a derived name such as a sub-variant `i7r<k>_0`) spelled back as the
/// rule's own name: the comparison is modulo the names.
fn unrename(text: &str, back: &BTreeMap<&str, &str>) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find(|c: char| c.is_ascii_alphanumeric() || c == '_') {
        out.push_str(&rest[..at]);
        rest = &rest[at..];
        let end = rest
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
            .unwrap_or(rest.len());
        let word = &rest[..end];
        let body = word.trim_start_matches('_');
        let lead = &word[..word.len() - body.len()];
        let stem = body.split('_').next().unwrap_or(body);
        match back.get(stem) {
            Some(name) => {
                out.push_str(lead);
                out.push_str(name);
                out.push_str(&body[stem.len()..]);
            }
            None => out.push_str(word),
        }
        rest = &rest[end..];
    }
    out.push_str(rest);
    out
}

/// Every digit run inside a `__`-prefixed word becomes `#`.
fn erase_counters(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut in_synthetic = false;
    let mut prev_word_char = false;
    let mut prev_digit = false;
    for c in text.chars() {
        let word_char = c.is_ascii_alphanumeric() || c == '_';
        if word_char && !prev_word_char {
            in_synthetic = c == '_';
        }
        if !word_char {
            in_synthetic = false;
        }
        if in_synthetic && c.is_ascii_digit() {
            if !prev_digit {
                out.push('#');
            }
            prev_digit = true;
        } else {
            out.push(c);
            prev_digit = false;
        }
        prev_word_char = word_char;
    }
    out
}

/// Rules the pipeline synthesizes (`__`-prefixed) are named from their
/// parent rule and a counter that follows the rule order (which follows
/// the names), so they are compared as a multiset of plans with the
/// counters of synthesized names erased.
fn is_synthetic(name: &str) -> bool {
    name.starts_with("__")
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
    assert_eq!(subjects.len(), 9, "the 9 grammars of record");
    let mut failures = Vec::new();
    for subject in &subjects {
        let label = subject.path.display().to_string();
        let attrs = attrs_for(subject, &subject.path);
        let prepared = match prepare(&attrs) {
            Ok(prepared) => prepared,
            Err(err) => {
                failures.push(format!("{label}: root does not compile the grammar: {err}"));
                continue;
            }
        };
        let names: Vec<String> = rule_names(&prepared)
            .into_iter()
            .filter(|name| !is_synthetic(name))
            .collect();
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
        let renamed = match prepare(&attrs_for(subject, &renamed_path)) {
            Ok(renamed) => renamed,
            Err(err) => {
                failures.push(format!("{label}: the renamed copy does not compile: {err}"));
                continue;
            }
        };
        let back: BTreeMap<&str, &str> = map
            .iter()
            .map(|(name, opaque)| (opaque.as_str(), name.as_str()))
            .collect();
        let plan = prepared_plan(&prepared);
        let renamed_plan: BTreeMap<String, String> = prepared_plan(&renamed)
            .into_iter()
            .map(|(name, row)| (name, unrename(&row, &back)))
            .collect();
        let mut expected: Vec<&String> = map.values().collect();
        let mut got: Vec<&String> = renamed_plan.keys().filter(|n| !is_synthetic(n)).collect();
        expected.sort();
        got.sort();
        if got != expected {
            failures.push(format!(
                "{label}: the renaming did not rename rule for rule: {expected:?} became {got:?}"
            ));
            continue;
        }
        for (name, opaque) in &map {
            if plan[name] != renamed_plan[opaque] {
                failures.push(format!(
                    "{label}: rule `{name}` is planned differently as `{opaque}`: {} vs {}",
                    plan[name], renamed_plan[opaque]
                ));
            }
        }
        let synthetic = |plan: &BTreeMap<String, String>| {
            let mut rows: Vec<String> = plan
                .iter()
                .filter(|(name, _)| is_synthetic(name))
                .map(|(name, row)| erase_counters(&format!("{} {row}", unrename(name, &back))))
                .collect();
            rows.sort();
            rows
        };
        let (own_rows, renamed_rows) = (synthetic(&plan), synthetic(&renamed_plan));
        if own_rows != renamed_rows {
            failures.push(format!(
                "{label}: the synthesized rules are planned differently: {:?} vs {:?}",
                own_rows.iter().find(|row| !renamed_rows.contains(row)),
                renamed_rows.iter().find(|row| !own_rows.contains(row)),
            ));
        }
        let ident = &subject.ident;
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
