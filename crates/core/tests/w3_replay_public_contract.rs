//! Permanent W3 public-witness and replay authority.
//!
//! This target deliberately compiles only `public/0001..=0003`. Candidate
//! material is treated as sealed bytes: its ledgers are closed, but its
//! grammars are never passed to the compiler or to generated code.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt as _;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use bbnf::compiler::{ReplayComponent, ReplayError, replay_compilation};
use bbnf::{CompileOptions, CompiledModule, GrammarInstanceInput, StageResult, compile_instance};
use serde::de::{self, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};

const PUBLIC_ORDINALS: [&str; 3] = ["0001", "0002", "0003"];
const CANDIDATE_ORDINALS: [&str; 5] = ["0001", "0002", "0003", "0004", "0005"];
const PUBLIC_IDENTITIES: [(&str, &str); 3] = [
    ("0001", "PUB-JSON-EVENT-V1"),
    ("0002", "PUB-CSS-EVENT-V1"),
    ("0003", "PUB-SHEETS-PRATT-EVENT-V1"),
];
const PUBLIC_ALTERNATE_ENTRIES: [(&str, &str); 3] =
    [("0001", "value"), ("0002", "stylesheet"), ("0003", "atom")];
const CANDIDATE_IDENTITIES: [(&str, &str); 5] = [
    ("0001", "BNF"),
    ("0002", "CSV"),
    ("0003", "G4"),
    ("0004", "MATH"),
    ("0005", "REGEX"),
];
const ADDENDUM: &str = "restart/skinny/tranches/sk-v26/artifacts/W0-PUBLIC-WITNESS-ADDENDUM.tsv";
const PUBLIC_CASES: &str = "restart/skinny/tranches/sk-v26/artifacts/W3-PUBLIC-WITNESS-CASES.tsv";
const CANDIDATE_PRESEAL: &str = "restart/skinny/tranches/sk-v26/artifacts/W3-CANDIDATE-PRESEAL.tsv";
const PACKET_README: &str = "contracts/w3-public-witnesses/README.md";

const ADDENDUM_SHA256: &str = "60ca85e297278643b89aa4817a5c333e8114bc3774037344d083e7080c5ecc1c";
const PUBLIC_CASES_SHA256: &str =
    "81f004e7211afc1b2166f40d1b53f9545590d86e95ab6f5cad6592c0e86a4816";
const CANDIDATE_PRESEAL_SHA256: &str =
    "66d8d40cb0e372e103bfca2e129cabe0f0bd5cd35d196b3c3d1f8408450539d8";
const PACKET_README_SHA256: &str =
    "94e62b0bc1a9ee80af5b9e45330605c3ca7cc310169e754146b75ee64f595c18";
const W3_PRESELECTION_BASIS_COMMIT: &str = "28461bf7cd8fa73c54aabf098a978ec321d6e665";

const CASE_HEADER: [&str; 8] = [
    "case_id",
    "witness_id",
    "class",
    "input_path",
    "input_sha256",
    "expected_path",
    "expected_sha256",
    "disposition",
];

const GENERIC_BASELINE_SOURCE: &[u8] = br#"
document = open, payload, close;
open = "<";
payload = "x";
close = ">";
"#;

const GENERIC_PERMUTED_SOURCE: &[u8] = br#"
payload = "x";
close = ">";
document = open, payload, close;
open = "<";
"#;

const GENERIC_RENAMED_SOURCE: &[u8] = br#"
terminator = ">";
renamed_entry = initiator, content, terminator;
content = "x";
initiator = "<";
"#;

const MISLEADING_JSON_SOURCE: &[u8] = br#"
json_css_document = format_open, format_payload, format_close;
format_open = "<";
format_payload = "v";
format_close = ">";
"#;

const MISLEADING_CSS_SOURCE: &[u8] = br#"
json_css_document = format_open, format_payload, format_close;
format_open = "[";
format_payload = "v";
format_close = "]";
"#;

const NEUTRAL_FULL_W3_SOURCE: &[u8] = br#"
// Neutral, format-free genericity witness.
document = [ prefix ], body, { suffix } ?w;
prefix = "!";
suffix = "~";
body = projected | live_difference | expression;

/* Projection and Difference are both reachable from document. */
projected = left >> kept;
left = "l";
kept = "k";
live_difference = included - excluded;
included = "i" | "x";
excluded = "x";

expression = atom;
atom = number | "(" ?w, expression, ")" ?w;
number = /(?:0|[1-9][0-9]*)/ ?w;
plus = "+" ?w;

@pretty document group block indent dedent softline hardline blankline compact sep(", ");
"#;

#[derive(Clone, Copy)]
struct SyntheticControl {
    module: &'static str,
    id: &'static str,
    logical_path: &'static str,
    entry: &'static str,
    source: &'static [u8],
    pratt: bool,
    cases: &'static [SyntheticCase],
}

#[derive(Clone, Copy)]
struct SyntheticCase {
    id: &'static str,
    input: &'static [u8],
    outcome: SyntheticOutcome,
    cursor_after: SyntheticCursorAfter,
    observations: &'static [SyntheticObservation],
}

#[derive(Clone, Copy)]
enum SyntheticOutcome {
    Match,
    Mismatch,
}

#[derive(Clone, Copy)]
enum SyntheticCursorAfter {
    Entry,
    EndInput,
}

#[derive(Clone, Copy)]
enum SyntheticObservation {
    Token {
        symbol: &'static str,
        lexeme: &'static [u8],
    },
    Trivia(&'static [u8]),
    PrattReduce {
        fixity: &'static str,
        matcher_rule: &'static str,
        reduced: &'static [u8],
    },
    EndInput,
}

const ANGLE_X_OBSERVATIONS: &[SyntheticObservation] = &[
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"<",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"x",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b">",
    },
    SyntheticObservation::EndInput,
];

const ANGLE_X_CASES: &[SyntheticCase] = &[
    SyntheticCase {
        id: "SYNTHETIC-ANGLE-X-MATCH",
        input: b"<x>",
        outcome: SyntheticOutcome::Match,
        cursor_after: SyntheticCursorAfter::EndInput,
        observations: ANGLE_X_OBSERVATIONS,
    },
    SyntheticCase {
        id: "SYNTHETIC-ANGLE-X-TWIN-MISMATCH",
        input: b"[x]",
        outcome: SyntheticOutcome::Mismatch,
        cursor_after: SyntheticCursorAfter::Entry,
        observations: &[],
    },
];

const ANGLE_V_OBSERVATIONS: &[SyntheticObservation] = &[
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"<",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"v",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b">",
    },
    SyntheticObservation::EndInput,
];

const ANGLE_V_CASES: &[SyntheticCase] = &[
    SyntheticCase {
        id: "SYNTHETIC-SAME-LABEL-ANGLE-MATCH",
        input: b"<v>",
        outcome: SyntheticOutcome::Match,
        cursor_after: SyntheticCursorAfter::EndInput,
        observations: ANGLE_V_OBSERVATIONS,
    },
    SyntheticCase {
        id: "SYNTHETIC-SAME-LABEL-SQUARE-MISMATCH",
        input: b"[v]",
        outcome: SyntheticOutcome::Mismatch,
        cursor_after: SyntheticCursorAfter::Entry,
        observations: &[],
    },
];

const SQUARE_V_OBSERVATIONS: &[SyntheticObservation] = &[
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"[",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"v",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"]",
    },
    SyntheticObservation::EndInput,
];

const SQUARE_V_CASES: &[SyntheticCase] = &[
    SyntheticCase {
        id: "SYNTHETIC-SAME-LABEL-SQUARE-MATCH",
        input: b"[v]",
        outcome: SyntheticOutcome::Match,
        cursor_after: SyntheticCursorAfter::EndInput,
        observations: SQUARE_V_OBSERVATIONS,
    },
    SyntheticCase {
        id: "SYNTHETIC-SAME-LABEL-ANGLE-MISMATCH",
        input: b"<v>",
        outcome: SyntheticOutcome::Mismatch,
        cursor_after: SyntheticCursorAfter::Entry,
        observations: &[],
    },
];

const FULL_PROJECTION_OBSERVATIONS: &[SyntheticObservation] = &[
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"!",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"l",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"k",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"~",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"~",
    },
    SyntheticObservation::Trivia(b" "),
    SyntheticObservation::EndInput,
];

const FULL_DIFFERENCE_OBSERVATIONS: &[SyntheticObservation] = &[
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"i",
    },
    SyntheticObservation::EndInput,
];

const FULL_PRATT_OBSERVATIONS: &[SyntheticObservation] = &[
    SyntheticObservation::Token {
        symbol: "regex",
        lexeme: b"1",
    },
    SyntheticObservation::Token {
        symbol: "literal",
        lexeme: b"+",
    },
    SyntheticObservation::Token {
        symbol: "regex",
        lexeme: b"2",
    },
    SyntheticObservation::PrattReduce {
        fixity: "infix",
        matcher_rule: "plus",
        reduced: b"1+2",
    },
    SyntheticObservation::EndInput,
];

const FULL_W3_CASES: &[SyntheticCase] = &[
    SyntheticCase {
        id: "SYNTHETIC-NEUTRAL-PROJECTION-BRACKET-BRACE-TRIM-MATCH",
        input: b"!lk~~ ",
        outcome: SyntheticOutcome::Match,
        cursor_after: SyntheticCursorAfter::EndInput,
        observations: FULL_PROJECTION_OBSERVATIONS,
    },
    SyntheticCase {
        id: "SYNTHETIC-NEUTRAL-DIFFERENCE-MATCH",
        input: b"i",
        outcome: SyntheticOutcome::Match,
        cursor_after: SyntheticCursorAfter::EndInput,
        observations: FULL_DIFFERENCE_OBSERVATIONS,
    },
    SyntheticCase {
        id: "SYNTHETIC-NEUTRAL-PRATT-MATCH",
        input: b"1+2",
        outcome: SyntheticOutcome::Match,
        cursor_after: SyntheticCursorAfter::EndInput,
        observations: FULL_PRATT_OBSERVATIONS,
    },
    SyntheticCase {
        id: "SYNTHETIC-NEUTRAL-DIFFERENCE-EXCLUSION-MISMATCH",
        input: b"x",
        outcome: SyntheticOutcome::Mismatch,
        cursor_after: SyntheticCursorAfter::Entry,
        observations: &[],
    },
];

const SYNTHETIC_CONTROLS: [SyntheticControl; 6] = [
    SyntheticControl {
        module: "c_0001",
        id: "GENERIC-BASELINE",
        logical_path: "synthetic/baseline/no-format.data.bbnf",
        entry: "document",
        source: GENERIC_BASELINE_SOURCE,
        pratt: false,
        cases: ANGLE_X_CASES,
    },
    SyntheticControl {
        module: "c_0002",
        id: "GENERIC-RULE-ORDER-PERMUTATION",
        logical_path: "synthetic/permuted/no-format.data.bbnf",
        entry: "document",
        source: GENERIC_PERMUTED_SOURCE,
        pratt: false,
        cases: ANGLE_X_CASES,
    },
    SyntheticControl {
        module: "c_0003",
        id: "GENERIC-FULL-RULE-AND-PATH-RENAME",
        logical_path: "synthetic/wholly-renamed/unknown.extension.bbnf",
        entry: "renamed_entry",
        source: GENERIC_RENAMED_SOURCE,
        pratt: false,
        cases: ANGLE_X_CASES,
    },
    SyntheticControl {
        module: "c_0004",
        id: "SAME-LABEL-ANGLE-DELIMITER-TWIN",
        logical_path: "synthetic/shared/catalog.json.css.bbnf",
        entry: "json_css_document",
        source: MISLEADING_JSON_SOURCE,
        pratt: false,
        cases: ANGLE_V_CASES,
    },
    SyntheticControl {
        module: "c_0005",
        id: "SAME-LABEL-SQUARE-DELIMITER-TWIN",
        logical_path: "synthetic/shared/catalog.json.css.bbnf",
        entry: "json_css_document",
        source: MISLEADING_CSS_SOURCE,
        pratt: false,
        cases: SQUARE_V_CASES,
    },
    SyntheticControl {
        module: "c_0006",
        id: "NEUTRAL-REACHABLE-FULL-W3-FEATURES",
        logical_path: "synthetic/neutral/no-format.identity.bbnf",
        entry: "document",
        source: NEUTRAL_FULL_W3_SOURCE,
        pratt: true,
        cases: FULL_W3_CASES,
    },
];

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("core crate has the workspace root as its grandparent")
        .to_path_buf()
}

fn bytes(relative: &str) -> Vec<u8> {
    fs::read(root().join(relative)).unwrap_or_else(|error| panic!("{relative}: {error}"))
}

struct StrictValueSeed;

impl<'de> DeserializeSeed<'de> for StrictValueSeed {
    type Value = Value;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(StrictValueVisitor)
    }
}

struct StrictValueVisitor;

impl<'de> Visitor<'de> for StrictValueVisitor {
    type Value = Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("a JSON value with unique object keys")
    }

    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        Ok(Value::Bool(value))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E> {
        Ok(Value::Number(value.into()))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
        Ok(Value::Number(value.into()))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        serde_json::Number::from_f64(value)
            .map(Value::Number)
            .ok_or_else(|| E::custom("non-finite JSON number"))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> {
        Ok(Value::String(value.to_owned()))
    }

    fn visit_string<E>(self, value: String) -> Result<Self::Value, E> {
        Ok(Value::String(value))
    }

    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(Value::Null)
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(Value::Null)
    }

    fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(sequence.size_hint().unwrap_or(0));
        while let Some(value) = sequence.next_element_seed(StrictValueSeed)? {
            values.push(value);
        }
        Ok(Value::Array(values))
    }

    fn visit_map<A>(self, mut object: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut values = Map::new();
        while let Some(key) = object.next_key::<String>()? {
            if values.contains_key(&key) {
                return Err(de::Error::custom(format!(
                    "duplicate JSON object key `{key}`"
                )));
            }
            let value = object.next_value_seed(StrictValueSeed)?;
            values.insert(key, value);
        }
        Ok(Value::Object(values))
    }
}

fn strict_json_bytes(input: &[u8], at: &str) -> Result<Value, String> {
    let mut deserializer = serde_json::Deserializer::from_slice(input);
    let value = StrictValueSeed
        .deserialize(&mut deserializer)
        .map_err(|error| format!("{at}: invalid JSON: {error}"))?;
    deserializer
        .end()
        .map_err(|error| format!("{at}: trailing JSON: {error}"))?;
    Ok(value)
}

fn json_file(relative: &str) -> Value {
    strict_json_bytes(&bytes(relative), relative).unwrap_or_else(|error| panic!("{error}"))
}

#[test]
fn packet_json_decoder_rejects_duplicate_keys_at_every_depth() {
    assert_eq!(
        strict_json_bytes(br#"{"a":[true,{"b":2}],"c":null}"#, "valid").unwrap(),
        json!({"a": [true, {"b": 2}], "c": null})
    );
    for (label, input) in [
        ("top-level duplicate", br#"{"a":1,"a":2}"#.as_slice()),
        ("nested duplicate", br#"{"a":[{"b":1,"b":2}]}"#.as_slice()),
    ] {
        let error = strict_json_bytes(input, label).expect_err(label);
        assert!(
            error.contains("duplicate JSON object key"),
            "{label}: exact duplicate-key rejection: {error}"
        );
    }
}

fn discovered_public_ordinals() -> Vec<String> {
    let public_root = root().join("contracts/w3-public-witnesses/public");
    let mut packet_ordinals: Vec<String> = fs::read_dir(&public_root)
        .expect("public packet directory")
        .map(|entry| entry.expect("public ordinal entry"))
        .map(|entry| {
            assert!(
                entry.file_type().expect("public ordinal type").is_dir(),
                "{}: public root contains only ordinal directories",
                entry.path().display()
            );
            entry
                .file_name()
                .into_string()
                .expect("public ordinal is UTF-8")
        })
        .collect();
    packet_ordinals.sort();
    assert!(
        packet_ordinals
            .iter()
            .all(|ordinal| ordinal.len() == 4 && ordinal.bytes().all(|byte| byte.is_ascii_digit())),
        "public ordinal directory names are exact four-digit lexical identities"
    );

    let addendum_header = [
        "witness_id",
        "grammar_path",
        "grammar_sha256",
        "instance_path",
        "instance_sha256",
        "semantic_plane",
        "feature_id",
        "case_ledger_path",
        "case_ledger_sha256",
        "recovery_policy",
        "disposition",
    ];
    let authority_ordinals: BTreeSet<String> = read_tsv(ADDENDUM, &addendum_header)
        .into_iter()
        .map(|row| {
            let prefix = "contracts/w3-public-witnesses/public/";
            let suffix = "/grammar.bbnf";
            row[1]
                .strip_prefix(prefix)
                .and_then(|path| path.strip_suffix(suffix))
                .unwrap_or_else(|| panic!("{}: malformed public grammar authority", row[1]))
                .to_owned()
        })
        .collect();
    assert_eq!(
        packet_ordinals.iter().cloned().collect::<BTreeSet<_>>(),
        authority_ordinals,
        "public ordinal directories equal the sealed grammar authority inventory"
    );
    for ordinal in &packet_ordinals {
        let base = format!("contracts/w3-public-witnesses/public/{ordinal}");
        let manifest = json_file(&format!("{base}/instance.json"));
        assert_eq!(
            manifest["logical_path"].as_str(),
            Some(format!("{base}/grammar.bbnf").as_str()),
            "public/{ordinal}: discovered manifest/source authority"
        );
        assert!(root().join(format!("{base}/cases.tsv")).is_file());
    }
    packet_ordinals
}

fn packet_file_inventory() -> BTreeSet<String> {
    let mut files = BTreeSet::from(["README.md".to_owned()]);
    for (class, ordinals, has_grammar) in [
        ("public", PUBLIC_ORDINALS.as_slice(), true),
        ("candidates", CANDIDATE_ORDINALS.as_slice(), false),
    ] {
        for ordinal in ordinals {
            let base = format!("{class}/{ordinal}");
            files.insert(format!("{base}/instance.json"));
            files.insert(format!("{base}/cases.tsv"));
            if has_grammar {
                files.insert(format!("{base}/grammar.bbnf"));
            }
            for case in 1..=4 {
                files.insert(format!("{base}/cases/{case:04}.input.json"));
                files.insert(format!("{base}/cases/{case:04}.expected.json"));
            }
        }
    }
    files
}

fn packet_directory_inventory() -> BTreeSet<String> {
    let mut directories =
        BTreeSet::from([".".to_owned(), "public".to_owned(), "candidates".to_owned()]);
    for (class, ordinals) in [
        ("public", PUBLIC_ORDINALS.as_slice()),
        ("candidates", CANDIDATE_ORDINALS.as_slice()),
    ] {
        for ordinal in ordinals {
            directories.insert(format!("{class}/{ordinal}"));
            directories.insert(format!("{class}/{ordinal}/cases"));
        }
    }
    directories
}

fn walk_packet(
    packet_root: &Path,
    path: &Path,
    files: &mut BTreeSet<String>,
    directories: &mut BTreeSet<String>,
) {
    let metadata = fs::symlink_metadata(path)
        .unwrap_or_else(|error| panic!("{}: packet metadata: {error}", path.display()));
    assert!(
        !metadata.file_type().is_symlink(),
        "{}: packet symlink is forbidden",
        path.display()
    );
    let relative = path
        .strip_prefix(packet_root)
        .expect("packet walk remains below root");
    let relative = if relative.as_os_str().is_empty() {
        ".".to_owned()
    } else {
        relative
            .to_str()
            .expect("packet paths are UTF-8")
            .replace('\\', "/")
    };
    if metadata.is_dir() {
        assert!(
            directories.insert(relative),
            "{}: duplicate packet directory",
            path.display()
        );
        let mut children: Vec<PathBuf> = fs::read_dir(path)
            .unwrap_or_else(|error| panic!("{}: packet read_dir: {error}", path.display()))
            .map(|entry| entry.expect("packet directory entry").path())
            .collect();
        children.sort();
        for child in children {
            walk_packet(packet_root, &child, files, directories);
        }
    } else {
        assert!(
            metadata.is_file(),
            "{}: packet entry is not a regular file",
            path.display()
        );
        #[cfg(unix)]
        assert_eq!(
            metadata.permissions().mode() & 0o111,
            0,
            "{}: packet file must not be executable",
            path.display()
        );
        assert!(
            files.insert(relative),
            "{}: duplicate packet file",
            path.display()
        );
    }
}

fn is_checked_in_generated_module(name: &str) -> bool {
    name.len() == 9
        && (name.starts_with("g_") || name.starts_with("c_"))
        && name.ends_with(".rs")
        && name.as_bytes()[2..6].iter().all(u8::is_ascii_digit)
}

fn immutable_git_object_reader(repository: &Path) -> Command {
    let mut command = Command::new("git");
    command
        .env("GIT_NO_REPLACE_OBJECTS", "1")
        .current_dir(repository);
    command
}

fn preselection_basis_bytes(display: &str) -> Option<Vec<u8>> {
    let output = immutable_git_object_reader(&root())
        .args(["show", &format!("{W3_PRESELECTION_BASIS_COMMIT}:{display}")])
        .output()
        .unwrap_or_else(|error| panic!("read preselection-basis manifest `{display}`: {error}"));
    output.status.success().then_some(output.stdout)
}

fn normalized_rust_identifier(value: &str) -> &str {
    value.strip_prefix("r#").unwrap_or(value)
}

fn normalized_syn_path(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|segment| normalized_rust_identifier(&segment.ident.to_string()).to_owned())
        .collect::<Vec<_>>()
        .join("::")
}

fn owned_runtime_package(name: &str) -> bool {
    matches!(name, "bbnf" | "parse_that" | "parse-that")
}

fn normalize_repository_relative(path: &Path) -> Option<String> {
    use std::path::Component;

    let mut normalized = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::Normal(component) => {
                normalized.push(component.to_str()?.to_owned());
            }
            Component::ParentDir => {
                normalized.pop()?;
            }
            Component::Prefix(_) | Component::RootDir => return None,
        }
    }
    Some(normalized.join("/"))
}

fn manifest_dependency_rows(manifest: &toml::Value) -> Vec<(&str, &toml::Value)> {
    fn append<'a>(
        table: Option<&'a toml::map::Map<String, toml::Value>>,
        rows: &mut Vec<(&'a str, &'a toml::Value)>,
    ) {
        for section in ["dependencies", "build-dependencies", "dev-dependencies"] {
            if let Some(dependencies) = table
                .and_then(|table| table.get(section))
                .and_then(toml::Value::as_table)
            {
                rows.extend(
                    dependencies
                        .iter()
                        .map(|(name, dependency)| (name.as_str(), dependency)),
                );
            }
        }
    }

    let mut rows = Vec::new();
    append(manifest.as_table(), &mut rows);
    if let Some(targets) = manifest.get("target").and_then(toml::Value::as_table) {
        for target in targets.values() {
            append(target.as_table(), &mut rows);
        }
    }
    rows
}

fn workspace_manifest<'a>(
    display: &str,
    manifest: &toml::Value,
    manifests: &'a BTreeMap<String, toml::Value>,
) -> Option<&'a toml::Value> {
    let manifest_path = Path::new(display);
    let manifest_directory = manifest_path.parent().unwrap_or_else(|| Path::new(""));
    if let Some(explicit) = manifest
        .get("package")
        .and_then(toml::Value::as_table)
        .and_then(|package| package.get("workspace"))
        .and_then(toml::Value::as_str)
    {
        let workspace =
            normalize_repository_relative(&manifest_directory.join(explicit).join("Cargo.toml"))?;
        return manifests.get(&workspace);
    }

    let mut directory = Some(manifest_directory);
    while let Some(candidate_directory) = directory {
        let candidate = normalize_repository_relative(&candidate_directory.join("Cargo.toml"))?;
        if manifests
            .get(&candidate)
            .is_some_and(|candidate| candidate.get("workspace").is_some())
        {
            return manifests.get(&candidate);
        }
        directory = candidate_directory.parent();
    }
    None
}

fn dependency_actual_package<'a>(
    declared_name: &'a str,
    dependency: &'a toml::Value,
    workspace: Option<&'a toml::Value>,
) -> &'a str {
    let Some(specification) = dependency.as_table() else {
        return declared_name;
    };
    if let Some(package) = specification.get("package").and_then(toml::Value::as_str) {
        return package;
    }
    if specification
        .get("workspace")
        .and_then(toml::Value::as_bool)
        == Some(true)
        && let Some(workspace_dependency) = workspace
            .and_then(|workspace| workspace.get("workspace"))
            .and_then(toml::Value::as_table)
            .and_then(|workspace| workspace.get("dependencies"))
            .and_then(toml::Value::as_table)
            .and_then(|dependencies| dependencies.get(declared_name))
    {
        return workspace_dependency
            .as_table()
            .and_then(|dependency| dependency.get("package"))
            .and_then(toml::Value::as_str)
            .unwrap_or(declared_name);
    }
    declared_name
}

fn declares_owned_runtime_dependency(
    display: &str,
    manifest: &toml::Value,
    manifests: &BTreeMap<String, toml::Value>,
) -> bool {
    let workspace = workspace_manifest(display, manifest, manifests);
    manifest_dependency_rows(manifest)
        .into_iter()
        .any(|(declared_name, dependency)| {
            let actual = dependency_actual_package(declared_name, dependency, workspace);
            if owned_runtime_package(actual) {
                return true;
            }
            dependency.as_table().is_some_and(|specification| {
                specification
                    .get("workspace")
                    .and_then(toml::Value::as_bool)
                    == Some(true)
            }) && manifests.values().any(|candidate| {
                candidate
                    .get("workspace")
                    .and_then(toml::Value::as_table)
                    .and_then(|workspace| workspace.get("dependencies"))
                    .and_then(toml::Value::as_table)
                    .and_then(|dependencies| dependencies.get(declared_name))
                    .is_some_and(|workspace_dependency| {
                        owned_runtime_package(dependency_actual_package(
                            declared_name,
                            workspace_dependency,
                            None,
                        ))
                    })
            })
        })
}

fn is_active_w3_manifest_authority(base: &Path, display: &str, manifest: &toml::Value) -> bool {
    base == root()
        && display == "crates/core/Cargo.toml"
        && manifest["package"]["name"].as_str() == Some("bbnf")
        && manifest["package"]["version"].as_str() == Some("0.4.0")
        && manifest["lib"]["name"].as_str() == Some("bbnf")
        && manifest["lib"]["path"].as_str() == Some("src/lib.rs")
        && manifest["package"].get("build").is_none()
        && manifest["package"].get("links").is_none()
}

fn preselection_basis_paths(directory: &str) -> Option<Vec<String>> {
    let output = immutable_git_object_reader(&root())
        .args([
            "ls-tree",
            "-r",
            "-z",
            "--name-only",
            W3_PRESELECTION_BASIS_COMMIT,
            "--",
            directory,
        ])
        .output()
        .unwrap_or_else(|error| panic!("list preselection-basis directory `{directory}`: {error}"));
    if !output.status.success() {
        return None;
    }
    let paths = output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|path| !path.is_empty())
        .map(|path| {
            std::str::from_utf8(path)
                .expect("preselection-basis path is UTF-8")
                .to_owned()
        })
        .collect::<Vec<_>>();
    (!paths.is_empty()).then_some(paths)
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct FrozenConsumerEntry {
    mode: String,
    kind: String,
    object: String,
    size: Option<u64>,
}

#[derive(Debug, Eq, PartialEq)]
struct CurrentConsumerEntry {
    mode: String,
    kind: &'static str,
    bytes: Option<Vec<u8>>,
}

fn frozen_consumer_entries(
    display_directory: &str,
) -> Option<BTreeMap<String, FrozenConsumerEntry>> {
    let output = immutable_git_object_reader(&root())
        .args([
            "ls-tree",
            "-r",
            "-t",
            "-z",
            "-l",
            "--full-tree",
            W3_PRESELECTION_BASIS_COMMIT,
            "--",
            display_directory,
        ])
        .output()
        .unwrap_or_else(|error| panic!("list frozen consumer tree `{display_directory}`: {error}"));
    if !output.status.success() {
        return None;
    }

    let prefix = format!("{display_directory}/");
    let mut unfiltered = BTreeMap::new();
    for row in output.stdout.split(|byte| *byte == 0) {
        if row.is_empty() {
            continue;
        }
        let tab = row
            .iter()
            .position(|byte| *byte == b'\t')
            .expect("frozen consumer tree row has a path separator");
        let metadata =
            std::str::from_utf8(&row[..tab]).expect("frozen consumer tree metadata is UTF-8");
        let fields = metadata.split_whitespace().collect::<Vec<_>>();
        assert_eq!(fields.len(), 4, "frozen consumer tree metadata");
        let path = std::str::from_utf8(&row[tab + 1..]).expect("frozen consumer path is UTF-8");
        let relative = if path == display_directory {
            ".".to_owned()
        } else if let Some(relative) = path.strip_prefix(&prefix) {
            relative.to_owned()
        } else {
            continue;
        };
        let entry = FrozenConsumerEntry {
            mode: fields[0].to_owned(),
            kind: fields[1].to_owned(),
            object: fields[2].to_owned(),
            size: (fields[3] != "-").then(|| {
                fields[3]
                    .parse()
                    .expect("frozen consumer blob size is an integer")
            }),
        };
        assert!(
            unfiltered.insert(relative.clone(), entry).is_none(),
            "{display_directory}: duplicate frozen consumer entry `{relative}`"
        );
    }
    let inert_directories = unfiltered
        .iter()
        .filter(|(path, entry)| {
            (entry.kind == "tree" || entry.mode == "120000")
                && path.as_str() != "."
                && Path::new(path)
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| matches!(name, ".git" | "target" | "node_modules"))
        })
        .map(|(path, _)| path.clone())
        .collect::<Vec<_>>();
    let entries = unfiltered
        .into_iter()
        .filter(|(path, _)| {
            !inert_directories.iter().any(|inert| {
                path == inert
                    || path
                        .strip_prefix(inert)
                        .is_some_and(|suffix| suffix.starts_with('/'))
            })
        })
        .collect::<BTreeMap<_, _>>();
    entries.contains_key(".").then_some(entries)
}

fn frozen_consumer_blobs(
    entries: &BTreeMap<String, FrozenConsumerEntry>,
) -> BTreeMap<String, Vec<u8>> {
    let objects = entries
        .values()
        .filter(|entry| entry.kind == "blob")
        .map(|entry| entry.object.clone())
        .collect::<BTreeSet<_>>();
    let mut child = immutable_git_object_reader(&root())
        .args(["cat-file", "--batch"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn frozen consumer blob batch");
    let mut stdin = child
        .stdin
        .take()
        .expect("frozen consumer blob batch stdin");
    let requested = objects.clone();
    let writer = std::thread::spawn(move || {
        for object in &objects {
            std::io::Write::write_all(&mut stdin, object.as_bytes())
                .expect("write frozen consumer object");
            std::io::Write::write_all(&mut stdin, b"\n").expect("terminate frozen consumer object");
        }
    });

    let stdout = child
        .stdout
        .take()
        .expect("frozen consumer blob batch stdout");
    let mut reader = std::io::BufReader::new(stdout);
    let mut blobs = BTreeMap::new();
    for object in requested {
        let mut header = String::new();
        std::io::BufRead::read_line(&mut reader, &mut header)
            .expect("read frozen consumer blob header");
        let fields = header.split_whitespace().collect::<Vec<_>>();
        assert_eq!(fields.len(), 3, "{object}: frozen consumer blob header");
        assert_eq!(
            fields[0], object,
            "{object}: frozen consumer object identity"
        );
        assert_eq!(fields[1], "blob", "{object}: frozen consumer object kind");
        let size: usize = fields[2]
            .parse()
            .expect("frozen consumer blob size is an integer");
        let mut contents = vec![0; size];
        std::io::Read::read_exact(&mut reader, &mut contents)
            .expect("read frozen consumer blob bytes");
        let mut terminator = [0];
        std::io::Read::read_exact(&mut reader, &mut terminator)
            .expect("read frozen consumer blob terminator");
        assert_eq!(terminator, [b'\n'], "{object}: blob batch terminator");
        assert!(
            blobs.insert(object.clone(), contents).is_none(),
            "{object}: duplicate frozen consumer blob"
        );
    }
    writer.join().expect("join frozen consumer blob writer");
    let status = child.wait().expect("wait for frozen consumer blob batch");
    assert!(status.success(), "frozen consumer blob batch succeeds");
    blobs
}

fn current_consumer_entries(
    actual_directory: &Path,
) -> Option<BTreeMap<String, CurrentConsumerEntry>> {
    fn link_bytes(path: &Path) -> Option<Vec<u8>> {
        let target = fs::read_link(path).ok()?;
        #[cfg(unix)]
        {
            use std::os::unix::ffi::OsStrExt as _;
            Some(target.as_os_str().as_bytes().to_vec())
        }
        #[cfg(not(unix))]
        {
            Some(target.to_str()?.as_bytes().to_vec())
        }
    }

    fn file_mode(metadata: &fs::Metadata) -> String {
        #[cfg(unix)]
        {
            if metadata.permissions().mode() & 0o111 == 0 {
                "100644".to_owned()
            } else {
                "100755".to_owned()
            }
        }
        #[cfg(not(unix))]
        {
            "100644".to_owned()
        }
    }

    fn walk(
        directory: &Path,
        path: &Path,
        entries: &mut BTreeMap<String, CurrentConsumerEntry>,
    ) -> Option<()> {
        let metadata = fs::symlink_metadata(path).ok()?;
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        if path != directory
            && (metadata.is_dir() || metadata.file_type().is_symlink())
            && matches!(name, ".git" | "target" | "node_modules")
        {
            return Some(());
        }
        let relative = path.strip_prefix(directory).ok()?;
        let relative = if relative.as_os_str().is_empty() {
            ".".to_owned()
        } else {
            relative.to_str()?.replace('\\', "/")
        };
        let entry = if metadata.file_type().is_symlink() {
            CurrentConsumerEntry {
                mode: "120000".to_owned(),
                kind: "blob",
                bytes: Some(link_bytes(path)?),
            }
        } else if metadata.is_file() {
            CurrentConsumerEntry {
                mode: file_mode(&metadata),
                kind: "blob",
                bytes: Some(fs::read(path).ok()?),
            }
        } else if metadata.is_dir() {
            CurrentConsumerEntry {
                mode: "040000".to_owned(),
                kind: "tree",
                bytes: None,
            }
        } else {
            return None;
        };
        if entries.insert(relative, entry).is_some() {
            return None;
        }
        if metadata.is_dir() {
            let mut children = fs::read_dir(path)
                .ok()?
                .map(|entry| entry.ok().map(|entry| entry.path()))
                .collect::<Option<Vec<_>>>()?;
            children.sort();
            for child in children {
                walk(directory, &child, entries)?;
            }
        }
        Some(())
    }

    let mut entries = BTreeMap::new();
    walk(actual_directory, actual_directory, &mut entries)?;
    Some(entries)
}

fn fixture_consumer_matches_basis(actual_directory: &Path, display_directory: &str) -> bool {
    let Some(frozen) = frozen_consumer_entries(display_directory) else {
        return false;
    };
    let Some(current) = current_consumer_entries(actual_directory) else {
        return false;
    };
    if frozen.keys().ne(current.keys()) {
        return false;
    }
    let blobs = frozen_consumer_blobs(&frozen);
    frozen.iter().all(|(path, expected)| {
        let actual = &current[path];
        if expected.mode != actual.mode || expected.kind != actual.kind {
            return false;
        }
        match (&actual.bytes, expected.kind.as_str()) {
            (None, "tree") => expected.size.is_none(),
            (Some(bytes), "blob") => {
                expected.size == Some(bytes.len() as u64)
                    && blobs
                        .get(&expected.object)
                        .is_some_and(|frozen| frozen == bytes)
            }
            _ => false,
        }
    })
}

fn immutable_preselection_consumer(_base: &Path, path: &Path, display: &str) -> bool {
    if preselection_basis_bytes(display).is_none() {
        return false;
    }
    let display_directory = Path::new(display)
        .parent()
        .and_then(Path::to_str)
        .unwrap_or("");
    !display_directory.is_empty()
        && fixture_consumer_matches_basis(
            path.parent().expect("preselection manifest parent"),
            display_directory,
        )
}

#[derive(Default)]
struct CandidateAuthorityInventory {
    expected: BTreeMap<String, String>,
    by_len: BTreeMap<u64, BTreeMap<String, BTreeSet<String>>>,
    basis_symlinks: BTreeMap<String, Vec<u8>>,
    materialized_symlinks: BTreeMap<String, Vec<u8>>,
}

struct PreselectionBasisEntry {
    mode: String,
    object: String,
    size: Option<u64>,
    path: String,
}

fn preselection_path_is_ignored(path: &str) -> bool {
    Path::new(path).components().any(|component| {
        matches!(
            component.as_os_str().to_str(),
            Some(".git" | "target" | "node_modules")
        )
    })
}

fn preselection_basis_entries() -> Vec<PreselectionBasisEntry> {
    let output = immutable_git_object_reader(&root())
        .args([
            "ls-tree",
            "-r",
            "-z",
            "-l",
            "--full-tree",
            W3_PRESELECTION_BASIS_COMMIT,
        ])
        .output()
        .expect("list frozen preselection-basis tree");
    assert!(
        output.status.success(),
        "list frozen preselection-basis tree: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|entry| !entry.is_empty())
        .filter_map(|entry| {
            let tab = entry
                .iter()
                .position(|byte| *byte == b'\t')
                .expect("preselection-basis tree row has a path separator");
            let metadata = std::str::from_utf8(&entry[..tab])
                .expect("preselection-basis tree metadata is UTF-8");
            let fields = metadata.split_whitespace().collect::<Vec<_>>();
            assert_eq!(fields.len(), 4, "preselection-basis tree metadata");
            if fields[1] != "blob" {
                return None;
            }
            Some(PreselectionBasisEntry {
                mode: fields[0].to_owned(),
                object: fields[2].to_owned(),
                size: (fields[3] != "-").then(|| {
                    fields[3]
                        .parse()
                        .expect("preselection-basis blob size is an integer")
                }),
                path: std::str::from_utf8(&entry[tab + 1..])
                    .expect("preselection-basis path is UTF-8")
                    .to_owned(),
            })
        })
        .filter(|entry| !preselection_path_is_ignored(&entry.path))
        .collect()
}

fn preselection_basis_blob(object: &str, path: &str) -> Vec<u8> {
    let output = immutable_git_object_reader(&root())
        .args(["cat-file", "blob", object])
        .output()
        .unwrap_or_else(|error| panic!("read preselection-basis blob `{path}`: {error}"));
    assert!(
        output.status.success(),
        "read preselection-basis blob `{path}`: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

fn assert_replacement_refs_cannot_rewrite_frozen_basis() {
    fn object_identity(repository: &Path, revision: &str) -> String {
        let output = command_output(
            Command::new("git")
                .args(["rev-parse", "--verify", revision])
                .current_dir(repository),
            &format!("resolve disposable replacement-ref object `{revision}`"),
        );
        std::str::from_utf8(&output.stdout)
            .expect("disposable replacement-ref object identity is UTF-8")
            .trim()
            .to_owned()
    }

    fn commit_fixture(repository: &Path, message: &str) {
        command_output(
            Command::new("git")
                .args(["add", "--all"])
                .current_dir(repository),
            "stage disposable replacement-ref fixture",
        );
        command_output(
            Command::new("git")
                .args([
                    "-c",
                    "user.name=SK-V26 contract",
                    "-c",
                    "user.email=sk-v26@example.invalid",
                    "-c",
                    "commit.gpgSign=false",
                    "-c",
                    "core.hooksPath=/dev/null",
                    "commit",
                    "--quiet",
                    "-m",
                    message,
                ])
                .current_dir(repository),
            "commit disposable replacement-ref fixture",
        );
    }

    let temporary = tempfile::tempdir().expect("disposable replacement-ref repository");
    let repository = temporary.path();
    command_output(
        Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(repository),
        "initialize disposable replacement-ref repository",
    );
    fs::write(repository.join("sealed-basis.txt"), b"sealed basis\n")
        .expect("write disposable sealed basis");
    commit_fixture(repository, "sealed basis");
    let basis_commit = object_identity(repository, "HEAD");
    let basis_tree = object_identity(repository, "HEAD^{tree}");

    let candidate_source = candidate_authority_inventory()
        .expected
        .keys()
        .find(|path| path.ends_with(".bbnf"))
        .expect("candidate grammar authority for replacement-ref fixture");
    let candidate_bytes = bytes(candidate_source);
    fs::write(repository.join("added-candidate.data"), &candidate_bytes)
        .expect("write replacement-ref candidate bytes");
    commit_fixture(repository, "replacement tree");
    let replacement_tree = object_identity(repository, "HEAD^{tree}");
    assert_ne!(
        basis_tree, replacement_tree,
        "the disposable replacement tree must add candidate bytes"
    );
    command_output(
        Command::new("git")
            .args(["replace", &basis_tree, &replacement_tree])
            .current_dir(repository),
        "install disposable replacement-tree ref",
    );

    let vulnerable_tree = command_output(
        Command::new("git")
            .args(["ls-tree", "-r", "--name-only", &basis_commit])
            .current_dir(repository),
        "demonstrate replacement-ref tree substitution",
    );
    assert!(
        std::str::from_utf8(&vulnerable_tree.stdout)
            .expect("replacement-ref tree listing is UTF-8")
            .lines()
            .any(|path| path == "added-candidate.data"),
        "an unsealed Git object read must demonstrate the replacement-tree attack"
    );
    let vulnerable_candidate = command_output(
        Command::new("git")
            .args(["show", &format!("{basis_commit}:added-candidate.data")])
            .current_dir(repository),
        "demonstrate replacement-ref candidate-byte injection",
    );
    assert_eq!(
        vulnerable_candidate.stdout, candidate_bytes,
        "the unsealed Git object read must expose exact added candidate bytes"
    );

    let sealed_tree = command_output(
        immutable_git_object_reader(repository).args([
            "ls-tree",
            "-r",
            "--name-only",
            &basis_commit,
        ]),
        "read replacement-ref-resistant frozen tree",
    );
    assert_eq!(
        std::str::from_utf8(&sealed_tree.stdout)
            .expect("sealed frozen-tree listing is UTF-8")
            .lines()
            .collect::<Vec<_>>(),
        ["sealed-basis.txt"],
        "the immutable object reader must retain the original frozen tree"
    );
    let sealed_candidate = immutable_git_object_reader(repository)
        .args(["show", &format!("{basis_commit}:added-candidate.data")])
        .output()
        .expect("probe replacement-ref-resistant candidate-byte read");
    assert!(
        !sealed_candidate.status.success() && sealed_candidate.stdout.is_empty(),
        "the immutable object reader must reject replacement-only candidate bytes"
    );
}

fn frozen_w0_materialized_symlinks() -> BTreeMap<String, Vec<u8>> {
    fn relative_target(target: &str, source: &str) -> String {
        let target_parent = Path::new(target)
            .parent()
            .expect("materialized target has a parent")
            .components()
            .map(|component| component.as_os_str().to_owned())
            .collect::<Vec<_>>();
        let source = Path::new(source)
            .components()
            .map(|component| component.as_os_str().to_owned())
            .collect::<Vec<_>>();
        let common = target_parent
            .iter()
            .zip(&source)
            .take_while(|(left, right)| left == right)
            .count();
        let mut relative = PathBuf::new();
        for _ in common..target_parent.len() {
            relative.push("..");
        }
        for component in &source[common..] {
            relative.push(component);
        }
        relative
            .to_str()
            .expect("materialized symlink target is UTF-8")
            .replace('\\', "/")
    }

    let authority = "restart/skinny/tranches/sk-v26/artifacts/W0-CORPUS-MATERIALIZATION.tsv";
    let data = preselection_basis_bytes(authority)
        .unwrap_or_else(|| panic!("{authority}: frozen preselection-basis authority"));
    assert!(data.ends_with(b"\n"), "{authority}: terminal LF");
    assert!(!data.contains(&b'\r'), "{authority}: CR is forbidden");
    let text = std::str::from_utf8(&data).expect("W0 corpus materialization authority is UTF-8");
    let mut lines = text.lines();
    assert_eq!(
        lines.next(),
        Some("repository\ttarget\tsource\tdisposition"),
        "{authority}: exact header"
    );
    let mut symlinks = BTreeMap::new();
    for (index, line) in lines.enumerate() {
        let fields = line.split('\t').collect::<Vec<_>>();
        assert_eq!(fields.len(), 4, "{authority}: row {} width", index + 2);
        if fields[0] != "bbnf-lang-skv26" {
            continue;
        }
        assert_eq!(
            fields[3],
            "IGNORED_SYMLINK_FROM_TRACKED_CONTENT_ADDRESSED_SOURCE",
            "{authority}: row {} disposition",
            index + 2
        );
        for (label, path) in [("target", fields[1]), ("source", fields[2])] {
            assert_eq!(
                normalize_repository_relative(Path::new(path)).as_deref(),
                Some(path),
                "{authority}: row {} {label} path is normalized",
                index + 2
            );
        }
        let target = relative_target(fields[1], fields[2]).into_bytes();
        assert!(
            symlinks.insert(fields[1].to_owned(), target).is_none(),
            "{authority}: duplicate BBNF materialized target {}",
            fields[1]
        );
    }
    assert_eq!(
        symlinks.len(),
        7,
        "{authority}: exact BBNF W0 materialized symlink count"
    );
    symlinks
}

fn candidate_authority_inventory() -> &'static CandidateAuthorityInventory {
    static INVENTORY: OnceLock<CandidateAuthorityInventory> = OnceLock::new();
    INVENTORY.get_or_init(|| {
        let rows = read_tsv(
            CANDIDATE_PRESEAL,
            &[
                "candidate_id",
                "ordinal",
                "grammar_path",
                "grammar_sha256",
                "instance_path",
                "instance_sha256",
                "case_ledger_path",
                "case_ledger_sha256",
                "case_classes",
                "recovery_policy",
                "execution_state",
                "disposition",
            ],
        );
        let mut inventory = CandidateAuthorityInventory::default();
        let mut register = |path: &str, hash: &str| {
            let authority = bytes(path);
            assert_eq!(sha256_hex(&authority), hash, "{path}: candidate authority");
            assert!(
                inventory
                    .expected
                    .insert(path.to_owned(), hash.to_owned())
                    .is_none(),
                "{path}: duplicate candidate authority path"
            );
            inventory
                .by_len
                .entry(authority.len() as u64)
                .or_default()
                .entry(hash.to_owned())
                .or_default()
                .insert(path.to_owned());
        };
        for row in rows {
            register(&row[2], &row[3]);
            register(&row[4], &row[5]);
            register(&row[6], &row[7]);
            for case in read_tsv(&row[6], &CASE_HEADER) {
                register(&case[3], &case[4]);
                register(&case[5], &case[6]);
            }
        }
        assert_eq!(
            inventory.expected.len(),
            CANDIDATE_ORDINALS.len() * 11,
            "every candidate grammar, instance, ledger, input, and expectation is inventoried"
        );

        let candidate_lengths: BTreeSet<u64> = inventory.by_len.keys().copied().collect();
        let candidate_hashes: BTreeSet<String> = inventory
            .by_len
            .values()
            .flat_map(|hashes| hashes.keys().cloned())
            .collect();
        for hashes in inventory.by_len.values_mut() {
            for paths in hashes.values_mut() {
                paths.clear();
            }
        }
        for entry in preselection_basis_entries() {
            if entry.mode == "120000" {
                let target = preselection_basis_blob(&entry.object, &entry.path);
                assert!(
                    inventory
                        .basis_symlinks
                        .insert(entry.path.clone(), target)
                        .is_none(),
                    "{}: duplicate preselection-basis symlink",
                    entry.path
                );
                continue;
            }
            if !matches!(entry.mode.as_str(), "100644" | "100755")
                || !entry
                    .size
                    .is_some_and(|size| candidate_lengths.contains(&size))
            {
                continue;
            }
            let contents = preselection_basis_blob(&entry.object, &entry.path);
            let hash = sha256_hex(&contents);
            if !candidate_hashes.contains(&hash) {
                continue;
            }
            let paths = inventory
                .by_len
                .get_mut(&(contents.len() as u64))
                .and_then(|hashes| hashes.get_mut(&hash))
                .expect("candidate hash retains its exact byte length");
            assert!(
                paths.insert(entry.path.clone()),
                "{}: duplicate candidate match in preselection-basis tree",
                entry.path
            );
        }
        for (path, hash) in &inventory.expected {
            assert!(
                inventory
                    .by_len
                    .values()
                    .any(|hashes| hashes.get(hash).is_some_and(|paths| paths.contains(path))),
                "{path}: canonical candidate authority is absent from the frozen preselection basis"
            );
        }
        inventory.materialized_symlinks = frozen_w0_materialized_symlinks();
        assert!(
            inventory
                .materialized_symlinks
                .keys()
                .all(|path| !inventory.basis_symlinks.contains_key(path)),
            "tracked historical and ignored W0 materialized symlink authorities are disjoint"
        );
        inventory
    })
}

fn materialize_preselection_basis_directory(display_directory: &str, destination: &Path) {
    let prefix = format!("{display_directory}/");
    for basis_path in preselection_basis_paths(display_directory)
        .unwrap_or_else(|| panic!("{display_directory}: preselection-basis directory"))
    {
        let relative = basis_path
            .strip_prefix(&prefix)
            .unwrap_or_else(|| panic!("{basis_path}: preselection-basis directory prefix"));
        let destination_path = destination.join(relative);
        fs::create_dir_all(
            destination_path
                .parent()
                .expect("preselection fixture file parent"),
        )
        .expect("create preselection fixture directory");
        fs::write(
            &destination_path,
            preselection_basis_bytes(&basis_path)
                .unwrap_or_else(|| panic!("{basis_path}: preselection-basis bytes")),
        )
        .unwrap_or_else(|error| {
            panic!(
                "{}: materialize preselection fixture: {error}",
                destination_path.display()
            )
        });
    }
}

fn preselection_findings(base: &Path) -> Vec<String> {
    struct PreselectionScan<'a> {
        base: &'a Path,
        manifests: &'a BTreeMap<String, toml::Value>,
        candidate_inventory: &'a CandidateAuthorityInventory,
        findings: Vec<String>,
        harnesses: Vec<String>,
        candidate_authorities_seen: BTreeSet<String>,
        basis_symlinks_seen: BTreeSet<String>,
    }

    fn collect_manifests(base: &Path, path: &Path, manifests: &mut BTreeMap<String, toml::Value>) {
        let relative = path
            .strip_prefix(base)
            .expect("manifest walk remains under its base");
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        if matches!(name, ".git" | "target" | "node_modules") {
            return;
        }
        let metadata = fs::symlink_metadata(path)
            .unwrap_or_else(|error| panic!("{}: manifest metadata: {error}", path.display()));
        if metadata.file_type().is_symlink() {
            return;
        }
        if metadata.is_dir() {
            let mut children: Vec<PathBuf> = fs::read_dir(path)
                .unwrap_or_else(|error| panic!("{}: manifest read_dir: {error}", path.display()))
                .map(|entry| entry.expect("manifest directory entry").path())
                .collect();
            children.sort();
            for child in children {
                collect_manifests(base, &child, manifests);
            }
        } else if metadata.is_file() && name == "Cargo.toml" {
            let display = relative.to_string_lossy().replace('\\', "/");
            let text = fs::read_to_string(path).unwrap_or_else(|error| {
                panic!("{}: Cargo manifest must be UTF-8: {error}", path.display())
            });
            let manifest = toml::from_str(&text)
                .unwrap_or_else(|error| panic!("{}: Cargo manifest TOML: {error}", path.display()));
            assert!(
                manifests.insert(display, manifest).is_none(),
                "{}: duplicate Cargo manifest",
                path.display()
            );
        }
    }

    fn inspect_file(scan: &mut PreselectionScan<'_>, path: &Path, display: &str, name: &str) {
        if is_checked_in_generated_module(name) {
            scan.findings
                .push(format!("checked-in generated module `{display}`"));
        }
        if name == "Cargo.toml" {
            let manifest = scan
                .manifests
                .get(display)
                .unwrap_or_else(|| panic!("{display}: discovered Cargo manifest"));
            let named_w3_runtime = manifest
                .get("package")
                .and_then(toml::Value::as_table)
                .and_then(|package| package.get("name"))
                .and_then(toml::Value::as_str)
                == Some("bbnf-w3-public-runtime");
            let owned_runtime_dependency =
                declares_owned_runtime_dependency(display, manifest, scan.manifests);
            if !is_active_w3_manifest_authority(scan.base, display, manifest)
                && (named_w3_runtime
                    || (owned_runtime_dependency
                        && !immutable_preselection_consumer(scan.base, path, display)))
            {
                scan.harnesses.push(display.to_owned());
            }
        }
        if path.extension().and_then(|extension| extension.to_str()) == Some("rs")
            && fs::read_to_string(path).is_ok_and(|source| {
                source.starts_with("// @generated by bbnf; schema=bbnf.generated-rust.v6;")
            })
        {
            scan.findings
                .push(format!("checked-in generated-v6 product `{display}`"));
        }

        let target_metadata = fs::symlink_metadata(path).unwrap_or_else(|error| {
            panic!("{}: candidate target metadata: {error}", path.display())
        });
        assert!(
            target_metadata.is_file() && !target_metadata.file_type().is_symlink(),
            "{}: candidate byte scan receives only regular files",
            path.display()
        );
        if let Some(hashes) = scan.candidate_inventory.by_len.get(&target_metadata.len()) {
            let contents = fs::read(path)
                .unwrap_or_else(|error| panic!("{}: candidate scan: {error}", path.display()));
            let hash = sha256_hex(&contents);
            if let Some(allowed_paths) = hashes.get(&hash) {
                if !allowed_paths.contains(display) {
                    scan.findings.push(format!(
                        "candidate authority copy `{display}` duplicates {allowed_paths:?}"
                    ));
                } else if fs::symlink_metadata(path)
                    .expect("candidate authority metadata")
                    .file_type()
                    .is_symlink()
                {
                    scan.findings
                        .push(format!("candidate authority symlink `{display}`"));
                } else {
                    scan.candidate_authorities_seen.insert(display.to_owned());
                }
            }
        }
    }

    fn walk(scan: &mut PreselectionScan<'_>, path: &Path) {
        let relative = path
            .strip_prefix(scan.base)
            .expect("preselection walk remains under its base");
        let display = relative.to_string_lossy().replace('\\', "/");
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        if matches!(name, ".git" | "target" | "node_modules") {
            return;
        }
        let metadata = fs::symlink_metadata(path)
            .unwrap_or_else(|error| panic!("{}: preselection metadata: {error}", path.display()));
        let forbidden_component = relative.components().any(|component| {
            matches!(
                component.as_os_str().to_str(),
                Some("selected" | "selected-instance" | "materialized")
            )
        });
        if forbidden_component {
            scan.findings
                .push(format!("postselection path `{display}`"));
        }
        if matches!(
            name,
            "W3-HELDOUT-SELECTION.md"
                | "W3-GENERALITY-DIFF.json"
                | "W3-REGEN.json"
                | "W3-REPLAY.md"
        ) {
            scan.findings
                .push(format!("postselection artifact `{display}`"));
        }
        if metadata.file_type().is_symlink() {
            let target = match fs::read_link(path) {
                Ok(target) => target,
                Err(error) => {
                    scan.findings.push(format!(
                        "unreadable repository symlink `{display}`: {error}"
                    ));
                    return;
                }
            };
            let Some(target) = target.to_str() else {
                scan.findings
                    .push(format!("non-UTF-8 repository symlink `{display}`"));
                return;
            };
            if name == "Cargo.toml" {
                scan.findings.push(format!(
                    "repository manifest symlink obscures preselection `{display}`"
                ));
                return;
            }
            let expected = scan
                .candidate_inventory
                .basis_symlinks
                .get(&display)
                .or_else(|| scan.candidate_inventory.materialized_symlinks.get(&display));
            match expected {
                Some(expected) if expected.as_slice() == target.as_bytes() => {
                    assert!(
                        scan.basis_symlinks_seen.insert(display.clone()),
                        "{display}: duplicate preselection-basis symlink"
                    );
                }
                Some(_) => scan.findings.push(format!(
                    "changed repository symlink `{display}` differs from its frozen \
                     preselection-basis link object"
                )),
                None => scan.findings.push(format!(
                    "new repository symlink `{display}` is absent from the frozen \
                     preselection-basis tree"
                )),
            }
            return;
        }
        if metadata.is_dir() {
            let mut children: Vec<PathBuf> = fs::read_dir(path)
                .unwrap_or_else(|error| {
                    panic!("{}: preselection read_dir: {error}", path.display())
                })
                .map(|entry| entry.expect("preselection directory entry").path())
                .collect();
            children.sort();
            for child in children {
                walk(scan, &child);
            }
        } else if metadata.is_file() {
            inspect_file(scan, path, &display, name);
        } else {
            scan.findings
                .push(format!("non-file active entry `{display}`"));
        }
    }

    let mut manifests = BTreeMap::new();
    collect_manifests(base, base, &mut manifests);
    let candidate_inventory = candidate_authority_inventory();
    let mut scan = PreselectionScan {
        base,
        manifests: &manifests,
        candidate_inventory,
        findings: Vec::new(),
        harnesses: Vec::new(),
        candidate_authorities_seen: BTreeSet::new(),
        basis_symlinks_seen: BTreeSet::new(),
    };
    walk(&mut scan, base);
    if base == root() {
        let expected: BTreeSet<String> = candidate_inventory
            .by_len
            .values()
            .flat_map(BTreeMap::values)
            .flat_map(|paths| paths.iter().cloned())
            .collect();
        if scan.candidate_authorities_seen != expected {
            scan.findings.push(format!(
                "candidate authority inventory differs: missing={:?} extra={:?}",
                expected
                    .difference(&scan.candidate_authorities_seen)
                    .collect::<Vec<_>>(),
                scan.candidate_authorities_seen
                    .difference(&expected)
                    .collect::<Vec<_>>()
            ));
        }
        let expected_symlinks: BTreeSet<String> = candidate_inventory
            .basis_symlinks
            .keys()
            .chain(candidate_inventory.materialized_symlinks.keys())
            .cloned()
            .collect();
        if scan.basis_symlinks_seen != expected_symlinks {
            scan.findings.push(format!(
                "repository symlink inventory differs from frozen preselection basis: \
                 missing={:?} extra={:?}",
                expected_symlinks
                    .difference(&scan.basis_symlinks_seen)
                    .collect::<Vec<_>>(),
                scan.basis_symlinks_seen
                    .difference(&expected_symlinks)
                    .collect::<Vec<_>>()
            ));
        }
    }
    if !scan.harnesses.is_empty() {
        scan.findings.push(format!(
            "checked-in external runtime harnesses {:?}",
            scan.harnesses
        ));
    }
    scan.findings.sort();
    scan.findings.dedup();
    scan.findings
}

// Named born-RED gate (RULINGS 2026-11-12d COMP1B-W3 (b); LEDGER LW3.COMP-1b-BR1): its
// findings at the spine merge (master's grammar copies under crates/core/grammar/ and
// typescript/test/fixtures/grammar/lang/, the reverse-dependency crates, the symlink
// inventory) are cured in LW3 by COMP-1d (vi), which removes this attribute. LG-3
// cannot close while it is ignored.
#[test]
#[ignore = "LW3.COMP-1d"]
fn active_roots_remain_preselection_only_and_the_guard_self_falsifies() {
    let repository = root();
    assert_eq!(
        preselection_findings(&repository),
        Vec::<String>::new(),
        "the exhaustive repository contains no selection/materialization output, generated \
         modules, obscuring directory symlinks, or checked-in external W3 harness"
    );
    assert_replacement_refs_cannot_rewrite_frozen_basis();

    let temporary = tempfile::tempdir().expect("preselection guard fixtures");
    let base = temporary.path();
    assert!(preselection_findings(base).is_empty());

    let selected = base.join("previously-unlisted-root/nested/selected");
    fs::create_dir_all(&selected).expect("selected fixture");
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains("postselection path"))
    );
    fs::remove_dir_all(base.join("previously-unlisted-root")).expect("remove selected fixture");

    fs::create_dir(base.join("unknown-product-root")).expect("unknown product root");
    fs::write(base.join("unknown-product-root/g_0001.rs"), b"// generated")
        .expect("generated fixture");
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains("checked-in generated module"))
    );
    fs::remove_dir_all(base.join("unknown-product-root")).expect("remove generated fixture");

    fs::write(
        base.join("arbitrary-generated-name.rs"),
        b"// @generated by bbnf; schema=bbnf.generated-rust.v6;\n",
    )
    .expect("generated-v6 header fixture");
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains("checked-in generated-v6 product"))
    );
    fs::remove_file(base.join("arbitrary-generated-name.rs"))
        .expect("remove generated-v6 header fixture");

    fs::create_dir_all(base.join("waves")).expect("postselection wave fixture");
    fs::write(base.join("waves/W3-REPLAY.md"), b"postselection")
        .expect("postselection artifact fixture");
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains("postselection artifact"))
    );
    fs::remove_dir_all(base.join("waves")).expect("remove postselection fixture");

    let historical_directory = "crates/bootstrap";
    let historical_manifest = "crates/bootstrap/Cargo.toml";
    let historical_root = base.join(historical_directory);
    materialize_preselection_basis_directory(historical_directory, &historical_root);
    assert!(
        preselection_findings(base).is_empty(),
        "byte-identical preselection-basis consumer closure remains inert historical evidence"
    );
    command_output(
        Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(base),
        "initialize immutable-consumer adversarial repository",
    );
    command_output(
        Command::new("git")
            .args(["add", "--force", historical_directory])
            .current_dir(base),
        "stage immutable-consumer basis",
    );
    command_output(
        Command::new("git")
            .args([
                "-c",
                "user.name=SK-V26 contract",
                "-c",
                "user.email=sk-v26@example.invalid",
                "-c",
                "commit.gpgSign=false",
                "-c",
                "core.hooksPath=/dev/null",
                "commit",
                "--quiet",
                "-m",
                "basis",
            ])
            .current_dir(base),
        "commit immutable-consumer basis",
    );

    let ignored_addition = historical_root.join("data/ignored-active-addition.out");
    fs::create_dir_all(ignored_addition.parent().expect("ignored addition parent"))
        .expect("create ignored addition directory");
    fs::write(base.join(".git/info/exclude"), b"crates/bootstrap/data/\n")
        .expect("write adversarial repository-local exclusion");
    fs::write(&ignored_addition, b"ignored active addition")
        .expect("write ignored active addition");
    assert!(
        Command::new("git")
            .args([
                "check-ignore",
                "--quiet",
                "crates/bootstrap/data/ignored-active-addition.out",
            ])
            .current_dir(base)
            .status()
            .expect("check ignored active addition")
            .success(),
        "the adversarial active addition is hidden by standard Git exclusions"
    );
    let hidden_untracked = command_output(
        Command::new("git")
            .args([
                "ls-files",
                "--others",
                "--exclude-standard",
                "--",
                historical_directory,
            ])
            .current_dir(base),
        "probe ignored immutable-consumer addition",
    );
    assert!(
        hidden_untracked.stdout.is_empty(),
        "the superseded Git untracked probe must be blind to the ignored addition"
    );
    assert!(
        !fixture_consumer_matches_basis(&historical_root, historical_directory),
        "filesystem closure rejects an ignored active addition independently of Git excludes"
    );
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains(historical_manifest)),
        "an ignored addition under an unchanged manifest cannot inherit preselection"
    );
    fs::remove_dir_all(
        ignored_addition
            .parent()
            .expect("ignored addition directory"),
    )
    .expect("remove ignored addition fixture");

    let historical_source = historical_root.join("src/lib.rs");
    let basis_source =
        preselection_basis_bytes("crates/bootstrap/src/lib.rs").expect("historical source basis");
    let mut changed_source = basis_source.clone();
    changed_source.extend_from_slice(b"\n// index-hidden W3 runner source mutation\n");
    command_output(
        Command::new("git")
            .args([
                "update-index",
                "--skip-worktree",
                "crates/bootstrap/src/lib.rs",
            ])
            .current_dir(base),
        "hide immutable-consumer source in the Git index",
    );
    fs::write(&historical_source, changed_source).expect("write index-hidden source mutation");
    assert!(
        Command::new("git")
            .args([
                "diff",
                "--quiet",
                "--no-ext-diff",
                "HEAD",
                "--",
                historical_directory,
            ])
            .current_dir(base)
            .status()
            .expect("probe index-hidden immutable-consumer mutation")
            .success(),
        "the superseded Git diff probe must be blind to skip-worktree state"
    );
    assert!(
        !fixture_consumer_matches_basis(&historical_root, historical_directory),
        "filesystem closure rejects an index-hidden tracked mutation"
    );
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains(historical_manifest)),
        "an index-hidden source mutation under an unchanged manifest cannot inherit preselection"
    );
    command_output(
        Command::new("git")
            .args([
                "update-index",
                "--no-skip-worktree",
                "crates/bootstrap/src/lib.rs",
            ])
            .current_dir(base),
        "restore immutable-consumer index visibility",
    );
    fs::write(&historical_source, basis_source).expect("restore historical source fixture");
    #[cfg(unix)]
    {
        let original_mode = fs::symlink_metadata(&historical_source)
            .expect("historical source metadata")
            .permissions()
            .mode();
        let mut executable = fs::symlink_metadata(&historical_source)
            .expect("historical source metadata")
            .permissions();
        executable.set_mode(original_mode | 0o111);
        fs::set_permissions(&historical_source, executable)
            .expect("set adversarial executable source mode");
        assert!(
            !fixture_consumer_matches_basis(&historical_root, historical_directory),
            "filesystem closure rejects an executable-bit mutation"
        );
        let mut restored = fs::symlink_metadata(&historical_source)
            .expect("historical source metadata")
            .permissions();
        restored.set_mode(original_mode);
        fs::set_permissions(&historical_source, restored).expect("restore historical source mode");
    }
    let historical = base.join(historical_manifest);
    let basis_bytes =
        preselection_basis_bytes(historical_manifest).expect("sealed preselection-basis manifest");
    let mut changed_historical = basis_bytes;
    changed_historical.extend_from_slice(b"\n# W3 runner mutation\n");
    fs::write(&historical, changed_historical).expect("write historical manifest mutation");
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains(historical_manifest)),
        "a changed historical consumer cannot inherit its preselection disposition"
    );
    fs::remove_dir_all(base.join("crates")).expect("remove historical manifest fixture");

    for (index, (label, dependency)) in [
        (
            "parse-that registry alias",
            "runtime={package='parse_that',version='=0.5.0',default-features=false}",
        ),
        (
            "parse-that path alias",
            "runtime={package='parse_that',version='=0.5.0',path='../runtime'}",
        ),
        (
            "parse-that git alias",
            "runtime={package='parse_that',version='=0.5.0',git='https://invalid.example.invalid/runtime'}",
        ),
        ("parse-that workspace", "parse_that={workspace=true}"),
        (
            "BBNF path alias",
            "compiler={package='bbnf',version='=0.4.0',path='../compiler'}",
        ),
        (
            "BBNF git alias",
            "compiler={package='bbnf',version='=0.4.0',git='https://invalid.example.invalid/compiler'}",
        ),
        ("BBNF workspace", "bbnf={workspace=true}"),
    ]
    .into_iter()
    .enumerate()
    {
        let harness = base.join(format!("renamed-harness-{index}"));
        fs::create_dir(&harness).expect("harness fixture");
        fs::write(
            harness.join("Cargo.toml"),
            format!(
                "[package]\nname='renamed-heldout-runner-{index}'\nversion='0.0.0'\n\
                 [dependencies]\n{dependency}\n"
            ),
        )
        .expect("write renamed semantic harness fixture");
        let expected_path = format!("renamed-harness-{index}/Cargo.toml");
        assert!(
            preselection_findings(base).iter().any(|finding| {
                finding.contains("checked-in external runtime harnesses")
                    && finding.contains(&expected_path)
            }),
            "{label}: a dependency alias/source change cannot conceal a checked-in W3 harness"
        );
        fs::remove_dir_all(&harness).expect("remove renamed harness fixture");
    }
    for (index, (package, owned_path)) in [
        ("parse_that", pinned_parse_that_package_dir()),
        ("bbnf", root().join("crates/core")),
    ]
    .into_iter()
    .enumerate()
    {
        let harness = base.join(format!("renamed-owned-path-harness-{index}"));
        fs::create_dir(&harness).expect("owned-path harness fixture");
        fs::write(
            harness.join("Cargo.toml"),
            format!(
                "[package]\nname='renamed-owned-path-runner-{index}'\nversion='0.0.0'\n\
                 [dependencies]\nruntime={{package='{package}',path={:?}}}\n",
                owned_path.to_str().expect("owned package path is UTF-8")
            ),
        )
        .expect("write renamed exact-owned-path harness fixture");
        let expected_path = format!("renamed-owned-path-harness-{index}/Cargo.toml");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains(&expected_path)),
            "{package}: an exact owned path without a version remains a W3 harness dependency"
        );
        fs::remove_dir_all(&harness).expect("remove exact-owned-path harness fixture");
    }

    let workspace_harness = base.join("workspace-alias");
    fs::create_dir_all(workspace_harness.join("runner")).expect("workspace alias harness fixture");
    fs::write(
        workspace_harness.join("Cargo.toml"),
        "[workspace]\nmembers=['runner']\n\
         [workspace.dependencies]\ncompiler={package='bbnf',path='../compiler'}\n",
    )
    .expect("write workspace alias root");
    fs::write(
        workspace_harness.join("runner/Cargo.toml"),
        "[package]\nname='renamed-workspace-runner'\nversion='0.0.0'\n\
         [dependencies]\ncompiler={workspace=true}\n",
    )
    .expect("write inherited workspace alias consumer");
    assert!(
        preselection_findings(base).iter().any(|finding| {
            finding.contains("workspace-alias/runner/Cargo.toml")
                && finding.contains("checked-in external runtime harnesses")
        }),
        "workspace-inherited package aliases remain source-agnostic harness dependencies"
    );
    fs::remove_dir_all(&workspace_harness).expect("remove workspace alias harness");

    for (index, section) in ["dependencies", "build-dependencies", "dev-dependencies"]
        .into_iter()
        .enumerate()
    {
        let relative = format!("targeted-harness-{index}");
        let targeted_harness = base.join(&relative);
        fs::create_dir(&targeted_harness).expect("target-specific harness fixture");
        fs::write(
            targeted_harness.join("Cargo.toml"),
            format!(
                "[package]\nname='targeted-runner-{index}'\nversion='0.0.0'\n\
                 [target.'cfg(unix)'.{section}]\n\
                 compiler={{package='bbnf',git='https://invalid.example.invalid/compiler'}}\n"
            ),
        )
        .expect("write target-specific harness manifest");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains(&format!("{relative}/Cargo.toml"))),
            "target-specific {section} remains in the harness closure"
        );
        fs::remove_dir_all(&targeted_harness).expect("remove target-specific harness");
    }

    assert!(
        preselection_findings(base).is_empty(),
        "the exact W0 trees require no checked-in external runtime harness"
    );
    for retired in [
        "contracts/w2-generated-control",
        "contracts/w2-record-control",
        "contracts/w2-recursive-control",
    ] {
        assert!(
            !root().join(retired).exists(),
            "{retired} must be absent after its laws migrate in-tree"
        );
    }

    let candidate_copy_root = base.join("arbitrary-candidate-copy");
    fs::create_dir(&candidate_copy_root).expect("candidate copy fixture");
    let candidate_source = candidate_authority_inventory()
        .expected
        .keys()
        .find(|path| path.ends_with(".bbnf"))
        .expect("candidate grammar authority");
    fs::write(
        candidate_copy_root.join("opaque.data"),
        bytes(candidate_source),
    )
    .expect("write arbitrary candidate copy");
    assert!(
        preselection_findings(base)
            .iter()
            .any(|finding| finding.contains("candidate authority copy")),
        "candidate bytes at an arbitrary path are rejected"
    );
    fs::remove_file(candidate_copy_root.join("opaque.data")).expect("remove candidate copy");
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(
            root().join(candidate_source),
            candidate_copy_root.join("opaque-link.data"),
        )
        .expect("candidate authority symlink fixture");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains("new repository symlink")),
            "a new symlink cannot hide candidate bytes behind an arbitrary path"
        );
        fs::remove_file(candidate_copy_root.join("opaque-link.data"))
            .expect("remove arbitrary candidate symlink");

        let canonical_link = base.join(candidate_source);
        fs::create_dir_all(
            canonical_link
                .parent()
                .expect("candidate canonical fixture parent"),
        )
        .expect("create candidate canonical fixture directory");
        std::os::unix::fs::symlink(root().join(candidate_source), &canonical_link)
            .expect("candidate canonical symlink fixture");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains("new repository symlink")),
            "even an exact candidate path cannot replace its regular-file authority with a symlink"
        );
        fs::remove_dir_all(
            base.join(
                Path::new(candidate_source)
                    .components()
                    .next()
                    .expect("candidate path root component")
                    .as_os_str(),
            ),
        )
        .expect("remove canonical candidate symlink fixture");
    }
    fs::remove_dir_all(&candidate_copy_root).expect("remove candidate-copy fixture");

    #[cfg(unix)]
    {
        let (basis_path, basis_target) = candidate_authority_inventory()
            .basis_symlinks
            .iter()
            .next()
            .expect("frozen preselection basis contains a historical symlink");
        let basis_target = std::str::from_utf8(basis_target)
            .expect("frozen preselection-basis symlink target is UTF-8");
        let fixture = base.join(basis_path);
        fs::create_dir_all(fixture.parent().expect("basis symlink fixture parent"))
            .expect("create basis symlink fixture parent");
        std::os::unix::fs::symlink(basis_target, &fixture)
            .expect("materialize exact frozen-basis symlink object");
        assert!(
            preselection_findings(base).is_empty(),
            "an unchanged frozen-basis symlink object is admitted without following its target"
        );
        fs::remove_file(&fixture).expect("remove exact frozen-basis symlink fixture");

        std::os::unix::fs::symlink("changed-preselection-target", &fixture)
            .expect("changed frozen-basis symlink fixture");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains("changed repository symlink")),
            "a changed frozen-basis symlink object is rejected without following its target"
        );
        fs::remove_file(&fixture).expect("remove changed frozen-basis symlink fixture");

        for (name, target) in [
            ("broken-link", "missing-preselection-target"),
            (
                "out-of-repository-link",
                "/outside/frozen/preselection/repository",
            ),
        ] {
            let link = base.join(name);
            std::os::unix::fs::symlink(target, &link)
                .expect("new hermetic symlink rejection fixture");
            assert!(
                preselection_findings(base)
                    .iter()
                    .any(|finding| finding.contains("new repository symlink")),
                "{name}: a new broken or out-of-repository symlink is rejected by link object"
            );
            fs::remove_file(link).expect("remove new hermetic symlink fixture");
        }
    }

    let harness = base.join("harness");
    fs::create_dir(&harness).expect("symlink harness fixture");

    #[cfg(unix)]
    {
        fs::write(
            harness.join("manifest-target.toml"),
            "[package]\nname='linked-runtime'\nversion='0.0.0'\n",
        )
        .expect("write symlink manifest target");
        std::os::unix::fs::symlink(
            harness.join("manifest-target.toml"),
            base.join("Cargo.toml"),
        )
        .expect("manifest symlink fixture");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains("manifest symlink obscures preselection")),
            "a Cargo manifest symlink is rejected without escaping manifest discovery"
        );
        fs::remove_file(base.join("Cargo.toml")).expect("remove manifest symlink fixture");

        std::os::unix::fs::symlink(&harness, base.join("linked-runtime")).expect("symlink fixture");
        assert!(
            preselection_findings(base)
                .iter()
                .any(|finding| finding.contains("new repository symlink"))
        );
    }
}

fn object<'a>(value: &'a Value, at: &str) -> &'a Map<String, Value> {
    value
        .as_object()
        .unwrap_or_else(|| panic!("{at}: expected object"))
}

fn exact_keys(map: &Map<String, Value>, expected: &[&str], at: &str) {
    let actual: BTreeSet<&str> = map.keys().map(String::as_str).collect();
    let expected: BTreeSet<&str> = expected.iter().copied().collect();
    assert_eq!(actual, expected, "{at}: object key set");
}

fn is_identifier(value: &str) -> bool {
    let mut bytes = value.bytes();
    matches!(bytes.next(), Some(b'a'..=b'z' | b'A'..=b'Z' | b'_'))
        && bytes.all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
}

fn validate_instance(value: &Value, at: &str) {
    let instance = object(value, at);
    exact_keys(
        instance,
        &[
            "schema",
            "logical_path",
            "entry",
            "trivia_regex",
            "pratt_tables",
        ],
        at,
    );
    assert_eq!(
        instance["schema"].as_str(),
        Some("bbnf.grammar-instance.v1"),
        "{at}: schema"
    );
    let logical_path = instance["logical_path"]
        .as_str()
        .unwrap_or_else(|| panic!("{at}: logical_path"));
    assert!(
        !logical_path.is_empty()
            && !logical_path.starts_with('/')
            && !logical_path.contains('\\')
            && !logical_path.split('/').any(|part| part == "..")
            && !logical_path.contains('\0'),
        "{at}: portable logical_path"
    );
    assert!(
        is_identifier(
            instance["entry"]
                .as_str()
                .unwrap_or_else(|| panic!("{at}: entry"))
        ),
        "{at}: identifier entry"
    );
    assert!(
        instance["trivia_regex"].is_null() || instance["trivia_regex"].is_string(),
        "{at}: nullable trivia_regex"
    );

    let tables = instance["pratt_tables"]
        .as_array()
        .unwrap_or_else(|| panic!("{at}: pratt_tables"));
    for (table_index, table) in tables.iter().enumerate() {
        let table_at = format!("{at}.pratt_tables[{table_index}]");
        let table = object(table, &table_at);
        exact_keys(table, &["site_rule", "atom_rule", "operators"], &table_at);
        for name in ["site_rule", "atom_rule"] {
            assert!(
                is_identifier(
                    table[name]
                        .as_str()
                        .unwrap_or_else(|| panic!("{table_at}.{name}"))
                ),
                "{table_at}.{name}"
            );
        }
        let operators = table["operators"]
            .as_array()
            .unwrap_or_else(|| panic!("{table_at}.operators"));
        assert!(!operators.is_empty(), "{table_at}: empty operator table");
        for (operator_index, operator) in operators.iter().enumerate() {
            let operator_at = format!("{table_at}.operators[{operator_index}]");
            let operator = object(operator, &operator_at);
            exact_keys(
                operator,
                &[
                    "matcher_rule",
                    "fixity",
                    "left_binding_power",
                    "right_binding_power",
                ],
                &operator_at,
            );
            assert!(
                is_identifier(
                    operator["matcher_rule"]
                        .as_str()
                        .unwrap_or_else(|| panic!("{operator_at}.matcher_rule"))
                ),
                "{operator_at}: matcher identifier"
            );
            let left = binding_power(&operator["left_binding_power"], &operator_at);
            let right = binding_power(&operator["right_binding_power"], &operator_at);
            match operator["fixity"].as_str() {
                Some("prefix") => {
                    assert_eq!(left, None, "{operator_at}: prefix left");
                    assert!(right.is_some(), "{operator_at}: prefix right");
                }
                Some("postfix") => {
                    assert!(left.is_some(), "{operator_at}: postfix left");
                    assert_eq!(right, None, "{operator_at}: postfix right");
                }
                Some("infix") => {
                    let left = left.unwrap_or_else(|| panic!("{operator_at}: infix left"));
                    let right = right.unwrap_or_else(|| panic!("{operator_at}: infix right"));
                    assert!(
                        right == left || right == left + 1,
                        "{operator_at}: associativity relation"
                    );
                }
                other => panic!("{operator_at}: invalid fixity {other:?}"),
            }
        }
    }
}

fn binding_power(value: &Value, at: &str) -> Option<u16> {
    if value.is_null() {
        return None;
    }
    let power = value
        .as_u64()
        .unwrap_or_else(|| panic!("{at}: binding power must be an integer"));
    assert!(
        (1..=u64::from(u16::MAX)).contains(&power),
        "{at}: binding power range"
    );
    Some(power as u16)
}

fn read_tsv(relative: &str, expected_header: &[&str]) -> Vec<Vec<String>> {
    let data = bytes(relative);
    assert!(data.ends_with(b"\n"), "{relative}: missing terminal LF");
    assert!(!data.contains(&b'\r'), "{relative}: CR is forbidden");
    let text = std::str::from_utf8(&data).unwrap_or_else(|error| panic!("{relative}: {error}"));
    let mut lines = text.lines();
    let header: Vec<&str> = lines
        .next()
        .unwrap_or_else(|| panic!("{relative}: empty ledger"))
        .split('\t')
        .collect();
    assert_eq!(header, expected_header, "{relative}: header");
    lines
        .enumerate()
        .map(|(index, line)| {
            assert!(!line.is_empty(), "{relative}: blank row {}", index + 2);
            let fields: Vec<String> = line.split('\t').map(str::to_owned).collect();
            assert_eq!(
                fields.len(),
                expected_header.len(),
                "{relative}: row {} width",
                index + 2
            );
            assert!(
                fields.iter().all(|field| !field.is_empty()),
                "{relative}: empty field in row {}",
                index + 2
            );
            fields
        })
        .collect()
}

fn candidate_deny_hashes() -> &'static (BTreeSet<String>, BTreeSet<String>) {
    static HASHES: OnceLock<(BTreeSet<String>, BTreeSet<String>)> = OnceLock::new();
    HASHES.get_or_init(|| {
        let rows = read_tsv(
            CANDIDATE_PRESEAL,
            &[
                "candidate_id",
                "ordinal",
                "grammar_path",
                "grammar_sha256",
                "instance_path",
                "instance_sha256",
                "case_ledger_path",
                "case_ledger_sha256",
                "case_classes",
                "recovery_policy",
                "execution_state",
                "disposition",
            ],
        );
        assert_eq!(rows.len(), CANDIDATE_ORDINALS.len());
        let mut source = BTreeSet::new();
        let mut manifest = BTreeSet::new();
        for (row, (ordinal, identity)) in rows.iter().zip(CANDIDATE_IDENTITIES) {
            assert_eq!(row[0], identity);
            assert_eq!(row[1], ordinal);
            for (label, hash) in [("grammar", &row[3]), ("instance", &row[5])] {
                assert!(
                    hash.len() == 64
                        && hash
                            .bytes()
                            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
                    "{identity}: {label} preseal hash is canonical lowercase SHA-256"
                );
            }
            assert!(
                source.insert(row[3].clone()),
                "{identity}: duplicate candidate grammar hash"
            );
            assert!(
                manifest.insert(row[5].clone()),
                "{identity}: duplicate candidate instance hash"
            );
        }
        (source, manifest)
    })
}

fn assert_sha(relative: &str, expected: &str) {
    assert_eq!(
        sha256_hex(&bytes(relative)),
        expected,
        "{relative}: SHA-256"
    );
}

fn decode_input(relative: &str) -> Vec<u8> {
    let value = json_file(relative);
    let envelope = object(&value, relative);
    exact_keys(
        envelope,
        &["schema", "encoding", "bytes", "decoded_sha256"],
        relative,
    );
    assert_eq!(
        envelope["schema"].as_str(),
        Some("bbnf.w3.input-bytes.v1"),
        "{relative}: schema"
    );
    assert_eq!(envelope["encoding"].as_str(), Some("hex"), "{relative}");
    let encoded = envelope["bytes"]
        .as_str()
        .unwrap_or_else(|| panic!("{relative}: bytes"));
    assert_eq!(encoded.len() % 2, 0, "{relative}: odd hex length");
    assert!(
        encoded
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
        "{relative}: hex must be lowercase"
    );
    let decoded: Vec<u8> = encoded
        .as_bytes()
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| (hex_nibble(pair[0]).unwrap() << 4) | hex_nibble(pair[1]).unwrap())
        .collect();
    assert_eq!(
        sha256_hex(&decoded),
        envelope["decoded_sha256"].as_str().unwrap(),
        "{relative}: decoded SHA-256"
    );
    decoded
}

fn validate_expectation(relative: &str, class: &str, recovery_policy: &str) -> Value {
    let value = json_file(relative);
    let expectation = object(&value, relative);
    let outcome = expectation["outcome"]
        .as_str()
        .unwrap_or_else(|| panic!("{relative}: outcome"));
    let expected_keys: &[&str] = if outcome == "MATCH" {
        &[
            "schema",
            "oracle",
            "outcome",
            "full_consumption",
            "recovery_policy",
            "runtime_recovery",
            "cursor_after",
            "observations",
        ]
    } else {
        assert_eq!(outcome, "MISMATCH", "{relative}: outcome");
        &[
            "schema",
            "oracle",
            "outcome",
            "full_consumption",
            "recovery_policy",
            "runtime_recovery",
            "cursor_after",
            "observations",
            "reason",
        ]
    };
    exact_keys(expectation, expected_keys, relative);
    assert_eq!(
        expectation["schema"].as_str(),
        Some("bbnf.w3.symbolic-expectation.v1")
    );
    assert_eq!(
        expectation["recovery_policy"].as_str(),
        Some(recovery_policy),
        "{relative}: policy"
    );
    assert_eq!(
        expectation["runtime_recovery"].as_str(),
        Some("Disabled"),
        "{relative}: runtime recovery"
    );
    let oracle = object(&expectation["oracle"], &format!("{relative}.oracle"));
    exact_keys(
        oracle,
        &[
            "kind",
            "authority",
            "implementation_generated",
            "normalization",
        ],
        &format!("{relative}.oracle"),
    );
    assert_eq!(
        oracle["kind"].as_str(),
        Some("INDEPENDENT_LOWERED_PLAN_EVENT_ORACLE")
    );
    assert_eq!(oracle["implementation_generated"].as_bool(), Some(false));
    assert_eq!(oracle["normalization"].as_str(), Some(PACKET_README));
    assert!(
        !oracle["authority"].as_str().unwrap_or_default().is_empty(),
        "{relative}: independent authority"
    );

    let observations = expectation["observations"]
        .as_array()
        .unwrap_or_else(|| panic!("{relative}: observations"));
    let mut error_tokens = 0;
    for (index, observation) in observations.iter().enumerate() {
        let at = format!("{relative}.observations[{index}]");
        let observation = object(observation, &at);
        match observation["kind"].as_str() {
            Some("TOKEN") => {
                exact_keys(observation, &["kind", "symbol", "lexeme"], &at);
                assert!(
                    matches!(observation["symbol"].as_str(), Some("literal" | "regex")),
                    "{at}: closed TOKEN symbol"
                );
                assert!(
                    !observation["lexeme"]
                        .as_str()
                        .unwrap_or_else(|| panic!("{at}: TOKEN lexeme must be a string"))
                        .is_empty(),
                    "{at}: zero-width TOKEN observations are forbidden"
                );
            }
            Some("TRIVIA") => {
                exact_keys(observation, &["kind", "lexeme"], &at);
                assert!(
                    !observation["lexeme"]
                        .as_str()
                        .unwrap_or_else(|| panic!("{at}: TRIVIA lexeme must be a string"))
                        .is_empty(),
                    "{at}: empty TRIVIA observations are forbidden"
                );
            }
            Some("ERROR_TOKEN") => {
                error_tokens += 1;
                exact_keys(observation, &["kind", "transform", "lexeme"], &at);
                assert_eq!(
                    observation["transform"].as_str(),
                    Some("error"),
                    "{at}: only the ordinary inert error transform is normalized"
                );
                assert!(
                    !observation["lexeme"]
                        .as_str()
                        .unwrap_or_else(|| panic!("{at}: ERROR_TOKEN lexeme must be a string"))
                        .is_empty(),
                    "{at}: ERROR_TOKEN must consume input"
                );
            }
            Some("PRATT_REDUCE") => {
                exact_keys(
                    observation,
                    &["kind", "fixity", "matcher_rule", "reduced"],
                    &at,
                );
                assert!(
                    matches!(
                        observation["fixity"].as_str(),
                        Some("prefix" | "postfix" | "infix")
                    ),
                    "{at}: closed Pratt fixity"
                );
                assert!(
                    is_identifier(
                        observation["matcher_rule"]
                            .as_str()
                            .unwrap_or_else(|| panic!("{at}: matcher_rule must be a string"))
                    ),
                    "{at}: matcher_rule identifier"
                );
                assert!(
                    !observation["reduced"]
                        .as_str()
                        .unwrap_or_else(|| panic!("{at}: reduced must be a string"))
                        .is_empty(),
                    "{at}: Pratt reduction span must consume input"
                );
            }
            Some("END_INPUT") => exact_keys(observation, &["kind"], &at),
            other => panic!("{at}: unknown normalized observation {other:?}"),
        }
    }
    if error_tokens != 0 {
        assert_eq!(
            recovery_policy, "GRAMMAR_ERROR_TOKEN_CONTINUE",
            "{relative}: ERROR_TOKEN belongs only to the grammar recovery witness"
        );
    }

    match class {
        "POSITIVE" => assert_eq!(outcome, "MATCH", "{relative}: positive intent"),
        "NEGATIVE" | "FULL_CONSUMPTION" => {
            assert_eq!(outcome, "MISMATCH", "{relative}: rejection intent")
        }
        "RECOVERY_POLICY" if recovery_policy == "ATOMIC_REJECT" => {
            assert_eq!(outcome, "MISMATCH", "{relative}: atomic recovery intent")
        }
        "RECOVERY_POLICY" => {
            assert_eq!(
                recovery_policy, "GRAMMAR_ERROR_TOKEN_CONTINUE",
                "{relative}: recovery policy"
            );
            assert_eq!(
                outcome, "MATCH",
                "{relative}: grammar error-token recovery must commit and continue"
            );
            assert!(
                error_tokens != 0,
                "{relative}: committed grammar recovery must expose its ERROR_TOKEN"
            );
        }
        other => panic!("{relative}: unknown class {other}"),
    }
    if outcome == "MATCH" {
        assert_eq!(expectation["full_consumption"].as_bool(), Some(true));
        assert_eq!(expectation["cursor_after"].as_str(), Some("END_INPUT"));
        assert_eq!(
            observations.last().and_then(|row| row["kind"].as_str()),
            Some("END_INPUT"),
            "{relative}: terminal observation"
        );
    } else {
        assert_eq!(expectation["full_consumption"].as_bool(), Some(false));
        assert_eq!(expectation["cursor_after"].as_str(), Some("ENTRY"));
        assert!(observations.is_empty(), "{relative}: rollback observations");
        assert!(
            !expectation["reason"]
                .as_str()
                .unwrap_or_default()
                .is_empty(),
            "{relative}: mismatch reason"
        );
    }
    value
}

#[test]
fn grammar_instance_schema_and_all_sealed_instances_are_strict() {
    let schema_path = "contracts/w3-grammar-instance.schema.json";
    let schema = json_file(schema_path);
    jsonschema::draft202012::meta::validate(&schema)
        .unwrap_or_else(|error| panic!("{schema_path}: invalid Draft 2020-12 schema: {error}"));
    let validator = jsonschema::draft202012::new(&schema)
        .unwrap_or_else(|error| panic!("{schema_path}: schema compilation failed: {error}"));
    let top = object(&schema, schema_path);
    assert_eq!(
        top["$schema"].as_str(),
        Some("https://json-schema.org/draft/2020-12/schema")
    );
    assert_eq!(top["type"].as_str(), Some("object"));
    assert_eq!(top["additionalProperties"].as_bool(), Some(false));
    assert_eq!(
        top["required"],
        json!([
            "schema",
            "logical_path",
            "entry",
            "trivia_regex",
            "pratt_tables"
        ])
    );
    let properties = object(&top["properties"], "schema.properties");
    exact_keys(
        properties,
        &[
            "schema",
            "logical_path",
            "entry",
            "trivia_regex",
            "pratt_tables",
        ],
        "schema.properties",
    );
    assert_eq!(
        properties["schema"]["const"].as_str(),
        Some("bbnf.grammar-instance.v1")
    );
    assert!(
        properties["logical_path"].get("maxLength").is_none(),
        "representation schema must not invent a path budget"
    );
    let definitions = object(&top["$defs"], "schema.$defs");
    assert_eq!(
        definitions["identifier"]["pattern"].as_str(),
        Some("^[A-Za-z_][A-Za-z0-9_-]*$")
    );
    assert!(is_identifier("packet-compatible-name"));
    assert!(!is_identifier("-not-an-identifier"));
    for name in [
        "pratt_table",
        "prefix_operator",
        "postfix_operator",
        "infix_operator",
    ] {
        assert_eq!(
            definitions[name]["additionalProperties"].as_bool(),
            Some(false),
            "schema.$defs.{name}"
        );
    }
    assert_eq!(
        definitions["pratt_table"]["properties"]["operators"]["minItems"].as_u64(),
        Some(1)
    );
    assert_eq!(definitions["binding_power"]["minimum"].as_u64(), Some(1));
    assert_eq!(
        definitions["binding_power"]["maximum"].as_u64(),
        Some(65_535)
    );
    assert_eq!(
        definitions["prefix_operator"]["properties"]["fixity"]["const"].as_str(),
        Some("prefix")
    );
    assert_eq!(
        definitions["postfix_operator"]["properties"]["fixity"]["const"].as_str(),
        Some("postfix")
    );
    assert_eq!(
        definitions["infix_operator"]["properties"]["fixity"]["const"].as_str(),
        Some("infix")
    );

    for (class, ordinals) in [
        ("public", PUBLIC_ORDINALS.as_slice()),
        ("candidates", CANDIDATE_ORDINALS.as_slice()),
    ] {
        for ordinal in ordinals {
            let relative = format!("contracts/w3-public-witnesses/{class}/{ordinal}/instance.json");
            let instance = json_file(&relative);
            validator
                .validate(&instance)
                .unwrap_or_else(|error| panic!("{relative}: schema rejection: {error}"));
            validate_instance(&instance, &relative);
        }
    }

    let mut hyphenated = json_file("contracts/w3-public-witnesses/public/0001/instance.json");
    hyphenated["entry"] = json!("packet-compatible-entry");
    validate_instance(&hyphenated, "synthetic hyphenated retained identifier");
}

#[test]
fn packet_inventory_ordinals_modes_and_preselection_paths_are_exact() {
    let packet_root = root().join("contracts/w3-public-witnesses");
    let mut files = BTreeSet::new();
    let mut directories = BTreeSet::new();
    walk_packet(&packet_root, &packet_root, &mut files, &mut directories);

    assert_eq!(files.len(), 84, "sealed packet file count");
    assert_eq!(files, packet_file_inventory(), "sealed packet files");
    assert_eq!(directories.len(), 19, "sealed packet directory count");
    assert_eq!(
        directories,
        packet_directory_inventory(),
        "sealed packet directories and exact ordinal roots"
    );
    for forbidden in ["selected", "selected-instance", "materialized"] {
        assert!(
            !packet_root.join(forbidden).exists(),
            "pre-freeze packet must not contain a {forbidden} directory"
        );
    }
}

#[test]
fn packet_authorities_hash_envelopes_and_symbol_ledgers_close_independently() {
    assert_eq!(
        sha256_hex(b"abc"),
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    );
    assert_sha(ADDENDUM, ADDENDUM_SHA256);
    assert_sha(PUBLIC_CASES, PUBLIC_CASES_SHA256);
    assert_sha(CANDIDATE_PRESEAL, CANDIDATE_PRESEAL_SHA256);
    assert_sha(PACKET_README, PACKET_README_SHA256);

    let addendum_header = [
        "witness_id",
        "grammar_path",
        "grammar_sha256",
        "instance_path",
        "instance_sha256",
        "semantic_plane",
        "feature_id",
        "case_ledger_path",
        "case_ledger_sha256",
        "recovery_policy",
        "disposition",
    ];
    let addendum = read_tsv(ADDENDUM, &addendum_header);
    assert_eq!(addendum.len(), 23, "public feature rows");
    let mut witnesses: BTreeMap<String, (String, String)> = BTreeMap::new();
    let mut public_authorities: BTreeMap<String, [String; 7]> = BTreeMap::new();
    for row in &addendum {
        let (ordinal, expected_witness) = PUBLIC_IDENTITIES
            .iter()
            .find(|(_, witness)| *witness == row[0])
            .copied()
            .unwrap_or_else(|| panic!("unknown public witness identity {}", row[0]));
        assert_eq!(row[0], expected_witness);
        assert_eq!(
            row[1],
            format!("contracts/w3-public-witnesses/public/{ordinal}/grammar.bbnf")
        );
        assert_eq!(
            row[3],
            format!("contracts/w3-public-witnesses/public/{ordinal}/instance.json")
        );
        assert_eq!(
            row[7],
            format!("contracts/w3-public-witnesses/public/{ordinal}/cases.tsv")
        );
        assert_sha(&row[1], &row[2]);
        assert_sha(&row[3], &row[4]);
        assert_sha(&row[7], &row[8]);
        assert_eq!(
            json_file(&row[3])["logical_path"].as_str(),
            Some(row[1].as_str()),
            "{}: manifest logical_path is not its grammar authority",
            row[0]
        );
        assert_eq!(row[5], "P2_EVENT");
        assert_eq!(row[10], "PRESEALED_CORRECTIVE_W0_AUTHORITY");
        let complete_authority = [
            row[1].clone(),
            row[2].clone(),
            row[3].clone(),
            row[4].clone(),
            row[7].clone(),
            row[8].clone(),
            row[9].clone(),
        ];
        assert_eq!(
            public_authorities
                .entry(row[0].clone())
                .or_insert(complete_authority.clone()),
            &complete_authority,
            "{} changes its complete grammar/instance/case authority tuple",
            row[0]
        );
        let identity = (row[7].clone(), row[9].clone());
        assert_eq!(
            witnesses.entry(row[0].clone()).or_insert(identity.clone()),
            &identity,
            "{} changes packet identity",
            row[0]
        );
    }
    let exact_public_ids: BTreeSet<String> = PUBLIC_IDENTITIES
        .iter()
        .map(|(_, witness)| (*witness).to_owned())
        .collect();
    assert_eq!(
        witnesses.keys().cloned().collect::<BTreeSet<_>>(),
        exact_public_ids,
        "stable exact public witness IDs"
    );
    assert_eq!(
        public_authorities.len(),
        PUBLIC_IDENTITIES.len(),
        "one closed authority tuple per public witness"
    );

    let global_public = read_tsv(PUBLIC_CASES, &CASE_HEADER);
    assert_eq!(global_public.len(), 12);
    let mut concatenated_public = Vec::new();
    for ordinal in PUBLIC_ORDINALS {
        let local = format!("contracts/w3-public-witnesses/public/{ordinal}/cases.tsv");
        let local_rows = read_tsv(&local, &CASE_HEADER);
        assert_eq!(local_rows.len(), 4, "{local}: four class rows");
        close_case_rows(&local_rows, &witnesses);
        concatenated_public.extend(local_rows);
    }
    assert_eq!(
        global_public, concatenated_public,
        "global public ledger is the ordered concatenation of ordinal ledgers"
    );

    let candidate_header = [
        "candidate_id",
        "ordinal",
        "grammar_path",
        "grammar_sha256",
        "instance_path",
        "instance_sha256",
        "case_ledger_path",
        "case_ledger_sha256",
        "case_classes",
        "recovery_policy",
        "execution_state",
        "disposition",
    ];
    let candidates = read_tsv(CANDIDATE_PRESEAL, &candidate_header);
    assert_eq!(candidates.len(), CANDIDATE_ORDINALS.len());
    let (candidate_sources, candidate_manifests) = candidate_deny_hashes();
    assert_eq!(
        candidate_sources,
        &candidates
            .iter()
            .map(|row| row[3].clone())
            .collect::<BTreeSet<_>>(),
        "compiler candidate-source deny guard is derived from the sealed preseal rows"
    );
    assert_eq!(
        candidate_manifests,
        &candidates
            .iter()
            .map(|row| row[5].clone())
            .collect::<BTreeSet<_>>(),
        "compiler candidate-manifest deny guard is derived from the sealed preseal rows"
    );
    for (row, (ordinal, candidate_id)) in candidates.iter().zip(CANDIDATE_IDENTITIES) {
        assert_eq!(row[0], candidate_id, "stable exact candidate name");
        assert_eq!(row[1], ordinal);
        assert_eq!(
            row[4],
            format!("contracts/w3-public-witnesses/candidates/{ordinal}/instance.json")
        );
        assert_eq!(
            row[6],
            format!("contracts/w3-public-witnesses/candidates/{ordinal}/cases.tsv")
        );
        assert_sha(&row[2], &row[3]);
        assert_sha(&row[4], &row[5]);
        assert_sha(&row[6], &row[7]);
        assert_eq!(
            json_file(&row[4])["logical_path"].as_str(),
            Some(row[2].as_str()),
            "{candidate_id}: manifest logical_path is not its grammar authority"
        );
        assert_eq!(row[8], "POSITIVE,NEGATIVE,RECOVERY_POLICY,FULL_CONSUMPTION");
        assert_eq!(row[10], "NOT_COMPILED_OR_EXECUTED");
        assert_eq!(row[11], "PRESEALED_W0_POOL_INPUT");

        let local_rows = read_tsv(&row[6], &CASE_HEADER);
        assert_eq!(local_rows.len(), 4, "{}: four sealed classes", row[0]);
        let candidate_witness = format!("CANDIDATE-{ordinal}-PRESEAL-V1");
        let authority =
            BTreeMap::from([(candidate_witness.clone(), (row[6].clone(), row[9].clone()))]);
        assert!(
            local_rows.iter().all(|case| case[1] == candidate_witness),
            "{candidate_id}: stable exact candidate witness ID"
        );
        close_case_rows(&local_rows, &authority);
    }
}

fn close_case_rows(rows: &[Vec<String>], authorities: &BTreeMap<String, (String, String)>) {
    let mut classes = BTreeSet::new();
    let mut case_ids = BTreeSet::new();
    for row in rows {
        assert!(case_ids.insert(row[0].clone()), "duplicate case {}", row[0]);
        assert!(classes.insert(row[2].clone()), "duplicate class {}", row[2]);
        let (ledger, policy) = authorities
            .get(&row[1])
            .unwrap_or_else(|| panic!("{}: unknown witness {}", row[0], row[1]));
        assert!(
            row[3].starts_with("contracts/w3-public-witnesses/"),
            "{}: input outside packet",
            row[0]
        );
        assert!(
            row[5].starts_with("contracts/w3-public-witnesses/"),
            "{}: expectation outside packet",
            row[0]
        );
        assert_sha(&row[3], &row[4]);
        assert_sha(&row[5], &row[6]);
        let _decoded = decode_input(&row[3]);
        validate_expectation(&row[5], &row[2], policy);
        assert_eq!(row[7], "PRESEALED_UNEXECUTED");
        let local = Path::new(ledger).parent().unwrap();
        assert_eq!(
            Path::new(&row[3]).parent().and_then(Path::parent),
            Some(local),
            "{}: input is not local to ledger",
            row[0]
        );
        assert_eq!(
            Path::new(&row[5]).parent().and_then(Path::parent),
            Some(local),
            "{}: expectation is not local to ledger",
            row[0]
        );
    }
    assert_eq!(
        classes,
        BTreeSet::from([
            "POSITIVE".to_owned(),
            "NEGATIVE".to_owned(),
            "RECOVERY_POLICY".to_owned(),
            "FULL_CONSUMPTION".to_owned(),
        ])
    );
}

fn public_instance(ordinal: &str) -> (Vec<u8>, Vec<u8>, Value) {
    assert!(
        PUBLIC_ORDINALS.contains(&ordinal),
        "only sealed public ordinals may cross compile_instance"
    );
    let directory = format!("contracts/w3-public-witnesses/public/{ordinal}");
    let manifest_bytes = bytes(&format!("{directory}/instance.json"));
    let source_bytes = bytes(&format!("{directory}/grammar.bbnf"));
    let manifest = strict_json_bytes(&manifest_bytes, &format!("{directory}/instance.json"))
        .expect("sealed instance JSON");
    (manifest_bytes, source_bytes, manifest)
}

#[derive(Clone, Copy)]
enum CompileAuthority {
    Public,
    Synthetic,
}

fn compile_authority(
    authority: CompileAuthority,
    manifest_bytes: &[u8],
    source_bytes: &[u8],
    at: &str,
) -> CompiledModule {
    let manifest =
        strict_json_bytes(manifest_bytes, at).unwrap_or_else(|error| panic!("{at}: {error}"));
    let logical_path = manifest["logical_path"]
        .as_str()
        .unwrap_or_else(|| panic!("{at}: logical_path"));
    match authority {
        CompileAuthority::Public => assert!(
            logical_path.starts_with("contracts/w3-public-witnesses/public/"),
            "{at}: public compile ingress"
        ),
        CompileAuthority::Synthetic => assert!(
            logical_path.starts_with("synthetic/"),
            "{at}: synthetic compile ingress"
        ),
    }
    let (candidate_sources, candidate_manifests) = candidate_deny_hashes();
    assert!(
        !candidate_manifests.contains(&sha256_hex(manifest_bytes))
            && !candidate_sources.contains(&sha256_hex(source_bytes)),
        "{at}: sealed candidate bytes reached the compiler boundary"
    );
    match compile_instance(
        GrammarInstanceInput::new(manifest_bytes, source_bytes),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(diagnostics.is_empty(), "{at}: accepted with diagnostics");
            value
        }
        StageResult::Rejected { diagnostics } => {
            let rendered: Vec<String> = diagnostics.iter().map(ToString::to_string).collect();
            panic!("{at}: compilation rejected: {rendered:?}");
        }
    }
}

fn compile_public(ordinal: &str) -> CompiledModule {
    let (manifest_bytes, source_bytes, _) = public_instance(ordinal);
    compile_authority(
        CompileAuthority::Public,
        &manifest_bytes,
        &source_bytes,
        &format!("public/{ordinal}"),
    )
}

fn synthetic_manifest(control: SyntheticControl) -> Vec<u8> {
    let pratt_tables = if control.pratt {
        json!([{
            "site_rule": "expression",
            "atom_rule": "atom",
            "operators": [{
                "matcher_rule": "plus",
                "fixity": "infix",
                "left_binding_power": 30,
                "right_binding_power": 31
            }]
        }])
    } else {
        json!([])
    };
    serde_json::to_vec(&json!({
        "schema": "bbnf.grammar-instance.v1",
        "logical_path": control.logical_path,
        "entry": control.entry,
        "trivia_regex": if control.pratt {
            Value::String("[ \\t]*".to_owned())
        } else {
            Value::Null
        },
        "pratt_tables": pratt_tables
    }))
    .expect("synthetic genericity manifest is serializable")
}

fn compile_synthetic(control: SyntheticControl) -> CompiledModule {
    compile_authority(
        CompileAuthority::Synthetic,
        &synthetic_manifest(control),
        control.source,
        &format!("synthetic genericity control {}", control.id),
    )
}

fn assert_generated_parse_module_boundary(label: &str, source: &str) {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct Calls {
        functions: BTreeSet<String>,
        parse_module_receivers: Vec<syn::Expr>,
        parse_full_calls: usize,
    }
    impl<'ast> syn::visit::Visit<'ast> for Calls {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let syn::Expr::Path(path) = call.func.as_ref()
                && let Some(segment) = path.path.segments.last()
            {
                self.functions.insert(segment.ident.to_string());
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            match call.method.to_string().as_str() {
                "parse_module" => self.parse_module_receivers.push((*call.receiver).clone()),
                "parse_full" => self.parse_full_calls += 1,
                _ => {}
            }
            syn::visit::visit_expr_method_call(self, call);
        }
    }

    fn single_binding(expression: &syn::Expr) -> Option<String> {
        let syn::Expr::Path(path) = expression else {
            return None;
        };
        (path.path.segments.len() == 1).then(|| path.path.segments[0].ident.to_string())
    }

    fn typed_bindings(function: &syn::ItemFn) -> Vec<String> {
        function
            .sig
            .inputs
            .iter()
            .filter_map(|argument| match argument {
                syn::FnArg::Typed(argument) => match argument.pat.as_ref() {
                    syn::Pat::Ident(binding)
                        if binding.by_ref.is_none()
                            && binding.mutability.is_none()
                            && binding.subpat.is_none() =>
                    {
                        Some(binding.ident.to_string())
                    }
                    _ => None,
                },
                syn::FnArg::Receiver(_) => None,
            })
            .collect()
    }

    fn call_path(expression: &syn::Expr) -> Option<Vec<String>> {
        let syn::Expr::Call(call) = expression else {
            return None;
        };
        let syn::Expr::Path(function) = call.func.as_ref() else {
            return None;
        };
        Some(
            function
                .path
                .segments
                .iter()
                .map(|segment| segment.ident.to_string())
                .collect(),
        )
    }

    fn tail_call(function: &syn::ItemFn) -> Option<&syn::ExprCall> {
        let syn::Stmt::Expr(syn::Expr::Call(call), None) = function.block.stmts.last()? else {
            return None;
        };
        Some(call)
    }

    fn tail_callee(function: &syn::ItemFn) -> Option<String> {
        let call = tail_call(function)?;
        let syn::Expr::Path(path) = call.func.as_ref() else {
            return None;
        };
        (path.path.segments.len() == 1).then(|| path.path.segments[0].ident.to_string())
    }

    fn run_with_sink_receiver(
        expression: &syn::Expr,
        input_binding: &str,
        sink_binding: &str,
    ) -> bool {
        let syn::Expr::Call(call) = expression else {
            return false;
        };
        let syn::Expr::Path(function) = call.func.as_ref() else {
            return false;
        };
        function.path.segments.len() == 2
            && function.path.segments[0].ident == "Run"
            && function.path.segments[1].ident == "with_sink"
            && call.args.len() == 2
            && single_binding(&call.args[0]).as_deref() == Some(input_binding)
            && single_binding(&call.args[1]).as_deref() == Some(sink_binding)
    }

    let syntax =
        syn::parse_file(source).unwrap_or_else(|error| panic!("{label}: generated Rust: {error}"));
    let functions: BTreeMap<String, &syn::ItemFn> = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function) => Some((function.sig.ident.to_string(), function)),
            _ => None,
        })
        .collect();
    let boundary_names: Vec<String> = functions
        .iter()
        .filter_map(|(name, function)| {
            let mut calls = Calls::default();
            calls.visit_block(&function.block);
            (!calls.parse_module_receivers.is_empty()).then(|| name.clone())
        })
        .collect();
    assert_eq!(
        boundary_names.len(),
        1,
        "{label}: the sole parse_module call has one enclosing callable"
    );
    let boundary_name = &boundary_names[0];
    let boundary = functions
        .get(boundary_name)
        .expect("discovered parse_module boundary exists");
    let boundary_bindings = typed_bindings(boundary);
    assert_eq!(
        boundary_bindings.len(),
        3,
        "{label}: live parse_module boundary has input, sink, and config ingress"
    );
    let Some(syn::Stmt::Expr(syn::Expr::MethodCall(tail), None)) = boundary.block.stmts.last()
    else {
        panic!("{label}: parse_with_sink does not tail-call a method");
    };
    assert_eq!(tail.method, "parse_module", "{label}: live boundary method");
    assert_eq!(
        tail.args.len(),
        1,
        "{label}: live boundary supplies exactly one generated parser closure"
    );
    assert!(
        matches!(&tail.args[0], syn::Expr::Closure(_)),
        "{label}: live boundary parser is the generated closure"
    );
    assert!(
        run_with_sink_receiver(&tail.receiver, &boundary_bindings[0], &boundary_bindings[1],),
        "{label}: live boundary is not Run::with_sink(...).parse_module"
    );
    let mut boundary_calls = Calls::default();
    boundary_calls.visit_block(&boundary.block);
    assert!(
        boundary_calls.functions.contains(&boundary_bindings[2])
            || boundary.block.stmts.iter().any(|statement| {
                let mut paths = Vec::new();
                struct BindingPaths<'a> {
                    binding: &'a str,
                    hits: &'a mut Vec<()>,
                }
                impl<'ast> syn::visit::Visit<'ast> for BindingPaths<'_> {
                    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
                        if path.path.is_ident(self.binding) {
                            self.hits.push(());
                        }
                        syn::visit::visit_expr_path(self, path);
                    }
                }
                let mut visitor = BindingPaths {
                    binding: &boundary_bindings[2],
                    hits: &mut paths,
                };
                visitor.visit_stmt(statement);
                !paths.is_empty()
            }),
        "{label}: live boundary config ingress is not consumed"
    );

    let mut whole_calls = Calls::default();
    whole_calls.visit_file(&syntax);
    assert_eq!(
        whole_calls.parse_module_receivers.len(),
        1,
        "{label}: exactly one structural parse_module call"
    );
    assert_eq!(
        whole_calls.parse_full_calls, 0,
        "{label}: generated module must not call parse_full"
    );

    for root_name in ["check", "parse_events"] {
        let root_function = functions
            .get(root_name)
            .unwrap_or_else(|| panic!("{label}: missing public {root_name} root"));
        assert!(
            matches!(root_function.vis, syn::Visibility::Public(_)),
            "{label}: {root_name} is not public"
        );
        let mut current = root_name.to_owned();
        let mut chain = Vec::new();
        let mut seen = BTreeSet::new();
        while current != *boundary_name {
            assert!(
                seen.insert(current.clone()),
                "{label}: {root_name} tail-value chain cycles at {current}"
            );
            chain.push(current.clone());
            let function = functions
                .get(&current)
                .unwrap_or_else(|| panic!("{label}: missing tail-value callable {current}"));
            current = tail_callee(function).unwrap_or_else(|| {
                panic!(
                    "{label}: {root_name} does not return a unique direct call on its path to {boundary_name}"
                )
            });
        }
        chain.push(boundary_name.clone());
        assert!(
            chain.len() >= 2,
            "{label}: {root_name} must traverse the shared live boundary"
        );
    }

    let check = functions.get("check").expect("public check");
    assert_eq!(
        check.block.stmts.len(),
        1,
        "{label}: check has only its live returned call"
    );
    let check_bindings = typed_bindings(check);
    assert_eq!(check_bindings.len(), 1, "{label}: check input ingress");
    let check_tail = tail_call(check).expect("check tail call");
    assert_eq!(check_tail.args.len(), 2, "{label}: check tail arguments");
    assert_eq!(
        single_binding(&check_tail.args[0]).as_deref(),
        Some(check_bindings[0].as_str()),
        "{label}: check forwards its exact input"
    );
    assert_eq!(
        call_path(&check_tail.args[1]).as_deref(),
        Some(["LimitsConfig".to_owned(), "default".to_owned()].as_slice()),
        "{label}: check supplies only the default limits"
    );
    let check_with_limits_name = tail_callee(check).expect("check limits boundary");
    let check_with_limits = functions
        .get(&check_with_limits_name)
        .expect("check limits callable");
    assert_eq!(
        check_with_limits.block.stmts.len(),
        1,
        "{label}: check limits has only its live returned call"
    );
    let check_limit_bindings = typed_bindings(check_with_limits);
    assert_eq!(
        check_limit_bindings.len(),
        2,
        "{label}: check limits input/config ingress"
    );
    let check_limit_tail = tail_call(check_with_limits).expect("check limits tail");
    assert_eq!(
        tail_callee(check_with_limits).as_deref(),
        Some(boundary_name.as_str()),
        "{label}: check limits returns the discovered live boundary"
    );
    assert_eq!(check_limit_tail.args.len(), 3);
    assert!(
        matches!(&check_limit_tail.args[0], syn::Expr::Call(call)
            if call_path(&syn::Expr::Call(call.clone())).as_deref()
                == Some(["Input".to_owned(), "new".to_owned()].as_slice())
                && call.args.len() == 1
                && single_binding(&call.args[0]).as_deref()
                    == Some(check_limit_bindings[0].as_str())),
        "{label}: check limits constructs Input only from its input ingress"
    );
    assert!(
        matches!(&check_limit_tail.args[1], syn::Expr::Call(call)
            if call_path(&syn::Expr::Call(call.clone())).as_deref()
                == Some(["NoSink".to_owned(), "new".to_owned()].as_slice())
                && call.args.is_empty()),
        "{label}: check limits constructs only NoSink"
    );
    assert_eq!(
        single_binding(&check_limit_tail.args[2]).as_deref(),
        Some(check_limit_bindings[1].as_str()),
        "{label}: check limits forwards its exact config"
    );

    let parse_events = functions.get("parse_events").expect("public parse_events");
    assert_eq!(
        parse_events.block.stmts.len(),
        1,
        "{label}: parse_events has only its live returned call"
    );
    let event_bindings = typed_bindings(parse_events);
    assert_eq!(
        event_bindings.len(),
        1,
        "{label}: parse_events input ingress"
    );
    let event_tail = tail_call(parse_events).expect("parse_events tail call");
    assert_eq!(event_tail.args.len(), 2);
    assert_eq!(
        single_binding(&event_tail.args[0]).as_deref(),
        Some(event_bindings[0].as_str()),
        "{label}: parse_events forwards its exact input"
    );
    assert_eq!(
        call_path(&event_tail.args[1]).as_deref(),
        Some(["LimitsConfig".to_owned(), "default".to_owned()].as_slice()),
        "{label}: parse_events supplies only the default limits"
    );
    let events_with_limits_name = tail_callee(parse_events).expect("events limits boundary");
    let events_with_limits = functions
        .get(&events_with_limits_name)
        .expect("events limits callable");
    assert_eq!(
        events_with_limits.block.stmts.len(),
        2,
        "{label}: event limits has only its Input capture and live returned call"
    );
    let event_limit_bindings = typed_bindings(events_with_limits);
    assert_eq!(
        event_limit_bindings.len(),
        2,
        "{label}: event limits input/config ingress"
    );
    let event_limit_tail = tail_call(events_with_limits).expect("event limits tail");
    assert_eq!(
        tail_callee(events_with_limits).as_deref(),
        Some(boundary_name.as_str()),
        "{label}: event limits returns the discovered live boundary"
    );
    assert_eq!(event_limit_tail.args.len(), 3);
    let source_binding =
        single_binding(&event_limit_tail.args[0]).expect("event limits forwards one Input local");
    let source_initializers: Vec<&syn::Expr> = events_with_limits
        .block
        .stmts
        .iter()
        .filter_map(|statement| match statement {
            syn::Stmt::Local(local)
                if matches!(&local.pat, syn::Pat::Ident(binding)
                    if binding.ident == source_binding.as_str()) =>
            {
                local.init.as_ref().map(|init| init.expr.as_ref())
            }
            _ => None,
        })
        .collect();
    assert_eq!(source_initializers.len(), 1);
    assert!(
        matches!(source_initializers[0], syn::Expr::Call(call)
            if call_path(&syn::Expr::Call(call.clone())).as_deref()
                == Some(["Input".to_owned(), "new".to_owned()].as_slice())
                && call.args.len() == 1
                && single_binding(&call.args[0]).as_deref()
                    == Some(event_limit_bindings[0].as_str())),
        "{label}: event limits source local derives only from its input ingress"
    );
    assert!(
        matches!(&event_limit_tail.args[1], syn::Expr::Call(call)
            if call_path(&syn::Expr::Call(call.clone())).as_deref()
                == Some(["EventBuffer".to_owned(), "new".to_owned()].as_slice())
                && call.args.len() == 1
                && single_binding(&call.args[0]).as_deref()
                    == Some(source_binding.as_str())),
        "{label}: event limits sink borrows the same Input local"
    );
    assert_eq!(
        single_binding(&event_limit_tail.args[2]).as_deref(),
        Some(event_limit_bindings[1].as_str()),
        "{label}: event limits forwards its exact config"
    );

    let recovery = syntax.items.iter().find_map(|item| match item {
        syn::Item::Enum(item) if item.ident == "RecoveryPolicy" => Some(item),
        _ => None,
    });
    let recovery = recovery.unwrap_or_else(|| panic!("{label}: missing RecoveryPolicy"));
    assert_eq!(
        recovery
            .variants
            .iter()
            .map(|variant| variant.ident.to_string())
            .collect::<Vec<_>>(),
        ["Disabled"],
        "{label}: RecoveryPolicy surface"
    );
    let recovery_constant = syntax.items.iter().find_map(|item| match item {
        syn::Item::Const(item) if item.ident == "RECOVERY_POLICY" => Some(item),
        _ => None,
    });
    let recovery_constant =
        recovery_constant.unwrap_or_else(|| panic!("{label}: missing RECOVERY_POLICY"));
    let syn::Expr::Path(disabled) = recovery_constant.expr.as_ref() else {
        panic!("{label}: RECOVERY_POLICY is not a path");
    };
    assert_eq!(
        disabled
            .path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>(),
        ["RecoveryPolicy", "Disabled"],
        "{label}: exact disabled recovery constant"
    );
    assert!(
        functions
            .keys()
            .all(|name| !name.to_ascii_lowercase().contains("recover")),
        "{label}: independent recovery dispatcher is forbidden"
    );
}

fn replay_stage_packets(compiled: &CompiledModule) -> [(&'static str, Vec<u8>); 10] {
    [
        (
            "bbnf.source-map.v4",
            compiled.source_map().replay_json().unwrap(),
        ),
        ("bbnf.program.v3", compiled.program().replay_json().unwrap()),
        (
            "bbnf.node-remap.v1",
            compiled.remap().replay_json().unwrap(),
        ),
        (
            "bbnf.dependency-solution.v2",
            compiled.dependency().replay_json().unwrap(),
        ),
        (
            "bbnf.type-solution.v3",
            compiled.types().replay_json().unwrap(),
        ),
        (
            "bbnf.analysis-facts.v4",
            compiled.facts().replay_json().unwrap(),
        ),
        (
            "bbnf.lowering-plan.v3",
            compiled.lowering().replay_json().unwrap(),
        ),
        (
            "bbnf.scalar-events.v5",
            compiled.events().replay_json().unwrap(),
        ),
        (
            "bbnf.generated-module.v5",
            compiled.generated().replay_json().unwrap(),
        ),
        ("bbnf.compiled-module.v6", compiled.replay_json().unwrap()),
    ]
}

fn tampered_replay(packet: &[u8], mutate: impl FnOnce(&mut Value)) -> Vec<u8> {
    let mut value: Value = serde_json::from_slice(packet).expect("canonical replay is JSON");
    mutate(&mut value);
    serde_json::to_vec(&value).expect("tampered replay remains JSON")
}

fn json_shape(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    }
}

fn mutate_same_shape_leaf(value: &mut Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(boolean) => {
            *boolean = !*boolean;
            true
        }
        Value::Number(number) => {
            if let Some(unsigned) = number.as_u64() {
                *number = unsigned.saturating_add(1).into();
            } else if let Some(signed) = number.as_i64() {
                *number = signed.saturating_add(1).into();
            } else {
                *number = serde_json::Number::from_f64(number.as_f64().unwrap() + 1.0)
                    .expect("finite replay number remains finite");
            }
            true
        }
        Value::String(string) => {
            string.push('~');
            true
        }
        Value::Array(values) => values.iter_mut().any(mutate_same_shape_leaf),
        Value::Object(values) => values.values_mut().any(mutate_same_shape_leaf),
    }
}

fn assert_replay_component_mismatch(
    packet: &[u8],
    options: CompileOptions,
    field: &str,
    component: ReplayComponent,
) {
    let original: Value = serde_json::from_slice(packet).expect("canonical replay is JSON");
    let original_component = original["value"][field].clone();
    let tampered = tampered_replay(packet, |value| {
        let carrier = &mut value["value"][field];
        assert!(
            mutate_same_shape_leaf(carrier),
            "{field}: carrier has a mutable non-null leaf"
        );
        assert_eq!(
            json_shape(carrier),
            json_shape(&original_component),
            "{field}: adversarial mutation preserves the carrier JSON shape"
        );
        assert_ne!(
            carrier, &original_component,
            "{field}: adversarial mutation changes carrier content"
        );
        assert!(
            !carrier.is_null(),
            "{field}: replay falsifier is not the prior null-only shortcut"
        );
    });
    assert_eq!(
        replay_compilation(&tampered, options),
        Err(ReplayError::ValueMismatch { component }),
        "trusted replay must route `{field}` tampering to its exact carrier"
    );
}

fn assert_no_forbidden_manifest_dimensions(value: &Value, at: &str) {
    match value {
        Value::Object(object) => {
            for (key, child) in object {
                let compact = key
                    .bytes()
                    .filter(u8::is_ascii_alphanumeric)
                    .map(|byte| byte.to_ascii_lowercase())
                    .collect::<Vec<_>>();
                let compact = std::str::from_utf8(&compact).expect("ASCII metadata key");
                for forbidden in [
                    "backend",
                    "callback",
                    "dependency",
                    "dependencies",
                    "dispatch",
                    "feature",
                    "format",
                    "host",
                    "native",
                    "plugin",
                    "registry",
                ] {
                    assert!(
                        !compact.contains(forbidden),
                        "{at}: forbidden generated-manifest dimension `{key}`"
                    );
                }
                assert_no_forbidden_manifest_dimensions(child, &format!("{at}.{key}"));
            }
        }
        Value::Array(array) => {
            for (index, child) in array.iter().enumerate() {
                assert_no_forbidden_manifest_dimensions(child, &format!("{at}[{index}]"));
            }
        }
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
    }
}

fn validate_generated_manifest(label: &str, compiled: &CompiledModule) {
    #[derive(serde::Serialize)]
    struct ExpectedGeneratedManifest<'a> {
        schema: &'static str,
        event_schema: &'a str,
        revision: String,
        source_blake3: String,
        source_bytes: usize,
        nodes: usize,
        rules: usize,
        regexes: usize,
        transforms: usize,
        fields: usize,
        trivia_dfas: usize,
        pratt_tables: usize,
        pratt_operators: usize,
    }

    let lowering = compiled.lowering();
    let events = compiled.events();
    let expected = serde_json::to_string(&ExpectedGeneratedManifest {
        schema: "bbnf.generated-rust.v6",
        event_schema: events.schema(),
        revision: compiled.source_map().revision().hex(),
        source_blake3: blake3::hash(compiled.generated().source().as_bytes())
            .to_hex()
            .to_string(),
        source_bytes: compiled.generated().source().len(),
        nodes: lowering.nodes().len(),
        rules: lowering.rules().len(),
        regexes: lowering.regexes().len(),
        transforms: events.transforms().len(),
        fields: lowering.fields().len(),
        trivia_dfas: usize::from(lowering.trivia().is_some()),
        pratt_tables: lowering.pratt_tables().len(),
        pratt_operators: lowering.pratt_operators().len(),
    })
    .expect("expected generated manifest is serializable");
    assert_eq!(
        compiled.generated().manifest(),
        expected,
        "{label}: generated manifest is the exact canonical closed serialization"
    );

    let manifest: Value = serde_json::from_str(compiled.generated().manifest())
        .unwrap_or_else(|error| panic!("{label}: generated manifest JSON: {error}"));
    assert_no_forbidden_manifest_dimensions(&manifest, label);
    let manifest = object(&manifest, label);
    exact_keys(
        manifest,
        &[
            "schema",
            "event_schema",
            "revision",
            "source_blake3",
            "source_bytes",
            "nodes",
            "rules",
            "regexes",
            "transforms",
            "fields",
            "trivia_dfas",
            "pratt_tables",
            "pratt_operators",
        ],
        label,
    );
    assert_eq!(
        manifest["schema"].as_str(),
        Some("bbnf.generated-rust.v6"),
        "{label}: generated schema"
    );
    assert_eq!(
        manifest["event_schema"].as_str(),
        Some("bbnf.scalar-events.v5"),
        "{label}: event schema"
    );
    let expected_value: Value =
        serde_json::from_str(&expected).expect("expected generated manifest JSON");
    assert_eq!(
        Value::Object(manifest.clone()),
        expected_value,
        "{label}: every generated manifest field is bound to its owning carrier"
    );
}

fn assert_owned_parse_that_parse_module_contract(source: &str) {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct DelegationCalls {
        parse_full_receivers: Vec<syn::Expr>,
        module_result_new: usize,
        function_calls: Vec<Vec<String>>,
        method_calls: Vec<String>,
        closures: usize,
        macros: Vec<String>,
        self_fields: Vec<String>,
        simple_expression_paths: Vec<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for DelegationCalls {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let syn::Expr::Path(path) = call.func.as_ref() {
                let segments: Vec<String> = path
                    .path
                    .segments
                    .iter()
                    .map(|segment| segment.ident.to_string())
                    .collect();
                if segments
                    .iter()
                    .map(String::as_str)
                    .eq(["ModuleResult", "new"])
                {
                    self.module_result_new += 1;
                }
                self.function_calls.push(segments);
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.method_calls.push(call.method.to_string());
            if call.method == "parse_full" {
                self.parse_full_receivers.push((*call.receiver).clone());
            }
            syn::visit::visit_expr_method_call(self, call);
        }

        fn visit_expr_closure(&mut self, closure: &'ast syn::ExprClosure) {
            self.closures += 1;
            syn::visit::visit_expr_closure(self, closure);
        }

        fn visit_macro(&mut self, item: &'ast syn::Macro) {
            self.macros.push(
                item.path
                    .segments
                    .iter()
                    .map(|segment| segment.ident.to_string())
                    .collect::<Vec<_>>()
                    .join("::"),
            );
            syn::visit::visit_macro(self, item);
        }

        fn visit_expr_field(&mut self, field: &'ast syn::ExprField) {
            if path_is(&field.base, &["self"])
                && let syn::Member::Named(name) = &field.member
            {
                self.self_fields.push(name.to_string());
            }
            syn::visit::visit_expr_field(self, field);
        }

        fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
            if path.path.segments.len() == 1
                && let Some(segment) = path.path.segments.first()
            {
                self.simple_expression_paths.push(segment.ident.to_string());
            }
            syn::visit::visit_expr_path(self, path);
        }
    }

    fn path_is(expression: &syn::Expr, expected: &[&str]) -> bool {
        matches!(
            expression,
            syn::Expr::Path(path)
                if path.path.segments.len() == expected.len()
                    && path.path.segments.iter().zip(expected).all(
                        |(segment, expected)| segment.ident == *expected
                    )
        )
    }

    fn single_path_binding(expression: &syn::Expr) -> Option<String> {
        let syn::Expr::Path(path) = expression else {
            return None;
        };
        if path.path.segments.len() != 1 {
            return None;
        }
        path.path
            .segments
            .first()
            .map(|segment| segment.ident.to_string())
    }

    fn self_field_name(expression: &syn::Expr) -> Option<String> {
        let syn::Expr::Field(field) = expression else {
            return None;
        };
        if !path_is(&field.base, &["self"]) {
            return None;
        }
        match &field.member {
            syn::Member::Named(name) => Some(name.to_string()),
            syn::Member::Unnamed(_) => None,
        }
    }

    fn local_initializer<'a>(statements: &'a [syn::Stmt], binding: &str) -> &'a syn::Expr {
        let matches: Vec<&syn::Local> = statements
            .iter()
            .filter_map(|statement| match statement {
                syn::Stmt::Local(local)
                    if matches!(&local.pat, syn::Pat::Ident(candidate)
                        if candidate.ident == binding) =>
                {
                    Some(local)
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            matches.len(),
            1,
            "Run::parse_module dataflow binding `{binding}` must have one definition"
        );
        let initializer = matches[0]
            .init
            .as_ref()
            .unwrap_or_else(|| panic!("Run::parse_module `{binding}` has no initializer"));
        assert!(
            initializer.diverge.is_none(),
            "Run::parse_module `{binding}` uses a let-else side path"
        );
        &initializer.expr
    }

    fn step_pattern_argument<'a>(pattern: &'a syn::Pat, variant: &str) -> &'a syn::Pat {
        let syn::Pat::TupleStruct(tuple) = pattern else {
            panic!("Run::parse_module `{variant}` arm does not use a tuple-struct pattern");
        };
        assert!(
            tuple.path.segments.len() == 2
                && tuple.path.segments[0].ident == "Step"
                && tuple.path.segments[1].ident == variant,
            "Run::parse_module `{variant}` arm pattern has the wrong Step variant"
        );
        assert_eq!(
            tuple.elems.len(),
            1,
            "Run::parse_module `{variant}` arm pattern arity"
        );
        &tuple.elems[0]
    }

    fn step_constructor<'a>(expression: &'a syn::Expr, variant: &str) -> &'a syn::ExprCall {
        let syn::Expr::Call(call) = expression else {
            panic!("Run::parse_module `{variant}` arm is not a Step constructor");
        };
        assert!(
            path_is(&call.func, &["Step", variant]),
            "Run::parse_module `{variant}` arm constructs the wrong outcome"
        );
        assert_eq!(
            call.args.len(),
            1,
            "Run::parse_module `{variant}` constructor arity"
        );
        call
    }

    let syntax = syn::parse_file(source).expect("owned parse-that run.rs syntax");
    let methods: Vec<&syn::ImplItemFn> = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item)
                if matches!(
                    item.self_ty.as_ref(),
                    syn::Type::Path(path)
                        if path.path.segments.last().is_some_and(|segment| segment.ident == "Run")
                ) =>
            {
                Some(item)
            }
            _ => None,
        })
        .flat_map(|item| item.items.iter())
        .filter_map(|item| match item {
            syn::ImplItem::Fn(method) if method.sig.ident == "parse_module" => Some(method),
            _ => None,
        })
        .collect();
    assert_eq!(methods.len(), 1, "one owned Run::parse_module definition");
    assert!(
        matches!(methods[0].vis, syn::Visibility::Public(_)),
        "Run::parse_module is public"
    );
    assert!(
        matches!(
            methods[0].sig.inputs.first(),
            Some(syn::FnArg::Receiver(receiver))
                if receiver.reference.is_none() && receiver.mutability.is_some()
        ),
        "Run::parse_module consumes mutable self"
    );
    let parser_parameters: Vec<&syn::PatType> = methods[0]
        .sig
        .inputs
        .iter()
        .filter_map(|argument| match argument {
            syn::FnArg::Typed(argument) => Some(argument),
            syn::FnArg::Receiver(_) => None,
        })
        .collect();
    assert_eq!(
        parser_parameters.len(),
        1,
        "Run::parse_module has exactly one typed parser ingress"
    );
    let syn::Pat::Ident(parser_pattern) = parser_parameters[0].pat.as_ref() else {
        panic!("Run::parse_module parser ingress must be one structural binding");
    };
    assert!(
        parser_pattern.by_ref.is_none()
            && parser_pattern.mutability.is_none()
            && parser_pattern.subpat.is_none(),
        "Run::parse_module parser ingress cannot alias or destructure its binding"
    );
    let parser_binding = parser_pattern.ident.to_string();

    let [
        syn::Stmt::Local(input_statement),
        syn::Stmt::Local(outcome_statement),
        syn::Stmt::Expr(tail, None),
    ] = methods[0].block.stmts.as_slice()
    else {
        panic!(
            "Run::parse_module must be exactly source capture, delegated outcome, and tail ModuleResult"
        );
    };
    let syn::Expr::Call(result) = tail else {
        panic!("Run::parse_module tail is not ModuleResult::new");
    };
    assert!(
        path_is(&result.func, &["ModuleResult", "new"]),
        "Run::parse_module tail constructor"
    );
    assert_eq!(result.args.len(), 3, "ModuleResult::new argument count");
    let input_binding = single_path_binding(&result.args[0])
        .expect("Run::parse_module constructor source must be a local binding");
    let outcome_binding = single_path_binding(&result.args[1])
        .expect("Run::parse_module constructor outcome must be a local binding");
    assert!(
        matches!(&input_statement.pat, syn::Pat::Ident(binding)
            if binding.ident == input_binding.as_str()),
        "Run::parse_module first statement defines the constructor source binding"
    );
    assert!(
        matches!(&outcome_statement.pat, syn::Pat::Ident(binding)
            if binding.ident == outcome_binding.as_str()),
        "Run::parse_module second statement defines the constructor outcome binding"
    );
    let input_field = self_field_name(local_initializer(&methods[0].block.stmts, &input_binding))
        .expect("Run::parse_module source binding must capture one self field");
    let sink_field = self_field_name(&result.args[2])
        .expect("Run::parse_module constructor sink must move one self field");
    assert_ne!(
        input_field, sink_field,
        "Run::parse_module source and sink must remain distinct Run fields"
    );

    let syn::Expr::Match(delegated_outcome) =
        local_initializer(&methods[0].block.stmts, &outcome_binding)
    else {
        panic!("Run::parse_module outcome is not the delegated match");
    };
    let syn::Expr::MethodCall(parse_full) = delegated_outcome.expr.as_ref() else {
        panic!("Run::parse_module outcome does not directly match self.parse_full(parser)");
    };
    assert_eq!(parse_full.method, "parse_full");
    assert!(
        path_is(&parse_full.receiver, &["self"]),
        "Run::parse_module delegates through self.parse_full"
    );
    assert_eq!(
        parse_full.args.len(),
        1,
        "Run::parse_module supplies one parser to parse_full"
    );
    assert!(
        single_path_binding(&parse_full.args[0]).as_deref() == Some(parser_binding.as_str()),
        "Run::parse_module must delegate its parser parameter unchanged"
    );
    let [matched_arm, mismatch_arm, fault_arm] = delegated_outcome.arms.as_slice() else {
        panic!("Run::parse_module delegated match must have exactly three Step arms");
    };
    assert!(
        delegated_outcome.arms.iter().all(|arm| arm.guard.is_none()),
        "Run::parse_module Step propagation cannot be conditional"
    );
    assert!(
        matches!(
            step_pattern_argument(&matched_arm.pat, "Matched"),
            syn::Pat::Wild(_)
        ),
        "Run::parse_module discards only the delegated matched value"
    );
    let matched = step_constructor(&matched_arm.body, "Matched");
    let syn::Expr::MethodCall(span) = &matched.args[0] else {
        panic!("Run::parse_module matched arm does not return the complete input span");
    };
    assert_eq!(span.method, "span_unchecked");
    assert_eq!(
        single_path_binding(&span.receiver).as_deref(),
        Some(input_binding.as_str())
    );
    assert_eq!(span.args.len(), 2, "complete-span argument count");
    assert!(
        matches!(&span.args[0], syn::Expr::Lit(literal)
            if matches!(&literal.lit, syn::Lit::Int(value) if value.base10_digits() == "0")),
        "Run::parse_module complete span begins at zero"
    );
    let syn::Expr::MethodCall(input_len) = &span.args[1] else {
        panic!("Run::parse_module complete span does not end at input.len()");
    };
    assert_eq!(input_len.method, "len");
    assert_eq!(
        single_path_binding(&input_len.receiver).as_deref(),
        Some(input_binding.as_str())
    );
    assert!(
        input_len.args.is_empty(),
        "Run::parse_module input.len receives no argument"
    );
    for (arm, variant) in [(mismatch_arm, "Mismatch"), (fault_arm, "Fault")] {
        let syn::Pat::Ident(identifier) = step_pattern_argument(&arm.pat, variant) else {
            panic!("Run::parse_module `{variant}` arm does not bind its delegated payload");
        };
        assert!(
            identifier.by_ref.is_none()
                && identifier.mutability.is_none()
                && identifier.subpat.is_none(),
            "Run::parse_module `{variant}` payload binding must be moved unchanged"
        );
        let binding = identifier.ident.to_string();
        let constructor = step_constructor(&arm.body, variant);
        assert!(
            single_path_binding(&constructor.args[0]).as_deref() == Some(binding.as_str()),
            "Run::parse_module `{variant}` arm changes `{binding}`"
        );
    }

    let mut calls = DelegationCalls::default();
    calls.visit_block(&methods[0].block);
    assert_eq!(
        calls.parse_full_receivers.len(),
        1,
        "Run::parse_module delegates exactly once to parse_full"
    );
    assert!(
        matches!(&calls.parse_full_receivers[0], syn::Expr::Path(path)
            if path.path.is_ident("self")),
        "Run::parse_module delegates through self.parse_full"
    );
    assert_eq!(
        calls.module_result_new, 1,
        "Run::parse_module returns one ModuleResult built from the delegated outcome"
    );
    assert_eq!(
        calls.closures, 0,
        "Run::parse_module cannot conceal independent parser work in a closure"
    );
    assert!(
        calls.macros.is_empty(),
        "Run::parse_module cannot conceal independent parser/transaction work in macros: {:?}",
        calls.macros
    );
    assert_eq!(
        calls
            .simple_expression_paths
            .iter()
            .filter(|binding| binding.as_str() == parser_binding.as_str())
            .count(),
        1,
        "Run::parse_module parser ingress is live only at self.parse_full"
    );
    calls.self_fields.sort();
    let mut expected_self_fields = vec![input_field, sink_field];
    expected_self_fields.sort();
    assert_eq!(
        calls.self_fields, expected_self_fields,
        "Run::parse_module touches only its structurally derived source and sink fields"
    );
    calls.method_calls.sort();
    assert_eq!(
        calls.method_calls,
        vec![
            "len".to_owned(),
            "parse_full".to_owned(),
            "span_unchecked".to_owned(),
        ],
        "Run::parse_module has no independent method work"
    );
    let mut function_calls: Vec<String> = calls
        .function_calls
        .iter()
        .map(|segments| segments.join("::"))
        .collect();
    function_calls.sort();
    assert_eq!(
        function_calls,
        vec![
            "ModuleResult::new".to_owned(),
            "Step::Fault".to_owned(),
            "Step::Matched".to_owned(),
            "Step::Mismatch".to_owned(),
        ],
        "Run::parse_module has no independent parser/transaction function work"
    );
}

/// The parse_that package directory this workspace's lock pins (LW3.COMP-1b:
/// the sk-v26 `../parse-that-skv26` sibling edge became a git+rev edge),
/// resolved through `cargo metadata` rather than a sibling checkout.
fn pinned_parse_that_package_dir() -> PathBuf {
    let output = command_output(
        Command::new(env!("CARGO"))
            .args(["metadata", "--locked", "--offline", "--format-version", "1"])
            .current_dir(root()),
        "pinned parse_that metadata",
    );
    let metadata: Value =
        serde_json::from_slice(&output.stdout).expect("pinned parse_that metadata JSON");
    let packages = metadata["packages"]
        .as_array()
        .expect("metadata packages")
        .iter()
        .filter(|package| package["name"] == "parse_that")
        .collect::<Vec<_>>();
    assert_eq!(
        packages.len(),
        1,
        "the lock resolves exactly one parse_that"
    );
    assert!(
        packages[0]["source"].as_str().is_some_and(
            |source| source.starts_with("git+https://github.com/mkbabb/parse-that?rev=")
        ),
        "parse_that is a git+rev edge: {}",
        packages[0]["source"]
    );
    PathBuf::from(
        packages[0]["manifest_path"]
            .as_str()
            .expect("parse_that manifest path"),
    )
    .parent()
    .expect("parse_that package dir")
    .to_path_buf()
}

#[test]
fn owned_parse_that_parse_module_shape_is_live_name_agnostic_and_self_falsifying() {
    let run_source = pinned_parse_that_package_dir().join("src/kernel/run.rs");
    let owned = fs::read_to_string(&run_source)
        .unwrap_or_else(|error| panic!("{}: {error}", run_source.display()));
    assert_owned_parse_that_parse_module_contract(&owned);

    let renamed = r#"
impl Run {
    pub fn parse_module<T, F>(mut self, callback: F) -> ModuleResult {
        let original_bytes = self.bytes;
        let final_step = match self.parse_full(callback) {
            Step::Matched(_) => Step::Matched(
                original_bytes.span_unchecked(0, original_bytes.len())
            ),
            Step::Mismatch(no_match) => Step::Mismatch(no_match),
            Step::Fault(problem) => Step::Fault(problem),
        };
        ModuleResult::new(original_bytes, final_step, self.output)
    }
}
"#;
    assert_owned_parse_that_parse_module_contract(renamed);

    let dead_decoy = r#"
impl Run {
    pub fn parse_module<T, F>(mut self, callback: F) -> ModuleResult {
        let original_bytes = self.bytes;
        let final_step = if false {
            self.parse_full(callback)
        } else {
            unreachable!()
        };
        ModuleResult::new(original_bytes, final_step, self.output)
    }
}
"#;
    assert!(
        std::panic::catch_unwind(|| {
            assert_owned_parse_that_parse_module_contract(dead_decoy);
        })
        .is_err(),
        "a dead parse_full decoy must not satisfy the owned live delegation law"
    );

    let drop_guard = renamed.replacen(
        "let final_step =",
        "let _guard = SideEffectOnDrop;\n        let final_step =",
        1,
    );
    assert!(
        std::panic::catch_unwind(|| {
            assert_owned_parse_that_parse_module_contract(&drop_guard);
        })
        .is_err(),
        "a call-free Drop guard is still independent transaction work"
    );
}

#[test]
fn exactly_three_public_instances_compile_twice_to_byte_identical_carriers() {
    for ordinal in PUBLIC_ORDINALS {
        let first = compile_public(ordinal);
        let second = compile_public(ordinal);
        validate_generated_manifest(&format!("public/{ordinal} generated manifest"), &first);
        assert_eq!(
            first.generated().source(),
            second.generated().source(),
            "public/{ordinal}: generated Rust identity"
        );
        assert_eq!(
            first.generated().manifest(),
            second.generated().manifest(),
            "public/{ordinal}: generated manifest identity"
        );
        assert_eq!(
            first.replay_json().unwrap(),
            second.replay_json().unwrap(),
            "public/{ordinal}: complete replay identity"
        );
    }
}

#[test]
fn synthetic_genericity_controls_have_fresh_hash_path_entry_and_order_identities() {
    let public_revisions: BTreeSet<[u8; 32]> = PUBLIC_ORDINALS
        .iter()
        .map(|ordinal| compile_public(ordinal).source_map().revision().bytes())
        .collect();
    let mut synthetic_revisions = BTreeSet::new();
    for control in SYNTHETIC_CONTROLS {
        let compiled = compile_synthetic(control);
        validate_generated_manifest(&format!("{} generated manifest", control.id), &compiled);
        let revision = compiled.source_map().revision().bytes();
        assert!(
            !public_revisions.contains(&revision),
            "{} accidentally reuses a public revision hash",
            control.id
        );
        assert!(
            synthetic_revisions.insert(revision),
            "{} does not have a fresh revision hash",
            control.id
        );
        assert_generated_parse_module_boundary(control.id, compiled.generated().source());
    }
    assert_eq!(synthetic_revisions.len(), SYNTHETIC_CONTROLS.len());
    let rule_names = |module: &CompiledModule| {
        module
            .program()
            .rules()
            .iter()
            .map(|rule| rule.name().to_owned())
            .collect::<Vec<_>>()
    };
    let baseline = compile_synthetic(SYNTHETIC_CONTROLS[0]);
    let permuted = compile_synthetic(SYNTHETIC_CONTROLS[1]);
    let renamed = compile_synthetic(SYNTHETIC_CONTROLS[2]);
    let baseline_rules = rule_names(&baseline);
    let permuted_rules = rule_names(&permuted);
    let renamed_rules = rule_names(&renamed);
    assert_eq!(
        baseline_rules.iter().cloned().collect::<BTreeSet<_>>(),
        permuted_rules.iter().cloned().collect::<BTreeSet<_>>(),
        "rule-order control preserves the complete rule-name set"
    );
    assert!(
        baseline_rules
            .iter()
            .all(|name| !renamed_rules.contains(name)),
        "full-rename control renames every rule"
    );
    assert_eq!(
        baseline_rules.len(),
        renamed_rules.len(),
        "full-rename control preserves grammar topology width"
    );
    assert_eq!(SYNTHETIC_CONTROLS[0].entry, SYNTHETIC_CONTROLS[1].entry);
    assert_ne!(
        SYNTHETIC_CONTROLS[0].source, SYNTHETIC_CONTROLS[1].source,
        "rule-order permutation changes exact source bytes"
    );
    assert_ne!(
        SYNTHETIC_CONTROLS[0].entry, SYNTHETIC_CONTROLS[2].entry,
        "full rename changes entry identity"
    );
    assert_eq!(
        (
            SYNTHETIC_CONTROLS[3].logical_path,
            SYNTHETIC_CONTROLS[3].entry,
        ),
        (
            SYNTHETIC_CONTROLS[4].logical_path,
            SYNTHETIC_CONTROLS[4].entry,
        ),
        "delimiter twins deliberately share the same misleading path and entry labels"
    );
    assert_ne!(
        SYNTHETIC_CONTROLS[3].source, SYNTHETIC_CONTROLS[4].source,
        "delimiter twins vary only source-defined grammar behavior"
    );
}

fn count_object_key(value: &Value, key: &str) -> usize {
    match value {
        Value::Object(object) => {
            usize::from(object.contains_key(key))
                + object
                    .values()
                    .map(|child| count_object_key(child, key))
                    .sum::<usize>()
        }
        Value::Array(array) => array.iter().map(|child| count_object_key(child, key)).sum(),
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => 0,
    }
}

#[test]
fn w3_schema_matrix_and_trusted_replay_retain_instance_and_source() {
    for ordinal in PUBLIC_ORDINALS {
        let compiled = compile_public(ordinal);
        let revision = compiled.source_map().revision().hex();
        let stage_packets = replay_stage_packets(&compiled);
        for (schema, packet) in &stage_packets {
            let envelope: Value =
                serde_json::from_slice(packet).expect("stage replay packet is JSON");
            let envelope = object(&envelope, "stage replay envelope");
            exact_keys(
                envelope,
                &["schema", "revision", "value"],
                &format!("public/{ordinal}: {schema} envelope"),
            );
            assert_eq!(envelope["schema"].as_str(), Some(*schema));
            assert_eq!(envelope["revision"].as_str(), Some(revision.as_str()));
            assert!(
                !envelope["value"].is_null(),
                "public/{ordinal}: {schema} replay value is non-null"
            );
        }
        assert!(
            compiled
                .generated()
                .source()
                .starts_with("// @generated by bbnf; schema=bbnf.generated-rust.v6;"),
            "public/{ordinal}: generated Rust schema"
        );
        validate_generated_manifest(
            &format!("public/{ordinal} replay generated manifest"),
            &compiled,
        );

        let (manifest_bytes, source_bytes, manifest) = public_instance(ordinal);
        let packet = compiled.replay_json().unwrap();
        let packet_value: Value = serde_json::from_slice(&packet).unwrap();
        let envelope = object(&packet_value, "complete replay envelope");
        exact_keys(
            envelope,
            &["schema", "revision", "value"],
            &format!("public/{ordinal}: complete replay envelope"),
        );
        let complete = object(&packet_value["value"], "complete replay value");
        exact_keys(
            complete,
            &[
                "options",
                "instance",
                "source",
                "source_map",
                "program",
                "remap",
                "dependency",
                "types",
                "facts",
                "lowering",
                "events",
                "generated_source",
                "generated_manifest",
            ],
            &format!("public/{ordinal}: complete replay value"),
        );
        assert_eq!(
            packet_value["value"]["instance"], manifest,
            "public/{ordinal}: complete replay must retain the typed instance"
        );
        assert_eq!(
            packet_value["value"]["source"],
            json!(source_bytes),
            "public/{ordinal}: top-level replay source is the sole byte authority"
        );
        let source_modules = packet_value["value"]["source_map"]["modules"]
            .as_array()
            .expect("source-map modules");
        assert_eq!(
            source_modules.len(),
            1,
            "public/{ordinal}: grammar instance has exactly one source-map module"
        );
        for (index, module) in source_modules.iter().enumerate() {
            let module = object(module, "complete replay source-map module");
            assert!(
                !module.contains_key("logical_path") && !module.contains_key("bytes"),
                "public/{ordinal}: source_map.modules[{index}] duplicates instance/source authority"
            );
        }
        assert_eq!(
            count_object_key(&packet_value["value"], "logical_path"),
            1,
            "public/{ordinal}: instance is the sole logical-path authority"
        );
        assert_eq!(
            count_object_key(&packet_value["value"], "source"),
            1,
            "public/{ordinal}: complete replay has one exact source-byte authority"
        );

        let rebuilt =
            replay_compilation(&packet, CompileOptions::default()).expect("trusted replay inverse");
        assert_eq!(
            rebuilt, compiled,
            "public/{ordinal}: trusted replay reconstructs the exact compiled carrier"
        );
        assert_eq!(
            replay_stage_packets(&rebuilt),
            stage_packets,
            "public/{ordinal}: every rebuilt stage packet equals its original"
        );
        assert_eq!(rebuilt.replay_json().unwrap(), packet);
        let mut changed_instance = packet_value.clone();
        let alternate_entry = PUBLIC_ALTERNATE_ENTRIES
            .iter()
            .find_map(|(candidate, entry)| (*candidate == ordinal).then_some(*entry))
            .expect("public alternate entry");
        changed_instance["value"]["instance"]["entry"] = Value::String(alternate_entry.to_owned());
        let changed_manifest = serde_json::to_vec(&changed_instance["value"]["instance"]).unwrap();
        let _accepted_changed_instance = compile_authority(
            CompileAuthority::Public,
            &changed_manifest,
            &source_bytes,
            &format!("public/{ordinal}: alternate existing entry {alternate_entry}"),
        );
        assert!(
            matches!(
                replay_compilation(
                    &serde_json::to_vec(&changed_instance).unwrap(),
                    CompileOptions::default()
                ),
                Err(ReplayError::RevisionMismatch { .. })
            ),
            "public/{ordinal}: replay routes a trusted mutated instance to revision mismatch"
        );

        let mut changed_source = packet_value;
        let mut syntax_valid_source = source_bytes.clone();
        syntax_valid_source.extend_from_slice(b"\n// syntax-valid replay integrity mutation\n");
        let _accepted_changed_source = compile_authority(
            CompileAuthority::Public,
            &manifest_bytes,
            &syntax_valid_source,
            &format!("public/{ordinal}: syntax-valid comment/whitespace source mutation"),
        );
        changed_source["value"]["source"] = json!(syntax_valid_source);
        assert!(
            matches!(
                replay_compilation(
                    &serde_json::to_vec(&changed_source).unwrap(),
                    CompileOptions::default()
                ),
                Err(ReplayError::RevisionMismatch { .. })
            ),
            "public/{ordinal}: replay routes a trusted mutated source to revision mismatch"
        );

        assert!(
            !manifest_bytes.is_empty(),
            "public/{ordinal}: manifest transport is nonempty"
        );
    }
}

#[test]
fn trusted_replay_rejects_every_w3_authority_carrier_and_encoding_tamper() {
    let compiled = compile_synthetic(SYNTHETIC_CONTROLS[0]);
    let packet = compiled.replay_json().expect("complete replay packet");
    let options = CompileOptions::default();

    let schema = tampered_replay(&packet, |value| {
        value["schema"] = json!("bbnf.compiled-module.v0");
    });
    assert!(matches!(
        replay_compilation(&schema, options),
        Err(ReplayError::SchemaMismatch { .. })
    ));

    let malformed_revision = tampered_replay(&packet, |value| {
        value["revision"] = json!("not-a-canonical-revision");
    });
    assert!(matches!(
        replay_compilation(&malformed_revision, options),
        Err(ReplayError::InvalidRevision { .. })
    ));

    let wrong_revision = tampered_replay(&packet, |value| {
        value["revision"] =
            json!("0000000000000000000000000000000000000000000000000000000000000000");
    });
    assert!(matches!(
        replay_compilation(&wrong_revision, options),
        Err(ReplayError::RevisionMismatch { .. })
    ));

    let packet_options = tampered_replay(&packet, |value| {
        let options = value["value"]["options"]
            .as_object_mut()
            .expect("typed replay options");
        let current = options["source_bytes"]
            .as_u64()
            .expect("CompileOptions.source_bytes is an unsigned budget");
        options["source_bytes"] = json!(current.saturating_add(1));
    });
    assert_eq!(
        replay_compilation(&packet_options, options),
        Err(ReplayError::ValueMismatch {
            component: ReplayComponent::CompileOptions
        })
    );
    let mut caller_options = options;
    caller_options.source_bytes += 1;
    assert_eq!(
        replay_compilation(&packet, caller_options),
        Err(ReplayError::ValueMismatch {
            component: ReplayComponent::CompileOptions
        })
    );

    for (field, component) in [
        ("source_map", ReplayComponent::SourceMap),
        ("program", ReplayComponent::Program),
        ("remap", ReplayComponent::NodeRemap),
        ("dependency", ReplayComponent::Dependency),
        ("types", ReplayComponent::Types),
        ("facts", ReplayComponent::Analysis),
        ("lowering", ReplayComponent::Lowering),
        ("events", ReplayComponent::Events),
        ("generated_source", ReplayComponent::GeneratedSource),
        ("generated_manifest", ReplayComponent::GeneratedManifest),
    ] {
        assert_replay_component_mismatch(&packet, options, field, component);
    }

    let changed_instance = tampered_replay(&packet, |value| {
        value["value"]["instance"]["logical_path"] =
            json!("contracts/w3/replay-mutated-authority.bbnf");
    });
    assert!(matches!(
        replay_compilation(&changed_instance, options),
        Err(ReplayError::RevisionMismatch { .. })
    ));

    let changed_source = tampered_replay(&packet, |value| {
        let source = value["value"]["source"]
            .as_array_mut()
            .expect("top-level source authority");
        source.extend([json!(b'\n'), json!(b'/'), json!(b'/')]);
    });
    assert!(matches!(
        replay_compilation(&changed_source, options),
        Err(ReplayError::RevisionMismatch { .. })
    ));

    for suffix in [b"\n".as_slice(), b" ".as_slice()] {
        let mut noncanonical = packet.clone();
        noncanonical.extend_from_slice(suffix);
        assert_eq!(
            replay_compilation(&noncanonical, options),
            Err(ReplayError::NonCanonical),
            "trailing JSON whitespace is never a second canonical encoding"
        );
    }

    let mut unknown_envelope = serde_json::from_slice::<Value>(&packet).unwrap();
    unknown_envelope["unknown"] = json!(0);
    assert_eq!(
        replay_compilation(&serde_json::to_vec(&unknown_envelope).unwrap(), options),
        Err(ReplayError::NonCanonical),
        "unknown replay-envelope fields are noncanonical"
    );
    let mut unknown_value = serde_json::from_slice::<Value>(&packet).unwrap();
    unknown_value["value"]["unknown"] = json!(0);
    assert_eq!(
        replay_compilation(&serde_json::to_vec(&unknown_value).unwrap(), options),
        Err(ReplayError::NonCanonical),
        "unknown complete-value fields are noncanonical"
    );

    let canonical = std::str::from_utf8(&packet).expect("canonical replay is UTF-8 JSON");
    let reordered = format!(
        "{{\"value\":{},\"revision\":{},\"schema\":{}}}",
        serde_json::to_string(&serde_json::from_slice::<Value>(&packet).unwrap()["value"]).unwrap(),
        serde_json::to_string(&serde_json::from_slice::<Value>(&packet).unwrap()["revision"])
            .unwrap(),
        serde_json::to_string(&serde_json::from_slice::<Value>(&packet).unwrap()["schema"])
            .unwrap(),
    );
    assert_ne!(reordered.as_bytes(), canonical.as_bytes());
    assert_eq!(
        replay_compilation(reordered.as_bytes(), options),
        Err(ReplayError::NonCanonical),
        "reordered replay-envelope fields are noncanonical"
    );

    let duplicate = canonical.replacen(
        "{\"schema\":",
        "{\"schema\":\"bbnf.compiled-module.v6\",\"schema\":",
        1,
    );
    assert_ne!(duplicate.as_bytes(), packet);
    assert_eq!(
        replay_compilation(duplicate.as_bytes(), options),
        Err(ReplayError::NonCanonical)
    );

    let mut trailing_garbage = packet;
    trailing_garbage.extend_from_slice(b"x");
    assert!(matches!(
        replay_compilation(&trailing_garbage, options),
        Err(ReplayError::InvalidJson { .. })
    ));
}

fn rust_bytes(value: &[u8]) -> String {
    let mut source = String::from("&[");
    for (index, byte) in value.iter().enumerate() {
        if index != 0 {
            source.push(',');
        }
        write!(source, "0x{byte:02x}").unwrap();
    }
    source.push(']');
    source
}

fn expectation_observation_source(observation: &Value) -> String {
    let kind = observation["kind"]
        .as_str()
        .expect("validated observation kind");
    match kind {
        "TOKEN" => format!(
            "Observation::Token{{symbol:{:?},lexeme:{}}}",
            observation["symbol"].as_str().unwrap(),
            rust_bytes(observation["lexeme"].as_str().unwrap().as_bytes())
        ),
        "TRIVIA" => format!(
            "Observation::Trivia{{lexeme:{}}}",
            rust_bytes(observation["lexeme"].as_str().unwrap().as_bytes())
        ),
        "ERROR_TOKEN" => format!(
            "Observation::ErrorToken{{transform:{:?},lexeme:{}}}",
            observation["transform"].as_str().unwrap(),
            rust_bytes(observation["lexeme"].as_str().unwrap().as_bytes())
        ),
        "PRATT_REDUCE" => format!(
            "Observation::PrattReduce{{fixity:{:?},matcher_rule:{:?},reduced:{}}}",
            observation["fixity"].as_str().unwrap(),
            observation["matcher_rule"].as_str().unwrap(),
            rust_bytes(observation["reduced"].as_str().unwrap().as_bytes())
        ),
        "END_INPUT" => "Observation::EndInput".to_owned(),
        other => panic!("validated observation has unknown kind {other}"),
    }
}

fn synthetic_observation_source(observation: SyntheticObservation) -> String {
    match observation {
        SyntheticObservation::Token { symbol, lexeme } => format!(
            "Observation::Token{{symbol:{symbol:?},lexeme:{}}}",
            rust_bytes(lexeme)
        ),
        SyntheticObservation::Trivia(lexeme) => {
            format!("Observation::Trivia{{lexeme:{}}}", rust_bytes(lexeme))
        }
        SyntheticObservation::PrattReduce {
            fixity,
            matcher_rule,
            reduced,
        } => format!(
            "Observation::PrattReduce{{fixity:{fixity:?},matcher_rule:{matcher_rule:?},reduced:{}}}",
            rust_bytes(reduced)
        ),
        SyntheticObservation::EndInput => "Observation::EndInput".to_owned(),
    }
}

fn rule_ordinal(compiled: &CompiledModule, name: &str) -> u32 {
    compiled
        .program()
        .rules()
        .iter()
        .find(|rule| rule.name() == name)
        .unwrap_or_else(|| panic!("compiled public witness omitted rule {name}"))
        .id()
        .index()
        .try_into()
        .expect("rule ordinal fits generated u32 identity")
}

fn optional_u16_source(value: &Value) -> String {
    if value.is_null() {
        "None".to_owned()
    } else {
        format!("Some({}u16)", value.as_u64().unwrap())
    }
}

fn pratt_metadata_source(instance: &Value, compiled: &CompiledModule) -> (String, String) {
    let mut tables = String::from("&[");
    let mut operators = String::from("&[");
    let mut operator_start = 0_u32;
    let mut operator_id = 0_u32;
    for (table_index, table) in instance["pratt_tables"]
        .as_array()
        .unwrap()
        .iter()
        .enumerate()
    {
        let rows = table["operators"].as_array().unwrap();
        let site_name = table["site_rule"].as_str().unwrap();
        let atom_name = table["atom_rule"].as_str().unwrap();
        writeln!(
            tables,
            "ExpectedPrattTable{{id:{table_index}u32,site_rule:{}u32,atom_rule:{}u32,operator_start:{operator_start}u32,operator_len:{}u32}},",
            rule_ordinal(compiled, site_name),
            rule_ordinal(compiled, atom_name),
            rows.len()
        )
        .unwrap();
        for operator in rows {
            let matcher_name = operator["matcher_rule"].as_str().unwrap();
            write!(
                operators,
                "ExpectedPrattOperator{{id:{operator_id}u32,table:{table_index}u32,matcher_rule:{}u32,matcher_name:{matcher_name:?},fixity:{:?},left_binding_power:{},right_binding_power:{}}},",
                rule_ordinal(compiled, matcher_name),
                operator["fixity"].as_str().unwrap(),
                optional_u16_source(&operator["left_binding_power"]),
                optional_u16_source(&operator["right_binding_power"])
            )
            .unwrap();
            operator_id = operator_id.checked_add(1).expect("operator count fits u32");
        }
        operator_start = operator_start
            .checked_add(u32::try_from(rows.len()).expect("table operator count"))
            .expect("operator count fits u32");
    }
    tables.push(']');
    operators.push(']');
    (tables, operators)
}

fn runtime_runner_source() -> String {
    let public_ordinals = discovered_public_ordinals();
    let public_case_rows = read_tsv(PUBLIC_CASES, &CASE_HEADER);
    let mut source = String::from(
        r#"use parse_that::kernel::{EventSink, Input, Run, Span, Step, literal};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExpectedOutcome { Match, Mismatch }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExpectedCursorAfter { Entry, EndInput }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Observation<'a> {
    Token { symbol: &'static str, lexeme: &'a [u8] },
    Trivia { lexeme: &'a [u8] },
    ErrorToken { transform: &'static str, lexeme: &'a [u8] },
    PrattReduce {
        fixity: &'static str,
        matcher_rule: &'static str,
        reduced: &'a [u8],
    },
    EndInput,
}

struct Case {
    ordinal: usize,
    id: &'static str,
    input: &'static [u8],
    outcome: ExpectedOutcome,
    cursor_after: ExpectedCursorAfter,
    observations: &'static [Observation<'static>],
}

#[derive(Clone, Copy, Debug)]
struct ExpectedPrattTable {
    id: u32,
    site_rule: u32,
    atom_rule: u32,
    operator_start: u32,
    operator_len: u32,
}

#[derive(Clone, Copy, Debug)]
struct ExpectedPrattOperator {
    id: u32,
    table: u32,
    matcher_rule: u32,
    matcher_name: &'static str,
    fixity: &'static str,
    left_binding_power: Option<u16>,
    right_binding_power: Option<u16>,
}

#[derive(Default)]
struct LawSink(Vec<usize>);

impl EventSink for LawSink {
    type Event = usize;

    fn len(&self) -> usize {
        self.0.len()
    }

    fn truncate(&mut self, len: usize) {
        self.0.truncate(len);
    }

    fn push(&mut self, event: usize) {
        self.0.push(event);
    }
}

fn outcome_and_diagnostic_offset(
    step: &Step<'_, Span<'_>>,
) -> (ExpectedOutcome, Option<usize>) {
    match step {
        Step::Matched(span) => {
            assert_eq!(span.start(), 0, "successful module span starts at entry");
            (ExpectedOutcome::Match, None)
        }
        // Failure::at remains diagnostic evidence only. It is deliberately not
        // treated as the module's transactional cursor-after state.
        Step::Mismatch(failure) => (ExpectedOutcome::Mismatch, Some(failure.at().offset())),
        Step::Fault(fault) => panic!("generated parser faulted: {fault:?}"),
    }
}

fn exact_parse_full_cursor_and_sink_law() {
    let input = Input::new(b"ab");
    let mut matched = Run::with_sink(input, LawSink::default());
    let matched_outcome = matched.parse_full(|run| match literal(run, b"ab") {
        Step::Matched(span) => {
            run.emit(span.end());
            Step::Matched(span)
        }
        Step::Mismatch(failure) => Step::Mismatch(failure),
        Step::Fault(fault) => Step::Fault(fault),
    });
    let Step::Matched(span) = matched_outcome else {
        panic!("parse_full exact match did not match");
    };
    assert_eq!((span.start(), span.end()), (0, input.len()));
    assert_eq!(matched.cursor().offset(), input.len(), "match cursor commits");
    assert_eq!(EventSink::len(matched.sink()), 1, "match sink commits");

    let mut mismatched = Run::with_sink(input, LawSink::default());
    let mismatch_outcome = mismatched.parse_full(|run| match literal(run, b"a") {
        Step::Matched(span) => {
            run.emit(span.end());
            literal(run, b"z")
        }
        Step::Mismatch(failure) => Step::Mismatch(failure),
        Step::Fault(fault) => Step::Fault(fault),
    });
    let Step::Mismatch(failure) = mismatch_outcome else {
        panic!("parse_full partial mismatch did not mismatch");
    };
    assert_eq!(
        failure.at().offset(),
        1,
        "Failure::at remains furthest diagnostic evidence"
    );
    assert_eq!(
        mismatched.cursor().offset(),
        0,
        "mismatch cursor observably rolls back to ENTRY"
    );
    assert_eq!(
        EventSink::len(mismatched.sink()),
        0,
        "mismatch sink observably rolls back"
    );
}

fn assert_case_cursor_oracle(case: &Case) {
    match (case.outcome, case.cursor_after) {
        (ExpectedOutcome::Match, ExpectedCursorAfter::EndInput)
        | (ExpectedOutcome::Mismatch, ExpectedCursorAfter::Entry) => {}
        other => panic!(
            "{}: cursor-after oracle contradicts the exact linked parse_full law: {other:?}",
            case.id
        ),
    }
}

macro_rules! define_runner {
    ($run:ident, $normalize:ident, $module:ident, $tables:expr, $operators:expr) => {
        fn $normalize(
            input: &'static [u8],
            events: &[$module::ScalarEvent],
        ) -> Vec<Observation<'static>> {
            let mut normalized = Vec::new();
            let mut pratt_node = None;
            let mut end_input_count = 0usize;
            let expected_operators: &[ExpectedPrattOperator] = $operators;
            for (event_index, event) in events.iter().enumerate() {
                let start = event.start() as usize;
                let end = event.end() as usize;
                assert!(start <= end && end <= input.len(), "event span is in source");
                if !matches!(event.kind(), $module::EventKind::PrattReduce) {
                    assert_eq!(
                        event.operator(),
                        None,
                        "operator identity is exclusive to PrattReduce"
                    );
                }
                match event.kind() {
                    $module::EventKind::EnterRule
                    | $module::EventKind::LeaveRule
                    | $module::EventKind::EnterField
                    | $module::EventKind::LeaveField
                    | $module::EventKind::ChoiceBranch
                    | $module::EventKind::RepeatCount => {}
                    $module::EventKind::Literal => {
                        if start != end {
                            normalized.push(Observation::Token {
                                symbol: "literal",
                                lexeme: &input[start..end],
                            });
                        }
                    }
                    $module::EventKind::Regex => {
                        if start != end {
                            normalized.push(Observation::Token {
                                symbol: "regex",
                                lexeme: &input[start..end],
                            });
                        }
                    }
                    $module::EventKind::Trivia => {
                        assert!(start < end, "zero-width Trivia is forbidden");
                        assert!(event.node().is_some(), "Trivia carries Trim node identity");
                        assert!(
                            matches!(event.value(), 0 | 1),
                            "Trivia value is the closed leading/trailing side identity"
                        );
                        normalized.push(Observation::Trivia {
                            lexeme: &input[start..end],
                        });
                    }
                    $module::EventKind::Transform => {
                        let transform = event.transform().expect("Transform event identity");
                        let descriptor = $module::TRANSFORMS
                            .iter()
                            .find(|row| row.id() == transform)
                            .expect("generated transform descriptor");
                        if descriptor.name() == "error" {
                            normalized.push(Observation::ErrorToken {
                                transform: descriptor.name(),
                                lexeme: &input[start..end],
                            });
                        }
                    }
                    $module::EventKind::PrattReduce => {
                        assert_eq!(event.value(), 0, "PrattReduce value is reserved");
                        let node = event.node().expect("PrattReduce node identity");
                        if let Some(previous) = pratt_node {
                            assert_eq!(node, previous, "PrattReduce node identity consistency");
                        } else {
                            pratt_node = Some(node);
                        }
                        let operator = event.operator().expect("PrattReduce operator");
                        let descriptor = $module::PRATT_OPERATORS
                            .iter()
                            .find(|row| row.id() == operator)
                            .expect("PrattReduce generated operator descriptor");
                        let expected = expected_operators
                            .iter()
                            .find(|row| row.matcher_rule == descriptor.matcher_rule())
                            .expect("descriptor matcher identity maps to compiled rule");
                        let fixity = match descriptor.fixity() {
                            $module::PrattFixity::Prefix => "prefix",
                            $module::PrattFixity::Postfix => "postfix",
                            $module::PrattFixity::Infix => "infix",
                        };
                        normalized.push(Observation::PrattReduce {
                            fixity,
                            matcher_rule: expected.matcher_name,
                            reduced: &input[start..end],
                        });
                    }
                    $module::EventKind::EndInput => {
                        end_input_count += 1;
                        assert_eq!(event_index + 1, events.len(), "EndInput is terminal");
                        assert_eq!((start, end), (input.len(), input.len()), "EndInput span");
                        assert_eq!(event.rule(), None, "EndInput rule identity");
                        assert_eq!(event.node(), None, "EndInput node identity");
                        assert_eq!(event.field(), None, "EndInput field identity");
                        assert_eq!(event.transform(), None, "EndInput transform identity");
                        assert_eq!(event.operator(), None, "EndInput operator identity");
                        assert_eq!(event.value(), 0, "EndInput value");
                        normalized.push(Observation::EndInput);
                    }
                }
            }
            assert!(end_input_count <= 1, "at most one EndInput event");
            normalized
        }

        fn $run(case: &Case) {
            assert_eq!(
                $module::RECOVERY_POLICY,
                $module::RecoveryPolicy::Disabled,
                "{}: generated recovery policy",
                case.id
            );
            let expected_tables: &[ExpectedPrattTable] = $tables;
            let expected_operators: &[ExpectedPrattOperator] = $operators;
            assert_eq!(
                $module::PRATT_TABLES.len(),
                expected_tables.len(),
                "{}: Pratt table descriptor count",
                case.id
            );
            assert_eq!(
                $module::PRATT_OPERATORS.len(),
                expected_operators.len(),
                "{}: Pratt operator descriptor count",
                case.id
            );
            for (actual, expected) in $module::PRATT_TABLES.iter().zip(expected_tables) {
                assert_eq!(actual.id(), expected.id, "{}: table id", case.id);
                assert_eq!(
                    actual.site_rule(),
                    expected.site_rule,
                    "{}: table site backreference",
                    case.id
                );
                assert_eq!(
                    actual.atom_rule(),
                    expected.atom_rule,
                    "{}: table atom backreference",
                    case.id
                );
                assert_eq!(
                    actual.operator_start(),
                    expected.operator_start,
                    "{}: table operator range start",
                    case.id
                );
                assert_eq!(
                    actual.operator_len(),
                    expected.operator_len,
                    "{}: table operator range length",
                    case.id
                );
                let start = actual.operator_start() as usize;
                let end = start + actual.operator_len() as usize;
                assert!(
                    end <= $module::PRATT_OPERATORS.len()
                        && $module::PRATT_OPERATORS[start..end]
                            .iter()
                            .all(|operator| operator.table() == actual.id()),
                    "{}: table range/operator backreference closure",
                    case.id
                );
            }
            for (actual, expected) in $module::PRATT_OPERATORS.iter().zip(expected_operators) {
                assert_eq!(actual.id(), expected.id, "{}: operator id", case.id);
                assert_eq!(
                    actual.table(),
                    expected.table,
                    "{}: operator table backreference",
                    case.id
                );
                assert_eq!(
                    actual.matcher_rule(),
                    expected.matcher_rule,
                    "{}: operator matcher identity",
                    case.id
                );
                let actual_fixity = match actual.fixity() {
                    $module::PrattFixity::Prefix => "prefix",
                    $module::PrattFixity::Postfix => "postfix",
                    $module::PrattFixity::Infix => "infix",
                };
                assert_eq!(
                    actual_fixity, expected.fixity,
                    "{}: operator fixity",
                    case.id
                );
                assert_eq!(
                    actual.left_binding_power(),
                    expected.left_binding_power,
                    "{}: operator left binding power",
                    case.id
                );
                assert_eq!(
                    actual.right_binding_power(),
                    expected.right_binding_power,
                    "{}: operator right binding power",
                    case.id
                );
            }
            let retained = $module::parse_events(case.input);
            let recognition_only = $module::check(case.input);
            let retained_outcome_evidence =
                outcome_and_diagnostic_offset(retained.outcome());
            let recognition_outcome_evidence =
                outcome_and_diagnostic_offset(recognition_only.outcome());
            assert_eq!(
                retained_outcome_evidence, recognition_outcome_evidence,
                "{}: retaining/NoSink outcome and diagnostic-evidence parity",
                case.id
            );
            assert_eq!(
                retained_outcome_evidence.0, case.outcome,
                "{}: outcome",
                case.id
            );
            assert_case_cursor_oracle(case);
            assert_eq!(
                EventSink::len(retained.sink()),
                EventSink::len(recognition_only.sink()),
                "{}: retaining/NoSink logical-event parity",
                case.id
            );
            let actual = $normalize(case.input, retained.sink().events());
            assert_eq!(actual, case.observations, "{}: normalized events", case.id);
            if case.outcome == ExpectedOutcome::Mismatch {
                assert_eq!(EventSink::len(retained.sink()), 0, "{}: rollback", case.id);
            }
        }
    };
}

"#,
    );

    for ordinal in &public_ordinals {
        writeln!(
            source,
            "#[path = \"generated/g_{ordinal}.rs\"] mod g_{ordinal};"
        )
        .unwrap();
    }
    for control in SYNTHETIC_CONTROLS {
        writeln!(
            source,
            "#[path = \"generated/{}.rs\"] mod {};",
            control.module, control.module
        )
        .unwrap();
    }
    source.push('\n');
    for ordinal in &public_ordinals {
        let instance = json_file(&format!(
            "contracts/w3-public-witnesses/public/{ordinal}/instance.json"
        ));
        let compiled = compile_public(ordinal);
        let (tables, operators) = pratt_metadata_source(&instance, &compiled);
        writeln!(
            source,
            "define_runner!(run_{ordinal}, normalize_{ordinal}, g_{ordinal}, {tables}, {operators});"
        )
        .unwrap();
    }
    for control in SYNTHETIC_CONTROLS {
        let manifest: Value = serde_json::from_slice(&synthetic_manifest(control))
            .expect("synthetic genericity manifest JSON");
        let compiled = compile_synthetic(control);
        let (tables, operators) = pratt_metadata_source(&manifest, &compiled);
        writeln!(
            source,
            "define_runner!(run_{}, normalize_{}, {}, {tables}, {operators});",
            control.module, control.module, control.module
        )
        .unwrap();
    }

    source.push_str("\nconst CASES: &[Case] = &[\n");
    for row in &public_case_rows {
        let ordinal = Path::new(&row[3])
            .components()
            .map(|component| component.as_os_str().to_string_lossy())
            .collect::<Vec<_>>()
            .windows(2)
            .find_map(|window| (window[0] == "public").then(|| window[1].to_string()))
            .expect("public case ordinal");
        let ordinal_index = public_ordinals
            .iter()
            .position(|candidate| *candidate == ordinal)
            .expect("sealed public ordinal");
        let expectation = json_file(&row[5]);
        let outcome = match expectation["outcome"].as_str().unwrap() {
            "MATCH" => "ExpectedOutcome::Match",
            "MISMATCH" => "ExpectedOutcome::Mismatch",
            other => panic!("validated outcome {other}"),
        };
        let cursor_after = match expectation["cursor_after"].as_str().unwrap() {
            "ENTRY" => "ExpectedCursorAfter::Entry",
            "END_INPUT" => "ExpectedCursorAfter::EndInput",
            other => panic!("validated cursor_after {other}"),
        };
        let observations = expectation["observations"]
            .as_array()
            .unwrap()
            .iter()
            .map(expectation_observation_source)
            .collect::<Vec<_>>()
            .join(",");
        writeln!(
            source,
            "Case{{ordinal:{ordinal_index},id:{:?},input:{},outcome:{outcome},cursor_after:{cursor_after},observations:&[{observations}]}},",
            row[0],
            rust_bytes(&decode_input(&row[3]))
        )
        .unwrap();
    }
    for (control_index, control) in SYNTHETIC_CONTROLS.iter().copied().enumerate() {
        for case in control.cases {
            let outcome = match case.outcome {
                SyntheticOutcome::Match => "ExpectedOutcome::Match",
                SyntheticOutcome::Mismatch => "ExpectedOutcome::Mismatch",
            };
            let cursor_after = match case.cursor_after {
                SyntheticCursorAfter::Entry => "ExpectedCursorAfter::Entry",
                SyntheticCursorAfter::EndInput => "ExpectedCursorAfter::EndInput",
            };
            let observations = case
                .observations
                .iter()
                .copied()
                .map(synthetic_observation_source)
                .collect::<Vec<_>>()
                .join(",");
            writeln!(
                source,
                "Case{{ordinal:{},id:{:?},input:{},outcome:{outcome},cursor_after:{cursor_after},observations:&[{observations}]}},",
                public_ordinals.len() + control_index,
                format!("{}::{}", control.id, case.id),
                rust_bytes(case.input)
            )
            .unwrap();
        }
    }
    source.push_str("];\n\n");
    writeln!(
        source,
        "const RUNNERS: [fn(&Case); {}] = [",
        public_ordinals.len() + SYNTHETIC_CONTROLS.len()
    )
    .unwrap();
    for ordinal in &public_ordinals {
        writeln!(source, "run_{ordinal},").unwrap();
    }
    for control in SYNTHETIC_CONTROLS {
        writeln!(source, "run_{},", control.module).unwrap();
    }
    source.push_str("];\n\n");
    let case_count = public_case_rows.len()
        + SYNTHETIC_CONTROLS
            .iter()
            .map(|row| row.cases.len())
            .sum::<usize>();
    writeln!(
        source,
        r#"fn main() {{
    assert_eq!(parse_that::kernel::KERNEL_ABI, 4);
    exact_parse_full_cursor_and_sink_law();
    assert_eq!(CASES.len(), {case_count});
    for case in CASES {{
        RUNNERS[case.ordinal](case);
    }}
}}"#
    )
    .unwrap();
    source
}

fn external_runtime_manifest() -> String {
    "[package]\nname = \"bbnf-w3-public-runtime\"\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[dependencies]\nparse_that = { version = \"=0.4.0\", default-features = false }\n".to_owned()
}

fn local_registry_config(registry: &Path) -> String {
    format!(
        "[source.crates-io]\nreplace-with = \"w3-local\"\n\n[source.w3-local]\nlocal-registry = {:?}\n\n[net]\noffline = true\n",
        registry
    )
}

fn generate_external_runtime(output_root: &Path, registry: &Path) -> BTreeMap<String, Vec<u8>> {
    assert!(output_root.is_dir(), "output root must already exist");
    assert!(
        fs::read_dir(output_root)
            .expect("read initially empty output root")
            .next()
            .is_none(),
        "{}: generation root was not initially empty",
        output_root.display()
    );

    let mut products = BTreeMap::new();
    products.insert(
        "Cargo.toml".to_owned(),
        external_runtime_manifest().into_bytes(),
    );
    products.insert(
        ".cargo/config.toml".to_owned(),
        local_registry_config(registry).into_bytes(),
    );
    products.insert(
        "src/main.rs".to_owned(),
        runtime_runner_source().into_bytes(),
    );
    let public_ordinals = discovered_public_ordinals();
    for ordinal in &public_ordinals {
        let compiled = compile_public(ordinal);
        validate_generated_manifest(&format!("public/{ordinal} external manifest"), &compiled);
        assert_generated_parse_module_boundary(
            &format!("public/{ordinal}"),
            compiled.generated().source(),
        );
        products.insert(
            format!("src/generated/g_{ordinal}.rs"),
            compiled.generated().source().as_bytes().to_vec(),
        );
    }
    for control in SYNTHETIC_CONTROLS {
        let compiled = compile_synthetic(control);
        validate_generated_manifest(&format!("{} external manifest", control.id), &compiled);
        assert_generated_parse_module_boundary(control.id, compiled.generated().source());
        products.insert(
            format!("src/generated/{}.rs", control.module),
            compiled.generated().source().as_bytes().to_vec(),
        );
    }
    let mut expected_products = BTreeSet::from([
        ".cargo/config.toml".to_owned(),
        "Cargo.toml".to_owned(),
        "src/main.rs".to_owned(),
    ]);
    expected_products.extend(
        public_ordinals
            .iter()
            .map(|ordinal| format!("src/generated/g_{ordinal}.rs")),
    );
    expected_products.extend(
        SYNTHETIC_CONTROLS
            .iter()
            .map(|control| format!("src/generated/{}.rs", control.module)),
    );
    assert_eq!(
        products.keys().cloned().collect::<BTreeSet<_>>(),
        expected_products,
        "one generic harness plus discovered public and synthetic modules"
    );
    for (relative, contents) in &products {
        let path = output_root.join(relative);
        fs::create_dir_all(path.parent().expect("generated product parent"))
            .expect("create generated product parent");
        fs::write(&path, contents)
            .unwrap_or_else(|error| panic!("{}: write product: {error}", path.display()));
    }
    assert!(
        !output_root.join("selected").exists()
            && !output_root.join("selected-instance").exists()
            && !output_root.join("materialized").exists(),
        "pre-freeze generic output must not create a selected/materialized directory"
    );
    products
}

fn assert_generic_external_runtime_is_dual_root_identical_and_executes_all_public_cases(
    registry: &Path,
    home: &Path,
    target: &Path,
) {
    let temporary = tempfile::tempdir().expect("external public-witness consumer");
    let first_root = temporary.path().join("first-empty-root");
    let second_root = temporary.path().join("second-empty-root");
    fs::create_dir(&first_root).expect("first initially empty root");
    fs::create_dir(&second_root).expect("second initially empty root");

    let first_products = generate_external_runtime(&first_root, registry);
    let second_products = generate_external_runtime(&second_root, registry);
    assert_eq!(
        first_products, second_products,
        "independent generation into two initially empty roots is byte-identical"
    );
    let harness = first_products.get("src/main.rs").expect("generic harness");
    let harness_text = std::str::from_utf8(harness).expect("generic harness is UTF-8");
    assert_eq!(
        harness_text.matches("fn main()").count(),
        1,
        "exactly one generic external harness"
    );

    let cargo = |arguments: &[&str], label: &str| {
        command_output(
            Command::new(env!("CARGO"))
                .args(arguments)
                .env("CARGO_HOME", home)
                .env("CARGO_NET_OFFLINE", "true")
                .env("CARGO_PROFILE_DEV_DEBUG", "0")
                .env("CARGO_TARGET_DIR", target)
                .env_remove("RUSTC_WRAPPER")
                .current_dir(&first_root),
            label,
        )
    };
    cargo(
        &["generate-lockfile", "--offline"],
        "external runtime lock generation",
    );
    let metadata = cargo(
        &["metadata", "--locked", "--offline", "--format-version", "1"],
        "external runtime metadata",
    );
    let metadata: Value =
        serde_json::from_slice(&metadata.stdout).expect("external runtime metadata JSON");
    let runtime = metadata["packages"]
        .as_array()
        .expect("external runtime metadata packages")
        .iter()
        .find(|package| {
            package["name"].as_str() == Some("parse_that")
                && package["version"].as_str() == Some("0.4.0")
        })
        .expect("external runtime exact parse_that");
    assert!(
        runtime["source"].as_str() == Some("registry+https://github.com/rust-lang/crates.io-index"),
        "source replacement preserves the crates.io logical identity while the isolated offline \
         Cargo home and temporary local-registry configuration prove the physical package source"
    );
    cargo(
        &["run", "--locked", "--quiet", "--offline"],
        "external public-witness consumer",
    );
}

#[derive(Clone)]
struct PackagedCrate {
    name: String,
    version: String,
    archive: PathBuf,
    sha256: String,
    files: BTreeSet<String>,
}

fn command_output(command: &mut Command, label: &str) -> std::process::Output {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("{label}: spawn: {error}"));
    assert!(
        output.status.success(),
        "{label}: failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn package_archive_entries(archive: &Path) -> BTreeSet<String> {
    let output = command_output(
        Command::new("tar").args(["-tzf"]).arg(archive),
        &format!("list {}", archive.display()),
    );
    let text = std::str::from_utf8(&output.stdout).expect("package file list is UTF-8");
    let entries: BTreeSet<String> = text.lines().map(str::to_owned).collect();
    assert_eq!(
        entries.len(),
        text.lines().count(),
        "{}: archive has no duplicate paths",
        archive.display()
    );
    assert!(
        entries.iter().all(|entry| {
            !entry.starts_with('/')
                && !entry.split('/').any(|component| component == "..")
                && !entry.contains('\\')
        }),
        "{}: archive paths remain normalized and relative",
        archive.display()
    );
    entries
}

fn registry_index_relative(name: &str) -> PathBuf {
    let name = name.to_ascii_lowercase();
    match name.len() {
        1 => PathBuf::from("1").join(name),
        2 => PathBuf::from("2").join(name),
        3 => PathBuf::from("3").join(&name[0..1]).join(name),
        _ => PathBuf::from(&name[0..2]).join(&name[2..4]).join(name),
    }
}

fn cargo_home() -> PathBuf {
    std::env::var_os("CARGO_HOME").map_or_else(
        || {
            PathBuf::from(
                std::env::var_os("HOME").expect("HOME is available for Cargo registry cache"),
            )
            .join(".cargo")
        },
        PathBuf::from,
    )
}

fn local_registry_record(package: &Value, checksum: &str) -> String {
    let mut dependencies = Vec::new();
    for dependency in package["dependencies"]
        .as_array()
        .expect("metadata package dependencies")
    {
        // `cargo package` drops a path dependency that names no version (a
        // path-only dev-dependency); the record follows the packaged manifest.
        if dependency["path"].is_string() && dependency["req"].as_str() == Some("*") {
            continue;
        }
        let actual_name = dependency["name"].as_str().expect("dependency name");
        let rename = dependency["rename"].as_str();
        dependencies.push(json!({
            "name": rename.unwrap_or(actual_name),
            "req": dependency["req"],
            "features": dependency["features"],
            "optional": dependency["optional"],
            "default_features": dependency["uses_default_features"],
            "target": dependency["target"],
            "kind": dependency["kind"].as_str().unwrap_or("normal"),
            "registry": dependency["registry"]
                .as_str()
                .unwrap_or("https://github.com/rust-lang/crates.io-index"),
            "package": rename.map(|_| actual_name),
        }));
    }

    let mut features = Map::new();
    let mut features2 = Map::new();
    for (name, members) in package["features"].as_object().expect("package features") {
        let modern = members
            .as_array()
            .expect("feature members")
            .iter()
            .filter_map(Value::as_str)
            .any(|member| member.starts_with("dep:") || member.contains("?/"));
        if modern {
            features2.insert(name.clone(), members.clone());
        } else {
            features.insert(name.clone(), members.clone());
        }
    }

    let mut record = Map::from_iter([
        ("name".to_owned(), package["name"].clone()),
        ("vers".to_owned(), package["version"].clone()),
        ("deps".to_owned(), Value::Array(dependencies)),
        ("cksum".to_owned(), Value::String(checksum.to_owned())),
        ("features".to_owned(), Value::Object(features)),
        ("yanked".to_owned(), Value::Bool(false)),
        ("links".to_owned(), package["links"].clone()),
    ]);
    if !features2.is_empty() {
        record.insert("features2".to_owned(), Value::Object(features2));
        record.insert("v".to_owned(), json!(2));
    }
    if !package["rust_version"].is_null() {
        record.insert("rust_version".to_owned(), package["rust_version"].clone());
    }
    serde_json::to_string(&Value::Object(record)).expect("local registry record")
}

/// sk-v27 LW3.COMP-1b-T (RULINGS 2026-11-12d COMP1B-W3 (a)): the sealed record
/// of the upstream packages at the pinned commits, in place of sk-v26's
/// `W3-PARSE-THAT-ABI3.json`. A re-pin re-records it (`BBNF_W3_ABI4_RECEIPT_OUT`)
/// and edits this hash in the same commit.
const PARSE_THAT_ABI4_RECEIPT: &str =
    "restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp1b-t/LW3-PARSE-THAT-ABI4.json";
const PARSE_THAT_ABI4_RECEIPT_SHA256: &str =
    "073a80705f1c30b79f3c5ce41114e5db439de0526d92da6d6c3f490a1d1ce1a6";
const PARSE_THAT_ABI4_RECEIPT_SCHEMA: &str = "bbnf.sk-v27.parse-that-abi4-receipt.v1";

/// The upstream packages in publish (dependency) order, and the repository
/// each is packaged from.
const SK_V27_UPSTREAM_PACKAGES: [(&str, &str); 7] = [
    ("pprint_derive", "pprint"),
    ("pprint", "pprint"),
    ("parse-that-egraph-derive", "parse-that"),
    ("parse-that-egraph", "parse-that"),
    ("parse-that-simd", "parse-that"),
    ("parse-that-regex", "parse-that"),
    ("parse_that", "parse-that"),
];
/// This repository's packages, in publish order (`bbnf-path` is a versioned
/// dev-dependency of `bbnf`, so a registry verification of `bbnf` needs it).
const SK_V27_OWNED_PACKAGES: [&str; 4] = ["bbnf-ser", "bbnf-ir", "bbnf-path", "bbnf"];

const CRATES_IO_INDEX: &str = "registry+https://github.com/rust-lang/crates.io-index";

/// One crates.io package a lockfile names: its exact registry record from the
/// sparse-index cache and, where Cargo has downloaded it, its archive.
struct ThirdPartyPackage {
    name: String,
    version: String,
    record: String,
    archive: Option<PathBuf>,
}

/// Every crates.io package the three locks of the lock-step name (this
/// workspace's, and the pinned parse-that and pprint workspaces'), read from
/// Cargo's sparse-index cache. The record's checksum must equal the lock's,
/// and a cached archive must hash to it.
fn lockstep_third_party_packages(locks: &[PathBuf]) -> Vec<ThirdPartyPackage> {
    let cargo_home = cargo_home();
    let mut wanted: BTreeMap<(String, String), String> = BTreeMap::new();
    for lock in locks {
        let text = fs::read_to_string(lock)
            .unwrap_or_else(|error| panic!("{}: lockfile: {error}", lock.display()));
        let parsed: toml::Value = toml::from_str(&text).expect("lockfile TOML");
        for package in parsed["package"].as_array().expect("lock packages") {
            if package.get("source").and_then(toml::Value::as_str) != Some(CRATES_IO_INDEX) {
                continue;
            }
            let name = package["name"].as_str().expect("locked name").to_owned();
            let version = package["version"]
                .as_str()
                .expect("locked version")
                .to_owned();
            let checksum = package["checksum"]
                .as_str()
                .expect("a crates.io lock entry carries its checksum")
                .to_owned();
            if let Some(previous) = wanted.insert((name.clone(), version.clone()), checksum.clone())
            {
                assert_eq!(
                    previous, checksum,
                    "{name} {version}: the three locks agree on its checksum"
                );
            }
        }
    }
    let mut index_roots: Vec<PathBuf> = fs::read_dir(cargo_home.join("registry/index"))
        .expect("Cargo registry index cache")
        .map(|entry| entry.expect("index cache entry").path())
        .collect();
    index_roots.sort();
    let mut packages = Vec::new();
    for ((name, version), checksum) in wanted {
        let mut found = None;
        for index_root in &index_roots {
            let sparse = index_root
                .join(".cache")
                .join(registry_index_relative(&name));
            let Ok(sparse_bytes) = fs::read(&sparse) else {
                continue;
            };
            for field in sparse_bytes.split(|byte| *byte == 0) {
                if !field.starts_with(b"{") {
                    continue;
                }
                let Ok(record) = serde_json::from_slice::<Value>(field) else {
                    continue;
                };
                if record["name"].as_str() == Some(name.as_str())
                    && record["vers"].as_str() == Some(version.as_str())
                {
                    found = Some((
                        index_root.clone(),
                        record,
                        std::str::from_utf8(field)
                            .expect("registry record is UTF-8")
                            .to_owned(),
                    ));
                }
            }
            if found.is_some() {
                break;
            }
        }
        let (index_root, record, text) = found.unwrap_or_else(|| {
            panic!("{name} {version}: exact registry record absent from the sparse-index cache")
        });
        assert_eq!(
            record["cksum"].as_str(),
            Some(checksum.as_str()),
            "{name} {version}: the registry record matches the lock's checksum"
        );
        let archive = cargo_home
            .join("registry/cache")
            .join(index_root.file_name().expect("registry identity"))
            .join(format!("{name}-{version}.crate"));
        let archive = archive.is_file().then(|| {
            assert_eq!(
                sha256_hex(&fs::read(&archive).expect("cached archive")),
                checksum,
                "{name} {version}: cached archive matches its registry checksum"
            );
            archive
        });
        packages.push(ThirdPartyPackage {
            name,
            version,
            record: text,
            archive,
        });
    }
    packages
}

/// A local registry holding the third-party records (with every cached
/// archive) and the fresh packages, written whole each time it grows.
fn write_lockstep_registry(
    registry: &Path,
    third_party: &[ThirdPartyPackage],
    metadata: &Value,
    packaged: &[PackagedCrate],
) {
    if registry.exists() {
        fs::remove_dir_all(registry).expect("reset local registry");
    }
    let index = registry.join("index");
    fs::create_dir_all(&index).expect("local registry index");
    fs::write(
        index.join("config.json"),
        b"{\"dl\":\"https://invalid.example.invalid\",\"api\":\"https://invalid.example.invalid\"}\n",
    )
    .expect("local registry config");
    let mut records: BTreeMap<PathBuf, Vec<(String, String)>> = BTreeMap::new();
    for package in third_party {
        if let Some(archive) = &package.archive {
            fs::copy(
                archive,
                registry.join(format!("{}-{}.crate", package.name, package.version)),
            )
            .unwrap_or_else(|error| {
                panic!(
                    "{} {}: copy archive: {error}",
                    package.name, package.version
                )
            });
        }
        records
            .entry(registry_index_relative(&package.name))
            .or_default()
            .push((package.version.clone(), package.record.clone()));
    }
    for fresh in packaged {
        let entry = metadata["packages"]
            .as_array()
            .expect("metadata packages")
            .iter()
            .find(|package| {
                package["name"].as_str() == Some(fresh.name.as_str())
                    && package["version"].as_str() == Some(fresh.version.as_str())
            })
            .unwrap_or_else(|| panic!("{} {}: metadata package", fresh.name, fresh.version));
        fs::copy(
            &fresh.archive,
            registry.join(format!("{}-{}.crate", fresh.name, fresh.version)),
        )
        .unwrap_or_else(|error| panic!("{}: copy fresh archive: {error}", fresh.name));
        records
            .entry(registry_index_relative(&fresh.name))
            .or_default()
            .push((
                fresh.version.clone(),
                local_registry_record(entry, &fresh.sha256),
            ));
    }
    for (relative, mut versions) in records {
        versions.sort_by(|left, right| left.0.cmp(&right.0));
        versions.dedup_by(|left, right| left.0 == right.0);
        let path = index.join(relative);
        fs::create_dir_all(path.parent().expect("registry index parent"))
            .expect("registry index parent");
        let mut contents = versions
            .into_iter()
            .map(|(_, record)| record)
            .collect::<Vec<_>>()
            .join("\n");
        contents.push('\n');
        fs::write(&path, contents)
            .unwrap_or_else(|error| panic!("{}: write registry index: {error}", path.display()));
    }
}

/// A Cargo home for the crossing that reads nothing from the user's but the
/// git databases the workspace lock names (parse-that, pprint and the
/// third-party git sources), linked read-only so the pinned workspaces load
/// offline; every crates.io read goes to the local registry.
fn isolated_lockstep_cargo_home(scratch: &Path) -> PathBuf {
    let home = scratch.join("cargo-home");
    let database = home.join("git/db");
    fs::create_dir_all(&database).expect("isolated git database root");
    let lock = fs::read_to_string(root().join("Cargo.lock")).expect("workspace lock");
    let repositories: BTreeSet<String> = lock
        .lines()
        .filter_map(|line| line.strip_prefix("source = \"git+"))
        .map(|url| {
            url.split(['?', '#', '"'])
                .next()
                .expect("git URL")
                .trim_end_matches(".git")
                .rsplit('/')
                .next()
                .expect("repository name")
                .to_owned()
        })
        .collect();
    assert!(
        repositories.contains("parse-that") && repositories.contains("pprint"),
        "the workspace lock names both sibling repositories: {repositories:?}"
    );
    let source = cargo_home().join("git/db");
    let mut linked = BTreeSet::new();
    for entry in fs::read_dir(&source).expect("Cargo git database cache") {
        let entry = entry.expect("git database entry");
        let name = entry.file_name().into_string().expect("git database name");
        let repository = name.rsplit_once('-').map(|(stem, _)| stem).unwrap_or(&name);
        if !repositories.contains(repository) {
            continue;
        }
        #[cfg(unix)]
        std::os::unix::fs::symlink(entry.path(), database.join(&name))
            .unwrap_or_else(|error| panic!("link git database {name}: {error}"));
        linked.insert(repository.to_owned());
    }
    assert_eq!(
        linked, repositories,
        "every git repository the lock names has a local git database"
    );
    home
}

fn lockstep_metadata(home: &Path, registry: &Path) -> Value {
    let output = command_output(
        Command::new(env!("CARGO"))
            .args(["metadata", "--locked", "--offline", "--format-version", "1"])
            .args(lockstep_registry_flags(registry))
            .env("CARGO_HOME", home)
            .env("CARGO_NET_OFFLINE", "true")
            .env_remove("RUSTC_WRAPPER")
            .current_dir(root()),
        "W3 package graph metadata",
    );
    serde_json::from_slice(&output.stdout).expect("W3 package graph metadata JSON")
}

fn lockstep_registry_flags(registry: &Path) -> Vec<String> {
    vec![
        "--config".to_owned(),
        "source.crates-io.replace-with=\"w3-local\"".to_owned(),
        "--config".to_owned(),
        format!("source.w3-local.local-registry={:?}", registry),
        "--config".to_owned(),
        "net.offline=true".to_owned(),
    ]
}

/// `cargo package` twice into fresh target directories inside the isolated
/// Cargo home. Cargo's own verification build is replaced by two stronger
/// reads the crossing makes of every archive: the unpacked package resolves,
/// dev-dependencies included, from the local registry as it stands
/// ([`assert_package_resolves_from_registry`]), and the registry-only consumer
/// compiles every packaged library from its archive.
fn package_twice_in(
    home: &Path,
    scratch: &Path,
    manifest: &Path,
    name: &str,
    version: &str,
    registry: &Path,
) -> PackagedCrate {
    let manifest_value: toml::Value =
        toml::from_str(&fs::read_to_string(manifest).expect("package manifest"))
            .expect("package manifest TOML");
    assert_eq!(manifest_value["package"]["name"].as_str(), Some(name));
    assert_eq!(
        manifest_value["package"]["version"].as_str(),
        Some(version),
        "{name}: the manifest declares the locked version"
    );
    let mut archives = Vec::new();
    let mut file_lists = Vec::new();
    for pass in 1..=2 {
        let target = scratch.join(format!("package-pass-{pass}"));
        command_output(
            Command::new(env!("CARGO"))
                .args(["package", "--manifest-path"])
                .arg(manifest)
                .args([
                    "--locked",
                    "--offline",
                    "--allow-dirty",
                    "--no-verify",
                    "--target-dir",
                ])
                .arg(&target)
                .args(lockstep_registry_flags(registry))
                .env("CARGO_HOME", home)
                .env("CARGO_NET_OFFLINE", "true")
                .env_remove("RUSTC_WRAPPER")
                .current_dir(scratch),
            &format!("cargo package {name} pass {pass}"),
        );
        let archive = target
            .join("package")
            .join(format!("{name}-{version}.crate"));
        let archive_bytes = fs::read(&archive)
            .unwrap_or_else(|error| panic!("{}: packaged archive: {error}", archive.display()));
        file_lists.push(package_archive_entries(&archive));
        archives.push((archive, archive_bytes));
    }
    assert_eq!(
        archives[0].1, archives[1].1,
        "{name}: successive package archives are byte-identical"
    );
    assert_eq!(
        file_lists[0], file_lists[1],
        "{name}: successive package file inventories are identical"
    );
    let archive_root = format!("{name}-{version}/");
    assert!(
        file_lists[0]
            .iter()
            .all(|entry| entry.starts_with(&archive_root)),
        "{name}: every packaged path is confined to its exact archive root"
    );
    let library = manifest_value
        .get("lib")
        .and_then(|lib| lib.get("path"))
        .and_then(toml::Value::as_str)
        .map_or("src/lib.rs", |path| path.trim_start_matches("./"));
    for required in ["Cargo.toml", "Cargo.toml.orig", library] {
        assert!(
            file_lists[0].contains(&format!("{archive_root}{required}")),
            "{name}: package includes {required}"
        );
    }
    let kept = scratch.join(format!("{name}-{version}.crate"));
    fs::copy(&archives[0].0, &kept).expect("keep the first archive");
    PackagedCrate {
        name: name.to_owned(),
        version: version.to_owned(),
        archive: kept,
        sha256: sha256_hex(&archives[0].1),
        files: file_lists.remove(0),
    }
}

/// The unpacked archive is a package of its own: its normalized manifest,
/// dev-dependencies included, resolves offline from the local registry.
fn assert_package_resolves_from_registry(
    home: &Path,
    scratch: &Path,
    package: &PackagedCrate,
    registry: &Path,
) {
    let unpacked = scratch.join(format!("resolve-{}", package.name));
    fs::create_dir_all(&unpacked).expect("resolve directory");
    command_output(
        Command::new("tar")
            .arg("-xzf")
            .arg(&package.archive)
            .arg("-C")
            .arg(&unpacked),
        &format!("unpack {} for resolution", package.name),
    );
    let package_root = unpacked.join(format!("{}-{}", package.name, package.version));
    fs::remove_file(package_root.join("Cargo.lock")).ok();
    command_output(
        Command::new(env!("CARGO"))
            .args(["generate-lockfile", "--offline"])
            .args(lockstep_registry_flags(registry))
            .env("CARGO_HOME", home)
            .env("CARGO_NET_OFFLINE", "true")
            .env_remove("RUSTC_WRAPPER")
            .current_dir(&package_root),
        &format!(
            "{}: the packaged manifest resolves from the registry",
            package.name
        ),
    );
}

/// Identity by tree: every packaged file other than the three Cargo writes
/// (the normalized manifest, the lockfile, the VCS record) equals the blob at
/// the pinned commit, `Cargo.toml.orig` equals the committed manifest, and the
/// VCS record names that commit and path.
fn assert_archive_is_the_pinned_tree(
    package: &PackagedCrate,
    checkout: &Path,
    rev: &str,
    path_in_vcs: &str,
    scratch: &Path,
) {
    let unpacked = scratch.join(format!("unpacked-{}", package.name));
    fs::create_dir_all(&unpacked).expect("unpack directory");
    command_output(
        Command::new("tar")
            .arg("-xzf")
            .arg(&package.archive)
            .arg("-C")
            .arg(&unpacked),
        &format!("unpack {}", package.name),
    );
    let archive_root = format!("{}-{}/", package.name, package.version);
    // Files the manifest names outside the package directory (a `readme` or
    // `license-file` of `../README.md`) are packaged at the archive root.
    let original: toml::Value = toml::from_str(
        &fs::read_to_string(unpacked.join(&archive_root).join("Cargo.toml.orig"))
            .expect("packaged Cargo.toml.orig"),
    )
    .expect("Cargo.toml.orig TOML");
    let mut relocated = BTreeMap::new();
    for field in ["readme", "license-file"] {
        if let Some(named) = original["package"].get(field).and_then(toml::Value::as_str)
            && named.split('/').any(|component| component == "..")
        {
            let mut components: Vec<&str> = path_in_vcs
                .split('/')
                .filter(|part| !part.is_empty())
                .collect();
            for part in named.split('/') {
                match part {
                    ".." => assert!(
                        components.pop().is_some(),
                        "named file stays inside the repository"
                    ),
                    "." | "" => {}
                    other => components.push(other),
                }
            }
            relocated.insert(
                named.rsplit('/').next().expect("file name").to_owned(),
                components.join("/"),
            );
        }
    }
    let mut expected: Vec<(String, String, Vec<u8>)> = Vec::new();
    for entry in &package.files {
        let relative = entry.strip_prefix(&archive_root).expect("archive root");
        let bytes = fs::read(unpacked.join(entry))
            .unwrap_or_else(|error| panic!("{entry}: unpacked file: {error}"));
        match relative {
            "Cargo.toml" | "Cargo.lock" => continue,
            ".cargo_vcs_info.json" => {
                let record: Value = serde_json::from_slice(&bytes).expect("VCS record JSON");
                assert_eq!(
                    record["git"]["sha1"].as_str(),
                    Some(rev),
                    "{}: packaged from the pinned commit",
                    package.name
                );
                assert_eq!(
                    record["path_in_vcs"].as_str(),
                    Some(path_in_vcs),
                    "{}: packaged from its path in the pinned tree",
                    package.name
                );
                assert!(
                    record["git"].get("dirty").is_none(),
                    "{}: the packaged files are clean at the pin",
                    package.name
                );
                continue;
            }
            _ => {}
        }
        let tracked = if let Some(tracked) = relocated.get(relative) {
            tracked.clone()
        } else if relative == "Cargo.toml.orig" {
            format!("{path_in_vcs}/Cargo.toml")
        } else if path_in_vcs.is_empty() {
            relative.to_owned()
        } else {
            format!("{path_in_vcs}/{relative}")
        };
        expected.push((tracked, relative.to_owned(), bytes));
    }
    // One `git cat-file --batch` reads every blob at the pin.
    let mut reader = Command::new("git")
        .arg("-C")
        .arg(checkout)
        .args(["cat-file", "--batch"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn git cat-file --batch");
    let request = expected
        .iter()
        .map(|(tracked, _, _)| format!("{rev}:{tracked}\n"))
        .collect::<String>();
    let mut stdin = reader.stdin.take().expect("cat-file stdin");
    let writer = std::thread::spawn(move || {
        use std::io::Write as _;
        stdin
            .write_all(request.as_bytes())
            .expect("write cat-file request");
    });
    let output = reader
        .wait_with_output()
        .expect("git cat-file --batch output");
    writer.join().expect("cat-file request thread");
    assert!(
        output.status.success(),
        "{}: git cat-file --batch",
        package.name
    );
    let mut cursor = &output.stdout[..];
    for (tracked, relative, bytes) in &expected {
        let header_end = cursor
            .iter()
            .position(|byte| *byte == b'\n')
            .expect("cat-file header");
        let header = std::str::from_utf8(&cursor[..header_end]).expect("cat-file header UTF-8");
        let fields: Vec<&str> = header.split(' ').collect();
        assert!(
            fields.len() == 3 && fields[1] == "blob",
            "{}: {tracked} is a blob at {rev} (cat-file said {header:?})",
            package.name
        );
        let size: usize = fields[2].parse().expect("blob size");
        let blob = &cursor[header_end + 1..header_end + 1 + size];
        assert!(
            blob == &bytes[..],
            "{}: packaged {relative} equals {tracked} at {rev}",
            package.name
        );
        cursor = &cursor[header_end + 1 + size + 1..];
    }
    assert!(
        cursor.is_empty(),
        "{}: cat-file answered every request once",
        package.name
    );
    assert!(
        expected.len() > 1,
        "{}: the tree comparison read files",
        package.name
    );
}

fn assert_registry_only_package_consumer(registry: &Path, scratch: &Path, home: &Path) {
    let consumer = scratch.join("registry-consumer");
    fs::create_dir_all(consumer.join("src")).expect("registry consumer source");
    fs::create_dir_all(consumer.join(".cargo")).expect("registry consumer Cargo config");
    let manifest = r#"[package]
name = "bbnf-w3-registry-consumer"
version = "0.0.0"
edition = "2024"
publish = false

[dependencies]
bbnf = { version = "=0.3.0", default-features = false }
bbnf-path = { version = "=0.1.0" }
parse_that = { version = "=0.4.0", default-features = false }
"#;
    let parsed: toml::Value = toml::from_str(manifest).expect("consumer manifest TOML");
    assert!(
        parsed.get("patch").is_none()
            && parsed.get("workspace").is_none()
            && parsed["dependencies"]
                .as_table()
                .expect("consumer dependencies")
                .values()
                .all(
                    |dependency| toml_keys(dependency) == string_set(&["version"])
                        || toml_keys(dependency) == string_set(&["default-features", "version"])
                ),
        "external consumer inherits no path, git, patch, or workspace authority"
    );
    fs::write(consumer.join("Cargo.toml"), manifest).expect("registry consumer manifest");
    fs::write(
        consumer.join(".cargo/config.toml"),
        local_registry_config(registry),
    )
    .expect("registry consumer configuration");
    fs::write(
        consumer.join("src/main.rs"),
        r##"use bbnf::{CompileOptions, GrammarInstanceInput, StageResult, compile_instance};

fn main() {
    assert_eq!(parse_that::kernel::KERNEL_ABI, 4);
    let instance = br#"{"schema":"bbnf.grammar-instance.v1","logical_path":"registry/crossing.bbnf","entry":"document","trivia_regex":null,"pratt_tables":[]}"#;
    let grammar = br#"document = "x";"#;
    let StageResult::Accepted { value, diagnostics } =
        compile_instance(GrammarInstanceInput::new(instance, grammar), CompileOptions::default())
    else {
        panic!("registry-only BBNF consumer rejected its generic grammar");
    };
    assert!(diagnostics.is_empty());
    assert!(
        value
            .generated()
            .manifest()
            .starts_with("{\"schema\":\"bbnf.generated-rust.v6\",")
    );
}
"##,
    )
    .expect("registry consumer source");

    let run = |arguments: &[&str], label: &str| {
        command_output(
            Command::new(env!("CARGO"))
                .args(arguments)
                .current_dir(&consumer)
                .env("CARGO_HOME", home)
                .env("CARGO_NET_OFFLINE", "true")
                .env("CARGO_PROFILE_DEV_DEBUG", "0")
                .env("CARGO_TARGET_DIR", scratch.join("consumer-target"))
                .env_remove("RUSTC_WRAPPER"),
            label,
        )
    };
    run(
        &["generate-lockfile", "--offline"],
        "registry consumer lock generation",
    );
    let metadata_output = run(
        &["metadata", "--locked", "--offline", "--format-version", "1"],
        "registry consumer metadata",
    );
    let consumer_metadata: Value =
        serde_json::from_slice(&metadata_output.stdout).expect("registry consumer metadata JSON");
    for (name, version) in [
        ("bbnf", "0.3.0"),
        ("parse_that", "0.4.0"),
        ("parse-that-regex", "0.1.0"),
        ("parse-that-simd", "0.1.0"),
        ("parse-that-egraph", "0.1.0"),
        ("pprint", "0.5.0-alpha.0"),
        ("bbnf-ir", "0.2.0"),
        ("bbnf-ser", "0.1.0"),
        ("bbnf-path", "0.1.0"),
    ] {
        let package = consumer_metadata["packages"]
            .as_array()
            .expect("consumer metadata packages")
            .iter()
            .find(|package| {
                package["name"].as_str() == Some(name)
                    && package["version"].as_str() == Some(version)
            })
            .unwrap_or_else(|| panic!("{name} {version}: registry consumer package"));
        assert!(
            package["source"].as_str() == Some(CRATES_IO_INDEX),
            "{name} {version}: source replacement retains the crates.io logical identity while \
             the isolated offline Cargo home proves resolution from the temporary package \
             registry; observed source was {:?}",
            package["source"]
        );
    }
    assert!(
        consumer_metadata["packages"]
            .as_array()
            .unwrap()
            .iter()
            .all(
                |package| package["source"].as_str() == Some(CRATES_IO_INDEX)
                    || package["name"].as_str() == Some("bbnf-w3-registry-consumer")
            ),
        "the registry consumer resolves no git or path package"
    );
    run(
        &["run", "--locked", "--offline", "--quiet"],
        "registry-only BBNF consumer",
    );
}

#[test]
fn exact_packages_cross_a_fresh_local_registry_and_public_runtime() {
    // LW3.COMP-1b-T: the sk-v26 form packaged `../parse-that-skv26`'s
    // parse_that 0.5.0 and bbnf-regex 0.1.0 and bound them to the ABI-3
    // receipt. The sk-v27 crossing packages the whole lock-step closure from
    // the pinned commits (pprint, the parse-that crates) and this repository,
    // each twice and registry-verified, binds the upstream archives to the
    // ABI-4 receipt and to their git trees, then drives a registry-only
    // consumer and the generic public runtime.
    let record_to = std::env::var_os("BBNF_W3_ABI4_RECEIPT_OUT").map(PathBuf::from);
    let receipt = if record_to.is_none() {
        assert_sha(PARSE_THAT_ABI4_RECEIPT, PARSE_THAT_ABI4_RECEIPT_SHA256);
        let receipt = json_file(PARSE_THAT_ABI4_RECEIPT);
        assert_eq!(
            receipt
                .as_object()
                .expect("receipt object")
                .keys()
                .cloned()
                .collect::<BTreeSet<_>>(),
            BTreeSet::from([
                "kernel_abi".to_owned(),
                "packages".to_owned(),
                "parse_that_rev".to_owned(),
                "pprint_rev".to_owned(),
                "schema".to_owned(),
            ]),
            "ABI-4 receipt surface"
        );
        assert_eq!(
            receipt["schema"].as_str(),
            Some(PARSE_THAT_ABI4_RECEIPT_SCHEMA)
        );
        assert_eq!(receipt["kernel_abi"].as_u64(), Some(4));
        Some(receipt)
    } else {
        None
    };

    let temporary = tempfile::tempdir().expect("W3 package crossing");
    let scratch = temporary.path();
    let started = std::time::Instant::now();
    let phase = |label: &str| {
        eprintln!(
            "[crossing] {label}: {:.1}s",
            started.elapsed().as_secs_f64()
        )
    };
    let home = isolated_lockstep_cargo_home(scratch);
    let registry = scratch.join("w3-local-registry");

    // The workspace's own lock names the pins; its metadata gives the pinned
    // checkouts (inside the isolated home) and every owned package's record.
    let workspace_lock = root().join("Cargo.lock");
    let bootstrap_third_party =
        lockstep_third_party_packages(std::slice::from_ref(&workspace_lock));
    write_lockstep_registry(
        &registry,
        &bootstrap_third_party,
        &json!({"packages": []}),
        &[],
    );
    let metadata = lockstep_metadata(&home, &registry);
    let owned = sk_v27_owned_git_packages(&metadata);
    let checkout_root = |repository: &str| {
        let anchor = if repository == "pprint" {
            "pprint"
        } else {
            "parse_that"
        };
        let manifest = &owned[anchor].1;
        let mut directory = manifest.parent().expect("manifest directory").to_path_buf();
        while !directory.join(".git").exists() {
            directory = directory.parent().expect("checkout root").to_path_buf();
        }
        directory
    };
    let rev_of = |repository: &str| {
        let anchor = if repository == "pprint" {
            "pprint"
        } else {
            "parse_that"
        };
        owned[anchor]
            .2
            .rsplit_once('#')
            .expect("git source fragment")
            .1
            .to_owned()
    };
    let parse_that_rev = rev_of("parse-that");
    let pprint_rev = rev_of("pprint");
    let locks = vec![
        workspace_lock,
        checkout_root("parse-that").join("rust/Cargo.lock"),
        checkout_root("pprint").join("rust/Cargo.lock"),
    ];
    let third_party = lockstep_third_party_packages(&locks);

    let mut packaged = Vec::new();
    let mut receipt_rows = Vec::new();
    for (name, repository) in SK_V27_UPSTREAM_PACKAGES {
        write_lockstep_registry(&registry, &third_party, &metadata, &packaged);
        let (version, manifest, _) = owned
            .get(name)
            .unwrap_or_else(|| panic!("{name}: owned package in the pinned graph"))
            .clone();
        let package = package_twice_in(&home, scratch, &manifest, name, &version, &registry);
        let checkout = checkout_root(repository);
        let rev = rev_of(repository);
        let path_in_vcs = manifest
            .parent()
            .unwrap()
            .strip_prefix(&checkout)
            .expect("package inside its checkout")
            .to_str()
            .expect("UTF-8 path")
            .to_owned();
        assert_archive_is_the_pinned_tree(&package, &checkout, &rev, &path_in_vcs, scratch);
        receipt_rows.push(json!({
            "name": package.name,
            "version": package.version,
            "repository": repository,
            "rev": rev,
            "path_in_vcs": path_in_vcs,
            "sha256": package.sha256,
            "files": package.files.len(),
            "successive_archives_byte_identical": true,
            "registry_verify": "resolved and built from the local registry by this law",
        }));
        packaged.push(package);
    }

    if let Some(path) = record_to {
        let record = json!({
            "schema": PARSE_THAT_ABI4_RECEIPT_SCHEMA,
            "kernel_abi": 4,
            "parse_that_rev": parse_that_rev,
            "pprint_rev": pprint_rev,
            "packages": receipt_rows,
        });
        fs::write(
            &path,
            format!("{}\n", serde_json::to_string_pretty(&record).unwrap()),
        )
        .expect("write ABI-4 receipt");
        panic!(
            "ABI-4 receipt recorded to {} (record mode never reads GREEN)",
            path.display()
        );
    }
    let receipt = receipt.expect("receipt read");
    assert_eq!(
        receipt["parse_that_rev"].as_str(),
        Some(parse_that_rev.as_str()),
        "the receipt is the record of the parse-that commit this workspace pins"
    );
    assert_eq!(
        receipt["pprint_rev"].as_str(),
        Some(pprint_rev.as_str()),
        "the receipt is the record of the pprint commit this workspace pins"
    );
    let sealed = receipt["packages"].as_array().expect("receipt packages");
    assert_eq!(
        sealed.len(),
        receipt_rows.len(),
        "the receipt names every upstream package, once"
    );
    for (expected, fresh) in sealed.iter().zip(&receipt_rows) {
        assert_eq!(
            expected, fresh,
            "{}: the fresh archive binds the sealed ABI-4 receipt",
            fresh["name"]
        );
        assert_eq!(
            expected["successive_archives_byte_identical"].as_bool(),
            Some(true)
        );
        assert_eq!(
            expected["registry_verify"].as_str(),
            Some("resolved and built from the local registry by this law")
        );
    }
    let kernel_abi_source = fs::read_to_string(
        owned["parse_that"]
            .1
            .parent()
            .unwrap()
            .join("src/kernel/mod.rs"),
    )
    .expect("pinned kernel module");
    assert!(
        kernel_abi_source.contains("pub const KERNEL_ABI: u32 = 4;"),
        "the pinned parse_that declares kernel ABI 4 (COMP-4b)"
    );

    phase("upstream packaged");
    for name in SK_V27_OWNED_PACKAGES {
        write_lockstep_registry(&registry, &third_party, &metadata, &packaged);
        let entry = metadata["packages"]
            .as_array()
            .unwrap()
            .iter()
            .find(|package| package["name"].as_str() == Some(name) && package["source"].is_null())
            .unwrap_or_else(|| panic!("{name}: workspace package"));
        let version = entry["version"].as_str().unwrap().to_owned();
        let manifest = PathBuf::from(entry["manifest_path"].as_str().unwrap());
        packaged.push(package_twice_in(
            &home, scratch, &manifest, name, &version, &registry,
        ));
    }
    write_lockstep_registry(&registry, &third_party, &metadata, &packaged);
    phase("owned packaged");
    for package in &packaged {
        assert_package_resolves_from_registry(&home, scratch, package, &registry);
    }
    phase("resolved");
    // One isolated home and one target directory for the two registry-only
    // builds: the consumer compiles every packaged library from its archive.
    assert_registry_only_package_consumer(&registry, scratch, &home);
    phase("consumer built and ran");
    assert_generic_external_runtime_is_dual_root_identical_and_executes_all_public_cases(
        &registry,
        &home,
        &scratch.join("consumer-target"),
    );
}

fn attribute_selects_module_path(attribute: &syn::Attribute) -> bool {
    if attribute.path().is_ident("path") {
        return true;
    }
    attribute.path().is_ident("cfg_attr")
        && attribute.meta.require_list().is_ok_and(|list| {
            list.tokens
                .to_string()
                .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                .any(|token| token == "path")
        })
}

fn collect_use_alias_paths(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    paths: &mut Vec<Vec<String>>,
    aliases: &mut Vec<(String, Vec<String>)>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(normalized_rust_identifier(&path.ident.to_string()).to_owned());
            collect_use_alias_paths(&path.tree, prefix, paths, aliases);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let name = normalized_rust_identifier(&name.ident.to_string()).to_owned();
            if name == "self" {
                paths.push(prefix.clone());
                if let Some(alias) = prefix.last() {
                    aliases.push((alias.clone(), prefix.clone()));
                }
            } else {
                prefix.push(name.clone());
                paths.push(prefix.clone());
                aliases.push((name, prefix.clone()));
                prefix.pop();
            }
        }
        syn::UseTree::Rename(rename) => {
            let name = normalized_rust_identifier(&rename.ident.to_string()).to_owned();
            let alias = normalized_rust_identifier(&rename.rename.to_string()).to_owned();
            if name == "self" {
                paths.push(prefix.clone());
                aliases.push((alias, prefix.clone()));
            } else {
                prefix.push(name);
                paths.push(prefix.clone());
                aliases.push((alias, prefix.clone()));
                prefix.pop();
            }
        }
        syn::UseTree::Glob(_) => paths.push(prefix.clone()),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_alias_paths(item, prefix, paths, aliases);
            }
        }
    }
}

fn external_effect_macro(name: &str) -> bool {
    matches!(
        normalized_rust_identifier(name),
        "asm"
            | "env"
            | "global_asm"
            | "include"
            | "include_bytes"
            | "include_str"
            | "llvm_asm"
            | "option_env"
    )
}

fn macro_name_resolves(
    name: &str,
    aliases: &[(String, Vec<String>)],
    matches_target: impl Fn(&str) -> bool + Copy,
) -> bool {
    fn visit(
        path: &[String],
        aliases: &[(String, Vec<String>)],
        matches_target: impl Fn(&str) -> bool + Copy,
        seen: &mut BTreeSet<Vec<String>>,
    ) -> bool {
        if path.last().is_some_and(|name| matches_target(name))
            && (path.len() == 1
                || matches!(
                    path.first().map(String::as_str),
                    Some("std" | "core" | "crate" | "self" | "super")
                ))
        {
            return true;
        }
        if !seen.insert(path.to_vec()) {
            return false;
        }
        let Some(root) = path.first() else {
            return false;
        };
        aliases
            .iter()
            .filter(|(alias, _)| alias == root)
            .any(|(_, authority)| {
                let mut expanded = authority.clone();
                expanded.extend_from_slice(&path[1..]);
                visit(&expanded, aliases, matches_target, seen)
            })
    }

    visit(
        &[normalized_rust_identifier(name).to_owned()],
        aliases,
        matches_target,
        &mut BTreeSet::new(),
    )
}

fn rust_macro_tokens(input: &str) -> Vec<String> {
    fn skip_quoted(bytes: &[u8], mut at: usize, quote: u8) -> usize {
        at += 1;
        while at < bytes.len() {
            if bytes[at] == b'\\' {
                at = (at + 2).min(bytes.len());
            } else if bytes[at] == quote {
                return at + 1;
            } else {
                at += 1;
            }
        }
        at
    }

    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut at = 0;
    while at < bytes.len() {
        if bytes[at] == b'"' {
            at = skip_quoted(bytes, at, b'"');
            continue;
        }
        if bytes[at] == b'\'' {
            let next_quote = bytes[at + 1..]
                .iter()
                .position(|byte| *byte == b'\'')
                .map(|offset| at + 1 + offset);
            if next_quote.is_some_and(|end| !bytes[at + 1..end].iter().any(u8::is_ascii_whitespace))
            {
                at = next_quote.unwrap() + 1;
                continue;
            }
        }
        if bytes[at].is_ascii_alphabetic() || bytes[at] == b'_' {
            let start = at;
            at += 1;
            while at < bytes.len() && (bytes[at].is_ascii_alphanumeric() || bytes[at] == b'_') {
                at += 1;
            }
            let identifier = &input[start..at];
            if matches!(identifier, "b" | "br" | "r")
                && (bytes.get(at) == Some(&b'"') || bytes.get(at) == Some(&b'#'))
            {
                let mut hashes = 0;
                while bytes.get(at + hashes) == Some(&b'#') {
                    hashes += 1;
                }
                if bytes.get(at + hashes) == Some(&b'"') {
                    at += hashes + 1;
                    while at < bytes.len() {
                        if bytes[at] == b'"'
                            && (0..hashes).all(|offset| bytes.get(at + 1 + offset) == Some(&b'#'))
                        {
                            at += 1 + hashes;
                            break;
                        }
                        at += 1;
                    }
                    continue;
                }
            }
            if identifier == "r"
                && bytes.get(at) == Some(&b'#')
                && bytes
                    .get(at + 1)
                    .is_some_and(|byte| byte.is_ascii_alphabetic() || *byte == b'_')
            {
                at += 1;
                let raw_start = at;
                at += 1;
                while at < bytes.len() && (bytes[at].is_ascii_alphanumeric() || bytes[at] == b'_') {
                    at += 1;
                }
                tokens.push(input[raw_start..at].to_owned());
                continue;
            }
            tokens.push(normalized_rust_identifier(identifier).to_owned());
            continue;
        }
        if bytes[at] == b':' && bytes.get(at + 1) == Some(&b':') {
            tokens.push("::".to_owned());
            at += 2;
            continue;
        }
        if bytes[at] == b'=' && bytes.get(at + 1) == Some(&b'>') {
            tokens.push("=>".to_owned());
            at += 2;
            continue;
        }
        if matches!(
            bytes[at],
            b'!' | b'$'
                | b'#'
                | b'('
                | b')'
                | b'*'
                | b'+'
                | b','
                | b';'
                | b'<'
                | b'='
                | b'>'
                | b'?'
                | b'['
                | b']'
                | b'{'
                | b'}'
        ) {
            tokens.push((bytes[at] as char).to_string());
        }
        at += 1;
    }
    tokens
}

fn matching_macro_delimiter(tokens: &[String], open: usize) -> Option<usize> {
    let expected = match tokens.get(open).map(String::as_str) {
        Some("(") => ")",
        Some("[") => "]",
        Some("{") => "}",
        _ => return None,
    };
    let mut stack = vec![expected];
    for (index, token) in tokens.iter().enumerate().skip(open + 1) {
        match token.as_str() {
            "(" => stack.push(")"),
            "[" => stack.push("]"),
            "{" => stack.push("}"),
            ")" | "]" | "}" => {
                if stack.last().copied() != Some(token) {
                    return None;
                }
                stack.pop();
                if stack.is_empty() {
                    return Some(index);
                }
            }
            _ => {}
        }
    }
    None
}

fn macro_effect_token_ranges(name: &str, tokens: &[String]) -> Vec<(usize, usize)> {
    if normalized_rust_identifier(name) != "macro_rules" {
        return vec![(0, tokens.len())];
    }

    let mut ranges = Vec::new();
    let mut at = 0;
    while at < tokens.len() {
        if tokens[at] != "=>" {
            at += 1;
            continue;
        }
        let open = at + 1;
        let close = matching_macro_delimiter(tokens, open)
            .unwrap_or_else(|| panic!("macro_rules transcriber after token {at} is not one group"));
        ranges.push((open + 1, close));
        at = close + 1;
    }
    assert!(
        !ranges.is_empty(),
        "macro_rules definition exposes at least one transcriber"
    );
    ranges
}

fn identifier_macro_token(token: &str) -> bool {
    token
        .bytes()
        .next()
        .is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_')
}

fn macro_tokens_have_dynamic_effect_composition(tokens: &[String]) -> bool {
    for index in 0..tokens.len() {
        if tokens[index] == "$"
            && tokens
                .get(index + 1)
                .is_some_and(|token| identifier_macro_token(token))
            && tokens.get(index + 2).map(String::as_str) == Some("!")
        {
            return true;
        }
        if tokens[index] != "$"
            || !matches!(
                tokens.get(index + 1).map(String::as_str),
                Some("(" | "[" | "{")
            )
        {
            continue;
        }
        let open = index + 1;
        let Some(close) = matching_macro_delimiter(tokens, open) else {
            return true;
        };
        let body = &tokens[open + 1..close];
        let body_has_metavariable = body
            .windows(2)
            .any(|pair| pair[0] == "$" && identifier_macro_token(&pair[1]));
        if (body_has_metavariable
            && body
                .iter()
                .any(|token| matches!(token.as_str(), "::" | "!")))
            || macro_tokens_have_dynamic_effect_composition(body)
        {
            return true;
        }
        let mut cursor = close + 1;
        let separator = (!matches!(
            tokens.get(cursor).map(String::as_str),
            Some("*" | "+" | "?")
        ))
        .then(|| tokens.get(cursor).map(String::as_str))
        .flatten();
        if separator.is_some() {
            cursor += 1;
        }
        if !matches!(
            tokens.get(cursor).map(String::as_str),
            Some("*" | "+" | "?")
        ) {
            continue;
        }
        let after_repetition = cursor + 1;
        if matches!(separator, Some("::" | "!"))
            || matches!(
                tokens.get(after_repetition).map(String::as_str),
                Some("::" | "!")
            )
            || matches!(
                index
                    .checked_sub(1)
                    .and_then(|before| tokens.get(before))
                    .map(String::as_str),
                Some("::")
            )
        {
            return true;
        }
    }
    false
}

fn macro_invocation_at(tokens: &[String], at: usize) -> Option<(String, usize)> {
    let first = tokens.get(at)?;
    if !identifier_macro_token(first) {
        return None;
    }
    let mut path = vec![normalized_rust_identifier(first).to_owned()];
    let mut cursor = at + 1;
    while tokens.get(cursor).map(String::as_str) == Some("::") {
        let segment = tokens.get(cursor + 1)?;
        if !identifier_macro_token(segment) {
            break;
        }
        path.push(normalized_rust_identifier(segment).to_owned());
        cursor += 2;
    }
    if tokens.get(cursor).map(String::as_str) != Some("!")
        || !matches!(
            tokens.get(cursor + 1).map(String::as_str),
            Some("(" | "[" | "{")
        )
    {
        return None;
    }
    let close = matching_macro_delimiter(tokens, cursor + 1)?;
    Some((path.join("::"), close + 1))
}

fn macro_path_resolves(
    path: &str,
    aliases: &[(String, Vec<String>)],
    matches_target: impl Fn(&str) -> bool + Copy,
) -> bool {
    fn visit(
        segments: Vec<String>,
        aliases: &[(String, Vec<String>)],
        matches_target: impl Fn(&str) -> bool + Copy,
        seen: &mut BTreeSet<Vec<String>>,
    ) -> bool {
        if !seen.insert(segments.clone()) {
            return false;
        }
        let Some(first) = segments.first() else {
            return false;
        };
        if segments.last().is_some_and(|name| matches_target(name))
            && (segments.len() == 1
                || matches!(
                    segments.first().map(String::as_str),
                    Some("std" | "core" | "crate" | "self" | "super")
                ))
        {
            return true;
        }
        aliases
            .iter()
            .filter(|(alias, _)| alias == first)
            .any(|(_, authority)| {
                let mut expanded = authority.clone();
                expanded.extend_from_slice(&segments[1..]);
                visit(expanded, aliases, matches_target, seen)
            })
    }

    visit(
        path.split("::")
            .map(|segment| normalized_rust_identifier(segment).to_owned())
            .collect(),
        aliases,
        matches_target,
        &mut BTreeSet::new(),
    )
}

fn resolved_macro_alias_paths(
    path: &str,
    aliases: &[(String, Vec<String>)],
) -> BTreeSet<Vec<String>> {
    fn visit(
        path: Vec<String>,
        aliases: &[(String, Vec<String>)],
        seen: &mut BTreeSet<Vec<String>>,
        resolved: &mut BTreeSet<Vec<String>>,
    ) {
        if !seen.insert(path.clone()) {
            return;
        }
        let Some(root) = path.first() else {
            return;
        };
        let authorities = aliases
            .iter()
            .filter(|(alias, _)| alias == root)
            .map(|(_, authority)| authority)
            .collect::<Vec<_>>();
        if authorities.is_empty() {
            resolved.insert(path);
            return;
        }
        for authority in authorities {
            let mut expanded = authority.clone();
            expanded.extend_from_slice(&path[1..]);
            visit(expanded, aliases, seen, resolved);
        }
    }

    let mut resolved = BTreeSet::new();
    visit(
        path.split("::")
            .map(|segment| normalized_rust_identifier(segment).to_owned())
            .collect(),
        aliases,
        &mut BTreeSet::new(),
        &mut resolved,
    );
    resolved
}

fn macro_name_is_pure_stringifier(
    name: &str,
    aliases: &[(String, Vec<String>)],
    pure_stringifiers: &BTreeSet<String>,
    macro_definitions: &BTreeSet<String>,
) -> bool {
    let resolved = resolved_macro_alias_paths(name, aliases);
    !resolved.is_empty()
        && resolved.iter().all(|path| match path.as_slice() {
            [name] if name == "stringify" => !macro_definitions.contains("stringify"),
            [root, name] if matches!(root.as_str(), "std" | "core") && name == "stringify" => true,
            path => {
                let Some(name) = path.last() else {
                    return false;
                };
                name != "stringify"
                    && pure_stringifiers.contains(name)
                    && (path.len() == 1
                        || matches!(
                            path.first().map(String::as_str),
                            Some("crate" | "self" | "super")
                        ))
            }
        })
}

fn executable_macro_tokens(
    tokens: &[String],
    aliases: &[(String, Vec<String>)],
    pure_stringifiers: &BTreeSet<String>,
    macro_definitions: &BTreeSet<String>,
) -> Vec<String> {
    let mut executable = Vec::new();
    let mut at = 0;
    while at < tokens.len() {
        if let Some((macro_path, after)) = macro_invocation_at(tokens, at)
            && macro_name_is_pure_stringifier(
                &macro_path,
                aliases,
                pure_stringifiers,
                macro_definitions,
            )
        {
            at = after;
            continue;
        }
        executable.push(tokens[at].clone());
        at += 1;
    }
    executable
}

fn macro_rules_definition_name(name: &str, source: &str) -> Option<String> {
    if normalized_rust_identifier(name) != "macro_rules" {
        return None;
    }
    rust_macro_tokens(source)
        .iter()
        .find(|token| identifier_macro_token(token))
        .map(|token| normalized_rust_identifier(token).to_owned())
}

fn macro_definition_names(macros: &[(String, String)]) -> BTreeSet<String> {
    macros
        .iter()
        .filter_map(|(name, source)| macro_rules_definition_name(name, source))
        .collect()
}

fn catalogued_item_macro(item: &syn::ItemMacro) -> (String, String) {
    let name = normalized_syn_path(&item.mac.path);
    let source = if normalized_rust_identifier(&name) == "macro_rules" {
        let definition = item
            .ident
            .as_ref()
            .expect("macro_rules item has a definition identifier");
        format!("{definition} {}", item.mac.tokens)
    } else {
        item.mac.tokens.to_string()
    };
    (name, source)
}

fn rust_source_macro_definition_names(source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct Definitions(Vec<(String, String)>);

    impl<'ast> syn::visit::Visit<'ast> for Definitions {
        fn visit_item_macro(&mut self, item: &'ast syn::ItemMacro) {
            self.0.push(catalogued_item_macro(item));
            for attribute in &item.attrs {
                self.visit_attribute(attribute);
            }
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            self.0
                .push((normalized_syn_path(&macro_.path), macro_.tokens.to_string()));
            syn::visit::visit_macro(self, macro_);
        }
    }

    let syntax = syn::parse_file(source).expect("macro-definition source is valid Rust");
    let mut definitions = Definitions::default();
    definitions.visit_file(&syntax);
    macro_definition_names(&definitions.0)
}

fn pure_stringifier_names(
    macros: &[(String, String)],
    aliases: &[(String, Vec<String>)],
    known_macro_definitions: &BTreeSet<String>,
) -> BTreeSet<String> {
    let local_macro_definitions = macro_definition_names(macros);
    let mut pure = BTreeSet::new();
    loop {
        let mut changed = false;
        for defined_name in &local_macro_definitions {
            if defined_name == "stringify" || pure.contains(defined_name) {
                continue;
            }
            let stringify_only = macros
                .iter()
                .filter(|(name, source)| {
                    macro_rules_definition_name(name, source).as_ref() == Some(defined_name)
                })
                .all(|(name, source)| {
                    let tokens = rust_macro_tokens(source);
                    let ranges = macro_effect_token_ranges(name, &tokens);
                    ranges.iter().all(|(start, end)| {
                        executable_macro_tokens(
                            &tokens[*start..*end],
                            aliases,
                            &pure,
                            known_macro_definitions,
                        )
                        .iter()
                        .all(|token| {
                            matches!(
                                token.as_str(),
                                "(" | ")" | "[" | "]" | "{" | "}" | "," | ";"
                            )
                        })
                    })
                });
            if stringify_only {
                pure.insert(defined_name.clone());
                changed = true;
            }
        }
        if !changed {
            return pure;
        }
    }
}

fn macro_hides_module_ingress(
    name: &str,
    source: &str,
    aliases: &[(String, Vec<String>)],
    pure_stringifiers: &BTreeSet<String>,
    macro_definitions: &BTreeSet<String>,
) -> bool {
    if macro_name_is_pure_stringifier(name, aliases, pure_stringifiers, macro_definitions) {
        return false;
    }
    if macro_name_resolves(name, aliases, |candidate| candidate == "include") {
        return true;
    }
    let tokens = rust_macro_tokens(source);
    for (start, end) in macro_effect_token_ranges(name, &tokens) {
        let executable = executable_macro_tokens(
            &tokens[start..end],
            aliases,
            pure_stringifiers,
            macro_definitions,
        );
        let tokens = executable.as_slice();
        let has_unresolved_use = tokens.iter().enumerate().any(|(index, token)| {
            token == "use"
                && index
                    .checked_sub(1)
                    .and_then(|previous| tokens.get(previous))
                    .map(String::as_str)
                    != Some("$")
        });
        if has_unresolved_use
            || macro_tokens_have_dynamic_effect_composition(tokens)
            || tokens.iter().any(|token| token == "mod")
        {
            return true;
        }
        for index in 0..tokens.len() {
            if let Some((macro_path, _)) = macro_invocation_at(tokens, index)
                && macro_path_resolves(&macro_path, aliases, |candidate| candidate == "include")
            {
                return true;
            }
        }
        for index in 0..tokens.len().saturating_sub(2) {
            if tokens[index] != "#" || tokens[index + 1] != "[" {
                continue;
            }
            if tokens[index + 2] == "path" {
                return true;
            }
            if tokens[index + 2] == "cfg_attr" {
                let end = tokens[index + 3..]
                    .iter()
                    .position(|token| token == "]")
                    .map_or(tokens.len(), |offset| index + 3 + offset);
                if tokens[index + 3..end]
                    .windows(2)
                    .any(|pair| pair[0] == "path" && pair[1] == "=")
                {
                    return true;
                }
            }
        }
    }
    false
}

fn collect_active_rust_module_graph(entry: &Path, sources: &mut BTreeMap<String, String>) {
    use syn::visit::Visit as _;

    fn collect_item_modules(
        items: &[syn::Item],
        module_directory: &Path,
        external: &mut Vec<(PathBuf, String)>,
    ) -> usize {
        let mut count = 0;
        for item in items {
            let syn::Item::Mod(module) = item else {
                continue;
            };
            count += 1;
            assert!(
                !module.attrs.iter().any(attribute_selects_module_path),
                "{}: #[path] and cfg_attr(path) module redirection are forbidden",
                module.ident
            );
            if let Some((_, nested)) = &module.content {
                count += collect_item_modules(
                    nested,
                    &module_directory.join(module.ident.to_string()),
                    external,
                );
            } else {
                external.push((module_directory.to_path_buf(), module.ident.to_string()));
            }
        }
        count
    }

    #[derive(Default)]
    struct MacroCatalog {
        aliases: Vec<(String, Vec<String>)>,
        macros: Vec<(String, String)>,
    }
    impl<'ast> syn::visit::Visit<'ast> for MacroCatalog {
        fn visit_item_macro(&mut self, item: &'ast syn::ItemMacro) {
            self.macros.push(catalogued_item_macro(item));
            for attribute in &item.attrs {
                self.visit_attribute(attribute);
            }
        }

        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            collect_use_alias_paths(
                &item.tree,
                &mut Vec::new(),
                &mut Vec::new(),
                &mut self.aliases,
            );
            syn::visit::visit_item_use(self, item);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            self.macros
                .push((normalized_syn_path(&macro_.path), macro_.tokens.to_string()));
            syn::visit::visit_macro(self, macro_);
        }
    }

    struct AllModules<'a> {
        structural: usize,
        macro_ingress: Vec<String>,
        aliases: &'a [(String, Vec<String>)],
        pure_stringifiers: &'a BTreeSet<String>,
        macro_definitions: &'a BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for AllModules<'_> {
        fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
            self.structural += 1;
            syn::visit::visit_item_mod(self, module);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            let name = normalized_syn_path(&macro_.path);
            if macro_hides_module_ingress(
                &name,
                &macro_.tokens.to_string(),
                self.aliases,
                self.pure_stringifiers,
                self.macro_definitions,
            ) {
                self.macro_ingress.push(name);
            }
            syn::visit::visit_macro(self, macro_);
        }
    }

    let repository = root();
    let repository_canonical = repository
        .canonicalize()
        .expect("repository canonical path");
    let entry_parent = entry.parent().expect("Rust entry parent").to_path_buf();
    let mut pending = vec![(entry.to_path_buf(), entry_parent)];
    let mut visited = BTreeMap::<PathBuf, PathBuf>::new();
    let mut graph_sources = BTreeMap::new();
    let mut graph_structural_counts = BTreeMap::new();
    while let Some((path, module_directory)) = pending.pop() {
        let metadata = fs::symlink_metadata(&path)
            .unwrap_or_else(|error| panic!("{}: active module metadata: {error}", path.display()));
        assert!(
            metadata.is_file() && !metadata.file_type().is_symlink(),
            "{}: active Rust module is one regular non-symlink file",
            path.display()
        );
        let canonical = path
            .canonicalize()
            .unwrap_or_else(|error| panic!("{}: canonical module path: {error}", path.display()));
        assert!(
            canonical.starts_with(&repository_canonical),
            "{}: active Rust module escapes the repository",
            path.display()
        );
        if let Some(previous_directory) =
            visited.insert(canonical.clone(), module_directory.clone())
        {
            assert_eq!(
                previous_directory,
                module_directory,
                "{}: one source cannot have two module-resolution authorities",
                path.display()
            );
            continue;
        }

        let relative = canonical
            .strip_prefix(&repository_canonical)
            .expect("active source remains in repository")
            .to_str()
            .expect("active source path is UTF-8")
            .replace('\\', "/");
        let source = fs::read_to_string(&canonical)
            .unwrap_or_else(|error| panic!("{}: active source UTF-8: {error}", path.display()));
        let syntax = syn::parse_file(&source)
            .unwrap_or_else(|error| panic!("{relative}: active Rust syntax: {error}"));
        let mut external = Vec::new();
        let structural_count =
            collect_item_modules(&syntax.items, &module_directory, &mut external);
        assert!(
            graph_structural_counts
                .insert(relative.clone(), structural_count)
                .is_none(),
            "{relative}: duplicate structural module count"
        );
        assert!(
            graph_sources.insert(relative.clone(), source).is_none(),
            "{relative}: duplicate active source in one module graph"
        );

        for (directory, name) in external {
            let flat = directory.join(format!("{name}.rs"));
            let nested = directory.join(&name).join("mod.rs");
            let candidates = [flat, nested]
                .into_iter()
                .filter(|candidate| candidate.exists())
                .collect::<Vec<_>>();
            assert_eq!(
                candidates.len(),
                1,
                "{relative}: external module `{name}` must resolve to exactly one source"
            );
            let child = candidates.into_iter().next().unwrap();
            let child_directory =
                if child.file_name().and_then(|value| value.to_str()) == Some("mod.rs") {
                    child.parent().expect("mod.rs parent").to_path_buf()
                } else {
                    directory.join(name)
                };
            pending.push((child, child_directory));
        }
    }

    let graph_macro_definitions: BTreeSet<String> = graph_sources
        .values()
        .flat_map(|source| rust_source_macro_definition_names(source))
        .collect();
    for (relative, source) in &graph_sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("{relative}: active Rust syntax: {error}"));
        let mut catalog = MacroCatalog::default();
        catalog.visit_file(&syntax);
        let pure_stringifiers =
            pure_stringifier_names(&catalog.macros, &catalog.aliases, &graph_macro_definitions);
        let mut all_modules = AllModules {
            structural: 0,
            macro_ingress: Vec::new(),
            aliases: &catalog.aliases,
            pure_stringifiers: &pure_stringifiers,
            macro_definitions: &graph_macro_definitions,
        };
        all_modules.visit_file(&syntax);
        assert_eq!(
            Some(&all_modules.structural),
            graph_structural_counts.get(relative),
            "{relative}: local/block macro-shaped module declarations are forbidden; \
             every active module must be structurally resolvable"
        );
        assert!(
            all_modules.macro_ingress.is_empty(),
            "{relative}: macros cannot hide module declarations, path redirection, or \
             transcriber-local alias ingress: {:?}",
            all_modules.macro_ingress
        );
    }
    for (relative, source) in graph_sources {
        assert!(
            sources.insert(relative.clone(), source).is_none(),
            "{relative}: duplicate active source across module graphs"
        );
    }
}

fn assert_structural_no_overfit(
    label: &str,
    source: &str,
    sealed_needles: &BTreeSet<String>,
    sealed_short_identities: &BTreeSet<String>,
    sealed_sources: &[Vec<u8>],
) {
    use syn::visit::Visit as _;

    fn compact_identifier(value: &str) -> String {
        value
            .bytes()
            .filter(u8::is_ascii_alphanumeric)
            .map(|byte| byte.to_ascii_lowercase() as char)
            .collect()
    }

    fn compact_authority(value: &str) -> String {
        value
            .chars()
            .filter(|character| {
                character.is_ascii_alphanumeric()
                    || matches!(character, '/' | '\\' | '.' | '_' | '-')
            })
            .flat_map(char::to_lowercase)
            .collect()
    }

    const FORBIDDEN_DISPATCHERS: [&str; 7] = [
        "pluginregistry",
        "formatdispatch",
        "dispatchformat",
        "hostcallback",
        "selectedinstance",
        "materializedinstance",
        "recoverydispatcher",
    ];

    #[derive(Default)]
    struct Surface {
        identifiers: Vec<String>,
        literals: Vec<Vec<u8>>,
        cfg_features: Vec<String>,
        macros: Vec<(String, String)>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Surface {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.identifiers.push(ident.to_string());
        }

        fn visit_lit_str(&mut self, literal: &'ast syn::LitStr) {
            self.literals.push(literal.value().into_bytes());
        }

        fn visit_lit_byte_str(&mut self, literal: &'ast syn::LitByteStr) {
            self.literals.push(literal.value());
        }

        fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
            if attribute.path().is_ident("cfg") || attribute.path().is_ident("cfg_attr") {
                let tokens = attribute
                    .meta
                    .require_list()
                    .map(|list| list.tokens.to_string())
                    .unwrap_or_default();
                if tokens.contains("feature") {
                    self.cfg_features.push(tokens);
                }
            }
            syn::visit::visit_attribute(self, attribute);
        }

        fn visit_macro(&mut self, item: &'ast syn::Macro) {
            self.macros.push((
                item.path
                    .segments
                    .last()
                    .map(|segment| segment.ident.to_string())
                    .unwrap_or_default(),
                item.tokens.to_string(),
            ));
            syn::visit::visit_macro(self, item);
        }
    }

    let syntax =
        syn::parse_file(source).unwrap_or_else(|error| panic!("{label}: Rust syntax: {error}"));
    let mut surface = Surface::default();
    surface.visit_file(&syntax);
    assert!(
        surface.cfg_features.is_empty(),
        "{label}: cfg(feature) dispatch: {:?}",
        surface.cfg_features
    );
    for identifier in &surface.identifiers {
        let compact = compact_identifier(identifier);
        for forbidden in FORBIDDEN_DISPATCHERS {
            assert!(
                !compact.contains(forbidden),
                "{label}: forbidden structural dispatcher identifier {identifier}"
            );
        }
    }
    for literal in &surface.literals {
        if let Ok(text) = std::str::from_utf8(literal) {
            assert!(
                !sealed_short_identities.contains(text),
                "{label}: embeds sealed short identity {text}"
            );
            for needle in sealed_needles {
                assert!(
                    !text.contains(needle),
                    "{label}: embeds sealed identity/path/hash {needle}"
                );
            }
        }
        for sealed_source in sealed_sources {
            assert!(
                sealed_source.is_empty()
                    || !literal
                        .windows(sealed_source.len())
                        .any(|window| window == sealed_source),
                "{label}: embeds a complete sealed grammar source"
            );
        }
    }
    for (name, tokens) in &surface.macros {
        let compact_tokens = compact_identifier(tokens);
        for forbidden in FORBIDDEN_DISPATCHERS {
            assert!(
                !compact_tokens.contains(forbidden),
                "{label}: macro `{name}` hides forbidden dispatcher `{forbidden}`"
            );
        }
        if matches!(name.as_str(), "cfg" | "cfg_attr") {
            assert!(
                !compact_tokens.contains("feature"),
                "{label}: macro `{name}` hides feature dispatch"
            );
        }

        let authority_tokens = compact_authority(tokens);
        for needle in sealed_needles {
            let needle = compact_authority(needle);
            assert!(
                needle.is_empty() || !authority_tokens.contains(&needle),
                "{label}: macro `{name}` embeds or constructs sealed authority `{needle}`"
            );
        }
        for sealed_source in sealed_sources {
            if let Ok(sealed_source) = std::str::from_utf8(sealed_source) {
                let sealed_source = compact_authority(sealed_source);
                assert!(
                    sealed_source.is_empty() || !authority_tokens.contains(&sealed_source),
                    "{label}: macro `{name}` embeds or constructs a complete sealed grammar"
                );
            }
        }
    }
}

fn compiler_library_external_ingress_findings_with_graph(
    source: &str,
    graph_macro_definitions: &BTreeSet<String>,
) -> Vec<String> {
    use syn::visit::Visit as _;

    fn forbidden_authority(segments: &[String]) -> bool {
        if segments
            .first()
            .map(|segment| normalized_rust_identifier(segment))
            != Some("std")
        {
            return false;
        }
        match segments
            .get(1)
            .map(|segment| normalized_rust_identifier(segment))
        {
            Some("io") => segments.get(2).map(String::as_str) != Some("Cursor"),
            Some("env" | "fs" | "net" | "path" | "process" | "thread" | "time") => true,
            Some("os") => segments.iter().skip(2).any(|segment| {
                matches!(
                    normalized_rust_identifier(segment),
                    "env" | "fs" | "io" | "net" | "process" | "thread" | "time"
                )
            }),
            _ => false,
        }
    }

    fn expression_path(expression: &syn::Expr) -> Option<Vec<String>> {
        match expression {
            syn::Expr::Path(path) => Some(
                path.path
                    .segments
                    .iter()
                    .map(|segment| {
                        normalized_rust_identifier(&segment.ident.to_string()).to_owned()
                    })
                    .collect(),
            ),
            syn::Expr::Reference(reference) => expression_path(&reference.expr),
            syn::Expr::Cast(cast) => expression_path(&cast.expr),
            syn::Expr::Group(group) => expression_path(&group.expr),
            syn::Expr::Paren(paren) => expression_path(&paren.expr),
            _ => None,
        }
    }

    fn pattern_binding(pattern: &syn::Pat) -> Option<String> {
        match pattern {
            syn::Pat::Ident(binding)
                if binding.by_ref.is_none()
                    && binding.mutability.is_none()
                    && binding.subpat.is_none() =>
            {
                Some(normalized_rust_identifier(&binding.ident.to_string()).to_owned())
            }
            syn::Pat::Type(typed) => pattern_binding(&typed.pat),
            syn::Pat::Paren(paren) => pattern_binding(&paren.pat),
            syn::Pat::Reference(reference) => pattern_binding(&reference.pat),
            _ => None,
        }
    }

    fn resolves_to_forbidden_authority(path: &[String], aliases: &[(String, Vec<String>)]) -> bool {
        fn visit(
            path: &[String],
            aliases: &[(String, Vec<String>)],
            seen: &mut BTreeSet<Vec<String>>,
        ) -> bool {
            if forbidden_authority(path) {
                return true;
            }
            if !seen.insert(path.to_vec()) {
                return false;
            }
            let Some(root) = path.first() else {
                return false;
            };
            aliases
                .iter()
                .filter(|(alias, _)| alias == root)
                .any(|(_, authority)| {
                    let mut expanded = authority.clone();
                    expanded.extend_from_slice(&path[1..]);
                    visit(&expanded, aliases, seen)
                })
        }

        visit(path, aliases, &mut BTreeSet::new())
    }

    fn macro_effect_findings(
        name: &str,
        source: &str,
        aliases: &[(String, Vec<String>)],
        pure_stringifiers: &BTreeSet<String>,
        macro_definitions: &BTreeSet<String>,
    ) -> Vec<String> {
        if macro_name_is_pure_stringifier(name, aliases, pure_stringifiers, macro_definitions) {
            return Vec::new();
        }

        #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
        struct MacroPathSegment {
            name: String,
            dynamic: bool,
        }

        fn identifier_token(token: &str) -> bool {
            token
                .bytes()
                .next()
                .is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_')
        }

        fn path_segment(tokens: &[String], at: usize) -> Option<(MacroPathSegment, usize)> {
            if tokens.get(at).map(String::as_str) == Some("$") {
                let name = tokens.get(at + 1)?;
                return identifier_token(name).then(|| {
                    (
                        MacroPathSegment {
                            name: name.clone(),
                            dynamic: true,
                        },
                        at + 2,
                    )
                });
            }
            let name = tokens.get(at)?;
            identifier_token(name).then(|| {
                (
                    MacroPathSegment {
                        name: name.clone(),
                        dynamic: false,
                    },
                    at + 1,
                )
            })
        }

        fn std_path_may_reach_forbidden(path: &[MacroPathSegment]) -> bool {
            let Some(module) = path.get(1) else {
                return false;
            };
            if module.dynamic {
                return true;
            }
            match module.name.as_str() {
                "io" => !matches!(
                    path.get(2),
                    Some(MacroPathSegment {
                        name,
                        dynamic: false,
                    }) if name == "Cursor"
                ),
                "env" | "fs" | "net" | "path" | "process" | "thread" | "time" => true,
                "os" => path.iter().skip(2).any(|segment| {
                    segment.dynamic
                        || matches!(
                            segment.name.as_str(),
                            "env" | "fs" | "io" | "net" | "process" | "thread" | "time"
                        )
                }),
                _ => false,
            }
        }

        fn path_may_reach_forbidden(
            path: &[MacroPathSegment],
            aliases: &[(String, Vec<String>)],
            seen: &mut BTreeSet<Vec<MacroPathSegment>>,
        ) -> bool {
            let Some(root) = path.first() else {
                return false;
            };
            if !seen.insert(path.to_vec()) {
                return false;
            }
            if root.dynamic {
                let mut expanded = vec![MacroPathSegment {
                    name: "std".to_owned(),
                    dynamic: false,
                }];
                expanded.extend_from_slice(&path[1..]);
                return std_path_may_reach_forbidden(&expanded);
            }
            if root.name == "std" {
                return std_path_may_reach_forbidden(path);
            }
            aliases
                .iter()
                .filter(|(alias, _)| alias == &root.name)
                .any(|(_, authority)| {
                    let mut expanded = authority
                        .iter()
                        .map(|segment| MacroPathSegment {
                            name: segment.clone(),
                            dynamic: false,
                        })
                        .collect::<Vec<_>>();
                    expanded.extend_from_slice(&path[1..]);
                    path_may_reach_forbidden(&expanded, aliases, seen)
                })
        }

        fn display_path(path: &[MacroPathSegment]) -> String {
            path.iter()
                .map(|segment| {
                    if segment.dynamic {
                        format!("${}", segment.name)
                    } else {
                        segment.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join("::")
        }

        let tokens = rust_macro_tokens(source);
        let mut findings = Vec::new();
        if macro_name_resolves(name, aliases, external_effect_macro) {
            findings.push(format!("external-ingress macro `{name}`"));
        }
        if macro_hides_module_ingress(name, source, aliases, pure_stringifiers, macro_definitions) {
            findings.push(format!(
                "macro `{name}` hides a module declaration or path redirection"
            ));
        }
        for (start, end) in macro_effect_token_ranges(name, &tokens) {
            let executable = executable_macro_tokens(
                &tokens[start..end],
                aliases,
                pure_stringifiers,
                macro_definitions,
            );
            if executable.iter().enumerate().any(|(index, token)| {
                token == "use"
                    && index
                        .checked_sub(1)
                        .and_then(|previous| executable.get(previous))
                        .map(String::as_str)
                        != Some("$")
            }) {
                findings.push(format!(
                    "macro `{name}` contains a transcriber-local `use` declaration whose \
                     alias provenance is not structural"
                ));
            }
            if macro_tokens_have_dynamic_effect_composition(&executable) {
                findings.push(format!(
                    "macro `{name}` dynamically composes an external path or macro invocation"
                ));
            }
            for (index, token) in executable.iter().enumerate() {
                if let Some((macro_path, _)) = macro_invocation_at(&executable, index)
                    && macro_path_resolves(&macro_path, aliases, external_effect_macro)
                {
                    findings.push(format!(
                        "macro `{name}` invokes external macro `{macro_path}!`"
                    ));
                }
                if token == "extern"
                    && matches!(
                        executable.get(index + 1).map(String::as_str),
                        Some("fn" | "{" | "crate")
                    )
                {
                    findings.push(format!(
                        "macro `{name}` hides a foreign-function or extern-crate ingress"
                    ));
                }

                if index != 0 && executable[index - 1] == "$" {
                    continue;
                }
                let Some((first, mut cursor)) = path_segment(&executable, index) else {
                    continue;
                };
                let mut path = vec![first];
                while executable.get(cursor).map(String::as_str) == Some("::") {
                    let Some((segment, next)) = path_segment(&executable, cursor + 1) else {
                        break;
                    };
                    path.push(segment);
                    cursor = next;
                }
                if path.len() > 1 && path_may_reach_forbidden(&path, aliases, &mut BTreeSet::new())
                {
                    findings.push(format!(
                        "macro `{name}` contains external authority path `{}`",
                        display_path(&path)
                    ));
                }
            }
        }
        findings
    }

    #[derive(Default)]
    struct Ingress {
        paths: Vec<Vec<String>>,
        aliases: Vec<(String, Vec<String>)>,
        macros: Vec<(String, String)>,
        foreign_modules: usize,
        non_rust_abis: Vec<String>,
        link_attributes: Vec<String>,
        module_path_attributes: Vec<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Ingress {
        fn visit_item_macro(&mut self, item: &'ast syn::ItemMacro) {
            self.macros.push(catalogued_item_macro(item));
            for attribute in &item.attrs {
                self.visit_attribute(attribute);
            }
        }

        fn visit_path(&mut self, path: &'ast syn::Path) {
            self.paths.push(
                path.segments
                    .iter()
                    .map(|segment| {
                        normalized_rust_identifier(&segment.ident.to_string()).to_owned()
                    })
                    .collect(),
            );
            syn::visit::visit_path(self, path);
        }

        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            collect_use_alias_paths(
                &item.tree,
                &mut Vec::new(),
                &mut self.paths,
                &mut self.aliases,
            );
            syn::visit::visit_item_use(self, item);
        }

        fn visit_item_extern_crate(&mut self, item: &'ast syn::ItemExternCrate) {
            let authority = normalized_rust_identifier(&item.ident.to_string()).to_owned();
            let alias = item
                .rename
                .as_ref()
                .map(|(_, alias)| normalized_rust_identifier(&alias.to_string()).to_owned())
                .unwrap_or_else(|| authority.clone());
            self.aliases.push((alias, vec![authority]));
            syn::visit::visit_item_extern_crate(self, item);
        }

        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(binding) = pattern_binding(&local.pat)
                && let Some(initializer) = &local.init
                && let Some(authority) = expression_path(&initializer.expr)
            {
                self.aliases.push((binding, authority));
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
            if let Some(authority) = expression_path(&item.expr) {
                self.aliases.push((
                    normalized_rust_identifier(&item.ident.to_string()).to_owned(),
                    authority,
                ));
            }
            syn::visit::visit_item_const(self, item);
        }

        fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
            if let Some(authority) = expression_path(&item.expr) {
                self.aliases.push((
                    normalized_rust_identifier(&item.ident.to_string()).to_owned(),
                    authority,
                ));
            }
            syn::visit::visit_item_static(self, item);
        }

        fn visit_macro(&mut self, item: &'ast syn::Macro) {
            self.macros
                .push((normalized_syn_path(&item.path), item.tokens.to_string()));
            syn::visit::visit_macro(self, item);
        }

        fn visit_item_foreign_mod(&mut self, item: &'ast syn::ItemForeignMod) {
            self.foreign_modules += 1;
            syn::visit::visit_item_foreign_mod(self, item);
        }

        fn visit_abi(&mut self, abi: &'ast syn::Abi) {
            let name = abi
                .name
                .as_ref()
                .map(syn::LitStr::value)
                .unwrap_or_else(|| "C".to_owned());
            if name != "Rust" {
                self.non_rust_abis.push(name);
            }
            syn::visit::visit_abi(self, abi);
        }

        fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
            if attribute_selects_module_path(attribute) {
                self.module_path_attributes.push(
                    attribute
                        .path()
                        .segments
                        .iter()
                        .map(|segment| segment.ident.to_string())
                        .collect::<Vec<_>>()
                        .join("::"),
                );
            }
            if matches!(
                attribute.path().segments.last().map(|segment| &segment.ident),
                Some(name) if matches!(
                    name.to_string().as_str(),
                    "link" | "link_name" | "link_ordinal"
                )
            ) {
                self.link_attributes
                    .push(attribute.path().segments.last().unwrap().ident.to_string());
            }
            if attribute.path().is_ident("cfg_attr")
                && attribute.meta.require_list().is_ok_and(|list| {
                    let tokens = list.tokens.to_string().to_ascii_lowercase();
                    tokens.contains("link") || tokens.contains("asm") || tokens.contains("extern")
                })
            {
                self.link_attributes
                    .push("cfg_attr(native-ingress)".to_owned());
            }
            syn::visit::visit_attribute(self, attribute);
        }
    }

    let syntax = syn::parse_file(source).expect("compiler-library ingress fixture is valid Rust");
    let mut ingress = Ingress::default();
    ingress.visit_file(&syntax);
    let local_macro_definitions = macro_definition_names(&ingress.macros);
    let mut macro_definitions = graph_macro_definitions.clone();
    macro_definitions.extend(local_macro_definitions);
    let pure_stringifiers =
        pure_stringifier_names(&ingress.macros, &ingress.aliases, &macro_definitions);
    let mut findings = Vec::new();
    findings.extend(
        ingress
            .paths
            .iter()
            .filter(|path| resolves_to_forbidden_authority(path, &ingress.aliases))
            .map(|path| format!("external authority `{}`", path.join("::"))),
    );
    for (name, tokens) in &ingress.macros {
        findings.extend(macro_effect_findings(
            name,
            tokens,
            &ingress.aliases,
            &pure_stringifiers,
            &macro_definitions,
        ));
    }
    if ingress.foreign_modules != 0 {
        findings.push("foreign-function ingress".to_owned());
    }
    findings.extend(
        ingress
            .non_rust_abis
            .iter()
            .map(|abi| format!("non-Rust ABI `{abi}`")),
    );
    findings.extend(
        ingress
            .link_attributes
            .iter()
            .map(|attribute| format!("native link attribute `{attribute}`")),
    );
    findings.extend(
        ingress
            .module_path_attributes
            .iter()
            .map(|attribute| format!("module path redirection `{attribute}`")),
    );
    findings.sort();
    findings.dedup();
    findings
}

fn compiler_library_external_ingress_findings(source: &str) -> Vec<String> {
    compiler_library_external_ingress_findings_with_graph(source, &BTreeSet::new())
}

fn assert_compiler_library_has_no_external_ingress(label: &str, source: &str) {
    let findings = compiler_library_external_ingress_findings(source);
    assert!(
        findings.is_empty(),
        "{label}: compiler library crosses an external-ingress boundary: {findings:?}"
    );
}

fn assert_generated_runtime_has_no_host_ingress(label: &str, source: &str) {
    use syn::visit::Visit as _;

    fn use_roots(tree: &syn::UseTree, roots: &mut Vec<String>) {
        match tree {
            syn::UseTree::Path(path) => roots.push(path.ident.to_string()),
            syn::UseTree::Name(name) => roots.push(name.ident.to_string()),
            syn::UseTree::Rename(rename) => roots.push(rename.ident.to_string()),
            syn::UseTree::Glob(_) => roots.push("*".to_owned()),
            syn::UseTree::Group(group) => {
                for item in &group.items {
                    use_roots(item, roots);
                }
            }
        }
    }

    #[derive(Default)]
    struct GeneratedImports {
        roots: Vec<String>,
        extern_crates: Vec<String>,
        std_paths: Vec<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for GeneratedImports {
        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            use_roots(&item.tree, &mut self.roots);
            syn::visit::visit_item_use(self, item);
        }

        fn visit_item_extern_crate(&mut self, item: &'ast syn::ItemExternCrate) {
            self.extern_crates.push(item.ident.to_string());
            syn::visit::visit_item_extern_crate(self, item);
        }

        fn visit_path(&mut self, path: &'ast syn::Path) {
            if path
                .segments
                .first()
                .is_some_and(|segment| segment.ident == "std")
            {
                self.std_paths.push(
                    path.segments
                        .iter()
                        .map(|segment| segment.ident.to_string())
                        .collect::<Vec<_>>()
                        .join("::"),
                );
            }
            syn::visit::visit_path(self, path);
        }
    }

    assert_compiler_library_has_no_external_ingress(label, source);
    let syntax =
        syn::parse_file(source).unwrap_or_else(|error| panic!("{label}: generated Rust: {error}"));
    let mut imports = GeneratedImports::default();
    imports.visit_file(&syntax);
    for root in imports.roots {
        assert!(
            matches!(root.as_str(), "core" | "parse_that"),
            "{label}: generated runtime imports non-runtime authority `{root}`"
        );
    }
    assert!(
        imports.extern_crates.is_empty(),
        "{label}: generated runtime declares extern crates {:?}",
        imports.extern_crates
    );
    assert!(
        imports.std_paths.is_empty(),
        "{label}: generated runtime names forbidden std paths {:?}",
        imports.std_paths
    );
}

fn assert_rust_fixture_compiles(label: &str, source: &str) {
    let temporary = tempfile::tempdir().expect("Rust ingress compile fixture");
    let source_path = temporary.path().join("fixture.rs");
    let metadata_path = temporary.path().join("fixture.rmeta");
    fs::write(&source_path, source).expect("write Rust ingress compile fixture");
    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let output = Command::new(rustc)
        .args([
            "--crate-type",
            "lib",
            "--edition",
            "2024",
            "--emit",
            "metadata",
        ])
        .arg("-o")
        .arg(&metadata_path)
        .arg(&source_path)
        .output()
        .unwrap_or_else(|error| panic!("{label}: spawn rustc: {error}"));
    assert!(
        output.status.success(),
        "{label}: adversarial ingress fixture must be valid compiling Rust\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn assert_rust_module_fixture_compiles(label: &str, files: &[(&str, &str)]) {
    let temporary = tempfile::tempdir().expect("Rust module-graph ingress compile fixture");
    for (relative, source) in files {
        let path = temporary.path().join(relative);
        fs::create_dir_all(path.parent().expect("module fixture parent"))
            .expect("create module fixture parent");
        fs::write(&path, source)
            .unwrap_or_else(|error| panic!("{label}: write {}: {error}", path.display()));
    }
    let source_path = temporary.path().join("lib.rs");
    assert!(source_path.is_file(), "{label}: fixture has a lib.rs entry");
    let metadata_path = temporary.path().join("fixture.rmeta");
    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let output = Command::new(rustc)
        .args([
            "--crate-type",
            "lib",
            "--edition",
            "2024",
            "--emit",
            "metadata",
        ])
        .arg("-o")
        .arg(&metadata_path)
        .arg(&source_path)
        .output()
        .unwrap_or_else(|error| panic!("{label}: spawn rustc: {error}"));
    assert!(
        output.status.success(),
        "{label}: adversarial module-graph fixture must be valid compiling Rust\nstdout:\n{}\n\
         stderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn compiler_library_ingress_policy_self_falsifies_pure_and_effectful_surfaces() {
    const DYNAMIC_MACRO_INGRESS: [(&str, &str, &str); 3] = [
        (
            "dynamic std module segment",
            r#"
macro_rules! hidden {
    ($segment:ident) => { std::$segment::read("authority") };
}
pub fn ingress() { let _ = hidden!(fs); }
"#,
            "std::$segment::read",
        ),
        (
            "dynamic root and module segments",
            r#"
macro_rules! hidden {
    ($root:ident, $segment:ident) => { $root::$segment::read("authority") };
}
pub fn ingress() { let _ = hidden!(std, fs); }
"#,
            "$root::$segment::read",
        ),
        (
            "dynamic root module and operation segments",
            r#"
macro_rules! hidden {
    ($root:ident, $segment:ident, $operation:ident) => {
        $root::$segment::$operation("authority")
    };
}
pub fn ingress() { let _ = hidden!(std, fs, read); }
"#,
            "$root::$segment::$operation",
        ),
    ];

    for (label, source) in [
        (
            "pure logical-path text and in-memory Cursor",
            r#"
use std::io::Cursor;
fn inspect(bytes: &[u8], logical: &str) {
    let cursor = Cursor::new(bytes);
    let _ = cursor.position();
    let _ = logical.split('/').all(|component| component != "..");
}
"#,
        ),
        (
            "pure OS string view and lookalike methods",
            r#"
#[cfg(unix)]
fn inspect(name: &std::ffi::OsStr) {
    use std::os::unix::ffi::OsStrExt as _;
    let _ = name.as_bytes();
}
struct Pure;
impl Pure {
    fn open(&self) {}
    fn status(&self) {}
    fn exists(&self) {}
}
fn lookalikes(value: &Pure) {
    value.open();
    value.status();
    value.exists();
}
type PureCallback = fn(&[u8]);
"#,
        ),
        (
            "pure macro vocabulary is not host authority",
            r#"
macro_rules! retain_names {
    ($time:ident, $thread:ident) => {
        stringify!($time, $thread)
    };
}
fn inspect(time: usize, thread: &str) {
    assert_eq!(time, 0);
    let _ = format!("pure io fs process time thread {thread}");
    let _ = retain_names!(time, thread);
}
"#,
        ),
        (
            "pure dynamic in-memory Cursor path",
            r#"
macro_rules! in_memory {
    ($root:ident) => { $root::io::Cursor::new(&[] as &[u8]) };
}
fn inspect() { let _ = in_memory!(std); }
"#,
        ),
        (
            "pure forbidden-looking path tokens are not executed",
            r#"
fn inspect() {
    let _ = stringify!(std::fs::read);
}
"#,
        ),
        (
            "aliased and local-wrapper stringify do not execute their payload",
            r#"
use std::stringify as retain;
macro_rules! retain_path_tokens {
    ($($tokens:tt)*) => { stringify!($($tokens)*) };
}
fn inspect() {
    let _ = retain!(std::fs::read);
    let _ = retain_path_tokens!(std::fs::read);
}
"#,
        ),
    ] {
        assert_rust_fixture_compiles(label, source);
        assert!(
            compiler_library_external_ingress_findings(source).is_empty(),
            "{label} must remain admitted"
        );
    }

    for (label, source, expected) in [
        (
            "root self alias preserves standard-library provenance",
            r#"
use std::{self as host};
pub fn ingress() { let _ = host::fs::read("/etc/hosts"); }
"#,
            "external authority `host::fs::read`",
        ),
        (
            "macro transcriber local use alias",
            r#"
macro_rules! hidden {
    () => {{
        use std as host;
        host::fs::read("/etc/hosts")
    }};
}
pub fn ingress() { let _ = hidden!(); }
"#,
            "transcriber-local `use` declaration",
        ),
    ] {
        assert_rust_fixture_compiles(label, source);
        let findings = compiler_library_external_ingress_findings(source);
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "{label}: compiling aliased host ingress must retain exact provenance: {findings:?}"
        );
    }

    let graph_root = r#"
#[macro_use]
mod macro_authority;
mod consumer;
pub use consumer::ingress;
"#;
    let graph_macro_authority = r#"
macro_rules! stringify {
    ($($tokens:tt)*) => { $($tokens)* };
}
"#;
    let graph_consumer = r#"
pub fn ingress() {
    let _ = stringify!({
        mod hidden {}
        std::fs::read("/etc/hosts")
    });
}
"#;
    assert_rust_module_fixture_compiles(
        "active-graph shadowed stringify",
        &[
            ("lib.rs", graph_root),
            ("macro_authority.rs", graph_macro_authority),
            ("consumer.rs", graph_consumer),
        ],
    );
    assert!(
        compiler_library_external_ingress_findings(graph_consumer).is_empty(),
        "without active-graph provenance, bare stringify has the exact builtin interpretation"
    );
    let graph_macro_definitions = [graph_root, graph_macro_authority, graph_consumer]
        .into_iter()
        .flat_map(rust_source_macro_definition_names)
        .collect::<BTreeSet<_>>();
    assert_eq!(
        graph_macro_definitions,
        BTreeSet::from(["stringify".to_owned()]),
        "the active module graph discovers the shadowing macro definition exactly once"
    );
    let graph_findings = compiler_library_external_ingress_findings_with_graph(
        graph_consumer,
        &graph_macro_definitions,
    );
    assert!(
        graph_findings
            .iter()
            .any(|finding| finding.contains("external authority path `std::fs::read`")),
        "an active-graph macro named stringify cannot inherit builtin purity: {graph_findings:?}"
    );
    assert!(
        graph_findings
            .iter()
            .any(|finding| finding.contains("hides a module declaration")),
        "an active-graph macro named stringify cannot hide module expansion: {graph_findings:?}"
    );

    for (label, source, expected_path) in DYNAMIC_MACRO_INGRESS {
        assert_rust_fixture_compiles(label, source);
        let findings = compiler_library_external_ingress_findings(source);
        assert!(
            findings.iter().any(|finding| {
                finding.contains(&format!("external authority path `{expected_path}`"))
            }),
            "{label}: the exact dynamic authority path must be rejected: {findings:?}"
        );
    }
    for (label, source) in [
        (
            "repetition-spliced dynamic path",
            r#"
macro_rules! hidden {
    ($($segment:ident),+) => { $($segment)::+("Cargo.toml") };
}
pub fn ingress() { let _ = hidden!(std, fs, read); }
"#,
        ),
        (
            "nested repetition-spliced dynamic path",
            r#"
macro_rules! hidden {
    ($( $root:ident => ($($segment:ident),+) ),+) => {
        $(let _ = $root::$($segment)::+("Cargo.toml");)+
    };
}
pub fn ingress() { hidden!(std => (fs, read)); }
"#,
        ),
        (
            "repetition body emits one path separator per segment",
            r#"
macro_rules! hidden {
    ($($segment:ident)+) => { $($segment::)+ read("Cargo.toml") };
}
pub fn ingress() { let _ = hidden!(std fs); }
"#,
        ),
        (
            "dynamic external macro name",
            r#"
macro_rules! hidden {
    ($macro_name:ident) => { $macro_name!("PATH") };
}
pub fn ingress() { let _ = hidden!(option_env); }
"#,
        ),
    ] {
        assert_rust_fixture_compiles(label, source);
        let findings = compiler_library_external_ingress_findings(source);
        assert!(
            findings
                .iter()
                .any(|finding| finding.contains("dynamically composes")),
            "{label}: dynamic macro composition must be rejected: {findings:?}"
        );
    }

    for (label, source) in [
        (
            "aliased include_bytes macro",
            r#"
use std::include_bytes as leak;
pub const AUTHORITY: &[u8] = leak!("/etc/hosts");
"#,
        ),
        (
            "aliased include_str macro",
            r#"
use std::include_str as leak;
pub const AUTHORITY: &str = leak!("/etc/hosts");
"#,
        ),
        (
            "effectful macro cannot impersonate built-in stringify",
            r#"
use std::include_bytes as stringify;
pub const AUTHORITY: &[u8] = stringify!("/etc/hosts");
"#,
        ),
        (
            "stringifier wrapper cannot inherit an effectful stringify alias",
            r#"
use std::include_bytes as stringify;
macro_rules! retain_path_tokens {
    ($path:literal) => { stringify!($path) };
}
pub const AUTHORITY: &[u8] = retain_path_tokens!("/etc/hosts");
"#,
        ),
        (
            "aliased include module ingress",
            r#"
use std::include as inject;
inject!("/dev/null");
"#,
        ),
        (
            "aliased option_env macro",
            r#"
use std::option_env as inspect_host;
pub const AUTHORITY: Option<&str> = inspect_host!("PATH");
"#,
        ),
        (
            "raw include_bytes macro identifier",
            r#"pub const AUTHORITY: &[u8] = r#include_bytes!("/etc/hosts");"#,
        ),
        (
            "raw standard-library root identifier",
            r#"pub fn ingress() { let _ = r#std::fs::read("/etc/hosts"); }"#,
        ),
        (
            "Path method filesystem ingress",
            r#"
use std::path::Path;
pub fn ingress() -> bool { Path::new("/etc/hosts").exists() }
"#,
        ),
        (
            "PathBuf method filesystem ingress",
            r#"
use std::path::PathBuf;
pub fn ingress() { let _ = PathBuf::from("/tmp").read_dir(); }
"#,
        ),
    ] {
        assert_rust_fixture_compiles(label, source);
        let findings = compiler_library_external_ingress_findings(source);
        assert!(
            !findings.is_empty(),
            "{label}: compiling host ingress must be rejected"
        );
    }

    for (label, source) in [
        (
            "chained local and function-pointer filesystem aliases",
            r#"
fn ingress() {
    let first: fn(_) -> _ = std::fs::read;
    let second = first;
    let third = second;
    let _ = third("authority");
}
"#,
        ),
        (
            "aliased environment read",
            r#"use std::env as surroundings; fn ingress() { let _ = surroundings::var("X"); }"#,
        ),
        (
            "aliased process",
            r#"use std::process::Command as Child; fn ingress() { let _ = Child::new("tool").status(); }"#,
        ),
        (
            "aliased network",
            r#"use std::net::TcpStream as Wire; fn ingress() { let _ = Wire::connect("127.0.0.1:1"); }"#,
        ),
        (
            "aliased standard input",
            r#"use std::io::stdin as input; fn ingress() { let _ = input(); }"#,
        ),
        (
            "wall clock",
            r#"fn ingress() { let _ = std::time::SystemTime::now(); }"#,
        ),
        (
            "host parallelism",
            r#"fn ingress() { let _ = std::thread::available_parallelism(); }"#,
        ),
        (
            "OS process extension",
            r#"use std::os::unix::process::CommandExt as _;"#,
        ),
        (
            "split macro authority",
            r#"
macro_rules! hidden {
    ($root:path) => { $root::fs::read("authority") };
}
fn ingress() { let _ = hidden!(std); }
"#,
        ),
        (
            "split macro standard input authority",
            r#"
macro_rules! hidden {
    ($root:path) => { $root::io::stdin() };
}
fn ingress() { let _ = hidden!(std); }
"#,
        ),
        (
            "hidden include",
            r#"macro_rules! hidden { () => { include_bytes!("authority.bin") }; }"#,
        ),
        (
            "direct module path redirection",
            r#"#[path = "authority.rs"] mod hidden;"#,
        ),
        (
            "conditional module path redirection",
            r#"#[cfg_attr(unix, path = "authority.rs")] mod hidden;"#,
        ),
        (
            "macro-hidden module path redirection",
            r#"macro_rules! hidden { () => { #[path = "authority.rs"] mod host; }; }"#,
        ),
        (
            "extern crate alias",
            r#"extern crate std as host; type File = host::fs::File;"#,
        ),
        (
            "bare foreign callback",
            r#"type Callback = unsafe extern "C" fn(*const u8) -> usize;"#,
        ),
        (
            "foreign function",
            r#"unsafe extern "C" { fn ingress() -> i32; }"#,
        ),
    ] {
        assert!(
            !compiler_library_external_ingress_findings(source).is_empty(),
            "{label} must be rejected by the ingress policy"
        );
    }

    let chained_alias_findings = compiler_library_external_ingress_findings(
        r#"
fn ingress() {
    let first: fn(_) -> _ = std::fs::read;
    let second = first;
    let third = second;
    let _ = third("authority");
}
"#,
    );
    assert!(
        chained_alias_findings
            .iter()
            .any(|finding| finding == "external authority `third`"),
        "typed function-pointer provenance must survive every local alias hop: \
         {chained_alias_findings:?}"
    );
}

/// sk-v27 LW3.COMP-1b-T (RULINGS 2026-11-12d COMP1B-W3 (a)): the git+rev
/// edges the workspace takes into the sibling repositories. Every edge names a
/// pushed commit (no sibling path, no `[patch]`); a re-pin edits the rev, the
/// lockfile and the sealed graph identities below in one commit.
const SK_V27_PARSE_THAT_GIT: &str = "https://github.com/mkbabb/parse-that";
const SK_V27_PPRINT_GIT: &str = "https://github.com/mkbabb/pprint";
/// The exact width of the locked metadata package graph (every platform).
const SK_V27_METADATA_PACKAGE_COUNT: usize = 511;
/// sha256 over the sorted `bbnf` target inventory (`name:kind`, one per line).
/// Re-taken at LW3.COMP-5a: the test target `synthetic_grammar_strategy`
/// (the deleted ident-keyed table's tests) left, the sole difference.
const SK_V27_BBNF_TARGETS_SHA256: &str =
    "2390e4e652d49ab7359256b9ca22602a5e797ced809967c61a515fd7c0ac8a7a";
/// sha256 over the sorted `bbnf` metadata dependency rows.
const SK_V27_BBNF_DEPENDENCY_ROWS_SHA256: &str =
    "c1db28fb8c440b855d8afc374e27dd148b1e43c56ab88e6996c27e5466909199";
/// sha256 over the complete resolved package/feature/target identity rows.
/// Re-taken at LW3.STALE-0: LW3.PERF-1c deleted the `bbnf-ir` test target
/// `kernel_shape` (`crates/ir/tests/kernel_shape.rs`), the sole row difference.
/// Re-taken at LW3.COMP-5a: `bbnf-ir`'s `build-script-build` (the deleted
/// strategy-table build script) and `bbnf`'s `synthetic_grammar_strategy`
/// test target left, the sole row differences.
const SK_V27_METADATA_IDENTITY_SHA256: &str =
    "2572498fda8bcfb753e56d23c15d80109cb7aad18b10a8d2ba11a3bdb5f43e10";
/// sha256 of the complete Cargo.lock. Re-taken at LW3.COMP-5a: `bbnf-ir`
/// lost its `toml 0.8.23` build dependency with the strategy-table build
/// script, the sole lockfile line that moved.
const SK_V27_LOCK_SHA256: &str = "da02047eb5a065fa614f57cd0c2c6121f6b22b6b0a8f601d08f503b703136c4a";

fn toml_keys(value: &toml::Value) -> BTreeSet<String> {
    value
        .as_table()
        .expect("TOML table")
        .keys()
        .cloned()
        .collect::<BTreeSet<_>>()
}

fn string_set(names: &[&str]) -> BTreeSet<String> {
    names.iter().map(|name| (*name).to_owned()).collect()
}

/// The owned packages the workspace resolves through its git+rev edges, read
/// from `cargo metadata` (name -> (version, manifest path, source)).
fn sk_v27_owned_git_packages(metadata: &Value) -> BTreeMap<String, (String, PathBuf, String)> {
    metadata["packages"]
        .as_array()
        .expect("metadata packages")
        .iter()
        .filter(|package| {
            package["source"].as_str().is_some_and(|source| {
                source.starts_with(&format!("git+{SK_V27_PARSE_THAT_GIT}?rev="))
                    || source.starts_with(&format!("git+{SK_V27_PPRINT_GIT}?rev="))
            })
        })
        .map(|package| {
            (
                package["name"]
                    .as_str()
                    .expect("owned package name")
                    .to_owned(),
                (
                    package["version"]
                        .as_str()
                        .expect("owned package version")
                        .to_owned(),
                    PathBuf::from(
                        package["manifest_path"]
                            .as_str()
                            .expect("owned package manifest path"),
                    ),
                    package["source"]
                        .as_str()
                        .expect("owned package source")
                        .to_owned(),
                ),
            )
        })
        .collect()
}

fn assert_git_rev_edge(label: &str, dependency: &toml::Value, git: &str, version: &str) -> String {
    assert_eq!(
        toml_keys(dependency),
        string_set(&["git", "rev", "version"]),
        "{label}: exact git+rev edge shape (no path, branch, tag or feature override)"
    );
    assert_eq!(dependency["git"].as_str(), Some(git), "{label}: git URL");
    assert_eq!(
        dependency["version"].as_str(),
        Some(version),
        "{label}: version"
    );
    let rev = dependency["rev"].as_str().expect("edge rev").to_owned();
    assert!(
        rev.len() == 40
            && rev
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase()),
        "{label}: rev is one full lowercase commit id, got {rev:?}"
    );
    rev
}

#[test]
fn cargo_graph_build_feature_surfaces_and_generated_products_are_closed() {
    // LW3.COMP-1b-T: the sk-v26 form of this law sealed a one-member
    // workspace with `../parse-that-skv26` sibling edges. LW3-ORDER COMP-1b
    // prescribes the sk-v27 layout instead: the workspace, `.cargo` and the
    // toolchain take master's side, `crates/core/Cargo.toml` is the union, and
    // the sibling edges are git+rev pins. This law seals that layout exactly.
    let root_manifest_path = root().join("Cargo.toml");
    let core_manifest_path = root().join("crates/core/Cargo.toml");
    let cargo_config_path = root().join(".cargo/config.toml");
    let lock_path = root().join("Cargo.lock");
    let toolchain_path = root().join("rust-toolchain.toml");
    for (label, path) in [
        ("workspace manifest", &root_manifest_path),
        ("BBNF manifest", &core_manifest_path),
        ("Cargo configuration", &cargo_config_path),
        ("lockfile", &lock_path),
        ("toolchain pin", &toolchain_path),
    ] {
        let metadata =
            fs::symlink_metadata(path).unwrap_or_else(|error| panic!("{label}: {error}"));
        assert!(
            metadata.is_file() && !metadata.file_type().is_symlink(),
            "{label} must be one repository-owned regular file"
        );
    }
    let cargo_directory_metadata =
        fs::symlink_metadata(root().join(".cargo")).expect("root .cargo metadata");
    assert!(
        cargo_directory_metadata.is_dir() && !cargo_directory_metadata.file_type().is_symlink(),
        "root .cargo must be one repository-owned directory"
    );

    let root_manifest: toml::Value =
        toml::from_str(&fs::read_to_string(&root_manifest_path).unwrap()).expect("root Cargo.toml");
    let core_manifest: toml::Value =
        toml::from_str(&fs::read_to_string(&core_manifest_path).unwrap()).expect("core Cargo.toml");
    let cargo_config: toml::Value =
        toml::from_str(&fs::read_to_string(&cargo_config_path).unwrap())
            .expect("root Cargo config");
    let toolchain: toml::Value =
        toml::from_str(&fs::read_to_string(&toolchain_path).unwrap()).expect("rust-toolchain.toml");
    let keys = toml_keys;

    // ── The toolchain: the stable pin (TC-1) ──────────────────────────────
    assert_eq!(keys(&toolchain), string_set(&["toolchain"]));
    assert_eq!(
        keys(&toolchain["toolchain"]),
        string_set(&["channel", "components", "profile"])
    );
    assert_eq!(
        toolchain["toolchain"]["channel"].as_str(),
        Some("1.99.1"),
        "TC-1: the stable pin stands; the spine's nightly is not taken"
    );
    assert_eq!(toolchain["toolchain"]["profile"].as_str(), Some("minimal"));
    assert_eq!(
        toolchain["toolchain"]["components"].as_array(),
        Some(&vec![
            toml::Value::String("rustfmt".to_owned()),
            toml::Value::String("clippy".to_owned()),
        ])
    );

    // ── The workspace manifest: master's side ─────────────────────────────
    assert_eq!(keys(&root_manifest), string_set(&["profile", "workspace"]));
    assert_eq!(
        keys(&root_manifest["workspace"]),
        string_set(&["dependencies", "exclude", "members", "metadata", "resolver"])
    );
    let expected_members = [
        "crates/core",
        "crates/analysis",
        "crates/ir",
        "crates/lsp",
        "crates/ser",
        "crates/gorgeous",
        "crates/bootstrap",
        "crates/bbnf-path",
        "crates/bbnf-path-ts",
        "crates/corpus",
        "crates/oracle",
        "xtask",
    ];
    assert_eq!(
        root_manifest["workspace"]["members"].as_array(),
        Some(
            &expected_members
                .iter()
                .map(|member| toml::Value::String((*member).to_owned()))
                .collect::<Vec<_>>()
        ),
        "workspace member set and order"
    );
    assert_eq!(
        root_manifest["workspace"]["exclude"].as_array(),
        Some(&vec![toml::Value::String("wasm".to_owned())]),
        "workspace exclusions"
    );
    assert_eq!(
        root_manifest["workspace"]["resolver"].as_str(),
        Some("2"),
        "workspace resolver"
    );
    assert_eq!(
        keys(&root_manifest["workspace"]["metadata"]),
        string_set(&["bbnf"])
    );
    assert_eq!(
        keys(&root_manifest["workspace"]["metadata"]["bbnf"]),
        string_set(&["grammars"])
    );
    let regen_grammars = root_manifest["workspace"]["metadata"]["bbnf"]["grammars"]
        .as_array()
        .expect("regen grammar table");
    assert_eq!(
        regen_grammars
            .iter()
            .map(|row| row["ident"].as_str().expect("regen ident"))
            .collect::<Vec<_>>(),
        [
            "bbnf",
            "json",
            "css_l4",
            "css_pretty",
            "google_sheets",
            "ebnf",
            "bnf",
            "csv",
            "math"
        ],
        "the nine regenerated grammars, in order"
    );
    for row in regen_grammars {
        // sk-v27 LW3.COMP-5a (RULINGS 2026-11-13c COMP5A-S): the entry
        // supplies the grammar's runtime binding (`builder`, `document`).
        assert_eq!(
            keys(row),
            string_set(&["builder", "document", "features", "ident", "path"])
        );
        let path = row["path"].as_str().expect("regen grammar path");
        assert!(
            root().join(path).is_file(),
            "{path}: every regenerated grammar is a tracked source"
        );
    }
    assert_eq!(
        keys(&root_manifest["profile"]),
        string_set(&["bench", "dev", "profiling-prep", "release"])
    );
    assert_eq!(
        keys(&root_manifest["profile"]["release"]),
        string_set(&["codegen-units", "debug", "lto", "split-debuginfo", "strip"])
    );
    assert_eq!(
        root_manifest["profile"]["release"]["lto"].as_str(),
        Some("thin")
    );
    assert_eq!(
        root_manifest["profile"]["release"]["codegen-units"].as_integer(),
        Some(1)
    );
    assert_eq!(
        root_manifest["profile"]["release"]["debug"].as_bool(),
        Some(true)
    );
    assert_eq!(
        root_manifest["profile"]["release"]["strip"].as_bool(),
        Some(false)
    );
    assert_eq!(
        root_manifest["profile"]["release"]["split-debuginfo"].as_str(),
        Some("packed")
    );
    assert_eq!(
        keys(&root_manifest["profile"]["bench"]),
        string_set(&["codegen-units", "debug", "lto"])
    );
    assert_eq!(
        root_manifest["profile"]["bench"]["lto"].as_str(),
        Some("fat")
    );
    assert_eq!(
        root_manifest["profile"]["bench"]["codegen-units"].as_integer(),
        Some(1)
    );
    assert_eq!(
        root_manifest["profile"]["bench"]["debug"].as_bool(),
        Some(true)
    );
    assert_eq!(
        keys(&root_manifest["profile"]["profiling-prep"]),
        string_set(&["debug", "inherits", "split-debuginfo", "strip"])
    );
    assert_eq!(
        root_manifest["profile"]["profiling-prep"]["inherits"].as_str(),
        Some("release")
    );
    assert_eq!(
        keys(&root_manifest["profile"]["dev"]),
        string_set(&["codegen-units", "incremental", "package"])
    );
    assert_eq!(
        keys(&root_manifest["profile"]["dev"]["package"]),
        string_set(&["bbnf-ir", "csp-solver", "parse_that"])
    );
    for package in ["bbnf-ir", "csp-solver", "parse_that"] {
        assert_eq!(
            keys(&root_manifest["profile"]["dev"]["package"][package]),
            string_set(&["opt-level"])
        );
        assert_eq!(
            root_manifest["profile"]["dev"]["package"][package]["opt-level"].as_integer(),
            Some(1)
        );
    }

    // The sibling edges: git+rev pins, one parse-that commit at a time.
    let workspace_dependencies = &root_manifest["workspace"]["dependencies"];
    assert_eq!(
        keys(workspace_dependencies),
        string_set(&[
            "csp-solver",
            "parse-that-egraph",
            "parse-that-egraph-derive",
            "parse-that-regex",
            "parse_that",
            "pprint",
        ]),
        "workspace dependency surface is closed"
    );
    let mut parse_that_revs = BTreeSet::new();
    for (name, version) in [
        ("parse_that", "0.4"),
        ("parse-that-regex", "0.1.0"),
        ("parse-that-egraph", "0.1.0"),
        ("parse-that-egraph-derive", "0.1.0"),
    ] {
        parse_that_revs.insert(assert_git_rev_edge(
            name,
            &workspace_dependencies[name],
            SK_V27_PARSE_THAT_GIT,
            version,
        ));
    }
    assert_eq!(
        parse_that_revs.len(),
        1,
        "every parse-that edge pins one commit (LW3-ORDER §4: one PT SHA at a time): {parse_that_revs:?}"
    );
    let parse_that_rev = parse_that_revs.into_iter().next().unwrap();
    let pprint_rev = assert_git_rev_edge(
        "pprint",
        &workspace_dependencies["pprint"],
        SK_V27_PPRINT_GIT,
        "0.5.0-alpha.0",
    );
    assert_eq!(
        workspace_dependencies["csp-solver"].as_str(),
        Some("0.4.0"),
        "csp-solver: the one published home"
    );
    // No sibling path edge and no `[patch]` table: the top-level and workspace
    // key sets above are exact, and every owned edge's shape is exactly
    // {git, rev, version}.
    assert!(
        root_manifest.get("patch").is_none()
            && workspace_dependencies
                .as_table()
                .unwrap()
                .values()
                .all(|dependency| dependency.get("path").is_none()),
        "the workspace carries no sibling path edge and no [patch] table"
    );

    // ── The BBNF manifest: the union ──────────────────────────────────────
    assert_eq!(
        keys(&core_manifest),
        string_set(&[
            "bench",
            "bin",
            "dependencies",
            "dev-dependencies",
            "features",
            "lib",
            "package",
            "target",
            "test",
        ])
    );
    assert_eq!(
        keys(&core_manifest["package"]),
        string_set(&[
            "description",
            "edition",
            "license",
            "name",
            "repository",
            "version",
        ]),
        "BBNF package surface is closed (master's autotests, autobins and autobenches: defaults)"
    );
    for (key, value) in [
        ("name", "bbnf"),
        ("version", "0.3.0"),
        ("edition", "2024"),
        ("license", "MIT"),
        (
            "description",
            "BBNF grammar parser and code generator built on parse_that",
        ),
        ("repository", "https://github.com/mkbabb/bbnf-lang"),
    ] {
        assert_eq!(
            core_manifest["package"][key].as_str(),
            Some(value),
            "BBNF package.{key}"
        );
    }
    assert_eq!(
        env!("CARGO_PKG_VERSION"),
        "0.3.0",
        "compiled BBNF package identity"
    );
    assert!(
        core_manifest["package"].get("build").is_none()
            && core_manifest["package"].get("links").is_none(),
        "BBNF has no build-script or native-link surface"
    );
    assert_eq!(keys(&core_manifest["lib"]), string_set(&["name", "path"]));
    assert_eq!(core_manifest["lib"]["name"].as_str(), Some("bbnf"));
    assert_eq!(core_manifest["lib"]["path"].as_str(), Some("src/lib.rs"));
    let bins = core_manifest["bin"].as_array().expect("BBNF explicit bins");
    assert_eq!(bins.len(), 1, "BBNF has exactly one explicit binary target");
    assert_eq!(keys(&bins[0]), string_set(&["name", "path"]));
    assert_eq!(bins[0]["name"].as_str(), Some("bbnf-gen"));
    assert_eq!(bins[0]["path"].as_str(), Some("src/bin/bbnf-gen.rs"));
    let expected_tests = [
        ("frontend_contract", "tests/frontend_contract.rs"),
        ("dependency_scc_laws", "tests/dependency_scc_laws.rs"),
        ("type_analysis_laws", "tests/type_analysis_laws.rs"),
        ("emitter_contract", "tests/emitter_contract.rs"),
        ("replay_edit_laws", "tests/replay_edit_laws.rs"),
        ("record_semantic_laws", "tests/record_semantic_laws.rs"),
        ("carrier_lookup_laws", "tests/carrier_lookup_laws.rs"),
        ("identity_opacity", "tests/identity_opacity.rs"),
        (
            "w3_instance_frontend_contract",
            "tests/w3_instance_frontend_contract.rs",
        ),
        (
            "w3_semantic_pratt_contract",
            "tests/w3_semantic_pratt_contract.rs",
        ),
        (
            "w3_replay_public_contract",
            "tests/w3_replay_public_contract.rs",
        ),
    ];
    let tests = core_manifest["test"]
        .as_array()
        .expect("BBNF explicit test targets");
    assert_eq!(
        tests.len(),
        expected_tests.len(),
        "the spine's 11 explicit test targets"
    );
    for (test, (name, path)) in tests.iter().zip(expected_tests) {
        assert_eq!(keys(test), string_set(&["name", "path"]));
        assert_eq!(test["name"].as_str(), Some(name));
        assert_eq!(test["path"].as_str(), Some(path));
    }
    let features = core_manifest["features"].as_table().expect("BBNF features");
    let feature_rows = features
        .iter()
        .map(|(name, members)| {
            format!(
                "{name}={}",
                members
                    .as_array()
                    .expect("feature members")
                    .iter()
                    .map(|member| member.as_str().expect("feature member"))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        feature_rows,
        string_set(&[
            "callgrind=iai",
            "competitor=",
            "dhat-heap=dep:dhat",
            "iai=",
            "stress=",
            "vm=",
            "wasm-bench=",
        ]),
        "BBNF feature surface is master's bench-tier set, exactly"
    );
    let benches = core_manifest["bench"].as_array().expect("BBNF benches");
    assert_eq!(
        benches
            .iter()
            .map(|bench| bench["name"].as_str().expect("bench name"))
            .collect::<Vec<_>>(),
        [
            "json_monolithic",
            "css_l4",
            "google_sheets_monolithic",
            "bbnf_monolithic",
            "compile_pipeline",
            "path_ascent",
            "json_competitors",
            "css_competitors",
            "json_parse_that",
            "json_value",
            "json_stress",
            "css_stress",
            "json_vm",
            "css_vm",
            "google_sheets_vm",
            "json_callgrind",
            "json_ts",
            "json_wasm",
            "css_ts",
            "css_wasm",
        ],
        "BBNF bench table and order"
    );
    for bench in benches {
        assert_eq!(bench["harness"].as_bool(), Some(false));
    }
    assert_eq!(
        keys(&core_manifest["target"]),
        string_set(&["cfg(target_os = \"linux\")"])
    );
    assert_eq!(
        core_manifest["target"]["cfg(target_os = \"linux\")"]["dev-dependencies"]["iai-callgrind"]
            .as_str(),
        Some("0.12")
    );
    assert_eq!(
        keys(&core_manifest["dependencies"]),
        string_set(&[
            "bbnf-ir",
            "bbnf-ser",
            "blake3",
            "dhat",
            "indexmap",
            "parse-that-egraph",
            "parse-that-regex",
            "parse_that",
            "pprint",
            "proc-macro2",
            "quote",
            "rustc-hash",
            "serde",
            "serde_json",
            "syn",
        ])
    );
    for name in [
        "parse_that",
        "parse-that-regex",
        "parse-that-egraph",
        "pprint",
    ] {
        let dependency = &core_manifest["dependencies"][name];
        assert_eq!(
            keys(dependency),
            string_set(&["workspace"]),
            "{name}: core takes the sibling edge from the workspace, exactly"
        );
        assert_eq!(dependency["workspace"].as_bool(), Some(true));
    }
    for (name, version, path) in [("bbnf-ir", "0.2.0", "../ir"), ("bbnf-ser", "0.1", "../ser")] {
        let dependency = &core_manifest["dependencies"][name];
        assert_eq!(keys(dependency), string_set(&["path", "version"]));
        assert_eq!(dependency["version"].as_str(), Some(version));
        assert_eq!(dependency["path"].as_str(), Some(path));
    }
    for (name, version, features) in [
        ("blake3", "1.8", &["std"][..]),
        ("serde", "1", &["derive", "std"][..]),
        ("serde_json", "1", &["std"][..]),
    ] {
        let dependency = &core_manifest["dependencies"][name];
        assert_eq!(
            keys(dependency),
            string_set(&["default-features", "features", "version"]),
            "{name}: the spine's explicit registry dependency shape"
        );
        assert_eq!(dependency["version"].as_str(), Some(version));
        assert_eq!(dependency["default-features"].as_bool(), Some(false));
        assert_eq!(
            dependency["features"]
                .as_array()
                .expect("dependency feature array")
                .iter()
                .map(|feature| feature.as_str().expect("feature string"))
                .collect::<Vec<_>>(),
            features,
            "{name}: exact explicit feature identity"
        );
    }
    assert_eq!(
        keys(&core_manifest["dev-dependencies"]),
        string_set(&[
            "bbnf-corpus",
            "bbnf-path",
            "cssparser",
            "divan",
            "fast-float2",
            "jiter",
            "jsonschema",
            "lightningcss",
            "mimalloc",
            "nom",
            "parse_that",
            "pest",
            "pest_grammars",
            "pprint",
            "prettyplease",
            "quote",
            "regex",
            "serde",
            "serde_json",
            "serde_json_borrow",
            "sha2",
            "simd-json",
            "sonic-rs",
            "syn",
            "tempfile",
            "toml",
            "tree-sitter",
            "tree-sitter-json",
            "trybuild",
            "wasmtime",
            "winnow",
        ])
    );
    assert_eq!(
        core_manifest["dev-dependencies"]["jsonschema"]["version"].as_str(),
        Some("0.49.2")
    );
    assert_eq!(
        core_manifest["dev-dependencies"]["jsonschema"]["default-features"].as_bool(),
        Some(false)
    );
    assert_eq!(
        core_manifest["dev-dependencies"]["syn"]["features"]
            .as_array()
            .expect("syn features"),
        &vec![
            toml::Value::String("full".to_owned()),
            toml::Value::String("visit".to_owned()),
        ]
    );
    for (name, version) in [
        ("sha2", "0.10"),
        ("tempfile", "3"),
        ("toml", "1.1.4"),
        ("trybuild", "1"),
    ] {
        assert_eq!(
            core_manifest["dev-dependencies"][name].as_str(),
            Some(version),
            "{name}: exact BBNF dev dependency"
        );
    }
    // The sk-v26 sibling edges are gone (X-5): the dependency name sets above
    // are exact (no `bbnf-regex`), and every path edge stays in this repository.
    let repository_canonical = root().canonicalize().expect("repository canonical path");
    for table in ["dependencies", "dev-dependencies"] {
        for (name, dependency) in core_manifest[table].as_table().unwrap() {
            if let Some(path) = dependency.get("path").and_then(toml::Value::as_str) {
                let resolved = root()
                    .join("crates/core")
                    .join(path)
                    .canonicalize()
                    .unwrap_or_else(|error| panic!("{name}: path edge {path}: {error}"));
                assert!(
                    resolved.starts_with(&repository_canonical),
                    "{name}: a path edge stays inside this repository, got {path}"
                );
            }
        }
    }

    // ── The Cargo configuration: master's side ────────────────────────────
    assert_eq!(
        keys(&cargo_config),
        string_set(&["alias", "profile", "target"])
    );
    assert!(
        cargo_config.get("build").is_none(),
        "no build override (rustc-wrapper, rustflags) in the root configuration"
    );
    assert_eq!(
        keys(&cargo_config["profile"]),
        string_set(&["ax-iter", "ay-final", "bench-ci", "bench-iter"])
    );
    assert_eq!(
        cargo_config["profile"]["ax-iter"]["inherits"].as_str(),
        Some("dev")
    );
    assert_eq!(
        keys(&cargo_config["target"]),
        string_set(&["x86_64-unknown-linux-gnu"])
    );
    let aliases = cargo_config["alias"].as_table().expect("root aliases");
    assert_eq!(aliases.len(), 41, "root command aliases are closed");
    for (alias, command) in [
        ("xtask", "run -p xtask --profile ax-iter --"),
        (
            "iter-clippy",
            "clippy --profile ax-iter --workspace --all-targets -- -D warnings",
        ),
        (
            "iter-test",
            "nextest run --cargo-profile ax-iter --workspace",
        ),
        ("iter-check-full", "check --profile ax-iter --workspace"),
    ] {
        assert_eq!(
            cargo_config["alias"][alias].as_str(),
            Some(command),
            "{alias}: exact root command alias"
        );
    }
    let cargo_configuration_entries: BTreeSet<String> = fs::read_dir(root().join(".cargo"))
        .expect("root .cargo directory")
        .map(|entry| {
            entry
                .expect("root .cargo entry")
                .file_name()
                .into_string()
                .expect("root .cargo entry is UTF-8")
        })
        .collect();
    assert_eq!(
        cargo_configuration_entries,
        string_set(&["config.toml"]),
        "root has exactly one Cargo configuration authority"
    );
    assert!(
        !root().join(".cargo/config").exists()
            && !root().join("config.toml").exists()
            && !root().join(".cargo.toml").exists(),
        "alternate Cargo configuration authorities are forbidden"
    );
    assert!(
        !root().join("build.rs").exists() && !root().join("crates/core/build.rs").exists(),
        "active BBNF package has no build script input"
    );
    assert!(
        root().join("xtask/Cargo.toml").is_file(),
        "xtask is restored whole (regen --check, corpus fetch/verify)"
    );

    // ── The locked metadata graph ─────────────────────────────────────────
    let metadata_output = Command::new(env!("CARGO"))
        .args(["metadata", "--locked", "--offline", "--format-version", "1"])
        .current_dir(root())
        .output()
        .expect("cargo metadata");
    assert!(
        metadata_output.status.success(),
        "cargo metadata failed: {}",
        String::from_utf8_lossy(&metadata_output.stderr)
    );
    let metadata: Value =
        serde_json::from_slice(&metadata_output.stdout).expect("cargo metadata JSON");
    let packages = metadata["packages"].as_array().expect("metadata packages");
    assert_eq!(
        packages.len(),
        SK_V27_METADATA_PACKAGE_COUNT,
        "exact locked metadata package graph width"
    );

    // The owned packages behind the git+rev edges, at the pinned commits.
    let owned = sk_v27_owned_git_packages(&metadata);
    assert_eq!(
        owned.keys().cloned().collect::<BTreeSet<_>>(),
        string_set(&[
            "parse-that-egraph",
            "parse-that-egraph-derive",
            "parse-that-regex",
            "parse-that-simd",
            "parse_that",
            "pprint",
            "pprint_derive",
        ]),
        "the owned sibling packages the lock resolves"
    );
    for (name, (_, _, source)) in &owned {
        let (git, rev) = if name.starts_with("pprint") {
            (SK_V27_PPRINT_GIT, &pprint_rev)
        } else {
            (SK_V27_PARSE_THAT_GIT, &parse_that_rev)
        };
        assert_eq!(
            source,
            &format!("git+{git}?rev={rev}#{rev}"),
            "{name}: resolved from exactly the pinned commit"
        );
    }
    let parse_that_manifest_path = owned["parse_that"].1.clone();
    let regex_manifest_path = owned["parse-that-regex"].1.clone();
    for (label, path) in [
        ("owned parse_that manifest", &parse_that_manifest_path),
        ("owned parse-that-regex manifest", &regex_manifest_path),
    ] {
        let metadata =
            fs::symlink_metadata(path).unwrap_or_else(|error| panic!("{label}: {error}"));
        assert!(
            metadata.is_file() && !metadata.file_type().is_symlink(),
            "{label} must be one regular file of the pinned checkout"
        );
    }
    let parse_that_manifest: toml::Value =
        toml::from_str(&fs::read_to_string(&parse_that_manifest_path).unwrap())
            .expect("owned parse_that Cargo.toml");
    let regex_manifest: toml::Value =
        toml::from_str(&fs::read_to_string(&regex_manifest_path).unwrap())
            .expect("owned parse-that-regex Cargo.toml");
    assert_eq!(
        keys(&parse_that_manifest),
        string_set(&[
            "bench",
            "dependencies",
            "dev-dependencies",
            "features",
            "lib",
            "package"
        ]),
        "owned parse_that manifest surface"
    );
    assert_eq!(
        keys(&parse_that_manifest["package"]),
        string_set(&[
            "description",
            "edition",
            "license",
            "name",
            "repository",
            "version"
        ]),
        "owned parse_that package identity surface"
    );
    for (key, value) in [
        ("name", "parse_that"),
        ("version", "0.4.0"),
        ("edition", "2024"),
        ("license", "MIT"),
        (
            "description",
            "Zero-copy parser combinator library for Rust",
        ),
        ("repository", SK_V27_PARSE_THAT_GIT),
    ] {
        assert_eq!(
            parse_that_manifest["package"][key].as_str(),
            Some(value),
            "owned parse_that package.{key}"
        );
    }
    assert!(
        parse_that_manifest["package"].get("build").is_none()
            && parse_that_manifest["package"].get("links").is_none(),
        "owned parse_that has no build or native-link ingress"
    );
    assert_eq!(
        keys(&parse_that_manifest["lib"]),
        string_set(&["name", "path"])
    );
    assert_eq!(
        parse_that_manifest["lib"]["name"].as_str(),
        Some("parse_that")
    );
    assert_eq!(
        parse_that_manifest["lib"]["path"].as_str(),
        Some("src/lib.rs")
    );
    let parse_that_features = parse_that_manifest["features"]
        .as_table()
        .expect("parse_that features")
        .iter()
        .map(|(name, members)| {
            format!(
                "{name}={}",
                members
                    .as_array()
                    .expect("feature members")
                    .iter()
                    .map(|member| member.as_str().expect("feature member"))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        parse_that_features,
        string_set(&[
            "combinators=",
            "default=combinators",
            "diagnostics=dep:colored",
            "nightly=smallbox/nightly",
        ]),
        "owned parse_that feature surface; nightly is opt-in and off by default"
    );
    assert_eq!(
        keys(&parse_that_manifest["dependencies"]),
        string_set(&[
            "aho-corasick",
            "colored",
            "fast-float2",
            "memchr",
            "parse-that-regex",
            "parse-that-simd",
            "pprint",
            "smallbox",
            "smallvec",
        ]),
        "owned parse_that dependency surface"
    );
    let parse_that_pprint = &parse_that_manifest["dependencies"]["pprint"];
    assert_eq!(
        assert_git_rev_edge(
            "parse_that -> pprint",
            parse_that_pprint,
            SK_V27_PPRINT_GIT,
            "0.5.0-alpha.0"
        ),
        pprint_rev,
        "lock-step: parse_that at the pin and this workspace pin one pprint commit"
    );
    assert_eq!(
        keys(&regex_manifest),
        string_set(&[
            "bench",
            "dependencies",
            "dev-dependencies",
            "features",
            "lib",
            "package"
        ]),
        "owned parse-that-regex manifest surface"
    );
    assert_eq!(
        keys(&regex_manifest["package"]),
        string_set(&[
            "description",
            "edition",
            "license",
            "name",
            "repository",
            "version"
        ]),
        "owned parse-that-regex package identity surface"
    );
    for (key, value) in [
        ("name", "parse-that-regex"),
        ("version", "0.1.0"),
        ("edition", "2024"),
        ("license", "MIT"),
        (
            "description",
            "Regex engine for parser generators: HIR, NFA→DFA compilation, e-graph simplification, structural classification",
        ),
        ("repository", SK_V27_PARSE_THAT_GIT),
    ] {
        assert_eq!(
            regex_manifest["package"][key].as_str(),
            Some(value),
            "owned parse-that-regex package.{key}"
        );
    }
    assert!(
        regex_manifest["package"].get("build").is_none()
            && regex_manifest["package"].get("links").is_none(),
        "owned parse-that-regex has no build or native-link ingress"
    );
    assert_eq!(keys(&regex_manifest["lib"]), string_set(&["name", "path"]));
    assert_eq!(
        regex_manifest["lib"]["name"].as_str(),
        Some("parse_that_regex")
    );
    assert_eq!(regex_manifest["lib"]["path"].as_str(), Some("src/lib.rs"));
    assert_eq!(
        keys(&regex_manifest["features"]),
        string_set(&["default", "serde"])
    );
    assert_eq!(
        regex_manifest["features"]["default"]
            .as_array()
            .map(Vec::len),
        Some(0),
        "owned parse-that-regex default feature set is sealed empty"
    );
    assert_eq!(
        regex_manifest["features"]["serde"]
            .as_array()
            .expect("parse-that-regex serde feature"),
        &vec![toml::Value::String("dep:serde".to_owned())],
        "owned parse-that-regex serde feature only enables its optional dependency"
    );
    assert_eq!(
        keys(&regex_manifest["dependencies"]),
        string_set(&[
            "parse-that-egraph",
            "parse-that-egraph-derive",
            "proc-macro2",
            "regex-automata",
            "regex-syntax",
            "rustc-hash",
            "serde",
            "smallvec",
        ])
    );
    assert_eq!(
        regex_manifest["dependencies"]["smallvec"].as_str(),
        Some("1.13"),
        "owned parse-that-regex smallvec identity"
    );
    assert_eq!(
        regex_manifest["dependencies"]["regex-automata"]["version"].as_str(),
        Some("=0.4.14"),
        "OR-7 Route A: the exact determinizer pin"
    );
    assert_eq!(
        regex_manifest["dependencies"]["regex-syntax"]["version"].as_str(),
        Some("=0.8.10")
    );
    assert_eq!(
        regex_manifest["dependencies"]["serde"]["optional"].as_bool(),
        Some(true)
    );
    assert_eq!(
        keys(&regex_manifest["dev-dependencies"]),
        string_set(&["divan", "mimalloc", "serde_json"])
    );
    assert_eq!(
        regex_manifest["bench"]
            .as_array()
            .expect("owned parse-that-regex bench array")
            .iter()
            .map(|bench| bench["name"].as_str().expect("bench name"))
            .collect::<Vec<_>>(),
        [
            "regex_parse",
            "regex_classify",
            "regex_hir_egraph",
            "regex_dfa_compile"
        ],
        "owned parse-that-regex exact bench table"
    );

    // The one BBNF package.
    let package_matches: Vec<&Value> = packages
        .iter()
        .filter(|package| package["name"].as_str() == Some("bbnf"))
        .collect();
    assert_eq!(
        package_matches.len(),
        1,
        "one active BBNF workspace package"
    );
    let package = package_matches[0];
    assert_eq!(package["version"].as_str(), Some("0.3.0"));
    assert!(package["source"].is_null(), "BBNF is the workspace package");
    assert_eq!(
        package["features"]
            .as_object()
            .expect("resolved package features")
            .keys()
            .cloned()
            .collect::<BTreeSet<_>>(),
        string_set(&[
            "callgrind",
            "competitor",
            "dhat-heap",
            "iai",
            "stress",
            "vm",
            "wasm-bench"
        ]),
        "resolved package feature map is the manifest's, exactly"
    );
    let target_rows = package["targets"]
        .as_array()
        .expect("BBNF metadata targets")
        .iter()
        .map(|target| {
            format!(
                "{}:{}",
                target["name"].as_str().expect("target name"),
                target["kind"]
                    .as_array()
                    .expect("target kinds")
                    .iter()
                    .map(|kind| kind.as_str().expect("target kind"))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        })
        .collect::<BTreeSet<_>>();
    for required in ["bbnf:lib", "bbnf-gen:bin", "root_api_present:test"]
        .into_iter()
        .map(str::to_owned)
        .chain(
            expected_tests
                .iter()
                .map(|(name, _)| format!("{name}:test")),
        )
    {
        assert!(
            target_rows.contains(&required),
            "BBNF target inventory holds {required}"
        );
    }
    let target_bytes = target_rows
        .iter()
        .map(|row| format!("{row}\n"))
        .collect::<String>();
    assert_eq!(
        sha256_hex(target_bytes.as_bytes()),
        SK_V27_BBNF_TARGETS_SHA256,
        "BBNF metadata target inventory is exact ({} targets)",
        target_rows.len()
    );
    let dependency_rows: BTreeSet<String> = package["dependencies"]
        .as_array()
        .expect("metadata dependencies")
        .iter()
        .map(|dependency| {
            let mut features = dependency["features"]
                .as_array()
                .expect("dependency feature list")
                .iter()
                .map(|feature| feature.as_str().expect("feature string"))
                .collect::<Vec<_>>();
            features.sort_unstable();
            format!(
                "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                dependency["name"].as_str().expect("dependency name"),
                dependency["req"].as_str().expect("dependency requirement"),
                dependency["kind"].as_str().unwrap_or("normal"),
                dependency["uses_default_features"]
                    .as_bool()
                    .expect("default-feature flag"),
                features.join(","),
                dependency["optional"].as_bool().expect("optional flag"),
                dependency["target"].as_str().unwrap_or_default(),
                dependency["source"].as_str().unwrap_or("path"),
            )
        })
        .collect();
    for dependency in package["dependencies"]
        .as_array()
        .expect("metadata dependencies")
    {
        assert!(
            dependency["rename"].is_null(),
            "no dependency rename dispatch"
        );
        assert!(dependency["registry"].is_null());
        let name = dependency["name"].as_str().expect("dependency name");
        if owned.contains_key(name) {
            assert!(
                dependency["path"].is_null(),
                "{name}: an owned sibling is never a path edge"
            );
            let (git, rev) = if name.starts_with("pprint") {
                (SK_V27_PPRINT_GIT, &pprint_rev)
            } else {
                (SK_V27_PARSE_THAT_GIT, &parse_that_rev)
            };
            assert_eq!(
                dependency["source"].as_str(),
                Some(format!("git+{git}?rev={rev}").as_str()),
                "{name}: owned dependency source is the pinned git edge"
            );
        } else if let Some(path) = dependency["path"].as_str() {
            let canonical = PathBuf::from(path).canonicalize().expect("path dependency");
            assert!(
                canonical.starts_with(root().canonicalize().unwrap()),
                "{name}: a path dependency stays inside this repository"
            );
        } else {
            assert_eq!(
                dependency["source"].as_str(),
                Some("registry+https://github.com/rust-lang/crates.io-index"),
                "{name}: every other dependency comes from crates.io"
            );
        }
    }
    let dependency_bytes = dependency_rows
        .iter()
        .map(|row| format!("{row}\n"))
        .collect::<String>();
    assert_eq!(
        sha256_hex(dependency_bytes.as_bytes()),
        SK_V27_BBNF_DEPENDENCY_ROWS_SHA256,
        "BBNF metadata dependency identities and feature requests are exact ({} rows)",
        dependency_rows.len()
    );

    let metadata_by_id: BTreeMap<&str, &Value> = packages
        .iter()
        .map(|package| {
            (
                package["id"].as_str().expect("metadata package id"),
                package,
            )
        })
        .collect();
    let mut metadata_identity_rows = BTreeSet::new();
    for node in metadata["resolve"]["nodes"]
        .as_array()
        .expect("complete metadata resolve graph")
    {
        let package = metadata_by_id[node["id"].as_str().expect("resolve package id")];
        let mut features = node["features"]
            .as_array()
            .expect("resolved features")
            .iter()
            .map(|feature| feature.as_str().expect("resolved feature"))
            .collect::<Vec<_>>();
        features.sort_unstable();
        let mut targets = package["targets"]
            .as_array()
            .expect("metadata package targets")
            .iter()
            .map(|target| {
                format!(
                    "{}:{}:{}:{}:{}:{}:{}",
                    target["name"].as_str().expect("target name"),
                    target["kind"]
                        .as_array()
                        .expect("target kinds")
                        .iter()
                        .map(|kind| kind.as_str().expect("target kind"))
                        .collect::<Vec<_>>()
                        .join(","),
                    target["crate_types"]
                        .as_array()
                        .expect("target crate types")
                        .iter()
                        .map(|kind| kind.as_str().expect("crate type"))
                        .collect::<Vec<_>>()
                        .join(","),
                    target["edition"].as_str().expect("target edition"),
                    target["doc"].as_bool().expect("target doc flag"),
                    target["doctest"].as_bool().expect("target doctest flag"),
                    target["test"].as_bool().expect("target test flag"),
                )
            })
            .collect::<Vec<_>>();
        targets.sort();
        metadata_identity_rows.insert(format!(
            "{}\t{}\t{}\t{}\t{}",
            package["name"].as_str().expect("package name"),
            package["version"].as_str().expect("package version"),
            package["source"].as_str().unwrap_or_default(),
            features.join(","),
            targets.join("|"),
        ));
    }
    let metadata_identity_bytes = metadata_identity_rows
        .iter()
        .map(|row| format!("{row}\n"))
        .collect::<String>();
    assert_eq!(
        sha256_hex(metadata_identity_bytes.as_bytes()),
        SK_V27_METADATA_IDENTITY_SHA256,
        "complete resolved package/feature/target metadata identity"
    );

    // ── The lockfile ──────────────────────────────────────────────────────
    let lock_bytes = fs::read(&lock_path).expect("Cargo.lock bytes");
    assert_eq!(
        sha256_hex(&lock_bytes),
        SK_V27_LOCK_SHA256,
        "the complete locked dependency graph is an exact authority (a re-pin edits it here)"
    );
    let lock: toml::Value =
        toml::from_str(std::str::from_utf8(&lock_bytes).expect("Cargo.lock UTF-8"))
            .expect("Cargo.lock TOML");
    assert_eq!(
        keys(&lock),
        string_set(&["package", "version"]),
        "Cargo.lock top-level surface is exact"
    );
    assert_eq!(lock["version"].as_integer(), Some(4), "Cargo.lock format");
    let lock_packages = lock["package"].as_array().expect("lock packages");
    // The lock holds every optional dependency; the default-feature graph
    // above does not. The lock is read against the all-features graph, with
    // the lock's percent-encoded git sources decoded.
    let all_features_output = Command::new(env!("CARGO"))
        .args([
            "metadata",
            "--locked",
            "--offline",
            "--all-features",
            "--format-version",
            "1",
        ])
        .current_dir(root())
        .output()
        .expect("cargo metadata --all-features");
    assert!(
        all_features_output.status.success(),
        "cargo metadata --all-features failed: {}",
        String::from_utf8_lossy(&all_features_output.stderr)
    );
    let all_features: Value =
        serde_json::from_slice(&all_features_output.stdout).expect("all-features metadata JSON");
    let all_packages = all_features["packages"]
        .as_array()
        .expect("all-features metadata packages");
    assert_eq!(
        lock_packages.len(),
        all_packages.len(),
        "the all-features metadata and lock package graph widths agree"
    );
    let default_identities: BTreeSet<String> = packages
        .iter()
        .map(|package| {
            format!(
                "{}\t{}\t{}",
                package["name"].as_str().unwrap(),
                package["version"].as_str().unwrap(),
                package["source"].as_str().unwrap_or_default()
            )
        })
        .collect();
    let percent_decoded = |text: &str| {
        let bytes = text.as_bytes();
        let mut decoded = Vec::with_capacity(bytes.len());
        let mut index = 0;
        while index < bytes.len() {
            if bytes[index] == b'%' && index + 2 < bytes.len() {
                let high = hex_nibble(bytes[index + 1].to_ascii_lowercase());
                let low = hex_nibble(bytes[index + 2].to_ascii_lowercase());
                if let (Some(high), Some(low)) = (high, low) {
                    decoded.push(high * 16 + low);
                    index += 3;
                    continue;
                }
            }
            decoded.push(bytes[index]);
            index += 1;
        }
        String::from_utf8(decoded).expect("decoded lock source is UTF-8")
    };
    let lock_identity_rows: BTreeSet<String> = lock_packages
        .iter()
        .map(|package| {
            format!(
                "{}\t{}\t{}",
                package["name"].as_str().expect("locked package name"),
                package["version"].as_str().expect("locked package version"),
                percent_decoded(
                    package
                        .get("source")
                        .and_then(|value| value.as_str())
                        .unwrap_or_default()
                ),
            )
        })
        .collect();
    let metadata_lock_identity_rows: BTreeSet<String> = all_packages
        .iter()
        .map(|package| {
            format!(
                "{}\t{}\t{}",
                package["name"].as_str().expect("metadata package name"),
                package["version"]
                    .as_str()
                    .expect("metadata package version"),
                package
                    .get("source")
                    .and_then(|value| value.as_str())
                    .unwrap_or_default(),
            )
        })
        .collect();
    assert_eq!(
        lock_identity_rows.len(),
        lock_packages.len(),
        "Cargo.lock contains no duplicate package identity"
    );
    assert_eq!(
        metadata_lock_identity_rows.len(),
        all_packages.len(),
        "metadata contains no duplicate package identity"
    );
    assert_eq!(
        lock_identity_rows, metadata_lock_identity_rows,
        "Cargo.lock and the full all-features metadata package identities agree exactly"
    );
    assert!(
        default_identities.is_subset(&metadata_lock_identity_rows),
        "the default-feature graph is inside the locked graph"
    );
    let locked = |name: &str, version: &str| {
        let matches: Vec<&toml::Value> = lock_packages
            .iter()
            .filter(|package| {
                package["name"].as_str() == Some(name)
                    && package["version"].as_str() == Some(version)
            })
            .collect();
        assert_eq!(matches.len(), 1, "{name} {version}: exact lock identity");
        matches[0]
    };
    let bbnf_lock = locked("bbnf", "0.3.0");
    assert!(
        bbnf_lock.get("source").is_none() && bbnf_lock.get("checksum").is_none(),
        "bbnf: the workspace lock entry stays path-owned"
    );
    for (name, (version, _, source)) in &owned {
        let entry = locked(name, version);
        assert_eq!(
            entry.get("source").and_then(toml::Value::as_str),
            Some(source.as_str()),
            "{name}: lock entry names the pinned git source"
        );
        assert!(
            entry.get("checksum").is_none(),
            "{name}: a git entry carries no registry checksum"
        );
    }
    let lock_dependencies = |package: &toml::Value| {
        package
            .get("dependencies")
            .and_then(toml::Value::as_array)
            .map(|dependencies| {
                dependencies
                    .iter()
                    .map(|dependency| {
                        dependency
                            .as_str()
                            .expect("lock dependency is a string")
                            .split(' ')
                            .next()
                            .expect("lock dependency name")
                            .to_owned()
                    })
                    .collect::<BTreeSet<_>>()
            })
            .unwrap_or_default()
    };
    let bbnf_lock_dependencies = lock_dependencies(bbnf_lock);
    for name in core_manifest["dependencies"]
        .as_table()
        .unwrap()
        .keys()
        .chain(core_manifest["dev-dependencies"].as_table().unwrap().keys())
    {
        assert!(
            bbnf_lock_dependencies.contains(name),
            "bbnf lock dependency table holds {name}"
        );
    }
    assert_eq!(
        lock_dependencies(locked("parse_that", &owned["parse_that"].0)),
        string_set(&[
            "aho-corasick",
            "colored",
            "fast-float2",
            "memchr",
            "parse-that-regex",
            "parse-that-simd",
            "pprint",
            "smallbox",
            "smallvec",
        ]),
        "parse_that lock dependency table"
    );
    assert_eq!(
        lock_dependencies(locked("parse-that-regex", &owned["parse-that-regex"].0)),
        string_set(&[
            "parse-that-egraph",
            "parse-that-egraph-derive",
            "proc-macro2",
            "regex-automata",
            "regex-syntax",
            "rustc-hash",
            "serde",
            "smallvec",
        ]),
        "parse-that-regex lock dependency table"
    );

    // ── The spine's compiler graph: no feature dispatch, no ingress ───────
    struct CfgFeatureVisitor {
        hits: Vec<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for CfgFeatureVisitor {
        fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
            if attribute.path().is_ident("cfg") || attribute.path().is_ident("cfg_attr") {
                let tokens = attribute
                    .meta
                    .require_list()
                    .map(|list| list.tokens.to_string())
                    .unwrap_or_default();
                if tokens.contains("feature") {
                    self.hits.push(format!(
                        "attribute {}({tokens})",
                        attribute
                            .path()
                            .segments
                            .last()
                            .expect("cfg attribute path")
                            .ident
                    ));
                }
            }
            syn::visit::visit_attribute(self, attribute);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            if macro_.path.segments.last().is_some_and(|segment| {
                matches!(segment.ident.to_string().as_str(), "cfg" | "cfg_attr")
            }) {
                let tokens = macro_.tokens.to_string();
                if tokens.contains("feature") {
                    self.hits.push(format!(
                        "macro {}!({tokens})",
                        macro_.path.segments.last().expect("cfg macro path").ident
                    ));
                }
            }
            syn::visit::visit_macro(self, macro_);
        }
    }
    use syn::visit::Visit as _;
    let core_root = root().join("crates/core");
    // The crate root is hand-written at COMP-1b (the root modules plus
    // `compiler`); the law's compiler library is the spine's module graph,
    // rooted at `compiler/mod.rs` as the crate root names it.
    let library_entry = core_root.join(
        core_manifest["lib"]["path"]
            .as_str()
            .expect("BBNF library entry path"),
    );
    let library_root = fs::read_to_string(&library_entry).expect("BBNF crate root");
    assert!(
        syn::parse_file(&library_root)
            .expect("BBNF crate root syntax")
            .items
            .iter()
            .any(|item| matches!(item, syn::Item::Mod(module) if module.ident == "compiler" && module.content.is_none())),
        "the crate root declares the spine's `compiler` module"
    );
    let compiler_entry = core_root.join("src/compiler/mod.rs");
    let mut compiler_library_sources = BTreeMap::new();
    collect_active_rust_module_graph(&compiler_entry, &mut compiler_library_sources);
    assert!(
        compiler_library_sources.len() > 1
            && compiler_library_sources
                .keys()
                .all(|relative| relative.starts_with("crates/core/src/compiler/")),
        "the compiler graph is the spine's modules, exactly: {:?}",
        compiler_library_sources.keys().collect::<Vec<_>>()
    );
    let mut active_sources = compiler_library_sources.clone();
    for binary in bins {
        let binary_entry = core_root.join(binary["path"].as_str().expect("BBNF binary entry path"));
        collect_active_rust_module_graph(&binary_entry, &mut active_sources);
    }
    let mut visitor = CfgFeatureVisitor { hits: Vec::new() };
    for (relative, source) in &active_sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("{relative}: active Rust syntax: {error}"));
        visitor.visit_file(&syntax);
    }
    let compiler_graph_macro_definitions: BTreeSet<String> = compiler_library_sources
        .values()
        .flat_map(|source| rust_source_macro_definition_names(source))
        .collect();
    for (relative, source) in &compiler_library_sources {
        let findings = compiler_library_external_ingress_findings_with_graph(
            source,
            &compiler_graph_macro_definitions,
        );
        assert!(
            findings.is_empty(),
            "{relative}: compiler library crosses an external-ingress boundary: {findings:?}"
        );
    }
    assert!(
        visitor.hits.is_empty(),
        "active compiler/generator contains cfg(feature) surfaces: {:?}",
        visitor.hits
    );

    let addendum_header = [
        "witness_id",
        "grammar_path",
        "grammar_sha256",
        "instance_path",
        "instance_sha256",
        "semantic_plane",
        "feature_id",
        "case_ledger_path",
        "case_ledger_sha256",
        "recovery_policy",
        "disposition",
    ];
    let addendum = read_tsv(ADDENDUM, &addendum_header);
    let witness_ids: BTreeSet<String> = addendum.iter().map(|row| row[0].clone()).collect();
    let public_authority_paths: BTreeSet<String> = addendum
        .iter()
        .flat_map(|row| [row[1].clone(), row[3].clone(), row[7].clone()])
        .collect();
    let candidate_header = [
        "candidate_id",
        "ordinal",
        "grammar_path",
        "grammar_sha256",
        "instance_path",
        "instance_sha256",
        "case_ledger_path",
        "case_ledger_sha256",
        "case_classes",
        "recovery_policy",
        "execution_state",
        "disposition",
    ];
    let candidate_rows = read_tsv(CANDIDATE_PRESEAL, &candidate_header);
    let mut sealed_short_identities: BTreeSet<String> =
        candidate_rows.iter().map(|row| row[0].clone()).collect();
    sealed_short_identities.extend(
        SYNTHETIC_CONTROLS
            .iter()
            .map(|control| control.id.to_owned()),
    );
    let mut sealed_identity_needles: BTreeSet<String> = witness_ids
        .iter()
        .cloned()
        .chain(public_authority_paths.iter().cloned())
        .collect();
    for row in &addendum {
        sealed_identity_needles.extend([row[2].clone(), row[4].clone(), row[8].clone()]);
    }
    for row in &candidate_rows {
        sealed_identity_needles.extend([
            format!("CANDIDATE-{}-PRESEAL-V1", row[1]),
            row[2].clone(),
            row[3].clone(),
            row[4].clone(),
            row[5].clone(),
            row[6].clone(),
            row[7].clone(),
        ]);
    }
    for control in SYNTHETIC_CONTROLS {
        let manifest = synthetic_manifest(control);
        sealed_identity_needles.extend([
            control.logical_path.to_owned(),
            sha256_hex(&manifest),
            sha256_hex(control.source),
        ]);
    }
    let sealed_source_paths: BTreeSet<String> = addendum
        .iter()
        .map(|row| row[1].clone())
        .chain(candidate_rows.iter().map(|row| row[2].clone()))
        .collect();
    let mut sealed_sources: Vec<Vec<u8>> = sealed_source_paths
        .iter()
        .map(|relative| bytes(relative))
        .collect();
    sealed_sources.extend(
        SYNTHETIC_CONTROLS
            .iter()
            .map(|control| control.source.to_vec()),
    );

    for (relative, source) in &active_sources {
        assert_structural_no_overfit(
            relative,
            source,
            &sealed_identity_needles,
            &sealed_short_identities,
            &sealed_sources,
        );
    }

    for ordinal in PUBLIC_ORDINALS {
        let compiled = compile_public(ordinal);
        let (_, source_bytes, manifest) = public_instance(ordinal);
        let generated = format!(
            "{}\n{}",
            compiled.generated().source(),
            compiled.generated().manifest()
        );
        let lower = generated.to_lowercase();
        assert_structural_no_overfit(
            &format!("public/{ordinal} generated Rust"),
            compiled.generated().source(),
            &sealed_identity_needles,
            &sealed_short_identities,
            &sealed_sources,
        );
        assert_generated_runtime_has_no_host_ingress(
            &format!("public/{ordinal} generated Rust"),
            compiled.generated().source(),
        );

        for authority_path in &sealed_identity_needles {
            assert!(
                !generated.contains(authority_path),
                "public/{ordinal}: generated product embeds sealed identity {authority_path}"
            );
        }
        for forbidden_surface in [
            "pub const plugins:",
            "struct pluginregistry",
            "enum formatdispatch",
            "fn dispatch_format",
            "hostcallback",
            "host_callback",
            "include_bytes!(",
            "include_str!(",
            "std::fs::read",
            "std::fs::read_to_string",
            "compile_instance(",
            "compile_module(",
            "grammarinstanceinput",
            "parse_source(",
            "extern \"c\"",
            "unsafe {",
        ] {
            assert!(
                !lower.contains(forbidden_surface),
                "public/{ordinal}: forbidden generated surface {forbidden_surface}"
            );
        }
        for import in compiled
            .generated()
            .source()
            .lines()
            .map(str::trim)
            .filter(|line| line.starts_with("use "))
        {
            assert!(
                import.starts_with("use core::") || import.starts_with("use parse_that::"),
                "public/{ordinal}: generated product imports a non-runtime host surface: {import}"
            );
        }
        let logical_path = manifest["logical_path"].as_str().unwrap();
        assert!(
            !generated.contains(logical_path),
            "public/{ordinal}: generated product embeds logical path"
        );
        if let Ok(grammar_text) = std::str::from_utf8(&source_bytes) {
            assert!(
                !generated.contains(grammar_text),
                "public/{ordinal}: generated product embeds source bytes"
            );
        }
    }
    for control in SYNTHETIC_CONTROLS {
        let compiled = compile_synthetic(control);
        let generated = format!(
            "{}\n{}",
            compiled.generated().source(),
            compiled.generated().manifest()
        );
        assert_structural_no_overfit(
            &format!("{} generated Rust", control.id),
            compiled.generated().source(),
            &sealed_identity_needles,
            &sealed_short_identities,
            &sealed_sources,
        );
        assert_generated_runtime_has_no_host_ingress(
            &format!("{} generated Rust", control.id),
            compiled.generated().source(),
        );
        for identity in &sealed_identity_needles {
            assert!(
                !generated.contains(identity),
                "{}: generated product embeds sealed identity {identity}",
                control.id
            );
        }
        assert!(
            !generated.contains(control.logical_path),
            "{}: generated product embeds logical path",
            control.id
        );
        if let Ok(grammar_text) = std::str::from_utf8(control.source) {
            assert!(
                !generated.contains(grammar_text),
                "{}: generated product embeds source bytes",
                control.id
            );
        }
    }
}

fn hex_nibble(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        _ => None,
    }
}

fn sha256_hex(input: &[u8]) -> String {
    format!("{:x}", Sha256::digest(input))
}
