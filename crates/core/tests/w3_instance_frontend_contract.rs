use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use bbnf::compiler::{NodeId, NodeKind, ProjectionKeep};
use bbnf::{
    CompileOptions, CompiledModule, DiagnosticCode, GrammarInstanceInput, StageResult,
    compile_instance,
};
use serde_json::{Map, Value, json};

const CONST_EMPTY_INSTANCE: GrammarInstanceInput<'static> = GrammarInstanceInput::new(b"", b"");

const SIMPLE_SOURCE: &[u8] = br#"
first = "first";
root = "legacy-root";
document = "chosen";
alternate = "alternate";
hyphen-entry = "hyphenated";
"#;

const SIMPLE_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/simple.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": []
}"#;

const PRATT_SOURCE: &[u8] = br#"
document = expression ?w;
alternate = expression ?w;
expression = atom;
expression-hyphen = atom-hyphen;
atom = number | group;
atom-hyphen = number;
group = "(" ?w, expression, ")" ?w;
number = /(?:0|[1-9][0-9]*)/ ?w;
prefix_minus = "-" ?w;
postfix_bang = "!" ?w;
sum = "+" ?w;
operator-hyphen = "~" ?w;
"#;

const PRATT_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/revision-vector.bbnf",
  "entry": "document",
  "trivia_regex": "[ \t\r\n]*",
  "pratt_tables": [
    {
      "site_rule": "expression",
      "atom_rule": "atom",
      "operators": [
        {
          "matcher_rule": "prefix_minus",
          "fixity": "prefix",
          "left_binding_power": null,
          "right_binding_power": 70
        },
        {
          "matcher_rule": "postfix_bang",
          "fixity": "postfix",
          "left_binding_power": 90,
          "right_binding_power": null
        },
        {
          "matcher_rule": "sum",
          "fixity": "infix",
          "left_binding_power": 40,
          "right_binding_power": 41
        }
      ]
    }
  ]
}"#;

const PRATT_MANIFEST_REFORMATTED: &[u8] = br#"{"pratt_tables":[{"operators":[{"right_binding_power":70,"left_binding_power":null,"fixity":"prefix","matcher_rule":"prefix_minus"},{"right_binding_power":null,"left_binding_power":90,"fixity":"postfix","matcher_rule":"postfix_bang"},{"right_binding_power":41,"left_binding_power":40,"fixity":"infix","matcher_rule":"sum"}],"atom_rule":"atom","site_rule":"expression"}],"trivia_regex":"[ \t\r\n]*","entry":"document","logical_path":"contracts/w3/revision-vector.bbnf","schema":"bbnf.grammar-instance.v1"}"#;

const PRETTY_SOURCE: &[u8] = br#"
document = item ?w, { "," ?w, item ?w };
item = "x";
@pretty document group block indent dedent softline hardline blankline compact sep(", ");
"#;

const PRETTY_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/presentation.bbnf",
  "entry": "document",
  "trivia_regex": "[ \t]*",
  "pratt_tables": []
}"#;

fn compile(
    manifest_bytes: &[u8],
    source_bytes: &[u8],
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    compile_instance(
        GrammarInstanceInput::new(manifest_bytes, source_bytes),
        options,
    )
}

fn accepted_with(
    manifest_bytes: &[u8],
    source_bytes: &[u8],
    options: CompileOptions,
) -> CompiledModule {
    match compile(manifest_bytes, source_bytes, options) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(
                diagnostics.is_empty(),
                "accepted W3 fixture carried diagnostics:\n{}",
                diagnostics
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("\n")
            );
            value
        }
        StageResult::Rejected { diagnostics } => panic!(
            "expected W3 instance acceptance:\n{}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        ),
    }
}

fn accepted(manifest_bytes: &[u8], source_bytes: &[u8]) -> CompiledModule {
    accepted_with(manifest_bytes, source_bytes, CompileOptions::default())
}

fn assert_rejected(manifest_bytes: &[u8], source_bytes: &[u8]) {
    let StageResult::Rejected { diagnostics } =
        compile(manifest_bytes, source_bytes, CompileOptions::default())
    else {
        panic!("expected W3 instance rejection");
    };
    assert!(
        !diagnostics.is_empty(),
        "rejected W3 instance must carry a diagnostic"
    );
}

fn assert_rejected_with(
    manifest_bytes: &[u8],
    source_bytes: &[u8],
    options: CompileOptions,
    expected: DiagnosticCode,
) {
    let StageResult::Rejected { diagnostics } = compile(manifest_bytes, source_bytes, options)
    else {
        panic!("expected W3 instance rejection with {expected:?}");
    };
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code() == expected),
        "expected {expected:?}, got:\n{}",
        diagnostics
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n")
    );
}

fn assert_default_rejected_with(
    manifest_bytes: &[u8],
    source_bytes: &[u8],
    expected: DiagnosticCode,
) {
    assert_rejected_with(
        manifest_bytes,
        source_bytes,
        CompileOptions::default(),
        expected,
    );
}

fn manifest_value(bytes: &[u8]) -> Value {
    serde_json::from_slice(bytes).expect("manifest fixture is JSON")
}

fn representation_validator() -> jsonschema::Validator {
    let schema: Value = serde_json::from_slice(include_bytes!(
        "../../../contracts/w3-grammar-instance.schema.json"
    ))
    .expect("W3 representation schema is JSON");
    jsonschema::draft202012::new(&schema).expect("W3 representation schema compiles")
}

fn assert_schema_accepts(manifest_bytes: &[u8]) {
    let value = manifest_value(manifest_bytes);
    representation_validator()
        .validate(&value)
        .unwrap_or_else(|error| panic!("representation schema rejected `{value}`: {error}"));
}

fn assert_schema_rejects(manifest_bytes: &[u8]) {
    let value = manifest_value(manifest_bytes);
    assert!(
        representation_validator().validate(&value).is_err(),
        "representation schema accepted invalid `{value}`"
    );
}

fn edited_manifest(bytes: &[u8], edit: impl FnOnce(&mut Value)) -> Vec<u8> {
    let mut value = manifest_value(bytes);
    edit(&mut value);
    serde_json::to_vec(&value).expect("edited manifest serializes")
}

fn edited_source_rule(bytes: &[u8], rule: &str, replacement: &str) -> Vec<u8> {
    let source = std::str::from_utf8(bytes).expect("source fixture is UTF-8");
    let prefix = format!("{rule} =");
    let original = source
        .lines()
        .find(|line| line.starts_with(&prefix))
        .unwrap_or_else(|| panic!("fixture rule `{rule}` exists"));
    assert_eq!(
        source
            .lines()
            .filter(|line| line.starts_with(&prefix))
            .count(),
        1,
        "fixture rule `{rule}` occurs once"
    );
    source.replacen(original, replacement, 1).into_bytes()
}

fn duplicate_member_before(
    bytes: &[u8],
    key: &str,
    occurrence: usize,
    duplicate_value: &Value,
) -> Vec<u8> {
    let source = std::str::from_utf8(bytes).expect("duplicate-key fixture is UTF-8 JSON");
    let needle = format!("\"{key}\"");
    let offset = source
        .match_indices(&needle)
        .nth(occurrence)
        .map(|(offset, _)| offset)
        .unwrap_or_else(|| panic!("JSON fixture contains occurrence {occurrence} of `{key}`"));
    let duplicate = format!(
        "\"{key}\":{},",
        serde_json::to_string(duplicate_value).expect("duplicate value serializes")
    );
    let mut output = Vec::with_capacity(bytes.len() + duplicate.len());
    output.extend_from_slice(&bytes[..offset]);
    output.extend_from_slice(duplicate.as_bytes());
    output.extend_from_slice(&bytes[offset..]);
    output
}

fn two_table_pratt_manifest() -> Vec<u8> {
    edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"]
            .as_array_mut()
            .expect("Pratt tables are an array")
            .push(json!({
                "site_rule": "expression-hyphen",
                "atom_rule": "atom-hyphen",
                "operators": [{
                    "matcher_rule": "operator-hyphen",
                    "fixity": "postfix",
                    "left_binding_power": 50,
                    "right_binding_power": null
                }]
            }));
    })
}

fn object_mut<'a>(value: &'a mut Value, at: &str) -> &'a mut Map<String, Value> {
    value
        .as_object_mut()
        .unwrap_or_else(|| panic!("{at} is an object"))
}

fn revision(manifest_bytes: &[u8], source_bytes: &[u8]) -> [u8; 32] {
    accepted(manifest_bytes, source_bytes)
        .source_map()
        .revision()
        .bytes()
}

fn rule_root(module: &CompiledModule, name: &str) -> NodeId {
    module
        .program()
        .rules()
        .iter()
        .find(|rule| rule.name() == name)
        .unwrap_or_else(|| panic!("fixture rule `{name}` exists"))
        .root()
}

fn kind(module: &CompiledModule, node: NodeId) -> NodeKind<'_> {
    module
        .program()
        .node(node)
        .expect("node belongs to fixture program")
        .kind()
}

#[test]
fn explicit_instance_entry_has_no_root_or_first_fallback() {
    let module = accepted(SIMPLE_MANIFEST, SIMPLE_SOURCE);
    let entry = module
        .program()
        .rule(module.program().entry())
        .expect("entry belongs to program");

    assert_eq!(entry.name(), "document");
    assert_ne!(entry.name(), "root");
    assert_ne!(entry.name(), "first");

    let unknown = edited_manifest(SIMPLE_MANIFEST, |manifest| {
        manifest["entry"] = "missing_but_valid_name".into();
    });
    assert_default_rejected_with(&unknown, SIMPLE_SOURCE, DiagnosticCode::UnknownEntry);
}

#[test]
fn strict_schema_rejects_wrong_missing_duplicate_and_additional_fields_at_every_depth() {
    let wrong_schema = edited_manifest(SIMPLE_MANIFEST, |manifest| {
        manifest["schema"] = "bbnf.grammar-instance.v0".into();
    });
    let missing_top = [
        "schema",
        "logical_path",
        "entry",
        "trivia_regex",
        "pratt_tables",
    ]
    .map(|missing| {
        edited_manifest(SIMPLE_MANIFEST, |manifest| {
            object_mut(manifest, "manifest").remove(missing);
        })
    });
    let extra_top = edited_manifest(SIMPLE_MANIFEST, |manifest| {
        manifest["format"] = "json".into();
    });
    let missing_table = ["site_rule", "atom_rule", "operators"].map(|missing| {
        edited_manifest(PRATT_MANIFEST, |manifest| {
            object_mut(&mut manifest["pratt_tables"][0], "table").remove(missing);
        })
    });
    let extra_table = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["priority"] = 7.into();
    });
    let missing_operator = [
        "matcher_rule",
        "fixity",
        "left_binding_power",
        "right_binding_power",
    ]
    .map(|missing| {
        edited_manifest(PRATT_MANIFEST, |manifest| {
            object_mut(&mut manifest["pratt_tables"][0]["operators"][0], "operator")
                .remove(missing);
        })
    });
    let extra_operator = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["operators"][0]["token"] = "-".into();
    });
    let simple = manifest_value(SIMPLE_MANIFEST);
    let pratt = manifest_value(PRATT_MANIFEST);
    let duplicate_top = [
        ("schema", &simple["schema"]),
        ("logical_path", &simple["logical_path"]),
        ("entry", &simple["entry"]),
        ("trivia_regex", &simple["trivia_regex"]),
        ("pratt_tables", &simple["pratt_tables"]),
    ]
    .map(|(key, value)| duplicate_member_before(SIMPLE_MANIFEST, key, 0, value));
    let duplicate_table = [
        ("site_rule", &pratt["pratt_tables"][0]["site_rule"]),
        ("atom_rule", &pratt["pratt_tables"][0]["atom_rule"]),
        ("operators", &pratt["pratt_tables"][0]["operators"]),
    ]
    .map(|(key, value)| duplicate_member_before(PRATT_MANIFEST, key, 0, value));
    let duplicate_operator = [
        (
            "matcher_rule",
            &pratt["pratt_tables"][0]["operators"][0]["matcher_rule"],
        ),
        (
            "fixity",
            &pratt["pratt_tables"][0]["operators"][0]["fixity"],
        ),
        (
            "left_binding_power",
            &pratt["pratt_tables"][0]["operators"][0]["left_binding_power"],
        ),
        (
            "right_binding_power",
            &pratt["pratt_tables"][0]["operators"][0]["right_binding_power"],
        ),
    ]
    .map(|(key, value)| duplicate_member_before(PRATT_MANIFEST, key, 0, value));

    let representation_validator = representation_validator();
    for manifest in [SIMPLE_MANIFEST, PRATT_MANIFEST, PRETTY_MANIFEST] {
        let value = manifest_value(manifest);
        representation_validator
            .validate(&value)
            .unwrap_or_else(|error| panic!("valid instance rejected by schema: {error}"));
    }
    for manifest in [
        wrong_schema.as_slice(),
        extra_top.as_slice(),
        extra_table.as_slice(),
        extra_operator.as_slice(),
    ] {
        let value = manifest_value(manifest);
        assert!(
            representation_validator.validate(&value).is_err(),
            "representation schema accepted an invalid manifest: {value}"
        );
    }

    for manifest in &missing_top {
        assert_schema_rejects(manifest);
    }
    for manifest in [wrong_schema.as_slice()]
        .into_iter()
        .chain(missing_top.iter().map(Vec::as_slice))
        .chain([extra_top.as_slice()])
    {
        assert_default_rejected_with(
            manifest,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }
    for manifest in &duplicate_top {
        assert_default_rejected_with(
            manifest,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }
    for manifest in missing_table.iter().chain(&missing_operator) {
        assert_schema_rejects(manifest);
    }
    for manifest in [extra_table.as_slice()]
        .into_iter()
        .chain(missing_table.iter().map(Vec::as_slice))
        .chain(missing_operator.iter().map(Vec::as_slice))
        .chain([extra_operator.as_slice()])
    {
        assert_default_rejected_with(
            manifest,
            PRATT_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }
    for manifest in duplicate_table.iter().chain(&duplicate_operator) {
        assert_default_rejected_with(
            manifest,
            PRATT_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }

    let wrong_top_shapes = [
        edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["schema"] = Value::Bool(true);
        }),
        edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["logical_path"] = json!(["contracts/w3/simple.bbnf"]);
        }),
        edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["entry"] = json!(7);
        }),
        edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["trivia_regex"] = Value::Bool(false);
        }),
        edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["pratt_tables"] = json!({});
        }),
    ];
    let wrong_pratt_shapes = [
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["site_rule"] = json!(["expression"]);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["atom_rule"] = json!(false);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"] = json!({});
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"] = json!([]);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0]["matcher_rule"] = json!(9);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0]["fixity"] = Value::Bool(true);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0]["right_binding_power"] = json!("70");
        }),
    ];
    for (source, manifests) in [
        (SIMPLE_SOURCE, wrong_top_shapes.as_slice()),
        (PRATT_SOURCE, wrong_pratt_shapes.as_slice()),
    ] {
        for manifest in manifests {
            let value = manifest_value(manifest);
            assert!(
                representation_validator.validate(&value).is_err(),
                "representation schema accepted a wrong-shaped instance: {value}"
            );
            assert_default_rejected_with(manifest, source, DiagnosticCode::InvalidGrammarInstance);
        }
    }

    let trailing_json = [SIMPLE_MANIFEST, b"\n{}".as_slice()].concat();
    for malformed in [
        Vec::new(),
        vec![0xff],
        b"{".to_vec(),
        b"null".to_vec(),
        b"[]".to_vec(),
        b"\"instance\"".to_vec(),
        trailing_json,
    ] {
        assert_default_rejected_with(
            &malformed,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }
}

#[test]
fn manifest_paths_and_all_rule_name_positions_are_closed_and_portable() {
    for path in [
        "",
        "/absolute.bbnf",
        "C:/absolute.bbnf",
        "z:/absolute.bbnf",
        "../escape.bbnf",
        "contracts/../escape.bbnf",
        r"contracts\windows.bbnf",
        "contracts/\0hidden.bbnf",
        "contracts/\nnewline.bbnf",
        "contracts/\ttab.bbnf",
        "contracts/\u{1f}control.bbnf",
        "contracts/\u{7f}delete.bbnf",
    ] {
        let manifest = edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["logical_path"] = path.into();
        });
        assert_schema_rejects(&manifest);
        assert_default_rejected_with(
            &manifest,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }

    let hyphen_entry = edited_manifest(SIMPLE_MANIFEST, |manifest| {
        manifest["entry"] = "hyphen-entry".into();
    });
    assert_schema_accepts(&hyphen_entry);
    let hyphen_entry_module = accepted(&hyphen_entry, SIMPLE_SOURCE);
    assert_eq!(
        hyphen_entry_module
            .program()
            .rule(hyphen_entry_module.program().entry())
            .expect("hyphenated entry exists")
            .name(),
        "hyphen-entry"
    );

    let hyphen_pratt_names = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["site_rule"] = "expression-hyphen".into();
        manifest["pratt_tables"][0]["atom_rule"] = "atom-hyphen".into();
        manifest["pratt_tables"][0]["operators"][2]["matcher_rule"] = "operator-hyphen".into();
    });
    assert_schema_accepts(&hyphen_pratt_names);
    accepted(&hyphen_pratt_names, PRATT_SOURCE);

    for bad_name in ["", "9name", "-leading", "has.dot", "naïve"] {
        let bad_entry = edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest["entry"] = bad_name.into();
        });
        assert_schema_rejects(&bad_entry);
        assert_default_rejected_with(
            &bad_entry,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );

        for location in ["site_rule", "atom_rule", "matcher_rule"] {
            let manifest = edited_manifest(PRATT_MANIFEST, |manifest| match location {
                "site_rule" | "atom_rule" => {
                    manifest["pratt_tables"][0][location] = bad_name.into();
                }
                "matcher_rule" => {
                    manifest["pratt_tables"][0]["operators"][0][location] = bad_name.into();
                }
                _ => unreachable!(),
            });
            assert_schema_rejects(&manifest);
            assert_default_rejected_with(
                &manifest,
                PRATT_SOURCE,
                DiagnosticCode::InvalidGrammarInstance,
            );
        }
    }
}

#[test]
fn forbidden_metadata_and_historical_source_surfaces_fail_closed() {
    let representation_schema: Value = serde_json::from_slice(include_bytes!(
        "../../../contracts/w3-grammar-instance.schema.json"
    ))
    .expect("W3 representation schema is JSON");
    let representation_validator = jsonschema::draft202012::new(&representation_schema)
        .expect("W3 representation schema compiles");

    for forbidden_key in [
        "normalization",
        "recovery",
        "imports",
        "ws",
        "recover",
        "actions",
        "callbacks",
        "native",
        "dependencies",
        "features",
        "plugins",
        "backend",
        "format",
        "grammar_functions",
        "unary_negation",
        "pratt",
    ] {
        let manifest = edited_manifest(SIMPLE_MANIFEST, |manifest| {
            manifest[forbidden_key] = json!({"enabled": true});
        });
        let value = manifest_value(&manifest);
        assert!(
            representation_validator.validate(&value).is_err(),
            "representation schema accepted forbidden `{forbidden_key}` metadata"
        );
        assert_default_rejected_with(
            &manifest,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
    }

    let forbidden_sources: &[(&str, &[u8])] = &[
        (
            "import directive",
            br#"@import "dependency.bbnf"; document = "x";"#,
        ),
        ("legacy @ws directive", br#"@ws /[ \t]*/; document = "x";"#),
        (
            "legacy @recover directive",
            br#"@recover document /[^;]+/; document = "x";"#,
        ),
        (
            "unknown top-level directive",
            br#"@unknown ignored; document = "x";"#,
        ),
        (
            "top-level action directive",
            br#"@action document callback; document = "x";"#,
        ),
        (
            "historical @{...} action block",
            br#"document = @{"native_callback()"};"#,
        ),
        ("host arrow", br#"document = "x" -> Span;"#),
        (
            "legacy @host declaration",
            br#"@host decode: Span; document = "x";"#,
        ),
        (
            "block-bodied @host function",
            br#"@host fn decode(input: Span) -> Span { input } document = "x";"#,
        ),
        (
            "callback directive",
            br#"@callback parse native_callback; document = "x";"#,
        ),
        (
            "native directive",
            br#"@native "parser.rs"; document = "x";"#,
        ),
        (
            "dependency directive",
            br#"@dependency "serde"; document = "x";"#,
        ),
        ("feature directive", br#"@feature simd; document = "x";"#),
        ("plugin directive", br#"@plugin formatter; document = "x";"#),
        ("backend directive", br#"@backend rust; document = "x";"#),
        ("format directive", br#"@format json; document = "x";"#),
        (
            "grammar function parameterized definition",
            br#"document(argument) = "x";"#,
        ),
        (
            "grammar function invocation",
            br#"document = helper("x"); helper = "x";"#,
        ),
        ("grammar function closure", br#"document = |argument| "x";"#),
        ("unary negation", br#"document = !"x";"#),
        (
            "source Pratt directive",
            br#"@pratt expression atom; document = "x";"#,
        ),
        (
            "duplicate source Pratt directives",
            br#"@pratt expression atom; @pratt expression atom; document = "x";"#,
        ),
    ];
    for (label, source) in forbidden_sources {
        let StageResult::Rejected { diagnostics } =
            compile(SIMPLE_MANIFEST, source, CompileOptions::default())
        else {
            panic!("{label}: forbidden historical source surface was accepted");
        };
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.code() == DiagnosticCode::Syntax),
            "{label}: expected Syntax, got:\n{}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    let admitted = br#"
document = @capture("x") | @error("y");
@pretty document group;
"#;
    let module = accepted(SIMPLE_MANIFEST, admitted);
    let transform_names = module
        .program()
        .nodes()
        .iter()
        .filter_map(|node| match node.kind() {
            NodeKind::Transform { name, .. } => Some(name),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        transform_names,
        BTreeSet::from(["capture", "error"]),
        "action-free expression transforms remain inert admitted metadata"
    );
}

#[test]
fn every_new_compile_budget_accepts_the_exact_cap_and_rejects_one_under() {
    let instance_size = SIMPLE_MANIFEST.len();
    let mut exact_instance = CompileOptions::default();
    exact_instance.instance_bytes = instance_size;
    accepted_with(SIMPLE_MANIFEST, SIMPLE_SOURCE, exact_instance);
    let mut under_instance = exact_instance;
    under_instance.instance_bytes = instance_size - 1;
    assert_rejected_with(
        SIMPLE_MANIFEST,
        SIMPLE_SOURCE,
        under_instance,
        DiagnosticCode::ResourceLimit,
    );

    let mut exact_tables = CompileOptions::default();
    exact_tables.pratt_tables = 1;
    accepted_with(PRATT_MANIFEST, PRATT_SOURCE, exact_tables);
    let mut under_tables = exact_tables;
    under_tables.pratt_tables = 0;
    assert_rejected_with(
        PRATT_MANIFEST,
        PRATT_SOURCE,
        under_tables,
        DiagnosticCode::ResourceLimit,
    );

    let mut exact_operators = CompileOptions::default();
    exact_operators.pratt_operators = 3;
    accepted_with(PRATT_MANIFEST, PRATT_SOURCE, exact_operators);
    let mut under_operators = exact_operators;
    under_operators.pratt_operators = 2;
    assert_rejected_with(
        PRATT_MANIFEST,
        PRATT_SOURCE,
        under_operators,
        DiagnosticCode::ResourceLimit,
    );

    let mut exact_hints = CompileOptions::default();
    exact_hints.presentation_hints = 9;
    accepted_with(PRETTY_MANIFEST, PRETTY_SOURCE, exact_hints);
    let mut under_hints = exact_hints;
    under_hints.presentation_hints = 8;
    assert_rejected_with(
        PRETTY_MANIFEST,
        PRETTY_SOURCE,
        under_hints,
        DiagnosticCode::ResourceLimit,
    );

    let two_table_manifest = two_table_pratt_manifest();
    let mut exact_aggregate = CompileOptions::default();
    exact_aggregate.pratt_tables = 2;
    exact_aggregate.pratt_operators = 4;
    accepted_with(&two_table_manifest, PRATT_SOURCE, exact_aggregate);

    let mut under_aggregate_tables = exact_aggregate;
    under_aggregate_tables.pratt_tables = 1;
    assert_rejected_with(
        &two_table_manifest,
        PRATT_SOURCE,
        under_aggregate_tables,
        DiagnosticCode::ResourceLimit,
    );
    let mut under_aggregate_operators = exact_aggregate;
    under_aggregate_operators.pratt_operators = 3;
    assert_rejected_with(
        &two_table_manifest,
        PRATT_SOURCE,
        under_aggregate_operators,
        DiagnosticCode::ResourceLimit,
    );

    let multi_row_pretty = [PRETTY_SOURCE, b"\n@pretty item group;\n".as_slice()].concat();
    let mut exact_aggregate_hints = CompileOptions::default();
    exact_aggregate_hints.presentation_hints = 10;
    accepted_with(PRETTY_MANIFEST, &multi_row_pretty, exact_aggregate_hints);
    let mut under_aggregate_hints = exact_aggregate_hints;
    under_aggregate_hints.presentation_hints = 9;
    assert_rejected_with(
        PRETTY_MANIFEST,
        &multi_row_pretty,
        under_aggregate_hints,
        DiagnosticCode::ResourceLimit,
    );
}

#[test]
fn trivia_must_be_nullable_used_and_coupled_to_trim_syntax() {
    accepted(PRATT_MANIFEST, PRATT_SOURCE);

    let nonnullable = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["trivia_regex"] = "[ \\t]+".into();
    });
    let invalid_regex = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["trivia_regex"] = "[".into();
    });
    let absent_but_used = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["trivia_regex"] = Value::Null;
    });
    let present_but_unused = edited_manifest(SIMPLE_MANIFEST, |manifest| {
        manifest["trivia_regex"] = "[ \\t]*".into();
    });

    for (manifest, source) in [
        (nonnullable.as_slice(), PRATT_SOURCE),
        (invalid_regex.as_slice(), PRATT_SOURCE),
        (absent_but_used.as_slice(), PRATT_SOURCE),
        (present_but_unused.as_slice(), SIMPLE_SOURCE),
    ] {
        assert_default_rejected_with(manifest, source, DiagnosticCode::InvalidGrammarInstance);
    }
}

fn push_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(out: &mut Vec<u8>, value: usize) {
    out.extend_from_slice(
        &u32::try_from(value)
            .expect("reference collection count fits u32")
            .to_le_bytes(),
    );
}

fn push_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    out.extend_from_slice(
        &u64::try_from(bytes.len())
            .expect("reference byte length fits u64")
            .to_le_bytes(),
    );
    out.extend_from_slice(bytes);
}

fn push_string(out: &mut Vec<u8>, value: &Value, at: &str) {
    push_bytes(
        out,
        value
            .as_str()
            .unwrap_or_else(|| panic!("{at} is a string"))
            .as_bytes(),
    );
}

fn push_optional_power(out: &mut Vec<u8>, value: &Value, at: &str) {
    if value.is_null() {
        out.push(0);
        return;
    }
    out.push(1);
    let value = value
        .as_u64()
        .unwrap_or_else(|| panic!("{at} is an unsigned integer"));
    push_u16(
        out,
        u16::try_from(value).unwrap_or_else(|_| panic!("{at} fits u16")),
    );
}

fn reference_revision(manifest_bytes: &[u8], source_bytes: &[u8]) -> [u8; 32] {
    let manifest = manifest_value(manifest_bytes);
    let mut encoded = Vec::new();
    encoded.extend_from_slice(b"bbnf.w3.revision.v1\0");
    push_string(&mut encoded, &manifest["schema"], "schema");
    push_string(&mut encoded, &manifest["logical_path"], "logical_path");
    push_string(&mut encoded, &manifest["entry"], "entry");

    if manifest["trivia_regex"].is_null() {
        encoded.push(0);
    } else {
        encoded.push(1);
        push_string(&mut encoded, &manifest["trivia_regex"], "trivia_regex");
    }

    let tables = manifest["pratt_tables"]
        .as_array()
        .expect("pratt_tables is an array");
    push_u32(&mut encoded, tables.len());
    for (table_index, table) in tables.iter().enumerate() {
        push_string(
            &mut encoded,
            &table["site_rule"],
            &format!("table {table_index} site"),
        );
        push_string(
            &mut encoded,
            &table["atom_rule"],
            &format!("table {table_index} atom"),
        );
        let operators = table["operators"]
            .as_array()
            .expect("operators is an array");
        push_u32(&mut encoded, operators.len());
        for (operator_index, operator) in operators.iter().enumerate() {
            push_string(
                &mut encoded,
                &operator["matcher_rule"],
                &format!("operator {operator_index} matcher"),
            );
            encoded.push(match operator["fixity"].as_str() {
                Some("prefix") => 0,
                Some("postfix") => 1,
                Some("infix") => 2,
                other => panic!("operator {operator_index} fixity: {other:?}"),
            });
            push_optional_power(
                &mut encoded,
                &operator["left_binding_power"],
                &format!("operator {operator_index} left power"),
            );
            push_optional_power(
                &mut encoded,
                &operator["right_binding_power"],
                &format!("operator {operator_index} right power"),
            );
        }
    }

    push_bytes(&mut encoded, source_bytes);
    *blake3::hash(&encoded).as_bytes()
}

fn hex(bytes: [u8; 32]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

#[test]
fn canonical_revision_matches_an_independent_typed_reference_vector() {
    const EXPECTED: &str = "0f51312026da8e482babfb834000acffddca432977091d62ce2e2449b768d181";

    let reference = reference_revision(PRATT_MANIFEST, PRATT_SOURCE);
    assert_eq!(hex(reference), EXPECTED, "test-local reference vector");
    assert_eq!(
        revision(PRATT_MANIFEST, PRATT_SOURCE),
        reference,
        "product Revision must use the domain-separated typed encoding"
    );
    assert_eq!(
        revision(PRATT_MANIFEST_REFORMATTED, PRATT_SOURCE),
        reference,
        "JSON key order and insignificant whitespace are not revision inputs"
    );
}

#[test]
fn reference_revision_encoder_separates_each_manifest_field_without_grouped_mutations() {
    let baseline = reference_revision(PRATT_MANIFEST, PRATT_SOURCE);
    let isolated = [
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["schema"] = "bbnf.grammar-instance.v0".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["logical_path"] = "contracts/w3/reference-renamed.bbnf".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["entry"] = "alternate".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["trivia_regex"] = "(?:[ \\t]|\\r?\\n)*".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["trivia_regex"] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["site_rule"] = "expression-hyphen".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["atom_rule"] = "atom-hyphen".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["matcher_rule"] = "operator-hyphen".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["fixity"] = "prefix".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 39.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 40.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"]
                .as_array_mut()
                .expect("operators are an array")
                .push(manifest_value(PRATT_MANIFEST)["pratt_tables"][0]["operators"][2].clone());
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"]
                .as_array_mut()
                .expect("operators are an array")
                .swap(0, 1);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"] = json!([]);
        }),
        two_table_pratt_manifest(),
        edited_manifest(&two_table_pratt_manifest(), |manifest| {
            manifest["pratt_tables"]
                .as_array_mut()
                .expect("tables are an array")
                .swap(0, 1);
        }),
    ];
    let mut revisions = BTreeSet::from([baseline]);
    for manifest in isolated {
        let changed = reference_revision(&manifest, PRATT_SOURCE);
        assert_ne!(
            changed, baseline,
            "one isolated manifest field/order change must change the reference encoding"
        );
        assert!(
            revisions.insert(changed),
            "isolated typed revision fields must not collapse"
        );
    }

    let mut changed_source = PRATT_SOURCE.to_vec();
    changed_source.push(b' ');
    assert_ne!(
        reference_revision(PRATT_MANIFEST, &changed_source),
        baseline,
        "the exact source byte string is an independently encoded revision field"
    );
}

#[test]
fn canonical_revision_separates_every_semantic_field_order_and_exact_source_bytes() {
    let baseline = revision(PRATT_MANIFEST, PRATT_SOURCE);
    let semantic_manifests = [
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["logical_path"] = "contracts/w3/renamed.bbnf".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["entry"] = "alternate".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["trivia_regex"] = "(?:[ \\t]|\\r?\\n)*".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["site_rule"] = "expression-hyphen".into();
            manifest["pratt_tables"][0]["atom_rule"] = "atom-hyphen".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["matcher_rule"] = "operator-hyphen".into();
            manifest["pratt_tables"][0]["operators"][2]["fixity"] = "postfix".into();
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 40.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 41.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 41.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["matcher_rule"] = "operator-hyphen".into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 40.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"]
                .as_array_mut()
                .expect("operators is an array")
                .swap(0, 1);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"] = json!([]);
        }),
        two_table_pratt_manifest(),
        edited_manifest(&two_table_pratt_manifest(), |manifest| {
            manifest["pratt_tables"]
                .as_array_mut()
                .expect("tables are an array")
                .swap(0, 1);
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"]
                .as_array_mut()
                .expect("operators are an array")
                .push(json!({
                    "matcher_rule": "operator-hyphen",
                    "fixity": "postfix",
                    "left_binding_power": 50,
                    "right_binding_power": null
                }));
        }),
    ];

    let mut revisions = BTreeSet::from([baseline]);
    for manifest in semantic_manifests {
        let changed = accepted(&manifest, PRATT_SOURCE)
            .source_map()
            .revision()
            .bytes();
        assert_ne!(changed, baseline);
        assert!(
            revisions.insert(changed),
            "distinct semantic fixtures must not collapse to one Revision"
        );
    }

    let mut changed_source = PRATT_SOURCE.to_vec();
    changed_source.extend_from_slice(b"\n// exact source-byte change\n");
    assert_ne!(revision(PRATT_MANIFEST, &changed_source), baseline);
}

#[test]
fn product_revision_matches_the_independent_encoder_for_each_isolated_semantic_field() {
    let baseline = revision(PRATT_MANIFEST, PRATT_SOURCE);
    assert_eq!(
        baseline,
        reference_revision(PRATT_MANIFEST, PRATT_SOURCE),
        "the product and independent typed encoder agree at the baseline"
    );

    let mut fixtures = Vec::<(&str, Vec<u8>, Vec<u8>)>::new();
    fixtures.push((
        "logical_path",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["logical_path"] = "contracts/w3/renamed.bbnf".into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "entry",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["entry"] = "alternate".into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "trivia pattern",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["trivia_regex"] = "(?:[ \\t]|\\r?\\n)*".into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "trivia null tag",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["trivia_regex"] = Value::Null;
        }),
        std::str::from_utf8(PRATT_SOURCE)
            .expect("Pratt source is UTF-8")
            .replace(" ?w", "")
            .into_bytes(),
    ));
    fixtures.push((
        "site_rule alone",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["site_rule"] = "expression-alias".into();
        }),
        [PRATT_SOURCE, b"\nexpression-alias = atom;\n".as_slice()].concat(),
    ));
    fixtures.push((
        "atom_rule with only its required source-root coupling",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["atom_rule"] = "atom-hyphen".into();
        }),
        edited_source_rule(PRATT_SOURCE, "expression", "expression = atom-hyphen;"),
    ));
    fixtures.push((
        "matcher_rule alone",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["matcher_rule"] = "operator-hyphen".into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "fixity with only its required optional-power shape coupling",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["fixity"] = "postfix".into();
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 40.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = Value::Null;
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "prefix right_binding_power alone",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0]["right_binding_power"] = 71.into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "postfix left_binding_power alone",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][1]["left_binding_power"] = 91.into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "infix left_binding_power alone",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 41.into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "infix right_binding_power alone",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 40.into();
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "operator order",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"]
                .as_array_mut()
                .expect("operators is an array")
                .swap(0, 1);
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "zero table count",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"] = json!([]);
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "table count",
        two_table_pratt_manifest(),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "table order",
        edited_manifest(&two_table_pratt_manifest(), |manifest| {
            manifest["pratt_tables"]
                .as_array_mut()
                .expect("tables is an array")
                .swap(0, 1);
        }),
        PRATT_SOURCE.to_vec(),
    ));
    fixtures.push((
        "operator count",
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"]
                .as_array_mut()
                .expect("operators is an array")
                .push(json!({
                    "matcher_rule": "operator-hyphen",
                    "fixity": "postfix",
                    "left_binding_power": 50,
                    "right_binding_power": null
                }));
        }),
        PRATT_SOURCE.to_vec(),
    ));

    let mut changed_source = PRATT_SOURCE.to_vec();
    changed_source.extend_from_slice(b"\n// exact source-byte change\n");
    fixtures.push((
        "exact source bytes and length",
        PRATT_MANIFEST.to_vec(),
        changed_source,
    ));

    let mut revisions = BTreeSet::from([baseline]);
    for (label, manifest, source) in fixtures {
        assert_schema_accepts(&manifest);
        let reference = reference_revision(&manifest, &source);
        let changed = revision(&manifest, &source);
        assert_eq!(
            changed, reference,
            "{label}: product must encode the exact independently specified field bytes"
        );
        assert_ne!(
            changed, baseline,
            "{label}: one semantic field/order/source mutation changes Revision"
        );
        assert!(
            revisions.insert(changed),
            "{label}: distinct semantic fixtures must not collapse to one Revision"
        );
    }
}

#[test]
fn pratt_rejects_duplicate_or_unresolved_sites_atoms_and_operators() {
    let duplicate_site = edited_manifest(PRATT_MANIFEST, |manifest| {
        let duplicate = manifest["pratt_tables"][0].clone();
        manifest["pratt_tables"]
            .as_array_mut()
            .expect("tables is an array")
            .push(duplicate);
    });
    let duplicate_operator = edited_manifest(PRATT_MANIFEST, |manifest| {
        let duplicate = manifest["pratt_tables"][0]["operators"][2].clone();
        manifest["pratt_tables"][0]["operators"]
            .as_array_mut()
            .expect("operators is an array")
            .push(duplicate);
    });
    let duplicate_operator_with_distinct_powers = edited_manifest(PRATT_MANIFEST, |manifest| {
        let mut duplicate = manifest["pratt_tables"][0]["operators"][2].clone();
        duplicate["left_binding_power"] = 41.into();
        duplicate["right_binding_power"] = 41.into();
        manifest["pratt_tables"][0]["operators"]
            .as_array_mut()
            .expect("operators is an array")
            .push(duplicate);
    });
    let duplicate_operator_with_distinct_fixity = edited_manifest(PRATT_MANIFEST, |manifest| {
        let mut duplicate = manifest["pratt_tables"][0]["operators"][2].clone();
        duplicate["fixity"] = "postfix".into();
        duplicate["left_binding_power"] = 50.into();
        duplicate["right_binding_power"] = Value::Null;
        manifest["pratt_tables"][0]["operators"]
            .as_array_mut()
            .expect("operators is an array")
            .push(duplicate);
    });
    let duplicate_site_with_distinct_rows = edited_manifest(PRATT_MANIFEST, |manifest| {
        let mut duplicate = manifest["pratt_tables"][0].clone();
        duplicate["operators"] = json!([{
            "matcher_rule": "operator-hyphen",
            "fixity": "postfix",
            "left_binding_power": 50,
            "right_binding_power": null
        }]);
        manifest["pratt_tables"]
            .as_array_mut()
            .expect("tables is an array")
            .push(duplicate);
    });
    let unresolved_site = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["site_rule"] = "missing_site".into();
    });
    let unresolved_atom = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["atom_rule"] = "missing_atom".into();
    });
    let unresolved_operator = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["operators"][0]["matcher_rule"] = "missing_operator".into();
    });

    for manifest in [
        duplicate_site,
        duplicate_site_with_distinct_rows,
        duplicate_operator,
        duplicate_operator_with_distinct_powers,
        duplicate_operator_with_distinct_fixity,
        unresolved_site,
        unresolved_atom,
        unresolved_operator,
    ] {
        assert_schema_accepts(&manifest);
        assert_default_rejected_with(&manifest, PRATT_SOURCE, DiagnosticCode::InvalidPrattTable);
    }
}

#[test]
fn pratt_site_root_must_be_exactly_its_atom_reference() {
    let illegal_source_roots = [
        ("Epsilon", "expression = epsilon;"),
        ("Literal", "expression = \"x\";"),
        ("Regex", "expression = /x/;"),
        ("Sequence", "expression = atom, epsilon;"),
        ("Choice", "expression = atom | number;"),
        ("Repeat", "expression = atom?;"),
        ("Repeat", "expression = atom*;"),
        ("Repeat", "expression = atom+;"),
        ("Repeat", "expression = { atom };"),
        ("Repeat", "expression = [ atom ];"),
        ("Transform", "expression = @capture(atom);"),
        ("Record", "expression = %{ value: atom };"),
        ("Projection", "expression = atom << number;"),
        ("Projection", "expression = atom >> number;"),
        ("Difference", "expression = atom - number;"),
        ("Trim", "expression = atom ?w;"),
    ];
    for (_kind, replacement) in illegal_source_roots {
        let source = edited_source_rule(PRATT_SOURCE, "expression", replacement);
        assert_default_rejected_with(PRATT_MANIFEST, &source, DiagnosticCode::InvalidPrattTable);
    }

    let syntax_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/compiler/syntax.rs");
    let syntax_source = fs::read_to_string(syntax_path).expect("syntax.rs");
    let syntax = syn::parse_file(&syntax_source).expect("syntax.rs syntax");
    let node_kind_data = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Enum(item) if item.ident == "NodeKindData" => Some(item),
            _ => None,
        })
        .expect("one internal NodeKindData enum");
    let variants = node_kind_data
        .variants
        .iter()
        .map(|variant| variant.ident.to_string())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        variants,
        BTreeSet::from([
            "Choice".to_owned(),
            "Difference".to_owned(),
            "Epsilon".to_owned(),
            "Literal".to_owned(),
            "Pratt".to_owned(),
            "Projection".to_owned(),
            "Record".to_owned(),
            "Regex".to_owned(),
            "Repeat".to_owned(),
            "RuleRef".to_owned(),
            "Sequence".to_owned(),
            "Transform".to_owned(),
            "Trim".to_owned(),
        ]),
        "a new source NodeKind requires an explicit Pratt site-root disposition"
    );
    let covered_source_variants = illegal_source_roots
        .iter()
        .map(|(kind, _)| (*kind).to_owned())
        .collect::<BTreeSet<_>>();
    let expected_source_variants = variants
        .iter()
        .filter(|kind| !matches!(kind.as_str(), "RuleRef" | "Pratt"))
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_eq!(
        covered_source_variants, expected_source_variants,
        "every non-reference source root has a product rejection fixture"
    );
    assert!(
        variants.contains("Pratt"),
        "the synthetic Pratt root is closed separately by duplicate-site rejection"
    );
}

#[test]
fn pratt_fixity_and_optional_binding_power_shapes_are_closed() {
    accepted(PRATT_MANIFEST, PRATT_SOURCE);
    for valid_edge in [
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0]["right_binding_power"] = 1.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][0]["right_binding_power"] = 65_535.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][1]["left_binding_power"] = 1.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][1]["left_binding_power"] = 65_535.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 1.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 1.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 1.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 2.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 65_534.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 65_535.into();
        }),
        edited_manifest(PRATT_MANIFEST, |manifest| {
            manifest["pratt_tables"][0]["operators"][2]["left_binding_power"] = 65_535.into();
            manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 65_535.into();
        }),
    ] {
        assert_schema_accepts(&valid_edge);
        accepted(&valid_edge, PRATT_SOURCE);
    }

    let representation_invalid = [
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][0]["fixity"] = "ternary".into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][0]["left_binding_power"] = 1.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][0]["right_binding_power"] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][0]["right_binding_power"] = 0.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][0]["right_binding_power"] = 65_536.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][1]["left_binding_power"] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][1]["right_binding_power"] = 91.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][1]["left_binding_power"] = 0.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][1]["left_binding_power"] = 65_536.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][2]["left_binding_power"] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][2]["right_binding_power"] = Value::Null;
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][2]["left_binding_power"] = 0.into();
            m["pratt_tables"][0]["operators"][2]["right_binding_power"] = 1.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][2]["left_binding_power"] = 65_536.into();
            m["pratt_tables"][0]["operators"][2]["right_binding_power"] = 65_536.into();
        }),
        edited_manifest(PRATT_MANIFEST, |m| {
            m["pratt_tables"][0]["operators"][2]["left_binding_power"] = 65_535.into();
            m["pratt_tables"][0]["operators"][2]["right_binding_power"] = 65_536.into();
        }),
    ];

    for manifest in representation_invalid {
        assert_schema_rejects(&manifest);
        assert_default_rejected_with(&manifest, PRATT_SOURCE, DiagnosticCode::InvalidPrattTable);
    }

    let invalid_associativity = edited_manifest(PRATT_MANIFEST, |manifest| {
        manifest["pratt_tables"][0]["operators"][2]["right_binding_power"] = 42.into();
    });
    assert_schema_accepts(&invalid_associativity);
    assert_default_rejected_with(
        &invalid_associativity,
        PRATT_SOURCE,
        DiagnosticCode::InvalidPrattTable,
    );
}

#[test]
fn frontend_comments_postfix_binary_comma_and_repeat_precedence_are_exact() {
    let source = br#"
// Leading source trivia is transactional.
document = "a" ?w << "b" - "x" >> "c", { "d" }, [ "e" ];
/* One non-nested source comment. */
literal_comment = "//";
regex_comment = /\/\*/;
choice_precedence = "left" | "a" << "b", "c" - "d" | "right";
"#;
    let manifest = br#"{
      "schema": "bbnf.grammar-instance.v1",
      "logical_path": "contracts/w3/frontend-shapes.bbnf",
      "entry": "document",
      "trivia_regex": "[ \t\r\n]*",
      "pratt_tables": []
    }"#;
    let module = accepted(manifest, source);

    let sequence = match kind(&module, rule_root(&module, "document")) {
        NodeKind::Sequence(children) => children.collect::<Vec<_>>(),
        other => panic!("comma concatenation must be outside the binary tier: {other:?}"),
    };
    assert_eq!(sequence.len(), 3);

    let binary_left = match kind(&module, sequence[0]) {
        NodeKind::Projection { left, right, keep } => {
            assert_eq!(keep, ProjectionKeep::Right, "`>>` keeps its right operand");
            assert!(matches!(kind(&module, right), NodeKind::Literal(bytes) if bytes == b"c"));
            left
        }
        other => panic!("left-associative binary tier must end in `>>`: {other:?}"),
    };
    let projected_left = match kind(&module, binary_left) {
        NodeKind::Difference { include, exclude } => {
            assert!(matches!(kind(&module, exclude), NodeKind::Literal(bytes) if bytes == b"x"));
            include
        }
        other => panic!("`-` must associate between the projections: {other:?}"),
    };
    let trimmed = match kind(&module, projected_left) {
        NodeKind::Projection { left, right, keep } => {
            assert_eq!(keep, ProjectionKeep::Left, "`<<` keeps its left operand");
            assert!(matches!(kind(&module, right), NodeKind::Literal(bytes) if bytes == b"b"));
            left
        }
        other => panic!("the inner binary operation must be `<<`: {other:?}"),
    };
    assert!(matches!(kind(&module, trimmed), NodeKind::Trim { .. }));
    assert!(matches!(
        kind(&module, sequence[1]),
        NodeKind::Repeat {
            minimum: 0,
            maximum: None,
            ..
        }
    ));
    assert!(matches!(
        kind(&module, sequence[2]),
        NodeKind::Repeat {
            minimum: 0,
            maximum: Some(1),
            ..
        }
    ));
    assert!(matches!(
        kind(&module, rule_root(&module, "literal_comment")),
        NodeKind::Literal(bytes) if bytes == b"//"
    ));
    assert!(matches!(
        kind(&module, rule_root(&module, "regex_comment")),
        NodeKind::Regex(pattern) if pattern == r"\/\*"
    ));

    let choice = match kind(&module, rule_root(&module, "choice_precedence")) {
        NodeKind::Choice(children) => children.collect::<Vec<_>>(),
        other => panic!("choice must be the outermost expression tier: {other:?}"),
    };
    assert_eq!(choice.len(), 3);
    let middle = match kind(&module, choice[1]) {
        NodeKind::Sequence(children) => children.collect::<Vec<_>>(),
        other => panic!("comma concatenation must remain inside choice: {other:?}"),
    };
    assert_eq!(middle.len(), 2);
    assert!(matches!(
        kind(&module, middle[0]),
        NodeKind::Projection {
            keep: ProjectionKeep::Left,
            ..
        }
    ));
    assert!(matches!(
        kind(&module, middle[1]),
        NodeKind::Difference { .. }
    ));
}

#[test]
fn comments_commit_only_after_a_complete_delimiter_and_never_nest() {
    let valid = br#"
document /* between name and equals */ = "/" /* committed */, "*" // line
  , "//", /\/\*/;
"#;
    let manifest = br#"{
      "schema": "bbnf.grammar-instance.v1",
      "logical_path": "contracts/w3/comment-transaction.bbnf",
      "entry": "document",
      "trivia_regex": null,
      "pratt_tables": []
    }"#;
    accepted(manifest, valid);

    let unterminated = br#"document = "x"; /* never committed"#;
    let nested = br#"document = "x"; /* outer /* nested */ still outer */"#;
    assert_default_rejected_with(manifest, unterminated, DiagnosticCode::Syntax);
    assert_default_rejected_with(manifest, nested, DiagnosticCode::Syntax);
}

fn json_has_key(value: &Value, expected: &str) -> bool {
    match value {
        Value::Object(map) => map
            .iter()
            .any(|(key, value)| key == expected || json_has_key(value, expected)),
        Value::Array(values) => values.iter().any(|value| json_has_key(value, expected)),
        _ => false,
    }
}

fn replay_value(bytes: Vec<u8>) -> Value {
    serde_json::from_slice(&bytes).expect("carrier replay is JSON")
}

#[test]
fn pretty_vocabulary_is_finite_targets_validate_and_rows_replay_only_in_source_map() {
    let module = accepted(PRETTY_MANIFEST, PRETTY_SOURCE);
    let source_replay = replay_value(
        module
            .source_map()
            .replay_json()
            .expect("source replay serializes"),
    );
    assert_eq!(
        source_replay["schema"], "bbnf.source-map.v4",
        "presentation lives in the adjudicated source-map schema"
    );
    assert_eq!(
        source_replay["value"]["modules"][0]["presentation"],
        json!([{
            "target": "document",
            "hints": [
                {"kind": "group"},
                {"kind": "block"},
                {"kind": "indent"},
                {"kind": "dedent"},
                {"kind": "softline"},
                {"kind": "hardline"},
                {"kind": "blankline"},
                {"kind": "compact"},
                {"kind": "sep", "literal": [44, 32]}
            ]
        }]),
        "presentation rows preserve exact source order and typed sep bytes"
    );

    let non_source_replays = [
        module.program().replay_json().expect("program replay"),
        module.remap().replay_json().expect("remap replay"),
        module
            .dependency()
            .replay_json()
            .expect("dependency replay"),
        module.types().replay_json().expect("type replay"),
        module.facts().replay_json().expect("facts replay"),
        module.lowering().replay_json().expect("lowering replay"),
        module.events().replay_json().expect("event replay"),
        module.generated().replay_json().expect("generated replay"),
    ];
    for replay in non_source_replays {
        assert!(
            !json_has_key(&replay_value(replay), "presentation"),
            "presentation metadata must not leak beyond the source map"
        );
    }

    for invalid_source in [
        br#"document = "x" ?w; @pretty missing group;"#.as_slice(),
        br#"document = "x" ?w; @pretty document fast;"#.as_slice(),
        br#"document = "x" ?w; @pretty document nobreak;"#.as_slice(),
        br#"document = "x" ?w; @pretty document sep();"#.as_slice(),
        br#"document = "x" ?w; @pretty document sep(/[ ,]/);"#.as_slice(),
    ] {
        assert_rejected(PRETTY_MANIFEST, invalid_source);
    }
}

fn rust_caller_files(root: &Path, output: &mut BTreeSet<PathBuf>) {
    let mut entries = fs::read_dir(root)
        .unwrap_or_else(|error| panic!("read {}: {error}", root.display()))
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|error| panic!("read entry below {}: {error}", root.display()));
    entries.sort_by_key(|entry| entry.file_name());

    for entry in entries {
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if path.is_dir() {
            if matches!(name.as_ref(), "test-generated" | "target" | "candidates") {
                continue;
            }
            rust_caller_files(&path, output);
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("rs") {
            output.insert(path);
        }
    }
}

fn module_graph_union_findings(_relative: &str, source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    struct ModuleGraph {
        item_capable_macro: bool,
        findings: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for ModuleGraph {
        fn visit_item_macro(&mut self, item: &'ast syn::ItemMacro) {
            let previous = std::mem::replace(&mut self.item_capable_macro, true);
            syn::visit::visit_item_macro(self, item);
            self.item_capable_macro = previous;
        }

        fn visit_stmt_macro(&mut self, statement: &'ast syn::StmtMacro) {
            let previous = std::mem::replace(&mut self.item_capable_macro, true);
            syn::visit::visit_stmt_macro(self, statement);
            self.item_capable_macro = previous;
        }

        fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
            if attribute.path().is_ident("path") {
                self.findings
                    .insert("out-of-tree #[path] module edge".to_owned());
            }
            if attribute.path().is_ident("cfg_attr")
                && attribute.meta.require_list().is_ok_and(|list| {
                    list.tokens
                        .to_string()
                        .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                        .any(|word| word == "path")
                })
            {
                self.findings
                    .insert("conditional #[cfg_attr(..., path = ...)] module edge".to_owned());
            }
            syn::visit::visit_attribute(self, attribute);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            use syn::parse::Parser as _;

            fn token_stream_carries_module_edge(
                input: syn::parse::ParseStream<'_>,
            ) -> syn::Result<bool> {
                let mut carries_edge = false;
                while !input.is_empty() {
                    if input.peek(syn::token::Paren) {
                        let content;
                        syn::parenthesized!(content in input);
                        carries_edge |= token_stream_carries_module_edge(&content)?;
                        continue;
                    }
                    if input.peek(syn::token::Bracket) {
                        let content;
                        syn::bracketed!(content in input);
                        carries_edge |= token_stream_carries_module_edge(&content)?;
                        continue;
                    }
                    if input.peek(syn::token::Brace) {
                        let content;
                        syn::braced!(content in input);
                        carries_edge |= token_stream_carries_module_edge(&content)?;
                        continue;
                    }
                    if input.peek(syn::Ident) {
                        let ident = input.parse::<syn::Ident>()?;
                        if ident == "include" && input.peek(syn::Token![!]) {
                            carries_edge = true;
                        }
                        continue;
                    }
                    let spelling = input.step(|cursor| {
                        cursor
                            .token_tree()
                            .map(|(token, next)| (token.to_string(), next))
                            .ok_or_else(|| cursor.error("expected a token tree"))
                    })?;
                    carries_edge |= spelling == "mod";
                }
                Ok(carries_edge)
            }

            #[derive(Default)]
            struct InvocationEdges(bool);
            impl<'ast> syn::visit::Visit<'ast> for InvocationEdges {
                fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
                    self.0 = true;
                    syn::visit::visit_item_mod(self, item);
                }

                fn visit_attribute(&mut self, attribute: &'ast syn::Attribute) {
                    self.0 |= attribute.path().is_ident("path")
                        || (attribute.path().is_ident("cfg_attr")
                            && attribute.meta.require_list().is_ok_and(|list| {
                                list.tokens
                                    .to_string()
                                    .split(|character: char| {
                                        !(character == '_' || character.is_alphanumeric())
                                    })
                                    .any(|word| word == "path")
                            }));
                    syn::visit::visit_attribute(self, attribute);
                }

                fn visit_macro(&mut self, nested: &'ast syn::Macro) {
                    self.0 |= nested
                        .path
                        .segments
                        .last()
                        .is_some_and(|segment| segment.ident == "include");
                    syn::visit::visit_macro(self, nested);
                }
            }

            let macro_name = macro_
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default();
            if macro_name == "include" {
                self.findings
                    .insert("unsealed include! source edge".to_owned());
            }
            if macro_name == "macro_rules" {
                let tokens = macro_.tokens.to_string();
                let words = tokens
                    .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                    .filter(|word| !word.is_empty())
                    .collect::<BTreeSet<_>>();
                if ["include", "mod", "path"]
                    .into_iter()
                    .any(|word| words.contains(word))
                {
                    self.findings
                        .insert("macro-generated module edge".to_owned());
                }
            } else if macro_name != "include" && self.item_capable_macro {
                let mut edges = InvocationEdges::default();
                if let Ok(file) = syn::parse2::<syn::File>(macro_.tokens.clone()) {
                    edges.visit_file(&file);
                }
                if let Ok(block) = syn::parse2::<syn::Block>(macro_.tokens.clone()) {
                    edges.visit_block(&block);
                }
                if let Ok(expression) = syn::parse2::<syn::Expr>(macro_.tokens.clone()) {
                    edges.visit_expr(&expression);
                }
                let token_edge = token_stream_carries_module_edge
                    .parse2(macro_.tokens.clone())
                    .unwrap_or(true);
                if edges.0 || token_edge {
                    self.findings
                        .insert("macro invocation carries an unsealed module edge".to_owned());
                }
            }
            syn::visit::visit_macro(self, macro_);
        }
    }

    let syntax = syn::parse_file(source).expect("module-graph source is valid Rust");
    let mut graph = ModuleGraph {
        item_capable_macro: false,
        findings: BTreeSet::new(),
    };
    graph.visit_file(&syntax);
    graph.findings
}

fn legacy_surface_findings(source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    fn revision_domain() -> String {
        ["bbnf.w2.source", ".v4"].concat()
    }

    #[derive(Default)]
    struct LegacySurface {
        findings: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for LegacySurface {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            let ident = ident.to_string();
            if matches!(
                ident.as_str(),
                "CompilationSession"
                    | "EditError"
                    | "ModuleInput"
                    | "compile_module"
                    | "for_module"
            ) {
                self.findings.insert(format!("legacy identifier `{ident}`"));
            }
        }

        fn visit_lit_str(&mut self, literal: &'ast syn::LitStr) {
            if literal.value().contains(&revision_domain()) {
                self.findings.insert("legacy W2 revision domain".to_owned());
            }
        }

        fn visit_lit_byte_str(&mut self, literal: &'ast syn::LitByteStr) {
            let domain = revision_domain();
            if literal
                .value()
                .windows(domain.len())
                .any(|window| window == domain.as_bytes())
            {
                self.findings.insert("legacy W2 revision domain".to_owned());
            }
        }

        fn visit_macro(&mut self, item: &'ast syn::Macro) {
            if !item.path.is_ident("macro_rules") {
                syn::visit::visit_macro(self, item);
                return;
            }
            let tokens = item.tokens.to_string();
            for forbidden in [
                "CompilationSession",
                "EditError",
                "ModuleInput",
                "compile_module",
                "for_module",
            ] {
                if tokens
                    .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                    .any(|token| token == forbidden)
                {
                    self.findings
                        .insert(format!("legacy macro identifier `{forbidden}`"));
                }
            }
            syn::visit::visit_macro(self, item);
        }
    }

    let syntax = syn::parse_file(source).expect("legacy-surface source is valid Rust");
    let mut surface = LegacySurface::default();
    surface.visit_file(&syntax);
    surface.findings
}

fn resolution_fallback_findings(syntax_source: &str, source_source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    fn exact_cfg_test(attributes: &[syn::Attribute]) -> bool {
        attributes.iter().any(|attribute| {
            attribute.path().is_ident("cfg")
                && attribute
                    .meta
                    .require_list()
                    .is_ok_and(|list| list.tokens.to_string() == "test")
        })
    }

    fn collect_blocks(
        items: &[syn::Item],
        inherited_test: bool,
        blocks: &mut std::collections::BTreeMap<String, Vec<syn::Block>>,
        resolve: &mut Vec<syn::Block>,
    ) {
        for item in items {
            let item_test = inherited_test
                || match item {
                    syn::Item::Fn(item) => exact_cfg_test(&item.attrs),
                    syn::Item::Impl(item) => exact_cfg_test(&item.attrs),
                    syn::Item::Mod(item) => exact_cfg_test(&item.attrs),
                    _ => false,
                };
            match item {
                syn::Item::Fn(function) if !item_test => {
                    blocks
                        .entry(function.sig.ident.to_string())
                        .or_default()
                        .push((*function.block).clone());
                }
                syn::Item::Impl(item) if !item_test => {
                    let program = matches!(
                        item.self_ty.as_ref(),
                        syn::Type::Path(path)
                            if path.qself.is_none()
                                && path.path.segments.last().is_some_and(
                                    |segment| segment.ident == "Program"
                                )
                    );
                    for member in &item.items {
                        let syn::ImplItem::Fn(function) = member else {
                            continue;
                        };
                        if exact_cfg_test(&function.attrs) {
                            continue;
                        }
                        blocks
                            .entry(function.sig.ident.to_string())
                            .or_default()
                            .push(function.block.clone());
                        if program && function.sig.ident == "resolve" {
                            resolve.push(function.block.clone());
                        }
                    }
                }
                syn::Item::Mod(module) => {
                    if let Some((_, nested)) = &module.content {
                        collect_blocks(nested, item_test, blocks, resolve);
                    }
                }
                _ => {}
            }
        }
    }

    #[derive(Default)]
    struct LocalCalls {
        names: BTreeSet<String>,
        aliases: std::collections::BTreeMap<String, String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for LocalCalls {
        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let syn::Pat::Ident(binding) = &local.pat
                && let Some(syn::LocalInit { expr, .. }) = &local.init
                && let syn::Expr::Path(path) = expr.as_ref()
                && path.qself.is_none()
                && path.path.segments.len() == 1
                && let Some(target) = path.path.get_ident()
            {
                self.aliases
                    .insert(binding.ident.to_string(), target.to_string());
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let syn::Expr::Path(path) = call.func.as_ref()
                && path.qself.is_none()
                && path.path.segments.len() == 1
                && let Some(name) = path.path.get_ident()
            {
                self.names.insert(name.to_string());
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.names.insert(call.method.to_string());
            syn::visit::visit_expr_method_call(self, call);
        }
    }

    #[derive(Default)]
    struct Forbidden {
        findings: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Forbidden {
        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            let name = call.method.to_string();
            let direct = matches!(name.as_str(), "first" | "next" | "position")
                || (matches!(name.as_str(), "get" | "nth")
                    && call.args.len() == 1
                    && call.args.first().is_some_and(|argument| {
                        matches!(
                            argument,
                            syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Int(literal),
                                ..
                            }) if matches!(literal.base10_parse::<u64>(), Ok(0))
                        )
                    }));
            if direct {
                self.findings
                    .insert(format!("resolution fallback primitive `{name}`"));
            }
            syn::visit::visit_expr_method_call(self, call);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if matches!(
                call.func.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none()
                        && path.path.segments.last().is_some_and(|segment| {
                            matches!(
                                segment.ident.to_string().as_str(),
                                "first" | "next" | "position"
                            )
                        })
            ) {
                self.findings
                    .insert("resolution fallback associated/free call".to_owned());
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_index(&mut self, expression: &'ast syn::ExprIndex) {
            if matches!(
                expression.index.as_ref(),
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Int(literal),
                    ..
                }) if matches!(literal.base10_parse::<u64>(), Ok(0))
            ) {
                self.findings
                    .insert("resolution fallback zero index".to_owned());
            }
            syn::visit::visit_expr_index(self, expression);
        }

        fn visit_lit_str(&mut self, literal: &'ast syn::LitStr) {
            if literal.value() == "root" {
                self.findings
                    .insert("resolution fallback `root` spelling".to_owned());
            }
        }

        fn visit_lit_byte_str(&mut self, literal: &'ast syn::LitByteStr) {
            if literal.value() == b"root" {
                self.findings
                    .insert("resolution fallback byte `root` spelling".to_owned());
            }
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            use syn::parse::Parser as _;

            let macro_name = macro_
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string());
            if let Ok(arguments) =
                syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated
                    .parse2(macro_.tokens.clone())
            {
                let skips_template = matches!(
                    macro_name.as_deref(),
                    Some(
                        "eprint"
                            | "eprintln"
                            | "format"
                            | "format_args"
                            | "panic"
                            | "print"
                            | "println"
                            | "unreachable"
                    )
                );
                for (index, argument) in arguments.iter().enumerate() {
                    if skips_template && index == 0 && matches!(argument, syn::Expr::Lit(_)) {
                        continue;
                    }
                    self.visit_expr(argument);
                }
            } else {
                let compact = macro_
                    .tokens
                    .to_string()
                    .chars()
                    .filter(|character| !character.is_whitespace())
                    .collect::<String>();
                let contains_zero_call = |method: &str| {
                    let needle = format!(".{method}(");
                    let mut cursor = 0;
                    while let Some(offset) = compact[cursor..].find(&needle) {
                        let arguments_start = cursor + offset + needle.len();
                        let Some(arguments_end_offset) = compact[arguments_start..].find(')')
                        else {
                            return false;
                        };
                        let arguments_end = arguments_start + arguments_end_offset;
                        let literal = compact[arguments_start..arguments_end]
                            .chars()
                            .filter(|character| *character != '_')
                            .collect::<String>();
                        if literal.starts_with('0')
                            && literal
                                .chars()
                                .all(|character| character.is_ascii_alphanumeric())
                        {
                            return true;
                        }
                        cursor = arguments_end + 1;
                    }
                    false
                };
                for (forbidden, present) in [
                    ("first", compact.contains("first(")),
                    ("get", contains_zero_call("get")),
                    ("next", compact.contains("next(")),
                    ("nth", contains_zero_call("nth")),
                    ("position", compact.contains("position(")),
                    ("root", compact.contains("\"root\"")),
                ] {
                    if present {
                        self.findings
                            .insert(format!("macro-hidden resolution fallback `{forbidden}`"));
                    }
                }
            }
            syn::visit::visit_macro(self, macro_);
        }
    }

    let syntax = syn::parse_file(syntax_source).expect("resolution syntax source is valid Rust");
    let source = syn::parse_file(source_source).expect("resolution source source is valid Rust");
    let mut blocks = std::collections::BTreeMap::<String, Vec<syn::Block>>::new();
    let mut resolve = Vec::new();
    collect_blocks(&syntax.items, false, &mut blocks, &mut resolve);
    collect_blocks(&source.items, false, &mut blocks, &mut resolve);
    let mut findings = BTreeSet::new();
    if resolve.len() != 1 {
        findings.insert(format!(
            "Program::resolve closure root count is {}, expected 1",
            resolve.len()
        ));
        return findings;
    }

    let mut pending = vec![("Program::resolve".to_owned(), resolve.remove(0))];
    let mut inspected_blocks = BTreeSet::new();
    let mut expanded_names = BTreeSet::from(["resolve".to_owned()]);
    while let Some((key, block)) = pending.pop() {
        if !inspected_blocks.insert(key) {
            continue;
        }
        let mut forbidden = Forbidden::default();
        forbidden.visit_block(&block);
        findings.extend(forbidden.findings);

        let mut calls = LocalCalls::default();
        calls.visit_block(&block);
        for invoked in calls.names {
            let mut name = invoked;
            let mut alias_chain = BTreeSet::new();
            while alias_chain.insert(name.clone()) {
                let Some(target) = calls.aliases.get(&name) else {
                    break;
                };
                name = target.clone();
            }
            if !expanded_names.insert(name.clone()) {
                continue;
            }
            if let Some(candidates) = blocks.get(&name) {
                for (index, candidate) in candidates.iter().enumerate() {
                    pending.push((format!("{name}#{index}"), candidate.clone()));
                }
            }
        }
    }
    findings
}

fn rust_identifiers(source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct Identifiers(BTreeSet<String>);
    impl<'ast> syn::visit::Visit<'ast> for Identifiers {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.0.insert(ident.to_string());
        }
    }

    let syntax = syn::parse_file(source).expect("identifier source is valid Rust");
    let mut identifiers = Identifiers::default();
    identifiers.visit_file(&syntax);
    identifiers.0
}

fn cli_byte_transport_findings(source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    fn collect_utf8_argument_aliases(
        tree: &syn::UseTree,
        prefix: &mut Vec<String>,
        aliases: &mut BTreeSet<String>,
    ) {
        match tree {
            syn::UseTree::Path(path) => {
                prefix.push(path.ident.to_string());
                collect_utf8_argument_aliases(&path.tree, prefix, aliases);
                prefix.pop();
            }
            syn::UseTree::Name(name) => {
                let mut path = prefix.clone();
                path.push(name.ident.to_string());
                if path.ends_with(&["env".to_owned(), "args".to_owned()]) {
                    aliases.insert(name.ident.to_string());
                }
            }
            syn::UseTree::Rename(rename) => {
                let mut path = prefix.clone();
                path.push(rename.ident.to_string());
                if path.ends_with(&["env".to_owned(), "args".to_owned()]) {
                    aliases.insert(rename.rename.to_string());
                }
            }
            syn::UseTree::Group(group) => {
                for item in &group.items {
                    collect_utf8_argument_aliases(item, prefix, aliases);
                }
            }
            syn::UseTree::Glob(_) => {
                if prefix.ends_with(&["std".to_owned(), "env".to_owned()])
                    || prefix.ends_with(&["env".to_owned()])
                {
                    aliases.insert("*".to_owned());
                }
            }
        }
    }

    fn path_binding(expression: &syn::Expr, expected: &str) -> bool {
        match expression {
            syn::Expr::Path(path) => path.qself.is_none() && path.path.is_ident(expected),
            syn::Expr::Reference(reference) => path_binding(&reference.expr, expected),
            syn::Expr::Paren(paren) => path_binding(&paren.expr, expected),
            _ => false,
        }
    }

    fn path_is_one_of(path: &syn::Path, alternatives: &[&[&str]]) -> bool {
        alternatives.iter().any(|expected| {
            path.leading_colon.is_none()
                && path.segments.len() == expected.len()
                && path
                    .segments
                    .iter()
                    .zip(expected.iter())
                    .all(|(segment, expected)| {
                        segment.ident == *expected
                            && matches!(segment.arguments, syn::PathArguments::None)
                    })
        })
    }

    fn exact_fs_read(expression: &syn::Expr, expected_path: &str) -> bool {
        match expression {
            syn::Expr::Call(call) => {
                matches!(
                    call.func.as_ref(),
                    syn::Expr::Path(path)
                        if path.qself.is_none()
                        && path_is_one_of(
                            &path.path,
                            &[&["fs", "read"], &["std", "fs", "read"]],
                        )
                        && call.args.len() == 1
                ) && call
                    .args
                    .first()
                    .is_some_and(|argument| path_binding(argument, expected_path))
            }
            syn::Expr::MethodCall(call)
                if matches!(call.method.to_string().as_str(), "expect" | "unwrap")
                    && call.args.len() <= 1 =>
            {
                exact_fs_read(&call.receiver, expected_path)
            }
            syn::Expr::Try(expression) => exact_fs_read(&expression.expr, expected_path),
            syn::Expr::Paren(expression) => exact_fs_read(&expression.expr, expected_path),
            _ => false,
        }
    }

    fn exact_next_argument(expression: &syn::Expr) -> bool {
        match expression {
            syn::Expr::Group(group) => exact_next_argument(&group.expr),
            syn::Expr::Paren(paren) => exact_next_argument(&paren.expr),
            syn::Expr::Try(try_) => exact_next_argument(&try_.expr),
            syn::Expr::MethodCall(call) if call.method == "expect" && call.args.len() == 1 => {
                exact_next_argument(&call.receiver)
            }
            syn::Expr::MethodCall(call) if call.method == "next" && call.args.is_empty() => {
                matches!(
                    call.receiver.as_ref(),
                    syn::Expr::Path(path)
                        if path.qself.is_none() && path.path.is_ident("arguments")
                )
            }
            _ => false,
        }
    }

    fn exact_next_path(expression: &syn::Expr) -> bool {
        match expression {
            syn::Expr::Group(group) => exact_next_path(&group.expr),
            syn::Expr::Paren(paren) => exact_next_path(&paren.expr),
            syn::Expr::Try(try_) => exact_next_path(&try_.expr),
            syn::Expr::Call(call)
                if call.args.len() == 1
                    && matches!(
                        call.func.as_ref(),
                        syn::Expr::Path(path)
                            if path.qself.is_none()
                                && path_is_one_of(
                                    &path.path,
                                    &[
                                        &["PathBuf", "from"],
                                        &["std", "path", "PathBuf", "from"],
                                    ],
                                )
                    ) =>
            {
                call.args.first().is_some_and(exact_next_argument)
            }
            _ => false,
        }
    }

    fn exact_arguments_source(expression: &syn::Expr) -> bool {
        let syn::Expr::MethodCall(skip) = expression else {
            return false;
        };
        if skip.method != "skip"
            || skip.args.len() != 1
            || !matches!(
                skip.args.first(),
                Some(syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Int(literal),
                    ..
                })) if matches!(literal.base10_parse::<u64>(), Ok(1))
            )
        {
            return false;
        }
        matches!(
            skip.receiver.as_ref(),
            syn::Expr::Call(call)
                if call.args.is_empty()
                    && matches!(
                        call.func.as_ref(),
                        syn::Expr::Path(path)
                            if path.qself.is_none()
                                && path_is_one_of(
                                    &path.path,
                                    &[
                                        &["env", "args_os"],
                                        &["std", "env", "args_os"],
                                    ],
                                )
                    )
        )
    }

    fn exact_default_options(expression: &syn::Expr) -> bool {
        matches!(
            expression,
            syn::Expr::Call(call)
                if call.args.is_empty()
                    && matches!(
                        call.func.as_ref(),
                        syn::Expr::Path(path)
                            if path.qself.is_none()
                                && path_is_one_of(
                                    &path.path,
                                    &[
                                        &["CompileOptions", "default"],
                                        &["bbnf", "CompileOptions", "default"],
                                    ],
                                )
                    )
        )
    }

    fn exact_generated_source(
        expression: &syn::Expr,
        compilation_bindings: &BTreeSet<String>,
    ) -> bool {
        let syn::Expr::MethodCall(source) = expression else {
            return false;
        };
        if source.method != "source" || !source.args.is_empty() {
            return false;
        }
        let syn::Expr::MethodCall(generated) = source.receiver.as_ref() else {
            return false;
        };
        generated.method == "generated"
            && generated.args.is_empty()
            && matches!(
                generated.receiver.as_ref(),
                syn::Expr::Path(binding)
                    if binding.qself.is_none()
                        && binding.path.get_ident().is_some_and(|ident| {
                            compilation_bindings.contains(&ident.to_string())
                        })
            )
    }

    struct Transport<'a> {
        utf8_argument_aliases: &'a BTreeSet<String>,
        argument_sources: usize,
        utf8_argument_sources: usize,
        exact_argument_bindings: usize,
        noncanonical_next_calls: usize,
        exact_argument_next_calls: usize,
        fs_reads: usize,
        fs_writes: usize,
        manifest_path_uses: usize,
        source_path_uses: usize,
        output_path_uses: usize,
        manifest_bytes_uses: usize,
        source_bytes_uses: usize,
        exact_manifest_path_declarations: usize,
        exact_source_path_declarations: usize,
        exact_output_path_declarations: usize,
        exact_manifest_reads: usize,
        exact_source_reads: usize,
        exact_constructors: usize,
        exact_input_bindings: BTreeSet<String>,
        compilation_bindings: BTreeSet<String>,
        compile_calls: usize,
        exact_compile_calls: usize,
        exact_output_writes: usize,
        identity_surfaces: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Transport<'_> {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.manifest_path_uses += usize::from(ident == "manifest_path");
            self.source_path_uses += usize::from(ident == "source_path");
            self.output_path_uses += usize::from(ident == "output_path");
            self.manifest_bytes_uses += usize::from(ident == "manifest_bytes");
            self.source_bytes_uses += usize::from(ident == "source_bytes");
            if matches!(
                ident.to_string().as_str(),
                "File" | "OpenOptions" | "read_to_end" | "read_to_string" | "stdin"
            ) {
                self.identity_surfaces
                    .insert(format!("noncanonical I/O identifier `{ident}`"));
            }
        }

        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let (syn::Pat::Ident(binding), Some(syn::LocalInit { expr, .. })) =
                (&local.pat, &local.init)
            {
                if binding.ident == "arguments"
                    && binding.mutability.is_some()
                    && exact_arguments_source(expr)
                {
                    self.exact_argument_bindings += 1;
                }
                if binding.ident == "manifest_path" && exact_next_path(expr) {
                    self.exact_manifest_path_declarations += 1;
                }
                if binding.ident == "source_path" && exact_next_path(expr) {
                    self.exact_source_path_declarations += 1;
                }
                if binding.ident == "output_path" && exact_next_path(expr) {
                    self.exact_output_path_declarations += 1;
                }
                if binding.ident == "manifest_bytes" && exact_fs_read(expr, "manifest_path") {
                    self.exact_manifest_reads += 1;
                }
                if binding.ident == "source_bytes" && exact_fs_read(expr, "source_path") {
                    self.exact_source_reads += 1;
                }
                let exact_constructor = matches!(
                    expr.as_ref(),
                    syn::Expr::Call(call)
                        if matches!(
                            call.func.as_ref(),
                            syn::Expr::Path(path)
                                if path.qself.is_none()
                                    && path_is_one_of(
                                        &path.path,
                                        &[
                                            &["GrammarInstanceInput", "new"],
                                            &["bbnf", "GrammarInstanceInput", "new"],
                                        ],
                                    )
                        )
                            && call.args.len() == 2
                            && call.args.first().is_some_and(|argument| {
                                path_binding(argument, "manifest_bytes")
                            })
                            && call.args.last().is_some_and(|argument| {
                                path_binding(argument, "source_bytes")
                            })
                );
                if exact_constructor {
                    self.exact_input_bindings.insert(binding.ident.to_string());
                }
                if expression_mentions_ident(expr, "compile_instance") {
                    self.compilation_bindings.insert(binding.ident.to_string());
                }
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            let path = match call.func.as_ref() {
                syn::Expr::Path(path) if path.qself.is_none() => Some(&path.path),
                _ => None,
            };
            self.argument_sources += usize::from(path.is_some_and(|path| {
                path.segments
                    .iter()
                    .rev()
                    .take(2)
                    .map(|segment| segment.ident.to_string())
                    .collect::<Vec<_>>()
                    == ["args_os", "env"]
            }));
            self.utf8_argument_sources += usize::from(path.is_some_and(|path| {
                path.segments
                    .last()
                    .is_some_and(|segment| segment.ident == "args")
                    || path.get_ident().is_some_and(|ident| {
                        self.utf8_argument_aliases.contains(&ident.to_string())
                    })
            }));
            self.fs_reads += usize::from(path.is_some_and(|path| {
                path.segments
                    .iter()
                    .rev()
                    .take(2)
                    .map(|segment| segment.ident.to_string())
                    .collect::<Vec<_>>()
                    == ["read", "fs"]
            }));
            self.fs_writes += usize::from(path.is_some_and(|path| {
                path.segments
                    .iter()
                    .rev()
                    .take(2)
                    .map(|segment| segment.ident.to_string())
                    .collect::<Vec<_>>()
                    == ["write", "fs"]
            }));
            self.compile_calls += usize::from(path.is_some_and(|path| {
                path.segments
                    .last()
                    .is_some_and(|segment| segment.ident == "compile_instance")
            }));
            self.exact_compile_calls += usize::from(
                path.is_some_and(|path| {
                    path_is_one_of(
                        path,
                        &[&["compile_instance"], &["bbnf", "compile_instance"]],
                    )
                }) && call.args.len() == 2
                    && call.args.first().is_some_and(|argument| {
                        matches!(
                            argument,
                            syn::Expr::Call(constructor)
                                if matches!(
                                    constructor.func.as_ref(),
                                    syn::Expr::Path(path)
                                        if path.qself.is_none()
                                            && path_is_one_of(
                                                &path.path,
                                                &[
                                                    &["GrammarInstanceInput", "new"],
                                                    &[
                                                        "bbnf",
                                                        "GrammarInstanceInput",
                                                        "new",
                                                    ],
                                                ],
                                            )
                                )
                                    && constructor.args.len() == 2
                                    && constructor.args.first().is_some_and(|argument| {
                                        path_binding(argument, "manifest_bytes")
                                    })
                                    && constructor.args.last().is_some_and(|argument| {
                                        path_binding(argument, "source_bytes")
                                    })
                        ) || matches!(
                            argument,
                            syn::Expr::Path(binding)
                                if binding.qself.is_none()
                                    && binding.path.get_ident().is_some_and(|ident| {
                                        self.exact_input_bindings
                                            .contains(&ident.to_string())
                                    })
                        )
                    })
                    && call.args.last().is_some_and(exact_default_options),
            );
            self.exact_constructors += usize::from(
                path.is_some_and(|path| {
                    path_is_one_of(
                        path,
                        &[
                            &["GrammarInstanceInput", "new"],
                            &["bbnf", "GrammarInstanceInput", "new"],
                        ],
                    )
                }) && call.args.len() == 2
                    && call
                        .args
                        .first()
                        .is_some_and(|argument| path_binding(argument, "manifest_bytes"))
                    && call
                        .args
                        .last()
                        .is_some_and(|argument| path_binding(argument, "source_bytes")),
            );
            self.exact_output_writes += usize::from(
                path.is_some_and(|path| {
                    path_is_one_of(path, &[&["fs", "write"], &["std", "fs", "write"]])
                }) && call.args.len() == 2
                    && call
                        .args
                        .first()
                        .is_some_and(|argument| path_binding(argument, "output_path"))
                    && call.args.last().is_some_and(|argument| {
                        exact_generated_source(argument, &self.compilation_bindings)
                    }),
            );
            let qualified_next = path.is_some_and(|path| {
                path.segments
                    .last()
                    .is_some_and(|segment| segment.ident == "next")
            });
            if qualified_next {
                self.noncanonical_next_calls += 1;
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            let is_next = call.method == "next" && call.args.is_empty();
            let is_exact_next = is_next
                && matches!(
                    call.receiver.as_ref(),
                    syn::Expr::Path(path)
                        if path.qself.is_none() && path.path.is_ident("arguments")
                );
            self.noncanonical_next_calls += usize::from(is_next && !is_exact_next);
            self.exact_argument_next_calls += usize::from(is_exact_next);
            if matches!(
                call.method.to_string().as_str(),
                "canonicalize"
                    | "components"
                    | "contains"
                    | "ends_with"
                    | "extension"
                    | "file_name"
                    | "file_stem"
                    | "starts_with"
                    | "to_str"
                    | "to_string_lossy"
            ) && ["manifest_path", "source_path", "output_path"]
                .into_iter()
                .any(|name| expression_mentions_ident(&call.receiver, name))
            {
                self.identity_surfaces
                    .insert(format!("path inspection `{}`", call.method));
            }
            if matches!(
                call.method.to_string().as_str(),
                "read"
                    | "read_exact"
                    | "read_to_end"
                    | "read_to_string"
                    | "read_until"
                    | "write_all"
            ) {
                self.identity_surfaces
                    .insert(format!("noncanonical I/O method `{}`", call.method));
            }
            syn::visit::visit_expr_method_call(self, call);
        }

        fn visit_item_foreign_mod(&mut self, item: &'ast syn::ItemForeignMod) {
            self.identity_surfaces
                .insert("foreign I/O/argument surface".to_owned());
            syn::visit::visit_item_foreign_mod(self, item);
        }

        fn visit_expr_unsafe(&mut self, expression: &'ast syn::ExprUnsafe) {
            self.identity_surfaces
                .insert("unsafe I/O/argument surface".to_owned());
            syn::visit::visit_expr_unsafe(self, expression);
        }

        fn visit_lit_str(&mut self, literal: &'ast syn::LitStr) {
            let lower = literal.value().to_ascii_lowercase();
            for forbidden in ["candidate", "css", "format", "json", "plugin", "sheets"] {
                if lower
                    .split(|character: char| !character.is_ascii_alphanumeric())
                    .any(|word| word == forbidden)
                {
                    self.identity_surfaces
                        .insert(format!("identity literal `{forbidden}`"));
                }
            }
        }

        fn visit_lit_byte_str(&mut self, literal: &'ast syn::LitByteStr) {
            if let Ok(value) = String::from_utf8(literal.value()) {
                let lower = value.to_ascii_lowercase();
                for forbidden in ["candidate", "css", "format", "json", "plugin", "sheets"] {
                    if lower
                        .split(|character: char| !character.is_ascii_alphanumeric())
                        .any(|word| word == forbidden)
                    {
                        self.identity_surfaces
                            .insert(format!("identity byte literal `{forbidden}`"));
                    }
                }
            }
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            fn next_census(source: &str) -> (usize, usize) {
                let compact = source
                    .chars()
                    .filter(|character| !character.is_whitespace())
                    .collect::<String>();
                let bytes = compact.as_bytes();
                let mut cursor = 0;
                let mut all = 0;
                let mut exact = 0;
                while let Some(offset) = compact[cursor..].find("next(") {
                    let next = cursor + offset;
                    let arguments_start = next + "next(".len();
                    let Some(arguments_end_offset) = compact[arguments_start..].find(')') else {
                        break;
                    };
                    let arguments_end = arguments_start + arguments_end_offset;
                    let arguments = &compact[arguments_start..arguments_end];
                    all += 1;
                    let mut receiver_start = next.saturating_sub(1);
                    while receiver_start != 0
                        && (bytes[receiver_start - 1] == b'_'
                            || bytes[receiver_start - 1].is_ascii_alphanumeric())
                    {
                        receiver_start -= 1;
                    }
                    let exact_method = arguments.is_empty()
                        && next != 0
                        && bytes[next - 1] == b'.'
                        && &compact[receiver_start..next - 1] == "arguments"
                        && (receiver_start == 0
                            || !matches!(
                                bytes[receiver_start - 1],
                                b'.' | b':' | b'_' | b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z'
                            ));
                    exact += usize::from(exact_method);
                    cursor = arguments_end + 1;
                }
                (all, exact)
            }
            let (macro_next_calls, exact_macro_next_calls) =
                next_census(&macro_.tokens.to_string());
            self.noncanonical_next_calls += macro_next_calls.saturating_sub(exact_macro_next_calls);
            self.exact_argument_next_calls += exact_macro_next_calls;
            let compact = macro_
                .tokens
                .to_string()
                .bytes()
                .filter(u8::is_ascii_alphanumeric)
                .map(|byte| byte.to_ascii_lowercase() as char)
                .collect::<String>();
            for forbidden in [
                "candidate",
                "extension",
                "filename",
                "filestem",
                "formatdispatch",
                "json",
                "plugin",
                "sheets",
            ] {
                if compact.contains(forbidden) {
                    self.identity_surfaces
                        .insert(format!("macro identity surface `{forbidden}`"));
                }
            }
            for forbidden in [
                "File",
                "OpenOptions",
                "stdin",
                "read_to_end",
                "read_to_string",
            ] {
                if compact.contains(&forbidden.to_ascii_lowercase()) {
                    self.identity_surfaces
                        .insert(format!("macro I/O surface `{forbidden}`"));
                }
            }
            syn::visit::visit_macro(self, macro_);
        }
    }

    let syntax = syn::parse_file(source).expect("CLI source is valid Rust");
    fn has_renamed_or_glob_import(tree: &syn::UseTree) -> bool {
        match tree {
            syn::UseTree::Path(path) => has_renamed_or_glob_import(&path.tree),
            syn::UseTree::Group(group) => group.items.iter().any(has_renamed_or_glob_import),
            syn::UseTree::Rename(_) | syn::UseTree::Glob(_) => true,
            syn::UseTree::Name(_) => false,
        }
    }

    #[derive(Default)]
    struct AuthorityProvenance {
        imports: BTreeSet<Vec<String>>,
        findings: BTreeSet<String>,
    }
    impl AuthorityProvenance {
        fn record_shadow(&mut self, name: &syn::Ident, kind: &str) {
            if matches!(
                name.to_string().as_str(),
                "CompileOptions"
                    | "GrammarInstanceInput"
                    | "Iterator"
                    | "PathBuf"
                    | "StageResult"
                    | "bbnf"
                    | "compile_instance"
                    | "env"
                    | "fs"
                    | "std"
            ) {
                self.findings
                    .insert(format!("{kind} shadows canonical CLI authority `{name}`"));
            }
        }
    }
    impl<'ast> syn::visit::Visit<'ast> for AuthorityProvenance {
        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            if item.leading_colon.is_some() || has_renamed_or_glob_import(&item.tree) {
                self.findings
                    .insert("renamed, globbed, or absolute CLI authority import".to_owned());
            }
            collect_use_paths(&item.tree, &mut Vec::new(), &mut self.imports);
            syn::visit::visit_item_use(self, item);
        }

        fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
            self.record_shadow(&item.ident, "module");
            syn::visit::visit_item_mod(self, item);
        }

        fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
            self.record_shadow(&item.sig.ident, "function");
            syn::visit::visit_item_fn(self, item);
        }

        fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
            self.record_shadow(&item.ident, "struct");
            syn::visit::visit_item_struct(self, item);
        }

        fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
            self.record_shadow(&item.ident, "enum");
            syn::visit::visit_item_enum(self, item);
        }

        fn visit_item_union(&mut self, item: &'ast syn::ItemUnion) {
            self.record_shadow(&item.ident, "union");
            syn::visit::visit_item_union(self, item);
        }

        fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
            self.record_shadow(&item.ident, "trait");
            syn::visit::visit_item_trait(self, item);
        }

        fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
            self.record_shadow(&item.ident, "type alias");
            syn::visit::visit_item_type(self, item);
        }

        fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
            self.record_shadow(&item.ident, "constant");
            syn::visit::visit_item_const(self, item);
        }

        fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
            self.record_shadow(&item.ident, "static");
            syn::visit::visit_item_static(self, item);
        }

        fn visit_pat_ident(&mut self, pattern: &'ast syn::PatIdent) {
            self.record_shadow(&pattern.ident, "binding");
            syn::visit::visit_pat_ident(self, pattern);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            if macro_.path.is_ident("macro_rules") {
                let words = macro_
                    .tokens
                    .to_string()
                    .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                    .filter(|word| !word.is_empty())
                    .map(ToOwned::to_owned)
                    .collect::<BTreeSet<_>>();
                for name in [
                    "CompileOptions",
                    "GrammarInstanceInput",
                    "Iterator",
                    "PathBuf",
                    "StageResult",
                    "bbnf",
                    "compile_instance",
                    "env",
                    "fs",
                    "std",
                ] {
                    if words.contains(name) {
                        self.findings.insert(format!(
                            "macro shadows or fabricates CLI authority `{name}`"
                        ));
                    }
                }
            }
            syn::visit::visit_macro(self, macro_);
        }
    }

    let expected_authority_imports = [
        &["bbnf", "CompileOptions"][..],
        &["bbnf", "GrammarInstanceInput"][..],
        &["bbnf", "StageResult"][..],
        &["bbnf", "compile_instance"][..],
        &["std", "env"][..],
        &["std", "fs"][..],
        &["std", "path", "PathBuf"][..],
    ]
    .into_iter()
    .map(|path| path.iter().map(ToString::to_string).collect::<Vec<_>>())
    .collect::<BTreeSet<_>>();
    let mut provenance = AuthorityProvenance::default();
    provenance.visit_file(&syntax);
    if provenance.imports != expected_authority_imports {
        provenance.findings.insert(format!(
            "CLI authority imports are {:?}, expected {expected_authority_imports:?}",
            provenance.imports
        ));
    }

    #[derive(Default)]
    struct Utf8ArgumentImports(BTreeSet<String>);
    impl<'ast> syn::visit::Visit<'ast> for Utf8ArgumentImports {
        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            collect_utf8_argument_aliases(&item.tree, &mut Vec::new(), &mut self.0);
            syn::visit::visit_item_use(self, item);
        }
    }
    let mut imports = Utf8ArgumentImports::default();
    imports.visit_file(&syntax);
    let utf8_argument_aliases = imports.0;
    let mut transport = Transport {
        utf8_argument_aliases: &utf8_argument_aliases,
        argument_sources: 0,
        utf8_argument_sources: 0,
        exact_argument_bindings: 0,
        noncanonical_next_calls: 0,
        exact_argument_next_calls: 0,
        fs_reads: 0,
        fs_writes: 0,
        manifest_path_uses: 0,
        source_path_uses: 0,
        output_path_uses: 0,
        manifest_bytes_uses: 0,
        source_bytes_uses: 0,
        exact_manifest_path_declarations: 0,
        exact_source_path_declarations: 0,
        exact_output_path_declarations: 0,
        exact_manifest_reads: 0,
        exact_source_reads: 0,
        exact_constructors: 0,
        exact_input_bindings: BTreeSet::new(),
        compilation_bindings: BTreeSet::new(),
        compile_calls: 0,
        exact_compile_calls: 0,
        exact_output_writes: 0,
        identity_surfaces: BTreeSet::new(),
    };
    transport.visit_file(&syntax);
    let mut findings = provenance.findings;
    if !utf8_argument_aliases.is_empty() {
        findings.insert(format!(
            "env::args UTF-8 argument imports are present as {utf8_argument_aliases:?}"
        ));
    }
    for (surface, count, expected) in [
        ("env::args_os", transport.argument_sources, 1),
        (
            "live arguments = env::args_os().skip(1)",
            transport.exact_argument_bindings,
            1,
        ),
        (
            "live arguments.next",
            transport.exact_argument_next_calls,
            4,
        ),
        ("noncanonical next", transport.noncanonical_next_calls, 0),
        ("fs::read", transport.fs_reads, 2),
        ("fs::write", transport.fs_writes, 1),
    ] {
        if count != expected {
            findings.insert(format!(
                "{surface} call count is {count}, expected {expected}"
            ));
        }
    }
    if transport.utf8_argument_sources != 0 {
        findings.insert(format!(
            "env::args UTF-8 argument source count is {}, expected 0",
            transport.utf8_argument_sources
        ));
    }
    for (name, uses) in [
        ("manifest_path", transport.manifest_path_uses),
        ("source_path", transport.source_path_uses),
        ("output_path", transport.output_path_uses),
    ] {
        if uses != 2 {
            findings.insert(format!(
                "{name} has {uses} AST uses, expected declaration plus sole I/O call"
            ));
        }
    }
    for (name, count) in [
        ("manifest_path", transport.exact_manifest_path_declarations),
        ("source_path", transport.exact_source_path_declarations),
        ("output_path", transport.exact_output_path_declarations),
    ] {
        if count != 1 {
            findings.insert(format!(
                "{name} exact arguments.next() declaration count is {count}"
            ));
        }
    }
    if transport.manifest_bytes_uses != 2 {
        findings.insert(format!(
            "manifest_bytes has {} AST uses, expected declaration plus constructor only",
            transport.manifest_bytes_uses
        ));
    }
    if transport.source_bytes_uses != 2 {
        findings.insert(format!(
            "source_bytes has {} AST uses, expected declaration plus constructor only",
            transport.source_bytes_uses
        ));
    }
    if transport.exact_manifest_reads != 1 {
        findings.insert(format!(
            "exact manifest fs::read initializer count is {}",
            transport.exact_manifest_reads
        ));
    }
    if transport.exact_source_reads != 1 {
        findings.insert(format!(
            "exact source fs::read initializer count is {}",
            transport.exact_source_reads
        ));
    }
    if transport.exact_constructors != 1 {
        findings.insert(format!(
            "exact GrammarInstanceInput constructor count is {}",
            transport.exact_constructors
        ));
    }
    if transport.compile_calls != 1 {
        findings.insert(format!(
            "compile_instance call count is {}",
            transport.compile_calls
        ));
    }
    if transport.exact_compile_calls != 1 {
        findings.insert(format!(
            "compile_instance with exact default options count is {}",
            transport.exact_compile_calls
        ));
    }
    if transport.exact_output_writes != 1 {
        findings.insert(format!(
            "exact output-path fs::write count is {}",
            transport.exact_output_writes
        ));
    }
    findings.extend(transport.identity_surfaces);
    findings
}

const JSON_DECODERS: [&str; 5] = [
    "deserialize",
    "from_reader",
    "from_slice",
    "from_str",
    "from_value",
];
const JSON_DECODER_TYPES: [&str; 2] = ["Deserializer", "StreamDeserializer"];

fn collect_serde_decoder_imports(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    roots: &mut BTreeSet<String>,
    decoder_aliases: &mut BTreeSet<String>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            collect_serde_decoder_imports(&path.tree, prefix, roots, decoder_aliases);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix.clone();
            path.push(name.ident.to_string());
            if path == ["serde_json"] {
                roots.insert("serde_json".to_owned());
            } else if path == ["serde", "Deserialize"] {
                roots.insert("Deserialize".to_owned());
                roots.insert("serde".to_owned());
            } else if path.first().map(String::as_str) == Some("serde_json")
                && path
                    .last()
                    .is_some_and(|name| JSON_DECODER_TYPES.contains(&name.as_str()))
            {
                roots.insert(name.ident.to_string());
            } else if path.first().map(String::as_str) == Some("serde_json")
                && path
                    .last()
                    .is_some_and(|name| JSON_DECODERS.contains(&name.as_str()))
            {
                decoder_aliases.insert(name.ident.to_string());
            } else if path.first().map(String::as_str) == Some("serde_json") {
                roots.insert(name.ident.to_string());
            }
        }
        syn::UseTree::Rename(rename) => {
            let mut path = prefix.clone();
            path.push(rename.ident.to_string());
            let serde_json_import = path.first().map(String::as_str) == Some("serde_json");
            if path == ["serde_json"]
                || path == ["serde_json", "self"]
                || path == ["serde"]
                || path == ["serde", "self"]
                || path == ["serde", "Deserialize"]
                || (serde_json_import
                    && path
                        .last()
                        .is_some_and(|name| JSON_DECODER_TYPES.contains(&name.as_str())))
            {
                roots.insert(rename.rename.to_string());
            } else if serde_json_import
                && path
                    .last()
                    .is_some_and(|name| JSON_DECODERS.contains(&name.as_str()))
            {
                decoder_aliases.insert(rename.rename.to_string());
            } else if serde_json_import {
                roots.insert(rename.rename.to_string());
            }
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_serde_decoder_imports(item, prefix, roots, decoder_aliases);
            }
        }
        syn::UseTree::Glob(_) => {
            if prefix.first().map(String::as_str) == Some("serde_json") {
                decoder_aliases.extend(JSON_DECODERS.map(str::to_owned));
            }
        }
    }
}

fn serde_decoder_name(
    path: &syn::Path,
    roots: &BTreeSet<String>,
    decoder_aliases: &BTreeSet<String>,
) -> Option<String> {
    let segments = path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>();
    if segments.len() == 1 && decoder_aliases.contains(&segments[0]) {
        return Some(segments[0].clone());
    }
    let first = segments.first()?;
    let last = segments.last()?;
    (roots.contains(first) && JSON_DECODERS.contains(&last.as_str())).then(|| last.clone())
}

fn decoder_returns_json_value(path: &syn::Path) -> bool {
    let Some(last) = path.segments.last() else {
        return false;
    };
    let syn::PathArguments::AngleBracketed(arguments) = &last.arguments else {
        return false;
    };
    arguments.args.len() == 1
        && matches!(
            arguments.args.first(),
            Some(syn::GenericArgument::Type(syn::Type::Path(value)))
                if value.qself.is_none() && value.path.is_ident("Value")
        )
}

fn decoder_returns_nominal(path: &syn::Path, expected: &str) -> bool {
    let Some(last) = path.segments.last() else {
        return false;
    };
    let syn::PathArguments::AngleBracketed(arguments) = &last.arguments else {
        return false;
    };
    arguments.args.len() == 1
        && matches!(
            arguments.args.first(),
            Some(syn::GenericArgument::Type(syn::Type::Path(value)))
                if value.qself.is_none() && value.path.is_ident(expected)
        )
}

fn replay_envelope_argument(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Path(path) => {
            path.qself.is_none() && path.path.get_ident().is_some_and(|ident| ident == "packet")
        }
        syn::Expr::Reference(reference) => matches!(
            reference.expr.as_ref(),
            syn::Expr::Path(path)
                if path.qself.is_none()
                    && path.path.get_ident().is_some_and(
                        |ident| ident == "canonical_packet"
                    )
        ),
        _ => false,
    }
}

fn reconstructed_manifest_key(expression: &syn::ExprArray) -> Option<String> {
    let mut bytes = Vec::with_capacity(expression.elems.len());
    for element in &expression.elems {
        let byte = match element {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Int(literal),
                ..
            }) => u8::try_from(literal.base10_parse::<u16>().ok()?).ok()?,
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Byte(literal),
                ..
            }) => literal.value(),
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Char(literal),
                ..
            }) => u8::try_from(u32::from(literal.value())).ok()?,
            _ => return None,
        };
        bytes.push(byte);
    }
    let value = String::from_utf8(bytes).ok()?;
    matches!(
        value.as_str(),
        "schema"
            | "logical_path"
            | "entry"
            | "trivia_regex"
            | "pratt_tables"
            | "site_rule"
            | "atom_rule"
            | "operators"
            | "matcher_rule"
            | "fixity"
            | "left_binding_power"
            | "right_binding_power"
            | "bbnf.grammar-instance.v1"
    )
    .then_some(value)
}

fn macro_manifest_surface(macro_: &syn::Macro) -> Option<String> {
    use syn::parse::Parser as _;

    let source = macro_.tokens.to_string();
    let macro_name = macro_
        .path
        .segments
        .last()
        .map(|segment| segment.ident.to_string());
    let words = source
        .split(|character: char| !(character == '_' || character.is_alphanumeric()))
        .filter(|word| !word.is_empty())
        .map(str::to_ascii_lowercase)
        .collect::<BTreeSet<_>>();
    let decoder = [
        "serde_json",
        "deserialize",
        "deserializer",
        "from_slice",
        "from_str",
        "from_reader",
        "from_value",
    ]
    .into_iter()
    .any(|needle| words.contains(needle));
    let manifest_nominal = words.contains("grammarinstance");
    let manifest_vocabulary = [
        "schema",
        "logical_path",
        "entry",
        "trivia_regex",
        "pratt_tables",
    ]
    .into_iter()
    .all(|needle| words.contains(needle));
    let manifest = manifest_nominal
        || manifest_vocabulary
        || source.contains("bbnf.grammar-instance.v1")
        || (macro_.path.is_ident("concat")
            && syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated
                .parse2(macro_.tokens.clone())
                .ok()
                .and_then(|arguments| {
                    let mut joined = Vec::new();
                    for argument in arguments {
                        match argument {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Str(literal),
                                ..
                            }) => joined.extend_from_slice(literal.value().as_bytes()),
                            syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::ByteStr(literal),
                                ..
                            }) => joined.extend_from_slice(&literal.value()),
                            _ => return None,
                        }
                    }
                    Some(joined)
                })
                .and_then(|joined| String::from_utf8(joined).ok())
                .is_some_and(|joined| {
                    matches!(
                        joined.as_str(),
                        "schema"
                            | "logical_path"
                            | "entry"
                            | "trivia_regex"
                            | "pratt_tables"
                            | "site_rule"
                            | "atom_rule"
                            | "operators"
                            | "matcher_rule"
                            | "fixity"
                            | "left_binding_power"
                            | "right_binding_power"
                            | "bbnf.grammar-instance.v1"
                    )
                }));
    (decoder || manifest).then(|| macro_name.unwrap_or_else(|| "<anonymous>".to_owned()))
}

fn manifest_ownership_findings(relative: &str, source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    struct Ownership {
        decoder_calls: usize,
        typed_instance_decoder_calls: usize,
        invalid_decoder_calls: usize,
        replay_envelope_decoders: usize,
        grammar_instance_definitions: usize,
        five_field_models: usize,
        grammar_instance_five_field_models: usize,
        grammar_instance_schema_literals: usize,
        manifest_field_literals: BTreeSet<String>,
        public_ingress_calls: usize,
        reconstructed_manifest_keys: BTreeSet<String>,
        macro_manifest_surfaces: BTreeSet<String>,
        source_dependent_identifiers: BTreeSet<String>,
        diagnostic_identifiers: BTreeSet<String>,
        decoder_roots: BTreeSet<String>,
        decoder_aliases: BTreeSet<String>,
        current_function: Option<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Ownership {
        fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
            if item.ident == "GrammarInstance" {
                self.grammar_instance_definitions += 1;
            }
            if let syn::Fields::Named(fields) = &item.fields {
                let names = fields
                    .named
                    .iter()
                    .filter_map(|field| field.ident.as_ref())
                    .map(ToString::to_string)
                    .collect::<BTreeSet<_>>();
                if names
                    == BTreeSet::from([
                        "entry".to_owned(),
                        "logical_path".to_owned(),
                        "pratt_tables".to_owned(),
                        "schema".to_owned(),
                        "trivia_regex".to_owned(),
                    ])
                {
                    self.five_field_models += 1;
                    self.grammar_instance_five_field_models +=
                        usize::from(item.ident == "GrammarInstance");
                }
            }
            syn::visit::visit_item_struct(self, item);
        }

        fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
            if item.ident == "GrammarInstance" {
                self.grammar_instance_definitions += 1;
            }
            syn::visit::visit_item_enum(self, item);
        }

        fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
            if item.ident == "GrammarInstance" {
                self.grammar_instance_definitions += 1;
            }
            syn::visit::visit_item_type(self, item);
        }

        fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
            let previous = self
                .current_function
                .replace(function.sig.ident.to_string());
            syn::visit::visit_item_fn(self, function);
            self.current_function = previous;
        }

        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let (
                syn::Pat::Ident(binding),
                Some(syn::LocalInit {
                    expr,
                    diverge: None,
                    ..
                }),
            ) = (&local.pat, &local.init)
                && let syn::Expr::Path(path) = expr.as_ref()
                && serde_decoder_name(&path.path, &self.decoder_roots, &self.decoder_aliases)
                    .is_some()
            {
                self.decoder_aliases.insert(binding.ident.to_string());
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let syn::Expr::Path(path) = call.func.as_ref() {
                let segments = path
                    .path
                    .segments
                    .iter()
                    .map(|segment| segment.ident.to_string())
                    .collect::<Vec<_>>();
                if let Some(decoder) =
                    serde_decoder_name(&path.path, &self.decoder_roots, &self.decoder_aliases)
                {
                    self.decoder_calls += 1;
                    self.typed_instance_decoder_calls +=
                        usize::from(decoder_returns_nominal(&path.path, "GrammarInstance"));
                    let is_replay_envelope = self.current_function.as_deref()
                        == Some("replay_compilation")
                        && decoder == "from_slice"
                        && decoder_returns_json_value(&path.path)
                        && call.args.len() == 1
                        && call.args.first().is_some_and(replay_envelope_argument);
                    if is_replay_envelope {
                        self.replay_envelope_decoders += 1;
                    } else {
                        self.invalid_decoder_calls += 1;
                    }
                }
                if segments.last().map(String::as_str) == Some("compile_instance") {
                    self.public_ingress_calls += 1;
                }
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_array(&mut self, expression: &'ast syn::ExprArray) {
            if let Some(key) = reconstructed_manifest_key(expression) {
                self.reconstructed_manifest_keys.insert(key);
            }
            syn::visit::visit_expr_array(self, expression);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            if let Some(name) = macro_manifest_surface(macro_) {
                self.macro_manifest_surfaces.insert(name);
            }
            syn::visit::visit_macro(self, macro_);
        }

        fn visit_lit_str(&mut self, literal: &'ast syn::LitStr) {
            if literal.value() == "bbnf.grammar-instance.v1" {
                self.grammar_instance_schema_literals += 1;
            }
            if matches!(
                literal.value().as_str(),
                "entry" | "logical_path" | "pratt_tables" | "trivia_regex"
            ) {
                self.manifest_field_literals.insert(literal.value());
            }
        }

        fn visit_lit_byte_str(&mut self, literal: &'ast syn::LitByteStr) {
            if literal.value() == b"bbnf.grammar-instance.v1" {
                self.grammar_instance_schema_literals += 1;
            }
            if matches!(
                literal.value().as_slice(),
                b"entry" | b"logical_path" | b"pratt_tables" | b"trivia_regex"
            ) {
                self.manifest_field_literals.insert(
                    String::from_utf8(literal.value())
                        .expect("manifest field byte literal is UTF-8"),
                );
            }
        }

        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            let name = ident.to_string();
            if matches!(
                name.as_str(),
                "Program"
                    | "SourceModule"
                    | "SourceRuleData"
                    | "UnknownEntry"
                    | "parse_module"
                    | "parse_source"
            ) {
                self.source_dependent_identifiers.insert(name.clone());
            }
            if matches!(
                name.as_str(),
                "InvalidGrammarInstance" | "InvalidPrattTable" | "UnknownEntry"
            ) {
                self.diagnostic_identifiers.insert(name);
            }
        }
    }

    let syntax = syn::parse_file(source).expect("ownership source is valid Rust");
    struct DecoderImports {
        roots: BTreeSet<String>,
        aliases: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for DecoderImports {
        fn visit_item_use(&mut self, import: &'ast syn::ItemUse) {
            collect_serde_decoder_imports(
                &import.tree,
                &mut Vec::new(),
                &mut self.roots,
                &mut self.aliases,
            );
            syn::visit::visit_item_use(self, import);
        }

        fn visit_item_extern_crate(&mut self, import: &'ast syn::ItemExternCrate) {
            if matches!(import.ident.to_string().as_str(), "serde" | "serde_json") {
                self.roots.insert(import.rename.as_ref().map_or_else(
                    || import.ident.to_string(),
                    |(_, rename)| rename.to_string(),
                ));
            }
            syn::visit::visit_item_extern_crate(self, import);
        }

        fn visit_item_type(&mut self, alias: &'ast syn::ItemType) {
            if matches!(
                alias.ty.as_ref(),
                syn::Type::Path(path)
                    if path.qself.is_none()
                        && path.path.segments.first().is_some_and(|first| {
                            let first = first.ident.to_string();
                            self.roots.contains(&first)
                                && (first != "serde" && first != "serde_json"
                                    || path.path.segments.iter().any(|segment| {
                                        JSON_DECODER_TYPES
                                            .contains(&segment.ident.to_string().as_str())
                                    }))
                        })
            ) {
                self.roots.insert(alias.ident.to_string());
            }
            syn::visit::visit_item_type(self, alias);
        }

        fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
            if let syn::Expr::Path(path) = item.expr.as_ref()
                && serde_decoder_name(&path.path, &self.roots, &self.aliases).is_some()
            {
                self.aliases.insert(item.ident.to_string());
            }
            syn::visit::visit_item_const(self, item);
        }

        fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
            if let syn::Expr::Path(path) = item.expr.as_ref()
                && serde_decoder_name(&path.path, &self.roots, &self.aliases).is_some()
            {
                self.aliases.insert(item.ident.to_string());
            }
            syn::visit::visit_item_static(self, item);
        }
    }
    let mut imports = DecoderImports {
        roots: BTreeSet::from(["serde".to_owned(), "serde_json".to_owned()]),
        aliases: BTreeSet::new(),
    };
    syn::visit::Visit::visit_file(&mut imports, &syntax);
    // A second pass resolves const/static callable aliases even when their
    // `use ... as ...` authority appears later in the file.
    syn::visit::Visit::visit_file(&mut imports, &syntax);
    let mut ownership = Ownership {
        decoder_calls: 0,
        typed_instance_decoder_calls: 0,
        invalid_decoder_calls: 0,
        replay_envelope_decoders: 0,
        grammar_instance_definitions: 0,
        five_field_models: 0,
        grammar_instance_five_field_models: 0,
        grammar_instance_schema_literals: 0,
        manifest_field_literals: BTreeSet::new(),
        public_ingress_calls: 0,
        reconstructed_manifest_keys: BTreeSet::new(),
        macro_manifest_surfaces: BTreeSet::new(),
        source_dependent_identifiers: BTreeSet::new(),
        diagnostic_identifiers: BTreeSet::new(),
        decoder_roots: imports.roots,
        decoder_aliases: imports.aliases,
        current_function: None,
    };
    ownership.visit_file(&syntax);
    let is_instance = relative == "crates/core/src/compiler/instance.rs";
    let is_syntax = relative == "crates/core/src/compiler/syntax.rs";
    let is_replay = relative == "crates/core/src/compiler/replay.rs";
    let mut findings = BTreeSet::new();
    if !is_instance && ownership.grammar_instance_definitions != 0 {
        findings.insert("duplicate GrammarInstance model".to_owned());
    }
    if !is_instance && ownership.five_field_models != 0 {
        findings.insert("duplicate five-field grammar-instance model".to_owned());
    }
    if !is_instance && ownership.grammar_instance_schema_literals != 0 {
        findings.insert("duplicate grammar-instance schema authority".to_owned());
    }
    if !is_instance && !ownership.reconstructed_manifest_keys.is_empty() {
        findings.insert(format!(
            "reconstructed manifest key authority outside instance.rs {:?}",
            ownership.reconstructed_manifest_keys
        ));
    }
    if !is_instance && !ownership.macro_manifest_surfaces.is_empty() {
        findings.insert(format!(
            "macro-hidden manifest decoder/model authority outside instance.rs {:?}",
            ownership.macro_manifest_surfaces
        ));
    }
    if !is_instance && !is_replay && !ownership.manifest_field_literals.is_empty() {
        findings.insert(format!(
            "manifest field vocabulary outside instance.rs {:?}",
            ownership.manifest_field_literals
        ));
    }
    if is_instance && ownership.grammar_instance_definitions != 1 {
        findings.insert(format!(
            "instance.rs GrammarInstance definition count is {}",
            ownership.grammar_instance_definitions
        ));
    }
    if is_instance
        && (ownership.five_field_models != 1 || ownership.grammar_instance_five_field_models != 1)
    {
        findings.insert(format!(
            "instance.rs five-field manifest-model count is total={} GrammarInstance={}, expected 1/1",
            ownership.five_field_models, ownership.grammar_instance_five_field_models
        ));
    }
    if is_instance && (ownership.decoder_calls != 1 || ownership.typed_instance_decoder_calls != 1)
    {
        findings.insert(format!(
            "instance.rs decoder census is total={} typed-GrammarInstance={}, expected 1/1",
            ownership.decoder_calls, ownership.typed_instance_decoder_calls
        ));
    }
    if !is_instance && !is_replay && ownership.decoder_calls != 0 {
        findings.insert("manifest decoder outside instance.rs".to_owned());
    }
    if is_replay && ownership.invalid_decoder_calls != 0 {
        findings.insert("replay contains a non-envelope JSON decoder".to_owned());
    }
    if is_replay && ownership.replay_envelope_decoders != 2 {
        findings.insert(format!(
            "replay Value-envelope decoder count is {}, expected supplied plus canonical",
            ownership.replay_envelope_decoders
        ));
    }
    if is_replay && !ownership.manifest_field_literals.is_empty() {
        findings.insert(format!(
            "replay manually decodes grammar-instance fields {:?}",
            ownership.manifest_field_literals
        ));
    }
    if is_replay && ownership.public_ingress_calls != 0 {
        findings.insert("replay crosses the public compile_instance ingress".to_owned());
    }
    if is_instance && !ownership.source_dependent_identifiers.is_empty() {
        findings.insert(format!(
            "instance.rs owns source-dependent syntax identifiers {:?}",
            ownership.source_dependent_identifiers
        ));
    }
    if is_instance
        && !ownership
            .diagnostic_identifiers
            .contains("InvalidGrammarInstance")
    {
        findings.insert("instance.rs does not own InvalidGrammarInstance validation".to_owned());
    }
    if is_syntax
        && !["InvalidPrattTable", "UnknownEntry"]
            .into_iter()
            .all(|name| ownership.diagnostic_identifiers.contains(name))
    {
        findings.insert("syntax.rs lacks source-dependent entry/Pratt diagnostics".to_owned());
    }
    findings
}

fn collect_use_paths(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    paths: &mut BTreeSet<Vec<String>>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            collect_use_paths(&path.tree, prefix, paths);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix.clone();
            path.push(name.ident.to_string());
            paths.insert(path);
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_paths(item, prefix, paths);
            }
        }
        syn::UseTree::Rename(_) | syn::UseTree::Glob(_) => {}
    }
}

fn collect_nominal_alias_edges(items: &[syn::Item], edges: &mut Vec<(String, String)>) {
    use syn::visit::Visit as _;

    fn collect_use_edges(tree: &syn::UseTree, edges: &mut Vec<(String, String)>) {
        match tree {
            syn::UseTree::Path(path) => {
                collect_use_edges(&path.tree, edges);
            }
            syn::UseTree::Rename(rename) => {
                let original = rename.ident.to_string();
                let renamed = rename.rename.to_string();
                edges.push((original, renamed));
            }
            syn::UseTree::Group(group) => {
                for tree in &group.items {
                    collect_use_edges(tree, edges);
                }
            }
            syn::UseTree::Name(_) | syn::UseTree::Glob(_) => {}
        }
    }

    fn terminal_type_name(ty: &syn::Type) -> Option<String> {
        match ty {
            syn::Type::Group(group) => terminal_type_name(&group.elem),
            syn::Type::Paren(paren) => terminal_type_name(&paren.elem),
            syn::Type::Path(path) if path.qself.is_none() => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        }
    }

    struct Collector {
        edges: Vec<(String, String)>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Collector {
        fn visit_item_use(&mut self, import: &'ast syn::ItemUse) {
            collect_use_edges(&import.tree, &mut self.edges);
            syn::visit::visit_item_use(self, import);
        }

        fn visit_item_type(&mut self, alias: &'ast syn::ItemType) {
            if let Some(original) = terminal_type_name(&alias.ty) {
                self.edges.push((original, alias.ident.to_string()));
            }
            syn::visit::visit_item_type(self, alias);
        }
    }

    let mut collector = Collector { edges: Vec::new() };
    for item in items {
        collector.visit_item(item);
    }
    edges.extend(collector.edges);
}

fn nominal_aliases(syntax: &syn::File, canonical: &str) -> BTreeSet<String> {
    let mut edges = Vec::new();
    collect_nominal_alias_edges(&syntax.items, &mut edges);
    let mut aliases = BTreeSet::from([canonical.to_owned()]);
    loop {
        let mut changed = false;
        for (original, renamed) in &edges {
            if aliases.contains(original) {
                changed |= aliases.insert(renamed.clone());
            }
        }
        if !changed {
            return aliases;
        }
    }
}

fn type_is_nominal_alias(ty: &syn::Type, aliases: &BTreeSet<String>) -> bool {
    match ty {
        syn::Type::Group(group) => type_is_nominal_alias(&group.elem, aliases),
        syn::Type::Paren(paren) => type_is_nominal_alias(&paren.elem, aliases),
        syn::Type::Reference(reference) => type_is_nominal_alias(&reference.elem, aliases),
        syn::Type::Path(path) if path.qself.is_none() => path
            .path
            .segments
            .last()
            .is_some_and(|segment| aliases.contains(&segment.ident.to_string())),
        _ => false,
    }
}

fn type_mentions_nominal_alias(ty: &syn::Type, aliases: &BTreeSet<String>) -> bool {
    use syn::visit::Visit as _;

    struct Mentions<'a> {
        aliases: &'a BTreeSet<String>,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for Mentions<'_> {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.found |= self.aliases.contains(&ident.to_string());
        }
    }

    let mut mentions = Mentions {
        aliases,
        found: false,
    };
    mentions.visit_type(ty);
    mentions.found
}

fn return_type_mentions_nominal_alias(
    output: &syn::ReturnType,
    aliases: &BTreeSet<String>,
) -> bool {
    matches!(
        output,
        syn::ReturnType::Type(_, ty) if type_mentions_nominal_alias(ty, aliases)
    )
}

fn expression_mentions_any_ident(expression: &syn::Expr, expected: &BTreeSet<String>) -> bool {
    expected
        .iter()
        .any(|name| expression_mentions_ident(expression, name))
}

fn direct_tainted_binding(expression: &syn::Expr, tainted: &BTreeSet<String>) -> Option<String> {
    match expression {
        syn::Expr::Group(group) => direct_tainted_binding(&group.expr, tainted),
        syn::Expr::Paren(paren) => direct_tainted_binding(&paren.expr, tainted),
        syn::Expr::Path(path) if path.qself.is_none() => path
            .path
            .get_ident()
            .map(ToString::to_string)
            .filter(|name| tainted.contains(name)),
        syn::Expr::Reference(reference) => direct_tainted_binding(&reference.expr, tainted),
        _ => None,
    }
}

fn expression_contains_whole_tainted_binding(
    expression: &syn::Expr,
    tainted: &BTreeSet<String>,
) -> bool {
    use syn::visit::Visit as _;

    struct WholeBinding<'a> {
        tainted: &'a BTreeSet<String>,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for WholeBinding<'_> {
        fn visit_expr_field(&mut self, _field: &'ast syn::ExprField) {
            // A selected byte field is checked separately by the raw-field
            // census; its base is not a whole-carrier escape.
        }

        fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
            self.found |= path.qself.is_none()
                && path
                    .path
                    .get_ident()
                    .is_some_and(|ident| self.tainted.contains(&ident.to_string()));
            syn::visit::visit_expr_path(self, path);
        }
    }

    let mut whole = WholeBinding {
        tainted,
        found: false,
    };
    whole.visit_expr(expression);
    whole.found
}

fn exact_shared_instance_borrow(ty: &syn::Type) -> bool {
    matches!(
        ty,
        syn::Type::Reference(reference)
            if reference.lifetime.is_none()
                && reference.mutability.is_none()
                && matches!(
                    reference.elem.as_ref(),
                    syn::Type::Path(path)
                        if path.qself.is_none()
                            && path.path.leading_colon.is_none()
                            && path.path.segments.len() == 1
                            && path.path.is_ident("GrammarInstance")
                            && matches!(
                                path.path.segments[0].arguments,
                                syn::PathArguments::None
                            )
                )
    )
}

fn expression_mentions_ident(expression: &syn::Expr, expected: &str) -> bool {
    use syn::visit::Visit as _;

    struct Mentions<'a> {
        expected: &'a str,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for Mentions<'_> {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.found |= ident == self.expected;
        }
    }

    let mut mentions = Mentions {
        expected,
        found: false,
    };
    mentions.visit_expr(expression);
    mentions.found
}

fn block_mentions_ident(block: &syn::Block, expected: &str) -> bool {
    use syn::visit::Visit as _;

    struct Mentions<'a> {
        expected: &'a str,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for Mentions<'_> {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.found |= ident == self.expected;
        }
    }

    let mut mentions = Mentions {
        expected,
        found: false,
    };
    mentions.visit_block(block);
    mentions.found
}

fn expression_mentions_binding(expression: &syn::Expr, bindings: &BTreeSet<String>) -> bool {
    use syn::visit::Visit as _;

    struct Mentions<'a> {
        bindings: &'a BTreeSet<String>,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for Mentions<'_> {
        fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
            self.found |= path.qself.is_none()
                && path
                    .path
                    .get_ident()
                    .is_some_and(|ident| self.bindings.contains(&ident.to_string()));
            syn::visit::visit_expr_path(self, path);
        }

        fn visit_member(&mut self, _member: &'ast syn::Member) {
            // `module.rules` does not mention a local `rules` carrier. Only
            // exact expression-path bindings seed alias provenance.
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            let words = macro_
                .tokens
                .to_string()
                .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                .filter(|word| !word.is_empty())
                .map(str::to_owned)
                .collect::<BTreeSet<_>>();
            self.found |= self.bindings.iter().any(|binding| words.contains(binding));
            syn::visit::visit_macro(self, macro_);
        }
    }

    let mut mentions = Mentions {
        bindings,
        found: false,
    };
    mentions.visit_expr(expression);
    mentions.found
}

fn carrier_alias_closure(block: &syn::Block, initial: BTreeSet<String>) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    struct Aliases<'a> {
        known: &'a BTreeSet<String>,
        discovered: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for Aliases<'_> {
        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(syn::LocalInit { expr, .. }) = &local.init
                && expression_mentions_binding(expr, self.known)
            {
                self.discovered.extend(bound_identifiers(&local.pat));
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_assign(&mut self, assign: &'ast syn::ExprAssign) {
            if expression_mentions_binding(&assign.right, self.known) {
                struct Assigned(BTreeSet<String>);
                impl<'ast> syn::visit::Visit<'ast> for Assigned {
                    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
                        if path.qself.is_none()
                            && let Some(ident) = path.path.get_ident()
                        {
                            self.0.insert(ident.to_string());
                        }
                        syn::visit::visit_expr_path(self, path);
                    }

                    fn visit_member(&mut self, _member: &'ast syn::Member) {}
                }
                let mut assigned = Assigned(BTreeSet::new());
                assigned.visit_expr(&assign.left);
                self.discovered.extend(assigned.0);
            }
            syn::visit::visit_expr_assign(self, assign);
        }
    }

    let mut aliases = initial;
    loop {
        let mut pass = Aliases {
            known: &aliases,
            discovered: BTreeSet::new(),
        };
        pass.visit_block(block);
        let before = aliases.len();
        aliases.extend(pass.discovered);
        if aliases.len() == before {
            return aliases;
        }
    }
}

fn direct_named_field(expression: &syn::Expr, receiver: &str, member: &str) -> bool {
    matches!(
        expression,
        syn::Expr::Field(field)
            if matches!(
                field.base.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none() && path.path.is_ident(receiver)
            ) && matches!(
                &field.member,
                syn::Member::Named(ident) if ident == member
            )
    )
}

fn bound_identifiers(pattern: &syn::Pat) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct Bindings(BTreeSet<String>);
    impl<'ast> syn::visit::Visit<'ast> for Bindings {
        fn visit_pat_ident(&mut self, binding: &'ast syn::PatIdent) {
            self.0.insert(binding.ident.to_string());
            syn::visit::visit_pat_ident(self, binding);
        }
    }

    let mut bindings = Bindings::default();
    bindings.visit_pat(pattern);
    bindings.0
}

fn expression_has_instance_accessor(expression: &syn::Expr, expected: &str) -> bool {
    use syn::visit::Visit as _;

    struct Accessor<'a> {
        expected: &'a str,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for Accessor<'_> {
        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.found |= call.method == self.expected
                && matches!(
                    call.receiver.as_ref(),
                    syn::Expr::Path(path)
                        if path.qself.is_none() && path.path.is_ident("instance")
                );
            syn::visit::visit_expr_method_call(self, call);
        }
    }

    let mut accessor = Accessor {
        expected,
        found: false,
    };
    accessor.visit_expr(expression);
    accessor.found
}

fn expression_has_rule_name(expression: &syn::Expr) -> bool {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct RuleName(bool);
    impl<'ast> syn::visit::Visit<'ast> for RuleName {
        fn visit_expr_field(&mut self, field: &'ast syn::ExprField) {
            self.0 |= matches!(
                &field.member,
                syn::Member::Named(ident) if ident == "name"
            );
            syn::visit::visit_expr_field(self, field);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.0 |= call.method == "name" && call.args.is_empty();
            syn::visit::visit_expr_method_call(self, call);
        }
    }

    let mut name = RuleName::default();
    name.visit_expr(expression);
    name.0
}

fn block_tail_named_call_count(block: &syn::Block, expected: &str) -> usize {
    use syn::visit::Visit as _;

    struct Calls<'a> {
        expected: &'a str,
        count: usize,
    }
    impl<'ast> syn::visit::Visit<'ast> for Calls<'_> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            self.count += usize::from(matches!(
                call.func.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none()
                        && path
                            .path
                            .segments
                            .last()
                            .is_some_and(|segment| segment.ident == self.expected)
            ));
            syn::visit::visit_expr_call(self, call);
        }
    }

    let Some(syn::Stmt::Expr(expression, None)) = block.stmts.last() else {
        return 0;
    };
    let expression = match expression {
        syn::Expr::Return(return_) => {
            let Some(expression) = return_.expr.as_deref() else {
                return 0;
            };
            expression
        }
        expression => expression,
    };
    let mut calls = Calls { expected, count: 0 };
    calls.visit_expr(expression);
    calls.count
}

fn direct_expression_binding(expression: &syn::Expr) -> Option<String> {
    match expression {
        syn::Expr::Path(path) if path.qself.is_none() => {
            path.path.get_ident().map(ToString::to_string)
        }
        syn::Expr::Paren(paren) => direct_expression_binding(&paren.expr),
        _ => None,
    }
}

fn syntax_has_real_instance_resolution(syntax: &syn::File) -> bool {
    use syn::visit::Visit as _;

    fn exact_path(expression: &syn::Expr, expected: &[&str]) -> bool {
        matches!(
            expression,
            syn::Expr::Path(path)
                if path.qself.is_none()
                    && path.path.segments.len() == expected.len()
                    && path.path.segments.iter().zip(expected).all(
                        |(segment, expected)| {
                            segment.ident == *expected
                                && matches!(segment.arguments, syn::PathArguments::None)
                        }
                    )
        )
    }

    fn exact_plain_binding(pattern: &syn::Pat, expected: &str) -> bool {
        matches!(
            pattern,
            syn::Pat::Ident(binding)
                if binding.ident == expected
                    && binding.attrs.is_empty()
                    && binding.by_ref.is_none()
                    && binding.mutability.is_none()
                    && binding.subpat.is_none()
        )
    }

    fn exact_shared_borrow(ty: &syn::Type, expected: &str) -> bool {
        matches!(
            ty,
            syn::Type::Reference(reference)
                if reference.lifetime.is_none()
                    && reference.mutability.is_none()
                    && matches!(
                        reference.elem.as_ref(),
                        syn::Type::Path(path)
                            if path.qself.is_none()
                                && path.path.leading_colon.is_none()
                                && path.path.segments.len() == 1
                                && path.path.segments[0].ident == expected
                                && matches!(
                                    path.path.segments[0].arguments,
                                    syn::PathArguments::None
                                )
                    )
        )
    }

    let mut imports = BTreeSet::new();
    for item in &syntax.items {
        if let syn::Item::Use(import) = item {
            collect_use_paths(&import.tree, &mut Vec::new(), &mut imports);
        }
    }
    if !imports.contains(&vec![
        "super".to_owned(),
        "instance".to_owned(),
        "GrammarInstance".to_owned(),
    ]) {
        return false;
    }
    let invalid_pratt_helpers = syntax
        .items
        .iter()
        .filter(|item| {
            let syn::Item::Fn(function) = item else {
                return false;
            };
            if function.sig.ident != "invalid_pratt_table"
                || !matches!(function.vis, syn::Visibility::Inherited)
                || function.sig.constness.is_some()
                || function.sig.asyncness.is_some()
                || function.sig.unsafety.is_some()
                || function.sig.abi.is_some()
                || function.sig.variadic.is_some()
                || !function.sig.generics.params.is_empty()
                || function.sig.generics.where_clause.is_some()
            {
                return false;
            }
            let [syn::FnArg::Typed(message)] =
                function.sig.inputs.iter().collect::<Vec<_>>().as_slice()
            else {
                return false;
            };
            let message_binding = matches!(
                message.pat.as_ref(),
                syn::Pat::Ident(binding)
                    if binding.ident == "message"
                        && binding.attrs.is_empty()
                        && binding.by_ref.is_none()
                        && binding.mutability.is_none()
                        && binding.subpat.is_none()
            );
            let message_type = matches!(
                message.ty.as_ref(),
                syn::Type::ImplTrait(impl_trait)
                    if impl_trait.bounds.len() == 1
                        && matches!(
                            impl_trait.bounds.first(),
                            Some(syn::TypeParamBound::Trait(bound))
                                if bound.lifetimes.is_none()
                                    && matches!(bound.modifier, syn::TraitBoundModifier::None)
                                    && bound.path.segments.len() == 1
                                    && bound.path.segments[0].ident == "Into"
                                    && matches!(
                                        &bound.path.segments[0].arguments,
                                        syn::PathArguments::AngleBracketed(arguments)
                                            if arguments.args.len() == 1
                                                && matches!(
                                                    arguments.args.first(),
                                                    Some(syn::GenericArgument::Type(
                                                        syn::Type::Path(path)
                                                    )) if path.qself.is_none()
                                                        && path.path.is_ident("String")
                                                )
                                    )
                        )
            );
            let returns_diagnostic = matches!(
                &function.sig.output,
                syn::ReturnType::Type(_, ty)
                    if matches!(
                        ty.as_ref(),
                        syn::Type::Path(path)
                            if path.qself.is_none() && path.path.is_ident("Diagnostic")
                    )
            );
            let [syn::Stmt::Expr(syn::Expr::Call(diagnostic), None)] =
                function.block.stmts.as_slice()
            else {
                return false;
            };
            let exact_zero = |expression: &syn::Expr| {
                matches!(
                    expression,
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Int(integer),
                        ..
                    }) if integer.suffix().is_empty() && integer.base10_digits() == "0"
                )
            };
            message_binding
                && message_type
                && returns_diagnostic
                && exact_path(&diagnostic.func, &["Diagnostic", "new"])
                && diagnostic.args.len() == 4
                && diagnostic
                    .args
                    .first()
                    .is_some_and(|code| exact_path(code, &["DiagnosticCode", "InvalidPrattTable"]))
                && diagnostic.args.iter().nth(1).is_some_and(exact_zero)
                && diagnostic.args.iter().nth(2).is_some_and(exact_zero)
                && diagnostic.args.last().is_some_and(|argument| {
                    matches!(
                        argument,
                        syn::Expr::Path(path)
                            if path.qself.is_none() && path.path.is_ident("message")
                    )
                })
        })
        .count();

    syntax.items.iter().any(|item| {
        let syn::Item::Impl(item) = item else {
            return false;
        };
        if !matches!(
            item.self_ty.as_ref(),
            syn::Type::Path(path)
                if path.qself.is_none()
                    && path.path.segments.last().is_some_and(
                        |segment| segment.ident == "Program"
                    )
        ) {
            return false;
        }
        item.items.iter().any(|item| {
            let syn::ImplItem::Fn(method) = item else {
                return false;
            };
            if method.sig.ident != "resolve"
                || !method.attrs.is_empty()
                || !matches!(method.vis, syn::Visibility::Restricted(ref restricted)
                    if restricted.path.is_ident("super"))
                || method.sig.constness.is_some()
                || method.sig.asyncness.is_some()
                || method.sig.unsafety.is_some()
                || method.sig.abi.is_some()
                || method.sig.variadic.is_some()
                || !method.sig.generics.params.is_empty()
                || method.sig.generics.where_clause.is_some()
            {
                return false;
            }
            let has_module = method.sig.inputs.iter().any(|argument| {
                matches!(
                    argument,
                    syn::FnArg::Typed(argument)
                        if argument.attrs.is_empty()
                            && exact_plain_binding(&argument.pat, "module")
                            && exact_shared_borrow(&argument.ty, "SourceModule")
                )
            });
            let has_instance = method.sig.inputs.iter().any(|argument| {
                matches!(
                    argument,
                    syn::FnArg::Typed(argument)
                        if argument.attrs.is_empty()
                            && exact_plain_binding(&argument.pat, "instance")
                            && exact_shared_instance_borrow(&argument.ty)
                )
            });
            if !has_module || !has_instance || method.sig.inputs.len() != 2 {
                return false;
            }
            if !matches!(
                &method.sig.output,
                syn::ReturnType::Type(_, ty)
                    if exact_generic_ident(ty, "StageResult", "Self")
            ) {
                return false;
            }

            #[derive(Default)]
            struct Locals {
                entry_tainted: BTreeSet<String>,
                resolved_entry_tainted: BTreeSet<String>,
                rule_name_tainted: BTreeSet<String>,
                pratt_tainted: BTreeSet<String>,
                name_maps: BTreeSet<String>,
                entry_lookup_maps: BTreeSet<String>,
                name_map_population_maps: BTreeSet<String>,
                name_map_mutations: std::collections::BTreeMap<String, usize>,
                entry_calls: usize,
                entry_equalities: usize,
                entry_lookups: usize,
                pratt_calls: usize,
                entry_overwrites: usize,
            }
            impl Locals {
                fn mentions_any(expression: &syn::Expr, names: &BTreeSet<String>) -> bool {
                    names
                        .iter()
                        .any(|name| expression_mentions_ident(expression, name))
                }

                fn exact_binding_name(
                    expression: &syn::Expr,
                    names: &BTreeSet<String>,
                ) -> Option<String> {
                    match expression {
                        syn::Expr::Group(group) => Self::exact_binding_name(&group.expr, names),
                        syn::Expr::Paren(paren) => Self::exact_binding_name(&paren.expr, names),
                        syn::Expr::Path(path) if path.qself.is_none() => path
                            .path
                            .get_ident()
                            .map(ToString::to_string)
                            .filter(|ident| names.contains(ident)),
                        _ => None,
                    }
                }

                fn exact_binding(expression: &syn::Expr, names: &BTreeSet<String>) -> bool {
                    Self::exact_binding_name(expression, names).is_some()
                }

                fn exact_path(expression: &syn::Expr, expected: &[&str]) -> bool {
                    matches!(
                        expression,
                        syn::Expr::Path(path)
                            if path.qself.is_none()
                                && path.path.segments.len() == expected.len()
                                && path.path.segments.iter().zip(expected).all(
                                    |(segment, expected)| {
                                        segment.ident == *expected
                                            && matches!(
                                                segment.arguments,
                                                syn::PathArguments::None
                                            )
                                    }
                                )
                    )
                }

                fn exact_instance_entry(expression: &syn::Expr) -> bool {
                    matches!(
                        expression,
                        syn::Expr::MethodCall(call)
                            if call.method == "entry"
                                && call.args.is_empty()
                                && matches!(
                                    call.receiver.as_ref(),
                                    syn::Expr::Path(path)
                                        if path.qself.is_none()
                                            && path.path.is_ident("instance")
                                )
                    )
                }

                fn exact_rule_name(expression: &syn::Expr) -> bool {
                    match expression {
                        syn::Expr::Field(field)
                            if matches!(
                                &field.member,
                                syn::Member::Named(member) if member == "name"
                            ) =>
                        {
                            matches!(
                                field.base.as_ref(),
                                syn::Expr::Path(path)
                                    if path.qself.is_none()
                                        && path.path.get_ident().is_some()
                            )
                        }
                        syn::Expr::MethodCall(call)
                            if call.method == "name" && call.args.is_empty() =>
                        {
                            matches!(
                                call.receiver.as_ref(),
                                syn::Expr::Path(path)
                                    if path.qself.is_none()
                                        && path.path.get_ident().is_some()
                            )
                        }
                        syn::Expr::Group(group) => Self::exact_rule_name(&group.expr),
                        syn::Expr::Paren(paren) => Self::exact_rule_name(&paren.expr),
                        _ => false,
                    }
                }

                fn exact_name_lookup(&self, expression: &syn::Expr) -> Option<String> {
                    let syn::Expr::MethodCall(call) = expression else {
                        return None;
                    };
                    (call.method == "get"
                        && call.args.len() == 1
                        && call
                            .args
                            .first()
                            .is_some_and(|key| Self::exact_binding(key, &self.entry_tainted)))
                    .then(|| Self::exact_binding_name(&call.receiver, &self.name_maps))
                    .flatten()
                }

                fn exact_unknown_entry_else(init: &syn::LocalInit) -> bool {
                    let Some((_, alternative)) = &init.diverge else {
                        return false;
                    };
                    let syn::Expr::Block(block) = alternative.as_ref() else {
                        return false;
                    };
                    let [syn::Stmt::Expr(syn::Expr::Return(return_), _)] =
                        block.block.stmts.as_slice()
                    else {
                        return false;
                    };
                    let Some(syn::Expr::Call(rejected)) = return_.expr.as_deref() else {
                        return false;
                    };
                    if !Self::exact_path(&rejected.func, &["StageResult", "rejected"])
                        || rejected.args.len() != 1
                    {
                        return false;
                    }
                    let Some(syn::Expr::Call(diagnostic)) = rejected.args.first() else {
                        return false;
                    };
                    Self::exact_path(&diagnostic.func, &["Diagnostic", "new"])
                        && diagnostic.args.len() == 4
                        && diagnostic.args.first().is_some_and(|code| {
                            Self::exact_path(code, &["DiagnosticCode", "UnknownEntry"])
                        })
                }

                fn entry_equality_count(&self, expression: &syn::Expr) -> usize {
                    struct Equalities<'a> {
                        entry_tainted: &'a BTreeSet<String>,
                        rule_name_tainted: &'a BTreeSet<String>,
                        count: usize,
                    }
                    impl<'ast> syn::visit::Visit<'ast> for Equalities<'_> {
                        fn visit_expr_binary(&mut self, binary: &'ast syn::ExprBinary) {
                            if matches!(binary.op, syn::BinOp::Eq(_)) {
                                let left_entry =
                                    Locals::exact_binding(&binary.left, self.entry_tainted)
                                        || Locals::exact_instance_entry(&binary.left);
                                let right_entry =
                                    Locals::exact_binding(&binary.right, self.entry_tainted)
                                        || Locals::exact_instance_entry(&binary.right);
                                let left_name =
                                    Locals::exact_binding(&binary.left, self.rule_name_tainted)
                                        || Locals::exact_rule_name(&binary.left);
                                let right_name =
                                    Locals::exact_binding(&binary.right, self.rule_name_tainted)
                                        || Locals::exact_rule_name(&binary.right);
                                self.count += usize::from(
                                    (left_entry && right_name) || (right_entry && left_name),
                                );
                            }
                            syn::visit::visit_expr_binary(self, binary);
                        }
                    }

                    let mut equalities = Equalities {
                        entry_tainted: &self.entry_tainted,
                        rule_name_tainted: &self.rule_name_tainted,
                        count: 0,
                    };
                    equalities.visit_expr(expression);
                    equalities.count
                }
            }
            impl<'ast> syn::visit::Visit<'ast> for Locals {
                fn visit_local(&mut self, local: &'ast syn::Local) {
                    if let Some(init) = &local.init
                        && let Some(map) = self.exact_name_lookup(&init.expr)
                        && Self::exact_unknown_entry_else(init)
                    {
                        let resolved = bound_identifiers(&local.pat);
                        if resolved.len() == 1 {
                            self.resolved_entry_tainted.extend(resolved);
                            self.entry_lookup_maps.insert(map);
                            self.entry_lookups += 1;
                        }
                    }
                    let binding = match &local.pat {
                        syn::Pat::Ident(binding) => {
                            Some((binding.ident.to_string(), binding.mutability.is_some()))
                        }
                        syn::Pat::Type(typed) => match typed.pat.as_ref() {
                            syn::Pat::Ident(binding) => {
                                Some((binding.ident.to_string(), binding.mutability.is_some()))
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                    if let (Some((binding, mutable)), Some(syn::LocalInit { expr, .. })) =
                        (binding, &local.init)
                    {
                        if self.name_maps.contains(&binding) {
                            *self.name_map_mutations.entry(binding.clone()).or_default() += 1;
                        }
                        let name_map = matches!(
                            expr.as_ref(),
                            syn::Expr::Call(call)
                                if Self::exact_path(&call.func, &["BTreeMap", "new"])
                                    && call.args.is_empty()
                        );
                        if name_map {
                            self.name_maps.insert(binding.clone());
                        }
                        self.entry_overwrites +=
                            usize::from(self.resolved_entry_tainted.contains(&binding));
                        let resolved_entry = self.entry_equality_count(expr) != 0
                            || Self::mentions_any(expr, &self.resolved_entry_tainted);
                        if expression_has_instance_accessor(expr, "entry")
                            || Self::mentions_any(expr, &self.entry_tainted)
                        {
                            self.entry_tainted.insert(binding.clone());
                        }
                        if expression_has_rule_name(expr)
                            || Self::mentions_any(expr, &self.rule_name_tainted)
                        {
                            self.rule_name_tainted.insert(binding.clone());
                        }
                        if resolved_entry {
                            self.entry_overwrites += usize::from(mutable);
                            self.resolved_entry_tainted.insert(binding.clone());
                        }
                        if expression_has_instance_accessor(expr, "pratt_tables")
                            || Self::mentions_any(expr, &self.pratt_tainted)
                        {
                            self.pratt_tainted.insert(binding);
                        }
                    }
                    syn::visit::visit_local(self, local);
                }

                fn visit_expr_for_loop(&mut self, loop_: &'ast syn::ExprForLoop) {
                    let exact_module_rules = matches!(
                        loop_.expr.as_ref(),
                        syn::Expr::Reference(reference)
                            if reference.mutability.is_none()
                                && matches!(
                                    reference.expr.as_ref(),
                                    syn::Expr::Field(field)
                                        if matches!(
                                            field.base.as_ref(),
                                            syn::Expr::Path(path)
                                                if path.qself.is_none()
                                                    && path.path.is_ident("module")
                                        )
                                            && matches!(
                                                &field.member,
                                                syn::Member::Named(member) if member == "rules"
                                            )
                                )
                    );
                    if exact_module_rules {
                        fn exact_rule_field(
                            expression: &syn::Expr,
                            rules: &BTreeSet<String>,
                            member: &str,
                        ) -> bool {
                            matches!(
                                expression,
                                syn::Expr::Field(field)
                                    if matches!(
                                        &field.member,
                                        syn::Member::Named(field_member)
                                            if field_member == member
                                    )
                                        && Locals::exact_binding(&field.base, rules)
                            )
                        }

                        fn live_insert(expression: &syn::Expr) -> Option<&syn::ExprMethodCall> {
                            match expression {
                                syn::Expr::MethodCall(call) if call.method == "insert" => {
                                    Some(call)
                                }
                                syn::Expr::If(if_) => {
                                    let syn::Expr::MethodCall(is_some) = if_.cond.as_ref() else {
                                        return None;
                                    };
                                    if is_some.method != "is_some" || !is_some.args.is_empty() {
                                        return None;
                                    }
                                    let syn::Expr::MethodCall(insert) = is_some.receiver.as_ref()
                                    else {
                                        return None;
                                    };
                                    (insert.method == "insert").then_some(insert)
                                }
                                _ => None,
                            }
                        }

                        let rules = bound_identifiers(&loop_.pat);
                        let exact_maps = loop_
                            .body
                            .stmts
                            .iter()
                            .filter_map(|statement| match statement {
                                syn::Stmt::Expr(expression, _) => live_insert(expression),
                                _ => None,
                            })
                            .filter_map(|insert| {
                                let exact_name = insert.args.first().is_some_and(|name| {
                                    matches!(
                                        name,
                                        syn::Expr::MethodCall(clone)
                                            if clone.method == "clone"
                                                && clone.args.is_empty()
                                                && exact_rule_field(
                                                    &clone.receiver,
                                                    &rules,
                                                    "name"
                                                )
                                    )
                                });
                                let exact_id = insert
                                    .args
                                    .iter()
                                    .nth(1)
                                    .is_some_and(|id| exact_rule_field(id, &rules, "id"));
                                (insert.args.len() == 2 && exact_name && exact_id)
                                    .then(|| {
                                        Locals::exact_binding_name(
                                            &insert.receiver,
                                            &self.name_maps,
                                        )
                                    })
                                    .flatten()
                            })
                            .collect::<Vec<_>>();
                        if let [map] = exact_maps.as_slice() {
                            self.name_map_population_maps.insert(map.clone());
                        }
                    }
                    syn::visit::visit_expr_for_loop(self, loop_);
                }

                fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
                    let direct_instance = matches!(
                        call.receiver.as_ref(),
                        syn::Expr::Path(path)
                            if path.qself.is_none() && path.path.is_ident("instance")
                    );
                    self.entry_calls += usize::from(direct_instance && call.method == "entry");
                    self.pratt_calls +=
                        usize::from(direct_instance && call.method == "pratt_tables");
                    if matches!(
                        call.method.to_string().as_str(),
                        "append"
                            | "clear"
                            | "dedup"
                            | "extend"
                            | "get_mut"
                            | "insert"
                            | "last_mut"
                            | "pop"
                            | "push"
                            | "remove"
                            | "replace"
                            | "retain"
                            | "set"
                            | "sort"
                            | "sort_by"
                            | "sort_by_key"
                            | "swap"
                            | "swap_remove"
                            | "take"
                            | "truncate"
                    ) && let Some(map) =
                        Self::exact_binding_name(&call.receiver, &self.name_maps)
                    {
                        *self.name_map_mutations.entry(map).or_default() += 1;
                    }
                    if matches!(
                        call.method.to_string().as_str(),
                        "clear" | "get_mut" | "insert" | "replace" | "set" | "swap" | "take"
                    ) && Self::mentions_any(&call.receiver, &self.resolved_entry_tainted)
                    {
                        self.entry_overwrites += 1;
                    }
                    syn::visit::visit_expr_method_call(self, call);
                }

                fn visit_expr_assign(&mut self, assign: &'ast syn::ExprAssign) {
                    if let Some(map) = Self::exact_binding_name(&assign.left, &self.name_maps) {
                        *self.name_map_mutations.entry(map).or_default() += 1;
                    }
                    self.entry_overwrites += usize::from(Self::mentions_any(
                        &assign.left,
                        &self.resolved_entry_tainted,
                    ));
                    syn::visit::visit_expr_assign(self, assign);
                }

                fn visit_expr_reference(&mut self, reference: &'ast syn::ExprReference) {
                    self.entry_overwrites += usize::from(
                        reference.mutability.is_some()
                            && Self::mentions_any(&reference.expr, &self.resolved_entry_tainted),
                    );
                    syn::visit::visit_expr_reference(self, reference);
                }

                fn visit_expr_binary(&mut self, binary: &'ast syn::ExprBinary) {
                    if matches!(binary.op, syn::BinOp::Eq(_)) {
                        let left_entry = Self::exact_binding(&binary.left, &self.entry_tainted)
                            || Self::exact_instance_entry(&binary.left);
                        let right_entry = Self::exact_binding(&binary.right, &self.entry_tainted)
                            || Self::exact_instance_entry(&binary.right);
                        let left_name = Self::exact_binding(&binary.left, &self.rule_name_tainted)
                            || Self::exact_rule_name(&binary.left);
                        let right_name =
                            Self::exact_binding(&binary.right, &self.rule_name_tainted)
                                || Self::exact_rule_name(&binary.right);
                        self.entry_equalities +=
                            usize::from((left_entry && right_name) || (right_entry && left_name));
                    }
                    syn::visit::visit_expr_binary(self, binary);
                }
            }
            let mut locals = Locals::default();
            locals.visit_block(&method.block);

            #[derive(Default)]
            struct AcceptedSelf<'a> {
                rows: Vec<&'a syn::ExprStruct>,
            }
            impl<'ast> syn::visit::Visit<'ast> for AcceptedSelf<'ast> {
                fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
                    let accepted = matches!(
                        call.func.as_ref(),
                        syn::Expr::Path(path)
                            if path.qself.is_none()
                                && path.path.segments.last().is_some_and(
                                    |segment| segment.ident == "accepted"
                                )
                    );
                    if accepted
                        && call.args.len() == 1
                        && let Some(syn::Expr::Struct(record)) = call.args.first()
                        && record.path.is_ident("Self")
                    {
                        self.rows.push(record);
                    }
                    syn::visit::visit_expr_call(self, call);
                }
            }
            let mut accepted = AcceptedSelf::default();
            accepted.visit_block(&method.block);
            let [record] = accepted.rows.as_slice() else {
                return false;
            };
            let field_expression = |name: &str| {
                record
                    .fields
                    .iter()
                    .find(|field| {
                        matches!(
                            &field.member,
                            syn::Member::Named(ident) if ident == name
                        )
                    })
                    .map(|field| &field.expr)
            };
            let Some(entry_expression) = field_expression("entry") else {
                return false;
            };
            if !direct_expression_binding(entry_expression)
                .is_some_and(|name| locals.resolved_entry_tainted.contains(&name))
            {
                return false;
            }
            let Some(nodes_expression) = field_expression("nodes") else {
                return false;
            };
            let Some(rules_expression) = field_expression("rules") else {
                return false;
            };

            struct PrattAttachment<'a> {
                pratt_tainted: &'a BTreeSet<String>,
                node_names: BTreeSet<String>,
                rule_names: BTreeSet<String>,
                invalid_pratt_helper: bool,
                live_loops: usize,
            }
            impl<'ast> syn::visit::Visit<'ast> for PrattAttachment<'_> {
                fn visit_expr_for_loop(&mut self, loop_: &'ast syn::ExprForLoop) {
                    let table_driven =
                        expression_has_instance_accessor(&loop_.expr, "pratt_tables")
                            || self
                                .pratt_tainted
                                .iter()
                                .any(|name| expression_mentions_ident(&loop_.expr, name));
                    if table_driven {
                        #[derive(Clone, Copy, Eq, PartialEq)]
                        enum MutationEvent {
                            Node,
                            Rule,
                            AttachRoot,
                            CarrierAccess,
                        }

                        struct Body<'a> {
                            node_names: &'a BTreeSet<String>,
                            rule_names: &'a BTreeSet<String>,
                            table_names: BTreeSet<String>,
                            table_accessors: BTreeSet<String>,
                            exact_operator_loops: usize,
                            node_mutation: bool,
                            rule_mutation: bool,
                            pratt_node: bool,
                            invalid_pratt_helper: bool,
                            invalid_pratt: bool,
                            mutation_events: Vec<MutationEvent>,
                        }
                        impl<'ast> syn::visit::Visit<'ast> for Body<'_> {
                            fn visit_expr_assign(&mut self, assign: &'ast syn::ExprAssign) {
                                let node_mutation = self
                                    .node_names
                                    .iter()
                                    .any(|name| expression_mentions_ident(&assign.left, name));
                                let rule_mutation = self
                                    .rule_names
                                    .iter()
                                    .any(|name| expression_mentions_ident(&assign.left, name));
                                let root_attachment = rule_mutation
                                    && matches!(
                                        assign.left.as_ref(),
                                        syn::Expr::Field(field)
                                            if matches!(
                                                &field.member,
                                                syn::Member::Named(member) if member == "root"
                                            )
                                    );
                                self.visit_expr(&assign.left);
                                self.visit_expr(&assign.right);
                                self.node_mutation |= node_mutation;
                                self.rule_mutation |= rule_mutation;
                                if node_mutation {
                                    self.mutation_events.push(MutationEvent::Node);
                                }
                                if root_attachment {
                                    self.mutation_events.push(MutationEvent::AttachRoot);
                                } else if rule_mutation {
                                    self.mutation_events.push(MutationEvent::Rule);
                                }
                            }

                            fn visit_expr_binary(&mut self, binary: &'ast syn::ExprBinary) {
                                let compound_assignment = matches!(
                                    binary.op,
                                    syn::BinOp::AddAssign(_)
                                        | syn::BinOp::SubAssign(_)
                                        | syn::BinOp::MulAssign(_)
                                        | syn::BinOp::DivAssign(_)
                                        | syn::BinOp::RemAssign(_)
                                        | syn::BinOp::BitXorAssign(_)
                                        | syn::BinOp::BitAndAssign(_)
                                        | syn::BinOp::BitOrAssign(_)
                                        | syn::BinOp::ShlAssign(_)
                                        | syn::BinOp::ShrAssign(_)
                                );
                                let node_mutation = compound_assignment
                                    && self
                                        .node_names
                                        .iter()
                                        .any(|name| expression_mentions_ident(&binary.left, name));
                                let rule_mutation = compound_assignment
                                    && self
                                        .rule_names
                                        .iter()
                                        .any(|name| expression_mentions_ident(&binary.left, name));
                                syn::visit::visit_expr_binary(self, binary);
                                self.node_mutation |= node_mutation;
                                self.rule_mutation |= rule_mutation;
                                if node_mutation {
                                    self.mutation_events.push(MutationEvent::Node);
                                }
                                if rule_mutation {
                                    self.mutation_events.push(MutationEvent::Rule);
                                }
                            }

                            fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
                                if self
                                    .table_names
                                    .iter()
                                    .any(|name| expression_mentions_ident(&call.receiver, name))
                                    && matches!(
                                        call.method.to_string().as_str(),
                                        "site_rule" | "atom_rule" | "operators"
                                    )
                                {
                                    self.table_accessors.insert(call.method.to_string());
                                }
                                let mutating = matches!(
                                    call.method.to_string().as_str(),
                                    "append"
                                        | "clear"
                                        | "dedup"
                                        | "extend"
                                        | "get_mut"
                                        | "insert"
                                        | "last_mut"
                                        | "pop"
                                        | "push"
                                        | "remove"
                                        | "replace"
                                        | "resize"
                                        | "retain"
                                        | "set"
                                        | "sort"
                                        | "sort_by"
                                        | "sort_by_key"
                                        | "swap"
                                        | "swap_remove"
                                        | "take"
                                        | "truncate"
                                );
                                let node_mutation = mutating
                                    && self.node_names.iter().any(|name| {
                                        expression_mentions_ident(&call.receiver, name)
                                    });
                                let rule_mutation = mutating
                                    && self.rule_names.iter().any(|name| {
                                        expression_mentions_ident(&call.receiver, name)
                                    });
                                let carrier_access =
                                    self.node_names.iter().any(|name| {
                                        expression_mentions_ident(&call.receiver, name)
                                    }) || self.rule_names.iter().any(|name| {
                                        expression_mentions_ident(&call.receiver, name)
                                    }) || call.args.iter().any(|argument| {
                                        expression_mentions_binding(argument, self.node_names)
                                            || expression_mentions_binding(
                                                argument,
                                                self.rule_names,
                                            )
                                    });
                                syn::visit::visit_expr_method_call(self, call);
                                self.node_mutation |= node_mutation;
                                self.rule_mutation |= rule_mutation;
                                if carrier_access {
                                    self.mutation_events.push(MutationEvent::CarrierAccess);
                                }
                                if node_mutation {
                                    self.mutation_events.push(MutationEvent::Node);
                                }
                                if rule_mutation {
                                    self.mutation_events.push(MutationEvent::Rule);
                                }
                            }

                            fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
                                self.invalid_pratt |= self.invalid_pratt_helper
                                    && exact_path(&call.func, &["invalid_pratt_table"]);
                                let carrier_escape =
                                    expression_mentions_binding(&call.func, self.node_names)
                                        || expression_mentions_binding(&call.func, self.rule_names)
                                        || call.args.iter().any(|argument| {
                                            expression_mentions_binding(argument, self.node_names)
                                                || expression_mentions_binding(
                                                    argument,
                                                    self.rule_names,
                                                )
                                        });
                                syn::visit::visit_expr_call(self, call);
                                if carrier_escape {
                                    self.mutation_events.push(MutationEvent::CarrierAccess);
                                }
                            }

                            fn visit_expr_reference(
                                &mut self,
                                reference: &'ast syn::ExprReference,
                            ) {
                                let node_mutation = reference.mutability.is_some()
                                    && self.node_names.iter().any(|name| {
                                        expression_mentions_ident(&reference.expr, name)
                                    });
                                let rule_mutation = reference.mutability.is_some()
                                    && self.rule_names.iter().any(|name| {
                                        expression_mentions_ident(&reference.expr, name)
                                    });
                                syn::visit::visit_expr_reference(self, reference);
                                self.node_mutation |= node_mutation;
                                self.rule_mutation |= rule_mutation;
                                if node_mutation {
                                    self.mutation_events.push(MutationEvent::Node);
                                }
                                if rule_mutation {
                                    self.mutation_events.push(MutationEvent::Rule);
                                }
                            }

                            fn visit_expr_unsafe(&mut self, expression: &'ast syn::ExprUnsafe) {
                                let carrier_escape =
                                    self.node_names
                                        .iter()
                                        .any(|name| block_mentions_ident(&expression.block, name))
                                        || self.rule_names.iter().any(|name| {
                                            block_mentions_ident(&expression.block, name)
                                        });
                                syn::visit::visit_expr_unsafe(self, expression);
                                if carrier_escape {
                                    self.mutation_events.push(MutationEvent::CarrierAccess);
                                }
                            }

                            fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
                                let words = macro_
                                    .tokens
                                    .to_string()
                                    .split(|character: char| {
                                        !(character == '_' || character.is_alphanumeric())
                                    })
                                    .filter(|word| !word.is_empty())
                                    .map(str::to_owned)
                                    .collect::<BTreeSet<_>>();
                                if self
                                    .node_names
                                    .iter()
                                    .chain(self.rule_names.iter())
                                    .any(|name| words.contains(name))
                                {
                                    self.mutation_events.push(MutationEvent::CarrierAccess);
                                }
                                syn::visit::visit_macro(self, macro_);
                            }

                            fn visit_expr_for_loop(&mut self, loop_: &'ast syn::ExprForLoop) {
                                let exact_operator_rows = matches!(
                                    loop_.expr.as_ref(),
                                    syn::Expr::MethodCall(call)
                                        if call.method == "operators"
                                            && call.args.is_empty()
                                            && self.table_names.iter().any(|name| {
                                                expression_mentions_ident(&call.receiver, name)
                                            })
                                );
                                if exact_operator_rows {
                                    let operator_names = bound_identifiers(&loop_.pat);
                                    struct OperatorRows<'a> {
                                        names: &'a BTreeSet<String>,
                                        accessors: BTreeSet<String>,
                                    }
                                    impl<'ast> syn::visit::Visit<'ast> for OperatorRows<'_> {
                                        fn visit_expr_method_call(
                                            &mut self,
                                            call: &'ast syn::ExprMethodCall,
                                        ) {
                                            if self.names.iter().any(|name| {
                                                expression_mentions_ident(&call.receiver, name)
                                            }) && matches!(
                                                call.method.to_string().as_str(),
                                                "matcher_rule"
                                                    | "fixity"
                                                    | "left_binding_power"
                                                    | "right_binding_power"
                                            ) {
                                                self.accessors.insert(call.method.to_string());
                                            }
                                            syn::visit::visit_expr_method_call(self, call);
                                        }
                                    }
                                    let mut rows = OperatorRows {
                                        names: &operator_names,
                                        accessors: BTreeSet::new(),
                                    };
                                    rows.visit_block(&loop_.body);
                                    self.exact_operator_loops += usize::from(
                                        rows.accessors
                                            == BTreeSet::from([
                                                "fixity".to_owned(),
                                                "left_binding_power".to_owned(),
                                                "matcher_rule".to_owned(),
                                                "right_binding_power".to_owned(),
                                            ]),
                                    );
                                }
                                syn::visit::visit_expr_for_loop(self, loop_);
                            }

                            fn visit_ident(&mut self, ident: &'ast syn::Ident) {
                                self.pratt_node |= ident == "Pratt";
                                self.invalid_pratt |= ident == "InvalidPrattTable";
                            }
                        }
                        let mut body = Body {
                            node_names: &self.node_names,
                            rule_names: &self.rule_names,
                            table_names: bound_identifiers(&loop_.pat),
                            table_accessors: BTreeSet::new(),
                            exact_operator_loops: 0,
                            node_mutation: false,
                            rule_mutation: false,
                            pratt_node: false,
                            invalid_pratt_helper: self.invalid_pratt_helper,
                            invalid_pratt: false,
                            mutation_events: Vec::new(),
                        };
                        body.visit_block(&loop_.body);
                        let attachment_count = body
                            .mutation_events
                            .iter()
                            .filter(|event| **event == MutationEvent::AttachRoot)
                            .count();
                        let attachment_is_final =
                            body.mutation_events.last() == Some(&MutationEvent::AttachRoot);
                        self.live_loops += usize::from(
                            body.node_mutation
                                && body.rule_mutation
                                && body.pratt_node
                                && body.invalid_pratt
                                && body.table_accessors
                                    == BTreeSet::from([
                                        "atom_rule".to_owned(),
                                        "operators".to_owned(),
                                        "site_rule".to_owned(),
                                    ])
                                && body.exact_operator_loops == 1
                                && attachment_count == 1
                                && attachment_is_final,
                        );
                    }
                    syn::visit::visit_expr_for_loop(self, loop_);
                }
            }
            let Some(nodes_name) = direct_expression_binding(nodes_expression) else {
                return false;
            };
            let Some(rules_name) = direct_expression_binding(rules_expression) else {
                return false;
            };
            let node_aliases = carrier_alias_closure(&method.block, BTreeSet::from([nodes_name]));
            let rule_aliases = carrier_alias_closure(&method.block, BTreeSet::from([rules_name]));
            let mut attachment = PrattAttachment {
                pratt_tainted: &locals.pratt_tainted,
                node_names: node_aliases,
                rule_names: rule_aliases,
                invalid_pratt_helper: invalid_pratt_helpers == 1,
                live_loops: 0,
            };
            attachment.visit_block(&method.block);

            let table_loop_indices = method
                .block
                .stmts
                .iter()
                .enumerate()
                .filter_map(|(index, statement)| match statement {
                    syn::Stmt::Expr(syn::Expr::ForLoop(loop_), _) => {
                        let table_driven =
                            expression_has_instance_accessor(&loop_.expr, "pratt_tables")
                                || locals
                                    .pratt_tainted
                                    .iter()
                                    .any(|name| expression_mentions_ident(&loop_.expr, name));
                        table_driven.then_some(index)
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            let no_post_attachment_mutation = if let [table_loop_index] =
                table_loop_indices.as_slice()
            {
                struct LaterMutation<'a> {
                    node_names: &'a BTreeSet<String>,
                    rule_names: &'a BTreeSet<String>,
                    found: bool,
                }
                impl<'ast> syn::visit::Visit<'ast> for LaterMutation<'_> {
                    fn visit_local(&mut self, local: &'ast syn::Local) {
                        let rebound = bound_identifiers(&local.pat);
                        self.found |= rebound.iter().any(|name| {
                            self.node_names.contains(name) || self.rule_names.contains(name)
                        });
                        syn::visit::visit_local(self, local);
                    }

                    fn visit_expr_assign(&mut self, assign: &'ast syn::ExprAssign) {
                        self.found |= self
                            .node_names
                            .iter()
                            .any(|name| expression_mentions_ident(&assign.left, name))
                            || self
                                .rule_names
                                .iter()
                                .any(|name| expression_mentions_ident(&assign.left, name));
                        syn::visit::visit_expr_assign(self, assign);
                    }

                    fn visit_expr_binary(&mut self, binary: &'ast syn::ExprBinary) {
                        let compound_assignment = matches!(
                            binary.op,
                            syn::BinOp::AddAssign(_)
                                | syn::BinOp::SubAssign(_)
                                | syn::BinOp::MulAssign(_)
                                | syn::BinOp::DivAssign(_)
                                | syn::BinOp::RemAssign(_)
                                | syn::BinOp::BitXorAssign(_)
                                | syn::BinOp::BitAndAssign(_)
                                | syn::BinOp::BitOrAssign(_)
                                | syn::BinOp::ShlAssign(_)
                                | syn::BinOp::ShrAssign(_)
                        );
                        self.found |= compound_assignment
                            && (self
                                .node_names
                                .iter()
                                .any(|name| expression_mentions_ident(&binary.left, name))
                                || self
                                    .rule_names
                                    .iter()
                                    .any(|name| expression_mentions_ident(&binary.left, name)));
                        syn::visit::visit_expr_binary(self, binary);
                    }

                    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
                        self.found |= self
                            .node_names
                            .iter()
                            .any(|name| expression_mentions_ident(&call.receiver, name))
                            || self
                                .rule_names
                                .iter()
                                .any(|name| expression_mentions_ident(&call.receiver, name))
                            || call.args.iter().any(|argument| {
                                expression_mentions_binding(argument, self.node_names)
                                    || expression_mentions_binding(argument, self.rule_names)
                            });
                        syn::visit::visit_expr_method_call(self, call);
                    }

                    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
                        let exact_accepted_return = matches!(
                            call.func.as_ref(),
                            syn::Expr::Path(path)
                                if path.path.segments.last().is_some_and(
                                    |segment| segment.ident == "accepted"
                                )
                        ) && matches!(
                            call.args.first(),
                            Some(syn::Expr::Struct(record))
                                if call.args.len() == 1 && record.path.is_ident("Self")
                        );
                        if !exact_accepted_return {
                            self.found |= expression_mentions_binding(&call.func, self.node_names)
                                || expression_mentions_binding(&call.func, self.rule_names)
                                || call.args.iter().any(|argument| {
                                    expression_mentions_binding(argument, self.node_names)
                                        || expression_mentions_binding(argument, self.rule_names)
                                });
                        }
                        syn::visit::visit_expr_call(self, call);
                    }

                    fn visit_expr_reference(&mut self, reference: &'ast syn::ExprReference) {
                        self.found |= reference.mutability.is_some()
                            && (self
                                .node_names
                                .iter()
                                .any(|name| expression_mentions_ident(&reference.expr, name))
                                || self
                                    .rule_names
                                    .iter()
                                    .any(|name| expression_mentions_ident(&reference.expr, name)));
                        syn::visit::visit_expr_reference(self, reference);
                    }

                    fn visit_expr_unsafe(&mut self, expression: &'ast syn::ExprUnsafe) {
                        self.found |= self
                            .node_names
                            .iter()
                            .any(|name| block_mentions_ident(&expression.block, name))
                            || self
                                .rule_names
                                .iter()
                                .any(|name| block_mentions_ident(&expression.block, name));
                        syn::visit::visit_expr_unsafe(self, expression);
                    }

                    fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
                        let words = macro_
                            .tokens
                            .to_string()
                            .split(|character: char| {
                                !(character == '_' || character.is_alphanumeric())
                            })
                            .filter(|word| !word.is_empty())
                            .map(str::to_owned)
                            .collect::<BTreeSet<_>>();
                        self.found |= self
                            .node_names
                            .iter()
                            .chain(self.rule_names.iter())
                            .any(|name| words.contains(name));
                        syn::visit::visit_macro(self, macro_);
                    }
                }
                let mut later = LaterMutation {
                    node_names: &attachment.node_names,
                    rule_names: &attachment.rule_names,
                    found: false,
                };
                for statement in method.block.stmts.iter().skip(table_loop_index + 1) {
                    later.visit_stmt(statement);
                }
                !later.found
            } else {
                false
            };

            let exact_name_map_resolution = locals.entry_lookups == 1
                && locals.entry_lookup_maps.len() == 1
                && locals
                    .entry_lookup_maps
                    .is_subset(&locals.name_map_population_maps)
                && locals
                    .entry_lookup_maps
                    .iter()
                    .all(|map| locals.name_map_mutations.get(map).copied() == Some(1));
            let exact_entry_resolution = (locals.entry_equalities == 1
                && locals.entry_lookups == 0)
                || (locals.entry_equalities == 0 && exact_name_map_resolution);
            locals.entry_calls == 1
                && exact_entry_resolution
                && locals.pratt_calls == 1
                && locals.entry_overwrites == 0
                && attachment.live_loops == 1
                && no_post_attachment_mutation
                && block_tail_named_call_count(&method.block, "accepted") == 1
        })
    })
}

fn compiler_mod_instance_resolution_calls(source: &str) -> usize {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct Calls {
        current_function: Option<String>,
        exact: usize,
    }
    impl<'ast> syn::visit::Visit<'ast> for Calls {
        fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
            let previous = self
                .current_function
                .replace(function.sig.ident.to_string());
            syn::visit::visit_item_fn(self, function);
            self.current_function = previous;
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            let is_program_resolve = matches!(
                call.func.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none()
                        && path.path.segments.len() == 2
                        && path.path.segments[0].ident == "Program"
                        && path.path.segments[1].ident == "resolve"
            );
            let in_compile_spine = matches!(
                self.current_function.as_deref(),
                Some("compile_instance" | "compile_validated_instance")
            );
            let has_module = call
                .args
                .iter()
                .any(|argument| expression_mentions_ident(argument, "module"));
            let has_instance = call
                .args
                .iter()
                .any(|argument| expression_mentions_ident(argument, "instance"));
            if is_program_resolve
                && in_compile_spine
                && call.args.len() == 2
                && has_module
                && has_instance
            {
                self.exact += 1;
            }
            syn::visit::visit_expr_call(self, call);
        }
    }

    let syntax = syn::parse_file(source).expect("compiler mod source is valid Rust");
    let mut calls = Calls::default();
    calls.visit_file(&syntax);
    calls.exact
}

fn program_resolve_call_count(source: &str) -> usize {
    use syn::visit::Visit as _;

    #[derive(Default)]
    struct Calls(usize);
    impl<'ast> syn::visit::Visit<'ast> for Calls {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            self.0 += usize::from(matches!(
                call.func.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none()
                        && path.path.segments.len() == 2
                        && path.path.segments[0].ident == "Program"
                        && path.path.segments[1].ident == "resolve"
            ));
            syn::visit::visit_expr_call(self, call);
        }
    }

    let syntax = syn::parse_file(source).expect("Program::resolve caller source is valid Rust");
    let mut calls = Calls::default();
    calls.visit_file(&syntax);
    calls.0
}

fn program_construction_findings(relative: &str, source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    struct ProgramConstruction<'a> {
        aliases: &'a BTreeSet<String>,
        is_syntax: bool,
        in_program_impl: bool,
        current_function: Option<String>,
        resolve_methods: usize,
        findings: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for ProgramConstruction<'_> {
        fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
            if item.ident == "Program" {
                if !self.is_syntax {
                    self.findings
                        .insert("Program model outside syntax.rs".to_owned());
                }
                for attribute in &item.attrs {
                    if !attribute.path().is_ident("derive") {
                        continue;
                    }
                    let _ = attribute.parse_nested_meta(|meta| {
                        if matches!(
                            meta.path
                                .segments
                                .last()
                                .map(|segment| segment.ident.to_string())
                                .as_deref(),
                            Some("Default" | "Deserialize")
                        ) {
                            self.findings.insert(
                                "Program has an alternate synthesized constructor".to_owned(),
                            );
                        }
                        Ok(())
                    });
                }
            }
            syn::visit::visit_item_struct(self, item);
        }

        fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
            let previous = self
                .current_function
                .replace(function.sig.ident.to_string());
            if return_type_mentions_nominal_alias(&function.sig.output, self.aliases) {
                self.findings
                    .insert(format!("free Program constructor `{}`", function.sig.ident));
            }
            syn::visit::visit_item_fn(self, function);
            self.current_function = previous;
        }

        fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
            let previous = self.in_program_impl;
            self.in_program_impl = type_is_nominal_alias(&item.self_ty, self.aliases);
            if self.in_program_impl {
                for impl_item in &item.items {
                    let syn::ImplItem::Fn(function) = impl_item else {
                        continue;
                    };
                    let constructs =
                        return_type_mentions_nominal_alias(&function.sig.output, self.aliases)
                            || output_mentions_ident(&function.sig.output, "Self");
                    if function.sig.ident == "resolve" {
                        self.resolve_methods += 1;
                    } else if constructs {
                        self.findings.insert(format!(
                            "alternate Program constructor method `{}`",
                            function.sig.ident
                        ));
                    }
                }
            }
            syn::visit::visit_item_impl(self, item);
            self.in_program_impl = previous;
        }

        fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
            let previous = self
                .current_function
                .replace(function.sig.ident.to_string());
            syn::visit::visit_impl_item_fn(self, function);
            self.current_function = previous;
        }

        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            let program = expression
                .path
                .segments
                .last()
                .is_some_and(|segment| self.aliases.contains(&segment.ident.to_string()))
                || (self.in_program_impl && expression.path.is_ident("Self"));
            let authorized = self.is_syntax
                && self.in_program_impl
                && self.current_function.as_deref() == Some("resolve");
            if program && !authorized {
                self.findings
                    .insert("Program struct construction outside Program::resolve".to_owned());
            }
            syn::visit::visit_expr_struct(self, expression);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            let tokens = macro_.tokens.to_string();
            let words = tokens
                .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                .collect::<BTreeSet<_>>();
            if self
                .aliases
                .iter()
                .any(|alias| words.contains(alias.as_str()))
                || (self.in_program_impl && words.contains("Self"))
            {
                self.findings
                    .insert("macro-hidden Program construction or implementation".to_owned());
            }
            syn::visit::visit_macro(self, macro_);
        }
    }

    let syntax = syn::parse_file(source).expect("Program-construction source is valid Rust");
    let aliases = nominal_aliases(&syntax, "Program");
    let mut findings = BTreeSet::new();
    if aliases.len() != 1 {
        findings.insert(format!(
            "Program is renamed or type-aliased as {:?}",
            aliases
                .iter()
                .filter(|alias| alias.as_str() != "Program")
                .collect::<BTreeSet<_>>()
        ));
    }
    let mut visitor = ProgramConstruction {
        aliases: &aliases,
        is_syntax: relative == "crates/core/src/compiler/syntax.rs",
        in_program_impl: false,
        current_function: None,
        resolve_methods: 0,
        findings,
    };
    visitor.visit_file(&syntax);
    let expected_resolve = usize::from(visitor.is_syntax);
    if visitor.resolve_methods != expected_resolve {
        visitor.findings.insert(format!(
            "Program::resolve definition count is {}, expected {expected_resolve}",
            visitor.resolve_methods
        ));
    }
    visitor.findings
}

fn path_identity(path: &syn::Path, segment_count: usize) -> Option<String> {
    let names = path
        .segments
        .iter()
        .take(segment_count)
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>();
    (!names.is_empty()).then(|| {
        let prefix = if path.leading_colon.is_some() {
            "::"
        } else {
            ""
        };
        format!("{prefix}{}", names.join("::"))
    })
}

fn type_identity(ty: &syn::Type) -> Option<String> {
    match ty {
        syn::Type::Group(group) => type_identity(&group.elem),
        syn::Type::Paren(paren) => type_identity(&paren.elem),
        syn::Type::Reference(reference) => type_identity(&reference.elem),
        syn::Type::Path(path) if path.qself.is_none() => {
            path_identity(&path.path, path.path.segments.len())
        }
        _ => None,
    }
}

fn qualified_associated_method_identity(path: &syn::ExprPath) -> Option<(String, String, String)> {
    let qself = path.qself.as_ref()?;
    let receiver = type_identity(&qself.ty)?;
    let trait_name = path_identity(&path.path, qself.position)?;
    let method = path.path.segments.last()?.ident.to_string();
    Some((receiver, trait_name, method))
}

#[derive(Default)]
struct InstanceAccessCatalog {
    function_producers: BTreeSet<String>,
    method_producers: BTreeSet<String>,
    field_producers: BTreeSet<String>,
    associated_trait_methods: Vec<(String, String, BTreeSet<String>)>,
    trait_impls: BTreeSet<(String, String)>,
    nominal_trait_default_outputs: BTreeSet<(String, String)>,
    nominal_impl_trait_outputs: BTreeSet<(String, String, String)>,
    associated_method_producers: BTreeSet<(String, String, String)>,
}

fn instance_access_catalog<'a>(
    sources: impl IntoIterator<Item = &'a str>,
) -> InstanceAccessCatalog {
    use syn::visit::Visit as _;

    fn output_identifiers(output: &syn::ReturnType) -> BTreeSet<String> {
        struct Identifiers {
            names: BTreeSet<String>,
        }
        impl<'ast> syn::visit::Visit<'ast> for Identifiers {
            fn visit_ident(&mut self, ident: &'ast syn::Ident) {
                self.names.insert(ident.to_string());
            }
        }

        let mut identifiers = Identifiers {
            names: BTreeSet::new(),
        };
        if let syn::ReturnType::Type(_, ty) = output {
            identifiers.visit_type(ty);
        }
        identifiers.names
    }

    struct Producers<'a> {
        aliases: &'a BTreeSet<String>,
        instance_impl: bool,
        current_trait: Option<String>,
        current_impl_trait: Option<String>,
        current_impl_receiver: Option<String>,
        current_impl_nominal_outputs: BTreeSet<String>,
        catalog: InstanceAccessCatalog,
    }
    impl<'ast> syn::visit::Visit<'ast> for Producers<'_> {
        fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
            if return_type_mentions_nominal_alias(&function.sig.output, self.aliases) {
                self.catalog
                    .function_producers
                    .insert(function.sig.ident.to_string());
            }
            syn::visit::visit_item_fn(self, function);
        }

        fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
            let previous = self.current_trait.replace(item.ident.to_string());
            for trait_item in &item.items {
                if let syn::TraitItem::Type(output) = trait_item
                    && output
                        .default
                        .as_ref()
                        .is_some_and(|(_, ty)| type_mentions_nominal_alias(ty, self.aliases))
                {
                    self.catalog
                        .nominal_trait_default_outputs
                        .insert((item.ident.to_string(), output.ident.to_string()));
                }
            }
            syn::visit::visit_item_trait(self, item);
            self.current_trait = previous;
        }

        fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
            let previous_instance_impl = self.instance_impl;
            let previous_impl_trait = self.current_impl_trait.take();
            let previous_impl_receiver = self.current_impl_receiver.take();
            let previous_nominal_outputs = std::mem::take(&mut self.current_impl_nominal_outputs);
            self.instance_impl = type_is_nominal_alias(&item.self_ty, self.aliases);
            self.current_impl_trait = item
                .trait_
                .as_ref()
                .and_then(|(_, path, _)| path_identity(path, path.segments.len()));
            self.current_impl_receiver = type_identity(&item.self_ty);
            if let (Some(receiver), Some(trait_name)) =
                (&self.current_impl_receiver, &self.current_impl_trait)
            {
                self.catalog
                    .trait_impls
                    .insert((receiver.clone(), trait_name.clone()));
            }
            for impl_item in &item.items {
                if let syn::ImplItem::Type(output) = impl_item
                    && type_mentions_nominal_alias(&output.ty, self.aliases)
                {
                    let output_name = output.ident.to_string();
                    self.current_impl_nominal_outputs
                        .insert(output_name.clone());
                    if let (Some(receiver), Some(trait_name)) =
                        (&self.current_impl_receiver, &self.current_impl_trait)
                    {
                        self.catalog.nominal_impl_trait_outputs.insert((
                            receiver.clone(),
                            trait_name.clone(),
                            output_name,
                        ));
                    }
                }
            }
            syn::visit::visit_item_impl(self, item);
            self.instance_impl = previous_instance_impl;
            self.current_impl_trait = previous_impl_trait;
            self.current_impl_receiver = previous_impl_receiver;
            self.current_impl_nominal_outputs = previous_nominal_outputs;
        }

        fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
            if return_type_mentions_nominal_alias(&function.sig.output, self.aliases)
                || (self.instance_impl && output_mentions_ident(&function.sig.output, "Self"))
            {
                self.catalog
                    .method_producers
                    .insert(function.sig.ident.to_string());
            }
            if self
                .current_impl_nominal_outputs
                .iter()
                .any(|output| output_mentions_ident(&function.sig.output, output))
                && let (Some(receiver), Some(trait_name)) =
                    (&self.current_impl_receiver, &self.current_impl_trait)
            {
                self.catalog.associated_method_producers.insert((
                    receiver.clone(),
                    trait_name.clone(),
                    function.sig.ident.to_string(),
                ));
            }
            syn::visit::visit_impl_item_fn(self, function);
        }

        fn visit_trait_item_fn(&mut self, function: &'ast syn::TraitItemFn) {
            if return_type_mentions_nominal_alias(&function.sig.output, self.aliases) {
                self.catalog
                    .method_producers
                    .insert(function.sig.ident.to_string());
            }
            if let Some(trait_name) = &self.current_trait {
                self.catalog.associated_trait_methods.push((
                    trait_name.clone(),
                    function.sig.ident.to_string(),
                    output_identifiers(&function.sig.output),
                ));
            }
            syn::visit::visit_trait_item_fn(self, function);
        }

        fn visit_field(&mut self, field: &'ast syn::Field) {
            if type_mentions_nominal_alias(&field.ty, self.aliases) {
                let name = field
                    .ident
                    .as_ref()
                    .map_or_else(|| "<unnamed>".to_owned(), ToString::to_string);
                self.catalog.field_producers.insert(name);
            }
            syn::visit::visit_field(self, field);
        }
    }

    let mut catalog = InstanceAccessCatalog::default();
    for source in sources {
        let syntax = syn::parse_file(source).expect("instance-producer source is valid Rust");
        let aliases = nominal_aliases(&syntax, "GrammarInstance");
        let mut producers = Producers {
            aliases: &aliases,
            instance_impl: false,
            current_trait: None,
            current_impl_trait: None,
            current_impl_receiver: None,
            current_impl_nominal_outputs: BTreeSet::new(),
            catalog: InstanceAccessCatalog::default(),
        };
        producers.visit_file(&syntax);
        catalog
            .function_producers
            .extend(producers.catalog.function_producers);
        catalog
            .method_producers
            .extend(producers.catalog.method_producers);
        catalog
            .field_producers
            .extend(producers.catalog.field_producers);
        catalog
            .associated_trait_methods
            .extend(producers.catalog.associated_trait_methods);
        catalog.trait_impls.extend(producers.catalog.trait_impls);
        catalog
            .nominal_trait_default_outputs
            .extend(producers.catalog.nominal_trait_default_outputs);
        catalog
            .nominal_impl_trait_outputs
            .extend(producers.catalog.nominal_impl_trait_outputs);
        catalog
            .associated_method_producers
            .extend(producers.catalog.associated_method_producers);
    }
    for (trait_name, method_name, outputs) in &catalog.associated_trait_methods {
        for (receiver, impl_trait) in &catalog.trait_impls {
            let nominal_output = catalog
                .nominal_trait_default_outputs
                .iter()
                .any(|(bound_trait, output)| bound_trait == trait_name && outputs.contains(output))
                || catalog.nominal_impl_trait_outputs.iter().any(
                    |(bound_receiver, bound_trait, output)| {
                        bound_receiver == receiver
                            && bound_trait == trait_name
                            && outputs.contains(output)
                    },
                );
            if impl_trait == trait_name && nominal_output {
                catalog.associated_method_producers.insert((
                    receiver.clone(),
                    trait_name.clone(),
                    method_name.clone(),
                ));
            }
        }
    }
    catalog
}

fn instance_resolution_accessor_count_with_catalog(
    source: &str,
    catalog: &InstanceAccessCatalog,
) -> usize {
    use syn::visit::Visit as _;

    struct Accessors<'a> {
        type_aliases: &'a BTreeSet<String>,
        catalog: &'a InstanceAccessCatalog,
        bindings: BTreeSet<String>,
        callable_producers: BTreeSet<String>,
        instance_impl: bool,
        count: usize,
    }
    impl Accessors<'_> {
        fn path_is_producer(&self, path: &syn::ExprPath) -> bool {
            qualified_associated_method_identity(path).is_some_and(|identity| {
                self.catalog.associated_method_producers.contains(&identity)
            }) || path.path.segments.last().is_some_and(|segment| {
                let name = segment.ident.to_string();
                self.catalog.function_producers.contains(&name)
                    || self.catalog.method_producers.contains(&name)
                    || self.callable_producers.contains(&name)
            })
        }

        fn callable_producer_name(&self, expression: &syn::Expr) -> Option<String> {
            match expression {
                syn::Expr::Group(group) => self.callable_producer_name(&group.expr),
                syn::Expr::Paren(paren) => self.callable_producer_name(&paren.expr),
                syn::Expr::Path(path) => path
                    .path
                    .segments
                    .last()
                    .map(|segment| segment.ident.to_string())
                    .filter(|_| self.path_is_producer(path)),
                syn::Expr::Reference(reference) => self.callable_producer_name(&reference.expr),
                _ => None,
            }
        }

        fn expression_mentions_instance(&self, expression: &syn::Expr) -> bool {
            match expression {
                syn::Expr::Await(await_) => self.expression_mentions_instance(&await_.base),
                syn::Expr::Call(call) => {
                    let named_producer = matches!(
                        call.func.as_ref(),
                        syn::Expr::Path(path) if self.path_is_producer(path)
                    );
                    named_producer || self.expression_mentions_instance(&call.func)
                }
                syn::Expr::Field(field) => {
                    let producer = match &field.member {
                        syn::Member::Named(member) => {
                            self.catalog.field_producers.contains(&member.to_string())
                        }
                        syn::Member::Unnamed(_) => {
                            self.catalog.field_producers.contains("<unnamed>")
                        }
                    };
                    producer || self.expression_mentions_instance(&field.base)
                }
                syn::Expr::Group(group) => self.expression_mentions_instance(&group.expr),
                syn::Expr::MethodCall(call) => {
                    self.catalog
                        .method_producers
                        .contains(&call.method.to_string())
                        || (self.expression_mentions_instance(&call.receiver)
                            && matches!(
                                call.method.to_string().as_str(),
                                "as_deref"
                                    | "as_mut"
                                    | "as_ref"
                                    | "borrow"
                                    | "clone"
                                    | "deref"
                                    | "expect"
                                    | "to_owned"
                                    | "unwrap"
                                    | "unwrap_or"
                                    | "unwrap_or_else"
                            ))
                }
                syn::Expr::Index(index) => self.expression_mentions_instance(&index.expr),
                syn::Expr::Paren(paren) => self.expression_mentions_instance(&paren.expr),
                syn::Expr::Path(path) if path.qself.is_none() => path
                    .path
                    .get_ident()
                    .is_some_and(|ident| self.bindings.contains(&ident.to_string())),
                syn::Expr::Reference(reference) => {
                    self.expression_mentions_instance(&reference.expr)
                }
                syn::Expr::Try(try_) => self.expression_mentions_instance(&try_.expr),
                syn::Expr::Unary(unary) if matches!(unary.op, syn::UnOp::Deref(_)) => {
                    self.expression_mentions_instance(&unary.expr)
                }
                _ => false,
            }
        }

        fn seed_typed_arguments<'a>(&mut self, arguments: impl Iterator<Item = &'a syn::FnArg>) {
            for argument in arguments {
                match argument {
                    syn::FnArg::Receiver(_) if self.instance_impl => {
                        self.bindings.insert("self".to_owned());
                    }
                    syn::FnArg::Typed(argument)
                        if type_is_nominal_alias(&argument.ty, self.type_aliases) =>
                    {
                        self.bindings.extend(bound_identifiers(&argument.pat));
                    }
                    _ => {}
                }
            }
        }
    }
    impl<'ast> syn::visit::Visit<'ast> for Accessors<'_> {
        fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
            let previous = std::mem::take(&mut self.bindings);
            self.seed_typed_arguments(function.sig.inputs.iter());
            self.visit_block(&function.block);
            self.bindings = previous;
        }

        fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
            let previous = self.instance_impl;
            self.instance_impl = type_is_nominal_alias(&item.self_ty, self.type_aliases);
            syn::visit::visit_item_impl(self, item);
            self.instance_impl = previous;
        }

        fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
            let previous = std::mem::take(&mut self.bindings);
            self.seed_typed_arguments(function.sig.inputs.iter());
            self.visit_block(&function.block);
            self.bindings = previous;
        }

        fn visit_expr_closure(&mut self, closure: &'ast syn::ExprClosure) {
            let previous = self.bindings.clone();
            for input in &closure.inputs {
                if let syn::Pat::Type(typed) = input
                    && type_is_nominal_alias(&typed.ty, self.type_aliases)
                {
                    self.bindings.extend(bound_identifiers(&typed.pat));
                }
            }
            self.visit_expr(&closure.body);
            self.bindings = previous;
        }

        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(syn::LocalInit { expr, .. }) = &local.init {
                if let syn::Expr::Closure(closure) = expr.as_ref() {
                    if self.expression_mentions_instance(&closure.body) {
                        self.callable_producers
                            .extend(bound_identifiers(&local.pat));
                    }
                } else if self.callable_producer_name(expr).is_some() {
                    self.callable_producers
                        .extend(bound_identifiers(&local.pat));
                } else if self.expression_mentions_instance(expr) {
                    self.bindings.extend(bound_identifiers(&local.pat));
                }
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_assign(&mut self, assign: &'ast syn::ExprAssign) {
            struct Assigned(BTreeSet<String>);
            impl<'ast> syn::visit::Visit<'ast> for Assigned {
                fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
                    if path.qself.is_none()
                        && let Some(ident) = path.path.get_ident()
                    {
                        self.0.insert(ident.to_string());
                    }
                    syn::visit::visit_expr_path(self, path);
                }
            }
            let mut assigned = Assigned(BTreeSet::new());
            assigned.visit_expr(&assign.left);
            if self.callable_producer_name(&assign.right).is_some() {
                self.callable_producers.extend(assigned.0);
            } else if self.expression_mentions_instance(&assign.right) {
                self.bindings.extend(assigned.0);
            }
            syn::visit::visit_expr_assign(self, assign);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.count += usize::from(
                matches!(call.method.to_string().as_str(), "entry" | "pratt_tables")
                    && self.expression_mentions_instance(&call.receiver),
            );
            syn::visit::visit_expr_method_call(self, call);
        }
    }

    let syntax = syn::parse_file(source).expect("instance-accessor source is valid Rust");
    let type_aliases = nominal_aliases(&syntax, "GrammarInstance");
    let mut accessors = Accessors {
        type_aliases: &type_aliases,
        catalog,
        bindings: BTreeSet::new(),
        callable_producers: BTreeSet::new(),
        instance_impl: false,
        count: 0,
    };
    accessors.visit_file(&syntax);
    accessors.count
}

fn instance_resolution_accessor_count(source: &str) -> usize {
    let catalog = instance_access_catalog(std::iter::once(source));
    instance_resolution_accessor_count_with_catalog(source, &catalog)
}

fn grammar_instance_type_surface_findings(relative: &str, source: &str) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    if matches!(
        relative,
        "crates/core/src/compiler/instance.rs"
            | "crates/core/src/compiler/replay.rs"
            | "crates/core/src/compiler/syntax.rs"
    ) {
        return BTreeSet::new();
    }

    struct TypeSurface<'a> {
        aliases: &'a BTreeSet<String>,
        findings: BTreeSet<String>,
    }
    impl<'ast> syn::visit::Visit<'ast> for TypeSurface<'_> {
        fn visit_type(&mut self, ty: &'ast syn::Type) {
            if type_mentions_nominal_alias(ty, self.aliases) {
                self.findings
                    .insert("GrammarInstance type surface outside its exact owners".to_owned());
                return;
            }
            syn::visit::visit_type(self, ty);
        }
    }

    let syntax =
        syn::parse_file(source).expect("GrammarInstance type-surface source is valid Rust");
    let aliases = nominal_aliases(&syntax, "GrammarInstance");
    let mut surface = TypeSurface {
        aliases: &aliases,
        findings: BTreeSet::new(),
    };
    surface.visit_file(&syntax);
    surface.findings
}

fn grammar_instance_alias_findings(source: &str) -> BTreeSet<String> {
    let syntax = syn::parse_file(source).expect("GrammarInstance alias source is valid Rust");
    let aliases = nominal_aliases(&syntax, "GrammarInstance");
    if aliases.len() == 1 {
        BTreeSet::new()
    } else {
        BTreeSet::from([format!(
            "GrammarInstance is renamed or type-aliased as {:?}",
            aliases
                .iter()
                .filter(|alias| alias.as_str() != "GrammarInstance")
                .collect::<BTreeSet<_>>()
        )])
    }
}

fn exact_ident_type(ty: &syn::Type, expected: &str) -> bool {
    matches!(
        ty,
        syn::Type::Path(path)
            if path.qself.is_none() && path.path.is_ident(expected)
    )
}

fn exact_generic_inner_ident(ty: &syn::Type, outer: &str) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    if path.qself.is_some() || path.path.segments.len() != 1 {
        return None;
    }
    let segment = path.path.segments.first()?;
    if segment.ident != outer {
        return None;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };
    if arguments.args.len() != 1 {
        return None;
    }
    match arguments.args.first() {
        Some(syn::GenericArgument::Type(syn::Type::Path(inner)))
            if inner.qself.is_none() && inner.path.segments.len() == 1 =>
        {
            inner.path.get_ident().map(ToString::to_string)
        }
        _ => None,
    }
}

fn exact_generic_ident(ty: &syn::Type, outer: &str, inner: &str) -> bool {
    exact_generic_inner_ident(ty, outer).as_deref() == Some(inner)
}

fn pub_super(visibility: &syn::Visibility) -> bool {
    matches!(
        visibility,
        syn::Visibility::Restricted(restricted) if restricted.path.is_ident("super")
    )
}

fn exact_instance_input_type(ty: &syn::Type) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    if path.path.segments.len() != 1 {
        return false;
    }
    let segment = path.path.segments.first().expect("one path segment");
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return false;
    };
    segment.ident == "GrammarInstanceInput"
        && arguments.args.len() == 1
        && matches!(
            arguments.args.first(),
            Some(syn::GenericArgument::Lifetime(lifetime)) if lifetime.ident == "_"
        )
}

fn exact_stage_result_type(ty: &syn::Type) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    if path.path.segments.len() != 1 {
        return false;
    }
    let segment = path.path.segments.first().expect("one path segment");
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return false;
    };
    segment.ident == "StageResult"
        && arguments.args.len() == 1
        && matches!(
            arguments.args.first(),
            Some(syn::GenericArgument::Type(ty)) if exact_ident_type(ty, "CompiledModule")
        )
}

fn exact_compile_instance_signature(function: &syn::ItemFn) -> bool {
    if function.sig.ident != "compile_instance"
        || !matches!(function.vis, syn::Visibility::Public(_))
        || function.sig.constness.is_some()
        || function.sig.asyncness.is_some()
        || function.sig.unsafety.is_some()
        || function.sig.abi.is_some()
        || function.sig.variadic.is_some()
        || !function.sig.generics.params.is_empty()
        || function.sig.generics.where_clause.is_some()
    {
        return false;
    }
    let arguments = function.sig.inputs.iter().collect::<Vec<_>>();
    let [syn::FnArg::Typed(input), syn::FnArg::Typed(options)] = arguments.as_slice() else {
        return false;
    };
    if !matches!(
        input.pat.as_ref(),
        syn::Pat::Ident(binding) if binding.ident == "input"
    ) || !exact_instance_input_type(&input.ty)
        || !matches!(
            options.pat.as_ref(),
            syn::Pat::Ident(binding) if binding.ident == "options"
        )
        || !exact_ident_type(&options.ty, "CompileOptions")
    {
        return false;
    }
    matches!(
        &function.sig.output,
        syn::ReturnType::Type(_, ty) if exact_stage_result_type(ty)
    )
}

fn validated_carrier_ident(ty: &syn::Type) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    if path.qself.is_some() || path.path.segments.len() != 1 {
        return None;
    }
    let segment = path.path.segments.first()?;
    (!matches!(
        segment.ident.to_string().as_str(),
        "CompileOptions"
            | "CompiledModule"
            | "GrammarInstance"
            | "GrammarInstanceInput"
            | "SourceModule"
            | "Vec"
    ))
    .then(|| segment.ident.to_string())
}

fn exact_compile_validated_instance_signature(function: &syn::ItemFn) -> Option<String> {
    if function.sig.ident != "compile_validated_instance"
        || !matches!(function.vis, syn::Visibility::Inherited)
        || function.sig.constness.is_some()
        || function.sig.asyncness.is_some()
        || function.sig.unsafety.is_some()
        || function.sig.abi.is_some()
        || function.sig.variadic.is_some()
        || !function.sig.generics.params.is_empty()
        || function.sig.generics.where_clause.is_some()
        || !matches!(
            &function.sig.output,
            syn::ReturnType::Type(_, ty) if exact_stage_result_type(ty)
        )
    {
        return None;
    }
    let arguments = function.sig.inputs.iter().collect::<Vec<_>>();
    let [syn::FnArg::Typed(validated), syn::FnArg::Typed(options)] = arguments.as_slice() else {
        return None;
    };
    if !exact_ident_type(&options.ty, "CompileOptions") {
        return None;
    }
    validated_carrier_ident(&validated.ty)
}

fn stage_result_inner_nominal(output: &syn::ReturnType) -> Option<String> {
    let syn::ReturnType::Type(_, ty) = output else {
        return None;
    };
    let syn::Type::Path(path) = ty.as_ref() else {
        return None;
    };
    if path.qself.is_some() || path.path.segments.len() != 1 {
        return None;
    }
    let segment = path.path.segments.first()?;
    if segment.ident != "StageResult" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };
    if arguments.args.len() != 1 {
        return None;
    }
    match arguments.args.first() {
        Some(syn::GenericArgument::Type(syn::Type::Path(inner)))
            if inner.qself.is_none() && inner.path.segments.len() == 1 =>
        {
            inner
                .path
                .segments
                .first()
                .map(|segment| segment.ident.to_string())
        }
        _ => None,
    }
}

fn type_mentions_ident(ty: &syn::Type, expected: &str) -> bool {
    use syn::visit::Visit as _;

    struct Mentions<'a> {
        expected: &'a str,
        found: bool,
    }
    impl<'ast> syn::visit::Visit<'ast> for Mentions<'_> {
        fn visit_ident(&mut self, ident: &'ast syn::Ident) {
            self.found |= ident == self.expected;
        }
    }

    let mut mentions = Mentions {
        expected,
        found: false,
    };
    mentions.visit_type(ty);
    mentions.found
}

fn output_mentions_ident(output: &syn::ReturnType, expected: &str) -> bool {
    matches!(output, syn::ReturnType::Type(_, ty) if type_mentions_ident(ty, expected))
}

fn collect_renamed_instance_api(
    tree: &syn::UseTree,
    under_compiler: bool,
    findings: &mut BTreeSet<String>,
) {
    match tree {
        syn::UseTree::Path(path) => collect_renamed_instance_api(
            &path.tree,
            under_compiler || path.ident == "compiler",
            findings,
        ),
        syn::UseTree::Rename(rename)
            if matches!(
                rename.ident.to_string().as_str(),
                "GrammarInstanceInput"
                    | "compile_instance"
                    | "compile_validated_instance"
                    | "CompiledModule"
                    | "compiler"
            ) =>
        {
            findings.insert(format!(
                "renamed instance API `{}` as `{}`",
                rename.ident, rename.rename
            ));
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_renamed_instance_api(item, under_compiler, findings);
            }
        }
        syn::UseTree::Glob(_) if under_compiler => {
            findings.insert("glob re-export of compiler ingress".to_owned());
        }
        syn::UseTree::Name(_) | syn::UseTree::Rename(_) | syn::UseTree::Glob(_) => {}
    }
}

fn compile_ingress_surface_findings(relative: &str, source: &str) -> BTreeSet<String> {
    let syntax = syn::parse_file(source).expect("compile-ingress source is valid Rust");
    let input_aliases = nominal_aliases(&syntax, "GrammarInstanceInput");
    let compiled_aliases = nominal_aliases(&syntax, "CompiledModule");
    let mut findings = BTreeSet::new();
    for (canonical, aliases) in [
        ("GrammarInstanceInput", &input_aliases),
        ("CompiledModule", &compiled_aliases),
    ] {
        if aliases.len() != 1 {
            findings.insert(format!(
                "{canonical} is renamed or type-aliased as {:?}",
                aliases
                    .iter()
                    .filter(|alias| alias.as_str() != canonical)
                    .collect::<BTreeSet<_>>()
            ));
        }
    }

    fn inspect(
        relative: &str,
        items: &[syn::Item],
        findings: &mut BTreeSet<String>,
        nested: bool,
        input_aliases: &BTreeSet<String>,
        compiled_aliases: &BTreeSet<String>,
    ) {
        let allowed_function = |function: &syn::ItemFn| {
            if nested {
                return false;
            }
            if relative == "crates/core/src/compiler/mod.rs" {
                return (function.sig.ident == "compile_instance"
                    && exact_compile_instance_signature(function))
                    || (function.sig.ident == "compile_validated_instance"
                        && exact_compile_validated_instance_signature(function).is_some());
            }
            relative == "crates/core/src/compiler/replay.rs"
                && function.sig.ident == "replay_compilation"
        };

        for item in items {
            match item {
                syn::Item::Fn(function) => {
                    let name = function.sig.ident.to_string();
                    if return_type_mentions_nominal_alias(&function.sig.output, compiled_aliases)
                        && !allowed_function(function)
                    {
                        findings.insert(format!(
                            "unapproved CompiledModule ingress function `{name}`"
                        ));
                    }
                    if return_type_mentions_nominal_alias(&function.sig.output, input_aliases) {
                        findings.insert(format!(
                            "function `{name}` synthesizes GrammarInstanceInput"
                        ));
                    }
                }
                syn::Item::Impl(item) => {
                    let is_input = type_is_nominal_alias(&item.self_ty, input_aliases);
                    let is_compiled = type_is_nominal_alias(&item.self_ty, compiled_aliases);
                    if is_input
                        && (relative != "crates/core/src/compiler/instance.rs"
                            || item.trait_.is_some()
                            || nested)
                    {
                        findings.insert(
                            "GrammarInstanceInput trait or out-of-owner implementation".to_owned(),
                        );
                    }
                    for impl_item in &item.items {
                        let syn::ImplItem::Fn(function) = impl_item else {
                            continue;
                        };
                        let name = function.sig.ident.to_string();
                        if return_type_mentions_nominal_alias(
                            &function.sig.output,
                            compiled_aliases,
                        ) || (is_compiled && output_mentions_ident(&function.sig.output, "Self"))
                        {
                            findings.insert(format!(
                                "unapproved CompiledModule ingress method `{name}`"
                            ));
                        }
                        if return_type_mentions_nominal_alias(&function.sig.output, input_aliases)
                            && !(is_input && name == "new" && !nested)
                        {
                            findings.insert(format!(
                                "method `{name}` synthesizes GrammarInstanceInput"
                            ));
                        }
                    }
                    if is_compiled
                        && item
                            .items
                            .iter()
                            .any(|item| matches!(item, syn::ImplItem::Macro(_)))
                    {
                        findings.insert(
                            "macro invocation inside CompiledModule impl can hide ingress"
                                .to_owned(),
                        );
                    }
                    for impl_item in &item.items {
                        if let syn::ImplItem::Type(alias) = impl_item
                            && type_mentions_nominal_alias(&alias.ty, compiled_aliases)
                        {
                            findings.insert(format!(
                                "associated compile-ingress type alias `{}`",
                                alias.ident
                            ));
                        }
                    }
                }
                syn::Item::Type(alias)
                    if type_mentions_nominal_alias(&alias.ty, compiled_aliases)
                        || type_mentions_nominal_alias(&alias.ty, input_aliases) =>
                {
                    findings.insert(format!("compile-ingress type alias `{}`", alias.ident));
                }
                syn::Item::Static(item)
                    if type_mentions_nominal_alias(&item.ty, compiled_aliases)
                        || type_mentions_nominal_alias(&item.ty, input_aliases)
                        || expression_mentions_ident(&item.expr, "compile_instance") =>
                {
                    findings.insert(format!("static compile-ingress adapter `{}`", item.ident));
                }
                syn::Item::Const(item)
                    if type_mentions_nominal_alias(&item.ty, compiled_aliases)
                        || type_mentions_nominal_alias(&item.ty, input_aliases)
                        || expression_mentions_ident(&item.expr, "compile_instance") =>
                {
                    findings.insert(format!("const compile-ingress adapter `{}`", item.ident));
                }
                syn::Item::Trait(item) => {
                    for trait_item in &item.items {
                        let syn::TraitItem::Fn(function) = trait_item else {
                            continue;
                        };
                        if return_type_mentions_nominal_alias(
                            &function.sig.output,
                            compiled_aliases,
                        ) || return_type_mentions_nominal_alias(
                            &function.sig.output,
                            input_aliases,
                        ) {
                            findings.insert(format!(
                                "trait compile-ingress adapter `{}::{}`",
                                item.ident, function.sig.ident
                            ));
                        }
                    }
                }
                syn::Item::Use(import) => {
                    collect_renamed_instance_api(&import.tree, false, findings);
                }
                syn::Item::Mod(module) => {
                    if let Some((_, nested_items)) = &module.content {
                        inspect(
                            relative,
                            nested_items,
                            findings,
                            true,
                            input_aliases,
                            compiled_aliases,
                        );
                    }
                }
                syn::Item::Macro(item) => {
                    let tokens = item.mac.tokens.to_string();
                    if ["CompiledModule", "GrammarInstanceInput", "compile_instance"]
                        .into_iter()
                        .any(|forbidden| {
                            tokens
                                .split(|character: char| {
                                    !(character == '_' || character.is_alphanumeric())
                                })
                                .any(|token| token == forbidden)
                        })
                    {
                        findings.insert(
                            "macro tokens synthesize or alias the grammar-instance ingress"
                                .to_owned(),
                        );
                    }
                }
                _ => {}
            }
        }
    }
    inspect(
        relative,
        &syntax.items,
        &mut findings,
        false,
        &input_aliases,
        &compiled_aliases,
    );
    findings
}

fn validated_instance_spine_findings(
    instance_source: &str,
    compiler_source: &str,
) -> BTreeSet<String> {
    use syn::visit::Visit as _;

    fn input_type(ty: &syn::Type, aliases: &BTreeSet<String>) -> bool {
        type_is_nominal_alias(ty, aliases)
    }

    fn call_name(call: &syn::ExprCall) -> Option<String> {
        match call.func.as_ref() {
            syn::Expr::Path(path) if path.qself.is_none() => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        }
    }

    fn expression_call_count(expression: &syn::Expr, expected: &str) -> usize {
        struct Calls<'a> {
            expected: &'a str,
            count: usize,
        }
        impl<'ast> syn::visit::Visit<'ast> for Calls<'_> {
            fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
                self.count += usize::from(call_name(call).as_deref() == Some(self.expected));
                syn::visit::visit_expr_call(self, call);
            }
        }
        let mut calls = Calls { expected, count: 0 };
        calls.visit_expr(expression);
        calls.count
    }

    fn block_tail_call_count(block: &syn::Block, expected: &str) -> usize {
        let Some(statement) = block.stmts.last() else {
            return 0;
        };
        match statement {
            syn::Stmt::Expr(expression, None) => match expression {
                syn::Expr::Return(return_) => return_
                    .expr
                    .as_deref()
                    .map_or(0, |expression| expression_call_count(expression, expected)),
                expression => expression_call_count(expression, expected),
            },
            _ => 0,
        }
    }

    fn block_identifier_count(block: &syn::Block, expected: &str) -> usize {
        struct Ids<'a> {
            expected: &'a str,
            count: usize,
        }
        impl<'ast> syn::visit::Visit<'ast> for Ids<'_> {
            fn visit_ident(&mut self, ident: &'ast syn::Ident) {
                self.count += usize::from(ident == self.expected);
            }
        }
        let mut ids = Ids { expected, count: 0 };
        ids.visit_block(block);
        ids.count
    }

    fn exact_local_binding(pattern: &syn::Pat) -> Option<String> {
        match pattern {
            syn::Pat::Ident(binding) if binding.by_ref.is_none() && binding.subpat.is_none() => {
                Some(binding.ident.to_string())
            }
            _ => None,
        }
    }

    fn exact_binding_path(expression: &syn::Expr, bindings: &BTreeSet<String>) -> bool {
        match expression {
            syn::Expr::Group(group) => exact_binding_path(&group.expr, bindings),
            syn::Expr::Paren(paren) => exact_binding_path(&paren.expr, bindings),
            syn::Expr::Path(path) if path.qself.is_none() => path
                .path
                .get_ident()
                .is_some_and(|ident| bindings.contains(&ident.to_string())),
            _ => false,
        }
    }

    fn exact_manifest_len(expression: &syn::Expr, bindings: &BTreeSet<String>) -> bool {
        matches!(
            expression,
            syn::Expr::MethodCall(call)
                if call.method == "len"
                    && call.args.is_empty()
                    && exact_binding_path(&call.receiver, bindings)
        )
    }

    fn exact_instance_budget(expression: &syn::Expr) -> bool {
        direct_named_field(expression, "options", "instance_bytes")
    }

    fn exact_path(expression: &syn::Expr, expected: &[&str]) -> bool {
        matches!(
            expression,
            syn::Expr::Path(path)
                if path.qself.is_none()
                    && path.path.segments.len() == expected.len()
                    && path.path.segments.iter().zip(expected).all(|(segment, expected)| {
                        segment.ident == *expected
                            && matches!(segment.arguments, syn::PathArguments::None)
                    })
        )
    }

    fn exact_typed_manifest_decoder(
        expression: &syn::Expr,
        manifest_aliases: &BTreeSet<String>,
    ) -> bool {
        matches!(
            expression,
            syn::Expr::Call(call)
                if matches!(
                    call.func.as_ref(),
                    syn::Expr::Path(path)
                        if path.qself.is_none()
                            && path.path.segments.len() == 2
                            && path.path.segments[0].ident == "serde_json"
                            && path.path.segments[1].ident == "from_slice"
                            && decoder_returns_nominal(&path.path, "GrammarInstance")
                )
                    && call.args.len() == 1
                    && call.args.first().is_some_and(|argument| {
                        exact_binding_path(argument, manifest_aliases)
                    })
        )
    }

    fn exact_manifest_budget_condition(
        expression: &syn::Expr,
        bindings: &BTreeSet<String>,
    ) -> bool {
        matches!(
            expression,
            syn::Expr::Binary(binary)
                if exact_manifest_budget_binary(binary, bindings)
        )
    }

    fn exact_manifest_budget_binary(binary: &syn::ExprBinary, bindings: &BTreeSet<String>) -> bool {
        matches!(binary.op, syn::BinOp::Gt(_))
            && exact_manifest_len(&binary.left, bindings)
            && exact_instance_budget(&binary.right)
    }

    fn block_returns_resource_limit_rejection(block: &syn::Block) -> bool {
        let [syn::Stmt::Expr(syn::Expr::Return(return_), _)] = block.stmts.as_slice() else {
            return false;
        };
        let Some(syn::Expr::Call(rejected)) = return_.expr.as_deref() else {
            return false;
        };
        if !exact_path(&rejected.func, &["StageResult", "rejected"]) || rejected.args.len() != 1 {
            return false;
        }
        let Some(syn::Expr::Call(diagnostic)) = rejected.args.first() else {
            return false;
        };
        exact_path(&diagnostic.func, &["Diagnostic", "new"])
            && diagnostic.args.len() == 4
            && diagnostic
                .args
                .first()
                .is_some_and(|code| exact_path(code, &["DiagnosticCode", "ResourceLimit"]))
    }

    fn exact_decoder_match(expression: &syn::Expr, manifest_aliases: &BTreeSet<String>) -> bool {
        fn exact_zero(expression: &syn::Expr) -> bool {
            matches!(
                expression,
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Int(integer),
                    ..
                }) if integer.suffix().is_empty() && integer.base10_digits() == "0"
            )
        }

        fn exact_error_rejection(expression: &syn::Expr, error_binding: &str) -> bool {
            let syn::Expr::Block(block) = expression else {
                return false;
            };
            let [
                syn::Stmt::Local(message),
                syn::Stmt::Local(code),
                syn::Stmt::Expr(syn::Expr::Return(return_), _),
            ] = block.block.stmts.as_slice()
            else {
                return false;
            };

            let Some(message_binding) = exact_local_binding(&message.pat) else {
                return false;
            };
            let Some(message_init) = message.init.as_ref() else {
                return false;
            };
            let syn::Expr::MethodCall(to_string) = message_init.expr.as_ref() else {
                return false;
            };
            if message_init.diverge.is_some()
                || to_string.method != "to_string"
                || !to_string.args.is_empty()
                || !exact_binding_path(
                    &to_string.receiver,
                    &BTreeSet::from([error_binding.to_owned()]),
                )
            {
                return false;
            }

            let Some(code_binding) = exact_local_binding(&code.pat) else {
                return false;
            };
            let Some(code_init) = code.init.as_ref() else {
                return false;
            };
            let syn::Expr::If(code_choice) = code_init.expr.as_ref() else {
                return false;
            };
            let syn::Expr::MethodCall(contains) = code_choice.cond.as_ref() else {
                return false;
            };
            if code_init.diverge.is_some()
                || contains.method != "contains"
                || contains.args.len() != 1
                || !exact_binding_path(
                    &contains.receiver,
                    &BTreeSet::from([message_binding.clone()]),
                )
                || !contains
                    .args
                    .first()
                    .is_some_and(|needle| exact_path(needle, &["INVALID_PRATT_REPRESENTATION"]))
                || !matches!(
                    code_choice.then_branch.stmts.as_slice(),
                    [syn::Stmt::Expr(code, None)]
                        if exact_path(code, &["DiagnosticCode", "InvalidPrattTable"])
                )
                || !matches!(
                    code_choice.else_branch.as_ref(),
                    Some((_, alternative))
                        if matches!(
                            alternative.as_ref(),
                            syn::Expr::Block(block)
                                if matches!(
                                    block.block.stmts.as_slice(),
                                    [syn::Stmt::Expr(code, None)]
                                        if exact_path(
                                            code,
                                            &["DiagnosticCode", "InvalidGrammarInstance"]
                                        )
                                )
                        )
                )
            {
                return false;
            }

            let Some(syn::Expr::Call(rejected)) = return_.expr.as_deref() else {
                return false;
            };
            if !exact_path(&rejected.func, &["StageResult", "rejected"]) || rejected.args.len() != 1
            {
                return false;
            }
            let Some(syn::Expr::Call(diagnostic)) = rejected.args.first() else {
                return false;
            };
            if diagnostic.args.len() != 4 {
                return false;
            }
            let diagnostic_code = &diagnostic.args[0];
            let start = &diagnostic.args[1];
            let end = &diagnostic.args[2];
            let message = &diagnostic.args[3];
            exact_path(&diagnostic.func, &["Diagnostic", "new"])
                && exact_binding_path(diagnostic_code, &BTreeSet::from([code_binding]))
                && exact_zero(start)
                && exact_zero(end)
                && exact_binding_path(message, &BTreeSet::from([message_binding]))
        }

        let syn::Expr::Match(match_) = expression else {
            return false;
        };
        if !exact_typed_manifest_decoder(&match_.expr, manifest_aliases) || match_.arms.len() != 2 {
            return false;
        }
        let [accepted, rejected] = match_.arms.as_slice() else {
            return false;
        };
        let accepted_binding = match &accepted.pat {
            syn::Pat::TupleStruct(pattern)
                if pattern.path.is_ident("Ok") && pattern.elems.len() == 1 =>
            {
                exact_local_binding(&pattern.elems[0])
            }
            _ => None,
        };
        let rejected_binding = match &rejected.pat {
            syn::Pat::TupleStruct(pattern)
                if pattern.path.is_ident("Err") && pattern.elems.len() == 1 =>
            {
                pattern.elems.first().and_then(exact_local_binding)
            }
            _ => None,
        };
        accepted.guard.is_none()
            && accepted_binding.as_ref().is_some_and(|binding| {
                exact_binding_path(&accepted.body, &BTreeSet::from([binding.clone()]))
            })
            && rejected.guard.is_none()
            && rejected_binding
                .as_deref()
                .is_some_and(|binding| exact_error_rejection(&rejected.body, binding))
    }

    fn exact_decoder_local(
        local: &syn::Local,
        manifest_aliases: &BTreeSet<String>,
    ) -> Option<String> {
        let binding = exact_local_binding(&local.pat)?;
        local
            .init
            .as_ref()
            .filter(|init| exact_decoder_match(&init.expr, manifest_aliases))
            .map(|_| binding)
    }

    fn exact_validated_tail(
        statement: &syn::Stmt,
        carrier: &str,
        decoder_binding: &str,
        source_binding: &str,
    ) -> bool {
        let syn::Stmt::Expr(syn::Expr::Call(accepted), None) = statement else {
            return false;
        };
        if !exact_path(&accepted.func, &["StageResult", "accepted"]) || accepted.args.len() != 1 {
            return false;
        }
        let Some(syn::Expr::Struct(validated)) = accepted.args.first() else {
            return false;
        };
        if validated.qself.is_some()
            || validated
                .path
                .segments
                .last()
                .is_none_or(|segment| segment.ident != carrier)
            || validated.rest.is_some()
            || validated.fields.len() != 2
        {
            return false;
        }
        let mut instance = false;
        let mut source = false;
        for field in &validated.fields {
            match &field.member {
                syn::Member::Named(member) if member == "instance" => {
                    instance = exact_binding_path(
                        &field.expr,
                        &BTreeSet::from([decoder_binding.to_owned()]),
                    );
                }
                syn::Member::Named(member) if member == "source_bytes" => {
                    source = exact_binding_path(
                        &field.expr,
                        &BTreeSet::from([source_binding.to_owned()]),
                    );
                }
                _ => return false,
            }
        }
        instance && source
    }

    fn exact_source_alias(local: &syn::Local) -> Option<String> {
        let binding = exact_local_binding(&local.pat)?;
        local
            .init
            .as_ref()
            .filter(|init| direct_named_field(&init.expr, "input", "source_bytes"))
            .map(|_| binding)
    }

    fn exact_instance_validation_guard(statement: &syn::Stmt, decoder_binding: &str) -> bool {
        let syn::Stmt::Expr(syn::Expr::If(if_), _) = statement else {
            return false;
        };
        if if_.else_branch.is_some() {
            return false;
        }
        let syn::Expr::Let(condition) = if_.cond.as_ref() else {
            return false;
        };
        let diagnostic_binding = match condition.pat.as_ref() {
            syn::Pat::TupleStruct(pattern)
                if pattern.path.is_ident("Err") && pattern.elems.len() == 1 =>
            {
                pattern.elems.first().and_then(exact_local_binding)
            }
            _ => None,
        };
        let Some(diagnostic_binding) = diagnostic_binding else {
            return false;
        };
        let syn::Expr::Call(validation) = condition.expr.as_ref() else {
            return false;
        };
        if !exact_path(&validation.func, &["validate_instance"])
            || validation.args.len() != 2
            || !validation.args.first().is_some_and(|instance| {
                matches!(
                    instance,
                    syn::Expr::Reference(reference)
                        if exact_binding_path(
                            &reference.expr,
                            &BTreeSet::from([decoder_binding.to_owned()])
                        )
                )
            })
            || !validation
                .args
                .iter()
                .nth(1)
                .is_some_and(|options| exact_path(options, &["options"]))
        {
            return false;
        }
        let [syn::Stmt::Expr(syn::Expr::Return(return_), _)] = if_.then_branch.stmts.as_slice()
        else {
            return false;
        };
        let Some(syn::Expr::Call(rejected)) = return_.expr.as_deref() else {
            return false;
        };
        exact_path(&rejected.func, &["StageResult", "rejected"])
            && rejected.args.len() == 1
            && rejected.args.first().is_some_and(|diagnostic| {
                exact_binding_path(diagnostic, &BTreeSet::from([diagnostic_binding.to_owned()]))
            })
    }

    fn exact_top_level_raw_spine(decoder: &syn::ItemFn, carrier: &str) -> bool {
        let statements = &decoder.block.stmts;
        let aliases = statements
            .iter()
            .enumerate()
            .filter_map(|(index, statement)| match statement {
                syn::Stmt::Local(local) => {
                    let binding = exact_local_binding(&local.pat)?;
                    local
                        .init
                        .as_ref()
                        .is_some_and(|init| {
                            direct_named_field(&init.expr, "input", "manifest_bytes")
                        })
                        .then_some((index, binding))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let [(alias_index, alias)] = aliases.as_slice() else {
            return false;
        };
        let source_aliases = statements
            .iter()
            .enumerate()
            .filter_map(|(index, statement)| match statement {
                syn::Stmt::Local(local) => {
                    exact_source_alias(local).map(|binding| (index, binding))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let [(source_index, source_binding)] = source_aliases.as_slice() else {
            return false;
        };
        let manifest_aliases = BTreeSet::from([alias.clone()]);
        let guards = statements
            .iter()
            .enumerate()
            .filter_map(|(index, statement)| match statement {
                syn::Stmt::Expr(syn::Expr::If(if_), _)
                    if if_.else_branch.is_none()
                        && exact_manifest_budget_condition(&if_.cond, &manifest_aliases)
                        && block_returns_resource_limit_rejection(&if_.then_branch) =>
                {
                    Some(index)
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let decoders = statements
            .iter()
            .enumerate()
            .filter_map(|(index, statement)| match statement {
                syn::Stmt::Local(local) => {
                    exact_decoder_local(local, &manifest_aliases).map(|binding| (index, binding))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let [guard_index] = guards.as_slice() else {
            return false;
        };
        let [(decoder_index, decoder_binding)] = decoders.as_slice() else {
            return false;
        };
        let Some(tail_index) = statements.len().checked_sub(1) else {
            return false;
        };
        alias_index == &0
            && source_index == &1
            && guard_index == &2
            && decoder_index == &3
            && alias != source_binding
            && decoder_binding != alias
            && decoder_binding != source_binding
            && statements[decoder_index + 1..tail_index]
                .iter()
                .all(|statement| exact_instance_validation_guard(statement, decoder_binding))
            && exact_validated_tail(
                &statements[tail_index],
                carrier,
                decoder_binding,
                source_binding,
            )
    }

    let instance = syn::parse_file(instance_source).expect("instance seam source is valid Rust");
    let compiler = syn::parse_file(compiler_source).expect("compiler seam source is valid Rust");
    let input_aliases = nominal_aliases(&instance, "GrammarInstanceInput");
    let mut findings = BTreeSet::new();

    let input_ingresses = instance
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function)
                if function.sig.inputs.iter().any(|argument| {
                    matches!(
                        argument,
                        syn::FnArg::Typed(argument)
                            if input_type(&argument.ty, &input_aliases)
                    )
                }) =>
            {
                Some(function)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    if input_ingresses.len() != 1 {
        findings.insert(format!(
            "instance.rs raw GrammarInstanceInput ingress count is {}",
            input_ingresses.len()
        ));
        return findings;
    }
    let decoder = input_ingresses[0];
    let decoder_arguments = decoder.sig.inputs.iter().collect::<Vec<_>>();
    let exact_decoder_arguments = matches!(
        decoder_arguments.as_slice(),
        [syn::FnArg::Typed(input), syn::FnArg::Typed(options)]
            if matches!(
                input.pat.as_ref(),
                syn::Pat::Ident(binding) if binding.ident == "input"
            )
                && input_type(&input.ty, &input_aliases)
                && matches!(
                    options.pat.as_ref(),
                    syn::Pat::Ident(binding) if binding.ident == "options"
                )
                && exact_ident_type(&options.ty, "CompileOptions")
    );
    if !pub_super(&decoder.vis)
        || !exact_decoder_arguments
        || decoder.sig.constness.is_some()
        || decoder.sig.asyncness.is_some()
        || decoder.sig.unsafety.is_some()
        || decoder.sig.abi.is_some()
        || decoder.sig.variadic.is_some()
        || !decoder
            .sig
            .generics
            .params
            .iter()
            .all(|parameter| matches!(parameter, syn::GenericParam::Lifetime(_)))
        || decoder.sig.generics.where_clause.is_some()
    {
        findings.insert(
            "instance.rs raw ingress is not one pub(super) input-plus-options decoder".to_owned(),
        );
    }
    let Some(carrier) = stage_result_inner_nominal(&decoder.sig.output) else {
        findings.insert(
            "instance.rs raw ingress does not return StageResult<one nominal validated carrier>"
                .to_owned(),
        );
        return findings;
    };

    struct RawManifestFlow<'a> {
        input_aliases: &'a BTreeSet<String>,
        current_function: Option<String>,
        input_impl: bool,
        tainted: BTreeSet<String>,
        manifest_aliases: BTreeSet<String>,
        source_aliases: BTreeSet<String>,
        manifest_field_reads: usize,
        source_field_reads: usize,
        manifest_reads_outside_decoder: usize,
        source_reads_outside_decoder: usize,
        direct_manifest_aliases: usize,
        direct_source_aliases: usize,
        exact_manifest_budget_guards: usize,
        exact_typed_decoders: usize,
        typed_callable_ingresses: usize,
        decoder_typed_ingresses: usize,
        raw_manifest_macros: usize,
        raw_forwarding_or_escape: usize,
    }
    impl RawManifestFlow<'_> {
        fn typed_argument_count<'a>(
            &self,
            arguments: impl Iterator<Item = &'a syn::FnArg>,
        ) -> usize {
            arguments
                .filter(|argument| match argument {
                    syn::FnArg::Receiver(_) => self.input_impl,
                    syn::FnArg::Typed(argument) => {
                        type_is_nominal_alias(&argument.ty, self.input_aliases)
                    }
                })
                .count()
        }

        fn seed_typed_arguments<'a>(&mut self, arguments: impl Iterator<Item = &'a syn::FnArg>) {
            for argument in arguments {
                match argument {
                    syn::FnArg::Receiver(receiver) if self.input_impl => {
                        self.tainted.insert(
                            receiver
                                .reference
                                .as_ref()
                                .map_or("self", |_| "self")
                                .to_owned(),
                        );
                    }
                    syn::FnArg::Typed(argument)
                        if type_is_nominal_alias(&argument.ty, self.input_aliases) =>
                    {
                        self.tainted.extend(bound_identifiers(&argument.pat));
                    }
                    _ => {}
                }
            }
        }

        fn byte_aliases(&self) -> BTreeSet<String> {
            // The manifest alias is the sole raw value admitted into a call,
            // and only the exact typed decoder below may consume it. Source
            // bytes cross the seam through the validated carrier instead.
            self.manifest_aliases.clone()
        }
    }
    impl<'ast> syn::visit::Visit<'ast> for RawManifestFlow<'_> {
        fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
            let previous_function = self
                .current_function
                .replace(function.sig.ident.to_string());
            let typed_ingresses = self.typed_argument_count(function.sig.inputs.iter());
            self.typed_callable_ingresses += typed_ingresses;
            self.decoder_typed_ingresses +=
                usize::from(function.sig.ident == "decode_instance") * typed_ingresses;
            let previous_tainted = std::mem::take(&mut self.tainted);
            let previous_manifest_aliases = std::mem::take(&mut self.manifest_aliases);
            let previous_source_aliases = std::mem::take(&mut self.source_aliases);
            self.seed_typed_arguments(function.sig.inputs.iter());
            self.visit_block(&function.block);
            self.tainted = previous_tainted;
            self.manifest_aliases = previous_manifest_aliases;
            self.source_aliases = previous_source_aliases;
            self.current_function = previous_function;
        }

        fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
            let previous = self.input_impl;
            self.input_impl = type_is_nominal_alias(&item.self_ty, self.input_aliases);
            syn::visit::visit_item_impl(self, item);
            self.input_impl = previous;
        }

        fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
            let previous_function = self
                .current_function
                .replace(function.sig.ident.to_string());
            let typed_ingresses = self.typed_argument_count(function.sig.inputs.iter());
            self.typed_callable_ingresses += typed_ingresses;
            self.decoder_typed_ingresses +=
                usize::from(function.sig.ident == "decode_instance") * typed_ingresses;
            let previous_tainted = std::mem::take(&mut self.tainted);
            let previous_manifest_aliases = std::mem::take(&mut self.manifest_aliases);
            let previous_source_aliases = std::mem::take(&mut self.source_aliases);
            self.seed_typed_arguments(function.sig.inputs.iter());
            self.visit_block(&function.block);
            self.tainted = previous_tainted;
            self.manifest_aliases = previous_manifest_aliases;
            self.source_aliases = previous_source_aliases;
            self.current_function = previous_function;
        }

        fn visit_expr_closure(&mut self, closure: &'ast syn::ExprClosure) {
            let previous_function = self.current_function.replace("<closure>".to_owned());
            let previous_tainted = self.tainted.clone();
            let previous_manifest_aliases = self.manifest_aliases.clone();
            let previous_source_aliases = self.source_aliases.clone();
            self.typed_callable_ingresses += closure
                .inputs
                .iter()
                .filter(|input| {
                    matches!(
                        input,
                        syn::Pat::Type(typed)
                            if type_is_nominal_alias(&typed.ty, self.input_aliases)
                    )
                })
                .count();
            for input in &closure.inputs {
                if let syn::Pat::Type(typed) = input
                    && type_is_nominal_alias(&typed.ty, self.input_aliases)
                {
                    self.tainted.extend(bound_identifiers(&typed.pat));
                }
            }
            self.visit_expr(&closure.body);
            self.tainted = previous_tainted;
            self.manifest_aliases = previous_manifest_aliases;
            self.source_aliases = previous_source_aliases;
            self.current_function = previous_function;
        }

        fn visit_local(&mut self, local: &'ast syn::Local) {
            let direct_binding = exact_local_binding(&local.pat);
            let mut direct_manifest_alias = false;
            let mut direct_source_alias = false;
            let mut direct_whole_carrier = false;
            if let Some(syn::LocalInit { expr, .. }) = &local.init {
                direct_manifest_alias = direct_binding.as_ref().is_some_and(|_| {
                    self.current_function.as_deref() == Some("decode_instance")
                        && direct_named_field(expr, "input", "manifest_bytes")
                });
                direct_source_alias = direct_binding.as_ref().is_some_and(|_| {
                    self.current_function.as_deref() == Some("decode_instance")
                        && direct_named_field(expr, "input", "source_bytes")
                });
                direct_whole_carrier = direct_tainted_binding(expr, &self.tainted).is_some();
                let byte_aliases = self.byte_aliases();
                let exact_decoder_initializer = exact_decoder_match(expr, &byte_aliases);
                if !direct_manifest_alias && !direct_source_alias && !direct_whole_carrier {
                    self.raw_forwarding_or_escape += usize::from(
                        expression_contains_whole_tainted_binding(expr, &self.tainted),
                    );
                    self.raw_forwarding_or_escape += usize::from(
                        !exact_decoder_initializer
                            && expression_mentions_binding(expr, &byte_aliases),
                    );
                }
            }
            syn::visit::visit_local(self, local);
            if let Some(binding) = direct_binding {
                self.manifest_aliases.remove(&binding);
                self.source_aliases.remove(&binding);
                if direct_manifest_alias {
                    self.manifest_aliases.insert(binding);
                    self.direct_manifest_aliases += 1;
                } else if direct_source_alias {
                    self.source_aliases.insert(binding);
                    self.direct_source_aliases += 1;
                } else if direct_whole_carrier {
                    self.tainted.insert(binding);
                }
            } else if direct_whole_carrier {
                self.tainted.extend(bound_identifiers(&local.pat));
            }
        }

        fn visit_expr_if(&mut self, expression: &'ast syn::ExprIf) {
            let exact = self.current_function.as_deref() == Some("decode_instance")
                && expression.else_branch.is_none()
                && exact_manifest_budget_condition(&expression.cond, &self.manifest_aliases)
                && block_returns_resource_limit_rejection(&expression.then_branch);
            if exact {
                self.exact_manifest_budget_guards += 1;
            }
            syn::visit::visit_expr_if(self, expression);
        }

        fn visit_expr_field(&mut self, field: &'ast syn::ExprField) {
            let tainted_base = expression_mentions_any_ident(&field.base, &self.tainted);
            if tainted_base {
                match &field.member {
                    syn::Member::Named(member) if member == "manifest_bytes" => {
                        self.manifest_field_reads += 1;
                        self.manifest_reads_outside_decoder += usize::from(
                            self.current_function.as_deref() != Some("decode_instance"),
                        );
                    }
                    syn::Member::Named(member) if member == "source_bytes" => {
                        self.source_field_reads += 1;
                        self.source_reads_outside_decoder += usize::from(
                            self.current_function.as_deref() != Some("decode_instance"),
                        );
                    }
                    _ => {}
                }
            }
            syn::visit::visit_expr_field(self, field);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            let typed_decoder = matches!(
                call.func.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none()
                        && path.path.segments.len() == 2
                        && path.path.segments[0].ident == "serde_json"
                        && path.path.segments[1].ident == "from_slice"
                        && decoder_returns_nominal(&path.path, "GrammarInstance")
            );
            let exact = self.current_function.as_deref() == Some("decode_instance")
                && typed_decoder
                && call.args.len() == 1
                && call
                    .args
                    .first()
                    .is_some_and(|argument| exact_binding_path(argument, &self.manifest_aliases));
            self.exact_typed_decoders += usize::from(exact);
            self.raw_forwarding_or_escape += call
                .args
                .iter()
                .filter(|argument| {
                    expression_contains_whole_tainted_binding(argument, &self.tainted)
                })
                .count();
            let byte_aliases = self.byte_aliases();
            let raw_alias_arguments = call
                .args
                .iter()
                .filter(|argument| expression_mentions_binding(argument, &byte_aliases))
                .count();
            self.raw_forwarding_or_escape += raw_alias_arguments.saturating_sub(usize::from(exact));
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.raw_forwarding_or_escape += usize::from(
                expression_contains_whole_tainted_binding(&call.receiver, &self.tainted),
            );
            self.raw_forwarding_or_escape += call
                .args
                .iter()
                .filter(|argument| {
                    expression_contains_whole_tainted_binding(argument, &self.tainted)
                })
                .count();
            let byte_aliases = self.byte_aliases();
            let safe_length_read = call.method == "len"
                && call.args.is_empty()
                && exact_binding_path(&call.receiver, &byte_aliases);
            self.raw_forwarding_or_escape += usize::from(
                !safe_length_read && expression_mentions_binding(&call.receiver, &byte_aliases),
            );
            self.raw_forwarding_or_escape += call
                .args
                .iter()
                .filter(|argument| expression_mentions_binding(argument, &byte_aliases))
                .count();
            syn::visit::visit_expr_method_call(self, call);
        }

        fn visit_expr_assign(&mut self, assign: &'ast syn::ExprAssign) {
            self.raw_forwarding_or_escape += usize::from(
                expression_contains_whole_tainted_binding(&assign.left, &self.tainted)
                    || expression_contains_whole_tainted_binding(&assign.right, &self.tainted),
            );
            let byte_aliases = self.byte_aliases();
            self.raw_forwarding_or_escape += usize::from(
                expression_mentions_binding(&assign.left, &byte_aliases)
                    || expression_mentions_binding(&assign.right, &byte_aliases),
            );
            syn::visit::visit_expr_assign(self, assign);
        }

        fn visit_expr_array(&mut self, array: &'ast syn::ExprArray) {
            let byte_aliases = self.byte_aliases();
            self.raw_forwarding_or_escape += usize::from(
                array
                    .elems
                    .iter()
                    .any(|element| expression_mentions_binding(element, &byte_aliases)),
            );
            syn::visit::visit_expr_array(self, array);
        }

        fn visit_expr_tuple(&mut self, tuple: &'ast syn::ExprTuple) {
            let byte_aliases = self.byte_aliases();
            self.raw_forwarding_or_escape += usize::from(
                tuple
                    .elems
                    .iter()
                    .any(|element| expression_mentions_binding(element, &byte_aliases)),
            );
            syn::visit::visit_expr_tuple(self, tuple);
        }

        fn visit_expr_struct(&mut self, struct_: &'ast syn::ExprStruct) {
            let byte_aliases = self.byte_aliases();
            self.raw_forwarding_or_escape += usize::from(
                struct_
                    .fields
                    .iter()
                    .any(|field| expression_mentions_binding(&field.expr, &byte_aliases))
                    || struct_
                        .rest
                        .as_deref()
                        .is_some_and(|rest| expression_mentions_binding(rest, &byte_aliases)),
            );
            syn::visit::visit_expr_struct(self, struct_);
        }

        fn visit_expr_binary(&mut self, binary: &'ast syn::ExprBinary) {
            let compound_assignment = matches!(
                binary.op,
                syn::BinOp::AddAssign(_)
                    | syn::BinOp::SubAssign(_)
                    | syn::BinOp::MulAssign(_)
                    | syn::BinOp::DivAssign(_)
                    | syn::BinOp::RemAssign(_)
                    | syn::BinOp::BitXorAssign(_)
                    | syn::BinOp::BitAndAssign(_)
                    | syn::BinOp::BitOrAssign(_)
                    | syn::BinOp::ShlAssign(_)
                    | syn::BinOp::ShrAssign(_)
            );
            self.raw_forwarding_or_escape += usize::from(
                compound_assignment
                    && (expression_contains_whole_tainted_binding(&binary.left, &self.tainted)
                        || expression_contains_whole_tainted_binding(&binary.right, &self.tainted)),
            );
            let byte_aliases = self.byte_aliases();
            self.raw_forwarding_or_escape += usize::from(
                !exact_manifest_budget_binary(binary, &byte_aliases)
                    && (expression_mentions_binding(&binary.left, &byte_aliases)
                        || expression_mentions_binding(&binary.right, &byte_aliases)),
            );
            syn::visit::visit_expr_binary(self, binary);
        }

        fn visit_expr_return(&mut self, expression: &'ast syn::ExprReturn) {
            self.raw_forwarding_or_escape +=
                usize::from(expression.expr.as_deref().is_some_and(|value| {
                    expression_contains_whole_tainted_binding(value, &self.tainted)
                }));
            let byte_aliases = self.byte_aliases();
            self.raw_forwarding_or_escape += usize::from(
                expression
                    .expr
                    .as_deref()
                    .is_some_and(|value| expression_mentions_binding(value, &byte_aliases)),
            );
            syn::visit::visit_expr_return(self, expression);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            let tokens = macro_.tokens.to_string();
            let words = tokens
                .split(|character: char| !(character == '_' || character.is_alphanumeric()))
                .filter(|word| !word.is_empty())
                .collect::<BTreeSet<_>>();
            self.raw_manifest_macros += usize::from(words.contains("manifest_bytes"));
            self.raw_forwarding_or_escape += usize::from(
                self.tainted
                    .iter()
                    .any(|binding| words.contains(binding.as_str())),
            );
            self.raw_forwarding_or_escape += usize::from(
                self.manifest_aliases
                    .iter()
                    .any(|binding| words.contains(binding.as_str())),
            );
            syn::visit::visit_macro(self, macro_);
        }
    }
    let mut raw_flow = RawManifestFlow {
        input_aliases: &input_aliases,
        current_function: None,
        input_impl: false,
        tainted: BTreeSet::new(),
        manifest_aliases: BTreeSet::new(),
        source_aliases: BTreeSet::new(),
        manifest_field_reads: 0,
        source_field_reads: 0,
        manifest_reads_outside_decoder: 0,
        source_reads_outside_decoder: 0,
        direct_manifest_aliases: 0,
        direct_source_aliases: 0,
        exact_manifest_budget_guards: 0,
        exact_typed_decoders: 0,
        typed_callable_ingresses: 0,
        decoder_typed_ingresses: 0,
        raw_manifest_macros: 0,
        raw_forwarding_or_escape: 0,
    };
    raw_flow.visit_file(&instance);
    let exact_top_level_raw_spine = exact_top_level_raw_spine(decoder, &carrier);
    if decoder.sig.ident != "decode_instance"
        || raw_flow.manifest_field_reads != 1
        || raw_flow.source_field_reads != 1
        || raw_flow.manifest_reads_outside_decoder != 0
        || raw_flow.source_reads_outside_decoder != 0
        || raw_flow.direct_manifest_aliases != 1
        || raw_flow.exact_manifest_budget_guards != 1
        || raw_flow.exact_typed_decoders != 1
        || !exact_top_level_raw_spine
        || raw_flow.typed_callable_ingresses != 1
        || raw_flow.decoder_typed_ingresses != 1
        || raw_flow.raw_manifest_macros != 0
        || raw_flow.raw_forwarding_or_escape != 0
    {
        findings.insert(format!(
            "raw instance flow is decoder={} manifest_reads={} source_reads={} escaped_manifest_reads={} escaped_source_reads={} direct_manifest_aliases={} direct_source_aliases={} exact_manifest_budget_guards={} exact_typed_decoders={} exact_top_level_raw_spine={} typed_callable_ingresses={} decoder_typed_ingresses={} macro_reads={} forwarding_or_escape={}, expected decode_instance/1/1/0/0/1/(0-or-1)/1/1/true/1/1/0/0",
            decoder.sig.ident,
            raw_flow.manifest_field_reads,
            raw_flow.source_field_reads,
            raw_flow.manifest_reads_outside_decoder,
            raw_flow.source_reads_outside_decoder,
            raw_flow.direct_manifest_aliases,
            raw_flow.direct_source_aliases,
            raw_flow.exact_manifest_budget_guards,
            raw_flow.exact_typed_decoders,
            exact_top_level_raw_spine,
            raw_flow.typed_callable_ingresses,
            raw_flow.decoder_typed_ingresses,
            raw_flow.raw_manifest_macros,
            raw_flow.raw_forwarding_or_escape,
        ));
    }
    if matches!(
        carrier.as_str(),
        "CompiledModule" | "GrammarInstance" | "GrammarInstanceInput" | "SourceModule" | "Value"
    ) {
        findings.insert(format!(
            "instance.rs raw ingress returns unvalidated/non-private carrier `{carrier}`"
        ));
    }
    let carrier_models = instance
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == carrier => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    if carrier_models.len() != 1 || !pub_super(&carrier_models[0].vis) {
        findings.insert(format!(
            "validated carrier `{carrier}` is not one pub(super) instance.rs struct"
        ));
    }
    if let Some(model) = carrier_models.first() {
        let exact = match &model.fields {
            syn::Fields::Named(fields) => {
                let fields = fields.named.iter().collect::<Vec<_>>();
                matches!(
                    fields.as_slice(),
                    [instance, source]
                        if instance.ident.as_ref().is_some_and(|ident| ident == "instance")
                            && matches!(instance.vis, syn::Visibility::Inherited)
                            && exact_ident_type(&instance.ty, "GrammarInstance")
                            && source.ident.as_ref().is_some_and(|ident| ident == "source_bytes")
                            && matches!(source.vis, syn::Visibility::Inherited)
                            && matches!(
                                &source.ty,
                                syn::Type::Reference(reference)
                                    if reference.mutability.is_none()
                                        && matches!(
                                            reference.elem.as_ref(),
                                            syn::Type::Slice(slice)
                                                if exact_ident_type(&slice.elem, "u8")
                                        )
                            )
                )
            }
            _ => false,
        };
        if !exact {
            findings.insert(format!(
                "validated carrier `{carrier}` is not exactly private GrammarInstance plus source_bytes"
            ));
        }
    }

    let compile_instances = compiler
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == "compile_instance" => Some(function),
            _ => None,
        })
        .collect::<Vec<_>>();
    let compile_validated = compiler
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == "compile_validated_instance" => {
                Some(function)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    if compile_instances.len() != 1 || compile_validated.len() != 1 {
        findings.insert(format!(
            "compiler ingress cardinality is compile_instance={} compile_validated_instance={}",
            compile_instances.len(),
            compile_validated.len()
        ));
        return findings;
    }
    let public = compile_instances[0];
    let validated = compile_validated[0];
    if exact_compile_validated_instance_signature(validated).as_deref() != Some(carrier.as_str()) {
        findings.insert(format!(
            "compile_validated_instance does not privately consume `{carrier}` plus CompileOptions"
        ));
    }

    struct PublicFlow<'a> {
        decoder: &'a str,
        carrier_aliases: BTreeSet<String>,
        decoder_calls: usize,
        validated_calls: usize,
        live_validated_calls: usize,
    }
    impl<'ast> syn::visit::Visit<'ast> for PublicFlow<'_> {
        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(syn::LocalInit { expr, .. }) = &local.init
                && (expression_call_count(expr, self.decoder) != 0
                    || self
                        .carrier_aliases
                        .iter()
                        .any(|name| expression_mentions_ident(expr, name)))
            {
                self.carrier_aliases.extend(bound_identifiers(&local.pat));
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            let name = call_name(call);
            if name.as_deref() == Some(self.decoder) {
                self.decoder_calls += 1;
                let path_is_instance = matches!(
                    call.func.as_ref(),
                    syn::Expr::Path(path)
                        if path.path.segments.iter().any(
                            |segment| segment.ident == "instance"
                        )
                );
                if !path_is_instance
                    || call.args.len() != 2
                    || !call
                        .args
                        .first()
                        .is_some_and(|argument| expression_mentions_ident(argument, "input"))
                    || !call
                        .args
                        .last()
                        .is_some_and(|argument| expression_mentions_ident(argument, "options"))
                {
                    self.live_validated_calls = usize::MAX;
                }
            }
            if name.as_deref() == Some("compile_validated_instance") {
                self.validated_calls += 1;
                self.live_validated_calls += usize::from(
                    call.args.len() == 2
                        && call.args.first().is_some_and(|argument| {
                            expression_call_count(argument, self.decoder) != 0
                                || self
                                    .carrier_aliases
                                    .iter()
                                    .any(|name| expression_mentions_ident(argument, name))
                        })
                        && call
                            .args
                            .last()
                            .is_some_and(|argument| expression_mentions_ident(argument, "options")),
                );
            }
            syn::visit::visit_expr_call(self, call);
        }
    }
    let decoder_name = decoder.sig.ident.to_string();
    let mut public_flow = PublicFlow {
        decoder: &decoder_name,
        carrier_aliases: BTreeSet::new(),
        decoder_calls: 0,
        validated_calls: 0,
        live_validated_calls: 0,
    };
    public_flow.visit_block(&public.block);
    if public_flow.decoder_calls != 1
        || public_flow.validated_calls != 1
        || public_flow.live_validated_calls != 1
        || block_identifier_count(&public.block, "input") != 1
        || block_tail_call_count(&public.block, "compile_validated_instance") != 1
    {
        findings.insert(
            "compile_instance does not exclusively decode input then tail-return the validated carrier"
                .to_owned(),
        );
    }

    let carrier_parameter = validated
        .sig
        .inputs
        .first()
        .and_then(|argument| match argument {
            syn::FnArg::Typed(argument) => match argument.pat.as_ref() {
                syn::Pat::Ident(binding) => Some(binding.ident.to_string()),
                _ => None,
            },
            _ => None,
        });
    let Some(carrier_parameter) = carrier_parameter else {
        findings.insert("validated carrier parameter is not one direct binding".to_owned());
        return findings;
    };

    struct ValidatedFlow {
        carrier_aliases: BTreeSet<String>,
        program_aliases: BTreeSet<String>,
        resolve_calls: usize,
        live_resolve_calls: usize,
        accepted_program_fields: usize,
        compiled_structs: usize,
    }
    impl<'ast> syn::visit::Visit<'ast> for ValidatedFlow {
        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(syn::LocalInit { expr, .. }) = &local.init {
                let bindings = bound_identifiers(&local.pat);
                if self
                    .carrier_aliases
                    .iter()
                    .any(|name| expression_mentions_ident(expr, name))
                {
                    self.carrier_aliases.extend(bindings.iter().cloned());
                }
                if expression_call_count(expr, "resolve") != 0
                    || self
                        .program_aliases
                        .iter()
                        .any(|name| expression_mentions_ident(expr, name))
                {
                    self.program_aliases.extend(bindings);
                }
            }
            syn::visit::visit_local(self, local);
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            let exact_resolve = matches!(
                call.func.as_ref(),
                syn::Expr::Path(path)
                    if path.qself.is_none()
                        && path.path.segments.len() == 2
                        && path.path.segments[0].ident == "Program"
                        && path.path.segments[1].ident == "resolve"
            );
            if exact_resolve {
                self.resolve_calls += 1;
                self.live_resolve_calls += usize::from(
                    call.args.len() == 2
                        && call.args.iter().all(|argument| {
                            self.carrier_aliases
                                .iter()
                                .any(|name| expression_mentions_ident(argument, name))
                        }),
                );
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            if expression.path.is_ident("CompiledModule") {
                self.compiled_structs += 1;
                let program = expression.fields.iter().find(|field| {
                    matches!(
                        &field.member,
                        syn::Member::Named(ident) if ident == "program"
                    )
                });
                self.accepted_program_fields += usize::from(program.is_some_and(|field| {
                    self.program_aliases
                        .iter()
                        .any(|name| expression_mentions_ident(&field.expr, name))
                }));
            }
            syn::visit::visit_expr_struct(self, expression);
        }
    }
    let mut validated_flow = ValidatedFlow {
        carrier_aliases: BTreeSet::from([carrier_parameter]),
        program_aliases: BTreeSet::new(),
        resolve_calls: 0,
        live_resolve_calls: 0,
        accepted_program_fields: 0,
        compiled_structs: 0,
    };
    validated_flow.visit_block(&validated.block);
    if validated_flow.resolve_calls != 1
        || validated_flow.live_resolve_calls != 1
        || validated_flow.compiled_structs != 1
        || validated_flow.accepted_program_fields != 1
    {
        findings.insert(
            "validated carrier does not drive the sole Program::resolve into the sole CompiledModule"
                .to_owned(),
        );
    }

    findings
}

fn source_module_authority_findings(source: &str) -> BTreeSet<String> {
    let syntax = syn::parse_file(source).expect("source-module authority source is valid Rust");
    let modules = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == "SourceModule" => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut findings = BTreeSet::new();
    if modules.len() != 1 {
        findings.insert(format!(
            "expected one SourceModule definition, found {}",
            modules.len()
        ));
        return findings;
    }
    let syn::Fields::Named(fields) = &modules[0].fields else {
        findings.insert("SourceModule does not use named fields".to_owned());
        return findings;
    };
    let names = fields
        .named
        .iter()
        .filter_map(|field| field.ident.as_ref())
        .map(ToString::to_string)
        .collect::<BTreeSet<_>>();
    let expected = BTreeSet::from([
        "nodes".to_owned(),
        "revision".to_owned(),
        "rules".to_owned(),
    ]);
    if names != expected {
        findings.insert(format!(
            "SourceModule fields are {names:?}, expected {expected:?}"
        ));
    }
    findings
}

#[test]
fn grammar_instance_ingress_and_validation_ownership_are_exact() {
    fn is_borrowed_bytes(ty: &syn::Type, lifetime: &str) -> bool {
        let syn::Type::Reference(reference) = ty else {
            return false;
        };
        if reference.mutability.is_some()
            || reference
                .lifetime
                .as_ref()
                .is_none_or(|candidate| candidate.ident != lifetime)
        {
            return false;
        }
        matches!(
            reference.elem.as_ref(),
            syn::Type::Slice(slice)
                if matches!(
                    slice.elem.as_ref(),
                    syn::Type::Path(path)
                        if path.qself.is_none() && path.path.is_ident("u8")
                )
        )
    }

    let _const_constructor_proof = CONST_EMPTY_INSTANCE;
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let instance_path = manifest_dir.join("src/compiler/instance.rs");
    let metadata = fs::symlink_metadata(&instance_path).expect("compiler/instance.rs metadata");
    assert!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "compiler/instance.rs is one repository-owned regular file"
    );
    let instance_source = fs::read_to_string(&instance_path).expect("compiler/instance.rs source");
    let syntax = syn::parse_file(&instance_source).expect("compiler/instance.rs syntax");

    let inputs = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == "GrammarInstanceInput" => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(inputs.len(), 1, "one GrammarInstanceInput definition");
    let input = inputs[0];
    assert!(
        matches!(input.vis, syn::Visibility::Public(_)),
        "GrammarInstanceInput is public"
    );
    assert_eq!(input.generics.params.len(), 1);
    assert!(matches!(
        input.generics.params.first(),
        Some(syn::GenericParam::Lifetime(lifetime)) if lifetime.lifetime.ident == "a"
    ));
    let syn::Fields::Named(fields) = &input.fields else {
        panic!("GrammarInstanceInput uses two named borrowed fields");
    };
    assert_eq!(
        fields
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string())
            .collect::<Vec<_>>(),
        ["manifest_bytes", "source_bytes"]
    );
    for field in &fields.named {
        assert!(
            matches!(field.vis, syn::Visibility::Inherited),
            "GrammarInstanceInput fields remain private"
        );
        assert!(
            is_borrowed_bytes(&field.ty, "a"),
            "GrammarInstanceInput fields are exact immutable byte borrows"
        );
    }

    let mut forbidden_derives = BTreeSet::new();
    for attribute in &input.attrs {
        if attribute.path().is_ident("derive") {
            attribute
                .parse_nested_meta(|meta| {
                    if matches!(
                        meta.path
                            .segments
                            .last()
                            .map(|segment| segment.ident.to_string())
                            .as_deref(),
                        Some("Default" | "Deserialize")
                    ) {
                        forbidden_derives.insert(
                            meta.path
                                .segments
                                .last()
                                .expect("derive path is nonempty")
                                .ident
                                .to_string(),
                        );
                    }
                    Ok(())
                })
                .expect("GrammarInstanceInput derive syntax");
        }
    }
    assert!(
        forbidden_derives.is_empty(),
        "GrammarInstanceInput cannot be synthesized by {forbidden_derives:?}"
    );
    let input_impls = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item)
                if matches!(
                    item.self_ty.as_ref(),
                    syn::Type::Path(path)
                        if path.path.segments.last().is_some_and(
                            |segment| segment.ident == "GrammarInstanceInput"
                        )
                ) =>
            {
                Some(item)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        input_impls.len(),
        1,
        "GrammarInstanceInput has one inherent constructor impl and no trait synthesis"
    );
    assert!(
        input_impls[0].trait_.is_none(),
        "GrammarInstanceInput implements no Default/From/Deserialize or other synthesis trait"
    );
    assert_eq!(
        input_impls[0].items.len(),
        1,
        "GrammarInstanceInput exposes only its exact constructor"
    );

    let public_methods = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item)
                if matches!(
                    item.self_ty.as_ref(),
                    syn::Type::Path(path)
                        if path.path.segments.last().is_some_and(
                            |segment| segment.ident == "GrammarInstanceInput"
                        )
                ) =>
            {
                Some(item)
            }
            _ => None,
        })
        .flat_map(|item| &item.items)
        .filter_map(|item| match item {
            syn::ImplItem::Fn(method) if matches!(method.vis, syn::Visibility::Public(_)) => {
                Some(method)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(public_methods.len(), 1, "one public instance-input method");
    let constructor = public_methods[0];
    assert_eq!(constructor.sig.ident, "new");
    assert!(
        constructor.sig.constness.is_some(),
        "GrammarInstanceInput::new is const"
    );
    let constructor_arguments = constructor
        .sig
        .inputs
        .iter()
        .filter_map(|argument| match argument {
            syn::FnArg::Typed(argument) => Some(argument),
            syn::FnArg::Receiver(_) => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(constructor_arguments.len(), 2);
    for (argument, expected) in constructor_arguments
        .iter()
        .zip(["manifest_bytes", "source_bytes"])
    {
        assert!(matches!(
            argument.pat.as_ref(),
            syn::Pat::Ident(binding) if binding.ident == expected
        ));
        assert!(is_borrowed_bytes(&argument.ty, "a"));
    }
    assert!(matches!(
        &constructor.sig.output,
        syn::ReturnType::Type(_, ty)
            if matches!(
                ty.as_ref(),
                syn::Type::Path(path) if path.path.is_ident("Self")
            )
    ));

    let typed_models = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == "GrammarInstance" => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(typed_models.len(), 1, "one typed GrammarInstance model");
    assert!(
        matches!(
            &typed_models[0].vis,
            syn::Visibility::Restricted(restricted)
                if restricted.path.is_ident("super")
        ),
        "the decoded GrammarInstance model is pub(super), shared only inside compiler"
    );
    let syn::Fields::Named(model_fields) = &typed_models[0].fields else {
        panic!("GrammarInstance uses named typed fields");
    };
    assert!(
        model_fields
            .named
            .iter()
            .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
        "GrammarInstance representation fields remain encapsulated in instance.rs"
    );
    assert_eq!(
        model_fields
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string())
            .collect::<Vec<_>>(),
        [
            "schema",
            "logical_path",
            "entry",
            "trivia_regex",
            "pratt_tables",
        ],
        "the typed instance has exactly the five representation fields"
    );
    let model_field = |name: &str| {
        model_fields
            .named
            .iter()
            .find(|field| field.ident.as_ref().is_some_and(|ident| ident == name))
            .unwrap_or_else(|| panic!("GrammarInstance has `{name}`"))
    };
    for name in ["schema", "logical_path", "entry"] {
        assert!(
            exact_ident_type(&model_field(name).ty, "String"),
            "GrammarInstance::{name} is an owned typed String"
        );
    }
    assert!(
        exact_generic_ident(&model_field("trivia_regex").ty, "Option", "String"),
        "GrammarInstance::trivia_regex is exactly Option<String>"
    );
    let table_type = exact_generic_inner_ident(&model_field("pratt_tables").ty, "Vec")
        .expect("GrammarInstance::pratt_tables is Vec<one private typed table>");
    let table_models = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == table_type => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        table_models.len(),
        1,
        "one typed instance Pratt-table model"
    );
    assert!(
        pub_super(&table_models[0].vis),
        "the name-bearing instance Pratt table is compiler-private pub(super)"
    );
    let syn::Fields::Named(table_fields) = &table_models[0].fields else {
        panic!("instance Pratt table uses named typed fields");
    };
    assert!(
        table_fields
            .named
            .iter()
            .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
        "instance Pratt-table fields remain private to instance.rs"
    );
    assert_eq!(
        table_fields
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string())
            .collect::<Vec<_>>(),
        ["site_rule", "atom_rule", "operators"],
        "the instance Pratt-table model has exactly the representation fields"
    );
    let table_field = |name: &str| {
        table_fields
            .named
            .iter()
            .find(|field| field.ident.as_ref().is_some_and(|ident| ident == name))
            .unwrap_or_else(|| panic!("instance Pratt table has `{name}`"))
    };
    for name in ["site_rule", "atom_rule"] {
        assert!(
            exact_ident_type(&table_field(name).ty, "String"),
            "instance Pratt table `{name}` is an owned typed String"
        );
    }
    let operator_type = exact_generic_inner_ident(&table_field("operators").ty, "Vec")
        .expect("instance Pratt-table operators are Vec<one private typed operator>");
    let operator_models = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Struct(item) if item.ident == operator_type => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        operator_models.len(),
        1,
        "one typed instance Pratt-operator model"
    );
    assert!(
        pub_super(&operator_models[0].vis),
        "the name-bearing instance Pratt operator is compiler-private pub(super)"
    );
    let syn::Fields::Named(operator_fields) = &operator_models[0].fields else {
        panic!("instance Pratt operator uses named typed fields");
    };
    assert!(
        operator_fields
            .named
            .iter()
            .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
        "instance Pratt-operator fields remain private to instance.rs"
    );
    assert_eq!(
        operator_fields
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string())
            .collect::<Vec<_>>(),
        [
            "matcher_rule",
            "fixity",
            "left_binding_power",
            "right_binding_power",
        ],
        "the instance Pratt-operator model has exactly the representation fields"
    );
    let operator_field = |name: &str| {
        operator_fields
            .named
            .iter()
            .find(|field| field.ident.as_ref().is_some_and(|ident| ident == name))
            .unwrap_or_else(|| panic!("instance Pratt operator has `{name}`"))
    };
    assert!(
        exact_ident_type(&operator_field("matcher_rule").ty, "String"),
        "instance Pratt matcher is an owned typed String"
    );
    for name in ["left_binding_power", "right_binding_power"] {
        assert!(
            exact_generic_ident(&operator_field(name).ty, "Option", "u16"),
            "instance Pratt `{name}` is exactly Option<u16>"
        );
    }
    let fixity_type = match &operator_field("fixity").ty {
        syn::Type::Path(path) if path.qself.is_none() && path.path.segments.len() == 1 => {
            path.path.get_ident().expect("one fixity type").to_string()
        }
        _ => panic!("instance Pratt fixity is one private enum"),
    };
    let fixity_models = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Enum(item) if item.ident == fixity_type => Some(item),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(fixity_models.len(), 1, "one typed instance fixity enum");
    assert!(
        pub_super(&fixity_models[0].vis),
        "the name-bearing instance fixity is compiler-private pub(super)"
    );
    assert_eq!(
        fixity_models[0]
            .variants
            .iter()
            .map(|variant| variant.ident.to_string())
            .collect::<Vec<_>>(),
        ["Prefix", "Postfix", "Infix"],
        "instance fixity is exactly the closed three-variant representation enum"
    );
    assert!(
        fixity_models[0]
            .variants
            .iter()
            .all(|variant| matches!(variant.fields, syn::Fields::Unit)
                && variant.discriminant.is_none()),
        "instance fixity variants carry no raw JSON or hidden metadata"
    );
    let shared_accessors = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item)
                if item.trait_.is_none()
                    && matches!(
                        item.self_ty.as_ref(),
                        syn::Type::Path(path)
                            if path.path.is_ident("GrammarInstance")
                    ) =>
            {
                Some(item)
            }
            _ => None,
        })
        .flat_map(|item| &item.items)
        .filter_map(|item| match item {
            syn::ImplItem::Fn(method)
                if matches!(
                    method.vis,
                    syn::Visibility::Restricted(ref restricted)
                        if restricted.path.is_ident("super")
                ) =>
            {
                Some(method.sig.ident.to_string())
            }
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    assert!(
        ["entry", "pratt_tables"]
            .into_iter()
            .all(|required| shared_accessors.contains(required)),
        "syntax.rs receives entry and Pratt data only through exact typed accessors: {shared_accessors:?}"
    );
    assert!(
        instance_source.contains("bbnf.grammar-instance.v1")
            && instance_source.contains("serde_json"),
        "instance.rs owns the schema identity and duplicate-aware decoder"
    );

    let syntax_source =
        fs::read_to_string(manifest_dir.join("src/compiler/syntax.rs")).expect("syntax source");
    let syntax_file = syn::parse_file(&syntax_source).expect("syntax.rs syntax");
    assert!(
        syntax_has_real_instance_resolution(&syntax_file),
        "Program::resolve imports and immutably borrows the real sibling GrammarInstance"
    );
    for relative in [
        "src/compiler/frontend.rs",
        "src/compiler/emit.rs",
        "src/bin/bbnf-gen.rs",
    ] {
        let source = fs::read_to_string(manifest_dir.join(relative))
            .unwrap_or_else(|error| panic!("{relative}: {error}"));
        assert!(
            !rust_identifiers(&source).contains("GrammarInstance"),
            "{relative} must not duplicate or own the typed manifest model"
        );
    }
}

#[test]
fn clean_break_and_manifest_ownership_scanners_are_self_falsifying() {
    assert!(
        module_graph_union_findings(
            "crates/core/src/compiler/example.rs",
            "mod sibling; fn local() {}"
        )
        .is_empty(),
        "ordinary repository-local modules are covered by the lexical superset"
    );
    assert!(
        module_graph_union_findings(
            "crates/core/src/compiler/example.rs",
            "fn emit() -> TokenStream { (quote! { pub mod generated {} }) }"
        )
        .is_empty(),
        "expression-position token emission is data, not an enclosing source-module edge"
    );
    for source in [
        r#"#[path = "../../../outside.rs"] mod hidden;"#,
        r#"#[cfg_attr(feature = "hidden", path = "../../../outside.rs")] mod hidden;"#,
        r#"include!("../../../outside.rs");"#,
        r#"include!(concat!(env!("OUT_DIR"), "/generated.rs"));"#,
        r#"macro_rules! hidden { () => { mod outside; } }"#,
        r#"macro_rules! hidden { () => { include!("outside.rs"); } }"#,
        r#"
macro_rules! edge { ($($tokens:tt)*) => { $($tokens)* } }
edge!(#[path = "../../../outside.rs"] mod hidden;);
"#,
        r#"
macro_rules! edge { ($ignored:ident, $($tokens:tt)*) => { $($tokens)* } }
edge!(ignored, #[path = "../../../outside.rs"] mod hidden;);
"#,
        r#"
macro_rules! edge { ($meta:meta, $($tail:tt)*) => { #[$meta] $($tail)* } }
edge!(path = "../../../outside.rs", mod hidden;);
"#,
        r#"
macro_rules! edge { (# $junk:ident, $($tail:tt)*) => { $($tail)* } }
edge!(# ignored, mod hidden;);
"#,
        r#"
fn local() {
    macro_rules! edge { ($($tail:tt)*) => { $($tail)* } }
    edge! { #[path = "/tmp/nonexistent-bbnf-outside.rs"] mod hidden; }
}
"#,
    ] {
        assert!(
            !module_graph_union_findings("crates/core/src/compiler/example.rs", source).is_empty(),
            "unsealed path/cfg/include/macro module edge must be rejected: `{source}`"
        );
    }

    assert!(
        legacy_surface_findings(
            r#"
const NOTE: &str = "ModuleInput and compile_module are historical prose";
struct ModuleInputNotes;
fn compile_module_notes() {}
"#
        )
        .is_empty(),
        "prose and noncolliding identifiers are not legacy declarations"
    );
    assert!(
        legacy_surface_findings(
            r#"fn prose() { assert!(matches!("ModuleInput", "compile_module")); }"#
        )
        .is_empty(),
        "ordinary macro arguments containing historical prose are not generated declarations"
    );
    for source in [
        "struct ModuleInput;",
        "fn compile_module() {}",
        "struct CompilationSession;",
        "enum EditError {}",
        "fn stale() { Revision::for_module(\"x\", b\"y\"); }",
        "impl Revision { fn for_module() {} }",
        "fn stale() { R::for_module(); }",
        "macro_rules! stale { () => { fn compile_module() {} } }",
    ] {
        assert!(
            !legacy_surface_findings(source).is_empty(),
            "legacy scanner must reject `{source}`"
        );
    }
    let legacy_domain_source = ["const DOMAIN: &[u8] = b\"bbnf.w2.source", ".v4\\0\";"].concat();
    assert!(
        !legacy_surface_findings(&legacy_domain_source).is_empty(),
        "legacy scanner must reject the W2 revision domain"
    );

    assert!(
        !manifest_ownership_findings(
            "crates/core/src/compiler/frontend.rs",
            r#"
struct GrammarInstance;
fn decode(bytes: &[u8]) {
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = "bbnf.grammar-instance.v1";
}
"#
        )
        .is_empty(),
        "a copied frontend model/decoder/schema authority must be rejected"
    );
    for source in [
        r#"
use serde_json as json;
fn decode(bytes: &[u8]) { let _ = json::from_reader(bytes); }
"#,
        r#"
extern crate serde_json as json;
fn decode(bytes: &[u8]) { let _ = json::from_slice::<Value>(bytes); }
"#,
        r#"
use serde_json::from_slice as decode;
fn copied(bytes: &[u8]) { let _ = decode::<Value>(bytes); }
"#,
        r#"
fn copied(value: Value) {
    let decode = serde_json::from_value;
    let _ = decode(value);
}
"#,
        r#"
use serde_json::Deserializer as JsonDecoder;
fn copied(bytes: &[u8]) {
    let mut decoder = JsonDecoder::from_slice(bytes);
    let _ = Value::deserialize(&mut decoder);
}
"#,
        r#"
mod nested {
    use serde_json::de::Deserializer as Decoder;
    fn copied(bytes: &[u8]) {
        let mut decoder = Decoder::from_slice(bytes);
        let _ = serde::Deserialize::deserialize(&mut decoder);
    }
}
"#,
        r#"
use serde_json::de as json_de;
fn copied(bytes: &[u8]) {
    let mut decoder = json_de::Deserializer::from_slice(bytes);
    let _ = Value::deserialize(&mut decoder);
}
"#,
        r#"
type Decoder<'a> = serde_json::Deserializer<serde_json::de::SliceRead<'a>>;
fn copied(bytes: &[u8]) {
    let mut decoder = Decoder::from_slice(bytes);
    let _ = Value::deserialize(&mut decoder);
}
"#,
        r#"
use serde_json::Deserializer as JsonDecoder;
type Decoder<'a> = JsonDecoder<serde_json::de::SliceRead<'a>>;
fn copied(bytes: &[u8]) {
    let mut decoder = Decoder::from_slice(bytes);
    let _ = Value::deserialize(&mut decoder);
}
"#,
        r#"
use serde::Deserialize as Decode;
fn copied(decoder: &mut Decoder) {
    let _ = Decode::deserialize(decoder);
}
"#,
        r#"
fn copied(bytes: &[u8]) {
    let key = b"logical_path";
    let _ = bytes.windows(key.len()).position(|window| window == key);
}
"#,
        r#"
fn copied(bytes: &[u8]) {
    let key = b"logical\x5fpath";
    let _ = bytes.windows(key.len()).position(|window| window == key);
}
"#,
        r#"
fn copied(bytes: &[u8]) {
    let key = [108_u8, 111, 103, 105, 99, 97, 108, 95, 112, 97, 116, 104];
    let _ = bytes.windows(key.len()).position(|window| window == key);
}
"#,
        r#"
const DECODE: fn(&[u8]) -> Result<Value, Error> = serde_json::from_slice;
fn copied(bytes: &[u8]) { let _ = DECODE(bytes); }
"#,
        r#"
static DECODE: fn(&[u8]) -> Result<Value, Error> = serde_json::from_slice;
fn copied(bytes: &[u8]) { let _ = DECODE(bytes); }
"#,
        r#"
macro_rules! copied {
    ($bytes:expr) => { serde_json::from_slice::<Value>($bytes) };
}
"#,
        r#"
macro_rules! copied {
    () => {
        struct CopiedInstance {
            schema: String,
            logical_path: String,
            entry: String,
            trivia_regex: Option<String>,
            pratt_tables: Vec<Table>,
        }
    };
}
"#,
        r#"
fn copied(bytes: &[u8]) {
    let key = concat!("logical", "_path");
    let _ = bytes.windows(key.len()).position(|window| window == key.as_bytes());
}
"#,
        r#"
struct CopiedInstance {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
"#,
    ] {
        assert!(
            !manifest_ownership_findings("crates/core/src/compiler/frontend.rs", source).is_empty(),
            "decoder/model ownership scanner must reject `{source}`"
        );
    }
    assert!(
        manifest_ownership_findings(
            "crates/core/src/compiler/emit.rs",
            r#"fn encode<T: serde::Serialize>(value: &T) { let _ = serde_json::to_string(value); }"#
        )
        .is_empty(),
        "format-neutral serialization is not manifest decoding"
    );
    assert!(
        manifest_ownership_findings(
            "crates/core/src/backend/rust/emitter/shapes/hregex.rs",
            r#"
fn emit() {
    let grammar_suffix = "control";
    let _ = quote! {
        let decoded = core::str::from_utf8(input).unwrap_or("");
        generated_for_grammar(grammar_suffix, decoded)
    };
}
"#
        )
        .is_empty(),
        "punctuation-separated generated-code vocabulary cannot synthesize manifest authority"
    );
    assert!(
        manifest_ownership_findings(
            "crates/core/src/compiler/analysis.rs",
            r#"
fn explain(table: &PrattTable) -> String {
    format!("atom is {}", table.atom_rule())
}
"#
        )
        .is_empty(),
        "ordinary format arguments may consume typed Pratt accessors without owning the manifest"
    );
    assert!(
        !manifest_ownership_findings(
            "crates/core/src/compiler/replay.rs",
            r#"fn replay(input: Input, options: Options) { compile_instance(input, options); }"#
        )
        .is_empty(),
        "trusted replay must not re-enter through the public ingress"
    );
    assert!(
        manifest_ownership_findings(
            "crates/core/src/compiler/replay.rs",
            r#"
use serde_json::Value;
fn replay_compilation(packet: &[u8]) {
    let _supplied = serde_json::from_slice::<Value>(packet);
    let canonical_packet = Vec::<u8>::new();
    let _canonical = serde_json::from_slice::<Value>(&canonical_packet);
}
"#
        )
        .is_empty(),
        "replay may decode only supplied and compiler-canonical envelopes into Value"
    );
    for source in [
        r#"
use serde_json::Value;
fn replay_compilation(packet: &[u8]) {
    let _ = serde_json::from_reader::<_, Value>(packet);
}
"#,
        r#"
fn replay_compilation(packet: &[u8]) {
    let _ = serde_json::from_slice::<GrammarInstance>(packet);
}
"#,
        r#"
use serde_json::Value;
fn replay_compilation(manifest_bytes: &[u8]) {
    let _ = serde_json::from_slice::<Value>(manifest_bytes);
}
"#,
        r#"
use serde_json::Value;
fn replay_compilation(packet: &[u8]) {
    let value = serde_json::from_slice::<Value>(packet).unwrap();
    let _ = value.get("logical_path");
}
"#,
    ] {
        assert!(
            !manifest_ownership_findings("crates/core/src/compiler/replay.rs", source).is_empty(),
            "replay decoder scanner must reject `{source}`"
        );
    }

    assert!(
        manifest_ownership_findings(
            "crates/core/src/compiler/instance.rs",
            r#"
struct GrammarInstance {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
fn decode(bytes: &[u8]) {
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = InvalidGrammarInstance;
    let _ = "bbnf.grammar-instance.v1";
}
"#
        )
        .is_empty(),
        "instance.rs is the sole five-field model/decoder/local-validation owner"
    );
    for source in [
        r#"
struct GrammarInstance {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
struct RawManifest {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
fn decode(bytes: &[u8]) {
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = InvalidGrammarInstance;
    let _ = "bbnf.grammar-instance.v1";
}
"#,
        r#"
struct GrammarInstance {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
fn decode(bytes: &[u8]) {
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = InvalidGrammarInstance;
    let _ = "bbnf.grammar-instance.v1";
}
"#,
        r#"
struct GrammarInstance {
    raw: Value,
}
struct DecoyManifest {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
fn decode(bytes: &[u8]) {
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = InvalidGrammarInstance;
    let _ = "bbnf.grammar-instance.v1";
}
"#,
    ] {
        assert!(
            !manifest_ownership_findings("crates/core/src/compiler/instance.rs", source).is_empty(),
            "instance.rs must reject a second typed model or decoder"
        );
    }
    assert!(
        !manifest_ownership_findings(
            "crates/core/src/compiler/instance.rs",
            r#"
struct GrammarInstance {
    schema: String,
    logical_path: String,
    entry: String,
    trivia_regex: Option<String>,
    pratt_tables: Vec<Table>,
}
fn decode(bytes: &[u8], program: Program) {
    let _ = serde_json::from_slice::<GrammarInstance>(bytes);
    let _ = InvalidGrammarInstance;
    let _ = program;
}
"#
        )
        .is_empty(),
        "instance.rs must not absorb source-dependent Program resolution"
    );
    assert!(
        manifest_ownership_findings(
            "crates/core/src/compiler/syntax.rs",
            "fn resolve() { let _ = UnknownEntry; let _ = InvalidPrattTable; }"
        )
        .is_empty(),
        "syntax.rs owns source-dependent entry and Pratt diagnostics"
    );

    let exact_sibling_source = r#"
use super::instance::GrammarInstance;
struct Program;
struct SourceModule;
struct DiagnosticCode;
impl DiagnosticCode {
    const UnknownEntry: Self = Self;
    const InvalidPrattTable: Self = Self;
}
impl Program {
    pub(super) fn resolve(
        module: &SourceModule,
        instance: &GrammarInstance,
    ) -> StageResult<Self> {
        let entry_name = instance.entry();
        let entry = module.rules
            .iter()
            .find(|rule| rule.name == entry_name)
            .map(|rule| rule.id)
            .ok_or(DiagnosticCode::UnknownEntry)?;
        let mut nodes = module.nodes.clone();
        let mut rules = module.rules.clone();
        for table in instance.pratt_tables() {
            let site = table.site_rule();
            let atom = table.atom_rule();
            for operator in table.operators() {
                let _ = (
                    operator.matcher_rule(),
                    operator.fixity(),
                    operator.left_binding_power(),
                    operator.right_binding_power(),
                );
            }
            nodes.push(NodeKindData::Pratt { atom, table: 0 });
            rules.get_mut(site).unwrap().root = nodes.len() - 1;
            let _ = DiagnosticCode::InvalidPrattTable;
        }
        StageResult::accepted(Self { nodes, rules, entry })
    }
}
"#;
    let exact_sibling_use =
        syn::parse_file(exact_sibling_source).expect("exact sibling-use control parses");
    assert!(syntax_has_real_instance_resolution(&exact_sibling_use));
    let crate_visible_resolver =
        exact_sibling_source.replacen("pub(super) fn resolve", "pub(crate) fn resolve", 1);
    assert_ne!(
        crate_visible_resolver, exact_sibling_source,
        "resolver visibility falsifier must mutate the exact control"
    );
    assert!(
        !syntax_has_real_instance_resolution(
            &syn::parse_file(&crate_visible_resolver)
                .expect("crate-visible resolver falsifier parses")
        ),
        "Program::resolve authority remains minimally visible to its compiler parent"
    );
    for mutated in [
        exact_sibling_source.replacen(
            "pub(super) fn resolve(",
            "#[inline]\n    pub(super) fn resolve(",
            1,
        ),
        exact_sibling_source.replacen("pub(super) fn resolve(", "pub(super) async fn resolve(", 1),
        exact_sibling_source.replacen("pub(super) fn resolve(", "pub(super) unsafe fn resolve(", 1),
        exact_sibling_source.replacen(
            "pub(super) fn resolve(",
            "pub(super) extern \"C\" fn resolve(",
            1,
        ),
        exact_sibling_source.replacen(
            "pub(super) fn resolve(",
            "pub(super) fn resolve<'unused>(",
            1,
        ),
        exact_sibling_source.replacen(
            "        module: &SourceModule,",
            "        mut module: &SourceModule,",
            1,
        ),
        exact_sibling_source.replacen(
            "        instance: &GrammarInstance,",
            "        ref instance: &GrammarInstance,",
            1,
        ),
        exact_sibling_source.replacen(
            "        module: &SourceModule,",
            "        #[allow(unused)] module: &SourceModule,",
            1,
        ),
        exact_sibling_source.replacen(
            "        instance: &GrammarInstance,",
            "        instance: &'borrow GrammarInstance,",
            1,
        ),
        exact_sibling_source.replacen(
            "        module: &SourceModule,",
            "        module @ _: &SourceModule,",
            1,
        ),
    ] {
        assert_ne!(
            mutated, exact_sibling_source,
            "resolver-signature falsifier must mutate the exact control"
        );
        assert!(
            !syntax_has_real_instance_resolution(
                &syn::parse_file(&mutated).expect("resolver-signature falsifier parses")
            ),
            "Program::resolve remains one ordinary nongeneric unadorned function"
        );
    }
    let exact_name_map_source = exact_sibling_source.replacen(
        r#"        let entry_name = instance.entry();
        let entry = module.rules
            .iter()
            .find(|rule| rule.name == entry_name)
            .map(|rule| rule.id)
            .ok_or(DiagnosticCode::UnknownEntry)?;
"#,
        r#"        let entry_name = instance.entry();
        let mut names = BTreeMap::new();
        for rule in &module.rules {
            names.insert(rule.name.clone(), rule.id);
        }
        let Some(&entry) = names.get(entry_name) else {
            return StageResult::rejected(Diagnostic::new(
                DiagnosticCode::UnknownEntry,
                0,
                0,
                "missing entry",
            ));
        };
"#,
        1,
    );
    assert_ne!(
        exact_name_map_source, exact_sibling_source,
        "name-map control must replace the equality resolver"
    );
    assert!(
        syntax_has_real_instance_resolution(
            &syn::parse_file(&exact_name_map_source).expect("exact name-map control parses")
        ),
        "the exact populated name-map lookup is a live entry-resolution authority"
    );
    let helper_backed_pratt_diagnostic = [
        exact_name_map_source.replacen(
            "let _ = DiagnosticCode::InvalidPrattTable;",
            "let _ = invalid_pratt_table(\"invalid table\");",
            1,
        ),
        r#"
fn invalid_pratt_table(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::InvalidPrattTable, 0, 0, message)
}
"#
        .to_owned(),
    ]
    .concat();
    assert!(
        syntax_has_real_instance_resolution(
            &syn::parse_file(&helper_backed_pratt_diagnostic)
                .expect("helper-backed Pratt diagnostic control parses")
        ),
        "an exact typed helper may own the InvalidPrattTable constructor used by the live loop"
    );
    for (law, mutated) in [
        (
            "wrong helper diagnostic code",
            helper_backed_pratt_diagnostic.replacen(
                "Diagnostic::new(DiagnosticCode::InvalidPrattTable, 0, 0, message)",
                "Diagnostic::new(DiagnosticCode::UnknownEntry, 0, 0, message)",
                1,
            ),
        ),
        (
            "early-returning helper",
            helper_backed_pratt_diagnostic.replacen(
                "    Diagnostic::new(DiagnosticCode::InvalidPrattTable, 0, 0, message)",
                "    if true { return Diagnostic::new(DiagnosticCode::UnknownEntry, 0, 0, message); }\n    Diagnostic::new(DiagnosticCode::InvalidPrattTable, 0, 0, message)",
                1,
            ),
        ),
        (
            "inexact helper input",
            helper_backed_pratt_diagnostic.replacen(
                "fn invalid_pratt_table(message: impl Into<String>) -> Diagnostic",
                "fn invalid_pratt_table(message: String) -> Diagnostic",
                1,
            ),
        ),
        (
            "foreign helper call",
            helper_backed_pratt_diagnostic.replacen(
                "invalid_pratt_table(\"invalid table\")",
                "foreign_invalid_pratt_table(\"invalid table\")",
                1,
            ),
        ),
    ] {
        assert_ne!(
            mutated, helper_backed_pratt_diagnostic,
            "Pratt-diagnostic helper falsifier `{law}` must mutate its control"
        );
        assert!(
            !syntax_has_real_instance_resolution(
                &syn::parse_file(&mutated).expect("Pratt-diagnostic helper mutation parses")
            ),
            "Pratt-diagnostic helper falsifier `{law}` must be rejected"
        );
    }
    for (law, mutated) in [
        (
            "derived lookup key",
            exact_name_map_source.replacen(
                "names.get(entry_name)",
                "names.get(&entry_name.len())",
                1,
            ),
        ),
        (
            "derived map key",
            exact_name_map_source.replacen(
                "names.insert(rule.name.clone(), rule.id)",
                "names.insert(rule.name.len(), rule.id)",
                1,
            ),
        ),
        (
            "foreign unpopulated lookup map",
            exact_name_map_source.replacen(
                "names.get(entry_name)",
                "foreign_names.get(entry_name)",
                1,
            ),
        ),
        (
            "dead nested map population",
            exact_name_map_source.replacen(
                "names.insert(rule.name.clone(), rule.id);",
                "if false { names.insert(rule.name.clone(), rule.id); }",
                1,
            ),
        ),
        (
            "map cleared before entry lookup",
            exact_name_map_source.replacen(
                "let Some(&entry) = names.get(entry_name) else {",
                "names.clear();\n        let Some(&entry) = names.get(entry_name) else {",
                1,
            ),
        ),
        (
            "unrelated equality beside an unpopulated map",
            exact_name_map_source.replacen(
                "names.insert(rule.name.clone(), rule.id);",
                "let _decoy = rule.name == entry_name;",
                1,
            ),
        ),
    ] {
        assert_ne!(
            mutated, exact_name_map_source,
            "name-map self-falsifier `{law}` must mutate its control"
        );
        assert!(
            !syntax_has_real_instance_resolution(
                &syn::parse_file(&mutated).expect("name-map mutation parses")
            ),
            "name-map self-falsifier `{law}` must be rejected"
        );
    }
    for source in [
        r#"
type GrammarInstance = ();
struct Program;
impl Program { fn resolve(instance: &GrammarInstance) { let _ = instance; } }
"#,
        r#"
use super::instance::GrammarInstance as Instance;
struct Program;
impl Program { fn resolve(instance: &Instance) { let _ = instance; } }
"#,
        r#"
use super::instance::GrammarInstance;
struct Program;
impl Program {
    fn proof(instance: &GrammarInstance) { let _ = instance; }
    fn resolve(source: &SourceModule) { let _ = source; }
}
"#,
        r#"
use super::instance::GrammarInstance;
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) {
        let _entry = module.find_rule(instance.entry());
        for table in instance.pratt_tables() { module.attach_pratt(table); }
    }
}
"#,
        r#"
use super::instance::GrammarInstance;
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) {
        let _ = (module, instance);
        let _ = (UnknownEntry, InvalidPrattTable);
    }
}
"#,
        r#"
use super::instance::GrammarInstance;
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) {
        let _entry = module.find_rule(instance.entry());
        let _ = (UnknownEntry, InvalidPrattTable);
    }
}
"#,
        r#"
use super::instance::GrammarInstance;
struct Program;
fn moved(module: &SourceModule, instance: &GrammarInstance) {
    let _entry = module.find_rule(instance.entry());
    for table in instance.pratt_tables() { module.attach_pratt(table); }
    let _ = (UnknownEntry, InvalidPrattTable);
}
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) {
        let _ = (module, instance);
        let _ = (UnknownEntry, InvalidPrattTable);
    }
}
"#,
        r#"
use super::instance::GrammarInstance;
struct Program;
impl Program {
    pub(super) fn resolve(
        module: &SourceModule,
        instance: &GrammarInstance,
    ) -> StageResult<Self> {
        let _entry_decoy = instance.entry() == "";
        for _table in instance.pratt_tables() {}
        let _ = (UnknownEntry, InvalidPrattTable);
        StageResult::accepted(Self {
            nodes: module.nodes.clone(),
            rules: module.rules.clone(),
            entry: module.rules[0].id,
        })
    }
    fn build(module: &SourceModule) -> StageResult<Self> { todo!() }
}
"#,
    ] {
        assert!(
            !syntax_has_real_instance_resolution(
                &syn::parse_file(source).expect("sibling-use mutation parses")
            ),
            "dummy/aliased/non-live GrammarInstance use must not satisfy ownership"
        );
    }
    for (index, source) in [
        exact_sibling_source.replacen(
            "rule.name == entry_name",
            "rule.name.len() == entry_name.len()",
            1,
        ),
        exact_sibling_source.replacen("operator.fixity(),", "Fixity::Prefix,", 1),
        exact_sibling_source.replacen(
            "StageResult::accepted(Self { nodes, rules, entry })",
            "nodes.clear(); rules.clear(); StageResult::accepted(Self { nodes, rules, entry })",
            1,
        ),
        exact_sibling_source
            .replacen(
                "let mut nodes = module.nodes.clone();",
                "let forged_entry = entry_name;\n        let mut nodes = module.nodes.clone();",
                1,
            )
            .replacen(
                "StageResult::accepted(Self { nodes, rules, entry })",
                "StageResult::accepted(Self { nodes, rules, entry: forged_entry })",
                1,
            ),
        exact_sibling_source.replacen(
            "StageResult::accepted(Self { nodes, rules, entry })",
            "StageResult::accepted(Self { nodes: choose(nodes), rules, entry })",
            1,
        ),
        exact_sibling_source
            .replacen("let entry = module.rules", "let mut entry = module.rules", 1)
            .replacen(
                "let mut nodes = module.nodes.clone();",
                "entry = choose_entry();\n        let mut nodes = module.nodes.clone();",
                1,
            ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            nodes.clear();",
            1,
        ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            overwrite(&mut rules);",
            1,
        ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            rules.get_mut(site).unwrap().root = 0;",
            1,
        ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            rules.get_mut(site).unwrap().root += 1;",
            1,
        ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            rules.get_mut(site).unwrap().root.add_assign(1);",
            1,
        ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            nodes.as_mut_slice().reverse();",
            1,
        ),
        exact_sibling_source.replacen(
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;",
            "rules.get_mut(site).unwrap().root = nodes.len() - 1;\n            overwrite!(rules);",
            1,
        ),
        exact_sibling_source.replacen(
            "StageResult::accepted(Self { nodes, rules, entry })",
            "rules[0].root += 1;\n        StageResult::accepted(Self { nodes, rules, entry })",
            1,
        ),
        exact_sibling_source.replacen(
            "StageResult::accepted(Self { nodes, rules, entry })",
            "rules.as_mut_slice().reverse();\n        StageResult::accepted(Self { nodes, rules, entry })",
            1,
        ),
        exact_sibling_source.replacen(
            "StageResult::accepted(Self { nodes, rules, entry })",
            "overwrite!(rules);\n        StageResult::accepted(Self { nodes, rules, entry })",
            1,
        ),
        exact_sibling_source
            .replacen(
                "for table in instance.pratt_tables() {",
                "let rules_ptr = &mut rules as *mut _;\n        for table in instance.pratt_tables() {",
                1,
            )
            .replacen(
                "StageResult::accepted(Self { nodes, rules, entry })",
                "unsafe { (*rules_ptr)[0].root += 1; }\n        StageResult::accepted(Self { nodes, rules, entry })",
                1,
            ),
        exact_sibling_source
            .replacen(
                "for table in instance.pratt_tables() {",
                "let rules_ptr = &mut rules as *mut _;\n        let mut rules_handle = std::ptr::NonNull::new(rules_ptr).unwrap();\n        for table in instance.pratt_tables() {",
                1,
            )
            .replacen(
                "StageResult::accepted(Self { nodes, rules, entry })",
                "unsafe { rules_handle.as_mut().clear(); }\n        StageResult::accepted(Self { nodes, rules, entry })",
                1,
            ),
        exact_sibling_source
            .replacen(
                "for table in instance.pratt_tables() {",
                "let rules_ptr = std::ptr::addr_of_mut!(rules);\n        for table in instance.pratt_tables() {",
                1,
            )
            .replacen(
                "StageResult::accepted(Self { nodes, rules, entry })",
                "unsafe { (*rules_ptr)[0].root += 1; }\n        StageResult::accepted(Self { nodes, rules, entry })",
                1,
            ),
        exact_sibling_source
            .replacen(
                "for table in instance.pratt_tables() {",
                "let rules_ptr = std::ptr::addr_of_mut!(rules);\n        let mut rules_handle = std::ptr::NonNull::new(rules_ptr).unwrap();\n        for table in instance.pratt_tables() {",
                1,
            )
            .replacen(
                "StageResult::accepted(Self { nodes, rules, entry })",
                "unsafe { rules_handle.as_mut().clear(); }\n        StageResult::accepted(Self { nodes, rules, entry })",
                1,
            ),
    ]
    .into_iter()
    .enumerate()
    {
        assert!(
            !syntax_has_real_instance_resolution(
                &syn::parse_file(&source).expect("returned-resolution mutation parses")
            ),
            "non-equal, partial-row, or overwritten instance resolution mutation {index} must be rejected"
        );
    }
    assert_eq!(
        instance_resolution_accessor_count(
            r#"
fn moved(instance: &GrammarInstance) {
    let grammar = instance;
    let _ = grammar.entry();
    for _table in grammar.pratt_tables() {}
}
"#
        ),
        2,
        "renaming the typed instance cannot move entry/Pratt resolution to another module"
    );
    assert_eq!(
        instance_resolution_accessor_count(
            r#"
fn moved(grammar: &GrammarInstance) {
    let _ = grammar.entry();
    for _table in grammar.pratt_tables() {}
}
"#
        ),
        2,
        "typed parameter provenance is independent of the binding spelling"
    );
    assert_eq!(
        instance_resolution_accessor_count(
            r#"
use owner::GrammarInstance as GI;
fn moved(grammar: &GI) {
    let alias = grammar;
    let _ = alias.entry();
    for _table in alias.pratt_tables() {}
}
"#
        ),
        2,
        "use-renamed instance types and local aliases retain accessor provenance"
    );
    let function_wrapped_instance = r#"
use owner::GrammarInstance as GI;
struct Holder { grammar: GI }
fn expose(holder: &Holder) -> &GI { &holder.grammar }
fn moved(holder: &Holder) {
    let _ = expose(holder).entry();
    for _table in expose(holder).pratt_tables() {}
}
"#;
    assert_eq!(
        instance_resolution_accessor_count(function_wrapped_instance),
        2,
        "nominal function returns remain GrammarInstance-tainted at both accessors"
    );
    let method_wrapped_instance = r#"
use owner::GrammarInstance as GI;
struct Holder { grammar: GI }
impl Holder {
    fn expose(&self) -> &GI { &self.grammar }
}
fn moved(holder: &Holder) {
    let _ = holder.expose().entry();
    for _table in holder.expose().pratt_tables() {}
}
"#;
    assert_eq!(
        instance_resolution_accessor_count(method_wrapped_instance),
        2,
        "nominal method returns remain GrammarInstance-tainted at both accessors"
    );
    let field_wrapped_instance = r#"
use owner::GrammarInstance as GI;
struct Holder { grammar: GI }
fn moved(holder: &Holder) {
    let _ = holder.grammar.entry();
    for _table in holder.grammar.pratt_tables() {}
}
"#;
    assert_eq!(
        instance_resolution_accessor_count(field_wrapped_instance),
        2,
        "nominal wrapper fields remain GrammarInstance-tainted at both accessors"
    );
    let assignment_wrapped_instance = r#"
use owner::GrammarInstance as GI;
fn expose() -> GI { todo!() }
fn moved() {
    let grammar;
    grammar = expose();
    let _ = grammar.entry();
    for _table in grammar.pratt_tables() {}
}
"#;
    assert_eq!(
        instance_resolution_accessor_count(assignment_wrapped_instance),
        2,
        "assignment cannot erase nominal GrammarInstance provenance"
    );
    let cross_file_producer = r#"
use owner::GrammarInstance as GI;
fn expose(validated: &Validated) -> &GI { validated.instance() }
"#;
    let cross_file_consumer = r#"
fn moved(validated: &Validated) {
    let _ = expose(validated).entry();
    for _table in expose(validated).pratt_tables() {}
}
"#;
    let cross_file_catalog = instance_access_catalog([cross_file_producer, cross_file_consumer]);
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(cross_file_consumer, &cross_file_catalog,),
        2,
        "a nominal producer definition taints accessor consumers in another active module"
    );
    let cross_file_ufcs_producer = r#"
use owner::GrammarInstance as GI;
struct Validated { instance: GI }
impl Validated {
    fn instance(&self) -> &GI { &self.instance }
}
"#;
    let cross_file_ufcs_consumer = r#"
fn moved(validated: &Validated) {
    let _ = Validated::instance(validated).entry();
    for _table in Validated::instance(validated).pratt_tables() {}
}
"#;
    let cross_file_ufcs_catalog =
        instance_access_catalog([cross_file_ufcs_producer, cross_file_ufcs_consumer]);
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            cross_file_ufcs_consumer,
            &cross_file_ufcs_catalog,
        ),
        2,
        "UFCS invocation of a nominal method producer retains cross-file instance provenance"
    );
    let qualified_trait_producer = r#"
use owner::GrammarInstance as GI;
trait Expose { fn expose(&self) -> &GI; }
struct Holder { grammar: GI }
impl Expose for Holder {
    fn expose(&self) -> &GI { &self.grammar }
}
"#;
    let qualified_trait_consumer = r#"
fn moved(holder: &Holder) {
    let _ = <Holder as Expose>::expose(holder).entry();
    for _table in <Holder as Expose>::expose(holder).pratt_tables() {}
}
"#;
    let qualified_trait_catalog =
        instance_access_catalog([qualified_trait_producer, qualified_trait_consumer]);
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            qualified_trait_consumer,
            &qualified_trait_catalog,
        ),
        2,
        "qualified trait invocation of a nominal producer cannot erase instance provenance"
    );
    let associated_output_producer = r#"
use owner::GrammarInstance as GI;
trait Expose {
    type Output;
    fn expose(&self) -> &Self::Output;
}
struct Holder { grammar: GI }
impl Expose for Holder {
    type Output = GI;
    fn expose(&self) -> &Self::Output { &self.grammar }
}
struct TextHolder { text: String }
impl Expose for TextHolder {
    type Output = String;
    fn expose(&self) -> &Self::Output { &self.text }
}
"#;
    let associated_output_consumer = r#"
fn moved(holder: &Holder) {
    let _ = <Holder as Expose>::expose(holder).entry();
    for _table in <Holder as Expose>::expose(holder).pratt_tables() {}
}
"#;
    let associated_output_catalog =
        instance_access_catalog([associated_output_producer, associated_output_consumer]);
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            associated_output_consumer,
            &associated_output_catalog,
        ),
        2,
        "an associated Output bound to GrammarInstance cannot hide qualified producer access"
    );
    let associated_output_callable_consumer = r#"
fn moved(holder: &Holder) {
    let accessor = <Holder as Expose>::expose;
    let _ = accessor(holder).entry();
    for _table in accessor(holder).pratt_tables() {}
}
"#;
    let associated_output_callable_catalog = instance_access_catalog([
        associated_output_producer,
        associated_output_callable_consumer,
    ]);
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            associated_output_callable_consumer,
            &associated_output_callable_catalog,
        ),
        2,
        "callable aliases of associated-output UFCS producers retain instance provenance"
    );
    let unrelated_associated_output_consumer = r#"
fn moved(holder: &TextHolder) {
    let _ = <TextHolder as Expose>::expose(holder).entry();
    for _table in <TextHolder as Expose>::expose(holder).pratt_tables() {}
}
"#;
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            unrelated_associated_output_consumer,
            &associated_output_catalog,
        ),
        0,
        "same-trait and same-method Output=String sibling stays untainted by receiver identity"
    );
    let unrelated_associated_output_callable_consumer = r#"
fn moved(holder: &TextHolder) {
    let accessor = <TextHolder as Expose>::expose;
    let _ = accessor(holder).entry();
    for _table in accessor(holder).pratt_tables() {}
}
"#;
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            unrelated_associated_output_callable_consumer,
            &associated_output_catalog,
        ),
        0,
        "assigned UFCS aliases preserve the associated producer's exact receiver identity"
    );
    let cross_file_callable_producer = r#"
use owner::GrammarInstance as GI;
fn expose(validated: &Validated) -> &GI { validated.instance() }
"#;
    let cross_file_callable_consumer = r#"
fn moved(validated: &Validated) {
    let accessor;
    accessor = expose;
    let _ = accessor(validated).entry();
    for _table in accessor(validated).pratt_tables() {}
}
"#;
    let cross_file_callable_catalog =
        instance_access_catalog([cross_file_callable_producer, cross_file_callable_consumer]);
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(
            cross_file_callable_consumer,
            &cross_file_callable_catalog,
        ),
        2,
        "assigned callable aliases of nominal producers retain cross-file instance provenance"
    );
    for source in [
        function_wrapped_instance,
        method_wrapped_instance,
        field_wrapped_instance,
        r#"
trait Expose { type Output; fn expose(&self) -> Self::Output; }
impl Expose for Holder {
    type Output = GrammarInstance;
    fn expose(&self) -> Self::Output { todo!() }
}
"#,
    ] {
        assert!(
            !grammar_instance_type_surface_findings("crates/core/src/compiler/other.rs", source,)
                .is_empty(),
            "GrammarInstance wrapper/return type surfaces are confined to exact owners"
        );
    }

    let sole_program_constructor = r#"
struct Program;
impl Program {
    pub(super) fn resolve(
        module: &SourceModule,
        instance: &GrammarInstance,
    ) -> StageResult<Self> {
        todo!()
    }
    pub fn entry(&self) -> RuleId { todo!() }
}
"#;
    assert!(
        program_construction_findings(
            "crates/core/src/compiler/syntax.rs",
            sole_program_constructor
        )
        .is_empty(),
        "Program::resolve is the sole typed Program construction authority"
    );
    for source in [
        r#"
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
    fn build(module: &SourceModule) -> StageResult<Self> { todo!() }
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
}
fn build(module: &SourceModule) -> StageResult<Program> { todo!() }
"#,
        r#"
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
}
fn hidden(module: &SourceModule) {
    let _ = Program { entry: module.rules[0].id };
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
}
macro_rules! hidden {
    () => { impl Program { fn build() -> StageResult<Self> { todo!() } } };
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
}
macro_rules! hidden {
    ($entry:expr) => { Program { entry: $entry } };
}
"#,
        r#"
struct Program { entry: RuleId }
type ProgramAlias = Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
}
fn build(module: &SourceModule) -> ProgramAlias {
    ProgramAlias { entry: module.rules[0].id }
}
"#,
        r#"
struct Program { entry: RuleId }
use self::Program as ProgramAlias;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) -> StageResult<Self> { todo!() }
}
fn build(module: &SourceModule) -> ProgramAlias {
    ProgramAlias { entry: module.rules[0].id }
}
"#,
    ] {
        assert!(
            !program_construction_findings("crates/core/src/compiler/syntax.rs", source).is_empty(),
            "alternate/discarded Program constructor must be rejected: `{source}`"
        );
    }

    let exact_compile_call = r#"
fn compile_validated_instance(module: &SourceModule, instance: &GrammarInstance) {
    let _program = Program::resolve(module, instance);
}
"#;
    assert_eq!(
        compiler_mod_instance_resolution_calls(exact_compile_call),
        1,
        "the compile spine passes the real typed instance into Program::resolve"
    );
    for source in [
        r#"
fn compile_validated_instance(module: &SourceModule, instance: &GrammarInstance) {
    let _program = Program::resolve(module);
    let _ = instance;
}
"#,
        r#"
fn elsewhere(module: &SourceModule, instance: &GrammarInstance) {
    let _program = Program::resolve(module, instance);
}
"#,
        r#"
fn compile_validated_instance(module: &SourceModule, instance: &GrammarInstance) {
    let _program = resolve(module, instance);
}
"#,
    ] {
        assert_eq!(
            compiler_mod_instance_resolution_calls(source),
            0,
            "a missing, moved, or indirect instance-resolution call must not satisfy the spine"
        );
    }

    let exact_signature = syn::parse_file(
        r#"
pub fn compile_instance(
    input: GrammarInstanceInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    todo!()
}
"#,
    )
    .expect("exact ingress signature parses");
    let syn::Item::Fn(exact_signature) = &exact_signature.items[0] else {
        unreachable!();
    };
    assert!(exact_compile_instance_signature(exact_signature));
    for source in [
        "pub fn compile_instance<T>(input: T, options: CompileOptions) -> StageResult<CompiledModule> { todo!() }",
        "pub fn compile_instance(input: GrammarInstanceInput<'_>, options: impl Into<CompileOptions>) -> StageResult<CompiledModule> { todo!() }",
        "pub fn compile_instance(input: GrammarInstanceInput<'_>, options: CompileOptions) -> CompiledModule { todo!() }",
    ] {
        let syntax = syn::parse_file(source).expect("ingress mutation parses");
        let syn::Item::Fn(function) = &syntax.items[0] else {
            unreachable!();
        };
        assert!(
            !exact_compile_instance_signature(function),
            "broadened ingress must be rejected: `{source}`"
        );
    }

    assert!(
        compile_ingress_surface_findings(
            "crates/core/src/compiler/mod.rs",
            r#"
pub fn compile_instance(
    input: GrammarInstanceInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> { todo!() }
fn compile_validated_instance(
    validated: ValidatedGrammarInstance<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> { todo!() }
"#
        )
        .is_empty(),
        "the public ingress and one private validated replay spine are the exact compile surface"
    );
    assert!(
        compile_ingress_surface_findings(
            "crates/core/src/compiler/replay.rs",
            "pub fn replay_compilation(packet: &[u8]) -> Result<CompiledModule, ReplayError> { todo!() }"
        )
        .is_empty(),
        "the versioned replay crossing is not a manifest-less compiler adapter"
    );
    for source in [
        "pub fn build(source: &[u8]) -> StageResult<CompiledModule> { todo!() }",
        "pub use compile_instance as compile_source;",
        "pub use compiler as legacy_compiler;",
        "pub use compiler::*;",
        "impl Default for GrammarInstanceInput<'static> { fn default() -> Self { todo!() } }",
        "impl<'a> From<&'a [u8]> for GrammarInstanceInput<'a> { fn from(source: &'a [u8]) -> Self { todo!() } }",
        "type HiddenCompiler = fn(&[u8]) -> StageResult<CompiledModule>;",
        "static BUILD: fn(Input, Options) -> StageResult<CompiledModule> = compile_instance;",
        "trait Build { fn build(&self, source: &[u8]) -> StageResult<CompiledModule>; }",
        "mod hidden { pub fn build(source: &[u8]) -> StageResult<CompiledModule> { todo!() } }",
        "macro_rules! hidden { () => { pub fn build() -> StageResult<CompiledModule> { todo!() } } }",
        "impl CompiledModule { pub fn from_source(source: &[u8]) -> StageResult<Self> { todo!() } }",
        "impl Build for Builder { type Output = CompiledModule; fn build(&self) -> Self::Output { todo!() } }",
        "impl CompiledModule { hidden_ingress!(); }",
        "pub fn compile_validated_instance(validated: Validated<'_>, options: CompileOptions) -> StageResult<CompiledModule> { todo!() }",
        "use GrammarInstanceInput as GI; impl Default for GI<'static> { fn default() -> Self { GI::new(b\"\", b\"\") } }",
        "use CompiledModule as CM; fn build(source: &[u8]) -> StageResult<CM> { todo!() }",
        "use compile_instance as build; fn hidden(input: GrammarInstanceInput<'_>, options: CompileOptions) { let _ = build(input, options); }",
    ] {
        assert!(
            !compile_ingress_surface_findings("crates/core/src/compiler/other.rs", source)
                .is_empty(),
            "renamed, aliased, trait-synthesized, or nested ingress must be rejected: `{source}`"
        );
    }

    let exact_instance_seam = r#"
pub struct GrammarInstanceInput<'a> {
    manifest_bytes: &'a [u8],
    source_bytes: &'a [u8],
}
pub(super) struct Validated<'a> {
    instance: GrammarInstance,
    source_bytes: &'a [u8],
}
pub(super) fn decode_instance<'a>(
    input: GrammarInstanceInput<'a>,
    options: CompileOptions,
) -> StageResult<Validated<'a>> {
    let manifest_bytes = input.manifest_bytes;
    let source_bytes = input.source_bytes;
    if manifest_bytes.len() > options.instance_bytes {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::ResourceLimit,
            0,
            0,
            "instance too large",
        ));
    }
    let instance = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {
        Ok(instance) => instance,
        Err(error) => {
            let message = error.to_string();
            let code = if message.contains(INVALID_PRATT_REPRESENTATION) {
                DiagnosticCode::InvalidPrattTable
            } else {
                DiagnosticCode::InvalidGrammarInstance
            };
            return StageResult::rejected(Diagnostic::new(code, 0, 0, message));
        }
    };
    StageResult::accepted(Validated {
        instance,
        source_bytes,
    })
}
"#;
    let exact_compiler_spine = r#"
pub fn compile_instance(
    input: GrammarInstanceInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    let validated = match instance::decode_instance(input, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    compile_validated_instance(validated, options)
}
fn compile_validated_instance(
    validated: Validated<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    let module = frontend::parse_source(validated.source_bytes, options);
    let instance = validated.instance;
    let program = match Program::resolve(&module, &instance) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return StageResult::Rejected { diagnostics };
        }
    };
    StageResult::accepted(CompiledModule { program })
}
"#;
    assert!(
        validated_instance_spine_findings(exact_instance_seam, exact_compiler_spine).is_empty(),
        "one private validated carrier connects raw decode to the live compiled Program"
    );
    let second_raw_ingress = [
        exact_instance_seam,
        "\npub(super) fn decode_again(input: GrammarInstanceInput<'_>, options: CompileOptions) -> StageResult<Validated<'_>> { todo!() }\n",
    ]
    .concat();
    let public_validated = exact_compiler_spine.replacen(
        "fn compile_validated_instance(",
        "pub fn compile_validated_instance(",
        1,
    );
    let inspected_input = exact_compiler_spine.replacen(
        "let validated =",
        "inspect(&input);\n    let validated =",
        1,
    );
    let discarded_program = exact_compiler_spine.replacen(
        "StageResult::accepted(CompiledModule { program })",
        "let legacy_program = Program::legacy(&module);\n    let _ = program;\n    StageResult::accepted(CompiledModule { program: legacy_program })",
        1,
    );
    let raw_carrier = exact_instance_seam.replacen(
        "instance: GrammarInstance,",
        "instance: GrammarInstance,\n    raw: Value,",
        1,
    );
    let raw_byte_carrier = exact_instance_seam.replacen(
        r#"pub(super) struct Validated<'a> {
    instance: GrammarInstance,
    source_bytes: &'a [u8],
}"#,
        r#"pub(super) struct Validated<'a> {
    instance: GrammarInstance,
    source_bytes: &'a [u8],
    manifest_bytes: &'a [u8],
}"#,
        1,
    );
    let raw_string_carrier = exact_instance_seam.replacen(
        "instance: GrammarInstance,",
        "instance: GrammarInstance,\n    raw_manifest: String,",
        1,
    );
    let forwarded_manifest = exact_instance_seam.replacen(
        "serde_json::from_slice::<GrammarInstance>(manifest_bytes)",
        "foreign::decode(manifest_bytes)",
        1,
    );
    let duplicated_manifest_read = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    inspect(input.manifest_bytes);",
        1,
    );
    let borrowed_raw_helper = [
        exact_instance_seam,
        "\nfn inspect(raw: &GrammarInstanceInput<'_>) { let _ = raw.manifest_bytes; }\n",
    ]
    .concat();
    let method_raw_helper = [
        exact_instance_seam,
        "\nimpl GrammarInstanceInput<'_> { fn inspect(&self) { let _ = self.manifest_bytes; } }\n",
    ]
    .concat();
    let closure_raw_helper = [
        exact_instance_seam,
        "\nfn hidden() { let inspect = |raw: &GrammarInstanceInput<'_>| raw.source_bytes.len(); let _ = inspect; }\n",
    ]
    .concat();
    let unused_raw_method_ingress = [
        exact_instance_seam,
        "\nstruct Helper; impl Helper { fn ignored(&self, _raw: &GrammarInstanceInput<'_>) {} }\n",
    ]
    .concat();
    let aliased_raw_helper = [
        exact_instance_seam,
        "\ntype GI<'a> = GrammarInstanceInput<'a>; fn inspect(raw: &GI<'_>) { foreign(raw); }\n",
    ]
    .concat();
    let macro_raw_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    inspect!(input);",
        1,
    );
    let wrapped_raw_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let _hidden = Holder { raw: input };",
        1,
    );
    let assigned_raw_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let leak;\n    leak = &input;\n    foreign(leak);",
        1,
    );
    let destructuring_assignment_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let (mut leak, mut spare) = (None, None);\n    (leak, spare) = (Some(&input), None);\n    foreign(leak);",
        1,
    );
    let ref_pattern_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let ref leak = input;\n    foreign(leak);",
        1,
    );
    let assigned_store_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    holder.raw = Some(&input);",
        1,
    );
    let method_argument_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    foreign.store(&input);",
        1,
    );
    let compound_assignment_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    input += replacement;",
        1,
    );
    let returned_raw_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let escape = || -> &GrammarInstanceInput<'_> { return &input; };\n    foreign(escape());",
        1,
    );
    let array_manifest_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let _leaked = [manifest_bytes];",
        1,
    );
    let tuple_manifest_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let _leaked = (manifest_bytes,);",
        1,
    );
    let struct_manifest_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let _leaked = Holder { raw: manifest_bytes };",
        1,
    );
    let binary_manifest_escape = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    let _leaked = manifest_bytes == b\"\";",
        1,
    );
    for (law, mutated) in [
        ("array manifest escape", array_manifest_escape.as_str()),
        ("tuple manifest escape", tuple_manifest_escape.as_str()),
        ("struct manifest escape", struct_manifest_escape.as_str()),
        ("binary manifest escape", binary_manifest_escape.as_str()),
    ] {
        assert_ne!(
            mutated, exact_instance_seam,
            "aggregate raw-manifest self-falsifier `{law}` must mutate the accepted seam"
        );
        assert!(
            !validated_instance_spine_findings(mutated, exact_compiler_spine).is_empty(),
            "aggregate raw-manifest self-falsifier `{law}` must be rejected"
        );
    }

    let exact_decoder_statement = r#"    let instance = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {
        Ok(instance) => instance,
        Err(error) => {
            let message = error.to_string();
            let code = if message.contains(INVALID_PRATT_REPRESENTATION) {
                DiagnosticCode::InvalidPrattTable
            } else {
                DiagnosticCode::InvalidGrammarInstance
            };
            return StageResult::rejected(Diagnostic::new(code, 0, 0, message));
        }
    };
"#;
    let early_return_before_guard = exact_instance_seam.replacen(
        "    if manifest_bytes.len() > options.instance_bytes {",
        "    return StageResult::rejected();\n    if manifest_bytes.len() > options.instance_bytes {",
        1,
    );
    let forged_source_carrier =
        exact_instance_seam.replacen("        source_bytes,\n", "        source_bytes: &[],\n", 1);
    let indexed_before_cap = exact_instance_seam.replacen(
        "    if manifest_bytes.len() > options.instance_bytes {",
        "    manifest_bytes[0];\n    if manifest_bytes.len() > options.instance_bytes {",
        1,
    );
    let accepted_decoder_error = exact_instance_seam.replacen(
        "            return StageResult::rejected(Diagnostic::new(code, 0, 0, message));",
        "            return StageResult::accepted(Validated { instance: forged_instance, source_bytes: &[] });",
        1,
    );
    let bottom_decoder_error = exact_instance_seam.replacen(
        "            return StageResult::rejected(Diagnostic::new(code, 0, 0, message));",
        "            return StageResult::rejected(loop {});",
        1,
    );
    let exit_decoder_error = exact_instance_seam.replacen(
        "            return StageResult::rejected(Diagnostic::new(code, 0, 0, message));",
        "            return StageResult::rejected(std::process::exit(0));",
        1,
    );
    let decoder_shadows_source = exact_instance_seam
        .replacen(
            "    let instance = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {",
            "    let source_bytes = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {",
            1,
        )
        .replacen(
            "        instance,\n        source_bytes,",
            "        instance: source_bytes,\n        source_bytes,",
            1,
        );
    for (law, mutated) in [
        (
            "unchanged decoder after an early top-level return",
            early_return_before_guard.as_str(),
        ),
        (
            "validated carrier with forged source bytes",
            forged_source_carrier.as_str(),
        ),
        (
            "bare indexed raw-manifest use before the cap",
            indexed_before_cap.as_str(),
        ),
        (
            "decoder error arm accepts a forged carrier",
            accepted_decoder_error.as_str(),
        ),
        (
            "decoder error rejection argument diverges forever",
            bottom_decoder_error.as_str(),
        ),
        (
            "decoder error rejection argument exits the process",
            exit_decoder_error.as_str(),
        ),
        (
            "decoder binding shadows the source authority",
            decoder_shadows_source.as_str(),
        ),
    ] {
        assert_ne!(
            mutated, exact_instance_seam,
            "live raw-spine self-falsifier `{law}` must mutate the accepted seam"
        );
        assert!(
            !validated_instance_spine_findings(mutated, exact_compiler_spine).is_empty(),
            "live raw-spine self-falsifier `{law}` must be rejected"
        );
    }
    let decoder_in_guard = exact_instance_seam
        .replacen(
            r#"    if manifest_bytes.len() > options.instance_bytes {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::ResourceLimit,
            0,
            0,
            "instance too large",
        ));
    }
"#,
            r#"    if manifest_bytes.len() > options.instance_bytes {
        return StageResult::rejected(
            serde_json::from_slice::<GrammarInstance>(manifest_bytes)
        );
    }
"#,
            1,
        )
        .replacen(
            exact_decoder_statement,
            "    let instance = forged_instance;\n",
            1,
        );
    let decoder_in_false_branch = exact_instance_seam.replacen(
        exact_decoder_statement,
        r#"    if false {
        let _hidden = serde_json::from_slice::<GrammarInstance>(manifest_bytes);
    }
    let instance = forged_instance;
"#,
        1,
    );
    let decoder_in_closure = exact_instance_seam.replacen(
        exact_decoder_statement,
        r#"    let _hidden =
        || serde_json::from_slice::<GrammarInstance>(manifest_bytes);
    let instance = forged_instance;
"#,
        1,
    );
    let decoder_in_async = exact_instance_seam.replacen(
        exact_decoder_statement,
        r#"    let _hidden = async {
        serde_json::from_slice::<GrammarInstance>(manifest_bytes)
    };
    let instance = forged_instance;
"#,
        1,
    );
    let decoder_after_return = exact_instance_seam.replacen(
        exact_decoder_statement,
        r#"    return StageResult::rejected();
    let _hidden = serde_json::from_slice::<GrammarInstance>(manifest_bytes);
    let instance = forged_instance;
"#,
        1,
    );
    let decoder_in_nested_branch = exact_instance_seam.replacen(
        exact_decoder_statement,
        r#"    if condition {
        let _hidden = serde_json::from_slice::<GrammarInstance>(manifest_bytes);
    }
    let instance = forged_instance;
"#,
        1,
    );
    let forged_parallel_instance = exact_instance_seam
        .replacen(
            "let instance = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {",
            "let decoded = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {",
            1,
        )
        .replacen(
            "        instance,\n        source_bytes,",
            "        instance: forged_instance,\n        source_bytes,",
            1,
        );
    for (law, mutated) in [
        ("decoder inside the cap guard", decoder_in_guard.as_str()),
        (
            "decoder inside an if-false branch",
            decoder_in_false_branch.as_str(),
        ),
        ("decoder inside a closure", decoder_in_closure.as_str()),
        ("decoder inside an async block", decoder_in_async.as_str()),
        (
            "decoder after an unconditional return",
            decoder_after_return.as_str(),
        ),
        (
            "decoder inside a nested live branch",
            decoder_in_nested_branch.as_str(),
        ),
        (
            "parallel forged instance bypasses decoded binding",
            forged_parallel_instance.as_str(),
        ),
    ] {
        assert_ne!(
            mutated, exact_instance_seam,
            "control-flow raw-manifest self-falsifier `{law}` must mutate the accepted seam"
        );
        assert!(
            !validated_instance_spine_findings(mutated, exact_compiler_spine).is_empty(),
            "control-flow raw-manifest self-falsifier `{law}` must be rejected"
        );
    }

    let foreign_rejection_cap = exact_instance_seam.replacen(
        "return StageResult::rejected(Diagnostic::new(",
        "return ForeignStage::rejected(Diagnostic::new(",
        1,
    );
    let foreign_diagnostic_cap = exact_instance_seam.replacen(
        "return StageResult::rejected(Diagnostic::new(",
        "return StageResult::rejected(ForeignDiagnostic::new(",
        1,
    );
    let coincidental_resource_limit_cap = exact_instance_seam.replacen(
        r#"            DiagnosticCode::ResourceLimit,
            0,
            0,"#,
        r#"            DiagnosticCode::Syntax,
            ResourceLimit,
            0,"#,
        1,
    );
    for (law, mutated) in [
        (
            "foreign terminal-name rejected call",
            foreign_rejection_cap.as_str(),
        ),
        (
            "foreign diagnostic constructor",
            foreign_diagnostic_cap.as_str(),
        ),
        (
            "coincidental ResourceLimit identifier",
            coincidental_resource_limit_cap.as_str(),
        ),
    ] {
        assert_ne!(
            mutated, exact_instance_seam,
            "resource-cap self-falsifier `{law}` must mutate the accepted seam"
        );
        assert!(
            !validated_instance_spine_findings(mutated, exact_compiler_spine).is_empty(),
            "resource-cap self-falsifier `{law}` must be rejected"
        );
    }
    let unrelated_same_name_alias = exact_instance_seam
        .replacen(
            "let manifest_bytes = input.manifest_bytes;",
            "let manifest_bytes = unrelated_bytes;\n    let direct_manifest_bytes = input.manifest_bytes;",
            1,
        )
        .replacen(
            "if manifest_bytes.len() > options.instance_bytes {",
            "if direct_manifest_bytes.len() > options.instance_bytes {",
            1,
        );
    let foreign_manifest_alias = exact_instance_seam.replacen(
        "let manifest_bytes = input.manifest_bytes;",
        "let manifest_bytes = foreign::identity(input.manifest_bytes);",
        1,
    );
    let missing_manifest_cap = exact_instance_seam.replacen(
        r#"    if manifest_bytes.len() > options.instance_bytes {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::ResourceLimit,
            0,
            0,
            "instance too large",
        ));
    }
"#,
        "",
        1,
    );
    let canonicalized_manifest_cap = exact_instance_seam.replacen(
        "if manifest_bytes.len() > options.instance_bytes {",
        "if canonicalize(manifest_bytes).len() > options.instance_bytes {",
        1,
    );
    let second_raw_manifest_field_read = exact_instance_seam.replacen(
        "let source_bytes = input.source_bytes;",
        "let source_bytes = input.source_bytes;\n    inspect(input.manifest_bytes);",
        1,
    );
    let decoder_from_source_bytes = exact_instance_seam.replacen(
        "serde_json::from_slice::<GrammarInstance>(manifest_bytes)",
        "serde_json::from_slice::<GrammarInstance>(source_bytes)",
        1,
    );
    for (law, mutated) in [
        (
            "unrelated same-name alias",
            unrelated_same_name_alias.as_str(),
        ),
        ("indirect foreign alias", foreign_manifest_alias.as_str()),
        ("missing raw-length cap", missing_manifest_cap.as_str()),
        (
            "canonicalized rather than raw-length cap",
            canonicalized_manifest_cap.as_str(),
        ),
        (
            "second raw manifest field read",
            second_raw_manifest_field_read.as_str(),
        ),
        (
            "typed decoder fed source rather than manifest bytes",
            decoder_from_source_bytes.as_str(),
        ),
    ] {
        assert_ne!(
            mutated, exact_instance_seam,
            "raw-manifest self-falsifier `{law}` must mutate the accepted seam"
        );
        assert!(
            !validated_instance_spine_findings(mutated, exact_compiler_spine).is_empty(),
            "raw-manifest self-falsifier `{law}` must be rejected"
        );
    }
    for (index, (instance, compiler)) in [
        (second_raw_ingress.as_str(), exact_compiler_spine),
        (exact_instance_seam, public_validated.as_str()),
        (exact_instance_seam, inspected_input.as_str()),
        (exact_instance_seam, discarded_program.as_str()),
        (raw_carrier.as_str(), exact_compiler_spine),
        (raw_byte_carrier.as_str(), exact_compiler_spine),
        (raw_string_carrier.as_str(), exact_compiler_spine),
        (forwarded_manifest.as_str(), exact_compiler_spine),
        (duplicated_manifest_read.as_str(), exact_compiler_spine),
        (borrowed_raw_helper.as_str(), exact_compiler_spine),
        (method_raw_helper.as_str(), exact_compiler_spine),
        (closure_raw_helper.as_str(), exact_compiler_spine),
        (unused_raw_method_ingress.as_str(), exact_compiler_spine),
        (aliased_raw_helper.as_str(), exact_compiler_spine),
        (macro_raw_escape.as_str(), exact_compiler_spine),
        (wrapped_raw_escape.as_str(), exact_compiler_spine),
        (assigned_raw_escape.as_str(), exact_compiler_spine),
        (
            destructuring_assignment_escape.as_str(),
            exact_compiler_spine,
        ),
        (ref_pattern_escape.as_str(), exact_compiler_spine),
        (assigned_store_escape.as_str(), exact_compiler_spine),
        (method_argument_escape.as_str(), exact_compiler_spine),
        (compound_assignment_escape.as_str(), exact_compiler_spine),
        (returned_raw_escape.as_str(), exact_compiler_spine),
    ]
    .into_iter()
    .enumerate()
    {
        assert!(
            !validated_instance_spine_findings(instance, compiler).is_empty(),
            "duplicated/raw/discarded validated-instance spine mutation {index} must be rejected"
        );
    }

    assert!(
        source_module_authority_findings(
            "struct SourceModule { revision: Revision, nodes: Vec<Node>, rules: Vec<Rule> }"
        )
        .is_empty()
    );
    for source in [
        "struct SourceModule { revision: Revision, path: String, nodes: Vec<Node>, rules: Vec<Rule> }",
        "struct SourceModule { revision: Revision, input: Vec<u8>, nodes: Vec<Node>, rules: Vec<Rule> }",
    ] {
        assert!(
            !source_module_authority_findings(source).is_empty(),
            "renamed raw source/path authority must be rejected"
        );
    }

    let clean_resolution = r#"
struct Program;
impl Program {
    fn resolve(module: &SourceModule, instance: &GrammarInstance) {
        let requested = instance.entry();
        let _entry = module.rules.iter().find(|production| production.name == requested);
        let _diagnostic = format!("root rule lookup uses {:?}", module.rules.get(1));
    }
}
fn unrelated(productions: Vec<ProductionData>) {
    let historical = "root";
    let _ = (historical, productions.first());
}
#[cfg(test)]
fn exact_test_only(productions: Vec<ProductionData>) {
    let historical = "root";
    let _ = (historical, productions.first());
}
"#;
    assert!(
        resolution_fallback_findings(clean_resolution, "struct SourceModule;").is_empty(),
        "fallback-like operations outside the production resolution closure are irrelevant"
    );
    for source in [
        r#"struct Program; impl Program { fn resolve(rows: Rows) { let _ = rows.first(); } }"#,
        r#"struct Program; impl Program { fn resolve(rows: Rows) { let _ = rows.iter().next(); } }"#,
        r#"struct Program; impl Program { fn resolve(rows: Rows) { let _ = rows.get(0); } }"#,
        r#"struct Program; impl Program { fn resolve(rows: Rows) { let _ = rows.iter().nth(0); } }"#,
        r#"struct Program; impl Program { fn resolve(rows: Rows) { let _ = &rows[0]; } }"#,
        r#"struct Program; impl Program { fn resolve(rows: Rows) { let _ = rows.position(|_| true); } }"#,
        r#"struct Program; impl Program { fn resolve(name: Name) { let alias = "root"; let _ = name == alias; } }"#,
        r#"
struct Program;
impl Program { fn resolve(rows: Rows) { choose(rows); } }
fn choose(rows: Rows) { let renamed = rows; let _ = renamed.first(); }
"#,
        r#"
struct Program;
struct Rows;
impl Program { fn resolve(rows: Rows) { rows.find(); } }
impl Rows { fn find(self) { let _ = self.first(); } }
"#,
        r#"
struct Program;
impl Program { fn resolve(rows: Rows) { active(rows); } }
#[cfg(any(test, feature = "legacy"))]
fn active(rows: Rows) { let _ = rows.first(); }
"#,
        r#"
struct Program;
impl Program {
    fn resolve(rows: Rows) {
        let _ = format!("{}", rows.first());
    }
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(rows: Rows) {
        macro_rules! hidden { () => { rows.first() }; }
        let _ = hidden!();
    }
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(rows: Rows) {
        macro_rules! hidden { () => { rows.get(0usize) }; }
        let _ = hidden!();
    }
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(rows: Rows) {
        macro_rules! hidden { () => { Iterator::next(&mut rows) }; }
        let _ = hidden!();
    }
}
"#,
        r#"
struct Program;
impl Program {
    fn resolve(rows: Rows) {
        let choose = hidden_fallback;
        choose(rows);
    }
}
fn hidden_fallback(rows: Rows) { let _ = rows.first(); }
"#,
    ] {
        assert!(
            !resolution_fallback_findings(source, "struct SourceModule;").is_empty(),
            "scope/name-independent entry fallback scanner must reject `{source}`"
        );
    }
}

#[test]
fn bbnf_gen_is_the_exact_three_path_byte_transport_without_identity_dispatch() {
    let temporary = tempfile::tempdir().expect("CLI contract temporary directory");
    let manifest_path = temporary.path().join("misleading.css.instance");
    let source_path = temporary.path().join("misleading.json");
    let output_path = temporary.path().join("generated.rs");
    fs::write(&manifest_path, SIMPLE_MANIFEST).expect("write CLI manifest");
    fs::write(&source_path, SIMPLE_SOURCE).expect("write CLI source");

    let expected = accepted(SIMPLE_MANIFEST, SIMPLE_SOURCE)
        .generated()
        .source()
        .as_bytes()
        .to_vec();
    let output = Command::new(env!("CARGO_BIN_EXE_bbnf-gen"))
        .args([&manifest_path, &source_path, &output_path])
        .output()
        .expect("execute bbnf-gen");
    assert!(
        output.status.success(),
        "exact CLI invocation failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        fs::read(&output_path).expect("read generated CLI output"),
        expected,
        "CLI transports the exact manifest/source bytes through compile_instance"
    );
    for (ordinal, (manifest_name, source_name)) in [
        ("grammar.json.instance", "grammar.css"),
        ("candidate-bnf.regex", "sheets.csv"),
        ("opaque", "no-extension"),
    ]
    .into_iter()
    .enumerate()
    {
        let manifest_path = temporary.path().join(manifest_name);
        let source_path = temporary.path().join(source_name);
        let output_path = temporary.path().join(format!("misleading-{ordinal}.rs"));
        fs::write(&manifest_path, SIMPLE_MANIFEST).expect("write misleading manifest");
        fs::write(&source_path, SIMPLE_SOURCE).expect("write misleading source");
        let output = Command::new(env!("CARGO_BIN_EXE_bbnf-gen"))
            .args([&manifest_path, &source_path, &output_path])
            .output()
            .unwrap_or_else(|error| panic!("execute misleading CLI case {ordinal}: {error}"));
        assert!(
            output.status.success(),
            "misleading CLI case {ordinal} failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(
            fs::read(&output_path).expect("read misleading generated output"),
            expected,
            "CLI output cannot depend on filename, extension, or format-shaped labels"
        );
    }

    #[cfg(unix)]
    {
        use std::ffi::OsString;
        use std::os::unix::ffi::OsStringExt as _;

        let opaque_name = |prefix: &[u8], suffix: u8| {
            let mut bytes = prefix.to_vec();
            bytes.push(0xff);
            bytes.push(suffix);
            OsString::from_vec(bytes)
        };
        let manifest_path = temporary
            .path()
            .join(opaque_name(b"manifest-non-utf8-", b'm'));
        let source_path = temporary
            .path()
            .join(opaque_name(b"source-non-utf8-", b's'));
        let output_path = temporary
            .path()
            .join(opaque_name(b"output-non-utf8-", b'o'));
        let manifest_write = fs::write(&manifest_path, SIMPLE_MANIFEST);
        let source_write = fs::write(&source_path, SIMPLE_SOURCE);
        if manifest_write.is_ok() && source_write.is_ok() {
            let output = Command::new(env!("CARGO_BIN_EXE_bbnf-gen"))
                .args([&manifest_path, &source_path, &output_path])
                .output()
                .expect("execute non-UTF-8 CLI path case");
            assert!(
                output.status.success(),
                "non-UTF-8 CLI path case failed:\n{}",
                String::from_utf8_lossy(&output.stderr)
            );
            assert_eq!(
                fs::read(&output_path).expect("read non-UTF-8 generated output path"),
                expected,
                "CLI preserves all three platform-native path arguments"
            );
        } else {
            assert!(
                manifest_path.to_str().is_none()
                    && source_path.to_str().is_none()
                    && output_path.to_str().is_none(),
                "the path capability probe may skip only genuinely non-UTF-8 names"
            );
            fs::write(
                temporary.path().join("utf8-path-capability-control"),
                SIMPLE_SOURCE,
            )
            .expect("the temporary filesystem remains writable for its UTF-8 control");
        }
    }

    let extra_path = temporary.path().join("extra");
    for arity in [0_usize, 1, 2, 4] {
        let rejected_output = temporary.path().join(format!("rejected-{arity}.rs"));
        let arguments = [&manifest_path, &source_path, &rejected_output, &extra_path];
        let output = Command::new(env!("CARGO_BIN_EXE_bbnf-gen"))
            .args(&arguments[..arity])
            .output()
            .unwrap_or_else(|error| panic!("execute bbnf-gen arity {arity}: {error}"));
        assert!(
            !output.status.success(),
            "bbnf-gen accepted {arity} paths instead of exactly three"
        );
        assert!(
            !rejected_output.exists(),
            "rejected CLI arity {arity} created an output"
        );
    }

    let simple = manifest_value(SIMPLE_MANIFEST);
    let invalid_raw_manifests = [
        duplicate_member_before(SIMPLE_MANIFEST, "entry", 0, &simple["entry"]),
        [SIMPLE_MANIFEST, b"\n{}".as_slice()].concat(),
        vec![0xff],
    ];
    for (index, invalid_manifest) in invalid_raw_manifests.iter().enumerate() {
        assert_default_rejected_with(
            invalid_manifest,
            SIMPLE_SOURCE,
            DiagnosticCode::InvalidGrammarInstance,
        );
        let invalid_manifest_path = temporary.path().join(format!("invalid-{index}.instance"));
        let rejected_output = temporary.path().join(format!("invalid-{index}.rs"));
        fs::write(&invalid_manifest_path, invalid_manifest).expect("write invalid CLI manifest");
        let output = Command::new(env!("CARGO_BIN_EXE_bbnf-gen"))
            .args([&invalid_manifest_path, &source_path, &rejected_output])
            .output()
            .unwrap_or_else(|error| panic!("execute invalid bbnf-gen case {index}: {error}"));
        assert!(
            !output.status.success(),
            "bbnf-gen normalized or accepted invalid raw manifest {index}"
        );
        assert!(
            !rejected_output.exists(),
            "bbnf-gen wrote output for invalid raw manifest {index}"
        );
    }

    let cli_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/bin/bbnf-gen.rs");
    let cli_source = fs::read_to_string(&cli_path).expect("bbnf-gen source");
    let transport_findings = cli_byte_transport_findings(&cli_source);
    assert!(
        transport_findings.is_empty(),
        "bbnf-gen raw bytes have an extra inspection/normalization path: {transport_findings:?}"
    );
    let identifiers = rust_identifiers(&cli_source);
    for forbidden in [
        "logical_path",
        "candidate",
        "candidates",
        "backend",
        "plugin",
        "plugins",
        "format_dispatch",
        "dispatch_format",
    ] {
        assert!(
            !identifiers.contains(forbidden),
            "bbnf-gen contains forbidden identity/dispatch surface `{forbidden}`"
        );
    }
    assert!(
        identifiers.contains("compile_instance") && identifiers.contains("GrammarInstanceInput"),
        "bbnf-gen crosses only the explicit grammar-instance compiler ingress"
    );

    let exact_cli_control = r#"
use std::env;
use std::fs;
use std::path::PathBuf;
use bbnf::{CompileOptions, GrammarInstanceInput, StageResult, compile_instance};

fn main() {
    let mut arguments = env::args_os().skip(1);
    let manifest_path = PathBuf::from(arguments.next().expect("manifest path"));
    let source_path = PathBuf::from(arguments.next().expect("source path"));
    let output_path = PathBuf::from(arguments.next().expect("output path"));
    if arguments.next().is_some() {
        return;
    }
    let manifest_bytes = fs::read(&manifest_path).unwrap();
    let source_bytes = fs::read(&source_path).unwrap();
    let compilation = compile_instance(
        GrammarInstanceInput::new(&manifest_bytes, &source_bytes),
        CompileOptions::default(),
    );
    fs::write(output_path, compilation.generated().source()).unwrap();
}
"#;
    assert!(
        cli_byte_transport_findings(exact_cli_control).is_empty(),
        "exact raw-byte transport control"
    );
    let exact_assert_cli_control = exact_cli_control.replacen(
        r#"    if arguments.next().is_some() {
        return;
    }
"#,
        "    assert!(arguments.next().is_none(), \"exactly three paths\");\n",
        1,
    );
    assert_ne!(
        exact_assert_cli_control, exact_cli_control,
        "assert-macro argument control must replace the explicit branch"
    );
    assert!(
        cli_byte_transport_findings(&exact_assert_cli_control).is_empty(),
        "an exact assert-macro arity check remains visible to the AST transport census"
    );
    let iterator_authority_spoof = [
        r#"
mod Iterator {
    pub fn next<T>(_: &mut T) -> Option<std::ffi::OsString> { None }
}
"#,
        &exact_assert_cli_control.replacen(
            "arguments.next().is_none()",
            "Iterator::next(&mut arguments).is_none()",
            1,
        ),
    ]
    .concat();
    for (law, mutated) in [
        (
            "count-preserving nominal Iterator authority spoof",
            iterator_authority_spoof,
        ),
        (
            "foreign macro next",
            exact_assert_cli_control.replacen(
                "assert!(arguments.next().is_none()",
                "assert!(alternate_arguments.next().is_none()",
                1,
            ),
        ),
        (
            "duplicate macro next",
            exact_assert_cli_control.replacen(
                "assert!(arguments.next().is_none()",
                "assert!(arguments.next().is_none() && arguments.next().is_none()",
                1,
            ),
        ),
        (
            "statement-shaped macro body next",
            exact_assert_cli_control.replacen(
                "    let manifest_bytes =",
                "    macro_rules! hidden_next { () => {{ let _ = arguments.next(); }}; }\n    hidden_next!();\n    let manifest_bytes =",
                1,
            ),
        ),
        (
            "qualified macro body next",
            exact_assert_cli_control.replacen(
                "    let manifest_bytes =",
                "    macro_rules! hidden_next { () => {{ let _ = Iterator::next(&mut arguments); }}; }\n    hidden_next!();\n    let manifest_bytes =",
                1,
            ),
        ),
        (
            "foreign qualified macro next replacement",
            exact_assert_cli_control.replacen(
                "arguments.next().is_none()",
                "foreign::next(&mut arguments).is_none()",
                1,
            ),
        ),
        (
            "field-qualified macro receiver replacement",
            exact_assert_cli_control.replacen(
                "arguments.next().is_none()",
                "foreign.arguments.next().is_none()",
                1,
            ),
        ),
    ] {
        assert_ne!(
            mutated, exact_assert_cli_control,
            "macro next falsifier `{law}` must mutate its control"
        );
        assert!(
            !cli_byte_transport_findings(&mutated).is_empty(),
            "macro next falsifier `{law}` must be rejected"
        );
    }
    let inspect_manifest = exact_cli_control.replacen(
        "let source_bytes =",
        "inspect(&manifest_bytes);\n    let source_bytes =",
        1,
    );
    let inspect_source = exact_cli_control.replacen(
        "let compilation =",
        "inspect(&source_bytes);\n    let compilation =",
        1,
    );
    let inspect_extension = exact_cli_control.replacen(
        "let manifest_bytes =",
        "let _kind = source_path.extension();\n    let manifest_bytes =",
        1,
    );
    let inspect_file_name = exact_cli_control.replacen(
        "let manifest_bytes =",
        "let _kind = manifest_path.file_name();\n    let manifest_bytes =",
        1,
    );
    let identity_options = exact_cli_control.replacen(
        "CompileOptions::default(),",
        "if source_path.extension() == Some(OsStr::new(\"json\")) { CompileOptions::default() } else { CompileOptions::default() },",
        1,
    );
    let wrong_read_binding =
        exact_cli_control.replacen("fs::read(&manifest_path)", "fs::read(&source_path)", 1);
    let decoupled_compile_input = exact_cli_control.replacen(
        "GrammarInstanceInput::new(&manifest_bytes, &source_bytes)",
        "{ let _decoy = GrammarInstanceInput::new(&manifest_bytes, &source_bytes); reload_from_argv() }",
        1,
    );
    let second_argument_source = exact_cli_control.replacen(
        "compilation.generated().source()",
        "reload_output_bytes()",
        1,
    );
    let mixed_output_source = exact_cli_control.replacen(
        "compilation.generated().source()",
        "if choose_compiled() { compilation.generated().source() } else { reload_output_bytes() }",
        1,
    );
    let duplicate_read = exact_cli_control.replacen(
        "let compilation =",
        "let _again = fs::read(&manifest_path).unwrap();\n    let compilation =",
        1,
    );
    let duplicate_argv = exact_cli_control.replacen(
        "let manifest_bytes =",
        "let _again = env::args_os();\n    let manifest_bytes =",
        1,
    );
    let decoy_args_os = exact_cli_control.replacen(
        "let mut arguments = env::args_os().skip(1);",
        "let _decoy = env::args_os();\n    let mut arguments = env::args().skip(1);",
        1,
    );
    let aliased_utf8_args = [
        exact_cli_control,
        "\nfn hidden_utf8_arguments() { use std::env::args as utf8_args; let _ = utf8_args(); }\n",
    ]
    .concat();
    let detached_arguments = exact_cli_control.replacen(
        "let mut arguments = env::args_os().skip(1);",
        "let _decoy = env::args_os();\n    let mut arguments = alternate_arguments();",
        1,
    );
    let foreign_next = exact_cli_control.replacen(
        "if arguments.next().is_some() {",
        "if alternate_arguments.next().is_some() {",
        1,
    );
    let foreign_qualified_next = exact_cli_control.replacen(
        "if arguments.next().is_some() {",
        "if foreign::next(&mut arguments).is_some() {",
        1,
    );
    let rewritten_next_path = exact_cli_control.replacen(
        r#"PathBuf::from(arguments.next().expect("manifest path"))"#,
        r#"PathBuf::from(rewrite(arguments.next().expect("manifest path")))"#,
        1,
    );
    let mapped_next_path = exact_cli_control.replacen(
        r#"PathBuf::from(arguments.next().expect("source path"))"#,
        r#"PathBuf::from(arguments.next().map(rewrite).expect("source path"))"#,
        1,
    );
    let foreign_path_constructor = exact_cli_control.replacen(
        r#"PathBuf::from(arguments.next().expect("output path"))"#,
        r#"path_from(arguments.next().expect("output path"))"#,
        1,
    );
    let conditional_next_path = exact_cli_control.replacen(
        r#"PathBuf::from(arguments.next().expect("manifest path"))"#,
        r#"PathBuf::from(if rewrite_enabled() {
            rewrite(arguments.next().expect("manifest path"))
        } else {
            arguments.next().expect("manifest path")
        })"#,
        1,
    );
    let env_authority_spoof = exact_cli_control.replacen(
        "use std::env;",
        "mod env { pub fn args_os() -> Args { todo!() } }",
        1,
    );
    let fs_authority_spoof = exact_cli_control.replacen(
        "use std::fs;",
        "mod fs { pub fn read<T>(_: T) -> Bytes { todo!() } pub fn write<T, U>(_: T, _: U) {} }",
        1,
    );
    let compiler_authority_spoof = ["mod bbnf {}\n", exact_cli_control].concat();
    for source in [
        inspect_manifest,
        inspect_source,
        inspect_extension,
        inspect_file_name,
        identity_options,
        wrong_read_binding,
        decoupled_compile_input,
        second_argument_source,
        mixed_output_source,
        duplicate_read,
        duplicate_argv,
        decoy_args_os,
        aliased_utf8_args,
        detached_arguments,
        foreign_next,
        foreign_qualified_next,
        rewritten_next_path,
        mapped_next_path,
        foreign_path_constructor,
        conditional_next_path,
        env_authority_spoof,
        fs_authority_spoof,
        compiler_authority_spoof,
    ] {
        assert!(
            !cli_byte_transport_findings(&source).is_empty(),
            "raw path/byte inspection, identity dispatch, or swapped input cannot hide in the CLI"
        );
    }
}

#[test]
fn every_core_and_external_control_caller_uses_only_the_clean_break_instance_api() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let repository_root = manifest_dir
        .join("../..")
        .canonicalize()
        .expect("canonical repository root");
    let mut files = BTreeSet::new();
    rust_caller_files(manifest_dir, &mut files);
    for retired in [
        "contracts/w2-generated-control",
        "contracts/w2-recursive-control",
        "contracts/w2-record-control",
    ] {
        assert!(
            !repository_root.join(retired).exists(),
            "{retired} must be deleted after its unique laws cross into W3"
        );
    }
    assert!(!files.is_empty(), "caller traversal found Rust sources");
    let core_sources = files
        .iter()
        .filter_map(|path| {
            let relative = path
                .strip_prefix(&repository_root)
                .unwrap_or(path)
                .to_string_lossy()
                .replace('\\', "/");
            relative.starts_with("crates/core/src/").then(|| {
                fs::read_to_string(path)
                    .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
            })
        })
        .collect::<Vec<_>>();
    let active_instance_catalog = instance_access_catalog(core_sources.iter().map(String::as_str));

    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        let relative = path
            .strip_prefix(&repository_root)
            .unwrap_or(&path)
            .to_string_lossy()
            .replace('\\', "/");
        let module_graph = module_graph_union_findings(&relative, &source);
        assert!(
            module_graph.is_empty(),
            "{} escapes the exact lexical-plus-generated module union: {module_graph:?}",
            path.display(),
        );
        let findings = legacy_surface_findings(&source);
        assert!(
            findings.is_empty(),
            "{} retains a forbidden clean-break surface: {findings:?}",
            path.display(),
        );
        if relative.starts_with("crates/core/src/") {
            let ownership = manifest_ownership_findings(&relative, &source);
            assert!(
                ownership.is_empty(),
                "{} violates manifest validation ownership: {ownership:?}",
                path.display(),
            );
            let ingress = compile_ingress_surface_findings(&relative, &source);
            assert!(
                ingress.is_empty(),
                "{} violates the exclusive grammar-instance compile ingress: {ingress:?}",
                path.display(),
            );
            let program_constructors = program_construction_findings(&relative, &source);
            assert!(
                program_constructors.is_empty(),
                "{} has an alternate or moved Program constructor: {program_constructors:?}",
                path.display(),
            );
            let instance_types = grammar_instance_type_surface_findings(&relative, &source);
            assert!(
                instance_types.is_empty(),
                "{} exposes GrammarInstance outside the exact owner/resolver/replay set: {instance_types:?}",
                path.display(),
            );
            let instance_aliases = grammar_instance_alias_findings(&source);
            assert!(
                instance_aliases.is_empty(),
                "{} aliases the exact GrammarInstance nominal identity: {instance_aliases:?}",
                path.display(),
            );
            let expected_resolution_calls =
                usize::from(relative == "crates/core/src/compiler/mod.rs");
            assert_eq!(
                program_resolve_call_count(&source),
                expected_resolution_calls,
                "{} has a moved, duplicated, or missing Program::resolve call",
                path.display(),
            );
            if !matches!(
                relative.as_str(),
                "crates/core/src/compiler/instance.rs"
                    | "crates/core/src/compiler/replay.rs"
                    | "crates/core/src/compiler/syntax.rs"
            ) {
                assert_eq!(
                    instance_resolution_accessor_count_with_catalog(
                        &source,
                        &active_instance_catalog,
                    ),
                    0,
                    "{} moved source-dependent entry/Pratt resolution out of syntax.rs",
                    path.display(),
                );
            }
        }
    }
    assert!(
        !manifest_dir.join("src/compiler/edit.rs").exists(),
        "compiler/edit.rs must be deleted, not adapted"
    );

    let syntax_source =
        fs::read_to_string(manifest_dir.join("src/compiler/syntax.rs")).expect("syntax.rs");
    assert_eq!(
        instance_resolution_accessor_count_with_catalog(&syntax_source, &active_instance_catalog,),
        2,
        "syntax.rs owns exactly one entry and one Pratt-table accessor use in Program::resolve"
    );
    let source_source =
        fs::read_to_string(manifest_dir.join("src/compiler/source.rs")).expect("source.rs");
    let entry_fallbacks = resolution_fallback_findings(&syntax_source, &source_source);
    assert!(
        entry_fallbacks.is_empty(),
        "compiler/syntax.rs retains forbidden implicit entry selection: {entry_fallbacks:?}"
    );

    let source_module_findings = source_module_authority_findings(
        &fs::read_to_string(manifest_dir.join("src/compiler/source.rs"))
            .expect("compiler/source.rs"),
    );
    assert!(
        source_module_findings.is_empty(),
        "SourceModule retains duplicated path/source authority: {source_module_findings:?}"
    );

    let compiler_mod =
        fs::read_to_string(manifest_dir.join("src/compiler/mod.rs")).expect("compiler/mod.rs");
    let instance_source =
        fs::read_to_string(manifest_dir.join("src/compiler/instance.rs")).expect("instance.rs");
    let validated_spine = validated_instance_spine_findings(&instance_source, &compiler_mod);
    assert!(
        validated_spine.is_empty(),
        "raw instance decode, validated carrier, Program, and CompiledModule are not one live spine: {validated_spine:?}"
    );
    assert_eq!(
        compiler_mod_instance_resolution_calls(&compiler_mod),
        1,
        "the sole compiler spine call passes module and typed instance directly to Program::resolve"
    );
    let compiler_syntax = syn::parse_file(&compiler_mod).expect("compiler/mod.rs syntax");
    let compile_instance_functions = compiler_syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == "compile_instance" => Some(function),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        compile_instance_functions.len(),
        1,
        "one compile_instance definition"
    );
    assert!(
        exact_compile_instance_signature(compile_instance_functions[0]),
        "compile_instance has the exact non-generic two-borrow public signature"
    );
}
