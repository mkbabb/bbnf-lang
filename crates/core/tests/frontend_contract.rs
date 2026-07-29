mod instance_support;

use bbnf::compiler::NodeKind;
use bbnf::{CompileOptions, DiagnosticCode, StageResult};

const CONTROL_FIXTURE: &[u8] = r#"
root = greeting, tail*;
tail = ",", greeting;
greeting = "hello" | /[A-Z][a-z]+/;
optional = "x"?;
many = "y"+;
empty = epsilon;
unicode_empty = ε;
"#
.as_bytes();

fn accepted(bytes: &[u8], options: CompileOptions) -> bbnf::CompiledModule {
    match instance_support::compile("fixture.bbnf", bytes, options) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(diagnostics.is_empty());
            value
        }
        StageResult::Rejected { diagnostics } => {
            let messages = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("expected fixture to compile:\n{messages}");
        }
    }
}

fn rejected_code(bytes: &[u8], options: CompileOptions) -> DiagnosticCode {
    match instance_support::compile("fixture.bbnf", bytes, options) {
        StageResult::Accepted { .. } => panic!("expected fixture rejection"),
        StageResult::Rejected { diagnostics } => diagnostics
            .iter()
            .next()
            .expect("a rejection carries a diagnostic")
            .code(),
    }
}

fn rejected_diagnostics(
    bytes: &[u8],
    options: CompileOptions,
) -> Vec<(DiagnosticCode, usize, usize, String)> {
    match instance_support::compile("fixture.bbnf", bytes, options) {
        StageResult::Accepted { .. } => panic!("expected fixture rejection"),
        StageResult::Rejected { diagnostics } => diagnostics
            .iter()
            .map(|diagnostic| {
                (
                    diagnostic.code(),
                    diagnostic.start(),
                    diagnostic.end(),
                    diagnostic.message().to_owned(),
                )
            })
            .collect(),
    }
}

#[test]
fn genuine_control_fixture_builds_postorder_nodes_and_lexical_rules() {
    let compiled = accepted(CONTROL_FIXTURE, CompileOptions::default());

    assert_eq!(compiled.source_map().modules()[0].rule_count(), 7);
    assert_eq!(compiled.source_map().modules()[0].node_count(), 16);
    assert_eq!(
        compiled
            .program()
            .rules()
            .iter()
            .map(|rule| rule.name())
            .collect::<Vec<_>>(),
        [
            "root",
            "tail",
            "greeting",
            "optional",
            "many",
            "empty",
            "unicode_empty",
        ]
    );
    for (index, node) in compiled.program().nodes().iter().enumerate() {
        assert_eq!(node.id().index(), index);
    }
    for (index, rule) in compiled.program().rules().iter().enumerate() {
        assert_eq!(rule.id().index(), index);
        assert!(rule.root().index() < compiled.program().nodes().len());
    }
    assert_eq!(
        compiled
            .program()
            .rules()
            .iter()
            .map(|rule| rule.root().index())
            .collect::<Vec<_>>(),
        [3, 6, 9, 11, 13, 14, 15]
    );
    match compiled
        .program()
        .nodes()
        .get(3)
        .expect("root sequence")
        .kind()
    {
        bbnf::compiler::NodeKind::Sequence(children) => assert_eq!(
            children
                .iter()
                .map(|child| child.index())
                .collect::<Vec<_>>(),
            [0, 2]
        ),
        other => panic!("expected root postorder sequence, got {other:?}"),
    }
}

#[test]
fn source_output_and_revision_are_order_stable() {
    let first = accepted(CONTROL_FIXTURE, CompileOptions::default());
    let second = accepted(CONTROL_FIXTURE, CompileOptions::default());
    let first_source = first.source_map().replay_json().expect("source-map replay");
    let second_source = second
        .source_map()
        .replay_json()
        .expect("source-map replay");

    assert_eq!(first_source, second_source);
    assert_eq!(
        first.source_map().revision(),
        second.source_map().revision()
    );
    assert_eq!(
        first.source_map().modules()[0].revision(),
        first.source_map().revision()
    );
}

#[test]
fn declarative_transform_form_has_stable_identity_and_no_host_surface() {
    // W2 deliberately admits exactly this action-free form:
    // `@identifier(expression)`. The identifier is metadata, not executable
    // code, a callback, a dependency, or a build feature.
    let grammar = b"root = @capture(\"x\"), @capture(/y+/);";
    let first = accepted(grammar, CompileOptions::default());
    let second = accepted(grammar, CompileOptions::default());

    assert_eq!(first.source_map().modules()[0].transform_count(), 2);
    assert_eq!(
        first.source_map().replay_json().expect("transform replay"),
        second.source_map().replay_json().expect("transform replay"),
    );

    let transforms = first
        .program()
        .nodes()
        .iter()
        .filter_map(|node| match node.kind() {
            bbnf::compiler::NodeKind::Transform {
                child,
                transform,
                name,
            } => Some((node.id().index(), child.index(), transform.index(), name)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(transforms, [(1, 0, 0, "capture"), (3, 2, 1, "capture")]);
    assert_eq!(
        first
            .events()
            .transforms()
            .iter()
            .map(|site| (site.id().index(), site.node().index(), site.name()))
            .collect::<Vec<_>>(),
        [(0, 1, "capture"), (1, 3, "capture")]
    );

    // There is no compatibility arrow or host-action spelling.
    assert_eq!(
        rejected_code(b"root = \"x\" -> capture;", CompileOptions::default()),
        DiagnosticCode::Syntax
    );
}

#[test]
fn percent_records_have_nominal_owners_explicit_slots_and_stable_field_ids() {
    let grammar = r#"
root = node;
node = list | atom;
list = %{ ~"(", children: node*, ~")" };
atom = %{ text: @raw(/[A-Za-z]+/), suffix: "!"? };
empty = %{};
layout = %{ ~"[", ~"]" };
"#;
    let first = accepted(grammar.as_bytes(), CompileOptions::default());
    let second = accepted(grammar.as_bytes(), CompileOptions::default());
    let find_rule = |name: &str| {
        first
            .program()
            .rules()
            .iter()
            .find(|rule| rule.name() == name)
            .expect("record fixture rule exists")
    };

    assert_eq!(
        first
            .program()
            .fields()
            .iter()
            .map(|field| {
                (
                    field.id().index(),
                    field.owner().index(),
                    field.slot(),
                    field.name(),
                    field.child().index(),
                )
            })
            .collect::<Vec<_>>(),
        [
            (0, find_rule("list").id().index(), 1, "children", 6),
            (1, find_rule("atom").id().index(), 0, "text", 10),
            (2, find_rule("atom").id().index(), 1, "suffix", 12),
        ]
    );
    let first_program: serde_json::Value =
        serde_json::from_slice(&first.program().replay_json().expect("program replay"))
            .expect("program replay JSON");
    let second_program: serde_json::Value =
        serde_json::from_slice(&second.program().replay_json().expect("program replay"))
            .expect("program replay JSON");
    assert_eq!(
        first_program["value"]["fields"],
        second_program["value"]["fields"],
    );

    for (name, expected_slots) in [("list", 3), ("atom", 2), ("empty", 0), ("layout", 2)] {
        let rule = find_rule(name);
        match first
            .program()
            .node(rule.root())
            .expect("rule root belongs to this program")
            .kind()
        {
            NodeKind::Record { owner, slots } => {
                assert_eq!(owner, rule.id());
                assert_eq!(slots.len(), expected_slots);
            }
            other => panic!("expected `{name}` to own a nominal record, got {other:?}"),
        }
    }

    let children = first.program().fields().get(0).expect("children field");
    let field_start = grammar.find("children: node*").expect("field spelling");
    assert_eq!(children.span().start() as usize, field_start);
    assert_eq!(
        children.span().end() as usize,
        field_start + "children: node*".len()
    );
}

#[test]
fn record_duplicates_report_the_second_field_name_span() {
    let grammar = b"root = %{ value: \"x\", value: \"y\" };";
    let diagnostics = rejected_diagnostics(grammar, CompileOptions::default());
    let second = grammar
        .windows(b"value".len())
        .enumerate()
        .filter_map(|(index, bytes)| (bytes == b"value").then_some(index))
        .nth(1)
        .expect("fixture has a second value field");

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].0, DiagnosticCode::DuplicateField);
    assert_eq!((diagnostics[0].1, diagnostics[0].2), (second, second + 5));
    assert!(diagnostics[0].3.contains("record owned by rule `root`"));
}

#[test]
fn records_are_rejected_outside_one_direct_rule_root() {
    for grammar in [
        b"root = %{ value: \"x\" }?;".as_slice(),
        b"root = %{ value: \"x\" } | \"y\";".as_slice(),
        b"root = @capture(%{ value: \"x\" });".as_slice(),
        b"root = %{ child: %{ value: \"x\" } };".as_slice(),
    ] {
        assert_eq!(
            rejected_code(grammar, CompileOptions::default()),
            DiagnosticCode::RecordMustBeRuleRoot,
            "fixture should reject: {}",
            String::from_utf8_lossy(grammar),
        );
    }
}

#[test]
fn record_delimiters_do_not_steal_bare_or_at_braces() {
    let repeated = accepted(b"root = { \"x\" };", CompileOptions::default());
    let repeated_root = repeated.program().rules().get(0).expect("root rule").root();
    assert!(matches!(
        repeated
            .program()
            .node(repeated_root)
            .map(|node| node.kind()),
        Some(NodeKind::Repeat {
            minimum: 0,
            maximum: None,
            ..
        })
    ));
    assert_eq!(
        rejected_code(b"root = @{ \"x\" };", CompileOptions::default()),
        DiagnosticCode::Syntax
    );
    assert_eq!(
        rejected_code(b"root = %{ \"x\" };", CompileOptions::default()),
        DiagnosticCode::Syntax
    );
    assert_eq!(
        rejected_code(b"root = %{ value: \"x\", };", CompileOptions::default()),
        DiagnosticCode::Syntax
    );

    let compiled = accepted(
        b"root = %{ ~\"{\", value: \"x\", ~\"}\" };",
        CompileOptions::default(),
    );
    assert_eq!(compiled.program().fields().len(), 1);
    assert_eq!(
        compiled
            .program()
            .fields()
            .get(0)
            .expect("value field")
            .name(),
        "value"
    );
}

#[test]
fn malformed_and_trailing_input_are_rejected_at_the_frontend() {
    assert_eq!(
        rejected_code(b"root = (\"x\";", CompileOptions::default()),
        DiagnosticCode::Syntax
    );
    assert_eq!(
        rejected_code(b"root = \"x\"; trailing", CompileOptions::default()),
        DiagnosticCode::Syntax
    );
    assert_eq!(
        rejected_code(b"root = /[a-/;", CompileOptions::default()),
        DiagnosticCode::Regex
    );
}

#[test]
fn source_rule_node_and_nesting_budgets_fail_typed() {
    let mut source = CompileOptions::default();
    source.source_bytes = 4;
    assert_eq!(
        rejected_code(b"root = \"x\";", source),
        DiagnosticCode::SourceTooLarge
    );

    let mut rules = CompileOptions::default();
    rules.rules = 1;
    assert_eq!(
        rejected_code(b"root = leaf; leaf = \"x\";", rules),
        DiagnosticCode::ResourceLimit
    );

    let mut nodes = CompileOptions::default();
    nodes.nodes = 1;
    assert_eq!(
        rejected_code(b"root = \"x\", \"y\";", nodes),
        DiagnosticCode::ResourceLimit
    );

    let mut nesting = CompileOptions::default();
    nesting.syntax_nesting = 1;
    assert_eq!(
        rejected_code(b"root = ((\"x\"));", nesting),
        DiagnosticCode::ResourceLimit
    );
}
