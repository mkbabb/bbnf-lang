use bbnf::{CompileOptions, DiagnosticCode, ModuleInput, StageResult, compile_module};

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
    match compile_module(ModuleInput::new("fixture.bbnf", bytes), options) {
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
    match compile_module(ModuleInput::new("fixture.bbnf", bytes), options) {
        StageResult::Accepted { .. } => panic!("expected fixture rejection"),
        StageResult::Rejected { diagnostics } => diagnostics
            .iter()
            .next()
            .expect("a rejection carries a diagnostic")
            .code(),
    }
}

#[test]
fn genuine_control_fixture_builds_postorder_nodes_and_lexical_rules() {
    let compiled = accepted(CONTROL_FIXTURE, CompileOptions::default());

    assert_eq!(compiled.source_map.modules()[0].rule_count(), 7);
    assert_eq!(compiled.source_map.modules()[0].node_count(), 16);
    assert_eq!(
        compiled
            .program
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
    for (index, node) in compiled.program.nodes().iter().enumerate() {
        assert_eq!(node.id().index(), index);
    }
    for (index, rule) in compiled.program.rules().iter().enumerate() {
        assert_eq!(rule.id().index(), index);
        assert!(rule.root().index() < compiled.program.nodes().len());
    }
    assert_eq!(
        compiled
            .program
            .rules()
            .iter()
            .map(|rule| rule.root().index())
            .collect::<Vec<_>>(),
        [3, 6, 9, 11, 13, 14, 15]
    );
    match compiled.program.nodes()[3].kind() {
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
    let first_source =
        serde_json::to_value(first.source_map.modules()).expect("serialize first source map");
    let second_source =
        serde_json::to_value(second.source_map.modules()).expect("serialize second source map");

    assert_eq!(first_source, second_source);
    assert_eq!(first.source_map.revision(), second.source_map.revision());
    assert_eq!(first.source_map.modules()[0].bytes(), CONTROL_FIXTURE);
}

#[test]
fn declarative_transform_form_has_stable_identity_and_no_host_surface() {
    // W2 deliberately admits exactly this action-free form:
    // `@identifier(expression)`. The identifier is metadata, not executable
    // code, a callback, a dependency, or a build feature.
    let grammar = b"root = @capture(\"x\"), @capture(/y+/);";
    let first = accepted(grammar, CompileOptions::default());
    let second = accepted(grammar, CompileOptions::default());

    assert_eq!(first.source_map.modules()[0].transform_count(), 2);
    assert_eq!(
        serde_json::to_value(first.source_map.modules()).expect("serialize transform source"),
        serde_json::to_value(second.source_map.modules()).expect("serialize transform source"),
    );

    let transforms = first
        .program
        .nodes()
        .iter()
        .filter_map(|node| match node.kind() {
            bbnf::compiler::NodeKind::Transform {
                child,
                transform,
                name,
            } => Some((
                node.id().index(),
                child.index(),
                transform.index(),
                name.as_str(),
            )),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(transforms, [(1, 0, 0, "capture"), (3, 2, 1, "capture")]);
    assert_eq!(
        first
            .events
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
    let source = CompileOptions {
        source_bytes: 4,
        ..CompileOptions::default()
    };
    assert_eq!(
        rejected_code(b"root = \"x\";", source),
        DiagnosticCode::SourceTooLarge
    );

    let rules = CompileOptions {
        rules: 1,
        ..CompileOptions::default()
    };
    assert_eq!(
        rejected_code(b"root = leaf; leaf = \"x\";", rules),
        DiagnosticCode::ResourceLimit
    );

    let nodes = CompileOptions {
        nodes: 1,
        ..CompileOptions::default()
    };
    assert_eq!(
        rejected_code(b"root = \"x\", \"y\";", nodes),
        DiagnosticCode::ResourceLimit
    );

    let nesting = CompileOptions {
        syntax_nesting: 1,
        ..CompileOptions::default()
    };
    assert_eq!(
        rejected_code(b"root = ((\"x\"));", nesting),
        DiagnosticCode::ResourceLimit
    );
}
