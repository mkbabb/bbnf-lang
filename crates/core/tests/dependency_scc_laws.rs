use bbnf::compiler::NodeKind;
use bbnf::{CompileOptions, DiagnosticCode, ModuleInput, StageResult, compile_module};

fn accepted(source: &str) -> bbnf::CompiledModule {
    match compile_module(
        ModuleInput::new("dependency-laws.bbnf", source.as_bytes()),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            let rendered = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("expected compilation to succeed:\n{rendered}");
        }
    }
}

fn rejected_codes(source: &str) -> Vec<DiagnosticCode> {
    match compile_module(
        ModuleInput::new("dependency-laws.bbnf", source.as_bytes()),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { .. } => {
            panic!("expected compilation to be rejected")
        }
        StageResult::Rejected { diagnostics } => diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code())
            .collect(),
    }
}

#[test]
fn direct_dependencies_are_sorted_and_components_are_dependency_first() {
    let module = accepted(
        r#"
root = tail | head | tail;
head = "h", atom;
tail = atom, "t";
atom = /[0-9]+/;
"#,
    );
    let rules = module.program.rules();
    let root = rules[0].id();
    let head = rules[1].id();
    let tail = rules[2].id();
    let atom = rules[3].id();

    assert_eq!(
        module.dependency.graph().dependencies(root),
        &[head, tail],
        "dependency edges use RuleId order, not encounter order"
    );
    assert_eq!(module.dependency.graph().dependencies(head), &[atom]);
    assert_eq!(module.dependency.graph().dependencies(tail), &[atom]);
    assert!(module.dependency.graph().dependencies(atom).is_empty());

    let order = module.dependency.dependency_order();
    let position = |rule| {
        let component = module.dependency.component_of(rule);
        order
            .iter()
            .position(|candidate| *candidate == component)
            .expect("every component occurs in the dependency order")
    };
    assert!(position(atom) < position(head));
    assert!(position(atom) < position(tail));
    assert!(position(head) < position(root));
    assert!(position(tail) < position(root));

    for rule in rules {
        let component = module.dependency.component_of(rule.id());
        assert_eq!(module.dependency.component(component), &[rule.id()]);
    }
}

#[test]
fn stable_inputs_have_identical_graph_scc_ids_members_and_order() {
    let source = r#"
root = second | first;
first = "a";
second = first, "b";
"#;
    let first = accepted(source);
    let second = accepted(source);

    assert_eq!(first.program.revision(), second.program.revision());
    assert_eq!(first.dependency, second.dependency);
    assert_eq!(
        first.dependency.revision(),
        first.dependency.graph().revision()
    );
}

#[test]
fn self_recursion_is_rejected_instead_of_approximated() {
    let codes = rejected_codes("root = root | \"x\";");
    assert_eq!(codes, vec![DiagnosticCode::RecursiveRule]);
}

#[test]
fn mutual_recursion_is_rejected_instead_of_approximated() {
    let codes = rejected_codes(
        r#"
root = other;
other = root | "x";
"#,
    );
    assert_eq!(codes, vec![DiagnosticCode::RecursiveRule]);
}

#[test]
fn accepted_dependency_graph_contains_only_rule_reference_edges() {
    let module = accepted(
        r#"
root = ("x", leaf)+;
leaf = /[a-z]/;
"#,
    );
    let root = module.program.rules()[0].id();
    let leaf = module.program.rules()[1].id();
    assert_eq!(module.dependency.graph().dependencies(root), &[leaf]);

    let references = module
        .program
        .nodes()
        .iter()
        .filter(|node| matches!(node.kind(), NodeKind::RuleRef(_)))
        .count();
    assert_eq!(references, 1);
}
