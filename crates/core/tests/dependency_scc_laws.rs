use bbnf::compiler::NodeKind;
use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

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
    let rules = module.program().rules();
    let root = rules.get(0).expect("root").id();
    let head = rules.get(1).expect("head").id();
    let tail = rules.get(2).expect("tail").id();
    let atom = rules.get(3).expect("atom").id();

    assert_eq!(
        module
            .dependency()
            .graph()
            .dependencies(root)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![head, tail]),
        "dependency edges use RuleId order, not encounter order"
    );
    assert_eq!(
        module
            .dependency()
            .graph()
            .dependencies(head)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![atom])
    );
    assert_eq!(
        module
            .dependency()
            .graph()
            .dependencies(tail)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![atom])
    );
    assert_eq!(
        module
            .dependency()
            .graph()
            .dependencies(atom)
            .map(Iterator::collect::<Vec<_>>),
        Some(Vec::new())
    );

    let order = module.dependency().dependency_order().collect::<Vec<_>>();
    let position = |rule| {
        let component = module
            .dependency()
            .component_of(rule)
            .expect("rule belongs to its dependency carrier");
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
        let component = module
            .dependency()
            .component_of(rule.id())
            .expect("rule belongs to its dependency carrier");
        assert_eq!(
            module
                .dependency()
                .component(component)
                .map(Iterator::collect::<Vec<_>>),
            Some(vec![rule.id()])
        );
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

    assert_eq!(first.program().revision(), second.program().revision());
    assert_eq!(first.dependency(), second.dependency());
    assert_eq!(
        first.dependency().revision(),
        first.dependency().graph().revision()
    );
}

#[test]
fn consuming_self_recursion_is_retained_as_one_finite_component() {
    let module = accepted(r#"root = "(", root, ")" | "x";"#);
    let root = module.program().rules().get(0).expect("root").id();
    let component = module
        .dependency()
        .component_of(root)
        .expect("root belongs to its dependency carrier");

    assert_eq!(module.dependency().is_recursive(component), Some(true));
    assert_eq!(
        module
            .dependency()
            .component(component)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![root])
    );
    assert_eq!(
        module
            .dependency()
            .graph()
            .dependencies(root)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![root])
    );
}

#[test]
fn consuming_mutual_recursion_has_canonical_members_and_scc_identity() {
    let module = accepted(
        r#"
root = "a", other | atom;
other = "b", root | atom;
atom = "x";
"#,
    );
    let root = module.program().rules().get(0).expect("root").id();
    let other = module.program().rules().get(1).expect("other").id();
    let atom = module.program().rules().get(2).expect("atom").id();
    let recursive = module
        .dependency()
        .component_of(root)
        .expect("root belongs to its dependency carrier");

    assert_eq!(module.dependency().component_of(other), Some(recursive));
    assert_ne!(module.dependency().component_of(atom), Some(recursive));
    assert_eq!(
        module
            .dependency()
            .component(recursive)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![root, other])
    );
    assert_eq!(module.dependency().is_recursive(recursive), Some(true));

    let order = module.dependency().dependency_order().collect::<Vec<_>>();
    let atom_position = order
        .iter()
        .position(|id| Some(*id) == module.dependency().component_of(atom))
        .unwrap();
    let recursive_position = order.iter().position(|id| *id == recursive).unwrap();
    assert!(atom_position < recursive_position);
}

#[test]
fn accepted_dependency_graph_contains_only_rule_reference_edges() {
    let module = accepted(
        r#"
root = ("x", leaf)+;
leaf = /[a-z]/;
"#,
    );
    let root = module.program().rules().get(0).expect("root").id();
    let leaf = module.program().rules().get(1).expect("leaf").id();
    assert_eq!(
        module
            .dependency()
            .graph()
            .dependencies(root)
            .map(Iterator::collect::<Vec<_>>),
        Some(vec![leaf])
    );

    let references = module
        .program()
        .nodes()
        .iter()
        .filter(|node| matches!(node.kind(), NodeKind::RuleRef(_)))
        .count();
    assert_eq!(references, 1);
}
