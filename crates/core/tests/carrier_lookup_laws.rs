use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

fn accepted(path: &str, source: &str) -> bbnf::CompiledModule {
    match compile_module(
        ModuleInput::new(path, source.as_bytes()),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => panic!(
            "lookup fixture must compile:\n{}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        ),
    }
}

#[test]
fn opaque_ids_from_another_carrier_are_checked_at_every_public_lookup() {
    let large = accepted(
        "large-carrier.bbnf",
        r#"
root = record | tail;
record = %{ one: "a", two: "b" };
tail = middle;
middle = "x";
"#,
    );
    let small = accepted("small-carrier.bbnf", r#"root = "z";"#);

    let foreign_node = large.program().nodes().last().expect("large node").id();
    let foreign_rule = large.program().rules().last().expect("large rule").id();
    let foreign_field = large.program().fields().last().expect("large field").id();
    let foreign_plan_node = large
        .lowering()
        .nodes()
        .last()
        .expect("large plan node")
        .id();
    let foreign_type = large
        .types()
        .node_derivations()
        .iter()
        .map(|derivation| derivation.type_id())
        .max_by_key(|id| id.index())
        .expect("large type");
    let foreign_component = large
        .dependency()
        .dependency_order()
        .iter()
        .copied()
        .max_by_key(|id| id.index())
        .expect("large component");

    assert!(small.program().node(foreign_node).is_none());
    assert!(small.program().rule(foreign_rule).is_none());
    assert!(small.program().field(foreign_field).is_none());
    assert!(
        small
            .dependency()
            .graph()
            .dependencies(foreign_rule)
            .is_none()
    );
    assert!(small.dependency().component(foreign_component).is_none());
    assert!(small.dependency().component_of(foreign_rule).is_none());
    assert!(!small.dependency().is_recursive(foreign_component));
    assert!(small.types().semantic_type(foreign_type).is_none());
    assert!(small.types().node_type(foreign_node).is_none());
    assert!(small.types().rule_type(foreign_rule).is_none());
    assert!(small.types().rule_root_type(foreign_rule).is_none());
    assert!(small.facts().node(foreign_node).is_none());
    assert!(small.facts().rule(foreign_rule).is_none());
    assert!(small.lowering().node(foreign_plan_node).is_none());
}
