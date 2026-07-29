mod instance_support;

use bbnf::{CompileOptions, StageResult};

fn accepted(path: &str, source: &str) -> bbnf::CompiledModule {
    match instance_support::compile(path, source.as_bytes(), CompileOptions::default()) {
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
    let small = accepted("small-carrier.bbnf", r#"root = %{ only: "z" };"#);

    let same_index_node = large.program().nodes().get(0).expect("node").id();
    let same_index_rule = large.program().rules().get(0).expect("rule").id();
    let same_index_field = large.program().fields().get(0).expect("field").id();
    let same_index_plan_node = large.lowering().nodes().get(0).expect("plan node").id();
    let same_index_type = large
        .types()
        .node_derivations()
        .get(0)
        .expect("node derivation")
        .type_id();
    let same_index_component = large
        .dependency()
        .dependency_order()
        .get(0)
        .expect("component");

    assert_ne!(same_index_node.revision(), small.program().revision());
    assert!(small.program().node(same_index_node).is_none());
    assert!(small.program().rule(same_index_rule).is_none());
    assert!(small.program().field(same_index_field).is_none());
    assert!(
        small
            .dependency()
            .graph()
            .dependencies(same_index_rule)
            .is_none()
    );
    assert!(small.dependency().component(same_index_component).is_none());
    assert!(small.dependency().component_of(same_index_rule).is_none());
    assert_eq!(small.dependency().is_recursive(same_index_component), None);
    assert!(small.types().semantic_type(same_index_type).is_none());
    assert!(small.types().node_type(same_index_node).is_none());
    assert!(small.types().rule_type(same_index_rule).is_none());
    assert!(small.types().rule_root_type(same_index_rule).is_none());
    assert!(small.facts().node(same_index_node).is_none());
    assert!(small.facts().rule(same_index_rule).is_none());
    assert!(small.lowering().node(same_index_plan_node).is_none());
    assert!(small.remap().get(same_index_node).is_none());

    let foreign_node = large
        .program()
        .nodes()
        .next_back()
        .expect("large node")
        .id();
    let foreign_rule = large
        .program()
        .rules()
        .next_back()
        .expect("large rule")
        .id();
    let foreign_field = large
        .program()
        .fields()
        .next_back()
        .expect("large field")
        .id();
    let foreign_plan_node = large
        .lowering()
        .nodes()
        .next_back()
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
    assert_eq!(small.dependency().is_recursive(foreign_component), None);
    assert!(small.types().semantic_type(foreign_type).is_none());
    assert!(small.types().node_type(foreign_node).is_none());
    assert!(small.types().rule_type(foreign_rule).is_none());
    assert!(small.types().rule_root_type(foreign_rule).is_none());
    assert!(small.facts().node(foreign_node).is_none());
    assert!(small.facts().rule(foreign_rule).is_none());
    assert!(small.lowering().node(foreign_plan_node).is_none());
}

#[test]
fn revision_brand_survives_identical_rebuilds_and_invalidates_across_edits() {
    const PATH: &str = "revision-brand.bbnf";
    const SOURCE: &[u8] = b"root = \"a\";";

    let first = accepted(PATH, std::str::from_utf8(SOURCE).expect("ASCII fixture"));
    let second = accepted(PATH, std::str::from_utf8(SOURCE).expect("ASCII fixture"));
    let original_node = first.program().nodes().get(0).expect("node").id();
    let original_rule = first.program().rules().get(0).expect("rule").id();
    let original_type = first
        .types()
        .node_derivations()
        .get(0)
        .expect("node derivation")
        .type_id();
    let original_plan_node = first.lowering().nodes().get(0).expect("plan node").id();

    assert_eq!(first.program().revision(), second.program().revision());
    assert!(second.program().node(original_node).is_some());
    assert!(second.program().rule(original_rule).is_some());
    assert!(second.types().semantic_type(original_type).is_some());
    assert!(second.types().node_type(original_node).is_some());
    assert!(second.facts().node(original_node).is_some());
    assert!(second.lowering().node(original_plan_node).is_some());

    let edited = accepted(PATH, r#"root = "b";"#);
    assert_ne!(edited.program().revision(), first.program().revision());
    assert!(edited.program().node(original_node).is_none());
    assert!(edited.program().rule(original_rule).is_none());
    assert!(edited.types().semantic_type(original_type).is_none());
    assert!(edited.types().node_type(original_node).is_none());
    assert!(edited.facts().node(original_node).is_none());
    assert!(edited.lowering().node(original_plan_node).is_none());
    assert!(edited.remap().get(original_node).is_none());

    let restored = accepted(PATH, std::str::from_utf8(SOURCE).expect("ASCII fixture"));
    assert_eq!(restored.program().revision(), first.program().revision());
    assert!(restored.program().node(original_node).is_some());

    let program_packet: serde_json::Value =
        serde_json::from_slice(&restored.program().replay_json().expect("program replay"))
            .expect("program replay is JSON");
    assert!(
        program_packet["value"]["nodes"][0]["id"].is_number(),
        "wire packets retain compact carrier-local ordinals under a revision envelope"
    );
}

#[test]
fn every_outbound_capability_uses_its_carrier_revision_and_path_is_identity() {
    const SOURCE: &str = r#"root = %{ value: @capture("x") };"#;
    let left = accepted("left/path.bbnf", SOURCE);
    let right = accepted("right/path.bbnf", SOURCE);
    let revision = left.program().revision();

    assert_ne!(
        revision,
        right.program().revision(),
        "logical path is part of source identity even when bytes match"
    );

    for node in left.program().nodes() {
        assert_eq!(node.id().revision(), revision);
    }
    for rule in left.program().rules() {
        assert_eq!(rule.id().revision(), revision);
    }
    for field in left.program().fields() {
        assert_eq!(field.id().revision(), revision);
        assert_eq!(field.owner().revision(), revision);
        assert_eq!(field.child().revision(), revision);
    }
    for component in left.dependency().dependency_order() {
        assert_eq!(component.revision(), revision);
        for rule in left
            .dependency()
            .component(component)
            .expect("component belongs to dependency carrier")
        {
            assert_eq!(rule.revision(), revision);
        }
    }
    for derivation in left.types().node_derivations() {
        assert_eq!(derivation.node().revision(), revision);
        assert_eq!(derivation.type_id().revision(), revision);
    }
    for node in left.lowering().nodes() {
        assert_eq!(node.id().revision(), revision);
        assert_eq!(node.semantic_type().revision(), revision);
    }
    for field in left.lowering().fields() {
        assert_eq!(field.id().revision(), revision);
        assert_eq!(field.owner().revision(), revision);
        assert_eq!(field.child().revision(), revision);
        assert_eq!(field.semantic_type().revision(), revision);
    }
    let transform = left.events().transforms().get(0).expect("transform site");
    assert_eq!(transform.id().revision(), revision);
    assert_eq!(transform.node().revision(), revision);
    assert!(left.events().transform(transform.id()).is_some());
    assert!(right.events().transform(transform.id()).is_none());

    let node = left.program().nodes().get(0).expect("first node").id();
    let remapped = left.remap().get(node).expect("identity remap");
    assert_eq!(left.remap().from_revision(), revision);
    assert_eq!(left.remap().to_revision(), revision);
    assert_eq!(remapped.revision(), left.remap().to_revision());
    assert_eq!(remapped.index(), node.index());
    assert!(right.remap().get(node).is_none());
}
