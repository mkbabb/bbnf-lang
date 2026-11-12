mod instance_support;

use bbnf::compiler::{PlanOp, PlanRecordSlot, RecordSlot, SemanticType};
use bbnf::{CompileOptions, StageResult};

fn accepted(source: &str) -> bbnf::CompiledModule {
    match instance_support::compile(
        "record-semantic-laws.bbnf",
        source.as_bytes(),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => panic!(
            "record fixture must compile:\n{}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        ),
    }
}

#[test]
fn record_projection_is_retained_exactly_from_program_through_lowering() {
    let module = accepted(
        r#"
root = %{ ~"<", tag: /[A-Z]+/, payload: @raw(/[a-z]+/), ~">" };
"#,
    );
    let rule = module.program().rules().get(0).expect("root rule");
    let fields = module.program().fields();
    assert_eq!(fields.len(), 2);
    assert_eq!(
        fields
            .iter()
            .map(|field| (
                field.id().index(),
                field.owner().index(),
                field.slot(),
                field.name(),
            ))
            .collect::<Vec<_>>(),
        vec![(0, 0, 1, "tag"), (1, 0, 2, "payload")]
    );

    let bbnf::compiler::NodeKind::Record { owner, slots } = module
        .program()
        .node(rule.root())
        .expect("rule root belongs to this program")
        .kind()
    else {
        panic!("rule root must be a nominal record");
    };
    assert_eq!(owner, rule.id());
    assert!(matches!(
        slots.get(0).expect("slot zero"),
        RecordSlot::Discard { .. }
    ));
    assert!(matches!(
        slots.get(1).expect("slot one"),
        RecordSlot::Field { field, .. } if field.index() == 0
    ));
    assert!(matches!(
        slots.get(2).expect("slot two"),
        RecordSlot::Field { field, .. } if field.index() == 1
    ));
    assert!(matches!(
        slots.get(3).expect("slot three"),
        RecordSlot::Discard { .. }
    ));

    let Some(SemanticType::Record {
        owner: type_owner,
        fields: type_fields,
    }) = module
        .types()
        .rule_root_type(rule.id())
        .and_then(|root| module.types().semantic_type(root))
    else {
        panic!("record syntax must retain a distinct semantic record type");
    };
    assert_eq!(type_owner, rule.id());
    assert_eq!(type_fields.len(), 2);
    assert_eq!(
        type_fields.get(0).expect("tag type").field(),
        fields.get(0).expect("tag field").id()
    );
    assert_eq!(
        type_fields.get(1).expect("payload type").field(),
        fields.get(1).expect("payload field").id()
    );
    assert!(matches!(
        module.types().verify(module.program()),
        StageResult::Accepted { .. }
    ));

    let plan_rule = module.lowering().rules().get(0).expect("root plan");
    assert_eq!(
        Some(plan_rule.named_type()),
        module.types().rule_type(rule.id())
    );
    let plan_node = module
        .lowering()
        .node(plan_rule.root())
        .expect("lowered rule root belongs to this plan");
    assert_eq!(
        Some(plan_node.semantic_type()),
        module.types().node_type(rule.root())
    );
    let PlanOp::Record {
        owner: plan_owner,
        slots: plan_slots,
    } = plan_node.op()
    else {
        panic!("lowering must preserve record recognition and projection");
    };
    assert_eq!(plan_owner, rule.id());
    assert!(matches!(
        plan_slots.get(0).expect("plan slot zero"),
        PlanRecordSlot::Discard { .. }
    ));
    assert!(matches!(
        plan_slots.get(1).expect("plan slot one"),
        PlanRecordSlot::Field { field, .. }
            if field == fields.get(0).expect("tag field").id()
    ));
    assert_eq!(
        module.lowering().semantic_types().collect::<Vec<_>>(),
        module.types().arena().collect::<Vec<_>>()
    );
    assert_eq!(module.lowering().fields().len(), 2);
    assert_eq!(
        module
            .lowering()
            .fields()
            .get(1)
            .expect("payload field")
            .name(),
        "payload"
    );
}

#[test]
fn discarded_slots_remain_recognition_inputs_and_empty_records_stay_nominal() {
    let module = accepted(
        r#"
root = value | empty;
value = %{ ~"prefix", body: "x" };
empty = %{};
"#,
    );
    let find_rule = |name: &str| {
        module
            .program()
            .rules()
            .iter()
            .find(|rule| rule.name() == name)
            .expect("fixture rule")
            .id()
    };
    let value = find_rule("value");
    let empty = find_rule("empty");
    assert!(!module.facts().rule(value).expect("value facts").nullable());
    assert_eq!(
        module
            .facts()
            .rule(value)
            .expect("value facts")
            .minimum_consumption(),
        Some(7)
    );
    assert!(module.facts().rule(empty).expect("empty facts").nullable());
    assert_eq!(
        module
            .facts()
            .rule(empty)
            .expect("empty facts")
            .minimum_consumption(),
        Some(0)
    );
    assert!(matches!(
        module
            .types()
            .rule_root_type(empty)
            .and_then(|root| module.types().semantic_type(root)),
        Some(SemanticType::Record { owner, fields }) if owner == empty && fields.is_empty()
    ));
}
