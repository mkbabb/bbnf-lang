include!(concat!(env!("OUT_DIR"), "/generated.rs"));

const GENERATED: &str = include_str!(concat!(env!("OUT_DIR"), "/generated.rs"));
const MANIFEST: &str = include_str!(concat!(env!("OUT_DIR"), "/generated.manifest.json"));

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct View {
    kind: EventKind,
    rule: Option<u32>,
    node: Option<u32>,
    field: Option<u32>,
    transform: Option<u32>,
    start: u32,
    end: u32,
    value: u32,
}

fn view(event: &ScalarEvent) -> View {
    View {
        kind: event.kind(),
        rule: event.rule(),
        node: event.node(),
        field: event.field(),
        transform: event.transform(),
        start: event.start(),
        end: event.end(),
        value: event.value(),
    }
}

#[test]
fn nominal_record_descriptors_are_static_and_revision_bound() {
    assert_eq!(EVENT_SCHEMA, "bbnf.scalar-events.v4");
    assert_eq!(FIELDS.len(), 2);
    assert_eq!(
        (
            FIELDS[0].id(),
            FIELDS[0].owner_rule(),
            FIELDS[0].source_slot(),
            FIELDS[0].name(),
        ),
        (0, 2, 1, "children")
    );
    assert_eq!(
        (
            FIELDS[1].id(),
            FIELDS[1].owner_rule(),
            FIELDS[1].source_slot(),
            FIELDS[1].name(),
        ),
        (1, 3, 0, "text")
    );
    assert!(FIELDS[0].semantic_type() != FIELDS[1].semantic_type());
    assert!(MANIFEST.contains("\"schema\":\"bbnf.generated-rust.v4\""));
    assert!(MANIFEST.contains("\"event_schema\":\"bbnf.scalar-events.v4\""));
    assert!(MANIFEST.contains("\"fields\":2"));
}

#[test]
fn recursive_record_events_are_exact_in_both_sink_modes() {
    let input = b"(x)";
    let checked = check(input);
    assert!(matches!(
        checked.outcome(),
        parse_that::Step::Matched(span) if span.as_bytes() == input
    ));
    let parsed = parse_events(input);
    assert!(matches!(
        parsed.outcome(),
        parse_that::Step::Matched(span) if span.as_bytes() == input
    ));
    let actual = parsed.sink().events().iter().map(view).collect::<Vec<_>>();

    // Filled from the sealed grammar carrier, not reconstructed from runtime
    // parser control. Changes require an explicit event-schema fixture edit.
    let expected = expected_single_child();
    assert_eq!(actual, expected);
    assert_eq!(
        canonical_digest(&actual),
        "55a8a603ffd3b5633f37893d9e9ad0d1d7d8961818adfdc7cb701ccedf60e636"
    );
}

#[test]
fn later_slot_mismatch_and_resource_fault_roll_back_every_field_event() {
    let mismatch = parse_events(b"(x");
    assert!(matches!(
        mismatch.outcome(),
        parse_that::Step::Mismatch(_)
    ));
    assert!(mismatch.sink().events().is_empty());

    let limited = parse_events_with_limits(
        b"(x)",
        LimitsConfig::default().with_logical_events(6),
    );
    assert!(matches!(
        limited.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: parse_that::ResourceKind::Events,
            ..
        })
    ));
    assert!(limited.sink().events().is_empty());
}

#[test]
fn record_control_has_no_host_action_or_runtime_grammar_dispatch() {
    for forbidden in [
        "callback",
        "Action",
        "GrammarIR",
        "SourceModule",
        "match rule_name",
        "serde_json",
        "Value",
        "Tape",
        "Dom",
        "Path",
    ] {
        assert!(
            !GENERATED.contains(forbidden),
            "forbidden runtime surface `{forbidden}` leaked into generated control"
        );
    }
    assert!(GENERATED.contains("pub const FIELDS"));
    assert!(GENERATED.contains("EventKind::EnterField"));
    assert!(GENERATED.contains("EventKind::LeaveField"));
}

fn expected_single_child() -> Vec<View> {
    use EventKind::{
        ChoiceBranch, EndInput, EnterField, EnterRule, LeaveField, LeaveRule, Literal,
        Regex, RepeatCount, Transform,
    };
    vec![
        v(EnterRule, Some(0), None, None, None, 0, 0, 0),
        v(EnterRule, Some(1), None, None, None, 0, 0, 0),
        v(EnterRule, Some(2), None, None, None, 0, 0, 0),
        v(Literal, None, Some(4), None, None, 0, 1, 0),
        v(EnterField, Some(2), Some(6), Some(0), None, 1, 1, 0),
        v(EnterRule, Some(1), None, None, None, 1, 1, 0),
        v(EnterRule, Some(3), None, None, None, 1, 1, 0),
        v(EnterField, Some(3), Some(10), Some(1), None, 1, 1, 0),
        v(Regex, None, Some(9), None, None, 1, 2, 0),
        v(Transform, None, Some(10), None, Some(0), 1, 2, 0),
        v(LeaveField, Some(3), Some(10), Some(1), None, 1, 2, 0),
        v(LeaveRule, Some(3), None, None, None, 1, 2, 0),
        v(ChoiceBranch, None, Some(3), None, None, 1, 2, 1),
        v(LeaveRule, Some(1), None, None, None, 1, 2, 0),
        v(RepeatCount, None, Some(6), None, None, 1, 2, 1),
        v(LeaveField, Some(2), Some(6), Some(0), None, 1, 2, 0),
        v(Literal, None, Some(7), None, None, 2, 3, 0),
        v(LeaveRule, Some(2), None, None, None, 0, 3, 0),
        v(ChoiceBranch, None, Some(3), None, None, 0, 3, 0),
        v(LeaveRule, Some(1), None, None, None, 0, 3, 0),
        v(LeaveRule, Some(0), None, None, None, 0, 3, 0),
        v(EndInput, None, None, None, None, 3, 3, 0),
    ]
}

#[allow(clippy::too_many_arguments)]
const fn v(
    kind: EventKind,
    rule: Option<u32>,
    node: Option<u32>,
    field: Option<u32>,
    transform: Option<u32>,
    start: u32,
    end: u32,
    value: u32,
) -> View {
    View {
        kind,
        rule,
        node,
        field,
        transform,
        start,
        end,
        value,
    }
}

fn canonical_digest(events: &[View]) -> String {
    let mut bytes = Vec::with_capacity(events.len() * 32);
    for event in events {
        bytes.push(match event.kind {
            EventKind::EnterRule => 0,
            EventKind::LeaveRule => 1,
            EventKind::EnterField => 2,
            EventKind::LeaveField => 3,
            EventKind::Literal => 4,
            EventKind::Regex => 5,
            EventKind::ChoiceBranch => 6,
            EventKind::RepeatCount => 7,
            EventKind::Transform => 8,
            EventKind::EndInput => 9,
        });
        for value in [event.rule, event.node, event.field, event.transform] {
            bytes.extend_from_slice(&value.unwrap_or(u32::MAX).to_le_bytes());
        }
        bytes.extend_from_slice(&event.start.to_le_bytes());
        bytes.extend_from_slice(&event.end.to_le_bytes());
        bytes.extend_from_slice(&event.value.to_le_bytes());
    }
    blake3::hash(&bytes).to_hex().to_string()
}
