include!(concat!(env!("OUT_DIR"), "/generated.rs"));

const INPUT: &[u8] = b"ab123xyx!";

#[test]
fn both_sink_modes_execute_the_same_consuming_control() {
    let checked = check(INPUT);
    assert!(
        matches!(checked.outcome(), parse_that::Step::Matched(span) if span.as_bytes() == INPUT)
    );
    assert_eq!(checked.sink().len(), 0);

    let parsed = parse_events(INPUT);
    assert!(
        matches!(parsed.outcome(), parse_that::Step::Matched(span) if span.as_bytes() == INPUT)
    );
    assert!(!parsed.sink().events().is_empty());
    assert_eq!(
        parsed.sink().events().last(),
        Some(&ScalarEvent {
            kind: EventKind::EndInput,
            rule: None,
            node: None,
            start: INPUT.len() as u32,
            end: INPUT.len() as u32,
            value: 0,
        })
    );
}

#[test]
fn choice_and_repeat_restore_cursor_and_events_before_success() {
    let result = parse_events(INPUT);
    let events = result.sink().events();
    assert_eq!(
        events,
        &[
            scalar(EventKind::EnterRule, Some(0), None, 0, 0, 0),
            scalar(EventKind::EnterRule, Some(1), None, 0, 0, 0),
            scalar(EventKind::Literal, None, Some(10), 0, 1, 0),
            scalar(EventKind::Literal, None, Some(11), 1, 2, 0),
            scalar(EventKind::ChoiceBranch, None, Some(13), 0, 2, 1),
            scalar(EventKind::LeaveRule, Some(1), None, 0, 2, 0),
            scalar(EventKind::EnterRule, Some(2), None, 2, 2, 0),
            scalar(EventKind::Regex, None, Some(14), 2, 5, 0),
            scalar(EventKind::LeaveRule, Some(2), None, 2, 5, 0),
            scalar(EventKind::EnterRule, Some(3), None, 5, 5, 0),
            scalar(EventKind::Literal, None, Some(15), 5, 6, 0),
            scalar(EventKind::Literal, None, Some(16), 6, 7, 0),
            scalar(EventKind::LeaveRule, Some(3), None, 5, 7, 0),
            scalar(EventKind::RepeatCount, None, Some(3), 5, 7, 1),
            scalar(EventKind::Literal, None, Some(4), 7, 8, 0),
            scalar(EventKind::Literal, None, Some(5), 8, 9, 0),
            scalar(EventKind::LeaveRule, Some(0), None, 0, 9, 0),
            scalar(EventKind::EndInput, None, None, 9, 9, 0),
        ]
    );

    let regex = events
        .iter()
        .find(|event| event.kind == EventKind::Regex)
        .expect("digits regex event");
    assert_eq!((regex.start, regex.end), (2, 5));

    let literal_spans = events
        .iter()
        .filter(|event| event.kind == EventKind::Literal)
        .map(|event| (event.start, event.end))
        .collect::<Vec<_>>();
    assert_eq!(
        literal_spans,
        vec![(0, 1), (1, 2), (5, 6), (6, 7), (7, 8), (8, 9)]
    );

    let branch = events
        .iter()
        .find(|event| event.kind == EventKind::ChoiceBranch)
        .expect("choice branch event");
    assert_eq!(branch.value, 1);

    let repetition = events
        .iter()
        .find(|event| event.kind == EventKind::RepeatCount)
        .expect("repeat count event");
    assert_eq!(repetition.value, 1);
}

const fn scalar(
    kind: EventKind,
    rule: Option<u32>,
    node: Option<u32>,
    start: u32,
    end: u32,
    value: u32,
) -> ScalarEvent {
    ScalarEvent {
        kind,
        rule,
        node,
        start,
        end,
        value,
    }
}

#[test]
fn longest_prefix_does_not_relax_eof_or_trailing_rejection() {
    let trailing = parse_events(b"ab123xyx!!");
    assert!(matches!(trailing.outcome(), parse_that::Step::Mismatch(_)));
    assert!(
        trailing.sink().events().is_empty(),
        "full-parse rejection must restore every retained event"
    );

    let missing = parse_events(b"abxyx!");
    assert!(matches!(missing.outcome(), parse_that::Step::Mismatch(_)));
    assert!(missing.sink().events().is_empty());
}

#[test]
fn resource_limits_are_typed_and_transactional() {
    let input_limited = parse_events_with_limits(
        INPUT,
        LimitsConfig {
            input_bytes: INPUT.len() - 1,
            ..LimitsConfig::default()
        },
    );
    assert!(matches!(
        input_limited.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: parse_that::ResourceKind::Work,
            ..
        })
    ));
    assert!(input_limited.sink().events().is_empty());

    let depth_limited = parse_events_with_limits(
        INPUT,
        LimitsConfig {
            nesting: 1,
            ..LimitsConfig::default()
        },
    );
    assert!(matches!(
        depth_limited.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: parse_that::ResourceKind::Nesting,
            ..
        })
    ));
    assert!(depth_limited.sink().events().is_empty());

    let event_limited = parse_events_with_limits(
        INPUT,
        LimitsConfig {
            events: 1,
            ..LimitsConfig::default()
        },
    );
    assert!(matches!(
        event_limited.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: parse_that::ResourceKind::Events,
            ..
        })
    ));
    assert!(event_limited.sink().events().is_empty());
}

#[test]
fn generated_manifest_is_schema_and_revision_bound() {
    let manifest = include_str!(concat!(env!("OUT_DIR"), "/generated.manifest.json"));
    assert!(manifest.contains("\"schema\":\"bbnf.generated-rust.v1\""));
    assert!(manifest.contains("\"event_schema\":\"bbnf.scalar-events.v1\""));
    assert!(manifest.contains(REVISION));
}
