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
        parsed.sink().events().last().map(view),
        Some(scalar(
            EventKind::EndInput,
            None,
            None,
            None,
            INPUT.len() as u32,
            INPUT.len() as u32,
            0,
        ))
    );
}

#[test]
fn choice_and_repeat_restore_cursor_and_events_before_success() {
    let result = parse_events(INPUT);
    let events = result.sink().events();
    assert_eq!(
        views(events),
        &[
            scalar(EventKind::EnterRule, Some(0), None, None, 0, 0, 0),
            scalar(EventKind::EnterRule, Some(1), None, None, 0, 0, 0),
            scalar(EventKind::Literal, None, Some(11), None, 0, 1, 0),
            scalar(EventKind::Literal, None, Some(12), None, 1, 2, 0),
            scalar(EventKind::Transform, None, Some(14), Some(1), 0, 2, 0),
            scalar(EventKind::ChoiceBranch, None, Some(15), None, 0, 2, 1,),
            scalar(EventKind::LeaveRule, Some(1), None, None, 0, 2, 0),
            scalar(EventKind::EnterRule, Some(2), None, None, 2, 2, 0),
            scalar(EventKind::Regex, None, Some(16), None, 2, 5, 0),
            scalar(EventKind::LeaveRule, Some(2), None, None, 2, 5, 0),
            scalar(EventKind::EnterRule, Some(3), None, None, 5, 5, 0),
            scalar(EventKind::Literal, None, Some(17), None, 5, 6, 0),
            scalar(EventKind::Literal, None, Some(18), None, 6, 7, 0),
            scalar(EventKind::LeaveRule, Some(3), None, None, 5, 7, 0),
            scalar(EventKind::RepeatCount, None, Some(3), None, 5, 7, 1,),
            scalar(EventKind::Literal, None, Some(4), None, 7, 8, 0),
            scalar(EventKind::Literal, None, Some(5), None, 8, 9, 0),
            scalar(EventKind::LeaveRule, Some(0), None, None, 0, 9, 0),
            scalar(EventKind::EndInput, None, None, None, 9, 9, 0),
        ]
    );

    let regex = events
        .iter()
        .find(|event| event.kind() == EventKind::Regex)
        .expect("digits regex event");
    assert_eq!((regex.start(), regex.end()), (2, 5));

    let literal_spans = events
        .iter()
        .filter(|event| event.kind() == EventKind::Literal)
        .map(|event| (event.start(), event.end()))
        .collect::<Vec<_>>();
    assert_eq!(
        literal_spans,
        vec![(0, 1), (1, 2), (5, 6), (6, 7), (7, 8), (8, 9)]
    );

    let branch = events
        .iter()
        .find(|event| event.kind() == EventKind::ChoiceBranch)
        .expect("choice branch event");
    assert_eq!(branch.value(), 1);

    let repetition = events
        .iter()
        .find(|event| event.kind() == EventKind::RepeatCount)
        .expect("repeat count event");
    assert_eq!(repetition.value(), 1);

    assert_eq!(
        TRANSFORMS
            .iter()
            .map(|descriptor| (descriptor.id(), descriptor.name()))
            .collect::<Vec<_>>(),
        [(0, "discarded"), (1, "chosen")]
    );
    let retained_transforms = events
        .iter()
        .filter(|event| event.kind() == EventKind::Transform)
        .collect::<Vec<_>>();
    assert_eq!(retained_transforms.len(), 1);
    assert_eq!(retained_transforms[0].transform(), Some(1));
    assert_eq!(
        (retained_transforms[0].start(), retained_transforms[0].end(),),
        (0, 2)
    );
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ExpectedEvent {
    kind: EventKind,
    rule: Option<u32>,
    node: Option<u32>,
    transform: Option<u32>,
    start: u32,
    end: u32,
    value: u32,
}

const fn scalar(
    kind: EventKind,
    rule: Option<u32>,
    node: Option<u32>,
    transform: Option<u32>,
    start: u32,
    end: u32,
    value: u32,
) -> ExpectedEvent {
    ExpectedEvent {
        kind,
        rule,
        node,
        transform,
        start,
        end,
        value,
    }
}

fn view(event: &ScalarEvent) -> ExpectedEvent {
    scalar(
        event.kind(),
        event.rule(),
        event.node(),
        event.transform(),
        event.start(),
        event.end(),
        event.value(),
    )
}

fn views(events: &[ScalarEvent]) -> Vec<ExpectedEvent> {
    events.iter().map(view).collect()
}

fn oracle(input: &[u8]) -> Option<Vec<ExpectedEvent>> {
    let mut events = vec![
        scalar(EventKind::EnterRule, Some(0), None, None, 0, 0, 0),
        scalar(EventKind::EnterRule, Some(1), None, None, 0, 0, 0),
    ];

    match input.get(..2)? {
        b"ax" => {
            events.push(scalar(EventKind::Literal, None, Some(7), None, 0, 1, 0));
            events.push(scalar(
                EventKind::Transform,
                None,
                Some(8),
                Some(0),
                0,
                1,
                0,
            ));
            events.push(scalar(EventKind::Literal, None, Some(9), None, 1, 2, 0));
            events.push(scalar(
                EventKind::ChoiceBranch,
                None,
                Some(15),
                None,
                0,
                2,
                0,
            ));
        }
        b"ab" => {
            // The failed `@discarded("a"), "x"` branch has already consumed
            // and emitted before it mismatches. Transactionality requires
            // that neither its literal nor transform survive here.
            events.push(scalar(EventKind::Literal, None, Some(11), None, 0, 1, 0));
            events.push(scalar(EventKind::Literal, None, Some(12), None, 1, 2, 0));
            events.push(scalar(
                EventKind::Transform,
                None,
                Some(14),
                Some(1),
                0,
                2,
                0,
            ));
            events.push(scalar(
                EventKind::ChoiceBranch,
                None,
                Some(15),
                None,
                0,
                2,
                1,
            ));
        }
        _ => return None,
    }
    events.push(scalar(EventKind::LeaveRule, Some(1), None, None, 0, 2, 0));

    let mut at = 2usize;
    let digits_start = at;
    while input.get(at).is_some_and(u8::is_ascii_digit) {
        at += 1;
    }
    if at == digits_start {
        return None;
    }
    events.push(scalar(
        EventKind::EnterRule,
        Some(2),
        None,
        None,
        digits_start as u32,
        digits_start as u32,
        0,
    ));
    events.push(scalar(
        EventKind::Regex,
        None,
        Some(16),
        None,
        digits_start as u32,
        at as u32,
        0,
    ));
    events.push(scalar(
        EventKind::LeaveRule,
        Some(2),
        None,
        None,
        digits_start as u32,
        at as u32,
        0,
    ));

    let pairs_start = at;
    let mut pairs = 0u32;
    while input.get(at..at.saturating_add(2)) == Some(b"xy") {
        let start = at as u32;
        events.push(scalar(
            EventKind::EnterRule,
            Some(3),
            None,
            None,
            start,
            start,
            0,
        ));
        events.push(scalar(
            EventKind::Literal,
            None,
            Some(17),
            None,
            start,
            start + 1,
            0,
        ));
        events.push(scalar(
            EventKind::Literal,
            None,
            Some(18),
            None,
            start + 1,
            start + 2,
            0,
        ));
        at += 2;
        pairs += 1;
        events.push(scalar(
            EventKind::LeaveRule,
            Some(3),
            None,
            None,
            start,
            at as u32,
            0,
        ));
    }
    events.push(scalar(
        EventKind::RepeatCount,
        None,
        Some(3),
        None,
        pairs_start as u32,
        at as u32,
        pairs,
    ));

    if input.get(at) != Some(&b'x') || input.get(at + 1) != Some(&b'!') {
        return None;
    }
    events.push(scalar(
        EventKind::Literal,
        None,
        Some(4),
        None,
        at as u32,
        (at + 1) as u32,
        0,
    ));
    events.push(scalar(
        EventKind::Literal,
        None,
        Some(5),
        None,
        (at + 1) as u32,
        (at + 2) as u32,
        0,
    ));
    at += 2;
    if at != input.len() {
        return None;
    }
    events.push(scalar(
        EventKind::LeaveRule,
        Some(0),
        None,
        None,
        0,
        at as u32,
        0,
    ));
    events.push(scalar(
        EventKind::EndInput,
        None,
        None,
        None,
        at as u32,
        at as u32,
        0,
    ));
    Some(events)
}

fn assert_oracle_conformance(input: &[u8]) {
    let expected = oracle(input);
    let checked = check(input);
    let parsed = parse_events(input);
    match expected {
        Some(events) => {
            assert!(
                matches!(checked.outcome(), parse_that::Step::Matched(span) if span.as_bytes() == input),
                "check rejected oracle-positive input {input:?}: {:?}",
                checked.outcome()
            );
            assert!(
                matches!(parsed.outcome(), parse_that::Step::Matched(span) if span.as_bytes() == input),
                "event parse rejected oracle-positive input {input:?}: {:?}",
                parsed.outcome()
            );
            assert_eq!(
                views(parsed.sink().events()),
                events,
                "event divergence for input {input:?}"
            );
        }
        None => {
            assert!(
                matches!(checked.outcome(), parse_that::Step::Mismatch(_)),
                "check accepted oracle-negative input {input:?}: {:?}",
                checked.outcome()
            );
            assert!(
                matches!(parsed.outcome(), parse_that::Step::Mismatch(_)),
                "event parse accepted oracle-negative input {input:?}: {:?}",
                parsed.outcome()
            );
            assert!(
                parsed.sink().events().is_empty(),
                "rejected input retained transactional events: {input:?}"
            );
        }
    }
}

#[test]
fn deterministic_randomized_event_conformance_matches_independent_oracle() {
    for branch in [b"ax".as_slice(), b"ab".as_slice()] {
        for digits in [
            b"0".as_slice(),
            b"7".as_slice(),
            b"42".as_slice(),
            b"123".as_slice(),
            b"99999".as_slice(),
        ] {
            for pair_count in 0..=6 {
                let mut input = Vec::new();
                input.extend_from_slice(branch);
                input.extend_from_slice(digits);
                for _ in 0..pair_count {
                    input.extend_from_slice(b"xy");
                }
                input.extend_from_slice(b"x!");
                assert_oracle_conformance(&input);
            }
        }
    }

    for input in [
        b"".as_slice(),
        b"a",
        b"ab",
        b"ab0",
        b"ab0x",
        b"ab0x!!",
        b"ab0xy!",
        b"ax1xyx!tail",
    ] {
        assert_oracle_conformance(input);
    }

    // Fixed-seed xorshift64: reproducible broad negatives plus any positives
    // reached by the byte language. The oracle is a separate handwritten
    // recognizer and event constructor; it does not reuse generated control.
    const ALPHABET: &[u8] = b"abxy!01239q";
    let mut state = 0x9e37_79b9_7f4a_7c15u64;
    for _ in 0..2_048 {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        let len = (state as usize) % 24;
        let mut input = Vec::with_capacity(len);
        for _ in 0..len {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            input.push(ALPHABET[(state as usize) % ALPHABET.len()]);
        }
        assert_oracle_conformance(&input);
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
            logical_events: 1,
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

    let no_sink_event_limited = check_with_limits(
        INPUT,
        LimitsConfig {
            logical_events: 1,
            ..LimitsConfig::default()
        },
    );
    assert!(matches!(
        no_sink_event_limited.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: parse_that::ResourceKind::Events,
            ..
        })
    ));

    let work_limited = parse_events_with_limits(
        INPUT,
        LimitsConfig {
            work: 1,
            ..LimitsConfig::default()
        },
    );
    assert!(matches!(
        work_limited.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: parse_that::ResourceKind::Work,
            ..
        })
    ));
    assert!(work_limited.sink().events().is_empty());
}

#[test]
fn generated_manifest_is_schema_and_revision_bound() {
    let manifest = include_str!(concat!(env!("OUT_DIR"), "/generated.manifest.json"));
    assert!(manifest.contains("\"schema\":\"bbnf.generated-rust.v2\""));
    assert!(manifest.contains("\"event_schema\":\"bbnf.scalar-events.v2\""));
    assert!(manifest.contains("\"transforms\":2"));
    assert!(manifest.contains(REVISION));
}

#[test]
fn event_stream_preserves_revision_source_and_sealed_policies() {
    let parsed = parse_events(INPUT);
    let (_, outcome, buffer) = parsed.into_parts();
    assert!(matches!(outcome, parse_that::Step::Matched(_)));
    assert_eq!(buffer.revision(), REVISION);
    assert_eq!(buffer.source().bytes().as_ptr(), INPUT.as_ptr());
    assert_eq!(buffer.source().bytes().len(), INPUT.len());

    let stream = buffer.into_stream();
    assert_eq!(stream.revision(), REVISION);
    assert_eq!(stream.source().bytes().as_ptr(), INPUT.as_ptr());
    assert_eq!(stream.source().bytes().len(), INPUT.len());
    assert!(!stream.events().is_empty());

    assert_eq!(
        RAW_SOURCE_SPAN_NORMALIZATION,
        RawSourceSpanNormalization::OriginalInputByteOffsets
    );
    assert_eq!(RECOVERY_POLICY, RecoveryPolicy::Disabled);
    assert_eq!(
        TRANSACTIONAL_ERROR_POLICY,
        TransactionalErrorPolicy::RollbackAllEventsOnMismatchOrFault
    );
}
