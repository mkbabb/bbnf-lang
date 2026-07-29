include!(concat!(env!("OUT_DIR"), "/generated.rs"));

const GENERATED: &str = include_str!(concat!(env!("OUT_DIR"), "/generated.rs"));

fn assert_match(input: &[u8]) {
    let checked = check(input);
    assert!(
        matches!(checked.outcome(), parse_that::Step::Matched(span) if span.as_bytes() == input),
        "recognition mode rejected {input:?}: {:?}",
        checked.outcome()
    );

    let parsed = parse_events(input);
    assert!(
        matches!(parsed.outcome(), parse_that::Step::Matched(span) if span.as_bytes() == input),
        "event mode rejected {input:?}: {:?}",
        parsed.outcome()
    );
    assert!(parsed.sink().events().iter().all(|event| {
        event.start() <= event.end() && usize::try_from(event.end()).unwrap() <= input.len()
    }));
    assert_eq!(
        parsed.sink().events().last().map(ScalarEvent::kind),
        Some(EventKind::EndInput)
    );
}

fn assert_mismatch(input: &[u8]) {
    let checked = check(input);
    assert!(
        matches!(checked.outcome(), parse_that::Step::Mismatch(_)),
        "recognition mode accepted {input:?}: {:?}",
        checked.outcome()
    );

    let parsed = parse_events(input);
    assert!(
        matches!(parsed.outcome(), parse_that::Step::Mismatch(_)),
        "event mode accepted {input:?}: {:?}",
        parsed.outcome()
    );
    assert!(
        parsed.sink().events().is_empty(),
        "mismatch retained transactional events for {input:?}"
    );
}

fn assert_fault_both_modes(input: &[u8], limits: LimitsConfig, kind: parse_that::ResourceKind) {
    let checked = check_with_limits(input, limits);
    assert!(matches!(
        checked.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: actual,
            ..
        }) if *actual == kind
    ));

    let parsed = parse_events_with_limits(input, limits);
    assert!(matches!(
        parsed.outcome(),
        parse_that::Step::Fault(parse_that::Fault::ResourceLimit {
            kind: actual,
            ..
        }) if *actual == kind
    ));
    assert!(
        parsed.sink().events().is_empty(),
        "fault retained transactional events"
    );
}

#[test]
fn genuinely_recursive_generated_control_accepts_and_rejects_nested_inputs() {
    for input in [b"x".as_slice(), b"(x)", b"((x))", b"((((x))))"] {
        assert_match(input);
    }
    for input in [
        b"".as_slice(),
        b"(",
        b"(x",
        b"x)",
        b"()",
        b"((x)))",
        b"((y))",
    ] {
        assert_mismatch(input);
    }
}

#[test]
fn recursive_event_order_has_an_independent_exact_oracle() {
    let parsed = parse_events(b"(x)");
    assert!(matches!(
        parsed.outcome(),
        parse_that::Step::Matched(span) if span.as_bytes() == b"(x)"
    ));
    let actual = parsed
        .sink()
        .events()
        .iter()
        .map(|event| {
            (
                event.kind(),
                event.rule(),
                event.node(),
                event.field(),
                event.transform(),
                event.start(),
                event.end(),
                event.value(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        actual,
        vec![
            (EventKind::EnterRule, Some(0), None, None, None, 0, 0, 0),
            (EventKind::Literal, None, Some(0), None, None, 0, 1, 0),
            (EventKind::EnterRule, Some(0), None, None, None, 1, 1, 0),
            (EventKind::Literal, None, Some(4), None, None, 1, 2, 0),
            (
                EventKind::ChoiceBranch,
                None,
                Some(5),
                None,
                None,
                1,
                2,
                1,
            ),
            (EventKind::LeaveRule, Some(0), None, None, None, 1, 2, 0),
            (EventKind::Literal, None, Some(2), None, None, 2, 3, 0),
            (
                EventKind::ChoiceBranch,
                None,
                Some(5),
                None,
                None,
                0,
                3,
                0,
            ),
            (EventKind::LeaveRule, Some(0), None, None, None, 0, 3, 0),
            (EventKind::EndInput, None, None, None, None, 3, 3, 0),
        ]
    );
}

#[test]
fn recursive_faults_are_typed_mode_invariant_and_transactional() {
    let input = b"((x))";
    assert_fault_both_modes(
        input,
        LimitsConfig::default().with_input_bytes(input.len() - 1),
        parse_that::ResourceKind::Work,
    );
    assert_fault_both_modes(
        input,
        LimitsConfig::default().with_nesting(2),
        parse_that::ResourceKind::Nesting,
    );
    assert_fault_both_modes(
        input,
        LimitsConfig::default().with_logical_events(1),
        parse_that::ResourceKind::Events,
    );
    assert_fault_both_modes(
        input,
        LimitsConfig::default().with_work(1),
        parse_that::ResourceKind::Work,
    );
}

#[test]
fn custom_limits_can_only_tighten_baked_runtime_ceilings() {
    let oversized = LimitsConfig::default()
        .with_input_bytes(usize::MAX)
        .with_nesting(usize::MAX)
        .with_logical_events(usize::MAX)
        .with_work(usize::MAX);
    let defaults = LimitsConfig::default();
    let oversized_input = vec![b'x'; defaults.input_bytes() + 1];
    assert_fault_both_modes(&oversized_input, oversized, parse_that::ResourceKind::Work);
}

#[test]
fn recursive_literal_only_product_has_minimal_imports_and_source_identity() {
    assert!(GENERATED.contains("choice"));
    assert!(GENERATED.contains("literal"));
    for forbidden in ["RegexPlan", "RegexTerminal", "regex,", "repeat,"] {
        assert!(
            !GENERATED.contains(forbidden),
            "unused parser primitive `{forbidden}` leaked into generated imports"
        );
    }

    let parsed = parse_events(b"((x))");
    let (_, outcome, buffer) = parsed.into_parts();
    assert!(matches!(outcome, parse_that::Step::Matched(_)));
    let stream = buffer.into_stream();
    assert_eq!(stream.revision(), REVISION);
    assert_eq!(stream.source().bytes(), b"((x))");
    let (revision, source, events) = stream.into_parts();
    assert_eq!(revision, REVISION);
    assert_eq!(source.bytes(), b"((x))");
    assert!(!events.is_empty());
}
