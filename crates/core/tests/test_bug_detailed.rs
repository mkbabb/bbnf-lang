use parse_that::regex::classify::classify_regex;

#[test]
fn test_negated_patterns_that_trigger_bug() {
    let test_cases = vec![
        ("[^{};]+", "Negated class with common delimiters"),
        ("[^}]+", "Negated single character"),
        ("[^:\\n]+", "Negated with escape"),
        ("[^abc]+", "Negated multi-char"),
    ];

    for (pattern, desc) in test_cases {
        let result = classify_regex(pattern);
        println!("{}: {} -> {:?}", pattern, desc, result);

        // Demonstrate the negation detection using the bespoke HIR.
        use parse_that::regex::hir::{CharClass, Hir};
        let hir = parse_that::regex::parse_with(
            pattern,
            &parse_that::regex::ParseOptions::byte_mode(),
        )
        .unwrap();

        if let Hir::Repetition(ref rep) = hir {
            if let Hir::Class(CharClass::Bytes { ref ranges, negated }) = *rep.sub {
                let total_bytes: usize = ranges
                    .iter()
                    .map(|r| (r.end - r.start + 1) as usize)
                    .sum();
                println!(
                    "  -> {} ranges, {} bytes covered, negated={}",
                    ranges.len(),
                    total_bytes,
                    negated,
                );
            }
        }
    }
}

#[test]
fn test_positive_patterns() {
    let test_cases = vec![
        ("[a-z]+", "Lowercase only"),
        ("[a-zA-Z]+", "Mixed case letters"),
        ("[a-zA-Z_]+", "With underscore"),
        ("[a-zA-Z0-9]+", "Alphanumeric"),
    ];

    for (pattern, desc) in test_cases {
        let result = classify_regex(pattern);
        println!("{}: {} -> {:?}", pattern, desc, result);

        use parse_that::regex::hir::{CharClass, Hir};
        let hir = parse_that::regex::parse_with(
            pattern,
            &parse_that::regex::ParseOptions::byte_mode(),
        )
        .unwrap();

        if let Hir::Repetition(ref rep) = hir {
            if let Hir::Class(CharClass::Bytes { ref ranges, negated }) = *rep.sub {
                let total_bytes: usize = ranges
                    .iter()
                    .map(|r| (r.end - r.start + 1) as usize)
                    .sum();
                println!(
                    "  -> {} ranges, {} bytes covered, negated={}",
                    ranges.len(),
                    total_bytes,
                    negated,
                );
            }
        }
    }
}
