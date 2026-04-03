use bbnf::generate::regex::classify::classify_regex;

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

        // Demonstrate the negation detection
        use regex_syntax::hir::{Class, HirKind};
        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .build()
            .parse(pattern)
            .unwrap();

        if let HirKind::Repetition(rep) = hir.kind() {
            if let HirKind::Class(Class::Bytes(bc)) = rep.sub.kind() {
                let ranges = bc.ranges();
                let total_bytes: usize = ranges
                    .iter()
                    .map(|r| (r.end() - r.start() + 1) as usize)
                    .sum();
                println!(
                    "  -> {} ranges, {} bytes covered",
                    ranges.len(),
                    total_bytes
                );
                println!("  -> is_negated_class should be: ranges > 3 AND total_bytes > 200");
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

        use regex_syntax::hir::{Class, HirKind};
        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .build()
            .parse(pattern)
            .unwrap();

        if let HirKind::Repetition(rep) = hir.kind() {
            if let HirKind::Class(Class::Bytes(bc)) = rep.sub.kind() {
                let ranges = bc.ranges();
                let total_bytes: usize = ranges
                    .iter()
                    .map(|r| (r.end() - r.start() + 1) as usize)
                    .sum();
                println!(
                    "  -> {} ranges, {} bytes covered",
                    ranges.len(),
                    total_bytes
                );
                println!(
                    "  -> is_negated_class should be: ranges > 3 OR total_bytes > 200? NO - positive"
                );
            }
        }
    }
}
