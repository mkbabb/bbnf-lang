use bbnf::generate::regex::classify::{RegexClass, classify_regex};

#[test]
fn test_negated_class_misclassification() {
    // This is the bug: [^{};] should NOT be classified as Identifier
    let pattern = "[^{};]";
    let result = classify_regex(pattern);

    println!("Pattern: {}", pattern);
    println!("Classification: {:?}", result);

    // Expected: Unknown (it's a negated class, not an identifier)
    // Actual (buggy): Identifier (because regex-syntax 0.8 materializes [^{};]
    //   as positive ranges that include a-z and A-Z)

    match result {
        RegexClass::Identifier => println!("BUG: Classified as Identifier!"),
        RegexClass::Unknown => println!("Correct: Classified as Unknown"),
        other => println!("Unexpected: {:?}", other),
    }
}

#[test]
fn test_negated_class_detailed() {
    use regex_syntax::hir::{Class, HirKind};

    let pattern = "[^{};]";
    let hir = regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .build()
        .parse(pattern)
        .unwrap();

    println!("\nDetailed analysis of {}", pattern);
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        println!("Total ranges: {}", ranges.len());

        // Show first 5 and last 5 ranges for brevity
        for (i, r) in ranges.iter().enumerate() {
            if i < 5 || i >= ranges.len().saturating_sub(5) {
                println!("  [{}] 0x{:02X}-0x{:02X}", i, r.start(), r.end());
            } else if i == 5 {
                println!("  ... ({} more ranges) ...", ranges.len() - 10);
            }
        }

        // Count total bytes covered
        let total_bytes: usize = ranges
            .iter()
            .map(|r| (r.end() - r.start() + 1) as usize)
            .sum();
        println!("Total bytes covered: {}", total_bytes);

        // Check for letter ranges
        let has_lower = ranges.iter().any(|r| r.start() <= b'a' && r.end() >= b'z');
        let has_upper = ranges.iter().any(|r| r.start() <= b'A' && r.end() >= b'Z');
        println!("Has a-z: {}, Has A-Z: {}", has_lower, has_upper);
    }
}

#[test]
fn test_positive_identifier_class() {
    let pattern = "[a-zA-Z_]";
    let result = classify_regex(pattern);

    println!("\nPositive identifier class: {}", pattern);
    println!("Classification: {:?}", result);
}
