use parse_that::regex::classify::{RegexClass, classify_regex};

#[test]
fn test_negated_class_misclassification() {
    // This is the bug: [^{};] should NOT be classified as Identifier
    let pattern = "[^{};]";
    let result = classify_regex(pattern);

    println!("Pattern: {}", pattern);
    println!("Classification: {:?}", result);

    // Expected: Unknown (it's a negated class, not an identifier)
    // The bespoke HIR preserves the negated flag, so this should work correctly.

    match result {
        RegexClass::Identifier => println!("BUG: Classified as Identifier!"),
        RegexClass::Unknown => println!("Correct: Classified as Unknown"),
        other => println!("Unexpected: {:?}", other),
    }
}

#[test]
fn test_negated_class_detailed() {
    use parse_that::regex::hir::{CharClass, Hir};

    let pattern = "[^{};]";
    let hir = parse_that::regex::parse_with(
        pattern,
        &parse_that::regex::ParseOptions::byte_mode(),
    )
    .unwrap();

    println!("\nDetailed analysis of {}", pattern);
    if let Hir::Class(CharClass::Bytes { ref ranges, negated }) = hir {
        println!("Total ranges: {}, negated: {}", ranges.len(), negated);

        // Show first 5 and last 5 ranges for brevity
        for (i, r) in ranges.iter().enumerate() {
            if i < 5 || i >= ranges.len().saturating_sub(5) {
                println!("  [{}] 0x{:02X}-0x{:02X}", i, r.start, r.end);
            } else if i == 5 {
                println!("  ... ({} more ranges) ...", ranges.len() - 10);
            }
        }

        // Count total bytes covered
        let total_bytes: usize = ranges
            .iter()
            .map(|r| (r.end - r.start + 1) as usize)
            .sum();
        println!("Total bytes covered: {} (negated={})", total_bytes, negated);
    }
}

#[test]
fn test_positive_identifier_class() {
    let pattern = "[a-zA-Z_]";
    let result = classify_regex(pattern);

    println!("\nPositive identifier class: {}", pattern);
    println!("Classification: {:?}", result);
}
