// Test using the regex/classify.rs directly (the one used by emit)
use bbnf::generate::regex::classify::classify_regex;

#[test]
fn test_negated_class_from_regex_classify() {
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

        for (i, r) in ranges.iter().enumerate() {
            if i < 5 || i >= ranges.len().saturating_sub(5) {
                println!("  [{}] 0x{:02X}-0x{:02X}", i, r.start, r.end);
            } else if i == 5 {
                println!("  ... ({} more ranges) ...", ranges.len() - 10);
            }
        }

        let total_bytes: usize = ranges
            .iter()
            .map(|r| (r.end - r.start + 1) as usize)
            .sum();
        println!("Total bytes covered: {}", total_bytes);
    }

    let result = classify_regex(pattern);
    println!("Classification result: {:?}", result);
}
